unit ScreenCaptureUtils2;

// Region detection utilities for GDI

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math, ScreenCaptureTypes;

const
  kBlockSize = 16;
  kBytesPerPixel = 4;
  kBytesPerBlock = kBlockSize * kBytesPerPixel;

type
  // Main differ class
  TDiffer = class
  private
    FScreenRect: TFrameRect;
    FBytesPerRow: Integer;
    FDiffWidth: Integer;
    FDiffHeight: Integer;
    FFullBlocksX: Integer;
    FFullBlocksY: Integer;
    FPartialColumnWidth: Integer;
    FPartialRowHeight: Integer;
    FBlockStrideY: Integer;
    FDiffInfo: TArray<Byte>;
    FUseSSE2: Boolean;

    class function IsSSE2Available: Boolean; static;
    procedure MarkDirtyBlocks(PrevImage, CurrImage: PByte);
    procedure MergeBlocks(var DirtyRegion: TArray<TFrameRect>);
    function DiffPartialBlock(PrevImage, CurrImage: PByte; BytesPerRow, BytesPerBlock, Height: Integer): Byte;
    function DiffFullBlock(Image1, Image2: PByte; BytesPerRow: Integer): Byte;
    function DiffFullBlockSSE2(Image1, Image2: PByte; BytesPerRow: Integer): Byte;
    function DiffFullBlockC(Image1, Image2: PByte; BytesPerRow: Integer): Byte;
  public
    constructor Create(const Width, Height: Integer);
    procedure CalcDirtyRegion(PrevImage, CurrImage: PByte; var DirtyRegion: TArray<TFrameRect>);
  end;

implementation

type
  TCPUIDResult = record
    EAX: Cardinal;
    EBX: Cardinal;
    ECX: Cardinal;
    EDX: Cardinal;
  end;

function CPUID(FunctionID: Cardinal): TCPUIDResult;

asm
  {$IFDEF CPUX64}
  push rbx
  mov eax, FunctionID
  cpuid
  mov TCPUIDResult[rcx].EAX, eax
  mov TCPUIDResult[rcx].EBX, ebx
  mov TCPUIDResult[rcx].ECX, ecx
  mov TCPUIDResult[rcx].EDX, edx
  pop rbx
  {$ELSE}
  push ebx
  push edi
  mov edi, edx         // edx contains the pointer to result
  mov eax, FunctionID
  cpuid
  // Store results in the record
  mov dword ptr [edi], eax      // EAX
  mov dword ptr [edi + 4], ebx  // EBX
  mov dword ptr [edi + 8], ecx  // ECX
  mov dword ptr [edi + 12], edx // EDX
  pop edi
  pop ebx
  {$ENDIF}
end;

class function TDiffer.IsSSE2Available: Boolean;
const
  SSE2_FEATURE_BIT = $04000000;  // Bit 26 in EDX indicates SSE2 support
var
  CPUInfo: TCPUIDResult;
begin
  {$IFDEF CPUX86}
  // Get CPU feature flags
  CPUInfo := CPUID(1);
  Result := (CPUInfo.EDX and SSE2_FEATURE_BIT) <> 0;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TDiffer.DiffFullBlockC(Image1, Image2: PByte; BytesPerRow: Integer): Byte;
var
  Y: Integer;
begin
  Result := 0;
  for Y := 0 to kBlockSize - 1 do
  begin
    if not CompareMem(Image1, Image2, kBlockSize * kBytesPerPixel) then
      Exit(1);

    Inc(Image1, BytesPerRow);
    Inc(Image2, BytesPerRow);
  end;
end;

function TDiffer.DiffFullBlockSSE2(Image1, Image2: PByte; BytesPerRow: Integer): Byte;
var
  Y: Integer;
  Diff: Integer;
  Image1Vec, Image2Vec: PByte;
begin
  Result := 0;
  {$IFDEF CPUX86}
  for Y := 0 to kBlockSize - 1 do
  begin
    Image1Vec := Image1;
    Image2Vec := Image2;

    Diff := 0;

    asm
      push eax
      push ebx

      mov eax, Image1Vec
      mov ebx, Image2Vec

      // First 16 bytes
      movdqu xmm0, [eax]
      movdqu xmm1, [ebx]
      pcmpeqb xmm0, xmm1
      pmovmskb ecx, xmm0
      xor ecx, $FFFF
      or Diff, ecx

      // Second 16 bytes
      movdqu xmm0, [eax + 16]
      movdqu xmm1, [ebx + 16]
      pcmpeqb xmm0, xmm1
      pmovmskb ecx, xmm0
      xor ecx, $FFFF
      or Diff, ecx

      // Third 16 bytes
      movdqu xmm0, [eax + 32]
      movdqu xmm1, [ebx + 32]
      pcmpeqb xmm0, xmm1
      pmovmskb ecx, xmm0
      xor ecx, $FFFF
      or Diff, ecx

      // Fourth 16 bytes
      movdqu xmm0, [eax + 48]
      movdqu xmm1, [ebx + 48]
      pcmpeqb xmm0, xmm1
      pmovmskb ecx, xmm0
      xor ecx, $FFFF
      or Diff, ecx

      pop ebx
      pop eax
    end;

    if Diff <> 0 then
      Exit(1);

    Inc(Image1, BytesPerRow);
    Inc(Image2, BytesPerRow);
  end;
  {$ENDIF}
end;

function TDiffer.DiffFullBlock(Image1, Image2: PByte; BytesPerRow: Integer): Byte;
begin
  //if FUseSSE2 then
    //Result := DiffFullBlockSSE2(Image1, Image2, BytesPerRow)
  //else
    Result := DiffFullBlockC(Image1, Image2, BytesPerRow);
end;

function TDiffer.DiffPartialBlock(PrevImage, CurrImage: PByte;
  BytesPerRow, BytesPerBlock, Height: Integer): Byte;
var
  Y: Integer;
begin
  Result := 0;
  for Y := 0 to Height - 1 do
  begin
    if not CompareMem(PrevImage, CurrImage, BytesPerBlock) then
      Exit(1);

    Inc(PrevImage, BytesPerRow);
    Inc(CurrImage, BytesPerRow);
  end;
end;

constructor TDiffer.Create(const Width, Height: Integer);
begin
  inherited Create;

  FScreenRect.Left := 0;
  FScreenRect.Top := 0;
  FScreenRect.Right := Width;
  FScreenRect.Bottom := Height;

  FBytesPerRow := Width * kBytesPerPixel;
  FDiffWidth := ((Width + kBlockSize - 1) div kBlockSize) + 1;
  FDiffHeight := ((Height + kBlockSize - 1) div kBlockSize) + 1;
  FFullBlocksX := Width div kBlockSize;
  FFullBlocksY := Height div kBlockSize;

  FPartialColumnWidth := Width - (FFullBlocksX * kBlockSize);
  FPartialRowHeight := Height - (FFullBlocksY * kBlockSize);

  FBlockStrideY := FBytesPerRow * kBlockSize;

  SetLength(FDiffInfo, FDiffWidth * FDiffHeight);
  FillChar(FDiffInfo[0], Length(FDiffInfo), 0);

  //FUseSSE2 := IsSSE2Available;
end;

procedure TDiffer.MarkDirtyBlocks(PrevImage, CurrImage: PByte);
var
  PrevBlockRowStart, CurrBlockRowStart: PByte;
  PrevBlock, CurrBlock: PByte;
  IsDiffRowStart: PByte;
  DiffStride, X, Y: Integer;
  IsDifferent: PByte;
begin
  PrevBlockRowStart := PrevImage;
  CurrBlockRowStart := CurrImage;
  DiffStride := FDiffWidth;
  IsDiffRowStart := @FDiffInfo[0];

  for Y := 0 to FFullBlocksY - 1 do
  begin
    PrevBlock := PrevBlockRowStart;
    CurrBlock := CurrBlockRowStart;
    IsDifferent := IsDiffRowStart;

    for X := 0 to FFullBlocksX - 1 do
    begin
      IsDifferent^ := DiffFullBlock(PrevBlock, CurrBlock, FBytesPerRow);

      Inc(PrevBlock, kBytesPerBlock);
      Inc(CurrBlock, kBytesPerBlock);
      Inc(IsDifferent);
    end;

    if FPartialColumnWidth > 0 then
    begin
      IsDifferent^ := DiffPartialBlock(PrevBlock, CurrBlock, FBytesPerRow,
        FPartialColumnWidth * kBytesPerPixel, kBlockSize);
    end;

    Inc(PrevBlockRowStart, FBlockStrideY);
    Inc(CurrBlockRowStart, FBlockStrideY);
    Inc(IsDiffRowStart, DiffStride);
  end;

  if FPartialRowHeight > 0 then
  begin
    PrevBlock := PrevBlockRowStart;
    CurrBlock := CurrBlockRowStart;
    IsDifferent := IsDiffRowStart;

    for X := 0 to FFullBlocksX - 1 do
    begin
      IsDifferent^ := DiffPartialBlock(PrevBlock, CurrBlock, FBytesPerRow,
        kBytesPerBlock, FPartialRowHeight);

      Inc(PrevBlock, kBytesPerBlock);
      Inc(CurrBlock, kBytesPerBlock);
      Inc(IsDifferent);
    end;

    if FPartialColumnWidth > 0 then
    begin
      IsDifferent^ := DiffPartialBlock(PrevBlock, CurrBlock, FBytesPerRow,
        FPartialColumnWidth * kBytesPerPixel, FPartialRowHeight);
    end;
  end;
end;


procedure TDiffer.MergeBlocks(var DirtyRegion: TArray<TFrameRect>);
var
  IsDiffRowStart: PByte;
  DiffStride, X, Y: Integer;
  IsDifferent: PByte;
  Width, Height: Integer;
  Right: PByte;
  Bottom: PByte;
  FoundNewRow: Boolean;
  DirtyRect: TFrameRect;
  J: Integer;
begin
  SetLength(DirtyRegion, 0);
  IsDiffRowStart := @FDiffInfo[0];
  DiffStride := FDiffWidth;

  for Y := 0 to FDiffHeight - 1 do
  begin
    IsDifferent := IsDiffRowStart;

    for X := 0 to FDiffWidth - 1 do
    begin
      if IsDifferent^ <> 0 then
      begin
        Width := 1;
        Height := 1;

        IsDifferent^ := 0;

        Right := IsDifferent;
        Inc(Right);

        while Right^ <> 0 do
        begin
          Right^ := 0;
          Inc(Right);
          Inc(Width);
        end;

        Bottom := IsDifferent;
        repeat
          FoundNewRow := True;
          Inc(Bottom, DiffStride);
          Right := Bottom;

          for J := 0 to Width - 1 do
          begin
            if Right^ = 0 then
            begin
              FoundNewRow := False;
              Break;
            end;
            Inc(Right);
          end;

          if FoundNewRow then
          begin
            Inc(Height);

            Right := Bottom;
            for J := 0 to Width - 1 do
            begin
              Right^ := 0;
              Inc(Right);
            end;
          end;

        until not FoundNewRow;

        DirtyRect.Left := X * kBlockSize;
        DirtyRect.Top := Y * kBlockSize;
        DirtyRect.Right := DirtyRect.Left + (Width * kBlockSize);
        DirtyRect.Bottom := DirtyRect.Top + (Height * kBlockSize);

        if DirtyRect.Right > FScreenRect.Right then
          DirtyRect.Right := FScreenRect.Right;
        if DirtyRect.Bottom > FScreenRect.Bottom then
          DirtyRect.Bottom := FScreenRect.Bottom;

        SetLength(DirtyRegion, Length(DirtyRegion) + 1);
        DirtyRegion[High(DirtyRegion)] := DirtyRect;
      end;

      Inc(IsDifferent);
    end;

    Inc(IsDiffRowStart, DiffStride);
  end;
end;



procedure TDiffer.CalcDirtyRegion(PrevImage, CurrImage: PByte; var DirtyRegion: TArray<TFrameRect>);
begin
  SetLength(DirtyRegion, 0);
  MarkDirtyBlocks(PrevImage, CurrImage);
  MergeBlocks(DirtyRegion);
end;

end.

