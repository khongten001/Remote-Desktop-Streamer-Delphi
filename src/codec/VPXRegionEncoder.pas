unit VPXRegionEncoder;

interface

uses
  Windows, System.Types, System.SysUtils, System.Math, System.Generics.Collections,
  Vcl.Graphics, libyuv, vpx_encoder, vpx_codec, vp8cx;

const
  kMacroBlockSize = 16;

type
  TRect = System.Types.TRect;

  TRegion = class
  private
    FRects: TList<TRect>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRect(const ARect: TRect);
    procedure IntersectWith(const ARect: TRect);
    function GetRects: TArray<TRect>;
    procedure Clear;
  end;

  TVpxActiveMap = class
  private
    FActiveMapBuffer: TBytes;
    FActiveMapStruct: vpx_active_map_t;
    procedure ClearMap;
    procedure UpdateMapForRect(const ARect: TRect; APadding: Integer; AMaxWidth, AMaxHeight: Integer);
  public
    constructor Create(Width, Height: Integer);
    destructor Destroy; override;
    procedure SetFullMap;
    procedure UpdateFromRegion(const ARegion: TRegion; APadding: Integer; AMaxWidth, AMaxHeight: Integer);
    property ActiveMapStruct: vpx_active_map_t read FActiveMapStruct;
  end;

implementation

{ TRegion }

constructor TRegion.Create;
begin
  inherited;
  FRects := TList<TRect>.Create;
end;

destructor TRegion.Destroy;
begin
  FRects.Free;
  inherited;
end;

procedure TRegion.AddRect(const ARect: TRect);
begin
  FRects.Add(ARect);
end;

procedure TRegion.IntersectWith(const ARect: TRect);
var
  I: Integer;
begin
  for I := FRects.Count - 1 downto 0 do
    FRects[I] := TRect.Intersect(FRects[I], ARect);
end;

function TRegion.GetRects: TArray<TRect>;
begin
  Result := FRects.ToArray;
end;

procedure TRegion.Clear;
begin
  FRects.Clear;
end;

{ TVpxActiveMap }

constructor TVpxActiveMap.Create(Width, Height: Integer);
begin
  inherited Create;

  // Setup active map dimensions
  FActiveMapStruct.cols := (Width + kMacroBlockSize - 1) div kMacroBlockSize;
  FActiveMapStruct.rows := (Height + kMacroBlockSize - 1) div kMacroBlockSize;

  // Allocate active map buffer
  SetLength(FActiveMapBuffer, FActiveMapStruct.cols * FActiveMapStruct.rows);
  FActiveMapStruct.active_map := @FActiveMapBuffer[0];

  ClearMap;
end;

destructor TVpxActiveMap.Destroy;
begin
  SetLength(FActiveMapBuffer, 0);
  inherited;
end;

procedure TVpxActiveMap.ClearMap;
begin
  if Length(FActiveMapBuffer) > 0 then
    FillChar(FActiveMapBuffer[0], Length(FActiveMapBuffer), 0);
end;

procedure TVpxActiveMap.SetFullMap;
begin
  if Length(FActiveMapBuffer) > 0 then
    FillChar(FActiveMapBuffer[0], Length(FActiveMapBuffer), 1);
end;

procedure TVpxActiveMap.UpdateMapForRect(const ARect: TRect; APadding: Integer; AMaxWidth, AMaxHeight: Integer);
var
  PaddedRect: TRect;
  Left, Top, Right, Bottom: Integer;
begin
  // Apply padding and clip to bounds
  PaddedRect := TRect.Create(
    Max(0, ARect.Left - APadding),
    Max(0, ARect.Top - APadding),
    Min(AMaxWidth, ARect.Right + APadding),
    Min(AMaxHeight, ARect.Bottom + APadding)
  );

  // Convert to macroblock coordinates
  Left := PaddedRect.Left div kMacroBlockSize;
  Top := PaddedRect.Top div kMacroBlockSize;
  Right := (PaddedRect.Right - 1) div kMacroBlockSize;
  Bottom := (PaddedRect.Bottom - 1) div kMacroBlockSize;

  // Update active map
  for var Y := Top to Bottom do
  begin
    var RowOffset := Y * Integer(FActiveMapStruct.cols);
    for var X := Left to Right do
    begin
      if (RowOffset + X) < Length(FActiveMapBuffer) then
        FActiveMapBuffer[RowOffset + X] := 1;
    end;
  end;
end;

procedure TVpxActiveMap.UpdateFromRegion(const ARegion: TRegion; APadding: Integer; AMaxWidth, AMaxHeight: Integer);
begin
  ClearMap;

  // Process each rectangle in the region
  for var Rect in ARegion.GetRects do
    UpdateMapForRect(Rect, APadding, AMaxWidth, AMaxHeight);
end;

end.
