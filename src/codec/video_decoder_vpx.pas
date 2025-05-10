unit video_decoder_vpx;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics,
  libyuv, vp8dx, vpx_decoder, vpx_image, vpx_codec, System.IOUtils;

type
  // Frame header structure matching encoder
  TVpxFrameHeader = packed record
    Width: Word;
    Height: Word;
    Timestamp: Int64;
    DataSize: Cardinal;
    IsKeyFrame: Boolean;
    CodecFourCC: Cardinal;  // VP80 or VP90
  end;

  EVpxDecoderError = class(Exception);

  TImageConverter = class
  public
    class procedure YUVToARGB(const Img: Pvpx_image_t; Width, Height: Integer;
      const Bitmap: TBitmap);
  end;

  TVpxDecoder = class
  private
    FCodec: vpx_codec_ctx_t;
    FInitialized: Boolean;
    FCodecType: Cardinal;
    FWidth: Integer;
    FHeight: Integer;

    procedure InitializeDecoder(Width, Height: Integer; FourCC: Cardinal);
    function DecodeFrame(const EncodedData: TBytes): Pvpx_image_t;
    function ReadFrameHeader(AStream: TMemoryStream; out Header: TVpxFrameHeader): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Decode(InputMemStream: TMemoryStream): TBitmap;
  end;

implementation

procedure DebugLog(const Msg: string);
const
  LogFile = 'D:\VPXDecoderLog.txt';
begin
  try
    TFile.AppendAllText(LogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ': ' + Msg + sLineBreak);
  except
    // Silently fail if we can't write to the log
  end;
end;

function get_vpx_decoder_by_fourcc(fourcc: Cardinal): Pvpx_codec_iface_t;
begin
  case fourcc of
    $30385056:  // 'VP80'
      Result := vpx_codec_vp8_dx();
    $30395056:  // 'VP90'
      Result := vpx_codec_vp9_dx();
    else
      Result := nil;
  end;
end;

{ TImageConverter }

class procedure TImageConverter.YUVToARGB(const Img: Pvpx_image_t;
  Width, Height: Integer; const Bitmap: TBitmap);
var
  Y: Integer;
  RGBData: PByte;
  ScanLine: PByte;
begin
  if not Assigned(Img) or not Assigned(Bitmap) then
    raise EVpxDecoderError.Create('Invalid image conversion parameters');

  // Configure VCL bitmap
  Bitmap.PixelFormat := pf32bit;
  Bitmap.Width := Width;
  Bitmap.Height := Height;

  GetMem(RGBData, Width * Height * 4);
  try
    // Convert YUV to ARGB
    if I420ToARGB(
      Img^.planes[0], Img^.stride[0],    // Y
      Img^.planes[1], Img^.stride[1],    // U
      Img^.planes[2], Img^.stride[2],    // V
      RGBData, Width * 4,                // ARGB output
      Width, Height) <> 0 then
      raise EVpxDecoderError.Create('YUV to ARGB conversion failed');

    // Copy data to bitmap scanlines
    for Y := 0 to Height - 1 do
    begin
      ScanLine := Bitmap.ScanLine[Y];
      Move(RGBData[Y * Width * 4], ScanLine^, Width * 4);
    end;
  finally
    FreeMem(RGBData);
  end;
end;

{ TVpxDecoder }

constructor TVpxDecoder.Create;
begin
  inherited Create;
  FillChar(FCodec, SizeOf(FCodec), 0);
  FInitialized := False;
  FWidth := 0;
  FHeight := 0;
  FCodecType := 0;
end;

destructor TVpxDecoder.Destroy;
begin
  if FInitialized then
    vpx_codec_destroy(@FCodec);
  inherited;
end;

procedure TVpxDecoder.InitializeDecoder(Width, Height: Integer; FourCC: Cardinal);
var
  Cfg: vpx_codec_dec_cfg_t;
  Res: vpx_codec_err_t;
  CodecInterface: Pvpx_codec_iface_t;
begin
  // Only reinitialize if something changed
  if FInitialized and (FWidth = Width) and (FHeight = Height) and
     (FCodecType = FourCC) then
    Exit;

  // Clean up existing codec if initialized
  if FInitialized then
    vpx_codec_destroy(@FCodec);

  CodecInterface := get_vpx_decoder_by_fourcc(FourCC);
  if CodecInterface = nil then
    raise EVpxDecoderError.Create('Unsupported codec format');

  // Initialize decoder configuration
  FillChar(Cfg, SizeOf(Cfg), 0);
  Cfg.w := Width;
  Cfg.h := Height;
  Cfg.threads := System.CPUCount;

  // Initialize the decoder
  Res := vpx_codec_err_t(vpx_codec_dec_init(@FCodec, CodecInterface, @Cfg, 0));
  if Res <> VPX_CODEC_OK then
    raise EVpxDecoderError.CreateFmt('Failed to initialize decoder: %s',
      [vpx_codec_err_to_string(Res)]);

  FWidth := Width;
  FHeight := Height;
  FCodecType := FourCC;
  FInitialized := True;
end;

function TVpxDecoder.ReadFrameHeader(AStream: TMemoryStream;
  out Header: TVpxFrameHeader): Boolean;
begin
  Result := False;
  if AStream.Size < SizeOf(TVpxFrameHeader) then
    Exit;

  try
    AStream.ReadBuffer(Header, SizeOf(TVpxFrameHeader));
    Result := True;
  except
    on E: Exception do
      // Just return false on error
  end;
end;

function TVpxDecoder.DecodeFrame(const EncodedData: TBytes): Pvpx_image_t;
var
  Res: vpx_codec_err_t;
  Iter: vpx_codec_iter_t;
begin
  // Decode the frame
  Res := vpx_codec_err_t(vpx_codec_decode(@FCodec, @EncodedData[0],
    Length(EncodedData), nil, 0));
  if Res <> VPX_CODEC_OK then
    raise EVpxDecoderError.CreateFmt('Failed to decode frame: %s',
      [vpx_codec_err_to_string(Res)]);

  // Get the decoded frame
  Iter := nil;
  Result := vpx_codec_get_frame(@FCodec, @Iter);
  if Result = nil then
    raise EVpxDecoderError.Create('Failed to get decoded frame');
end;

function TVpxDecoder.Decode(InputMemStream: TMemoryStream): TBitmap;
var
  Header: TVpxFrameHeader;
  FrameData: TBytes;
  DecodedImage: Pvpx_image_t;
begin
  Result := nil;

  // Read and validate frame header
  if not ReadFrameHeader(InputMemStream, Header) then
    raise EVpxDecoderError.Create('Failed to read frame header');

  // Validate remaining data size
  if (InputMemStream.Size - InputMemStream.Position) < Header.DataSize then
    raise EVpxDecoderError.CreateFmt('Insufficient data. Expected: %d, Available: %d',
      [Header.DataSize, InputMemStream.Size - InputMemStream.Position]);

  try

    // Initialize or reinitialize decoder if needed
    InitializeDecoder(Header.Width, Header.Height, Header.CodecFourCC);

    // Read frame data
    SetLength(FrameData, Header.DataSize);
    if InputMemStream.Read(FrameData[0], Header.DataSize) <> Header.DataSize then
      raise EVpxDecoderError.Create('Failed to read complete frame data');

    // Decode frame
    DecodedImage := DecodeFrame(FrameData);

    // Convert to VCL bitmap
    Result := TBitmap.Create;
    try
      TImageConverter.YUVToARGB(DecodedImage, Header.Width, Header.Height, Result);
    except
      on E: Exception do
      begin
        FreeAndNil(Result);
        raise;
      end;
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      raise EVpxDecoderError.Create('Decoding failed: ' + E.Message);
    end;
  end;
end;

end.
