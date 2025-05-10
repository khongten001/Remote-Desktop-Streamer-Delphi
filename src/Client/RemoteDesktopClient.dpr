program RemoteDesktopClient;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles,
  libyuv in '..\codec\libyuv.pas',
  video_decoder_vpx in '..\codec\video_decoder_vpx.pas',
  video_encoder_vpx in '..\codec\video_encoder_vpx.pas',
  vp8cx in '..\codec\vp8cx.pas',
  vp8dx in '..\codec\vp8dx.pas',
  VPXRegionEncoder in '..\codec\VPXRegionEncoder.pas',
  vpx_codec in '..\codec\vpx_codec.pas',
  vpx_decoder in '..\codec\vpx_decoder.pas',
  vpx_encoder in '..\codec\vpx_encoder.pas',
  vpx_image in '..\codec\vpx_image.pas',
  InputManager in 'InputManager.pas',
  DXGIFrameProcessor in '..\capture\DXGIFrameProcessor.pas',
  GDIFrameProcessor in '..\capture\GDIFrameProcessor.pas',
  MonitorUtils in '..\capture\MonitorUtils.pas',
  ScreenCapture in '..\capture\ScreenCapture.pas',
  ScreenCaptureTypes in '..\capture\ScreenCaptureTypes.pas',
  ScreenCaptureUtils in '..\capture\ScreenCaptureUtils.pas',
  ScreenCaptureUtils2 in '..\capture\ScreenCaptureUtils2.pas',
  DX12.D3D11 in '..\DXHeaders\DX12.D3D11.pas',
  DX12.D3DCommon in '..\DXHeaders\DX12.D3DCommon.pas',
  DX12.DCommon in '..\DXHeaders\DX12.DCommon.pas',
  DX12.DXGI in '..\DXHeaders\DX12.DXGI.pas',
  DX12.DXGI1_2 in '..\DXHeaders\DX12.DXGI1_2.pas';

{$R *.res}

begin
  Application.Initialize;
  //ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Allports Green');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.








