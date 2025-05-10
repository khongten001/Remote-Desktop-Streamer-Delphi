program RemoteDesktopServer;

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
  vpx_codec in '..\codec\vpx_codec.pas',
  vpx_decoder in '..\codec\vpx_decoder.pas',
  vpx_encoder in '..\codec\vpx_encoder.pas',
  vpx_image in '..\codec\vpx_image.pas',
  PanelDisplay in 'PanelDisplay.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Allports');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.








