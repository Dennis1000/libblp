program BPLViewer;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  LibBLP in '..\..\Source\LibBLP.pas',
  LibBLP.BLPFormat in '..\..\Source\LibBLP.BLPFormat.pas',
  LibBLP.BLPHeader in '..\..\Source\LibBLP.BLPHeader.pas',
  LibBLP.BLPPixelFormat in '..\..\Source\LibBLP.BLPPixelFormat.pas',
  LibBLP.TextureCompressionType in '..\..\Source\LibBLP.TextureCompressionType.pas',
  LibBLP.DXT in '..\..\Source\LibBLP.DXT.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
