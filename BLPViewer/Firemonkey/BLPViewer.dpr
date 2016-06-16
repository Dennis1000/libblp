program BLPViewer;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  LibBLP.BLPFormat in '..\..\Source\LibBLP.BLPFormat.pas',
  LibBLP.BLPHeader in '..\..\Source\LibBLP.BLPHeader.pas',
  LibBLP.BLPPixelFormat in '..\..\Source\LibBLP.BLPPixelFormat.pas',
  LibBLP.DXT in '..\..\Source\LibBLP.DXT.pas',
  LibBLP in '..\..\Source\LibBLP.pas',
  LibBLP.TextureCompressionType in '..\..\Source\LibBLP.TextureCompressionType.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
