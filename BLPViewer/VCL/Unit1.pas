unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  LibBLP, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    BLP: TBLP;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.Math, PngImage;

procedure TForm1.Button1Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'BLP Files (*.blp)|*.blp';
  if OpenDialog1.Execute then
  begin
    BLP.LoadFromFile(OpenDialog1.FileName);
    Trackbar1.Min := 0;
    TrackBar1.Max := BLP.Header.MipMapCount - 1;
    TrackBar1.Position := 0;
    TrackBar1Change(Sender);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BLP := TBLP.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  BLP.Free;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
var
  MipLevel: Integer;
  Bitmap: TBitmap;
  MipMap: TBytes;
  RGBA: TBytes;
  X,Y: Integer;
  Png: TPngImage;
  MipSize: TSize;
begin
  MipLevel := Trackbar1.Position;
  Label3.Caption := IntToStr(MipLevel);

  MipMap := BLP.MipMap(MipLevel);
  if not Assigned(MipMap) then
    Exit;

  RGBA := BLP.DecompressMipMap(MipMap, MipLevel);
  MipSize := BLP.MipResolution(MipLevel);

  Bitmap := TBitmap.Create;
  Bitmap.PixelFormat := pf32bit;
  Bitmap.AlphaFormat := afDefined;
  Bitmap.Width := MipSize.cx;
  Bitmap.Height := MipSize.cy;

  // Copy image data to bitmap
  for Y := 0 to Bitmap.Height - 1 do
    Move((@RGBA[Y * 4 * Bitmap.Width])^, Bitmap.ScanLine[Y]^, 4 * Bitmap.Width);

  // using a PNG as workround, got some artifacts by using the bmp directly
  Png := TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, bitmap.Width , bitmap.Height);
  Png.Canvas.Draw(0,0,bitmap);

  // Update alpha values
  for Y := 0 to Bitmap.Height - 1 do
    for X := 0 to Bitmap.Width - 1 do
      Png.AlphaScanline[Y][X] := RGBA[Y * 4 * Bitmap.Width + + X*4 + 3];

  Image1.Picture.Assign(Png);

  Image1.Width := BLP.Header.Resolution.cx;
  Image1.Height := BLP.Header.Resolution.cy;

  Png.Free;
  Bitmap.Free;
end;

end.
