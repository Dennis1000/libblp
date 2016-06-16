unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Controls.Presentation, LibBLP, FMX.Edit;

type
  TForm1 = class(TForm)
    Button1: TButton;
    TrackBar1: TTrackBar;
    MipLevel: TLabel;
    Label1: TLabel;
    Image1: TImage;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    BLP: TBLP;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Math, FMX.Utils;

procedure TForm1.Button1Click(Sender: TObject);
begin
  OpenDialog1.Filename := '/storage/emulated/0/Attack.blp';
  //OpenDialog1.Filter := 'BLP Files (*.blp)|*.blp';
  //if OpenDialog1.Execute then
  begin
    BLP.LoadFromFile(OpenDialog1.Filename);
    Trackbar1.Min := 0;
    TrackBar1.Max := BLP.Header.MipMapCount - 1;
    TrackBar1.Value := 0;
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

procedure TForm1.FormShow(Sender: TObject);
begin
  Edit1.Text := GetHomePath;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
var
  MipLevel: Integer;
  Bitmap: TBitmap;
  MipMap: TBytes;
  RGBA: TBytes;
  X, Y: Integer;
  BitmapData: TBitmapData;
  Color: TAlphaColor;
  Offset: Integer;
  MipSize: TSize;
begin
  MipLevel := Trunc(Trackbar1.Value);
  Label1.Text := MipLevel.ToString;

  MipMap := BLP.MipMap(MipLevel);
  if not Assigned(MipMap) then
    Exit;

  MipSize := BLP.MipResolution(MipLevel);

  Bitmap := TBitmap.Create;
  Bitmap.Width := MipSize.cx;
  Bitmap.Height := MipSize.cy;

  RGBA := BLP.DecompressMipMap(MipMap, MipLevel);

  if Bitmap.Map(TMapAccess.ReadWrite, BitmapData) then
  try
    Offset := 0;
    for Y := 0 to Bitmap.Height - 1 do
      for X := 0 to Bitmap.Width - 1 do
      begin
        TAlphaColorRec(Color).B := RGBA[Offset];
        TAlphaColorRec(Color).G := RGBA[Offset + 1];
        TAlphaColorRec(Color).R := RGBA[Offset + 2];
        TAlphaColorRec(Color).A := RGBA[Offset + 3];
        BitmapData.SetPixel(X, Y, Color);
        Inc(Offset, 4);
      end;

  finally
    Bitmap.Unmap(BitmapData);
  end;

  Image1.Bitmap.Assign(Bitmap);
  Image1.Width := BLP.Header.Resolution.cx;
  Image1.Height := BLP.Header.Resolution.cy;
  Bitmap.Free;
end;


end.

