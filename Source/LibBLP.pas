{ LibBLP v1.0 - A BLP image reader library with DXT3/5 decompression support
  for Delphi 10.1 Berlin+ by Dennis Spreen
  http://blog.spreendigital.de/2016/06/16/libblp/

  (c) Copyrights 2016 Dennis D. Spreen <dennis@spreendigital.de>
  This unit is free and can be used for any needs. The introduction of
  any changes and the use of those changed library is permitted without
  limitations. Only requirement:
  This text must be present without changes in all modifications of library.

  * The contents of this file are used with permission, subject to
  * the Mozilla Public License Version 1.1 (the "License"); you may   *
  * not use this file except in compliance with the License. You may  *
  * obtain a copy of the License at                                   *
  * http:  www.mozilla.org/MPL/MPL-1.1.html                           *
  *                                                                   *
  * Software distributed under the License is distributed on an       *
  * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or    *
  * implied. See the License for the specific language governing      *
  * rights and limitations under the License.                         *
}
unit LibBLP;

interface

uses
  System.Classes, Generics.Collections, Generics.Defaults, System.SysUtils, System.TypInfo, System.IOUtils, System.Types,
  LibBLP.BLPFormat, LibBLP.BLPHeader, LibBLP.TextureCompressionType, LibBLP.BLPPixelFormat;

type
  TRGBQuad = record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
    rgbReserved: Byte;
  end;

  TBLP = class
  private
    function PeekFormat(BinaryReader: TBinaryReader): TBLPFormat;
  protected
    Palette: TList<TRGBQuad>;
    JPEGHeaderSize: UInt32;
    JPEGHeader: TBytes;
    MipMaps: TList<TBytes>;
    procedure DecompressPalettized(RGBA: TBytes; Width, Height: Integer; Blocks: TBytes); virtual;
    procedure DecompressUncompressed(RGBA: TBytes; Width, Height: Integer; Blocks: TBytes); virtual;
  public
    Header: TBLPHeader;
    constructor Create; virtual;
    destructor Destroy; override;

    procedure LoadFromFile(FileName: String); virtual;
    procedure LoadFromStream(Stream: TStream); overload; virtual;

    function MipMap(MipLevel: Integer): TBytes;
    function MipResolution(MipLevel: Integer): TSize;
    function DecompressMipMap(InData: TBytes; MipLevel: Integer): TBytes; virtual;
  end;

implementation

{ TBLP }

uses
  System.Math, LibBLP.DXT;

type
  TInt64Helper = record helper for Int64
    function Clamp(MinValue, MaxValue: Integer): Integer;
  end;

constructor TBLP.Create;
begin
  Palette := TList<TRGBQuad>.Create;
  Header := TBLPHeader.Create;
  MipMaps := TList<TBytes>.Create;
end;


function TBLP.DecompressMipMap(InData: TBytes; MipLevel: Integer): TBytes;
var
  TargetRes: TSize;
begin
  Result := NIL;

  if not Assigned(InData) then
    Exit;

  TargetRes := MipResolution(MipLevel);

  if (TargetRes.cx <=0 ) or (TargetRes.cy <= 0) then
    Exit;

  SetLength(Result, TargetRes.cx * TargetRes.cy * 4);

  case Header.CompressionType of
    JPEG: ;
    Palettized: DecompressPalettized(Result, TargetRes.cx, TargetRes.cy, InData);
    DXTC: TDXT.DecompressImage(Result, TargetRes.cx, TargetRes.cy, InData, Header.PixelFormat);
    Uncompressed: DecompressUncompressed(Result, TargetRes.cx, TargetRes.cy, InData);
    Uncompressed_Alternate: ;
  end;
end;

procedure TBLP.DecompressPalettized(RGBA: TBytes; Width, Height: Integer;
  Blocks: TBytes);
var
  X,Y: Integer;
  ASource, Source, Destination: PByte;
  ColorIndex: Byte;
  PaletteColor: TRGBQuad;
  I: Integer;
  DataByte: Byte;
  AlphaBit, AlphaValue: Byte;
begin
  Source := @Blocks[0];
  Destination := @RGBA[0];

  for I := 0 to Width * Height - 1 do
  begin
    ColorIndex := Source[0];
    PaletteColor := Palette[ColorIndex];
    ASource := @PaletteColor;
    Move(ASource[0], Destination[0], 3);
    Inc(Destination, 4);
    Inc(Source);
  end;

  // Write Alpha values
  case Header.AlphaBitDepth of
    0:
      // The map is fully opaque, nothing more to do
      Exit;
    1:
      // The alpha value is stored per-bit in the byte (8 alpha values per byte)
      begin
        Destination := @RGBA[3];
        DataByte := 0;
        I := 7;  // force new DataByte

        for Y := 0 to Height - 1 do
          for X := 0 to Width - 1 do
          begin
            Inc(I);
            if I = 8 then
            begin
              DataByte := Source[0];
              Inc(Source);
              I := 0;
            end;

            AlphaBit := (DataByte shr I) and $01;
            if AlphaBit > 0 then
              Destination[0] := 255
            else
              Destination[0] := 0;

            Inc(Destination, 4);
          end;
      end;
    4:
      // The alpha value is stored as half a byte (2 alpha values per byte)
      // (4 bits can hold 0 - 15 alpha)
      begin
        Destination := @RGBA[3];
        for Y := 0 to Height - 1 do
        begin
          X := 0;
          while (X < Width) do
          begin
            DataByte := Source[0];
            AlphaValue := DataByte shr 4;
            Destination[0] := AlphaValue;

            AlphaValue := DataByte and $0F;
            Destination[4] := AlphaValue;

            Inc(Destination, 8);
            Inc(Source);
            Inc(X, 2);
          end;
        end;
      end;
    8:
      // Directly write the alpha values
      begin
        Destination := @RGBA[3];
        for Y := 0 to Height - 1 do
          for X := 0 to Width - 1 do
          begin
            Destination[0] := Source[0];
            Inc(Source);
            Inc(Destination, 4);
          end;
      end;
  end;
end;

procedure TBLP.DecompressUncompressed(RGBA: TBytes; Width, Height: Integer;
  Blocks: TBytes);
var
  A, R, G, B: Byte;
  X, Y: Integer;
  Destination, Source: PByte;
begin
  Source := @Blocks[0];
  Destination := @RGBA[0];

  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
    begin
      A := Source[0];
      R := Source[1];
      G := Source[2];
      B := Source[3];
      Destination[0] := R;
      Destination[1] := G;
      Destination[2] := B;
      Destination[3] := A;
      Inc(Source, 4);
      Inc(Destination, 4);
    end;
end;

destructor TBLP.Destroy;
begin
  MipMaps.Free;
  Header.Free;
  Palette.Free;
  inherited;
end;

function TBLP.MipResolution(MipLevel: Integer): TSize;
begin
  Result.cx := Header.Resolution.cx div Trunc(IntPower(2, MipLevel)).Clamp(1, Header.Resolution.cx);
  Result.cy := Header.Resolution.cy div Trunc(IntPower(2, MipLevel)).Clamp(1, Header.Resolution.cy);
end;

function TBLP.MipMap(MipLevel: Integer): TBytes;
begin
  if MipMaps.Count > MipLevel then
    Result := MipMaps[MipLevel]
  else
    Result := NIL;
end;

procedure TBLP.LoadFromFile(FileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
  LoadFromStream(FileStream);
  FileStream.Free;
end;

procedure TBLP.LoadFromStream(Stream: TStream);
var
  BinaryReader: TBinaryReader;
  MemoryStream: TMemoryStream;
  FileHeaderBytes: TBytes;
  I: Integer;
  R,G,B,A: Byte;
  PaletteColor: TRGBQuad;
begin
  Palette.Clear;
  MipMaps.Clear;

  BinaryReader := TBinaryReader.Create(Stream, TEncoding.ANSI);
  try
    if PeekFormat(BinaryReader) = TBLPFormat.BLP2 then
      FileHeaderBytes := BinaryReader.ReadBytes(148)
    else
      FileHeaderBytes := BinaryReader.ReadBytes(156);

    MemoryStream := TMemoryStream.Create;
    try
      MemoryStream.WriteData(FileHeaderBytes, Length(FileHeaderBytes));
      MemoryStream.Position := 0;
      Header.LoadFromStream(MemoryStream);
    finally
      MemoryStream.Free;
    end;

    if Header.CompressionType = TTextureCompressionType.JPEG then
    begin
      JPEGHeaderSize := BinaryReader.ReadUInt32;
      JPEGHeader := BinaryReader.ReadBytes(JPEGHeaderSize);
    end
    else
    if Header.CompressionType = TTextureCompressionType.Palettized then
    begin
      for I := 0 to 255 do
      begin
        B := BinaryReader.ReadByte;
        G := BinaryReader.ReadByte;
        R := BinaryReader.ReadByte;
        A := BinaryReader.ReadByte;

        // Set Alpha to fully opaque if AlphaBitDepth = 0
        if Header.AlphaBitDepth = 0 then
          A := 255;

        PaletteColor.rgbBlue := B;
        PaletteColor.rgbGreen := G;
        PaletteColor.rgbRed := R;
        PaletteColor.rgbReserved := A;
        Palette.Add(PaletteColor);
      end;
    end
    else
    begin
      // Fill up an empty palette - the palette is always present, but we'll be going after offsets anyway
      PaletteColor.rgbBlue := 0;
      PaletteColor.rgbGreen := 0;
      PaletteColor.rgbRed := 0;
      PaletteColor.rgbReserved := 255;

      for I := 0 to 255 do
        Palette.Add(PaletteColor);
    end;

    // Read the raw mipmap data
    for I := 0 to Header.MipMapCount - 1 do
    begin
      BinaryReader.BaseStream.Position := Header.MipMapOffsets[I];
      MipMaps.Add(BinaryReader.ReadBytes(Header.MipMapSizes[I]));
    end;

  finally
    BinaryReader.Free;
  end;
end;

function TBLP.PeekFormat(BinaryReader: TBinaryReader): TBLPFormat;
var
  StartPosition: Int64;
  SignatureChars: TCharArray;
  SigChar: Char;
  Signature: String;
  Format, BLPFormat: TBLPFormat;
begin
  StartPosition := BinaryReader.BaseStream.Position;

  SignatureChars := BinaryReader.ReadChars(4);
  Signature := '';
  for SigChar in SignatureChars do
    Signature := Signature + SigChar;

  Format := BLPUnknown;
  for BLPFormat := Low(TBLPFormat) to High(TBLPFormat) do
    if GetEnumName(TypeInfo(TBLPFormat), Ord(BLPFormat)) = Signature then
    begin
      Format := BLPFormat;
      Break;
    end;

  BinaryReader.BaseStream.Position := StartPosition;
  if Format = BLPUnknown then
    raise Exception.Create('The provided data did not have a BLP signature.');

  Result := Format;
end;


{ TIntHelper }

function TInt64Helper.Clamp(MinValue, MaxValue: Integer): Integer;
begin
  if Self > MaxValue then
    Result := MaxValue
  else
  if Self < MinValue then
    Result := MinValue
  else
    Result := Self;
end;

end.
