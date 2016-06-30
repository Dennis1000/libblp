{ LibBLP v1.0 - A BLP image reader library with DXT3/5 decompression support
  for Delphi 10.1 Berlin+ by Dennis Spreen
  http://blog.spreendigital.de/2016/06/16/libblp/

  (c) Copyrights 2016 Dennis D. Spreen <dennis@spreendigital.de>

  Uses GPLv3 licensed code from Nihlus/libwarcraft (https://github.com/Nihlus/libwarcraft)

  LibBLP is Free Software, and is distributed under the GPLv3 license. 
  This means, in simple terms, that you are free to do whatever you want
  with the source code and any binaries compiled or generated from it as 
  long as you pass on those rights to anyone aquiring a copy of the source
  code or binaries. The full licence can be read in the file "LICENSE" at 
  the root of the source tree, or at http://choosealicense.com/licenses/gpl-3.0/, 
  where a more people-friendly summary is also available
}
unit LibBLP.BLPHeader;

interface

uses
  System.Classes, System.Types, Generics.Collections, Generics.Defaults, System.SysUtils, System.TypInfo, System.IOUtils,
  LibBLP.BLPFormat, LibBLP.TextureCompressionType, LibBLP.BLPPixelFormat;

type
  TBLPHeader = class
  public
    Signature: String;
    Format: TBLPFormat;
    Version: UInt32;
    CompressionType: TTextureCompressionType;
    AlphaBitDepth: UInt32;
    PixelFormat: TBLPPixelFormat;
    MipMapType: UInt32;
    Resolution: TSize;
    MipMapOffsets: TList<UInt32>;
    MipMapSizes: TList<UInt32>;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); overload; virtual;
    procedure LoadFromStream(BinaryReader: TBinaryReader); overload; virtual;
    function MipMapCount: Integer;
  end;


implementation

{ TBLPHeader }

constructor TBLPHeader.Create;
begin
  MipMapOffsets := TList<UInt32>.Create;
  MipMapSizes := TList<UInt32>.Create;
  Signature := 'BLP2';
  Version := 1;
end;

destructor TBLPHeader.Destroy;
begin
  MipMapOffsets.Free;
  MipMapSizes.Free;
  inherited;
end;

function TBLPHeader.MipMapCount: Integer;
begin
  Result := MipMapOffsets.Count;
end;

procedure TBLPHeader.LoadFromStream(Stream: TStream);
var
  BinaryReader: TBinaryReader;
begin
  BinaryReader := TBinaryReader.Create(Stream, TEncoding.ANSI);
  try
    LoadFromStream(BinaryReader);
  finally
    BinaryReader.Free;
  end;
end;

procedure TBLPHeader.LoadFromStream(BinaryReader: TBinaryReader);
var
  BLPFormat: TBLPFormat;
  SignatureChars: TCharArray;
  SigChar: Char;
  I: Integer;
  Offset, Size: UInt32;
begin
  SignatureChars := BinaryReader.ReadChars(4);
  Signature := '';
  for SigChar in SignatureChars do
    Signature := Signature + SigChar;

  for BLPFormat := Low(TBLPFormat) to High(TBLPFormat) do
    if GetEnumName(TypeInfo(TBLPFormat), Ord(BLPFormat)) = Signature then
    begin
      Format := BLPFormat;
      Break;
    end;

  if Format = BLPUnknown then
    raise Exception.Create('The provided data did not have a BLP signature.');

  if Format = TBLPFormat.BLP2 then
  begin
    Version := BinaryReader.ReadUInt32;
    CompressionType := TTextureCompressionType(BinaryReader.ReadByte);
    AlphaBitDepth := BinaryReader.ReadByte;
  end
  else
  begin
    CompressionType := TTextureCompressionType(BinaryReader.ReadUInt32);
    AlphaBitDepth := BinaryReader.ReadUInt32;
  end;

  if Format < TBLPFormat.BLP2 then
   Resolution.Create(BinaryReader.ReadUInt32, BinaryReader.ReadUInt32);

  if Format = TBLPFormat.BLP2 then
  begin
    PixelFormat := TBLPPixelFormat(BinaryReader.ReadByte);
    MipMapType := BinaryReader.ReadByte;
    Resolution.Create(BinaryReader.ReadUInt32, BinaryReader.ReadUInt32);
  end
  else
  begin
    PixelFormat := TBLPPixelFormat(BinaryReader.ReadUInt32);
    MipMapType := BinaryReader.ReadUInt32;
  end;

  MipMapOffsets.Clear;
  for I := 0 to 15 do
  begin
    Offset := BinaryReader.ReadUInt32;
    if Offset > 0 then
      MipMapOffsets.Add(Offset);
  end;

  MipMapSizes.Clear;
  for I := 0 to 15 do
  begin
    Size := BinaryReader.ReadUInt32;
    if Size > 0 then
      MipMapSizes.Add(Size);
  end;
end;

end.
