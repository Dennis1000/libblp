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
unit LibBLP.DXT;

interface

uses
  System.SysUtils, LibBLP.BLPPixelFormat;

type
  TDXT = class
  private
    class function Unpack565(PackedColor: PByte; Color: PByte): Integer;
    class procedure DecompressColour(RGBA: PByte; Block: PByte; IsDXT1: Boolean);
    class procedure DecompressAlphaDxt3(RGBA: PByte; Block: PByte);
    class procedure DecompressAlphaDxt5(RGBA: PByte; Block: PByte);
    class procedure Decompress(RGBA: PByte; Block: PByte; PixelFormat: TBLPPixelFormat);
  public
    class procedure DecompressImage(RGBA: TBytes; Width, Height: Integer; Blocks: TBytes; PixelFormat: TBLPPixelFormat);
  end;

implementation

{ TDXT }

// libSquish

class procedure TDXT.Decompress(RGBA, Block: PByte; PixelFormat: TBLPPixelFormat);
var
  ColourBlock: Integer;
begin
  // get the block locations
  if PixelFormat in [PIXEL_DXT3, PIXEL_DXT5] then
    ColourBlock := 8
  else
    ColourBlock := 0;

  DecompressColour(RGBA, @Block[ColourBlock], (PixelFormat = PIXEL_DXT1));

  // decompress alpha separately if necessary
  if PixelFormat = PIXEL_DXT3 then
    DecompressAlphaDxt3(RGBA, Block)
  else
  if PixelFormat = PIXEL_DXT5 then
    DecompressAlphaDxt5(RGBA, Block);
end;

class procedure TDXT.DecompressAlphaDxt3(RGBA, Block: PByte);
var
  I: Integer;
  Quant, Lo, Hi: Byte;
begin
  // unpack the alpha values pairwise
  for I := 0 to 7 do
  begin
    // quantise down to 4 bits
    Quant := Block[I];

    // unpack the values
    Lo := Quant and $0f;
    Hi := Quant and $f0;

    // convert back up to bytes
    RGBA[8*I + 3] := lo or (lo shl 4);
    RGBA[8*I + 7] := hi or (hi shr 4);
  end;
end;

class procedure TDXT.DecompressAlphaDxt5(RGBA, Block: PByte);
var
  Index, Byte, Value, Alpha0, Alpha1: Integer;
  Dest, Src: PByte;
  Codes, Indices: TBytes;
  I, J: Integer;
begin
  Alpha0 := Block[0];
  Alpha1 := Block[1];

  // compare the values to build the codebook
  SetLength(Codes, 8);
  Codes[0] := Alpha0;
  Codes[1] := Alpha1;

  if (Alpha0 <= Alpha1) then
  begin
    // use 5-alpha codebook
    for I := 1 to 4 do
      Codes[1 + I] := ((5 - I)*Alpha0 + I*Alpha1) div 5;

    Codes[6] := 0;
    Codes[7] := 255;
  end
  else
    // use 7-alpha codebook
    for I := 1 to 6 do
      Codes[1 + I] := ((7 - I)*Alpha0 + I*Alpha1 ) div 7;

  // decode the indices
  SetLength(Indices, 16);
  Src := @Block[2];
  Dest := @Indices[0];
  for I := 0 to 1 do
  begin
    // grab 3 bytes
    Value := 0;
    for J := 0 to 2 do
    begin
      Byte := Src[0];
      Inc(Src);
      Value := Value or (Byte shl (8*J));
    end;

    // unpack 8 3-bit values from it
    for J := 0 to 7 do
    begin
      Index := (Value shr (3*J)) and $7;
      Dest[0] := Index;
      Inc(Dest);
    end;
  end;

  // write out the indexed codebook values
  for I := 0 to 15 do
    RGBA[4*I + 3] := Codes[Indices[I]];
end;

class procedure TDXT.DecompressColour(RGBA: PByte; Block: PByte; IsDXT1: Boolean);
var
  Codes: TBytes;
  A, B, C, D: Integer;
  J, I: Integer;
  Indices: TBytes;
  Ind: PByte;
  PackedInd, Offset: Byte;
begin
  SetLength(Codes, 16);
  A := Unpack565(@Block[0], @Codes[0]);
  B := Unpack565(@Block[2], @Codes[4]);

  // generate the midpoints
  for I := 0 to 2 do
  begin
    C := Codes[I];
    D := Codes[4 + I];

    if (IsDXT1) and (a <= b) then
    begin
      Codes[8 + I] := (C + D) Div 2;
      Codes[12 + I] := 0;
    end
    else
    begin
      Codes[8 + I] := (2*C +D) Div 3;
      Codes[12 + I] := (C + 2*D) Div 3;
    end;
  end;

  // fill in alpha for the intermediate values
  Codes[8 + 3] := 255;
  if (IsDXT1) and (a <= b) then
    Codes[12 + 3] := 0
  else
    Codes[12 + 3] := 255;

  // unpack the indices
  SetLength(Indices, 16);
  for I := 0 to 3 do
  begin
    PackedInd := Block[4 + I];
    Ind := @Indices[4*I];
    Ind[0] := PackedInd and $3;
    Ind[1] := (PackedInd shr 2) and $3;
    Ind[2] := (PackedInd shr 4) and $3;
    Ind[3] := (PackedInd shr 6) and $3;
  end;

  // store out the colours
  for I := 0 to 15 do
  begin
    Offset := 4 * Indices[I];
    for J := 0 to 3 do
      RGBA[4*I + J] := Codes[Offset + J];
  end;
end;


class function TDXT.Unpack565(PackedColor, Color: PByte): Integer;
var
  Value: Integer;
  Red, Green, Blue: Byte;
begin
  // build the packed value
  Value := Integer(PackedColor[0]) or (Integer(PackedColor[1]) shl 8);

  // get the components in the stored range
  Red := (Value shr 11) and $1F;
  Green := (Value shr 5) and $3F;
  Blue := Value and $1F;

  // scale up to 8 bits
  Color[0] := (Blue shl 3) or (Blue shr 2);
  Color[1] := (Green shl 2) or (Green shr 4);
  Color[2] := (Red shl 3) or (Red shr 2);
  Color[3] := 255;

  Result := Value;
end;

class procedure TDXT.DecompressImage(RGBA: TBytes; Width, Height: Integer;
  Blocks: TBytes; PixelFormat: TBLPPixelFormat);
var
  BytesPerBlock: Integer;
  TargetRGBA: TBytes;
  X, Y: Integer;
  I: Integer;
  SourcePixel, TargetPixel: PByte;
  Sx, Sy, Py, Px: Integer;
  SourceBlock: PByte;
begin
  if PixelFormat = PIXEL_DXT1 then
    BytesPerBlock := 8
  else
    BytesPerBlock := 16;

  SetLength(TargetRGBA, 4*16);
  SourceBlock := @Blocks[0];

  // loop over blocks
  Y := 0;
  while Y < Height do
  begin
    X := 0;
    while (X < Width) do
    begin
      // decompress the block
      Decompress(@TargetRgba[0], SourceBlock, PixelFormat);

      // write the decompressed pixels to the correct image locations
      SourcePixel := @TargetRGBA[0];
      for Py := 0 to 3 do
        for Px := 0 to 3 do
        begin
          // get the target location
          Sx := X + Px;
          Sy := Y + Py;

          if (Sx < Width) and (Sy < Height) then
          begin
            TargetPixel := @RGBA[0];
            TargetPixel := TargetPixel + 4 * (Width*Sy + sx);

            // copy the rgba value
            for I := 0 to 3 do
            begin
              TargetPixel[0] := SourcePixel[0];
              Inc(TargetPixel);
              Inc(SourcePixel);
            end;
          end
          else
            // skip this pixel as its outside the image
            SourcePixel := SourcePixel + 4;
        end;

      // advance
      SourceBlock := SourceBlock + BytesPerBlock;

      X := X + 4;
    end;

    Inc(Y, 4);
  end;



end;


end.
