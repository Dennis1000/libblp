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
unit LibBLP.BLPPixelFormat;

interface

type
  {$Z4}  // enum size = 4 byte uint
  TBLPPixelFormat = (
    PIXEL_DXT1 = 0,
    PIXEL_DXT3 = 1,
    PIXEL_ARGB8888 = 2,
    PIXEL_A8R8G8B8 = 3,
    PIXEL_A8R8G8B8_2 = 4,
    PIXEL_R8G8B8A8_InvertedAlpha = 5,
    PIXEL_UNKNOWN = 6,
    PIXEL_DXT5 = 7,
    PIXEL_PALETTIZED = 8
  );

implementation

end.
