{
  GPGSX
  Copyright (C) 2007, Ascher Stefan. All rights reserved.
  sa@stievie.net, http://www.stievie.net/

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
  
 $Id: $
}

{
  A Unit to embed data in Windows Bitmap files and PNGs with 24 Bit color delpth.
  JPEG is not supported because it compresses lossy and therefore may remove
  some entropy. GIF is not supported because it can have only 256 colors. For
  these types another algorithm must be used, like F5 for JPEG.

  It shouldn't be too easy to examine whether the image contains hidden data,
  because this unit doesn't know it either. Depending on the password it chooses
  the coordinates and extracts there the least significant bit (LSB) and builds
  with those bits the data. This may be a real data or just random noise.

  Details

  1. Make an array with possible coordinates.
  2. Seed a pseudo random number generator (ISAAC) with the SHA-512 hash of the
    password.
  3. Make a random key with this random numbers.
  4. Seed a random generator with this random key.
  5. Randomize the array with coordinates with the random number generator seeded
    with the key.
  6. Use the coordinates in this array to store the data.
  7. Store the data in the least significant bit of each color of the pixel.

  So depending on the password you use you may get different data. If the wrong
  password is used, you'll get most likely just random data. But it does not say
  you there is no data. This is useful if the data is additionally encrypted.

  Another advantage of this method is that it chooses the pixel to store the data
  randomly, so the data is spread all over the image.
}

unit StegImage;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$i gsx.inc}

interface

uses
{$IFNDEF FPC}
  System.Types, Vcl.Imaging.pngimage,
{$ELSE}
  fpimage, Interfaces,
{$ENDIF}
  SysUtils, Classes, Graphics;

type
  TPointArray = array of TPoint;
  TCardinalArray = array of Cardinal;
  TColorRec = packed array[0..3] of Byte;

  TStegException = class(Exception);

  TStegProgressEvent = procedure(Sender: TObject; const Done, Size: integer; var Abort: boolean) of object;

  TStegImage = class(TPicture)
  private
    fBitmap: TBitmap;
    fCoords: TPointArray;
    fPassword: string;
    fData: TMemoryStream;
    fOccRatio: double;
    fOnProgress: TStegProgressEvent;
//    fPixelSize: integer;
    procedure InitCoords;
    procedure RandomizeCoords;
    procedure FillCoords;
    procedure SetCoords(Value: TPointArray);
    function GetCapacity: Cardinal;
    function GenKey: TCardinalArray;
    procedure SetData(Value: TStream);
    function GetData: TStream;
    procedure EmbedByte(const Data: Byte; var Coord, CurByte: integer; var PixelCol: TColorRec);
    procedure EmbedInt(const Data: Cardinal; var Coord, CurByte: integer; var PixelCol: TColorRec);
    function ExtractByte(var Coord, CurByte: integer; var PixelCol: TColorRec): Byte;
    function ExtractInt(var Coord, CurByte: integer; var PixelCol: TColorRec): Cardinal;
    function GetOccupiedRatio: double;
  protected
    function GetPixelSize: integer;
    procedure DoOnProgress(const Done, Size: integer; var Abort: boolean); virtual;
  public
    class function GetSuppFormatFilter: string;
    constructor Create;
    destructor Destroy; override;

    procedure Embed;
    procedure Extract;
    procedure ClearData;
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure Assign(Source: TPersistent); override;
    procedure LoadDataFromFile(const AFilename: string);
    procedure LoadDataFromString(const AData: string);
    procedure SaveDataToFile(const AFilename: string);
    procedure LoadDataFromStream(Stream: TStream);
    procedure SaveDataToStream(Stream: TStream);
    function GetDataAsString: string;

    property Capacity: Cardinal read GetCapacity;
    property OccupiedRatio: double read GetOccupiedRatio;
    property Data: TStream read GetData write SetData;
    property Coords: TPointArray read fCoords write SetCoords;
  published
    property Password: string read fPassword write fPassword;
    property OnProgress: TStegProgressEvent read fOnProgress write fOnProgress;
  end;

implementation

uses
  Isaac, HashSHA512, StegLang;

type
  TIntArr = packed array[0..SizeOf(Cardinal)-1] of Byte;
{$IFDEF FPC}
  TPngImageClass = TPortableNetworkGraphic;
{$ELSE}
  TPngImageClass = TPngImage;
{$ENDIF}

const
  // Supported pixel format
  SuppPixelFormats: set of TPixelFormat = [{pf16bit, }pf24bit{, pf32bit}];

constructor TStegImage.Create;
begin
  inherited;
  fBitmap := TBitmap.Create;
  fData := TMemoryStream.Create;
  fPassword := '';
  fOccRatio := 0.0;
//  fPixelSize := 0;
end;

destructor TStegImage.Destroy;
begin
  fBitmap.Free;
  fData.Free;
  inherited;
end;

function TStegImage.GetPixelSize: integer;
//const
//  PixelSize: array[TPixelFormat] of Integer = (0,0,0,0,0,2,3,4,0);
begin
  // Get the size of one pixel
//  if fPixelSize = 0 then begin
//    fPixelSize := PixelSize[fBitmap.PixelFormat];
//  end;
//  Result := fPixelSize;
  Result := 3;
end;

class function TStegImage.GetSuppFormatFilter: string;
begin
  // Get filefilter for supported formats
  Result := 'Supported files (*.bmp;*.png)|*.bmp;*.png';
end;

procedure TStegImage.DoOnProgress(const Done, Size: integer; var Abort: boolean);
begin
  if Assigned(fOnProgress) then
    fOnProgress(Self, Done, Size, Abort);
end;

procedure TStegImage.InitCoords;
begin
  SetLength(fCoords, fBitmap.Width * fBitmap.Height);
  FillCoords;
  RandomizeCoords;
end;

procedure TStegImage.RandomizeCoords;
var
  i: integer;
  ix: integer;
  tmp: TPoint;
  r: TIsaac;
  key: TCardinalArray;
  used: array of Boolean;
begin
  if fPassword = '' then
    raise TStegException.Create(SPasswordEmpty);
  key := GenKey;
  r := TIsaac.Create(key);
  try
    SetLength(used, High(fCoords)+1);
    for i := 0 to High(fCoords) do begin
      ix := r.IntVal(High(fCoords));
      if not used[ix] then begin
        // Use each pixel only once
        tmp := fCoords[i];
        fCoords[i] := fCoords[ix];
        fCoords[ix] := tmp;
        used[ix] := true;
      end;
    end;
  finally
    r.Free;
  end;
end;

procedure TStegImage.FillCoords;
var
  i, j, c: integer;
begin
  c := 0;
  for i := 0 to fBitmap.Width-1 do begin
    for j := 0 to fBitmap.Height-1 do begin
      fCoords[c] := Point(i, j);
      Inc(c);
    end;
  end;
end;

procedure TStegImage.SetCoords(Value: TPointArray);
begin
  // Set coordinates to use, should not be changed
  SetLength(fCoords, High(Value));
  Move(Value, fCoords, SizeOf(Value));
end;

procedure TStegImage.EmbedInt(const Data: Cardinal; var Coord, CurByte: integer; var PixelCol: TColorRec);
var
  i: integer;
begin
  // Embed an integer
  for i := 0 to SizeOf(Cardinal)-1 do
    EmbedByte(TIntArr(Data)[i], Coord, CurByte, PixelCol);
end;

procedure TStegImage.EmbedByte(const Data: Byte; var Coord, CurByte: integer; var PixelCol: TColorRec);
var
  bit: Byte;
  i: integer;
  p: TPoint;
  ps: integer;
begin
  // embed one byte
  ps := GetPixelSize;
  p := fCoords[Coord];
  for i := 1 to 8 do begin
    if Coord > High(fCoords) then
      raise TStegException.Create(SPicTooSmall);

    bit := (Data shr (i-1)) and 1;
    if CurByte = 0 then
      // Grab next pixel
      pixelcol := TColorRec(fBitmap.Canvas.Pixels[p.x, p.y]);
    // Set or unset the bit
    if bit <> 0 then
      pixelcol[CurByte] := pixelcol[CurByte] or 1
    else
      pixelcol[CurByte] := pixelcol[CurByte] and (not 1);

    Inc(CurByte);
    if (CurByte = ps) then begin
      // Store pixel
      fBitmap.Canvas.Pixels[p.x, p.y] := TColor(pixelcol);
      // Get next pixel
      Inc(Coord);
      p := fCoords[Coord];
      // Start with byte 0
      CurByte := 0;
    end;
  end;
  if (CurByte <> ps) then begin
    // Store remaining
    fBitmap.Canvas.Pixels[p.x, p.y] := TColor(pixelcol);
  end;
end;

procedure TStegImage.Embed;
var
  data: Byte;
  cc: integer;
  curbyte: integer;
  pixel: TColorRec;
  size: integer;
  ab: boolean;
begin
  // hide fData inside the pictrue
  if fBitmap.Empty then
    raise TStegException.Create(SBitmapEmpty);
  if fData.Size = 0 then
    raise TStegException.Create(SNothingToHide);
  InitCoords;

  size := fData.Size;
  ab := false;
  fData.Position := 0;
  cc := Low(fCoords);
  curbyte := 0;
  EmbedInt(size, cc, curbyte, pixel);
  data := 0;
  while fData.Read(data, 1) <> 0 do begin
    EmbedByte(data, cc, curbyte, pixel);
    DoOnProgress(fData.Position, size, ab);
    if ab then
      Break;
  end;
end;

function TStegImage.ExtractByte(var Coord, CurByte: integer; var PixelCol: TColorRec): Byte;
var
  p: TPoint;
  i, ps: integer;
begin
  // Extract one byte
  Result := 0;
  ps := GetPixelSize;
  p := fCoords[Coord];
  for i := 1 to 8 do begin
    if Coord > High(fCoords) then
      raise TStegException.Create(SPicTooSmall);

    if CurByte = 0 then
      PixelCol := TColorRec(fBitmap.Canvas.Pixels[p.x, p.y]);
    Result := Result + (Byte(PixelCol[CurByte] and 1) shl (i - 1));
    Inc(CurByte);
    if (CurByte = ps) then begin
      Inc(Coord);
      p := fCoords[Coord];
      CurByte := 0;
    end;
  end;
end;

function TStegImage.ExtractInt(var Coord, CurByte: integer; var PixelCol: TColorRec): Cardinal;
var
  i: integer;
begin
  // Extract and integer
  for i := 0 to SizeOf(Cardinal)-1 do
    TIntArr(Result)[i] := ExtractByte(Coord, CurByte, PixelCol);
end;

procedure TStegImage.Extract;
var
  cc: integer;
  curbyte: integer;
  size: Integer;
  pixel: TColorRec;
  i: integer;
  data: Byte;
  ab: boolean;
begin
  // Extract hidden data from the picture
  if fBitmap.Empty then
    raise TStegException.Create(SBitmapEmpty);
  InitCoords;

  fData.Clear;
  cc := Low(fCoords);
  curbyte := 0;
  try
    size := ExtractInt(cc, curbyte, pixel);
    if (size < 0) or (Cardinal(size) > Capacity) then
      // Size does not make sense
      raise TStegException.Create(SNoData);
    fData.Size := size;
    ab := false;
    for i := 0 to size-1 do begin
      data := ExtractByte(cc, curbyte, pixel);
      fData.Write(data, 1);
      DoOnProgress(i+1, size, ab);
      if ab then
        Break;
    end;
  except
    raise TStegException.Create(SNoData);
  end;
end;

function TStegImage.GetCapacity: Cardinal;
begin
{ Take for example a True-Color BMP image file format. A color of pixel is coded
  in 3 byte array of indices to RGB palete. If you change only LSB bit in each
  color element, then the picture will seem still the same, but is not. It carries
  hidden information. A picture with size 120x100 pixels can hold approximately
  up to 4500B of hidden data, if this method is used. }
  if fBitmap.Empty then
    Result := 0
  else
    Result := ((fBitmap.Width * fBitmap.Height * GetPixelSize) div 8) - SizeOf(Integer);
end;

function TStegImage.GenKey: TCardinalArray;
var
  r: TIsaac;
  seed: TCardinalArray;
  i: integer;
  d: array[0..63] of Byte;
begin
  // Genetrate a pseudo-random key
  THashSHA512.CalcString(fPassword, THashSHA512, d);
  SetLength(seed, High(d)+1);
  for i := 0 to High(d) do
    seed[i] := Integer(d[i]);
  SetLength(Result, 256);
  r := TIsaac.Create(seed);
  try
    for i := Low(Result) to High(Result) do
      Result[i] := r.Val;
  finally
    r.Free;
  end;
end;

procedure TStegImage.SetData(Value: TStream);
begin
  fData.LoadFromStream(Value);
  fOccRatio := 0.0;
end;

function TStegImage.GetData: TStream;
begin
  Result := fData;
end;

procedure TStegImage.ClearData;
begin
  // Clear data to hide
  fData.Clear;
  fOccRatio := 0.0;
end;

procedure TStegImage.LoadFromFile(const Filename: string);
begin
  fOccRatio := 0.0;
  // Let TPicture decide what type and let load it
  inherited;
  // If it is not a supported type raise an exception
  if (not ((Graphic is TBitmap) or (Graphic is TPngImageClass))) then
    raise TStegException.Create(SUnsuppPictrue);
  // Assign it to the bitmap
  fBitmap.Assign(Graphic);
  // If Pixelformat is not supported clear bitmap and raise an exception
{  if not (fBitmap.PixelFormat in SuppPixelFormats) then begin
    fBitmap.Assign(nil);
    raise TStegException.Create(SUnsuppPixelFormat);
  end;}
end;

procedure TStegImage.SaveToFile(const Filename: string);
begin
  // Assign the bitmap to the graphics
  Graphic.Assign(fBitmap);
  // Let TPicture save the file
  inherited SaveToFile(Filename);
end;

procedure TStegImage.Assign(Source: TPersistent);
begin
  fOccRatio := 0.0;
  // See TStegImage.LoadFromFile
  inherited;
  if (not ((Graphic is TBitmap) or (Graphic is TPngImageClass))) then
    raise TStegException.Create(SUnsuppPictrue);
  fBitmap.Assign(Graphic);
  if not (fBitmap.PixelFormat in SuppPixelFormats) then begin
    fBitmap.Assign(nil);
    raise TStegException.Create(SUnsuppPixelFormat);
  end;
end;

procedure TStegImage.LoadDataFromFile(const AFilename: string);
var
  fs: TFileStream;
begin
  fOccRatio := 0.0;
  // Load the data to hide from a file
  fs := TFileStream.Create(AFilename, fmOpenRead);
  try
    fData.Clear;
    fData.LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TStegImage.LoadDataFromString(const AData: string);
begin
  fOccRatio := 0.0;
  fData.Clear;
  fData.WriteBuffer(AData[1], Length(AData) * SizeOf(Char));
end;

function TStegImage.GetDataAsString: string;
begin
  fData.Position := 0;
  SetString(Result, PChar(fData.Memory), fData.Size div SizeOf(Char));
end;

procedure TStegImage.SaveDataToFile(const AFilename: string);
var
  fs: TFileStream;
begin
  // Save the extracted data to a file
  fs := TFileStream.Create(AFilename, fmCreate);
  try
    fData.SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

function TStegImage.GetOccupiedRatio: double;
begin
  if fOccRatio = 0.0 then
    fOccRatio := fData.Size / GetCapacity;
  Result := fOccRatio;
end;

procedure TStegImage.LoadDataFromStream(Stream: TStream);
begin
  fData.Clear;
  fData.LoadFromStream(Stream);
end;

procedure TStegImage.SaveDataToStream(Stream: TStream);
begin
  fData.SaveToStream(Stream);
end;

end.
