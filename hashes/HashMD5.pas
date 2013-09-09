{
  Hash functions
  Copyright (c) 1999-2002 David Barton
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
  Reimplementation of the hash functions from DCPcrypt
  <http://www.cityinthesky.co.uk/cryptography.html> by David Barton
}

unit HashMD5;

interface

uses
  SysUtils, Classes, Windows, Hash;

type
  THashMD5 = class(THash)
  private
    procedure Compress;
  protected
    fLenHi, fLenLo: Cardinal;
    fIndex: DWord;
    fCurHash: array[0..3] of DWord;
    fHashBuff: array[0..63] of byte;
  public
    procedure Init; override;
    procedure Update(const Buffer; Size: Cardinal); override;
    procedure Final(var Digest: array of Byte); override;
    function SelfTest: boolean; override;

    procedure Burn; override;
  end;
  THashMD5Class = class of THashMD5;

implementation
{$R-}{$Q-}

function LRot32(const a, b: longword): longword;
begin
  Result:= (a shl b) or (a shr (32-b));
end;

function THashMD5.SelfTest: boolean;
const
  Test1Out: array[0..15] of byte=
    ($90,$01,$50,$98,$3c,$d2,$4f,$b0,$d6,$96,$3f,$7d,$28,$e1,$7f,$72);
  Test2Out: array[0..15] of byte=
    ($c3,$fc,$d3,$d7,$61,$92,$e4,$00,$7d,$fb,$49,$6c,$ca,$67,$e1,$3b);
var
  TestOut: array[0..19] of byte;
begin
  Init;
  UpdateStr('abc');
  Final(TestOut);
  Result:= CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out));
  Init;
  UpdateStr('abcdefghijklmnopqrstuvwxyz');
  Final(TestOut);
  Result:= CompareMem(@TestOut,@Test2Out,Sizeof(Test2Out)) and Result;
end;

procedure THashMD5.Compress;
var
  Data: array[0..15] of dword;
  A, B, C, D: dword;
begin
  Move(fHashBuff, Data, SizeOf(Data));
  A:= fCurHash[0];
  B:= fCurHash[1];
  C:= fCurHash[2];
  D:= fCurHash[3];

  A:= B + LRot32(A + (D xor (B and (C xor D))) + Data[ 0] + $d76aa478,7);
  D:= A + LRot32(D + (C xor (A and (B xor C))) + Data[ 1] + $e8c7b756,12);
  C:= D + LRot32(C + (B xor (D and (A xor B))) + Data[ 2] + $242070db,17);
  B:= C + LRot32(B + (A xor (C and (D xor A))) + Data[ 3] + $c1bdceee,22);
  A:= B + LRot32(A + (D xor (B and (C xor D))) + Data[ 4] + $f57c0faf,7);
  D:= A + LRot32(D + (C xor (A and (B xor C))) + Data[ 5] + $4787c62a,12);
  C:= D + LRot32(C + (B xor (D and (A xor B))) + Data[ 6] + $a8304613,17);
  B:= C + LRot32(B + (A xor (C and (D xor A))) + Data[ 7] + $fd469501,22);
  A:= B + LRot32(A + (D xor (B and (C xor D))) + Data[ 8] + $698098d8,7);
  D:= A + LRot32(D + (C xor (A and (B xor C))) + Data[ 9] + $8b44f7af,12);
  C:= D + LRot32(C + (B xor (D and (A xor B))) + Data[10] + $ffff5bb1,17);
  B:= C + LRot32(B + (A xor (C and (D xor A))) + Data[11] + $895cd7be,22);
  A:= B + LRot32(A + (D xor (B and (C xor D))) + Data[12] + $6b901122,7);
  D:= A + LRot32(D + (C xor (A and (B xor C))) + Data[13] + $fd987193,12);
  C:= D + LRot32(C + (B xor (D and (A xor B))) + Data[14] + $a679438e,17);
  B:= C + LRot32(B + (A xor (C and (D xor A))) + Data[15] + $49b40821,22);

  A:= B + LRot32(A + (C xor (D and (B xor C))) + Data[ 1] + $f61e2562,5);
  D:= A + LRot32(D + (B xor (C and (A xor B))) + Data[ 6] + $c040b340,9);
  C:= D + LRot32(C + (A xor (B and (D xor A))) + Data[11] + $265e5a51,14);
  B:= C + LRot32(B + (D xor (A and (C xor D))) + Data[ 0] + $e9b6c7aa,20);
  A:= B + LRot32(A + (C xor (D and (B xor C))) + Data[ 5] + $d62f105d,5);
  D:= A + LRot32(D + (B xor (C and (A xor B))) + Data[10] + $02441453,9);
  C:= D + LRot32(C + (A xor (B and (D xor A))) + Data[15] + $d8a1e681,14);
  B:= C + LRot32(B + (D xor (A and (C xor D))) + Data[ 4] + $e7d3fbc8,20);
  A:= B + LRot32(A + (C xor (D and (B xor C))) + Data[ 9] + $21e1cde6,5);
  D:= A + LRot32(D + (B xor (C and (A xor B))) + Data[14] + $c33707d6,9);
  C:= D + LRot32(C + (A xor (B and (D xor A))) + Data[ 3] + $f4d50d87,14);
  B:= C + LRot32(B + (D xor (A and (C xor D))) + Data[ 8] + $455a14ed,20);
  A:= B + LRot32(A + (C xor (D and (B xor C))) + Data[13] + $a9e3e905,5);
  D:= A + LRot32(D + (B xor (C and (A xor B))) + Data[ 2] + $fcefa3f8,9);
  C:= D + LRot32(C + (A xor (B and (D xor A))) + Data[ 7] + $676f02d9,14);
  B:= C + LRot32(B + (D xor (A and (C xor D))) + Data[12] + $8d2a4c8a,20);

  A:= B + LRot32(A + (B xor C xor D) + Data[ 5] + $fffa3942,4);
  D:= A + LRot32(D + (A xor B xor C) + Data[ 8] + $8771f681,11);
  C:= D + LRot32(C + (D xor A xor B) + Data[11] + $6d9d6122,16);
  B:= C + LRot32(B + (C xor D xor A) + Data[14] + $fde5380c,23);
  A:= B + LRot32(A + (B xor C xor D) + Data[ 1] + $a4beea44,4);
  D:= A + LRot32(D + (A xor B xor C) + Data[ 4] + $4bdecfa9,11);
  C:= D + LRot32(C + (D xor A xor B) + Data[ 7] + $f6bb4b60,16);
  B:= C + LRot32(B + (C xor D xor A) + Data[10] + $bebfbc70,23);
  A:= B + LRot32(A + (B xor C xor D) + Data[13] + $289b7ec6,4);
  D:= A + LRot32(D + (A xor B xor C) + Data[ 0] + $eaa127fa,11);
  C:= D + LRot32(C + (D xor A xor B) + Data[ 3] + $d4ef3085,16);
  B:= C + LRot32(B + (C xor D xor A) + Data[ 6] + $04881d05,23);
  A:= B + LRot32(A + (B xor C xor D) + Data[ 9] + $d9d4d039,4);
  D:= A + LRot32(D + (A xor B xor C) + Data[12] + $e6db99e5,11);
  C:= D + LRot32(C + (D xor A xor B) + Data[15] + $1fa27cf8,16);
  B:= C + LRot32(B + (C xor D xor A) + Data[ 2] + $c4ac5665,23);

  A:= B + LRot32(A + (C xor (B or (not D))) + Data[ 0] + $f4292244,6);
  D:= A + LRot32(D + (B xor (A or (not C))) + Data[ 7] + $432aff97,10);
  C:= D + LRot32(C + (A xor (D or (not B))) + Data[14] + $ab9423a7,15);
  B:= C + LRot32(B + (D xor (C or (not A))) + Data[ 5] + $fc93a039,21);
  A:= B + LRot32(A + (C xor (B or (not D))) + Data[12] + $655b59c3,6);
  D:= A + LRot32(D + (B xor (A or (not C))) + Data[ 3] + $8f0ccc92,10);
  C:= D + LRot32(C + (A xor (D or (not B))) + Data[10] + $ffeff47d,15);
  B:= C + LRot32(B + (D xor (C or (not A))) + Data[ 1] + $85845dd1,21);
  A:= B + LRot32(A + (C xor (B or (not D))) + Data[ 8] + $6fa87e4f,6);
  D:= A + LRot32(D + (B xor (A or (not C))) + Data[15] + $fe2ce6e0,10);
  C:= D + LRot32(C + (A xor (D or (not B))) + Data[ 6] + $a3014314,15);
  B:= C + LRot32(B + (D xor (C or (not A))) + Data[13] + $4e0811a1,21);
  A:= B + LRot32(A + (C xor (B or (not D))) + Data[ 4] + $f7537e82,6);
  D:= A + LRot32(D + (B xor (A or (not C))) + Data[11] + $bd3af235,10);
  C:= D + LRot32(C + (A xor (D or (not B))) + Data[ 2] + $2ad7d2bb,15);
  B:= C + LRot32(B + (D xor (C or (not A))) + Data[ 9] + $eb86d391,21);

  Inc(fCurHash[0], A);
  Inc(fCurHash[1], B);
  Inc(fCurHash[2], C);
  Inc(fCurHash[3], D);
  fIndex := 0;
  FillChar(fHashBuff, SizeOf(fHashBuff), 0);
end;

procedure THashMD5.Init;
begin
  Burn;
  fCurHash[0]:= $67452301;
  fCurHash[1]:= $efcdab89;
  fCurHash[2]:= $98badcfe;
  fCurHash[3]:= $10325476;
end;

procedure THashMD5.Update(const Buffer; Size: Cardinal);
var
  PBuf: PByte;
begin
  Inc(fLenHi, Size shr 29);
  Inc(fLenLo, Size * 8);
  if fLenLo < (Size * 8) then
    Inc(fLenHi);

  PBuf:= @Buffer;
  while Size> 0 do begin
    if (SizeOf(fHashBuff) - fIndex) <= DWord(Size) then begin
      Move(PBuf^, fHashBuff[fIndex], SizeOf(fHashBuff) - fIndex);
      Dec(Size, SizeOf(fHashBuff) - fIndex);
      Inc(PBuf, SizeOf(fHashBuff) - fIndex);
      Compress;
    end else begin
      Move(PBuf^, fHashBuff[fIndex], Size);
      Inc(fIndex, Size);
      Size := 0;
    end;
  end;
end;

procedure THashMD5.Final(var Digest: array of Byte);
begin
  fHashBuff[fIndex]:= $80;
  if fIndex >= 56 then
    Compress;
  PDWord(@fHashBuff[56])^ := fLenLo;
  PDWord(@fHashBuff[60])^ := fLenHi;
  Compress;
  Move(fCurHash, Digest, SizeOf(fCurHash));
  Burn;
end;

procedure THashMD5.Burn; 
begin
  fLenHi := 0; fLenLo := 0;
  fIndex := 0;
  FillChar(fHashBuff, SizeOf(fHashBuff), 0);
  FillChar(fCurHash, Sizeof(fCurHash), 0);
end;

initialization
  THash.RegisterHashType('MD5', THashMD5, SizeOf(DWord) * 4);

end.
