{
  Hash functions
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
@abstract(The @name unit provides classes and types for calculating hashes.)
@author(Stefan Ascher)
@created(2 Dec 2007)
}

unit Hash;

interface

uses
  SysUtils, Classes;

type
  THash = class;
  THashClass = class of THash;

  PHashType = ^THashType;
  THashType = record
    HashClass: THashClass;
    Name: string;
    Size: Integer;
  end;

  {@abstract(Abstract base class for Hash classes.)}
  THash = class
  public
    procedure Init; virtual; abstract;
    procedure Update(const Buffer; Size: Cardinal); virtual; abstract;
    procedure Final(var Digest: array of Byte); virtual; abstract;
    procedure Burn; virtual; abstract;
    procedure UpdateStr(const AString: string); virtual;
    function SelfTest: boolean; virtual;

    // Helper methods
    procedure HashFile(const AFilename: string; var Digest: array of Byte);
    procedure HashString(const AString: string; var Digest: array of Byte);
    class procedure CalcFile(const AFilename: string; AHash: THashClass; var Digest: array of Byte);
    class procedure CalcString(const AString: string; AHash: THashClass; var Digest: array of Byte);
    class function DigestToString(const Digest: array of Byte): string;

    // Hash register methods
    class procedure RegisterHashType(const AName: string; AHashClass: THashClass;
      ASize: integer);
    class procedure UnRegisterHashType(AHashClass: THashClass);
    class function GetHashTypesCount: integer;
    class function GetHashType(const Index: integer): PHashType;
  end;

implementation
{$R-}{$Q-}

const
  BuffSize = 4096;

type
  THashTypesList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    function FindClassName(const Classname: string): THashClass;
    procedure Remove(AClass: THashClass);
  end;

{ THashTypesList }

constructor THashTypesList.Create;
begin
  inherited;
end;

destructor THashTypesList.Destroy;
var
  i: integer;
  h: PHashType;
begin
  for i := 0 to Count-1 do begin
    h := Items[i];
    Dispose(h);
  end;
  inherited;
end;

function THashTypesList.FindClassName(const Classname: string): THashClass;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
  begin
    Result := PHashType(Items[I])^.HashClass;
    if Result.ClassName = Classname then Exit;
  end;
  Result := nil;
end;

procedure THashTypesList.Remove(AClass: THashClass);
var
  I: Integer;
  P: PHashType;
begin
  for I := Count-1 downto 0 do
  begin
    P := PHashType(Items[I]);
    if P^.HashClass.InheritsFrom(AClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

{ THash }

class function THash.DigestToString(const Digest: array of Byte): string;
var
  i: integer;
begin
  Result := '';
  for i := Low(Digest) to High(Digest) do
    Result := Result + IntToHex(integer(Digest[i]), 2);
end;

procedure THash.HashFile(const AFilename: string; var Digest: array of Byte);
var
  fs: TFileStream;
  buff: array[0..BuffSize-1] of Byte;
  read: Longint;
begin
  fs := TFileStream.Create(AFilename, fmOpenRead);
  try
    Init;
    repeat
      read := fs.Read(buff, BuffSize);
      if read > 0 then
        Update(buff, read);
    until read = 0;
    Final(Digest);
  finally
    fs.Free;
  end;
end;

procedure THash.HashString(const AString: string; var Digest: array of Byte);
begin
  Init;
  Update(AString[1], Length(AString));
  Final(Digest);
end;

class procedure THash.CalcFile(const AFilename: string; AHash: THashClass; var Digest: array of Byte);
var
  h: THash;
begin
  h := AHash.Create;
  try
    h.HashFile(AFilename, Digest);
  finally
    h.Free;
  end;
end;

class procedure THash.CalcString(const AString: string; AHash: THashClass; var Digest: array of Byte);
var
  h: THash;
begin
  h := AHash.Create;
  try
    h.HashString(AString, Digest);
  finally
    h.Free;
  end;
end;

procedure THash.UpdateStr(const AString: string);
begin
  Update(AString[1], Length(AString));
end;

function THash.SelfTest: boolean;
begin
  Result := false;
end;

var
  HashTypes: THashTypesList = nil;

function GetHashTypes: THashTypesList;
begin
  if HashTypes = nil then HashTypes := THashTypesList.Create;
  Result := HashTypes;
end;

class procedure THash.RegisterHashType(const AName: string; AHashClass: THashClass;
  ASize: integer);
var
  h: PHashType;
begin
  New(h);
  h^.Name := AName;
  h^.HashClass := AHashClass;
  h^.Size := ASize;
  GetHashTypes.Add(h);
end;

class procedure THash.UnRegisterHashType(AHashClass: THashClass);
begin
  if HashTypes <> nil then
    HashTypes.Remove(AHashClass);
end;

class function THash.GetHashTypesCount: integer;
begin
  if HashTypes <> nil then
    Result := HashTypes.Count
  else
    Result := 0;
end;

class function THash.GetHashType(const Index: integer): PHashType;
begin
  if HashTypes <> nil then
    Result := PHashType(HashTypes[Index])
  else
    Result := nil;
end;

initialization

finalization
  if HashTypes <> nil then
    FreeAndNil(HashTypes);
end.
