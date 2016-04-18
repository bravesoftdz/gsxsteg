{
  GPGSX
  Copyright (C) 2007, Ascher Stefan. All rights reserved.
  stievie@inode.at, http://stievie.bplaced.net/gpgsx/

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

program gsxsteg;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{$IFDEF WINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

{$i gsx.inc}

uses
  SysUtils,
  Classes,
  StegImage in 'StegImage.pas';

const
  VERSION = '1.0.1';

type
  TStegAction = (saUnknown, saHide, saExtract);

{$i gsxstegstrs_en.inc}

var
  // Batch mode
  bBatch: boolean = false;
  // What to do
  Action: TStegAction = saUnknown;
  // Source medium
  sMedium: string = '';
  sDest: string = '';
  sHideFile: string = '';
  sPassword: string = '';

procedure ShowCopy;
begin
  WriteLn(Format(SVersion, [VERSION]));
  WriteLn(SCopy);
  WriteLn(SInfo);
  WriteLn;
end;

procedure ShowUsage;
begin
  WriteLn(SHelp);
end;

function GetFileSize(Filename: string): integer;
var
  s: TSearchRec;
begin
  if FindFirst(Filename, faAnyfile, s) = 0 then begin
    Result := s.Size;
    SysUtils.FindClose(s);
  end else
    Result := 0;
end;

function GetValue(const Name, Initial: string): string;
begin
  Write(Name, ' [', Initial, ']: ');
  ReadLn(Result);
  if (Result = '') and (Initial <> '') then
    Result := Initial;
end;

function HandleArgs: boolean;
var
  i: integer;
  s: string;
  si: TStegImage;
begin
  i := 1;
  while i <= ParamCount do begin
    s := ParamStr(i);
    if s <> '' then begin
      if CharInSet(s[1], ['-', '\']) then
        Delete(s, 1, 1);
      if SameText(s, 'help') or (s = '?') then begin
        ShowUsage;
        Halt(0);
      end else if SameText(s, 'batch') then
        bBatch := true
      else if SameText(s, 'hide') then
        Action := saHide
      else if SameText(s, 'extract') then
        Action := saExtract
      else if SameText(s, 'medium') then begin
        Inc(i);
        sMedium := ParamStr(i);
      end else if SameText(s, 'dest') then begin
        Inc(i);
        sDest := ParamStr(i);
      end else if SameText(s, 'msg') then begin
        Inc(i);
        sHideFile := ParamStr(i);
      end else if SameText(s, 'pass') then begin
        Inc(i);
        sPassword := ParamStr(i);
      end;
    end;
    Inc(i);
  end;
  Result := Action <> saUnknown;
  if not Result then
    Exit;

  if not bBatch then begin
    // Not batch mode get values from user
    sMedium := GetValue(StrMedium, sMedium);
    if Action = saHide then begin
      if FileExists(sMedium) then begin
        si := TStegImage.Create;
        try
          si.LoadFromFile(sMedium);
          WriteLn(Format(SCapacity, [si.Capacity]));
        finally
          si.Free;
        end;
      end else
        WriteLn(SMediumDoesNotExist);
    end;
    sDest := GetValue(StrDestination, sDest);
    if Action = saHide then begin
      sHideFile := GetValue(StrHide, sHideFile);
      if FileExists(sHideFile) then begin
        WriteLn(Format(SFileSize, [GetFileSize(sHideFile)]));
      end else begin
        WriteLn(Format(SFileSize, [Length(sHideFile) * SizeOf(Char)]));
      end;
    end;
    sPassword := GetValue(StrPassword, sPassword);
  end;
  Result := (sMedium <> '') and (sPassword <> '') and ((sHideFile <> '') or (Action = saExtract));
end;

function DoHide: boolean;
var
  si: TStegImage;
begin
  try
    si := TStegImage.Create;
    try
      si.LoadFromFile(sMedium);
      if FileExists(sHideFile) then
        si.LoadDataFromFile(sHideFile)
      else begin
        si.LoadDataFromString(sHideFile);
      end;
      si.Password := sPassword;
      si.Embed;
      si.SaveToFile(sDest);
      Result := true;
    finally
      si.Free;
    end;
  except
    on E: Exception do begin
      WriteLn(E.Message);
      Result := false;
    end;
  end;
end;

function DoExtract: boolean;
var
  si: TStegImage;
begin
  try
    si := TStegImage.Create;
    try
      si.LoadFromFile(sMedium);
      si.Password := sPassword;
      si.Extract;
      if (sDest = '') or (sDest = 'stdio') then
        WriteLn(si.GetDataAsString)
      else
        si.SaveDataToFile(sDest);
      Result := true;
    finally
      si.Free;
    end;
  except
    on E: Exception do begin
      WriteLn(E.Message);
      Result := false;
    end;
  end;
end;

function DoAction: boolean;
begin
  case Action of
    saHide: Result := DoHide;
    saExtract: Result := DoExtract;
    else Result := false;
  end;
end;

begin
  ShowCopy;
  if HandleArgs then begin
    if not DoAction then
      Halt(1);
  end else begin
    WriteLn;
    WriteLn(SWrongArgs);
    ShowUsage;
    Halt(1);
  end;
end.
