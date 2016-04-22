{
  gsxsteg
  2016, sa

  RGB histogram
}

unit StegHisto;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$i gsx.inc}

interface

uses
  {$IFNDEF FPC}
  System.Types,
  {$ELSE}
  LCLType, LCLIntf, IntfGraphics,
  {$ENDIF}
  Classes, SysUtils, Graphics;

type
  TStegHisto = class
  private
    FRed: array[0..255] of Integer;
    FGreen: array[0..255] of Integer;
    FBlue: array[0..255] of Integer;
    FMaxRed, FMaxGreen, FMaxBlue: integer;
    FMeanRed, FMeanGreen, FMeanBlue: double;

    {$IFDEF FPC}
    FBitmap: TLazIntfImage;
    {$ELSE}
    FBitmap: TBitmap;
    {$ENDIF}
    procedure Process;
    function GetRed(Index: Byte): integer;
    function GetGreen(Index: Byte): integer;
    function GetBlue(Index: Byte): integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetBitmap(Value: TBitmap);

    property Red[Index: Byte]: integer read GetRed;
    property Green[Index: Byte]: integer read GetGreen;
    property Blue[Index: Byte]: integer read GetBlue;
    property MaxRed: integer read FMaxRed;
    property MaxGreen: integer read FMaxGreen;
    property MaxBlue: integer read FMaxBlue;
    property MeanRed: double read FMeanRed;
    property MeanGreen: double read FMeanGreen;
    property MeanBlue: double read FMeanBlue;
  end;

implementation

uses
  Math;

type
  TRGBTripleRow = array[0..High(Word)-1] of TRGBTriple;
  PRGBTripleRow = ^TRGBTripleRow;

function ColorToRGB(AColor: TColor): TRGBTriple;
begin
  with Result do begin
    rgbtRed := GetRValue(AColor);
    rgbtGreen := GetGValue(AColor);
    rgbtBlue := GetBValue(AColor);
  end;
end;

constructor TStegHisto.Create;
begin
  inherited;
  {$IFDEF FPC}
  FBitmap := TLazIntfImage.Create(0, 0);
  {$ELSE}
  FBitmap := TBitmap.Create;
  {$ENDIF}
end;

destructor TStegHisto.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TStegHisto.SetBitmap(Value: TBitmap);
begin
  {$IFDEF FPC}
  FBitmap.LoadFromBitmap(Value.Handle, Value.MaskHandle);
  {$ELSE}
  FBitmap.Assign(Value);
  {$ENDIF}
  Process;
end;

function TStegHisto.GetRed(Index: Byte): integer;
begin
  Result := FRed[Index];
end;

function TStegHisto.GetGreen(Index: Byte): integer;
begin
  Result := FGreen[Index];
end;

function TStegHisto.GetBlue(Index: Byte): integer;
begin
  Result := FBlue[Index];
end;

procedure TStegHisto.Process;
var
  x, y, i, w, pc: integer;
  sr, sg, sb: integer;
  pp: PRGBTripleRow;
begin
  if (FBitmap.Width = 0) or (FBitmap.Height = 0) then
    Exit;

  for i := 0 to 255 do begin
    FRed[i] := 0;
    FGreen[i] := 0;
    FBlue[i] := 0;
  end;
  FMaxRed := 0;
  FMaxGreen := 0;
  FMaxBlue := 0;
  FMeanRed := 0.0;
  FMeanGreen := 0.0;
  FMeanBlue := 0.0;

  sr := 0; sg := 0; sb := 0;
  w := FBitmap.Width;
  for y := 0 to FBitmap.Height - 1 do begin
    pp := FBitmap.{$IFDEF FPC}GetDataLineStart(y){$ELSE}ScanLine[y]{$ENDIF};
    for x := 0 to w - 1 do begin
      Inc(FRed[pp[x].rgbtRed]);
      Inc(sr, pp[x].rgbtRed);
      Inc(FGreen[pp[x].rgbtGreen]);
      Inc(sg, pp[x].rgbtGreen);
      Inc(FBlue[pp[x].rgbtBlue]);
      Inc(sb, pp[x].rgbtBlue);
    end;
  end;
  pc := (FBitmap.Width * FBitmap.Height);
  FMeanRed := sr / pc;
  FMeanGreen := sg / pc;
  FMeanBlue := sb / pc;

  for i := 0 to 255 do begin
    FMaxRed := Max(FMaxRed, FRed[i]);
    FMaxGreen := Max(FMaxGreen, FGreen[i]);
    FMaxBlue := Max(FMaxBlue, FBlue[i]);
  end;

end;

end.

