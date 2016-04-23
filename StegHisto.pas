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
  LCLType, LCLIntf,
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
    FStdDevRed, FStdDevGreen, FStdDevBlue: double;

    FBitmap: TBitmap;
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
    property StdDevRed: double read FStdDevRed;
    property StdDevGreen: double read FStdDevGreen;
    property StdDevBlue: double read FStdDevBlue;
  end;

implementation

uses
  Math;

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
  FBitmap := TBitmap.Create;
end;

destructor TStegHisto.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TStegHisto.SetBitmap(Value: TBitmap);
begin
  FBitmap.Assign(Value);
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
  x, y, i: integer;
  bh, bw: integer;
  PixelCol: TRGBTriple;
  sr, sg, sb: integer;
begin
  if FBitmap.Empty then
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
  FStdDevRed := 0.0;
  FStdDevGreen := 0.0;
  FStdDevBlue := 0.0;

  sr := 0; sg := 0; sb := 0;
  bw := FBitmap.Width;
  bh := FBitmap.Height;
  for x := 0 to bw - 1 do begin
    for y := 0 to bh - 1 do begin
      PixelCol := ColorToRGB(fBitmap.Canvas.Pixels[x, y]);
      Inc(FRed[PixelCol.rgbtRed]);
      Inc(sr, PixelCol.rgbtRed);
      Inc(FGreen[PixelCol.rgbtGreen]);
      Inc(sg, PixelCol.rgbtGreen);
      Inc(FBlue[PixelCol.rgbtBlue]);
      Inc(sb, PixelCol.rgbtBlue);
    end;
  end;
  FMeanRed := sr / (bw * bh);
  FMeanGreen := sg / (bw * bh);
  FMeanBlue := sb / (bw * bh);

  for i := 0 to 255 do begin
    FMaxRed := Max(FMaxRed, FRed[i]);
    FMaxGreen := Max(FMaxGreen, FGreen[i]);
    FMaxBlue := Max(FMaxBlue, FBlue[i]);
    FStdDevRed := FRed[i] - FMeanRed;
    FStdDevGreen := FGreen[i] - FMeanGreen;
    FStdDevBlue := FBlue[i] - FMeanBlue;
  end;
  FStdDevRed := Sqrt(Abs(FStdDevRed));
  FStdDevGreen := Sqrt(Abs(FStdDevGreen));
  FStdDevBlue := Sqrt(Abs(FStdDevBlue));
end;

end.

