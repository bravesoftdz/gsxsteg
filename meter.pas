{
  gsxsteg
  2016, sa
}

unit meter;

{$mode objfpc}{$H+}
{$i gsx.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, LMessages, LCLType;

type
  TMeter = class(TPanel)
  private
    FValue: integer;
    procedure SetValue(Value: integer);
  protected
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Value: integer read FValue write SetValue;
  end;

implementation

uses
  Math;
  
const
  COLOR_TABLE: array[0..100] of Cardinal = (
    $0000ff,
    $0005ff,
    $000aff,
    $000fff,
    $0014ff,
    $0019ff,
    $001eff,
    $0023ff,
    $0028ff,
    $002dff,
    $0033ff,
    $0038ff,
    $003dff,
    $0042ff,
    $0047ff,
    $004cff,
    $0051ff,
    $0056ff,
    $005bff,
    $0060ff,
    $0066ff,
    $006bff,
    $0070ff,
    $0075ff,
    $007aff,
    $007fff,
    $0084ff,
    $0089ff,
    $008eff,
    $0093ff,
    $0099ff,
    $009eff,
    $00a3ff,
    $00a8ff,
    $00adff,
    $00b2ff,
    $00b7ff,
    $00bcff,
    $00c1ff,
    $00c6ff,
    $00ccff,
    $00d1ff,
    $00d6ff,
    $00dbff,
    $00e0ff,
    $00e5ff,
    $00eaff,
    $00efff,
    $00f4ff,
    $00f9ff,
    $00fffe,
    $00fff9,
    $00fff4,
    $00ffef,
    $00ffea,
    $00ffe5,
    $00ffe0,
    $00ffdb,
    $00ffd6,
    $00ffd1,
    $00ffcb,
    $00ffc6,
    $00ffc1,
    $00ffbc,
    $00ffb7,
    $00ffb2,
    $00ffad,
    $00ffa8,
    $00ffa3,
    $00ff9e,
    $00ff98,
    $00ff93,
    $00ff8e,
    $00ff89,
    $00ff84,
    $00ff7f,
    $00ff7a,
    $00ff75,
    $00ff70,
    $00ff6b,
    $00ff65,
    $00ff60,
    $00ff5b,
    $00ff56,
    $00ff51,
    $00ff4c,
    $00ff47,
    $00ff42,
    $00ff3d,
    $00ff38,
    $00ff32,
    $00ff2d,
    $00ff28,
    $00ff23,
    $00ff1e,
    $00ff19,
    $00ff14,
    $00ff0f,
    $00ff0a,
    $00ff05,
    $00ff00
  );

{ TMeter }

constructor TMeter.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csOpaque, csClickEvents, csDoubleClicks,
    csNeedsBorderPaint];
  ParentColor := false;
  Color := clWindow;
  DoubleBuffered := true;
end;

procedure TMeter.Paint;
var
  v: integer;
  cr: TRect;
  cw, ch: Integer;
begin
  cr := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(cr);
  Canvas.Brush.Color := TColor(COLOR_TABLE[100 - FValue]);
  cw := cr.Right - cr.Left;
  v := Trunc(cw * (FValue / 100));
  ch := cr.Bottom - cr.Top;
  Canvas.FillRect(0, 0, v, ch);
  inherited;
end;

procedure TMeter.WMSize(var Message: TLMSize);
begin
  inherited;
  Invalidate;
end;

procedure TMeter.SetValue(Value: integer);
begin
  if FValue <> Value then begin
    if (Value < 0) then
      FValue := 0
    else if (Value > 100) then
      FValue := 100
    else
      FValue := Value;
    Invalidate;
  end;
end;

end.

