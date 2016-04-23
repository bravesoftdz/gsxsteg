{
  gsxsteg
  2016, sa
}

unit Histogram;

{$mode objfpc}{$H+}
{$i gsx.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, Forms, StegHisto, LMessages;

type
  THistogram = class(TCustomControl)
  private
    FStegHisto: TStegHisto;
    procedure SetStegHisto(Value: TStegHisto);
  protected
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property StegHisto: TStegHisto read FStegHisto write SetStegHisto;
  published
    property Align;
  end;

implementation

constructor THistogram.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csOpaque, csClickEvents, csDoubleClicks,
    csNeedsBorderPaint];
  ParentColor := false;
  Color := clWindow;
  DoubleBuffered := true;
end;

procedure THistogram.SetStegHisto(Value: TStegHisto);
begin
  if Value <> FStegHisto then begin
    FStegHisto := Value;
    Invalidate;
  end;
end;

procedure THistogram.WMSize(var Message: TLMSize);
begin
  inherited;
  Invalidate;
end;

procedure THistogram.Paint;
var
  cr: TRect;
  x, y, i: integer;
  v: integer;
  cw, ch: integer;
  mr, mg, mb: integer;
  s: string;
begin
  inherited;
	cr := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(cr);

  if (csDesigning in ComponentState) or not Assigned(FStegHisto) then
    Exit;

  Canvas.Pen.Width := 1;

  cw := cr.Right - cr.Left - 2;
  ch := cr.Bottom - cr.Top - 2;

  mr := FStegHisto.MaxRed;
  mg := FStegHisto.MaxGreen;
  mb := FStegHisto.MaxBlue;

  Canvas.MoveTo(1, ch);
  Canvas.Pen.Color := clRed;
  for i := 0 to 255 do begin
    v := FStegHisto.Red[i];
    x := Round((cw / 255) * i) + 1;
    y := ch - Round((ch / mr) * v) + 1;
    Canvas.LineTo(x, y);
  end;
  Canvas.MoveTo(1, ch);
  Canvas.Pen.Color := clGreen;
  for i := 0 to 255 do begin
    v := FStegHisto.Green[i];
    x := Round((cw / 255) * i) + 1;
    y := ch - Round((ch / mg) * v) + 1;
    Canvas.LineTo(x, y);
  end;
  Canvas.MoveTo(1, ch);
  Canvas.Pen.Color := clBlue;
  for i := 0 to 255 do begin
    v := FStegHisto.Blue[i];
    x := Round((cw / 255) * i) + 1;
    y := ch - Round((ch / mb) * v) + 1;
    Canvas.LineTo(x, y);
  end;
  Canvas.Font.Color := clRed;
  y := 10;
  s := Format('Mean %.2f, StdDev %.2f', [FStegHisto.MeanRed, FStegHisto.StdDevRed]);
  x := cw - (Canvas.TextWidth(s) + 10);
  Canvas.TextOut(x, y, s);
  Inc(y, Canvas.TextHeight('^_g') + 2);
  Canvas.Font.Color := clGreen;
  Canvas.TextOut(x, y, Format('Mean %.2f, StdDev %.2f', [FStegHisto.MeanGreen, FStegHisto.StdDevGreen]));
  Inc(y, Canvas.TextHeight('^_g') + 2);
  Canvas.Font.Color := clBlue;
  Canvas.TextOut(x, y, Format('Mean %.2f, StdDev %.2f', [FStegHisto.MeanBlue, FStegHisto.StdDevBlue]));
end;

end.

