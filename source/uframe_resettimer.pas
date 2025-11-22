unit uframe_resettimer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls;

type

  { TFrameCircularTimer }

  TFrameCircularTimer = class(TFrame)
    btnStart: TButton;
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    FSecondsLeft: integer;
    procedure DrawCircle;
    procedure FinishTimer;
    procedure UpdateDisplay;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

uses Math, Graphics;

{ TFrameCircularTimer }

const
  CircularTimerDuration = 300;

constructor TFrameCircularTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSecondsLeft := CircularTimerDuration; {seconds}
  Timer1.Enabled := False;
  Timer1.Interval := 1000;
  Color := clWhite;
end;

procedure TFrameCircularTimer.ButtonStartClick(Sender: TObject);
begin
  if not Timer1.Enabled then
  begin
    // Start timer
    FSecondsLeft := CircularTimerDuration;
    Timer1.Enabled := True;
    btnStart.Caption := 'Reset';
  end
  else
  begin
    // Reset timer
    Timer1.Enabled := False;
    FSecondsLeft := CircularTimerDuration;
    btnStart.Caption := 'Start';
    PaintBox1.Invalidate;
  end;
end;

procedure TFrameCircularTimer.Timer1Timer(Sender: TObject);
begin
  if FSecondsLeft > 0 then
  begin
    Dec(FSecondsLeft);
    PaintBox1.Invalidate;
  end
  else
    FinishTimer;
end;

procedure TFrameCircularTimer.FinishTimer;
begin
  Timer1.Enabled := False;
  btnStart.Caption := 'Start';
  //ShowMessage('Your 5-minute reset is complete.');
end;

procedure TFrameCircularTimer.PaintBox1Paint(Sender: TObject);
begin
  DrawCircle;
end;

procedure TFrameCircularTimer.DrawCircle;
var
  C: TCanvas;
  cx, cy, r: Integer;
  angle: Double;
  sweep: Integer;
  LMin, LSec: Integer;
  timeStr: String;
begin
  C := PaintBox1.Canvas;

  cx := PaintBox1.Width div 2;
  cy := PaintBox1.Height div 2;
  r := Min(cx, cy) - 10;

  // Background
  C.Brush.Color := clWhite;
  C.FillRect(PaintBox1.ClientRect);

  // Outer circle
  C.Pen.Color := RGBToColor(120, 180, 160); // soft wellness green
  C.Pen.Width := 8;
  C.Brush.Style := bsClear;
  C.Ellipse(cx - r, cy - r, cx + r, cy + r);

  // Progress arc
  angle := (CircularTimerDuration - FSecondsLeft) / CircularTimerDuration;
  sweep := Round(360 * angle);

  C.Pen.Color := RGBToColor(40, 130, 110);

  // Draw arc from 90 degrees clockwise
  C.Arc(
    cx - r, cy - r, cx + r, cy + r,
    cx, cy - r,
    Round(cx + r * sin(sweep * Pi / 180)),
    Round(cy - r * cos(sweep * Pi / 180))
  );

  // Time text
  LMin := FSecondsLeft div 60;
  LSec := FSecondsLeft mod 60;
  timeStr := Format('%.2d:%.2d', [LMin, LSec]);

  C.Font.Size := 20;
  C.Font.Color := clBlack;
  C.Brush.Style := bsClear;
  C.TextOut(cx - C.TextWidth(timeStr) div 2,
            cy - C.TextHeight(timeStr) div 2,
            timeStr);
end;

procedure TFrameCircularTimer.UpdateDisplay;
begin
  PaintBox1.Invalidate;
end;

end.

