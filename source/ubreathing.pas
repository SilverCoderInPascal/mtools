unit ubreathing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, uAppSettings;

type
  TBarOrientation = (boHorizontal, boVertical);
  TBreathPhase = (bpInhale, bpHold1, bpExhale, bpHold2);

  { TForm1 }

  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FInhaleDur,
    FHold1Dur,
    FExhaleDur,
    FHold2Dur: Double;
    FBarOrientation: TBarOrientation;
    FPhase: TBreathPhase;
    FPhaseTime,
    FPhaseDur: Double;
    fpsInterval: Double;

    Settings: TAppSettings;

    procedure DrawBoxBorder(ACanvas: TCanvas; ARect: TRect);
    procedure DrawHorizontalBar(c: TCanvas; const ARect: TRect;
      const AFrac: Double; StartColor, EndColor: TColor);
    procedure DrawVerticalBar(c: TCanvas; const ARect: TRect;
      const AFrac: Double; StartColor, EndColor: TColor);
    procedure GetPhaseState(out BarFrac: Double; out StartColor,
      EndColor: TColor; out LabelText: String);
    procedure NextPhase(overflow: Double);
    procedure SetCanvasFont(c: TCanvas);
    procedure UpdatePhaseDur;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses Math, umathutils;


procedure TForm1.SetCanvasFont(c: TCanvas);
begin
  c.Font.Name := Settings.FontName;
  c.Font.Size := Settings.FontSize;
  c.Font.Color := Settings.FontColor;
  c.Font.Style := [fsBold];
  c.Brush.Style := bsClear;
end;

procedure TForm1.DrawBoxBorder(ACanvas: TCanvas; ARect: TRect);
begin
  ACanvas.Pen.Color := Settings.BorderColor;
  ACanvas.Pen.Width := Settings.BorderThickness;
  ACanvas.Brush.Style := bsClear;
  ACanvas.RoundRect(ARect.Left + 1, ARect.Top + 1,
                    ARect.Right - 1, ARect.Bottom - 1, 8, 8);
end;

procedure TForm1.DrawHorizontalBar(c: TCanvas; const ARect: TRect;
  const AFrac: Double; StartColor, EndColor: TColor);
var
  barWidth, barHeight, centerY: Integer;
  R: TRect;
begin
  barHeight := Settings.BarHeight;
  barWidth := Round(ARect.Width * AFrac);
  centerY := ARect.Top + (ARect.Height div 2);

  if barWidth > 0 then
  begin
    R := Rect(
      ARect.Left,
      centerY - (barHeight div 2),
      ARect.Left + barWidth,
      centerY + (barHeight div 2)
    );
    c.GradientFill(R, StartColor, EndColor, gdHorizontal);
  end;
end;


procedure TForm1.DrawVerticalBar(c: TCanvas; const ARect: TRect;
  const AFrac: Double; StartColor, EndColor: TColor);
var
  barWidth, barHeight, centerX: Integer;
  R: TRect;
begin
  barWidth := Settings.BarWidth;
  barHeight := Round(ARect.Height * AFrac);
  centerX := ARect.Left + (ARect.Width div 2);

  if barHeight > 0 then
  begin
    R := Rect(
      centerX - (barWidth div 2),
      ARect.Bottom - barHeight,
      centerX + (barWidth div 2),
      ARect.Bottom
    );
    c.GradientFill(R, StartColor, EndColor, gdVertical);
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Settings := TAppSettings.Instance;

  // setup default - durations in seconds
  FInhaleDur := 4.0;
  FHold1Dur  := 4.0;
  FExhaleDur := 4.0;
  FHold2Dur  := 4.0;

  FBarOrientation := boHorizontal; //boVertical;

  // "state machine" initialisation
  FPhase := bpInhale;
  FPhaseTime := 0;
  UpdatePhaseDur;

  // this will run about 60 frames per second
  fpsInterval := 16;
  Timer1.Interval := Round(fpsInterval);
  Timer1.Enabled := True;

  DoubleBuffered := True;
  Color := RGBToColor(20, 28, 38); // deep navy background
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  dt: Double;
begin
  dt := fpsInterval / 1000.0;
  FPhaseTime += dt;

  if FPhaseTime > FPhaseDur then
    NextPhase(FPhaseTime - FPhaseDur);

  PaintBox1.Invalidate;
end;

procedure TForm1.UpdatePhaseDur;
begin
  case FPhase of
    bpInhale: FPhaseDur := FInhaleDur;
    bpHold1:  FPhaseDur := FHold1Dur;
    bpExhale: FPhaseDur := FExhaleDur;
    bpHold2:  FPhaseDur := FHold2Dur;
  end;
end;

procedure TForm1.NextPhase(overflow: Double);
begin
  // switch states
  case FPhase of
    bpInhale: FPhase := bpHold1;
    bpHold1:  FPhase := bpExhale;
    bpExhale: FPhase := bpHold2;
    bpHold2:  FPhase := bpInhale;
  end;
  UpdatePhaseDur;

  //reset the phase time
  FPhaseTime := Min(overflow, FPhaseDur);
end;

procedure TForm1.GetPhaseState(out BarFrac: Double; out StartColor, EndColor: TColor; out LabelText: String);
var
  t: Double;
begin
case FPhase of
  bpInhale:
    begin
      t := EaseInOutSine(FPhaseTime / FPhaseDur);
      BarFrac := t;
      StartColor := Settings.InhaleStart;
      EndColor   := Settings.InhaleEnd;
      LabelText := 'Inhale';
    end;
  bpHold1:
    begin
      BarFrac := 1.0;
      StartColor := Settings.HoldColor;
      EndColor   := Settings.HoldColor;
      LabelText := 'Hold';
    end;
  bpExhale:
    begin
      t := EaseInOutSine(FPhaseTime / FPhaseDur);
      BarFrac := 1.0 - t;
      StartColor := Settings.ExhaleStart;
      EndColor   := Settings.ExhaleEnd;
      LabelText := 'Exhale';
    end;
  bpHold2:
    begin
      BarFrac := 0.0;
      StartColor := Settings.HoldColor;
      EndColor   := Settings.HoldColor;
      LabelText := 'Hold';
    end;
  end;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  c: TCanvas;
  t, barFrac: Double;
  phaseLabel: String;
  textY, textX: Integer;
  R: TRect;
  GradientStart, GradientEnd: TColor;
begin
  c := PaintBox1.Canvas;

  // --- background stuff ---
  c.Brush.Style := bsSolid;
  c.Brush.Color := Color; // form color (e.g., deep navy)
  c.FillRect(PaintBox1.ClientRect);
  DrawBoxBorder(c, PaintBox1.ClientRect);
  SetCanvasFont(c);

  // --- determine breathing phase ---
  GetPhaseState(barFrac, GradientStart, GradientEnd, phaseLabel);

  // --- draw gradient bar depending on orientation ---
  case FBarOrientation of

    boHorizontal:
      begin
        DrawHorizontalBar(c, PaintBox1.ClientRect,
          barFrac, GradientStart, GradientEnd);
        // text above horizontal bar
        textX := (PaintBox1.Width - c.TextWidth(phaseLabel)) div 2;
        textY := 1 //Settings.TextMargin;
          //(PaintBox1.Height div 2) - (Settings.BarHeight div 2) -
          //c.TextHeight(phaseLabel) - Settings.TextMargin;
      end;

    boVertical:
      begin
        DrawVerticalBar(c, PaintBox1.ClientRect,
          barFrac, GradientStart, GradientEnd);
        // text above the top of vertical bar
        textX := (PaintBox1.Width - c.TextWidth(phaseLabel)) div 2;
        textY := Settings.TextMargin;
      end;
  end;

  // --- draw text ---
  c.Brush.Style := bsClear;
  c.TextOut(textX, textY, phaseLabel);
end;



end.

