unit uframe_breathe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Graphics, DBCtrls, uAppSettings;

type

  TBarOrientation = (boHorizontal, boVertical);
  TBreathPhase = (bpInhale, bpHold1, bpExhale, bpHold2);

  { TFrameBreathingExercise }

  TFrameBreathingExercise = class(TFrame)
    Image1: TImage;
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
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
     constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

uses Math, umathutils, BGRABitmap, BGRABitmapTypes;


function LightenColor(AColor: TColor; Amount: Byte): TColor;
var
  r,g,b: Byte;
begin
  RedGreenBlue(AColor, r, g, b);
  r := r + ((255 - r) * Amount) div 255;
  g := g + ((255 - g) * Amount) div 255;
  b := b + ((255 - b) * Amount) div 255;
  Result := RGBToColor(r,g,b);
end;


procedure TFrameBreathingExercise.SetCanvasFont(c: TCanvas);
begin
  c.Font.Name := Settings.FontName;
  c.Font.Size := Settings.FontSize;
  c.Font.Color := Settings.FontColor;
  c.Font.Style := [fsBold];
  c.Brush.Style := bsClear;
end;

procedure TFrameBreathingExercise.DrawBoxBorder(ACanvas: TCanvas; ARect: TRect);
begin
  ACanvas.Pen.Color := Settings.BorderColor;
  ACanvas.Pen.Width := Settings.BorderThickness;
  ACanvas.Brush.Style := bsClear;
  ACanvas.RoundRect(ARect.Left + 1, ARect.Top + 1,
                    ARect.Right - 1, ARect.Bottom - 1, 28, 28);
end;

procedure TFrameBreathingExercise.DrawHorizontalBar(c: TCanvas; const ARect: TRect;
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


procedure TFrameBreathingExercise.DrawVerticalBar(c: TCanvas; const ARect: TRect;
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

procedure TFrameBreathingExercise.Timer1Timer(Sender: TObject);
var
  dt: Double;
begin
  dt := fpsInterval / 1000.0;
  FPhaseTime += dt;

  if FPhaseTime > FPhaseDur then
    NextPhase(FPhaseTime - FPhaseDur);

  PaintBox1.Invalidate;
end;

constructor TFrameBreathingExercise.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Settings := TAppSettings.Instance;

  // setup default - durations in seconds
  FInhaleDur := 6.0;
  FHold1Dur  := 4.0;
  FExhaleDur := 5.0;
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

procedure TFrameBreathingExercise.UpdatePhaseDur;
begin
  case FPhase of
    bpInhale: FPhaseDur := FInhaleDur;
    bpHold1:  FPhaseDur := FHold1Dur;
    bpExhale: FPhaseDur := FExhaleDur;
    bpHold2:  FPhaseDur := FHold2Dur;
  end;
end;

procedure TFrameBreathingExercise.NextPhase(overflow: Double);
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

procedure TFrameBreathingExercise.GetPhaseState(out BarFrac: Double; out StartColor, EndColor: TColor; out LabelText: String);
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

{
procedure TFrameBreathingExercise.PaintBox1Paint(Sender: TObject);
var
  c: TCanvas;
  t, barFrac: Double;
  phaseLabel: String;
  textY, textX: Integer;
  R: TRect;
  GradientStart, GradientEnd: TColor;
begin
  c := PaintBox1.Canvas;

  // Draw background (form image or solid)
  c.Brush.Style := bsSolid;
  c.Brush.Color := LightenColor(Color, 128); // make background lighter
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

}

procedure TFrameBreathingExercise.PaintBox1Paint(Sender: TObject);
var
  bmp: TBGRABitmap;
  barFrac: Double;
  phaseLabel: String;
  textX, textY: Integer;
  GradientStart, GradientEnd: TColor;
  barRect: TRect;
begin
  bmp := TBGRABitmap.Create(PaintBox1.Width, PaintBox1.Height);
  try
    SetCanvasFont(PaintBox1.Canvas);

    bmp.FillRoundRect(0, 0, bmp.Width, bmp.Height, 20, 20,
                 BGRA(0, 0, 0, 70)); //dmDrawWithTransparency);

    GetPhaseState(barFrac, GradientStart, GradientEnd, phaseLabel);

    case FBarOrientation of
      boHorizontal:
        begin
          barRect := Rect(0, bmp.Height div 2 - Settings.BarHeight div 2,
                          Round(bmp.Width * barFrac),
                          bmp.Height div 2 + Settings.BarHeight div 2);
          bmp.GradientFill(barRect.Left, barRect.Top,
                           barRect.Right, barRect.Bottom,
                           GradientStart, GradientEnd,
                           gtLinear,
                           PointF(0,0),
                           PointF(barRect.Right, barRect.Bottom),
                           dmSet);
          textX := (bmp.Width - bmp.Canvas.TextWidth(phaseLabel)) div 2;
          textY := barRect.Top - Settings.TextMargin -
            bmp.Canvas.TextHeight(phaseLabel);
        end;
      boVertical:
        begin
          barRect := Rect(bmp.Width div 2 - Settings.BarWidth div 2,
                          bmp.Height - Round(bmp.Height * barFrac),
                          bmp.Width div 2 + Settings.BarWidth div 2,
                          bmp.Height);
          bmp.GradientFill(barRect.Left, barRect.Top,
                           barRect.Right, barRect.Bottom,
                           GradientStart,GradientEnd,
                           gtLinear,
                           PointF(0,0),
                           PointF(barRect.Right, barRect.Bottom),
                           dmSet);
          textX := (bmp.Width - bmp.Canvas.TextWidth(phaseLabel)) div 2;
          textY := 2;
        end;
    end;

    bmp.FontHeight := Settings.FontSize;
    bmp.FontStyle := [fsBold];
    bmp.TextOut(textX, textY, phaseLabel, BGRA(245,245,245,255));
    bmp.Draw(PaintBox1.Canvas, 0, 0, False);
  finally
    bmp.Free;
  end;
end;


end.

