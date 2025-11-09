program mbreathing;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ubreathing, umathutils, uAppSettings
  { you can add units after this };

{$R *.res}

{
// Ref: https://nicmulvaney.com/easing
function EaseInOutSine(t: Double): Double;
begin
  Result := 0.5 * (1 - Cos(Pi * EnsureRange(t, 0.0, 1.0)));
end;
}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

