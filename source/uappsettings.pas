unit uAppSettings;

{$mode objfpc}{$H+}

interface

uses
  Graphics, SysUtils;

type
  { TAppSettings - Singleton }
  TAppSettings = class
  private
    class var FInstance: TAppSettings;
    constructor CreatePrivate;
  public
    // Phase colors
    InhaleStart: TColor;
    InhaleEnd: TColor;
    HoldColor: TColor;
    ExhaleStart: TColor;
    ExhaleEnd: TColor;

    // PaintBox appearance
    BorderColor: TColor;
    BorderThickness: Integer;

    // Bar appearance
    BarHeight: Integer;
    BarWidth: Integer;

    // Font appearance
    FontName: string;
    FontSize: Integer;
    FontColor: TColor;

    // Layout
    TextMargin: Integer;


    class function Instance: TAppSettings;
    procedure LoadDefaults;
  end;

implementation

{ TAppSettings }

constructor TAppSettings.CreatePrivate;
begin
  LoadDefaults;
end;

class function TAppSettings.Instance: TAppSettings;
begin
  if FInstance = nil then
    FInstance := TAppSettings.CreatePrivate;
  Result := FInstance;
end;

procedure TAppSettings.LoadDefaults;
begin
  // --- Phase Colors ---
  InhaleStart := TColor($00FFAA5A); // RGB(90,170,255)
  InhaleEnd   := TColor($00C8DC78); // RGB(120,220,200)
  HoldColor   := TColor($00B8D4A0); // RGB(160,212,184)
  ExhaleStart := TColor($00C8DC78);
  ExhaleEnd   := TColor($00FFAA5A);

  // --- Appearance ---
  BorderColor := TColor($8C7884);
  BorderThickness := 1;
  BarHeight := 40;
  BarWidth := 40;

  // --- Font ---
  FontName := 'Segoe UI';
  FontSize := 16;
  FontColor := TColor($FAF0E6);

  // --- Layout ---
  TextMargin := 10;
end;

end.

