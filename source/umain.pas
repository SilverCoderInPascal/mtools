unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uframe_quotes, uframe_about, uframe_breathe, uframe_resettimer;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAbout: TButton;
    btnBreathExercise: TButton;
    btnQuotes: TButton;
    btnResetTimer: TButton;
    PanelMenu: TPanel;
    PanelContent: TPanel;
    procedure btnAboutClick(Sender: TObject);
    procedure btnResetTimerClick(Sender: TObject);
    procedure btnBreathExerciseClick(Sender: TObject);
    procedure btnQuotesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCurrentFrame: TFrame;
    procedure LoadFrame(AFrame: TFrame);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

uses Math;

procedure TForm1.LoadFrame(AFrame: TFrame);
begin
  // Remove old frame if any
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Free;

  FCurrentFrame := AFrame;
  FCurrentFrame.Parent := PanelContent;
  FCurrentFrame.Align := alClient;
  FCurrentFrame.Show;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  FCurrentFrame := nil;
  LoadFrame(TFrameAbout.Create(Self));
end;

procedure TForm1.btnQuotesClick(Sender: TObject);
begin
  LoadFrame(TFrameQuotes.Create(Self));
end;

procedure TForm1.btnResetTimerClick(Sender: TObject);
begin
  LoadFrame(TFrameCircularTimer.Create(Self));
end;

procedure TForm1.btnAboutClick(Sender: TObject);
begin
  LoadFrame(TFrameAbout.Create(Self));
end;

procedure TForm1.btnBreathExerciseClick(Sender: TObject);
begin
  LoadFrame(TFrameBreathingExercise.Create(Self));
end;


end.

