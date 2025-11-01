unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uQuotes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    btnPrev: TButton;
    btnNext: TButton;
    Label1: TLabel;
    procedure btnPrevClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FQuotes: TQuoteList;
    procedure DisplayQuote(AText: string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

uses Math;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  FQuotes := TQuoteList.Create;
  btnPrev.Enabled:= False;
  btnNext.Enabled:= False;
  Label1.Caption:= '';
end;

procedure TForm1.DisplayQuote(AText: string);
var
  LQuote: TQuote;
begin
  LQuote := TQuote.Create(AText);
  Label1.Caption := LQuote.Text + #13#10 + '--' + LQuote.Author;
  LQuote.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  LRandomQuote: String;
begin
  LRandomQuote := FQuotes.NextQuote(lpRandom);
  DisplayQuote(LRandomQuote);
  btnPrev.Enabled:= True;
  btnNext.Enabled:= True;
end;

procedure TForm1.btnPrevClick(Sender: TObject);
var
  LRandomQuote: String;
begin
  LRandomQuote := FQuotes.NextQuote(lpPrev);
  DisplayQuote(LRandomQuote);
end;

procedure TForm1.btnNextClick(Sender: TObject);
var
  LRandomQuote: String;
begin
  LRandomQuote := FQuotes.NextQuote(lpNext);
  DisplayQuote(LRandomQuote);
end;

end.

