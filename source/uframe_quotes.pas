unit uframe_quotes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, Buttons, uQuotes;

type

  { TFrameQuotes }

  TFrameQuotes = class(TFrame)
    btnPrev: TBitBtn;
    btnNext: TBitBtn;
    Button1: TButton;
    Label1: TLabel;
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FQuotes: TQuoteList;
    procedure DisplayQuote(AText: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TFrameQuotes }

procedure TFrameQuotes.Button1Click(Sender: TObject);
var
  LRandomQuote: String;
begin
  LRandomQuote := FQuotes.NextQuote(lpRandom);
  DisplayQuote(LRandomQuote);
  btnPrev.Enabled:= True;
  btnNext.Enabled:= True;
end;

procedure TFrameQuotes.btnPrevClick(Sender: TObject);
var
  LRandomQuote: String;
begin
  LRandomQuote := FQuotes.NextQuote(lpPrev);
  DisplayQuote(LRandomQuote);
end;

procedure TFrameQuotes.btnNextClick(Sender: TObject);
var
  LRandomQuote: String;
begin
  LRandomQuote := FQuotes.NextQuote(lpNext);
  DisplayQuote(LRandomQuote);
end;

procedure TFrameQuotes.DisplayQuote(AText: string);
var
  LQuote: TQuote;
begin
  LQuote := TQuote.Create(AText);
  Label1.Caption := LQuote.Text + #13#10 + '--' + LQuote.Author;
  LQuote.Free;
end;

constructor TFrameQuotes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FQuotes := TQuoteList.Create;
  btnPrev.Enabled:= False;
  btnNext.Enabled:= False;
  Label1.Caption:= '';

  {
  // just playing around with colors...

  self.Color := RGBToColor(232, 245, 240);
  Label1.Font.Color := RGBToColor(46, 46, 46);

  btnNext.Color := RGBToColor(90, 111, 174);
  btnNext.Font.Color := clWhite;

  btnPrev.Color := RGBToColor(90, 111, 174);
  btnPrev.Font.Color := clWhite;

  Button1.Color := RGBToColor(90, 111, 174);
  }
end;

destructor TFrameQuotes.Destroy;
begin
  FQuotes.Free;
  inherited Destroy;
end;

end.

