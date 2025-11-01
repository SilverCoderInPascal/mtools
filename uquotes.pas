unit uQuotes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TQuote }

  TQuote = class
  private
    FAuthor: string;
    FText: string;
  public
    constructor Create(AString: string);
    property Author: string read FAuthor;
    property Text: string read FText;
  end;

  TListPosition = (lpRandom, lpPrev, lpNext);
  { TQuoteList }

  TQuoteList = class
    FList: TStringList;
    FIndex: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function NextQuote(APosition: TListPosition): string;
  end;

implementation

{ TQuote }

constructor TQuote.Create(AString: string);
var
  LParts: TAnsiStringArray;
begin
  LParts := AString.Split(',', '"', '"');
  FAuthor:= AnsiDequotedStr(LParts[0], '"');
  FText:= AnsiDequotedStr(LParts[1], '"');
  if FAuthor = '' then
    FAuthor := 'Anon'
end;

{ TQuoteList }

constructor TQuoteList.Create;
begin
  FIndex := -1;
  FList := TStringList.Create;
  FList.LoadFromFile('quotes.csv');
end;

destructor TQuoteList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

{
"Maya Angelou","Nothing will work unless you do."
"Byron Pulsifer","Into each life rain must fall but rain can be the giver of life and it is all in your attitude that makes rain produce sunshine."
"Harold Nicolson","We are all inclined to judge ourselves by our ideals; others, by their acts."

}
function TQuoteList.NextQuote(APosition: TListPosition): string;
begin
  if FIndex < 0 then
    APosition := lpRandom;

  case APosition of
   lpRandom:
     begin
       FIndex := Random(FList.Count)
     end;
   lpPrev:
     begin
       Dec(FIndex);
       if FIndex < 0 then
         FIndex := FList.Count - 1;
     end;
   lpNext:
     begin
       Inc(FIndex);
       if FIndex >= FList.Count then
         FIndex := 0;
     end;
  end;

  Result := FList[FIndex];
end;

end.

