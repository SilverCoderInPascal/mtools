unit uframe_about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, ExtCtrls;

type

  { TFrameAbout }

  TFrameAbout = class(TFrame)
    Image1: TImage;
    Label1: TLabel;
    procedure Image1Click(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private

  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TFrameAbout }

procedure TFrameAbout.Label1Click(Sender: TObject);
begin

end;

procedure TFrameAbout.Image1Click(Sender: TObject);
begin

end;

constructor TFrameAbout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.Color := image1.Canvas.Pixels[1,1];
end;

end.

