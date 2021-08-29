unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  StackPanel;

type

  { TForm1 }

  TForm1 = class(TForm)
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
end;

procedure TForm1.FormShow(Sender: TObject);
var
  s: TStackPanel;
  p: TPanel;
  b: TButton;
  i: Integer;
begin
  s := TStackPanel.Create(self);
  s.Parent := ScrollBox1;
  s.HorizontalPositioning:=sphpFill;
  s.Align:=alTop;
  s.AutoSize:=True;

  for i:=0 to 10 do
  begin
    p := TPanel.Create(self);
    p.Name:='p'+inttostr(i);
    p.Parent := s;
    p.Caption := IntToStr(i);
    b := TButton.Create(self);
    b.Name:='b'+inttostr(i);
    b.Caption:=inttostr(i);
    b.Parent := p;
    //p.Anchors:=[akleft, akRight];
    //p.AnchorSideLeft.Side:=asrBottom;
    //p.AnchorSideLeft.Control:=s;
    //p.AnchorSideRight.Side:=asrTop;
    //p.AnchorSideRight.Control:=s;
    //p.AutoSize:=True;
  end;
end;

end.

