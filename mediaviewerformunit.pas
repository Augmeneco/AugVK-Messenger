unit MediaViewerFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, AugImage,
  BCSVGButton, BGRASpriteAnimation;

type

  { TMediaViewerForm }

  TMediaViewerForm = class(TForm)
    AugImage1: TAugImage;
    BCSVGButton1: TBCSVGButton;
    procedure BCSVGButton1Click(Sender: TObject);
    procedure BCSVGButton1MouseEnter(Sender: TObject);
    procedure BCSVGButton1MouseLeave(Sender: TObject);
  private

  public

  end;

var
  MediaViewerForm: TMediaViewerForm;

implementation

{$R *.lfm}

{ TMediaViewerForm }

procedure TMediaViewerForm.BCSVGButton1Click(Sender: TObject);
begin
  Self.Close;
end;

procedure TMediaViewerForm.BCSVGButton1MouseEnter(Sender: TObject);
begin
  BCSVGButton1.ColorOpacity := 100;
end;

procedure TMediaViewerForm.BCSVGButton1MouseLeave(Sender: TObject);
begin
  BCSVGButton1.ColorOpacity := 50;
end;

end.

