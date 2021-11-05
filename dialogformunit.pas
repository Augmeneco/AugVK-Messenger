unit DialogFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BCMaterialDesignButton, Shadow, SlaveForms, Messages;

type

  TButtonSide = (bsLeft, bsRight);

  { TDialogForm }

  TDialogForm = class(TSlaveForm)
    ContentPanel: TPanel;
    ButtonPanel: TPanel;
  private
  protected
  public
    procedure AddButton(ACaption: String; ATag: PtrUInt; ACallback: TNotifyEvent; ASide: TButtonSide=bsRight);
    procedure SetFrame(AFrame: TFrame);
    procedure UpdateDialog;
  published
  end;

var
  DialogForm: TDialogForm;

implementation

{$R *.lfm}

{ TDialogForm }

procedure TDialogForm.AddButton(ACaption: String; ATag: PtrUInt;
  ACallback: TNotifyEvent; ASide: TButtonSide);
var
  Button: TBCMaterialDesignButton;
begin
  Button := TBCMaterialDesignButton.Create(Self);
  with Button do
  begin
    AutoSize := True;
    Align := alRight;
    //Width := 200;
    NormalColor := 16090919;
    NormalColorEffect := 14383395;
    Shadow := False;
    TextColor := clWhite;
    TextShadow := False;
    TextFont := 'default';
    //TextQuality := fqSystemClearType;
    Anchors := [akTop, akLeft, akRight];
    Caption := ACaption;
  end
end;

procedure TDialogForm.SetFrame(AFrame: TFrame);
begin
  AFrame.Parent := ContentPanel;
  AFrame.Align := alClient;
end;

procedure TDialogForm.UpdateDialog;
begin
  Top := (PopupParent.Top + (PopupParent.Height div 2)) - (Height div 2);
  Left := (PopupParent.Left + (PopupParent.Width div 2)) - (Width div 2);
end;

end.

