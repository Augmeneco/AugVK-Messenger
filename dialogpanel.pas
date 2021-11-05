unit dialogpanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls;

type

  { TDialogPanel }

  TDialogPanel = class(TPanel)
  public
    constructor Create(TheOwner: TComponent); override;
    procedure PlaceButtons(LeftSide: Array of Const; RightSide: Array of Const);
    procedure OpenDialog;
  end;

implementation

uses
  MainFormUnit;

{ TDialogPanel }

constructor TDialogPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  BevelOuter := bvNone;
  BorderSpacing.InnerBorder := 10;
end;

procedure TDialogPanel.PlaceButtons(LeftSide: array of const;
  RightSide: array of const);
begin

end;

procedure TDialogPanel.OpenDialog;
begin

end;

end.

