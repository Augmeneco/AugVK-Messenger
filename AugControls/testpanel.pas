unit TestPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TTestPanel = class(TPanel)
  private
    { The new attribute for the embedded label }
    FEmbeddedLabel: TLabel;
  public
    { The constructor must be overriden so the label can be created }
    constructor Create(AOwner: TComponent); override;
  private

  public

  published
    { Make the label visible in the IDE }
    property EmbeddedLabel: TLabel read FEmbeddedLabel;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Additional',[TTestPanel]);
end;

constructor TTestPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Set default width and height
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  // Add the embedded label
  FEmbeddedLabel := TLabel.Create(Self); // Add the embedded label
  FEmbeddedLabel.Parent := self;         // Show the label in the panel
  FEmbeddedLabel.SetSubComponent(true);  // Tell the IDE to store the modified properties
  FEmbeddedLabel.Name := 'EmbeddedLabel';
  FEmbeddedLabel.Caption := 'Howdy World!';

  // Make sure the embedded label can not be selected/deleted within the IDE
  FEmbeddedLabel.ControlStyle := FEmbeddedLabel.ControlStyle - [csNoDesignSelectable];

  // Set other properties if necessary
  //...

end;

end.
