unit NavigationDrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TNavigationDrawer }

  TNavigationDrawer = class(TComponent)
  private
    MainPanel: TPanel;
    FParent: TComponent;
    procedure SetParent(const AParent: TComponent);
    function GetParent: TComponent;
  protected

  public
    procedure Open;
    procedure Close;
    constructor Create(AOwner: TComponent); override;

  published
    property Parent: TComponent read FParent write SetParent;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I navigationdrawer_icon.lrs}
  RegisterComponents('AugControls',[TNavigationDrawer]);
end;

{ TNavigationDrawer }

procedure TNavigationDrawer.SetParent(const AParent: TComponent);
begin

end;

function TNavigationDrawer.GetParent: TComponent;
begin

end;

procedure TNavigationDrawer.Open;
begin

end;

procedure TNavigationDrawer.Close;
begin

end;

constructor TNavigationDrawer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  MainPanel := TPanel.Create(AOwner);
end;

end.
