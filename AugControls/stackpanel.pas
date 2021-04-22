unit StackPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type
  TCustomStackPanel = class;
  TStackPanelControlList = class;

  TStackPanelControl = class(TCollectionItem)
  private
    FControl: TControl;
    procedure SetControl(const aControl: TControl);
  protected
    procedure SetIndex(Value: Integer); override;
    procedure AssignTo(Dest: TPersistent); override;
    function FPCollection: TStackPanelControlList;
    function FPOwner: TCustomStackPanel;
  published
    property Control: TControl read FControl write SetControl;
    property Index;
  end;

  TStackPanelControlList = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TStackPanelControl;
    procedure SetItem(Index: Integer; const AItem: TStackPanelControl);
  protected
    function FPOwner: TCustomStackPanel;

    function Add: TStackPanelControl;
    procedure AddControl(AControl: TControl; AIndex: Integer = -1);
    procedure RemoveControl(AControl: TControl);
  public
    constructor Create(AOwner: TPersistent);
  public
    function IndexOf(AControl: TControl): Integer;

    property Items[Index: Integer]: TStackPanelControl read GetItem write SetItem; default;
  end;

  TCustomStackPanel = class(TCustomPanel)
  private
    FControlList: TFlowPanelControlList;
    procedure SetControlList(const AControlList: TStackPanelControlList);
  protected
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;

    procedure AlignControls(AControl: TControl; var RemainingClientRect: TRect); override;
    procedure CalculatePreferredSize(
                         var PreferredWidth, PreferredHeight: integer;
                         WithThemeSpace: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function GetControlIndex(AControl: TControl): Integer;
    procedure SetControlIndex(AControl: TControl; Index: Integer);

    property ControlList: TStackPanelControlList read FControlList write SetControlList;
  end;

  TStackPanel = class(TCustomStackPanel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoWrap default True;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderSpacing;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property ControlList;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlowLayout;
    property FlowStyle;
    property FullRepaint;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I stackpanel_icon.lrs}
  RegisterComponents('Additional',[TStackPanel]);
end;

end.
