unit StackPanel;

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics, Math,
  DelphiCompatibility;

type
 /// <summary>
  ///   Specifies orientation of TStackPanel.
  ///   spoVertical - stack will be oriented vertically, controls will be placed one under another
  ///   spoHorizontal - stack will be oriented horizontally, controls will be placed side to side
  /// </summary>
  TStackPanelOrientation = (spoVertical, spoHorizontal);

  /// <summary>
  ///   Enumeration type that specifies a control's horizontal positioning inside a vertically oriented TStackPanel.
  /// </summary>
  TStackPanelControlHorizontalPositioning = (
    /// <summary>
    ///   Control is positioned according to stack panel's HorizontalPositioning property.
    /// </summary>
    sphpDefault,
    /// <summary>
    ///   Control is positioned so that its left edge is aligned with the left edge of the stack panel.
    /// </summary>
    sphpLeft,
    /// <summary>
    ///   Control is positioned so that it is aligned horizontally with the center of the stack panel.
    /// </summary>
    sphpCenter,
    /// <summary>
    ///   Control is positioned so that its right edge is aligned with the right edge of the stack panel.
    /// </summary>
    sphpRight,
    /// <summary>
    ///   Control is resized so that its width matches the width of the stack panel.
    /// </summary>
    sphpFill);
  /// <summary>
  ///   Enumeration type that specifies the default horizontal positioning of a vertically orientated TStackPanel.
  /// </summary>
  TStackPanelHorizontalPositioning = TStackPanelControlHorizontalPositioning.sphpLeft..TStackPanelControlHorizontalPositioning.sphpFill;

  /// <summary>
  ///   Enumeration type that specifies a control's vertical positioning inside a horizontally oriented TStackPanel.
  /// </summary>
  TStackPanelControlVerticalPositioning = (
    /// <summary>
    ///   Control is positioned according to the stack panel's VerticalPositioning property.
    /// </summary>
    spvpDefault,
    /// <summary>
    ///   Control is positioned so that its top edge is aligned with the top edge of the stack panel.
    /// </summary>
    spvpTop,
    /// <summary>
    ///   Control is positioned so that it is aligned vertically with the center of the stack panel.
    /// </summary>
    spvpCenter,
    /// <summary>
    ///   Control is positioned so that its bottom edge is aligned with bottom edge of stack panel.
    /// </summary>
    spvpBottom,
    /// <summary>
    ///   Control is resized so that its height matches the height of the stack panel.
    /// </summary>
    spvpFill);
  /// <summary>
  ///   Enumeration type that specifies the default vertical positioning of a horizontally oriented TStackPanel.
  /// </summary>
  TStackPanelVerticalPositioning = TStackPanelControlVerticalPositioning.spvpTop..TStackPanelControlVerticalPositioning.spvpFill;

  /// <summary>
  ///   TStackPanelControlItem determines how the associated TControl instance is positioned within a TStackPanel.
  /// </summary>
  TStackPanelControlItem = class(TCollectionItem)
  strict private
    FControl: TControl;
    FHorizontalPositioning: TStackPanelControlHorizontalPositioning;
    FVerticalPositioning: TStackPanelControlVerticalPositioning;
  private
    function GetBounds: TRect;
    procedure SetBounds(const Value: TRect);
    procedure SetHorizontalPositioning(Value: TStackPanelControlHorizontalPositioning);
    procedure SetVerticalPositioning(Value: TStackPanelControlVerticalPositioning);
  public
    /// <summary>
    ///   Automatically sets the HorizontalPositioning and VerticalPositioning properties based on the associated
    ///   control's Align property.
    /// </summary>
    procedure MapAlign;
    constructor Create(ControlCollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary> Link to Control which is placed on the panel</summary>
    property Control: TControl read FControl write FControl;
    /// <summary> Control's bounds rectangle</summary>
    //property Bounds: TRect read GetBounds write SetBounds;
    /// <summary>
    ///   Use this property to override the default horizontal positioning of the control
    /// </summary>
    property HorizontalPositioning: TStackPanelControlHorizontalPositioning read FHorizontalPositioning
      write SetHorizontalPositioning default sphpDefault;
    /// <summary>
    ///   Use this property to override the default vertical positioning of the control
    /// </summary>
    property VerticalPositioning: TStackPanelControlVerticalPositioning read FVerticalPositioning write SetVerticalPositioning
      default spvpDefault;
  end;

  /// <summary>
  ///   Internal class used to implement the TStackPanel.ControlCollection property
  /// </summary>
  TStackPanelControlCollection = class(TOwnedCollection)
  strict private
    function GetItem(AIndex: Integer): TStackPanelControlItem; inline;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>
    ///   Use this method to add a new item to the collection.
    /// </summary>
    function Add: TStackPanelControlItem; inline;
    /// <summary>
    ///   Use this method to get the index of the TStackPanelControlItem instance that is associated with the specified
    ///   TControl.
    /// </summary>
    function IndexOf(AControl: TControl): Integer;
    /// <summary>
    ///   Array property that provides access to the individual TStackPanelControlItem instances in the collection.
    /// </summary>
    property Items[AIndex: Integer]: TStackPanelControlItem read GetItem; default;
  end;

  /// <summary>
  ///   TCustomStackPanel is the base class for the TStackPanel.
  /// </summary>
  TCustomStackPanel = class(TCustomPanel)
  private
    FControlCollection: TStackPanelControlCollection;
    FHorizontalPositioning: TStackPanelHorizontalPositioning;
    FOrientation: TStackPanelOrientation;
    FSpacing: Integer;
    FVerticalPositioning: TStackPanelVerticalPositioning;
    function GetControlHorizontalPositioning(AControl: TControl): TStackPanelControlHorizontalPositioning;
    function GetControlIndex(AControl: TControl): Integer;
    function GetControlVerticalPositioning(AControl: TControl): TStackPanelControlVerticalPositioning;
    procedure MoveControlToItsCoordinates(AMovedControl: TControl);
    procedure SetControlCollection(const Value: TStackPanelControlCollection);
    procedure SetControlHorizontalPositioning(AControl: TControl; Positioning: TStackPanelControlHorizontalPositioning);
    procedure SetControlIndex(AControl: TControl; Index: Integer);
    procedure SetHorizontalPositioning(const Value: TStackPanelHorizontalPositioning);
    procedure SetOrientation(const Value: TStackPanelOrientation);
    procedure SetVerticalPositioning(const Value: TStackPanelVerticalPositioning);
    procedure SetControlVerticalPositioning(AControl: TControl; Positioning: TStackPanelControlVerticalPositioning);
    procedure SetSpacing(const Value: Integer);
  protected
    const
      DefaultHeight = 200;
      DefaultWidth = 185;
      DefaultHorizontalPositioning = sphpLeft;
      DefaultOrientation = spoVertical;
      DefaultSpacing = 2;
      DefaultVerticalPositioning = spvpCenter;

    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure CMControlChange(var Msg: TCMControlChange); message CM_CONTROLCHANGE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    /// <summary>
    ///   Use this property to access the individual control items that hold the horizontal and vertical positioning
    ///   values for each control dropped onto a stack panel.
    /// </summary>
    /// <remarks>
    ///   This property is published in the TStackPanel class so that the custom selection editor can be registered
    ///   with the development environment. However, the selection editor removes the ControlCollection property from
    ///   the list of properties that appear in the Object Inspector.
    /// </remarks>
    property ControlCollection: TStackPanelControlCollection read FControlCollection write SetControlCollection;
    /// <summary>
    ///   Use this property to change the HorizontalPositioning property of an individual control
    /// </summary>
    property ControlHorizontalPositioning[AControl: TControl]: TStackPanelControlHorizontalPositioning
      read GetControlHorizontalPositioning write SetControlHorizontalPositioning;
    /// <summary>
    ///   Use this property to get the index in the ControlCollection of the TStackPanelControlItem associated with the
    ///   specified control instance.
    /// </summary>
    property ControlIndex[AControl: TControl]: Integer read GetControlIndex write SetControlIndex;
    /// <summary>
    ///   Use this property to change the VerticalPositioning property of an individual control
    /// </summary>
    property ControlVerticalPositioning[AControl: TControl]: TStackPanelControlVerticalPositioning
      read GetControlVerticalPositioning write SetControlVerticalPositioning;
    /// <summary>
    ///   Use this property to set the default horizontal positioning of controls contained within the stack panel.
    /// </summary>
    property HorizontalPositioning: TStackPanelHorizontalPositioning read FHorizontalPositioning
      write SetHorizontalPositioning;
    /// <summary>
    ///   Use this property to set the orientation of the stack panel.
    /// </summary>
    property Orientation: TStackPanelOrientation read FOrientation write SetOrientation;
    /// <summary>
    ///   Use this property to control the spacing between controls.
    /// </summary>
    property Spacing: Integer read FSpacing write SetSpacing;
    /// <summary>
    ///   Use this property to set the default vertical positioning of controls contained within the stack panel.
    /// </summary>
    property VerticalPositioning: TStackPanelVerticalPositioning read FVerticalPositioning write SetVerticalPositioning;
  end;

  TStackPanel = class(TCustomStackPanel)
  published
    property Align;
    property Anchors;
    property AutoSize;
    //property BevelEdges;
    property BevelInner;
    //property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property ControlCollection;
    //property Ctl3D;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property Height default TCustomStackPanel.DefaultHeight;
    property HorizontalPositioning default TCustomStackPanel.DefaultHorizontalPositioning;
    //property Locked;
    property Orientation default TCustomStackPanel.DefaultOrientation;
    //property Padding;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    //property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Spacing default TCustomStackPanel.DefaultSpacing;
    //property StyleElements;
    //property StyleName;
    property TabOrder;
    property TabStop;
    //property Touch;
    property UseDockManager default True;
    property VerticalPositioning default TCustomStackPanel.DefaultVerticalPositioning;
    property Visible;
    property Width default TCustomStackPanel.DefaultWidth;

    property OnAlignInsertBefore;
    property OnAlignPosition;
    //property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    //property OnGesture;
    property OnGetSiteInfo;
    //property OnMouseActivate;
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

implementation

{ TStackPanelControlItem Methods }

constructor TStackPanelControlItem.Create(ControlCollection: TCollection);
begin
  inherited;
  FHorizontalPositioning := sphpDefault;
  FVerticalPositioning := spvpDefault;
end;

function TStackPanelControlItem.GetBounds: TRect;
begin
  Result := Control.BoundsRect;
end;

procedure TStackPanelControlItem.MapAlign;
begin
  if Control <> nil then
  begin
    case Control.Align of
      alTop:
        begin
          HorizontalPositioning := sphpFill;
          VerticalPositioning := spvpTop;
        end;
      alBottom:
        begin
          HorizontalPositioning := sphpFill;
          VerticalPositioning := spvpBottom;
        end;
      alLeft:
        begin
          VerticalPositioning := spvpFill;
          HorizontalPositioning := sphpLeft;
        end;
      alRight:
        begin
          VerticalPositioning := spvpFill;
          HorizontalPositioning := sphpRight;
        end;
      alClient:
        begin
          HorizontalPositioning := sphpFill;
          VerticalPositioning := spvpFill;
        end;
      alNone, alCustom:
        { do nothing };
    end;
  end;
end;

procedure TStackPanelControlItem.SetBounds(const Value: TRect);
begin
  //Control.BorderSpacing..SetControlBounds(Value);
  Control.Width := Value.Width;
  Control.Height := Value.Height;
  Control.Left := Value.Left;
  Control.Top := Value.Top;
end;

procedure TStackPanelControlItem.SetHorizontalPositioning(Value: TStackPanelControlHorizontalPositioning);
begin
  if FHorizontalPositioning <> Value then
  begin
    FHorizontalPositioning := Value;
    Changed(False);
  end;
end;

procedure TStackPanelControlItem.SetVerticalPositioning(Value: TStackPanelControlVerticalPositioning);
begin
  if FVerticalPositioning <> Value then
  begin
    FVerticalPositioning := Value;
    Changed(False);
  end;
end;

procedure TStackPanelControlItem.Assign(Source: TPersistent);
begin
  if Source is TStackPanelControlItem then
  begin
    FHorizontalPositioning := TStackPanelControlItem(Source).HorizontalPositioning;
    FVerticalPositioning := TStackPanelControlItem(Source).VerticalPositioning;
    Control := TStackPanelControlItem(Source).Control;
    //Bounds := TStackPanelControlItem(Source).Bounds;
  end
  else
    inherited Assign(Source);
end;

{ TStackPanelControlCollection Methods }

function TStackPanelControlCollection.Add: TStackPanelControlItem;
begin
  Result := TStackPanelControlItem(inherited Add)
end;

function TStackPanelControlCollection.GetItem(AIndex: Integer): TStackPanelControlItem;
begin
  Result := TStackPanelControlItem(inherited Items[AIndex]);
end;

function TStackPanelControlCollection.IndexOf(AControl: TControl): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if TStackPanelControlItem(Items[Result]).Control = AControl then
      Exit;
  end;
  Result := -1;
end;

procedure TStackPanelControlCollection.Update(Item: TCollectionItem);
begin
  inherited;
  Assert(Owner is TWinControl);
  TWinControl(Owner).Realign;
  TWinControl(Owner).Invalidate;
end;

{ TCustomStackPanel Methods }

constructor TCustomStackPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControlCollection := TStackPanelControlCollection.Create(Self, TStackPanelControlItem);

  FHorizontalPositioning := DefaultHorizontalPositioning;
  FOrientation := DefaultOrientation;
  FSpacing := DefaultSpacing;
  FVerticalPositioning := DefaultVerticalPositioning;
  //ShowCaption := False;
  Height := DefaultHeight;
  Width := DefaultWidth;
end;

destructor TCustomStackPanel.Destroy;
begin
  FreeAndNil(FControlCollection);
  inherited;
end;

procedure TCustomStackPanel.AlignControls(AControl: TControl; var Rect: TRect);
var
  I: Integer;
  Position: TPoint;
  Size: TSize;
  Control: TControl;
  ControlInfo: TStackPanelControlItem;
  Bounds: TRect;
  HorzPos: TStackPanelControlHorizontalPositioning;
  VertPos: TStackPanelControlVerticalPositioning;
begin
  if (csDesigning in ComponentState) and (AControl <> nil) then
    MoveControlToItsCoordinates(AControl);

  if (FControlCollection <> nil) and (ControlCollection.Count > 0) then
  begin
    AdjustClientRect(Rect);
    Position := Rect.TopLeft;

    for I := 0 to ControlCollection.Count - 1 do
    begin
      ControlInfo := ControlCollection[I];
      if ControlInfo <> nil then
      begin
        ControlInfo.MapAlign;

        Control := ControlInfo.Control;
        if ((Control <> nil) and (Control.Visible or (csDesigning in ComponentState))) then
        begin
          // The area occupied by the control is affected by the Margins
          Size.cx := Control.BorderSpacing.ControlWidth;
          Size.cy := Control.BorderSpacing.ControlHeight;
          //Bounds := ControlInfo.Control.;
          Bounds.Width := Size.cx;
          Bounds.Height := Size.cy;

          if Orientation = spoVertical then
          begin
            HorzPos := ControlHorizontalPositioning[Control];
            if HorzPos = sphpDefault then
              HorzPos := FHorizontalPositioning;

            case HorzPos of
              sphpLeft:
                Bounds.SetLocation(Rect.Left, Position.Y);
              sphpCenter:
                Bounds.SetLocation(Rect.Left + (Rect.Width - Size.cx) div 2, Position.Y);
              sphpRight:
                Bounds.SetLocation(Rect.Left + Rect.Width - Size.cx, Position.Y);
              sphpFill:
                begin
                  Bounds.SetLocation(Rect.Left, Position.Y);
                  Bounds.Width := Rect.Width;
                end
            end;

            Inc(Position.Y, Size.cy + Spacing);
          end
          else if Orientation = spoHorizontal then
          begin
            VertPos := ControlVerticalPositioning[Control];
            if VertPos = spvpDefault then
              VertPos := FVerticalPositioning;

            case VertPos of
              spvpTop:
                Bounds.SetLocation(Position.X, Rect.Top);
              spvpCenter:
                Bounds.SetLocation(Position.X, Rect.Top + (Rect.Height - Size.cy) div 2);
              spvpBottom:
                Bounds.SetLocation(Position.X, Rect.Top + Rect.Height - Size.cy);
              spvpFill:
                begin
                  Bounds.SetLocation(Position.X, Rect.Top);
                  Bounds.Height := Rect.Height;
                end
            end;

            Inc(Position.X, Size.cx + Spacing);
          end;

          ControlInfo.SetBounds(Bounds);
          if csDesigning in ComponentState then
            Control.Invalidate;
        end;
      end;
    end;
    ControlsAligned;
  end;
  if Showing then
    {AdjustSize};
end;

procedure TCustomStackPanel.CMControlChange(var Msg: TCMControlChange);
var
  Index: Integer;
begin
  inherited;

  if (csDestroying in ComponentState) or (csLoading in ComponentState) or (FControlCollection = nil) then
    Exit;

  if Msg.Inserting and (Msg.Control.Parent = Self) then
  begin
    DisableAlign;
    try
      if (ControlCollection.IndexOf(Msg.Control) < 0) and (Msg.Control.Owner = Self.Owner) then
      begin
        ControlCollection.Add.Control := Msg.Control;
        if csDesigning in ComponentState then
          MoveControlToItsCoordinates(Msg.Control);
        Realign;
      end;
    finally
      EnableAlign;
    end;
  end
  else
  begin
    Index := ControlCollection.IndexOf(Msg.Control);
    if (Index > -1) then
      ControlCollection.Delete(Index);
  end;
end;

procedure TCustomStackPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Control: TControl;
begin
  if ControlCollection <> nil then
  begin
    for I := 0 to ControlCollection.Count - 1 do
    begin
      Control := TControl(ControlCollection[I].Control);
      if (Control <> nil) and (Control.Owner = Root) then
        Proc(Control);
    end;
  end;
end;

function TCustomStackPanel.GetControlHorizontalPositioning(AControl: TControl): TStackPanelControlHorizontalPositioning;
var
  ControlIdx: Integer;
begin
  Result := sphpDefault;
  ControlIdx := GetControlIndex(AControl);
  if (ControlIdx > -1) and (ControlIdx < ControlCollection.Count) then
    Result := ControlCollection[ControlIdx].HorizontalPositioning;
end;

function TCustomStackPanel.GetControlIndex(AControl: TControl): Integer;
begin
  Result := ControlCollection.IndexOf(AControl);
end;

function TCustomStackPanel.GetControlVerticalPositioning(AControl: TControl): TStackPanelControlVerticalPositioning;
var
  ControlIdx: Integer;
begin
  Result := spvpDefault;
  ControlIdx := GetControlIndex(AControl);
  if (ControlIdx > -1) and (ControlIdx < ControlCollection.Count) then
    Result := ControlCollection[ControlIdx].VerticalPositioning;
end;

procedure TCustomStackPanel.MoveControlToItsCoordinates(AMovedControl: TControl);
const
  MaxDelta = 8;
var
  I, CurIndex, FoundIndex, Delta: Integer;
  Control: TControl;
  ControlRect: TRect;
begin
  if (ControlCollection = nil) or (AMovedControl = nil) then
    Exit;

  CurIndex := GetControlIndex(AMovedControl);

  if (CurIndex = -1) or (CurIndex >= ControlCollection.Count) then
    Exit;

  ControlRect := AMovedControl.BoundsRect;
  FoundIndex := 0;

  for I := 0 to ControlCollection.Count - 1 do
  begin
    Control := ControlCollection[I].Control;
    if (Control = nil) or (Control = AMovedControl) then
      Continue;

    case Orientation of
      spoVertical:
        begin
          Delta := Min(MaxDelta, Control.Height div 2);
          if ((ControlRect.Top + Delta) > (Control.Top + Control.Height div 2)) then
            FoundIndex := I + 1;
        end;

      spoHorizontal:
        begin
          Delta := Min(MaxDelta, Control.Width div 2);
          if ((ControlRect.Left + Delta) > (Control.Left + Control.Width div 2)) then
            FoundIndex := I + 1;
        end;
    end;
  end;

  if (FoundIndex > CurIndex) and (FoundIndex > 0) then
    FoundIndex := FoundIndex - 1;

  if (FoundIndex >= ControlCollection.Count) then
    FoundIndex := ControlCollection.Count - 1;

  ControlCollection[CurIndex].Index := FoundIndex;
end;

procedure TCustomStackPanel.SetControlCollection(const Value: TStackPanelControlCollection);
begin
  ControlCollection.Assign(Value);
end;

procedure TCustomStackPanel.SetControlHorizontalPositioning(AControl: TControl;
  Positioning: TStackPanelControlHorizontalPositioning);
var
  ControlIdx: Integer;
begin
  ControlIdx := GetControlIndex(AControl);
  if (ControlIdx > -1) and (ControlIdx < ControlCollection.Count) then
    ControlCollection[ControlIdx].HorizontalPositioning := Positioning;
end;

procedure TCustomStackPanel.SetControlIndex(AControl: TControl; Index: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetControlIndex(AControl);
  if (CurIndex > -1) and (CurIndex <> Index) and (Index < ControlCollection.Count) then
  begin
    ControlCollection[CurIndex].Index := Index;
    Realign;
  end;
end;

procedure TCustomStackPanel.SetControlVerticalPositioning(AControl: TControl;
  Positioning: TStackPanelControlVerticalPositioning);
var
  Index: Integer;
begin
  Index := GetControlIndex(AControl);
  if (Index > -1) and (Index < ControlCollection.Count) then
    ControlCollection[Index].VerticalPositioning := Positioning;
end;

procedure TCustomStackPanel.SetHorizontalPositioning(const Value: TStackPanelHorizontalPositioning);
begin
  if FHorizontalPositioning <> Value then
  begin
    FHorizontalPositioning := Value;
    if Orientation = spoVertical then
      Realign;
  end;
end;

procedure TCustomStackPanel.SetOrientation(const Value: TStackPanelOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Realign;
  end;
end;

procedure TCustomStackPanel.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Realign;
  end;
end;

procedure TCustomStackPanel.SetVerticalPositioning(const Value: TStackPanelVerticalPositioning);
begin
  if FVerticalPositioning <> Value then
  begin
    FVerticalPositioning := Value;
    if Orientation = spoHorizontal then
      Realign;
  end;
end;

end.
