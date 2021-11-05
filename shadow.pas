unit Shadow;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, LCLIntf, LCLType, SlaveForms;

type

  { TShadowForm }

  TShadowForm = class(TSlaveForm)
  private
    FBmp: TBitmap;
    procedure FillControlRect(Control: TControl);
    procedure FillControlRects(Control: TWinControl);
  protected
    procedure Paint; override;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMDisplayChange(var Message: TMessage); message WM_DISPLAYCHANGE;
  public
    constructor CreateShadow(AForm: TCustomForm);
    destructor Destroy; override;
    procedure UpdateShadow;
  published
  end;

implementation

{$R *.lfm}

constructor TShadowForm.CreateShadow(AForm: TCustomForm);
begin
  inherited Create(AForm);
  PopupParent := AForm;
  FBmp := TBitmap.Create;
  FBmp.PixelFormat := pf24bit;
end;

destructor TShadowForm.Destroy;
begin
  FBmp.Free;
  inherited;
end;

procedure TShadowForm.Paint;
begin
  Canvas.Draw(0, 0, FBmp);
end;

procedure TShadowForm.FillControlRect(Control: TControl);
var
  I: Integer;
  R: TRect;
begin
  //if Control.Tag = 1 then
  //begin
  //  R := Control.BoundsRect;
  //  //MapWindowPoints(Control.Parent.Handle, PopupParent.Handle, R, 2);
  //  //B.SetSize(R.Width,R.Height);
  //  //B.Canvas.CopyRect(R, Panel.Canvas, R);
  //  FBmp.Canvas.FillRect(R);
  //end;
  if Control is TWinControl then
    FillControlRects(TWinControl(Control));
end;

procedure TShadowForm.FillControlRects(Control: TWinControl);
var
  I: Integer;
begin
  for I := 0 to Control.ControlCount-1 do
    FillControlRect(Control.Controls[I]);
end;

procedure TShadowForm.UpdateShadow;
var
  Pt: TPoint;
  R: TRect;
begin
  Pt := PopupParent.ClientOrigin;
  R := PopupParent.ClientRect;

  FBmp.Width := R.Right - R.Left;
  FBmp.Height := R.Bottom - R.Top;

  FBmp.Canvas.Brush.Color := Color;
  FBmp.Canvas.FillRect(Rect(0, 0, FBmp.Width, FBmp.Height));

  FBmp.Canvas.Brush.Color := clFuchsia;
  FillControlRects(PopupParent);

  SetBounds(Pt.X, Pt.Y, FBmp.Width, FBmp.Height);
  if Showing then
    Invalidate
  else
    ShowWindow(Handle, SW_SHOWNOACTIVATE);
end;

procedure TShadowForm.WMDisplayChange(var Message: TMessage);
begin
  inherited;
  UpdateShadow;
end;

procedure TShadowForm.WMMouseActivate(var Message: TWMMouseActivate);
begin
  //Message.Result := MA_NOACTIVATE;
end;

end.
