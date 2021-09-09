unit MessageFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics, augvkapi,
  Types;

type

	{ TMessageFrame }

  TMessageFrame = class(TFrame)
    AvatarImage: TImage;
    DateTimeLabel: TLabel;
    MessageTextLabel: TLabel;
		NameLabel: TLabel;
    Panel1: TPanel;
    procedure FrameMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FrameResize(Sender: TObject);
    procedure ChangeDesignToRight;
    procedure MakeRoundImage(Bmp: TBitmap);
  private

  public
    MessageObject: augvkapi.TMSG;
    procedure Fill(Msg: augvkapi.TMSG);
    procedure RecalcSize;
    destructor Free;
  end;

implementation

{$R *.lfm}

uses MainFormUnit, DateUtils, LCLType, LCLIntf, LMessages;

{ TMessageFrame }

procedure TMessageFrame.FrameMouseWheel(Sender: TObject; Shift: TShiftState;
	WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  //MainForm.ChatScroll.VertScrollBar.Position :=
  //  MainForm.ChatScroll.VertScrollBar.Position + (-Sign(WheelDelta)*15)
  if WheelDelta > 0 then
    SendMessage(MainForm.ChatScroll.Handle, LM_VSCROLL, SB_LINEUP, 0)
  else
    SendMessage(MainForm.ChatScroll.Handle, LM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TMessageFrame.FrameResize(Sender: TObject);
begin
  RecalcSize;
end;

procedure TMessageFrame.ChangeDesignToRight;
var
  Temp: Integer;
begin
  with Panel1 do
  begin
    Anchors := [akTop, akRight];
    AnchorSideLeft.Control := nil;
    AnchorSideRight.Control := Self;
    AnchorSideRight.Side := asrRight;
    // обмен значениями
    Temp := BorderSpacing.Right;
    BorderSpacing.Right := BorderSpacing.Left;
    BorderSpacing.Left := Temp;
    // обмен значениями
  end;

  with MessageTextLabel do
  begin
    Alignment := taRightJustify;
    AnchorSideLeft.Control := Self;
    AnchorSideLeft.Side := asrLeft;
    AnchorSideRight.Control := Panel1;
    AnchorSideRight.Side := asrLeft;
    // обмен значениями
    Temp := BorderSpacing.Right;
    BorderSpacing.Right := BorderSpacing.Left;
    BorderSpacing.Left := Temp;
    // обмен значениями
  end;

  with NameLabel do
  begin
    Anchors := [akTop, akRight];
    AnchorSideLeft.Control := nil;
    AnchorSideRight.Control := Panel1;
    AnchorSideRight.Side := asrLeft;
    // обмен значениями
    Temp := BorderSpacing.Right;
    BorderSpacing.Right := BorderSpacing.Left;
    BorderSpacing.Left := Temp;
    // обмен значениями
  end;

  with DateTimeLabel do
  begin
    Anchors := [akTop, akRight];
    AnchorSideLeft.Control := nil;
    AnchorSideRight.Control := NameLabel;
    AnchorSideRight.Side := asrLeft;
    // обмен значениями
    Temp := BorderSpacing.Right;
    BorderSpacing.Right := BorderSpacing.Left;
    BorderSpacing.Left := Temp;
    // обмен значениями
  end;
end;

procedure SetRoundRectRegion(Control: TWinControl;
  EllipseWidth, EllipseHeight: Integer);
var
  NewRgn: HRGN;
begin
  NewRgn := CreateRoundRectRgn(0, 0, Control.Width, Control.Height,
  EllipseWidth, EllipseHeight);
  SetWindowRgn(Control.Handle, NewRgn, Control.Showing);
end;

procedure TMessageFrame.MakeRoundImage(Bmp: TBitmap);
begin

end;

procedure TMessageFrame.Fill(Msg: augvkapi.TMSG);
var
  Date: TDateTime;
begin
  AvatarImage.Picture := Msg.fromId.Image;
  //MakeRoundImage(AvatarImage.Picture.Bitmap);
  //SetRoundRectRegion(Panel1, 50, 50);
  NameLabel.Caption := Msg.fromId.Name;
  MessageTextLabel.Caption := Msg.Text;
  MessageObject := Msg;
  Date := UnixToDateTime(Msg.Date);
  if DaysBetween(Now, Date) >= 1 then
    DateTimeLabel.Caption := FormatDateTime('DD.MM.YYYY h:nn', Date)
  else
    DateTimeLabel.Caption := FormatDateTime('h:nn', Date);

  if msg.FromId.Id = MainForm.ActiveUser.Id then
    ChangeDesignToRight;

  RecalcSize;
end;

procedure TMessageFrame.RecalcSize;
var
  AutoSizeHeight: Integer;
begin
  AutoSizeHeight := AvatarImage.BorderSpacing.Top+NameLabel.Height+MessageTextLabel.Height+10;
  if AutoSizeHeight > Constraints.MinHeight then
    Height := AutoSizeHeight;
end;

destructor TMessageFrame.Free;
begin
  FreeAndNil(MessageObject);
end;

end.

