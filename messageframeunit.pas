unit MessageFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics, augvkapi,
  AugImage, BGRAShape, atshapelinebgra, BCRoundedImage, BGRASpriteAnimation,
  RichView, Types;

type

	{ TMessageFrame }

  TMessageFrame = class(TFrame)
    AvatarImage: TImage;
    BGRAShape1: TBGRAShape;
    DateTimeLabel: TLabel;
    ImagesFlow: TFlowPanel;
    MessageTextLabel: TLabel;
		NameLabel: TLabel;
    Panel1: TPanel;
    AttachmentsPanel: TPanel;
    procedure FrameMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FrameResize(Sender: TObject);
    procedure ImageClick(Sender: TObject);
    procedure ChangeDesignToRight;
    procedure MakeRoundImage(Bmp: TBitmap);
    procedure FillImagePanel;
  private

  public
    MessageObject: augvkapi.TMSG;
    procedure Fill(Msg: augvkapi.TMSG);
    procedure RecalcSize;
    destructor Free;
  end;

implementation

{$R *.lfm}

uses
  MainFormUnit, DateUtils, LCLType, LCLIntf, LMessages, Math, MediaViewerFormUnit;

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

procedure TMessageFrame.ImageClick(Sender: TObject);
begin
  MediaViewerForm.AugImage1.Picture := TAugImage(Sender).Picture;
  MediaViewerForm.Show;
end;

procedure TMessageFrame.ChangeDesignToRight;
  procedure SwapAnchors(Control: TControl);
    function InvertSideRef(SideRef: TAnchorSideReference): TAnchorSideReference;
    begin
      if SideRef = asrTop then
        Result := asrBottom
      else if SideRef = asrBottom then
        Result := asrTop;
    end;

  var
    TempControl: TControl;
    TempBorder: Integer;
  begin
    with Control do
    begin
      if not ((akRight in Anchors) and (akLeft in Anchors)) then
      begin
        if akRight in Anchors then
        begin
          Anchors := Anchors - [akRight];
          Anchors := Anchors + [akLeft];
        end;
        if akLeft in Anchors then
        begin
          Anchors := Anchors - [akLeft];
          Anchors := Anchors + [akRight];
        end;
      end;

      TempControl := AnchorSideLeft.Control;
      TempBorder := BorderSpacing.Left;

      AnchorSideLeft.Control := AnchorSideRight.Control;
      BorderSpacing.Left := BorderSpacing.Right;

      AnchorSideRight.Control := TempControl;
      BorderSpacing.Right := TempBorder;
    end;
  end;

var
  Temp: Integer;
begin
  with Panel1 do
  begin
    AnchorSideRight.Side := asrRight;
    SwapAnchors(Panel1);
  end;

  with MessageTextLabel do
  begin
    Alignment := taRightJustify;
    AnchorSideLeft.Side := asrLeft;
    AnchorSideRight.Side := asrLeft;
    SwapAnchors(MessageTextLabel);
  end;

  with NameLabel do
  begin
    AnchorSideRight.Side := asrLeft;
    SwapAnchors(NameLabel);
  end;

  with DateTimeLabel do
  begin
    AnchorSideRight.Side := asrLeft;
    SwapAnchors(DateTimeLabel);
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

procedure TMessageFrame.FillImagePanel;
var
  Attach: TAttachment;
  Image: TAugImage;
  I: Integer;
begin
  for Attach in MessageObject.Attachments do
  begin
    if Attach.AttachType = atPhoto then
    begin
      Image := TAugImage.Create(Self);
      Image.Parent := ImagesFlow;
      Image.BorderSpacing.Around := 2;
      Image.Center := True;
      Image.Cover := True;
      Image.Picture := Attach.Preview;
      Image.OnClick := @ImageClick;
    end;
  end;
end;

procedure TMessageFrame.Fill(Msg: augvkapi.TMSG);
var
  Date: TDateTime;
begin
  MessageObject := Msg;
  AvatarImage.Picture := Msg.fromId.Image;
  //MakeRoundImage(AvatarImage.Picture.Bitmap);
  //SetRoundRectRegion(Panel1, 50, 50);
  NameLabel.Caption := Msg.fromId.Name;
  MessageTextLabel.Caption := Msg.Text;
  Date := UnixToDateTime(Msg.Date);
  if DaysBetween(Now, Date) >= 1 then
    DateTimeLabel.Caption := FormatDateTime('DD.MM.YYYY h:nn', Date)
  else
    DateTimeLabel.Caption := FormatDateTime('h:nn', Date);

  FillImagePanel;

  //if msg.FromId.Id = MainForm.ActiveUser.Id then
  //  ChangeDesignToRight;

  RecalcSize;
end;

procedure TMessageFrame.RecalcSize;
var
  AutoSizeHeight: Integer;
  i: Integer;
  ImagesMod: Integer;
  ImagesCount: Integer;
begin
  ImagesCount := ImagesFlow.ControlList.Count;
  if ImagesCount = 1 then
  begin
    with TAugImage(ImagesFlow.ControlList[0].Control) do
    begin
      Width := ImagesFlow.Width;
      Height := Trunc(Picture.Height*(ImagesFlow.Width/Picture.Width));
    end;
  end
  else
  begin
    ImagesMod := ImagesCount mod 3;
    for i:=0 to ImagesCount-1 do
    begin
      ImagesFlow.ControlList[i].Control.Height := 150;
      if i < ImagesCount-ImagesMod then
        ImagesFlow.ControlList[i].Control.Width := Trunc(ImagesFlow.Width/3)-4-1
      else
        ImagesFlow.ControlList[i].Control.Width := Trunc(ImagesFlow.Width/ImagesMod)-4-1;
    end;
  end;

  AutoSizeHeight := AttachmentsPanel.Top+AttachmentsPanel.Height+10;
    //AvatarImage.BorderSpacing.Top+NameLabel.Height+MessageTextLabel.Height+10;
  if AutoSizeHeight > Constraints.MinHeight then
    Height := AutoSizeHeight
  else
    Height := Constraints.MinHeight;
end;

destructor TMessageFrame.Free;
begin
  FreeAndNil(MessageObject);
end;

end.

