unit MessageFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics,
  PairSplitter, ButtonPanel, Design, augvkapi, BCLabel, Types;

type

	{ TMessageFrame }

  TMessageFrame = class(TFrame)
		AvatarImage: TImage;
    Label1: TLabel;
    MessageTextLabel: TLabel;
		NameLabel: TLabel;
    procedure FrameMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FrameResize(Sender: TObject);
  private

  public
    MessageObject: TMSG;
    procedure Fill(Msg: TMSG);
    procedure RecalcSize;
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

procedure TMessageFrame.Fill(Msg: augvkapi.TMSG);
var
  Date: TDateTime;
begin
  AvatarImage.Picture := Msg.fromId.Image;
  NameLabel.Caption := Msg.fromId.Name;
  MessageTextLabel.Caption := Msg.Text;
  MessageObject := Msg;
  Date := UnixToDateTime(Msg.Date);
  if DaysBetween(Now, Date) >= 1 then
    Label1.Caption := FormatDateTime('DD.MM.YYYY h:nn', Date)
  else
    Label1.Caption := FormatDateTime('h:nn', Date);

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

end.

