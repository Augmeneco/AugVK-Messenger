unit ChatFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, augvkapi,
  Design, DividerBevel, Types;

type

	{ TChatFrame }

  TChatFrame = class(TFrame)
    ChatAvatarImage: TImage;
    TitleLabel: TLabel;
    LastMessageLabel: TLabel;
    procedure FrameClick(Sender: TObject);
    procedure FrameMouseEnter(Sender: TObject);
    procedure FrameMouseLeave(Sender: TObject);
    procedure FrameMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private

  public
    Id: Integer;
    procedure Fill(Chat: TChat);
    constructor Create(TheOwner: TComponent);
  end;

implementation

uses
  MainFormUnit, MessageFrameUnit, LCLType, LCLIntf, LMessages;

{$R *.lfm}

{ TChatFrame }

procedure TChatFrame.FrameClick(Sender: TObject);
begin
  MainForm.OpenChat(Id);
end;

procedure TChatFrame.FrameMouseEnter(Sender: TObject);
begin
  Color := clSilver;
end;

procedure TChatFrame.FrameMouseLeave(Sender: TObject);
begin
  Color := DC_BACKGROUND;
end;

procedure TChatFrame.FrameMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    SendMessage(MainForm.ChatListScroll.Handle, LM_VSCROLL, SB_LINEUP, 0)
  else
    SendMessage(MainForm.ChatListScroll.Handle, LM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TChatFrame.Fill(Chat: TChat);
begin
  ChatAvatarImage.Picture := Chat.Image;
  TitleLabel.Caption := Chat.name;
  LastMessageLabel.Caption := Chat.previewMsg.text;
  Id := Chat.Id;
end;

constructor TChatFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csOpaque];
end;

end.

