unit ChatFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, augvkapi,
  Design, Types;

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
    ChatObject: TChat;
    Id: Integer;
    procedure Fill(Chat: TChat);
    constructor Create(TheOwner: TComponent); override;
    destructor Free;
  end;

implementation

uses
  MainFormUnit, LCLType, LCLIntf, LMessages;

{$R *.lfm}

{ TChatFrame }

procedure TChatFrame.FrameClick(Sender: TObject);
begin
  MainForm.OpenChat(Id, ChatObject.PreviewMsg);
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
    SendMessage(MainForm.DialogsScroll.Handle, LM_VSCROLL, SB_LINEUP, 0)
  else
    SendMessage(MainForm.DialogsScroll.Handle, LM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TChatFrame.Fill(Chat: TChat);
begin
  ChatAvatarImage.Picture := Chat.Image;
  TitleLabel.Caption := Chat.name;
  LastMessageLabel.Caption := Chat.previewMsg.text;
  Id := Chat.Id;
  ChatObject := Chat;
end;

constructor TChatFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csOpaque];
end;

destructor TChatFrame.Free;
begin
  FreeAndNil(ChatObject);
end;

end.

