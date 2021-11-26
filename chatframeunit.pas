unit ChatFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, augvkapi,
  Design, atshapelinebgra, BGRASpriteAnimation, Types;

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
    class function GeneratePreviewText(PreviewMessage: augvkapi.TMSG): String;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  MainFormUnit, LCLType, LCLIntf, LMessages, Utils;

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
  LastMessageLabel.Caption := GeneratePreviewText(Chat.PreviewMsg);
  Id := Chat.Id;
  ChatObject := Chat;
end;

class function TChatFrame.GeneratePreviewText(PreviewMessage: augvkapi.TMSG): String;
begin
  Result := '';
  if PreviewMessage.Attachments.Count > 0 then
  begin
    case PreviewMessage.Attachments[0].AttachType of
      atPhoto: Result += 'Фотография';
      atVideo: Result += 'Видео';
      atAudio: Result += 'Аудиозапись';
      atDoc  : Result += 'Документ';
      atWall : Result += 'Запись';
      atMarket:Result += 'Товар';
      atPoll : Result += 'Опрос';
      atSticker:Result += 'Стикер';
      atGIF  : Result += 'Гифка';
      atURL  : Result += 'Ссылка';
    end;

    if PreviewMessage.Text.Length > 0 then
      Result += ', ';
  end;

  if PreviewMessage.Text.Length > 0 then
    Result += PreviewMessage.Text;
end;

constructor TChatFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csOpaque];
end;

destructor TChatFrame.Destroy;
begin
  //FreeAndNil(ChatObject);
  inherited;
end;

end.

