unit ChatFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, augvkapi;

type

	{ TChatFrame }

  TChatFrame = class(TFrame)
    Bevel1: TBevel;
    ChatAvatarImage: TImage;
    TitleLabel: TLabel;
    LastMessageLabel: TLabel;
    procedure FrameClick(Sender: TObject);
    procedure FrameMouseEnter(Sender: TObject);
    procedure FrameMouseLeave(Sender: TObject);
  private

  public
    ChatObject: TChat;
    procedure Fill(Chat: TChat);
    constructor Create(TheOwner: TComponent);
  end;

implementation

uses
  MainFormUnit;

{$R *.lfm}

{ TChatFrame }

procedure TChatFrame.FrameClick(Sender: TObject);
var
  msg: TMSG;
begin
  MainForm.SelectedChat := ChatObject.id;
  MainForm.MessagesManager.Clear;
  for msg in augvk.getHistory(ChatObject.id, 30) do
  begin
    MainForm.MessagesManager.Add(msg);
  end;
  MainForm.ChatScroll.VertScrollBar.Position :=
    MainForm.ChatScroll.VertScrollBar.Range - MainForm.ChatScroll.VertScrollBar.Page;
end;

procedure TChatFrame.FrameMouseEnter(Sender: TObject);
begin
  Color := clSilver;
end;

procedure TChatFrame.FrameMouseLeave(Sender: TObject);
begin
  Color := clDefault;
end;

procedure TChatFrame.Fill(Chat: TChat);
begin
  //ChatAvatarImage.Picture := Chat.;
  TitleLabel.Caption := Chat.name;
  LastMessageLabel.Caption := Chat.previewMsg.text;
  ChatObject := Chat;
end;

constructor TChatFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csOpaque];
end;

end.

