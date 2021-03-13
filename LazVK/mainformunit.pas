unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
	StdCtrls, ComCtrls, VkLongpoll, fpjson, Utils, CachedLongpoll, augvkapi,
	MessageFrameUnit, Types, Math;

type

{ TMainForm }

	TMainForm = class(TForm)
		ListBox1: TListBox;
		ListBox2: TListBox;
		Memo1: TMemo;
		Panel1: TPanel;
		Panel2: TPanel;
		Panel3: TPanel;
		Panel4: TPanel;
		Panel5: TPanel;
		ScrollBox1: TScrollBox;
		SpeedButton1: TSpeedButton;
		Splitter1: TSplitter;
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure ListBox1Click(Sender: TObject);
		procedure ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
			WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
	private

	public

	end;

var
  MainForm: TMainForm;
  LongpollThread: TCachedLongpoll;

implementation

var
  AugVK: TAugVKAPI;

{$R *.lfm}

procedure NewMessageHandler(Event: TJSONArray);
var
  Chat: TChat;
begin
  //MainForm.ListBox2.AddItem(Event.Strings[5], nil);

  //augvk.updateChatsPosition(Event.Integers[3]);
  //MainForm.ListBox1.Items.Clear;
  //for chat in augvk.getChatsForDraw do
  //  MainForm.ListBox1.Items.Add(chat.name);
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  Token: String;
begin
  Token := Config.GetPath(Format('accounts[%d].token', [Config.Integers['active_account']])).AsString;
  AugVK := TAugVKAPI.Create(Token);

  LongpollThread := TCachedLongpoll.Create(Token);
  LongpollThread.RegisterEventHandler(4, @NewMessageHandler);

  LongpollThread.Start;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  Chat: TChat;
begin
  for Chat in AugVK.GetChatsForDraw do
    MainForm.ListBox1.Items.Add(Chat.Name);
end;

procedure TMainForm.ListBox1Click(Sender: TObject);
var
  chat: TChat;
  msg: TMSG;
  MsgFrame, PrevMsgFrame: TMessageFrame;
  i: Integer;
begin
  if ListBox1.ItemIndex = -1 then Exit;

  MainForm.ListBox2.Items.Clear;
  Chat := AugVK.GetChatByIndex(ListBox1.ItemIndex);

  i:=0;
  for msg in augvk.getHistory(chat.id,30) do
  begin
    MsgFrame := TMessageFrame.Create(nil);
    MsgFrame.Name := MsgFrame.Name+IntToStr(i);
    MsgFrame.Anchors := [akBottom, akLeft, akRight];

    MsgFrame.AnchorSide[akLeft].Control := ScrollBox1;
    MsgFrame.AnchorSide[akLeft].Side := asrLeft;
    MsgFrame.AnchorSide[akRight].Control := ScrollBox1;
    MsgFrame.AnchorSide[akRight].Side := asrRight;

    if i=0 then
    begin
      MsgFrame.AnchorSide[akBottom].Control := ScrollBox1;
      MsgFrame.AnchorSide[akBottom].Side := asrBottom;
		end
    else
    begin
      MsgFrame.AnchorSide[akBottom].Control := PrevMsgFrame;
      MsgFrame.AnchorSide[akBottom].Side := asrTop;
		end;
		MsgFrame.Parent := ScrollBox1;
    MsgFrame.AvatarImage.Picture.LoadFromFile('logo.png');
    MsgFrame.NameLabel.Caption := msg.fromId.name;
    MsgFrame.MessageTextLabel.Caption := msg.text;
    PrevMsgFrame := MsgFrame;
    Inc(i);
	end;
end;

procedure TMainForm.ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
	WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position + Sign(WheelDelta)
end;

end.

