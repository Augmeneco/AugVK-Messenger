unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, {IpHtml,} VkLongpoll, fpjson, Utils, CachedLongpoll, augvkapi;

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
   SpeedButton1: TSpeedButton;
   Splitter1: TSplitter;
   procedure FormCreate(Sender: TObject);
   procedure FormShow(Sender: TObject);
   procedure ListBox1Click(Sender: TObject);
 private

 public

 end;

var
  MainForm: TMainForm;
  LongpollThread: TCachedLongpoll;

implementation
var
  AugVK: TAugVKAPI;
  SelectedChat: Integer;

{$R *.lfm}

procedure NewMessageHandler(Event: TJSONArray);
var
  Chat: TChat;
begin
  //MainForm.ListBox2.AddItem(Event.Strings[5], nil);

  //writeln(MainForm.ListBox1.ItemIndex);

  AugVK.UpdateChatsPosition(Event.Integers[3]);
  MainForm.ListBox1.Items.Clear;
  for Chat in AugVK.GetChatsForDraw do
    MainForm.ListBox1.Items.Add(Chat.Name);

  if (SelectedChat = -1) then Exit;

  if SelectedChat = Event.Integers[3] then
    MainForm.ListBox2.Items.Add(
      LongpollThread.GetCache(Event.Integers[3],1)[0].text
    );
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  Token: String;
begin
  Token := Config.GetPath(Format('accounts[%d].token', [Config.Integers['active_account']])).AsString;
  AugVK := TAugVKAPI.Create(Token);
  SelectedChat := -1;

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
  Chat: TChat;
  Msgs: TMSGsArray;
  I: Integer;
begin
  if ListBox1.ItemIndex = -1 then Exit;

  MainForm.ListBox2.Items.Clear;
  Chat := AugVK.GetChatByIndex(ListBox1.ItemIndex);
  SelectedChat := Chat.Id;

  Msgs := LongpollThread.GetCache(SelectedChat);

  for I:=Length(Msgs)-1 downto 0 do
  begin
    MainForm.ListBox2.Items.Add(Msgs[I].Text);
  end;

  MainForm.ListBox2.ItemIndex := MainForm.ListBox2.Items.Count-1;
end;

end.

