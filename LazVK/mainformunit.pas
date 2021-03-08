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
 private

 public

 end;

var
  MainForm: TMainForm;
  LongpollThread: TCachedLongpoll;

implementation
var
  augvk: TAugVKAPI;

{$R *.lfm}

procedure NewMessageHandler(Event: TJSONArray);
var
  chat: TChat;
begin
  MainForm.ListBox2.AddItem(Event.Strings[5], nil);

  augvk.updateChatsPosition(Event.Integers[3]);
  MainForm.ListBox1.Items.Clear;
  for chat in augvk.getChatsForDraw do
    MainForm.ListBox1.Items.Add(chat.name);
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  token: String;
begin
  token := Config.GetPath(Format('accounts[%d].token', [Config.Integers['active_account']])).AsString;
  augvk := TAugVKAPI.Create(token);

  LongpollThread := TCachedLongpoll.Create(token);
  LongpollThread.RegisterEventHandler(4, @NewMessageHandler);

  LongpollThread.Start;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  chat: TChat;
begin
  for chat in augvk.getChatsForDraw do
    MainForm.ListBox1.Items.Add(chat.name);
end;

end.

