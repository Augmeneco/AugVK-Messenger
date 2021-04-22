unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ComCtrls, ActnList, VkLongpoll, fpjson, Utils, CachedLongpoll,
  augvkapi, MessageFrameUnit, ChatFrameUnit, OMultiPanel, Contnrs, fgl;

type
  { TMainForm }

  TChatsMap = specialize TFPGMap<Integer, TChatFrame>;

  TMainForm = class(TForm)
    ChatListScroll: TScrollBox;
    ActionList1: TActionList;
    FlowPanel1: TFlowPanel;
    FlowPanel2: TFlowPanel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    ChatScroll: TScrollBox;
    SpeedButton1: TSpeedButton;
    Splitter1: TSplitter;
    procedure FlowPanel1Resize(Sender: TObject);
    procedure FlowPanel2Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
  private

  public
    SelectedChat: Integer;
  end;

var
  MainForm: TMainForm;
  LongpollThread: TCachedLongpoll;
  AugVK: TAugVKAPI;

implementation

{$R *.lfm}

procedure NewMessageHandler(Event: TJSONArray);
var
  //Chat: TChat;
  Idx: Integer;
  Found: Boolean = False;
  Frame: TMessageFrame;
begin
  // перемещение наверх
  for Idx:=0 to MainForm.FlowPanel2.ControlList.Count-1 do
  begin
    if TChatFrame(MainForm.FlowPanel2.ControlList[Idx].Control).Id = Event.Integers[3] then
    begin
      Found := True;
      break;
    end;
  end;
  if Found = False then
    exit;

  TChatFrame(MainForm.FlowPanel2.ControlList[Idx].Control).LastMessageLabel.Caption := Event.Strings[5];
  MainForm.FlowPanel2.ControlList.Move(Idx, 0);

  // если чат не выбран то выйти
  if MainForm.SelectedChat = -1 then exit;

  // если чат открыт то добавить сообщение
  if MainForm.SelectedChat = Event.Integers[3] then
  begin
    Frame := TMessageFrame.Create(MainForm.FlowPanel1);
    Frame.Name := Frame.Name+IntToStr(Event.Integers[3]);
    Frame.Fill(LongpollThread.GetCache(Event.Integers[3], 1)[0]);
    Frame.Parent := MainForm.FlowPanel1;
    Frame.Width := MainForm.FlowPanel1.Width;
    MainForm.FlowPanel1.Height := MainForm.FlowPanel1.Height+Frame.Height;
  end;

  MainForm.ChatScroll.VertScrollBar.Position :=
    MainForm.ChatScroll.VertScrollBar.Range - MainForm.ChatScroll.VertScrollBar.Page;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  Token: string;
begin
  Token := Config.GetPath(Format('accounts[%d].token',
    [Config.Integers['active_account']])).AsString;
  AugVK := TAugVKAPI.Create(Token);

  SelectedChat := -1;

  LongpollThread := TCachedLongpoll.Create(Token);
  LongpollThread.RegisterEventHandler(4, @NewMessageHandler);

  LongpollThread.Start;
end;

procedure TMainForm.FlowPanel1Resize(Sender: TObject);
var
  i: integer;
begin
  for I:=0 to FlowPanel1.ControlList.Count-1 do
    FlowPanel1.ControlList.Items[I].Control.Width:=FlowPanel1.Width;
end;

procedure TMainForm.FlowPanel2Resize(Sender: TObject);
var
  i: integer;
begin
  for I:=0 to FlowPanel2.ControlList.Count-1 do
    FlowPanel2.ControlList.Items[I].Control.Width:=FlowPanel2.Width;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  Chat: TChat;
  Frame: TChatFrame;
begin
  for Chat in AugVK.GetChatsForDraw do
  begin
    //MainForm.ChatListManager.Add(Chat);
    Frame := TChatFrame.Create(MainForm.FlowPanel2);
    Frame.Name := Frame.Name+IntToStr(Chat.Id).Replace('-', '_');
    Frame.Fill(Chat);
    Frame.Parent := MainForm.FlowPanel2;
    Frame.Width := MainForm.FlowPanel2.Width;
    MainForm.FlowPanel2.Height := MainForm.FlowPanel2.Height+Frame.Height;
  end;
end;

procedure TMainForm.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
  begin
    if SelectedChat <> -1 then
    begin
      AugVK.SendMessage(Memo1.Text, SelectedChat);
      Memo1.Clear;
    end;
  end;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  if SelectedChat <> -1 then
  begin
    AugVK.SendMessage(Memo1.Text, SelectedChat);
    Memo1.Clear;
  end;
end;

{ Actions }


end.
