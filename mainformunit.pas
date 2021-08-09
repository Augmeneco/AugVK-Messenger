unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ComCtrls, ActnList, Menus, VkLongpoll, fpjson, Utils,
  CachedLongpoll, augvkapi, MessageFrameUnit, ChatFrameUnit, Design, StackPanel,
  BCSVGButton, fgl;

type
  { TMainForm }

  TChatsMap = specialize TFPGMap<Integer, TChatFrame>;

  TMainForm = class(TForm)
    ChatListScroll: TScrollBox;
    ActionList1: TActionList;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    ChatScroll: TScrollBox;
    SpeedButton1: TBCSVGButton;
    Splitter1: TSplitter;
    StackPanel1: TStackPanel;
    StackPanel2: TStackPanel;
    TrayIcon1: TTrayIcon;
    procedure ChatListScrollMouseEnter(Sender: TObject);
    procedure ChatListScrollMouseLeave(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure TrayIcon1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private

  public
    SelectedChat: Integer;
    ChatListWidthPercent: Real;
  end;

var
  MainForm: TMainForm;
  LongpollThread: TCachedLongpoll;
  AugVK: TAugVKAPI;

implementation

{$R *.lfm}

procedure NewMessageHandler(Event: TJSONArray);
var
  Idx: Integer;
  Found: Boolean = False;
  Frame: TMessageFrame;
  OnBottom: Boolean = False;
begin
  // перемещение наверх
  for Idx:=0 to MainForm.StackPanel1.ControlCollection.Count-1 do
  begin
    if TChatFrame(MainForm.StackPanel1.ControlCollection[Idx].Control).Id = Event.Integers[3] then
    begin
      Found := True;
      break;
    end;
  end;
  if Found = False then
    exit;

  TChatFrame(MainForm.StackPanel1.ControlCollection[Idx].Control).LastMessageLabel.Caption := Event.Strings[5];
  MainForm.StackPanel1.ControlCollection.Move(Idx, 0);

  // если чат не выбран то выйти
  if MainForm.SelectedChat = -1 then exit;

  if MainForm.ChatScroll.VertScrollBar.Position = MainForm.ChatScroll.VertScrollBar.Range - MainForm.ChatScroll.VertScrollBar.Page then
    OnBottom := True;

  // если чат открыт то добавить сообщение
  if MainForm.SelectedChat = Event.Integers[3] then
  begin
    Frame := TMessageFrame.Create(MainForm.StackPanel2.Owner);
    Frame.Name := Frame.Name+IntToStr(Event.Integers[1]);
    Frame.Fill(LongpollThread.GetCache(Event.Integers[3], 1)[0]);
    Frame.Parent := MainForm.StackPanel2;
    //Frame.Constraints.MaxWidth := MainForm.FlowPanel1.Width;
    //Frame.Width := MainForm.FlowPanel1.Width;
    //MainForm.FlowPanel1.Height := MainForm.FlowPanel1.Height+Frame.Height;
  end;

  if OnBottom then
    MainForm.ChatScroll.VertScrollBar.Position :=
      MainForm.ChatScroll.VertScrollBar.Range - MainForm.ChatScroll.VertScrollBar.Page;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  Token: string;
  Chat: TChat;
  Frame: TChatFrame;
begin
  Token := Config.GetPath(Format('accounts[%d].token',
    [Config.Integers['active_account']])).AsString;
  AugVK := TAugVKAPI.Create(Token);

  SelectedChat := -1;

  LongpollThread := TCachedLongpoll.Create(Token);
  LongpollThread.RegisterEventHandler(4, @NewMessageHandler);

  LongpollThread.Start;

  // загрузка чатов
  for Chat in AugVK.GetChatsForDraw do
  begin
    Frame := TChatFrame.Create(MainForm.StackPanel1.Owner);
    Frame.Name := Frame.Name+IntToStr(Chat.Id).Replace('-', '_');
    Frame.Fill(Chat);
    Frame.Parent := MainForm.StackPanel1;
    //Frame.Width := MainForm.FlowPanel2.Width;
    //MainForm.FlowPanel2.Height := MainForm.FlowPanel2.Height+Frame.Height;
  end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  Panel1.Width := Trunc(Width * ChatListWidthPercent);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  MainForm.Hide;
  CanClose:=False;
end;

procedure TMainForm.ChatListScrollMouseLeave(Sender: TObject);
begin
  ChatListScroll.VertScrollBar.Visible := False;
end;

procedure TMainForm.ChatListScrollMouseEnter(Sender: TObject);
begin
  ChatListScroll.VertScrollBar.Visible := True;
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

procedure TMainForm.Splitter1Moved(Sender: TObject);
begin
  Invalidate;
  ChatListWidthPercent := Panel1.Width / Width;
end;

procedure TMainForm.TrayIcon1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //if Button = mbRight then
  //  PopupMenu1.PopUp;
  if Button = mbLeft then
    if Visible then
      Hide
    else
      Show;
end;

{ Actions }


end.
