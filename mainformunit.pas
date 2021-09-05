unit MainFormUnit;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ComCtrls, ActnList, Menus, PairSplitter, VkLongpoll, fpjson, Utils,
  CachedLongpoll, augvkapi, MessageFrameUnit, ChatFrameUnit, Design, StackPanel,
  AugScrollBox, BCSVGButton, BCListBox, BCPanel, fgl, Types, AugVKApiThread,
  ConfigUtils, {WebBrowserFormUnit,} LoginFrameUnit;

type
  { TMainForm }

  TChatsMap = specialize TFPGMap<Integer, TChatFrame>;

  TMainForm = class(TForm)
    BCSVGButton1: TBCSVGButton;
    ChatListScroll: TScrollBox;
    ActionList1: TActionList;
    ChatScroll: TAugScrollBox;
    LoginFrameForm: TLoginFrame;
    Memo1: TMemo;
    DialogsPanel: TPanel;
    ChatPanel: TPanel;
    ShowMenuItem: TMenuItem;
    CloseMenuItem: TMenuItem;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    PopupMenu1: TPopupMenu;
    SpeedButton1: TBCSVGButton;
    Splitter1: TSplitter;
    StackPanel1: TStackPanel;
    StackPanel2: TStackPanel;
    TrayIcon1: TTrayIcon;
    procedure BCSVGButton1Click(Sender: TObject);
    procedure ChatScrollVScroll(Sender: TObject; var ScrollPos: Integer);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure CustomExceptionHandler(Sender: TObject; E: Exception);
    procedure FormResize(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ShowMenuItemClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure TrayIcon1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetDialogsWidthPrcnt(AWidth: Real);
    function GetDialogsWidthPrcnt: Real;
  private
    DialogsOffset: Integer;
    ChatPage: Integer;

    procedure OnLogined(Token: String; ExpiresIn, Id: Integer);
    procedure AfterLogin(Token: String);
  public
    SelectedChat: Integer;
    ActiveUser: TUser;
    CompactView: Boolean;

    procedure ShowBothPanels;
    procedure ShowOnlyChat;
    procedure ShowOnlyDialogs;
    procedure ClearChat;
    procedure LoadChat(Id: Integer; Page: Integer=0; StartMsg: Integer=-1; ToTop: Boolean=True);
    procedure LoadChatCallback(Response: TObject; Data: Pointer);
    procedure OpenChat(Id: Integer);
    procedure CloseChat;

    property DialogsWidthPrcnt: Real read GetDialogsWidthPrcnt write SetDialogsWidthPrcnt;
  end;

var
  MainForm: TMainForm;
  LongpollThread: TCachedLongpoll;
  AugVK: TAugVKAPI;

implementation

uses
  LazLogger;

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
  //WebBrowser: TWebBrowserForm;
  TokenPath: String;
begin
	Application.OnException := @CustomExceptionHandler;

  // Получение токена
  TokenPath := Format('accounts[%d].token', [Config.Integers['active_account']]);
  if Config.FindPath(TokenPath) = nil then
  begin
    //WebBrowser := TWebBrowserForm.Create(MainForm);
    //Token := WebBrowser.GetOAuthToken;
    LoginFrameForm.OnLogined := @OnLogined;
    LoginFrameForm.Show;
    LoginFrameForm.BringToFront;
  end
  else
    AfterLogin(Config.GetPath(TokenPath).AsString);

  DialogsWidthPrcnt := Config.Floats['dialogs_width'];
end;

procedure TMainForm.CustomExceptionHandler(Sender: TObject; E: Exception);
begin
  DebugLn(DumpExceptionCallStack(E));
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  if Width <= 500 then
    if not CompactView then
      ShowOnlyChat
    else // MEGA HAX00R HACK FOR STUPID OPTIMYZER THAT THINK INNER "if" CAN BE "and"
  else
    ShowBothPanels;
  DialogsPanel.Width := Trunc(Width * DialogsWidthPrcnt);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  MainForm.Hide;
  CanClose:=False;
end;

procedure TMainForm.BCSVGButton1Click(Sender: TObject);
begin
  ShowOnlyDialogs;
end;

procedure TMainForm.ChatScrollVScroll(Sender: TObject; var ScrollPos: Integer);
var
  BorderMessage: TMessageFrame;
begin
  if ChatScroll.VertScrollBar.Position = 0 then
  begin
    //ClearChat;
    BorderMessage := TMessageFrame(StackPanel2.ControlCollection[0].Control);
    ChatPage += 1;
    LoadChat(SelectedChat, ChatPage, BorderMessage.MessageObject.Id);
    //DebugLn(BorderMessage.Top);
    ChatScroll.VertScrollBar.Position := BorderMessage.Top;
      //Trunc(ChatScroll.VertScrollBar.Range / 2) - MainForm.ChatScroll.VertScrollBar.Page;
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

procedure TMainForm.ShowMenuItemClick(Sender: TObject);
begin
  Show;
end;

procedure TMainForm.CloseMenuItemClick(Sender: TObject);
begin
  Halt;
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
  Repaint;
  DialogsPanel.Repaint;
  ChatPanel.Repaint;
  Update;
  DialogsPanel.Update;
  ChatPanel.Update;
  Config.Floats['dialogs_width'] := DialogsWidthPrcnt;
  SaveConfig;
  //DebugLn(DialogsWidthPrcnt);
end;

procedure TMainForm.TrayIcon1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    PopupMenu1.PopUp;
  if Button = mbLeft then
    if Visible then
      Hide
    else
      Show;
end;

procedure TMainForm.SetDialogsWidthPrcnt(AWidth: Real);
begin
  DialogsPanel.Width := Trunc(Width*AWidth);
end;

function TMainForm.GetDialogsWidthPrcnt: Real;
begin
  Result := DialogsPanel.Width/Width;
end;

procedure TMainForm.OnLogined(Token: String; ExpiresIn, Id: Integer);
var
  AccountAddedId: Integer;
begin
  AccountAddedId := Config.Arrays['accounts'].Add(
    TJSONObject.Create(['token', Token, 'expires_in', ExpiresIn, 'user_id', Id]));
  Config.Integers['active_account'] := AccountAddedId;
  SaveConfig;

  LoginFrameForm.Hide;

  AfterLogin(Token);
end;

procedure TMainForm.AfterLogin(Token: String);
var
  Chat: TChat;
  Chats: TChatsList;
  Frame: TChatFrame;
begin
  // создание AugVKAPI
  AugVK := TAugVKAPI.Create(Token);

  ActiveUser := AugVK.GetUser(-1);

  SelectedChat := -1;

  LongpollThread := TCachedLongpoll.Create(Token);
  LongpollThread.RegisterEventHandler(4, @NewMessageHandler);

  LongpollThread.Start;

  // загрузка чатов
  Chats := AugVK.GetChats(20,0);
  for Chat in Chats do
  begin
    Frame := TChatFrame.Create(MainForm.StackPanel1.Owner);
    Frame.Name := Frame.Name+IntToStr(Chat.Id).Replace('-', '_');
    Frame.Fill(Chat);
    Frame.Parent := MainForm.StackPanel1;
    //Frame.Width := MainForm.FlowPanel2.Width;
    //MainForm.FlowPanel2.Height := MainForm.FlowPanel2.Height+Frame.Height;
  end;
end;

procedure TMainForm.ShowBothPanels;
begin
  DialogsPanel.Show;
  ChatPanel.Show;
  BCSVGButton1.Hide;
  DialogsPanel.Align := alLeft;
  Splitter1.Show;
  DialogsWidthPrcnt := Config.Floats['dialogs_width'];
  CompactView := False;
end;

procedure TMainForm.ShowOnlyChat;
begin
  DialogsPanel.Hide;
  ChatPanel.Show;
  BCSVGButton1.Show;
  Splitter1.Hide;
  DialogsPanel.Align := alClient;
  CompactView := True;
end;

procedure TMainForm.ShowOnlyDialogs;
begin
  DialogsPanel.Show;
  ChatPanel.Hide;
  Splitter1.Hide;
  ChatPanel.Align := alClient;
  CompactView := True;
end;

procedure TMainForm.ClearChat;
var
  Item : TControl;
begin
  while StackPanel2.ControlCount > 0 do
  begin
    try
      Item := StackPanel2.Controls[0];
      Item.Free;
    finally
    end;
  end;

end;

procedure TMainForm.LoadChat(Id: Integer; Page: Integer=0; StartMsg: Integer=-1; ToTop: Boolean=True);
begin
  //msgs := AugVK.GetHistory(Id, 30, Page*30, StartMsg);
  TVKThread.Create
  .AddCallback(@LoadChatCallback)
  .AddCallbackData(Pointer(Integer(ToTop)))
  .GetHistory(Id, 30, Page*30, StartMsg)
  .Start;
end;

procedure TMainForm.LoadChatCallback(Response: TObject; Data: Pointer);
var
  msgs: TMSGsList;
  msg: TMSG;
  frame: TMessageFrame;
  ToTop: Boolean;
begin
  ToTop := Boolean(Data);
  msgs := TMSGsList(Response);
	for msg in msgs do
  begin
    Frame := TMessageFrame.Create(Self);
    Frame.Name := Frame.Name+IntToStr(msg.Id);
    Frame.Parent := StackPanel2;
    Frame.Fill(msg);
    if ToTop then
      StackPanel2.ControlCollection.Move(StackPanel2.ControlCollection.Count-1, 0);
    //MainForm.StackPanel2.Height := MainForm.StackPanel2.Height+Frame.Height;
  end;
  //free TMSGsArray(Response)
end;

procedure TMainForm.OpenChat(Id: Integer);
begin
  if MainForm.CompactView then
    MainForm.ShowOnlyChat;
  if MainForm.SelectedChat = Id then
    exit;
  MainForm.SelectedChat := Id;

  // очистка чата
  ClearChat;

  LoadChat(Id);

  MainForm.ChatScroll.VertScrollBar.Position :=
    MainForm.ChatScroll.VertScrollBar.Range - MainForm.ChatScroll.VertScrollBar.Page;
end;

procedure TMainForm.CloseChat;
begin
  SelectedChat := -1;
  ClearChat;
end;

{ Actions }


end.
