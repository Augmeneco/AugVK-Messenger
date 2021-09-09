unit MainFormUnit;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ComCtrls, ActnList, Menus, VkLongpoll, fpjson, Utils,
  CachedLongpoll, augvkapi, MessageFrameUnit, ChatFrameUnit, StackPanel,
  AugScrollBox, BCSVGButton, BCListBox, fgl, Types, AugVKApiThread,
  ConfigUtils, LoginFrameUnit;

type
  { TMainForm }

  TChatsMap = specialize TFPGMap<Integer, TChatFrame>;

  TScrollTo = (stTop, stBottom, stNotScroll);

  TLoadScrollData = record
    AddToTop: Boolean;
    ScrollTo: TScrollTo;
  end;

  PLoadScrollData = ^TLoadScrollData;

  TMainForm = class(TForm)
    BCSVGButton1: TBCSVGButton;
    ActionList1: TActionList;
    DialogsScroll: TAugScrollBox;
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
    DialogsStack: TStackPanel;
    ChatStack: TStackPanel;
    TrayIcon1: TTrayIcon;
    procedure BCSVGButton1Click(Sender: TObject);
    procedure DialogsScrollVScroll(Sender: TObject; var ScrollPos: Integer);
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
    DialogsPage: Integer;
    ChatBlockLoad: Boolean;
    DialogsBlockLoad: Boolean;

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
    procedure LoadChat(Id: Integer; Page: Integer=0; StartMsg: Integer=-1;
      ToTop: Boolean=True; ScrollTo: TScrollTo=stNotScroll);
    procedure LoadChatCallback(Response: TObject; Data: Pointer);
    procedure OpenChat(Id: Integer; LastMessage: TMSG=nil);
    procedure CloseChat;

    procedure LoadDialogs(Page: Integer=0; ToTop: Boolean=False;
      ScrollTo: TScrollTo=stNotScroll);
    procedure LoadDialogsCallback(Response: TObject; Data: Pointer);

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
  for Idx:=0 to MainForm.DialogsStack.ControlCollection.Count-1 do
  begin
    if TChatFrame(MainForm.DialogsStack.ControlCollection[Idx].Control).Id = Event.Integers[3] then
    begin
      Found := True;
      break;
    end;
  end;
  if Found = False then
    exit;

  TChatFrame(MainForm.DialogsStack.ControlCollection[Idx].Control).LastMessageLabel.Caption := Event.Strings[5];
  MainForm.DialogsStack.ControlCollection.Move(Idx, 0);

  // если чат не выбран то выйти
  if MainForm.SelectedChat = -1 then exit;

  if MainForm.ChatScroll.VertScrollBar.Position = MainForm.ChatScroll.VertScrollBar.Range - MainForm.ChatScroll.VertScrollBar.Page then
    OnBottom := True;

  // если чат открыт то добавить сообщение
  if MainForm.SelectedChat = Event.Integers[3] then
  begin
    Frame := TMessageFrame.Create(MainForm.ChatStack.Owner);
    Frame.Name := Frame.Name+IntToStr(Event.Integers[1]);
    Frame.Fill(LongpollThread.GetCache(Event.Integers[3], 1)[0]);
    Frame.Parent := MainForm.ChatStack;
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

procedure TMainForm.DialogsScrollVScroll(Sender: TObject;
  var ScrollPos: Integer);
begin
  if DialogsBlockLoad then
    exit;
  if DialogsScroll.VertScrollBar.Position =
    (DialogsScroll.VertScrollBar.Range - DialogsScroll.VertScrollBar.Page) then
  begin
    //BorderMessage := TMessageFrame(DialogsStack.ControlCollection[0].Control);
    DialogsPage += 1;
    LoadDialogs(DialogsPage);
    //DebugLn(BorderMessage.Top);
  end;
end;

procedure TMainForm.ChatScrollVScroll(Sender: TObject; var ScrollPos: Integer);
var
  BorderMessage: TMessageFrame;
begin
  if ChatBlockLoad then
    exit;
  if ChatScroll.VertScrollBar.Position = 0 then
  begin
    BorderMessage := TMessageFrame(ChatStack.ControlCollection[0].Control);
    ChatPage += 1;
    LoadChat(SelectedChat, ChatPage, BorderMessage.MessageObject.Id);
    //DebugLn(BorderMessage.Top);
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
begin
  // создание AugVKAPI
  AugVK := TAugVKAPI.Create(Token);

  ActiveUser := AugVK.GetUser(-1);

  SelectedChat := -1;

  LongpollThread := TCachedLongpoll.Create(Token);
  LongpollThread.RegisterEventHandler(4, @NewMessageHandler);
  LongpollThread.Start;

  LoadDialogs(0, False, stTop);
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
  while ChatStack.ControlCount > 0 do
  begin
    try
      Item := ChatStack.Controls[0];
      Item.Free;
    finally
    end;
  end;
end;

procedure TMainForm.LoadChat(Id: Integer; Page: Integer=0; StartMsg: Integer=-1;
  ToTop: Boolean=True; ScrollTo: TScrollTo=stNotScroll);
var
  LoadChatData: PLoadScrollData;
begin
  if ChatBlockLoad then
    exit;
  ChatBlockLoad := True;

  //msgs := AugVK.GetHistory(Id, 30, Page*30, StartMsg);
  LoadChatData := New(PLoadScrollData);
  LoadChatData^.AddToTop := ToTop;
  LoadChatData^.ScrollTo := stNotScroll;//ScrollTo;

  TVKThread.Create
  .AddCallback(@LoadChatCallback)
  .AddCallbackData(LoadChatData)
  .GetHistory(Id, 30, Page*30, StartMsg)
  .Start;
end;

procedure TMainForm.LoadChatCallback(Response: TObject; Data: Pointer);
var
  msgs: TMSGsList;
  msg: TMSG;
  frame: TMessageFrame;
  LoadChatData: PLoadScrollData;
  OldHeight: Integer = 0;
begin
  LoadChatData := PLoadScrollData(Data);
  msgs := TMSGsList(Response);
  if msgs.Count > 0 then
  begin
    OldHeight := ChatStack.Height;
	  for msg in msgs do
    begin
      Frame := TMessageFrame.Create(Self);
      Frame.Name := Frame.Name+IntToStr(msg.Id);
      Frame.Parent := ChatStack;
      Frame.Fill(msg);
      if LoadChatData^.AddToTop then
        ChatStack.ControlCollection.Move(ChatStack.ControlCollection.Count-1, 0);
      case LoadChatData^.ScrollTo of
        stTop:
          ChatScroll.VertScrollBar.Position := 0;
        stBottom:
          ChatScroll.VertScrollBar.Position :=
            ChatScroll.VertScrollBar.Range - ChatScroll.VertScrollBar.Page;
        stNotScroll:
          if LoadChatData^.AddToTop then
            ChatScroll.VertScrollBar.Position := ChatStack.Height - OldHeight
          else
            ChatScroll.VertScrollBar.Position :=
              OldHeight - ChatScroll.VertScrollBar.Page;
      end;
      //MainForm.StackPanel2.Height := MainForm.StackPanel2.Height+Frame.Height;
    end;
  end
  else
    ChatPage -= 1;
  ChatBlockLoad := False;
  Dispose(LoadChatData);
end;

procedure TMainForm.OpenChat(Id: Integer; LastMessage: TMSG=nil);
var
  Frame: TMessageFrame;
  MsgId: Integer = -1;
begin
  if MainForm.CompactView then
    MainForm.ShowOnlyChat;
  if MainForm.SelectedChat = Id then
    exit;
  MainForm.SelectedChat := Id;

  // очистка чата
  ClearChat;

  //if Assigned(LastMessage) then
  //begin
  //  MsgId := LastMessage.Id;
  //  Frame := TMessageFrame.Create(Self);
  //  Frame.Name := Frame.Name+IntToStr(MsgId)+'ass';
  //  Frame.Parent := ChatStack;
  //  Frame.Fill(LastMessage);
  //end;

  LoadChat(Id, 0, MsgId);
end;

procedure TMainForm.CloseChat;
begin
  SelectedChat := -1;
  ClearChat;
end;

procedure TMainForm.LoadDialogs(Page: Integer=0; ToTop: Boolean=False;
  ScrollTo: TScrollTo=stNotScroll);
var
  LoadDialogsData: PLoadScrollData;
begin
  if DialogsBlockLoad then
    exit;
  DialogsBlockLoad := True;

  LoadDialogsData := New(PLoadScrollData);
  LoadDialogsData^.AddToTop := ToTop;
  LoadDialogsData^.ScrollTo := ScrollTo;

  TVKThread.Create
  .AddCallback(@LoadDialogsCallback)
  .AddCallbackData(LoadDialogsData)
  .GetChats(20, Page*20)
  .Start;
end;

procedure TMainForm.LoadDialogsCallback(Response: TObject; Data: Pointer);
var
  chats: TChatsList;
  chat: TChat;
  frame: TChatFrame;
  LoadDialogsData: PLoadScrollData;
  OldHeight: Integer = 0;
begin
  LoadDialogsData := PLoadScrollData(Data);
  chats := TChatsList(Response);
  if chats.Count > 0 then
  begin
    OldHeight := DialogsStack.Height;
	  for chat in chats do
    begin
      Frame := TChatFrame.Create(Self);
      Frame.Name := Frame.Name+IntToStr(chat.Id).Replace('-', '_');;
      Frame.Parent := DialogsStack;
      Frame.Fill(chat);
      if LoadDialogsData^.AddToTop then
        DialogsStack.ControlCollection.Move(DialogsStack.ControlCollection.Count-1, 0);
      case LoadDialogsData^.ScrollTo of
        stTop:
          DialogsScroll.VertScrollBar.Position := 0;
        stBottom:
          DialogsScroll.VertScrollBar.Position :=
            DialogsScroll.VertScrollBar.Range - DialogsScroll.VertScrollBar.Page;
        stNotScroll:
          if LoadDialogsData^.AddToTop then
            DialogsScroll.VertScrollBar.Position := ChatStack.Height - OldHeight
          else
            DialogsScroll.VertScrollBar.Position :=
              OldHeight - DialogsScroll.VertScrollBar.Page;
      end;
      //MainForm.StackPanel2.Height := MainForm.StackPanel2.Height+Frame.Height;
    end;
  end
  else
    DialogsPage -= 1;
  DialogsBlockLoad := False;
  Dispose(LoadDialogsData);
end;

{ Actions }


end.
