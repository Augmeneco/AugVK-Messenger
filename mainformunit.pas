unit MainFormUnit;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ComCtrls, ActnList, Menus, VkLongpoll, fpjson, Utils,
  CachedLongpoll, augvkapi, MessageFrameUnit, ChatFrameUnit, StackPanel,
  AugScrollBox, BCSVGButton, BCListBox, atshapelinebgra, BCButton, fgl, Types,
  AugVKApiThread, ConfigUtils, LoginFrameUnit, Math, AugImage, Contnrs;

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
    AttachmentsFlow: TFlowPanel;
    AttachmentsPanel: TPanel;
    BCButton1: TBCButton;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Panel2: TPanel;
    ShadowPanel: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    ReturnButton: TBCSVGButton;
    ActionList1: TActionList;
    AttachButton: TBCSVGButton;
    DialogsScroll: TAugScrollBox;
    ChatScroll: TAugScrollBox;
    LoginFrameForm: TLoginFrame;
    DialogsPanel: TPanel;
    ChatPanel: TPanel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    Shape1: TShape;
    ShapeLineBGRA1: TShapeLineBGRA;
    ShapeLineBGRA2: TShapeLineBGRA;
    ShapeLineBGRA3: TShapeLineBGRA;
    ShowMenuItem: TMenuItem;
    CloseMenuItem: TMenuItem;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    PopupMenu1: TPopupMenu;
    SendButton: TBCSVGButton;
    Splitter1: TSplitter;
    DialogsStack: TStackPanel;
    ChatStack: TStackPanel;
    TrayIcon1: TTrayIcon;
    procedure AttachButtonMouseEnter(Sender: TObject);
    procedure AttachButtonMouseLeave(Sender: TObject);
    procedure BCButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ReturnButtonClick(Sender: TObject);
    procedure DialogsScrollVScroll(Sender: TObject; var ScrollPos: Integer);
    procedure ChatScrollVScroll(Sender: TObject; var ScrollPos: Integer);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure CustomExceptionHandler(Sender: TObject; E: Exception);
    procedure FormResize(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ShadowPanelClick(Sender: TObject);
    procedure ShowMenuItemClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure TrayIcon1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AttachedPhotoClick(Sender: TObject);
    procedure DetachButtonClick(Sender: TObject);
    procedure DetachButtonMouseEnter(Sender: TObject);
    procedure DetachButtonMouseLeave(Sender: TObject);

    procedure SetDialogsWidthPrcnt(AWidth: Real);
    function GetDialogsWidthPrcnt: Real;
  private
    DialogsOffset: Integer;
    ChatPage: Integer;
    DialogsPage: Integer;
    ChatBlockLoad: Boolean;
    DialogsBlockLoad: Boolean;
    ActiveDialogFrame: TFrame;

    procedure OnLogined(Token: String; ExpiresIn, Id: Integer);
    procedure AfterLogin(Token: String);
  public
    SelectedChat: Integer;
    ActiveUser: TUser;
    CompactView: Boolean;
    AttachedFiles: TStringList;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ShowChat(AShow: Boolean);
    procedure ShowBothPanels;
    procedure ShowOnlyChat;
    procedure ShowOnlyDialogs;

    procedure ShowDialogWindow(Frame: TFrame; Title: String; Buttons: array of const; Callback: TNotifyEvent);
    procedure HideDialogWindow;
    procedure FreeDialogWindow;

    procedure ClearChat;
    procedure LoadChat(Id: Integer; Page: Integer=0; StartMsg: Integer=-1;
      ToTop: Boolean=True; ScrollTo: TScrollTo=stNotScroll);
    procedure LoadChatCallback(Response: TObject; Data: Pointer);
    procedure OpenChat(Id: Integer; LastMessage: augvkapi.TMSG=nil);
    procedure CloseChat;

    procedure LoadDialogs(Page: Integer=0; ToTop: Boolean=False;
      ScrollTo: TScrollTo=stNotScroll);
    procedure LoadDialogsCallback(Response: TObject; Data: Pointer);

    procedure AttachPhoto(Filename: String);
    procedure OnPasteFromClipboard;


    property DialogsWidthPrcnt: Real read GetDialogsWidthPrcnt write SetDialogsWidthPrcnt;
  end;

var
  MainForm: TMainForm;
  LongpollThread: TCachedLongpoll;
  AugVK: TAugVKAPI;

implementation

uses
  LazLogger, URIParser, LCLIntf, LCLType, fpmimetypes, PhotoAttachFrameUnit,
  Clipbrd, MediaViewerFormUnit;

{$R *.lfm}

procedure NewMessageHandler(Event: TJSONArray);
var
  Idx: Integer;
  Found: Boolean = False;
  Frame: TMessageFrame;
  OnBottom: Boolean = False;
  LastMessage: augvkapi.TMSG;
begin
  LastMessage := LongpollThread.GetCache(Event.Integers[3], 1)[0];
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

  TChatFrame(MainForm.DialogsStack.ControlCollection[Idx].Control).LastMessageLabel.Caption := TChatFrame.GeneratePreviewText(LastMessage);
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
    Frame.Fill(LastMessage);
    Frame.Parent := MainForm.ChatStack;
  end;

  if OnBottom then
    MainForm.ChatScroll.VertScrollBar.Position :=
      MainForm.ChatScroll.VertScrollBar.Range - MainForm.ChatScroll.VertScrollBar.Page;
end;

{ TMainForm }

procedure GetToken;
var
  ModalResult: TModalResult;
  URL: String;
  BookmarkParsed: TStringList;
begin
  ModalResult := QuestionDlg('Авторизация',
    'Вы будете перенаправлены в браузер для авторизации.'+LineEnding+
    'Скопируйте URL в след диалог после успешной авторизации',
    mtCustom,
    [mrYes, 'Открыть', mrNo, 'Выйти', 'IsDefault'], '');
  if (ModalResult = mrNo) or (ModalResult = mrCancel) then
    Halt;

  OpenURL('https://oauth.vk.com/authorize?client_id=7950449&scope=69662&redirect_uri=https://oauth.vk.com/blank.html&display=mobile&response_type=token&revoke=1');

  if not InputQuery('Авторизация', 'Введите токен', URL) then
    Halt;

  BookmarkParsed := TStringList.Create;
  BookmarkParsed.Delimiter := '&';
  BookmarkParsed.StrictDelimiter := True;
  BookmarkParsed.DelimitedText := ParseURI(Url).Bookmark;

  MainForm.OnLogined(BookmarkParsed.Values['access_token'],
      BookmarkParsed.Values['expires_in'].ToInteger,
      BookmarkParsed.Values['user_id'].ToInteger);
  BookmarkParsed.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  TokenPath: String;
begin
  AttachedFiles := TStringList.Create;
	Application.OnException := @CustomExceptionHandler;
  //CefLoginFrame1.OnLogined := @OnLogined;

  ShowChat(False);

  // Получение токена
  TokenPath := Format('accounts[%d].token', [Config.Integers['active_account']]);
  if Config.FindPath(TokenPath) = nil then
  begin
    //CefLoginFrame1.OpenLoginPage;
    GetToken;
  end
  else
    AfterLogin(Config.GetPath(TokenPath).AsString);

  DialogsWidthPrcnt := Config.Floats['dialogs_width'];

  Memo1.Height := Memo1.Font.Size+6;
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

procedure TMainForm.Memo1Change(Sender: TObject);
begin
  if Memo1.Lines.Count <= 12 then
    Memo1.Height := (Memo1.Font.Size+6)*IfThen(Memo1.Lines.Count=0, 1, Memo1.Lines.Count);
  //Memo1.Height:=1+Memo1.Lines.Count*Abs(Memo1.Font.Height*Screen.PixelsPerInch div 72); // I do this anywhere after adding the text and/or after editing it
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  MainForm.Hide;
  CanClose:=False;
end;

procedure TMainForm.ReturnButtonClick(Sender: TObject);
begin
  ShowOnlyDialogs;
end;

procedure TMainForm.FormHide(Sender: TObject);
begin
  //if Assigned(Shadow) then Shadow.Hide;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  R: TRect;
begin
  //ShadowPanel.BevelOuter := bvNone;
  //R := Panel1.ClientRect;
  //image1.Canvas.CopyRect(R, Panel1.Canvas, R);
  //image1.Canvas.Line(500,100,100,500);
  //ShadowPanel.BringToFront;
  //Invalidate;
  ShadowPanel.ControlStyle := ShadowPanel.ControlStyle - [csOpaque] + [csParentBackground];
end;

procedure TMainForm.AttachButtonMouseEnter(Sender: TObject);
var
  FormPoint: TPoint;
begin
  FormPoint := ScreenToClient(AttachButton.ClientToScreen(Point(0,0)));
  Panel8.Left := FormPoint.X-1;
  Panel8.Top := FormPoint.Y-Panel8.Height;
  Panel8.BringToFront;
  Panel8.Show;
end;

procedure TMainForm.AttachButtonMouseLeave(Sender: TObject);
begin
  Panel8.SendToBack;
  Panel8.Hide;
end;

procedure TMainForm.BCButton1Click(Sender: TObject);
var
  Frame: TPhotoAttachFrame;
begin
  Frame := TPhotoAttachFrame.Create(nil);
  ShowDialogWindow(Frame, 'Фотография', ['Отмена', 0], @Frame.DialogButtonClick);
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
    //  MousePoint := ScreenToClient(Mouse.CursorPos);
    //Control := ControlAtPos(MousePoint, [capfRecursive, capfAllowWinControls]);
    //
    //{capfAllowDisabled,   // include controls with Enabled=false
    // capfAllowWinControls,// include TWinControls
    // capfOnlyClientAreas, // use the client areas, not the whole child area
    // capfRecursive,       // search recursively in grand childrens
    // capfHasScrollOffset, // do not add the scroll offset to Pos (already included)
    // capfOnlyWinControls  // include only TWinControls (ignore TControls) }

end;

procedure TMainForm.FormShow(Sender: TObject);
begin
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
  if (Key = VK_V) and (ssCtrl in Shift) then
    OnPasteFromClipboard;
end;

procedure TMainForm.ShadowPanelClick(Sender: TObject);
begin
  HideDialogWindow;
end;

procedure TMainForm.ShowMenuItemClick(Sender: TObject);
begin
  Show;
end;

procedure TMainForm.CloseMenuItemClick(Sender: TObject);
begin
  Halt;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  TrayIcon1.Hide;
end;

procedure TMainForm.SendButtonClick(Sender: TObject);
var
  AttachIDs: TStringArray;
  AttachID: String;
  Item: TControl;
  i: Integer;
begin
  if SelectedChat <> -1 then
  begin
    if AttachedFiles.Count > 0 then
    begin
      for i:=0 to AttachedFiles.Count-1 do
      begin
        AttachID := AugVK.UploadPhoto(AttachedFiles.ValueFromIndex[i]);
        Insert(AttachID, AttachIDs, Length(AttachIDs));
      end;
    end;
    AugVK.SendMessage(Memo1.Text, SelectedChat, AttachIDs);
    Memo1.Clear;
    while AttachmentsFlow.ControlCount > 0 do
    begin
      Item := AttachmentsFlow.Controls[0];
      Item.Free;
    end;
    AttachedFiles.Clear;
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

procedure TMainForm.AttachedPhotoClick(Sender: TObject);
begin
  MediaViewerForm.AugImage1.Picture.LoadFromFile(AttachedFiles.Values[IntToStr(TControl(Sender).Tag)]);
  MediaViewerForm.Show;
end;

procedure TMainForm.DetachButtonClick(Sender: TObject);
begin
  AttachedFiles.Delete(TComponent(Sender).Tag);
  Application.ReleaseComponent(TComponent(Sender).Owner);
end;

procedure TMainForm.DetachButtonMouseEnter(Sender: TObject);
begin
  TBCSVGButton(Sender).ColorOpacity := 210;
end;

procedure TMainForm.DetachButtonMouseLeave(Sender: TObject);
begin
  TBCSVGButton(Sender).ColorOpacity := 127;
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

  //CefLoginFrame1.Hide;

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

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TMainForm.Destroy;
begin
  inherited Destroy;
end;

procedure TMainForm.ShowChat(AShow: Boolean);
begin
  Panel4.Visible:=AShow;
  ShapeLineBGRA1.Visible:=AShow;
  ShapeLineBGRA3.Visible:=AShow;
  Panel5.Visible:=AShow;
  AttachmentsPanel.Visible:=AShow;
  Panel2.Visible:=not AShow;
end;

procedure TMainForm.ShowBothPanels;
begin
  DialogsPanel.Show;
  ChatPanel.Show;
  ReturnButton.Hide;
  DialogsPanel.Align := alLeft;
  Splitter1.Show;
  DialogsWidthPrcnt := Config.Floats['dialogs_width'];
  CompactView := False;
end;

procedure TMainForm.ShowOnlyChat;
begin
  DialogsPanel.Hide;
  ChatPanel.Show;
  ReturnButton.Show;
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

procedure TMainForm.ShowDialogWindow(Frame: TFrame; Title: String; Buttons: array of const; Callback: TNotifyEvent);
var
  Button: TButton;
  i: Integer = 0;
begin
  if (Length(Buttons) mod 2) = 0 then
  begin
    //error
  end;
  if Assigned(ActiveDialogFrame) then
    FreeDialogWindow;
  while i <= High(Buttons) do
  begin
    if (Buttons[i].VType <> vtString) and (Buttons[i+1].VType <> vtInteger) then
    begin
      //error
    end;
    Button := TButton.Create(Self);
    Button.Caption := String(Buttons[i].VString);
    Button.Tag := Buttons[i+1].VInteger;
    Button.OnClick := Callback;
    Button.Parent := Panel7;
    Button.Align := alRight;
    i += 2;
  end;
  ActiveDialogFrame := Frame;
  ActiveDialogFrame.Parent := Panel6;
  Label1.Caption := Title;
  ShadowPanel.BringToFront;
end;

procedure TMainForm.HideDialogWindow;
begin
  ShadowPanel.SendToBack;
end;

procedure TMainForm.FreeDialogWindow;
var
  Item: TControl;
begin
  if Assigned(ActiveDialogFrame) then
  begin
    while Panel7.ControlCount > 0 do
    begin
      Item := Panel7.Controls[0];
      Item.Free;
    end;
    ActiveDialogFrame.Free;
  end;
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
  msg: augvkapi.TMSG;
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

procedure TMainForm.OpenChat(Id: Integer; LastMessage: augvkapi.TMSG=nil);
var
  Frame: TMessageFrame;
  MsgId: Integer = -1;
  ChatInfo: TChat;
begin
  if MainForm.CompactView then
    MainForm.ShowOnlyChat;
  if MainForm.SelectedChat = Id then
    exit;
  MainForm.SelectedChat := Id;

  ShowChat(True);

  ChatInfo := AugVK.GetChat(Id);
  Label2.Caption := ChatInfo.Name;
  Image2.Picture := ChatInfo.Image;

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

procedure TMainForm.AttachPhoto(Filename: String);
var
  Panel: TPanel;
  Image: TAugImage;
  DetachButton: TBCSVGButton;
begin
  AttachedFiles.AddPair(IntToStr(AttachedFiles.Count), Filename);

  Panel := TPanel.Create(Self);
  Panel.Parent := AttachmentsFlow;
  Panel.Height := 60;
  Panel.Width := 60;
  Panel.BevelOuter := bvNone;
  Panel.Tag := AttachedFiles.Count-1;

  Image := TAugImage.Create(Panel);
  Image.Parent := Panel;
  Image.Align := alClient;
  Image.Picture.LoadFromFile(Filename);
  Image.OnClick := @AttachedPhotoClick;
  Image.Cover := True;
  Image.Center := True;
  Image.Cursor := crHandPoint;
  Image.AntialiasingMode := amOn;

  DetachButton := TBCSVGButton.Create(Panel);
  DetachButton.Parent := Panel;
  DetachButton.SVGNormalXML.Text := '<?xml version="1.0" encoding="UTF-8"?><svg width="1em" height="1em" aria-hidden="true" role="img" viewBox="0 0 16 16" xmlns="http://www.w3.org/2000/svg"> <g fill="none"> <path d="M2.397 2.554l.073-.084a.75.75 0 0 1 .976-.073l.084.073L8 6.939l4.47-4.47a.75.75 0 1 1 1.06 1.061L9.061 8l4.47 4.47a.75.75 0 0 1 .072.976l-.073.084a.75.75 0 0 1-.976.073l-.084-.073L8 9.061l-4.47 4.47a.75.75 0 0 1-1.06-1.061L6.939 8l-4.47-4.47a.75.75 0 0 1-.072-.976l.073-.084l-.073.084z" fill="#fff"/> </g> </svg>';
  DetachButton.Color := clGray;
  DetachButton.ColorOpacity := 127;
  DetachButton.Cursor := crHandPoint;
  DetachButton.OnClick := @DetachButtonClick;
  DetachButton.OnMouseEnter := @DetachButtonMouseEnter;
  DetachButton.OnMouseLeave := @DetachButtonMouseLeave;
  DetachButton.AnchorSideRight.Control := Panel;
  DetachButton.AnchorSideRight.Side := asrRight;
  DetachButton.AnchorSideTop.Control := Panel;
  DetachButton.AnchorSideTop.Side := asrTop;
  DetachButton.Height := 15;
  DetachButton.Width := 15;
end;

procedure TMainForm.OnPasteFromClipboard;
var
  Bitmap: TBitmap;
  TempFile: String;
begin
  Bitmap := TBitmap.Create;
  if Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) then
  begin
    Bitmap.LoadFromClipboardFormat(CF_Bitmap);
    TempFile := GetTempFileName+'.bmp';
    Bitmap.SaveToFile(TempFile);
    AttachPhoto(TempFile);
  end;
  Bitmap.Free;
end;

{ Actions }

end.
