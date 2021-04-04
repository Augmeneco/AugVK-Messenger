unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ComCtrls, ActnList, VkLongpoll, fpjson, Utils, CachedLongpoll,
  augvkapi, MessageFrameUnit, ChatFrameUnit, Contnrs;

type

  { TFrameManager }

  TFrameManager = class
  protected
    FList: TObjectList;
    FParent: TWinControl;
    FSide: TAnchorKind; //akbottom
    FRefSide: TAnchorSideReference; //asrtop
    function GetCount: Integer;
    procedure PrepareFrame(Frame: TFrame);
  public
    constructor Create;
    function Add(Frame: TFrame): Integer;
    function AddFront(Frame: TFrame): Integer;
    function Get(Idx: Integer): TFrame;
    procedure Remove(Idx: Integer);
    procedure Remove(Frame: TFrame);
    procedure Clear;
    property Count: Integer read GetCount;
  end;

  { TMsgFrameManager }

  TMsgFrameManager = class (TFrameManager)
  public
    constructor Create;
    function CreateFrame(Msg: TMSG): TMessageFrame;
    function Add(Msg: TMSG): Integer;
    function AddFront(Msg: TMSG): Integer;
  end;

  { TChatFrameManager }

  TChatFrameManager = class (TFrameManager)
  public
    constructor Create;
    function CreateFrame(Chat: TChat): TChatFrame;
    function Add(Chat: TChat): Integer;
    function AddFront(Chat: TChat): Integer;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    ChatListScroll: TScrollBox;
    SendAction: TAction;
    ActionList1: TActionList;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    ChatScroll: TScrollBox;
    SpeedButton1: TSpeedButton;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SendActionExecute(Sender: TObject);
  private

  public
    MessagesManager: TMsgFrameManager;
    ChatListManager: TChatFrameManager;
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
  Chat: TChat;
begin
  augvk.updateChatsPosition(Event.Integers[3]);
  MainForm.ChatListManager.Clear;

  for Chat in augvk.getChatsForDraw do
    MainForm.ChatListManager.Add(Chat);

  if (MainForm.SelectedChat = -1) then
    Exit;

  if MainForm.SelectedChat = Event.Integers[3] then
    MainForm.MessagesManager.AddFront(
      LongpollThread.GetCache(Event.Integers[3], 1)[0]
      );

  MainForm.ChatScroll.VertScrollBar.Position :=
    MainForm.ChatScroll.VertScrollBar.Range - MainForm.ChatScroll.VertScrollBar.Page;
end;

{ TFrameManager }

function TFrameManager.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TFrameManager.PrepareFrame(Frame: TFrame);
begin
  Frame.Name := Frame.Name + IntToStr(GetCount);
  Frame.Anchors := [FSide, akLeft, akRight];
  Frame.AnchorSide[akLeft].Control := FParent;
  Frame.AnchorSide[akLeft].Side := asrLeft;
  Frame.AnchorSide[akRight].Control := FParent;
  Frame.AnchorSide[akRight].Side := asrRight;
end;

constructor TFrameManager.Create;
begin
  FList := TObjectList.Create;
end;

function TFrameManager.Add(Frame: TFrame): Integer;
begin
  PrepareFrame(Frame);

  if GetCount = 0 then
  begin
    Frame.AnchorSide[FSide].Control := FParent;
    if FRefSide = asrTop then
      Frame.AnchorSide[FSide].Side := asrBottom //asrbottom
    else
      Frame.AnchorSide[FSide].Side := asrTop;
  end
  else
  begin
    Frame.AnchorSide[FSide].Control := Get(GetCount - 1);
    Frame.AnchorSide[FSide].Side := FRefSide;//asrtop
  end;
  Frame.Parent := FParent;

  Result := Flist.Add(Frame);
end;

function TFrameManager.AddFront(Frame: TFrame): Integer;
begin
  PrepareFrame(Frame);

  Frame.Visible := False;

  Frame.Parent := FParent;
  if GetCount > 0 then
  begin
    Get(0).AnchorSide[FSide].Control := Frame;
    Get(0).AnchorSide[FSide].Side := FRefSide;//asrTop;
  end;
  Frame.AnchorSide[FSide].Control := FParent;
  if FRefSide = asrTop then
    Frame.AnchorSide[FSide].Side := asrBottom //asrbottom
  else
    Frame.AnchorSide[FSide].Side := asrTop;

  Frame.Visible := True;

  FList.Insert(0, Frame);
  Result := 0;
end;

function TFrameManager.Get(Idx: Integer): TFrame;
begin
  Result := TMessageFrame(FList[Idx]);
end;

procedure TFrameManager.Remove(Idx: Integer);
begin
  if Idx < GetCount-1 then
  begin
    TMessageFrame(FList[Idx+1]).AnchorSide[FSide].Control := TMessageFrame(FList[Idx]).AnchorSide[FSide].Control;
    TMessageFrame(FList[Idx+1]).AnchorSide[FSide].Side := TMessageFrame(FList[Idx]).AnchorSide[FSide].Side;
  end;
  FList.Delete(Idx);
end;

procedure TFrameManager.Remove(Frame: TFrame);
begin
  Remove(FList.IndexOf(Frame));
end;

procedure TFrameManager.Clear;
begin
  FList.Clear;
end;

{ TMsgFrameManager }

constructor TMsgFrameManager.Create;
begin
  inherited Create;
  FParent := MainForm.ChatScroll;
  FSide := akBottom;
  FRefSide := asrTop;
end;

function TMsgFrameManager.CreateFrame(Msg: TMSG): TMessageFrame;
begin
  Result := TMessageFrame.Create(FParent);
  Result.Fill(Msg);
end;

function TMsgFrameManager.Add(Msg: TMSG): Integer;
begin
  Result := inherited Add(CreateFrame(Msg));
end;

function TMsgFrameManager.AddFront(Msg: TMSG): Integer;
begin
  Result := inherited AddFront(CreateFrame(Msg));
end;

{ TChatFrameManager }

constructor TChatFrameManager.Create;
begin
  inherited Create;
  FParent := MainForm.ChatListScroll;
  FSide := akTop;
  FRefSide := asrBottom;
end;

function TChatFrameManager.CreateFrame(Chat: TChat): TChatFrame;
begin
  Result := TChatFrame.Create(FParent);
  Result.Fill(Chat);
end;

function TChatFrameManager.Add(Chat: TChat): Integer;
begin
  Result := inherited Add(CreateFrame(Chat));
end;

function TChatFrameManager.AddFront(Chat: TChat): Integer;
begin
  Result := inherited AddFront(CreateFrame(Chat));
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  Token: string;
begin
  Token := Config.GetPath(Format('accounts[%d].token',
    [Config.Integers['active_account']])).AsString;
  AugVK := TAugVKAPI.Create(Token);

  MessagesManager := TMsgFrameManager.Create;
  ChatListManager := TChatFrameManager.Create;

  LongpollThread := TCachedLongpoll.Create(Token);
  LongpollThread.RegisterEventHandler(4, @NewMessageHandler);

  LongpollThread.Start;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  Chat: TChat;
begin
  for Chat in AugVK.GetChatsForDraw do
    MainForm.ChatListManager.Add(Chat);
end;

procedure TMainForm.ListBox1Click(Sender: TObject);
begin

end;

procedure TMainForm.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
  begin
     AugVK.SendMessage(Memo1.Text, SelectedChat);
     Memo1.Lines.Clear;
  end;
end;

//procedure TMainForm.ListBox1Click(Sender: TObject);
//var
//  chat: TChat;
//  msg: TMSG;
//begin
//  if ListBox1.ItemIndex = -1 then
//    Exit;
//
//  Chat := AugVK.GetChatByIndex(ListBox1.ItemIndex);
//  SelectedChat := Chat.Id;
//
//  DrawnMsgsManager.Clear;
//  for msg in augvk.getHistory(chat.id, 30) do
//  begin
//    DrawnMsgsManager.Add(msg);
//  end;
//  ChatScroll.VertScrollBar.Position :=
//    ChatScroll.VertScrollBar.Range - ChatScroll.VertScrollBar.Page;
//end;

{ Actions }

procedure TMainForm.SendActionExecute(Sender: TObject);
begin
  ShowMessage('sos');
end;

end.
