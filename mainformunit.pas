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

type

  { TDrawnMsgsManager }

  TDrawnMsgsManager = class
  public
    type
      TItem = class
        Prev: TItem;
        Next: TItem;
        ApiMsg: TMSG;
        GuiMsg: TMessageFrame;
      end;
  private
    FirstItem: TItem;
  FCount: Integer;
  public
  function Add(Item: TItem): Integer;
  function AddMsg(Msg: TMSG): Integer;
    function Get(Idx: Integer): TItem;
  property Count: Integer read FCount;
  end;

var
  AugVK: TAugVKAPI;
  DrawnMsgsManager: TDrawnMsgsManager;

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

{ TDrawnMsgsManager }

function TDrawnMsgsManager.Add(Item: TItem): Integer;
var
  ItemIter: TItem;
  i: Integer;
begin
  if Assigned(FirstItem) then
  begin
    ItemIter := FirstItem;
    i:=0;
    while Assigned(ItemIter.Next) do
    begin
      ItemIter := ItemIter.Next;
      Inc(i);
		end;
		ItemIter.Next := Item;
    Item.Prev := ItemIter;
    FCount += 1;
    Result := i;
	end
  else
  begin
    FirstItem := Item;
    FCount := 1;
    Result := 0;
	end;
end;

function TDrawnMsgsManager.AddMsg(Msg: TMSG): Integer;
var
  Item: TItem;
  MsgFrame: TMessageFrame;
begin
  // создание titem
  Item := TItem.Create;
  // создание tmessageframe
  MsgFrame := TMessageFrame.Create(nil);

  // сохранение tmsg и tmessageframe в titem
  Item.GuiMsg := MsgFrame;
  Item.ApiMsg := Msg;

  // помещение tmessageframe на форму за последним фреймом из titem в этом списке
  MsgFrame.Name := MsgFrame.Name+IntToStr(FCount);
  MsgFrame.Anchors := [akBottom, akLeft, akRight];

  MsgFrame.AnchorSide[akLeft].Control := MainForm.ScrollBox1;
  MsgFrame.AnchorSide[akLeft].Side := asrLeft;
  MsgFrame.AnchorSide[akRight].Control := MainForm.ScrollBox1;
  MsgFrame.AnchorSide[akRight].Side := asrRight;

  MsgFrame.AvatarImage.Picture.LoadFromFile('logo.png');
  MsgFrame.NameLabel.Caption := msg.fromId.name;
  MsgFrame.MessageTextLabel.Caption := msg.text;

  if not Assigned(FirstItem) then
  begin
    MsgFrame.AnchorSide[akBottom].Control := MainForm.ScrollBox1;
    MsgFrame.AnchorSide[akBottom].Side := asrBottom;
	end
  else
  begin
    MsgFrame.AnchorSide[akBottom].Control := Get(FCount-1).GuiMsg;
    MsgFrame.AnchorSide[akBottom].Side := asrTop;
	end;
	MsgFrame.Parent := MainForm.ScrollBox1;

  Result := Add(Item);
end;

function TDrawnMsgsManager.Get(Idx: Integer): TItem;
begin
  if Assigned(FirstItem) or (Idx <= (FCount-1)) then
  begin
    Result := FirstItem;
    while Assigned(Result.Next) do
      Result := Result.Next;
 	end
  else
    raise Exception.Create('No such index');
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  Token: String;
begin
  Token := Config.GetPath(Format('accounts[%d].token', [Config.Integers['active_account']])).AsString;
  AugVK := TAugVKAPI.Create(Token);

  DrawnMsgsManager := TDrawnMsgsManager.Create;

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
begin
  if ListBox1.ItemIndex = -1 then Exit;

  MainForm.ListBox2.Items.Clear;
  Chat := AugVK.GetChatByIndex(ListBox1.ItemIndex);

  for msg in augvk.getHistory(chat.id,30) do
  begin
    DrawnMsgsManager.AddMsg(msg);
	end;
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Range-ScrollBox1.VertScrollBar.Page;
end;

procedure TMainForm.ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
	WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position + Sign(WheelDelta)
end;

end.

