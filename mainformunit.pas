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
    function Add(Msg: TMSG): Integer;
	  function Get(Idx: Integer): TItem;
    procedure Remove(Idx: Integer);
    procedure Clear;
    property Count: Integer read FCount;
	end;

var
  AugVK: TAugVKAPI;
  DrawnMsgsManager: TDrawnMsgsManager;
  SelectedChat: Integer;

{$R *.lfm}

procedure NewMessageHandler(Event: TJSONArray);
var
  Chat: TChat;
begin
  augvk.updateChatsPosition(Event.Integers[3]);
  MainForm.ListBox1.Items.Clear;

  for chat in augvk.getChatsForDraw do
    MainForm.ListBox1.Items.Add(chat.name);

  if (SelectedChat = -1) then Exit;

  if SelectedChat = Event.Integers[3] then
    DrawnMsgsManager.Add(
      LongpollThread.GetCache(Event.Integers[3],1)[0]
    );
end;

{ TDrawnMsgsManager }

{ !!!!!!!!!!!!!
  ВООБЩЕ НАФИГ ПЕРЕДЕЛАТЬ ОСНОВАВ СПИСОК НА ОСНОВЕ ФРЕЙМА
  !!!!!!!!!!!!! }

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

function TDrawnMsgsManager.Add(Msg: TMSG): Integer;
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

  //MsgFrame.AvatarImage.Picture.LoadFromFile('logo.png');
  MsgFrame.AvatarImage.Picture := msg.fromId.Image;
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
var
  i: Integer;
begin
  if Assigned(FirstItem) or (Idx <= (FCount-1)) then
  begin
    Result := FirstItem;
    i:=0;
    while i<Idx do
    begin
      Result := Result.Next;
      Inc(i);
		end;
	end
  else
    raise Exception.Create('No such index');
end;

procedure TDrawnMsgsManager.Remove(Idx: Integer);
var
  Item: TItem;
begin
  Item := Get(Idx);
  // тут отсчет идет относительно Item

  // если прошлый итем установлен то
  if Assigned(Item.Prev) then
  begin
    // заменить прошлому Next на Next этого итема
    Item.Prev.Next := Item.Next;
	end
	else
  begin
  // если прошлого итема нет значит это первый итем
  // значит производим с ним отдельные операции
    // значит меняем значение родительского класса на след. за этим итем
    FirstItem := Item.Next;
    // если след. за этим итем не нил - значит привязываем его к началу бокса
    if Assigned(Item.Next) then
      Item.Next.GuiMsg.AnchorSide[akBottom].Control := MainForm.ScrollBox1;
	end;

  // если след. итем установлен
  if Assigned(Item.Next) then
  begin
    // заменить след. итему Prev на Prev этого итема
    Item.Next.Prev := Item.Prev;
	end;

  // если след. итем не нил и пред. не нил
  // то у след. итема меняем привязку на пред. итем
  if Assigned(Item.Prev) then
    Item.Next.GuiMsg.AnchorSide[akBottom].Control := Item.Prev.GuiMsg;

  Dec(FCount);
end;

procedure TDrawnMsgsManager.Clear;
var
  i: Integer;
begin
  if FCount > 0 then
	  for i:=0 to FCount-1 do
	    Remove(0);
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

  Chat := AugVK.GetChatByIndex(ListBox1.ItemIndex);
  SelectedChat := Chat.Id;

  DrawnMsgsManager.Clear;
  for msg in augvk.getHistory(chat.id, 30) do
  begin
    DrawnMsgsManager.Add(msg);
	end;
  ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Range-ScrollBox1.VertScrollBar.Page;
end;

end.

