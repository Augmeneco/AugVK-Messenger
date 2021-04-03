unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ComCtrls, ActnList, VkLongpoll, fpjson, Utils, CachedLongpoll,
  augvkapi, MessageFrameUnit, Types, Math, Contnrs;

type

  { TMainForm }

  TMainForm = class(TForm)
    SendAction: TAction;
    ActionList1: TActionList;
    ListBox1: TListBox;
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
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SendActionExecute(Sender: TObject);
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
  private
    FList: TObjectList;
    function GetCount: Integer;
    function CreateFrame(Msg: TMSG): TMessageFrame;
  public
    constructor Create;
    function Add(Msg: TMSG): Integer;
    function AddFront(Msg: TMSG): Integer;
    function Get(Idx: Integer): TMessageFrame;
    procedure Remove(Idx: Integer);
    procedure Clear;
    property Count: Integer read GetCount;
  end;

var
  AugVK: TAugVKAPI;
  DrawnMsgsManager: TDrawnMsgsManager;
  SelectedChat: integer;

{$R *.lfm}

procedure NewMessageHandler(Event: TJSONArray);
var
  Chat: TChat;
begin
  augvk.updateChatsPosition(Event.Integers[3]);
  MainForm.ListBox1.Items.Clear;

  for chat in augvk.getChatsForDraw do
    MainForm.ListBox1.Items.Add(chat.Name);

  if (SelectedChat = -1) then
    Exit;

  if SelectedChat = Event.Integers[3] then
    DrawnMsgsManager.AddFront(
      LongpollThread.GetCache(Event.Integers[3], 1)[0]
      );

  MainForm.ScrollBox1.VertScrollBar.Position :=
    MainForm.ScrollBox1.VertScrollBar.Range - MainForm.ScrollBox1.VertScrollBar.Page;
end;

{ TDrawnMsgsManager }

function TDrawnMsgsManager.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDrawnMsgsManager.CreateFrame(Msg: TMSG): TMessageFrame;
begin
  Result := TMessageFrame.Create(MainForm);
  Result.Fill(Msg);
  Result.Name := Result.Name + IntToStr(GetCount);
  Result.Anchors := [akBottom, akLeft, akRight];
  Result.AnchorSide[akLeft].Control := MainForm.ScrollBox1;
  Result.AnchorSide[akLeft].Side := asrLeft;
  Result.AnchorSide[akRight].Control := MainForm.ScrollBox1;
  Result.AnchorSide[akRight].Side := asrRight;
end;

constructor TDrawnMsgsManager.Create;
begin
  FList := TObjectList.Create;
end;

function TDrawnMsgsManager.Add(Msg: TMSG): Integer;
var
  Frame: TMessageFrame;
begin
  Frame := CreateFrame(Msg);

  if GetCount = 0 then
  begin
    Frame.AnchorSide[akBottom].Control := MainForm.ScrollBox1;
    Frame.AnchorSide[akBottom].Side := asrBottom;
  end
  else
  begin
    Frame.AnchorSide[akBottom].Control := Get(GetCount - 1);
    Frame.AnchorSide[akBottom].Side := asrTop;
  end;
  Frame.Parent := MainForm.ScrollBox1;

  Result := Flist.Add(Frame);
end;

function TDrawnMsgsManager.AddFront(Msg: TMSG): Integer;
var
  Frame: TMessageFrame;
begin
  Frame := CreateFrame(Msg);

  Frame.Visible := False;

  Frame.Parent := MainForm.ScrollBox1;
  if GetCount > 0 then
  begin
    Get(0).AnchorSide[akBottom].Control := Frame;
    Get(0).AnchorSide[akBottom].Side := asrTop;
  end;
  Frame.AnchorSide[akBottom].Control := MainForm.ScrollBox1;
  Frame.AnchorSide[akBottom].Side := asrBottom;

  Frame.Visible := True;

  FList.Insert(0, Frame);
  Result := 0;
end;

function TDrawnMsgsManager.Get(Idx: Integer): TMessageFrame;
begin
  Result := TMessageFrame(FList[Idx]);
end;

procedure TDrawnMsgsManager.Remove(Idx: Integer);
begin
  //TMessageFrame(FList[Idx]).Parent := nil;
  //TMessageFrame(FList[Idx]).Free;
  //FList.Delete(Idx);
end;

procedure TDrawnMsgsManager.Clear;
begin
  FList.Clear;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  Token: string;
begin
  Token := Config.GetPath(Format('accounts[%d].token',
    [Config.Integers['active_account']])).AsString;
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
  if ListBox1.ItemIndex = -1 then
    Exit;

  Chat := AugVK.GetChatByIndex(ListBox1.ItemIndex);
  SelectedChat := Chat.Id;

  DrawnMsgsManager.Clear;
  for msg in augvk.getHistory(chat.id, 30) do
  begin
    DrawnMsgsManager.Add(msg);
  end;
  ScrollBox1.VertScrollBar.Position :=
    ScrollBox1.VertScrollBar.Range - ScrollBox1.VertScrollBar.Page;
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

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  AugVK.SendMessage(Memo1.Text, SelectedChat);
end;

{ Actions }

procedure TMainForm.SendActionExecute(Sender: TObject);
begin

end;

end.
