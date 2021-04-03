unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ComCtrls, ActnList, VkLongpoll, fpjson, Utils, CachedLongpoll,
  augvkapi, MessageFrameUnit;

type

  { TFrameManager }

  TFrameManager = class
  private
    FList: TObjectList;
    function GetCount: Integer;
    function CreateFrame(Msg: TMSG): TMessageFrame; abstract;
  public
    constructor Create;
    function Add(Msg: TMSG): Integer;
    function AddFront(Msg: TMSG): Integer;
    function Get(Idx: Integer): TMessageFrame;
    procedure Remove(Idx: Integer);
    procedure Remove(Frame: TMessageFrame);
    procedure Clear;
    property Count: Integer read GetCount;
  end;

  { TMsgFrameManager }


  { TChatFrameManager }

  { TMainForm }

  TMainForm = class(TForm)
    ScrollBox2: TScrollBox;
    SendAction: TAction;
    ActionList1: TActionList;
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
    procedure SendActionExecute(Sender: TObject);
  private

  public
    DrawnMsgsManager: TFrameManager;
  end;

var
  MainForm: TMainForm;
  LongpollThread: TCachedLongpoll;

implementation

var
  AugVK: TAugVKAPI;
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
    MainForm.DrawnMsgsManager.AddFront(
      LongpollThread.GetCache(Event.Integers[3], 1)[0]
      );

  MainForm.ScrollBox1.VertScrollBar.Position :=
    MainForm.ScrollBox1.VertScrollBar.Range - MainForm.ScrollBox1.VertScrollBar.Page;
end;

{ TFrameManager }

function TFrameManager.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TFrameManager.CreateFrame(Msg: TMSG): TMessageFrame;
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

constructor TFrameManager.Create;
begin
  FList := TObjectList.Create;
end;

function TFrameManager.Add(Msg: TMSG): Integer;
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

function TFrameManager.AddFront(Msg: TMSG): Integer;
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

function TFrameManager.Get(Idx: Integer): TMessageFrame;
begin
  Result := TMessageFrame(FList[Idx]);
end;

procedure TFrameManager.Remove(Idx: Integer);
begin
  if Idx < GetCount-1 then
  begin
    TMessageFrame(FList[Idx+1]).AnchorSideBottom.Control := TMessageFrame(FList[Idx]).AnchorSideBottom.Control;
    TMessageFrame(FList[Idx+1]).AnchorSideBottom.Side := TMessageFrame(FList[Idx]).AnchorSideBottom.Side;
  end;
  FList.Delete(Idx);
end;

procedure TFrameManager.Remove(Frame: TMessageFrame);
begin
  Remove(FList.IndexOf(Frame));
end;

procedure TFrameManager.Clear;
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

  DrawnMsgsManager := TFrameManager.Create;

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
//  ScrollBox1.VertScrollBar.Position :=
//    ScrollBox1.VertScrollBar.Range - ScrollBox1.VertScrollBar.Page;
//end;

{ Actions }

procedure TMainForm.SendActionExecute(Sender: TObject);
begin
  ShowMessage('sos');
end;

end.
