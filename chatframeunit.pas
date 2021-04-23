unit ChatFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, augvkapi, Design;

type

	{ TChatFrame }

  TChatFrame = class(TFrame)
    Bevel1: TBevel;
    ChatAvatarImage: TImage;
    TitleLabel: TLabel;
    LastMessageLabel: TLabel;
    procedure FrameClick(Sender: TObject);
    procedure FrameMouseEnter(Sender: TObject);
    procedure FrameMouseLeave(Sender: TObject);
  private

  public
    Id: Integer;
    procedure Fill(Chat: TChat);
    constructor Create(TheOwner: TComponent);
  end;

implementation

uses
  MainFormUnit, MessageFrameUnit;

{$R *.lfm}

{ TChatFrame }

procedure TChatFrame.FrameClick(Sender: TObject);
var
  msgs: TMSGsArray;
  i: integer;
  frame: TMessageFrame;
  Item : TControl;
begin
  if MainForm.SelectedChat = Id then exit;
  MainForm.SelectedChat := Id;
  // очистка чата
  while MainForm.FlowPanel1.ControlCount > 0 do
  begin
    Item := MainForm.FlowPanel1.Controls[0];
    Item.Free;
  end;
  mainform.FlowPanel1.Height:=0;
  //
  msgs := augvk.getHistory(id, 30);
  for i:=length(msgs)-1 downto 0 do
  begin
    Frame := TMessageFrame.Create(MainForm.FlowPanel1);
    Frame.Name := Frame.Name+IntToStr(msgs[i].Id);
    Frame.Fill(msgs[i]);
    Frame.Parent := MainForm.FlowPanel1;
    Frame.Width := MainForm.FlowPanel1.Width;
    MainForm.FlowPanel1.Height := MainForm.FlowPanel1.Height+Frame.Height;
  end;
end;

procedure TChatFrame.FrameMouseEnter(Sender: TObject);
begin
  Color := clSilver;
end;

procedure TChatFrame.FrameMouseLeave(Sender: TObject);
begin
  Color := DC_BACKGROUND;
end;

procedure TChatFrame.Fill(Chat: TChat);
begin
  ChatAvatarImage.Picture := Chat.Image;
  TitleLabel.Caption := Chat.name;
  LastMessageLabel.Caption := Chat.previewMsg.text;
  Id := Chat.Id;
end;

constructor TChatFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csOpaque];
end;

end.

