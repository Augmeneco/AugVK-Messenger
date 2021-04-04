unit MessageFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics, Types, Math,
  augvkapi;

type

	{ TMessageFrame }

  TMessageFrame = class(TFrame)
		AvatarImage: TImage;
		Bevel1: TBevel;
		NameLabel: TLabel;
		MessageTextLabel: TLabel;
   // procedure FrameMouseWheel(Sender: TObject; Shift: TShiftState;
			//WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private

  public
    MessageObject: TMSG;
    procedure Fill(Msg: TMSG);
  end;

implementation

{$R *.lfm}

uses MainFormUnit;

{ TMessageFrame }

//procedure TMessageFrame.FrameMouseWheel(Sender: TObject; Shift: TShiftState;
//	WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
//begin
//  MainForm.ChatScroll.VertScrollBar.Position :=
//    MainForm.ChatScroll.VertScrollBar.Position + (-Sign(WheelDelta)*15)
//end;

procedure TMessageFrame.Fill(Msg: TMSG);
begin
  AvatarImage.Picture := Msg.fromId.Image;
  NameLabel.Caption := Msg.fromId.Name;
  MessageTextLabel.Caption := Msg.Text;
  MessageObject := Msg;
end;

end.

