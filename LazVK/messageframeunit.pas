unit MessageFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics, Types, Math;

type

	{ TMessageFrame }

  TMessageFrame = class(TFrame)
		AvatarImage: TImage;
		Bevel1: TBevel;
		NameLabel: TLabel;
		MessageTextLabel: TLabel;
		procedure FrameMouseWheel(Sender: TObject; Shift: TShiftState;
			WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private

  public

  end;

implementation

{$R *.lfm}

uses MainFormUnit;

{ TMessageFrame }

procedure TMessageFrame.FrameMouseWheel(Sender: TObject; Shift: TShiftState;
	WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  MainForm.ScrollBox1.VertScrollBar.Position :=
    MainForm.ScrollBox1.VertScrollBar.Position + (-Sign(WheelDelta)*15)
end;

end.

