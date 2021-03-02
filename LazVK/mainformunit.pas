unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
	StdCtrls, IpHtml, VkLongpoll, fpjson;

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
		 SpeedButton1: TSpeedButton;
		 Splitter1: TSplitter;
		 procedure FormCreate(Sender: TObject);
   private

   public

   end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure NewMessageHandler(Event: TJSONArray);
begin
  writeln(Event.Strings[5]);
  MainForm.ListBox2.AddItem(Event.Strings[5], nil);
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  LongpollThread.RegisterEventHandler(4, @NewMessageHandler);
end;

end.

