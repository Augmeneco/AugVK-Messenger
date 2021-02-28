unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
	StdCtrls, IpHtml;

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

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin

end;

end.

