program LazVK;

{$mode objfpc}{$H+}
//{$Define CD_UseNativeText}

uses
   {$IFDEF UNIX}
   cthreads,
   {$ENDIF}
   {$IFDEF HASAMIGA}
   athreads,
   {$ENDIF}
   Interfaces, // this includes the LCL widgetset
   Forms, MainFormUnit, Net, ChatFrameUnit, {VkontakteApi,
	 VkLongpoll,} {WebBrowserFormUnit,} Utils, design;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
	Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

