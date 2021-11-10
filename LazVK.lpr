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
	 VkLongpoll,} {WebBrowserFormUnit,} Utils, design, ConfigUtils, LoginFrameUnit,
   MediaViewerFormUnit, lazrichview, LResources, dialogpanel;

{$R *.res}

begin
  {$I AugVK.lrs}
  RequireDerivedFormResource:=True;
	Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

