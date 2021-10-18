unit CefLoginFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, GlobalCefApplication, uCEFApplication,
  uCEFBrowserWindow, uCEFChromium, uCEFChromiumWindow, uCEFLinkedWindowParent,
  LoginUnit, uCEFTypes, uCEFInterfaces, uCEFChromiumEvents, uCEFWorkScheduler,
  uCEFWindowParent;

type

  { TCefLoginFrame }

  TCefLoginFrame = class(TFrame)
    BrowserWindow1: TBrowserWindow;
    procedure Chromium1AddressChange(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
  private

  public
    OnLogined: TOnLoginedCallback;
    procedure OpenLoginPage;
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

uses
  URIParser;

{ TCefLoginFrame }

procedure TCefLoginFrame.Chromium1AddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
var
  BookmarkParsed: TStringList;
begin
  BookmarkParsed := TStringList.Create;
  BookmarkParsed.Delimiter := '&';
  BookmarkParsed.StrictDelimiter := True;
  BookmarkParsed.DelimitedText := ParseURI(Url).Bookmark;

  // Добавить проверку существования параметров
  if BookmarkParsed.IndexOfName('access_token') < 0 then
    exit;
  if Assigned(OnLogined) then
    OnLogined(BookmarkParsed.Values['access_token'],
      BookmarkParsed.Values['expires_in'].ToInteger,
      BookmarkParsed.Values['user_id'].ToInteger);

  BookmarkParsed.Free;
end;

procedure TCefLoginFrame.OpenLoginPage;
begin
  BrowserWindow1.Chromium.LoadURL(VK_OAUTH_PAGE);
  Show;
  BringToFront;
end;

constructor TCefLoginFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  BrowserWindow1.Chromium.OnAddressChange := @Chromium1AddressChange;
end;

initialization
  {$IFDEF DARWIN}  // $IFDEF MACOSX
  AddCrDelegate;
  {$ENDIF}
  if GlobalCEFApp = nil then begin
    CreateGlobalCEFApp;
    if not GlobalCEFApp.StartMainProcess then begin
      DestroyGlobalCEFApp;
      DestroyGlobalCEFWorkScheduler;
      halt(0); // exit the subprocess
    end;
  end;

finalization
  (* Destroy from this unit, which is used after "Interfaces". So this happens before the Application object is destroyed *)
  if GlobalCEFWorkScheduler <> nil then
    GlobalCEFWorkScheduler.StopScheduler;
  DestroyGlobalCEFApp;
  DestroyGlobalCEFWorkScheduler;

end.

