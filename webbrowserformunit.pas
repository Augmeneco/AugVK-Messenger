unit WebBrowserFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, uCEFChromium,
  uCEFChromiumWindow, uCEFBrowserWindow, uCEFTypes, uCEFInterfaces,
  uCEFWorkScheduler, uCEFApplication, GlobalCefApplication,
  uCEFLazarusCocoa, uCEFWindowParent, uCEFLinkedWindowParent,
  uCEFTextfieldComponent, uCEFPanelComponent;

type

	{ TWebBrowserForm }

  TWebBrowserForm = class(TForm)
    BrowserWindow1: TBrowserWindow;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BrowserWindow1AddressChange(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
  private
    FURL: String;
    procedure SetURL(URL: String);
    function GetURL: String;
  public
    URLToOpen: String;
    function GetOAuthToken: String;
    property URL: String read GetURL write SetURL;
  end;

implementation

{$R *.lfm}

uses
  URIParser;

{ TWebBrowserForm }

procedure TWebBrowserForm.FormShow(Sender: TObject);
begin
  URL := URLToOpen;
end;

procedure TWebBrowserForm.BrowserWindow1AddressChange(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
var
  URLParts: TURI;
begin
  FURL := url;
  URLParts := ParseURI(url);
  if (URLParts.Host+URLParts.Path+URLParts.Document) = 'oauth.vk.com/blank.html' then
    Self.Close;
end;

procedure TWebBrowserForm.FormCreate(Sender: TObject);
begin
  BrowserWindow1.Chromium.OnAddressChange := @BrowserWindow1AddressChange;
end;

procedure TWebBrowserForm.SetURL(URL: String);
begin
  //IpHtmlPanel1.OpenURL(URL);
  BrowserWindow1.LoadURL(URL);
end;

function TWebBrowserForm.GetURL: String;
begin
  //Result := IpHtmlPanel1.CurURL;
  Result := FURL;
end;

function TWebBrowserForm.GetOAuthToken: String;
var
  URLParts: TURI;
  ParamList: TStringList;
  i:integer;
begin
  URLToOpen := 'https://oauth.vk.com/authorize?client_id=2685278&scope=69662&redirect_uri=https://oauth.vk.com/blank.html&display=mobile&response_type=token&revoke=1';
  Self.ShowModal;
  URLParts := ParseURI(url);
  ParamList := TStringList.Create;
  ParamList.Delimiter := '&';
  ParamList.StrictDelimiter := True;
  ParamList.DelimitedText := URLParts.Bookmark;
  Result := ParamList.Values['access_token'];
  ParamList.Free;
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

