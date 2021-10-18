unit WebBrowserFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, FramBrwz, HtmlView,
  UrlConn, HtmlGlobals, fprequests, HTMLUn2, fphttpclient, LazLoggerBase;

type

	{ TWebBrowserForm }

  TWebBrowserForm = class(TForm)
    FPHTTPClient1: TFPHTTPClient;
    FrameBrowser1: TFrameBrowser;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FPHTTPClient1Redirect(Sender: TObject; const ASrc: String;
      var ADest: String);
    procedure FrameBrowser1GetPostRequestEx(Sender: TObject; IsGet: Boolean;
      const URL, Query, EncType, Referer: ThtString; Reload: Boolean;
      var NewURL: ThtString; var DocType: ThtmlFileType; var Stream: TStream);
    procedure FrameBrowser1ImageRequest(Sender: TObject; const SRC: ThtString;
      var Stream: TStream);
  private
    Redirected: Boolean;
    FURL: String;
    //Requests: TRequests;
    procedure SetURL(AURL: String);
    function GetURL: String;
  public
    URLToOpen: String;
    Token: String;
    function GetOAuthToken: String;
    property LoadedURL: String read GetURL write SetURL;
  end;

implementation

{$R *.lfm}

uses
  URIParser;

{ TWebBrowserForm }

procedure TWebBrowserForm.FormShow(Sender: TObject);
begin
  //LoadedURL := URLToOpen;
end;

procedure TWebBrowserForm.FPHTTPClient1Redirect(Sender: TObject;
  const ASrc: String; var ADest: String);
begin
  FURL := ADest;
  Redirected := True;
end;

procedure TWebBrowserForm.FrameBrowser1GetPostRequestEx(Sender: TObject;
  IsGet: Boolean; const URL, Query, EncType, Referer: ThtString;
  Reload: Boolean; var NewURL: ThtString; var DocType: ThtmlFileType;
  var Stream: TStream);
var
  uri: TURI;
  ss: TStringStream;
begin
  Redirected := False;
  uri := ParseURI(url);
  //if String(URL).IndexOf('css') > 0 then
  //  exit;
  if IsGet then
  begin
    if uri.Protocol = 'file' then
    begin
      Stream := TFileStream.Create(uri.Host, fmOpenRead);
      DocType := ThtmlFileType.HTMLType;
      exit;
    end;
    Stream := TBytesStream.Create;
    FPHTTPClient1.Get(URL, Stream);
    DocType := THtmlFileType.HTMLType;
  end
  else
  begin
    Stream := TBytesStream.Create;
    FPHTTPClient1.FormPost(URL, Query, Stream);
    DocType := THtmlFileType.HTMLType;
  end;

  if not Redirected then
    FURL := URL
  else
    NewURL := FURL;
end;

procedure TWebBrowserForm.FrameBrowser1ImageRequest(Sender: TObject;
  const SRC: ThtString; var Stream: TStream);
var
  Filename: String;
begin
  Stream := TBytesStream.Create;
  //Filename := FrameBrowser1.HTMLExpandFilename(SRC);
  //Requests.Get(Filename, Stream);
  FPHTTPClient1.Get(SRC, Stream);
end;

procedure TWebBrowserForm.FormCreate(Sender: TObject);
begin
  //Requests := TRequests.Create;
  //Requests.AllowedCodes := [200, 301];
  FPHTTPClient1.AllowRedirect := True;
end;

procedure TWebBrowserForm.SetURL(AURL: String);
begin
  FrameBrowser1.LoadURL(AURL);
  //FrameBrowser1.LoadFromFile('vk.html');
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
  URLParts := ParseURI(LoadedURL);
  ParamList := TStringList.Create;
  ParamList.Delimiter := '&';
  ParamList.StrictDelimiter := True;
  ParamList.DelimitedText := URLParts.Bookmark;
  Result := ParamList.Values['access_token'];
  ParamList.Free;
end;

end.

