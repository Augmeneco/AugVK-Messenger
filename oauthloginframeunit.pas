unit OAuthLoginFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, Forms, Controls, FramBrwz, HtmlView, HtmlGlobals, HTMLUn2;

type

  { TOAuthLoginFrame }

  TOnLoginedCallback = procedure (Token: String; ExpiresIn, Id: Integer) of object;

  TOAuthLoginFrame = class(TFrame)
    FPHTTPClient1: TFPHTTPClient;
    FrameBrowser1: TFrameBrowser;
    procedure FPHTTPClient1Redirect(Sender: TObject; const ASrc: String;
      var ADest: String);
    procedure FrameBrowser1GetPostRequestEx(Sender: TObject; IsGet: Boolean;
      const URL, Query, EncType, Referer: ThtString; Reload: Boolean;
      var NewURL: ThtString; var DocType: ThtmlFileType; var Stream: TStream);
    procedure FrameBrowser1ImageRequest(Sender: TObject; const SRC: ThtString;
      var Stream: TStream);
  private
    Cookies: TStrings;
    Redirected: Boolean;
    FURL: String;
    FToken: String;
  public
    procedure OpenLoginPage;
    constructor Create(TheOwner: TComponent); override;
  public
    OnLogined: TOnLoginedCallback;
    property Token: String read FToken;
  end;

implementation

uses
  URIParser, LazLoggerBase;

{$R *.lfm}

{ TOAuthLoginFrame }

procedure TOAuthLoginFrame.FPHTTPClient1Redirect(Sender: TObject; const ASrc: String;
  var ADest: String);
begin
  FURL := ADest;
  Redirected := True;
end;

procedure TOAuthLoginFrame.FrameBrowser1GetPostRequestEx(Sender: TObject;
  IsGet: Boolean; const URL, Query, EncType, Referer: ThtString;
  Reload: Boolean; var NewURL: ThtString; var DocType: ThtmlFileType;
  var Stream: TStream);
var
  URLParts: TURI;
  ParamList: TStringList;
begin
  ParamList := TStringList.Create;
  ParamList.Delimiter := '&';
  ParamList.StrictDelimiter := True;

  Redirected := False;
  URLParts := ParseURI(url);
  //if String(URL).IndexOf('css') > 0 then
  //  exit;

  //FPHTTPClient1.Cookies := Cookies;

  DebugLn(Query);
  if IsGet then
  begin
    if URLParts.Protocol = 'file' then
    begin
      Stream := TFileStream.Create(URLParts.Host, fmOpenRead);
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
    ParamList.DelimitedText := Query;
    FPHTTPClient1.AddHeader('Referer', Referer);
    DebugLn(ParamList.Text);
    FPHTTPClient1.FormPost(URL, ParamList, Stream);
    DocType := THtmlFileType.HTMLType;
  end;

  //Cookies := FPHTTPClient1.Cookies;

  DebugLn(FPHTTPClient1.Cookies.Text);

  if not Redirected then
    FURL := URL
  else
    NewURL := FURL;

  URLParts := ParseURI(FURL);
  if URLParts.Document = 'blank.html' then
  begin
    ParamList.DelimitedText := URLParts.Bookmark;
    FToken := ParamList.Values['access_token'];
    OnLogined(ParamList.Values['access_token'],
      ParamList.Values['expires_in'].ToInteger,
      ParamList.Values['access_token'].ToInteger);
  end;
  ParamList.Free;
end;

procedure TOAuthLoginFrame.FrameBrowser1ImageRequest(Sender: TObject;
  const SRC: ThtString; var Stream: TStream);
begin
  Stream := TBytesStream.Create;
  //Filename := FrameBrowser1.HTMLExpandFilename(SRC);
  FPHTTPClient1.Get(SRC, Stream);
end;

procedure TOAuthLoginFrame.OpenLoginPage;
begin
  FrameBrowser1.LoadURL('https://oauth.vk.com/authorize?client_id=7950449&scope=69662&redirect_uri=https://oauth.vk.com/blank.html&display=mobile&response_type=token&revoke=1');
  Show;
end;

constructor TOAuthLoginFrame.Create(TheOwner: TComponent);
var
  Proxy: TProxyData;
begin
  inherited Create(TheOwner);

  //Proxy := TProxyData.Create;
  //Proxy.Host:='127.0.0.1';
  //Proxy.Port:=8888;
  //FPHTTPClient1.Proxy := Proxy;
end;

end.

