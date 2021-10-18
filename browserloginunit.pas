unit BrowserLoginUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LoginUnit;

type
  TOnLoginedCallback = procedure (Token: String; ExpiresIn, Id: Integer) of object;

  { TBrowserLogin }

  TBrowserLogin = class
    OnLogined: TOnLoginedCallback;
    procedure Open;
    procedure Close;
    procedure OpenLoginPage;
  end;

implementation

uses
  Process, uRegExpr, URIParser;

{ TBrowserLogin }

procedure TBrowserLogin.Open;
begin

end;

procedure TBrowserLogin.Close;
begin

end;

procedure TBrowserLogin.OpenLoginPage;
var
  StrOut: String;
  Re: TRegExpr;
  Url: String;
  BookmarkParsed: TStringList;
begin
  RunCommand('DialogBrowser', [VK_OAUTH_PAGE, '.*vk\.com\/blank\.html.*'], StrOut);

  Re := TRegExpr.Create('Result\n(.*)\n\/Result');
  if Re.Exec(StrOut) then
  begin
    Url := Re.Match[0];
  end;
  Re.Free;

  // Добавить проверку существования УРЛ
  BookmarkParsed := TStringList.Create;
  BookmarkParsed.Delimiter := '&';
  BookmarkParsed.StrictDelimiter := True;
  BookmarkParsed.DelimitedText := ParseURI(Url).Bookmark;

  // Добавить проверку существования параметров
  if Assigned(OnLogined) then
    OnLogined(BookmarkParsed.Values['access_token'],
      BookmarkParsed.Values['expires_in'].ToInteger,
      BookmarkParsed.Values['access_token'].ToInteger);

  BookmarkParsed.Free;
end;

end.

