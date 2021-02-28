unit VkontakteApi;


interface

  uses
    fpjson, jsonparser, sysutils,
    Net;

type

	{ TVkApi }

  TVkApi = class (TInterfacedObject)
  private
    Token: String;
  public
    function Login: String;
	  function Call(method: AnsiString; parameters: Array of AnsiString): TJSONData;
	  procedure SendMsg(peer_id: Integer; text: String);
  end;


implementation

uses Classes, URIParser, WebBrowserFormUnit;

function encodeUrl(url: string): string;
var
  x: integer;
  sBuff: string;
const
  SafeMask = ['A'..'Z', '0'..'9', 'a'..'z', '*', '@', '.', '_', '-'];
begin
  //Init
  sBuff := '';

  for x := 1 to Length(url) do
  begin
    //Check if we have a safe char
    if url[x] in SafeMask then
    begin
      //Append all other chars
      sBuff := sBuff + url[x];
    end
    else if url[x] = ' ' then
    begin
      //Append space
      sBuff := sBuff + '+';
    end
    else
    begin
      //Convert to hex
      sBuff := sBuff + '%' + IntToHex(Ord(url[x]), 2);
    end;
  end;

  Result := sBuff;
end;

function TVkApi.Login: String;
  procedure URLHandler(URL: String);
  var
    URLParsed: TURI;
    BookmarkParsed: TStringList;
  begin
    BookmarkParsed := TStringList.Create;
    URLParsed := ParseURI(URL);
    if URLParsed.Document = 'blank.html' then
    begin
      BookmarkParsed.AddDelimitedtext(URLParsed.Bookmark, '&', True);
      //BookmarkParsed.Values['access_token'];
    //  BookmarkParsed.Values['expires_in'];
		end;
		BookmarkParsed.Free;
	end;

var
  Browser: TWebBrowserForm;
begin
  Browser := TWebBrowserForm.Create(nil);
  Browser.URL := 'https://oauth.vk.com/authorize?client_id=1&display=page&redirect_uri=http://example.com/callback&scope=friends&response_type=token&v=5.130&state=123456';
  //Browser.URLRedirectHandler := ;
  Browser.ShowModal;
end;

function TVkApi.Call(method: AnsiString; parameters: array of AnsiString): TJSONData;
var
  params: Array of String;
  i: Integer;
  response: TResponse;
  json: TJSONObject;
begin
  setLength(params, 2);
  params[0] := 'access_token='+Token;
  params[1] := 'v=5.80';
  for i := 0 to length(parameters) - 1 do
    if i mod 2 = 0 then
    begin
      setLength(params, length(params)+1);
      params[high(params)] := parameters[i]+'='+parameters[i+1];
    end;

  response := post('https://api.vk.com/method/'+method, params, []);

  json := TJSONObject(getJSON(response.text));
  if json.indexOfName('error') <> -1 then
  begin
    writeln(format('VK ERROR #%d: "%s"'#13#10'PARAMS: %s', [json.getPath('error.error_code').asInteger,
                                                            json.getPath('error.error_msg').asString,
                                                            json.getPath('error.request_params').asJSON]));
    raise Exception.create('VK ERROR');
  end;

  result := json['response'];

  setLength(params, 0);
end;

procedure TVkApi.SendMsg(peer_id: Integer; text: String);
begin
  Call('messages.send', ['peer_id', intToStr(peer_id),
                         'message', text]);
end;

end.

