unit VkontakteApi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fprequests, jsonparser, fpjson;

const
  DEFAULT_API_VERSION = '5.130';

type

	{ TVKAPI }

  TVKAPI = class (TInterfacedObject)
  private
    Requests: TRequests;

  public
    Access_Token: String;
    Version: String;

    function Call(Method: String; Params: TParams): TJSONData;
    constructor Create;
    constructor Create(AToken: String);
  end;

implementation

function TVKAPI.Call(Method: String; Params: TParams): TJSONData;
var
  response: TJSONObject;
begin
  WriteLn('Call ',Method);
  response := TJSONObject(
     requests.post(
         Format('https://api.vk.com/method/%s',[method]),
         params
           .add('v',version)
           .add('access_token',access_token)
     ).json()
  );

  if response.IndexOfName('error') <> -1 then
  begin
    if response.getPath('error.error_code').asInteger = 6 then
    begin
      Sleep(1000);
      writeln('Forced sleep');
      Result := call(method,params);
      Exit;
    end;

    raise Exception.create(
    format('VK ERROR #%d: "%s"'#13#10'PARAMS: %s',
       [response.getPath('error.error_code').asInteger,
        response.getPath('error.error_msg').asString,
        response.getPath('error.request_params').asJSON]
    ));
  end;
  Result := response['response'];
end;

constructor TVKAPI.Create;
begin
  requests := TRequests.Create;
  version := DEFAULT_API_VERSION;

  inherited Create;
end;

constructor TVKAPI.Create(AToken: String);
begin
  Self.Create;
  access_token := AToken;
end;

end.
