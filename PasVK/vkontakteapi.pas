unit vkontakteapi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fprequests, jsonparser, fpjson;

type
  TVKAPI = class
    private
      requests: TRequests;

    public
      access_token: String;
      version: String;

      function call(method: String; params: TParams): TJSONData;
      constructor Create;

end;

implementation

function TVKAPI.call(method: String; params: TParams): TJSONData;
var
  response: TJSONObject;
begin
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
  version := '5.130';
end;

end.
