unit pasvk;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fprequests, fpjson, jsonparser;
type
  TConfig = record
    version: String;
    access_token: String;
end;

type TMSG = class;

type TMSGsArray = array of TMSG;

type
  TMSG = class
    id: Integer;
    text: String;
    date: Integer;
    peerId: Integer;
    fromId: Integer;
    //attachments: TAttachmentsArray;
    reply: TMSGsArray;
end;

type
  TUser = class
    name: String;
    id: Integer;
    online: Boolean;
    onlineTime: Integer;
end;

type
  TVKAPI = class
  private
    requests: TRequests;
  public
    function call(method: String; params: TParams): TJSONData;
    constructor Create;
end;

function getMSGById(msgId: Integer): TMSG;
function getMSGsById(msgsId: array of Integer): TMSGsArray;

var
  config: TConfig;
  vkapi: TVKAPI;

implementation

function getMSGById(msgId: Integer): TMSG;
begin
  Result := getMSGsById([msgId])[0];
end;

function getMSGsById(msgsId: array of Integer): TMSGsArray;
var
  ids: String;
  id, index: Integer;
  jsonEnum: TJSONEnum;
  response, msgObject: TJSONObject;
begin
  ids := '';

  for id in msgsId do
    ids += IntToStr(id)+',';

  response := TJSONObject(vkapi.call(
    'messages.getById',
    TParams.Create
      .add('message_ids',ids)
      .add('extended','0') //потом мб понадобится
  ));

  for jsonEnum in response.Arrays['items'] do
  begin
    msgObject := TJSONObject(jsonEnum.Value);

    SetLength(Result,Length(Result)+1);
    index := Length(Result)-1;

    Result[index] := TMSG.Create;
    Result[index].id := msgObject['id'].AsInteger;
    Result[index].text := msgObject['text'].AsString;
    Result[index].fromId := msgObject['from_id'].AsInteger;
    Result[index].peerId := msgObject['peer_id'].AsInteger;
    Result[index].date := msgObject['date'].AsInteger;
  end;
end;

function TVKAPI.call(method: String; params: TParams): TJSONData;
var
  response: TJSONObject;
begin
  response := TJSONObject(
     requests.post(
         Format('https://api.vk.com/method/%s',[method]),
         params
           .add('v',config.version)
           .add('access_token',config.access_token)
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
end;

initialization
begin
  {$include secretdata.txt}
  config.version := '5.130';
  vkapi := TVKAPI.Create;
end;

end.

