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
type TUsersArray = array of TUser;

type
  TChat = class
    id: Integer;
    name: String;
    previewMsg: TMSG;
end;
type TChatsArray = array of TChat;

type
  TVKAPI = class
  private
    requests: TRequests;
    config: TConfig;

    //function getUserFromCache(id: Integer): TUser;
    //function getUsersFromCache(ids: array of Integer): TUsersArray;

  public
    function call(method: String; params: TParams): TJSONData;

    function getMSGById(msgId: Integer): TMSG;
    function getMSGsById(msgsId: array of Integer): TMSGsArray;

    function getUser(id: Integer): TUser;
    function getUsers(ids: array of Integer): TUsersArray;

    function getChats: TChatsArray;

    constructor Create;
end;

implementation

function TVKAPI.getChats: TChatsArray;
var
  chatsArray: TJSONArray;
  chatObject: TJSONObject;
  jsonEnum: TJSONEnum;
  offset: Integer;
begin
  offset := 0;

  while True do
  begin
    chatObject := TJSONObject(Self.call(
       'messages.getConversations',
       TParams.Create
          .add('offset', offset)
          .add('count', 200)
          .add('extended', 1)
    ));
    if chatObject['count'].AsInteger <= 200 then break
    else offset += 200;

    chatsArray := chatObject.Arrays['items'];
    if chatsArray.Count = 0 then break;


  end;


end;

function TVKAPI.getUser(id: Integer): TUser;
begin
  Result := getUsers([id])[0];
end;

function TVKAPI.getUsers(ids: array of Integer): TUsersArray;
var
  idsStr: String;
  id, index: Integer;
  jsonEnum: TJSONEnum;
  response, userObject: TJSONObject;
begin
  idsStr := '';

  for id in ids do
    idsStr += IntToStr(id)+',';

  response := TJSONObject(Self.call(
    'users.get',
    TParams.Create
      .add('user_ids',idsStr)
      .add('fields','photo_50, last_seen')
  ));

  for jsonEnum in response.Arrays['items'] do
  begin
    userObject := TJSONObject(jsonEnum.Value);

    SetLength(Result,Length(Result)+1);
    index := Length(Result)-1;

    Result[index] := TUser.Create;
    //Result[index].id := userObject['id'].AsInteger;
    //Result[index].text := userObject['text'].AsString;
    //Result[index].fromId := userObject['from_id'].AsInteger;
    //Result[index].peerId := userObject['peer_id'].AsInteger;
    //Result[index].date := userObject['date'].AsInteger;
  end;
end;

function TVKAPI.getMSGById(msgId: Integer): TMSG;
begin
  Result := getMSGsById([msgId])[0];
end;

function TVKAPI.getMSGsById(msgsId: array of Integer): TMSGsArray;
var
  ids: String;
  id, index: Integer;
  jsonEnum: TJSONEnum;
  response, msgObject: TJSONObject;
begin
  ids := '';

  for id in msgsId do
    ids += IntToStr(id)+',';

  response := TJSONObject(Self.call(
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
  config.access_token := 'b2f8dccd59bc5fc95a7d273ae0986e62fbe5edb6a019f0653006eead69fabb06fc158e8852dd4efb88d21';
  config.version := '5.130';
end;

initialization
begin
  //
end;

end.

