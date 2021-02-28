unit augvkapi;

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
    //onlineTime: Integer;
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
  TAugVKAPI = class
  private
    requests: TRequests;
    config: TConfig;

    //function getUserFromCache(id: Integer): TUser;
    //function getUsersFromCache(ids: array of Integer): TUsersArray;

    function parseMsg(data: TJSONObject): TMSG;
    function parseUser(data: TJSONObject): TUser;

  public
    function call(method: String; params: TParams): TJSONData;

    function getMSGById(msgId: Integer): TMSG;
    function getMSGsById(msgsId: array of Integer): TMSGsArray;

    procedure addUser(user: TUser);
    function getUser(id: Integer): TUser;
    function getUsers(ids: array of Integer): TUsersArray;

    function getChats: TChatsArray;

    constructor Create;
end;

var
  knownUsers: TUsersArray;

implementation

function TAugVKAPI.getChats: TChatsArray;
var
  chatsArray, profilesArray: TJSONArray;
  chatObject, profileObject, response: TJSONObject;
  jsonEnum: TJSONEnum;
  offset, index: Integer;
  previewMsg: TMSG;
begin
  offset := 0;

  while True do
  begin
    response := TJSONObject(Self.call(
       'messages.getConversations',
       TParams.Create
          .add('offset', offset)
          .add('count', 200)
          .add('extended', 1)
    ));

    chatsArray := response.Arrays['items'];
    if chatsArray.Count = 0 then break;

    profilesArray := response.Arrays['profiles'];

    for jsonEnum in profilesArray do
    begin
      profileObject := TJSONObject(jsonEnum.Value);
      parseUser(profileObject);
    end;

    for jsonEnum in chatsArray do
    begin
      SetLength(Result,Length(Result)+1);
      index := Length(Result)-1;

      chatObject := TJSONObject(jsonEnum.Value);

      Result[index] := TChat.Create;
      Result[index].id := chatObject.GetPath('conversation.peer.id').AsInteger;

      if chatObject.GetPath('conversation.peer.type').AsString = 'chat' then
      begin
        Result[index].name := chatObject.GetPath('conversation.chat_settings.title').AsString;
      end
      else
      begin
        Result[index].name := getUser(Result[index].id).name;
      end;

      Result[index].previewMsg := parseMsg(chatObject.Objects['last_message']);
    end;

    if chatObject['count'].AsInteger <= 200 then break
    else offset += 200;
  end;
end;

function TAugVKAPI.parseUser(data: TJSONObject): TUser;
begin
  Result := TUser.Create;
  Result.id := data['id'].AsInteger;
  Result.name := data['first_name'].AsString + ' ' +
                 data['last_name'].AsString;

  addUser(Result);
end;

procedure TAugVKAPI.addUser(user: TUser);
var
  i: TUser;
begin
  for i in knownUsers do
     if i.id = user.id then break;

  SetLength(knownUsers,Length(knownUsers)+1);
  knownUsers[Length(knownUsers)-1] := user;
end;

function TAugVKAPI.getUser(id: Integer): TUser;
begin
  writeln('dive');
  Result := getUsers([id])[0];
end;

function TAugVKAPI.getUsers(ids: array of Integer): TUsersArray;
var
  idsStr: String;
  id, index: Integer;
  user: TUser;
  jsonEnum: TJSONEnum;
  userObject: TJSONObject;
  response: TJSONArray;
begin
  idsStr := '';

  for id in ids do
  begin
    for user in knownUsers do
    begin
       if user.id = id then
       begin
         SetLength(Result,Length(Result)+1);
         Result[Length(Result)-1] := user;
       end
       else
         idsStr += IntToStr(id)+',';
    end;
  end;

  response := TJSONArray(Self.call(
    'users.get',
    TParams.Create
      .add('user_ids',idsStr)
      .add('fields','photo_50, last_seen')
  ));

  writeln(response.FormatJSON());

  for jsonEnum in response do
  begin
    userObject := TJSONObject(jsonEnum.Value);

    SetLength(Result,Length(Result)+1);
    index := Length(Result)-1;

    Result[index] := parseUser(userObject);
  end;
end;

function TAugVKAPI.getMSGById(msgId: Integer): TMSG;
begin
  Result := getMSGsById([msgId])[0];
end;

function TAugVKAPI.getMSGsById(msgsId: array of Integer): TMSGsArray;
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
      .add('extended',1)
  ));

  for jsonEnum in response.Arrays['items'] do
  begin
    msgObject := TJSONObject(jsonEnum.Value);

    SetLength(Result,Length(Result)+1);
    index := Length(Result)-1;

    Result[index] := parseMsg(msgObject);
  end;
end;

function TAugVKAPI.parseMsg(data: TJSONObject): TMSG;
begin
  Result := TMSG.Create;
  Result.id := data['id'].AsInteger;
  Result.text := data['text'].AsString;
  Result.fromId := data['from_id'].AsInteger;
  Result.peerId := data['peer_id'].AsInteger;
  Result.date := data['date'].AsInteger;
end;

function TAugVKAPI.call(method: String; params: TParams): TJSONData;
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

constructor TAugVKAPI.Create;
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

