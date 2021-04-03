{%RunFlags MESSAGES+}
unit augvkapi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fprequests, fpjson, jsonparser, utils, fgl,
  vkontakteapi, Graphics;

type TMSG = class;
type TUser = class;

type TMSGsArray = array of TMSG;

type
  TMSG = class
    id: Integer;
    text: String;
    date: Integer;
    peerId: Integer;
    fromId: TUser;
    reply: TMSGsArray;
end;

type
  TUser = class
    Name: String;
    Id: Integer;
    Image: TPicture;
end;
type TUsersArray = array of TUser;

type
  TChat = class
    id: Integer;
    name: String;
    previewMsg: TMSG;
    type_: String;
end;
type TChatsArray = array of TChat;
type TChatsList = specialize TFPGList<TChat>;

type
  TAttachment = class
    //todo
end;
type TAttachmentsArray = array of TAttachment;

type
  TAugVKAPI = class
  private
    requests: TRequests;
    VKAPI: TVKAPI;

  public
    function parseMsg(data: TJSONObject): TMSG;
    function parseUser(data: TJSONObject): TUser;
    function parseGroup(data: TJSONObject): TUser;

    function getMSGById(msgId: Integer): TMSG;
    function getMSGsById(msgsId: array of Integer): TMSGsArray;

    procedure addUser(user: TUser);
    function getUser(id: Integer): TUser;
    function getUsers(ids: array of Integer): TUsersArray;

    function getChats: TChatsArray;
    function getChat(id: Integer): TChat;
    function getChatsById(ids: array of Integer): TChatsArray;

    function getHistory(peerId: Integer; count: Integer; offset: Integer = 0): TMSGsArray;

    function updateChatsPosition(peerId: Integer): TChatsArray;
    function getChatsForDraw: TChatsArray;
    function getChatByIndex(index: Integer): TChat;

    procedure SendMessage(Text: String; PeerId: Integer; Reply: Integer; Attachments: TAttachmentsArray);
    procedure SendMessage(Text: String; PeerId: Integer); overload;
    procedure SendMessage(Text: String; PeerId: Integer; Reply: Integer); overload;
    procedure SendMessage(Text: String; PeerId: Integer; Attachments: TAttachmentsArray); overload;

    constructor Create(token: String);
end;

implementation
var
  usersCache: TUsersArray;
  drawedChats: TChatsList;

procedure TAugVKAPI.SendMessage(Text: String; PeerId: Integer); overload;
begin
  SendMessage(Text,PeerId,-1,Nil);
end;

procedure TAugVKAPI.SendMessage(Text: String; PeerId: Integer; Reply: Integer); overload;
begin
  SendMessage(Text,PeerId,Reply,Nil);
end;

procedure TAugVKAPI.SendMessage(Text: String; PeerId: Integer; Attachments: TAttachmentsArray); overload;
begin
  SendMessage(Text,PeerId,-1,Attachments);
end;

procedure TAugVKAPI.SendMessage(Text: String; PeerId: Integer; Reply: Integer; Attachments: TAttachmentsArray);
var
  Params: TParams;
begin
  Params := TParams.Create;
  Params
     .Add('peer_id',PeerId)
     .Add('random_id',0)
     .Add('message',Text);

  if Reply <> -1 then
     Params.Add('reply_to',Reply);

  VKAPI.Call(
     'messages.send',
     Params
  );
end;

function TAugVKAPI.getChatByIndex(index: Integer): TChat;
begin
  Result := drawedChats[index];
end;

function TAugVKAPI.updateChatsPosition(peerId: Integer): TChatsArray;
var
  chat: TChat;
begin
  for chat in drawedChats do
    if chat.id = peerId then
    begin
      drawedChats.Move(
        drawedChats.IndexOf(chat),0);
      Exit;
    end;

  Result := getChatsForDraw;
end;

function TAugVKAPI.getChatsForDraw: TChatsArray;
var
  chat: TChat;
begin
  if drawedChats.Count = 0 then
    for chat in getChats do
      drawedChats.Add(chat);

  for chat in drawedChats do
  begin
    SetLength(Result,Length(Result)+1);
    Result[Length(Result)-1] := chat;
  end;
end;

function TAugVKAPI.getHistory(peerId: Integer; count: Integer; offset: Integer = 0): TMSGsArray;
var
  response: TJSONObject;
  jsonEnum: TJSONEnum;
  items, profilesArray: TJSONArray;
  item: TJSONObject;
  userType: String;
  index: Integer;
begin
  response := TJSONObject(vkapi.call('messages.getHistory',
    TParams.Create
      .add('count',count)
      .add('offset',offset)
      .add('peer_id',peerId)
      .add('extended',1)
  ));

  for userType in ['profiles','groups'] do
  begin
    if response.IndexOfName(userType) = -1 then continue;

    profilesArray := response.Arrays[userType];
    for jsonEnum in profilesArray do
      parseUser(TJSONObject(jsonEnum.Value));
  end;

  items := response.Arrays['items'];
  for jsonEnum in items do
  begin
    SetLength(Result,Length(Result)+1);
    index := Length(Result)-1;

    item := TJSONObject(jsonEnum.Value);
    Result[index] := parseMsg(item);
  end;

end;

function TAugVKAPI.getChat(id: Integer): TChat;
begin
  Result := getChatsById([id])[0];
end;

function TAugVKAPI.getChatsById(ids: array of Integer): TChatsArray;
var
  response: TJSONArray;
  chat: TJSONObject;
  jsonEnum: TJSONEnum;
  idsstr: String;
  id, index: Integer;
begin
  idsstr := '';
  for id in ids do
    idsstr += IntToStr(id-2000000000)+',';

  response := TJSONArray(vkapi.call('messages.getChat',
    TParams.Create
      .add('chat_ids',idsstr)
  ));
  for jsonEnum in response do
  begin
    SetLength(Result,Length(Result)+1);
    index := Length(Result)-1;

    chat := TJSONObject(jsonEnum.Value);
    Result[index] := TChat.Create;
    Result[index].name := chat.Strings['title'];
    Result[index].type_ := chat.Strings['type'];
    Result[index].id := chat.Integers['id'];
    Result[index].previewMsg := nil;
  end;

end;

function TAugVKAPI.getChats: TChatsArray;
var
  chatsArray, profilesArray: TJSONArray;
  chatObject, profileObject, response: TJSONObject;
  jsonEnum: TJSONEnum;
  offset, index: Integer;
  previewMsg: TMSG;
  userType: String;
begin
  offset := 0;

  while True do
  begin
    response := TJSONObject(vkapi.call(
       'messages.getConversations',
       TParams.Create
          .add('offset', offset)
          .add('count', 200)
          .add('extended', 1)
    ));

    writeln(Format('Loading chats %d / %d',[offset,response['count'].AsInteger]));

    chatsArray := response.Arrays['items'];
    if chatsArray.Count = 0 then break;

    for userType in ['profiles','groups'] do
    begin
      if response.IndexOfName(userType) = -1 then continue;

      profilesArray := response.Arrays[userType];
      for jsonEnum in profilesArray do
      begin
        profileObject := TJSONObject(jsonEnum.Value);
        parseUser(profileObject);
      end;
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

    if response['count'].AsInteger <= 200 then break
    else offset += 200;
  end;
end;

function TAugVKAPI.parseUser(data: TJSONObject): TUser;
var
  TmpStream: TFileStream;
  FileName: String;
  User: TUser;
begin
  Result := TUser.Create;
  Result.id := data['id'].AsInteger;

  for User in UsersCache do
    if User.Id = Data.Integers['id'] then
    begin
       Result := User;
       Exit;
    end;

  if data.IndexOfName('type') <> -1 then
    if (data['type'].AsString = 'group') or
       (data['type'].AsString = 'page') or
       (data['id'].AsInteger < 0) then
    begin
      Result := parseGroup(data);
      Exit;
    end;

  Result.name := data['first_name'].AsString + ' ' +
                 data['last_name'].AsString;

  FileName := Format('data/%d.jpg',[data['id'].AsInteger]);

  {Проверяем повреждено ли фото}
  if FileExists(FileName) then
  begin
    TmpStream := TFileStream.Create(
     FileName,
     fmOpenReadWrite
    );
    if TmpStream.Size = 0 then
    begin
      TmpStream.Free;
      DeleteFile(FileName);
    end;
    TmpStream.Free;
  end;

  {качаем фото}
  if not FileExists(FileName) then
  begin
    TmpStream := TFileStream.Create(
       FileName,
       fmCreate
    );
    requests.get(data['photo_50'].AsString, TmpStream);
    TmpStream.Free;
  end;

  writeln('Loading avatar id'+inttostr(data['id'].AsInteger));

  TmpStream := TFileStream.Create(
   FileName,
   fmOpenReadWrite
  );
  Result.Image := TPicture.Create;

  Result.Image.LoadFromStream(TmpStream);
  TmpStream.Free;

  addUser(Result);
end;

function TAugVKAPI.parseGroup(data: TJSONObject): TUser;
begin
  if (data['type'].AsString = 'group') or
     (data['type'].AsString = 'page') then
  begin
    Result := TUser.Create;
    Result.id := data['id'].AsInteger * -1;
    Result.name := data['name'].AsString;

    addUser(Result);
  end;
end;

procedure TAugVKAPI.addUser(user: TUser);
var
  i: TUser;
begin
  for i in usersCache do
     if i.id = user.id then exit;

  SetLength(usersCache,Length(usersCache)+1);
  usersCache[Length(usersCache)-1] := user;
end;

function TAugVKAPI.getUser(id: Integer): TUser;
begin
  Result := getUsers([id])[0];
end;

function TAugVKAPI.getUsers(ids: array of Integer): TUsersArray;
var
  userIds: String;
  id, index: Integer;
  user: TUser;
  jsonEnum: TJSONEnum;
  userObject: TJSONObject;
  response: TJSONArray;
  exists: Boolean;
begin
  userIds := '';

  for id in ids do
  begin
    exists := False;
    for user in usersCache do
    begin
       if user.id = id then
       begin
         SetLength(Result,Length(Result)+1);
         Result[Length(Result)-1] := user;
         exists := True;
         break;
       end;
    end;
    if not exists then
      userIds += IntToStr(id)+','
  end;

  if userIds <> '' then
  begin
    response := TJSONArray(vkapi.call(
      'users.get',
      TParams.Create
        .add('user_ids',userIds)
        .add('fields','photo_50, last_seen')
    ));

    for jsonEnum in response do
    begin
      userObject := TJSONObject(jsonEnum.Value);

      SetLength(Result,Length(Result)+1);
      index := Length(Result)-1;

      Result[index] := parseUser(userObject);
    end;
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

  response := TJSONObject(vkapi.call(
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
  Result.fromId := getUser(data['from_id'].AsInteger);
  Result.peerId := data['peer_id'].AsInteger;
  Result.date := data['date'].AsInteger;
end;

constructor TAugVKAPI.Create(token: String);
begin
  requests := TRequests.Create;
  vkapi := TVKAPI.Create;
  vkapi.access_token := token;
  vkapi.version := '5.130';
end;

initialization
begin
  drawedChats := TChatsList.Create;
end;

end.

