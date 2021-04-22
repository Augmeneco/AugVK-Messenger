{%RunFlags MESSAGES+}
unit augvkapi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fprequests, fpjson, jsonparser, utils, fgl,
  vkontakteapi, Graphics;

const
  IMAGE_PATH = 'data/image';

type TMSG = class;
type TUser = class;

type TMSGsArray = array of TMSG;

type
  TMSG = class
    Id: Integer;
    Text: String;
    Date: Integer;
    PeerId: Integer;
    FromId: TUser;
    Reply: TMSGsArray;
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
    Id: Integer;
    Name: String;
    PreviewMsg: TMSG;
    Image: TPicture;
    Form: String; //бывший type_
end;
type TChatsArray = array of TChat;
type TChatsList = specialize TFPGList<TChat>;
type TChatsMap = specialize TFPGMap<Integer, TChat>;

type
  TAttachment = class
    //todo
end;
type TAttachmentsArray = array of TAttachment;

type
  TAugVKAPI = class
  private
    Requests: TRequests;
    VKAPI: TVKAPI;

    function GetAvatar(URL: String; Path: String): TPicture;

  public
    function ParseMsg(Data: TJSONObject): TMSG;
    function ParseLPMsg(Data: TJSONArray): TMSG;
    function ParseUser(Data: TJSONObject): TUser;
    function ParseGroup(Data: TJSONObject): TUser;

    function GetMSGById(MsgId: Integer): TMSG;
    function GetMSGsById(MsgsId: array of Integer): TMSGsArray;

    procedure AddUser(User: TUser);
    function GetUser(Id: Integer): TUser;
    function GetUsers(Ids: array of Integer): TUsersArray;

    function GetChats(Offset: Integer = 0): TChatsArray;
    function GetChat(Id: Integer): TChat;
    function GetChatsById(Ids: array of Integer): TChatsArray;

    function GetHistory(PeerId: Integer; Count: Integer; Offset: Integer = 0): TMSGsArray;

    function UpdateChatsPosition(PeerId: Integer): TChatsArray;
    function GetChatsForDraw: TChatsArray;
    function GetChatByIndex(Index: Integer): TChat;

    procedure SendMessage(Text: String; PeerId: Integer; Reply: Integer; Attachments: TAttachmentsArray);
    procedure SendMessage(Text: String; PeerId: Integer); overload;
    procedure SendMessage(Text: String; PeerId: Integer; Reply: Integer); overload;
    procedure SendMessage(Text: String; PeerId: Integer; Attachments: TAttachmentsArray); overload;

    constructor Create(Token: String);
end;

implementation
var
  UsersCache: TUsersArray;
  ChatsCache: TChatsMap;
  DrawedChats: TChatsList;

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

function TAugVKAPI.GetChatByIndex(Index: Integer): TChat;
begin
  Result := drawedChats[index];
end;

function TAugVKAPI.UpdateChatsPosition(PeerId: Integer): TChatsArray;
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

function TAugVKAPI.GetChatsForDraw: TChatsArray;
var
  Chat: TChat;
  I: Integer;
begin
  if drawedChats.Count = 0 then
    for I:=0 to ChatsCache.Count-1 do
      DrawedChats.Add(ChatsCache.Data[I]);

  for chat in drawedChats do
  begin
    SetLength(Result,Length(Result)+1);
    Result[Length(Result)-1] := chat;
  end;
end;

function TAugVKAPI.GetHistory(PeerId: Integer; Count: Integer; Offset: Integer = 0): TMSGsArray;
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

function TAugVKAPI.GetChat(Id: Integer): TChat;
begin
  Result := getChatsById([id])[0];
end;

function TAugVKAPI.GetChatsById(Ids: array of Integer): TChatsArray;
var
  response: TJSONArray;
  ChatObject: TJSONObject;
  Chat: TChat;
  jsonEnum: TJSONEnum;
  idsstr: String;
  id, index: Integer;
begin
  idsstr := '';
  for id in ids do
  begin
    if ChatsCache.IndexOf(Id) <> -1 then
    begin
      SetLength(Result,Length(Result)+1);
      Result[Length(Result)-1] := ChatsCache.KeyData[Id];
      Continue;
    end;

    idsstr += IntToStr(id-2000000000)+',';
  end;

  response := TJSONArray(vkapi.call('messages.getChat',
    TParams.Create
      .add('chat_ids',idsstr)
  ));
  for jsonEnum in response do
  begin
    SetLength(Result,Length(Result)+1);
    Index := Length(Result)-1;

    ChatObject := TJSONObject(jsonEnum.Value);

    if ChatsCache.IndexOf(ChatObject.Integers['id']) <> -1 then
    begin
      Result[Index] := ChatsCache.KeyData[ChatObject.Integers['id']];
      Continue;
    end;

    Result[Index] := TChat.Create;
    Result[Index].Name := ChatObject.Strings['title'];
    Result[Index].Form := ChatObject.Strings['type'];
    Result[Index].Id := ChatObject.Integers['id'];
    Result[Index].PreviewMsg := Nil;
    Result[Index].Image := GetAvatar(
      ChatObject['photo_50'].AsString,
      Format(IMAGE_PATH+'/%d.jpg',[2000000000+Result[index].id])
    );

    ChatsCache.Add(Result[Index].Id, Result[Index]);
  end;

end;

function TAugVKAPI.GetAvatar(URL: String; Path: String): TPicture;
var
  TmpStream: TFileStream;
begin
  WriteLn('Loading avatar '+Path);
  {Проверяем повреждено ли фото}
  if FileExists(Path) then
  begin
    TmpStream := TFileStream.Create(
     Path,
     fmOpenReadWrite
    );
    if TmpStream.Size = 0 then
    begin
      TmpStream.Free;
      DeleteFile(Path);
    end;
    TmpStream.Free;
  end;

  {качаем фото}
  if not FileExists(Path) then
  begin
    TmpStream := TFileStream.Create(
       Path,
       fmCreate
    );
    Requests.Get(URL, TmpStream);
    TmpStream.Free;
  end;

  TmpStream := TFileStream.Create(
   Path,
   fmOpenReadWrite
  );
  Result := TPicture.Create;

  Result.LoadFromStream(TmpStream);
  TmpStream.Free;
end;

function TAugVKAPI.GetChats(Offset: Integer = 0): TChatsArray;
var
  chatsArray, profilesArray: TJSONArray;
  chatObject, profileObject, response: TJSONObject;
  jsonEnum: TJSONEnum;
  index: Integer;
  previewMsg: TMSG;
  userType: String;
  Chat: TChat;
begin
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

      ChatObject := TJSONObject(jsonEnum.Value);

      if ChatsCache.IndexOf(ChatObject.GetPath('conversation.peer.id').AsInteger) <> -1 then
      begin
        Result[Index] := ChatsCache.KeyData[ChatObject.GetPath('conversation.peer.id').AsInteger];
        Continue;
      end;

      Result[index] := TChat.Create;
      Result[index].id := chatObject.GetPath('conversation.peer.id').AsInteger;
      Result[index].previewMsg := parseMsg(chatObject.Objects['last_message']);

      if chatObject.GetPath('conversation.peer.type').AsString = 'chat' then
      begin
        Result[index].name := chatObject.GetPath('conversation.chat_settings.title').AsString;

        if ChatObject.Objects['conversation'].
                      Objects['chat_settings'].IndexOfName('photo') = -1 then
        begin
          Result[Index].Image := Result[Index].PreviewMsg.FromId.Image;
        end
        else
          Result[Index].Image := GetAvatar(
            ChatObject.GetPath('conversation.chat_settings.photo.photo_50').AsString,
            Format(IMAGE_PATH+'/%d.jpg',[Result[index].id])
          );
      end
      else
      begin
        Result[index].name := getUser(Result[index].id).name;
        Result[index].Image := GetUser(Result[Index].Id).Image;
      end;

      ChatsCache.Add(Result[Index].Id, Result[Index]);
    end;

    if response['count'].AsInteger <= 200 then break
    else offset += 200;
  end;
end;

function TAugVKAPI.ParseUser(Data: TJSONObject): TUser;
var
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

  Result.Image := GetAvatar(
    data['photo_50'].AsString,
    Format(IMAGE_PATH+'/%d.jpg',[data['id'].AsInteger])
  );

  addUser(Result);
end;

function TAugVKAPI.ParseGroup(Data: TJSONObject): TUser;
begin
  if (data['type'].AsString = 'group') or
     (data['type'].AsString = 'page') then
  begin
    Result := TUser.Create;
    Result.id := data['id'].AsInteger * -1;
    Result.name := data['name'].AsString;

    Result.Image := GetAvatar(
      data['photo_50'].AsString,
      Format(IMAGE_PATH+'/%d.jpg',[data['id'].AsInteger*-1])
    );

    addUser(Result);
  end;
end;

procedure TAugVKAPI.AddUser(User: TUser);
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

function TAugVKAPI.GetUsers(Ids: array of Integer): TUsersArray;
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

function TAugVKAPI.GetMSGById(MsgId: Integer): TMSG;
begin
  Result := getMSGsById([msgId])[0];
end;

function TAugVKAPI.GetMSGsById(MsgsId: array of Integer): TMSGsArray;
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

function TAugVKAPI.ParseLPMsg(Data: TJSONArray): TMSG;
begin
  Result := TMSG.Create;
  Result.Id := Data.Integers[1];
  Result.Date := -1;

  if Data.Objects[6].IndexOfName('from') = -1 then
  begin
    Result := GetMSGById(Data.Integers[1]);      //пофиксить костыль
    Exit;
  end;

  Result.FromId := GetUser(StrToInt(Data.Objects[6]['from'].AsString));
  Result.PeerId := Data.Integers[3];
  Result.Text := Data.Strings[5];
end;

function TAugVKAPI.ParseMsg(Data: TJSONObject): TMSG;
begin
  Result := TMSG.Create;
  Result.id := data['id'].AsInteger;
  Result.text := data['text'].AsString;
  Result.fromId := getUser(data['from_id'].AsInteger);
  Result.peerId := data['peer_id'].AsInteger;
  Result.date := data['date'].AsInteger;
end;

constructor TAugVKAPI.Create(Token: String);
begin
  requests := TRequests.Create;
  vkapi := TVKAPI.Create;
  vkapi.access_token := token;

  GetChats;
end;

initialization
begin
  DrawedChats := TChatsList.Create;
  ChatsCache := TChatsMap.Create;
  ForceDirectories(IMAGE_PATH);
end;

end.

