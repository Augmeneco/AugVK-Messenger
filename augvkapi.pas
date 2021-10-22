{%RunFlags MESSAGES+}
unit augvkapi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fprequests, fpjson, jsonparser, fgl,
  vkontakteapi, Graphics;

const
  AVATARS_PATH = 'data/avatars';
  THUMBNAILS_PATH = 'data/thumbnails';
  IMAGE_PATH = 'data/images';


type TMSG = class;
type TUser = class;

type
  TAttachType = (atPhoto, atVideo, atAudio, atDoc, atWall, atMarket, atPoll,
                 atSticker, atGIF, atURL);
  TAttachment = class
    Name: String;
    URL: String;
    Preview: TPicture;
    AttachType: TAttachType;

    destructor Destroy; override;
end;
type TAttachmentsList = specialize TFPGList<TAttachment>;

type TMSGsList = specialize TFPGList<TMSG>;

type
  TMSG = class
    Id: Integer;
    Text: String;
    Date: Integer;
    PeerId: Integer;
    FromId: TUser;
    Reply: TMSGsList;
    Attachments: TAttachmentsList;

    destructor Destroy; override;
end;

type
  TUser = class
    Name: String;
    Id: Integer;
    Image: TPicture;
end;
type TUsersList = specialize TFPGList<TUser>;

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

type TPhotoSize = (pzSmall, pzMedium, pzBig);

type
  TAugVKAPI = class
  private
    Requests: TRequests;
    VKAPI: TVKAPI;

  public
    function LoadPhoto(URL: String; Path: String): TPicture;

  public
    function ParseMsg(Data: TJSONObject): TMSG;
    function ParseLPMsg(Data: TJSONArray): TMSG;
    function ParseUser(Data: TJSONObject): TUser;
    function ParseGroup(Data: TJSONObject): TUser;

    function GetMSGById(MsgId: Integer): TMSG;
    function GetMSGsById(MsgsId: array of Integer): TMSGsList;

    procedure AddUser(User: TUser);
    function GetUser(Id: Integer): TUser;
    function GetUsers(Ids: array of Integer): TUsersList;

    function GetChats(Count: Integer; Offset: Integer = 0): TChatsList;
    function GetAllChats(Offset: Integer = 0): TChatsList;
    function GetChat(Id: Integer): TChat;
    function GetChatsById(Ids: array of Integer): TChatsList;

    function GetHistory(PeerId: Integer; Count: Integer; Offset: Integer; StartMessageId: Integer): TMSGsList;
    function GetHistory(PeerId: Integer; Count: Integer): TMSGsList; overload;
    function GetHistory(PeerId: Integer; Count: Integer; Offset: Integer): TMSGsList; overload;

    function UpdateChatsPosition(PeerId: Integer): TChatsList;
    function GetChatsForDraw: TChatsList;
    function GetChatByIndex(Index: Integer): TChat;

    function GetPhotoURL(Data: TJSONArray; NeededSize: TPhotoSize=pzBig): String;

    function GetAttachmentName(AttachObject: TJSONObject): String; overload;

    procedure SendMessage(Text: String; PeerId: Integer; Reply: Integer; Attachments: TAttachmentsList);
    procedure SendMessage(Text: String; PeerId: Integer); overload;
    procedure SendMessage(Text: String; PeerId: Integer; Reply: Integer); overload;
    procedure SendMessage(Text: String; PeerId: Integer; Attachments: TAttachmentsList); overload;

    constructor Create(Token: String);
end;

implementation

uses
  LazLogger, StrUtils, Utils;

var
  UsersCache: TUsersList;
  ChatsCache: TChatsMap;
  DrawedChats: TChatsList;


destructor TAttachment.Destroy;
begin
  FreeAndNil(Self.Preview);

  inherited;
end;

destructor TMSG.Destroy;
var
  Attachment: TAttachment;
begin
  while Self.Attachments.Count > 0 do
  begin
    try
      Attachment := Self.Attachments[0];
      Self.Attachments.Delete(0);
      FreeAndNil(Attachment);
    finally
    end;
  end;

  FreeAndNil(Self.Attachments);

  inherited;
end;

procedure TAugVKAPI.SendMessage(Text: String; PeerId: Integer); overload;
begin
  SendMessage(Text,PeerId,-1,Nil);
end;

procedure TAugVKAPI.SendMessage(Text: String; PeerId: Integer; Reply: Integer); overload;
begin
  SendMessage(Text,PeerId,Reply,Nil);
end;

procedure TAugVKAPI.SendMessage(Text: String; PeerId: Integer; Attachments: TAttachmentsList); overload;
begin
  SendMessage(Text,PeerId,-1,Attachments);
end;

procedure TAugVKAPI.SendMessage(Text: String; PeerId: Integer; Reply: Integer; Attachments: TAttachmentsList);
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

function TAugVKAPI.UpdateChatsPosition(PeerId: Integer): TChatsList;
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

function TAugVKAPI.GetChatsForDraw: TChatsList;
var
  Chat: TChat;
  I: Integer;
begin
  Result := TChatsList.Create;
  if drawedChats.Count = 0 then
    for I:=0 to ChatsCache.Count-1 do
      DrawedChats.Add(ChatsCache.Data[I]);

  for chat in drawedChats do
  begin
    Result.Add(Chat);
  end;
end;

function TAugVKAPI.GetHistory(PeerId: Integer; Count: Integer): TMSGsList; overload;
begin
  Result := GetHistory(PeerId, Count, 0, -1);
end;

function TAugVKAPI.GetHistory(PeerId: Integer; Count: Integer; Offset: Integer): TMSGsList; overload;
begin
  Result := GetHistory(PeerId, Count, Offset, -1);
end;

function TAugVKAPI.GetHistory(PeerId: Integer; Count: Integer; Offset: Integer; StartMessageId: Integer): TMSGsList;
var
  response: TJSONObject;
  jsonEnum: TJSONEnum;
  items, profilesArray: TJSONArray;
  item: TJSONObject;
  userType: String;
  index: Integer;
  params: TParams;
begin
  Result := TMSGsList.Create;
  params := TParams.Create
    .add('count',count)
    .add('offset',offset)
    .add('peer_id',peerId)
    .add('extended',1);
  if StartMessageId >= 0 then
    params.add('start_message_id',StartMessageId);
  response := TJSONObject(vkapi.call('messages.getHistory', params));

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
    Result.Add(
      parseMsg(TJSONObject(jsonEnum.Value))
    );
  end;

end;

function TAugVKAPI.GetChat(Id: Integer): TChat;
begin
  Result := getChatsById([id])[0];
end;

function TAugVKAPI.GetChatsById(Ids: array of Integer): TChatsList;
var
  response: TJSONArray;
  ChatObject: TJSONObject;
  Chat: TChat;
  jsonEnum: TJSONEnum;
  idsstr: String;
  id, index: Integer;
  ResultVar: TChat;
begin
  idsstr := '';
  Result := TChatsList.Create;
  for id in ids do
  begin
    if ChatsCache.IndexOf(Id) <> -1 then
    begin
      Result.Add(ChatsCache.KeyData[Id]);
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
    ChatObject := TJSONObject(jsonEnum.Value);

    if ChatsCache.IndexOf(ChatObject.Integers['id']) <> -1 then
    begin
      Result.Add(ChatsCache.KeyData[ChatObject.Integers['id']]);
      Continue;
    end;

    ResultVar := TChat.Create;
    ResultVar.Name := ChatObject.Strings['title'];
    ResultVar.Form := ChatObject.Strings['type'];
    ResultVar.Id := ChatObject.Integers['id'];
    ResultVar.PreviewMsg := Nil;
    ResultVar.Image := LoadPhoto(
      ChatObject['photo_50'].AsString,
      Format(AVATARS_PATH+'/%d.jpg',[2000000000+ResultVar.id])
    );

    ChatsCache.Add(ResultVar.Id, ResultVar);
  end;

end;

function TAugVKAPI.LoadPhoto(URL: String; Path: String): TPicture;
var
  TmpStream: TFileStream;
begin
  DebugLn('Loading photo '+Path);
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

function TAugVKAPI.GetChats(Count: Integer; Offset: Integer = 0): TChatsList;
var
  chatsArray, profilesArray: TJSONArray;
  chatObject, profileObject, response: TJSONObject;
  jsonEnum: TJSONEnum;
  index: Integer;
  previewMsg: TMSG;
  userType: String;
  Chat: TChat;
  ResultVar: TChat;
begin
  Result := TChatsList.Create;
  if Count > 200 then Count := 200;

  response := TJSONObject(vkapi.call(
     'messages.getConversations',
     TParams.Create
        .add('offset', offset)
        .add('count', Count)
        .add('extended', 1)
  ));

  DebugLn(Format('Loading chats %d / %d',[offset,response['count'].AsInteger]));

  chatsArray := response.Arrays['items'];
  if chatsArray.Count = 0 then Exit;

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
    ChatObject := TJSONObject(jsonEnum.Value);

    if ChatsCache.IndexOf(ChatObject.GetPath('conversation.peer.id').AsInteger) <> -1 then
    begin
      Result.Add(ChatsCache.KeyData[ChatObject.GetPath('conversation.peer.id').AsInteger]);
      Continue;
    end;

    ResultVar := TChat.Create;
    ResultVar.id := chatObject.GetPath('conversation.peer.id').AsInteger;
    ResultVar.previewMsg := parseMsg(chatObject.Objects['last_message']);

    if chatObject.GetPath('conversation.peer.type').AsString = 'chat' then
    begin
      ResultVar.name := chatObject.GetPath('conversation.chat_settings.title').AsString;

      if ChatObject.Objects['conversation'].
                    Objects['chat_settings'].IndexOfName('photo') = -1 then
      begin
        ResultVar.Image := ResultVar.PreviewMsg.FromId.Image;
      end
      else
        ResultVar.Image := LoadPhoto(
          ChatObject.GetPath('conversation.chat_settings.photo.photo_50').AsString,
          Format(AVATARS_PATH+'/%d.jpg',[ResultVar.id])
        );
    end
    else
    begin
      ResultVar.name := GetUser(ResultVar.id).name;
      ResultVar.Image := GetUser(ResultVar.Id).Image;
    end;
    Result.Add(ResultVar);

    ChatsCache.Add(ResultVar.Id, ResultVar);
  end;
end;

function TAugVKAPI.GetAllChats(Offset: Integer = 0): TChatsList;
var
  chatsArray, profilesArray: TJSONArray;
  chatObject, profileObject, response: TJSONObject;
  jsonEnum: TJSONEnum;
  index: Integer;
  previewMsg: TMSG;
  userType: String;
  Chat: TChat;
  ResultVar: TChat;
begin
  Result := TChatsList.Create;

  while True do
  begin
    response := TJSONObject(vkapi.call(
       'messages.getConversations',
       TParams.Create
          .add('offset', offset)
          .add('count', 200)
          .add('extended', 1)
    ));

    DebugLn(Format('Loading chats %d / %d',[offset,response['count'].AsInteger]));

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
      ChatObject := TJSONObject(jsonEnum.Value);

      if ChatsCache.IndexOf(ChatObject.GetPath('conversation.peer.id').AsInteger) <> -1 then
      begin
        Result.Add(ChatsCache.KeyData[ChatObject.GetPath('conversation.peer.id').AsInteger]);
        Continue;
      end;

      ResultVar := TChat.Create;
      ResultVar.id := chatObject.GetPath('conversation.peer.id').AsInteger;
      ResultVar.previewMsg := parseMsg(chatObject.Objects['last_message']);

      if chatObject.GetPath('conversation.peer.type').AsString = 'chat' then
      begin
        ResultVar.name := chatObject.GetPath('conversation.chat_settings.title').AsString;

        if ChatObject.Objects['conversation'].
                      Objects['chat_settings'].IndexOfName('photo') = -1 then
        begin
          ResultVar.Image := ResultVar.PreviewMsg.FromId.Image;
        end
        else
          ResultVar.Image := LoadPhoto(
            ChatObject.GetPath('conversation.chat_settings.photo.photo_50').AsString,
            Format(AVATARS_PATH+'/%d.jpg',[ResultVar.id])
          );
      end
      else
      begin
        ResultVar.name := GetUser(ResultVar.id).name;
        ResultVar.Image := GetUser(ResultVar.Id).Image;
      end;

      ChatsCache.Add(ResultVar.Id, ResultVar);
      Result.Add(ResultVar);
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

  Result.Image := LoadPhoto(
    data['photo_50'].AsString,
    Format(AVATARS_PATH+'/%d.jpg',[data['id'].AsInteger])
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

    Result.Image := LoadPhoto(
      data['photo_50'].AsString,
      Format(AVATARS_PATH+'/%d.jpg',[data['id'].AsInteger*-1])
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
  UsersCache.Add(User);
end;

function TAugVKAPI.GetUser(id: Integer): TUser;
begin
  Result := GetUsers([id])[0];
end;

function TAugVKAPI.GetUsers(Ids: array of Integer): TUsersList;
var
  userIds: String;
  id, index: Integer;
  user: TUser;
  jsonEnum: TJSONEnum;
  userObject: TJSONObject;
  response: TJSONArray;
  exists: Boolean;
  Params: TParams;
begin
  userIds := '';
  Result := TUsersList.Create;

  for id in ids do
  begin
    exists := False;
    for user in usersCache do
    begin
       if user.id = id then
       begin
         Result.Add(User);
         exists := True;
         break;
       end;
    end;
    if not exists then
      userIds += IntToStr(id)+','
  end;

  if userIds <> '' then
  begin
    Params := TParams.Create;
    Params.Add('fields','photo_50, last_seen');

    if UserIds <> '-1,' then
      Params.Add('user_ids',UserIds);

    response := TJSONArray(vkapi.call(
      'users.get',
      Params
    ));

    for jsonEnum in response do
    begin
      userObject := TJSONObject(jsonEnum.Value);
      Result.Add(ParseUser(UserObject));
    end;
  end;
end;

function TAugVKAPI.GetMSGById(MsgId: Integer): TMSG;
begin
  Result := getMSGsById([msgId])[0];
end;

function TAugVKAPI.GetMSGsById(MsgsId: array of Integer): TMSGsList;
var
  ids: String;
  id, index: Integer;
  jsonEnum: TJSONEnum;
  response, msgObject: TJSONObject;
begin
  Result := TMSGsList.Create;
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
    Result.Add(
      parseMsg(msgObject)
    );
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

function SortSizes(Item1, Item2: Pointer): Integer;
const
  Sizes: Array [0..9] of String = ('s','m','x','o','p','q','r','y','z','w');
var
  S1, S2: Integer;
begin
  S1 := IndexStr(TJSONObject(Item1).Strings['type'], Sizes);
  S2 := IndexStr(TJSONObject(Item2).Strings['type'], Sizes);
  if      S1 > S2 then Result := 1
  else if S1 = S2 then Result := 0
  else if S1 < S2 then Result := -1;
end;

function TAugVKAPI.GetPhotoURL(Data: TJSONArray; NeededSize: TPhotoSize=pzBig): String;
var
  PhotoObject: TJSONObject;
  JSONEnum: TJSONEnum;
  MSize: Integer;
  ResultObject: TJSONObject;
begin
  //if MaxSize then
  //  MSize := 0
  //else
  //  MSize := MaxInt;
  //
  //for JSONEnum in Data do
  //begin
  //  PhotoObject := TJSONObject(JSONEnum.Value);
  //  if MaxSize then
  //  begin
  //    if (PhotoObject['height'].AsInteger * PhotoObject['width'].AsInteger) >= MSize then
  //    begin
  //      MSize := PhotoObject['height'].AsInteger * PhotoObject['width'].AsInteger;
  //      ResultObject := PhotoObject;
  //    end
  //  end
  //  else
  //    if (PhotoObject['height'].AsInteger * PhotoObject['width'].AsInteger) <= MSize then
  //    begin
  //      MSize := PhotoObject['height'].AsInteger * PhotoObject['width'].AsInteger;
  //      ResultObject := PhotoObject;
  //    end;
  //end;

  Data.Sort(@SortSizes);

  case NeededSize of
    pzSmall: ResultObject := Data.Objects[0];
    pzBig: ResultObject := Data.Objects[Data.Count-1];
    pzMedium: ResultObject := Data.Objects[Trunc(Data.Count/2)];
  end;

  if ResultObject.IndexOfName('url') <> -1 then
    Result := ResultObject['url'].AsString
  else
    Result := ResultObject['src'].AsString;
end;

function TAugVKAPI.GetAttachmentName(AttachObject: TJSONObject): String;
var
  AttachType: String;
begin
  AttachType := AttachObject['type'].AsString;

  Result := AttachType + AttachObject.GetPath(AttachType+'.owner_id').AsString+
                         '_' + AttachObject.GetPath(AttachType+'.id').AsString;
end;

function TAugVKAPI.ParseMsg(Data: TJSONObject): TMSG;
var
  Attachment: TAttachment;
  AttachmentJSON: TJSONObject;
  JSONEnum: TJSONEnum;
  Sizes: TJSONArray;
  PhotoURL: String;
begin
  Result := TMSG.Create;
  Result.id := data['id'].AsInteger;
  Result.text := data['text'].AsString;
  Result.fromId := getUser(data['from_id'].AsInteger);
  Result.peerId := data['peer_id'].AsInteger;
  Result.date := data['date'].AsInteger;

  Result.Attachments := TAttachmentsList.Create;
  if Data.Arrays['attachments'].Count <> -1 then
  begin
    for JSONEnum in Data.Arrays['attachments'] do
    begin
      AttachmentJSON := TJSONObject(JSONEnum.Value);

      if AttachmentJSON['type'].AsString = 'photo' then
      begin
        Sizes := TJSONArray(AttachmentJSON.GetPath('photo.sizes'));

        Attachment := TAttachment.Create;
        Attachment.URL := GetPhotoURL(Sizes);
        Attachment.Name := GetAttachmentName(AttachmentJSON);

        PhotoURL := GetPhotoURL(Sizes, pzMedium);

        Attachment.Preview := LoadPhoto(
          PhotoURL,
          THUMBNAILS_PATH+'/'+GetAttachmentName(AttachmentJSON)+'.jpg'
        );
        Attachment.AttachType := TAttachType.atPhoto;

        Result.Attachments.Add(Attachment);
      end;

      if AttachmentJSON['type'].AsString = 'doc' then
      begin
        Attachment := TAttachment.Create;

        if AttachmentJSON.Objects['doc'].IndexOfName('preview') = -1 then
        begin
          Attachment.Preview := LoadPhoto(
            'https://sun9-57.userapi.com/impg/8eSBy-qs5hGTfda0rMXDzNdsY3TJbKmylFABRg/PB3ANc3ngBg.jpg?size=60x60&quality=96&sign=b2c1b375366032f3dfbc0b947dd90ffe&type=album',
            THUMBNAILS_PATH+'/doc.jpg'
          );
        end
        else
        begin
          Sizes := TJSONArray(AttachmentJSON.GetPath('doc.preview.photo.sizes'));
          Attachment.Preview := LoadPhoto(
            GetPhotoURL(Sizes, pzSmall),
            THUMBNAILS_PATH+'/'+GetAttachmentName(AttachmentJSON)+'.jpg'
          );
          Attachment.URL := GetPhotoURL(Sizes);
        end;

        Attachment.Name := AttachmentJSON.GetPath('doc.title').AsString;
        Attachment.URL := AttachmentJSON.GetPath('doc.url').AsString;
        Attachment.AttachType := TAttachType.atDoc;

        if AttachmentJSON.GetPath('doc.ext').AsString = 'gif' then
          Attachment.AttachType := TAttachType.atGIF;

        Result.Attachments.Add(Attachment);
      end;


    end;
  end;
end;

constructor TAugVKAPI.Create(Token: String);
begin
  Requests := TRequests.Create;
  Requests.AddHeader(
    'User-Agent',
    'KateMobileAndroid/48.2 lite-433 (Android 8.9.0; SDK 16; armeabi-v7a; LGE LG-E615; ru)'
  );

  VKAPI := TVKAPI.Create;
  VKAPI.Access_Token := Token;
  //GetChats;
end;

initialization
begin
  UsersCache := TUsersList.Create;
  DrawedChats := TChatsList.Create;
  ChatsCache := TChatsMap.Create;

  ForceDirectories(AVATARS_PATH);
  ForceDirectories(THUMBNAILS_PATH);
  ForceDirectories(IMAGE_PATH);
end;

end.

