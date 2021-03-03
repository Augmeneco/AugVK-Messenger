program test;

uses augvkapi, fprequests, fpjson;
var
  vkapi: TAugVKAPI;
  chats: TChatsArray;
  chat: TChat;

begin
  vkapi := TAugVKAPI.Create;

  chats := vkapi.getChats;
  writeln('Список чатов: '+LineEnding);

  for chat in chats do
    writeln(chat.name);


end.

