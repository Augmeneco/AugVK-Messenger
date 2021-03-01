program test;

uses augvkapi, fprequests, fpjson;
var
  vkapi: TAugVKAPI;

begin
  vkapi := TAugVKAPI.Create;

  writeln(vkapi.getChats[0].name);


end.

