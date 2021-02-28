program test;

uses augvkapi, fprequests, fpjson;
var
  vkapi: TAugVKAPI;

begin
  vkapi := TAugVKAPI.Create;

  writeln('Ur name: '+TJSONArray(vkapi.call('users.get',
     TParams.Create
  )).GetPath('[0].first_name').AsString);

  writeln(vkapi.getChats[0].name);


end.

