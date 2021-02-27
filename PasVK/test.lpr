program test;

uses pasvk, fprequests, fpjson;
var
  vkapi: TVKAPI;

begin
  vkapi := TVKAPI.Create;

  writeln(TJSONArray(vkapi.call('users.get',
     TParams.Create
       .add('user_ids','1')
  )).GetPath('[0].first_name').AsString);

end.

