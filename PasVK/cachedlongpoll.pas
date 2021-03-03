unit CachedLongpoll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VkLongpoll, fpjson, jsonparser, augvkapi;

type

{ TCachedLongpoll }

  TCachedLongpoll = class(TLongpollThread)
  protected
    procedure ProcessEvent(Event: TJSONArray); override;
end;

implementation
var
  cachedMsgs: TMSGsArray;

{ TCachedLongpoll }

procedure TCachedLongpoll.ProcessEvent(Event: TJSONArray);
var
  msg: TMSG;
begin
  //тут твой кеширующий говнокод
  //щас сосать заставлю тебя за гавнакод

  //writeln(Event.FormatJSON());
  //msg := augvk.getMSGById(Event.Integers[3]);
  //writeln(msg.fromId.name);


  inherited ProcessEvent(Event);
end;

end.

