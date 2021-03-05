unit CachedLongpoll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VkLongpoll, fpjson, jsonparser, augvkapi, fgl;

type
  TCachedMsgs = specialize TFPGMap<Integer, TMSGsArray>;

type

{ TCachedLongpoll }

  TCachedLongpoll = class(TLongpollThread)
  protected
    procedure ProcessEvent(Event: TJSONArray); override;
  public
    constructor Create(AToken: String);
end;

implementation
var
  cachedMsgs: TCachedMsgs;
  augvk: TAugVKAPI;

{ TCachedLongpoll }

constructor TCachedLongpoll.Create(AToken: String);
begin
  augvk := TAugVKAPI.Create(AToken);
  cachedMsgs := TCachedMsgs.Create;

  inherited Create(AToken);
end;

procedure TCachedLongpoll.ProcessEvent(Event: TJSONArray);
var
  msg: TMSG;
  msgs: TMSGsArray;
begin
  //тут твой кеширующий говнокод
  //щас сосать заставлю тебя за гавнакод

  //writeln(Event.FormatJSON());
  if Event.Integers[0] <> 4 then Exit;

  msg := augvk.getMSGById(Event.Integers[1]);
  if cachedMsgs.IndexOf(msg.peerId) = -1 then
  begin
    cachedMsgs.Add(
      msg.peerId,
      augvk.getHistory(msg.peerId,30)
    );
  end;
  msgs := cachedMsgs.KeyData[msg.peerId];
  SetLength(msgs,Length(msgs)+1);
  msgs[Length(msgs)-1] := msg;
  cachedMsgs.AddOrSetData(msg.peerId,msgs);

  msgs := cachedMsgs.KeyData[msg.peerId];


  inherited ProcessEvent(Event);
end;

end.

