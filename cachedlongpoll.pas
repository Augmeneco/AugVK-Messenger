unit CachedLongpoll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VkLongpoll, fpjson, jsonparser, augvkapi, fgl;

type
  TCachedMsgs = specialize TFPGMap<Integer, TMSGsList>;

type

{ TCachedLongpoll }

  TCachedLongpoll = class(TLongpollThread)
  protected
    procedure ProcessEvent(Event: TJSONArray); override;
  public
    function GetCache(PeerId: Integer): TMSGsList;
    function GetCache(PeerId: Integer; Count: Integer): TMSGsList; overload;
    constructor Create(AToken: String);
end;

implementation
var
  CachedMsgs: TCachedMsgs;
  Augvk: TAugVKAPI;

{ TCachedLongpoll }

function TCachedLongpoll.GetCache(PeerId: Integer; Count: Integer): TMSGsList; overload;
var
  Msgs: TMSGsList;
  Msg: TMSG;
  Count_: Integer;
  I: Integer;
begin
  Count_ := 0;
  Result := TMSGsList.Create;

  Msgs := GetCache(PeerId);

  for Msg in Msgs do
  begin
    if Count_ = Count then Break;

    Result.Add(Msg);
    Count_ += 1;
  end;
end;

function TCachedLongpoll.GetCache(PeerId: Integer): TMSGsList;
begin
  if CachedMsgs.IndexOf(PeerId) = -1 then
    CachedMsgs.Add(
      PeerId,
      Augvk.GetHistory(PeerId,100)
    );

  Result := CachedMsgs.KeyData[PeerId];
end;

constructor TCachedLongpoll.Create(AToken: String);
begin
  Augvk := TAugVKAPI.Create(AToken);
  CachedMsgs := TCachedMsgs.Create;

  inherited Create(AToken);
end;

procedure TCachedLongpoll.ProcessEvent(Event: TJSONArray);
var
  Msg: TMSG;
  Msgs: TMSGsList;
begin
  if Event.Integers[0] <> 4 then Exit;

  if CachedMsgs.IndexOf(Event.Integers[3]) = -1 then
  begin
    CachedMsgs.Add(
      Event.Integers[3],
      Augvk.GetHistory(Event.Integers[3],100)
    );
    Msgs := CachedMsgs.KeyData[Event.Integers[3]];
    Msg := Msgs.Items[0];   // зачем эта переменная тут? оставлю как есть
  end
  else
  begin
    Msg := {AugVK.ParseLPMsg(Event);}Augvk.GetMSGById(Event.Integers[1]);   //вот из-за этого мелкофризы появляются
    Msgs := CachedMsgs.KeyData[Msg.PeerId];
  end;

  Msgs.Insert(0, Msg);
  CachedMsgs.AddOrSetData(Msg.PeerId,Msgs);

  inherited ProcessEvent(Event);
end;

end.

