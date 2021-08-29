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
    function GetCache(PeerId: Integer): TMSGsArray;
    function GetCache(PeerId: Integer; Count: Integer): TMSGsArray; overload;
    constructor Create(AToken: String);
end;

implementation
var
  CachedMsgs: TCachedMsgs;
  Augvk: TAugVKAPI;

{ TCachedLongpoll }

function TCachedLongpoll.GetCache(PeerId: Integer; Count: Integer): TMSGsArray; overload;
var
  Msgs: TMSGsArray;
  Count_: Integer;
  I: Integer;
begin
  Count_ := 0;

  Msgs := GetCache(PeerId);

  for I:=0 to Length(Msgs) do
  begin
    if Count_ = Count then Break;

    SetLength(Result,Length(Result)+1);
    Result[Length(Result)-1] := Msgs[I];
    Count_ += 1;
  end;
end;

function TCachedLongpoll.GetCache(PeerId: Integer): TMSGsArray;
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

{Миша сунь это блин в поток лп}
procedure TCachedLongpoll.ProcessEvent(Event: TJSONArray);
var
  Msg: TMSG;
  Msgs: TMSGsArray;
begin
  if Event.Integers[0] <> 4 then Exit;

  if CachedMsgs.IndexOf(Event.Integers[3]) = -1 then
  begin
    CachedMsgs.Add(
      Event.Integers[3],
      Augvk.GetHistory(Event.Integers[3],100)
    );
    Msgs := CachedMsgs.KeyData[Event.Integers[3]];
    Msg := Msgs[0];
  end
  else
  begin

    Msg := AugVK.ParseLPMsg(Event);//Augvk.GetMSGById(Event.Integers[1]);   //вот из-за этого мелкофризы появляются
    Msgs := CachedMsgs.KeyData[Msg.PeerId];
  end;

  Insert([Msg],Msgs,0);
  CachedMsgs.AddOrSetData(Msg.PeerId,Msgs);

  inherited ProcessEvent(Event);
end;

end.

