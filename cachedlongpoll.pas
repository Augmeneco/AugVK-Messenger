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
  for I:=Length(Msgs)-1 downto 0 do
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

procedure TCachedLongpoll.ProcessEvent(Event: TJSONArray);
var
  Msg: TMSG;
  Msgs: TMSGsArray;
begin
  //writeln(Event.FormatJSON());

  if Event.Integers[0] <> 4 then Exit;

  Msg := Augvk.GetMSGById(Event.Integers[1]);

  if CachedMsgs.IndexOf(Msg.PeerId) = -1 then
  begin
    CachedMsgs.Add(
      Msg.PeerId,
      Augvk.GetHistory(Msg.PeerId,100)
    );
  end;

  Msgs := CachedMsgs.KeyData[Msg.PeerId];
  Insert([Msg],Msgs,0);
  CachedMsgs.AddOrSetData(Msg.PeerId,Msgs);

  writeln(msg.text);

  inherited ProcessEvent(Event);
end;

end.

