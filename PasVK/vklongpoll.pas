unit VkLongpoll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, fpjson, fgl, VkontakteApi, fprequests;

type
  TEventHandler = procedure(Event: TJSONArray);

  TEventHandlerMap = specialize TFPGMap<Integer, TEventHandler>;

	{ TLongpollThread }

  TLongpollThread = class (TThread)
  private
    // need initilize in create
    EventHandlerMap: TEventHandlerMap;
    VkApi: TVKAPI;

    LpServer, LpKey: String;
    LpTS: Integer;

    EventTypeToHandle: Integer;
    EventToHandle: TJSONArray;
  protected
    procedure Execute; override;
    procedure ExecuteHandler;
    procedure ProcessEvent(Event: TJSONArray); virtual;
  public
    constructor Create(AToken: String);
    procedure UpdateLPInfo;
    procedure RegisterEventHandler(EventType: Integer; Handler: TEventHandler);
  end;

implementation

uses fphttpclient, jsonparser;

procedure TLongpollThread.UpdateLPInfo;
var
  lpInfo: TJSONObject;
begin
  lpInfo := TJSONObject(
              VkApi.call('messages.getLongPollServer',
                TParams.Create
	                .add('lp_version', 3)
              )
            );
  LpServer := lpInfo.Strings['server'];
  LpKey := lpInfo.Strings['key'];
  LpTS := lpInfo.Integers['ts'];

  writeln('New longpoll info received');
end;

procedure TLongpollThread.Execute;
var
  LpResult: TJSONObject;
  Event: TJSONEnum;
begin
  UpdateLPInfo;

  while True do
  begin
    LpResult := TJSONObject(GetJSON(TFPHTTPClient.SimpleGet(
      Format('https://%s?act=a_check&key=%s&ts=%d&wait=25&mode=2&version=3', [LpServer, LpKey, LpTS]))));
    LpTS := LpResult.Integers['ts'];
    for Event in LpResult.Arrays['updates'] do
      ProcessEvent(TJSONArray(Event.Value));
    end;
end;

procedure TLongpollThread.ProcessEvent(Event: TJSONArray);
begin
  EventTypeToHandle := Event.Integers[0];
  EventToHandle := Event;
  writeln(EventTypeToHandle, ' ',  EventToHandle.AsJSON);
  Synchronize(@ExecuteHandler);
end;

procedure TLongpollThread.ExecuteHandler;
var
  Handler: TEventHandler;
begin
  if EventHandlerMap.IndexOf(EventTypeToHandle) <> -1 then
  begin
	  Handler := EventHandlerMap.KeyData[EventTypeToHandle];
	  Handler(EventToHandle);
	end;
end;

constructor TLongpollThread.Create(AToken: String);
begin
  EventHandlerMap := TEventHandlerMap.Create;
  VkApi := TVKAPI.Create(AToken);

  inherited Create(True);
end;

procedure TLongpollThread.RegisterEventHandler(EventType: Integer; Handler: TEventHandler);
begin
  EventHandlerMap.AddOrSetData(EventType, Handler);
end;

end.

