unit VkLongpoll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, fpjson, fgl, VkontakteApi;

type
  TEventHandler = procedure(Event: TJSONObject);

  TEventHandlerMap = specialize TFPGMap<Integer, TEventHandler>;

	{ TLongpollThread }

  TLongpollThread = class (TThread)
  private
    // need initilize in create
    EventHandlerMap: TEventHandlerMap;
    VkApi: TVkApi;

    EventTypeToHandle: Integer;
    EventToHandle: TJSONArray;
  protected
    procedure Execute; override;
    procedure ExecuteHandler;
  public
    constructor Create;
    procedure RegisterEventHandler(Handler: TEventHandler);
  end;

implementation

uses fphttpclient, jsonparser;

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure MyThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TLongpollThread }

procedure updateLPInfo();
begin
  lpInfo := TJSONObject(callVkApi('groups.getLongPollServer', ['group_id', config['group_id'].AsString]));
  logWrite('New longpoll info received');
end;

procedure TLongpollThread.Execute;
var
  LpURL, LpKey: String;
  LpTS: Integer;
  LpResult: TJSONObject;
  Event: TJSONEnum;
begin


  while True do
  begin
    LpResult := TJSONObject(GetJSON(TFPHTTPClient.SimpleGet(
      Format('https://%s?act=a_check&key=%s&ts=%s&wait=25&mode=2&version=2', [LpURL, LpKey, LpTS]))));
    for Event in LpResult.Arrays['updates'] do
    begin
      EventTypeToHandle := TJSONArray(Event.Value).Integers[0];
      EventToHandle := TJSONArray(Event.Value);
      Synchronize(@ExecuteHandler);
		end;
	end;
end;

procedure TLongpollThread.ExecuteHandler;
begin
  EventHandlerMap.KeyData[EventTypeToHandle](EventToHandle);
end;

constructor TLongpollThread.Create;
begin
  EventHandlerMap := TEventHandlerMap.Create;
end;

procedure TLongpollThread.RegisterEventHandler(Handler: TEventHandler);
begin
  EventHandlerMap.AddOrSetData(HandlerType, Handler);
end;

end.

