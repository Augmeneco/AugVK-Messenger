unit AugVKApiThread;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, augvkapi, LCLIntf, Contnrs, syncobjs, Utils, Variants;

type
	{ TVKThread }

  TVKThreadCallBack = procedure (Response: Variant);

	TVKQueueItem = class
    Method: String;
    Args: Array of Variant;
    Callback: TVKThreadCallBack;
	end;

	{ TVKWorkerThread }

  TVKWorkerThread = class(TThread)
	private
    Lock: TCriticalSection;
		QueryQueue: TQueue;
    AugVK: TAugVKAPI;

    ResponseData: Variant;
    CallbackToRun: TVKThreadCallBack;

  protected
    procedure Execute; override;
    procedure RunCallback;

  public
		constructor Create(CreateSuspended: Boolean);
		procedure GetHistory(PeerId: Integer; Count: Integer; Offset: Integer;
			StartMessageId: Integer; Callback: Pointer);
	end;

var
	VKWorkerThread: TVKWorkerThread;

implementation

{ TVKThread }

constructor TVKWorkerThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
	Lock := TCriticalSection.Create;
	QueryQueue := TQueue.Create;
  AugVK := TAugVKAPI.Create(Config.GetPath(Format('accounts[%d].token',
    [Config.Integers['active_account']])).AsString);
end;

procedure TVKWorkerThread.Execute;
var
  Item: TVKQueueItem;
  arr: TMSGsArray;
begin
  while not Terminated do
	begin
	  Lock.Acquire;
	  try
	    if QueryQueue.Count > 0 then
      	Item := TVKQueueItem(QueryQueue.Pop)
      else
        continue;
	  finally
	    Lock.Release;
	  end;

    try
	    if Item.Method = 'GetHistory' then
	    begin
        Aug
	      arr := AugVK.GetHistory(Integer(Item.Args[0]), Integer(Item.Args[1]), Integer(Item.Args[2]), Integer(Item.Args[3]));
        DynArrayToVariant(ResponseData, Pointer(arr), TypeInfo(TMSGsArray));
	      CallbackToRun := Item.Callback;
	      Synchronize(@RunCallback);
			end;
		except
      on E: Exception do
        WriteLn(DumpExceptionCallStack(E));
		end;

    Item.Free;
	end;
end;

procedure TVKWorkerThread.RunCallback;
begin
	CallbackToRun(ResponseData);
end;

procedure TVKWorkerThread.GetHistory(PeerId: Integer; Count: Integer;
	Offset: Integer; StartMessageId: Integer; Callback: Pointer);
var
  Item: TVKQueueItem;
begin
  Item := TVKQueueItem.Create;
  Item.Method := 'GetHistory';
  Item.Args := [PeerId, Count, Offset, StartMessageId];
  writeln(Integer(Item.Args[0]));
  Item.Callback := TVKThreadCallBack(Callback);
	Lock.Acquire;
  try
    QueryQueue.Push(Pointer(Item));
  finally
    Lock.Release;
  end;
end;

initialization
begin
  VKWorkerThread := TVKWorkerThread.Create(True);
  VKWorkerThread.Start;
end;

end.

