unit AugVKApiThread;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
	Classes, SysUtils, augvkapi, LCLIntf, Contnrs, syncobjs, Utils, Variants;

type
	{ TVKThread }

  TVKThreadCallBack = procedure (Response: TObject; Data: Pointer) of object;

	//TVKQueueItem = class
 //   Method: String;
 //   Args: Array of Variant;
 //   Callback: TVKThreadCallBack;
	//end;

	{ TVKWorkerThread }

  TVKThread = class(TThread)
	private
    //Lock: TCriticalSection;
		//QueryQueue: TQueue;
    AugVK: TAugVKAPI;

		Method: String;
    Args: Array of Variant;

    ResponseData: TObject;
    CallbackData: Pointer;
    Callback: TVKThreadCallBack;

  protected
    procedure Execute; override;
    procedure RunCallback;

  public
		constructor Create;
    procedure AddCallback(ACallback: TVKThreadCallBack);
    procedure AddCallbackData(AData: Pointer);

		procedure GetHistory(PeerId: Integer; Count: Integer; Offset: Integer;
			StartMessageId: Integer);
	end;

//var
//	VKWorkerThread: TVKWorkerThread;

implementation

{ TVKThread }

constructor TVKThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
	//Lock := TCriticalSection.Create;
	//QueryQueue := TQueue.Create;
  AugVK := TAugVKAPI.Create(Config.GetPath(Format('accounts[%d].token',
    [Config.Integers['active_account']])).AsString);
end;

procedure TVKThread.AddCallback(ACallback: TVKThreadCallBack);
begin
  Callback := ACallback;
end;

procedure TVKThread.AddCallbackData(AData: Pointer);
begin
  CallbackData := AData;
end;

procedure TVKThread.Execute;
begin
 // while not Terminated do
	//begin
	//  Lock.Acquire;
	//  try
	//    if QueryQueue.Count > 0 then
 //     	Item := TVKQueueItem(QueryQueue.Pop)
 //     else
 //       continue;
	//  finally
	//    Lock.Release;
	//  end;

    try
	    if Method = 'GetHistory' then
	    begin
	      ResponseData := AugVK.GetHistory(Integer(Args[0]), Integer(Args[1]), Integer(Args[2]), Integer(Args[3]));
	      Synchronize(@RunCallback);
			end;
		except
      on E: Exception do
        WriteLn(DumpExceptionCallStack(E));
		end;

    //Item.Free;
	//end;
end;

procedure TVKThread.RunCallback;
begin
	Callback(ResponseData, CallbackData);
end;

procedure TVKThread.GetHistory(PeerId: Integer; Count: Integer;
	Offset: Integer; StartMessageId: Integer);
//var
//  Item: TVKQueueItem;
begin
  //Item := TVKQueueItem.Create;
  Method := 'GetHistory';
  Args := [PeerId, Count, Offset, StartMessageId];
  Callback := Callback;
	//Lock.Acquire;
  //try
  //  QueryQueue.Push(Pointer(Item));
  //finally
  //  Lock.Release;
  //end;
end;

initialization
begin
  //VKWorkerThread := TVKWorkerThread.Create(True);
  //VKWorkerThread.Start;
end;

end.

