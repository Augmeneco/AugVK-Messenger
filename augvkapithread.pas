unit AugVKApiThread;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
	Classes, SysUtils, augvkapi, LCLIntf, Contnrs, syncobjs, Utils, Variants,
  ConfigUtils;

type
	{ TVKThread }

  TVKThreadCallBack = procedure (Response: TObject; Data: Pointer) of object;

  TVKThread = class(TThread)
	private
    AugVK: TAugVKAPI;

		Method: String;
    Args: Array of Variant;

    // Должна быть очищена после выполнения всех коллбеков
    ResponseData: TObject;
    // Должна быть очищена в коллбеке
    CallbackData: Pointer;
    Callback: TVKThreadCallBack;
    ThreadedCallback: TVKThreadCallBack;

  protected
    procedure Execute; override;
    procedure RunCallback;

  public
		constructor Create;
    destructor Free;
    function AddCallback(ACallback: TVKThreadCallBack): TVKThread;
    function AddThreadedCallback(AThreadedCallback: TVKThreadCallBack): TVKThread;
    function AddCallbackData(AData: Pointer): TVKThread;

		function GetHistory(PeerId, Count, Offset, StartMessageId: Integer): TVKThread;
    function GetChats(Count, Offset: Integer): TVKThread;
	end;

implementation

uses
  LazLogger;

{ TVKThread }

procedure TVKThread.Execute;
begin
  try
	  case Method of
      'GetHistory':
	      ResponseData := AugVK.GetHistory(Integer(Args[0]), Integer(Args[1]), Integer(Args[2]), Integer(Args[3]));
      'GetChats':
        ResponseData := AugVK.GetChats(Integer(Args[0]), Integer(Args[1]));
    end;

    if Assigned(ThreadedCallback) then
      ThreadedCallback(ResponseData, CallbackData);

    if Assigned(Callback) then
	    Synchronize(@RunCallback);

    if Assigned(ResponseData) then
      ResponseData.Free;
	except
    on E: Exception do
      DebugLn(DumpExceptionCallStack(E));
	end;
end;

procedure TVKThread.RunCallback;
begin
	Callback(ResponseData, CallbackData);
end;

constructor TVKThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  AugVK := TAugVKAPI.Create(Config.GetPath(Format('accounts[%d].token',
    [Config.Integers['active_account']])).AsString);
end;

destructor TVKThread.Free;
begin
  AugVK.Free;
end;

function TVKThread.AddCallback(ACallback: TVKThreadCallBack): TVKThread;
begin
  Callback := ACallback;
  Result := Self;
end;

function TVKThread.AddThreadedCallback(AThreadedCallback: TVKThreadCallBack): TVKThread;
begin
  ThreadedCallback := AThreadedCallback;
  Result := Self;
end;

function TVKThread.AddCallbackData(AData: Pointer): TVKThread;
begin
  CallbackData := AData;
  Result := Self;
end;

function TVKThread.GetHistory(PeerId, Count, Offset, StartMessageId: Integer): TVKThread;
begin
  Method := 'GetHistory';
  Args := [PeerId, Count, Offset, StartMessageId];
  Result := Self;
end;

function TVKThread.GetChats(Count, Offset: Integer): TVKThread;
begin
  Method := 'GetChats';
  Args := [Count, Offset];
  Result := Self;
end;

end.

