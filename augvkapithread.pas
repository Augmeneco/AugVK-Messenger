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

    ResponseData: TObject;
    CallbackData: Pointer;
    Callback: TVKThreadCallBack;

  protected
    procedure Execute; override;
    procedure RunCallback;

  public
		constructor Create;
    function AddCallback(ACallback: TVKThreadCallBack): TVKThread;
    function AddCallbackData(AData: Pointer): TVKThread;

		function GetHistory(PeerId: Integer; Count: Integer; Offset: Integer;
			StartMessageId: Integer): TVKThread;
	end;

implementation

{ TVKThread }

procedure TVKThread.Execute;
begin
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

function TVKThread.AddCallback(ACallback: TVKThreadCallBack): TVKThread;
begin
  Callback := ACallback;
  Result := Self;
end;

function TVKThread.AddCallbackData(AData: Pointer): TVKThread;
begin
  CallbackData := AData;
  Result := Self;
end;

function TVKThread.GetHistory(PeerId: Integer; Count: Integer;
	Offset: Integer; StartMessageId: Integer): TVKThread;
begin
  Method := 'GetHistory';
  Args := [PeerId, Count, Offset, StartMessageId];
  Callback := Callback;
  Result := Self;
end;

end.

