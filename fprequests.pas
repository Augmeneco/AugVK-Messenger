unit fprequests;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, openssl, jsonparser, fpjson, opensslsockets;

type
  TResponse = class (TInterfacedObject)
    Text: String;
    Code: Integer;
    Data: TBytes;

    function JSON(): TJSONData;
end;

type
  TParams = class (TInterfacedObject)
  private
    Params: array of array of String;
  public
    function Add(Key: String; Value: String): TParams; overload;
    function Add(Key: String; Value: Integer): TParams; overload;
    function BuildURL(): String;
    function BuildPost(): TStringList;
end;

type
  TRequests = class (TInterfacedObject)
  private
    Client: TFPHTTPClient;
  public
    AllowedCodes: Array of Integer;
    constructor Create;
    function Get(URL: String; Params: TParams): TResponse;
    function Get(URL: String): TResponse; overload;
    procedure Get(URL: String; Stream: TStream); overload;
    function Post(URL: String; Params: TParams): TResponse;
    function AddHeader(Key: String; Value: String): TRequests;
end;

function URLEncode(URL: String): String;

implementation

uses
  LazLogger;

var
  Encoder: TEncoding;

function TResponse.JSON(): TJSONData;
begin
  Result := GetJSON(text);
end;

procedure TRequests.Get(URL: String; Stream: TStream); overload;
begin
  Client.Get(URL,Stream);
end;

function TRequests.Get(URL: String): TResponse; overload;
begin
  Result := Get(URL, TParams.Create);
end;

function TRequests.Get(URL: String; Params: TParams): TResponse;
var
  Response: TResponse;
  BS: TBytesStream;
begin
  BS := TBytesStream.Create;
  Response := TResponse.Create;
  Client.HTTPMethod(
     'GET',
     Format('%s?%s',[URL, Params.BuildUrl()]),
     BS,
     AllowedCodes
  );
  Response.Text := Encoder.GetString(BS.Bytes);
  Response.Code := Client.ResponseStatusCode;
  Response.Data := BS.Bytes;

  Result := Response;

end;

function TRequests.Post(URL: String; Params: TParams): TResponse;
var
  Response: TResponse;
  BS: TBytesStream;
begin
  BS := TBytesStream.Create;
  Response := TResponse.Create;

  Client.FormPost(
     URL,
     Params.BuildPost(),
     BS
  );
  Response.Text := Encoder.GetString(BS.Bytes);
  Response.Code := Client.ResponseStatusCode;
  Response.Data := BS.Bytes;

  Result := Response;

end;

constructor TRequests.Create;
begin
  Client := TFPHTTPClient.Create(nil);
  AllowedCodes := [200];
end;

function TRequests.AddHeader(Key: String; Value: String): TRequests;
begin
  Client.AddHeader(Key, Value);
  Result := Self;
end;

function TParams.BuildUrl(): String;
var
  KV: array of String;
begin
  for KV in Params do
        Result += Format('%s=%s&',[
               URLEncode(KV[0]),
               URLEncode(KV[1])
        ]);
end;

function TParams.BuildPost(): TStringList;
var
  KV: array of String;
begin
  Result := TStringList.Create;
  for KV in params do
        Result.Add(Format('%s=%s',[
               KV[0],KV[1]
        ]));
end;

function TParams.Add(Key: String; Value: String): TParams; overload;
begin
  SetLength(Params,Length(Params)+1,2);
  Params[Length(Params)-1] := [Key,Value];

  Result := Self;
end;

function TParams.Add(Key: String; Value: Integer): TParams; overload;
begin
  SetLength(Params,Length(Params)+1,2);
  Params[Length(Params)-1] := [Key,IntToStr(Value)];

  Result := Self;
end;

function URLEncode(URL: String): String;
var
  x: integer;
  sBuff: string;
const
  SafeMask = ['A'..'Z', '0'..'9', 'a'..'z', '*', '@', '.', '_', '-', ','];
begin
  sBuff := '';

  for x := 1 to Length(url) do
  begin
    if url[x] in SafeMask then
    begin
      sBuff := sBuff + url[x];
    end
    else if url[x] = ' ' then
    begin
      sBuff := sBuff + '+';
    end
    else
    begin
      sBuff := sBuff + '%' + IntToHex(Ord(url[x]), 2);
    end;
  end;

  Result := sBuff;
end;

initialization
begin
  Encoder := TEncoding.UTF8;
end;

end.

