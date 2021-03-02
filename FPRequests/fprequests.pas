unit fprequests;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, openssl, jsonparser, fpjson, opensslsockets;

type
  TResponse = class (TInterfacedObject)
    text: String;
    code: Integer;
    data: array of Byte;

    function json(): TJSONData;
end;

type
  TParams = class
  private
    params: array of array of String;
  public
    function add(key: String; value: String): TParams; overload;
    function add(key: String; value: Integer): TParams; overload;
    function buildUrl(): String;
    function buildPost(): TStringList;
end;

type
  TRequests = class
  private
    client: TFPHTTPClient;
  public
    constructor Create;
    function get(URL: String; params: TParams): TResponse;
    function post(URL: String; params: TParams): TResponse;
end;

function URLEncode(URL: String): String;

implementation
var
  encoder: TEncoding;

function TResponse.json(): TJSONData;
begin
  Result := GetJSON(text);
end;

function TRequests.get(URL: String; params: TParams): TResponse;
var
  response: TResponse;
  bs: TBytesStream;
begin
  bs := TBytesStream.Create;
  response := TResponse.Create;
  client.Get(
     Format('%s?%s',[url, params.buildUrl()]),
     bs
  );
  response.text := encoder.GetString(bs.Bytes);
  response.code := client.ResponseStatusCode;
  response.data := bs.Bytes;

  Result := response;

end;

function TRequests.post(URL: String; params: TParams): TResponse;
var
  response: TResponse;
  bs: TBytesStream;
begin
  bs := TBytesStream.Create;
  response := TResponse.Create;
  client.FormPost(
     url,
     params.buildPost(),
     bs
  );
  response.text := encoder.GetString(bs.Bytes);
  response.code := client.ResponseStatusCode;
  response.data := bs.Bytes;

  Result := response;

end;

constructor TRequests.Create;
begin
  client := TFPHTTPClient.Create(nil);
end;

function TParams.buildUrl(): String;
var
  kv: array of String;
begin
  for kv in params do
        Result += Format('%s=%s&',[
               URLEncode(kv[0]),
               URLEncode(kv[1])
        ]);
end;

function TParams.buildPost(): TStringList;
var
  kv: array of String;
begin
  Result := TStringList.Create;
  for kv in params do
        Result.Add(Format('%s=%s',[
               URLEncode(kv[0]),
               URLEncode(kv[1])
        ]));
end;

function TParams.add(key: String; value: String): TParams; overload;
begin
  SetLength(params,Length(params)+1,2);
  params[Length(params)-1] := [key,value];

  Result := Self;
end;

function TParams.add(key: String; value: Integer): TParams; overload;
begin
  SetLength(params,Length(params)+1,2);
  params[Length(params)-1] := [key,IntToStr(value)];

  Result := Self;
end;

function URLEncode(URL: String): String;
var
  x: integer;
  sBuff: string;
const
  SafeMask = ['A'..'Z', '0'..'9', 'a'..'z', '*', '@', '.', '_', '-', ','];
begin
  //Init
  sBuff := '';

  for x := 1 to Length(url) do
  begin
    //Check if we have a safe char
    if url[x] in SafeMask then
    begin
      //Append all other chars
      sBuff := sBuff + url[x];
    end
    else if url[x] = ' ' then
    begin
      //Append space
      sBuff := sBuff + '+';
    end
    else
    begin
      //Convert to hex
      sBuff := sBuff + '%' + IntToHex(Ord(url[x]), 2);
    end;
  end;

  Result := sBuff;
end;

initialization
begin
  encoder := TEncoding.UTF8;
end;

end.

