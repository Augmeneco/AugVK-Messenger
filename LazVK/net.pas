unit Net;

interface
  uses
    classes;

  type
    TResponse = record
      text: AnsiString;
      data: Array of Byte;
      code: Integer;
    end;
    TFile = record
      name: String;
      filename: String;
      contenttype: String;
      contents: TStream;
    end;

  function get(url: String; timeout: Integer=0): TResponse;
  function post(url: String; formData: Array of AnsiString; fileData: Array of TFile;
                timeout: Integer=0): TResponse;


implementation
  uses
    SysUtils, fphttpclient, fpopenssl, openssl;

  var
    encoder: TEncoding;

  //function writeFunction(pBuff: Pointer; size: Integer; nmemb: Integer; pUserData: Pointer): Integer;
  //begin
  //  //writeLn(String(pBuff));
  //  //Response(pUserData).text := Response(pUserData).text + String(pBuff);
  //  TStream(pUserData).write(pBuff^, size*nmemb);
  //  writeFunction := size*nmemb;
  //end;

  function get(url: String; timeout: Integer=0): TResponse;
  var
    bs: TBytesStream;
    resp: TResponse;
    client: TFPHTTPClient;
  begin
    bs := TBytesStream.Create();
    client := TFPHttpClient.Create(nil);
    client.IOTimeout := timeout;
    client.get(url, bs);
    resp.code := client.ResponseStatusCode;
    resp.text := encoder.GetString(bs.Bytes);
    resp.data := bs.Bytes;

    FreeAndNil(client);
    FreeAndNil(bs);

    result := resp;
  end;

  function post(url: String; formData: Array of AnsiString; fileData: Array of TFile;
                timeout: Integer=0): TResponse;
  const
    CRLF = #13#10;
  var
    bs: TBytesStream;
    client: TFPHTTPClient;

    S, Sep : String;
    SS : TStringStream;
    I: Integer;
    N,V: String;
  begin
    bs := TBytesStream.Create();
    client := TFPHTTPClient.Create(nil);
    client.IOTimeout := timeout;

    Sep:=Format('%.8x_multipart_boundary',[Random($ffffff)]);
    client.AddHeader('Content-Type','multipart/form-data; boundary='+Sep);
    SS:=TStringStream.Create('');
    if (length(formData)<>0) or (length(fileData)<>0) then
    begin
      S := '--'+Sep+CRLF;
      SS.WriteBuffer(s[1], length(S));
    end;
    if (length(formData)<>0) then
      for I:=0 to length(formData) -1 do
        begin
        // not url encoded
        n := copy(formData[i], 0, pos('=', formData[i])-1);
        v := copy(formData[i], pos('=', formData[i])+1, length(formData[i])-pos('=', formData[i]));
        S := '';
        S:=S+Format('Content-Disposition: form-data; name="%s"'+CRLF+CRLF+'%s'+CRLF,[n, v]);
        S:=S+'--'+Sep+CRLF;
        SS.WriteBuffer(S[1],Length(S));
        end;
    //writeln(ss.DataString);
    if (length(fileData)<>0) then
      for I:=0 to length(fileData) -1 do
      begin
        S:='';
        s:=s+Format('Content-Disposition: form-data; name="%s"; filename="%s"'+CRLF,[fileData[i].name,ExtractFileName(fileData[i].filename)]);
        s:=s+'Content-Type: '+fileData[i].contentType+CRLF+CRLF;
        SS.WriteBuffer(S[1],Length(S));
        fileData[i].contents.Seek(0, soFromBeginning);
        SS.CopyFrom(fileData[i].contents,fileData[i].contents.Size);
        S:=CRLF+'--'+Sep+CRLF;
        SS.WriteBuffer(S[1],Length(S));
      end;
    //writeln(ss.DataString);
    SS.Position:=0;
    client.RequestBody:=SS;
    client.Post(url, bs);
    result.code := client.ResponseStatusCode;
    result.text := encoder.GetString(bs.Bytes);
    result.data := bs.Bytes;

    FreeAndNil(bs);
    client.RequestBody:=Nil;
    FreeAndNil(client);
    FreeAndNil(SS);
  end;

initialization
begin
  encoder := TEncoding.UTF8;
end;
end.

