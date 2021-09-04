unit ConfigUtils;

{$mode objfpc}{$H+}

interface

uses
  fpjson;

var
  Config: TJSONObject;
  ConfigLocation: String;

procedure SaveConfig;
procedure LoadConfig;

implementation

uses
  Forms, Classes, SysUtils;

var
  ConfigFile: TFileStream;

procedure SaveConfig;
begin
  ConfigFile := TFileStream.Create(ConfigLocation, fmCreate);
  ConfigFile.WriteAnsiString(AnsiString(Config.FormatJSON));
  ConfigFile.Free;
end;

procedure LoadConfig;
begin
  ConfigFile := TFileStream.Create(ConfigLocation, fmOpenRead);
  Config := TJSONObject.Create;
  Config := TJSONObject(GetJSON(ConfigFile));
  ConfigFile.Free;
end;

procedure FillNewConfig;
begin
  Config.Add('accounts', TJSONArray.Create);
  Config.Add('active_account', 0);
end;

initialization
begin
  ConfigLocation := Application.Location+'config.json';
  if not FileExists(ConfigLocation) then
  begin
    Config := TJSONObject.Create;
    FillNewConfig;
    SaveConfig;
  end
  else
    LoadConfig;
end;

end.

