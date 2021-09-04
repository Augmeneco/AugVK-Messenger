unit Utils;

interface

uses
  jsonConf, fpjson, jsonparser, sysutils;

type
  TLogType = (logNormal, logGood, logError, logWarning);

var
  botStartTime: Int64;

threadvar
  logThreadId: Integer;

procedure LogWrite(str: String; logType: TLogType=TLogType.logNormal);
function VeryBadToLower(str: String): String;
function UnescapeHTML ( const S : String ) : String;
function DumpExceptionCallStack(E: Exception): String;


implementation

uses
  Forms, classes;

var
  enableColors: Boolean = True;
  i: Integer;

procedure LogWrite(str: String; logType: TLogType=TLogType.logNormal);
var
  logTime: TDateTime;
begin
  logTime := now();
  //if enableColors then
  //  case logType of
  //    TLogType.logNormal:
  //      textColor(LightGray);
  //    TLogType.logGood:
  //      textColor(Green);
  //    TLogType.logError:
  //      textColor(Red);
  //    TLogType.logWarning:
  //      textColor(Yellow);
  //  end;
  writeln(format('[%s][%u] %s',
                 [formatDateTime('dd/mm/yy hh:nn:ss"."zzz', logTime),
                  logThreadId,
                  str]));
  //writeln(format('[%s] %s',
  //               [formatDateTime('dd/mm/yy hh:nn:ss"."zzz', logTime),
  //                str]));
  //if enableColors then
  //  textColor(LightGray);
end;

function VeryBadToLower(str: String): String;
const
  convLowers: Array [0..87] of String = ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u',
  	'v', 'w', 'x', 'y', 'z', 'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç', 'è', 'é', 'ê', 'ë', 'ì', 'í', 'î', 'ï',
  	'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'а', 'б', 'в', 'г', 'д', 'е', 'ё', 'ж',
  	'з', 'и', 'й', 'к', 'л', 'м', 'н', 'о', 'п', 'р', 'с', 'т', 'у', 'ф', 'х', 'ц', 'ч', 'ш', 'щ', 'ъ', 'ы',
  	'ь', 'э', 'ю', 'я');
  convUppers: Array [0..87] of String = ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U',
  	'V', 'W', 'X', 'Y', 'Z', 'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï',
  	'Ð', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', 'Ø', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ё', 'Ж',
  	'З', 'И', 'Й', 'К', 'Л', 'М', 'Н', 'О', 'П', 'Р', 'С', 'Т', 'У', 'Ф', 'Х', 'Ц', 'Ч', 'Ш', 'Щ', 'Ъ', 'Ъ',
  	'Ь', 'Э', 'Ю', 'Я');
var
  i: Integer;
begin
  result := str;
  for i := 0 to 87 do
    result := stringReplace(result, convUppers[i], convLowers[i], [rfReplaceAll]);
end;

function UnescapeHTML ( const S : String ) : String;
begin
  Result := StringReplace(s,      '&lt;',   '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;',   '>', [rfReplaceAll]);
  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result := StringReplace(Result, '&#39;',  #39, [rfReplaceAll]);
  Result := StringReplace(Result, '&apos;', #39, [rfReplaceAll]);
  Result := StringReplace(Result, '&amp;',  '&', [rfReplaceAll]);
  Result := StringReplace(Result, '<br/>',  LineEnding, [rfReplaceAll]);
end;

function DumpExceptionCallStack(E: Exception): String;
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  Result := Report;
  //ShowMessage(Report);
  //Halt; // End of program execution
end;

begin
  for i := 0 to ParamCount() do
    case ParamStr(i) of
      '--colorless':
        enableColors := False;
    end;
end.

