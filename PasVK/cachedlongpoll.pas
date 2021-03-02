unit CachedLongpoll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VkLongpoll;

type

	{ TCachedLongpoll }

  TCachedLongpoll = class(TLongpollThread)
  protected
    procedure ProcessEvent(Event: TJSONArray); override;
	end;

implementation

{ TCachedLongpoll }

procedure TCachedLongpoll.ProcessEvent(Event: TJSONArray);
begin
  //тут твой кеширующий говнокод
	inherited ProcessEvent(Event);
end;

end.

