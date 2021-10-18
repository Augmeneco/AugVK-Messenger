unit LoginUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  VK_OAUTH_PAGE = 'https://oauth.vk.com/authorize?client_id=7950449&scope=69662&redirect_uri=https://oauth.vk.com/blank.html&display=mobile&response_type=token&revoke=1';

type
  TOnLoginedCallback = procedure (Token: String; ExpiresIn, Id: Integer) of object;

  ILoginInterface = interface
    procedure OpenLoginPage;
    function GetOnLogined: TOnLoginedCallback;
    procedure SetOnLogined(Callback: TOnLoginedCallback);
    property OnLogined: TOnLoginedCallback read GetOnLogined write SetOnLogined;
  end;

implementation

end.

