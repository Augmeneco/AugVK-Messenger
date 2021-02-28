unit WebBrowserFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, IpHtml, Ipfilebroker,
	Iphttpbroker;

type

  TURLREdirectHandler = procedure(URL: String);

	{ TWebBrowserForm }

  TWebBrowserForm = class(TForm)
		IpHtmlPanel1: TIpHtmlPanel;
		IpHttpDataProvider1: TIpHttpDataProvider;
		procedure IpHtmlPanel1DocumentOpen(Sender: TObject);
  private
    procedure SetURL(URL: String);
    function GetURL: String;
  public
    property URL: String read GetURL write SetURL;
  public
    URLRedirectHandler: TURLREdirectHandler
  end;

implementation

{$R *.lfm}

{ TWebBrowserForm }

procedure TWebBrowserForm.IpHtmlPanel1DocumentOpen(Sender: TObject);
begin
  if URLRedirectHandler <> nil then
    URLRedirectHandler(IpHtmlPanel1.CurURL);
end;

procedure TWebBrowserForm.SetURL(URL: String);
begin
  IpHtmlPanel1.OpenURL(URL);
end;

function TWebBrowserForm.GetURL: String;
begin
  Result := IpHtmlPanel1.CurURL;
end;

end.

