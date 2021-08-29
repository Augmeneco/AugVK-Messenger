unit AugScrollBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LMessages;

type
  TScrollExEvent = procedure(Sender: TObject; var ScrollPos: Integer) of object;
  TAugScrollBox = class(TScrollBox)
  strict private
    FOnVScroll: TScrollExEvent;
    FOnHScroll: TScrollExEvent;
  private

  protected
    procedure WMHScroll(var Message : TLMHScroll); message LM_HScroll;
    procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
    procedure DoHScroll(var aScrollPos: integer);
    procedure DoVScroll(var aScrollPos: integer);
  public

  published
    property OnVScroll: TScrollExEvent read FOnVScroll write FOnVScroll;
    property OnHScroll: TScrollExEvent read FOnHScroll write FOnHScroll;
  end;

//  ...
//  TScrollExEvent = procedure(Sender: TObject; var ScrollPos: Integer) of object;
//  TScrollBox = class(TScrollingWinControl)
//  //Modified
//  strict private
//    FOnVScroll: TScrollExEvent;
//    FOnHScroll: TScrollExEvent;
//  //
//  protected
//    //Modified
//    procedure WMHScroll(var Message : TLMHScroll); message LM_HScroll;
//    procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
//    procedure DoHScroll(var aScrollPos: integer);
//    procedure DoVScroll(var aScrollPos: integer);
//    //
//    class procedure WSRegisterClass; override;
//  public
//    constructor Create(AOwner: TComponent); override;
//  published
//    //Modified
//    property OnVScroll: TScrollExEvent read FOnVScroll write FOnVScroll;
//    property OnHScroll: TScrollExEvent read FOnHScroll write FOnHScroll;
//    //
//....


procedure Register;

implementation

procedure Register;
begin
  {$I augscrollbox_icon.lrs}
  RegisterComponents('AugControls',[TAugScrollBox]);
end;

procedure TAugScrollBox.DoHScroll(var aScrollPos: integer);
begin
  if Assigned(FOnHScroll) then
    FOnHScroll(Self, aScrollPos);
end;

procedure TAugScrollBox.DoVScroll(var aScrollPos: integer);
begin
  if Assigned(FOnVScroll) then
    FOnVScroll(Self, aScrollPos);
end;

procedure TAugScrollBox.WMHScroll(var Message: TLMHScroll);
begin
  //HorzScrollBar.Position:=Message.Pos;
  inherited WMHScroll(Message);
  DoHScroll(Message.Pos);
end;

procedure TAugScrollBox.WMVScroll(var Message: TLMVScroll);
begin
  //writeln(Message.Pos);
  //VertScrollbar.Position:=Message.Pos;
  inherited WMVScroll(Message);
  DoVScroll(Message.Pos);
end;

end.
