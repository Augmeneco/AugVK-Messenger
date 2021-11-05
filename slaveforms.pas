unit SlaveForms;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, Messages, Forms, Contnrs, LMessages;

type

  { IMasterForm }

  TSlaveForm = class;
  TMasterForm = class;

  IMasterForm = interface
    procedure AddSlaveForm(SlaveForm: TSlaveForm);
    procedure RemoveSlaveForm(SlaveForm: TSlaveForm);
  end;

  { ISlaveForm }

  ISlaveForm = interface
    procedure SlaveMove(AX, AY: Integer);
    procedure SlaveShowWindow(AShowHide: Boolean);
    procedure SlaveSize(AWidth, AHeight: Integer);

    procedure SetMasterForm(AMasterForm: TMasterForm);
    function GetMasterForm: TMasterForm;
    property MasterForm: TMasterForm read GetMasterForm write SetMasterForm;
  end;

  { TMasterForm }

  TMasterForm = class(TForm, IMasterForm)
  protected
    SlaveFormsList: TObjectList;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMShowWindow(var Message: TLMShowWindow); message LM_SHOWWINDOW;
    procedure WMSize(var Message: TLMSize); message LM_Size;
  public
    procedure AddSlaveForm(SlaveForm: TSlaveForm);
    procedure RemoveSlaveForm(SlaveForm: TSlaveForm);
  end;

  { TSlaveForm }

  TSlaveForm = class(TForm, ISlaveForm)
  protected
    FMasterForm: TMasterForm;
    procedure SlaveMove(AX, AY: Integer);
    procedure SlaveShowWindow(AShowHide: Boolean);
    procedure SlaveSize(AWidth, AHeight: Integer);
  public
    procedure SetMasterForm(AMasterForm: TMasterForm);
    function GetMasterForm: TMasterForm;
  published
    property MasterForm: TMasterForm read GetMasterForm write SetMasterForm;
  end;

implementation

{ TMasterForm }

procedure TMasterForm.WMMove(var Message: TLMMove);
var
  i: Integer;
begin
  inherited;
  for i :=0 to SlaveFormsList.Count-1 do
  begin
    (SlaveFormsList[i] as ISlaveForm).SlaveMove(ClientOrigin.X, ClientOrigin.Y);
    TForm(SlaveFormsList[i]).ShowOnTop;
  end;
end;

procedure TMasterForm.WMShowWindow(var Message: TLMShowWindow);
var
  i: Integer;
begin
  inherited;
  for i :=0 to SlaveFormsList.Count-1 do
  begin
    (SlaveFormsList[i] as ISlaveForm).SlaveShowWindow(Message.Show);
    TForm(SlaveFormsList[i]).ShowOnTop;
  end;
end;

procedure TMasterForm.WMSize(var Message: TLMSize);
var
  i: Integer;
begin
  inherited;
  for i :=0 to SlaveFormsList.Count-1 do
  begin
    (SlaveFormsList[i] as ISlaveForm).SlaveSize(ClientRect.Width, ClientRect.Height);
    TForm(SlaveFormsList[i]).ShowOnTop;
  end;
end;

procedure TMasterForm.AddSlaveForm(SlaveForm: TSlaveForm);
begin
  SlaveFormsList.Add(SlaveForm);
  SlaveForm.PopupParent := Self;
end;

procedure TMasterForm.RemoveSlaveForm(SlaveForm: TSlaveForm);
begin
  SlaveFormsList.Remove(SlaveForm);
end;

{ TSlaveForm }

procedure TSlaveForm.SlaveMove(AX, AY: Integer);
begin
  Top := AY;
  Left := AX;
  Invalidate
end;

procedure TSlaveForm.SlaveShowWindow(AShowHide: Boolean);
begin
  if AShowHide = True then
    Show
  else
    Hide;
end;

procedure TSlaveForm.SlaveSize(AWidth, AHeight: Integer);
begin
  Width := AWidth;
  Height := AHeight;
end;

procedure TSlaveForm.SetMasterForm(AMasterForm: TMasterForm);
begin
  FMasterForm := AMasterForm;
  PopupParent := AMasterForm;
end;

function TSlaveForm.GetMasterForm: TMasterForm;
begin
  Result := FMasterForm;
end;

end.

