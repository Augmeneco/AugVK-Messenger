unit AugImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TAugImage }

  TAugImage = class(TImage)
  private
    FCover: Boolean;
    FOffsetX: Integer;
    FOffsetY: Integer;
    procedure SetOffsetX(AValue: Integer);
    procedure SetOffsetY(AValue: Integer);
  protected

  public
    function DestRect: TRect; override;
  published
    property Cover: Boolean read FCover write FCover default False;
    property OffsetX: Integer read FOffsetX write SetOffsetX default 0;
    property OffsetY: Integer read FOffsetY write SetOffsetY default 0;
  end;

procedure Register;

implementation

uses
  LCLIntf;

procedure Register;
begin
  {$I augimage_icon.lrs}
  RegisterComponents('AugControls',[TAugImage]);
end;

{ TAugImage }

procedure TAugImage.SetOffsetX(AValue: Integer);
begin
  if FOffsetX = AValue then exit;
  FOffsetX := AValue;
  PictureChanged(Self);
end;

procedure TAugImage.SetOffsetY(AValue: Integer);
begin
  if FOffsetY = AValue then exit;
  FOffsetY := AValue;
  PictureChanged(Self);
end;

function TAugImage.DestRect: TRect;
var
  imgRatio, containerRatio: Real;
  finalHeight, finalWidth: Integer;
  ChangeX, ChangeY: Integer;
begin
  Result := inherited DestRect;

  if Cover then
  begin
    imgRatio := (Picture.Height / Picture.Width);
    containerRatio := (Height / Width);

    if containerRatio > imgRatio then
    begin
      finalHeight := Height;
      finalWidth := Trunc(Height / imgRatio);
    end
    else
    begin
      finalWidth := Width;
      finalHeight := Trunc(Width * imgRatio);
    end;

    finalWidth := finalWidth+FOffsetX;
    finalHeight := finalHeight+FOffsetY;

    Result := Rect(0,0,finalWidth,finalHeight);

    if Center then
    begin
      ChangeX := (Width-finalWidth) div 2;
      ChangeY := (Height-finalHeight) div 2;
      if KeepOriginXWhenClipped and (ChangeX<0) then ChangeX := 0;
      if KeepOriginYWhenClipped and (ChangeY<0) then ChangeY := 0;
      OffsetRect(Result, ChangeX, ChangeY);
    end;
  end;
end;

end.
