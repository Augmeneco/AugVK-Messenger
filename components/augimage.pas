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
  protected

  public
    function DestRect: TRect; override;
  published
    property Cover: Boolean read FCover write FCover;
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
