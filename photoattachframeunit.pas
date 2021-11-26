unit PhotoAttachFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Dialogs,
  BGRASpriteAnimation, BCSVGViewer;

type

  { TPhotoAttachFrame }

  TPhotoAttachFrame = class(TFrame)
    BCSVGViewer1: TBCSVGViewer;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    procedure Panel1Click(Sender: TObject);
  private

  public
    procedure DialogButtonClick(Sender: TObject);

  end;

implementation

{$R *.lfm}

uses
  MainFormUnit, AugImage;

{ TPhotoAttachFrame }

procedure TPhotoAttachFrame.Panel1Click(Sender: TObject);
var
  Filename: String;
  Image: TAugImage;
begin
  //MimeTypes.LoadKnownTypes;
  //    ext:=ExtractFileExt(FileName);
  //    ext := MimeTypes.GetMimeType('jpg');
  //    MimeType := MimeTypes.GetMimeType(ExtractFileExt(FileName));
  //    AttachedFiles.Values[FileName] := MimeType;
  //    case MimeType.Split(['/'], 1)[0] of
  //      'image':
  //        begin
  //          Image := TAugImage.Create(Self);
  //          Image.Height := 30;
  //          Image.Width := 30;
  //          Image.Picture.LoadFromFile(FileName);
  //          Image.Cover := True;
  //          Image.Center := True;
  //          Image.Parent := AttachmentsFlow;
  //        end;
  //    end;

  if OpenDialog1.Execute then
    for Filename in OpenDialog1.Files do
    begin
      MainForm.AttachPhoto(Filename)
    end;
    MainForm.HideDialogWindow;
end;

procedure TPhotoAttachFrame.DialogButtonClick(Sender: TObject);
begin
  if TButton(Sender).Tag = 0 then
    MainForm.HideDialogWindow;
end;

end.

