unit LoginFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, BCMaterialDesignButton,
  fprequests, Graphics, fpjson;

type

  { TLoginFrame }

  TOnLoginedCallback = procedure (Token: String; ExpiresIn, Id: Integer) of object;

  TLoginFrame = class(TFrame)
    ErrorLabel: TLabel;
    ForceSMSLabel: TLabel;
    ValidationButton: TBCMaterialDesignButton;
    ValidCodeEdit: TEdit;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LoginButton: TBCMaterialDesignButton;
    CaptchaButton: TBCMaterialDesignButton;
    LoginEdit: TEdit;
    CaptchaPanel: TPanel;
    CaptchaEdit: TEdit;
    ValidationPanel: TPanel;
    PasswordEdit: TEdit;
    Label1: TLabel;
    LoginPanel: TPanel;
    function SendLoginRequest(Login, Password: String): TResponse;
    function SendLoginRequest(Login, Password: String;
      Params: TParams): TResponse;
    procedure NeedLogin(ErrorJson: TJSONObject);
    procedure LoginButtonClick(Sender: TObject);
    procedure NeedCaptcha(ErrorJson: TJSONObject);
    procedure CaptchaButtonClick(Sender: TObject);
    procedure NeedValidation(ErrorJson: TJSONObject);
    procedure ValidationButtonClick(Sender: TObject);
    procedure ForceSMSLabelClick(Sender: TObject);
    procedure ProcessResponse(Response: TResponse);
  private
    Requests: TRequests;
    CaptchaSid: String;

  public
    OnLogined: TOnLoginedCallback;
    constructor Create(TheOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

uses
  jsonparser;

{ TLoginFrame }

function TLoginFrame.SendLoginRequest(Login, Password: String): TResponse;
begin
  Result := SendLoginRequest(Login, Password, TParams.Create);
end;

function TLoginFrame.SendLoginRequest(Login, Password: String;
  Params: TParams): TResponse;
begin
  Params.Add('grant_type', 'password');
  Params.Add('client_id', '3140623');
  Params.Add('client_secret', 'VeWdmVclDCtn6ihuP1nt');
  Params.Add('scope', '69662');
  Params.Add('2fa_supported', 1);
  //Params.Add('test_redirect_uri', 1);
  Params.Add('username', Login);
  Params.Add('password', Password);
  Params.Add('v', '5.103');
  Result := Requests.Get('https://oauth.vk.com/token', Params);
end;

procedure TLoginFrame.NeedLogin(ErrorJson: TJSONObject);
begin
  LoginPanel.Show;
  ValidationPanel.Hide;
  CaptchaPanel.Hide;

  if ErrorJson <> nil then
    ErrorLabel.Caption := ErrorJson.Strings['error_description'];
end;

procedure TLoginFrame.LoginButtonClick(Sender: TObject);
var
  Response: TResponse;
  Json: TJSONObject;
begin
  Response := SendLoginRequest(LoginEdit.Text, PasswordEdit.Text);

  ProcessResponse(Response);

  Response.Free;
end;

procedure TLoginFrame.NeedCaptcha(ErrorJson: TJSONObject);
var
  Response: TResponse;
  MemStream: TMemoryStream;
  Jpeg: TJPEGImage;
begin
  LoginPanel.Hide;
  ValidationPanel.Hide;
  CaptchaPanel.Show;

  CaptchaSid := ErrorJson.Strings['captcha_sid'];

  Response := Requests.Get(ErrorJson.Strings['captcha_img']);
  MemStream := TMemoryStream.Create;
  MemStream.Write(Response.Data[0], Length(Response.Data));
  //MemStream.SaveToFile('captcha.jpg');
  MemStream.Position := 0;
  Image1.Picture.Graphic := TJPEGImage.Create;
  Image1.Picture.Graphic.LoadFromStream(MemStream);
  MemStream.Free;
end;

procedure TLoginFrame.CaptchaButtonClick(Sender: TObject);
var
  Response: TResponse;
begin
  Response := SendLoginRequest(LoginEdit.Text, PasswordEdit.Text,
    TParams.Create
    .Add('captcha_sid', CaptchaSid)
    .Add('captcha_key', CaptchaEdit.Text));

  ProcessResponse(Response);

  Response.Free;
end;

procedure TLoginFrame.NeedValidation(ErrorJson: TJSONObject);
begin
  LoginPanel.Hide;
  ValidationPanel.Show;
  CaptchaPanel.Hide;

  case ErrorJson.Strings['validation_type'] of
    '2fa_sms':
    begin
      Label4.Caption :=
        'Для подтвеждения входа введите код который мы выслали в SMS вам на номер '+ErrorJson.Strings['phone_mask'];
      //ForceSMSLabel.Caption := 'Отправить SMS повторно';
      ForceSMSLabel.Hide;
    end;
    '2fa_app':
    begin
      Label4.Caption :=
        'Для подтверждения входа введите код из приложения генерации паролей';
      ForceSMSLabel.Show;
    end;
  end;
end;

procedure TLoginFrame.ValidationButtonClick(Sender: TObject);
var
  Response: TResponse;
begin
  Response := SendLoginRequest(LoginEdit.Text, PasswordEdit.Text,
    TParams.Create
    .Add('code', ValidCodeEdit.Text));

  ProcessResponse(Response);

  Response.Free;
end;

procedure TLoginFrame.ForceSMSLabelClick(Sender: TObject);
var
  Response: TResponse;
begin
  Response := SendLoginRequest(LoginEdit.Text, PasswordEdit.Text,
    TParams.Create
    .Add('force_sms', 1));

  ProcessResponse(Response);

  Response.Free;
end;

procedure TLoginFrame.ProcessResponse(Response: TResponse);
var
  Json: TJSONObject;
begin
  Json := TJSONObject(GetJSON(Response.Text));

  if Json.Find('error') <> nil then
  begin
    case Json.Strings['error'] of
      'need_captcha':
        NeedCaptcha(Json);
      'need_validation':
        NeedValidation(Json);
      else
        NeedLogin(Json);
    end;
    exit();
  end;

  if Assigned(OnLogined) then
    OnLogined(Json.Strings['access_token'], Json.Integers['expires_in'], Json.Integers['user_id']);

  Json.Free;
end;

constructor TLoginFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Requests := TRequests.Create;
  Requests.AllowedCodes := [200, 401];

  PasswordEdit.PasswordChar := Chr(149);
  //PasswordEdit.Font.Style := PasswordEdit.Font.Style + [fsBold];
end;

end.

