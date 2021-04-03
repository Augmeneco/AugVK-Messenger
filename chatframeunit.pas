unit ChatFrameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type

	{ TChatFrame }

  TChatFrame = class(TFrame)
    Bevel1: TBevel;
    ChatAvatarImage: TImage;
    TitleLabel: TLabel;
    LastMessageLabel: TLabel;
  private

  public

  end;

implementation

{$R *.lfm}

{ TChatFrame }

end.

