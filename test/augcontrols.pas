{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit AugControls;

{$warn 5023 off : no warning about unused units}
interface

uses
  StackPanel, DelphiCompatibility, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('StackPanel', @StackPanel.Register);
end;

initialization
  RegisterPackage('AugControls', @Register);
end.
