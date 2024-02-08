library keyrepeat;

uses
  SysUtils,
  keyrepeatunit in 'keyrepeatunit.pas',
  keyrepeatform in 'keyrepeatform.pas' {Form_Key};

exports
  KNTGetPluginName index 1,
  KNTGetPluginVersion index 2,
  KNTGetPluginDescription index 3,
  KNTConfigurePlugin index 4,
  KNTGetPluginFeatures index 5,
  KNTPluginExecute index 6,
  KNTPluginExecute index 7,
  KNTSetPluginID index 8;

{$E .knl}

begin
end.
