library funckey;

uses
  SysUtils,
  funckeyunit in 'funckeyunit.pas',
  funckeyform in 'funckeyform.pas' {Form_FuncKey};

exports
  KNTGetPluginName index 1,
  KNTGetPluginVersion index 2,
  KNTGetPluginDescription index 3,
  KNTConfigurePlugin index 4,
  KNTGetPluginFeatures index 5,
  KNTPluginExecute index 6,
  KNTPluginCleanup index 7;

{$E .knl}

begin
end.
