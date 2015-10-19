library palmexport;

{
KeyNote plugin to export text
in PalmOS "doc" format (text only)
}

uses
  SysUtils,
  palmexportunit in 'palmexportunit.pas',
  palmexportform in 'palmexportform.pas';

exports
  KNTGetPluginName index 1,
  KNTGetPluginVersion index 2,
  KNTGetPluginDescription index 3,
  KNTConfigurePlugin index 4,
  KNTGetPluginFeatures index 5,
  KNTPluginExecute index 6,
  KNTPluginExecute index 7;

{$E .knl}

begin
end.
