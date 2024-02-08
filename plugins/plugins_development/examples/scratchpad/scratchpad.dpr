library scratchpad;

{
This library requires TRxRichEdit component
from the RxLib (www.rxlib.ru)
}

uses
  SysUtils,
  scratchpadunit in 'scratchpadunit.pas',
  scratchpadform in 'scratchpadform.pas' {Form_RTF};

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
