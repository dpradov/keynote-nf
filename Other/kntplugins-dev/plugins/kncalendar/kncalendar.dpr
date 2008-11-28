library KNcalendar;

uses
  SysUtils,
  KNcalendarunit in 'KNcalendarunit.pas',
  KNCalendarform in 'KNCalendarform.pas' {Form_Cal};

exports
  KNTGetPluginName index 1,
  KNTGetPluginVersion index 2,
  KNTGetPluginDescription index 3,
  KNTConfigurePlugin index 4,
  KNTGetPluginFeatures index 5,
  KNTPluginExecute index 6,
  KNTPluginCleanup index 7;

{$E .knl}
{$R *.RES}

begin
end.
