library plugintest;

(*
SAMPLE TEST PLUGIN for KeyNote
by Marek Jedlinski
<eristic@lodz.pdi.net>
http://www.lodz.pdi.net/~eristic/free/keynote.html
22 June 2001
*)

uses
  // actual code is contained here:
  plugintestunit in 'plugintestunit.pas';

exports
  // the names of these functions MUST be spelled
  // exactly as they are here; otherwise KeyNote
  // will not find them. Remember that DLL function
  // names are Case-Sensitive.
  KNTGetPluginName index 1,
  KNTGetPluginVersion index 2,
  KNTGetPluginDescription index 3,
  KNTConfigurePlugin index 4,
  KNTGetPluginFeatures index 5,
  KNTPluginExecute index 6,
  KNTPluginCleanup index 7;

// This tells the compiler to create a file
// with the .KNL extension, rather than the
// standard .DLL extension
{$E .knl}

begin
end.
