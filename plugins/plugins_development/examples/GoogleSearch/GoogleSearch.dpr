library GoogleSearch;

(*
GoogleSearch.KNL
Plugin for KeyNote.
by Daniel Prado Velasco
<dprado.keynote@gmail.com>
https://github.com/dpradov/keynote-nf
*)

uses
  // actual code is contained here:
  GoogleSearchUnit in 'GoogleSearchUnit.pas';

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
