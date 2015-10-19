// Base include file for all gf_XXX units
// usage:
// {$I gf_base.inc}

// enable this to use localization functions
{$DEFINE LOCALIZED}

// check Delphi version
{$IFDEF VER140}
  {$DEFINE DELPHI_6}
  {$DEFINE DELPHI_5_UP}
  {$DEFINE DELPHI_6_UP}
  {$D '[Compiled with Delphi 6]'}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE DELPHI_5}
  {$DEFINE DELPHI_5_UP}
  {$D '[Compiled with Delphi 5]'}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE DELPHI_4}
  {$DEFINE DELPHI_4_UP}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE DELPHI_3}
  {$DEFINE DELPHI_3_UP}
{$ENDIF}

// standard compiler directives

{$ALIGN ON}
{$BOOLEVAL OFF} // use shortcut boolean evaluation everywhere
                // (DO NOT CHANGE, or you will break a lot of code!)
{$EXTENDEDSYNTAX ON}
{$IOCHECKS ON}
{$LONGSTRINGS ON}
{$OPENSTRINGS ON}
{$OPTIMIZATION ON}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}
{$TYPEDADDRESS OFF}
{$VARSTRINGCHECKS OFF}
{$WRITEABLECONST ON} // legacy stuff, but I'm used to it and it's harmless.


{$IFDEF DELPHI_6_UP}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

