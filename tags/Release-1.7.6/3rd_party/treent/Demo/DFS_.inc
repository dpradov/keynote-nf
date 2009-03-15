(******************************************************************************)
(* Delphi Free Stuff Include File.  This file is used for all my components   *)
(* to create some standard defines.  This will help reduce headaches when new *)
(* versions of Delphi and C++Builder are released, among other things.        *)
(******************************************************************************)
(* Brad Stowers: bstowers@pobox.com                                           *)
(* Delphi Free Stuff: http://www.delphifreestuff.com/                         *)
(* August 20, 1999                                                            *)
(******************************************************************************)
(* Usage:   Add the following line to the top of your unit file:              *)
(*   {$I DFS.INC}                                                             *)
(******************************************************************************)
(*                                                                            *)
(* Complete Boolean Evaluation compiler directive is turned off by including  *)
(*   this file.                                                               *)
(* The $ObjExportAll directive is turned on if compiling with C++Builder 3 or *)
(*   higher.  This is required for Delphi components built in Builder with    *)
(*   run-time packages.                                                       *)
(*                                                                            *)
(* Here is a brief explanation of what each of the defines mean:              *)
(* DELPHI_FREE_STUFF    : Always defined when DFS.INC is included             *)
(* DFS_WIN16            : Compilation target is 16-bit Windows                *)
(* DFS_WIN32            : Compilation target is 32-bit Windows                *)
(* DFS_USEDEFSHLOBJ     : The SHLOBJ.PAS version contains no none errors.     *)
(*                        Delphi 2.0x and C++Builder 1.0x shipped with a      *)
(*                        version of SHLOBJ.PAS that had many nasty errors.   *)
(*                        See my web site in the Hints section for how to fix *)
(* DFS_NO_COM_CLEANUP   : The version of the compiler being used does not     *)
(*                        require COM objects to be released; it is done      *)
(*                        automatically when they go "out of scope".          *)
(* DFS_COMPILER_1       : Delphi 1.0 is the compiler.  Note that C++B 1.0     *)
(*                        does NOT cause this to be defined.  It is really    *)
(*                        the 2.0 compiler.                                   *)
(* DFS_COMPILER_1_UP    : Delphi 1.0x or higher, or C++B 1.0x or higher is    *)
(*                        the compiler.                                       *)
(* DFS_COMPILER_2       : Delphi 2.0x or C++B 1.0x is the compiler.           *)
(* DFS_COMPILER_2_UP    : Delphi 2.0x or higher, or C++B 1.0x or higher is    *)
(*                        the compiler.                                       *)
(* DFS_COMPILER_3       : Delphi 3.0x or C++B 3.0x is the compiler.           *)
(* DFS_COMPILER_3_UP    : Delphi 3.0x or higher, or C++B 3.0x or higher is    *)
(*                        the compiler.                                       *)
(* DFS_COMPILER_4       : Delphi 4.0x or C++B 4.0x is the compiler.           *)
(* DFS_COMPILER_4_UP    : Delphi 4.0x or higher, or C++B 4.0x or higher is    *)
(*                        the compiler.                                       *)
(* DFS_COMPILER_5       : Delphi 5.0x or C++B 5.0x is the compiler.           *)
(* DFS_COMPILER_5_UP    : Delphi 5.0x or higher, or C++B 5.0x or higher is    *)
(*                        the compiler.                                       *)
(* DFS_UNKNOWN_COMPILER : No sense could be made of the compiliation          *)
(*                        environment.                                        *)
(* DFS_CPPB             : Any version of C++B is being used.                  *)
(* DFS_CPPB_1           : C++B v1.0x is being used.                           *)
(* DFS_CPPB_3           : C++B v3.0x is being used.                           *)
(* DFS_CPPB_3_UP        : C++B v3.0x or higher is being used.                 *)
(* DFS_CPPB_4           : C++B v4.0x is being used.                           *)
(* DFS_CPPB_4_UP        : C++B v4.0x or higher is being used.                 *)
(* DFS_DELPHI           : Any version of Delphi is being used.                *)
(* DFS_DELPHI_1         : Delphi v1.0x is being used.                         *)
(* DFS_DELPHI_2         : Delphi v2.0x is being used.                         *)
(* DFS_DELPHI_2_UP      : Delphi v2.0x or higher is being used.               *)
(* DFS_DELPHI_3         : Delphi v3.0x is being used.                         *)
(* DFS_DELPHI_3_UP      : Delphi v3.0x or higher is being used.               *)
(* DFS_DELPHI_4         : Delphi v4.0x is being used.                         *)
(* DFS_DELPHI_4_UP      : Delphi v4.0x or higher is being used.               *)
(* DFS_DELPHI_5         : Delphi v5.0x is being used.                         *)
(* DFS_DELPHI_5_UP      : Delphi v5.0x or higher is being used.               *)
(******************************************************************************)


{ All DFS components rely on complete boolean eval compiler directive set off. }
{$B-}

{$DEFINE DELPHI_FREE_STUFF}

{$IFDEF WIN32}
  {$DEFINE DFS_WIN32}
{$ELSE}
  {$DEFINE DFS_WIN16}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE DFS_COMPILER_5}
  {$DEFINE DFS_DELPHI}
  {$DEFINE DFS_DELPHI_5}
  {$DEFINE DFS_USEDEFSHLOBJ} { Only D3+ and C++B 3+ have no errors in SHLOBJ }
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE DFS_COMPILER_4}
  {$DEFINE DFS_CPPB}
  {$DEFINE DFS_CPPB_4}
  {$DEFINE DFS_USEDEFSHLOBJ} { Only D3+ and C++B 3+ have no errors in SHLOBJ }
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE DFS_COMPILER_4}
  {$DEFINE DFS_DELPHI}
  {$DEFINE DFS_DELPHI_4}
  {$DEFINE DFS_USEDEFSHLOBJ} { Only D3+ and C++B 3+ have no errors in SHLOBJ }
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE DFS_COMPILER_3}
  {$DEFINE DFS_CPPB}
  {$DEFINE DFS_CPPB_3}
  {$DEFINE DFS_USEDEFSHLOBJ} { Only D3+ and C++B 3+ have no errors in SHLOBJ }
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE DFS_COMPILER_3}
  {$DEFINE DFS_DELPHI}
  {$DEFINE DFS_DELPHI_3}
  {$DEFINE DFS_USEDEFSHLOBJ} { Only D3+ and C++B 3+ have no errors in SHLOBJ }
{$ENDIF}

{$IFDEF VER93}
  {$DEFINE DFS_COMPILER_2}  { C++B v1 compiler is really v2 }
  {$DEFINE DFS_CPPB}
  {$DEFINE DFS_CPPB_1}
  {.$DEFINE DFS_USEDEFSHLOBJ} { C++B 1 has the correct SHLOBJ.H, but
                                SHLOBJ.PAS has errors so this isn't defined }
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE DFS_COMPILER_2}
  {$DEFINE DFS_DELPHI}
  {$DEFINE DFS_DELPHI_2}
{$ENDIF}

{$IFDEF VER80}
  {$DEFINE DFS_COMPILER_1}
  {$DEFINE DFS_DELPHI}
  {$DEFINE DFS_DELPHI_1}
{$ENDIF}

{$IFNDEF DFS_CPPB}
  {$IFNDEF DFS_DELPHI}
    { Don't know what the hell it is.  Could be new version, or could be old BP. }
    {$DEFINE DFS_UNKNOWN_COMPILER}
  {$ENDIF}
{$ENDIF}

{$IFDEF DFS_COMPILER_1}
  {$DEFINE DFS_COMPILER_1_UP}
{$ENDIF}

{$IFDEF DFS_COMPILER_2}
  {$DEFINE DFS_COMPILER_1_UP}
  {$DEFINE DFS_COMPILER_2_UP}
{$ENDIF}

{$IFDEF DFS_COMPILER_3}
  {$DEFINE DFS_COMPILER_1_UP}
  {$DEFINE DFS_COMPILER_2_UP}
  {$DEFINE DFS_COMPILER_3_UP}
{$ENDIF}

{$IFDEF DFS_COMPILER_4}
  {$DEFINE DFS_COMPILER_1_UP}
  {$DEFINE DFS_COMPILER_2_UP}
  {$DEFINE DFS_COMPILER_3_UP}
  {$DEFINE DFS_COMPILER_4_UP}
{$ENDIF}

{$IFDEF DFS_COMPILER_5}
  {$DEFINE DFS_COMPILER_1_UP}
  {$DEFINE DFS_COMPILER_2_UP}
  {$DEFINE DFS_COMPILER_3_UP}
  {$DEFINE DFS_COMPILER_4_UP}
  {$DEFINE DFS_COMPILER_5_UP}
{$ENDIF}

{$IFDEF DFS_DELPHI_2}
  {$DEFINE DFS_DELPHI_2_UP}
{$ENDIF}

{$IFDEF DFS_DELPHI_3}
  {$DEFINE DFS_DELPHI_2_UP}
  {$DEFINE DFS_DELPHI_3_UP}
{$ENDIF}

{$IFDEF DFS_DELPHI_4}
  {$DEFINE DFS_DELPHI_2_UP}
  {$DEFINE DFS_DELPHI_3_UP}
  {$DEFINE DFS_DELPHI_4_UP}
{$ENDIF}

{$IFDEF DFS_DELPHI_5}
  {$DEFINE DFS_DELPHI_2_UP}
  {$DEFINE DFS_DELPHI_3_UP}
  {$DEFINE DFS_DELPHI_4_UP}
  {$DEFINE DFS_DELPHI_5_UP}
{$ENDIF}

{$IFDEF DFS_CPPB_3}
  {$DEFINE DFS_CPPB_3_UP}
{$ENDIF}

{$IFDEF DFS_CPPB_4}
  {$DEFINE DFS_CPPB_3_UP}
  {$DEFINE DFS_CPPB_4_UP}
{$ENDIF}

{$IFDEF DFS_COMPILER_3_UP}
  {$DEFINE DFS_NO_COM_CLEANUP}
{$ENDIF}

{$IFDEF DFS_CPPB_3_UP}
  // C++Builder requires this if you use Delphi components in run-time packages.
  {$ObjExportAll On}
{$ENDIF}

