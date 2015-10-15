library kntutils;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 


{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
  dll_Main in 'dll_Main.pas',
  dll_Keyboard in 'dll_Keyboard.pas',
  dll_KBD in 'dll_KBD.pas' {Form_KBD},
  kn_DLLInterface in 'kn_DLLInterface.pas',
  MSOfficeConverters in 'MSOfficeConverters.pas',
  MSWordConverter in 'MSWordConverter.pas';

{$R *.RES}


exports
  DlgCustomizeKeyboard,
  MSWordConvertHTMLToRTF,
  MSWordConvertRTFToHTML,
  MSWordQuit,
  TextConvImportAsRTF,
  TextConvExportRTF;


begin

end.
