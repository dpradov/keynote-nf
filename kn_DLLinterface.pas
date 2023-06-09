unit kn_DLLinterface;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 
 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 

interface
uses
   Winapi.Windows,
   System.Classes,
   Vcl.Menus;

type
  TDllProc = (
    dllCustomizeKeyboard  );


type
  DlgCustomizeKeyboardProc = function(
    AppHandle : HWND;
    KBD_FN : PChar;
    KeyList : TList;
    ActivationHotkey : TShortCut
  ) : boolean;


  MSWordConvertHTMLToRTFProc = function (const inFilename : string; var OutStream: TMemoryStream) : boolean;
  MSWordConvertRTFToHTMLProc = function (const outFilename : string; const RTF: AnsiString) : boolean;
  MSWordQuitProc = function(): boolean;

  TextConvImportAsRTFProc = function (FileName: AnsiString; Converter: AnsiString; aStream : TStream; ConverterLocation : AnsiString ): Boolean;
  TextConvExportRTFProc=    function (FileName: AnsiString; Converter: AnsiString; aRTFText: PAnsiChar;   ConverterLocation : AnsiString ): Boolean;

implementation

end.
