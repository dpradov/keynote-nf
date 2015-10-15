unit kn_DLLinterface;

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

interface
uses Windows, Menus, Classes, kn_Const;

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


  MSWordConvertHTMLToRTFProc = function (const inFilename : WideString; var OutStream: TMemoryStream) : boolean;
  MSWordConvertRTFToHTMLProc = function (const outFilename : WideString; const RTF: string) : boolean;
  MSWordQuitProc = function(): boolean;

  TextConvImportAsRTFProc = function (FileName: String; Converter: String; aStream : TStream; ConverterLocation : string ): Boolean;
  TextConvExportRTFProc=    function (FileName: String; Converter: String; aRTFText: PChar;   ConverterLocation : string ): Boolean;

implementation

end.
