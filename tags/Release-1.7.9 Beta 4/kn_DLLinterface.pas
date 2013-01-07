unit kn_DLLinterface;
(* ************************************************************
 KEYNOTE: MOZILLA PUBLIC LICENSE STATEMENT.
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is KeyNote 1.0.

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 Released: 30 June 2001
 -----------------------------------------------------------
 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net

************************************************************ *)

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
