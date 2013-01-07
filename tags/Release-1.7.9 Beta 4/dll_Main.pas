unit dll_Main;
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

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <marekjed@users.sourceforge.net>

************************************************************ *)

interface
uses Windows, Forms, Classes, SysUtils, Controls, Dialogs,
   OleServer, WordXP, Variants,
   MSOfficeConverters,
   gf_files, kn_const,
   kn_DLLInterface, dll_KBD, dll_Keyboard;


function DlgCustomizeKeyboard(
  AppHandle : HWND;
  KBD_FN : PChar;
  KeyList : TList;
  ActivationHotkey : TShortCut ) : boolean;


implementation

function DlgCustomizeKeyboard(
  AppHandle : HWND;
  KBD_FN : PChar;
  KeyList : TList;
  ActivationHotkey : TShortCut ) : boolean;
var
  Form_KBD: TForm_KBD;

begin
  result := false;

  if ( AppHandle = 0 ) then
    AppHandle := GetActiveWindow;
  Application.Handle := AppHandle;

  Application.Helpfile := changefileext( application.exename, '.hlp' );

  Form_KBD := TForm_KBD.Create( Application );

  try
    try

      Form_KBD.myKeyList := KeyList;
      Form_KBD.myKBD_FN := KBD_FN;
      if ( Form_KBD.ShowModal = mrOK ) then
      begin
        result := true;
        SaveKeyboardList( KBD_FN, KeyList );
      end;

    except
      on E : Exception do
      begin
        messagedlg( 'Error in keyboard customization procedure: ' + E.Message, mtError, [mbOK], 0 );
      end;
    end;
  finally
    Application.Handle := 0;
    Form_KBD.Free;
  end;


end; // DlgCustomizeKeyboard

end.
