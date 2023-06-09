unit dll_Main;

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
uses
   Winapi.Windows,
   System.Classes,
   System.SysUtils,
   System.Variants,
   Vcl.Forms,
   Vcl.Controls,
   Vcl.Dialogs,
   Vcl.OleServer,
   WordXP,
   MSOfficeConverters,
   dll_KBD,
   dll_Keyboard;


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

  if (AppHandle = 0) then
      AppHandle := GetActiveWindow;

  Application.Handle := AppHandle;

  Application.Helpfile := changefileext( application.exename, '.hlp' );

  Form_KBD := TForm_KBD.Create( Application );

  try
    try

      Form_KBD.myKeyList := KeyList;
      Form_KBD.myKBD_FN := KBD_FN;
      if ( Form_KBD.ShowModal = mrOK ) then  begin
         result := true;
         SaveKeyboardList( KBD_FN, KeyList );
      end;

    except
      on E : Exception do
        messagedlg( 'Error in keyboard customization procedure: ' + E.Message, mtError, [mbOK], 0 );
    end;

  finally
    Application.Handle := 0;
    Form_KBD.Free;
  end;


end; // DlgCustomizeKeyboard

end.
