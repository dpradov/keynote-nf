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
uses Windows, Forms, Classes, SysUtils,
  Controls, Dialogs, Menus,
  gf_misc, gf_files, gf_strings,
  kn_Const, kn_Info, kn_INI,
  dll_HTMLConvIE, Contnrs,
  kn_DLLInterface,
  SHDocVw,
  OleServer,
  //Word2000, *1
  //Word97,   *1
  WordXP,   // *1
  dll_KBD,
  Variants, // *1
  MSOfficeConverters,
  dll_Keyboard;


function DlgCustomizeKeyboard(
  AppHandle : HWND;
  KBD_FN : PChar;
  KeyList : TList;
  ActivationHotkey : TShortCut ) : boolean;

function ConvertHTMLToRTF(
    AppHandle : HWND;
    HTMLMethod : THTMLImportMethod;
    inFileName : PChar;
    var outFileName : TFilenameBuffer;
    ConverterLocation : PChar
  ) : integer;

function ConvertRTFToHTML(
    AppHandle : HWND;
    RTFText : PChar;
    outFileName : PChar;
    ConverterLocation : PChar
  ) : boolean;

implementation

function ConvertRTFToHTML(
    AppHandle : HWND;
    RTFText : PChar;
    outFileName : PChar;
    ConverterLocation : PChar
  ) : boolean;
begin
  try
    result := ExportRTF( outFilename, 'HTML Document', RTFText, ConverterLocation );
  except
    on E : Exception do
    begin
      result := false;
      messagedlg( 'Error while exporting to HTML: ' + E.Message, mtError, [mbOK], 0 );
    end;
  end;
end; // ConvertRTFToHTML

function ConvertHTMLToRTF(
    AppHandle : HWND;
    HTMLMethod : THTMLImportMethod;
    inFileName : PChar;
    var outFileName : TFilenameBuffer;
    ConverterLocation : PChar
  ) : integer;
var
  Form_HtmlConvIE: TForm_HtmlConvIE;
  tmpFN : shortstring;
  s : string;
  SaveFormat : OleVariant;
  vFalse : OleVariant;
  WD : _Document;
  WordApp : TWordApplication;
  oldConfirmConversions, oldSavePropertiesPrompt : boolean;
  oleInFileName, oleOutFileName : OleVariant;
  myStream : TFileStream;
begin
  result := -1;

  fillchar( outFileName, sizeof( outFileName ), 0 );
  if ( not fileexists( inFileName )) then exit;

  tmpFN := extractfilepath( inFileName ) + RandomFileName( extractfilepath( inFileName ), '.tmp', 8 );
  move( tmpFN[1], outFileName, length( tmpFN ));

  case HTMLMethod of
    htmlWindowsNative : begin
      try
        myStream := TFileStream.Create( outFileName, ( fmCreate or fmShareDenyWrite ));
      except
        on E : Exception do
        begin
          messagedlg( E.Message, mtError, [mbOK], 0 );
          exit;
        end;
      end;
      try
        try
          if ImportAsRTF( inFileName, 'HTML Document', myStream, ConverterLocation ) then
            result := 0
          else
            result := 1;
        except
          on E : Exception do
          begin
            result := 99999;
            messagedlg( 'Error while importing HTML text: ' + E.Message, mtError, [mbOK], 0 );
          end;
        end;
      finally
        myStream.Free;
      end;
    end; // htmlOfficeSimple

    htmlOffice : begin
      try
        try
          SaveFormat := wdFormatRTF;

          s := inFileName;
          oleInFileName := s;
          oleOutFileName := tmpFN;

          WordApp := TWordApplication.Create( nil );

          try

            WordApp.ConnectKind := ckNewInstance;
            WordApp.AutoQuit := false;

            WordApp.Connect;

            try

              vFalse := false;

              WD := WordApp.Documents.Open( oleInFileName, vFalse,
                          EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam );

              oldConfirmConversions := WordApp.Options.ConfirmConversions;
              oldSavePropertiesPrompt := WordApp.Options.SavePropertiesPrompt;
              WordApp.Options.ConfirmConversions := false;
              WordApp.Options.SavePropertiesPrompt := false;

              try
                WD.SaveAs( oleOutFileName, SaveFormat,
                   EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam );

                WD.Close( vFalse, EmptyParam, EmptyParam );

                result := 0;

              finally
                WordApp.Options.ConfirmConversions := oldConfirmConversions;
                WordApp.Options.SavePropertiesPrompt := oldSavePropertiesPrompt;
              end;

            finally
              WordApp.Disconnect;
            end;

          finally
            WordApp.Free;
          end;

        except
          on E : Exception do
          begin
            messagedlg( E.Message, mtError, [mbOK], 0 );
            fillchar( outFileName, sizeof( outFileName ), 0 );
            result := GetLasterror;
            if ( result = 0 ) then
              result := 99999;
          end;
        end;
      finally
      end;
    end; // htmlOfficeFull

    htmlIE : begin
      Form_HtmlConvIE := TForm_HtmlConvIE.Create( nil );
      try
        try
          Form_HtmlConvIE.inFN := inFileName;
          Form_HtmlConvIE.outFN := tmpFN;

          if ( Form_HtmlConvIE.ShowModal = mrOK ) then
          begin
            result := 0;
          end;
        except
          on E : Exception do
          begin
            messagedlg( E.Message, mtError, [mbOK], 0 );
            fillchar( outFileName, sizeof( outFileName ), 0 );
            result := GetLasterror;
            if ( result = 0 ) then
              result := 99999;
          end;
        end;
      finally
        Form_HtmlConvIE.Free;
      end;
    end; // htmlIE
  end; // case HTMLMethod


end; // ConvertHTMLToRTF

function DlgCustomizeKeyboard(
  AppHandle : HWND;
  KBD_FN : PChar;
  KeyList : TList;
  ActivationHotkey : TShortCut ) : boolean;
var
  {
  testlist : TStringList;
  myItem : TKeyMenuItem;
  i, cnt : integer;
  }
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


      {
      cnt := KeyList.Count;
      testlist := TStringList.Create;
      try
        for i := 1 to cnt do
        begin
          myItem := TKeyMenuItem( KeyList[pred( i )] );
          testlist.Add( Format(
            '=%s @%d #%d',
            [myItem.Name, ord( myItem.Category ), myItem.ShortCut]
          ));
        end;
        testlist.SaveToFile( changefileext( application.exename, '.kkk' ));
      finally
        testlist.Free;
      end;
      }

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



  {
  if ( AppHandle = 0 ) then
    AppHandle := GetActiveWindow;
  Application.Handle := AppHandle;

  Form_KBD := TForm_KBD.Create( Application );
  try
    try
      if ( Form_KBD.ShowModal = mrOK ) then
      begin
        result := true;
      end;
    except
      On E : Exception do
      begin
        result := false;
        Application.MessageBox('Komunikat', 'Error', MB_OK+MB_ICONHAND+MB_DEFBUTTON1+MB_APPLMODAL);
      end;
    end;
  finally
    Form_KBD.Free;
    Application.Handle := 0;
  end;
  }

end; // DlgCustomizeKeyboard


end.
