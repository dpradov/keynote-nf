unit kn_ExportImport;

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
   Winapi.Messages,
   System.Classes,
   System.SysUtils,
   System.StrUtils,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.Clipbrd,
   UWebBrowserWrapper,
   gf_files,
   gf_streams,
   kn_const,
   kn_Info,
   kn_DllInterface,
   kn_DLLmng,
   kn_ClipUtils,
   Kn_Global,
   kn_Main;


   function ConvertHTMLToRTF(const inFilename : string; var OutStream: TMemoryStream) : boolean; overload;
   function ConvertHTMLToRTF(HTMLText: string; var RTFText : AnsiString) : boolean; overload;
   function ConvertRTFToHTML(const outFilename : String; const RTFText : AnsiString; const HTMLExpMethod : THTMLExportMethod) : boolean;
   procedure FreeConvertLibrary;


implementation

resourcestring
  STR_01 = 'Error while importing HTML text: ';
  STR_02 = 'Error while exporting to HTML (method= ';


function ConvertHTMLToRTF(const inFilename : string; var OutStream: TMemoryStream) : boolean;
var
  s: AnsiString;
  HTMLMethod : THTMLImportMethod;
  DlgTextConvImportAsRTF:     TextConvImportAsRTFProc;
  DlgMSWordConvertHTMLToRTF : MSWordConvertHTMLToRTFProc;
  ReleaseIE: boolean;

begin
  Result := false;
  if (not Fileexists( inFileName)) or (not assigned(OutStream)) then exit;

  try
      HTMLMethod:= KeyOptions.HTMLImportMethod;

      case HTMLMethod of
          htmlSharedTextConv : begin
              @DlgTextConvImportAsRTF := GetMethodInDLL(_DLLHandle, 'TextConvImportAsRTF');
              if not assigned(DlgTextConvImportAsRTF) then exit;
              Result:= DlgTextConvImportAsRTF( inFileName, 'HTML', OutStream, '' );
          end;


          htmlMSWord: begin
              @DlgMSWordConvertHTMLToRTF := GetMethodInDLL(_DLLHandle, 'MSWordConvertHTMLToRTF');
              if not assigned(DlgMSWordConvertHTMLToRTF) then exit;
              Result:= DlgMSWordConvertHTMLToRTF(inFileName, OutStream);
          end;


          htmlIE: begin
              // Si _IE no está asignado, saldrá sin estarlo. Sólo lo estará cuando se haya usado para convertir un texto disponible
              // en el portapapeles en formato HTML. En ese caso se dejará vivo porque lo más probable es que se use más veces, pues
              // es señal de que se está usando un programa como Firefox, que no ofrece en el portapapeles el formato RTF
              if not assigned(_IE) then begin
                 _IE:= TWebBrowserWrapper.Create(Form_Main);
                 ReleaseIE:= True;
              end
              else
                 ReleaseIE:= False;

              try
                  _IE.NavigateToLocalFile ( inFileName );
                  _IE.CopyAll;                           // Select all and then copy to clipboard
                  s:= Clipboard.AsRTF;
                  OutStream.WriteBuffer( s[1], length(s));
                  Result := True;
              finally
                  if ReleaseIE then
                     FreeAndNil(_IE);
              end;
          end;

      end; // case HTMLMethod

  except
    on E : Exception do
        messagedlg( STR_01 + E.Message, mtError, [mbOK], 0 );
  end;
end; // ConvertHTMLToRTF



function ConvertRTFToHTML(const outFilename : String; const RTFText : AnsiString; const HTMLExpMethod : THTMLExportMethod) : boolean;
var
  DlgTextConvExportRTF:       TextConvExportRTFProc;
  DlgMSWordConvertRTFToHTML : MSWordConvertRTFToHTMLProc;

begin
  Result := false;
  if RTFText = '' then exit;

  try
      case HTMLExpMethod of
          htmlExpMicrosoftHTMLConverter : begin
              @DlgTextConvExportRTF := GetMethodInDLL(_DLLHandle, 'TextConvExportRTF');
              if not assigned(DlgTextConvExportRTF) then exit;
              Result:= DlgTextConvExportRTF( outFileName, 'HTML', PAnsiChar(RTFText), GetSystem32Directory + 'html.iec' );
              end;

          htmlExpMSWord : begin
              @DlgMSWordConvertRTFToHTML := GetMethodInDLL(_DLLHandle, 'MSWordConvertRTFToHTML');
              if not assigned(DlgMSWordConvertRTFToHTML) then exit;
              Result:= DlgMSWordConvertRTFToHTML(outFileName, RTFText);
          end;

      end; // case HTMLMethod

  except
    on E : Exception do
        MessageDlg(STR_02 + HTMLExportMethods[HTMLExpMethod] + ') : ' + E.Message, mtError, [mbOK], 0 );
  end;
end; // ConvertRTFToHTML


{ Converts HTML to RTF with the help of WebBrowser
 If conversion is possible, it will return RTF text in the paremeter and also it will be available as
 RTF format in the clipboard }
function ConvertHTMLToRTF(HTMLText: string; var RTFText : AnsiString) : boolean;
begin
  Result := false;
  if (not _ConvertHTMLClipboardToRTF) or (HTMLText = '') then exit;

  try
      if not assigned(_IE) then
         _IE:= TWebBrowserWrapper.Create(Form_Main);

      _IE.LoadFromString  ( UTF8_BOM + HTMLText );
      _IE.CopyAll;                           // Select all and then copy to clipboard
      RTFText:= Clipboard.AsRTF;
      Result := True;

  except
     _ConvertHTMLClipboardToRTF:= false;
  end;
end; // ConvertHTMLToRTF


procedure FreeConvertLibrary;
var
   DlgMSWordQuit : MSWordQuitProc;

begin
    if _DllHandle > 0 then begin
       try
          @DlgMSWordQuit := GetMethodInDLL(_DLLHandle, 'MSWordQuit');
          if not assigned(DlgMSWordQuit) then exit;
          DlgMSWordQuit();

       finally
          FreeLibrary( _DllHandle );
          _DllHandle:= 0;
       end;
    end;
end;    // FreeConvertLibrary


initialization
  _DLLHandle:= 0;
  _IE:= nil;

finalization

end.