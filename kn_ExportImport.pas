unit kn_ExportImport;

interface
uses
   Classes, wideStrings, kn_const, kn_Info;

   function ConvertHTMLToRTF(const inFilename : WideString; var OutStream: TMemoryStream) : boolean; overload;
   function ConvertHTMLToRTF(HTMLText: string; var RTFText : string) : boolean; overload;
   function ConvertRTFToHTML(const outFilename : String; const RTFText : string; const HTMLExpMethod : THTMLExportMethod) : boolean;
   procedure FreeConvertLibrary;


implementation

uses
  Windows, Messages, SysUtils, StrUtils,
  Graphics, Controls, Forms, Dialogs,
  TntSysUtils, TntSystem,
  Kn_Global, kn_Main, gf_files,
  kn_DllInterface, kn_DLLmng, UWebBrowserWrapper, kn_ClipUtils;


function ConvertHTMLToRTF(const inFilename : WideString; var OutStream: TMemoryStream) : boolean;
var
  s: string;
  HTMLMethod : THTMLImportMethod;
  DlgTextConvImportAsRTF:     TextConvImportAsRTFProc;
  DlgMSWordConvertHTMLToRTF : MSWordConvertHTMLToRTFProc;
  ReleaseIE: boolean;

begin
  Result := false;
  if (not WideFileexists( inFileName)) or (not assigned(OutStream)) then exit;

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
        messagedlg( 'Error while importing HTML text: ' + E.Message, mtError, [mbOK], 0 );
  end;
end; // ConvertHTMLToRTF



function ConvertRTFToHTML(const outFilename : String; const RTFText : string; const HTMLExpMethod : THTMLExportMethod) : boolean;
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
              Result:= DlgTextConvExportRTF( outFileName, 'HTML', PChar(RTFText), GetSystem32Directory + 'html.iec' );
              end;

          htmlExpMSWord : begin
              @DlgMSWordConvertRTFToHTML := GetMethodInDLL(_DLLHandle, 'MSWordConvertRTFToHTML');
              if not assigned(DlgMSWordConvertRTFToHTML) then exit;
              Result:= DlgMSWordConvertRTFToHTML(outFileName, RTFText);
          end;

      end; // case HTMLMethod

  except
    on E : Exception do
        messagedlg( 'Error while exporting to HTML (method= ' + HTMLExportMethods[HTMLExpMethod] + ') : ' + E.Message, mtError, [mbOK], 0 );
  end;
end; // ConvertRTFToHTML


function ConvertHTMLToRTF(HTMLText: string; var RTFText : string) : boolean;
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