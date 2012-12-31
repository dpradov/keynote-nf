unit kn_ExportImport;

interface
uses
   Classes, wideStrings, kn_const, kn_Info;

   function ConvertHTMLToRTF(const inFilename : WideString; var OutStream: TMemoryStream) : boolean;
   function ConvertRTFToHTML(const outFilename : String; const RTFText : string; const HTMLExpMethod : THTMLExportMethod) : boolean;
   procedure FreeConvertLibrary;


implementation

uses
  Windows, Messages, SysUtils, StrUtils,
  Graphics, Controls, Forms, Dialogs,
  TntSysUtils,
  Kn_Global, kn_Main, gf_files,
  kn_DllInterface, kn_DLLmng, UWebBrowserWrapper, kn_ClipUtils;


function ConvertHTMLToRTF(const inFilename : WideString; var OutStream: TMemoryStream) : boolean;
var
  s: string;
  HTMLMethod : THTMLImportMethod;
  IE: TWebBrowserWrapper;
  DlgTextConvImportAsRTF:     TextConvImportAsRTFProc;
  DlgMSWordConvertHTMLToRTF : MSWordConvertHTMLToRTFProc;

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
              IE:= TWebBrowserWrapper.Create(Form_Main);

              try
                  IE.NavigateToLocalFile ( inFileName );
                  IE.CopyAll;                           // Select all and then copy to clipboard
                  s:= Clipboard.AsRTF;
                  OutStream.WriteBuffer( s[1], length(s));
                  Result := True;
              finally
                  FreeAndNil(IE);
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
        messagedlg( 'Error while exporting to HTML (method= ' + HTMLImportMethods[THTMLImportMethod(HTMLExpMethod)] + ') : ' + E.Message, mtError, [mbOK], 0 );
  end;
end; // ConvertRTFToHTML


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

finalization

end.