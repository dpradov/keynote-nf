unit MSWordConverter;

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
   gf_files;



function MSWordConvertHTMLToRTF(const inFilename : string; var OutStream: TMemoryStream) : boolean;
function MSWordConvertRTFToHTML(const outFilename : string; const RTF: AnsiString) : boolean;
function MSWordQuit() : boolean;

implementation

uses knt.App;

var
  _WordApp: WordXP.WordApplication;


// -----------------------------------------------------------------
//   MSWordConvertHTMLToRTF
// -----------------------------------------------------------------
function MSWordConvert(const inFilename : string; const outFilename : string; wdFormatOut: OleVariant) : boolean;
var
  s: string;
  vFalse : OleVariant;
  WD : _Document;
  WordApp : TWordApplication;
  oleInFileName, oleOutFileName : OleVariant;

begin
  result := False;

  try
      s := inFileName;
      oleInFileName := s;

      WordApp := TWordApplication.Create( nil );
      WordApp.AutoQuit := false;
      try
        if _WordApp <> nil then begin
           WordApp.ConnectTo(_WordApp);
           try
             WordApp.Visible:= False;   // Just to see if the application is still connected (user could have killed WINWORD process)
           except
            _WordApp:= nil;
           end;
        end;
        if _WordApp = nil then begin
           With WordApp do begin
               ConnectKind := ckNewInstance;
               Connect;
               _WordApp:= WordApp.Application;

           Options.ConfirmConversions := false;
           Options.SavePropertiesPrompt := false;
           DisplayAlerts := wdAlertsNone;
           end;
        end;


        try
          vFalse := false;
          WD := WordApp.Documents.Open( oleInFileName, vFalse, EmptyParam, vFalse,
                       EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
                       EmptyParam, EmptyParam, vFalse, EmptyParam, EmptyParam, EmptyParam );

          oleOutFileName := OutFileName;

          WD.SaveAs( oleOutFileName, wdFormatOut,
             EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam );

          WD.Close( vFalse, EmptyParam, EmptyParam );

          result := True;


        finally
          WordApp.Disconnect;
        end;

      finally
        WordApp.Free;
      end;


  except
    on E : Exception do
        App.ErrorPopup( 'Error while converting text: ' + E.Message, E);
  end;
end; // MSWordConvert

// -----------------------------------------------------------------
//   MSWordConvertHTMLToRTF
// -----------------------------------------------------------------
function MSWordConvertHTMLToRTF(const inFilename : string; var OutStream: TMemoryStream) : boolean;
var
  tmpFN, tmpDirectory: string;
begin
  Result := False;

  try
    tmpDirectory:= GetTempDirectory;
    tmpFN:= tmpDirectory + RandomFileName( tmpDirectory, '.tmp', 8 );

    if MSWordConvert(inFilename, tmpFN, wdFormatRTF) then begin
       OutStream.LoadFromFile(tmpFN);
       DeleteFile(tmpFN);
       Result:= true
    end;

  except
    on E : Exception do
        App.ErrorPopup( 'Error while converting text: ' + E.Message, E);
  end;

end; // MSWordConvertHTMLToRTF


// -----------------------------------------------------------------
//   MSWordConvertRTFtoHTML
// -----------------------------------------------------------------
function MSWordConvertRTFToHTML(const outFilename : string; const RTF: AnsiString) : boolean;
var
  tmpFN, tmpDirectory: string;
  Stream: TMemoryStream;
begin
  result := False;

  try
      Stream:= TMemoryStream.Create;
      try
         tmpDirectory:= GetTempDirectory;
         tmpFN:= tmpDirectory + RandomFileName( tmpDirectory, '.rtf', 8 );
         Stream.WriteBuffer(RTF[1], length(RTF));
         Stream.SaveToFile(tmpFN);
      finally
         Stream.Free;
      end;

    Result:= MSWordConvert(tmpFN, outFilename, wdFormatHTML);

    DeleteFile(tmpFN);

  except
    on E : Exception do
        App.ErrorPopup( 'Error while converting text: ' + E.Message, E);
  end;
end; // MSWordConvertRTFtoHTML


// -----------------------------------------------------------------
//   MSWordQuit
// -----------------------------------------------------------------
function MSWordQuit: boolean;
var
   WordApp : TWordApplication;
begin
   Result:= False;
   if assigned(_WordApp) then begin
      WordApp := TWordApplication.Create( nil );
      try
          WordApp.ConnectTo(_WordApp);
          WordApp.Quit;
          WordApp.Free;

          Result:= True
      except
      end;
   end;
end;

end.
