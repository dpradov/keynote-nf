unit MSWordConverter;

{ -----------------------------------------------------------------------------}
{ Modified by Daniel Prado <dprado.keynote@gmail.com> on Dec 2012              }

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
   gf_files, kn_const,
   kn_DLLInterface;


function MSWordConvertHTMLToRTF(const inFilename : WideString; var OutStream: TMemoryStream) : boolean;
function MSWordConvertRTFToHTML(const outFilename : WideString; const RTF: string) : boolean;
function MSWordQuit() : boolean;

implementation

var
  _WordApp: WordXP.WordApplication;


// -----------------------------------------------------------------
//   MSWordConvertHTMLToRTF
// -----------------------------------------------------------------
function MSWordConvert(const inFilename : WideString; const outFilename : WideString; wdFormatOut: OleVariant) : boolean;
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
        messagedlg( 'Error while converting text: ' + E.Message, mtError, [mbOK], 0 );
  end;
end; // MSWordConvert

// -----------------------------------------------------------------
//   MSWordConvertHTMLToRTF
// -----------------------------------------------------------------
function MSWordConvertHTMLToRTF(const inFilename : WideString; var OutStream: TMemoryStream) : boolean;
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
        messagedlg( 'Error while converting text: ' + E.Message, mtError, [mbOK], 0 );
  end;

end; // MSWordConvertHTMLToRTF


// -----------------------------------------------------------------
//   MSWordConvertRTFtoHTML
// -----------------------------------------------------------------
function MSWordConvertRTFToHTML(const outFilename : WideString; const RTF: string) : boolean;
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
        messagedlg( 'Error while converting text: ' + E.Message, mtError, [mbOK], 0 );
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
