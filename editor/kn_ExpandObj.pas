unit kn_ExpandObj;

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
uses Windows, Classes, SysUtils, WideStrings;

function LoadGlossaryInfo( const FN : string ) : boolean;
function SaveGlossaryInfo( const FN : string ) : boolean;

var
  GlossaryList : TWideStringList;
  Glossary_FN : string;

implementation
uses TntSystem;

function LoadGlossaryInfo( const FN : string ) : boolean;
var
   AnsiGlossaryList: TStringList;
begin
  result := false;
  if ( not assigned( GlossaryList )) then exit;
  if ( not fileexists( FN )) then exit;

  try
    AnsiGlossaryList:= TStringList.Create;
    AnsiGlossaryList.LoadFromFile( FN );
    GlossaryList.Clear;
    GlossaryList.Text:= UTF8ToWideString(AnsiGlossaryList.Text);
  finally
    AnsiGlossaryList.Free;
  end;
  result := true;
end; // LoadGlossaryInfo

function SaveGlossaryInfo( const FN : string ) : boolean;
var
  AnsiGlossaryList: TStringList;
begin
  result := false;
  if ( not assigned( GlossaryList )) then exit;
  try
    AnsiGlossaryList:= TStringList.Create;
    AnsiGlossaryList.Text:= WideStringToUTF8(GlossaryList.Text);
    AnsiGlossaryList.SaveToFile( Glossary_FN );
  finally
    AnsiGlossaryList.Free;
  end;

  result := true;
end; // SaveGlossaryInfo

Initialization
  GlossaryList := TWideStringList.Create;
  with GlossaryList do
  begin
    sorted := true;
    duplicates := dupError;
  end;

Finalization

  if ( GlossaryList <> nil ) then
    GlossaryList.Free;
  GlossaryList := nil;
  
end.
