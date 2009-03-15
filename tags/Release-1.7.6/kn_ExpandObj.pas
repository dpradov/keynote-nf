
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
 <cicho@tenbit.pl>

************************************************************ *)

unit kn_ExpandObj;

interface
uses Windows, Classes, SysUtils;

function LoadGlossaryInfo( const FN : string ) : boolean;
function SaveGlossaryInfo( const FN : string ) : boolean;

var
  GlossaryList : TStringList;
  Glossary_FN : string;

implementation

function LoadGlossaryInfo( const FN : string ) : boolean;
begin
  result := false;
  if ( not assigned( GlossaryList )) then exit;
  if ( not fileexists( FN )) then exit;

  GlossaryList.Clear;
  GlossaryList.LoadFromFile( FN );
  result := true;

end; // LoadGlossaryInfo

function SaveGlossaryInfo( const FN : string ) : boolean;
begin
  result := false;
  if ( not assigned( GlossaryList )) then exit;
  GlossaryList.SaveToFile( FN );
  result := true;
end; // SaveGlossaryInfo

Initialization
  GlossaryList := TStringList.Create;
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
