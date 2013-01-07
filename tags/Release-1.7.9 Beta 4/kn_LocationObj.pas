unit kn_LocationObj;
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

interface
uses Windows, Classes, SysUtils, gf_misc,
  kn_Const, kn_Info, IniFiles, WideStrings;

type
  TLocation = class( TObject )
  private
    FName : wideString;
    FFileName : wideString;
    FNoteName : wideString;
    FNodeName : wideString;
    FCaretPos : integer;
    FSelLength : integer;
    FNoteID : longint;
    FNodeID : longint;
    FExternalDoc : boolean;
    FParams : wideString;
    FTag : integer; //used in TForm_Main.List_ResFindDrawItem

    function GetDisplayText : wideString;
    function GetDisplayTextLong : wideString;
    {
    function GetPath : string;
    function GetLinkText : string;
    function GetLinkTextByNames : string;
    }

  public
    property Name : wideString read FName write FName;
    property FileName : wideString read FFileName write FFileName;
    property NoteName : wideString read FNoteName write FNoteName;
    property NodeName : wideString read FNodeName write FNodeName;
    property CaretPos : integer read FCaretPos write FCaretPos;
    property SelLength : integer read FSelLength write FSelLength;
    property NoteID : longint read FNoteID write FNoteID;
    property NodeID : longint read FNodeID write FNodeID;
    property ExternalDoc : boolean read FExternalDoc write FExternalDoc;
    property Params : wideString read FParams write FParams;
    property Tag : integer read FTag write FTag;

    property DisplayText : wideString read GetDisplayText;
    property DisplayTextLong : wideString read GetDisplayTextLong;

    {
    property Path : string read GetPath;
    property LinkText : string read GetLinkText;
    property LinkTextByNames : string read GetLinkTextByNames;
    }

    constructor Create;
    procedure Assign( const aLocation : TLocation );
  end;


type
  TNavigationHistoryTable = array[1..MAX_NAVHISTORY_COUNT] of TLocation;

var
  Location_List : TWideStringList; // used to build list of matches for the resource panel search function
  Favorites_List : TWideStringList; // favorites list (saved in keynote.fvr)
  _KNTLocation : TLocation;
  _NavHistory : TNavigationHistoryTable;
  _NavHistoryIndex : integer;

procedure ClearLocationList( const aList : TWideStringList );
procedure ClearNavigationHistoryFrom( const Start : integer );
procedure ClearNavigationHistory;

procedure LoadFavorites( const FN : string );
procedure SaveFavorites( const FN : string );

implementation
uses gf_files;

procedure LoadFavorites( const FN : string );
var
  IniFile : TWMemIniFile;
  section: string;
  name : wideString;
  sections : TStringList;
  myFav : TLocation;
  i, cnt : integer;
begin
  if ( not fileexists( FN )) then exit;
  ClearLocationList( Favorites_List );

  try
    IniFile := TWMemIniFile.Create( FN );
  except
    exit;
  end;

  sections := TStringList.Create;

  try

    IniFile.ReadSections( sections );

    cnt := sections.Count;
    for i := 0 to pred( cnt ) do
    begin
      section := sections[i];

      with IniFile do
      begin
        name := readstringW( section, 'Name', '' );
        if ( name <> '' ) then
        begin
          myFav := TLocation.Create;
          myFav.Name := name;
          myFav.FileName := readstringW( section, 'File', '' );

            myFav.NoteName := readstringW( section, 'Note', '' );
            myFav.NoteID := readinteger( section, 'NoteID', 0 );
            myFav.NodeName := readstringW( section, 'Node', '' );
            myFav.NodeID := readinteger( section, 'NodeID', 0 );
            myFav.CaretPos := readinteger( section, 'Pos', 0 );
            myFav.SelLength := readinteger( section, 'Len', 0 );
            myFav.ExternalDoc := readbool( section, 'ExternalDoc', false );
            myFav.Params := readstringW( section, 'Params', '' );
            Favorites_List.AddObject( myFav.Name, myFav )
        end;
      end;
    end;

  finally
    IniFile.Free;
    sections.Free;
  end;

end; // LoadFavorites

procedure SaveFavorites( const FN : string );
var
  IniFile : TWMemIniFile;
  section : string;
  myFav : TLocation;
  i, cnt : integer;
begin

  if ( Favorites_List.Count = 0 ) then exit;
  deletefile( FN );

  try
    IniFile := TWMemIniFile.Create( FN );
  except
    exit;
  end;

  try
    cnt := Favorites_List.Count;
    for i := 0 to pred( cnt ) do
    begin
      section := Format( '%d', [succ( i )] );
      myFav := TLocation( Favorites_List.Objects[i] );

      with IniFile do
      begin
        writestringW( section, 'Name', myFav.Name );
        writestringW( section, 'File', myFav.FileName );
        if myFav.ExternalDoc then
        begin
          writebool( section, 'ExternalDoc', myFav.ExternalDoc );
          writestringW( section, 'Params', myFav.Params );
        end
        else
        begin
          writestringW( section, 'Note', myFav.NoteName );
          writeinteger( section, 'NoteID', myFav.NoteID );
          writeinteger( section, 'NodeID', myFav.NodeID );
          writestringW( section, 'Node', myFav.NodeName );
          writeinteger( section, 'Pos', myFav.CaretPos );
          writeinteger( section, 'Len', myFav.SelLength );
        end;
      end;

    end;
    IniFile.UpdateFile;

  finally
    IniFile.Free;
  end;
end; // SaveFavorites

procedure ClearLocationList( const aList : TWideStringList );
var
  i, cnt : integer;
begin
  cnt := aList.Count;
  try
    try
      for i := 1 to cnt do
      begin
        aList.Objects[pred( i )].Free;
      end;
    except
    end;
  finally
    aList.Clear;
  end;
end; // ClearLocationList

constructor TLocation.Create;
begin
  FName := '';
  FFileName := '';
  FNoteName := '';
  FNodeName := '';
  FCaretPos := 0;
  FSelLength := 0;
  FNoteID := 0;
  FNodeID := 0;
  FExternalDoc := false;
  FParams := '';
  FTag := 0;
end; // create

procedure TLocation.Assign( const aLocation : TLocation );
begin
  FName := aLocation.Name;
  FFilename := aLocation.FileName;
  FNoteName := aLocation.NoteName;
  FNodeName := aLocation.NodeName;
  FCaretPos := aLocation.CaretPos;
  SelLength := aLocation.SelLength;
  FNoteID := aLocation.NoteID;
  FNodeID := aLocation.NodeID;
  FExternalDoc := aLocation.FExternalDoc;
  FParams := aLocation.FParams;
end; // SetKNTLocation


function TLocation.GetDisplayText : wideString;
var
  CPos : string;
begin
  result := '';
  if ( FCaretPos >= 0 ) then
    CPos := Format( '/ %d', [FCaretPos] )
  else
    CPos := '';
  if ( FNodeID > 0 ) then
    result := WideFormat(
      '%s / %s %s',
      [FNoteName, FNodeName, CPos]
    )
  else
    result := WideFormat(
      '%s %s',
      [FNoteName, CPos]
    );
end; // GetDisplayText

function TLocation.GetDisplayTextLong : wideString;
begin
  result := FFilename + ' / ' + GetDisplayText;
end; // GetDisplayTextLong

(*
function TLocation.GetPath : string;
begin
  result := ''; // [x]
end; // GetPath

function TLocation.GetLinkText : string;
begin
  result := ''; // [x]
end; // GetLinkText

function TLocation.GetLinkTextByNames : string;
begin
  result := ''; // [x]
end; // GetLinkTextByNames
*)

procedure ClearNavigationHistoryFrom( const Start : integer );
var
  i : integer;
begin
  for i := Start to MAX_NAVHISTORY_COUNT do
  begin
    if ( _NavHistory[i] <> nil ) then
    begin
      _NavHistory[i].Free;
      _NavHistory[i] := nil;
    end;
  end;
end; // ClearNavigationHistoryFrom

procedure ClearNavigationHistory;
begin
  _NavHistoryIndex := 1;
  ClearNavigationHistoryFrom( 1 );
end; // ClearNavigationHistory


Initialization
  Location_List := TWideStringList.Create;
  _KNTLocation := TLocation.Create; // used for jumps to KNT locations

  Favorites_List := TWideStringList.Create;
  Favorites_List.Sorted := true;
  Favorites_List.Duplicates := dupIgnore;

  // FillChar( _NavHistory, sizeof( _NavHistory ), 0 );
  _NavHistoryIndex := 1;

Finalization
  ClearLocationList( Location_List );
  ClearLocationList( Favorites_List );
  try
    Location_List.Free;
    Favorites_List.Free;
   _KNTLocation.Free;
  except
  end;
  // ClearNavigationHistory;

end.
