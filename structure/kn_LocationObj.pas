unit kn_LocationObj;

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
   System.Classes,
   System.SysUtils,
   System.IniFiles,
   kn_Const;

type
  TLocation = class( TObject )
  private
    FName : string;
    FFileName : string;
    FNoteName : string;
    FNodeName : string;
    FCaretPos : integer;
    FSelLength : integer;
    FNoteID : longint;
    FNodeID : longint;
    FExternalDoc : boolean;
    FParams : string;
    FTag : integer; //used in TForm_Main.List_ResFindDrawItem

    function GetDisplayText : string;
    function GetDisplayTextLong : string;
    {
    function GetPath : string;
    function GetLinkText : string;
    function GetLinkTextByNames : string;
    }

  public
    property Name : string read FName write FName;
    property FileName : string read FFileName write FFileName;
    property NoteName : string read FNoteName write FNoteName;
    property NodeName : string read FNodeName write FNodeName;
    property CaretPos : integer read FCaretPos write FCaretPos;
    property SelLength : integer read FSelLength write FSelLength;
    property NoteID : longint read FNoteID write FNoteID;
    property NodeID : longint read FNodeID write FNodeID;
    property ExternalDoc : boolean read FExternalDoc write FExternalDoc;
    property Params : string read FParams write FParams;
    property Tag : integer read FTag write FTag;

    property DisplayText : string read GetDisplayText;
    property DisplayTextLong : string read GetDisplayTextLong;

    {
    property Path : string read GetPath;
    property LinkText : string read GetLinkText;
    property LinkTextByNames : string read GetLinkTextByNames;
    }

    constructor Create;
    procedure Assign( const aLocation : TLocation );
    function Clone: TLocation;
    function Equal (Location: TLocation; considerCaretPos: boolean = true; considerOnlyKntLinks: boolean = true): boolean;
  end;


var
  Location_List : TStringList; // used to build list of matches for the resource panel search function
  Favorites_List : TStringList; // favorites list (saved in keynote.fvr)
  _KNTLocation : TLocation;

procedure ClearLocationList( const aList : TStringList );

procedure LoadFavorites( const FN : string );
procedure SaveFavorites( const FN : string );


implementation

procedure LoadFavorites( const FN : string );
var
  IniFile : TMemIniFile;
  section: string;
  name : string;
  sections : TStringList;
  myFav : TLocation;
  i, cnt : integer;
begin
  if ( not fileexists( FN )) then exit;
  ClearLocationList( Favorites_List );

  try
    IniFile := TMemIniFile.Create( FN );
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
        name := readstring( section, 'Name', '' );
        if ( name <> '' ) then
        begin
          myFav := TLocation.Create;
          myFav.Name := name;
          myFav.FileName := readstring( section, 'File', '' );

            myFav.NoteName := readstring( section, 'Note', '' );
            myFav.NoteID := readinteger( section, 'NoteID', 0 );
            myFav.NodeName := readstring( section, 'Node', '' );
            myFav.NodeID := readinteger( section, 'NodeID', 0 );
            myFav.CaretPos := readinteger( section, 'Pos', 0 );
            myFav.SelLength := readinteger( section, 'Len', 0 );
            myFav.ExternalDoc := readbool( section, 'ExternalDoc', false );
            myFav.Params := readstring( section, 'Params', '' );
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
  IniFile : TMemIniFile;
  section : string;
  myFav : TLocation;
  i, cnt : integer;
begin

  if ( Favorites_List.Count = 0 ) then exit;
  deletefile( FN );

  try
    IniFile := TMemIniFile.Create( FN );
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
        writestring( section, 'Name', myFav.Name );
        writestring( section, 'File', myFav.FileName );
        if myFav.ExternalDoc then
        begin
          writebool( section, 'ExternalDoc', myFav.ExternalDoc );
          writestring( section, 'Params', myFav.Params );
        end
        else
        begin
          writestring( section, 'Note', myFav.NoteName );
          writeinteger( section, 'NoteID', myFav.NoteID );
          writeinteger( section, 'NodeID', myFav.NodeID );
          writestring( section, 'Node', myFav.NodeName );
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

procedure ClearLocationList( const aList : TStringList );
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


function TLocation.Clone: TLocation;
begin
    Result:= TLocation.Create;
    Result.Assign(Self);
end;



function TLocation.Equal (Location: TLocation; considerCaretPos: boolean = true; considerOnlyKntLinks: boolean = true): boolean;
begin
  Result:= false;
  if not assigned(Location) then Exit;

  if FName <> Location.Name then Exit;
  if FNoteID <> Location.NoteID then Exit;
  if FNodeID <> Location.NodeID then Exit;
  if considerOnlyKntLinks then
     if FCaretPos <> Location.CaretPos then Exit;

  if not considerOnlyKntLinks then begin
     if FExternalDoc <> Location.ExternalDoc then Exit;
     if FParams <> Location.Params then Exit;
  end;

  Result:= true;
end;


function TLocation.GetDisplayText : string;
var
  CPos : string;
begin
  result := '';
  if ( FCaretPos >= 0 ) then
    CPos := Format( '/ %d', [FCaretPos] )
  else
    CPos := '';
  if ( FNodeID > 0 ) then
    result := Format(
      '%s / %s %s',
      [FNoteName, FNodeName, CPos]
    )
  else
    result := Format(
      '%s %s',
      [FNoteName, CPos]
    );
end; // GetDisplayText

function TLocation.GetDisplayTextLong : string;
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



Initialization
  Location_List := TStringList.Create;
  _KNTLocation := TLocation.Create; // used for jumps to KNT locations

  Favorites_List := TStringList.Create;
  Favorites_List.Sorted := true;
  Favorites_List.Duplicates := dupIgnore;

  // FillChar( _NavHistory, sizeof( _NavHistory ), 0 );

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
