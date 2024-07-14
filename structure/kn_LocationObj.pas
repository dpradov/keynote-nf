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
   System.Generics.Collections,

   VirtualTrees.Types,

   kn_KntFolder,
   knt.model.note;


type
  TLocation = class;
  TLocationList = TList<TLocation>;


  TLocation = class(TObject)
  protected
    function GetNode: PVirtualNode;

  public
    FileName : string;

    Name : string;
    ExternalDoc : boolean;
    Params : string;

    Folder: TKntFolder;
    NNode: TNoteNode;
    NEntry: TNoteEntry;
    Calculated: boolean;

    FolderID : Cardinal;
    NNodeID : Word;
    NNodeGID : Cardinal;
    NEntryID : Word;        // %%%
    Mark : byte;            // To be used with KNT links (InsertOrMarkKNTLink)
    CaretPos : integer;
    SelLength : integer;

    Bookmark09: boolean;     // In case FMark <> 0, Is it one of the bookmarks set with Search|Set Bookmark?
    ScrollPosInEditor: TPoint;

    FolderName: string;       // to use in certain cases
    NoteName: string;

    constructor Create;
    procedure Assign( const aLocation : TLocation );
    function Clone: TLocation;
    function Equal (Location: TLocation; considerCaretPos: boolean = true; considerOnlyKntLinks: boolean = true): boolean;

    property Node: PVirtualNode read GetNode;

    function GetDisplayText : string;
    function GetDisplayTextLong : string;
  end;


var
  Location_List : TLocationList; // used to build list of matches for the resource panel search function
  Favorites_List : TLocationList; // favorites list (saved in keynote.fvr)
  _KNTLocation : TLocation;

procedure ClearLocationList( const aList : TLocationList);

procedure LoadFavorites( const FN : string );
procedure SaveFavorites( const FN : string );


implementation
uses
  kn_Const;

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
    for i := 0 to pred( cnt ) do begin
      section := sections[i];

      with IniFile do begin
        name := readstring( section, 'Name', '' );
        if ( name <> '' ) then begin
          myFav := TLocation.Create;
          myFav.Name := name;
          myFav.FileName := readstring( section, 'File', '' );

            myFav.FolderName := readstring( section, 'Note', '' );     // Knt Folder...
            myFav.FolderID := readinteger( section, 'NoteID', 0 );     // Knt FolderID...
            myFav.NoteName := readstring( section, 'Node', '' );
            myFav.NNodeID := readinteger( section, 'NodeID', 0 );
            myFav.NNodeGID := readinteger( section, 'NodeGID', 0 );
            myFav.CaretPos := readinteger( section, 'Pos', 0 );
            myFav.SelLength := readinteger( section, 'Len', 0 );
            myFav.ExternalDoc := readbool( section, 'ExternalDoc', false );
            myFav.Params := readstring( section, 'Params', '' );
            Favorites_List.Add(myFav )
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
    for i := 0 to pred( cnt ) do begin
      section := Format( '%d', [succ( i )] );
      myFav := TLocation( Favorites_List[i] );

      with IniFile do begin
        writestring( section, 'Name', myFav.Name );
        writestring( section, 'File', myFav.FileName );
        if myFav.ExternalDoc then begin
          writebool( section, 'ExternalDoc', myFav.ExternalDoc );
          writestring( section, 'Params', myFav.Params );
        end
        else begin
          writestring( section, 'Note', myFav.FolderName );         // Knt Folder...
          writeinteger( section, 'NoteID', myFav.FolderID );      // Knt FolderID...
          writeinteger( section, 'NodeID', myFav.NNodeID );
          writeinteger( section, 'NodeGID', myFav.NNodeGID );
          writestring( section, 'Node', myFav.NoteName );
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

procedure ClearLocationList( const aList : TLocationList );
var
  i, cnt : integer;
begin
  cnt := aList.Count;
  try
    try
      for i := 1 to cnt do
        aList[pred( i )].Free;
    except
    end;
  finally
    aList.Clear;
  end;
end; // ClearLocationList

constructor TLocation.Create;
begin
  Name := '';
  FileName := '';
  FolderName:= '';
  NoteName:= '';
  CaretPos := 0;
  SelLength := 0;
  FolderID := 0;
  NNodeID := 0;
  NNodeGID := 0;
  NEntryID := 0;
  Mark := 0;
  ExternalDoc := false;
  Params := '';
  Mark:= 0;
  Bookmark09:= false;
  ScrollPosInEditor.X:= -1;
  ScrollPosInEditor.Y:= -1;

  Folder:= nil;
  NNode:= nil;
  NEntry:= nil;
  Calculated:= false;

end; // create

procedure TLocation.Assign( const aLocation : TLocation );
begin
  Name := aLocation.Name;
  Filename := aLocation.FileName;
  FolderName := aLocation.FolderName;
  NoteName := aLocation.NoteName;
  CaretPos := aLocation.CaretPos;
  SelLength := aLocation.SelLength;
  FolderID := aLocation.FolderID;
  NNodeID := aLocation.NNodeID;
  NNodeGID := aLocation.NNodeGID;
  NEntryID := aLocation.NEntryID;
  Mark :=  aLocation.Mark;
  Bookmark09:= aLocation.Bookmark09;
  ExternalDoc := aLocation.ExternalDoc;
  Params := aLocation.Params;
  ScrollPosInEditor:= aLocation.ScrollPosInEditor;

  Folder:= aLocation.Folder;
  NNode:= aLocation.NNode;
  NEntry:= aLocation.NEntry;
  Calculated:= aLocation.Calculated;

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

  if Name <> Location.Name then Exit;
  if FolderID <> Location.FolderID then Exit;
  if NNodeID <> Location.NNodeID then Exit;
  if NNodeGID <> Location.NNodeGID then Exit;
  if NEntryID <> Location.NEntryID then Exit;
  if considerOnlyKntLinks then begin
     if considerCaretPos then begin
        if CaretPos <> Location.CaretPos then Exit;
        if Mark <> Location.Mark then Exit;
        if Bookmark09 <> Location.Bookmark09 then Exit;
     end;
  end;

  if not considerOnlyKntLinks then begin
     if ExternalDoc <> Location.ExternalDoc then Exit;
     if Params <> Location.Params then Exit;
  end;

  Result:= true;
end;


function TLocation.GetNode: PVirtualNode;
begin
   Result:= nil;
   if NNode <> nil then
      Result:= NNode.TVNode;
end;

function TLocation.GetDisplayText : string;
var
  CPos : string;
begin
  result := '';
  if (CaretPos >= 0) then
     CPos := Format( '/ %d', [CaretPos] )
  else
     CPos := '';

  if ( NNodeID > 0 ) then
     result := Format('%s / %s %s', [FolderName, NoteName, CPos])
  else
     result := Format('%s %s',      [FolderName, CPos] );
end;


function TLocation.GetDisplayTextLong : string;
begin
  result := Filename + ' / ' + GetDisplayText;
end;



Initialization
  Location_List := TLocationList.Create;
  _KNTLocation := TLocation.Create; // used for jumps to KNT locations

  Favorites_List := TLocationList.Create;


Finalization
  ClearLocationList( Location_List );
  ClearLocationList( Favorites_List );
  try
    Location_List.Free;
    Favorites_List.Free;
   _KNTLocation.Free;
  except
  end;

end.
