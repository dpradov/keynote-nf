unit kn_NodeList;
                       
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
uses Windows, Classes, graphics, comctrls,
  SysUtils, gf_misc, gf_files,
  kn_Const, kn_Info,
  TreeNT;

type
  TNodeControl = (
    ncNone, ncRTF, ncIE
  );

type
  EVirtualNodeError = class( Exception);
  
  TNoteNode = class( TObject )
  private
    FStream : TMemoryStream;
    FID : longint;
    FName : string;
    FLevel : integer;
    FSelStart : integer;
    FSelLength : integer;
    FChecked : boolean;
    FFlagged : boolean;
    FBold : boolean;
    FRTFBGColor : TColor;
    FVirtualMode : TVirtualMode;
    FVirtualFN : string;
    FRelativeVirtualFN : string;
    FRTFModified : boolean;
    FExpanded : boolean;
    FImageIndex : integer;
    FHasNodeColor : boolean;
    FHasNodeBGColor : boolean;
    FNodeColor : TColor;
    FNodeBGColor : TColor;
    FHasNodeFontFace : boolean;
    FNodeFontFace : string;
    FTag : integer;
    FWordWrap : TNodeWordWrap;
    FChildrenCheckbox: Boolean;    // [dpv]
    FFiltered: Boolean;            // [dpv]: True -> Hidden
    FAlarm: TDateTime;             // [dpv]

    procedure SetName( AName : string );
    procedure SetLevel( ALevel : integer );
    procedure SetVirtualFN( aVirtualFN : string );
    function GetVirtualFN: string;
    function  GetMirrorNode: TTreeNTNode;
    procedure SetMirrorNode( aNode : TTreeNTNode );
    function GetMirrorNodePath: string;
    procedure SetMirrorNodeID( ID : string );
    procedure SetID( AID : longint );
    procedure SetNodeFontFace( const aFace : string );
    function GetRelativeVirtualFN : string;
    procedure SetWordWrap( const Value : TNodeWordWrap );
    function GetAlarm: TDateTime;

  public
    property Stream : TMemoryStream read FStream;
    property Name : string read FName write SetName;
    property ID : longint read FID write SetID;
    property Level : integer read FLevel write SetLevel;
    property SelStart : integer read FSelStart write FSelStart;
    property SelLength : integer read FSelLength write FSelLength;
    property Checked : boolean read FChecked write FChecked;
    property ChildrenCheckbox : boolean read FChildrenCheckbox write FChildrenCheckbox;    // [dpv]
    property Filtered : boolean read FFiltered write FFiltered;                            // [dpv]
    property Alarm : TDateTime read GetAlarm write FAlarm;                                   // [dpv]
    property AlarmF : TDateTime read FAlarm;
    property Flagged : boolean read FFlagged write FFlagged;
    property Tag : integer read FTag write FTag;
    property RTFBGColor : TColor read FRTFBGColor write FRTFBGColor;
    property Bold : boolean read FBold write FBold;
    property VirtualMode : TVirtualMode read FVirtualMode write FVirtualMode;
    property VirtualFN : string read GetVirtualFN write SetVirtualFN;
    property RelativeVirtualFN : string read GetRelativeVirtualFN write FRelativeVirtualFN;

    property MirrorNodePath : string read GetMirrorNodePath;
    property MirrorNode: TTreeNTNode read GetMirrorNode write SetMirrorNode;
    property MirrorNodeID: string read FVirtualFN write SetMirrorNodeID;
    procedure AddedMirrorNode;
    procedure RemovedAllMirrorNodes;
    function HasMirrorNodes: boolean;

    property Expanded : boolean read FExpanded write FExpanded;
    property ImageIndex : integer read FImageIndex write FImageIndex;
    property HasNodeColor : boolean read FHasNodeColor write FHasNodeColor;
    property HasNodeBGColor : boolean read FHasNodeBGColor write FHasNodeBGColor;
    property NodeColor : TColor read FNodeColor write FNodeColor;
    property NodeBGColor : TColor read FNodeBGColor write FNodeBGColor;
    property HasNodeFontFace : boolean read FHasNodeFontFace;
    property NodeFontFace : string read FNodeFontFace write SetNodeFontFace;
    property WordWrap : TNodeWordWrap read FWordWrap write SetWordWrap;

    property RTFModified : boolean read FRTFModified write FRTFModified;

    constructor Create;
    destructor Destroy; override;

    function PropertiesToFlagsString : TFlagsString;
    procedure FlagsStringToProperties( const FlagsStr : TFlagsString );

    procedure Assign( Source : TNoteNode );
    procedure LoadVirtualFile;
    procedure SaveVirtualFile;
    procedure LoadMirrorNode;
    function HasVNodeError : boolean;

  end;

type
  TNodeList = class( TList )
  private
    function GetNode( index : integer ) : TNoteNode;
    procedure PutNode( index : integer; item : TNoteNode );
  public
    property Items[index:integer] : TNoteNode read GetNode write PutNode; default;
    constructor Create;
    destructor Destroy; override;
    function Remove( item : TNoteNode ) : integer;
    procedure Delete( index : integer );
    function HasVirtualNodes : boolean;
    procedure Insert( const aIndex : integer; const item : TNoteNode );
  end;

var
  _VNDoBackup : boolean;
  _VNBackupExt : string;
  _VNBackupAddExt : boolean;
  _VNBackupDir : string;
  _VNKeyNoteFileName : string;


implementation
uses kn_Global,
     StrUtils, kn_TreeNoteMng, kn_NoteObj, kn_LinksMng, kn_LocationObj;

constructor TNoteNode.Create;
begin
  inherited Create;
  FName := '';
  FID := 0;
  FLevel := 0;
  FSelStart := 0;
  FSelLength := 0;
  FChecked := false;
  FFlagged := false;
  FTag := 0;
  FBold := false;
  FRTFBGColor := clWindow;
  FExpanded := true;
  FVirtualMode := vmNone;
  FVirtualFN := '';
  FRelativeVirtualFN := '';
  FImageIndex := 0;
  FRTFModified := false;
  FHasNodeColor := false;
  FHasNodeBGColor := false;
  FStream := TMemoryStream.Create;
  FNodeColor := clWindowText;
  FNodeBGColor := clWindow;
  FHasNodeFontFace := false;
  FNodeFontFace := '';
  FWordWrap := wwAsNote;
  FChildrenCheckbox:= false;   // [dpv]
  FFiltered:= false;           // [dpv]
  FAlarm := 0;                 // [dpv]
end; // CREATE

destructor TNoteNode.Destroy;
begin
  if assigned( FStream ) and (FVirtualMode <> vmKNTNode) then begin
     FStream.Free;
  end;
  inherited Destroy;
end; // DESTROY

procedure TNoteNode.SetID( AID : longint );
begin
  if ( FID = 0 ) then
    FID := AID;
  // otherwise, never allow the ID to be modified
end; // SetID

function TNoteNode.PropertiesToFlagsString : TFlagsString;
begin
  result := DEFAULT_FLAGS_STRING;
  result[1] := BOOLEANSTR[FChecked];
  result[2] := BOOLEANSTR[FFlagged];
  result[3] := BOOLEANSTR[FBold];

  {
  if ( not ( FHasNodeColor or FHasNodeBGColor )) then
  begin
    result[4] := _FlagHasNodeColorNone;
  end
  else
  if FHasNodeColor then
  begin
    if FHasNodeBGColor then
      result[4] := _FlagHasNodeColorBoth
    else
      result[4] := _FlagHasNodeColor;
  end
  else
  if FHasNodeBGColor then
  begin
    result[4] := _FlagHasNodeBGColor;
  end;
  }
  // result[5] := BOOLEANSTR[FFontBright]; UNUSED (obsolete)
  result[6] := inttostr( ord( FVirtualMode ))[1];
  result[7] := BOOLEANSTR[FExpanded];
  result[8] := BOOLEANSTR[FHasNodeColor];
  result[9] := BOOLEANSTR[FHasNodeBGColor];

  case FWordWrap of
    wwAsNote : result[10] := '0';
    wwYes : result[10] := '1';
    else
      result[10] := '2';
  end;
  result[11] := BOOLEANSTR[FChildrenCheckbox];        // [dpv]
  result[12] := BOOLEANSTR[FFiltered];                // [dpv]

end; // PropertiesToFlagsString

procedure TNoteNode.FlagsStringToProperties( const FlagsStr : TFlagsString );
begin
  if ( length( FlagsStr ) < FLAGS_STRING_LENGTH ) then exit;
  FChecked     := FlagsStr[1] = BOOLEANSTR[true];
  FFlagged     := FlagsStr[2] = BOOLEANSTR[true];
  FBold        := FlagsStr[3] = BOOLEANSTR[true];

  FHasNodeColor := FlagsStr[8] = BOOLEANSTR[true];
  FHasNodeBGColor := FlagsStr[9] = BOOLEANSTR[true];


  case FlagsStr[10] of
    '1' : FWordWrap := wwYes;
    '2' : FWordWrap := wwNo;
    else
      FWordWrap := wwAsNote;
  end;

  // backward-compatibility hassle:
  case FlagsStr[4] of
    '1'..'9' : begin
      FHasNodeColor := true;
      if ( FlagsStr[5] = BOOLEANSTR[true] ) then // "bright font" flag
        FNodeColor := _NODE_COLORS_LIGHT[strtoint( FlagsStr[4] )]
      else
        FNodeColor := _NODE_COLORS_DARK[strtoint( FlagsStr[4] )];
    end;
  end;
  FVirtualMode := TVirtualMode( StrToInt( FlagsStr[6] ));
  FExpanded    := FlagsStr[7] = BOOLEANSTR[true];
  FChildrenCheckbox:= FlagsStr[11] = BOOLEANSTR[true];    // [dpv]
  FFiltered:= FlagsStr[12] = BOOLEANSTR[true];            // [dpv]
end; // FlagsStringToProperties


procedure TNoteNode.SetVirtualFN( aVirtualFN : string );
var
  myExt : string;
begin
  aVirtualFN := normalFN( aVirtualFN );

  try

    if ( FVirtualMode <> vmNone ) then
    begin
      FVirtualFN := aVirtualFN;
      exit;
    end;

    if ( aVirtualFN <> FVirtualFN ) then
    begin
      FVirtualFN := aVirtualFN;
      if ( FVirtualFN <> '' ) then
      begin
        { procedure moved to TForm_Main.VirtualNodeProc
        if IsDriveRemovable( FVirtualFN ) then
        begin
          raise Exception.CreateFmt( 'Cannot link virtual node to a file on removable drive %s:\ ', [Extractfiledrive( FVirtualFN )] );
          FVirtualMode := vmNone;
          FVirtualFN := '';
        end;
        }

        myExt := lowercase( extractfileext( FVirtualFN ));
        if ( myExt = ext_RTF ) then
          FVirtualMode := vmRTF
        else
        if ( pos( 'htm', myExt ) > 0 ) then
          FVirtualMode := vmHTML
        else
          FVirtualMode := vmText;
      end
      else
      begin
        FVirtualMode := vmNone;
      end;
    end
    else
    begin
      begin
        FVirtualFN := '';
      end;
    end;
  finally
    // FRelativeVirtualFN := ExtractRelativePath( _VNKeyNoteFileName, FVirtualFN );
  end;
end; // SetVirtualFN


function TNoteNode.HasMirrorNodes: boolean;
begin
   Result:= (FTag = 1);
end;

procedure TNoteNode.AddedMirrorNode;
begin
   FTag := 1;
end;
procedure TNoteNode.RemovedAllMirrorNodes;
begin
   FTag := 0;
end;

function TNoteNode.GetVirtualFN: string;
begin
    if FVirtualMode <> vmKNTNode then
       Result:= FVirtualFN
    else
       Result:= GetMirrorNodePath;
end;

procedure TNoteNode.SetMirrorNodeID( ID : string );
begin
    FVirtualMode := vmKNTNode;
    FVirtualFN:= ID;
end;

function TNoteNode.GetMirrorNode: TTreeNTNode;
var
   p: integer;
begin
    if FVirtualMode = vmKNTNode then begin
       p := pos( KNTLINK_SEPARATOR, FVirtualFN );
       Result:= GetTreeNode(strtoint( AnsiLeftStr(FVirtualFN, p-1) ), strtoint( AnsiMidStr (FVirtualFN, p+1,255)));
    end
    else
      Result:= nil;
end; // GetVirtualKNTNode


function TNoteNode.GetMirrorNodePath: string;
var
   note: TTabNote;
   node : TTreeNTNode;
begin
     node:= GetMirrorNode;
     if assigned(Node) then begin
        note:= NoteFile.GetNoteByTreeNode(node);
        Result:= PathOfKNTLink(node, note, -1);
     end;
end;

procedure TNoteNode.LoadMirrorNode;
var
   Node : TTreeNTNode;
begin
    if FVirtualMode = vmKNTNode then begin
       try
         node:= GetMirrorNode;
         FStream:= TNoteNode(Node.Data).FStream;       // This node shares its content with the other node
         FStream.Position := 0;
       finally
       end;
    end;
end; // LoadVirtualKNTNode

procedure TNoteNode.SetMirrorNode( aNode : TTreeNTNode );
var
   aNote: TTabNote;
begin
     if assigned(aNode) then begin
         if FVirtualMode <> vmKNTNode then begin
            FStream.Free;
         end;

         if TNoteNode(aNode.Data).VirtualMode = vmKNTnode then          // point to non virtual node
            aNode:= TNoteNode(aNode.Data).MirrorNode;

         if assigned(aNode) then begin
            aNote:= NoteFile.GetNoteByTreeNode(aNode);
            FVirtualFN:= inttostr(aNote.ID) + KNTLINK_SEPARATOR + inttostr(TNoteNode(aNode.Data).ID);
            FVirtualMode := vmKNTNode;
            FStream:= TNoteNode(aNode.Data).FStream;   // This node shares its content with the other node
         end;
     end
     else
       if FVirtualMode = vmKNTNode then begin
           FVirtualMode:= vmNone;
           FVirtualFN:= '';
           FStream := TMemoryStream.Create;
       end;

     FStream.Position := 0;
end; // SetVirtualKNTNode

function TNoteNode.GetAlarm: TDateTime;
var
   Node : TTreeNTNode;
begin
    if (FVirtualMode= vmKNTNode) then begin
       Node:= MirrorNode;
       Result:= TNoteNode(Node.Data).Alarm;
    end
    else
       Result:= FAlarm;
end;

procedure TNoteNode.LoadVirtualFile;
var
  NewVirtualFN : string;
begin
  FStream.Clear;
  FStream.Position := 0;

  if ( not fileexists( FVirtualFN )) then
  begin
    // try relative path
    if ( FRelativeVirtualFN <> '' ) then
    begin
      NewVirtualFN := ExpandFileName( FRelativeVirtualFN ); // must set current dir previously to the KeyNOte file's dir, using ChDir
      if fileexists( NewVirtualFN ) then
        FVirtualFN := NewVirtualFN;
    end;
  end;

  if ( FVirtualMode in [vmText, vmRTF, vmHTML] ) then
    FStream.LoadFromFile( FVirtualFN );
  FStream.Position := 0;
end; // LoadVirtualFile

procedure TNoteNode.SaveVirtualFile;
var
  bakFN : string;
begin
  if ( FVirtualMode in [vmText, vmRTF, vmHTML] ) then
  begin
    FRelativeVirtualFN := ExtractRelativePath( _VNKeyNoteFileName, FVirtualFN );

    // only saved file if was actually changed
    if ( FRTFModified or ( not fileexists( FVirtualFN ))) then
    begin
      // back up the original file. Problem:
      // we have no access to KeyOptions here,
      // so we don't have all the information
      // necessary to create backup files.
      // We use GLOBAL variables instead. Bad design.
      if _VNDoBackup then
      begin
        case _VNBackupAddExt of
          false : bakFN := changefileext( FVirtualFN, _VNBackupExt );
          true : bakFN := FVirtualFN + _VNBackupExt;
        end;

        if ( _VNBackupDir <> '' ) then
          bakFN := _VNBackupDir + extractfilename( bakFN );

        CopyFile( PChar( FVirtualFN ), PChar( bakFN ), false );
      end;

      FStream.SaveToFile( FVirtualFN );
    end;
  end;
  FRTFModified := false;
end; // SaveVirtualFile

procedure TNoteNode.Assign( Source : TNoteNode );
begin
  if ( not assigned( Source )) then exit;

  FName := Source.Name;
  FLevel := Source.Level;
  FStream.Clear;
  FStream.LoadFromStream( Source.Stream );
  FStream.Position := 0;
  FChecked := Source.Checked;
  FFlagged := Source.Flagged;
  FTag := Source.Tag;
  FBold := Source.Bold;
  FSelStart := Source.SelStart;
  FSelLength := Source.SelLength;
  FRTFBGColor := Source.RTFBGColor;
  FVirtualMode := Source.VirtualMode;
  FVirtualFN := Source.FVirtualFN; 
  FRelativeVirtualFN := ''; // auto-create when saving
  FExpanded := Source.Expanded;
  FImageIndex := Source.ImageIndex;
  FHasNodeColor := Source.HasNodeColor;
  FHasNodeBGColor := Source.HasNodeBGColor;
  FNodeColor := Source.NodeColor;
  FNodeBGColor := Source.NodeBGColor;
  FRTFModified := false;
  FHasNodeFontFace := Source.HasNodeFontFace;
  FNodeFontFace := Source.NodeFontFace;
  FWordWrap := Source.WordWrap;
  FChildrenCheckbox:= Source.FChildrenCheckbox;  // [dpv]
  FFiltered:= Source.FFiltered;                  // [dpv]
  FAlarm:= Source.FAlarm;

end; // Assign


procedure TNoteNode.SetNodeFontFace( const aFace : string );
begin
  FNodeFontFace := aFace;
  FHasNodeFontFace := ( aFace <> '' );
end; // SetNodeFontFace

procedure TNoteNode.SetWordWrap( const Value : TNodeWordWrap );
begin
  FWordWrap := Value;
end; // SetWordWrap

function TNoteNode.HasVNodeError : boolean;
begin
  result := (( FVirtualFN <> '' ) and
    ( FVirtualFN[1] = _VIRTUAL_NODE_ERROR_CHAR ));
end; // HasVNodeError

procedure TNoteNode.SetName( AName : string );
begin
  if ( AName <> FName ) then
  begin
    FName := AName;
    // must enforce max node name length, because
    // if it exceeds 255 characters, KeyNote will crash
    // on Windows 95/98
    if ( length( FName ) > TREENODE_NAME_LENGTH ) then
      delete( FName, TREENODE_NAME_LENGTH, length( FName ));
    {
    if ( _ALLOW_VCL_UPDATES and assigned( FTreeNode )) then
      FTreeNode.Text := FName;
    }
  end;
end; // SetName

procedure TNoteNode.SetLevel( ALevel : integer );
begin
  if ( ALevel <> FLevel ) then
    FLevel := ALevel;
  if ( FLevel < 0 ) then FLevel := 0;
end; // SetLevel

function TNoteNode.GetRelativeVirtualFN : string;
begin
  if ( FRelativeVirtualFN = '' ) then
  begin
    FRelativeVirtualFN := ExtractRelativePath( _VNKeyNoteFileName, FVirtualFN );
  end;
  result := FRelativeVirtualFN;
end; // GetRelativeVirtualFN

// ------- TNODELIST ---------

constructor TNodeList.Create;
begin
  inherited Create;
end; // CREATE

destructor TNodeList.Destroy;
var
  i : integer;
begin
  if ( Count > 0 ) then
    for i := 0 to pred( Count ) do
      if assigned( Items[i] ) then
      begin
        Items[i].Free;
      end;
  Clear;
  inherited Destroy;
end; // DESTROY


function TNodeList.GetNode( index : integer ) : TNoteNode;
begin
  result := TNoteNode( inherited items[index] );
end; // PutStream

procedure TNodeList.PutNode( index : integer; item : TNoteNode );
begin
  inherited Put( index, item );
end; // PutStream

function TNodeList.Remove( item : TNoteNode ) : integer;
begin
  if assigned( item ) then Item.Free;
  result := inherited remove( item );
end; // Remove

procedure TNodeList.Delete( index : integer );
begin
  if (( index >= 0 ) and ( index < Count ) and assigned( items[index] )) then
    Items[index].Free;
  inherited Delete( index );
end; // Delete

function TNodeList.HasVirtualNodes : boolean;
var
  i : integer;
begin
  result := false;
  if ( Count = 0 ) then exit;
  for i := 0 to pred( Count ) do
  begin
    if ( Items[i].VirtualMode <> vmNone ) and
       ( Items[i].VirtualMode <> vmKNTnode ) then  // HasVirtualNodes: Virtual and extern...
    begin
      result := true;
      break;
    end;
  end;
end; // HasVirtualNodes


procedure TNodeList.Insert( const aIndex : integer; const item : TNoteNode );
begin
  inherited Insert( aIndex, item );
end; // Insert

Initialization
  _VNDoBackup := true;
  _VNBackupExt := ext_BAK;
  _VNBackupAddExt := true;
  _VNBackupDir := '';
  _VNKeyNoteFileName := '';

end.
