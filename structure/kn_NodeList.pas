unit kn_NodeList;

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
   System.StrUtils,
   Vcl.Graphics,
   Vcl.ComCtrls,
   TreeNT,
   gf_misc,
   gf_files,
   kn_Const,
   kn_Info;



type
  TNodeControl = (
    ncNone, ncRTF, ncIE
  );

type
  EVirtualNodeError = class( Exception);

  TNoteNode = class( TObject )
  private
    FStream : TMemoryStream;
    FNodeTextPlain : string;
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

  public
    property Stream : TMemoryStream read FStream;
    property NodeTextPlain : string read FNodeTextPlain write FNodeTextPlain;
    property Name : string read FName write SetName;
    property ID : longint read FID write SetID;
    property Level : integer read FLevel write SetLevel;
    property SelStart : integer read FSelStart write FSelStart;
    property SelLength : integer read FSelLength write FSelLength;
    property Checked : boolean read FChecked write FChecked;
    property ChildrenCheckbox : boolean read FChildrenCheckbox write FChildrenCheckbox;    // [dpv]
    property Filtered : boolean read FFiltered write FFiltered;                            // [dpv]
    property Flagged : boolean read FFlagged write FFlagged;
    property Tag : integer read FTag write FTag;
    property RTFBGColor : TColor read FRTFBGColor write FRTFBGColor;
    property Bold : boolean read FBold write FBold;
    property VirtualMode : TVirtualMode read FVirtualMode write FVirtualMode;
    property VirtualFN : string read GetVirtualFN write SetVirtualFN;
    property RelativeVirtualFN : string read GetRelativeVirtualFN write FRelativeVirtualFN;
    function NonVirtualNode: TNoteNode;
    function GetAlarms(considerDiscarded: boolean): TList;
    function HasAlarms (considerDiscarded: boolean): boolean;

    property MirrorNodePath : string read GetMirrorNodePath;
    property MirrorNode: TTreeNTNode read GetMirrorNode write SetMirrorNode;
    property MirrorNodeID: string read FVirtualFN write SetMirrorNodeID;
    procedure AddedMirrorNode;
    procedure RemovedAllMirrorNodes;
    function HasMirrorNodes: boolean;
    procedure ForceID( AID : longint );

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
uses
   kn_Global,
   kn_TreeNoteMng,
   kn_NoteObj,
   kn_LinksMng
   ;

resourcestring
  STR_01 = 'Unable to load the mirror node.' +  #13#10 + 'Node non virtual to wich this node was linked could not be found';



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

procedure TNoteNode.ForceID( AID : longint );
begin
   FID := AID;
end;

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
  result[6] := AnsiChar(inttostr( ord( FVirtualMode ))[1]);
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

        myExt := lowercase( ExtractFileExt( FVirtualFN ));
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
    // *1
    // Commented because it is more convenient to set the relative virtual path when it is necessary, on saving
    // the .knt file. This path will depend on the directory where the .knt is finally saved, and this can be vary,
    // for example, if Save As.. or Copy To.. are used.
    // When saving the .knt file, it will end up calling SaveVirtualFile, where FRelativeVirtualFN will be calculated.
    // Also, will be determined on accesing to GetRelativeVirtualFN. In both cases, it is obtained with the help of
    // _VNKeyNoteFileName, where is set the base path

    // FRelativeVirtualFN := ExtractRelativePath( _VNKeyNoteFileName, FVirtualFN );  // *1
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
        Result:= PathOfKNTLink(node, note, -1, false, false);
     end;
end;

procedure TNoteNode.LoadMirrorNode;
var
   Node : TTreeNTNode;
   cad: TStringList;
begin
    if FVirtualMode = vmKNTNode then begin
       node:= GetMirrorNode;
       if assigned(node) then begin
          FStream:= TNoteNode(Node.Data).FStream;       // This node shares its content with the other node
          FStream.Position := 0;
       end
       else begin
          MirrorNode:= nil;
          cad:= TStringList.Create;
          cad.Add(STR_01);
          cad.WriteBOM:= False;
          cad.SaveToStream(FStream);
          FStream.Position := 0;
          cad.Free;
          end;
    end;
end; // LoadMirrorNode

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
end; // SetMirrorNode


function TNoteNode.NonVirtualNode: TNoteNode;
var
   myNode: TNoteNode;
   node: TTreeNTNode;
begin
    Result:= Self;
    if (FVirtualMode= vmKNTNode) then begin
       node:= MirrorNode;
       if assigned(node) then
          Result:= TNoteNode(Node.Data);
    end;
end;

function TNoteNode.HasAlarms(considerDiscarded: boolean): boolean;
begin
    Result:= AlarmManager.HasAlarms(nil, NonVirtualNode, considerDiscarded);
end;

function TNoteNode.GetAlarms(considerDiscarded: boolean): TList;
begin
   Result:= AlarmManager.GetAlarms(nil, NonVirtualNode, considerDiscarded);
end;


procedure TNoteNode.LoadVirtualFile;
var
  NewVirtualFN : string;
begin
  FStream.Clear;
  FStream.Position := 0;

  if ( not FileExists( FVirtualFN )) then
  begin
    // try relative path
    if ( FRelativeVirtualFN <> '' ) then
    begin
      NewVirtualFN := ExpandFileName( FRelativeVirtualFN ); // must set current dir previously to the KeyNOte file's dir, using ChDir
      if FileExists( NewVirtualFN ) then
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
    if ( FRTFModified or ( not FileExists( FVirtualFN ))) then
    begin
      // back up the original file. Problem:
      // we have no access to KeyOptions here,
      // so we don't have all the information
      // necessary to create backup files.
      // We use GLOBAL variables instead. Bad design.
      if _VNDoBackup then
      begin
        case _VNBackupAddExt of
          false : bakFN := ChangefileExt( FVirtualFN, _VNBackupExt );
          true : bakFN := FVirtualFN + _VNBackupExt;
        end;

        if ( _VNBackupDir <> '' ) then
          bakFN := _VNBackupDir + ExtractFilename( bakFN );

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
//  FAlarm:= Source.FAlarm;

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
