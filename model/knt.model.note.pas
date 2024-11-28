unit knt.model.note;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2024 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

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
   VirtualTrees,
   VirtualTrees.Types,
   gf_misc,
   kn_Const,
   kn_Info
   ;


type
 TNote = class;
 TNoteEntry = class;
 TNoteNode = class;
 TNoteTag = class;

 TNoteList = TSimpleObjList<TNote>;           // TList<TNote>;            >> System.Generics.Collections
 TNoteNodeList = TSimpleObjList<TNoteNode>;   // TList<TNoteNode>;
 TNoteTagList= TSimpleObjList<TNoteTag>;      // TList<TNoteTag>;

 TResource = TNote;                     // TODO: Define TResource as a structure with a note and a typology (at least)
 TResourceList = TSimpleObjList<TResource>;
 TNoteEntryArray = TArray<TNoteEntry>;
 TAliasArray = TArray<string>;          // Consider also TStringList and System.IniFiles.THashedStringList (already used in TMemIniFIle...)


 TNoteState = (
   nsReadOnly,          // TODO: Allow marking individual notes as read-only (apart from being able to do so at higher levels as well: Folder, File)
   nsArchived,          // TODO: Archived notes -> way to mark them as material that we do not want to appear by default in searches or complicate the viewing of notes
   nsShowEmbedded,      // TODO: Show as embedded note when viewing the parent note (Ref: pg.72)
   nsNoEmbeddable,
   // will not be serialized to file:
   nsModified,
   nsLoading_OldMirror
  );
  TNoteStates = set of TNoteState;


 {
  * Summary: We could include an fSummary (string) field, but it seems more general to allow one of the note entries (and only one) to be marked as such.
  * fEntries:
    In many cases, especially notes from previous versions of the application, the notes will have a single entry. Thus,
    and to try to optimize memory, TArray is used and not TList
    Something analogous happens with fNNodes

  * Tags
    Instead of defining tags at the note level, as it is interesting that it be possible to define them at the entry level, if it is necessary that
    there be default tags for the entire note, we can assume that the Main Entry Tags fulfill that function. (Only one entry will be the Main Entry,
    initially the first created)

  * Note vs NoteNode
    In most cases each note (TNote) will be located in a single node (TNoteNode) of a single folder (in a single tree --VirtualTreeView--,
    from where it will inherit the hierarchy and corresponding relationships)
    Although it may be in several nodes, we can also consider the first one, where it was created from, as the main one. The rest will be nodes
    linked (generalizing what was previously mirror nodes).

  * Alias
    Note names can act as tags to help locate other notes. The note hierarchy makes it intuitive to use grouping notes in a parent as a way
    to establish a relationship between one series of notes and another that serves the function of scope/concept/feature/tag/etc.
    Defining an alias will make that functionality even easier.

  * fSelEntry, fSelStart, fSelLength, fScrollPosInEditor
    The note will be the minimum unit from which a position will be remembered. That is, for each note we will save a single position,
    which will make reference to an entry and a position within it. The position of the scrollbar in that entry will also be saved.
    ** Additionally, we can consider saving more detail for the last X notes, where we remember the arrangement of the entries or some
    settings that may have been changed during the session.
 }


  TFolderObj = TObject;

  TNoteNodeInFolder = record
     NNode:   TNoteNode;
     Folder: TFolderObj;
  end;
  TNoteNodeInFolderArray = TArray<TNoteNodeInFolder>;
  TNoteNodeArray         = TArray<TNoteNode>;


  TNote = class(TObject)
  private
    fGID : Cardinal;
    fName : string;
    fAlias : string;
    fVirtualFN : string;         // Absolute path to the file. When saving to disk it will also be saved relatively

    fStates: TNoteStates;
    fLastModified: TDateTime;

    fNNodes: TNoteNodeInFolderArray;
    fEntries: TNoteEntryArray;
    fResources: TResourceList;

    fSelEntry : TNoteEntry;
    fSelStart : Cardinal;         // In SelEntry
    fSelLength : Cardinal;        // ,,
    fScrollPosInEditor: TPoint;   // ,,

    procedure SetGID(ID : Cardinal);

  protected
    procedure SetName(const AName : string);
    procedure SetAlias(const Alias : string);
    function GetModified: boolean;
    procedure SetModified_(value: boolean);

    function GetSelEntry : TNoteEntry;
    function GetNumEntries: integer; inline;
    function GetNextNumEntry: Word;
    function GetDateCreated: TDateTime;

    function GetNumNNodes: integer; inline;


  public
    constructor Create;
    destructor Destroy; override;

    property GID : Cardinal read fGID write SetGID;           // Global ID. Unique note number within the .knt file. Matches the GID of one of its TNoteNode
    property Name : string read fName write SetName;
    property Alias : string read fAlias write SetAlias;
    procedure SetModified; inline;
    property Modified: boolean read GetModified write SetModified_;
    property States: TNoteStates read fStates;
    property LastModified: TDateTime read fLastModified write fLastModified;
    property DateCreated: TDateTime read GetDateCreated;

    property SelEntry : TNoteEntry read GetSelEntry write fSelEntry;
    property SelStart : Cardinal read fSelStart write fSelStart;
    property SelLength : Cardinal read fSelLength write fSelLength;
    property ScrollPosInEditor: TPoint read fScrollPosInEditor write fScrollPosInEditor;

    procedure LoadStates(States: TNoteStates);
    function StatesToString: string;
    procedure StringToStates(HexStr: string);
    function ResourcesToString: string;
    procedure SetLoadingAsOldMirror(const MirrorID: string);


   protected
    function InternalAddEntry(Entry: TNoteEntry): integer;
   public
    property NumEntries: integer read GetNumEntries;
    property Entries: TNoteEntryArray read fEntries;
    function AddEntry(Entry: TNoteEntry; ID: Word): integer;
    function AddNewEntry: TNoteEntry;
    procedure DeleteEntry(IndexToRemove: integer);
    function GetEntry(ID: Word): TNoteEntry;
    function IsValid(Entry: TNoteEntry): Boolean;


    property Resources: TResourceList read fResources;
    function AddResource(Resource: TNote): integer;
    function DeleteResource(Resource: TNote): integer;


    property NNodes: TNoteNodeInFolderArray read fNNodes;
    property NumNNodes: integer read GetNumNNodes;
    function AddNNode(NNode: TNoteNode; Folder: TFolderObj): integer;
    function RemoveNNode(NNode: TNoteNode): integer; overload;
    procedure RemoveNNode(IndexToRemove: integer); overload;
    function GetIndexOfNNodeInFolder(NNode: TNoteNode): integer;
    function HasNNodesInOtherFoldersThan(Folder: TFolderObj): boolean;
    procedure UpdateFolderInNNode(NNode: TNoteNode; Folder: TFolderObj);


   protected
    procedure SetVirtualFN(aVirtualFN : string); overload;
   public
    function IsVirtual: boolean;
    property VirtualFN : string read fVirtualFN write SetVirtualFN;
    procedure SetVirtualFN(aFN : string; aRelativeFN: string; const KntFilePath: string); overload;
    procedure LoadVirtualFile;
    procedure SaveVirtualFile;
    function HasVNodeError : boolean;

  end;



  TNoteEntryState = (
    nesModified,
    nesPlainTXT,       // If no existe => es RTF
    nesHTML,

    // TODO: 
    nesReadOnly,
    nesEncrypted,
    nesArchived,       // Entry that we do not want to appear by default in searches or complicate the display of notes
    nesEntryAndNote,   // It allows to point out that an entry also constitutes a note in itself (it is the 'first' entry of that one, or its MainEntry, and that it may have other Entries)
    nesFixed,

    nesIsMain,      // Main entry in a note, which identifies the note, usually the first. Its tags will apply globally to the entire note
    nesIsSummary,   // Only one entry can be used as Summary in a note
    nesIsStarred,
    nesIsDoc,
    nesIsToDO,
    nesIsRequirements
  );

  TNoteEntryStates = set of TNoteEntryState;


  TNoteTag = class(TObject)                        // TODO
  private
    fTagID : Cardinal;
    fName : string;
    fAlias : TAliasArray;
  public
  end;


  {
   TODO nesEntryAndNote
   An entry can also end up being a note: GID1, Entry7: GID8
   To do this, could it be enough to also make Note0 of that other note? That is, create a note that points to this note as main (fMainEntry).

   La entrada no sabe de sus 'padre'.
   De hecho puede pertenecer a varios si ésta ha sido utilizada también para constituir una nota en sí misma (-> MainEntry)
   Sólo permitiremos que una entrada pueda ser 'Main' entrada de una única nota

   Dada una entrada, en la que su estado incluye nsEntryAndNote, cómo podemos localizar su nota principal?
   En el fichero:
    EntriesAsNotes: TDictionary<TNoteEntry, TNote>

   A la hora de serializar sobre el archivo, al guardar, se podría hacer algo como:
   *Note1
    GID1
   ..
   -Entry"1" // Debo poder insertar marcadores... GID1.Entry1  (123.1)
    ...
   -Entry"4"
    ...

   *Note2
    GID2
    ..
   -Entry"4" ("Main")
    GID2.4   ===> + nesEntryAndNote

  }



  {
   * It is not necessary to define a Name/Caption field. The title will be the note to which it belongs. In the case of wanting to be able to "compact"
     an entry and see something as a title -> define it as the first line of the text.
   * fTags
     It can be nil. Specific tags (e.g. #ToDO -> "ToDO". May have aliases: "PTE", "ToDo", ...  }


  TNoteEntry = class(TObject)
  private
    fID: Word;
    fStates: TNoteEntryStates;
    fDateCreated: TDateTime;
    fTags: TNoteTagList;
    fStream : TMemoryStream;
    fTextPlain : string;

   protected
    function GetModified: boolean; inline;
    function GetIsRTF: boolean; inline;
    function GetIsPlainTXT: boolean; inline;
    function GetIsHTML: boolean; inline;
    procedure SetModified_(value: boolean);
    procedure SetIsRTF(value: boolean);
    procedure SetIsPlainTXT(value: boolean);
    procedure SetIsHTML(value: boolean);

  public
    constructor Create (NumEntry: Word = 0);
    destructor Destroy; override;
    procedure Assign(Source : TNoteEntry);

    property ID: Word read fID;
    property Stream : TMemoryStream read FStream;
    property TextPlain : string read fTextPlain write fTextPlain;
    property States: TNoteEntryStates read fStates;
    procedure SetModified;
    property Modified: boolean read GetModified write SetModified_;
    property Tags: TNoteTagList read fTags;
    property Created: TDateTime read fDateCreated write fDateCreated;


    property IsRTF: boolean read GetIsRTF write SetIsRTF;
    property IsPlainTXT: boolean read GetIsPlainTXT write SetIsPlainTXT;
    property IsHTML: boolean read GetIsHTML write SetIsHTML;

    function StatesToString: string;
    procedure StringToStates(HexStr: string);
  end;


  // Nodes in the tree only show notes, not entries

  TNumberingMethod = (NumberAndName, OnlyNumber, NoNumbering);


  { Certain information about each TNoteNode is enough to have within the VirtualTree node itself
    In fact, it will be within the tree node where we will have the current situation of that data. There we can look for it
    During the loading of the tree, based on the data in a file, it is convenient for us to recover some information
    previously persisted (and serialized) on our TNoteNode. Other (eg. level) will be temporarily saved in Folder }

  TNoteNodeState = (
    nnsBold,
    nnsTreeFilterMatch,          // Fulfills the tree filtering condition (if it exists)
    nnsFindFilterMatch,          // ,,                  ,,        de Find All  ,,
    nnsChildrenCheckbox,
    nnsShowOutlineNumberAndName,
    nnsShowOutlineOnlyNumber,
    nnsCustomNumberingSubtree,
    nnsWordWrap,                 // WordWrap -> apply / NoWordWarp -> do not apply. If neither nsWordWrap nor nsNoWordWrap is indicated -> default value
    nnsNoWordWrap,
    nnsFlagged,                  // TODO
    nnsSaved_Expanded,
    nnsSaved_Checked,
    nnsSaved_Hidden
  );

  TNoteNodeStates = set of TNoteNodeState;



  { The hierarchy will be embedded in the VirtualTree. It must be saved when serializing over the file.
    And vice versa, deserialize to be able to rebuild the tree.
   * fTVNode
     To be able to "ask" the tree and query the existing hierarchy relationships for a given node.
     (It is not required to "paint" the node, since the PVirtualNode object will contain as data a pointer to our TNoteNode)
        PVirtualNode = ^TVirtualNode (See VirtualTrees.Types.pas, lin. 888)
      We can access the parent node easily (which will be the TreeView when it is the root)

   In most cases nodes use data at the folder level (except perhaps Bold, which can be set as an option, in States, and ImageIndex)
  }


  TNoteNode = class(TObject)
  private
    fID : Word;
    fGID : Cardinal;
    fNote: TNote;
    fTVNode: PVirtualNode;
  	fImageIndex : integer;

  public
    States: TNoteNodeStates;
    EditorBGColor : TColor;   // clNone => HasEditorBGColor = False
    NodeColor : TColor;       // clNone => HasNodeColor = False
    NodeBGColor : TColor;     // clNone => HasNodeBGColor = False
    NodeFontFace : string;    // ''     => HasNodeFontFace = False


  protected
    procedure SetID(ID : Word);
    procedure SetGID(ID : Cardinal);
    procedure SetTVNode (value: PVirtualNode);

    function GetNodeBold: boolean; inline;
    function GetFlagged: boolean; inline;
    function GetTreeFilterMatch: boolean; inline;
    function GetFindFilterMatch: boolean; inline;
    function GetChildrenCheckbox: boolean; inline;
    function GetNoteName: string;
    function GetNumberingMethod: TNumberingMethod;
    function GetCustomNumberingSubtree: boolean;
    function GetWordWrap: TNodeWordWrap;
    procedure SetNodeBold(value: boolean);
    procedure SetFlagged(value: boolean);
    procedure SetTreeFilterMatch(value: boolean);
    procedure SetFindFilterMatch(value: boolean);
    procedure SetChildrenCheckbox(value: boolean);
    procedure SetNumberingMethod (value: TNumberingMethod);
    procedure SetCustomNumberingSubtree(value: boolean);
    procedure SetWordWrap(value: TNodeWordWrap);

  public
    constructor Create (Note: TNote);
    destructor Destroy; override;
    procedure Assign(Source : TNoteNode);

    property ID : Word read fID write SetID;              // [Old] Unique within the folder in which it was created
    property GID : Cardinal read fGID write SetGID;       // Global ID. Unique note Node number within the .knt file
    procedure ForceID(ID : Word);

    property TVNode: PVirtualNode read fTVNode write SetTVNode;
    property Note: TNote read fNote write fNote;
    property NoteName: string read GetNoteName;
    function NodeName (TreeUI: TObject): string;
    function GetNodeName (Folder: TObject): string;

    function IsVirtual: boolean; inline;

    property Bold : boolean read GetNodeBold write SetNodeBold;
    property Flagged : boolean read GetFlagged write SetFlagged;
    property TreeFilterMatch : boolean read GetTreeFilterMatch write SetTreeFilterMatch;
    property FindFilterMatch : boolean read GetFindFilterMatch write SetFindFilterMatch;
    property ChildrenCheckbox: boolean read GetChildrenCheckbox write SetChildrenCheckbox;
    property NumberingMethod: TNumberingMethod read GetNumberingMethod write SetNumberingMethod;
    property CustomNumberingSubtree: boolean read GetCustomNumberingSubtree write SetCustomNumberingSubtree;
    property WordWrap: TNodeWordWrap read GetWordWrap write SetWordWrap;
    property ImageIndex : integer read fImageIndex write fImageIndex;
    procedure ResetChrome;

    procedure UpdateStates(TV: TBaseVirtualTree);
    function StatesToString (IgnoreFilterMatch: boolean): string;
    procedure StringToStates(HexStr: string);
  end;

  PNoteNode = ^TNoteNode;


implementation
uses
   gf_files,
   gf_streams,
   kn_Global,
   kn_LinksMng,
   knt.ui.tree,
   kn_KntFolder,
   knt.App
   ;




// =========================================================================================================
//     TNote
// =========================================================================================================

{$REGION TNote }

constructor TNote.Create;
begin
  inherited Create;

  fGID := 0;
  fName := '';
  fAlias :=  '';
  fVirtualFN := '';

  fSelEntry := nil;
  fSelStart := 0;
  fSelLength := 0;
  fScrollPosInEditor.X:= -1;
  fScrollPosInEditor.Y:= -1;

  fLastModified := 0;
  fEntries := nil;
  fResources := nil;
  fNNodes:= nil;

  // Default/initial state:
  fStates:= [];
end;


destructor TNote.Destroy;
var
   E: TNoteEntry;
   nnf: TNoteNodeInFolder;
   i: integer;
begin

  if fEntries <> nil then begin
     for i:= 0 to High(fEntries) do begin
        E:= fEntries[i];
        if not (TNoteEntryState.nesEntryAndNote in E.States) then
           E.Free;
     end;
     fEntries:= nil;
  end;

  if fResources <> nil then
     fResources.Free;

  // The responsibility of freeing the nodes falls on the folders
  fNNodes:= nil;

  // TODO Report, through App so that those notes that include this as a resource, remove it

  inherited Destroy;
end;


procedure TNote.SetGID(ID : Cardinal);
begin
  if ( fGID = 0 ) then
    fGID := ID;
end;


procedure TNote.SetName(const AName : string);
begin
   if fName = AName then exit;

   fName := AName;
   if (Length(fName) > TREENODE_NAME_LENGTH) then
      Delete(fName, TREENODE_NAME_LENGTH, 9999);

   SetModified;                                             // = Modified:= true;
   App.NoteNameModified(Self);
end;


procedure TNote.SetAlias(const Alias : string);
begin
   if fAlias = Alias then exit;

   fName := Alias;
   SetModified;
end;


function TNote.GetModified: boolean;
var
  i: integer;
begin
  Result:= (nsModified in fStates);

  if not Result then begin
     for i:= 0 to High(fEntries) do
        if fEntries[i].Modified then
           exit(true);
  end;
end;

procedure TNote.SetModified;
begin
   if ActiveFileIsBusy or AFileIsLoading then exit;

   Include(fStates, nsModified);
   fLastModified:= Now;
end;


procedure TNote.SetModified_(value: boolean);
var
  i: integer;
begin
   if value then
      SetModified
   else begin
      Exclude(fStates, nsModified);
      for i:= 0 to High(fEntries) do
         fEntries[i].Modified := false;
   end;
end;


function TNote.GetSelEntry : TNoteEntry;
begin
   Result:= fSelEntry;
   if (Result = nil) and (fEntries <> nil) then
      Result:= Entries[0];
end;

function TNote.GetNumEntries: integer;
begin
   Result:= Length(fEntries);
end;


function TNote.GetDateCreated: TDateTime;
begin
   if fEntries <> nil then
      Result:= fEntries[0].fDateCreated;
end;

function TNote.GetNextNumEntry: Word;
var
  i: integer;
begin
   Result:= 0;
   if (fEntries = nil) then exit;

   for i:= 0 to High(fEntries) do
      if fEntries[i].fID >= Result then
         inc(Result);
end;


function TNote.GetIndexOfNNodeInFolder(NNode: TNoteNode): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to High(NNodes) do
     if NNodes[i].NNode = NNode then exit(i);
end;


procedure TNote.UpdateFolderInNNode(NNode: TNoteNode; Folder: TFolderObj);
var
   i: integer;
begin
   i:= GetIndexOfNNodeInFolder(NNode);
   if i >= 0 then
      fNNodes[i].Folder:= Folder;
end;


function TNote.HasNNodesInOtherFoldersThan(Folder: TFolderObj): boolean;
var
  i: integer;
begin
  Result:= false;
  for i:= 0 to High(NNodes) do
     if NNodes[i].Folder = Folder then exit(true);
end;


function TNote.InternalAddEntry(Entry: TNoteEntry): integer;
var
  i: integer;
begin
   i:= GetNumEntries;
   SetLength(fEntries, i+1);
   fEntries[i]:= Entry;
   Result:= i;

   SetModified;
end;


function TNote.AddNewEntry: TNoteEntry;
begin
   Result:= TNoteEntry.Create(GetNextNumEntry);
   Result.fDateCreated:= Now();
   InternalAddEntry(Result);
end;


function TNote.AddEntry(Entry: TNoteEntry; ID: Word): integer;
begin
   Result:= InternalAddEntry(Entry);
   Entry.fID:= ID;
end;


procedure TNote.DeleteEntry(IndexToRemove: integer);
var
  NEntry: TNoteEntry;
begin
   if (fEntries = nil) then exit;

   assert((IndexToRemove >= Low(fEntries)) and (IndexToRemove <= High(fEntries)) );
   NEntry:= fEntries[IndexToRemove];
   Delete(fEntries, IndexToRemove, 1);

   if NEntry <> nil then
      NEntry.Free;

   SetModified;
end;


function TNote.IsValid(Entry: TNoteEntry): Boolean;
var
   i: integer;
begin
   Result:= false;
   if (Entry = nil) then exit;

   for i:= 0 to High(fEntries) do
      if fEntries[i] = Entry then
         exit(true)
end;

function TNote.GetEntry(ID: Word): TNoteEntry;
var
   i: integer;
begin
   Result:= nil;
   for i:= 0 to High(fEntries) do
      if fEntries[i].ID = ID then
         exit(fEntries[i]);
end;


function TNote.AddResource(Resource: TNote): integer;
begin
   if fResources = nil then
      fResources:= TResourceList.Create;

   Result:= fResources.IndexOf(Resource);
   if Result < 0 then begin
      Result:= fResources.Add(Resource);

      SetModified;
   end;
end;

function TNote.DeleteResource(Resource: TNote): integer;
begin
   if fResources = nil then exit(-1);
   Result:= fResources.Remove(Resource);

   SetModified;
end;

function TNote.ResourcesToString: string;
var
   i: integer;
   SEP: string;
begin
   Result:= '';
   if fResources = nil then exit;

   SEP:= '';
   for i:= 0 to fResources.Count -1 do begin
      Result:= Result + SEP + fResources[i].fGID.ToString;
      SEP:= '|';
   end;
end;



function TNote.AddNNode(NNode: TNoteNode; Folder: TFolderObj): integer;
var
  nnf: TNoteNodeInFolder;
  i: integer;
begin
  nnf.NNode:= NNode;
  nnf.Folder:= Folder;

  i:= GetNumNNodes;
  SetLength(fNNodes, i+1);        // There will be 1 single NNode or very few
  NNodes[i]:= nnf;
  Result:= i;

  SetModified;
end;

procedure TNote.RemoveNNode(IndexToRemove: integer);
var
  nnf: TNoteNodeInFolder;
begin
   if (fNNodes = nil) then exit;

   assert((IndexToRemove >= Low(fNNodes)) and (IndexToRemove <= High(fNNodes)) );
   Delete(fNNodes, IndexToRemove, 1);

   SetModified;
end;

function TNote.GetNumNNodes: integer;
begin
   Result:= Length(fNNodes);
end;

function TNote.RemoveNNode(NNode: TNoteNode): integer;
var
   i: integer;
begin
   i:= GetIndexOfNNodeInFolder(NNode);
   if i >= 0 then
      RemoveNNode(i);

   Result:= i;
end;



procedure TNote.SetLoadingAsOldMirror(const MirrorID: string);
begin
  Include(fStates, nsLoading_OldMirror);
  fVirtualFN:= MirrorID;
end;

function TNote.IsVirtual: boolean;
begin
   Result:= fVirtualFN <> '';
end;


procedure TNote.SetVirtualFN(aVirtualFN : string);
var
  myExt : string;
  NEntry: TNoteEntry;
begin
   aVirtualFN := normalFN( aVirtualFN );

   if (aVirtualFN = fVirtualFN) then exit;

   FVirtualFN := aVirtualFN;
   if (FVirtualFN <> '') then begin
      NEntry:= Entries[0];
      myExt := LowerCase(ExtractFileExt(FVirtualFN));
      if (myExt = ext_RTF) then
         NEntry.IsRTF:= true
      else
      if (pos('htm', myExt) > 0 ) then
         NEntry.IsHTML:= true
      else
         NEntry.IsPlainTXT:= true;
   end;

   SetModified;
end;

procedure TNote.SetVirtualFN(aFN : string; aRelativeFN: string; const KntFilePath: string);
begin
  aFN := normalFN(aFN);
  if not (FileExists(aFN)) then begin
     aRelativeFN := normalFN(aRelativeFN);
     aFN:= GetAbsolutePath(KntFilePath, aRelativeFN);
  end;

  SetVirtualFN (aFN);
end;



function TNote.HasVNodeError : boolean;
begin
   result := (FVirtualFN <> '') and (FVirtualFN[1] = _VIRTUAL_NODE_ERROR_CHAR);
end;


procedure TNote.LoadVirtualFile;
var
  Stream: TMemoryStream;
  ext: string;

begin
  if not IsVirtual then exit;

  Stream:= Entries[0].Stream;    // Virtual notes always have a single entry, 0.
  Stream.Clear;
  Stream.Position := 0;

  LoadTxtOrRTFFromFile(Stream, FVirtualFN);
  ext := ExtractFileExt(FVirtualFN);

  Entries[0].IsRTF:= ExtIsRTF(ext);
  Entries[0].IsHTML:= ExtIsHTML(ext);

  Stream.Position := 0;
end;


procedure TNote.SaveVirtualFile;
var
  NEntry: TNoteEntry;
  bakFN : string;
  Stream: TMemoryStream;
begin
  if not IsVirtual then exit;
    Stream:= Entries[0].Stream;

    // only saved file if was actually changed
    if Modified or (not FileExists(FVirtualFN)) then begin
      // back up the original file.
      if KeyOptions.BackupVNodes then begin
        case KeyOptions.BackupAppendExt of
          false : bakFN := ChangefileExt(FVirtualFN, KeyOptions.BackupExt);
          true : bakFN := FVirtualFN + KeyOptions.BackupExt;
        end;
        if (KeyOptions.BackupDir <> '') then
          bakFN := KeyOptions.BackupDir + ExtractFilename(bakFN);

        CopyFile(PChar(FVirtualFN), PChar(bakFN), false);
      end;

      Stream.SaveToFile(FVirtualFN);
    end;

//  Entries[0].Modified:= false;
  Exclude(fStates, nsModified);       // Modified:= false;
end;

procedure TNote.LoadStates(States: TNoteStates);
begin
   fStates:= States;
end;

function TNote.StatesToString: String;
var
  StatesAux: TNoteStates;
begin
  StatesAux:= fStates;
  Exclude(StatesAux, nsModified);
  Exclude(StatesAux, nsLoading_OldMirror);
  Result:= IntToHex(SetToInt(StatesAux, SizeOf(TNoteStates)), SizeOf(TNoteStates)*2);
end;

procedure TNote.StringToStates(HexStr: string);
begin
   IntToSet(StrToIntDef('$' + HexStr, 0), fStates, SizeOf(TNoteStates));
end;

{$ENDREGION }


// =========================================================================================================
//     TNoteEntry
// =========================================================================================================

{$REGION TNoteEntry }

constructor TNoteEntry.Create (NumEntry: Word = 0);
begin
  inherited Create;

  fID := NumEntry;
  fStream := TMemoryStream.Create;

  fDateCreated := 0;
  if not AFileIsLoading then
     fDateCreated := Now;

  fTags := nil;
  fTextPlain := '';

  fStates := [];      // By default RTF (as nesPlainTXT is not included)
end;

destructor TNoteEntry.Destroy;
begin
  if TNoteEntryState.nesEntryAndNote in fStates then exit;

  if assigned(fStream) then
     fStream.Free;

  if fTags <> nil then
     fTags.Free;

  inherited Destroy;
end;


procedure TNoteEntry.Assign(Source : TNoteEntry);
begin
  if not assigned(Source) then exit;

  fStream.Clear;
  fStream.LoadFromStream(Source.Stream);
  fStream.Position := 0;
  fTextPlain:= Source.TextPlain;

  fStates:= Source.States;          // TODO: nesEntryAndNote ....

  if assigned(Source.fTags) and (Source.fTags.Count > 0) then
     fTags:= TNoteTagList.Create(Source.fTags);

  SetModified;
end;


function TNoteEntry.GetModified: boolean;
begin
  Result:= (nesModified in fStates);
end;

procedure TNoteEntry.SetModified;
begin
   Include(fStates, nesModified);
end;

procedure TNoteEntry.SetModified_(value: boolean);
begin
   if value then
      SetModified
   else
      Exclude(fStates, nesModified);
end;

function TNoteEntry.GetIsRTF: boolean;
begin
  Result:= not (nesPlainTXT in fStates);
end;

function TNoteEntry.GetIsPlainTXT: boolean;
begin
  Result:= (nesPlainTXT in fStates);
end;

function TNoteEntry.GetIsHTML: boolean;
begin
  Result:= not (nesHTML in fStates);
end;

procedure TNoteEntry.SetIsRTF(value: boolean);
begin
  if value then begin
     Exclude(fStates, nesPlainTXT);
     Exclude(fStates, nesHTML);
  end
  else
     Include(fStates, nesPlainTXT);
end;

procedure TNoteEntry.SetIsPlainTXT(value: boolean);
begin
  if value then
     Include(fStates, nesPlainTXT)
  else
     Exclude(fStates, nesPlainTXT);
end;

procedure TNoteEntry.SetIsHTML(value: boolean);
begin
  if value then begin
     Include(fStates, nesHTML);
     Include(fStates, nesPlainTXT);
  end
  else
     Exclude(fStates, nesHTML);
end;


function TNoteEntry.StatesToString: String;
var
  StatesAux: TNoteEntryStates;
begin
  StatesAux:= fStates;
  Exclude(StatesAux, nesModified);
  Result:= IntToHex(SetToInt(StatesAux, SizeOf(TNoteEntryStates)), SizeOf(TNoteEntryStates)*2);
end;

procedure TNoteEntry.StringToStates(HexStr: string);
begin
   IntToSet(StrToIntDef('$' + HexStr, 0), fStates, SizeOf(TNoteEntryStates));
end;

{$ENDREGION }


// =========================================================================================================
//     TNoteNode
// =========================================================================================================

{$REGION TNoteNode }

constructor TNoteNode.Create(Note: TNote);
begin
  inherited Create;

  fID := 0;
  fGID := 0;

  fNote := Note;
  fTVNode := nil;
  fImageIndex := 0;
  ResetChrome;
end;

destructor TNoteNode.Destroy;
begin
  inherited Destroy;
end;


procedure TNoteNode.Assign(Source : TNoteNode);
begin
   if not assigned(Source) then exit;

   fImageIndex:= Source.ImageIndex;
   States:= Source.States;

   EditorBGColor := Source.EditorBGColor;
   NodeColor :=  Source.NodeColor;
   NodeBGColor :=  Source.NodeBGColor;
   NodeFontFace :=  Source.NodeFontFace;

   if (TVNode <> nil) and (Source.TVNode <> nil) then
      TVNode.CheckType:= Source.TVNode.CheckType;

end;


procedure TNoteNode.SetID(ID: Word);
begin
  if (fID = 0) then
    fID := ID;
end; // SetID

procedure TNoteNode.ForceID(ID : Word);
begin
   fID := ID;
end;

procedure TNoteNode.SetGID(ID : Cardinal);
begin
  if (fGID = 0) then
     fGID := ID;
end;


function TNoteNode.GetNoteName: string;
begin
   Result:= fNote.Name;
end;

function TNoteNode.NodeName (TreeUI: TObject): string;
begin
  if Self.NumberingMethod = NoNumbering then
     Result:= fNote.Name
  else begin
     assert(TreeUI <> nil);
     Result:= TKntTreeUI(TreeUI).GetNodeCaption(TVNode);
  end;
end;

function TNoteNode.GetNodeName (Folder: TObject): string;
begin
   if Folder = nil then
      Result:= fNote.Name
   else
      Result:= TKntTreeUI(TKntFolder(Folder).TreeUI).OutlineNumber(TVNode);
end;


procedure TNoteNode.SetTVNode (value: PVirtualNode);
begin
   fTVNode:= value;
end;

procedure TNoteNode.ResetChrome;
begin
  EditorBGColor:= clNone;
  NodeColor:= clNone;
  NodeBGColor:= clNone;
  NodeFontFace:= '';
end;


function TNoteNode.IsVirtual: boolean;
begin
   Result:= Note.IsVirtual;
end;


function TNoteNode.GetNodeBold: boolean;
begin
   Result:= (nnsBold in States);
end;

procedure TNoteNode.SetNodeBold(value: boolean);
begin
   if Value then
      Include(States, nnsBold)
   else
      Exclude(States, nnsBold);
end;

function TNoteNode.GetFlagged: boolean;
begin
   Result:= (nnsFlagged in States);
end;

procedure TNoteNode.SetFlagged(value: boolean);
begin
   if Value then
      Include(States, nnsFlagged)
   else
      Exclude(States, nnsFlagged);
end;


function TNoteNode.GetTreeFilterMatch: boolean;
begin
   Result:= (nnsTreeFilterMatch in States);
end;

procedure TNoteNode.SetTreeFilterMatch(value: boolean);
begin
   if Value then
      Include(States, nnsTreeFilterMatch)
   else
      Exclude(States, nnsTreeFilterMatch);
end;


function TNoteNode.GetFindFilterMatch: boolean;
begin
   Result:= (nnsFindFilterMatch in States);
end;

procedure TNoteNode.SetFindFilterMatch(value: boolean);
begin
   if Value then
      Include(States, nnsFindFilterMatch)
   else
      Exclude(States, nnsFindFilterMatch);
end;


function TNoteNode.GetChildrenCheckbox: boolean;
begin
   Result:= (nnsChildrenCheckbox in States);
end;

procedure TNoteNode.SetChildrenCheckbox(value: boolean);
begin
   if Value then
      Include(States, nnsChildrenCheckbox)
   else
      Exclude(States, nnsChildrenCheckbox);
end;

function TNoteNode.GetNumberingMethod: TNumberingMethod;
begin
   if (nnsShowOutlineOnlyNumber in States) then exit(TNumberingMethod.OnlyNumber);
   if (nnsShowOutlineNumberAndName in States) then exit(TNumberingMethod.NumberAndName);
   Result:= TNumberingMethod.NoNumbering;
end;

procedure TNoteNode.SetNumberingMethod (value: TNumberingMethod);
begin
   Exclude(States, nnsShowOutlineOnlyNumber);
   Exclude(States, nnsShowOutlineNumberAndName);
   case value of
     NoNumbering: ;
     NumberAndName:  Include(States, nnsShowOutlineNumberAndName);
     OnlyNumber:     Include(States, nnsShowOutlineOnlyNumber);
    end;
end;

function TNoteNode.GetCustomNumberingSubtree: boolean;
begin
   Result:= nnsCustomNumberingSubtree in States;
end;

procedure TNoteNode.SetCustomNumberingSubtree(value: boolean);
begin
   if Value then
      Include(States, nnsCustomNumberingSubtree)
   else
      Exclude(States, nnsCustomNumberingSubtree);
end;


function TNoteNode.GetWordWrap: TNodeWordWrap;
begin
  if nnsWordWrap in States then exit(wwYes);
  if nnsNoWordWrap in States then exit(wwNo);

  Result:= wwAsFolder;
end;

procedure TNoteNode.SetWordWrap(value: TNodeWordWrap);
begin
   Exclude(States, nnsWordWrap);
   Exclude(States, nnsNoWordWrap);
   case value of
     wwYes: Include(States, nnsWordWrap);
     wwNo:  Include(States, nnsNoWordWrap);
   end;
end;


procedure TNoteNode.UpdateStates(TV: TBaseVirtualTree);
begin
  Exclude(States, nnsSaved_Expanded);
  Exclude(States, nnsSaved_Checked);
  Exclude(States, nnsSaved_Hidden);

  if TV.Expanded[TVNode] then
     Include(States, nnsSaved_Expanded);

  if TVNode.CheckState = csCheckedNormal then
     Include(States, nnsSaved_Checked);

  if not TV.IsVisible[TVNode] then
     Include(States, nnsSaved_Hidden);
end;


function TNoteNode.StatesToString(IgnoreFilterMatch: boolean): String;
var
  StatesBak: TNoteNodeStates;
begin
  StatesBak:= States;
  if IgnoreFilterMatch then begin
     Exclude(States, nnsTreeFilterMatch);
     Exclude(States, nnsFindFilterMatch);
  end;

  Result:= IntToHex(SetToInt(States, SizeOf(TNoteNodeStates)), SizeOf(TNoteNodeStates)*2);
  States:= StatesBak
end;

procedure TNoteNode.StringToStates(HexStr: string);
begin
   IntToSet(StrToIntDef('$' + HexStr, 0), States, SizeOf(TNoteNodeStates));
end;

{$ENDREGION }

Initialization

end.