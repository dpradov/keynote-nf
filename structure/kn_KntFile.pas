unit kn_KntFile;

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
   System.AnsiStrings,
   System.IOUtils,
   Vcl.Graphics,
   Vcl.FileCtrl,
   Vcl.Controls,
   Vcl.Dialogs,

   VirtualTrees,
   VirtualTrees.BaseTree,


   comctrls95,
   DCPcrypt,
   ZLibEx,

   gf_streams,
   gf_files,
   kn_Const,
   kn_Info,
   kn_LocationObj,
   kn_KntFolder,
   knt.model.note,
   kn_ImagesMng,
   kn_LinksMng
   ;



type
  EKeyKntFileError = class( Exception );
  EPassphraseError = class( Exception );

type
  TGetAccessPassphraseFunc = function( const FN : string ) : string;

type
  TBookmarks = array[0..MAX_BOOKMARKS] of TLocation;


type
  TKntFile = class( TObject )
  private
    FVersion : TKntFileVersion;
    FFileName : string;
    FFileFormat : TKntFileFormat;
    FCompressionLevel: TZCompressionLevel;
    FDescription : TCommentStr;
    FComment : TCommentStr;
    FDateCreated : TDateTime;
    FModified : boolean;
    FReadOnly : boolean;
    FOpenAsReadOnly : boolean;
    FShowTabIcons : boolean;
    FNoMultiBackup : boolean;
    FTrayIconFN : string;
    FTabIconsFN : string;
    FSavedWithRichEdit3 : boolean;
    FCryptMethod : TCryptMethod;
    FPassPhrase : UTF8String;
    FPassphraseFunc : TGetAccessPassphraseFunc;
    FSavedActiveFolderID : Cardinal;

    FPageCtrl : TPage95Control;

    fFolders: TFolderList;
    fNotes: TNoteList;
    fNextNNodeGID: Cardinal;     // Global ID of next note node to be created

    FBookmarks : TBookmarks;
    FTextPlainVariablesInitialized: boolean;

    FIsBusy : boolean; // if TRUE, file is being saved, opened or closed, so we can't mess with it


    function GetFolderCount : integer;
    procedure SetVersion;
    procedure SetDescription( ADescription : TCommentStr );
    procedure SetComment( AComment : TCommentStr );
    procedure SetFileFormat( AFileFormat : TKntFileFormat );
    procedure SetModified( AModified : boolean );
    function GetPassphrase( const FN : string ) : boolean;

    procedure SetFilename( const Value : string );
    function GetBookmark(Index: integer): TLocation;
    procedure WriteBookmark (Index: integer; Value: TLocation);
    function GetFile_Name: string;
    function GetFile_NameNoExt: string;
    function GetFile_Path: string;

    procedure SetIsBusy(Value: boolean);

  public
    State : TFileState;         // for file change notification (remembers previous file size, date and time)
    ChangedOnDisk : boolean;    // for file change notification. If true, we will prompt to reload at nearest opportunity.

    property Version : TKntFileVersion read FVersion;
    property FileName : string read FFileName write SetFileName;
    property File_Name : string read GetFile_Name;
    property File_NameNoExt : string read GetFile_NameNoExt;
    property File_Path : string read GetFile_Path;

    property Comment : TCommentStr read FComment write SetComment;
    property Description : TCommentStr read FDescription write SetDescription;
    property FolderCount : integer read GetFolderCount;
    property DateCreated : TDateTime read FDateCreated;
    property Modified : boolean read FModified write SetModified;
    property FileFormat : TKntFileFormat read FFileFormat write SetFileFormat;
    property CompressionLevel: TZCompressionLevel read FCompressionLevel write FCompressionLevel;
    property TrayIconFN : string read FTrayIconFN write FTrayIconFN;
    property TabIconsFN : string read FTabIconsFN write FTabIconsFN;
    property ReadOnly : boolean read FReadOnly write FReadOnly;
    property SavedWithRichEdit3 : boolean read FSavedWithRichEdit3;
    property PageCtrl : TPage95Control read FPageCtrl write FPageCtrl;
    property SavedActiveFolderID : Cardinal read FSavedActiveFolderID write FSavedActiveFolderID;
    property OpenAsReadOnly : boolean read FOpenAsReadOnly write FOpenAsReadOnly;
    property ShowTabIcons : boolean read FShowTabIcons write FShowTabIcons;
    property NoMultiBackup : boolean read FNoMultiBackup write FNoMultiBackup;

    property CryptMethod : TCryptMethod read FCryptMethod write FCryptMethod;
    property Passphrase : UTF8String read FPassphrase write FPassphrase;
    property PassphraseFunc : TGetAccessPassphraseFunc read FPassphraseFunc write FPassphraseFunc;

    property Bookmarks[index: integer]: TLocation read GetBookmark write WriteBookmark;

    property IsBusy: boolean read FIsBusy write SetIsBusy;

    constructor Create;
    destructor Destroy; override;


  private
    function InternalAddFolder( AFolder : TKntFolder ) : integer;
    procedure GenerateFolderID( const AFolder : TKntFolder );
    procedure VerifyFolderIds;
  public
    property Folders : TFolderList read FFolders; // write FFolders;
    function GetFolderByID( const aID : Cardinal ) : TKntFolder; // identifies folder UNIQUELY
    function GetFolderByName( const aName : string ) : TKntFolder; // will return the first folder whose name matches aName. If more notes have the same name, function will only return the first one.
    function GetFolderByTreeNode( const myTreeNode: PVirtualNode ) : TKntFolder;  // return the folder that contains the tree with the passed node
    function GetFolderByTabIndex(TabIdx: integer): TKntFolder;
    function AddFolder( AFolder : TKntFolder ) : integer;
    procedure DeleteFolder( AFolder : TKntFolder );
    function IsValid(AFolder : TKntFolder ): Boolean;
    //function HasExtendedFolders : boolean; // TRUE is file contains any notes whose FKind is not ntRTF


    function  Save(FN: string;
                  var SavedFolders: integer; var SavedNodes: integer;
                  ExportingMode: boolean= false; OnlyCurrentNodeAndSubtree: PVirtualNode= nil;
                  OnlyNotHiddenNodes: boolean= false; OnlyCheckedNodes: boolean= false): integer;
    function  Load( FN : string; ImgManager: TImageMng; var ClipCapIdx: integer; AddProcessAlarms: boolean) : integer;
    procedure LoadNotes(var tf : TTextFile; var FileExhausted : boolean; var NextBlock: TNextBlock);
    procedure LoadVirtualNote (Note: TNote; const VirtFN, RelativeVirtFN: string; List: TStringList);
    function  ConvertKNTLinksToNewFormatInNotes(NoteGIDs: TMergedNotes; var GIDsNotConverted: integer): boolean;
    procedure EncryptFileInStream( const FN : string; const CryptStream : TMemoryStream );
    procedure DecryptFileToStream( const FN : string; const CryptStream : TMemoryStream );
  private
    function  PropertiesToFlagsString : TFlagsString; virtual;
    procedure FlagsStringToProperties( const FlagsStr : TFlagsString ); virtual;
    procedure SaveTextToFile(var tf : TTextFile; DataStream : TMemoryStream; PlainText: Boolean; PlaintextLeader: AnsiString = _NF_PLAINTEXTLEADER);


  public
    property TextPlainVariablesInitialized: boolean read FTextPlainVariablesInitialized write FTextPlainVariablesInitialized;
    procedure UpdateTextPlainVariables (nMax: integer);

    procedure UpdateImagesStorageModeInFile (ToMode: TImagesStorageMode; ApplyOnlyToFolder: TKntFolder= nil; ExitIfAllImagesInSameModeDest: boolean = true);
    function  EnsurePlainTextAndRemoveImages (myFolder: TKntFolder): boolean;
    procedure RemoveImagesCountReferences (myFolder: TKntFolder); overload;
    procedure RemoveImagesCountReferences (Note: TNote); overload;
    procedure UpdateImagesCountReferences (myFolder: TKntFolder); overload;
    procedure UpdateImagesCountReferences (Note: TNote); overload;


  public
    property Notes: TNoteList read fNotes;
    function  GetNoteByGID(const aGID: Cardinal): TNote;
    procedure GetNNodeByGID(const aGID : Cardinal; var NNode: TNoteNode; var Folder: TKntFolder);
    function  GetNNodeByFolderAndID (FolderID: Cardinal; NodeID: Word): TNoteNode;
    property NextNNoteGID: Cardinal read fNextNNodeGID;             // NoteGID:  identifies Note UNIQUELY in whole file
    procedure RecalcNextNNodeGID;
    procedure VerifyNoteGIDs;
    function AddLoadedNote(Folder: TKntFolder): TNoteNode;
    function AddNewNote(Folder: TKntFolder; CopyFromNNode: TNoteNode = nil; UpdateImagesCountRef: Boolean= true): TNoteNode;
    function AddNewVirtualNote(Folder: TKntFolder; CopyFromNNode: TNoteNode; UpdateImagesCountRef: Boolean= true): TNoteNode;
    function DeleteNote(Note: TNote): Boolean;
    function AddLoadedNNode(Note: TNote; Folder: TKntFolder; GID: Cardinal): TNoteNode;
    function AddNewNNode(Note: TNote; Folder: TKntFolder; CopyFromNNode: TNoteNode = nil; AddToFolder: boolean= true; ForceInherit: boolean = false): TNoteNode;
    function CheckNNode(var NNode: TNoteNode; var Folder: TKntFolder): boolean;
    function HasVirtualNotes : boolean; // TRUE is file contains any notes which have VIRTUAL NODES
    function GetVirtualNoteByFileName( const aNote : TNote; FN : string ): TNote;
    procedure ConvertOldMirrorNodesToNNodes;

  end;

  procedure TransferNEntryText(ListTextStr : TStringList; StreamText: TMemoryStream; var IsRTF: boolean);

const
  Scratch_TabIdX = 999;


implementation
uses
   RxRichEd,
   Blowfish,
   SHA1,
   IDEA,

   gf_strings,
   gf_misc,

   kn_RTFUtils,
   kn_Global,
   kn_Main,
   kn_EditorUtils,
   knt.ui.tree,
   knt.ui.editor,
   kn_AlertMng,
   kn_BookmarksMng,
   knt.App
   ;


resourcestring
  STR_01 = 'Cannot open "%s": File not found';
  STR_02 = 'Invalid file header in "%s" (not a KeyNote file)';
  STR_03 = 'Access passphrase not specified: cannot open encrypted file.';
  STR_04 = 'The passphrase is invalid. Try again?';
  STR_05 = '%s: This file was created with a version of KeyNote later than the version you are using. ' +
                'Expected version ID: "%s.%s" This file version ID: "%s.%s"  You need the latest version of KeyNote to open this file.';
  STR_06 = ': This file was created with a version of KeyNote newer than the version you are using. ' +
                'The file can be opened, but some information can be lost or misinterpreted. As a safety measure, the file should be opened in Read-Only mode. ' +
                'Would you like to open the file as Read-Only?';
  STR_07 = '%s: Invalid file header or version, or corrupt file.';
  STR_08 = 'Error loading folder ';
  STR_10 = 'This file contains notes which are not compatible with %s format. Only %s notes can be saved in this format.';
  STR_12 = 'Error: Filename not specified.';
  STR_13 = 'Error while saving folder "%s": %s';
  STR_14 = 'Cannot save: Passphrase not set';
  STR_17 = 'Stream size error: Encrypted file is invalid or corrupt.';
  STR_18 = 'Invalid passphrase: Cannot open encrypted file.';
  STR_19 = 'Exception trying to ensure plain text and removing of images: ';

  STR_20 = 'Virtual note "%s" cannot write file ';



//=======================================================================
//  TKntFile
//=======================================================================

// Create / Destroy  =========================================

{$REGION Create / Destroy }


constructor TKntFile.Create;
var
  i: integer;
begin
  inherited Create;
  FFileName := '';
  FDescription := '';
  FComment := '';
  FDateCreated := now;
  FSavedActiveFolderID := 0;
  FPageCtrl := nil;
  FModified := false;
  FPassPhrase := '';
  FFileFormat := nffKeyNote;
  FCryptMethod := low( TCryptMethod );
  FReadOnly := false;
  FOpenAsReadOnly := false;
  FTrayIconFN := ''; // use default
  FTabIconsFN := ''; // use default
  FPassphraseFunc := nil;
  FShowTabIcons := true;
  FNoMultiBackup := false;
  FSavedWithRichEdit3 := false;
  fNextNNodeGID:= 1;                // by default (for new files), next GID must be 1, not 0
  SetVersion;

  fFolders := TFolderList.Create;
  fNotes := TNoteList.Create;

  for i:= 0 to MAX_BOOKMARKS do
     FBookmarks[i]:= nil;

  FIsBusy := true;
end; // CREATE


destructor TKntFile.Destroy;
var
   F: TKntFolder;
   N: TNote;
   i: integer;
begin
  if fFolders <> nil then begin
     for i := 0 to fFolders.Count-1 do begin
        f:= fFolders[i];
        if F <> nil then
           F.Free;           // It will free all it's NNodes
     end;

    fFolders.Free;
    fFolders := nil;
  end;

  if fNotes <> nil then begin
     for i := 0 to fNotes.Count-1 do begin
        N:= fNotes[i];
        if N <> nil then
           N.Free;          // Note.Free will not free it's associated NNodes (already freed by Folder.Free)
     end;

    fNotes.Free;
    fNotes := nil;
  end;


  inherited Destroy;
end; // DESTROY


{$ENDREGION }


// TKntFile properties  =========================================

{$REGION TKntFile properties }

function TKntFile.GetPassphrase( const FN : string ) : boolean;
begin
  result := false;
  if ( not assigned( FPassphraseFunc )) then exit;
  FPassphrase := FPassphraseFunc( FN );
  result := ( FPassphrase <> '' );
end;


procedure TKntFile.SetVersion;
begin

  case FFileFormat of

    nffKeyNote : begin
      with FVersion do begin
        ID := NFHDR_ID; // GFKNT
        Major := NFILEVERSION_MAJOR;
        Minor := NFILEVERSION_MINOR;

        {
        if ( HasExtendedFolders ) then begin
          ID := NFHDR_ID; // GFKNT
          Major := NFILEVERSION_MAJOR;
          Minor := NFILEVERSION_MINOR;
        end
        else begin
          if _USE_OLD_KEYNOTE_FILE_FORMAT then
            ID := NFHDR_ID_OLD // GFKNX
          else
            ID := NFHDR_ID; // GFKNT
          Major := NFILEVERSION_MAJOR_OLD;
          Minor := NFILEVERSION_MINOR_OLD;
        end;
        }
      end;
    end;

    nffKeyNoteZip : begin
      with FVersion do begin
        ID := NFHDR_ID_COMPRESSED; // GFKNZ
        Major := NFILEVERSION_MAJOR;
        {
        if HasExtendedFolders then
           Major := NFILEVERSION_MAJOR
        else
           Major := NFILEVERSION_MAJOR_NOTREE;
        }
        Minor := NFILEVERSION_MINOR;
      end;
    end;

    nffEncrypted : begin
      with FVersion do begin
        ID := NFHDR_ID_ENCRYPTED; // GFKNE
        Major := NFILEVERSION_MAJOR;
        {
        if HasExtendedFolders then
           Major := NFILEVERSION_MAJOR
        else
           Major := NFILEVERSION_MAJOR_NOTREE;
        }
        Minor := NFILEVERSION_MINOR;
      end;
    end;

  end;
end; // SetVersion


procedure TKntFile.SetDescription( ADescription : TCommentStr );
begin
  ADescription := trim( ADescription );
  if ( FDescription = ADescription ) then exit;
  FDescription := ADescription;
  Modified := true;
end;


procedure TKntFile.SetComment( AComment : TCommentStr );
begin
  AComment := trim( AComment );
  if ( FComment = AComment ) then exit;
  FComment := AComment;
  Modified := true;
end;


procedure TKntFile.SetFileFormat( AFileFormat : TKntFileFormat );
begin
  if ( FFileFormat = AFileFormat ) then exit;
  FFileFormat := AFileFormat;
end;


procedure TKntFile.SetModified( AModified : boolean );
var
  i : integer;
begin
  if FModified = AModified then exit;
  if FIsBusy and AModified then exit;

  FModified := AModified;

  if not FModified then begin
     for i := 0 to FFolders.Count -1 do
        FFolders[i].Modified := false;

     for i := 0 to fNotes.Count -1 do
        fNotes[i].Modified := false;
  end;
end;


procedure TKntFile.SetIsBusy(Value: boolean);
begin
    FIsBusy:= Value;
    if ActiveFile = Self then
       ActiveFileIsBusy:= Value;
end;

procedure TKntFile.SetFilename( const Value : string );
begin
  FFilename := Value;
end;


function TKntFile.GetFile_Name: string;
begin
   Result:= ExtractFileName(FileName);
end;


function TKntFile.GetFile_NameNoExt: string;
begin
   Result:= ExtractFileNameNoExt(FileName);
end;


function TKntFile.GetFile_Path: string;
begin
   Result:= ExtractFilePath(FileName);
end;


function TKntFile.GetBookmark(Index: integer): TLocation;
begin
  Result := FBookmarks[Index];
end;

procedure TKntFile.WriteBookmark (Index: integer; Value: TLocation);
begin
   FBookmarks[Index]:= Value;
end;


procedure TKntFile.UpdateTextPlainVariables (nMax: integer);
var
  i: integer;
  myFolder: TKntFolder;
  AllNotesInitialized: boolean;
  RTFAux: TAuxRichEdit;
begin
    if FIsBusy or (FFolders = nil) then Exit;
    if FTextPlainVariablesInitialized then Exit;

    RTFAux:= CreateAuxRichEdit;
    try
      try
        AllNotesInitialized:= True;

        for i := 0 to FFolders.Count -1 do begin
           myFolder := FFolders[i];
           if not myFolder.InitializeTextPlainVariables(nMax, RTFAux) then
               AllNotesInitialized:= false;
        end;

        if AllNotesInitialized then
           FTextPlainVariablesInitialized:= true;

      except
      end;

    finally
      RTFAux.Free;
    end;

end;



{$ENDREGION }


// Folders  =========================================

{$REGION Folders }

function TKntFile.GetFolderByID( const aID : Cardinal ) : TKntFolder;
var
  i : Cardinal;
  myFolder: TKntFolder;
begin
  result := nil;
  if (FFolders.Count = 0) then exit;

  for i := 0 to FFolders.Count-1 do begin
     myFolder:= FFolders[i];
     if ( myFolder.ID = aID ) then
        exit(myFolder);
  end;
end;

function TKntFile.GetFolderByTreeNode( const myTreeNode: PVirtualNode ) : TKntFolder;
var
  i: integer;
  myTV: TBaseVirtualTree;
  myFolder: TKntFolder;
begin
  result := nil;
  if (FFolders.Count = 0) then exit;

  myTV := TreeFromNode(myTreeNode);
  for i := 0 to FFolders.Count-1 do begin
     myFolder:= FFolders[i];
     if ( myFolder.TV = myTV ) then
        exit(myFolder)
  end;

end;


function TKntFile.GetFolderByName( const aName : string ) : TKntFolder;
// aName is NOT case-sensitive
var
  i: integer;
  myFolder: TKntFolder;
begin
  result := nil;
  if (FFolders.Count = 0) then exit;

  for i := 0 to FFolders.Count-1 do begin
     myFolder:= FFolders[i];
     if ( AnsiCompareText( myFolder.Name, aName ) = 0 ) then
        exit(myFolder);
  end;

end;

function TKntFile.GetFolderByTabIndex(TabIdx: integer): TKntFolder;
var
  i: integer;
  myFolder: TKntFolder;
begin
  result := nil;

  for i := 0 to FFolders.Count-1 do begin
     myFolder:= FFolders[i];
     if (myFolder.TabIndex = TabIdx ) then
        exit(myFolder);
  end;
end;


function TKntFile.InternalAddFolder( AFolder : TKntFolder ) : integer;
begin
  result := Folders.Add( AFolder );
end;


function TKntFile.AddFolder(AFolder : TKntFolder) : integer;
begin
  result := -1;
  if (not assigned(AFolder)) then exit;
  result := InternalAddFolder( AFolder );
  if (AFolder.ID = 0) then
    GenerateFolderID( AFolder );
  Modified := true;
end;


procedure TKntFile.DeleteFolder( AFolder : TKntFolder );
begin
  if AFolder = nil then exit;

  fFolders.Remove(AFolder);
  AFolder.Free;
  RecalcNextNNodeGID;
  Modified := true;
end;


function TKntFile.IsValid(AFolder : TKntFolder ): Boolean;
begin
  Result:= Folders.IndexOf(AFolder) >= 0;
end;


procedure TKntFile.VerifyFolderIds;
var
  i: integer;
  myFolder : TKntFolder;
begin
  for i := 0 to FFolders.Count-1 do begin
     myFolder := FFolders[i];
     if myFolder.ID <= 0 then
        GenerateFolderID(myFolder);
  end;
end;


procedure TKntFile.GenerateFolderID( const AFolder : TKntFolder );
var
  i, hiID : Cardinal;
  myFolder : TKntFolder;
begin
  hiID := 0;

  for i := 0 to FFolders.Count-1 do begin
     myFolder := FFolders[i];
     if myFolder.ID > hiID then
        hiID := myFolder.ID; // find highest folder ID
  end;

  inc( hiID ); // make it one higher
  AFolder.ID := hiID;

end;


function TKntFile.GetFolderCount : integer;
begin
  if assigned( FFolders ) then
    result := FFolders.Count
  else
    result := 0;
end;


{
function TKntFile.HasExtendedFolders : boolean;
var
  i : integer;
begin
  result := false;
  if ( FNotes.Count > 0 ) then
     Result:= true;
end;
}

{$ENDREGION }


// Notes / NNodes  =========================================

{$REGION Notes / NNodes }


procedure TKntFile.GetNNodeByGID( const aGID : Cardinal; var NNode: TNoteNode; var Folder: TKntFolder);
var
  F: TKntFolder;
  i: integer;
begin
  for i := 0 to Folders.Count-1 do begin
     F:= Folders[i];

     NNode:= F.GetNNodeByGID(aGID);
     if NNode <> nil then begin
        Folder:= F;
        exit;
     end;
  end;

  NNode:= nil;
  Folder:= nil;
end;


function TKntFile.GetNoteByGID(const aGID: Cardinal): TNote;
var
   N: TNote;
   i: integer;
begin
  if aGID >= 1 then
     for i := 0 to Notes.Count-1 do begin
        N:= Notes[i];
         if N.GID = aGID then
            exit(N);
     end;
  Result:= nil;
end;


function TKntFile.GetNNodeByFolderAndID (FolderID: Cardinal; NodeID: Word): TNoteNode;
var
   Folder: TKntFolder;
begin
   Result:= nil;
   if (FolderID <> 0) and (NodeID <> 0) then begin
       Folder := GetFolderByID(FolderID);
       if assigned(Folder) then
          Result := Folder.GetNNodeByID(NodeID);
   end;
end;


procedure TKntFile.RecalcNextNNodeGID;
var
  i, j: integer;
  hiGID : Cardinal;
  F : TKntFolder;
  NNode: TNoteNode;

begin
  hiGID:= 0;

  for i := 0 to FFolders.Count-1 do begin
     F := FFolders[i];
     for j := 0 to F.NNodes.Count-1 do begin
        NNode := F.NNodes[j];
        if NNode.GID > hiGID then
           hiGID := NNode.GID;   // find highest note node GID
     end;
  end;

  inc(hiGID);
  fNextNNodeGID:= hiGID;
end;


procedure TKntFile.VerifyNoteGIDs;
var
  F: TKntFolder;
  NN: TNoteNode;
  N: TNote;
  i, j, k: integer;
begin
  for i := 0 to Folders.Count-1 do begin
     F:= Folders[i];

     for j := 0 to F.NNodes.Count-1 do begin
        NN:= F.NNodes[j];

        if NN.GID = 0 then begin
           if (NN.Note.GID <> 0) and (NN.Note.NumEntries=1) then
              NN.GID:= NN.Note.GID
           else begin
              NN.GID := fNextNNodeGID;
              inc(fNextNNodeGID);
              if NN.Note.GID = 0 then
                 NN.Note.GID := NN.GID;
           end;
        end;
     end;
  end;

  // There may be Notes without NNodes (less common, but possible)
  for i := 0 to Notes.Count-1 do begin
     N:= Notes[i];
     if N.GID = 0 then begin
        N.GID := fNextNNodeGID;
        inc(fNextNNodeGID);
     end;
  end;

end;


function TKntFile.AddLoadedNote(Folder: TKntFolder): TNoteNode;
var
  Note: TNote;
  NEntry: TNoteEntry;
  NNode: TNoteNode;

begin
   Note:= TNote.Create;
   NNode:= TNoteNode.Create(Note);
   NEntry:= Note.AddNewEntry;
   NEntry.Created:= 0;

   Folder.AddNNode(NNode);
   Note.AddNNode(NNode, Folder);
   fNotes.Add(Note);
   Result:= NNode;
end;


function TKntFile.AddLoadedNNode(Note: TNote; Folder: TKntFolder; GID: Cardinal): TNoteNode;
var
  NNode: TNoteNode;
begin
   NNode:= TNoteNode.Create(Note);
   NNode.GID:= GID;

   Note.AddNNode(NNode, Folder);
   Folder.AddNNode(NNode);        // -> Modified := true;

   Result:= NNode;
   Modified := false;
end;


function TKntFile.AddNewNote(Folder: TKntFolder; CopyFromNNode: TNoteNode = nil; UpdateImagesCountRef: Boolean= true): TNoteNode;
var
  Note, CopyFromNote, R: TNote;
  NEntry, SourceNEntry: TNoteEntry;
  NNode: TNoteNode;
  nEntries, i: integer;
begin
   Note:= TNote.Create;
   NNode:= TNoteNode.Create(Note);

   NNode.GID:= fNextNNodeGID;    // The first NoteNode of a note shares the GID with the note
   Note.GID:=  fNextNNodeGID;
   inc(fNextNNodeGID);

   fNotes.Add(Note);
   Note.AddNNode(NNode, Folder);
   Folder.AddNNode(NNode);        // -> Modified := true;

   Result:= NNode;

   if not assigned(CopyFromNNode) then
      NEntry:= Note.AddNewEntry

   else begin
      CopyFromNote:= CopyFromNNode.Note;

      Note.LoadStates(CopyFromNote.States);
      Note.Name := CopyFromNote.Name;
      Note.Alias :=  CopyFromNote.Alias;
      NNode.WordWrap:= CopyFromNNode.WordWrap;
      // If CopyFromNote is virtual, the new note should not be (we will ignore VirtualFN)

      nEntries:= CopyFromNote.NumEntries;
      for i:= 0 to nEntries-1 do begin
         SourceNEntry:= CopyFromNote.Entries[i];
         NEntry:= Note.AddNewEntry;
         NEntry.Assign(SourceNEntry);
   //      if UpdateImagesCountRef and NEntry.IsRTF then
   //         ActiveFile.UpdateImagesCountReferences (NEntry);       //%%% You should be able to receive a TNoteEntry instead of TNote
      end;
      if UpdateImagesCountRef then
         ActiveFile.UpdateImagesCountReferences(Note);


      if CopyFromNote.Resources <> nil then
        for i := 0 to CopyFromNote.Resources.Count-1 do
            Note.AddResource(CopyFromNote.Resources[i]);

      NNode.Assign(CopyFromNNode);
   end;

  Modified := true;
end;


function TKntFile.AddNewVirtualNote(Folder: TKntFolder; CopyFromNNode: TNoteNode; UpdateImagesCountRef: Boolean= true): TNoteNode;
var
  Note, CopyFromNote, R: TNote;
  NEntry: TNoteEntry;
  NNode: TNoteNode;
  i: integer;
  List: TStringList;
begin
   assert((CopyFromNNode<> nil) and CopyFromNNode.Note.IsVirtual);

   Note:= TNote.Create;
   NNode:= TNoteNode.Create(Note);

   NNode.GID:= fNextNNodeGID;    // The first NoteNode of a note shares the GID with the note
   Note.GID:=  fNextNNodeGID;
   inc(fNextNNodeGID);

   fNotes.Add(Note);
   Note.AddNNode(NNode, Folder);
   Folder.AddNNode(NNode);        // -> Modified := true;
   NEntry:= Note.AddNewEntry;

   Result:= NNode;

   CopyFromNote:= CopyFromNNode.Note;

   Note.LoadStates(CopyFromNote.States);
   Note.Name := CopyFromNote.Name;
   Note.Alias :=  CopyFromNote.Alias;
   Note.LastModified:= CopyFromNote.LastModified;
   NNode.WordWrap:= CopyFromNNode.WordWrap;

   List:= TStringList.Create;
   try
      LoadVirtualNote(Note, CopyFromNote.VirtualFN, '', List);
   finally
      List.Free;
   end;

   if UpdateImagesCountRef then
      ActiveFile.UpdateImagesCountReferences(Note);

   if CopyFromNote.Resources <> nil then
     for i := 0 to CopyFromNote.Resources.Count-1 do
         Note.AddResource(CopyFromNote.Resources[i]);

   NNode.Assign(CopyFromNNode);
   Modified := true;
end;


function TKntFile.AddNewNNode(Note: TNote; Folder: TKntFolder; CopyFromNNode: TNoteNode = nil; AddToFolder: boolean= true; ForceInherit: boolean = false): TNoteNode;
var
  NNode: TNoteNode;
begin
   NNode:= TNoteNode.Create(Note);
   NNode.GID:= fNextNNodeGID;
   inc(fNextNNodeGID);

   Note.AddNNode(NNode, Folder);
   if AddToFolder then
      Folder.AddNNode(NNode);        // -> Modified := true;

   if (CopyFromNNode <> nil) and (ForceInherit or KntTreeOptions.InheritNodeProperties) then
      NNode.Assign(CopyFromNNode);

   Result:= NNode;

   Modified := true;
end;


function TKntFile.DeleteNote(Note: TNote): Boolean;
var
  GID: Cardinal;
begin
   if Note = nil then exit;

   RemoveImagesCountReferences(Note);
   GID:= Note.GID;

   if Note <> nil then
      Note.Free;

   fNotes.Remove(Note);

   if GID+1 = fNextNNodeGID  then
      RecalcNextNNodeGID;

  Modified := true;
end;



{
 We can have the NNode referenced from a TLocation, along with its associated Folder (the one it had when
 it was calculated), but it could have been moved to another Folder or even deleted
}
function TKntFile.CheckNNode(var NNode: TNoteNode; var Folder: TKntFolder): boolean;
var
  F: TKntFolder;
  i: integer;
begin
  Result:= false;

  if (NNode = nil) or (Folder = nil) then exit;

  if (Folder.NNodes.IndexOf(NNode) >= 0) then exit(true);     // OK. NNode exists in Folder

  for i := 0 to Folders.Count-1 do begin
      F:= Folders[i];
      if F = Folder then continue;
      if (F.NNodes.IndexOf(NNode) >= 0) then begin
         Folder:= F;
         exit(true);
      end;
  end;

  NNode:= nil;
  Folder:= nil;
end;


function TKntFile.HasVirtualNotes : boolean;
var
  i, j : integer;
  F: TKntFolder;
begin
  result := false;

  for i := 0 to FFolders.Count-1 do begin
     F:= FFolders[i];
     for j := 0 to F.NNodes.Count-1 do begin
        if F.NNodes[j].IsVirtual then
           exit(true);
     end;
  end;

end;

function TKntFile.GetVirtualNoteByFileName(const aNote : TNote; FN : string ): TNote;
var
  i, j : integer;
  F : TKntFolder;
  Note: TNote;
begin
  result := nil;

  FN:= FN.ToUpper;
  for i := 0 to FFolders.Count-1 do begin
     F := FFolders[i];
     for j := 0 to F.NNodes.Count-1 do begin
        Note:= F.NNodes[j].Note;
        if Note = aNote then continue;
        if Note.VirtualFN.ToUpper = FN then
           exit(Note);
     end;
  end;
end;


{$ENDREGION }


// Images  =========================================

{$REGION Images }


procedure TKntFile.UpdateImagesStorageModeInFile (ToMode: TImagesStorageMode; ApplyOnlyToFolder: TKntFolder= nil; ExitIfAllImagesInSameModeDest: boolean = true);
var
  i, j: integer;
  myFolder: TKntFolder;
  Note: TNote;
  NNode: TNoteNode;
  NEntry: TNoteEntry;
  Stream: TMemoryStream;
  ImagesIDs: TImageIDs;

   procedure UpdateImagesStorageMode (Stream: TMemoryStream);
   var
     ReplaceCorrectedIDs: boolean;
   begin
       if ToMode <> smEmbRTF then begin
          ImagesIDs:= myFolder.CheckSavingImagesOnMode (imLink, Stream, ExitIfAllImagesInSameModeDest);
          ImageMng.UpdateImagesCountReferences (nil, ImagesIDs);
          if (SavedActiveFolderID = myFolder.ID) then
             myFolder.ImagesReferenceCount:= ImagesIDs;
       end
       else
          myFolder.CheckSavingImagesOnMode (imImage, Stream, ExitIfAllImagesInSameModeDest);
   end;

begin
   if (ApplyOnlyToFolder = nil)  then
      ImageMng.ResetAllImagesCountReferences;

   // ApplyOnlyToFolder: Para usar desde MergeFromKNTFile

   for i := 0 to FFolders.Count -1 do begin
      myFolder := FFolders[i];
      if (ApplyOnlyToFolder <> nil) and (myFolder <> ApplyOnlyToFolder) then continue;

      myFolder.EditorToDataStream;

      for j := 0 to myFolder.NNodes.Count - 1 do  begin
         NNode:= myFolder.NNodes[j];
         Note:= NNode.Note;
         if not Note.IsVirtual then begin
            if Note.NNodes[0].NNode <> NNode then continue;
            NEntry:= NNode.Note.Entries[0];                     //%%%
            if NEntry.IsPlainTXT then continue;

            Stream:= NEntry.Stream;
            UpdateImagesStorageMode (Stream);
            if Length(ImagesIDs) > 0 then
               NEntry.TextPlain:= '';      // Will have updated the Stream but not the editor, and been able to introduce/change image codes => force it to be recalculated when required

            if NNode = myFolder.FocusedNNode  then
               myFolder.DataStreamToEditor;
         end;
      end;

   end;

end;


function TKntFile.EnsurePlainTextAndRemoveImages (myFolder: TKntFolder): boolean;
var
  i: integer;
  NNode: TNoteNode;
  NEntry: TNoteEntry;
  Stream: TMemoryStream;
  RTFAux : TRxRichEdit;

  procedure EnsurePlainTextAndCheckRemoveImages (UpdateEditor: boolean);
  var
     ImagesIDs: TImageIDs;
  begin
      ImagesIDs:= ImageMng.GetImagesIDInstancesFromRTF (Stream);
      if Length(ImagesIDs) > 0 then
         ImageMng.RemoveImagesReferences (ImagesIDs);

      if NodeStreamIsRTF (Stream) then begin
         Stream.Position:= 0;
         ConvertStreamContent(Stream, sfRichText, sfPlainText, RTFAux);
      end;

      if UpdateEditor then
         myFolder.DataStreamToEditor;
  end;


begin
   Result:= true;

   try
      RTFAux:= CreateAuxRichEdit;
      try
         for i := 0 to myFolder.NNodes.Count - 1 do  begin
            NNode:= myFolder.NNodes[i];
            if not NNode.IsVirtual then begin
               NEntry:= NNode.Note.Entries[0];
               Stream:= nEntry.Stream;
               EnsurePlainTextAndCheckRemoveImages (NNode = myFolder.FocusedNNode );
            end;
         end;
         myFolder.ResetImagesReferenceCount;

      finally
        RTFAux.Free;
      end;

   except on E: Exception do begin
     MessageDlg( STR_19 + E.Message, mtError, [mbOK], 0 );
     Result:= false;
     end
   end;


end;


procedure TKntFile.RemoveImagesCountReferences (myFolder: TKntFolder);
var
  i: integer;
  Note: TNote;
  NNode: TNoteNode;
begin
   for i := 0 to myFolder.NNodes.Count - 1 do  begin
      NNode:= myFolder.NNodes[i];
      Note:= NNode.Note;
      if not Note.IsVirtual then begin
         if (Note.NumNNodes = 1) or not Note.HasNNodesInOtherFoldersThan(myFolder) then begin
            if Note.NNodes[0].NNode = NNode then
               RemoveImagesCountReferences(Note);
         end;
      end;
   end;
   myFolder.ResetImagesReferenceCount;
end;


procedure TKntFile.RemoveImagesCountReferences (Note: TNote);
var
  NEntry: TNoteEntry;
  Stream: TMemoryStream;
  ImagesIDs: TImageIDs;

begin
   if not Note.IsVirtual then begin
      NEntry:= Note.Entries[0];        // %%%
      if not NEntry.IsRTF then exit;

      Stream:= NEntry.Stream;
      ImagesIDs:= ImageMng.GetImagesIDInstancesFromRTF (Stream);
      if Length(ImagesIDs) > 0 then
         ImageMng.RemoveImagesReferences (ImagesIDs);
   end;
end;

procedure TKntFile.UpdateImagesCountReferences (Note: TNote);
var
  NEntry: TNoteEntry;
  Stream: TMemoryStream;
  ImagesIDs: TImageIDs;

begin
   NEntry:= Note.Entries[0];        // %%%
   if not NEntry.IsRTF then exit;
   Stream:= NEntry.Stream;
   ImagesIDs:= ImageMng.GetImagesIDInstancesFromRTF (Stream);
   if Length(ImagesIDs) > 0 then
      ImageMng.UpdateImagesCountReferences (nil, ImagesIDs);
end;


// To be used from MergeFromKNTFile
procedure TKntFile.UpdateImagesCountReferences (myFolder: TKntFolder);
var
  i: integer;
  Note: TNote;
  NNode: TNoteNode;

begin
   for i := 0 to myFolder.NNodes.Count - 1 do  begin
      NNode:= myFolder.NNodes[i];
      Note:= NNode.Note;
      if not Note.IsVirtual then begin
         if Note.NNodes[0].NNode = NNode then
            UpdateImagesCountReferences(Note);
      end;
   end;

end;

{$ENDREGION }


// Load / Save  =========================================

{$REGION Load / Save }

function TKntFile.Load( FN : string; ImgManager: TImageMng; var ClipCapIdx: integer; AddProcessAlarms: boolean) : integer;
var
  Folder : TKntFolder;
  Attrs : TFileAttributes;
  Stream : TFileStream;
  MemStream : TMemoryStream;
  ds, ds1 : AnsiString;
  ch : AnsiChar;
  p, i : integer;
  HasLoadError, FileIDTestFailed : boolean;
  tf: TTextFile;
  OldLongDateFormat,
  OldShortDateFormat : string;
  OldLongTimeFormat : string;
  OldDateSeparator,
  OldTimeSeparator : char;
  ID_CHAR : AnsiChar;
  FileExhausted : boolean;
  InHead : boolean;
  TestString : string[12];
  VerID : TKntFileVersion;
  NextBlock: TNextBlock;


begin
  result := -1; // error before opening file
  Folder := nil;
  HasLoadError := false;

  FFileFormat := nffKeyNote; // assume
  NextBlock:= nbRTF;

  if ( FN = '' ) then
     FN := FFileName;
  if ( FFileName = '' ) then
     FFileName := FN;

  if ( not FileExists( FN )) then begin
     App.DoMessageBox(Format( STR_01, [FN] ), mtError, [mbOK], 0);
     raise Exception.Create('');
  end;


  ClipCapIdx := -1;

  // check if file is read-only; if so, set FReadOnly flag
  Attrs := TFile.GetAttributes(FN);
  if (TFileAttribute.faReadOnly in Attrs) then
    FReadOnly := true;

  ImgManager.KntFile:= Self;

  result := 1;
  Stream := TFileStream.Create( FN, ( fmOpenRead or fmShareDenyWrite ));

  FileIDTestFailed := true; // assume the worst
  result := 2;

  try
    // short test for file format

    SetLength( TestString, 12 );
    Stream.ReadBuffer( TestString[1], 12 );

    if ( pos( NFHDR_ID, TestString ) > 0 ) then begin
      FFileFormat := nffKeyNote;
      _IS_OLD_KEYNOTE_FILE_FORMAT := false;
      VerID.ID := NFHDR_ID;
    end
    else
    if ( pos( NFHDR_ID_OLD, TestString ) > 0 ) then begin
      FFileFormat := nffKeyNote;
      _IS_OLD_KEYNOTE_FILE_FORMAT := true;
      VerID.ID := NFHDR_ID_OLD;
    end
    else
    if ( pos( NFHDR_ID_COMPRESSED, TestString ) > 0 ) then begin
      FFileFormat := nffKeyNoteZip;
      _IS_OLD_KEYNOTE_FILE_FORMAT := false;
      VerID.ID := NFHDR_ID_COMPRESSED;
    end
    else
    if ( pos( NFHDR_ID_ENCRYPTED, TestString ) > 0 ) then begin
      FFileFormat := nffEncrypted;
      _IS_OLD_KEYNOTE_FILE_FORMAT := false;
      VerID.ID := NFHDR_ID_ENCRYPTED;
    end
    else begin
      App.DoMessageBox(Format( STR_02, [FN] ), mtError, [mbOK], 0);
      raise Exception.Create('');
      exit;
    end;

    Stream.Position := 0;
    result := 3;

  finally
    Stream.Free;
    Stream := nil;
  end;

  MemStream := nil;
  try
    try
      if ( FFileFormat = nffEncrypted ) then begin
        MemStream := TMemoryStream.Create;

        repeat // repeatedly prompt for passphrase, unless other action chosen
            if ( not GetPassphrase( FN )) then
              raise EKeyKntFileError.Create( STR_03 );

            try
              DecryptFileToStream( FN, MemStream );
              break; // no error, so exit this loop
            except
              On e : EPassphraseError do begin
                HasLoadError := false;
                if ( messagedlg(STR_04, mtError, [mbYes,mbNo], 0  ) <> mrYes ) then raise;
              end;
            end;

        until false;

        TestString := FVersion.ID + #32 + FVersion.Major + '.' + FVersion.Minor;

      end;

      if ( FFileFormat = nffKeyNoteZip ) then begin
          var PosIniNonCompressed, NToRead, PosToWrite: Int64;

          MemStream := TMemoryStream.Create;
          Stream := TFileStream.Create(FN, fmOpenRead);
          try
             Stream.ReadBuffer(FVersion, sizeof( FVersion ));
             Stream.ReadBuffer(FCompressionLevel, sizeof( FCompressionLevel ));
             PosIniNonCompressed:= ZDecompressStream (Stream, MemStream);
             TestString := FVersion.ID + #32 + FVersion.Major + '.' + FVersion.Minor;
             if PosIniNonCompressed > 0 then begin
                 Stream.Position := PosIniNonCompressed;
                 NToRead:= Stream.Size - (Stream.Position + 1);
                 PosToWrite:= MemStream.Position;
                 MemStream.SetSize(MemStream.Size + NToRead);
                 Stream.Read(PByte(MemStream.Memory)[PosToWrite], NToRead);
             end;


          finally
             Stream.Free;
             Stream := nil;
          end;

          Log_StoreTick( 'After decompressed stream', 1 );
      end;

      if ( FFileFormat = nffKeyNote ) then begin
          MemStream := TMemoryStream.Create;
          MemStream.LoadFromFile(FN);
      end;

      case FFileFormat of
        nffKeyNote, nffKeyNoteZip, nffEncrypted : begin
          if _TEST_KEYNOTE_FILE_VERSION then begin  // global var, allows to bypass testing
              p := pos( VerID.ID, TestString );
              delete( TestString, 1, p+ID_STR_LENGTH );
              if ( length( TestString ) > 2 ) then begin
                 VerID.Major := TestString[1];
                 VerId.Minor := TestString[3];

                 if (( VerID.Major in ['0'..'9'] ) and ( VerID.Minor in ['0'..'9'] )) then begin
                    if ( VerID.Major > NFILEVERSION_MAJOR ) then begin
                       App.DoMessageBox(Format( STR_05, [ExtractFilename( FN ), NFILEVERSION_MAJOR, NFILEVERSION_MINOR, VerID.Major, VerID.Minor] ), mtError, [mbOK], 0);
                       raise EKeyKntFileError.Create('');
                    end;

                    if (VerID.Major = NFILEVERSION_MAJOR) and ( VerID.Minor > NFILEVERSION_MINOR ) then begin
                       case App.DoMessageBox( ExtractFilename( FN ) + STR_06, mtWarning, [mbYes,mbNo,mbCancel,mbHelp], _HLP_KNTFILES ) of
                         mrNo : begin
                           // nothing, just fall through
                         end;
                         mrCancel : begin
                           // do not open the file at all
                           result := 4;
                           exit;
                         end;
                         else // mrYes and all other responses
                           FReadOnly := true;
                       end;
                    end;
                    FileIDTestFailed := false;
                 end;
                 FVersion:= VerID;
              end;
          end
          else
            FileIDTestFailed := false;

          if FileIDTestFailed then begin
            App.DoMessageBox(Format( STR_07, [ExtractFilename( FN )] ), mtError, [mbOK], 0);
            raise EKeyKntFileError.Create('');
          end;

          InHead := true;

          OldShortDateFormat := FormatSettings.ShortDateFormat;
          OldLongDateFormat := FormatSettings.LongDateFormat;
          OldLongTimeFormat := FormatSettings.LongTimeFormat;
          OldDateSeparator := FormatSettings.DateSeparator;
          OldTimeSeparator := FormatSettings.TimeSeparator;
          FormatSettings.DateSeparator := _DATESEPARATOR;
          FormatSettings.TimeSeparator := _TIMESEPARATOR;
          FormatSettings.ShortDateFormat := _SHORTDATEFMT;
          FormatSettings.LongDateFormat := _LONGDATEFMT;
          FormatSettings.LongTimeFormat := _LONGTIMEFMT;
          FileExhausted := false;

          tf:= TTextFile.Create();
          tf.assignstream( MemStream );

          tf.Reset;

          try
            while ( not tf.eof) do begin
              ds:= tf.readln();
              if ( ds = '' ) then continue;

              if ( ds[1] = _NF_COMMENT ) then begin
                 if InHead then begin
                    if ( length( ds ) > 2 ) then begin
                      ID_CHAR := upcase( ds[2] );
                      delete( ds, 1, 2 );
                      ds := System.AnsiStrings.Trim( ds );

                      case ID_CHAR of
                        _NF_AID : begin // Version ID
                          // [x] verify ID and version
                        end;
                        _NF_DCR : begin // Date Created
                          try
                            FDateCreated := strtodatetime( ds );
                          except
                            FDateCreated := now;
                          end;
                        end;
                        _NF_FCO : begin // File comment
                          FComment := TryUTF8ToUnicodeString(ds);
                        end;
                        _NF_FDE : begin // File description
                          FDescription := TryUTF8ToUnicodeString(ds);
                        end;
                        _NF_ACT : begin // Active folder
                            FSavedActiveFolderID := StrToUIntDef( ds, 0 );
                        end;
                        _NF_ClipCapFolder : begin
                            ClipCapIdx := StrToIntDef( ds, -1 );
                        end;
                        _NF_FileFlags : begin
                          FlagsStringToProperties( ds );
                        end;
                        _NF_TrayIconFile : begin
                          if ( ds <> '' ) then
                            FTrayIconFN := ds;
                        end;
                        _NF_TabIconsFile : begin
                          if ( ds <> '' ) then
                            FTabIconsFN := ds;
                        end;
                        _NF_ReadOnlyOpen : begin // obsolete (flags)
                          FOpenAsReadOnly := ( ds = BOOLEANSTR[true] );
                          if FOpenAsReadOnly then FReadOnly := true;
                        end;
                        _NF_ShowTabIcons : begin // obsolete (flags)
                          FShowTabIcons := ( ds = BOOLEANSTR[true] );
                        end;
                      end; // case ID_CHAR
                    end; // length( ds ) > 2
                 end; // InHead
                continue;
              end; // _NF_COMMENT



             if Copy(ds,1,2) = _NumNotes then begin
                var NumNotes: integer;
                InHead := false;
                NextBlock:= nbNotes;
                NumNotes:= StrToIntDef(Copy(ds,4), 0);
                fNotes.Capacity:= NumNotes;
                break;
             end;

              // '%' markers, start a new entry
              if FVersion.Major < '3' then
                 if ( ds = _NF_TabFolder ) then begin          // Old KNT files
                   InHead := false;
                   NextBlock:= nbRTF;
                   break;
                 end;

              if ( ds = _NF_Folder ) then begin       // = _NF_TreeFolder in Old KNT Files
                InHead := false;
                NextBlock:= nbTree;
                break;
              end;

              if ( ds = _NF_Bookmarks ) then begin
                InHead := false;
                NextBlock:= nbBookmarks;        // Bookmarks begins
                break;
              end;

              if ( ds = _NF_StoragesDEF ) then begin
                InHead := false;
                NextBlock:= nbImages;         // Images definition begins
                break;
              end;

              if ( ds = _NF_EOF ) then begin
                InHead := false;
                FileExhausted := true;
                break;
              end;

            end; // eof( tf )

            while ( not ( FileExhausted or tf.eof)) do begin
               if NextBlock = nbBookmarks then
                   LoadBookmarks(tf, FileExhausted, NextBlock)

               else if NextBlock = nbImages then
                   ImgManager.LoadState(tf, FileExhausted)

               else if NextBlock = nbNotes then
                   LoadNotes(tf, FileExhausted, NextBlock)

               else begin
                   case NextBlock of
                     nbRTF, nbTree  : Folder := TKntFolder.Create(Self);
                   end;

                   try
                     Folder.LoadFromFile( tf, FileExhausted, NextBlock, (NextBlock = nbRTF));
                     AddFolder(Folder);
                     if AddProcessAlarms then
                        AlarmMng.AddProcessedAlarms();

                     // if assigned( FOnNoteLoad ) then FOnNoteLoad( self );
                   except
                     On E : Exception do begin
                       HasLoadError := true;
                       messagedlg( STR_08 + Folder.Name + #13#13 + E.Message, mtError, [mbOK], 0 );
                       Folder.Free;
                       // raise;
                     end;
                   end;
               end;
            end; // EOF( tf )


          finally
            FormatSettings.DateSeparator := OldDateSeparator;
            FormatSettings.TimeSeparator := OldTimeSeparator;
            FormatSettings.ShortDateFormat := OldShortDateFormat;
            FormatSettings.LongDateFormat := OldLongDateFormat;
            FormatSettings.LongTimeFormat := OldLongTimeFormat;
            tf.CloseFile;
            tf.Free;
            if assigned( MemStream ) then MemStream.Free;
          end;

        end; // nffKeyNote

      end;

    except
      raise;
    end;

  finally
     if assigned( Stream ) then Stream.Free;

     VerifyFolderIds;
     RecalcNextNNodeGID;
     VerifyNoteGIDs;
     ImgManager.FileIsNew:= false;

     ConvertOldMirrorNodesToNNodes;                  // Convert old "Mirror nodes" in normal TNoteNodes

     Modified := false;
  end;

  if HasLoadError then
     result := 99
  else
     result := 0;

end; // Load


procedure TKntFile.LoadVirtualNote (Note: TNote; const VirtFN, RelativeVirtFN: string; List: TStringList);
var
   IsRTF: boolean;
begin
   try
     Note.SetVirtualFN(VirtFN, RelativeVirtFN, File_Path);
     Note.LoadVirtualFile;
   except
     on E : Exception do begin
       List.Add( STR_10 );
       List.Add( Note.VirtualFN );
       List.Add( E.Message );
       Note.VirtualFN := _VIRTUAL_NODE_ERROR_CHAR + Note.VirtualFN;
       Note.Entries[0].IsRTF:= False;
       TransferNEntryText(List, Note.Entries[0].Stream, IsRTF);
     end;
   end;
end;


procedure TKntFile.LoadNotes(var tf : TTextFile; var FileExhausted : boolean; var NextBlock: TNextBlock);
var
  InNote, InNoteEntry, InEntryContent, RTFContent : boolean;
  List : TStringList;
  s, key : AnsiString;
  p, linecount : integer;

  Note: TNote;
  NEntry: TNoteEntry;
  NEntryID, NoteSelEntryID: Word;

  VirtualFN, RelativeVirtualFN: string;


    procedure AddTextToNewNEntry (ClosingNote: boolean = true);
    var
      IsRTF: boolean;
      NSelEntry: TNoteEntry;
    begin
      InEntryContent := false;
      Note.AddEntry(NEntry, NEntryID);

      if (VirtualFN <> '') or (RelativeVirtualFN <> '') then
         LoadVirtualNote (Note, VirtualFN, RelativeVirtualFN, List)

      else begin
         TransferNEntryText(List, NEntry.Stream, IsRTF);      // transfer Text data (RTF or plain text) from list to Note Entry
         assert((IsRTF = RTFContent) or (NEntry.Stream.Size=0));
         NEntry.IsRTF:= RTFContent;
      end;

      if ClosingNote then begin
         NSelEntry:= Note.GetEntry(NoteSelEntryID);
         Note.SelEntry:= NSelEntry;
      end;

      List.Clear;
      VirtualFN:= '';
      RelativeVirtualFN:= '';
    end;

begin
  FileExhausted := false;
  InNote := false;
  InNoteEntry := false;
  InEntryContent := false;

  List := TStringList.Create;
  List.BeginUpdate;
  try
    while (not tf.eof()) do begin
       s:= tf.readln();

       if (s = _NF_TxtContent) then begin
         // Entry content (plain Txt) begins
         InEntryContent := true;
         RTFContent := false;
         continue;
       end;
       if (s = _NF_RTFContent) then begin
         // Entry content (RTF) begins
         InEntryContent := true;
         RTFContent := true;
         continue;
       end;
       if ( s = _NF_Note) then begin
         // new TNote begins
         if ( InNote ) then AddTextToNewNEntry;
         InNote := true;
         InNoteEntry := false;

         Note:= TNote.Create;
         fNotes.Add(Note);
         NoteSelEntryID:= 0;
         NEntry:= nil;
         continue;
       end;
       if ( s = _NF_NEntry) then begin
         // new TNoteEntry begins
         if ( InEntryContent ) then AddTextToNewNEntry (false);
         InNoteEntry := true;
         NEntry:= TNoteEntry.Create;
         NEntryID:= 0;                    // By default
         continue;
       end;
       if ( s = _NF_Folder ) then begin
         NextBlock:= nbTree;
         if assigned(NEntry) then AddTextToNewNEntry;
         break; // New TreeNote begins
       end;
       if ( s = _NF_StoragesDEF ) then begin
         NextBlock:= nbImages;
         if assigned(NEntry) then AddTextToNewNEntry;
         break; // Images definition begins
       end;
       if ( s = _NF_Bookmarks ) then begin
         NextBlock:= nbBookmarks;
         if assigned(NEntry) then AddTextToNewNEntry;
         break; // Bookmarks begins
       end;
       if ( s = _NF_EOF ) then begin
         FileExhausted := true;
         if assigned(NEntry) then AddTextToNewNEntry;
         break; // END OF FILE
       end;


       if InEntryContent then begin
         if not RTFContent then
            delete( s, 1, 1 ); // strip _NF_PLAINTEXTLEADER
         List.Add( s );
         continue;
       end;


       p := pos('=', s);
       if ( p <> 3 ) then continue; // not a valid key=value format
       key := copy(s, 1, 2);
       delete(s, 1, 3);


       if InNote then begin
          if ( key = _NoteName ) then
            Note.Name:= TryUTF8ToUnicodeString(s)
          else
          if ( key = _NoteAlias ) then
            Note.Alias:= TryUTF8ToUnicodeString(s)
          else
          if ( key = _NoteGID ) then
              Note.GID:= StrToUIntDef(s, 0)
          else
          if ( key = _NoteState ) then
              Note.StringToStates(s)
          else
          if ( key = _LastModified ) then
              fDateCreated := StrToDateTimeDef(s, 0)
          else
          if ( key = _VirtualFN ) then
            VirtualFN := TryUTF8ToUnicodeString(s)
          else
          if ( key = _RelativeVirtualFN ) then
            RelativeVirtualFN := TryUTF8ToUnicodeString(s)
          else
          if ( key = _NoteSelEntry ) then
             NoteSelEntryID:= StrToUIntDef(s, 0)
          else
          if ( key = _NEntrySelStart ) then begin
              if _SAVE_RESTORE_CARETPOS then
                 Note.SelStart := StrToIntDef( s, 0 )
              else
                 Note.SelStart := 0;
          end;

          //	ToDO:     fResources: TResourceList;  _NoteResources = 'NR';

          continue;
       end; // if InNoteNode ...


       if InNoteEntry then begin
          if ( key = _NEntryID ) then
              NEntryID := StrToUIntDef(s, 0)
          else
          if ( key = _NoteState ) then
              Note.StringToStates(s)
          else
          if ( key = _DateCreated ) then
              fDateCreated := StrToDateTimeDef(s, 0);

           // ToDO:       fTags: TNoteTagList;
       end;


    end; { while not eof( tf ) }

  finally
    List.EndUpdate;
    List.Free;
  end;

  FModified := false;

end;


procedure TransferNEntryText(ListTextStr : TStringList; StreamText: TMemoryStream; var IsRTF: boolean);
var
   NewRTF: string;
begin
    IsRTF:= false;
    if (ListTextStr.Count = 0) then exit;

    if App.opt_Clean then begin
       if CleanRTF(ListTextStr.Text, NewRTF) then begin
          StreamText.LoadFromStream(TStringStream.Create(NewRTF));
          exit;
       end;
    end;

    ListTextStr.WriteBOM:= False;
    ListTextStr.SaveToStream(StreamText);
    if NodeStreamIsRTF(StreamText) then begin
      IsRTF:= true;
      // In notes/nodes with RTF content we are interested in the buffer ending in #0 to be able to treat it as a string (accessing .Memory)
      assert((PByte(StreamText.Memory)[StreamText.Size-1] <> 0), 'The Stream already ends at 0');
      StreamText.WriteData(0);
    end;
end;


procedure TKntFile.ConvertOldMirrorNodesToNNodes;
{
 By the time we are recovering the notes from the old format we can see a reference to a mirror node, which can
 be pointing to a note that we have not yet processed. Therefore we cannot process it from Folder, but from File
 upon completion of the Load of all folders. And at that moment we will decide that a note created should be deleted
 and instead we must create a new NNode on another note.

 We have marked the notes from Folder.LoadFromFile by calling Note.SetLoadingAsOldMirror(s)
}

var
   F: TKntFolder;
   N, TargetN: TNote;
   NN, NewNN: TNoteNode;
   i, j: integer;

   function GetLinkedNote (N: TNote): TNote;
   var
      p: integer;
      GID: Integer;
      Folder: TKntFolder;
      NNode: TNoteNode;
   begin
       Result:= nil;
       p := pos( KNTLINK_SEPARATOR, N.VirtualFN );
       if p > 0 then begin
          NNode:= GetNNodeByFolderAndID(StrToInt(Copy(N.VirtualFN,1, p-1) ), StrToInt(Copy(N.VirtualFN, p+1)));     // FolderID, NoteID
          if NNode <> nil then
             Result:= NNode.Note;
       end
       else begin
          GID:= StrToIntDef(N.VirtualFN, 0);
          Result:= GetNoteByGID(GID);
       end;
   end;

begin
   for i := 0 to Folders.Count-1 do begin
      F:= Folders[i];

      for j := 0 to F.NNodes.Count-1 do begin
          NN:= F.NNodes[j];
          N:= NN.Note;
          if nsLoading_OldMirror in N.States then begin
             TargetN:= GetLinkedNote(N);
             if TargetN <> nil then begin
                NewNN:= AddNewNNode(TargetN, F, NN, false, true);    // Don't add to Folder, let's replace the one located in [i]
                F.NNodes[j]:= NewNN;
                Self.DeleteNote(N);
             end;
          end;
      end;
   end;
end;


function TKntFile.ConvertKNTLinksToNewFormatInNotes (NoteGIDs: TMergedNotes; var GIDsNotConverted: integer): boolean;
var
  i, j: integer;
  NNode: TNoteNode;
  NEntry: TNoteEntry;
  Folder: TKntFolder;
  NewRTF: AnsiString;

begin
  Result:= false;

  for i := 0 to FFolders.Count-1 do begin
     Folder := FFolders[i];
     if assigned(NoteGIDs) and (Folder.Info = 0) then continue;                   // only selected folders

     for j := 0 to Folder.NNodes.Count-1 do begin
        NNode := Folder.NNodes[j];
        NEntry:= NNode.Note.Entries[0];          // %%%
        if NEntry.IsHTML then continue;

        NewRTF:= ConvertKNTLinksToNewFormat(NEntry.Stream.Memory, NEntry.Stream.Size, NoteGIDs, GIDsNotConverted);
        if NewRTF <> '' then begin
           NEntry.Stream.SetSize(Length(NewRTF));
           NEntry.Stream.Position:= 0;
           StringToMemoryStream(NewRTF, NEntry.Stream);
           Result:= true;
        end;
     end;
  end;

end;


{FN:   Where to create and save the file.
       - Can be a temporal file. For safety, we will write data to a temp file, and only overwrite
         the actual keynote file after the save process is complete. This will be done by the caller
         (KntFileSave, in kn_NoteFileMng))
       - Can be a file selected as a copy (File -> Copy To...)

       In both cases, the actual keynote file won't be modified (in the first one, at least here, in this
       TKntFile.Save method)

       (FN can't be ''. When the user clicks on Save As.., KntFileSave, will ask for a new
       filename, that must be passed here, in FN)

      Also, in both cases, when saving the .knt file, although it may be a copy to another directory,
      modified virtual file nodes will be saved too. So, it is important that, if the virtual
      files nodes must be backed (if it applies, based on configuration), it is done.
      The assingment of _VNKeyKntFileName ensures it (must be done by the caller)
}
function TKntFile.Save(FN: string;
                        var SavedFolders: integer; var SavedNodes: integer;
                        ExportingMode: boolean= false; OnlyCurrentNodeAndSubtree: PVirtualNode= nil;
                        OnlyNotHiddenNodes: boolean= false; OnlyCheckedNodes: boolean= false): integer;
var
  i : integer;
  Stream : TFileStream;
  myFolder : TKntFolder;
  ds : AnsiString;
  tf : TTextFile;
  AuxStream : TMemoryStream;


  procedure WriteNEntry (NEntry: TNoteEntry; Note: TNote);
  begin
     tf.WriteLine(_NF_NEntry);                              // TNoteEntry begins
     if NEntry.ID <> 0 then
        tf.WriteLine(_NEntryID + '=' + NEntry.ID.ToString );

     if NEntry.Created <> 0 then
       tf.WriteLine(_DateCreated + '=' + FormatDateTime(_DATETOFILE, NEntry.Created) );
     if (NEntry.States <> []) and (NEntry.States <> [nesModified]) then
       tf.WriteLine(_NoteState + '=' + NEntry.StatesToString);

     // ToDO: fTags: TNoteTagList

     if not Note.IsVirtual then
        SaveTextToFile(tf, NEntry.Stream, NEntry.IsPlainTXT);
  end;


  procedure WriteNote (Note: TNote);
  var
    i: integer;
  begin
    tf.WriteLine(_NF_Note);              // TNote begins
    tf.WriteLine(_NoteGID + '=' + Note.GID.ToString );    // Here means NoteGID
    tf.WriteLine(_NoteName + '=' + Note.Name, True);
    if Note.Alias <> '' then
       tf.WriteLine(_NoteAlias + '=' + Note.Alias, True);
    if Note.LastModified <> 0 then
       tf.WriteLine(_LastModified + '=' + FormatDateTime(_DATETOFILE, Note.LastModified) );
    if (Note.States <> []) and (Note.States <> [nsModified]) then
       tf.WriteLine(_NoteState + '=' + Note.StatesToString);

    if Note.SelEntry.ID > 0 then
       tf.WriteLine(_NoteSelEntry + '=' + Note.SelEntry.ID.ToString  );
    if ( _SAVE_RESTORE_CARETPOS and ( Note.SelStart > 0 )) then
       tf.WriteLine( _NEntrySelStart + '=' + Note.SelStart.ToString  );
    if Note.Resources <> nil then
       tf.WriteLine(_NoteResources + '=' + Note.ResourcesToString  );

    if Note.IsVirtual then begin
      if Note.HasVNodeError then
         // there was an error when we tried to load this file, so don't try to save it (assume no valid data in node)
          tf.WriteLine( _VirtualFN + '=' + copy( Note.VirtualFN, 2, length( Note.VirtualFN )), True )
      else
          try
             Note.SaveVirtualFile;
             tf.WriteLine( _VirtualFN + '=' + Note.VirtualFN, True  );
             tf.WriteLine( _RelativeVirtualFN + '=' + ExtractRelativePath(ActiveFile.File_Path, Note.VirtualFN), True  );
          except
            on E : Exception do
              // [x] A note may have hundreds of nodes.We should allow user to ABORT here or to skip subsequent error messages
              App.DoMessageBox(Format(STR_20 + #13#13+ '%s', [Note.Name, Note.VirtualFN, E.Message]), mtError, [mbOK], 0 );
          end;
    end;

    for i := 0 to High(Note.Entries) do
       WriteNEntry(Note.Entries[i], Note);

  end;

  procedure WriteFolder (myFolder: TKntFolder);
  begin
      try
        if assigned( myFolder ) then begin
          if ExportingMode and not (myFolder.Info > 0) then     // Notes to be exported are marked with Info=1
             Exit;

          SavedNodes:= SavedNodes + myFolder.SaveToFile( tf, OnlyCurrentNodeAndSubtree, OnlyNotHiddenNodes, OnlyCheckedNodes);
          inc (SavedFolders);
        end;
      except
        on E : Exception do begin
            result := 3;
            App.DoMessageBox( Format(STR_13, [myFolder.Name, E.Message]), mtError, [mbOK], 0 );
            exit;
        end;
      end;
  end;


  procedure WriteKntFile (SaveImages: boolean);
  var
     i: integer;
     ClipCapOnFolder: TKntFolder;
  begin
    //writeln(tf, _NF_COMMENT, _NF_AID, FVersion.ID, #32, FVersion.Major + '.' + FVersion.Minor );
    if FFileFormat = nffKeyNote then begin
       tf.WriteLine( _NF_COMMENT + _NF_AID + FVersion.ID + ' ' + FVersion.Major + '.' + FVersion.Minor);
       tf.WriteLine(_NF_WARNING);
    end;
    tf.WriteLine(_NF_COMMENT + _NF_FDE + FDescription, True );
    tf.WriteLine(_NF_COMMENT + _NF_FCO + FComment, True );

    tf.WriteLine(_NF_COMMENT + _NF_ACT + FSavedActiveFolderID.ToString );

    tf.WriteLine(_NF_COMMENT + _NF_DCR + FormatDateTime( _SHORTDATEFMT + ' ' + _LONGTIMEFMT, FDateCreated ) );
    tf.WriteLine(_NF_COMMENT + _NF_FileFlags + PropertiesToFlagsString );
    if ( TrayIconFN <> '' ) then
      tf.WriteLine( _NF_COMMENT + _NF_TrayIconFile + TrayIconFN );
    if ( FTabIconsFN <> '' ) then
      tf.WriteLine( _NF_COMMENT + _NF_TabIconsFile + FTabIconsFN );
    with ClipCapMng do begin
       if ClipCapActive then begin
          var Idx: integer;
          ClipCapOnFolder:= ClipCapFolder;
          if assigned(ClipCapOnFolder) then
             Idx:= ClipCapOnFolder.TabIndex             //ClipCapOnFolder.TabSheet.PageIndex.ToString
          else
             Idx:= Scratch_TabIdX;
          tf.WriteLine( _NF_COMMENT + _NF_ClipCapFolder + Idx.ToString );
       end;
    end;

    // Save Notes (TNote) with its Entries (TNoteEntry)
    tf.WriteLine(_NumNotes + '=' + FNotes.Count.ToString);
    for i := 0 to FNotes.Count -1 do
       WriteNote(FNotes[i]);


    if ( assigned( FPageCtrl ) and ( FPageCtrl.PageCount > 0 )) then begin
      // this is done so that we preserve the order of tabs.
       for i := 0 to FPageCtrl.PageCount - 1 do begin
          myFolder := TKntFolder(FPageCtrl.Pages[i].PrimaryObject);
          WriteFolder(myFolder);
       end;
    end
    else begin
      // Go by FFolders instead of using FPageCtrl. This may cause folders to be saved in wrong order.
      for i := 0 to FFolders.Count -1 do
         WriteFolder(FFolders[i]);

    end;

    SerializeBookmarks(tf);

    Log_StoreTick( 'After saving Folders', 1 );


    if SaveImages then begin
       ImageMng.DeleteOrphanImages;
       ImageMng.SaveState(tf);
       ImageMng.SaveEmbeddedImages(tf);
       Log_StoreTick( 'After saving state and embedded images', 1 );

       tf.WriteLine( _NF_EOF );
    end;

    result := 0;
  end;

begin
  result := -1; // error before saving file
  Stream := nil;
  SetVersion;
  FSavedWithRichEdit3 := ( _LoadedRichEditVersion = 3 );

  SavedFolders:= 0;
  SavedNodes:= 0;

  if ( FN = '' ) then
    raise EKeyKntFileError.Create( STR_12 );

  {
  if ( not assigned( FPageCtrl )) then
    raise EKeyKntFileError.Create( 'Error: PageCtrl not assigned.' );
  }


  result := 2; // error writing to file
  try
    try
      // FNoteCount := Notes.Count;
      if ActiveFolder <> nil then
         FSavedActiveFolderID := ActiveFolder.ID
      else
         FSavedActiveFolderID := 0;

      if Assigned(ActiveFolder) then
         ActiveFolder.EditorToDataStream;


      case FFileFormat of

        nffKeyNote : begin

          tf:= TTextFile.Create();
          tf.assignfile(FN);
          tf.rewrite();

          try
            WriteKntFile (true);
          finally
            tf.closefile();
          end;
        end; // nffKeyNote (text file format)


        nffKeyNoteZip : begin

          AuxStream := TMemoryStream.Create;
          Stream := TFileStream.Create( FN, (fmCreate or fmShareExclusive));
          try
            Stream.WriteBuffer(FVersion, sizeof(FVersion));
            Stream.WriteBuffer(FCompressionLevel, sizeof(FCompressionLevel));

            tf:= TTextFile.Create();
            try
              tf.assignstream( AuxStream );
              tf.rewrite;
              WriteKntFile (false);

              ImageMng.DeleteOrphanImages;
              ImageMng.SaveState(tf);
            finally
              tf.closefile();
            end;
            AuxStream.Position := 0;
            Log_StoreTick( 'After saving images state', 1 );
            ZCompressStream(AuxStream, Stream, FCompressionLevel);

          finally
            FreeAndNil(AuxStream);
            FreeAndNil(Stream);
            Log_StoreTick( 'After compress stream to disk', 1 );
          end;

          tf.assignfile(FN);
          tf.Append();
          try
             ImageMng.SaveEmbeddedImages(tf);
             Log_StoreTick( 'After saving embedded images', 1 );

             tf.WriteLine( _NF_EOF );
          finally
             tf.CloseFile ();
          end;

        end; // nffKeyNoteZip format


        nffEncrypted : begin

          if ( FPassphrase = '' ) then
            raise EKeyKntFileError.Create( STR_14 );

          AuxStream := TMemoryStream.Create;
          try
            tf:= TTextFile.Create();
            tf.assignstream( AuxStream );
            tf.rewrite;

            try
              WriteKntFile (true);
            finally
              tf.closefile();
            end;

            Log_StoreTick( 'After write file to stream', 1 );
            EncryptFileInStream( FN, AuxStream );
            Log_StoreTick( 'After encrypt stream to disk', 1 );

          finally
            AuxStream.Free;
          end;

        end; // nffEncrypted format

      end; // CASE

    except
      raise;
    end;
  finally
      if assigned(tf) then
         tf.Free;

      ImageMng.ConversionStorageMode_End;
  end;

end; // SAVE


procedure TKntFile.SaveTextToFile(var tf : TTextFile; DataStream : TMemoryStream; PlainText: Boolean; PlaintextLeader: AnsiString = _NF_PLAINTEXTLEADER);
var
  List : TStringList;
  cnt: integer;
  i, pos : integer;
  addCRLF: boolean;
  StreamAux : TMemoryStream;
  ImagesIDs: TImageIDs;

begin
  if DataStream.Size = 0 then
     exit;

  StreamAux:= nil;
  try

    if (not PlainText) and ImageMng.ExportingMode then begin
       var RTFwithProccesedImages: AnsiString;
       var ContainsImgIDsRemoved: boolean;
       var ContainsImages: boolean;
       var ImgMode: TImagesMode;


       if KeyOptions.ImgStorageModeOnExport = smeEmbRTF then
          ImgMode:= imImage
       else
          ImgMode:= imLink;

       RTFwithProccesedImages:= ImageMng.ProcessImagesInRTF(DataStream.Memory, DataStream.Size, '', ImgMode, '', 0, ContainsImgIDsRemoved, ContainsImages, true);
       if (RTFwithProccesedImages = '') and ContainsImages and (KeyOptions.ImgStorageModeOnExport <> smeEmbKNT) then
          RTFwithProccesedImages:= MemoryStreamToString (DataStream);

       if (RTFwithProccesedImages <> '') then begin
          if (KeyOptions.ImgStorageModeOnExport <> smeEmbKNT) then
             RTFwithProccesedImages:= RemoveKNTHiddenCharactersInRTF(RTFwithProccesedImages, hmOnlyImages);

          StreamAux := TMemoryStream.Create;
          StreamAux.Write(RTFwithProccesedImages[1], ByteLength(RTFwithProccesedImages));

          DataStream:= StreamAux;
       end;

       if ContainsImages and (KeyOptions.ImgStorageModeOnExport = smeEmbKNT) then begin
          ImagesIDs:= ImageMng.GetImagesIDInstancesFromRTF (DataStream);
          ImageMng.RegisterImagesReferencesExported (ImagesIDs);
       end;
    end;


    DataStream.Position := 0;

    if PlainText then
       tf.WriteLine(_NF_TxtContent)     // end of TNoteEntry header; Plain Text data follows
    else
       tf.WriteLine(_NF_RTFContent);    // end of TNoteEntry header; RTF data follows


    if PlainText then begin
        // Looking for: ;[<BOM>]first line...
        //               ;second line...

        i:= 0;
        addCRLF:= false;
        repeat
           tf.write( PlaintextLeader );
           pos:= PosEx(AnsiString(#13#10), PAnsiChar(DataStream.Memory), i+1);       // The index is 1-based.
           if (pos=0) or (pos > DataStream.Size) then begin
               pos:= DataStream.Size-1;
               if (i = pos) and (PByte(DataStream.Memory)[i] = 0) then break;
               addCRLF:= true;
           end;
           tf.write(PByte(DataStream.Memory)[i], pos-i+1);
           i:= pos + 1;
        until i >= DataStream.Size;

        if addCRLF then
           tf.write(#13#10);

    end
    else begin
       DataStream.Position := 0;
       i:= 0;
       // When compiled in D2006 with RxRichEdit 2.75 not ocurred, but now, when saving the stream in RTF an extra #0 is added. But I checked it
       // The existence of these final #0s really suits me well. See comment to TTabNote.CheckSavingImagesOnMode
       if PByte(DataStream.Memory)[DataStream.Size-1] = 0 then i:= 1;
       tf.F.CopyFrom(DataStream, DataStream.Size - i);
    end;

  finally
     if StreamAux <> nil then
        StreamAux.Free;
  end;
end;


procedure TKntFile.EncryptFileInStream( const FN : string; const CryptStream : TMemoryStream );
var
  Hash : TDCP_sha1;
  HashDigest : array[0..31] of byte;
  Encrypt : TDCP_blockcipher;
  savefile : file;
  Info : TEncryptedFileInfo;
  wordsize : integer;
  dataptr : pointer;
  streamsize : integer;
  F: TFileStream;
begin

  CryptStream.Position := 0;
  streamsize := CryptStream.Size;

  F:= TFileStream.Create( FN, ( fmCreate or fmShareExclusive ));


  with Info do begin
    Method := FCryptMethod;
    DataSize := streamsize;
    NoteCount := FFolders.Count;
  end;

  wordsize := sizeof( FVersion );
  F.WriteBuffer(wordSize, sizeof(wordsize));
  F.WriteBuffer(FVersion, sizeof(FVersion));

  wordsize := sizeof( Info );
  F.WriteBuffer(wordSize, sizeof(wordsize));
  F.WriteBuffer(Info, sizeof(Info));

  case FCryptMethod of
    tcmBlowfish : begin
      Encrypt := TDCP_Blowfish.Create( nil );
    end;
    else
      Encrypt := TDCP_Idea.Create( nil );
  end;

  try
    FillChar(HashDigest,Sizeof( HashDigest ), $FF );
    Hash:= TDCP_sha1.Create( nil );
    try
      Hash.Init;
      Hash.UpdateStr( FPassphrase );
      Hash.Final( HashDigest );
    finally
      Hash.Free;
    end;

    Encrypt.Init( HashDigest, Sizeof( HashDigest )*8, nil );
    Encrypt.EncryptCBC( HashDigest, HashDigest, Sizeof( HashDigest ));
    Encrypt.Reset;

    wordsize := sizeof( HashDigest );
    F.WriteBuffer(wordSize, sizeof(wordsize));
    F.WriteBuffer(HashDigest, sizeof(HashDigest));

    getmem( dataptr, streamsize );

    try
      Encrypt.EncryptCBC( cryptstream.memory^, dataptr^, streamsize );
      F.WriteBuffer(dataptr^, streamsize );

    finally
      freemem( dataptr, streamsize );
    end;

  finally
    Encrypt.Burn;
    Encrypt.Free;
    if assigned(F) then
       F.Free;
  end;

end;


procedure RaiseStreamReadError;
begin
  raise EKeyKntFileError.Create( STR_17 );
end;


procedure TKntFile.DecryptFileToStream( const FN : string; const CryptStream : TMemoryStream );
var
  Hash: TDCP_sha1;
  HashDigest, HashRead: array[0..31] of byte;
  Decrypt: TDCP_blockcipher;
  readfile: TFileStream;
  Info : TEncryptedFileInfo;
  chunksize, sizeread : integer; // MUST be 32-bit value, i.e. 4 bytes
  array32bits : array[0..3] of byte;
  dataptr : pointer;
begin
  readfile:= TFileStream.Create( FN, ( fmOpenRead ));

  try
    readfile.Read(array32bits, sizeof(array32bits));

    chunksize := integer( array32bits );
    sizeread:= readfile.Read(FVersion, chunksize);
    if ( sizeread <> chunksize ) then RaiseStreamReadError;

    readfile.Read(array32bits, sizeof(array32bits));
    chunksize := integer( array32bits );
    sizeread:= readfile.Read(Info, chunksize);
    if ( sizeread <> chunksize ) then RaiseStreamReadError;

    FCryptMethod := Info.Method;

    case FCryptMethod of
      tcmBlowfish : begin
        Decrypt := TDCP_Blowfish.Create( nil );
      end;
      else
        Decrypt := TDCP_Idea.Create( nil );
    end;

    try

      FillChar( HashDigest, Sizeof( HashDigest ), $FF );
      Hash:= TDCP_sha1.Create( nil );
      try
        Hash.Init;
        Hash.UpdateStr( FPassphrase );
        Hash.Final( HashDigest );
      finally
        Hash.Free;
      end;

      Decrypt.Init( HashDigest, Sizeof( HashDigest )*8, nil );
      Decrypt.EncryptCBC( HashDigest, HashDigest, Sizeof( HashDigest ));
      Decrypt.Reset;

      readfile.Read(array32bits, sizeof(array32bits));
      chunksize := integer( array32bits );
      sizeread:= readfile.Read(HashRead, chunksize);
      if ( sizeread <> chunksize ) then RaiseStreamReadError;

      if ( not CompareMem( @HashRead, @HashDigest, Sizeof( HashRead ))) then
        raise EPassphraseError.Create( STR_18 );

      getmem( dataptr, Info.DataSize );

      try
        sizeread:= readfile.Read(dataptr^, Info.DataSize);
        if ( sizeread <> Info.DataSize ) then RaiseStreamReadError;

        Decrypt.DecryptCBC( dataptr^, dataptr^, Info.DataSize );

        CryptStream.Position := 0;
        CryptStream.Write( dataptr^, Info.DataSize );
        CryptStream.Position := 0;

      finally
        freemem( dataptr, Info.DataSize );
      end;

    finally
      Decrypt.Burn;
      Decrypt.Free;
    end;

  finally
    readFile.Free;
  end;

end;


function TKntFile.PropertiesToFlagsString : TFlagsString;
begin
  result := DEFAULT_FLAGS_STRING;
  result[1] := BOOLEANSTR[FOpenAsReadOnly];
  result[2] := BOOLEANSTR[FShowTabIcons];
  result[3] := BOOLEANSTR[FSavedWithRichEdit3];
  result[4] := BOOLEANSTR[FNoMultiBackup];
end; // PropertiesToFlagsString


procedure TKntFile.FlagsStringToProperties( const FlagsStr : TFlagsString );
begin
  if ( length( FlagsStr ) < FLAGS_STRING_LENGTH ) then exit;
  FOpenAsReadOnly     := FlagsStr[1] = BOOLEANSTR[true];
  FShowTabIcons       := FlagsStr[2] = BOOLEANSTR[true];
  FSavedWithRichEdit3 := FlagsStr[3] = BOOLEANSTR[true];
  FNoMultiBackup      := FlagsStr[4] = BOOLEANSTR[true];
end;



{$ENDREGION}


end.

