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
   System.Character,
   Vcl.Graphics,
   Vcl.FileCtrl,
   Vcl.Controls,
   Vcl.Dialogs,
   Vcl.Forms,

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
   knt.ui.info,
   knt.ui.editor,
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
  THash = array[0..31] of byte;

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
    FKeyDerivIterations: Cardinal;
    FPassPhrase : UTF8String;
    FPassphraseFunc : TGetAccessPassphraseFunc;
    FCachedEncryptionKey: THash;
    FCachedVerificationHash: THash;
    FKeysAreCached: Boolean;
    FSavedActiveFolderID : Cardinal;

    FPageCtrl : TPage95Control;

    fNoteTags: TNoteTagList;
    fNoteTagsSorted: TNoteTagList;
    fNoteTagsTemporalAdded: TNoteTagList;
    fFolders: TFolderList;
    fNotes: TNoteList;
    fNextNNodeGID: Cardinal;     // Global ID of next note node to be created
    fNotesSorted: boolean;       // Normally notes are ordered by GID

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
    procedure EnsureKeysAreCached;

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
    property KeyDerivIterations: Cardinal read FKeyDerivIterations write FKeyDerivIterations;

    property Bookmarks[index: integer]: TLocation read GetBookmark write WriteBookmark;

    property IsBusy: boolean read FIsBusy write SetIsBusy;

    constructor Create;
    destructor Destroy; override;
    procedure ReleaseNoteUIs;


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
    procedure LoadNoteTags(var tf : TTextFile; var FileExhausted : boolean; var NextBlock: TNextBlock);
    procedure LoadVirtualNote (Note: TNote; const VirtFN, RelativeVirtFN: string);
    function  ConvertKNTLinksToNewFormatInNotes(FolderIDs: array of TMergeFolders; NoteGIDs: TMergedNotes; var GIDsNotConverted: integer): boolean;
    procedure CalculateOldPassphraseHash (Decrypt: TDCP_blockcipher; var EncryptionKey : THash);
    procedure EncryptFileInStream( const FN : string; const CryptStream : TMemoryStream );
    procedure DecryptFileToStream( const FN : string; const CryptStream : TMemoryStream );
    procedure InvalidateKeyCache;
    procedure ErasePassword;

  private
    function  PropertiesToFlagsString : TFlagsString; virtual;
    procedure FlagsStringToProperties( const FlagsStr : TFlagsString ); virtual;
    procedure SaveTextToFile(var tf : TTextFile; DataStream : TMemoryStream; PlainText: Boolean; PlaintextLeader: AnsiString = _NF_PLAINTEXTLEADER);


  public
    property TextPlainVariablesInitialized: boolean read FTextPlainVariablesInitialized write FTextPlainVariablesInitialized;
    procedure UpdateTextPlainVariables (nMax: integer);

    procedure UpdateImagesStorageModeInFile (ToMode: TImagesStorageMode; ApplyOnlyToFolder: TKntFolder= nil; ExitIfAllImagesInSameModeDest: boolean = true);
    function  TogglePlainText_RTF (NoteUI: INoteUI): boolean;
    procedure RemoveImagesCountReferences (myFolder: TKntFolder); overload;
    procedure RemoveImagesCountReferences (Note: TNote); overload;
    procedure UpdateImagesCountReferences (myFolder: TKntFolder); overload;
    procedure UpdateImagesCountReferences (Note: TNote); overload;


  private
    function InternalAddNTag( NTag : TNoteTag ) : integer;
    procedure VerifyNTagsIds;
    procedure GenerateNTagID( ANTag : TNoteTag );
    function GetNoteTagsSorted: TNoteTagList;
  public
    property NoteTags: TNoteTagList read fNoteTags;
    property NoteTagsSorted: TNoteTagList read GetNoteTagsSorted;
    property NoteTagsTemporalAdded: TNoteTagList read fNoteTagsTemporalAdded;
    function GetNTagByID( const aID : Cardinal ) : TNoteTag;
    function GetNTagByName( const aName : string ) : TNoteTag; overload;
    function GetNTagByName( const aName : string; const NTags: TNoteTagList) : TNoteTag; overload;
    function AddNTag(const Name, Description : string) : TNoteTag;
    procedure DeleteNTag( NTag : TNoteTag );
    procedure DeleteNTagsReferences( SelectedTags: TNoteTagArray; RemoveRefInNotesText: boolean);
    function GetNTagsCount : integer;
    function CheckNTagsSorted: boolean;
    procedure UpdateNTagsMatching(const Str : string; var NTags: TNoteTagList);
    //function ContainsNTagsMatching(const Str : string; const NTags: TNoteTagList): boolean;
    procedure SortNoteTags;


  public
    property Notes: TNoteList read fNotes;
    function  GetNoteByGID(const aGID: Cardinal): TNote;
    procedure GetNNodeByGID(const aGID : Cardinal; var NNode: TNoteNode; var Folder: TKntFolder);
    function  GetNNodeByFolderAndID (FolderID: Cardinal; NodeID: Word): TNoteNode;
    property NextNNoteGID: Cardinal read fNextNNodeGID;             // NoteGID:  identifies Note UNIQUELY in whole file
    procedure CheckNotesSorted;
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

    procedure TryToDeduceDates(RemoveDateFromName: boolean);

  end;

  procedure TransferedNEntryText(NEntry: TNoteEntry);
  procedure CalculatePassphraseHashes (Password: string; var EncryptionKey, VerificationHash : THash; IterationsOnVerif: Cardinal);

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
   gf_miscvcl,

   kn_RTFUtils,
   kn_Global,
   kn_Main,
   kn_EditorUtils,
   knt.ui.tree,
   kn_AlertMng,
   kn_BookmarksMng,
   kn_FindReplaceMng,
   knt.ui.tagSelector,
   knt.App,
   knt.RS
   ;


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
  FKeyDerivIterations := KEY_ITERATIONS_VERIF_DEFAULT;
  InvalidateKeyCache;
  FReadOnly := false;
  FOpenAsReadOnly := false;
  FTrayIconFN := ''; // use default
  FTabIconsFN := ''; // use default
  FPassphraseFunc := nil;
  FShowTabIcons := true;
  FNoMultiBackup := false;
  FSavedWithRichEdit3 := false;
  fNextNNodeGID:= 1;                // by default (for new files), next GID must be 1, not 0
  fNotesSorted:= true;
  SetVersion;

  fNoteTagsSorted:= nil;
  fNoteTagsTemporalAdded:= TNoteTagList.Create;
  fNoteTags:= TNoteTagList.Create;
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
  IsBusy:= True;

  if fNoteTags <> nil then begin
     for i := 0 to fNoteTags.Count-1 do
         fNoteTags[i].Free;
     fNoteTags.Free;
     fNoteTags:= nil;
  end;
  if fNoteTagsSorted <> nil then begin
     fNoteTagsSorted.Free;
     fNoteTagsSorted:= nil;
  end;


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


procedure TKntFile.ReleaseNoteUIs;
var
   i: integer;
begin
  for i := 0 to FFolders.Count-1 do
     FFolders[i].NoteUI:= nil;
end;


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
  if UpdatingTextPlain then Exit;

  Log_StoreTick('UpdateTextPlainVariables - BEGIN', 2, +1);

  UpdatingTextPlain:= True;
  RTFAux:= CreateAuxRichEdit;
  RTFAux.BeginUpdate;             // This instruction reduces the times obtained (See InitializeTextPlain_Compare_RTF, in kn_KntFolder)
  try
    repeat
      try
        AllNotesInitialized:= True;

        for i := 0 to FFolders.Count -1 do begin
           myFolder := FFolders[i];
           if not myFolder.InitializeTextPlainVariables(nMax, RTFAux) then begin
              AllNotesInitialized:= false;
              break;
           end;
        end;

        if AllNotesInitialized then
           FTextPlainVariablesInitialized:= true;

      except
      end;

      if (MillisecondsIdle < 450) then exit;

    until FTextPlainVariablesInitialized or FIsBusy;

  finally
    RTFAux.Free;
    UpdatingTextPlain:= False;
    Log_StoreTick('UpdateTextPlainVariables - END', 2, -1);
   {$IFDEF KNT_DEBUG}
     if FTextPlainVariablesInitialized then
        Log.Add( '* UpdateTextPlainVariables FINISHED' );
   {$ENDIF}


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
  N: TNote;
begin
  // *1  The vast majority of nodes will be the only node for a note, with which they share a GID

  N:= GetNoteByGID(aGID);     // *1
  if N <> nil then begin
     NNode:= N.NNodes[0].NNode;
     Folder:= TKntFolder(N.NNodes[0].Folder);
  end
  else begin
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
end;


function TKntFile.GetNoteByGID(const aGID: Cardinal): TNote;
var
   N: TNote;
   i, B,T: Cardinal;
   Dif: Int64;

   function NoteByGIDAscending: TNote;
   var
      j: Cardinal;
   begin
      for j := i to T do
         if Notes[j].GID = aGID then
            exit(Notes[j]);
      exit(nil);
   end;

   function NoteByGIDDescending: TNote;
   var
      j: Cardinal;
   begin
      for j := i downto B do
         if Notes[j].GID = aGID then
            exit(Notes[j]);
      exit(nil);
   end;

begin
  // GID=0 is not a valid value, and GIDs cannot be repeated

  if aGID >= 1 then begin
     T:= Notes.Count - 1;
     B:= 0;

     i:= aGID -1;
     if i > T then
        i:= T;
     T:= i;

     if not fNotesSorted then begin
        i:= 0;
        exit(NoteByGIDAscending());
     end;

     repeat
        N:= Notes[i];
        if N.GID = aGID then
           exit(N);

        Dif:= Int64(N.GID) - Int64(aGID);
        if Dif > 0 then begin
           dec(i);
           if Dif < 50 then
              exit(NoteByGIDDescending());
           T:= i;
        end
        else begin
           inc(i);
           if Dif > -50 then
              exit(NoteByGIDAscending());
           B:= i;
        end;

        if B >= T then
           break;

        i:= B + ((T-B) div 2);
     until false;
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


procedure TKntFile.CheckNotesSorted;
var
  i: integer;
  lastGID : Cardinal;
  N: TNote;
begin
  lastGID:= 0;
  for i := 0 to Notes.Count-1 do begin
     N:= Notes[i];
     if N.GID <= lastGID then begin
        fNotesSorted:= false;
        break;
     end;
     lastGID:= N.GID;
  end;
end;


function CompareNotes(Item1, Item2: Pointer): Integer;
var
  N1, N2: TNote;
begin
  N1:= TNote(Item1);
  N2:= TNote(Item2);

  Result:= 1;               //  N1.GID > N2.GID
  if N1.GID = N2.GID then
     Result:= 0
  else
  if N1.GID < N2.GID then
     Result:= -1;
end;



procedure TKntFile.RecalcNextNNodeGID;
var
  i, j: integer;
  hiGID : Cardinal;
  F : TKntFolder;
  NNode: TNoteNode;
  N: TNote;
begin
  hiGID:= 0;

  // NNode.GID and Note.GID take its value from fNextNNodeGID
  // Normally a note has one entry. The first NoteNode of a note shares the GID with the note

  for i := 0 to Notes.Count-1 do begin
     N:= Notes[i];
     if N.GID > hiGID then begin
        N.GID := fNextNNodeGID;
          hiGID := N.GID;
     end;
  end;

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


{
  * Deduce missing date information
    ---------------------------------
    KeyNote can search and register missing note dates: creation and last modified
    If one note already has creation date registered, it will be ignored (except if command executed with Ctrl)
   
    It will act on notes of active folder:
    
    + It will search in node name, according to defined 'Default Name for New nodes', if includes '%D' token
      It will try to identify the date using the format defined in configuration options (Advanced/Formats/Date format).
    
       Ex: If Default Name for New nodes = '(%D) - ' and Date format = 'dd MMM yy'
         "(05 oct. 16) - Node A"   => Creation date: 05/10/16


        * You should ensure the correct values for folder config ('Default Name for New nodes') and 
          Advanced/Formats (Date format).

       
        - Besides trying with the date format configured in Configuration Options, KeyNote will also automatically try
          with default regional short date (usually 'dd/mm/yy' or 'mm/dd/yy') and even some long formats, like:

            "domingo, 20 de octubre de 2022"
            "monday, february 1, 2021"
            "domingo, 20 de octubre de 2022 - 21:12"
      
         - It will also attempt to automatically handle short month names that include "." and without it.
           "(05 oct 16) ..." 
           "(05 oct. 16) ..."

       
    + It will search for dates also in the note's content, looking for lines that include only date (and optionally time).
      If several matches founded, the oldest date will be used as creation date and the most recent date as the last modification
      date
    
          Line 1, bla, bla, ... 07/09/16 ...
          ...
          05 oct. 16 - 20:15
          ------------------
          ...
          31/12/18 15:00

       => Creation date:     05/10/16 20:15
       => Last modification: 31/12/18 15:00
      
      - Note: Date found in note name will have priority as Creation date over dates found in note content.
      
      
  * Remove date prefixes from node names
    -----------------------------------
    Optionally, KeyNote can attempt to remove dates used as prefixes in note names.
    Eg.  "(05 oct. 16) - Node A"  (and '(%D) - ')  => New name: "Node A"

    This command will also previously identify (and register) any missing dates

      
  ** If there are nodes selected (1 or more) when you execute any of this commands, it will be applied only to those node. 
     Otherwise all the notes in the folder will be considered.

} 

procedure TKntFile.TryToDeduceDates (RemoveDateFromName: boolean);
var
  F: TKntFolder;
  N: TNote;
  NN: TNoteNode;
  i: integer;
  pI, pF: integer;
  CreationDate, LastModif, DateFound, DateFoundInName: TDateTime;
  str, msg: string;
  Ch: Char;
  NodesSelected, OnlyActiveFolder, ForceReconsidere: boolean;
  TextPlain: string;
  posCR, posLastCR: integer;
  MinLen, MaxLen: integer;
  RegionalSettings: TFormatSettings;
  DateFormatSts: array of TFormatSettings;
  RTFAux : TAuxRichEdit;
  UseLongFormatAdapted: boolean;
  LongDateSeparator: string;
  
  procedure PrepareDateFormatSettings;
  var
     DFSt: TFormatSettings;
     iDFs, pI, pF: integer;
     
     function RemoveDotInShortMonthNames: boolean;
     var
       i: Integer;
     begin
       if DFSt.ShortMonthNames[1][Length(DFSt.ShortMonthNames[1])-1] <> '.' then result:= false;
       
       for i := 1 to 12 do
         DFSt.ShortMonthNames[i] := Copy(DFSt.ShortMonthNames[i], 1, DFSt.ShortMonthNames[i].Length-1);
         
       Result:= true;
     end;

     function LongerDayName: integer;
     var
       i, L: Integer;
     begin
       Result:= 0;
       for i := 1 to 7 do begin
          L:= Length(DFSt.LongDayNames[i]);
          if L > Result then
             Result:= L;
       end;
     end;

     function LongerMonthName: integer;
     var
       i, L: Integer;
     begin
       Result:= 0;
       for i := 1 to 12 do begin
          L:= Length(DFSt.LongMonthNames[i]);
          if L > Result then
             Result:= L;
       end;
     end;
     
     
  begin
    
     DateFormatSts:= nil;
     SetLength(DateFormatSts, 4);
     iDFs:= 0;

     DFSt:= FormatSettings;
     DFSt.ShortTimeFormat := KeyOptions.TimeFmt;
     DFSt.ShortDateFormat := KeyOptions.DateFmt;      // It seems that LongDateFormat is not used for parse and it is ignored by StrToDateTime
     DateFormatSts[iDFs]:= DFSt;
     inc(iDFs);
    
     if RemoveDotInShortMonthNames then begin
        DateFormatSts[iDFs]:= DFSt;
        inc(iDFs);
     end;

     RegionalSettings:= FormatSettings;                      // Use default regional ShortDateFormat
     /// RegionalSettings:= TFormatSettings.Create('en-US');
     DateFormatSts[iDFs]:= RegionalSettings;
     inc(iDFs);

     
     // Try also with long regional format
     //   Eg: 'dddd, d'' de ''mmmm'' de ''yyyy'  --> 'd mmmm yyyy'
     
     UseLongFormatAdapted:= false;
     str:= RegionalSettings.LongDateFormat;
     str:= StringReplace(str, 'dddd,','', []);
     pI:= pos('mmmm', str);
     pF:= pos('yyyy', str);
     if (pI > 0) and (pF > 0) then begin
        if pI < pF then
           LongDateSeparator := Copy(str, pI+4, pF-pI-4)
        else
           LongDateSeparator := Copy(str, pF+4, pI-pF-4);

        if pos(',', LongDateSeparator) = 0 then begin
           str := StringReplace(str, LongDateSeparator,' ', [rfReplaceAll]);
           LongDateSeparator:= StringReplace(LongDateSeparator, '''','', [rfReplaceAll]);
        end
        else
            LongDateSeparator:= '';
            
        DFSt:= RegionalSettings;    
        DFSt.ShortDateFormat := Trim(str);
        DateFormatSts[iDFs]:= DFSt;
        inc(iDFs);
        UseLongFormatAdapted:= true;
     end;
     
     SetLength(DateFormatSts, iDFs);

     // Longer: like: 'dddd, dd de mmmm de yyyy - HH:mm'
     // Min: d/mm/yy  : 7
     MinLen:= 7;
     MaxLen:= Length(FormatSettings.LongDateFormat) + LongerMonthName + LongerDayName  // -8:dddd mmmm  +8: ' - HH:mm'
  end;
  
  
  function ParseDate(str: string): TDateTime;
  var
     DFSt: TFormatSettings; 
     i: integer;
     Inc: integer;
     Digits: boolean;
  begin
     Result:= 0;
     try
         str:= trim(str);
         if Length(str) < MinLen then exit;

         for i := 1 to Length(str) do
            if IsDigit(str[i]) then begin
               Digits:= true;
               break;
            end;

         if not Digits then 
            exit;

         Inc:= 0;   
         if UseLongFormatAdapted then
            Inc:= -1;
      
         for i:= 0 to High(DateFormatSts) + Inc do begin
             Result:= StrToDateTimeDef(str, 0, DateFormatSts[i]);
             if Result <> 0 then exit;
         end;

         if UseLongFormatAdapted then begin
            i:= Pos(',', str);
            if i > 0 then
               str:= Copy(str, i+1);
            str:= StringReplace(str, LongDateSeparator,' ', [rfReplaceAll]);
            Result:= StrToDateTimeDef(str, 0, DateFormatSts[High(DateFormatSts)]);
         end;

     except
     end;
  end;

  
begin
  if ActiveFolder = nil then exit;
  
  F:= ActiveFolder;
  
  ForceReconsidere:= CtrlDown;              // Ctrl  -> Force reconsider dates
  
  if RemoveDateFromName then
     msg:= GetRS(sFile22)
  else
     msg:= GetRS(sFile21);

  if App.DoMessageBox (msg + GetRS(sFile23) + GetRS(sFile24), mtWarning, [mbYes, mbNo, mbCancel]) <> mrYes then exit;


  PrepareDateFormatSettings;
  RTFAux:= CreateAuxRichEdit;
  
  try
            
     NodesSelected:= (F.TreeUI.TV.SelectedCount >=  1);
     pI:= Pos(NODEINSDATE, F.DefaultNoteName);                                              //  NODEINSDATE  = '%D'; // expands to current date
     if pI <= 0 then exit;
     
     Ch:= F.DefaultNoteName[pI+2];
     
     for i := 0 to F.NNodes.Count-1 do begin
        NN:= F.NNodes[i];
        N:= NN.Note;
        
        if NodesSelected and not F.TreeUI.TV.Selected[NN.TVNode] then
           continue;
        
        LastModif:= 0;

        if (N.DateCreated = 0) or RemoveDateFromName or ForceReconsidere then begin
           
           if (N.DateCreated = 0) or ForceReconsidere then begin     // Search in note content
              TextPlain:= F.PrepareTextPlain(NN, RTFAux);
              posLastCR:= 0;
              repeat
                 posCR:= Pos(#13, TextPlain, posLastCR+1);
                 if posCR > 0 then begin
                    if ((posCR - posLastCR) >= MinLen) and ((posCR - posLastCR) <= MaxLen)  then begin
                       str:= Trim(Copy(TextPlain, posLastCR+1, posCR - posLastCR));
                       DateFound:= ParseDate(str);
                       if DateFound <> 0 then begin
                          if DateFound > LastModif then
                             LastModif := DateFound;
                          if (DateFound < CreationDate) or (CreationDate = 0) then   
                             CreationDate := DateFound;
                       end;
                    end;
                    posLastCR:= posCR;
                 end;
              until posCR = 0;
           end;

           // Date found in note name has priority as Creation date :
           pF:= -1;
           if Ch <> ' ' then
              pF:= Pos(Ch, N.Name, pI);
           if pF <= 0 then
              pF:= Length(KeyOptions.DateFmt) + pI;    // Suppose KeyOptions.DateFmt used in note name
           str:= Copy(N.Name, pI, pF-pI);
           DateFoundInName:= ParseDate(str);
           if DateFoundInName <> 0 then
              CreationDate := DateFoundInName;
              
           
           if CreationDate <> 0 then begin
              if (N.DateCreated = 0) or ForceReconsidere then begin
                 N.Entries[0].Created:= CreationDate;
                 N.LastModified:= LastModif;
              end;
              if RemoveDateFromName and (DateFoundInName <> 0) then begin
                 LastModif:= N.LastModified;
                 N.Name:= Trim(Copy(N.Name, pF + (F.DefaultNoteName.Length-pI)));
                 N.LastModified:= LastModif;
              end;
           end;
        end;       
     end;

     F.NoteUI.ReloadMetadataFromDataModel(false);
     F.TreeUI.ShowAdditionalColumns(true);

     F.Modified:= True;
     
  finally
     RTFAux.Free;
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

   LoadVirtualNote(Note, CopyFromNote.VirtualFN, '');

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


// Note Tags  =========================================

{$REGION Note Tags }

function CompareNTagsByID(Item1, Item2: Pointer): Integer;
var
  T1, T2: TNoteTag;
begin
  T1:= TNoteTag(Item1);
  T2:= TNoteTag(Item2);

  Result:= 1;               //  N1.GID > N2.GID
  if T1.ID = T2.ID then
     Result:= 0
  else
  if T1.ID < T2.ID then
     Result:= -1;
end;

function CompareNTagsByName(Item1, Item2: Pointer): Integer;
begin
  Result:= AnsiCompareText( TNoteTag(Item1).Name, TNoteTag(Item2).Name)
end;


function CompareNTagsByNameAndDesc(Item1, Item2: Pointer): Integer;
var
  T1, T2: TNoteTag;
  T1s, T2s: boolean;
begin
  T1:= TNoteTag(Item1);
  T2:= TNoteTag(Item2);

  T1s:= AnsiStartsText(TagSubstr, T1.Name);
  T2s:= AnsiStartsText(TagSubstr, T2.Name);
  if T1s then begin
     if T2s then
        Result:= AnsiCompareText( T1.Name, T2.Name)
     else
        Result:= -1;
  end
  else
  if T2s then
     Result:= 1
  else
     Result:= AnsiCompareText( T1.Name, T2.Name );
end;


function TKntFile.GetNTagByID( const aID : Cardinal ) : TNoteTag;
var
  i, j, T : Cardinal;
  NTag: TNoteTag;
begin
   result := nil;
   if (aID = 0) or (NoteTags.Count = 0) then exit;

   T:= NoteTags.Count - 1;
   i:= aID -1;
   if i > T then
      i:= T;

   NTag:= NoteTags[i];
   if NTag.ID = aID then
       exit(NTag);

   if aID > NTag.ID then begin
      for j := i to T do
         if NoteTags[j].ID = aID then
            exit(NoteTags[j]);
   end
   else begin
      for j := i downto 0 do
         if NoteTags[j].ID = aID then
            exit(NoteTags[j]);
   end;

   exit(nil);
end;


function TKntFile.GetNTagByName( const aName : string ) : TNoteTag;
//  NOT case-sensitive
begin
  result:= GetNTagByName(aName, NoteTags);
end;


function TKntFile.GetNTagByName( const aName : string; const NTags: TNoteTagList) : TNoteTag;
//  NOT case-sensitive
var
  i: integer;
  NTag: TNoteTag;
begin
  result := nil;
  if (NTags.Count = 0) then exit;

  for i := 0 to NTags.Count-1 do begin
     NTag:= NTags[i];
     if ( AnsiCompareText( NTag.Name, aName ) = 0 ) then
        exit(NTag);
  end;

end;


procedure TKntFile.UpdateNTagsMatching(const Str : string; var NTags: TNoteTagList);
//  NOT case-sensitive
var
  i: integer;
  NTag: TNoteTag;
begin
   if NTags = nil then
      NTags:= TNoteTagList.Create;

   NTags.Clear;
   if Length(Str) < 3 then begin
      for i := 0 to NoteTags.Count-1 do begin
         NTag:= NoteTags[i];
         if AnsiStartsText(Str, NTag.Name) then
            NTags.Add(NTag);
      end;
   end
   else begin
      for i := 0 to NoteTags.Count-1 do begin
         NTag:= NoteTags[i];
         if AnsiContainsText(NTag.Name, Str) or AnsiContainsText(NTag.Description, Str) then
            NTags.Add(NTag);
      end;
   end;
   NTags.Sort(CompareNTagsByNameAndDesc);
end;

{ Not used by now
function TKntFile.ContainsNTagsMatching(const Str : string; const NTags: TNoteTagList): boolean;
//  NOT case-sensitive
var
  i: integer;
begin
   Result:= False;
   if (NTags <> nil) and (NTags.Count > 0) then begin
      for i := 0 to NTags.Count-1 do
         if AnsiStartsText(Str, NTags[i].Name) then
            exit(True);
   end;
end;
}


function TKntFile.InternalAddNTag( NTag : TNoteTag ) : integer;
begin
  result := NoteTags.Add( NTag );
  if fNoteTagsSorted <> nil then
     fNoteTagsSorted.Add(NTag);
end;


function TKntFile.AddNTag(const Name, Description : string) : TNoteTag;
var
   NTag: TNoteTag;
begin
  result := nil;
  if Name = '' then exit;

  if GetNTagByName(Name) = nil then begin
     NTag:= TNoteTag.Create;
     NTag.Name:= Name;
     NTag.Description:= Description;
     InternalAddNTag( NTag );
     GenerateNTagID( NTag );
     Modified := true;
     App.TagsUpdated;
     result:= NTag;
  end;
end;


procedure TKntFile.DeleteNTag( NTag : TNoteTag );
begin
  if NTag = nil then exit;

  fNoteTags.Remove(NTag);
  if fNoteTagsSorted <> nil then
     fNoteTagsSorted.Remove(NTag);
  NTag.Free;
  Modified := true;
  App.TagsUpdated;
end;


procedure TKntFile.DeleteNTagsReferences(SelectedTags : TNoteTagArray; RemoveRefInNotesText: boolean);
var
  i, j: integer;
  Note: TNote;
begin
  for i := 0 to Notes.Count-1 do begin
     Note:= Notes[i];
     for j := 0 to High(Note.Entries) do
         Note.Entries[j].RemoveTags(SelectedTags);
  end;

  if RemoveRefInNotesText then
     for i := 0 to High(SelectedTags) do
        ReplaceInNotes(SelectedTags[i].Name, '', True, False, False);
end;


procedure TKntFile.VerifyNTagsIds;
var
  i: integer;
  hID: Cardinal;
  NTag: TNoteTag;
begin
  hID:= 0;
  for i := 0 to NoteTags.Count-1 do begin
     NTag := NoteTags[i];
     if NTag.ID <= 0 then begin
        if hID > 0 then begin
           inc(hID);
           NTag.ID:= hID;
        end
        else begin
           GenerateNTagID(NTag);
           hID:= NTag.ID;
        end;
     end;
  end;
end;


procedure TKntFile.GenerateNTagID( ANTag : TNoteTag );
var
  i: integer;
  hiID : Cardinal;
  NTag : TNoteTag;
begin
  hiID := 0;

  for i := 0 to NoteTags.Count-1 do begin
     NTag := NoteTags[i];
     if NTag.ID > hiID then
        hiID := NTag.ID; // find highest ID
  end;

  inc( hiID ); // make it one higher
  ANTag.ID := hiID;
end;


function TKntFile.GetNTagsCount : integer;
begin
  if assigned( fNoteTags ) then
    result := fNoteTags.Count
  else
    result := 0;
end;

function TKntFile.CheckNTagsSorted: boolean;
var
  i: integer;
  lastID : Cardinal;
  T: TNoteTag;
begin
  Result:= True;
  lastID:= 0;
  for i := 0 to NoteTags.Count-1 do begin
     T:= NoteTags[i];
     if T.ID <= lastID then
        exit(False);
     lastID:= T.ID;
  end;
end;


function TKntFile.GetNoteTagsSorted: TNoteTagList;
var
   i: integer;
begin
  if fNoteTagsSorted = nil then begin
     fNoteTagsSorted:= TNoteTagList.Create;
     for i := 0 to NoteTags.Count-1 do
        fNoteTagsSorted.Add(NoteTags[i]);
  end;
  Result:= fNoteTagsSorted;
end;

procedure TKntFile.SortNoteTags;
begin
   NoteTagsSorted.Sort(CompareNTagsByName);
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
  ImagesIDs: TImageIDs;

   procedure UpdateImagesStorageMode (Stream: TMemoryStream);
   var
     ReplaceCorrectedIDs: boolean;
   begin
       if ToMode <> smEmbRTF then begin
          ImagesIDs:= myFolder.CheckSavingImagesOnMode (imLink, Stream, ExitIfAllImagesInSameModeDest);
          ImageMng.UpdateImagesCountReferences (nil, ImagesIDs);
//          if (SavedActiveFolderID = myFolder.ID) then
//             myFolder.ImagesReferenceCount:= ImagesIDs;
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

      myFolder.SaveEditorToDataModel;

      for j := 0 to myFolder.NNodes.Count - 1 do  begin
         NNode:= myFolder.NNodes[j];
         Note:= NNode.Note;
         if not Note.IsVirtual then begin
            if Note.NNodes[0].NNode <> NNode then continue;
            NEntry:= NNode.Note.Entries[0];                     //%%%
            if NEntry.IsPlainTXT or (NEntry.Stream.Size = 0) then continue;

            UpdateImagesStorageMode (NEntry.Stream);
            if Length(ImagesIDs) > 0 then
               NEntry.TextPlain:= '';      // Will have updated the Stream but not the editor, and been able to introduce/change image codes => force it to be recalculated when required

            if NNode = myFolder.FocusedNNode  then
               myFolder.ReloadEditorFromDataModel (false);
         end;
      end;

   end;

end;


function TKntFile.TogglePlainText_RTF (NoteUI: INoteUI): boolean;
var
  NNode: TNoteNode;
  NEntry: TNoteEntry;
  Stream: TMemoryStream;
  RTFAux : TRxRichEdit;
  ImagesIDs: TImageIDs;
  SourceFormat, TargetFormat: TRichStreamFormat;

begin
   assert(NoteUI<>nil);

   if not NoteUI.Editor.PlainText and (NoteUI.Editor.TextLength > 0) then
      if (App.DoMessageBox(GetRS(sFile18), mtWarning, [mbYes,mbNo,mbCancel], def3) <> mrYes) then exit;

   if NoteUI.Editor.PlainText then begin
      SourceFormat:= sfPlainText;
      TargetFormat:= sfRichText;
   end
   else begin
      SourceFormat:= sfRichText;
      TargetFormat:= sfPlainText;
   end;

   NoteUI.SaveToDataModel;
   NNode:= NoteUI.NNode;

   Result:= true;

   try
      RTFAux:= CreateAuxRichEdit;
      try
         NEntry:= NNode.Note.Entries[0];         // %%%%
         Stream:= nEntry.Stream;

         if Stream.Size <> 0 then begin
            if TargetFormat = sfPlainText then begin
               ImagesIDs:= ImageMng.GetImagesIDInstancesFromRTF (Stream);
               if Length(ImagesIDs) > 0 then
                  ImageMng.RemoveImagesReferences (ImagesIDs);
            end;
            Stream.Position:= 0;
            ConvertStreamContent(Stream, SourceFormat, TargetFormat, RTFAux, TKntFolder(NoteUI.GetFolder));
         end;

         if TargetFormat = sfPlainText then
            NoteUI.ResetImagesReferenceCount;

         NEntry.IsPlainTXT:= (TargetFormat = sfPlainText);
         NoteUI.ReloadFromDataModel;
         NNode.Note.Modified:= True;
         TKntFolder(NoteUI.GetFolder).Modified:= True;
         App.EditorSaved(NoteUI.Editor);    // To ensure synchronization on open linked nodes (if any)

         App.EditorFocused(NoteUI.Editor);  // To update UI

      finally
        RTFAux.Free;
      end;

   except on E: Exception do begin
     App.ErrorPopup( GetRS(sFile19) + E.Message, E);
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
   myFolder.NoteUI.ResetImagesReferenceCount;
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
     App.ErrorPopup(Format( GetRS(sFile01), [FN] ));
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
      App.ErrorPopup(Format( GetRS(sFile02), [FN] ));
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
              raise EKeyKntFileError.Create( GetRS(sFile03) );

            try
              DecryptFileToStream( FN, MemStream );
              break; // no error, so exit this loop
            except
              On e : EPassphraseError do begin
                HasLoadError := false;
                if ( App.DoMessageBox(GetRS(sFile04), mtError, [mbYes,mbNo]) <> mrYes ) then raise;
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
              delete( TestString, 1, p + ID_STR_LENGTH );
              if ( length( TestString ) > 2 ) then begin
                 VerID.Major := TestString[1];
                 VerId.Minor := TestString[3];

                 if (( VerID.Major in ['0'..'9'] ) and ( VerID.Minor in ['0'..'9'] )) then begin
                    if ( VerID.Major > NFILEVERSION_MAJOR ) then begin
                       App.ErrorPopup(Format( GetRS(sFile05), [ExtractFilename( FN ), NFILEVERSION_MAJOR, NFILEVERSION_MINOR, VerID.Major, VerID.Minor] ));
                       raise EKeyKntFileError.Create('');
                    end;

                    if (VerID.Major = NFILEVERSION_MAJOR) and ( VerID.Minor > NFILEVERSION_MINOR ) then begin
                       case App.DoMessageBox( ExtractFilename( FN ) + GetRS(sFile06), mtWarning, [mbYes,mbNo,mbCancel,mbHelp], def1, _HLP_KNTFILES ) of
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
            App.ErrorPopup(Format( GetRS(sFile07), [ExtractFilename( FN )] ));
            raise EKeyKntFileError.Create('');
          end;

          InHead := true;

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
                          FDateCreated := StrToDateTimeDef( ds, Now, LongDateToFileSettings);
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


              if ( ds = _NF_Tags ) then begin
                InHead := false;
                NextBlock:= nbTags;
                break;
              end;

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

               else if NextBlock = nbTags then
                   LoadNoteTags(tf, FileExhausted, NextBlock)

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
                       App.ErrorPopup(GetRS(sFile08) + Folder.Name + #13#13 + E.Message);
                       Folder.Free;
                       // raise;
                     end;
                   end;
               end;
            end; // EOF( tf )


          finally
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


procedure TKntFile.LoadVirtualNote (Note: TNote; const VirtFN, RelativeVirtFN: string);
var
   str: AnsiString;
begin
   try
     Note.SetVirtualFN(VirtFN, RelativeVirtFN, File_Path);
     Note.LoadVirtualFile;
   except
     on E : Exception do begin
       str:= GetRS(sFld39) + _CRLF + Note.VirtualFN + _CRLF + E.Message;
       Note.Entries[0].Stream.WriteBuffer(str[1], Length(str));
       Note.VirtualFN := _VIRTUAL_NODE_ERROR_CHAR + Note.VirtualFN;
       Note.Entries[0].IsRTF:= False;
     end;
   end;
end;


procedure TKntFile.LoadNotes(var tf : TTextFile; var FileExhausted : boolean; var NextBlock: TNextBlock);
var
  InNote, InNoteEntry, InEntryContent, RTFContent : boolean;
  s, key : AnsiString;
  p, linecount : integer;

  Note: TNote;
  NEntry: TNoteEntry;
  NEntryID, NoteSelEntryID: Word;

  VirtualFN, RelativeVirtualFN: string;


    procedure AddTextToNewNEntry (ClosingNote: boolean = true);
    var
      NSelEntry: TNoteEntry;
    begin
      InEntryContent := false;

      // *1 See reason described in the notes of the commit ca19e28bfe:
      //  (Fixed: Internally, the ID associated with each note entry is not being 0, as it should be.)
      if not ((Version.Major > '3') or ((Version.Major = '3') and (Version.Minor > '0'))) then
         NEntryID:= 0;           // *1

      Note.AddEntry(NEntry, NEntryID);

      if (VirtualFN <> '') or (RelativeVirtualFN <> '') then
         LoadVirtualNote (Note, VirtualFN, RelativeVirtualFN)

      else begin
         TransferedNEntryText(NEntry);
         assert((NEntry.IsRTF = RTFContent) or (NEntry.Stream.Size=0));
      end;

      if ClosingNote then begin
         NSelEntry:= Note.GetEntry(NoteSelEntryID);
         Note.SelEntry:= NSelEntry;
      end;

      VirtualFN:= '';
      RelativeVirtualFN:= '';
    end;

begin
  FileExhausted := false;
  InNote := false;
  InNoteEntry := false;
  InEntryContent := false;

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
     if ( s = _NF_NEntry) or (s = _NF_NEntry_Beta) then begin
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

       if assigned(NEntry) then begin
          if s <> '' then
             NEntry.Stream.WriteBuffer(s[1], Length(s));
          NEntry.Stream.WriteBuffer(_CRLF_BYTES, Length(_CRLF_BYTES));
       end;
       continue;
     end;


     p := pos('=', s);
     if ( p <> 3 ) then continue; // not a valid key=value format
     key := copy(s, 1, 2);
     delete(s, 1, 3);


     if InNoteEntry then begin
        if ( key = _NEntryID ) then
            NEntryID := StrToUIntDef(s, 0)
        else
        if ( key = _NEntryState ) then
           NEntry.StringToStates(s)
        else
        if ( key = _DateCreated ) then
           NEntry.Created:= StrToDate_Compact(s)
        else
        if ( key = _NEntryTags ) then
           NEntry.StringToTags(s)
     end;

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
            Note.LastModified:= StrToDate_Compact(s)
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


  end; { while not eof( tf ) }


  CheckNotesSorted;
  if not fNotesSorted then
     Notes.Sort(CompareNotes);

  FModified := false;

end;


procedure TKntFile.LoadNoteTags(var tf : TTextFile; var FileExhausted : boolean; var NextBlock: TNextBlock);
var
  s, key : AnsiString;
  p: Integer;
  NTag: TNoteTag;
begin
   NTag:= nil;

   while ( not tf.eof()) do begin
      s:= tf.readln();

      if Copy(s,1,2) = _NumNotes then begin
         var NumNotes: integer;
         NextBlock:= nbNotes;
         NumNotes:= StrToIntDef(Copy(s,4), 0);
         fNotes.Capacity:= NumNotes;
         break;
      end;
      if ( s = _NF_EOF ) then begin
        FileExhausted := true;
        break; // END OF FILE
      end;

      p := pos('=', s );
      if p <> 3 then continue;  // not a valid key=value format
      key := copy(s, 1, 2);
      delete(s, 1, 3);

      if ( key = _TagID ) then begin
          if (NTag <> nil) and (NTag.Name <> '') then
             Self.InternalAddNTag(NTag);
          NTag:= TNoteTag.Create;
          NTag.ID:= StrToUIntDef(s, 0);
      end
      else
      if ( key = _TagName ) then begin
         if NTag = nil then                        // It wouldn't be normal..
            NTag:= TNoteTag.Create;
         NTag.Name:= TryUTF8ToUnicodeString(s);
      end
      else
      if ( key = _TagDescription ) then begin
         if NTag = nil then                        // It wouldn't be normal..
            NTag:= TNoteTag.Create;
         NTag.Description:= TryUTF8ToUnicodeString(s);
      end
   end;

   if (NTag <> nil) and (NTag.Name <> '') then
      Self.InternalAddNTag(NTag);

   if not CheckNTagsSorted then
      NoteTags.Sort(CompareNTagsByID);
end;


procedure TransferedNEntryText(NEntry: TNoteEntry);
var
   RTF, NewRTF: string;
   IsRTF: boolean;
begin
    if (NEntry = nil) or (NEntry.Stream.Size = 0) then exit;

    IsRTF:= false;

    if App.opt_Clean then begin
       RTF:= MemoryStreamToString(NEntry.Stream);
       if CleanRTF(RTF, NewRTF) then begin
          NEntry.Stream.Clear;
          NEntry.Stream.LoadFromStream(TStringStream.Create(NewRTF));
       end;
    end;

    if NodeStreamIsRTF(NEntry.Stream) then begin
      IsRTF:= true;
      // In notes/nodes with RTF content we are interested in the buffer ending in #0 to be able to treat it as a string (accessing .Memory)
      assert((PByte(NEntry.Stream.Memory)[NEntry.Stream.Size-1] <> 0), 'The Stream already ends at 0');
      NEntry.Stream.WriteData(0);
    end;

    NEntry.IsRTF:= IsRTF;
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


function TKntFile.ConvertKNTLinksToNewFormatInNotes (FolderIDs: array of TMergeFolders; NoteGIDs: TMergedNotes; var GIDsNotConverted: integer): boolean;
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
        if NEntry.IsHTML or (NEntry.Stream.Size = 0) then continue;

        NewRTF:= ConvertKNTLinksToNewFormat(NEntry.Stream.Memory, NEntry.Stream.Size, NoteGIDs, FolderIDs, GIDsNotConverted);
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
  NotesToSave: TNoteList;


  procedure WriteNEntry (NEntry: TNoteEntry; Note: TNote);
  begin
     tf.WriteLine(_NF_NEntry);                              // TNoteEntry begins
     if NEntry.ID <> 0 then
        tf.WriteLine(_NEntryID + '=' + NEntry.ID.ToString );

     if NEntry.Created <> 0 then
       tf.WriteLine(_DateCreated + '=' + FormatDateTime(_COMPACT_DATETIME_TOFILE, NEntry.Created) );
     if (NEntry.States <> []) and (NEntry.States <> [nesModified]) then
       tf.WriteLine(_NEntryState + '=' + NEntry.StatesToString);

     if NEntry.Tags <> nil then
        tf.WriteLine(_NEntryTags + '=' + NEntry.TagsToString);

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
       tf.WriteLine(_LastModified + '=' + FormatDateTime(_COMPACT_DATETIME_TOFILE, Note.LastModified) );
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
              App.ErrorPopup(Format(GetRS(sFile20) + #13#13+ '%s', [Note.Name, Note.VirtualFN, E.Message]));
          end;
    end;

    for i := 0 to High(Note.Entries) do
       WriteNEntry(Note.Entries[i], Note);

  end;

  procedure WriteFolder (myFolder: TKntFolder);
  begin
      try
        if assigned( myFolder ) then begin
          if ExportingMode and not (myFolder.Info > 0) then     // Folders to be exported are marked with Info=1
             Exit;

          SavedNodes:= SavedNodes + myFolder.SaveToFile( tf, OnlyCurrentNodeAndSubtree, OnlyNotHiddenNodes, OnlyCheckedNodes);
          inc (SavedFolders);
        end;
      except
        on E : Exception do begin
            result := 3;
            App.ErrorPopup( Format(GetRS(sFile13), [myFolder.Name, E.Message]));
            exit;
        end;
      end;
  end;


  procedure WriteKntFile (SaveImages: boolean);
  var
     i: integer;
     ClipCapOnFolder: TKntFolder;
     NTag: TNoteTag;
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


    // Save Tags
    if NoteTags.Count > 0 then begin
       tf.WriteLine(_NF_Tags);
       for i := 0 to NoteTags.Count -1 do begin
          NTag:= NoteTags[i];
          tf.WriteLine(_TagID + '=' + NTag.ID.ToString);
          tf.WriteLine(_TagName + '=' + NTag.Name, True);
          if NTag.Description <> '' then
             tf.WriteLine(_TagDescription + '=' + NTag.Description, True);
       end;
    end;


    if ExportingMode then begin
       NotesToSave:= TNoteList.Create;
       for i := 0 to FFolders.Count -1 do
          if FFolders[i].Info > 0 then          // Folders to be exported are marked with Info=1
             FFolders[i].GetNotesToBeSaved(NotesToSave, OnlyCurrentNodeAndSubtree, OnlyNotHiddenNodes, OnlyCheckedNodes);

       // Save Notes (TNote) with its Entries (TNoteEntry)
       tf.WriteLine(_NumNotes + '=' + NotesToSave.Count.ToString);
       for i := 0 to NotesToSave.Count -1 do
          WriteNote(NotesToSave[i]);
    end
    else begin
       // Save Notes (TNote) with its Entries (TNoteEntry)
       tf.WriteLine(_NumNotes + '=' + FNotes.Count.ToString);
       for i := 0 to FNotes.Count -1 do
          WriteNote(FNotes[i]); 
    end;
   


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

    Log_StoreTick( 'After saving Folders and bookmarks', 1 );


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
  NotesToSave:= nil;


  if ( FN = '' ) then
    raise EKeyKntFileError.Create( GetRS(sFile12) );

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

      for i := 0 to FFolders.Count -1 do
         FFolders[i].SaveEditorToDataModel;



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
            raise EKeyKntFileError.Create( GetRS(sFile14) );

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
      if assigned(NotesToSave) then
         NotesToSave.Free;

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


function DeriveKey(const Password: string; const Salt: string; Iterations: Integer): THash;
var
  Hash : TDCP_sha1;
  i: Integer;
  TempStr: string;
begin
  Hash := TDCP_sha1.Create(nil);
  try
    FillChar(Result, Sizeof(Result), $FF);
    TempStr := Password + Salt;

    Hash.Init;                               // First iteration
    Hash.UpdateStr(TempStr);
    Hash.Final(Result);

    for i := 2 to Iterations do begin       // Additional iterations
      Hash.Init;
      Hash.Update(Result, SizeOf(Result));
      Hash.Final(Result);
    end;

  finally
    Hash.Free;
  end;
end;


procedure CalculatePassphraseHashes (Password: string; var EncryptionKey, VerificationHash : THash; IterationsOnVerif: Cardinal);
begin
  {
   The attacker tests passwords against the VerificationHash (stored on disk). Only when a match is found does the attacker derive the EncryptionKey
   (only once). The attacker then decrypts the file (only once).
   Therefore:
   The bottleneck of the attack is the VerificationHash. The EncryptionKey is only calculated once when the correct password is found.
   It doesn't make sense for both to have the same number of iterations.
   The user perceives the total time (both derivations). The attacker invests that same time for each attempt
   Legitimate user: pays the cost once upon opening/saving; Attacker: pays the cost thousands/millions of times
   The attacker cannot brute-force the EncryptionKey because: they have no way of knowing if they succeeded (they would have to decrypt
   the entire file), decrypting large files is expensive, they cannot easily validate whether the decrypted result is correct
   They must first crack the VerificationHash, which will have many iterations.
   The KEY_ITERATIONS_ENCRYP (1000) iterations on EncryptKey provide a margin of safety.
   }
  EncryptionKey:=    DeriveKey(Password, 'ENCRYPTION_KEY_SALT', KEY_ITERATIONS_ENCRYP);
  VerificationHash:= DeriveKey(Password, 'VERIFICATION_KEY_SALT', IterationsOnVerif);
end;


procedure TKntFile.InvalidateKeyCache;
begin
  FKeysAreCached := False;
  FillChar(FCachedEncryptionKey, SizeOf(FCachedEncryptionKey), 0);
  FillChar(FCachedVerificationHash, SizeOf(FCachedVerificationHash), 0);
end;

procedure TKntFile.EnsureKeysAreCached;
begin
  if not FKeysAreCached then begin
     CalculatePassphraseHashes (FPassPhrase, FCachedEncryptionKey, FCachedVerificationHash, FKeyDerivIterations);
     FKeysAreCached := True;
  end;
end;

procedure TKntFile.ErasePassword;
begin
  if FPassphrase = '' then exit;

  FillChar(FPassphrase[1], Length(FPassphrase) * SizeOf(Char), 0);
end;



procedure TKntFile.CalculateOldPassphraseHash (Decrypt: TDCP_blockcipher; var EncryptionKey : THash);
var
  Hash : TDCP_sha1;
begin
   FillChar(EncryptionKey,  Sizeof(EncryptionKey), $FF );

   Hash:= TDCP_sha1.Create( nil );
   try
     Hash.Init;
     Hash.UpdateStr(FPassphrase);
     Hash.Final( EncryptionKey );

   finally
     Hash.Free;
   end;

   Decrypt.Init( EncryptionKey, Sizeof( EncryptionKey )*8, nil );
   Decrypt.EncryptCBC( EncryptionKey, EncryptionKey, Sizeof(EncryptionKey));
   Decrypt.Reset;
end;


procedure TKntFile.EncryptFileInStream( const FN : string; const CryptStream : TMemoryStream );
var
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
    EnsureKeysAreCached;

    wordsize := sizeof( FCachedVerificationHash );
    F.WriteBuffer(wordSize, sizeof(wordsize));
    F.WriteBuffer(FCachedVerificationHash, sizeof(FCachedVerificationHash));

    wordsize := sizeof( FKeyDerivIterations );
    F.WriteBuffer(wordSize, sizeof(wordsize));
    F.WriteBuffer(FKeyDerivIterations, sizeof(FKeyDerivIterations));

    getmem( dataptr, streamsize );

    try
      Encrypt.Init( FCachedEncryptionKey, Sizeof( FCachedEncryptionKey )*8, nil );
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
  raise EKeyKntFileError.Create( GetRS(sFile15) );
end;


procedure TKntFile.DecryptFileToStream( const FN : string; const CryptStream : TMemoryStream );
var
  VerificationHash, HashRead : THash;
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

      readfile.Read(array32bits, sizeof(array32bits));
      chunksize := integer( array32bits );
      sizeread:= readfile.Read(HashRead, chunksize);
      if ( sizeread <> chunksize ) then RaiseStreamReadError;

      // Since file fomat version 3.2 -> Use simplified PBKDF2 mechanism to derive keys from passwords with a configurable number of iterations
      if (Version.Major > '3') or ((Version.Major = '3') and (Version.Minor >= '2')) then begin
         readfile.Read(array32bits, sizeof(array32bits));
         chunksize := integer( array32bits );
         sizeread:= readfile.Read(FKeyDerivIterations, chunksize);
         if ( sizeread <> chunksize ) then RaiseStreamReadError;

         EnsureKeysAreCached;
         VerificationHash:= FCachedVerificationHash;
         Decrypt.Init( FCachedEncryptionKey, Sizeof( FCachedEncryptionKey )*8, nil );              // *1(a)
      end
      else begin
        // Check if the hash works with the previous (weaker, see issue #545) mechanism
        // Once the file is saved, it will be using the new mechanism.
         CalculateOldPassphraseHash(Decrypt, VerificationHash);                       // *1(b)
      end;


      if ( not CompareMem( @HashRead, @VerificationHash, Sizeof( HashRead ))) then begin
           InvalidateKeyCache;
           raise EPassphraseError.Create( GetRS(sFile16) );
      end;

      getmem( dataptr, Info.DataSize );

      try
        sizeread:= readfile.Read(dataptr^, Info.DataSize);
        if ( sizeread <> Info.DataSize ) then RaiseStreamReadError;

        { *1:
         Decryption depends on the Hash value set in Decrypt.Init.
         If the file was encrypted with the new mechanism (2.15+), we will have set it in *1(a), using 'EncryptionKey'.
         If it uses the previous mechanism, it will already have been set in *1(b) using CalculateOldPassphraseHash,
           since it was necessary to encrypt the Hash itself, as that encrypted value was saved to disk.
         That is, the value saved to disk is not used (in either the new nor the previous mechanism)
         to encrypt or decrypt the file.
         }

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
  result[5] := BOOLEANSTR[(ImageMng.ImagesMode = imLink)];   // 5º: Hide images (Saves the (inverse) state of the button 'Show or Hide images')
end;


procedure TKntFile.FlagsStringToProperties( const FlagsStr : TFlagsString );
begin
  if ( length( FlagsStr ) < FLAGS_STRING_LENGTH ) then exit;
  FOpenAsReadOnly     := FlagsStr[1] = BOOLEANSTR[true];
  FShowTabIcons       := FlagsStr[2] = BOOLEANSTR[true];
  FSavedWithRichEdit3 := FlagsStr[3] = BOOLEANSTR[true];
  FNoMultiBackup      := FlagsStr[4] = BOOLEANSTR[true];
  if FlagsStr[5] = BOOLEANSTR[true] then
     ImageMng.ImagesMode := imLink;
end;



{$ENDREGION}


end.

