unit kn_FileObj;

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

   TreeNT,
   comctrls95,
   DCPcrypt,
   ZLibEx,

   kn_Const,
   kn_LocationObj,
   kn_NoteObj,
   kn_NodeList,
   kn_ImagesMng
   ;



type
  EKeyKntFileError = class( Exception );
  EPassphraseError = class( Exception );

type
  TGetAccessPassphraseFunc = function( const FN : string ) : string;

type

{
  TBookmark = record
    Name : string;
    CaretPos : integer;
    SelLength : integer;
    Folder : TKntFolder;
    Node : TKntNote;
  end;
  PBookmark = ^TBookmark;
}
  //TBookmarks = array[0..MAX_BOOKMARKS] of TBookmark;
  TBookmarks = array[0..MAX_BOOKMARKS] of TLocation;


type
  TKntFolderList = class( TList )
  private
    function GetNote( index : integer ) : TKntFolder;
    procedure PutNote( index : integer; item : TKntFolder );
  public
    property Items[index:integer] : TKntFolder read GetNote write PutNote; default;
    constructor Create;
    destructor Destroy; override;
    function Remove( item : TKntFolder ) : integer;
    procedure Delete( index : integer );
    function IndexOf( item : TKntFolder ) : integer;
  end;

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
    FActiveFolderID : integer;
    FNotes : TKntFolderList;
    FPageCtrl : TPage95Control;
    FModified : boolean;
    FReadOnly : boolean;
    FOpenAsReadOnly : boolean;
    FShowTabIcons : boolean;
    FNoMultiBackup : boolean;
    FClipCapFolder : TKntFolder;
    FTrayIconFN : string;
    FTabIconsFN : string;
    FSavedWithRichEdit3 : boolean;

    FCryptMethod : TCryptMethod;
    FPassPhrase : UTF8String;
    FPassphraseFunc : TGetAccessPassphraseFunc;

    FBookmarks : TBookmarks; // [?] bookmarks are NOT persistent
    FTextPlainVariablesInitialized: boolean;

    function GetModified : boolean;
    function GetCount : integer;
    procedure SetVersion;
    procedure SetDescription( ADescription : TCommentStr );
    procedure SetComment( AComment : TCommentStr );
    procedure SetFileFormat( AFileFormat : TKntFileFormat );
    procedure SetModified( AModified : boolean );
    function GetPassphrase( const FN : string ) : boolean;

    function InternalAddFolder( AFolder : TKntFolder ) : integer;
    procedure GenerateFolderID( const AFolder : TKntFolder );
    procedure VerifyFolderIds;

    function PropertiesToFlagsString : TFlagsString; virtual;
    procedure FlagsStringToProperties( const FlagsStr : TFlagsString ); virtual;
    procedure SetFilename( const Value : string );
    //function GetBookmark(Index: integer): PBookmark;
    function GetBookmark(Index: integer): TLocation;
    procedure WriteBookmark (Index: integer; Value: TLocation);
    function GetFile_Name: string;
    function GetFile_NameNoExt: string;
    function GetFile_Path: string;

  public
    property Version : TKntFileVersion read FVersion;
    property FileName : string read FFileName write SetFileName;
    property File_Name : string read GetFile_Name;
    property File_NameNoExt : string read GetFile_NameNoExt;
    property File_Path : string read GetFile_Path;

    property Comment : TCommentStr read FComment write SetComment;
    property Description : TCommentStr read FDescription write SetDescription;
    property NoteCount : integer read GetCount;
    property DateCreated : TDateTime read FDateCreated;
    property ActiveFolderID : integer read FActiveFolderID write FActiveFolderID;
    property Folders : TKntFolderList read FNotes write FNotes;
    property PageCtrl : TPage95Control read FPageCtrl write FPageCtrl;
    property Modified : boolean read GetModified write SetModified;
    property FileFormat : TKntFileFormat read FFileFormat write SetFileFormat;
    property CompressionLevel: TZCompressionLevel read FCompressionLevel write FCompressionLevel;
    property TrayIconFN : string read FTrayIconFN write FTrayIconFN;
    property TabIconsFN : string read FTabIconsFN write FTabIconsFN;
    property ReadOnly : boolean read FReadOnly write FReadOnly;
    property SavedWithRichEdit3 : boolean read FSavedWithRichEdit3;

    property OpenAsReadOnly : boolean read FOpenAsReadOnly write FOpenAsReadOnly;
    property ShowTabIcons : boolean read FShowTabIcons write FShowTabIcons;
    property NoMultiBackup : boolean read FNoMultiBackup write FNoMultiBackup;
    property ClipCapFolder : TKntFolder read FClipCapFolder write FClipCapFolder;

    property CryptMethod : TCryptMethod read FCryptMethod write FCryptMethod;
    property Passphrase : UTF8String read FPassphrase write FPassphrase;
    property PassphraseFunc : TGetAccessPassphraseFunc read FPassphraseFunc write FPassphraseFunc;

    //property Bookmarks[index: integer]: PBookmark read GetBookmark; // write FBookmarks;
    property Bookmarks[index: integer]: TLocation read GetBookmark write WriteBookmark;

    property TextPlainVariablesInitialized: boolean read FTextPlainVariablesInitialized write FTextPlainVariablesInitialized;

    constructor Create;
    destructor Destroy; override;

    function AddFolder( AFolder : TKntFolder ) : integer;
    procedure DeleteFolder( AFolder : TKntFolder );

    function Save(FN: string;
                  var SavedFolders: integer; var SavedNodes: integer;
                  ExportingMode: boolean= false; OnlyCurrentNodeAndSubtree: TTreeNTNode= nil;
                  OnlyNotHiddenNodes: boolean= false; OnlyCheckedNodes: boolean= false): integer;
    function Load( FN : string; ImgManager: TImageManager ) : integer;

    procedure EncryptFileInStream( const FN : string; const CryptStream : TMemoryStream );
    procedure DecryptFileToStream( const FN : string; const CryptStream : TMemoryStream );

    //function HasExtendedNotes : boolean; // TRUE is file contains any notes whose FKind is not ntRTF
    function HasVirtualNodes : boolean; // TRUE is file contains any notes which have VIRTUAL NODES
    function HasVirtualNodeByFileName( const aNoteNode : TKntNote; const FN : string ) : boolean;

    function GetFolderByID( const aID : integer ) : TKntFolder; // identifies folder UNIQUELY
    function GetFolderByName( const aName : string ) : TKntFolder; // will return the first folder whose name matches aName. If more notes have the same name, function will only return the first one.
    function GetFolderByTreeNode( const myTreeNode: TTreeNTNode ) : TKntFolder;  // return the folder that contains the tree with the passed node

    procedure SetupMirrorNodes (Folder : TKntFolder);
    procedure ManageMirrorNodes(Action: integer; node: TTreeNTNode; targetNode: TTreeNTNode);

    procedure UpdateTextPlainVariables (nMax: integer);
    procedure UpdateImagesStorageModeInFile (ToMode: TImagesStorageMode; ApplyOnlyToFolder: TKntFolder= nil; ExitIfAllImagesInSameModeDest: boolean = true);
    function  EnsurePlainTextAndRemoveImages (myFolder: TKntFolder): boolean;
    procedure RemoveImagesCountReferences (myFolder: TKntFolder); overload;
    procedure RemoveImagesCountReferences (myNote: TKntNote); overload;
    procedure UpdateImagesCountReferences (myFolder: TKntFolder); overload;
    procedure UpdateImagesCountReferences (myNote: TKntNote); overload;

  end;


implementation
uses
   RxRichEd,
   Blowfish,
   SHA1,
   IDEA,

   gf_streams,
   gf_strings,
   gf_files,

   kn_TreeNoteMng,
   kn_Global,
   kn_Main,
   kn_EditorUtils,
   kn_BookmarksMng
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
  STR_09 = '%s: Invalid DartNotes file header: ';
  STR_10 = 'This file contains notes which are not compatible with %s format. Only %s notes can be saved in this format.';
  STR_12 = 'Error: Filename not specified.';
  STR_13 = 'Error while saving folder "%s": %s';
  STR_14 = 'Cannot save: Passphrase not set';
  STR_17 = 'Stream size error: Encrypted file is invalid or corrupt.';
  STR_18 = 'Invalid passphrase: Cannot open encrypted file.';
  STR_19 = 'Exception trying to ensure plain text and removing of images: ';

constructor TKntFolderList.Create;
begin
  inherited Create;
end; // TNoteList.CREATE

destructor TKntFolderList.Destroy;
var
  i : integer;
begin
  if ( Count > 0 ) then
     for i := 0 to pred( Count ) do begin
        if assigned( Items[i] ) then
           Items[i].Free;  // Items[i] := nil;
     end;
  Clear;
  inherited Destroy;
end; // TNoteList DESTROY

function TKntFolderList.GetNote( index : integer ) : TKntFolder;
begin
  result := TKntFolder( inherited Items[index] );
end; // GetNote

procedure TKntFolderList.PutNote( index : integer; item : TKntFolder );
begin
  inherited Put( index, item );
end; // PutNote

function TKntFolderList.Remove( item : TKntFolder ) : integer;
begin
  if assigned( item ) then Item.Free;
  result := inherited remove( item );
end; // Remove

procedure TKntFolderList.Delete( index : integer );
begin
  if (( index >= 0 ) and ( index < Count ) and assigned( items[index] )) then
    Items[index].Free;
  inherited Delete( index );
end; // Delete

function TKntFolderList.IndexOf( item : TKntFolder ) : integer;
begin
  result := inherited IndexOf( item );
end; // IndexOf


// ************************************************** //
// NOTE FILE METHODS
// ************************************************** //

constructor TKntFile.Create;
var
  i: integer;
begin
  inherited Create;
  FFileName := '';
  FDescription := '';
  FComment := '';
  FDateCreated := now;
  FActiveFolderID := -1;
  FNotes := TKntFolderList.Create;
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
  FClipCapFolder := nil;
  FSavedWithRichEdit3 := false;
  SetVersion;

  for i:= 0 to MAX_BOOKMARKS do
     FBookmarks[i]:= nil;
  
end; // CREATE


destructor TKntFile.Destroy;
begin
  FFileName:= '<DESTROYING>';       // This way I'll know file is closing
  if assigned( FNotes ) then FNotes.Free;
  FNotes := nil;
  inherited Destroy;
end; // DESTROY


function TKntFile.GetPassphrase( const FN : string ) : boolean;
begin
  result := false;
  if ( not assigned( FPassphraseFunc )) then exit;
  FPassphrase := FPassphraseFunc( FN );
  result := ( FPassphrase <> '' );
end; // GetPassphrase


function TKntFile.AddFolder( AFolder : TKntFolder ) : integer;
begin
  result := -1;
  if ( not assigned( AFolder )) then exit;
  result := InternalAddFolder( AFolder );
  if ( AFolder.ID = 0 ) then
    GenerateFolderID( AFolder );
  Modified := true;
end; // AddFolder


function TKntFile.InternalAddFolder( AFolder : TKntFolder ) : integer;
begin
  result := Folders.Add( AFolder );
  AFolder.Modified := false;
end; // InternalAddFolder


procedure TKntFile.VerifyFolderIds;
var
  i, count : longint;
  myFolder : TKntFolder;
begin
  count := FNotes.Count;
  for i := 1 to count do begin
     myFolder := FNotes[pred( i )];
     if ( myFolder.ID <= 0 ) then
        GenerateFolderID( myFolder );
  end;
end; // VerifyFolderIds


procedure TKntFile.GenerateFolderID( const AFolder : TKntFolder );
var
  i, count, myID, hiID : longint;
  myFolder : TKntFolder;
begin
  myID := 0;
  hiID := 0;

  count := FNotes.Count;
  for i := 1 to count do begin
     myFolder := FNotes[pred( i )];
     if ( myFolder.ID > hiID ) then
        hiID := myFolder.ID; // find highest folder ID
  end;

  inc( hiID ); // make it one higher
  AFolder.ID := hiID;

end; // GenerateNoteID


procedure TKntFile.DeleteFolder( AFolder : TKntFolder );
var
  idx : integer;
begin
  if ( not assigned( AFolder )) then exit;
  idx := FNotes.IndexOf( AFolder );
  if ( idx < 0 ) then exit;
  FNotes.Delete( idx );
  Modified := true;
end; // DeleteKntFolder


function TKntFile.GetModified : boolean;
var
  i : integer;
begin
  if FModified then begin
    result := true;
    exit;
  end;
  if ( assigned( FNotes ) and ( FNotes.Count > 0 )) then begin
    for i := 0 to pred( FNotes.Count ) do
       if FNotes[i].Modified then begin
          FModified := true;
          break;
       end;
  end;
  result := FModified;
end; // GetModified


function TKntFile.GetCount : integer;
begin
  if assigned( FNotes ) then
    result := FNotes.Count
  else
    result := 0;
end; // GetCount


procedure TKntFile.SetVersion;
begin

  case FFileFormat of

    nffKeyNote : begin
      with FVersion do begin
        ID := NFHDR_ID; // GFKNT
        Major := NFILEVERSION_MAJOR;
        Minor := NFILEVERSION_MINOR;

        {
        if ( HasExtendedNotes ) then begin
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
        if HasExtendedNotes then
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
        if HasExtendedNotes then
           Major := NFILEVERSION_MAJOR
        else
           Major := NFILEVERSION_MAJOR_NOTREE;
        }
        Minor := NFILEVERSION_MINOR;
      end;
    end;

{$IFDEF WITH_DART}
    nffDartNotes : begin
      with FVersion do begin
        ID := NFHDR_ID;
        Major := NFILEVERSION_MAJOR;
        Minor := NFILEVERSION_MINOR;
      end;
    end;
{$ENDIF}

  end;
end; // SetVersion


procedure TKntFile.SetDescription( ADescription : TCommentStr );
begin
  ADescription := trim( ADescription );
  if ( FDescription = ADescription ) then exit;
  FDescription := ADescription;
  Modified := true;
end; // SetDescription


procedure TKntFile.SetComment( AComment : TCommentStr );
begin
  AComment := trim( AComment );
  if ( FComment = AComment ) then exit;
  FComment := AComment;
  Modified := true;
end; // SetComment


procedure TKntFile.SetFileFormat( AFileFormat : TKntFileFormat );
begin
  if ( FFileFormat = AFileFormat ) then exit;
  FFileFormat := AFileFormat;
end; // SetFileFormat


procedure TKntFile.SetModified( AModified : boolean );
var
  i : integer;
begin
  if FModified = AModified then exit;

  FModified := AModified;
  Form_Main.TB_FileSave.Enabled := FModified;

  if (( not FModified ) and ( FNotes.Count > 0 )) then
  begin
    for i := 0 to pred( FNotes.Count ) do
      FNotes[i].Modified := false;
  end;
end; // SetModified



function TKntFile.Load( FN : string; ImgManager: TImageManager ) : integer;
var
  Folder : TKntFolder;
  Attrs : TFileAttributes;
  Stream : TFileStream;
  MemStream : TMemoryStream;
  ds, ds1 : AnsiString;
  ch : AnsiChar;
  p, ClipCapIdx : integer;
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


{$IFDEF WITH_DART}
  Hdr : TDartNotesHdr;
{$ENDIF}
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
     DoMessageBox(Format( STR_01, [FN] ), mtError, [mbOK], 0);
     raise Exception.Create('');
  end;

  _VNKeyKntFileName := FN;
  {$I-}
  ChDir( extractfilepath( _VNKeyKntFileName )); // virtual node relative paths depend on it
  {$I+}

  ClipCapIdx := -1;

  // check if file is read-only; if so, set FReadOnly flag
  Attrs := TFile.GetAttributes(FN);
  if (TFileAttribute.faReadOnly in Attrs) then
    FReadOnly := true;

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
{$IFDEF WITH_DART}
    else
    if ( pos( _DART_STOP + _DART_ID + _DART_STOP, TestString ) > 0 ) then begin
      FFileFormat := nffDartNotes;
      _IS_OLD_KEYNOTE_FILE_FORMAT := false;
      VerID.ID := _DART_ID;
    end
{$ENDIF}
    else begin
      DoMessageBox(Format( STR_02, [FN] ), mtError, [mbOK], 0);
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
                       DoMessageBox(Format( STR_05, [ExtractFilename( FN ), NFILEVERSION_MAJOR, NFILEVERSION_MINOR, VerID.Major, VerID.Minor] ), mtError, [mbOK], 0);
                       raise EKeyKntFileError.Create('');
                    end;

                    if ( VerID.Minor > NFILEVERSION_MINOR ) then begin
                       case DoMessageBox( ExtractFilename( FN ) + STR_06, mtWarning, [mbYes,mbNo,mbCancel,mbHelp], _HLP_KNTFILES ) of
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
              end;
          end
          else
            FileIDTestFailed := false;

          if FileIDTestFailed then begin
            DoMessageBox(Format( STR_07, [ExtractFilename( FN )] ), mtError, [mbOK], 0);
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
                          try
                            FActiveFolderID := strtoint( ds );
                          except
                            FActiveFolderID := 0;
                          end;
                        end;
                        _NF_ClipCapFolder : begin
                          try
                            ClipCapIdx := strtoint( ds );
                          except
                            ClipCapIdx := -1;
                          end;
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

              // '%' markers, start a new entry
              if ( ds = _NF_TabNote ) then begin
                InHead := false;
                NextBlock:= nbRTF;
                break;
              end;

              if ( ds = _NF_TreeNote ) then begin
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

               else begin
                   case NextBlock of
                     nbRTF, nbTree  : Folder := TKntFolder.Create;
                   end;

                   try
                     Folder.LoadFromFile( tf, FileExhausted, NextBlock, (NextBlock = nbRTF));
                     InternalAddFolder( Folder );
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

            FClipCapFolder := nil;
            if (( ClipCapIdx >= 0 ) and ( ClipCapIdx < FNotes.Count )) then
               for p := 0 to pred( FNotes.Count ) do begin
                  Folder := FNotes[p];
                  if (( Folder.TabIndex = ClipCapIdx ) and ( not Folder.ReadOnly )) then begin
                    //if ( Folder.Kind = ntRTF ) then
                    FClipCapFolder := Folder;
                    break;
                  end;
               end;

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


{$IFDEF WITH_DART}
        nffDartNotes : begin

            Stream := TFileStream.Create( FN, ( fmOpenRead or fmShareDenyWrite ));
            ds := '';
            repeat
              Stream.ReadBuffer( ch, sizeof( ch ));
              if ( ch = _DART_STOP ) then break;
              ds := ds + ch;
            until ( length( ds ) > 16 ); // means it's not DartNotes file anyway

            if ( ch = _DART_STOP ) then begin
              try
                 Hdr.BlockLen := strtoint( ds );
                 ds := '';
                 SetLength( ds, Hdr.BlockLen );
                 Stream.ReadBuffer( ds[1], Hdr.BlockLen );
                 if ( pos( _DART_ID, ds ) = 1 ) then begin // success
                    Hdr.ID := _DART_ID;
                    delete( ds, 1, succ( length( _DART_ID )));
                    p := pos( _DART_STOP, ds );
                    if ( p > 0 ) then begin
                        Hdr.Ver := strtoint( copy( ds, 1, pred( p )));
                        if ( ds[length( ds )] = _DART_STOP ) then begin
                          // now go backwards from the end,
                          // since we don't care about the info in the middle
                          ds1 := '';
                          p := pred( length( ds ));
                          repeat
                            ch := ds[p];
                            if ( ch = _DART_STOP ) then break;
                            ds1 := ch + ds1;
                            dec( p );
                          until ( p = 0 );
                          Hdr.LastTabIdx := strtoint( ds1 );
                          FileIDTestFailed := false; // FINALLY VERIFIED
                        end;
                    end;
                 end;

              except
                FileIDTestFailed := true;
              end;
            end;

            if FileIDTestFailed then begin
              DoMessageBox(Format( STR_09 + VerID.ID, [ExtractFilename( FN )] ), mtError, [mbOK], 0);
              raise Exception.Create('');
            end;

            // initialize some stuff we got from the file already,
            // and some stuff that is not present in Dart file header
            FDescription := '';
            FComment := '';
            // FNoteCount := 0; // we don't know yet
            FDateCreated := now; // UNKNOWN!
            FActiveNote := Hdr.LastTabIdx;
            NoteKind := ntRTF;

            while ( Stream.Position < Stream.Size ) do begin
              Folder := TKntFolder.Create;
              try
                Folder.LoadDartNotesFormat( Stream );
                InternalAddNote( Folder );
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
        end; // nffDartNotes

{$ENDIF}

      end;

    except
      raise;
    end;

  finally
     if assigned( Stream ) then Stream.Free;
     // FNoteCount := Notes.Count;
     Modified := false;
     VerifyFolderIds;
     ImgManager.FileIsNew:= false;
  end;

  if HasLoadError then
     result := 99
  else
     result := 0;

end; // Load


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
                        ExportingMode: boolean= false; OnlyCurrentNodeAndSubtree: TTreeNTNode= nil;
                        OnlyNotHiddenNodes: boolean= false; OnlyCheckedNodes: boolean= false): integer;
var
  i : integer;
  Stream : TFileStream;
  myFolder : TKntFolder;
  ds : AnsiString;
  tf : TTextFile;
  AuxStream : TMemoryStream;

  procedure WriteNote (myFolder: TKntFolder);
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
            DoMessageBox( Format(STR_13, [myFolder.Name, E.Message]), mtError, [mbOK], 0 );
            exit;
        end;
      end;
  end;

  procedure WriteKntFile (SaveImages: boolean);
  var
     i: integer;
  begin
    //writeln(tf, _NF_COMMENT, _NF_AID, FVersion.ID, #32, FVersion.Major + '.' + FVersion.Minor );
    if FFileFormat = nffKeyNote then begin
       tf.WriteLine( _NF_COMMENT + _NF_AID + FVersion.ID + ' ' + FVersion.Major + '.' + FVersion.Minor);
       tf.WriteLine(_NF_WARNING);
    end;
    tf.WriteLine(_NF_COMMENT + _NF_FDE + FDescription, True );
    tf.WriteLine(_NF_COMMENT + _NF_FCO + FComment, True );

    tf.WriteLine(_NF_COMMENT + _NF_ACT + FActiveFolderID.ToString );

    tf.WriteLine(_NF_COMMENT + _NF_DCR + FormatDateTime( _SHORTDATEFMT + ' ' + _LONGTIMEFMT, FDateCreated ) );
    tf.WriteLine(_NF_COMMENT + _NF_FileFlags + PropertiesToFlagsString );
    // WriteLine( tf, _NF_COMMENT, _NF_ReadOnlyOpen, BOOLEANSTR[FOpenAsReadOnly] );
    // WriteLine( tf, _NF_COMMENT, _NF_ShowTabIcons, BOOLEANSTR[FShowTabIcons] );
    if ( TrayIconFN <> '' ) then
      tf.WriteLine( _NF_COMMENT + _NF_TrayIconFile + TrayIconFN );
    if ( FTabIconsFN <> '' ) then
      tf.WriteLine( _NF_COMMENT + _NF_TabIconsFile + FTabIconsFN );
    if assigned( FClipCapFolder ) then
      tf.WriteLine( _NF_COMMENT + _NF_ClipCapFolder + FClipCapFolder.TabSheet.PageIndex.ToString );

    if ( assigned( FPageCtrl ) and ( FPageCtrl.PageCount > 0 )) then begin
      // this is done so that we preserve the order of tabs.
       for i := 0 to pred( FPageCtrl.PageCount ) do begin
          myFolder := TKntFolder(FPageCtrl.Pages[i].PrimaryObject);
          WriteNote(myFolder);
       end;
    end
    else begin
      // Go by FNotes instead of using FPageCtrl.
      // This may cause notes to be saved in wrong order.
      for i := 0 to pred( FNotes.Count ) do begin
         myFolder := FNotes[i];
         WriteNote(myFolder);
      end;
    end;

    SerializeBookmarks(tf);

    Log_StoreTick( 'After saving Folders', 1 );


    if SaveImages then begin
       ImagesManager.DeleteOrphanImages;
       ImagesManager.SaveState(tf);
       ImagesManager.SaveEmbeddedImages(tf);
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

{$IFDEF WITH_DART}
  if ((FFileFormat in [nffDartNotes]) and HasExtendedNotes ) then
    raise EKeyKntFileError.CreateFmt( STR_10, [FILE_FORMAT_NAMES[FFileFormat], TABNOTE_KIND_NAMES[ntRTF]] );
{$ENDIF}

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
      if (( Folders.Count > 0 ) and assigned( FPageCtrl ) and assigned( FPageCtrl.ActivePage )) then
        FActiveFolderID := FPageCtrl.ActivePage.PageIndex
      else
        FActiveFolderID := 0;

      if Assigned(kn_global.ActiveKntFolder) then
         kn_global.ActiveKntFolder.EditorToDataStream;


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

              ImagesManager.DeleteOrphanImages;
              ImagesManager.SaveState(tf);
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
             ImagesManager.SaveEmbeddedImages(tf);
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

{$IFDEF WITH_DART}
        nffDartNotes : begin
          Stream := TFileStream.Create( FN, ( fmCreate or fmShareExclusive ));
          try
            ds := _DART_ID + _DART_STOP +
                  _DART_VER + _DART_STOP + _DART_VEROK +
                  _DART_STOP + _DART_VEROK + _DART_STOP +
                  inttostr( FActiveNote ) + _DART_STOP;
            ds := ( inttostr( length( ds )) + _DART_STOP ) + ds;
            Stream.WriteBuffer( ds[1], length( ds ));

            if ( FPageCtrl.PageCount > 0 ) then
              // this is done so that we preserve the order of tabs.
              for i := 0 to pred( FPageCtrl.PageCount ) do begin
                myFolder := TKntFolder(FPageCtrl.Pages[i].PrimaryObject);
                myFolder.SaveDartNotesFormat( Stream );
              end
            else begin
               for i := 0 to pred( FNotes.Count ) do begin
                 myFolder := FNotes[i];
                 if assigned( myFolder ) then
                   myFolder.SaveDartNotesFormat( Stream );
               end;
            end;

            result := 0;
          finally
            Stream.Free;
          end;
        end; // nffDartNotes
{$ENDIF}

      end; // CASE

    except
      raise;
    end;
  finally
      if assigned(tf) then
         tf.Free;

      ImagesManager.ConversionStorageMode_End;
  end;

end; // SAVE


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
    NoteCount := FNotes.Count;
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

end; // EncryptFileInStream


procedure RaiseStreamReadError;
begin
  raise EKeyKntFileError.Create( STR_17 );
end; // RaiseStreamReadError


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

end; // DecryptFileToStream


function TKntFile.GetFolderByID( const aID : integer ) : TKntFolder;
var
  i, cnt : integer;
begin
  result := nil;
  cnt := FNotes.Count;
  for i := 1 to cnt do begin
     if ( FNotes[pred( i )].ID = aID ) then begin
       result := FNotes[pred( i )];
       break;
     end;
  end;
end; // GetNoteByID

function TKntFile.GetFolderByTreeNode( const myTreeNode: TTreeNTNode ) : TKntFolder;
var
  i, cnt : integer;
  myTV: TTreeNT;
begin
  result := nil;
  myTV := TTreeNT(myTreeNode.TreeView);
  cnt := FNotes.Count;
  for i := 1 to cnt do
     if ( TKntFolder(FNotes[pred( i )]).TV = myTV ) then begin
       result := FNotes[pred( i )];
       break;
     end;
end; // GetFolderByTreeNode


function TKntFile.GetFolderByName( const aName : string ) : TKntFolder;
// aName is NOT case-sensitive
var
  i, cnt : integer;
begin
  result := nil;
  cnt := FNotes.Count;
  for i := 1 to cnt do
     if ( ansicomparetext( FNotes[pred( i )].Name, aName ) = 0 ) then begin
       result := FNotes[pred( i )];
       break;
     end;
end;

{
function TKntFile.HasExtendedNotes : boolean;
var
  i : integer;
begin
  result := false;
  if ( FNotes.Count > 0 ) then
     Result:= true;
end;
}

function TKntFile.HasVirtualNodes : boolean;
var
  i : integer;
begin
  result := false;
  if ( FNotes.Count > 0 ) then
     for i := 0 to pred( FNotes.Count ) do begin
        if FNotes[i].Nodes.HasVirtualNodes then begin
          result := true;
          break;
        end;
     end;
end;


function TKntFile.HasVirtualNodeByFileName( const aNoteNode : TKntNote; const FN : string ) : boolean;
var
  cnt, i, n : integer;
  myFolder : TKntFolder;
begin
  result := false;
  cnt := FNotes.Count;
  if ( cnt <= 0 ) then Exit;

  for i := 0 to pred( cnt ) do begin
     myFolder := FNotes[i];
     if ( myFolder.Nodes.Count > 0 ) then
         for n := 0 to pred( myFolder.Nodes.Count ) do
            if ( myFolder.Nodes[n].VirtualMode <> vmNone ) then
              if ( myFolder.Nodes[n].VirtualFN = FN ) then
                 if ( aNoteNode <> myFolder.Nodes[n] ) then begin
                   result := true;
                   break;
                 end;
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


procedure TKntFile.SetFilename( const Value : string );
begin
  FFilename := Value;
  _VNKeyKntFileName := Value;
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

{
function TKntFile.GetBookmark(Index: integer): PBookmark;
begin
  Result := @FBookmarks[Index];
end;
}

function TKntFile.GetBookmark(Index: integer): TLocation;
begin
  Result := FBookmarks[Index];
end;

procedure TKntFile.WriteBookmark (Index: integer; Value: TLocation);
begin
   FBookmarks[Index]:= Value;
end;


procedure TKntFile.SetupMirrorNodes (Folder : TKntFolder);
var
  Node, Mirror : TTreeNTNode;
  p: integer;

  procedure SetupTreeNote;
  begin
    Node := Folder.TV.Items.GetFirstNode;
    while assigned( Node ) do begin // go through all nodes
        if assigned(Node.Data) and (TKntNote(Node.Data).VirtualMode= vmKNTNode) then begin
           TKntNote(Node.Data).LoadMirrorNode;
           Mirror:= TKntNote(Node.Data).MirrorNode;
           if assigned(Mirror) then
              AddMirrorNode(Mirror, Node)
           else
              SelectIconForNode( Node, Folder.IconKind);
        end;
        Node := Node.GetNext; // select next node to search
    end;

    if (Folder = kn_global.ActiveKntFolder) and assigned(Folder.TV.Selected)
         and (TKntNote(Folder.TV.Selected.Data).VirtualMode = vmKNTNode) then
       Folder.DataStreamToEditor;
  end;

begin
    if assigned(Folder) then
       SetupTreeNote
    else
       for p := 0 to pred( Folders.Count ) do begin
          Folder := Folders[p];
          SetupTreeNote;
       end;
end;


procedure TKntFile.ManageMirrorNodes(Action: integer; node: TTreeNTNode; targetNode: TTreeNTNode);
var
    nonVirtualTreeNode, newNonVirtualTreeNode: TTreeNTNode;
    i: integer;
    noteNode: TKntNote;

    p: Pointer;
    o: TObject;
    NodesVirtual: TList;

    procedure ManageVirtualNode (NodeVirtual: TTreeNTNode);
    begin
       if not assigned(NodeVirtual) then exit;
       noteNode:= NodeVirtual.Data;
       if not assigned(noteNode) then exit;
       case Action of
          1: noteNode.MirrorNode:= targetNode;

          2: if NodeVirtual <> node then
                ChangeCheckedState(TTreeNT(NodeVirtual.TreeView), NodeVirtual, (node.CheckState = csChecked), true);

          3: if not assigned(newNonVirtualTreeNode) then begin
                newNonVirtualTreeNode:= NodeVirtual;
                noteNode.MirrorNode:= nil;
                TKntNote(node.Data).Stream.SaveToStream(noteNode.Stream);
              end
              else
                noteNode.MirrorNode:= newNonVirtualTreeNode;
       end;
    end;

begin
   if not assigned(node) or not assigned(node.Data) then exit;

  // 1: Moving node to targetNode
  // 2: Changed checked state of node
  // 3: Deleting node
  try
      noteNode:= TKntNote(node.Data);
      if noteNode.VirtualMode = vmKNTNode then begin
          nonVirtualTreeNode:= noteNode.MirrorNode;
          if not assigned(nonVirtualTreeNode) then exit;
          case Action of
            1: exit;
            2: ChangeCheckedState(TTreeNT(nonVirtualTreeNode.TreeView), nonVirtualTreeNode, (node.CheckState = csChecked), true);
            3: begin
               RemoveMirrorNode(nonVirtualTreeNode, Node);
               exit;
               end;
          end;
      end
      else
          nonVirtualTreeNode:= node;

      p:= GetMirrorNodes(nonVirtualTreeNode);
      if assigned(p) then begin
         newNonVirtualTreeNode:= nil;
         o:= p;
         if o is TTreeNTNode then
            ManageVirtualNode(TTreeNTNode(p))
         else begin
           NodesVirtual:= p;
           for i := 0 to pred( NodesVirtual.Count ) do
              ManageVirtualNode(NodesVirtual[i]);
         end;
         case Action of
            1: ReplaceNonVirtualNode(nonVirtualTreeNode, targetNode);
            3: begin
                 if assigned(newNonVirtualTreeNode) and assigned(newNonVirtualTreeNode.Data) then begin
                   RemoveMirrorNode(nonVirtualTreeNode, newNonVirtualTreeNode);
                   ReplaceNonVirtualNode(nonVirtualTreeNode, newNonVirtualTreeNode);
                   SelectIconForNode( newNonVirtualTreeNode, GetFolderByTreeNode(newNonVirtualTreeNode).IconKind);
                 end;
               end;
         end;
      end;
      if (Action = 3) then
         AlarmManager.RemoveAlarmsOfNode(TKntNote(nonVirtualTreeNode.Data));

  finally
  end;

end;

procedure TKntFile.UpdateTextPlainVariables (nMax: integer);
var
  i: integer;
  myFolder: TKntFolder;
  AllNotesInitialized: boolean;
  RTFAux: TTabRichEdit;
begin
    if FileIsBusy then Exit;
    if FTextPlainVariablesInitialized then Exit;

    RTFAux:= CreateRTFAuxEditorControl;
    try
      try
        AllNotesInitialized:= True;

        for i := 0 to pred( FNotes.Count ) do begin
           myFolder := FNotes[i];
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


procedure TKntFile.UpdateImagesStorageModeInFile (ToMode: TImagesStorageMode; ApplyOnlyToFolder: TKntFolder= nil; ExitIfAllImagesInSameModeDest: boolean = true);
var
  i, j: integer;
  myFolder: TKntFolder;
  myNotes: TKntNoteList;
  Stream: TMemoryStream;
  ImagesIDs: TImageIDs;

   procedure UpdateImagesStorageMode (Stream: TMemoryStream);
   var
     ReplaceCorrectedIDs: boolean;
   begin
       if ToMode <> smEmbRTF then begin
          ImagesIDs:= myFolder.CheckSavingImagesOnMode (imLink, Stream, ExitIfAllImagesInSameModeDest);
          ImagesManager.UpdateImagesCountReferences (nil, ImagesIDs);
          if (ActiveFolderID = myFolder.ID) then
             myFolder.ImagesReferenceCount:= ImagesIDs;
       end
       else
          myFolder.CheckSavingImagesOnMode (imImage, Stream, ExitIfAllImagesInSameModeDest);
   end;

begin
   if (ApplyOnlyToFolder = nil)  then
      ImagesManager.ResetAllImagesCountReferences;

   // ApplyOnlyToNote: Para usar desde MergeFromKNTFile

   for i := 0 to pred( FNotes.Count ) do begin
      myFolder := FNotes[i];
      if myFolder.PlainText then continue;
      if (ApplyOnlyToFolder <> nil) and (myFolder <> ApplyOnlyToFolder) then continue;

      myFolder.EditorToDataStream;

      myNotes:= myFolder.Nodes;
      for j := 0 to myNotes.Count - 1 do  begin
         if (myNotes[j].VirtualMode = vmNone) then begin
            Stream:= myNotes[j].Stream;
            UpdateImagesStorageMode (Stream);
            if Length(ImagesIDs) > 0 then
               myNotes[j].NodeTextPlain:= '';      // Will have updated the Stream but not the editor, and been able to introduce/change image codes => force it to be recalculated when required

            if myNotes[j] = myFolder.SelectedNode then
               myFolder.DataStreamToEditor;
         end;
      end;

   end;

end;




function TKntFile.EnsurePlainTextAndRemoveImages (myFolder: TKntFolder): boolean;
var
  i: integer;
  myNotes: TKntNoteList;
  Stream: TMemoryStream;
  RTFAux : TRxRichEdit;

  procedure EnsurePlainTextAndCheckRemoveImages (UpdateEditor: boolean);
  var
     ImagesIDs: TImageIDs;
  begin
      ImagesIDs:= ImagesManager.GetImagesIDInstancesFromRTF (Stream);
      if Length(ImagesIDs) > 0 then
         ImagesManager.RemoveImagesReferences (ImagesIDs);

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
      RTFAux:= CreateRTFAuxEditorControl;
      try
         myNotes:= myFolder.Nodes;
         for i := 0 to myNotes.Count - 1 do  begin
            if (myNotes[i].VirtualMode = vmNone) then begin
               Stream:= myNotes[i].Stream;
               EnsurePlainTextAndCheckRemoveImages (myNotes[i] = myFolder.SelectedNode);
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
  myNotes: TKntNoteList;
  Stream: TMemoryStream;
  ImagesIDs: TImageIDs;

begin
   myNotes:= myFolder.Nodes;
   for i := 0 to myNotes.Count - 1 do  begin
      if (myNotes[i].VirtualMode = vmNone) then begin
         Stream:= myNotes[i].Stream;
         ImagesIDs:= ImagesManager.GetImagesIDInstancesFromRTF (Stream);
         if Length(ImagesIDs) > 0 then
            ImagesManager.RemoveImagesReferences (ImagesIDs);
      end;
   end;
   myFolder.ResetImagesReferenceCount;
end;


procedure TKntFile.RemoveImagesCountReferences (myNote: TKntNote);
var
  Stream: TMemoryStream;
  ImagesIDs: TImageIDs;

begin
   if (myNote.VirtualMode = vmNone) then begin
      Stream:= myNote.Stream;
      ImagesIDs:= ImagesManager.GetImagesIDInstancesFromRTF (Stream);
      if Length(ImagesIDs) > 0 then
         ImagesManager.RemoveImagesReferences (ImagesIDs);
   end;
end;

procedure TKntFile.UpdateImagesCountReferences (myNote: TKntNote);
var
  Stream: TMemoryStream;
  ImagesIDs: TImageIDs;

begin
   Stream:= myNote.Stream;
   ImagesIDs:= ImagesManager.GetImagesIDInstancesFromRTF (Stream);
   if Length(ImagesIDs) > 0 then
      ImagesManager.UpdateImagesCountReferences (nil, ImagesIDs);
end;


// To be used from MergeFromKNTFile
procedure TKntFile.UpdateImagesCountReferences (myFolder: TKntFolder);
var
  i: integer;
  myNotes: TKntNoteList;
  Stream: TMemoryStream;
  ImagesIDs: TImageIDs;

begin
   myNotes:= myFolder.Nodes;
   for i := 0 to myNotes.Count - 1 do  begin
      if (myNotes[i].VirtualMode = vmNone) then begin
         Stream:= myNotes[i].Stream;
         ImagesIDs:= ImagesManager.GetImagesIDInstancesFromRTF (Stream);
         ImagesManager.UpdateImagesCountReferences (nil, ImagesIDs);
      end;
   end;
   myFolder.ImagesReferenceCount:= ImagesIDs;
end;


end.

