unit kn_ImagesMng;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain)

  Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
  in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


// See "kn_ImagesMng_Readme.txt"


interface

uses
   Winapi.Windows,
   Winapi.Messages,
   Winapi.ShellAPI,
   System.SysUtils,
   System.Math,
   System.Classes,
   System.Contnrs,
   System.IOUtils,
   System.Zip,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.ComCtrls,
   Vcl.Clipbrd,

   RxRichEd,
   SynGdiPlus,

   gf_streams,
   kn_KntFolder,
   knt.model.note,
   knt.ui.editor,
   kn_Const
   ;



const
  SEP = '|';
  IMG_LOG_FILE = '_LOG.txt';
  NUM_MIN_TO_FREE_ImageStreamsNotRecentlyUsed = 15;

type


//---------------------------------------------------------------------
//  TFilesStorage
//---------------------------------------------------------------------

 TStorageType = (stZIP, stFolder, stEmbeddedKNT, stEmbeddedRTF);


type

  TKntImage = class;
  TKntImageArray = Array of TKntImage;


  TFilesStorage = class abstract

  strict protected
    FPath: String;
    FAbsolutePath: String;
    FBasePath: String;                                          // Path of KeyNote file..
    function GeStorageType: TStorageType; virtual; abstract;

  public
    constructor Create(const Path: String; const BasePath: String); virtual;
    procedure Relocate(const Path: String); virtual;

    property StorageType: TStorageType read GeStorageType;
    function GetStorageDefinition: String; inline;
    function IsValid: Boolean; virtual; abstract;

    property Path: String read FPath;
    property AbsolutePath: String read FAbsolutePath;
    property BasePath: String read FBasePath;

    function OpenForRead: boolean; virtual;
    function OpenForWrite: boolean; virtual;
    procedure Close; virtual;

    function ExistsFile (const Path: String; const Name: String): Boolean; virtual; abstract;

    function SaveFile (const Stream: TMemoryStream; const Path: String; const Name: String; CompresionMethod: TZipCompression = zcStored): Boolean; virtual; abstract;
    function DeleteFile (const Path: String; const Name: String): Boolean; virtual; abstract;
    function Save (SaveAllImages: boolean= false): integer;
    procedure RegisterInLogFile (Strs: TStrings); virtual;

    function GetImageStream (const Path: String; const Name: String): TMemoryStream; virtual; abstract;          // Caller must free stream
  end;


//---------------------------------------------------------------------
//   TZipStorage
//---------------------------------------------------------------------

  TZipStorage = class(TFilesStorage)
  private
    fZip: TZipFile;
    fZipDeleted: TZipFile;
    fDeletedImagesZipFile: String;

  strict protected
    function GeStorageType: TStorageType; override;
    function CreateZipFile (const Path: String): boolean;

  public
    constructor Create(const Path: String; const BasePath: String); override;
    procedure Relocate(const Path: String); override;
    destructor Destroy; override;

    class function IsValidZip (const Path: string): Boolean;
    function IsValid: Boolean; override;
    function ZipDeletedIsValid: Boolean;

    function CreateImagesZipFile: boolean;
    function CreateDeletedZipFile: boolean;
    function OpenForRead: boolean; override;
    function OpenForWrite: boolean; override;
    procedure Close; override;

    function ExistsFile(const Path: String; const Name: String): Boolean; override;

    function SaveFile(const Stream: TMemoryStream; const Path: String; const Name: String;
                      CompresionMethod: TZipCompression = zcStored): Boolean; override;
    function DeleteFile(const Path: String; const Name: String): Boolean; override;
    function AddToDeletedZip(const Stream: TMemoryStream; const Path: String; const Name: String;
                      CompresionMethod: TZipCompression = zcStored): Boolean;

    function GetImageStream(const Path: String; const Name: String): TMemoryStream; override;
  end;


//---------------------------------------------------------------------
//   TFolderStorage
//---------------------------------------------------------------------

  TFolderStorage = class(TFilesStorage)
  private
    fNotOwned: boolean;

  strict protected
    function GeStorageType: TStorageType; override;

  public
    constructor Create(const Path: String; const BasePath: String); override;
    procedure Relocate(const Path: String); override;

    function IsValid: Boolean; override;
    function ExistsFile(const Path: String; const Name: String): Boolean; override;


    function SaveFile(const Stream: TMemoryStream; const Path: String; const Name: String;
                      CompresionMethod: TZipCompression = zcStored): Boolean; override;
    function DeleteFile(const Path: String; const Name: String): Boolean; override;

    function GetImageStream(const Path: String; const Name: String): TMemoryStream; override;

    function GetImagePath(const Path: String; const Name: String): string;
  end;


//---------------------------------------------------------------------
//   TEmbeddedKNTStorage
//---------------------------------------------------------------------

  TEmbeddedKNTStorage = class(TFilesStorage)

  strict protected
    function GeStorageType: TStorageType; override;

  public
    function IsValid: Boolean; override;

    function ExistsFile(const Path: String; const Name: String): Boolean; override;
    function SaveFile(const Stream: TMemoryStream; const Path: String; const Name: String; CompresionMethod: TZipCompression = zcStored): Boolean; override;
    function DeleteFile(const Path: String; const Name: String): Boolean; override;

    function GetImageStream(const Path: String; const Name: String): TMemoryStream; override;
  end;





//---------------------------------------------------------------------
//  TKntImage
//---------------------------------------------------------------------

  TKntImage = class
  private
    FID: Integer;
    FName: String;          // File name
    FPath: String;          // Path of the file. Together with Name identifies the file in Storage
    FCaption: String;
    FOwned: boolean;
    FReferenceCount: Integer;
    FMustBeSavedExternally: Boolean;
    FIsEncrypted: Boolean;
    FStreamIsEncrypted: Boolean;

    procedure SetImageStream (Stream: TMemoryStream);
    procedure FreeImageStream;
    procedure SetAccessed;
    procedure SetIsEncrypted (Value: boolean);
    function GetDetails: string;

  strict private
    FImageFormat: TImageFormat;
    FCRC32: DWORD;
    FOriginalPath: String;
    FWidth: Integer;
    FHeight: Integer;
    FImageStream: TMemoryStream;
    FLastAccessed: TDateTime;

    function GetName: String;
    function GetPath: String;
    function GetFileName: String;
    function GetImageStream: TMemoryStream; overload;
    function GetImageStreamAvailable: boolean;
    procedure DecryptImageStream;

  public
    constructor Create(ID: Integer;
                       const OriginalPath: String;
                       Owned: boolean;
                       ImageFormat: TImageFormat;
                       Width, Height: integer;
                       crc_32: DWORD;
                       ReferenceCount: Integer;
                       Stream: TMemoryStream= nil
                       );

    destructor Destroy; override;

    procedure GenerateName(const FolderName: string; const Source: string; ZipPathFormat: boolean; const NameProposed: string = '');
    procedure ForceName(Name: string);

    property ID: Integer read FID;
    property ImageFormat: TImageFormat read FImageFormat;
    property Name: String read GetName;
    property Path: String read GetPath;
    property Caption: string read FCaption write FCaption;
    property CRC32: DWORD read FCRC32;
    property OriginalPath: String read FOriginalPath;
    property IsOwned: boolean read FOwned;
    property FileName: String read GetFileName;

    property Width: integer read FWidth;
    property Height: integer read FHeight;
    procedure SetDimensions(Width, Height: integer);

    property ImageStream: TMemoryStream read GetImageStream;
    property ImageStreamAvailable: boolean read GetImageStreamAvailable;

    property ReferenceCount: Integer read FReferenceCount;
    property LastAccessed: TDateTime read FLastAccessed;
    property MustBeSavedExternally: boolean read FMustBeSavedExternally write FMustBeSavedExternally;
    property IsEncrypted: Boolean read FIsEncrypted write SetIsEncrypted;
    property StreamIsEncrypted: Boolean read FStreamIsEncrypted write FStreamIsEncrypted;

    property Details: string read GetDetails;
    function ImageDefinition: String; inline;
  end;



//---------------------------------------------------------------------
//  TImageMng
//---------------------------------------------------------------------

  TImageMng = class

  strict private

    fStorageMode : TImagesStorageMode;
    fExternalStorageToRead:  TFilesStorage;
    fExternalStorageToSave:  TFilesStorage;              //  = fExternalStorageToRead, salvo cuando se esté cambiando a un almacenamiento externo distinto
    fNotOwnedStorage: TFolderStorage;                    // Permite acceder a las imágenes Linked (not owned)
    fWasInformedOfStorageNotAvailable: Boolean;

    fFileIsNew: boolean;                                          // Is New => Not saved yet
    fIntendedExternalStorageType: TImagesExternalStorage;
    fIntendedStorageLocationPath: string;
    fChangingImagesStorage: boolean;
    fSaveAllImagesToExtStorage: boolean;
    fKntFile: TObject;                       // Not TKntFile to avoid circular reference with kn_KntFile

    // Si <> nil => se utilizará para recuperar los Stream de las imágenes que aparezcan referenciadas con sus IDs en los nodos de la
    // nota indicada (se usará para notas concretas, en combinación con KntFile.UpdateImagesStorageModeInFile (fStorageMode, ApplyOnlyToNote)
    fExternalImagesManager: TImageMng;

    fImagesMode: TImagesMode;
    fImages: TList;                             // All images (TKntImage)
    fNextImageID: Integer;
    fNextTempImageID: Integer;
    fLastCleanUpImgStreams: TDateTime;
    fReconsiderImageDimensionsGoal: boolean;
    fDoNotRegisterNewImages: boolean;

    fExportingMode: boolean;
    fImagesIDExported: TList;


    function GetNewID(): Integer;
    procedure SerializeEmbeddedImages(const tf: TTextFile);
    procedure SetExportingMode(Value: boolean);

    function GetExternalStorageType: TImagesExternalStorage;
    function GetExternalStorageLocation: string;
    function GetExternalStorageIsMissing: boolean;


  protected
     function GetImgViewerIsOpen: boolean;
     function GetImgIDinViewer: integer;

  public
    constructor Create;
    destructor Destroy; override;
    procedure SetInitialValues;
    procedure Clear (SetIniValues: boolean= true; ClearImages: boolean = true);

    property StorageMode: TImagesStorageMode read fStorageMode;
    property ExternalStorageType: TImagesExternalStorage read GetExternalStorageType;
    property ExternalStorageLocation: string read GetExternalStorageLocation;
    property ExternalStorageIsMissing: boolean read GetExternalStorageIsMissing;

    property  ChangingImagesStorage: boolean read fChangingImagesStorage;
    procedure ConversionStorageMode_End;
    function  PrepareImagesStorageToSave(const FN: string): boolean;
    procedure SetInitialImagesStorageMode (StorageMode: TImagesStorageMode; ExternalStorageType: TImagesExternalStorage);
    function  SetImagesStorage (StorageMode: TImagesStorageMode; ExternalStorageType: TImagesExternalStorage;
                                Path: string;
                                KntFilePath: string;
                                CreateExternalStorageOnNewFile: boolean= false;
                                ExternalStorageRelocated: boolean= false): boolean;
    procedure AdaptPathFormatInImages (ZipPathFormat: boolean);
    function GetDefaultExternalLocation (ExtType: TImagesExternalStorage; FN: string= ''): string;
    property FileIsNew: boolean read fFileIsNew write fFileIsNew;
    property KntFile: TObject read fKntFile write fKntFile;

    property Images: TList read fImages;
    property ImagesMode: TImagesMode read fImagesMode write fImagesMode;            // See TKntFolder.ImagesMode  ==> ImagesManager.ProcessImagesInRTF
    property NextImageID: Integer read fNextImageID;
    property NextTempImageID: Integer read fNextTempImageID;
    property ReconsiderImageDimensionsGoal: boolean read fReconsiderImageDimensionsGoal write fReconsiderImageDimensionsGoal;
    property DoNotRegisterNewImages: boolean read fDoNotRegisterNewImages write fDoNotRegisterNewImages;

    procedure CheckToMarkSaveExternally(Img: TKntImage);
    function CheckUniqueName (var Name: string): boolean;
    function CheckRegisterImage (Stream: TMemoryStream; ImgFormat: TImageFormat;
                                 Width, Height: integer;
                                 const FolderName: string;
                                 const OriginalPath: String;
                                 Owned: boolean;
                                 const Source: String;
                                 var Img: TKntImage;
                                 const NameProposed: string = ''
                                 ): boolean;

    function RegisterNewImage (Stream: TMemoryStream;
                               ImageFormat: TImageFormat;
                               Width, Height: integer;
                               crc32: DWORD;
                               const OriginalPath: String;
                               Owned: boolean;
                               const Source: String;
                               const FolderName: String;
                               const NameProposed: string = ''
                               ): TKntImage;

    function GetMaxSavedImageID: integer;
    function GetImageFromStream (Stream: TMemoryStream; var CRC32: DWORD; SetLastAccess: boolean= true): TKntImage;
    function GetImageFromID (ImgID: integer; SetLastAccess: boolean= true): TKntImage; overload;
    function GetImageFromFileName (const FileName: string; SetLastAccess: boolean= true): TKntImage; overload;
    function GetPrevImage (ImgID: integer; SetLastAccess: boolean= true): TKntImage;
    function GetNextImage (ImgID: integer; SetLastAccess: boolean= true): TKntImage;
    function GetImagePath (Img: TKntImage): string;

    procedure ReloadImageStream (Img: TKntImage);
    procedure CheckFreeImageStreamsNotRecentlyUsed;
    procedure FreeImage (Img: TKntImage);
    function RecalcNextID: boolean;

    procedure InsertImage (FileName: String; Editor: TKntRichEdit; Owned: boolean; const NameProposed: string = '');
    procedure InsertImageFromClipboard (Editor: TKntRichEdit; FolderName: string; TryAddCaption: boolean = true);
    procedure RegisterAndInsertImage(FileName: String; Editor: TKntRichEdit; Owned: boolean;
                                     var RTFForImageInsertion: AnsiString;
                                     const NameProposed: string= '';
                                     Stream: TMemoryStream = nil;
                                     AddToEditor: boolean = true;
                                     W: integer = 0; H: integer = 0);


    function ProcessImagesInRTF (const RTFText: AnsiString;
                                 const FolderName: string;
                                 ImagesModeDest: TImagesMode;
                                 const Source: string;
                                 FirstImageID: integer= 0;
                                 ExitIfAllImagesInSameModeDest: boolean= false
                                 ): AnsiString; overload;

    function ProcessImagesInRTF (const Buffer: Pointer; BufSize: integer;
                                 const FolderName: string;
                                 ImagesModeDest: TImagesMode;
                                 const Source: string;
                                 FirstImageID: integer;
                                 var ContainsImgIDsRemoved: boolean;
                                 var ContainsImages: boolean;
                                 ExitIfAllImagesInSameModeDest: boolean = false
                                 ): AnsiString; overload;

    function ProcessImagesInClipboard(Editor: TKntRichEdit; const FolderName: string; SelStartBeforePaste: integer; FirstImageID: integer= 0): boolean;
    function TryRTFPictToImage (Buffer: Pointer; BufSize: integer; var Img: TKntImage): boolean;

    procedure ResetAllImagesCountReferences;
    procedure RemoveImagesReferences (const IDs: TImageIDs);
    function  GetImagesIDInstancesFromRTF (Stream: TMemoryStream): TImageIDs;
    function  GetImagesIDInstancesFromTextPlain (TextPlain: String): TImageIDs;
    procedure UpdateImagesCountReferences (const IDsBefore: TImageIDs;  const IDsAfter: TImageIDs);
    function  ImageInCurrentEditors (ImgID: integer; UseFreshTextPlain: boolean= false): Boolean;

    procedure ReloadImages(const IDs: TImageIDs);

    function GetPositionOffset (Stream: TMemoryStream; Pos_ImLinkTextPlain: integer; CaretPos: integer; const imLinkTextPlain: String; RTFModified: boolean; ForceCalc: boolean = false): integer;
    function GetPositionOffset_FromImLinkTP (Stream: TMemoryStream; Pos_ImLinkTextPlain: integer; const imLinkTextPlain: String; RTFModified: boolean; ForceCalc: boolean = false): integer;
    function GetPositionOffset_FromEditorTP (Stream: TMemoryStream; CaretPos: integer; const imLinkTextPlain: String; RTFModified: boolean; ForceCalc: boolean = false): integer;


    procedure LoadState (const tf: TTextFile; var FileExhausted: Boolean);
    procedure SaveState (const tf: TTextFile);
    procedure DeleteOrphanImages();
    procedure SerializeImagesDefinition  (const tf: TTextFile);
    procedure SaveEmbeddedImages (const tf: TTextFile);

    property  ExportingMode: boolean read fExportingMode write SetExportingMode;
    procedure SaveStateInExportingMode (const tf: TTextFile);
    procedure RegisterImagesReferencesExported (const IDs: TImageIDs);

    property ExternalImagesManager: TImageMng read fExternalImagesManager write fExternalImagesManager;

    property ImgViewerIsOpen: boolean read GetImgViewerIsOpen;
    property ImgIDinViewer: integer read GetImgIDinViewer;
    procedure OpenImageFile(FilePath: string);
    procedure OpenImageViewer (ImgID: integer; ShowExternalViewer: boolean; SetLastFormImageOpened: boolean; Img: TKntImage = nil);
    procedure CheckBringToFrontLastImageViewer;

    procedure ProcessEncryptedImages;
    procedure DecryptAllImages;
    procedure ToogleEncrypted(ImgID: integer);
  end;




//--------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------

implementation

uses  System.DateUtils,
      WinApi.MMSystem,
      ComCtrls95,
      CRC32,
      gf_misc,
      gf_miscvcl,
      gf_files,
      gf_strings,
      kn_ClipUtils,
      kn_Global,
      kn_RTFUtils,
      kn_ImageForm,
      kn_ImagesUtils,
      kn_Main,
      kn_KntFile,
      knt.App,
      knt.RS
      ;






//==========================================================================================
//                                         TFilesStorage
//==========================================================================================

constructor TFilesStorage.Create(const Path: String; const BasePath: String);
begin
  inherited Create;
  FBasePath:= BasePath;
  Relocate(Path);
end;

procedure TFilesStorage.Relocate(const Path: String);
begin
  FPath:= Path;
  if FBasePath <> '' then
     FAbsolutePath := GetAbsolutePath(FBasePath, Path)
  else
     FAbsolutePath := '';
end;


function TFilesStorage.GetStorageDefinition: String;
begin
//  <ALM>: SD= Type (Zip/Folder)| Path

   Result:= Format('%s=%d|%s',  [_StorageDEF,  Ord(StorageType), Path])
end;


function TFilesStorage.OpenForRead: boolean;
begin
   Result:= IsValid;
end;

function TFilesStorage.OpenForWrite: boolean;
begin
   Result:= IsValid;
end;


procedure TFilesStorage.Close;
begin
end;


function TFilesStorage.Save (SaveAllImages: boolean= false): integer;
var
   i, j: Integer;
   Img: TKntImage;
   Images: TList;
   Strs: TStrings;
   CompressionMethod: TZipCompression;
   ImgStream: TMemoryStream;
   MaxSavedImgID: integer;
begin
   { Browse all images added and not yet saved. Some of them may have subsequently been marked for deletion.
     At that time it is not removed from this list (FImagesToSave) in anticipation of it being rolled back based on the use of the UNDO mechanism.
     Now, when saving, we check if they are still marked for deletion. If so, they will be removed from both lists and of course not saved to disk.
   }
   Images:= ImageMng.Images;

   MaxSavedImgID:= 0;

   Strs:= TStringList.Create;
   OpenForWrite;
   try
      for i := 0 to Images.Count -1 do begin
          try
             Img := TKntImage(Images[i]);
             if not Img.IsOwned then begin
                if Img.ID > MaxSavedImgID then
                   MaxSavedImgID:= Img.ID;
               continue;
             end;

             if not SaveAllImages and not Img.MustBeSavedExternally then continue;

             if Img.ReferenceCount = 0 then begin
                Img.FreeImageStream;
                Img.Free;
                Images[i]:= nil;
             end
             else begin
                CompressionMethod:= KeyOptions.ImgDefaultCompression;
                if Img.ImageFormat= imgJpg then
                   CompressionMethod:= zcStored;

                ImgStream:= Img.ImageStream;
                if (ImgStream <> nil) then begin
                   ImgStream.Position := 0;
                   if SaveFile(ImgStream, Img.Path, Img.Name, CompressionMethod) then begin
                      Img.MustBeSavedExternally:= False;
                      Strs.Add(FormatDateTime('dd/MMM/yyyy HH:nn - ', Now) + 'Added:   ' + Img.ImageDefinition);
                      if Img.ID > MaxSavedImgID then
                         MaxSavedImgID:= Img.ID;
                   end;
                end
                else begin
                   App.WarningPopup(Format(GetRS(sImg21), [Img.Name, Img.ID]));
                   if Img.ID > MaxSavedImgID then
                      MaxSavedImgID:= Img.ID;          // To avoid confusion we will 'consume' your ID
                   Images[i]:= nil;                    // It will be deleted in next for loop
                end;
             end;

          except
          end;
      end;

      for i := Images.Count -1 downto 0 do begin
         Img := TKntImage(Images[i]);
         if Img = nil then
            Images.Delete(i);
      end;


      if Strs.Count > 0 then
         RegisterInLogFile (Strs);

   finally
      Close;
      Strs.Free;
   end;

   Result:= MaxSavedImgID;

end;


procedure TFilesStorage.RegisterInLogFile (Strs: TStrings);
var
  str, LogFN: string;
begin
   str:= Strs.Text;
   LogFN:= FAbsolutePath + IMG_LOG_FILE;

   if not FileExists(LogFN) then
      TFile.WriteAllText(LogFN, 'PD= IDImg|Path|Name|Format(GIF,PNG,JPG,BMP,TIF,WMF,EMF)|Width|Height|crc32|OriginalPath|Owned|RefCount|Caption|MustBeSavedExt' + #13#10#13#10);

   TFile.AppendAllText(LogFN, str, TEncoding.UTF8);
end;



//=====================================================================
//  TZipStorage
//=====================================================================

constructor TZipStorage.Create(const Path: String; const BasePath: String);
begin
  inherited Create (Path, BasePath);
  try
     fZip:= TZipFile.Create();
     fZipDeleted:= TZipFile.Create();

     Relocate (Path);

  except on E: Exception do
     App.ErrorPopup(E, GetRS(sImg16));
  end;
end;


procedure TZipStorage.Relocate(const Path: String);
var
   p: integer;
begin
  inherited Relocate (Path);

  fDeletedImagesZipFile:= FAbsolutePath;
  p := lastpos( '.', fDeletedImagesZipFile );
  if ( p > 2 ) then begin
     delete( fDeletedImagesZipFile, p, length(fDeletedImagesZipFile));
     fDeletedImagesZipFile:= fDeletedImagesZipFile + 'Deleted.zip';
  end
  else
      fDeletedImagesZipFile:= '';
end;


destructor TZipStorage.Destroy;
begin
   if assigned(fZip) then
      fZip.Free;
   if assigned(fZipDeleted) then
      fZipDeleted.Free;

   inherited Destroy;
end;


function TZipStorage.GeStorageType: TStorageType;
begin
  Result:= stZIP;
end;

class function TZipStorage.IsValidZip (const Path: string): Boolean;
begin
   Result:= FileExists(Path) and TZipFile.IsValid(Path);
end;


function TZipStorage.IsValid: Boolean;
begin
   Result:= FileExists(FAbsolutePath) and fZip.IsValid(FAbsolutePath);
end;

function TZipStorage.ZipDeletedIsValid: Boolean;
begin
   Result:= FileExists(fDeletedImagesZipFile) and fZip.IsValid(fDeletedImagesZipFile);
end;


function TZipStorage.OpenForRead: boolean;
begin
   if fZip.Mode = zmRead then exit (true);

   if fZip.Mode <> zmClosed then
      fZip.Close;

   Result:= false;
   try
      if IsValid then begin
         fZip.Open(FAbsolutePath, zmRead);
         Result:= true;
      end;
   except
   end;
end;

function TZipStorage.OpenForWrite: boolean;
begin
   if fZip.Mode = zmReadWrite then exit (true);

   if fZip.Mode <> zmClosed then
      fZip.Close;

   Result:= false;
   try
      if IsValid then begin
         fZip.Open(FAbsolutePath, zmReadWrite);
         Result:= true;
      end;
   except
   end;
end;

procedure TZipStorage.Close;
begin
   fZip.Close;
end;


function TZipStorage.CreateImagesZipFile: boolean;
begin
    Result:= CreateZipFile (FAbsolutePath);
end;

function TZipStorage.CreateDeletedZipFile: boolean;
begin
    Result:= CreateZipFile (fDeletedImagesZipFile);
end;


function TZipStorage.CreateZipFile (const Path: String): boolean;
begin
   Result:= false;
   try
      if not FileExists(Path) then begin
         fZip.Open(Path, zmWrite);
         fZip.Close;
         Result:= true;
      end;
   except
   end;
end;



//https://en.delphipraxis.net/topic/3328-delphi%E2%80%99s-tzipfile-working-on-a-stream/

function TZipStorage.ExistsFile(const Path: String; const Name: String): Boolean;
begin
   Result:= fZip.IndexOf(Path + Name) >= 0;
end;

function TZipStorage.GetImageStream(const Path: String; const Name: String): TMemoryStream;
var
  LocalHeader: TZipHeader;
  Stream: TStream;
  FN: string;
begin
  Result:= TMemoryStream.Create;
  try
     FN:= Path + Name;                     // Use / instead of \
     fZip.Read(FN, Stream, LocalHeader);
     Result.CopyFrom(Stream, 0);

  except on E: Exception do
     FreeAndNil(Result);
  end;
end;

function TZipStorage.SaveFile(const Stream: TMemoryStream; const Path: String; const Name: String; CompresionMethod: TZipCompression = zcStored): Boolean;
begin
  Result:= false;
  try
    fZip.Add(Stream, Path + Name, CompresionMethod);
    Result:= true;

  except on E: Exception do
     App.ErrorPopup(E, GetRS(sImg17));
  end;
end;


function TZipStorage.DeleteFile(const Path: String; const Name: String): Boolean;
begin
  try
    fZip.Delete(Path + Name);
    Result:= true;
  except
    Result:= false;
  end;
end;


function TZipStorage.AddToDeletedZip(const Stream: TMemoryStream; const Path: String; const Name: String; CompresionMethod: TZipCompression = zcStored): Boolean;
begin
    if (fDeletedImagesZipFile = '') or (Stream = nil) then exit(false);

    if not ZipDeletedIsValid then begin
       if not CreateDeletedZipFile then
          exit(false);
    end;

   try
      fZipDeleted.Open(fDeletedImagesZipFile, zmReadWrite);
      Stream.Position := 0;
      fZipDeleted.Add(Stream, Path + Name, CompresionMethod);
      fZipDeleted.Close;
   except
   end;


end;


//=====================================================================
//  TFolderStorage
//=====================================================================

constructor TFolderStorage.Create(const Path: String; const BasePath: String);
begin
  inherited Create (Path, BasePath);
  fNotOwned:= false;
  Relocate(Path);
end;

procedure TFolderStorage.Relocate(const Path: String);
begin
  inherited Relocate (Path);
  if (FAbsolutePath.Length > 1) and (FAbsolutePath[FAbsolutePath.Length] <> '\') then
     FAbsolutePath:= FAbsolutePath + '\';
end;


function TFolderStorage.GeStorageType: TStorageType;
begin
  Result:= stFolder;
end;

function TFolderStorage.IsValid: Boolean;
begin
  if fNotOwned then exit (true);

  Result:= TDirectory.Exists(FAbsolutePath);
end;


function TFolderStorage.ExistsFile(const Path: String; const Name: String): Boolean;
begin
    Result:= FileExists(FAbsolutePath + Path + Name);
end;

function TFolderStorage.GetImageStream(const Path: String; const Name: String): TMemoryStream;
var
   FilePath: String;
begin
    Result:= TMemoryStream.Create;
    try
       FilePath:= GetImagePath(Path, Name);
       Result.LoadFromFile(FilePath);
    except
       FreeAndNil (Result);
    end;
end;


function TFolderStorage.GetImagePath(const Path: String; const Name: String): string;
var
   FilePath: String;
begin
   FilePath:= Path + Name;
   if fNotOwned then
      FilePath:= GetAbsolutePath(FBasePath, FilePath)
   else
      FilePath:= FAbsolutePath + FilePath;

   Result:= FilePath;
end;


function TFolderStorage.SaveFile(const Stream: TMemoryStream; const Path: String; const Name: String; CompresionMethod: TZipCompression = zcStored): Boolean;
var
  AbsolutePath: string;
begin
   if fNotOwned then exit (false);

   { If it cannot be saved because the folder corresponding to the storage is not available, or exists
     some problem in its access, the image will be temporarily saved in the KNT embedded storage}

    Result:= false;
    if IsValid then begin
       AbsolutePath:= FAbsolutePath + Path;              // Path:  *inside* the Storage
       if ForceDirectories (AbsolutePath) then begin
          try
             Stream.SaveToFile(AbsolutePath + Name);
             Result:= true;
          except
          end;
       end;
    end;

end;


function TFolderStorage.DeleteFile(const Path: String; const Name: String): Boolean;
var
   FileName, RecFolder: string;
begin
    Result:= false;
    if fNotOwned then exit;

   if IsValid then begin
      FileName:= FAbsolutePath + Path + Name;
      if KeyOptions.ImgUseRecycleBin then begin
         RecFolder:= FAbsolutePath + '_RecycleBin\';
         if ForceDirectories (RecFolder) then
            Result:= MoveFileExW_n (FileName, RecFolder + Name, 3)
      end
      else
         Result:= System.SysUtils.DeleteFile(FileName);
   end;

end;


//=====================================================================
//  TEmbeddedStorage
//=====================================================================


function TEmbeddedKNTStorage.GeStorageType: TStorageType;
begin
  Result:= stEmbeddedKNT;
end;

function TEmbeddedKNTStorage.IsValid: Boolean;
begin
   Result:= true;
end;

function TEmbeddedKNTStorage.ExistsFile(const Path: String; const Name: String): Boolean;
begin
   Result:= True;
end;


function TEmbeddedKNTStorage.GetImageStream(const Path: String; const Name: String): TMemoryStream;
begin
   // This method should not be called, since the images (TKntImages) must have and maintain the Stream
   Result:= nil;
end;

function TEmbeddedKNTStorage.SaveFile(const Stream: TMemoryStream; const Path: String; const Name: String; CompresionMethod: TZipCompression = zcStored): Boolean;
begin
    Result:= true;
end;

function TEmbeddedKNTStorage.DeleteFile(const Path: String; const Name: String): Boolean;
begin
    Result:= true;
end;



//==========================================================================================
//                                         TKNTImage
//==========================================================================================


constructor TKntImage.Create(ID: Integer;
                             const OriginalPath: String;
                             Owned: boolean;
                             ImageFormat: TImageFormat;
                             Width, Height: integer;
                             crc_32: DWORD;
                             ReferenceCount: Integer;
                             Stream: TMemoryStream= nil
                             );
begin
   inherited Create;

   FMustBeSavedExternally:= False;
   FIsEncrypted:= False;
   FStreamIsEncrypted:= False;

   FID:= ID;
   FImageFormat:= ImageFormat;
   FWidth:= Width;
   FHeight:= Height;
   FOriginalPath:= OriginalPath;
   FOwned:= Owned;
   FReferenceCount:= ReferenceCount;
   FImageStream:= Stream;

   if crc_32 <> 0 then
      FCRC32:= crc_32
   else
      if Stream <> nil then
         CalcCRC32(Stream.Memory, Stream.Size, FCRC32);
end;


destructor TKntImage.Destroy;
begin
   if assigned(FImageStream) then
      FImageStream.Free;

   inherited Destroy;
end;


{ Generates a Name and Path for a new image, unique in the storage, and taking into account that it has been created on the indicated node/folder  }

procedure TKntImage.GenerateName(const FolderName: string; const Source: string; ZipPathFormat: boolean; const NameProposed: string = '');
var
  src: String;
  strDate: string;
begin
    if not fOwned then exit;

    if FOriginalPath <> '' then begin
       FName:= ExtractFilename(FOriginalPath);
       if not KeyOptions.ImgKeepOrigName then
          FName:= Format('%d_%s', [ID, FName])
       else begin
          if NameProposed <> '' then
             FName:= NameProposed;
          ImageMng.CheckUniqueName (FName);
       end;
    end
    else begin
       if Source = '' then
          src:= 'Image'
       else
          src:= Source;

       // It seems that from some locales the following expression adds a dot to the end and in others it doesn't
       // We cannot therefore depend on that dot to separate the extension. We must control it explicitily
       strDate:= FormatDateTime( 'ddMMM', Now);
       if strDate[Length(strDate)]= '.' then
          strDate:= Copy(strDate, 1, Length(strDate)-1);

       FName:= Format('%d_%s_%s.%s', [ID, src, strDate, IMAGE_FORMATS[FImageFormat] ]);
    end;

    if KeyOptions.ImgSaveInSubfolders and (FolderName <> '') then begin
       FPath:= MakeValidFileName( FolderName, [' '], MAX_FILENAME_LENGTH );

       if ZipPathFormat then
          FPath:= FPath + '/'
       else
          FPath:= FPath + '\'
    end;

end;

procedure TKntImage.ForceName(Name: string);
begin
   if fOwned then
      FName:= Name;
end;

function TKntImage.GetName: String;
begin
   if fOwned then
      Result:= FName
   else
      Result:= ExtractFilename(FOriginalPath);
end;


function TKntImage.GetPath: String;
begin
   if fOwned then
      Result:= FPath
   else
      Result:= ExtractFilePath(FOriginalPath);
end;


function TKntImage.GetFileName: String;
begin
   if not fOwned then
      Result:= FOriginalPath
   else
      if FPath <> '' then
         Result:= FPath + FName
      else
         Result:= FName;
end;


procedure TKntImage.SetDimensions(Width, Height: integer);
begin
    FWidth:= Width;
    FHeight:= Height;
end;


function TKntImage.ImageDefinition: String;
var
  path, name: string;
begin
   { <IMG1>:
     PD= IDImg|Path|Name|ImageFormat|Width|Height|crc32|OriginalPath|Owned|ReferenceCount|Caption|MustBeSavedExternally|Encrypted
   }
   if fOwned then begin
      path:= Self.Path;
      name:= Self.Name;
   end;

   Result:= Format('%s=%d|%s|%s|%d|%d|%d|%s|%s|%s|%d|%s|%s|%s',
               [_ImageDEF,  ID, path, name, Ord(ImageFormat), FWidth, FHeight, FCRC32.ToString,
                OriginalPath, BOOLEANSTR[FOwned], ReferenceCount, FCaption, BOOLEANSTR[FMustBeSavedExternally],BOOLEANSTR[FIsEncrypted]]);
end;


function TKntImage.GetDetails: string;
var
  location: string;
  sizeKB: String;
begin
   //  MyFile.png | MyPath | 9.0 KB | 120x25 PNG ....

   if fOwned then begin
      location:= Name;
      if Path <> '' then
         location:= location + ' | ' + Path;
      {if OriginalPath <> '' then
         location:= location + ' [ ' + OriginalPath + ' ]';}
   end
   else
      location:= OriginalPath;

   if ImageStream <> nil then
      SizeKB:= SimpleRoundTo(fImageStream.Size/1024, -1).ToString
   else
      SizeKB:= '-- ';

   if FIsEncrypted then
      location:= '[*] ' + location;

   Result:= Format('%s | %s KB | %d x %d %s', [location, SizeKB, FWidth,FHeight,IMAGE_FORMATS[ImageFormat].ToUpper ]);

   if ReferenceCount <> 1 then
      Result:= Result + Format(GetRS(sImg09), [ReferenceCount]);

end;


function TKntImage.GetImageStreamAvailable: boolean;
begin
   Result:= assigned(FImageStream);
end;

procedure TKntImage.DecryptImageStream;
var
   DecryptedStream: TMemoryStream;
   EncryptionInfo: TEncryptionInfo;

begin
   if FImageStream = nil then exit;

   try
     EncryptionInfo:= TKntFile(ImageMng.KntFile).GetEncryptionInfo;
     DecryptedStream:= TMemoryStream.Create;
     EncryptionInfo.DataSize:= FImageStream.Size;
     FImageStream.Position:= 0;
     TKntFile(ImageMng.KntFile).DecryptStream(EncryptionInfo, FImageStream, DecryptedStream);
     FImageStream.Clear;
     FImageStream.Free;
     FImageStream:= DecryptedStream;

     FStreamIsEncrypted:= False;

   except
     on E: Exception do
       App.ErrorPopup(E, GetRS(sImg23));
   end;
end;


{ Returns the image content in the original format.
-> CALLER should not modify or release the stream }

function TKntImage.GetImageStream: TMemoryStream;
begin
   if FImageStream = nil then
      ImageMng.ReloadImageStream(Self);

   if FIsEncrypted and FStreamIsEncrypted and not AFileIsLoading and not TKntFile(ImageMng.KntFile).EncryptedContentMustBeHidden then
      DecryptImageStream;

   Result:= FImageStream;
end;

procedure TKntImage.SetImageStream (Stream: TMemoryStream);
begin
   FImageStream:= Stream;
   if not AFileIsLoading then
      FStreamIsEncrypted:= False;
end;

procedure TKntImage.SetAccessed;
begin
   FLastAccessed:= Now;
end;

procedure TKntImage.FreeImageStream;
begin
   if assigned(FImageStream) then
      FreeAndNil(FImageStream);
end;

procedure TKntImage.SetIsEncrypted (Value: boolean);
begin
   if FIsEncrypted = Value then exit;
   if not TKntFile(ImageMng.KntFile).CheckAuthorized(True) then exit;

   FIsEncrypted:= Value;
   if FIsEncrypted then
      MustBeSavedExternally:= False                 // TODO: Delete from external storage... (if it was saved externally without encryption)

   else begin
      ImageMng.CheckToMarkSaveExternally(Self);
      if FStreamIsEncrypted then
         DecryptImageStream;
   end;
end;


//==========================================================================================
//                                   TImageMng
//==========================================================================================

//----------------------------------
//          Creation / Destruction
//----------------------------------

{ Not created as Singleton because for the MergeFromKNTFile functionality we need to have a
  TImageMng object that manages the file to be merged. }

constructor TImageMng.Create;
begin
  inherited Create;

  fImages:= TList.Create;
  fExternalImagesManager:= nil;
  fImagesIDExported:= nil;

  if (RichEditVersion <= 4) then
     KeyOptions.ImgFormatInsideRTF:= ifWmetafile8;

  SetInitialValues;
end;


destructor TImageMng.Destroy;
var
   i: Integer;
begin
   Clear;

   fImages.Free;
   fNotOwnedStorage.Free;
   if fExternalImagesManager <> nil then
      fExternalImagesManager.Free;

   if fImagesIDExported  <> nil then
      fImagesIDExported.Free;

   inherited Destroy;
end;

procedure TImageMng.SetInitialValues;
begin
   fImagesMode:= imImage;
   fStorageMode:= smEmbRTF;
   fLastCleanUpImgStreams:= Now;
   fWasInformedOfStorageNotAvailable:= false;
   fNextImageID:= 1;
   fNextTempImageID:= 1;
   fExportingMode:= false;
   fChangingImagesStorage:= false;
   fFileIsNew:= true;
   fKntFile:= nil;
   fIntendedExternalStorageType:= issFolder;
   fIntendedStorageLocationPath:= '';
   fReconsiderImageDimensionsGoal:= false;
   fDoNotRegisterNewImages:= false;
end;


procedure TImageMng.Clear (SetIniValues: boolean= true; ClearImages: boolean = true);
var
  i: integer;
  Img: TKntImage;
begin
   if (fExternalStorageToSave <> fExternalStorageToRead) and (fExternalStorageToSave <> nil) then
      FreeAndNil(fExternalStorageToSave);

   fExternalStorageToSave:= nil;

   if assigned(fExternalStorageToRead) then
      FreeAndNil(fExternalStorageToRead);

   if clearImages then begin
      for i := 0 to fImages.Count-1 do begin
         Img:= TKntImage(fImages[i]);
         Img.FreeImageStream;
         Img.FID:= -1;
         Img.Free;
      end;
      fImages.Clear;
   end;

   if SetIniValues then
     SetInitialValues;
end;


procedure TImageMng.SetExportingMode(Value: boolean);
begin
  if fExportingMode = Value then exit;

  fExportingMode:= Value;

  if fExportingMode then
     fImagesIDExported:= TList.Create
  else
    if fImagesIDExported  <> nil then
       fImagesIDExported.Free;

end;


//-----------------------------------------
//      SAVE KNT -> SAVE IMAGES AND STATE
//-----------------------------------------


procedure TImageMng.SerializeImagesDefinition(const tf: TTextFile);
var
   Img: TKntImage;
   i: integer;
begin

   { *1
   We will copy the definition of those images that at this point are still in the list even with ReferenceCount = 0.
   This must be because they could not be deleted from the external storage as it is not available.
   They will be removed from there, and also from the list of definitions, when they become available.
   }

   for i := 0 to fImages.Count-1 do begin
     Img:= TKntImage(fImages[i]);
     //if Img.ReferenceCount > 0 then      // *1

     if fExportingMode and (fImagesIDExported.IndexOf(Pointer(Img.ID)) < 0) then
        continue;

     tf.WriteLine(Img.ImageDefinition(), true);
   end;
end;


{ Serializes the images that must remain embedded in the KNT file }

procedure TImageMng.SerializeEmbeddedImages(const tf: TTextFile);
var
   Img: TKntImage;
   ImgStream: TMemoryStream;
   EncryptedStream: TMemoryStream;
   EncryptionInfo: TEncryptionInfo;
   i, NumImagesToBeSavedExternally: integer;
   OnlySelectedImages: boolean;       // Couldn't be saved externally (temporarily problem with storage -> MustBeSavedExternally) or must be saved embedded (IsEncrypted)
   onlyStr: string;
begin

{
  %EI
  EI=IDImage|FileName|size
  <--imagen en binario-->
  ##END_IMAGE##
  EI=IDImage|FileName|size
  ....
}
   NumImagesToBeSavedExternally:= 0;

   { We can keep in the image definition section some that need to be deleted, but that could not be deleted from its external storage.
     We will keep them there until we manage to delete them, but we should not keep them in embedded images, among other things because
     we will not have their Stream }
   // TODO: nº of retries

   OnlySelectedImages:= true;
   if ExportingMode or (fStorageMode = smEmbKNT) or (fStorageMode = smExternalAndEmbKNT)  then
      OnlySelectedImages:= false;


   EncryptionInfo:= TkntFile(KntFile).GetEncryptionInfo;
   EncryptedStream:= TMemoryStream.Create;
   try

       for i := 0 to fImages.Count-1 do begin
         Img:= TKntImage(fImages[i]);
         if not Img.FOwned then continue;
         if OnlySelectedImages and not (Img.MustBeSavedExternally or Img.IsEncrypted) then continue;
         if Img.ReferenceCount = 0 then continue;

         if fExportingMode and (fImagesIDExported.IndexOf(Pointer(Img.ID)) < 0) then
            continue;


         if Img.MustBeSavedExternally then
            Inc(NumImagesToBeSavedExternally);

         ImgStream:= Img.ImageStream;
         if ImgStream = nil then continue;     // It could be an unavailable image, and are exporting the file with File | Copy To..

         tf.WriteLine(Format('%s=%d|%s|%d', [_EmbeddedImage, Img.ID, Img.FileName, ImgStream.Size]), true);
         ImgStream.Position := 0;
         if Img.IsEncrypted and not Img.StreamIsEncrypted then begin
            EncryptionInfo.DataSize:= ImgStream.Size;
            TKntFile(KntFile).EncryptStream(EncryptionInfo, ImgStream, EncryptedStream);
            ImgStream:= EncryptedStream;
         end;

         tf.Write(ImgStream.Memory^, ImgStream.Size);
         tf.Write(_CRLF);
         tf.WriteLine(_END_OF_EMBEDDED_IMAGE, False);

         EncryptedStream.Clear;
       end;

       if (not ExportingMode) and (NumImagesToBeSavedExternally > 0) and not fWasInformedOfStorageNotAvailable then begin
          App.WarningPopup(GetRS(sImg05));
          fWasInformedOfStorageNotAvailable:= true;
       end;

   finally
       EncryptedStream.Free;
   end;
end;


procedure TImageMng.SaveState(const tf: TTextFile);
var
   MaxSavedImgID: integer;
   ModeUseExternalStorage: boolean;

   procedure UpdateMaxSavedImgID;
   var
      i: integer;
      Img: TKntImage;
   begin
      MaxSavedImgID:= 0;
      for i := 0 to Images.Count -1 do begin
         Img := TKntImage(Images[i]);
         if Img.ReferenceCount = 0 then continue;
         if Img.ID > MaxSavedImgID then
            MaxSavedImgID:= Img.ID;
      end;
   end;

begin
   if ExportingMode then begin
      SaveStateInExportingMode(tf);
      exit;
   end;

   if fStorageMode = smEmbRTF then exit;

   ModeUseExternalStorage:= (fStorageMode = smExternal) or (fStorageMode = smExternalAndEmbKNT);

   tf.WriteLine(_NF_StoragesDEF);
   tf.WriteLine(_NF_StorageMode + '=' + IntToStr(Ord(fStorageMode)));
   if ModeUseExternalStorage and (fExternalStorageToSave <> nil) then begin
      tf.WriteLine(fExternalStorageToSave.GetStorageDefinition(), true);
      MaxSavedImgID:= FExternalStorageToSave.Save (fSaveAllImagesToExtStorage);
      if MaxSavedImgID + 1 > fNextImageID then
         fNextImageID:= MaxSavedImgID + 1;
      fNextTempImageID:= fNextImageID;
   end
   else begin
      UpdateMaxSavedImgID;
      fNextImageID:= MaxSavedImgID + 1;
      fNextTempImageID:= fNextImageID;
   end;


   // Get the information to be saved about the images
   tf.WriteLine(_NF_ImagesDEF);
   tf.WriteLine(_NF_IDNextImage + '=' + IntToStr(fNextImageID));
   SerializeImagesDefinition(tf);

end;


procedure TImageMng.SaveEmbeddedImages(const tf: TTextFile);
begin
   if ExportingMode and (KeyOptions.ImgStorageModeOnExport <> smeEmbKNT) then exit;

   // Images that, due to some problem, could not be saved externally (as configured) will also be saved.
   tf.WriteLine(_NF_EmbeddedIMAGES);
   SerializeEmbeddedImages(tf);

   if not ExportingMode then begin
     fFileIsNew:= false;
     fIntendedStorageLocationPath:= '';
   end;
end;


procedure TImageMng.SaveStateInExportingMode(const tf: TTextFile);
var
   i: Integer;
   MaxSavedImgID: integer;
begin
   if not ExportingMode then exit;

   if KeyOptions.ImgStorageModeOnExport <> smeEmbKNT then exit;

   tf.WriteLine(_NF_StoragesDEF);
   tf.WriteLine(_NF_StorageMode + '=' + IntToStr(Ord(smEmbKNT)));

   // Get the information to be saved about the images
   tf.WriteLine(_NF_ImagesDEF);
   tf.WriteLine(_NF_IDNextImage + '=' + IntToStr(fNextImageID));
   SerializeImagesDefinition(tf);
end;



procedure TImageMng.DeleteOrphanImages();
var
  Img: TKntImage;
  i: integer;
  Strs: TStrings;
begin
   if ExportingMode then exit;

   // Delete the (previously) saved images with ReferenceCount=0

   Strs:= TStringList.Create;
   try
      for i := fImages.Count-1 downto 0 do begin
        Img:= TKntImage(fImages[i]);

        if (Img.ReferenceCount = 0) then begin
           if Img.MustBeSavedExternally then
              continue;    // It will be a temporary image, added and deleted before saving, it will be discarded from SaveState

           if Img.IsOwned and (StorageMode = smExternal) or (StorageMode = smExternalAndEmbKNT) then begin
              if KeyOptions.ImgUseRecycleBin and (fExternalStorageToSave.StorageType = stZip) then
                 TZipStorage(fExternalStorageToSave).AddToDeletedZip(Img.ImageStream, Img.Path, Img.Name);

               if fExternalStorageToSave.OpenForWrite then begin
                  fExternalStorageToSave.DeleteFile(Img.Path, Img.Name);
                  // Delete might fail if the file no longer exists, but we'll ignore it. OpenForWrite has worked, then storage is available
                  Strs.Add(FormatDateTime('dd/MMM/yyyy HH:nn - ', Now) + 'Deleted: ' + Img.ImageDefinition);
                  fImages.Delete(i);
                  Img.FreeImageStream;
                  Img.Free;
               end
               else
                  Img.FreeImageStream;
                  // We keep the image with ReferenceCount = 0 -> it will be deleted when external storage is available
           end
           else begin
              fImages.Delete(i);
              Img.FreeImageStream;
              Img.Free;
           end;
        end;
      end;

      if Strs.Count > 0 then
         fExternalStorageToSave.RegisterInLogFile (Strs);


   finally
      if assigned(fExternalStorageToSave) then
         fExternalStorageToSave.Close;

      Strs.Free;
   end;

end;


//-----------------------------------------
//      OPEN KNT -> LOAD IMAGES AND STATE
//-----------------------------------------

procedure TImageMng.LoadState(const tf: TTextFile; var FileExhausted: Boolean);
var
  s, key : AnsiString;
  ws: String;
  p, i: Integer;
  Error: Boolean;

  Path, Name, OriginalPath, FileName, Caption: String;
  Owned, MustBeSavedExternally: boolean;
  section: (sNone, sStoragesDef, sImagesDef, sEmbeddedImgs);

  Stream: TMemoryStream;
  Strs: TStrings;
  Storage: TFilesStorage;
  Img: TKntImage;
  ImageFormat: TImageFormat;
  Width, Height: integer;
  crc32: DWORD;
  IDImg: Integer;
  StorageType: TStorageType;
  Size: Integer;
  NumberOfInstances: Integer;
  MaxImageID: Integer;

begin
  section:= sStoragesDef;
  MaxImageID:= 0;
  FNextImageID:= -1;

  Strs:= TStringList.Create;

  try
    while ( not tf.eof()) do begin
       Error:= False;

       s:= tf.readln();

       if s = _NF_StoragesDEF then begin
          // Storages definition section begins
          section:= sStoragesDef;
          continue;
       end;
       if s = _NF_ImagesDEF then begin
          // Images definition section begins
          section:= sImagesDef;
          continue;
       end;
       if s = _NF_EmbeddedImages then begin
         // Embedded images section begins
         section:= sEmbeddedImgs;
         continue;
       end;
       if ( s = _NF_EOF ) then begin
         FileExhausted := true;
         break; // END OF FILE
       end;


       p := pos('=', s );
       if p <> 3 then continue;  // not a valid key=value format
       key := copy(s, 1, 2);
       delete(s, 1, 3);
       Strs.Clear;

       // Storages definition
       if section = sStoragesDef then begin
          if ( key = _NF_StorageMode ) then
             FStorageMode := TImagesStorageMode(StrToInt( s ))

          else
          if key = _StorageDEF then begin
            // <Store>: SD= Tipo (Zip/Folder)|Path
             try
               Storage:= nil;
               ws:= TryUTF8ToUnicodeString(s);
               SplitString(Strs, ws, SEP, false);

               StorageType:=  TStorageType(StrToInt(Strs[0]));
               case StorageType of
                 stZIP:         FExternalStorageToRead:= TZipStorage.Create(Strs[1], TKntFile(fKntFile).File_Path);
                 stFolder:      FExternalStorageToRead:= TFolderStorage.Create(Strs[1], TKntFile(fKntFile).File_Path);
               end;

               FExternalStorageToSave:= FExternalStorageToRead;

             except
                On E : Exception do
                  App.ErrorPopup(GetRS(sImg01) + ws);
             end;
            continue;
          end;
       end
       else
       // Images definition
       if section = sImagesDef then begin
          if ( key = _NF_IDNextImage ) then
             FNextImageID := StrToInt( s )

          else
          if key = _ImageDEF then begin
            {
              <IMG>: PD= IDImg|Path|Name|ImageFormat|Width|Height|crc32|OriginalPath|Owned|ReferenceCount|Caption|MustBeSavedExternally [|Encrypted]
            }
             try
               ws:= TryUTF8ToUnicodeString(s);
               SplitString(Strs, ws, SEP, false);


               IDImg:= StrToInt(Strs[0]);
         		   Path:= Strs[1];
			         Name:= Strs[2];
               ImageFormat:= TImageFormat(StrToInt(Strs[3]));
               Width:=  StrToInt(Strs[4]);
               Height:= StrToInt(Strs[5]);
               crc32:= StrToUInt(Strs[6]);
       			   OriginalPath:= Strs[7];
               Owned:= Strs[8] = BOOLEANSTR[true];
			         NumberOfInstances:= StrToInt(Strs[9]);
               Caption:= Strs[10];
               MustBeSavedExternally:= Strs[11] = BOOLEANSTR[true];

               Img:= TKntImage.Create(IDImg, OriginalPath, Owned, ImageFormat, Width, Height, crc32, NumberOfInstances, nil);
               Img.FPath:= Path;
               Img.FName:= Name;
               Img.FCaption:= Caption;
               Img.FMustBeSavedExternally:= MustBeSavedExternally;
               Img.FIsEncrypted:= False;
               if (Strs.Count > 12) and (Strs[12] = BOOLEANSTR[true]) then begin
                  Img.FIsEncrypted:= True;
                  Img.FStreamIsEncrypted:= True;
               end;

               FImages.Add(Pointer(Img));

               MaxImageID:= Max(MaxImageID, IDImg);

             except
                On E : Exception do
                  App.ErrorPopup(E, GetRS(sImg02) + ws);
             end;
            continue;
          end;
       end
       else
       // Embedded Images
       if section = sEmbeddedImgs then begin
          if key = _EmbeddedImage then begin
           {
            EI=IDImg|FileName|size
            <-- content image in binary-->
            ##END_IMAGE##
           }

             Stream:= TMemoryStream.Create;
             try
               ws:= TryUTF8ToUnicodeString(s);
               SplitString(Strs, ws, SEP, false);

               IDImg:= StrToInt(Strs[0]);
               FileName:= Strs[1];
			         Size:= StrToInt(Strs[2]);
               if tf.ReadToStream(Stream, Size) < Size then
                  Error:= True

               else begin
                  Img:= GetImageFromID(IDImg, false);
                  if Img <> nil then
                     Img.SetImageStream (Stream);
                  // else: the image is not referenced. We can ignore it. This must be an error  // ToDO: Save to some file and notify?

                  s:= tf.Readln;
                  s:= tf.Readln;
                  if s <> _END_OF_EMBEDDED_IMAGE then
                     Error:= True;
               end;

             except
                On E : Exception do begin
                  Error:= True;
                  App.ErrorPopup(E, GetRS(sImg03) + ws);
                end;
             end;

             if Error then
                App.ErrorPopup(GetRS(sImg03) + ws);

            continue;
          end;
       end;

    end;

  finally
     fNotOwnedStorage:= TFolderStorage.Create('', TKntFile(fKntFile).File_Path);
     fNotOwnedStorage.fNotOwned:= True;

     if assigned(Strs) then
        Strs.Free;

     if (FNextImageID < 0) or (FNextImageID <= MaxImageID) then
        FNextImageID:= MaxImageID + 1;
     FNextTempImageID:= FNextImageID;
     fFileIsNew:= false;
  end;
end;


//-----------------------------------------
//    CHANGE/SET - STORAGE MODE
//-----------------------------------------

procedure TImageMng.SetInitialImagesStorageMode(StorageMode: TImagesStorageMode; ExternalStorageType: TImagesExternalStorage);
begin
  fStorageMode:= StorageMode;
  fIntendedExternalStorageType:= ExternalStorageType;
end;


function TImageMng.PrepareImagesStorageToSave (const FN: string): boolean;
var
  KntFilePath: string;
begin
   if not fFileIsNew then exit (true);

   KntFilePath:= ExtractFilePath(FN);
   fNotOwnedStorage:= TFolderStorage.Create('', KntFilePath);
   fNotOwnedStorage.fNotOwned:= True;

   if (fIntendedStorageLocationPath = '') then
      fIntendedStorageLocationPath:= GetDefaultExternalLocation(fIntendedExternalStorageType, FN);

   Result:= SetImagesStorage(fStorageMode, fIntendedExternalStorageType, fIntendedStorageLocationPath,
                             KntFilePath,
                             not (fStorageMode in [smEmbRTF, smEmbKNT]));
end;


function TImageMng.SetImagesStorage(StorageMode: TImagesStorageMode; ExternalStorageType: TImagesExternalStorage;
                                    Path: string;
                                    KntFilePath: string;
                                    CreateExternalStorageOnNewFile: boolean= false;
                                    ExternalStorageRelocated: boolean= false): boolean;
var
   AbsolutePath: String;
   CurrentStorageMode: TImagesStorageMode;
   ZipPathFormat: boolean;
   ok, ModifyPathFormat, OnlyRelocated, CreateExternalStorage: boolean;
   ExternalStoreTypeChanged: boolean;


   function CheckNewExternalStorage: boolean;
   begin
      Result:= false;

      if not fFileIsNew then begin
        if (fExternalStorageToRead <> nil) and not fExternalStorageToRead.IsValid then begin
           App.WarningPopup(Format(GetRS(sImg14), [AbsolutePath]));
           exit;
        end;
      end;

      if ExternalStorageType = issFolder then begin
         if (TDirectory.Exists(AbsolutePath) and not TDirectory.IsEmpty(AbsolutePath)) or FileExists(AbsolutePath) then begin
            App.WarningPopup(Format(GetRS(sImg07), [AbsolutePath]));
            exit;
         end;
      end
      else begin
         if TFile.Exists(AbsolutePath) then begin
            App.WarningPopup(Format(GetRS(sImg08), [AbsolutePath]));
            exit;
         end;
      end;
      CreateExternalStorage:= true;
      Result:= true;
   end;

   function GetStorageType(extStorageType: TImagesExternalStorage): TStorageType;
   begin
        case extStorageType of
           issZip: Result:= stZip;
           issFolder: Result:= stFolder;
        end;
   end;

   function CheckRelocatedStorage: boolean;
   begin
      Result:= true;

      if ExternalStorageType = issFolder then begin
         if not TDirectory.Exists(AbsolutePath) or TDirectory.IsEmpty(AbsolutePath) then begin
            App.WarningPopup(Format(GetRS(sImg11), [AbsolutePath]));
            Result:= false;
         end;
      end
      else begin
         if not TZipStorage.IsValidZip(AbsolutePath) then begin
            App.WarningPopup(Format(GetRS(sImg12), [AbsolutePath]));
            Result:= false;
         end;
      end;
   end;

   procedure CreateNewExternalStorage;
   begin
      if ExternalStorageType = issFolder then begin
         fExternalStorageToSave:= TFolderStorage.Create(Path, KntFilePath);
         TDirectory.CreateDirectory(AbsolutePath);
      end
      else begin
         fExternalStorageToSave:= TZipStorage.Create(Path, KntFilePath);
         TZipStorage(fExternalStorageToSave).CreateImagesZipFile;
      end;
      fSaveAllImagesToExtStorage:= true;
   end;

   procedure RefreshEditorInAllNotes;
   var
     i: integer;
     myFolder: TKntFolder;
   begin
     // In case they contain images that could not be located because the storage was moved

      for i := 0 to TKntFile(fKntFile).Folders.Count -1 do begin
         myFolder := TKntFile(fKntFile).Folders[i];
         if not myFolder.Editor.PlainText then begin
           myFolder.ReloadEditorFromDataModel(True);   // If editor was modified it will be saved first
         end;
      end;
   end;

begin
   Result:= false;
   if fChangingImagesStorage and (not fFileIsNew) then exit;

   try
      ModifyPathFormat:= false;
      fSaveAllImagesToExtStorage:= false;
      OnlyRelocated:= false;
      CreateExternalStorage:= false;
      ExternalStoreTypeChanged:= (fExternalStorageToRead <> nil) and (fExternalStorageToRead.StorageType <> GetStorageType(ExternalStorageType));
      ok:= true;
      Path:= ExtractRelativePath(KntFilePath, Path);
      AbsolutePath:= GetAbsolutePath(KntFilePath, Path);
      if ExternalStorageType = issFolder then
         AbsolutePath:= AbsolutePath + '\';             // To be able to compare with fExternalStorageToRead.AbsolutePath

      if not fFileIsNew then begin                      // The storage mode of a previously saved file is being changed

         if fStorageMode = StorageMode then begin
            case StorageMode of
               smEmbRTF, smEmbKNT: exit;                // Nothing to do

               smExternal, smExternalAndEmbKNT: begin
                 if (not ExternalStoreTypeChanged) and ( (fExternalStorageToRead.AbsolutePath = AbsolutePath) or ExternalStorageRelocated ) then begin
                     if (fExternalStorageToRead.AbsolutePath = AbsolutePath) or (not CheckRelocatedStorage) then
                        exit
                     else begin
                        fExternalStorageToRead.Relocate(Path);
                        OnlyRelocated:= true;
                     end;
                 end
                 else begin
                    ok:= CheckNewExternalStorage;
                    ModifyPathFormat:= ExternalStoreTypeChanged;
                 end;
               end;
            end;

         end
         else begin
            case StorageMode of
               smEmbRTF, smEmbKNT: begin
                  ModifyPathFormat:= (fStorageMode in [smExternal, smExternalAndEmbKNT]) and (fExternalStorageToRead.StorageType = stZip);
                  if (not (fStorageMode in [smEmbKNT, smExternalAndEmbKNT])) and ExternalStorageIsMissing then begin
                     App.WarningPopup(Format(GetRS(sImg14), [AbsolutePath]));
                     exit;
                  end;
               end;

               smExternal, smExternalAndEmbKNT:
                   if ((StorageMode = smExternal) and (fStorageMode = smExternalAndEmbKNT)) or
                      ((StorageMode = smExternalAndEmbKNT) and (fStorageMode = smExternal)) then begin
                      // It was External + [EmbKNT] and it is still External + [EmbKNT]. Has incorporated or removed the use of embKNT
                      if ExternalStorageIsMissing then begin
                         App.WarningPopup(Format(GetRS(sImg14), [AbsolutePath]));
                         exit;
                      end;
                      ModifyPathFormat:= ((ExternalStoreTypeChanged) or (ExternalStorageType = issZip));
                      if (ExternalStoreTypeChanged) or (fExternalStorageToRead.AbsolutePath <> AbsolutePath) then begin  // Has it changed location and/or format (zip/folder)?
                         if (not ExternalStoreTypeChanged) and ExternalStorageRelocated then begin
                            ok:= CheckRelocatedStorage;
                            if ok then
                               fExternalStorageToRead.Relocate(Path);
                         end
                         else
                            ok:= CheckNewExternalStorage;              // => fExternalStorageToRead <> fExternalStorageToSave
                      end;
                   end
                   else begin
                      // Before it did not include External and now it does
                      ok:= CheckNewExternalStorage;
                      if ok then begin
                         fExternalStorageToRead:= fExternalStorageToSave;
                         ModifyPathFormat:= (ExternalStorageType = issZip);
                      end;
                   end;
            end;

         end;
      end
      else begin
         if not CreateExternalStorageOnNewFile then begin
            assert(fFileIsNew);
            fIntendedExternalStorageType:= ExternalStorageType;
            fIntendedStorageLocationPath:= Path;
         end
         else begin
            case StorageMode of
               smEmbRTF, smEmbKNT:;

               smExternal, smExternalAndEmbKNT: begin
                   ok:= CheckNewExternalStorage;
                   if ok then begin
                      fExternalStorageToRead:= fExternalStorageToSave;
                      ModifyPathFormat:= (ExternalStorageType = issZip);
                   end;
               end;
            end;
         end;

      end;

      if not ok then exit;
      if (not fFileIsNew) and not OnlyRelocated and (App.DoMessageBox(GetRS(sImg13), mtConfirmation, [mbOK,mbCancel]) <> mrOK) then
         exit;


      Result:= true;

      if fFileIsNew and (fStorageMode= StorageMode) and (not CreateExternalStorageOnNewFile)  then
         exit;

      fStorageMode:= StorageMode;

      if OnlyRelocated then begin
         RefreshEditorInAllNotes;
         exit;
      end;


      fChangingImagesStorage:= true;
      screen.Cursor := crHourGlass;
      try
         if CreateExternalStorage then
            CreateNewExternalStorage;

         TKntFile(fKntFile).UpdateImagesStorageModeInFile (fStorageMode);


         if ModifyPathFormat then begin
            ZipPathFormat:= false;
            if assigned(fExternalStorageToSave) and (fExternalStorageToSave.StorageType= stZip) then
               ZipPathFormat:= true;

            AdaptPathFormatInImages(ZipPathFormat);
         end;

      finally
         screen.Cursor := crDefault;
      end;

      if not fFileIsNew then
         App.InfoPopup(Format(GetRS(sImg15), [fImages.Count]));

  except on E: Exception do
     App.ErrorPopup(E, GetRS(sImg16));
  end;
end;


procedure TImageMng.ConversionStorageMode_End();
begin
   case fStorageMode of
      smExternal, smExternalAndEmbKNT:
         if fExternalStorageToSave <> fExternalStorageToRead then begin
            fExternalStorageToRead.Free;
            fExternalStorageToRead:= fExternalStorageToSave;
         end;

     smEmbRTF:  Clear (false, true);
     smEmbKNT:  Clear (false, false);
   end;

   fChangingImagesStorage:= false;
   fSaveAllImagesToExtStorage:= false;
end;


//-----------------------------------------
//      STORAGE MANAGEMENT
//-----------------------------------------

function TImageMng.GetExternalStorageType: TImagesExternalStorage;
begin
 Result:= KeyOptions.ImgDefaultExternalStorage;

 if (fExternalStorageToSave <> nil) then begin
   case fExternalStorageToSave.StorageType of
       stFolder: Result:= issFolder;
       stZip:    Result:= issZIP;
   end;
 end
 else
    Result:= fIntendedExternalStorageType;

end;


function TImageMng.GetExternalStorageLocation: string;
begin
 Result:= '';

 if (fExternalStorageToSave <> nil) then
    Result:= fExternalStorageToSave.AbsolutePath
 else
    Result:= fIntendedStorageLocationPath;

 if (Result.Length > 1) and (Result[Result.Length] = '\') then
    Result:= Copy(Result, 1, Result.Length-1);
end;


function TImageMng.GetExternalStorageIsMissing: boolean;
begin
   Result:=  (fExternalStorageToRead <> nil) and not fExternalStorageToRead.IsValid;
end;


function TImageMng.GetDefaultExternalLocation (ExtType: TImagesExternalStorage; FN: string= ''): string;
begin
  if (FN = '') and (fKntFile <> nil) then
     FN:= TKntFile(fKntFile).FileName;

  Result:= ExtractFilePath(FN) + ExtractFileNameNoExt(FN);
  if Result = '' then
     Result:= '<File path>\<FileName>';   // Fake path

  case ExtType of
    issZip:    Result:= Result + '_img.zip';
    issFolder: Result:= Result + '_img';
  end;

end;


//-----------------------------------------
//      IMAGES MANAGEMENT
//-----------------------------------------


function TImageMng.GetNewID(): Integer;
begin
   Result:= fNextTempImageID;
   Inc(fNextTempImageID);
end;


function TImageMng.GetImageFromStream (Stream: TMemoryStream; var CRC32: DWORD; SetLastAccess: boolean= true): TKntImage;
var
  Img: TKntImage;
  i: integer;
begin
   CalculateCRC32 (Stream.Memory, Stream.Size, CRC32);

   for i := fImages.Count-1 downto 0 do begin
     Img:= TKntImage(fImages[i]);

     if Img.CRC32 = CRC32 then begin
        if SetLastAccess then Img.SetAccessed;
        exit(Img);
     end;
   end;

   Result:= nil;
end;


function TImageMng.GetMaxSavedImageID: integer;
var
  Img: TKntImage;
  i, MaxID: integer;
begin
   MaxID:= 0;
   for i := fImages.Count-1 downto 0 do begin
     Img:= TKntImage(fImages[i]);
     if Img.ID > MaxID then
        MaxID:= Img.ID;
   end;
   Result:= MaxID;
end;


function TImageMng.GetImageFromID (ImgID: integer; SetLastAccess: boolean= true): TKntImage;
var
  Img: TKntImage;
  i: integer;
begin
   Result:= nil;
   for i := fImages.Count-1 downto 0 do begin
     Img:= TKntImage(fImages[i]);

     if Img.ID = ImgID then begin
        if SetLastAccess then Img.SetAccessed;
        exit(Img);
     end;
   end;
end;


function TImageMng.GetNextImage (ImgID: integer; SetLastAccess: boolean= true): TKntImage;
var
  Img: TKntImage;
begin
   repeat
      Inc(ImgID);
      Img:= GetImageFromID (ImgID);
   until (Img <> nil) or (ImgID >= fNextTempImageID);
   Result:= Img;
end;

function TImageMng.GetPrevImage (ImgID: integer; SetLastAccess: boolean= true): TKntImage;
var
  Img: TKntImage;
begin
   repeat
      Dec(ImgID);
      Img:= GetImageFromID (ImgID);
   until (Img <> nil) or (ImgID <= 1);
   Result:= Img;
end;


function TImageMng.GetImageFromFileName (const FileName: string; SetLastAccess: boolean= true): TKntImage;
var
  Img: TKntImage;
  i: integer;
begin
   Result:= nil;
   for i := fImages.Count-1 downto 0 do begin
     Img:= TKntImage(fImages[i]);

     if String.Compare(Img.FileName, FileName, True) = 0 then begin
        if SetLastAccess then Img.SetAccessed;
        exit(Img);
     end;
   end;
end;


function TImageMng.GetImagePath (Img: TKntImage): string;
var
   Strg: TFilesStorage;

begin
    Result:= '';

    if Img.IsOwned then
       Strg:= fExternalStorageToRead
    else
       Strg:= fNotOwnedStorage;

    if not assigned(Strg) then exit;

    if (Strg.StorageType <> stFolder) then
       Result:= Strg.AbsolutePath
    else
       Result:= TFolderStorage(Strg).GetImagePath(Img.Path, Img.Name);
end;


procedure TImageMng.AdaptPathFormatInImages (ZipPathFormat: boolean);
var
  Img: TKntImage;
  i: integer;
  Src, Dst: Char;
begin
   if ZipPathFormat then begin
      Src:= '\';
      Dst:= '/';
   end
   else begin
      Src:= '/';
      Dst:= '\';
   end;

   for i := 0 to fImages.Count-1 do begin
     Img:= TKntImage(fImages[i]);
     if not Img.FOwned then continue;

     Img.FPath := StringReplace(Img.FPath, Src, Dst, [rfReplaceAll]);
   end;
end;



procedure TImageMng.ReloadImageStream (Img: TKntImage);
var
   i: integer;
   Strg: TFilesStorage;
   Stream: TMemoryStream;
begin
     if fExternalImagesManager <> nil then begin
        fExternalImagesManager.ReloadImageStream(Img);
        exit;
     end;


    if Img.IsOwned then
       Strg:= fExternalStorageToRead
    else
       Strg:= fNotOwnedStorage;

    if not assigned(Strg) then exit;

    Strg.OpenForRead;                                  // If it's already open, it won't open it again
    Stream:= Strg.GetImageStream(Img.Path, Img.Name);
    if Stream <> nil then begin
       Img.SetImageStream(Stream);
       Img.SetAccessed;
       exit;
    end;
end;

procedure TImageMng.CheckFreeImageStreamsNotRecentlyUsed;
var
  Img: TKntImage;
  i: integer;
  T: TDateTime;
begin

   if (fStorageMode <> smExternal) or (ExternalStorageIsMissing) then exit;

   T:= IncMinute(Now, -NUM_MIN_TO_FREE_ImageStreamsNotRecentlyUsed);
   if fLastCleanUpImgStreams > T then exit;

   Log_StoreTick('CheckFreeImageStreamsNotRecentlyUsed - BEGIN', 4, +1);

   try
      for i := 0 to fImages.Count-1 do begin
         Img:= TKntImage(fImages[i]);
         if Img.MustBeSavedExternally then continue;
         if Img.LastAccessed >= T then continue;
         if ImageInCurrentEditors (Img.ID) then
             continue;

         Img.FreeImageStream;
      end;

      Log_StoreTick('CheckFreeImageStreamsNotRecentlyUsed - END', 4, -1);

   finally
      fLastCleanUpImgStreams:= Now;
   end;

end;

procedure TImageMng.FreeImage (Img: TKntImage);
begin
   if assigned(Img) and (Img.ID = 0) then
      Img.FreeImageStream;
end;


function TImageMng.RecalcNextID: boolean;
var
   NewID: integer;
begin
   Result:= false;
   if (fStorageMode = smEmbRTF) or TKntFile(fKntFile).Modified then exit;

   NewID:= GetMaxSavedImageID + 1;
   if NewID <> fNextImageID then begin
      fNextImageID:= NewID;
      fNextTempImageID:= fNextImageID;
      TKntFile(fKntFile).Modified:= True;
      Result:= true;
   end;

end;

function TImageMng.ImageInCurrentEditors (ImgID: integer; UseFreshTextPlain: boolean= false): Boolean;
var
   i, j: integer;
   ImagesIDs: TImageIDs;
begin
    for i := 0 to TKntFile(fKntFile).FolderCount -1 do begin
       if UseFreshTextPlain then
          ImagesIDs:= GetImagesIDInstancesFromTextPlain (TKntFile(fKntFile).Folders[i].Editor.TextPlain)
       else
          ImagesIDs:= TKntFile(fKntFile).Folders[i].ImagesInstances;

       for j := Low(ImagesIDs) to High(ImagesIDs) do begin
           if ImagesIDs[j] = ImgID then
               exit(true);
       end;
    end;
    Result:= false;
end;


//-----------------------------------------
//      IMAGES - Adding NEW images
//-----------------------------------------

function TImageMng.RegisterNewImage(
                                         Stream: TMemoryStream;
                                         ImageFormat: TImageFormat;
                                         Width, Height: integer;
                                         crc32: DWORD;
                                         const OriginalPath: String;
                                         Owned: boolean;
                                         const Source: String;
                                         const FolderName: string;
                                         const NameProposed: string = ''
                                         ): TKntImage;
var
   Img: TKntImage;
   Path: string;
   ImgID: integer;
   ZipPathFormat: boolean;
   StreamIMG: TMemoryStream;

begin
   ImgID:= GetNewID();

   if not Owned and KeyOptions.ImgLinkRelativePath then
      Path:= ExtractRelativePath(TKntFile(fKntFile).File_Path, OriginalPath)
   else
      Path:= OriginalPath;

   if fExternalImagesManager <> nil then begin
      StreamIMG:= TMemoryStream.Create;
      StreamIMG.LoadFromStream(Stream);
   end
   else
      StreamIMG:= Stream;

   Img:= TKntImage.Create(ImgID, Path, Owned, ImageFormat, Width, Height, crc32, 0, StreamIMG);

   ZipPathFormat:= false;
   if (assigned(fExternalStorageToSave)) and (fExternalStorageToSave.StorageType= stZip) then
      ZipPathFormat:= true;

   Img.GenerateName(FolderName, Source, ZipPathFormat, NameProposed);

   fImages.Add(Pointer(Img));
   CheckToMarkSaveExternally(Img);

   Result:= Img;
end;


procedure TImageMng.CheckToMarkSaveExternally(Img: TKntImage);
begin
   if Img.FOwned and ((fStorageMode = smExternal) or (fStorageMode = smExternalAndEmbKNT)) then
      Img.MustBeSavedExternally:= true;
end;


// Returns True if it has just been registered
function TImageMng.CheckRegisterImage (
                                           Stream: TMemoryStream;
                                           ImgFormat: TImageFormat;
                                           Width, Height: integer;
                                           const FolderName: string;
                                           const OriginalPath: String;
                                           Owned: boolean;
                                           const Source: String;
                                           var Img: TKntImage;
                                           const NameProposed: string = ''
                                           ): boolean;
var
  crc32: DWORD;
begin
    Result:= false;          // No need to register

    Img:= GetImageFromStream (Stream, crc32);
    if (Img = nil) and not fDoNotRegisterNewImages then begin
       Img:= RegisterNewImage(Stream, imgFormat, Width, Height, crc32, OriginalPath, Owned, Source, FolderName, NameProposed);
       Result:= True;        // Registered
    end;
end;


function TImageMng.CheckUniqueName (var Name: string): boolean;
var
   Img: TKntImage;
   n: integer;
   NameNoExt, NewName, Ext: string;
begin
   Result:= true;
   n:= 1;
   NameNoExt:= ExtractFileNameNoExt(Name);
   Ext:= ExtractFileExt(Name);
   NewName:= Name;
   repeat
      Img:= GetImageFromFileName(NewName, false);
      if Img <> nil then begin
          if (Img.ReferenceCount = 0) and (not ImageInCurrentEditors(Img.ID, true)) then begin
             Img.FName:= Format('%d_%s', [Img.ID, Img.FName]);
          end
          else begin
            inc(n);
            NewName:= Format('%s_%d%s', [NameNoExt, n, Ext]);
          end;
      end;
   until Img = nil;

   if n > 1 then begin
      Name:= NewName;
      Result:= false;
   end;
end;


{ Explicit insertion of an image. Replaces the current method that relies on the use of the clipboard }

procedure TImageMng.InsertImage(FileName: String; Editor: TKntRichEdit; Owned: boolean; const NameProposed: string= '');
var
   RTFFormImgIns: AnsiString;
begin
   RegisterAndInsertImage(FileName, Editor, Owned, RTFFormImgIns, NameProposed);
end;


procedure TImageMng.RegisterAndInsertImage(FileName: String; Editor: TKntRichEdit; Owned: boolean;
                                           var RTFForImageInsertion: AnsiString;
                                           const NameProposed: string= '';
                                           Stream: TMemoryStream = nil;
                                           AddToEditor: boolean = true;
                                           W: integer = 0; H: integer = 0);
var
  ImgFormat, ImgFormatDest: TImageFormat;
  Width, Height, WidthGoal, HeightGoal: integer;

  ImgID: integer;
  Img: TKntImage;
  CreateStream, StreamRegistered: boolean;
  Folder: TKntFolder;
  FolderName: String;

begin
  ImgID:= 0;
  ImgFormat:= imgUndefined;
  StreamRegistered:= False;

  Folder:= TKntFolder(Editor.FolderObj);
  FolderName:= '';
  if assigned(Folder) then
     FolderName:= Folder.Name;

  CreateStream:= (Stream = nil);

  if CreateStream then
     Stream:= TMemoryStream.Create;
  try
     if CreateStream then
        Stream.LoadFromFile(FileName);
     Stream.Position:= 0;

     if Editor.SupportsRegisteredImages then begin
        ImgFormat:= GetImageFormat(Stream);
        StreamRegistered:= CheckRegisterImage (Stream, ImgFormat, Width, Height, FolderName, FileName, Owned, '', Img, NameProposed);
        if Img <> nil then begin           //  DoNotRegisterNewImages coult it be = True and the image not included in the file
           ImgID:= Img.ID;
           if (not StreamRegistered) and (Img.ReferenceCount = 0) and (NameProposed <> '') then
              Img.FName:= NameProposed;
        end;
     end;

     if (Img <> nil) and (NameProposed <> '') and (Img.Caption = '') and (NameProposed <> ExtractFileName(FileName))  then
        Img.Caption:= NameProposed;

     Width:= -1;
     WidthGoal:= -1;
     if (W <> 0) and (H <> 0) then begin
        WidthGoal:= W;
        HeightGoal:= H;
     end;

     RTFForImageInsertion:= GetRTFforImageInsertion(ImgID, Stream, ImgFormat, Width, Height, WidthGoal, HeightGoal, AddToEditor, StreamRegistered, true, True);

     if StreamRegistered then
        Img.SetDimensions(Width, Height);

     if AddToEditor then
        Editor.PutRtfText(RTFForImageInsertion, True);

  finally
     if not StreamRegistered then
        Stream.Free;
  end;

end;


procedure TImageMng.InsertImageFromClipboard(Editor: TKntRichEdit; FolderName: string; TryAddCaption: boolean = true);
var
  Stream: TMemoryStream;
  bmp: TBitmap;
  ImgFormat, ImgFormatDest: TImageFormat;
  Img: TKntImage;
  ImgID: integer;

  Width, Height, WidthGoal, HeightGoal: integer;

  posOpening, posClosing, W,H: integer;
  HTMLText, SourceImg, StrRTF: AnsiString;
  TextAlt: String;
  DefaultImageFormat_Bak: TImageFormat;

  WasCopiedByKNT, StreamRegistered: boolean;
  Source: String;

begin
  // https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Pasting_Graphics_from_the_Clipboard
  ImgID:= 0;
  ImgFormat:= imgUndefined;
  StreamRegistered:= false;

  if TryAddCaption then begin
     HTMLText:= Clipboard.AsHTML;
     Clipboard.GetNextImageInformation(HTMLText,1, SourceImg, TextAlt, W, H, posOpening, posClosing);
  end;

  Stream:= TMemoryStream.Create;

  bmp:= TBitmap.Create;
  try
     bmp.Assign(Clipboard);
     bmp.PixelFormat:= KeyOptions.ImgBmpPixelFormat;
     bmp.SaveToStream(Stream);

     Width:=  bmp.Width;
     Height:= bmp.Height;

     Stream.Position:= 0;

     WasCopiedByKNT:= ClipboardContentWasCopiedByKNT;
     if WasCopiedByKNT then
        ImgID:= LastCopiedIDImage;            // If <> 0 => We have copied this image from KNT and we had already registered it

     if (ImgID = 0) and (fStorageMode <> smEmbRTF) then begin
        if ClipCapMng.IsBusy then
           Source:= 'ClipCap'
        else
           Source:= 'Clipb';

        ImgFormat:= imgBMP;
        ImgFormatDest:= KeyOptions.ImgDefaultFormatFromClipb;
        if SourceImg <> '' then begin                                         // Pasting from browser
           imgFormatDest:= imgJpg;
           Source := Source + 'WEB';
        end;

        ConvertStreamToImgFormatDest (Stream, ImgFormat, imgFormatDest);
        if Editor.SupportsRegisteredImages then begin
           ImgFormat:= ImgFormatDest;
           StreamRegistered:= CheckRegisterImage (Stream, ImgFormatDest, Width, Height, FolderName, '', True, Source, Img);
           if Img <> nil then            //  DoNotRegisterNewImages coult it be = True and the image not included in the file
              ImgID:=Img.ID;
        end;
     end;

     WidthGoal:= -1;

     StrRTF:= GetRTFforImageInsertion(ImgID, Stream, ImgFormat, Width, Height, WidthGoal, HeightGoal, True, StreamRegistered, WasCopiedByKNT);
     Editor.PutRtfText(StrRTF, True);

 {
     if TryAddURLLink and (SourceImg <> '') and (TextAlt<>'') then begin
         Editor.SelText:= #13;
         Editor.SelStart:= Editor.SelStart + 1;
         InsertURL(SourceImg, TextAlt, Folder);
         if (Img <> nil) and StreamRegistered then
            Img.Caption:= TextAlt;
     end;
}

     if TryAddCaption and (TextAlt <> '') and (Img <> nil) and (Img.Caption = '') then begin
         Img.Caption:= TextAlt;
     end;


  finally
     bmp.Free;
     if not StreamRegistered then
        Stream.Free;
  end;

end;



//------------------------------------------------------------------------------------------
//      IMAGES - Processing RTF - Conversion imLink / imImage - Registering new images
//------------------------------------------------------------------------------------------


{ Will only be called when it detects that the content on the clipboard was not copied by this application
 If it has been, there is no need to process the images, since they will already be adapted and with the
 hidden marks that have been precise
 * There are situations where it is also called when copied from KeyNote. See comments in TKntRichEdit.PasteBestAvailableFormat
}
function TImageMng.ProcessImagesInClipboard(Editor: TKntRichEdit; const FolderName: string; SelStartBeforePaste: integer; FirstImageID: integer= 0): boolean;
var
  SelStartBak: integer;
  p1, p2: integer;
  RTFText, RTFTextOut: AnsiString;
  ImagesMode: TImagesMode;

begin
    Result:= false;

    p1:= SelStartBeforePaste;
    p2:= Editor.SelStart;
    SelStartBak:= p2;
    if p2 = Editor.TextLength then
       Inc(p2);

    Editor.SetSelection(p1, p2, true);
    RTFText:= Editor.RtfSelText;

    if Editor.DoRegisterNewImages then
       ImagesMode:= FImagesMode              // Editor or floating editor in folder
    else
       ImagesMode:= imImage;                 // Scrapbook or floating editor in scrapbook

    RTFTextOut:= ProcessImagesInRTF(RTFText, FolderName, ImagesMode, 'Clipboard', FirstImageID);

    if RTFTextOut <> '' then begin
       Editor.PutRtfText(RTFTextOut,True,True);
       Result:= true;
    end
    else
       Editor.SetSelection(p2, p2, true);

end;


function TImageMng.ProcessImagesInRTF(const RTFText: AnsiString;
                                          const FolderName: String;
                                          ImagesModeDest: TImagesMode;
                                          const Source: string;
                                          FirstImageID: integer= 0;
                                          ExitIfAllImagesInSameModeDest: boolean= false
                                          ): AnsiString;
var
   ContainsImgIDsRemoved: boolean;
   ContainsImages: boolean;
begin
   if RTFText = '' then exit('');

   Result:= ProcessImagesInRTF(@RTFText[1], ByteLength(RTFText), FolderName, ImagesModeDest, Source, FirstImageID, ContainsImgIDsRemoved, ContainsImages, ExitIfAllImagesInSameModeDest);
end;



// ---- ProcessImagesInRTF --------------------

(* If no images are found, '' will be returned
  If ExitIfAllImagesInSameModeDest and all images, if any, are in the same format, too, it is returned ''
  In all cases it is always indicated in ContainsImages if there are images, whether or not they have had to be processed, and they are
  in imImage (Pict..) or imLink mode

NOTE:
 Consider the following situation:
 A registered jpg image, with dimensions of 1400x957 and displayed as 410x281, is deleted and the KNT file is saved.
 After that the user presses UNDO. This recovers on the RTF control the previous string, with its hidden image mark and the \pict string
 of the image. But the hidden mark references an image that no longer exists, so the image stream must be analyzed as a new one. And here
  the following happens:
 - The content of the jpg image, as seen in the hexadecimal string within \jpegblip, varies slightly in a few points of the beginning of
   the image (header of the same, surely) with respect to the one built by the application (which corresponds exactly to the content of
   the jpg image in the file --if it has been obtained from a file). This causes the CRC32 value to be different than that corresponding
   to the initial image and it is considered a different image. If we have deleted the registered image, we really no longer have it,
   but we could be processing this image due to an error (see below)   [*]

 - The image header tells us, through its labels \picw and \pich, values that do not correspond to the real size of the image, but are
   equivalent to those of the visible dimensions (\picwgoal and \pichgoal ). If we use these values we will be registering as 410x281
   an image that is really 1400x957, and thus we will show it in the internal viewer when we do it 100%.

 The above may have occurred previously as a consequence of errors not treated correctly. For example, in a situation like the following:
  \v\'11I7\'12\v0\par \v\'11I11\'12\v0\v\'11I12\'12\v0{\pict{....
 The valid tag is the one immediately before the image (->ImgID=2), but it has previously been used (incorrectly) the first one found
 before the \pict tag (->ImgID=7). If image 7 is not valid, the application will have had to generate a new ID from the stream contained
 in the image \pict, with the problem indicated above.

 Thus:
 - Not obtaining the dimensions from the RTF when we can (and should) handle the stream.
 - We cannot avoid the first case (the user saves the KNT file after deleting the image and before doing UNDO), but at least the new image
   that can be created will be equivalent, with the same dimensions. In any case, if the 'Use Recycle bin' option is active, can always
   recover and use the same image again, although it will be assigned a new ID.

 [*] The observed differences appear to be due to changes that the RichEdit control can make, which will remove the following information
     from the image, if it finds it:  Embedded color profile, EXIF metada, IPTC metadata

*)
function TImageMng.ProcessImagesInRTF(const Buffer: Pointer; BufSize: integer;
                                          const FolderName: string;
                                          ImagesModeDest: TImagesMode;
                                          const Source: string;
                                          FirstImageID: integer;
                                          var ContainsImgIDsRemoved: boolean;
                                          var ContainsImages: boolean;
                                          ExitIfAllImagesInSameModeDest: boolean = false
                                          ): AnsiString;
var
  SelStartBak: integer;
  pIn, pOut,  pPict, pLinkImg, pLinkImgFolded, pRTFImageEnd, pID,pIDr, pIDff, pIDcheck: integer;
  LinkImgFolded: boolean;
  pPatt1,pPatt2, pImgIni: integer;
  In_shppict: boolean;
  ImgRTF, RTFTextOut, ImgIDStr,StrAux: AnsiString;
  Stream: TMemoryStream;
  Width, Height, WidthGoal, HeightGoal: integer;
  ImgFormat, ImgFormatDest: TImageFormat;
  NBytes: integer;

  ImgID, ID: integer;
  Img: TKntImage;
  StreamRegistered, StreamNeeded, UseExtenalImagesManager, Owned: boolean;
  ImgIDwasPresent, GetStream, MaintainWMF_EMF: boolean;
  ImageMode: TImagesMode;
  ImgCaption: string;
  RTFText: PAnsiChar;
  i: integer;
  ImgIDsToRemove: TImageIDs;

const
   beginIDImg = KNT_RTF_HIDDEN_MARK_L + KNT_RTF_HIDDEN_IMAGE;         // \'11I
   endIDImg = KNT_RTF_HIDDEN_MARK_R;                                  // \'12
   Lb = Length(beginIDImg);


   procedure CheckHiddenMarkInLink;
   var
     k: integer;
   begin
     { *1
      We are going to control a rare case: from imLink mode the user separated two images that did not have any separating
      character and that therefore appeared joined (although not internally). Depending on how you have positioned the cursor,
      it may be the case that white spaces are being interspersed between our hidden label and the hyperlink. (You could also
      enter other characters at that same point, but that is rarer and we will not control it. }
      k:= 1;
      while RTFTextOut[pOut-k] = ' ' do
          inc(k);
      pOut:= pOut-k + 1;
   end;


   procedure CheckHiddenMarkInImage;
   var
      k: integer;
      antBracket: boolean;
   begin
     // Rare case control but that could occur. See *1, above
      k:= 1;
      antBracket:= RTFTextOut[pOut-1] = '{';
      if antBracket then k:= 2;
      while RTFTextOut[pOut-k] = ' ' do
          inc(k);
      pOut:= pOut-k + 1;
      if antBracket then begin
         RTFTextOut[pOut]:= '{';
         inc(pOut);
      end;
   end;


begin
 {$IFDEF KNT_DEBUG}
  Log_StoreTick('ProcessImagesInRTF - BEGIN', 4, +1);
  try
 {$ENDIF}

{
*2 Using PosPAnsiChar() instead of Pos()
    Note, it is important to use the PosPAnsiChar method, added to gf_streams and built from System.AnsiStrings.StrPos, compared to using
    Pos(..) in these cases in which the search is performed on a null-terminated string ( *)
    Using Pos(...) which expects to receive a string (either UnicodeString, AnsiString, RawByteString or ShortString), was causing each call
    to this method to do a conversion of the entire null-terminated string to a unicode string, over which then carried out the search. In
    addition to being inefficient, it could cause Out of memory when converting EmbeddedRTF files with very large nodes (with many images
    on the same node)
    From the tests done, and receiving PosPAnsiChar two PAnsiChar as parameters (in addition to an optional integer), the following calls
    would be equivalent, it is not necessary to cast the substring, the compiler seems to interpret it correctly:
     pPict:= PosPAnsiChar('{\pict{', RTFText, pIn) -1;
     pPict:= PosPAnsiChar(AnsiString('{\pict{'), RTFText, pIn) -1;

   The same does not seem to happen in calls like the following, where TextPlain is of type AnsiString. The first ends up causing a conversion
   of both strings to UnicodeString before performing the search, the second does not.

    const
       literal = 'substr';
    Pos(Literal, TextPlain, pID);
    Pos(AnsiString(Literal), TextPlain, pID);

   The first call could be used with the same behavior as the second if we define the constant of the form:
    const
       literal = AnsiString('substr');

   (*) Starting in this case from the content of a TMemoryStream, in which we have ensured that it ends in #0
       (See for example comment *1 in TKntFolder.CheckSavingImagesOnMode (kn_KntFolder)
}

   ContainsImages:= false;

   StreamRegistered:= false;
   pRTFImageEnd:= -1;
   pIn:= 1;
   pOut:= 1;
   pPict:= -99;
   pLinkImg:= -99;
   pLinkImgFolded:= -99;
   pPatt1:= -99;
   pPatt2:= -99;
   In_shppict:= false;


   Result:= '';
   if BufSize = 0 then exit;

   RTFText:= PAnsiChar(Buffer);

   if (Length(RTFText) > BufSize) then begin
      assert(Length(RTFText) <= BufSize );
      exit;
   end;

   // fExtenalImagesManager <> nil : We are processing folder images from a file we are merging (see MergeFromKNTFile)

   UseExtenalImagesManager:= ((ImagesModeDest = imLink) or (fStorageMode = smEmbRTF)) and (fExternalImagesManager <> nil);
   StreamNeeded:=  (ImagesModeDest = imImage) or (fExternalImagesManager <> nil);


   if ExitIfAllImagesInSameModeDest then begin
      if fStorageMode <> smEmbRTF then        // If = smEmbRTF =>  fChangingImagesStorage=True
         MaintainWMF_EMF:= true;
      pPict:= PosPAnsiChar('{\pict{', RTFText, pIn) -1;                             // *2
      pLinkImg:= PosPAnsiChar(KNT_IMG_LINK_PREFIX, RTFText, pIn) -1;
      pLinkImgFolded:= PosPAnsiChar(KNT_IMG_LINK_FOLDED_PREFIX, RTFText, pIn) -1;
      if (pPict >= 0) or (pLinkImg >= 0) or (pLinkImgFolded >= 0) then
          ContainsImages:= true;

      if ImagesModeDest = imLink then begin
         if pPict = -1 then exit
      end
      else begin
         if (pLinkImg = -1) and (pLinkImgFolded = -1) then exit
      end;
   end;


   Stream:= TMemoryStream.Create;
   try
      try
         RTFTextOut:= '';
   (*
         RTFText:= '{\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang3082{\fonttbl{\f0\fnil\fcharset0 Calibri;}} ' + #13#10 +
                   '{\*\generator Riched20 10.0.19041}\viewkind4\uc1' + #13#10 +
                   '\pard\sa200\sl276\slmult1\f0\fs22\lang10{\object\objemb{\*\objclass Excel.Sheet.12}\objw2400\objh735{\*\objdata ' + #13#10 +
                   '0105000011111111111111111' + #13#10 +
                   '}{\result{\pict{\*\picprop}\wmetafile8\picw2400\pich735\picwgoal2400\pichgoal735 '+ #13#10 +
                   '010009000003db08000006003806000000003806000026060f00660c574d464301000000000001'+ #13#10 +
                   '}}}\par IMAGEN1: '+ #13#10 +
                   '{\pict{\*\picprop}\wmetafile8\picw130\pich999\picwgoal4444\pichgoal735 '+ #13#10 +
                   '010009000003db08000006003806000000003806000026060f00660c574d464301000000000001'+ #13#10 +
                   '}\par IMAGEN EMF+WMF:'+ #13#10 +
                   '{\*\shppict{\pict{\*\picprop}\emfblip\picw130\pich999\picwgoal4444\pichgoal735 '+ #13#10 +
                   '010009000003db08000006003806000000003806000026060f00660c574d464301000000000001'+ #13#10 +
                   '}}{\*\nonshppict{\pict{\*\picprop}\wmetafile8\picw130\pich999\picwgoal4444\pichgoal735 '+ #13#10 +
                   '010009000003db08000006003806000000003806000026060f00660c574d464301000000000001'+ #13#10 +
                   '}}\par '+ #13#10 +
                   'BYE }';
          pIn:= 1;

        \v\'11I999999\'12\v0    -> 20 max (If necessary we can increase it)

        The hidden mark next to the image, seen in its RTF form, can be altered from how it was added:

          \v\'11I333\'12\v0{\*\shppict{\pict{\*\picprop}\emfblip
          \v\f0\fs20\lang1033\'11I333\'12\v0
          \pard\cf1\v\f0\fs20\lang1033\'11I333\'12\v0{\pict{...}\pngblip\
          \v\'11I333\'12\cf1\v0\lang1033{\pict ...

        But it seems that in all cases they are always necessarily kept together with the characters that make up the content.
        of the marker (\'11I333\'12 in example). What can happen is that other control characters are introduced within the
        hidden mark control characters.
        ...

           \v\'11I222\'12{\v0{\field{\*\fldinst{HYPERLINK ...
           \v\'11I-3\'12\v0 a{{\field{\*\fldinst{HYPERLINK "img:-
           {\cf0{\field{\*\fldinst{HYPERLINK "file: ...


       Note:
       When RTF content with images is saved to disk in imImage mode, the conversion made from imImage to imLink is
       the one saved. In this case the RTF constructed in this procedure is the saved one.
       An image will be saved in the form:

         \v\'11I9\'12\v0{\field{\*\fldinst{HYPERLINK "img:9,482,409"}}{\fldrslt{MAIN_NOTE\\9_sample.wmf}}}

       Notes/nodes edited in imLink mode (images not visible) will be saved exactly as those hyperlinks are interpreted by
       the editor. Thus, edited RTF expressions that include images will be saved as follows:

         \v\'11I9\'12{\v0{\field{\*\fldinst{HYPERLINK "img:9,482,409"}}{\fldrslt{MAIN_NOTE\\9_sample.wmf}}}}\v0

       Despite this alteration, subsequent conversion to imImage mode will be interpreted as expected:
         \v\'11I9\'12{\v0{\pict...}}\v0    ->    \v\'11I9\'12\v0{\pict..}

       (unedited notes/nodes are not altered)

   *)

         repeat
            if StreamRegistered then begin
               Stream:= TMemoryStream.Create;
               StreamRegistered:= False;
            end
            else
               Stream.Clear;

            Img:= nil;
            ImgID:= 0;
            ImgIDwasPresent:= false;
            ImgIDsToRemove:= nil;

            if pPict = -99 then
               pPict:= PosPAnsiChar('{\pict{', RTFText, pIn)-1;

            if pPict >= BufSize then
               pPict := -1;

            if pPict >= 0 then begin
                if (pPatt1 <> -1) and (pPatt1 < pPict) then begin
                   if pPatt1 < 0 then
                      pPatt1:= PosPAnsiChar('\result{\pict{', RTFText, pIn)-1;
                   if (pPatt1 >= 0) and (pPatt1 < pPict) then begin
                      pIn:= pPatt1 + Length('\result{\pict{');
                      pPatt1:= -99;       // We will go back to look for another one. We have already 'consumed' this one
                      continue;
                   end;
                end;
                // {\*\shppict{\pict{.... }}{\*\nonshppict{\pict{.... }}     Usually in emfblip
                if (pPatt2 <> -1) and (pPatt2 < pPict) then begin
                   if pPatt2 < 0 then
                      pPatt2:= PosPAnsiChar('{\*\shppict', RTFText, pIn)-1;
                   if (pPatt2 >= 0) and (pPatt2 < pPict) then begin
                      pPatt2:= -99;       // We will go back to look for another one. We have already 'consumed' this one
                      In_shppict:= true;
                   end;
                end;
            end;

            if (pLinkImg <> -1) and ((pLinkImg < pPict) or (pPict=-1)) then begin
               if pLinkImg < 0 then begin
                  pLinkImg:= PosPAnsiChar(KNT_IMG_LINK_PREFIX, RTFText, pIn)-1;
                  if pLinkImg >= BufSize then
                     pLinkImg := -1;
               end;
            end;

            if (pLinkImgFolded <> -1) and ((pLinkImgFolded < pPict) or (pPict=-1))
                                      and ((pLinkImgFolded < pLinkImg) or (pLinkImg=-1)) then begin
               if pLinkImgFolded < 0 then begin
                  pLinkImgFolded:= PosPAnsiChar(KNT_IMG_LINK_FOLDED_PREFIX, RTFText, pIn)-1;
                  if pLinkImgFolded >= BufSize then
                     pLinkImgFolded := -1;
               end;
            end;

            if (pPict = -1) and (pLinkImg = -1) and (pLinkImgFolded = -1) then
               break;



            if ((pLinkImg >= 0) and ((pLinkImg < pPict) or (pPict=-1)))
                 or  ((pLinkImgFolded >= 0) and ((pLinkImgFolded < pPict) or (pPict=-1)))  then begin
               ImageMode:= imLink;                            // The image is in link mode
               if (pLinkImgFolded >= 0) and ((pLinkImgFolded < pLinkImg) or (pLinkImg=-1)) then begin
                  pImgIni:= pLinkImgFolded;
                  LinkImgFolded:= True;
               end
               else begin
                  pImgIni:= pLinkImg;
                  LinkImgFolded:= False;
               end;
            end
            else begin
               ImageMode:= imImage;
               pImgIni:= pPict;
               LinkImgFolded:= False;
            end;
            NBytes:= pImgIni - pRTFImageEnd-1;                // Previous bytes to copy


            pID:= PosPAnsiChar(beginIDImg, RTFText, pIn);
            pIDff:= pID;                                                   // first found
            while (pID >= 1) and (pID < pImgIni) do begin
               pIDr:= PosPAnsiChar(endIDImg, RTFText, pID);                         // \v\'11I999999\'12\v0        pID: \'11I999999  pIDr: \'12      (Max-normal-: pIDr-pID=11) -> 12 ..
               if (pIDr >= 1) and ((pIDr-pID) <= 12) then begin
                  ImgIDStr:= CopyPAnsiChar(RTFText, pID + Lb, (pIDr - pID) -Lb);
                  if TryStrToInt(ImgIDStr, ImgID) then
                     ImgIDwasPresent:= true;

                  {  We have to make sure that this hidden mark does not correspond to a deleted or incorrectly inserted image
                    (see what is described in: https://github.com/dpradov/keynote-nf/issues/623#issuecomment-1824896077 }
                  pIDcheck:= PosPAnsiChar(beginIDImg, RTFText, pIDr);
                  if (pIDcheck >= 1) and (pIDcheck < pImgIni) then begin
                     if ImgIDwasPresent then begin
                        SetLength(ImgIDsToRemove, Length(ImgIDsToRemove) + 1);
                        ImgIDsToRemove[Length(ImgIDsToRemove)-1]:= ImgID;
                        ImgIDwasPresent:= false;
                     end;
                     pID:= pIDcheck;
                  end
                  else
                     break;
               end;
            end;



            if (FirstImageID <> 0) and (ImgID = 0) then begin
               ImgID:= FirstImageID;
               FirstImageID:= 0;
            end;



            if RTFTextOut = '' then begin
               { If ImagesModeDest = imImage and the images come in Link mode, we will have to increase the length of the output string.
                 At the end we will adjust the length }
               SetLength(RTFTextOut, BufSize);
               {$IF Defined(DEBUG) AND Defined(KNT_DEBUG)}
               for var k: integer := 1 to BufSize do
                  RTFTextOut[k]:= ' ';  //ZeroMemory(@RTFTextOut[1], BufSize);
               {$ENDIF}
            end;

            // Copy bytes previous to the image or link found
            if In_shppict then
               Dec(NBytes, Length('{\*\shppict'));
            Move(RTFText[pRTFImageEnd+1], RTFTextOut[pOut], NBytes);
            Inc(pOut, NBytes);


            Owned:= true;
            GetStream:= true;
            if ImgID <> 0 then begin

                if UseExtenalImagesManager then
                   Img:= fExternalImagesManager.GetImageFromID (ImgID)
                else
                   Img:= GetImageFromID (ImgID);

                if Img <> nil then begin
                   if KeyOptions.ImgFormatInsideRTF = ifAccordingImage then
                      GetStream:= False;                     // We don't need RTFPictToImage to get the Stream from the content in the RTF
                   if StreamNeeded then
                      Stream:= Img.ImageStream;

                   ImgFormat:= Img.ImageFormat;
                   Width:= Img.Width;
                   Height:= Img.Height;
                   StreamRegistered:= true;
                end;


                if (Img = nil) or UseExtenalImagesManager then begin
                   { If UseExtenalImagesManager: We have only relied on it to obtain the Stream. The ID is not valid for us, it corresponds to the registration
                     in another file. If not, it must be a lost ID, which I should ignore. Maybe it's due to a deleted image
                     (after saving) and recovered in the editor by doing UNDO  }
                     // pID: \'11I999999  pIDr: \'12
                  if ImgIDwasPresent then begin
                     SetLength(ImgIDsToRemove, Length(ImgIDsToRemove) + 1);
                     ImgIDsToRemove[Length(ImgIDsToRemove)-1]:= ImgID;
                     ImgIDwasPresent:= false;
                  end;
                  ImgID:= 0;
                end;
            end;

            // Discard abandoned image IDs that may have been left up to this point and have not yet been deleted
            if Length(ImgIDsToRemove) > 0 then begin
               ContainsImgIDsRemoved:= true;
               var Discount: integer:= 0;
               StrAux:= Copy(RTFText, pIDff, pImgIni - pIDff +1);
               for i:= 0 to Length(ImgIDsToRemove)-1 do begin
                  ImgIDStr:= Format(KNT_RTF_IMG_HIDDEN_MARK_CONTENT, [ImgIDsToRemove[i]]);
                  StrAux:= StringReplace(StrAux, ImgIDStr, '', []);
                  Inc(Discount, Length(ImgIDStr));
               end;
               if StrAux <> '' then              // It may be if it is LinkImgFolded
                  Move(StrAux[1], RTFTextOut[pOut - (pImgIni-pIDff+1)], Length(StrAux));
               Dec(pOut, Discount);
            end;



            if ImageMode = imLink then begin     // Image is in link mode
               RTFLinkToImage (RTFText, pImgIni, WidthGoal, HeightGoal, pRTFImageEnd, LinkImgFolded);
               if LinkImgFolded then                                          // We will not convert folded image links
                  ImgRTF:= Copy(RTFText+1, pImgIni, pRTFImageEnd-pImgIni + 1);
            end
            else
               RTFPictToImage (RTFText, pPict, Stream, ImgFormat, Width, Height, WidthGoal, HeightGoal, pRTFImageEnd, GetStream);

            if fReconsiderImageDimensionsGoal then begin
               if Img <> nil then begin
                  Width:= Img.Width;
                  Height:= Img.Height;
               end;
               WidthGoal:= Width;
               HeightGoal:= Height;
               if TKntRichEdit.FoldingInScrapbook then begin
                  WidthGoal:= 2;
                  HeightGoal:= 2;
               end
               else
                  CheckDimensionGoals (WidthGoal, HeightGoal);
            end;

            if (fStorageMode <> smEmbRTF) then begin
               if (ImgID = 0) and ((ImageMode = imImage) or UseExtenalImagesManager) and (Stream <> nil) and (Stream.Size > 0)
                   and ((Img = nil) or not Img.StreamIsEncrypted)
               then begin
                  {if FolderName = '' then begin
                     assert(FolderName <> '');
                     Exit('');       // It shouldn't happen, but if it does, we are exporting notes. We leave without processing anything
                  end;}

                  if Img <> nil then begin            // Processing images from MergeFromKNTFile
                    ImgCaption:= Img.Caption;
                    if CheckRegisterImage (Stream, Img.ImageFormat,  Width, Height, FolderName, Img.OriginalPath, Img.IsOwned, 'MergeKNT', Img) then begin
                       StreamRegistered:= true;
                       if ImgCaption <> '' then
                          Img.Caption:= ImgCaption;
                    end;
                    ImgID:= Img.ID;
                  end
                  else begin
                    ImgFormatDest := imgUndefined;
                    ConvertStreamToImgFormatDest (Stream, ImgFormat, imgFormatDest);
                    if Stream.Size > 0 then begin
                       ImgFormat:= ImgFormatDest;
                       if CheckRegisterImage (Stream, ImgFormatDest,  Width, Height, FolderName, '', true, Source, Img) then
                          StreamRegistered:= true;
                       if Img <> nil then            //  DoNotRegisterNewImages coult it be = True and the image not included in the file
                          ImgID:= Img.ID;
                    end;
                  end;
               end;
            end;



            if ImgIDwasPresent and (ImagesModeDest = ImageMode) and (not fReconsiderImageDimensionsGoal) then begin
              { If ImagesMode = ImagesModeDest and the image is already registered, just copy it to the output string
                If the current mode is smEmbRTF and we are converting from a node with image registration (smEmbKNT, for example)
                we will have to remove the hidden ID marks anyway, we will do it later  }
                NBytes:= pRTFImageEnd - pImgIni +1;
                Move(RTFText[pImgIni], RTFTextOut[pOut], NBytes);
                Inc(pOut, NBytes);
            end
            else begin
                if (ImagesModeDest = imImage) and (Stream <> nil) and (Stream.Size > 0) and
                   ((Img = nil) or not Img.IsEncrypted or not TkntFile(KntFile).EncryptedContentMustBeHidden)
                then begin
                   if ImgIDwasPresent or (fStorageMode = smEmbRTF) then begin
                      ID:= 0;                                             // To not add the hidden tag from GetRTFforImageInsertion
                      if (fStorageMode <> smEmbRTF) then
                         CheckHiddenMarkInImage;
                   end
                   else
                      ID:= ImgID;

                   if not LinkImgFolded then     // We will not convert folded image links. We already have the link on ImgRTF
                      ImgRTF:= GetRTFforImageInsertion(ID, Stream, ImgFormat, Width, Height, WidthGoal, HeightGoal, False, StreamRegistered, MaintainWMF_EMF);

                   if ImageMode = imLink then
                      SetLength(RTFTextOut, Length(RTFTextOut) + Length(ImgRTF) + 100)
                   else
                      { We may be pasting an image from the clipboard, previously registered, and the string generated by us using
                       the Stream may have a larger size, because the RichEdit control will have eliminated the following information
                       if it was present: Embedded color profile, EXIF metada, IPTC metadata }
                      if (pOut + Length(ImgRTF)) > Length(RTFTextOut) then
                         SetLength(RTFTextOut, Length(RTFTextOut) + (pOut + Length(ImgRTF) - Length(RTFTextOut)) + 100);

                end
                else begin
                  if Img = nil then                    // Image not included in the list of definitions (and we do not have its stream)
                     ImgCaption:= GetRS(sImg04) + '?'
                  else
                     if (ImagesModeDest = imImage) then begin
                        if Img.IsEncrypted and TKntFile(KntFile).EncryptedContentMustBeHidden then
                           ImgCaption:= '[*] ' + Img.FileName
                        else
                           ImgCaption:= GetRS(sImg04) + Img.FileName;
                     end
                     else begin
                        ImgCaption:= Img.Caption;
                        if ImgCaption = '' then ImgCaption:= Img.FileName;
                     end;

                  if not LinkImgFolded then begin    // We will not convert folded image links. We already have the link on ImgRTF
                     if (Img <> nil) and Img.IsEncrypted and not TKntFile(KntFile).EncryptedContentMustBeHidden then
                        ImgCaption:= '[*] ' + ImgCaption;
                     ImgRTF:= Format(KNT_IMG_LINK, [ImgID, WidthGoal, HeightGoal, URLToRTF(ImgCaption, true)]);         // {\field{\*\fldinst{HYPERLINK "img:%d:%d,%d}"}}{\fldrslt{%s}}}
                  end;
                  if (fStorageMode <> smEmbRTF) and (ImgID <> 0) then begin
                      if not ImgIDwasPresent then begin    // The tag with the image ID was incorrect and had to be re-registered, or we are converting from smEmbRTF
                         var LinkImg: AnsiString:= KNT_RTF_IMG_HIDDEN_MARK;
                         if LinkImgFolded then
                            LinkImg:= KNT_RTF_IMG_HIDDEN_MARK_CONTENT;
                         ImgRTF:= Format(LinkImg, [ImgID]) + ImgRTF
                      end
                      else
                         CheckHiddenMarkInLink;
                  end;

                  if ImageMode = imLink then
                     SetLength(RTFTextOut, Length(RTFTextOut) + Length(ImgRTF));     // The image in imLink format may be shorter than this one, which indicates Image not found..
                end;

                NBytes:= Length(ImgRTF);
                Move(ImgRTF[1], RTFTextOut[pOut], NBytes);
                Inc(pOut, NBytes);
            end;


            if In_shppict then begin
               pRTFImageEnd:= PosPAnsiChar('}}', RTFText, pRTFImageEnd+1 +1); //-1;             '}}' corresponding to: {\*\nonshppict{\pict{
               //Inc(pRTFImageEnd);
               In_shppict:= false;
            end;

            if ImageMode = imLink then begin      // Image was in link mode
               if LinkImgFolded then
                  pLinkImgFolded:= -99           // We will go back to look for another one. We have already 'consumed' this one
               else
                  pLinkImg:= -99;                // Idem
            end
            else
               pPict:= -99;                      // Idem

            pIn:= pRTFImageEnd + 1;

         until (pPict = -1) and (pLinkImg = -1) and (pLinkImgFolded = -1);


         { Search and mark to eliminate any possible hidden label that may have been left isolated, abandoned, 
           as a result of some uncontrolled error }
         pID:= PosPAnsiChar(beginIDImg, RTFText, pIn);
         pIDff:= pID;
         while (pID >= 1) and (pID < BufSize) do begin
            pIDr:= PosPAnsiChar(endIDImg, RTFText, pID);                         // \v\'11I999999\'12\v0        pID: \'11I999999  pIDr: \'12      (Max-normal-: pIDr-pID=11) -> 12 ..
            if (pIDr >= 1) and ((pIDr-pID) <= 12) and (pID < BufSize) then begin
               ImgIDStr:= CopyPAnsiChar(RTFText, pID + Lb, (pIDr - pID) -Lb);
               if TryStrToInt(ImgIDStr, ImgID) then begin
                  SetLength(ImgIDsToRemove, Length(ImgIDsToRemove) + 1);
                  ImgIDsToRemove[Length(ImgIDsToRemove)-1]:= ImgID;
               end;
            end
            else
               pIDr:= pID + 12;

            pID:= PosPAnsiChar(beginIDImg, RTFText, pIDr);
         end;



         if Length(ImgIDsToRemove) = 0 then begin
            // Ok. There are no hidden abandoned image marks left. Copy remaining text from RTFText
            if (RTFTextOut <> '')  then begin
               if pIn < BufSize then begin
                  NBytes:= BufSize-pIn +1;
                  Move(RTFText[pIn], RTFTextOut[pOut], NBytes);
                  Inc(pOut, NBytes);
               end;
               SetLength(RTFTextOut, pOut-2);

               ContainsImages:= true;
            end;

         end
         else begin
            // Abandoned hidden image marks have been located, which must be removed
            ContainsImgIDsRemoved:= true;
            if (RTFTextOut = '')  then begin
               SetLength(RTFTextOut, Length(RTFText));
               Move(RTFText[1], RTFTextOut[1], pIDff-1);
               pOut:= pIDff;
            end
            else begin
               NBytes:= pIDff-pIn -1;
               Move(RTFText[pIn], RTFTextOut[pOut], NBytes);
               Inc(pOut, NBytes);
               ContainsImages:= true;
            end;

            StrAux:= Copy(RTFText, pIDff, BufSize - pIDff +1);
            for i:= 0 to Length(ImgIDsToRemove)-1 do begin
               ImgIDStr:= Format(KNT_RTF_IMG_HIDDEN_MARK_CONTENT, [ImgIDsToRemove[i]]);
               StrAux:= StringReplace(StrAux, ImgIDStr, '', []);
            end;
            Move(StrAux[1], RTFTextOut[pOut], Length(StrAux));
            SetLength(RTFTextOut, pOut + Length(StrAux) -1);
         end;

         Result:= RTFTextOut;

      except
        on E: Exception do
           App.ErrorPopup(E, GetRS(sImg20));
      end;


   finally
     if (fStorageMode = smExternal) and (fExternalStorageToRead <> nil) then          // It can be a new file
        fExternalStorageToRead.Close;

     if (not StreamRegistered) and (Stream <> nil) then
        Stream.Free;
   end;

 {$IFDEF KNT_DEBUG}
  finally
     Log_StoreTick('ProcessImagesInRTF - END', 4, -1);
  end
 {$ENDIF}
end;


function TImageMng.TryRTFPictToImage (Buffer: Pointer; BufSize: integer; var Img: TKntImage): boolean;
var
  pPict, pRTFImageEnd: integer;
  Stream: TMemoryStream;
  Width, Height, WidthGoal, HeightGoal: integer;
  ImgFormat: TImageFormat;

  ImgID: integer;
  RTFText: PAnsiChar;

begin
   Img:= nil;
   RTFText:= PAnsiChar(Buffer);

   if (Length(RTFText) > BufSize) then begin
      assert(Length(RTFText) <= BufSize );
      exit;
   end;

   pPict:= PosPAnsiChar('{\pict{', RTFText, 1) -1;
   if pPict = -1 then exit;

   Stream:= TMemoryStream.Create;
   try
      if RTFPictToImage (RTFText, pPict, Stream, ImgFormat, Width, Height, WidthGoal, HeightGoal, pRTFImageEnd, true) then begin
         Img:= TKntImage.Create(0, '', true, ImgFormat, Width, Height, 0,0, Stream);
         Img.Caption:= GetRS(sImg22);
      end;

   finally
      if Img = nil then
         Stream.Free;
   end;

end;


//-----------------------------------------
//      IMAGES - Instances - ReferenceCount
//-----------------------------------------


procedure TImageMng.ResetAllImagesCountReferences;
var
  Img: TKntImage;
  i: integer;
begin
   for i := FImages.Count-1 downto 0 do begin
     Img:= TKntImage(FImages[i]);
     Img.FReferenceCount := 0;
   end;
end;


function TImageMng.GetImagesIDInstancesFromRTF (Stream: TMemoryStream): TImageIDs;
var
  pID,pIDr: integer;
  ImgID, Num: integer;
  Text: PAnsiChar;

const
   beginIDImg = KNT_RTF_HIDDEN_MARK_L + KNT_RTF_HIDDEN_IMAGE;         // \'11I
   endIDImg = KNT_RTF_HIDDEN_MARK_R;                                  // \'12
   Lb = Length(beginIDImg);

begin
    Result:= nil;
    if Stream.Size = 0 then exit;

    pID:= 0;
    Num:= 0;
    Text:= PAnsiChar(Stream.Memory);

    repeat
       pID:= PosPAnsiChar(beginIDImg, Text, pID+1);
       if (pID > 0) and (pID < Stream.Size) then begin
          pIDr:= PosPAnsiChar(endIDImg, Text, pID);                             // \v\'11I999999\'12\v0        pID-> \'11I999999  pIDr-> \'12      (Max-normal-: pIDr-pID=11) -> 12 ..
          if (pIDr > 0) and ((pIDr-pID) <= 12) then begin
             if TryStrToInt(CopyPAnsiChar(Text, pID + Lb, (pIDr - pID) -Lb), ImgID) then begin
                Inc(Num);
                SetLength(Result, Num);
                Result[Num-1]:= ImgID;
             end;
          end;
       end;

    until (pID = 0) or ((pID >= Stream.Size));

end;


function TImageMng.GetImagesIDInstancesFromTextPlain (TextPlain: String): TImageIDs;
var
  pID,pIDr: integer;
  ImgID, Num: integer;

const
   beginIDImg = KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_IMAGE;         // $11I
   endIDImg = KNT_RTF_HIDDEN_MARK_R_CHAR;                                  // $12
   Lb = Length(beginIDImg);

begin
   pID:= 0;
   Num:= 0;
   Result:= nil;

   repeat
      pID:= Pos(beginIDImg, TextPlain, pID+1);
      if pID > 0 then begin
         pIDr:= Pos(endIDImg, TextPlain, pID);                             // L1I999999R
         if (pIDr > 0) and ((pIDr-pID) <= 10) then begin
            if TryStrToInt(Copy(TextPlain, pID + Lb, (pIDr - pID) -Lb), ImgID) then begin
               Inc(Num);
               SetLength(Result, Num);
               Result[Num-1]:= ImgID;
            end;
         end;
      end;

   until pID = 0;

end;


procedure TImageMng.UpdateImagesCountReferences (const IDsBefore: TImageIDs;  const IDsAfter: TImageIDs);
var
   a, b, IDAfter, ID: integer;
   Img: TKntImage;
   _IDsBefore, _IDsAfter: TImageIDs;

begin
     _IDsBefore := IDsBefore;
     _IDsAfter := IDsAfter;
     SetLength(_IDsBefore, Length(IDsBefore));
     SetLength(_IDsAfter, Length(IDsAfter));

     for a := Low(_IDsAfter) to High(_IDsAfter) do begin
        IDAfter:= _IDsAfter[a];

         for b := Low(_IDsBefore) to High(_IDsBefore) do begin
            if IDAfter =  _IDsBefore[b] then begin
                _IDsAfter[a]:= 0;
                _IDsBefore[b]:= 0;
                break;
            end;
         end;
     end;

    // Added instances
    for a := Low(_IDsAfter) to High(_IDsAfter) do begin
        ID:= _IDsAfter[a];
        if ID <> 0 then begin
           Img:= GetImageFromID (ID);
           if Img <> nil then
              Inc(Img.FReferenceCount);
        end;
    end;

    // Deleted instances
    for b := Low(_IDsBefore) to High(_IDsBefore) do begin
        ID:= _IDsBefore[b];
        if ID <> 0 then begin
           Img:= GetImageFromID (ID);
           if Img <> nil then
              Dec(Img.FReferenceCount);
        end;
    end;

end;


procedure TImageMng.RemoveImagesReferences (const IDs: TImageIDs);
var
  i, ID: integer;
  Img: TKntImage;
begin
    for i := Low(IDs) to High(IDs) do begin
        ID:= IDs[i];
        if ID <> 0 then begin
           Img:= GetImageFromID (ID);
           if Img <> nil then                 // Could be nil if some image tag has been identified that we don't have in our list
              Dec(Img.FReferenceCount);
        end;
    end;
end;


procedure TImageMng.ReloadImages(const IDs: TImageIDs);
var
  i, ID: integer;
  Img: TKntImage;
begin
    for i := Low(IDs) to High(IDs) do begin
        ID:= IDs[i];
        if ID <> 0 then begin
           Img:= GetImageFromID (ID);
           if Img <> nil then begin
              if Img.IsOwned and (not (fStorageMode in [smExternal, smExternalAndEmbKNT]) or Img.MustBeSavedExternally) then continue;

              Img.FreeImageStream;
              ReloadImageStream(Img);
           end;
        end;
    end;
end;

procedure TImageMng.RegisterImagesReferencesExported (const IDs: TImageIDs);
var
  i, ID: integer;
begin
    for i := Low(IDs) to High(IDs) do begin
       ID:= IDs[i];
       if fImagesIDExported.IndexOf(Pointer(ID)) < 0 then
          fImagesIDExported.Add(Pointer(ID));
    end;
end;


function TImageMng.GetPositionOffset (Stream: TMemoryStream; Pos_ImLinkTextPlain: integer; CaretPos: integer; const imLinkTextPlain: String; RTFModified: boolean; ForceCalc: boolean = false): integer;
var
  pID,pIDr: integer;
  pID_e,pIDr_e: integer;
  Text: PAnsiChar;
  Offset, charsLink: integer;
  nLinks, n, nLinksVisible: integer;
  TextPlainInEditor: String;
  ImagesVisible: Array of integer;
  SomeImagesAreVisible: boolean;


const
   beginIDImg = KNT_RTF_HIDDEN_MARK_L + KNT_RTF_HIDDEN_IMAGE;         // \'11I
   endIDImg = KNT_RTF_HIDDEN_MARK_R;                                  // \'12

   beginIDImgChar = KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_IMAGE;         // $11I
   endIDImgChar = KNT_RTF_HIDDEN_MARK_R_CHAR;                                  // $12


   function CheckValidImageHiddenMark: boolean;
   begin
       Result:= false;
       if (pIDr_e > 0) and ((pIDr_e-pID_e) <= 10) then begin
          // We consider the hidden mark valid -> we count the hyperlink
          Inc(nLinks);
          SetLength(ImagesVisible, nLinks);
          ImagesVisible[nLinks-1]:= 0;
          if TextPlainInEditor[pIDr_e+1] <> 'H' then begin           // <L>1I999999<R>HYPERLINK  or  <L>I999999<R><L>"img:<ImgID>,... in Folded mode
             if Copy(TextPlainInEditor, pIDr_e+1, Length(KNT_IMG_FOLDED_PREFIX)) = KNT_IMG_FOLDED_PREFIX then
                // It is an image inside a folded block => hidden   (See comment *1, below)
             else begin
                ImagesVisible[nLinks-1]:= 1;
                SomeImagesAreVisible:= true;
             end;
          end;
          Result:= true;
       end;
   end;

begin

   {
     The position in the editor (Editor.SelStart) corresponds to the one deduced in the plain text string
     obtained with Editor.TextPlain.
     When saving, and to perform searches in a homogeneous manner, a plain text string corresponding to the
     imLink mode is used, in which the images are not visible, and hyperlinks are displayed instead.
     An image consulted in Editor.TextPlain is equivalent to a single character, while the associated
     hyperlink takes up more characters, the hidden ones (HYPERLINK plus the URL) and the visible text).
     In both cases we are using a hidden tag with the image id:
      In Plain text:
          imLink  -> <L>I999999<R>HYPERLINK img:<ImgID>,<W>,<H> <textOfHyperlink>
          imImage -> <L>I999999<R>[I]
          [I]: Character representing the image

           In imLink mode, for each image we show the hidden characters and a hyperlink,
           while in imImage mode we only show those hidden characters and the image itself
          (which only occupies one character in TextPlain):

     As a consequence of the above, the position of a specific character in the TextPlain-imLink string will
     always be greater than or equal to the position in TextPlan-Editor. If the text does not contain images,
     it will be the same position. It will also be the same if the images are in a position after this character.

     We must take into account that in the editor there can be both visible (imImage mode) and hidden (imLink mode)
     individual images.

     This method returns, from a position defined according to TextPlain-imLink (if Pos_ImLinkTextPlain >= 0),
     or according to TextPlain-Editor (for the indicated folder/node) if CaretPos >=0, the positive difference (offset)
     with respect to the equivalent position in the alternative TextPlain (for the indicated folder/node)


     As the searches are carried out using TextPlain-imLink as a reference, this method allows us to obtain
     the exact position in the editor (subtracting the offset calculated here), to be able to correctly highlight
     a match in the editor, when this is required .
     It also allows us, given a position in the editor, to obtain the corresponding position in TextPlain-imLink
     necessary to know from which point to start the search (in RunFindNext)


     - Pos_ImLinkTextPlain: Position in imLinkTextPlain
     - Stream: the content of the folder/node, with the RTF corresponding to the imLink mode
     - imLinkTextPlain: TextPlain in imLink mode corresponding to the stream received.
                        Used when saving to disk and for searching

     To calculate the offset we need to have the stream of the folder/node, where we have the RTF equivalent to the
     TextPlain-imLink received. Through it we can count the length of the image hyperlinks that are before the
     position received as a parameter.
     (Actually the difference with what the image occupies in imLink mode -> number of hyperlink characters - 1)
     But we will only calculate and add the lengths of those hyperlinks whose images are visible in the editor. 
     If an image is also in hidden mode (imLink) in the editor, there will be no difference to account for.

       - -
       Note: if on a folder/node with all the images hidden we insert an image, which will be visible, and we intend
       to highlight a match from a search prior to said change, it is normal that it will not be highlighted properly,
       simply because the editor has changed. It is necessary to run the query again.
       - -

     First: we need to count how many hyperlinks are located in front of the received position, checking it in imLinkTextPlain

     At first, it might seem that if ImagesManager.ImagesMode = imLink no offset needs to be calculated.
     But it may happen that if the folder/node is being edited, some images are visible and others are not. For example,
     it is possible to set the mode to imLink (hiding already inserted images) and continue adding images.
     All new images are always incorporated visibly. It could also happen that an image in link format that had
     previously been copied to the clipboard was pasted and then pasted, when the mode is imImage

     Therefore, unless we can confirm that the above has not happened, we must also go through the TextPlain obtained
     from the editor (TextPlainInEditor), as it is being displayed at this moment, to know which of the images are in
     one mode or another and thus consider them or not when calculating the offset.
     }

     (*

     *1
     Update.
     Since new folding functionality it is necessary to consider images that can be inside folded texts.
     That images are always hidden and their format is different.

     Normal image:
       imLink ->   <L>I999999<R>HYPERLINK img:<ImgID>,<W>,<H> <textOfHyperlink>
       RTF    ->   \v\'11I2\'12\v0{\field{\*\fldinst{HYPERLINK "img:2,35,39"}}{\fldrslt {Image.png}}}

     Image insided folded text:
        imLink ->  <L>I999999<R><L>"img:<ImgID>,<W>,<H>"@Image.png<R>
        RTF    ->  \'11I2\'12\'11L"img:<ImgID>,<W>,<H>"@Image.png\'12

     *)


    Result:= 0;


    // Check if no offset needs to be applied

    // There is no need to make any adaptation, since TextPlainInEditor must necessarily be equal to imLinkTextPlain
    if (not ForceCalc) and (ImageMng.ImagesMode = imLink) and (not RTFModified) then
       exit;

    // There is no need to adapt anything because there are no images (neither hidden nor visible)
    if Pos(beginIDImgChar, imLinkTextPlain, 1) = 0 then
       exit;

    TextPlainInEditor:= ActiveEditor.TextPlain;

    { If the length of TextPlainInEditor = length of imLinkTextPlain this will be because there are images but they
      are all hidden, hence the coincidence in the lengths. It would be highly unlikely that there would be a number
      of visible images equal to the number of characters added to the folder/node and not yet saved to the stream. }
    if (not ForceCalc) and (Length(imLinkTextPlain) = Length(TextPlainInEditor)) then
       exit;



    pID:= 0;
    pID_e:= 0;
    nLinks:= 0;


    if CaretPos >= 0 then begin

       repeat
          pID_e:= Pos(beginIDImgChar, TextPlainInEditor, pID_e + 1);
          if  (pID_e > 0) and (pID_e < Length(TextPlainInEditor)) and (pID_e < CaretPos) then begin
             pIDr_e:= Pos(endIDImgChar, TextPlainInEditor, pID_e);                             // L1I999999R
             CheckValidImageHiddenMark;     // Can update nLinks, ImagesVisible and SomeImagesAreVisible
             pID_e:= pIDr_e;
          end
          else
             break;
       until (pID_e >= Length(TextPlainInEditor)) or (pID_e >= CaretPos);

    end
    else begin
       repeat
          pID:= Pos(beginIDImgChar, imLinkTextPlain, pID + 1);
          pID_e:= Pos(beginIDImgChar, TextPlainInEditor, pID_e + 1);
          if  (pID > 0) and (pID < Length(imLinkTextPlain)) and (pID < Pos_ImLinkTextPlain) and
              (pID_e > 0) and (pID_e < Length(TextPlainInEditor))
          then begin
             pIDr:= Pos(endIDImgChar, imLinkTextPlain, pID);                             // L1I999999R
             pIDr_e:= Pos(endIDImgChar, TextPlainInEditor, pID_e);
             if CheckValidImageHiddenMark then     // Can update nLinks, ImagesVisible and SomeImagesAreVisible
                pID:= pIDr + 20
             else
                pID:= pIDr;

             pID_e:= pIDr_e;
          end
          else
             break;
       until (pID >= Length(imLinkTextPlain)) or (pID >= Pos_ImLinkTextPlain)
          or (pID_e >= Length(TextPlainInEditor));

    end;



    if not SomeImagesAreVisible then
       exit;



    // Second: add the length of those hyperlinks to consider it as offset

    Offset:= 0;
    pID:= 0;
    n:= 0;
    nLinksVisible:= 0;
    Text:= PAnsiChar(Stream.Memory);

    repeat
       pID:= PosPAnsiChar(beginIDImg, Text, pID+1);
       if (pID > 0) and (pID < Stream.Size) and (n < nLinks) then begin
          pIDr:= PosPAnsiChar(endIDImg, Text, pID);                             // \v\'11I999999\'12\v0        pID-> \'11I999999  pIDr-> \'12      (Max-normal-: pIDr-pID=11) -> 12 ..
          if (pIDr > 0) and ((pIDr-pID) <= 12) then begin
             // We accept the hidden mark as valid (and not inside a folded block --see *1) -> we count the hyperlink
              if ImagesVisible[n] = 1 then begin
                 charsLink:= CountRTFLinkChars(Text, pIDr + 1);
                 Inc(Offset, charsLink);
                 Inc(nLinksVisible);
              end
              else
                  charsLink:= 20;              // We count at least the hidden part, which will be at least: 'HYPERLINK "img:9,9,9"'
              Inc(n);                          // We count the hyperlinks that we are processing
              pID:= pIDr + charsLink;
          end
          else
             pID:= pIDr + 1;
       end
       else
          break;

    until (pID >= Stream.Size) or (n >= nLinks);

    Result:= Offset - nLinksVisible;
end;


function TImageMng.GetPositionOffset_FromImLinkTP (Stream: TMemoryStream; Pos_ImLinkTextPlain: integer; const imLinkTextPlain: String; RTFModified: boolean; ForceCalc: boolean = false): integer;
begin
   Result:= GetPositionOffset(Stream, Pos_ImLinkTextPlain, -1, imLinkTextPlain, RTFModified, ForceCalc);
end;


function TImageMng.GetPositionOffset_FromEditorTP (Stream: TMemoryStream; CaretPos: integer; const imLinkTextPlain: String; RTFModified: boolean; ForceCalc: boolean = false): integer;
begin
   Result:= GetPositionOffset(Stream, -1, CaretPos, imLinkTextPlain, RTFModified, ForceCalc);
end;


//-----------------------------------------
//      ENCRYPTED IMAGES
//-----------------------------------------

procedure TImageMng.ProcessEncryptedImages;
var
  Img: TKntImage;
  i: integer;
  Stream: TMemoryStream;
begin
  if TKntFile(KntFile).EncryptedContentMustBeHidden then exit;

  for i := 0 to fImages.Count-1 do begin
    Img:= TKntImage(fImages[i]);
    if Img.IsEncrypted then
       Stream:= Img.ImageStream;
  end;
end;

procedure TImageMng.DecryptAllImages;
var
   i: integer;
begin
   for i := 0 to fImages.Count-1 do
     TKntImage(fImages[i]).IsEncrypted:= False;
end;

procedure TImageMng.ToogleEncrypted(ImgID: integer);
var
  Img: TKntImage;
begin
  Img:= GetImageFromID(ImgID);
  if Img <> nil then begin
     Img.IsEncrypted:= not Img.IsEncrypted;
     TKntFile(KntFile).Modified:= True;
  end;
end;


//-----------------------------------------
//      IMAGE VIEWER
//-----------------------------------------


procedure TImageMng.OpenImageFile(FilePath: string);
var
  ShellExecResult: integer;
  Parameters: string;

begin
  screen.Cursor := crAppStart;

  if KeyOptions.ImgViewerPath <> '' then begin
     Parameters:= FilePath;
     FilePath:= GetAbsolutePath(ExtractFilePath(Application.ExeName), KeyOptions.ImgViewerPath);
  end
  else
     Parameters:= '';

  try
      ShellExecResult := ShellExecute( 0, 'open', PChar(FilePath), PChar(Parameters), nil, SW_NORMAL );
  finally
      screen.Cursor := crDefault;
  end;

  if ( ShellExecResult <= 32 ) then begin
    if (( ShellExecResult > 2 ) or KeyOptions.ShellExecuteShowAllErrors ) then
      App.ErrorPopup(Format(GetRS(sImg10), [ShellExecResult, FilePath, TranslateShellExecuteError(ShellExecResult)]));
  end
  else begin
    if KeyOptions.MinimizeOnURL then
       Application_Minimize;
  end;

end;


function TImageMng.GetImgViewerIsOpen: boolean;
begin
   Result:= ImgViewerInstance <> nil;
end;

function TImageMng.GetImgIDinViewer: integer;
var
   imgViewer: TForm_Image;
begin
   Result:= -1;
   imgViewer:= ImgViewerInstance;
   if imgViewer <> nil then
      Result:= imgViewer.Image.ID;
end;

procedure TImageMng.CheckBringToFrontLastImageViewer;
begin
  if kn_ImageForm.LastImgViewerOpen <> nil then begin
     LastImgViewerOpen.BringToFront;
     LastImgViewerOpen:= nil;
  end;

end;

procedure TImageMng.OpenImageViewer (ImgID: integer; ShowExternalViewer: boolean; SetLastFormImageOpened: boolean; Img: TKntImage = nil);
var
  Form_Image, OpenedViewer: TForm_Image;
  FilePath: string;
  UsingOpenViewer: boolean;

begin
   if (ImgID = 0) and not assigned(Img) then exit;

   try
      if not assigned(Img) then
         Img:= GetImageFromID(ImgID);
      if Img= nil then exit;

      if Img.IsEncrypted and ActiveFile.EncryptedContentMustBeHidden and not ActiveFile.CheckAuthorized(True) then exit;

      if ShowExternalViewer then begin
         FilePath:= GetImagePath(Img);
         if FileExists(FilePath) then begin
            OpenImageFile(FilePath);
            exit;
         end;
      end;

      if ActiveEditor.NNodeObj <> nil then
         ActiveFolder.SaveEditorToDataModel;

      UsingOpenViewer:= false;

      OpenedViewer:= ImgViewerInstance;
      if (OpenedViewer <> nil) and (KeyOptions.ImgSingleViewerInstance) then begin
         Form_Image:= OpenedViewer;
         UsingOpenViewer:= true;
      end
      else begin
         Form_Image := TForm_Image.Create( Form_Main );
         NewImageViewer(Form_Image);
      end;

      { We use kn_ImageForm.LastFormImageOpened to be able to give focus to the viewer from Form_Main.RxRTFMouseUp,
        since it is lost if we open the viewer by right-clicking on a link. But we do not want to lose focus on the
        main screen if KeyOptions.ImgSingleViewerInstance=True and the viewer had already been created previously
        (it was visible before clicking on the link) to, among others, favor being able to implement the
        ImgHotTrackViewer option. For this we look at ImgViewerInstance
      }
      if SetLastFormImageOpened and (OpenedViewer = nil) then
         kn_ImageForm.LastImgViewerOpen:= Form_Image;
      Form_Image.Image:= Img;
      Form_Image.Folder:= ActiveFolder;
      if not UsingOpenViewer then
         Form_Image.Show;

   except
     on E: Exception do
       App.ErrorPopup(E, GetRS(sImg18));
   end;
end;



initialization


end.
