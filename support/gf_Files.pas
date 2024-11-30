unit gf_files;

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

{$I gf_base.inc}

interface

uses
   Winapi.Windows,
   Winapi.ShellAPI,
   System.Classes,
   System.IniFiles,
   System.SysUtils,
   Vcl.Graphics
   ;

const
  // set of characters accepted in filenames (very restrictive)
  FileNameChars  = ['_', '-', '(', ')', '!', '$', '.', '&', ','];
  // characters that are not permitted in filenames
  // (may be OK for directories!)
  INVALID_FILENAME_CHARS : string = '\/:"*?<>,|';
  PERMIT_SPACES_IN_FILENAMES : boolean = true;

type
  CharacterSet = set of AnsiChar;


const
  // typical filters for open/save dialog boxes
  filterTextHTML = 'Text files (*.txt)|*.txt|HTML files (*.htm*)|*.htm*|All files (*.*)|*.*';
  filterText     = 'Text files (*.txt)|*.txt|All files (*.*)|*.*';
  filterHTML     = 'HTML files (*.htm*)|*.htm*;*.?htm*|All files (*.*)|*.*';

type
  TFileState = record
    Name : string;
    Time : TDateTime;
    Size : longint;
  end;

type
  TFileHandlerProc = procedure(
    const DirInfo : TSearchRec;
    const Path : string );

type
  string12 = string[12];
  TTextFileType = ( tfDOS, tfUNIX, tfMAC, tfUnknown );
  {
  TFileKind = ( fkUnknown, fkText, fkHTML, fkRTF,
                fkMSOffice, fkExecutable, fkSound,
                fkArchive, fkPDF, fkImage );
  }

const
  MaxTestBufSize = 1024;

type
  TTestBuffer = array[1..MaxTestBufSize] of AnsiChar;
{
function GetFileKind( const FN : string ) : TFileKind;
}

function FileNameIsValid( const fn : string ) : boolean;
function ExtractFileNameNoExt( const fn : string ) : string;
function GetUniqueFileName( const folder, fn : string ) : string;
function IsDriveRemovable( const path : string ) : boolean;
function IsValidFileChar( const Key : char ) : boolean;
function MakeValidFilename( const s : string; const AdditionalValidChars : CharacterSet; const maxlen : integer ) : string;
function RandomFileName( aPath : string; aExt: string12; len : byte ) : string;
function GetTextFileType( const fn : string ) : TTextFileType;
function GetFileSize( const fn : string ) : longint;
function GetFileDateStamp( const fn : string ) : TDateTime;
function FilesAreOfSameType( const aList : TStringList ) : boolean;
procedure GetFileState( const FN : string; var FileState : TFileState );
function MoveFileExW_n (SourceFN: string; DestFN: string; Retries: Integer): Boolean;

function GetFilesInFolder( const folder, mask : string;
                           const LowCase, AddDirName : boolean;
                           FL : TStrings {TStringList} ) : integer;
function GetSubfoldersList( folder : string;
                            const LowCase : boolean;
                            FL : TStrings {TStringList} ) : integer;


function FilesFound(
   const FileMask : string;  // file name or mask to look for
   const AttrMask : integer; // attributes
   const StartPath : string; // folder to start searchin in
   const Recurse : boolean;  // descend into subfolders?
   const FindAll : boolean;  // find all files or finish on 1st match
   Handler : TFileHandlerProc ) : integer;


function MinimizeName(const Filename: string; Canvas: TCanvas;  MaxLen: Integer): string;

function PathCombineW(lpszDest: PChar; const lpszDir, lpszFile:
PChar): PChar; stdcall; external 'shlwapi.dll';

function GetAbsolutePath(basePath: string; relativePath: string): string;
function GetTempDirectory: String;
function GetSystem32Directory: String;



{  Note: Now TIniFile (not TWIniFile) is only used in dll_keyboard.pas kn_KBD.pas and some files in plugins folder
   What it is used is, mainly, is TMemIniFile (TWMemIniFile before refactoring to Delphi CE)          }

type
TMemIniFileHelper = class helper for TMemIniFile
  function ReadString (const Section, Ident, Default : string) : string;
  procedure WriteString(const Section, Ident: string; Value: string);
end;




const
  MaxFileBuf = 4096;

var
  ___FILE_ERROR_STR : string;


implementation
uses
   gf_misc,
   gf_strings;

(*
function GetFileKind( const FN : string ) : TFileKind;
var
  ext : string;
begin
  ext := lowercase( extractfileext( FN ));
  if ( ext = '.txt' ) or
     ( ext = '.ini' ) or
     ( ext = '.inf' ) or
     ( ext = '.diz' ) or
     ( ext = '.faq' ) or
     ( ext = '.log' ) or
     ( ext = '.asc' ) or
     ( ext = '.pas' ) or
     ( ext = '.c' ) or
     ( ext = '.h' ) or
     ( ext = '.bas' ) or
     ( ext = '.js' ) or
     ( ext = '.' ) or
     ( ext = '.pl' ) then
     result := fkText
  else
  if ( ext = '.rtf' ) then
     result := fkRTF
  else
  if ( ext = '.exe' ) or
     ( ext = '.com' ) or
     ( ext = '.dll' ) or
     ( ext = '.bat' ) then
     result := fkExecutable
  else
  if ( ext = '.htm' ) or
     ( ext = '.html' ) or
     ( ext = '.shtm' ) or
     ( ext = '.shtml' ) or
     ( ext = '.php' ) or
     ( ext = '.cgi' ) or
     ( ext = '.asp' ) or
     ( ext = '.css' ) then
     result := fkHTML
  else
  if ( ext = '.doc' ) or
     ( ext = '.dot' ) or
     ( ext = '.xls' ) or
     ( ext = '.mdb' ) then
     result := fkMSOffice
  else
  if ( ext = '.zip' ) or
     ( ext = '.arj' ) or
     ( ext = '.rar' ) or
     ( ext = '.tar' ) or
     ( ext = '.gz' ) or
     ( ext = '.zoo' ) or
     ( ext = '.uue' ) then
     result := fkArchive
  else
  if ( ext = '.bmp' ) or
     ( ext = '.jpg' ) or
     ( ext = '.gif' ) or
     ( ext = '.jpeg' ) or
     ( ext = '.ico' ) or
     ( ext = '.wmf' ) or
     ( ext = '.tiff' ) or
     ( ext = '.tga' ) or
     ( ext = '.pcx' ) or
     ( ext = '.png' ) or
     ( ext = '.ani' ) or
     ( ext = '.mpeg' ) or
     ( ext = '.tif' ) then
     result := fkImage
  else
  if ( ext = '.wav' ) or
     ( ext = '.mp3' ) or
     ( ext = '.au' ) or
     ( ext = '.aud' ) or
     ( ext = '.ra' ) or
     ( ext = '.ram' ) or
     ( ext = '.lqt' ) or
     ( ext = '.snd' ) or
     ( ext = '.pcm' ) then
     result := fkSound
  else
  if ( ext = '.pdf' ) then
    result := fkPDF
  else
    result := fkUnknown;
end; // GetFileKind
*)

function GetTempDirectory: String;
var
  tempFolder: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @tempFolder);
  result := StrPas(tempFolder);
end;

function GetSystem32Directory: String;
var
  tempFolder: array[0..MAX_PATH] of Char;
begin
  GetWindowsDirectory(@tempFolder, MAX_PATH);
  result := StrPas(tempFolder) + '\system32\';
end;

function GetAbsolutePath(basePath: string; relativePath: string): string;
var
   path: string;
begin
    SetLength(path, 5*MAX_PATH);
    PathCombineW(@path[1], PChar(basePath), PChar(relativePath));
    Result:= PChar(path);
end;

function FileNameIsValid( const fn : string ) : boolean;
var
  i, l : integer;
  d : string;
begin
  result := false;
  if ( fn = '' ) then exit; // blank is not valid!
  if (( not PERMIT_SPACES_IN_FILENAMES ) and ( pos( #32, fn ) > 0 )) then exit;

  d := Extractfilepath( fn );
  if (( d <> '' ) and ( not DirectoryExists( d ))) then exit;
  // for the filename to be valid, the directory must already exist.
  // if you don't like that, only pass bare filenames to this function,
  // not full paths

  d := ExtractFilename( fn );
  if ( d = '' ) then exit; // blank filename is not valid
  l := length( d );
  for i := 1 to l do
    if ( pos( d[i], INVALID_FILENAME_CHARS ) > 0 ) then exit;

  result := true;
end; // FileNameIsValid


function MakeValidFilename(
  const s : string;
  const AdditionalValidChars : CharacterSet;
  const maxlen : integer
  ) : string;

var
  i : integer;
  r : string;
  ext : string;
begin
  r := trim( s );

  // replace invalid characters with "?" (as an auxiliary character)
  for i := 1 to length( r ) do
  begin
    if ( not ( IsCharAlphaNumeric( r[i] ) or ( CharInSet(r[i], FileNameChars) ) or ( CharInSet(r[i], AdditionalValidChars) ))) then
      r[i] := '?';
  end;

  // compress multiple "?"
  i := pos( '??', r );
  while ( i > 0 ) do
  begin
    delete( r, i, 1 );
    i := pos( '??', r );
  end;

  // trim leading and trailing "?"
  while (( r <> '' ) and ( r[1] = '?' )) do
    delete( r, 1, 1 );
  while (( r <> '' ) and ( r[length( r )] = '?' )) do
    delete( r, length( r ), 1 );

   r:= StringReplace(r, '?', '_', [rfReplaceAll]);

  // limit length to maxlen
  // (no limit if maxlen = 0 )
  i := lastpos( '.', r );
  if ( i > 0 ) then
  begin
    ext := copy( r, i, length( r ));
    delete( r, i, length( r ));
  end
  else
    ext := '';

  if (( maxlen > 8 ) and (( length( r ) + length( ext )) > maxlen )) then
      delete( r, maxlen-length( ext )+1, length( r ));
  if ( r = '' ) then
    r := '_';
  result := r + ext;
end; // MakeValidFilename


function RandomFileName( aPath : string;
                         aExt : string12;
                         len : byte ) : string;
var
  t : string;
  c : byte;
begin
  result := '';
  if (( aPath <> '' ) and ( aPath[length(aPath)] = '\' )) then
    delete( aPath, length( aPath ), 1 ); // strip trailing backslash
  if (( aExt <> '' ) and ( aExt[1] <> '.' )) then
    aExt := '.' + aExt; // make sure extension begins with a '.'
  if ( len < 1 ) then
      len := 8;
  repeat
      t := '';
      for c := 1 to len do
      begin
          t := t + chr( random( 25 ) + 97 ); // #97 to #122
      end;
  until ( not FileExists( aPath + '\' + t + aExt ));

  result := t + aExt; // return only filename, without the path

end; // RandomFileName;


function FilesFound(
   const FileMask : string;
   const AttrMask : integer;
   const StartPath : string;
   const Recurse : boolean;
   const FindAll : boolean;
   Handler : TFileHandlerProc ) : integer;
var
   ThisPath : string;
   DirInfo : TSearchRec;
   FileCount, TmpCount : integer;
   FindResult : integer;
begin
   FileCount := 0;
   if ( StartPath = '' ) then
       Getdir( 0, ThisPath )
   else
       ThisPath := StartPath;
   if ( ThisPath[length(ThisPath)] <> '\' ) then
       ThisPath := ThisPath + '\';

   FindResult := findfirst( ThisPath+FileMask, AttrMask, DirInfo );
   repeat
       if ( FindResult = 0 ) then // OK, found a file
       begin
           inc( FileCount );
           if assigned(Handler) then
               Handler( DirInfo, ThisPath );
       end;

       if ( not FindAll ) then
           break
       else
           FindResult := FindNext( DirInfo );

   until ( FindResult <> 0 );

   if ( not Recurse ) then
   begin
       result := FileCount;
       findclose( DirInfo );
       exit; // all done
   end;

   FindResult := findfirst( ThisPath+'*.*', faDirectory, DirInfo );
   repeat
       if ( FindResult = 0 ) then
       begin
           if ((( DirInfo.Attr and faDirectory ) > 0 ) and
                ( DirInfo.Name <> '.' ) and ( DirInfo.Name <> '..' )) then
           begin
               TmpCount := FilesFound( FileMask, AttrMask, ThisPath + DirInfo.Name, true, FindAll, Handler );
               if ( TmpCount > 0 ) then
               begin
                   inc( FileCount, TmpCount );
                   if ( not FindAll ) then
                       break;
               end;
           end;
       end;
       FindResult := findnext( DirInfo );
   until ( FindResult <> 0 );

   findclose( DirInfo );
   result := FileCount;
end; // FilesFound


function GetTextFileType( const fn : string ) : TTextFileType;
var
  f : file;
  p : ^TTestBuffer;
  s, ch : integer;
begin
  result := tfUnknown;
  assignfile( f, fn );
  FileMode := 0;
  try
    try
      reset( f, 1 );
    except
      on E :Exception do
      begin
        ___FILE_ERROR_STR := E.Message;
        exit;
      end;
    end;
  finally
    FileMode := 2;
  end;
    new( p );
    blockread( f, p^, MaxTestBufSize, s );
    closefile( f );
    for ch := 1 to s-1 do
    begin
      if ( p^[ch] = #13 ) then
      begin
        if ( p^[ch+1] = #10 ) then
          result := tfDOS
        else
          result := tfMAC;
        break;
      end
      else
      begin
        if ( p^[ch] = #10 ) then
        begin
          result := tfUNIX;
          break;
        end;
     end;
  end;
  dispose( p );
end; // GetTextFileType


function GetFileSize( const fn : string ) : longint;
var
   f : TFileStream;
begin
   F:= TFileStream.Create( fn, ( fmOpenRead ));
   try
     try
       result:= F.Size;
       F.Free;
     except
       on E :Exception do
       begin
         ___FILE_ERROR_STR := E.Message;
         result := -1;
       end;
     end;
   finally
   end;
end; // GetFileSize


function GetFileDateStamp( const fn : string ) : TDateTime;
var
   fh : integer;
begin
   try
     fh := FileOpen( fn, 0 );
     try
       result := FileDateToDateTime( FileGetDate( fh ));
     finally
       FileClose( fh );
     end;
   except
     on E :Exception do
     begin
       ___FILE_ERROR_STR := E.Message;
       result := 0;
     end;
   end;
end; // GetFileDateStamp


procedure GetFileState( const FN : string; var FileState : TFileState );
begin
  FileState.Name := FN;
  try
    if Fileexists( FN ) then
    begin
      FileState.Time := GetFileDateStamp( FN );
      FileState.Size := GetFileSize( FN );
    end
    else
    begin
      FileState.Time := 0;
      FileState.Size := -1;
    end;
  except
     // Possible exception: "Cannot open file "...". The process cannot access the file because it is being used by another process"
      FileState.Time := 0;
      FileState.Size := -99;
  end;
end; // GetFileState


function GetFilesInFolder( const folder, mask : string;
                           const LowCase, AddDirName : boolean;
                           FL : Tstrings {TStringList} ) : integer;
var
   DirInfo : TSearchRec;
   FindResult : integer;
begin

   result := 0;
   if (( not assigned( FL )) or ( not DirectoryExists( folder ))) then exit;
   // FL.Clear; Do NOT clear - some programs rely on it
   // (just add items to the list; calling app is responsible for clearing)
   FindResult := FindFirst( folder + mask, faAnyFile, DirInfo );
   while ( FindResult = 0 ) do
   begin
     if LowCase then
       DirInfo.Name := lowercase( DirInfo.Name );
     if AddDirName then
       FL.Add( folder + DirInfo.Name )
     else
       FL.Add( DirInfo.Name );
     FindResult := FindNext( DirInfo );
   end;
   result := FL.Count;
   FindClose( DirInfo );
end; // GetFilesInFolder


function GetSubfoldersList( folder : string;
                           const LowCase : boolean;
                           FL : TStrings {TStringList} ) : integer;
var
  DirInfo : TSearchRec;
  FindResult : integer;
begin
  result := 0;
  if (( folder <> '' ) and ( folder[length( folder )] <> '\' )) then
    folder := folder + '\';
  if (( not assigned( FL )) or ( not DirectoryExists( folder ))) then exit;
  FindResult := FindFirst( folder+'*.*', faDirectory, DirInfo );
  while ( FindResult = 0 ) do
  begin
    if ((( DirInfo.Attr and faDirectory ) > 0 ) and ( DirInfo.Name <> '.' ) and ( DirInfo.Name <> '..' )) then
    begin
      if LowCase then
        DirInfo.Name := lowercase( DirInfo.Name );
      FL.Add( folder + DirInfo.Name + '\');
    end;
    FindResult := FindNext( DirInfo );
  end;
  result := FL.Count;
  FindClose( DirInfo );
end; // GetSubfoldersList


function IsDriveRemovable( const path : string ) : boolean;
begin
  try
    result := ( GetDriveType( PChar( ExtractFileDrive( path ) + '\' )) in [0, 1, DRIVE_REMOVABLE,DRIVE_REMOTE,DRIVE_CDROM,DRIVE_RAMDISK] );
  except
    result := true;
  end;

end; // IsDriveRemovable


function IsValidFileChar( const Key : char ) : boolean;
begin
  result := ( IsCharAlphaNumeric( Key ) or
    ( CharInSet(Key, FileNameChars) ));
end; // IsValidFileChar


function ExtractFileNameNoExt( const fn : string ) : string;
var
  p : integer;
begin
  result := ExtractFilename( fn );
  p := lastpos( '.', result );
  if ( p > 2 ) then
    delete( result, p, length( result ));
end; // ExtractFileNameNoExt


function GetUniqueFileName( const folder, fn : string ) : string;
var
  name, ext : string;
  i : integer;
begin
  result := fn;
  if Fileexists( folder + fn ) then
  begin
    ext := extractfileext( fn );
    name := ExtractFilename( fn );
    i := lastpos( '.', name );
    if ( i > 1 ) then
      name := copy( name, 1, pred( i ));

    i := 1;
    repeat
      result := Format(
        '%s(%d)%s',
        [name, i, ext]
      );
      inc( i );
    until ( not Fileexists( folder + result ));
  end;
end; // GetUniqueFileName


function FilesAreOfSameType( const aList : TStringList ) : boolean;
var
  i : integer;
  ext, prevext : string;
begin
  result := true;
  if ( aList.Count < 2 ) then exit;
  prevext := Lowercase( extractfileext( aList[0] ));
  for i := 2 to aList.Count do
  begin
    ext := Lowercase( extractfileext( aList[pred( i )] ));
    if ( ext <> prevext ) then
    begin
      result := false;
      exit;
    end;
    prevext := ext;
  end;
end; // FilesAreOfSameType


procedure CutFirstDirectory(var S: string);
var
  Root: Boolean;
  P: Integer;
begin
  if S = '\' then
    S := ''
  else
  begin
    if S[1] = '\' then
    begin
      Root := True;
      Delete(S, 1, 1);
    end
    else
      Root := False;
    if S[1] = '.' then
      Delete(S, 1, 4);
    P := Pos('\',S);
    if P <> 0 then
    begin
      Delete(S, 1, P);
      S := '...\' + S;
    end
    else
      S := '';
    if Root then
      S := '\' + S;
  end;
end;


function MinimizeName(const Filename: string; Canvas: TCanvas;
  MaxLen: Integer): string;
var
  Drive: string;
  Dir: string;
  Name: string;
begin
  Result := FileName;
  Dir := ExtractFilePath(Result);
  Name := ExtractFileName(Result);

  if (Length(Dir) >= 2) and (Dir[2] = ':') then
  begin
    Drive := Copy(Dir, 1, 2);
    Delete(Dir, 1, 2);
  end
  else
    Drive := '';
  while ((Dir <> '') or (Drive <> '')) and (Canvas.TextWidth(Result) > MaxLen) do
  begin
    if Dir = '\...\' then
    begin
      Drive := '';
      Dir := '...\';
    end
    else if Dir = '' then
      Drive := ''
    else
      CutFirstDirectory(Dir);
    Result := Drive + Dir + Name;
  end;
end;



function MoveFileExW_n (SourceFN: string; DestFN: string; Retries: Integer): Boolean;
var
   Ok: Boolean;
   n: Integer;
   Str: string;
   UseMoveFileEx: Boolean;
begin
   n:= 0;
   Ok:= False;
   UseMoveFileEx:= True;
   if not _OSIsWindowsNT then
      UseMoveFileEx:= False;

   repeat
      n:= n + 1;
      if UseMoveFileEx then begin
         if MoveFileEx(PChar(SourceFN), PChar(DestFN), MOVEFILE_REPLACE_EXISTING or MOVEFILE_COPY_ALLOWED) then
            Ok:= True;
      end
      else begin
         if CopyFile(PChar(SourceFN), PChar(DestFN), false) then begin
            deletefile(PChar(SourceFN));
            Ok:= True;
         end;
      end;
      if not Ok then begin
         sleep(200);
         if GetLastError <> 5 then       // 5: ERROR_ACCESS_DENIED
            UseMoveFileEx:= False;
      end;

   until Ok or (n >= Retries);

   Result:= Ok;
end;



function TMemIniFileHelper.ReadString (const Section, Ident, Default : string) : string;

  // TMemIniFile Doesn't Handle Quoted Strings Properly (http://qc.embarcadero.com/wc/qcmain.aspx?d=4519)
  function ReadString_Base (const Section, Ident, Default : string) : string;
  begin
     result := inherited ReadString (Section, Ident, Default);
     if length (result) >= 2 then begin
        if result [1] = '"' then begin
           if result [length (result)] = '"' then
              result := copy (result, 2, length (result) - 2);
        end
        else if result [1] = '''' then begin
           if result [length (result)] = '''' then
              result := copy (result, 2, length (result) - 2);
        end;
     end;
  end;

begin
   if not ValueExists(Section, Ident) then
      Result:= Default
   else
      //Result:= TryUTF8ToUnicodeString(AnsiString(ReadString_Base(Section, Ident, '')));
      Result:= ReadString_Base(Section, Ident, '');
end;


procedure TMemIniFileHelper.WriteString(const Section, Ident: string; Value: string);
var
  rbs: RawByteString;

begin
   if CanSaveAsANSI(Value) then
      inherited WriteString(Section, Ident, value)    // To make it more compatible with older versions. Only use UTF8 if it's necessary

   else begin
      rbs:= UTF8String(Value);
      //SetCodePage(rbs, Self.Encoding.CodePage, False);
      SetCodePage(rbs, TEncoding.UTF8.CodePage, False);
      inherited WriteString(Section, Ident, rbs);
   end;
end;




END.
