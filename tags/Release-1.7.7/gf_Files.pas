unit gf_files;
{$I gf_base.inc}

(* ************************************************************
 MOZILLA PUBLIC LICENSE STATEMENT
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is "gf_Files.pas".

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 To do:
 -----------------------------------------------------------
 Released: 20 August 2001
 -----------------------------------------------------------
 URLs:

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <cicho@tenbit.pl>

************************************************************ *)


interface

uses Windows, Classes, Graphics, IniFiles,
     SysUtils, FileCtrl,wideStrings,
     ShellAPI, gf_strings, gf_misc;

const
  // set of characters accepted in filenames (very restrictive)
  FileNameChars  = ['_', '-', '(', ')', '!', '$', '.', '&', ','];
  // characters that are not permitted in filenames
  // (may be OK for directories!)
  INVALID_FILENAME_CHARS : string = '\/:"*?<>,|';
  PERMIT_SPACES_IN_FILENAMES : boolean = true;

type
  CharacterSet = set of char;


const
  // typical filters for open/save dialog boxes
  filterTextHTML = 'Text files (*.txt)|*.txt|HTML files (*.htm*)|*.htm*|All files (*.*)|*.*';
  filterText     = 'Text files (*.txt)|*.txt|All files (*.*)|*.*';
  filterHTML     = 'HTML files (*.htm*)|*.htm*;*.?htm*|All files (*.*)|*.*';

type
  TFileState = record
    Name : wideString;
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
  TTestBuffer = array[1..MaxTestBufSize] of char;
{
function GetFileKind( const FN : string ) : TFileKind;
}

function FileNameIsValid( const fn : wideString ) : boolean;
function ExtractFileNameNoExt( const fn : wideString ) : wideString;
function GetUniqueFileName( const folder, fn : wideString ) : wideString;
function IsDriveRemovable( const path : string ) : boolean;
function IsValidFileChar( const Key : char ) : boolean;
function MakeValidFilename( const s : wideString; const AdditionalValidChars : CharacterSet; const maxlen : integer ) : wideString;
function RandomFileName( aPath : wideString; aExt: string12; len : byte ) : string;
function GetTextFileType( const fn : wideString ) : TTextFileType;
function GetFileSize( const fn : wideString ) : longint;
function GetFileDateStamp( const fn : wideString ) : TDateTime;
function FilesAreOfSameType( const aList : TWideStringList ) : boolean;
procedure GetFileState( const FN : wideString; var FileState : TFileState );

function GetFilesInFolder( const folder, mask : wideString;
                           const LowCase, AddDirName : boolean;
                           FL : TWideStrings {TStringList} ) : integer;
function GetSubfoldersList( folder : wideString;
                            const LowCase : boolean;
                            FL : TWideStrings {TStringList} ) : integer;


function FilesFound(
   const FileMask : string;  // file name or mask to look for
   const AttrMask : integer; // attributes
   const StartPath : string; // folder to start searchin in
   const Recurse : boolean;  // descend into subfolders?
   const FindAll : boolean;  // find all files or finish on 1st match
   Handler : TFileHandlerProc ) : integer;

function WideMinimizeName(const Filename: WideString; Canvas: TCanvas;
  MaxLen: Integer): WideString;

function PathCombineW(lpszDest: PWideChar; const lpszDir, lpszFile:
PWideChar): PWideChar; stdcall; external 'shlwapi.dll';

function GetAbsolutePath(basePath: wideString; relativePath: wideString): wideString;  //***1

type
  TWIniFile = class(TIniFile)
  private
    //FFileName: WideString;
  public
    //constructor Create(const FileName: string; ensureUnicode: boolean);
    function ReadStringW(const Section, Ident, Default: WideString): WideString;
    procedure WriteStringW(const Section, Ident, Value: WideString);
  end;



const
  MaxFileBuf = 4096;

var
  ___FILE_ERROR_STR : string;

implementation
uses RTLConsts, TntSystem, TntSysUtils, TntClasses;
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

function GetAbsolutePath(basePath: wideString; relativePath: wideString): wideString;
var
   path: wideString;
begin
    SetLength(path, 5*MAX_PATH);
    PathCombineW(@path[1], PWideChar(basePath), PWideChar(relativePath));
    Result:= PWideChar(path);
end;

function FileNameIsValid( const fn : wideString ) : boolean;
var
  i, l : integer;
  d : wideString;
begin
  result := false;
  if ( fn = '' ) then exit; // blank is not valid!
  if (( not PERMIT_SPACES_IN_FILENAMES ) and ( pos( #32, fn ) > 0 )) then exit;

  d := wideExtractfilepath( fn );
  if (( d <> '' ) and ( not directoryexists( d ))) then exit;
  // for the filename to be valid, the directory must already exist.
  // if you don't like that, only pass bare filenames to this function,
  // not full paths

  d := WideExtractFilename( fn );
  if ( d = '' ) then exit; // blank filename is not valid
  l := length( d );
  for i := 1 to l do
    if ( pos( d[i], INVALID_FILENAME_CHARS ) > 0 ) then exit;

  result := true;
end; // FileNameIsValid


function MakeValidFilename(
  const s : wideString;
  const AdditionalValidChars : CharacterSet;
  const maxlen : integer
  ) : wideString;

var
  i : integer;
  r : wideString;
  ext : wideString;
begin
  r := trim( s );

  // replace invalid characters with underscores
  for i := 1 to length( r ) do
  begin
    if ( not ( IsCharAlphaNumericW( r[i] ) or ( r[i] in FileNameChars ) or ( r[i] in AdditionalValidChars ))) then
      r[i] := '_';
  end;

  // compress multiple underscores
  i := pos( '__', r );
  while ( i > 0 ) do
  begin
    delete( r, i, 1 );
    i := pos( '__', r );
  end;

  // trim leading and trailing underscores
  while (( r <> '' ) and ( r[1] = '_' )) do
    delete( r, 1, 1 );
  while (( r <> '' ) and ( r[length( r )] = '_' )) do
    delete( r, length( r ), 1 );

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

function RandomFileName( aPath : wideString;
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
  until ( not WideFileExists( aPath + '\' + t + aExt ));

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

function GetTextFileType( const fn : wideString ) : TTextFileType;
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

function GetFileSize( const fn : wideString ) : longint;
var
   f : TTntFileStream;
begin
   F:= TTntFileStream.Create( fn, ( fmOpenRead ));
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

function GetFileDateStamp( const fn : wideString ) : TDateTime;
var
   fh : integer;
begin
   try
     fh := WideFileOpen( fn, 0 );
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

procedure GetFileState( const FN : wideString; var FileState : TFileState );
begin
  FileState.Name := FN;
  try
    if wideFileexists( FN ) then
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
      FileState.Time := 0;
      FileState.Size := -1;
  end;
end; // GetFileState

function GetFilesInFolder( const folder, mask : wideString;
                           const LowCase, AddDirName : boolean;
                           FL : TWideStrings {TStringList} ) : integer;
var
   DirInfo : TSearchRecW;
   FindResult : integer;
begin

   result := 0;
   if (( not assigned( FL )) or ( not directoryexists( folder ))) then exit;
   // FL.Clear; Do NOT clear - some programs rely on it
   // (just add items to the list; calling app is responsible for clearing)
   FindResult := WideFindFirst( folder + mask, faAnyFile, DirInfo );
   while ( FindResult = 0 ) do
   begin
     if LowCase then
       DirInfo.Name := lowercase( DirInfo.Name );
     if AddDirName then
       FL.Add( folder + DirInfo.Name )
     else
       FL.Add( DirInfo.Name );
     FindResult := WideFindNext( DirInfo );
   end;
   result := FL.Count;
   WideFindClose( DirInfo );
end; // GetFilesInFolder

function GetSubfoldersList( folder : wideString;
                           const LowCase : boolean;
                           FL : TWideStrings {TStringList} ) : integer;
var
  DirInfo : TSearchRec;
  FindResult : integer;
begin
  result := 0;
  if (( folder <> '' ) and ( folder[length( folder )] <> '\' )) then
    folder := folder + '\';
  if (( not assigned( FL )) or ( not directoryexists( folder ))) then exit;
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
  SysUtils.FindClose( DirInfo );
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
  result := ( IsCharAlphaNumericA( Key ) or
    ( Key in FileNameChars ));
end; // IsValidFileChar

function ExtractFileNameNoExt( const fn : wideString ) : wideString;
var
  p : integer;
begin
  result := WideExtractFilename( fn );
  p := lastpos( '.', result );
  if ( p > 2 ) then
    delete( result, p, length( result ));
end; // ExtractFileNameNoExt

function GetUniqueFileName( const folder, fn : wideString ) : wideString;
var
  name, ext : wideString;
  i : integer;
begin
  result := fn;
  if WideFileexists( folder + fn ) then
  begin
    ext := extractfileext( fn );
    name := WideExtractFilename( fn );
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
    until ( not WideFileexists( folder + result ));
  end;
end; // GetUniqueFileName


function FilesAreOfSameType( const aList : TWideStringList ) : boolean;
var
  i : integer;
  ext, prevext : wideString;
begin
  result := true;
  if ( aList.Count < 2 ) then exit;
  prevext := wideLowercase( extractfileext( aList[0] ));
  for i := 2 to aList.Count do
  begin
    ext := wideLowercase( extractfileext( aList[pred( i )] ));
    if ( ext <> prevext ) then
    begin
      result := false;
      exit;
    end;
    prevext := ext;
  end;
end; // FilesAreOfSameType

procedure WideCutFirstDirectory(var S: WideString);
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

function WideMinimizeName(const Filename: WideString; Canvas: TCanvas;
  MaxLen: Integer): WideString;
var
  Drive: WideString;
  Dir: WideString;
  Name: WideString;
begin
  Result := FileName;
  Dir := WideExtractFilePath(Result);
  Name := WideExtractFileName(Result);

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
      WideCutFirstDirectory(Dir);
    Result := Drive + Dir + Name;
  end;
end;


// In Notepad included with Windows, we can choose 3 encoding formats in Unicode.
// These are "Unicode" (UTF16-little Endian), "Unicode big Endian" (UTF16-big Endian),
// and "UTF-8". We can use only UTF16-little endian of these formats as an INI file format.
// The other encodings do not work correctly (you examine it once). The reason
// is that Windows NT, 2000 or XP uses the encoding internally. This is why Windows
// particularly names UTF16-little Endian "Unicode".
//   Reference:
//    * Unicode Enabled - about Unicode around Windows: http://www.microsoft.com/globaldev/getwr/steps/wrg_unicode.mspx

// ensureUnicode: If not UTF16-little Endian -> File will be renamed as .bak and created
// again (blank) as UTF16-little Endian

(*
constructor TWIniFile.Create(const FileName: string; ensureUnicode: boolean);
var
  F: File;
  v: Word;
const
  wBOM = $FEFF;           // UTF16-little Endian
begin
  FFileName:= FileName;
  if ensureUnicode then begin
      AssignFile( F, filename );
      FileMode := fmOpenRead;
      Reset(F, 1);
      if not Eof(F) then
         BlockRead(f, v, 2);
      CloseFile(f);

      if v <> wBOM then begin
         DeleteFile(FileName + '.bak');
         Rename(F, FileName + '.bak');
         AssignFile( F, Filename );
         ReWrite(F, 1);
         v:= wBOM;
         BlockWrite(F, v, 2);
         CloseFile(F);
      end;
  end;
  inherited Create(FileName);
end;

function TWIniFile.ReadStringW(const Section, Ident, Default: WideString): WideString;
var
  Buffer: array[0..1024] of WideChar;
begin

  SetString(Result, Buffer, GetPrivateProfileStringW(PWideChar(Section),
    PWideChar(Ident), PWideChar(Default), Buffer, SizeOf(Buffer), PWideChar(FFileName)));
end;

procedure TWIniFile.WriteStringW(const Section, Ident, Value: WideString);
begin
  if not WritePrivateProfileStringW(PWideChar(Section), PWideChar(Ident),
                                   PWideChar(Value), PWideChar(FFileName)) then
    raise EIniFileException.CreateResFmt(@SIniFileWriteError, [FFileName]);
end;
*)

function TWIniFile.ReadStringW(const Section, Ident, Default: WideString): WideString;
begin
   Result:= TryUTF8ToWideString(ReadString(Section, Ident, Default));
end;

procedure TWIniFile.WriteStringW(const Section, Ident, Value: WideString);
var
   S: string;
begin
   S:= string(Value);
   if Value = WideString(S) then
      WriteString(Section, Ident, S)    // To make it more compatible with older versions. Only use UTF8 if it's necessary
   else
      WriteString(Section, Ident, WideStringToUTF8(Value));
end;


END.
