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

unit gf_files;

interface

uses Windows, Classes, Controls,
     SysUtils, FileCtrl, Dialogs,
     ShellAPI, gf_strings, gf_misc;

const
  // set of characters accepted in filenames (very restrictive)
  FileNameChars  = ['_', '-', '(', ')', '!', '$', '.', {'&',} ','];
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
  TTestBuffer = array[1..MaxTestBufSize] of char;
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
function CheckDir( const aDir : string; const AutoCreate : boolean ) : boolean;
function FilesAreOfSameType( const aList : TStringList ) : boolean;
procedure GetFileState( const FN : string; var FileState : TFileState );

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

const
  MaxFileBuf = 4096;

var
  ___FILE_ERROR_STR : string;

implementation

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

function FileNameIsValid( const fn : string ) : boolean;
var
  i, l : integer;
  d : string;
begin
  result := false;
  if ( fn = '' ) then exit; // blank is not valid!
  if (( not PERMIT_SPACES_IN_FILENAMES ) and ( pos( #32, fn ) > 0 )) then exit;

  d := extractfilepath( fn );
  if (( d <> '' ) and ( not directoryexists( d ))) then exit;
  // for the filename to be valid, the directory must already exist.
  // if you don't like that, only pass bare filenames to this function,
  // not full paths

  d := extractfilename( fn );
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

  // replace invalid characters with underscores
  for i := 1 to length( r ) do
  begin
    if ( not ( IsCharAlphaNumericA( r[i] ) or ( r[i] in FileNameChars ) or ( r[i] in AdditionalValidChars ))) then
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
       getdir( 0, ThisPath )
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
   f : file;
begin
   assignfile( f, fn );
   FileMode := 0;
   try
     try
       reset( f, 1 );
       result := FileSize( f );
       closefile( f );
     except
       on E :Exception do
       begin
         ___FILE_ERROR_STR := E.Message;
         result := -1;
       end;
     end;
   finally
     FileMode := 2;
   end;
end; // GetFileSize

function GetFileDateStamp( const fn : string ) : TDateTime;
var
   fh : integer;
begin
   try
     fh := FileOpen( fn, 0 );
     result := FileDateToDateTime( FileGetDate( fh ));
     FileClose( fh );
   except
     on E :Exception do
     begin
       ___FILE_ERROR_STR := E.Message;
       result := 0;
     end;
   end;
end; // GetFileDateStamp

function CheckDir( const aDir : string; const AutoCreate : boolean ) : boolean;
var
  io : integer;
begin
  result := DirectoryExists( aDir );
  if (( not result ) and AutoCreate )then
  begin
    if ( messagedlg( 'Directory does not exist:' +#13+
      aDir +#13+ 'Attempt to create directory?', mtError, [mbYes,mbNo], 0 ) = mrYes ) then
      begin
        {$I-}
        mkdir( aDir );
        io := IOResult;
        {$I+}
        result := ( io = 0 );
        if result then
          messagedlg( 'Directory successfully created.', mtInformation, [mbOK], 0 )
        else
          messagedlg( 'Failed to create directory. Error code: ' + inttostr( io ), mtError, [mbOK], 0 );
      end;
  end;
end; // CheckDir

procedure GetFileState( const FN : string; var FileState : TFileState );
begin
  FileState.Name := FN;
  try
    if fileexists( FN ) then
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

function GetFilesInFolder( const folder, mask : string;
                           const LowCase, AddDirName : boolean;
                           FL : TStrings {TStringList} ) : integer;
var
   DirInfo : TSearchRec;
   FindResult : integer;
begin

   result := 0;
   if (( not assigned( FL )) or ( not directoryexists( folder ))) then exit;
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
   SysUtils.FindClose( DirInfo );
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

function ExtractFileNameNoExt( const fn : string ) : string;
var
  p : integer;
begin
  result := extractfilename( fn );
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
  if fileexists( folder + fn ) then
  begin
    ext := extractfileext( fn );
    name := extractfilename( fn );
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
    until ( not fileexists( folder + result ));
  end;
end; // GetUniqueFileName


function FilesAreOfSameType( const aList : TStringList ) : boolean;
var
  i : integer;
  ext, prevext : string;
begin
  result := true;
  if ( aList.Count < 2 ) then exit;
  prevext := ansilowercase( extractfileext( aList[0] ));
  for i := 2 to aList.Count do
  begin
    ext := ansilowercase( extractfileext( aList[pred( i )] ));
    if ( ext <> prevext ) then
    begin
      result := false;
      exit;
    end;
    prevext := ext;
  end;
end; // FilesAreOfSameType

END.
