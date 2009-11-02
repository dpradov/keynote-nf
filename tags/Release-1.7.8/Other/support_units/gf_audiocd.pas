unit gf_audiocd;
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

 The Original Code is "gf_AudioCD.pas".

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

Uses Classes, Windows, SysUtils, ShellAPI, FileCtrl, IniFiles,
     gf_misc;

function GetAudioCDSerialNumber( DriveLetter : char ) : integer;
function EnumerateCDDrives: string;
function GetAudioCDInformation( const drive : char; const inifn : string; var Artist, Title : string ) : boolean;
function GetAudioCDInfoFromSerialNumber( const theSerial : integer; const inifn : string; var Artist, Title : string ) : boolean;
function IsAudioCD( const Drive : char; var theSerial : longint ) : boolean;

implementation


function GetAudioCDSerialNumber( DriveLetter : char ) : integer;
var
   root, name, system : string;
   flags, length      : dword;
   serial             : pdword;
   numer_seryjny      : integer;
begin
   SetLength(name,50);
   SetLength(system,50);
   New(serial);
   root := DriveLetter + ':\';
   GetVolumeInformation( PChar( root ),PChar( name ),50,serial,length,flags,PChar( system ),50 );
   numer_seryjny:=serial^;
   Dispose( serial );
   result := numer_seryjny;
end; // GetAudioCDSerialNumber

function EnumerateCDDrives : string;
var
   drive : char;
   drivelist : string;
begin
   result := '';
   drivelist := '';
   for drive := 'a' to 'z' do
     if ( GetDriveType( PChar( drive + ':\' )) = DRIVE_CDROM ) then drivelist := drivelist + drive;
   result := drivelist;
end; // EnumerateCDDrives

function GetAudioCDInformation( const drive : char; const inifn : string; var Artist, Title : string ) : boolean;
var
   IniFile : TIniFile;
   SerialNumber : integer;
   SerialString : string;
begin
   result := false;
   Artist := '';
   Title := '';
   if ( not fileexists( inifn )) then
       exit;
   SerialNumber := GetAudioCDSerialNumber( drive );
   if ( SerialNumber < 1 ) then
       exit;
   SerialString := uppercase( inttohex( SerialNumber, 6 ));
   IniFile := TIniFile.Create( inifn );
   with IniFile do
   begin
       Artist := readstring( SerialString, 'artist', '' );
       Title := readstring( SerialString, 'title', '' );
   end;
   IniFile.Free;
   result := (( title <> '' ) or ( artist <> '' ));
end; // GetAudioCDInformation


function GetAudioCDInfoFromSerialNumber( const theSerial : integer; const inifn : string; var Artist, Title : string ) : boolean;
var
   IniFile : TIniFile;
   SerialString : string;
begin
   result := false;
   Artist := '';
   Title := '';
   if ( not fileexists( inifn )) then exit;
   if ( theSerial < 1 ) then exit;
   SerialString := uppercase( inttohex( theSerial, 6 ));
   IniFile := TIniFile.Create( inifn );
   with IniFile do
   begin
       Artist := readstring( SerialString, 'artist', '' );
       Title := readstring( SerialString, 'title', '' );
   end;
   IniFile.Free;
   result := (( title <> '' ) or ( artist <> '' ));
end; // GetAudioCDInfoFromSerialNumber


function IsAudioCD(
  const Drive : char;
  var theSerial : longint ) : boolean;
var
  DrivePath : string;
  MaximumComponentLength : DWORD;
  FileSystemFlags : DWORD;
  VolumeName : string;
  serial : pdword;

begin
  Result := false;
  DrivePath := Drive + ':\';
  if GetDriveType( PChar( DrivePath )) <> DRIVE_CDROM then exit;
  SetLength(VolumeName, 64);
  theSerial := 0;
  new( serial );
  GetVolumeInformation( PChar( DrivePath ),
   PChar( VolumeName ),
   Length( VolumeName ),
   serial,
   MaximumComponentLength,
   FileSystemFlags,
   nil,
   0);
  result := ( lStrCmp( PChar( VolumeName ),'Audio CD' ) = 0 );
  theserial := serial^;
  dispose( serial );
end; // IsAudioCD

end.
