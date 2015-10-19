unit kn_Macro;

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
uses Forms, Windows, Classes, Controls,
  SysUtils, ShellAPI, Dialogs, wideStrings,
  gf_misc, gf_files, kn_Const, kn_Info,
  kn_Cmd, kn_MacroCmd, gf_strings;

const
  _MACRO_VERSION_MAJOR = '1';
  _MACRO_VERSION_MINOR = '0';

const
  // filenames for AutoRun macros
  _MACRO_AUTORUN_PREFIX   = '_auto';
  _MACRO_AUTORUN_STARTUP  = _MACRO_AUTORUN_PREFIX + 'startup' + ext_Macro;
  _MACRO_AUTORUN_NEW_NOTE = _MACRO_AUTORUN_PREFIX + 'newnote' + ext_Macro;
  _MACRO_AUTORUN_NEW_TREE = _MACRO_AUTORUN_PREFIX + 'newtree' + ext_Macro;
  _MACRO_AUTORUN_NEW_FILE = _MACRO_AUTORUN_PREFIX + 'newfile' + ext_Macro;
  _MACRO_AUTORUN_NEW_NODE = _MACRO_AUTORUN_PREFIX + 'newnode' + ext_Macro;


type
  TMacroVersion = record
    Major, Minor : char;
  end;

type
  TMacro = class( TObject )
  private
    LoadOnlyInfo : boolean;
    ErrStr : string;

    function ParseInfoLine( infostr : wideString ) : boolean;
    function MakeInfoLine : wideString;

  public
    FileName : string;
    Name : WideString;
    Description : wideString;
    DateModified : string;
    Version : TMacroVersion;
    Lines : TStringList;
    AbortOnError : boolean;

    constructor Create;
    destructor Destroy; override;

    function Load : boolean;
    function LoadInfo : boolean;
    function Save : boolean;
    procedure Clear;

    function LastError : string;


  end;

var
  Macro_Folder : string;
  Macro_List : TWideStringList;

procedure LoadMacroList;
procedure ClearMacroList;
function MakeMacroFileName( const s : string ) : string;

implementation
uses TntSysUtils, TntSystem;

resourcestring
  STR_Macro_01 = 'Invalid macro header';
  STR_Macro_02 = 'Invalid macro version information';
  STR_Macro_03 = 'Error while loading macro "%s": %s' + #13#13 + 'Continue loading macros?';
  STR_Macro_04 = 'Unexpected error while loading macro "%s": %s';

constructor TMacro.Create;
begin
  inherited Create;
  Lines := TStringList.Create;
  LoadOnlyInfo := false;
  Name := '';
  Description := '';
  DateModified := DateTimeToStr( now );
  Version.Major := _MACRO_VERSION_MAJOR;
  Version.Minor := _MACRO_VERSION_MINOR;
  AbortOnError := true;
end; // Create

destructor TMacro.Destroy;
begin
  Lines.Free;
  inherited Destroy;
end; // Destroy

function TMacro.Load : boolean;
var
  f : TextFile;
  s, fn : string;
begin
  result := false;
  Lines.Clear;

  if ( pos( '\', FileName ) = 0 ) then
    fn := Macro_Folder + FileName
  else
    fn := FileName;

  assignfile( f, fn );
  try
    reset( f );
  except
    On E : Exception do
    begin
      ErrStr := E.Message;
      exit;
    end;
  end;

  try
    try
      if not eof( f ) then
        readln( f, s );

      result := ParseInfoLine( TryUTF8ToWideString(s) );
      if (( not result ) or LoadOnlyInfo ) then
        exit;

      while not eof( f ) do
      begin
        readln( f, s );
        Lines.Add( s );
      end;

      result := true;

    except
      On E : Exception do
      begin
        ErrStr := E.Message;
        exit;
        result := false;
        Lines.Clear;
      end;
    end;
  finally
    closefile( f );
  end;

end; // Load

procedure TMacro.Clear;
begin
  Lines.Clear;
end; // Clear

function TMacro.LoadInfo : boolean;
begin
  try
    LoadOnlyInfo := true;
    result := Load;
  finally
    LoadOnlyInfo := false;
  end;
end; // LoadInfo

function TMacro.Save : boolean;
var
  f : TextFile;
  i : integer;
  fn : string;
begin
  result := false;
  DateModified := DateTimeToStr( now );

  if ( pos( '\', FileName ) = 0 ) then
    fn := Macro_Folder + FileName
  else
    fn := FileName;

  assignfile( f, fn );
  try
    rewrite( f );
  except
    On E : Exception do
    begin
      ErrStr := E.Message;
      exit;
    end;
  end;

  try
    try
      writeln( f, WideStringToUTF8(MakeInfoLine) );
      for i := 1 to Lines.Count do
        writeln( f, Lines[pred( i )] );
      result := true;
    except
      On E : Exception do
      begin
        ErrStr := E.Message;
        exit;
      end;              
    end;
  finally
    closefile( f );
  end;

end; // Save

function TMacro.ParseInfoLine( infostr : WideString ) : boolean;
var
  p, q : integer;
  s : wideString;
begin
  result := false;

  {
  Info string syntax is as follows:
  ;Version|Macro Name|Description|Date modified
  }

  ErrStr := STR_Macro_01;
  if ( infostr = '' ) then exit;
  if ( infostr[1] <> _MACRO_COMMENT_CHAR ) then exit;
  delete( infostr, 1, 1 );

  p := pos( _MACRO_DELIMITER_CHAR, infostr );
  if ( p = 0 ) then exit;

  s := copy( infostr, 1, p-1 );
  delete( infostr, 1, p );

  // validate version string
  ErrStr := STR_Macro_02;
  if ( length( s ) < 3 ) then exit;
  q := pos( '.', s );
  if ( q = 0 ) then exit;
  Version.Major := Char(s[pred( q )]);
  Version.Minor := Char(s[succ( q )]);

  ErrStr := '';
  result := true;

  p := pos( _MACRO_DELIMITER_CHAR, infostr );
  Name := copy( infostr, 1, p-1 );
  delete( infostr, 1, p );
  if ( Name = '' ) then
    Name := WideExtractFilename( FileName );

  p := pos( _MACRO_DELIMITER_CHAR, infostr );
  Description := copy( infostr, 1, p-1 );
  delete( infostr, 1, p );

  p := pos( _MACRO_DELIMITER_CHAR, infostr );
  AbortOnError := copy( infostr, 1, p-1 ) = BOOLEANSTR[true];
  delete( infostr, 1, p );

  DateModified := infostr;

end; // ParseInfoLine

function TMacro.MakeInfoLine : wideString;
begin
  result := _MACRO_COMMENT_CHAR +
    WideFormat(
      '%s.%s',
      [Version.Major, Version.Minor]
    ) + _MACRO_DELIMITER_CHAR +
    Name + _MACRO_DELIMITER_CHAR +
    Description + _MACRO_DELIMITER_CHAR +
    BOOLEANSTR[AbortOnError] + _MACRO_DELIMITER_CHAR +
    DateModified;
end; // MakeInfoLine

function TMacro.LastError : string;
begin
  result := ErrStr;
  ErrStr := '';
end; // LastError

procedure LoadMacroList;
var
  DirInfo : TSearchRec;
  FindResult : integer;
  Macro : TMacro;
begin
  Macro_List.BeginUpdate;

  FindResult := FindFirst(
    Macro_Folder + '*' + ext_MACRO,
    faAnyFile,
    DirInfo );

  try
    try
      while ( FindResult = 0 ) do
      begin
        Macro := TMacro.Create;
        Macro.FileName := ansilowercase( DirInfo.Name );
        if Macro.LoadInfo then
        begin
          Macro_List.AddObject( Macro.Name, Macro );
        end
        else
        begin
          if ( messagedlg( Format(STR_Macro_03,[DirInfo.Name, Macro.LastError]),
            mtWarning, [mbYes, mbNo], 0 ) <> mrYes ) then
              FindResult := -1; // will abort loop
          Macro.Free;
        end;
        FindResult := FindNext( DirInfo );
      end;

    except
      ON E : Exception do
      begin
        messagedlg( Format(STR_Macro_04, [DirInfo.Name, E.Message] ), mtError, [mbOK], 0 );
      end;
    end;

  finally
    Macro_List.EndUpdate;
    SysUtils.FindClose( DirInfo );
  end;

end; // LoadMacroList

procedure ClearMacroList;
var
  i : integer;
begin
  try
    for i := 1 to Macro_List.Count do
    begin
      try
        TMacro( Macro_List.Objects[pred( i )] ).Free;
      except
      end;
    end;
  finally
    Macro_List.Clear;
  end;
end; // ClearMacroList

function MakeMacroFileName( const s : string ) : string;
var
  i : integer;
  fn : string;
begin
  i := 1;

  fn := MakeValidFilename( s, [], MAX_FILENAME_LENGTH );
  result := fn + ext_Macro;
  while fileexists( Macro_Folder + result ) do
  begin
    result := Format( '%s%d%s', [fn,i,ext_Macro] );
    inc( i );
  end;

end; // MakeMacroFileName


(* UNUSED
function GetDefaultIntegerArg( const cmd : TMacroCmd ) : integer;
function GetDefaultStringArg( const cmd : TMacroCmd ) : string;

function GetDefaultIntegerArg( const cmd : TMacroCmd ) : integer;
begin
  result := 0;
  case cmd of
    macWait : result := 250; // miliseconds
    macGoLeft, macGoRight, macGoDown, macGoUp : result := 1;
  end;
end; // GetDefaultIntegerArg

function GetDefaultStringArg( const cmd : TMacroCmd ) : string;
begin
  result := '';
  case cmd of
    macStatus : result := '';
    macFontColor : result := 'clBlack';
    macBGColor : result := 'clWindow';
    macHighlightColor : result := 'clInfoBk';
  end;
end; // GetDefaultStringArg
*)

initialization

  Macro_Folder := properfoldername( extractfilepath( application.exename ) + _MACRO_FOLDER );
  Macro_List := TWideStringList.Create;
  Macro_List.Sorted := true;
  Macro_List.Duplicates := dupError;

finalization

  try
    ClearMacroList;
    Macro_List.Free;
  except
  end;


end.
