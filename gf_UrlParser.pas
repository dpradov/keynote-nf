unit gf_URLParser;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 
 Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
 in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 


(*
  reserved charscters in urls:
  ";", "/", "?", ":", "@", "=", "&"

  reserved characters in HTTP user and password fields:
  ":", "@" "/" (must be URL-encoded)

  within MAILTO urls, the "%" sign must be encoded

  "unsafe" characters in URls (RFC-1738)
  " ", "<", ">", "#", '"', "%",
  "[", "]", "{", "}", "|", "\",
  "^", "~", "`"

*)

interface
uses Windows, SysUtils, Classes, gf_misc, gf_files;

const
  _URL_RESERVED_CHARS = [
    ';', '/', '?', ':', '@', '=', '&'
  ];

  _URL_UNSAFE_CHARS = [
    ' ', '<', '>', '#', '"', '%',
    '[', ']', '{', '}', '|', '\',
    '^', '~', '`'
  ];

  _URL_NORMAL_CHARS = [
    'A'..'Z', 'a'..'z', '0'..'9',
    '$', '-', '_', '.', '+', '!', '*', '''', '(', ')', ','
  ];

type
  TURLScheme = (
    { RFC-1738 schemes }
    urlHTTP,
    urlHTTPS,
    urlFTP,
    urlMAILTO,
    urlFILE,
    urlTELNET,
    urlNEWS,
    urlNNTP,
    urlGOPHER,
    urlWAIS,
    urlPROSPERO,
    { additional proprietary }
    urlPNM, {RealAudio}
    urlMMS, {MS Media Player streaming audio/video}
    { other }
    urlUNKNOWN
  );

  TProtocols = set of TURLScheme;

const
  // url scheme prefixes
  _URL_SCHEMES : array[TURLScheme] of string = (
    'http://',
    'https://',
    'ftp://',
    'mailto:',
    'file://',
    'telnet://',
    'news:',
    'nntp://',
    'gopher://',
    'wais://',
    'prospero://',
    'pnm://',
    'mms://',
    ''
  );

  _URL_SCHEME_NAMES : array[TURLScheme] of string = (
    'http',
    'https',
    'ftp',
    'mailto',
    'file',
    'telnet',
    'news',
    'nntp',
    'gopher',
    'wais',
    'prospero',
    'pnm',
    'mms',
    ''
  );
  // standard port numbers,
  // http://www.isi.edu/in-notes/iana/assignments/port-numbers
  _URL_SCHEME_PORTS : array[TURLScheme] of integer = (
    80,   { http }
    443,  { https }
    21,   { ftp control port; ftp data goes on port 20 }
    25,   { mailto }
    0,    { file }
    23,   { telnet }
    532,  { news ?? }
    119,  { nntp }
    70,   { gopher }
    0,    { wais }
    1525, { prospero (also uses several other posrts }
    0,    { pnm }
    0,    { mms }
    0     { unknown }
  );



type
  EURLError = class( Exception );

  TURLErrorEvent = procedure(
    sender : TObject; const ErrorMsg : string
  ) of object;

  TURL = class( TObject )
  private
    FText : string;
    FURLScheme : TURLScheme;
    FScheme : string;
    FHost : string; // host name, e.g. "ftp.simtel.net"
    FPort : string; // port number, as string
    FPath : string; // full path, e.g. "/users/johndoe/"
    FFilename : string; // filename, e.g. "index.html"
    FQuery : string; // CGI query string, including the leading '?'
    FFragment : string; // document fragment, including the leading '#'
    FUsername : string;
    FPassword : string;
    FParams : string;
    FValid : boolean;
    FThrowExceptions : boolean;

    // events
    FOnURLError : TURLErrorEvent;

    procedure ClearAllFields;
    procedure DoError( const ErrorMsg : string );

    procedure SetText(const Value: string);
    procedure SetURLScheme(const Value: TURLScheme);
    procedure SetFragment(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetPath(const Value: string);
    procedure SetPort(const Value: string);
    procedure SetQuery(const Value: string);
    procedure SetScheme(const Value: string);
    procedure SetUsername(const Value: string);
    procedure SetFilename(const Value: string);
    procedure SetHost(const Value: string);
    procedure SetParams(const Value: string);
    function GetValid: boolean;


  public
    property Text : string read FText write SetText;
    property URLScheme : TURLScheme read FURLScheme write SetURLScheme;
    property Scheme : string read FScheme write SetScheme;
    property Host : string read FHost write SetHost;
    property Port : string read FPort write SetPort;
    property Path : string read FPath write SetPath;
    property Filename : string read FFilename write SetFilename;
    property Query : string read FQuery write SetQuery;
    property Fragment : string read FFragment write SetFragment;
    property Username : string read FUsername write SetUsername;
    property Password : string read FPassword write SetPassword;
    property Params : string read FParams write SetParams;
    property Valid : boolean read GetValid;
    property ThrowExceptions : boolean read FThrowExceptions write FThrowExceptions;

    // events
    property OnURLError : TURLErrorEvent read FOnURLError write FOnURLError;

    constructor Create; overload;
    constructor Create( const aText : string ); overload;

    destructor Destroy; override;

    function Assemble( const Abbreviated : boolean ) : string;
    function Canonical : string;
    function ExpandRelativeURL( RelativeURL : string ) : string;

    function GetLoginStr : string;
    function GetPortStr( const Abbreviated : boolean ) : string;
    function GetParamStr : string;
    function GetQueryStr : string;
    function GetFragmentStr( const Abbreviated : boolean ) : string;

    function GetLocalFilename( const MakeUnique : boolean; const folder : string ) : string;

  end;

function ExtractURLScheme( const URL : string; var SchemeStr : string ) : TURLScheme;
function LargerInt( const a, b : integer ) : integer;
function URLEncode( const s : string ) : string;
function URLIsRelative( URL : string ) : boolean;

function StringFilterMatch( const Filter : string; const s : string ) : boolean;

implementation

function StringFilterMatch( const Filter : string; const s : string ) : boolean;
begin
  result := ( pos( '|' + s + '|', Filter ) > 0 );
end; // StringFilterMatch

function URLIsRelative( URL : string ) : boolean;
var
  prot : TURLScheme;
begin
  result := true;
  if ( URL = '' ) then exit;

  if ( URL[1] in ['/','.'] ) then exit;

  URL := lowercase( URL );
  for prot := low( TURLScheme ) to pred( high( TURLScheme )) do
  begin
    if ( pos( _URL_SCHEMES[prot], URL ) = 1 ) then
    begin
      result := false;
      break;
    end;
  end;
end; // URLIsRelative


function URLEncode( const s : string ) : string;
var
  i, len : integer;
begin
  result := '';
  len := length( s );
  for i := 1 to len do
  begin
    if ( s[i] in _URL_NORMAL_CHARS ) then
    begin
      result := result + s[i];
    end
    else
    begin
      result := result + '%' + IntToHex( ord( s[i] ), 2 );
    end;
  end;
end; // URLEncode

function LargerInt( const a, b : integer ) : integer;
begin
  if ( a > b ) then
    result := a
  else
    result := b; // if they're equal, it doesn't matter which we return
end; // LargerInt

function ExtractURLScheme( const URL : string; var SchemeStr : string ) : TURLScheme;
var
  aScheme : TURLScheme;
  tmps : string;
  p : integer;
begin
  result := urlUNKNOWN;
  SchemeStr := '';
  if ( URL = '' ) then exit;
  tmps := lowercase( URL );

  for aScheme := low( TURLScheme ) to high( TURLScheme ) do
  begin
    if ( pos( _URL_SCHEMES[aScheme], tmps ) = 1 ) then
    begin
      result := aScheme;
      SchemeStr := _URL_SCHEMES[result];
      break;
    end;
  end;

  if ( result <> urlUNKNOWN ) then exit;

  p := pos( '://', tmps );
  if ( p > 2 ) then
  begin
    SchemeStr := copy( URL, 1, p+2 );
  end
  else
  begin
    p := pos( ':', tmps );
    if ( p > 2 ) then
    begin
      SchemeStr := copy( URL, 1, p );
    end
    else
    begin
      // appears to have no scheme prefix.
      // Use simple heuristics instead
      if ( pos( ':\', tmps ) = 2 ) then
      begin
        result := urlFILE;
      end
      else
      if (( pos( 'www.', tmps ) = 1 ) or
         ( pos( '.htm', tmps ) > lastpos( '/', tmps ))) then
      begin
        result := urlHTTP;
      end
      else
      if ( pos( 'ftp.', tmps ) = 1 ) then
      begin
        result := urlFTP;
      end
      else
      begin
        p := pos( '@', tmps );
        if (( p > 0 ) and ( pos( '/', tmps ) = 0 )) then
        begin
          { assume MAILTO }
          { this fails on vaguely invalid URLs such as: }
          { user:pass@www.xxx.com }
          { www.xxx.com/cgi-bin/foo?some@query-string }
          result := urlMAILTO;
        end;
      end;
    end;
  end;
end; // ExtractURLScheme

{ TURL }


constructor TURL.Create;
begin
  inherited Create;
  FText := '';
  FOnURLError := nil;
  FThrowExceptions := true;
  ClearAllFields;
end; // CREATE {1}

constructor TURL.Create(const aText: string);
begin
  Create;
  FThrowExceptions := false;
  try
    SetText( aText );
  finally
    FThrowExceptions := true;
  end;
end; // CREATE {2}

destructor TURL.Destroy;
begin
  inherited Destroy;
end; // DESTROY

function TURL.Assemble(const Abbreviated : boolean ): string;
var
  hoststr : string;
begin

  case FURLScheme of

    urlHTTP, urlHTTPS : begin
      result := Format(
        '%s%s%s%s%s%s%s%s%s',
        [_URL_SCHEMES[FURLScheme], GetLoginStr, FHost, GetPortStr( Abbreviated ), FPath, FFilename, GetParamStr, GetQueryStr, GetFragmentStr( Abbreviated )]
      );
    end; // HTTP, Https

    urlFTP, urlTELNET : begin
      result := Format(
        '%s%s%s%s%s%s',
        [_URL_SCHEMES[FURLScheme], GetLoginStr, FHost, GetPortStr( Abbreviated ), FPath, FFilename]
      );

    end; // FTP, Telnet

    urlMAILTO : begin
      result := Format(
        '%s@%s',
        [FUsername, FHost]
      );
      if ( not Abbreviated ) then
        result := _URL_SCHEMES[urlMAILTO] + result;
    end; // Mailto

    urlFILE : begin
      if Abbreviated then
      begin
        if (( FHost = '/' ) or ( FHost = 'localhost' )) then
          hoststr := ''
        else
          hoststr := FHost;
        result := Format(
          '%s%s%s',
          [hoststr, FPath, FFileName]
        );
      end
      else
      begin
        result := Format(
          '%s%s%s%s',
          [_URL_SCHEMES[urlFILE], FHost, FPath, FFileName]
        );
      end;
    end; // File

    else // schemes not currently parsed
    begin
      result := FText;
    end;

  end; // case FURLScheme
end; // Assemble

function TURL.GetLocalFilename(const MakeUnique: boolean;
  const folder: string): string;
var
  ext : string;
begin
  if ( FFilename <> '' ) then
    result := FFilename
  else
  if ( length( FPath ) > 1 ) then
    result := FPath
  else
  if ( FHost <> '' ) then
    result := FHost
  else
    result := FText;

  if ( result <> '' ) then

  begin
    ext := extractfileext( result );
    if ( ext = '.com' ) then
    begin
      case FURLScheme of
        urlHTTP, urlHTTPS : result := result + '.html';
        else
          result := result + '.txt';
      end;
    end;
  end;

  result := MakeValidFilename( result, [' '], 127 );

  if MakeUnique then
    result := GetUniqueFileName( folder, result );

end; // GetLocalFilename

procedure TURL.SetText(const Value: string);
var
  s, pathstr, hoststr, loginstr : string;
  p, pAt, pQues, pSlash, pBSlash, pHash, pColon, pSemi : integer;
begin
  ClearAllFields;
  FText := trim( Value );

  (*
  // check if URL contains spaces, and if so, encode them.
  p := pos( #32, FText );
  while ( p > 0 ) do
  begin
    delete( FText, p, 1 );
    insert( '%20', FText, p );
    p := pos( #32, FText );
  end;
  *)

  s := FText;

  // get scheme
  FURLScheme := ExtractURLScheme( FText, FScheme );

  if ( FScheme <> '' ) then
  begin
    // URL had scheme prefix
    delete( s, 1, length( FScheme ));
  end
  else
  begin
    // URL had no scheme prefix,
    if ( FURLScheme <> urlUNKNOWN ) then
    begin
      // we inferred scheme anyway
      FScheme := _URL_SCHEMES[FURLScheme];
    end
    else
    begin
      // no scheme and no scheme prefix!
      DoError( 'Invalid URL value; cannot parse.' );
      exit;
    end;
  end;


  // we have identified the scheme, and now the string s
  // has no scheme prefix.

  if ( s = '' ) then
  begin
    DoError( 'Invalid URL value; cannot parse.' );
    exit;
  end;

  FValid := true;

  FPort := inttostr( _URL_SCHEME_PORTS[FURLScheme] ); // set default

  case FURLScheme of

    urlHTTP, urlHTTPS : begin
      { //<user>:<password>@<host>:<port>/<url-path> }
      { ;Params ?Query #Fragment }
      { Assumed: Query can contain any reserved characters, }
      { including ";" and '@', EXCEPT "#", which always delimits }
      { the "Fragment" section. According to RFC2396, if both Query }
      { and Params are present, Params must come before Query. }
      { Also assumed that if ANY segment follows the "Host" part, }
      { then it MUST be preceded by a slash, i.e. }
      { "http://user:pass@www.host.net/?query" is VALID, and }
      { "http://user:pass@www.host.net?query" is INVALID. }

      pSlash := pos( '/', s );
      pAt := pos( '@', s );

      // scan for username:password (login) segment
      if ( pAt > 0 ) then
      begin
        { possibly username:password segment, }
        { but @ sign could also be in Query or Params }
        if (( pAt < pSlash ) or ( pSlash = 0 )) then
        begin
          // we are now sure that the first "@" sign delimits the
          // username:password segment
          loginstr := copy( s, 1, pred( pAt ));
          delete( s, 1, pAt );
          pColon := pos( ':', loginstr );
          if ( pColon > 0 ) then
          begin
            FUsername := copy( loginstr, 1, pred( pColon ));
            delete( loginstr, 1, pColon );
            FPassword := loginstr;
          end
          else
          begin
            FUsername := loginstr;
            FPassword := '';
          end;
        end;
        { no ELSE here; it just means that the "@" did not belong to }
        { username:password segment }
      end;

      // string s now begins with host, possibly followed by port
      pSlash := pos( '/', s );
      if ( pSlash > 0 ) then
      begin
        hoststr := copy( s, 1, pred( pSlash ));
        delete( s, 1, pred( pSlash ));
      end
      else
      begin
        hoststr := s;
        s := '';
      end;

      pColon := pos( ':', hoststr );
      if ( pColon > 0 ) then
      begin
        FHost := copy( hoststr, 1, pred( pColon ));
        delete( hoststr, 1, pColon );
        FPort := hoststr;
      end
      else
      begin
        FHost := hoststr;
      end;

      if ( pSlash = 0 ) then
      begin
        FPath := '/';
        exit; // all done
      end;

      // string s now begins with "path" segment,
      // possibly followed by other segments
      assert( s <> '', 'Parse string is empty' );
      assert( s[1] = '/', 'Parse string does not begin with a slash.' );

      // path and file names cannot contain reserved characters,
      // '?' or ';'

      // get the Fragment section
      pHash := pos( '#', s );
      if ( pHash > 0 ) then
      begin
        FFragment := copy( s, succ( pHash ), length( s ));
        delete( s, pHash, length( s ));
      end;

      pQues := pos( '?', s );
      pSemi := pos( ';', s );

      if (( pQues = 0 ) and ( pSemi = 0 )) then
      begin
        // parse string contains path/filename only
        pathstr := s;
      end
      else
      begin
        // Params must PRECEDE Query. Otherwise,
        // if "?" comes first, everything is a query
        // and there's no "params" segment
        if ((( pSemi < pQues ) and ( pSemi > 0 )) or ( pQues = 0 )) then
        begin
          pathstr := copy( s, 1, pred( pSemi ));
          delete( s, 1, pSemi );
          dec( pQues, pSemi );
          if ( pQues > 0 ) then
          begin
            FParams := copy( s, 1, pred( pQues ));
            delete( s, 1, pQues );
            FQuery := s;
          end
          else
          begin
            FParams := s;
          end;
        end
        else
        begin
          pathstr := copy( s, 1, pred( pQues ));
          delete( s, 1, pQues );
          FQuery := s;
        end;
      end;

      // finally, splith pathstr into path and filename segments
      pSlash := lastpos( '/', pathstr );
      FPath := copy( pathstr, 1, pSlash );
      delete( pathstr, 1, pSlash );
      FFilename := pathstr;

    end; // HTTP, HTTPS

    urlFTP : begin
      { //<user>:<password>@<host>:<port>/<url-path> }
      { "url-path" part format: }
      { <cwd1>/<cwd2>/.../<cwdN>/<name>;type=<typecode> }
      { [x] "typecode" currently not supported }
      pAt := pos( '@', s ); // scan for username:password
      if ( pAt > 0 ) then
      begin
        loginstr := copy( s, 1, pred( pAt ));
        delete( s, 1, pAt );
        pColon := pos( ':', loginstr );
        if ( pColon > 0 ) then
        begin
          // both username and password
          FUsername := copy( loginstr, 1, pred( pColon ));
          delete( loginstr, 1, pColon );
          FPassword := loginstr;
        end
        else
        begin
          // only username (cannot have only password without username)
          FUsername := loginstr;
          FPassword := '';
        end;
      end;

      {
      ftp://ftp.example.com/file.zip
      ftp://ftp.example.com:555/file.zip
      ftp://ftp.example.com/dir/file.zip
      ftp://ftp.example.com:555/dir/file.zip
      }

      // string s now begins with "host" part
      pSlash := pos( '/', s ); // scan for path
      if ( pSlash > 0 ) then
      begin
        // have path
        hoststr := copy( s, 1, pred( pSlash ));
        delete( s, 1, pred( pSlash ));
      end
      else
      begin
        // no path
        hoststr := s;
        s := '';
      end;

      pColon := pos( ':', hoststr ); // scan for port
      if ( pColon > 0 ) then
      begin
        // have port
        FHost := copy( hoststr, 1, pred( pColon ));
        delete( hoststr, 1, pColon );
        FPort := hoststr;
      end
      else
      begin
        FHost := hoststr;
      end;

      // string s, if non blank, now begins with "path" part
      pSlash := lastpos( '/', s );
      if ( pSlash > 0 ) then
      begin
        FPath := copy( s, 1, pSlash );
        delete( s, 1, pSlash );
      end
      else
      begin
        FPath := '/';
      end;

      FFilename := s;

    end; // FTP

    urlMAILTO : begin
      pAt := pos( '@', s );
      if ( pAt > 0 ) then
      begin
        FUsername := copy( s, 1, pred( pAt ));
        delete( s, 1, pAt );
        FHost := s;
      end;
    end; // mailto

    urlTELNET : begin
      { telnet://<user>:<password>@<host>:<port>/ }

      pAt := pos( '@', s ); // scan for username:password
      if ( pAt > 0 ) then
      begin
        loginstr := copy( s, 1, pred( pAt ));
        delete( s, 1, pAt );
        pColon := pos( ':', loginstr );
        if ( pColon > 0 ) then
        begin
          // both username and password
          FUsername := copy( loginstr, 1, pred( pColon ));
          delete( loginstr, 1, pColon );
          FPassword := loginstr;
        end
        else
        begin
          // only username (cannot have only password without username)
          FUsername := loginstr;
          FPassword := '';
        end;
      end;

      if (( s <> '' ) and ( s[length( s )] = '/' )) then
        delete( s, length( s ), 1 );

      // string s now begins with host part
      pColon := pos( ':', s ); // scan for port
      if ( pColon > 0 ) then
      begin
        FHost := copy( s, 1, pred( pColon ));
        delete( s, 1, pColon );
        FPort := s;
      end
      else
      begin
        FHost := s;
      end;
    end; // telnet

    urlFILE : begin
      { file://<host>/<path> }

      pSlash := pos( '/', s );
      if ( pSlash > 0 ) then
      begin
        // has hostname, possibly just a "/"
        FHost := copy( s, 1, pSlash );
        delete( s, 1, pSlash );
      end
      else
      begin
        FHost := '/';
      end;

      // string s has now no hostname
      // Note: MS-DOS filenames might have '\' instead of '/'
      pSlash := lastpos( '/', s );
      pBSlash := lastpos( '\', s );

      p := LargerInt( pSlash, pBSlash );
      if ( p > 0 ) then
      begin
        FPath := copy( s, 1, p );
        delete( s, 1, p );
      end;
      FFilename := s;
    end; // file

    else
    begin
      // schemes not currently parsed
    end;

  end; // case FURLScheme


end; // SetText

procedure TURL.SetURLScheme(const Value: TURLScheme);
begin
  FURLScheme := Value;
  FScheme := _URL_SCHEMES[FURLScheme];
  Assemble( false );
end; // SetURLScheme

procedure TURL.SetFragment(const Value: string);
begin
  FFragment := Value;
  Assemble( false );
end; // SetFragment

procedure TURL.SetPassword(const Value: string);
begin
  FPassword := Value;
  Assemble( false );
end; // SetPassword

procedure TURL.SetPath(const Value: string);
begin
  FPath := Value;
  Assemble( false );
end; // SetPath

procedure TURL.SetPort(const Value: string);
begin
  try
    strtoint( Value );
    FPort := Value;
    Assemble( false );
  except
    DoError( Format(
      'Invalid Port value "%s"',
      [Value]
    ));
  end;
end; // SetPort

procedure TURL.SetQuery(const Value: string);
begin
  FQuery := Value;
  Assemble( false );
end; // SetQuery

procedure TURL.SetScheme(const Value: string);
begin
  FScheme := Value;
  Assemble( false );
end; // SetScheme

procedure TURL.SetUsername(const Value: string);
begin
  FUsername := Value;
  Assemble( false );
end; // SetUsername

function TURL.ExpandRelativeURL( RelativeURL: string ): string;
var
  tmps : string;
  p : integer;
begin
  result := '';
  if ( not ( FURLScheme in [urlHTTP, urlHTTPS, urlFTP, urlFILE] )) then exit;

  if ( RelativeURL = '' ) then
  begin
    result := FText;
    exit;
  end;

  // make sure RelativeURL really is relative
  if ( not URLIsRelative( RelativeURL )) then
  begin
    result := FText;
    exit;
  end;

  tmps := '';

  if ( RelativeURL[1] <> '/' ) then
  begin

    while ( pos( './', RelativeURL ) = 1 ) do
      delete( RelativeURL, 1, 2 );

    tmps := FPath;
    p := pos( '../', RelativeURL );
    if ( p = 1 ) then
    begin
      delete( tmps, length( tmps ), 1 ); // remova trailing slash
      repeat
        delete( RelativeURL, 1, 3 );
        p := lastpos( '/', tmps );
        if ( p > 0 ) then
          delete( tmps, p, length( tmps ));
      until ( pos( '../', RelativeURL ) <> 1 );
      RelativeURL := '/' + RelativeURL;
    end;

    // file:// scheme may use backslashes "\" instead
    if ( FURLScheme = urlFILE ) then
    begin
      while ( pos( '.\', RelativeURL ) = 1 ) do
        delete( RelativeURL, 1, 2 );

      tmps := FPath;
      p := pos( '..\', RelativeURL );
      if ( p = 1 ) then
      begin
        delete( tmps, length( tmps ), 1 ); // remove trailing slash
        repeat
          delete( RelativeURL, 1, 3 );
          p := lastpos( '\', tmps );
          if ( p > 0 ) then
            delete( tmps, p, length( tmps ));
        until ( pos( '..\', RelativeURL ) <> 1 );
        RelativeURL := '\' + RelativeURL;
      end;
    end;

  end;

  case FURLScheme of

    urlHTTP, urlHTTPS : begin
      result := Format(
        '%s%s%s%s%s%s',
        [_URL_SCHEMES[FURLScheme], GetLoginStr, FHost, GetPortStr( true ), tmps, RelativeURL]
      );
    end;

    urlFTP : begin
      result := Format(
        '%s%s%s%s%s%s',
        [_URL_SCHEMES[FURLScheme], GetLoginStr, FHost, GetPortStr( true ), tmps, RelativeURL]
      );
    end;

    urlFILE : begin
      result := Format(
        '%s%s%s%s',
        [_URL_SCHEMES[urlFILE], FHost, tmps, RelativeURL]
      );
    end;

  end;
end; // ExpandRelativeURL

procedure TURL.SetFilename(const Value: string);
begin
  FFilename := Value;
  Assemble( false );
end; // SetFilename

procedure TURL.SetHost(const Value: string);
begin
  FHost := Value;
  Assemble( false );
end; // SetHost

procedure TURL.DoError(const ErrorMsg: string);
begin
  FValid := false;
  if assigned( FOnURLError ) then
    FOnURLError( self, ErrorMsg )
  else
  begin
    if FThrowExceptions then
      raise EURLError.Create( ErrorMsg );
  end;
end; // DoError

procedure TURL.ClearAllFields;
begin
  FURLScheme := urlUNKNOWN;
  FScheme := '';
  FHost := '';
  FPort := '';
  FPath := '';
  FFilename := '';
  FQuery := '';
  FFragment := '';
  FUsername := '';
  FPassword := '';
  FParams := '';
  FValid := false;
end; // ClearAllFields

function TURL.Canonical: string;
begin
  result := Format(
    '<URL:%s>',
    [Assemble( true )]
  );
  // [x] note: we do not currently PARSE such form
end; // Canonical

procedure TURL.SetParams(const Value: string);
begin
  FParams := Value;
end; // SetParams

function TURL.GetLoginStr : string;
begin
  if ( FUsername <> '' ) then
  begin
    if ( FPassword <> '' ) then
      result := Format(
        '%s:%s@',
        [FUsername, FPassword]
      )
    else
    result := Format(
      '%s@',
      [FUsername]
    );
  end
  else
  begin
    result := '';
  end;
end; // GetLoginStr

function TURL.GetFragmentStr( const Abbreviated : boolean ) : string;
begin
  if ( Abbreviated or ( FFragment = '' )) then
    result := ''
  else
    result := '#' + FFragment;
end; // GetFragmentStr

function TURL.GetParamStr: string;
begin
  if ( FParams = '' ) then
    result := ''
  else
    result := ';' + FParams;
end; // GetParamStr

function TURL.GetPortStr( const Abbreviated : boolean ): string;
var
  portint : integer;
begin
  try
    if ( FURLScheme <> urlUNKNOWN ) then
      portint := strtoint( FPort )
    else
      portint := 0;
  except
    portint := _URL_SCHEME_PORTS[FURLScheme];
  end;
  if (( portint = 0 ) or ( Abbreviated and ( portint = _URL_SCHEME_PORTS[FURLScheme] ))) then
  begin
    result := '';
  end
  else
  begin
    result := Format(
      ':%s',
      [FPort]
    );
  end;
end; // GetPortStr

function TURL.GetQueryStr: string;
begin
  if ( FQuery = '' ) then
    result := ''
  else
    result := '?' + FQuery;
end; // GetQueryStr

function TURL.GetValid: boolean;
begin
  result := FValid;
end;



end.
