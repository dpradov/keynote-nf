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

 The Original Code is "gf_HttpCookie.pas".

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
 * Importing Opera cookies (file format specification?)
 * Converting date from RFC-1123 format ('Tue, 01 Jan 2036 08:00:38 GMT')
   to TDateTime
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

unit gf_HttpCookie;
{$I gf_base.inc}

interface
uses Windows, SysUtils, Classes, contnrs,
  FileCtrl, gf_misc, gf_files, gf_strings, gf_URLParser;

const
  _MAX_EXPIRY_DATE = 'Tue, 01 Jan 2036 08:00:38 GMT';

type
  ECookieError = class( Exception );

  TCookieErrorEvent = procedure(
    sender : TObject; const ErrorMsg : string
  ) of object;

  THttpCookie = class( TObject )
  private
    FName : string;
    FValue : string;
    FExpires : string;
    FDomain : string;
    FPath : string;
    FSecure : boolean;

    // events
    FOnError : TCookieErrorEvent;

    procedure ClearAll;
    function GetText: string;
    procedure SetDomain(const Value: string);
    procedure SetExpires(const Value: string);
    procedure SetName(const Value: string);
    procedure SetPath(const Value: string);
    procedure SetValue(const Value: string);
    function GetCookie: string;

  public
    property Text : string read GetText;
    property Cookie : string read GetCookie;
    property Name : string read FName write SetName;
    property Value : string read FValue write SetValue;
    property Expires : string read FExpires write SetExpires;
    property Domain : string read FDomain write SetDomain;
    property Path : string read FPath write SetPath;
    property Secure : boolean read FSecure write FSecure;

    // events
    property OnError : TCookieErrorEvent read FOnError write FOnError;

    constructor Create;
    destructor Destroy; override;

    procedure SetText(const Value: string); overload;
    procedure SetText( const Value : string; const URL : TURL ); overload;

    function Expired : boolean; // [x] not implemented
    procedure SetExpirationDate( const DT : TDateTime );

  end;

type
  TCookieList = class( TObjectList )
  private
    function GetItems(Index: Integer): THttpCookie;
    procedure SetItems(Index: Integer; ACookie : THttpCookie );
  public
    function Add(ACookie: THttpCookie): Integer;
    function Remove(ACookie: THttpCookie): Integer;
    function IndexOf(ACookie: THttpCookie): Integer;
    procedure Insert(Index: Integer; ACookie: THttpCookie);
    procedure Sort;
    property Items[Index: Integer]: THttpCookie read GetItems write SetItems; default;
  end;

type
  TCookieCollection = class( TObject )
  private
    FCookies : TCookieList;
    FFilename : string; // file where cookie collection is stored
    FReplaceExisting : boolean; // replace existing cookie (if name, domain and path are the same)
    FValidDomainsOnly : boolean; // discard cookies with invallid domain segments

    function GetCount: integer;

  public
    property Cookies : TCookieList read FCookies write FCookies;
    property Filename : string read FFilename write FFilename;
    property ReplaceExisting : boolean read FReplaceExisting write FReplaceExisting;
    property ValidDomainsOnly : boolean read FValidDomainsOnly write FValidDomainsOnly;
    property Count : integer read GetCount;

    constructor Create;
    destructor Destroy; override;

    function AddCookie( const s : string; const URL : TURL ) : boolean;
    function IndexOfCookie( const aCookie : THttpCookie ) : integer;
    function GetCookie( const name, domain, path : string ) : THttpCookie;
    procedure DeleteExpired;

    function Save( FN : string ) : integer;
    function Load( FN : string ) : integer;

    function ImportNetscape( const FN : string ) : integer;
    function ImportMSIE( Folder : string ) : integer;
    function ImportOpera( const FN : string ) : integer;

    function ServeCookie( const domain, path : string ) : string; overload;
    function ServeCookie( const aURL : TURL ) : string; overload;

    procedure Sort;

  end;


function IsValidCookie( const CookieDomain : string ) : boolean;

implementation

function IsValidCookie( const CookieDomain : string ) : boolean;
var
  p, dots : integer;
  topdomain : string;
begin
    p := lastpos( '.', CookieDomain );
    topdomain := copy( CookieDomain, p, length( CookieDomain ));
    if ( p > 0 ) then
    begin
      dots := CountChars( '.', CookieDomain );
      if ( length( topdomain ) < 4 ) then
      begin
        // two-letter top domain: must have at least 3 dots in domain
        result := ( dots > 2 );
      end
      else
      begin
        // three-letter top domain: must have at least 2 dots in domain
        result := ( dots > 1 );
      end;
    end
    else
    begin
      result := false;
    end;
end; // IsValidCookie

function CookieListCompare( Item1, Item2 : pointer ) : integer;
// sort cookies in the order in which they should be sent to
// the server, ie longest (most specific) path first
var
  Path1, Path2 : integer;
begin
  Path1 := length( THttpCookie( Item1 ).Path );
  Path2 := length( THttpCookie( Item2 ).Path );
  if ( Path1 > Path2 ) then
    result := -1
  else
  if ( Path1 < Path2 ) then
    result := 1
  else
    result := 0;
end; // CookieListCompare


{ THttpCookie }

procedure THttpCookie.ClearAll;
begin
  FName := '';
  FValue := '';
  FExpires := _MAX_EXPIRY_DATE;
  FDomain := '';
  FPath := '/';
  FSecure := false;
end; // ClearAll

constructor THttpCookie.Create;
begin
  inherited Create;
  ClearAll;
end; // CREATE

destructor THttpCookie.Destroy;
begin
  inherited Destroy;
end; // DESTROY

function THttpCookie.Expired: boolean;
begin
  result := false; // [x]
end;

function THttpCookie.GetCookie: string;
begin
  // returns "name=value" part, as it should be sent to the server
  result := Format(
    '%s=%s',
    [FName, FValue]
  );
end; // GetCookie

function THttpCookie.GetText: string;
begin
  // returns a string of all cookie fields
  // (e.g. for storage or conversion, NOT for sending to the server)
  result := Format(
    '%s=%s; domain=%s; path=%s',
    [FName, FValue, FDomain, FPath]
  );
  if ( FExpires <> '' ) then
    result := result + Format(
      '; expires=%s',
      [FExpires]
    );
  if FSecure then
    result := result + '; secure';
end; // GetText

procedure THttpCookie.SetDomain(const Value: string);
begin
  FDomain := lowercase( Value ); // domain names are not case-sensitive, so we smash case for later comparisons
  if ( FDomain = '' ) then
    raise ECookieError.Create( 'Cookie domain not set.' );
end; // SetDomain

procedure THttpCookie.SetExpirationDate(const DT: TDateTime);
begin
  try
    // [x] convert TDateTime to GMT date string
    FExpires := RFC1123_Date( DT ) + ' GMT';
  except
    FExpires := _MAX_EXPIRY_DATE;
  end;
end; // SetExpirationDate

procedure THttpCookie.SetExpires(const Value: string);
begin
  FExpires := Value;
  {
  if ( FExpires = '' ) then
    FExpires := _MAX_EXPIRY_DATE;
  }
end; // SetExpires

procedure THttpCookie.SetName(const Value: string);
begin
  FName := Value;
  if ( FName = '' ) then
    raise ECookieError.Create( 'Cookie name not set.' );
end; // SetName

procedure THttpCookie.SetPath(const Value: string);
begin
  FPath := Value;
  if ( pos( '/', FPath ) <> 1 ) then
    FPath := '/' + Fpath;
end; // SetPAth

procedure THttpCookie.SetText(const Value: string);
var
  ctext, vs, ns : string;
  i, pEq : integer;
  list : TStringList;
begin
  // simple parsing
  ClearAll;

  ctext := trim( Value );
  if pos( 'set-cookie: ', lowercase( ctext ) ) = 1 then
    delete( ctext, 1, 12 );

  list := TStringList.Create;
  try
    CSVTextToStrs( list, ctext, ';' );
    for i := 1 to list.count do
    begin
      ctext := list[pred( i )];
      pEq := pos( '=', ctext );
      if ( pEq > 0 ) then
      begin
        ns := trim( lowercase( copy( ctext, 1, pred( pEq ))));
        delete( ctext, 1, pEq );
        vs := trim( ctext );

        if ( ns = 'expires' ) then
          SetExpires( vs )
        else
        if ( ns = 'path' ) then
          SetPath( vs )
        else
        if ( ns = 'domain' ) then
          SetDomain( vs )
        else
        begin
          SetName( ns );
          SetValue( vs );
        end;
      end
      else
      begin
        if ( lowercase( trim( ctext )) = 'secure' ) then
          FSecure := true;
      end;
    end;
  finally
    list.Free;
  end;

end; // SetText {1}

procedure THttpCookie.SetText(const Value: string; const URL: TURL);
begin
  // this version is preferred, because the cookie itself, as set
  // by server, will often not contain full domain or path information.
  // Instead, the browser is supposed to supply these fields from
  // current URL (location). So, in order to always set cookie properly,
  // we need both cookie and URL.
  try
    SetText( Value );
  except
    on E : ECookieError do
    begin
      if assigned( URL ) then
      begin
        if ( FPath = '' ) then
          FPath := URL.Path;
        if ( FDomain = '' ) then
          FDomain := URL.Host;
      end
      else
        raise;
    end
    else
      raise;
  end;
end; // SetText {2}

procedure THttpCookie.SetValue(const Value: string);
begin
  FValue := Value;
end;

{ TCookieList }

function TCookieList.Add(ACookie: THttpCookie): Integer;
begin
  Result := inherited Add(ACookie);
end;

function TCookieList.GetItems(Index: Integer): THttpCookie;
begin
  Result := THttpCookie(inherited Items[Index]);
end;

function TCookieList.IndexOf(ACookie: THttpCookie): Integer;
begin
  Result := inherited IndexOf(ACookie);
end;

procedure TCookieList.Insert(Index: Integer; ACookie: THttpCookie);
begin
  inherited Insert(Index, ACookie);
end;

function TCookieList.Remove(ACookie: THttpCookie): Integer;
begin
  Result := inherited Remove(ACookie);
end;

procedure TCookieList.SetItems(Index: Integer; ACookie: THttpCookie);
begin
  inherited Items[Index] := ACookie;
end;

procedure TCookieList.Sort;
begin
  inherited Sort( CookieListCompare );
end; // Sort

{ TCookieCollection }

function TCookieCollection.AddCookie(const s: string; const URL : TURL): boolean;
var
  aCookie, oldCookie : THttpCookie;
begin
  // returns TRUE if cookie was successfully added to collection
  result := false;
  if ( s = '' ) then exit;

  aCookie := THttpCookie.Create;
  try
    aCookie.SetText( s, URL );
  except
    aCookie.Free;
    exit;
  end;

  if FValidDomainsOnly then
  begin
    if ( not IsValidCookie( aCookie.Domain )) then
    begin
      aCookie.Free;
      exit;
    end;
  end;

  oldCookie := GetCookie( aCookie.Name, aCookie.Domain, aCookie.Path );
  if ( oldCookie <> nil ) then
  begin
    if FReplaceExisting then
      FCookies.Remove( oldCookie )
    else
    begin
      aCookie.Free;
      exit;
    end;
  end;

  FCookies.Add( aCookie );
  result := true;

end; // AddCookie

constructor TCookieCollection.Create;
begin
  inherited Create;
  FCookies := TCookieList.Create;
  FFilename := '';
  FReplaceExisting := true;
  FValidDomainsOnly := true;
end; // CREATE

procedure TCookieCollection.DeleteExpired;
var
  i : integer;
begin
  for i := FCookies.Count downto 1 do
  begin
    if FCookies[pred( i )].Expired then
      FCookies.Delete( pred( i ));
  end;
end; // DeleteExpired

destructor TCookieCollection.Destroy;
begin
  FCookies.Free;
  inherited Destroy;
end; // DESTROY


function TCookieCollection.GetCookie(const name, domain,
  path: string): THttpCookie;
var
  i : integer;
  aCookie : THttpCookie;
begin
  result := nil;
  for i := 1 to FCookies.Count do
  begin
    aCookie := FCookies[pred( i )];
    if (( aCookie.Name = name ) and
        ( aCookie.Domain = domain ) and
        ( aCookie.Path = path )) then
    begin
      result := aCookie;
      break;
    end;
  end;
end; // GetCookie

function TCookieCollection.GetCount: integer;
begin
  result := FCookies.Count;
end;

function TCookieCollection.ImportMSIE( Folder: string): integer;
var
  list, files : TStringList;
  p, cnt, i, line : integer;
  Cookie : THttpCookie;
  s, fn : string;
  dt : TDateTime;
  // Y, M, D, Hr, Min, Sec, MSec : word;
begin
  result := -1;
  if ( not DirectoryExists( Folder )) then exit;
  if ( folder[length( folder )] <> '\' ) then
    folder := folder + '\';

  files := TStringList.Create;
  list := TStringList.Create;

  try
    try

      GetFilesInFolder( Folder, '*.txt', false, false, files );
      if ( files.Count = 0 ) then exit;
      result := 0;

      for i := 1 to files.Count do
      begin

        fn := Folder + files[pred( i )];

        cookie := THttpCookie.Create;
        try
          try
            list.clear;
            list.LoadFromFile( fn );
            cnt := 1;
            dt := 0;
            for line := 1 to list.Count do
            begin
              s := list[pred( line )];
              if ( s = '' ) then continue;
              if ( s = '*' ) then
              begin
                // add cookie
                cnt := 1;
                // cookie.SetExpirationDate( dt );
                cookie.Expires := _MAX_EXPIRY_DATE;
                if AddCookie( cookie.Text, nil ) then
                  inc( result );
                continue;
              end;
              case cnt of
              (* 1 name
                 2 value
                 3 domain+path
                 4 secure (0 or 1)
                 5 expiration date - UNKNOWN LARGE INTEGER FORMAT
                 6 expiration time - UNKNOWN LARGE INTEGER FORMAT
                 last_used date
                 last_used time
                 *                            *)
                1 : begin
                  cookie.Name := s;
                end;
                2 : begin
                  cookie.Value := s;
                end;
                3 : begin
                  p := pos( '/', s );
                  if ( p > 0 ) then
                  begin
                    cookie.Domain := copy( s, 1, pred( p ));
                    delete( s, 1, p );
                    cookie.Path := s;
                  end
                  else
                  begin
                    cookie.Domain := s;
                    cookie.Path := '/';
                  end;
                end;
                4 : begin
                  cookie.Secure := ( s = '1' );
                end;
                5 : begin
                  try
                    // dt := strtofloat( s );
                  except
                  end;
                end;
                6 : begin
                  try
                    // dt := dt + strtofloat( s );
                  except
                  end;
                end;

              end;

              inc( cnt );
            end;
          except
            continue;
          end;
        finally
          cookie.Free;
        end;
      end;
    except
    end;
  finally
    files.Free;
    list.Free;
  end;

end; // ImportMSIE

function TCookieCollection.ImportNetscape(const FN: string): integer;
var
  list, TabList : TStringList;
  i : integer;
  s : string;
  cookie : THttpCookie;
begin
  result := -1;

(* FORMAT:
  .domain.com	TRUE	/	FALSE	2082787238	cookiename	cookievalue
  www.domain.pl	FALSE	/	FALSE	1102311970	cookiename	cookievalue
*)

  list := TStringList.Create;
  TabList := TStringList.Create;

  try
    try
      list.LoadFromFile( FN );
      if ( list.Count > 0 ) then
      begin
        result := 0;
        for i := 1 to list.count do
        begin
          s := list[pred( i )];
          if (( s <> '' ) and ( s[1] <> '#' )) then
          begin
            TabList.Clear;
            CSVTextToStrs( TabList, s, #9 );
            if ( TabList.Count < 7 ) then continue;
            cookie := THttpCookie.Create;
            try
              try
                cookie.Domain := TabList[0];
                cookie.Path := TabList[2];
                cookie.Secure := ( lowercase( Tablist[3] ) = 'true' );
                cookie.Name := TabList[5];
                cookie.Value := TabList[6];
                cookie.SetExpirationDate( UnixTimeToDateTime( strtoint( Tablist[4] )));

                if AddCookie( cookie.Text, nil ) then
                  inc( result );

              except
                continue;
              end;
            finally
              cookie.Free;
            end;
          end;
        end;
      end;
    except
      exit;
    end;
  finally
    list.Free;
    TabList.Free;
  end;

end; // ImportNetscape

function TCookieCollection.ImportOpera(const FN: string): integer;
begin
  result := -1;
  // [x] Opera uses a binary format file to store cookies.
  // I've no idea how to read it. Please help if you can.
end;

function TCookieCollection.IndexOfCookie(
  const aCookie: THttpCookie): integer;
begin
  result := FCookies.IndexOf( aCookie );
end;

function TCookieCollection.Load(FN: string): integer;
var
  f : textfile;
  aCookie : THttpCookie;
  s : string;
begin
  FCookies.Clear;
  result := -1;
  if ( FN = '' ) then
    FN := FFilename;
  if ( FFilename = '' ) then
    FFilename := FN;

  if ( FN = '' ) then exit;

  result := 1;

  assignfile( f, FN );
  reset( f );

  try
    while ( not eof( f )) do
    begin
      readln( f, s );
      if (( s = '' ) or ( s[1] = ';' )) then continue;
      aCookie := THttpCookie.Create;
      try
        aCookie.SetText( s );
        FCookies.Add( aCookie );
      except
        aCookie.Free;
      end;
    end;
    result := 0;
    // FCookies.Sort;
  finally
    closefile( f );
  end;

end; // LOAD

function TCookieCollection.Save(FN: string): integer;
var
  f : textfile;
  i : integer;
begin
  // we should probably not save cookies whose expiry data
  // is blank, because such cookies are session-only, and
  // are not meant to be stored

  result := -1;
  if ( FN = '' ) then
    FN := FFilename;
  if ( FFilename = '' ) then
    FFilename := FN;

  if ( FN = '' ) then exit;

  result := 1;

  assignfile( f, FN );
  rewrite( f );

  try
    for i := 1 to FCookies.Count do
      writeln( f, FCookies[pred( i )].Text );
    result := 0;
  finally
    closefile( f );
  end;

end; // SAVE

function TCookieCollection.ServeCookie(const domain, path: string): string;
var
  i : integer;
  aCookie : THttpCookie;
  list : TCookieList;
begin
  // given a domain and path, return a string containing all
  // matching cookies, as they should be sent to the server.
  // Cookies are arranged in proper order.
  result := '';
  if ( FCookies.Count = 0 ) then exit;
  list := TCookieList.Create( false );
  try
    for i := 1 to FCookies.Count do
    begin
      aCookie := FCookies[pred( i )];
      if TailMatch( domain, aCookie.Domain ) then
      begin
        if ( pos( aCookie.Path, path ) = 1 ) then
          list.Add( aCookie );
      end;
    end;
    list.Sort; // longer paths first
    for i := 1 to list.Count do
    begin
      aCookie := list[pred( i )];
      if ( i = 1 ) then
        result := aCookie.Cookie
      else
        result := result + '; ' + aCookie.Cookie;
    end;
  finally
    list.Free;
  end;

end; // ServeCookie

function TCookieCollection.ServeCookie(const aURL: TURL): string;
begin
  result := ServeCookie( aURL.Host, aURL.Path );
end;

procedure TCookieCollection.Sort;
begin
  FCookies.Sort;
end;

end.


