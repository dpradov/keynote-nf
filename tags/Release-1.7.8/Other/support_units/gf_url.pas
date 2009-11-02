unit gf_url;

(* ************************************************************
 KEYNOTE: MOZILLA PUBLIC LICENSE STATEMENT.
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is KeyNote 1.0.

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 Released: 30 June 2001
 -----------------------------------------------------------
 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <marekjed@users.sourceforge.net>

************************************************************ *)

(*
NOTES:

* The TURLRec cration (SplitURL) may be buggy.
  The record does not contain a PORT field.
  It might also be wise to supply username and password fields
  (for FTP and HTTP)

* ExtractHTMLTitle only works if there's a properly formatted <HEAD></HEAD> tag
  (in fact, only <HEAD> is necessary). Some HTML docs do not contain this, e.g.
  only have <HTML> or not even that. Might make it an option to search for the
  <TITLE>..</TITLE> sequence throughtout the file, not only in <HEAD>.

*)

interface

const
   TOKEN_LEN = 16;
   URL_SCHEMES_CNT = 10;

const
   URL_SCHEMES : array[1..URL_SCHEMES_CNT] of string = (
       'file://',
       'ftp://',
       'gopher://',
       'http://',
       'https://',
       'mailto:',
       'news:',
       'nntp://',
       // 'prospero://',
       'telnet://',
       'wais://'
   );

type

   string255 = string[255];

   TAnchorRec = record
       URL  : string;
       Desc : string;
   end;


   TAnchorRec255 = record
       URL  : string255;
       Desc : string255;
   end;

type
   TURLScheme = ( schemeNIL, schemeFILE, schemeFTP,
                  schemeGOPHER, schemeHTTP, schemeHTTPS,
                  schemeMAILTO, schemeNEWS, schemeNNTP,
                  {schemePROSPERO,}
                  schemeTELNET, schemeWAIS );

type
   TURLRec = record
       Scheme     : TURLScheme;
       SchemeName : string; // name of scheme
       HostName   : string; // host name
       PathName   : string; // path section
       FileName   : string; // remote file name
       SpotName   : string; // the #name string (/doc.html#spot)
       CGIstring  : string; // the CGI data chunk (following '?' in a URL)
       LocalName  : string; // local file name
   end;

   TURLRec255 = record
       Scheme     : TURLScheme;
       SchemeName : string[TOKEN_LEN];
       HostName   : string255;
       PathName   : string255;
       FileName   : string255;
       SpotName   : string255;
       CGIstring  : string255;
       LocalName  : string255;
   end;

function SplitAnchor( a : string ) : TAnchorRec;
// Splits <A HREF="url">desc text</A> construct into URL and description

function CombineURL( baseurl, parturl : string ) : string;
// Takes a BASE HREF url and a relative URL, returns a fully qualified URL

function FindScheme( const s : string ) : TURLScheme;
// Returns the type of url scheme. String 's' must begin
// with a valid url scheme token (such as 'ftp://').
// Otherwise, returns schemeNIL (none, or unknown)

function URLSchemeToStr( const s : TURLScheme ) : string;
// For a TURLScheme returns the actual scheme string, e.g. 'ftp://'

procedure SplitURL( const u : string; var r : TURLRec );
// splits an URL into component parts, leaving LocalName empty

procedure URL2LocalFileName( var r : TURLRec );
// determins LocalName for a TURLRec

function ExtractHTMLTitle( const fn : string; const maxscanlines : integer ) : string;
// extracts contents of <title>..</title> tag from HTML files

implementation

uses SysUtils, streamfile, gf_files;

function SplitAnchor( a : string ) : TAnchorRec;
var
   p : integer;
   tmprec : TAnchorRec;
begin
   with tmprec do
   begin
       url := '';
       desc := '';
   end;
   result := tmprec;
   if ( a = '' ) then
       exit;

   p := pos( '<a href="', lowercase( a ));
   if ( p = 0 ) then
       exit;
   delete( a, 1, p+8 );
   p := pos( '"', a );
   tmprec.url := copy( a, 1, p-1 );
   delete( a, 1, p );
   p := pos( '>', a );
   delete( a, 1, p );
   p := pos( '</a', lowercase( a ));
   tmprec.desc := copy( a, 1, p-1 );
   result := tmprec;

end; // SplitAnchor

function CombineURL( baseurl, parturl : string ) : string;
var
   baseR : TURLRec;
   p : integer;
begin
   if ( baseURL = '' ) then
   begin
       result := partURL;
       exit;
   end
   else
   begin
       if ( parturl = '' ) then
       begin
           result := baseurl;
           exit;
       end;
   end;

   // If PartURL is fully formed, return it
   if ( FindScheme( partURL ) <> schemeNIL ) then
   begin
       result := partURL;
       exit;
   end;

   splitURL( baseURL, baseR );

   if ( not ( baseR.Scheme in [schemeFTP, schemeHTTP, schemeHTTPS] )) then
   begin
       result := partURL; // BASE HREF only makes sense for http and ftp schemes
       exit;
   end;

   if ( baseR.PathName[length(baseR.PathName)] <> '/' ) then
       baseR.PathName := baseR.PathName + '/'; // splitURL removes this
   p := length( baseR.PathName );

   if ( partURL[1] = '/' ) then
   begin
       delete( partURL, 1, 1 );
   end
   else
   begin
       if ( pos( './', parturl ) = 1 ) then
       begin
           // delete this bit, it doesn't change the path
           delete( parturl, 1, 2 );
       end;
       while ( pos( '../', parturl ) = 1 ) do
       begin
           delete( parturl, 1, 3 );
           if ( p > 1 ) then
           begin
               dec( p );
               while ( p > 1 ) and ( baseR.PathName[p] <> '/' ) do
                   dec( p );
           end;
       end;
   end;

   result := baseR.SchemeName + baseR.HostName + copy( baseR.PathName, 1, p ) + partURL;

end; // CombineURL


procedure SplitURL( const u : string; var r : TURLRec );
var
   i : integer;
   s : string;
begin

   with r do
   begin
       Scheme     := FindScheme( u );
       SchemeName := URLSchemeToStr( Scheme );
       HostName   := '';
       PathName   := '';
       FileName   := '';
       SpotName   := '';
       LocalName  := '';
   end;
   s := u;

   // remove scheme identifier, if any
   i := pos( '://', s );
   if ( i > 0 ) then
   begin
       delete( s, 1, i+2 );
   end
   else
   begin
       i := pos( ':', s );
       if ( i > 0 ) then
           delete( s, 1, i );
   end;

   // get HOST (everything up to the FIRST slash)
   i := pos( '/', s );
   if ( i > 0 ) then
   begin
       r.HostName := copy( s, 1, i-1 );
       delete( s, 1, i-1 );
   end
   else
   begin
       r.HostName := s; // no slash, e.g. http://www.time.com
       r.PathName := '/';
       exit; // nothing left to parse
   end;

   // We need to extract "spot" markers and CGI data
   // before doing anything else, because these strings
   // can be pesky and contain, for instance, slashes-
   // which then breaks the path analysis. NOTE:
   // we assume that the URL will contain the '#'
   // spot marker XOR the '?' CGI stuff, but not BOTH.
   i := pos( '#', s );
   if ( i > 0 ) then
   begin
       r.SpotName := copy( s, i, length( s ));
       delete( s, i, length( s ));
   end
   else
   begin
       i := pos( '?', s );
       if ( i > 0 ) then
       begin
           r.CGIstring := copy( s, i, length( s ));
           delete( s, i, length( s ));
       end;
   end;


   // get PATH (everything up to the LAST slash )
   for i := length( s ) downto 0 do
   begin
       if ( i = 0 ) then
           break;
       if ( s[i] = '/' ) then
       begin
           break;
       end;
   end;

   case i of
       0 : begin
               r.PathName := '/';
           end;
       1 : begin
               r.PathName := '/';
               delete( s, 1, 1 );
               r.FileName := s;
           end;
       else
           begin
               r.PathName := copy( s, 1, i-1 );
               r.FileName := copy( s, i+1, length( s ));
           end;
   end;
end; // SplitURL;

procedure URL2LocalFileName( var r : TURLRec );
var
   i : integer;
   p : string;
begin
   if ( r.FileName <> '' ) then
   begin
       r.LocalName := MakeValidFilename( r.FileName, 0, true );
   end
   else
   begin
       if ( length( r.PathName ) > 1 ) then
       begin
           // use only the last directory in pathname
           // (like Netscape does)
           p := '';
           for i := length( r.PathName ) downto 1 do
           begin
               if ( r.PathName[i] = '/' ) then
               begin
                   break;
               end
               else
               begin
                   p := r.PathName[i] + p;
               end;
           end;
           if ( p <> '' ) then
               r.LocalName := MakeValidFilename( p, 0, false )
           else
               r.LocalName := MakeValidFilename( r.PathName, 0, false )
       end
       else
       begin
           r.LocalName := MakeValidFilename( r.HostName, 0, false );
       end;
   end;
end; // URL2LocalFileName

function FindScheme( const s : string ) : TURLScheme;
var
   token : string[TOKEN_LEN];
   thescheme : byte;
begin
   result := schemeNIL;
   token := lowercase( copy( s, 1, TOKEN_LEN ));

   for thescheme := 1 to URL_SCHEMES_CNT do
       if ( pos( URL_SCHEMES[thescheme], token ) = 1 ) then
       begin
           result := TURLScheme(thescheme);
           break;
       end;

end; // FindScheme

function URLSchemeToStr( const s : TURLScheme ) : string;
begin
   if ( s = schemeNIL ) then
       result := ''
   else
       result := URL_SCHEMES[ord(s)];
end; // URLSchemeToStr


function ExtractHTMLTitle( const fn : string; const maxscanlines : integer ) : string;
var
   s, lows : string;
   f : textfile;
   p, line : integer;
   intitle : boolean;
   titlestr : string;
   InFileSize, SizeRead, CRLFSize : integer;
begin
   result := '';

   if ( not fileexists( FN )) then exit;

   line := 0;
   intitle := false;
   titlestr := '';

   InFileSize := GetFileSize( FN );
   if ( InFileSize < 1 ) then exit;

   if ( GetTextFileType( FN ) = tfUNIX ) then
   begin
       CRLFSize := 1;
       assignstreamfile( f, fn )
   end
   else
   begin
       CRLFSize := 2;
       assignfile( f, fn );
   end;

   try
     reset( f );
   except
     exit;
   end;

   SizeRead := 0;
   while not eof( f ) do
   begin
       readln( f, s );
       inc( line );
       inc( SizeRead, length( s ) + CRLFSize ); // workaround for StreamFile BUG
       lows := lowercase( s );
       p := pos( '<title>', lows );
       if ( p > 0 ) then
       begin
           intitle := true;
           delete( s, 1, p+6 );
           delete( lows, 1, p+6 );
       end;
       if intitle then
       begin
           titlestr := titlestr + #32 + s; // concatenate
           p := pos( '</title>', lowercase( titlestr ));
           if ( p > 0 ) then
           begin
               delete( titlestr, p, length( titlestr ));
               // intitle := false;
               break;
           end;
       end;
       if (( line = maxscanlines ) or ( SizeRead > ( InFileSize + ( 2 * CRLFSize )))) then
         break;

   end;
   closefile( f );
   titlestr := trim( titlestr );
   {
   while (( titlestr <> '' ) and ( titlestr[1] in [#0..#32] )) do
       delete( titlestr, 1, 1 );
   while (( titlestr <> '' ) and ( titlestr[length(titlestr)] in [#0..#32] )) do
       delete( titlestr, length(titlestr), 1 );
   }
   result := titlestr;
end; // ExtractHTMLTitle


end.


