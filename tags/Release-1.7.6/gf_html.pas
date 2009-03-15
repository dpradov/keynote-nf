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

 The Original Code is "gf_HTML.pas".

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

unit gf_html;

interface


function TextToHTML( s : string ) : string;
function StripHTML( s : string; var InTag : boolean ) : string;

implementation


function TextToHTML(  s : string ) : string;
// MUST be text WITHOUT any HTML codes!
var
   i, slen : integer;
begin
   result := '';
   slen := length( s );
   if ( slen = 0 ) then exit;

   for i := slen downto 1 do
   begin
    case s[i] of
      '>' : begin
        result := '&gt;' + result;
      end;
      '<' : begin
        result := '&lt;' + result;
      end;
      '&' : begin
        result := '&amp;' + result;
      end;
      else
      begin
        result := s[i] + result;
      end;
    end;
   end;

end; // SanitizeForHTML

function StripHTML( s : string; var InTag : boolean ) : string;
// SIMPLISTIC, brain-dead. Do not use!
var
   posbeg, posend : integer;
   temps : string;
begin
   temps := '';
   if ( length( s ) < 3 ) then
   begin
       result := s;
       exit;
   end;

   if InTag then
   begin
       posend := pos( '>', s );
       if ( posend = 0 ) then
       begin
           result := s;
           exit;
       end;
       delete( s, 1, posend );
       InTag := false;
   end;

   posbeg := pos( '<', s );
   while ( posbeg > 0 ) do
   begin
       temps := temps + copy( s, 1, posbeg-1 );
       delete( s, 1, posbeg );
       InTag := true;
       posend := pos( '>', s );
       if ( posend = 0 ) then
       begin
         break;
       end
       else
       begin
           delete( s, 1, posend );
           InTag := false;
       end;
       posbeg := pos( '<', s );
   end;

   if InTag then
       result := temps
   else
       result := temps + s;

end; // StripHTML


end.

