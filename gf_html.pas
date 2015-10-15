unit gf_html;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 
 Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
 in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 

{$I gf_base.inc}

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

