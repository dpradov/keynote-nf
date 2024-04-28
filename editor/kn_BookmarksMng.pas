unit kn_BookmarksMng;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2024 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface

uses
  Winapi.Messages,
  System.SysUtils,
  Vcl.Menus,
  gf_streams,
  kn_Const
  ;



    procedure BookmarkAdd( const Number : integer );
    procedure BookmarkGoTo( const Number : integer );
    procedure BookmarkClear( const Number : integer; DeleteBookmark: boolean= true );
    //procedure BookmarkClearAll;
    procedure BookmarkInitializeAll;
    function BookmarkGetMenuItem( const Number : integer ) : TMenuItem;

    procedure SerializeBookmarks(const tf: TTextFile);
    procedure LoadBookmarks(const tf: TTextFile; var FileExhausted: Boolean; var NextBlock: TNextBlock);

implementation
uses
  kn_Info,
  kn_Global,
  kn_Main,
  kn_LocationObj,
  kn_LinksMng,
  knt.App
  ;

resourcestring
  STR_Assigned = ' Bookmark %d assigned.';
  STR_NotAssigned = ' Bookmark %d not assigned!';
  STR_CannotAccess = ' Cannot access bookmark %d - Cleared';
  STR_Jumped = ' Jumped to bookmark %d';



procedure RegisterBookmark( const Number : integer; aLocation : TLocation);
begin
  if aLocation = nil then exit;

  KntFile.Bookmarks[Number]:= aLocation;

  with BookmarkGetMenuItem( Number ) do begin
     Enabled := true;
     Checked := true;
  end;
end;

procedure BookmarkAdd( const Number : integer );
var
  aLocation : TLocation;
begin
  if  not (Form_Main.HaveKntFolders( true, true ) and assigned( ActiveFolder )) then exit;

  BookmarkClear( Number );

  aLocation:= TLocation.Create;
  ActiveFolder.Editor.SelLength:= 0;
  InsertOrMarkKNTLink( aLocation, false, '', Number + 1 );            // Bookmark0-9 -> [1-10]
  RegisterBookmark(Number, aLocation);

  Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_Assigned, [Number] );
end;


procedure BookmarkInitializeAll;
var
  i : integer;
begin
  if assigned( KntFile ) then  begin
    for i := 0 to MAX_BOOKMARKS do
        BookmarkClear(i, false);
  end;

end;


procedure BookmarkClear( const Number : integer; DeleteBookmark: boolean= true );
var
  aLocation : TLocation;
begin
  aLocation:= KntFile.Bookmarks[Number];
  if assigned(aLocation) then begin
     if DeleteBookmark then
        DeleteBookmark09(aLocation);
     KntFile.Bookmarks[Number]:= nil;
     aLocation.Free;
     with BookmarkGetMenuItem( Number ) do begin
        Enabled := false;
        Checked := false;
     end;

  end;
end;



procedure BookmarkGoTo( const Number : integer );
var
  aLocation: TLocation;

begin
  aLocation:= KntFile.Bookmarks[Number];
  if assigned(aLocation) then begin
     if JumpToLocation(aLocation) then
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_Jumped, [Number] )
     else begin
        BookmarkClear (Number, false);
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_CannotAccess, [Number] );
     end;
  end
  else
      Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_NotAssigned, [Number] );
end;

procedure SerializeBookmarks(const tf: TTextFile);
var
   i: integer;
   aLocation: TLocation;
   Str: AnsiString;
begin
    tf.WriteLine(_NF_Bookmarks);

    for i := 0 to MAX_BOOKMARKS do begin
        aLocation:= KntFile.Bookmarks[i];
        if assigned(aLocation) then begin
           Str:= BuildKNTLocationText(aLocation);
           tf.WriteLine(Format(_NF_Bookmark + '=%d,%s', [i, Str]), false);
        end;
    end;
end;

procedure LoadBookmarks(const tf: TTextFile; var FileExhausted: Boolean; var NextBlock: TNextBlock);
var
  s, key : AnsiString;
  p, N: Integer;
  aLocation: TLocation;

begin

    while ( not tf.eof()) do begin
       s:= tf.readln();

       if ( s = _NF_StoragesDEF ) then begin
         NextBlock:= nbImages;         // Images definition begins
         break;
       end;
       if ( s = _NF_EOF ) then begin
         FileExhausted := true;
         break; // END OF FILE
       end;

       p := pos('=', s );
       if p <> 3 then continue;  // not a valid key=value format
       key := copy(s, 1, 2);
       delete(s, 1, 3);

       if key = _NF_Bookmark then begin
         // BK=9,file:///*8|2|4|0|1     or   BK=9,file:///*8|2|4|0  or
         // BK=9,file:///<23|4|0|1      or   BK=9,file:///<23|4|0
          try
             N:= StrToInt(s[1]);
             delete(s, 1, 2);
             aLocation:= BuildBookmark09FromString(s);
             RegisterBookmark(N, aLocation);
          except
          end;
         continue;
       end;

    end;
end;


function BookmarkGetMenuItem( const Number : integer ) : TMenuItem;
begin
  with Form_Main do
  begin
      case Number of
        0 : result := MMBkmJ0;
        1 : result := MMBkmJ1;
        2 : result := MMBkmJ2;
        3 : result := MMBkmJ3;
        4 : result := MMBkmJ4;
        5 : result := MMBkmJ5;
        6 : result := MMBkmJ6;
        7 : result := MMBkmJ7;
        8 : result := MMBkmJ8;
        9 : result := MMBkmJ9;
        else
          result := nil;
      end;
  end;
end; // BookmarkGetMenuItem


end.
