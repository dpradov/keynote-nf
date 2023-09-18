unit kn_BookmarksMng;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface

uses
  Winapi.Messages,
  System.SysUtils,
  Vcl.Menus,
  TreeNT,
  gf_strings,
  kn_Const,
  kn_Info,
  kn_NoteObj,
  kn_NodeList,
  kn_FileObj,
  kn_LocationObj,
  kn_LinksMng;



    procedure BookmarkAdd( const Number : integer );
    procedure BookmarkGoTo( const Number : integer );
    procedure BookmarkClear( const Number : integer );
    procedure BookmarkClearAll;
    function BookmarkGetMenuItem( const Number : integer ) : TMenuItem;

implementation
uses
  kn_Global,
  kn_Main;

resourcestring
  STR_Assigned = ' Bookmark %d assigned.';
  STR_NotAssigned = ' Bookmark %d not assigned!';
  STR_CannotAccess = ' Cannot access bookmark %d - Cleared';
  STR_Jumped = ' Jumped to bookmark %d';

procedure BookmarkAdd( const Number : integer );
begin
  if  not (Form_Main.HaveNotes( true, true ) and assigned( ActiveNote )) then exit;
  if (( Number < 0 ) or ( Number > MAX_BOOKMARKS )) then exit;

  with NoteFile.Bookmarks[Number]^ do
  begin
    Name := Format( '%d - %s', [Number, RemoveAccelChar( ActiveNote.Name )] );
    CaretPos := ActiveNote.Editor.SelStart;
    SelLength := ActiveNote.Editor.SelLength;
    Note := ActiveNote;
    if ( ActiveNote.Kind = ntTree ) then
      Node := TNoteNode( TTreeNote( ActiveNote ).TV.Selected.Data )
    else
      Node := nil;
  end;
  with BookmarkGetMenuItem( Number ) do
    Enabled := true;
  Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_Assigned, [Number] );

end; // BookmarkAdd

procedure BookmarkClearAll;
var
  i : integer;
begin
  if assigned( NoteFile ) then  begin
    for i := 0 to MAX_BOOKMARKS do
       BookmarkClear( i );
  end;
end; // BookmarkClearAll

procedure BookmarkClear( const Number : integer );
begin
  if ( not assigned( NoteFile )) then exit;
  if (( Number < 0 ) or ( Number > MAX_BOOKMARKS )) then exit;

  with NoteFile.Bookmarks[Number]^ do begin
    Name := '';
    CaretPos := 0;
    SelLength := 0;
    Note := nil;
    Node := nil;
  end;
  with BookmarkGetMenuItem( Number ) do
    Enabled := false;

end; // BookmarkClear

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

procedure BookmarkGoTo( const Number : integer );
var
  myTreeNode : TTreeNTNode;
  LocBeforeGoTo: TLocation;
  Bookmark: PBookmark;

    procedure ClearBookmark;
    begin
      BookmarkClear( Number );
      Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_CannotAccess, [Number] );
    end;

begin
  if ( not Form_Main.HaveNotes( true, true ) and assigned( ActiveNote )) then exit;
  if (( Number < 0 ) or ( Number > MAX_BOOKMARKS )) then exit;

  myTreeNode := nil;

  Bookmark:= NoteFile.Bookmarks[Number];

  if ( Bookmark.Note <> nil ) then begin
    LocBeforeGoTo:= nil;
    GetKntLocation (LocBeforeGoTo, true);

    try
      if ( Bookmark.Note <> ActiveNote ) then begin
        Form_Main.Pages.ActivePage := Bookmark.Note.TabSheet;
        Form_Main.PagesChange( Form_Main.Pages );
      end;

      if ( Bookmark.Node <> nil ) then begin
        if ( ActiveNote.Kind = ntTree ) then
          myTreeNode := TTreeNote( ActiveNote ).TV.Items.FindNode( [ffData], '', Bookmark.Node );
        if ( not assigned( myTreeNode )) then begin
          ClearBookmark;
          exit;
        end;
        TTreeNote( ActiveNote ).TV.Selected := myTreeNode;
      end;

      with ActiveNote.Editor do begin
        SelStart := Bookmark.CaretPos;
        SelLength := Bookmark.SelLength;
        Perform( EM_SCROLLCARET, 0, 0 );
      end;

      Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_Jumped, [Number] );

      if (LocBeforeGoTo.NoteID = ActiveNote.ID)
          and ((ActiveNote.Kind = ntRTF) or (TTreeNote(ActiveNote).SelectedNode.ID = LocBeforeGoTo.NodeID))
          and (LocBeforeGoTo.CaretPos <> Bookmark.CaretPos) then begin

          AddHistoryLocation (ActiveNote, false, LocBeforeGoTo);
          _LastMoveWasHistory:= false;
          UpdateHistoryCommands;
      end;


    except
      on E : Exception do begin
        ClearBookmark;
        exit;
      end;
    end;

  end
  else
     Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_NotAssigned, [Number] );

end; // BookmarkGoTo

end.
