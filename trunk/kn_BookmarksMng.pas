unit kn_BookmarksMng;

interface
uses
   Menus;

    procedure BookmarkAdd( const Number : integer );
    procedure BookmarkGoTo( const Number : integer );
    procedure BookmarkClear( const Number : integer );
    procedure BookmarkClearAll;
    function BookmarkGetMenuItem( const Number : integer ) : TMenuItem;

implementation
uses SysUtils, Messages,
     TreeNT, gf_strings,
     kn_Global, kn_Main, kn_NoteObj, kn_NodeList, kn_Const, kn_Info;

resourcestring
  STR_Assigned = ' Bookmark %d assigned.';
  STR_NotAssigned = ' Bookmark %d not assigned!';
  STR_CannotAccess = ' Cannot access bookmark %d - Cleared';
  STR_Jumped = ' Jumped to bookmark %d';

procedure BookmarkAdd( const Number : integer );
begin
  if  not (Form_Main.HaveNotes( true, true ) and assigned( ActiveNote )) then exit;
  if (( Number < 0 ) or ( Number > MAX_BOOKMARKS )) then exit;

  with NoteFile.Bookmarks[Number] do
  begin
    Name := WideFormat( '%d - %s', [Number, RemoveAccelChar( ActiveNote.Name )] );
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
  if assigned( NoteFile ) then
  begin
    for i := 0 to MAX_BOOKMARKS do
      BookmarkClear( i );
  end;
end; // BookmarkClearAll

procedure BookmarkClear( const Number : integer );
begin
  if ( not assigned( NoteFile )) then exit;
  if (( Number < 0 ) or ( Number > MAX_BOOKMARKS )) then exit;
  with NoteFile.Bookmarks[Number] do
  begin
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

    procedure ClearBookmark;
    begin
      BookmarkClear( Number );
      Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_CannotAccess, [Number] );
    end;

begin
  if ( not Form_Main.HaveNotes( true, true ) and assigned( ActiveNote )) then exit;
  if (( Number < 0 ) or ( Number > MAX_BOOKMARKS )) then exit;

  myTreeNode := nil;

  if ( NoteFile.Bookmarks[Number].Note <> nil ) then
  begin
    try
      if ( NoteFile.Bookmarks[Number].Note <> ActiveNote ) then
      begin
        Form_Main.Pages.ActivePage := NoteFile.Bookmarks[Number].Note.TabSheet;
        Form_Main.PagesChange( Form_Main.Pages );
      end;
      if ( NoteFile.Bookmarks[Number].Node <> nil ) then
      begin
        if ( ActiveNote.Kind = ntTree ) then
        begin
          myTreeNode := TTreeNote( ActiveNote ).TV.Items.FindNode( [ffData], '', NoteFile.Bookmarks[Number].Node );
        end;
        if ( not assigned( myTreeNode )) then
        begin
          ClearBookmark;
          exit;
        end;
        TTreeNote( ActiveNote ).TV.Selected := myTreeNode;
      end;

      with ActiveNote.Editor do
      begin
        SelStart := NoteFile.Bookmarks[Number].CaretPos;
        SelLength := NoteFile.Bookmarks[Number].SelLength;
        Perform( EM_SCROLLCARET, 0, 0 );
      end;

      Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_Jumped, [Number] );

    except
      on E : Exception do
      begin
        ClearBookmark;
        exit;
      end;
    end;

  end
  else
  begin
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_NotAssigned, [Number] );
  end;

end; // BookmarkGoTo

end.
