unit kn_FindReplaceMng;

interface
uses
    TreeNT, RxRichEd,
    kn_LocationObj, kn_Info, kn_Find, kn_Replace;

var
    Text_To_Find : WideString;

    Form_Find : TForm_Find;        // GLOBAL FORM!
    Form_Replace : TForm_Replace;  // GLOBAL FORM!

    Is_Replacing : boolean;     // Find and Replace functions share some procedures; this is how we tell the difference where necessary
    SearchInProgress : boolean; // TRUE while searching or replacing
    UserBreak : boolean;

    SearchNode_Text, SearchNode_TextPrev : wideString;

    _Executing_History_Jump : boolean;
    _LastMoveWasHistory : boolean;
    LastGoTo : string; // last line number for the "Go to line" command

procedure RunFinder;
function RunFindNext : boolean;
procedure RunFindAllEx;
procedure RunReplace;
procedure RunReplaceNext;
function ConfirmReplace : boolean;
procedure FindEventProc( sender : TObject );
procedure ReplaceEventProc( ReplaceAll : boolean );
procedure Form_FindClosed( sender : TObject );
procedure Form_ReplaceClosed( sender : TObject );
procedure FindResultsToEditor( const SelectedOnly : boolean );


var
num: integer;


function SearchTree( var StartTreeNode : TTreeNTNode; SearchStart : integer; const SearchOpts : TRichSearchTypes ) : integer;

implementation
uses Classes, Dialogs, Forms, SysUtils, Controls, Windows,
     RichEdit,
     gf_miscvcl, gf_strings,
     kn_Global, Kn_const, kn_NoteObj,
     kn_NoteMng, kn_Main, kn_NodeList, kn_Cmd, kn_VCLControlsMng,
     kn_TreeNoteMng, kn_MacroMng, kn_LinksMng, kn_NoteFileMng;

resourcestring
  STR_01 = 'Replace this occurrence?';
  STR_02 = 'Pattern not found: "%s"';
  STR_03 = 'Note "%s" does not exist in this file.';
  STR_04 = 'Tree node "%s" does not exist in note "%s".';
  STR_05 = 'Search results are not available.';
  STR_06 = 'Options';
  STR_07 = ' Searching - press ESC to abort.';
  STR_08 = 'An error occurred during search:';
  STR_09 = ' Pattern found at pos ';
  STR_10 = ' Pattern not found.';
  STR_11 = ' Replaced %d occurrence(s)';
  STR_12 = 'Information';

procedure RunFinder;
begin
  if ( not Form_Main.HaveNotes( true, true )) then exit;
  if ( not assigned( ActiveNote )) then exit;
  if ( FileIsBusy or SearchInProgress ) then exit;

  if ( ActiveNote.Editor.SelLength > 0 ) then
  begin
    FindOptions.Pattern := trim( ActiveNote.Editor.SelTextW );
  end
  else
  begin
    if FindOptions.WordAtCursor then
      FindOptions.Pattern := ActiveNote.Editor.GetWordAtCursorNew( true );
  end;

  FindOptions.FindAllMatches := false; // only TRUE when invoked from resource panel

  if ( Form_Find = nil ) then
  begin
    Form_Find := TForm_Find.Create( Form_Main );
    FindOptions.FindNew := true;

    with Form_Find do
    begin
      myNotifyProc := Form_Main.FindNotify;
      myRestrictedOptions := IsRecordingMacro;
      MyFindOptions := FindOptions;
      ShowHint := KeyOptions.ShowTooltips;
      FindEvent := FindEventProc;
      FormCloseEvent := Form_FindClosed;
    end;
  end;

  try
    if assigned( Form_Replace ) then
      Form_Replace.Close;

    Form_Find.Combo_Text.Text := FindOptions.Pattern;
    Form_Find.Show;

  except
    on E : Exception do
    begin
      messagedlg( E.Message, mtError, [mbOK], 0 );
    end;
  end;

end; // RunFinder



procedure RunReplace;
begin
  if ( not Form_Main.HaveNotes( true, true )) then exit;
  if ( not assigned( ActiveNote )) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;
  if ( FileIsBusy or SearchInProgress ) then exit;

  if ( ActiveNote.Editor.SelLength > 0 ) then
  begin
    FindOptions.ReplacePattern := trim( ActiveNote.Editor.SelTextW );
  end
  else
  begin
    if FindOptions.WordAtCursor then
      FindOptions.ReplacePattern := ActiveNote.Editor.GetWordAtCursorNew( true );
  end;

  if ( Form_Replace = nil ) then
  begin
    Form_Replace := TForm_Replace.Create( Form_Main );
    FindOptions.FindNew := true;
    with Form_Replace do
    begin
      myNotifyProc := Form_Main.FindNotify;
      MyFindOptions := FindOptions;
      ShowHint := KeyOptions.ShowTooltips;
      FindEvent := FindEventProc;
      ReplaceEvent := ReplaceEventProc;
      FormCloseEvent := Form_ReplaceClosed;
    end;
  end;

  try
    if assigned( Form_Find ) then
      Form_Find.Close;

    Form_Replace.Combo_Text.Text := FindOptions.Pattern;
    Form_Replace.Show;

  except
    on E : Exception do
    begin
      showmessage( E.Message );
    end;
  end;

end; // RunReplace

function ConfirmReplace : boolean;
begin
  result := true;
  if FindOptions.ReplaceConfirm then
  begin
    result := ( messagedlg( STR_01, mtConfirmation, [mbYes,mbNo], 0 ) = mrYes );
  end;
end;

procedure RunReplaceNext;
begin
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;
  Is_Replacing := true;
  Text_To_Find := FindOptions.ReplacePattern;
  try
    if RunFindNext then
    begin
      if ( ActiveNote.Editor.SelLength > 0 ) then
      begin
        ActiveNote.Editor.SelTextW := FindOptions.ReplaceWith;
        NoteFile.Modified := true;
        UpdateNoteFileState( [fscModified] );
      end;
    end
    else
    begin
      DoMessageBox(WideFormat( STR_02, [Text_To_Find] ), STR_12, 0);
      if assigned(Form_Replace) then
         Form_Replace.SetFocus;
    end;
  finally
    Is_Replacing := false;
  end;
end; // RunReplaceNext


procedure FindResultsToEditor( const SelectedOnly : boolean );
var
  i, cnt : integer;
  aLocation: TLocation;
begin
  if ( not Form_Main.Pages_Res.Visible ) then exit;
  if ( not assigned( ActiveNote )) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;

  cnt := Location_List.Count;
  if ( cnt = 0 ) then
  begin
    showmessage( STR_05 );
    exit;
  end;

  if SelectedOnly then
  begin
    i := Form_Main.List_ResFind.ItemIndex;
    aLocation:= TLocation( Form_Main.List_ResFind.Items.Objects[i]);
    InsertOrMarkKNTLink(aLocation, true, '');
  end
  else
  begin
    for i := 1 to cnt do
    begin
      aLocation:= TLocation( TLocation( Location_List.Objects[pred( i )] ));
      ActiveNote.Editor.SelTextW := #13 + Format( '%d. ', [i] );
      ActiveNote.Editor.SelStart:= ActiveNote.Editor.SelStart + ActiveNote.Editor.SelLength;
      InsertOrMarkKNTLink(aLocation, true, '');
    end;
  end;
  ActiveNote.Editor.SelLength := 0;

end; // FindResultsToEditor


procedure RunFindAllEx;
var
  oldAllNodes, oldEntireScope, oldWrap : boolean;
  FindDone : boolean;
  Location : TLocation; // object, kn_LocationObj.pas
  SearchOpts : TRichSearchTypes;
  noteidx, i, MatchCount, Counter, PatternPos, PatternLen, SearchOrigin : integer;
  myNote : TTabNote;
  myTreeNode : TTreeNTNode;
  myTNote: TTreeNote;         // [dpv]
  myNoteNode : TNoteNode;
  lastNoteID, lastNodeID : integer;
  lastTag : integer;
  //Form_RTF : TForm_tmpRTF;         // [dpv]   (002)
  RTFAux : TRxRichEdit;              // [dpv]   (002)
  numNodosNoLimpiables: integer;
  thisWord : WideString;
  wordList : TStringList;
  wordidx, wordcnt : integer;
  MultiMatchOK : boolean;
  ApplyFilter: boolean;         // [dpv]
  nodeToFilter: boolean;        // [dpv]
  nodesSelected: boolean;       // [dpv]
  oldActiveNote: TTabNote;

        procedure AddLocation; // INLINE
        var
          path : wideString;
        begin
          if assigned( myTreeNode ) then
          begin
            if ApplyFilter and (not nodesSelected) then      // [dpv] Only once per note
               MarkAllFiltered(myTNote);           // There will be at least one node in the selection
            nodeToFilter:= false;      // [dpv]
            nodesSelected:= true;      // [dpv]
          end;
          path := WideFormat( '%d. %s', [Counter, PathOfKNTLink(myTreeNode, myNote, location.CaretPos)] );

          Location_List.AddObject(
            path,
            Location
          );
        end; // AddLocation
begin

  if ( not Form_Main.HaveNotes( true, true )) then exit;
  if ( not assigned( ActiveNote )) then exit;
  if ( FileIsBusy or SearchInProgress ) then exit;

  if ( Form_Main.Combo_ResFind.Text = '' ) then exit;

  oldActiveNote:= ActiveNote; // [dpv] For use if ApplyFilter=true

  Form_Main.CloseNonModalDialogs;

  with Form_Main do
  begin
    // add search pattern to history
    if ( Combo_ResFind.Items.IndexOf( Combo_ResFind.Text ) < 0 ) then
      Combo_ResFind.Items.Insert( 0, Combo_ResFind.Text );

    // transfer FindAll options to FindOptions
    FindOptions.MatchCase := CB_ResFind_CaseSens.Checked;
    FindOptions.WholeWordsOnly := CB_ResFind_WholeWords.Checked;
    FindOptions.AllTabs := CB_ResFind_AllNotes.Checked;
    FindOptions.SearchNodeNames := CB_ResFind_NodeNames.Checked;
    FindOptions.SearchMode := TSearchMode( RG_ResFind_Type.ItemIndex );
    FindOptions.HiddenNodes:= CB_ResFind_HiddenNodes.Checked;   // [dpv]

    ApplyFilter:= CB_ResFind_Filter.Checked;   // [dpv]
  end;


  // these FindOptions will be preserved and restored
  oldAllNodes := FindOptions.AllNodes;
  FindOptions.AllNodes := true;
  oldEntireScope := FindOptions.EntireScope;
  FindOptions.EntireScope := true;
  oldWrap := FindOptions.Wrap;
  FindOptions.Wrap := false;

  FindOptions.FindNew := true;
  FindOptions.Pattern := trim( Form_Main.Combo_ResFind.Text ); // leading and trailing blanks need to be stripped

  myTreeNode := nil;
  myNoteNode := nil;

  FindOptions.FindAllMatches := true;

  if FindOptions.WholeWordsOnly then
    SearchOpts := [stWholeWord]
  else
    SearchOpts := [];
  if FindOptions.MatchCase then
    SearchOpts := SearchOpts + [stMatchCase];

  PatternPos := 0;
  PatternLen := length( FindOptions.Pattern );
  MatchCount := 0;
  FindDone := false;
  noteidx := 0;
  Counter := 0;

  //Form_RTF := TForm_tmpRTF.Create( Form_Main );        // [dpv]
  RTFAux := TRxRichEdit.Create( ActiveNote.TabSheet);
  RTFAux.Visible:= False;
  RTFAux.Parent:=ActiveNote.TabSheet ;

  SearchInProgress := true;
  screen.Cursor := crHourGlass;
  Form_Main.List_ResFind.Items.BeginUpdate;

  wordList := TStringList.Create;


  try
    try
      ClearLocationList( Location_List );
      Form_Main.List_ResFind.Items.Clear;

      CSVTextToStrs( wordList, FindOptions.Pattern, #32 );
      wordcnt := wordList.count;


      repeat

        SearchOrigin := 0; // starting a new note
        nodesSelected:= false; // [dpv] in this note, at the moment

        if FindOptions.AllTabs then
          myNote := TTabNote( Form_Main.Pages.Pages[noteidx].PrimaryObject ) // initially 0
        else
          myNote := ActiveNote; // will exit after one loop

        case myNote.Kind of
          ntRTF : begin

            myTreeNode := nil;
            myNoteNode := nil;

            case FindOptions.SearchMode of
              smPhrase : begin

                repeat

                  PatternPos := myNote.Editor.FindText(
                    FindOptions.Pattern,
                    SearchOrigin, myNote.Editor.GetTextLen - SearchOrigin,
                    SearchOpts
                  );
                  if ( PatternPos >= 0 ) then
                  begin
                    SearchOrigin := PatternPos + PatternLen; // move forward in text
                    inc( Counter );
                    Location := TLocation.Create;
                    Location.FileName := Notefile.FileName;
                    Location.NoteName := myNote.Name;
                    Location.CaretPos := PatternPos;
                    Location.SelLength := PatternLen;
                    Location.NoteID := myNote.ID;
                    AddLocation;
                  end;

                  Application.ProcessMessages;
                  if UserBreak then
                  begin
                    FindDone := true;
                  end;

                until ( FindDone or ( PatternPos < 0 )); // repeat

              end; // smPhrase

              smAny, smAll : begin
                MultiMatchOK := false;
                for wordidx := 1 to wordcnt do
                begin
                  thisWord := wordList[pred( wordidx )];

                  PatternPos := myNote.Editor.FindText(
                    thisWord,
                    0, myNote.Editor.GetTextLen,
                    SearchOpts
                  );

                  case FindOptions.SearchMode of
                    smAll : begin
                      if ( PatternPos >= 0 ) then
                      begin
                        MultiMatchOK := true; // assume success
                      end
                      else
                      begin
                        MultiMatchOK := false;
                        break; // note must have ALL words
                      end;
                    end; // smAll

                    smAny : begin
                      if ( PatternPos >= 0 ) then
                      begin
                        MultiMatchOK := true;
                        break; // enough to find just 1 word
                      end;
                    end; // smAny

                  end; // case FindOptions.SearchMode of

                  Application.ProcessMessages;
                  if UserBreak then
                  begin
                    FindDone := true;
                    break;
                  end;

                end; // for wordidx

                if MultiMatchOK then
                begin
                  inc( Counter );
                  Location := TLocation.Create;
                  Location.FileName := Notefile.FileName;
                  Location.NoteName := myNote.Name;
                  Location.CaretPos := 0;
                  Location.SelLength := 0;
                  Location.NoteID := myNote.ID;
                  AddLocation;
                end;

              end; // smAny, smAll

            end; // case FindOptions.SearchMode

          end; // ntRTF

          ntTree : begin
            myTNote:= TTreeNote( myNote);
            //ActiveNote.EditorToDataStream; // update node's datastream    [dpv]
            myTNote.EditorToDataStream; // update node's datastream

            myTreeNode := myTNote.TV.Items.GetFirstNode;
            if assigned( myTreeNode ) and myTreeNode.Hidden and (not FindOptions.HiddenNodes) then     // [dpv]
               myTreeNode := myTreeNode.GetNextNotHidden;

            while assigned( myTreeNode ) do // go through all nodes
            begin
              nodeToFilter:= true;      // [dpv]
              // get this node's object and transfer
              // its RTF data to temp editor
              myNoteNode := TNoteNode( myTreeNode.Data );
              myNoteNode.Stream.Position := 0;
              //Form_RTF.RTF.Lines.LoadFromStream( myNoteNode.Stream );   // [dpv]  (002)
              RTFAux.Lines.LoadFromStream( myNoteNode.Stream );           // [dpv]  (002)
              SearchOrigin := 0;

              case FindOptions.SearchMode of

                smPhrase : begin

                  // look for match in node name first
                  if FindOptions.SearchNodeNames then
                  begin
                    if FindOptions.MatchCase then
                      PatternPos := pos( FindOptions.Pattern, myNoteNode.Name )
                    else
                      PatternPos := pos( ansilowercase( FindOptions.Pattern ), ansilowercase( myNoteNode.Name ));
                    if ( PatternPos > 0 ) then
                    begin
                      inc( Counter );
                      Location := TLocation.Create;
                      Location.FileName := notefile.FileName;
                      Location.NoteName := myNote.Name;
                      Location.NodeName := myNoteNode.Name;
                      Location.CaretPos := -1; // means: text found in node name
                      Location.SelLength := 0;
                      Location.NoteID := myNote.ID;
                      Location.NodeID := myNoteNode.ID;
                      AddLocation;
                    end;
                  end;

                  repeat // find all matches in current node

                    // search in the temp editor
                    PatternPos := RTFAux.FindText(
                      FindOptions.Pattern,
                      SearchOrigin, RTFAux.GetTextLen - SearchOrigin,
                      SearchOpts
                    );

                    if ( PatternPos >= 0 ) then
                    begin
                      SearchOrigin := PatternPos + PatternLen; // move forward in text
                      inc( Counter );
                      Location := TLocation.Create;
                      Location.FileName := notefile.FileName;
                      Location.NoteName := myNote.Name;
                      Location.NodeName := myNoteNode.Name;
                      Location.CaretPos := PatternPos;
                      Location.SelLength := PatternLen;
                      Location.NoteID := myNote.ID;
                      Location.NodeID := myNoteNode.ID;
                      AddLocation;
                    end;

                    Application.ProcessMessages;
                    if UserBreak then
                    begin
                      FindDone := true;
                    end;

                  until ( FindDone or ( PatternPos < 0 )); // repeat

                end; // smPhrase

                smAny, smAll : begin

                  MultiMatchOK := false;
                  for wordidx := 1 to wordcnt do
                  begin
                    thisWord := wordList[pred( wordidx )];

                    PatternPos := RTFAux.FindText(
                      thisWord,
                      0, RTFAux.GetTextLen,
                      SearchOpts
                    );

                    case FindOptions.SearchMode of
                      smAll : begin
                        if ( PatternPos >= 0 ) then
                        begin
                          MultiMatchOK := true; // assume success
                        end
                        else
                        begin
                          MultiMatchOK := false;
                          break; // note must have ALL words
                        end;
                      end; // smAll

                      smAny : begin
                        if ( PatternPos >= 0 ) then
                        begin
                          MultiMatchOK := true;
                          break; // enough to find just 1 word
                        end;
                      end; // smAny

                    end; // case FindOptions.SearchMode of

                    Application.ProcessMessages;
                    if UserBreak then
                    begin
                      FindDone := true;
                      break;
                    end;

                  end; // for wordidx

                  if MultiMatchOK then
                  begin
                    inc( Counter );
                    Location := TLocation.Create;
                    Location.FileName := notefile.FileName;
                    Location.NoteName := myNote.Name;
                    Location.NodeName := myNoteNode.Name;
                    Location.CaretPos := 0;
                    Location.SelLength := 0;
                    Location.NoteID := myNote.ID;
                    Location.NodeID := myNoteNode.ID;
                    AddLocation;
                  end;

                end; // smAny, smAll

              end; // case FindOptions.SearchMode


              RTFAux.Clear;
              checkEmpty(RTFAux);    // [dpv]
              if ApplyFilter and (not nodeToFilter) then    // [dpv]
                 TNoteNode(myTreeNode.Data).Filtered := false;

              if FindOptions.HiddenNodes then     // [dpv]
                 myTreeNode := myTreeNode.GetNext
              else
                 myTreeNode := myTreeNode.GetNextNotHidden;

            end; // while assigned( myTreeNode ) do
          end; // ntTree

        end;

        if ApplyFilter and nodesSelected then begin   // [dpv]
           Form_Main.FilterApplied(myTNote);
           ActiveNote:= nil;      // -> TreeNodeSelected will exit doing nothing
           HideFilteredNodes (myTNote);
           if myTNote.HideCheckedNodes then
              HideCheckedNodes (myTNote);

           ActiveNote:= myNote;
           myTreeNode := myTNote.TV.Items.GetFirstNode;
           if myTreeNode.Hidden then myTreeNode := myTreeNode.GetNextNotHidden;
           myTNote.TV.Selected:= nil;
           myTNote.TV.Selected:= myTreeNode;   // force to select -> TreeNodeSelected
           ActiveNote:= oldActiveNote;

           NoteFile.Modified := true;
           UpdateNoteFileState( [fscModified] );
        end;


        if FindOptions.AllTabs then
        begin
          inc( noteidx );
          if ( noteidx >= NoteFile.NoteCount ) then
            FindDone := true;
        end
        else
        begin
          FindDone := true;
        end;

      until FindDone;


      MatchCount := Location_List.Count;
      if ( MatchCount > 0 ) then
      begin
        lastNoteID := 0;
        lastNodeID := 0;
        lastTag := 0;
        for i := 1 to MatchCount do
        begin
          Location := TLocation( Location_List.Objects[pred( i )] );
          if ( i > 1 ) then
          begin
            if (( lastNoteID <> Location.NoteID ) or ( lastNodeID <> Location.NodeID )) then
            begin
              case lastTag of
                0 : Location.Tag := 1;
                else
                  Location.Tag := 0;
              end;
            end
            else
            begin
              Location.Tag := lastTag;
            end;
          end;
          lastNoteID := Location.NoteID;
          lastNodeID := Location.NodeID;
          lastTag := Location.Tag;
          Form_Main.List_ResFind.Items.AddObject(
            Location_List[pred( i )],
            Location
          );
        end;
        with Form_Main do
        begin
          List_ResFind.ItemIndex := 0;
          try
            Btn_ResFlip.Caption := STR_06;
            Ntbk_ResFind.PageIndex := 0;
            List_ResFind.SetFocus;
          except
          end;
        end;
      end
      else
      begin
        DoMessageBox(WideFormat( STR_02, [FindOptions.Pattern] ), STR_12, 0);
      end;

    except
      on E : Exception do
      begin
        messagedlg( E.Message, mtError, [mbOK], 0 );
      end;
    end;
  finally
    ActiveNote:= oldActiveNote;   // [dpv]
    UpdateNoteDisplay;  // [dpv]

    // restore previous FindOptions settings
    Form_Main.List_ResFind.Items.EndUpdate;
    FindOptions.AllNodes := oldAllNodes;
    FindOptions.EntireScope := oldEntireScope;
    FindOptions.Wrap := oldWrap;
    FindOptions.FindAllMatches := false;
    SearchInProgress := false;
    screen.Cursor := crDefault;
    wordList.Free;
    RTFAux.Free;
  end;

end; // RunFindAllEx


function RunFindNext : boolean;
var
  myNote : TTabNote;
  myTreeNode : TTreeNTNode;
  FindDone, Found : boolean;
  PatternPos : integer;
  SearchOrigin : integer;
  ScannedTabs, ScannedNodes, tabidx : integer;
  SearchOpts : TRichSearchTypes;
begin
  result := false;
  if ( not ( Form_Main.HaveNotes( true, true ) and assigned( ActiveNote ))) then exit;
  if ( SearchInProgress or FileIsBusy or ( Text_To_Find {FindOptions.Pattern} = '' )) then exit;

  FindOptions.FindAllMatches := false; // only TRUE when invoked from resource panel

  FindDone := false;
  Found := false;
  ScannedTabs := 0;
  ScannedNodes := 0;
  PatternPos := -1;
  UserBreak := false;
  myTreeNode := nil;

  if FindOptions.WholeWordsOnly then
    SearchOpts := [stWholeWord]
  else
    SearchOpts := [];
  if FindOptions.MatchCase then
    SearchOpts := SearchOpts + [stMatchCase];

  Form_Main.StatusBar.Panels[PANEL_HINT].text := STR_07;

  if ( FindOptions.FindNew and FindOptions.EntireScope ) then
  begin
    SearchOrigin := 0;
  end
  else
  begin
    if ( ActiveNote.Editor.SelLength <> length( Text_To_Find {FindOptions.Pattern} )) then
      SearchOrigin := ActiveNote.Editor.SelStart
    else
      SearchOrigin := ActiveNote.Editor.SelStart+1;
  end;
  FindOptions.FindNew := false;

  SearchInProgress := true;
  try
    try
      myNote := ActiveNote;
      myNote.Tag := 0; // mark note as NOT searched;
      if ( myNote.Kind = ntTree ) then
        myTreeNode := TTreeNote( myNote ).TV.Selected;

      while ( not FindDone ) do
      begin

        case myNote.Kind of
          ntRTF : begin
            PatternPos := myNote.Editor.FindText(
              Text_To_Find,
              SearchOrigin, myNote.Editor.GetTextLen - SearchOrigin,
              SearchOpts
            );
          end;

          ntTree : begin
            if ( ScannedNodes = 0 ) then
            begin
              ActiveNote.EditorToDataStream; // KONIECZNIE!
              PatternPos := myNote.Editor.FindText(
                // FindOptions.Pattern,
                Text_To_Find,
                SearchOrigin, myNote.Editor.GetTextLen - SearchOrigin,
                SearchOpts
              );
            end;

            if ( PatternPos < 0 ) then
            begin
              if ( FindOptions.AllTabs or FindOptions.AllNodes ) then
              begin
                if ( ScannedNodes = 0 ) then
                begin
                  inc( ScannedNodes );
                  SearchOrigin := 0;
                  if assigned( myTreeNode ) then
                    if FindOptions.HiddenNodes then     // [dpv]
                       myTreeNode := myTreeNode.GetNext
                    else
                       myTreeNode := myTreeNode.GetNextNotHidden;
                end;
                PatternPos := SearchTree( myTreeNode, SearchOrigin, SearchOpts );
              end
              else
              begin
                FindDone := true;
              end;
            end;
          end;
        end;

        if ( PatternPos >= 0 ) then
        begin
          // pattern found, display note (select tree node if
          // necessary) and position caret at pattern
          Found := true;
          FindOptions.FindNew := false;
          FindDone := true;

          if ( myNote <> ActiveNote ) then
          begin
            with Form_Main do begin
              Pages.ActivePage := myNote.TabSheet;
              PagesChange( Pages );
            end;
          end;

          if ( ActiveNote.Kind = ntTree ) then
            if TTreeNote( ActiveNote ).TV.Selected <> myTreeNode then begin
              myTreeNode.MakeVisible;  // Could be hidden
              TTreeNote( ActiveNote ).TV.Selected := myTreeNode;
            end;

          with myNote.Editor do
          begin
            SelStart := PatternPos;
            SelLength := length( Text_To_Find {FindOptions.Pattern} );
            try
              //   SetFocus;
            except
            end;
          end;

        end
        else
        begin
          // Pattern not found. Abort, or scan
          // other notes if AllTabs is TRUE
          if ( FindOptions.AllTabs and ( Form_Main.Pages.PageCount > 1 )) then
          begin
            inc( ScannedTabs );
            if (( ScannedTabs < Form_Main.Pages.PageCount )
              or ( FindOptions.Wrap and ( not Is_Replacing ) and ( ScannedTabs = Form_Main.Pages.PageCount )))
              then
            begin
              SearchOrigin := 0; // other notes, always look from top
              tabidx := myNote.TabSheet.PageIndex;
              if ( tabidx < pred( Form_Main.Pages.PageCount )) then
                inc( tabidx )
              else
                tabidx := 0;

              myNote := TTabNote( Form_Main.Pages.Pages[tabidx].PrimaryObject );

              if ( myNote.Kind = ntTree ) then
              begin
                if myNote.Editor.Modified then
                begin
                  myNote.EditorToDataStream;
                  myNote.Editor.Modified := false;
                  myNote.Modified := true;
                end;
                myTreeNode := TTreeNote( myNote ).TV.Items.GetFirstNode;
                if assigned( myTreeNode ) and myTreeNode.Hidden and (not FindOptions.HiddenNodes) then     // [dpv]
                   myTreeNode := myTreeNode.GetNextNotHidden;

              end;

            end
            else
            begin
              FindDone := true;
            end;
          end
          else
          begin
            // We're NOT looking through all tabs, so check if we should
            // wrap to the top of current note
            if ( FindOptions.Wrap and ( not Is_Replacing )) then
            begin
              SearchOrigin := 0;
              if ( myNote.Kind = ntTree ) then begin
                myTreeNode := TTreeNote( myNote ).TV.Items.GetFirstNode;
                if assigned( myTreeNode ) and myTreeNode.Hidden and (not FindOptions.HiddenNodes) then     // [dpv]
                   myTreeNode := myTreeNode.GetNextNotHidden;
              end;
              if ( myNote.Tag = 0 ) then
              begin
                myNote.Tag := 1; // mark this note as searched
                FindDone := false; // start over from top (fix for .Wrap bug in tree nodes
              end
              else
                FindDone := true;
            end
            else
            begin
              FindDone := true;
            end;
          end;
        end;

        Application.ProcessMessages;
        if UserBreak then
          FindDone := true;

      end; // while not FindDone


    except
      on E : Exception do
      begin
        PopupMessage( STR_08 +#13+ E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end;

  finally
    SearchInProgress := false;
    result := Found;

    if Found then
    begin
      Form_Main.StatusBar.Panels[PANEL_HINT].text := STR_09 + inttostr( PatternPos );

      if IsRecordingMacro then
      begin
        AddMacroEditCommand( ecFindText );
      end;

    end
    else
    begin
      Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_10;
      if ( not ( UserBreak or Is_Replacing )) then
       DoMessageBox(WideFormat( STR_02, [Text_To_Find] ), STR_12, 0);
       if assigned(Form_Find) then
          Form_Find.SetFocus
       else if assigned(Form_Replace) then
          Form_Replace.SetFocus;
    end;

    UserBreak := false;

  end;

end; // RunFindNext;

function SearchTree(
  var StartTreeNode : TTreeNTNode;
  SearchStart : integer;
  const SearchOpts : TRichSearchTypes ) : integer;
var
  //Form_RTF : TForm_tmpRTF;
  RTFAux : TRxRichEdit;              // DPV (002)
  myTreeNode : TTreeNTNode;
  myNoteNode : TNoteNode;
begin
  result := -1;
  myTreeNode := StartTreeNode;

  //Form_RTF := TForm_tmpRTF.Create( Form_Main );
  RTFAux := TRxRichEdit.Create( ActiveNote.TabSheet);
  RTFAux.Visible:= False;
  RTFAux.Parent:=ActiveNote.TabSheet ;

  try

    while assigned( myTreeNode ) do
    begin
      // get this node's object and transfer
      // its RTF data to temp editor
      myNoteNode := TNoteNode( myTreeNode.Data );
      myNoteNode.Stream.Position := 0;
      RTFAux.Lines.LoadFromStream( myNoteNode.Stream );

      // search in the temp editor
      result := RTFAux.FindText(
        // FindOptions.Pattern,
        Text_To_Find,
        SearchStart, RTFAux.GetTextLen,
        SearchOpts
      );
      if ( result >= 0 ) then
      begin
        StartTreeNode := myTreeNode;
        break;
      end;

      Application.ProcessMessages;
      if UserBreak then break;

      RTFAux.Clear;
      checkEmpty(RTFAux);    // [dpv]
      SearchStart := 0;

      if FindOptions.HiddenNodes then
         myTreeNode := myTreeNode.GetNext
      else
         myTreeNode := myTreeNode.GetNextNotHidden;

//      // wrap around, but make sure we scan the whole tree only once
//      if ( not assigned( MyTreeNode )) then
//      begin
//        if FindOptions.AllTabs then
//          break; // this tree done; will move to next note (bugfix)
//        MyTreeNode := TTreeNT( StartTreeNode.TreeView ).Items.GetFirstNode;
//      end;
//      if ( MyTreeNode = StartTreeNode ) then
//        break;
    end;

  finally
    RTFAux.Free;
  end;

end; // SearchTree

procedure FindEventProc( sender : TObject );
begin
  if assigned( Form_Find ) then
  begin
    FindOptions := Form_Find.MyFindOptions;
    Text_To_Find := FindOptions.Pattern;
    if RunFindNext then
    begin
      Form_Find.MyFindOptions := FindOptions; // must preserve .FindNew field
      if FindOptions.AutoClose then
        Form_Find.Close
      else
        Form_Find.SetFocus;
    end
    else
    begin
      if FindOptions.AutoClose then
        Form_Find.Close;
    end;
  end
  else
  begin
    if assigned( Form_Replace ) then
    begin
      FindOptions := Form_Replace.MyFindOptions;
      Text_To_Find := FindOptions.ReplacePattern;
      if RunFindNext then
      begin
        Form_Replace.MyFindOptions := FindOptions; // must preserve .FindNew field
        {
        if FindOptions.AutoClose then
          Form_Replace.Close
        else
        }
          Form_Replace.SetFocus;
      end
      else
      begin
        {
        if FindOptions.AutoClose then
          Form_Replace.Close;
        }
      end;
    end;
  end;
end; // FindEventProc

procedure ReplaceEventProc( ReplaceAll : boolean );
var
  ReplaceCnt : integer;
  ReplaceOK : boolean;
  Original_Confirm : boolean;
  Original_EntireScope : boolean;
begin
  ReplaceCnt := 0;
  if ( not assigned( Form_Replace )) then exit;
  FindOptions := Form_Replace.MyFindOptions;
  Text_To_Find := FindOptions.ReplacePattern;
  Original_Confirm := FindOptions.ReplaceConfirm;
  Original_EntireScope := FindOptions.EntireScope;
  if ReplaceAll then
    FindOptions.EntireScope := true;

  Is_Replacing := true;
  try
    while RunFindNext do
    begin
      try
        if ( ActiveNote.Editor.SelLength > 0 ) then
        begin

          ReplaceOK := false;
          if FindOptions.ReplaceConfirm then
          begin
            case messagedlg( STR_01,
              mtConfirmation, [mbYes,mbNo,mbAll,mbCancel], 0 ) of
              mrYes : ReplaceOK := true;
              mrNo : ReplaceOK := false;
              mrAll : begin
                ReplaceOK := true;
                FindOptions.ReplaceConfirm := false;
              end;
              mrCancel : begin
                ReplaceOK := false;
                ReplaceAll := false; // will break out of loop
              end;
            end;
          end
          else
          begin
            ReplaceOK := true;
          end;

          if ReplaceOK then
          begin
            inc( ReplaceCnt );
            ActiveNote.Editor.SelTextW := FindOptions.ReplaceWith;
            ActiveNote.Editor.SelStart := ActiveNote.Editor.SelStart + length( ActiveNote.Editor.SelTextW );
          end;
        end;

      Application.ProcessMessages;
      if UserBreak then break;

      except
        On E : Exception do
        begin
          showmessage( E.Message );
          break;
        end;
      end;
      if ( not ReplaceAll ) then break;
    end;
    Form_Replace.SetFocus;
  finally
    Is_Replacing := false;
    UserBreak := false;
    FindOptions.ReplaceConfirm := Original_Confirm;
    FindOptions.EntireScope := Original_EntireScope;
  end;

  Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_11, [ReplaceCnt] );
  if ( ReplaceCnt > 0 ) then
  begin
    NoteFile.Modified := true;
    UpdateNoteFileState( [fscModified] );
  end
  else
  begin
    DoMessageBox(WideFormat( STR_02, [Text_To_Find] ), STR_12, 0);
    if assigned(Form_Replace) then
       Form_Replace.SetFocus;
  end;

end; // ReplaceEventProc

procedure Form_FindClosed( sender : TObject );
begin
  try
    try
      if SearchInProgress then
      begin
        UserBreak := true;
        SearchInProgress := false;
      end;
      FindOptions := Form_Find.MyFindOptions;
      Form_Find.Release;
    except
    end;
  finally
    Form_Find := nil;
    Form_Main.FindNotify( true );
  end;
end; // Form_FindClosed

procedure Form_ReplaceClosed( sender : TObject );
begin
  try
    try
      if SearchInProgress then
      begin
        UserBreak := true;
        SearchInProgress := false;
      end;
      FindOptions := Form_Replace.MyFindOptions;
      Form_Replace.Release;
    except
    end;
  finally
    Form_Replace := nil;
    Form_Main.FindNotify( true );
  end;
end; // Form_ReplaceClosed


Initialization
    Text_To_Find := '';
    Is_Replacing := false;
    Form_Find := nil;
    Form_Replace := nil;
    SearchInProgress := false;
    UserBreak := false;
    _Executing_History_Jump := false;
    _LastMoveWasHistory := false;

end.
