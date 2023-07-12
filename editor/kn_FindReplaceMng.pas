unit kn_FindReplaceMng;

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
uses
   Winapi.Windows,
   Winapi.RichEdit,
   System.Classes,
   System.SysUtils,
   Vcl.Dialogs,
   Vcl.Forms,
   Vcl.Controls,
   TreeNT,
   RxRichEd,
   gf_miscvcl,
   gf_strings,
   gf_streams,
   kn_const,
   kn_Info,
   kn_Global,
   kn_LocationObj,
   kn_FindReplace,
   kn_NoteObj,
   kn_RTFUtils,
   kn_NodeList,
   kn_Cmd,
   kn_VCLControlsMng,
   kn_NoteMng,
   kn_TreeNoteMng,
   kn_MacroMng,
   kn_NoteFileMng,
   kn_EditorUtils;


var
    Text_To_Find : string;

    Form_FindReplace : TForm_FindReplace;  // GLOBAL FORM!

    Is_Replacing : boolean;     // Find and Replace functions share some procedures; this is how we tell the difference where necessary
    SearchInProgress : boolean; // TRUE while searching or replacing
    UserBreak : boolean;

    SearchNode_Text, SearchNode_TextPrev : string;
    StartNote: TTabNote;
    StartNode: TTreeNTNode;


procedure RunFinder;
function RunFindNext (Is_ReplacingAll: Boolean= False): boolean;
procedure RunFindAllEx;
procedure RunReplace;
procedure RunReplaceNext;
procedure FindEventProc( sender : TObject );
procedure ReplaceEventProc( ReplaceAll : boolean );
procedure Form_FindReplaceClosed( sender : TObject );
procedure FindResultsToEditor( const SelectedOnly : boolean );


implementation
uses
  kn_LinksMng,
  kn_Main;


var
   NumberFoundItems: integer;
   EditControl: TRxRichEdit;


resourcestring
  STR_01 = 'Replace this occurrence?';
  STR_02 = 'Pattern not found: "%s"';
  STR_03 = 'Note "%s" does not exist in this file.';
  STR_04 = 'Tree node "%s" does not exist in note "%s".';
  STR_05 = 'Search results are not available.';
  STR_06 = 'Options';
  STR_07 = ' Searching - press ESC to abort.';
  STR_08 = 'An error occurred during search:';
  STR_09 = ' Pattern found at pos %d (%d occurrence(s))';
  STR_10 = ' Pattern not found.';
  STR_11 = ' Replaced %d occurrence(s)';
  STR_12 = 'Information';


procedure RunFindReplace (modeReplace: boolean);
begin
  if ( not Form_Main.HaveNotes( true, true )) then exit;
  if ( not assigned( ActiveNote )) then exit;
  if ( FileIsBusy or SearchInProgress ) then exit;

  if ( ActiveNote.Editor.SelLength > 0 ) then
     FindOptions.Pattern := trim( ActiveNote.Editor.SelVisibleText )
  else
     if FindOptions.WordAtCursor then
        FindOptions.Pattern := ActiveNote.Editor.GetWordAtCursor( true );

  FindOptions.FindAllMatches := false; // only TRUE when invoked from resource panel

  if ( Form_FindReplace = nil ) then
  begin
    Form_FindReplace := TForm_FindReplace.Create( Form_Main );
    FindOptions.FindNew := true;
    with Form_FindReplace do
    begin
      myNotifyProc := Form_Main.FindNotify;
      MyFindOptions := FindOptions;
      ShowHint := KeyOptions.ShowTooltips;
      FindEvent := FindEventProc;
      ReplaceEvent := ReplaceEventProc;
      FormCloseEvent := Form_FindReplaceClosed;
    end;
  end;

  try
    Form_FindReplace.Combo_Text.Text := FindOptions.Pattern;
    Form_FindReplace.ModeReplace:= modeReplace;
    Form_FindReplace.Show;

  except
     on E : Exception do
        messagedlg( E.Message, mtError, [mbOK], 0 );
  end;

end; // RunFindReplace


procedure RunFinder;
begin
  RunFindReplace (false);
end;

procedure RunReplace;
begin
  RunFindReplace (true);
end;


procedure RunReplaceNext;
begin
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;

  ReplaceEventProc(false);
end; // RunReplaceNext


procedure FindResultsToEditor( const SelectedOnly : boolean );
var
  i, cnt : integer;
  aLocation: TLocation;
  CaretInKNTLinksBAK: boolean;
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

  CaretInKNTLinksBAK:= TreeOptions.CaretInKNTLinks;
  TreeOptions.CaretInKNTLinks:= True;
  try
      if SelectedOnly then begin
        i := Form_Main.List_ResFind.ItemIndex;
        aLocation:= TLocation( Form_Main.List_ResFind.Items.Objects[i]);
        InsertOrMarkKNTLink(aLocation, true, '');
      end
      else begin
        for i := 1 to cnt do begin
          aLocation:= TLocation( TLocation( Location_List.Objects[pred( i )] ));
          ActiveNote.Editor.SelText := #13 + Format( '%d. ', [i] );
          ActiveNote.Editor.SelStart:= ActiveNote.Editor.SelStart + ActiveNote.Editor.SelLength;
          InsertOrMarkKNTLink(aLocation, true, '');
        end;
      end;
      ActiveNote.Editor.SelLength := 0;

  finally
    TreeOptions.CaretInKNTLinks:= CaretInKNTLinksBAK;
  end;

end; // FindResultsToEditor



function FindPattern (const Substr: string; Str: String; SearchOrigin: integer; var SizeInternalHiddenText: integer; IgnoreKNTHiddenMarks: boolean = true): integer;
var
{ The search string could contain hidden text corresponding to a used marker (for now only this type of hidden marker)
  as internal KNT link target. This would make it impossible to locate the searched string (currently done with RxRichEdit.FindText) 
  if it is not taken into account.
  Assume * a hidden character of KNT and [ and ] delimiting the text to search for. We could find something like: 

   A: [          ]
   B: ***
   C: [          ]    ***
   D: ***    [          ]
   E: [          *** ]
   F: [ ***          ]
   G: [      ***     ]
   H: [      ***   ***  ]
   I: [??????***??]    ***   [          ]
 
  p will have the position of the first occurrence of the search text, from the position to search.
  pH will have the position of the first occurrence of the beginning of the hidden text, from the position to search for.

  If the hidden text is not found (pH=0) or it is placed to the right of the localized text, it does not affect us. We have successfully found
  the first appearance        

  If we find only hidden text, or the position of the hidden text is lower than that of the serched text, how do we know that there 
  was no hidden text that we have not located precisely because it is 'mixed' with that hidden text?
  In this case we must do a search step by step, which ignores possible characters from our hidden string, and which should start
  some distance to the left of the hidden text (in anticipation of case E) and continue searching assuming that the beginning
  of the string to search for could be immediately before the hidden text (case F)
         
  Although it would not be normal, there could be more than one string hidden within the text to be searched for (case H). If upon 
  reaching the second hidden string we see keeping the match in the search text comparison, we will simply ignore this second string and continue
  comparing. If after passing the last character of the first hidden string there was no match, we should continue looking for new possible
  hidden texts, to repeat the process.  
}

 p: integer;          // Position of the search text (first occurrence)
 pH, pHf: integer;    // Start and end position of the first occurrence of hidden text (KNT)
 PPattern: PChar;     // Pointer to pattern
 PText: PChar;        // Pointer to the text where to search at any time
 posIT: integer;      // Comparison start position, for the text to search for
 iP: integer;         // Index on the pattern, on the comparison started at p
 iT: integer;         // Index on the text where to search, on comparison started at p
 LenPattern: integer; // Length of the text to search
 SizeAux: integer;
 Dif: boolean;
 LenStr: integer;

begin   
    SizeInternalHiddenText:= 0;
   
    if (Substr = '') or (Str='') then Exit(0);
   
    LenPattern:= Length(Substr);
    LenStr := Length(Str);
    pH:= 0;
    if IgnoreKNTHiddenMarks then begin
       pH:= Pos(KNT_RTF_HIDDEN_MARK_L_CHAR, Str, SearchOrigin);
       PPattern:=  @Substr[1];
    end;

    repeat
      p:= Pos(Substr, Str, SearchOrigin);
      
      if FindOptions.WholeWordsOnly and (p > 0) then begin
          if ((p+LenPattern) <= LenStr) and  IsCharAlphaNumeric(Str[p+LenPattern])   then begin
             SearchOrigin:= p+LenPattern +1;
             continue;
          end;
          if (p > 1) and IsCharAlphaNumeric(Str[p-1]) then begin
             SearchOrigin:= p+LenPattern +1;
             continue;
          end;
          break;
      end;
    until (not FindOptions.WholeWordsOnly) or (p=0) or (SearchOrigin >= LenStr);

    
    repeat    
        if (pH=0) or ((pH > p) and (p<>0)) then
           Exit(p)

        else begin
           posIT:= pH - LenPattern+ 1;          // in anticipation of case E (at least 1 character would be placed after the hidden string)
           pHf:= pH;
           
           if posIT <= 0 then
              posIT:= 1;

           repeat                              // New search, starting at posIT
             iP:= -1;
             iT:= -1;
             PText:= @Str[posIT];
             Dif:= False;             

             if FindOptions.WholeWordsOnly then begin               
                repeat
                    if (posIT > 1) and IsCharAlphaNumeric(PText[iT]) then
                       inc(iT)
                    else
                       break;
                until false or (PText[iT]=#0);
                if iT > -1 then begin
                   posIT:= posIT + IT +1;
                   Dif:= True;
                   continue;
                end;
             end;
             
             repeat                          // Keep comparing pattern and text, as long as they match and the end of one or the other is not reached
                inc(iT);
                inc(iP);

                if (PText[iT]=KNT_RTF_HIDDEN_MARK_L_CHAR) then begin     // Ignore our possible hidden strings (just count their length)
                  SizeAux:= 0;
                  repeat
                      inc(iT);
                      inc(SizeInternalHiddenText);
                      inc(SizeAux);
                  until (PText[iT]=KNT_RTF_HIDDEN_MARK_R_CHAR) or (PText[iT]=#0) or (SizeAux > KNT_RTF_HIDDEN_MAX_LENGHT);      // Looking for <StartHiddenText>.....<EndHiddenText>  (ex: HB5H  where  H=KNT_RTF_HIDDEN_MARK_CHAR)
                  if (SizeAux > KNT_RTF_HIDDEN_MAX_LENGHT) then begin
                     Dif:= True;
                     pHf:= pH;
                  end
                  else begin
                    inc(iT);
                    inc(SizeInternalHiddenText);
                    if pHf = pH then
                       pHf:= posIT + iT;
                    if SizeInternalHiddenText = iT then
                       Dif:= True;
                  end;
                end;

                if (PPattern[iP] <> PText[iT]) then
                   Dif:= True;

             until Dif or (iP >= LenPattern-1) or (PText[iT]=#0);

             if not Dif then begin
                if (PText[iT] <> #0) and IsCharAlphaNumeric(PText[iT+1]) then begin
                   posIT:= posIT + iT +1;
                   Dif:= True;
                end
             end
             else begin            
                SizeInternalHiddenText:= 0;
                if (posIT >= pH) then                  
                    posIT:= pHf + 1
                else
                    inc(posIT);                                   // We will start searching from the next position
             end;

           until not Dif or (posIT >=LenStr) or (posIT > pHf);

           if not Dif then
              Exit(posIT)
           else
              pH:= Pos(KNT_RTF_HIDDEN_MARK_L_CHAR, Str, posIT +1);

        end;

    until false;           // We will exit only through the points identified within

end;

{
// Preparar el editor (TTabRichEdit) al que preguntaremos por el texto buscado
procedure PrepareEditControl(myNote: TTabNote; myTreeNode: TTreeNTNode);
var
    myNoteNode : TNoteNode;
begin
    if (myNote.Kind <> ntTree)  or ((myNote = ActiveNote) and (TTreeNote(ActiveNote).TV.Selected = myTreeNode)) then begin
        if myNote.Editor.Modified or (myNote.NoteTextPlain = '') then
            myNote.EditorToDataStream;
        EditControl:= myNote.Editor
    end
    else begin
       // Get the actual node's object and transfer its RTF data to temp editor
        myNoteNode := TNoteNode( myTreeNode.Data );
        myNoteNode.Stream.Position := 0;
        EditControl:= GetAuxEditorControl;              // It will create if it's necessary (lazy load)
        EditControl.Lines.LoadFromStream( myNoteNode.Stream );
    end;
end;
}

function PrepareTextPlain(myNote: TTabNote; myTreeNode: TTreeNTNode): string;
var
    myNoteNode : TNoteNode;
begin

   if myNote.Editor.Modified or ((myNote.Kind <> ntTree) and (myNote.NoteTextPlain = '')) then
      myNote.EditorToDataStream;

   if (myNote.Kind <> ntTree) then
       Result:= myNote.NoteTextPlain
   else begin
      myNoteNode := TNoteNode( myTreeNode.Data );
      TTreeNote(myNote).InitializeTextPlain(myNoteNode);
      Result:= myNoteNode.NodeTextPlain;
   end;

end;


procedure RunFindAllEx;
var
  oldAllNodes, oldEntireScope, oldWrap : boolean;
  FindDone : boolean;
  Location : TLocation; // object, kn_LocationObj.pas
  SearchOpts : TRichSearchTypes;
  noteidx, i, MatchCount, Counter, PatternPos, PatternLen, SearchOrigin : integer;
  myNote : TTabNote;
  myTreeNode : TTreeNTNode;
  myTNote: TTreeNote;
  myNoteNode : TNoteNode;
  lastNoteID, lastNodeID : integer;
  lastTag : integer;
  numNodosNoLimpiables: integer;
  thisWord : string;
  wordList : TStringList;
  wordidx, wordcnt : integer;
  MultiMatchOK : boolean;
  ApplyFilter: boolean;
  nodeToFilter: boolean;
  nodesSelected: boolean;
  oldActiveNote: TTabNote;
  SearchModeToApply : TSearchMode;
  TreeNodeToSearchIn : TTreeNTNode;
  TextPlain, TextToFind: string;
  SizeInternalHiddenText: integer;

type
   TLocationType= (lsNormal, lsNodeName, lsMultimatch);

       procedure AddLocation (LocationType: TLocationType); // INLINE
       var
         path : string;
       begin
          Location := TLocation.Create;
          Location.FileName := Notefile.FileName;
          Location.NoteName := myNote.Name;
          Location.NoteID := myNote.ID;

          if assigned(myTreeNode) then begin
             myNoteNode := TNoteNode(myTreeNode.Data);
             Location.NodeName := myNoteNode.Name;
             Location.NodeID := myNoteNode.ID;
          end;

          case LocationType of
             lsNormal: begin
                 Location.CaretPos := PatternPos;
                 Location.SelLength := PatternLen + SizeInternalHiddenText;
                 end;
             lsNodeName: begin
                 Location.CaretPos := -1; // means: text found in node name
                 Location.SelLength := 0;
                 end;
             lsMultimatch: begin
                 Location.CaretPos := 0;
                 Location.SelLength := 0;
                 end;
          end;

          if assigned( myTreeNode ) then begin
            if ApplyFilter and (not nodesSelected) then     // Only once per note
               MarkAllFiltered(myTNote);           // There will be at least one node in the selection
            nodeToFilter:= false;
            nodesSelected:= true;
          end;
          path := Format( '%d. %s', [Counter, PathOfKNTLink(myTreeNode, myNote, location.CaretPos, true, false)] );
          Location_List.AddObject(path, Location);
       end;

       procedure GetCurrentNode;
       begin
          if myNote.Kind <> ntTree then exit;
          myTreeNode := TTreeNote(myNote).TV.Selected;
          TreeNodeToSearchIn:= myTreeNode;
       end;

       procedure GetFirstNode;
       begin
          if myNote.Kind <> ntTree then exit;

          myTreeNode := TTreeNote(myNote).TV.Items.GetFirstNode;
          if assigned( myTreeNode ) and myTreeNode.Hidden and (not FindOptions.HiddenNodes) then
             myTreeNode := myTreeNode.GetNextNotHidden;
       end;

       procedure GetNextNode;
       begin
          if (myNote.Kind <> ntTree) or (not assigned(myTreeNode)) then exit;

          if FindOptions.HiddenNodes then     // [dpv]
             myTreeNode := myTreeNode.GetNext
          else
             myTreeNode := myTreeNode.GetNextNotHidden;

          if (myTreeNode <> nil) and (TreeNodeToSearchIn <> nil) then
             if ((FindOptions.HiddenNodes) and (TreeNodeToSearchIn.GetNextSibling = myTreeNode))  or
                (not (FindOptions.HiddenNodes) and (TreeNodeToSearchIn.GetNextSiblingNotHidden = myTreeNode)) then begin
              myTreeNode := nil;
              exit;
             end;

       end;

       procedure GetNextNote;
       begin
          if FindOptions.AllTabs then begin
             inc( noteidx );

             if ( noteidx >= NoteFile.NoteCount ) then
                FindDone := true
             else begin
                myNote := TTabNote(Form_Main.Pages.Pages[noteidx].PrimaryObject);
                if myNote.Kind = ntTree then
                   GetFirstNode;
                end;
          end
          else
            FindDone := true;
       end;


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
    FindOptions.CurrentNodeAndSubtree := CB_ResFind_CurrentNodeAndSubtree.Checked;
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

  SearchModeToApply := FindOptions.SearchMode;

  myTreeNode := nil;
  myNoteNode := nil;
  TreeNodeToSearchIn:= nil;

  FindOptions.FindAllMatches := true;

  if FindOptions.WholeWordsOnly then
    SearchOpts := [stWholeWord]
  else
    SearchOpts := [];
  if FindOptions.MatchCase then
    SearchOpts := SearchOpts + [stMatchCase];

  if FindOptions.MatchCase then
     TextToFind:= FindOptions.Pattern
  else
     TextToFind:= AnsiUpperCase(FindOptions.Pattern);
    
  PatternPos := 0;
  PatternLen := length( FindOptions.Pattern );
  MatchCount := 0;
  FindDone := false;
  noteidx := 0;
  Counter := 0;

  SearchInProgress := true;
  screen.Cursor := crHourGlass;
  Form_Main.List_ResFind.Items.BeginUpdate;

  wordList := TStringList.Create;


  try
    try
      ClearLocationList( Location_List );
      Form_Main.List_ResFind.Items.Clear;

      CSVTextToStrs( wordList, TextToFind, #32 );
      wordcnt := wordList.count;

      if wordcnt = 1 then
         SearchModeToApply := smPhrase;    // If not used smPhrase then the line number will not be shown, and will be confusing

      if wordcnt = 0 then begin
         Form_Main.Combo_ResFind.Text:= '';
         Form_Main.Btn_ResFind.Enabled:= False;
         exit;
      end;

      if FindOptions.AllTabs then
         myNote := TTabNote( Form_Main.Pages.Pages[noteidx].PrimaryObject ) // initially 0
      else
         myNote := ActiveNote; // will exit after one loop

      if myNote.Kind = ntTree then begin
         if FindOptions.CurrentNodeAndSubtree then
            GetCurrentNode
         else begin
            GetFirstNode;
            while (not FindDone) and (not assigned(myTreeNode)) do begin
                 GetNextNote();
                if (myNote.Kind <> ntTree) then break;
            end;
         end;
      end;


      // Recorremos cada nota
      repeat
            nodesSelected:= false; // in this note, at the moment
            if myNote.Kind = ntTree then myTNote:= TTreeNote(myNote);

            // Recorremos cada nodo (si es ntTree) o el único texto (si <> ntTree)
            repeat
                // PrepareEditControl(myNote, myTreeNode);
                TextPlain:= PrepareTextPlain(myNote, myTreeNode);
                if not FindOptions.MatchCase then
                   TextPlain:=  AnsiUpperCase(TextPlain);
                
                nodeToFilter:= true;                     // Supongo que no se encontrará el patrón, con lo que se filtrará el nodo (si ApplyFilter=True)
                SearchOrigin := 0; // starting a new node

                case SearchModeToApply of
                    smPhrase :
                        begin
                            // look for match in node name first (si estamos buscando dentro de una nota ntTree)
                            if assigned(myTreeNode) and FindOptions.SearchNodeNames then
                            begin
                              myNoteNode := TNoteNode(myTreeNode.Data);
                              if FindOptions.MatchCase then
                                 PatternPos := pos( FindOptions.Pattern, myNoteNode.Name )
                              else
                                 PatternPos := pos( ansilowercase( FindOptions.Pattern ), ansilowercase( myNoteNode.Name ));
                              if ( PatternPos > 0 ) then begin
                                 inc( Counter );
                                 AddLocation(lsNodeName);
                              end;
                            end;

                            repeat
                               PatternPos:= FindPattern(TextToFind, TextPlain, SearchOrigin+1, SizeInternalHiddenText) -1;
                               { 
                               PatternPos := EditControl.FindText(
                                 FindOptions.Pattern,
                                 SearchOrigin, -1,
                                 SearchOpts
                               );
                               }
                                
                               if ( PatternPos >= 0 ) then begin
                                   SearchOrigin := PatternPos + PatternLen; // move forward in text
                                   inc( Counter );
                                   AddLocation(lsNormal);
                               end;
                               Application.ProcessMessages;
                            until UserBreak or (PatternPos < 0);
                          end;

                    smAny, smAll :
                      begin
                         MultiMatchOK := false;
                         for wordidx := 1 to wordcnt do
                         begin
                            thisWord := wordList[pred( wordidx )];

                            PatternPos:= FindPattern(thisWord, TextPlain, 0, SizeInternalHiddenText) -1;
                            {
                            PatternPos := EditControl.FindText(
                              thisWord,
                              0, -1,
                              SearchOpts
                            );
                            }

                            case SearchModeToApply of
                                smAll:
                                    if ( PatternPos >= 0 ) then
                                       MultiMatchOK := true // assume success
                                    else begin
                                       MultiMatchOK := false;
                                       break; // note must have ALL words
                                    end;

                                smAny:
                                    if ( PatternPos >= 0 ) then begin
                                       MultiMatchOK := true;
                                       break; // enough to find just 1 word
                                    end;
                             end;

                            Application.ProcessMessages;
                            if UserBreak then begin
                               FindDone := true;
                               break;
                            end;

                         end; // for wordidx

                         if MultiMatchOK then begin
                            inc( Counter );
                            AddLocation (lsMultimatch);
                         end;

                      end; // smAny, smAll

                end; // case FindOptions.SearchMode

                if ApplyFilter and ((myNote.Kind = ntTree) and (not nodeToFilter)) then
                   TNoteNode(myTreeNode.Data).Filtered := false;
                GetNextNode;

            until UserBreak or not assigned(myTreeNode);


            if ApplyFilter and nodesSelected then begin
               Form_Main.FilterApplied(myTNote);
               ActiveNote:= nil;      // -> TreeNodeSelected will exit doing nothing
               HideFilteredNodes (myTNote);
               ActiveNote:= myNote;
               myTreeNode := myTNote.TV.Items.GetFirstNode;
               if myTreeNode.Hidden then myTreeNode := myTreeNode.GetNextNotHidden;
               myTNote.TV.Selected:= nil;
               myTNote.TV.Selected:= myTreeNode;   // force to select -> TreeNodeSelected

               NoteFile.Modified := true;
               UpdateNoteFileState( [fscModified] );
               myTreeNode:= nil;
            end;

            while (not UserBreak) and ((not FindDone) and (not assigned(myTreeNode))) do begin
               GetNextNote();
               if (myNote.Kind <> ntTree) then break;
            end;

      until FindDone or UserBreak;


      MatchCount := Location_List.Count;
      if ( MatchCount > 0 ) then
      begin
        lastNoteID := -1;
        lastNodeID := -1;
        lastTag := 1;
        for i := 1 to MatchCount do
        begin
          Location := TLocation( Location_List.Objects[pred( i )] );
          if (( lastNoteID <> Location.NoteID ) or ( lastNodeID <> Location.NodeID )) then begin
              if lastTag = 1 then
                 lastTag := 0
              else
                 lastTag := 1;
              lastNoteID := Location.NoteID;
              lastNodeID := Location.NodeID;
          end;
          Location.Tag := lastTag;
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
         DoMessageBox(Format( STR_02, [FindOptions.Pattern] ), STR_12, 0);

    except
      on E : Exception do
         messagedlg( E.Message, mtError, [mbOK], 0 );
    end;

  finally
    ActiveNote:= oldActiveNote;
    UpdateNoteDisplay;

    // restore previous FindOptions settings
    Form_Main.List_ResFind.Items.EndUpdate;
    FindOptions.AllNodes := oldAllNodes;
    FindOptions.EntireScope := oldEntireScope;
    FindOptions.Wrap := oldWrap;
    FindOptions.FindAllMatches := false;
    SearchInProgress := false;
    screen.Cursor := crDefault;
    wordList.Free;
    FreeAuxEditorControl;
  end;

end; // RunFindAllEx


function RunFindNext (Is_ReplacingAll: Boolean= False): boolean;
var
  myNote : TTabNote;
  myTreeNode : TTreeNTNode;
  FindDone, Found : boolean;
  PatternPos : integer;
  SearchOrigin : integer;
  SearchOpts : TRichSearchTypes;
  handle: HWND;

  l1, l2: integer;
  SizeInternalHiddenText: integer;
  TextPlain: string;

  function LoopCompleted(Wrap: boolean): Boolean;
  begin
      Result:= True;

      if not assigned(myNote) then exit;

      if not (
         (myNote = StartNote)
          and ((myNote.Kind <> ntTree) or (myTreeNode = StartNode))    ) then
         Result:= False
      else
          if Wrap and (NumberFoundItems > 0) then begin    // Wrap será siempre false si Is_ReplacingAll = True
             Result:= False;
             NumberFoundItems:= 0;
             end;
  end;


  procedure GetFirstNode();
  begin
      myTreeNode := TTreeNote( myNote ).TV.Items.GetFirstNode;
      if assigned( myTreeNode ) and myTreeNode.Hidden and (not FindOptions.HiddenNodes) then
         myTreeNode := myTreeNode.GetNextNotHidden;
  end;


  // Actualiza el siguiente nodo a utilizar, que podrá ser nil si no se puede avanzar hacia
  // ningún nodo de la nota actual.
  // También puede establecer FindDone a True indicando que la búsqueda ha finalizado.
  // Si FindDone = False y el nodo = nil implicará que debe continuarse con la siguiente nota.
  //
  procedure GetNextNode();
  var
     Wrap: boolean;
  begin
     FindDone:= (myNote.Kind = ntTree);   // Si es una nota de tipo árbol supondremos inicialmente que no se podrá avanzar más

     if not assigned(myTreeNode) or FindOptions.SelectedText then exit;

     Wrap:= FindOptions.Wrap and not Is_ReplacingAll;
     if not (FindOptions.AllTabs_FindReplace or FindOptions.AllNodes or Wrap) then exit;

     if FindOptions.AllTabs_FindReplace or FindOptions.AllNodes then begin
         if FindOptions.HiddenNodes then
            myTreeNode := myTreeNode.GetNext
         else
            myTreeNode := myTreeNode.GetNextNotHidden;

         if not assigned( myTreeNode) then
            if (Wrap or Is_ReplacingAll) and (not FindOptions.AllTabs_FindReplace) then  // Si AllTabs -> pasaremos a otra nota
               GetFirstNode;
     end;

     if not LoopCompleted(Wrap) then begin
        FindDone:= False;
        SearchOrigin := 0;
     end;


  end;

  procedure GetNextNote();
  var
     tabidx : integer;
     wrap: boolean;
  begin
      FindDone:= True;   // Supondremos inicialmente que no se podrá avanzar más

      Wrap:= FindOptions.Wrap and not Is_ReplacingAll;

      if (FindOptions.AllTabs_FindReplace) and (not FindOptions.SelectedText) and (Form_Main.Pages.PageCount > 1) then begin
          tabidx := myNote.TabSheet.PageIndex;
          if tabidx < pred(Form_Main.Pages.PageCount) then
             inc(tabidx)
          else
             tabidx := 0;

          myNote := TTabNote(Form_Main.Pages.Pages[tabidx].PrimaryObject);

          if myNote.Kind = ntTree then
             GetFirstNode;

          if not LoopCompleted(Wrap) then begin
             SearchOrigin := 0;
             FindDone:= False;
          end;
      end
      else
          if (myNote.Kind <> ntTree) and (not LoopCompleted(Wrap)) then begin
             SearchOrigin := 0;
             FindDone:= False;
          end;

   end;


  procedure SelectPatternFound();
  begin
      if (myNote <> ActiveNote) then
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
        SelLength := length( Text_To_Find) + SizeInternalHiddenText;
      end;
  end;


begin
  result := false;
  if ( not ( Form_Main.HaveNotes( true, true ) and assigned( ActiveNote ))) then exit;
  if ( SearchInProgress or FileIsBusy or ( Text_To_Find = '' )) then exit;


  if assigned( Form_FindReplace ) then
      handle:= Form_FindReplace.Handle
  else
      handle:= 0;

  FindOptions.FindAllMatches := false; // only TRUE when invoked from resource panel
  FindDone := false;
  Found := false;

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


  SearchInProgress := true;
  try
    try

      // Identificación de la posición de inicio de la búsqueda ---------------------------
      if ( FindOptions.FindNew and FindOptions.EntireScope ) then
          SearchOrigin := 0
      else begin
          SearchOrigin := ActiveNote.Editor.SelStart;
          l1:= length(ActiveNote.Editor.SelVisibleText);
          if l1 = length( Text_To_Find)  then begin
             l2:= length(ActiveNote.Editor.SelText);
             if l1 = l2 then
                inc(SearchOrigin)
             else
                 SearchOrigin:= SearchOrigin + l2;
             
          end;
      end;

      myNote := ActiveNote;
      if myNote.Kind = ntTree then
         myTreeNode := TTreeNote( myNote ).TV.Selected;

      if FindOptions.FindNew then begin
         StartNote:= myNote;
         StartNode:= myTreeNode;
         NumberFoundItems:= 0;
         FindOptions.FindNew := False;
      end;


      // Búsqueda del patrón iterando sobre los nodos / notas hasta encontrar uno -------------
      // Según las opciones establecidas así podrán recorrerse o no todos los nodos de una nota, todas las notas
      // o incluso continuar buscando desde el punto de partida, de manera cíclica.

      repeat
            //PrepareEditControl(myNote, myTreeNode);
            TextPlain:= PrepareTextPlain(myNote, myTreeNode);
            if FindOptions.MatchCase then
               PatternPos:= FindPattern(Text_To_Find, TextPlain, SearchOrigin+1, SizeInternalHiddenText) -1
            else 
               PatternPos:= FindPattern(AnsiUpperCase(Text_To_Find), AnsiUpperCase(TextPlain), SearchOrigin+1, SizeInternalHiddenText) -1;
            
            {
            PatternPos := EditControl.FindText(
              Text_To_Find,
              SearchOrigin, -1,
              SearchOpts
            );
            }
            
            if PatternPos < 0 then begin
               GetNextNode;                 // Podrá actualizar FindDone
               while (not FindDone) and (not assigned(myTreeNode)) do begin
                   GetNextNote();
                   if (myNote.Kind <> ntTree) then break;
               end;
            end;
            Application.ProcessMessages;    // Para permitir que el usuario cancele (UserBreak)

      until FindDone or (PatternPos >= 0) or UserBreak;


      if ( PatternPos >= 0 ) then begin
          // pattern found, display note (select tree node if necessary) and position caret at pattern
          Found := true;
          FindOptions.FindNew := false;
          FindDone := true;
          inc(NumberFoundItems);
          SelectPatternFound();
      end;


    except
      on E: Exception do begin
          PopupMessage( STR_08 +#13+ E.Message, mtError, [mbOK], 0 );
          exit;
      end;
    end;

  finally
      if Found then begin
          Form_Main.StatusBar.Panels[PANEL_HINT].text := Format(STR_09, [PatternPos, NumberFoundItems]);
          if IsRecordingMacro then
             AddMacroEditCommand( ecFindText );
      end
      else begin
          Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_10;
          if not (UserBreak or Is_Replacing) then
             DoMessageBox(Format( STR_02, [Text_To_Find] ), STR_12, 0, handle);
      end;

      result := Found;
      SearchInProgress := false;
      UserBreak := false;
  end;

end; // RunFindNext;


procedure FindEventProc( sender : TObject );
var
   autoClose: boolean;
begin
  if assigned( Form_FindReplace ) then
  begin
      FindOptions := Form_FindReplace.MyFindOptions;
      Text_To_Find := FindOptions.Pattern;
      autoClose:= FindOptions.AutoClose and not Form_FindReplace.ModeReplace;
      if RunFindNext then
      begin
          Form_FindReplace.MyFindOptions := FindOptions; // must preserve .FindNew field
          if autoClose then
            Form_FindReplace.Close
          else
            Form_FindReplace.SetFocus;
      end
      else begin
        if autoClose then
           Form_FindReplace.Close;
      end;
  end;
end; // FindEventProc


procedure ReplaceEventProc( ReplaceAll : boolean );
var
  ReplaceCnt : integer;
  Original_Confirm : boolean;
  Original_EntireScope : boolean;
  SelectedTextToReplace: boolean;
  DoReplace: Boolean;
  txtMessage: string;
  handle: HWND;
  AppliedBeginUpdate: Boolean;


  function GetReplacementConfirmation: Boolean;
  begin
      Result := false;
      if ReplaceAll and FindOptions.ReplaceConfirm then
          // Note: With DoMessageBox I can't show All button
          case messagedlg( STR_01,
             mtConfirmation, [mbYes,mbNo,mbAll,mbCancel], 0 ) of
             mrYes:  Result := true;
             mrNo:   Result := false;
             mrAll:  begin
                     Result := true;
                     FindOptions.ReplaceConfirm := false;
                     end;
             mrCancel: begin
                     Result := false;
                     ReplaceAll := false; // will break out of loop
                     end;
          end

      else
          Result := true;
  end;

  function IdentifySelectedTextToReplace: Boolean;
  var
    WordAtCursor: string;
    SelectedTextLength: integer;
  begin
      SelectedTextLength:= ActiveNote.Editor.SelLength;

      Result:= (SelectedTextLength > 0);
      if Result then begin
         if FindOptions.WholeWordsOnly then begin
             WordAtCursor:= ActiveNote.Editor.GetWordAtCursor( false, true );
             if length(WordAtCursor) <> SelectedTextLength then
                Result:= False;
         end;
         if Result then
            if FindOptions.MatchCase then
               Result:= (ActiveNote.Editor.SelText = Text_To_Find)
            else
               Result:= AnsiSameText(ActiveNote.Editor.SelText, Text_To_Find);
         end;
  end;

  procedure BeginUpdateOnNotes;
  var
     i: integer;
     note: TTabNote;
  begin
     AppliedBeginUpdate:= True;
     for i := 0 to Form_Main.Pages.PageCount - 1 do
     begin
          note:= TTabNote(Form_Main.Pages.Pages[i].PrimaryObject);
          note.Editor.EndUpdate;
     end;
  end;

  procedure EndUpdateOnNotes;
  var
     i: integer;
     note: TTabNote;
  begin
     if not AppliedBeginUpdate then exit;
     
     for i := 0 to Form_Main.Pages.PageCount - 1 do
     begin
        note:= TTabNote(Form_Main.Pages.Pages[i].PrimaryObject);
        note.Editor.EndUpdate;
     end;
     AppliedBeginUpdate:= False;
  end;

begin
  if assigned( Form_FindReplace ) then begin
     FindOptions := Form_FindReplace.MyFindOptions;
     handle:= Form_FindReplace.Handle;
  end
  else
     handle:= 0;

  ReplaceCnt := 0;
  Text_To_Find := FindOptions.Pattern;
  Original_Confirm := FindOptions.ReplaceConfirm;
  Original_EntireScope := FindOptions.EntireScope;

  Is_Replacing := true;
  AppliedBeginUpdate:= False;

  try
    DoReplace:= True;

    // Verificamos si hay que restringir la búsqueda a la selección actual
    if FindOptions.FindNew then begin
        if ReplaceAll and FindOptions.SelectedText then begin
           FindOptions.SelectionStart:= ActiveNote.Editor.SelStart;
           FindOptions.SelectionEnd:= FindOptions.SelectionStart + ActiveNote.Editor.SelLength;
           FindOptions.EntireScope := False;
        end;
    end;

    // Comprobamos (si no se ha pulsado ReplaceAll) en primer lugar si el texto que se encuentra
    // seleccionado es el que hay que buscar y reemplazar. Si es así, haremos el reemplazo con éste,
    // directamente.
    SelectedTextToReplace:= False;
    if not ReplaceAll then
       SelectedTextToReplace:= IdentifySelectedTextToReplace;

    if not SelectedTextToReplace then begin
       SelectedTextToReplace:= RunFindNext(ReplaceAll);
       if not ReplaceAll then
          DoReplace:= False;   // Lo dejaremos seleccionado pero no lo reemplazaremos. El usuario no ha llegado
                               //a ver ese texto y debe confirmarlo pulsando conscientemente en Replace (el
                               // checkbox 'confirm replace' sólo se aplica a ReplaceAll)
    end;


    if DoReplace then begin
        if ReplaceAll and not FindOptions.ReplaceConfirm then
           BeginUpdateOnNotes;

        while SelectedTextToReplace do
        begin
            try
                // ¿Hay que restringirse al texto inicialmente seleccionado?
                if ReplaceAll and FindOptions.SelectedText then
                   if (ActiveNote.Editor.SelStart < FindOptions.SelectionStart) or
                     ((ActiveNote.Editor.SelStart + ActiveNote.Editor.SelLength) > FindOptions.SelectionEnd) then
                       break;

                if GetReplacementConfirmation then begin
                   inc(ReplaceCnt);
                   ActiveNote.Editor.SelText := FindOptions.ReplaceWith;
                   ActiveNote.Editor.SelStart := ActiveNote.Editor.SelStart + length( ActiveNote.Editor.SelText );
                end;

                Application.ProcessMessages;
                if UserBreak then break;

                SelectedTextToReplace:= RunFindNext(ReplaceAll);     // Localizamos el siguiente patrón a remplazar

                if (not ReplaceAll) then break;          // Dejamos simplemente localizado el texto si no ReplaceAll

            except
               On E : Exception do begin
                  showmessage( E.Message );
                  break;
               end;
            end;
        end; // while

    end;

  finally
    EndUpdateOnNotes;
    Is_Replacing := false;
    UserBreak := false;
    FindOptions.ReplaceConfirm := Original_Confirm;
    FindOptions.EntireScope := Original_EntireScope;
  end;

  txtMessage:= Format( STR_11, [ReplaceCnt] );
  Form_Main.StatusBar.Panels[PANEL_HINT].Text := txtMessage;
  if ( ReplaceCnt > 0 ) then begin
     if ReplaceAll then DoMessageBox(txtMessage, STR_12, 0, handle);
     NoteFile.Modified := true;
     UpdateNoteFileState( [fscModified] );
     end
  else
      if not SelectedTextToReplace then begin
         DoMessageBox(Format( STR_02, [Text_To_Find] ), STR_12, 0, handle);
      end;

end; // ReplaceEventProc


procedure Form_FindReplaceClosed( sender : TObject );
begin
  try
    try
      if SearchInProgress then
      begin
        UserBreak := true;
        SearchInProgress := false;
      end;
      FindOptions := Form_FindReplace.MyFindOptions;
      Form_FindReplace.Release;
      FreeAuxEditorControl;

    except
    end;
  finally
    Form_FindReplace := nil;
    Form_Main.FindNotify( true );
  end;
end; // Form_ReplaceClosed


Initialization
    Text_To_Find := '';
    Is_Replacing := false;
    Form_FindReplace := nil;
    SearchInProgress := false;
    UserBreak := false;

end.
