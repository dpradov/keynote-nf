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
   System.Math,
   Vcl.Dialogs,
   Vcl.Forms,
   Vcl.Controls,
   TreeNT,
   RxRichEd,
   kn_KntFolder,
   kn_FindReplace
   ;


var
    Text_To_Find : string;

    Form_FindReplace : TForm_FindReplace;  // GLOBAL FORM!

    Is_Replacing : boolean;     // Find and Replace functions share some procedures; this is how we tell the difference where necessary
    SearchInProgress : boolean; // TRUE while searching or replacing
    UserBreak : boolean;

    SearchNode_Text, SearchNode_TextPrev : string;
    StartFolder: TKntFolder;
    StartNode: TTreeNTNode;


procedure DoFindNext;
procedure RunFinder;
function RunFindNext (Is_ReplacingAll: Boolean= False): boolean;
procedure RunFindAllEx;
procedure RunReplace;
procedure RunReplaceNext;

procedure ClearFindAllResults;
procedure UpdateFindAllResultsWidth;
procedure FindResultsToEditor( const SelectedOnly : boolean );

procedure FindAllResults_SelectMatch (Prev: boolean);
procedure FindAllResults_OnSelectionChange(Editor: TRxRichEdit);
procedure FindAllResults_RightClick (CharIndex: integer);


implementation
uses
   System.DateUtils,
   gf_miscvcl,
   gf_strings,
   kn_const,
   kn_Info,
   kn_Global,
   kn_Cmd,
   kn_KntNote,
   kn_LocationObj,
   kn_EditorUtils,
   knt.ui.editor,
   kn_RTFUtils,
   kn_VCLControlsMng,
   kn_TreeNoteMng,
   kn_MacroMng,
   kn_NoteFileMng,
   kn_LinksMng,
   kn_Main,
   knt.App
   ;

var
   NumberFoundItems: integer;
   SearchOriginNew : integer;
   EditControl: TRxRichEdit;
   LastResultCellWidth: string;
   SelectedMatch: integer;          // < 0: In the process of identify the selectedMatch
   FollowMatch: boolean;
   LastFindNextAt: TDateTime;

   ReplacingLastNode: TTreeNTNode;                 // ReplaceLastSearchedNode ReplaceLastNode
   ReplacingLastNodeHasRegImg: Boolean;
   SearchingFolder: TKntFolder;
   SearchingInEditor: TKntRichEdit;


resourcestring
  STR_01 = 'Replace this occurrence?';
  STR_02 = 'Pattern not found: "%s"';
  STR_03 = 'Folder "%s" does not exist in this file.';
  STR_04 = 'Tree node "%s" does not exist in folder "%s".';
  STR_05 = 'Search results are not available.';
  STR_06 = 'Options';
  STR_07 = ' Searching - press ESC to abort.';
  STR_08 = 'An error occurred during search:';
  STR_09 = ' Pattern found at pos %d (%d occurrence(s))';
  STR_10 = ' Pattern not found.';
  STR_11 = ' Replaced %d occurrence(s)';
  STR_12 = 'Information';
  STR_13 = ' matches';


function RunFindNextInNotes (Is_ReplacingAll: Boolean= False): boolean; forward;
function RunFindNextInEditor (Is_ReplacingAll: Boolean= False): boolean; forward;

procedure FindEventProc( sender : TObject ); forward;
procedure ReplaceEventProc( ReplaceAll : boolean ); forward;
procedure Form_FindReplaceClosed( sender : TObject ); forward;



procedure RunFindReplace (modeReplace: boolean);
var
  Editor: TKntRichEdit;
begin
  if not App.CheckActiveEditor then exit;
  if ( FileIsBusy or SearchInProgress ) then exit;

  Editor:= ActiveEditor;

  if ( Editor.SelLength > 0 ) then
     FindOptions.Pattern := trim( Editor.SelVisibleText )
  else
     if FindOptions.WordAtCursor then
        FindOptions.Pattern := Editor.GetWordAtCursor( true );

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
  if not App.CheckActiveEditorNotReadOnly then exit;

  ReplaceEventProc(false);
end; // RunReplaceNext


procedure FindResultsToEditor( const SelectedOnly : boolean );
var
  i, cnt : integer;
  aLocation: TLocation;
  CaretInKNTLinksBAK: boolean;
  Editor: TKntRichEdit;
begin
  if ( not Form_Main.Pages_Res.Visible ) then exit;
  if not App.CheckActiveEditorNotReadOnly then exit;

  cnt := Location_List.Count;
  if ( cnt = 0 ) then
  begin
    showmessage( STR_05 );
    exit;
  end;

  Editor:= ActiveEditor;

  CaretInKNTLinksBAK:= TreeOptions.CaretInKNTLinks;
  TreeOptions.CaretInKNTLinks:= True;
  try
      if SelectedOnly then begin
        i := SelectedMatch;
        aLocation:= TLocation( TLocation( Location_List.Objects[pred( i )] ));
        InsertOrMarkKNTLink(aLocation, true, '');
      end
      else begin
        for i := 1 to cnt do begin
          aLocation:= TLocation( TLocation( Location_List.Objects[pred( i )] ));
          Editor.SelText := #13 + Format( '%d. ', [i] );
          Editor.SelStart:= Editor.SelStart + Editor.SelLength;
          InsertOrMarkKNTLink(aLocation, true, '');
        end;
      end;
      Editor.SelLength := 0;

  finally
    TreeOptions.CaretInKNTLinks:= CaretInKNTLinksBAK;
  end;

end; // FindResultsToEditor



function FindPattern (const Substr: string; const Str: String; SearchOrigin: integer; var SizeInternalHiddenText: integer; IgnoreKNTHiddenMarks: boolean = true): integer;
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

    if SearchOrigin >= LenStr then
       Exit(0);
    
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
                  if iT = 0 then begin
                     Dif:= True;
                     posIT := pH;
                     break;
                  end
                  else begin
                     SizeAux:= 0;
                     repeat
                         inc(iT);
                         inc(SizeInternalHiddenText);
                         inc(SizeAux);
                     until (PText[iT]=KNT_RTF_HIDDEN_MARK_R_CHAR) or (PText[iT]=#0) or (SizeAux > KNT_RTF_HIDDEN_MAX_LENGHT_CHAR);      // Looking for <StartHiddenText>.....<EndHiddenText>  (ex: HB5H  where  H=KNT_RTF_HIDDEN_MARK_CHAR)
                     if (SizeAux > KNT_RTF_HIDDEN_MAX_LENGHT_CHAR) then begin
                        Dif:= True;
                        pHf:= pH;
                     end
                     else begin
                       inc(iT);
                       inc(SizeInternalHiddenText);
                       if pHf = pH then
                          pHf:= pH + SizeAux;
                       if SizeInternalHiddenText = iT then
                          Dif:= True;
                     end;
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


function GetFindedPatternExtract (const Str: string;
                                  const Pattern: string;
                                  pPos: integer;          // zero-based
                                  var lastPos: integer;
                                  wordList : TStringList = nil;
                                  const LastPattern: string = ''; LastPatternPos: integer= -1; SecondPart: boolean= false): string;
var
  pL, pR, p, pGap, pStart, pMin: integer;
  extR: integer;
  len, i, iMin, gap: integer;
  bakFirstChar: char;
  strSearch: string;
  Word, scapeFirstChar: string;

begin
    if (Str='') then Exit('');

    inc(pPos);

    bakFirstChar:= Str[pPos];

    // Looking for line break to the left
    for pL := pPos downto Max(1, pPos-50) do
      if Str[pL] = #13 then
         break;
    if pL = 0 then pL:= 1;
    if Str[pL] = #13 then inc(pL);

    // Looking for line break to the right
    extR:= 50;
    if not SecondPart and (wordlist <> nil) then
       extR:= 75;

    pR:= Pos(#13, Str, pPos)-1;
    if pR < 0 then
       pR := Length(Str);
    pR:= Min(pR, pPos + extR);

    // *1  Mark the first appearance to highlight, there may be others before it, as a consequence 
    //     of the context recovered by the left ( #26; SUB Substitute )
    Result:= Copy(Str, pL, pR-pL+1);
    Result[pPos-pL+1]:= #26;                           // *1
    Result:= RemoveKNTHiddenCharactersInText(Result);
    Result:= ScapeSpecialRTFCharacters(Result);
    Result:= StringReplace(Result, #7, ' | ', [rfReplaceAll]);    // Replace column separator in Tables

    len:= length(ScapeSpecialRTFCharacters(Pattern));
    pPos:= Pos(#26, Result, 1);
    if pPos > 0  then begin                            // Should be > 0 ...
       Result[pPos]:= bakFirstChar;
       scapeFirstChar:= '';
       if bakFirstChar in ['\','{','}'] then
          scapeFirstChar:= '\';

       if not SecondPart  then begin
          insert('{\b\cf2 ' + scapeFirstChar, Result, pPos);
          insert('}', Result, pPos + len + 8);         // 8= length('{\b\cf2 ')       9: + '}'
          pStart:= pPos + len + 9;
       end
       else begin
          if scapeFirstChar <> '' then
             insert('\', Result, pPos);
          pStart:= 1;
       end;
    end;

    if wordlist <> nil then begin
        // Highlight occurrences of all the words provided, if they are found in the excerpt. In the second part of
        // the excerpt (if any), highlight only up to the last word found in the match.
        gap:= 0;
        if FindOptions.MatchCase then
           StrSearch:= Result
        else
           StrSearch:= AnsiUpperCase(Result);

        repeat
            pMin:= integer.MaxValue;

            for i := 0 to wordlist.Count-1 do begin
               Word:= ScapeSpecialRTFCharacters(wordlist[i]);
               p:= Pos(Word, StrSearch, pStart);
               if (p > 0) and (p < pMin)  then begin
                  if FindOptions.WholeWordsOnly then begin
                     if (p > 1) and IsCharAlphaNumeric(StrSearch[p-1]) then continue;
                     len:= length(Word);
                     if (p+len < StrSearch.Length) and IsCharAlphaNumeric(StrSearch[p+len]) then continue;
                  end;
                  pMin:= p;
                  iMin:= i;
               end;
            end;

            if pMin <> integer.MaxValue then begin
               if SecondPart and (pMin > pPos) then break;

               pGap:= pMin + gap;
               len:= length(ScapeSpecialRTFCharacters(wordlist[iMin]));
               insert('{\b\cf2 ', Result, pGap);
               insert('}', Result, pGap + Len + 8);
               pStart:= pMin + len;
               gap:= gap + 9;
            end

        until pMin = integer.MaxValue;
    end;

    if pR < LastPatternPos then                    // => only in smAll
       Result:= Result + '{\b\cf2  . . //. . }' +
                GetFindedPatternExtract(Str, LastPattern, LastPatternPos, lastPos, wordList, '',-1, true);

    lastPos:= pR;   // To use with smAny
end;




{
  New in 1.8.0:

- Excerpts from the note of the matches found are displayed, where the searched words are highlighted

- The treatment of the options 'All the words' and 'Any of the words' is extended :
  Now results are shown that include all the words considered, and there may be more than one result per node / note.

   - 'All the words': the first word found from the list provided is highlighted, as well
   as those others that appear within the excerpt, to its right. If the last word found
   within the identified match is not shown in the excerpt obtained for the first word
   found, an additional excerpt for the last word is added, where all searched words
   are highlighted.

   - 'Any of the words': Results are created that include, and highlight, all occurrences
   of each of the words in the search term. New results are only added for those words
   that are not visible within the previous excerpt.
}

procedure RunFindAllEx;
var
  oldAllNodes, oldEntireScope, oldWrap : boolean;
  FindDone : boolean;
  Location : TLocation;
  SearchOpts : TRichSearchTypes;
  noteidx, i, MatchCount, PatternPos, PatternPos1, PatternPosN, PatternLen, SearchOrigin : integer;
  myFolder : TKntFolder;
  myTreeNode : TTreeNTNode;
  myNote : TKntNote;
  LastFolderID, lastNoteGID : Cardinal;
  lastTag : integer;
  numNodosNoLimpiables: integer;
  thisWord : string;
  wordList : TStringList;
  wordcnt : integer;
  MultiMatchOK : boolean;
  ApplyFilter: boolean;
  nodeToFilter: boolean;
  nodesSelected: boolean;
  oldActiveFolder: TKntFolder;
  SearchModeToApply : TSearchMode;
  TreeNodeToSearchIn : TTreeNTNode;
  TreeNodeToSearchIn_AncestorPathLen: integer;
  TextPlain, TextPlainBAK: string;               // TextPlain = TextPlainBAK, in uppercase if not MatchCase
  TextToFind, PatternInPos1, PatternInPosN: string;
  SizeInternalHiddenText, SizeInternalHiddenTextInPos1: integer;
  str, s, path, strLocationMatch, strNodeFontSize, strNumberingFontSize, strBgColor: string;
  widthTwips: integer;
  RTFAux : TAuxRichEdit;

type
   TLocationType= (lsNormal, lsNodeName, lsMultimatch);

       function AddLocation (SearchingInNodeName: boolean; LocationType: TLocationType;
                             const FirstPattern: string; PatternPos: integer;
                             wordList : TStringList = nil;
                             const LastPattern: string = ''; LastPatternPos: integer= -1): integer;
       var
         str, strExtract : string;
       begin
          Location := TLocation.Create;
          //Location.FileName := KntFile.FileName;
          //Location.FolderName := myFolder.Name;
          Location.FolderID := myFolder.ID;

          if assigned(myTreeNode) then begin
             myNote := TKntNote(myTreeNode.Data);
             //Location.NoteName := myNote.Name;
             Location.NoteGID := myNote.GID;
          end;

          if not SearchingInNodeName then begin
             str:= TextPlainBAK;                                                      // original text, without case change
             Location.CaretPos := PatternPos;
             Location.SelLength := Length(FirstPattern) + SizeInternalHiddenTextInPos1;
          end
          else begin
             str:= myNote.Name;
             Location.SelLength := 0;
             Location.CaretPos := -1     // means: text found in node name
          end;

          if assigned( myTreeNode ) then begin
            if ApplyFilter and (not nodesSelected) then     // Only once per note
               MarkAllFiltered(myFolder);           // There will be at least one node in the selection
            nodeToFilter:= false;
            nodesSelected:= true;
          end;

          strExtract:= GetFindedPatternExtract(str, FirstPattern, PatternPos, Result, wordList, LastPattern, LastPatternPos);
          Location_List.AddObject(strExtract, Location);
       end;

       procedure GetCurrentNode;
       var
          path: string;
       begin
          myTreeNode := myFolder.TV.Selected;
          TreeNodeToSearchIn:= myTreeNode;

          if (TreeNodeToSearchIn <> nil) and TreeOptions.ShowFullPathSearch then begin
             path:= GetNodePath( TreeNodeToSearchIn, TreeOptions.NodeDelimiter, true );
             TreeNodeToSearchIn_AncestorPathLen:= Length(path) + Length(myFolder.Name) + 1 - Length(TreeNodeToSearchIn.Text);
          end;

       end;

       function EnsureCheckMode (myTreeNode: TTreeNTNode): TTreeNTNode;
       begin
          if assigned( myTreeNode ) then begin
             if (myTreeNode.CheckState = csChecked) and (FindOptions.CheckMode = scOnlyNonChecked) then
                myTreeNode := myTreeNode.GetNextNonChecked (FindOptions.HiddenNodes)
             else
             if (myTreeNode.CheckState = csUnchecked) and (FindOptions.CheckMode = scOnlyChecked) then
                myTreeNode := myTreeNode.GetNextChecked (FindOptions.HiddenNodes);
          end;
          Result:= myTreeNode;
       end;

       procedure GetFirstNode;
       begin
          myTreeNode := myFolder.TV.Items.GetFirstNode;

          if assigned( myTreeNode ) then begin
             if myTreeNode.Hidden and (not FindOptions.HiddenNodes) then
                myTreeNode := myTreeNode.GetNextNotHidden;

             myTreeNode:= EnsureCheckMode(myTreeNode);
          end;
       end;

       procedure GetNextNode;
       begin
          if (not assigned(myTreeNode)) then exit;

          if FindOptions.HiddenNodes then     // [dpv]
             myTreeNode := myTreeNode.GetNext
          else
             myTreeNode := myTreeNode.GetNextNotHidden;

          myTreeNode:= EnsureCheckMode(myTreeNode);

          if (TreeNodeToSearchIn <> nil) and (myTreeNode <> nil) and
               (myTreeNode.Level <= TreeNodeToSearchIn.Level) then begin

              myTreeNode := nil;
              exit;
          end;

       end;

       procedure GetNextNote;
       begin
          if FindOptions.AllTabs then begin
             inc( noteidx );

             if ( noteidx >= ActiveFile.FolderCount ) then
                FindDone := true
             else begin
                myFolder := TKntFolder(Form_Main.Pages.Pages[noteidx].PrimaryObject);
                GetFirstNode;
             end;
          end
          else
            FindDone := true;
       end;

       procedure FindPatternInText (SearchingInNodeName: boolean);
       var
          wordidx : integer;
       begin
          case SearchModeToApply of
              smPhrase :
                  begin
                      repeat
                         PatternPos:= FindPattern(TextToFind, TextPlain, SearchOrigin+1, SizeInternalHiddenTextInPos1) -1;
                         {
                         PatternPos := EditControl.FindText(
                           FindOptions.Pattern,
                           SearchOrigin, -1,
                           SearchOpts
                         );
                         }

                         if ( PatternPos >= 0 ) then begin
                             SearchOrigin := PatternPos + PatternLen; // move forward in text
                             AddLocation(SearchingInNodeName, lsNormal, TextToFind, PatternPos);
                         end;
                         Application.ProcessMessages;
                      until UserBreak or (PatternPos < 0);
                    end;

              smAny, smAll :
                begin
                   repeat
                       PatternPos1:= Integer.MaxValue;
                       PatternPosN:= -1;

                       MultiMatchOK := false;
                       for wordidx := 0 to wordcnt -1 do begin
                          thisWord := wordList[ wordidx ];

                          PatternPos:= FindPattern(thisWord, TextPlain, SearchOrigin + 1, SizeInternalHiddenText) -1;
                          {
                          PatternPos := EditControl.FindText(
                            thisWord,
                            0, -1,
                            SearchOpts
                          );
                          }

                          if ( PatternPos >= 0 ) then begin
                             MultiMatchOK := true; // assume success
                             if PatternPos < PatternPos1 then begin
                                PatternPos1:= PatternPos;
                                PatternInPos1:= thisWord;
                                SizeInternalHiddenTextInPos1:= SizeInternalHiddenText;
                             end;
                          end;

                          case SearchModeToApply of
                              smAll:
                                  if ( PatternPos >= 0 ) then begin
                                     if PatternPos > PatternPosN then begin
                                        PatternPosN:= PatternPos;
                                        PatternInPosN:= thisWord;
                                     end;
                                  end
                                  else begin
                                     MultiMatchOK := false;
                                     break; // note must have ALL words
                                  end;
                           end;

                          Application.ProcessMessages;
                          if UserBreak then begin
                             FindDone := true;
                             break;
                          end;

                       end; // for wordidx

                       if MultiMatchOK then begin
                          if SearchModeToApply = smAll then begin
                             AddLocation (SearchingInNodeName, lsMultimatch, PatternInPos1, PatternPos1, wordList, PatternInPosN, PatternPosN);
                             SearchOrigin := PatternPosN + 1; // move forward in text
                          end
                          else
                             SearchOrigin:= 1 + AddLocation (SearchingInNodeName, lsMultimatch, PatternInPos1, PatternPos1, wordList);
                       end;

                       Application.ProcessMessages;

                   until UserBreak or not MultiMatchOK;


                end; // smAny, smAll

          end; // case FindOptions.SearchMode

       end;

begin

  if ( not Form_Main.HaveKntFolders( true, true )) then exit;
  if ( not assigned( ActiveFolder )) then exit;
  if ( FileIsBusy or SearchInProgress ) then exit;

  if ( Form_Main.Combo_ResFind.Text = '' ) then exit;

  oldActiveFolder:= ActiveFolder; // [dpv] For use if ApplyFilter=true

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
    FindOptions.SearchScope := TSearchScope( RG_ResFind_Scope.ItemIndex );
    FindOptions.SearchMode := TSearchMode( RG_ResFind_Type.ItemIndex );
    FindOptions.CheckMode := TSearchCheckMode( RG_ResFind_ChkMode.ItemIndex );
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
  myNote := nil;
  TreeNodeToSearchIn:= nil;
  TreeNodeToSearchIn_AncestorPathLen:= 0;

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

  LastResultCellWidth:= '';
  SearchInProgress := true;
  screen.Cursor := crHourGlass;
  Form_Main.FindAllResults.ReadOnly:= False;
  Form_Main.FindAllResults.Clear;
  Application.ProcessMessages;
  Form_Main.FindAllResults.BeginUpdate;

  wordList := TStringList.Create;
  RTFAux:= CreateAuxRichEdit;


  try
    try
      ClearLocationList( Location_List );

      CSVTextToStrs( wordList, TextToFind, #32 );
      for i := wordList.count - 1 downto 0 do
         if wordList[i] = '' then wordList.delete(i);

      wordcnt := wordList.count;

      if wordcnt = 1 then begin
         SearchModeToApply := smPhrase;    // If not used smPhrase then the line number will not be shown, and will be confusing
         TextToFind:= wordList[0];         // '"Windows 10"' --> 'Windows 10'
      end;

      if wordcnt = 0 then begin
         Form_Main.Combo_ResFind.Text:= '';
         Form_Main.Btn_ResFind.Enabled:= False;
         exit;
      end;

      if FindOptions.AllTabs then
         myFolder := TKntFolder(Form_Main.Pages.Pages[noteidx].PrimaryObject) // initially 0
      else
         myFolder := ActiveFolder; // will exit after one loop

      if FindOptions.CurrentNodeAndSubtree then
         GetCurrentNode
      else begin
         GetFirstNode;
         while (not FindDone) and (not assigned(myTreeNode)) do
            GetNextNote();
      end;


      // Recorremos cada nota
      repeat
            nodesSelected:= false; // in this note, at the moment

            // Recorremos cada nodo (si es ntTree) o el único texto (si <> ntTree)
            repeat
                nodeToFilter:= true;               // Supongo que no se encontrará el patrón, con lo que se filtrará el nodo (si ApplyFilter=True)
                SearchOrigin := 0;                 // starting a new node

                if assigned(myTreeNode) and (FindOptions.SearchScope <> ssOnlyContent) then begin
                  myNote := TKntNote(myTreeNode.Data);
                  if FindOptions.MatchCase then
                     TextPlain:= myNote.Name
                  else
                     TextPlain:= AnsiUpperCase( myNote.Name);

                  FindPatternInText(true);
                end;

                if assigned(myTreeNode) and (FindOptions.SearchScope <> ssOnlyNodeName) then begin
                   TextPlainBAK:= myFolder.PrepareTextPlain(myTreeNode, RTFAux);
                   TextPlain:= TextPlainBAK;
                   if not FindOptions.MatchCase then
                      TextPlain:=  AnsiUpperCase(TextPlain);

                   SearchOrigin := 0;
                   FindPatternInText(false);
                end;

                if ApplyFilter and (assigned(myTreeNode) and (not nodeToFilter)) then
                   TKntNote(myTreeNode.Data).Filtered := false;

                GetNextNode;

            until UserBreak or not assigned(myTreeNode);


            if ApplyFilter and nodesSelected then begin
               Form_Main.FilterApplied(myFolder);

               ActiveFolder:= nil;      // -> TreeNodeSelected will exit doing nothing    ToDO: Not use ActiveFolder for this...
               HideFilteredNodes (myFolder);

               myTreeNode := myFolder.TV.Items.GetFirstNode;
               if myTreeNode.Hidden then myTreeNode := myTreeNode.GetNextNotHidden;
               myFolder.TV.Selected:= nil;
               myFolder.TV.Selected:= myTreeNode;   // force to select -> TreeNodeSelected
               ActiveFolder:= myFolder;

               KntFile.Modified := true;
               myTreeNode:= nil;
            end;

            while (not UserBreak) and ((not FindDone) and (not assigned(myTreeNode))) do
               GetNextNote();

      until FindDone or UserBreak;

      UpdateKntFileState( [fscModified] );

      MatchCount := Location_List.Count;
      Form_Main.LblFindAllNumResults.Caption:= MatchCount.ToString + STR_13;
      str:=
            '{\rtf1\ansi{\fonttbl{\f0\fnil\fcharset0 Calibri;}}' +
            '{\colortbl ;\red0\green159\blue159;\red255\green0\blue0;\red0\green125\blue125;\red255\green255\blue235;}' +
            '\pard\fs4\par' +
            '\pard\fs' + (2 * ResPanelOptions.FontSizeFindResults).ToString + ' ';

      strNodeFontSize:= (2 * (ResPanelOptions.FontSizeFindResults + 2)).ToString + ' ';
      strNumberingFontSize:= (2 * (ResPanelOptions.FontSizeFindResults - 3)).ToString + ' ';

      if ( MatchCount > 0 ) then begin
         widthTwips := DotsToTwips(Form_Main.FindAllResults.Width) - 500;
         LastResultCellWidth:= '\cellx' + widthTwips.ToString;

         LastFolderID := 0;
         lastNoteGID := 0;

         for i := 1 to MatchCount do begin
           Location := TLocation( Location_List.Objects[pred( i )] );
           if (( LastFolderID <> Location.FolderID ) or ( lastNoteGID <> Location.NoteGID )) then begin
               LastFolderID := Location.FolderID;
               lastNoteGID := Location.NoteGID;
               GetTreeNodeFromLocation(Location, myFolder, myTreeNode);

               Path:= '';
               if myTreeNode = nil then
                  strLocationMatch:= myFolder.Name
               else
                  strLocationMatch:= myTreeNode.Text;

               if (myTreeNode <> nil) and TreeOptions.ShowFullPathSearch then begin
                  Path:= PathOfKNTLink(myTreeNode.Parent, myFolder, 0, false, false, true);
                  if (TreeNodeToSearchIn_AncestorPathLen > 0) then
                     Delete(Path, 1, TreeNodeToSearchIn_AncestorPathLen);
                  if Path <> '' then
                     Path:= Path + TreeOptions.NodeDelimiter;
               end;
               s:= '160';
               if i = 1 then s:= '60';
               str:= str + Format('\pard\li80\sa60\sb%s \trowd\trgaph0%s \intbl {\cf1\b %s{\cf3\fs%s%s }}\cell\row \pard\li120\sb60\sa60 ',
                                 [s, LastResultCellWidth, Path, strNodeFontSize, strLocationMatch])
           end;
           s:= i.ToString;
           strBgColor:= '';
           if Location.CaretPos < 0 then   // text found in node name
              strBgColor:= '\clcbpat4';

           str:= str + Format('\trowd\trgaph0%s%s \intbl{\v\''11%s }{\b\fs%s %s.  }%s\cell\row ',
                 [strBgColor, LastResultCellWidth, s, strNumberingFontSize, s, Location_List[pred( i )]]);
         end;

         str:= str + '}';
         Form_Main.FindAllResults.PutRtfText(str, true, true);
         Form_Main.FindAllResults.SelStart:= 0;
      end;
      Form_Main.Btn_ResFlip.Caption := STR_06;
      Form_Main.Ntbk_ResFind.PageIndex := 0;

    except
      on E : Exception do
         messagedlg( E.Message, mtError, [mbOK], 0 );
    end;

  finally
    ActiveFolder:= oldActiveFolder;
    UpdateFolderDisplay;

    // restore previous FindOptions settings
    FindOptions.AllNodes := oldAllNodes;
    FindOptions.EntireScope := oldEntireScope;
    FindOptions.Wrap := oldWrap;
    FindOptions.FindAllMatches := false;
    SearchInProgress := false;
    screen.Cursor := crDefault;
    wordList.Free;
    RTFAux.Free;
    Form_Main.FindAllResults.EndUpdate;
    Form_Main.FindAllResults.ReadOnly:= True;
  end;

end; // RunFindAllEx


procedure ClearFindAllResults;
begin
  Form_Main.FindAllResults.ReadOnly:= False;
  Form_Main.FindAllResults.Clear;
  Form_Main.FindAllResults.PutRtfText('{\rtf1\ansi \pard\par}', true, false);
  Form_Main.FindAllResults.ReadOnly:= True;

  Form_Main.LblFindAllNumResults.Caption:= '';
  ClearLocationList( Location_List );
end;


procedure UpdateFindAllResultsWidth;
var
  widthTwips: integer;
  sRTF, cellWidth: string;

begin
   if LastResultCellWidth <> '' then begin
      widthTwips:= DotsToTwips(Form_Main.FindAllResults.Width) - 500;
      cellWidth:= '\cellx' + widthTwips.ToString;
      if LastResultCellWidth = cellWidth then exit;
      with Form_Main.FindAllResults do begin
          sRTF:= RtfText;
          sRTF:= StringReplace(sRTF, LastResultCellWidth, cellWidth, [rfReplaceAll]);
          ReadOnly:= False;
          Clear;
          PutRtfText(sRTF, true, false);
          ReadOnly:= True;
      end;
      LastResultCellWidth:= cellWidth;
   end;

end;


procedure FindAllResults_SelectedMatch (i: integer);
begin
  Form_Main.LblFindAllNumResults.Caption:= i.ToString + ' / ' + Location_List.Count.ToString + STR_13;
  SelectedMatch:= i;
end;


procedure FindAllResults_FollowMatch(i: integer);
var
  Location : TLocation;
begin
  Location := TLocation( Location_List.Objects[i-1] );
  if ( not assigned( Location )) then exit;

  JumpToLocation( Location );
end;


procedure FindAllResults_SelectMatch (Prev: boolean);
var
  pS, p, offset: integer;
  opt: TRichSearchTypes;
begin
  opt:= [];
  offset:= 5;
  if Prev then begin
     opt:= [stBackward];
     offset:= -7;
  end;

  with Form_Main.FindAllResults do begin
      pS:= SelStart;
      p:= FindText(KNT_RTF_HIDDEN_MARK_L_CHAR, pS + offset, -1, opt);
      if (p > 0) then
         SetSelection(p+5, p+5, true);
      end;
end;


procedure FindAllResults_RightClick (CharIndex: integer);
begin
  FollowMatch:= false;
  Form_Main.FindAllResults.SetSelection(CharIndex, CharIndex, true);
end;


procedure FindAllResults_OnSelectionChange(Editor: TRxRichEdit);
var
  pS, pLaux, pL, pR: integer;
  item: integer;
  s:string;
  matchSelected: boolean;
begin
  if SelectedMatch < 0 then exit;             // *1
//if Editor.SelLength > 0 then exit;          // *1

  SelectedMatch := -1;
  try
  

 { *1
   The statement Editor.SetSelection(pL, pR, true); will cause FindAllResults_OnSelectionChange to be called again.
   To avoid recursion, we could easily check Editor.SelLength and exit when the selection length is <> 0. This works fine in
   newer RichEdit controls (at least with versions available with W10 and W11), but in older versions, such as version 4 (on W7),
   it doesn't, probably due to a bug in that RichEdit version.
   After the call to Editor.SetSelection (with a selection > 0), when queried on reentry, Selength returns 0.
   So, for this to work also in that old version, the reentry control will be done with a variable, managed by
   this procedure: SelectedMatch. If < 0 => we are in the process of identifying the selected Match

   (See problem described here: https://github.com/dpradov/keynote-nf/issues/602#issuecomment-1704371949)
 }



  {
  #$D
  #$FFF9#$D<path 1>#7#$FFFB#$D
  #$FFF9#$D#$111 <match 1> #7#$FFFB#$D
  #$FFF9#$D#$112 <match 2> #7#$FFFB#$D
  ...
  #$FFF9#$D<path 13>#7#$FFFB#$D
  #$FFF9#$D#$1130 <match 30> #7#$FFFB#$D
  #$FFF9#$D#$1131 <match 31> #7#$FFFB#$D
  ...
  }


  pS:= Editor.SelStart;

  pLaux:= Editor.FindText(#$FFF9, pS+1, -1, [stBackward]) +1;
  pL:= Editor.FindText(KNT_RTF_HIDDEN_MARK_L_CHAR, pS+1, -1, [stBackward]) +1;
  matchSelected:= (pL > 0) and (pL > pLaux);
  Form_Main.FAMCopytoEditor.Enabled:= matchSelected;

  if matchSelected then begin
     s:= Editor.GetTextRange(pL, pL+6);
     pR:= pos(' ', s, 1);
     item:= StrToInt(Copy(s, 1, pR-1));
     pR:= Editor.FindText(#$FFFB, pS, -1, []) -1;
     Editor.SetSelection(pL, pR, true);

     FindAllResults_SelectedMatch (item);
     if FollowMatch then
        FindAllResults_FollowMatch(item)
     else
         matchSelected:= false;
     FollowMatch:= true;
  end
  else begin
     SelectedMatch:= 0;          // we are going to force reentrance, but controlled (FindAllResults_SelectMatch)
     if Editor.FindText(#$FFFB, pS, -1, []) < 0 then
        FindAllResults_SelectMatch (true)      // Prev
     else
        FindAllResults_SelectMatch (false);    // Next
  end;


 finally
    if SelectedMatch < 0 then
       SelectedMatch:= 0;
 end;
end;


procedure DoFindNext;
begin
   SearchingFolder:= nil;
   if (FindOptions.ResetNextAftN > 0) then begin
       if (incSecond(LastFindNextAt, FindOptions.ResetNextAftN) < now) then begin
          FindOptions.FindNew:= true;
          FindOptions.Pattern:= '';
       end;
       LastFindNextAt:= now;
   end;

   if ( FindOptions.Pattern = '' ) then
      RunFinder
   else
      RunFindNext;
end;


function RunFindNext (Is_ReplacingAll: Boolean= False): boolean;
begin
   if ActiveEditor = nil then exit(false);

   if ActiveEditor.NoteObj <> nil then
      Result:= RunFindNextInNotes (Is_ReplacingAll)
   else
      Result:= RunFindNextInEditor (Is_ReplacingAll);
end;


function RunFindNextInNotes (Is_ReplacingAll: Boolean= False): boolean;
var
  myFolder : TKntFolder;
  Editor: TKntRichEdit;
  myTreeNode : TTreeNTNode;
  FindDone, Found : boolean;
  PatternPos : integer;
  SearchOrigin : integer;
  SearchOpts : TRichSearchTypes;
  handle: HWND;

  l1, l2: integer;
  SizeInternalHiddenText: integer;
  TextPlain: string;
  RTFAux: TAuxRichEdit;

  function LoopCompleted(Wrap: boolean): Boolean;
  begin
      Result:= True;

      if not assigned(myFolder) then exit;

      if not (
         (myFolder = StartFolder)
          and (myTreeNode = StartNode)    ) then
         Result:= False
      else
          if Wrap and (NumberFoundItems > 0) then begin    // Wrap será siempre false si Is_ReplacingAll = True
             Result:= False;
             NumberFoundItems:= 0;
          end;
  end;


  procedure GetFirstNode();
  begin
      myTreeNode := myFolder.TV.Items.GetFirstNode;
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
     FindDone:= True;     // Supondremos inicialmente que no se podrá avanzar más

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

  procedure GetNextFolder();
  var
     tabidx : integer;
     wrap: boolean;
  begin
      FindDone:= True;   // Supondremos inicialmente que no se podrá avanzar más

      Wrap:= FindOptions.Wrap and not Is_ReplacingAll;

      if (FindOptions.AllTabs_FindReplace) and (not FindOptions.SelectedText) and (Form_Main.Pages.PageCount > 1) then begin
          tabidx := myFolder.TabSheet.PageIndex;
          if tabidx < pred(Form_Main.Pages.PageCount) then
             inc(tabidx)
          else
             tabidx := 0;

          myFolder := TKntFolder(Form_Main.Pages.Pages[tabidx].PrimaryObject);

          GetFirstNode;

          if not LoopCompleted(Wrap) then begin
             SearchOrigin := 0;
             FindDone:= False;
          end;
      end;
   end;


  procedure SelectPatternFound();
  begin
      if (myFolder <> ActiveFolder) then
         App.ActivateFolder(myFolder);

      if myFolder.TV.Selected <> myTreeNode then begin
         myTreeNode.MakeVisible;  // Could be hidden
         myFolder.TV.Selected := myTreeNode;
      end;

      SearchCaretPos (myFolder.Editor, myTreeNode, PatternPos, length( Text_To_Find) + SizeInternalHiddenText, true, Point(-1,-1), false, ReplacingLastNodeHasRegImg);
  end;

  procedure UpdateReplacingLastNodeHasRegImg;
  begin
      if Is_ReplacingAll and (myTreeNode <> ReplacingLastNode) then begin
         if not Editor.SupportsRegisteredImages then
            ReplacingLastNodeHasRegImg:= false
         else
            ReplacingLastNodeHasRegImg:= Editor.ContainsRegisteredImages;
         ReplacingLastNode:= myTreeNode;
      end;
  end;


begin
  result := false;
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

  RTFAux:= CreateAuxRichEdit;
  SearchInProgress := true;
  try
    try
      myFolder:= nil;
      if not FindOptions.FindNew then
         myFolder:= SearchingFolder;
      if not assigned(myFolder) then begin
         myFolder := ActiveFolder;
         SearchingFolder:= myFolder;
      end;
      if not assigned(myFolder) then exit;

      Editor:= myFolder.Editor;
      myTreeNode := myFolder.TV.Selected;

      UpdateReplacingLastNodeHasRegImg;

      // Identificación de la posición de inicio de la búsqueda ---------------------------
      if ( FindOptions.FindNew and FindOptions.EntireScope ) then
          SearchOrigin := 0

      else begin
          SearchOrigin := Editor.SelStart;
          if ReplacingLastNodeHasRegImg then
             SearchOrigin:= PositionInImLinkTextPlain (myFolder, myTreeNode, SearchOrigin);

          l1:= length(Editor.SelVisibleText);
          if l1 = length( Text_To_Find)  then begin
             l2:= length(Editor.SelText);
             if l1 = l2 then
                inc(SearchOrigin)
             else
                 SearchOrigin:= SearchOrigin + l2;
          end;
      end;

      if FindOptions.FindNew then begin
         StartFolder:= myFolder;
         StartNode:= myTreeNode;
         NumberFoundItems:= 0;
         FindOptions.FindNew := False;
      end;


      // Búsqueda del patrón iterando sobre los nodos / notas hasta encontrar uno -------------
      // Según las opciones establecidas así podrán recorrerse o no todos los nodos de una nota, todas las notas
      // o incluso continuar buscando desde el punto de partida, de manera cíclica.

      repeat
            UpdateReplacingLastNodeHasRegImg;

            if ReplacingLastNodeHasRegImg then
               TextPlain:= myFolder.PrepareTextPlain(myTreeNode, RTFAux)
            else
               TextPlain:= myFolder.GetTextPlainFromNode(myTreeNode, RTFAux);

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
               while (not FindDone) and (not assigned(myTreeNode)) do
                   GetNextFolder();
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
          if not Is_ReplacingAll then
             Form_Main.StatusBar.Panels[PANEL_HINT].text := Format(STR_09, [PatternPos, NumberFoundItems]);
          if IsRecordingMacro then
             AddMacroEditCommand( ecFindText );
      end
      else begin
          Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_10;
          if not (UserBreak or Is_Replacing) then
             DoMessageBox(Format( STR_02, [Text_To_Find] ), STR_12, 0, handle);
      end;

      SearchingFolder:= myFolder;
      SearchingInEditor:= myFolder.Editor;
      result := Found;
      SearchInProgress := false;
      UserBreak := false;
      RTFAux.Free;
  end;

end; // RunFindNext;


function RunFindNextInEditor (Is_ReplacingAll: Boolean= False): boolean;
var
  Editor: TKntRichEdit;
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
      if Wrap and (SearchOrigin < SearchOriginNew ) and (NumberFoundItems > 0) then begin    // Wrap será siempre false si Is_ReplacingAll = True
         Result:= False;
         NumberFoundItems:= 0;
      end;
  end;

  procedure SelectPatternFound();
  begin
     SearchCaretPos (Editor, nil, PatternPos, length( Text_To_Find) + SizeInternalHiddenText, true, Point(-1,-1), false);
  end;


begin
  result := false;
  if not App.CheckActiveEditor then exit;

  if ( SearchInProgress or FileIsBusy or ( Text_To_Find = '' )) then exit;

  if assigned( Form_FindReplace ) then
      handle:= Form_FindReplace.Handle
  else
      handle:= 0;

  FindOptions.FindAllMatches := false; // only TRUE when invoked from resource panel
  Found := false;

  PatternPos := -1;

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

      Editor:= ActiveEditor;
      SearchingInEditor:= Editor;
      TextPlain:= Editor.TextPlain();

      if ( FindOptions.FindNew and FindOptions.EntireScope ) then
          SearchOrigin := 0

      else begin
          SearchOrigin := Editor.SelStart;

          l1:= length(Editor.SelVisibleText);
          if l1 = length( Text_To_Find)  then begin
             l2:= length(Editor.SelText);
             if l1 = l2 then
                inc(SearchOrigin)
             else
                 SearchOrigin:= SearchOrigin + l2;
          end;
      end;

      if FindOptions.FindNew then begin
         NumberFoundItems:= 0;
         FindOptions.FindNew := False;
         SearchOriginNew:= SearchOrigin;
      end;

      repeat
         if FindOptions.MatchCase then
            PatternPos:= FindPattern(Text_To_Find, TextPlain, SearchOrigin+1, SizeInternalHiddenText) -1
         else
            PatternPos:= FindPattern(AnsiUpperCase(Text_To_Find), AnsiUpperCase(TextPlain), SearchOrigin+1, SizeInternalHiddenText) -1;

         if PatternPos < 0 then begin
            var Wrap: boolean := FindOptions.Wrap and not Is_ReplacingAll;
            FindDone:= True;
            if not (FindOptions.SelectedText or not Wrap or LoopCompleted(Wrap)) then begin
               FindDone:= False;
               SearchOrigin := 0;
            end;
         end;

      until FindDone or (PatternPos >= 0) or UserBreak;

      if ( PatternPos >= 0 ) then begin
          Found := true;
          FindOptions.FindNew := false;
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
          if not Is_ReplacingAll then
             App.ShowInfoInStatusBar(Format(STR_09, [PatternPos, NumberFoundItems]));
          if IsRecordingMacro then
             AddMacroEditCommand( ecFindText );
      end
      else begin
          App.ShowInfoInStatusBar(STR_10);
          if not Is_Replacing then
             DoMessageBox(Format( STR_02, [Text_To_Find] ), STR_12, 0, handle);
      end;

      result := Found;
      SearchInProgress := false;
  end;

end; // RunFindNextInEditor;



procedure FindEventProc( sender : TObject );
var
   autoClose: boolean;
   ResultFind: boolean;
begin
  if assigned( Form_FindReplace ) then begin
      FindOptions := Form_FindReplace.MyFindOptions;
      Text_To_Find := FindOptions.Pattern;
      autoClose:= FindOptions.AutoClose and not Form_FindReplace.ModeReplace;
      if ActiveEditor.NoteObj <> nil then
         ResultFind:= RunFindNext
      else
         ResultFind:= RunFindNextInEditor;

      if ResultFind then begin
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
  Editor: TKntRichEdit;


  procedure BeginUpdateOnFolders;
  var
     i: integer;
     folder: TKntFolder;
  begin
     AppliedBeginUpdate:= True;

     if Editor.NoteObj = nil then begin
        Editor.BeginUpdate;
        exit;
     end;

     for i := 0 to ActiveFile.Folders.Count-1 do begin
          folder:= ActiveFile.Folders[i];
          folder.Editor.BeginUpdate;
     end;
  end;

  procedure EndUpdateOnFolders;
  var
     i: integer;
     folder: TKntFolder;
  begin
     if not AppliedBeginUpdate then exit;

     if Editor.NoteObj = nil then begin
        Editor.EndUpdate;
        exit;
     end;

     for i := 0 to ActiveFile.Folders.Count-1 do begin
        folder:= ActiveFile.Folders[i];
        folder.Editor.EndUpdate;
     end;
     AppliedBeginUpdate:= False;
  end;

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
                     BeginUpdateOnFolders;
                     screen.Cursor := crHourGlass;
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
      SelectedTextLength:= Editor.SelLength;

      Result:= (SelectedTextLength > 0);
      if Result then begin
         if FindOptions.WholeWordsOnly then begin
             WordAtCursor:= Editor.GetWordAtCursor( false, true );
             if length(WordAtCursor) <> SelectedTextLength then
                Result:= False;
         end;
         if Result then
            if FindOptions.MatchCase then
               Result:= (Editor.SelText = Text_To_Find)
            else
               Result:= AnsiSameText(Editor.SelText, Text_To_Find);
         end;
  end;


begin
  if assigned( Form_FindReplace ) then begin
     FindOptions := Form_FindReplace.MyFindOptions;
     handle:= Form_FindReplace.Handle;
  end
  else
     handle:= 0;

  SearchingFolder:= ActiveFolder;
  SearchingInEditor:= ActiveEditor;
  Editor:= SearchingInEditor;

  ReplaceCnt := 0;
  Text_To_Find := FindOptions.Pattern;
  Original_Confirm := FindOptions.ReplaceConfirm;
  Original_EntireScope := FindOptions.EntireScope;

  Is_Replacing := true;
  AppliedBeginUpdate:= False;
  ReplacingLastNode:= nil;
  ReplacingLastNodeHasRegImg:= true;

  try
    DoReplace:= True;

    // Verificamos si hay que restringir la búsqueda a la selección actual
    if FindOptions.FindNew then begin
        if ReplaceAll and FindOptions.SelectedText then begin
           FindOptions.SelectionStart:= Editor.SelStart;
           FindOptions.SelectionEnd:= FindOptions.SelectionStart + Editor.SelLength;
           FindOptions.EntireScope := False;
        end;
    end;

    // Comprobamos (si no se ha pulsado ReplaceAll) en primer lugar si el texto que se encuentra
    // seleccionado es el que hay que buscar y reemplazar. Si es así, haremos el reemplazo con éste,
    // directamente.
    SelectedTextToReplace:= False;
    if not ReplaceAll then
       SelectedTextToReplace:= IdentifySelectedTextToReplace
    else
        if not FindOptions.ReplaceConfirm then begin
           BeginUpdateOnFolders;
           screen.Cursor := crHourGlass;
        end;


    if not SelectedTextToReplace then begin
       SelectedTextToReplace:= RunFindNext(ReplaceAll);
       Editor:= SearchingInEditor;
       if not ReplaceAll then
          DoReplace:= False;   // Lo dejaremos seleccionado pero no lo reemplazaremos. El usuario no ha llegado
                               //a ver ese texto y debe confirmarlo pulsando conscientemente en Replace (el
                               // checkbox 'confirm replace' sólo se aplica a ReplaceAll)
    end;


    if DoReplace then begin
        while SelectedTextToReplace do begin
            try
                // ¿Hay que restringirse al texto inicialmente seleccionado?
                if ReplaceAll and FindOptions.SelectedText then
                   if (Editor.SelStart < FindOptions.SelectionStart) or
                     ((Editor.SelStart + Editor.SelLength) > FindOptions.SelectionEnd) then
                       break;

                if GetReplacementConfirmation then begin
                   inc(ReplaceCnt);
                   Editor.AddText(FindOptions.ReplaceWith);
                   if Editor.NoteObj <> nil then
                      App.NoteModified(TKntNote(Editor.NoteObj), TKntFolder(Editor.FolderObj), false);   // false: don't update UI
                end;

                Application.ProcessMessages;
                if UserBreak then break;

                SelectedTextToReplace:= RunFindNext(ReplaceAll);     // Localizamos el siguiente patrón a remplazar
                Editor:= SearchingInEditor;

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
    screen.Cursor := crDefault;
    EndUpdateOnFolders;
    Is_Replacing := false;
    UserBreak := false;
    FindOptions.ReplaceConfirm := Original_Confirm;
    FindOptions.EntireScope := Original_EntireScope;
  end;

  txtMessage:= Format( STR_11, [ReplaceCnt] );
  Form_Main.StatusBar.Panels[PANEL_HINT].Text := txtMessage;
  if ( ReplaceCnt > 0 ) then begin
     if ReplaceAll then begin
        Editor.SelLength:= 0;
        DoMessageBox(txtMessage, STR_12, 0, handle);
     end;
     App.ActivateFolder(ActiveFolder);
     UpdateKntFileState( [fscModified] );
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
    LastResultCellWidth:= '';
    FollowMatch:= true;
    SelectedMatch:= 0;
    LastFindNextAt := 0;

end.
