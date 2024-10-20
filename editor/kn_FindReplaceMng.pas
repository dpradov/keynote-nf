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
   VirtualTrees,
   knt.ui.tree,
   RxRichEd,
   kn_Info,
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
    StartNode: PVirtualNode;


procedure DoFindNext;
procedure RunFinder;
function RunFindNext (Is_ReplacingAll: Boolean= False): boolean;
function RunFindAllEx (myFindOptions : TFindOptions; ApplyFilter, TreeFilter: Boolean): boolean;
procedure PreprocessTextPattern (var myFindOptions : TFindOptions);
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
   kn_Global,
   kn_Cmd,
   knt.model.note,
   kn_LocationObj,
   kn_EditorUtils,
   knt.ui.editor,
   kn_RTFUtils,
   kn_VCLControlsMng,
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

   ReplacingLastNode: PVirtualNode;                 // ReplaceLastSearchedNode ReplaceLastNode
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
  if ( ActiveFileIsBusy or SearchInProgress ) then exit;

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

end;


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
end;


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

  CaretInKNTLinksBAK:= KntTreeOptions.CaretInKNTLinks;
  KntTreeOptions.CaretInKNTLinks:= True;
  try
      if SelectedOnly then begin
        i := SelectedMatch;
        aLocation:= Location_List[pred(i)];
        InsertOrMarkKNTLink(aLocation, true, '');
      end
      else begin
        for i := 1 to cnt do begin
          aLocation:= Location_List[pred(i)];
          Editor.SelText := #13 + Format( '%d. ', [i] );
          Editor.SelStart:= Editor.SelStart + Editor.SelLength;
          InsertOrMarkKNTLink(aLocation, true, '');
        end;
      end;
      Editor.SelLength := 0;

  finally
    KntTreeOptions.CaretInKNTLinks:= CaretInKNTLinksBAK;
  end;

end;



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



procedure PreprocessTextPattern (var myFindOptions : TFindOptions);
var
  pF: integer;
  NumDaysBack: integer;
begin
  // Trying to identify '-<number>'  in the beginning Eg: '-3' '-7 search term'
  // If current day is '15/10/2024' then:
  //   '-2' is equivalent to [LastModified] >= '13/10/2024'
  //   '-0' is equivalent to [LastModified] >= '15/10/2024'  (modified on current day)
  // If enclosed in "" it will managed as a normal text pattern:
  //  '"-2"'

  if (myFindOptions.Pattern.Length < 2) or (myFindOptions.Pattern[1] <> '-') then exit;

  pF:= Pos(' ', myFindOptions.Pattern, 2);
  if pF <= 0 then pF:= 9999;

  NumDaysBack:= StrToIntDef(Copy(myFindOptions.Pattern, 2, pF-2), -1);
  if NumDaysBack >= 0 then begin
      myFindOptions.LastModifFrom := Today() -NumDaysBack;
      Delete(myFindOptions.Pattern, 1, pF-1);
      myFindOptions.Pattern:= trim(myFindOptions.Pattern);
  end;
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

function RunFindAllEx (myFindOptions : TFindOptions; ApplyFilter, TreeFilter: Boolean): boolean;
var
  FindDone : boolean;
  Location : TLocation;
  SearchOpts : TRichSearchTypes;
  noteidx, i, MatchCount, PatternPos, PatternPos1, PatternPosN, PatternLen, SearchOrigin : integer;
  myFolder : TKntFolder;
  myTreeNode : PVirtualNode;
  myNNode : TNoteNode;
  LastFolderID, lastNoteGID : Cardinal;
  lastTag : integer;
  numNodosNoLimpiables: integer;
  thisWord : string;
  wordList : TStringList;
  wordcnt : integer;
  MultiMatchOK : boolean;
  nodeToFilter: boolean;
  nodesSelected: boolean;
  SearchModeToApply : TSearchMode;
  TreeNodeToSearchIn : PVirtualNode;
  TreeNodeToSearchIn_AncestorPathLen: integer;
  TextPlain, TextPlainBAK: string;               // TextPlain = TextPlainBAK, in uppercase if not MatchCase
  TextToFind, PatternInPos1, PatternInPosN: string;
  SizeInternalHiddenText, SizeInternalHiddenTextInPos1: integer;
  str, s, path, strLocationMatch, strNodeFontSize, strNumberingFontSize, strBgColor: string;
  NodeNameInSearch: string;
  widthTwips: integer;
  RTFAux : TAuxRichEdit;
  TreeUI: TKntTreeUI;
  TV: TVTree;
  SearchingByDates: boolean;

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
          Location.FolderID := myFolder.ID;
          Location.Folder:= myFolder;

          if assigned(myTreeNode) then begin
             myNNode:= TreeUI.GetNNode(myTreeNode);
             Location.NNodeGID := myNNode.GID;
             Location.NNode:= myNNode;

             nodesSelected:= true;
             nodeToFilter:= false;
          end;
          Location.NEntry:= myNNode.Note.Entries[0];       // %%%
          Location.Calculated:= True;

          if not SearchingInNodeName then begin
             str:= TextPlainBAK;                                                      // original text, without case change
             Location.CaretPos := PatternPos;
             Location.SelLength := Length(FirstPattern) + SizeInternalHiddenTextInPos1;
          end
          else begin
             //str:= myNNode.NoteName;
             str:= NodeNameInSearch;
             Location.SelLength := 0;
             Location.CaretPos := -1     // means: text found in node name
          end;

          strExtract:= GetFindedPatternExtract(str, FirstPattern, PatternPos, Result, wordList, LastPattern, LastPatternPos); // ** Result is set here
          Location.Params:= strExtract;
          if not TreeFilter then
             Location_List.Add(Location);
       end;

       procedure GetCurrentNode;
       var
          path: string;
       begin
          myTreeNode := TreeUI.FocusedNode;
          TreeNodeToSearchIn:= myTreeNode;

          if (TreeNodeToSearchIn <> nil) and KntTreeOptions.ShowFullPathSearch then begin
             path:= TreeUI.GetNodePath( TreeNodeToSearchIn, KntTreeOptions.NodeDelimiter, true );
             TreeNodeToSearchIn_AncestorPathLen:= Length(path) + Length(myFolder.Name) + 1 - Length(TreeUI.GetNNode(TreeNodeToSearchIn).NoteName);
          end;

       end;

       function EnsureCheckMode (myTreeNode: PVirtualNode): PVirtualNode;
       begin
          if assigned( myTreeNode ) then begin
             if (myTreeNode.CheckState = csCheckedNormal) and (myFindOptions.CheckMode = scOnlyNonChecked) then
                myTreeNode := TV.GetNextNotChecked(myTreeNode, myFindOptions.HiddenNodes)
             else
             if (myTreeNode.CheckState <> csCheckedNormal) and (myFindOptions.CheckMode = scOnlyChecked) then
                myTreeNode := TV.GetNextChecked(myTreeNode, myFindOptions.HiddenNodes)
          end;
          Result:= myTreeNode;
       end;

       procedure GetFirstNode;
       begin
          myTreeNode := TV.GetFirst;

          if assigned( myTreeNode ) then begin
             if not TV.IsVisible[myTreeNode] and (not myFindOptions.HiddenNodes) then
               myTreeNode := TV.GetNextNotHidden(myTreeNode, TreeFilter);

             myTreeNode:= EnsureCheckMode(myTreeNode);
          end;
       end;

       procedure GetNextNode;
       begin
          if (not assigned(myTreeNode)) then exit;

          if myFindOptions.HiddenNodes then     // [dpv]
             myTreeNode := TV.GetNext(myTreeNode)
          else
             myTreeNode := TV.GetNextNotHidden(myTreeNode, TreeFilter);

          myTreeNode:= EnsureCheckMode(myTreeNode);

          if (TreeNodeToSearchIn <> nil) and (myTreeNode <> nil) and
               (TV.GetNodeLevel(myTreeNode) <= TV.GetNodeLevel(TreeNodeToSearchIn)) then begin

              myTreeNode := nil;
              exit;
          end;

       end;

       procedure GetNextFolder;
       begin
          if myFindOptions.AllTabs then begin
             inc( noteidx );

             if ( noteidx >= ActiveFile.FolderCount ) then
                FindDone := true
             else begin
                myFolder := TKntFolder(Form_Main.Pages.Pages[noteidx].PrimaryObject);
                TreeUI:= myFolder.TreeUI;
                TV:= TreeUI.TV;

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
                           myFindOptions.Pattern,
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

          end; // case myFindOptions.SearchMode

       end;

begin
  Result:= false;
  if ( not Form_Main.HaveKntFolders( true, true )) then exit;
  if ( not assigned( ActiveFolder )) then exit;
  if ( ActiveFileIsBusy or SearchInProgress ) then exit;

  myFindOptions.Pattern := trim( myFindOptions.Pattern ); // leading and trailing blanks need to be stripped
  PreprocessTextPattern(myFindOptions);

  with myFindOptions do
     SearchingByDates:= (LastModifFrom <> 0) or (LastModifUntil <> 0) or (CreatedFrom <> 0) or (CreatedUntil <> 0);

  if (myFindOptions.Pattern = '') and not SearchingByDates then exit;

  UserBreak := false;
  Form_Main.CloseNonModalDialogs;

  if not TreeFilter then begin
     FindOptions.MatchCase:= myFindOptions.MatchCase;
     FindOptions.WholeWordsOnly:= myFindOptions.WholeWordsOnly;
     FindOptions.AllTabs:= myFindOptions.AllTabs;
     FindOptions.CurrentNodeAndSubtree:= myFindOptions.CurrentNodeAndSubtree;
     FindOptions.SearchScope:= myFindOptions.SearchScope;
     FindOptions.SearchMode:= myFindOptions.SearchMode;
     FindOptions.CheckMode:= myFindOptions.CheckMode;
     FindOptions.HiddenNodes:= myFindOptions.HiddenNodes;
     FindOptions.SearchPathInNodeNames:= myFindOptions.SearchPathInNodeNames;

     FindOptions.FindAllMatches := false;
     FindOptions.FindNew := true;
  end;


  SearchModeToApply := myFindOptions.SearchMode;

  myTreeNode := nil;
  myNNode := nil;
  TreeNodeToSearchIn:= nil;
  TreeNodeToSearchIn_AncestorPathLen:= 0;


  if myFindOptions.WholeWordsOnly then
    SearchOpts := [stWholeWord]
  else
    SearchOpts := [];
  if myFindOptions.MatchCase then
    SearchOpts := SearchOpts + [stMatchCase];

  if myFindOptions.MatchCase then
     TextToFind:= myFindOptions.Pattern
  else
     TextToFind:= AnsiUpperCase(myFindOptions.Pattern);

  PatternPos := 0;
  PatternLen := length( myFindOptions.Pattern );
  MatchCount := 0;
  FindDone := false;
  noteidx := 0;

  LastResultCellWidth:= '';
  SearchInProgress := true;
  screen.Cursor := crHourGlass;
  if not TreeFilter  then begin
     Form_Main.FindAllResults.ReadOnly:= False;
     Form_Main.FindAllResults.Clear;
  end;
  Application.ProcessMessages;
  Form_Main.FindAllResults.BeginUpdate;

  wordList := TStringList.Create;
  RTFAux:= CreateAuxRichEdit;


  try
    try
      if not TreeFilter  then
         ClearLocationList( Location_List );

      CSVTextToStrs( wordList, TextToFind, #32 );
      for i := wordList.count - 1 downto 0 do
         if wordList[i] = '' then wordList.delete(i);

      wordcnt := wordList.count;

      if wordcnt = 1 then begin
         SearchModeToApply := smPhrase;    // If not used smPhrase then the line number will not be shown, and will be confusing
         TextToFind:= wordList[0];         // '"Windows 10"' --> 'Windows 10'
      end;

      if (wordcnt = 0) and not SearchingByDates then begin
         Form_Main.Combo_ResFind.Text:= '';
         Form_Main.Btn_ResFind.Enabled:= False;
         exit;
      end;

      if myFindOptions.AllTabs then
         myFolder := TKntFolder(Form_Main.Pages.Pages[noteidx].PrimaryObject) // initially 0
      else
         myFolder := ActiveFolder; // will exit after one loop

      TreeUI:= myFolder.TreeUI;
      TV:= TreeUI.TV;

      if myFindOptions.CurrentNodeAndSubtree then
         GetCurrentNode
      else begin
         GetFirstNode;
         while (not FindDone) and (not assigned(myTreeNode)) do
            GetNextFolder();
      end;


      // Go through each Folder
      repeat
            nodesSelected:= false; // in this Folder, at the moment
            if ApplyFilter then begin
               if TreeFilter then
                  TreeUI.ClearAllTreeMatch
               else
                  TreeUI.ClearAllFindMatch;
            end;

            // Go through each Note Node (NNode)
            repeat
                nodeToFilter:= true;               // Supongo que no se encontrar� el patr�n, con lo que se filtrar� el nodo (si ApplyFilter=True)
                SearchOrigin := 0;                 // starting a new node

                if assigned(myTreeNode) then
                   myNNode := TreeUI.GetNNode(myTreeNode)
                else
                   break;

                // TODO: Consider the dates of the Entries of the note
                if (not SearchingByDates or (myNNode.Note.LastModified <> 0) ) and
                   (myNNode.Note.LastModified.GetDate  >= myFindOptions.LastModifFrom) and
                   ((myFindOptions.LastModifUntil = 0) or (myNNode.Note.LastModified.GetDate <= myFindOptions.LastModifUntil)) and
                   (myNNode.Note.DateCreated.GetDate  >= myFindOptions.CreatedFrom) and
                   ((myFindOptions.CreatedUntil = 0) or (myNNode.Note.DateCreated.GetDate <= myFindOptions.CreatedUntil))
                then begin

                   if TextToFind = '' then
                      AddLocation(true, lsNormal, TextToFind, 0)      // => nodeToFilter = False

                   else begin

                      if (myFindOptions.SearchScope <> ssOnlyContent) then begin
                        if myFindOptions.SearchPathInNodeNames then
                           TextPlain:= myFolder.TreeUI.GetNodePath( myTreeNode, KntTreeOptions.NodeDelimiter, true )
                        else
                           TextPlain:= myNNode.NoteName;

                        NodeNameInSearch:= TextPlain;
                        if not myFindOptions.MatchCase then
                           TextPlain:= AnsiUpperCase( TextPlain);

                        FindPatternInText(true);
                      end;

                      if (myFindOptions.SearchScope <> ssOnlyNodeName) then begin
                         TextPlainBAK:= myFolder.PrepareTextPlain(myNNode, RTFAux);
                         TextPlain:= TextPlainBAK;
                         if not myFindOptions.MatchCase then
                            TextPlain:=  AnsiUpperCase(TextPlain);

                         SearchOrigin := 0;
                         FindPatternInText(false);
                      end;
                   end;

                   if ApplyFilter and (not nodeToFilter) then begin
                      if TreeFilter then
                         myNNode.TreeFilterMatch := True
                      else
                         myNNode.FindFilterMatch := True;

                      if not myFolder.ReadOnly then
                         myFolder.Modified:= True;    // Filter matches won't be saved in read only folders
                   end;

                end;

                GetNextNode;

            until UserBreak or not assigned(myTreeNode);


            if ApplyFilter then begin
               if TreeFilter then
                  TreeUI.TreeFilterApplied:= true
               else begin
                  TreeUI.FindFilterApplied:= true;
                  Form_Main.ShowTBTree(true);
               end;
               TreeUI.ActivateFilter;

               myTreeNode:= nil;
            end;

            while (not UserBreak) and ((not FindDone) and (not assigned(myTreeNode))) do
               GetNextFolder();

      until FindDone or UserBreak;


      if not TreeFilter  then begin
         MatchCount := Location_List.Count;
         Form_Main.LblFindAllNumResults.Caption:= '  ' + MatchCount.ToString + STR_13;
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
              Location := Location_List[pred(i)];
              if (( LastFolderID <> Location.FolderID ) or ( lastNoteGID <> Location.NNodeGID )) then begin
                  LastFolderID := Location.FolderID;
                  lastNoteGID := Location.NNodeGID;
                  myFolder:= Location.Folder;
                  myTreeNode:= Location.Node;

                  Path:= '';
                  if Location.NNode = nil then
                     strLocationMatch:= myFolder.Name
                  else
                     strLocationMatch:= Location.NNode.NoteName;

                  if (myTreeNode <> nil) and KntTreeOptions.ShowFullPathSearch then begin
                     Path:= PathOfKntLink(myTreeNode.Parent, myFolder, 0, false, false, true);
                     if (TreeNodeToSearchIn_AncestorPathLen > 0) then
                        Delete(Path, 1, TreeNodeToSearchIn_AncestorPathLen);
                     if Path <> '' then
                        Path:= Path + KntTreeOptions.NodeDelimiter;
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
                    [strBgColor, LastResultCellWidth, s, strNumberingFontSize, s, Location_List[pred(i)].Params]);
            end;

            str:= str + '}';
            Form_Main.FindAllResults.PutRtfText(str, true, true);
            Form_Main.FindAllResults.SelStart:= 0;
         end;
         Form_Main.Btn_ResFlip.Caption := STR_06;
         Form_Main.Ntbk_ResFind.PageIndex := 0;
      end;

      Result:= true;

    except
      on E : Exception do
         messagedlg( E.Message, mtError, [mbOK], 0 );
    end;

  finally
    Form_Main.UpdateFolderDisplay;

    SearchInProgress := false;
    screen.Cursor := crDefault;
    wordList.Free;
    RTFAux.Free;
    Form_Main.FindAllResults.EndUpdate;
    Form_Main.FindAllResults.ReadOnly:= True;
  end;

end;


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
  Form_Main.LblFindAllNumResults.Caption:= '  ' + i.ToString + ' / ' + Location_List.Count.ToString + STR_13;
  SelectedMatch:= i;
end;


procedure FindAllResults_FollowMatch(i: integer);
var
  Location : TLocation;
begin
  Location := Location_List[i-1];
  if ( not assigned( Location )) then exit;

  JumpToLocation( Location, true,true,urlOpen,false,  true );
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

   if ActiveEditor.NNodeObj <> nil then
      Result:= RunFindNextInNotes (Is_ReplacingAll)
   else
      Result:= RunFindNextInEditor (Is_ReplacingAll);
end;


function RunFindNextInNotes (Is_ReplacingAll: Boolean= False): boolean;
var
  myFolder : TKntFolder;
  Editor: TKntRichEdit;
  myTreeNode : PVirtualNode;
  FindDone, Found : boolean;
  PatternPos : integer;
  SearchOrigin : integer;
  SearchOpts : TRichSearchTypes;
  handle: HWND;

  l1, l2: integer;
  SizeInternalHiddenText: integer;
  TextPlain: string;
  RTFAux: TAuxRichEdit;
  TreeUI: TKntTreeUI;
  TV: TVTree;
  NNode: TNoteNode;

  function LoopCompleted(Wrap: boolean): Boolean;
  begin
      Result:= True;

      if not assigned(myFolder) then exit;

      if not (
         (myFolder = StartFolder)
          and (myTreeNode = StartNode)    ) then
         Result:= False
      else
          if Wrap and (NumberFoundItems > 0) then begin    // Wrap ser� siempre false si Is_ReplacingAll = True
             Result:= False;
             NumberFoundItems:= 0;
          end;
  end;


  procedure GetFirstNode();
  begin
      myTreeNode := myFolder.TreeUI.GetFirstNode;
      if assigned(myTreeNode) and not TV.IsVisible[myTreeNode] and (not FindOptions.HiddenNodes) then
         myTreeNode := TV.GetNextNotHidden(myTreeNode);
  end;


  // Actualiza el siguiente nodo a utilizar, que podr� ser nil si no se puede avanzar hacia
  // ning�n nodo de la nota actual.
  // Tambi�n puede establecer FindDone a True indicando que la b�squeda ha finalizado.
  // Si FindDone = False y el nodo = nil implicar� que debe continuarse con la siguiente nota.
  //
  procedure GetNextNode();
  var
     Wrap: boolean;
  begin
     FindDone:= True;     // Supondremos inicialmente que no se podr� avanzar m�s

     if not assigned(myTreeNode) or FindOptions.SelectedText then exit;

     Wrap:= FindOptions.Wrap and not Is_ReplacingAll;
     if not (FindOptions.AllTabs_FindReplace or FindOptions.AllNodes or Wrap) then exit;

     if FindOptions.AllTabs_FindReplace or FindOptions.AllNodes then begin
         if FindOptions.HiddenNodes then
            myTreeNode := TV.GetNext(myTreeNode)
         else
            myTreeNode := TV.GetNextNotHidden(myTreeNode);

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
      FindDone:= True;   // Supondremos inicialmente que no se podr� avanzar m�s

      Wrap:= FindOptions.Wrap and not Is_ReplacingAll;

      if (FindOptions.AllTabs_FindReplace) and (not FindOptions.SelectedText) and (Form_Main.Pages.PageCount > 1) then begin
          tabidx := myFolder.TabSheet.PageIndex;
          if tabidx < pred(Form_Main.Pages.PageCount) then
             inc(tabidx)
          else
             tabidx := 0;

          myFolder := TKntFolder(Form_Main.Pages.Pages[tabidx].PrimaryObject);
          TreeUI:= myFolder.TreeUI;
          TV:= TreeUI.TV;

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

      if myFolder.TreeUI.FocusedNode <> myTreeNode then begin
         myFolder.TreeUI.MakePathVisible(myTreeNode);   // Could be hidden
         myFolder.TreeUI.SelectAlone(myTreeNode);
      end;

      SearchCaretPos (myFolder.Editor, PatternPos, length( Text_To_Find) + SizeInternalHiddenText, true, Point(-1,-1),
                      false, ReplacingLastNodeHasRegImg, true);
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
  if ( SearchInProgress or ActiveFileIsBusy or ( Text_To_Find = '' )) then exit;


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

      TreeUI:= myFolder.TreeUI;
      TV:= TreeUI.TV;

      Editor:= myFolder.Editor;
      myTreeNode := TreeUI.FocusedNode;
      NNode:= TreeUI.GetNNode(myTreeNode);

      UpdateReplacingLastNodeHasRegImg;

      // Identificaci�n de la posici�n de inicio de la b�squeda ---------------------------
      if ( FindOptions.FindNew and FindOptions.EntireScope ) then
          SearchOrigin := 0

      else begin
          SearchOrigin := Editor.SelStart;
          if ReplacingLastNodeHasRegImg then
             SearchOrigin:= PositionInImLinkTextPlain (myFolder, NNode, SearchOrigin);

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


      // B�squeda del patr�n iterando sobre los nodos / notas hasta encontrar uno -------------
      // Seg�n las opciones establecidas as� podr�n recorrerse o no todos los nodos de una nota, todas las notas
      // o incluso continuar buscando desde el punto de partida, de manera c�clica.

      repeat
            UpdateReplacingLastNodeHasRegImg;

            if ReplacingLastNodeHasRegImg then
               TextPlain:= myFolder.PrepareTextPlain(NNode, RTFAux)
            else
               TextPlain:= myFolder.GetTextPlainFromNode(NNode, RTFAux);

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
               GetNextNode;                 // Podr� actualizar FindDone
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
          App.PopupMessage( STR_08 +#13+ E.Message, mtError, [mbOK] );
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
             App.InfoPopup(Format( STR_02, [Text_To_Find] ));
      end;

      SearchingFolder:= myFolder;
      SearchingInEditor:= myFolder.Editor;
      result := Found;
      SearchInProgress := false;
      UserBreak := false;
      RTFAux.Free;
  end;

end;


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
      if Wrap and (SearchOrigin < SearchOriginNew ) and (NumberFoundItems > 0) then begin    // Wrap ser� siempre false si Is_ReplacingAll = True
         Result:= False;
         NumberFoundItems:= 0;
      end;
  end;

  procedure SelectPatternFound();
  begin
     SearchCaretPos (Editor, PatternPos, length( Text_To_Find) + SizeInternalHiddenText, true, Point(-1,-1), false);
  end;


begin
  result := false;
  if not App.CheckActiveEditor then exit;

  if ( SearchInProgress or ActiveFileIsBusy or ( Text_To_Find = '' )) then exit;

  if assigned( Form_FindReplace ) then
      handle:= Form_FindReplace.Handle
  else
      handle:= 0;

  FindOptions.FindAllMatches := false; // only TRUE when invoked from resource panel
  Found := false;
  UserBreak := false;

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
          App.PopupMessage( STR_08 +#13+ E.Message, mtError, [mbOK] );
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

end;



procedure FindEventProc( sender : TObject );
var
   autoClose: boolean;
   ResultFind: boolean;
begin
  if assigned( Form_FindReplace ) then begin
      FindOptions := Form_FindReplace.MyFindOptions;
      Text_To_Find := FindOptions.Pattern;
      autoClose:= FindOptions.AutoClose and not Form_FindReplace.ModeReplace;
      if ActiveEditor.NNodeObj <> nil then
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

     if Editor.NNodeObj = nil then begin
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

     if Editor.NNodeObj = nil then begin
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

    // Verificamos si hay que restringir la b�squeda a la selecci�n actual
    if FindOptions.FindNew then begin
        if ReplaceAll and FindOptions.SelectedText then begin
           FindOptions.SelectionStart:= Editor.SelStart;
           FindOptions.SelectionEnd:= FindOptions.SelectionStart + Editor.SelLength;
           FindOptions.EntireScope := False;
        end;
    end;

    // Comprobamos (si no se ha pulsado ReplaceAll) en primer lugar si el texto que se encuentra
    // seleccionado es el que hay que buscar y reemplazar. Si es as�, haremos el reemplazo con �ste,
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
                               // checkbox 'confirm replace' s�lo se aplica a ReplaceAll)
    end;


    if DoReplace then begin
        while SelectedTextToReplace do begin
            try
                // �Hay que restringirse al texto inicialmente seleccionado?
                if ReplaceAll and FindOptions.SelectedText then
                   if (Editor.SelStart < FindOptions.SelectionStart) or
                     ((Editor.SelStart + Editor.SelLength) > FindOptions.SelectionEnd) then
                       break;

                if GetReplacementConfirmation then begin
                   inc(ReplaceCnt);
                   Editor.AddText(FindOptions.ReplaceWith);
                   if Editor.NNodeObj <> nil then
                      App.ChangeInEditor(Editor);
                end;

                Application.ProcessMessages;
                if UserBreak then break;

                SelectedTextToReplace:= RunFindNext(ReplaceAll);     // Localizamos el siguiente patr�n a remplazar
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
  end
  else
      if not SelectedTextToReplace then begin
         DoMessageBox(Format( STR_02, [Text_To_Find] ), STR_12, 0, handle);
      end;

end;


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
end;


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
