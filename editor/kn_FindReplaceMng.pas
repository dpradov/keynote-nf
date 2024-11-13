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
   Vcl.Graphics,
   VirtualTrees,
   gf_misc,
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

type
   TResultSearch = class
      BeginOfParagraph: integer;
      WordsPos: array of integer;
      WordsSel: array of integer;
   end;
   TResultsSearch= TSimpleObjList<TResultSearch>;      // TList<TResultSearch>;

   TWordInResultSearch = class
      BeginOfParagraph: integer;
      WordPos: integer;
      WordSel: integer;
   end;

var
   ResultsSearch: TResultsSearch;


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
   knt.App,
   knt.RS
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

type
  CharacterSet = set of char;

type
   TDistanceScope = (dsAll, dsSentence, dsParagraph);

type
   TSearchWord = class
      word: string;
      RightSep: char;
      Scope: TDistanceScope;
      BeginDScope: boolean;
      EndDScope: boolean;
      WordPos: integer;
      SizeInternalHiddenText: integer;
   end;

  TSearchWordList= TSimpleObjList<TSearchWord>;      // TList<TSearchWord>;

  TLastWordFollowed = record
      iResults: integer;
      iWord: integer;
  end;

var
  LastWordFollowed: TLastWordFollowed;
  WordInResultSearch: TWordInResultSearch;



function RunFindNextInNotes (Is_ReplacingAll: Boolean= False): boolean; forward;
function RunFindNextInEditor (Is_ReplacingAll: Boolean= False): boolean; forward;

procedure FindEventProc( sender : TObject ); forward;
procedure ReplaceEventProc( ReplaceAll : boolean ); forward;
procedure Form_FindReplaceClosed( sender : TObject ); forward;


const
   CHR_SEP_SENTENCE  = Chr(17);       // 17 ($11): DC1 (Device Control 1)    <==>  '..'
   CHR_SEP_PARAGRAPH = Chr(18);       // 18 ($12): DC2 (Device Control 2)    <==>  '...'
   FIND_SENTENCE_SCOPE_SEP = '..';
   FIND_PARAGRAPH_SCOPE_SEP = '...';
   FIND_EMPHASIZED_SEARCH_WORDS_CHR = '*';
   FIND_EMPHASIZED_SEARCH_PARAGRAPH_CHR = '**';




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
    showmessage( sFnd05 );
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
                                  var pL_Extract, pR_Extract: integer;
                                  wordList : TSearchWordList = nil;
                                  const LastPattern: string = ''; LastPatternPos: integer= -1; SecondPart: boolean= false): string;
var
  pL, pR, p, pGap, pStart, pMin: integer;
  extL, extR: integer;
  len, i, iMin, gap: integer;
  bakFirstChar: char;
  strSearch, strExtract: string;
  Word, scapeFirstChar: string;
  p2Pos, ip2: integer;

begin
    if (Str='') then Exit('');

    // pL_Extract, pR_Extract: If they are > 0, they indicate the extremes to try to consider, as far as possible, of the extract to be used.

    p2Pos:= integer.MaxValue;
    if (LastPattern <> '') and (wordlist.count > 2) then begin
       for i := 0 to wordList.Count - 1 do begin
          p:= wordList[i].WordPos;
          if (p >= 0) and (p < p2Pos) and (p <> pPos) and (p <> LastPatternPos) then begin
             p2Pos:= p;
             ip2:= i;
          end;
       end;
       if p2Pos < integer.MaxValue then
          inc(p2Pos, wordList[ip2].word.Length);
    end;


    inc(pPos);

    bakFirstChar:= Str[pPos];

    // Searching for left limit of extract
    extL:= pPos-50;
    if (pL_Extract >= 0) then
       if (pPos-pL_Extract < 100) then
          extL:= pL_Extract
       else
          extL:= pPos-75;

    pL:= pPos+1;
    repeat
       dec(pL);
    until (pL = 1) or (Str[pL] = #13) or ((pL < extL) and (Str[pL] in [#32,#9,#7]));
    if Str[pL] = #13 then inc(pL);

    // Searching for right limit of extract
    extR:= 50;
    if (pR_Extract >= 0) then
       if (pR_Extract-pPos < 100) then
          extR:= pR_Extract
       else
          extR:= 75
    else
    if not SecondPart then
       if (p2Pos <> integer.MaxValue) and (p2Pos-pPos < 100) then
          extR:= p2Pos
       else
       if (wordlist <> nil) then
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
               Word:= ScapeSpecialRTFCharacters(wordlist[i].word);
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
               len:= length(ScapeSpecialRTFCharacters(wordlist[iMin].word));
               insert('{\b\cf2 ', Result, pGap);
               insert('}', Result, pGap + Len + 8);
               pStart:= pMin + len;
               gap:= gap + 9;
            end

        until pMin = integer.MaxValue;
    end;

    if pR < (LastPatternPos + LastPattern.Length) then begin       // => only in smAll  (or smAny with distance scope <> dsALL)
       if pR >= LastPatternPos then
          pR := LastPatternPos - 1;
       pL_Extract:= pR+1;       // So that the second part of the extract does not start from a previous position

       strExtract:= GetFindedPatternExtract(Str, LastPattern, LastPatternPos, lastPos, pL_Extract, pR_Extract, wordList, '',-1, true);
       if pL_Extract > pR+1 then
          Result:= Result + '{\b\cf2  . . //. . }';
       Result:= Result + strExtract;
       lastPos:= LastPatternPos + LastPattern.Length;
    end
    else
       lastPos:= pR;   // To use with smAny

    pL_Extract:= pL -1;
    if lastPos > pR_Extract then
       pR_Extract:= lastPos;
end;


{
   Convert a search pattern into a list of words tagged according to:

 * Whether they can be located anywhere in the content of a note or, on the contrary, if they must be found
   in the same paragraph or the same sentence.
  - By default, separating words with spaces means that they can be found anywhere, there is no restriction
    on the distance between them (dsAll)
  - If two or more words are separated by "...", they must all be located in the same paragraph to be recognized as a match.
  - If two or more words are separated by "..", they must all be located in the same sentence to be recognized as a match.
     Sentences are understood to be separated by a period and at least one space or by a period and a tabulation sign,
     or by a line break or a column separator in a table.

  - If there are more than two words joined with ".." or "..." only the first separator will be considered.
    The rest should be the same, and if they are not, they are forced.
    For example, searching for something like [w1..w2...w3 w4] will be equivalent to [w1..w2..w3 w4]
   - The order in which the words are joined is not important. Searching for [w1..w2] is the same as searching for [w2..w1]
   - If you need to search for the separator characters themselves, they should be enclosed in quotes and will be treated
     as normal text to search for. Ex: "w1..w2"

   - This restriction on distance or separation between words can be used with both the 'All' method (All the words)
     and the 'Any' method (Any of the words).
     Even if it is the 'Any' method, all words linked by distance (.. or ...) must be found to be considered a match.
  Example:

  w1...w2..w3 w4..w5 w6
     w1, w2 and w3 must be located within the same paragraph; w4 and w5 must be found within the same sentence, which
     may or may not be within the same paragraph in which w1, w2 and w3 were found; w6 must be found in the note
     but anywhere in it.
  	 -> If the method is All: all combinations will be displayed for which the presence of ALL words can be counted:
   	    w1, w2 and w3 must be located together in the same paragraph, w4 and w5 in the same sentence and w6, anywhere.

     -> If the method is ANY: all matches will be displayed, independently, for [w1, w2 and w3] (treated as a unit,
        that is, all three must be found in the same paragraph to be considered a match), [w4 and w5] (both in the
        same sentence, also as a unit) and w6.

 * If the '*' character is found individually, it will mean that the words to be searched for must be emphasized:
   bold, underlined or highlighted.

      Example: [* text plain]

 * If the modifier '**' is found, all words must be located in the same paragraph or in the name of a note.
   In addition, all words in that paragraph must be emphasized: in bold, underlined or highlighted,
   or with a font size larger than the words in the following paragraph (the first characters after the line break will
   be ignored in case any additional line break was added with the same font size).
   - All this will be checked at least on the first and last word searched as well as the beginning and end of the paragraph,
   - Note: words underlined as a result of containing hyperlinks won't be not considered emphasized.
   - Note: when this modifier is found, search scope used will be ssContentsAndNodeName (ignoring the current selection
     of this group box)
   - If a note/node name could be matched, and the same paragraph is used at the beginning of the note content,
     also as a match, then only the latter will be included in the results list.

   The idea is to use this modifier to locate the headings or sections in which the concepts or topics we are interested in
   can be defined. These will normally be in a single sentence, but paragraph mode is used because sometimes small sentences
   can be used together, e.g.: "Options in Exporting. Examples"

      Examples: [** images] [** keyboard short]
}

procedure SearchPatternToSearchWords(WordList : TSearchWordList; const SearchPattern : string;
                                     var myFindOptions: TFindOptions );
var
  aStr, s : string;
  InQuotes : boolean;
  p, L : integer;
  ch, prevch : char;
  i: integer;
  SW: TSearchWord;
  Delims : CharacterSet;
  InDScope: boolean;
  SearchMode: TSearchMode;

  procedure AddWord;
  begin
     SW:= TSearchWord.Create;
     SW.word:= s;
     SW.RightSep:= ch;
     SW.EndDScope:= false;
     WordList.Add(SW);
  end;

begin

  if ( SearchPattern = '' ) then exit;

  s := '';
  p := 0;
  prevch := #0;
  InQuotes := false;

  SearchMode:= myFindOptions.SearchMode;

  Delims:= [#32, CHR_SEP_SENTENCE, CHR_SEP_PARAGRAPH];
  aStr:= SearchPattern;

  aStr:= StringReplace(aStr, FIND_PARAGRAPH_SCOPE_SEP, CHR_SEP_PARAGRAPH, [rfReplaceAll]);
  aStr:= StringReplace(aStr, FIND_SENTENCE_SCOPE_SEP,  CHR_SEP_SENTENCE, [rfReplaceAll]);
  L := length( aStr );

  try
    while ( p < L ) do begin
      inc( p );
      ch := aStr[p];
      if ( ch in Delims) then begin
         if ( InQuotes and ( prevch <> '"' )) then
            s := s + ch
         else begin
            AddWord;
            s := '';
            InQuotes := false;
         end;
      end
      else
      if ( ch = '"' ) then begin
         if ( prevch = '"' ) then begin   // dooubled quotes are "escaped", i.e. they're real quote characters rather than group words
           s := s + '"';
           ch := #0;
         end
         else begin
            if ( InQuotes or ( s <> '' )) then begin
               if InQuotes then begin     // nothing; we will see what to do when we get the next char
                  if ( p = L ) then
                     Ch := #32;           // otherwise we'll lose the last field if it is a blank string
               end
               else
                 // IMPOSSIBLE: the string was not quoted, so it can't contain embedded quote characters
                 //raise EConvertError.Create( 'Unmatched double quote in string at pos ' + inttostr( p ));
                 exit;
            end
            else begin
               InQuotes := true;
               ch := #0;
            end;
         end;

      end
      else
         s := s + ch;

      prevch := ch;
    end;

    if (s <> '') or (prevch in Delims) then begin
       ch:= ' ';
       AddWord;
    end;

  finally

     for i := wordList.count - 1 downto 0 do begin
        if wordList[i].word = '' then
           wordList.delete(i)
        else
        if wordList[i].word = FIND_EMPHASIZED_SEARCH_PARAGRAPH_CHR then begin        // **
           myFindOptions.EmphasizedSearch:= esParagraph;
           myFindOptions.SearchScope:= ssContentsAndNodeName;
           wordList.delete(i);
        end
        else
        if wordList[i].word = FIND_EMPHASIZED_SEARCH_WORDS_CHR then begin            // *
           myFindOptions.EmphasizedSearch:= esWords;
           wordList.delete(i);
        end
        else begin
           wordList[i].word := StringReplace(wordList[i].word,  CHR_SEP_PARAGRAPH, FIND_PARAGRAPH_SCOPE_SEP, [rfReplaceAll]);
           wordList[i].word := StringReplace(wordList[i].word , CHR_SEP_SENTENCE,  FIND_SENTENCE_SCOPE_SEP,  [rfReplaceAll]);
        end;
     end;


     if myFindOptions.EmphasizedSearch = esParagraph then
        for i := 0 to wordList.count - 1 do begin
           wordList[i].Scope:= dsParagraph;
           wordList[i].BeginDScope:= (i=0);
           wordList[i].EndDScope:= (i<>0);
        end

     else begin
         InDScope:= false;
         for i := 0 to wordList.count - 2 do begin
            ch:= wordList[i].RightSep;
            case ch of
               CHR_SEP_PARAGRAPH, CHR_SEP_SENTENCE: begin
                   wordList[i].BeginDScope:= not InDScope;
                   InDScope:= true;

                   if ch = CHR_SEP_PARAGRAPH then
                       wordList[i].Scope:= dsParagraph
                   else
                       wordList[i].Scope:= dsSentence;
                   wordList[i+1].Scope:= wordList[i].Scope;
                   if wordList[i+1].RightSep <> ' ' then
                      wordList[i+1].RightSep:= ch;
               end
               else begin
                  InDScope:= false;
                  wordList[i].EndDScope:= true;
               end;
            end;
         end;

         for i := 0 to wordList.count - 1 do begin
             if wordList[i].Scope = dsAll then begin
                wordList[i].BeginDScope:= true;
                wordList[i].EndDScope:= true;
             end
         end;
         wordList[wordList.count - 1].EndDScope:= true;
     end;


  end;

end; // SearchPatternToSearchWords


function GetTextScope(const Text: string; Scope: TDistanceScope; PosInsideScope: integer; var pL_Scope, pR_Scope: integer; pLmin: integer): string;
var
  p, i: integer;
begin
  { We have a position (PosInsideScope) located within a sentence or paragraph (according to Scope). We have to locate
    the beginning and end of it and return it
    .... [ ......  X ....... ] ....

     #9: TAB   #7:column separator in Tables
  }

  if Scope = dsAll then exit(Text);

  pR_Scope:= Pos(#13, Text, PosInsideScope);
  if pR_Scope <= 0 then
     pR_Scope:= Text.Length;

  if Scope = dsSentence then begin
     for p := PosInsideScope + 1 to pR_Scope-1 do
       if ((Text[p] = '.') and (Text[p+1] in [' ', #9])) or (Text[p] = #7)   then
          break;
     if p < pR_Scope then
        pR_Scope:= p;
  end;


  pL_Scope := NFromLastCharPos(Text, #13, 1, PosInsideScope);
  if pL_Scope <= 0 then
     pL_Scope:= 1;
  if pL_Scope < pLmin then
     pL_Scope:= pLmin;

  if Scope = dsSentence then begin
     for p := PosInsideScope-1 downto pL_Scope do
       if ((Text[p] = '.') and (Text[p+1] in [' ', #9])) or (Text[p] = #7)   then
          break;
     if (p > pL_Scope) then
        pL_Scope := p;
  end;

  Result:= Copy(Text, pL_Scope, pR_Scope - pL_Scope + 1);
end;


procedure PreprocessTextPattern (var myFindOptions : TFindOptions);
var
  pF: integer;
  NumDaysBack: integer;
begin
  {
   Trying to identify '-<number>'  in the beginning Eg: '-3' '-7 search term'
   If current day is '15/10/2024' then:
     '-2' is equivalent to [LastModified] >= '13/10/2024'
     '-0' is equivalent to [LastModified] >= '15/10/2024'  (modified on current day)
   If enclosed in "" it will managed as a normal text pattern:
    '"-2"'
 }

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


  New in 2.0.0:
  - Possible to use as a criterion the date of creation or modification of the notes
  - It is possible to indicate whether some of the words to be searched for must be in the same paragraph or sentence.
  - It is possible to indicate if the words to be searched must be emphasized, or also inside a same
    paragraph

    See more detailed description in the comment at the beginning of the SearchPatternToSearchWords procedure
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
  wordList : TSearchWordList;
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
  LastLocationAdded_Str: String;
  LastLocationAdded_NodeName: boolean;

  SearchIn: string;
  LastDScope, CurrentDScope: TDistanceScope;
  pL_DScope, pR_DScope: integer;       // start and end position, within TextPlain, of a sentence or paragraph
  widxBeginDScope, PosFirstWordDScope, SearchOriginDScope, AbsSearchOrigin: integer;
  pL_DScopeToConsider, pR_DScopeToConsider: integer;
  PatternPos1_DScope, PatternPosN_DScope: integer;
  PatternInPos1_DScope, PatternInPosN_DScope: string;
  SizeInternalHiddenTextInPos1_DScope: integer;
  IsBeginInDScope: boolean;
  NewDScope: boolean;
  RetryDScope: boolean;
  ReusedWordPos: boolean;
  Paragraph: string;
  PositionsLastLocationAdded: array of integer;

type
   TLocationType= (lsNormal, lsNodeName, lsMultimatch);


       procedure AddResultSearch(wordList : TSearchWordList; pL_Extract: integer = 0; pR_Extract: integer = 0);
       var
          ResultSearch: TResultSearch;
          i, j, p, N, M: integer;
          pMin, iMin: integer;
       begin
          ResultSearch:= nil;

          if wordList <> nil then begin
             ResultSearch:= TResultSearch.Create;
             with ResultSearch do begin
                 BeginOfParagraph:= NFromLastCharPos(TextPlainBAK, #13, 1, pL_Extract);
                 N:= wordList.Count;
                 SetLength(WordsPos, 2 * N);
                 SetLength(WordsSel, 2 * N);

                 for i := 0 to N - 1 do begin
                    p:= wordList[i].wordPos;
                    WordsPos[i]:= -1;
                    if (p >= pL_Extract) and ((p <= pR_Extract)) then begin
                       WordsPos[i+N]:= p;
                       WordsSel[i+N]:= wordList[i].word.Length + wordList[i].SizeInternalHiddenText;
                    end;
                 end;

                 M:= 0;
                 for j := 0 to N - 1 do begin
                    iMin:= -1;
                    pMin:= integer.MaxValue;
                    for i := 0 to N - 1 do begin
                       if (WordsPos[i+N] < pMin) and (WordsPos[i+N] >=0) then begin
                          pMin:= WordsPos[i+N];
                          iMin:= i;
                       end;
                    end;
                    if iMin >= 0 then begin
                       WordsPos[j]:= WordsPos[iMin+N];
                       WordsSel[j]:= WordsSel[iMin+N];
                       WordsPos[iMin+N]:= -1;
                       inc(M);
                    end;
                 end;
                 SetLength(WordsPos, M);
                 SetLength(WordsSel, M);
             end;
          end;

          ResultsSearch.Add(ResultSearch);
       end;


       procedure ClearResultsSearch;
       var
         i: integer;
       begin
          if ResultsSearch <> nil then begin
             for i := 0 to ResultsSearch.Count-1 do
                 ResultsSearch[i].Free;
             ResultsSearch.Clear;
          end;
       end;

       function AddLocation (SearchingInNodeName: boolean; LocationType: TLocationType;
                             const FirstPattern: string; PatternPos: integer;
                             wordList : TSearchWordList = nil;
                             pL_Extract: integer= -1; pR_Extract: integer= -1;
                             const LastPattern: string = ''; LastPatternPos: integer= -1;
                             Paragraph: string = ''): integer;
       var
         str, strExtract : string;
         iLast: integer;

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
             Location.CaretPos := -1;     // means: text found in node name
          end;

          if (myFindOptions.EmphasizedSearch = esParagraph) and LastLocationAdded_NodeName then begin
              Paragraph:= StringReplace(Paragraph, #13,'', [rfReplaceAll]);
              if LastLocationAdded_Str.ToUpper.Trim = Paragraph.ToUpper.Trim then begin
                 iLast:= Location_List.Count-1;
                 Location_List.Delete(iLast);
                 ResultsSearch.Delete(iLast);
              end;
          end;

          strExtract:= GetFindedPatternExtract(str, FirstPattern, PatternPos, Result, pL_Extract, pR_Extract, wordList, LastPattern, LastPatternPos); // ** Result is set here
          Location.Params:= strExtract;
          if not TreeFilter then
             Location_List.Add(Location);

          if not SearchingInNodeName then
             AddResultSearch(wordList, pL_Extract, pR_Extract)
          else
             AddResultSearch(nil);

          if myFindOptions.EmphasizedSearch = esParagraph then begin
             LastLocationAdded_Str:= Paragraph;
             LastLocationAdded_NodeName:= SearchingInNodeName;
          end;
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

       function CheckEmphasized(EmphasizedSearch: TEmphasizedSerch;
                                SearchingInNodeName: boolean;
                                const PatternText1: string; PatternPos1: integer;
                                const PatternTextN: string= ''; PatternPosN: integer = -1;
                                pL_Parag: integer = -1;
                                pR_Parag: integer = -1): boolean;
       var
         FontStyles: TFontStyles;
         NEntry: TNoteEntry;
         FontSizeNextParag: integer;

         function CheckPattern(PatternPos: integer): boolean;
         var
           FSize: integer;
           SelColor: TColor;
         begin
            Result:= False;
            with RTFAux do begin
               SelStart:= PatternPos + 1;
               FontStyles:= SelAttributes.Style;
               FSize:= SelAttributes.Size;
               SelColor:= SelAttributes.BackColor;
               if not SelAttributes.Link and
                  ( (fsBold in FontStyles) or
                    (fsUnderline in FontStyles) or
                    ((SelColor <> clWindow) and not ((SelColor = clWhite) and ((RTFAux.Color = clWhite) or (RTFAux.Color = clWindow))) ) or
                    ((FSize > 10) and (FSize > FontSizeNextParag))
                   ) then
                    Result:= True;
            end;

         end;

       begin
           if SearchingInNodeName or (EmphasizedSearch = esNone) or (PatternText1 = '') then exit(true);

           Result:= False;

           with RTFAux do begin
               if RTFAux.TextLength = 0 then begin
                  NEntry:= myNNode.Note.Entries[0];         // %%%
                  LoadStreamInRTFAux (NEntry.Stream, RTFAux);
               end;

               FontSizeNextParag:= 9999;
               if (EmphasizedSearch = esParagraph) then begin
                  SelStart:= pR_Parag + 2;
                  FontSizeNextParag:= SelAttributes.Size;
               end;

               if not CheckPattern(PatternPos1) then
                  exit;
               if (PatternTextN <> '') and not CheckPattern(PatternPosN) then
                  exit;

               if (EmphasizedSearch = esParagraph) then begin
                  if (pL_Parag <> -1) and not CheckPattern(pL_Parag) then
                     exit;
                  if (pR_Parag <> -1) and not CheckPattern(pR_Parag - 3) then
                     exit;
               end;

               Result:= True;
           end;
       end;

       function RememberLastPositionsFound (PatternPos1, PatternPosN: integer): boolean;
       var
          i, p: integer;
       begin
          for i:= 0 to wordlist.count-1 do begin
             p:= wordlist[i].WordPos;
             if ((p = PatternPos1) or (p <= PatternPosN)) then
                PositionsLastLocationAdded[i]:= p
             else
                PositionsLastLocationAdded[i]:= -1;
          end;
       end;

       function CheckNewPositionsFound(PatternPos1, PatternPosN: integer): boolean;
       var
          i, p: integer;
       begin
          Result:= false;
          for i:= 0 to wordlist.count-1 do begin
             p:= wordlist[i].WordPos;
             if not ((p = PatternPos1) or (p <= PatternPosN)) then continue;
             if (p > PositionsLastLocationAdded[i]) then
                exit(true);
          end;
       end;

       function ClearLastPositionsFound: boolean;
       var
          i: integer;
       begin
          for i:= 0 to wordlist.count-1 do
             PositionsLastLocationAdded[i]:= -1;
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
                         { PatternPos := EditControl.FindText(myFindOptions.Pattern, SearchOrigin, -1, SearchOpts ); }
                         if ( PatternPos >= 0 ) then begin
                             SearchOrigin := PatternPos + PatternLen; // move forward in text
                             if myFindOptions.EmphasizedSearch = esParagraph then
                                Paragraph:= GetTextScope(TextPlain, dsParagraph, PatternPos + 1, pL_DScope, pR_DScope, 1);
                             if CheckEmphasized(myFindOptions.EmphasizedSearch, SearchingInNodeName, TextToFind, PatternPos,'',-1, pL_DScope, pR_DScope) then
                                AddLocation(SearchingInNodeName, lsNormal, TextToFind, PatternPos,nil,-1,-1,'',-1,Paragraph);
                         end;
                         Application.ProcessMessages;
                      until UserBreak or (PatternPos < 0);
                    end;

              smAny, smAll :
                begin
                   var i: integer;
                   for i := 0 to wordList.Count-1 do wordList[i].WordPos:= -1;
                   ClearLastPositionsFound;
                   repeat
                       LastDScope:= dsAll;
                       widxBeginDScope:= -1;
                       pL_DScopeToConsider:= -1;
                       pR_DScopeToConsider:= -1;
                       NewDScope:= True;
                       RetryDScope:= False;
                       wordidx:= 0;

                       PatternPos1:= Integer.MaxValue;
                       PatternPosN:= -1;
                       PatternInPos1:= '';
                       PatternInPosN:= '';
                       MultiMatchOK := false;

                       repeat
                          thisWord := wordList[wordidx].word;
                          CurrentDScope:= wordList[wordidx].Scope;
                          IsBeginInDScope:= wordList[wordidx].BeginDScope;

                          if IsBeginInDScope and (widxBeginDScope <> wordidx) then
                             NewDScope:= true;

                          if NewDScope then begin
                             PosFirstWordDScope:= 0;
                             widxBeginDScope:= wordidx;
                          end;
                          if NewDScope or RetryDScope then begin
                             PatternPos1_DScope:= Integer.MaxValue;
                             PatternPosN_DScope:= -1;
                             pL_DScope:= -1;
                             pR_DScope:= -1;
                          end;
                          NewDScope:= false;
                          RetryDScope:= false;

                          if IsBeginInDScope then begin     // (All words where Scope = dsAll => IsBeginInDScope=True)
                             SearchIn:= TextPlain;
                             if PosFirstWordDScope > 0 then
                                SearchOriginDScope:= PosFirstWordDScope + wordList[widxBeginDScope].word.Length + wordList[widxBeginDScope].SizeInternalHiddenText
                             else
                                SearchOriginDScope:= SearchOrigin + 1;
                             AbsSearchOrigin:= SearchOriginDScope;
                          end
                          else begin
                             SearchIn:= GetTextScope(TextPlain, CurrentDScope, PosFirstWordDScope + 1, pL_DScope, pR_DScope, SearchOrigin+1);
                             SearchOriginDScope:= 1;
                             AbsSearchOrigin:= pL_DScope;
                          end;
                          LastDScope:= CurrentDScope;

                          ReusedWordPos:= false;
                          if wordList[wordidx].WordPos > AbsSearchOrigin then begin
                             PatternPos:= wordList[wordidx].WordPos;
                             SizeInternalHiddenText:= wordList[wordidx].SizeInternalHiddenText;
                             ReusedWordPos:= true;
                          end
                          else
                             PatternPos:= FindPattern(thisWord, SearchIn, SearchOriginDScope, SizeInternalHiddenText) -1;
                          { PatternPos := EditControl.FindText(thisWord, 0, -1, SearchOpts); }

                          if (CurrentDScope <> dsAll) then begin
                             if IsBeginInDScope then
                                PosFirstWordDScope:= PatternPos
                             else
                             if (PatternPos < 0) then begin
                                MultiMatchOK := false;
                                wordidx:= widxBeginDScope;   // Search again the first word, from the last found position (PosFirstWordScope)
                                RetryDScope:= true;
                                inc(PosFirstWordDScope);
                                continue;
                             end;
                          end;

                          if ( PatternPos >= 0 ) then begin          // ----------- PatternPos >= 0
                             wordList[wordidx].SizeInternalHiddenText:= SizeInternalHiddenText;
                             if (CurrentDScope <> dsAll) then begin
                                if not IsBeginInDScope and not ReusedWordPos then
                                    inc(PatternPos, pL_DScope-1);       // It is a relative search, within the sentence or paragraph
                                if PatternPos < PatternPos1_DScope then begin
                                   PatternPos1_DScope:= PatternPos;
                                   PatternInPos1_DScope:= thisWord;
                                   SizeInternalHiddenTextInPos1_DScope:= SizeInternalHiddenText;
                                end;
                                if PatternPos > PatternPosN_DScope then begin
                                   PatternPosN_DScope:= PatternPos;
                                   PatternInPosN_DScope:= thisWord;
                                end;
                             end
                             else    // ---- dsAll
                                if (PatternPos < PatternPos1) then begin
                                   PatternPos1:= PatternPos;
                                   PatternInPos1:= thisWord;
                                   SizeInternalHiddenTextInPos1:= SizeInternalHiddenText;
                                end;

                             wordList[wordidx].WordPos:= PatternPos;

                             if SearchModeToApply = smAll then begin
                                 if PatternPos > PatternPosN then begin
                                    PatternPosN:= PatternPos;
                                    PatternInPosN:= thisWord;
                                 end;
                             end;

                             if wordList[wordidx].EndDScope then begin       // (All dsAll => EndScope =True)
                                MultiMatchOK := true; // assume success
                                if (PatternPos1_DScope < PatternPos1) and (CurrentDScope <> dsAll) then begin
                                   PatternPos1:= PatternPos1_DScope;
                                   PatternInPos1:= PatternInPos1_DScope;
                                   SizeInternalHiddenTextInPos1:= SizeInternalHiddenTextInPos1_DScope;

                                   if SearchModeToApply = smAny then begin
                                      PatternPosN:= PatternPosN_DScope;
                                      PatternInPosN:= PatternInPosN_DScope;
                                   end
                                   else
                                   if SearchModeToApply = smAll then
                                      pL_DScopeToConsider:= pL_DScope;
                                end;
                                if (SearchModeToApply = smAll) and (pR_DScope > PatternPosN) then
                                   pR_DScopeToConsider:= pR_DScope;
                             end;

                          end
                          else          // --------------------------     PatternPos < 0
                             case SearchModeToApply of
                                 smAll: begin
                                        MultiMatchOK := false;
                                        break; // note must have ALL words
                                 end;
                                 smAny:
                                    if (CurrentDScope <> dsAll) then begin
                                        // If we search for something like w1..w2..w3 w4, and no match is found
                                        // for w1 or w2 or w3, we have to continue to w4.
                                        // Using .. (or ...) causes those words to be treated as a block that must
                                        // be located as a whole to be considered a match.
                                        // We will jump to the next DScope (which can be dsAll) and PosFirstWordDScope <= 0
                                        // so w4 will be searched from the beginning (from SearchOrigin)
                                        while (wordidx+1 <= wordcnt -1)
                                               and (not wordList[wordidx+1].BeginDScope)  do
                                           inc(wordidx);

                                        // If the word we haven't found is the first one in the DScope set (w1 in the ex.)
                                        // and there is no other word to search for (like w4 in the example) then, after the
                                        // inc(wordidx) before the until, we will exit the iteration and since MultiMatchOK=False,
                                        // we will stop searching in this note. It would be correct.
                                        // But if the one we haven't found is not the first one (w1) over another one in the set
                                        // (w2 or w3) then we should not exit the iteration, but do another search
                                        // starting with the first word. We should continue beyond where we have searched

                                        if not IsBeginInDScope and (wordidx = wordcnt -1) then begin
                                           wordidx:= -1;                              // tras inc(wordidx) -> = 0
                                           SearchOrigin := PatternPos1_DScope + 1;    // move forward in text
                                        end;
                                        NewDScope:= True;
                                    end;
                              end;


                          Application.ProcessMessages;
                          if UserBreak then begin
                             FindDone := true;
                             break;
                          end;

                         inc(wordidx);
                       until wordidx > wordcnt -1;


                       if MultiMatchOK then begin
                          // if myFindOptions.EmphasizedSearch = esParapraph => All words must necessarily belong to the same paragraph
                          // -> pL_DScope, pR_DScope will be used

                          if not CheckNewPositionsFound(PatternPos1, PatternPosN) or
                             not CheckEmphasized(myFindOptions.EmphasizedSearch, SearchingInNodeName, PatternInPos1, PatternPos1, PatternInPosN, PatternPosN, pL_DScope, pR_DScope) then begin
                             SearchOrigin := PatternPos1 + PatternInPos1.Length;
                             continue;
                          end;

                          RememberLastPositionsFound (PatternPos1, PatternPosN);

                          { *1
                           Within the search in a note (NNode) we will change to smAny mode from smAll after
                           the first match to allow locating and offering more useful results.
                           Let's suppose that we search for something like: [w1..w2 w3] (or simply [w1 w2 w3]
                           and we do it in a note with a text of the form:
                               ... w2 ... w1
                               ....... w3 ...
                               ...
                               ... w1 ... w2 ...

                           So far we have only been returning as a match the one that includes from the first w2
                           to w3. Since there are no more w3, the final terms, w1 and w2 are ignored because from the
                           final position of the last match (w3) we cannot find all the terms again.
                           But why are the first ones (w2 .. w1) going to be more significant than the second ones (w1 .. w2)?
                           We will surely be interested in knowing about the latter as well.
                           * The nature of the smAll method is maintained in the sense that we are offering all these
                           matches within this note because all the searched terms are found in it
                           From that moment on, and once verified, we are interested in showing all the terms in the same
                           way as we do with smAny, including several terms together in the same match if
                           they are close and can be offered in the same extract.

                           If the text were as follows, we will still return the two pairs w1, w2.
                           The number of results may vary slightly depending on whether or not the word w3
                           has been displayed in any previous result.

                               ... w2 ... w1
                               ...
                               ... w1 ... w2 ..
                               .... w3 ...
                          }

                          if SearchModeToApply = smAll then begin
                             if pL_DScopeToConsider > PatternPos1 then pL_DScopeToConsider:= -1;
                             if pR_DScopeToConsider < PatternPosN then pR_DScopeToConsider:= -1;
                             AddLocation (SearchingInNodeName, lsMultimatch, PatternInPos1, PatternPos1, wordList, pL_DScopeToConsider, pR_DScopeToConsider, PatternInPosN, PatternPosN, SearchIn);
                             SearchOrigin := PatternPos1 + PatternInPos1.Length;   // move forward in text
                             SearchModeToApply := smAny;                           // *1
                          end
                          else
                             SearchOrigin:= 1 + AddLocation (SearchingInNodeName, lsMultimatch, PatternInPos1, PatternPos1, wordList, -1, -1, PatternInPosN, PatternPosN, SearchIn);
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

  wordList := TSearchWordList.Create;
  RTFAux:= CreateAuxRichEdit;


  try
    try
      if not TreeFilter  then begin
         ClearLocationList( Location_List );
         ClearResultsSearch;
      end;

      SearchPatternToSearchWords (wordList, TextToFind, myFindOptions);
      SetLength(PositionsLastLocationAdded, wordList.Count);
      if TreeFilter then
         myFindOptions.EmphasizedSearch:= esNone;


      wordcnt := wordList.count;

      if wordcnt = 1 then begin
         myFindOptions.SearchMode := smPhrase;    // If not used smPhrase then the line number will not be shown, and will be confusing
         TextToFind:= wordList[0].word;           // '"Windows 10"' --> 'Windows 10'
      end;

      if (wordcnt = 0) and not SearchingByDates then begin
         Form_Main.Combo_ResFind.Text:= '';
         Form_Main.Btn_ResFind.Enabled:= False;
         exit;
      end;
      SearchModeToApply := myFindOptions.SearchMode;

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
                nodeToFilter:= true;               // I assume the pattern will not be found, so the node will be filtered (if ApplyFilter=True)
                SearchOrigin := 0;                 // starting a new node
                RTFAux.Clear;                      // Let's make sure it's empty before calling PrepareTextPlain, just in case it wasn't necessary to use it.
                                                   // and we are interested in having the RTF content of the node (in case the Emphasized option is used)
                SearchModeToApply := myFindOptions.SearchMode;    // Within each NNode we can temporarily switch from smAll to smAny

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
         Form_Main.LblFindAllNumResults.Caption:= '  ' + MatchCount.ToString + sFnd13;
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
         Form_Main.Btn_ResFlip.Caption := sFnd06;
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
  Form_Main.LblFindAllNumResults.Caption:= '  ' + i.ToString + ' / ' + Location_List.Count.ToString + sFnd13;
  SelectedMatch:= i;
end;


procedure FindAllResults_FollowMatch(i: integer);
var
  Location : TLocation;
  iWord: integer;
  WordInRS: TWordInResultSearch;
begin
  Location := Location_List[i-1];
  if ( not assigned( Location )) then exit;

  { It is not trivial to know if the current position in the editor matches any of the words found
    in the match, since FindAll searches over TextPlain (CaretPosition is a position in imLinkTextPlain)
    and since there may be images, it is possible that the real position of the cursor is displaced with an offset.
    Therefore, the simplest thing is that from here, we force JumpToLocation to select a specific word,
    different from the one saved in Location, if necessary, helping us with LastWordFollowed
  }
  iWord:= -1;
  WordInRS:= nil;

  if ResultsSearch[i-1] <> nil then begin
     WordInRS:= WordInResultSearch;
     if LastWordFollowed.iResults = i-1 then begin
        iWord:= LastWordFollowed.iWord;
        WordInRS.BeginOfParagraph:= -1;
     end
     else
        WordInRS.BeginOfParagraph:= ResultsSearch[i-1].BeginOfParagraph;

     inc(iWord);
     if iWord >= Length(ResultsSearch[i-1].WordsPos) then
        iWord:= 0;

     WordInRS.WordPos:= ResultsSearch[i-1].WordsPos[iWord];
     WordInRS.WordSel:= ResultsSearch[i-1].WordsSel[iWord];
  end;

  LastWordFollowed.iResults := i-1;
  LastWordFollowed.iWord := iWord;

  JumpToLocation( Location, true,true,urlOpen,false,  true, WordInRS);
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
          if Wrap and (NumberFoundItems > 0) then begin    // Wrap será siempre false si Is_ReplacingAll = True
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
      FindDone:= True;   // Supondremos inicialmente que no se podrá avanzar más

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

  Form_Main.StatusBar.Panels[PANEL_HINT].text := sFnd07;

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

      // Identificación de la posición de inicio de la búsqueda ---------------------------
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


      // Búsqueda del patrón iterando sobre los nodos / notas hasta encontrar uno -------------
      // Según las opciones establecidas así podrán recorrerse o no todos los nodos de una nota, todas las notas
      // o incluso continuar buscando desde el punto de partida, de manera cíclica.

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
          App.PopupMessage( sFnd08 +#13+ E.Message, mtError, [mbOK] );
          exit;
      end;
    end;

  finally
      if Found then begin
          if not Is_ReplacingAll then
             Form_Main.StatusBar.Panels[PANEL_HINT].text := Format(sFnd09, [PatternPos, NumberFoundItems]);
          if IsRecordingMacro then
             AddMacroEditCommand( ecFindText );
      end
      else begin
          Form_Main.StatusBar.Panels[PANEL_HINT].Text := sFnd10;
          if not (UserBreak or Is_Replacing) then
             App.InfoPopup(Format( sFnd02, [Text_To_Find] ));
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
      if Wrap and (SearchOrigin < SearchOriginNew ) and (NumberFoundItems > 0) then begin    // Wrap será siempre false si Is_ReplacingAll = True
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

  Form_Main.StatusBar.Panels[PANEL_HINT].text := sFnd07;


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
          App.PopupMessage( sFnd08 +#13+ E.Message, mtError, [mbOK] );
          exit;
      end;
    end;

  finally
      if Found then begin
          if not Is_ReplacingAll then
             App.ShowInfoInStatusBar(Format(sFnd09, [PatternPos, NumberFoundItems]));
          if IsRecordingMacro then
             AddMacroEditCommand( ecFindText );
      end
      else begin
          App.ShowInfoInStatusBar(sFnd10);
          if not Is_Replacing then
             DoMessageBox(Format( sFnd02, [Text_To_Find] ), sFnd12, 0, handle);
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
          case messagedlg( sFnd01,
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
                   if Editor.NNodeObj <> nil then
                      App.ChangeInEditor(Editor);
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

  txtMessage:= Format( sFnd11, [ReplaceCnt] );
  Form_Main.StatusBar.Panels[PANEL_HINT].Text := txtMessage;
  if ( ReplaceCnt > 0 ) then begin
     if ReplaceAll then begin
        Editor.SelLength:= 0;
        DoMessageBox(txtMessage, sFnd12, 0, handle);
     end;
     App.ActivateFolder(ActiveFolder);
  end
  else
      if not SelectedTextToReplace then begin
         DoMessageBox(Format( sFnd02, [Text_To_Find] ), sFnd12, 0, handle);
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
    ResultsSearch:= TSimpleObjList<TResultSearch>.Create;
    WordInResultSearch:= TWordInResultSearch.Create;
    LastWordFollowed.iResults:= -1;

end.
