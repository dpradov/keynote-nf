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
    FoundClosingTag: boolean;
    SearchInProgress : boolean; // TRUE while searching or replacing
    UserBreak : boolean;

    SearchNode_Text, SearchNode_TextPrev : string;
    StartFolder: TKntFolder;
    StartNode: PVirtualNode;

type
   TDistanceScope = (dsAll, dsSentence, dsParagraph);

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


type
  TTextInterval = record
     PosI:    integer;
     PosF:    integer;
  end;

  TNoteFragments = class
     NumFrag: integer;
     Fragments: Array of TTextInterval;
  end;
  TNoteFragmentsList = TSimpleObjList<TNoteFragments>;

procedure FreeFragments(var FoundNodes: TNodeList; var FragmentsInNodes: TNoteFragmentsList);



var
  FoundNodes: TNodeList;
  FragmentsInNodes: TNoteFragmentsList;
  Fragments_LastNodeProcessed: PVirtualNode;
  Fragments_iLastNodeProcessed: integer;



procedure DoFindNext;
procedure RunFinder;
function RunFindNext (Is_ReplacingAll: Boolean= False): boolean;
function RunFindAllEx (myFindOptions : TFindOptions; ApplyFilter, TreeFilter: Boolean;
                       OnlyGetFragmentsInfo: boolean = False;
                       OnlyNode: PVirtualNode= nil;
                       FolderToUse: TKntFolder = nil;
                       TextPlainToUse: string = ''): boolean;
procedure PreprocessTextPattern (var myFindOptions : TFindOptions);
procedure RunReplace;
procedure RunReplaceNext;

function FindTag(const Substr: string; const Str: String; SearchOrigin: integer; LenStr: integer = -1): integer;
procedure ReplaceInNotes(const ToReplace, ReplaceWith: string; TagSearch: boolean; MatchCase: boolean; WholeWordsOnly: boolean);

procedure ClearFindAllResults;
procedure UpdateFindAllResultsWidth;
procedure FindResultsToEditor( const SelectedOnly : boolean );

procedure FindAllResults_SelectMatch (Editor: TRxRichEdit; Prev: boolean);
procedure FindAllResults_OnSelectionChange(Editor: TRxRichEdit);
procedure FindAllResults_OnKeyDown (Editor: TRxRichEdit; Key: Word);
procedure FindAllResults_RightClick (CharIndex: integer);

function GetTextScope(const Text: string; Scope: TDistanceScope; PosInsideScope: integer; var pL_Scope, pR_Scope: integer; pLmin: integer): string;

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


  TSearchTagInfo = record
     TagName: String;
     Open:  boolean;
     Pos:   integer;
     sI:    integer;           // scope - Initial position
     sF:    integer;           // scope - Final position
     sFtag: integer;           // original scope - Final position
  end;

  TSearchTagsInfo =  Array of TSearchTagInfo;


  TTagsORinfo = record
     TagsInfo:   TSearchTagsInfo;
     Next:       TTextInterval;   // Considering the "sum" of all OR tags
     SecondNext: TTextInterval;   //      ,,
  end;

  TSearchTagInclInfo = record
     TagsOR:     TNoteTagArray;
     InMetadata: boolean;         // Can we ignore it in the in-text search since it is located in the note's metadata?
     TagsORinfo: TTagsORinfo;
  end;

  TSearchTagsInclInfo = Array of TSearchTagInclInfo;

var
  LastWordFollowed: TLastWordFollowed;
  WordInResultSearch: TWordInResultSearch;

  SearchTagsInclInfo: TSearchTagsInclInfo;
  SearchTagsExclInfo: TTagsORinfo;

  NextTextIntervalToConsider: TTextInterval;
  SecondNextTextIntervalToConsider: TTextInterval;

  ExcludedUntilTheEnd: boolean;



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


procedure FreeFragments(var FoundNodes: TNodeList; var FragmentsInNodes: TNoteFragmentsList);
var
  i: integer;
begin
   if FoundNodes <> nil then
      FreeAndNil(FoundNodes);

   if FragmentsInNodes <> nil then begin
      for i:= 0 to FragmentsInNodes.Count-1 do
         FragmentsInNodes[i].Free;
      FreeAndNil(FragmentsInNodes);
   end;
end;


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
        App.ErrorPopup(E);
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
    App.InfoPopup( GetRS(sFnd05) );
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

// IMPORTANT: Substr is assumed to be in upper case and starts with #. Str is also is assumed to be in upper case

function FindTag(const Substr: string; const Str: String; SearchOrigin: integer; LenStr: integer = -1): integer;
var
   p, pF: integer;
   Found, IncludeClosingTags: boolean;
   LenPattern: integer;
begin
   // If a tag is being renamed or removed (replacing it with ""), FindTag must also locate any block closures associated with that tag.
   // Ex: #Tag --> ##Tag.
   IncludeClosingTags:= Is_Replacing;

   LenPattern:= Length(Substr);
   if LenStr < 0 then
      LenStr:= Length(Str);

   Found:= False;
   p:= SearchOrigin -1;
   repeat
      p:= PosPChar(Substr, Str, p + 1);
      pF:= p + LenPattern;
      if (p > 0) and
          (IncludeClosingTags or  ((p = 1) or (Str[p-1] <> '#')) ) and
          ((pF > LenStr) or (Str[pF]  in TagCharsDelimiters)) then begin
         Found:= True;
      end;

   until Found or (p <= 0);

   FoundClosingTag:=  (p > 1) and IncludeClosingTags and (Str[p-1] = '#');

   Result:= p;
end;


function FindPattern (const Substr: string; const Str: String; SearchOrigin: integer; var SizeInternalHiddenText: integer;
                      IgnoreKNTHiddenMarks: boolean;  ConsiderAllTextInStr: boolean): integer;
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

  NOTE: IgnoreKNTHiddenMarks=False => The search will be performed without special treatment of possible hidden characters, if any.
       This allows you to locate these characters, if you are interested, but it can also speed up the search when you know that
       there cannot be hidden characters when searching within note names or in plain, non-RTF notes.
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

    LenPattern:= Length(Substr);
    if ConsiderAllTextInStr then
       LenStr := Length(Str)
    else begin
       if NextTextIntervalToConsider.PosI > 0 then
          LenStr := NextTextIntervalToConsider.PosF
       else
          LenStr := Length(PChar(Str));
    end;

    if (Substr = '') or (LenStr=0) then Exit(0);

    if FindOptions.TagSearch then begin
       Result:= FindTag(Substr, Str, SearchOrigin, LenStr);
       exit;
    end;


    pH:= 0;
    if IgnoreKNTHiddenMarks then begin
       pH:= PosPChar(KNT_RTF_HIDDEN_MARK_L_CHAR, Str, SearchOrigin);
       PPattern:=  @Substr[1];
    end;

    repeat
      p:= PosPChar(Substr, Str, SearchOrigin);
      
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
              pH:= PosPChar(KNT_RTF_HIDDEN_MARK_L_CHAR, Str, posIT +1);

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

    if Pattern = '' then begin
       if pPos >= 0 then begin
          len:= 75;
          if (pR_Extract > 0) and (pPos + len > pR_Extract) then
              len:= pR_Extract - pPos;
          Result:= Trim(Copy(Str, pPos+1, len));
          Result:= StringReplace(Result, #13, '|', []);
          pR:= Pos(#13, Result, 1);
          if pR > 0 then
             Result:= Copy(Result, 1, pR-1);
       end;
       exit;
    end;


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

  if myFindOptions.SearchMode <> smPhrase then begin
     s := '';
     p := 0;
     prevch := #0;
     InQuotes := false;

     Delims:= [#32, CHR_SEP_SENTENCE, CHR_SEP_PARAGRAPH];
     aStr:= SearchPattern;

     aStr:= StringReplace(aStr, FIND_PARAGRAPH_SCOPE_SEP, CHR_SEP_PARAGRAPH, [rfReplaceAll]);
     aStr:= StringReplace(aStr, FIND_SENTENCE_SCOPE_SEP,  CHR_SEP_SENTENCE, [rfReplaceAll]);
     L := length( aStr );

     if (aStr[1] = CHR_SEP_PARAGRAPH) or (aStr[1] = CHR_SEP_SENTENCE) or
        (aStr[L] = CHR_SEP_PARAGRAPH) or (aStr[L] = CHR_SEP_SENTENCE) then begin
         myFindOptions.SearchMode := smPhrase;
     end;
  end;

  if myFindOptions.SearchMode = smPhrase then begin
     s:= SearchPattern;
     AddWord;
     exit;
  end;


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
     pR_Scope:= Text.Length + 1;

  if Scope = dsSentence then begin
     for p := PosInsideScope to pR_Scope-1 do
       if ((Text[p] in ['.','¿','?','¡','!']) and (Text[p+1] in [' ', #9])) or (Text[p] = #7)   then
          break;
     if p < pR_Scope then
        pR_Scope:= p;
  end;


  pL_Scope := NFromLastCharPos(Text, #13, 1, PosInsideScope);
  if pL_Scope <= 0 then
     pL_Scope:= 1;
  if pL_Scope < pLmin then
     pL_Scope:= pLmin
  else
  if Text[pL_Scope] = #13 then
     inc(pL_Scope);                // do not include left #13

  if Scope = dsSentence then begin
     for p := PosInsideScope downto pL_Scope do
       if ((Text[p] in ['.','¿','?','¡','!']) and (Text[p+1] in [' ', #9])) or (Text[p] = #7)   then
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

function RunFindAllEx (myFindOptions : TFindOptions; ApplyFilter, TreeFilter: Boolean;
                       OnlyGetFragmentsInfo: boolean = False;
                       OnlyNode: PVirtualNode = nil;
                       FolderToUse: TKntFolder = nil;
                       TextPlainToUse: string = ''): boolean;
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
  TextPlainInUpperCase: string;                  // TextPlain in upper case, to be used if looking for Tags
  TextToFind, PatternInPos1, PatternInPosN: string;
  SizeInternalHiddenText, SizeInternalHiddenTextInPos1: integer;
  str, s, s2, path, strLocationMatch, strNodeFontSize, strNumberingFontSize, strBgColor: string;
  NodeNameInSearch: string;
  widthTwips: integer;
  RTFAux : TAuxRichEdit;
  TreeUI: TKntTreeUI;
  TV: TVTree;
  SearchingByDates: boolean;
  LastLocationAdded_Str: String;
  LastLocationAdded_NodeName: boolean;
  SearchingInNonRTFText: boolean;
  ClearRTFAux: boolean;
  HideEncrypted: boolean;

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

  SearchTagsInText, SearchTagsInMetadata, UseInheritedTags: boolean;
  UsingTags: boolean;
  ConsiderNode, ConsiderAllTextInNode: boolean;
  IgnoreWithTagsInText: boolean;
  IgnoreWithoutTagsInText: boolean;
  InheritedTags: TNoteTagArray;
  iNode: integer;

  FindAllSearch: boolean;          // Excecuted with Find All - Search (in contrast with TreeFilter or OnlyGetIntervalFragments)

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
                    WordsPos[i+N]:= -1;
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
         NoteFrags: TNoteFragments;

       begin

          if OnlyGetFragmentsInfo then begin
             if not assigned(myTreeNode) then exit;
             if Fragments_LastNodeProcessed <> myTreeNode then begin
                if Fragments_LastNodeProcessed <> nil then begin
                   NoteFrags:= FragmentsInNodes[Fragments_iLastNodeProcessed];
                   SetLength(NoteFrags.Fragments, NoteFrags.NumFrag);
                end;
                Fragments_iLastNodeProcessed:= FoundNodes.Add(myTreeNode);
                Fragments_LastNodeProcessed := myTreeNode;
                NoteFrags:= TNoteFragments.Create;
                NoteFrags.NumFrag:= 0;
                SetLength(NoteFrags.Fragments, 10);
                FragmentsInNodes.Add(NoteFrags);
             end;
             NoteFrags:= FragmentsInNodes[Fragments_iLastNodeProcessed];
             inc(NoteFrags.NumFrag);
             if NoteFrags.NumFrag > Length(NoteFrags.Fragments) then
                SetLength(NoteFrags.Fragments, Length(NoteFrags.Fragments) + 10);

             NoteFrags.Fragments[NoteFrags.NumFrag-1].PosI:= PatternPos;
             NoteFrags.Fragments[NoteFrags.NumFrag-1].PosF:= pR_Extract;
             exit;
          end;

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

          //Form_Main.LblFindAllNumResults.Caption:= '  ' + MatchCount.ToString + GetRS(sFnd13);
       end;

       procedure GetCurrentNode;
       var
          path: string;
       begin
          if OnlyNode <> nil then
             myTreeNode := OnlyNode
          else
             myTreeNode := TreeUI.FocusedNode;

          TreeNodeToSearchIn:= myTreeNode;

          if (TreeNodeToSearchIn <> nil) and KntTreeOptions.ShowFullPathSearch then begin
             path:= TreeUI.GetNodePath( TreeNodeToSearchIn, KntTreeOptions.NodeDelimiter, true );
             TreeNodeToSearchIn_AncestorPathLen:= Length(path) + Length(myFolder.Name) + 1 - Length(TreeUI.GetNNode(TreeNodeToSearchIn).NoteName);
          end;

          if UseInheritedTags then
             TreeUI.PopulateInheritedTags;
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
               myTreeNode := TV.GetNextNotHidden(myTreeNode, not FindAllSearch);

             myTreeNode:= EnsureCheckMode(myTreeNode);
          end;

          if assigned(myTreeNode) and UseInheritedTags then
             TreeUI.PopulateInheritedTags;
       end;

       procedure GetNextNode;
       begin
          if (not assigned(myTreeNode)) then exit;

          if myFindOptions.HiddenNodes then     // [dpv]
             myTreeNode := TV.GetNext(myTreeNode)
          else
             myTreeNode := TV.GetNextNotHidden(myTreeNode, not FindAllSearch);

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
             repeat
                inc( noteidx );
                if ( noteidx >= ActiveFile.FolderCount ) then
                   FindDone := true;

                if not FindDone then begin
                   myFolder := TKntFolder(Form_Main.Pages.Pages[noteidx].PrimaryObject);
                   if not OnlyGetFragmentsInfo or (myFolder.Info = 1) then break;
                end;
             until FindDone;

             if not FindDone then begin
                //myFolder := TKntFolder(Form_Main.Pages.Pages[noteidx].PrimaryObject);
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


       function CheckFolded(SearchingInNodeName: boolean;
                            PatternPos1: integer;
                            PatternPosN: integer = -1): boolean;
       var
         pI, pF, pIcontent: integer;
         InFolded: boolean;
         FirstWord: string;

         function CheckPosition(Position: integer): boolean;
         var
            p,p1,p2: integer;
         begin
            Result:= False;

            // Position starts at zero

            if (pI = -99) or not ((Position >= pI) and (Position <= pF)) then begin
               if not ConsiderAllTextInNode then
                  // It will look for ..#$13 instead of ...#$13, in case the temporary break (#0) added from FindPatternInTextFragments might override the final string of a folded block.
                  InFolded:= PositionInFoldedBlock_FindAll(TextPlain, Position, pI, pF)
               else
                  InFolded:= PositionInFoldedBlock(TextPlain, Position, nil, pI, pF);
               inc(pI);
               inc(pF);
            end;

            if not InFolded then
               pI:= -99;

            if (InFolded and (myFindOptions.FoldedMode = sfExcludeFolded)) or
               (not InFolded and (myFindOptions.FoldedMode = sfOnlyFolded)) then
               exit;

            if (InFolded and (myFindOptions.FoldedMode = sfExcludeTaggedFolded)) then begin
               if IsTaggedFolded(pI, pF, TextPlain) then          // "Tagged" folded: a folded block that begins with a tag
                  exit;
            end;

            Result:= True;
         end;

       begin
           if SearchingInNodeName or (myFindOptions.FoldedMode = sfAll) or (PatternPos1 < 0) then exit(True);

           Result:= False;
           pI:= -99;
           pF:= integer.MaxValue;
           if not CheckPosition(PatternPos1) then
              exit;
           if (PatternPosN >= 0) and not CheckPosition(PatternPosN) then
              exit;

           Result:= True;
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


       {
        Identify the start and end of a new text fragment in the note that meets the Tag criteria (With and/or Without)
        By default, the FindPatternInText method searches the entire TextToFind, advancing through SearchOrigin.
        We can replace the TextPlain search with the fragment to search for. Depending on the size of the fragment and the note,
        it may be convenient to directly use a copy of that fragment, but in general, it seems more convenient to use SearchOrigin,
        as well as checking that the possible pattern located is within the fragment.
        In SearchTagsInclInfo, we will have already marked with InMetadata = True those tags in the "With tags" section that have
        been satisfied by the note's metadata. The remaining missing tags must be searched for in the text. We must keep in mind that
        there may be tags that respond to an OR condition.

        If tags must be excluded ("Without tags"), then we must ensure that they are not present in the fragments.
        These tags and their progress will be recorded in SearchTagsExclInfo.


        When checking whether a text fragment satisfies the tag conditions, it is important to keep in mind that each tag defines a scope
        (an interval, with a lower bound and an upper bound for the text it affects) and that this will also depend on whether the tag has been
        defined "openly" (e.g., #Critical: ) or not, affecting in this last case only the paragraph in which it is located.
        We will need to look for the "intersection" of these scopes in the case of the "With tags" condition. The specific scopes of each
        located tag will only need to be identified if there is a possibility of meeting the condition.
        For example, if the text must meet the tags Tag1 and Tag2, and we locate Tag1, we will not need to identify its scope until we
        locate at least one instance of Tag2.

        When looking at the intersection between the scopes, those defined by OR tags (in TagsOR) will be added together beforehand,
        before carrying out the analysis.



        It seems reasonable that when we decide to exclude text with a certain tag while requiring the presence of other tags, we should
        most likely be indicating tags to be excluded that are primarily used in open mode. That is, we will want to discard chunks of
        text marked in this way.
        If we only ask to exclude certain tags but don't require identifying tags to be considered, then it is more likely that the tag to
        be excluded will also appear in non-open mode.

        If we only ask to exclude but not to include tags, it is clear that we must directly locate all possible occurrences of those tags
        to be excluded, processing all the previous intervals we identify that are free of them.

        If we need to consider both tags to be included and those to be excluded, based on the initial assumption, it seems more convenient
        to start by searching for the possible presence of the tags to be excluded, as they can cancel out large fragments.


        The following is only an approximation of the necessary steps:


        0->  Get the next closest contiguous range to exclude:
             Loop through each exclusion tag, acting where no instance has been located.
             - Find the next instance, noting its location, as well as whether it is used in "open" mode or not.

             Determine the ranges for each instance that are not yet available.
              - Is it not "open" and do we have a scope already identified, of a non-open type, whose range includes the position of this instance?
               Yes -> this will have the same scope range -> we can ignore it.
               No -> Identify the scope by searching for it in the text.

             "Add" (overlap) the identified intervals, obtaining the closest one.
              - The "addition" can be reflected over any of the participating labels, deleting the other intervals.
              - Intervals that do not overlap will remain independent, linked to one of their labels.

              In principle, the next exclusive interval to consider will be the interval resulting from the "addition" with the closest lower endpoint.
              Before using it to "cut" the possible intervals to include (according to "With tags"), we must look for the next exclusive interval.
              If we don't find it, or it's not contiguous, we will use the one we already have and keep the new one for another iteration.
              If we find it and it's contiguous, we will "add" it to the previous one and proceed as before, looking for another possible new interval
              until we don't find it or it's not contiguous.


              => Next contiguous interval to exclude (NextExcludingTextInterval)
              => Another possible subsequent, non-contiguous interval (SecondNextExclTextInterval)


        1-> Search for the next instance of each required tag (for which we don't have a pre-calculated range available)
            noting its location, as well as whether it is used in "open" mode or not.
              In the case of OR tags with an AND operand (included in SearchTagsInclInfo.TagsOR), the next instance will be searched for individually,
              but they will be treated together as if they were all the same tag, adding their possible ranges.

        2-> After each located instance, check: Is there already an instance of the other AND tags available?
           No => Search for an instance of another tag for which one is not yet available.
                 Is an instance found?
                 No -> We can give up; no new fragments can be found.
                 Yes -> Iterate in 2.

           Yes => Identify the range of all those for which we haven't yet determined it.
               -> Subtract from each interval the possible intersection with the closest adjacent interval to exclude (identified as 0.)
                     This subtraction can yield up to two intervals for each tag:
                     TagIncl        TagExcl     =>
                     [.                             [.]
                      .              [.                 <----------- 'a'
                      .               .
                      .               .]                <----------- 'b'
                      .                             [.
                      .]                             .] <----------- 'c'

                   If this happens, we must treat each set of intervals separately, starting with those above 'a'.
                   Before treating those below 'b', we must ensure that there is no excludable interval before 'c'. At 0, we will have identified
                   a next contiguous interval to exclude (NextExcludingTextInterval) as well as possibly another non-contiguous interval (SecondNextExclTextInterval).

              -> Intersect the intervals within their respective scopes
              -> Thus, obtain a possible valid text fragment ([I, F], which will satisfy the "With and/or Without" condition.
              -> For each AND tag, there may be a remaining interval to continue considering in further iterations. This remainder will be the part
                 of its interval that extends beyond F.

               *1 For each identified tag, we must always remember the original start of its scope, in addition to the possible
                  start and end of the interval resulting from the intersection.
                  This will make it possible to optimize the intersection of tags that are not in "open" mode:
                   If we have identified the scope of a *non* open tag (let it be [i, f]), any other *non* open tag located at position P will be
                   known not to intersect if P < i, since P must necessarily be located within the scope of its tag, or in other words, the scope
                   will not extend beyond P.


              Have we obtained a valid text fragment? [I, F], or on the contrary, there was no intersection between all the available
              intervals for each AND tag.

               Yes -> Have we already identified a valid previous fragment?
                      No -> We annotate this fragment and search for a possible next fragment --> 1.
                      Yes -> Are these two fragments contiguous?
                           Yes -> Merge into one and search for a possible next fragment --> 1.
                           No  -> Process the previous fragment (perform the necessary text searches on it)
                               -> Keep only the new fragment and search for a possible next fragment --> 1.

                No -> Have we reached the end of the text?
                      Yes -> Process the possible previous fragment we have ((perform the necessary text searches on it)). Exit.
                       No -> - We will keep the most advanced interval, and any other intervals that intersect with it.
                             - From each interval we keep, we will remove the initial chunks that do not intersect.
                             - We will iterate over 1 again to look for new instances of those tags for which we have not kept any intervals.

                            Eg:
                                 Tag1       Tag2     Tag3     Tag4
                                 [.
                                  *]                          [*
                                                               .]

                                                     [.
                                             [*       *]             - - - - - - - - - - -  "x"
                                              .
                                              .]

                            In this example, the most advanced interval is the one corresponding to Tag2, which we keep.
                            That interval intersects Tag3, which we also keep. But neither Tag1 nor Tag4 intersect Tag2, 
                            so we discard their available instances and intervals.

                            We'll calculate "x" as the most advanced lower bound. We'll cut all the intervals we keep with this value.
                            From Tag2, we'll keep its entire interval.
                            From Tag3, we'll keep only "x" and above.


           We should try to optimize some of the following specific cases:
            - There may be tags to exclude, and these may be open-ended. For example, if, starting from a position, we find something
              like Tag5: (and Tag5 is to be excluded), it could be that all or a large portion of the rest of the text is being excluded.

            - We can try to search for the presence of two or more tags to be included, and what we find are many occurrences of one of them,
              but not of the others. If they are not "open" tags, we should be able to cancel some of the instances found.

            Trying to avoid copying fragments to limit searches, instead, although it's not in principle a "recommended" practice, I'll follow 
            the same principle I used with RemoveKNTHiddenCharactersInRTF, inserting #0 in those positions where I want to "cut" the search,
            then restoring the correct character.
            This will require replacing the use of Pos(...) with a method similar to PosPAnsiChar(...) but for PChar (wideChar) -> PosPChar(...)
            This null-terminator convention is consistent throughout the Windows API, regardless of whether we use ANSI functions ('A' suffix)
            or Unicode functions ('W' suffix). Delphi itself adds a null character to the end of string content, and indicates in the help
            regarding the internal format of "Long String Types":
            "The NULL character at the end of a string memory block is automatically maintained by the compiler and the built-in
            string handling routines. This makes it possible to typecast a string directly to a null-terminated string""
                This mechanism, where I modify (insert #0) the string indirectly using pointers, prevents Delphi's shared string protection
            mechanism from being triggered when attempting to modify a string, since it isn't done through the language's standard operators
            and methods (such as assignment or string manipulation). Therefore, it prevents Delphi from making a copy of the string, which
            improves performance. But it also forces us to take responsibility for ensuring that we don't corrupt a string that could be shared by other variables.
       }


       procedure IdentifyNextORTextFrag (var TagsORInfo: TTagsORinfo);
       var
          i, j: integer;
          p: integer;
          pI, pF: integer;
          SelectedInterval: integer;
          NoMoreIntervals: boolean;
          NewTextInterval: TTextInterval;
          TagsInfo: TSearchTagsInfo;
          Pointer: PChar;
          CharBAK: Char;


          procedure IdentifyNotOpenTagsInTheSameParag(FromI: Integer);
          var
             j: integer;
          begin
             for j:= FromI + 1 to High(TagsInfo) do begin
                 if TagsInfo[j].Open then continue;
                 if (TagsInfo[j].sI < 0) and (TagsInfo[j].Pos >= TagsInfo[FromI].sI) and
                                             (TagsInfo[j].Pos <= TagsInfo[FromI].sF)      then begin
                     TagsInfo[j].sI:= 0;    // => Interval already processed or to be ignored
                     TagsInfo[j].sFtag:= TagsInfo[FromI].sFtag;
                 end;
             end
          end;

       begin

           TagsInfo:= TagsORInfo.TagsInfo;

         { Although .sF will initially capture the end of the scope of a found tag, we can modify that value
           during the interval "sum" operation. Therefore, we will also save the initial scope in .sFtag, which we will not modify there.
           This will allow us to use it to perform subsequent searches for occurrences of that tag, instead of having to search from the
           last position found (.Pos). In principle, we could find a tag within the previous scope, but we could ignore it.
           Eg:   #ToDO:
                 ...
                 ... #ToDo

                ##ToDO

          And even, theoretically, something like this would be possible:
                #ToDO:
                 ...
                 ... #ToDO .. ##ToDO ....

           In this case, the first tag, in "open" mode, would end before the second, which would end at the end of the paragraph.
           But this situation doesn't make any sense. It's better to ignore it and be able to speed up searches, starting from more advanced positions. }

          TagsORInfo.Next.PosI:= 0;

          if TagsORInfo.SecondNext.PosI > 0 then begin
             TagsORInfo.Next:= TagsORInfo.SecondNext;
             TagsORInfo.SecondNext.PosI:= 0;
          end;

          if ExcludedUntilTheEnd then begin
             Pointer:= PChar(@TextPlainInUpperCase[1]);
             CharBAK:= Pointer[SearchTagsExclInfo.Next.PosI - 1];
             Pointer[SearchTagsExclInfo.Next.PosI -1] := #0;                 // Limit search of Tags to the begin of the excluded fragment
          end;

          NewTextInterval.PosI:= 0;
          repeat

             // Loop through each tag, acting where no instance is located

             for i:= 0 to High(TagsInfo) do begin
                if (TagsInfo[i].Pos = -1) or (TagsInfo[i].sI >= 1) then continue;

                p:= -1;
                if (TagsInfo[i].sFtag < Length(TextPlainInUpperCase)) and (not ExcludedUntilTheEnd or (TagsInfo[i].sFtag + 1 < SearchTagsExclInfo.Next.PosI) ) then
                   p:= FindTag(TagsInfo[i].TagName, TextPlainInUpperCase, TagsInfo[i].sFtag + 1);
                if (p >= 1) and (not ExcludedUntilTheEnd or (p < SearchTagsExclInfo.Next.PosI)) then begin
                   TagsInfo[i].Open:= OpenTagsConcatenated(p, Length(TagsInfo[i].TagName), TextPlainInUpperCase);
                   TagsInfo[i].Pos:= p;
                   TagsInfo[i].sI := -1;        // -1 => Pending to identify the interval
                end
                else begin
                   TagsInfo[i].Pos:= -1;        // -1 => Don't look for this tag in this note anymore.
                   TagsInfo[i].sI := 0;         //  0 => Interval already processed or to be ignored
                end;
             end;

             { We will follow the following agreement:
                  TagsInfo[x].sI = < 0 => Pending to identify the interval
                  TagsInfo[x].sI = 0   => Interval already processed or to be ignored

               GetBlockAtPosition(TextPlainInUpperCase, TagsInfo[i].Pos-1, ...);  Pos -1 because that method expects positions starting at 0, as is the case with Editor.SelStart
             }


             // Determine the intervals for each instance that are not yet available

             NoMoreIntervals:= True;

             for i:= 0 to High(TagsInfo) do begin
                if TagsInfo[i].Pos = -1 then continue;

                NoMoreIntervals:= False;
                if (not TagsInfo[i].Open) and (TagsInfo[i].sI >= 1) then
                   IdentifyNotOpenTagsInTheSameParag(i);
             end;


             if not NoMoreIntervals then begin

                for i:= 0 to High(TagsInfo) do begin
                   if TagsInfo[i].sI < 0 then
                      if GetBlockAtPosition(TextPlainInUpperCase, TagsInfo[i].Pos-1, nil, pI, pF, False, TagsInfo[i].TagName, '', '', True, False) then begin
                         TagsInfo[i].sI:= pI + 1;
                         TagsInfo[i].sF:= pF + 1;
                         TagsInfo[i].sFtag:= pF + 1;

                         if not TagsInfo[i].Open then
                            IdentifyNotOpenTagsInTheSameParag(i);
                      end
                      else
                         TagsInfo[i].Pos:= -1;      // Don't look for this tag in this note anymore.
                end;


                // "Add" (overlap) the identified intervals, obtaining the closest one.

                for i:= 0 to High(TagsInfo) do begin
                   if TagsInfo[i].sI <= 0 then continue;

                   for j:= i + 1 to High(TagsInfo) do begin
                      if TagsInfo[j].sI <= 0 then continue;
                      if not ( (TagsInfo[i].sF < TagsInfo[j].sI) or (TagsInfo[i].sI > TagsInfo[j].sF) ) then begin
                         if TagsInfo[j].sI < TagsInfo[i].sI then
                            TagsInfo[i].sI := TagsInfo[j].sI;
                         if TagsInfo[j].sF > TagsInfo[i].sF then
                            TagsInfo[i].sF := TagsInfo[j].sF;

                         TagsInfo[j].sI:= 0;       // Already processed
                      end;

                   end;
                end;

                NewTextInterval.PosI:= Integer.MaxValue;
                SelectedInterval:= -1;
                for i:= 0 to High(TagsInfo) do begin
                   if TagsInfo[i].sI <= 0 then continue;

                   if TagsInfo[i].sI < NewTextInterval.PosI then begin
                      NewTextInterval.PosI:= TagsInfo[i].sI;
                      NewTextInterval.PosF:= TagsInfo[i].sF;
                      SelectedInterval:= i;
                   end;
                end;


                if SelectedInterval >= 0 then begin
                   TagsInfo[SelectedInterval].sI:= 0;

                   if (TagsORInfo.Next.PosI = 0) then
                      TagsORInfo.Next:= NewTextInterval

                   else
                   if not ( (TagsORInfo.Next.PosF < NewTextInterval.PosI) or (TagsORInfo.Next.PosI > NewTextInterval.PosF) ) then begin
                      if NewTextInterval.PosI < TagsORInfo.Next.PosI then
                         TagsORInfo.Next.PosI := NewTextInterval.PosI;
                      if NewTextInterval.PosF > TagsORInfo.Next.PosF then
                         TagsORInfo.Next.PosF := NewTextInterval.PosF;
                   end
                   else begin
                      TagsORInfo.SecondNext:= NewTextInterval;
                      break;
                   end;
                end;

             end;

          until NoMoreIntervals;


          if ExcludedUntilTheEnd then
             Pointer[SearchTagsExclInfo.Next.PosI -1] := CharBAK;

       end;



       procedure IdentifyNextExcludingTextFrag;
       begin
          if not IgnoreWithoutTagsInText then
             IdentifyNextORTextFrag(SearchTagsExclInfo);
       end;



       procedure IdentifyNextTextFragWithRequiredTags;
       var
          i, j: integer;
          p: integer;
          NewTextInterval, AndInterval: TTextInterval;
          NoMoreIntervals: boolean;
          TagsInfo: TSearchTagsInfo;

       begin
          NextTextIntervalToConsider.PosI:= 0;

          repeat

             // Process the OR intervals included in each AND operand, obtaining a new one where necessary
             NewTextInterval.PosI:= 0;
             NoMoreIntervals:= False;
             for i:= 0 to High(SearchTagsInclInfo) do begin
                if (SearchTagsInclInfo[i].InMetadata) then continue;
                if (SearchTagsInclInfo[i].TagsORinfo.Next.PosI = 0) or
                   (SearchTagsInclInfo[i].TagsORinfo.Next.PosF < NextTextIntervalToConsider.PosF) then
                  IdentifyNextORTextFrag(SearchTagsInclInfo[i].TagsORinfo);

                if SearchTagsInclInfo[i].TagsORinfo.Next.PosI = 0 then begin
                   NoMoreIntervals:= True;
                   break;   // We can give up; no new fragments can be found for this required tag (or set of tags -OR)
                end;
             end;

             if (not NoMoreIntervals) then begin
                // Calculate the intersection between the AND components
                // We'll discard everything before the intersection. We'll keep everything after it and process it in further iterations.

                NewTextInterval.PosI:= 1;
                NewTextInterval.PosF:= Integer.MaxValue;

                for i:= 0 to High(SearchTagsInclInfo) do begin
                   if (SearchTagsInclInfo[i].InMetadata) then continue;
                   AndInterval:= SearchTagsInclInfo[i].TagsORinfo.Next;
                   if AndInterval.PosI > NewTextInterval.PosI then
                      NewTextInterval.PosI:= AndInterval.PosI;

                   if AndInterval.PosF < NewTextInterval.PosF then
                      NewTextInterval.PosF:= AndInterval.PosF;
                end;

                // Keep, in each AND operand (and in each internal OR operand), only the chunks that may lie beyond
                // the end of the intersection. (or the lowest posF, if there is no intersection between the intervals)
                for i:= 0 to High(SearchTagsInclInfo) do begin
                   if (SearchTagsInclInfo[i].InMetadata) then continue;
                   AndInterval:= SearchTagsInclInfo[i].TagsORinfo.Next;
                   AndInterval.PosI:= NewTextInterval.PosF;
                   if AndInterval.PosF <= AndInterval.PosI then begin
                      SearchTagsInclInfo[i].TagsORinfo.Next:= SearchTagsInclInfo[i].TagsORinfo.SecondNext;
                      SearchTagsInclInfo[i].TagsORinfo.SecondNext.PosI:= 0;
                   end;

                   TagsInfo:= SearchTagsInclInfo[i].TagsORinfo.TagsInfo;
                   for j:= 0 to High(TagsInfo) do begin
                      if TagsInfo[j].sI = 0 then continue;
                      TagsInfo[j].sI:= NewTextInterval.PosF;
                      if TagsInfo[j].sF <= TagsInfo[j].sI then
                         TagsInfo[j].sI:= 0;
                   end;
                end;


                if NewTextInterval.PosF > NewTextInterval.PosI then begin
                   // We have a valid interval
                   NextTextIntervalToConsider:= NewTextInterval;
                   break;
                end
                else
                   NextTextIntervalToConsider.PosF:= NewTextInterval.PosF;
                 { We need to keep iterating, looking for an interval where the condition is met
                   We don't have a valid interval, but the NextTextIntervalToConsider.PosF value will help us continue searching
                   from that point on. 
                   That is, we should only get new fragments for those operands where we don't have intervals beyond that point. }

             end;

          until NoMoreIntervals;

       end;



       procedure IdentifyNextTextFragment;
       var
          GetNewNextReqInterv: boolean;
       begin

            repeat
               if (NextTextIntervalToConsider.PosF >= Length(TextPlainInUpperCase)) or (NextTextIntervalToConsider.PosI = -1) then begin
                  NextTextIntervalToConsider.PosI:= -1;
                  exit;
               end;

               GetNewNextReqInterv:= True;
               if (SecondNextTextIntervalToConsider.PosI > 0) then begin
                  NextTextIntervalToConsider:= SecondNextTextIntervalToConsider;
                  SecondNextTextIntervalToConsider.PosI:= 0;
                  GetNewNextReqInterv:= False;
               end;

               if not (ExcludedUntilTheEnd or (SearchTagsExclInfo.Next.PosI = -1)) and
                  ((SearchTagsExclInfo.Next.PosI = 0) or (SearchTagsExclInfo.Next.PosF < NextTextIntervalToConsider.PosF)) then begin
                  IdentifyNextExcludingTextFrag;
                  if SearchTagsExclInfo.Next.PosI = 0 then
                     SearchTagsExclInfo.Next.PosI:= -1
                  else
                  if SearchTagsExclInfo.Next.PosF + 2 >= Length(TextPlainInUpperCase) then  // + 2: In anticipation of a closing ##Tag with up to one space and line break
                     ExcludedUntilTheEnd:= True;
               end;

               if GetNewNextReqInterv then begin
                  if IgnoreWithTagsInText then begin
                     NextTextIntervalToConsider.PosI:= NextTextIntervalToConsider.PosF + 1;
                     NextTextIntervalToConsider.PosF:= Length(TextPlainInUpperCase);
                  end
                  else
                     IdentifyNextTextFragWithRequiredTags;

                  if NextTextIntervalToConsider.PosI = 0 then begin
                     NextTextIntervalToConsider.PosI:= -1;
                     exit;
                  end;
               end;


               if (SearchTagsExclInfo.Next.PosI > 0) then begin
                  if not ( (SearchTagsExclInfo.Next.PosF < NextTextIntervalToConsider.PosI) or (SearchTagsExclInfo.Next.PosI > NextTextIntervalToConsider.PosF) ) then begin
                     { There is an intersection between the exclusion interval and the interval to be considered.
                       Leave the usable remainder, if any, beyond the interval to be excluded in SecondNextTextIntervalToConsider, 
                       and the interval preceding the interval to be excluded, if any, in NextTextIntervalToConsider. }
                     SecondNextTextIntervalToConsider:= NextTextIntervalToConsider;
                     NextTextIntervalToConsider.PosF:= SearchTagsExclInfo.Next.PosI -1;
                     SecondNextTextIntervalToConsider.PosI:= SearchTagsExclInfo.Next.PosF +1;
                     if (SecondNextTextIntervalToConsider.PosI < Length(TextPlainInUpperCase)) and
                        (TextPlainInUpperCase[SecondNextTextIntervalToConsider.PosI] in TagCharsDelimiters) then
                        inc(SecondNextTextIntervalToConsider.PosI);
                     if SecondNextTextIntervalToConsider.PosF <= SecondNextTextIntervalToConsider.PosI then  // There is no usable remainder afterwards
                        SecondNextTextIntervalToConsider.PosI:= 0;
                     if NextTextIntervalToConsider.PosF <= NextTextIntervalToConsider.PosI then begin        // There is no usable remainder before
                        if (SecondNextTextIntervalToConsider.PosI = 0) and IgnoreWithTagsInText then begin
                           NextTextIntervalToConsider.PosI:= -1;        // If there was no usable remainder afterwards and there wasn't With tags condition we will be using the maximum possible interval, we can exit
                           exit;
                        end
                        else begin
                           NextTextIntervalToConsider.PosI:= 0;         // We'll look for another possible interval. If we've loaded a later usable
                           continue;                                    // remainder on SecondNextTextIntervalToConsider, that's the one we'll use.
                        end;
                     end
                     else
                        exit;                // There is a usable remainder from before -> consume it
                  end
                  else
                  if (NextTextIntervalToConsider.PosI > SearchTagsExclInfo.Next.PosF) then begin
                    { The interval that meets the With conditions is before the last interval to be excluded.
                      We can't just use it; we have to make sure there are no other exclusionary intervals or that this one is later.
                      We can't simply iterate from here to the beginning, because we haven't used this interval to be considered yet.
                      We need to load it first via SecondNextTextIntervalToConsider to reconsider it. }
                      SecondNextTextIntervalToConsider:= NextTextIntervalToConsider;
                      NextTextIntervalToConsider.PosI:= 0;
                      NextTextIntervalToConsider.PosF:= 0;
                      continue;
                  end
                  else
                      exit;        // The interval to be considered is completely ahead of the interval to be excluded -> consume it


               end
               else
                  exit;       // If there are no more exclusive intervals we can directly use the interval to consider that we have located

            until false;

       end;


       procedure FindPatternInText (SearchingInNodeName: boolean; SearchingInNonRTFText: boolean);
       var
          wordidx : integer;
          IgnoreKNTHiddenMarks: boolean;

       begin
          IgnoreKNTHiddenMarks:= not (SearchingInNodeName or SearchingInNonRTFText);
          case SearchModeToApply of
              smPhrase :
                  begin
                      repeat
                         PatternPos:= FindPattern(TextToFind, TextPlain, SearchOrigin+1, SizeInternalHiddenTextInPos1, IgnoreKNTHiddenMarks, ConsiderAllTextInNode) -1;
                         { PatternPos := EditControl.FindText(myFindOptions.Pattern, SearchOrigin, -1, SearchOpts ); }
                         if ( PatternPos >= 0 ) then begin
                             SearchOrigin := PatternPos + PatternLen; // move forward in text
                             if myFindOptions.EmphasizedSearch = esParagraph then
                                Paragraph:= GetTextScope(TextPlain, dsParagraph, PatternPos + 1, pL_DScope, pR_DScope, 1);
                             if CheckFolded(SearchingInNodeName, PatternPos,-1) and
                                CheckEmphasized(myFindOptions.EmphasizedSearch, SearchingInNodeName, TextToFind, PatternPos,'',-1, pL_DScope, pR_DScope) then
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
                             PatternPos:= FindPattern(thisWord, SearchIn, SearchOriginDScope, SizeInternalHiddenText, IgnoreKNTHiddenMarks, ConsiderAllTextInNode) -1;
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
                             not CheckFolded(SearchingInNodeName,PatternPos1, PatternPosN) or
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


       procedure FindPatternInTextFragments (SearchingInNonRTFText: boolean);
       var
          CharBAK: Char;
          P: PChar;
          LenFrag, LenTTF, LenTrimFrag: integer;
       begin
          if TextPlain = '' then exit;

          P:= PChar(@TextPlain[1]);

          repeat
             IdentifyNextTextFragment;
             if NextTextIntervalToConsider.PosI <= 0 then break;

             SearchOrigin := NextTextIntervalToConsider.PosI - 1;      // Start search from the beginning of the fragment

             LenFrag:= NextTextIntervalToConsider.PosF - NextTextIntervalToConsider.PosI + 1;
             LenTTF:= Length(TextToFind);
             LenTrimFrag:= LenFrag;
             if (LenFrag < LenTTF) then
                continue;
             if LenFrag < 20 then
                LenTrimFrag:= Length(Trim(Copy(TextPlain,SearchOrigin,LenFrag)));
             if (LenTrimFrag < LenTTF) or ((TextToFind = '') and (LenTrimFrag = 0)) then continue;

             if TextToFind <> '' then begin
                CharBAK:= P[NextTextIntervalToConsider.PosF];
                P[NextTextIntervalToConsider.PosF] := #0;                 // Limit search to the end of the fragment

                FindPatternInText(False, SearchingInNonRTFText);

                P[NextTextIntervalToConsider.PosF] := CharBAK;
             end
             else
                AddLocation(False, lsNormal, TextToFind, NextTextIntervalToConsider.PosI - 1, nil, -1, NextTextIntervalToConsider.PosF);

          until false;

       end;


  {
   It's important to keep the necessary information in SearchTagsInclInfo and SearchTagsExclInfo to determine whether or
   not to consider a certain fragment of text from a note (when, based on the note's metadata, we haven't been able to
   conclude that we can use the full text).
   We must differentiate between tags whose presence will exclude the text (FindTagsExcl) and those that must be present
   for it to be considered (FindTagsIncl) ("With tags").

   We must keep the latter in mind at all times if we need to search for tags in the note's text.


   However, we can ignore the former if they are satisfied with the note's metadata (if it is indicated that these must also
   be considered). For example, if it is indicated that the Critical and Urgent tags are required and these are located in the
   metadata, we can consider the "With tags" section fulfilled (-> IgnoreWithTagsInText = True).
   This "With tags" section may have been defined as ANY or ALL. If it is defined as ALL, it is possible that one or more of
   the required tags have already been found in the note's metadata, but not all of them; in that case, we can completely ignore
   those already found and focus, when searching for tags within the text, on those not available in the metadata.
   Even in ALL mode, it has been possible to establish criteria that, as operands, constitute OR expressions.

     Eg: Err* Critical -> ej: (Error|Error.High) & Critical

    If we have found "Error" in the metadata, we would only need to find Critical in the text.
    If we have NOT found either "Error" or "Error.High" in the metadata, we can manage the detection of both in a single place, 
    treating either one (Error or Error.High) interchangeably.
  }
       procedure PrepareTagsTextFoundInfo;
       var
          i, j: integer;
          FindTags: TFindTags;
          Strs: TStringList;
       begin
          SearchTagsInclInfo:= nil;
          SearchTagsExclInfo.TagsInfo:= nil;
          IgnoreWithoutTagsInText:= True;

          if not SearchTagsInText then exit;

          Strs:= TStringList.Create;


          FindTags:= myFindOptions.FindTagsIncl;
          if FindTags <> nil then begin
             SetLength(SearchTagsInclInfo, Length(FindTags));
             for i:= 0 to High(FindTags) do begin
                SetLength(SearchTagsInclInfo[i].TagsORinfo.TagsInfo, Length(FindTags[i]));
                SearchTagsInclInfo[i].TagsOR:= FindTags[i];
                for j:= 0 to High(FindTags[i]) do
                    SearchTagsInclInfo[i].TagsORinfo.TagsInfo[j].TagName:= '#' + FindTags[i][j].Name.ToUpper;
             end;
          end;
          if (myFindOptions.FindTagsInclNotReg <> '') then begin
             SplitString(Strs, myFindOptions.FindTagsInclNotReg, ' ', False);
             if not myFindOptions.TagsModeOR then begin
                 j:= Length(FindTags);
                 SetLength(SearchTagsInclInfo, j + Strs.Count);
                 for i:= 0 to Strs.Count - 1 do begin
                    SetLength(SearchTagsInclInfo[i+j].TagsORinfo.TagsInfo, 1);
                    SearchTagsInclInfo[i+j].TagsOR:= nil;
                    SearchTagsInclInfo[i+j].TagsORinfo.TagsInfo[0].TagName:= '#' + Strs[i].ToUpper;
                 end;
             end
             else begin
                if Length(FindTags) = 0 then
                   SetLength(SearchTagsInclInfo, 1);
                j:= Length(SearchTagsInclInfo[0].TagsORinfo.TagsInfo);
                SetLength(SearchTagsInclInfo[0].TagsOR, j + Strs.Count);
                SetLength(SearchTagsInclInfo[0].TagsORinfo.TagsInfo, j + Strs.Count);
                for i:= 0 to Strs.Count - 1 do begin
                   SearchTagsInclInfo[0].TagsOR[i+j]:= nil;
                   SearchTagsInclInfo[0].TagsORinfo.TagsInfo[i+j].TagName:= '#' + Strs[i].ToUpper;
                end;
             end;
          end;


          Strs.Clear;
          FindTags:= myFindOptions.FindTagsExcl;
          if (FindTags <> nil) then begin
             IgnoreWithoutTagsInText:= False;
             SetLength(SearchTagsExclInfo.TagsInfo, Length(FindTags[0]));
             for i:= 0 to High(FindTags[0]) do
                SearchTagsExclInfo.TagsInfo[i].TagName:= '#' + FindTags[0][i].Name.ToUpper;
          end;
          if (myFindOptions.FindTagsExclNotReg <> '') then begin
             IgnoreWithoutTagsInText:= False;
             SplitString(Strs, myFindOptions.FindTagsExclNotReg, ' ', False);
             j:= Length(SearchTagsExclInfo.TagsInfo);
             SetLength(SearchTagsExclInfo.TagsInfo, j + Strs.Count);
             for i:= 0 to Strs.Count - 1 do
                SearchTagsExclInfo.TagsInfo[i+j].TagName:= '#' + Strs[i].ToUpper;
          end;


          Strs.Free;
       end;

       procedure CleanTagsTextFoundInfo;
       var
         i, j: integer;
         NEntry: TNoteEntry;
         NTag: TNoteTag;
       begin
         // myNNode: TNoteNode where we are searching, whose tags (metadata) we have verified
         // InheritedTags: It will have, if applicable, the labels inherited by the node

         // Takes into account IgnoreWithTagsInText.
         // Tags that we can ignore because they may have been located in the metadata will also be flagged (if AND mode)

          NextTextIntervalToConsider.PosI:= 0;
          NextTextIntervalToConsider.PosF:= 0;
          SecondNextTextIntervalToConsider.PosI:= 0;

          SearchTagsExclInfo.Next.PosI:= 0;
          SearchTagsExclInfo.SecondNext.PosI:= 0;
          ExcludedUntilTheEnd:= False;


          if not IgnoreWithTagsInText then begin
             NEntry:= myNNode.Note.Entries[0];               //%%%

             for i:= 0 to High(SearchTagsInclInfo) do begin
                NTag:= nil;
                for j:= 0 to High(SearchTagsInclInfo[i].TagsORinfo.TagsInfo) do begin
                   if SearchTagsInclInfo[i].TagsOR <> nil then
                      NTag:= SearchTagsInclInfo[i].TagsOR[j];
                   with SearchTagsInclInfo[i].TagsORinfo.TagsInfo[j] do begin
                      Open:= False;
                      Pos:= 0;
                      sI:= -1;
                      sFtag:= 0;
                   end;
                   if SearchTagsInMetadata and (NTag <> nil) and (NEntry.HasTag(NTag) or TNoteTagArrayUtils.HasTag(InheritedTags, NTag)) then begin
                      SearchTagsInclInfo[i].InMetadata:= True;
                      break;
                   end;
                end;
             end;
          end;


          for i:= 0 to High(SearchTagsExclInfo.TagsInfo) do begin
             with SearchTagsExclInfo.TagsInfo[i] do begin
                Open:= False;
                Pos:= 0;
                sI:= -1;
                sFtag:= 0;
             end;
          end;

       end;


begin
  Result:= false;
  if ( not Form_Main.HaveKntFolders( true, true )) then exit;
  if ( not assigned( ActiveFolder )) then exit;
  if ( (ActiveFileIsBusy and not OnlyGetFragmentsInfo) or SearchInProgress ) then exit;

  myFindOptions.Pattern := trim( myFindOptions.Pattern ); // leading and trailing blanks need to be stripped
  PreprocessTextPattern(myFindOptions);

  FindAllSearch:= not (TreeFilter or OnlyGetFragmentsInfo);

  with myFindOptions do begin
     UsingTags:= ((FindTagsIncl <> nil) or (FindTagsExcl <> nil) or (FindTagsInclNotReg <> '') or (FindTagsExclNotReg <> ''));
     SearchTagsInText:=     TagsText and UsingTags;
     SearchTagsInMetadata:= TagsMetadata and UsingTags;
     UseInheritedTags:=  InheritedTags and SearchTagsInMetadata;
     if SearchTagsInText then
        PrepareTagsTextFoundInfo;       // -> TagsTextFoundInfo
     SearchingByDates:= (LastModifFrom <> 0) or (LastModifUntil <> 0) or (CreatedFrom <> 0) or (CreatedUntil <> 0);
  end;

  if (myFindOptions.Pattern = '') and not SearchingByDates and not (SearchTagsInMetadata or SearchTagsInText ) then exit;

  UserBreak := false;
  Form_Main.CloseNonModalDialogs;

  if TreeFilter then
     myFindOptions.FoldedMode:= sfAll

  else begin
     FindOptions.MatchCase:= myFindOptions.MatchCase;
     FindOptions.WholeWordsOnly:= myFindOptions.WholeWordsOnly;
     FindOptions.AllTabs:= myFindOptions.AllTabs;
     FindOptions.CurrentNodeAndSubtree:= myFindOptions.CurrentNodeAndSubtree;
     FindOptions.SearchScope:= myFindOptions.SearchScope;
     FindOptions.SearchMode:= myFindOptions.SearchMode;
     FindOptions.CheckMode:= myFindOptions.CheckMode;
     FindOptions.FoldedMode:= myFindOptions.FoldedMode;
     FindOptions.HiddenNodes:= myFindOptions.HiddenNodes;
     FindOptions.SearchPathInNodeNames:= myFindOptions.SearchPathInNodeNames;
     FindOptions.TagsMetadata:= myFindOptions.TagsMetadata;
     FindOptions.TagsText:= myFindOptions.TagsText;

     FindOptions.FindAllMatches := false;
     FindOptions.FindNew := true;
  end;


  myTreeNode := nil;
  myNNode := nil;
  TreeNodeToSearchIn:= nil;
  TreeNodeToSearchIn_AncestorPathLen:= 0;
  NextTextIntervalToConsider.PosI:= 0;

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
  HideEncrypted:= ActiveFile.EncryptedContentMustBeHidden;

  LastResultCellWidth:= '';
  SearchInProgress := true;
  screen.Cursor := crHourGlass;
  if FindAllSearch  then begin
     Form_Main.FindAllResults.ReadOnly:= False;
     Form_Main.FindAllResults.Clear;
  end;
  Application.ProcessMessages;
  Form_Main.FindAllResults.BeginUpdate;

  wordList := TSearchWordList.Create;
  RTFAux:= CreateAuxRichEdit;
  RTFAux.Clear;
  RTFAux.BeginUpdate;


  try
    try
      if FindAllSearch  then begin
         ClearLocationList( Location_List );
         ClearResultsSearch;
         Form_Main.LblFindAllNumResults.Caption:= '';
      end;

      SearchPatternToSearchWords (wordList, TextToFind, myFindOptions);
      SetLength(PositionsLastLocationAdded, wordList.Count);

      if not FindAllSearch then
         myFindOptions.EmphasizedSearch:= esNone;


      wordcnt := wordList.count;

      if wordcnt = 1 then begin
         myFindOptions.SearchMode := smPhrase;    // If not used smPhrase then the line number will not be shown, and will be confusing
         TextToFind:= wordList[0].word;           // '"Windows 10"' --> 'Windows 10'
      end;

      if (wordcnt = 0) and not SearchingByDates and not (SearchTagsInMetadata or SearchTagsInText) then begin
         Form_Main.Combo_ResFind.Text:= '';
         Form_Main.Btn_ResFind.Enabled:= False;
         exit;
      end;
      SearchModeToApply := myFindOptions.SearchMode;


      if OnlyGetFragmentsInfo then begin
         FreeFragments (FoundNodes, FragmentsInNodes);
         FoundNodes:= TNodeList.Create;
         FragmentsInNodes:= TNoteFragmentsList.Create;
         Fragments_LastNodeProcessed:= nil;
         Fragments_iLastNodeProcessed:= -1;
      end;


      if OnlyNode <> nil then
         myFolder:= FolderToUse
      else
      if myFindOptions.AllTabs then
         myFolder := TKntFolder(Form_Main.Pages.Pages[noteidx].PrimaryObject) // initially 0
      else
         myFolder := ActiveFolder; // will exit after one loop

      TreeUI:= myFolder.TreeUI;
      TV:= TreeUI.TV;

      if myFindOptions.CurrentNodeAndSubtree or (OnlyNode <> nil) then
         GetCurrentNode
      else begin
         GetFirstNode;
         while (not FindDone) and (not assigned(myTreeNode)) do
            GetNextFolder();
      end;

      // To make sure that RTFAux is empty after calling PrepareTextPlain, if we are explicitily interested in having
      // the RTF content of the node, in case the Emphasized option is used
      ClearRTFAux:= (myFindOptions.EmphasizedSearch <> esNone);

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

                SearchModeToApply := myFindOptions.SearchMode;    // Within each NNode we can temporarily switch from smAll to smAny

                if assigned(myTreeNode) then
                   myNNode := TreeUI.GetNNode(myTreeNode)
                else
                   break;

                // TODO: Consider the dates of the Entries of the note
                if (not (HideEncrypted and myNNode.Note.IsEncrypted) ) and
                   (not SearchingByDates or (myNNode.Note.LastModified <> 0) ) and
                   (myNNode.Note.LastModified.GetDate  >= myFindOptions.LastModifFrom) and
                   ((myFindOptions.LastModifUntil = 0) or (myNNode.Note.LastModified.GetDate <= myFindOptions.LastModifUntil)) and
                   (myNNode.Note.DateCreated.GetDate  >= myFindOptions.CreatedFrom) and
                   ((myFindOptions.CreatedUntil = 0) or (myNNode.Note.DateCreated.GetDate <= myFindOptions.CreatedUntil))
                then begin

                   // Check Tags in metadata

                  { If the text must also be searched, we can only initially exclude a node based on the presence of a tag
                    included in FindTagsExcl in its metadata.
                    The tags that the node has among those required (FindTagsIncl) may be sufficient to consider its entire
                    text, or they may complement those found in the text. For example, text tagged as Priority and Urgent
                    may be searched for. The node may be tagged only as Priority, but within it may contain text tagged
                    as Urgent. It is this text tagged in this way that must be considered.
                    Therefore, a process is required to identify text fragments that meet the tag criteria.

                    It is from FindPatternInText that all possible instances of the pattern to be searched for are located.
                    Currently, this method works at all times, considering the entire text of the note
                    (in TextPlain). If the node does not meet the tagging criteria based on the metadata and text fragments
                     need to be worked with, this must be done from that method (orchestrated by the new FindPatternInTextFragments method).
                   }

                   ConsiderNode:= True;

                   if SearchTagsInMetadata then begin
                      ConsiderAllTextInNode:= True;

                      InheritedTags:= nil;
                      if UseInheritedTags then begin
                         iNode:= TreeUI.ParentNodesWithInheritedTags.IndexOf(myTreeNode.Parent);
                         if iNode >= 0 then
                            InheritedTags:= TreeUI.InheritedParentTags[iNode];
                      end;

                      if (myFindOptions.FindTagsExcl <> nil) then begin
                         ConsiderNode:= not myNNode.MatchesTags(myFindOptions.FindTagsExcl, InheritedTags);
                         if SearchTagsInText then
                            ConsiderAllTextInNode:= False;    // Fragments that may have the tags to be excluded should be ignored.
                      end;

                      IgnoreWithTagsInText:= True;
                      if ConsiderNode and ((myFindOptions.FindTagsInclNotReg <> '') or (myFindOptions.FindTagsIncl <> nil)) then begin
                         IgnoreWithTagsInText:= False;
                         ConsiderNode:= myNNode.MatchesTags(myFindOptions.FindTagsIncl, InheritedTags);
                         if ConsiderNode and ((myFindOptions.FindTagsInclNotReg = '') or
                                              (myFindOptions.TagsModeOR and (myFindOptions.FindTagsIncl <> nil)) ) then
                            IgnoreWithTagsInText:= True       // The WITH condition is met with the metadata. It is sufficient.
                         else begin
                            // Unregistered tags cannot be linked to notes. If having an unregistered tag is a requirement,
                            // it can only be fulfilled through some fragment
                            ConsiderNode:= False;
                            if SearchTagsInText then begin
                               ConsiderNode:= True;
                               ConsiderAllTextInNode:= False;
                               IgnoreWithTagsInText:= False;     // We will have to try to meet the condition by looking at the metadata
                            end
                         end;
                      end;

                      if ConsiderNode and SearchTagsInText and not IgnoreWithoutTagsInText then     // IgnoreWithoutTagsInText is set in PrepareTagsTextFoundInfo
                         ConsiderAllTextInNode:= False;

                   end
                   else begin
                      ConsiderAllTextInNode:= not SearchTagsInText;
                      IgnoreWithTagsInText:= (myFindOptions.FindTagsIncl = nil) and (myFindOptions.FindTagsInclNotReg = '');
                   end;


                   if ConsiderNode then begin

                      if (TextToFind = '') and ConsiderAllTextInNode then begin
                         NodeNameInSearch:= myNNode.NoteName;
                         AddLocation(true, lsNormal, TextToFind, 0);      // => nodeToFilter = False
                      end
                      else begin

                         if (myFindOptions.SearchScope <> ssOnlyContent) and (TextToFind <> '') and (myFindOptions.FoldedMode <> sfOnlyFolded) then begin
                           if myFindOptions.SearchPathInNodeNames then
                              TextPlain:= myFolder.TreeUI.GetNodePath( myTreeNode, KntTreeOptions.NodeDelimiter, true )
                           else
                              TextPlain:= myNNode.NoteName;

                           NodeNameInSearch:= TextPlain;
                           if not myFindOptions.MatchCase then
                              TextPlain:= AnsiUpperCase( TextPlain);

                           FindPatternInText(true, true);
                         end;

                         if (myFindOptions.SearchScope <> ssOnlyNodeName) then begin
                            if TextPlainToUse <> '' then
                               TextPlainBAK:= TextPlainToUse
                            else
                               TextPlainBAK:= myFolder.PrepareTextPlain(myNNode, RTFAux, ClearRTFAux);
                            TextPlain:= TextPlainBAK;
                            if not myFindOptions.MatchCase then
                               TextPlain:=  AnsiUpperCase(TextPlain);

                            SearchOrigin := 0;
                            SearchingInNonRTFText:= myNNode.Note.Entries[0].IsPlainTXT;   // ### Entries[0]

                            if ConsiderAllTextInNode then
                                FindPatternInText(false, SearchingInNonRTFText)

                            else begin
                                if not myFindOptions.MatchCase then
                                   TextPlainInUpperCase:= TextPlain                    // In upper case, to be used if looking for Tags
                                else
                                   TextPlainInUpperCase:=  AnsiUpperCase(TextPlain);

                                CleanTagsTextFoundInfo;
                                FindPatternInTextFragments(SearchingInNonRTFText);
                            end;

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
                end;

                if OnlyNode <> nil then
                   break;

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

            if OnlyNode <> nil then
               break;

            while (not UserBreak) and ((not FindDone) and (not assigned(myTreeNode))) do
               GetNextFolder();

      until FindDone or UserBreak;


      if FindAllSearch  then begin
         MatchCount := Location_List.Count;
         Form_Main.LblFindAllNumResults.Caption:= '  ' + MatchCount.ToString + GetRS(sFnd13);
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
                  s2:= Path;
                  if (myFindOptions.SearchScope <> ssOnlyNodeName) and not ((TextToFind = '') and not SearchTagsInText) then begin
                     str:= str + Format('\pard\li80\sa60\sb%s \trowd\trgaph0%s \intbl {\cf1\b %s{\cf3\fs%s%s }}\cell\row \pard\li120\sb60\sa60 ',
                                     [s, LastResultCellWidth, Path, strNodeFontSize, strLocationMatch]);
                     s2:= '';
                  end;
              end;
              s:= i.ToString;
              strBgColor:= '';
              if Location.CaretPos < 0 then   // text found in node name
                 strBgColor:= '\clcbpat4';

              str:= str + Format('\trowd\trgaph0%s%s \intbl{\v\''11%s }{\b\fs%s %s.  }%s%s\cell\row ',
                    [strBgColor, LastResultCellWidth, s, strNumberingFontSize, s, s2, Location_List[pred(i)].Params]);
            end;

            str:= str + '}';
            Form_Main.FindAllResults.PutRtfText(str, true, true);
            Form_Main.FindAllResults.SelStart:= 0;
         end;
         Form_Main.Btn_ResFlip.Caption := GetRS(sFnd06);
         Form_Main.Ntbk_ResFind.PageIndex := 0;
      end;

      Result:= true;

    except
      on E : Exception do
         App.ErrorPopup(E);
    end;

  finally
    Form_Main.UpdateFolderDisplay;

    SearchInProgress := false;
    screen.Cursor := crDefault;
    wordList.Free;
    RTFAux.Free;
    Form_Main.FindAllResults.EndUpdate;
    Form_Main.FindAllResults.ReadOnly:= True;
    NextTextIntervalToConsider.PosI:= 0;
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
  Form_Main.LblFindAllNumResults.Caption:= '  ' + i.ToString + ' / ' + Location_List.Count.ToString + GetRS(sFnd13);
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
  Form_Main.FindAllResults.SetFocus;
end;


procedure FindAllResults_SelectMatch (Editor: TRxRichEdit; Prev: boolean);
var
  pS, p, offset: integer;
begin
  with Form_Main.FindAllResults do begin
      pS:= SelStart;
      if Prev then begin
         offset:= -7;
         p:= NFromLastCharPos(Editor.TextPlain, KNT_RTF_HIDDEN_MARK_L_CHAR, 1, pS + offset);
      end
      else begin
         offset:= 5;
         p:= Pos(KNT_RTF_HIDDEN_MARK_L_CHAR, Editor.TextPlain, pS + offset);
      end;

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
  TxtPlain: string;
begin
  if SelectedMatch < 0 then exit;             // *1
//if Editor.SelLength > 0 then exit;          // *1

  TxtPlain:= Editor.TextPlain;
  if TxtPlain = '' then exit;


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

  pLaux:= NFromLastCharPos(TxtPlain,#$FFF9,1, pS+1);                     //  pLaux:= Editor.FindText( #$FFF9, pS+1, -1, [stBackward]) +1;
  pL:= NFromLastCharPos(TxtPlain,KNT_RTF_HIDDEN_MARK_L_CHAR,1, pS+1);    //pL:= Editor.FindText(KNT_RTF_HIDDEN_MARK_L_CHAR, pS+1, -1, [stBackward]) +1;
  matchSelected:= (pL > 0) and (pL > pLaux);
  Form_Main.FAMCopytoEditor.Enabled:= matchSelected;

  if matchSelected then begin
     s:= Editor.GetTextRange(pL, pL+6);
     pR:= pos(' ', s, 1);
     item:= StrToInt(Copy(s, 1, pR-1));
     pR:= Pos(#$FFFB, TxtPlain, pS) - 2;                                 // pR:= Editor.FindText(#$FFFB, pS, -1, []) -1;
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
     pR:= Pos(#$FFFB, TxtPlain, pS) - 1;                                 // pR:= Editor.FindText(#$FFFB, pS, -1, []);
     if pR < 0 then
        FindAllResults_SelectMatch (Editor, true)      // Prev
     else
        FindAllResults_SelectMatch (Editor, false);    // Next
  end;


 finally
    if SelectedMatch < 0 then
       SelectedMatch:= 0;
 end;
end;

procedure FindAllResults_OnKeyDown (Editor: TRxRichEdit; Key: Word);
var
  SS: integer;
begin
   SS:= Editor.SelStart;
   case Key of
     VK_RETURN: begin
        ActiveEditor.SetFocus;
        ActiveEditor.SelLength:= 0;
     end;

     VK_UP, VK_DOWN: begin
        FindAllResults_SelectMatch (Editor, key=VK_UP);
        Editor.SetFocus;
     end;

     VK_HOME: begin
        Editor.SelStart:= 5;
        Editor.SetFocus;
     end;

     VK_END: begin
        Editor.SelStart:= Editor.TextLength;
        Editor.SetFocus;
     end;

     VK_PRIOR: begin
        SS:= SS - 700;
        if SS < 5 then
           SS:= 5;
        Editor.SelStart:= SS;
        Editor.SetFocus;
     end;

     VK_NEXT: begin
        Editor.SelStart:= SS + 700;
        Editor.SetFocus;
     end;

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

procedure ReplaceInNotes(const ToReplace, ReplaceWith: string; TagSearch: boolean; MatchCase: boolean; WholeWordsOnly: boolean);
var
  FindOptionsBAK: TFindOptions;
  LocBeforeReplacing: TLocation;

begin

  try
     LocBeforeReplacing:= nil;
     GetKntLocation (ActiveFolder, LocBeforeReplacing, false);
     LocBeforeReplacing.ScrollPosInEditor:= ActiveFolder.Editor.GetScrollPosInEditor;
     FindOptionsBAK:= FindOptions;
     _Executing_History_Jump:= True;         // A way to not record note selections in the history mechanism

     if TagSearch then begin
        MatchCase:= False;
        WholeWordsOnly:= False;
     end;

     try
        FindOptions.Pattern:= ToReplace;
        FindOptions.ReplaceWith:= ReplaceWith;
        FindOptions.TagSearch:= TagSearch;
        FindOptions.MatchCase:= MatchCase;
        FindOptions.WholeWordsOnly:= WholeWordsOnly;

        with FindOptions do begin
           FindNew:= True;
           EntireScope:= True;
           ReplaceConfirm:= False;
           HiddenNodes:= True;
           AllNodes:= True;
           AllTabs_FindReplace:= True;
           Wrap:= True;
        end;
        ReplaceEventProc(True);

     finally
       try
          JumpToLocation(LocBeforeReplacing, true, false);
       finally
          LocBeforeReplacing.Free;
       end;
       FindOptions:= FindOptionsBAK;
       _Executing_History_Jump:= False;
     end;

  except
    on E : Exception do begin
      App.ErrorPopup(E);
    end;
  end;

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
  SearchInImLinkTextPlain: boolean;  // True: PatternPos is a position in imLinkTextPlain
  IgnoreKNTHiddenMarks: boolean;
  HideEncrypted: boolean;
  pI, pF: integer;

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
  var
     ContainsRegImages: boolean;       // True: 'Can' contain images
     incSel: integer;
  begin
      if (myFolder <> ActiveFolder) then
         App.ActivateFolder(myFolder);

      if myFolder.TreeUI.FocusedNode <> myTreeNode then begin
         myFolder.TreeUI.MakePathVisible(myTreeNode);   // Could be hidden
         myFolder.TreeUI.SelectAlone(myTreeNode);
      end;

      ContainsRegImages:= (not Is_ReplacingAll) or ReplacingLastNodeHasRegImg;

     // If a tag is being removed (replacing it with ""), FindTag can have located a block closures associated
     // with that tag. Ex: #Tag --> ##Tag. In this case we must expand the text to be selected by 1
      incSel:= 0;
      if FindOptions.TagSearch and FoundClosingTag and (FindOptions.ReplaceWith = '') then
         inc(incSel);

      SearchCaretPos (myFolder.Editor, PatternPos - incSel, length( Text_To_Find) + SizeInternalHiddenText + incSel, true, Point(-1,-1),
                      false, ContainsRegImages, SearchInImLinkTextPlain);
  end;

  function GetTextPlainFromNode(NNode: TNoteNode; RTFAux: TAuxRichEdit): string;
  var
     NEntry: TNoteEntry;

  begin
     if myFolder.NoteUI.NNode = NNode then begin
        SearchInImLinkTextPlain:= false;
        Result:= myFolder.NoteUI.Editor.TextPlain;
     end
     else begin
        NEntry:= NNode.Note.Entries[0];         // %%%
        LoadStreamInRTFAux (NEntry.Stream, RTFAux);
        SearchInImLinkTextPlain:= true;
        Result:= RTFAux.TextPlain;
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

  App.ShowInfoInStatusBar(GetRS(sFnd07));

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

      ReplacingLastNodeHasRegImg:= false;
      if Is_ReplacingAll then begin
         if not Editor.SupportsRegisteredImages then
            ReplacingLastNodeHasRegImg:= false
         else
            ReplacingLastNodeHasRegImg:= Editor.ContainsRegisteredImages;
         ReplacingLastNode:= myTreeNode;
      end;


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


      HideEncrypted:= ActiveFile.EncryptedContentMustBeHidden;

      // Búsqueda del patrón iterando sobre los nodos / notas hasta encontrar uno -------------
      // Según las opciones establecidas así podrán recorrerse o no todos los nodos de una nota, todas las notas
      // o incluso continuar buscando desde el punto de partida, de manera cíclica.

      repeat
            NNode:= TreeUI.GetNNode(myTreeNode);

            if not (HideEncrypted and NNode.Note.IsEncrypted) then begin

                if Is_ReplacingAll and (ReplacingLastNode <> myTreeNode) then
                   ReplacingLastNodeHasRegImg:= true;   // We can't know this unless we query NNode.TextPlain because the Editor will still be 'pointing' to another node

                if ReplacingLastNodeHasRegImg then begin
                   TextPlain:= myFolder.PrepareTextPlain(NNode, RTFAux);
                   SearchInImLinkTextPlain:= true;
                end
                else
                   TextPlain:= GetTextPlainFromNode(NNode, RTFAux);

                if PositionInFoldedBlock(TextPlain, Editor.SelStart, nil, pI, pF) then
                   SearchOrigin:= pF + 1;

                if Is_ReplacingAll and (ReplacingLastNode <> myTreeNode) then begin
                   ReplacingLastNode:= myTreeNode;
                   ReplacingLastNodeHasRegImg:= (ImageMng.GetImagesIDInstancesFromTextPlain(TextPlain) <> nil);  // Next replacements on the same node will be optimized if this node has no images
                end;

                IgnoreKNTHiddenMarks:= NNode.Note.Entries[0].IsRTF;   // ### Entries[0]

                if FindOptions.MatchCase then
                   PatternPos:= FindPattern(Text_To_Find, TextPlain, SearchOrigin+1, SizeInternalHiddenText, IgnoreKNTHiddenMarks, True) -1
                else
                   PatternPos:= FindPattern(AnsiUpperCase(Text_To_Find), AnsiUpperCase(TextPlain), SearchOrigin+1, SizeInternalHiddenText, IgnoreKNTHiddenMarks, True) -1;

                {
                PatternPos := EditControl.FindText(
                  Text_To_Find,
                  SearchOrigin, -1,
                  SearchOpts
                );
                }
            end;

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
          App.ErrorPopup(E, GetRS(sFnd08));
          exit;
      end;
    end;

  finally
      if Found then begin
          if not Is_ReplacingAll then
             App.ShowInfoInStatusBar(Format(GetRS(sFnd09), [PatternPos, NumberFoundItems]));
          if IsRecordingMacro then
             AddMacroEditCommand( ecFindText );
      end
      else begin
          App.ShowInfoInStatusBar(GetRS(sFnd10));
          if not (UserBreak or Is_Replacing) then
             App.InfoPopup(Format( GetRS(sFnd02), [Text_To_Find] ));
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
  pI, pF: integer;

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

  App.ShowInfoInStatusBar(GetRS(sFnd07));


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
         if PositionInFoldedBlock(TextPlain, Editor.SelStart, nil, pI, pF) then
            SearchOrigin:= pF + 1;

         if FindOptions.MatchCase then
            PatternPos:= FindPattern(Text_To_Find, TextPlain, SearchOrigin+1, SizeInternalHiddenText, True, True) -1
         else
            PatternPos:= FindPattern(AnsiUpperCase(Text_To_Find), AnsiUpperCase(TextPlain), SearchOrigin+1, SizeInternalHiddenText, True, True) -1;

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
          App.ErrorPopup(E, GetRS(sFnd08));
          exit;
      end;
    end;

  finally
      if Found then begin
          if not Is_ReplacingAll then
             App.ShowInfoInStatusBar(Format(GetRS(sFnd09), [PatternPos, NumberFoundItems]));
          if IsRecordingMacro then
             AddMacroEditCommand( ecFindText );
      end
      else begin
          App.ShowInfoInStatusBar(GetRS(sFnd10));
          if not Is_Replacing then
             App.InfoPopup(Format( GetRS(sFnd02), [Text_To_Find] ));
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
  ReplaceCnt, IgnoreCnt : integer;
  Original_Confirm : boolean;
  Original_EntireScope : boolean;
  SelectedTextToReplace: boolean;
  ReplaceWith: string;
  DoReplace: Boolean;
  txtMessage: string;
  handle: HWND;
  AppliedBeginUpdate: Boolean;
  Editor: TKntRichEdit;
  pI, pF: integer;
  SS: integer;
  Selection: TInsideOrPartialSelection;


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
          case MessageDlg( GetRS(sFnd01),
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
  IgnoreCnt := 0;
  Text_To_Find := FindOptions.Pattern;
  ReplaceWith:= FindOptions.ReplaceWith;
  if FindOptions.TagSearch then begin
     Text_To_Find := '#' + Text_To_Find.ToUpper;
     if ReplaceWith <> '' then
        ReplaceWith := '#' + ReplaceWith;
  end;


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
                SS:= Editor.SelStart;

                // ¿Hay que restringirse al texto inicialmente seleccionado?
                if ReplaceAll and FindOptions.SelectedText then
                   if (SS < FindOptions.SelectionStart) or
                     ((SS + Editor.SelLength) > FindOptions.SelectionEnd) then
                       break;

                if GetReplacementConfirmation then begin
                   Selection:= InsideOrPartiallySelectedProtectedBlock(Editor, True);
                   if Selection = ipsNone then begin
                      inc(ReplaceCnt);
                      Editor.AddText(ReplaceWith);
                      if Editor.NNodeObj <> nil then
                         App.ChangeInEditor(Editor);
                   end
                   else begin
                      if Selection = ipsFolded then begin
                         PositionInFoldedBlock(Editor.TextPlain, Editor.SelStart, Editor, pI, pF);
                         Editor.SelStart:= pF + 1;
                         inc(IgnoreCnt);
                      end
                      else
                         Editor.SelStart:= SS + 1;
                   end;
                end;

                Application.ProcessMessages;
                if UserBreak then break;

                SelectedTextToReplace:= RunFindNext(ReplaceAll);     // Localizamos el siguiente patrón a remplazar
                Editor:= SearchingInEditor;

                if (not ReplaceAll) then break;          // Dejamos simplemente localizado el texto si no ReplaceAll

            except
               On E : Exception do begin
                  App.ErrorPopup( E.Message );
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

  txtMessage:= Format( GetRS(sFnd11), [ReplaceCnt] );
  App.ShowInfoInStatusBar(txtMessage);
  if ( ReplaceCnt > 0 ) then begin
     if ReplaceAll then begin
        Editor.SelLength:= 0;
        if not FindOptions.TagSearch then
           App.InfoPopup(txtMessage);
     end;
     if Editor.ParentEditor <> nil then
        Editor.Parent.SetFocus
     else
        App.ActivateFolder(ActiveFolder);
  end
  else
      if not SelectedTextToReplace and not FindOptions.TagSearch and (IgnoreCnt = 0) then begin
         App.InfoPopup(Format( GetRS(sFnd02), [Text_To_Find]));
      end;

  if (IgnoreCnt > 0) then
      App.WarningPopup(Format( GetRS(sFnd14), [IgnoreCnt]));

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
    NextTextIntervalToConsider.PosI:= 0;
    FoundNodes:= nil;
    FragmentsInNodes:= nil;
end.
