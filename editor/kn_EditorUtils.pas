unit kn_EditorUtils;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface
uses
   Winapi.Windows,
   Winapi.Messages,
   Winapi.RichEdit,
   Winapi.MMSystem,
   System.SysUtils,
   System.StrUtils,
   System.Classes,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.Clipbrd,
   Vcl.ComCtrls,
   Vcl.Graphics,
   Vcl.ExtDlgs,
   ExtCtrls,

   BrowseDr,
   TreeNT,
   Parser,
   FreeWordWeb,
   UAS,
   RxRichEd,
   SynGdiPlus, // RxGIF,
   RichPrint,
   AJBSpeller,

   gf_misc,
   gf_files,
   gf_strings,
   gf_miscvcl,
   GFTipDlg,
   kn_INI,
   kn_const,
   kn_Cmd,
   kn_Msgs,
   kn_Info,
   kn_FileObj,
   kn_NewNote,
   kn_NoteFileMng,
   kn_TreeNoteMng,
   kn_NoteMng,
   kn_NodeList,
   kn_Chars,
   kn_ClipUtils,
   kn_ExpTermDef,
   kn_RTFUtils,
   kn_LinksMng,
   kn_NoteObj;

type
   THiddenMarks = (hmOnlyBookmarks, hmOnlyImages, hmAll);

    // glossary management
    procedure ExpandTermProc;
    procedure AddGlossaryTerm;
    procedure EditGlossaryTerms;

    function GetWordCount( const t : string ) : longint;
    procedure UpdateWordCount;
    procedure CleanWordCount;
    procedure CheckWordCount (cleanWC: boolean= false);
    procedure CheckWordCountWaiting;
    function HasNonAlphaNumericOrWordDelimiter(const s : string) : boolean;

    function GetEditorWithNoKNTHiddenCharacters (const Editor: TTabRichEdit; HiddenMarksToRemove: THiddenMarks; const selection: boolean= true): TTabRichEdit;
    function RemoveKNTHiddenCharacters(const s: string; checkIfNeeded: boolean = true): string;
    function RemoveKNTHiddenCharactersInRTF(const s: AnsiString; HiddenMarks: THiddenMarks): AnsiString;
    function KeepOnlyLeadingKNTHiddenCharacters(const txt: string): string;

    procedure CheckToSelectLeftImageHiddenMark (Editor: TRxRichEdit; var SelStartOrig: integer; var SelLengthOrig: integer; offset: integer= 0); overload;
    procedure CheckToSelectLeftImageHiddenMark (Editor: TRxRichEdit; offset: integer= 0); overload;
    procedure CheckToSelectRightImageHiddenMark (Editor: TRxRichEdit);
    procedure CheckToMoveLefOftHiddenMark (Editor : TRxRichEdit);
    procedure CheckToSelectImageHiddenMarkOnDelete (Editor: TRxRichEdit);

    function CheckToIdentifyImageID (Editor : TRxRichEdit; var posFirstHiddenChar: integer): integer;

    procedure MatchBracket;
    procedure TrimBlanks( const TrimWhat : integer );
    procedure CompressWhiteSpace;
    procedure EvaluateExpression;

    procedure InsertSpecialCharacter;
    procedure InsertPictureOrObject( const AsPicture : boolean );

    procedure ArabicToRoman;
    procedure RomanToArabic;

    procedure ShowStatistics;
    procedure WordWebLookup;

    procedure EnableOrDisableUAS;
    procedure ConfigureUAS;

    // clipboard capture and paste
    procedure TryPasteRTF(Note: TTabNote; HTMLText: AnsiString='');
    procedure PasteBestAvailableFormat (Note: TTabNote; TryOfferRTF: boolean= True; CorrectHTMLtoRTF: boolean = False);
    procedure PasteBestAvailableFormatInEditor (Editor: TRxRichEdit; Note: TTabNote; TryOfferRTF: boolean= True; CorrectHTMLtoRTF: boolean = False);
    procedure ToggleClipCap( const TurnOn : boolean; const aNote : TTabNote );
    procedure SetClipCapState( const IsOn : boolean );
    procedure PasteOnClipCap (ClpStr: string);
    procedure PasteAsWebClip (const PasteAsText: boolean);
    procedure PasteIntoNew( const AsNewNote : boolean );

    procedure PrintRTFNote;

    procedure RunSpellcheckerForNote;

    function GetEditorZoom (Editor: TRxRichEdit = nil): integer;
    procedure SetEditorZoom(ZoomValue : integer; ZoomString : string; Increment: integer= 0 );  overload;
    procedure SetEditorZoom( Editor: TRxRichEdit; ZoomValue : integer; ZoomString : string; Increment: integer= 0 ); overload;
    procedure SetMargins();

    procedure GetIndentInformation(Editor: TRxRichEdit;
                                   var Indent: integer; var NextIndent: integer;
                                   var LineStr: string;
                                   var posBegin : integer;
                                   paragraphMode: boolean= false);
    function NumberOfLineFeed(Str: string): integer;

    procedure ShowTipOfTheDay;

    function GetTitleFromURL (const URL: String; TryForLimitedTime: boolean): String;
    procedure CleanCacheURLs (OnlyWithoutTitle: boolean);

const
  WordDelimiters = [#9, #10, #13, #32];

implementation

uses
   Kn_Global,
   kn_Glossary,
   kn_ExportImport,
   kn_VCLControlsMng,
   kn_MacroMng,
   UWebBrowserWrapper,
   kn_Main;

var
   LastURLPasted: string;


resourcestring

  STR_Gloss_01 = ' Function not available';
  STR_Gloss_02 = ' No word at cursor';
  STR_Gloss_03 = ' Word not in glossary. Use Shift+F7 to add.';
  STR_Gloss_04 = 'Term expansion glossary "%s" is not loaded.';
  STR_Gloss_05 = 'Glossary term already exists: "%s" -> "%s". OK to redefine term as "%s"?';
  STR_Gloss_06 = ' Added to glossary: "%s" -> "%s"';
  STR_Bracket_01 = ' No valid bracket at cursor position ';
  STR_Bracket_02 = ' Matching bracket FOUND';
  STR_Bracket_03 = ' Matching bracket NOT FOUND';
  STR_Trim_01 = 'OK to trim white space characters in whole note?';
  STR_Compress_01 = 'OK to compress white space characters in whole note?';
  STR_Eval_01 = ' Result: ';
  STR_Eval_02 = 'Paste last eval result: ';
  STR_Eval_03 = 'Expression %s evaluates to: %s' + #13#13 + 'Result was copied to clipboard. Click OK to insert.';
  STR_Img_01 = 'Select image to insert';
  STR_Img_02 = 'All image files';
  STR_Img_03 = 'All files';
  STR_ConvDec_01 = 'Convert decimal to Roman';
  STR_ConvDec_02 = 'Enter a decimal number:';
  STR_ConvDec_03 = '%s is not a valid number';
  STR_ConvRoman_01 = 'Convert Roman to decimal';
  STR_ConvRoman_02 = 'Enter a Roman number:';
  STR_ConvRoman_03 = '%s is not a valid Roman number';
  STR_Statistics_01 = ' Calculating statistics... Please wait';
  STR_Statistics_02 = 'Selected text';
  STR_Statistics_03 = 'Note text';
  STR_Statistics_04 = '%s statistics' + #13#13 +
       'Characters: %s' + #13 +
       'Alphabetic: %s' + #13 +
       'Whitespace: %s' + #13#13 +
       'Words: %s' + #13 +
       'Lines: %s';
  STR_Statistics_05 = #13#13+'Number of nodes in tree: %d';
  STR_Statistics_06 = 'Chars: %d  Alph: %d  Words: %d';
  STR_Statistics_07 = #13#13+'Clik OK to copy information to clipboard.';
  STR_WordWeb_01 = 'Lookup in WordWeb';
  STR_WordWeb_02 = 'Enter word to look up:';
  STR_WordWeb_03 = 'Error loading WordWeb. The program may not be installed ' +
            'on your computer. See file "wordweb.txt" for more information.' +
            #13#13 +
            'Error message: ';
  STR_UAS_01 = 'UAS path';
  STR_UAS_02 = 'Please specify full path to uas.exe';
  STR_UAS_03 = 'KeyNote cannot find the location of uas.exe. UltimaShell Autocompletion Server will not be loaded.';
  STR_UAS_04 = ' UltimaShell Autocompletion Server loaded.';
  STR_UAS_05 = 'Cannot load UltimaShell Autocompletion Server. It may not be installed. Would you like to go to the UAS website and download the application?';
  STR_UAS_06 = ' UltimaShell Autocompletion Server unloaded.';
  STR_UAS_07 = ' UltimaShell Autocompletion Server is not loaded.';
  STR_ClipCap_01 = 'A Read-Only note cannot be used for clipboard capture.';
  STR_ClipCap_02 = 'a new node';
  STR_ClipCap_03 = 'whichever node is currently selected';
  STR_ClipCap_04 = 'This is a %s note. Each copied item will be pasted into %s in the tree. Continue?';
  STR_ClipCap_05 = ' Clipboard capture is now ';
  STR_ClipCap_06 = ' Capturing text from clipboard';
  STR_ClipCap_07 = 'Cannot obtain tree node for pasting data.';
  STR_ClipCap_09 = ' Clipboard capture done';
  STR_Print_01 = 'Current note is a Tree-type note and contains more than one node. Do you want to print all nodes? Answer No to only print the selected node.';
  STR_Print_02 = 'Replace editor contents with result from spellchecker?';
  STR_Zoom_01 = 'Invalid zoom ratio: ';
  STR_Tip_01 = 'Cannot display Tip of the Day: file "%s" not found.';
  STR_Tip_02 = ': Tip of the Day';


//=================================================================
// ExpandTermProc
//=================================================================
procedure ExpandTermProc;
var
  w, replw : string;
  wordlen : integer;
begin
  with Form_Main do begin
      if ( not ( assigned( GlossaryList ) and assigned( ActiveNote ) and ( ActiveNote.FocusMemory = focRTF ))) then begin
        StatusBar.Panels[PANEL_HINT].Text := STR_Gloss_01;
        exit;
      end;
      if NoteIsReadOnly( ActiveNote, true ) then exit;

      UpdateLastCommand( ecExpandTerm );
      if IsRecordingMacro then
        AddMacroEditCommand( ecExpandTerm );

      if ( ActiveNote.Editor.SelLength = 0 ) then
        w := ActiveNote.Editor.GetWordAtCursor( true )
      else
        w := ActiveNote.Editor.SelText;
      wordlen := length( w );

      if ( length( w ) = 0 ) then begin
        StatusBar.Panels[PANEL_HINT].Text := STR_Gloss_02;
        exit;
      end;

      replw := GlossaryList.Values[w];

      if ( replw = '' ) then begin
        StatusBar.Panels[PANEL_HINT].Text := STR_Gloss_03;
        exit;
      end;

      StatusBar.Panels[PANEL_HINT].Text := Format( ' %s -> %s', [w,replw] );
      replw := ExpandMetaChars( replw );
      ActiveNote.Editor.SelText := replw;

      ActiveNote.Editor.SelStart := ActiveNote.Editor.SelStart + ActiveNote.Editor.SelLength;

      NoteFile.Modified := true;
      UpdateNoteFileState( [fscModified] );
  end;

end; // ExpandTermProc

//=================================================================
// AddGlossaryTerm
//=================================================================
procedure AddGlossaryTerm;
var
  Form_TermDef : TForm_TermDef;
  nstr, vstr : string;
begin
  if ( not assigned( GlossaryList )) then begin
    showmessage( Format(STR_Gloss_04, [Glossary_FN] ));
    exit;
  end;

  nstr := '';
  vstr := '';

  if assigned( ActiveNote ) then begin
    if ( ActiveNote.Editor.SelLength > 0 ) then
      nstr := trim( copy( ActiveNote.Editor.SelText, 1, 255 ))
    else
      nstr := ActiveNote.Editor.GetWordAtCursor( true );
    if ( nstr <> '' ) then
      vstr := GlossaryList.Values[nstr];
  end;

  Form_TermDef := TForm_TermDef.Create( Form_Main );
  try
    with Form_TermDef do begin
      Edit_Term.Text := nstr;
      Edit_Exp.Text := vstr;
      if ( ShowModal = mrOK ) then begin
        nstr := trim( Edit_Term.Text );
        vstr := trim( Edit_Exp.Text );

        if (( nstr <> '' ) and ( vstr <> '' ) and ( nstr <> vstr )) then begin

          if ( GlossaryList.IndexOfName( nstr ) >= 0 ) then begin
            if ( messagedlg( Format(STR_Gloss_05,
              [nstr,GlossaryList.Values[nstr],vstr] ),
              mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;
          end;

          try
            try
              GlossaryList.Sorted := false;
              GlossaryList.Values[nstr] := vstr;
            finally
              GlossaryList.Sorted := true;
            end;
            GlossaryList.SaveToFile( Glossary_FN, TEncoding.UTF8);
            Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format(STR_Gloss_06, [nstr,vstr] );
          except
            on E : Exception do
              showmessage( E.Message );
          end;
        end;
      end;
    end;
  finally
    Form_TermDef.Free;
  end;

end; // AddGlossaryTerm

procedure EditGlossaryTerms;
var
  Form_Glossary : TForm_Glossary;
begin
  Form_Glossary := TForm_Glossary.Create( Form_Main );
  try
    Form_Glossary.ShowModal;
  finally
    Form_Glossary.Free;
  end;
end; // EditGlossaryTerms


//=================================================================
// GetWordCount
//=================================================================
function GetWordCount( const t : string ) : longint;
var
  i, len : longint;
begin
  len := length( t );
  result := 0;
  if (len > 0) then begin
    i := 1;
    repeat
      if AnsiChar(t[i]) in WordDelimiters then begin
         inc( i );
         continue;
      end
      else
        inc( result );

      // scan to end of word
      while (i <= len) and (not (AnsiChar(t[i]) in WordDelimiters)) do
        inc( i );
    until ( i > len );
  end;
end; // GetWordCount


function HasNonAlphaNumericOrWordDelimiter(const s : string) : boolean;
var
  i: integer;
begin
  for i := 1 to length(s) do
    if not ( (AnsiChar(s[i]) in WordDelimiters) or IsCharAlphaNumeric(s[i]) or (s[i]='''')) then        // ': don't ...  Normal in english words
        Exit(true);

  Result:= False;
end;


function GetEditorWithNoKNTHiddenCharacters (const Editor: TTabRichEdit; HiddenMarksToRemove: THiddenMarks; const selection: boolean= true): TTabRichEdit;
var
  s: string;
  len: integer;
begin
    Result:= Editor;

    case HiddenMarksToRemove of
       hmOnlyBookmarks: s:= KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_BOOKMARK;
       hmOnlyImages:    s:= KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_IMAGE;
       hmAll:           s:= KNT_RTF_HIDDEN_MARK_L_CHAR;
    end;

    if Result.FindText(s, 0, -1, []) >= 0 then begin

       if selection then
          s:= ActiveNote.Editor.RtfSelText
       else
          s:= ActiveNote.Editor.RtfText;

       len:= s.Length;                          // There might be a hidden markup in the editor, but maybe not in the selection
       s:= RemoveKNTHiddenCharactersInRTF(s, HiddenMarksToRemove);   // In that case this method will return the same string

       if s.Length <> Len then begin
          Result:= CreateRTFAuxEditorControl;      // Caller should call free on this control after used
          Result.PutRtfText(s, true, true, true);
          if RichEditVersion <= 4 then
             Result.SetSelection(0, Result.TextLength, false);
       end;
    end
end;


function RemoveKNTHiddenCharacters(const s: string; checkIfNeeded: boolean= true): string;
var
   i: integer;
   L, R: integer;
   P: PChar;
begin
  if s='' then Exit(s);
  if checkIfNeeded and (pos(KNT_RTF_HIDDEN_MARK_L_CHAR, s, 1) = 0) then Exit(s);

  Result:= s;
  P:= PChar(Result);
  i:= 0;
  repeat
     if (P[i]=KNT_RTF_HIDDEN_MARK_L_CHAR) then begin
        L:= i;
        repeat
           inc(i);
        until (P[i]=#0) or (P[i]=KNT_RTF_HIDDEN_MARK_R_CHAR);
        Delete(Result, L+1, i-L+1);
        P:= PChar(Result);
        i:= L-1;
     end;
     inc(i);
  until (P[i]=#0);

end;


function RemoveKNTHiddenCharactersInRTF(const s: AnsiString; HiddenMarks: THiddenMarks): AnsiString;
var
   pI, pPrefix, pF, len: integer;
   Prefix: AnsiString;
begin
  if S='' then Exit('');

  //  {\rtf1\ansi {\v\'11B5\'12} XXX };   {\rtf1\ansi \v\'11B5\'12\v0 XXX};  {\rtf1\ansi \v\'11T999999\'12\v0 XXX};

  { *1
     hello \v\''11B1\''12\v0\fs36 BIG WORLD  =>  hello \fs36 BIG WORLD
     hello \v\''11B1\''12\v0 world           =>  hello world    (-> remove space after \v0)
  }


  (*

   When looking for our hidden marks as RTF we must take into account that the RichEdit control can alter them, adding
   control tags within the block that we have bounded between \v and \v0 (actually through {\v....} ). Doesn't seem to have a problem
   in including the tags inside, although they define hidden content...
   We may find that an added tag like:	  
      ...    \v\''11B2\''12\v0 BYE!!
   it ends up appearing for example as:
	  \v\f0\fs16\lang3082\''11B2\''12\v0 BYE!!
	 ... \pard\cf1\ul\f0\fs20\lang1033 Hello\ulnone  World \v\f1\fs40\'11B1\'12\v0 BYE!!!\f0\fs20\par
   \v\''11I1\''12\cf0\v0\f2\fs16\lang3082{\pict{...
	 ...  etc.

   We should convert:
     "\v\f0\fs16\lang3082\''11B2\''12\v0 BYE!!"   ->  "\f0\fs16\lang3082 BYE!!"

   If the hidden mark is in it's normal state it will be completely removed: \v\''11B2\''12\v0 BYE!! -> BYE!!
   But if it is not, we will only remove our hidden characters ('11T999999\'12) but not the RTF controls. The remaining \v and \v0
   will be ignored later by RTFEdit control.

   (This is a problem only when we are looking for the hidden marks through RTF syntax. In most situations, KNT works directly with
    hidden characters)


   We will also allow deleting only the hidden marks corresponding to KNT Link markers \v\'11B....\'12\v0
   KNT will use other marks that we may not want to delete, such as those that will label images.

   The hidden strings used by KNT will have a variable length, but will normally have a maximum of the form maximum: HT999999H
   where H:KNT_RTF_HIDDEN_MARK_CHAR, T: Type (B:Bookmark, T:Tag, I:Image...)
     \v\'11T999999\'12\v0    -> 20 max (If necessary we can increase it)          KNT_RTF_HIDDEN_MAX_LENGHT
       \'11T999999\'12       -> 15 max                                            KNT_RTF_HIDDEN_MAX_LENGHT_CONTENT

  *)

  case HiddenMarks of
     hmOnlyBookmarks: Prefix:= KNT_RTF_HIDDEN_MARK_L + KNT_RTF_HIDDEN_BOOKMARK;
     hmOnlyImages:    Prefix:= KNT_RTF_HIDDEN_MARK_L + KNT_RTF_HIDDEN_IMAGE;
     hmAll:           Prefix:= KNT_RTF_HIDDEN_MARK_L;
  end;

  Result:= s;
  pI:= 1;

  repeat
     pI:= Pos(AnsiString('\v\'), Result, pI);

     if pI > 0 then begin
        pPrefix:= Pos(Prefix, Result, pI+2);
        if (pPrefix = 0) then break;

        pF:= Pos(AnsiString(KNT_RTF_HIDDEN_MARK_R + '\v0'), Result, pPrefix + Length(Prefix));
        len:= pF-pI + Length(KNT_RTF_HIDDEN_MARK_R + '\v0');
        if (pF > 0) and (pPrefix = pI + 2) and (len <= KNT_RTF_HIDDEN_MAX_LENGHT) then begin
           // Normal case: \v\'11B5\'12\v0 XXX
            if Result[pI + len] = ' ' then          // *1
               Inc(len);
            Delete(Result, pI, len);
            pI:= pF+1;
        end
        else begin
           // Problematic case. Ex:
           //  \v\f1\fs40\'11B1\'12\v0 xxx                         --> \v\f1\fs40\v0 xxx                     ...> \v\f1\fs40 xxx
           //  \v\''11I1\''12\cf0\v0\f2\fs16\lang3082{\pict{...    --> \v\cf0\v0\f2\fs16\lang3082{\pict{...  ...> \cf0\f2\fs16\lang3082{\pict{...

            pF:= Pos(AnsiString(KNT_RTF_HIDDEN_MARK_R), Result, pPrefix + Length(Prefix));        // Do not include \v0
            if (pF = 0) then break;

            len:= pF-pPrefix + Length(KNT_RTF_HIDDEN_MARK_R);
            if (len <= KNT_RTF_HIDDEN_MAX_LENGHT_CONTENT) then
               Delete(Result, pPrefix, len);
            pI:= pF+1;
        end;

     end;

  until pI = 0;

end;




{ 
  If an image is selected (or cursor is just to the left of it and DELETE (SUPR) has been pressed)
  -> check if the hidden mark KNT_RTF_IMG_HIDDEN_MARK is present (it should) and if so expand the selection to include it.
  Also, check if just to the left is the hidden " at the end of a hyperlink. If so, select its hidden characters, which will include in the
  selection our hidden mark, if there is one.

  Result: initial SelStart (original SelStart was modified because a hidden image mark has been selected)
  If < 0 -> SelStart was not modified
  Offset: To use with Backspace, we are interested in looking one character further to the left, skipping the possible image that is deleted
}
procedure CheckToSelectLeftImageHiddenMark (Editor : TRxRichEdit; var SelStartOrig: integer; var SelLengthOrig: integer; offset: integer= 0); overload;
var
  L, S, newS: integer;
  Ch: Char;
  UndoSelect: boolean;

begin
  SelStartOrig:= -1;

  with Editor do begin
     S:= SelStart;
     newS:= S-1 +offset;
     if newS < 0 then exit;

     Ch:= GetTextRange(newS, S +offset)[1];
     if (ch = KNT_RTF_HIDDEN_MARK_R_CHAR) or (ch = '"') then begin        // (ch = '"'), in case the hidden mark is next to a hyperlink
       SelLengthOrig:= SelLength;
       OnSelectionChange := nil;
       try
         SelStart:= newS;
         L:= SelStart;
         UndoSelect:= true;
         if L <> newS then begin
            SetSelection(L, S + SelLengthOrig, true);                     // It will have been placed to the left of the first hidden character. See coment in TForm_Main.RxRTFKeyDown  (Left cursor)
            if SelText.StartsWith(KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_IMAGE) then
               UndoSelect:= false;
         end;

         if UndoSelect then
            SetSelection(S, S + SelLengthOrig, true)
         else
            SelStartOrig:= S;

       finally
         OnSelectionChange := Form_Main.RxRTFSelectionChange;
       end;

     end;
  end;
end;

procedure CheckToSelectLeftImageHiddenMark (Editor : TRxRichEdit; offset: integer= 0); overload;
var
   SelStartOrig, SelLengthOrig: integer;
begin
   CheckToSelectLeftImageHiddenMark (Editor, SelStartOrig, SelLengthOrig, offset);
end;


// It is assumed that SelLenght = 0
procedure CheckToSelectRightImageHiddenMark (Editor : TRxRichEdit);
var
  R, S, newS: integer;
  SelLengthOrig: integer;
  Ch: Char;
  UndoSelect: boolean;
  TextSelected: string;

begin

  with Editor do begin
     S:= SelStart;
     newS:= S+1;

     try
       Ch:= GetTextRange(S, newS)[1];
       if (ch = KNT_RTF_HIDDEN_MARK_L_CHAR) or (ch = '"') then begin         // (ch = '"'), in case the hidden mark is next to a hyperlink
         OnSelectionChange := nil;                                           // *1
         try
           SelStart:= newS;
           R:= SelStart;
           UndoSelect:= true;
           if R <> newS then begin
              // It will have been placed to the right of the last hidden character.  See coment in TForm_Main.RxRTFKeyDown  (Left cursor)
              SetSelection(S, R, true);
              TextSelected:= SelText;
              if TextSelected.StartsWith(KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_IMAGE) then begin
                 UndoSelect:= false;
                 if TextSelected[Length(TextSelected)] = KNT_RTF_HIDDEN_MARK_R_CHAR then
                    SetSelection(S, R+1, true);                              // We must be next to a visible image ({pict...}). We have to select it too
              end;
           end;

           if UndoSelect then
              SelStart:= S;

         finally
           OnSelectionChange := Form_Main.RxRTFSelectionChange;
         end;
       end;

     except   // If we are in the last position of the note
     end;
  end;
end;

procedure CheckToSelectImageHiddenMarkOnDelete (Editor: TRxRichEdit);
begin
   // Note that it is also possible to right click on an visible image and select Delete

    if (ImagesManager.StorageMode <> smEmbRTF) and NoteSupportsRegisteredImages then begin
       if Editor.SelLength = 0 then
          {
            If we are to the left of a hyperlink corresponding to an image, therefore just to the left of its hidden identification characters,
            and we press DELETE (SUPR), the hyperlink would be deleted and our hidden characters would remain.
            To avoid this we can select all the hidden characters, to the right, including those of the hyperlink and ours, so that everything is deleted as a block.
            #$11'I1'#$12'HYPERLINK "img:1,32,32"N'
            Something analogous must be controlled if we are to the left of a visible image
          }
          CheckToSelectRightImageHiddenMark(Editor)
       else
          CheckToSelectLeftImageHiddenMark(Editor);
    end;

end;


{ Check if there is a hidden image identification mark just to our left }

function CheckToIdentifyImageID (Editor : TRxRichEdit; var posFirstHiddenChar: integer): integer;
var
  L, S: integer;
  SelLengthBak: integer;
  Str: String;

begin

  Result:= 0;              // 0 is not a valid ID

  // <L>I999999<R>

  with Editor do begin
     S:= SelStart;
     if GetTextRange(S-1, S) = KNT_RTF_HIDDEN_MARK_R_CHAR then begin
       SelLengthBak:= SelLength;
       OnSelectionChange := nil;
       try
         SelStart:= S-1;
         L:= SelStart;
         if L <> S-1 then begin
            // It will have been placed to the left of the first hidden character. See coment in TForm_Main.RxRTFKeyDown  (Left cursor)
            SetSelection(L, S + 1, true);
            Str:= Editor.GetTextRange(L, S);
            if (Str[1] = KNT_RTF_HIDDEN_MARK_L_CHAR) and (Str[2] = KNT_RTF_HIDDEN_IMAGE) then
               Result:= StrToIntDef(Copy(Str, 3, (S - L)-3), 0);
         end;
         SetSelection(S, S + SelLengthBak, true);
         posFirstHiddenChar:= L;

       finally
         OnSelectionChange := Form_Main.RxRTFSelectionChange;
       end;

     end;
  end;
end;



// This procedure asumes that SelLength = 0 (it will invoked currently from InsertContent  (kn_NoteFileMng)
procedure CheckToMoveLefOftHiddenMark (Editor : TRxRichEdit);
var
  S: integer;
begin
  with Editor do begin
     S:= SelStart;
     if GetTextRange(S-1, S) = KNT_RTF_HIDDEN_MARK_R_CHAR then begin
       SelStart:= S-1;
       if SelStart <> S-1 then
       else
          SelStart:= S;              // It was not hidden. We leave it where it was
     end;
  end;

end;


//=================================================================
// UpdateWordCount
//=================================================================
procedure UpdateWordCount;
var
  p, s : string;
  wc : longint;
  i: integer;
  numPag: single;
begin
  if ( not assigned( ActiveNote )) then exit;

  if EditorOptions.WordCountTrack then begin

    if ( ActiveNote.Editor.SelLength = 0 ) then
       wc := GetWordCount( ActiveNote.Editor.Text )    // ActiveNote.Editor.Text is much faster than ActiveNote.Editor.Lines.Text
    else
       wc := GetWordCount( ActiveNote.Editor.SelText );

    if ( wc > 0 ) then begin
      if ( EditorOptions.WordsPerPage > 0 ) then
         p := Format(' / %.1f', [wc / EditorOptions.WordsPerPage] )
      else
         p := '';
      s := Format( ' W: %d', [wc] ) + p;
    end
    else
       s := ' W: 0';

    Form_Main.StatusBar.Panels[PANEL_CARETPOS].Text := s;
  end
  else begin
    if ( not EditorOptions.TrackCaretPos ) then begin
       Form_Main.StatusBar.Panels[PANEL_CARETPOS].Text := '';
       exit;
    end;
  end;

end; // UpdateWordCount


procedure CleanWordCount;
begin
    Form_Main.StatusBar.Panels[PANEL_CARETPOS].Text := ' W: ... / ...';
end;

procedure CheckWordCount (cleanWC: boolean= false);
begin
  if EditorOptions.WordCountTrack then
     if (ActiveNote.Editor.TextLength <= 30000) then
        UpdateWordCount
     else
         if cleanWC then
            CleanWordCount;
end;

procedure CheckWordCountWaiting;
const
   Waiting : string = ' (...) ';

begin
  if EditorOptions.WordCountTrack and (ActiveNote.Editor.TextLength > 30000) then
      with Form_Main.StatusBar.Panels[PANEL_CARETPOS] do begin
           if pos(waiting, Text) <> 1 then
              Text:= Waiting + Text;
           end;
end;


//=================================================================
// MatchBracket
//=================================================================
procedure MatchBracket;
type
  TDir = ( dirFwd, dirBack );
const
  OpenBrackets = '([{<';
  CloseBrackets = ')]}>';
var
  startch, seekch, curch : char;
  i, stack, startsel, curcol, curline : integer;
  p : TPoint;
  dir : TDir;
  Found : boolean;
begin
  if ( not Form_Main.HaveNotes( true, true ) and assigned( ActiveNote )) then exit;
  startsel := ActiveNote.Editor.SelStart;

  p := ActiveNote.Editor.CaretPos;

  if (( ActiveNote.Editor.Lines.Count = 0 ) or
      ( length( ActiveNote.Editor.Lines[p.y] ) = 0 )) then
      exit;

  if ( ActiveNote.Editor.SelLength = 0 ) then
    inc( p.x );
  startch := ActiveNote.Editor.Lines[p.y][p.x];

  i := pos( startch, OpenBrackets );
  if ( i > 0 ) then begin
    seekch := CloseBrackets[i];
    dir := dirFwd;
  end
  else begin
    i := pos( startch, CloseBrackets );
    if ( i > 0 ) then begin
      seekch := OpenBrackets[i];
      dir := dirBack;
    end
    else begin
      Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_Bracket_01;
      exit;
    end;
  end;

  // StatusBar.Panels[PANEL_HINT].Text := Format( 'line: %d col: %d %s -> %s', [p.y,p.x,startch, seekch] );

  curline := p.y;
  stack := 0;
  Found := false;

  case dir of
    dirFwd : begin
      curcol := p.x+1;
      while ( curline < ActiveNote.Editor.Lines.Count ) do begin
        while ( curcol <= length( ActiveNote.Editor.Lines[curline] )) do begin
          curch := ActiveNote.Editor.Lines[curline][curcol];
          if ( curch = startch ) then
             inc( stack )
          else
          if ( curch = seekch ) then begin
            if ( stack > 0 ) then
              dec( stack )
            else begin
              p.x := curcol;
              p.y := curline;
              Found := true;
              break;
            end;
          end;
          inc( curcol );
        end;
        if Found then
          break;
        curcol := 1;
        inc( curline );
      end;
    end;
    dirBack : begin
      curcol := p.x-1;
      while ( curline >= 0 ) do begin
        while( curcol > 0 ) do begin
          curch := ActiveNote.Editor.Lines[curline][curcol];
          if ( curch = startch ) then
            inc( stack )
          else
          if ( curch = seekch ) then begin
            if ( stack > 0 ) then
              dec( stack )
            else begin
              p.x := curcol;
              p.y := curline;
              Found := true;
              break;
            end;
          end;
          dec( curcol );
        end;
        if Found then
          break;
        dec( curline );
        if ( curline >= 0 ) then
          curcol := length( ActiveNote.Editor.Lines[curline] );
      end;
    end;
  end;

  if Found then begin
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_Bracket_02;
    with ActiveNote.Editor do begin
      SelStart := Perform( EM_LINEINDEX, p.y, 0 );
      SelStart := SelStart + pred( p.x );
      Perform( EM_SCROLLCARET, 0, 0 );
      SelLength := 1;
    end;
  end
  else
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_Bracket_03;
end; // MatchBracket


//=================================================================
// TrimBlanks
//=================================================================
procedure TrimBlanks( const TrimWhat : integer );
var
  i : integer;
  tempList : TStringList;
  s: string;
  wholeNote: boolean;

  function TrimString(const str: string): string;
  begin
      case TrimWhat of
        ITEM_TAG_TRIMLEFT :
            Result := trimLeft(str);
        ITEM_TAG_TRIMRIGHT :
            Result := trimRight(str);
        ITEM_TAG_TRIMBOTH :
           Result := trim(str);
      end;
  end;


begin
  if ( not Form_Main.HaveNotes( true, true ) and assigned( ActiveNote )) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;
  if ( ActiveNote.Editor.Lines.Count < 1 ) then exit;

  wholeNote:= false;
  if ( ActiveNote.Editor.SelLength = 0 ) then begin
    wholeNote:= true;
    if messagedlg(STR_Trim_01, mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes then exit;
  end;

  ActiveNote.Editor.BeginUpdate;
  Screen.Cursor := crHourGlass;

  try
      if wholeNote then
         s:= ActiveNote.Editor.GetTextRange(0, ActiveNote.Editor.TextLength)
      else
         s := ActiveNote.Editor.SelText;

      if Length(s) = 0 then Exit;

      if Pos(#13, s, 1) = 0 then
         s:= TrimString(s)

      else begin
         tempList := TStringList.Create;
         try
            tempList.Text := s;
            for i := 0 to tempList.Count-1 do
               tempList[i]:= TrimString(tempList[i]);
            s:= tempList.Text;
         finally
            tempList.Free;
         end;
      end;

      if wholeNote then
         ActiveNote.Editor.Text := s
      else
         ActiveNote.Editor.SelText := s;

      ActiveNote.Editor.HideKNTHiddenMarks(true);

  finally
    ActiveNote.Editor.EndUpdate;
    Screen.Cursor := crDefault;
    NoteFile.Modified := true;
    UpdateNoteFileState( [fscModified] );
  end;

end; // TrimBlanks

//=================================================================
// CompressWhiteSpace
//=================================================================
procedure CompressWhiteSpace;
const
  WhiteSpace : set of AnsiChar = [#9, #32];
var
  WasWhite : boolean;
  i, l : integer;
  s : string;
begin
  if ( not Form_Main.HaveNotes( true, true ) and assigned( ActiveNote )) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;
  if ( ActiveNote.Editor.Lines.Count < 1 ) then exit;


  if ( ActiveNote.Editor.SelLength = 0 ) then begin
     if ( messagedlg(STR_Compress_01, mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then
         exit;
  end;

  ActiveNote.Editor.BeginUpdate;
  Screen.Cursor := crHourGlass;
  WasWhite := false;

  try
    if ( ActiveNote.Editor.SelLength = 0 ) then begin

       for l := 0 to ActiveNote.Editor.Lines.Count-1 do begin
         if (ActiveNote.Editor.Lines[l] = '') then continue;

         WasWhite := false;
         i := 1;
         s := ActiveNote.Editor.Lines[l];

         while ( i <= length( s )) do begin
            if ( AnsiChar(s[i]) in WhiteSpace ) then begin
               if WasWhite then
                  delete( s, i, 1 )
               else
                  inc(i);
               WasWhite := true;
            end
            else begin
               WasWhite := false;
               inc(i);
            end;
         end;
         ActiveNote.Editor.Lines[l] := s;
         ActiveNote.Editor.HideKNTHiddenMarks(true);
       end;
       ActiveNote.Editor.SelStart := 0;

    end
    else begin
       s := ActiveNote.Editor.SelText;
       i := 1;
       while ( i <= length( s )) do begin
          if ( AnsiChar(s[i]) in WhiteSpace ) then begin
             if WasWhite then
                delete( s, i, 1 )
             else
                inc( i );
             WasWhite := true;
          end
          else begin
             WasWhite := false;
             inc( i );
          end;
       end;
       ActiveNote.Editor.SelText := s;
       ActiveNote.Editor.HideKNTHiddenMarks(true);
       ActiveNote.Editor.SelLength := 0;
    end;


  finally
     ActiveNote.Editor.EndUpdate;
     Screen.Cursor := crDefault;
     NoteFile.Modified := true;
     UpdateNoteFileState( [fscModified] );
  end;

end; // CompressWhiteSpace


//=================================================================
// EvaluateExpression
//=================================================================
procedure EvaluateExpression;
var
  src : string;
  i, l, lineindex : integer;
  MathParser : TMathParser;
begin
  with Form_Main do begin
      if ( not assigned( ActiveNote )) then exit;

      if ( ActiveNote.Editor.SelLength = 0 ) then
        with ActiveNote.Editor do begin
          lineindex := perform( EM_EXLINEFROMCHAR, 0, SelStart );
          SelStart  := perform( EM_LINEINDEX, lineindex, 0 );
          SelLength := perform( EM_LINEINDEX, lineindex + 1, 0 ) - ( SelStart+1 );
        end;

      src := trim( ActiveNote.Editor.SelText );

      if ( src = '' ) then begin
        ErrNoTextSelected;
        exit;
      end;

      UpdateLastCommand( ecEvaluateExpression );
      if IsRecordingMacro then
        AddMacroEditCommand( ecEvaluateExpression );

      if ( src[length( src )] = '=' ) then
        delete( src, length( src ), 1 );

      l := length( src );
      for i := 1 to l do begin
        if ( src[i] = ',' ) then
          src[i] := '.';
      end;

      MathParser := TMathParser.Create( Form_Main );
      try

        with MathParser do begin
          OnParseError := MathParserParseError;
          MathParser.ParseString := src;
          Parse;
        end;

        if ( not MathParser.ParseError ) then begin
          LastEvalExprResult := FloatToStrF(MathParser.ParseValue, ffGeneral, 15, 2);
          Clipboard.SetTextBuf( PChar( LastEvalExprResult ));
          StatusBar.Panels[PANEL_HINT].Text := STR_Eval_01 + LastEvalExprResult;
          MMEditPasteEval.Hint := STR_Eval_02 + LastEvalExprResult;

          if ( KeyOptions.AutoPasteEval and ( not NoteIsReadOnly( ActiveNote, false ))) then begin
              ActiveNote.Editor.SelStart := ActiveNote.Editor.SelStart + ActiveNote.Editor.SelLength;
              ActiveNote.Editor.SelText := #32 + LastEvalExprResult;
          end
          else
            if ( messagedlg( Format( STR_Eval_03, [src,LastEvalExprResult] ), mtInformation, [mbOK,mbCancel], 0 ) = mrOK ) then begin
               if ( not NoteIsReadOnly( ActiveNote, true )) then begin
                  ActiveNote.Editor.SelStart := ActiveNote.Editor.SelStart + ActiveNote.Editor.SelLength;
                  ActiveNote.Editor.SelText := #32 + LastEvalExprResult;
               end;
            end;
        end;

      finally
        MathParser.Free;
      end;
  end;

end; // EvaluateExpression

//=================================================================
// InsertSpecialCharacter
//=================================================================
procedure InsertSpecialCharacter;
begin
  with Form_Main do begin
      if ( not ( HaveNotes( true, true ) and assigned( ActiveNote ))) then
        exit;

      if ( Form_Chars = nil ) then begin
        Form_Chars := TForm_Chars.Create( Form_Main );
        with Form_Chars.FontDlg.Font do begin
          if ( KeyOptions.InsCharKeepFont and ( InsCharFont.Size > 0 )) then begin
            Name := InsCharFont.Name;
            Charset := InsCharFont.Charset;
            Size := InsCharFont.Size;
            Form_Chars.myFontChanged := true;
          end
          else begin
            Name := NoteSelText.Name;
            Charset := NoteSelText.Charset;
            Size := NoteSelText.Size;
          end;
        end;

        with Form_Chars do begin
          ShowHint := KeyOptions.ShowTooltips;
          CharInsertEvent := CharInsertProc;
          FormCloseEvent := Form_Main.Form_CharsClosed;
          myShowFullSet := KeyOptions.InsCharFullSet;
          if KeyOptions.InsCharWinClose then
            Button_Insert.ModalResult := mrOK
          else
            Button_Insert.ModalResult := mrNone;
        end;
      end;

      try
        Form_Chars.Show;
      except
        on E : Exception do
        begin
          showmessage( E.Message );
        end;
      end;
  end;

end; // InsertSpecialCharacter

//=================================================================
// InsertPictureOrObject
//=================================================================
procedure InsertPictureOrObject( const AsPicture : boolean );
var
  Pict: TPicture;
  wasmodified : boolean;
  OpenPictureDlg : TOpenPictureDialog;
  SelStartOrig, SelLengthOrig: integer;
  NewName: string;
begin
  if ( not Form_Main.HaveNotes(true, true)) then exit;
  if ( not assigned(ActiveNote)) then exit;
  if Form_Main.NoteIsReadOnly(ActiveNote, true) then exit;
  if ActiveNote.PlainText then exit;

  wasmodified:= false;


  OpenPictureDlg := TOpenPictureDialog.Create( Form_Main );
  try
    if AsPicture then
       with OpenPictureDlg do begin
          Options := [ofHideReadOnly,ofPathMustExist,ofFileMustExist];
          Title:= STR_Img_01;
          Filter:= STR_Img_02 + FILTER_IMAGES;

          if Execute then begin
             if (ImagesManager.StorageMode <> smEmbRTF) and NoteSupportsRegisteredImages then begin
                if ActiveNote.Editor.SelLength > 0 then
                   CheckToSelectLeftImageHiddenMark (ActiveNote.Editor);
             end;

             NewName:= ExtractFilename(FileName);
             if (not KeyOptions.ImgDefaultLinkMode) and (not ImagesManager.CheckUniqueName (NewName)) then begin
                var FileList: TStringList;
                try
                   FileList:= TStringList.Create;
                   FileList.Add (FileName);
                   FileDropped(nil, FileList);
                finally
                   FileList.Free;
                end;
             end
             else
                ImagesManager.InsertImage(FileName, ActiveNote, not KeyOptions.ImgDefaultLinkMode, NewName);

              // See comments before TImageManager.InsertImage
              {
              Pict := TPicture.Create;
              try
                 Pict.LoadFromFile(FileName);
                 Clipboard.Assign(Pict);
                 Activenote.Editor.PasteIRichEditOLE(0);
              finally
                 Pict.Free;
                 wasmodified:= true;
              end;
              }
          end;
       end
    else begin
      if (ImagesManager.StorageMode <> smEmbRTF) and NoteSupportsRegisteredImages then begin
         CheckToSelectLeftImageHiddenMark (ActiveNote.Editor, SelStartOrig, SelLengthOrig);
         if Activenote.Editor.InsertObjectDialog then
            wasmodified := true
         else
            if SelStartOrig >= 0 then
               ActiveNote.Editor.SetSelection(SelStartOrig, SelStartOrig + SelLengthOrig, true);
      end
      else
         if Activenote.Editor.InsertObjectDialog then
            wasmodified := true;
    end;

  finally
    if wasmodified then begin
       NoteFile.Modified:= true;
       UpdateNoteFileState([fscModified]);
    end;
    OpenPictureDlg.Free;
  end;
end; // InsertPictureOrObject



//=================================================================
// KeepOnlyLeadingKNTHiddenMark
//=================================================================
function KeepOnlyLeadingKNTHiddenCharacters(const txt: string): string;
var
   SS, SL, pR: integer;
begin
    Result:= txt;
    if txt[1] = KNT_RTF_HIDDEN_MARK_L_CHAR then begin
       SS:= ActiveNote.Editor.SelStart;
       SL:= ActiveNote.Editor.SelLength;
       pR:= Pos(KNT_RTF_HIDDEN_MARK_R_CHAR, txt, 1);
       Result:= Copy(txt, pR+1);
       ActiveNote.Editor.SelStart:=  SS + pR;
       ActiveNote.Editor.SelLength:= SL - pR;
    end;
    Result:= RemoveKNTHiddenCharacters(Result);
end;


//=================================================================
// ArabicToRoman
//=================================================================
procedure ArabicToRoman;
var
  s : string;
  i : integer;
begin
  if ( not assigned( ActiveNote )) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;

  s := ActiveNote.Editor.SelText;
  if ( s = '' ) then
    InputQuery( STR_ConvDec_01, STR_ConvDec_02, s )
  else
     s:= KeepOnlyLeadingKNTHiddenCharacters(s);

  if ( s = '' ) then exit;

  try
    s := trim( s );
    i := strtoint( s );
    s := DecToRoman( i );
  except
    messagedlg( Format( STR_ConvDec_03, [s] ), mtError, [mbOK], 0 );
    exit;
  end;

  ActiveNote.Editor.SelText := s;
end; // ArabicToRoman;

//=================================================================
// RomanToArabic
//=================================================================
procedure RomanToArabic;
var
  s : string;
  i : integer;
begin
  if ( not assigned( ActiveNote )) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;
  i := -1;

  s := ActiveNote.Editor.SelText;
  if ( s = '' ) then
    InputQuery( STR_ConvRoman_01, STR_ConvRoman_02, s )
  else
     s:= KeepOnlyLeadingKNTHiddenCharacters(s);

  if ( s = '' ) then exit;

  try
    s := AnsiUpperCase( trim( s ));
    i := RomanToDec( s );
  except
    messagedlg( Format( STR_ConvRoman_03, [s] ), mtError, [mbOK], 0 );
    exit;
  end;

  if ( i < 0 ) then exit;

  ActiveNote.Editor.SelText := inttostr( i );
end; // RomanToArabic


//=================================================================
// ShowStatistics
//=================================================================
procedure ShowStatistics;
var
  s, title : string;
  lista : TStringList;
  i, l, len, numChars, numSpaces, numAlpChars,
  numLines, numWords, numNodes : integer;
  WasAlpha : boolean;
  ch : char;
begin
  if ( not Form_Main.HaveNotes( true, true ) and assigned( ActiveNote )) then exit;

  Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_Statistics_01;

  screen.Cursor := crHourGlass;

  lista := TStringList.Create;

  try

    if ( ActiveNote.Editor.SelLength > 0 ) then begin
      lista.Text := ActiveNote.Editor.SelText;
      title := STR_Statistics_02;
    end
    else begin
      lista.Text := ActiveNote.Editor.Lines.Text;
      title := STR_Statistics_03;
    end;

    numLines := lista.count;

    numChars := 0;
    numSpaces := 0;
    numAlpChars := 0;
    numWords := 0;

    for l := 0 to lista.count-1 do begin
      s := lista[l];
      len := length( s );
      inc( numChars, len );
      WasAlpha := false;

      for i := 1 to len do begin
        ch := s[i];
        if IsCharAlpha( ch ) then begin
          inc( numAlpChars );
          WasAlpha := true;
        end
        else begin
          if ( ch in [#9,#32] ) then
            inc( numSpaces );
          if WasAlpha then
            inc( numWords );
          WasAlpha := false;
        end;
      end;

      if WasAlpha then
        inc( numWords );
    end;


  finally
    lista.Free;
    screen.Cursor := crDefault;
  end;

  s := format(STR_Statistics_04,[Title,inttostr( numChars ),inttostr( numAlpChars ),
                inttostr( numSpaces ),inttostr( numWords ),inttostr( numLines )]);

  if ( ActiveNote.Kind = ntTree ) then begin
    numNodes := TTreeNote( ActiveNote ).TV.Items.Count;
    s := s + Format( STR_Statistics_05,  [numNodes] );
  end;

  Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format(
    STR_Statistics_06, [numChars, numAlpChars, numWords] );

  if ( messagedlg( s + STR_Statistics_07, mtInformation, [mbOK,mbCancel], 0 ) = mrOK ) then
    Clipboard.SetTextBuf( Pchar( s ));

end; // ShowStatistics


//=================================================================
// WordWebLookup
//=================================================================
procedure WordWebLookup;
var
  WordWeb : TFreeWordWeb;
  myWord, newWord : string;

begin
  if ( not ( assigned( ActiveNote ) and ( ActiveNote.FocusMemory = focRTF ))) then exit;

  if ShiftDown then
    myWord := ''
  else begin
    if ( ActiveNote.Editor.SelLength > 0 ) then
      myWord := trim( ActiveNote.Editor.SelText )
    else
      myWord := ActiveNote.Editor.GetWordAtCursor( true );
  end;

  if ( myWord = '' ) then begin
    if ( not InputQuery( STR_WordWeb_01, STR_WordWeb_02, myWord )) then
      exit;
  end;

  WordWeb := nil;
  try
    WordWeb := TFreeWordWeb.Create( Form_Main );
  except
    On E : Exception do begin
      messagedlg( STR_WordWeb_03 + E.Message, mtError, [mbOK], 0 );
      exit;
    end;
  end;

  newWord := '';

  try
    try
      WordWeb.CloseOnCopy := true;
      WordWeb.LookupWord := myWord;

      if WordWeb.Execute then
        newWord := WordWeb.ReturnWord
      else
        newWord := '';

      if (( newWord <> '' ) and ( newWord <> myWord )) then
         ActiveNote.Editor.SelText := newWord + #32;

    except
      On E : Exception do begin
        Form_Main.RTFMWordWeb.Enabled := false;
        Form_Main.TB_WordWeb.Enabled := false;
        messagedlg( STR_WordWeb_03 + E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end;
  finally
    WordWeb.Free;
  end;

end; // WordWebLookup

//=================================================================
// EnableOrDisableUAS
//=================================================================
procedure EnableOrDisableUAS;
var
  UASPath : string;
begin
  try
    if KeyOptions.UASEnable then begin
      UASPath := GetUASPath; // let UAS find itself

      if ( not fileexists( UASPath )) then begin
        UASPath := KeyOptions.UASPath; // maybe we already have it configured

        if ( not fileexists( UASPath )) then begin
          // ...we don't so ask user and check answer
          if ( InputQuery( STR_UAS_01, STR_UAS_02, UASPath ) and
               fileexists( UASPath )) then
            KeyOptions.UASPath := UASPath // found it, so store it for later
          else begin
            // user canceled or entered invalid path, so bail out
            messagedlg( STR_UAS_03, mtError, [mbOK], 0 );
            KeyOptions.UASEnable := false;
            exit;
          end;
        end;
      end;

      if LoadUAS( UASPath ) then begin
        UAS_Window_Handle := GetUASWnd;
        // check if really loaded
        KeyOptions.UASEnable := ( UAS_Window_Handle <> 0 );
      end
      else
        KeyOptions.UASEnable := false;

      if KeyOptions.UASEnable then
        // success
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_UAS_04
      else begin
        // something went wrong
        KeyOptions.UASEnable := false;
        if ( messagedlg( STR_UAS_05, mtWarning, [mbOK,mbCancel], 0 ) = mrOK ) then
          GoDownloadUAS;
      end;
    end
    else begin
      if ( UAS_Window_Handle <> 0 ) then begin
        SendMessage(GetUASWnd,WM_CLOSE,0,0);
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_UAS_06;
      end;
    end;

  finally
    if ( not KeyOptions.UASEnable ) then
      UAS_Window_Handle := 0;
    Form_Main.MMToolsUAS.Checked := KeyOptions.UASEnable;
    Form_Main.MMToolsUASConfig.Enabled := KeyOptions.UASEnable;
    Form_Main.MMToolsUASConfig.Visible := KeyOptions.UASEnable;
  end;

end; // EnableOrDisableUAS

//=================================================================
// ConfigureUAS
//=================================================================
procedure ConfigureUAS;
var
  ptCursor : TPoint;
begin
  if ( UAS_Window_Handle = 0 ) then begin
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_UAS_07;
    exit;
  end;

  GetCursorPos( ptCursor );
  SetForegroundWindow( UAS_Window_Handle );
  PostMessage( UAS_Window_Handle, WM_APP+$2001, ptCursor.x, ptCursor.y );

end; // ConfigureUAS


//=================================================================
// TryToDetectAndSolveHTMLtoRTFbadConv
//=================================================================

const
  RTFCONVERSON_MAX_TEXTSIZE_TO_CHECK: integer= 15;
  RTFCONVERSON_MAX_DIF_TO_IGNORE: integer= 5;


procedure TryToDetectAndSolveHTMLtoRTFbadConv (const Editor: TRxRichEdit; posI: integer);
var
  posF: integer;
  PlainText: string;

begin
    // posI must receive Editor.SelStart before paste the suspicious text

    if not (Clipboard.HasHTMLformat and Clipboard.HasRTFformat) then
       exit;

    posF := Editor.SelStart;
    if (posF-posI) > RTFCONVERSON_MAX_TEXTSIZE_TO_CHECK then
       exit;

    {
      If the clipboard has HTML and RTF formats, it is possible that the RTF format comes from our conversion (with TWebBrowser)
      and in some cases it can be wrong. At least, as verified with the following site, whose html seems to be odd: stackoverflow.com
      For example, consider the following URLs and try to paste several paragraphs altogether. 
      Most of them will produce '<>' or '', others '<<<' or something similar and a few can truncate some text.

       https://stackoverflow.com/questions/46991729/how-to-load-a-stringstream-with-utf8-characters-to-a-stringlist-in-android
       https://stackoverflow.com/questions/11122197/why-does-memo-lines-use-tstrings-instead-of-tstringlist

      Even the conversion with MSWord of the HTML copied by the browser to the clipboard it is no ok
      If in examples we copy (and paste) only fragments of certain paragraphs or just snippets of code, it seems to be ok.


        When it's time to find out how to identify this problematic HTMLs with 'correct' ones, it is necessary to view how nor problematic
        HTMLs behaves, compared with the plain text format offered by clipboard.

        For example, in a page interpreted ok, we could select only a few characters, and observe de following difference:
        (from : http://www.cryer.co.uk/brian/delphi/twebbrowser/get_HTML.htm)

          'L. '#$D'I'#$D            <- Pasted from our conversion, seen in plain text with Editor.SelText
          'L.'#$D#$A'I'#$D#$A       <- plain text include in the clipboard:  Clipboard.AsText

        => Clipboard.AsText will include 2 bytes for each line break, whereas the Editor counts a returns 1.
        => In the exmple, the text copied included the final two characters ('L.') and the first one ('I') of two consecutive
           bullet list items. In this case we how a space is added after de dot

        On this other example, copied from the same site (http://www.cryer.co.uk/brian/delphi/twebbrowser/navigate_frameset.htm), I selected
        a text including the following selection, where another difference is observed:

          (...) that does not contain a frameset:</p>
              <blockquote>
                <pre>procedure TMyForm.NavigateFrameset (...)

          From our conversion, seen with Editor.SelText:
            (...) that does not contain a frameset:'#$D'procedure TMyForm.NavigateFrameset (...)

          From Clipboard.AsText:
            (...) that does not contain a frameset:'#$D#$A#$D#$A'procedure TMyForm.NavigateFrameset (...)

        => Apart from the 'noraml' 2 bytes per break line, Clipboard.AsText has added another line break just before blockquuote.
          The text selected wasn't little and this was the only difference..

        We need to identify the problematic situation in a fast a 'cheap' way. Even in 'correct' situations may be differences
        in the length (once counted just 1 byte per line break), although small.
        On the other side, problematic pages will lead to huge differences, because in this cases normal thing is that nothing
        is pasted (or something like '<>' o '<<<' )

        In RTFCONVERSON_MAX_DIF_TO_IGNORE we are setting the max. difference that assume can be present normal in normal situations.
        We are going to conservative
        To not penalize normal situations, in RTFCONVERSON_MAX_TEXTSIZE_TO_CHECK we are setting the max size of the text pasted
        that we are going to analize. Problematic cases normally will paste nothing or something ridiculous, but as in some cases
        text can be truncated, we are going to anlyze text until certain length. 
        Even in cases with text below RTFCONVERSON_MAX_TEXTSIZE_TO_CHECK we will only get an extra PlainText:= Clipboard.AsText 
        (if all is finally ok)


     NOTES:
       1.
       The problem can arise in Clipboard.TryOfferRTF, and manifest itself with Editor.PutRtfText or PasteBestAvailableFormat

       Clipboard.TryOfferRTF is also called from other points:
         kn_Main.MMEditPasteSpecialClick

       Even PasteBestAvailableFormat can call Clipboard.TryOfferRTF, and PasteBestAvailableFormat is called from:
         - this method (TryPasteRTF)
         - kn_MacroMng.PerformCmd (ecPaste)

       From kn_MacroMng.PerformCmdPastePlain could call TryPasteRTF

       When copying and playing from inside KeyNote, we don't need to convert from HTML to RTF. Neither if we are copying and pasting
       from an external RTF source. These cases are completely unaffected (also in terms of performance).
       The only points we need to control are the calls originated in ClipCapture and Web Clip, boths coming from
       kn_EditorUtils.PasteOnClipCap, and from PerformCmd (ecPaste), when we have HTML but not RTF format in clipboard.

       The action Paste Special.... (kn_Main.MMEditPasteSpecialClick) could also insert incorrect RTF if this format is selected.
       But there the user (we) are consciuous of the problem, and can select another format. Besides, RTF generated can
       be in some situations imperfect, but include parcial valid RTF conversions, like hyperlinks, for example.

       2. RTFCONVERSON_MAX_TEXTSIZE_TO_CHECK (2000 characters at this moment) it is not so small because can ocurre that some
         text is truncated, but other is maintained. Also, the text pasted can contains hidden text (because of the presence
         of hyperlinks).
         This hidden text can do that the lenth of the pasted text be equal or greater that the one in Clipboard.AsText, even
         if there is truncated text.
         This case could be better analyzed using Editor.SelVisibleText, to compare it with Clipboard.AsText
         But this situation is probably less habitual, can also be more obvious to the user, and we don't want to penalize normal
         cases. Besides, it will be still possible to use 'Paste Special...'

       3. Although it is not usual, we can find character #11 ($B) when looking at RTF content. And this can have associated,
          in the plain text version, multiple spaces

          ASCII Table (https://www.ascii-code.com/)
          ...
          09 	00001001 	&#9; 	HT 	Horizontal Tab
          0A 	00001010 	&#10; 	LF 	Line Feed
          0B 	00001011 	&#11; 	VT 	Vertical Tab
          0C 	00001100 	&#12; 	FF 	Form Feed
          0D 	00001101 	&#13; 	CR 	Carriage Return

          So, we are going to count the number of characters non controls nor space (> 32)

       4. http://www.cryer.co.uk/brian/delphi/twebbrowser/navigate_frameset.htm
          If we copy from "How to navigate a frameset" including part of the code snippet, unformatted version whill show a dashed line,
          not present in RTF version. When counting characters in both strings, even only >32, unformatted version will have
          80 more, because of the dash line...
          And this page can be paste perfectly ok in RTF

       5. CONCLUSION:
          It's not easy to identify without doubt when RTF is truncating characters. It is not worth spending a lot of resources
          on distinguish this situation, clearly unusual.
          I'll to control only the cases more obvious and problematic, where return RTF text is like '', '<>' or similar.
          The user will always have the RTF and Text formats available in the clipboard (Paste Special..)
     }


     {
      fragments used..
      PlainText:= Clipboard.AsText;
      Dif:= (length(PlainText) - CountChars(#13, PlainText))  - (posF-posI);
      RTFText:= Editor.GetTextRange(posI, posF);
      RTFText:= Editor.SelVisibleText;
      Dif:= CountNonControlCharsNoSpace(PlainText) - CountNonControlCharsNoSpace(RTFText);
     }


     PlainText:= Clipboard.TryAsText;
     if (Length(PlainText) - (posF-posI))  > RTFCONVERSON_MAX_DIF_TO_IGNORE then begin
        Editor.SuspendUndo;
        try
           Editor.SelStart  := posI;
           Editor.SelLength := posF - posI;

           Editor.SelText := PlainText;
           Editor.SelStart  := posI + Editor.SelLength;

           Editor.SelLength := 0;

        finally
           Editor.ResumeUndo;
        end;
     end;

end;


//=================================================================
// TryPasteRTF
//=================================================================
procedure TryPasteRTF(Note: TTabNote; HTMLText: AnsiString='');
var
  RTFText: AnsiString;
  posI: integer;
  Editor: TRxRichEdit;

begin
    Editor:= Note.Editor;
    posI := Editor.SelStart;

    RTFText:= Clipboard.TryOfferRTF(HTMLText, Editor.SelAttributes);            // Can return '' if clipboard already contained RTF Format
    if RTFText <> '' then
        Editor.PutRtfText(RTFText, true)
    else
        PasteBestAvailableFormat(Note, false, false);      // false: don't to try to offer RTF (Maybe it's already available), false: don't try to detect and correct HTML to RTF (will be done here)

    TryToDetectAndSolveHTMLtoRTFbadConv(Editor, posI);

end;


//=================================================================
// PasteBestAvailableFormat
//=================================================================

{ 13/06/2023:
  So far KeyNote has been trying to offer RTF Format in the clipboard when we clicked
  on Paste Special... if that format wasn't available yet and there was HTML format.
  That conversion from HTML to RTF was also realized (if possible) when using Clipboard Capture
  and Web Clip, because of the necessity to process the HTML in the clipboard.
  Also, if we had selected the option 'Paste external as Plain text' then, when pasting,
  that 'plain' was interpreted according with what was configured in 'Plain text mode', and
  here, a conversion from HTML to RTF could be done.

  So, if we just paste normal from a web page, without being selected 'Paste external as Plain text'
  we might end up pasting completely plain text, while selecting 'Paste external as Plain text' we
  might paste more or rich RTF (depending on 'Plain text mode') (...). Confusing.

  The reason? Browsers (at least actual ones), when copying selected text to the clipboard
  not include RTF Format.

  Now, in most cases, when we paste, if there is a HTML format content but not RTF, KeyNote
  will try to get RTF format from HTML (as we have been doing on Paste Special...)
}

procedure PasteBestAvailableFormatInEditor (Editor: TRxRichEdit; Note: TTabNote; TryOfferRTF: boolean= True; CorrectHTMLtoRTF: boolean = False);
var
  posI, pos: integer;
  RTFText: AnsiString;
  WasCopiedByKNT, ClipbHasRTFFormat: boolean;

begin
    WasCopiedByKNT:= ClipboardContentWasCopiedByKNT;         // If not -> it will also make LastCopiedIDImage <-0

    { If we are pasting an image copied from a browser, we must give preference to checking if it contains an image,
      because in those cases we will also find HTML content with the address and alternative text of the image, 
      and if we do not do so we will always end up pasting the conversion to RTF from that alt text }
    if Clipboard.HasFormat(CF_BITMAP) and NoteSupportsImages then begin
       ImagesManager.InsertImageFromClipboard (Note);
       exit;
    end;

    posI:= Editor.SelStart;

    if TryOfferRTF then
       RTFText:= Clipboard.TryOfferRTF('', Editor.SelAttributes);

    // If we have added RTF, we are going to use it instead of calling PasteFromClipboard, because the default font and size
    // will have set in this returned RTF text, not in the text in the clipboard
    if RTFText <> '' then
       Editor.PutRtfText(RTFText, true)

    else begin
      // If I paste text and images from WordPad it may appear as a "Wordpad Document" format, and in that case it only appears to paste the text.
      // It seems best to try to paste the RTF format if it is available
      ClipbHasRTFFormat:= Clipboard.HasFormat(CFRtf);
      if ClipbHasRTFFormat then
         Editor.PasteIRichEditOLE(CFRtf)
      else
         Editor.PasteFromClipboard;       //Editor.PasteIRichEditOLE(0);

      if (ImagesManager.StorageMode <> smEmbRTF) and ClipbHasRTFFormat and NoteSupportsRegisteredImages then begin
         pos:= Editor.SelStart;
         { We will have treated Copy or Cut in a special way if SelLength=1 because this implied that an image could be selected individually and we
           want to be able to offer it as such and not only as RTF (which would happen if we copy the image to the clipboard along with the hidden mark ) 
          (Although this image copied in this way - without a mark - when it is in PNG or JPB, the control does not offer it as BMP but only as a Metafile
           and not all programs recognize it. For example, GreenShot does recognize it, but Photoshop or WordPad itself They don't recognize it --WordPad
           does paste it, but because it uses the RTF format) }
         if (not WasCopiedByKNT) or ((pos = posI + 1) and (LastCopiedIDImage<>0)) then
            ImagesManager.ProcessImagesInClipboard (Editor, Note, posI, LastCopiedIDImage);
      end;
    end;

    if CorrectHTMLtoRTF then
       TryToDetectAndSolveHTMLtoRTFbadConv(Editor, posI);

end;


procedure PasteBestAvailableFormat (Note: TTabNote; TryOfferRTF: boolean= True; CorrectHTMLtoRTF: boolean = False);
begin
    PasteBestAvailableFormatInEditor(Note.Editor, Note, TryOfferRTF, CorrectHTMLtoRTF);
end;


//=================================================================
// ToggleClipCap
//=================================================================
procedure ToggleClipCap( const TurnOn : boolean; const aNote : TTabNote );
var
  nodemode : string;
begin
  with Form_Main do begin
      if ( not HaveNotes( true, true )) then exit;
      if ( not assigned( aNote )) then exit;

      ClipCapCRC32 := 0; // reset test value
      LastURLPasted:= '';

      try
        try
          if TurnOn then begin
            // turn ON clipboard capture for active note
            if aNote.ReadOnly then begin
              TB_ClipCap.Down := false;
              PopupMessage( STR_ClipCap_01, mtInformation, [mbOK], 0 );
              exit;
            end;

            if ( aNote.Kind = ntTree ) then begin
              if ( Initializing or ( not ClipOptions.TreeClipConfirm )) then
                ClipCapNode := GetCurrentNoteNode
              else begin
                if ClipOptions.PasteAsNewNode then
                  nodemode := STR_ClipCap_02
                else
                  nodemode := STR_ClipCap_03;
                case MessageDlg( Format(
                  STR_ClipCap_04,
                  [TABNOTE_KIND_NAMES[aNote.Kind], nodemode] ), mtConfirmation, [mbOK,mbCancel], 0 ) of
                  mrOK : begin
                    ClipCapNode := GetCurrentNoteNode;
                  end;
                  else begin
                    TB_ClipCap.Down := false;
                    exit;
                  end;
                end;
              end;
            end
            else
              ClipCapNode := nil;

            if ( NoteFile.ClipCapNote <> nil ) then begin
              // some other note was clipcap before
              // NoteFile.ClipCapNote.TabSheet.Caption := NoteFile.ClipCapNote.Name;
              NoteFile.ClipCapNote := nil;
              ClipCapActive := false;
              SetClipCapState( false );
            end;
            NoteFile.ClipCapNote := aNote;
            Pages.MarkedPage := NoteFile.ClipCapNote.TabSheet;
            SetClipCapState( true );
            ClipCapActive := ( NoteFile.ClipCapNote <> nil );

          end
          else begin
            // turn OFF clipboard capture
            ClipCapActive := false;
            ClipCapNode := nil;
            if ( NoteFile.ClipCapNote = aNote ) then begin
              // restore note name on the tab
              Pages.MarkedPage := nil;
              SetClipCapState( false );
            end
            else begin
              // showmessage( 'Error: Tried to turn off ClipCap for a non-active note.' );
            end;
            NoteFile.ClipCapNote := nil;
          end;
        except
          on e : exception do begin
            messagedlg( E.Message, mtError, [mbOK], 0 );
            NoteFile.ClipCapNote := nil;
            Pages.MarkedPage := nil;
          end;
        end;
      finally
        Pages.Invalidate; // force redraw to update "MarkedPage" tab color
        MMNoteClipCapture.Checked := ( NoteFile.ClipCapNote <> nil );
        TMClipCap.Checked := MMNoteClipCapture.Checked;
        StatusBar.Panels[PANEL_HINT].Text := STR_ClipCap_05 + TOGGLEARRAY[(NoteFile.ClipCapNote <> nil)];
      end;
  end;

end; // ToggleClipCap


//=================================================================
// SetClipCapState
//=================================================================
procedure SetClipCapState( const IsOn : boolean );
begin
  with Form_Main do begin
      _IS_CHAINING_CLIPBOARD := true;
      try
        try
          case IsOn of
            true : begin
              ClipCapNextInChain := SetClipboardViewer( Handle );
              LoadTrayIcon( ClipOptions.SwitchIcon );
            end;
            false : begin
              ChangeClipboardChain(Handle, ClipCapNextInChain );
              ClipCapNextInChain := 0;
              LoadTrayIcon( false );
            end;
          end;
        except
          ClipCapNextInChain := 0;
          if assigned( NoteFile ) then
            NoteFile.ClipCapNote := nil;
          Pages.MarkedPage := nil;
          TB_ClipCap.Down := false;
          LoadTrayIcon( false );
        end;
      finally
        _IS_CHAINING_CLIPBOARD := false;
      end;
  end;

end; // SetClipCapState


var
   cacheURLs:   TStringList;
   cacheTitles: TStringList;

const
   CACHE_URLs_MAX: Integer = 20;
   CACHE_URLs_REDUCE_TO: Integer = 10;
   MAX_TIME_TO_GET_TITLE: Integer = 2000;   // milliseconds


procedure CleanCacheURLs (OnlyWithoutTitle: boolean);
var
   I: Integer;
begin
   if not assigned(cacheURLs) then exit;

   if not OnlyWithoutTitle then begin
     cacheURLs.Clear;
     cacheTitles.Clear;
   end
   else begin
      for I := cacheURLs.Count - 1 downto 0  do
          if cacheTitles[i] = '' then begin
             cacheURLs.Delete(i);
             cacheTitles.Delete(i);
          end;
   end;
end;

function GetTitleFromURL (const URL: String; TryForLimitedTime: boolean): String;
var
  I: Integer;
  MaxTime: Integer;
begin
   if URL = '' then begin
      Result:= '';
      exit;
   end;

   if assigned(cacheURLs) then begin
      I:= cacheURLs.IndexOf(URL);
      if I >= 0 then begin
         Result:= cacheTitles[i];
         exit;
      end;
   end
   else begin
      cacheURLs:= TStringList.Create;
      cacheTitles:= TStringList.Create;
   end;

   if not assigned(_IE) then
     _IE:= TWebBrowserWrapper.Create(Form_Main);

   MaxTime:= 0;
   if TryForLimitedTime then
      MaxTime := MAX_TIME_TO_GET_TITLE;
   Result:= _IE.GetTitleFromURL(URL, MaxTime);

   cacheURLs.Add(URL);
   cacheTitles.Add(Result);

   if cacheURLs.Count >= CACHE_URLs_MAX then       // If we reach MAX
      for I := 1 to CACHE_URLs_REDUCE_TO do begin  // remove the older ones
         cacheURLs.Delete(0);
         cacheTitles.Delete(0);
      end;

end;


//=================================================================
// PasteOnClipCap
//=================================================================
procedure PasteOnClipCap (ClpStr: string);
var
  DividerString: string;
  i, j, len : integer;
  wavfn: string;
  myNodeName : string;
  myTreeNode, myParentNode : TTreeNTNode;
  PasteOK, PasteOnlyURL : boolean;
  SourceURLStr : string;
  TitleURL : string;
  AuxStr : string;
  HTMLClipboard: string;
  Note: TTabNote;
  Editor: TTabRichEdit;

  GetTitle: boolean;
  _S, _SL, _U: integer;
  TryForLimitedTime: boolean;
  CLIPSOURCE_Token: string;
  IgnoreCopiedText: boolean;

  Using2ndDivider: boolean;

 const
    URL_YOUTUBE: string = 'https://www.youtube.com';

begin
  HTMLClipboard:= '';

  with Form_Main do begin
        myTreeNode := nil;
        SourceURLStr:= '';

        Note:= NoteFile.ClipCapNote;
        Editor:= Note.Editor;


        // TrayIcon.Icon := TrayIcon.Icons[1];
        LoadTrayIcon( not ClipOptions.SwitchIcon ); // flash tray icon

        Editor.OnChange := nil;
        NoteFile.Modified := true; // bugfix 27-10-03

        DividerString := ClipOptions.Divider;
        Using2ndDivider:= False;


        i:= pos(CLIPSECONDDIV, DividerString);
        if (i > 0) then begin
           HTMLClipboard:= Clipboard.AsHTML;
           SourceURLStr := Clipboard.GetURLFromHTML (HTMLClipboard);
           if (SourceURLStr <> '') and (SourceURLStr = lastURLPasted) then begin
              delete(DividerString, 1, i + length(CLIPSECONDDIV)-1);
              Using2ndDivider:= true;
           end
           else begin
              delete(DividerString, i, 9999);
              lastURLPasted:= SourceURLStr;
           end;
        end;


        { *1
          From now it seems not possible to obtain the title of a page of YouTube, because the html returned by TWebBrowser
          in this case does'nt contain the final html page
          To avoid loosing time and to avoid wasting we URLs cache, will not try to return title if URL begins with https://www.youtube.com
          Now it is easier to click three times in one word of the video title, to select it

          If the URL begins with 'https://www.youtube.com' then
          - Will not try to look for the title using WebBroser
          - If the text selected includes only 1 line and it hasn't more that 100 characters, then it wil be assumed as the title

          (https://www.smperth.com/resources/youtube/youtube-character-limit/)
          }

         IgnoreCopiedText:= false;       // *1


        _S  := pos(CLIPSOURCE, DividerString);
        _SL := pos(CLIPSOURCE_LIMITED, DividerString);
        _U  := pos(CLIPSOURCE_ONLY_URL, DividerString);

        if ClipOptions.InsertSourceURL then begin
           if HTMLClipboard ='' then begin
              HTMLClipboard:= Clipboard.AsHTML;
              SourceURLStr := Clipboard.GetURLFromHTML (HTMLClipboard);
              //TitleURL:= Clipboard.GetTitleFromHTML (HTMLClipboard);       // This data is not currently copied to the clipboard
           end;

           if (_S=0) and (_SL=0) and (_U > 0) then begin
               GetTitle:= False;
               CLIPSOURCE_Token:= CLIPSOURCE_ONLY_URL;
           end
           else
           if (_SL > 0) then begin
              GetTitle:= True;
              TryForLimitedTime:= True;
              CLIPSOURCE_Token:= CLIPSOURCE_LIMITED;
           end
           else begin
              GetTitle:= True;
              TryForLimitedTime:= false;
              CLIPSOURCE_Token:= CLIPSOURCE;
           end;

           if GetTitle then begin
              if pos(URL_YOUTUBE, SourceURLStr) = 1 then begin  // *1
                 TitleURL:= Clipboard.TryAsText;
                 i:= pos(#13, TitleURL);
                 j:= length(TitleURL);
                 if (j > 100) or ((i > 0) and (i <= j-2)) then   // If select caption clicking 3 times, #13#10 will be added..
                    TitleURL:= ''
                 else
                    IgnoreCopiedText:= True;
              end
              else
                 TitleURL:= GetTitleFromURL (SourceURLStr, TryForLimitedTime);
           end
           else
              TitleURL:= '';
           end
        else begin
            SourceURLStr := '';
            if _S > 0 then
               delete( DividerString, _S, length(CLIPSOURCE));
            if _SL > 0 then
               delete( DividerString, _SL, length(CLIPSOURCE_LIMITED));
            if _U > 0 then
               delete( DividerString, _U, length(CLIPSOURCE_ONLY_URL));
        end;

        PasteOK := true;

        try
          StatusBar.Panels[PANEL_HINT].Text := STR_ClipCap_06;
          PasteOnlyURL:= false;
          if ClipOptions.URLOnly and (SourceURLStr <> '') then begin
             AuxStr:= copy(ClpStr,1,30);
             if (GetWordCount(AuxStr)=1) and (not HasNonAlphaNumericOrWordDelimiter(AuxStr)) and (not Using2ndDivider) then begin
               PasteOnlyURL:= true;
               if IgnoreCopiedText then            // YouTube...  See *1
                  TitleURL:= '';
             end;
          end;

          if not PasteOnlyURL and ( Note.Kind = ntTree ) then begin
              // ClipCapNode := nil;
              if ClipOptions.PasteAsNewNode then begin
                 if (pos(CLIPDATECHAR, DividerString)=0) and (pos(CLIPTIMECHAR, DividerString)=0) and ((SourceURLStr = '') or (pos(CLIPSOURCE_Token, DividerString)=0)) then
                    DividerString:= '';   // Si no hay que separar de nada y el propia cadena de separación no incluye fecha, ni hora ni se va a mostrar el origen, ignorarla

                 if ( ClipCapNode <> nil ) then
                    myParentNode := TTreeNote( Note ).TV.Items.FindNode( [ffData], '', ClipCapNode )
                 else
                    myParentNode := nil;

                 case ClipOptions.ClipNodeNaming of
                   clnDefault : myNodeName := '';
                   clnClipboard : myNodeName:= Clipboard.TryGetFirstLine(TREENODE_NAME_LENGTH_CAPTURE);
                   clnDateTime :  myNodeName:= FormatDateTime(FormatSettings.ShortDateFormat + #32 + FormatSettings.ShortTimeFormat, now );
                 end;

                 if assigned( myParentNode ) then
                    myTreeNode := TreeNoteNewNode( TTreeNote( Note ), tnAddChild, myParentNode, myNodeName, true )
                 else
                    myTreeNode := TreeNoteNewNode( TTreeNote( Note ), tnAddLast, nil, myNodeName, true );

              end
              else begin
                 myTreeNode := TTreeNote( Note ).TV.Selected;
                 if ( not assigned( myTreeNode )) then
                    myTreeNode := TreeNoteNewNode( TTreeNote( Note ), tnAddLast, nil, myNodeName, true );
              end;
              if ( not assigned( myTreeNode )) then begin
                 PasteOK := false;
                 PopupMessage( STR_ClipCap_07, mtError, [mbOK], 0 );
                 exit;
              end;
          end;


          try
              Editor.BeginUpdate;

              if not PasteOnlyURL then begin

                for i := 1 to length( DividerString ) do begin
                   if ( DividerString[i] = CLIPDIVCHAR ) then
                      DividerString[i] := #13;
                end;

                i:= 1;
                repeat
                    i := posEx( CLIPSOURCEDELIMITER, DividerString, i );
                    if ( i > 0 ) then begin
                       len:= length(CLIPSOURCEDELIMITER);
                       if SourceURLStr = '' then begin
                          j := PosEx( CLIPSOURCEDELIMITER, DividerString, i + 2);
                          if j > 0 then
                             len:= len + j - i;
                       end;
                       delete( DividerString, i, len);
                    end;
                until i = 0;


                i := pos( CLIPDATECHAR, DividerString );
                if ( i > 0 ) then begin
                  delete( DividerString, i, length( CLIPDATECHAR ));
                  if ( length( DividerString ) > 0 ) then
                    insert( FormatDateTime( KeyOptions.DateFmt, now ), DividerString, i )
                  else
                    DividerString := FormatDateTime( KeyOptions.DateFmt, now );
                end;

                i := pos( CLIPTIMECHAR, DividerString );
                if ( i > 0 ) then begin
                  delete( DividerString, i, length( CLIPTIMECHAR ));
                  if ( length( DividerString ) > 0 ) then
                    insert( FormatDateTime( KeyOptions.TimeFmt, now ), DividerString, i )
                  else
                    DividerString := FormatDateTime( KeyOptions.TimeFmt, now );
                end;

                  // do not add leading blank lines if pasting in a new tree node
                if (( Note.Kind <> ntRTF ) and ClipOptions.PasteAsNewNode ) then
                   DividerString := trimleft( DividerString );

                i := pos( CLIPSOURCE_DOMAIN, DividerString );
                if ( i > 0 ) then begin
                  delete( DividerString, i, length( CLIPSOURCE_DOMAIN ));
                  if not Note.PlainText then begin
                     AuxStr:= DomainFromHttpURL(SourceURLStr, TitleURL);        // If TitleURL = '' will always return '', because URL will be shown
                     if ( length( DividerString ) > 0 ) then
                        insert(AuxStr , DividerString, i )
                     else
                        DividerString := AuxStr;
                  end;
                end;

                i := pos( CLIPSOURCE_Token, DividerString );
                if ( i > 0 ) then begin
                    delete( DividerString, i, length( CLIPSOURCE_Token ));
                    if ( SourceURLStr <> '' ) then begin
                       Editor.SelText := Copy(DividerString,1, i-1);
                       delete(DividerString, 1, i-1);   // Remaining divider will be pasted after source url
                    end;
                end
                else begin
                   Editor.SelText := DividerString;
                   DividerString:= '';
                end;
                Editor.SelStart :=  Editor.SelStart + Editor.SelLength;
              end;

              if ((SourceURLStr <> '' ) or PasteOnlyURL) and (not Using2ndDivider) then begin
                   InsertURL(SourceURLStr, TitleURL, Note);
                   if PasteOnlyURL then begin
                     Editor.SelText := #13;
                     Editor.SelStart :=  Editor.SelStart + 1;
                   end
                   else
                   if (DividerString = '') then  // Si no se ha indicado dónde colocar el origen o se ha puesto justo al final del separador añadir un salto de línea
                       DividerString:= #13;
              end;

              if not PasteOnlyURL then begin
                 if (DividerString <> '') then begin
                     Editor.SelText := DividerString;
                     Editor.SelStart :=  Editor.SelStart + Editor.SelLength;
                 end;

                if not IgnoreCopiedText then
                   if not ClipOptions.PasteAsText and not Note.PlainText then
                      TryPasteRTF(Note, HTMLClipboard)
                   else
                      PerformCmdPastePlain(Note, ClpStr, HTMLClipboard, false, ClipOptions.MaxSize);

              end;

           finally
              Editor.EndUpdate;
           end;


          if Note <> ActiveNote then
             Note.EditorToDataStream;


        finally
          Editor.OnChange := RxRTFChange;
          if PasteOK then begin
            NoteFile.Modified := true;
            UpdateNoteFileState( [fscModified] );

            if assigned ( myTreeNode ) then
               TNoteNode( myTreeNode.Data ).RTFModified := true;

            StatusBar.Panels[PANEL_HINT].Text := STR_ClipCap_09;
            wavfn := extractfilepath( application.exename ) + 'clip.wav';
            if ( ClipOptions.PlaySound and fileexists( wavfn )) then
               sndplaysound( PChar( wavfn ), SND_FILENAME or SND_ASYNC or SND_NOWAIT );

            Application.ProcessMessages;
            sleep( ClipOptions.SleepTime * 100 ); // in tenths of a second; default: 5 = half a second
            LoadTrayIcon( ClipOptions.SwitchIcon ); // unflash tray icon
          end;
        end;
  end;

end; // PasteOnClipCap


//=================================================================
// PasteAsWebClip
//=================================================================
procedure PasteAsWebClip (const PasteAsText: boolean);
var
  oldClipCapNote : TTabNote;
  oldClipCapNode : TNoteNode;
  oldDividerString : string;
  oldAsText, oldTreeClipConfirm, oldInsertSourceURL, oldClipPlaySound, oldPasteAsNewNode : boolean;
  oldMaxSize, oldSleepTime : integer;
begin
  if ( _IS_CAPTURING_CLIPBOARD or _IS_CHAINING_CLIPBOARD ) then exit;
  if ( not Form_Main.HaveNotes( true, true )) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;

  oldClipPlaySound:= ClipOptions.PlaySound;
  oldDividerString := ClipOptions.Divider;
  oldInsertSourceURL := ClipOptions.InsertSourceURL;
  oldMaxSize := ClipOptions.MaxSize;
  oldSleepTime := ClipOptions.SleepTime;
  oldTreeClipConfirm := ClipOptions.TreeClipConfirm;
  oldAsText := ClipOptions.PasteAsText;
  oldPasteAsNewNode:= ClipOptions.PasteAsNewNode;

  oldClipCapNote := NoteFile.ClipCapNote;
  oldClipCapNode := ClipCapNode;

  try
    ClipOptions.PlaySound:= false;
    ClipOptions.PasteAsNewNode:= false;
    ClipOptions.MaxSize := 0;
    ClipOptions.SleepTime := 0;
    ClipOptions.TreeClipConfirm := false;
    ClipOptions.InsertSourceURL := true;
    if ClipOptions.WCDivider <> '' then                // Let use Divider also for Web Clip if WCDivider = ''
       ClipOptions.Divider := ClipOptions.WCDivider;

    ClipOptions.PasteAsText := PasteAsText;

    NoteFile.ClipCapNote := ActiveNote;
    ClipCapNode := nil;

    PasteOnClipCap(Clipboard.TryAsText);

  finally
    ClipOptions.PlaySound:= oldClipPlaySound;
    ClipOptions.Divider := oldDividerString;
    ClipOptions.InsertSourceURL := oldInsertSourceURL;
    ClipOptions.MaxSize := oldMaxSize;
    ClipOptions.SleepTime := oldSleepTime;
    ClipOptions.TreeClipConfirm := oldTreeClipConfirm;
    ClipOptions.PasteAsText := oldAsText;
    ClipOptions.PasteAsNewNode:= oldPasteAsNewNode;

    NoteFile.ClipCapNote := oldClipCapNote;
    ClipCapNode := oldClipCapNode;
  end;

end; // PasteAsWebClip


//=================================================================
// PasteIntoNew
//=================================================================
procedure PasteIntoNew( const AsNewNote : boolean );
var
  oldCNT : integer;
  CanPaste : boolean;
  myNodeName : string;
  myTreeNode : TTreeNTNode;
begin
  if ( not Form_Main.HaveNotes( true, false )) then exit;
  oldCNT := NoteFile.Notes.Count;
  CanPaste := false;

  try
    if AsNewNote then begin
      NewNote( true, true, ntRTF );
      CanPaste := ( OldCNT < NoteFile.Notes.Count );
    end
    else begin
      if ( assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree )) then begin
        case ClipOptions.ClipNodeNaming of
          clnDefault : myNodeName := '';
          clnClipboard : myNodeName:= Clipboard.TryGetFirstLine(TREENODE_NAME_LENGTH_CAPTURE);
          clnDateTime :  myNodeName:= FormatDateTime(FormatSettings.ShortDateFormat + #32 + FormatSettings.ShortTimeFormat, now );
        end;

        myTreeNode := TreeNoteNewNode( nil, tnAddAfter, GetCurrentTreeNode, myNodeName, false );
        CanPaste := assigned( myTreeNode );
      end;
    end;
  except
    exit;
  end;

  if CanPaste then begin
    if ShiftDown then
      PerformCmd( ecPastePlain )
    else
      PerformCmd( ecPaste );
  end;

end; // PasteIntoNew


//=================================================================
// PrintRTFNote
//=================================================================
procedure PrintRTFNote;
var
  PrintRE : TRichEdit;
  MS : TMemoryStream;
  PrintAllNodes : boolean;
  tNote : TTreeNote;
  myTreeNode : TTreeNTNode;
  myNoteNode : TNoteNode;
begin
  if ( not Form_Main.HaveNotes( true, true )) then exit;
  if ( not assigned( ActiveNote )) then exit;
  if ( not assigned( Form_Main.RichPrinter )) then    // [dpv]
  begin
      try                                     // [DPV]
         Form_Main.RichPrinter := TRichPrinter.Create(Form_Main);
      except
        On E : Exception do
        begin
          showmessage( E.Message );
          exit;
        end;
      end;
  end;
  PrintAllNodes := false;

  if (( ActiveNote.Kind = ntTree ) and ( TTreeNote( ActiveNote ).TV.Items.Count > 1 )) then
  case messagedlg(STR_Print_01,
      mtConfirmation, [mbYes,mbNo,mbCancel], 0 ) of
    mrYes : PrintAllNodes := true;
    mrNo : PrintAllNodes := false;
    else
      exit;
  end;

  if Form_Main.PrintDlg.Execute then begin
    Form_Main.RichPrinter.Title := RemoveAccelChar( ActiveNote.Name );

    PrintRe := TRichEdit.Create( nil );
    MS := TMemoryStream.Create;

    try

      screen.Cursor := crHourGlass;

      with PrintRe do begin
        parent := Form_Main;
        visible := false;
        WordWrap := false;
      end;

      if (( ActiveNote.Kind = ntRTF ) or ( not PrintAllNodes )) then begin

        if KeyOptions.SafePrint then begin
          ActiveNote.Editor.Print( RemoveAccelChar( ActiveNote.Name ));
          (*
          ActiveNote.Editor.Lines.SaveToStream( MS );
          MS.Position := 0;
          PrintRE.Lines.LoadFromStream( MS );
          if ( ActiveNote.Editor.SelLength > 0 ) then
          begin
            PrintRE.SelStart := ActiveNote.Editor.SelStart;
            PrintRE.SelLength := ActiveNote.Editor.SelLength;
          end;
          RichPrinter.PrintRichEdit( TCustomRichEdit( PrintRE ), 1 );
          *)
        end
        else
          Form_Main.RichPrinter.PrintRichEdit( TCustomRichEdit( ActiveNote.Editor ), 1 );
      end
      else begin
        tNote := TTreeNote( ActiveNote );
        myTreeNode := tNote.TV.Items.GetFirstNode;
        if myTreeNode.Hidden then myTreeNode := myTreeNode.GetNextNotHidden;   // [dpv]
        while assigned( myTreeNode ) do begin
          myNoteNode := TNoteNode( myTreeNode.Data );
          if assigned( myNoteNode ) then begin
            myNoteNode.Stream.Position := 0;
            PrintRE.Lines.LoadFromStream( myNoteNode.Stream );
            if KeyOptions.SafePrint then
              PrintRE.Print( RemoveAccelChar( ActiveNote.Name ))
            else
              Form_Main.RichPrinter.PrintRichEdit( TCustomRichEdit( PrintRE ), 1 );
          end;
          //myTreeNode := myTreeNode.GetNext;          // [dpv]
          myTreeNode := myTreeNode.GetNextNotHidden;   // [dpv]
        end;
      end;

    finally
      screen.Cursor := crDefault;
      if assigned( PrintRE ) then PrintRE.Free;
      if assigned( MS ) then MS.Free;
    end;

  end;
end; // PrintRTFNote


//=================================================================
// PrintRTFNote
//=================================================================
procedure RunSpellcheckerForNote;
var
  AJBSpell : TAJBSpell;
begin
  if ( not assigned( ActiveNote )) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;

  AJBSpell := TAJBSpell.Create( Form_Main );
  try
    try
      ActiveNote.Editor.SelectAll;
      ActiveNote.Editor.CopyToClipboard;
      if AJBSpell.CheckClipboardSpell then begin
        if ( messagedlg( STR_Print_02, mtConfirmation, [mbOK, mbCancel], 0 ) = mrOK ) then
          ActiveNote.Editor.PasteFromClipboard;
      end;
    except
      on E : Exception do
        messagedlg( E.Message, mtError, [mbOK], 0 );
    end;

  finally
    AJBSpell.Free;
  end;
end; // RunSpellcheckerForNote


function GetEditorZoom (Editor: TRxRichEdit = nil): integer;
var
  W, L : integer;
begin
  result := DefaultEditorProperties.DefaultZoom;
  if ( _LoadedRichEditVersion < 3 ) then exit; // cannot zoom
  if not assigned( Editor ) and assigned(ActiveNote) then
      Editor:= ActiveNote.Editor;
  if ( not assigned( Editor )) then exit;

  SendMessage( Editor.Handle, EM_GETZOOM, integer(@w), integer(@l) );
  if ( w = 0 ) then w := 1;
  if ( l = 0 ) then l := 1;
  result := makepercentage( w, l );
end; // GetEditorZoom

procedure SetEditorZoom( Editor: TRxRichEdit; ZoomValue : integer; ZoomString : string; Increment: integer= 0 ); overload;
var
  CurrentZoom : integer;
  NewZoom : integer;
  p : integer;
begin
  if ( not assigned( Editor )) then exit;
  if ( _LoadedRichEditVersion < 3 ) then exit; // cannot zoom

  CurrentZoom := GetEditorZoom (Editor);
  NewZoom := DefaultEditorProperties.DefaultZoom; // initialize

  // if integer argument is greater than zero, use the integer as zoom value.
  // if integer is <=0, string argument is '' and increment argrument is not 0, apply increment
  // if string argument is empty, do nothing
  // Otherwise try to derive zoom value from the string argument.
  
  if ( ZoomValue > 0 ) then
    NewZoom := ZoomValue

  else if ( ZoomString = '') and (Increment <> 0) then
    NewZoom := CurrentZoom + Increment

  else begin
    ZoomString := trim( ZoomString );
    if ( ZoomString = '' ) then
        //NewZoom := 100 // reset is empty string passed
        exit         // don't do anything

    else begin
      p := pos( '%', ZoomString );
      if ( p > 0 ) then
        delete( ZoomString, p, 1 );
      try
        NewZoom := strtoint( ZoomString );
      except
        on E : Exception do begin
          messagedlg( STR_Zoom_01 + E.Message, mtError, [mbOK], 0 );
          NewZoom := CurrentZoom;
        end;
      end;
    end;
  end;


  // Sanity check:
  if ( NewZoom > 1000 ) then
     NewZoom := 1000 // max zoom
  else
  if ( NewZoom <= 0 ) then
     NewZoom := _ZOOM_MIN; // min zoom

  SendMessage( Editor.Handle, EM_SETZOOM, NewZoom, 100 );

end; // ZoomEditor

procedure SetEditorZoom( ZoomValue : integer; ZoomString : string; Increment: integer= 0);  overload;
var
  Note: TTabNote;
  Editor: TRxRichEdit;
  i: integer;
begin
  if not assigned( NoteFile ) then exit;
  if ( _LoadedRichEditVersion < 3 ) then exit; // cannot zoom

  try
      if CtrlDown then
         SetEditorZoom (ActiveNote.Editor, ZoomValue, ZoomString, Increment)

      else begin
         for i := 0 to NoteFile.Notes.Count -1 do
            SetEditorZoom (NoteFile.Notes[i].Editor, ZoomValue, ZoomString, Increment);

         SetEditorZoom (Form_Main.Res_RTF, ZoomValue, ZoomString, Increment)
      end;

  finally
    if assigned(ActiveNote) then begin
      _LastZoomValue := GetEditorZoom (ActiveNote.Editor);
      Form_Main.Combo_Zoom.Text := Format('%d%%', [_LastZoomValue] );
    end;
  end;
end;


procedure SetMargins();
begin

  if Form_Main.MMAlternativeMargins.Checked then
     ActiveNote.Editor.SetMargins(KeyOptions.MarginAltLeft, KeyOptions.MarginAltRight)
  else
     ActiveNote.Editor.SetMargins(0, 0);
end;


{
 Determines the indent of the current line or paragraph (depending on paragraphMode
 parameter) and the indent that could be applied to next line with AutoIndent.
 If paragraphMode = False (lineMode) then
   LineStr: the line (as shown in Editor) where the caret is positioned
 If paragraphMode = True:
   LineStr: the line (as shown in Editor) corresponding to the beginning of
   paragraph where the caret is positioned

  PosBegin: the position of the first character of LineStr, in the Editor
}
procedure GetIndentInformation(Editor: TRxRichEdit;
                               var Indent: integer; var NextIndent: integer;
                               var LineStr: string;
                               var posBegin : integer;
                               paragraphMode: boolean= false);
var
   SelS, Col: integer;
   LineBegin, LineCaret : integer;
begin
    with Editor do begin
        SelS:= SelStart;
        LineCaret:= PerForm(EM_EXLINEFROMCHAR,0, SelS );
        posBegin:=  Perform(EM_LINEINDEX, LineCaret,0 );
        Col:= SelS - posBegin;

        if paragraphMode then begin
           posBegin  := FindText(#13, SelS, -1, [stBackward]) + 1;
           LineBegin := PerForm( EM_EXLINEFROMCHAR,0, posBegin);
        end
        else
           LineBegin:= LineCaret;

        LineStr:= lines[LineBegin];
        Indent:= GetIndentOfLine(LineStr);
        if (Indent > Col) and (LineBegin = LineCaret) then
           NextIndent:= col
        else
           NextIndent:= Indent;
    end;
end;

function NumberOfLineFeed(Str: string): integer;
var
  i: integer;
begin
   Result:= 0;
   i:= 1;
   while i <= Length(Str) do begin
      if (Str[i] = #10) then
         Inc(Result);
      Inc(i);   
   end;
end;


procedure ShowTipOfTheDay;
var
  TipDlg : TGFTipDlg;
  wasiconic : boolean;
begin
  if ( not fileexists( TIP_FN )) then begin
    PopupMessage( Format(STR_Tip_01, [extractfilename( TIP_FN )] ), mtInformation, [mbOK], 0 );
    // turn tips off, so that we don't get this error message
    // every time KeyNote starts. (e.g. if user deleted the .tip file)
    KeyOptions.TipOfTheDay := false;
    exit;
  end;
  wasiconic := ( IsIconic(Application.Handle) = TRUE );
  if wasiconic then
    Application.Restore;
  Application.BringToFront;

  TipDlg := TGFTipDlg.Create( Form_Main );
  try
    with TipDlg do begin
      ShowAtStartup := KeyOptions.TipOfTheDay;
      TipFile := TIP_FN;
      DlgCaption := Program_Name + STR_Tip_02;
      PanelColor := _GF_CLWINDOW;
      TipFont.Size := 10;
      TipTitleFont.Size := 12;
      SelectedTip := KeyOptions.TipOfTheDayIdx;
      Execute;
      KeyOptions.TipOfTheDayIdx := SelectedTip;
      KeyOptions.TipOfTheDay := ShowAtStartup;
    end;
  finally
    TipDlg.Free;
  end;
  if wasiconic then
    Application.Minimize;
end; // ShowTipOfTheDay


initialization
   cacheURLs:= nil;
   cacheTitles:= nil;
   LastURLPasted:= '';

end.
