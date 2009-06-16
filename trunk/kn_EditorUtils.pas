unit kn_EditorUtils;

interface
uses
 kn_NoteObj;

    // glossary management
    procedure ExpandTermProc;
    procedure AddGlossaryTerm;
    procedure EditGlossaryTerms;

    function GetWordCount( const t : string ) : longint;
    procedure UpdateWordCount;

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
    procedure ToggleClipCap( const TurnOn : boolean; const aNote : TTabNote );
    procedure SetClipCapState( const IsOn : boolean );
    procedure PasteOnClipCap;
    procedure PasteAsWebClip;
    procedure PasteIntoNew( const AsNewNote : boolean );

    procedure PrintRTFNote;

    procedure RunSpellcheckerForNote;

    function GetEditorZoom : integer;
    procedure SetEditorZoom( ZoomValue : integer; ZoomString : string );

    procedure ShowTipOfTheDay;

implementation
uses
  { Borland units }
  Windows, Messages, SysUtils, Classes,
  Controls, Forms, Dialogs, Clipbrd,ComCtrls,
  RichEdit, mmsystem, Graphics,ExtDlgs,
  { 3rd-party units }
  BrowseDr, TreeNT, Parser,FreeWordWeb, UAS, RxGIF,RichPrint,
  RxRichEd,AJBSpeller,
  { Own units - covered by KeyNote's MPL}
  gf_misc, gf_files, gf_Const,
  gf_strings, gf_miscvcl,
  kn_INI, kn_const, kn_Cmd, kn_Msgs,
  kn_Info, kn_FileObj, kn_NewNote,
  kn_NodeList, kn_ExpandObj, kn_MacroMng,
  Kn_Global, kn_Chars, kn_NoteMng, kn_ClipUtils,
  kn_ExpTermDef, kn_Glossary, kn_VCLControlsMng,
  kn_NoteFileMng, kn_Main, kn_TreeNoteMng, GFTipDlg;


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
  STR_Statistics_05 = 'Number of nodes in tree: %d';
  STR_Statistics_06 = 'Chars: %d  Alph: %d  Words: %d';
  STR_Statistics_07 = 'Clik OK to copy information to clipboard.';
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
  STR_ClipCap_08 = '[source: ';
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
      if ( not ( assigned( GlossaryList ) and assigned( ActiveNote ) and ( ActiveNote.FocusMemory = focRTF ))) then
      begin
        StatusBar.Panels[PANEL_HINT].Text := STR_Gloss_01;
        exit;
      end;
      if NoteIsReadOnly( ActiveNote, true ) then exit;

      UpdateLastCommand( ecExpandTerm );
      if IsRecordingMacro then
        AddMacroEditCommand( ecExpandTerm );

      if ( ActiveNote.Editor.SelLength = 0 ) then
        w := ActiveNote.Editor.GetWordAtCursorNew( true )
      else
        w := ActiveNote.Editor.SelText;
      wordlen := length( w );

      if ( length( w ) = 0 ) then
      begin
        StatusBar.Panels[PANEL_HINT].Text := STR_Gloss_02;
        exit;
      end;

      replw := GlossaryList.Values[w];

      if ( replw = '' ) then
      begin
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
  if ( not assigned( GlossaryList )) then
  begin
    showmessage( Format(STR_Gloss_04, [Glossary_FN] ));
    exit;
  end;

  nstr := '';
  vstr := '';

  if assigned( ActiveNote ) then
  begin
    if ( ActiveNote.Editor.SelLength > 0 ) then
      nstr := trim( copy( ActiveNote.Editor.SelText, 1, 255 ))
    else
      nstr := ActiveNote.Editor.GetWordAtCursorNew( true );
    if ( nstr <> '' ) then
      vstr := GlossaryList.Values[nstr];
  end;

  Form_TermDef := TForm_TermDef.Create( Form_Main );
  try
    with Form_TermDef do
    begin
      Edit_Term.Text := nstr;
      Edit_Exp.Text := vstr;
      if ( ShowModal = mrOK ) then
      begin
        nstr := trim( Edit_Term.Text );
        vstr := trim( Edit_Exp.Text );

        if (( nstr <> '' ) and ( vstr <> '' ) and ( nstr <> vstr )) then
        begin

          if ( GlossaryList.IndexOfName( nstr ) >= 0 ) then
          begin
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
            GlossaryList.SaveToFile( Glossary_FN );
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
const
  WordDelimiters = [#9, #10, #13, #32];
var
  i, len : longint;
begin
  len := length( t );
  result := 0;
  if ( len > 0 ) then
  begin
    i := 1;
    repeat
      if ( t[i] in WordDelimiters ) then
      begin
        inc( i );
        continue;
      end
      else
        inc( result );

      // scan to end of word
      while (( i <= len ) and ( not ( t[i] in WordDelimiters ))) do
      begin
        inc( i );
      end;
    until ( i > len );
  end;
end; // GetWordCount

//=================================================================
// UpdateWordCount
//=================================================================
procedure UpdateWordCount;
var
  p, s : string;
  wc : longint;
begin
  if ( not assigned( ActiveNote )) then exit;

  if EditorOptions.WordCountTrack then
  begin

    if ( ActiveNote.Editor.SelLength = 0 ) then
      wc := GetWordCount( ActiveNote.Editor.Lines.Text )
    else
      wc := GetWordCount( ActiveNote.Editor.SelText );

    if ( wc > 0 ) then
    begin
      if ( EditorOptions.WordsPerPage > 0 ) then
      begin
        p := ' / ' + FloatToStr( wc / EditorOptions.WordsPerPage );
      end
      else
      begin
        p := '';
      end;
      s := Format( ' W: %d', [wc] ) + p;
    end
    else
    begin
      s := ' W: 0';
    end;

    Form_Main.StatusBar.Panels[PANEL_CARETPOS].Text := s;
  end
  else
  begin
    if ( not EditorOptions.TrackCaretPos ) then
    begin
      Form_Main.StatusBar.Panels[PANEL_CARETPOS].Text := '';
      exit;
    end;
  end;

end; // UpdateWordCount


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
  if ( i > 0 ) then
  begin
    seekch := CloseBrackets[i];
    dir := dirFwd;
  end
  else
  begin
    i := pos( startch, CloseBrackets );
    if ( i > 0 ) then
    begin
      seekch := OpenBrackets[i];
      dir := dirBack;
    end
    else
    begin
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
      while ( curline < ActiveNote.Editor.Lines.Count ) do
      begin
        while ( curcol <= length( ActiveNote.Editor.Lines[curline] )) do
        begin
          curch := ActiveNote.Editor.Lines[curline][curcol];
          if ( curch = startch ) then
          begin
            inc( stack );
          end
          else
          if ( curch = seekch ) then
          begin
            if ( stack > 0 ) then
            begin
              dec( stack );
            end
            else
            begin
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
      while ( curline >= 0 ) do
      begin
        while( curcol > 0 ) do
        begin
          curch := ActiveNote.Editor.Lines[curline][curcol];
          if ( curch = startch ) then
          begin
            inc( stack );
          end
          else
          if ( curch = seekch ) then
          begin
            if ( stack > 0 ) then
            begin
              dec( stack );
            end
            else
            begin
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

  if Found then
  begin
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_Bracket_02;
    with ActiveNote.Editor do
    begin
      SelStart := Perform( EM_LINEINDEX, p.y, 0 );
      SelStart := SelStart + pred( p.x );
      Perform( EM_SCROLLCARET, 0, 0 );
      SelLength := 1;
    end;
  end
  else
  begin
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_Bracket_03;
  end;
end; // MatchBracket


//=================================================================
// TrimBlanks
//=================================================================
procedure TrimBlanks( const TrimWhat : integer );
var
  i : integer;
  tempList : TStringList;
begin
  if ( not Form_Main.HaveNotes( true, true ) and assigned( ActiveNote )) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;
  if ( ActiveNote.Editor.Lines.Count < 1 ) then exit;

  if ( ActiveNote.Editor.SelLength = 0 ) then
  begin
    if ( messagedlg( STR_Trim_01,
      mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;
  end;


  ActiveNote.Editor.Lines.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try

    if ( ActiveNote.Editor.SelLength = 0 ) then
    begin
      for i := 0 to ActiveNote.Editor.Lines.Count-1 do
      begin
        case TrimWhat of
          ITEM_TAG_TRIMLEFT : begin
              ActiveNote.Editor.Lines[i] := trimleft( ActiveNote.Editor.Lines[i] );
          end;
          ITEM_TAG_TRIMRIGHT : begin
            ActiveNote.Editor.Lines[i] := trimright( ActiveNote.Editor.Lines[i] );
          end;
          ITEM_TAG_TRIMBOTH : begin
            ActiveNote.Editor.Lines[i] := trim( ActiveNote.Editor.Lines[i] );
          end;
        end;
      end;
      ActiveNote.Editor.SelStart := 0;
    end
    else
    begin
      tempList := TStringList.Create;
      try
        tempList.Text := ActiveNote.Editor.SelText;
        if ( tempList.Count > 0 ) then
        for i := 0 to tempList.Count-1 do
        begin
          case TrimWhat of
            ITEM_TAG_TRIMLEFT : begin
                tempList[i] := trimleft( tempList[i] );
            end;
            ITEM_TAG_TRIMRIGHT : begin
              tempList[i] := trimright( tempList[i] );
            end;
            ITEM_TAG_TRIMBOTH : begin
              tempList[i] := trim( tempList[i] );
            end;
          end;
        end;
        ActiveNote.Editor.SelText := tempList.Text;
      finally
        tempList.Free;
      end;
    end;

  finally
    ActiveNote.Editor.Lines.EndUpdate;
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
  WhiteSpace : set of char = [#9, #32];
var
  WasWhite : boolean;
  i, l : integer;
  s : string;
begin
  if ( not Form_Main.HaveNotes( true, true ) and assigned( ActiveNote )) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;
  if ( ActiveNote.Editor.Lines.Count < 1 ) then exit;


  if ( ActiveNote.Editor.SelLength = 0 ) then
  begin
    if ( messagedlg( STR_Compress_01,
      mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;
  end;

  ActiveNote.Editor.Lines.BeginUpdate;
  Screen.Cursor := crHourGlass;
  WasWhite := false;

  try
    if ( ActiveNote.Editor.SelLength = 0 ) then
    begin

      for l := 0 to ActiveNote.Editor.Lines.Count-1 do
      begin
        if ( ActiveNote.Editor.Lines[l] = '' ) then continue;
        WasWhite := false;
        i := 1;
        s := ActiveNote.Editor.Lines[l];

        while ( i <= length( s )) do
        begin
          if ( s[i] in WhiteSpace ) then
          begin
            if WasWhite then
              delete( s, i, 1 )
            else
              inc( i );
            WasWhite := true;
          end
          else
          begin
            WasWhite := false;
            inc( i );
          end;
        end;
        ActiveNote.Editor.Lines[l] := s;
      end;
      ActiveNote.Editor.SelStart := 0;
    end
    else
    begin
      s := ActiveNote.Editor.SelText;
      i := 1;
      while ( i <= length( s )) do
      begin
        if ( s[i] in WhiteSpace ) then
        begin
          if WasWhite then
            delete( s, i, 1 )
          else
            inc( i );
          WasWhite := true;
        end
        else
        begin
          WasWhite := false;
          inc( i );
        end;
      end;
      ActiveNote.Editor.SelText := s;
      ActiveNote.Editor.SelLength := 0;
    end;

  finally
    ActiveNote.Editor.Lines.EndUpdate;
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
      with ActiveNote.Editor do
      begin
        lineindex := perform( EM_EXLINEFROMCHAR, 0, SelStart );
        SelStart  := perform( EM_LINEINDEX, lineindex, 0 );
        SelLength := perform( EM_LINEINDEX, lineindex + 1, 0 ) - ( SelStart+1 );
      end;

      src := trim( ActiveNote.Editor.SelText );

      if ( src = '' ) then
      begin
        ErrNoTextSelected;
        exit;
      end;

      UpdateLastCommand( ecEvaluateExpression );
      if IsRecordingMacro then
        AddMacroEditCommand( ecEvaluateExpression );

      if ( src[length( src )] = '=' ) then
        delete( src, length( src ), 1 );

      l := length( src );
      for i := 1 to l do
      begin
        if ( src[i] = ',' ) then
          src[i] := '.';
      end;

      MathParser := TMathParser.Create( Form_Main );
      try

        with MathParser do
        begin
          OnParseError := MathParserParseError;
          MathParser.ParseString := src;
          Parse;
          LastEvalExprResult := FloatToStrF(ParseValue, ffGeneral, 15, 2);
        end;

        if ( not MathParser.ParseError ) then
        begin
          Clipboard.SetTextBuf( PChar( LastEvalExprResult ));
          StatusBar.Panels[PANEL_HINT].Text := STR_Eval_01 + LastEvalExprResult;
          MMEditPasteEval.Hint := STR_Eval_02 + LastEvalExprResult;

          if ( KeyOptions.AutoPasteEval and ( not NoteIsReadOnly( ActiveNote, false ))) then
          begin
            begin
              ActiveNote.Editor.SelStart := ActiveNote.Editor.SelStart + ActiveNote.Editor.SelLength;
              ActiveNote.Editor.SelText := #32 + LastEvalExprResult;
            end;
          end
          else
          begin
            if ( messagedlg( Format( STR_Eval_03, [src,LastEvalExprResult] ), mtInformation, [mbOK,mbCancel], 0 ) = mrOK ) then
            begin
              if ( not NoteIsReadOnly( ActiveNote, true )) then
              begin
                ActiveNote.Editor.SelStart := ActiveNote.Editor.SelStart + ActiveNote.Editor.SelLength;
                ActiveNote.Editor.SelText := #32 + LastEvalExprResult;
              end;
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

      if ( Form_Chars = nil ) then
      begin
        Form_Chars := TForm_Chars.Create( Form_Main );
        with Form_Chars.FontDlg.Font do
        begin
          if ( KeyOptions.InsCharKeepFont and ( InsCharFont.Size > 0 )) then
          begin
            Name := InsCharFont.Name;
            Charset := InsCharFont.Charset;
            Size := InsCharFont.Size;
            Form_Chars.myFontChanged := true;
          end
          else
          begin
            Name := NoteSelText.Name;
            Charset := NoteSelText.Charset;
            Size := NoteSelText.Size;
          end;
        end;

        with Form_Chars do
        begin
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
begin
  if ( not Form_Main.HaveNotes( true, true )) then exit;
  if ( not assigned( ActiveNote )) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;
  wasmodified := false;

  OpenPictureDlg := TOpenPictureDialog.Create( Form_Main );
  try
    if AsPicture then
    begin
      with OpenPictureDlg do
      begin
        Options := [ofHideReadOnly,ofPathMustExist,ofFileMustExist];
        Title := STR_Img_01;
        Filter := Format( '%s|%s|%s', [
          GraphicFilter(TBitmap),
          GraphicFilter(TGIFImage),
          // GraphicFilter(TJPEGImage),
          GraphicFilter(TMetafile)
        ]);
        if Execute then
        begin
          Pict := TPicture.Create;
          try
            Pict.LoadFromFile(FileName);
            Clipboard.Assign(Pict);
            Activenote.Editor.PasteFromClipboard;
          finally
            Pict.Free;
            wasmodified := true;
          end;
        end;
      end;
    end
    else
    begin
      if Activenote.Editor.InsertObjectDialog then
        wasmodified := true;
    end;

  finally
    if wasmodified then
    begin
      NoteFile.Modified := true;
      UpdateNoteFileState( [fscModified] );
    end;
    OpenPictureDlg.Free;
  end;
end; // InsertPictureOrObject



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
    InputQuery( STR_ConvDec_01, STR_ConvDec_02, s );
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
    InputQuery( STR_ConvRoman_01, STR_ConvRoman_02, s );
  if ( s = '' ) then exit;

  try
    s := uppercase( trim( s ));
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

    if ( ActiveNote.Editor.SelLength > 0 ) then
    begin
      lista.Text := ActiveNote.Editor.SelText;
      title := STR_Statistics_02;
    end
    else
    begin
      lista.Text := ActiveNote.Editor.Lines.Text;
      title := STR_Statistics_03;
    end;

    numLines := lista.count;

    numChars := 0;
    numSpaces := 0;
    numAlpChars := 0;
    numWords := 0;

    for l := 0 to lista.count-1 do
    begin
      s := lista[l];
      len := length( s );
      inc( numChars, len );
      WasAlpha := false;

      for i := 1 to len do
      begin
        ch := s[i];
        if IsCharAlphaA( ch ) then
        begin
          inc( numAlpChars );
          WasAlpha := true;
        end
        else
        begin
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

  if ( ActiveNote.Kind = ntTree ) then
  begin
    numNodes := TTreeNote( ActiveNote ).TV.Items.Count;
    s := s + #13#13 + Format( STR_Statistics_05,  [numNodes] );
  end;

  Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format(
    STR_Statistics_06, [numChars, numAlpChars, numWords] );

  if ( messagedlg( s + #13#13 + STR_Statistics_07, mtInformation, [mbOK,mbCancel], 0 ) = mrOK ) then
    Clipboard.SetTextBuf( Pchar( s ));

end; // ShowStatistics


//=================================================================
// WordWebLookup
//=================================================================
procedure WordWebLookup;
var
  WordWeb : TFreeWordWeb;
  myWord, newWord : string;
  errmsg : string;
begin
  if ( not ( assigned( ActiveNote ) and ( ActiveNote.FocusMemory = focRTF ))) then exit;

  if ShiftDown then
  begin
    myWord := '';
  end
  else
  begin
    if ( ActiveNote.Editor.SelLength > 0 ) then
      myWord := trim( ActiveNote.Editor.SelText )
    else
      myWord := ActiveNote.Editor.GetWordAtCursorNew( true );
  end;

  if ( myWord = '' ) then
  begin
    if ( not InputQuery( STR_WordWeb_01, STR_WordWeb_02, myWord )) then
      exit;
  end;

  errmsg := STR_WordWeb_03;

  WordWeb := nil;
  try
    WordWeb := TFreeWordWeb.Create( Form_Main );
  except
    On E : Exception do
    begin
      messagedlg( errmsg + E.Message, mtError, [mbOK], 0 );
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
      begin
        ActiveNote.Editor.SelText := newWord + #32;
      end;
    except
      On E : Exception do
      begin
        Form_Main.RTFMWordWeb.Enabled := false;
        Form_Main.TB_WordWeb.Enabled := false;
        messagedlg( errmsg + E.Message, mtError, [mbOK], 0 );
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
    if KeyOptions.UASEnable then
    begin
      UASPath := GetUASPath; // let UAS find itself

      if ( not fileexists( UASPath )) then
      begin
        UASPath := KeyOptions.UASPath; // maybe we already have it configured

        if ( not fileexists( UASPath )) then
        begin
          // ...we don't so ask user and check answer
          if ( InputQuery( STR_UAS_01, STR_UAS_02, UASPath ) and
               fileexists( UASPath )) then
          begin
            KeyOptions.UASPath := UASPath; // found it, so store it for later
          end
          else
          begin
            // user canceled or entered invalid path, so bail out
            messagedlg( STR_UAS_03, mtError, [mbOK], 0 );
            KeyOptions.UASEnable := false;
            exit;
          end;
        end;
      end;

      if LoadUAS( UASPath ) then
      begin
        UAS_Window_Handle := GetUASWnd;
        // check if really loaded
        KeyOptions.UASEnable := ( UAS_Window_Handle <> 0 );
      end
      else
      begin
        KeyOptions.UASEnable := false;
      end;

      if KeyOptions.UASEnable then
      begin
        // success
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_UAS_04;
      end
      else
      begin
        // something went wrong
        KeyOptions.UASEnable := false;
        if ( messagedlg( STR_UAS_05, mtWarning, [mbOK,mbCancel], 0 ) = mrOK ) then
        begin
          GoDownloadUAS;
        end;
      end;
    end
    else
    begin
      if ( UAS_Window_Handle <> 0 ) then
      begin
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
  if ( UAS_Window_Handle = 0 ) then
  begin
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_UAS_07;
    exit;
  end;

  GetCursorPos( ptCursor );
  SetForegroundWindow( UAS_Window_Handle );
  PostMessage( UAS_Window_Handle, WM_APP+$2001, ptCursor.x, ptCursor.y );

end; // ConfigureUAS



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

      try
        try
          if TurnOn then
          begin
            // turn ON clipboard capture for active note
            if aNote.ReadOnly then
            begin
              TB_ClipCap.Down := false;
              PopupMessage( STR_ClipCap_01, mtInformation, [mbOK], 0 );
              exit;
            end;

            if ( aNote.Kind = ntTree ) then
            begin
              if ( Initializing or ( not ClipOptions.TreeClipConfirm )) then
              begin
                ClipCapNode := GetCurrentNoteNode;
              end
              else
              begin
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
                  else
                  begin
                    TB_ClipCap.Down := false;
                    exit;
                  end;
                end;
              end;
            end
            else
            begin
              ClipCapNode := nil;
            end;

            if ( NoteFile.ClipCapNote <> nil ) then
            begin
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
          else
          begin
            // turn OFF clipboard capture
            ClipCapActive := false;
            ClipCapNode := nil;
            if ( NoteFile.ClipCapNote = aNote ) then
            begin
              // restore note name on the tab
              Pages.MarkedPage := nil;
              SetClipCapState( false );
            end
            else
            begin
              // showmessage( 'Error: Tried to turn off ClipCap for a non-active note.' );
            end;
            NoteFile.ClipCapNote := nil;
          end;
        except
          on e : exception do
          begin
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


//=================================================================
// PasteOnClipCap
//=================================================================
procedure PasteOnClipCap;
var
  DividerString: string;
  ClpStr : WideString;       //***1
  i : integer;
  wavfn, myNodeName : string;
  myTreeNode, myParentNode : TTreeNTNode;
  PasteOK : boolean;
  SourceURLStr : string;
  AuxStr : string;
begin
  with Form_Main do begin
        myTreeNode := nil;


        _IS_CAPTURING_CLIPBOARD := true;
        // TrayIcon.Icon := TrayIcon.Icons[1];
        LoadTrayIcon( not ClipOptions.SwitchIcon ); // flash tray icon

        NoteFile.ClipCapNote.Editor.OnChange := nil;
        NoteFile.Modified := true; // bugfix 27-10-03

        if ClipOptions.InsertSourceURL then
          SourceURLStr := GetURLFromHTMLClipboard
        else
          SourceURLStr := '';

        PasteOK := true;
        try
          StatusBar.Panels[PANEL_HINT].Text := STR_ClipCap_06;
          if ClipOptions.URLOnly then
          begin
            // [x] NOT IMPLEMENTED
          end
          else
          begin

            DividerString := ClipOptions.Divider;
            for i := 1 to length( DividerString ) do
            begin
              if ( DividerString[i] = CLIPDIVCHAR ) then
                DividerString[i] := #13;
            end;

            i := pos( CLIPDATECHAR, DividerString );
            if ( i > 0 ) then
            begin
              delete( DividerString, i, length( CLIPDATECHAR ));
              if ( length( DividerString ) > 0 ) then
                insert( FormatDateTime( KeyOptions.DateFmt, now ), DividerString, i )
              else
                DividerString := FormatDateTime( KeyOptions.DateFmt, now );
            end;

            i := pos( CLIPTIMECHAR, DividerString );
            if ( i > 0 ) then
            begin
              delete( DividerString, i, length( CLIPTIMECHAR ));
              if ( length( DividerString ) > 0 ) then
                insert( FormatDateTime( KeyOptions.TimeFmt, now ), DividerString, i )
              else
                DividerString := FormatDateTime( KeyOptions.TimeFmt, now );
            end;

            if ( NoteFile.ClipCapNote.Kind = ntTree ) then
            begin
              // ClipCapNode := nil;
              if ClipOptions.PasteAsNewNode then
              begin
              if ( ClipCapNode <> nil ) then
                myParentNode := TTreeNote( NoteFile.ClipCapNote ).TV.Items.FindNode( [ffData], '', ClipCapNode )
              else
                myParentNode := nil;

              case ClipOptions.ClipNodeNaming of
                clnDefault : myNodeName := '';
                clnClipboard : myNodeName := FirstLineFromClipboard( TREENODE_NAME_LENGTH );
                clnDateTime : myNodeName := FormatDateTime( ShortDateFormat + #32 + ShortTimeFormat, now );
              end;

              if assigned( myParentNode ) then
                myTreeNode := TreeNoteNewNode( TTreeNote( NoteFile.ClipCapNote ), tnAddChild, myParentNode, myNodeName, true )
              else
                myTreeNode := TreeNoteNewNode( TTreeNote( NoteFile.ClipCapNote ), tnAddLast, nil, myNodeName, true );
              end
              else
              begin
                myTreeNode := TTreeNote( NoteFile.ClipCapNote ).TV.Selected;
                if ( not assigned( myTreeNode )) then
                begin
                  myTreeNode := TreeNoteNewNode( TTreeNote( NoteFile.ClipCapNote ), tnAddLast, nil, myNodeName, true );
                end;
              end;
              if ( not assigned( myTreeNode )) then
              begin
                PasteOK := false;
                PopupMessage( STR_ClipCap_07, mtError, [mbOK], 0 );
                exit;
              end;
            end;

            with NoteFile.ClipCapNote do
            begin
              // do not add leading blank lines if pasting in a new tree node
              if (( Kind <> ntRTF ) and ClipOptions.PasteAsNewNode ) then
                DividerString := trimleft( DividerString );

                Editor.SelText := DividerString;
              Editor.SelStart :=  Editor.SelStart + Editor.SelLength;
            end;

            if ( SourceURLStr <> '' ) then
            begin
              AuxStr := STR_ClipCap_08 + SourceURLStr + ']' + #13;
              with NoteFile.ClipCapNote.Editor do
              begin
                SelText := AuxStr;
                SelStart := SelStart + SelLength;
              end;
            end;

            if ClipOptions.PasteAsText then
            begin
              ClpStr := ClipboardAsWString;
              if (( ClipOptions.MaxSize > 0 ) and ( length( ClpStr ) > ClipOptions.MaxSize )) then
                delete( ClpStr, succ( ClipOptions.MaxSize ), length( ClpStr ));
              with NoteFile.ClipCapNote.Editor do
              begin
                SelTextW := ClpStr;
                SelStart := SelStart + SelLength;
              end;
            end
            else
              NoteFile.ClipCapNote.Editor.PasteFromClipboard;

          end;

        finally
          NoteFile.ClipCapNote.Editor.OnChange := RxRTFChange;
          if PasteOK then
          begin

            NoteFile.Modified := true;
            UpdateNoteFileState( [fscModified] );

            if assigned ( myTreeNode ) then
            begin
              TNoteNode( myTreeNode.Data ).RTFModified := true;
            end;

            StatusBar.Panels[PANEL_HINT].Text := STR_ClipCap_09;
            wavfn := extractfilepath( application.exename ) + 'clip.wav';
            if ( ClipOptions.PlaySound and fileexists( wavfn )) then
            begin
              sndplaysound( PChar( wavfn ), SND_FILENAME or SND_ASYNC or SND_NOWAIT );
            end;
            sleep( ClipOptions.SleepTime * 100 ); // in tenths of a second; default: 5 = half a second
            LoadTrayIcon( ClipOptions.SwitchIcon ); // unflash tray icon
          end;
          _IS_CAPTURING_CLIPBOARD := false;
        end;
  end;

end; // PasteOnClipCap


//=================================================================
// PasteAsWebClip
//=================================================================
procedure PasteAsWebClip;
var
  oldClipCapNote : TTabNote;
  oldClipCapNode : TNoteNode;
  oldDividerString : string;
  oldURLOnly, oldAsText, oldTreeClipConfirm, oldInsertSourceURL : boolean;
  oldMaxSize, oldSleepTime : integer;
begin
  if ( _IS_CAPTURING_CLIPBOARD or _IS_CHAINING_CLIPBOARD ) then exit;
  if ( not Form_Main.HaveNotes( true, true )) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;


  oldDividerString := ClipOptions.Divider;
  oldInsertSourceURL := ClipOptions.InsertSourceURL;
  oldMaxSize := ClipOptions.MaxSize;
  oldSleepTime := ClipOptions.SleepTime;
  oldTreeClipConfirm := ClipOptions.TreeClipConfirm;
  oldAsText := ClipOptions.PasteAsText;
  oldURLOnly := ClipOptions.URLOnly;

  oldClipCapNote := NoteFile.ClipCapNote;
  oldClipCapNode := ClipCapNode;

  try
    ClipOptions.MaxSize := 0;
    ClipOptions.SleepTime := 0;
    ClipOptions.TreeClipConfirm := false;
    ClipOptions.InsertSourceURL := true;
    ClipOptions.URLOnly := false;
    ClipOptions.Divider := ClipOptions.WCDivider;
    ClipOptions.PasteAsText := ClipOptions.WCPasteAsText;

    NoteFile.ClipCapNote := ActiveNote;
    ClipCapNode := nil;

    PasteOnClipCap; // reuse this routine... ugliness, but that's all we can do now

  finally
    ClipOptions.Divider := oldDividerString;
    ClipOptions.InsertSourceURL := oldInsertSourceURL;
    ClipOptions.MaxSize := oldMaxSize;
    ClipOptions.SleepTime := oldSleepTime;
    ClipOptions.TreeClipConfirm := oldTreeClipConfirm;
    ClipOptions.PasteAsText := oldAsText;
    ClipOptions.URLOnly := oldURLOnly;

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
    if AsNewNote then
    begin
      NewNote( true, true, ntRTF );
      CanPaste := ( OldCNT < NoteFile.Notes.Count );
    end
    else
    begin
      if ( assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree )) then
      begin
        case ClipOptions.ClipNodeNaming of
          clnDefault : myNodeName := '';
          clnClipboard : myNodeName := FirstLineFromClipboard( TREENODE_NAME_LENGTH );
          clnDateTime : myNodeName := FormatDateTime( ShortDateFormat + #32 + ShortTimeFormat, now );
        end;

        myTreeNode := TreeNoteNewNode( nil, tnAddAfter, GetCurrentTreeNode, myNodeName, false );
        CanPaste := assigned( myTreeNode );
      end;
    end;
  except
    exit;
  end;

  if CanPaste then
  begin
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

  if Form_Main.PrintDlg.Execute then
  begin
    Form_Main.RichPrinter.Title := RemoveAccelChar( ActiveNote.Name );

    PrintRe := TRichEdit.Create( nil );
    MS := TMemoryStream.Create;

    try

      screen.Cursor := crHourGlass;

      with PrintRe do
      begin
        parent := Form_Main;
        visible := false;
        WordWrap := false;
      end;

      if (( ActiveNote.Kind = ntRTF ) or ( not PrintAllNodes )) then
      begin

        if KeyOptions.SafePrint then
        begin
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
        begin
          Form_Main.RichPrinter.PrintRichEdit( TCustomRichEdit( ActiveNote.Editor ), 1 );
        end;
      end
      else
      begin
        tNote := TTreeNote( ActiveNote );
        myTreeNode := tNote.TV.Items.GetFirstNode;
        if myTreeNode.Hidden then myTreeNode := myTreeNode.GetNextNotHidden;   // [dpv]
        while assigned( myTreeNode ) do
        begin
          myNoteNode := TNoteNode( myTreeNode.Data );
          if assigned( myNoteNode ) then
          begin
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
      if AJBSpell.CheckClipboardSpell then
      begin
        if ( messagedlg( STR_Print_02, mtConfirmation, [mbOK, mbCancel], 0 ) = mrOK ) then
        begin
          ActiveNote.Editor.PasteFromClipboard;
        end;
      end;
    except
      on E : Exception do
      begin
        messagedlg( E.Message, mtError, [mbOK], 0 );
      end;
    end;
  finally
    AJBSpell.Free;
  end;
end; // RunSpellcheckerForNote


function GetEditorZoom : integer;
var
  W, L : integer;
begin
  result := 100;
  if ( _LoadedRichEditVersion < 3 ) then exit; // cannot zoom
  if ( not assigned( ActiveNote )) then exit;
  SendMessage( ActiveNote.Editor.Handle, EM_GETZOOM, integer(@w), integer(@l) );
  if ( w = 0 ) then w := 1;
  if ( l = 0 ) then l := 1;
  result := makepercentage( w, l );
end; // GetEditorZoom

procedure SetEditorZoom( ZoomValue : integer; ZoomString : string );
var
  CurrentZoom : integer;
  NewZoom : integer;
  p : integer;
begin
  if ( _LoadedRichEditVersion < 3 ) then exit; // cannot zoom
  if ( not assigned( ActiveNote )) then exit;

  CurrentZoom := GetEditorZoom;
  NewZoom := 100; // initialize

  // if integer argument is greater than zero, use the integer as zoom value.
  // if integer is 0, reset zoom to 100%.
  // if integer is less than 0, derive zoom value from the string argument.
  // if string argument is an empty string, reset zoom to 100% (this allows
  // user to delete the text in combobox and press Enter to reset zoom)

  try

    if ( ZoomValue > 0 ) then
      NewZoom := ZoomValue
    else
    if ( ZoomValue = 0 ) then
      NewZoom := 100
    else
    begin
      ZoomString := trim( ZoomString );
      if ( ZoomString = '' ) then
      begin
        NewZoom := 100; // reset is empty string passed
      end
      else
      begin
        p := pos( '%', ZoomString );
        if ( p > 0 ) then
          delete( ZoomString, p, 1 );
        try
          NewZoom := strtoint( ZoomString );
        except
          on E : Exception do
          begin
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
    SendMessage( ActiveNote.Editor.Handle, EM_SETZOOM, NewZoom, 100 );

  finally
    _LastZoomValue := GetEditorZoom;
    Form_Main.Combo_Zoom.Text := Format(
      '%d%%',
      [_LastZoomValue] );
  end;

end; // ZoomEditor

procedure ShowTipOfTheDay;
var
  TipDlg : TGFTipDlg;
  wasiconic : boolean;
begin
  if ( not fileexists( TIP_FN )) then
  begin
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
    with TipDlg do
    begin
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


end.
