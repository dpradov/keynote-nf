unit knt.ui.editor;

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
   Winapi.Messages,
   Winapi.ShellAPI,
   Winapi.RichEdit,
   System.Classes,
   System.SysUtils,
   System.StrUtils,
   System.AnsiStrings,
   System.IniFiles,
   Vcl.Clipbrd,
   Vcl.Graphics,
   Vcl.FileCtrl,
   Vcl.Controls,
   Vcl.ComCtrls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.ExtDlgs,
   Vcl.ExtCtrls,
   Vcl.Menus,
   RxRichEd,
   kn_Info,
   kn_Const
;

type
  THiddenMarks = (hmOnlyBookmarks, hmOnlyImages, hmAll);


type
  TKntRichEdit= class;
  TChangedSelectionEvent = procedure(Sender: TKntRichEdit; ConsiderAllOnPlainText: boolean = false) of object;


//======================================

  TAuxRichEdit = class(TRxRichEdit )
  private
   function ProtectChange(const Message: TMessage; StartPos, EndPos: Integer): Boolean; override;

  public
    constructor Create( AOwner : TWinControl );

    procedure PrepareEditorforPlainText (Chrome: TChrome);
    procedure LoadFromEditor(Editor: TKntRichEdit; FromSelection: Boolean= True);

    procedure RemoveKNTHiddenCharacters (selection: boolean= true);
    function GetRichEditorWithNoKNTHiddenCharacters (HiddenMarksToRemove: THiddenMarks; const selection: boolean= true): TRxRichEdit;
  end;


//======================================

  TKntRichEdit = class( TRxRichEdit )
  private class var
    DraggingImageID: integer;
    DraggingImage_PosImage: integer;
    DraggingImage_PosFirstHiddenChar: integer;
    FPopupMenuBAK: TPopupMenu;

  private
    fFileObj:   TObject;
    fFolderObj: TObject;
    fNNodeObj:  TObject;
    fNEntryObj: TObject;

    FAutoIndent : boolean;
    FUseTabChar : boolean;
    FTabSize : byte;
    FRecreateWndProtect : boolean;

   	// Fields initialized in Folder.ConfigureEditor. Also in CreateScratchEditor()
    FPlainText: boolean;
    FSupportsRegisteredImages: boolean;
    FSupportsImages: boolean;
    FChrome: TChrome;

    FZoomGoal: integer;
    FZoomCurrent: integer;

    LastSS, LastPy, LastPx, LastX : integer;

    FIgnoreSelectionChange: boolean;
    FChangedSelection: TChangedSelectionEvent;

    procedure CMDialogKey( var Message: TCMDialogKey ); message CM_DIALOGKEY;

  protected
    procedure CMRecreateWnd(var Message: TMessage); message CM_RECREATEWND;

    procedure DoEnter; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUP(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure SelectionChange; override;

    procedure RxRTFProtectChangeEx(Sender: TObject; const Message: TMessage; StartPos, EndPos: Integer; var AllowChange: Boolean);
    procedure RxRTFURLClick(Sender: TObject; const URLText: string; Button: TMouseButton);
    procedure RxRTFStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure RxRTFEndDrag(Sender, Target: TObject; X, Y: Integer);

    procedure CheckWordCountWaiting;
    procedure CleanWordCount;

  public
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;

    property FileObj: TObject read fFileObj;
    property FolderObj: TObject read fFolderObj;
    property NNodeObj: TObject read fNNodeObj;
    property NEntryObj: TObject read fNEntryObj;

    property PlainText: boolean read FPlainText write FPlainText;
    property SupportsRegisteredImages: boolean read FSupportsRegisteredImages write FSupportsRegisteredImages;
    property SupportsImages: boolean read FSupportsImages write FSupportsImages;
    property Chrome: TChrome read FChrome write FChrome;
    property ZoomGoal: integer read FZoomGoal;
    property ZoomCurrent: integer read FZoomCurrent;

    procedure SetVinculatedObjs(FileObj, FolderObj, NNodeObj, NEntryObj: TObject);
    function ContainsRegisteredImages: boolean;

    property AutoIndent : boolean read FAutoIndent write FAutoIndent;
    property UseTabChar : boolean read FUseTabChar write FUseTabChar;
    property TabSize : byte read FTabSize write FTabSize;
    property RecreateWndProtect : boolean read FRecreateWndProtect write FRecreateWndProtect;

    property OnChangedSelection: TChangedSelectionEvent read FChangedSelection write FChangedSelection;
    procedure ChangedSelection; virtual;
    procedure Change; override;

    function FontInfoString : string;
    function ParaInfoString : string;
    procedure SaveParagraphAttributes(var Paragraph: TParaFormat2);
    procedure ApplyParagraphAttributes(var Paragraph: TParaFormat2; const Reduced: Boolean = False);
    procedure SaveTextAttributes(var Format: TCharFormat2);
    procedure ApplyTextAttributes(var Format: TCharFormat2);
    function ParagraphsSelected: Boolean;
    procedure GetIndentInformation(var Indent: integer; var NextIndent: integer;
                                   var LineStr: string; var posBegin : integer;
                                   paragraphMode: boolean= false);

    procedure GetLinkAtCursor(var URL: string; var TextURL: string;
                              var LeftE: integer; var RightE: integer; SelectURL: boolean= true);
    function GetWordAtCursor(const LeaveSelected : boolean;
                             const IgnoreActualSelection: boolean = False;
                             const DiscardKNTHiddenCharacters: boolean= True;
                             const SpacesAsWordDelim: boolean= False) : string;

    procedure UpdateWordCount;
    procedure CheckWordCount (cleanWC: boolean= false);
    procedure UpdateCursorPos;
    procedure GoToLine(const s : string);
    procedure Navigate(NavDirection: TNavDirection);
    function GetStatistics (var numChars, numAlpChars, numWords: integer): string;

    function MakeKNTHiddenMarksVisible: boolean;
    function ChangeKNTHiddenMarksVisibility(Hide: boolean; Selection: boolean = true): boolean;
    function GetRichEditorWithNoKNTHiddenCharacters (HiddenMarksToRemove: THiddenMarks; const selection: boolean= true): TRxRichEdit;
    procedure RemoveKNTHiddenCharacters (selection: boolean= true);
    function KeepOnlyLeadingKNTHiddenCharacters(const txt: string): string;
    procedure HideKNTHiddenMarks(Selection: boolean = true);
    procedure CheckToSelectLeftImageHiddenMark (var SelStartOrig: integer; var SelLengthOrig: integer; offset: integer= 0); overload;
    procedure CheckToSelectLeftImageHiddenMark (offset: integer= 0); overload;
    procedure CheckToSelectRightImageHiddenMark;
    procedure CheckToMoveLefOftHiddenMark;
    procedure CheckToSelectImageHiddenMarkOnDelete;
    function CheckToIdentifyImageID (var posFirstHiddenChar: integer): integer;
    procedure ReconsiderImageDimensionGoals(Selection: boolean; ImagesMode: TImagesMode);
    procedure ReconsiderImages(Selection: boolean; ImagesMode: TImagesMode; ReconsiderDimensionsGoal: boolean); overload;
    procedure ReconsiderImages(Selection: boolean; ImagesMode: TImagesMode); overload;

    function GetZoom: integer;
    procedure SetZoom(ZoomValue : integer; ZoomString : string; Increment: integer= 0 );
    procedure RestoreZoomGoal;
    procedure RestoreZoomCurrent;
    function GetAndRememberCurrentZoom: integer;
    procedure SetMargins;

    procedure TryPasteRTF(HTMLText: AnsiString=''; FolderName: String= '');
    procedure PastePlain (StrClp: string = ''; HTMLClip: AnsiString= ''; ForcePlainText: boolean = false; MaxSize: integer = 0 );
    procedure PasteBestAvailableFormat (const FolderName: string;
                                        TryOfferRTF: boolean= True; CorrectHTMLtoRTF: boolean = False;
                                        PrioritizeImage: boolean = False);
    procedure TryToDetectAndSolveHTMLtoRTFbadConv (posI: integer);


    function CheckReadOnly: boolean;
    procedure ArabicToRoman;
    procedure RomanToArabic;

    procedure MatchBracket;
    procedure TrimBlanks( const TrimWhat : integer );
    procedure CompressWhiteSpace;

    procedure EvaluateExpression;

    procedure InsertPictureOrObject (const AsPicture : boolean);

    procedure InsertSpecialCharacter;
    procedure InsertChar (const ch: Char; const Count: integer; const FontName: string; const FontCharset: TFontCharset; Unicode: boolean= true);
    class procedure CharInsertProc( const ch : Char; const Count : integer; const FontName : string; const FontCharset : TFontCharset; Unicode: boolean= true);
    class procedure Form_CharsClosed( sender : TObject );

    procedure ExpandTermProc;

    procedure RunSpellchecker;
    procedure WordWebLookup;


  end; // TKntRichEdit


//======================================

  function CreateAuxRichEdit: TAuxRichEdit;

  function RemoveKNTHiddenCharactersInText (const s: string; checkIfNeeded: boolean= true): string;
  function RemoveKNTHiddenCharactersInRTF  (const s: AnsiString; HiddenMarks: THiddenMarks): AnsiString;
  function GetHumanizedKNTHiddenCharacters (const s: string): string;

  procedure AddGlossaryTerm;
  procedure EditGlossaryTerms;



var
  _LoadedRichEditVersion : Single;


implementation
uses
   AJBSpeller,
   FreeWordWeb,
   Parser,
   gf_misc,
   gf_strings,
   gf_miscvcl,
   kn_global,
   kn_LinksMng,
   kn_Macro,
   kn_EditorUtils,
   kn_RTFUtils,
   kn_ClipUtils,
   kn_ImagesMng,
   kn_ImagesUtils,
   kn_MacroMng,
   kn_NoteFileMng,
   kn_Cmd,
   kn_StyleObj,
   kn_CharsNew,
   kn_Glossary,
   kn_ExpTermDef,
   knt.App,
   knt.RS
  ;



const
  RTFCONVERSON_MAX_TEXTSIZE_TO_CHECK: integer= 15;
  RTFCONVERSON_MAX_DIF_TO_IGNORE: integer= 5;

  procedure RemoveKNTHiddenCharactersFromEditor (Editor: TRxRichEdit; selection: boolean= true); forward;
  function GetRichEditorWithNoKNTHiddenCharacters (Editor: TRxRichEdit; HiddenMarksToRemove: THiddenMarks; const selection: boolean= true): TRxRichEdit; forward;



function CreateAuxRichEdit: TAuxRichEdit;
begin
   Result:= TAuxRichEdit.Create(Form_Main);
end;


//=========================================================================================
//        TAuxRichEdit
//=========================================================================================

constructor TAuxRichEdit.Create( AOwner : TWinControl );
begin
  inherited Create( AOwner );

  // *1   Tt's necessary to be able to use RTFAux.Perform(EM_LINEINDEX, L, 0)   (See for example: kn_Main.RxRTF_KeyPress)
  //      Otherwise, the character position returned by that method will not match the line break, but rather a probably default line width (22).
  // *2   This sentence must be executed before assigning Parent. If not, then can cause kn_KntFolder:TKntRichEdit.CMRecreateWnd to be called
  // *3   After migrating to Delphi 11, with the use of unRxLib (instead of RX Library), it is necessary to define StreamFormat as sfRichText in this RTFAux control
  //      Without it, the indentation of multiple lines done in kn_Main.RxRTF_KeyPress, would not work (would show RTF contet as text)

   Visible:= False;
   WordWrap:= false;            // *1
   Parent:= AOwner;

   Clear;
   StreamMode := [];
   StreamFormat := sfRichText;  // *3
   //WordWrap:= false;          // Commented: *2
end;


procedure TAuxRichEdit.PrepareEditorforPlainText (Chrome: TChrome);
begin
    with DefAttributes do begin
      Charset := Chrome.Font.Charset;
      Name := Chrome.Font.Name;
      Size := Chrome.Font.Size;
      Style := Chrome.Font.Style;
      Color := Chrome.Font.Color;
      Language := Chrome.Language;
    end;
end;


procedure TAuxRichEdit.LoadFromEditor(Editor: TKntRichEdit; FromSelection: Boolean= True);
var
  Stream: TStream;
  Str: String;

begin
   if not assigned(Editor) then exit;

   if Editor.FPlainText  then begin
      Self.StreamFormat := sfPlainText;
      Self.PrepareEditorforPlainText(Editor.Chrome);
      if FromSelection then
         Str:= Editor.SelText
      else
         Str:= Editor.Text
   end
   else
      if FromSelection then
         Str:= Editor.RtfSelText
      else
         Str:= Editor.RtfText;

   Stream:= TStringStream.Create(Str);
   try
      Self.Lines.LoadFromStream(Stream);
   finally
      Stream.Free;
   end;

end;

function TAuxRichEdit.ProtectChange(const Message: TMessage; StartPos, EndPos: Integer): boolean;
begin
  Result := True;
end;

function TAuxRichEdit.GetRichEditorWithNoKNTHiddenCharacters (HiddenMarksToRemove: THiddenMarks; const selection: boolean= true): TRxRichEdit;
begin
   Result:= knt.ui.editor.GetRichEditorWithNoKNTHiddenCharacters(Self, HiddenMarksToRemove, selection);
end;

procedure TAuxRichEdit.RemoveKNTHiddenCharacters (selection: boolean= true);
begin
   RemoveKNTHiddenCharactersFromEditor(Self, selection);
end;



//=========================================================================================
//        TKntRichEdit
//=========================================================================================

constructor TKntRichEdit.Create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  // NOTE: Microsoft have FIXED THE BUG which required this
  // special protection method in version 4 of the richedit control
  // (2002; WinXP SP1). So the check below could read:
  // FRecreateWndProtect := ( _LoadedRichEditVersion = 2 );
  // However, just to be on the safe side since versiopn 4 of the
  // richedit control is very new, we keep the protection for all
  // richedit dll versions above 2.
  FRecreateWndProtect := ( _LoadedRichEditVersion > 2 );
  FAutoIndent := false;
  FUseTabChar := true;
  FTabSize := DEF_TAB_SIZE;

  FIgnoreSelectionChange:= false;
  DraggingImageID:= 0;

  FZoomGoal:= 100;
  FZoomCurrent:= -1;

  LastSS:= -1;
  LastPy:= -1;
  LastPx:= -1;
  LastX:= -1;

  OnProtectChangeEx:= RxRTFProtectChangeEx;
  OnURLClick:= RxRTFURLClick;

  {
   By default DragMode=dmManual, and the mechanism in TRxRichEdit is controlled through the IRichEditOleCallback interface.
   (see comment *3 in RxRichEd.pas)
   //OnDragOver := RxRTFDragOver;
   //DragMode :=  dmManual; // dmAutomatic;
  }
  OnStartDrag := RxRTFStartDrag;    // See comment *4 in RxRichEd
  OnEndDrag   := RxRTFEndDrag;        // ,,
end; // TKntRichEdit CREATE


destructor TKntRichEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TKntRichEdit.SetVinculatedObjs(FileObj, FolderObj, NNodeObj, NEntryObj: TObject);
begin
   fFileObj:= FileObj;
   fFolderObj:= FolderObj;
   fNNodeObj:= NNodeObj;
   fNEntryObj:= NEntryObj;
end;


function TKntRichEdit.ContainsRegisteredImages: boolean;
var
  ImgIDs: TImageIDs;
begin
  ImgIDs:= ImageMng.GetImagesIDInstancesFromTextPlain (TextPlain);
  Result:= (ImgIDs <> nil);
end;



{
 NOTE:
  If DragAcceptFiles is not called from the RxRichEdit control, since we are doing it in the main form of the application (TForm_Main.CreateWnd)
  we would still be able to receive the WM_DROPFILES message, through the event handler defined within TForm_Main
  ( procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES; )

  But we will also call that method for the RichEdit control to have the option, if necessary, of being able to distinguish which control is receiving
  the file. Furthermore, before we were calling DragAcceptFiles from our derived class, TKntRichEdit, but we have started to call it directly from
  RxRichEdit itself, in its CreateWnd method, to allow offering from that control, in an integrated way, a new event, OnFileDropped. That event will be
  launched from WM_DROPFILES, and in order to receive that message it must be enabled by calling DragAcceptFiles

  See more clarifications in comment *3 on RxRichEd.pas
}
{                                                                                             [dpv]
procedure TKntRichEdit.CreateWnd;
begin
  inherited;
  DragAcceptFiles( handle, true ); // [MJ]
end; // CreateWnd

procedure TKntRichEdit.DestroyWnd;
begin
  DragAcceptFiles( handle, false ); // [MJ]
  inherited;
end; // DestroyWnd
}

procedure TKntRichEdit.CMRecreateWnd(var Message: TMessage);
var
  FRTF: string;
  mySelStart, mySelLength : longint;
  PerformFix : boolean;
begin
  mySelStart := self.SelStart;
  mySelLength := self.SelLength;

  PerformFix := ( FRecreateWndProtect and ( WindowHandle <> 0 ));

  if PerformFix then begin
    FRTF:= Self.RtfText;
    Lines.Clear; // [!] if we do this...
  end;

  inherited;

  if PerformFix then begin
    Lines.BeginUpdate;
    try
      // Lines.Clear;
     {$IFDEF KNT_DEBUG}Log.Add('TKntRichEdit_CMRecreateWnd. PerformFix',  4 ); {$ENDIF}
      Self.PutRtfText(FRTF, true);
      ClearUndo; // [!] ...we must also do this, otherwise if user hits Ctrl+Z right after Ctrl+W, all text is gone withno way to bring it back!
    finally
      Lines.EndUpdate;
      FRTF := '';
    end;
  end;

  self.SelStart := mySelStart;
  self.SelLength := mySelLength;

end; // CMRecreateWnd


procedure TKntRichEdit.CMDialogKey( var Message: TCMDialogKey );
begin
  if ( Message.CharCode <> VK_TAB ) then
    inherited;
end; // CMDialogKey

(*
procedure TKntRichEdit.WMDropFiles(var Msg: TWMDropFiles);  // [MJ]
var
  CFileName : array[0..MAX_PATH] of Char;
  FileList : TStringList;
  i, count : integer;
begin
  FileList := TStringList.Create;

  try
    count := DragQueryFile( Msg.Drop, $FFFFFFFF, CFileName, MAX_PATH );

    if ( count > 0 ) then
    begin
      for i := 0 to count-1 do
      begin
        DragQueryFile( Msg.Drop, i, CFileName, MAX_PATH );
        FileList.Add( CFileName );
      end;
    end;

    if assigned( FOnFileDropped ) then OnFileDropped( self, FileList );

  finally
    FileList.Free;
    DragFinish(Msg.Drop);
  end;

end; // WMDropFiles
*)


//-------------------------------------------------------------------------------------------------------------------------------

procedure RemoveKNTHiddenCharactersFromEditor (Editor: TRxRichEdit; selection: boolean= true);
var
  s: AnsiString;
  len: integer;
begin
  with Editor do begin
     if FindText(KNT_RTF_HIDDEN_MARK_L_CHAR, 0, -1, []) >= 0 then begin
        if selection then
           s:= RtfSelText
        else
           s:= RtfText;

        len:= Length(s);
        s:= RemoveKNTHiddenCharactersInRTF(s, hmAll);
        if Length(s) <> Len then
           PutRtfText(s, true, selection, true);
     end
  end;
end;

function GetRichEditorWithNoKNTHiddenCharacters (Editor: TRxRichEdit; HiddenMarksToRemove: THiddenMarks; const selection: boolean= true): TRxRichEdit;
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
          s:= Editor.RtfSelText
       else
          s:= Editor.RtfText;

       len:= s.Length;                          // There might be a hidden markup in the editor, but maybe not in the selection
       s:= RemoveKNTHiddenCharactersInRTF(s, HiddenMarksToRemove);   // In that case this method will return the same string

       if s.Length <> Len then begin
          Result:= CreateAuxRichEdit;      // Caller should call free on this control after used
          Result.PutRtfText(s, true, true, true);
          if RichEditVersion <= 4 then
             Result.SetSelection(0, Result.TextLength, false);
       end;
    end
end;


function RemoveKNTHiddenCharactersInText(const s: string; checkIfNeeded: boolean= true): string;
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


function GetHumanizedKNTHiddenCharacters (const s: string): string;
begin
  Result:= StringReplace(s,      KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_BOOKMARK,   '[BMK:', [rfReplaceAll]);
  Result:= StringReplace(Result, KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_Bookmark09, '[TmpBMK:', [rfReplaceAll]);
  Result:= StringReplace(Result, KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_IMAGE,      '[IMG:', [rfReplaceAll]);
  Result:= StringReplace(Result, KNT_RTF_HIDDEN_MARK_R_CHAR,                             ']', [rfReplaceAll]);
end;

//----------------------

procedure TKntRichEdit.HideKNTHiddenMarks(Selection: boolean = true);
begin
   ChangeKNTHiddenMarksVisibility(True, Selection);
end;

function TKntRichEdit.MakeKNTHiddenMarksVisible: boolean;
begin
    Result:= ChangeKNTHiddenMarksVisibility(False, False);
end;

function TKntRichEdit.GetRichEditorWithNoKNTHiddenCharacters (HiddenMarksToRemove: THiddenMarks; const selection: boolean= true): TRxRichEdit;
begin
   Result:= knt.ui.editor.GetRichEditorWithNoKNTHiddenCharacters(Self, HiddenMarksToRemove, selection);
end;

procedure TKntRichEdit.RemoveKNTHiddenCharacters (selection: boolean= true);
begin
   RemoveKNTHiddenCharactersFromEditor(Self, selection);
end;


function TKntRichEdit.KeepOnlyLeadingKNTHiddenCharacters(const txt: string): string;
var
   SS, SL, pR: integer;
begin
    Result:= txt;
    if txt[1] = KNT_RTF_HIDDEN_MARK_L_CHAR then begin
       SS:= SelStart;
       SL:= SelLength;
       pR:= Pos(KNT_RTF_HIDDEN_MARK_R_CHAR, txt, 1);
       Result:= Copy(txt, pR+1);
       SelStart:=  SS + pR;
       SelLength:= SL - pR;
    end;
    Result:= RemoveKNTHiddenCharactersInText(Result);
end;


function TKntRichEdit.ChangeKNTHiddenMarksVisibility(Hide: boolean; Selection: boolean = true): boolean;
var
   p, pF: integer;
   SS, SL, Offset: integer;
   Str: String;
begin
   // {\rtf1\ansi {\v\'11B5\'12} XXX };   {\rtf1\ansi \v\'11B5\'12\v0 XXX};  {\rtf1\ansi \v\'11T999999\'12\v0 XXX};   '#$11'B5'#$12'

  Result:= false;

  Str:= TextPlain(Selection);
  p:= Pos(KNT_RTF_HIDDEN_MARK_L_CHAR, Str, 1);
  if p = 0 then Exit;

  SS:= SelStart;
  SL:= SelLength;
  if Selection then
     Offset:= SS
  else
     Offset:= 0;

  BeginUpdate;
  SuspendUndo;
  try
    repeat
       if p > 0 then begin
          pF:= Pos(KNT_RTF_HIDDEN_MARK_R_CHAR, Str, p+3);
          if (pF > 0) and (pF-p <= KNT_RTF_HIDDEN_MAX_LENGHT_CHAR) then begin
             SetSelection(p+Offset-1, pF+Offset, true);
             SelAttributes.Hidden:= Hide;
             p:= pF + 1;
             Result:= true;
          end;
          p:= Pos(KNT_RTF_HIDDEN_MARK_L_CHAR, Str, p);
       end;
    until p = 0;

  finally
     SetSelection(SS, SS+SL, true);
     ResumeUndo;
     EndUpdate;
  end;

end;   // HideKNTHiddenMarks



{
  If an image is selected (or cursor is just to the left of it and DELETE (SUPR) has been pressed)
  -> check if the hidden mark KNT_RTF_IMG_HIDDEN_MARK is present (it should) and if so expand the selection to include it.
  Also, check if just to the left is the hidden " at the end of a hyperlink. If so, select its hidden characters, which will include in the
  selection our hidden mark, if there is one.

  Result: initial SelStart (original SelStart was modified because a hidden image mark has been selected)
  If < 0 -> SelStart was not modified
  Offset: To use with Backspace, we are interested in looking one character further to the left, skipping the possible image that is deleted
}
procedure TKntRichEdit.CheckToSelectLeftImageHiddenMark (var SelStartOrig: integer; var SelLengthOrig: integer; offset: integer= 0);
var
  L, S, newS: integer;
  Ch: Char;
  UndoSelect: boolean;
  Str: String;

begin
  SelStartOrig:= -1;

  S:= SelStart;
  newS:= S-1 +offset;
  if newS < 0 then exit;

  Str:= GetTextRange(newS, S +offset);
  if Length(Str) > 0 then begin
     Ch:= Str[1];
     if (ch = KNT_RTF_HIDDEN_MARK_R_CHAR) or (ch = '"') then begin        // (ch = '"'), in case the hidden mark is next to a hyperlink
       SelLengthOrig:= SelLength;
       FIgnoreSelectionChange:= true;
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
         FIgnoreSelectionChange:= false;
       end;
     end;
  end;
end;


procedure TKntRichEdit.CheckToSelectLeftImageHiddenMark (offset: integer= 0);
var
   SelStartOrig, SelLengthOrig: integer;
begin
   CheckToSelectLeftImageHiddenMark (SelStartOrig, SelLengthOrig, offset);
end;


// It is assumed that SelLenght = 0
procedure TKntRichEdit.CheckToSelectRightImageHiddenMark;
var
  R, S, newS: integer;
  SelLengthOrig: integer;
  Ch: Char;
  UndoSelect: boolean;
  Str, TextSelected: string;

begin
  S:= SelStart;
  newS:= S+1;

  try
    Str:= GetTextRange(S, newS);
    if Length(Str) > 0 then begin
       Ch:= Str[1];
       if (ch = KNT_RTF_HIDDEN_MARK_L_CHAR) or (ch = '"') then begin         // (ch = '"'), in case the hidden mark is next to a hyperlink
         FIgnoreSelectionChange:= true;
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
           FIgnoreSelectionChange:= false;
         end;
       end;
    end;

  except   // If we are in the last position of the folder
  end;
end;

procedure TKntRichEdit.CheckToSelectImageHiddenMarkOnDelete;
begin
   // Note that it is also possible to right click on an visible image and select Delete

    if FSupportsRegisteredImages then begin
       if SelLength = 0 then
          {
            If we are to the left of a hyperlink corresponding to an image, therefore just to the left of its hidden identification characters,
            and we press DELETE (SUPR), the hyperlink would be deleted and our hidden characters would remain.
            To avoid this we can select all the hidden characters, to the right, including those of the hyperlink and ours, so that everything is deleted as a block.
            #$11'I1'#$12'HYPERLINK "img:1,32,32"N'
            Something analogous must be controlled if we are to the left of a visible image
          }
          CheckToSelectRightImageHiddenMark
       else
          CheckToSelectLeftImageHiddenMark;
    end;

end;


// This procedure asumes that SelLength = 0 (it will invoked currently from InsertContent  (kn_NoteFileMng)
procedure TKntRichEdit.CheckToMoveLefOftHiddenMark;
var
  S: integer;
begin
   S:= SelStart;
   if GetTextRange(S-1, S) = KNT_RTF_HIDDEN_MARK_R_CHAR then begin
     SelStart:= S-1;
     if SelStart <> S-1 then
     else
        SelStart:= S;              // It was not hidden. We leave it where it was
   end;

end;


//----------------------

{ Check if there is a hidden image identification mark just to our left }

function TKntRichEdit.CheckToIdentifyImageID (var posFirstHiddenChar: integer): integer;
var
  L, S: integer;
  SelLengthBak: integer;
  Str: String;

begin

  Result:= 0;              // 0 is not a valid ID

  // <L>I999999<R>

  S:= SelStart;
  if GetTextRange(S-1, S) = KNT_RTF_HIDDEN_MARK_R_CHAR then begin
    SelLengthBak:= SelLength;
    FIgnoreSelectionChange:= true;
    try
      SelStart:= S-1;
      L:= SelStart;
      if L <> S-1 then begin
         // It will have been placed to the left of the first hidden character. See coment in TForm_Main.RxRTFKeyDown  (Left cursor)
         SetSelection(L, S + 1, true);
         Str:= GetTextRange(L, S);
         if (Length(Str) > 0) and (Str[1] = KNT_RTF_HIDDEN_MARK_L_CHAR) and (Str[2] = KNT_RTF_HIDDEN_IMAGE) then
            Result:= StrToIntDef(Copy(Str, 3, (S - L)-3), 0);
      end;
      SetSelection(S, S + SelLengthBak, true);
      posFirstHiddenChar:= L;

    finally
      FIgnoreSelectionChange:= false;
    end;

  end;
end;


procedure TKntRichEdit.ReconsiderImageDimensionGoals(Selection: boolean; ImagesMode: TImagesMode);
begin
   ReconsiderImages(Selection, ImagesMode, true);
end;

procedure TKntRichEdit.ReconsiderImages(Selection: boolean; ImagesMode: TImagesMode);
begin
   ReconsiderImages(Selection, ImagesMode, false);
end;

procedure TKntRichEdit.ReconsiderImages(Selection: boolean; ImagesMode: TImagesMode; ReconsiderDimensionsGoal: boolean);
var
  strRTF: AnsiString;
  SelectAll: boolean;
  SS: integer;
begin
   if ReadOnly then exit;

   GetAndRememberCurrentZoom;
   ImageMng.DoNotRegisterNewImages:= (NNodeObj = nil);
   if ReconsiderDimensionsGoal then
      ImageMng.ReconsiderImageDimensionsGoal:= true;
   try
      if Selection then begin
         SelectAll:= false;
         CheckToSelectLeftImageHiddenMark;
         strRTF:= RtfSelText;
      end;

      if strRTF = '' then begin
         strRTF:= RtfText;
         SelectAll:= true;
      end;

      if strRTF <> '' then begin
         strRTF:= ImageMng.ProcessImagesInRTF(strRTF, Self.Name, ImagesMode, '', 0, false);
         if strRTF <> '' then begin
            SS:= SelStart;
            PutRtfText(strRTF, True, not SelectAll);
            SelStart:= SS;
         end;
      end;

   finally
      ImageMng.ReconsiderImageDimensionsGoal:= false;
      ImageMng.DoNotRegisterNewImages:= (ActiveEditor.NNodeObj = nil);
      RestoreZoomCurrent;
   end;
end;

//-----------------------------------------------------------


procedure TKntRichEdit.DoEnter;
begin
  if FUpdating > 0 then exit;

  ImageMng.DoNotRegisterNewImages:= (NNodeObj = nil);

  inherited;
end;


procedure TKntRichEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  NextIndent, Indent, posFirstChar : integer;
  LineStr : string;
  ptCursor :  TPoint;
  URLStr, TxtSel: string;
  L, R: Integer;
  Offset: integer;
  ch: char;

begin

  if ( not ( key in [16..18] )) then begin  // skip bare modifier keys
    App.Kbd.LastRTFKey.Key:= Key;
    App.Kbd.LastRTFKey.Shift := Shift;
    if IsRecordingMacro then
       AddMacroKeyPress( Key, Shift );
  end;


  if ( shift = [] ) then begin
    case Key of

      VK_LEFT: begin   // Left cursor
           { Suppose something like 'LEFT visible text'<hidden text>'RIGHT visible text' where we use the <hidden text> as metadata linked to the
             visible text on its right, next to the one we want it to remain. That hidden text can be a marker that we will use as a target reference
             of a hyperlink.

             It is not possible to put the cursor inside hidden text, and if we advance from 'LEFT' by pressing the right cursor, we will be able to
             position ourselves just before of <hidden text> or past the 'T' of visible text. Therefore, if after placing the cursor just after LEFT
             we write something (eg **"), those characters will be written to the left of the hidden text. Ok

             But if from 'RIGHT visible text' we move to the left with the cursor, we can place it just to the right of <hidden text>, immediately
             before the first visible character from the right. That would be a problem if we don't control it, because if we write something
             (eg ** or line breaks) we would be separating the hidden text from the visible text with which it should be linked.

             Note: In MS Word this would not happen, because moving to the left would also skip non-visible characters

             For this reason, we are interested in detecting if, in the case of completing the displacement to the left, we would place ourselves
             immediately leaving to our left the hidden character. Since we are going to use a specific character (completely unusual) to delimit
             these hidden texts, we will check if that is the one that would be to our left. And for more security, we will verify if it is hidden,
             which we will verify in a simple way based on another behavior of the RichEdit control: since it does not allow placing the cursor
             inside hidden texts, if we change the position of it (with SelStart) to the left of the character we are checking, the final position
             of the cursor will not be the one we are setting if it was hidden, but it will correspond to the first position immediately before the hidden text.

             This has the advantage that although our hidden text can be of variable length, we can easily place the cursor at the beginning of the
             same.
             If instead of checking if SelStart has been respected we did SelLength:= 1; and then look at SelAttributes.Hidden; we would see that it
             is True because we would be selecting hidden text.

             Comparison with '"', to detect the possible end of the hidden text of a hyperlink. If we have added a hidden mark next to a link,
             we are also interested in ensuring that no text or other items are interspersed between the two
           }

          L:= SelStart;
          if SelLength = 0 then
             Offset:= 2
          else
             Offset:= 1;

          if L >= Offset then begin
             TxtSel:= GetTextRange(L-Offset, L-Offset + 1);
             if Length(TxtSel) > 0 then begin
                ch:= TxtSel[1];
                if (ch = KNT_RTF_HIDDEN_MARK_R_CHAR) or (ch = '"') then begin
                   FIgnoreSelectionChange:= true;
                   try
                     SelStart:= L-Offset;
                     if SelStart <> L-Offset then
                        key:= 0                    // It will have been placed to the left of the first hidden character
                     else
                        SelStart:= L;              // It was not hidden. We leave it where it was and allow the keyboard event to be handled as normal.
                   finally
                      FIgnoreSelectionChange:= false;
                      UpdateCursorPos;
                   end;
                end;
             end;
          end;
      end;


      VK_INSERT : begin
        if EditorOptions.DisableINSKey then begin
          key := 0;
          App.ShowInfoInStatusBar(sEdt04);
        end
        else
          PerformCmdEx( ecInsOvrToggle );
      end;

      VK_HOME: begin
          GetIndentInformation(Indent, NextIndent, LineStr, posFirstChar);
          if ((SelStart > (posFirstChar + Indent)) or
              ((Indent>0) and (SelStart = posFirstChar)) ) then begin
             SelStart:= posFirstChar + Indent;
             key:= 0;
             // Apparently it wouldn't matter if we entered if (Indent=0), but when
             // wrapping is enabled and we are at the final of one line SelStart can return the
             // same value as if we were at the beginning of the next one. In that case, the cursor
             // would move incorrectly to the first character of next line.
          end;
      end;

      VK_RETURN :
           if EditorOptions.AutoIndent and (Paragraph.TableStyle = tsNone) then begin
              if CheckReadOnly then exit;

              GetIndentInformation(Indent, NextIndent, LineStr, posFirstChar, true);
              if (Indent = length(LineStr)) and (Paragraph.Numbering <> nsNone) and (Paragraph.NumberingStyle <> nsNoNumber) then
                 Paragraph.Numbering :=  nsNone
              else if (length(LineStr)= 0) and (Paragraph.NumberingStyle = nsNoNumber) then begin
                  key := 0;
                  SelText := #13#10;
                  SelStart := ( SelStart+SelLength ) -1;
                  Paragraph.NumberingStyle := nsNoNumber;
              end
              else
                if NextIndent > 0 then begin
                  // insert a linebreak followed by the substring of blanks and tabs
                  // If we are inside a hyperlink, do nothing here. It's risky and unuseful
                  GetLinkAtCursor(URLStr, TxtSel, L, R, false);
                  if URLStr = '' then begin
                     key := 0;
                     TxtSel:= #13 + Copy(LineStr, 1, NextIndent);
                     SelText := TxtSel;
                     SelStart := SelStart + Length(TxtSel);
                  end;
                end;

           end;

       VK_DELETE:
          CheckToSelectImageHiddenMarkOnDelete;

       VK_BACK:
          if FSupportsRegisteredImages then begin
             if SelLength = 0 then
                Offset:= -1
             else
                Offset:= 0;
             CheckToSelectLeftImageHiddenMark(Offset);
          end;
    end;

  end  // if ( shift = [] )
  else
  if ( shift = [ssShift] ) then begin
    case key of
      VK_F10 : begin
        key := 0;
        GetCursorPos( ptCursor );
        if assigned(PopupMenu) then
           PopupMenu.Popup(ptCursor.X, ptCursor.Y);
      end;
    end;
  end;

  inherited;
end; // KeyDown


procedure TKntRichEdit.KeyPress(var Key: Char);
var
  Sel: TCharRange;
  SelectionLength: integer;
  posBegin, posEnd: integer;
  L, toLine: integer;
  MoveSelectedLines, ShiftPressed: boolean;
  RTFAux : TAuxRichEdit;
  Str: string;
  SpacesAsTab: string;

  procedure SendSpaces(n: integer);
  var
    i: integer;
  begin
     for i := 1 to n do
         Perform(WM_CHAR, 32, 0);
  end;

  procedure RemoveTABorSpaces(Editor: TRxRichEdit; Line: integer; NSpaces: integer);
  var
    i, num: integer;
    Str: string;
  begin
     Str:= Editor.Lines[Line];
     i:= 1;
     num:= 0;
     while (i <= Length(Str)) and (num < NSpaces) do begin
        if (Str[i] = ' ') or ((Str[i] = #9) and (num = 0)) then
           Inc(num);
        if (Str[i] <> ' ') then
           break;
        Inc(i);
     end;
     if num > 0  then begin
        Editor.SelLength:= num;
        Editor.SelText:= '';
     end;
  end;


begin
  if ( App.Kbd.RxRTFKeyProcessed or ((Key = #9) and (GetKeyState(VK_CONTROL) < 0)) ) then begin
    Key := #0;
    App.Kbd.RxRTFKeyProcessed := false;
    exit;
  end;

  if CheckReadOnly then begin
     if (key = #9) and (GetKeyState( VK_SHIFT ) < 0 ) then begin
        App.ShowInfoInStatusBar('');
        inherited;
     end;
     exit;
  end;

  posBegin:= 0;     // To avoid compiler warning on SelStart:= posBegin (but really no necessary)

  case key of

    #9 :    begin
               ShiftPressed:= (GetKeyState( VK_SHIFT ) < 0 );
               Sel:= GetSelection;
               MoveSelectedLines:= false;
               if not (Sel.cpMin= Sel.cpMax) then begin   // There is text selected
                  if FindText(#13, Sel.cpMin, Sel.cpMax-Sel.cpMin, []) > 0 then begin
                     MoveSelectedLines:= true;
                     posBegin := FindText(#13, Sel.cpMin, -1, [stBackward]) + 1;
                     posEnd :=   FindText(#13, Sel.cpMax-1, -1, []);
                     if posEnd < 0 then
                        posEnd:= TextLength;
                     SetSelection(posBegin, posEnd+1, true);
                  end;
               end;

               if not MoveSelectedLines then begin
                  if not ShiftPressed and not UseTabChar then begin
                      Key := #0;
                      SendSpaces(TabSize);
                  end;
               end
               else begin
                   SpacesAsTab:= StringOfChar (' ', TabSize);

                   RTFAux:= CreateAuxRichEdit;
                   RTFAux.LoadFromEditor (Self);

                   toLine:=   RTFAux.Lines.Count-1;
                   BeginUpdate;
                   try
                      Key := #0;
                      if not ShiftPressed then begin
                         for L := 0 to toLine do begin
                            RTFAux.SelStart:= RTFAux.Perform(EM_LINEINDEX, L, 0);
                            if UseTabChar then
                               RTFAux.Perform(WM_CHAR, 9, 0)
                            else
                               RTFAux.SelText := SpacesAsTab;
                         end;
                      end
                      else
                         for L := 0 to toLine do begin
                            RTFAux.SelStart:= RTFAux.Perform(EM_LINEINDEX, L, 0);
                            RemoveTABorSpaces(RTFAux, L, TabSize);
                         end;

                      SelectionLength:= RTFAux.TextLength;
                      if PlainText  then
                         Str:= RTFAux.Text + #13#10
                      else
                         Str:= RTFAux.RtfText;

                      PutRtfText(Str, True);

                      SelStart:= posBegin;                           // MoveSelectedLines=True => posBegin is initialized
                      SelLength:= SelectionLength + 1;
                   finally
                      RTFAux.Free;
                      EndUpdate;
                   end;
               end;

            end;

       else begin
          if FSupportsRegisteredImages then begin
             if SelLength > 0 then
                CheckToSelectLeftImageHiddenMark;
          end;
       end;


  end;

  if EditorOptions.WordCountTrack then begin
     if key in WordDelimiters then
        CheckWordCount
     else
        CheckWordCountWaiting;

  end;

  inherited;
end;  // KeyPress


procedure TKntRichEdit.RxRTFProtectChangeEx(Sender: TObject; const Message: TMessage; StartPos, EndPos: Integer; var AllowChange: Boolean);
begin
  AllowChange := EditorOptions.EditProtected;
end; // RxRTF_ProtectChangeEx



procedure TKntRichEdit.SelectionChange;
var
  FontStyles: TFontStyles;
  SubscriptStyle: TSubscriptStyle;
  Numbering: TRxNumbering;
  SelAttrib: TRxTextAttributes;
begin
  if FUpdating > 0 then exit;
  if FIgnoreSelectionChange then exit;

  App.Kbd.RTFUpdating := true;
  try
    { Ensure that the hidden mark accompanies its image in the event that it moves by dragging
      *1:
       Finally I apply ClearUndo because SupendUndo and ResumeUndo do not work as expected here:
       if you press Undo then the image does not return to its initial position, and the added
       hidden characters become visible.
       Could have not used SupendUndo and ResumeUndo, but in that case we risk pressing Undo an
       insufficient number of times, and the operation being left halfway, with the image moved
       but without the hidden label correctly added next to it
     }
     if (DraggingImageID > 0) then begin
       var DraggingImage_NewPosImage: integer;
       var RTFHiddenMark: AnsiString;
       var pI, pF, Len: integer;

       RTFHiddenMark:= Format(KNT_RTF_IMG_HIDDEN_MARK_CHAR, [DraggingImageID]);
       DraggingImageID:= 0;

       FIgnoreSelectionChange:= true;
       BeginUpdate;
       SuspendUndo;
       try
          DraggingImage_NewPosImage:= SelStart;
          SelLength:= 0;
          SelText:= RTFHiddenMark;
          SelAttributes.Hidden := True;

          pI:= DraggingImage_PosFirstHiddenChar;
          pF:= DraggingImage_PosImage;
          if DraggingImage_NewPosImage < DraggingImage_PosImage then begin
             Len:= Length(RTFHiddenMark)+1;
             inc(pI, Len);
             inc(pF, Len);
             inc(DraggingImage_NewPosImage, Len);
          end;
          SetSelection(pI, pF, true);
          SelText:= '';
          SelStart:= DraggingImage_NewPosImage;

       finally
         ResumeUndo;
         ClearUndo;                                         // *1
         EndUpdate;
         FIgnoreSelectionChange:= false;
       end;
    end;
    DraggingImageID:= 0;

    Self.ChangedSelection;           // -> Update toolbar...

  finally
    App.Kbd.RTFUpdating := false;
  end;

  if KeyOptions.ImgHotTrackViewer then begin
     if ImageMng.ImgViewerIsOpen then begin
        var ImgID, p: integer;
        ImgID:= 0;

        if (SelLength = 1) then
           ImgID:= CheckToIdentifyImageID(p)

        else begin
            if App.ShowingImageOnTrack  then
               App.ShowingImageOnTrack:= false
            else
              if SelAttributes.Link and (SelLength = 0) then begin
                 var URLText, TxtSel: string;
                 var chrg: TCharRange;
                 var L, R: integer;
                 GetLinkAtCursor(URLText, TxtSel, L, R, false);
                 ImgID:= GetImageIDinPlaintext(URLText);
              end;
        end;

        if (ImgID > 0) and (ImageMng.ImgIDinViewer <> ImgID) then begin
           ImageMng.OpenImageViewer(ImgID, false, false);
           App.ShowingImageOnTrack:= true;
        end;
     end;

  end;

  UpdateCursorPos;

  inherited;
end; // Selection Change


procedure TKntRichEdit.Change;
begin
  if FUpdating > 0 then exit;

  if FUpdating = 0 then begin
    if Modified and not IgnoringEditorChanges then
       App.ChangeInEditor(Self);
  end;

  inherited;
end; // Change


procedure TKntRichEdit.ChangedSelection;
begin
    if assigned(OnChangedSelection) then
       OnChangedSelection(Self);
end;



procedure TKntRichEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   bakS, S: integer;
   MousePos: TPoint;
begin
  if (Button = mbRight) then begin
      if SelLength > 0 then exit;

      bakS:= SelStart;
      MousePos.X  := X;
      MousePos.Y  := Y;
      S:= GetCharFromPos(MousePos);

      BeginUpdate;
      SetSelection(S, S + 1, false);
      if SelAttributes.Link then begin
         FPopupMenuBAK:= Self.PopUpMenu;
         Self.PopUpMenu := nil;
      end;

      SetSelection(bakS, bakS, false);
      EndUpdate;
  end;

  inherited;
end;


procedure TKntRichEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (CopyFormatMode <> cfDisabled) then
     PerformCmd(ecPasteFormat);

  if (Button = mbRight) and (FPopupMenuBAK <> nil) then
     PopupMenu := FPopupMenuBAK;

  ImageMng.CheckBringToFrontLastImageViewer;

  inherited;
end;


procedure TKntRichEdit.DblClick;
var
  ImgID, p, SL: integer;
  txt: AnsiString;
  ImgIDs: TImageIDs;
  Img: TKntImage;

begin
   ImgID:= 0;
   Img:= nil;
   if FSupportsRegisteredImages then begin
      SL:= SelLength;
      if SL = 1 then
         ImgID:= CheckToIdentifyImageID(p)

      else if SL > 1 then begin
         txt:= SelVisibleText;
         if Length(txt) < SL then begin
           ImgIDs:= ImageMng.GetImagesIDInstancesFromTextPlain(SelText);
           if ImgIDs <> nil then
              ImgID:= ImgIDs[0];
         end;
      end;
   end;

   if (ImgID = 0) and FSupportsImages and (not FSupportsRegisteredImages or not assigned(NNodeObj) ) then begin
      txt:= RtfSelText;
      if length(txt) > 0 then
         ImageMng.TryRTFPictToImage (@txt[1], Length(txt), Img);
   end;

   if (ImgID <> 0) or (Img <> nil) then
      ImageMng.OpenImageViewer(ImgID, false, false, Img);

   inherited;
end;


procedure TKntRichEdit.RxRTFURLClick(Sender: TObject; const URLText: string; Button: TMouseButton);
var
  chrg: TCharRange;
  myURLAction: TURLAction;
  CtrlWasDown, AltWasDown: boolean;
  EnsureAsk: boolean;

begin

  CtrlWasDown := CtrlDown;
  AltWasDown := AltDown;

  EnsureAsk:= false;

  if Button = mbLeft then begin
      if CtrlWasDown then
          myURLAction:= KeyOptions.URLCtrlAction
      else
         if AltWasDown then
            myURLAction:= urlCopy
         else
            myURLAction:= KeyOptions.URLAction;
  end
  else begin
      EnsureAsk:= true;
      if PopUpMenu = nil then
         myURLAction:= urlAsk
      else
         exit;
  end;

  chrg:= LinkClickRange;
  ClickOnURL (URLText, chrg, myURLAction, EnsureAsk);
end;



{  See comment in SetUpVCLControls (kn_VCLControlsMng.pas)

procedure TKntRichEdit.RxRTFStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  _Is_Dragging_Text := true;
end;

procedure TKntRichEdit.RxRTFEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  _Is_Dragging_Text := false;
  // StatusBar.Panels[0].Text := 'RTF end drag';
end;
}

procedure TKntRichEdit.RxRTFStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  // To ensure that the hidden image mark accompanies it in the event that it moves by dragging
   if FSupportsRegisteredImages then begin
      if SelLength = 1 then begin
         DraggingImageID:= CheckToIdentifyImageID(DraggingImage_PosFirstHiddenChar);
         if DraggingImageID > 0 then begin
            DraggingImage_PosImage:= SelStart;
            DraggingImageID:= - DraggingImageID;
         end;
      end;
   end;
end;


procedure TKntRichEdit.RxRTFEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  { The action that incorporates the hidden mark near the image is not performed here, since this event
    does not occur exactly at the completion of the action. If we compare Editor.SelStart here and from
    RxRTFSelectionChange we can see that when dragging to a later position, the value is different (not
    updated at this point), but it does match if you drag to an earlier position.
    To avoid problems, the action will be performed from RxRTFSelectionChange, and from here what we do
    is confirm that the action must be performed because the dragging action has not been canceled, and
    has been finished within the same editor. This is the reason why at RxRTFStartDrag we are assigning
    DraggingImageID a negative value.   }
  DraggingImageID:= - DraggingImageID;
end;



//----------------------------------------------------------------------------

procedure TKntRichEdit.TryPasteRTF(HTMLText: AnsiString=''; FolderName: String= '');
var
  RTF: AnsiString;
  posI: integer;

begin
    posI := SelStart;

    RTF:= Clipboard.TryOfferRTF(HTMLText, SelAttributes);            // Can return '' if clipboard already contained RTF Format
    if RTF <> '' then
        PutRtfText(RTF, true)
    else
        PasteBestAvailableFormat(FolderName, false, false);      // false: don't to try to offer RTF (Maybe it's already available), false: don't try to detect and correct HTML to RTF (will be done here)

    TryToDetectAndSolveHTMLtoRTFbadConv(posI);
end;


procedure TKntRichEdit.PastePlain( StrClp: string = ''; HTMLClip: AnsiString= '';
                                   ForcePlainText: boolean = false;
                                   MaxSize: integer = 0 );
var
   SS, j: integer;
   TextToReplace: string;
   Ok, DoUndo: boolean;

  function PasteOperationWasOK (): boolean;
  var
    i, j, n, m: integer;
    Selection: string;
    beginPos, len: Integer;
  begin
     i:= 1;
     j:= 1;

     n:= Length(StrClp);
     len:= 5;            // Will study only the beginning
     if n < 5 then
        len:= n;

     Selection:= SelVisibleText;
     m:= Length(Selection);

     Ok:= True;
     DoUndo:= False;
     while (i <= len) do begin
         if StrClp[i] <> #$A then begin
            if (j > m) or (StrClp[i] <> Selection[j]) then begin
               Ok:= False;
               break;
            end;
            Inc(j);
         end;
         Inc(i);
     end;

     if not Ok then begin
        // In some incorrect replacements, the selection is not modified, not replaced with
        // the plain text, but simply unselected. We need to distinguish this case,
        // because we should not emit any Undo operation, or it could undo other previous and correct
        // modification
        // I will see if the initial selection keeps the same

        n:= Length(TextToReplace);
        len:= 5;            // Will study only the beginning
        if n < 5 then
           len:= n;

        Selection:= GetTextRange(SS, SS + 5);
        m:= Length(Selection);

        i:= 1;
        j:= 1;
        while (i <= len) do begin
          if (j > m) or (TextToReplace[i] <> Selection[j]) then begin
             DoUndo:= True;
             break;
          end;
          Inc(j);
          Inc(i);
        end;

     end;

     Result:= Ok;
  end;

  procedure ReplaceBullets;
  var
     NewSelStart: integer;
     PlainText: string;
  begin
       // Replace bullets when pasting in plain text. By default a symbol in Cambria Math font + Tab character are added
       NewSelStart:= SelStart;
       PlainText:= GetTextRange(SS, NewSelStart);
       if pos(Char(10625), PlainText) > 0 then begin
          PlainText:= StringReplace(PlainText, Char(10625) + #9, EditorOptions.BulletsInPlainText, [rfReplaceAll]);  // Ord(TextToReplace) = 10625 : Symbol of font Cambria Math
          SetSelection(SS, NewSelStart, True);
          PutRtfText(PlainText, true);
       end;
  end;

begin
    Ok:= True;

    SS := SelStart;

    if ForcePlainText or PlainText or (ClipOptions.PlainTextMode = clptPlainText) then begin

       if RichEditVersion < 5 then
          TextToReplace:= GetTextRange(SS, SS + 5);    // To use it from PasteOperationWasOK()

       if MaxSize > 0 then begin
           if StrClp = '' then
              StrClp:= Clipboard.TryAsText;
           if ( length( StrClp ) > MaxSize ) then begin
              delete( StrClp, succ( ClipOptions.MaxSize ), length( StrClp ));
              Clipboard.AsText:= StrClp;
           end;
       end;

       PasteIRichEditOLE(CF_TEXT);

       ReplaceBullets;

       if RichEditVersion < 5 then begin
          if not PasteOperationWasOK() then begin
             if DoUndo then
                Undo;
             SelStart:= SS;
             SelLength:= 0;
          end;
       end;

    end
    else begin
          if HTMLClip = '' then
             HTMLClip:= Clipboard.AsHTML;

           SaveParagraphAttributes(ParaFormatToCopy);
           if (ClipOptions.PlainTextMode = clptAllowHyperlink) then
              SaveTextAttributes(FontFormatToCopy);

           var FolderName: string:= '';
           if assigned(ActiveFolder) then
              FolderName:= ActiveFolder.Name;

           TryPasteRTF(HTMLClip, FolderName);
           j := SelStart;
           SuspendUndo;
           try
              SelStart := SS;
              SelLength := j-SS+1;
              if (ClipOptions.PlainTextMode = clptAllowHyperlink) then begin
                 ApplyParagraphAttributes(ParaFormatToCopy);
                 ApplyTextAttributes(FontFormatToCopy);
              end else begin
                 ApplyParagraphAttributes(ParaFormatToCopy, True);
                 if (ClipOptions.PlainTextMode = clptAllowFontStyle) then begin
                    SelAttributes.Name := Form_Main.Combo_Font.FontName;
                    SelAttributes.Size := strtoint( Form_Main.Combo_FontSize.Text );
                 end;
              end;

              SelStart := j;
              SelLength := 0;
           finally
              ResumeUndo;
           end;
    end;
end; // PastePlain


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

procedure TKntRichEdit.PasteBestAvailableFormat (const FolderName: string;
                                           TryOfferRTF: boolean= True; CorrectHTMLtoRTF: boolean = False;
                                           PrioritizeImage: boolean = False);
var
  posI, pos: integer;
  RTFText: AnsiString;
  WasCopiedByKNT, ClipbHasRTFFormat: boolean;

begin
    WasCopiedByKNT:= ClipboardContentWasCopiedByKNT;         // If not -> it will also make LastCopiedIDImage <-0

    ClipbHasRTFFormat:= Clipboard.HasFormat(CFRtf);

    { If we are pasting an image copied from a browser, we must give preference to checking if it contains an image,
      because in those cases we will also find HTML content with the address and alternative text of the image,
      and if we do not do so we will always end up pasting the conversion to RTF from that alt text }
    if FSupportsImages and Clipboard.HasFormat(CF_BITMAP) and (PrioritizeImage or (not ClipbHasRTFFormat)) then begin
       ImageMng.InsertImageFromClipboard (Self, FolderName);
       exit;
    end;

    posI:= SelStart;

    if TryOfferRTF then
       RTFText:= Clipboard.TryOfferRTF('', SelAttributes);

    // If we have added RTF, we are going to use it instead of calling PasteFromClipboard, because the default font and size
    // will have set in this returned RTF text, not in the text in the clipboard
    if RTFText <> '' then
       PutRtfText(RTFText, true)

    else begin
      // If I paste text and images from WordPad it may appear as a "Wordpad Document" format, and in that case it only appears to paste the text.
      // It seems best to try to paste the RTF format if it is available

      if ClipbHasRTFFormat then
         try
            PasteIRichEditOLE(CFRtf);
         except
           { The conversion from HTML to RTF may be incorrect on some pages and cause an exception on the previous line
             for example returning the message "FORMATEC structure not valid"
             This happens, for example, when you copy and paste the first answer that appears on this page:
              https://stackoverflow.com/questions/4960829/how-to-convert-from-dos-path-to-file-scheme-uri-in-batch-file}
            Self.PasteFromClipboard;
            exit;
         end

      else
         Self.PasteFromClipboard;       //Editor.PasteIRichEditOLE(0);

      if FSupportsRegisteredImages and ClipbHasRTFFormat then begin
         pos:= SelStart;
         { We will have treated Copy or Cut in a special way if SelLength=1 because this implied that an image could be selected individually and we
           want to be able to offer it as such and not only as RTF (which would happen if we copy the image to the clipboard along with the hidden mark )
          (Although this image copied in this way - without a mark - when it is in PNG or JPG, the control does not offer it as BMP but only as a Metafile
           and not all programs recognize it. For example, GreenShot does recognize it, but Photoshop or WordPad itself They don't recognize it --WordPad
           does paste it, but because it uses the RTF format) }
         if (not WasCopiedByKNT) or LastCopyFromScratchpad or ((pos = posI + 1) and (LastCopiedIDImage<>0)) then
            if ImageMng.ProcessImagesInClipboard (Self, FolderName, posI, LastCopiedIDImage) then begin
               if (LastCopiedIDImage=0) and LastCopyFromScratchpad then
                  Form_Main.Res_RTF.ReconsiderImages(true, imImage);      // Tag the image[s] with the hidden mark[s] in Sratchpad
            end;
      end;

    end;

    if CorrectHTMLtoRTF then
       TryToDetectAndSolveHTMLtoRTFbadConv(posI);

end;


procedure TKntRichEdit.TryToDetectAndSolveHTMLtoRTFbadConv (posI: integer);
var
  posF: integer;
  PlainText: string;

begin
    // posI must receive Editor.SelStart before paste the suspicious text

    if not (Clipboard.HasHTMLformat and Clipboard.HasRTFformat) then
       exit;

    posF := SelStart;
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
        SuspendUndo;
        try
           SelStart  := posI;
           SelLength := posF - posI;

           SelText := PlainText;
           SelStart  := posI + SelLength;

           SelLength := 0;

        finally
           ResumeUndo;
        end;
     end;

end;


//-----------------------------------------------------------------------

function TKntRichEdit.GetZoom: integer;
var
  W, L : integer;
begin
  result := DefaultEditorProperties.DefaultZoom;
  if ( _LoadedRichEditVersion < 3 ) then exit; // cannot zoom

  SendMessage( Handle, EM_GETZOOM, integer(@w), integer(@l) );
  if ( w = 0 ) then w := 1;
  if ( l = 0 ) then l := 1;
  result := makepercentage( w, l );
end; // GetEditorZoom


procedure TKntRichEdit.SetZoom(ZoomValue : integer; ZoomString : string; Increment: integer= 0 );
var
  CurrentZoom : integer;
  NewZoom : integer;
  p : integer;
begin
  if ( _LoadedRichEditVersion < 3 ) then exit; // cannot zoom

  CurrentZoom := GetZoom;
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
          App.ErrorPopup(E, sEdt01);
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

  SendMessage( Handle, EM_SETZOOM, NewZoom, 100 );
  SendMessage( Handle, EM_SCROLLCARET, 0, 0);

  FZoomGoal:= NewZoom;
  FZoomCurrent:= NewZoom;

  if Focused then
     App.ShowCurrentZoom(NewZoom);

end; // SetZoom


procedure TKntRichEdit.RestoreZoomGoal;
begin
   if fZoomGoal <> GetZoom then
      SetZoom(fZoomGoal, '');
end;

procedure TKntRichEdit.RestoreZoomCurrent;
begin
  if ( _LoadedRichEditVersion < 3 ) then exit; // cannot zoom

  SendMessage( Handle, EM_SETZOOM, FZoomCurrent, 100 );
  SendMessage( Handle, EM_SCROLLCARET, 0, 0);

  if Focused then
     App.ShowCurrentZoom(FZoomCurrent);
end; // RestoreZoomTemp

function TKntRichEdit.GetAndRememberCurrentZoom: integer;
begin
   FZoomCurrent:= GetZoom;
end;



procedure TKntRichEdit.SetMargins;
begin
  with KeyOptions do
     if AltMargins then
        inherited SetMargins(MarginAltLeft, MarginAltRight)
     else
        inherited SetMargins(0, 0);
end;



//-------------------------------------------

var
  ShowingSelectionInformation: boolean;


procedure TKntRichEdit.UpdateWordCount;
var
  p, s : string;
  wc : longint;
  i: integer;
  numPag: single;
begin
  if EditorOptions.WordCountTrack then begin

    if ( SelLength = 0 ) then
       wc := GetWordCount( Text )    // Editor.Text is much faster than Editor.Lines.Text
    else
       wc := GetWordCount( SelText );

    if ( wc > 0 ) then begin
      if ( EditorOptions.WordsPerPage > 0 ) then
         p := Format(' / %.1f', [wc / EditorOptions.WordsPerPage] )
      else
         p := '';
      s := Format( ' W: %d', [wc] ) + p;
    end
    else
       s := ' W: 0';

    App.WordCountInfoInStatusBar:= s;

  end
  else begin
    if ( not EditorOptions.TrackCaretPos ) then begin
       App.WordCountInfoInStatusBar:= '';
       exit;
    end;
  end;

end; // UpdateWordCount


procedure TKntRichEdit.CleanWordCount;
begin
    App.WordCountInfoInStatusBar:= ' W: ... / ...';
end;

procedure TKntRichEdit.CheckWordCount (cleanWC: boolean= false);
begin
  if EditorOptions.WordCountTrack then
     if (TextLength <= 30000) then
        UpdateWordCount
     else
         if cleanWC then
            CleanWordCount;
end;

procedure TKntRichEdit.CheckWordCountWaiting;
const
   Waiting : string = ' (...) ';
var
   txt: string;

begin
   if EditorOptions.WordCountTrack and (TextLength > 30000) then begin
      txt:= App.WordCountInfoInStatusBar;
      if (pos('...', txt) = 0) then
         App.WordCountInfoInStatusBar:= Waiting + txt;
   end;
end;

procedure TKntRichEdit.UpdateCursorPos;
var
  p : TPoint;
  Str: string;
  SSi, SS, SSexp, SL, V, X, IncX: integer;
begin
    SL:= SelLength;
    SS:= SelStart;
    if ( SL > 0 ) then ShowingSelectionInformation:= true;

    if EditorOptions.TrackCaretPos then begin
      {
       CaretPos returns the position of the cursor on the line as x (p.X), counting both visible and hidden characters.
       Hidden are mainly those corresponding to hyperlinks (HYPERLINK "<URL>"), although there may also be some hidden
       characters used by KeyNote to identify images or set bookmarks for the location of KNT Links.
       The first thing is to check if we have any hidden characters in the line position we are in. A quick way can be to search
       for the word HYPERLINK and one of the hidden characters that KNT uses. If there are none, we can safely use the value
       returned by CaretPos. If the word HYPERLINK was not hidden, nothing happens, we will simply do the calculation by one of
       the other ways.

       If there are hidden characters on the left, the second thing is to check if the cursor is inside a hyperlink or not.
       The reason? We will need to select the text from the beginning of the line to be able to obtain the visible text, but
       if we are within a hyperlink this will select not only to the position we are in, but to the end of the hyperlink.
       Everything that is selected beyond our position (the rest of the hyperlink) is visible text.
       After selection -> Consult both the length of the visible text obtained and the most extreme position reached, therefore
       including the end of the hyperlink. Anything that has expanded the selection too much will be hidden characters that we
       must subtract from the length of the visible text:

        .....Text of some hyperlink
        ^         ^               ^
        SSi       SS              SSexp
                  <------ D ------>
        <-------------------------> V
        <---------> X

       If SS = SSexpand => the cursor is not over any hyperlink.

       Although we use BeginUpdate and EndUpdate(false) (false so that it does not trigger an Invalidate and Refresh after the
       EndUpdate), a slight flicker is still noticeable in certain parts of the editor at certain times when quickly scrolling
       through a line in which there are hidden characters (e.g., by having a hidden marker).
       To avoid this I am going to apply a certain optimization:
       I am going to save the latest calculated data and try to take advantage of it to minimize the number of times it is necessary
       to change the selection to see the length of the visible text.
      }

       p := CaretPos;
       if ( SL = 0 ) then begin
          IncX:= 0;
          X:= -1;
          SSi:= -1;
          if (p.Y = LastPy) then begin
             if (p.X = LastPx+1) or (p.X = LastPx-1) then
                X:= LastX + (p.X - LastPx)
             else
             if (SS > LastSS) and (LastX > 0) then begin
                SSi:= LastSS;
                IncX:= LastX;
             end
             else
                LastPx:= 0;
          end
          else
              LastPx:= 0;

          if X < 0 then begin
             if SSi = -1 then
                SSi:= Perform(EM_LINEINDEX, p.y, 0);

             Str:= GetTextRange(SSi, SSi + p.X - LastPx);
             if (pos(KNT_RTF_HIDDEN_MARK_L_CHAR, Str, 1) = 0) and (pos('HYPERLINK ', Str, 1) = 0) then
                X:= (p.X - LastPx) + IncX

             else begin
                FIgnoreSelectionChange:= True;
                BeginUpdate;
                try
                  SetSelection(SSi, SS, false);
                  SSexp:= SelStart + SelLength;
                  V:= Length(Self.SelVisibleText);
                  X:= V - (SSexp-SS) + IncX;
                  SelStart:= SS;
                finally
                  EndUpdate (False);
                  FIgnoreSelectionChange:= False;
                end;
             end;
          end;
          LastSS:= SS;
          LastPx:= p.X;
          LastPy:= p.Y;
          LastX:= X;
          App.WordCountInfoInStatusBar:= Format( sEdt02, [succ( p.y ), Lines.Count, succ(X)] );
       end
       else begin
          LastPy:= -1;
          Str:= SelVisibleText;
          App.WordCountInfoInStatusBar:= Format( sEdt03, [Length(Str), GetWordCount(Str)] );
       end;
    end
    else begin
      if EditorOptions.WordCountTrack  then begin
         if ( SL > 0 ) then
            UpdateWordCount
         else
            if ShowingSelectionInformation then
               CheckWordCount(true);
      end else
         App.WordCountInfoInStatusBar:= '';
    end;

    if ( SelLength = 0 ) then
       ShowingSelectionInformation:= false;

end; // UpdateCursorPos


procedure TKntRichEdit.GoToLine (const s : string);
var
  curIdx, newIdx : integer;
  P : TPoint;
begin
   p := CaretPos;
   curIdx := succ( p.Y ); // zero-based
   if ( s[1] in ['+','-'] ) then
      newIdx := curIdx + strtoint( s )
   else
      newIdx := strtoint( s );

   if ( newIdx < 1 ) then
      newIdx := 1
   else
   if ( newIdx > Lines.Count ) then
      newIdx := Lines.Count;

   SelStart := Perform( EM_LINEINDEX, pred( newIdx ), 0 );
   Perform( EM_SCROLLCARET, 0, 0 );

end; // GoToLine

procedure TKntRichEdit.Navigate(NavDirection : TNavDirection);
var
  VKey : Word;
  SBVal, ScrollVal, ScrollMsg : integer;
begin

  VKey := 0;
  SBVal := SB_LINEDOWN;
  ScrollVal := SB_VERT;
  ScrollMsg := WM_VSCROLL;

    case NavDirection of
      navUp : begin
        SBVal := SB_LINEUP;
        ScrollVal := SB_VERT;
        ScrollMsg := WM_VSCROLL;
      end;
      navDown : begin
        SBVal := SB_LINEDOWN;
        ScrollVal := SB_VERT;
        ScrollMsg := WM_VSCROLL;
      end;
      navLeft : begin
        SBVal := SB_LINELEFT;
        ScrollVal := SB_HORZ;
        ScrollMsg := WM_HSCROLL;
      end;
      navRight : begin
        SBVal := SB_LINERIGHT;
        ScrollVal := SB_HORZ;
        ScrollMsg := WM_HSCROLL;
      end;
    end;

    perform(ScrollMsg, MakeLong(SBVal, GetScrollPos(handle, ScrollVal)), 0);
    perform(ScrollMsg, MakeLong(SB_ENDSCROLL, GetScrollPos(handle, ScrollVal)), 0);

end; // Navigate


function TKntRichEdit.GetStatistics (var numChars, numAlpChars, numWords: integer): string;
var
  s, title : string;
  lista : TStringList;
  i, l, len, numSpaces, numLines: integer;
  WasAlpha : boolean;
  ch : char;

begin

  App.ShowInfoInStatusBar(sEdt28);
  screen.Cursor := crHourGlass;
  lista := TStringList.Create;

  try
     if (SelLength > 0) then begin
       lista.Text := SelText;
       title := sEdt29;
     end
     else begin
       lista.Text := Lines.Text;
       title := sEdt30;
     end;

     numLines := lista.count;

     numChars := 0;
     numSpaces := 0;
     numAlpChars := 0;
     numWords := 0;

     for l := 0 to lista.count-1 do begin
       s := lista[l];
       len := length(s);
       inc(numChars, len);
       WasAlpha := false;

       for i := 1 to len do begin
          ch := s[i];
          if IsCharAlpha(ch) then begin
            inc(numAlpChars);
            WasAlpha := true;
          end
          else begin
            if ( ch in [#9,#32] ) then
              inc(numSpaces);
            if WasAlpha then
              inc(numWords);
            WasAlpha := false;
          end;
       end;

       if WasAlpha then
          inc(numWords);
     end;


  finally
    lista.Free;
    screen.Cursor := crDefault;
  end;

  s := format(sEdt31,
               [Title, inttostr(numChars), inttostr(numAlpChars),
                inttostr(numSpaces), inttostr(numWords), inttostr(numLines)]);

  Result:= s;

end; // GetStatistics



//-------------------------------------------------------------------------

function TKntRichEdit.FontInfoString : string;
begin
  result := Format(
    ' %s %d pt %s %s ',
    [SelAttributes.Name,
    SelAttributes.Size,
    FontStyleToStr( SelAttributes.Style ),
    ColorToString( SelAttributes.Color )
    ]);

    if SelAttributes.Disabled then
      result := result + 'Dsbl ';
    if ( SelAttributes.SubscriptStyle <> ssNone ) then
      result := result + SubscriptStyleToStr( SelAttributes.SubscriptStyle ) + ' ';
    if ( SelAttributes.BackColor <> clWindow ) then
      result := result + 'High';
end; // FontInfoString


function TKntRichEdit.ParaInfoString : string;
begin
  result := Format(
  ' %s, %s spc, %s L:%d F:%d R:%d Bef:%d Aft:%d',
  [
    AlignmentToStr( Paragraph.Alignment ),
    LineSpacingToStr( Paragraph.LineSpacing ),
    NumberingToStr( Paragraph.Numbering ),
    Paragraph.LeftIndent,
    Paragraph.FirstIndent,
    Paragraph.RightIndent,
    Paragraph.SpaceBefore,
    Paragraph.SpaceAfter
  ]);
end; // ParaInfoString


/// Save on 'Paragraph' the paragraph formatting attributes of the current selection
//
procedure TKntRichEdit.SaveParagraphAttributes(var Paragraph: TParaFormat2);
begin
   Self.Paragraph.GetAttributes(Paragraph);
end;

/// Sets the paragraph formatting for the current selection with the attributes saved on 'Paragraph'
//
procedure TKntRichEdit.ApplyParagraphAttributes(var Paragraph: TParaFormat2;
                                                const Reduced: Boolean = False);
begin
   Paragraph.dwMask:= PFM_STARTINDENT or PFM_RIGHTINDENT or PFM_OFFSET or
                      PFM_ALIGNMENT or
                      PFM_TABSTOPS or
                      PFM_SPACEBEFORE or PFM_SPACEAFTER or PFM_LINESPACING or
                      PFM_SHADING or
                      PFM_BORDER or
                      PFM_NUMBERING or PFM_NUMBERINGTAB or PFM_NUMBERINGSTART or PFM_NUMBERINGSTYLE
                      // or PFM_EFFECTS or PFM_RTLPARA or PFM_OFFSETINDENT or PFM_STYLE
                      ;

   if Reduced then     // Preserve: Numbering, FirstIndent, LeftIndent, RightIndent, Alignment
       Paragraph.dwMask:= Paragraph.dwMask
                           and not PFM_NUMBERING and not PFM_STARTINDENT and not PFM_OFFSET and not PFM_RIGHTINDENT and not PFM_ALIGNMENT;

   Self.Paragraph.SetAttributes(Paragraph);
   HideKNTHiddenMarks(true);
end;


/// Save on 'Format' the attributes of the first character
//
procedure TKntRichEdit.SaveTextAttributes(var Format: TCharFormat2);
var
   SelLengthBak: Integer;
begin

   SelLengthBak:= SelLength;

   if SelLengthBak = 0 then
       SelAttributes.GetAttributes(Format)
   else begin
      // A single charecter is selected because EM_GETCHARFORMAT is actually returning the attributes
      // of the last 'selected' character. This is, actually you could obtain different attributes if
      // you select the same text from right to left instead of left to right (suppose the first character
      // is bold and last one is italic) (¿RichEdit Bug?)

      BeginUpdate;
      SelLength:= 1;
      try
         SelAttributes.GetAttributes(Format);
      finally
        SelLength:= SelLengthBak;
        EndUpdate;
      end;
   end;

end;


/// Sets character formatting on the selection (or word at cursor) with the attributes saved in 'Format'
//
procedure TKntRichEdit.ApplyTextAttributes(var Format: TCharFormat2);
begin
   { *1 We must ignore CFM_HIDDEN or else, when pasting the format from a text that begins with a hyperlink
        we can hide the text to which we apply it }
   Format.dwMask := DWORD(CFM_ALL2) and not CFM_HIDDEN;  // *1
   if _LoadedRichEditVersion < 6 then begin     // #529 : Paste Font Attributes destroy selected hyperlinks when applied (in Richedit <= 5.0)
       Format.dwMask := Format.dwMask
                        //and not CFM_HIDDEN
                        and not CFM_LINK;
   end;

   if SelLength = 0 then
      WordAttributes.SetAttributes(Format)
   else
      SelAttributes.SetAttributes(Format);

   HideKNTHiddenMarks(true);
end;


// Returns True if there is one or more paragraphs selected (although partially)

function TKntRichEdit.ParagraphsSelected: Boolean;
var
  Sel: TCharRange;
  posBegin, posEnd: integer;
begin
   Result:= False;
   Sel:= GetSelection;

   if not (Sel.cpMin= Sel.cpMax) then begin   // There is text selected
      if FindText(#13, Sel.cpMin, Sel.cpMax-Sel.cpMin, []) > 0 then
          Result:= True
      else
          if ((Sel.cpMin=0) or (GetTextRange(Sel.cpMin-1, Sel.cpMin) = #13))   and
             ((GetTextRange(Sel.cpMax, Sel.cpMax+1) = #13) or (Sel.cpMax=TextLength)) then
             Result:= True;
   end;
end;


{ Determines the indent of the current line or paragraph (depending on paragraphMode
 parameter) and the indent that could be applied to next line with AutoIndent.
 If paragraphMode = False (lineMode) then
   LineStr: the line (as shown in Editor) where the caret is positioned
 If paragraphMode = True:
   LineStr: the line (as shown in Editor) corresponding to the beginning of
   paragraph where the caret is positioned

  PosBegin: the position of the first character of LineStr, in the Editor
}
procedure TKntRichEdit.GetIndentInformation(var Indent: integer; var NextIndent: integer;
                               var LineStr: string;
                               var posBegin : integer;
                               paragraphMode: boolean= false);
var
   SelS, Col: integer;
   LineBegin, LineCaret : integer;
begin
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


//--------------------------------------------------------------------------

procedure TKntRichEdit.GetLinkAtCursor(var URL: string; var TextURL: string;
                                       var LeftE: integer; var RightE: integer;
                                       SelectURL: boolean= true);
var
    link: TRxLinkStyle;    // lsNone: link style not set  lsLink: link style set    lsMixed: mixed                             
    TextLen: integer;
    Left, Right: integer;
    SelS, SelL: integer;
begin

  BeginUpdate;
  try

    URL:= '';
    TextURL:= '';
    SelS:= SelStart;
    SelL:= SelLength;
    Left:= SelS;
    Right:= SelL + Left;

    SetSelection(Left, Left+1, false);
    link:= SelAttributes.LinkStyle;
    if link = lsLink then begin
       SetSelection(Left-1, Left, false);
       link:= SelAttributes.LinkStyle;
    end;

    if (link <> lsLink) then begin
        SetSelection(Left, Right, false);
        LeftE:= Left;
        RightE:= Left;
    end
    else begin
        TextLen:= TextLength;

        // Buscamos el extremo izquierdo

        { Character "H" of HYPERLINK ".... cannot be selected in versions >= 5. Also in that versiones, when trying to selection
          one hidden character selection of that hyperlink, the whole link is selected }
        link:= lsLink;
        while (Left >0) and (link=lsLink) and (SelLength=1) do begin
              Left:= Left - 1;
              SetSelection(Left-1, Left, false);
              link:= SelAttributes.LinkStyle;
        end;

        if (SelLength > 1) then begin
           Left:= SelStart;
           Right:= Left + SelLength;
           LeftE:= Left;
           RightE:= Right;
        end
        else begin
           LeftE:= Left;
           // Ampliamos la selección incluyendo el/los caracteres '<' que pueda haber
           while (LeftE >0) and (SelText='<') do begin
                 LeftE:= LeftE - 1;
                 SetSelection(LeftE-1, LeftE, false);
           end;


           // Buscamos el extremo derecho
           link:= lsLink;
           while (Right < TextLen) and (link=lsLink) do begin
                 Right:= Right + 1;
                 SetSelection(Right, Right+1, false);
                 link:= SelAttributes.LinkStyle;
           end;

           RightE:= Right;
           // Ampliamos la selección incluyendo el/los caracteres '>' que pueda haber
           while (Right < TextLen) and (SelText='>') do begin
                 RightE:= RightE + 1;
                 SetSelection(RightE, RightE+1, false);
           end;
        end;

        URL:= GetTextRange(Left, Right);
        if pos('HYPERLINK "', URL)= 1 then begin
           // If it is an hyperlink with text associated then the string will be: URL"TextURL, where only TextURL is visible
           SetSelection(Left, Right, false);
           TextURL:= SelVisibleText;
           URL:= Copy(URL, 12, Length(URL) - Length(TextURL)- 12);
        end
        else
          if SelectURL then
              SetSelection(LeftE, RightE, false);

        if not SelectURL then
           SetSelection(SelS, SelS + SelL, false);

    end;

  finally
     EndUpdate;
  end;
end;


function TKntRichEdit.GetWordAtCursor( const LeaveSelected : boolean; const IgnoreActualSelection: boolean = False;
                                          const DiscardKNTHiddenCharacters: boolean= True;
                                          const SpacesAsWordDelim: boolean= False
                                           ) : string;
var
  L, R,  Rm, Offset: integer;
  SS, SL: integer;
  SSw: integer;
  Str: string;
  KeepSelected: boolean;


  function IsWordDelimiter(i: integer; const Str: string; const SpaceAsWDelim: boolean): boolean; inline;
  begin
    Result:= (Str[i]<>KNT_RTF_HIDDEN_MARK_L_CHAR) and (Str[i]<>KNT_RTF_HIDDEN_MARK_R_CHAR)
               and (   (SpaceAsWDelim and (Str[i] in [#32,#9,#13,#10]))
                    or (not SpaceAsWDelim and not IsCharAlphaNumeric(Str[i])) );
  end;

begin
  SL:= SelLength;
  if (SL > 0) and not IgnoreActualSelection then Exit(SelText);

  SS:=  SelStart;
  L:= SS-40;
  if L < 0 then L:= 0;
  R:= SS + 40;


  SelStart:= L;
  SelLength:= R-L;


  // We must check the values actually set, at least L, because trying to place the cursor in the middle of
  // hidden text (eg from a HYPERLINK) or extending the selection over them, can cause the position of this to change,
  // and we need it as a reference to know where it really is, in the string that we retrieve with TextPlain, the initial position (SS).

  // Although we have tried to get an apparently sufficient number of characters for the word, in anticipation that that the cursor
  // could be to the left or to the right of it, if there are hidden texts, in the end the selection will be enlarged and we will obtain
  // more characters.

  L:= SelStart;
  KeepSelected:= False;

  try
      Str:= TextPlain(True);
      if Trim(Str) = '' then Exit;

      SSw:= SS-L +1;
      Rm:= Length(Str);
      Offset:= 0;
      if SSw > Rm then begin
         SSw:= Rm;
         dec(SS);
      end;

      if IsWordDelimiter(SSW, Str, SpacesAsWordDelim) then begin
        dec(SSw);
        Offset:= -1;
        if (SSw=0) or IsWordDelimiter(SSW, Str, SpacesAsWordDelim) then
           Exit;
      end;

      KeepSelected:= LeaveSelected;

      L:= SSw;
      repeat
         if (Str[L]=KNT_RTF_HIDDEN_MARK_R_CHAR) then begin
            repeat
                dec(L);
            until (L<1) or (Str[L]=KNT_RTF_HIDDEN_MARK_L_CHAR);
         end;
         dec(L);
      until (L<1) or IsWordDelimiter(L, Str, SpacesAsWordDelim);
      inc(L);

      R:= SSw;
      repeat
         if (Str[R]=KNT_RTF_HIDDEN_MARK_L_CHAR) then begin
            repeat
                inc(R);
            until (R>Rm) or (Str[R]=KNT_RTF_HIDDEN_MARK_R_CHAR);
         end;
         inc(R);
      until  (R>Rm) or IsWordDelimiter(R, Str, SpacesAsWordDelim);
      dec(R);

      Result:= Copy(Str, L, R-L +1);

  finally
      if DiscardKNTHiddenCharacters then
         Result:= RemoveKNTHiddenCharactersInText(Result);

      if KeepSelected then begin
         SelStart := SS-(SSw-L) + Offset;
         SelLength := R-L +1;
      end
      else begin
        SelStart:= SS;
        SelLength:= SL;
      end;

  end;

end;   // GetWordAtCursor


function TKntRichEdit.CheckReadOnly: boolean;
begin
    Result:= False;

    if ReadOnly then begin
       App.WarnEditorIsReadOnly;
       Result:= True;
       exit;
    end;
end;

//----------------------------------------------------------------------------

procedure TKntRichEdit.ArabicToRoman;
var
  s : string;
  i : integer;
begin
  if CheckReadOnly then exit;

  s := SelText;
  if (s = '') then
    InputQuery(sEdt05, sEdt06, s)
  else
     s:= KeepOnlyLeadingKNTHiddenCharacters(s);

  if (s = '') then exit;

  try
    s := trim( s );
    i := strtoint( s );
    s := DecToRoman( i );
  except
    App.ErrorPopup(Format(sEdt07, [s]));
    exit;
  end;

  SelText := s;

end; // ArabicToRoman;


procedure TKntRichEdit.RomanToArabic;
var
  s : string;
  i : integer;
begin
  if CheckReadOnly then exit;
  i := -1;

  s := SelText;
  if ( s = '' ) then
     InputQuery( sEdt08, sEdt09, s )
  else
     s:= KeepOnlyLeadingKNTHiddenCharacters(s);

  if (s = '') then exit;

  try
    s := AnsiUpperCase(trim(s));
    i := RomanToDec(s);
  except
    App.ErrorPopup(Format(sEdt10, [s]));
    exit;
  end;

  if ( i < 0 ) then exit;

  SelText := inttostr( i );

end; // RomanToArabic


//---------------------------------------------------

procedure TKntRichEdit.MatchBracket;
type
  TDir = ( dirFwd, dirBack );
const
  OpenBrackets = '([{<';
  CloseBrackets = ')]}>';
var
  startch, seekch, curch : char;
  i, stack, curcol, curline : integer;
  p : TPoint;
  dir : TDir;
  Found : boolean;

begin

  p := CaretPos;

  if (( Lines.Count = 0 ) or
      ( length( Lines[p.y] ) = 0 )) then
      exit;

  if (SelLength = 0) and (Length(Lines[p.y]) > p.x) then
    inc( p.x );
  startch := Lines[p.y][p.x];

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
      App.ShowInfoInStatusBar(sEdt11);
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
      while ( curline < Lines.Count ) do begin
        while ( curcol <= length( Lines[curline] )) do begin
          curch := Lines[curline][curcol];
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
          curch := Lines[curline][curcol];
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
          curcol := length( Lines[curline] );
      end;
    end;
  end;

  if Found then begin
    App.ShowInfoInStatusBar(sEdt12);
    SelStart := Perform( EM_LINEINDEX, p.y, 0 );
    SelStart := SelStart + pred( p.x );
    Perform( EM_SCROLLCARET, 0, 0 );
    SelLength := 1;
  end
  else
    App.ShowInfoInStatusBar(sEdt13);

end; // MatchBracket


procedure TKntRichEdit.TrimBlanks( const TrimWhat : integer );
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
  if CheckReadOnly then exit;


  if ( Lines.Count < 1 ) then exit;

  wholeNote:= false;
  if ( SelLength = 0 ) then begin
    wholeNote:= true;
    if messagedlg(sEdt14, mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes then exit;
  end;

  BeginUpdate;
  Screen.Cursor := crHourGlass;

  try
      if wholeNote then
         s:= GetTextRange(0, TextLength)
      else
         s := SelText;

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
         Text := s
      else
         SelText := s;

      HideKNTHiddenMarks(true);

  finally
    EndUpdate;
    Screen.Cursor := crDefault;
    Change;
  end;


end; // TrimBlanks


procedure TKntRichEdit.CompressWhiteSpace;
const
  WhiteSpace : set of AnsiChar = [#9, #32];
var
  WasWhite : boolean;
  i, l : integer;
  s : string;

begin
  if CheckReadOnly then exit;
  if ( Lines.Count < 1 ) then exit;

  if ( SelLength = 0 ) then begin
     if ( messagedlg(sEdt15, mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then
         exit;
  end;

  BeginUpdate;
  Screen.Cursor := crHourGlass;
  WasWhite := false;

  try
    if ( SelLength = 0 ) then begin

       for l := 0 to Lines.Count-1 do begin
         if (Lines[l] = '') then continue;

         WasWhite := false;
         i := 1;
         s := Lines[l];

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
         Lines[l] := s;
         HideKNTHiddenMarks(true);
       end;
       SelStart := 0;

    end
    else begin
       s := SelText;
       i := 1;
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
       SelText := s;
       HideKNTHiddenMarks(true);
       SelLength := 0;
    end;


  finally
     EndUpdate;
     Screen.Cursor := crDefault;
     ActiveEditor.Change;
  end;

end; // CompressWhiteSpace


//---------------------------------------------------

procedure TKntRichEdit.EvaluateExpression;
var
  src : string;
  i, l, lineindex : integer;
  MathParser : TMathParser;

begin

   if ( SelLength = 0 ) then begin
       lineindex := perform( EM_EXLINEFROMCHAR, 0, SelStart );
       SelStart  := perform( EM_LINEINDEX, lineindex, 0 );
       SelLength := perform( EM_LINEINDEX, lineindex + 1, 0 ) - ( SelStart+1 );
   end;

   src := trim( SelText );

   if ( src = '' ) then begin
     App.WarnNoTextSelected;
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
       OnParseError := Form_Main.MathParserParseError;
       MathParser.ParseString := src;
       Parse;
     end;

     if ( not MathParser.ParseError ) then begin
       LastEvalExprResult := FloatToStrF(MathParser.ParseValue, ffGeneral, 15, 2);
       Clipboard.SetTextBuf(PChar(LastEvalExprResult));
       App.ShowInfoInStatusBar(sEdt16 + LastEvalExprResult);
       Form_Main.MMEditPasteEval.Hint := sEdt17 + LastEvalExprResult;

       if KeyOptions.AutoPasteEval and ( not Self.ReadOnly) then begin
          SelStart := SelStart + SelLength;
          SelText := #32 + LastEvalExprResult;
       end
       else
          if ( messagedlg( Format( sEdt18, [src,LastEvalExprResult] ), mtInformation, [mbOK,mbCancel], 0 ) = mrOK ) then begin
             if not CheckReadOnly then begin
               SelStart := SelStart + SelLength;
               SelText := #32 + LastEvalExprResult;
             end;
          end;
     end;

   finally
     MathParser.Free;
   end;

end; // EvaluateExpression


//---------------------------------------------------

procedure TKntRichEdit.InsertPictureOrObject (const AsPicture : boolean);
var
  Pict: TPicture;
  wasmodified : boolean;
  OpenPictureDlg : TOpenPictureDialog;
  SelStartOrig, SelLengthOrig: integer;
  NewName: string;

begin
  if CheckReadOnly then exit;
  if not SupportsImages then exit;

  wasmodified:= false;

  OpenPictureDlg := TOpenPictureDialog.Create( Form_Main );
  try
    if AsPicture then
       with OpenPictureDlg do begin
          Options := [ofHideReadOnly,ofPathMustExist,ofFileMustExist];
          Title:= sEdt19;
          Filter:= sEdt20 + FILTER_IMAGES;

          if Execute then begin
             if SupportsRegisteredImages then begin
                if SelLength > 0 then
                   CheckToSelectLeftImageHiddenMark;
             end;

             NewName:= ExtractFilename(FileName);
             if (not KeyOptions.ImgDefaultLinkMode) and (not ImageMng.CheckUniqueName (NewName)) then begin
                var FileList: TStringList;
                try
                   FileList:= TStringList.Create;
                   FileList.Add (FileName);
                   FileDropped(nil, FileList, Self);
                finally
                   FileList.Free;
                end;
             end
             else
                ImageMng.InsertImage(FileName, Self, not KeyOptions.ImgDefaultLinkMode, NewName);

              // See comments before TImageMng.InsertImage
              {
              Pict := TPicture.Create;
              try
                 Pict.LoadFromFile(FileName);
                 Clipboard.Assign(Pict);
                 ActiveFolder.Editor.PasteIRichEditOLE(0);
              finally
                 Pict.Free;
                 wasmodified:= true;
              end;
              }
          end;
       end
    else begin
      if SupportsRegisteredImages then begin
         CheckToSelectLeftImageHiddenMark (SelStartOrig, SelLengthOrig);
         if InsertObjectDialog then
            wasmodified := true
         else
            if SelStartOrig >= 0 then
               SetSelection(SelStartOrig, SelStartOrig + SelLengthOrig, true);
      end
      else
         if InsertObjectDialog then
            wasmodified := true;
    end;

  finally
    if wasmodified then
       Change;
    OpenPictureDlg.Free;
  end;

end; // InsertPictureOrObject


//------------------------------------------------------------


procedure TKntRichEdit.InsertSpecialCharacter;
begin
   if CheckReadOnly then exit;

   if (Form_Chars = nil) then begin
      Form_Chars := TForm_CharsNew.Create( Form_Main );

      with Form_Chars do begin
        ShowHint :=      KeyOptions.ShowTooltips;
        AutoAddNew :=    KeyOptions.InsCharAutoAddNew;
        RTFCustomChars:= KeyOptions.InsCharCustom;

        CharInsertEvent := Self.CharInsertProc;
        FormCloseEvent  := Self.Form_CharsClosed;

        if KeyOptions.InsCharWinClose then
           btnInsert.ModalResult := mrOK
        else
           btnInsert.ModalResult := mrNone;
      end;
   end;

   try
     Form_Chars.Show;

   except
     on E : Exception do
       ShowMessage( E.Message );
   end;

end; // InsertSpecialCharacter


class procedure TKntRichEdit.CharInsertProc( const ch : Char; const Count : integer; const FontName : string; const FontCharset : TFontCharset; Unicode: boolean= true);
begin
   if assigned(ActiveEditor) then
     ActiveEditor.InsertChar(ch, Count, FontName, FontCharset, Unicode);
end;


procedure TKntRichEdit.InsertChar(const ch: Char; const Count: integer; const FontName: string; const FontCharset: TFontCharset;
                                  Unicode: boolean= true);
var
  s : String;
  CurrentFontName: string;
  CurrentCharset: TFontCharset;
  Cmd: TEditCmd;
  AnsiCh: AnsiChar;

begin
  if CheckReadOnly then exit;

  try
    if FSupportsRegisteredImages then begin
       if SelLength > 0 then
          CheckToSelectLeftImageHiddenMark;
    end;

    if Unicode then
       Cmd:= ecInsCharacterU
    else begin
       Cmd:= ecInsCharacter;
       AnsiCh:= AnsiChar(Ch);
    end;

    with CommandRecall do begin
      CharInfo.Code := ord( ch );
      CharInfo.Name := FontName;
      CharInfo.Count := Count;
      CharInfo.Charset := FontCharset;
    end;
    UpdateLastCommand( Cmd );
    if IsRecordingMacro then
       AddMacroEditCommand( Cmd );

    if Unicode then begin
       if ( Count = 1 ) then
         s := ch
       else
         s:= StringOfChar(ch, Count);
    end
    else begin
       if ( Count = 1 ) then
         s := AnsiCh
       else
         s:= StringOfChar(AnsiCh, Count);
    end;

    SelText := s;
    if ( FontName <> '' ) then begin
       CurrentFontName:= SelAttributes.Name;
       CurrentCharset:= SelAttributes.Charset;
       SelAttributes.Name := FontName;
       SelAttributes.Charset := FontCharset;
    end;
    SelStart := SelStart + Count;

    SelAttributes.Name := CurrentFontName;
    SelAttributes.Charset := CurrentCharset;

    Changed;

  except
  on E: Exception do
    App.ErrorPopup(E);
  end;

end; // InsertChar


class procedure TKntRichEdit.Form_CharsClosed( sender : TObject );
begin
  try
    try
      KeyOptions.InsCharAutoAddNew := Form_Chars.AutoAddNew;
      KeyOptions.InsCharCustom:= Form_Chars.RTFCustomChars;
      Form_Chars.Release;
    except
    end;

  finally
    Form_Chars := nil;
  end;
end; // Form_CharsClosed


//-------------------------------------------------------

procedure TKntRichEdit.ExpandTermProc;
var
  w, replw : string;
  SS: integer;

begin

    if not assigned(GlossaryList) then begin
      App.ShowInfoInStatusBar(sEdt21);
      exit;
    end;

    if CheckReadOnly then exit;

    UpdateLastCommand( ecExpandTerm );
    if IsRecordingMacro then
       AddMacroEditCommand( ecExpandTerm );


    SS:= -1;
    if ( SelLength = 0 ) then begin
      SS:= SelStart;
      w := GetWordAtCursor( true, false, true, true );       // SpacesAsWordDelim=True
    end
    else
      w := SelText;

    if ( length( w ) = 0 ) then begin
      App.ShowInfoInStatusBar(sEdt22);
      exit;
    end;

    replw := GlossaryList.Values[w];

    if (replw = '') and (SS > 0) then begin
       SelStart:= SS;
       w := GetWordAtCursor( true, false, true, false );    // SpacesAsWordDelim=False
       replw := GlossaryList.Values[w];
    end;


    if ( replw = '' ) then begin
      App.ShowInfoInStatusBar(sEdt23);
      exit;
    end;

    App.ShowInfoInStatusBar(Format( ' %s -> %s', [w,replw] ));
    replw := ExpandMetaChars( replw );

    SelText := replw;
    SelStart := SelStart + SelLength;

    Change;

end; // ExpandTermProc


procedure AddGlossaryTerm;
var
  Form_TermDef : TForm_TermDef;
  nstr, vstr : string;

begin
  if (not assigned( GlossaryList)) then begin
    showmessage( Format(sEdt24, [Glossary_FN] ));
    exit;
  end;

  nstr := '';
  vstr := '';

  if assigned(ActiveEditor) then begin
    if (ActiveEditor.SelLength > 0) then
       nstr := Trim(Copy(ActiveEditor.SelText, 1, 255 ))
    else
       nstr := ActiveEditor.GetWordAtCursor( true );
    if (nstr <> '') then
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
            if ( messagedlg( Format(sEdt25,
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
            Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format(sEdt26, [nstr,vstr] );

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


//--------------------------------------------

procedure TKntRichEdit.RunSpellchecker;
var
  AJBSpell : TAJBSpell;

begin
  if CheckReadOnly then exit;

  AJBSpell := TAJBSpell.Create( Form_Main );

  try
    try
      SelectAll;
      CopyToClipboard;
      if AJBSpell.CheckClipboardSpell then begin
        if ( messagedlg( sEdt27, mtConfirmation, [mbOK, mbCancel], 0 ) = mrOK ) then
            PasteFromClipboard;
      end;

    except
      on E : Exception do
         App.ErrorPopup(E);
    end;

  finally
    AJBSpell.Free;
  end;

end; // RunSpellchecker


//-------------------------------------------------

procedure TKntRichEdit.WordWebLookup;
var
  WordWeb : TFreeWordWeb;
  myWord, newWord : string;

begin
  if CheckReadOnly then exit;

  if ShiftDown then
    myWord := ''
  else begin
    if (SelLength > 0 ) then
      myWord := Trim(SelText)
    else
      myWord := GetWordAtCursor(true);
  end;

  if (myWord = '') then begin
    if (not InputQuery( sEdt32, sEdt33, myWord )) then
       exit;
  end;

  WordWeb := nil;
  try
    WordWeb := TFreeWordWeb.Create(Form_Main);
  except
    On E : Exception do begin
      App.ErrorPopup(E, sEdt34);
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
         SelText := newWord + #32;

    except
      On E : Exception do begin
        Form_Main.RTFMWordWeb.Enabled := false;
        Form_Main.TB_WordWeb.Enabled := false;
        App.ErrorPopup(E, sEdt34);
        exit;
      end;
    end;

  finally
    WordWeb.Free;
  end;

end; // WordWebLookup



Initialization
   ShowingSelectionInformation:= false;

end.

