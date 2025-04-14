unit knt.ui.editor;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2025 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

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
   Vcl.StdCtrls,
   Vcl.ExtDlgs,
   Vcl.ExtCtrls,
   Vcl.Menus,
   RxRichEd,
   VirtualTrees, VirtualTrees.Types, VirtualTrees.BaseTree, VirtualTrees.BaseAncestorVCL, VirtualTrees.AncestorVCL,
   kn_Info,
   kn_Const,
   knt.model.note,
   knt.ui.Selector
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
    FFoldingInScrapbook: boolean;
    FUnfolding: boolean;
    FCopying: boolean;

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
    FLinkHover: TCharRange;

    procedure CMDialogKey( var Message: TCMDialogKey ); message CM_DIALOGKEY;

  protected
    procedure CMRecreateWnd(var Message: TMessage); message CM_RECREATEWND;

    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUP(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure SelectionChange; override;

    procedure RxRTFProtectChangeEx(Sender: TObject; const Message: TMessage; StartPos, EndPos: Integer; var AllowChange: Boolean);
    procedure RxRTFURLClick(Sender: TObject; const URLText: string; Button: TMouseButton);
    procedure RxRTFURLMouseMove(Sender: TObject; WParam: WPARAM);
    procedure RxRTFStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure RxRTFEndDrag(Sender, Target: TObject; X, Y: Integer);

    procedure CheckWordCountWaiting;
    procedure CleanWordCount;

    function GetDoRegisterNewImages: boolean;

  public
    FloatingEditor: TObject;
    ParentEditor: TKntRichEdit;

    class constructor Create;
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;

    property FileObj: TObject read fFileObj;
    property FolderObj: TObject read fFolderObj;
    property NNodeObj: TObject read fNNodeObj;
    property NEntryObj: TObject read fNEntryObj;

    class property Copying: boolean read FCopying write FCopying;
    class property FoldingInScrapbook: boolean read FFoldingInScrapbook write FFoldingInScrapbook;

    property PlainText: boolean read FPlainText write FPlainText;
    property SupportsRegisteredImages: boolean read FSupportsRegisteredImages write FSupportsRegisteredImages;
    property SupportsImages: boolean read FSupportsImages write FSupportsImages;
    property DoRegisterNewImages: boolean read GetDoRegisterNewImages;
    property Chrome: TChrome read FChrome write FChrome;
    property ZoomGoal: integer read FZoomGoal;
    property ZoomCurrent: integer read FZoomCurrent;

    procedure SetVinculatedObjs(FileObj, FolderObj, NNodeObj, NEntryObj: TObject);
    function ContainsRegisteredImages: boolean;

    procedure SetLangOptions(AfterChangeRTL: boolean);

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
    function EnsureGetRtfSelText: AnsiString;

    procedure Fold (SelectedText: boolean);
    procedure Unfold;
    procedure PreviewFoldedBlock;
    procedure HideNestedFloatingEditor;
    procedure HidingFloatingEditor;
    function Focused: boolean; override;

    function GetZoom: integer;
    procedure SetZoom(ZoomValue : integer; ZoomString : string; Increment: integer= 0 );
    procedure RestoreZoomGoal;
    procedure RestoreZoomCurrent;
    function GetAndRememberCurrentZoom: integer;
    procedure SetMargins;

    procedure TryPasteRTF(HTMLText: AnsiString=''; FolderName: String= ''; PlainText: boolean = false);
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

    procedure CommitAddedTags;
    procedure CheckSelectingRegisteredTag(TypingBack: boolean = false);

  end; // TKntRichEdit


//=======================================

  function CreateAuxRichEdit: TAuxRichEdit;

  function RemoveKNTHiddenCharactersInText (const s: string; checkIfNeeded: boolean= true): string;
  function RemoveKNTHiddenCharactersInRTF  (const s: AnsiString; HiddenMarks: THiddenMarks;
                                            ReplaceWithStdBk: Boolean = False;
                                            NNodeGID: Cardinal= 0): AnsiString; overload;
  procedure RemoveKNTHiddenCharactersInRTF (const S: AnsiString; HiddenMarks: THiddenMarks;
                                            var RTFTextOut: AnsiString;
                                            var pOut: integer;
                                            ReplaceWithStdBk: Boolean = False;
                                            NNodeGID: Cardinal= 0;
                                            Offset: Integer = 1; ProcessUntilOffset: Integer = -1 ); overload;

  function GetHumanizedKNTHiddenCharacters (const s: string): string;

  function IsWordDelimiter(i: integer; const Str: string; const OnlySpaceAsWDelim: boolean): boolean; inline;
  function GetWordAtCursorFromRichEd(
                           RichEdit: TRxRichEdit;
                           const LeaveSelected : boolean;
                           const IgnoreActualSelection: boolean = False;
                           const DiscardKNTHiddenCharacters: boolean= True;
                           const SpacesAsWordDelim: boolean= False) : string;

  function IsATag(const Word: string): boolean;
  function IsAPossibleTag(const Word: string): boolean;
  function PrepareRTFtoBeFolded  (RTFIn: AnsiString; var RTFOut: AnsiString; Editor: TKntRichEdit;
                                  AddEndGenericBlock: Boolean = False; MinLenExtract: integer= 0): boolean;
  function PrepareRTFtoBeExpanded(RTFIn: AnsiString; var RTFOut: AnsiString; Editor: TKntRichEdit): boolean;
  function PositionInFoldedBlock(const TxtPlain: string; PosSS: integer; Editor: TRxRichEdit; var pBeginBlock, pEndBlock: integer): boolean;
  function OpenTagsConcatenated(PosTag, Lo: integer; const TxtPlain: string): boolean;
  function GetBlockAtPosition(const TxtPlain: string; PosSS: integer; Editor: TRxRichEdit;
                              var pBeginBlock, pEndBlock: integer; FoldedBlock: boolean;
                              const WordOpening: string; WordClosing: string;
                              WordOpening_Nested: string = '';
                              IsTag: boolean = false;
                              IgnoreFoldedBlocks: boolean = True;
                              ForFolding: boolean = False): boolean;
  function InsideOrPartiallySelectedProtectedBlock (Editor: TKntRichEdit): boolean;

  procedure Unfold (Editor: TRxRichEdit; TxtPlain: String; SS: integer);
  function RemoveFoldedBlock (Editor: TRxRichEdit; TxtPlain: String; SS: integer; OnlyIfTaggedFolded: boolean = false): integer;
  function IsTaggedFolded(pI, pF: integer; TextPlain: string): boolean;
  procedure RemoveTags(Editor: TRxRichEdit);

  procedure AddGlossaryTerm;
  procedure EditGlossaryTerms;

type
  TFoldingBlock = record
     Opening: String;
     Closing: String;
     CaseSensitive: boolean;
  end;

procedure LoadFoldingBlockInfo;
procedure SaveFoldingBlockInfo(LV: TListView);


const
   TagCharsDelimiters = [' ', ':', ',', #13, #9, '#'];
   TagCharsDelimWithoutHash = TagCharsDelimiters - ['#'];

var
  _LoadedRichEditVersion : Single;
   FoldBlocks: Array of TFoldingBlock;


implementation
uses
   AJBSpeller,
   FreeWordWeb,
   Parser,
   gf_misc,
   gf_strings,
   gf_miscvcl,
   gf_streams,
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
   kn_KntFolder,
   kn_Cmd,
   kn_StyleObj,
   kn_CharsNew,
   kn_Glossary,
   kn_ExpTermDef,
   kn_FindReplaceMng,
   knt.ui.tagSelector,
   knt.ui.TagMng,
   knt.ui.floatingEditor,
   knt.App,
   knt.RS
  ;



const
  WORD_MAX_LENGTH = 40;
  FOLDED_BLOCK_VISIBLE_EXTRACT_MAX_LENGTH = 500;


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

class constructor TKntRichEdit.Create;
begin
   FUnfolding:= False;
   FFoldingInScrapbook:= False;
   FCopying:= False;
end;


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
  OnURLMouseMove:= RxRTFURLMouseMove;

  {
   By default DragMode=dmManual, and the mechanism in TRxRichEdit is controlled through the IRichEditOleCallback interface.
   (see comment *3 in RxRichEd.pas)
   //OnDragOver := RxRTFDragOver;
   //DragMode :=  dmManual; // dmAutomatic;
  }
  OnStartDrag := RxRTFStartDrag;    // See comment *4 in RxRichEd
  OnEndDrag   := RxRTFEndDrag;        // ,,

  FloatingEditor:= nil;
  ParentEditor:= nil;
  FLinkHover.cpMin:= -1;

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


procedure TKntRichEdit.SetLangOptions(AfterChangeRTL: boolean);
var
  LangOpt: TRichLangOptions;
begin
   LangOpt:= [];
   if KeyOptions.IMEAutoFont then
      LangOpt:= [rlAutoFont];
   if AfterChangeRTL and KeyOptions.IMEAutoKeyboard then
      LangOpt:= LangOpt + [rlAutoKeyboard];

   LangOptions:= LangOpt;
end;


function TKntRichEdit.ContainsRegisteredImages: boolean;
var
  ImgIDs: TImageIDs;
begin
  ImgIDs:= ImageMng.GetImagesIDInstancesFromTextPlain (TextPlain);
  Result:= (ImgIDs <> nil);
end;


function TKntRichEdit.GetDoRegisterNewImages: boolean;
begin
   Result:= (NNodeObj <> nil) or ((ParentEditor <> nil) and ParentEditor.DoRegisterNewImages);
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


function RemoveKNTHiddenCharactersInRTF  (const s: AnsiString; HiddenMarks: THiddenMarks;
                                          ReplaceWithStdBk: Boolean = False;
                                          NNodeGID: Cardinal= 0): AnsiString;
var
  pOut: integer;
begin
   // Offset = 1 => pOut = 1
   RemoveKNTHiddenCharactersInRTF(S, HiddenMarks, Result, pOut, ReplaceWithStdBk, NNodeGID);
end;


procedure RemoveKNTHiddenCharactersInRTF(const S: AnsiString; HiddenMarks: THiddenMarks;
                                         var RTFTextOut: AnsiString;
                                         var pOut: integer;
                                         ReplaceWithStdBk: Boolean = False;
                                         NNodeGID: Cardinal= 0;
                                         Offset: Integer = 1; ProcessUntilOffset: Integer = -1 );
var
   pI, pPrefix, pF, len, p: integer;
   Prefix: AnsiString;
   RTFIn: PAnsiChar;
   pIn, pOut_Ini, NBytes: integer;
   LastCharIn: AnsiChar;

   bmkID:  AnsiString;
   bmkStd: AnsiString;
   ReplaceWith: AnsiString;
   HiddenCommand: AnsiChar;
   HiddenSubCommand: AnsiChar;
   HeaderText: AnsiString;
   CommandsJoined: boolean;    // I try to avoid something like the following, but just in case it happens:  \cf0\v\f0\fs16\'11D14\'12\'11FS+HLevel1\'12\cf2\v0\f1\fs24\par

const
   BMK_STD = '{\*\bkmkstart %d_%s%d}{\*\bkmkend %d_%s%d}';
   EmptyBraces = '{}';
   PrefixBmk = KNT_RTF_HIDDEN_MARK_L + KNT_RTF_HIDDEN_BOOKMARK;
   PrefixBmkPos = KNT_RTF_HIDDEN_MARK_L + KNT_RTF_HIDDEN_BMK_POSITION;

   PrefixData = KNT_RTF_HIDDEN_MARK_L + KNT_RTF_HIDDEN_DATA;


   procedure CheckCreateResult (N: Integer);
   begin
      if RTFTextOut = '' then begin
         SetLength(RTFTextOut, Length(S) + 8000);
         {$IF Defined(DEBUG) AND Defined(KNT_DEBUG)}
         ZeroMemory(@RTFTextOut[pOut], 1000);
         {$ENDIF}
      end
      else
         if (pOut + N) > Length(RTFTextOut) then begin
            SetLength(RTFTextOut, Length(RTFTextOut) + N + 4000);
            {$IF Defined(DEBUG) AND Defined(KNT_DEBUG)}
            ZeroMemory(@RTFTextOut[pOut], 1000);
            {$ENDIF}
         end;
   end;

   procedure RemoveReplace (p: Integer; const ReplaceWith: AnsiString);
   begin
      NBytes:= p - pIn;

      CheckCreateResult (NBytes + Length(ReplaceWith));
      Move(S[pIn], RTFTextOut[pOut], NBytes);
      inc(pOut, NBytes);
      if ReplaceWith <> '' then begin
         Move(ReplaceWith[1], RTFTextOut[pOut], Length(ReplaceWith));
         inc(pOut, Length(ReplaceWith));
      end;

      pIn:= p + Len;
   end;

   function GetStdBk(MarkID: integer): AnsiString;
   begin
      Result:= Format(BMK_STD, [NNodeGID, HiddenCommand, MarkID,   NNodeGID, HiddenCommand, MarkID]);
   end;


   function GetRTFCommands (const Str: AnsiString): AnsiString;
   var
      MarkID: integer;
      p, LenCmd, L: integer;
      HiddenSubCommand: AnsiChar;
      Param: AnsiString;
   begin
     // Str: "B36" or "I123" or
     // "FP" or "FHMy page header" or "FD+F+S+HMy page header" or ...

     {
      *1 Remember (RTF Specification)
      Section Break
      \sbknone	No section break.
      \sbkcol	Section break starts a new column.
      \sbkpage	Section break starts a new page (the default).
      \sbkeven	Section break starts at an even page.
      \sbkodd	Section break starts at an odd page.

     *2:
     \page doesn't work if it's between \v and v0, but \sect does
     }

      Result:= '';
      L:= Length(Str);

      case HiddenCommand of
        KNT_RTF_HIDDEN_BOOKMARK, KNT_RTF_HIDDEN_BMK_POSITION:
            begin
               MarkID:= StrToIntDef(Copy(Str, 2, L-1),  0);
               Result:= GetStdBk(MarkID);
            end;
        KNT_RTF_HIDDEN_DATA:
            begin
               HiddenCommand:= KNT_RTF_HIDDEN_BOOKMARK;
               Result:= GetStdBk(0);
            end;

        KNT_RTF_HIDDEN_FORMAT:
            begin
               p:= 2;
               Result:= '';
               repeat
                  HiddenSubCommand:= Str[p];
                  LenCmd:= 1;
                  case HiddenSubCommand of
                     KNT_RTF_HIDDEN_FORMAT_DIMENSIONS: Result:= Result + GetPageDimensionesInRTF;
                     KNT_RTF_HIDDEN_FORMAT_SECTION:    Result:= Result + '\sect ';    // See *1
                     KNT_RTF_HIDDEN_FORMAT_PAGE:       Result:= Result + '\sect ';    // See *2
                     KNT_RTF_HIDDEN_FORMAT_FOOTER:     Result:= Result + GetFooterInRTF;
                     KNT_RTF_HIDDEN_FORMAT_HEADER:     begin
                                                         Param:= Copy(Str, p+1, L-2);
                                                         Result:= Result + GetHeaderInRTF(Param);
                                                         inc(LenCmd, Length(Param));
                                                       end;
                     else
                        break;
                  end;
                  inc(p, LenCmd);
                  if (p < L) and (Str[p] = KNT_RTF_HIDDEN_FORMAT_PLUS) then
                     inc(p)
                  else
                     break;
               until p >= L;
            end;
      end;

      if Result = '' then
         Result := EmptyBraces;
   end;


   function ValidHiddenLength: boolean;
   begin
       Result:= (len <= KNT_RTF_HIDDEN_MAX_LENGHT) or (HiddenCommand = KNT_RTF_HIDDEN_FORMAT);
   end;

begin
  if S='' then Exit;

  //  {\rtf1\ansi {\v\'11B5\'12} XXX };   {\rtf1\ansi \v\'11B5\'12\v0 XXX};  {\rtf1\ansi \v\'11T999999\'12\v0 XXX};

  (* *1
     hello \v\'11B1\'12\v0\fs36 BIG WORLD  =>  hello \fs36 BIG WORLD
                                               hello {}\fs36 BIG WORLD
     hello \v\'11B1\'12\v0 world           =>  hello world      (-> remove space after \v0)
                                               hello {}world    (-> remove space after \v0)

    It is not easy to distinguish between these two possible cases. In the first case we should not remove the space after \v0
    and in the second case we should. To avoid problems it seems convenient to replace the text to be removed with "{}",
    always removing the space after \v0 :

    \pard\cf3\b\v\'11B12\'12\v0 hello world  => \pard\cf3\b hello world
                                                \pard\cf3\b{}hello world
    Compre\v\'11B8\'12\v0 ssion and        => Compres{}ssion and       --> RichEdit will eventually turn it into Compresssion and
  *)

  (*   *2
     We need to detect this situation, tags used for informational purposes, as part of the text.
     It would really be something completely unusual, but at least it is given in the KeyNote help document...
   <<... The use of these tags (e.g. "\v\'11I3\'12\v0") simplifies ...>>
   It is represented in RTF:
   <<... The use of these tags (e.g. "\cf3\\v\\'11I3\\'12\\v0\cf0 ") simplifies ...>>

   If we don't take this into account we can turn it into the following, which will truncate further
   processing of the generated RTF:
   <<... The use of these tags (e.g. "\cf7\\v\{}\\v0\cf0 ") simplifies the ...>>   BAD

  *)


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

  RTFIn:= PAnsiChar(@S[1]);

  if ProcessUntilOffset > 1 then begin
     LastCharIn:= S[ProcessUntilOffset];
     if LastCharIn <> #0 then
        RTFIn[ProcessUntilOffset-1] := #0;
  end
  else begin
     LastCharIn:= S[Length(S)];
     if LastCharIn <> #0 then
        RTFIn[Length(S)-1] := #0;
  end;


  CommandsJoined:= False;

  pIn:= Offset;

  if Offset = 1 then
     pOut:= 1;

  if (pOut > 1) and (RTFTextOut[pOut-1] = #0) then
     dec(pOut);
  pOut_Ini:= pOut;
  pI:= Offset;

  repeat
     if not CommandsJoined then
        pI:= PosPAnsiChar('\v\', RTFIn, pI);

     CommandsJoined:= False;
     ReplaceWith:= EmptyBraces;

     if pI > 0 then begin
        pPrefix:= PosPAnsiChar(PAnsiChar(Prefix), RTFIn, pI+2);
        if (pPrefix = 0) then break;
        if (pPrefix > 1) and (S[pPrefix-1] = '\') then begin    // See *2
           pI:= pPrefix+1;
           continue;
        end;

        p:= pPrefix + Length(KNT_RTF_HIDDEN_MARK_L);
        HiddenCommand:= S[p];

        pF:= PosPAnsiChar(KNT_RTF_HIDDEN_MARK_R + '\v0', RTFIn, pPrefix + Length(Prefix));
        len:= pF-pI + Length(KNT_RTF_HIDDEN_MARK_R + '\v0');
        if (pF > 0) and (pPrefix = pI + 2) and ValidHiddenLength then begin
           // Normal case: \v\'11B5\'12\v0 XXX
            if (S[pI + len] = ' ') then          // *1   (S[pI+Len]=RTFIn[pI + len-1])
               Inc(len);

            if ReplaceWithStdBk then
               ReplaceWith:= GetRTFCommands(Copy(S, p, pF-p));  // Parameter: "B5"

            RemoveReplace(pI, ReplaceWith);
        end
        else begin
           // Problematic case. Ex:
           //  \v\f1\fs40\'11B1\'12\v0 xxx                         --> \v\f1\fs40\v0 xxx                     ...> \v\f1\fs40 xxx
           //  \v\''11I1\''12\cf0\v0\f2\fs16\lang3082{\pict{...    --> \v\cf0\v0\f2\fs16\lang3082{\pict{...  ...> \cf0\f2\fs16\lang3082{\pict{...

            pF:= PosPAnsiChar(KNT_RTF_HIDDEN_MARK_R, RTFIn, pPrefix + Length(Prefix));        // Do not include \v0
            if (pF = 0) then break;

            len:= pF-pPrefix + Length(KNT_RTF_HIDDEN_MARK_R);
            if ValidHiddenLength then begin
               if ReplaceWithStdBk then
                  ReplaceWith:= GetRTFCommands(Copy(S, p, pF-p));  // Parameter: "B5" or "I123" or "FHMy Header" or ...

               RemoveReplace(pPrefix, ReplaceWith);
            end;
        end;

        if Copy(S, pF + Length(KNT_RTF_HIDDEN_MARK_R), Length(KNT_RTF_HIDDEN_MARK_L)) = KNT_RTF_HIDDEN_MARK_L then   // Ex:  \cf0\v\f0\fs16\'11D14\'12\'11FS+HLevel1\'12\cf2\v0\f1\fs24\par
           CommandsJoined:= True;
        pI:= pF + 1;
     end;

  until pI = 0;

  if (pOut = pOut_Ini) and (RTFTextOut = '') and (ProcessUntilOffset = -1) then begin
     RTFTextOut:= S;
  end
  else begin
     if ProcessUntilOffset > 1 then
        NBytes:= ProcessUntilOffset - pIn
     else
        NBytes:= Length(S) - pIn;
     CheckCreateResult(NBytes);
     Move(S[pIn], RTFTextOut[pOut], NBytes);
     inc(pOut, NBytes);

     if ProcessUntilOffset = -1 then
        SetLength(RTFTextOut, pOut-1);
  end;

  if LastCharIn <> #0 then begin
     if ProcessUntilOffset > 1 then
        RTFIn[ProcessUntilOffset-1] := LastCharIn
     else
        RTFIn[Length(S)-1] := LastCharIn;
  end;

end;


function GetHumanizedKNTHiddenCharacters (const s: string): string;
begin
  Result:= StringReplace(s,      KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_BOOKMARK,   '[BMK:', [rfReplaceAll]);
  Result:= StringReplace(Result, KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_Bookmark09, '[TmpBMK:', [rfReplaceAll]);
  Result:= StringReplace(Result, KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_IMAGE,      '[IMG:', [rfReplaceAll]);

  Result:= StringReplace(Result, KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_FORMAT,     '[FmtExp:', [rfReplaceAll]);

  Result:= StringReplace(Result, KNT_RTF_BEGIN_FOLDED_PREFIX_CHAR,  '[FOLD:', [rfReplaceAll]);
  Result:= StringReplace(Result, KNT_RTF_FOLDED_LINK_BLOCK_CHAR,    '[Fold:', [rfReplaceAll]);   // Nested fold
  Result:= StringReplace(Result, KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_LINK,               '[HiddLnk:', [rfReplaceAll]);

  Result:= StringReplace(Result, KNT_RTF_HIDDEN_MARK_R_CHAR,                             ']', [rfReplaceAll]);
  Result:= StringReplace(Result, KNT_RTF_HIDDEN_MARK_EndLink_CHAR,                       ']', [rfReplaceAll]);
end;



//----------------------


function RTFLinkToLinkFolded(const RTFLink: AnsiString; Offset: integer;
                             var PosRTFLinkEnd: integer; var RTFLinkFolded: AnsiString): boolean;
var
  p1,p2,p3: integer;
  URL, TextURL: AnsiString;
  L: integer;
begin

  Result:= false;
  try

  (*
	   {\field{\*\fldinst{HYPERLINK xxxx}}{\fldrslt{yyyy}}}  ===> {\'11Lxxxx@yyyy\'12}

	   Ex: {\field{\*\fldinst{HYPERLINK "img:22,33,44"}}{\fldrslt{\ul\cf1 ABC}}}
	        ===>
  		   {\'11L"img:22,33,44"@\ul\cf1 ABC\'12}

     LINK_PREFIX = '{\field{\*\fldinst{HYPERLINK ';
     KNT_RTF_FOLDED_LINK = {\'11L%s@%s\'12}
  *)

   p1:= Pos('}}', RTFLink, Offset + Length(LINK_PREFIX));
   URL:= Copy(RTFLink, Offset + Length(LINK_PREFIX), p1 - Offset - Length(LINK_PREFIX));

   p2:= Pos('\fldrslt{', RTFLink, p1);
   p3:= Pos('}}}', RTFLink, p2);
   L:= Length('\fldrslt{');
   TextURL:= Copy(RTFLink, p2 + L, p3-p2-L);

   p1:= Pos('}}}', RTFLink, p3);

   PosRTFLinkEnd:= p1+2;      // RTFLink[PosRTFImageEnd] = '}'

   RTFLinkFolded:= Format(KNT_RTF_FOLDED_LINK, [URL, TextURL]);

   Result:= true;

 except
 end;

end;


function RTFLinkFoldedToLink(const RTFLinkFolded: AnsiString;
                             Offset: integer; PosRTFLinkEnd: integer;
                             var RTFLink: AnsiString): boolean;
var
  p1,p2,p3: integer;
  URL, TextURL: AnsiString;
  L: integer;
begin

  Result:= false;
  try

  (*
     \'11Lxxxx@yyyy\'12      => 	   {\field{\*\fldinst{HYPERLINK xxxx}}{\fldrslt{yyyy}}}

	   Ex: \'11L"img:22,33,44"@\ul\cf1 ABC\'12  => {\field{\*\fldinst{HYPERLINK "img:22,33,44"}}{\fldrslt{\ul\cf1 ABC}}}

     KNT_RTF_FOLDED_LINK_PREFIX = \'11L

     PosRTFLinkEnd points to \'12
  *)

   p1:= Pos('@', RTFLinkFolded, Offset);
   URL:= Copy(RTFLinkFolded, Offset + Length(KNT_RTF_FOLDED_LINK_PREFIX), p1 - Offset - Length(KNT_RTF_FOLDED_LINK_PREFIX));

   p2:= Pos(KNT_RTF_HIDDEN_MARK_R, RTFLinkFolded, p1);
   p2:= PosRTFLinkEnd;
   TextURL:= Copy(RTFLinkFolded, p1 + 1, p2-p1-1);

   RTFLink:= Format(LINK_RTF_2, [URL, TextURL]);

   Result:= true;

 except
 end;

end;


procedure MarkEndVisibleExtract(RTFAux : TAuxRichEdit; TextPlain: String; OffsetVisibleExtract: Integer = 1; MinLenExtract: integer = 0);
var
  Len: integer;
  p: integer;
  Txt: string;

   function GetRestOfSentence(const Text: string; PosInsideSentence: integer): string;
   var
     p, pR_Scope, i: integer;
   begin
     { We have a position (PosInsideScope) located within a sentence. We have to locate
       the end of it and return it from that initial position
       .... [ ......  X ....... ] ....

        #9: TAB   #7:column separator in Tables

        KNT_RTF_HIDDEN_MARK_EndLink_CHAR = Chr(19);    // 19 (#$13): DC3 (Device Control 3)
        #$FFFC = Unicode Character (U+FFFC)  Object Replacement Character   https://www.compart.com/en/unicode/U+FFFC
     }

     pR_Scope:= Pos(#13, Text, PosInsideSentence) -1;
     if pR_Scope <= -1 then
        pR_Scope:= Text.Length;

     if pR_Scope <= MinLenExtract then
        Result:= Copy(Text,1, MinLenExtract)

     else begin
        p:= 1;
        for p := PosInsideSentence + 1 to pR_Scope-1 do
          if ((Text[p] = '.') and (Text[p+1] in [' ', #9])) or (Text[p] = #7)
                 or (Text[p] = KNT_RTF_HIDDEN_MARK_EndLink_CHAR)
                 or (Text[p] = #$FFFC) then
             break;

        if Text[p] = #$FFFC then
           dec(p);
        if p < pR_Scope then
           pR_Scope:= p;
        Result:= Copy(Text, PosInsideSentence, pR_Scope - PosInsideSentence + 1);
     end;
   end;

begin
  { If two spaces are found, they will be used as the end of the visible extract, otherwise the entire sentence will be used
   --or until the appearance of a link or an image  }

  Txt:= GetRestOfSentence(TextPlain, OffsetVisibleExtract);
  p:= pos('  ', Txt);               // End at the first double space we find
  if p > 0 then
     delete(Txt, p+2, Length(Txt));

  p:= pos('HYPERLINK', Txt);        // Do not include hyperlink texts, truncate at that point
  if p > 0 then
     delete(Txt, p-1, Length(Txt));

  p:= Length(Txt) + OffsetVisibleExtract -1;

  if Copy(Txt, Length(Txt)-Length(KNT_RTF_END_FOLDED_WITHOUT_v0_CHAR) + 1,
               Length(KNT_RTF_END_FOLDED_WITHOUT_v0_CHAR) ) = KNT_RTF_END_FOLDED_WITHOUT_v0_CHAR then
     dec(p, Length(KNT_RTF_END_FOLDED_WITHOUT_v0_CHAR));

  RTFAux.SelStart:= p;
  RTFAux.SelLength:= 0;
  RTFAux.SelText:= KNT_RTF_HIDDEN_MARK_AUX_CHAR;
end;


function PrepareRTFtoBeFolded(RTFIn: AnsiString; var RTFOut: AnsiString; Editor: TKntRichEdit; AddEndGenericBlock: Boolean = False; MinLenExtract: integer= 0): boolean;
var
   pI, pF, len, p, PosRTFLinkEnd: integer;
   pIn, pOut, NBytes: integer;
   RTFInWithImagesHidden: AnsiString;
   ReplaceWith: AnsiString;
   RTFAux : TAuxRichEdit;
   ImagesMode: TImagesMode;
   ReconsiderImageDimensionsGoalBAK: boolean;


   procedure CheckCreateResult (N: Integer);
   begin
      if RTFOut = '' then begin
         SetLength(RTFOut, Length(RTFIn) + 150);
         {$IF Defined(DEBUG) AND Defined(KNT_DEBUG)}
         ZeroMemory(@RTFOut[pOut], 150);
         {$ENDIF}
      end
      else
         if (pOut + N) > Length(RTFOut) then begin
            SetLength(RTFOut, Length(RTFOut) + N + 100);
            {$IF Defined(DEBUG) AND Defined(KNT_DEBUG)}
            ZeroMemory(@RTFOut[pOut], 100);
            {$ENDIF}
         end;
   end;

   procedure RemoveReplace (p: Integer; const ReplaceWith: AnsiString);
   begin
      NBytes:= p - pIn;

      CheckCreateResult (NBytes + Length(ReplaceWith));
      Move(RTFIn[pIn], RTFOut[pOut], NBytes);
      inc(pOut, NBytes);
      if ReplaceWith <> '' then begin
         Move(ReplaceWith[1], RTFOut[pOut], Length(ReplaceWith));
         inc(pOut, Length(ReplaceWith));
      end;

      pIn:= p + Len;
   end;


begin
 (*
   We want to get an output RTF text (RTFOut) that is the folding of the received RTF text (RTFIn). Folding
   involves hiding all the content of the text except for an initial extract that will remain visible.
   We must take several precautions:
       - RichText does not allow nested use of \v and \v0, nor to include hyperlinks within \v and \v0
         In our application the \v and \v0 controls are related to the use of hidden custom bookmarks, used
         as destination of links or as image identification, e.g. (Example: \v\'11B5\'12\v0)
         but also by the management of a folded block


      - RichText also does not allow to keep an image hidden. If we do for example:
         \v Image [IMAGEN] hidden \v0
         Lo cambiará a:
         \v Image\v0 [IMAGEN]\v hidden \v0

      - The text we are going to fold can contain other folded blocks, which will become nested folded blocks
        These blocks will have parts protected with \protect and \protect0

   * We need to hide any images by converting them to the hidden imLink format.
    * We need to remove the control marks \v and \v0, as well as the existing \protect and \protect0

     This removal is not easy without correctly parsing all the Rtf content. Therefore, the safest thing to do is to
     use a RichEdit control:
     * We will select all the text and unprotect it and make it visible.
         By doing so, our hidden bookmarks, with their 'special' characters, will be visible. Ex: \'11B5\'12
       but that is not a problem because in the end everything will be hidden (except the visible extract, where we will
       explicitly hide these internal bookmarks)

   * Add \v after the part that should be visible, as an extract.
     We will use a mark (#$14), which we will insert to indicate the final position of that visible extract.

   From this point we will be able to work directly with an RTF text string (dumped onto RTFIn from the RTFAux control):

	* We must transform hyperlinks into hidden bookmarks (although without \v and \v0)

	   {\field{\*\fldinst{HYPERLINK xxxx}}{\fldrslt{yyyy}}}  ===> {\'11Lxxxx@yyyy\'12}

	   Ex: {\field{\*\fldinst{HYPERLINK "img:22,33,44"}}{\fldrslt{\ul\cf1 ABC}}}
	        ===>
  		   {\'11L"img:22,33,44"@\ul\cf1 ABC\'12}


	* We must protect the entire text, adding at the beginning \protect\{link [collapsed]}
	    Ex: "En un lugar de la Mancha de cuyo nombre no quiero acordarme"
		  ->
		  Ex: \protect{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1 +}}}En un lugar de la Mancha de cuyo nombre no quiero acordarme

	* Add at the end, as a closing: "\v0 ...\'13\protect0"
     We are making use of another ANSI character that should not be at all normal to find in a text, #$13, as an aid to
     mark the end of folded blocks. (// 19 ($13): DC2 (Device Control 3) )


  ** Note: Folding is much easier than expanding.

  As an example of folding:
  (Two spaces are being used to mark the end of the visible excerpt and not using the complete sentence --or even the appearance of a link or an image--, which is the default behavior
   "muy difícil  \v de" )

    RTFIn = '{\rtf1\ansi
              En un lugar de la \v\''11B7\''12\v0 Mancha  de cuyo
              \protect{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1 +}}}-muy dificil  \v de \''11B5\''12recordar-\v0 ...\''13\protect0
              nombre no quiero \v\''11B6\''12\v0 acordarme}';

    RTFOut= '{\rtf1\ansi
              \protect{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1 +}}}
              En un lugar de la \v\''11B7\''12\v0 Mancha  \v de cuyo
              \''11L"fold:"@\ul\cf1 +\''12-muy dificil  de \''11B5\''12recordar-...\''13
              nombre no quiero \''11B6\''12acordarme\v0 ...\''13\protect0}';

*)

//    RTFIn:= '{\rtf1\ansi En un lugar de la \v\''11B7\''12\v0 Mancha  de cuyo ' +
//             '\protect{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1 +}}}muy difícil  \v de ' +
//             '\''11B5\''12recordar\v0 ...\''13\protect0 ' +
//             'nombre no quiero \v\''11B6\''12\v0 acordarme}';

  if RTFIn='' then Exit;


  ReconsiderImageDimensionsGoalBAK:= ImageMng.ReconsiderImageDimensionsGoal;
  try
     ImagesMode:= imLink;
     if (Editor <> nil) and not Editor.DoRegisterNewImages then begin
        ImageMng.ReconsiderImageDimensionsGoal:= True;               // To treat the Scrapbook editor as a special case
        ImagesMode:= imImage;
        TKntRichEdit.FoldingInScrapbook := True;
     end;

     RTFInWithImagesHidden:= ImageMng.ProcessImagesInRTF(RTFIn, '', ImagesMode, '', 0, false);
     if RTFInWithImagesHidden <> '' then begin
        RTFIn:= RTFInWithImagesHidden;
        RTFInWithImagesHidden:= '';
     end;

  finally
     TKntRichEdit.FoldingInScrapbook:= False;
     ImageMng.ReconsiderImageDimensionsGoal:= ReconsiderImageDimensionsGoalBAK;
  end;



  RTFAux:= CreateAuxRichEdit;
  try
     RTFAux.Clear;
     RTFAux.BeginUpdate;

     // It will add a \par at the end that we should ignore:
     // The end will be of the form: ...\par'#$D#$A'}'#$D#$A#0
     RTFAux.PutRtfText(RTFIn, true, false);

     if AddEndGenericBlock then begin
        RTFAux.SelStart:= RTFAux.TextLength;
        RTFAux.AddText(KNT_RTF_END_GENERIC_BLOCK);
     end;

     RTFAux.SelStart:= 0;
     RTFAux.SelLength:= FOLDED_BLOCK_VISIBLE_EXTRACT_MAX_LENGTH;
     MarkEndVisibleExtract(RTFAux, RTFAux.TextPlain(True), 1, MinLenExtract);       // We will insert #$14 to indicate the final position of that visible excerpt

     RTFAux.SelectAll;
     RTFAux.SelAttributes.Protected := True;
     RTFAux.SelAttributes.Hidden:= False;

     RTFIn:= RTFAux.RtfText;

  finally
     RTFAux.Free;
  end;


  // From this point on we do not need to use the RichEdit control, we can work with the RTFIn string

  pIn:= 1;
  pOut:= 1;
  RTFOut := '';

  pI:= Pos('\protect', RTFIn, 1);
  Len:= 0;                                                        // Add, without replacing
  RemoveReplace(pI + Length('\protect'), KNT_RTF_BEGIN_FOLDED);

  // We need to make sure that the hidden internal markers ($11...$12) remain hidden in the part we serve as a visible extract.
  // We use as help the #$14 mark we inserted from MarkEndVisibleExtract
  pI:= pI + Length('\protect');
  p:= Pos(KNT_RTF_HIDDEN_MARK_AUX, RTFIn, pI);

  ReplaceWith:= Copy(RTFIn, pI, p - pI);
  ReplaceWith:= StringReplace(ReplaceWith, KNT_RTF_HIDDEN_MARK_L, '\v' + KNT_RTF_HIDDEN_MARK_L, [rfReplaceAll]);
  ReplaceWith:= StringReplace(ReplaceWith, KNT_RTF_HIDDEN_MARK_R, KNT_RTF_HIDDEN_MARK_R +'\v0 ', [rfReplaceAll]);
  ReplaceWith:= ReplaceWith + '\v ';

  Len:= p - pI +  Length(KNT_RTF_HIDDEN_MARK_AUX);           // Length to be replaced
  RemoveReplace(pI, ReplaceWith);

  repeat
     pI:= Pos(LINK_PREFIX, RTFIn, pI);                      // LINK_PREFIX = '{\field{\*\fldinst{HYPERLINK ';

     if pI > 0 then begin
        if not RTFLinkToLinkFolded(RTFIn, pI, PosRTFLinkEnd, ReplaceWith) then
           exit;    // ToDO

        Len:= PosRTFLinkEnd - pI +1;
        RemoveReplace(pI, ReplaceWith);

        pI:= PosRTFLinkEnd + 1;
     end;
  until pI = 0;

  // Add \v0 ...\'13 before last }  (there will be no \protect0 because it is not necessary, since it applies to everything)
  // Remember: remove the extra line break that is being added in RTFAux.PutRtfTex
  // The end will be like this: ...\par'#$D#$A'}'#$D#$A#0
  pI := Lastpos( '}', RTFIn ) - Length('\par'+#$D#$A);
  Len:= Length('\par'+#$D#$A + '}');
  RemoveReplace(pI, KNT_RTF_END_FOLDED + '}');

  NBytes:= Length(RTFIn) - pIn;
  CheckCreateResult(NBytes);
  Move(RTFIn[pIn], RTFOut[pOut], NBytes);
  inc(pOut, NBytes);

  SetLength(RTFOut, pOut-1);
end;


// --------------------------------------

function PrepareRTFtoBeExpanded(RTFIn: AnsiString; var RTFOut: AnsiString; Editor: TKntRichEdit): boolean;

type
   TFoldedLink = record
     pI, pF: integer;
     FoldedBlock: boolean;
     pEndBlock: integer;
     FirstLevel: boolean;
     Visible: boolean;
   end;

var
   i, j, k: integer;
   pI, pF, pAux, pEnd, p, len : integer;
   pEnd_Last1stLevelBlock, pIplain_Last1stLevelBlock: integer;
   Num1stLevelBlocks: integer;
   pIn, pOut, NBytes: integer;
   OffsetC: integer;                 // Correction offset
   ReplaceWith: AnsiString;
   RTFAux : TAuxRichEdit;
   TextPlain: String;
   FoldedLinks: array of TFoldedLink;
   RTFOutWithProcessedImages: AnsiString;
   ImagesMode: TImagesMode;
   ReconsiderImageDimensionsGoalBAK: boolean;


   procedure CheckCreateResult (N: Integer);
   begin
      if RTFOut = '' then begin
         SetLength(RTFOut, Length(RTFIn) + 150);
         {$IF Defined(DEBUG) AND Defined(KNT_DEBUG)}
         ZeroMemory(@RTFOut[pOut], Length(RTFIn) + 150 -1);
         {$ENDIF}
      end
      else
         if (pOut + N) > Length(RTFOut) then begin
            SetLength(RTFOut, Length(RTFOut) + N + 100);
            {$IF Defined(DEBUG) AND Defined(KNT_DEBUG)}
            ZeroMemory(@RTFOut[pOut], N + 100 -1);
            {$ENDIF}
         end;
   end;

   procedure RemoveReplace (p: Integer; const ReplaceWith: AnsiString);
   begin
      NBytes:= p - pIn;

      CheckCreateResult (NBytes + Length(ReplaceWith));
      Move(RTFIn[pIn], RTFOut[pOut], NBytes);
      inc(pOut, NBytes);
      if ReplaceWith <> '' then begin
         Move(ReplaceWith[1], RTFOut[pOut], Length(ReplaceWith));
         inc(pOut, Length(ReplaceWith));
      end;

      pIn:= p + Len;
   end;

   procedure RemoveReplace_EnsureHiddenMarksNotVisible (pIni, pEnd: integer; LengthToBeReplaced: integer; const AddFinal: AnsiString = '');
   begin
     ReplaceWith:= Copy(RTFIn, pIni, pEnd - pIni);
     ReplaceWith:= StringReplace(ReplaceWith, KNT_RTF_HIDDEN_MARK_L, '\v' + KNT_RTF_HIDDEN_MARK_L, [rfReplaceAll]);
     ReplaceWith:= StringReplace(ReplaceWith, KNT_RTF_HIDDEN_MARK_R, KNT_RTF_HIDDEN_MARK_R +'\v0 ', [rfReplaceAll]);
     if AddFinal <> '' then
        ReplaceWith:= ReplaceWith + AddFinal;
     Len:= LengthToBeReplaced;
     RemoveReplace(pIni, ReplaceWith);
   end;

begin
 (*
   We want to cancel the adjustments made on the text to be folded

   Ex1:
   \protect{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1 +}}}En un lugar de la Mancha \v de cuyo nombre no quiero acordarme\v0 ...\'13\protect0
   -->
   En un lugar de la Mancha de cuyo nombre no quiero acordarme

   Ex2:
   \protect{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1 +}}}Extracto visible\v LALALA {\'11L"fold:"@\ul\cf1 +\'12}TEXTO ANIDADO BLABLABLA...\'13 LALALA2\v0 ...\'13\protect0
   -->
   Extracto visible LALALA \protect{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1 +}}}TEXTO ANIDADO\v BLABLABLA\v0 ...\'13\protect0 LALALA2

   In Ex1 there is no nested collapsed block, so the link and other hidden codes used to manage the collapsed text disappear.
   In Ex2 there was a nested collapsed block. In this case the link and other hidden codes used to manage the main collapsed text disappear, but the ones necessary
   for the first level nested block(s) we are dealing with appear.

   We must take into account that although we have been able to build the RTF text in this way, more or less cleanly, when the time comes to expand, the RTF may be modified.
   For example we could have
     \protect{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1 +}}}Extracto visible \v HOLA {\'11L"fold:"@\ul\cf1 +\'12}ANIDADO...\'13 ADIOS \v0 ...\'13\protect0

   And have become:
     {\protect\f1\lang1049{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1\cf1\ul +}}}}\protect\f1\fs20\lang1049 Extracto visible \v HOLA \'11L"fold:"@\cf1\ul +\'12\cf0\ulnone ANIDADO...\'13 ADIOS  \v0 ...\'13\protect0

   For this reason, it is convenient to perform certain initial actions from a RichEdit control:


     - Select all and unprotect and make all visible
        Ex: \protect{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1 +}}}Extracto visible \v LALALA....
             {\'11L"fold:"@\ul\cf1 +\'12}TEXTO ANIDADO BLABLABLA...\'13 LALALA2\v0 ...\'13\protect0

         If we look at it as Text (or TextPlain), what we have is:
         Ex: HYPERLINK "fold:"+Extracto visible LALALA $L"fold:"@+$TEXTO ANIDADO BLABLABLA...$LALALA2...$
         ($ will actually correspond to codes #$11, #$12 or #$13

     - Select the text corresponding to the initial link (up to the "+" included) and delete it:
       Ex: Extracto visible LALALA $L"fold:"@+$TEXTO ANIDADO BLABLABLA...$LALALA2...$

     - Remove end of collapsed block (...$)
       Ex: Extracto visible LALALA $L"fold:"@+$TEXTO ANIDADO BLABLABLA...$LALALA2


   And now, working with the text string (RtfText):

    - Locate the positions of all the folded links and identify which ones correspond to folded blocks and which of these
      are first level (they are not nested in another folded block)

        [ ] -> first level folded blocks ("fold:")
        ( ) -> nested folded blocks
        <>  -> "Normal" links, folded, but not blocks
        *   -> hidden marker. Example: \v\''11B7\''12\v0 ---- \''11B7\''12
        ... -> part that will remain visible.

                    pI pAux          pEnd          pI    pAux    pEnd
        |...*..<>..[...   (  <> ) *  ]....*.......[..<>..        ]...*.|

         pI   pF    pAux         pEnd
       [$11--$12    \v           \v0 ...$13]

       pI   pF
       <    >

       In FoldedLinks we will save for each folded block: pI, pF, pEnd, FirstLevel?
       And for each "normal" folded link: pI, pF

      * In this identification process, if we locate first-level folded blocks we will have to make their initial extracts
        visible, and to do so we will have to locate their final position. To do this we must use the plain text, not the
        RTF, so the way to not invalidate the positions recorded for each folded link and at the same time be able to mark
        the end of these visible extracts so that we can pre-empt them later is to insert a special character (#$14) that
        we will then ignore. Each time we insert one of these characters we will increase the positions already recorded for
        the folded links that are located after it, in the length occupied by that character in RTF mode (\'14). We will also
        increase the final position recorded for the first-level folded block itself in which we have placed the mark. Since
        this is a first-level block, there is no block processed before it that we must increase its positions, since all of
        them will be prior to the mark; if it were not so, this block that we are processing would not be first level..
        If we have had to insert these marks we will need to retrieve the RTF content again from the RichEdit control. From 
        this point on we will no longer need to access the RichText control, this last RTF text will be enough for us.


    - From this information the only thing that will be necessary to search, with Pos(...),

     - Replace all link markers with the corresponding LINK, taking into account:
        * In the case of folded blocks ("fold:"), only do this if they are first-level blocks.
        * If they are first-level folded blocks (to be converted into LINKs), add \protect before and \v after the visible extract.

           Extracto visible LALALA $L"fold:"@+$TEXTO ANIDADO BLABLABLA...$LALALA2
             ->
           Extracto visible LALALA \protect{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1 +}}}TEXTO ANIDADO\v BLABLABLA...\'13 LALALA2

      * Replace the string "...\'13" identified as the end of the folded block with '\v0 ...\'13\protect0'

             ....TEXTO ANIDADO\v BLABLABLA...\'13 LALALA2
              ->
             ....TEXTO ANIDADO\v BLABLABLA\v0 ...\'13\protect0 LALALA2

      * We need to make sure that hidden markers ($11...$12) stay hidden in the parts that become visible, so we need to add \v and \v0 to them
      -> Outside the first level collapsed blocks and in the visible excerpt of them

      * The text to be expanded may contain images in hidden mode (imLink) If the current mode is imImage we must
        make them visible, as long as they are not inside a nested block, not a first-level block

      - The text to be collapsed may contain other collapsed blocks, which will become nested collapsed blocks
        These blocks will have parts protected with \protect and \protect0

      * We need to remove the \v and \v0 control codes, as well as the existing \protect and \protect0
      * We need to hide possible images by converting them to the hidden internal format


    Example of conversion with a first-level collapsed block nested inside the one we are going to expand, and several hidden markers
    to consider, where some must be hidden again because they will be exposed in an excerpt that will now become visible. Even the
    first hidden marker in this example (\v\''11B7\''12\v0) will have to be hidden because the RTF string that will be used to perform
    the processing will be obtained after all the content has been unprotected and made visible, for the reason indicated above.


    (Two spaces are being used to mark the end of the visible excerpt in the nested collapsed block, rather than using
    the entire sentence --or even the appearance of a link or image--, which is the default behavior.
    "muy difícil  de")

    RTFIn:= '{\rtf1\ansi
              \protect{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1 +}}}
              En un lugar de la \v\''11B7\''12\v0 Mancha  \v de cuyo
              \''11L"fold:"@\ul\cf1 +\''12-muy difícil  de \''11B5\''12recordar-...\''13
              nombre no quiero \''11B6\''12acordarme\v0 ...\''13\protect0}';

    RTFOut= '{\rtf1\ansi
              En un lugar de la \v\''11B7\''12\v0 Mancha de cuyo
              \protect{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1 +}}}-muy difícil  \v de \''11B5\''12recordar-\v0 ...\''13\protect0
              nombre no quiero \v\''11B6\''12\v0 acordarme}';
*)


//  RTFIn:= '{\rtf1\ansi\protect{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1 +}}}' +
//           'En un lugar de la \v\''11B7\''12\v0 Mancha  \v de cuyo \''11L"fold:"@\ul\cf1 +\''12' +
//           '-muy dificil  de \''11B5\''12recordar-...\''13nombre no quiero \''11B6\''12acordarme\v0 ...\''13\protect0}';

  if RTFIn='' then Exit;


  RTFAux:= CreateAuxRichEdit;

  try
     RTFAux.Clear;
     RTFAux.BeginUpdate;

     RTFAux.PutRtfText(RTFIn, false, false, true);

     // First actions performed directly on the RichEdit control:
     RTFAux.SelectAll;
     RTFAux.SelAttributes.Protected := False;
     RTFAux.SelAttributes.Hidden:= False;

     RTFAux.SelStart:= 0;
     RTFAux.SelLength:= Length(KNT_RTF_BEGIN_FOLDED_PREFIX_CHAR);          // 18 ..
     RTFAux.SelText:= '';

     RTFAux.SelStart:= RTFAux.TextLength- Length(KNT_RTF_END_FOLDED_WITHOUT_v0_CHAR);  // 4 ..
     RTFAux.SelLength:= Length(KNT_RTF_END_FOLDED_WITHOUT_v0_CHAR);
     RTFAux.SelText:= '';


     // We retrieve the RTF content (RTFIn) and the equivalent plain text (TextPlain) and continue without needing to
     // interact with the RichEdit control
     RTFIn:= RTFAux.RtfText;
     TextPlain:= RTFAux.TextPlain;

     pIn:= 1;
     pOut:= 1;
     RTFOut := '';


    // Identify all folded links, as well as those corresponding to first-level folded blocks
     SetLength(FoldedLinks, 40);
     i:= 0;
     pI:= 1;
     repeat
        pI:= Pos(KNT_RTF_FOLDED_LINK_PREFIX, RTFIn, pI);
        if pI > 0 then begin
           FoldedLinks[i].pI:= pI;
           pF:= Pos(KNT_RTF_HIDDEN_MARK_R, RTFIn, pI);
           FoldedLinks[i].pF:= pF;                                                        // KNT_RTF_FOLDED_LINK_BLOCK_PREFIX = \''11L"fold:"@
           FoldedLinks[i].FoldedBlock:= (Copy(RTFIn, pI, Length(KNT_RTF_FOLDED_LINK_BLOCK_PREFIX)) = KNT_RTF_FOLDED_LINK_BLOCK_PREFIX);
           FoldedLinks[i].pEndBlock:= 0;
           FoldedLinks[i].Visible:= False;
           inc(i);
           inc(pI);
           if i > Length(FoldedLinks) then
              SetLength(FoldedLinks, Length(FoldedLinks) + 20);
        end;
     until pI = 0;
     SetLength(FoldedLinks, i);

    // Record for each folded block its corresponding closure
    // We are traversing the block closure strings ...\'13 from start to end, and the blocks cannot
    // intersect. Therefore, we will link the closure string to the first block we find, here traversing
    // from the last block to the first, where the closure position is after its start.
     for i:= 0 to Length(FoldedLinks)-1 do begin
        if not FoldedLinks[i].FoldedBlock then continue;
        pEnd:= Pos(KNT_RTF_END_FOLDED_WITHOUT_v0, RTFIn, FoldedLinks[i].pI);          // KNT_RTF_END_FOLDED_WITHOUT_v0 = ...\'13
        for j:= Length(FoldedLinks)-1 downto 0 do begin
            if not FoldedLinks[j].FoldedBlock then continue;
            if (pEnd > FoldedLinks[j].pF) and (FoldedLinks[j].pEndBlock = 0) then begin
               FoldedLinks[j].pEndBlock:= pEnd;
               break;
            end;
        end;
     end;

    // Mark the first-level folded blocks ("fold:") located and insert a mark (#$14) for each one
    // that points to the end of the visible excerpt
     pEnd_Last1stLevelBlock:= 0;
     pIplain_Last1stLevelBlock:= 0;
     Num1stLevelBlocks:= 0;

     for i:= 0 to Length(FoldedLinks)-1 do begin
        pI:= FoldedLinks[i].pI;
        if pI > pEnd_Last1stLevelBlock then
           FoldedLinks[i].Visible:= True;                    // It can be a 'normal' link, and visible, outside of any block (not considering the one we are expanding...)
        if not FoldedLinks[i].FoldedBlock then continue;

        if pI > pEnd_Last1stLevelBlock then begin            // If Visible
           pF:= FoldedLinks[i].pF;
           FoldedLinks[i].FirstLevel:= True;
           pEnd_Last1stLevelBlock:= FoldedLinks[i].pEndBlock;

           // Mark the final position of the visible extract ------------------
           // We are not going to update the TextPlain variable after each insertion of $14. Instead we will take into account
           // the offset when transferring the position of the start of the visible text to MarkEndVisibleExtract (with Num1stLevelBlocks)
           // (Offset of 1 byte in PlainText, and 4 in Rtf; the latter to be reflected in FoldedLinks)
           // In pIplain_Last1stLevelBlock we are saving the start position of the last processed first-level folded block
           // but referring to TextPlain, not to the RTF string, therefore the position where '$11L"fold:"@+#$12' is located
           pIplain_Last1stLevelBlock:= Pos(KNT_RTF_FOLDED_LINK_BLOCK_CHAR, TextPlain, pIplain_Last1stLevelBlock + 1);
           pF:= pIplain_Last1stLevelBlock + Length(KNT_RTF_FOLDED_LINK_BLOCK_CHAR) + Num1stLevelBlocks;  // pF -> Primer carácter del extracto

           // Insert character $14 -> \'14 (4 characters in RTF) KNT_RTF_HIDDEN_MARK_AUX = \'14
           // I can't save in FoldedLinks[i] the position of this marker ($14) because the insertion is based
           // on TextPlain and not on the RTF code, which is what we save in FoldedLinks and what we need for further processing
           MarkEndVisibleExtract(RTFAux, TextPlain, pF);

           // We need to shift the positions recorded in the following links by the length of the inserted mark,
           // since we will base ourselves on those positions later, using at the same time the RTF with the added marks #$14
           // This offset may need to be adjusted as the #$14 marks are processed (we'll use OffsetC )
           // This is because depending on where it falls, the modified RTF may be increased by 4 characters ("\'14") or less.
           // If the character is located right after a control command, the mark will be placed right after it,
           // eliminating the space that separates the command from the subsequent text. In this example the increment is 3:
           //    \highlight0 KeyNote NF\b0  is an evolution of Tranglos
           //    \highlight0\'14KeyNote NF\b0  is an evolution of Tranglos

           inc(FoldedLinks[i].pEndBlock, Length(KNT_RTF_HIDDEN_MARK_AUX));
           for j:= i + 1 to Length(FoldedLinks)-1 do begin
               inc(FoldedLinks[j].pI, Length(KNT_RTF_HIDDEN_MARK_AUX));
               inc(FoldedLinks[j].pF, Length(KNT_RTF_HIDDEN_MARK_AUX));
               inc(FoldedLinks[j].pEndBlock, Length(KNT_RTF_HIDDEN_MARK_AUX));
           end;

           inc(Num1stLevelBlocks);
           // ------------------

        end;
     end;

     if pEnd_Last1stLevelBlock <> 0 then
        RTFIn:= RTFAux.RtfText;              // Due to the displacement caused by the insertion of \'14

  finally
     RTFAux.Free;
  end;

  // From this point on we do not need to use the RichEdit control, we can work with the RTFIn string



   { Replace all link markers with the corresponding LINK, taking into account:
      * In the case of folding blocks ("fold:"), only do so if they are first-level blocks.
      * If they are first-level folded blocks, it is necessary to add other commands at the beginning and end of the block
        (\protect, \protecto) as well as treating the visible extract
      * We must ensure that the hidden markers ($11...$12) remain hidden in the parts that become visible,
   }

  OffsetC:= 0;

  // Position of the next hidden marker. If it does not match any of the identified ones, it is a non-link marker, 
  // and we must keep it hidden (by adding \v and \v0) if it is to be in a visible area
  p:= Pos(KNT_RTF_HIDDEN_MARK_L, RTFIn, 1);
  pEnd:= 0;
  for i:= 0 to Length(FoldedLinks) - 1 do begin
     if not FoldedLinks[i].Visible then continue;
     if FoldedLinks[i].FoldedBlock and not FoldedLinks[i].FirstLevel then continue;    // Redundant?

     pI:= FoldedLinks[i].pI + OffsetC;        // Start of a "normal" link or a first-level folded block (both visible, the second with hidden content)
     pF:= FoldedLinks[i].pF + OffsetC;
     pEnd:= FoldedLinks[i].pEndBlock + OffsetC;
     if not FoldedLinks[i].FoldedBlock then
       pEnd:= pF;

     if (p > 0) then begin
        if (p < pI) then begin
           // Let's grab all the text (that will be visible) between p and pI, and add
           // \v and \v0 to the hidden markers
(*        pIn                     p                              pI
          v                       v                              v
 RTFIn:= ' .... En un lugar de la \''11B7\''12Mancha  \v de cuyo \''11L"fold:"@\ul\cf1 +\''12-muy difícil

 RTFOut= ' .... En un lugar de la \v\''11B7\''12\v0 Mancha  de cuyo \protect{\field{\*\fldinst{HYPERLINK "fold:"}....
*)
           RemoveReplace_EnsureHiddenMarksNotVisible(p, pI, pI - p);

        end;
        p:= Pos(KNT_RTF_HIDDEN_MARK_L, RTFIn, pEnd + 1);
     end;


     if not RTFLinkFoldedToLink(RTFIn, pI, pF, ReplaceWith) then  // -> ReplaceWith: Hyperlink RTF "desplegado"
        exit;    // ToDO

     Len:= pF + Length(KNT_RTF_HIDDEN_MARK_R) - pI;    // Length to replace (Between \''11 and \''12 , inclusive)
     if not FoldedLinks[i].FoldedBlock then            // It is a 'normal' Link (not of type FoldedBlock)
     (*                   pI                                  pF
                          v                                   v
       RTFIn:= '....AAAAAA\''11L"http:midir.com"@\ul\cf1 MiDIR\''12 BBBBBBBB

       RTFOut= '....AAAAAA\{\field{\*\fldinst{HYPERLINK "http:midir.com"}}{\fldrslt{\ul\cf1 MiDIR}}}BBBBBBBBB
     *)
        RemoveReplace(pI, ReplaceWith)

     else begin                                  // It is a first level FoldedBlock

(*
                     pI                     pF   pF'           pAux                         pEnd
                     v                      v    v             v                            v
    RTFIn:= '....cuyo\''11L"fold:"@\ul\cf1 +\''12-muy difícil  \''14de \''11B5\''12recordar-...\''13
              nombre no quiero \''11B6\''12 acordarme\v0 ...\''13\protect0}';

    RTFOut= '....cuyo\protect{\field{\*\fldinst{HYPERLINK "fold:"}}{\fldrslt{\ul\cf1 +}}}-muy dificil  \v de \''11B5\''12recordar-\v0...\''13\protect0
              nombre no quiero \v\''11B6\''12\v0 acordarme}';
*)


        ReplaceWith:= '\protect' + ReplaceWith;
        RemoveReplace(pI, ReplaceWith);
        pAux:= Pos(KNT_RTF_HIDDEN_MARK_AUX, RTFIn, pF);   // $14. Mark used to identify the end of the visible extract...

        inc(pF, Length(KNT_RTF_HIDDEN_MARK_R));        // pF --> pF'
        RemoveReplace_EnsureHiddenMarksNotVisible(pF, pAux, pAux - pF + Length(KNT_RTF_HIDDEN_MARK_AUX), '\v ');

        Len:= Length(KNT_RTF_END_FOLDED_WITHOUT_v0);
        if Copy(RTFIn, pEnd-1, Len) = KNT_RTF_END_FOLDED_WITHOUT_v0 then
           dec(OffsetC, 1);
        RemoveReplace(pEnd + OffsetC, KNT_RTF_END_FOLDED + '\protect0 ');
     end;

  end;

  if p > pEnd then begin
     pEnd:= Length(RTFIn);
     RemoveReplace_EnsureHiddenMarksNotVisible(p, pEnd, pEnd-p);
  end
  else begin
     NBytes:= Length(RTFIn) - pIn;
     CheckCreateResult(NBytes);
     Move(RTFIn[pIn], RTFOut[pOut], NBytes);
     inc(pOut, NBytes);
  end;

  SetLength(RTFOut, pOut-1);

  // A \par will have been added at the end that we must remove:
  //  ...\par'#$D#$A'}'#$D#$A  --->  ...}
  ReplaceWith:= '}';
  RTFOut[pOut-Length('\par'+#$D#$A+'}'+#$D#$A)]:= '}';
  SetLength(RTFOut, pOut - Length('\par'+#$D#$A+'}'+#$D#$A));


  ReconsiderImageDimensionsGoalBAK:= ImageMng.ReconsiderImageDimensionsGoal;
  try
     ImagesMode:= ActiveFolder.ImagesMode;
     if (Editor <> nil) and not Editor.DoRegisterNewImages then begin
        ImageMng.ReconsiderImageDimensionsGoal:= True;               // To treat the Scrapbook editor as a special case
        ImagesMode:= imImage;
     end;

     RTFOutWithProcessedImages:= ImageMng.ProcessImagesInRTF(RTFOut, '', ImagesMode, '', 0, false);
     if RTFOutWithProcessedImages <> '' then
        RTFOut:= RTFOutWithProcessedImages;

  finally
     ImageMng.ReconsiderImageDimensionsGoal:= ReconsiderImageDimensionsGoalBAK;
  end;

end;


function OpenTagsConcatenated(PosTag, Lo: integer; const TxtPlain: string): boolean;
var
  i: integer;
  pF, pF2: integer;

begin
  { In situations like the following one, each these tags will be treated as tag in "open" mode, where
    its scope begin in the tag (suppose "#Tag1") and end at the position of ##Tag1 or ## or the end of the note

   Something ... #Tag1#Tag2#Tag3: bla, bla, bla
   ....

   #Tag1 #Tag2 #Tag3
   ....

   #Tag1,#Tag2, #Tag3:
   ....


   The following will not be considered as "open" tags, but normal tags applying to a paragraph, concatenated
   (note the absence of ":")
   Something ... #Tag1#Tag2#Tag3

   Something ... #Tag1 #Tag2, #Tag3


  }
 // PosSS: Initial position of a tag
 // Lo: Length of the tag, including the # character

  pF  := Pos(':', TxtPlain, PosTag + Lo);
  pF2 := Pos(#13, TxtPlain, PosTag + Lo);
  if (pF2 < pF) or (pF = 0) then begin
     pF:= pF2;
     i:= NFromLastCharPos(TxtPlain, #13, 1, PosTag);
     if i <= 0 then
        i:= 1;
  end
  else
     i:= PosTag + Lo;

  while i < pF do begin
     while (i < pF) and (TxtPlain[i] in TagCharsDelimWithoutHash) do
        inc(i);

     if (i < pF) and (TxtPlain[i] <> '#') then
        exit(false);
     inc(i);
     while (i < pF) and not (TxtPlain[i] in TagCharsDelimiters) do
        inc(i);
  end;

  Result:= True;
end;



{ ToDO: Nested open tag blocks
  #ToDO: ....
    ....
    #BUG:
    ....
    ...
    ##         (And/or ##BUG, and ## => close all previous Tags?)
  ....
  ...
  ##           (And/or ##ToDO)
}

// PosSS, pBeginBlock, pEndBlock: Begin at zero

function GetBlockAtPosition(const TxtPlain: string; PosSS: integer; Editor: TRxRichEdit;
                            var pBeginBlock, pEndBlock: integer; FoldedBlock: boolean;
                            const WordOpening: string; WordClosing: string;
                            WordOpening_Nested: string = '';
                            IsTag: boolean = false;
                            IgnoreFoldedBlocks: boolean = True;
                            ForFolding: boolean = False): boolean;
var
  p, pI, pF: integer;
  Lo, Lc: integer;
  pfI, pfF: integer;
  nEnd: integer;
  OffsetOpInCl: integer;
  IsProtected: boolean;
  IsValid: boolean;
  CheckWholeWords: boolean;
  ConsiderNestedBlocks: boolean;
  //IsOpenTag: boolean;           // A tag like #ToDO: (with ":" or immediately before $13), closed with ## (or ##ToDO) or the end of the note

  function WordIsValid (const Word: string; var p: integer): boolean;
  var
     pfI, pfF, L: integer;
  begin
     Result:= True;
     L:= Length(Word);
     if CheckWholeWords and
         ( ((p+L <= Length(TxtPlain)) and not IsWordDelimiter(p+L, TxtPlain, False) ) or
           ((p > 1) and not IsWordDelimiter(p-1, TxtPlain, False) )
                                                                     ) then
        Result:= False;

     if Result and (IgnoreFoldedBlocks and PositionInFoldedBlock(TxtPlain, p, Editor, pfI, pfF) ) then begin
        Result:= False;
        p:= pfF;                   // Saltar el bloque folded
     end;
  end;

  function CheckProtectedAtPos(X: integer): boolean;
  var
    SS, SL: integer;
  begin
      Result:= True;                                    // By default we will assume yes, and check if Editor is provided
      if Editor <> nil then begin
         Editor.BeginUpdate;
         SS:= Editor.SelStart;
         SL:= Editor.SelLength;

         Editor.SetSelection(X, X+1, False);
         IsProtected:= Editor.SelAttributes.Protected;

         Editor.SelStart:= SS;
         Editor.SelLength:= SL;
         Editor.EndUpdate;

         if not IsProtected then
            Result:= False;
      end;
  end;

begin
   Result:= False;

   if FoldedBlock and not CheckProtectedAtPos(PosSS) then exit;

   inc(PosSS);   // We are going to work using Pos(...), and strings whose characters start at 1..

   if IsTag then begin
      ConsiderNestedBlocks:= False;
      CheckWholeWords:= False;
   end
   else begin
      // If WordOpening = '' or is searching for a FoldedBlock -> CheckWholeWords=ConsiderNestedBlocks= False
      ConsiderNestedBlocks:= True;
      CheckWholeWords:= not FoldedBlock and (WordOpening <> '');

      if WordClosing = '' then begin
         ConsiderNestedBlocks:= False;
         if WordOpening = '' then
            WordClosing:= KNT_RTF_END_GENERIC_BLOCK;
      end;

      if ConsiderNestedBlocks and (WordOpening_Nested = '') then
         WordOpening_Nested:= WordOpening;

      // Is the opening token included in the closing token? Ex: "IF" / "END IF". Determine in which position
      OffsetOpInCl:= 0;
      if CheckWholeWords then
         OffsetOpInCl:= Pos(WordOpening, WordClosing, 1);
   end;

   Lo:= Length(WordOpening);

   pBeginBlock:= 0;
   pF:= 0;


   // Locate the start (as internal as possible in relation to the position) of the block,
   // ensuring, if necessary, that complete words are located, that the found word is not really
   // a closing word that includes an opening word, and ignoring folded blocks if so indicated.
   if IsTag then begin
      if OpenTagsConcatenated(PosSS, Lo, TxtPlain) then begin
         //IsOpenTag:= True;
         pBeginBlock:= PosSS;
         WordClosing:= KNT_RTF_END_TAG;     // '##'
      end
      else begin
         //IsOpenTag:= False;
         GetTextScope(TxtPlain, dsParagraph, PosSS, pI, pF, 0);
         pBeginBlock:= pI;
         pEndBlock:= pF - 1;
         Result:= True;
      end;
   end
   else
   if (WordOpening = '') or (Copy(TxtPlain, PosSS, Lo) = WordOpening) then
      pBeginBlock:= PosSS

   else begin
      pI:= 0;
      repeat
        IsValid:= True;
        pI:= Pos(WordOpening, TxtPlain, pI + 1);
        if (pI > 0) and not FoldedBlock then begin
           if ( (OffsetOpInCl > 0) and (pI-OffsetOpInCl >=1) and (Copy(TxtPlain, pI-OffsetOpInCl+1, Lc) = WordClosing) ) or
              ( not WordIsValid(WordOpening, pI) ) then
              IsValid:= False;
        end;
        if (pI > 0) and (pI <= PosSS) and IsValid then
           pBeginBlock:= pI;
      until (pI = 0) or (pI >= PosSS);
   end;


   Lc:= Length(WordClosing);


   // Locate the end of the block, taking into account, if necessary, the presence of nested blocks
   // If we have found a folded block when searching for the opening, we will have skipped the entire block. But we can
   // find another one while searching for the end.
   if (pBeginBlock > 0) and (pF <= 0) then begin
      pI:= pBeginBlock;
      pF:= pI;

      nEnd:= 1;
      repeat
        IsValid:= True;
        if WordClosing = '' then
           pF:= 0
        else begin
           pF:= Pos(WordClosing, TxtPlain, pF + Lc);
           if (pF > 0) and not FoldedBlock then
               if IsTag then begin                                               // => WordClosing = '##'
                  if ((pF = 1) or (TxtPlain[pF-1] <> '#')) and
                     ((pF + 2 > Length(TxtPlain)) or ((TxtPlain[pF+2] <> '#') and (TxtPlain[pF+2] in TagCharsDelimiters))) then
                      // IsValid
                  else
                  if ((Copy(TxtPlain, pF + 1, Lo) = WordOpening)) then       // Ex: Opening: #ToDO --> ##ToDO
                     Lc:= Lo + 1
                  else
                     IsValid:= False;
               end
               else
               if not WordIsValid(WordClosing, pF) then
                  IsValid:= False;
        end;

        if (pF > 0) and IsValid then begin
           dec(nEnd);
           if ConsiderNestedBlocks then begin
              repeat
                 IsValid:= True;
                 pI:= Pos(WordOpening_Nested, TxtPlain, pI + Lo);
                 if (pI > 0) and (pI < pF) and (not FoldedBlock) and
                       ( ((OffsetOpInCl > 0) and (Copy(TxtPlain, pI-OffsetOpInCl+1, Lc) = WordClosing)) or
                          not WordIsValid(WordOpening, pI) ) then
                    IsValid:= False;
              until IsValid or (pI = 0) or (pI > pF);
              if (pI > 0) and (pI < pF) then
                 inc(nEnd);                   // Found nested block
           end;
        end;
      until (pF = 0) or (nEnd=0);


      if pF > 0 then begin
         pEndBlock:= pF + Lc -1;     // pEndBlock points to the last character of the block (e.g. $13 in folded blocks) TxtPlain[pEndBlock] = #$13
         Result:= True;
      end
      else
      // If IsTag: IsOpenTag=True  -> WordClosing=KNT_RTF_END_TAG -> Find the end of the note
      //           IsOpenTag=False -> WordClosing=''              -> Find the end of the paragraph or note
      if (WordClosing = '') or (WordClosing = KNT_RTF_END_GENERIC_BLOCK) or (WordClosing = KNT_RTF_END_TAG)  then begin
          if (WordClosing <> KNT_RTF_END_TAG) then
             pF:= Pos(#13, TxtPlain, pBeginBlock + 1);
          if pF = 0 then
             pF:= Length(TxtPlain)
          else
             dec(pF);         // Excluding #13
          pEndBlock:= pF;

          if ForFolding and ( (pF - pBeginBlock) < 50) then begin
             p:= Pos(' ', TxtPlain, pBeginBlock);
             if (p >= pF) or ((pF - p) < 10) then
                pF:= 0;
          end;

          if (pF > 0) then
             Result:= True;
      end;

      if pEndBlock < PosSS then
         Result:= False;
   end;

   // Finally, express starting at 0
   dec(pBeginBlock);
   dec(pEndBlock);
end;


function PositionInFoldedBlock(const TxtPlain: string; PosSS: integer; Editor: TRxRichEdit; var pBeginBlock, pEndBlock: integer): boolean;
begin
   Result:= GetBlockAtPosition(TxtPlain, PosSS, Editor, pBeginBlock, pEndBlock, True,
                              KNT_RTF_BEGIN_FOLDED_PREFIX_CHAR, KNT_RTF_END_FOLDED_WITHOUT_v0_CHAR,
                              KNT_RTF_FOLDED_LINK_BLOCK_CHAR, False, False);
end;


function IsATag(const Word: string): boolean;
begin
  if (Word = '') or (Word[1] <> '#') then
     exit(False);
  Result:=  (ActiveFile.GetNTagByName(Copy(Word,2)) <> nil);
end;


function IsAPossibleTag(const Word: string): boolean;
begin
  Result:= False;
  if (Length(Word) >= 2) and (Word[1] = '#') and not (Word[2] in TagCharsDelimiters) then
     Result:= True;
end;


function GetClosingToken(const OpeningToken: string; var ClosingToken: string; var CaseSens: boolean; var IsTag: boolean; IgnoreTagCase: boolean = False): boolean;
var
   i: integer;
begin
   Result:= False;
   IsTag:= False;
   ClosingToken:= '';

   // Closing = Opening -> Does not allow nested blocks
   // Closing <> Opening -> Yes it allows nested blocks
   for i := 0 to Length(FoldBlocks) -1 do begin
     if (FoldBlocks[i].CaseSensitive and (OpeningToken = FoldBlocks[i].Opening)) or
        (not FoldBlocks[i].CaseSensitive and (OpeningToken.ToUpper = FoldBlocks[i].Opening.ToUpper)) then begin
        ClosingToken:= FoldBlocks[i].Closing;
        CaseSens:= FoldBlocks[i].CaseSensitive;
        Exit(True);
     end;
   end;

   if IsAPossibleTag(OpeningToken) then begin
      Result:= True;
      IsTag:= True;
      CaseSens:= False;
   end;

end;


procedure TKntRichEdit.Fold (SelectedText: boolean);
var
  RTFIn, RTFOut: AnsiString;
  SS, SL: integer;
  WordAtPos, ClosingWord: String;
  CaseSens, IsTag: boolean;
  TxtPlain: String;
  pI, pF, p: integer;
  AddEndGenericBlock: Boolean;
  MinLenExtract: integer;

  function InsidePossibleTag: boolean;
  var
     pI, pF, L: integer;
  begin
     // To allow folding text by Ctrl+Clicking on tags like #[x]
     // SS: Caret position
     // SL: Selection length

     Result:= False;
     if SS < 2 then exit;

     pI:= SS;
     repeat
        dec(pI);
     until (pI < 1) or (TxtPlain[pI] in TagCharsDelimiters);

     if (TxtPlain[pI] = '#') and ((pI <= 1) or (TxtPlain[pI-1] <> '#')) then begin
        Result:= True;

        L:= Length(TxtPlain);
        pF:= SS + SL + 1;
        while (pF < L) and not (TxtPlain[pF] in TagCharsDelimiters) do
           inc(pF);
        WordAtPos:= Copy(TxtPlain, pI, pF-pI);
        SS:= pI-1;
     end;
  end;


begin
   if CheckReadOnly or PlainText then exit;

   BeginUpdate;
   try
      SS:= SelStart;
      SL:= SelLength;

      AddEndGenericBlock:= False;
      TxtPlain:= Self.TextPlain;

      if SelectedText or (SL = 0) then begin
         if (SL > 0) and ((SS+SL <= Length(TxtPlain)) and (TxtPlain[SS+SL] = #13)) then begin
            SelLength:= SL-1;
            dec(SL);
         end;
         GetTextScope(TxtPlain, dsParagraph, SS+1, pI, pF, 0);
         dec(pI);
         dec(pF);

         if (SL > 0) then
         // Commented: Don't add automatically an EndGenericBlock "[.]" when the user folds a selected text
         // AddEndGenericBlock:= not ((SS = pI) and (SL = pF - pI) )         // -> KNT_RTF_END_GENERIC_BLOCK
         else begin
            SS:= pI;
            SL:= pF - pI;
         end;
      end;

      // Do not allow folding inside a folded block or partially selected
      SelLength:= 0;
      if SelAttributes.Protected then exit;
      SelStart:= SS + SL;
      if SelAttributes.Protected then exit;

      MinLenExtract:= 0;

      SelStart:= SS;
      SelLength:= SL;
      if not SelectedText then begin
         //WordAtCursor:= GetWordAtCursor(True,true);
         WordAtPos:= SelText.Trim;                    // If we access via Ctrl+DblClick it is enough, and it also allows us to select texts such as "**", "<>", etc.
         SS:= SelStart;
         if (SS >= 1) and (TxtPlain[SS] = '#') and ((SS <= 1) or (TxtPlain[SS-1] <> '#'))  then begin
            WordAtPos:= '#' + WordAtPos;
            dec(SS);
            SelStart:= SS;
         end
         else
         if InsidePossibleTag() then        // -> it will update WordAtPos and SS
            SelStart:= SS;


         if not GetClosingToken(WordAtPos, ClosingWord, CaseSens, IsTag) then begin
           // It is not a defined block opening word, nor a tag.
           // The initial position will be considered, and the final position will be the position of the following [.]
           // (which is not included in another block) and if not found, the end of the paragraph
            WordAtPos:= '';
         end
         else   // The token is recognized
            MinLenExtract:= Length(WordAtPos);

         if IsTag and (TxtPlain[SS+Length(WordAtPos)+1] = ':') then begin
            inc(MinLenExtract);
         end;

         if not CaseSens then begin
            TxtPlain:= TxtPlain.ToUpper;
            WordAtPos:= WordAtPos.ToUpper;
            ClosingWord:= ClosingWord.ToUpper;
         end;
         if not GetBlockAtPosition(TxtPlain, SS, Self, pI, pF, False, WordAtPos, ClosingWord, '', IsTag, True, True) then exit;

         SetSelection(pI, pF + 1, false);
      end;

      RTFIn:= EnsureGetRtfSelText;
      PrepareRTFtoBeFolded(RTFIn, RTFOut, Self, AddEndGenericBlock, MinLenExtract);
      RtfSelText:= RTFOut;

      sleep(100);         // If we don't do this, the initial word will remain selected.
      SelStart:= pI;
      SelLength:= 0;

   finally
      EndUpdate;
   end;

end;

procedure Unfold (Editor: TRxRichEdit; TxtPlain: String; SS: integer);
var
  RTFIn, RTFOut: AnsiString;
  pI, pF: integer;
begin
   with Editor do
      if PositionInFoldedBlock(TxtPlain, SS, Editor, pI, pF) then begin
         SetSelection(pI, pF+1, false);
         RTFIn:= RtfSelText;
         PrepareRTFtoBeExpanded(RTFIn, RTFOut, nil);
         RtfSelText:= RTFOut;
         SelStart:= pI;
      end;
end;

function RemoveFoldedBlock (Editor: TRxRichEdit; TxtPlain: String; SS: integer; OnlyIfTaggedFolded: boolean = false): integer;
var
  pI, pF: integer;
  i, L: integer;
begin
   with Editor do
      if PositionInFoldedBlock(TxtPlain, SS, Editor, pI, pF) then begin
         if OnlyIfTaggedFolded and not IsTaggedFolded(pI+1, pF, TxtPlain) then
            exit(pF);

         L:= Length(TxtPlain);
         inc(pF);                 // #$13  HIDDEN_MARK_EndLink
         while (pF < L) and (TxtPlain[pF + 1] in [#9, #32, #13]) do
            inc(pF);

         SetSelection(pI, pF, false);
         SelText:= '';
         exit(SS);
      end
      else
         exit(SS + 1);
end;


// "Tagged" folded: a folded block that begins with a tag
function IsTaggedFolded(pI, pF: integer; TextPlain: string): boolean;
var
   p,p1,p2: integer;
   pIcontent: integer;
   FirstWord: string;
begin
   Result:= False;

   pIcontent:= pI + Length(KNT_RTF_BEGIN_FOLDED_PREFIX_CHAR);
   p:= Integer.MaxValue;
   p1:= Pos(#13, TextPlain, pIcontent);
   if (p1 > 0) and (p1 < pF) then
      p:= p1;
   p2:= Pos(' ', TextPlain, pIcontent);
   if (p2 > 0) and (p2 < pF) and (p2 < p) then
      p:= p2;

   if p < Integer.MaxValue then begin
      FirstWord:= Copy(TextPlain, pIcontent, p-pIcontent);
      if (FirstWord <> '') and (FirstWord[Length(FirstWord)] = ':') then
         delete(FirstWord, Length(FirstWord), 1);

      if (FirstWord <> '') and IsAPossibleTag(FirstWord) then
         Result:= True;
   end;

end;

procedure RemoveTags(Editor: TRxRichEdit);
var
  TxtPlain: AnsiString;
  SS: integer;
  pF, L: integer;
begin
  Editor.BeginUpdate;
  SS:= 1;
  repeat
     TxtPlain:= Editor.TextPlain;
     L:= Length(TxtPlain);
     SS:= Pos('#', TxtPlain, SS);
     if (SS = 0) or (SS = L) then break;

     if (SS + 2 <= L) and (TxtPlain[SS + 1]='#') and (TxtPlain[SS + 2]='#') then begin
         inc(SS, 3);
         while (TxtPlain[SS] = '#') do
            inc(SS);
         continue;
     end
     else begin
        pF:= SS+1;
        if (TxtPlain[SS + 1]='#') then
           inc(pF);
        while (pF < L) and not (TxtPlain[pF] in TagCharsDelimiters) do
           inc(pF);

        if TxtPlain[pF] <> ':' then
           dec(pF);
        if ((pF < L) and (TxtPlain[pF+1] in [#13, ' '])) and ((SS = 1) or (TxtPlain[SS-1] = #13) ) then
           inc(pF);

        Editor.SetSelection(SS-1, pF, false);
        if Editor.SelLength = pF - (SS-1) then
           Editor.SelText:= ''
        else
           SS:= pF + 1;
     end;

  until false;

  Editor.EndUpdate;
end;



procedure TKntRichEdit.Unfold;
var
  RTFIn, RTFOut: AnsiString;
  SS: integer;
  pI, pF: integer;
begin
   if CheckReadOnly then exit;

   if PositionInFoldedBlock(Self.TextPlain, Self.SelStart, Self, pI, pF) then begin
      BeginUpdate;
      SetSelection(pI, pF+1, false);
      RTFIn:= EnsureGetRtfSelText;
      PrepareRTFtoBeExpanded(RTFIn, RTFOut, Self);
      FUnfolding:= True;
      RtfSelText:= RTFOut;
      FUnfolding:= False;
      SelStart:= pI;
      EndUpdate;
   end;
end;


function TKntRichEdit.EnsureGetRtfSelText: AnsiString;
begin
   Result:= RtfSelText;
   if (NNodeObj <> nil) and (Copy(Result, 1, 6 ) <> '{\rtf1') then begin
      TKntFolder(FolderObj).SaveEditorToDataModel;
      Result:= RtfSelText;
   end;
end;


procedure TKntRichEdit.PreviewFoldedBlock;
var
  RTFIn, RTFOut: AnsiString;
  SS: integer;
  pI, pF: integer;
  CursorPos: TPoint;
  FontHeight: integer;
  FE: TFloatingEditor;
begin
   if PositionInFoldedBlock(Self.TextPlain, Self.SelStart, Self, pI, pF) then begin
      BeginUpdate;
      SetSelection(pI, pF+1, false);
      FontHeight:= Round(Abs(SelAttributes.Height) * (ZoomCurrent/100));
      RTFIn:= EnsureGetRtfSelText;
      PrepareRTFtoBeExpanded(RTFIn, RTFOut, Self);
      SelStart:= pI;
      EndUpdate;

      CursorPos:= ActiveEditor.ClientToScreen(ActiveEditor.GetCharPos(pI));

      if FloatingEditor = nil then
         FloatingEditor:= TFloatingEditor.Create(Form_Main, Self);

      FE:= TFloatingEditor(FloatingEditor);
      FE.Editor.RtfText:= RTFOut;
      FE.Editor.Modified:= False;
      FE.Editor.ReadOnly:= Self.ReadOnly;
      FE.Editor.FZoomGoal:= FZoomGoal;
      FE.Editor.FZoomCurrent:= FZoomCurrent;
      TagMng.CreateTagSelector(TForm(FloatingEditor));

      Form_Main.ActiveControl:= nil;
      FE.ShowEditor(CursorPos.X, CursorPos.Y, FontHeight);
   end;
end;

procedure TKntRichEdit.HideNestedFloatingEditor;
begin
  if FloatingEditor <> nil then begin
     TFloatingEditor(FloatingEditor).HideEditor;
     TFloatingEditor(FloatingEditor).Free;
     FloatingEditor:= nil;
  end;
end;

procedure TKntRichEdit.HidingFloatingEditor;
var
  FE: TFloatingEditor;
  pI, pF: integer;
  RTFIn, RTFOut: AnsiString;
  FormParent: TForm;

begin
  if FloatingEditor <> nil then begin
     FormParent:= Form_Main;
     if ParentEditor = nil then
        FormParent:= Form_Main
     else
        FormParent:= TForm(Self.Parent);
     TagMng.CreateTagSelector(FormParent);

     FE:= TFloatingEditor(FloatingEditor);
     if not FE.Editor.Modified then exit;

     if PositionInFoldedBlock(Self.TextPlain, Self.SelStart, Self, pI, pF) then begin
        RTFIn:= FE.Editor.RtfText;
        BeginUpdate;
        SetSelection(pI, pF+1, false);
        PrepareRTFtoBeFolded(RTFIn, RTFOut, Self);
        RtfSelText:= RTFOut;
        SelStart:= pI;
        SelLength:= 0;
        EndUpdate;
     end;

  end;
end;


function TKntRichEdit.Focused: boolean;
begin
   Result:= inherited Focused or ((ParentEditor <> nil) and (ActiveFolder.FocusMemory = focRTF));
end;


function InsideOrPartiallySelectedProtectedBlock (Editor: TKntRichEdit): boolean;
var
   IsProtected: boolean;
   SS, SL: integer;

begin
   with Editor do begin
      SS:= SelStart;
      SL:= SelLength;

      IsProtected:= False;
      BeginUpdate;
      SelLength:= 0;
      IsProtected:= SelAttributes.Protected;
      if not IsProtected then begin
         SelStart:= SS + SL;
         IsProtected:= SelAttributes.Protected
      end;
      SelStart:= SS;
      SelLength:= SL;
      EndUpdate;
   end;

   Result:= IsProtected;
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
   ImageMng.DoNotRegisterNewImages:= not DoRegisterNewImages;
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
      ImageMng.DoNotRegisterNewImages:= not ActiveEditor.DoRegisterNewImages;
      RestoreZoomCurrent;
   end;
end;

//-----------------------------------------------------------


procedure TKntRichEdit.DoEnter;
begin
  HideNestedFloatingEditor;
  if FUpdating > 0 then exit;

  ImageMng.DoNotRegisterNewImages:= not DoRegisterNewImages;

  inherited;
end;


procedure TKntRichEdit.DoExit;
begin
  if IntroducingTagsState <> itNoTags then
     TagMng.CheckEndTagIntroduction;

  CommitAddedTags;
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

  if (IntroducingTagsState = itWithTagSelector) and (cTagSelector.EditorCtrlUI = Self) then
     if key in [VK_RETURN, VK_TAB] then
        Key:= 0
     else
     if (key in [VK_UP, VK_DOWN]) then begin
        if (PotentialNTags.Count > 1) then begin
           cTagSelector.SelectTag(Key);
           Key:= 0;
        end
        else begin
           if AnsiCompareText(TagSubstr, cTagSelector.SelectedTagName) <> 0 then
              Key:= 0;
           cTagSelector.CloseTagSelector(false);
        end;
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
          App.ShowInfoInStatusBar(GetRS(sEdt04));
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
          begin
             if FSupportsRegisteredImages then begin
                if SelLength = 0 then
                   Offset:= -1
                else
                   Offset:= 0;
                CheckToSelectLeftImageHiddenMark(Offset);
             end;
             if (IntroducingTagsState = itNoTags) then
                ActiveEditor.CheckSelectingRegisteredTag(True);
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
  SS, SL: integer;
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

  procedure ProccessKeyHash;
  var
    str: string;
  begin
     if cTagSelector.EditorCtrlUI = Self then
        TagMng.CheckEndTagIntroduction(true);

     // We are calling ProccessKeyHash having already inserted the # character, so SelStart already has the value
     // that we have been saving in CaretPosTag
     CaretPosTag:= SelStart;
     if (CaretPosTag = 1) or (GetTextRange(CaretPosTag-2, CaretPosTag-1)[1] <> '#') then begin
        IgnoreTagSubstr:= '';
        IgnoreSelectorForTagSubsr:= '';
        cTagSelector.EditorCtrlUI:= Self;
        IntroducingTagsState := itHashTyped;

        // In case we add a # to an existing word, which forms a registered tag:
        str:= GetTextRange(CaretPosTag, CaretPosTag+1);
        if (str='') or not (str[1] in TagCharsDelimiters) then begin
           str:= GetTextRange(CaretPosTag-1, CaretPosTag + TAG_MAX_LENGTH) + ' ';
           str:= TagMng.GetTagSubstr(str);
           TagSubstr:= str;
           TagMng.UpdateTagSelector;
        end;
     end;
  end;

   function GetEndOfSelectionNeededForTag: integer;
   var
      i: integer;
      Txt: string;
   begin
      txt:= ActiveEditor.GetTextRange(SS, SS + TAG_MAX_LENGTH) + ' ';
      for i:= 1 to Length(Txt) do
         if Txt[i] in TagCharsDelimiters then
            break;
      Result:= SS + i;

      if (Key <> Txt[i]) then
         dec(Result);
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


  if (IntroducingTagsState = itNoTags) or (cTagSelector.EditorCtrlUI = Self) then
     if (Key = '#') and (IntroducingTagsState <> itWithTagSelector) then begin
        AddText('#');
        Key:= #0;
        ProccessKeyHash;
     end
     else
        case IntroducingTagsState of

           itHashTyped:
              if not (key in TagCharsDelimiters) then begin
                 TagSubstr:= Key;
                 TagMng.UpdateTagSelector;
              end
              else
                 IntroducingTagsState := itNoTags;

            itWithTagSelector:
               if (Key in TagCharsDelimiters) or (Key = '.' ) then
               begin
                  TagSubstr:= cTagSelector.SelectedTagName;
                  if Key = #9  then Key:= #0;
                  if Key = #13 then Key:= ' ';

                  BeginUpdate;
                  SS:= SelStart;
                  SS:= GetEndOfSelectionNeededForTag;
                  SetSelection(CaretPosTag, SS, true);
                  SelText:= TagSubstr;
                  SelStart:= CaretPosTag + Length(TagSubstr);
                  if Key <> #0 then
                     AddText(Key);
                  EndUpdate;
                  case key of
                      #9: ;
                     '#': begin
                       TagMng.EndTagIntroduction;
                       ProccessKeyHash;
                     end;
                     '.': begin
                        TagSubstr:= TagSubstr + '.';
                        TagMng.UpdateTagSelector;
                     end
                     else
                        TagMng.EndTagIntroduction;
                  end;
                  Key:= #0;
                  exit;
               end;

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

                      SL:= RTFAux.TextLength;
                      if PlainText  then
                         Str:= RTFAux.Text + #13#10
                      else
                         Str:= RTFAux.RtfText;

                      PutRtfText(Str, True);

                      SelStart:= posBegin;                           // MoveSelectedLines=True => posBegin is initialized
                      SelLength:= SL + 1;
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
var
  pI, pF, SS: integer;
begin
  if FUnfolding or FCopying then
     AllowChange:= True

  else begin
     AllowChange := EditorOptions.EditProtected;

     SS:= EndPos;
     BeginUpdate;
     SetSelection(SS, SS+1, False);
     if not SelAttributes.Protected and (SS > 0) then
        dec(SS);                                     // Cursor can be just after the last character of the protected block, and press BACKSPACE..
     if StartPos <> EndPos then begin
        SetSelection(StartPos, StartPos+1, False);
        if SelAttributes.Protected then
           SS:= StartPos;
     end;
     SetSelection(StartPos, EndPos, False);
     EndUpdate;
     if PositionInFoldedBlock(Self.TextPlain, SS, Self, pI, pF) then
        AllowChange:= False;
  end;
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
  if (IntroducingTagsState <> itNoTags) and (cTagSelector.EditorCtrlUI = Self) then begin
     if (Self.SelLength > 0) then
        cTagSelector.CloseTagSelector(false)
     else
        TagMng.CheckEndTagIntroduction;
  end
  else
  if IgnoreSelectorForTagSubsr = ' ' then
     IgnoreSelectorForTagSubsr := '';

  if FLinkHover.cpMin <> -1 then begin
     App.ShowInfoInStatusBar('');
     FLinkHover.cpMin:= -1;
  end;

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
  if (IntroducingTagsState = itWithTagSelector) then begin
     IgnoreSelectorForTagSubsr := cTagSelector.SelectedTagName;
     cTagSelector.CloseTagSelector(false);
  end;

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
         end
         else
            if SL = 2 then
               ImgID:= CheckToIdentifyImageID(p);
      end;
   end;

   if (ImgID = 0) and FSupportsImages and (not FSupportsRegisteredImages or not assigned(NNodeObj) ) then begin
      txt:= RtfSelText;
      if length(txt) > 0 then
         ImageMng.TryRTFPictToImage (@txt[1], Length(txt), Img);
   end;

   if (ImgID <> 0) or (Img <> nil) then
      ImageMng.OpenImageViewer(ImgID, CtrlDown, false, Img)

   else
   if CtrlDown then
      Fold (false);

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


procedure TKntRichEdit.RxRTFURLMouseMove(Sender: TObject; WParam: WPARAM);
begin
  if (FLinkHover.cpMin = LinkClickRange.cpMin) then exit;
  App.ShowInfoInStatusBar(GetTextRange(LinkClickRange.cpMin, LinkClickRange.cpMax));
  FLinkHover:= LinkClickRange;
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

procedure TKntRichEdit.TryPasteRTF(HTMLText: AnsiString=''; FolderName: String= ''; PlainText: boolean = false);
var
  RTF: AnsiString;
  posI: integer;

begin
    posI := SelStart;

    RTF:= Clipboard.TryOfferRTF(HTMLText, SelAttributes, PlainText);            // Can return '' if clipboard already contained RTF Format
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

       PasteIRichEditOLE(CF_UNICODETEXT);

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

           TryPasteRTF(HTMLClip, FolderName, True);
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

  SendMessage( Handle, EM_GETZOOM, WPARAM(@w), LPARAM(@l) );
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
          App.ErrorPopup(E, GetRS(sEdt01));
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
          App.WordCountInfoInStatusBar:= Format( GetRS(sEdt02), [succ( p.y ), Lines.Count, succ(X)] );
       end
       else begin
          LastPy:= -1;
          Str:= SelVisibleText;
          App.WordCountInfoInStatusBar:= Format( GetRS(sEdt03), [Length(Str), GetWordCount(Str)] );
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

    perform(ScrollMsg, MakeWParam(SBVal, GetScrollPos(handle, ScrollVal)), 0);
    perform(ScrollMsg, MakeWParam(SB_ENDSCROLL, GetScrollPos(handle, ScrollVal)), 0);

end; // Navigate


function TKntRichEdit.GetStatistics (var numChars, numAlpChars, numWords: integer): string;
var
  s, title : string;
  lista : TStringList;
  i, l, len, numSpaces, numLines: integer;
  WasAlpha : boolean;
  ch : char;

begin

  App.ShowInfoInStatusBar(GetRS(sEdt28));
  screen.Cursor := crHourGlass;
  lista := TStringList.Create;

  try
     if (SelLength > 0) then begin
       lista.Text := SelText;
       title := GetRS(sEdt29);
     end
     else begin
       lista.Text := Lines.Text;
       title := GetRS(sEdt30);
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

  s := format(GetRS(sEdt31),
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


function IsWordDelimiter(i: integer; const Str: string; const OnlySpaceAsWDelim: boolean): boolean; inline;
begin
 Result:= (Str[i]<>KNT_RTF_HIDDEN_MARK_L_CHAR) and (Str[i]<>KNT_RTF_HIDDEN_MARK_R_CHAR)
            and (   (OnlySpaceAsWDelim and (Str[i] in [#32,#9,#13,#10]))
                 or (not OnlySpaceAsWDelim and not IsCharAlphaNumeric(Str[i])) );
end;

function GetWordAtCursorFromRichEd(
                          RichEdit: TRxRichEdit;
                          const LeaveSelected : boolean; const IgnoreActualSelection: boolean = False;
                          const DiscardKNTHiddenCharacters: boolean= True;
                          const SpacesAsWordDelim: boolean= False
                          ) : string;
var
  L, R,  Rm, Offset: integer;
  SS, SL: integer;
  SSw: integer;
  Str: string;
  KeepSelected: boolean;

begin
 with RichEdit do begin
  SL:= SelLength;
  if (SL > 0) and not IgnoreActualSelection then Exit(SelText);

  SS:=  SelStart;
  L:= SS-WORD_MAX_LENGTH;
  if L < 0 then L:= 0;
  R:= SS + WORD_MAX_LENGTH;


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
 end;

end;   // GetWordAtCursorFromRichEd


function TKntRichEdit.GetWordAtCursor( const LeaveSelected : boolean; const IgnoreActualSelection: boolean = False;
                                          const DiscardKNTHiddenCharacters: boolean= True;
                                          const SpacesAsWordDelim: boolean= False
                                           ) : string;
begin
    Result:= GetWordAtCursorFromRichEd(Self, LeaveSelected, IgnoreActualSelection, DiscardKNTHiddenCharacters, SpacesAsWordDelim);
end;



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
    InputQuery(GetRS(sEdt05), GetRS(sEdt06), s)
  else
     s:= KeepOnlyLeadingKNTHiddenCharacters(s);

  if (s = '') then exit;

  try
    s := trim( s );
    i := strtoint( s );
    s := DecToRoman( i );
  except
    App.ErrorPopup(Format(GetRS(sEdt07), [s]));
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
     InputQuery( GetRS(sEdt08), GetRS(sEdt09), s )
  else
     s:= KeepOnlyLeadingKNTHiddenCharacters(s);

  if (s = '') then exit;

  try
    s := AnsiUpperCase(trim(s));
    i := RomanToDec(s);
  except
    App.ErrorPopup(Format(GetRS(sEdt10), [s]));
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
      App.ShowInfoInStatusBar(GetRS(sEdt11));
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
    App.ShowInfoInStatusBar(GetRS(sEdt12));
    SelStart := Perform( EM_LINEINDEX, p.y, 0 );
    SelStart := SelStart + pred( p.x );
    Perform( EM_SCROLLCARET, 0, 0 );
    SelLength := 1;
  end
  else
    App.ShowInfoInStatusBar(GetRS(sEdt13));

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
    if messagedlg(GetRS(sEdt14), mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes then exit;
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
     if ( messagedlg(GetRS(sEdt15), mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then
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
       App.ShowInfoInStatusBar(GetRS(sEdt16) + LastEvalExprResult);
       Form_Main.MMEditPasteEval.Hint := GetRS(sEdt17) + LastEvalExprResult;

       if KeyOptions.AutoPasteEval and ( not Self.ReadOnly) then begin
          SelStart := SelStart + SelLength;
          SelText := #32 + LastEvalExprResult;
       end
       else
          if ( messagedlg( Format( GetRS(sEdt18), [src,LastEvalExprResult] ), mtInformation, [mbOK,mbCancel], 0 ) = mrOK ) then begin
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
          Title:= GetRS(sEdt19);
          Filter:= GetRS(sEdt20) + FILTER_IMAGES;

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
      App.ShowInfoInStatusBar(GetRS(sEdt21));
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
      App.ShowInfoInStatusBar(GetRS(sEdt22));
      exit;
    end;

    replw := GlossaryList.Values[w];

    if (replw = '') and (SS > 0) then begin
       SelStart:= SS;
       w := GetWordAtCursor( true, false, true, false );    // SpacesAsWordDelim=False
       replw := GlossaryList.Values[w];
    end;


    if ( replw = '' ) then begin
      App.ShowInfoInStatusBar(GetRS(sEdt23));
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
    showmessage( Format(GetRS(sEdt24), [Glossary_FN] ));
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
            if ( messagedlg( Format(GetRS(sEdt25),
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
            Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format(GetRS(sEdt26), [nstr,vstr] );

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

procedure LoadFoldingBlockInfo;
var
   i, p1,p2: integer;
   str: string;
   SL: TStringList;
begin
    try
       if FileExists( FoldingBlock_FN) then begin
          SL := TStringList.Create;
          SL.LoadFromFile (FoldingBlock_FN);
          SetLength(FoldBlocks, SL.Count);
          for i := 0 to SL.Count -1 do begin
             Str:= SL[i];
             p1:= Pos(',', Str, 1);
             p2:= Pos(',', Str, p1+1);
             if (p1 > 0) and (p2 > 0) then begin
                FoldBlocks[i].Opening:= Trim(Copy(Str,1,p1-1));
                FoldBlocks[i].Closing:= Trim(Copy(Str,p1+1,p2-1-p1));
                FoldBlocks[i].CaseSensitive := (Trim(Copy(Str,p2+1)))[1]='1';
             end;
          end;
          SL.Free;
       end;

    except
      On E : Exception do begin
        ShowMessage( GetRS(sFoldBl4) + E.Message );
      end;
    end;
end;


procedure SaveFoldingBlockInfo(LV: TListView);
var
  i : integer;
  SL : TStringList;
  Item : TListItem;
  Opening, Closing, CaseSens: String;
begin
   try
     SL := TStringList.Create;
     try
       SetLength(FoldBlocks, LV.Items.Count);
       for i := 0 to LV.Items.Count -1 do begin
          Item := LV.Items[i];
          Opening:= Item.Caption;
          Closing:= Item.Subitems[0];
          CaseSens:= Item.Subitems[1];
          FoldBlocks[i].Opening:= Opening;
          FoldBlocks[i].Closing:= Closing;
          FoldBlocks[i].CaseSensitive:= (CaseSens = 'x');
          SL.Add(Opening + ', ' + Closing + ', ' + BOOLEANSTR[FoldBlocks[i].CaseSensitive]);
       end;
       SL.SaveToFile( FoldingBlock_FN, TEncoding.UTF8);

     finally
       SL.Free;
     end;

 except
   on E : Exception do
      messagedlg( GetRS(sFoldBl3) + E.Message, mtError, [mbOK], 0 );
 end;
end;

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
        if ( messagedlg( GetRS(sEdt27), mtConfirmation, [mbOK, mbCancel], 0 ) = mrOK ) then
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
    if (not InputQuery( GetRS(sEdt32), GetRS(sEdt33), myWord )) then
       exit;
  end;

  WordWeb := nil;
  try
    WordWeb := TFreeWordWeb.Create(Form_Main);
  except
    On E : Exception do begin
      App.ErrorPopup(E, GetRS(sEdt34));
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
        App.ErrorPopup(E, GetRS(sEdt34));
        exit;
      end;
    end;

  finally
    WordWeb.Free;
  end;

end; // WordWebLookup


procedure TKntRichEdit.CheckSelectingRegisteredTag(TypingBack: boolean = false);
var
  Str: string;
  i: integer;
  SS, p, CaretPos: integer;
  MaxLength: integer;

begin
    // Note: Only called if IntroducingTagsState = itNoTags

    if IgnoreSelectorForTagSubsr <> '' then exit;

    CaretPosTag:= -1;
    SS:= ActiveEditor.SelStart;
    if TypingBack and (SS > 0) then
       dec(SS);
    Str:= ActiveEditor.GetTextRange(SS, SS+1);
    if (Str = '#') and
      ((SS >= ActiveEditor.TextLength-1) or (ActiveEditor.GetTextRange(SS, SS+1)[1] <> '#' )) then
       CaretPosTag:= SS

    else begin
       MaxLength:= TAG_MAX_LENGTH;
       if SS < TAG_MAX_LENGTH then
          MaxLength:= SS;

       Str:= ActiveEditor.GetTextRange(SS-MaxLength, SS);
       CaretPos:= SS-1;
       for i:= MaxLength downto 1 do begin
          if Str[i] = '#' then begin
             CaretPosTag:= CaretPos;
             break;
          end
          else
          if Str[i] in TagCharsDelimiters then
             break;
          dec(CaretPos);
       end;
    end;

    { *2
      ** AutoDiscoverTags: Currently not implemented. It will be assumed as AutoDiscoverTags = False
      If we place the cursor inside or next to an unregistered tag (not visible in the global list of tags)
      and the AutoDiscoverTags option is not enabled, that is, there may be texts recognizable as tags that we do not
      want to register (or it is simply seen as unnecessary to have to search for these possible tags when saving
      each note to the model) we will still activate the selector and the detection of possible new tags, but we will
      indicate (on "IgnoreTagSubstr") that this tag name is to be ignored.
      In this way we will allow adding new tags based on the modification of that tag.
      For example, if "#SecretTag" is not registered and we place ourselves on that tag, we can delete "Secret" and
      write "Normal" ("#NormalTag") instead and the resulting tag will be registered. Or we can even delete everything
      except the # character and type anything: we will see that it tries to recognize it as a tag.
      This seems to be a more intuitive behavior than only recognizing tags if the # character is pressed.
      On the other hand, this CheckSelectingRegisteredTag method is executed only after a minimum moment of inactivity,
      seeking not to penalize performance, avoiding doing the checks every time the selection changes, and also seeking
      not to bother by showing the selector when we simply scroll quickly through a tag.
      (It is also executed in response to the VB_BACK key press, from TKntRichEdit.KeyDown)
    }
    if CaretPosTag >= 0 then begin
       TagSubstr:= TagMng.GetTagSubstr(ActiveEditor.GetTextRange(CaretPosTag, CaretPosTag + TAG_MAX_LENGTH) + ' ');
       if TagSubstr <> '' then begin
          IgnoreTagSubstr:= '';                                                                           // *2
          if {(not KeyOptions.AutoDiscoverTags) and}
             (IntroducingTagsState <> itHashTyped) and (ActiveFile.GetNTagByName(TagSubstr) = nil) then
             IgnoreTagSubstr:= TagSubstr;

          inc(CaretPosTag);  // CaretPosTag:                v
                             //               #TagX --> | # | T | a | ....
          cTagSelector.EditorCtrlUI:= Self;
          TagMng.UpdateTagSelector;
          exit;
       end;
    end;

    IgnoreSelectorForTagSubsr:= ' ';
end;


// Go through the temporarily added tags, removing those that may have been added by mistake.
// This method does not search for all possible tags present, which may have been added not only
// with the help of the selector but in other ways, such as copying and pasting.
// That more exhaustive search will only be performed when saving the note
procedure TKntRichEdit.CommitAddedTags;
var
   i, p: integer;
   Txt: string;
   NTag: TNoteTag;
   N: integer;
   NextConfirmedID: Cardinal;
   TagsStateBAK: TTagsState;

begin
   if (ActiveFile = nil) or (ActiveFile.NoteTagsTemporalAdded.Count = 0) then exit;

   Txt:= Self.TextPlain;
   Txt:= Txt.ToUpper;

  // Delete all temporarily added tags not found in the text, that must have been added by mistake.
  // The IDs of the rest of temporarily added tags can be modified, to avoid unnecessarily increasing the value of the Tag IDs

   N:= ActiveFile.NoteTagsTemporalAdded.Count-1;
   NextConfirmedID:= ActiveFile.NoteTagsTemporalAdded[0].ID;

   TagsStateBAK:= App.TagsState;
   App.TagsState := tsHidden;

   for i := 0 to N do begin
      NTag:= ActiveFile.NoteTagsTemporalAdded[i];
      p:= FindTag('#' + NTag.Name.ToUpper, Txt, 1);
      if p <= 0 then
         ActiveFile.DeleteNTag(NTag)
      else begin
         NTag.ID:= NextConfirmedID;
         inc(NextConfirmedID);
      end;
   end;
   ActiveFile.NoteTagsTemporalAdded.Clear;

   App.TagsState := TagsStateBAK;
   App.TagsUpdated;
end;


Initialization
   ShowingSelectionInformation:= false;

end.

