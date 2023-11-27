unit kn_NoteObj;

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
   Vcl.Graphics,
   Vcl.FileCtrl,
   Vcl.Controls,
   Vcl.ComCtrls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.ExtCtrls,
   comctrls95,
   TreeNT,
   RxRichEd,
   langs,
   gf_misc,
   gf_streams,
   gf_strings,
   gf_miscvcl,
 {$IFDEF KNT_DEBUG}
   GFLog,
 {$ENDIF}
   kn_NodeList,
   kn_StyleObj,
   kn_Info,
   kn_Const,
   kn_History
   {$IFDEF WITH_IE}
   ,SHDocVw_TLB
   {$ENDIF}
   ;


type
  ETabNoteError = class( Exception );


type
  TTabRichEdit = class; // FORWARD DECLARATION

  TTabNote = class( TPersistent )
  private
    FEditorChrome : TChrome; // user-defined fonts, colors etc.
    FID : longint; // unique note ID
    FName : TNoteNameStr; // user-defined name for the note object
    FKind : TNoteType; // redundant, because we can check object type at runtime, but it's easier this way
    FVisible : boolean; // UNUSED but may be useful later (with specialized notes)
    FReadOnly : boolean;
    FInfo : longint; // internal use only
    FTag : longint;
    FModified : boolean;
    FDateCreated : TDateTime;
    FTabIndex : integer;
    FImageIndex : integer;
    FPlainText : boolean; // if true, contents of editor are saved as plain text
    FDataStream : TMemoryStream;
    FNoteTextPlain : string;
    FFocusMemory : TFocusMemory; // which control was last focused

    FWordWrap : boolean;  // for RTF-type notes only
    FURLDetect : boolean; // highlight URLs; for RTF-type notes only
    FTabSize : byte;  // for RTF-type notes only
    FUseTabChar : boolean;  // for RTF-type notes only
    FIsInsertMode : boolean;  // for RTF-type notes only
    FCaretPos : TPoint; // caret position saved to data file (to be restored on Load; do not use otherwise because it is ONLY updated on saving file!)

    FAuxiliarAlarmList: TList;

    // VCL controls. By default, the note does NOT access them in any way
    FTabSheet : TTab95Sheet; // the tabsheet which holds this note
    FEditor : TTabRichEdit;  // for RTF-type notes only

    FHistory : TKNTHistory;        // Note (local) history

    FImagesMode: TImagesMode;
    fImagesReferenceCount: TImageIDs;


    // events
    FOnChange : TNotifyEvent;

    procedure SetName( AName : TNoteNameStr );
    procedure SetID( AID : longint );
    procedure SetReadOnly( AReadOnly : boolean );
    procedure SetPlainText( APlainText : boolean );
    procedure SetTabIndex( ATabIndex : integer );
    procedure SetModified( AModified : boolean );
    function  GetModified : boolean;
    procedure SetImageIndex( AImgIdx : integer );
    procedure SetEditorChrome( AChrome : TChrome );
    procedure SetTabSheet( ATabSheet : TTab95Sheet );

    procedure SetWordWrap( AWordWrap : boolean );
    procedure SetURLDetect( AURLDetect : boolean );
    procedure SetTabSize( ATabSize : byte );
    procedure SetEditor( AEditor : TTabRichEdit );

    function CheckEditor : boolean;
    function CheckTabSheet : boolean;

    procedure BaseSaveProc( var tf : TTextFile );

    function PropertiesToFlagsString : TFlagsString; virtual;
    procedure FlagsStringToProperties( const FlagsStr : TFlagsString ); virtual;

    procedure SaveAlarms(var tf : TTextFile; node: TNoteNode = nil);
    procedure ProcessAlarm (s: AnsiString; node: TNoteNode = nil);

    procedure SetImagesMode(ImagesMode: TImagesMode);

    function InitializeTextPlain(RTFAux: TRxRichEdit): boolean;

  public
    property Editor : TTabRichEdit read FEditor write SetEditor;
    property ID : longint read FID write SetID;
    property EditorChrome : TChrome read FEditorChrome write SetEditorChrome;
    property Kind : TNoteType read FKind;
    property Name : TNoteNameStr read FName write SetName;
    property ImageIndex : integer read FImageIndex write SetImageIndex;
    property Visible : boolean read FVisible write FVisible;
    property ReadOnly : boolean read FReadOnly write SetReadOnly;
    property Info : longint read FInfo write FInfo;
    property Tag : longint read FTag write FTag;
    property Modified : boolean read GetModified write SetModified;
    property DateCreated : TDateTime read FDateCreated write FDateCreated;
    property TabIndex : integer read FTabIndex write SetTabIndex;

    property History : TKNTHistory read FHistory;

    property ImagesMode: TImagesMode read FImagesMode write SetImagesMode;
    property ImagesReferenceCount: TImageIDs read fImagesReferenceCount write fImagesReferenceCount;
    property ImagesInstances: TImageIDs read fImagesReferenceCount;

    property PlainText : boolean read FPlainText write SetPlainText;
    property WordWrap : boolean read FWordWrap write SetWordWrap;
    property URLDetect : boolean read FURLDetect write SetURLDetect;
    property TabSize : byte read FTabSize write SetTabSize;
    property UseTabChar : boolean read FUseTabChar write FUseTabChar;
    property IsInsertMode : boolean read FIsInsertMode write FIsInsertMode;
    property CaretPos : TPoint read FCaretPos write FCaretPos;
    property FocusMemory : TFocusMemory read FFocusMemory write FFocusMemory;

    property TabSheet : TTab95Sheet read FTabSheet write SetTabSheet;

    property DataStream : TMemoryStream read FDataStream;
    property NoteTextPlain : string read FNoteTextPlain write FNoteTextPlain;

    // events
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

    constructor Create;
    destructor Destroy; override;

    procedure SaveToFile( var tf : TTextFile ); virtual;
    procedure LoadFromFile( var tf : TTextFile; var FileExhausted : boolean; var NextBlock: TNextBlock); virtual;

    procedure SaveRTFToFile(var tf : TTextFile; DataStream : TMemoryStream; PlainText: Boolean; PlaintextLeader: AnsiString = _NF_PLAINTEXTLEADER);
    

{$IFDEF WITH_DART}
    procedure SaveDartNotesFormat( Stream : TStream ); virtual;
    procedure LoadDartNotesFormat( Stream : TStream ); virtual;
{$ENDIF}

    procedure SetEditorProperties( const aProps : TNoteEditorProperties );
    procedure GetEditorProperties( var aProps : TNoteEditorProperties );

    procedure SetTabProperties( const aProps : TNoteTabProperties; UpdateName: boolean= True );
    procedure GetTabProperties( var aProps : TNoteTabProperties );

    procedure UpdateEditor; virtual;
    procedure UpdateTabSheet;

    function PrepareTextPlain (myTreeNode: TTreeNTNode; RTFAux: TTabRichEdit): string; overload;
    function PrepareTextPlain (myNoteNode: TNoteNode; RTFAux: TTabRichEdit): string; overload;

    procedure DataStreamToEditor; virtual;
    function EditorToDataStream: TMemoryStream; virtual;

    procedure GetImagesIDInstances (Stream: TMemoryStream; TextPlain: String);
    procedure ResetImagesReferenceCount;
    function CheckSavingImagesOnMode (ImagesMode: TImagesMode;
                                      Stream: TMemoryStream;
                                      ExitIfAllImagesInSameModeDest: boolean = true): TImageIDs;

    function GetAlarms(considerDiscarded: boolean): TList;
    function HasAlarms (considerDiscarded: boolean): boolean;

    procedure AddProcessedAlarms ();
    procedure AddProcessedAlarmsOfNote (newNote: TTabNote);
    procedure AddProcessedAlarmsOfNode (node: TNoteNode; newNote: TTabNote; newNode: TNoteNode);

  end; // TTabNote


  // TFileDroppedEvent = procedure( sender : Tobject; FileList : TStringList ) of object;

  TTabRichEdit = class( TRxRichEdit )
  private
    // just a few properties tacked onto the RxRichEdit
    // to keep them all in one place
    FNoteObj : TTabNote;
    FAutoIndent : boolean;
    FUseTabChar : boolean;
    FTabSize : byte;
    FRecreateWndProtect : boolean;
    //FOnFileDropped : TFileDroppedEvent;                                                   // [dpv] Commented

    procedure CMDialogKey( var Message: TCMDialogKey ); message CM_DIALOGKEY;

    // procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;

  protected
    //procedure CreateWnd; override;                                                        // [dpv] Commented
    //procedure DestroyWnd; override;
    procedure CMRecreateWnd(var Message: TMessage); message CM_RECREATEWND;

  public
    property NoteObj : TTabNote read FNoteObj write FNoteObj;
    property AutoIndent : boolean read FAutoIndent write FAutoIndent;
    property UseTabChar : boolean read FUseTabChar write FUseTabChar;
    property TabSize : byte read FTabSize write FTabSize;
    property RecreateWndProtect : boolean read FRecreateWndProtect write FRecreateWndProtect;

    // [dpv] Did not work. It has now been incorporated into RxRichEdit (see comment *3 in RxRichEd.pas)
    //property OnFileDropped : TFileDroppedEvent read FOnFileDropped write FOnFileDropped;

    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;

    procedure SimulateDoubleClick; // unused

    function FontInfoString : string;
    function ParaInfoString : string;

    function GetWordAtCursor( const LeaveSelected : boolean; const IgnoreActualSelection: boolean = False;
                                 const DiscardKNTHiddenCharacters: boolean= True ) : string;
    procedure GetLinkAtCursor(var URL: string; var TextURL: string; var LeftE: integer; var RightE: integer; SelectURL: boolean= true);
    function ParagraphsSelected: Boolean;
    procedure HideKNTHiddenMarks(Selection: boolean = true);
    procedure RemoveKNTHiddenCharacters (selection: boolean= true);

  end; // TTabRichEdit

type
  TTreeNote = class( TTabNote )
  private
    FNodes : TNodeList;
    FSplitter : TSplitter;
    FTV : TTreeNT;
    FIconKind : TNodeIconKind;
    FTreeChrome : TChrome;
    FSelectedNode : TNoteNode;
    FOldSelectedIndex : integer;
    FTreeHidden : boolean;
    FHideCheckedNodes: boolean;       // [dpv]
    FFiltered: boolean;               // [dpv]

    FDefaultNodeName : string;
    FAutoNumberNodes : boolean;
    FCheckboxes : boolean;
    FVerticalLayout : boolean;

    {$IFDEF WITH_IE}
    FMainPanel : TPanel;
    FWebBrowser : TWebBrowser;
    {$ENDIF}

    // state that needs to be recalled
    FTreeWidth : integer;

    function GetCount : integer;
    function CheckTree : boolean;
    procedure SetTreeChrome( AChrome : TChrome );

    // function GetSelectedIndex : integer;
    function GetSelectedNode : TNoteNode;
    procedure SetSelectedNode( aNode : TNoteNode );

    function PropertiesToFlagsString : TFlagsString; override;
    procedure FlagsStringToProperties( const FlagsStr : TFlagsString ); override;

    function InternalAddNode( const aNode : TNoteNode ) : integer;
    procedure InternalInsertNode( const aIndex : integer; const aNode : TNoteNode );
    procedure GenerateNodeID( const aNode : TNoteNode );
    procedure VerifyNodeIDs;

  public
    property Nodes : TNodeList read FNodes;
    property NodeCount : integer read GetCount;

    property Splitter : TSplitter read FSplitter write FSplitter;
    property TV : TTreeNT read FTV write FTV;
    property IconKind : TNodeIconKind read FIconKind write FIconKind;
    property TreeWidth : integer read FTreeWidth write FTreeWidth;
    property HideCheckedNodes: Boolean read FHideCheckedNodes write FHideCheckedNodes; // [dpv]
    property Filtered: Boolean read FFiltered write FFiltered;                         // [dpv]
    property SelectedNode : TNoteNode read GetSelectedNode write SetSelectedNode;
    property OldSelectedIndex : integer read FOldSelectedIndex;
    property Checkboxes : boolean read FCheckboxes write FCheckboxes;
    property TreeChrome : TChrome read FTreeChrome write SetTreeChrome;
    property DefaultNodeName : string read FDefaultNodeName write FDefaultNodeName;
    property AutoNumberNodes : boolean read FAutoNumberNodes write FAutoNumberNodes;
    property VerticalLayout : boolean read FVerticalLayout write FVerticalLayout;
    property TreeHidden : boolean read FTreeHidden write FTreeHidden;

    {$IFDEF WITH_IE}
    property MainPanel : TPanel read FMainPanel write FMainPanel;
    property WebBrowser : TWebBrowser read FWebBrowser write FWebBrowser;
    // property Grid : TStringAlignGrid read FGrid write FGrid;
    // property GridSplitter : TSplitter read FGridSplitter write FGridSplitter;
    {$ENDIF}

    constructor Create;
    destructor Destroy; override;

    function SaveToFile( var tf : TTextFile;  OnlyCurrentNodeAndSubtree: TTreeNTNode= nil;
                         OnlyNotHiddenNodes: boolean= false; OnlyCheckedNodes: boolean= false): integer;
    procedure LoadFromFile( var tf : TTextFile; var FileExhausted : boolean; var NextBlock: TNextBlock); override;

    function NewNode( const AParent : TNoteNode; AName : string; const AInheritProperties : boolean ) : TNoteNode;
    function AddNode( const aNode : TNoteNode ) : integer;
    procedure InsertNode( const aIndex : integer; const aNode : TNoteNode );
    procedure RemoveNode( const aNode : TNoteNode );

    procedure DataStreamToEditor; override;
    function EditorToDataStream: TMemoryStream; override;
    function StreamFormatInNode: TRichStreamFormat;
    procedure SetTreeProperties( const aProps : TNoteTreeProperties );
    procedure GetTreeProperties( var aProps : TNoteTreeProperties );

    procedure UpdateEditor; override;
    procedure UpdateTree;

    procedure LoadFromTreePadFile( const FN : string );

    function GetNodeByID( const aID : integer ) : TNoteNode;
    function GetTreeNodeByID( const aID : integer ) : TTreeNTNode;

    function InitializeTextPlainVariables( nMax: integer; RTFAux: TTabRichEdit ): boolean;
    function InitializeTextPlain(myNoteNode: TNoteNode; RTFAux: TRxRichEdit): boolean; overload;

  end;


  procedure SaveParagraphAttributes(const Editor: TTabRichEdit; var Paragraph: TParaFormat2);
  procedure ApplyParagraphAttributes(const Editor: TTabRichEdit; var Paragraph: TParaFormat2;
                                       const Reduced: Boolean = False);

  procedure SaveTextAttributes(const Editor: TTabRichEdit; var Format: TCharFormat2);
  procedure ApplyTextAttributes(const Editor: TTabRichEdit; var Format: TCharFormat2);


var
  _LoadedRichEditVersion : Single;

implementation
uses
   kn_global,
   kn_AlertMng,
   kn_LinksMng,
   kn_Main,
   kn_EditorUtils,
   kn_RTFUtils,
   kn_ImagesMng,
   kn_NoteFileMng,
   kn_TreeNoteMng
   ;


resourcestring
  STR_01 = 'Fatal: attempted to save an extended note as a simple RTF note.';
  STR_02 = '"%s" is a %s note and cannot be saved using %s format';
  STR_03 = 'Stream not assigned in LoadDartNotesFormat';
  STR_04 = 'LoadDartNotes: file format error or file damaged.';
  STR_05 = 'Problem while saving tree-type note "%s": Node count mismatch (Tree: %d  Internal: %d) ' +
      'The note may not be saved correctly. Continue?';
  STR_06 = 'Warning: "%s"';
  STR_07 = 'Node count mismatch.';
  STR_08 = 'Virtual node "%s" in note "%s" cannot write file ';
  STR_09 = 'Note contains %d nodes, but only %d were saved.';
  STR_10 = 'Could not load Virtual Node file:';
  STR_11 = 'Failed to open TreePad file ';


  /// Save on 'Paragraph' the paragraph formatting attributes of the current selection
  //
  procedure SaveParagraphAttributes(const Editor: TTabRichEdit; var Paragraph: TParaFormat2);
  begin
     Editor.Paragraph.GetAttributes(Paragraph);
  end;

  /// Sets the paragraph formatting for the current selection with the attributes saved on 'Paragraph'
  //
  procedure ApplyParagraphAttributes(const Editor: TTabRichEdit; var Paragraph: TParaFormat2;
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

     Editor.Paragraph.SetAttributes(Paragraph);
     ActiveNote.Editor.HideKNTHiddenMarks(true);
  end;


  /// Save on 'Format' the attributes of the first character
  //
  procedure SaveTextAttributes(const Editor: TTabRichEdit; var Format: TCharFormat2);
  var
     SelLengthBak: Integer;
  begin

     with Editor do begin
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

  end;


  /// Sets character formatting on the selection (or word at cursor) with the attributes saved in 'Format'
  //
  procedure ApplyTextAttributes(const Editor: TTabRichEdit; var Format: TCharFormat2);
  begin
     Format.dwMask := DWORD(CFM_ALL2);
     if _LoadedRichEditVersion < 6 then begin     // #529 : Paste Font Attributes destroy selected hyperlinks when applied (in Richedit <= 5.0)
         Format.dwMask := Format.dwMask
                          and not CFM_HIDDEN and not CFM_LINK;
     end;

     if Editor.SelLength = 0 then
        Editor.WordAttributes.SetAttributes(Format)
     else
        Editor.SelAttributes.SetAttributes(Format);

     ActiveNote.Editor.HideKNTHiddenMarks(true);
  end;



// ---------------- TTABNOTE -----------------

constructor TTabNote.Create;
begin
  inherited Create;
  FKind := ntRTF;
  FOnChange := nil;
  FTabSheet := nil;
  FTabIndex := 0;
  FName := DEFAULT_NEW_NOTE_NAME;
  FID := 0;
  FVisible := true;
  FReadOnly := false;
  FPlainText := false;
  FInfo := 0;
  FTag := 0;
  FImageIndex := -1;
  FDateCreated := now;
  FFocusMemory := focNil;
  FModified := false;
  FDataStream := TMemoryStream.Create;
  FNoteTextPlain := '';
  InitializeChrome( FEditorChrome );

  FEditor := nil;
  FWordWrap := false;
  FURLDetect := true;
  FTabSize := DEF_TAB_SIZE;
  FUseTabChar := false;
  FIsInsertMode := true;
  with FCaretPos do
  begin
    x := 0;
    y := 0;
  end;
  FAuxiliarAlarmList:= TList.Create;

  FHistory := TKNTHistory.Create;

end; // CREATE

destructor TTabNote.Destroy;
begin
  FOnChange := nil;
  if assigned( FDataStream ) then FDataStream.Free;
  AlarmManager.RemoveAlarmsOfNote(Self);
  if assigned (FAuxiliarAlarmList) then FAuxiliarAlarmList.Free;

  inherited Destroy;
end; // TTabNote .Destroy;

function TTabNote.PropertiesToFlagsString : TFlagsString;
begin
  result := DEFAULT_FLAGS_STRING;
  result[1] := BOOLEANSTR[FVisible];
  result[2] := BOOLEANSTR[FReadOnly];
  result[3] := BOOLEANSTR[FWordWrap];
  result[4] := BOOLEANSTR[FURLDetect];
  result[5] := BOOLEANSTR[FUseTabChar];
  result[6] := BOOLEANSTR[FPlainText];
end; // PropertiesToFlagsString

procedure TTabNote.FlagsStringToProperties( const FlagsStr : TFlagsString );
begin
  if ( length( FlagsStr ) < FLAGS_STRING_LENGTH ) then exit;
  FVisible    := FlagsStr[1] = BOOLEANSTR[true];
  FReadOnly   := FlagsStr[2] = BOOLEANSTR[true];
  FWordWrap   := FlagsStr[3] = BOOLEANSTR[true];
  FURLDetect  := FlagsStr[4] = BOOLEANSTR[true];
  FUseTabChar := FlagsStr[5] = BOOLEANSTR[true];
  FPlainText  := FlagsStr[6] = BOOLEANSTR[true];
end; // FlagsStringToProperties


procedure TTabNote.UpdateTabSheet;
begin
  // CheckTabSheet;
  with FTabSheet do
  begin
    Caption := FName;
    ImageIndex := FImageIndex;
  end;
end; // UpdateTabSheetProperties

procedure TTabNote.UpdateEditor;

var
//  tabstopcnt : integer;
  TextLen: integer;
  SS, SL: integer;

begin
  if ( not CheckEditor ) then exit;

  FEditor.BeginUpdate;
  try
    FEditor.WordWrap := FWordWrap;
    FEditor.TabSize := FTabSize;
    FEditor.AutoURLDetect := FURLDetect;

    FEditor.Color := FEditorChrome.BGColor;
    TextLen:= FEditor.TextLength;
    if (TextLen = 0) or PlainText then begin          // Solves the problem indicated in kn_NoteMng.EditProperties...*1
       with FEditor.DefAttributes do begin
         Charset := FEditorChrome.Font.Charset;
         Name := FEditorChrome.Font.Name;
         Size := FEditorChrome.Font.Size;
         Style := FEditorChrome.Font.Style;
         Color := FEditorChrome.Font.Color;
         Language := FEditorChrome.Language;
       end;
    end;

    if PlainText and (TextLen > 0) then begin       // Related to *1. If PlainText then we do want it to always change the font format
       SS:= FEditor.SelStart;
       SL:= FEditor.SelLength;
       FEditor.SelectAll;
       FEditor.SelAttributes.Assign( FEditor.DefAttributes );
      {
      FEditor.Paragraph.TabCount := 8; // max is 32, but what the hell
      for tabstopcnt := 0 to 7 do
        FEditor.Paragraph.Tab[tabstopcnt] := (tabstopcnt+1) * (2*FTabSize); // [x] very rough!
      }
      FEditor.SetSelection(SS, SS+SL, True);
    end;
    FEditor.UseTabChar := FUseTabChar;
    FEditor.ReadOnly := FReadOnly;
  finally
    FEditor.EndUpdate;
  end;

end; // UpdateEditor

procedure TTabNote.SetEditorChrome( AChrome : TChrome );
begin
  FEditorChrome := AChrome;
  FModified := true;
end; // SetEditorChrome

procedure TTabNote.SetEditorProperties( const aProps : TNoteEditorProperties );
begin
  FPlainText := aProps.PlainText;
  FTabSize := aProps.TabSize;
  FURLDetect := aProps.URLDetect;
  FUseTabChar := aProps.UseTabChar;
  FWordWrap := aProps.WordWrap;
  FModified := true;
end; // SetEditorProperties

procedure TTabNote.GetEditorProperties( var aProps : TNoteEditorProperties );
begin
  aProps.PlainText := FPlainText;
  aProps.TabSize := FTabSize;
  aProps.URLDetect := FURLDetect;
  aProps.UseTabChar := FUseTabChar;
  aProps.WordWrap := FWordWrap;
end; // GetEditorProperties

procedure TTabNote.SetTabProperties( const aProps : TNoteTabProperties; UpdateName: boolean= True);
begin
  if UpdateName then
     FName := aProps.Name;
  FImageIndex := aProps.ImageIndex;
  FModified := true;
end; // SetTabProperties

procedure TTabNote.GetTabProperties( var aProps : TNoteTabProperties );
begin
  aProps.ImageIndex := FImageIndex;
  aProps.Name := FName;
end; // GetTabProperties

procedure TTabNote.SetTabSheet( ATabSheet : TTab95Sheet );
begin
  if ( ATabSheet <> FTabSheet ) then
  begin
    FTabSheet := ATabSheet;
  end;
end; // SetTabSheet

procedure TTabNote.SetEditor( AEditor : TTabRichEdit );
begin
  if ( AEditor <> FEditor ) then
  begin
    FEditor := AEditor;
  end;
end; // TTabNote.SetEditor

procedure TTabNote.SetName( AName : TNoteNameStr );
begin
  AName := trim( AName );
  if (( FName = AName ) or ( AName = '' )) then exit;
  FName := copy( AName, 1, TABNOTE_NAME_LENGTH );
  FModified := true;
  if _ALLOW_VCL_UPDATES and assigned( FTabSheet ) then FTabSheet.Caption := FName;
end; // SetName

procedure TTabNote.SetID( AID : longint );
begin
  if ( FID = 0 ) then
    FID := AID;
  // otherwise, never allow the ID to be changed
end; // SetID


function TTabNote.PrepareTextPlain(myTreeNode: TTreeNTNode; RTFAux: TTabRichEdit): string;
var
    myNoteNode : TNoteNode;
begin
   if FEditor.Modified or ((Kind <> ntTree) and (NoteTextPlain = '')) then
      EditorToDataStream;

   if (Kind <> ntTree) then
       Result:= NoteTextPlain
   else begin
      myNoteNode := TNoteNode( myTreeNode.Data );
      TTreeNote(Self).InitializeTextPlain(myNoteNode, RTFAux);
      Result:= myNoteNode.NodeTextPlain;
   end;
end;

function TTabNote.PrepareTextPlain(myNoteNode: TNoteNode; RTFAux: TTabRichEdit): string;
begin
   if FEditor.Modified or ((Kind <> ntTree) and (NoteTextPlain = '')) then
      EditorToDataStream;

   if (Kind <> ntTree) then
       Result:= NoteTextPlain
   else begin
      TTreeNote(Self).InitializeTextPlain(myNoteNode, RTFAux);
      Result:= myNoteNode.NodeTextPlain;
   end;
end;



procedure TTabNote.DataStreamToEditor;
var
  ReadOnlyBAK: boolean;
  {$IFDEF KNT_DEBUG}
  str: String;
  dataSize: integer;
  {$ENDIF}
  strRTF: AnsiString;
  ContainsImgIDsRemoved: boolean;
  ContainsImages: boolean;
begin
  if CheckEditor then begin
    ReadOnlyBAK:= FEditor.ReadOnly;
    ContainsImgIDsRemoved:= false;

    try
      FEditor.ReadOnly:= false;    // To prevent the problem indicated in issue #537
      FEditor.OnChange := nil;
      FDataSTream.Position := 0;
      strRTF:= '';

      fImagesReferenceCount:= nil;
      if NodeStreamIsRTF (FDataStream) then begin
         FEditor.StreamFormat:= sfRichText;
         if (ImagesManager.StorageMode <> smEmbRTF) and (not FPlainText) then begin
            GetImagesIDInstances (FDataStream, NoteTextPlain);
            strRTF:= ImagesManager.ProcessImagesInRTF(FDataStream.Memory, FDataStream.Size, Self, ImagesManager.ImagesMode, '', 0, ContainsImgIDsRemoved, ContainsImages, true);
         end;
      end
      else
         FEditor.StreamFormat:= sfPlainText;


      Log_StoreTick('TTabNote.DataStreamToEditor - BEGIN', 4, +1);
      {$IFDEF KNT_DEBUG}
       if log.Active and  (log.MaxDbgLevel >= 4) then begin
          dataSize:= FDataStream.Size;
          str:= Copy(String(PAnsiChar(FDataStream.Memory)), 1, 250);
          Log.Add(string.format('sfRichText?:%s DataSize:%d  RTF:"%s"...', [BoolToStr(FEditor.StreamFormat=sfRichText), dataSize, str]),  4 );
       end;
      {$ENDIF}

      if StrRTF <> '' then begin
         FEditor.PutRtfText(strRTF,True,False);               // => ImageManager.StorageMode <> smEmbRTF
         FEditor.ClearUndo;
      end
      else
         FEditor.Lines.LoadFromStream( FDataStream );


      Log_StoreTick('TTabNote.DataStreamToEditor - END', 4, -1);

      FEditor.OnChange := Form_Main.RxRTFChange;
    finally
      FEditor.ReadOnly:= ReadOnlyBAK;
      if not ContainsImgIDsRemoved then
         FEditor.Modified := false;
     end;
  end;
end; // DataStreamToEditor

{
  If Editor was modified then it will return the Stream associated to the node that will be updated
}
function TTabNote.EditorToDataStream: TMemoryStream;
var
 Encoding: TEncoding;
 strRTF: AnsiString;
 ImagesIDs_New: TImageIDs;

begin
  Result:= nil;
  Encoding:= nil;

  if not assigned(FEditor) then Exit;

  if FEditor.Modified then
    try
      with FDataStream do begin
        Clear;
        Position := 0;
      end;
      FEditor.StreamMode := [];
      if FPlainText then begin
        FEditor.StreamFormat := sfPlainText;
        if not CanSaveAsANSI(FEditor.Text) then
           //FEditor.StreamMode := [smUnicode];
           Encoding:= TEncoding.UTF8;
      end
      else
        FEditor.StreamFormat := sfRichText;

      FEditor.Lines.SaveToStream( FDataStream, Encoding);

      ImagesIDs_New:= nil;
      if (ImagesManager.StorageMode <> smEmbRTF) and (not FPlainText) then begin
         ImagesIDs_New:= CheckSavingImagesOnMode (imLink, FDataStream, true);
         ImagesManager.UpdateImagesCountReferences (fImagesReferenceCount, ImagesIDs_New);
         fImagesReferenceCount:= ImagesIDs_New;
      end;

      if ImagesIDs_New = nil then
         NoteTextPlain:= FEditor.TextPlain
      else begin
         { If the node has images we will make sure that in TextPlain we save the version corresponding to imLink,
           to facilitate search management. See notes on TImageManager.GetSearchOffset }
         NoteTextPlain:= '';
         InitializeTextPlain(RTFAux_Note);
      end;

      FEditor.Modified:= false;
      Result:= FDataStream;

    finally
      FEditor.StreamFormat := sfRichText;
      FEditor.StreamMode := [];
    end

  else
     if (NoteTextPlain = '') then
        InitializeTextPlain(RTFAux_Note);

end; // EditorToDataStream



function TTabNote.CheckSavingImagesOnMode (ImagesMode: TImagesMode; Stream: TMemoryStream;
                                           ExitIfAllImagesInSameModeDest: boolean = true): TImageIDs;
var
  strRTF: AnsiString;
  ContainsImgIDsRemoved: boolean;
  ContainsImages: boolean;

begin
    Result:= nil;

    strRTF:= ImagesManager.ProcessImagesInRTF(Stream.Memory, Stream.Size, Self, ImagesMode, '', 0, ContainsImgIDsRemoved, ContainsImages, ExitIfAllImagesInSameModeDest);
    if strRTF <> '' then begin                                // Changes in images must be reflected (=> ContainsImages=true)
       if ImagesManager.StorageMode = smEmbRTF then           // If smEmbRTF -> we are calling from UpdateImagesStorageMode (when converting from a different mode to smEmbRTF)
          strRTF:= RemoveKNTHiddenCharactersInRTF(strRTF, hmOnlyImages);

       { *1 : I'm interested in adding a #0 at the end (in nodes/notes with RTF content), so that methods like ProcessImagesInRTF,
          which use the Pos() method directly on the Stream.Memory, always have an end...
          Note: When we execute FEditor.Lines.SaveToStream( FDataStream, Encoding); in the Stream it ends with a #0    }
       Stream.Size:= Length(StrRTF);
       Stream.Position := 0;
       StringToMemoryStream(StrRTF, Stream);
       if (StrRTF[Length(StrRTF)] <> #0) then                // *1
          Stream.WriteData(0);
    end;


    if ImagesManager.StorageMode <> smEmbRTF then begin       // If = smEmbRTF =>  fChangingImagesStorage=True

       if ContainsImages then
          Result:= ImagesManager.GetImagesIDInstancesFromRTF (Stream);
    end;
end;


procedure TTabNote.GetImagesIDInstances (Stream: TMemoryStream; TextPlain: String);
begin
   if (TextPlain <> '') then
      fImagesReferenceCount:= ImagesManager.GetImagesIDInstancesFromTextPlain (TextPlain)
   else
      fImagesReferenceCount:= ImagesManager.GetImagesIDInstancesFromRTF (Stream);
end;

procedure TTabNote.ResetImagesReferenceCount;
begin
    SetLength(fImagesReferenceCount, 0);
end;


function TTabNote.CheckEditor : boolean;
begin
  result := assigned( FEditor );
end; // CheckEditor

function TTabNote.CheckTabSheet : boolean;
begin
  result := assigned( FTabSheet );
end; // CheckTabSheet

procedure TTabNote.BaseSaveProc( var tf : TTextFile );
var
  HaveVCLControls : boolean;
begin
  HaveVCLControls := ( CheckTabSheet and CheckEditor );

  if HaveVCLControls then
  begin
    FTabIndex := FTabSheet.PageIndex; // remember tab position
    with FCaretPos do
    begin
      x := FEditor.SelStart;
      y := FEditor.SelLength;
    end;
  end;

  tf.WriteLine( _NoteName + '=' + FName,  True);
  tf.WriteLine( _NoteID + '=' + FID.ToString  );
  tf.WriteLine( _ImageIndex + '=' + FImageIndex.ToString  );
  tf.WriteLine( _DateCreated + '=' + FormatDateTime( _SHORTDATEFMT + #32 + _LONGTIMEFMT, FDateCreated ) );
  tf.WriteLine( _TabIndex + '=' + FTabIndex.ToString  );
  tf.WriteLine( _TabSize + '=' + inttostr( FTabSize )  );

  tf.WriteLine( _PosX + '=' + FCaretPos.X.ToString  );
  tf.WriteLine( _PosY + '=' + FCaretPos.Y.ToString  );
  tf.WriteLine( _CHBGColor + '=' + ColorToString( FEditorChrome.BGColor )  );
  tf.WriteLine( _CHFontCharset + '=' + inttostr( FEditorChrome.Font.Charset )  );
  tf.WriteLine( _CHFontColor + '=' + ColorToString( FEditorChrome.Font.Color )  );
  tf.WriteLine( _CHFontName + '=' + FEditorChrome.Font.Name  );
  tf.WriteLine( _CHFontSize + '=' + FEditorChrome.Font.Size.ToString  );
  tf.WriteLine( _CHLanguage + '=' + IntToStr(FEditorChrome.Language) );
  tf.WriteLine( _CHFontStyle + '=' + FontStyleToStr( FEditorChrome.Font.Style )  );
  tf.WriteLine( _Flags + '=' + PropertiesToFlagsString  );

  { // REMOVED: FlagsString is used instead
  writeln( tf, _UseTabChar, '=', BOOLEANSTR[FUseTabChar] );
  writeln( tf, _ReadOnly, '=', BOOLEANSTR[FReadOnly] );
  writeln( tf, _WordWrap, '=', BOOLEANSTR[FWordWrap] );
  writeln( tf, _URLDetect, '=', BOOLEANSTR[FURLDetect] );
  }
  if HasAlarms(true) then
     SaveAlarms(tf);

end; // BaseSaveProc

(*
Format of the serialized alarm:  NA=[D]Reminder[/Expiration][*Format][|subject]
Ej: NA=D10-06-2010 08:00:00/10-06-2010 07:55:00*B100/1200|Comment to the alarm

[] => optional
D: Discarded
Expiration or Reminder: DD-MM-YYYY HH:MM:SS
Format: BoldFormatFontColor/BackColor
BoldFormat: B or N   (Bold or Normal)
FontColor - BackColor: number (TColor)
subject: unicode text

*)
procedure TTabNote.SaveAlarms(var tf : TTextFile; node: TNoteNode = nil);
var
   I: integer;
   Alarms: TList;
   s: string;
   alarm: TAlarm;
   BoldStr: char;
begin
  try
     if assigned(node) then
        Alarms:= node.getAlarms(true)
     else
        Alarms:= Self.getAlarms(true);

     I:= 0;
     while I <= Alarms.Count - 1 do begin
        alarm:= TAlarm(Alarms[i]);
        s:= '';
        if alarm.ExpirationDate <> 0 then
           s:= '/' + FormatDateTime( _SHORTDATEFMT + #32 + _LONGTIMEFMT, alarm.ExpirationDate );

        if alarm.Bold or (alarm.FontColor <> clWindowText) or (alarm.BackColor <> clWindow) then begin
           if alarm.Bold then BoldStr:= 'B' else BoldStr:= 'N';
           s:= s + '*' + BoldStr + IntToStr(alarm.FontColor) + '/' + IntToStr(alarm.BackColor);
           end;

        if alarm.AlarmNote <> '' then
           s:= s + '|' + StringReplace(alarm.AlarmNote, #13#10, 'ªª', [rfReplaceAll]);
        s:= FormatDateTime( _SHORTDATEFMT + #32 + _LONGTIMEFMT, alarm.AlarmReminder ) + s;
        if alarm.Status = TAlarmDiscarded then
           s:= 'D' + s;
        tf.WriteLine( _NodeAlarm + '=' + s, True );

        I:= I + 1;
     end;

  except
  end;
end;


procedure TTabNote.SaveRTFToFile(var tf : TTextFile; DataStream : TMemoryStream; PlainText: Boolean; PlaintextLeader: AnsiString = _NF_PLAINTEXTLEADER);
var
  List : TStringList;
  cnt: integer;
  i, pos : integer;
  addCRLF: boolean;
  StreamAux : TMemoryStream;
  ImagesIDs: TImageIDs;

begin
    if DataStream.Size = 0 then
       exit;

  StreamAux:= nil;
  try

    if (not PlainText) and ImagesManager.ExportingMode then begin
       var RTFwithProccesedImages: AnsiString;
       var ContainsImgIDsRemoved: boolean;
       var ContainsImages: boolean;
       var ImgMode: TImagesMode;


       if KeyOptions.ImgStorageModeOnExport = smeEmbRTF then
          ImgMode:= imImage
       else
          ImgMode:= imLink;

       RTFwithProccesedImages:= ImagesManager.ProcessImagesInRTF(DataStream.Memory, DataStream.Size, nil, ImgMode, '', 0, ContainsImgIDsRemoved, ContainsImages, true);
       if (RTFwithProccesedImages = '') and ContainsImages and (KeyOptions.ImgStorageModeOnExport <> smeEmbKNT) then
          RTFwithProccesedImages:= MemoryStreamToString (DataStream);

       if (RTFwithProccesedImages <> '') then begin
          if (KeyOptions.ImgStorageModeOnExport <> smeEmbKNT) then
             RTFwithProccesedImages:= RemoveKNTHiddenCharactersInRTF(RTFwithProccesedImages, hmOnlyImages);

          StreamAux := TMemoryStream.Create;
          StreamAux.Write(RTFwithProccesedImages[1], ByteLength(RTFwithProccesedImages));

          DataStream:= StreamAux;
       end;

       if ContainsImages and (KeyOptions.ImgStorageModeOnExport = smeEmbKNT) then begin
          ImagesIDs:= ImagesManager.GetImagesIDInstancesFromRTF (DataStream);
          ImagesManager.RegisterImagesReferencesExported (ImagesIDs);
       end;
    end;


    DataStream.Position := 0;

    tf.WriteLine( _NF_RTF );

    if PlainText then begin
        // Looking for: ;[<BOM>]first line...
        //               ;second line...

        i:= 0;
        addCRLF:= false;
        repeat
           tf.write( PlaintextLeader );
           pos:= PosEx(AnsiString(#13#10), PAnsiChar(DataStream.Memory), i+1);       // The index is 1-based.
           if (pos=0) or (pos > DataStream.Size) then begin
               pos:= DataStream.Size-1;
               if (i = pos) and (PByte(DataStream.Memory)[i] = 0) then break;
               addCRLF:= true;
           end;
           tf.write(PByte(DataStream.Memory)[i], pos-i+1);
           i:= pos + 1;
        until i >= DataStream.Size;

        if addCRLF then
           tf.write(#13#10);

    end
    else begin
       DataStream.Position := 0;
       i:= 0;
       // When compiled in D2006 with RxRichEdit 2.75 not ocurred, but now, when saving the stream in RTF an extra #0 is added. But I checked it
       // The existence of these final #0s really suits me well. See comment to TTabNote.CheckSavingImagesOnMode
       if PByte(DataStream.Memory)[DataStream.Size-1] = 0 then i:= 1;
       tf.F.CopyFrom(DataStream, DataStream.Size - i);
    end;

  finally
     if StreamAux <> nil then
        StreamAux.Free;
  end;
end;


procedure TTabNote.SaveToFile( var tf : TTextFile );
var
  HaveVCLControls : boolean;

begin

  HaveVCLControls := CheckEditor;

  if ( FKind <> ntRTF ) then
     raise ETabNoteError.Create(STR_01);


  tf.WriteLine( _NF_TabNote ); // marks beginning of TTabNote
  BaseSaveProc( tf );


  if HaveVCLControls then begin
    EditorToDataStream;
    tf.WriteLine( _LineCount + '=' + FEditor.Lines.Count.ToString );
  end;


  case _USE_OLD_KEYNOTE_FILE_FORMAT of
    false :
        SaveRTFToFile(tf, FDataStream, FPlainText);

    true :
        SaveRTFToFile(tf, FDataStream, true, _Lines+'=');
  end;


  Modified := false; // triggers SetModified

end; // TTabNote SaveToFile



{$IFDEF WITH_DART}
procedure TTabNote.SaveDartNotesFormat( Stream : TStream );
var
  ds, ds1 : string;
  dsi : integer;
  HaveVCLControls : boolean;
begin

  // Only simple RTF notes are compatible with DartNotes format.
  // If FKind is not ntRTF, bail out
  if ( FKind <> ntRTF ) then
    raise ETabNoteError.CreateFmt( STR_02, [FName, TABNOTE_KIND_NAMES[FKind], FILE_FORMAT_NAMES[nffDartNotes]] );

  HaveVCLControls := ( CheckTabSheet and CheckEditor );
  if HaveVCLControls then
  begin
    with FDataStream do
    begin
      Clear;
      Position := 0;
    end;
    FTabIndex := FTabSheet.PageIndex; // remember tab position
    FEditor.Lines.SaveToStream( FDataStream );
  end;

  FDataStream.Position := 0;

  try
    ds := FName + _DART_STOP +
          inttostr( integer( FEditorChrome.BgColor )) + _DART_STOP +
          BOOLEANSTR[FWordWrap] + _DART_STOP;
    dsi := length( ds );
    ds := ds + inttostr( FDataStream.Size ) + _DART_STOP;
    ds1 := inttostr( length( ds ) + FDataStream.Size );
    ds := _DART_STOP + inttostr( dsi ) + _DART_STOP + ds;
    Stream.WriteBuffer( ds1[1], length( ds1 ));
    Stream.WriteBuffer( ds[1], length( ds ));

    Stream.CopyFrom( FDataStream, 0 );

  finally
    if HaveVCLControls then
      FDataStream.Clear;
  end;

  Modified := false; // triggers SetModified

end; // SaveDartNotesFormat
{$ENDIF}


function TTabNote.HasAlarms(considerDiscarded: boolean): boolean;
begin
    Result:= AlarmManager.HasAlarms(Self, nil, considerDiscarded);
end;

function TTabNote.GetAlarms(considerDiscarded: boolean): TList;
begin
   Result:= AlarmManager.GetAlarms(Self, nil, considerDiscarded);
end;


(*
Format of the serialized alarm:  NA=[D]Reminder[/Expiration][*Format][|subject]
Ej: NA=D10-06-2010 08:00:00/10-06-2010 07:55:00*B100/1200|Comment to the alarm

[] => optional
D: Discarded
Expiration or Reminder: DD-MM-YYYY HH:MM:SS
Format: BoldFormatFontColor/BackColor
BoldFormat: B or N   (Bold or Normal)
FontColor - BackColor: number (TColor)
subject: unicode text

*)
procedure TTabNote.ProcessAlarm (s: AnsiString; node: TNoteNode = nil);
var
    alarm: TAlarm;
    p, p2: integer;
    format: AnsiString;
begin
   try

      alarm:= TAlarm.Create;

      p := Pos( '|', s );
      if ( p > 0 ) then begin
          alarm.AlarmNote:= StringReplace(TryUTF8ToUnicodeString(copy(s, p+1, length(s))), 'ªª', #13#10, [rfReplaceAll]);
          delete( s, p, length(s));
      end;

      p := Pos( '*', s );
      if ( p > 0 ) then begin
          format:= copy(s, p+1, length(s));
          if format[1] = 'B' then
             alarm.Bold:= true;
          p2 := Pos( '/', format );
          alarm.FontColor := StrToInt(copy(format, 2, p2-2));
          alarm.BackColor := StrToInt(copy(format, p2+1, length(format)));
          delete( s, p, length(s));
      end;

      p := Pos( '/', s );
      if ( p > 0 ) then begin
          alarm.ExpirationDate:= strtodatetime(copy(s, p+1, length(s)));
          delete( s, p, length(s));
      end;
      if s[1] = 'D' then begin
         alarm.Status := TAlarmDiscarded;
         s:= Copy(s,2,MaxInt)
      end;
      alarm.AlarmReminder:= strtodatetime(s);
      if p <= 0  then
         alarm.ExpirationDate:= 0;

      alarm.node:= node;
      alarm.note:= Self;

      FAuxiliarAlarmList.Add(alarm);

   except
   end;
end;

procedure TTabNote.AddProcessedAlarms ();
var
  I: Integer;
  alarm: TAlarm;
begin
   if not assigned(FAuxiliarAlarmList) then exit;
   I:= 0;
   while I <= FAuxiliarAlarmList.Count - 1 do begin
      alarm:= TAlarm(FAuxiliarAlarmList[i]);
      AlarmManager.AddAlarm(alarm);
      I:= I + 1;
   end;

   FAuxiliarAlarmList.Clear;
end;

procedure TTabNote.AddProcessedAlarmsOfNote (newNote: TTabNote);
var
  I: Integer;
  alarm: TAlarm;
begin
   if not assigned(FAuxiliarAlarmList) then exit;

   I:= 0;
   while I <= FAuxiliarAlarmList.Count - 1 do begin
      alarm:= TAlarm(FAuxiliarAlarmList[i]);
      if (alarm.note = Self) and (alarm.node= nil) then begin
         alarm.note := newNote;
         AlarmManager.AddAlarm(alarm);
      end;
      I:= I + 1;
   end;
end;

procedure TTabNote.AddProcessedAlarmsOfNode (node: TNoteNode; newNote: TTabNote; newNode: TNoteNode);
var
  I: Integer;
  alarm: TAlarm;
begin
   if not assigned(FAuxiliarAlarmList) then exit;

   I:= 0;
   while I <= FAuxiliarAlarmList.Count - 1 do begin
      alarm:= TAlarm(FAuxiliarAlarmList[i]);
      if (alarm.note = Self) and (alarm.node = node) then begin
         alarm.note := newNote;
         alarm.node := newNode;
         AlarmManager.AddAlarm(alarm);
      end;
      I:= I + 1;
   end;
end;

procedure TransferRTFData(ListRTFStr : TStringList; StreamRTF: TMemoryStream);
var
   NewRTF: string;
begin
    if (ListRTFStr.Count = 0) then exit;

    if opt_Clean then begin
       if CleanRTF(ListRTFStr.Text, NewRTF) then begin
          //ListRTFStr.SaveToStream(StreamRTF);
          StreamRTF.LoadFromStream(TStringStream.Create(NewRTF));
          exit;
       end;
    end;

    ListRTFStr.WriteBOM:= False;
    ListRTFStr.SaveToStream(StreamRTF);
    if NodeStreamIsRTF(StreamRTF) then begin
      // In notes/nodes with RTF content we are interested in the buffer ending in #0 to be able to treat it as a string (accessing .Memory)
      assert((PByte(StreamRTF.Memory)[StreamRTF.Size-1] <> 0), 'The Stream already ends at 0');
      StreamRTF.WriteData(0);
    end;
end;


procedure TTabNote.LoadFromFile( var tf : TTextFile; var FileExhausted : boolean; var NextBlock: TNextBlock );
var
  List : TStringList;
  s, key : AnsiString;
  p, linecount : integer;
  InRichText : boolean;
begin
  with FDataStream do
  begin
    Clear;
    Position := 0;
  end;
  FileExhausted := false;
  InRichText := false;

  List := TStringList.Create;
  try
    while ( not tf.eof()) do
    begin
      s:= tf.readln();
      if ( s = _NF_RTF ) then
      begin
        InRichText := ( not _IS_OLD_KEYNOTE_FILE_FORMAT );
        continue;
      end;
      if ( s = _NF_TabNote ) then
      begin
        NextBlock:= nbRTF;
        break; // New TabNote begins
      end;
      if ( s = _NF_TreeNote ) then
      begin
        NextBlock:= nbTree;
        break; // New TreeNote begins
      end;
      if ( s = _NF_StoragesDEF ) then
      begin
        NextBlock:= nbImages;
        break; // Images definition begins
      end;
      if ( s = _NF_EOF ) then
      begin
        FileExhausted := true;
        break; // END OF FILE
      end;


      if InRichText then
      begin
        { can only be TRUE if the file uses the new "LI-less" format,
          because we got the _NF_RTF token, above. Old format doesn't
          have this token, so InRichText is never true }
        if FPlainText then
          delete( s, 1, 1 ); // strip _NF_PLAINTEXTLEADER
        List.Add( s );
        continue;
      end;

      p := pos( '=', s );
      if ( p <> 3 ) then continue; // not a valid entry
      key := copy( s, 1, 2 );
      delete( s, 1, 3 );

      if ( key = _CHBGColor ) then
      begin
        try
          FEditorChrome.BGColor := StringToColor( s );
        except
        end;
      end
      else
      if ( key = _CHFontCharset ) then
      begin
        try
          FEditorChrome.Font.Charset := StrToInt( s );
        except
          FEditorChrome.Font.Charset := DEFAULT_CHARSET;
        end;
      end
      else
      if ( key = _CHFontColor ) then
      begin
        FEditorChrome.Font.Color := StringToColor( s );
      end
      else
      if ( key = _CHFontName ) then
      begin
        FEditorChrome.Font.Name := s;
      end
      else
      if ( key = _CHFontSize ) then
      begin
        try
          FEditorChrome.Font.Size := strtoint( s );
        except
          FEditorChrome.Font.Size := 10;
        end;
      end
      else
      if ( key = _CHFontStyle ) then
      begin
        FEditorChrome.Font.Style := StrToFontStyle( s );
      end
      else
      if ( key = _CHLanguage ) then
      begin
        try
          FEditorChrome.Language := strtoint( s );
        except
        end;
      end
      else
      if ( key = _DateCreated ) then
      begin
        try
          FDateCreated := strtodatetime( s );
        except
          FDateCreated := now;
        end;
      end
      else
      if ( key = _ImageIndex ) then
      begin
        try
          FImageIndex := strtoint( s );
        except
          FImageIndex := 0;
        end;
      end
      else
      {
      if ( key = _Info ) then
      begin
        try
          FInfo := strtoint( s );
        except
          FInfo := 0;
        end;
      end
      else
      }
      if ( key = _LineCount ) then
      begin
        try
          linecount := strtoint( s );
        except
          linecount := DEFAULT_CAPACITY;
        end;
        if ( List.Capacity < linecount ) then
          List.Capacity := succ( linecount );
      end
      else
      if ( key = _NoteName ) then
      begin
        FName := TryUTF8ToUnicodeString(s) ;
      end
      else
      if ( key = _NoteID ) then
      begin
        try
          FID := strtoint( s );
        except
          FID := 0; // owning file will generate new ID when note is added
        end;
      end
      else
      if ( key = _PosX ) then
      begin
        try
          FCaretPos.X := strtoint( s );
        except
          FCaretPos.X := 0;
        end;
      end
      else
      if ( key = _POSY ) then
      begin
        try
          FCaretPos.Y := strtoint( s );
        except
          FCaretPos.Y := 0;
        end;
      end
      else
      if ( key = _TabIndex ) then
      begin
        try
          FTabIndex := strtoint( s );
        except
          FTabIndex := 0;
        end;
      end
      else
      if ( key = _TabSize ) then
      begin
        try
          FTabSize := strtoint( s );
        except
          FTabSize := DEF_TAB_SIZE;
        end;
      end
      else
      if ( key = _Flags ) then
      begin
        FlagsStringToProperties( s );
      end
      else
      if ( key = _Lines ) then
      begin
        List.Add( s );
      end
      // [x] REMOVE the keys below for version 1.0;
      // these are only retained so that users of older
      // versions do not lose important note settings.
      // These properties are now stored in FlagsString
      else
      if ( key = _WordWrap ) then
      begin
        FWordWrap := ( s = BOOLEANSTR[true] );
      end
      else
      if ( key = _ReadOnly ) then
      begin
        FReadOnly := ( s = BOOLEANSTR[true] );
      end
      else
      if ( key = _URLDetect ) then
      begin
        FURLDetect := ( s = BOOLEANSTR[true] );
      end
      else if ( key = _NodeAlarm ) then
      begin
         ProcessAlarm(s);
      end;

    end; // eof( tf )

    TransferRTFData(List, FDataStream);

  finally
    List.Free;
  end;

  FModified := false;

end; // LoadFromFile


{$IFDEF WITH_DART}
procedure TTabNote.LoadDartNotesFormat( Stream : TStream );
var
  TextSize : longint;
  ds : string;
  ch : char;
  p, blocklen, rtfoffset : integer;
begin
  if ( not assigned( Stream )) then
    raise ETabNoteError.Create( STR_03 );

  with FDataStream do
  begin
    Clear;
    Position := 0;
  end;

  try
    ds := '';
    repeat
      Stream.ReadBuffer( ch, sizeof( ch ));
      if ( ch = _DART_STOP ) then break;
      ds := ds + ch;
    until ( Stream.Position = Stream.Size ); // safety check

    try
      blocklen := strtoint( ds );
      ds := '';
      repeat
        Stream.ReadBuffer( ch, sizeof( ch ));
        if ( ch = _DART_STOP ) then break;
        ds := ds + ch;
      until ( Stream.Position = Stream.Size ); // safety check
      rtfoffset := strtoint( ds );
      ds := '';
      SetLength( ds, rtfoffset );
      Stream.ReadBuffer( ds[1], rtfoffset );
      p := pos( _DART_STOP, ds );
      FName := copy( ds, 1, pred( p ));
      delete( ds, 1, p );
      p := pos( _DART_STOP, ds );
      FEditorChrome.BGColor := strtoint( copy( ds, 1, pred( p )));
      delete( ds, 1, p );
      if ( ds <> '' ) then
        FWordWrap := ( ds[1] = '1' );
      ds := '';
      repeat
        Stream.ReadBuffer( ch, sizeof( ch ));
        if ( ch = _DART_STOP ) then break;
        ds := ds + ch;
      until ( Stream.Position = Stream.Size ); // safety check
      TextSize := strtoint( ds );
      FDataStream.CopyFrom( Stream, TextSize );
    except
      on E : EConvertError do
      begin
        raise Exception.Create( STR_04 );
      end;
      on E : Exception do raise;
    end;

  finally
  end;

  FModified := false;

end; // LoadDartNotesFormat
{$ENDIF}


procedure TTabNote.SetReadOnly( AReadOnly : boolean );
begin
  if ( AReadOnly <> FReadOnly ) then
  begin
    FReadOnly := AReadOnly;
    FModified := true;
    if _ALLOW_VCL_UPDATES and assigned( FEditor ) then FEditor.ReadOnly := FReadOnly;
  end;
end; // TTabNote.SetReadOnly


procedure TTabNote.SetPlainText( APlainText : boolean );
begin
  if ( APlainText <> FPlainText ) then begin
    FPlainText := APlainText;
    FModified := true;
  end;
end;


function TTabNote.GetModified : boolean;
begin
  result := FModified;
  {
  if assigned( FEditor ) then
    result := result or FEditor.Modified;
  }
end; // GetModified

procedure TTabNote.SetModified( AModified : boolean );
begin
  FModified := AModified;
  if assigned( FEditor ) then
    FEditor.Modified := FModified;
end; // SetModified

procedure TTabNote.SetTabIndex( ATabIndex : integer );
begin
  if ( FTabIndex = ATabIndex ) then exit;
  FTabIndex := ATabIndex;
  if _ALLOW_VCL_UPDATES and assigned( FTabSheet ) then
    FTabSheet.PageIndex := FTabIndex;
end; // TTabNote.SetTabIndex

procedure TTabNote.SetImageIndex( AImgIdx : integer );
begin
  if ( FImageIndex = AImgIdx ) then exit;
  FImageIndex := AImgIdx;
  if _ALLOW_VCL_UPDATES and assigned( FTabSheet ) then
    FTabSheet.ImageIndex := FImageIndex;
end; // TTabNote.SetImgIdx


procedure TTabNote.SetImagesMode(ImagesMode: TImagesMode);
var
   RTFIn, RTFOut: AnsiString;
   currentNoteModified, currentFileModified: boolean;

begin
    if FImagesMode <> ImagesMode then begin
       FImagesMode:= ImagesMode;

       RTFIn:= Editor.RtfText;
       RTFOut:= ImagesManager.ProcessImagesInRTF(RTFIn, Self, ImagesMode, '', 0, true);
       if RTFOut <> '' then begin
          currentNoteModified:= FModified;
          currentFileModified:= NoteFile.Modified;
          Editor.PutRtfText(RTFout,True,False);
          if currentNoteModified <> FModified then begin
             FModified:=  currentNoteModified;  // <- false...
             NoteFile.Modified:= currentFileModified;
             UpdateNoteFileState( [fscModified] );
          end;

       end;
    end;
end;

procedure TTabNote.SetWordWrap( AWordWrap : boolean );
begin
  if ( FWordWrap = AWordWrap ) then exit;
  FWordWrap := AWordWrap;
  FModified := true;
  if _ALLOW_VCL_UPDATES and assigned( FEditor ) then
    FEditor.WordWrap := FWordWrap;
end; // TTabNote.SetWordWrap

procedure TTabNote.SetURLDetect( AURLDetect : boolean );
begin
  if ( FURLDetect = AURLDetect ) then exit;
  FURLDetect := AURLDetect;
  FModified := true;
  if _ALLOW_VCL_UPDATES and assigned( FEditor ) then
    FEditor.AutoURLDetect := FURLDetect;
end; // TTabNote.SetURLDetect

procedure TTabNote.SetTabSize( ATabSize : byte );
begin
  if ( FTabSize = ATabSize ) then exit;
  FTabSize := ATabSize;
  FModified := true;
  if _ALLOW_VCL_UPDATES and assigned( FEditor ) then
    FEditor.TabSize := FTabSize;
end; // TTabNote.SetTabSize


// ------------ TTabRichEdit --------------

constructor TTabRichEdit.Create( AOwner : TComponent );
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
  FNoteObj := nil;
  FAutoIndent := false;
  FUseTabChar := true;
  FTabSize := DEF_TAB_SIZE;
// FOnFileDropped := nil;
end; // TTabRichEdit CREATE

destructor TTabRichEdit.Destroy;
begin
  inherited Destroy;
end; // TTabRichEdit DESTROY

{
 NOTE:
  If DragAcceptFiles is not called from the RxRichEdit control, since we are doing it in the main form of the application (TForm_Main.CreateWnd)
  we would still be able to receive the WM_DROPFILES message, through the event handler defined within TForm_Main
  ( procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES; )

  But we will also call that method for the RichEdit control to have the option, if necessary, of being able to distinguish which control is receiving
  the file. Furthermore, before we were calling DragAcceptFiles from our derived class, TTabRichEdit, but we have started to call it directly from 
  RxRichEdit itself, in its CreateWnd method, to allow offering from that control, in an integrated way, a new event, OnFileDropped. That event will be
  launched from WM_DROPFILES, and in order to receive that message it must be enabled by calling DragAcceptFiles

  See more clarifications in comment *3 on RxRichEd.pas
}
{                                                                                             [dpv]
procedure TTabRichEdit.CreateWnd;
begin
  inherited;
  DragAcceptFiles( handle, true ); // [MJ]
end; // CreateWnd

procedure TTabRichEdit.DestroyWnd;
begin
  DragAcceptFiles( handle, false ); // [MJ]
  inherited;
end; // DestroyWnd
}

procedure TTabRichEdit.CMRecreateWnd(var Message: TMessage);
var
  FRTF: string;
  mySelStart, mySelLength : longint;
  PerformFix : boolean;
begin
  mySelStart := self.SelStart;
  mySelLength := self.SelLength;

  PerformFix := ( FRecreateWndProtect and ( WindowHandle <> 0 ));

  if PerformFix then
  begin
    FRTF:= Self.RtfText;
    Lines.Clear; // [!] if we do this...
  end;

  inherited;

  if PerformFix then
  begin
    Lines.BeginUpdate;
    try
      // Lines.Clear;
     {$IFDEF KNT_DEBUG}Log.Add('TTabRichEdit_CMRecreateWnd. PerformFix',  4 ); {$ENDIF}
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

procedure TTabRichEdit.CMDialogKey( var Message: TCMDialogKey );
begin
  // if (( Message.CharCode <> VK_TAB ) and ( Message.CharCode <> VK_RETURN )) then
  if ( Message.CharCode <> VK_TAB ) then
    inherited;
end; // CMDialogKey

(*
procedure TTabRichEdit.WMDropFiles(var Msg: TWMDropFiles);  // [MJ]
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

function TTabRichEdit.FontInfoString : string;
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


function TTabRichEdit.ParaInfoString : string;
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



function TTabRichEdit.GetWordAtCursor( const LeaveSelected : boolean; const IgnoreActualSelection: boolean = False;
                                          const DiscardKNTHiddenCharacters: boolean= True ) : string;
var
  L, R,  Rm, Offset: integer;
  SS, SL: integer;
  SSw: integer;
  Str: string;
  KeepSelected: boolean;


  function IsWordDelimiter(i: integer; const Str: string): boolean; inline;
  begin
    Exit ( (Str[i]<>KNT_RTF_HIDDEN_MARK_L_CHAR) and (Str[i]<>KNT_RTF_HIDDEN_MARK_R_CHAR) and not IsCharAlphaNumeric(Str[i]) );
  end;

begin
  SL:= SelLength;
  if (SL > 0) and not IgnoreActualSelection then Exit(SelText);

  SS:=  SelStart;
  L:= SS-40;
  if L < 0 then L:= 0;
  R:= SS + 40;

  BeginUpdate;

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

      if IsWordDelimiter(SSW, Str) then begin
        dec(SSw);
        Offset:= -1;
        if (SSw=0) or IsWordDelimiter(SSW, Str) then
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
      until (L<1) or IsWordDelimiter(L, Str);
      inc(L);

      R:= SSw;
      repeat
         if (Str[R]=KNT_RTF_HIDDEN_MARK_L_CHAR) then begin
            repeat
                inc(R);
            until (R>Rm) or (Str[R]=KNT_RTF_HIDDEN_MARK_R_CHAR);
         end;
         inc(R);
      until  (R>Rm) or IsWordDelimiter(R, Str);
      dec(R);

      Result:= Copy(Str, L, R-L +1);

  finally
      if DiscardKNTHiddenCharacters then
         Result:= kn_EditorUtils.RemoveKNTHiddenCharacters(Result);

      if KeepSelected then begin
         SelStart := SS-(SSw-L) + Offset;
         SelLength := R-L +1;
      end
      else begin
        SelStart:= SS;
        SelLength:= SL;
      end;

      EndUpdate;
  end;

end;   // GetWordAtCursor


procedure TTabRichEdit.HideKNTHiddenMarks(Selection: boolean = true);
var
   p, pF: integer;
   SS, SL: integer;
   Str: String;
begin
   // {\rtf1\ansi {\v\'11B5\'12} XXX };   {\rtf1\ansi \v\'11B5\'12\v0 XXX};  {\rtf1\ansi \v\'11T999999\'12\v0 XXX};   '#$11'B5'#$12'

  Str:= TextPlain(Selection);
  p:= Pos(KNT_RTF_HIDDEN_MARK_L_CHAR, Str, 1);
  if p = 0 then Exit;

  SS:= SelStart;
  SL:= SelLength;

  BeginUpdate;
  SuspendUndo;
  try
    repeat
       if p > 0 then begin
          pF:= Pos(KNT_RTF_HIDDEN_MARK_R_CHAR, Str, p+3);
          if (pF > 0) and (pF-p <= KNT_RTF_HIDDEN_MAX_LENGHT_CHAR) then begin
             SetSelection(p+SS-1, pF+SS, true);
             SelAttributes.Hidden:= True;
             p:= pF + 1;
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


procedure TTabRichEdit.RemoveKNTHiddenCharacters (selection: boolean= true);
var
  s: AnsiString;
  len: integer;
begin
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



// Returns True if there is one or more paragraphs selected (although partially)

function TTabRichEdit.ParagraphsSelected: Boolean;
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

procedure TTabRichEdit.GetLinkAtCursor(var URL: string; var TextURL: string;
                                       var LeftE: integer; var RightE: integer;
                                       SelectURL: boolean= true);
var
    link: TRxLinkStyle;    // lsNone: link style not set  lsLink: link style set    lsMixed: mixed                             
    TextLen: integer;
    Left, Right: integer;
    SelS, SelL: integer;
begin

  ActiveNote.Editor.BeginUpdate;
  try

    URL:= '';
    TextURL:= '';
    SelS:= ActiveNote.Editor.SelStart;
    SelL:= ActiveNote.Editor.SelLength;
    Left:= SelS;
    Right:= SelL + Left;

    ActiveNote.Editor.SetSelection(Left, Left+1, false);
    link:= ActiveNote.Editor.SelAttributes.LinkStyle;
    if link = lsLink then begin
       ActiveNote.Editor.SetSelection(Left-1, Left, false);
       link:= ActiveNote.Editor.SelAttributes.LinkStyle;
    end;

    if (link <> lsLink) then begin
        ActiveNote.Editor.SetSelection(Left, Right, false);
        LeftE:= Left;
        RightE:= Left;
    end
    else begin
        TextLen:= ActiveNote.Editor.TextLength;

        // Buscamos el extremo izquierdo

        { Character "H" of HYPERLINK ".... cannot be selected in versions >= 5. Also in that versiones, when trying to selection
          one hidden character selection of that hyperlink, the whole link is selected }
        link:= lsLink;
        while (Left >0) and (link=lsLink) and (ActiveNote.Editor.SelLength=1) do begin
              Left:= Left - 1;
              ActiveNote.Editor.SetSelection(Left-1, Left, false);
              link:= ActiveNote.Editor.SelAttributes.LinkStyle;
        end;

        if (ActiveNote.Editor.SelLength > 1) then begin
           Left:= ActiveNote.Editor.SelStart;
           Right:= Left + ActiveNote.Editor.SelLength;
           LeftE:= Left;
           RightE:= Right;
        end
        else begin
           LeftE:= Left;
           // Ampliamos la selección incluyendo el/los caracteres '<' que pueda haber
           while (LeftE >0) and (ActiveNote.Editor.SelText='<') do begin
                 LeftE:= LeftE - 1;
                 ActiveNote.Editor.SetSelection(LeftE-1, LeftE, false);
           end;


           // Buscamos el extremo derecho
           link:= lsLink;
           while (Right < TextLen) and (link=lsLink) do begin
                 Right:= Right + 1;
                 ActiveNote.Editor.SetSelection(Right, Right+1, false);
                 link:= ActiveNote.Editor.SelAttributes.LinkStyle;
           end;

           RightE:= Right;
           // Ampliamos la selección incluyendo el/los caracteres '>' que pueda haber
           while (Right < TextLen) and (ActiveNote.Editor.SelText='>') do begin
                 RightE:= RightE + 1;
                 ActiveNote.Editor.SetSelection(RightE, RightE+1, false);
           end;
        end;

        URL:= ActiveNote.Editor.GetTextRange(Left, Right);
        if pos('HYPERLINK "', URL)= 1 then begin
           // If it is an hyperlink with text associated then the string will be: URL"TextURL, where only TextURL is visible
           ActiveNote.Editor.SetSelection(Left, Right, false);
           TextURL:= ActiveNote.Editor.SelVisibleText;
           URL:= Copy(URL, 12, Length(URL) - Length(TextURL)- 12);
        end
        else
          if SelectURL then
              ActiveNote.Editor.SetSelection(LeftE, RightE, false);

        if not SelectURL then
           ActiveNote.Editor.SetSelection(SelS, SelS + SelL, false);

    end;

  finally
     ActiveNote.Editor.EndUpdate;
  end;
end;

procedure TTabRichEdit.SimulateDoubleClick;
var
  pt : TPoint;
begin
  { does NOT work?}
  perform( EM_POSFROMCHAR, WPARAM(@pt), selstart );
  perform( WM_LBUTTONDBLCLK, 0,
    MakeLParam( pt.x, pt.y ));
  perform( WM_LBUTTONUP, 0,
    MakeLParam( pt.x, pt.y ));
end; // SimulateDoubleClick

// ------------ TTreeNote --------------

constructor TTreeNote.Create;
begin
  inherited Create;
  FKind := ntTree;
  FSplitter := nil;
  FTV := nil;
  FTreeWidth := 0; // flag, so that default width will be used
  FIconKind := niStandard;
  FCheckboxes := false;
  FSelectedNode := nil;
  FTreeHidden := false;
  FHideCheckedNodes:= false;  // [dpv]
  FFocusMemory := focTree; // initially focus tree
  FOldSelectedIndex := -1;
  FAutoNumberNodes := false;
  FVerticalLayout := false;
  InitializeChrome( FTreeChrome );
  FDefaultNodeName := DEFAULT_NEW_NODE_NAME;
  FNodes := TNodeList.Create;

  {$IFDEF WITH_IE}
  FMainPanel := nil;
  FWebBrowser := nil;
  // FGrid := nil;
  // FGridSplitter := nil;
  {$ENDIF}
end; // CREATE

destructor TTreeNote.Destroy;
begin
  try
    if assigned( FNodes ) then FNodes.Free;
    FHistory.Free;
  except
  end;

  inherited destroy;
end; // DESTROY

function TTreeNote.PropertiesToFlagsString : TFlagsString;
begin
  result := inherited PropertiesToFlagsString;

  // values 1-12 are reserved for TTabNote
  result[TREENOTE_FLAG_BASE+1] := AnsiChar(inttostr( ord( FIconKind ))[1]);
  result[TREENOTE_FLAG_BASE+2] := BOOLEANSTR[FAutoNumberNodes];
  result[TREENOTE_FLAG_BASE+3] := BOOLEANSTR[FCheckboxes];
  result[TREENOTE_FLAG_BASE+4] := BOOLEANSTR[FVerticalLayout];

  // added in 1.5.9:
  result[TREENOTE_FLAG_BASE+5] := BOOLEANSTR[FTreeHidden];
  result[TREENOTE_FLAG_BASE+6] := BOOLEANSTR[(( not FTreeHidden ) and ( FFocusMemory = focTree ))];

  // added in 1.7.0:          // [dpv]
  result[TREENOTE_FLAG_BASE+7] := BOOLEANSTR[FHideCheckedNodes];

end; // PropertiesToFlagsString

procedure TTreeNote.FlagsStringToProperties( const FlagsStr : TFlagsString );
begin
  inherited FlagsStringToProperties( FlagsStr );
  if ( length( FlagsStr ) < FLAGS_STRING_LENGTH ) then exit;
  case FlagsStr[TREENOTE_FLAG_BASE+1] of
    '0' : FIconKind := niNone;
    '1' : FIconKind := niStandard;
    '2' : FIconKind := niCustom;
  end;
  FAutoNumberNodes := FlagsStr[TREENOTE_FLAG_BASE+2] = BOOLEANSTR[true];
  FCheckboxes      := FlagsStr[TREENOTE_FLAG_BASE+3] = BOOLEANSTR[true];
  FVerticalLayout  := FlagsStr[TREENOTE_FLAG_BASE+4] = BOOLEANSTR[true];

  FTreeHidden := FlagsStr[TREENOTE_FLAG_BASE+5] = BOOLEANSTR[true];
  if (( not FTreeHidden ) and ( FlagsStr[TREENOTE_FLAG_BASE+6] = BOOLEANSTR[true] )) then
    FFocusMemory := focTree
  else
    FFocusMemory := focRTF;

  FHideCheckedNodes      := FlagsStr[TREENOTE_FLAG_BASE+7] = BOOLEANSTR[true];    // [dpv]

end; // FlagsStringToProperties


function TTreeNote.GetCount : integer;
begin
  if assigned( FNodes ) then
    result := FNodes.Count
  else
    result := 0;
end; // GetNodeCount

function TTreeNote.GetSelectedNode : TNoteNode;
begin
  result := FSelectedNode;
end; // GetSelectedNode

procedure TTreeNote.SetSelectedNode( aNode : TNoteNode );
begin
  FSelectedNode := aNode;
end; // SetSelectedNode


function TTreeNote.NewNode(
  const AParent : TNoteNode;
  AName : string;
  const AInheritProperties : boolean ) : TNoteNode;
var
  myNode : TNoteNode;
begin
  myNode := TNoteNode.Create;
  myNode.Name := AName;
  myNode.RTFBGColor := FEditorChrome.BGColor;

  if assigned( APArent ) then
  begin
    myNode.Level := succ( AParent.Level ); // else Node is already initialized to Level 0
    if AInheritProperties then
    begin
      myNode.Checked := AParent.Checked;
      myNode.Bold := AParent.Bold;
      myNode.HasNodeColor := AParent.HasNodeColor;
      myNode.HasNodeBGColor := APArent.HasNodeBGColor;
      myNode.NodeColor := AParent.NodeColor;
      myNode.NodeBGColor := AParent.NodeBGColor;
      myNode.ImageIndex := AParent.ImageIndex;
      // [x] inherit other properties here? (flagged?)
    end;
  end;

  AddNode( myNode );
  result := myNode;
end; // NewNode

function TTreeNote.AddNode( const aNode : TNoteNode ) : integer;
begin
  FModified := true;
  result := InternalAddNode( aNode );
  if (( result >= 0 ) and ( aNode.ID = 0 )) then
    GenerateNodeID( aNode );
end; // AddNode

procedure TTreeNote.InsertNode( const aIndex : integer; const aNode : TNoteNode );
begin
  FModified := true;
  InternalInsertNode( aIndex, aNode );
  if ( aNode.ID = 0 ) then
    GenerateNodeID( aNode );
end; // InternalInsertNode

procedure TTreeNote.InternalInsertNode( const aIndex : integer; const aNode : TNoteNode );
begin
  if assigned( aNode ) then
    FNodes.Insert( aIndex, aNode );
end; // InternalInsertNode

function TTreeNote.InternalAddNode( const aNode : TNoteNode ) : integer;
begin
  if assigned( aNode ) then
    result := FNodes.Add( aNode )
  else
    result := -1;
end; // InternalAddNode

procedure TTreeNote.GenerateNodeID( const aNode : TNoteNode );
var
  i, count, myID, hiID : longint;
  myNode : TNoteNode;
begin
  myID := 0;
  hiID := 0;

  Count := FNodes.Count;
  for i := 1 to Count do
  begin
    myNode := FNodes[pred( i )];
    if ( myNode.ID > hiID ) then
      hiID := myNode.ID; // find highest note ID
  end;

  inc( hiID ); // make it one higher
  ANode.ID := hiID;

end; // GenerateNodeID

procedure TTreeNote.VerifyNodeIDs;
var
  i, count : longint;
  myNode : TNoteNode;
begin
  count := FNodes.Count;
  for i := 1 to count do
  begin
    myNode := FNodes[pred( i )];
    if ( myNode.ID <= 0 ) then
      GenerateNodeID( myNode );
  end;
end; // VerifyNodeIDs

procedure TTreeNote.RemoveNode( const aNode : TNoteNode );
begin
  if ( not assigned( ANode )) then exit;

  NoteFile.RemoveImagesCountReferences(aNode);

  FNodes.Remove( aNode );
  FModified := true;
end;

procedure TTreeNote.SetTreeChrome( AChrome : TChrome );
begin
  FTreeChrome := AChrome;
  FModified := true;
end; // SetTreeChrome

procedure TTreeNote.SetTreeProperties( const aProps : TNoteTreeProperties );
begin
  FCheckboxes := aProps.CheckBoxes;
  FIconKind := aProps.IconKind;
  FDefaultNodeName := aProps.DefaultName;
  FAutoNumberNodes := aProps.AutoNumberNodes;
  FVerticalLayout := aProps.VerticalLayout;
  FHideCheckedNodes := aProps.HideChecked;           // [dpv]
end; // SetTreeProperties

procedure TTreeNote.GetTreeProperties( var aProps : TNoteTreeProperties );
begin
  aProps.CheckBoxes := FCheckboxes;
  aProps.IconKind := FIconKind;
  aProps.DefaultName := FDefaultNodeName;
  aProps.AutoNumberNodes := FAutoNumberNodes;
  aProps.VerticalLayout := FVerticalLayout;
  aProps.HideChecked:= FHideCheckedNodes;          // [dpv]
end; // GetTreeProperties

procedure TTreeNote.UpdateEditor;
var
  TextLen: integer;
  SS, SL: integer;

begin
  if ( not CheckEditor ) then exit;


  FEditor.BeginUpdate;
  try
      inherited UpdateEditor;

        if assigned(FSelectedNode) then begin
           case FSelectedNode.WordWrap of
              wwAsNote : FEditor.WordWrap := FWordWrap;
              wwYes : FEditor.WordWrap := true;
              wwNo : FEditor.WordWrap := false;
           end;

           FEditor.Color := FSelectedNode.RTFBGColor;
        end;


  finally
     FEditor.EndUpdate;
  end;
end; // UpdateEditor


procedure TTreeNote.UpdateTree;
begin
  // CheckTree;
  // FTV.Items.BeginUpdate;
  try
    FTV.Color := FTreeChrome.BGColor;
    with FTreeChrome.Font do
    begin
      FTV.Font.Name := Name;
      FTV.Font.Size := Size;
      FTV.Font.Style := Style;
      FTV.Font.Charset := Charset;
      FTV.Font.Color := Color;
    end;
  finally
    // FTV.Items.EndUpdate;
  end;
end; // UpdateTree


procedure TTreeNote.DataStreamToEditor;
var
  ReadOnlyBAK: boolean;
{$IFDEF WITH_IE}
  ov : OleVariant;
{$ENDIF}

{$IFDEF KNT_DEBUG}
 str: String;
 dataSize: integer;
{$ENDIF}

 strRTF: AnsiString;
 ContainsImgIDsRemoved: boolean;
 ContainsImages: boolean;

begin
  if not assigned(FEditor) then exit;
  if not assigned(FSelectedNode) then  begin
    FEditor.Clear;
    exit;
  end;

  case FSelectedNode.VirtualMode of
    vmNone, vmText, vmRTF, vmHTML, vmKNTNode : begin
      FEditor.BeginUpdate;
      ReadOnlyBAK:= FEditor.ReadOnly;
      ContainsImgIDsRemoved:= false;
      try
        FEditor.OnChange := nil;
        FEditor.ReadOnly:= false;   // To prevent the problem indicated in issue #537
        FEditor.Clear;

        FSelectedNode.Stream.Position := 0;
        strRTF:= '';

        if ( FPlainText or ( FSelectedNode.VirtualMode in [vmText, vmHTML] )) then
           UpdateEditor;

        fImagesReferenceCount:= nil;
        if NodeStreamIsRTF (FSelectedNode.Stream) then begin
           FEditor.StreamFormat:= sfRichText;
           if (ImagesManager.StorageMode <> smEmbRTF) and (FSelectedNode.VirtualMode in [vmNone, vmKNTNode]) and (not FPlainText) then begin
              GetImagesIDInstances (FSelectedNode.Stream, FSelectedNode.NodeTextPlain);
              strRTF:= ImagesManager.ProcessImagesInRTF(FSelectedNode.Stream.Memory, FSelectedNode.Stream.Size, Self, ImagesManager.ImagesMode, '', 0, ContainsImgIDsRemoved, ContainsImages, true);
           end;
        end
        else
           FEditor.StreamFormat:= sfPlainText;

        Log_StoreTick('TTreeNote.DataStreamToEditor - BEGIN', 4, +1);
       {$IFDEF KNT_DEBUG}
        if log.Active and  (log.MaxDbgLevel >= 4) then begin
           dataSize:= FSelectedNode.Stream.Size;
           str:= Copy(String(PAnsiChar(FSelectedNode.Stream.Memory)), 1, 250);
           Log.Add(string.format('sfRichText?:%s DataSize:%d  RTF:"%s"...', [BoolToStr(FEditor.StreamFormat=sfRichText), dataSize, str]),  4 );
        end;
       {$ENDIF}

        if StrRTF <> '' then begin
           FEditor.PutRtfText(strRTF,True,False);               // => ImageManager.StorageMode <> smEmbRTF
           FEditor.ClearUndo;
        end
        else
           FEditor.Lines.LoadFromStream( FSelectedNode.Stream );

        Log_StoreTick('TTreeNote.DataStreamToEditor - END', 4, -1);

        FEditor.Color := FSelectedNode.RTFBGColor;
        FEditor.SelStart := FSelectedNode.SelStart;
        FEditor.SelLength := FSelectedNode.SelLength;

        if FSelectedNode.Stream.Size = 0 then     // Ensures that new nodes are correctly updated based on default properties (font color, size, ...)
           UpdateEditor;

      finally
        FEditor.ReadOnly:= ReadOnlyBAK;
        FEditor.EndUpdate;
        if not ContainsImgIDsRemoved then
           FEditor.Modified := false;
        FEditor.OnChange := Form_Main.RxRTFChange;
      end;
    end;
    vmIELocal, vmIERemote : begin
      {$IFDEF WITH_IE}
      ov := 0;
      FWebBrowser.Navigate( FSelectedNode.VirtualFN, ov, ov, ov, ov );
      {$ENDIF}
    end;

  end;

end; // DataStreamToEditor

{
  If Editor was modified then it will return the Stream associated to the node that will be updated
}
function TTreeNote.EditorToDataStream: TMemoryStream;
var
   KeepUTF8: boolean;
   Encoding: TEncoding;
   strRTF: AnsiString;
   ImagesIDs_New: TImageIDs;
   TextPlain: string;
begin
  Result:= nil;
  Encoding:= nil;
  if assigned(FEditor) and assigned(FSelectedNode) then begin
     FSelectedNode.SelStart  := FEditor.SelStart;
     FSelectedNode.SelLength := FEditor.SelLength;

     if FEditor.Modified then begin
        FEditor.Lines.BeginUpdate;
        try
           KeepUTF8:= False;
           if (FSelectedNode.VirtualMode in [vmText, vmHTML]) and NodeStreamIsUTF8WithBOM(FSelectedNode.Stream) then
               KeepUTF8:= True;

           FModified := FModified or FEditor.Modified;
           FSelectedNode.Stream.Clear;

           try
             FEditor.StreamFormat:= StreamFormatInNode();
             FEditor.StreamMode := [];
             if FEditor.StreamFormat = sfPlainText then begin
                // Si es un nodo virtual respetaremos la codificación UTF8 que pueda tener.
                // En caso contrario sólo se guardará como UTF8 si es necesario
                if KeepUTF8 or not CanSaveAsANSI(FEditor.Text) then
                   //FEditor.StreamMode := [smUnicode];
                   Encoding:= TEncoding.UTF8;
             end;

             FEditor.Lines.SaveToStream( FSelectedNode.Stream, Encoding);

             ImagesIDs_New:= nil;
             if (ImagesManager.StorageMode <> smEmbRTF) and (FSelectedNode.VirtualMode in [vmNone, vmKNTNode]) and (FEditor.StreamFormat = sfRichText) then begin
                ImagesIDs_New:= CheckSavingImagesOnMode (imLink, FSelectedNode.Stream, true);
                ImagesManager.UpdateImagesCountReferences (fImagesReferenceCount, ImagesIDs_New);
                fImagesReferenceCount:= ImagesIDs_New;
             end;

             if ImagesIDs_New = nil then
                FSelectedNode.NodeTextPlain:= FEditor.TextPlain
             else begin
                { If the node has images we will make sure that in TextPlain we save the version corresponding to imLink,
                  to facilitate search management. See notes on TImageManager.GetSearchOffset }
                FSelectedNode.NodeTextPlain := '';
                InitializeTextPlain(FSelectedNode, RTFAux_Note);
             end;
             FSelectedNode.Stream.Position := 0;
             Result:= FSelectedNode.Stream;
             FEditor.Modified:= false;

           finally
             FEditor.StreamFormat := sfRichText;
             FEditor.StreamMode := [];
           end;


        finally
          FEditor.Lines.EndUpdate;
        end;
     end
     else
       if (FSelectedNode.NodeTextPlain = '') then
          InitializeTextPlain(FSelectedNode, RTFAux_Note);

  end;
end; // EditorToDataStream


function TTreeNote.StreamFormatInNode: TRichStreamFormat;
begin
    case FSelectedNode.VirtualMode of
      vmNone, vmKNTNode : begin
        if FPlainText then
          Result:= sfPlainText
        else
          Result:= sfRichText;
      end;
      vmText, vmHTML : begin
        Result:= sfPlainText;
      end;
      vmRTF : begin
        Result:= sfRichText;
      end;
    end;

end;

function TTreeNote.CheckTree : boolean;
begin
  result := assigned( FTV );
end; // CheckTree


function TTreeNote.SaveToFile( var tf : TTextFile; OnlyCurrentNodeAndSubtree: TTreeNTNode= nil;
                               OnlyNotHiddenNodes: boolean= false; OnlyCheckedNodes: boolean= false ): integer;
var
  treenode : TTreeNTNode;
  notenode : TNoteNode;
  nodessaved, NodeCnt, NodeIdx : integer;
  wasmismatch : boolean;
  HaveVCLControls : boolean;
  // bakFN : string;
begin
  HaveVCLControls := CheckTree;
  nodessaved := 0;

  // sanity check

  wasmismatch := ( HaveVCLControls and (( FTV.Items.Count ) <> ( FNodes.Count )));
  if wasmismatch then begin
     if ( DoMessageBox(Format(STR_05, [FName,FTV.Items.Count,FNodes.Count]),
                       Format(STR_06, [FName]), MB_YESNO+MB_ICONEXCLAMATION+MB_DEFBUTTON1+MB_APPLMODAL ) <> ID_YES ) then
        raise ETabNoteError.Create(STR_07);
  end;


  if HaveVCLControls then begin
    if FVerticalLayout then
       FTreeWidth := FTV.Height
    else
       FTreeWidth := FTV.Width;

    if assigned( FTV.Selected ) then
       FOldSelectedIndex := FTV.Selected.AbsoluteIndex
    else
       FOldSelectedIndex := -1;
  end;



  try
    tf.WriteLine( _NF_TreeNote ); // marks beginning of TTreeNote
    BaseSaveProc( tf );

    // basic treenote properties
    tf.WriteLine( _SelectedNode + '=' + FOldSelectedIndex.ToString );
    tf.WriteLine( _TreeWidth + '=' + FTreeWidth.ToString );
    tf.WriteLine( _DefaultNodeName + '=' + FDefaultNodeName,  True);

    // tree chrome
    tf.WriteLine( _CHTRBGColor + '=' + ColorToString( FTreeChrome.BGColor ) );
    tf.WriteLine( _CHTRFontCharset + '=' + intToStr( FTreeChrome.Font.Charset ) );
    tf.WriteLine( _CHTRFontColor + '=' + ColorToString( FTreeChrome.Font.Color ) );
    tf.WriteLine( _CHTRFontName + '=' + FTreeChrome.Font.Name );
    tf.WriteLine( _CHTRFontSize + '=' + FTreeChrome.Font.Size.ToString );
    tf.WriteLine( _CHTRFontStyle + '=' + FontStyleToStr( FTreeChrome.Font.Style ) );

    NodeCnt := FNodes.Count;
    NodeIdx := 0;

    notenode := nil;
    treenode := nil; // initialize to eliminate compiler warning

    // obtain first node
    if HaveVCLControls then  begin
       if OnlyCurrentNodeAndSubtree <> nil then
          treenode := OnlyCurrentNodeAndSubtree
       else
          treenode := FTV.Items.GetFirstNode;
       if assigned( treenode ) then begin
          notenode := TNoteNode( treenode.data );
          if assigned( notenode ) then
             notenode.Level := treenode.Level;
       end;
    end
    else begin
       if ( NodeCnt > 0 ) then
          notenode := FNodes[0];
    end;

    while assigned( notenode ) do begin
      if not ( (OnlyCheckedNodes   and not notenode.Checked) or
               (OnlyNotHiddenNodes and Self.GetTreeNodeByID(notenode.ID).Hidden) )  then begin

          inc( nodessaved );

          tf.WriteLine( _NF_TRN  );
          tf.WriteLine( _NodeLevel + '=' + notenode.Level.ToString );

          tf.WriteLine( _NodeName + '=' + notenode.Name, True);
          tf.WriteLine( _NodeID + '=' + notenode.ID.ToString  );
          tf.WriteLine( _NodeFlags + '=' + notenode.PropertiesToFlagsString);
          tf.WriteLine( _NodeRTFBGColor + '=' + ColorToString(notenode.RTFBGColor) );
          tf.WriteLine( _NodeImageIndex + '=' + notenode.ImageIndex.ToString  );
          if noteNode.HasNodeColor then
             tf.WriteLine( _NodeColor + '=' + ColorToString(noteNode.NodeColor) );
          if noteNode.HasNodeBGColor then
             tf.WriteLine( _NodeBGColor + '=' + ColorToString(noteNode.NodeBGColor) );
          if noteNode.HasNodeFontFace then
             tf.WriteLine( _NodeFontFace + '=' + noteNode.NodeFontFace );

          if (NoteNode.VirtualMode <> vmKNTNode) and (noteNode.HasAlarms(true)) then
             SaveAlarms(tf, noteNode);

          if ( _SAVE_RESTORE_CARETPOS and ( notenode.SelStart > 0 )) then
             tf.WriteLine( _NodeSelStart + '=' + notenode.SelStart.ToString  );



          if (NoteNode.VirtualMode = vmNone ) then
             SaveRTFToFile(tf, notenode.Stream, FPlainText)

          else
          if NoteNode.VirtualMode = vmKNTNode  then
             tf.WriteLine( _VirtualNode + '=' + notenode.MirrorNodeID  )

          else begin
            if notenode.HasVNodeError then
               // there was an error when we tried to load this file, so don't try to save it (assume no valid data in node)
                tf.WriteLine( _VirtualFN + '=' + copy( notenode.VirtualFN, 2, length( notenode.VirtualFN )), True )

            else
                try
                   NoteNode.SaveVirtualFile;

                   tf.WriteLine( _RelativeVirtualFN + '=' + notenode.RelativeVirtualFN, True  ); // MUST be done AFTER NoteNode.SaveVirtualFile. MUST also be saved BERFORE notenode.VirtualFN.
                   tf.WriteLine( _VirtualFN + '=' + notenode.VirtualFN, True  );
                except
                  on E : Exception do
                    // [x] A note may have hundreds of nodes.We should allow user to ABORT here or to skip subsequent error messages
                    DoMessageBox(Format(STR_08 + #13+ '%s'+ #13#13+ '%s', [notenode.Name, self.Name, notenode.VirtualFN, E.Message]), mtError, [mbOK], 0 );
                end;
          end;

      end;

      // obtain next node, or bail out if NIL
      notenode := nil;
      if HaveVCLControls then  begin
         treenode := treenode.GetNext;
         if OnlyCurrentNodeAndSubtree <> nil then begin
             if (OnlyCurrentNodeAndSubtree.GetNextSibling = treeNode) then
                treeNode := nil;
         end;
         if assigned( treenode ) then begin
            notenode := TNoteNode( treenode.data );
            if assigned( notenode ) then
               notenode.Level := treenode.Level;
         end;
      end
      else begin
         inc( NodeIdx );
         if ( NodeIdx < NodeCnt ) then
            notenode := FNodes[NodeIdx];
      end;
    end;

    Modified := false;

  finally
    if (OnlyCurrentNodeAndSubtree = nil) and not OnlyCheckedNodes and not OnlyNotHiddenNodes
       and ( nodessaved <> FNodes.Count ) then
        raise ETabNoteError.CreateFmt(STR_09, [FNodes.Count, nodessaved]);

    Result:= nodesSaved;
  end;

end; // SaveToFile


procedure TTreeNote.LoadFromFile( var tf : TTextFile; var FileExhausted : boolean; var NextBlock: TNextBlock);
var
  InRichText : boolean;
  InNoteNode : boolean;
  List : TStringList;
  s, key : AnsiString;
  p, linecount : integer;
  myNode : TNoteNode;

    procedure AddNewNode;
    begin
      // transfer RTF data from list to node
      InRichText := false;
      TransferRTFData(List, myNode.Stream);
      myNode.Stream.Position := 0;
      List.Clear;
      InternalAddNode( myNode );
      myNode := nil;
    end; // AddNewNode

begin

  FileExhausted := false;
  InRichText := false;
  InNoteNode := false;
  myNode := nil; // DO NOT REMOVE! (Otherwise we get an AV when loading a tree with zero nodes)

  List := TStringList.Create;
  List.BeginUpdate;
  try
    while ( not tf.eof()) do
    begin
          s:= tf.readln();

          if ( s = _NF_RTF ) then
          begin
            // RTF data begins
            InRichText := true;
            continue;
          end;
          if ( s = _NF_TRN ) then
          begin
            // new NoteNode begins
            if ( InNoteNode ) then AddNewNode; // we were here previously, i.e. there a node to be added
            InNoteNode := true;
            // create a new blank node
            myNode := TNoteNode.Create;
            myNode.RTFBGColor := EditorCHrome.BGColor;
            continue;
          end;
          if ( s = _NF_TabNote ) then
          begin
            NextBlock:= nbRTF;
            if assigned( myNode ) then AddNewNode;
            break; // New TabNote begins
          end;
          if ( s = _NF_TreeNote ) then
          begin
            NextBlock:= nbTree;
            if ( myNode <> nil ) then
              AddNewNode;
            break; // New TreeNote begins
          end;
          if ( s = _NF_StoragesDEF ) then
          begin
            NextBlock:= nbImages;
            if ( myNode <> nil ) then
              AddNewNode;
            break; // Images definition begins
          end;
          if ( s = _NF_EOF ) then
          begin
            FileExhausted := true;
            if assigned( myNode ) then AddNewNode;
            break; // END OF FILE
          end;


          if InRichText then
          begin
            { can only be TRUE if the file uses the new "LI-less" format,
              because we got the _NF_EOH token, above. Old format doesn't
              have this token, so InRichText is never true }
            if FPlainText then
              delete( s, 1, 1 ); // strip _NF_PLAINTEXTLEADER
            List.Add( s );
            continue;
          end;


          p := pos( '=', s );
          if ( p <> 3 ) then continue; // not a valid key=value format
          key := copy( s, 1, 2 );
          delete( s, 1, 3 );

          if InNoteNode then
          begin
                if ( key = _NodeName ) then
                begin
                  myNode.Name := TryUTF8ToUnicodeString(s);
                end
                else
                if ( key = _NodeID ) then
                begin
                  try
                    myNode.ID := strtoint( s );
                  except
                    myNode.ID := 0;
                  end;
                end
                else
                if ( key = _NodeLevel ) then
                begin
                  try
                    myNode.Level := StrToInt( s );
                  except
                    myNode.Level := 0;
                  end;
                end
                else
                if ( key = _NodeFlags ) then
                begin
                  myNode.FlagsStringToProperties( s );
                end
                else
                if ( key = _NodeRTFBGColor ) then
                begin
                  try
                    myNode.RTFBGColor := StringToColor( s );
                  except
                  end;
                end
                else
                if ( key = _VirtualNode ) then
                begin
                  myNode.MirrorNodeID := TryUTF8ToUnicodeString(s);
                end
                else
                if ( key = _RelativeVirtualFN ) then
                begin
                  myNode.RelativeVirtualFN := TryUTF8ToUnicodeString(s);
                end
                else
                if ( key = _VirtualFN ) then
                begin
                  myNode.VirtualFN := TryUTF8ToUnicodeString(s);
                  try
                    myNode.LoadVirtualFile;
                  except
                    on E : Exception do
                    begin
                      List.Add( STR_10 );
                      List.Add( myNode.VirtualFN );
                      List.Add( E.Message );
                      myNode.VirtualFN := _VIRTUAL_NODE_ERROR_CHAR + myNode.VirtualFN;
                    end;
                  end;
                end
                else
                if ( key = _NodeSelStart ) then
                begin
                  try
                    if _SAVE_RESTORE_CARETPOS then
                      myNode.SelStart := StrToInt( s )
                    else
                      myNode.SelStart := 0;
                  except
                    myNode.SelStart := 0;
                  end;
                end
                else
                if ( key = _NodeImageIndex ) then
                begin
                  try
                    myNode.ImageIndex := StrToInt( s );
                  except
                    myNode.ImageIndex := -1;
                  end;
                end
                else
                if ( key = _NodeColor ) then
                begin
                  try
                    myNode.NodeColor := StringToColor( s );
                  except
                    myNode.HasNodeColor := false;
                  end;
                end
                else
                if ( key = _NodeBGColor ) then
                begin
                  try
                    myNode.NodeBGColor := StringToColor( s );
                  except
                    myNode.HasNodeBGColor := false;
                  end;
                end
                else
                if ( key = _NodeFontFace ) then
                begin
                  myNode.NodeFontFace := s;
                end
                else
                if ( key = _NodeAlarm ) then      // [dpv*]
                begin
                    ProcessAlarm(s, myNode);
                end;
                continue;
          end; // if InNoteNode ...


          if ( key = _SelectedNode ) then
          begin
              try
                FOldSelectedIndex := StrToInt( s );
              except
                FOldSelectedIndex := -1;
              end;
          end
          else
          if ( key = _TreeWidth ) then
          begin
              try
                FTreeWidth := StrToInt( s );
              except
                FTreeWidth := 0;
              end;
          end
          else
          if ( key = _DefaultNodeName ) then
          begin
              if ( s <> '' ) then
                 FDefaultNodeName := TryUTF8ToUnicodeString(s);
          end
          else
          if ( key = _CHTRBGColor ) then
          begin
              try
                FTreeChrome.BGColor := StringToColor( s );
              except
              end;
          end
          else
          if ( key = _CHTRFontCharset ) then
          begin
              try
                FTreeChrome.Font.Charset := StrToInt( s );
              except
                FTreeChrome.Font.Charset := DEFAULT_CHARSET;
              end;
          end
          else
          if ( key = _CHTRFontColor ) then
          begin
              FTreeChrome.Font.Color := StringToColor( s );
          end
          else
          if ( key = _CHTRFontName ) then
          begin
              FTreeChrome.Font.Name := s;
          end
          else
          if ( key = _CHTRFontSize ) then
          begin
              try
                FTreeChrome.Font.Size := strtoint( s );
              except
                FTreeChrome.Font.Size := 10;
              end;
          end
          else
          if ( key = _CHTRFontStyle ) then
          begin
              FTreeChrome.Font.Style := StrToFontStyle( s );
          end
          else
          if ( key = _CHBGColor ) then
          begin
              try
                FEditorChrome.BGColor := StringToColor( s );
              except
              end;
          end
          else
          if ( key = _CHFontCharset ) then
          begin
              try
                FEditorChrome.Font.Charset := StrToInt( s );
              except
                FEditorChrome.Font.Charset := DEFAULT_CHARSET;
              end;
          end
          else
          if ( key = _CHFontColor ) then
          begin
              FEditorChrome.Font.Color := StringToColor( s );
          end
          else
          if ( key = _CHFontName ) then
          begin
              FEditorChrome.Font.Name := s;
          end
          else
          if ( key = _CHFontSize ) then
          begin
              try
                FEditorChrome.Font.Size := strtoint( s );
              except
                FEditorChrome.Font.Size := 10;
              end;
          end
          else
          if ( key = _CHFontStyle ) then
          begin
              FEditorChrome.Font.Style := StrToFontStyle( s );
          end
          else
          if ( key = _CHLanguage ) then
          begin
              try
                FEditorChrome.Language := strtoint( s );
              except
              end;
          end
          else
          if ( key = _DateCreated ) then
          begin
              try
                FDateCreated := strtodatetime( s );
              except
                FDateCreated := now;
              end;
          end
          else
          if ( key = _ImageIndex ) then
          begin
              try
                FImageIndex := strtoint( s );
              except
                FImageIndex := 0;
              end;
          end
          else
          {
          if ( key = _Info ) then
          begin
            try
              FInfo := strtoint( s );
            except
              FInfo := 0;
            end;
          end
          else
          }
          if ( key = _LineCount ) then
          begin
              try
                linecount := strtoint( s );
              except
                linecount := DEFAULT_CAPACITY;
              end;
              if ( List.Capacity < linecount ) then
                List.Capacity := succ( linecount );
          end
          else
          if ( key = _NoteName ) then
          begin
              FName := TryUTF8ToUnicodeString(s);
          end
          else
          if ( key = _NoteID ) then
          begin
              try
                FID := strtoint( s );
              except
                FID := 0; // owning file will generate new ID when note is added
              end;
          end
          else
          if ( key = _Flags ) then
          begin
              FlagsStringToProperties( s );
          end
          else
          if ( key = _PosX ) then
          begin
              try
                FCaretPos.X := strtoint( s );
              except
                FCaretPos.X := 0;
              end;
          end
          else
          if ( key = _POSY ) then
          begin
              try
                FCaretPos.Y := strtoint( s );
              except
                FCaretPos.Y := 0;
              end;
          end
          else
          if ( key = _TabIndex ) then
          begin
              try
                FTabIndex := strtoint( s );
              except
                FTabIndex := 0;
              end;
          end
          else
          if ( key = _TabSize ) then
          begin
              try
                FTabSize := strtoint( s );
              except
                FTabSize := DEF_TAB_SIZE;
              end;

          // [x] REMOVE the keys below for version 1.0;
          // these are only retained so that users of older
          // versions do not lose important note settings.
          // Thse properties are now stored in FlagsString
          (*
          end
          else
          if ( key = _ShowIcons ) then
          begin
            FShowIcons := ( s = BOOLEANSTR[true] );
          end
          else
          if ( key = _WordWrap ) then
          begin
            FWordWrap := ( s = BOOLEANSTR[true] );
          end
          else
          if ( key = _ReadOnly ) then
          begin
            FReadOnly := ( s = BOOLEANSTR[true] );
          end
          else
          if ( key = _URLDetect ) then
          begin
            FURLDetect := ( s = BOOLEANSTR[true] );
          *)
          end
          else
          if ( key = _NodeAlarm ) then
          begin
              ProcessAlarm(s);
          end;


    end; { while not eof( tf ) }

  finally
    VerifyNodeIDs;
    List.EndUpdate;
    List.Free;
  end;

  FModified := false;

end; // LoadFromFile


procedure TTreeNote.LoadFromTreePadFile( const FN : string );
var
  s, nodeName : string;
  tf : TTextFile;
  InNode : boolean;
  level : integer;
  myNode : TNoteNode;
  List : TStringList;

    procedure AddNewNode;
    begin
      // transfer RTF data from list to node
      List.WriteBOM:= False;
      List.SaveToStream( myNode.Stream );
      myNode.Stream.Position := 0;
      List.Clear;
      InternalAddNode( myNode );
      myNode := nil;
    end; // AddNewNode

begin
  InNode := false;

  if ( not fileexists( FN )) then exit;

  tf:= TTextFile.Create();
  tf.AssignFile(FN);

  try
    tf.Reset;
  except
    messagedlg( STR_11 + FN, mtError, [mbOK], 0 );
    exit;
  end;


  List := TStringList.Create;

  try

    while ( not tf.Eof) do begin
       s:= tf.readln;

       case InNode of

         false : begin
            if ( s = _TREEPAD_NODE ) then begin
              InNode := true;
              try
                nodeName:= TryUTF8ToUnicodeString(tf.Readln);
                s:= tf.Readln; // node level
                try
                  level := StrToInt( s );
                except
                  level := 0;
                end;
                myNode := TNoteNode.Create;
                myNode.RTFBGColor := EditorChrome.BGColor;
                myNode.Level := Level;
                myNode.Name := nodeName;
              except
                InNode := false;
              end;
              continue;
            end;
         end;

         true : begin
            if ( s = _TREEPAD_ENDNODE ) then begin
               InNode := false;
               AddNewNode;
               Continue;
            end;
            List.Add( s );
        end;

      end;

    end;

  finally
    VerifyNodeIDs;
    List.Free;
    tf.CloseFile;
  end;

end; // LoadFromTreePadFile


function TTreeNote.GetNodeByID( const aID : integer ) : TNoteNode;
var
  myTreeNode : TTreeNTNode;
begin
  myTreeNode := GetTreeNodeByID( aID );
  if assigned( myTreeNode ) then
    result := TNoteNode( myTreeNode.Data )
  else
    result := nil;
end; // GetNodeByID


function TTreeNote.GetTreeNodeByID( const aID : integer ) : TTreeNTNode;
var
  myTreeNode : TTreeNTNode;
begin
  result := nil;
  myTreeNode := FTV.Items.GetFirstNode;
  while assigned( myTreeNode ) do
  begin
    if assigned( myTreeNode.Data ) then
    begin
      if ( TNoteNode( myTreeNode.Data ).ID = aID ) then
      begin
        result := myTreeNode;
        break;
      end;
    end;
    myTreeNode := myTreeNode.GetNext;
  end;
end; // GetTreeNodeByID


procedure LoadStreamInRTFAux(Stream: TMemoryStream; RTFAux: TRxRichEdit);
begin
    Stream.Position := 0;
    RTFAux.Clear;
    RTFAux.StreamMode := [];

    if NodeStreamIsRTF (Stream) then
       RTFAux.StreamFormat:= sfRichText
    else
       RTFAux.StreamFormat:= sfPlainText;
    try
       RTFAux.Lines.LoadFromStream( Stream );
    except
    end;
end;


function TTabNote.InitializeTextPlain(RTFAux: TRxRichEdit): boolean;
begin
    Result:= False;  // Initialization was required?

    if NoteTextPlain = '' then begin
       if (FImagesMode = imLink) and (not Editor.Modified) then
          NoteTextPlain:= Editor.TextPlain
       else begin
          LoadStreamInRTFAux (DataStream, RTFAux);
          NoteTextPlain:= RTFAux.TextPlain;
       end;
       Result:= True;
    end;
end;


function TTreeNote.InitializeTextPlain(myNoteNode: TNoteNode; RTFAux: TRxRichEdit): boolean;
begin
    Result:= False;  // Initialization was required?

    if myNoteNode.NodeTextPlain = '' then begin
       LoadStreamInRTFAux (myNoteNode.Stream, RTFAux);
       myNoteNode.NodeTextPlain:= RTFAux.TextPlain;
       Result:= True;
    end;
end;


function TTreeNote.InitializeTextPlainVariables( nMax: integer; RTFAux: TTabRichEdit): boolean;
var
  i, N: integer;
begin
  Result:= false;          // Returns True if all nodes have TextPlain initialized

  N:= 0;
  for i := 0 to FNodes.Count - 1 do  begin
     if (i mod 20) = 0 then begin
        Application.ProcessMessages;
        if (MillisecondsIdle <= 450) then Exit;
     end;

     if InitializeTextPlain (FNodes[i], RTFAux) then
        inc (N);

     if N >= nMax then Exit;
  end;

  Result:= true;
end;


Initialization

end.
