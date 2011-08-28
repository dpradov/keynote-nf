unit kn_NoteObj;

(* ************************************************************
 KEYNOTE: MOZILLA PUBLIC LICENSE STATEMENT.
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is KeyNote 1.0.

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 Released: 30 June 2001
 -----------------------------------------------------------
 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <cicho@tenbit.pl>

************************************************************ *)

interface
uses Windows, Classes, Graphics,
  SysUtils, IniFiles, FileCtrl, WideStrings,
  controls, comctrls95, gf_misc,
  ComCtrls,
  Dialogs, Messages,
  Extctrls,
  TreeNT,
  langs,
  kn_NodeList,
  ShellAPI,
  RichEdit,
  RxRichEd,
  kn_StyleObj,
  kn_Info,
  kn_Const,
  kn_History,
  {$IFDEF WITH_IE}
  SHDocVw_TLB,
  {$ENDIF}
  kn_RTFUtils,
  gf_streams, TntClasses;


type
  ETabNoteError = class( Exception );

const
  _NEW_NOTE_KIND : TNoteType = ntRTF;  // global, passed between this unit and kn_FileObj

type
  TkntParaAttributes = record
    FirstIndent : Longint;
    LeftIndent : Longint;
    LineSpacing : longint;
    LineSpacingRule : TLineSpacingRule;
    Alignment : TParaAlignment;
    Numbering : TRxNumbering;
    NumberingStyle : TRxNumberingStyle;
    NumberingTab : word;
    RightIndent : Longint;
    SpaceAfter : Longint;
    SpaceBefore : Longint;
    TabCount : integer;
    // [x] Tab[index : integer]
  end;

type
  TkntFontAttributes = record
    Charset : TFontCharset;
    BackColor : TColor;
    Color : TColor;
    Disabled : boolean;
    Hidden : boolean;
    Link : boolean;
    Name : TFontName;
    Offset : integer;
    Pitch : TFontPitch;
    IsProtected : boolean;          // Protected-> IsProtected  //*1
    RevAuthorIndex : byte;
    SubscriptStyle : TSubscriptStyle;
    Size : integer;
    Style : TFontStyles;
    Height : integer;
    Language : TLanguage;
    UnderlineType : TUnderlineType;
  end;

type
  TkntNumberingStyle = (
    numArabic, numLoCaseLetter, numUpCaseLetter,
    numLoCaseRoman, numUpCaseRoman
  );
  TkntOutlineFormat = record
    NumStyle : TkntNumberingStyle;
    StrBefore : string;
    StrAfter : string;
    StartNum : integer;
    IncludeParentLevel : boolean;
  end;

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
    FDataStream : TTntMemoryStream;
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

    // events
    FOnChange : TNotifyEvent;

    procedure SetName( AName : TNoteNameStr );
    procedure SetID( AID : longint );
    procedure SetReadOnly( AReadOnly : boolean );
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

    procedure BaseSaveProc( var tf : TWTextFile );

    function PropertiesToFlagsString : TFlagsString; virtual;
    procedure FlagsStringToProperties( const FlagsStr : TFlagsString ); virtual;

    procedure SaveAlarms(var tf : TWTextFile; node: TNoteNode = nil);
    procedure ProcessAlarm (s: WideString; node: TNoteNode = nil);

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

    property PlainText : boolean read FPlainText write FPlainText;
    property WordWrap : boolean read FWordWrap write SetWordWrap;
    property URLDetect : boolean read FURLDetect write SetURLDetect;
    property TabSize : byte read FTabSize write SetTabSize;
    property UseTabChar : boolean read FUseTabChar write FUseTabChar;
    property IsInsertMode : boolean read FIsInsertMode write FIsInsertMode;
    property CaretPos : TPoint read FCaretPos write FCaretPos;
    property FocusMemory : TFocusMemory read FFocusMemory write FFocusMemory;

    property TabSheet : TTab95Sheet read FTabSheet write SetTabSheet;

    property DataStream : TTntMemoryStream read FDataStream;

    // events
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

    constructor Create;
    destructor Destroy; override;

    procedure SaveToFile( var tf : TWTextFile ); virtual;
    procedure SaveDartNotesFormat( Stream : TStream ); virtual;

    procedure LoadFromFile( var tf : TWTextFile; var FileExhausted : boolean ); virtual;
    procedure LoadDartNotesFormat( Stream : TStream ); virtual;

    procedure SetEditorProperties( const aProps : TNoteEditorProperties );
    procedure GetEditorProperties( var aProps : TNoteEditorProperties );

    procedure SetTabProperties( const aProps : TNoteTabProperties );
    procedure GetTabProperties( var aProps : TNoteTabProperties );

    procedure UpdateEditor; virtual;
    procedure UpdateTabSheet;

    procedure DataStreamToEditor; virtual;
    procedure EditorToDataStream; virtual;


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
    FOnFileDropped : TFileDroppedEvent;

    procedure CMDialogKey( var Message: TCMDialogKey ); message CM_DIALOGKEY;
    // procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;

  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure CMRecreateWnd(var Message: TMessage); message CM_RECREATEWND;

  public
    property NoteObj : TTabNote read FNoteObj write FNoteObj;
    property AutoIndent : boolean read FAutoIndent write FAutoIndent;
    property UseTabChar : boolean read FUseTabChar write FUseTabChar;
    property TabSize : byte read FTabSize write FTabSize;
    property RecreateWndProtect : boolean read FRecreateWndProtect write FRecreateWndProtect;
    property OnFileDropped : TFileDroppedEvent read FOnFileDropped write FOnFileDropped;

    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;

    procedure SimulateDoubleClick; // unused

    function FontInfoString : string;
    function ParaInfoString : string;

    function GetWordAtCursorNew( const LeaveSelected : boolean; const IgnoreActualSelection: boolean = False  ) : WideString;
    function SelectWordAtCursor : string;
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

    FDefaultNodeName : WideString;
    FAutoNumberNodes : boolean;
    FCheckboxes : boolean;
    FVerticalLayout : boolean;

    FHistory : TKNTHistory;

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
    property DefaultNodeName : WideString read FDefaultNodeName write FDefaultNodeName;
    property AutoNumberNodes : boolean read FAutoNumberNodes write FAutoNumberNodes;
    property VerticalLayout : boolean read FVerticalLayout write FVerticalLayout;
    property TreeHidden : boolean read FTreeHidden write FTreeHidden;

    property History : TKNTHistory read FHistory;

    {$IFDEF WITH_IE}
    property MainPanel : TPanel read FMainPanel write FMainPanel;
    property WebBrowser : TWebBrowser read FWebBrowser write FWebBrowser;
    // property Grid : TStringAlignGrid read FGrid write FGrid;
    // property GridSplitter : TSplitter read FGridSplitter write FGridSplitter;
    {$ENDIF}

    constructor Create;
    destructor Destroy; override;

    procedure SaveToFile( var tf : TWTextFile ); override;
    procedure LoadFromFile( var tf : TWTextFile; var FileExhausted : boolean ); override;

    function NewNode( const AParent : TNoteNode; AName : WideString; const AInheritProperties : boolean ) : TNoteNode;
    function AddNode( const aNode : TNoteNode ) : integer;
    procedure InsertNode( const aIndex : integer; const aNode : TNoteNode );
    procedure RemoveNode( const aNode : TNoteNode );

    procedure DataStreamToEditor; override;
    procedure EditorToDataStream; override;
    procedure SetTreeProperties( const aProps : TNoteTreeProperties );
    procedure GetTreeProperties( var aProps : TNoteTreeProperties );

    procedure UpdateEditor; override;
    procedure UpdateTree;

    procedure LoadFromTreePadFile( const FN : string );

    function GetNodeByID( const aID : integer ) : TNoteNode;
    function GetTreeNodeByID( const aID : integer ) : TTreeNTNode;


  end;

function LoadedRichEditVersion : integer;
procedure ParaAttrsRX2KNT( const RxFmt : TRxParaAttributes; var KntFmt : TkntParaAttributes );
procedure ParaAttrsKNT2RX( const KntFmt : TkntParaAttributes; const RxFmt : TRxParaAttributes );
procedure FontAttrsRX2KNT( const RxFmt : TRxTextAttributes; var KntFmt : TkntFontAttributes );
procedure FontAttrsKNT2RX( const KntFmt : TkntFontAttributes; const RxFmt : TRxTextAttributes );

var
  _LoadedRichEditVersion : integer;

implementation
uses kn_global, kn_AlertMng, kn_LinksMng, kn_Main, gf_strings, gf_miscvcl, WideStrUtils;

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


function LoadedRichEditVersion : integer;
var
  ModuleHandle : HMODULE;
  ModulePath : array[0..MAX_PATH] of char;
  s : string;
  dummy : DWORD;
  size : integer;
  buffer : PChar;
  vsInfo : PVSFixedFileInfo;
  vsInfoSize : UINT;
  FileVersionMinor : integer;

// return value:
//      -1 - internal failure.
//       0 - no RichEdit DLL is loaded.
//       1 - RichEdit 1.0 is loaded.
//       2 - RichEdit 2.0 is loaded.
//       3 - RichEdit 3.0 is loaded.
//       4 - RichEdit 4.0 is loaded.
begin


  result := RichEditVersion; // get from RxRichEd.pas initially

  // try RE 2.0/3.0/4.0 DLL
  ModuleHandle := GetModuleHandle( 'riched20.dll' );
  if ( ModuleHandle = 0 ) then
  begin
    // try RE 1.0 DLL
    ModuleHandle := GetModuleHandle( 'riched32.dll' );
    if ( ModuleHandle <> 0 ) then
      result := 1;
    exit; // possibly return 1, obtained above
  end;

  // get the path to the DLL
  if ( GetModuleFileName( ModuleHandle, ModulePath, MAX_PATH ) = 0 ) then exit; // function failed
  s := ModulePath;

  // get size of version information
  size := getfileversioninfosize( PChar( s ), dummy );
  if ( size = 0 ) then exit; // function failed

  GetMem( buffer, size );

  try
    if ( not GetFileVersionInfo( ModulePath, 0, size, buffer )) then exit;

    if ( not VerQueryValue(
        buffer, '\', pointer( vsInfo ), vsInfoSize )) then exit;

    FileVersionMinor := loword( vsInfo^.dwFileVersionMS );
    {
      Richedit 2: 0
      RichEdit 3: 30
      RichEdit 4: 40
    }


    case FileVersionMinor of
      0 : result := 2;
      30 : result := 3;
      40 : result := 4;
      else // who knows what the future will bring :)
        if ( FileVersionMinor > 40 ) then
          result := FileVersionMinor div 10
        else
          result := 3; // fairly safe assumption by now
    end;

  finally
    FreeMem( buffer, size );
  end;

end; // LoadedRichEditVersion

procedure ParaAttrsRX2KNT( const RxFmt : TRxParaAttributes; var KntFmt : TkntParaAttributes );
begin
  with RxFmt do
  begin
    KntFmt.FirstIndent := FirstIndent;
    KntFmt.LeftIndent := LeftIndent;
    KntFmt.LineSpacing := LineSpacing;
    KntFmt.LineSpacingRule := LineSpacingRule;
    KntFmt.Alignment := Alignment;
    KntFmt.Numbering := Numbering;
    KntFmt.NumberingStyle := NumberingStyle;
    KntFmt.NumberingTab := NumberingTab;
    KntFmt.RightIndent := RightIndent;
    KntFmt.SpaceAfter := SpaceAfter;
    KntFmt.SpaceBefore := SpaceBefore;
    KntFmt.TabCount := TabCount;
    // [x] Tab[index : integer]
  end;
end; // ParaAttrsRX2KNT

procedure ParaAttrsKNT2RX( const KntFmt : TkntParaAttributes; const RxFmt : TRxParaAttributes );
begin
  with KntFmt do
  begin
    RxFmt.FirstIndent := FirstIndent;
    RxFmt.LeftIndent := LeftIndent;
    RxFmt.LineSpacing := LineSpacing;
    RxFmt.LineSpacingRule := LineSpacingRule;
    RxFmt.Alignment := Alignment;
    RxFmt.Numbering := Numbering;
    RxFmt.NumberingStyle := NumberingStyle;
    RxFmt.NumberingTab := NumberingTab;
    RxFmt.RightIndent := RightIndent;
    RxFmt.SpaceAfter := SpaceAfter;
    RxFmt.SpaceBefore := SpaceBefore;
    RxFmt.TabCount := TabCount;
    // [x] Tab[index : integer]
  end;
end; // ParaAttrsKNT2RX

procedure FontAttrsRX2KNT( const RxFmt : TRxTextAttributes; var KntFmt : TkntFontAttributes );
begin
  with RxFmt do
  begin
    KntFmt.Charset := Charset;
    KntFmt.BackColor := BackColor;
    KntFmt.Color := Color;;
    KntFmt.Disabled := Disabled;
    KntFmt.Hidden := Hidden;
    KntFmt.Link := Link;
    KntFmt.Name := Name;
    KntFmt.Offset := Offset;
    KntFmt.Pitch := Pitch;
    KntFmt.IsProtected := IsProtected;
    KntFmt.RevAuthorIndex := RevAuthorIndex;
    KntFmt.SubscriptStyle := SubscriptStyle;
    KntFmt.Size := Size;
    KntFmt.Style := Style;
    KntFmt.Height := Height;
    KntFmt.UnderlineType := UnderlineType;
    KntFmt.Language := Language;
  end;
end; // FontAttrsRX2KNT

procedure FontAttrsKNT2RX( const KntFmt : TkntFontAttributes; const RxFmt : TRxTextAttributes );
begin
  with KNTFmt do
  begin
    RxFmt.Charset := Charset;
    RxFmt.BackColor := BackColor;
    RxFmt.Color := Color;
    RxFmt.Disabled := Disabled;
    RxFmt.Hidden := Hidden;
    RxFmt.Link := Link;
    RxFmt.Name := Name;
    RxFmt.Offset := Offset;
    RxFmt.Pitch := Pitch;
    RxFmt.IsProtected := IsProtected;
    RxFmt.RevAuthorIndex := RevAuthorIndex;
    RxFmt.SubscriptStyle := SubscriptStyle;
    RxFmt.Size := Size;
    RxFmt.Style := Style;
    RxFmt.Height := Height;
    RxFmt.UnderlineType := UnderlineType;
    RxFmt.Language := Language;
  end;
end; // FontFormatKNT2RX



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
  FDataStream := TTntMemoryStream.Create;
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
{
var
  tabstopcnt : integer;
}
begin
  if ( not CheckEditor ) then exit;
  FEditor.Lines.BeginUpdate;
  try
    FEditor.WordWrap := FWordWrap;
    FEditor.TabSize := FTabSize;
    FEditor.AutoURLDetect := FURLDetect;

    FEditor.Color := FEditorChrome.BGColor;
    with FEditor.DefAttributes do
    begin
      Charset := FEditorChrome.Font.Charset;
      Name := FEditorChrome.Font.Name;
      Size := FEditorChrome.Font.Size;
      Style := FEditorChrome.Font.Style;
      Color := FEditorChrome.Font.Color;
      Language := FEditorChrome.Language;
    end;
    if ( FEditor.Lines.Count = 0 ) then // IMOPRTANT!
    begin
      FEditor.SelectAll;
      FEditor.SelAttributes.Assign( FEditor.DefAttributes );

      {
      FEditor.Paragraph.TabCount := 8; // max is 32, but what the hell
      for tabstopcnt := 0 to 7 do
        FEditor.Paragraph.Tab[tabstopcnt] := (tabstopcnt+1) * (2*FTabSize); // [x] very rough!
      }

      FEditor.SelLength := 0;
    end;
    FEditor.UseTabChar := FUseTabChar;
    FEditor.ReadOnly := FReadOnly;
  finally
    FEditor.Lines.EndUpdate;
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

procedure TTabNote.SetTabProperties( const aProps : TNoteTabProperties );
begin
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

procedure TTabNote.DataStreamToEditor;
begin
  if CheckEditor then
  begin
    FEditor.OnChange := nil;
    FDataSTream.Position := 0;

    if NodeStreamIsRTF (FDataStream) then
       FEditor.StreamFormat:= sfRichText
    else begin
       FEditor.StreamFormat:= sfPlainText;
       AddUTF8_BOM (FDataStream);
    end;

    FEditor.Lines.LoadFromStream( FDataStream );
    FDataStream.Clear;
    FEditor.OnChange := Form_Main.RxRTFChange;
  end;
end; // DataStreamToEditor

procedure TTabNote.EditorToDataStream;
var
   NodeTextW: wideString;
begin
  if CheckEditor then
  begin
    try
      with FDataStream do
      begin
        Clear;
        Position := 0;
      end;
      FEditor.StreamMode := [];
      if FPlainText then begin
        FEditor.StreamFormat := sfPlainText;
        NodeTextW:= FEditor.TextW;
        if NodeTextW <> string(NodeTextW) then
           FEditor.StreamMode := [smUnicode];
        end
      else
        FEditor.StreamFormat := sfRichText;

      CleanHyperlinks;
      FEditor.Lines.SaveToStream( FDataStream );
    finally
      FEditor.StreamFormat := sfRichText;
      FEditor.StreamMode := [];
    end;
  end;
end; // EditorToDataStream

function TTabNote.CheckEditor : boolean;
begin
  result := assigned( FEditor );
end; // CheckEditor

function TTabNote.CheckTabSheet : boolean;
begin
  result := assigned( FTabSheet );
end; // CheckTabSheet

procedure TTabNote.BaseSaveProc( var tf : TWTextFile );
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

  tf.writeln( [_NoteName, '=', FName ] );
  tf.writeln( [_NoteID, '=', FID ] );
  tf.writeln( [_ImageIndex, '=', FImageIndex ] );
  tf.writeln( [_DateCreated, '=', FormatDateTime( _SHORTDATEFMT + #32 + _LONGTIMEFMT, FDateCreated )] );
  tf.writeln( [_TabIndex, '=', FTabIndex ] );
  tf.writeln( [_TabSize, '=', inttostr( FTabSize ) ] );

  tf.writeln( [_PosX, '=', FCaretPos.X ] );
  tf.writeln( [_PosY, '=', FCaretPos.Y ] );
  tf.writeln( [_CHBGColor, '=', ColorToString( FEditorChrome.BGColor ) ] );
  tf.writeln( [_CHFontCharset, '=', inttostr( FEditorChrome.Font.Charset ) ] );
  tf.writeln( [_CHFontColor, '=', ColorToString( FEditorChrome.Font.Color ) ] );
  tf.writeln( [_CHFontName, '=', FEditorChrome.Font.Name ] );
  tf.writeln( [_CHFontSize, '=', FEditorChrome.Font.Size ] );
  tf.writeln( [_CHLanguage, '=', FEditorChrome.Language ] );
  tf.writeln( [_CHFontStyle, '=', FontStyleToStr( FEditorChrome.Font.Style ) ] );
  tf.writeln( [_Flags, '=', PropertiesToFlagsString ] );

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
procedure TTabNote.SaveAlarms(var tf : TWTextFile; node: TNoteNode = nil);
var
   I: integer;
   Alarms: TList;
   s: WideString;
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
           s:= s + '|' + WideStringReplace(alarm.AlarmNote, #13#10, 'ªª', [rfReplaceAll]);
        s:= FormatDateTime( _SHORTDATEFMT + #32 + _LONGTIMEFMT, alarm.AlarmReminder ) + s;
        if alarm.Status = TAlarmDiscarded then
           s:= 'D' + s;
        tf.writeln( [_NodeAlarm, '=', s ] );

        I:= I + 1;
     end;

  except
  end;
end;

procedure TTabNote.SaveToFile( var tf : TWTextFile );
var
  List : TStringList;
  cnt, i : integer;
  HaveVCLControls : boolean;
begin

  HaveVCLControls := CheckEditor;

  if ( FKind <> ntRTF ) then
    raise ETabNoteError.Create(
      STR_01
    );

  List := TStringList.Create;
  try

    tf.writeln( [_NF_TabNote] ); // marks beginning of TTabNote
    BaseSaveProc( tf );


    if HaveVCLControls then
    begin
      EditorToDataStream;
      cnt := FEditor.Lines.Count;
      tf.writeln( [_LineCount, '=', cnt ]);
      if ( cnt < 10000 ) then
        List.Capacity := succ( cnt );
    end;

    FDataStream.Position := 0;

    List.LoadFromStream( FDataStream );
    cnt := pred( list.Count );

    case _USE_OLD_KEYNOTE_FILE_FORMAT of
      false : begin
        if ( cnt >= 0 ) then // if list is empty, cnt is -1
        begin
          tf.writeln( [_NF_RTF] );
          if FPlainText then
          begin
            for i := 0 to cnt do
              tf.writeln( [_NF_PLAINTEXTLEADER, List[i]] );
          end
          else
          begin
            // FPlaintext property not supported
            for i := 0 to cnt do
              tf.writeln( [List[i] ]);
          end;
        end;
      end;
      true : begin
        if ( cnt > 0 ) then
        begin
          for i := 0 to cnt do
            tf.writeln( [_Lines, '=', List[i]] );
        end;
      end;
    end;
  finally
    List.Free;
    if HaveVCLControls then
      FDataStream.Clear;
  end;

  Modified := false; // triggers SetModified

end; // TTabNote SaveToFile

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
procedure TTabNote.ProcessAlarm (s: WideString; node: TNoteNode = nil);
var
    alarm: TAlarm;
    p, p2: integer;
    format: string;
begin
   try

      alarm:= TAlarm.Create;

      p := Pos( '|', s );
      if ( p > 0 ) then begin
          alarm.AlarmNote:= WideStringReplace(TryUTF8ToWideString(copy(s, p+1, length(s))), 'ªª', #13#10, [rfReplaceAll]);
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


procedure TTabNote.LoadFromFile( var tf : TWTextFile; var FileExhausted : boolean );
var
  List : TStringList;
  s, key : string;
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
        _NEW_NOTE_KIND := ntRTF;
        break; // New TabNote begins
      end;
      if ( s = _NF_TreeNote ) then
      begin
        _NEW_NOTE_KIND := ntTree;
        break; // New TreeNote begins
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
        FName := TryUTF8ToWideString(s) ;
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

    if ( List.Count > 0 ) then
    begin
      List.SaveToStream( FDataStream );
    end;

  finally
    List.Free;
  end;

  FModified := false;

end; // LoadFromFile


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

procedure TTabNote.SetReadOnly( AReadOnly : boolean );
begin
  if ( AReadOnly <> FReadOnly ) then
  begin
    FReadOnly := AReadOnly;
    FModified := true;
    if _ALLOW_VCL_UPDATES and assigned( FEditor ) then FEditor.ReadOnly := FReadOnly;
  end;
end; // TTabNote.SetReadOnly

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
  FOnFileDropped := nil;
end; // TTabRichEdit CREATE

destructor TTabRichEdit.Destroy;
begin
  inherited Destroy;
end; // TTabRichEdit DESTROY

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
    FRTF := GetRichText(Self, true, false);
    Lines.Clear; // [!] if we do this...
  end;

  inherited;

  if PerformFix then
  begin
    Lines.BeginUpdate;
    try
      // Lines.Clear;
      PutRichText( FRTF, Self, true, true );
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

function TTabRichEdit.SelectWordAtCursor : string;
{ by Peter Below, TeamB }
var
  i : integer;
begin
  result := '';
  i:= perform( em_findwordbreak, WB_ISDELIMITER, selstart );
  if i <> 0 then
  begin
    // no word at caret
    exit;
  end
  else
  begin
    i := perform( em_findwordbreak, WB_MOVEWORDLEFT, selstart );
    selstart := i;
    i := perform( em_findwordbreak, WB_MOVEWORDRIGHT, selstart );
    While (perform( em_findwordbreak, WB_ISDELIMITER, i ) <> 0) and
          (i > SelStart)
    Do Dec(i);
    sellength := i - selstart;
    result := seltext;
  end;
end; // SelectWordAtCursor

(*
  LeaveSelected = False => Preserve actual selected text
  IgnoreActualSelection: If word at cursor is "example" and it selected "ample" then if this paremeter is set to false
      the result will be "example" and not "ample"
*)
function TTabRichEdit.GetWordAtCursorNew( const LeaveSelected : boolean; const IgnoreActualSelection: boolean = False ) : WideString;
var
  P : TPoint;
  LineLen,  NewSelStart, startidx, wordlen : integer;

  lineWStr: WideString;
  posFirstChar: integer;
begin
  // a better version of WordAtCursor property

  if ( SelLength > 0 ) and not IgnoreActualSelection then
  begin
    result := SelText;
    exit;
  end;

  NewSelStart := SelStart + SelLength;

  wordlen := 0;
  p := CaretPos;
  LineLen := length( Lines[p.y] );

  if ( LineLen <> 0 ) then begin

      posFirstChar:= Perform( EM_LINEINDEX,p.y,0 );
      lineWStr:= GetTextRange(posFirstChar, posFirstChar + lineLen );

      if ( p.x = 0 ) then
      begin
        p.x := 1; // beginning of line
        inc( NewSelStart );
      end;
      if (lineWStr[p.x] = ' ') or (lineWStr[p.x] = #9) then begin
          p.x:= p.x + 1;
          NewSelStart:= NewSelStart + 1;
      end;
      startidx := p.x;

      while (( startidx > 0 ) and ( startidx <= LineLen )) do
      begin
        if IsCharAlphaNumericW( lineWStr[startidx] ) then
        begin
          dec( startidx );
          dec( NewSelStart );
          inc( wordlen );
        end
        else
          break;
      end;

      while ( p.x < LineLen ) do
      begin
        if IsCharAlphaNumericW( lineWStr[p.x] ) then
        begin
          inc( p.x );
          inc( wordlen );
        end
        else
          break;
      end;

      if ( wordlen > 0 ) then
      begin
          Result:= Copy(lineWStr,NewSelStart-posFirstChar+1,wordlen);
          if ( not IsCharAlphaNumericW( result[wordlen] )) then begin
              dec(wordlen);
              SetLength(Result,wordlen);
          end;
          if LeaveSelected then begin
             SelStart := NewSelStart;
             SelLength := wordlen;
          end;
      end;

  end;

end; // GetWordAtCursorNew


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

  FHistory := TKNTHistory.Create;

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
  result[TREENOTE_FLAG_BASE+1] := inttostr( ord( FIconKind ))[1];
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
  AName : WideString;
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
begin
  if ( not CheckEditor ) then exit;

  FEditor.Lines.BeginUpdate;
  try
    if assigned( FSelectedNode ) then
    begin
      case FSelectedNode.WordWrap of
        wwAsNote : FEditor.WordWrap := FWordWrap;
        wwYes : FEditor.WordWrap := true;
        wwNo : FEditor.WordWrap := false;
      end;
    end
    else
      FEditor.WordWrap := FWordWrap;

    FEditor.TabSize := FTabSize;
    FEditor.AutoURLDetect := FURLDetect;

    if ( not assigned( FSelectedNode )) then
      FEditor.Color := FEditorChrome.BGColor;
    with FEditor.DefAttributes do
    begin
      Charset := FEditorChrome.Font.Charset;
      Name := FEditorChrome.Font.Name;
      Size := FEditorChrome.Font.Size;
      Style := FEditorChrome.Font.Style;
      Color := FEditorChrome.Font.Color;
      Language := FEditorChrome.Language;
    end;

    FEditor.Font.Name := FEditorChrome.Font.Name;
    FEditor.Font.Charset := FEditorChrome.Font.Charset;
    FEditor.Font.Size := FEditorChrome.Font.Size;
    FEditor.Font.Style := FEditorChrome.Font.Style;
    FEditor.Font.Color := FEditorChrome.Font.Color;

    if ( FEditor.Lines.Count = 0 ) then
    begin
      FEditor.SelectAll; // IMOPRTANT!
      FEditor.SelAttributes.Assign( FEditor.DefAttributes );
      FEditor.SelLength := 0;
    end;
    FEditor.UseTabChar := FUseTabChar;
    FEditor.ReadOnly := FReadOnly;
  finally
    FEditor.Lines.EndUpdate;
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
{$IFDEF WITH_IE}
var
  ov : OleVariant;
{$ENDIF}
begin
  if ( not CheckEditor ) then exit;
  if ( not assigned( FSelectedNode )) then
  begin
    FEditor.Lines.Clear;
    exit;
  end;

  case FSelectedNode.VirtualMode of
    vmNone, vmText, vmRTF, vmHTML, vmKNTNode : begin
      FEditor.Lines.BeginUpdate;
      try
        FEditor.OnChange := nil;
        FEditor.Lines.Clear;
        if assigned( FSelectedNode ) then
        begin
          FSelectedNode.Stream.Position := 0;
          if ( FPlainText or ( FSelectedNode.VirtualMode in [vmText, vmHTML] )) then
          begin
            UpdateEditor;
          end;

          if NodeStreamIsRTF (FSelectedNode.Stream) then
             FEditor.StreamFormat:= sfRichText
          else begin
             FEditor.StreamFormat:= sfPlainText;
             AddUTF8_BOM (FSelectedNode.Stream);
          end;

          FEditor.Lines.LoadFromStream( FSelectedNode.Stream );
          FEditor.Color := FSelectedNode.RTFBGColor;
          FEditor.SelStart := FSelectedNode.SelStart;
          FEditor.SelLength := FSelectedNode.SelLength;
        end;
      finally
        FEditor.Lines.EndUpdate;
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

procedure TTreeNote.EditorToDataStream;
var
   NodeTextW: wideString;
begin
  if CheckEditor then
  begin
    FEditor.Lines.BeginUpdate;
    try
      if assigned( FSelectedNode ) then
      begin
        FSelectedNode.SelStart := FEditor.SelStart;
        FSelectedNode.SelLength := FEditor.SelLength;
        FModified := FModified or FEditor.Modified;
        FSelectedNode.Stream.Clear;

        try

          case FSelectedNode.VirtualMode of
            vmNone, vmKNTNode : begin
              if FPlainText then
                FEditor.StreamFormat := sfPlainText
              else
                FEditor.StreamFormat := sfRichText;
            end;
            vmText, vmHTML : begin
              FEditor.StreamFormat := sfPlainText;
            end;
            vmRTF : begin
              FEditor.StreamFormat := sfRichText;
            end;
          end;

          FEditor.StreamMode := [];
          if FEditor.StreamFormat = sfPlainText then begin
              NodeTextW:= FEditor.TextW;
              if NodeTextW <> string(NodeTextW) then
                 FEditor.StreamMode := [smUnicode];
          end;

          CleanHyperlinks;

          FEditor.Lines.SaveToStream( FSelectedNode.Stream );


        finally
          FEditor.StreamFormat := sfRichText;
          FEditor.StreamMode := [];
        end;

        FSelectedNode.Stream.Position := 0;
      end;
    finally
      FEditor.Lines.EndUpdate;
    end;
  end;
end; // EditorToDataStream

function TTreeNote.CheckTree : boolean;
begin
  result := assigned( FTV );
end; // CheckTree

procedure TTreeNote.SaveToFile( var tf : TWTextFile );
var
  List : TStringList;
  i, cnt : integer;
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
  if wasmismatch then
  begin
    if ( DoMessageBox(WideFormat(STR_05,
      [FName,FTV.Items.Count,FNodes.Count]
    ),
    WideFormat(
      STR_06,
      [FName]
     ), MB_YESNO+MB_ICONEXCLAMATION+MB_DEFBUTTON1+MB_APPLMODAL ) <> ID_YES ) then
    begin
      raise ETabNoteError.Create(
        STR_07
      );
    end;
  end;

  if HaveVCLControls then
  begin
    if FVerticalLayout then
      FTreeWidth := FTV.Height
    else
      FTreeWidth := FTV.Width;

    if assigned( FTV.Selected ) then
      FOldSelectedIndex := FTV.Selected.AbsoluteIndex
    else
      FOldSelectedIndex := -1;
  end;

  List := TStringList.Create;
  try
    tf.writeln( [_NF_TreeNote ] ); // marks beginning of TTreeNote
    BaseSaveProc( tf );

    // basic treenote properties
    tf.writeln( [_SelectedNode, '=', FOldSelectedIndex ] );
    tf.writeln( [_TreeWidth, '=', FTreeWidth ] );
    tf.writeln( [_DefaultNodeName, '=', FDefaultNodeName ] );

    // tree chrome
    tf.writeln( [_CHTRBGColor, '=', ColorToString( FTreeChrome.BGColor ) ] );
    tf.writeln( [_CHTRFontCharset, '=', inttostr( FTreeChrome.Font.Charset ) ] );
    tf.writeln( [_CHTRFontColor, '=', ColorToString( FTreeChrome.Font.Color ) ] );
    tf.writeln( [_CHTRFontName, '=', FTreeChrome.Font.Name ] );
    tf.writeln( [_CHTRFontSize, '=', FTreeChrome.Font.Size ] );
    tf.writeln( [_CHTRFontStyle, '=', FontStyleToStr( FTreeChrome.Font.Style ) ] );

    NodeCnt := FNodes.Count;
    NodeIdx := 0;

    notenode := nil;
    treenode := nil; // initialize to eliminate compiler warning

    // obtain first node
    if HaveVCLControls then
    begin
      treenode := FTV.Items.GetFirstNode;
      if assigned( treenode ) then
      begin
        notenode := TNoteNode( treenode.data );
        if assigned( notenode ) then
          notenode.Level := treenode.Level;
      end;
    end
    else
    begin
      if ( NodeCnt > 0 ) then
        notenode := FNodes[0];
    end;

    while assigned( notenode ) do
    begin
      inc( nodessaved );

      tf.writeln( [_NF_TRN ] );
      tf.writeln( [_NodeLevel, '=', notenode.Level ] );

      tf.writeln( [_NodeName, '=', notenode.Name ] );
      tf.writeln( [_NodeID, '=', notenode.ID ] );
      tf.writeln( [_NodeFlags, '=', notenode.PropertiesToFlagsString ] );
      tf.writeln( [_NodeRTFBGColor, '=', ColorToString( notenode.RTFBGColor )] );
      tf.writeln( [_NodeImageIndex, '=', notenode.ImageIndex ] );
      if noteNode.HasNodeColor then
        tf.writeln( [_NodeColor, '=', ColorToString( noteNode.NodeColor )] );
      if noteNode.HasNodeBGColor then
        tf.writeln( [_NodeBGColor, '=', ColorToString( noteNode.NodeBGColor )] );
      if noteNode.HasNodeFontFace then
        tf.writeln( [_NodeFontFace, '=', noteNode.NodeFontFace ] );

      if (NoteNode.VirtualMode <> vmKNTNode) and (noteNode.HasAlarms(true)) then
          SaveAlarms(tf, noteNode);

      if ( _SAVE_RESTORE_CARETPOS and ( notenode.SelStart > 0 )) then
        tf.writeln( [_NodeSelStart, '=', notenode.SelStart ] );
      if ( NoteNode.VirtualMode = vmNone ) then
      begin
        tf.writeln( [_NF_RTF ] );
        notenode.Stream.Position := 0;
        list.clear;
        list.LoadFromStream( notenode.Stream );
        cnt := pred( list.Count );
        if ( cnt >= 0 ) then // if list is empty, cnt is -1
        begin
          if FPlainText then
          begin
            for i := 0 to cnt do
              tf.writeln( [_NF_PLAINTEXTLEADER, List[i] ] );
          end
          else
          begin
            // FPlaintext property not supported
            for i := 0 to cnt do
              tf.writeln( [List[i] ] );
          end;
        end;
      end
      else
      if NoteNode.VirtualMode = vmKNTNode  then begin
         tf.writeln( [_VirtualNode, '=', notenode.MirrorNodeID ] );
      end
      else
      begin
        if notenode.HasVNodeError then
        begin
          // there was an error when we tried to load this file,
          // so don't try to save it (assume no valid data in node)
          tf.writeln( [_VirtualFN, '=', copy( notenode.VirtualFN, 2, length( notenode.VirtualFN ))] );
        end
        else
        begin
          try
            NoteNode.SaveVirtualFile;

            tf.writeln( [_RelativeVirtualFN, '=', notenode.RelativeVirtualFN ] ); // MUST be done AFTER NoteNode.SaveVirtualFile. MUST also be saved BERFORE notenode.VirtualFN.
            tf.writeln( [_VirtualFN, '=', notenode.VirtualFN ] );
          except
            on E : Exception do
            begin
              // [x] A note may have hundreds of nodes.
              // We should allow user to ABORT here or
              // to skip subsequent error messages
              DoMessageBox( WideFormat(
                STR_08 + #13 + '%s' + #13#13 + '%s',
                [notenode.Name, self.Name, notenode.VirtualFN, E.Message ] ),
                mtError, [mbOK], 0 );
            end;
          end;
        end;
      end;


      // obtain next node, or bail out if NIL
      notenode := nil;
      if HaveVCLControls then
      begin
        treenode := treenode.GetNext;
        if assigned( treenode ) then
        begin
          notenode := TNoteNode( treenode.data );
          if assigned( notenode ) then
            notenode.Level := treenode.Level;
        end;
      end
      else
      begin
        inc( NodeIdx );
        if ( NodeIdx < NodeCnt ) then
          notenode := FNodes[NodeIdx];
      end;
    end;

    Modified := false;
  finally
    if ( nodessaved <> FNodes.Count ) then
    begin
      raise ETabNoteError.CreateFmt(
        STR_09,
        [FNodes.Count, nodessaved]
      );
    end;
    List.Free;
  end;

end; // SaveToFile

procedure TTreeNote.LoadFromFile( var tf : TWTextFile; var FileExhausted : boolean );
var
  InRichText : boolean;
  InNoteNode : boolean;
  List : TStringList;
  s, key : string;
  p, linecount : integer;
  myNode : TNoteNode;

    procedure AddNewNode;
    begin
      // transfer RTF data from list to node
      InRichText := false;
      if ( List.Count > 0 ) then
        List.SaveToStream( myNode.Stream );
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
            _NEW_NOTE_KIND := ntRTF;
            if assigned( myNode ) then AddNewNode;
            break; // New TabNote begins
          end;
          if ( s = _NF_TreeNote ) then
          begin
            _NEW_NOTE_KIND := ntTree;
            if ( myNode <> nil ) then
              AddNewNode;
            break; // New TreeNote begins
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
                  myNode.Name := TryUTF8ToWideString(s);
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
                  myNode.MirrorNodeID := TryUTF8ToWideString(s);
                end
                else
                if ( key = _RelativeVirtualFN ) then
                begin
                  myNode.RelativeVirtualFN := TryUTF8ToWideString(s);
                end
                else
                if ( key = _VirtualFN ) then
                begin
                  myNode.VirtualFN := TryUTF8ToWideString(s);
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
                 FDefaultNodeName := TryUTF8ToWideString(s);
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
              FName := TryUTF8ToWideString(s);
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
  tf : TextFile;
  InNode : boolean;
  level : integer;
  myNode : TNoteNode;
  List : TStringList;

    procedure AddNewNode;
    begin
      // transfer RTF data from list to node
      List.SaveToStream( myNode.Stream );
      myNode.Stream.Position := 0;
      List.Clear;
      InternalAddNode( myNode );
      myNode := nil;
    end; // AddNewNode

begin
  InNode := false;

  if ( not fileexists( FN )) then exit;

  assignfile( tf, FN );

  try
    reset( tf );
  except
    messagedlg( STR_11 + FN, mtError, [mbOK], 0 );
    exit;
  end;


  List := TStringList.Create;

  try

    while ( not eof( tf )) do
    begin
      readln( tf, s );

      case InNode of
        false : begin
          if ( s = _TREEPAD_NODE ) then
          begin
            InNode := true;
            try
              readln( tf, nodeName ); // node name
              readln( tf, s ); // node level
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
          if ( s = _TREEPAD_ENDNODE ) then
          begin
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
    closefile( tf );
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

Initialization
  try
    _LoadedRichEditVersion := LoadedRichEditVersion;
  except
    _LoadedRichEditVersion := RichEditVersion; // from RxRichEdit (not always the correct value)
  end;


end.
