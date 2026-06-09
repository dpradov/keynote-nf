unit knt.ui.noteEntries;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2025 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  RxRichEd,
  VirtualTrees,
  TB97Ctls,

  gf_streams,
  gf_strings,
  kn_info,
  kn_Const,
  kn_Global,
  kn_KntFolder,
  knt.model.note,
  knt.ui.info,
  knt.ui.editor,
  knt.App
  ;



type
  TBeforeEditorLoadedEvent = procedure(Note: TNote) of object;
  TAfterEditorLoadedEvent  = procedure(Note: TNote) of object;

type
  TEntryShown = record
    Note: TNote;
    NNode: TNoteNode;
    NEntry: TNoteEntry;
    StartingPos: integer;
    StartingContentPos: integer;
    FinalPos: integer;
    Content: TContentInMultiEntriesMode;
  end;

  TActionOnEntry = (aModified, aCreating, aCreatingFromOtherPanel, aCreated, aDeleted, aModifiedMetadata, aChangedVisibility, aRefreshHeader, aNull);

type
  TKntNoteEntriesUI = class(TFrame)
    pnlEntries: TPanel;
    pnlButtons: TPanel;
    txtCreationDate: TEdit;
    txtName: TEdit;
    txtTags: TEdit;
    btnNextEntry: TToolbarButton97;
    btnPrevEntry: TToolbarButton97;
    btnOptions: TToolbarButton97;
    btnToggleMulti: TToolbarButton97;
    cFocusedFlag: TPaintBox;
    procedure txtNameChange(Sender: TObject);
    procedure txtEnter(Sender: TObject);
    procedure txtNameMouseEnter(Sender: TObject);
    procedure txtCreationDateMouseEnter(Sender: TObject);
    procedure txtNameExit(Sender: TObject);
    procedure txtTagsEnter(Sender: TObject);
    procedure btnPrevEntryClick(Sender: TObject);
    procedure btnNextEntryClick(Sender: TObject);
    procedure btnToggleMultiClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure cFocusedFlagPaint(Sender: TObject);
    procedure txtTagsExit(Sender: TObject);

  private class var
    FColorTxts: TColor;

  private
    FNote: TNote;                  // Selected Note
    FNNode: TNoteNode;             //  ,,     Note Node
    FNEntry: TNoteEntry;           //  ,,     Note Entry
    FKntFolder: TKntFolder;
    FEditor: TKntRichEdit;
    FNoteUI: INoteUI;
    FOnUse: boolean;
    FPanelHidden: boolean;               // To mark panels, in QueryLayout, where no entry is available (panels not shown because of maximized other panel will not be marked)
    FEntriesShown: Array of TEntryShown;
    FiEntry: integer;
    FPanelConfig: TPanelConfiguration;
    FTagsToUseOnNewEntry: TNoteTagArray;

    RTFAux: TAuxRichEdit;

    FInfoPanelHidden: boolean;
    fImagesReferenceCount: TImageIDs;

    //FLastEditorUIWidth: string;

    fChangingInCode: boolean;
    FReadOnly: boolean;
    FPanelInitialized: boolean;

    FOnEnterOnEditor: TNotifyEvent;
    FOnMouseUpOnNoteEntries: TNotifyEvent;
    FOnMouseMoveOnNoteEntries: TNotifyEvent;


  public
    constructor Create(AOwner: TComponent; NoteUI: INoteUI);
    destructor Destroy; override;

    property Editor : TKntRichEdit read FEditor;

  public
    property Folder: TKntFolder read FKntFolder;
    property Note: TNote read FNote;
    property NNode: TNoteNode read FNNode;
    property NEntry: TNoteEntry read FNEntry write FNEntry;
    property NoteUI: INoteUI read FNoteUI;
    property PanelConfig: TPanelConfiguration read FPanelConfig write FPanelConfig;
    procedure LoadFromDataModel (APanelConfig: TPanelConfiguration; SavePreviousContent: boolean; InformReloaded: boolean = false; ActionOnEntry: TActionOnEntry = aNull);
    procedure ReloadFromDataModel (CalculateEntriesToShow: boolean = true;
                                   NEntryToConsider: TNoteEntry = nil;
                                   ActionOnEntry: TActionOnEntry = aNull;
                                   InformReloaded: boolean = false);
    procedure ReloadMetadataFromDataModel (ReloadTags: boolean = true);
    procedure ReloadVisibleContentOfEntries (ModifyAll: boolean; NewContent: TContentInMultiEntriesMode; iEntry: integer= -1;
                                             IgnoreHiddenEntries: boolean = true; OnlyHiddenEntries: boolean = false;
                                             LimitToCreatedBeforeSelectedEntry: boolean = false);
    procedure ShowHiddenEntries(UndoHidden: boolean);
    procedure RefreshHeaderOfEntries(OnlyNEntry: TNoteEntry = nil);
    procedure ModifiedMetadataOfEntry(NEntry: TNoteEntry);
    procedure NEntryDeleted(NEntry: TNoteEntry);
    procedure NEntryHidden(NEntry: TNoteEntry; Hidden: boolean; CreatedBefore: TDateTime = 0);
    procedure NEntryReadOnlyChanged(NEntry: TNoteEntry);
    procedure SaveToDataModel; overload;
    procedure SaveToDataModel (RTFAux: TAuxRichEdit; NEntry: TNoteEntry); overload;
    procedure SavePositionInPanel;
    procedure ReloadNoteName;
    procedure EditorChangedSelectionInMultiEntries;
    procedure EditorDblClickInMultiEntries(Ctrl, Alt: boolean; LimitToCreatedBeforeSelectedEntry: boolean = false);
    procedure ToggleOnlyHeaders_WholeContent;
    function GetIndexOfIncludedEntry(NEntry: TNoteEntry): integer;
    function GetPreparedForJump(NEntry: TNoteEntry; var PosStartEntry: integer; var PosEndEntry: integer; AllowEdit: boolean = false): boolean;
    function IsDisplayingEntry(NEntry: TNoteEntry; var Content: TContentInMultiEntriesMode): boolean;
    function NumberOfIncludedEntries(OnlyNotHidden: boolean): integer;
    function DisplayingAnyHiddenEntry: boolean;
    function HasAnyEntryNonVisible: boolean;
    procedure GetEntryBoundaries(NEntry: TNoteEntry; var PosStartEntry: integer; var PosEndEntry: integer);
    procedure ModifyContentForNextReload(NEntry: TNoteEntry; NewContent: TContentInMultiEntriesMode);
    procedure ConfigureEditor(iEntry: integer = -1);
    //procedure UpdateEntriesHeaderWidth(EnsureRefreshOnEditor: boolean);
  protected
    function StreamFormatInNEntry(const NEntry: TNoteEntry): TRichStreamFormat;
    //function GetHeaderCellx: AnsiString;
    function GetEntryHeader (Note: TNote; NEntry: TNoteEntry; FirstEntry: boolean = False; Folded: boolean = False): AnsiString;

  protected
    procedure SetInfoPanelHidden(value: boolean);
    procedure ShowControlsPanelIdentif(Show: boolean);
    procedure CheckPnlButtonsLocation;
    procedure OnEndEditTagsIntroduction(PressedReturn: boolean; txtTags: TEdit);
    procedure AdjustTxtTagsWidth (AllowEdition: boolean = False);
    procedure ShowEntriesButtons(Show: boolean);
    procedure SelectEntry(iEntry: integer; LastPos: boolean = false; InformReloaded: boolean = True);
    procedure FrameResize(Sender: TObject);
    function InfoBarShowingNoteMetadata: boolean;
  public
    procedure EditTags;
    procedure RefreshTags;
    property TagsToUseOnNewEntry: TNoteTagArray read FTagsToUseOnNewEntry write FTagsToUseOnNewEntry;
    function HideTemporarilyInfoPanel: boolean;
    property InfoPanelHidden: boolean read FInfoPanelHidden write SetInfoPanelHidden;
    procedure ReconsiderColorInfoPanel;
    procedure ReconsiderInfoPanelVisibility;
    procedure SetTopIncControlsOfInfoPanel;
    procedure RefreshEntry;
    procedure SelectNextEntry(InformReloaded: boolean);
    procedure SelectPrevEntry(InformReloaded: boolean);

  protected
    function GetReadOnly: boolean;
    procedure SetReadOnly( AReadOnly : boolean );
    procedure ForceTempReadOnly( AReadOnly : boolean );
    procedure SetPanelHidden( Value : boolean );
  public
    property ReadOnly : boolean read GetReadOnly write SetReadOnly;
    property OnUse: boolean read FOnUse;
    property PanelHidden: boolean read FPanelHidden write SetPanelHidden;
    procedure SetAsUnused;

  protected
    function GetImagesInstances: TImageIDs;
    property ImagesReferenceCount: TImageIDs read fImagesReferenceCount write fImagesReferenceCount;
  public
    property ImagesInstances: TImageIDs read GetImagesInstances;
    function GetImagesIDInstances (Stream: TMemoryStream; TextPlain: String): TImageIDs;
    procedure ResetImagesReferenceCount;
    procedure ReloadImagesOnEditor;
    procedure ReconsiderImageDimensionGoalsOnEditor (Selection: boolean; ImagesMode: TImagesMode);
    procedure SetImagesMode(ImagesMode: TImagesMode);

  protected
    procedure NoteEntriesUIEnter(Sender: TObject);
    procedure NoteEntriesUIExit(Sender: TObject);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  public
    function HideNestedFloatingEditor: boolean;
  public
    procedure SetOnEnter(AEvent: TNotifyEvent);
    procedure SetOnMouseUpOnNote(AEvent: TNotifyEvent);
    procedure SetOnMouseMoveOnNote(AEvent: TNotifyEvent);
    procedure SetFocusOnEditor;
  end;



implementation

{$R *.dfm}

uses
  System.DateUtils,
  gf_misc,
  gf_miscvcl,
  kn_LinksMng,
  kn_EditorUtils,
  kn_ImagesUtils,
  kn_RTFUtils,
  kn_KntFile,
  knt.ui.TagMng,
  knt.ui.note,
  knt.RS;

const
  MIN_TAGS_WIDTH = 17;


// Create  / Destroy =========================================

{$REGION Create / Destroy}

constructor TKntNoteEntriesUI.Create(AOwner: TComponent; NoteUI: INoteUI);
var
 i: integer;
begin
   inherited Create(AOwner);

   FNoteUI:= NoteUI;
   FKntFolder:= TKntFolder(NoteUI.GetFolder);

   FEditor := TKntRichEdit.Create( pnlEntries );
   with FEditor do begin
      Parent := pnlEntries;

      Align := alClient;
      HelpContext := 11;
      MaxLength := 0; // unlimited text size
      ParentFont := false;
      WantTabs := true;
      WantReturns := true;
      AllowInPlace := true;
      AllowObjects := true;
      AutoVerbMenu := true;
      HideSelection := false;
      SelectionBar := true;
      UndoLimit := EditorOptions.UndoLimit;
      WordSelection := EditorOptions.WordSelect;
      RecreateWndProtect := KeyOptions.RichEditv3;
      LangOptions := [];
      if EditorOptions.AutoKeyboard then
        LangOptions := LangOptions + [rlAutoKeyboard];
      if EditorOptions.AutoFont then
        LangOptions := LangOptions + [rlAutoFont];
      ScrollBars := ssBoth;

      OnMouseUp := EditorMouseUp;
      OnMouseMove := EditorMouseMove;
   end;

   OnEnter:= NoteEntriesUIEnter;
   OnExit:= NoteEntriesUIExit;
   OnResize:= FrameResize;

   FColorTxts:= RGB(248,248,248);
   txtName.Color:= FColorTxts;
   txtCreationDate.Color:= FColorTxts;
   txtTags.Color:= FColorTxts;

   SetTopIncControlsOfInfoPanel;

   btnToggleMulti.Font.Size:= 8;

   SetReadOnly(FKntFolder.ReadOnly);
   fChangingInCode:= false;
   //FLastEditorUIWidth:= '';
   FPanelConfig:= nil;
   FOnUse:= False;
   FPanelHidden:= True;
   FPanelInitialized:= false;
   FTagsToUseOnNewEntry:= nil;

   UpdateEditor (FEditor, FKntFolder, true); // do this BEFORE placing RTF text in editor

   App.EditorAvailable(FEditor);
end;


destructor TKntNoteEntriesUI.Destroy;
begin
    if assigned( FEditor ) then begin
      App.EditorUnavailable(FEditor);
      FreeAndNil(FEditor);
    end;
    if RTFAux <> nil then
      FreeAndNil(RTFAux);

   fImagesReferenceCount:= nil;

   inherited;
end;

{$ENDREGION}


// Controls. Events

{$REGION Controls. Properties and Events }


function TKntNoteEntriesUI.GetReadOnly: boolean;
begin
   Result:= Editor.ReadOnly;
end;

procedure TKntNoteEntriesUI.SetReadOnly( AReadOnly : boolean );
begin
   FReadOnly:= AReadOnly;
   Editor.ReadOnly:= AReadOnly;
   txtName.ReadOnly:= AReadOnly;
   txtTags.ReadOnly:= AReadOnly;
end;

procedure TKntNoteEntriesUI.ForceTempReadOnly( AReadOnly : boolean );
begin
   Editor.ReadOnly:= AReadOnly;
   txtName.ReadOnly:= AReadOnly;
   txtTags.ReadOnly:= AReadOnly;
end;

procedure TKntNoteEntriesUI.SetPanelHidden( Value : boolean );
begin
   FPanelHidden:= Value;
   if FPanelConfig <> nil then begin
      FPanelConfig.Hidden:= Value;
      if not Value and (PanelConfig.StLayout = spInQL_ets) then
         PanelConfig.StLayout:= spInQL;
   end;
end;

procedure TKntNoteEntriesUI.SetAsUnused;
begin
  FOnUse:= False;
  FPanelConfig:= nil;
  PanelHidden:= True;
  FNNode:= nil;
  FNEntry:= nil;
  Editor.BeginUpdate;
  Editor.Clear;
  Editor.EndUpdate;
end;


procedure TKntNoteEntriesUI.SetOnEnter(AEvent: TNotifyEvent);
begin
  FOnEnterOnEditor:= AEvent;
end;

procedure TKntNoteEntriesUI.SetOnMouseUpOnNote(AEvent: TNotifyEvent);
begin
   FOnMouseUpOnNoteEntries:= AEvent;
end;

procedure TKntNoteEntriesUI.SetOnMouseMoveOnNote(AEvent: TNotifyEvent);
begin
   FOnMouseMoveOnNoteEntries:= AEvent;
end;


procedure TKntNoteEntriesUI.NoteEntriesUIEnter(Sender: TObject);
begin
  if FNote = nil then exit;
  if TKntNoteUI(FNoteUI).ChangingLayout then exit;

  FloatingEditorCannotBeSaved:= False;
  Editor.HideNestedFloatingEditor;
  App.EditorFocused(Editor);
  TagMng.UpdateTxtTagsHint(txtTags);
  if Assigned(FOnEnterOnEditor) then
    FOnEnterOnEditor(Self);

  TKntNoteUI(FNoteUI).NEntriesUIEditorEnter(Self);

  if FloatingEditorCannotBeSaved then
     Editor.ActivateFloatingEditor;

  cFocusedFlag.Refresh;
  ReconsiderInfoPanelVisibility;
end;


function TKntNoteEntriesUI.HideNestedFloatingEditor: boolean;
begin
  Result:= True;
  FloatingEditorCannotBeSaved:= False;
  Editor.HideNestedFloatingEditor;

  if FloatingEditorCannotBeSaved then begin
     Editor.ActivateFloatingEditor;
     Result:= False;
  end;
end;


procedure TKntNoteEntriesUI.NoteEntriesUIExit(Sender: TObject);
begin
   cFocusedFlag.Refresh;
   if (PanelConfig = nil) then exit;
   HideTemporarilyInfoPanel;
end;

procedure TKntNoteEntriesUI.EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUpOnNoteEntries) then
    FOnMouseUpOnNoteEntries(Self);
end;

procedure TKntNoteEntriesUI.EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMoveOnNoteEntries) then
    FOnMouseMoveOnNoteEntries(Self);
end;

procedure TKntNoteEntriesUI.SetInfoPanelHidden(value: boolean);
begin
   FInfoPanelHidden:= value;
   if FOnUse then
      ShowControlsPanelIdentif(not value);
end;


procedure TKntNoteEntriesUI.CheckPnlButtonsLocation;
var
  W, SW, X: integer;
begin
   if pnlButtons.Visible then begin
      W:= 0;
      SW:= GetSystemMetrics(SM_CXVSCROLL);
      if not txtName.Visible and Editor.IsVerticalScrollBarVisible then
         W:= SW;
      X:= (Self.Width-W) - (pnlButtons.Left + pnlButtons.Width);
      if X > 1 then begin
         pnlButtons.Left:= pnlButtons.Left + SW;
         txtCreationDate.Left:= txtCreationDate.Left + SW;
      end
      else
      if X < 1 then begin
         pnlButtons.Left:= pnlButtons.Left - SW;
         txtCreationDate.Left:= txtCreationDate.Left - SW;
      end;
   end;
end;


procedure TKntNoteEntriesUI.ShowControlsPanelIdentif(Show: boolean);
var
  W, SW, X: integer;
begin
   txtName.Visible:= Show and ((PanelConfig = nil) or PanelConfig.ShowEditorInfoPanel);
   txtCreationDate.Visible:= Show and ((PanelConfig = nil) or PanelConfig.ShowEditorInfoPanel); // or (Mode = meSingleEntry));
   txtTags.Visible:= Show;
   pnlButtons.Visible:= Show;
   CheckPnlButtonsLocation;

  if not txtName.Visible then
     pnlEntries.Height:= Self.Height
  else
     pnlEntries.Height:= Self.Height - txtTags.Height -2; //txtTags.Top -2;
end;

function TKntNoteEntriesUI.HideTemporarilyInfoPanel: boolean;
var
  KeepVisible: boolean;
begin
  KeepVisible:= txtTags.Focused or txtName.Focused; // or txtCreationDate.Focused;
  if not KeepVisible then
     KeepVisible:= IsMouseOver(txtTags) or IsMouseOver(pnlButtons);

  if (FInfoPanelHidden or not PanelConfig.ShowEditorInfoPanel) and not KeepVisible then
     ShowControlsPanelIdentif(false);

  Result:= not KeepVisible;
end;


procedure TKntNoteEntriesUI.ReconsiderColorInfoPanel;
var
  colorEdLay, colorMax: TColor;
begin
  //colorEdLay:= txtName.Color;
  colorMax:= clBtnFace;
  if PanelConfig.ShowEditorInfoPanel then begin
    if PanelConfig.EditingLayout then begin
       //colorEdLay:= RGB(190,190,190);
       colorMax:= RGB(220,220,240);
    end;
    if PanelConfig.Maximized then
       colorMax:= clLtGray;
  end;
  //Self.Color:= colorEdLay;
  btnPrevEntry.Color:= colorMax;
  btnNextEntry.Color:= colorMax;
  btnToggleMulti.Color:= colorMax;
  btnOptions.Color:= colorMax;
end;


procedure TKntNoteEntriesUI.ReconsiderInfoPanelVisibility;
var
  colorEdLay, colorMax: TColor;
begin
  if Height <= HEIGHT_REDUCED_TO_HIDDEN then exit;

  ShowControlsPanelIdentif(True);        // Temporarily if not PanelConfig.ShowEditorInfoPanel
  if (PanelConfig.Maximized) or
     (PanelConfig.ShowEditorInfoPanel and
          ( ((txtTags.Left + txtTags.Width + 2) <> txtName.Left ) or
            ((txtName.Left + txtName.Width + 2) <> txtCreationDate.Left ) )) or
     ((txtTags.Width <= MIN_TAGS_WIDTH) And (FNEntry <> nil) and (FNEntry.Tags <> nil)) then
     FrameResize(nil);

  ReconsiderColorInfoPanel;

  FNoteUI.KeepInfoPanelTemporarilyVisible;
end;


procedure TKntNoteEntriesUI.SetTopIncControlsOfInfoPanel;
var
  T, T2: integer;
begin
     if KeyOptions.EditorInfoPanelTop then begin
      T:= 0;
      T2:= txtCreationDate.Height + 2;
   end
   else begin
      T:= Self.Height - txtCreationDate.Height;
      T2:= 0;
   end;
   txtName.Top:= T;
   txtCreationDate.Top:= T;
   txtTags.Top:= T;
   pnlButtons.Top:= T;
   pnlEntries.Top:= T2;
end;


procedure TKntNoteEntriesUI.cFocusedFlagPaint(Sender: TObject);
begin
  with cFocusedFlag.Canvas do
  begin
    if (PanelConfig <> nil) and not PanelConfig.Maximized and NoteUI.MultipleVisibleEditors and not NoteUI.HideFocusFlag and Editor.Focused then
       Brush.Color := clRed
    else
       Brush.Color := clBtnFace;
    Brush.Style := bsSolid;
    Pen.Style   := psClear;
    FillRect(cFocusedFlag.ClientRect);
  end;
end;



procedure TKntNoteEntriesUI.txtNameChange(Sender: TObject);
begin
   if not ActiveFileIsBusy and assigned(NNode) then begin
      NNode.Note.Name:= txtName.Text;
      FKntFolder.Modified:= True;
   end;
end;

procedure TKntNoteEntriesUI.txtNameMouseEnter(Sender: TObject);
var
   s, path: string;
   AncestorPathLen: integer;

begin
   s:= NNode.NoteName;
   if NNode <> nil then begin
      path:= FKntFolder.TreeUI.GetNodePath(NNode.TVNode, KntTreeOptions.NodeDelimiter, true );
      AncestorPathLen:= Length(path) - Length(NNode.NoteName);
      if AncestorPathLen > 1 then
         s:= s + '  (' +  Copy(path, 1, AncestorPathLen) + ')';

      if (NNode.Note.Tags <> nil) then
         s:= s + '  # [' + FNNode.Note.MainEntry.TagsNames + ']';
   end;
   txtName.Hint:= s;
end;

procedure TKntNoteEntriesUI.txtCreationDateMouseEnter(Sender: TObject);
var
  s, lm: string;
begin
  if (FNote <> nil) then begin
      if (PanelConfig.CurrentMode = meMultipleEntries) or (FNote.NumEntries = 1) then begin
         if FNote.LastModified <> 0 then begin
            if (FNote.LastModified).GetTime <> 0 then
                S:= ' - ' + FormatSettings.ShortTimeFormat;
            lm:= FormatDateTime(FormatSettings.ShortDateFormat + S, FNote.LastModified);
         end;
         s:= Format(GetRS(sUInote01), [txtCreationDate.Text, lm]);
      end
      else
         s:= Format(GetRS(sUInote02), [txtCreationDate.Text]);
  end;
  txtCreationDate.Hint:= s;
end;

procedure TKntNoteEntriesUI.txtEnter(Sender: TObject);
begin
   if (FNote = nil) then exit;

   if txtName.Focused and not txtName.ReadOnly then
      txtName.Color:= clWindow;

   txtName.SelLength:= 0;
   NoteEntriesUIEnter(Sender);
end;

procedure TKntNoteEntriesUI.txtNameExit(Sender: TObject);
begin
   txtName.Color:= FColorTxts;
end;


procedure TKntNoteEntriesUI.SetFocusOnEditor;
begin
  try
     Editor.SetFocus;
  except
  end;
end;


{$ENDREGION}


// Tags =========================================

{$REGION Tags }

procedure TKntNoteEntriesUI.EditTags;
begin
   SetInfoPanelHidden(False);
   txtTags.SetFocus;
   txtTags.SelStart:= txtTags.GetTextLen;
end;


function TKntNoteEntriesUI.InfoBarShowingNoteMetadata: boolean;
begin
   Result:= (FNEntry = FNNode.Note.MainEntry);

//   if FNNode.Note.MainEntry = FNEntry then
//      Result:= True;
//   else
//      Result:= PanelConfig.ShowEditorInfoPanel and
//              ((PanelConfig.CurrentMode = meMultipleEntries) or (FNNode.Note.NumEntries = 1));
end;


procedure TKntNoteEntriesUI.RefreshTags;
var
   S: string;
   Color: TColor;
   NEntry: TNoteEntry;
   Tags: TNoteTagArray;
begin
   if FNNode = nil then exit;

   Color:= clWindowText;

   if InfoBarShowingNoteMetadata then
      NEntry:= FNNode.Note.MainEntry
   else begin
      NEntry:= FNEntry;
      Color:= RGB(0,0, 180);
   end;

   if NEntry <> nil then begin
      S:= NEntry.TagsNames;
   end
   else begin
      Tags:= TagsToUseOnNewEntry;
      if Tags = nil then
         Tags:= PanelConfig.VinculatedTags;
      if (Tags <> nil) then begin
         S:= TNoteTagArrayUtils.ToNames(Tags);
         Color:= clMaroon;
      end
   end;

   txtTags.Text:= S;
   TagMng.UpdateTxtTagsHint(txtTags);
   if S = '' then
      txtTags.Text:= EMPTY_TAGS;

   txtTags.Font.Color:= Color;
   AdjustTxtTagsWidth;
end;

procedure TKntNoteEntriesUI.txtTagsEnter(Sender: TObject);
var
   NEntry: TNoteEntry;
begin
   if PanelConfig = nil then exit;

   if InfoBarShowingNoteMetadata then
      NEntry:= FNNode.Note.MainEntry
   else begin
      if CtrlDown then begin
         NEntry:= FNNode.Note.MainEntry;
         txtTags.Text:= NEntry.TagsNames;
      end
      else
         NEntry:= FNEntry;
   end;

   if (NEntry = nil) or txtTags.ReadOnly then begin
      SetFocusOnEditor;
      exit;
   end;

   TagMng.StartTxtEditTagIntrod(txtTags, OnEndEditTagsIntroduction, FNote, NEntry, Folder);
   AdjustTxtTagsWidth(True);
end;

procedure TKntNoteEntriesUI.txtTagsExit(Sender: TObject);
begin
   RefreshTags;                   // In case we have forced the use of MainEntry from txtTagsEnter because Ctrl was pressed
   ReconsiderInfoPanelVisibility;
end;

procedure TKntNoteEntriesUI.OnEndEditTagsIntroduction(PressedReturn: boolean; txtTags: TEdit);
begin
  if PressedReturn then
     Editor.SetFocus;

   txtTags.Color:= FColorTxts;
   if not InfoBarShowingNoteMetadata and (FNEntry <> nil) then
      txtTags.Font.Color:= RGB(0,0, 180);

   AdjustTxtTagsWidth;

   InfoPanelHidden:= Folder.EditorInfoPanelHidden;
end;


procedure TKntNoteEntriesUI.ShowEntriesButtons(Show: boolean);
var
   W: integer;
begin
   if btnPrevEntry.Visible = Show then exit;

   btnPrevEntry.Visible:= Show;
   W:= btnPrevEntry.Width*2 + btnToggleMulti.Width;
   if Show then
      W:= W * -1;

   pnlButtons.Left:= pnlButtons.Left + W;
   pnlButtons.Width:= pnlButtons.Width - W;
   txtCreationDate.Left:= txtCreationDate.Left + W;
   txtName.Width:= txtName.Width + W;
end;

procedure TKntNoteEntriesUI.AdjustTxtTagsWidth (AllowEdition: boolean = False);
var
  MinNoteNameWidth, MaxAvailableWidth: integer;
  MaxAvailableForTags, TagsWidth: integer;
begin
  MinNoteNameWidth:= 0;
  if txtName.Visible then
     MinNoteNameWidth:= TagMng.GetTextWidth(Note.Name, txtName) + 10;
  TagsWidth:=   MIN_TAGS_WIDTH;
  if txtTags.Text <> EMPTY_TAGS then
     TagsWidth:= TagMng.GetTextWidth(txtTags.Text, txtTags) + 10;

  MaxAvailableWidth:= (pnlButtons.Left-4);
  if txtCreationDate.Visible then
     dec(MaxAvailableWidth, (txtCreationDate.Width + 4));

  MaxAvailableForTags:= MaxAvailableWidth;

  if not AllowEdition then
     dec(MaxAvailableForTags, MinNoteNameWidth)
  else begin
     TagsWidth:= TagsWidth * 2;
     if TagsWidth < 170 then
        TagsWidth:= 170;
  end;
  if TagsWidth > MaxAvailableForTags then
     TagsWidth := MaxAvailableForTags;

  if TagsWidth < MIN_TAGS_WIDTH then
     TagsWidth := MIN_TAGS_WIDTH;

  txtTags.Width:= TagsWidth;
  if txtName.Visible then begin
     txtName.Width:= MaxAvailableWidth - TagsWidth;
     txtName.Left:= txtTags.Left + TagsWidth + 2;
  end;
end;


procedure TKntNoteEntriesUI.FrameResize(Sender: TObject);
begin
   if Note <> nil then begin
      ShowEntriesButtons(Length(FEntriesShown) > 1);
      CheckPnlButtonsLocation;
      AdjustTxtTagsWidth(txtTags.Focused);
      FPanelInitialized:= true;
   end;
end;


{$ENDREGION}


// Load and save Editor from Note node =========================================

{$REGION Load, save and configure Editor for a Note node }


function TKntNoteEntriesUI.GetIndexOfIncludedEntry(NEntry: TNoteEntry): integer;
var
   i: integer;
begin
   Result:= -1;
   if NEntry = nil then exit;

   for i:= Length(FEntriesShown)-1 downto 0 do
      if FEntriesShown[i].NEntry = NEntry then
         exit(i);
end;

procedure TKntNoteEntriesUI.ModifyContentForNextReload(NEntry: TNoteEntry; NewContent: TContentInMultiEntriesMode);
var
   iNEntry: integer;
begin
    iNEntry:= GetIndexOfIncludedEntry(NEntry);
    if iNEntry >= 0 then
       FEntriesShown[iNEntry].Content:= NewContent;
end;

procedure TKntNoteEntriesUI.LoadFromDataModel(APanelConfig: TPanelConfiguration; SavePreviousContent: boolean; InformReloaded: boolean = false; ActionOnEntry: TActionOnEntry = aNull);
var
  NEntry: TNoteEntry;
  KeepModified: boolean;

begin
   Editor.BeginUpdate;         // -> It will also ignore Enter and Change events

   KeepModified:= false;

   try
     try
       if SavePreviousContent and not FNoteUI.GetNNodeDeleted then begin
          SaveToDataModel();
          Editor.HideNestedFloatingEditor;
       end;

       //FNNodeDeleted:= false;    //##
       FPanelConfig:= APanelConfig;
       FNNode:= nil;
       FNote:= nil;
       FNEntry:= nil;
       FOnUse:= False;

       if FPanelConfig <> nil then begin
           if (PanelConfig.Scope = fsSelectedNode) and (PanelConfig.SelectedNNode <> nil) then begin         //***
              FOnUse:= True;
              PanelHidden:= False;

              case PanelConfig.SelectedNNode.WordWrap of
                wwAsFolder : Editor.WordWrap := FKntFolder.WordWrap;
                wwYes : Editor.WordWrap := true;
                wwno :  Editor.WordWrap := false;
              end;
           end
           else
             txtName.Visible:= True;
       end;

       ReloadFromDataModel(true, nil, ActionOnEntry, InformReloaded);

       { The normal thing is to set Editor.Modified = False at the end of the LoadFocusedNNodeIntoEditor method
         But if hidden marks to be eliminated have been identified (and corrected), it will have been kept as Modified,
         to ensure that this correction ends up persisting. Here we will do the same }
       if Editor.Modified then
          KeepModified:= True;


     except
       On E : Exception do begin
         App.ErrorPopup(E);
         exit;
       end;
     end;

   finally
     Editor.EndUpdate;
   end;
end;


procedure TKntNoteEntriesUI.ReloadMetadataFromDataModel(ReloadTags: boolean = true);
var
  S: string;
  ActiveFileIsBusyBAK: boolean;
  i: integer;
  Created: TDateTime;
begin
   if not assigned(FNote) then begin
      txtTags.Text:= '';
      txtName.Text:= '';
      txtCreationDate.Text:= '';
      ShowEntriesButtons(false);
      exit;
   end;

   ActiveFileIsBusyBAK:= ActiveFileIsBusy;
   ActiveFileIsBusy:= True;                   // To avoid txtNameChange => Modified:True
   try
      txtName.Text:= FNote.Name;

      if (FNEntry = nil) then
         txtCreationDate.Visible:= False;
      if FNEntry <> nil then begin
        if (PanelConfig.CurrentMode = meMultipleEntries) then
           Created:= FNote.DateCreated
        else
           Created:= FNEntry.Created;
        if Created <> 0  then begin
           if (Created).GetTime <> 0 then
                 S:= ' - ' + FormatSettings.ShortTimeFormat;
           txtCreationDate.Text:= FormatDateTime(FormatSettings.ShortDateFormat + S, Created);
        end
        else
           txtCreationDate.Text:= '';
      end;

      if ReloadTags then
         RefreshTags;

   finally
      ActiveFileIsBusy:= ActiveFileIsBusyBAK;
   end;
end;


procedure TKntNoteEntriesUI.ReloadFromDataModel (CalculateEntriesToShow: boolean = true;
                                                 NEntryToConsider: TNoteEntry = nil;
                                                 ActionOnEntry: TActionOnEntry = aNull;
                                                 InformReloaded: boolean = false);
var
  ReadOnlyBAK: boolean;
  str: String;

{$IFDEF KNT_DEBUG}
 dataSize: integer;
{$ENDIF}

 strRTF: AnsiString;
 ContainsImgIDsRemoved: boolean;
 ContainsImages: boolean;

 OnEnterBak: TNotifyEvent;

 cEditor: TRxRichEdit;
 i, iEntry, iEntryToConsider, iEntryAdded, iSelectedEntry: integer;
 ImagesAux: TImageIDs;
 CannotShow_Encrypted: boolean;
 SS, SL, Offset: integer;
 EntryToAdd, EntryToRemove, MustBeIncluded: boolean;
 Mode: TModeEntriesUI;
 FNEntry_Initial: TNoteEntry;
 FiEntry_Initial: integer;
 NumVisibleEntriesBefore, NumVisibleEntriesAfter: integer;


 function NEntryMustBeIncludedInPanel (NEntry: TNoteEntry): boolean;
 begin
   Result:= False;

   case PanelConfig.Scope of
      fsSelectedNode: begin
         Result:= FNote.IsValid(NEntry) and  not ((PanelConfig.VinculatedTags <> nil) and not NEntry.HasTags(PanelConfig.VinculatedTags));
      end;

      fsSelectedNodeAndSubtree: ;
      fsSelectedNodeAndAncestors: ;
      fsSelectedNodes: ;      // -> PanelConfig.NNodes
      fsFolder: ;
      fsFile: ;
   end;

 end;


 // -> FEntriesShown, FNNode, FNote, [FNEntry, CannotShow_Encrypted]
 procedure PopulateEntriesToShow;
 var
   N: integer;
   iEntry, j: integer;
   NEntry: TNoteEntry;
   Created: TDateTime;

   function GetContentToAssign(NEntry: TNoteEntry; DefaultContentInME: TContentInMultiEntriesMode; IgnoreIsHidden: boolean = false): TContentInMultiEntriesMode;
   begin
      if (NEntry.IsHidden) and not IgnoreIsHidden then
         Result:= cmHidden
      else
      if (NEntry.IsEncrypted and ActiveFile.EncryptedContentMustBeHidden) then begin
         if ActiveFile.HideEncryptedNodesAndEntries then
            Result:= cmHidden
         else
            Result:= cmOnlyHeader;
      end
      else
      if Mode = meMultipleEntries then
         Result:= DefaultContentInME
      else
         Result:= cmWholeEntry;
   end;

   procedure CheckCandidateEntry;
   begin
      NEntry:= Note.Entries[iEntry];
      if (PanelConfig.VinculatedTags = nil) or NEntry.HasTags(PanelConfig.VinculatedTags) then begin
         FEntriesShown[N].NEntry:= NEntry;
         FEntriesShown[N].NNode:= FNNode;
         FEntriesShown[N].Note:= FNote;
         FEntriesShown[N].Content:= GetContentToAssign(NEntry, PanelConfig.MEContent);
         inc(N);
      end;
   end;


 begin

    case PanelConfig.Scope of
      fsSelectedNode: begin
         FNNode:= PanelConfig.SelectedNNode;
         FNote:= FNNode.Note;

         N:= Length(FEntriesShown);

         if EntryToRemove then begin
            for iEntry:= iEntryToConsider to Length(FEntriesShown)-2 do
               FEntriesShown[iEntry]:= FEntriesShown[iEntry+1];
            dec(N);
            SetLength(FEntriesShown, N);
            if (iEntryToConsider < FiEntry) then
               dec(FiEntry)
            else
            if FiEntry > N-1 then
               dec(FiEntry);

            if N = 0 then begin
               FiEntry:= -1;
               FNEntry:= nil;
            end;
         end
         else
         if not EntryToAdd then begin
             FEntriesShown:= nil;

             if ActiveFile.EncryptedContentMustBeHidden and FNote.IsEncrypted then begin
                CannotShow_Encrypted:= True;
                exit;
             end;

             SetLength(FEntriesShown, Note.NumEntries);

             N:= 0;
             if PanelConfig.DescendingOrder then
                 for iEntry:= Length(FEntriesShown)-1 downto 0 do
                    CheckCandidateEntry
             else
                 for iEntry:= 0 to Length(FEntriesShown)-1 do
                    CheckCandidateEntry;

             SetLength(FEntriesShown, N);
         end
         else begin                                         // EntryToAdd = True
            inc(N);
            Created:= NEntryToConsider.Created;
            SetLength(FEntriesShown, N);

            if N = 1 then
               iEntryAdded:= 0

            else begin

              if not PanelConfig.DescendingOrder then begin
                 for iEntry:= N-2 downto 0 do
                    if Created > FEntriesShown[iEntry].NEntry.Created then break;
                 iEntryAdded:= iEntry+1;
                 for iEntry:= iEntryAdded + 1 to N-1 do
                    FEntriesShown[iEntry+1]:= FEntriesShown[iEntry];
              end
              else begin
                 for iEntry:= 0 to N-2 do
                    if Created > FEntriesShown[iEntry].NEntry.Created then break;
                 iEntryAdded:= iEntry;

                 for iEntry:= N-1 downto iEntryAdded + 1 do
                    FEntriesShown[iEntry]:= FEntriesShown[iEntry-1];
              end;
            end;
            if iEntryAdded <= FiEntry then
               inc(FiEntry);
            if FiEntry < 0 then begin
               FiEntry:= 0;
               iEntryToConsider:= 0;
            end;

            FEntriesShown[iEntryAdded].NEntry:= NEntryToConsider;
            FEntriesShown[iEntryAdded].NNode:= FNNode;
            FEntriesShown[iEntryAdded].Note:= FNote;
            FEntriesShown[iEntryAdded].Content:= GetContentToAssign(NEntryToConsider, cmOnlyHeader);
         end;

//       case PanelConfig.Order of
//          eoDateCreation: ;
//          eoHierarchyAndDateCreation: ;       // Use hierarchy in tree + DataCreation
//          eoTagsAndDateCreation: ;            // Use TNoteAdvancedOptions.DefaultTagsOrder + DataCreation
//       end;

      end;

      fsSelectedNodeAndSubtree: ;
      fsSelectedNodeAndAncestors: ;
      fsSelectedNodes: ;      // -> PanelConfig.NNodes
      fsFolder: ;
      fsFile: ;
    end;


    for j:= 0 to Length(PanelConfig.HiddenEntriesDisplayed)-1 do
       for iEntry:= 0 to Length(FEntriesShown)-1 do
          if FEntriesShown[iEntry].NEntry = PanelConfig.HiddenEntriesDisplayed[j] then begin
             FEntriesShown[iEntry].Content:= cmWholeEntry;
             break;
          end;

    for j:= 0 to Length(PanelConfig.EntriesOnlyHeader)-1 do
      for iEntry:= 0 to Length(FEntriesShown)-1 do
         if FEntriesShown[iEntry].NEntry = PanelConfig.EntriesOnlyHeader[j] then begin
            FEntriesShown[iEntry].Content:= GetContentToAssign(FEntriesShown[iEntry].NEntry, cmOnlyHeader, True);
            break;
         end;

    if FPanelInitialized then
       ShowEntriesButtons(Length(FEntriesShown) > 1);
 end;


 // Updates strRTF or cEditor
 procedure PrepareEntryContent (iEntry: integer);
 var
   NEntry: TNoteEntry;
   str: string;
 begin
     if (Mode = meMultipleEntries) then
        cEditor.Clear;

     NEntry:= FEntriesShown[iEntry].NEntry;
     NEntry.Stream.Position := 0;
     strRTF:= '';

     if (Mode = meSingleEntry) or (FEntriesShown[iEntry].Content <> cmOnlyHeader) then begin

         if NEntry.IsEncrypted and ActiveFile.EncryptedContentMustBeHidden then begin
            cEditor.AddText(GetRS(sEdt52));
            exit;
         end;

         if (Mode = meMultipleEntries) and (FEntriesShown[iEntry].Content = cmOnlyFirstLines) then begin
            str:= NEntry.GetExtractOfText(Folder.NoteAdvOptions.ExtractOfText_MaxLength, Folder.NoteAdvOptions.ExtractOfText_MaxLines);
            if str <> '' then begin
               if str[length(str)] <> #13 then
                  str:= str + ' ';
               str:= str + '(...)';
               cEditor.AddText(str);
            end;
            exit;
         end;


         if not NEntry.IsRTF then
            UpdateEditor (cEditor, FKntFolder, False);

         // *1 For newly created, empty notes, this must be ensured (when the note is not intended to be created as plain text. See call to ConfigureEditor).
         //    If we don't do this, we may encounter with an exception when calling LoadFromStream while working with the note, before it
         //    is persisted to the model (for example, when selecting another note from the tree). This can occur if in that situation
         //    we select several lines and press Shift+TAB (to tab multiple lines, decreasing indentation)

         if (not cEditor.PlainText) and (NEntry.Stream.Size = 0) then
            cEditor.StreamFormat:= sfRichText                             // *1

         else begin
           if NodeStreamIsRTF (NEntry.Stream) then begin
              cEditor.StreamFormat:= sfRichText;
              if FEditor.SupportsRegisteredImages then begin
                 ImagesAux:= GetImagesIDInstances (NEntry.Stream, NEntry.TextPlain);
                 strRTF:= ImageMng.ProcessImagesInRTF(NEntry.Stream.Memory, NEntry.Stream.Size, Self.Name, ImageMng.ImagesMode, '', 0, ContainsImgIDsRemoved, ContainsImages, true);
                 if (Mode = meSingleEntry) then
                    fImagesReferenceCount:= ImagesAux
                 else
                    CombineImagesInstances(ImagesAux, fImagesReferenceCount);
              end;
           end
           else
              cEditor.StreamFormat:= sfPlainText;
         end;

         Log_StoreTick('TKntNoteEntriesUI.LoadFromDataModel - BEGIN', 4, +1);
        {$IFDEF KNT_DEBUG}
         if log.Active and  (log.MaxDbgLevel >= 5) then begin
            dataSize:= NEntry.Stream.Size;
            if dataSize > 0 then
               str:= Copy(String(PAnsiChar(NEntry.Stream.Memory)), 1, 90)
            else
               str:= '';
            Log.Add(string.format('sfRichText?:%s DataSize:%d  RTF:"%s"...', [BoolToStr(cEditor.StreamFormat=sfRichText), dataSize, str]),  4 );
         end;
        {$ENDIF}


         if StrRTF <> '' then begin
            if (Mode = meSingleEntry) then begin
               cEditor.PutRtfText(strRTF,True,False);               // => ImageManager.StorageMode <> smEmbRTF
               cEditor.ClearUndo;
            end;
         end
         else
         if NEntry.Stream.Size > 0 then
            cEditor.Lines.LoadFromStream( NEntry.Stream );
     end;

 end;

 procedure ShowHeader(iEntry: integer);
 begin
    Editor.PutRtfText(GetEntryHeader(FEntriesShown[iEntry].Note, FEntriesShown[iEntry].NEntry, (iEntry=0), (FEntriesShown[iEntry].Content=cmOnlyHeader)), True,True);
    FEntriesShown[iEntry].StartingContentPos:= Editor.SelStart;
 end;

 procedure ShowEntry(iEntry: integer);
 var
   TL, SS: integer;
 begin
     ConfigureEditor (iEntry);
     PrepareEntryContent (iEntry);         // -> strRTF or cEditor

     if (Mode = meMultipleEntries) then begin
        FEntriesShown[iEntry].StartingPos:= Editor.SelStart;
        ShowHeader(iEntry);
        if StrRTF = '' then begin
           if cEditor.StreamFormat = sfPlainText then begin
              TL:= cEditor.TextLength;
              cEditor.StreamFormat:= sfRichText;
              cEditor.SetSelection(0, TL, false);
              strRTF:= cEditor.RtfSelText;
           end
           else
              strRTF:= cEditor.RtfText;
        end;
        Editor.PutRtfText(strRTF,True,True);
        SS:= Editor.SelStart;
        if Editor.GetTextRange(SS-1, SS) = #13 then begin
           Editor.SetSelection(SS-1, SS, false);
           Editor.SelAttributes.Size:= 1;
           Editor.SelStart:= SS;
        end;
        FEntriesShown[iEntry].FinalPos:= SS -1;
     end;
 end;

 procedure ReconsiderEntry(iEntry: integer);
 var
   i: integer;
   L, Offset, TL: integer;
 begin
    Offset:= 0;
    for i:= 0 to High(FEntriesShown) do begin
       if (i = iEntry) then begin
           if EntryToRemove or not (ActionOnEntry in [aModifiedMetadata, aRefreshHeader]) then begin
              L:= FEntriesShown[i].FinalPos - FEntriesShown[i].StartingPos;
              Editor.SetSelection(FEntriesShown[i].StartingPos, FEntriesShown[i].FinalPos+1, false);
           end
           else begin
              L:= FEntriesShown[i].StartingContentPos - FEntriesShown[i].StartingPos;
              Editor.SetSelection(FEntriesShown[i].StartingPos, FEntriesShown[i].StartingContentPos, false);
           end;

           if EntryToRemove or ((ActionOnEntry = aChangedVisibility) and (FEntriesShown[i].Content = cmHidden)) then begin
              Offset:= - L - 1;
              Editor.SelText:= '';
              if not EntryToRemove then begin
                 FEntriesShown[i].StartingContentPos:= FEntriesShown[i].StartingPos;
                 FEntriesShown[i].FinalPos:= FEntriesShown[i].StartingPos;
              end;
           end
           else begin
              if not (ActionOnEntry in [aModifiedMetadata, aRefreshHeader]) then begin
                 ShowEntry (i);
                 Offset:= (FEntriesShown[i].FinalPos - FEntriesShown[i].StartingPos) - L;
              end
              else begin
                 ShowHeader(i);
                 Offset:= (FEntriesShown[i].StartingContentPos - FEntriesShown[i].StartingPos) - L;
                 inc(FEntriesShown[i].FinalPos, Offset);
              end;
           end;
       end
       else
       if (Offset <> 0) then begin
          inc(FEntriesShown[i].StartingPos, Offset);
          inc(FEntriesShown[i].StartingContentPos, Offset);
          inc(FEntriesShown[i].FinalPos, Offset);
       end;
    end;

 end;


 procedure SaveContentStateOfEntries;
 var
    i, N: integer;
 begin
    SetLength(PanelConfig.EntriesOnlyHeader, Length(FEntriesShown));
    N:= 0;
    for i:= 0 to Length(FEntriesShown)-1 do
        if FEntriesShown[i].Content = cmOnlyHeader then begin
           PanelConfig.EntriesOnlyHeader[N]:= FEntriesShown[i].NEntry;
           inc(N);
        end;
    SetLength(PanelConfig.EntriesOnlyHeader, N);


    SetLength(PanelConfig.HiddenEntriesDisplayed, Length(FEntriesShown));
    N:= 0;
    for i:= 0 to Length(FEntriesShown)-1 do
        if (FEntriesShown[i].NEntry.IsHidden) and (FEntriesShown[i].Content <> cmHidden) then begin
           PanelConfig.HiddenEntriesDisplayed[N]:= FEntriesShown[i].NEntry;
           inc(N);
        end;
    SetLength(PanelConfig.HiddenEntriesDisplayed, N);
 end;


 procedure ShowNewEntryToAdd;
 var
   iEntry: integer;
 begin
    if iEntryAdded = 0 then
       Editor.SelStart:= 0
    else
       Editor.SelStart:= FEntriesShown[iEntryAdded-1].FinalPos + 1;

    ShowEntry (iEntryAdded);
    Offset:= (FEntriesShown[iEntryAdded].FinalPos - FEntriesShown[iEntryAdded].StartingPos) + 1;
    for iEntry:= iEntryAdded+1 to Length(FEntriesShown)-1 do begin
        inc(FEntriesShown[iEntry].StartingPos, Offset);
        inc(FEntriesShown[iEntry].StartingContentPos, Offset);
        inc(FEntriesShown[iEntry].FinalPos, Offset);
    end;

    Editor.SelStart := FEntriesShown[FiEntry].StartingPos;
    Editor.SelStart := FEntriesShown[FiEntry].StartingContentPos + PanelConfig.SelStart;
    Editor.SelLength := PanelConfig.SelLength;
    //inc(PanelConfig.ScrollPosInEditor.Y, 35);                    // TODO ***
    Editor.SetScrollPosInEditor(PanelConfig.ScrollPosInEditor);
 end;

 procedure ClearAndSetAsEmpty;
 begin
    Editor.Clear;
    fImagesReferenceCount:= nil;
    PanelConfig.CurrentMode:= meSingleEntry;
    Mode:= meSingleEntry;
    FiEntry:= -1;
    FNEntry:= nil;
 end;


begin
   if (PanelConfig = nil) or ((PanelConfig.Scope = fsSelectedNode) and (PanelConfig.SelectedNNode = nil)) then begin
      FNNode:= nil;
      FNote:= nil;
      ConfigureEditor;
      ReloadMetadataFromDataModel;
      exit;
   end;


   EntryToAdd:= false;
   EntryToRemove:= false;
   Mode:= PanelConfig.CurrentMode;


   { *1
     If the note already has at least 2 entries, we will show the entry we have identified but avoided displaying
     in the editor, so as not to offer two identical copies of the same entry }
   if (PanelConfig.StLayout = spInQL_ets) then
      if (FNote <> nil) and (FNote.NumEntries <= 1) or (ActionOnEntry = aCreated) then
         exit
      else begin
         CalculateEntriesToShow:= True;
         ActionOnEntry:= aNull;
      end;


   CannotShow_Encrypted:= False;
   if CalculateEntriesToShow then begin
      FNNode:= nil;
      FNote:= nil;
      FNEntry:= nil;
      NEntryToConsider:= nil;

      PopulateEntriesToShow;

      if (FEntriesShown = nil) and (PanelConfig.StLayout = spInQL_ets) then      // *1
         exit;

      if (ActionOnEntry in [aCreating, aCreatingFromOtherPanel]) and (PanelConfig.VinculatedTags = nil) then begin
         // We must be in a note with one entry where a new entry is being created from this or other panel (a "Single Entry" panel)
      end
      else
      if (PanelConfig.StLayout = spInQL) and (PanelConfig.Panel <> pnCenter) and (FNote <> nil) and (FNote.NumEntries = 1) then begin  // *1
         PanelConfig.StLayout:= spInQL_ets;
         PanelHidden:= True;
         exit;
      end
      else
      if Length(FEntriesShown) <= 1 then
         PanelConfig.CurrentMode:= meSingleEntry;
   end;



   Mode:= PanelConfig.CurrentMode;
   FNEntry_Initial:= FNEntry;
   FiEntry_Initial:= FiEntry;

   NumVisibleEntriesBefore:= NumberOfIncludedEntries(True);

   // NEntryToConsider: If it's included among the considered entries, check if it should remain so and, if so,redisplay it, using its current content and tags.
   //   If FMode = meSingleEntry, this NEntryToReconsider will be reflected in FEntriesShown, but it doesn't necessarily have to be reflected in the editor if
   //    the entry displayed there is different.
   //   If aCreated  -> Check if it should be included in the panel
   //   If aModified -> Check if it is included and if it should be included o removed. Content will be updated if added or maintained.
   //   If aModifiedMetadata -> Check if it is included and if it should be included o removed. If already included , content doesn't need to be updated
   //   If aDeleted -> Remove if it is present
   //
   // PanelConfig.SelNEntry: Indicates which entry should be displayed, if FMode = meSingleEntry, or, in the case of FMode = meMultipleEntries, which entry
   //   should be selected, the one containing the cursor. In both cases, it will determine the number of the entry displayed on the button associated with btnToggleMulti.

   iSelectedEntry:= GetIndexOfIncludedEntry(PanelConfig.SelNEntry);

   if (NEntryToConsider <> nil) then begin
       if not (ActionOnEntry in [aDeleted, aChangedVisibility]) then
          MustBeIncluded:= NEntryMustBeIncludedInPanel(NEntryToConsider);

       if (ActionOnEntry = aCreated) then begin
          if not MustBeIncluded then exit;
          EntryToAdd:= true;
          PopulateEntriesToShow;
          if (Mode = meSingleEntry) then begin
              if (FiEntry_Initial = -1) and ((FNEntry = nil) or (FNEntry = NEntryToConsider)) then begin
                // We've already prepared the editor. Once the first modification is made, we'll enter
                // here, and what we need to do is update the information corresponding to the entry, making
                // the information panel temporarily visible
                 btnToggleMulti.Caption:= (iEntryAdded+1).ToString;
                 FiEntry:= iEntryAdded;
                 FNNode:= FEntriesShown[iEntryAdded].NNode;
                 FNote:= FEntriesShown[iEntryAdded].Note;
                 FNEntry:= NEntryToConsider;                   // In case FNEntry was nil (eg: Ctrl+Shift+Intro in empty panel vinculated to tags)
                 FEditor.OnEditorChanged:= nil;                //  ,,
                 ConfigureEditor;
                 if PanelConfig.StLayout = spInEL then
                    FramResizePendingInNoteUI:= TKntNoteUI(NoteUI);
                 ShowControlsPanelIdentif(True);
                 FNoteUI.KeepInfoPanelTemporarilyVisible;
              end
              else
                 btnToggleMulti.Caption:= (FiEntry+1).ToString;

              exit;
          end;
       end
       else begin
          iEntryToConsider:= GetIndexOfIncludedEntry(NEntryToConsider);
          if (ActionOnEntry = aDeleted) then begin
             if (iEntryToConsider < 0) then exit;
             EntryToRemove:= true;
          end
          else
          if (ActionOnEntry in [aModified, aModifiedMetadata]) then begin
             if MustBeIncluded and (iEntryToConsider < 0) then begin
                EntryToAdd:= true;
                PopulateEntriesToShow;
             end
             else
             if not MustBeIncluded and (iEntryToConsider >= 0) then
                EntryToRemove:= true;
          end;
       end;

       if (Mode = meSingleEntry) and not EntryToRemove and not EntryToAdd and
           ( (ActionOnEntry = aModifiedMetadata) or
             ((ActionOnEntry = aModified) and (FNEntry <> nil) and (FNEntry <> NEntryToConsider) )) then begin

          ReloadMetadataFromDataModel;
          exit;
       end;
   end;

   if FPanelHidden and not (EntryToAdd or (PanelConfig.StLayout = spInQL_ets)) then exit;


   if EntryToRemove then
       if (Mode = meSingleEntry) then begin
          PopulateEntriesToShow;
          if iEntryToConsider <> FiEntry_Initial then
             exit
          else
          if FEntriesShown <> nil then begin
             btnToggleMultiClick(nil);
             exit;
          end;                             // ELSE -> Continue: Editor.Clear, ...
       end
       else begin  // Mode = meMultipleEntries
          if (Length(FEntriesShown) = 2) then begin
              PopulateEntriesToShow;
              btnToggleMultiClick(nil);
              exit;
          end;
       end;


   if EntryToAdd then begin
      if ( (Length(FEntriesShown) = 2) and (PanelConfig.MainMode = meMultipleEntries) and (PanelConfig.VinculatedTags = nil) ) then begin
         EntryToAdd:= false;          // Process the two entries, not just the one to add
         NEntryToConsider:= nil;
      end;

      // See *1, above
      if (PanelConfig.StLayout = spInQL) and (PanelConfig.Panel <> pnCenter) and (FNote <> nil) and (FNote.NumEntries = 1) then begin
         PanelConfig.StLayout:= spInQL_ets;
         PanelHidden:= True;
         exit;
      end;


      if (Mode = meSingleEntry) then begin
         if (Length(FEntriesShown) = 2) then begin
            if (PanelConfig.VinculatedTags <> nil) then begin
               if FiEntry < 0 then
                  FiEntry:= 0;
               if FEntriesShown[FiEntry].Content <> cmHidden then
                  FEntriesShown[FiEntry].Content:= cmWholeEntry;
               PanelConfig.CurrentMode:= meMultipleEntries;
               Mode:= meMultipleEntries;
               EntryToAdd:= false;
               NEntryToConsider:= nil;
            end
         end
         else
         if (FNEntry <> nil) then begin
            btnToggleMulti.Caption:= (FiEntry+1).ToString;     // First entry (1) can now be second entry (2). Show it in the navigate button
            exit;
         end;
      end;
   end;


   if ( (EntryToAdd and PanelConfig.Hidden) or
        ((ActionOnEntry = aNull) and (PanelConfig.StLayout = spInQL_ets)) ) and
       (Length(FEntriesShown) >= 1) and (FEntriesShown[0].NEntry.Stream.Size = 0) then begin
      // If we're changing the metadata of a newly created entry, and this should make a panel visible,
      // we'll make sure to display it in multi-entry mode, showing only the header.
       PanelConfig.CurrentMode:= meMultipleEntries;
       Mode:= meMultipleEntries;
       FEntriesShown[0].Content:= cmOnlyHeader;
       NEntryToConsider:= nil;
   end;


   // Content of editor must be reviewed

   Editor.BeginUpdate;                   // -> It will also ignore Enter and Change events

   if (Mode = meMultipleEntries) then begin
      if RTFAux = nil then
         RTFAux:= CreateAuxRichEdit();
      cEditor:= RTFAux;
      cEditor.BeginUpdate;
   end
   else
      cEditor:= FEditor;

   SetReadOnly(FKntFolder.ReadOnly);
   ReadOnlyBAK:= FReadOnly;

   ContainsImgIDsRemoved:= false;

   try                                                         // -------------------------------- TRY

     fChangingInCode:= True;
     Editor.ReadOnly:= false;   // To prevent the problem indicated in issue #537

     if (Mode = meMultipleEntries) then begin
         if EntryToAdd then begin
            ShowNewEntryToAdd;
            if FNEntry = nil then
               FiEntry:= 0;
            exit;
         end;

         if EntryToRemove then begin
            ReconsiderEntry(iEntryToConsider);
            PopulateEntriesToShow;
            if (FiEntry >= 0) and (NumberOfIncludedEntries(true) > 0) then begin
               if iEntryToConsider = FiEntry_Initial then begin
                  SS:= FEntriesShown[FiEntry].StartingContentPos;
                  PanelConfig.SelStart:= SS;
                  PanelConfig.SelLength:= 0;
                  Editor.SelStart:= SS;
               end;
               exit;
            end
            else begin
               PanelConfig.CurrentMode:= meSingleEntry;
               Mode:= meSingleEntry;
               NEntryToConsider:= nil;
            end;
         end;
     end;


     if (Mode = meSingleEntry) or (NEntryToConsider = nil) then begin
        Editor.Clear;
        Editor.ClearUndo;
        fImagesReferenceCount:= nil;
     end;

     Editor.Color:= GetColor(NNode.EditorBGColor, FKntFolder.EditorChrome.BGColor);

     FiEntry:= -1;
     if FEntriesShown <> nil then begin
       FiEntry:= iSelectedEntry;
       if ((FiEntry < 0) or (FiEntry > Length(FEntriesShown)-1))
                    and (ActionOnEntry <> aCreating) then begin

           FiEntry:= 0;
           if Folder.NoteAdvOptions.ShowNewestEntryAtStartup then begin
              if not PanelConfig.DescendingOrder then        // Descending -> 0 => most recent
                  FiEntry:= Length(FEntriesShown)-1;
           end
           else begin
              if PanelConfig.DescendingOrder then
                 FiEntry:= Length(FEntriesShown)-1;
           end;
       end;

       if CalculateEntriesToShow and (FiEntry >= 0) and (FEntriesShown[FiEntry].Content = cmHidden) and
          (not FEntriesShown[FiEntry].NEntry.IsEncrypted or not ActiveFile.EncryptedContentMustBeHidden)  then
          FEntriesShown[FiEntry].Content:= cmWholeEntry;

       // We might have an encrypted entry selected, which then becomes hidden. We must select a non-hidden entry.
       // We'll start by selecting the entry immediately below it, and if there isn't one, the one immediately above it.
       if (FiEntry >= 0) and (FEntriesShown[FiEntry].Content = cmHidden) then begin
           PanelConfig.SelStart:= 0;
           PanelConfig.SelLength:= 0;
           iEntry:= FiEntry;
           FiEntry:= -1;
           for i:= iEntry + 1 to Length(FEntriesShown)-1 do
              if (FEntriesShown[i].Content <> cmHidden) then begin
                  FiEntry:= i;
                  break;
              end;
           if FiEntry = -1 then begin
              for i:= iEntry -1 downto 0 do
                  if (FEntriesShown[i].Content <> cmHidden) then begin
                      FiEntry:= i;
                      break;
                  end;
           end;

           if FiEntry = -1 then begin
              ClearAndSetAsEmpty;
              exit;
           end;
       end;



       if Mode = meMultipleEntries then begin    // --- meMultipleEntries
          if NEntryToConsider <> nil then begin
             ReconsiderEntry(iEntryToConsider);
             if (ActionOnEntry = aChangedVisibility) and (NumberOfIncludedEntries(true) = 0) then
                ClearAndSetAsEmpty;
          end
          else begin
              var pos: integer:= 0;
              FEntriesShown[0].StartingContentPos:= 0;
              FEntriesShown[0].FinalPos:= 0;
              FEntriesShown[0].StartingContentPos:= 0;

              for iEntry:= 0 to Length(FEntriesShown)-1 do begin
                 if (FEntriesShown[iEntry].Content <> cmHidden) then
                    ShowEntry (iEntry)
                 else begin
                    if (iEntry > 0) then begin
                       pos:= FEntriesShown[iEntry-1].FinalPos;
                       if FEntriesShown[iEntry-1].Content <> cmHidden then
                          inc(pos);
                       FEntriesShown[iEntry].StartingPos:= pos;
                       FEntriesShown[iEntry].StartingContentPos:= pos;
                       FEntriesShown[iEntry].FinalPos:= pos;
                    end;
                 end;
              end;

              inc(FEntriesShown[High(FEntriesShown)].FinalPos);      // Last shown entry in the editor
          end;
       end
       else begin                              // --- meSingleEntry
          if (NEntryToConsider <> nil) and (FEntriesShown[iEntryToConsider].Content <> cmHidden) then
             ReconsiderEntry(iEntryToConsider)
          else
          if FiEntry >= 0 then
             ShowEntry (FiEntry)
          else begin
             FNEntry:= nil;
             PanelConfig.SelStart:= 0;
             PanelConfig.SelLength:= 0;
          end;

       end;


       if (Mode = meSingleEntry) and (FNEntry <> nil) and (FNEntry.Stream.Size = 0) then     // Ensures that new nodes are correctly updated based on default properties (font color, size, ...)
          UpdateEditor (Editor, FKntFolder, false);


       SS:= PanelConfig.SelStart;
       SL:= PanelConfig.SelLength;
       if (Mode = meSingleEntry) then begin
          Editor.SelStart := SS;
          Editor.SelLength := SL;
       end
       else begin
          Editor.SelStart := FEntriesShown[FiEntry].StartingPos;
          Editor.SelStart := FEntriesShown[FiEntry].StartingContentPos + SS;
          Editor.SelLength := SL;
       end;


     end
     else begin                    // FEntriesShown = nil and not: (PanelConfig.Scope = fsSelectedNode) and (PanelConfig.SelectedNNode = nil)
        if CannotShow_Encrypted then begin
           FEditor.AddText(GetRS(sEdt52));
           ReadOnlyBAK:= True;
        end;
     end;



   finally

     if (FEntriesShown <> nil) and (FiEntry >= 0) then begin
        FNNode:= FEntriesShown[FiEntry].NNode;
        FNote:= FEntriesShown[FiEntry].Note;
        FNEntry:= FEntriesShown[FiEntry].NEntry;
     end;

     ReloadMetadataFromDataModel;

     if (FNEntry <> nil) and (Mode = meSingleEntry) and
        (FNEntry.IsReadOnly or (FNEntry.IsEncrypted and ActiveFile.EncryptedContentMustBeHidden) ) then
        ReadOnlyBAK:= True;

     ForceTempReadOnly(ReadOnlyBAK);
     if (Mode = meMultipleEntries) then
        Editor.ReadOnly:= true;

     Editor.ZoomCurrent:= PanelConfig.ZoomCurrent;
     Editor.RestoreZoomCurrent;

     if (PanelConfig.ScrollPosInEditor.Y > 0) then
        Editor.SetScrollPosInEditor(PanelConfig.ScrollPosInEditor);

     if not CalculateEntriesToShow and (FNEntry_Initial <> FNEntry) then
        App.NEntrySelected(Editor, FNEntry);
     ConfigureEditor;


     btnToggleMulti.Caption:= (FiEntry+1).ToString;

     Editor.SetLangOptions(False);
     Editor.EndUpdate;

     if not ContainsImgIDsRemoved then
        Editor.Modified := false;

     Editor.CheckWordCount(true);

     Editor.ChangedSelection;
     Editor.Change;

     if RTFAux <> nil then
        RTFAux.Clear;

     if InformReloaded and not ClipCapMng.IsBusy then
        App.EditorReloaded(Editor, Editor.Focused);

     SaveContentStateOfEntries;
     fChangingInCode:= false;


     if not FPanelHidden and (FNEntry = nil) then
        FNoteUI.PanelEmpty(PanelConfig.Panel, (NumberOfIncludedEntries(true) = 0))
     else
     if FPanelHidden and (EntryToAdd or (PanelConfig.StLayout = spInQL_ets)) then
        FNoteUI.ShowEntriesUIPanel(PanelConfig.Panel, True);

     if PanelConfig.StLayout = spInQL_ets then
        PanelConfig.StLayout:= spInQL;


     if PanelConfig.StLayout = spInEL then begin
        NumVisibleEntriesAfter:= NumberOfIncludedEntries(True);
        if (NumVisibleEntriesBefore <> NumVisibleEntriesAfter) and (NumVisibleEntriesBefore * NumVisibleEntriesAfter = 0) then
           FramResizePendingInNoteUI:= TKntNoteUI(NoteUI);
     end;

   end;


  Editor.Enabled:= true;
  txtName.Enabled:= True;
  txtTags.Enabled:= true;
  txtCreationDate.Enabled:= True;
  btnOptions.Enabled:= true;
end;


function TKntNoteEntriesUI.IsDisplayingEntry(NEntry: TNoteEntry; var Content: TContentInMultiEntriesMode): boolean;
var
   i: integer;
begin
   Result:= False;
   i:= GetIndexOfIncludedEntry(NEntry);
   if i >= 0 then begin
      Result:= True;
      Content:= FEntriesShown[i].Content;
   end;
end;


function TKntNoteEntriesUI.NumberOfIncludedEntries(OnlyNotHidden: boolean): integer;
var
   i: integer;
begin
    if OnlyNotHidden then begin
       Result:= 0;
       for i:= Length(FEntriesShown)-1 downto 0 do
          if FEntriesShown[i].Content <> cmHidden then
             inc(Result);
    end
    else
       Result:= Length(FEntriesShown);
end;

function TKntNoteEntriesUI.DisplayingAnyHiddenEntry: boolean;
var
   i: integer;
begin
   Result:= False;
   for i:= 0 to Length(FEntriesShown)-1 do
      if FEntriesShown[i].NEntry.IsHidden and (FEntriesShown[i].Content <> cmHidden) then
         exit(true);
end;

function TKntNoteEntriesUI.HasAnyEntryNonVisible: boolean;
var
   i: integer;
begin
   Result:= False;
   for i:= 0 to Length(FEntriesShown)-1 do
      if (FEntriesShown[i].Content = cmHidden) and not (FEntriesShown[i].NEntry.IsEncrypted and ActiveFile.EncryptedContentMustBeHidden) then
         exit(true);
end;


procedure TKntNoteEntriesUI.GetEntryBoundaries(NEntry: TNoteEntry; var PosStartEntry: integer; var PosEndEntry: integer);
var
   i: integer;
begin
   PosStartEntry:= 0;
   PosEndEntry:= -1;

   if (PanelConfig = nil) or (PanelConfig.CurrentMode = meSingleEntry) then exit;

   i:= GetIndexOfIncludedEntry(NEntry);
   if i >= 0 then begin
      PosStartEntry:= FEntriesShown[i].StartingContentPos;
      PosEndEntry:= FEntriesShown[i].FinalPos;
   end;
end;



function TKntNoteEntriesUI.GetPreparedForJump(NEntry: TNoteEntry; var PosStartEntry: integer; var PosEndEntry: integer; AllowEdit: boolean = false): boolean;
 var
    i: integer;

 begin
    Result:= False;
    PosStartEntry:= 0;
    PosEndEntry:= -1;

    if (PanelConfig.CurrentMode = meSingleEntry) and (FNEntry = NEntry) then
       exit (true)

    else begin
       i:= GetIndexOfIncludedEntry(NEntry);
       if i >= 0 then begin
          Result:= True;
          if (PanelConfig.CurrentMode = meMultipleEntries) then begin
             if (FEntriesShown[i].Content in [cmOnlyHeader, cmHidden]) then begin
                PanelConfig.SelNEntry:= NEntry;
                ReloadVisibleContentOfEntries (false, cmWholeEntry, i);
             end;

             PosStartEntry:= FEntriesShown[i].StartingContentPos;
             PosEndEntry:= FEntriesShown[i].FinalPos;
             if AllowEdit then
                btnToggleMultiClick(nil)
          end;

          SelectEntry(i);
       end;
    end;
end;



(*
function TKntNoteEntriesUI.GetHeaderCellx: AnsiString;
var
  w, widthTwips: integer;
begin
      w:= Editor.Width;
      if KeyOptions.AltMargins then
         w:= w - KeyOptions.MarginAltLeft - KeyOptions.MarginAltRight;

      widthTwips := DotsToTwips(w - 30);
      //widthTwips := 999999;
      Result:= '\cellx' + widthTwips.ToString;
end;

function TKntNoteEntriesUI.GetEntryHeader (Note: TNote; NEntry: TNoteEntry; FirstHeader: boolean = False): AnsiString;
var
  str, strFontSize, strHiddenMarkB: AnsiString;
  s, strInfo, strEntrID: string;
  strEntryID: string;

begin
   if FLastEditorUIWidth = '' then
      FLastEditorUIWidth:= GetHeaderCellx;

   strFontSize:= (2 * 10).ToString + ' ';
   strEntryID:= Format('%d.%d', [Note.GID, NEntry.ID]);
   strInfo:= '';
   if PanelConfig.MMShowTagsInHeader and (Length(NEntry.Tags) > 0) then begin
      strInfo:= ' #[' + Trim(NEntry.TagsNames) + ']     ';
   end;
   if PanelConfig.MMShowDateInHeader and (NEntry.Created <> 0) then begin
      if (NEntry.Created).GetTime <> 0 then
         S:= ' - ' + FormatSettings.ShortTimeFormat;
      strInfo:= strInfo + FormatDateTime(FormatSettings.ShortDateFormat + S, NEntry.Created);
   end;
   strInfo := strInfo + '   \u8203.';                      // '\u200B'  Zero-Width Space  (invisible)

   strHiddenMarkB:= '\v' + KNT_RTF_HIDDEN_MARK_L + KNT_RTF_HIDDEN_DATA + strEntryID + KNT_RTF_HIDDEN_MARK_R + '\v0';

   str:= '{\rtf1\ansi{\colortbl ;\red102\green102\blue102;}';
   if FirstHeader then
      str:= str + Format('\qr\cf1\fs%s %s\sa40\par}', [strFontSize, strInfo])
   else
      str:= str + Format('\fs5\par\par\trowd\trgaph0%s \intbl\fs1%s\cell\row\pard\qr\cf1\fs%s %s\sa40\par}',
                          [FLastEditorUIWidth, strHiddenMarkB, strFontSize, strInfo]);
   Result:= str;
end;


procedure TKntNoteEntriesUI.UpdateEntriesHeaderWidth(EnsureRefreshOnEditor: boolean);
var
  sRTF, cellWidth: string;
  strPlain: string;
  strHeader: string;
  strHiddenMark: AnsiString;
  p, pE, L,L2: integer;
  incOffset, Offset: integer;
  SSBak, SSLen: integer;

begin
   if FLastEditorUIWidth <> '' then begin
      cellWidth:= GetHeaderCellx;
      L:= cellwidth.Length;
      if FLastEditorUIWidth = cellWidth then exit;

      Offset:= 0;
      incOffset:= L - Length(FLastEditorUIWidth);

      strHiddenMark:= KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_DATA;
      L2:= Length(strHiddenMark);
      with Editor do begin
          ReadOnly:= False;
          strPlain:= TextPlain;
          p:= Pos(strHiddenMark, strPlain);
          BeginUpdate;
          SSBak:= SelStart;
          SSLen:= SelLength;
          while p > 0 do begin
             pE:= Pos(#13, strPlain, p + 1);
             if pE = 0 then break;

             Editor.SetSelection(p-1 + Offset, pE + Offset, False);
             sRTF:= Editor.RtfSelText;
             sRTF:= StringReplace(sRTF, FLastEditorUIWidth, cellWidth, []);
             Editor.RtfSelText:= sRTF;
             p:= Pos(strHiddenMark, strPlain, p+1);
             inc(Offset, incOffset);
          end;
          SelStart:= SSBak;
          SelLength:= SSLen;
          EndUpdate;

          ReadOnly:= True;
          Modified:= False;
      end;
      FLastEditorUIWidth:= cellWidth;
   end
   else
   if EnsureRefreshOnEditor then
      Editor.Refresh;
end;
*)


function TKntNoteEntriesUI.GetEntryHeader (Note: TNote; NEntry: TNoteEntry; FirstEntry: boolean = False; Folded: boolean = False): AnsiString;
var
  strLine: AnsiString;
  s, strIni, strInfo, strSA, strFontInfo: string;
  MainIni, MainEnd: string;
  EditorBackColor, ColorLine, ColorInfo: TColor;
  ShowTags, ShowDate: boolean;

begin
   // # ToDO —  08/11/2025 - 11:36  —

   if NEntry.IsMain then begin
      MainIni:= '{\ul ';
      MainEnd:= '}';
   end;


   ShowTags:= false;
   ShowDate:= false;

   if PanelConfig.MEShowTagsInHeader and (Length(NEntry.Tags) > 0) then begin
      strInfo:= '# ' + Trim(NEntry.TagsNames);
      ShowTags:= true;
   end;
   if PanelConfig.MEShowDateInHeader and (NEntry.Created <> 0) then begin
      if ShowTags then
         strInfo:= strInfo + '  · ';
      if (NEntry.Created).GetTime <> 0 then
         S:= ' - ' + FormatSettings.ShortTimeFormat;
      strInfo:= strInfo + FormatDateTime(FormatSettings.ShortDateFormat + S, NEntry.Created);
      ShowDate:= true;
   end;

   strInfo:= strInfo + MainEnd;

   if PanelConfig.MEShowLineInHeader then begin
      strLine:= GetRTFPrintableLineAux(999999);
      strInfo := strInfo + ' \u8203.';                      // '\u200B'  Zero-Width Space  (invisible)
   end
   else
   if ShowTags or ShowDate then
      strInfo := strInfo + ' —'
   else
      strInfo := strInfo + '———';


   if Folded then
      strIni:= ' \u10133+ '         // ➕
   else
   if not PanelConfig.MEShowLineInHeader and (ShowTags or ShowDate) then
      strIni:= ' — '
   else
      strIni:= '   ';

   strInfo:= strIni + MainIni + strInfo;

   if not PanelConfig.MECompactHeader or (ShowDate or ShowTags or Folded) then
      strSA:= '\sa80';

   if not PanelConfig.MECompactHeader or (ShowDate or ShowTags or Folded or not PanelConfig.MEShowLineInHeader) then
      strFontInfo:= '\fs18 '
   else
      strFontInfo:= '\fs4 ';

   (*
   if PanelConfig.MMShowLineInHeader then
      strLine:= '\fs1\par\trowd\trgaph0\cellx999999 \intbl\fs1\cell\row\pard';

   Result:= '{\rtf1\ansi{\colortbl ;' + GetRTFColor(clWebDarkBlue) + ';}' + StrLine + '\qr\cf1\b\fs18 ' + strInfo + '\sa80\par}';' +
   *)


   // Change the header color according to the brightness of the background color, as happens with hyperlinks

   EditorBackColor:= ColorToRGB(Editor.Color);
   if GetRelativeLuminosity(EditorBackColor) >= 0.45 then begin
      ColorLine:= clSilver;
      ColorInfo:= RGB(100,100,100);
   end
   else begin
      ColorLine:= RGB(155,155,155);
      ColorInfo:= RGB(220,220,220);
   end;

   if NEntry.IsEncrypted and ActiveFile.HighlightProtectedNodesAndEntries then
      ColorInfo:= clRed;


   Result:= '{\rtf1\ansi{\colortbl ;' + GetRTFColor(EditorBackColor) + ';' +
                                        GetRTFColor(ColorLine) + ';' +
                                        GetRTFColor(ColorInfo) + ';}\fs1\par' +
              StrLine + '\qr\cf3\b' + strFontInfo  + strInfo + strSA + '\par}';
end;


procedure TKntNoteEntriesUI.RefreshEntry;
begin
   Editor.Refresh;
end;


// Previously: EditorToDataStream

procedure TKntNoteEntriesUI.SaveToDataModel;
var
   KeepUTF8: boolean;
   Encoding: TEncoding;
   strRTF: AnsiString;
   ImagesIDs_New: TImageIDs;
   TextPlain: string;

begin
  Encoding:= nil;

  if assigned(NNode) and (FNEntry <> nil) and (PanelConfig.CurrentMode <> meMultipleEntries) then begin
     if (FEditor.FloatingEditor <> nil) then
        FEditor.DoSaveChangesInFloatingEditor;

     if FEditor.Modified then begin
        FEditor.BeginUpdate;
        try
           KeepUTF8:= False;
           if NNode.IsVirtual and FNEntry.IsPlainTXT and NodeStreamIsUTF8WithBOM(FNEntry.Stream) then
               KeepUTF8:= True;

           FNEntry.Stream.Clear;

           try
             FEditor.StreamFormat:= StreamFormatInNEntry(FNEntry);
             FEditor.StreamMode := [];
             if FEditor.StreamFormat = sfPlainText then begin
                // If it is a virtual node we will respect the UTF8 encoding it may have.
                // Otherwise it will only be saved as UTF8 if necessary
                if KeepUTF8 or not CanSaveAsANSI(FEditor.Text) then
                   Encoding:= TEncoding.UTF8;
             end;

             FEditor.Lines.SaveToStream( FNEntry.Stream, Encoding);

             ImagesIDs_New:= nil;
             if FEditor.SupportsRegisteredImages then begin
                ImagesIDs_New:= FKntFolder.CheckSavingImagesOnMode (imLink, FNEntry.Stream, true);
                ImageMng.UpdateImagesCountReferences (fImagesReferenceCount, ImagesIDs_New);
                fImagesReferenceCount:= ImagesIDs_New;
             end;

             if ImagesIDs_New = nil then
                FNEntry.TextPlain:= FEditor.TextPlain
             else begin
                { If the node has images we will make sure that in TextPlain we save the version corresponding to imLink,
                  to facilitate search management. See notes on TImageMng.GetPositionOffset }
                FNEntry.TextPlain := '';
                InitializeTextPlain(FNEntry, RTFAux_Note);
             end;
             FNEntry.Stream.Position := 0;
             FEditor.Modified:= false;

           finally
             FEditor.StreamFormat := sfRichText;
             FEditor.StreamMode := [];
           end;


        finally
          FEditor.EndUpdate;
          App.EditorSaved(FEditor);
        end;
     end
     else begin
       if (FNEntry <> nil) and (FNEntry.TextPlain = '') then
          InitializeTextPlain(FNEntry, RTFAux_Note);
     end;

  end;
end;


procedure TKntNoteEntriesUI.SaveToDataModel (RTFAux: TAuxRichEdit; NEntry: TNoteEntry);
var
   KeepUTF8: boolean;
   Encoding: TEncoding;

begin
   Encoding:= nil;

   KeepUTF8:= False;
   if FNNode.IsVirtual and NEntry.IsPlainTXT and NodeStreamIsUTF8WithBOM(NEntry.Stream) then
      KeepUTF8:= True;

   NEntry.Stream.Clear;
   if RTFAux.StreamFormat = sfPlainText then begin
      // If it is a virtual node we will respect the UTF8 encoding it may have.
      // Otherwise it will only be saved as UTF8 if necessary
      if KeepUTF8 or not CanSaveAsANSI(RTFAux.Text) then
         Encoding:= TEncoding.UTF8;
   end;

   RTFAux.Lines.SaveToStream( NEntry.Stream, Encoding);

   NEntry.TextPlain:= RTFAux.TextPlain;
   NEntry.Stream.Position := 0;
end;


procedure TKntNoteEntriesUI.SavePositionInPanel;
begin
   if PanelConfig.StLayout = spInQL_ets then exit;

   if FEntriesShown = nil then begin
      FNEntry:= nil;
      FiEntry:= -1;
   end;

   PanelConfig.ScrollPosInEditor:= Editor.GetScrollPosInEditor;
   PanelConfig.SelNEntry := FNEntry;
   PanelConfig.SelStart  := Editor.SelStart;
   PanelConfig.SelLength := Editor.SelLength;
   Editor.GetAndRememberCurrentZoom;
   PanelConfig.ZoomCurrent:= Editor.ZoomCurrent;

   if (PanelConfig.CurrentMode = meMultipleEntries) and (FEntriesShown <> nil) and (FiEntry >= 0) then begin
      dec(PanelConfig.SelStart, FEntriesShown[FiEntry].StartingContentPos);
      if PanelConfig.SelStart < 0 then begin
         PanelConfig.SelStart := 0;        // Can occur if the entry is collapsed and only shown its header
         PanelConfig.SelLength := 0;
      end;
   end;

end;


procedure TKntNoteEntriesUI.ReloadNoteName;
begin
   txtName.Text:= FNote.Name;
end;

procedure TKntNoteEntriesUI.SelectPrevEntry(InformReloaded: boolean);
var
   iNextEntry: integer;
   SS: integer;
begin
   SS:= Editor.SelStart;

   if (FiEntry > 0) or ((PanelConfig.CurrentMode = meMultipleEntries) and (SS > FEntriesShown[FiEntry].StartingContentPos)) then begin
      iNextEntry:= FiEntry;
      repeat
         if (PanelConfig.CurrentMode = meSingleEntry) or (SS <= FEntriesShown[iNextEntry].StartingContentPos) then
            dec(iNextEntry);
         if FEntriesShown[iNextEntry].Content <> cmHidden then begin
            SelectEntry(iNextEntry, false, InformReloaded);
            break;
         end
         else
            dec(iNextEntry);
      until iNextEntry <= 0;

      if (iNextEntry <> FiEntry) and (iNextEntry >= 0) then
         SelectEntry(iNextEntry, false, InformReloaded);
   end;

   ReconsiderInfoPanelVisibility;
end;

procedure TKntNoteEntriesUI.btnPrevEntryClick(Sender: TObject);
begin
   SelectPrevEntry(True);
end;

procedure TKntNoteEntriesUI.SelectNextEntry(InformReloaded: boolean);
var
   iNextEntry: integer;
begin
   if FiEntry < Length(FEntriesShown) -1 then begin
      iNextEntry:= FiEntry;
      repeat
         inc(iNextEntry);
         if FEntriesShown[iNextEntry].Content <> cmHidden then begin
            SelectEntry(iNextEntry, false, InformReloaded);
            break;
         end;
      until iNextEntry >= Length(FEntriesShown) - 1;

      if (iNextEntry <> FiEntry) and (iNextEntry <= Length(FEntriesShown) - 1) then
         SelectEntry(iNextEntry, false, InformReloaded);
   end;

   ReconsiderInfoPanelVisibility;
end;

procedure TKntNoteEntriesUI.btnNextEntryClick(Sender: TObject);
begin
   SelectNextEntry(True);
end;


procedure TKntNoteEntriesUI.SelectEntry(iEntry: integer; LastPos: boolean = false; InformReloaded: boolean = True);
var
  SS: integer;
begin
   if (PanelConfig.CurrentMode = meMultipleEntries) then begin
       if LastPos and (FEntriesShown[iEntry].Content <> cmOnlyHeader) then
          Editor.SelStart:= FEntriesShown[iEntry].FinalPos
       else begin
          SS:= FEntriesShown[iEntry].StartingPos;
          if (PanelConfig.MEShowLineInHeader) and (FEntriesShown[iEntry].Content = cmOnlyHeader) then
             inc(SS, 6);
          Editor.SelStart:= SS;
          if FEntriesShown[iEntry].Content <> cmOnlyHeader then
             Editor.SelStart:= FEntriesShown[iEntry].StartingContentPos;
       end;
   end
   else begin
       Editor.OnEditorChanged := nil;
       SaveToDataModel();
       btnToggleMulti.Caption:= (iEntry+1).ToString;
       Editor.HideNestedFloatingEditor;
       PanelConfig.SelNEntry:= FEntriesShown[iEntry].NEntry;
       PanelConfig.SelStart:= 0;
       PanelConfig.SelLength:= 0;
       ReloadFromDataModel(false, nil, aNull, InformReloaded);
   end;
end;



procedure TKntNoteEntriesUI.btnToggleMultiClick(Sender: TObject);
begin
   if Sender <> nil then
      TKntNoteUI(NoteUI).ReturnToQLFromAllEntriesInSingleMode:= false;

   SavePositionInPanel;

   if (PanelConfig.CurrentMode = meMultipleEntries) then begin
      PanelConfig.CurrentMode:= meSingleEntry;
   end
   else begin
      SaveToDataModel();
      PanelConfig.CurrentMode:= meMultipleEntries;
   end;

   Editor.NavigatePanelsEnabled:= True;
   Editor.HideNestedFloatingEditor;

   NoteUI.HideFocusFlag:= false;
   cFocusedFlag.Refresh;
   ReloadFromDataModel(false, nil, aNull, True);
   ReconsiderInfoPanelVisibility;
end;


procedure TKntNoteEntriesUI.btnOptionsClick(Sender: TObject);
begin
  //
end;



procedure TKntNoteEntriesUI.ConfigureEditor (iEntry: integer = -1);
var
   plainTxt: boolean;
   NEntry: TNoteEntry;
   i: integer;
begin
  if FNNode = nil then begin
     FEditor.SupportsRegisteredImages:= false;
     FEditor.SupportsImages:= false;
     FEditor.SetVinculatedObjs(nil, nil, nil, nil, nil, false);
     FEditor.Enabled:= False;
     txtTags.Enabled:= False;
     txtName.Enabled:= False;
     txtCreationDate.Enabled:= False;
     btnOptions.Enabled:= false;
  end
  else begin
     if iEntry >= 0 then
        NEntry:= FEntriesShown[iEntry].NEntry
     else
        NEntry:= FNEntry;


     FEditor.SetVinculatedObjs(FKntFolder.KntFile, FKntFolder, FNNode, NEntry, Self, (PanelConfig.CurrentMode = meMultipleEntries));
     FEditor.Chrome:= FKntFolder.EditorChrome;

     if (iEntry >=0) or (PanelConfig.CurrentMode = meSingleEntry) then begin
        plainTxt:= (NEntry <> nil) and NEntry.IsPlainTXT;

     end
     else begin
        plainTxt:= true;
        for i:= 0 to Length(FEntriesShown)-1 do begin
           if FEntriesShown[i].Content = cmOnlyHeader then continue;
           NEntry:= FEntriesShown[i].NEntry;
           if not NEntry.IsPlainTXT then begin
              plainTxt:= false;
              break;
           end;
        end;

        if (PanelConfig.CurrentMode = meMultipleEntries) then
           FEditor.StreamFormat:= sfRichText;
     end;

     FEditor.PlainText:= plainTxt;
     FEditor.SupportsRegisteredImages:= (ImageMng.StorageMode <> smEmbRTF) and not plainTxt and not FNNode.IsVirtual;
     FEditor.SupportsImages:= not plainTxt;
  end;

end;

procedure TKntNoteEntriesUI.EditorChangedSelectionInMultiEntries;
var
   SS, i: integer;
begin
   if fChangingInCode then exit;
   if (FiEntry < 0) or (FEntriesShown = nil) then exit;

   SS:= Editor.SelStart;
   if (SS < FEntriesShown[FiEntry].StartingPos) or (SS > FEntriesShown[FiEntry].FinalPos) then begin
      for i:=0 to High(FEntriesShown) do
          if (SS >= FEntriesShown[i].StartingPos) and (SS <= FEntriesShown[i].FinalPos) then begin
             FiEntry:= i;
             btnToggleMulti.Caption:= (i+1).ToString;
             FNNode:= FEntriesShown[i].NNode;
             FNote:= FEntriesShown[i].Note;
             FNEntry:= FEntriesShown[i].NEntry;
             ReloadMetadataFromDataModel();
             ReconsiderInfoPanelVisibility;
             FEditor.SetVinculatedEntryObj(FNEntry);
             App.NEntrySelected(Editor, FNEntry);
             break;
          end;
   end;

   if (SS >= FEntriesShown[FiEntry].StartingPos) and (SS < FEntriesShown[FiEntry].StartingContentPos) then begin
       if (FiEntry > 0) and (VKeyDown(VK_LEFT) or VKeyDown(VK_UP)) then
          SelectEntry(FiEntry-1, True)
       else
       if (FiEntry <= Length(FEntriesShown) -1) and (VKeyDown(VK_RIGHT) or VKeyDown(VK_DOWN)) then begin
           if (SS = (FEntriesShown[FiEntry].StartingPos + 6)) and (FiEntry < (Length(FEntriesShown) -1)) then
              SelectEntry(FiEntry + 1)
           else
              SelectEntry(FiEntry);
       end;
   end;

end;



procedure TKntNoteEntriesUI.EditorDblClickInMultiEntries(Ctrl, Alt: boolean; LimitToCreatedBeforeSelectedEntry: boolean = false);
var
   SS, i: integer;
   NewCont: TContentInMultiEntriesMode;
begin
   {
                 DblClick -> Toggle between cmOnlyHeader and cmWholeEntry, on selected entry
          Ctrl + DblClick ->                   ,,                        , on all not hidden entries
           Alt + DblClick -> Hide selected entry (-> cmHidden)
    Ctrl + Alt + DblClick -> Toggle between cmOnlyHeader and cmWholeEntry, on all entries (included hidden)
   }

   SS:= Editor.SelStart;

   if (SS >= FEntriesShown[FiEntry].StartingPos) and (SS < FEntriesShown[FiEntry].StartingContentPos) then begin

      case FEntriesShown[FiEntry].Content of
        cmOnlyHeader,
        cmOnlyFirstLines: NewCont:= cmWholeEntry;
        cmWholeEntry:     NewCont:= cmOnlyHeader;
        cmHidden:         NewCont:= cmOnlyFirstLines;
      end;

      if Alt and not Ctrl then begin
         if FNEntry.IsMain then
            exit;
         NewCont:= cmHidden;
      end;

      Sleep(100);
      Application.ProcessMessages;

      PanelConfig.SelNEntry:= FNEntry;
      ReloadVisibleContentOfEntries (Ctrl, NewCont, FiEntry, not Alt,  false, LimitToCreatedBeforeSelectedEntry);

      if FiEntry >= 0 then           // It could be the last visible, and Alt+DblClick was pressed on it
         SelectEntry(FiEntry);
   end;
end;


procedure TKntNoteEntriesUI.ToggleOnlyHeaders_WholeContent;
begin
   if (FiEntry < 0) or (PanelConfig.CurrentMode <> meMultipleEntries) then exit;

   Editor.SelStart:= FEntriesShown[FiEntry].StartingPos;
   EditorDblClickInMultiEntries(True, False, CtrlDown);
end;

procedure TKntNoteEntriesUI.ReloadVisibleContentOfEntries (ModifyAll: boolean; NewContent: TContentInMultiEntriesMode; iEntry: integer= -1;
                                                           IgnoreHiddenEntries: boolean = true; OnlyHiddenEntries: boolean = false;
                                                           LimitToCreatedBeforeSelectedEntry: boolean = false);
var
   i: integer;
   NEntryToConsider: TNoteEntry;
   NEntry: TNoteEntry;
   CreatedDate: TDateTime;
begin
   if not ModifyAll and ((iEntry < 0) or (FEntriesShown[iEntry].Content = NewContent)) then exit;

   if iEntry >= 0 then
      FEntriesShown[iEntry].Content:= NewContent;

   if ModifyAll then begin
      CreatedDate:= 0;
      if LimitToCreatedBeforeSelectedEntry then
         CreatedDate:= FNEntry.Created;

      for i:=0 to High(FEntriesShown) do begin
         if IgnoreHiddenEntries and (FEntriesShown[i].Content = cmHidden) then continue;
         NEntry:= FEntriesShown[i].NEntry;
         if OnlyHiddenEntries and not ((NEntry.IsHidden) or (FEntriesShown[i].Content = cmHidden)) then continue;
         if NEntry.IsEncrypted and ActiveFile.EncryptedContentMustBeHidden and ActiveFile.HideEncryptedNodesAndEntries then continue;
         if (NewContent = cmHidden) and (NEntry.IsMain) then continue;
         if LimitToCreatedBeforeSelectedEntry and (NEntry.Created > CreatedDate) then continue;

         FEntriesShown[i].Content:= NewContent;
      end;
   end;

   SaveToDataModel();
   Editor.HideNestedFloatingEditor;

   NEntryToConsider:= nil;
   if not ModifyAll then
      NEntryToConsider:= FEntriesShown[iEntry].NEntry;
   ReloadFromDataModel(false, NEntryToConsider, aChangedVisibility);

   App.EditorReloaded(Editor, Editor.Focused);
end;


procedure TKntNoteEntriesUI.ShowHiddenEntries(UndoHidden: boolean);
var
   i: integer;
   Shift: boolean;
begin
   Shift:= ShiftDown;

   if not (CtrlDown or Shift) then
      ReloadVisibleContentOfEntries(True, cmOnlyFirstLines, -1, false, true)

   else begin
      // Ctrl: Show and undo hidden
      // Shift: Only not hidden (Alt+DblClick)

      for i:=0 to High(FEntriesShown) do begin
         if Shift then begin
            if (FEntriesShown[i].Content = cmHidden) and (not FEntriesShown[i].NEntry.IsHidden) and
               not (FEntriesShown[i].NEntry.IsEncrypted and ActiveFile.EncryptedContentMustBeHidden) then
               FEntriesShown[i].Content:= cmOnlyFirstLines;
         end
         else  // Ctrl
         if FEntriesShown[i].NEntry.IsHidden then begin
            FEntriesShown[i].NEntry.IsHidden:= False;

            if (FEntriesShown[i].NEntry.IsEncrypted and ActiveFile.EncryptedContentMustBeHidden) then begin
               if ActiveFile.HideEncryptedNodesAndEntries then
                  FEntriesShown[i].Content:= cmHidden
               else
                  FEntriesShown[i].Content:= cmOnlyFirstLines;
            end
            else
            if FEntriesShown[i].Content = cmHidden then
               FEntriesShown[i].Content:= cmOnlyFirstLines;
         end;
      end;
      App.ReconsiderVisibilityOfEntries(FNote);
   end;

end;



procedure TKntNoteEntriesUI.RefreshHeaderOfEntries(OnlyNEntry: TNoteEntry = nil);
begin
   SavePositionInPanel;
   ReloadFromDataModel(false, OnlyNEntry, aRefreshHeader);
end;


procedure TKntNoteEntriesUI.ModifiedMetadataOfEntry(NEntry: TNoteEntry);
var
   N: integer;
begin
   SavePositionInPanel;
   N:= Length(FEntriesShown);

   ReloadFromDataModel(false, NEntry, aModifiedMetadata);

   if (N = 0) and (Length(FEntriesShown) > 0) and (PanelConfig.StLayout <> spInQL_ets) then
      SelectEntry(0, false, false);

end;


procedure TKntNoteEntriesUI.NEntryDeleted(NEntry: TNoteEntry);
begin
   SavePositionInPanel;
   ReloadFromDataModel(false, NEntry, aDeleted);
end;

procedure TKntNoteEntriesUI.NEntryHidden(NEntry: TNoteEntry; Hidden: boolean; CreatedBefore: TDateTime = 0);
var
   iEntry: integer;
   Cont, NewCont: TContentInMultiEntriesMode;
   AnyEntryChanged: boolean;

   procedure ChangeContent;
   begin
      Cont:= FEntriesShown[iEntry].Content;
      NewCont:= Cont;
      if Hidden then
         NewCont:= cmHidden
      else
      if Cont = cmHidden then
         NewCont:= cmOnlyHeader;

      if Cont <> NewCont then begin
         FEntriesShown[iEntry].Content:= NewCont;
         AnyEntryChanged:= True;
      end;
   end;

begin
   if NEntry <> nil then begin
      iEntry:= GetIndexOfIncludedEntry(NEntry);
      if iEntry < 0 then exit;
      ChangeContent;
   end
   else begin
      AnyEntryChanged:= False;

      for iEntry:=0 to High(FEntriesShown) do begin
         if (FEntriesShown[iEntry].NEntry.Created <= CreatedBefore) and not FEntriesShown[iEntry].NEntry.IsMain then
            ChangeContent;
      end;

      if not AnyEntryChanged then exit;
   end;

   SavePositionInPanel;
   ReloadFromDataModel(false, NEntry, aChangedVisibility);
end;


procedure TKntNoteEntriesUI.NEntryReadOnlyChanged(NEntry: TNoteEntry);
begin
   if PanelConfig.CurrentMode = meMultipleEntries then exit;
   if GetIndexOfIncludedEntry(NEntry) < 0 then exit;

   ForceTempReadOnly(FKntFolder.ReadOnly or NEntry.IsReadOnly);
end;



function TKntNoteEntriesUI.StreamFormatInNEntry(const NEntry: TNoteEntry): TRichStreamFormat;
begin
    if NEntry.IsRTF then
       Result:= sfRichText
    else
       Result:= sfPlainText;

end;



{$ENDREGION}


// Images  =========================================

{$REGION Images }

function TKntNoteEntriesUI.GetImagesInstances: TImageIDs;
begin
   if FEditor.SupportsRegisteredImages and FEditor.Modified then
      fImagesReferenceCount:= GetImagesIDInstances (nil, FEditor.TextPlain);

   Result:= fImagesReferenceCount;
end;


function TKntNoteEntriesUI.GetImagesIDInstances (Stream: TMemoryStream; TextPlain: String): TImageIDs;
begin
   if (TextPlain <> '') then
      Result:= ImageMng.GetImagesIDInstancesFromTextPlain (TextPlain)
   else
      Result:= ImageMng.GetImagesIDInstancesFromRTF (Stream);
end;


procedure TKntNoteEntriesUI.ResetImagesReferenceCount;
begin
    SetLength(fImagesReferenceCount, 0);
end;


procedure TKntNoteEntriesUI.ReloadImagesOnEditor;
var
   ImgeIDs: TImageIDs;
   SS: integer;
begin
   SS:= Editor.SelStart;

   ImgeIDs:= ImageMng.GetImagesIDInstancesFromTextPlain (Editor.TextPlain);
   ImageMng.ReloadImages(ImgeIDs);

   SaveToDataModel;
   ReloadFromDataModel;

   Editor.SelStart:= SS;

end;


procedure TKntNoteEntriesUI.ReconsiderImageDimensionGoalsOnEditor(Selection: boolean; ImagesMode: TImagesMode);
var
   SS: integer;
begin
   if ReadOnly then
      Selection:= False;      // If true -> The note would have to be modified, and since it is not possible, the images would disappear...

    if Selection then
       FEditor.ReconsiderImageDimensionGoals(Selection, ImagesMode)

    else begin
       ImageMng.ReconsiderImageDimensionsGoal:= true;
       FEditor.GetAndRememberCurrentZoom;
       try
          SS:= Editor.SelStart;
          SaveToDataModel;
          ReloadFromDataModel;
          FEditor.SelStart:= SS;

       finally
          FEditor.RestoreZoomCurrent;
          ImageMng.ReconsiderImageDimensionsGoal:= false;
       end;
    end;

end;


procedure TKntNoteEntriesUI.SetImagesMode(ImagesMode: TImagesMode);
var
   RTFIn, RTFOut: AnsiString;
   SS: integer;
   RestoreRO: boolean;

begin
    if (FNEntry = nil) or not FEditor.SupportsRegisteredImages then exit;

    SS:= FEditor.SelStart;

    if (PanelConfig.CurrentMode = meMultipleEntries) then begin
       if (FiEntry >= 0) and (FEntriesShown <> nil) then begin
          if (ImagesMode = imLink) then                                       // imImage --> imLink
             SS:= PositionInImLinkTextPlain (FEditor, FNEntry, SS, True, FEntriesShown[FiEntry].StartingContentPos, FEntriesShown[FiEntry].FinalPos)   // True: Force calculation
          else
             dec(SS, FEntriesShown[FiEntry].StartingContentPos);

          SavePositionInPanel;
          ReloadFromDataModel(false, nil, aNull, false);
          SearchCaretPos(Editor, SS, 0, true, Point(-1,-1), true,true,true, FNEntry, FEntriesShown[FiEntry].StartingContentPos, FEntriesShown[FiEntry].FinalPos);
       end;
    end
    else begin
        if (ImagesMode = imLink) then                                       // imImage --> imLink
           SS:= PositionInImLinkTextPlain (FEditor, FNEntry, SS, True);   // True: Force calculation

        RTFIn:= Editor.RtfText;
        RTFOut:= ImageMng.ProcessImagesInRTF(RTFIn, Self.Name, ImagesMode, '', 0, true);
        if RTFOut <> '' then begin
           Editor.BeginUpdate;
           FEditor.GetAndRememberCurrentZoom;
           try
              RestoreRO:= Editor.ReadOnly;
              try
                 Editor.ReadOnly:= False;                     // We must allow images to be shown or hidden even if the note is read only

                 IgnoringEditorChanges:= True;
                 Editor.PutRtfText(RTFout,True,False);
              finally
                 IgnoringEditorChanges:= False;
                 if RestoreRO then begin
                    Editor.ReadOnly:= True;
                    Editor.Modified:= False;
                 end;
              end;
              SearchCaretPos(Editor, SS, 0, true, Point(-1,-1), true,true,true, FNEntry);
           finally
             FEditor.RestoreZoomCurrent;
             Editor.EndUpdate;
           end;
        end;
    end;
end;

{$ENDREGION}


end.
