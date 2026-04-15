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
    Content: TContentInMultipleMode;
  end;

  TActionOnEntry = (aModified, aCreated, aDeleted, aChangedVisibility, aNull);

type
  TKntNoteEntriesUI = class(TFrame)
    pnlEntries: TPanel;
    pnlIdentif: TPanel;
    txtCreationDate: TEdit;
    txtName: TEdit;
    txtTags: TEdit;
    btnNextEntry: TToolbarButton97;
    btnPrevEntry: TToolbarButton97;
    btnOptions: TToolbarButton97;
    btnToggleMulti: TToolbarButton97;
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
    FEntriesShown: Array of TEntryShown;
    FiEntry: integer;
    FMode: TModeEntriesUI;        // To not modify (and not saved) the value in PanelConfig
    FPanelConfig: TPanelConfiguration;
    FTagsOfEntryModified: boolean;

    RTFAux: TAuxRichEdit;

    FInfoPanelHidden: boolean;
    fImagesReferenceCount: TImageIDs;
    FEditor_SupportsRegisteredImages: boolean;       // For use with meMultipleEntries

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
    property Mode: TModeEntriesUI read FMode write FMode;
    property PanelConfig: TPanelConfiguration read FPanelConfig write FPanelConfig;
    procedure LoadFromDataModel (APanelConfig: TPanelConfiguration; SavePreviousContent: boolean; InformReloaded: boolean = false);
    procedure ReloadFromDataModel (CalculateEntriesToShow: boolean = true;
                                   NEntryToConsider: TNoteEntry = nil;
                                   ActionOnEntry: TActionOnEntry = aNull;
                                   InformReloaded: boolean = false);
    procedure ReloadMetadataFromDataModel (ReloadTags: boolean = true);
    procedure ReloadVisibleContentOfEntries (ModifyAll: boolean; NewContent: TContentInMultipleMode; iEntry: integer= -1);
    procedure SaveToDataModel;
    procedure SavePositionInPanel;
    procedure ReloadNoteName;
    procedure EditorChangedSelectionInMultiEntries;
    procedure EditorDblClickInMultiEntries;
    function VinculatedToMultipleEntries: boolean;
    function GetIndexOfVisibleEntry(NEntry: TNoteEntry): integer;
    procedure ModifyContentForNextReload(NEntry: TNoteEntry; NewContent: TContentInMultipleMode);
    procedure ConfigureEditor(iEntry: integer = -1);
    //procedure UpdateEntriesHeaderWidth(EnsureRefreshOnEditor: boolean);
  protected
    function StreamFormatInNEntry(const NEntry: TNoteEntry): TRichStreamFormat;
    //function GetHeaderCellx: AnsiString;
    function GetEntryHeader (Note: TNote; NEntry: TNoteEntry; FirstEntry: boolean = False; Folded: boolean = False): AnsiString;

  protected
    procedure SetInfoPanelHidden(value: boolean);
    procedure OnEndEditTagsIntroduction(PressedReturn: boolean);
    procedure AdjustTxtTagsWidth (AllowEdition: boolean = False);
    procedure ShowEntriesButtons(Show: boolean);
    procedure SelectEntry(iEntry: integer; LastPos: boolean = false);
    procedure FrameResize(Sender: TObject);
  public
    procedure EditTags;
    procedure RefreshTags;
    function HideTemporarilyInfoPanel: boolean;
    property InfoPanelHidden: boolean read FInfoPanelHidden write SetInfoPanelHidden;
    procedure RefreshEntry;
    property TagsOfEntryModified: boolean read FTagsOfEntryModified;

  protected
    function GetReadOnly: boolean;
    procedure SetReadOnly( AReadOnly : boolean );
    procedure ForceTempReadOnly( AReadOnly : boolean );
  public
    property ReadOnly : boolean read GetReadOnly write SetReadOnly;
    property OnUse: boolean read FOnUse;
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
   if KeyOptions.EditorInfoPanelTop then begin
      txtName.Top:= 0;
      txtCreationDate.Top:= 0;
      txtTags.Top:= 0;
      pnlIdentif.Align:= alTop;
   end;

   btnToggleMulti.Font.Size:= 8;

   SetReadOnly(FKntFolder.ReadOnly);
   fChangingInCode:= false;
   //FLastEditorUIWidth:= '';
   FPanelConfig:= nil;
   FOnUse:= False;
   FPanelInitialized:= false;

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


procedure TKntNoteEntriesUI.SetAsUnused;
begin
  FOnUse:= False;
  Editor.Clear;
  FNNode:= nil;
  FNEntry:= nil;
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

  FloatingEditorCannotBeSaved:= False;
  Editor.HideNestedFloatingEditor;
  App.EditorFocused(Editor);
  TagMng.UpdateTxtTagsHint(txtTags);
  if Assigned(FOnEnterOnEditor) then
    FOnEnterOnEditor(Self);

  TKntNoteUI(FNoteUI).NEntriesUIEditorEnter(Self);

  if FloatingEditorCannotBeSaved then
     Editor.ActivateFloatingEditor;

  if not FInfoPanelHidden and not PanelConfig.ShowEditorInfoPanel then begin
     pnlIdentif.Visible := True;     // Temporarily..
     if (txtTags.Width <= MIN_TAGS_WIDTH) And (FNEntry <> nil) and (FNEntry.Tags <> nil) then
        FrameResize(nil);
  end;
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
   if FNote = nil then exit;
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
   pnlIdentif.Visible := not value;
end;

function TKntNoteEntriesUI.HideTemporarilyInfoPanel: boolean;
var
  KeepVisible: boolean;
begin
  KeepVisible:= (txtTags.Focused or txtName.Focused or txtCreationDate.Focused);
  if not KeepVisible then
     KeepVisible:= IsMouseOver(pnlIdentif) or IsMouseOver(txtTags) or IsMouseOver(txtCreationDate);

  if not FInfoPanelHidden and not PanelConfig.ShowEditorInfoPanel and not KeepVisible then
     pnlIdentif.Visible := False;

  Result:= not KeepVisible;
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
   end;
   txtName.Hint:= s;
end;

procedure TKntNoteEntriesUI.txtCreationDateMouseEnter(Sender: TObject);
var
  s, lm: string;
begin
  if (FNote <> nil) then begin
      if (FMode = meMultipleEntries) or (FNote.NumEntries = 1) then begin
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


procedure TKntNoteEntriesUI.RefreshTags;
var
   S: string;
   Color: TColor;
begin
   if FNEntry <> nil then begin
      S:= FNEntry.TagsNames;
      Color:= clWindowText;
   end
   else
   if PanelConfig.VinculatedTags <> nil then begin
      S:= TNoteTagArrayUtils.ToNames(PanelConfig.VinculatedTags);
      Color:= clMaroon;
   end
   else
      exit;

   txtTags.Text:= S;
   TagMng.UpdateTxtTagsHint(txtTags);
   if S = '' then begin
      txtTags.Text:= EMPTY_TAGS;
      txtTags.Font.Color:= clGray;
   end
   else
      txtTags.Font.Color:= Color;

   AdjustTxtTagsWidth;
end;

procedure TKntNoteEntriesUI.txtTagsEnter(Sender: TObject);
begin
   if txtTags.ReadOnly then exit;

   TagMng.StartTxtEditTagIntrod(txtTags, OnEndEditTagsIntroduction, FNote, FNEntry, Folder);
   AdjustTxtTagsWidth(True);
end;

procedure TKntNoteEntriesUI.OnEndEditTagsIntroduction(PressedReturn: boolean);
begin
  if PressedReturn then
     Editor.SetFocus;

   FTagsOfEntryModified:= True;
   txtTags.Color:= FColorTxts;
   AdjustTxtTagsWidth;

   InfoPanelHidden:= Folder.EditorInfoPanelHidden;
end;


procedure TKntNoteEntriesUI.ShowEntriesButtons(Show: boolean);
var
   W: integer;
begin
   if btnPrevEntry.Visible = Show then exit;

   btnPrevEntry.Visible:= Show;
   btnNextEntry.Visible:= Show;
   btnToggleMulti.Visible:= Show;

   W:= btnPrevEntry.Width*2 + btnToggleMulti.Width;
   if Show then
      W:= W * -1;

   btnOptions.Left:= btnOptions.Left + W;
   txtCreationDate.Left:= txtCreationDate.Left + W;
   txtName.Width:= txtName.Width + W;
end;

procedure TKntNoteEntriesUI.AdjustTxtTagsWidth (AllowEdition: boolean = False);
var
  MinNoteNameWidth, MaxAvailableWidth: integer;
  MaxAvailableForTags, TagsWidth: integer;

begin
  MinNoteNameWidth:= TagMng.GetTextWidth(Note.Name, txtName) + 10;
  TagsWidth:=   MIN_TAGS_WIDTH;
  if txtTags.Text <> EMPTY_TAGS then
     TagsWidth:= TagMng.GetTextWidth(txtTags.Text, txtTags) + 10;

  MaxAvailableWidth:= (btnOptions.Left-2) - txtCreationDate.Width -4;
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
  txtName.Width:= MaxAvailableWidth - TagsWidth;
  txtName.Left:= txtTags.Left + TagsWidth + 2;
end;


procedure TKntNoteEntriesUI.FrameResize(Sender: TObject);
begin
   if Note <> nil then begin
      ShowEntriesButtons(Length(FEntriesShown) > 1);
      AdjustTxtTagsWidth(txtTags.Focused);
      FPanelInitialized:= true;
   end;
end;


{$ENDREGION}


// Load and save Editor from Note node =========================================

{$REGION Load, save and configure Editor for a Note node }


function TKntNoteEntriesUI.GetIndexOfVisibleEntry(NEntry: TNoteEntry): integer;
var
   i: integer;
begin
   Result:= -1;
   if NEntry = nil then exit;

   for i:= Length(FEntriesShown)-1 downto 0 do
      if FEntriesShown[i].NEntry = NEntry then
         exit(i);
end;

procedure TKntNoteEntriesUI.ModifyContentForNextReload(NEntry: TNoteEntry; NewContent: TContentInMultipleMode);
var
   iNEntry: integer;
begin
    iNEntry:= GetIndexOfVisibleEntry(NEntry);
    if iNEntry >= 0 then
       FEntriesShown[iNEntry].Content:= NewContent;
end;

procedure TKntNoteEntriesUI.LoadFromDataModel(APanelConfig: TPanelConfiguration; SavePreviousContent: boolean; InformReloaded: boolean = false);
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
           FMode:= PanelConfig.Mode;
           if (PanelConfig.Scope = fsSelectedNode) and (PanelConfig.SelectedNNode <> nil) then begin         //***
              FOnUse:= True;
              if not FInfoPanelHidden then
                 pnlIdentif.Visible := True;

              txtName.Visible := PanelConfig.ShowEditorInfoPanel;
              case PanelConfig.SelectedNNode.WordWrap of
                wwAsFolder : Editor.WordWrap := FKntFolder.WordWrap;
                wwYes : Editor.WordWrap := true;
                wwno :  Editor.WordWrap := false;
              end;
           end
           else
             txtName.Visible:= True;
       end;

       ReloadFromDataModel(true, nil, aNull, InformReloaded);

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

      txtCreationDate.Visible:= (FNEntry <> nil);
      if FNEntry <> nil then begin
        if (FMode = meMultipleEntries) then
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
 iEntry, iEntryToConsider, iEntryAdded, iSelectedEntry: integer;
 ImagesAux: TImageIDs;
 CannotShow_Encrypted: boolean;
 SS, SL, Offset: integer;
 Tags: TNoteTagArray;
 FindTags: TFindTags;
 EntryToAdd, EntryToRemove, MustBeIncluded: boolean;


 function NEntryMustBeIncludedInPanel (NEntry: TNoteEntry): boolean;
 begin
    Result:= True;
    if (FindTags <> nil) and not NEntry.MatchesTags(FindTags) then
        exit(false);
 end;


 // -> FEntriesShown, FNNode, FNote, [FNEntry, CannotShow_Encrypted]
 procedure PopulateEntriesToShow;
 var
   N: integer;
   iEntry, j: integer;
   NEntry: TNoteEntry;
   Created: TDateTime;

   procedure CheckCandidateEntry;
   begin
      NEntry:= Note.Entries[iEntry];
      if (FindTags = nil) or NEntry.MatchesTags(FindTags) then begin
         FEntriesShown[N].NEntry:= NEntry;
         FEntriesShown[N].NNode:= FNNode;
         FEntriesShown[N].Note:= FNote;
         FEntriesShown[N].Content:= PanelConfig.MMContent;
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

            FEntriesShown[iEntryAdded].NEntry:= NEntryToConsider;
            FEntriesShown[iEntryAdded].NNode:= FNNode;
            FEntriesShown[iEntryAdded].Note:= FNote;
            FEntriesShown[iEntryAdded].Content:= cmOnlyHeader;
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


    for j:= 0 to Length(PanelConfig.EntriesOnlyHeader)-1 do
      for iEntry:= 0 to Length(FEntriesShown)-1 do
         if FEntriesShown[iEntry].NEntry = PanelConfig.EntriesOnlyHeader[j] then begin
            FEntriesShown[iEntry].Content:= cmOnlyHeader;
            break;
         end;

    if FPanelInitialized then
       ShowEntriesButtons(Length(FEntriesShown) > 1);
 end;


 // Updates strRTF or cEditor
 procedure PrepareEntryContent (iEntry: integer);
 var
   NEntry: TNoteEntry;
 begin
     if (FMode = meMultipleEntries) then begin
        cEditor.Clear;
        if FEditor.SupportsRegisteredImages then
           FEditor_SupportsRegisteredImages:= True;
     end;

     NEntry:= FEntriesShown[iEntry].NEntry;
     NEntry.Stream.Position := 0;
     strRTF:= '';

     if (FMode = meSingleEntry) or (FEntriesShown[iEntry].Content <> cmOnlyHeader) then begin
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
                 if (FMode = meSingleEntry) then
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
            if (FMode = meSingleEntry) then begin
               cEditor.PutRtfText(strRTF,True,False);               // => ImageManager.StorageMode <> smEmbRTF
               cEditor.ClearUndo;
            end;
         end
         else
         if NEntry.Stream.Size > 0 then
            cEditor.Lines.LoadFromStream( NEntry.Stream );
     end;

 end;

 procedure ShowEntry(iEntry: integer);
 var
   TL, SS: integer;
 begin
     ConfigureEditor (iEntry);
     PrepareEntryContent (iEntry);         // -> strRTF or cEditor

     if (FMode = meMultipleEntries) then begin
        FEntriesShown[iEntry].StartingPos:= Editor.SelStart;
        Editor.PutRtfText(GetEntryHeader(FEntriesShown[iEntry].Note, FEntriesShown[iEntry].NEntry, (iEntry=0), (FEntriesShown[iEntry].Content=cmOnlyHeader)), True,True);
        FEntriesShown[iEntry].StartingContentPos:= Editor.SelStart;
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
           L:= FEntriesShown[i].FinalPos - FEntriesShown[i].StartingPos;
           Editor.SetSelection(FEntriesShown[i].StartingPos, FEntriesShown[i].FinalPos+1, false);
           if EntryToRemove then begin
              Offset:= - L;
              Editor.SelText:= '';
           end
           else begin
              ShowEntry (i);
              Offset:= (FEntriesShown[i].FinalPos - FEntriesShown[i].StartingPos) - L;
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
    inc(PanelConfig.ScrollPosInEditor.Y, 35);                    // TODO ***
    Editor.SetScrollPosInEditor(PanelConfig.ScrollPosInEditor);
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

   if CalculateEntriesToShow or (NEntryToConsider <> nil) then begin
      Tags:= PanelConfig.VinculatedTags;
      if (Tags <> nil) then
         FindTags:= FindTagsGetModeAND(Tags);
   end;


   CannotShow_Encrypted:= False;
   if CalculateEntriesToShow then begin
      FNNode:= nil;
      FNote:= nil;
      FNEntry:= nil;
      FTagsOfEntryModified:= false;
      NEntryToConsider:= nil;

      PopulateEntriesToShow;
      if Length(FEntriesShown) <= 1 then
         FMode := meSingleEntry;

      if not EditorOptions.SaveCaretPos then
         PanelConfig.SelNEntry:= nil;
   end;


   // NEntryToConsider: If it's included among the considered entries, check if it should remain so and, if so,redisplay it, using its current content and tags.
   //   If FMode = meSingleEntry, this NEntryToReconsider will be reflected in FEntriesShown, but it doesn't necessarily have to be reflected in the editor if
   //    the entry displayed there is different.
   //   If aCreated  -> Check if it should be included in the panel
   //   If aModified -> Check if it is included and if it should be included o removed
   //   If aDeleted -> Remove if it is present
   //
   // PanelConfig.SelNEntry: Indicates which entry should be displayed, if FMode = meSingleEntry, or, in the case of FMode = meMultipleEntries, which entry
   //   should be selected, the one containing the cursor. In both cases, it will determine the number of the entry displayed on the button associated with btnToggleMulti.

   iSelectedEntry:= GetIndexOfVisibleEntry(PanelConfig.SelNEntry);

   if (NEntryToConsider <> nil) then begin
       if (ActionOnEntry <> aDeleted) then
          MustBeIncluded:= NEntryMustBeIncludedInPanel(NEntryToConsider);

       if (ActionOnEntry = aCreated) then begin
          if not MustBeIncluded then exit;
          EntryToAdd:= true;
          PopulateEntriesToShow;
          if (FMode = meSingleEntry) then begin
             if (FiEntry = -1) and (FNEntry = NEntryToConsider) then begin
                btnToggleMulti.Caption:= (iEntryAdded+1).ToString;
                FiEntry:= iEntryAdded;
                FNNode:= FEntriesShown[iEntryAdded].NNode;
                FNote:= FEntriesShown[iEntryAdded].Note;
                pnlIdentif.Visible:= true;
                FNoteUI.KeepInfoPanelTemporarilyVisible;
             end;
          end;
       end
       else begin
          iEntryToConsider:= GetIndexOfVisibleEntry(NEntryToConsider);
          if (ActionOnEntry = aDeleted) then begin
             if (iEntryToConsider < 0) then exit;
             EntryToRemove:= true;
          end
          else
          if (ActionOnEntry = aModified) then begin
             if MustBeIncluded and (iEntryToConsider < 0) then begin
                EntryToAdd:= true;
                PopulateEntriesToShow;
                if (FMode = meSingleEntry) then exit;
             end
             else
             if not MustBeIncluded and (iEntryToConsider >= 0) then
                EntryToRemove:= true;
          end;
       end;
   end;

   if not CalculateEntriesToShow and (FiEntry < 0) and (FMode = meSingleEntry) then exit;


   if EntryToRemove and (FMode = meSingleEntry) then begin    // ToDO ****
      exit;
   end;

   if EntryToAdd then begin
      if (Length(FEntriesShown) = 2) and (PanelConfig.Mode = meMultipleEntries) and (PanelConfig.VinculatedTags = nil) then begin
         FMode:= meMultipleEntries;
         EntryToAdd:= false;          // Process the two entries, not just the one to add
         NEntryToConsider:= nil;
      end;
      if (FMode = meSingleEntry) then
         exit;
   end;


   Editor.BeginUpdate;                   // -> It will also ignore Enter and Change events

   if (FMode = meMultipleEntries) then begin
      SetReadOnly(true);
      if RTFAux = nil then
         RTFAux:= CreateAuxRichEdit();
      cEditor:= RTFAux;
      cEditor.BeginUpdate;
      FEditor_SupportsRegisteredImages:= False;
   end
   else begin
      SetReadOnly(FKntFolder.ReadOnly);
      cEditor:= FEditor;
   end;


   ReadOnlyBAK:= FReadOnly;
   ContainsImgIDsRemoved:= false;
   try
     fChangingInCode:= True;
     Editor.ReadOnly:= false;   // To prevent the problem indicated in issue #537

     if (FMode = meSingleEntry) or (NEntryToConsider = nil) then begin
        Editor.Clear;
        Editor.ClearUndo;

        fImagesReferenceCount:= nil;
     end;


     if EntryToAdd then begin             // and FMode = meMultipleEntries
        ShowNewEntryToAdd;
        exit;
     end;

     if EntryToRemove then begin          // and FMode = meMultipleEntries
        ReconsiderEntry(iEntryToConsider);
        PopulateEntriesToShow;
        if iEntryToConsider < FiEntry then
           dec(FiEntry);
        exit;
     end;



     FiEntry:= -1;

     if FEntriesShown <> nil then begin
       FiEntry:= iSelectedEntry;
       if (FiEntry < 0) and ((FMode = meMultipleEntries) or (PanelConfig.SelNEntry <> nil)) then begin
           FiEntry:= 0;
           if not Folder.NoteAdvOptions.ShowNewestEntryInSingleEntry then begin
              if not PanelConfig.DescendingOrder then
                  FiEntry:= Length(FEntriesShown)-1;
           end
           else begin
              if PanelConfig.DescendingOrder then
                  FiEntry:= Length(FEntriesShown)-1;
           end;
       end;


       if FMode = meMultipleEntries then begin    // --- meMultipleEntries
          if NEntryToConsider <> nil then
             ReconsiderEntry(iEntryToConsider)

          else begin
              for iEntry:= 0 to Length(FEntriesShown)-1 do
                 ShowEntry (iEntry);

              inc(FEntriesShown[High(FEntriesShown)].FinalPos);      // Last shown entry in the editor
          end;
       end
       else begin                              // --- meSingleEntry
          if NEntryToConsider <> nil then
             ReconsiderEntry(iEntryToConsider);
          if FiEntry >= 0 then
             ShowEntry (FiEntry)
          else begin
             ConfigureEditor (-1);
             PanelConfig.SelStart:= 0;
             PanelConfig.SelLength:= 0;
          end;
       end;

       if FiEntry >= 0 then begin
          FNNode:= FEntriesShown[FiEntry].NNode;
          FNote:= FEntriesShown[FiEntry].Note;
          FNEntry:= FEntriesShown[FiEntry].NEntry;
       end;
       Editor.Color:= GetColor(NNode.EditorBGColor, FKntFolder.EditorChrome.BGColor);

       if (FMode = meSingleEntry) then begin
          if (NEntry <> nil) and (NEntry.Stream.Size = 0) then     // Ensures that new nodes are correctly updated based on default properties (font color, size, ...)
              UpdateEditor (Editor, FKntFolder, false);
       end;


       SS:= PanelConfig.SelStart;
       SL:= PanelConfig.SelLength;
       if (FMode = meSingleEntry) then begin
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
           FNEntry:= FNote.Entries[0];
           FEditor.AddText(GetRS(sEdt52));
           ReadOnlyBAK:= True;
        end;
        ConfigureEditor;
     end;

     ReloadMetadataFromDataModel;

     Log_StoreTick('TKntNoteEntriesUI.LoadFromDataModel - END', 4, -1);


   finally
     ForceTempReadOnly(ReadOnlyBAK);

     Editor.RestoreZoomGoal;

     if (PanelConfig.ScrollPosInEditor.Y > 0) then
        Editor.SetScrollPosInEditor(PanelConfig.ScrollPosInEditor);

     if (FMode = meMultipleEntries) then begin
        Editor.StreamFormat:= sfRichText;
        Editor.PlainText:= False;
        Editor.SupportsRegisteredImages:= FEditor_SupportsRegisteredImages;
     end;

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
   end;


  Editor.Enabled:= true;
  txtName.Enabled:= True;
end;


function TKntNoteEntriesUI.VinculatedToMultipleEntries: boolean;
begin
   Result:= Length(FEntriesShown) > 1;
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
  s, strInfo: string;

begin
   // # ToDO —  08/11/2025 - 11:36  —

   if Folded then
      strInfo:= ' \u10133+ '          // ➕
   else
      strInfo:= ' — ';

   if PanelConfig.MMShowTagsInHeader and (Length(NEntry.Tags) > 0) then begin
      strInfo:= strInfo + '# ' + Trim(NEntry.TagsNames) + '  ';
   end;
   if PanelConfig.MMShowDateInHeader and (NEntry.Created <> 0) then begin
      if (NEntry.Created).GetTime <> 0 then
         S:= ' - ' + FormatSettings.ShortTimeFormat;
      strInfo:= strInfo + FormatDateTime(FormatSettings.ShortDateFormat + S, NEntry.Created);
   end;
   strInfo := strInfo + ' —';

   if PanelConfig.MMShowLineInHeader then
      strLine:= '\fs1\par\trowd\trgaph0\cellx999999 \intbl\fs1\cell\row\pard';

   Result:= '{\rtf1\ansi{\colortbl ;' + GetRTFColor(clWebDarkBlue) + ';}' + StrLine + '\qr\cf1\b\fs18 ' + strInfo + '\sa80\par}';
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
  if (FMode = meMultipleEntries) then exit;
  Encoding:= nil;

  if assigned(NNode) and (FNEntry <> nil) then begin
     if (FEditor.FloatingEditor <> nil) then
        FEditor.DoSaveChangesInFloatingEditor;

     if FEditor.Modified then begin
        FTagsOfEntryModified:= false;

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
       if FTagsOfEntryModified then begin
          FTagsOfEntryModified:= false;
          App.EditorSaved(FEditor);
       end;

       if (FNEntry <> nil) and (FNEntry.TextPlain = '') then
          InitializeTextPlain(FNEntry, RTFAux_Note);
     end;

  end;
end;


procedure TKntNoteEntriesUI.SavePositionInPanel;
begin
   PanelConfig.ScrollPosInEditor:= Editor.GetScrollPosInEditor;
   PanelConfig.SelNEntry := FNEntry;
   PanelConfig.SelStart  := Editor.SelStart;
   PanelConfig.SelLength := Editor.SelLength;

   if FMode = meMultipleEntries then begin
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


procedure TKntNoteEntriesUI.btnPrevEntryClick(Sender: TObject);
var
   iNextEntry: integer;
   SS: integer;
begin
   SS:= Editor.SelStart;
   if (FiEntry = -1) and (Length(FEntriesShown) > 0) then
       FiEntry := 1;
   if (FiEntry > 0) or ((FMode = meMultipleEntries) and (SS > FEntriesShown[FiEntry].StartingContentPos)) then begin
      iNextEntry:= FiEntry;
      if (FMode = meSingleEntry) or (SS <= FEntriesShown[FiEntry].StartingContentPos) then
         dec(iNextEntry);
      SelectEntry(iNextEntry);
   end;
   FNoteUI.KeepInfoPanelTemporarilyVisible;
end;


procedure TKntNoteEntriesUI.btnNextEntryClick(Sender: TObject);
begin
   if FiEntry < Length(FEntriesShown) -1 then
      SelectEntry(FiEntry+1);
   FNoteUI.KeepInfoPanelTemporarilyVisible;
end;


procedure TKntNoteEntriesUI.SelectEntry(iEntry: integer; LastPos: boolean = false);
var
  SS: integer;
begin
   if (FMode = meMultipleEntries) then begin
       if LastPos then
          Editor.SelStart:= FEntriesShown[iEntry].FinalPos
       else begin
          SS:= FEntriesShown[iEntry].StartingPos;
          if (PanelConfig.MMShowLineInHeader) and (FEntriesShown[iEntry].Content = cmOnlyHeader) then
             inc(SS, 6);
          Editor.SelStart:= SS;
          if FEntriesShown[iEntry].Content <> cmOnlyHeader then
             Editor.SelStart:= FEntriesShown[iEntry].StartingContentPos;
       end;
   end
   else begin
       SaveToDataModel();
       btnToggleMulti.Caption:= (iEntry+1).ToString;
       Editor.HideNestedFloatingEditor;
       PanelConfig.SelNEntry:= FEntriesShown[iEntry].NEntry;
       PanelConfig.SelStart:= 0;
       PanelConfig.SelLength:= 0;
       ReloadFromDataModel(false);
   end;
end;



procedure TKntNoteEntriesUI.btnToggleMultiClick(Sender: TObject);
begin
   SavePositionInPanel;

   if (FMode = meMultipleEntries) then begin
      FMode:= meSingleEntry;
   end
   else begin
      SaveToDataModel();
      FMode:= meMultipleEntries;
   end;

   Editor.HideNestedFloatingEditor;
   ReloadFromDataModel(false);
end;


procedure TKntNoteEntriesUI.btnOptionsClick(Sender: TObject);
begin
  //
end;



procedure TKntNoteEntriesUI.ConfigureEditor (iEntry: integer = -1);
var
   plainTxt: boolean;
   NEntry: TNoteEntry;
begin
  if FNNode = nil then begin
     FEditor.SupportsRegisteredImages:= false;
     FEditor.SupportsImages:= false;
     FEditor.SetVinculatedObjs(nil, nil, nil, nil, nil, (FMode=meMultipleEntries));
     FEditor.Enabled:= False;
     txtName.Enabled:= False;
  end
  else begin
     if iEntry >= 0 then
        NEntry:= FEntriesShown[iEntry].NEntry
     else
        NEntry:= FNEntry;

     plainTxt:= false;
     if NEntry <> nil then
        plainTxt:= NEntry.IsPlainTXT;
     FEditor.SetVinculatedObjs(FKntFolder.KntFile, FKntFolder, FNNode, NEntry, Self, (FMode=meMultipleEntries));
     FEditor.PlainText:= plainTxt;
     FEditor.Chrome:= FKntFolder.EditorChrome;

     FEditor.SupportsRegisteredImages:= (ImageMng.StorageMode <> smEmbRTF) and not plainTxt and not FNNode.IsVirtual;
     FEditor.SupportsImages:= not plainTxt;
  end;

end;

procedure TKntNoteEntriesUI.EditorChangedSelectionInMultiEntries;
var
   SS, i: integer;
begin
   if fChangingInCode then exit;

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



procedure TKntNoteEntriesUI.EditorDblClickInMultiEntries;
var
   SS, i: integer;
   NewCont: TContentInMultipleMode;
begin
   SS:= Editor.SelStart;

   if (SS >= FEntriesShown[FiEntry].StartingPos) and (SS < FEntriesShown[FiEntry].StartingContentPos) then begin

      if FEntriesShown[FiEntry].Content <> cmOnlyHeader then
         NewCont:= cmOnlyHeader
      else
         NewCont:= cmWholeEntry;

      Sleep(100);
      Application.ProcessMessages;

      PanelConfig.SelNEntry:= NEntry;
      ReloadVisibleContentOfEntries (CtrlDown, NewCont, FiEntry);

      SelectEntry(FiEntry);
   end;
end;


procedure TKntNoteEntriesUI.ReloadVisibleContentOfEntries (ModifyAll: boolean; NewContent: TContentInMultipleMode; iEntry: integer= -1);
var
   SS, i: integer;
   NEntryToConsider: TNoteEntry;
begin
   if not ModifyAll and (FEntriesShown[iEntry].Content = NewContent) then exit;

   FEntriesShown[iEntry].Content:= NewContent;
   if ModifyAll then
      for i:=0 to High(FEntriesShown) do
          FEntriesShown[i].Content:= NewContent;

   SaveToDataModel();
   Editor.HideNestedFloatingEditor;

   NEntryToConsider:= nil;
   if not ModifyAll then
      NEntryToConsider:= FEntriesShown[iEntry].NEntry;
   ReloadFromDataModel(false, NEntryToConsider, aChangedVisibility);
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
   if FEditor_SupportsRegisteredImages and FEditor.Modified then
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
begin
   ImgeIDs:= ImageMng.GetImagesIDInstancesFromTextPlain (Editor.TextPlain);
   ImageMng.ReloadImages(ImgeIDs);

   SaveToDataModel;
   ReloadFromDataModel;
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
    SS:= FEditor.SelStart;
    if ImagesMode = imLink then                                       // imImage --> imLink
       SS:= PositionInImLinkTextPlain (FKntFolder, NNode, SS, True);   // True: Force calculation

    RTFIn:= Editor.RtfText;
    RTFOut:= ImageMng.ProcessImagesInRTF(RTFIn, Self.Name, ImagesMode, '', 0, true);
    if RTFOut <> '' then begin
       Editor.BeginUpdate;
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
          SearchCaretPos(Self.Editor, SS, 0, true, Point(-1,-1), true,true,true);
       finally
         Editor.EndUpdate;
       end;
    end;
end;

{$ENDREGION}


end.
