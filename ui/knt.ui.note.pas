unit knt.ui.note;

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

  gf_misc,
  kn_info,
  kn_Const,
  kn_Global,
  kn_KntFolder,
  knt.model.note,
  knt.ui.info,
  knt.ui.editor,
  knt.ui.noteEntries,
  knt.App
  ;


type
  TKntNoteUI = class(TFrame, INoteUI)
    pnlAuxC: TPanel;
    pnlLeft: TPanel;
    splL: TSplitter;
    pnlAuxC2: TPanel;
    pnlTop: TPanel;
    splT: TSplitter;
    pnlAuxC3: TPanel;
    pnlCenter: TPanel;
    splB: TSplitter;
    pnlBottom: TPanel;
    pnlTL: TPanel;
    splTC: TSplitter;
    pnlTR: TPanel;
    pnlBL: TPanel;
    splBC: TSplitter;
    pnlBR: TPanel;
    procedure splTCMoved(Sender: TObject);
    procedure splBCMoved(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure splBMoved(Sender: TObject);
    procedure splTMoved(Sender: TObject);
    procedure splTCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure splLMoved(Sender: TObject);

  private class var
    FColorTxts: TColor;

  private
    FNote: TNote;
    FNNode: TNoteNode;
    FKntFolder: TKntFolder;
    FNNodeDeleted: boolean;

    FNEntryUI: array[TNEntriesPanel] of TKntNoteEntriesUI;
    FNNodeUIConfig: TNNodeUIConfiguration;
    FNewNNodeUIConfig: boolean;

    fSplitterNoteMoving: boolean;
    FUpdatingOnResize: boolean;
    IncResize: integer;

    Timer: TTimer;

{  // DEBUG
    FEditorL  : TKntRichEdit;
    FEditorTL : TKntRichEdit;
    FEditorTR : TKntRichEdit;
    FEditorBL : TKntRichEdit;
    FEditorBR : TKntRichEdit;
}

    function GetEditor: TKntRichEdit;
    function GetNNode: TNoteNode;
    function GetFolder: TObject;

  public
    constructor Create(AOwner: TComponent; KntFolder: TKntFolder);
    destructor Destroy; override;
    //procedure TestCreatePanel;

    property Editor : TKntRichEdit read GetEditor;

  public
    property Folder: TKntFolder read FKntFolder;
    property Note: TNote read FNote;
    property NNode: TNoteNode read GetNNode;
    procedure LoadFromNNode (NNode: TNoteNode; SavePreviousContent: boolean);
    procedure ReloadFromDataModel;
    function ReloadMetadataFromDataModel(ReloadTags: boolean = true): TNoteEntry;
    procedure SaveToDataModel;
    procedure ReloadNoteName;
    procedure ConfigureEditor;

  protected
    procedure SetInfoPanelHidden(value: boolean);

  protected
    function GetNEntryUI (Panel: TNEntriesPanel): TKntNoteEntriesUI; overload;
    function GetNEntryUI (Editor: TKntRichEdit): TKntNoteEntriesUI; overload;
    procedure CreateNewEntry(RequestedFromEditor: TKntRichEdit); overload;
    procedure CreateNewEntry(RequestedFromNEntryUI: TKntNoteEntriesUI); overload;
    procedure TimerTimer(Sender: TObject);
 public
    procedure NEntryUIEditorEnter(Sender: TObject);

 public
    procedure Refresh;
  public
    procedure ShowLeftPanel(value: boolean);
    procedure ShowTopPanels(value: boolean);
    procedure ShowBottomPanels(value: boolean);
    procedure ShowPanelsTop(TL, TR: boolean);
    procedure ShowPanelsBottom(BL, BR: boolean);
    function GetPanel (Panel: TNEntriesPanel): TPanel;
    //procedure TestPanels;

  protected
    function GetReadOnly: boolean;
    procedure SetReadOnly( AReadOnly : boolean );

  protected
    function GetImagesInstances: TImageIDs;
    property ImagesInstances: TImageIDs read GetImagesInstances;
    procedure GetImagesIDInstances (Stream: TMemoryStream; TextPlain: String);
    procedure ResetImagesReferenceCount;
    procedure ReloadImagesOnEditor;
    procedure ReconsiderImageDimensionGoalsOnEditor (Selection: boolean; ImagesMode: TImagesMode);
    procedure SetImagesMode(ImagesMode: TImagesMode);

  public
    procedure SetOnEnter(AEvent: TNotifyEvent);
    procedure SetOnMouseUpOnNote(AEvent: TNotifyEvent);
    procedure SetOnMouseMoveOnNote(AEvent: TNotifyEvent);
    procedure SetFocusOnEditor;
    procedure NNodeDeleted;
    function GetNNodeDeleted: boolean;

  public
    procedure RefreshTags;
    procedure EditTags;
  end;



implementation

{$R *.dfm}

uses
  knt.RS;


// Create  / Destroy =========================================

{$REGION Create / Destroy}

constructor TKntNoteUI.Create(AOwner: TComponent; KntFolder: TKntFolder);
var
  p: TNEntriesPanel;
begin
   inherited Create(AOwner);

   FKntFolder:= KntFolder;

   for p := pnTL to High(TNEntriesPanel) do
      FNEntryUI[p]:= nil;

   FNEntryUI[pnCenter]:= TKntNoteEntriesUI.Create( PnlCenter, Self );
   FNEntryUI[pnCenter].Parent:= PnlCenter;

   //TestCreatePanel;

   fSplitterNoteMoving:= false;
   FUpdatingOnResize:= false;
   Timer:= TTimer.Create(Self);
   Timer.Enabled := false;
   Timer.Interval := 3000;  // 3 seconds
   Timer.OnTimer:= TimerTimer;
end;


destructor TKntNoteUI.Destroy;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntryUI[p] <> nil then
         FreeAndNil(FNEntryUI[p]);

   Timer.Free;

   inherited;
end;

{
procedure TKntNoteUI.TestCreatePanel;
begin
   FTLTR_Ratio:= 0.5;
   FBLBR_Ratio:= 0.5;
   FTopOther_Ratio:= pnlTop.Height / Self.Height;
   FBottomOther_Ratio:= pnlBottom.Height / Self.Height;

   FEditorL := TKntRichEdit.Create( pnlLeft );
   FEditorL.Parent := pnlLeft;
   FEditorL.Align := alClient;
   FEditorTL := TKntRichEdit.Create( pnlTL );
   FEditorTL.Parent := pnlTL;
   FEditorTL.Align := alClient;
   FEditorTR := TKntRichEdit.Create( pnlTR );
   FEditorTR.Parent := pnlTR;
   FEditorTR.Align := alClient;
   FEditorBL := TKntRichEdit.Create( pnlBL );
   FEditorBL.Parent := pnlBL;
   FEditorBL.Align := alClient;
   FEditorBR := TKntRichEdit.Create( pnlBR );
   FEditorBR.Parent := pnlBR;
   FEditorBR.Align := alClient;
end;
}

{$ENDREGION}


// Controls. Events

{$REGION Controls. Properties and Events }

function TKntNoteUI.GetEditor: TKntRichEdit;
begin
  Result:= FNEntryUI[pnCenter].Editor;
end;


function TKntNoteUI.GetReadOnly: boolean;
begin
   Result:= FNEntryUI[pnCenter].ReadOnly;
end;

procedure TKntNoteUI.SetReadOnly( AReadOnly : boolean );
begin
   FNEntryUI[pnCenter].ReadOnly:= AReadOnly;
end;


procedure TKntNoteUI.SetOnEnter(AEvent: TNotifyEvent);
begin
  FNEntryUI[pnCenter].SetOnEnter(AEvent);
end;

procedure TKntNoteUI.SetOnMouseUpOnNote(AEvent: TNotifyEvent);
begin
   FNEntryUI[pnCenter].SetOnMouseUpOnNote(AEvent);
end;

procedure TKntNoteUI.SetOnMouseMoveOnNote(AEvent: TNotifyEvent);
begin
   FNEntryUI[pnCenter].SetOnMouseMoveOnNote(AEvent);
end;


procedure TKntNoteUI.SetInfoPanelHidden(value: boolean);
begin
   FNEntryUI[pnCenter].InfoPanelHidden:= value;
end;


procedure TKntNoteUI.SetFocusOnEditor;
begin
   FNEntryUI[pnCenter].SetFocus;
end;


{$ENDREGION}

// Panels =========================================

{$REGION Panels }

procedure TKntNoteUI.ShowLeftPanel(value: boolean);
begin
   if pnlLeft.Visible = value then
      exit;

   pnlLeft.Visible:= value;
   splL.Visible:= value;
   pnlBL.Width:= Round(pnlBottom.Width * FNNodeUIConfig.BLBR_Ratio);
   pnlTL.Width:= Round(pnlTop.Width * FNNodeUIConfig.TLTR_Ratio);
   if value then
      splL.Left:= pnlLeft.Width;
end;

procedure TKntNoteUI.ShowTopPanels(value: boolean);
var
   IncResize: integer;
begin
   if pnlTop.Visible = value then
      exit;

   pnlTop.Visible:= value;
   splT.Visible:= value;
   IncResize:= pnlTop.Height;
   if value then begin
      pnlTop.Height:= Round(Self.Height * FNNodeUIConfig.Top_Ratio);
      splT.Top := pnlTop.Height + 3;
      IncResize:= - IncResize;
   end;

   if pnlBottom.visible then
      pnlCenter.Height:= pnlCenter.Height + IncResize;
end;


procedure TKntNoteUI.splBCMoved(Sender: TObject);
begin
   FNNodeUIConfig.BLBR_Ratio:= pnlBL.Width / pnlBottom.Width;
end;

procedure TKntNoteUI.splBMoved(Sender: TObject);
begin
   FNNodeUIConfig.Bottom_Ratio:= pnlBottom.Height / Self.Height;
end;

procedure TKntNoteUI.splLMoved(Sender: TObject);
begin
   pnlBL.Width:= Round(pnlBottom.Width * FNNodeUIConfig.BLBR_Ratio);
   pnlTL.Width:= Round(pnlTop.Width * FNNodeUIConfig.TLTR_Ratio);
end;

procedure TKntNoteUI.splTCMoved(Sender: TObject);
begin
   FNNodeUIConfig.TLTR_Ratio:= pnlTL.Width / pnlTop.Width;
end;

procedure TKntNoteUI.splTCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
begin
  IncResize:= (NewSize - pnlTop.Height);
  if not FUpdatingOnResize and pnlBottom.Visible then begin
    FNEntryUI[pnCenter].Editor.BeginUpdate;
    if FNEntryUI[pnBL] <> nil then
       FNEntryUI[pnBL].Editor.BeginUpdate;
    if FNEntryUI[pnBR] <> nil then
       FNEntryUI[pnBR].Editor.BeginUpdate;
    FUpdatingOnResize:= True;
  end;
end;

procedure TKntNoteUI.splTMoved(Sender: TObject);
begin
  // This method will be called 2 times, from TSplitter.UpdateControlSize and TSplitter.StopSizing
  // We Will ignore the first one
  if not fSplitterNoteMoving then begin
     fSplitterNoteMoving:= true;
     exit;
  end;
  fSplitterNoteMoving:= false;

  FNNodeUIConfig.Top_Ratio:= pnlTop.Height / Self.Height;

  if pnlBottom.Visible then begin
     pnlCenter.Height:= pnlCenter.Height - IncResize;
     FNEntryUI[pnCenter].Editor.EndUpdate;
    if FNEntryUI[pnBL] <> nil then
       FNEntryUI[pnBL].Editor.EndUpdate;
    if FNEntryUI[pnBR] <> nil then
       FNEntryUI[pnBR].Editor.EndUpdate;
  end;

  FUpdatingOnResize:= False;
end;



procedure TKntNoteUI.ShowBottomPanels(value: boolean);
begin
   if pnlBottom.Visible = value then
      exit;

   pnlBottom.Visible:= value;
   if value then begin
      pnlCenter.Align:= alTop;
      pnlCenter.Height:= pnlAuxC3.Height - Round(Self.Height * FNNodeUIConfig.Bottom_Ratio) - 3;
      splB.Top := pnlBottom.Top - 3;
   end
   else
      pnlCenter.Align:= alClient;

   splB.Visible:= value;
end;

procedure TKntNoteUI.ShowPanelsTop(TL, TR: boolean);
begin
   if not (TL or TR) then
      ShowTopPanels(false)

   else begin
      ShowTopPanels(true);
      if (pnlTL.Visible = TL) and (pnlTR.Visible = TR) then
          exit;

      pnlTL.Visible:= TL;
      pnlTR.Visible:= TR;
      if TL and TR then begin
         pnlTL.Align:= alLeft;
         pnlTR.Align:= alClient;
         pnlTL.Width:= Round(pnlTop.Width * FNNodeUIConfig.TLTR_Ratio);
         splTC.Visible:= True;
         splTC.Left:= pnlTL.Width;
      end
      else begin
        splTC.Visible:= False;
        if not TL then
           pnlTR.Align:= alClient;
        if not TR then
           pnlTL.Align:= alClient;
      end;
   end;
end;

procedure TKntNoteUI.ShowPanelsBottom(BL, BR: boolean);
begin
   if not (BL or BR) then
      ShowBottomPanels(false)

   else begin
      ShowBottomPanels(true);
      if (pnlBL.Visible = BL) and (pnlBR.Visible = BR) then
          exit;

      pnlBL.Visible:= BL;
      pnlBR.Visible:= BR;
      if BL and BR then begin
         pnlBL.Align:= alLeft;
         pnlBR.Align:= alClient;
         pnlBL.Width:= Round(pnlBottom.Width * FNNodeUIConfig.BLBR_Ratio);
         splBC.Visible:= True;
         splBC.Left:= pnlBL.Width;
      end
      else begin
        splBC.Visible:= False;
        if not BL then
           pnlBR.Align:= alClient;
        if not BR then
           pnlBL.Align:= alClient;
      end;
   end;
end;


procedure TKntNoteUI.FrameResize(Sender: TObject);
begin
   if pnlBL.Visible and pnlBR.Visible then
      pnlBL.Width:= Round(pnlBottom.Width * FNNodeUIConfig.BLBR_Ratio);
   if pnlTL.Visible and pnlTR.Visible then
      pnlTL.Width:= Round(pnlTop.Width * FNNodeUIConfig.TLTR_Ratio);
   if pnlTop.Visible then
      pnlTop.Height:= Round(Self.Height * FNNodeUIConfig.Top_Ratio);
   if pnlBottom.Visible then
      pnlCenter.Height:= pnlAuxC3.Height - Round(Self.Height * FNNodeUIConfig.Bottom_Ratio) - 3;
end;


function TKntNoteUI.GetPanel (Panel: TNEntriesPanel): TPanel;
begin
   case Panel of
     pnCenter: Result:= pnlCenter;
     pnTL: Result:= pnlTL;
     pnTR: Result:= pnlTR;
     pnBL: Result:= pnlBL;
     pnBR: Result:= pnlBR;
   end;
end;

{
procedure TKntNoteUI.TestPanels;
begin
  FEditorL.Text := 'Left';
  FEditorBL.Text := 'BL';
  FEditorBR.Text := 'BR';
  FEditorTL.Text := 'TL';
  FEditorTR.Text := 'TR';

  ShowLeftPanel(False);
  Application.ProcessMessages;
  ShowLeftPanel(True);
  Application.ProcessMessages;

  ShowTopPanels(False);
  Application.ProcessMessages;
  ShowTopPanels(True);
  Application.ProcessMessages;

  ShowBottomPanels(False);
  Application.ProcessMessages;
  ShowBottomPanels(True);
  Application.ProcessMessages;


  ShowBottomPanels(False);
  Application.ProcessMessages;
  ShowPanelsBottom(True, False);
  Application.ProcessMessages;
  ShowPanelsBottom(False, True);
  Application.ProcessMessages;
  ShowPanelsBottom(True, True);
  Application.ProcessMessages;
  ShowPanelsBottom(False, False);
  Application.ProcessMessages;
  ShowBottomPanels(True);


  ShowTopPanels(False);
  Application.ProcessMessages;
  ShowPanelsTop(True, False);
  Application.ProcessMessages;
  ShowPanelsTop(True, True);
  Application.ProcessMessages;
  ShowPanelsTop(False, False);
  Application.ProcessMessages;
  ShowPanelsTop(False, True);
  Application.ProcessMessages;
  ShowTopPanels(True);
  Application.ProcessMessages;

  ShowPanelsBottom(True, True);
  Application.ProcessMessages;
  ShowPanelsTop(True, True);
  Application.ProcessMessages;
end;
}

{$ENDREGION}


// Entries =========================================

{$REGION Entries }

function TKntNoteUI.GetNEntryUI (Panel: TNEntriesPanel): TKntNoteEntriesUI;
var
  pnl: TPanel;
begin
   pnl:= GetPanel(panel);
   if FNEntryUI[Panel] =  nil then begin
      FNEntryUI[Panel]:= TKntNoteEntriesUI.Create(pnl, Self );
      FNEntryUI[Panel].Parent:= pnl;
   end;

   Result:= FNEntryUI[Panel];
end;


function TKntNoteUI.GetNEntryUI (Editor: TKntRichEdit): TKntNoteEntriesUI;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if (FNEntryUI[p] <> nil) and (FNEntryUI[p].Editor = Editor) then
         exit (FNEntryUI[p]);

   Result:= nil;
end;

procedure TKntNoteUI.Refresh;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntryUI[p] <> nil then
         FNEntryUI[p].UpdateEntriesHeaderWidth(True);
end;

procedure TKntNoteUI.CreateNewEntry(RequestedFromEditor: TKntRichEdit);
begin
   CreateNewEntry(GetNEntryUI(RequestedFromEditor));
end;

procedure TKntNoteUI.CreateNewEntry(RequestedFromNEntryUI: TKntNoteEntriesUI);
var
  NEntry: TNoteEntry;
  NEntriesUI: TKntNoteEntriesUI;

  PanelConfig: TPanelConfiguration;
  PnlEdit: TNEntriesPanel;

begin
   if (RequestedFromNEntryUI = nil) or (Note = nil) then exit;

   NEntry:= Note.AddNewEntry;
   Folder.Modified:= True;

   if FNewNNodeUIConfig and (NNode.note.NumEntries = 2) then begin
      FNNodeUIConfig.Free;
      FNNodeUIConfig:= TNNodeUIConfiguration.CreateDefault (NNode);
      RequestedFromNEntryUI.PanelConfig:= FNNodeUIConfig.PanelConfig(pnCenter);
   end;

   PnlEdit:= NoteAdvOptions.EditCentralPanelEntriesIn;
   PanelConfig:= FNNodeUIConfig.PanelConfig(PnlEdit);
   PanelConfig.NEntryID:= NEntry.ID;
   NEntriesUI:= GetNEntryUI(PnlEdit);
   NEntriesUI.LoadFromNNode(NNode, PanelConfig, True);
   case PnlEdit of
      pnTL: ShowPanelsTop(True, False);
      pnBL: ShowPanelsBottom(True, False);
   end;

   FNNodeDeleted:= false;

   NEntriesUI.SetFocusOnEditor;
end;


procedure TKntNoteUI.NEntryUIEditorEnter(Sender: TObject);
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntryUI[p] <> nil then
         if not FNEntryUI[p].HideNestedFloatingEditor then
            exit;

  Timer.Enabled:= True;
end;

procedure TKntNoteUI.TimerTimer(Sender: TObject);
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if (p <> pnCenter) and (FNEntryUI[p] <> nil)  then
         FNEntryUI[p].HideTemporarilyInfoPanel;

   Timer.Enabled:= False;
end;


{$ENDREGION}

// Tags =========================================

{$REGION Tags }

procedure TKntNoteUI.RefreshTags;
begin
   FNEntryUI[pnCenter].RefreshTags;
end;

procedure TKntNoteUI.EditTags;
begin
   FNEntryUI[pnCenter].EditTags;
end;



{$ENDREGION}


// Load and save Editor from Note node =========================================

{$REGION Load, save and configure Editor for a Note node }


function TKntNoteUI.GetNNode: TNoteNode;
begin
   Result:= FNNode;
end;

function TKntNoteUI.GetFolder: TObject;
begin
   Result:= FKntFolder;
end;

procedure TKntNoteUI.LoadFromNNode(NNode: TNoteNode; SavePreviousContent: boolean);
var
   ShowPanels: boolean;
   P: TNEntriesPanel;
   i: integer;
   PanelConfig: TPanelConfiguration;
   ShowPanel: array[TNEntriesPanel] of boolean;
begin
   if SavePreviousContent and (FNNode <> nil) then
      SaveToDataModel;

   FNNode:= NNode;
   FNNodeUIConfig:= nil;

   if assigned(NNode) then begin
     FNote:= NNode.Note;
     FNNodeUIConfig:= Folder.GetNNodeUIConfig(NNode);
     FNewNNodeUIConfig:= false;
     if FNNodeUIConfig = nil then begin
        FNNodeUIConfig:= TNNodeUIConfiguration.CreateDefault (NNode);
        FNewNNodeUIConfig:= true;
     end;
   end;

   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      ShowPanel[p]:= false;

   for i := 0 to High(FNNodeUIConfig.PanelsConfig) do begin
       PanelConfig:= FNNodeUIConfig.PanelsConfig[i];
       if PanelConfig.Visible then begin
          ShowPanel[PanelConfig.Panel]:= True;
          GetNEntryUI(PanelConfig.Panel).LoadFromNNode(NNode, PanelConfig, False);
       end;
   end;

   // Clear unused editors  (##)
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
       if not ShowPanel[p] and (FNEntryUI[p] <> nil) then
         FNEntryUI[p].SetAsUnused;

   ShowLeftPanel(False);
   ShowPanelsTop(ShowPanel[pnTL], ShowPanel[pnTR]);
   ShowPanelsBottom(ShowPanel[pnBL], ShowPanel[pnBR]);
   FrameResize(nil);

   FNNodeDeleted:= false;
end;

function TKntNoteUI.ReloadMetadataFromDataModel(ReloadTags: boolean = true): TNoteEntry;
begin
   Result:= FNEntryUI[pnCenter].ReloadMetadataFromDataModel(ReloadTags);
end;


procedure TKntNoteUI.ReloadFromDataModel;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntryUI[p] <> nil then
         FNEntryUI[p].ReloadFromDataModel;
end;

procedure TKntNoteUI.SaveToDataModel;
var
  p: TNEntriesPanel;
  iOnUse: integer;
const
  TTNEntriesPanelCount = Ord(High(TNEntriesPanel)) - Ord(Low(TNEntriesPanel)) + 1;

begin
   Log_StoreTick('TKntNoteUI.SaveToDataModel - BEGIN', 4, +1);

   SetLength(FNNodeUIConfig.PanelsConfig, TTNEntriesPanelCount);

   iOnUse:= 0;
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntryUI[p] <> nil then begin
         FNEntryUI[p].SaveToDataModel;
         if FNEntryUI[p].OnUse then begin
            FNNodeUIConfig.PanelsConfig[iOnUse]:= FNEntryUI[p].PanelConfig;
            inc(iOnUse);
         end;
      end;

   SetLength(FNNodeUIConfig.PanelsConfig, iOnUse);
   if FNewNNodeUIConfig and (NNode.Note.NumEntries > 1) then begin  // ToDO .. and configuration has changed and it is <> than default
      Folder.AddNNodeUIConfig(FNNodeUIConfig);
      FNewNNodeUIConfig:= false;
   end;

   Log_StoreTick('TKntNoteUI.SaveToDataModel - END', 4, -1);
end;

procedure TKntNoteUI.ReloadNoteName;
begin
   FNEntryUI[pnCenter].ReloadNoteName;
end;


procedure TKntNoteUI.NNodeDeleted;
begin
   FNNodeDeleted:= True;
end;

function TKntNoteUI.GetNNodeDeleted: boolean;
begin
  Result:= FNNodeDeleted;
end;

procedure TKntNoteUI.ConfigureEditor;
begin
  FNEntryUI[pnCenter].ConfigureEditor;
end;


{$ENDREGION}


// Images  =========================================

{$REGION Images }

function TKntNoteUI.GetImagesInstances: TImageIDs;
begin
//##...
   Result:= FNEntryUI[pnCenter].ImagesInstances;
end;

procedure TKntNoteUI.GetImagesIDInstances (Stream: TMemoryStream; TextPlain: String);
begin
//##...
   FNEntryUI[pnCenter].GetImagesIDInstances(Stream, TextPlain);
end;


procedure TKntNoteUI.ResetImagesReferenceCount;
begin
    FNEntryUI[pnCenter].ResetImagesReferenceCount;
end;

procedure TKntNoteUI.ReloadImagesOnEditor;
begin
   FNEntryUI[pnCenter].ReloadImagesOnEditor;
end;


procedure TKntNoteUI.ReconsiderImageDimensionGoalsOnEditor(Selection: boolean; ImagesMode: TImagesMode);
begin
   FNEntryUI[pnCenter].ReconsiderImageDimensionGoalsOnEditor(Selection, ImagesMode);
end;


procedure TKntNoteUI.SetImagesMode(ImagesMode: TImagesMode);
begin
   FNEntryUI[pnCenter].SetImagesMode(ImagesMode);
end;

{$ENDREGION}


end.
