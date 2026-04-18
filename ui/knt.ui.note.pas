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
  TKntNoteEntriesUIArray = Array of TKntNoteEntriesUI;

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

    FNEntriesUI: array[TNEntriesPanel] of TKntNoteEntriesUI;
    FNNodeUIConfig: TNNodeUIConfiguration;
    FNewNNodeUIConfig: boolean;
    FSelectedNEntriesUI: TKntNoteEntriesUI;
    FQueryLayout: boolean;
    FMultipleVisibleEditors: boolean;

    fSplitterNoteMoving: boolean;
    FUpdatingOnResize: boolean;
    IncResize: integer;

    TimerInfoPanel: TTimer;

   {$IFDEF KNT_DEBUG}
    FDBGEntriesUI: TKntNoteEntriesUIArray;
   {$ENDIF}

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
    function GetSelectedNEntry: TNoteEntry;
    function GetBasicNEntriesLayout: boolean;

  public
    constructor Create(AOwner: TComponent; KntFolder: TKntFolder);
    destructor Destroy; override;
    //procedure TestCreatePanel;

    property Editor : TKntRichEdit read GetEditor;

  public
    property Folder: TKntFolder read FKntFolder;
    property Note: TNote read FNote;
    property NNode: TNoteNode read GetNNode;
    property SelectedNEntry: TNoteEntry read GetSelectedNEntry;
    procedure LoadFromNNode (NNode: TNoteNode; SavePreviousContent: boolean;
                             QueryLayoutToUse: TBasicNEntriesLayout;
                             EditingNEntry: TNoteEntry = nil;
                             OfferEditorForNewEntry: boolean = False);
    procedure ReloadFromDataModel;
    procedure ReloadMetadataFromDataModel(ReloadTags: boolean = true);
    procedure SaveToDataModel;
    procedure ReloadNoteName;
    procedure ConfigureEditor;

  protected
    procedure SetInfoPanelHidden(value: boolean);

  protected
    function GetNEntriesUI (Panel: TNEntriesPanel): TKntNoteEntriesUI; overload;
    function GetNEntriesUI (Editor: TKntRichEdit): TKntNoteEntriesUI; overload;
    procedure NewEntryRequested(ReqFromEditor: TKntRichEdit);
    procedure SelectNextEntry;
    procedure SelectPreviousEntry;
    procedure CreateNewEntry(ReqFromNEntriesUI: TKntNoteEntriesUI); overload;
    procedure EditInInMultiEntries(ReqFromNEntriesUI: TKntNoteEntriesUI; NEntry: TNoteEntry; NewEntry: boolean;
                                   SS: integer=-1; SL: integer=-1);
    procedure IntroInEditorOfEntriesUI(RequestedFromEditor: TKntRichEdit; CtrlDown: boolean);
    procedure EditorChangedInEmptyPanel(Editor: TKntRichEdit);
    procedure UpdateFMultipleVisibleEditors;
    procedure TimerInfoTimer(Sender: TObject);
 public
    procedure NEntriesUIEditorEnter(Sender: TObject);
    function GetSelectedNEntriesUI (Editor: TKntRichEdit): TObject;
    function MultipleVisibleEditors: boolean;
    function NavigatePanels(NavDirection: TNavDirection): boolean;
    procedure ToggleMaximizeSelectedPanel;
    procedure KeepInfoPanelTemporarilyVisible;
   {$IFDEF KNT_DEBUG}
    function GetDBG_NEntriesUI(): TKntNoteEntriesUIArray;
   {$ENDIF}


 public
    procedure Refresh;
  public
    procedure ShowLeftPanel(value: boolean);
    procedure ShowTopPanels(value: boolean);
    procedure ShowBottomPanels(value: boolean);
    procedure ShowPanelsTop(TL, TR: boolean);
    procedure ShowPanelsBottom(BL, BR: boolean);
    procedure ToggleMaximizePanel (Panel: TNEntriesPanel);
    procedure RestoreSplits;
    function GetPanel (Panel: TNEntriesPanel): TPanel;
    //procedure TestPanels;

  protected
    function GetReadOnly: boolean;
    procedure SetReadOnly( AReadOnly : boolean );

  protected
    function GetImagesInstances: TImageIDs;
    property ImagesInstances: TImageIDs read GetImagesInstances;
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
  kn_ImagesUtils,
  kn_VCLControlsMng,
  knt.RS;

const SPLT_WIDTH = 2;

// Create  / Destroy =========================================

{$REGION Create / Destroy}

constructor TKntNoteUI.Create(AOwner: TComponent; KntFolder: TKntFolder);
var
  p: TNEntriesPanel;
begin
   inherited Create(AOwner);

   splL.Width:=  SPLT_WIDTH;
   splTC.Width:= SPLT_WIDTH;
   splBC.Width:= SPLT_WIDTH;
   splT.Height:= SPLT_WIDTH;
   splB.Height:= SPLT_WIDTH;

   FKntFolder:= KntFolder;
   FNNode:= nil;
   FNote:= nil;
   FMultipleVisibleEditors:= false;

   for p := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do
      FNEntriesUI[p]:= nil;

   FNEntriesUI[pnCenter]:= TKntNoteEntriesUI.Create( PnlCenter, Self );
   FNEntriesUI[pnCenter].Parent:= PnlCenter;
   FSelectedNEntriesUI:= FNEntriesUI[pnCenter];

   //TestCreatePanel;

   fSplitterNoteMoving:= false;
   FUpdatingOnResize:= false;
   TimerInfoPanel:= TTimer.Create(Self);
   TimerInfoPanel.Enabled := false;
   TimerInfoPanel.Interval := 2000;  // 2 seconds
   TimerInfoPanel.OnTimer:= TimerInfoTimer;
end;


destructor TKntNoteUI.Destroy;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntriesUI[p] <> nil then
         FreeAndNil(FNEntriesUI[p]);

   TimerInfoPanel.Free;

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
  Result:= FSelectedNEntriesUI.Editor;
end;


function TKntNoteUI.GetReadOnly: boolean;
begin
   Result:= FNEntriesUI[pnCenter].ReadOnly;
end;

procedure TKntNoteUI.SetReadOnly( AReadOnly : boolean );
begin
   FNEntriesUI[pnCenter].ReadOnly:= AReadOnly;
end;


procedure TKntNoteUI.SetOnEnter(AEvent: TNotifyEvent);
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntriesUI[p] <> nil then
         FNEntriesUI[p].SetOnEnter(AEvent);
end;

procedure TKntNoteUI.SetOnMouseUpOnNote(AEvent: TNotifyEvent);
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntriesUI[p] <> nil then
         FNEntriesUI[p].SetOnMouseUpOnNote(AEvent);
end;

procedure TKntNoteUI.SetOnMouseMoveOnNote(AEvent: TNotifyEvent);
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntriesUI[p] <> nil then
         FNEntriesUI[p].SetOnMouseMoveOnNote(AEvent);
end;


procedure TKntNoteUI.SetInfoPanelHidden(value: boolean);
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntriesUI[p] <> nil then
         FNEntriesUI[p].InfoPanelHidden:= value;

   if not value then
      TimerInfoPanel.Enabled:= True;
end;


procedure TKntNoteUI.SetFocusOnEditor;
begin
   FSelectedNEntriesUI.SetFocusOnEditor;
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
      splT.Top := pnlTop.Height + SPLT_WIDTH;
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
    FNEntriesUI[pnCenter].Editor.BeginUpdate;
    if FNEntriesUI[pnBL] <> nil then
       FNEntriesUI[pnBL].Editor.BeginUpdate;
    if FNEntriesUI[pnBR] <> nil then
       FNEntriesUI[pnBR].Editor.BeginUpdate;
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
     FNEntriesUI[pnCenter].Editor.EndUpdate;
    if FNEntriesUI[pnBL] <> nil then
       FNEntriesUI[pnBL].Editor.EndUpdate;
    if FNEntriesUI[pnBR] <> nil then
       FNEntriesUI[pnBR].Editor.EndUpdate;
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
      pnlCenter.Height:= pnlAuxC3.Height - Round(Self.Height * FNNodeUIConfig.Bottom_Ratio) - SPLT_WIDTH;
      splB.Top := pnlBottom.Top - SPLT_WIDTH;
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
var
   H: integer;
begin
   if pnlBL.Visible and pnlBR.Visible then
      pnlBL.Width:= Round(pnlBottom.Width * FNNodeUIConfig.BLBR_Ratio);
   if pnlTL.Visible and pnlTR.Visible then
      pnlTL.Width:= Round(pnlTop.Width * FNNodeUIConfig.TLTR_Ratio);
   if pnlTop.Visible then
      pnlTop.Height:= Round(Self.Height * FNNodeUIConfig.Top_Ratio);
   if pnlBottom.Visible then begin
      H:= pnlAuxC3.Height - Round(Self.Height * FNNodeUIConfig.Bottom_Ratio);
      if not FNNodeUIConfig.SelectedPanelMaximized then
         dec(H, SPLT_WIDTH);
      pnlCenter.Height:= H;
   end;
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

procedure TKntNoteUI.RestoreSplits;
var
  TL, TR: boolean;
  BL, BR: boolean;
begin
   if pnlTop.Visible then begin
       splT.Visible:= True;
       splT.Top := pnlTop.Height + SPLT_WIDTH;
       TL:= pnlTL.Visible;
       TR:= pnlTR.Visible;
       if TL and TR then begin
          splTC.Visible:= True;
          splTC.Left:= pnlTL.Width;
       end
       else
         splTC.Visible:= False;
   end;

   if pnlBottom.Visible then begin
       splB.Top := pnlBottom.Top - SPLT_WIDTH;
       splB.Visible:= true;
       if BL and BR then begin
          splBC.Visible:= True;
          splBC.Left:= pnlBL.Width;
       end
       else
         splBC.Visible:= False;
   end;

end;

procedure TKntNoteUI.ToggleMaximizePanel (Panel: TNEntriesPanel);
var
  pnl: TPanel;
begin
   if not MultipleVisibleEditors then exit;

   pnl:= GetPanel(panel);
   if not (pnl.Visible and (FNEntriesUI[Panel] <> nil)) then exit;

   if not FNNodeUIConfig.SelectedPanelMaximized then begin
      FNNodeUIConfig.PanelMaximized:= Panel;
      if not ActiveFolder.EditorInfoPanelHidden then
        FSelectedNEntriesUI.PanelConfig.ShowEditorInfoPanel:= True;
   end
   else begin
      FNNodeUIConfig.SelectedPanelMaximized:= False;
      FSelectedNEntriesUI.PanelConfig.ShowEditorInfoPanel:= (not ActiveFolder.EditorInfoPanelHidden and (FNNodeUIConfig.GetVisibleBottomPanel = Panel));
   end;

   FSelectedNEntriesUI.ReconsiderInfoPanelVisibility;

   splT.Visible:= False;
   splB.Visible:= False;
   splBC.Visible:= False;
   splTC.Visible:= False;


   FrameResize(nil);
   if not FNNodeUIConfig.SelectedPanelMaximized then
       RestoreSplits;
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

function TKntNoteUI.GetNEntriesUI (Panel: TNEntriesPanel): TKntNoteEntriesUI;
var
  pnl: TPanel;
begin
   pnl:= GetPanel(panel);
   if FNEntriesUI[Panel] =  nil then begin
      FNEntriesUI[Panel]:= TKntNoteEntriesUI.Create(pnl, Self );
      SetUpEditor(FNEntriesUI[Panel].Editor, FNEntriesUI[pnCenter].Editor.ZoomGoal);
      FNEntriesUI[Panel].Parent:= pnl;
   end;

   Result:= FNEntriesUI[Panel];
end;


function TKntNoteUI.GetNEntriesUI (Editor: TKntRichEdit): TKntNoteEntriesUI;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].Editor = Editor) then
         exit (FNEntriesUI[p]);

   Result:= nil;
end;

function TKntNoteUI.GetSelectedNEntriesUI (Editor: TKntRichEdit): TObject;
begin
   Result:= GetNEntriesUI(Editor);
end;


function TKntNoteUI.MultipleVisibleEditors: boolean;
begin
   Result:= FMultipleVisibleEditors;
end;

procedure TKntNoteUI.UpdateFMultipleVisibleEditors;
var
  p: TNEntriesPanel;
  i: integer;
begin
   FMultipleVisibleEditors:= false;
   i:= 0;
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if (FNEntriesUI[p] <> nil) and FNEntriesUI[p].OnUse then begin
         inc(i);
         if i > 1 then begin
            FMultipleVisibleEditors:= True;
            exit;
         end;
      end;
end;


function TKntNoteUI.NavigatePanels(NavDirection: TNavDirection): boolean;
var
  pnl, nextPnl: TNEntriesPanel;
  panelConfig: TPanelConfiguration;

begin
  Result:= false;
  if not FMultipleVisibleEditors or FNNodeUIConfig.SelectedPanelMaximized then exit;
  if (FSelectedNEntriesUI = nil) or not FSelectedNEntriesUI.Editor.NavigatePanelsEnabled then exit;

  pnl:= FSelectedNEntriesUI.PanelConfig.Panel;

  case NavDirection of
     navUp  :  if not FNNodeUIConfig.GetUpperVisiblePanel(pnl, nextPnl) then
                  exit;

     navDown:  if not FNNodeUIConfig.GetBelowVisiblePanel(pnl, nextPnl) then
                  exit;

     navLeft:  begin
                 if pnl = pnTR then
                    nextPnl:= pnTL
                 else
                 if pnl = pnBR then
                    nextPnl:= pnBL
                 else
                    exit;
               end;

     navRight: begin
                 if pnl = pnTL then
                    nextPnl:= pnTR
                 else
                 if pnl = pnBL then
                    nextPnl:= pnBR
                 else
                    exit;
               end;
  end;


  panelConfig:= FNNodeUIConfig.GetCreatedPanelConfig(nextPnl);
  if (panelConfig <> nil) and panelConfig.Visible then begin
     FNEntriesUI[nextPnl].SetFocusOnEditor;
     Result:= True;
  end;
end;


procedure TKntNoteUI.ToggleMaximizeSelectedPanel;
begin
   ToggleMaximizePanel(FSelectedNEntriesUI.PanelConfig.Panel);
end;


{$IFDEF KNT_DEBUG}
function TKntNoteUI.GetDBG_NEntriesUI(): TKntNoteEntriesUIArray;
var
  p: TNEntriesPanel;
  i: integer;
begin
   SetLength(FDBGEntriesUI, TNEntriesPanel_Count);
   i:= 0;
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntriesUI[p] <> nil then begin
          FDBGEntriesUI[i]:= FNEntriesUI[p];
          inc(i);
      end;
   SetLength(FDBGEntriesUI, i);
   Result:= FDBGEntriesUI;
end;

{$ENDIF}



procedure TKntNoteUI.Refresh;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntriesUI[p] <> nil then
         FNEntriesUI[p].RefreshEntry;
end;


// Only Ctrl+INTRO are intercepted
// or, also, INTRO in editors where Editor.MultiEntries = True (=> NEntriesUI.Mode = meMultiEntries)
procedure TKntNoteUI.IntroInEditorOfEntriesUI(RequestedFromEditor: TKntRichEdit; CtrlDown: boolean);
var
   NEntriesUI: TKntNoteEntriesUI;
   NEntry: TNoteEntry;
   SS, SL: integer;
begin
  if ActiveFile.EncryptedContentMustBeHidden and FNote.IsEncrypted then exit;

  NEntriesUI:= GetNEntriesUI(RequestedFromEditor);
  NEntry:= NEntriesUI.NEntry;

  if CtrlDown then begin                            // QueryLayout <-> EditingLoayout
     if (NEntriesUI.Mode = meSingleEntry) then begin
        if FQueryLayout or (NEntriesUI.PanelConfig.VinculatedTags <> nil) then
           NEntriesUI.btnToggleMultiClick(nil)
        else begin
           ActiveFile.SetNoteIsOnEditingLayout(FNote, false);
           LoadFromNNode(FNNode, True, neQueryLayout);
        end;
     end
     else begin
       if FQueryLayout then begin
          NEntriesUI.SavePositionInPanel;
          SS:= NEntriesUI.PanelConfig.SelStart;
          SL:= NEntriesUI.PanelConfig.SelLength;
          LoadFromNNode(FNNode, True, neEditingLayout, NEntry);
          EditInInMultiEntries(NEntriesUI, NEntry, false, SS, SL);
       end
       else begin
          ActiveFile.SetNoteIsOnEditingLayout(FNote, false);
          LoadFromNNode(FNNode, True, neQueryLayout);
       end;
     end;

  end
  else begin                                        // Not CtrlDown => Mode= meMultiEntries
     if FQueryLayout then
        NEntriesUI.btnToggleMultiClick(nil)
     else
        EditInInMultiEntries(NEntriesUI, NEntry, false);
  end;

end;


procedure TKntNoteUI.EditorChangedInEmptyPanel(Editor: TKntRichEdit);
var
  p: TNEntriesPanel;
begin
   Editor.OnEditorChanged:= nil;

   // This handler is configured only to 'listen' for editor changes in 'empty' panels, without any entry set yet.
   CreateNewEntry(GetNEntriesUI(Editor));
end;


procedure TKntNoteUI.NewEntryRequested(ReqFromEditor: TKntRichEdit);
var
  ReqFromNEntriesUI: TKntNoteEntriesUI;
begin
   if ActiveFile.EncryptedContentMustBeHidden and FNote.IsEncrypted then exit;

   ReqFromNEntriesUI:= GetNEntriesUI(ReqFromEditor);
   { LoadFromNNode(.., True, neEditingLayout) if FQueryLayout:
     The Query mode does allow modifications from the visible panels, but only while mode = meSingleEntry.
     It's simply a different configuration/layout, designed for viewing notes as we navigate through the tree.
     This mode is typically configured to offer fewer panels, or only when there is data to display.
     For example, if the note has only one entry, normally only one panel will be shown. }
   if FQueryLayout then
      LoadFromNNode(FNNode, True, neEditingLayout, nil, true)
   else
      EditInInMultiEntries(ReqFromNEntriesUI, nil, true);

   FSelectedNEntriesUI.Editor.OnEditorChanged := EditorChangedInEmptyPanel;
end;


procedure TKntNoteUI.SelectNextEntry;
begin
   FSelectedNEntriesUI.btnNextEntryClick(nil);
end;


procedure TKntNoteUI.SelectPreviousEntry;
begin
   FSelectedNEntriesUI.btnPrevEntryClick(nil);
end;


procedure TKntNoteUI.CreateNewEntry(ReqFromNEntriesUI: TKntNoteEntriesUI);
var
  NewNEntry: TNoteEntry;
  p: TNEntriesPanel;
begin
   if (ReqFromNEntriesUI = nil) or (Note = nil) then exit;

   if (ReqFromNEntriesUI.Editor.TextLength=0) then exit;  // Do not create a new entry by mistake if the current entry is empty

   NewNEntry:= Note.AddNewEntry;
   Folder.Modified:= True;
   if ReqFromNEntriesUI.PanelConfig.VinculatedTags <> nil then
      NewNEntry.Tags:= ReqFromNEntriesUI.PanelConfig.VinculatedTags;

   if ReqFromNEntriesUI.NEntry = nil then begin
      // Add new entry in panel (the user has just started making changes in the empty editor of the associated panel)
      ReqFromNEntriesUI.PanelConfig.SelNEntry:= NewNEntry;
      ReqFromNEntriesUI.NEntry:= NewNEntry;
      ReqFromNEntriesUI.ReloadMetadataFromDataModel;
      ReqFromNEntriesUI.ConfigureEditor;
   end;

   // Inform the panels that a new entry has been added. Those panels where it fits will include it, initially only showing the header
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do begin
      if (FNEntriesUI[p] <> nil) and ((FNEntriesUI[p].OnUse)) then
         FNEntriesUI[p].ReloadFromDataModel(false, NewNEntry, aCreated);
   end;

end;


procedure TKntNoteUI.EditInInMultiEntries(ReqFromNEntriesUI: TKntNoteEntriesUI; NEntry: TNoteEntry; NewEntry: boolean;
                                          SS: integer=-1; SL: integer=-1);
var
  NEntriesUI: TKntNoteEntriesUI;
  PanelConfig: TPanelConfiguration;
  PnlEdit: TNEntriesPanel;
  RequestedFromMultiEntry: boolean;

begin
   if (ReqFromNEntriesUI = nil) or (Note = nil) then exit;

   RequestedFromMultiEntry:= (ReqFromNEntriesUI.Mode = meMultipleEntries);

   if (ReqFromNEntriesUI.Mode = meMultipleEntries) and (ReqFromNEntriesUI.PanelConfig.VinculatedTags = nil) then begin
      if not FNNodeUIConfig.GetSingleEntryPanelForEditing(PnlEdit) then begin
         PnlEdit:= ReqFromNEntriesUI.PanelConfig.Panel;
         if not NewEntry then begin
            ReqFromNEntriesUI.btnToggleMultiClick(nil);       // Use requested NEntriesUI for editing
            exit;
         end;
      end;
      NEntriesUI:= GetNEntriesUI(PnlEdit);
   end
   else
      NEntriesUI:= ReqFromNEntriesUI;

   NEntriesUI.SaveToDataModel();

   PanelConfig:= NEntriesUI.PanelConfig;
   PanelConfig.SelNEntry:= NEntry;

  { AutoCollapseEntryOnEditing
   if NEntriesUI <> ReqFromNEntriesUI then begin
      ReqFromNEntriesUI.SavePositionInPanel;
      if NEntry <> nil then
         ReqFromNEntriesUI.ReloadVisibleContentOfEntries(false, cmOnlyHeader, ReqFromNEntriesUI.GetIndexOfVisibleEntry(NEntry));
      PanelConfig.SelStart:= ReqFromNEntriesUI.PanelConfig.SelStart;
      PanelConfig.SelLength:= ReqFromNEntriesUI.PanelConfig.SelLength;
   end;
   }
   if SS >= 0 then begin
      PanelConfig.SelStart:= SS;
      PanelConfig.SelLength:= SL;
   end;

   NEntriesUI.Editor.HideNestedFloatingEditor;
   NEntriesUI.Mode:= meSingleEntry;
   NEntriesUI.ReloadFromDataModel(True, nil, aNull, true);

   FNNodeDeleted:= false;
   NEntriesUI.SetFocusOnEditor;
end;


procedure TKntNoteUI.NEntriesUIEditorEnter(Sender: TObject);
var
  p: TNEntriesPanel;
begin
   if not FloatingEditorCannotBeSaved then
      for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
        if FNEntriesUI[p] <> nil then begin
           FNEntriesUI[p].Editor.NavigatePanelsEnabled:= True;
           if not FNEntriesUI[p].HideNestedFloatingEditor then
              exit;
        end;

  FSelectedNEntriesUI:= TKntNoteEntriesUI(Sender);
  TimerInfoPanel.Enabled:= False;
  TimerInfoPanel.Enabled:= True;
end;

procedure TKntNoteUI.TimerInfoTimer(Sender: TObject);
var
  p: TNEntriesPanel;
  KeepEnabled: boolean;
begin
   KeepEnabled:= False;
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].PanelConfig <> nil) and not FNEntriesUI[p].PanelConfig.ShowEditorInfoPanel then
         if not FNEntriesUI[p].HideTemporarilyInfoPanel then
            KeepEnabled:= True;

   if not KeepEnabled then
      TimerInfoPanel.Enabled:= False;
end;


procedure TKntNoteUI.KeepInfoPanelTemporarilyVisible;
begin
  TimerInfoPanel.Enabled:= False;
  TimerInfoPanel.Enabled:= True;
end;


{$ENDREGION}

// Tags =========================================

{$REGION Tags }

procedure TKntNoteUI.RefreshTags;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntriesUI[p] <> nil then
         FNEntriesUI[p].RefreshTags;
end;

procedure TKntNoteUI.EditTags;
begin
   FSelectedNEntriesUI.EditTags;
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

function TKntNoteUI.GetSelectedNEntry: TNoteEntry;
begin
   Result:= FSelectedNEntriesUI.NEntry;
end;

function TKntNoteUI.GetBasicNEntriesLayout: boolean;
begin
   Result:= FQueryLayout;
end;

procedure TKntNoteUI.LoadFromNNode(NNode: TNoteNode; SavePreviousContent: boolean;
                                   QueryLayoutToUse: TBasicNEntriesLayout;
                                   EditingNEntry: TNoteEntry = nil;
                                   OfferEditorForNewEntry: boolean = False);
var
   ShowPanels: boolean;
   Pnl, PnlVisibleBottom, PnlEdit, PnlToSetFocus, MainPanel: TNEntriesPanel;
   i: integer;
   PanelConfig: TPanelConfiguration;
   ShowPanel: array[TNEntriesPanel] of boolean;
   NEntriesUI: TKntNoteEntriesUI;
   QueryLayout: boolean;
   DefinedSingleEntryPanelForEditing: boolean;
   SetNoteSelEntryOnMainPanel: boolean;
   EnableNavigatePanels: boolean;
begin
   EnableNavigatePanels:= (QueryLayoutToUse <> neLastLayout);

   if SavePreviousContent and (FNNode <> nil) then
      SaveToDataModel;

   if FloatingEditorCannotBeSaved then exit;

   // When switching from EditingLayout to QueryLayout -> Set the NEntry of the current panel to the one selected in the main panel
   // This will have been saved in FNote.SelEntry from TKntNoteUI.SaveToDataModel
   SetNoteSelEntryOnMainPanel:= (QueryLayoutToUse = neQueryLayout) and not FQueryLayout;

   FNNode:= NNode;
   FNNodeUIConfig:= nil;
   QueryLayout:= True;

   if assigned(NNode) then begin
     FNote:= NNode.Note;
     if QueryLayoutToUse = neLastLayout then
        QueryLayout:= not ActiveFile.GetNoteIsOnEditingLayout(FNote)
     else
        QueryLayout:= (QueryLayoutToUse = neQueryLayout);

     FNNodeUIConfig:= Folder.GetNNodeUIConfig(NNode, QueryLayout);
     FNewNNodeUIConfig:= false;
     if FNNodeUIConfig = nil then begin
        FNNodeUIConfig:= TNNodeUIConfiguration.CreateDefault (NNode, Folder, QueryLayout);
        FNewNNodeUIConfig:= true;
     end;
   end
   else
      FNNodeUIConfig:= TNNodeUIConfiguration.CreateDefault (nil, Folder, QueryLayout);

   FQueryLayout:= QueryLayout;

   for Pnl := Low(TNEntriesPanel) to High(TNEntriesPanel) do begin
      ShowPanel[Pnl]:= false;
      if (FNEntriesUI[Pnl] <> nil) and not FNEntriesUI[Pnl].HideNestedFloatingEditor then
         exit;
   end;

   if assigned(NNode) then begin
      PnlVisibleBottom:= FNNodeUIConfig.GetVisibleBottomPanel;
      MainPanel:= FNNodeUIConfig.GetMainPanel;
      PnlToSetFocus:= MainPanel;
      if OfferEditorForNewEntry then begin
         DefinedSingleEntryPanelForEditing:= FNNodeUIConfig.GetSingleEntryPanelForEditing(PnlEdit);
         if DefinedSingleEntryPanelForEditing then
            PnlToSetFocus:= PnlEdit;
      end;
      if FNNodeUIConfig.SelectedPanelMaximized then
         PnlToSetFocus:= FNNodeUIConfig.PanelMaximized;

      for i := 0 to High(FNNodeUIConfig.PanelsConfig) do begin
          PanelConfig:= FNNodeUIConfig.PanelsConfig[i];
          Pnl:= PanelConfig.Panel;
          if PanelConfig.Visible then begin
             ShowPanel[Pnl]:= True;
             PanelConfig.ShowEditorInfoPanel:= (PnlVisibleBottom = Pnl) or (FNNodeUIConfig.SelectedPanelMaximized and (FNNodeUIConfig.PanelMaximized= Pnl));
             NEntriesUI:= GetNEntriesUI(Pnl);

             { AutoCollapseEntryOnEditing
             if (EditingNEntry <> nil) and not QueryLayout and (PanelConfig.Mode = meMultipleEntries) and (PanelConfig.VinculatedTags = nil) then begin
                SetLength(PanelConfig.EntriesOnlyHeader, Length(PanelConfig.EntriesOnlyHeader)+1);
                PanelConfig.EntriesOnlyHeader[Length(PanelConfig.EntriesOnlyHeader)-1]:= EditingNEntry;
             end;
             }
             if SetNoteSelEntryOnMainPanel and (Pnl = MainPanel) then begin
                PanelConfig.SelNEntry:= FNote.SelEntry;
                PanelConfig.SelStart:= FNote.SelStart;
                PanelConfig.SelLength:= FNote.SelLength;
                PanelConfig.ScrollPosInEditor.Y:= 0;
             end;

             if OfferEditorForNewEntry and (Pnl = PnlToSetFocus) then begin
                PanelConfig.SelNEntry:= nil;
                NEntriesUI.Mode:= meSingleEntry;
             end;
             NEntriesUI.LoadFromDataModel(PanelConfig, False, (Pnl = PnlToSetFocus));

             if (NEntriesUI.NEntry = nil) then
                NEntriesUI.Editor.OnEditorChanged := EditorChangedInEmptyPanel
             else
                NEntriesUI.Editor.OnEditorChanged := nil;

             NEntriesUI.Editor.NavigatePanelsEnabled:= EnableNavigatePanels;
          end;
      end;
   end
   else
      GetNEntriesUI(pnCenter).LoadFromDataModel(nil, False);


   // Clear unused editors  (##)
   for Pnl := Low(TNEntriesPanel) to High(TNEntriesPanel) do
       if not ShowPanel[Pnl] and (FNEntriesUI[Pnl] <> nil) then
          FNEntriesUI[Pnl].SetAsUnused;

   UpdateFMultipleVisibleEditors;

   ShowLeftPanel(False);
   ShowPanelsTop(ShowPanel[pnTL], ShowPanel[pnTR]);
   ShowPanelsBottom(ShowPanel[pnBL], ShowPanel[pnBR]);
   FrameResize(nil);

   if EditingNEntry = nil then begin                       // If <> nil -> Focus in FSelectedNEntriesUI will be set from EditInInMultiEntries
      FSelectedNEntriesUI:= GetNEntriesUI(PnlToSetFocus);
      if not ActiveTreeUI.Focused then
         FSelectedNEntriesUI.SetFocusOnEditor;
      FSelectedNEntriesUI.Editor.NavigatePanelsEnabled:= EnableNavigatePanels;
   end;

   if not QueryLayout and not Folder.EditorInfoPanelHidden then
      KeepInfoPanelTemporarilyVisible
   else
      TimerInfoTimer(nil);

   if not QueryLayout then
      ActiveFile.SetNoteIsOnEditingLayout(FNote, True);

{$IFDEF KNT_DEBUG}
   GetDBG_NEntriesUI;
{$ENDIF}

   FNNodeDeleted:= false;
end;

procedure TKntNoteUI.ReloadMetadataFromDataModel(ReloadTags: boolean = true);
begin
   FNEntriesUI[pnCenter].ReloadMetadataFromDataModel(ReloadTags);        //***
end;


procedure TKntNoteUI.ReloadFromDataModel;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntriesUI[p] <> nil then
         FNEntriesUI[p].ReloadFromDataModel(true,nil,aNull, (FNEntriesUI[p]=FSelectedNEntriesUI) );
end;

procedure TKntNoteUI.SaveToDataModel;
var
  p: TNEntriesPanel;
  iOnUse: integer;
  SelNEntriesUI: TKntNoteEntriesUI;

begin
   Log_StoreTick('TKntNoteUI.SaveToDataModel - BEGIN', 4, +1);

   SetLength(FNNodeUIConfig.PanelsConfig, TNEntriesPanel_Count);

   iOnUse:= 0;
   SelNEntriesUI:= nil;
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntriesUI[p] <> nil then begin
         FNEntriesUI[p].SaveToDataModel;
         if FNEntriesUI[p].OnUse then begin
            FNEntriesUI[p].SavePositionInPanel;
            FNNodeUIConfig.PanelsConfig[iOnUse]:= FNEntriesUI[p].PanelConfig;
            inc(iOnUse);
            if p in MainPanels then begin                              // Main panels: [pnTL..pnBR]
               if FNEntriesUI[p].PanelConfig.VinculatedTags = nil then
                  SelNEntriesUI:= FNEntriesUI[p];
            end;
         end;
      end;

   if (FSelectedNEntriesUI <> nil) and (FSelectedNEntriesUI.PanelConfig.Panel in MainPanels) and (FSelectedNEntriesUI.PanelConfig.VinculatedTags = nil) then
      SelNEntriesUI:= FSelectedNEntriesUI;

   if SelNEntriesUI <> nil then
      with SelNEntriesUI do begin
         FNote.ScrollPosInEditor:= Editor.GetScrollPosInEditor;
         FNote.SelEntry  := NEntry;
         FNote.SelStart  := PanelConfig.SelStart;
         FNote.SelLength := PanelConfig.SelLength;
      end
   else
      FNote.SelEntry:= nil;


   SetLength(FNNodeUIConfig.PanelsConfig, iOnUse);
   if FNewNNodeUIConfig and (NNode.Note.NumEntries > 1) then begin
      Folder.AddNNodeUIConfig(FNNodeUIConfig);
      FNewNNodeUIConfig:= false;
   end;

   Log_StoreTick('TKntNoteUI.SaveToDataModel - END', 4, -1);
end;

procedure TKntNoteUI.ReloadNoteName;
begin
   FNEntriesUI[pnCenter].ReloadNoteName;
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
  FNEntriesUI[pnCenter].ConfigureEditor;
end;


{$ENDREGION}


// Images  =========================================

{$REGION Images }

function TKntNoteUI.GetImagesInstances: TImageIDs;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntriesUI[p] <> nil then
         CombineImagesInstances(FNEntriesUI[p].ImagesInstances, Result);
end;


procedure TKntNoteUI.ResetImagesReferenceCount;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntriesUI[p] <> nil then
         FNEntriesUI[p].ResetImagesReferenceCount;
end;


procedure TKntNoteUI.ReloadImagesOnEditor;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntriesUI[p] <> nil then
         FNEntriesUI[p].ReloadImagesOnEditor;
end;

procedure TKntNoteUI.ReconsiderImageDimensionGoalsOnEditor(Selection: boolean; ImagesMode: TImagesMode);
begin
   FSelectedNEntriesUI.ReconsiderImageDimensionGoalsOnEditor(Selection, ImagesMode);
end;

procedure TKntNoteUI.SetImagesMode(ImagesMode: TImagesMode);
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if FNEntriesUI[p] <> nil then
         FNEntriesUI[p].SetImagesMode(ImagesMode);
end;


{$ENDREGION}


end.
