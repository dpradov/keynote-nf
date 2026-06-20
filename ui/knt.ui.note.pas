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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  gf_misc,
  gf_miscvcl,
  kn_info,
  kn_Const,
  kn_Global,
  kn_KntFolder,
  kn_LocationObj,
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
    FHideFocusFlag: boolean;

    FUpdatingOnResize: boolean;
    IncResize: integer;
    FChangingLayout: boolean;
    FReturnToQLFromAllEntriesInSingleMode: boolean;          // See comment (*1) in IntroInEditorOfEntriesUI

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
                             LayoutToUse: TBasicNEntriesLayout;
                             EditingNEntry: TNoteEntry = nil;
                             OfferEditorForNewEntry: boolean = False;
                             TagsToAddToNewEntry: TNoteTagArray = nil);
    procedure ReloadFromDataModel;
    procedure ReloadMetadataFromDataModel(ReloadTags: boolean = true);
    procedure SaveToDataModel;
    procedure ReloadNoteName;
    procedure ConfigureEditor;
    procedure SetAsDefaultLayoutInFolder(var NoteAdvOptions: TNoteAdvancedOptions);
    procedure ResetPanelSizes;

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
                                   TagsToAddToNewEntry: TNoteTagArray = nil;
                                   SS: integer=-1; SL: integer=-1);
    procedure IntroInEditorOfEntriesUI(RequestedFromEditor: TKntRichEdit; CtrlDown: boolean);
    procedure EditorChangedInEmptyPanel(Editor: TKntRichEdit);
    procedure UpdateFMultipleVisibleEditors;
    procedure TimerInfoTimer(Sender: TObject);
    function GetHideFocusFlag: boolean;
    procedure SetHideFocusFlag(value: boolean);

 public
    procedure NEntriesUIEditorEnter(Sender: TObject);
    function GetSelectedNEntriesUI (Editor: TKntRichEdit): TObject;
    function GetNEntriesUITargetForJump(LocationObj: TObject): TObject;
    procedure GetPanelConfigOrderForFindSearch(NNode: TNoteNode; NEntry: TNoteEntry; TagsIncl: TNoteTagArray; var DescendingOrder: boolean);
    function GetPanelConfigForFindSelection(NNodeUIConfig: TNNodeUIConfiguration; NEntry: TNoteEntry; TagsIncl: TNoteTagArray = nil): TPanelConfiguration;
    function GetNEntriesUITargetForFindSelection(NEntry: TNoteEntry; TagsIncl: TNoteTagArray = nil): TObject;
    function MultipleVisibleEditors: boolean;
    function NumberOfVisibleEntries(Panel: TNEntriesPanel): integer;
    function NavigatePanels(NavDirection: TNavDirection): boolean;
    procedure ToggleMaximizeSelectedPanel;
    procedure KeepInfoPanelTemporarilyVisible;
    property HideFocusFlag: boolean read GetHideFocusFlag write SetHideFocusFlag;
    procedure SetBGColorInEditors(Color: TColor);
    procedure SetEditorZoom( ZoomValue : integer; const ZoomString : string; Increment: integer= 0);
    procedure RestoreZoomGoal;
    procedure RefreshHeaderOfEntries(OnlyNEntry: TNoteEntry = nil);
    procedure ApplyChangeInPanelCustomiz(MECustomiz: TMEPanelCustomization; ForceApplyFilter: boolean; IgnorePanel: TNEntriesPanel);
    procedure ReconsiderVisibilityOfHiddenPanels;
    procedure ReconsiderVisibilityOfEntries;
    procedure ShowHiddenEntries;
    procedure HideHiddenRevealed;
    procedure ModifiedMetadataOfEntry(NEntry: TNoteEntry);
    property ReturnToQLFromAllEntriesInSingleMode: boolean read FReturnToQLFromAllEntriesInSingleMode write FReturnToQLFromAllEntriesInSingleMode;


   {$IFDEF KNT_DEBUG}
    function GetDBG_NEntriesUI(): TKntNoteEntriesUIArray;
   {$ENDIF}

  protected
    procedure FixPossibleProblemWith0HeigthPanels;
    procedure CancelMaximizedPanel;
  public
    procedure Refresh;
    procedure ShowLeftPanel(value: boolean);
    procedure ShowTopPanels(value: boolean);
    procedure ShowBottomPanels(value: boolean);
    procedure ShowPanelsTop(TL, TR: boolean);
    procedure ShowPanelsBottom(BL, BR: boolean);
    procedure ToggleMaximizeMainPanel (Panel: TNEntriesMainPanel);
    procedure RestoreSplits;
    function GetPanel (Panel: TNEntriesPanel): TPanel;
    procedure ShowEntriesUIPanel(Panel: TNEntriesMainPanel; Show: boolean);
    procedure PanelEmpty(Panel: TNEntriesMainPanel; WithoutVisibleEntries: boolean);
    procedure ReviewInfoBarVisibility;
    property ChangingLayout: boolean read FChangingLayout;
    procedure RefreshPanelsLayout;
    procedure TreeFocused;
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
   FHideFocusFlag:= false;
   FChangingLayout:= false;

   for p := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do
      FNEntriesUI[p]:= nil;

   FNEntriesUI[pnCenter]:= TKntNoteEntriesUI.Create( PnlCenter, Self );
   FNEntriesUI[pnCenter].Parent:= PnlCenter;
   FSelectedNEntriesUI:= FNEntriesUI[pnCenter];

   //TestCreatePanel;

   FUpdatingOnResize:= false;
   TimerInfoPanel:= TTimer.Create(Self);
   TimerInfoPanel.Enabled := false;
   TimerInfoPanel.Interval := 1300;  // 1,3 seconds
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
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
         FNEntriesUI[p].SetOnEnter(AEvent);
end;

procedure TKntNoteUI.SetOnMouseUpOnNote(AEvent: TNotifyEvent);
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
         FNEntriesUI[p].SetOnMouseUpOnNote(AEvent);
end;

procedure TKntNoteUI.SetOnMouseMoveOnNote(AEvent: TNotifyEvent);
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
         FNEntriesUI[p].SetOnMouseMoveOnNote(AEvent);
end;


procedure TKntNoteUI.SetInfoPanelHidden(value: boolean);
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if (FNEntriesUI[p] <> nil) then
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
   end
   else begin
      pnlTL.Visible:= False;
      pnlTR.Visible:= False;
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
  if CtrlDown then begin
     FNNodeUIConfig.BottomAutoFromHidden_Ratio:= Max(0.15, pnlBottom.Height / Self.Height);
     FramResizePendingInNoteUI:= Self;
  end
  else
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

  if pnlTop.Height <= HEIGHT_REDUCED_TO_HIDDEN then begin
     FNNodeUIConfig.Top_Ratio:= RATIO_EQUIV_HIDDEN;
     pnlTop.Height:=  HEIGHT_REDUCED_TO_HIDDEN;
     splT.Top:=       HEIGHT_REDUCED_TO_HIDDEN;
  end
  else
  if CtrlDown then begin
     FNNodeUIConfig.TopAutoFromHidden_Ratio:= Max(0.15, pnlTop.Height / Self.Height);
     FramResizePendingInNoteUI:= Self;
  end
  else
     FNNodeUIConfig.Top_Ratio:= pnlTop.Height / Self.Height;

  if pnlBottom.Visible and FUpdatingOnResize then begin
     pnlCenter.Height:= pnlCenter.Height - IncResize;
     FNEntriesUI[pnCenter].Editor.EndUpdate;
     if FNEntriesUI[pnBL] <> nil then
        FNEntriesUI[pnBL].Editor.EndUpdate;
     if FNEntriesUI[pnBR] <> nil then
        FNEntriesUI[pnBR].Editor.EndUpdate;

     FUpdatingOnResize:= False;
  end;

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
   else begin
      pnlBL.Visible:= False;
      pnlBR.Visible:= False;
      pnlCenter.Align:= alClient;
   end;

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
   if FNNodeUIConfig = nil then exit;

   if pnlBL.Visible and pnlBR.Visible then
      pnlBL.Width:= Round(pnlBottom.Width * FNNodeUIConfig.BLBR_Ratio);
   if pnlTL.Visible and pnlTR.Visible then
      pnlTL.Width:= Round(pnlTop.Width * FNNodeUIConfig.TLTR_Ratio);
   if pnlTop.Visible then begin
      H:= Round(Self.Height * FNNodeUIConfig.Top_Ratio);
      if (H < 4) and (FNNodeUIConfig.MaximizedPanel = pnNone) then
         H:= 4;
      pnlTop.Height:= H;
   end;
   if pnlBottom.Visible then begin
      H:= pnlAuxC3.Height - Round(Self.Height * FNNodeUIConfig.Bottom_Ratio);
      if FNNodeUIConfig.MaximizedPanel = pnNone then
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
  pnl: TNEntriesPanel;
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
       BL:= pnlBL.Visible;
       BR:= pnlBR.Visible;
       if BL and BR then begin
          splBC.Visible:= True;
          splBC.Left:= pnlBL.Width;
       end
       else
         splBC.Visible:= False;
   end;

   FixPossibleProblemWith0HeigthPanels;
end;


{
 If, while a panel is maximized (which forces splits using ratios —e.g., FNNodeUIConfig.BLBR_Ratio— which can be 0),
 we move to another node and then restore it, some editors don't seem to register the change. Even though the height of the panel
 containing them is greater than 0, they continue to display a height of 0. I've verified that assigning them focus reactivates
 them and restores the correct height. We should also check the .Top properties of the information bar controls, as they might be
 displaying negative values for those properties for a similar reason.
 None of this occurs if we use maximize/restore without leaving the node.
}
procedure TKntNoteUI.FixPossibleProblemWith0HeigthPanels;
var
  Pnl: TNEntriesPanel;
  Fixed, TreeWasFocused: boolean;
  NEntriesUI, FocusedNEntriesUI: TKntNoteEntriesUI;
begin
   TreeWasFocused:= ActiveTreeUI.Focused;
   FocusedNEntriesUI:= FSelectedNEntriesUI;

   Fixed:= false;
   for Pnl := Low(TNEntriesPanel) to High(TNEntriesPanel) do begin
       NEntriesUI:= FNEntriesUI[Pnl];
       if (NEntriesUI <> nil) and (GetPanel(Pnl).Height > HEIGHT_REDUCED_TO_HIDDEN) and
           ((NEntriesUI.txtTags.Top < 0) or (NEntriesUI.Editor.Height = 0)) then begin

           NEntriesUI.SetFocusOnEditor;
           NEntriesUI.SetTopIncControlsOfInfoPanel;
           Fixed:= true;
       end;
   end;

   if Fixed then begin
      FSelectedNEntriesUI:= FocusedNEntriesUI;
      FSelectedNEntriesUI.SetFocusOnEditor;
      if TreeWasFocused then
         ActiveFolder.SetFocusOnTree;
   end;
end;


procedure TKntNoteUI.ToggleMaximizeMainPanel (Panel: TNEntriesMainPanel);
var
  pnl: TPanel;
begin
 if not MultipleVisibleEditors then exit;

 LockControl(pnlAuxC, True);
 try

   FHideFocusFlag:= false;
   pnl:= GetPanel(panel);
   if not (pnl.Visible and (FNEntriesUI[Panel] <> nil)) then exit;

   if FNNodeUIConfig.MaximizedPanel = pnNone then begin
      FNNodeUIConfig.MaximizedPanel:= Panel;
      if not ActiveFolder.EditorInfoPanelHidden then
         FSelectedNEntriesUI.PanelConfig.ShowEditorInfoPanel:= True;
   end
   else begin
      FNNodeUIConfig.MaximizedPanel:= pnNone;
      FSelectedNEntriesUI.PanelConfig.ShowEditorInfoPanel:= (FNNodeUIConfig.GetWhereToShowEditorInfoBar = Panel);
   end;

   FSelectedNEntriesUI.PanelConfig.Maximized:= (FNNodeUIConfig.MaximizedPanel <> pnNone);
   FSelectedNEntriesUI.ReconsiderColorInfoPanel;

   splT.Visible:= False;
   splB.Visible:= False;
   splBC.Visible:= False;
   splTC.Visible:= False;


   FrameResize(nil);
   if FNNodeUIConfig.MaximizedPanel = pnNone then
       RestoreSplits;

   FSelectedNEntriesUI.ReconsiderEditorInfoBarVisibility;

 finally
    LockControl(pnlAuxC, False);
    FrameResize(nil);
 end;
end;


procedure TKntNoteUI.CancelMaximizedPanel;
begin
   splT.Visible:= False;
   splB.Visible:= False;
   splBC.Visible:= False;
   splTC.Visible:= False;
   FrameResize(nil);
   RestoreSplits;
end;

procedure TKntNoteUI.ShowEntriesUIPanel(Panel: TNEntriesMainPanel; Show: boolean);
var
  NEntriesUI: TKntNoteEntriesUI;
  TL, TR: boolean;
  BL, BR: boolean;
  Pnl: TPanel;

begin
   if Panel = pnCenter then exit;

   NEntriesUI:= GetNEntriesUI(Panel);
   Pnl:= GetPanel(Panel);

   NEntriesUI.PanelHidden:= not Show;
   UpdateFMultipleVisibleEditors;

   if Panel in [pnTL, pnTR] then begin
      TL:= pnlTL.Visible;
      TR:= pnlTR.Visible;
      if Pnl = pnlTL then
         TL:= Show
      else
         TR:= Show;
      ShowPanelsTop(TL, TR);
   end
   else
   if Panel in [pnBL, pnBR] then begin
      BL:= pnlBL.Visible;
      BR:= pnlBR.Visible;
      if Pnl = pnlBL then
         BL:= Show
      else
         BR:= Show;
      ShowPanelsBottom(BL, BR);
   end;

   ReviewInfoBarVisibility;
end;


procedure TKntNoteUI.PanelEmpty(Panel: TNEntriesMainPanel; WithoutVisibleEntries: boolean);
var
  NEntriesUI: TKntNoteEntriesUI;
begin
   NEntriesUI:= GetNEntriesUI(Panel);
   if NEntriesUI = nil then exit;

   if FQueryLayout and WithoutVisibleEntries and (NEntriesUI.PanelConfig.Panel <> pnCenter) then begin
      if not NEntriesUI.PanelConfig.MECustomiz.Filter.Enabled then
         ShowEntriesUIPanel(NEntriesUI.PanelConfig.Panel, False);
   end
   else begin
      NEntriesUI.Editor.OnEditorChanged := EditorChangedInEmptyPanel;
      DisableChangedInEmptyPanelAt:= now;
   end;
end;


procedure TKntNoteUI.ReviewInfoBarVisibility;
var
  pnl, PnlWithEditorInfoPanel: TNEntriesMainPanel;

begin
   if not FChangingLayout and FQueryLayout then begin
      UpdateFMultipleVisibleEditors;
      PnlWithEditorInfoPanel:= FNNodeUIConfig.GetWhereToShowEditorInfoBar;

      if FNEntriesUI[PnlWithEditorInfoPanel].PanelConfig.ShowEditorInfoPanel then exit;

      for Pnl := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do
          if (FNEntriesUI[Pnl] <> nil) and FNEntriesUI[Pnl].OnUse and (FNEntriesUI[Pnl].PanelConfig <> nil) then begin
             FNEntriesUI[Pnl].PanelConfig.ShowEditorInfoPanel:= (Pnl = PnlWithEditorInfoPanel);
             if not FNEntriesUI[Pnl].PanelConfig.Hidden then
                FNEntriesUI[Pnl].ReconsiderEditorInfoBarVisibility
             else
                FNEntriesUI[Pnl].HideTemporarilyEditorInfoBar;
          end;
   end;
end;



procedure TKntNoteUI.RefreshPanelsLayout;
begin
   LockControl(pnlAuxC, True);
   try
     FrameResize(nil);
     FSelectedNEntriesUI.ReconsiderEditorInfoBarVisibility;

   finally
     LockControl(pnlAuxC, false);
   end;

end;

procedure TKntNoteUI.TreeFocused;
begin
   FNNodeUIConfig.FocusedPanel:= pnNone;
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
      SetUpEditor(FNEntriesUI[Panel].Editor, Folder.ZoomGoal);
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


function TKntNoteUI.GetNEntriesUITargetForJump(LocationObj: TObject): TObject;
var
  CheckOnlySingleEntry, CheckOnlyEntrySelected: boolean;
  Content: TContentInMultiEntryMode;
  MainEntriesUI, MaximizedEntriesUI: TKntNoteEntriesUI;
  NEntry: TNoteEntry;
  Location: TLocation;

  function CheckNEntriesUI(NEntriesUI: TKntNoteEntriesUI): boolean;
  begin
      Result:= false;
      if CheckOnlySingleEntry then
         Result:= (NEntriesUI.PanelConfig.CurrentMode = meSingleEntry) and (not CheckOnlyEntrySelected or (NEntriesUI.NEntry = NEntry))
      else
         if CheckOnlyEntrySelected then
            Result:= (NEntriesUI.NEntry = NEntry)
         else
            Result:= (NEntriesUI.IsDisplayingEntry(NEntry, Content));
  end;

  function FindNEntriesUI: TKntNoteEntriesUI;
  var
    CheckTagsVinc, CheckMain: boolean;

    function FindNEntriesUIAux: TKntNoteEntriesUI;
    var
      p : TNEntriesPanel;
      NEntrUI: TKntNoteEntriesUI;
    begin
        Result:= nil;
        for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do begin
           NEntrUI:= FNEntriesUI[p];
           if NEntrUI = nil then continue;
           if not NEntrUI.OnUse then continue;
           if CheckTagsVinc and
                      ( (NEntrUI.PanelConfig.LinkedTags = nil) or
                         not NEntry.HasTags(NEntrUI.PanelConfig.LinkedTags) ) then continue;
           if CheckMain and (NEntrUI <> MainEntriesUI) then continue;
           if CheckNEntriesUI(NEntrUI) then
              exit (NEntrUI);
        end;
    end;

  begin
     Result:= nil;

     CheckTagsVinc:= False;
     CheckMain:= False;

     if CheckOnlyEntrySelected then
        Result:= FindNEntriesUIAux

     else begin
        CheckTagsVinc:= True;
        Result:= FindNEntriesUIAux;
        if Result = nil then begin
           CheckTagsVinc:= False;
           CheckMain:= True;
           Result:= FindNEntriesUIAux;
           if Result = nil then begin
              //CheckTagsVinc:= False;
              CheckMain:= False;
              Result:= FindNEntriesUIAux;
           end;
        end;
     end;
  end;

begin                                                                           // ** TODO: Inmersive Mode

 {
  Search order:
  - Maximized panel (including the searched entry)
  - Single Entry with searched entry
  - Multi-Entry (including the searched entry)
    - Panel with that entry selected
    - Panel linked to Tags
    - Main panel
    - Other
  - Single Entry including the searched entry
    - Panel linked to Tags
    - Main panel
    - Other
  }

   Location:= TLocation(LocationObj);
   NEntry:= Location.NEntry;

   CheckOnlyEntrySelected:= false;
   CheckOnlySingleEntry:= false;

   if FNNodeUIConfig.MaximizedPanel <> pnNone then begin
      MaximizedEntriesUI:= GetNEntriesUI(FNNodeUIConfig.MaximizedPanel);
      if CheckNEntriesUI(MaximizedEntriesUI) then begin
         Result:= MaximizedEntriesUI;
         exit;
      end;
   end;


   CheckOnlyEntrySelected:= true;
   //CheckOnlySingleEntry:= false;

   if (Location.NEntriesUIObj <> nil) and CheckNEntriesUI(TKntNoteEntriesUI(Location.NEntriesUIObj)) then
      Result:= TKntNoteEntriesUI(Location.NEntriesUIObj)

   else begin
      //CheckOnlyEntrySelected:= true;
      CheckOnlySingleEntry:= true;
      Result:= FindNEntriesUI;

      if Result = nil then begin
         CheckOnlySingleEntry:= false;
         //CheckOnlyEntrySelected:= true;
         Result:= FindNEntriesUI;

         if Result = nil then begin
            MainEntriesUI:= GetNEntriesUI(FNNodeUIConfig.GetMainPanel);

            //CheckOnlySingleEntry:= false;
            CheckOnlyEntrySelected:= false;
            Result:= FindNEntriesUI;

            if Result = nil then begin
               CheckOnlySingleEntry:= true;
               //CheckOnlyEntrySelected:= false;
               Result:= FindNEntriesUI;
            end;
        end;
     end;
   end;

   if (Result <> nil) and (FNNodeUIConfig.MaximizedPanel <> pnNone) then
      ToggleMaximizeSelectedPanel;

end;


function TKntNoteUI.GetPanelConfigForFindSelection(NNodeUIConfig: TNNodeUIConfiguration; NEntry: TNoteEntry; TagsIncl: TNoteTagArray = nil): TPanelConfiguration;

  function FindPanelConfigVinculatedToTags: TPanelConfiguration;
  var
    i: integer;
    PanelConfig: TPanelConfiguration;
  begin
      Result:= nil;

      for i := 0 to High(NNodeUIConfig.PanelsConfig) do begin
          PanelConfig:= NNodeUIConfig.PanelsConfig[i];
          if PanelConfig.Hidden then continue;
          if TNoteTagArrayUtils.HasTags(PanelConfig.LinkedTags, TagsIncl) then
             exit (PanelConfig);
      end;
  end;

begin

   if (FSelectedNEntriesUI.NEntry = NEntry) and (FSelectedNEntriesUI.PanelConfig.CurrentMode = meSingleEntry) then
      Result:= FSelectedNEntriesUI.PanelConfig

   else
   if TagsIncl <> nil then
      Result:= FindPanelConfigVinculatedToTags

   else
      Result:= NNodeUIConfig.PanelConfig(NNodeUIConfig.GetMainPanel);
end;


function TKntNoteUI.GetNEntriesUITargetForFindSelection(NEntry: TNoteEntry; TagsIncl: TNoteTagArray = nil): TObject;
begin
   Result:= GetNEntriesUI(GetPanelConfigForFindSelection(FNNodeUIConfig, NEntry, TagsIncl).Panel);
end;


procedure TKntNoteUI.GetPanelConfigOrderForFindSearch(NNode: TNoteNode; NEntry: TNoteEntry; TagsIncl: TNoteTagArray; var DescendingOrder: boolean);
var
   QueryLayout: boolean;
   NNodeUIConfig: TNNodeUIConfiguration;
   PanelConfig: TPanelConfiguration;
begin
   if NNode = nil then exit;

   QueryLayout:= not ActiveFile.GetNoteIsOnEditingLayout(NNode.Note);

   NNodeUIConfig:= Folder.GetNNodeUIConfig(NNode, QueryLayout);     // Get current layout
   if NNodeUIConfig <> nil then begin
      PanelConfig:= GetPanelConfigForFindSelection(NNodeUIConfig, NEntry, TagsIncl);
      DescendingOrder:= PanelConfig.MECustomiz.DescendingOrder;
   end
   else
      TNNodeUIConfiguration.GetDefaultPanelOrder(NNode, Folder, DescendingOrder);
end;


function TKntNoteUI.MultipleVisibleEditors: boolean;
begin
   Result:= FMultipleVisibleEditors;
end;


function TKntNoteUI.NumberOfVisibleEntries(Panel: TNEntriesPanel): integer;
var
  pnl: TPanel;
begin
   pnl:= GetPanel(panel);
   Result:= 0;
   if FNEntriesUI[Panel] =  nil then exit;

   Result:= FNEntriesUI[Panel].NumberOfIncludedEntries(True);
end;


procedure TKntNoteUI.UpdateFMultipleVisibleEditors;
var
  p: TNEntriesPanel;
  i: integer;
begin
   FMultipleVisibleEditors:= false;
   i:= 0;
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if (FNEntriesUI[p] <> nil) and FNEntriesUI[p].OnUse and not FNEntriesUI[p].PanelHidden then begin
         inc(i);
         if i > 1 then begin
            FMultipleVisibleEditors:= True;
            exit;
         end;
      end;
end;


function TKntNoteUI.NavigatePanels(NavDirection: TNavDirection): boolean;
var
  pnl, nextPnl: TNEntriesMainPanel;
  panelConfig: TPanelConfiguration;

begin
  Result:= false;
  if not FMultipleVisibleEditors or (FNNodeUIConfig.MaximizedPanel <> pnNone) then exit;
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
  if (panelConfig <> nil) and not panelConfig.Hidden then begin
     FNEntriesUI[nextPnl].SetFocusOnEditor;
     Result:= True;
  end;
end;


procedure TKntNoteUI.ToggleMaximizeSelectedPanel;
begin
   ToggleMaximizeMainPanel(FSelectedNEntriesUI.PanelConfig.Panel);
end;


procedure TKntNoteUI.SetBGColorInEditors(Color: TColor);
var
  p: TNEntriesPanel;
begin
   LockControl(pnlAuxC, True);
   try
      for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
         if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then begin
            FNEntriesUI[p].Editor.Color:= Color;
            FNEntriesUI[p].RefreshHeaderOfEntries;
         end;

   finally
      LockControl(pnlAuxC, False);
   end;
end;

procedure TKntNoteUI.SetEditorZoom( ZoomValue : integer; const ZoomString : string; Increment: integer= 0);
var
  p: TNEntriesPanel;
begin
  for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
     if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
        FNEntriesUI[p].Editor.SetZoom(ZoomValue, ZoomString, Increment);
end;


procedure TKntNoteUI.RestoreZoomGoal;
var
  p: TNEntriesPanel;
begin
  for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
     if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
        FNEntriesUI[p].Editor.RestoreZoomGoal;
end;


procedure TKntNoteUI.RefreshHeaderOfEntries(OnlyNEntry: TNoteEntry = nil);
var
  p: TNEntriesPanel;
begin
  for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
     if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
        FNEntriesUI[p].RefreshHeaderOfEntries(OnlyNEntry);
end;


procedure TKntNoteUI.ApplyChangeInPanelCustomiz(MECustomiz: TMEPanelCustomization; ForceApplyFilter: boolean; IgnorePanel: TNEntriesPanel);
var
  p: TNEntriesPanel;
begin
  for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
     if (p <> IgnorePanel) and (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
        FNEntriesUI[p].ApplyChangeInPanelCustomiz(MECustomiz, ForceApplyFilter);
end;


procedure TKntNoteUI.ReconsiderVisibilityOfEntries;
var
  p: TNEntriesPanel;
begin
  for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
     if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
        FNEntriesUI[p].ReloadFromDataModel(false, nil, aChangedVisibility);
end;

procedure TKntNoteUI.ReconsiderVisibilityOfHiddenPanels;
var
  pnl: TNEntriesMainPanel;
begin
   if not FChangingLayout and FQueryLayout then begin
      for Pnl := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do
          if (FNEntriesUI[Pnl] <> nil) and FNEntriesUI[Pnl].OnUse and FNEntriesUI[Pnl].PanelHidden then
              FNEntriesUI[Pnl].ReloadFromDataModel(false);
   end;
end;

procedure TKntNoteUI.ShowHiddenEntries;
var
  p: TNEntriesPanel;
begin
  // (Ctrl: Show and undo hidden)
  // Shift: Only not hidden (Alt+DblClick)
  for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
     if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
        FNEntriesUI[p].ShowHiddenEntries;

  ReconsiderVisibilityOfHiddenPanels;
end;

procedure TKntNoteUI.HideHiddenRevealed;
var
  p: TNEntriesPanel;
begin
  for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
     if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
        FNEntriesUI[p].HideHiddenRevealed;
end;


procedure TKntNoteUI.ModifiedMetadataOfEntry(NEntry: TNoteEntry);
var
  p: TNEntriesPanel;
  NoteEntriesUI: TKntNoteEntriesUI;
  N: integer;
begin
  N:= 0;

  LockControl(pnlAuxC, True);
  try
     for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
        if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then begin
           FNEntriesUI[p].ModifiedMetadataOfEntry(NEntry);
           if FQueryLayout then begin
              FNEntriesUI[p].PanelConfig.ShowEditorInfoPanel:= false;
              FNEntriesUI[p].ReconsiderEditorInfoBarVisibility;
           end;
           inc(N);
        end;

     if N = 0 then exit;

     if FQueryLayout then begin
        p:= FNNodeUIConfig.GetWhereToShowEditorInfoBar;
        FNNodeUIConfig.PanelConfig(p).ShowEditorInfoPanel:= True;
        NoteEntriesUI:= GetNEntriesUI(p);
        if NoteEntriesUI.PanelConfig = nil then exit;
        NoteEntriesUI.ReconsiderEditorInfoBarVisibility;
     end;

  finally
     LockControl(pnlAuxC, false);
  end;
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
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then begin
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
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
         FNEntriesUI[p].RefreshEntry;
end;


// Only Ctrl+INTRO are intercepted
// or, also, INTRO in editors where Editor.MultiEntry = True (=> NEntriesUI.CurrentMode = meMultiEntry)

{
Behavior of Ctrl+Enter
Ctrl+Enter: Outside of its objective mode (MainMode)?
     Yes: Return to that mode, keeping the layout -> ToggleMulti
     No: Switch to the other layout.
Examples:
- In QL: in "All entries" panel (MainMode=MultiEntry), multi-entry mode => switch to EditingLayout (EL)
- In QL: in "All entries" panel, single-entry mode => stay in QueryLayout (QL) and return to multi-entry mode
- In QL: in "Linked to tags" panel (MainMode=MultiEntry), multi-entry mode => switch to EL
- In QL: in "Selected entry" panel (MainMode=SingleEntry), single-entry mode => switch to EL
- In QL: in "Selected entry" panel, multi-entry mode  => stay in QL and return to single-entry mode
- Idem with EL


(*1) Exception to that rule:
   If in EL there is no "Selected entry" panel and we are in the "All Entries" panel (MainMode=MultiEntry and not vinculated to tags)
   displaying a single entry, as a consequence of a switch from QL => Ctrl+Enter will switch back to QL.
   If in that situation (no "Selected entry" panel), in EL, while we are in the "All Entries" panel we return to multi-entry mode
   using the button in info panel (button with the number of the current entry) and then edit any entry using ENTER, when we then
   press Ctrl+Enter with "All Entries" panel displaying a single entry, the behaviour will reset to default: the panel will return
   to multi-entry mode.

   This behavior aims to make it more intuitive the inmediate return to QL from EL: select an entry from QL, press Ctrl+Enter ->
   switch to EL and edit that entry in that layout, on its "All entries" panel in single-entry mode; Ctrl+Enter -> switch back to QL
   (the same observed behaviour, with that sequence, if in EL there was a "Selected entry" panel)
   However, if once we switched to EL from QL we have moved the "All Entries" panel to multi-entry mode (using the button),
   it seems more intuitive to prioritize the default behavior, that is, returning to what it is more recent: the panel in
   multi-entry mode, which is the target mode, MainMode.


Switching from EL to QL or vice versa
- If an entry is selected in EL from an "All entries" panel or from the "Selected entry" panel, upon returning to QL,
  that same entry will be selected. If there is a "Selected entry" panel in QL, it will be selected there; otherwise,
  it will be selected in the "All entries" panel.
  Regardless of this possible change in the selected entry, upon returning, the focus will remain on the panel where it
  was previously selected.

- When switching to EL from QL, the focus is always placed on the "Selected entry" panel, if present, and otherwise on
  the "All entries" panel. In both cases: the panel that allows viewing/editing the entry.
}

procedure TKntNoteUI.IntroInEditorOfEntriesUI(RequestedFromEditor: TKntRichEdit; CtrlDown: boolean);
var
   NEntriesUI: TKntNoteEntriesUI;
   NEntry: TNoteEntry;
   SS, SL: integer;
   ToQueryLayout, ToEditingLayout: boolean;
   PnlEdit: TNEntriesMainPanel;
begin
  if ActiveFile.EncryptedContentMustBeHidden and FNote.IsEncrypted then exit;

  ToQueryLayout:= False;
  ToEditingLayout:= False;

  NEntriesUI:= GetNEntriesUI(RequestedFromEditor);
  NEntry:= NEntriesUI.NEntry;
  FHideFocusFlag:= false;

  if CtrlDown then begin
     if (FQueryLayout and (NEntry = nil) and (NEntriesUI.PanelConfig.CurrentMode <> meMultiEntry)) or
        ((FNote.NumEntries > 1) and FNNodeUIConfig.AnyPanelInQL_ets) then begin

        LoadFromNNode(FNNode, True, neQueryLayout);
        if (NEntriesUI.PanelConfig.EntryModeForUse <> NEntriesUI.PanelConfig.CurrentMode) then
           NEntriesUI.btnToggleMultiClick(nil);
        exit;
     end
     else
     if (not NEntriesUI.PanelConfig.UseIsMultiEntry) and (NEntriesUI.PanelConfig.CurrentMode = meMultiEntry) then begin
        NEntriesUI.btnToggleMultiClick(nil);
        exit;
     end
     else
     if (NEntriesUI.PanelConfig.CurrentMode = meSingleEntry) then begin
        if not FQueryLayout and FReturnToQLFromAllEntriesInSingleMode and (NEntriesUI.PanelConfig.Panel= FNNodeUIConfig.GetMainPanel) and not FNNodeUIConfig.GetSingleEntryPanelForEditing(PnlEdit) then
           // -> ToQueryLayout:= True    (*1)

        else begin
           if (NEntriesUI.PanelConfig.UseIsMultiEntry) then begin   // -> Single <> Multi
              NEntriesUI.btnToggleMultiClick(nil);
              exit;
           end
           else
           if FQueryLayout and (NEntry = nil) then begin
              NEntriesUI.ReloadFromDataModel(false, nil);
              exit;
           end;
        end;

     end;

     if FQueryLayout then
        ToEditingLayout:= True
     else
        ToQueryLayout:= True;
  end
  else begin                                        // Not CtrlDown => CurrentMode= meMultiEntries
     // Edit in single panel (pnuShowSelectedEntry), if found; otherwise -> btnToggleMultiClick
     EditInInMultiEntries(NEntriesUI, NEntry, false);
     exit;
  end;


  // (ToQueryLayout or ToEditingLayout) = True

  if (FNNodeUIConfig.MaximizedPanel <> pnNone) then
      ToggleMaximizeSelectedPanel;

  if ToEditingLayout then begin
     NEntriesUI.SavePositionInPanel;
     SS:= NEntriesUI.PanelConfig.SelStart;
     SL:= NEntriesUI.PanelConfig.SelLength;
     LoadFromNNode(FNNode, True, neEditingLayout, NEntry);
     EditInInMultiEntries(nil, NEntry, false, nil, SS, SL);       // ReqFromNEntriesUI=Nil: We don't know if the same panel is configured in EL
  end
  else
  if ToQueryLayout then begin
     ActiveFile.SetNoteIsOnEditingLayout(FNote, false);
     LoadFromNNode(FNNode, True, neQueryLayout);
  end;

end;


procedure TKntNoteUI.EditorChangedInEmptyPanel(Editor: TKntRichEdit);
var
  p: TNEntriesPanel;
  NEntriesUI: TKntNoteEntriesUI;

begin
   if DisableChangedInEmptyPanelAt <> 0 then exit;

   Editor.OnEditorChanged:= nil;

   NEntriesUI:= GetNEntriesUI(Editor);
   if (NEntriesUI = nil) or (NEntriesUI.NEntry <> nil) then exit;       // For safety...


   // This handler is configured only to 'listen' for editor changes in 'empty' panels, without any entry set yet.
   CreateNewEntry(NEntriesUI);
   NEntriesUI.TagsToUseOnNewEntry:= nil;
end;


procedure TKntNoteUI.NewEntryRequested(ReqFromEditor: TKntRichEdit);
var
  ReqFromNEntriesUI: TKntNoteEntriesUI;
  TagsToAddToNewEntry: TNoteTagArray;
  PnlEdit: TNEntriesMainPanel;
begin
   if ActiveFile.EncryptedContentMustBeHidden and FNote.IsEncrypted then exit;

   ReqFromNEntriesUI:= GetNEntriesUI(ReqFromEditor);
   { LoadFromNNode(.., True, neEditingLayout) if FQueryLayout:
     The Query mode does allow modifications from the visible panels, but only while mode = meSingleEntry.
     It's simply a different configuration/layout, designed for viewing notes as we navigate through the tree.
     This mode is typically configured to offer fewer panels, or only when there is data to display.
     For example, if the note has only one entry, normally only one panel will be shown. }

   TagsToAddToNewEntry:= ReqFromNEntriesUI.PanelConfig.LinkedTags;


   DisableChangedInEmptyPanelAt:= Now;              // Will be enabled in TForm_Main.ApplicationEventsIdle

   if FQueryLayout and (FNote.NumEntries = 1) then begin
      if FNNodeUIConfig.GetSingleEntryPanelForEditing(PnlEdit) and (PnlEdit <> FNNodeUIConfig.GetMainPanel) then begin
         LoadFromNNode(FNNode, True, neQueryLayout, nil, true);
         exit;
      end;
   end;

   if FQueryLayout and Folder.NoteAdvOptions.NewEntriesAlwaysOnEdLayout then begin
      if (FNNodeUIConfig.MaximizedPanel <> pnNone) then
          ToggleMaximizeSelectedPanel;
      LoadFromNNode(FNNode, True, neEditingLayout, nil, true, TagsToAddToNewEntry)
   end
   else
      EditInInMultiEntries(ReqFromNEntriesUI, nil, true, TagsToAddToNewEntry);

   FSelectedNEntriesUI.Editor.OnEditorChanged := EditorChangedInEmptyPanel;
   DisableChangedInEmptyPanelAt:= now;
   sleep(50);
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

   CreatingNewEntry:= True;
   try
     NewNEntry:= Note.AddNewEntry;
     Folder.Modified:= True;

     if ReqFromNEntriesUI.TagsToUseOnNewEntry <> nil then
        NewNEntry.Tags:= ReqFromNEntriesUI.TagsToUseOnNewEntry
     else
     if ReqFromNEntriesUI.PanelConfig.LinkedTags <> nil then
        NewNEntry.Tags:= ReqFromNEntriesUI.PanelConfig.LinkedTags;

     if ReqFromNEntriesUI.NEntry = nil then begin
        // Add new entry in panel (the user has just started making changes in the empty editor of the associated panel)
        ReqFromNEntriesUI.PanelConfig.SelNEntry:= NewNEntry;
        ReqFromNEntriesUI.NEntry:= NewNEntry;
        ReqFromNEntriesUI.ReloadMetadataFromDataModel;
        ReqFromNEntriesUI.ConfigureEditor;
     end;

     // Inform the panels that a new entry has been added. Those panels where it fits will include it, initially only showing the header
     for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do begin
        if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
           FNEntriesUI[p].ReloadFromDataModel(false, NewNEntry, aCreated);
     end;

   finally
     CreatingNewEntry:= false;
   end;
end;


procedure TKntNoteUI.EditInInMultiEntries(ReqFromNEntriesUI: TKntNoteEntriesUI; NEntry: TNoteEntry; NewEntry: boolean;
                                          TagsToAddToNewEntry: TNoteTagArray = nil;
                                          SS: integer=-1; SL: integer=-1);
var
  NEntriesUI: TKntNoteEntriesUI;
  PanelConfig: TPanelConfiguration;
  PnlReq, PnlEdit: TNEntriesMainPanel;
  Action: TActionOnEntry;
  DefinedSingleEntryPanelForEditing: boolean;
  InitialReqWasNil: boolean;

begin
   if (Note = nil) then exit;

   DefinedSingleEntryPanelForEditing:= FNNodeUIConfig.GetSingleEntryPanelForEditing(PnlEdit);
   InitialReqWasNil:= (ReqFromNEntriesUI = nil);

   if not InitialReqWasNil then
      PnlReq:= ReqFromNEntriesUI.PanelConfig.Panel
   else begin
      PnlReq:= FNNodeUIConfig.GetMainPanel;               // "Main" panel:  "All entries" panel
      ReqFromNEntriesUI:= GetNEntriesUI(PnlReq);
   end;


   if FNNodeUIConfig.MaximizedPanel = PnlReq then
      PnlEdit:= PnlReq
   else
   if not DefinedSingleEntryPanelForEditing or (not Folder.NoteAdvOptions.EditTagLinkedEntryInSelectedEntryPanel) then begin
      PnlEdit:= PnlReq;
      if not NewEntry and not InitialReqWasNil then begin
         ReqFromNEntriesUI.btnToggleMultiClick(nil);       // Use requested NEntriesUI for editing
         exit;
      end;
   end;
   NEntriesUI:= GetNEntriesUI(PnlEdit);

   NEntriesUI.SaveToDataModel();

   PanelConfig:= NEntriesUI.PanelConfig;
   PanelConfig.SelNEntry:= NEntry;

   if NEntriesUI <> ReqFromNEntriesUI then begin
      ReqFromNEntriesUI.SavePositionInPanel;
      PanelConfig.SelStart:= ReqFromNEntriesUI.PanelConfig.SelStart;
      PanelConfig.SelLength:= ReqFromNEntriesUI.PanelConfig.SelLength;
      if NEntriesUI.PanelHidden and not FMultipleVisibleEditors and (ReqFromNEntriesUI.PanelConfig.CurrentMode = meSingleEntry) then
         ReqFromNEntriesUI.btnToggleMultiClick(nil);
   end;

   if SS >= 0 then begin
      PanelConfig.SelStart:= SS;
      PanelConfig.SelLength:= SL;
   end;

   Action:= aNull;
   if NewEntry then begin
      Action:= aCreating;
      NEntriesUI.TagsToUseOnNewEntry:= TagsToAddToNewEntry;
   end;

   NEntriesUI.Editor.HideNestedFloatingEditor;
   NEntriesUI.PanelConfig.CurrentMode:= meSingleEntry;
   NEntriesUI.ReloadFromDataModel(True, nil, Action, true);

   FNNodeDeleted:= false;
   NEntriesUI.SetFocusOnEditor;
   NEntriesUI.ReconsiderEditorInfoBarVisibility;
end;


procedure TKntNoteUI.NEntriesUIEditorEnter(Sender: TObject);
var
  p, FocPanel: TNEntriesPanel;
begin
   FHideFocusFlag:= false;

   if FSelectedNEntriesUI <> nil then
     FSelectedNEntriesUI.cFocusedFlag.Color:= clBtnFace;

   if not FloatingEditorCannotBeSaved then
      for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
        if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then begin
           FNEntriesUI[p].Editor.NavigatePanelsEnabled:= True;
           if not FNEntriesUI[p].HideNestedFloatingEditor then
              exit;
        end;

  FSelectedNEntriesUI:= TKntNoteEntriesUI(Sender);
  FSelectedNEntriesUI.cFocusedFlag.Color:= clSkyBlue;
  FocPanel:= FSelectedNEntriesUI.PanelConfig.Panel;
  FNNodeUIConfig.FocusedPanel:= FocPanel;

  if (Folder.NoteAdvOptions.AutoExpandInPanels) or
     ((FocPanel in [pnCenter, pnTL,pnTR]) and
         ( ((FNNodeUIConfig.PanelReducedToHidden(pnTL)) and (PnlTL.Visible or PnlTR.Visible)) or
           ((PnlTL.Visible and (NumberOfVisibleEntries(pnTL) = 0)) or ((PnlTR.Visible and (NumberOfVisibleEntries(pnTR)=0)))) ) ) or
     ((FocPanel in [pnCenter, pnBL,pnBR]) and
         ( ((FNNodeUIConfig.PanelReducedToHidden(pnBL)) and (PnlBL.Visible or PnlBR.Visible)) or
           ((PnlBL.Visible and (NumberOfVisibleEntries(pnBL) = 0)) or ((PnlBR.Visible and (NumberOfVisibleEntries(pnBR)=0)))) ) )  then
     FramResizePendingInNoteUI:= Self;

  TimerInfoPanel.Enabled:= False;
  TimerInfoPanel.Enabled:= True;
end;

function TKntNoteUI.GetHideFocusFlag: boolean;
begin
   Result:= FHideFocusFlag;
end;

procedure TKntNoteUI.SetHideFocusFlag(value: boolean);
begin
   FHideFocusFlag:= value;
   KeepInfoPanelTemporarilyVisible;
end;


procedure TKntNoteUI.TimerInfoTimer(Sender: TObject);
var
  p: TNEntriesPanel;
  KeepEnabled: boolean;
begin
   KeepEnabled:= False;

   if Sender <> nil then
      FHideFocusFlag:= true;

   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do begin
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) and (FNEntriesUI[p].PanelConfig <> nil) then begin
         FNEntriesUI[p].cFocusedFlag.Refresh;
         if (Folder.EditorInfoPanelHidden or not FNEntriesUI[p].PanelConfig.ShowEditorInfoPanel) and not FNEntriesUI[p].HideTemporarilyEditorInfoBar then
            KeepEnabled:= True;
      end;
   end;

   if not KeepEnabled and (Sender <> nil) then
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
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
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
                                   LayoutToUse: TBasicNEntriesLayout;
                                   EditingNEntry: TNoteEntry = nil;
                                   OfferEditorForNewEntry: boolean = False;
                                   TagsToAddToNewEntry: TNoteTagArray = nil);
var
   ShowPanels: boolean;
   Pnl, PnlEdit, PnlToSetFocus, MainPanel, PnlWithEditorInfoPanel: TNEntriesMainPanel;
   i: integer;
   PanelConfig: TPanelConfiguration;
   ShowPanel: array[TNEntriesMainPanel] of boolean;
   NEntriesUI: TKntNoteEntriesUI;
   QueryLayout: boolean;
   DefinedSingleEntryPanelForEditing: boolean;
   SetNoteSelEntry: boolean;
   EnableNavigatePanels: boolean;
   Action: TActionOnEntry;
   CancelMaximizedPanelNeeded: boolean;
begin
 EnableNavigatePanels:= (LayoutToUse <> neLastLayout);

 if SavePreviousContent and (FNNode <> nil) and not FNNodeDeleted then
    SaveToDataModel;

 FHideFocusFlag:= false;
 if FloatingEditorCannotBeSaved then exit;

 LockControl(pnlAuxC, True);
 FChangingLayout:= True;
 FReturnToQLFromAllEntriesInSingleMode:= True;
 try
   CancelMaximizedPanelNeeded:= (FNNodeUIConfig <> nil) and (FNNodeUIConfig.MaximizedPanel <> pnNone);


   // When switching from EditingLayout to QueryLayout -> Set the NEntry of the current panel to the one selected in the main panel
   // This will have been saved in FNote.SelEntry from TKntNoteUI.SaveToDataModel
   SetNoteSelEntry:= (LayoutToUse = neQueryLayout) and not FQueryLayout;

   FNNode:= NNode;
   FNNodeUIConfig:= nil;
   QueryLayout:= True;

   if assigned(NNode) then begin
     FNote:= NNode.Note;
     if LayoutToUse = neLastLayout then
        QueryLayout:= not ActiveFile.GetNoteIsOnEditingLayout(FNote)
     else
        QueryLayout:= (LayoutToUse = neQueryLayout);

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

   if CancelMaximizedPanelNeeded then
      CancelMaximizedPanel;


   for Pnl := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do begin
      ShowPanel[Pnl]:= false;
      if (FNEntriesUI[Pnl] <> nil) and not FNEntriesUI[Pnl].HideNestedFloatingEditor then
         exit;
   end;

   if assigned(NNode) then begin
      MainPanel:= pnCenter;
      if not (FQueryLayout and not OfferEditorForNewEntry and (FNote.NumEntries = 1)) then
         MainPanel:= FNNodeUIConfig.GetMainPanel;

      PnlToSetFocus:= MainPanel;
      DefinedSingleEntryPanelForEditing:= FNNodeUIConfig.GetSingleEntryPanelForEditing(PnlEdit);

      if OfferEditorForNewEntry then begin
         if DefinedSingleEntryPanelForEditing then
            PnlToSetFocus:= PnlEdit;
      end
      else begin
         if FNNodeUIConfig.FocusedPanel <> pnNone then
            PnlToSetFocus:= FNNodeUIConfig.FocusedPanel;
         if FNNodeUIConfig.MaximizedPanel <> pnNone then
            PnlToSetFocus:= FNNodeUIConfig.MaximizedPanel;

         if ((PnlToSetFocus in [pnTL, pnTR]) and FNNodeUIConfig.PanelReducedToHidden(pnTL)) or
            ((PnlToSetFocus in [pnBL, pnBR]) and FNNodeUIConfig.PanelReducedToHidden(pnBL))    then
            PnlToSetFocus:= pnCenter;
         FNNodeUIConfig.FocusedPanel:= PnlToSetFocus;
      end;

      for i := 0 to High(FNNodeUIConfig.PanelsConfig) do begin
          PanelConfig:= FNNodeUIConfig.PanelsConfig[i];
          Pnl:= PanelConfig.Panel;
          ShowPanel[Pnl]:= True;
          PanelConfig.Hidden:= false;               // By now
          PanelConfig.ShowEditorInfoPanel:= False;  //  ,,

          NEntriesUI:= GetNEntriesUI(Pnl);

          if SetNoteSelEntry then begin
             if (Pnl = PnlEdit) or (not DefinedSingleEntryPanelForEditing and (Pnl = MainPanel)) then begin
                PanelConfig.SelNEntry:= FNote.SelEntry;
                PanelConfig.SelStart:= FNote.SelStart;
                PanelConfig.SelLength:= FNote.SelLength;
                PanelConfig.ScrollPosInEditor.Y:= 0;
             end;
          end;

          Action:= aNull;
          if OfferEditorForNewEntry then begin
             Action:= aCreatingFromOtherPanel;
             if (Pnl = PnlToSetFocus) then begin
                PanelConfig.SelNEntry:= nil;
                PanelConfig.CurrentMode:= meSingleEntry;
                Action:= aCreating;
                NEntriesUI.TagsToUseOnNewEntry:= TagsToAddToNewEntry;
             end;
          end;

          NEntriesUI.LoadFromDataModel(PanelConfig, False, (Pnl = PnlToSetFocus), Action);

          if (NEntriesUI.NEntry = nil) then begin
             NEntriesUI.Editor.OnEditorChanged := EditorChangedInEmptyPanel;
             DisableChangedInEmptyPanelAt:= now;
          end
          else
             NEntriesUI.Editor.OnEditorChanged := nil;
      end;
   end
   else
      GetNEntriesUI(pnCenter).LoadFromDataModel(nil, False);



   for Pnl := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do
       if (FNEntriesUI[Pnl] <> nil) and not ShowPanel[Pnl] then
           FNEntriesUI[Pnl].SetAsUnused;          // Clear unused editors

   UpdateFMultipleVisibleEditors;

   PnlWithEditorInfoPanel:= FNNodeUIConfig.GetWhereToShowEditorInfoBar;

   for Pnl := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do
       if (FNEntriesUI[Pnl] <> nil) and FNEntriesUI[Pnl].OnUse then begin
          { We calculate ShowEditorInfoPanel now because, in QL, as a result of previous calls to NEntriesUI.LoadFromDataModel,
            the Hidden (PanelHidden) state of the panel may have changed, by having (or not having) some visible entry}

          if Pnl = PnlWithEditorInfoPanel then
             FNEntriesUI[Pnl].PanelConfig.ShowEditorInfoPanel:= True;

          if (Pnl <> pnCenter) and QueryLayout and not FNEntriesUI[Pnl].PanelConfig.MECustomiz.Filter.Enabled and
                                not (OfferEditorForNewEntry and (Pnl = PnlToSetFocus)) and (FNEntriesUI[Pnl].NEntry = nil) then
             ShowPanel[Pnl]:= False        // OnUse but not visible for now

          else begin
             FNEntriesUI[Pnl].Editor.NavigatePanelsEnabled:= EnableNavigatePanels;
             if ((Pnl = PnlWithEditorInfoPanel) or EnableNavigatePanels) and not FNNodeUIConfig.PanelReducedToHidden(Pnl) then
                FNEntriesUI[Pnl].ReconsiderEditorInfoBarVisibility
             else
                FNEntriesUI[Pnl].HideTemporarilyEditorInfoBar;
          end;
       end;


   ShowLeftPanel(False);
   ShowPanelsTop(ShowPanel[pnTL], ShowPanel[pnTR]);
   ShowPanelsBottom(ShowPanel[pnBL], ShowPanel[pnBR]);

   if assigned(NNode) then begin
      FixPossibleProblemWith0HeigthPanels;

      if EditingNEntry = nil then begin                       // If <> nil -> Focus in FSelectedNEntriesUI will be set from EditInInMultiEntries
         FSelectedNEntriesUI:= GetNEntriesUI(PnlToSetFocus);
         if not ActiveTreeUI.Focused then
            FSelectedNEntriesUI.SetFocusOnEditor;
         FSelectedNEntriesUI.Editor.NavigatePanelsEnabled:= EnableNavigatePanels;
      end;

      KeepInfoPanelTemporarilyVisible;

      if not QueryLayout then
         ActiveFile.SetNoteIsOnEditingLayout(FNote, True);
   end;

{$IFDEF KNT_DEBUG}
   GetDBG_NEntriesUI;
{$ENDIF}

   FNNodeDeleted:= false;

 finally
    LockControl(pnlAuxC, False);
    FrameResize(nil);
    FChangingLayout:= False;
 end;
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
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
         FNEntriesUI[p].ReloadFromDataModel(true,nil,aNull, (FNEntriesUI[p]=FSelectedNEntriesUI) );
end;

procedure TKntNoteUI.SaveToDataModel;
var
  p: TNEntriesPanel;
  iOnUse: integer;
  SelNEntriesUI: TKntNoteEntriesUI;

begin
   if FNNode = nil then exit;

   Log_StoreTick('TKntNoteUI.SaveToDataModel - BEGIN', 4, +1);

   SetLength(FNNodeUIConfig.PanelsConfig, TNEntriesPanel_Count);

   iOnUse:= 0;
   SelNEntriesUI:= nil;
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then begin
         FNEntriesUI[p].SaveToDataModel;
         if FNEntriesUI[p].OnUse then begin
            FNEntriesUI[p].SavePositionInPanel;
            FNNodeUIConfig.PanelsConfig[iOnUse]:= FNEntriesUI[p].PanelConfig;
            inc(iOnUse);
            if p in MainPanels then begin                              // Main panels: [pnTL..pnBR]
               if FNEntriesUI[p].PanelConfig.LinkedTags = nil then
                  SelNEntriesUI:= FNEntriesUI[p];
            end;
         end
         else
            FreeAndNil(FNEntriesUI[p].PanelConfig);
      end;

   if (FSelectedNEntriesUI <> nil) and (FSelectedNEntriesUI.PanelConfig.Panel in MainPanels) and (FSelectedNEntriesUI.PanelConfig.Use <> pnuShowTagLinkedEntries) then
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
   if FNewNNodeUIConfig then
      if (NNode.Note.NumEntries > 1) or (Editor.ZoomCurrent <> Editor.ZoomGoal) then begin
         Folder.AddNNodeUIConfig(FNNodeUIConfig);
         FNewNNodeUIConfig:= false;
      end
      else begin
         for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
            if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
               FreeAndNil(FNEntriesUI[p].PanelConfig);
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


procedure TKntNoteUI.SetAsDefaultLayoutInFolder(var NoteAdvOptions: TNoteAdvancedOptions);
var
  i: integer;
  NNodeUIConfig: TNNodeUIConfiguration;
  PanelConfig: TPanelConfiguration;

  procedure SaveSizeRatio (PSR: PPanelSizeRatios; SR: TPanelSizeRatios);
  begin
     if SR.Top > 0 then
        PSR.Top:= SR.Top;
     if SR.Bottom > 0 then
        PSR.Bottom:= SR.Bottom;
     if SR.TLTR > 0 then
        PSR.TLTR:= SR.TLTR;
     if SR.BLBR > 0 then
        PSR.BLBR:= SR.BLBR;
  end;

begin
   // Use the layout configuration of this note (size ratios, headers and filters) as the default in this folder


   // Query Layout
   if FQueryLayout then
      NNodeUIConfig:= FNNodeUIConfig

   else begin
      SaveToDataModel;
      NNodeUIConfig:= Folder.GetNNodeUIConfig(NNode, true);
   end;


   for i := 0 to High(NNodeUIConfig.PanelsConfig) do begin
      PanelConfig:= NNodeUIConfig.PanelsConfig[i];
      NoteAdvOptions.DefaultMECustomizForQL[PanelConfig.Panel]:= PanelConfig.MECustomiz;
   end;
   SaveSizeRatio(@NoteAdvOptions.SizeRatiosQL, NNodeUIConfig.InternalSizeRatios);

   // Editing Layout
   NNodeUIConfig:= Folder.GetNNodeUIConfig(NNode, false);

   if NNodeUIConfig <> nil then begin
      for i := 0 to High(NNodeUIConfig.PanelsConfig) do begin
         PanelConfig:= NNodeUIConfig.PanelsConfig[i];
         NoteAdvOptions.DefaultMECustomizForEL[PanelConfig.Panel]:= PanelConfig.MECustomiz;
      end;

      SaveSizeRatio(@NoteAdvOptions.SizeRatiosEL, NNodeUIConfig.InternalSizeRatios);
   end;

end;


procedure TKntNoteUI.ResetPanelSizes;
var
  NNodeUIConfig: TNNodeUIConfiguration;
begin
   // Query Layout
   if FQueryLayout then
      NNodeUIConfig:= FNNodeUIConfig
   else begin
      SaveToDataModel;
      NNodeUIConfig:= Folder.GetNNodeUIConfig(NNode, true);
   end;
   NNodeUIConfig.InternalSizeRatios:= Folder.NoteAdvOptions.SizeRatiosQL;

   // Editing Layout
   NNodeUIConfig:= Folder.GetNNodeUIConfig(NNode, false);
   if NNodeUIConfig <> nil then
      NNodeUIConfig.InternalSizeRatios:= Folder.NoteAdvOptions.SizeRatiosEL;

   FramResizePendingInNoteUI:= Self;
end;


{$ENDREGION}


// Images  =========================================

{$REGION Images }

function TKntNoteUI.GetImagesInstances: TImageIDs;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
         CombineImagesInstances(FNEntriesUI[p].ImagesInstances, Result);
end;


procedure TKntNoteUI.ResetImagesReferenceCount;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
         FNEntriesUI[p].ResetImagesReferenceCount;
end;


procedure TKntNoteUI.ReloadImagesOnEditor;
var
  p: TNEntriesPanel;
begin
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
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
   SaveToDataModel;
   for p := Low(TNEntriesPanel) to High(TNEntriesPanel) do
      if (FNEntriesUI[p] <> nil) and (FNEntriesUI[p].OnUse) then
         FNEntriesUI[p].SetImagesMode(ImagesMode);
end;


{$ENDREGION}


end.
