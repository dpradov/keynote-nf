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

  private class var
    FColorTxts: TColor;

  private
    FNote: TNote;
    FNNode: TNoteNode;
    FKntFolder: TKntFolder;
    FNNodeDeleted: boolean;

    FNEntriesUI: TKntNoteEntriesUI;

    FTopOther_Ratio: Single;
    FBottomOther_Ratio: Single;
    FTLTR_Ratio: Single;
    FBLBR_Ratio: Single;

    fSplitterNoteMoving: boolean;
    FUpdatingOnResize: boolean;
    IncResize: integer;

    FEditorL  : TKntRichEdit;    // DEBUG
    FEditorTL : TKntRichEdit;
    FEditorTR : TKntRichEdit;
    FEditorBL : TKntRichEdit;
    FEditorBR : TKntRichEdit;

    function GetEditor: TKntRichEdit;
    function GetNNode: TNoteNode;
    function GetFolder: TObject;

  public
    constructor Create(AOwner: TComponent; KntFolder: TKntFolder);
    destructor Destroy; override;
    procedure TestCreatePanel;

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

public
    procedure ShowLeftPanel(value: boolean);
    procedure ShowTopPanels(value: boolean);
    procedure ShowBottomPanels(value: boolean);
    procedure ShowPanelsTop(TL, TR: boolean);
    procedure ShowPanelsBottom(BL, BR: boolean);
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
begin
   inherited Create(AOwner);

   FKntFolder:= KntFolder;
   FNEntriesUI:= TKntNoteEntriesUI.Create( PnlCenter, Self );
   FNEntriesUI.Parent:= PnlCenter;

   TestCreatePanel;

   fSplitterNoteMoving:= false;
   FUpdatingOnResize:= false;
   FTLTR_Ratio:= 0.5;
   FBLBR_Ratio:= 0.5;
   FTopOther_Ratio:= pnlTop.Height / Self.Height;
   FBottomOther_Ratio:= pnlBottom.Height / Self.Height;
end;


destructor TKntNoteUI.Destroy;
begin
   if assigned(FNEntriesUI) then
      FreeAndNil(FNEntriesUI);

   inherited;
end;


procedure TKntNoteUI.TestCreatePanel;
begin
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

{$ENDREGION}


// Controls. Events

{$REGION Controls. Properties and Events }

function TKntNoteUI.GetEditor: TKntRichEdit;
begin
  Result:= FNEntriesUI.Editor;
end;


function TKntNoteUI.GetReadOnly: boolean;
begin
   Result:= FNEntriesUI.ReadOnly;
end;

procedure TKntNoteUI.SetReadOnly( AReadOnly : boolean );
begin
   FNEntriesUI.ReadOnly:= AReadOnly;
end;


procedure TKntNoteUI.SetOnEnter(AEvent: TNotifyEvent);
begin
  FNEntriesUI.SetOnEnter(AEvent);
end;

procedure TKntNoteUI.SetOnMouseUpOnNote(AEvent: TNotifyEvent);
begin
   FNEntriesUI.SetOnMouseUpOnNote(AEvent);
end;

procedure TKntNoteUI.SetOnMouseMoveOnNote(AEvent: TNotifyEvent);
begin
   FNEntriesUI.SetOnMouseMoveOnNote(AEvent);
end;


procedure TKntNoteUI.SetInfoPanelHidden(value: boolean);
begin
   FNEntriesUI.InfoPanelHidden:= value;
end;


procedure TKntNoteUI.SetFocusOnEditor;
begin
   FNEntriesUI.SetFocus;
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
   pnlBL.Width:= Round(pnlBottom.Width * FBLBR_Ratio);
   pnlTL.Width:= Round(pnlTop.Width * FTLTR_Ratio);
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
      pnlTop.Height:= Round(Self.Height * FTopOther_Ratio);
      splT.Top := pnlTop.Height + 3;
      IncResize:= - IncResize;
   end;

   if pnlBottom.visible then
      pnlCenter.Height:= pnlCenter.Height + IncResize;
end;


procedure TKntNoteUI.splBCMoved(Sender: TObject);
begin
   FBLBR_Ratio:= pnlBL.Width / pnlBottom.Width;
end;

procedure TKntNoteUI.splBMoved(Sender: TObject);
begin
   FBottomOther_Ratio:= pnlBottom.Height / Self.Height;
end;

procedure TKntNoteUI.splTCMoved(Sender: TObject);
begin
   FTLTR_Ratio:= pnlTL.Width / pnlTop.Width;
end;

procedure TKntNoteUI.splTCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
begin
  IncResize:= (NewSize - pnlTop.Height);
  if not FUpdatingOnResize then begin
    FNEntriesUI.Editor.BeginUpdate;
    FEditorBL.BeginUpdate;
    FEditorBR.BeginUpdate;
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

  FTopOther_Ratio:= pnlTop.Height / Self.Height;

  pnlCenter.Height:= pnlCenter.Height - IncResize;
  FNEntriesUI.Editor.EndUpdate;
  FEditorBL.EndUpdate;
  FEditorBR.EndUpdate;
  FUpdatingOnResize:= False;
end;



procedure TKntNoteUI.ShowBottomPanels(value: boolean);
begin
   if pnlBottom.Visible = value then
      exit;

   pnlBottom.Visible:= value;
   if value then begin
      pnlCenter.Align:= alTop;
      pnlCenter.Height:= pnlAuxC3.Height - Round(Self.Height * FBottomOther_Ratio) - 3;
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
         pnlTL.Width:= Round(pnlTop.Width * FTLTR_Ratio);
         splTC.Visible:= True;
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
         pnlBL.Width:= Round(pnlBottom.Width * FBLBR_Ratio);
         splBC.Visible:= True;
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
   pnlBL.Width:= Round(pnlBottom.Width * FBLBR_Ratio);
   pnlTL.Width:= Round(pnlTop.Width * FTLTR_Ratio);
   pnlTop.Height:= Round(Self.Height * FTopOther_Ratio);
   pnlCenter.Height:= pnlAuxC3.Height - Round(Self.Height * FBottomOther_Ratio) - 3;
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

  ShowPanelsBottom(True, True);
  Application.ProcessMessages;
  ShowPanelsTop(True, True);
  Application.ProcessMessages;
end;
}

{$ENDREGION}

// Tags =========================================

{$REGION Tags }

procedure TKntNoteUI.RefreshTags;
begin
   FNEntriesUI.RefreshTags;
end;

procedure TKntNoteUI.EditTags;
begin
   FNEntriesUI.EditTags;
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
   Mode: TModeEntriesUI;
   ShowPanels: boolean;
begin
   Mode:= meSingleEntry;
   if assigned(NNode) and (NNode.Note.NumEntries > 1) then
      Mode:= meMultipleEntries;

   FNEntriesUI.LoadFromNNode(NNode, Mode, 0, SavePreviousContent);
   ShowPanels:= (Mode = meMultipleEntries);

   ShowLeftPanel(False);
   ShowTopPanels(ShowPanels);
   ShowBottomPanels(ShowPanels);

   FNNodeDeleted:= false;
end;

function TKntNoteUI.ReloadMetadataFromDataModel(ReloadTags: boolean = true): TNoteEntry;
begin
   Result:= FNEntriesUI.ReloadMetadataFromDataModel(ReloadTags);
end;

procedure TKntNoteUI.ReloadFromDataModel;
begin
   FNEntriesUI.ReloadFromDataModel;
end;

procedure TKntNoteUI.SaveToDataModel;
begin
   Log_StoreTick('TKntNoteUI.SaveToDataModel - BEGIN', 4, +1);
   
   FNEntriesUI.SaveToDataModel;
   
   Log_StoreTick('TKntNoteUI.SaveToDataModel - END', 4, -1);
end;

procedure TKntNoteUI.ReloadNoteName;
begin
   FNEntriesUI.ReloadNoteName;
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
  FNEntriesUI.ConfigureEditor;
end;


{$ENDREGION}


// Images  =========================================

{$REGION Images }

function TKntNoteUI.GetImagesInstances: TImageIDs;
begin
//##...
   Result:= FNEntriesUI.ImagesInstances;
end;

procedure TKntNoteUI.GetImagesIDInstances (Stream: TMemoryStream; TextPlain: String);
begin
//##...
   FNEntriesUI.GetImagesIDInstances(Stream, TextPlain);
end;


procedure TKntNoteUI.ResetImagesReferenceCount;
begin
    FNEntriesUI.ResetImagesReferenceCount;
end;

procedure TKntNoteUI.ReloadImagesOnEditor;
begin
   FNEntriesUI.ReloadImagesOnEditor;
end;


procedure TKntNoteUI.ReconsiderImageDimensionGoalsOnEditor(Selection: boolean; ImagesMode: TImagesMode);
begin
   FNEntriesUI.ReconsiderImageDimensionGoalsOnEditor(Selection, ImagesMode);
end;


procedure TKntNoteUI.SetImagesMode(ImagesMode: TImagesMode);
begin
   FNEntriesUI.SetImagesMode(ImagesMode);
end;

{$ENDREGION}


end.
