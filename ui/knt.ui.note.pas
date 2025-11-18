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
    pnlEntries: TPanel;

  private class var
    FColorTxts: TColor;

  private
    FNote: TNote;
    FNNode: TNoteNode;
    FKntFolder: TKntFolder;
    FNNodeDeleted: boolean;

    FNEntriesUI: TKntNoteEntriesUI;

    function GetEditor: TKntRichEdit;
    function GetNNode: TNoteNode;
    function GetFolder: TObject;

  public
    constructor Create(AOwner: TComponent; KntFolder: TKntFolder);
    destructor Destroy; override;

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
 i: integer;
begin
   inherited Create(AOwner);

   FKntFolder:= KntFolder;
   FNEntriesUI:= TKntNoteEntriesUI.Create( pnlEntries, Self );
   FNEntriesUI.Parent:= pnlEntries;
end;


destructor TKntNoteUI.Destroy;
begin
   if assigned(FNEntriesUI) then
      FreeAndNil(FNEntriesUI);

   inherited;
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
begin
   FNEntriesUI.LoadFromNNode(NNode, 0, SavePreviousContent);

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
   FNEntriesUI.SaveToDataModel;
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
