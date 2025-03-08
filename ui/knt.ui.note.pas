unit knt.ui.note;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2024 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
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
  TKntNoteUI = class(TFrame, INoteUI)
    pnlEntries: TPanel;
    pnlIdentif: TPanel;
    txtCreationDate: TEdit;
    txtName: TEdit;
    txtTags: TEdit;
    procedure txtNameChange(Sender: TObject);
    procedure txtEnter(Sender: TObject);
    procedure txtNameMouseEnter(Sender: TObject);
    procedure txtCreationDateMouseEnter(Sender: TObject);
    procedure txtNameExit(Sender: TObject);
    procedure txtTagsEnter(Sender: TObject);

  private class var
    FColorTxts: TColor;

  private
    FNote: TNote;
    FNNode: TNoteNode;
    FKntFolder: TKntFolder;
    FEditor: TKntRichEdit;

    FCanvasAux: TControlCanvas;

    FInfoPanelHidden: boolean;
    FNNodeDeleted: boolean;
    fImagesReferenceCount: TImageIDs;

    fChangingInCode: boolean;

    FOnEnterOnEditor: TNotifyEvent;
    FOnMouseUpOnNote: TNotifyEvent;
    FOnMouseMoveOnNote: TNotifyEvent;
    FOnBeforeEditorLoaded: TBeforeEditorLoadedEvent;
    FOnAfterEditorLoaded: TAfterEditorLoadedEvent;

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
    procedure RefreshTags;
    function  SaveToDataModel: TMemoryStream;
    procedure ReloadNoteName;
    procedure ConfigureEditor;
  protected
    function StreamFormatInNEntry(const NEntry: TNoteEntry): TRichStreamFormat;

  protected
    procedure SetInfoPanelHidden(value: boolean);
    procedure EditTags;
    procedure OnEndTagsIntroduction(PressedReturn: boolean);
  public
    property InfoPanelHidden: boolean read FInfoPanelHidden write SetInfoPanelHidden;

  protected
    function GetReadOnly: boolean;
    procedure SetReadOnly( AReadOnly : boolean );
  public
    property ReadOnly : boolean read GetReadOnly write SetReadOnly;

  protected
    function GetImagesInstances: TImageIDs;
    property ImagesReferenceCount: TImageIDs read fImagesReferenceCount write fImagesReferenceCount;
    property ImagesInstances: TImageIDs read GetImagesInstances;
    procedure GetImagesIDInstances (Stream: TMemoryStream; TextPlain: String);
    procedure ResetImagesReferenceCount;
    procedure ReloadImagesOnEditor;
    procedure ReconsiderImageDimensionGoalsOnEditor (Selection: boolean; ImagesMode: TImagesMode);
    procedure SetImagesMode(ImagesMode: TImagesMode);

  protected
    procedure BeforeEditorLoaded(Note: TNote); dynamic;
    procedure AfterEditorLoaded(Note: TNote); dynamic;
    procedure NoteUIEnter(Sender: TObject);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  public
    property OnBeforeEditorLoaded: TBeforeEditorLoadedEvent read FOnBeforeEditorLoaded write FOnBeforeEditorLoaded;
    property OnAfterEditorLoaded: TAfterEditorLoadedEvent read FOnAfterEditorLoaded write FOnAfterEditorLoaded;
    procedure SetOnEnter(AEvent: TNotifyEvent);
    procedure SetOnMouseUpOnNote(AEvent: TNotifyEvent);
    procedure SetOnMouseMoveOnNote(AEvent: TNotifyEvent);
    procedure SetFocusOnEditor;
    procedure NNodeDeleted;

  protected
    procedure AdjustTxtTagsWidth (AllowEdition: boolean = False);
    procedure FrameResize(Sender: TObject);
  end;



implementation

{$R *.dfm}

uses
  System.DateUtils,
  kn_LinksMng,
  kn_EditorUtils,
  knt.ui.TagMng,
  knt.RS;


// Create  / Destroy =========================================

{$REGION Create / Destroy}

constructor TKntNoteUI.Create(AOwner: TComponent; KntFolder: TKntFolder);
var
 i: integer;
begin
   inherited Create(AOwner);

   FKntFolder:= KntFolder;

   FEditor := TKntRichEdit.Create( pnlEntries );
   with FEditor do begin
      Parent := pnlEntries;

      Align := alClient;
      HelpContext := 10;
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

   OnEnter:= NoteUIEnter;
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

   FCanvasAux := TControlCanvas.Create;
   FCanvasAux.Control := txtTags;
   FCanvasAux.Font := TxtTags.Font;

   SetReadOnly(KntFolder.ReadOnly);
   fChangingInCode:= false;

   UpdateEditor (FEditor, KntFolder, true); // do this BEFORE placing RTF text in editor

   App.EditorAvailable(FEditor);
end;


destructor TKntNoteUI.Destroy;
begin
    if assigned( FEditor ) then begin
      App.EditorUnavailable(FEditor);
      FreeAndNil(FEditor);
    end;

   if assigned(FCanvasAux) then
      FCanvasAux.Free;

   fImagesReferenceCount:= nil;

   inherited;
end;

{$ENDREGION}


// Controls. Events

{$REGION Controls. Properties and Events }

function TKntNoteUI.GetEditor: TKntRichEdit;
begin
   Result:= FEditor;
end;


function TKntNoteUI.GetReadOnly: boolean;
begin
   Result:= Editor.ReadOnly;
end;

procedure TKntNoteUI.SetReadOnly( AReadOnly : boolean );
begin
   Editor.ReadOnly:= AReadOnly;
   txtName.ReadOnly:= AReadOnly;
   txtTags.ReadOnly:= AReadOnly;

   ConfigureEditor;
end;


procedure TKntNoteUI.SetOnEnter(AEvent: TNotifyEvent);
begin
  FOnEnterOnEditor:= AEvent;
end;

procedure TKntNoteUI.SetOnMouseUpOnNote(AEvent: TNotifyEvent);
begin
   FOnMouseUpOnNote:= AEvent;
end;

procedure TKntNoteUI.SetOnMouseMoveOnNote(AEvent: TNotifyEvent);
begin
   FOnMouseMoveOnNote:= AEvent;
end;


procedure TKntNoteUI.NoteUIEnter(Sender: TObject);
begin
  App.EditorFocused(Editor);
  TagMng.UpdateTxtTagsHint(txtTags);
  if Assigned(FOnEnterOnEditor) then
    FOnEnterOnEditor(Self);
end;

procedure TKntNoteUI.EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUpOnNote) then
    FOnMouseUpOnNote(Self);
end;

procedure TKntNoteUI.EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMoveOnNote) then
    FOnMouseMoveOnNote(Self);
end;

procedure TKntNoteUI.SetInfoPanelHidden(value: boolean);
begin
   FInfoPanelHidden:= value;
   pnlIdentif.Visible := not value;
end;

procedure TKntNoteUI.EditTags;
begin
   SetInfoPanelHidden(False);
   txtTags.SetFocus;
   txtTags.SelStart:= txtTags.GetTextLen;
end;

procedure TKntNoteUI.txtNameChange(Sender: TObject);
begin
   if not ActiveFileIsBusy and assigned(NNode) then begin
      NNode.Note.Name:= txtName.Text;
      FKntFolder.Modified:= True;
   end;
end;

procedure TKntNoteUI.txtNameMouseEnter(Sender: TObject);
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

procedure TKntNoteUI.txtCreationDateMouseEnter(Sender: TObject);
var
  s, lm: string;
begin
  if NNode <> nil then begin
     if FNote.LastModified <> 0 then begin
        if (FNote.LastModified).GetTime <> 0 then
            S:= ' - ' + FormatSettings.ShortTimeFormat;
        lm:= FormatDateTime(FormatSettings.ShortDateFormat + S, FNote.LastModified);
     end;
     s:= Format(GetRS(sUInote01), [txtCreationDate.Text, lm]);
  end;
  txtCreationDate.Hint:= s;
end;

procedure TKntNoteUI.txtEnter(Sender: TObject);
begin
   if txtName.Focused and not txtName.ReadOnly then
      txtName.Color:= clWindow;

   txtName.SelLength:= 0;
   NoteUIEnter(Sender);
end;

procedure TKntNoteUI.txtNameExit(Sender: TObject);
begin
   txtName.Color:= FColorTxts;
end;


procedure TKntNoteUI.SetFocusOnEditor;
begin
  try
     Editor.SetFocus;
  except
  end;
end;


{$ENDREGION}


// Tags =========================================

{$REGION Tags }

procedure TKntNoteUI.txtTagsEnter(Sender: TObject);
begin
   if txtTags.ReadOnly then exit;

   TagMng.StartTxtTagProcessing(txtTags, tmEdit, OnEndTagsIntroduction, Note, Folder);
   AdjustTxtTagsWidth(True);
end;

procedure TKntNoteUI.OnEndTagsIntroduction(PressedReturn: boolean);
begin
  if PressedReturn then
     Editor.SetFocus;

   txtTags.Color:= FColorTxts;
   AdjustTxtTagsWidth;

   InfoPanelHidden:= Folder.EditorInfoPanelHidden;
end;


procedure TKntNoteUI.AdjustTxtTagsWidth (AllowEdition: boolean = False);
var
  MinNoteNameWidth, MaxAvailableWidth: integer;
  MaxAvailableForTags, TagsWidth: integer;

begin
  MinNoteNameWidth:= FCanvasAux.TextWidth(Note.Name) + 10;
  TagsWidth:= FCanvasAux.TextWidth(txtTags.Text) + 10;

  MaxAvailableWidth:= pnlIdentif.Width - txtCreationDate.Width -4;
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

  txtTags.Width:= TagsWidth;
  txtName.Width:= MaxAvailableWidth - TagsWidth;
  txtName.Left:= txtTags.Left + TagsWidth + 2;
end;


procedure TKntNoteUI.FrameResize(Sender: TObject);
begin
   if Note <> nil then
      AdjustTxtTagsWidth(txtTags.Focused);
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
  NEntry: TNoteEntry;
  KeepModified: boolean;

begin
   Editor.BeginUpdate;         // -> It will also ignore Enter and Change events

   KeepModified:= false;

   try
     try
       if SavePreviousContent and not FNNodeDeleted then
          SaveToDataModel();

       FNNode:= NNode;
       FNNodeDeleted:= false;

       Editor.Clear;
       Editor.ClearUndo;

       if assigned(NNode) then begin
          FNote:= NNode.Note;

          case NNode.WordWrap of
            wwAsFolder : Editor.WordWrap := FKntFolder.WordWrap;
            wwYes : Editor.WordWrap := true;
            wwno :  Editor.WordWrap := false;
          end;

          ReloadFromDataModel;

          { The normal thing is to set Editor.Modified = False at the end of the LoadFocusedNNodeIntoEditor method
            But if hidden marks to be eliminated have been identified (and corrected), it will have been kept as Modified,
            to ensure that this correction ends up persisting. Here we will do the same }
          if Editor.Modified then
             KeepModified:= True;

       end
       else begin
          txtName.Text:= '';
          txtCreationDate.Text:= '';
          txtTags.Text:= '';
          ConfigureEditor;
       end;

     except
       On E : Exception do begin
         messagedlg(E.Message, mtError, [mbOK], 0);
         exit;
       end;
     end;

   finally
     Editor.EndUpdate;
   end;
end;


// TODO: We will have to manage the possible multiple entries of a note.
// FOR THE MOMENT we will work with what we will assume is the only entry

function TKntNoteUI.ReloadMetadataFromDataModel(ReloadTags: boolean = true): TNoteEntry;
var
  S: string;
  ActiveFileIsBusyBAK: boolean;
  i: integer;
begin
   Result:= nil;
   if not assigned(NNode) then exit;

   ActiveFileIsBusyBAK:= ActiveFileIsBusy;
   ActiveFileIsBusy:= True;                   // To avoid txtNameChange => Modified:True
   try
      Result:= Note.Entries[0];       // %%%%
      txtName.Text:= FNote.Name;

      if Result.Created <> 0  then begin
         if (Result.Created).GetTime <> 0 then
               S:= ' - ' + FormatSettings.ShortTimeFormat;
         txtCreationDate.Text:= FormatDateTime(FormatSettings.ShortDateFormat + S, Result.Created);
      end
      else
         txtCreationDate.Text:= '';

      if ReloadTags then
         RefreshTags;

   finally
      ActiveFileIsBusy:= ActiveFileIsBusyBAK;
   end;
end;


procedure TKntNoteUI.RefreshTags;
var
   S: string;
   NEntry: TNoteEntry;
begin
   NEntry:= Note.Entries[0];       // %%%%
   S:= NEntry.TagsNames;
   txtTags.Text:= S;
   TagMng.UpdateTxtTagsHint(txtTags);
   if S = '' then begin
      txtTags.Text:= ' #';
      txtTags.Font.Color:= clGray;
   end
   else
      txtTags.Font.Color:= clWindowText;

   AdjustTxtTagsWidth;
end;


procedure TKntNoteUI.ReloadFromDataModel;
var
  ReadOnlyBAK: boolean;

{$IFDEF KNT_DEBUG}
 str: String;
 dataSize: integer;
{$ENDIF}

 strRTF: AnsiString;
 ContainsImgIDsRemoved: boolean;
 ContainsImages: boolean;

 OnEnterBak: TNotifyEvent;
 NEntry: TNoteEntry;

begin
   NEntry:= ReloadMetadataFromDataModel;
   if NEntry = nil then exit;

   BeforeEditorLoaded(Note);     //%%% ¿Informar tb. del posible cambio de NEntry?

   ConfigureEditor;

   FEditor.BeginUpdate;                   // -> It will also ignore Enter and Change events

   ReadOnlyBAK:= FEditor.ReadOnly;
   ContainsImgIDsRemoved:= false;
   try
     FEditor.ReadOnly:= false;   // To prevent the problem indicated in issue #537
     FEditor.Clear;

     NEntry.Stream.Position := 0;
     strRTF:= '';

     if not NEntry.IsRTF then
        UpdateEditor (FEditor, FKntFolder, False);

     fImagesReferenceCount:= nil;
     if NodeStreamIsRTF (NEntry.Stream) then begin
        FEditor.StreamFormat:= sfRichText;
        if FEditor.SupportsRegisteredImages then begin
           GetImagesIDInstances (NEntry.Stream, NEntry.TextPlain);
           strRTF:= ImageMng.ProcessImagesInRTF(NEntry.Stream.Memory, NEntry.Stream.Size, Self.Name, ImageMng.ImagesMode, '', 0, ContainsImgIDsRemoved, ContainsImages, true);
        end;
     end
     else
        FEditor.StreamFormat:= sfPlainText;

     Log_StoreTick('TKntNoteUI.LoadFromDataModel - BEGIN', 4, +1);
    {$IFDEF KNT_DEBUG}
     if log.Active and  (log.MaxDbgLevel >= 4) then begin
        dataSize:= NEntry.Stream.Size;
        if dataSize > 0 then
           str:= Copy(String(PAnsiChar(NEntry.Stream.Memory)), 1, 250)
        else
           str:= '';
        Log.Add(string.format('sfRichText?:%s DataSize:%d  RTF:"%s"...', [BoolToStr(FEditor.StreamFormat=sfRichText), dataSize, str]),  4 );
     end;
    {$ENDIF}

     if StrRTF <> '' then begin
        FEditor.PutRtfText(strRTF,True,False);               // => ImageManager.StorageMode <> smEmbRTF
        FEditor.ClearUndo;
     end
     else
        FEditor.Lines.LoadFromStream( NEntry.Stream );

     Log_StoreTick('TKntNoteUI.LoadFromDataModel - END', 4, -1);

     FEditor.Color:= GetColor(NNode.EditorBGColor, FKntFolder.EditorChrome.BGColor);
     FEditor.SelStart := Note.SelStart;
     FEditor.SelLength := Note.SelLength;

     if NEntry.Stream.Size = 0 then     // Ensures that new nodes are correctly updated based on default properties (font color, size, ...)
        UpdateEditor (FEditor, FKntFolder, false);

   finally
     FEditor.ReadOnly:= ReadOnlyBAK;

     Editor.RestoreZoomGoal;

     // TODO We should act on the corresponding entry.
     //NEntry:= NNode.Note.SelEntry;
     if assigned(NNode) and (NNode.Note.ScrollPosInEditor.Y > 0) then
        Editor.SetScrollPosInEditor(NNode.Note.ScrollPosInEditor);

     FEditor.EndUpdate;

     if not ContainsImgIDsRemoved then
        FEditor.Modified := false;

     Editor.CheckWordCount(true);

     Editor.ChangedSelection;
     Editor.Change;

     if not ClipCapMng.IsBusy then
        App.EditorReloaded(Editor, Editor.Focused);
   end;


  FEditor.Enabled:= true;
  txtName.Enabled:= True;

  AfterEditorLoaded(Note);
end;


{
  If Editor was modified then it will return the Stream associated to the NEntry that will be updated
}
// Previously: EditorToDataStream

function TKntNoteUI.SaveToDataModel(): TMemoryStream;
var
   KeepUTF8: boolean;
   Encoding: TEncoding;
   strRTF: AnsiString;
   ImagesIDs_New: TImageIDs;
   TextPlain: string;

   NEntry: TNoteEntry;

begin
  Result:= nil;
  Encoding:= nil;

  if assigned(NNode) then begin
     NEntry:= Note.Entries[0];

     Note.ScrollPosInEditor:= FEditor.GetScrollPosInEditor;
     Note.SelStart  := FEditor.SelStart;
     Note.SelLength := FEditor.SelLength;

     if FEditor.Modified then begin
        FEditor.BeginUpdate;
        try
           KeepUTF8:= False;
           if NNode.IsVirtual and NEntry.IsPlainTXT and NodeStreamIsUTF8WithBOM(NEntry.Stream) then
               KeepUTF8:= True;

           NEntry.Stream.Clear;

           try
             FEditor.StreamFormat:= StreamFormatInNEntry(NEntry);
             FEditor.StreamMode := [];
             if FEditor.StreamFormat = sfPlainText then begin
                // If it is a virtual node we will respect the UTF8 encoding it may have.
                // Otherwise it will only be saved as UTF8 if necessary
                if KeepUTF8 or not CanSaveAsANSI(FEditor.Text) then
                   Encoding:= TEncoding.UTF8;
             end;

             FEditor.Lines.SaveToStream( NEntry.Stream, Encoding);

             ImagesIDs_New:= nil;
             if FEditor.SupportsRegisteredImages then begin
                ImagesIDs_New:= FKntFolder.CheckSavingImagesOnMode (imLink, NEntry.Stream, true);
                ImageMng.UpdateImagesCountReferences (fImagesReferenceCount, ImagesIDs_New);
                fImagesReferenceCount:= ImagesIDs_New;
             end;

             if ImagesIDs_New = nil then
                NEntry.TextPlain:= FEditor.TextPlain
             else begin
                { If the node has images we will make sure that in TextPlain we save the version corresponding to imLink,
                  to facilitate search management. See notes on TImageMng.GetPositionOffset }
                NEntry.TextPlain := '';
                InitializeTextPlain(NEntry, RTFAux_Note);
             end;
             NEntry.Stream.Position := 0;
             Result:= NEntry.Stream;
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
     else
       if (NEntry.TextPlain = '') then
          InitializeTextPlain(NEntry, RTFAux_Note);

  end;
end;

procedure TKntNoteUI.ReloadNoteName;
begin
   txtName.Text:= FNote.Name;
end;


procedure TKntNoteUI.NNodeDeleted;
begin
   FNNodeDeleted:= True;
end;


procedure TKntNoteUI.ConfigureEditor;
var
   plainTxt: boolean;
   NEntry: TNoteEntry;
begin

  if NNode = nil then begin
     FEditor.SupportsRegisteredImages:= false;
     FEditor.SupportsImages:= false;
     FEditor.SetVinculatedObjs(nil, nil, nil, nil);
     FEditor.Enabled:= False;
     txtName.Enabled:= False;
  end
  else begin
     NEntry:= NNode.Note.Entries[0];    //%%%
     plainTxt:= NEntry.IsPlainTXT;
     FEditor.SetVinculatedObjs(FKntFolder.KntFile, FKntFolder, NNode, NEntry);
     FEditor.PlainText:= plainTxt;
     FEditor.Chrome:= FKntFolder.EditorChrome;

     FEditor.SupportsRegisteredImages:= (ImageMng.StorageMode <> smEmbRTF) and not plainTxt and not NNode.IsVirtual;
     FEditor.SupportsImages:= not plainTxt;
  end;

end;


function TKntNoteUI.StreamFormatInNEntry(const NEntry: TNoteEntry): TRichStreamFormat;
begin
    if NEntry.IsRTF then
       Result:= sfRichText
    else
       Result:= sfPlainText;

end;


procedure TKntNoteUI.BeforeEditorLoaded(Note: TNote);
begin
   if assigned(FOnBeforeEditorLoaded) then OnBeforeEditorLoaded(Note);
end;


procedure TKntNoteUI.AfterEditorLoaded(Note: TNote);
begin
   if assigned(FOnAfterEditorLoaded) then OnAfterEditorLoaded(Note);
end;



{$ENDREGION}


// Images  =========================================

{$REGION Images }

function TKntNoteUI.GetImagesInstances: TImageIDs;
begin
   Result:= fImagesReferenceCount;
end;


procedure TKntNoteUI.GetImagesIDInstances (Stream: TMemoryStream; TextPlain: String);
begin
   if (TextPlain <> '') then
      fImagesReferenceCount:= ImageMng.GetImagesIDInstancesFromTextPlain (TextPlain)
   else
      fImagesReferenceCount:= ImageMng.GetImagesIDInstancesFromRTF (Stream);
end;


procedure TKntNoteUI.ResetImagesReferenceCount;
begin
    SetLength(fImagesReferenceCount, 0);
end;


procedure TKntNoteUI.ReloadImagesOnEditor;
var
   ImgeIDs: TImageIDs;
begin
   ImgeIDs:= ImageMng.GetImagesIDInstancesFromTextPlain (Editor.TextPlain);
   ImageMng.ReloadImages(ImgeIDs);

   SaveToDataModel;
   ReloadFromDataModel;
end;


procedure TKntNoteUI.ReconsiderImageDimensionGoalsOnEditor(Selection: boolean; ImagesMode: TImagesMode);
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


procedure TKntNoteUI.SetImagesMode(ImagesMode: TImagesMode);
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
