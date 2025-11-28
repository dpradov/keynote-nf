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
  TKntNoteEntriesUI = class(TFrame)
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
    FNEntryID: Word;
    FKntFolder: TKntFolder;
    FEditor: TKntRichEdit;
    FNoteUI: INoteUI;

    FInfoPanelHidden: boolean;
    fImagesReferenceCount: TImageIDs;

    fChangingInCode: boolean;

    FOnEnterOnEditor: TNotifyEvent;
    FOnMouseUpOnNoteEntries: TNotifyEvent;
    FOnMouseMoveOnNoteEntries: TNotifyEvent;

    function GetEditor: TKntRichEdit;
    function GetNNode: TNoteNode;
    function GetFolder: TObject;


  public
    constructor Create(AOwner: TComponent; NoteUI: INoteUI);
    destructor Destroy; override;

    property Editor : TKntRichEdit read GetEditor;

  public
    property Folder: TKntFolder read FKntFolder;
    property Note: TNote read FNote;
    property NNode: TNoteNode read GetNNode;
    procedure LoadFromNNode (NNode: TNoteNode; NEntryID: Word; SavePreviousContent: boolean);
    procedure ReloadFromDataModel;
    function ReloadMetadataFromDataModel(ReloadTags: boolean = true): TNoteEntry;
    procedure SaveToDataModel;
    procedure ReloadNoteName;
    procedure ConfigureEditor;
  protected
    function StreamFormatInNEntry(const NEntry: TNoteEntry): TRichStreamFormat;

  protected
    procedure SetInfoPanelHidden(value: boolean);
    procedure OnEndEditTagsIntroduction(PressedReturn: boolean);
    procedure AdjustTxtTagsWidth (AllowEdition: boolean = False);
    procedure FrameResize(Sender: TObject);
  public
    procedure EditTags;
    procedure RefreshTags;
    property InfoPanelHidden: boolean read FInfoPanelHidden write SetInfoPanelHidden;

  protected
    function GetReadOnly: boolean;
    procedure SetReadOnly( AReadOnly : boolean );
  public
    property ReadOnly : boolean read GetReadOnly write SetReadOnly;

  protected
    function GetImagesInstances: TImageIDs;
    property ImagesReferenceCount: TImageIDs read fImagesReferenceCount write fImagesReferenceCount;
  public
    property ImagesInstances: TImageIDs read GetImagesInstances;
    procedure GetImagesIDInstances (Stream: TMemoryStream; TextPlain: String);
    procedure ResetImagesReferenceCount;
    procedure ReloadImagesOnEditor;
    procedure ReconsiderImageDimensionGoalsOnEditor (Selection: boolean; ImagesMode: TImagesMode);
    procedure SetImagesMode(ImagesMode: TImagesMode);

  protected
    procedure NoteEntriesUIEnter(Sender: TObject);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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
  kn_LinksMng,
  kn_EditorUtils,
  knt.ui.TagMng,
  knt.RS;


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

   SetReadOnly(FKntFolder.ReadOnly);
   fChangingInCode:= false;

   UpdateEditor (FEditor, FKntFolder, true); // do this BEFORE placing RTF text in editor

   App.EditorAvailable(FEditor);
end;


destructor TKntNoteEntriesUI.Destroy;
begin
    if assigned( FEditor ) then begin
      App.EditorUnavailable(FEditor);
      FreeAndNil(FEditor);
    end;

   fImagesReferenceCount:= nil;

   inherited;
end;

{$ENDREGION}


// Controls. Events

{$REGION Controls. Properties and Events }

function TKntNoteEntriesUI.GetEditor: TKntRichEdit;
begin
   Result:= FEditor;
end;


function TKntNoteEntriesUI.GetReadOnly: boolean;
begin
   Result:= Editor.ReadOnly;
end;

procedure TKntNoteEntriesUI.SetReadOnly( AReadOnly : boolean );
begin
   Editor.ReadOnly:= AReadOnly;
   txtName.ReadOnly:= AReadOnly;
   txtTags.ReadOnly:= AReadOnly;

   ConfigureEditor;
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
  FloatingEditorCannotBeSaved:= False;
  Editor.HideNestedFloatingEditor;
  App.EditorFocused(Editor);
  TagMng.UpdateTxtTagsHint(txtTags);
  if Assigned(FOnEnterOnEditor) then
    FOnEnterOnEditor(Self);

  if FloatingEditorCannotBeSaved then
     Editor.ActivateFloatingEditor;
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

procedure TKntNoteEntriesUI.txtEnter(Sender: TObject);
begin
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
   NEntry: TNoteEntry;
begin
   NEntry:= Note.Entries[FNEntryID];       // %%%%
   S:= NEntry.TagsNames;
   txtTags.Text:= S;
   TagMng.UpdateTxtTagsHint(txtTags);
   if S = '' then begin
      txtTags.Text:= EMPTY_TAGS;
      txtTags.Font.Color:= clGray;
   end
   else
      txtTags.Font.Color:= clWindowText;

   AdjustTxtTagsWidth;
end;

procedure TKntNoteEntriesUI.txtTagsEnter(Sender: TObject);
begin
   if txtTags.ReadOnly then exit;

   TagMng.StartTxtEditTagIntrod(txtTags, OnEndEditTagsIntroduction, Note, Folder);
   AdjustTxtTagsWidth(True);
end;

procedure TKntNoteEntriesUI.OnEndEditTagsIntroduction(PressedReturn: boolean);
begin
  if PressedReturn then
     Editor.SetFocus;

   txtTags.Color:= FColorTxts;
   AdjustTxtTagsWidth;

   InfoPanelHidden:= Folder.EditorInfoPanelHidden;
end;


procedure TKntNoteEntriesUI.AdjustTxtTagsWidth (AllowEdition: boolean = False);
const
  MinTagsWidth = 17;
var
  MinNoteNameWidth, MaxAvailableWidth: integer;
  MaxAvailableForTags, TagsWidth: integer;

begin
  MinNoteNameWidth:= TagMng.GetTextWidth(Note.Name, txtName) + 10;
  TagsWidth:=   MinTagsWidth;
  if txtTags.Text <> EMPTY_TAGS then
     TagsWidth:= TagMng.GetTextWidth(txtTags.Text, txtTags) + 10;

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


procedure TKntNoteEntriesUI.FrameResize(Sender: TObject);
begin
   if Note <> nil then
      AdjustTxtTagsWidth(txtTags.Focused);
end;


{$ENDREGION}


// Load and save Editor from Note node =========================================

{$REGION Load, save and configure Editor for a Note node }


function TKntNoteEntriesUI.GetNNode: TNoteNode;
begin
   Result:= FNNode;
end;

function TKntNoteEntriesUI.GetFolder: TObject;
begin
   Result:= FKntFolder;
end;

procedure TKntNoteEntriesUI.LoadFromNNode(NNode: TNoteNode; NEntryID: Word; SavePreviousContent: boolean);
var
  NEntry: TNoteEntry;
  KeepModified: boolean;

begin
   Editor.BeginUpdate;         // -> It will also ignore Enter and Change events

   KeepModified:= false;

   try
     try
       if SavePreviousContent and not FNoteUI.GetNNodeDeleted then
          SaveToDataModel();

       FNNode:= NNode;
       FNEntryID:= NEntryID;
       //FNNodeDeleted:= false;    //##

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
         App.ErrorPopup(E);
         exit;
       end;
     end;

   finally
     Editor.EndUpdate;
   end;
end;


function TKntNoteEntriesUI.ReloadMetadataFromDataModel(ReloadTags: boolean = true): TNoteEntry;
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
      Result:= Note.Entries[FNEntryID];       // %%%%
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

procedure TKntNoteEntriesUI.ReloadFromDataModel;
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

     // *1 For newly created, empty notes, this must be ensured (when the note is not intended to be created as plain text. See call to ConfigureEditor).
     //    If we don't do this, we may encounter with an exception when calling LoadFromStream while working with the note, before it
     //    is persisted to the model (for example, when selecting another note from the tree). This can occur if in that situation
     //    we select several lines and press Shift+TAB (to tab multiple lines, decreasing indentation)

     fImagesReferenceCount:= nil;
     if (not FEditor.PlainText) and (NEntry.Stream.Size = 0) then
        FEditor.StreamFormat:= sfRichText                             // *1

     else begin
       if NodeStreamIsRTF (NEntry.Stream) then begin
          FEditor.StreamFormat:= sfRichText;
          if FEditor.SupportsRegisteredImages then begin
             GetImagesIDInstances (NEntry.Stream, NEntry.TextPlain);
             strRTF:= ImageMng.ProcessImagesInRTF(NEntry.Stream.Memory, NEntry.Stream.Size, Self.Name, ImageMng.ImagesMode, '', 0, ContainsImgIDsRemoved, ContainsImages, true);
          end;
       end
       else
          FEditor.StreamFormat:= sfPlainText;
     end;

     Log_StoreTick('TKntNoteEntriesUI.LoadFromDataModel - BEGIN', 4, +1);
    {$IFDEF KNT_DEBUG}
     if log.Active and  (log.MaxDbgLevel >= 5) then begin
        dataSize:= NEntry.Stream.Size;
        if dataSize > 0 then
           str:= Copy(String(PAnsiChar(NEntry.Stream.Memory)), 1, 90)
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
     if NEntry.Stream.Size > 0 then
        FEditor.Lines.LoadFromStream( NEntry.Stream );

     Log_StoreTick('TKntNoteEntriesUI.LoadFromDataModel - END', 4, -1);

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

     FEditor.SetLangOptions(False);
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
end;


// Previously: EditorToDataStream

procedure TKntNoteEntriesUI.SaveToDataModel;
var
   KeepUTF8: boolean;
   Encoding: TEncoding;
   strRTF: AnsiString;
   ImagesIDs_New: TImageIDs;
   TextPlain: string;

   NEntry: TNoteEntry;

begin
  Encoding:= nil;

  if assigned(NNode) then begin
     if FEditor.FloatingEditor <> nil then
        FEditor.DoSaveChangesInFloatingEditor;

     NEntry:= Note.Entries[FNEntryID];        // %%%%

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

procedure TKntNoteEntriesUI.ReloadNoteName;
begin
   txtName.Text:= FNote.Name;
end;


procedure TKntNoteEntriesUI.ConfigureEditor;
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
     NEntry:= NNode.Note.Entries[FNEntryID];    // %%%%
     plainTxt:= NEntry.IsPlainTXT;
     FEditor.SetVinculatedObjs(FKntFolder.KntFile, FKntFolder, NNode, NEntry);
     FEditor.PlainText:= plainTxt;
     FEditor.Chrome:= FKntFolder.EditorChrome;

     FEditor.SupportsRegisteredImages:= (ImageMng.StorageMode <> smEmbRTF) and not plainTxt and not NNode.IsVirtual;
     FEditor.SupportsImages:= not plainTxt;
  end;

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
   Result:= fImagesReferenceCount;
end;


procedure TKntNoteEntriesUI.GetImagesIDInstances (Stream: TMemoryStream; TextPlain: String);
begin
   if (TextPlain <> '') then
      fImagesReferenceCount:= ImageMng.GetImagesIDInstancesFromTextPlain (TextPlain)
   else
      fImagesReferenceCount:= ImageMng.GetImagesIDInstancesFromRTF (Stream);
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
