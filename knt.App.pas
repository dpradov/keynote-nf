unit knt.App;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface
uses
   Winapi.Windows,
   Winapi.Messages,
   Winapi.ShellAPI,
   Winapi.RichEdit,
   System.Classes,
   System.SysUtils,
   System.StrUtils,
   System.AnsiStrings,
   System.IniFiles,
   Vcl.Clipbrd,
   Vcl.Graphics,
   Vcl.FileCtrl,
   Vcl.Controls,
   Vcl.ComCtrls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.ExtCtrls,

   VirtualTrees,

   gf_misc,
   gf_miscvcl,
   kn_ImagesMng,
   kn_AlertMng,
   kn_global,
   kn_info,
   kn_const,
   kn_cmd,
   kn_KntFile,
   kn_KntFolder,
   knt.model.note,
   kn_EditorUtils,
   knt.ui.editor,
   knt.ui.tree,
   kn_Main
   ;

type
   TKeyboardState = record     //==================== KEYBOARD / HOTKEY
      HotKeySuccess : boolean;        // if true, we registered the hotkey successully, so we will remember to unregister it when we shut down
      OtherCommandsKeys: TList;       // List of TKeyOtherCommandItem
      LastRTFKey : TKeyCode;
      RxRTFKeyProcessed : boolean;    // for TAB handling; some tabs are eaten by TRichEdit, others must not be
      RTFUpdating : boolean;          // TRUE while in RxRTFSelectionChange; some things cannot be done during that time
   end;
   TTagsState = (tsHidden, tsPendingUpdate, tsVisible);

   TNNodeSelectedEvent = procedure(NNode: TNoteNode) of object;
   TFolderSelectedEvent = procedure(Folder: TKntFolder) of object;

   TKntRichEditList =  TSimpleObjList<TKntRichEdit>;


   TKntApp = class
   private class var
      fInstance: TKntApp;   // For Singleton pattern

   public class var
      Kbd: TKeyboardState;

      //================================================== OPTIONS
      { These options are seperate from KeyOptions, because then may also be set via commandline. Basically, the logic is:
        opt_XXX := ( commandline_argument_XXX OR inifile_options_XXX );
      }
      opt_Minimize : boolean; // minimize on startup
      //opt_Setup : boolean; // run setup (OBSOLETE, unused)
      opt_Debug : boolean; // debug info
      opt_NoRegistry : boolean; // use .MRU file instead, do not use registry
      opt_NoReadOpt : boolean; // do not read config files (if TRUE, then opt_NoSaveOpt is also set to TRUE)
      opt_NoSaveOpt : boolean; // do not save config files
      opt_NoDefaults : boolean; // do not load .DEF file (editor and tree defaults)
      opt_RegExt : boolean; // register .KNT and .KNE extensions
      opt_SaveDefaultIcons : boolean; // save default tab icons to file
      opt_NoUserIcons : boolean; // do not use custom .ICN file
      opt_SaveToolbars : boolean; // save default toolbar state (debug)
      opt_SaveMenus : boolean; // save menu item information
      opt_DoNotDisturb : boolean; // Ignore for purposes of "SingleInstance"
      opt_Title: string; // Title to use in main window (mainly for its use with kntLauncher)
      opt_Clean : boolean; // Clean the file, actually looking for invalid hyperlinks (see issue #59: http://code.google.com/p/keynote-nf/issues/detail?id=59
      opt_ConvKNTLinks: boolean;  // Convert Knt Links to the new format (using GID)

      ShowingImageOnTrack: boolean;

   private
      fNNodeSelected:   TNNodeSelectedEvent;
      fFolderSelected: TFolderSelectedEvent;
      fAvailableEditors: TKntRichEditList;

      fVirtualUnEncryptWarningDone: boolean;
      fUI_RTL: boolean;
      fTagsState: TTagsState;

      constructor Create;
      procedure Initialize;

   protected
      procedure UpdateEnabledActionsAndRTFState(Editor: TKntRichEdit);

      procedure EditorSelected (Editor: TKntRichEdit; Focused: boolean); overload;
      procedure EnsureContentEditorUpdated (Editor: TKntRichEdit);
      procedure FolderSelected(Folder: TKntFolder; PrevFolder: TKntFolder);
      procedure NNodeSelected(NNode: TNoteNode);

      procedure ShowWordCountInfoInStatusBar(const str: string);
      function GetWordCountInfoInStatusBar: string;

   public
      class function GetInstance: TKntApp; static;

      class procedure FileSetModified; inline;

      procedure ApplyBiDiMode;
      procedure ApplyBiDiModeOnForm(Form: TForm);
      property UI_RTL: boolean read fUI_RTL write fUI_RTL;
      property OnNNodeSelected: TNNodeSelectedEvent read FNNodeSelected write FNNodeSelected;
      property OnFolderSelected: TFolderSelectedEvent read FFolderSelected write FFolderSelected;

      procedure AuxEditorFocused(Sender: TObject);
      procedure SetTopMost(hWND: HWND; OnlyWithFloatingEditor: boolean = True);
      procedure HideNestedFloatingEditors;

      procedure EditorAvailable (Editor: TKntRichEdit);
      procedure EditorUnavailable (Editor: TKntRichEdit);
      procedure EditorFocused (Editor: TKntRichEdit);
      procedure EditorReloaded (Editor: TKntRichEdit; Focused: boolean);
      procedure EditorSaved (Editor: TKntRichEdit);
      procedure ChangeInEditor (Editor: TKntRichEdit);
      procedure NEntryModified (NEntry: TNoteEntry; Note: TNote; Folder: TKntFolder);
      procedure EditorPropertiesModified (Editor: TKntRichEdit);
      procedure SetEditorZoom( ZoomValue : integer; const ZoomString : string; Increment: integer= 0);
      procedure ShowCurrentZoom (Zoom: integer);

      procedure TreeFocused (Tree: TKntTreeUI);
      procedure NNodeFocused(NNode: TNoteNode);
      procedure FolderDeleted (Folder: TKntFolder; TabIndex: integer);
      procedure FolderPropertiesModified (Folder: TKntFolder);

      procedure FileNew (aFile: TKntFile);
      procedure FileOpening (aFile: TKntFile);
      procedure FileOpen (aFile: TKntFile);
      procedure FileClosed (aFile: TKntFile);

      procedure ActivateFolder (Folder: TKntFolder); overload;
      procedure ActivateFolder (TabIndex: Integer); overload;

      procedure NoteNameModified(Note: TNote);

      property WordCountInfoInStatusBar: string read GetWordCountInfoInStatusBar write ShowWordCountInfoInStatusBar;
      procedure ShowStatistics;
      procedure ShowTipOfTheDay;

      function CheckActiveEditor: boolean;
      function CheckActiveEditorNotReadOnly: boolean;

      procedure TagsUpdated;
      property TagsState: TTagsState read fTagsState write fTagsState;

      procedure ShowInfoInStatusBar(const str: string);
      procedure WarnEditorIsReadOnly;
      procedure WarnNoTextSelected;

      function DoMessageBox(const Str: string; DlgType: TMsgDlgType;
                            const Buttons: TMsgDlgButtons;
                            DefButton: TMsgDlgDefBtn = def1;
                            HelpCtx: Longint = 0; hWnd: HWND= 0): integer;
      function PopUpMessage(const Str: string; const mType: TMsgDlgType;
                            const Buttons: TMsgDlgButtons;
                            const DefButton: TMsgDlgDefBtn = def1;
                            const HelpCtx: integer= 0): word;
      procedure InfoPopup(const aStr: string);
      procedure WarningPopup(const aStr: string);
      procedure ErrorPopup(const aStr: string); overload;
      procedure ErrorPopup(const E: Exception = nil; const Str: string = ''); overload;
      procedure WarnFunctionNotImplemented(const aStr: string);
      procedure WarnCommandNotImplemented(const aStr: string);

      property Virtual_UnEncrypt_Warning_Done: boolean read fVirtualUnEncryptWarningDone write fVirtualUnEncryptWarningDone;
   end;


  function GetCurrentTreeNode : PVirtualNode;
  function GetTreeUI(TV: TVirtualStringTree): TKntTreeUI;


var
   App: TKntApp;

   ActiveFile   : TKntFile;
   ActiveFolder : TKntFolder;
   ActiveNNode  : TNoteNode;
   ActiveNEntry : TNoteEntry;

   ActiveEditor : TKntRichEdit;
   ActiveTreeUI : TKntTreeUI;

   Form_Main:  TForm_Main;
   ImageMng:   TImageMng;
   AlarmMng:   TAlarmMng;
   ClipCapMng: TClipCapMng;

   ActiveFileIsBusy : boolean;
   AFileIsLoading: boolean;
   HandlingTimerTick: boolean;
   UpdatingTextPlain: boolean;

   IgnoringEditorChanges: boolean;

   //================================================ APPLICATION OPTIONS
   // these are declared in kn_Info.pas
   KeyOptions : TKeyOptions; // general program config
   TabOptions : TTabOptions; // options related to tabs, icons etc
   ClipOptions : TClipOptions; // clipboard capture options
   EditorOptions : TEditorOptions;
   ResPanelOptions : TResPanelOptions;
   KntTreeOptions : TKntTreeOptions;
   FindOptions : TFindOptions;

   //================================================== DEFAULT PROPERTIES
   DefaultEditorProperties : TFolderEditorProperties;
   DefaultTabProperties : TFolderTabProperties;
   DefaultEditorChrome : TChrome;
   DefaultTreeChrome : TChrome;
   DefaultTreeProperties : TFolderTreeProperties;

   LongDateToFileSettings: TFormatSettings;


implementation
uses
   GFTipDlg,
   gf_Lang,
   kn_MacroMng,
   kn_VCLControlsMng,
   kn_LinksMng,
   kn_FindReplaceMng,
   kn_NoteFileMng,
   knt.ui.TagMng,
   knt.RS;



constructor TKntApp.Create;
begin
  inherited Create;

  Initialize;
end;

class function TKntApp.GetInstance: TKntApp;
begin
  if (fInstance = nil) then                                // Singleton pattern
     fInstance:= TKntApp.Create;
  result:= fInstance;
end;


procedure TKntApp.Initialize;
begin
   Kbd.RTFUpdating := false;
   ShowingImageOnTrack:= false;
   fUI_RTL:= false;

   opt_Minimize := false;
   //opt_Setup := false;
   opt_Debug := false;
   opt_NoRegistry := false;
   opt_NoReadOpt := false;
   opt_NoSaveOpt := false;
   opt_NoDefaults := false;
   opt_RegExt := false;
   opt_SaveDefaultIcons  := false;
   opt_NoUserIcons := false;
   opt_SaveToolbars := false;
   opt_SaveMenus := false;
   opt_DoNotDisturb:= false;
   opt_Title:= '';
   opt_Clean := false;
   opt_ConvKNTLinks:= false;

   fAvailableEditors:= TKntRichEditList.Create;
   fVirtualUnEncryptWarningDone:= false;

   ActiveFile := nil;
   ActiveEditor := nil;
   ActiveFolder := nil;
   ActiveFileIsBusy := false;

   IgnoringEditorChanges:= false;
   HandlingTimerTick:= false;
   UpdatingTextPlain:= false;
   fTagsState:= tsPendingUpdate;

   LongDateToFileSettings:= TFormatSettings.Create;
   with LongDateToFileSettings do begin
      DateSeparator := _DATESEPARATOR;
      TimeSeparator := _TIMESEPARATOR;
      ShortDateFormat := _SHORTDATEFMT;
      LongDateFormat := _LONG_DATETIME_TOFILE;    // I don't thik this field is used to parse a string..
      LongTimeFormat := _LONGTIMEFMT;             // Idem..
   end;

end;


procedure TKntApp.ApplyBiDiMode;
var
  BiDi: TBidiMode;
  i: integer;
begin
   if Form_Main = nil then exit;

   BiDi:= bdLeftToRight;
   if fUI_RTL then
      BiDi:= bdRightToLeft;

   if Form_Main.BiDiMode <> BiDi then begin
      Form_Main.BiDiMode:= BiDi;
      Form_Main.Menu_TV.BiDiMode:= BiDi;
      Form_Main.Menu_RTF.BiDiMode:= BiDi;

      if BiDi = bdRightToLeft then
         BiDi:= bdRightToLeftNoAlign;
      Form_Main.Combo_Font.BiDiMode:= BiDi;

      // *1 To ensure icon is shown after changing BiDiMode
      //    TabSheet  (TTab95Sheet) is an old component and has not BiDiMode property..
      if ActiveFile <> nil then
         for i := 0 to ActiveFile.Folders.Count -1 do begin
             ActiveFile.Folders[i].UpdateTabSheet;       // *1
             ActiveFile.Folders[i].TreeUI.ApplyBiDiMode;
         end;
   end;

   if (BiDI = bdLeftToRight) and IsRightToLeftLanguage() then begin  // UserDefaultUILanguage is RTL...
      Form_Main.Combo_ResFind.BiDiMode:= bdRightToLeft;
      Form_Main.Dock_Top.BiDiMode:= bdRightToLeftNoAlign;
      Form_Main.Dock_Bottom.BiDiMode:= bdRightToLeftNoAlign;
      if ActiveFile <> nil then
         for i := 0 to ActiveFile.Folders.Count -1 do
             ActiveFile.Folders[i].TreeUI.ApplyBiDiMode;
   end;

end;

procedure TKntApp.ApplyBiDiModeOnForm(Form: TForm);
begin
  if fUI_RTL = (Form.BiDiMode = bdRightToLeft) then exit;

  if fUI_RTL then begin
     Form.BiDiMode:= bdRightToLeft;
     AdjustRTLControls(Form);
  end
  else
     Form.BiDiMode:= bdLeftToRight;
end;



function TKntApp.CheckActiveEditor: boolean;
begin
    Result:= False;
    if not assigned(ActiveEditor) then begin
       ShowInfoInStatusBar(GetRS(sApp02));
       exit;
    end;

    Result:= True;
end;

function TKntApp.CheckActiveEditorNotReadOnly: boolean;
begin
    Result:= False;
    if not assigned(ActiveEditor) then begin
       ShowInfoInStatusBar(GetRS(sApp02));
       exit;
    end;

    if ActiveEditor.ReadOnly then begin
       WarnEditorIsReadOnly;
       exit;
    end;

    Result:= True;
end;

procedure TKntApp.ActivateFolder (TabIndex: integer);
begin
    ActivateFolder (ActiveFile.GetFolderByTabIndex(TabIndex));
end;


procedure TKntApp.ActivateFolder (Folder: TKntFolder);
var
 TabIndex: integer;
 FocusedOk: boolean;
begin
    if not assigned(ActiveFile) then exit;

    with Form_Main do begin
       if not assigned(Folder) then begin
          Folder:= ActiveFile.GetFolderByID(ActiveFile.SavedActiveFolderID);
       end;

       if assigned(Folder) then
          TabIndex:= Folder.TabIndex
       else begin
          TabIndex:= 0;
          Folder:= ActiveFile.GetFolderByTabIndex(TabIndex);
       end;

       if ( Pages.PageCount > TabIndex ) then
          Pages.ActivePage := Pages.Pages[TabIndex];

       if assigned(Folder) then begin
          FocusedOk:= false;
          if not Initializing and  (Folder.FocusMemory <> focNil) then
             try
                if (Folder.FocusMemory = focTree) and not Folder.TreeHidden then
                   Folder.TV.SetFocus
                else
                   Folder.Editor.SetFocus;

                FocusedOk:= true;
             except
                 // On E : Exception do ShowMessage( E.Message );
             end;

          if not FocusedOk or (ActiveFolder <> Folder) then      // (ActiveFolder <> Folder) Puede ocurrir si se ha ejecutado Editor.BeginUpdate ...
             EditorSelected(Folder.Editor, false);

          if not initializing then
             CheckFilterOnTags(True);
       end;
    end;
end;

procedure TKntApp.NoteNameModified(Note: TNote);
var
  i: integer;
  nnf: TNoteNodeInFolder;
  Folder: TKntFolder;
begin
   if ActiveFileIsBusy or AFileIsLoading then exit;

   for i:= 0 to High(Note.NNodes) do begin
       nnf:= Note.NNodes[i];
       Folder:= TKntFolder(nnf.Folder);
       TKntFolder(nnf.Folder).NoteNameModified(nnf.NNode);
   end;

end;


procedure TKntApp.UpdateEnabledActionsAndRTFState(Editor: TKntRichEdit);
var
  Edit_PlainText, Edit_SupportsImages, Edit_SupportsRegImages, Edit_NoteObj, Edit_Enabled: boolean;

begin
  if (Editor = nil) or not Editor.Enabled then begin
      Edit_PlainText:= true;
      Edit_SupportsImages:= false;
      Edit_SupportsRegImages:= false;
      Edit_NoteObj:= false;
      Edit_Enabled:= false;
  end
  else begin
      Edit_PlainText:= Editor.PlainText;
      Edit_SupportsImages:= Editor.SupportsImages;
      Edit_SupportsRegImages:= Editor.SupportsRegisteredImages;
      Edit_NoteObj:= (Editor.NNodeObj <> nil);
      Edit_Enabled:= not Editor.ReadOnly;
  end;

  Form_Main.EnableActionsForEditor(not Edit_PlainText, Edit_Enabled);
  Form_Main.EnableActionsForEditor(Edit_NoteObj, Edit_SupportsImages, Edit_SupportsRegImages);
  Form_Main.RxChangedSelection(Editor, true);
  Form_Main.UpdateWordWrap;
  ClipCapMng.ShowState;
end;


procedure TKntApp.SetTopMost(hWND: HWND; OnlyWithFloatingEditor: boolean = True);
begin
   if OnlyWithFloatingEditor and ((ActiveFolder = nil) or (ActiveFolder.Editor.FloatingEditor = nil)) then exit;

   SetWindowPos(hWND, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE)
end;

procedure TKntApp.HideNestedFloatingEditors;
begin
  if (ActiveFolder <> nil) and (ActiveFolder.Editor.FloatingEditor <> nil) then
      ActiveFolder.Editor.HideNestedFloatingEditor;
end;


procedure TKntApp.AuxEditorFocused(Sender: TObject);
begin
  EditorFocused(TKntRichEdit(Sender));
end;


procedure TKntApp.EditorFocused (Editor: TKntRichEdit);
begin
   EditorSelected(Editor, true);
   if Form_Main.ShortcutAltDownMenuItem <> nil then
      Form_Main.ShortcutAltDownMenuItem.Enabled:= True;
end;

procedure TKntApp.EditorReloaded (Editor: TKntRichEdit; Focused: boolean);
begin
   if Editor = nil then exit;

   EditorSelected(Editor, Focused);     // Focused=False: Will not set ActiveFolder.FocusMemory:= focRTF (but will not set := focTree either)
end;


procedure TKntApp.EditorSelected (Editor: TKntRichEdit; Focused: boolean);
var
   OldNNode: TNoteNode;
   OldFolder: TKntFolder;
begin
    EnsureContentEditorUpdated (Editor);

    ActiveEditor:= Editor;

    if Focused then begin
       UpdateEnabledActionsAndRTFState(Editor);
       ShowCurrentZoom(Editor.GetZoom);
       Editor.UpdateCursorPos;
    end;

    if assigned(Editor.NNodeObj) then begin
       OldFolder:= ActiveFolder;
       OldNNode:= ActiveNNode;

       ActiveNEntry:= TNoteEntry(Editor.NEntryObj);
       ActiveNNode:= TNoteNode(Editor.NNodeObj);
       ActiveFolder:= TKntFolder(Editor.FolderObj);
       ActiveFile:= TKntFile(Editor.FileObj);
       ActiveTreeUI:= nil;
       if ActiveFolder <> nil then
          ActiveTreeUI:= ActiveFolder.TreeUI;

       if Focused then begin
          ActiveFolder.FocusMemory:= focRTF;
          if (ActiveTreeUI.TV.SelectedCount <> 1) and (ActiveTreeUI.TV.FocusedNode <> nil) then begin
             ActiveTreeUI.TV.ClearSelection;
             ActiveTreeUI.TV.Selected[ActiveTreeUI.TV.FocusedNode] := True;
          end;
       end;

       if OldFolder <> ActiveFolder then
          FolderSelected(ActiveFolder, OldFolder);
       if OldNNode <> ActiveNNode then
          NNodeSelected(ActiveNNode);
    end;
end;

procedure TKntApp.EditorAvailable (Editor: TKntRichEdit);
begin
   if fAvailableEditors.IndexOf(Editor) < 0 then
      fAvailableEditors.Add(Editor);
end;

procedure TKntApp.EditorUnavailable (Editor: TKntRichEdit);
begin
   fAvailableEditors.Remove(Editor);
end;

procedure TKntApp.EditorSaved (Editor: TKntRichEdit);
var
   NNodeSavedEditor, NNode: TNoteNode;
   NoteSavedEditor: TNote;
   E: TKntRichEdit;
   i: integer;
   SP: TPoint;
   SS,SL: integer;

begin
   if Editor = nil then exit;

   NNodeSavedEditor:= TNoteNode(Editor.NNodeObj);
   if NNodeSavedEditor = nil then exit;
   NoteSavedEditor:= NNodeSavedEditor.Note;

   if NoteSavedEditor.NumNNodes <= 1 then exit;

   for i:= 0 to fAvailableEditors.Count-1 do begin
      E:= fAvailableEditors[i];
      if (E = Editor) then continue;

      NNode:= TNoteNode(E.NNodeObj);
      if NNode = nil then continue;
      if NoteSavedEditor = NNode.Note then begin
         SP:= E.GetScrollPosInEditor;
         SS := E.SelStart;
         SL := E.SelLength;
         TKntFolder(E.FolderObj).ReloadEditorFromDataModel(false);
         E.SelStart:= SS;
         E.SelLength:= SL;
         E.SetScrollPosInEditor(SP);
      end;
   end;

end;


procedure TKntApp.EnsureContentEditorUpdated (Editor: TKntRichEdit);
var
   NNodeSelecEditor, NNode: TNoteNode;
   NoteSelecEditor: TNote;
   E: TKntRichEdit;
   i: integer;
begin
   if Editor = nil then exit;

   NNodeSelecEditor:= TNoteNode(Editor.NNodeObj);
   if NNodeSelecEditor = nil then exit;
   NoteSelecEditor:= NNodeSelecEditor.Note;

   if NoteSelecEditor.NumNNodes <= 1 then exit;

   for i:= 0 to fAvailableEditors.Count-1 do begin
      E:= fAvailableEditors[i];
      if (E = Editor) or (not E.Modified) then continue;

      NNode:= TNoteNode(E.NNodeObj);
      if NNode = nil then continue;
      if NoteSelecEditor = NNode.Note then begin
         TKntFolder(E.FolderObj).SaveEditorToDataModel;   // Will force reload from any Editor with the same (linked) NNode open => App.EditorSaved()
         exit;
      end;
   end;

end;


procedure TKntApp.NNodeFocused(NNode: TNoteNode);
begin
   ActiveNNode:= NNode;
   NNodeSelected(NNode);
end;

procedure TKntApp.NNodeSelected(NNode: TNoteNode);
begin
   if assigned(NNode) then begin
      if ActiveFolder.FocusMemory = focTree then
         Form_Main.ShowNodeChromeState (ActiveFolder.TreeUI);
   end
   else
      UpdateEnabledActionsAndRTFState(TKntRichEdit(nil));

   if assigned(fNNodeSelected) then
      OnNNodeSelected(NNode);
end;


procedure TKntApp.ChangeInEditor (Editor: TKntRichEdit);
var
   NNode: TNoteNode;

begin
  with Form_Main do begin
     TB_EditUndo.Enabled := Editor.CanUndo;
     TB_EditRedo.Enabled := Editor.CanRedo;
     RTFMUndo.Enabled := TB_EditUndo.Enabled;
  end;

  if CopyFormatMode= cfEnabled then
     EnableCopyFormat(False);

  NNode:= TNoteNode(Editor.NNodeObj);
  if not assigned(NNode) then exit;           // Eg. Scratchpad

  NEntryModified (TNoteEntry(Editor.NEntryObj), NNode.Note, TKntFolder(Editor.FolderObj));
end;


procedure TKntApp.NEntryModified(NEntry: TNoteEntry; Note: TNote; Folder: TKntFolder);
begin
  if ActiveFileIsBusy then exit;

  NEntry.Modified:= true;
  Note.Modified:= true;         // Will also update last modified in note
  Folder.Modified := true;      // => KntFile.Modified := true;
end;


procedure TKntApp.EditorPropertiesModified (Editor: TKntRichEdit);
begin
    if (Editor = ActiveEditor) and (ActiveEditor.Focused or (ActiveFolder.FocusMemory= focRTF)) then
       Self.UpdateEnabledActionsAndRTFState(Editor);
end;

procedure TKntApp.FolderPropertiesModified (Folder: TKntFolder);
begin
    if (Folder = ActiveFolder) and (ActiveFolder.FocusMemory= focTree) then
       Form_Main.EnableActionsForTree(Folder.TreeUI, Folder.ReadOnly);
end;



procedure TKntApp.TreeFocused (Tree: TKntTreeUI);
var
  PrevFolder: TKntFolder;
begin
  if Tree <> ActiveTreeUI then begin
     PrevFolder:= ActiveFolder;
     ActiveTreeUI:= Tree;
     ActiveFolder:= TKntFolder(Tree.Folder);
     ActiveNNode:= ActiveFolder.FocusedNNode;
     ActiveFile:= TKntFile(ActiveFolder.KntFile);
     ActiveEditor:= ActiveFolder.Editor;

     EnsureContentEditorUpdated (ActiveEditor);

     FolderSelected(ActiveFolder, PrevFolder);
     NNodeSelected(ActiveNNode);
  end
  else
     ActiveFolder.FocusMemory:= focTree;

  if ActiveFolder.FocusMemory = focTree then
     Form_Main.EnableActionsForTree(Tree, ActiveFolder.ReadOnly);

  if Form_Main.ShortcutAltDownMenuItem <> nil then
     Form_Main.ShortcutAltDownMenuItem.Enabled:= True;
end;


procedure TKntApp.FolderSelected(Folder: TKntFolder; PrevFolder: TKntFolder);
var
   ModifiedDataStream: TMemoryStream;

begin
   try
      if assigned(Folder) then begin

         if assigned(PrevFolder) then begin
            Form_Main.CheckRestoreAppWindowWidth (true);

            if not _Executing_History_Jump then begin
               AddHistoryLocation (PrevFolder, true);                  // true: add to local history maintaining it's index, and without removing forward history
               _LastMoveWasHistory := false;
            end;
         end;

         Folder.ImagesMode := ImageMng.ImagesMode;

         Form_Main.TAM_ActiveName.Caption := Folder.Name;
         Form_Main.TB_Color.AutomaticColor := Folder.EditorChrome.Font.Color;
      end
      else begin
         Form_Main.TB_Color.AutomaticColor := clWindowText;
         Form_Main.TAM_ActiveName.Caption := GetRS(sApp04);
      end;

   finally
       Form_Main.UpdateFolderDisplay;
       if assigned(Folder) then
          Folder.Editor.CheckWordCount(true);
       if not _Executing_History_Jump then
          UpdateHistoryCommands;

       ShowInfoInStatusBar('');
   end;


   if assigned(FFolderSelected) then
      OnFolderSelected(Folder);
end;


procedure TKntApp.FolderDeleted (Folder: TKntFolder; TabIndex: integer);
begin
   if Folder = ActiveFolder then begin
      ActiveFolder:= nil;
      ActiveNNode:= nil;
      ActiveTreeUI:= nil;
      if ActiveEditor.NNodeObj <> nil then
         ActiveEditor:= nil;
   end;
   ActivateFolder (TabIndex-1);
end;


procedure TKntApp.FileClosed (aFile: TKntFile);
begin
   if aFile = ActiveFile then begin
      try
         ActiveFolder:= nil;
         ActiveNNode:= nil;
         ActiveFile:= nil;
         ActiveTreeUI:= nil;
         ActiveFileIsBusy:= false;

         if AppIsClosing then exit;

         if assigned(ActiveEditor) and (ActiveEditor.NNodeObj <> nil) then begin
            ActiveEditor:= nil;
            with Form_Main do
               if (Pages_Res.ActivePage = ResTab_RTF) and (ResTab_RTF.Visible) then
                  Res_RTF.SetFocus
               else
                  UpdateEnabledActionsAndRTFState(TKntRichEdit(nil));
         end;

         Form_Main.FindTagsIncl:= nil;
         Form_Main.FindTagsExcl:= nil;
         if Form_Main.Ntbk_ResFind.PageIndex = 0 then
            Form_Main.Btn_ResFlipClick(nil);              // Display the search options panel

      except
      end;
   end;
end;


procedure TKntApp.FileNew (aFile: TKntFile);
begin
   ActiveFolder:= nil;
   ActiveNNode:= nil;
   ActiveFile:= aFile;
   ActiveTreeUI:= nil;
   if assigned(ActiveEditor) and (ActiveEditor.NNodeObj <> nil) then
      ActiveEditor:= nil;
end;

procedure TKntApp.FileOpening (aFile: TKntFile);
begin
   ActiveFile:= aFile;
   ActiveFileIsBusy := true;
   AFileIsLoading:= True;
end;

procedure TKntApp.FileOpen (aFile: TKntFile);   // aFile can be nil (file open failed)
begin
   ActiveFile:= aFile;
   ActiveFileIsBusy := false;
   AFileIsLoading:= false;

   if aFile <> nil then begin
      TagMng.ImportTagsFromFile(Tags_FN, True);   // Import common tags (new or modified tags)

      with Form_Main do
         if KeyOptions.ResPanelShow and (Pages_Res.ActivePage = ResTab_Find) then begin
            // Make sure the labels are interpreted according to this file
            CheckTagsField(txtTagsIncl, FindTagsIncl);
            CheckTagsField(txtTagsExcl, FindTagsExcl);
         end;
   end;
end;

procedure TKntApp.TagsUpdated;
begin
   if ActiveFile = nil then begin
      Form_Main.TVTags.RootNodeCount:= 0;
      Form_Main.TVTags.PopupMenu:= nil;
      exit;
   end
   else
      Form_Main.TVTags.PopupMenu:= Form_Main.Menu_Tags;

   if App.TagsState = tsVisible then begin
      ActiveFile.SortNoteTags;
      Form_Main.TVTags.RootNodeCount:= ActiveFile.NoteTags.Count;
      Form_Main.CheckFilterTags;
   end
   else
      App.TagsState := tsPendingUpdate;
end;


class procedure TKntApp.FileSetModified;
begin
    ActiveFile.Modified:= true;
end;

procedure TKntApp.SetEditorZoom( ZoomValue : integer; const ZoomString : string; Increment: integer= 0);
var
  Folder: TKntFolder;
  i: integer;
begin
  if not assigned(ActiveFile) and not assigned(ActiveEditor) then exit;
  if ( _LoadedRichEditVersion < 3 ) then exit; // cannot zoom

  if CtrlDown then begin
     if assigned(ActiveEditor) then
        ActiveEditor.SetZoom (ZoomValue, ZoomString, Increment)
  end
  else begin
     if assigned(ActiveFile) then
        for i := 0 to ActiveFile.Folders.Count -1 do
           ActiveFile.Folders[i].Editor.SetZoom (ZoomValue, ZoomString, Increment);

     Form_Main.Res_RTF.SetZoom (ZoomValue, ZoomString, Increment);
  end;

end;



procedure TKntApp.ShowCurrentZoom (Zoom: integer);
begin
  Form_Main.Combo_Zoom.Text := Format('%d%%', [Zoom] );
end;



procedure TKntApp.ShowInfoInStatusBar(const str: string);
begin
   Form_Main.StatusBar.Panels[PANEL_HINT].Text := str;
end;

procedure TKntApp.WarnEditorIsReadOnly;
begin
   ShowInfoInStatusBar(GetRS(sApp01));
end;

procedure TKntApp.WarnNoTextSelected;
begin
  ShowInfoInStatusBar(GetRS(sApp05));
end;


procedure TKntApp.InfoPopup(const aStr: string);
begin
  PopupMessage(aStr, TMsgDlgType.mtInformation, [mbOK]);
end;

procedure TKntApp.WarningPopup(const aStr: string);
begin
  PopupMessage(aStr, TMsgDlgType.mtWarning, [mbOK]);
end;

procedure TKntApp.ErrorPopup(const aStr: string);
begin
  PopupMessage(aStr, TMsgDlgType.mtError, [mbOK]);
end;


procedure TKntApp.WarnFunctionNotImplemented(const aStr: string);
begin
  WarningPopup(GetRS(sApp03) + aStr);
{$IFDEF KNT_DEBUG}
  Log.Add( 'Not implemented call: ' + aStr );
{$ENDIF}
end;

procedure TKntApp.ErrorPopup(const E: Exception = nil; const Str: string = '');
var
  msg: string;
begin
  if Str = '' then
     msg:= GetRS(sApp07)
  else
     msg:= Str;

  if E <> nil then
     msg:= msg + #13 + E.Message;

  ErrorPopup(msg);
end;

procedure TKntApp.WarnCommandNotImplemented(const aStr: string);
begin
  WarningPopup(GetRS(sApp06) + aStr);
{$IFDEF KNT_DEBUG}
  Log.Add( 'Not implemented call: ' + aStr );
{$ENDIF}
end;



procedure TKntApp.ShowWordCountInfoInStatusBar(const str: string);
begin
   Form_Main.StatusBar.Panels[PANEL_CARETPOS].Text := str;
end;

function TKntApp.GetWordCountInfoInStatusBar: string;
begin
   Result:= Form_Main.StatusBar.Panels[PANEL_CARETPOS].Text;
end;


procedure TKntApp.ShowStatistics;
var
  s: string;
  numChars, numAlpChars, numWords, numNodes : integer;

begin
  if not assigned(ActiveEditor) and not assigned(ActiveFolder) then exit;

  s:= '';
  if assigned(ActiveEditor) then
     s:= ActiveEditor.GetStatistics (numChars, numAlpChars, numWords);

  if assigned(ActiveFolder) then begin
     numNodes := ActiveFolder.TV.TotalCount;
     s := s + Format( GetRS(sApp08),  [numNodes] );
  end;

  App.ShowInfoInStatusBar(Format(GetRS(sApp09), [numChars, numAlpChars, numWords] ));

  if ( MessageDlg( s + GetRS(sApp10), mtInformation, [mbOK,mbCancel], 0 ) = mrOK ) then
      Clipboard.SetTextBuf( Pchar( s ));

end;


procedure TKntApp.ShowTipOfTheDay;
var
  TipDlg : TGFTipDlg;
  wasiconic : boolean;
begin
  if ( not fileexists( TIP_FN )) then begin
    PopupMessage( Format(GetRS(sApp11), [extractfilename( TIP_FN )] ), mtInformation, [mbOK] );
    // turn tips off, so that we don't get this error message
    // every time KeyNote starts. (e.g. if user deleted the .tip file)
    KeyOptions.TipOfTheDay := false;
    exit;
  end;
  wasiconic := ( IsIconic(Application.Handle) = TRUE );
  if wasiconic then
    Application.Restore;
  Application.BringToFront;

  TipDlg := TGFTipDlg.Create( Form_Main );
  try
    with TipDlg do begin
      ShowAtStartup := KeyOptions.TipOfTheDay;
      TipFile := TIP_FN;
      DlgCaption := Program_Name + GetRS(sApp12);
      PanelColor := _GF_CLWINDOW;
      TipFont.Size := 10;
      TipTitleFont.Size := 12;
      SelectedTip := KeyOptions.TipOfTheDayIdx;
      Execute;
      KeyOptions.TipOfTheDayIdx := SelectedTip;
      KeyOptions.TipOfTheDay := ShowAtStartup;
    end;
  finally
    TipDlg.Free;
  end;

  if wasiconic then
    Application.Minimize;

end; // ShowTipOfTheDay


function GetCaptionMessage: string;
begin
   if assigned(ActiveFile) then
      Result:= ExtractFilename(ActiveFile.FileName) + ' - ' + Program_Name
   else
      Result:= Program_Name;
end;

function TKntApp.DoMessageBox (const Str: string; DlgType: TMsgDlgType;
                               const Buttons: TMsgDlgButtons;
                               DefButton: TMsgDlgDefBtn = def1;
                               HelpCtx: Longint = 0; hWnd: HWND= 0): integer;
begin
   Result:= gf_miscvcl.DoMessageBox(Str, GetCaptionMessage, DlgType, Buttons,DefButton, HelpCtx, hWnd);
end;

function TKntApp.PopUpMessage( const Str: string; const mType: TMsgDlgType;
                               const Buttons: TMsgDlgButtons;
                               const DefButton: TMsgDlgDefBtn = def1;
                               const HelpCtx: integer= 0): word;
begin
   Result:= gf_miscvcl.PopUpMessage(Str, GetCaptionMessage, mType, Buttons, DefButton, HelpCtx);
end;

function GetCurrentTreeNode : PVirtualNode;
begin
  result := nil;
  if not assigned(ActiveTreeUI) then exit;
  result:= ActiveTreeUI.FocusedNode;
end;

function GetTreeUI(TV: TVTree): TKntTreeUI;
var
  i: Cardinal;
  Folder: TKntFolder;
begin
  Result:= nil;
  for i := 0 to ActiveFile.Folders.Count-1 do begin
     Folder := ActiveFile.Folders[i];
     if Folder.TV = TV then begin
        Result:= Folder.TreeUI;
        exit;
     end;
  end;
end;


Initialization
    App:= TKntApp.GetInstance;
end.