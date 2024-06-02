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

   TreeNT,

   kn_ImagesMng,
   kn_AlertMng,
   kn_global,
   kn_info,
   kn_const,
   kn_cmd,
   kn_KntFile,
   kn_KntFolder,
   kn_KntNote,
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

   TNoteSelectedEvent = procedure(Note: TKntNote) of object;
   TFolderSelectedEvent = procedure(Folder: TKntFolder) of object;

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
      FNoteSelected:   TNoteSelectedEvent;
      FFolderSelected: TFolderSelectedEvent;

      constructor Create;
      procedure Initialize;

   protected
      procedure UpdateEnabledActionsAndRTFState(Editor: TKntRichEdit);

      procedure EditorSelected (Editor: TKntRichEdit; Focused: boolean);
      procedure NoteSelected(Note: TKntNote);
      procedure FolderSelected(Folder: TKntFolder; PrevFolder: TKntFolder);

      procedure ShowWordCountInfoInStatusBar(const str: string);
      function GetWordCountInfoInStatusBar: string;


   public
      class function GetInstance: TKntApp; static;

      property OnNoteSelected: TNoteSelectedEvent read FNoteSelected write FNoteSelected;
      property OnFolderSelected: TFolderSelectedEvent read FFolderSelected write FFolderSelected;

      procedure EditorFocused (Editor: TKntRichEdit);
      procedure EditorLoaded (Editor: TKntRichEdit);
      procedure ChangeInEditor (Editor: TKntRichEdit);
      procedure NoteModified (Note: TKntNote; Folder: TKntFolder);
      procedure EditorPropertiesModified (Editor: TKntRichEdit);
      procedure SetEditorZoom( ZoomValue : integer; const ZoomString : string; Increment: integer= 0);
      procedure ShowCurrentZoom (Zoom: integer);

      procedure TreeFocused (Tree: TKntTreeUI);
      procedure FolderDeleted (Folder: TKntFolder; TabIndex: integer);
      procedure FolderPropertiesModified (Folder: TKntFolder);

      procedure FileNew (aFile: TKntFile);
      procedure FileClosed (aFile: TKntFile);

      procedure ActivateFolder (Folder: TKntFolder); overload;
      procedure ActivateFolder (TabIndex: Integer); overload;

      property WordCountInfoInStatusBar: string read GetWordCountInfoInStatusBar write ShowWordCountInfoInStatusBar;
      procedure ShowStatistics;
      procedure ShowTipOfTheDay;

      function CheckActiveEditor: boolean;
      function CheckActiveEditorNotReadOnly: boolean;

      procedure ShowInfoInStatusBar(const str: string);
      procedure WarnEditorIsReadOnly;
      procedure WarnNoTextSelected;

      function DoMessageBox(const Str: string; DlgType: TMsgDlgType;
                            const Buttons: TMsgDlgButtons; HelpCtx: Longint = 0; hWnd: HWND= 0): integer;
      function PopUpMessage(const Str: string; const mType: TMsgDlgType;
                            const Buttons: TMsgDlgButtons; const HelpCtx: integer): word;
      procedure InfoPopup(const aStr: string);
      procedure WarningPopup(const aStr: string);
      procedure ErrorPopup(const aStr: string); overload;
      procedure ErrorPopup(const E: Exception = nil; const Str: string = ''); overload;
      procedure WarnFunctionNotImplemented(const aStr: string);
      procedure WarnCommandNotImplemented(const aStr: string);
   end;


  function GetCurrentTreeNode : TTreeNTNode;
  function GetTreeUI(TV: TTreeNT): TKntTreeUI;


var
   App: TKntApp;

   ActiveFile   : TKntFile;
   ActiveFolder : TKntFolder;
   ActiveNote   : TKntNote;
   ActiveEditor : TKntRichEdit;
   ActiveTreeUI:  TKntTreeUI;

   Form_Main:  TForm_Main;
   ImageMng:   TImageMng;
   AlarmMng:   TAlarmMng;
   ClipCapMng: TClipCapMng;

   //================================================ APPLICATION OPTIONS
   // these are declared in kn_Info.pas
   KeyOptions : TKeyOptions; // general program config
   TabOptions : TTabOptions; // options related to tabs, icons etc
   ClipOptions : TClipOptions; // clipboard capture options
   EditorOptions : TEditorOptions;
   ResPanelOptions : TResPanelOptions;
   TreeOptions : TKNTTreeOptions;
   FindOptions : TFindOptions;

   //================================================== DEFAULT PROPERTIES
   DefaultEditorProperties : TFolderEditorProperties;
   DefaultTabProperties : TFolderTabProperties;
   DefaultEditorChrome : TChrome;
   DefaultTreeChrome : TChrome;
   DefaultTreeProperties : TFolderTreeProperties;


implementation
uses
   GFTipDlg,
   gf_misc,
   gf_miscvcl,
   kn_MacroMng,
   kn_VCLControlsMng,
   kn_LinksMng,
   kn_FindReplaceMng,
   kn_NoteFileMng;

resourcestring
  STR_01 = ' Cannot perform operation: Editor is Read-Only';
  STR_02 = 'There is no active editor';
  STR_10 = 'Function not implemented. ';
  STR_15 = '(none)';
  STR_16 = ' Select some text before issuing this command.';
  STR_50 = 'Unexpected or not implemented command: ';
  STR_80 = 'Unexpected error. ';
  STR_Stat_05 = #13#13+'Number of nodes (notes) in tree: %d';
  STR_Stat_06 = 'Chars: %d  Alph: %d  Words: %d';
  STR_Stat_07 = #13#13+'Clik OK to copy information to clipboard.';
  STR_Tip_01 = 'Cannot display Tip of the Day: file "%s" not found.';
  STR_Tip_02 = ': Tip of the Day';



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
end;


function TKntApp.CheckActiveEditor: boolean;
begin
    Result:= False;
    if not assigned(ActiveEditor) then begin
       ShowInfoInStatusBar(STR_02);
       exit;
    end;

    Result:= True;
end;

function TKntApp.CheckActiveEditorNotReadOnly: boolean;
begin
    Result:= False;
    if not assigned(ActiveEditor) then begin
       ShowInfoInStatusBar(STR_02);
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
          Folder:= ActiveFile.GetFolderByID(ActiveFile.ActiveFolderID);
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
       end;
    end;
end;


procedure TKntApp.UpdateEnabledActionsAndRTFState(Editor: TKntRichEdit);
var
  Edit_PlainText, Edit_SupportsImages, Edit_SupportsRegImages, Edit_NoteObj: boolean;

begin
  if (Editor = nil) or not Editor.Enabled then begin
      Edit_PlainText:= true;
      Edit_SupportsImages:= false;
      Edit_SupportsRegImages:= false;
      Edit_NoteObj:= false;
  end
  else begin
      Edit_PlainText:= Editor.PlainText or Editor.ReadOnly;
      Edit_SupportsImages:= Editor.SupportsImages;
      Edit_SupportsRegImages:= Editor.SupportsRegisteredImages;
      Edit_NoteObj:= (Editor.NoteObj <> nil);
  end;

  Form_Main.EnableActionsForEditor(not Edit_PlainText);
  Form_Main.EnableActionsForEditor(Edit_NoteObj, Edit_SupportsImages, Edit_SupportsRegImages);
  Form_Main.RxChangedSelection(Editor, true);
  Form_Main.UpdateWordWrap;
  ClipCapMng.ShowState;
end;



procedure TKntApp.EditorFocused (Editor: TKntRichEdit);
begin
   EditorSelected(Editor, true);
end;

procedure TKntApp.EditorLoaded (Editor: TKntRichEdit);
begin
   if not assigned(Editor) then exit;

   if (Editor.FolderObj = ActiveFolder) then
      EditorSelected(Editor, false);
end;


procedure TKntApp.EditorSelected (Editor: TKntRichEdit; Focused: boolean);
var
   OldNote: TKntNote;
   OldFolder: TKntFolder;
begin
    ActiveEditor:= Editor;

    if Editor.Focused then begin
       UpdateEnabledActionsAndRTFState(Editor);
       ShowCurrentZoom(Editor.GetZoom);
       Editor.UpdateCursorPos;
    end;

    if assigned(Editor.NoteObj) then begin
       OldFolder:= ActiveFolder;
       OldNote:= ActiveNote;

       ActiveNote:= TKntNote(Editor.NoteObj);
       ActiveFolder:= TKntFolder(Editor.FolderObj);
       ActiveFile:= TKntFile(Editor.FileObj);
       ActiveTreeUI:= nil;
       if ActiveFolder <> nil then
          ActiveTreeUI:= ActiveFolder.TreeUI;

       if Focused then
          ActiveFolder.FocusMemory:= focRTF;

       if OldFolder <> ActiveFolder then
          FolderSelected(ActiveFolder, OldFolder);
       if OldNote <> ActiveNote then
          NoteSelected(ActiveNote);
    end;
end;


procedure TKntApp.NoteSelected(Note: TKntNote);
begin
   if assigned(Note) then begin
      if ActiveFolder.FocusMemory = focTree then
         Form_Main.ShowNodeChromeState (ActiveFolder.TreeUI);
   end
   else
      Self.UpdateEnabledActionsAndRTFState(TKntRichEdit(nil));

   if assigned(FNoteSelected) then
      OnNoteSelected(Note);
end;


procedure TKntApp.ChangeInEditor (Editor: TKntRichEdit);
var
   Note: TKntNote;
begin
  with Form_Main do begin
     TB_EditUndo.Enabled := Editor.CanUndo;
     TB_EditRedo.Enabled := Editor.CanRedo;
     RTFMUndo.Enabled := TB_EditUndo.Enabled;
  end;

  if CopyFormatMode= cfEnabled then
     EnableCopyFormat(False);

  Note:= TKntNote(Editor.NoteObj);
  if not assigned(Note) then exit;           // Eg. Scratchpad

  if not Note.RTFModified then
     NoteModified(Note, TKntFolder(Editor.FolderObj));
end;


procedure TKntApp.NoteModified(Note: TKntNote; Folder: TKntFolder);
begin
  Note.RTFModified:= true;
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
     ActiveNote:= ActiveFolder.SelectedNote;
     ActiveFile:= TKntFile(ActiveFolder.KntFile);
     ActiveEditor:= ActiveFolder.Editor;

     FolderSelected(ActiveFolder, PrevFolder);
     NoteSelected(ActiveNote);
  end
  else
     ActiveFolder.FocusMemory:= focTree;

  if ActiveFolder.FocusMemory = focTree then
     Form_Main.EnableActionsForTree(Tree, ActiveFolder.ReadOnly);

end;


procedure TKntApp.FolderSelected(Folder: TKntFolder; PrevFolder: TKntFolder);
var
   ModifiedDataStream: TMemoryStream;

begin
   try
      if assigned(Folder) then begin

         if assigned(PrevFolder) then begin
            Form_Main.CheckRestoreAppWindowWidth (true);
            ModifiedDataStream:= PrevFolder.EditorToDataStream;   // If its Editor is not modified it will do nothing. Necessary to ensure that changes are seen among mirror nodes

            if not _Executing_History_Jump then begin
               AddHistoryLocation (PrevFolder, true);                  // true: add to local history maintaining it's index, and without removing forward history
               _LastMoveWasHistory := false;
            end;
         end;

         if assigned(ActiveNote) and (ActiveNote.Stream = ModifiedDataStream) then
            Folder.DataStreamToEditor;
         Folder.ImagesMode := ImageMng.ImagesMode;

         Form_Main.TAM_ActiveName.Caption := Folder.Name;
         Form_Main.TB_Color.AutomaticColor := Folder.EditorChrome.Font.Color;
      end
      else begin
         Form_Main.TB_Color.AutomaticColor := clWindowText;
         Form_Main.TAM_ActiveName.Caption := STR_15;
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
      ActiveNote:= nil;
      ActiveTreeUI:= nil;
      if ActiveEditor.NoteObj <> nil then
         ActiveEditor:= nil;
   end;
   ActivateFolder (TabIndex-1);
end;


procedure TKntApp.FileClosed (aFile: TKntFile);
begin
   if aFile = ActiveFile then begin
      ActiveFolder:= nil;
      ActiveNote:= nil;
      ActiveFile:= nil;
      ActiveTreeUI:= nil;
      if assigned(ActiveEditor) and (ActiveEditor.NoteObj <> nil) then begin
         ActiveEditor:= nil;
         with Form_Main do
            if (Pages_Res.ActivePage = ResTab_RTF) and (ResTab_RTF.Visible) then
               Res_RTF.SetFocus
            else
               UpdateEnabledActionsAndRTFState(TKntRichEdit(nil));
      end;
   end;
end;


procedure TKntApp.FileNew (aFile: TKntFile);
begin
   ActiveFolder:= nil;
   ActiveNote:= nil;
   ActiveFile:= aFile;
   ActiveTreeUI:= nil;
   if assigned(ActiveEditor) and (ActiveEditor.NoteObj <> nil) then
      ActiveEditor:= nil;
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
   ShowInfoInStatusBar(STR_01);
end;

procedure TKntApp.WarnNoTextSelected;
begin
  ShowInfoInStatusBar(STR_16);
end;


procedure TKntApp.InfoPopup(const aStr: string);
begin
  PopupMessage(aStr, TMsgDlgType.mtInformation, [mbOK], 0);
end;

procedure TKntApp.WarningPopup(const aStr: string);
begin
  PopupMessage(aStr, TMsgDlgType.mtWarning, [mbOK], 0);
end;

procedure TKntApp.ErrorPopup(const aStr: string);
begin
  PopupMessage(aStr, TMsgDlgType.mtError, [mbOK], 0);
end;


procedure TKntApp.WarnFunctionNotImplemented(const aStr: string);
begin
  WarningPopup(STR_10 + aStr);
{$IFDEF KNT_DEBUG}
  Log.Add( 'Not implemented call: ' + aStr );
{$ENDIF}
end;

procedure TKntApp.ErrorPopup(const E: Exception = nil; const Str: string = '');
var
  msg: string;
begin
  if Str = '' then
     msg:= STR_80
  else
     msg:= Str;

  if E <> nil then
     msg:= msg + #13 + E.Message;

  ErrorPopup(msg);
end;

procedure TKntApp.WarnCommandNotImplemented(const aStr: string);
begin
  WarningPopup(STR_50 + aStr);
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
     numNodes := ActiveFolder.TV.Items.Count;
     s := s + Format( STR_Stat_05,  [numNodes] );
  end;

  App.ShowInfoInStatusBar(Format(STR_Stat_06, [numChars, numAlpChars, numWords] ));

  if ( MessageDlg( s + STR_Stat_07, mtInformation, [mbOK,mbCancel], 0 ) = mrOK ) then
      Clipboard.SetTextBuf( Pchar( s ));

end;


procedure TKntApp.ShowTipOfTheDay;
var
  TipDlg : TGFTipDlg;
  wasiconic : boolean;
begin
  if ( not fileexists( TIP_FN )) then begin
    PopupMessage( Format(STR_Tip_01, [extractfilename( TIP_FN )] ), mtInformation, [mbOK], 0 );
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
      DlgCaption := Program_Name + STR_Tip_02;
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
                               const Buttons: TMsgDlgButtons; HelpCtx: Longint = 0; hWnd: HWND= 0): integer;
begin
   Result:= gf_miscvcl.DoMessageBox(Str, GetCaptionMessage, DlgType, Buttons,0, hWnd);
end;

function TKntApp.PopUpMessage( const Str: string; const mType: TMsgDlgType;
                               const Buttons: TMsgDlgButtons; const HelpCtx: integer): word;
begin
   Result:= gf_miscvcl.PopUpMessage(Str, GetCaptionMessage, mType, Buttons, HelpCtx);
end;

function GetCurrentTreeNode : TTreeNTNode;
begin
  result := nil;
  if not assigned(ActiveTreeUI) then exit;
  result:= ActiveTreeUI.SelectedNode;
end;

function GetTreeUI(TV: TTreeNT): TKntTreeUI;
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