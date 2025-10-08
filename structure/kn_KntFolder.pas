unit kn_KntFolder;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2024 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

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
   System.DateUtils,
   Vcl.Graphics,
   Vcl.FileCtrl,
   Vcl.Controls,
   Vcl.ComCtrls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.ExtCtrls,
   comctrls95,
   VirtualTrees,

   RxRichEd,

   gf_streams,
   gf_misc,
 {$IFDEF KNT_DEBUG}
   GFLog,
 {$ENDIF}
   kn_StyleObj,
   kn_Info,
   kn_Const,
   knt.ui.info,
   knt.ui.editor,
   knt.ui.tree,
   knt.model.note
   {$IFDEF WITH_IE}
   ,SHDocVw_TLB
   {$ENDIF}
   ;


type
  EKntFolderError = class( Exception );


type
  TBeforeEditorLoadedEvent = procedure(Note: TNote) of object;
  TAfterEditorLoadedEvent  = procedure(Note: TNote) of object;

  TKntFolder = class;
  TFolderList = TSimpleObjList<TKntFolder>;
  TKNTHistoryObj = TObject;            // To avoid circular references


  // [*] -> Folder properties saved to .knt

  TKntFolder = class( TPersistent )
  private
    FModified : boolean;

    FID : Cardinal;               // [*] unique folder ID
    FKntFile: TObject;
    fNNodes: TNoteNodeList;

    FEditorChrome : TChrome;      // [*] user-defined fonts, colors etc.
    FName : TNoteNameStr;         // [*] user-defined name for the folder object
    FVisible : boolean;           // [*] UNUSED but may be useful later (with specialized folders)
    FReadOnly : boolean;          // [*]
    FDateCreated : TDateTime;     // [*]
    FImageIndex : integer;        // [*]
    FFocusMemory : TFocusMemory;  // [*] which control was last focused

    FDefaultPlainText : boolean;   // [*] if true, contents of new created notes (or new entries of existing notes) are saved as plain text
    FWordWrap : boolean;           // [*]
    FURLDetect : boolean;          // [*] highlight URLs
    FTabSize : byte;               // [*]
    FUseTabChar : boolean;         // [*]
    FRTL : boolean;                // [*]

    FIsInsertMode : boolean;
    FInfo : Byte;                 // internal use only

    { I don't use something like TBytes because I don't want to depend on the number of nodes token being incorrect,
      and this makes it easier to dynamically add the different elements. Additionally, it will also be used when
      loading old files, where that token was not used and I could not preset the capacity }
    FLoadingLevels: TIntegerList;


    // VCL controls
    FTabSheet : TTab95Sheet;          // the tabsheet which holds this folder
    FTreeUI: TKntTreeUI;
    FNoteUI: INoteUI;
    FSplitter : TSplitter;
    FTV : TVTree;

    FHistory : TKNTHistoryObj;         // Folder (local) history
    FImagesMode: TImagesMode;
    fImagesReferenceCount: TImageIDs;

    FIconKind : TNodeIconKind;         // [*]
    FTreeChrome : TChrome;             // [*]
    FTreeHidden : boolean;             // [*]
    FEditorInfoPanelHidden: boolean;   // [*]
    FHideCheckedNodes: boolean;        // [*]
    FFiltered: boolean;                // [*]
    FPosDateCol: integer;              // [*]
    FPosFlaggedCol: integer;           // [*]

    FSavedSelectedIndex : integer;     // [*]
    FSavedTabIndex : integer;          // [*]

    FDefaultNoteName : string;         // [*]
    //FAutoNumberNotes : boolean;      // Information is currently saved at the NNodes level
    FCheckboxes : boolean;             // [*] All Checkboxes
    FVerticalLayout : boolean;         // [*]
    FTagSelectorDisabled: boolean;     // [*]

    // state that needs to be recalled
    FTreeWidth : integer;              // [*]
    FTreeMaxWidth: integer;            // [*]
    //-----------


    procedure SetName( AName : TNoteNameStr );
    procedure SetID( ID : Cardinal );
    procedure SetReadOnly( AReadOnly : boolean );
    procedure SetRTL( RTL : boolean );
    procedure SetDefaultPlainText( APlainText : boolean );
    function GetTabIndex: integer;
    procedure SetTabIndex( ATabIndex : integer );
    procedure SetModified( AModified : boolean );
    function  GetModified : boolean;
    procedure SetImageIndex( AImgIdx : integer );
    procedure SetEditorChrome( AChrome : TChrome );
    procedure SetTabSheet( ATabSheet : TTab95Sheet );
    procedure SetEditorInfoPanelHidden(value: boolean);

    procedure SetWordWrap( AWordWrap : boolean );
    procedure SetURLDetect( AURLDetect : boolean );
    procedure SetTabSize( ATabSize : byte );
    procedure SetNoteUI( ANoteUI : INoteUI );
    function GetEditor: TKntRichEdit;

    procedure SetImagesMode(ImagesMode: TImagesMode); overload;

    procedure SetTreeUI(tree: TKntTreeUI);
    procedure SetTreeChrome( AChrome : TChrome );

    procedure UpdateLoadingLevels;

  public
    class function NewKntFolder(const DefaultFolder, CanFocus : boolean) : boolean;
    class procedure CreateNewKntFolder;
    class procedure DeleteKntFolder;
    class procedure RenameKntFolder;
    class procedure EditKntFolderProperties( const PropertiesAction : TPropertiesAction );

    property ID : Cardinal read FID write SetID;
    property Name : string read fName write SetName;
    property NNodes: TNoteNodeList read fNNodes;

    property EditorChrome : TChrome read FEditorChrome write SetEditorChrome;
    property ImageIndex : integer read FImageIndex write SetImageIndex;
    property Visible : boolean read FVisible write FVisible;
    property ReadOnly : boolean read FReadOnly write SetReadOnly;
    property Modified : boolean read GetModified write SetModified;
    property DateCreated : TDateTime read FDateCreated write FDateCreated;
    property Info : Byte read FInfo write FInfo;

    property History : TKNTHistoryObj read FHistory;

    property RTL : boolean read FRTL write SetRTL;
    property DefaultPlainText : boolean read FDefaultPlainText write SetDefaultPlainText;
    property WordWrap : boolean read FWordWrap write SetWordWrap;
    property URLDetect : boolean read FURLDetect write SetURLDetect;
    property TabSize : byte read FTabSize write SetTabSize;
    property UseTabChar : boolean read FUseTabChar write FUseTabChar;
    property TagSelectorDisabled : boolean read FTagSelectorDisabled write FTagSelectorDisabled;
    property IsInsertMode : boolean read FIsInsertMode write FIsInsertMode;
    property FocusMemory : TFocusMemory read FFocusMemory write FFocusMemory;

    property IconKind : TNodeIconKind read FIconKind write FIconKind;
    property TreeWidth : integer read FTreeWidth write FTreeWidth;
    property TreeMaxWidth : integer read FTreeMaxWidth write FTreeMaxWidth;
    property HideCheckedNodes: Boolean read FHideCheckedNodes write FHideCheckedNodes;
    property PosDateCol: integer read FPosDateCol write FPosDateCol;
    property PosFlaggedCol: integer read FPosFlaggedCol write FPosFlaggedCol;
    property Filtered: Boolean read FFiltered write FFiltered;
    property SavedSelectedIndex : integer read FSavedSelectedIndex write FSavedSelectedIndex;
    property Checkboxes : boolean read FCheckboxes write FCheckboxes;
    property TreeChrome : TChrome read FTreeChrome write SetTreeChrome;
    property DefaultNoteName : string read FDefaultNoteName write FDefaultNoteName;
    //property AutoNumberNodes : boolean read FAutoNumberNotes write FAutoNumberNotes;
    property VerticalLayout : boolean read FVerticalLayout write FVerticalLayout;
    property TreeHidden : boolean read FTreeHidden write FTreeHidden;
    property EditorInfoPanelHidden : boolean read FEditorInfoPanelHidden write SetEditorInfoPanelHidden;

    property Editor : TKntRichEdit read GetEditor;

    property TreeUI: TKntTreeUI read fTreeUI write SetTreeUI;
    property NoteUI: INoteUI read fNoteUI write SetNoteUI;
    property KntFile: TObject read FKntFile;
    property TabIndex : integer read GetTabIndex write SetTabIndex;
    property TabSheet : TTab95Sheet read FTabSheet write SetTabSheet;
    property Splitter : TSplitter read FSplitter write FSplitter;
    property TV : TVTree read FTV write FTV;

    constructor Create(KntFile: TObject);
    destructor Destroy; override;


    procedure LoadEditorFromNNode(NNode: TNoteNode; SavePreviousContent: boolean);
    procedure ReloadEditorFromDataModel(SavePreviousContent: boolean= true);
    function  SaveEditorToDataModel: TMemoryStream;
    procedure SetEditorProperties( const aProps : TFolderEditorProperties );
    procedure GetEditorProperties( var aProps : TFolderEditorProperties );
    procedure SetFocusOnNoteEditor;

  public
    procedure UpdateTree;
    procedure UpdateTabSheet;
    procedure SetTabProperties( const aProps : TFolderTabProperties; UpdateName: boolean= True );
    procedure GetTabProperties( var aProps : TFolderTabProperties );
    procedure SetTreeProperties( const aProps : TFolderTreeProperties );
    procedure GetTreeProperties( var aProps : TFolderTreeProperties );
    procedure SetFocusOnTree;

    function GetNNode(Node : PVirtualNode): TNoteNode;
    function GetNNodeByGID(const aGID : Cardinal): TNoteNode;
    function GetNNodeByID(const aID : Word): TNoteNode;
    function GetNNodeByNoteName(const aName : string): TNoteNode;
    function AddNNode(NNode: TNoteNode): integer;
    function AddNewNote(CopyFromNNode: TNoteNode = nil; InheritFromNode: PVirtualNode = nil): TNoteNode;
    function AddNewNNode(Note: TNote; CopyFromNNode: TNoteNode = nil): TNoteNode;
    function DeleteNNode(NNode: TNoteNode): integer;
    function RemoveNNode(NNode: TNoteNode): integer;
    procedure NoNodeInTree;
    procedure NodeSelected(const Node : PVirtualNode; const LastNodeSelected: PVirtualNode);
    function GetFocusedNNode : TNoteNode;
    property FocusedNNode : TNoteNode read GetFocusedNNode;

    procedure NoteNameModified(NNode: TNoteNode);

    function GetImagesInstances: TImageIDs;
    property ImagesMode: TImagesMode read FImagesMode write SetImagesMode;
    property ImagesInstances: TImageIDs read GetImagesInstances;
    function CheckSavingImagesOnMode (ImagesMode: TImagesMode; Stream: TMemoryStream; ExitIfAllImagesInSameModeDest: boolean = true): TImageIDs;
    procedure ReconsiderImageDimensionGoalsOnEditor (Selection: boolean);
    procedure SetImagesMode(ImagesMode: TImagesMode; ForceMode: boolean); overload;


    procedure LoadFromFile( var tf : TTextFile; var FileExhausted : boolean; var NextBlock: TNextBlock; LoadOldSimpleNote: boolean= false);
    procedure LoadFromTreePadFile( const FN : string );
    function SaveToFile( var tf : TTextFile;  OnlyCurrentNodeAndSubtree: PVirtualNode= nil;
                         OnlyNotHiddenNodes: boolean= false; OnlyCheckedNodes: boolean= false): integer;
    procedure GetNotesToBeSaved(NoteList: TNoteList;
                                OnlyCurrentNodeAndSubtree: PVirtualNode= nil;
                                OnlyNotHiddenNodes: boolean= false;
                                OnlyCheckedNodes: boolean= false );

    property LoadingLevels: TIntegerList read FLoadingLevels;
  protected
    function PropertiesToFlagsString : TFlagsString;
    procedure FlagsStringToProperties( const FlagsStr : TFlagsString );
    procedure NoteFlagsStringToProperties(const FlagsStr : TFlagsString; NNode: TNoteNode);     // To process old .knt files


  public
    function CheckVirtualNote (Node: PVirtualNode): boolean;
    procedure VirtualNoteRefresh(const DoPrompt : boolean);
    procedure VirtualNoteUnlink;
    procedure VirtualNoteProc(VirtFN : string );


    function InitializeTextPlainVariables( nMax: integer; RTFAux: TAuxRichEdit ): boolean;
    function PrepareTextPlain (NNode: TNoteNode; RTFAux: TAuxRichEdit; ClearRTFAux: boolean= False): string;

  end; // TKntFolder

  function InitializeTextPlain(NEntry: TNoteEntry; RTFAux: TAuxRichEdit): boolean;
  procedure LoadStreamInRTFAux(Stream: TMemoryStream; RTFAux: TAuxRichEdit); forward;

implementation
uses
   gf_strings,
   gf_miscvcl,
   gf_files,
   kn_global,
   kn_AlertMng,
   kn_LinksMng,
   kn_LocationObj,
   kn_Main,
   kn_Macro,
   kn_EditorUtils,
   kn_Defaults,
   kn_RTFUtils,
   kn_ImagesMng,
   kn_NoteFileMng,
   kn_KntFolder_New,
   kn_MacroMng,
   kn_ConfigMng,
   kn_VCLControlsMng,
   kn_FileMgr,
   kn_History,
   kn_KntFile,
   knt.App,
   knt.RS
   ;



// =========================================================================================================
//     TKntFolder - Class methods
// =========================================================================================================

class function TKntFolder.NewKntFolder(const DefaultFolder, CanFocus : boolean) : boolean;
var
  myFolder: TKntFolder;
  Form_NewNote : TForm_NewKntFolder;
  FileWasBusy, TimerWasEnabled : boolean;
begin
  result := false;
  with Form_Main do begin
    if ( not HaveKntFolders( true, false )) then exit;
    myFolder := nil;
    FileWasBusy := ActiveFileIsBusy;
    ActiveFile.IsBusy := true;
    StatusBar.Panels[PANEL_HINT].Text := '';
    TimerWasEnabled := Timer.Enabled;
    Timer.Enabled := false;
  end;
  try
    try
      if DefaultFolder then begin
        myFolder := TKntFolder.Create (ActiveFile);
        myFolder.SetEditorProperties( DefaultEditorProperties );
        myFolder.SetTabProperties( DefaultTabProperties );
        myFolder.EditorChrome := DefaultEditorChrome;
        myFolder.SetTreeProperties( DefaultTreeProperties );
        myFolder.TreeChrome := DefaultTreeChrome;
      end
      else begin
        Form_NewNote := TForm_NewKntFolder.Create( Form_Main );
        try
          with Form_NewNote do begin
            ShowHint := KeyOptions.ShowTooltips;
            myEditorProperties := DefaultEditorProperties;
            myTabProperties := DefaultTabProperties;
            myChrome :=  DefaultEditorChrome;
            myTabNameHistory := KeyOptions.TabNameHistory;
            myNodeNameHistory := KeyOptions.NodeNameHistory;
            myHistoryCnt := FindOptions.HistoryMaxCnt;
            TAB_CHANGEABLE := true;
            myTreeProperties := DefaultTreeProperties;
            myTreeChrome := DefaultTreeChrome;
            myTreeOptions := KntTreeOptions;
          end;
          if ( Form_NewNote.ShowModal = mrOK ) then begin
            KeyOptions.TabNameHistory := Form_NewNote.myTabNameHistory;
            myFolder := TKntFolder.Create (ActiveFile);
            myFolder.SetEditorProperties( Form_NewNote.myEditorProperties );
            myFolder.SetTabProperties( Form_NewNote.myTabProperties );
            myFolder.EditorChrome := Form_NewNote.myChrome;
            KeyOptions.NodeNameHistory := Form_NewNote.myNodeNameHistory;
            with myFolder do begin
              SetTreeProperties( Form_NewNote.myTreeProperties );
              TreeChrome := Form_NewNote.myTreeChrome;
            end;
            if ActiveFolder <> nil then
               myFolder.FEditorInfoPanelHidden:= ActiveFolder.FEditorInfoPanelHidden;
          end;
        finally
          Form_NewNote.Free;
        end;
      end;

      if assigned( myFolder ) then begin
        ActiveFile.AddFolder( myFolder );
        App.ShowInfoInStatusBar(GetRS(sFld21));
        try
          with Form_Main do begin
             CreateVCLControlsForFolder( myFolder );
             SetUpVCLControls( myFolder );
             AddToFileManager( ActiveFile.FileName, ActiveFile ); // update manager (number of notes has changed)
          end;

        finally
          ActiveFile.Modified := ( not DefaultFolder );
          TTab95Sheet(myFolder.TabSheet).TabVisible := true; // was created hidden
        end;

        myFolder.TreeUI.NewNode(tnTop, nil, '', true );

        App.ActivateFolder(myFolder);

      end;

    except
      on E : Exception do
      begin
        {$IFDEF KNT_DEBUG}
         Log.Add( 'Exception in NewKntFolder: ' + E.Message );
        {$ENDIF}
         App.ErrorPopup(E);
      end;
    end;

  finally
    Form_Main.Timer.Enabled := TimerWasEnabled;
    ActiveFile.IsBusy := FileWasBusy;
    ActiveFile.Modified := true;
    result := assigned( myFolder );
   {$IFDEF KNT_DEBUG}
    if assigned( myFolder ) then
      Log.Add( 'Added new folder: ' + myFolder.Name )
    else
      Log.Add( 'New folder NOT added.' );
   {$ENDIF}
  end;
end; // NewKntFolder


class procedure TKntFolder.DeleteKntFolder;
var
  TabIdx : integer;
  Folder: TKntFolder;
begin
  with Form_Main do begin
      if ( not HaveKntFolders( true, true )) then exit;
      Folder:= ActiveFolder;
      if ( not assigned( Folder )) then exit;
      if FolderIsReadOnly( Folder, true ) then exit;

      if KeyOptions.ConfirmTabDelete then begin
	       if (App.DoMessageBox( Format(GetRS(sFld22), [RemoveAccelChar( Folder.Name )]), mtWarning, [mbYes, mbNo], def2 ) <> mrYes ) then
            exit;
      end;

      try
        TabIdx := Folder.TabSheet.TabIndex;
        DestroyVCLControlsForFolder(Folder, true);
        ActiveFile.DeleteFolder(Folder);
        AddToFileManager( ActiveFile.FileName, ActiveFile ); // update manager (number of notes has changed)

      finally
          App.FolderDeleted(Folder, TabIdx);
          App.ShowInfoInStatusBar(GetRS(sFld24));
      end;
  end;
end; // DeleteKntFolder


class procedure TKntFolder.CreateNewKntFolder;
begin
  if assigned(ActiveFolder) then
     ActiveFolder.SaveEditorToDataModel;
  if TKntFolder.NewKntFolder( false, true ) then begin
    Application.ProcessMessages;
    if KeyOptions.RunAutoMacros then
       ExecuteMacro( _MACRO_AUTORUN_NEW_TREE, '' );
  end;
end; // CreateNewKntFolder


class procedure TKntFolder.RenameKntFolder;
var
  Form_NewNote : TForm_NewKntFolder;
begin
  with Form_Main do begin
      if ( not HaveKntFolders( true, true )) then exit;
      if ( not assigned( ActiveFolder )) then exit;
      if FolderIsReadOnly(ActiveFolder, true) then exit;

      Form_NewNote := TForm_NewKntFolder.Create( Form_Main );
      try
        with Form_NewNote do begin
          TAB_CHANGEABLE := false;
          ShowHint := KeyOptions.ShowTooltips;
          myTabProperties.Name := ActiveFolder.Name;
          myTabProperties.ImageIndex := ActiveFolder.ImageIndex;
          myTabNameHistory := KeyOptions.TabNameHistory;
          myHistoryCnt := FindOptions.HistoryMaxCnt;
          Button_Properties.Enabled := false;
          Button_Properties.Visible := false;
        end;
        if ( Form_NewNote.ShowModal = mrOK ) then begin
          KeyOptions.TabNameHistory := Form_NewNote.myTabNameHistory;
          ActiveFile.Modified := true;
          ActiveFolder.Name := Form_NewNote.myTabProperties.Name;
          ActiveFolder.ImageIndex := Form_NewNote.myTabProperties.ImageIndex;
          StatusBar.Panels[PANEL_HINT].Text := GetRS(sFld25);
        end;

      finally
        Form_NewNote.Free;
        ActiveFile.Modified := true;
      end;
  end;
end; // RenameKntFolder


class procedure TKntFolder.EditKntFolderProperties( const PropertiesAction : TPropertiesAction );
var
  Form_Defaults : TForm_Defaults;
  i : integer;
  TreeLayoutChanged : boolean;
  oldIconKind : TNodeIconKind;
  oldShowCheckboxes : boolean;
  oldHideChecked: boolean;      // [dpv]
  oldPlainText: boolean;
  EnsuredPlainText: boolean;
  myFile:   TKntFile;
  myFolder: TKntFolder;
  F: TKntFolder;
  NewPropertiesAction : TPropertiesAction;
  TreeUI: TKntTreeUI;

begin
  myFile:= ActiveFile;
  myFolder:= ActiveFolder;

  with Form_Main do begin
      if (PropertiesAction = propThisFolder ) and (not assigned(myFolder)) then exit;

      TreeLayoutChanged := false;

      Form_Defaults := TForm_Defaults.Create( Form_Main );

      oldIconKind := niStandard;
      oldShowCheckboxes := false;

      try

        with Form_Defaults do begin
          ShowHint := KeyOptions.ShowTooltips;
          Action := PropertiesAction;
          DefaultsFN := DEF_FN;
          myTabNameHistory := KeyOptions.TabNameHistory;
          myNoteIsReadOnly := (( PropertiesAction = propThisFolder ) and FolderIsReadOnly( myFolder, false ));

          myNodeNameHistory := KeyOptions.NodeNameHistory;

          myCurrentFileName:= '';
          if assigned(myFile) and (myFile.FileName <> '') then
             myCurrentFileName := ExtractFilename( myFile.FileName );


          case PropertiesAction of
            propThisFolder : begin

              myEditorChrome := myFolder.EditorChrome;
              myFolder.GetTabProperties( myTabProperties );
              myFolder.GetEditorProperties( myEditorProperties );

              myEditorProperties.DefaultZoom:= DefaultEditorProperties.DefaultZoom;    // Just for show it

              // [x] bug workaround: despite the fact that RichEdit has
              // protection against losing styles in RecreateWnd, changing
              // wordwrap HERE still causes all font styles to be lost.
              // I have no time for investigating this anymore, so the
              // wordwrap option is disabled here. It's not needed here
              // anyway, since you can just press Ctrl+W in the editor,
              // which is more convenient and does NOT cause the
              // font style loss.
              {  *1
                 Disabling and hiding the combo did not solve the problem: pressing Ok from the properties
                 of a folder after the state of WordWrap have been changed (in the active node or simply for
                 selecting nodes with different WodWrap state) always caused formatting to be lost. Since version 1.6.5...

              CB_WordWrap.Enabled := false;
              CB_WordWrap.Visible := false;
              }
              CB_WordWrap.Checked:= myEditorProperties.WordWrap;    // *1


              with myFolder do begin

                myInheritBGColor:= KntTreeOptions.InheritNodeBG;
                if KntTreeOptions.InheritNodeBG  and assigned(FocusedNNode) then
                   myEditorChrome.BGColor := GetColor(FocusedNNode.EditorBGColor, EditorChrome.BGColor);

                myTreeChrome := TreeChrome;
                GetTreeProperties( myTreeProperties );
                StartWithEditorTab := ( not TreeUI.Focused );

                oldIconKind := myTreeProperties.IconKind;
                oldShowCheckboxes := myTreeProperties.CheckBoxes;
                oldHideChecked := myTreeProperties.HideChecked;       // [dpv]
              end;
              oldPlainText := ActiveFolder.DefaultPlainText;
            end;

            propDefaults : begin
              StartWithEditorTab := true;

              myEditorChrome := DefaultEditorChrome;
              myTabProperties := DefaultTabProperties;
              myEditorProperties := DefaultEditorProperties;

              // this picks the BG color of the current node,
              // rather than DEFAULT BG color for whole folder
              if myCurrentFileName <> '' then
                 mySaveFileDefaults := ( DEF_FN <> OrigDEF_FN );

              myTreeChrome := DefaultTreeChrome;
              myTreeProperties := DefaultTreeProperties;
            end;

          end;
        end;

        if ( Form_Defaults.ShowModal = mrOK ) then begin

          with Form_Defaults do begin
            NewPropertiesAction:= Action;        // User can now select the check 'Save as Defaults'

            KeyOptions.TabNameHistory := myTabNameHistory;
            KeyOptions.NodeNameHistory := myNodeNameHistory;

            TreeUI:= myFolder.TreeUI;

            if (PropertiesAction = propThisFolder) and not myNoteIsReadOnly  then begin
                myFolder.Modified:= True;

                myFolder.SetTabProperties( myTabProperties, not (NewPropertiesAction = propDefaults));
                myFolder.SetEditorProperties( myEditorProperties );
                myFolder.EditorChrome := myEditorChrome;

                // reflect changes in controls
                UpdateEditor (myFolder.Editor, myFolder, true);
                myFolder.UpdateTabSheet;
                Form_Main.TB_Color.AutomaticColor := myEditorChrome.Font.Color;      // 'Default' color in Color picker control


                with myFolder do begin
                  // this will apply the selected BG color to current NODE
                  // besides setting the new default BG color for whole NOTE.
                  if KntTreeOptions.InheritNodeBG  and assigned(FocusedNNode) then begin
                    FocusedNNode.EditorBGColor := myFolder.EditorChrome.BGColor;
                    myFolder.Editor.Color := myFolder.EditorChrome.BGColor;
                  end;

                  TreeLayoutChanged := ( VerticalLayout <> myTreeProperties.VerticalLayout );
                  SetTreeProperties( myTreeProperties );
                  TreeChrome := myTreeChrome;
                end;

                // update changes to tree control
                if ( oldIconKind <> myTreeProperties.IconKind ) then
                   TreeUI.ShowOrHideIcons;
                if ( oldShowCheckboxes <> myTreeProperties.CheckBoxes ) then
                   TreeUI.ShowOrHideCheckBoxes;

                if ( oldHideChecked <> myTreeProperties.HideChecked ) then    // [dpv]
                   if myTreeProperties.HideChecked then
                      TreeUI.HideChildNodesUponCheckState (nil, True)
                   else
                      TreeUI.ShowNonFilteredNodes(nil);

                TreeUI.UpdateTreeChrome;
                TreeUI.UpdateTreeColumns;


            end;

            if ApplyTreeChromeToAllFolders and HaveKntFolders( false, true ) then begin
                for i := 0 to myFile.FolderCount -1 do begin
                   F:= myFile.Folders[i];
                   if ((PropertiesAction = propThisFolder) and (F = myFolder)) or (F.ReadOnly) then
                       continue;
                   F.Modified:= True;
                   F.TreeChrome := myTreeChrome;
                   F.TreeUI.UpdateTreeChrome;
                   F.TreeUI.UpdateTreeColumns;
                end;
            end;


            if (PropertiesAction = propDefaults) or (NewPropertiesAction = propDefaults) then begin

                // must update all richedits and trees with the modified EditorOptions and TreeOptions:
                if HaveKntFolders( false, true ) then begin
                    for i := 0 to myFile.FolderCount -1 do begin
                       F:= myFile.Folders[i];
                       F.Editor.WordSelection := EditorOptions.WordSelect;
                       F.Editor.UndoLimit := EditorOptions.UndoLimit;
                       F.TreeUI.UpdateTreeOptions;
                    end;
                end;

                DefaultEditorChrome := myEditorChrome;
                DefaultEditorProperties := myEditorProperties;
                DefaultTabProperties := myTabProperties;
                DEFAULT_NEW_FOLDER_NAME := DefaultTabProperties.Name;

                DefaultTreeChrome := myTreeChrome;
                DefaultTreeProperties := myTreeProperties;

                if mySaveFileDefaults then
                   DEF_FN := myFile.FileName + ext_DEFAULTS

                else begin
                  // if mySaveFileDefaults was true before, and is now false, delete the file-specific .def file
                  if DEF_FN <> OrigDEF_FN then
                     deletefile( DEF_FN );
                  DEF_FN := OrigDEF_FN;
                end;

                SaveDefaults;
            end;
          end;

     		  myFolder.Editor.RestoreZoomGoal;

        end;

      finally
        myFolder.Editor.UpdateCursorPos;
        UpdateFolderDisplay;
        Form_Defaults.Free;
      end;

      if TreeLayoutChanged then begin
        screen.Cursor := crHourGlass;
        Pages.OnChange := nil;
        try
          myFolder.TreeWidth := 0;
          myFolder.SaveEditorToDataModel;
          myFolder.Editor.Clear;
          myFolder.UpdateLoadingLevels;
          DestroyVCLControlsForFolder( myFolder, false );
          CreateVCLControlsForFolder( myFolder );
          myFolder.LoadEditorFromNNode(myFolder.FocusedNNode, False);
          SetUpVCLControls( myFolder );
          FocusActiveKntFolder;
        finally
          screen.Cursor := crDefault;
          Pages.OnChange := PagesChange;
        end;

      end;

  end;

end; // EditKntFolderProperties




//=======================================================================
//  TKntFolder
//=======================================================================

// Create / Destroy  =========================================

{$REGION Create / Destroy }

constructor TKntFolder.Create (KntFile: TObject);
begin
  inherited Create;

  FKntFile:= KntFile;

  FTabSheet := nil;
  FSavedTabIndex := 0;
  FName := DEFAULT_NEW_FOLDER_NAME;
  FID := 0;
  FVisible := true;
  FReadOnly := false;
  FDefaultPlainText := false;
  FInfo := 0;
  FImageIndex := -1;
  FDateCreated := now;
  FModified := false;
  InitializeChrome( FEditorChrome );

  FWordWrap := false;
  FURLDetect := true;
  FTabSize := DEF_TAB_SIZE;
  FUseTabChar := false;
  FIsInsertMode := true;

  FHistory := TKNTHistory.Create;


  FSplitter := nil;
  FTV := nil;
  FTreeWidth := 0; // flag, so that default width will be used
  FTreeMaxWidth:= 0;
  FIconKind := niStandard;
  FCheckboxes := false;
  FTreeHidden := false;
  FHideCheckedNodes:= false;
  FPosDateCol:= -1;
  FPosFlaggedCol:= -1;
  FFocusMemory := focTree; // initially focus tree
  FSavedSelectedIndex := -1;
  //FAutoNumberNotes := false;
  FVerticalLayout := false;
  InitializeChrome( FTreeChrome );
  FDefaultNoteName := DEFAULT_NEW_NOTE_NAME;
  fNNodes := TNoteNodeList.Create;
  FLoadingLevels:= TIntegerList.Create;
  FEditorInfoPanelHidden:= false;

end; // CREATE


destructor TKntFolder.Destroy;
var
  i: integer;
  NNode: TNoteNode;
  ClosingFile: boolean;

begin
  AlarmMng.RemoveAlarmsOfFolder(Self, false);

  try
    ClosingFile:= TKntFile(KntFile).IsBusy;
    FHistory.Free;
    if fNNodes <> nil then begin

       for i := 0 to fNNodes.Count-1 do begin
          NNode:= fNNodes[i];
          if NNode = nil then continue;

          if not ClosingFile then begin         // Closing the file -> IsBusy=True -> TKntTreeUI.Destroy -> TV.OnFreeNode:= nil
             NNode.Note.RemoveNNode(NNode);
             if NNode.Note.NumNNodes = 0 then
                TKntFile(KntFile).DeleteNote(NNode.Note);
          end;

          NNode.Free;
       end;

       fNNodes.Free;
    end;

  except
  end;

  inherited Destroy;
end;

{$ENDREGION }


// Folder properties  =========================================

{$REGION Folder properties }

procedure TKntFolder.UpdateTabSheet;
begin
  with FTabSheet do begin
    Caption := FName;
    ImageIndex := FImageIndex;
  end;
end;

procedure TKntFolder.SetTabProperties( const aProps : TFolderTabProperties; UpdateName: boolean= True);
begin
  if UpdateName then
     FName := aProps.Name;
  FImageIndex := aProps.ImageIndex;
  RTL := aProps.RTL;
  Modified := true;
end;


procedure TKntFolder.GetTabProperties( var aProps : TFolderTabProperties );
begin
  aProps.ImageIndex := FImageIndex;
  aProps.Name := FName;
  aProps.RTL := FRTL;
end;


procedure TKntFolder.SetTabSheet( ATabSheet : TTab95Sheet );
begin
  if ( ATabSheet <> FTabSheet ) then
    FTabSheet := ATabSheet;
end;


function TKntFolder.GetEditor: TKntRichEdit;
begin
   Result:= NoteUI.Editor;
end;

procedure TKntFolder.SetNoteUI( ANoteUI : INoteUI );
begin
   fNoteUI:= ANoteUI;
   if fNoteUI <> nil then
      fNoteUI.SetInfoPanelHidden(FEditorInfoPanelHidden);
end;

procedure TKntFolder.SetTreeUI(tree: TKntTreeUI);
begin
   fTreeUI:= tree;
   fTV:= fTreeUI.TV;
   fTV.NodeDataSize := SizeOf(TNoteNode);
end;


procedure TKntFolder.SetName( AName : TNoteNameStr );
begin
  AName := trim( AName );
  if (( FName = AName ) or ( AName = '' )) then exit;
  FName := copy( AName, 1, TABNOTE_NAME_LENGTH );
  Modified := true;
  if _ALLOW_VCL_UPDATES and assigned( FTabSheet ) then FTabSheet.Caption := FName;
end;


procedure TKntFolder.SetID( ID : Cardinal );
begin
  if ( FID = 0 ) then
    FID := ID;
  // otherwise, never allow the ID to be changed
end;


procedure TKntFolder.SetReadOnly( AReadOnly : boolean );
begin
  if ( AReadOnly <> FReadOnly ) then
  begin
    FReadOnly := AReadOnly;
    Modified := true;

    if _ALLOW_VCL_UPDATES then begin
        if TreeUI <> nil then TreeUI.ReadOnly:= AReadOnly;
        if assigned( NoteUI ) then NoteUI.SetReadOnly(FReadOnly);
    end;

    App.FolderPropertiesModified(Self);
    if NoteUI <> nil then
       App.EditorPropertiesModified(NoteUI.Editor);
  end;
end;


procedure TKntFolder.SetRTL( RTL : boolean );
var
  BiDiMode: TBiDiMode;
begin
  if ( RTL <> FRTL ) then begin
    FRTL := RTL;
    Modified := true;

    TreeUI.ApplyBiDiMode;
    App.FolderPropertiesModified(Self);

    if NoteUI <> nil then begin
       BiDiMode:= bdLeftToRight;
       if RTL then
          BiDiMode:= bdRightToLeftNoAlign;
       NoteUI.Editor.BiDiMode:= BiDiMode;
       App.EditorPropertiesModified(NoteUI.Editor);
    end;
  end;
end;


procedure TKntFolder.SetDefaultPlainText( APlainText : boolean );
begin
  if ( APlainText <> FDefaultPlainText ) then begin
    FDefaultPlainText := APlainText;
    Modified := true;

    App.FolderPropertiesModified(Self);
    if NoteUI <> nil then
       App.EditorPropertiesModified(NoteUI.Editor);
  end;
end;


function TKntFolder.GetModified : boolean;
begin
  result := FModified;
end;


procedure TKntFolder.SetModified( AModified : boolean );
begin
  if FModified = AModified then exit;

  FModified := AModified;
  if AModified then
     TKntFile(KntFile).Modified:= True;
end;


function TKntFolder.GetTabIndex: integer;
begin
   Result:= -1;
   if assigned( FTabSheet ) then
     Result:= FTabSheet.TabIndex;
end;


procedure TKntFolder.SetTabIndex( ATabIndex : integer );
begin
  if ( TabIndex = ATabIndex ) then exit;
  if _ALLOW_VCL_UPDATES and assigned( FTabSheet ) then
    FTabSheet.PageIndex := ATabIndex;
end;


procedure TKntFolder.SetImageIndex( AImgIdx : integer );
begin
  if ( FImageIndex = AImgIdx ) then exit;
  FImageIndex := AImgIdx;
  if _ALLOW_VCL_UPDATES and assigned( FTabSheet ) then
    FTabSheet.ImageIndex := FImageIndex;
end;


procedure TKntFolder.SetWordWrap( AWordWrap : boolean );
begin
  if ( FWordWrap = AWordWrap ) then exit;
  FWordWrap := AWordWrap;
  Modified := true;
  if _ALLOW_VCL_UPDATES and assigned( NoteUI ) then
    NoteUI.Editor.WordWrap := FWordWrap;
end;


procedure TKntFolder.SetURLDetect( AURLDetect : boolean );
begin
  if ( FURLDetect = AURLDetect ) then exit;
  FURLDetect := AURLDetect;
  Modified := true;
  if _ALLOW_VCL_UPDATES and assigned( NoteUI ) then
    NoteUI.Editor.AutoURLDetect := FURLDetect;
end;


procedure TKntFolder.SetTabSize( ATabSize : byte );
begin
  if ( FTabSize = ATabSize ) then exit;
  FTabSize := ATabSize;
  Modified := true;
  if _ALLOW_VCL_UPDATES and assigned( NoteUI ) then
    NoteUI.Editor.TabSize := FTabSize;
end;

procedure TKntFolder.SetEditorInfoPanelHidden(value: boolean);
begin
   FEditorInfoPanelHidden:= value;
   NoteUI.SetInfoPanelHidden(value);
   Form_Main.MMViewEditorInfoPanel.Checked:= not FEditorInfoPanelHidden;
end;


procedure TKntFolder.UpdateLoadingLevels;
var
   Node: PVirtualNode;
   Level: integer;
begin
   LoadingLevels.Capacity:= NNodes.Count;

   Node := TV.GetFirst;
   while assigned(Node) do begin
      Level:= TV.GetNodeLevel(Node);
      LoadingLevels.Add(Level);
      Node := TV.GetNext(Node);
   end;
end;


{$ENDREGION}



// NNodes: Get by ID, GID, Add, Remove, NodeSelected, ...  =========================================

{$REGION NNodes: Get by ID, GID, Add, Remove, NodeSelected, ...}

function TKntFolder.GetNNode(Node : PVirtualNode): TNoteNode;
begin
  Result:= TreeUI.GetNNode(Node);
end;

function TKntFolder.GetNNodeByID(const aID : Word): TNoteNode;
var
   i: integer;
begin
  for i:= 0 to NNodes.Count-1 do begin
     Result:= NNodes[i];
     if Result.ID = aID then exit;
  end;
  Result:= nil;
end;

function TKntFolder.GetNNodeByGID(const aGID : Cardinal): TNoteNode;
var
   i: integer;
begin
  for i:= 0 to NNodes.Count-1 do begin
     Result:= NNodes[i];
     if Result.GID = aGID then exit;
  end;
  Result:= nil;
end;

function TKntFolder.GetNNodeByNoteName(const aName : string): TNoteNode;
var
   i: integer;
begin
  for i:= 0 to NNodes.Count-1 do begin
     Result:= NNodes[i];
     if Result.NoteName = aName then exit;
  end;
  Result:= nil;
end;


function TKntFolder.GetFocusedNNode : TNoteNode;
begin
  Result:= TreeUI.GetFocusedNNode;
end;



function TKntFolder.AddNNode(NNode: TNoteNode): integer;
begin
   Result:= fNNodes.Add(NNode);
end;

function TKntFolder.RemoveNNode(NNode: TNoteNode): integer;
begin
   Result:= fNNodes.Remove(NNode);
end;



function TKntFolder.AddNewNote(CopyFromNNode: TNoteNode = nil; InheritFromNode: PVirtualNode = nil): TNoteNode;
var
   NNode, InheritFromNNode: TNoteNode;
begin
   NNode:= TKntFile(KntFile).AddNewNote(Self, CopyFromNNode);

   if (CopyFromNNode = nil) and (KntTreeOptions.InheritNodeProperties) and (InheritFromNode <> nil) then begin
      InheritFromNNode := TreeUI.GetNNode(InheritFromNode);

      NNode.Bold :=          InheritFromNNode.Bold;
      NNode.ImageIndex :=    InheritFromNNode.ImageIndex;
      NNode.EditorBGColor := InheritFromNNode.EditorBGColor;
      NNode.NodeColor :=     InheritFromNNode.NodeColor;
      NNode.NodeBGColor :=   InheritFromNNode.NodeBGColor;
      NNode.EditorBGColor := InheritFromNNode.EditorBGColor;
      NNode.Note.Entries[0].IsPlainTXT := InheritFromNNode.Note.SelEntry.IsPlainTXT;
   end
   else
      NNode.Note.Entries[0].IsPlainTXT := Self.DefaultPlainText;

   Result:= NNode;
end;


function TKntFolder.AddNewNNode(Note: TNote; CopyFromNNode: TNoteNode = nil): TNoteNode;
begin
  Result:= TKntFile(KntFile).AddNewNNode(Note, Self, CopyFromNNode);
end;

function TKntFolder.DeleteNNode(NNode: TNoteNode): integer;
begin
   assert(NNode<>nil);

   Result:= fNNodes.Remove(NNode);
   NNode.Note.RemoveNNode(NNode);
   AlarmMng.RemoveAlarmsOfNNode(NNode, False);
   NoteUI.NNodeDeleted;

   if NNode.Note.NumNNodes = 0 then
      TKntFile(KntFile).DeleteNote(NNode.Note);

   NNode.Free;
   Modified := true;
end;



procedure TKntFolder.NoNodeInTree;
begin
   NoteUI.LoadFromNNode(nil, true);
end;


procedure TKntFolder.NodeSelected(const Node: PVirtualNode; const LastNodeSelected: PVirtualNode);
var
  NNode : TNoteNode;
  LocBeforeSelChanged: TLocation;
  LastNNodeFocused: TNoteNode;

begin
   if (not assigned(Node)) then exit;

   NNode:= TreeUI.GetNNode(Node);

   LastNNodeFocused:= nil;
   if LastNodeSelected <> nil then
      LastNNodeFocused:= TreeUI.GetNNode(LastNodeSelected);

   if (not _Executing_History_Jump) and (not _Executing_JumpToKNTLocation_ToOtherNote) then begin
       if LastNNodeFocused <> nil then begin
          // Add to history the location of previous node, before new selection. Editor (and so caret position) has not been changed yet (It is done in this method)
          LocBeforeSelChanged:= nil;
          GetKntLocation (ActiveFolder, LocBeforeSelChanged, false, LastNNodeFocused);
          AddHistoryLocation(Self, false, LocBeforeSelChanged);
       end;
      _LastMoveWasHistory := false;
      UpdateHistoryCommands;
   end;

  LoadEditorFromNNode(NNode, true);

end; // NodeSelected


procedure TKntFolder.NoteNameModified(NNode: TNoteNode);
begin
   if NNode.TVNode <> nil then                         // Name may not have been assigned yet and is being modified
      TV.InvalidateNode(NNode.TVNode);

   if assigned(NoteUI) and (NoteUI.NNode=NNode) then
      NoteUI.ReloadNoteName;
end;


{$ENDREGION}



// Virtual Notes  =========================================

{$REGION Virtual Notes }

function TKntFolder.CheckVirtualNote (Node: PVirtualNode): boolean;
var
   Note: TNote;
begin
  Result:= false;
  if (Node = nil ) then exit;
  Note:= TreeUI.GetNNode(Node).Note;
  Result:= Note.IsVirtual;
  if not Result then
     App.ErrorPopup(Format(GetRS(sFld47), [Note.Name]));
end;


procedure TKntFolder.VirtualNoteRefresh (const DoPrompt : boolean);
var
  Node: PVirtualNode;
  Note : TNote;

begin
  Node:= TV.FocusedNode;
  if not CheckVirtualNote (Node) then exit;

  Note:= TreeUI.GetNNode(Node).Note;

  if Note.Modified then begin
    if ( App.DoMessageBox(Format(GetRS(sFld42) + GetRS(sFld40), [Note.Name, ExtractFilename( Note.VirtualFN )] ),
                           mtWarning, [mbOK,mbCancel] ) <> mrOK ) then
    exit;
  end
  else
  if DoPrompt then begin
    if (App.DoMessageBox( Format(GetRS(sFld43) + GetRS(sFld40), [Note.Name, ExtractFilename( Note.VirtualFN )] ),
                          mtConfirmation, [mbOK,mbCancel] ) <> mrOK ) then
    exit;
  end;

  try
    try
      Note.LoadVirtualFile;
      Note.Modified:= false;
    except
      on E : Exception do begin
        App.ErrorPopup(E, GetRS(sFld44));
        exit;
      end;
    end;

    try
      NoteUI.ReloadFromDataModel;          // Changes in editor will be ignored
      App.EditorSaved(NoteUI.Editor);      // To ensure synchronization on open linked nodes (if any)
      App.ShowInfoInStatusBar(GetRS(sFld45));
    except
      App.ShowInfoInStatusBar(GetRS(sFld46));
    end;

  finally
    Modified := true;
  end;

end;


procedure TKntFolder.VirtualNoteUnlink;
var
  Node: PVirtualNode;
  Note, NewNote : TNote;
  NNode, NewNNode: TNoteNode;

begin
  Node:= TV.FocusedNode;
  if not CheckVirtualNote (Node) then exit;

  NNode:= TreeUI.GetNNode(Node);
  Note:= NNode.Note;

  if (App.DoMessageBox( Format(GetRS(sFld41), [Note.Name, Note.VirtualFN]),
       mtConfirmation, [mbOK, mbCancel] ) = mrOK ) then begin

     Note.VirtualFN := '';
     Modified := true;

     NoteUI.ConfigureEditor;
     App.FolderPropertiesModified(Self);
     App.EditorPropertiesModified(NoteUI.Editor);
  end;

end;


procedure TKntFolder.VirtualNoteProc(VirtFN : string );
var
  Node : PVirtualNode;
  Note : TNote;
  NNode: TNoteNode;
  oldDlgFilter : string;
  ext : string;
  IsVNError, IsFlushingData, IsChangingFile : boolean;

begin
  Node:= TV.FocusedNode;
  if (Node = nil) then exit;
  if Form_Main.FolderIsReadOnly( Self, true ) then exit;

  NNode:= TreeUI.GetNNode(Node);
  Note:= NNode.Note;

  IsFlushingData := false;
  IsChangingFile := false;
  IsVNError := false;

  if (Note.IsVirtual) then begin
    // Already a virtual node. Ask if user wants to change the file with which the node is linked.
    // Do not prompt if there was an error loading the node (in that case, assume the user DOES want to relink the node)

    if Note.HasVNodeError then begin
      IsChangingFile := true;
      IsVNError := true;
    end
    else begin
      if (App.DoMessageBox(Format(GetRS(sFld31), [Note.Name, Note.VirtualFN]), mtConfirmation, [mbOK, mbCancel] ) = mrOK) then
         IsChangingFile := true;
    end;

    if ( not IsChangingFile ) then exit;

  end
  else begin
    // not a virtual node. If it has text, we have to have an additional prompt
    if ( Editor.Lines.Count > 0 ) then begin
      if (App.DoMessageBox(Format(GetRS(sFld32), [Note.Name]), mtConfirmation, [mbOK,mbCancel] ) <> mrOK) then
         exit;
      IsFlushingData := true; // needs a SaveDlg, not an OpenDlg
    end;

  end;

  with Form_Main do begin
      if (( ActiveFile.FileFormat = nffEncrypted ) and ( not App.Virtual_UnEncrypt_Warning_Done )) then begin
        if ( App.DoMessageBox(GetRS(sFld33), mtWarning, [mbYes,mbNo]) <> mrYes ) then exit;
        App.Virtual_UnEncrypt_Warning_Done := true;
      end;

      if ( VirtFN = '' ) then begin

        if IsFlushingData then begin
           // use SaveDlg
           oldDlgFilter := SaveDlg.Filter;
           SaveDlg.Filter := FILTER_RTFFILES + '|' + FILTER_TEXTFILES + '|' + FILTER_HTMLFILES + '|' + GetRS(FILTER_ALLFILES);
           SaveDlg.Title := GetRS(sFld34);
           SaveDlg.Filename := Note.Name;

           try
             if ( not SaveDlg.Execute ) then exit;
           finally
             SaveDlg.Filter := oldDlgFilter;
           end;
           VirtFN := SaveDlg.FileName;
           if (ExtractFileExt(VirtFN) = '') then
              VirtFN := VirtFN + ext_RTF;
        end
        else begin
           // use OpenDlg
           oldDlgFilter := OpenDlg.Filter;
           OpenDlg.Filter := FILTER_RTFFILES + '|' + FILTER_TEXTFILES + '|' + FILTER_HTMLFILES + '|' + GetRS(FILTER_ALLFILES);
           OpenDlg.Title := GetRS(sFld34);
           if IsVNError then
             OpenDlg.Filename := copy( Note.VirtualFN, 2, length( Note.VirtualFN ))
           else
             OpenDlg.Filename := Note.VirtualFN;

           try
             if ( not OpenDlg.Execute ) then exit;
           finally
             OpenDlg.Filter := oldDlgFilter;
           end;
           VirtFN := OpenDlg.FileName;

        end; // if IsFlushingData
      end; // if ( VirtFN = '' );


      VirtFN := normalFN( VirtFN );

      if directoryexists( VirtFN ) then begin
        // not a file, but a directory - cannot import
        // (user could have drag-dropped a directory, so we must check)
        exit;
      end;

      ext := ExtractFileExt( VirtFN );
      if not ( ExtIsRTF(ext) or ExtIsText(ext) or ExtIsHTML(ext)) then begin
         App.ErrorPopup( GetRS(sFld35));
         exit;
      end;

      // It is not reccommended to link files on virtual media (floppies,
      // CD-ROMs, ZIP drives, etc. So we check.
      if IsDriveRemovable(VirtFN) then begin
        case KntTreeOptions.RemovableMediaVNodes of
          _REMOVABLE_MEDIA_VNODES_DENY : begin
            App.ErrorPopup( Format(GetRS(sFld36),[Extractfiledrive( VirtFN )] ));
            exit;
          end;
          _REMOVABLE_MEDIA_VNODES_WARN : begin
            if ( App.DoMessageBox( Format(GetRS(sFld37),
              [Extractfiledrive( VirtFN )] ), mtWarning, [mbOK,mbCancel]) <> mrOK ) then
                exit;
          end;
          { _REMOVABLE_MEDIA_VNODES_ALLOW or any other value: allow }
        end;
      end;


      // any given file can be linked to a virtual node only once per KNT file. So we must check if the selected file already
      // exists as a virtual node in the currently open KNT file.
      // Sí es posible crear uno o varios nodos enlazados a ese nodo virtual
      if ActiveFile.GetVirtualNoteByFileName( Note, VirtFN ) <> nil then begin
        App.ErrorPopup(GetRS(sFld38));
        exit;
      end;



      Editor.BeginUpdate;
      try
        try

          if IsChangingFile then begin
            // Node must save its existing data first:
            if not IsVNError then begin
              NoteUI.SaveToDataModel;
              Note.SaveVirtualFile;
            end;
            // now clear the editor
            Editor.Clear;
            Editor.ClearUndo;
          end;

          Note.VirtualFN := VirtFN;

          if IsFlushingData then begin
            NoteUI.SaveToDataModel;
            Note.SaveVirtualFile;
          end
          else begin
            Note.LoadVirtualFile;
            App.EditorSaved(NoteUI.Editor);  // To ensure synchronization on open linked nodes (if any)
            NoteUI.ReloadFromDataModel;
          end;
          Note.Modified:= false;     // To warn if user edit this editor from this moment

          if ( KntTreeOptions.AutoNameVNodes and ( not IsFlushingData )) then begin
            Note.Name := ExtractFilename( Note.VirtualFN );
            (* [x] ImportFileNamesWithExt ignored for virtual nodes, because it is useful to have extension visible
            if KeyOptions.ImportFileNamesWithExt then
              myNote.Name := ExtractFilename( myNote.VirtualFN )
            else
              myNote.Name := ExtractFilenameNoExt( myNote.VirtualFN );
            *)
          end;

        except
          on E : Exception do begin
            Note.VirtualFN := '';
            App.ErrorPopup(E, GetRS(sFld39));
          end;
        end;

      finally
        Modified := true;
        Editor.Modified := false;
        Editor.EndUpdate;

        TreeUI.TV.InvalidateNode(Node);

        NoteUI.ConfigureEditor;
        App.FolderPropertiesModified(Self);
        App.EditorPropertiesModified(Editor);
      end;
  end;
end;

{$ENDREGION}



// Tree. Chrome and Properties  =========================================

{$REGION Tree Chrome and Properties }

procedure TKntFolder.SetTreeChrome( AChrome : TChrome );
begin
  FTreeChrome := AChrome;
  Modified := true;
end;


procedure TKntFolder.SetTreeProperties( const aProps : TFolderTreeProperties );
begin
  FCheckboxes := aProps.CheckBoxes;
  FIconKind := aProps.IconKind;
  FDefaultNoteName := aProps.DefaultName;
  //FAutoNumberNotes := aProps.AutoNumberNodes;
  if FVerticalLayout <> aProps.VerticalLayout then
     FTreeMaxWidth:= -Abs(FTreeMaxWidth);
  FVerticalLayout := aProps.VerticalLayout;
  FHideCheckedNodes := aProps.HideChecked;
  FPosDateCol := aProps.PosDateCol;
  FPosFlaggedCol := aProps.PosFlaggedCol;
end;


procedure TKntFolder.GetTreeProperties( var aProps : TFolderTreeProperties );
begin
  aProps.CheckBoxes := FCheckboxes;
  aProps.IconKind := FIconKind;
  aProps.DefaultName := FDefaultNoteName;
  //aProps.AutoNumberNodes := FAutoNumberNotes;
  aProps.VerticalLayout := FVerticalLayout;
  aProps.HideChecked:= FHideCheckedNodes;
  aProps.PosDateCol := FPosDateCol;
  aProps.PosFlaggedCol := FPosFlaggedCol;
end;


procedure TKntFolder.UpdateTree;
begin
    FTV.Color := FTreeChrome.BGColor;
    with FTreeChrome.Font do begin
      fTV.Font.Name := Name;
      fTV.Font.Size := Size;
      fTV.Font.Style := Style;
      fTV.Font.Charset := Charset;
      fTV.Font.Color := Color;
    end;
end;


procedure TKntFolder.SetFocusOnTree;
begin
  try
     TV.SetFocus;
  except
  end;
end;


{$ENDREGION}


// Editor <-> DataStream. Configure, Chrome, Properties, UpdateEditor, ..  ==========================

{$REGION Editor <-> DataStream. Configure, Chrome, Properties, UpdateEditor, .. }

// TODO: We will have to manage the possible multiple entries of a note.
// FOR THE MOMENT we will work with what we will assume is the only entry

procedure TKntFolder.LoadEditorFromNNode(NNode: TNoteNode; SavePreviousContent: boolean);
var
  Node: PVirtualNode;
  HintStatusBar: string;

begin
  if NNode = nil then exit;

  with Form_Main do begin
      try
        NoteUI.LoadFromNNode(NNode, SavePreviousContent);

        if assigned(NNode) then begin
             Node:= NNode.TVNode;
             if not NNode.IsVirtual then begin
                if (not EditorOptions.TrackStyle) then begin
                  if KntTreeOptions.ShowFullPath then
                    HintStatusBar := TreeUI.GetNodePath(Node, KntTreeOptions.NodeDelimiter, KntTreeOptions.PathTopToBottom)
                  else
                    HintStatusBar := NNode.NodeName(TreeUI);
                end;
             end
             else begin
               if (not EditorOptions.TrackStyle) then
                 HintStatusBar := GetRS(sFld01) + NNode.Note.VirtualFN;
             end;
             TVCheckNode.Checked := Node.CheckState.IsChecked;
             TVBoldNode.Checked :=  NNode.Bold;
             TVChildrenCheckbox.Checked:= NNode.ChildrenCheckbox;

             UpdateAlarmStatus;
             UpdateShowImagesState;
        end
        else
            HintStatusBar:= '';

        App.ShowInfoInStatusBar(HintStatusBar);

      except
        On E : Exception do begin
          App.ErrorPopup(E.Message);
          exit;
        end;
      end;
  end;

end;


procedure TKntFolder.ReloadEditorFromDataModel(SavePreviousContent: boolean= true);
begin
   if SavePreviousContent then
      NoteUI.SaveToDataModel;
   NoteUI.ReloadFromDataModel;
end;


{
  If Editor was modified then it will return the Stream associated to the node that will be updated
}
function TKntFolder.SaveEditorToDataModel: TMemoryStream;
begin
  Result:= NoteUI.SaveToDataModel;
end;



procedure TKntFolder.SetFocusOnNoteEditor;
begin
  try
     NoteUI.SetFocusOnEditor;
  except
  end;
end;


procedure TKntFolder.SetEditorChrome( AChrome : TChrome );
begin
  FEditorChrome := AChrome;
  Modified := true;
end; // SetEditorChrome


procedure TKntFolder.SetEditorProperties( const aProps : TFolderEditorProperties );
begin
  FDefaultPlainText := aProps.PlainText;
  FTabSize := aProps.TabSize;
  FURLDetect := aProps.URLDetect;
  FUseTabChar := aProps.UseTabChar;
  FWordWrap := aProps.WordWrap;
  FTagSelectorDisabled := aProps.TagSelectorDisabled;
  Modified := true;
end; // SetEditorProperties


procedure TKntFolder.GetEditorProperties( var aProps : TFolderEditorProperties );
begin
  aProps.PlainText := FDefaultPlainText;
  aProps.TabSize := FTabSize;
  aProps.URLDetect := FURLDetect;
  aProps.UseTabChar := FUseTabChar;
  aProps.WordWrap := FWordWrap;
  aProps.TagSelectorDisabled := FTagSelectorDisabled;
end; // GetEditorProperties



{$ENDREGION}



// TextPlain  =========================================

{$REGION TextPlain }

procedure InitializeTextPlain_Compare(NEntry: TNoteEntry; RTFAux: TAuxRichEdit); forward;
procedure InitializeTextPlain_Compare_RTF(NEntry: TNoteEntry; RTFAux: TAuxRichEdit);  forward;

function InitializeTextPlain(NEntry: TNoteEntry; RTFAux: TAuxRichEdit): boolean;
var
  AStr: AnsiString;
  Str: String;
//Ok: Integer;

begin
    Result:= False;  // Initialization was required?
//  Ok:= 1;

    if NEntry.TextPlain = '' then begin
       if NEntry.IsPlainTXT then begin
         AStr:= MemoryStreamToString(NEntry.Stream);
         Str:= TryUTF8ToUnicodeString(AStr);
         Str:= StringReplace(Str, #13#10, #13, [rfReplaceAll]);
         if (Length(Str) > 1) and (Str[1] = #$FEFF)  then      // UTF16_BE_BOM = AnsiString(#$FEFF)
            Delete(Str, 1, 1);
         NEntry.TextPlain:= Str;
       end
       else begin
          LoadStreamInRTFAux (NEntry.Stream, RTFAux);
          NEntry.TextPlain:= RTFAux.TextPlain;
{          if Ok= 2 then
             InitializeTextPlain_Compare_RTF(NEntry, RTFAux);
}
       end;

{
       if NEntry.IsPlainTXT then begin
          LoadStreamInRTFAux (NEntry.Stream, RTFAux);
          NEntry.TextPlain:= RTFAux.TextPlain;
          assert(not (NEntry.IsPlainTXT and (Str <> NEntry.TextPlain)));
          if Ok= 2 then
             InitializeTextPlain_Compare(NEntry, RTFAux);
       end;
}
       Result:= (NEntry.TextPlain <> '');  // If it haven't data, don't count it in TKntFolder.InitializeTextPlainVariables
    end;
end;

function TKntFolder.InitializeTextPlainVariables( nMax: integer; RTFAux: TAuxRichEdit): boolean;
var
  i, N: integer;
  NEntry: TNoteEntry;
  LastTick, Tick: integer;

begin
  Result:= false;          // Returns True if all nodes have TextPlain initialized

  N:= 0;
  LastTick:= GetTickCount;
  for i := 0 to NNodes.Count - 1 do  begin

     if (i mod 60) = 0 then begin
        if (MillisecondsIdle <= 450) then Exit;
        Tick:= GetTickCount;
        if Tick - LastTick > 1000 then begin
           Application.ProcessMessages;
           LastTick:= Tick;
        end;
     end;

     NEntry:= NNodes[i].Note.Entries[0];                  // %%%
     if InitializeTextPlain (NEntry, RTFAux) then
        inc (N);

     if N >= nMax then Exit;
  end;

  Result:= true;
end;

// myTreeNode must be active node in folder, so that the Editor is showing its content
function TKntFolder.PrepareTextPlain(NNode: TNoteNode; RTFAux: TAuxRichEdit; ClearRTFAux: boolean= False): string;
var
   NEntry: TNoteEntry;
begin
   if NoteUI.Editor.Modified then
      NoteUI.SaveToDataModel;

   NEntry:= NNode.Note.Entries[0];         // %%%
   if InitializeTextPlain(NEntry, RTFAux) and ClearRTFAux and NEntry.IsRTF then
      RTFAux.Clear;
   Result:= NEntry.TextPlain;
end;


procedure InitializeTextPlain_Compare(NEntry: TNoteEntry; RTFAux: TAuxRichEdit);
var
    AStr: AnsiString;
    Tick: integer;
    Total1,Total2,Total3, Total4, Total5: Single;
    i, N: integer;
    LBuffer: TBytes;
    Str1,Str2,Str3,Str4,Str5: String;
    S: string;
    Ok: boolean;

  procedure Wait;
  var
     i, j: NativeInt;
  begin
     j:= 0;
     for i:= 0 to Cardinal.MaxValue div 2 do
        inc(j);
  end;

begin
     Ok:= True;
     N:= 300000;

     Form_Main.Timer.Enabled := false;

     // The first two strings used for comparison contained ANSI text, small in size. The first in UTF8
     // was small, and the second occupied 15481 bytes, with characters in multiple languages, along with Non-Emoji Symbol,
     // with symbols from:
     //   https://www.lennyfacecopypaste.com/text-symbols/square-rectangle.html
     //   https://www.prepressure.com/fonts/basics/emoji-list
     //    ...

     //1:   N:300.000 ->   0,21799 | 1,04700  | utf8:   0,20299 |     11,156
     //2:   N:300.000 -> 374,0697  | -        | utf8: 428,90    | 20.568,9
     //3:   N:300.000 ->   0,125   | 0,42199  | utf8:   0,15600 |      6,85900
     //4:   N:300.000 ->   0,32800 | 1,20299  | utf8:   0,23499 |     10,84399
     //5:   N:300.000 ->   0,20299 | 0,5      | utf8:   0,20299 |      6,43800

     // The most optimal option is the third one, using MemoryStreamToString, TryUTF8ToUnicodeString and StringReplace
     // Compared to it, getting it using RichText, with calls to LoadStreamInRTFAux and RTFAux.TextPlain
     // is many orders of magnitude slower. Almost 3000 times slower:

     //3:   N:300.000 ->      1   | -        | utf8:      1    |        1
     //2:   N:300.000 -> x 2992   | -        | utf8:  x 826    |   x 2998


     Sleep(1000);
     Wait;
     Tick:= GetTickCount;
     for i:= 1 to N do begin
        AStr:= MemoryStreamToString(NEntry.Stream);
        Str1:= TryUTF8ToUnicodeString(AStr);
        Str1:= StringWithoutLineFeeds(Str1);
     end;
     Total1:= (GetTickCount - Tick)/1000;        // N:300.000 -> 0,21799 | 1,04700  | utf8: 0,20299  | 11,156

     Sleep(1000);
     Wait;
     Tick:= GetTickCount;
     for i:= 1 to N do begin
       LoadStreamInRTFAux (NEntry.Stream, RTFAux);
       NEntry.TextPlain:= RTFAux.TextPlain;
     end;
     Total2:= (GetTickCount - Tick)/1000;       // N:10.000 -> 12,46899 | ..       | utf8: 14,296699
                                                // N: 1.000 ->   ..     | ..       | utf8: ..         | 68,56300
                                      // .. =>    N:300.000 -> 374,0697 | ..       | utf8: 428,90     | 20568,9
   try
     Sleep(1000);
     Wait;
     Tick:= GetTickCount;
     for i:= 1 to N do begin
        AStr:= MemoryStreamToString(NEntry.Stream);
        Str3:= TryUTF8ToUnicodeString(AStr);
        Str3:= StringReplace(Str3,#13#10,#13,[rfReplaceAll]);
        if (Length(Str3)>1) and (Str3[1]=#$FEFF)  then      // UTF16_BE_BOM = AnsiString(#$FEFF)
           Delete(Str3,1,1);
     end;
     Total3:= (GetTickCount - Tick)/1000;        // N:300.000 -> 0,125      | 0,42199  | utf8: 0,15600 | 6,85900
   except
    On E: Exception do
       S:= E.Message;
   end;

   try
     Sleep(1000);
     Wait;
     Tick:= GetTickCount;
     for i:= 1 to N do begin
        SetLength(LBuffer, NEntry.Stream.Size);
        NEntry.Stream.Position:= 0;
        NEntry.Stream.Read(LBuffer[0], NEntry.Stream.Size);
        Str4:= ConvertToUnicodeString(LBuffer);
        Str4:= StringWithoutLineFeeds(Str4);
     end;
     Total4:= (GetTickCount - Tick)/1000;        // N:300.000 -> 0,32800    | 1,20299  | utf8: 0,23499 | 10,84399
   except
    On E: Exception do
       S:= E.Message;
   end;

   try
     Sleep(1000);
     Wait;
     Tick:= GetTickCount;
     for i:= 1 to N do begin
        SetLength(LBuffer, NEntry.Stream.Size);
        NEntry.Stream.Position:= 0;
        NEntry.Stream.Read(LBuffer[0], NEntry.Stream.Size);
        Str5:= ConvertToUnicodeString(LBuffer);
        Str5:= StringReplace(Str5,#13#10,#13,[rfReplaceAll]);
        if (Length(Str5)>1) and (Str5[1]=#$FEFF)  then      // UTF16_BE_BOM = AnsiString(#$FEFF)
           Delete(Str5,1,1);
     end;
     Total5:= (GetTickCount - Tick)/1000;         // N:300.000 -> 0,20299 | 0,5     | utf8: 0,20299 | 6,43800
   except
    On E: Exception do
       S:= E.Message;
   end;


   if not ((NEntry.TextPlain=Str1) and (str1=Str3) and (str1=Str4) and (str1=Str5)) then
      Ok:= False;

   Form_Main.Timer.Enabled := True;
end;


procedure InitializeTextPlain_Compare_RTF(NEntry: TNoteEntry; RTFAux: TAuxRichEdit);
var
    AStr: AnsiString;
    Tick: integer;
    Total1,Total2,Total3, Total4, Total5: Single;
    i, N: integer;
    Str1,Str2,Str3,Str4,Str5: String;
    S: string;
    Ok: boolean;

  procedure Wait;
  var
     i, j: NativeInt;
  begin
     j:= 0;
     for i:= 0 to Cardinal.MaxValue div 2 do
        inc(j);
  end;

begin
     Ok:= True;
     N:= 100;

     Form_Main.Timer.Enabled := false;
     {
      100 iterations
      The times obtained with LoadStreamInRTFAux are in all cases better than with PutRtfText
      As an optimization, I can see that the use of BeginUpdate and EndUpdate significantly improves the times obtained.
      Among other things, the times obtained with the use of BeginUpdate are more stable, probably without BeginUpdate
      many unnecessary messages are generated whose management by the system can give rise to these variations.

      As an example (Stream size in Bytes -> Times in seconds (several attempts measured)
      Tamaño                   Without BeginUpdate                           WITH BeginUpdate
      172                0,015                                            0,016
      1735               0,422 0,406  0,39    (-> x 1,78)                 0,25 0,234  0,2189
      5045               0,5                  (-> x 1,52)                 0,328
     19410               0,594                (-> x 1,584)                0,375
     65152               1,375  2,547         (-> x 1,375 - x 1,94)       1  1,312
     89922               1,562  2,891 2,844   (-> x 1,407 - x 2,20)       1,11  1,313  1,312
    149082               9,235 17,906 17,719  (-> x 1,79  - x 3,176)      5,156  5,637  5,61
     }


     Sleep(1000);
     Wait;
     Tick:= GetTickCount;
     for i:= 1 to N do begin
        LoadStreamInRTFAux (NEntry.Stream, RTFAux);
        Str1:= RTFAux.TextPlain;
     end;
     Total1:= (GetTickCount - Tick)/1000;

     Sleep(1000);
     Wait;
     Tick:= GetTickCount;
     RTFAux.BeginUpdate;
     for i:= 1 to N do begin
        LoadStreamInRTFAux (NEntry.Stream, RTFAux);
        Str2:= RTFAux.TextPlain;
     end;
     RTFAux.EndUpdate;
     Total2:= (GetTickCount - Tick)/1000;


     Sleep(1000);
     Wait;
     Tick:= GetTickCount;
     for i:= 1 to N do begin
        LoadStreamInRTFAux (NEntry.Stream, RTFAux);
        Str1:= RTFAux.TextPlain;
     end;
     Total3:= (GetTickCount - Tick)/1000;

     Sleep(1000);
     Wait;
     Tick:= GetTickCount;
     RTFAux.BeginUpdate;
     for i:= 1 to N do begin
        LoadStreamInRTFAux (NEntry.Stream, RTFAux);
        Str2:= RTFAux.TextPlain;
     end;
     RTFAux.EndUpdate;
     Total4:= (GetTickCount - Tick)/1000;


{
     Sleep(1000);
     Wait;
     Tick:= GetTickCount;
     for i:= 1 to N do begin
       RTFAux.Clear;
       AStr:= MemoryStreamToString(NEntry.Stream);
       Str3:= TryUTF8ToUnicodeString(AStr);
       RTFAux.PutRtfText(Str3, True);
       Str3:= RTFAux.TextPlain;
     end;
     Total3:= (GetTickCount - Tick)/1000;

     Sleep(1000);
     Wait;
     Tick:= GetTickCount;
     for i:= 1 to N do begin
       RTFAux.Clear;
       AStr:= MemoryStreamToString(NEntry.Stream);
       RTFAux.PutRtfText(AStr, True);
       Str4:= RTFAux.TextPlain;
     end;
     Total4:= (GetTickCount - Tick)/1000;

     Sleep(1000);
     Wait;
     Tick:= GetTickCount;
     RTFAux.BeginUpdate;
     for i:= 1 to N do begin
       RTFAux.Clear;
       AStr:= MemoryStreamToString(NEntry.Stream);
       RTFAux.PutRtfText(AStr, True);
       Str5:= RTFAux.TextPlain;
     end;
     RTFAux.EndUpdate;
     Total5:= (GetTickCount - Tick)/1000;
}

  Form_Main.Timer.Enabled := true;

end;


{$ENDREGION }



// Images  =========================================

{$REGION Images }

function TKntFolder.CheckSavingImagesOnMode (ImagesMode: TImagesMode; Stream: TMemoryStream;
                                           ExitIfAllImagesInSameModeDest: boolean = true): TImageIDs;
var
  strRTF: AnsiString;
  ContainsImgIDsRemoved: boolean;
  ContainsImages: boolean;

begin
    Result:= nil;
    if Stream.Size = 0 then exit;

    strRTF:= ImageMng.ProcessImagesInRTF(Stream.Memory, Stream.Size, Self.Name, ImagesMode, '', 0, ContainsImgIDsRemoved, ContainsImages, ExitIfAllImagesInSameModeDest);
    if strRTF <> '' then begin                           // Changes in images must be reflected (=> ContainsImages=true)
       if ImageMng.StorageMode = smEmbRTF then           // If smEmbRTF -> we are calling from UpdateImagesStorageMode (when converting from a different mode to smEmbRTF)
          strRTF:= RemoveKNTHiddenCharactersInRTF(strRTF, hmOnlyImages);

       { *1 : I'm interested in adding a #0 at the end (in nodes/notes with RTF content), so that methods like ProcessImagesInRTF,
          which use the Pos() method directly on the Stream.Memory, always have an end...
          Note: When we execute FEditor.Lines.SaveToStream( FDataStream, Encoding); in the Stream it ends with a #0    }
       Stream.Size:= Length(StrRTF);
       Stream.Position := 0;
       StringToMemoryStream(StrRTF, Stream);
       if (StrRTF[Length(StrRTF)] <> #0) then                // *1
          Stream.WriteData(0);
    end;


    if ImageMng.StorageMode <> smEmbRTF then begin       // If = smEmbRTF =>  fChangingImagesStorage=True

       if ContainsImages then
          Result:= ImageMng.GetImagesIDInstancesFromRTF (Stream);
    end;
end;


function TKntFolder.GetImagesInstances: TImageIDs;
begin
   Result:= NoteUI.GetImagesInstances;
end;

procedure TKntFolder.ReconsiderImageDimensionGoalsOnEditor(Selection: boolean);
begin
   NoteUI.ReconsiderImageDimensionGoalsOnEditor(Selection, ImagesMode)
end;


procedure TKntFolder.SetImagesMode(ImagesMode: TImagesMode; ForceMode: boolean);
var
   RTFIn, RTFOut: AnsiString;
   currentNoteModified, currentFileModified: boolean;
   SS: integer;
   myTreeNode: PVirtualNode;
   RestoreRO: boolean;

begin
    if ForceMode or (FImagesMode <> ImagesMode) then begin
       NoteUI.SetImagesMode(ImagesMode);
       FImagesMode:= ImagesMode;
    end;
end;


procedure TKntFolder.SetImagesMode(ImagesMode: TImagesMode);
begin
   SetImagesMode(ImagesMode, false);
end;



{$ENDREGION}



// Load / Save  =========================================

{$REGION Load / Save }

function TKntFolder.SaveToFile( var tf : TTextFile; OnlyCurrentNodeAndSubtree: PVirtualNode= nil;
                               OnlyNotHiddenNodes: boolean= false; OnlyCheckedNodes: boolean= false ): integer;
var
  Node : PVirtualNode;
  nodessaved : integer;
  wasmismatch : boolean;
  LastSavedLevel: integer;

  procedure SaveNNode(Node: PVirtualNode);
  var
     NNode : TNoteNode;
     Level: integer;
  begin
     NNode:= TreeUI.GetNNode(Node);

     tf.WriteLine( _NF_NNode  );                                   // TNoteNode begins

     // _NoteGID (optionally) and _NodeGID, in this order, are the first fields to save
     if NNode.GID <> NNode.Note.GID then
        tf.WriteLine( _NoteGID + '=' + NNode.Note.GID.ToString );
     tf.WriteLine( _NodeGID + '=' + NNode.GID.ToString );

     if NNode.ID <> 0 then
        tf.WriteLine( _NodeID + '=' + NNode.ID.ToString );

     NNode.UpdateStates(TV);
     if NNode.States <> [] then
        tf.WriteLine(_NodeState + '=' + NNode.StatesToString(fReadOnly));

     Level:= TV.GetNodeLevel(Node);
     if Level <> LastSavedLevel then begin
        tf.WriteLine( _NodeLevel + '=' + Level.ToString );
        LastSavedLevel := Level;
     end;

     if NNode.EditorBGColor <> clNone then
        tf.WriteLine( _NodeEditorBGColor + '=' + ColorToString(NNode.EditorBGColor) );
     if NNode.ImageIndex <> 0 then
        tf.WriteLine( _NodeImageIndex + '=' + NNode.ImageIndex.ToString  );
     if NNode.NodeColor <> clNone then
        tf.WriteLine( _NodeColor + '=' + ColorToString(NNode.NodeColor) );
     if NNode.NodeBGColor  <> clNone then
        tf.WriteLine( _NodeBGColor + '=' + ColorToString(NNode.NodeBGColor) );
     if NNode.NodeFontFace <> '' then
        tf.WriteLine( _NodeFontFace + '=' + NNode.NodeFontFace );

      AlarmMng.SaveAlarms(tf, NNode, Self);

      inc(nodessaved);
  end;


begin
  nodessaved := 0;

  // sanity check

  wasmismatch := TV.TotalCount <> fNNodes.Count;
  if wasmismatch then begin
	   if (App.DoMessageBox( Format(GetRS(sFld05), [FName,FTV.TotalCount, fNNodes.Count]), mtWarning, [mbYes, mbNo], def1 ) <> mrYes ) then
         raise EKntFolderError.Create(GetRS(sFld07));

  end;


  if assigned( TV.FocusedNode ) then
     FSavedSelectedIndex := TV.AbsoluteIndex(TV.FocusedNode)
  else
     FSavedSelectedIndex := -1;


  try
     tf.WriteLine( _NF_Folder ); // marks beginning of TKntFolder

     FSavedTabIndex := FTabSheet.PageIndex; // remember tab position

     tf.WriteLine( _FolderName + '=' + FName,  True);
     tf.WriteLine( _FolderID + '=' + FID.ToString  );
     tf.WriteLine( _ImageIndex + '=' + FImageIndex.ToString  );
     tf.WriteLine( _DateCreated + '=' + FormatDateTime( _SHORTDATEFMT + #32 + _LONGTIMEFMT, FDateCreated ) );
     tf.WriteLine( _TabIndex + '=' + FSavedTabIndex.ToString  );
     tf.WriteLine( _TabSize + '=' + inttostr( FTabSize )  );

     // Editor chrome
     tf.WriteLine( _CHBGColor + '=' + ColorToString( FEditorChrome.BGColor )  );
     tf.WriteLine( _CHFontCharset + '=' + inttostr( FEditorChrome.Font.Charset )  );
     tf.WriteLine( _CHFontColor + '=' + ColorToString( FEditorChrome.Font.Color )  );
     tf.WriteLine( _CHFontName + '=' + FEditorChrome.Font.Name  );
     tf.WriteLine( _CHFontSize + '=' + FEditorChrome.Font.Size.ToString  );
     tf.WriteLine( _CHLanguage + '=' + IntToStr(FEditorChrome.Language) );
     tf.WriteLine( _CHFontStyle + '=' + FontStyleToStr( FEditorChrome.Font.Style )  );
     tf.WriteLine( _Flags + '=' + PropertiesToFlagsString  );

     AlarmMng.SaveAlarms(tf, nil, Self);


    // basic tree properties
    tf.WriteLine( _SelectedNode + '=' + FSavedSelectedIndex.ToString );
    tf.WriteLine( _TreeWidth + '=' + FTreeWidth.ToString );
    tf.WriteLine( _TreeMaxWidth + '=' + Abs(FTreeMaxWidth).ToString );
    tf.WriteLine( _DefaultNoteName + '=' + FDefaultNoteName,  True);

    // tree chrome
    tf.WriteLine( _CHTRBGColor + '=' + ColorToString( FTreeChrome.BGColor ) );
    tf.WriteLine( _CHTRFontCharset + '=' + intToStr( FTreeChrome.Font.Charset ) );
    tf.WriteLine( _CHTRFontColor + '=' + ColorToString( FTreeChrome.Font.Color ) );
    tf.WriteLine( _CHTRFontName + '=' + FTreeChrome.Font.Name );
    tf.WriteLine( _CHTRFontSize + '=' + FTreeChrome.Font.Size.ToString );
    tf.WriteLine( _CHTRFontStyle + '=' + FontStyleToStr( FTreeChrome.Font.Style ) );


    tf.WriteLine(_NumNNodes + '=' + fNNodes.Count.ToString);

    LastSavedLevel:= -1;

    // obtain first node
    if OnlyCurrentNodeAndSubtree <> nil then
       Node := OnlyCurrentNodeAndSubtree
    else
       Node := TV.GetFirst;

    while assigned(Node) do begin
      if not ( (OnlyCheckedNodes   and (Node.CheckState <> csCheckedNormal)) or
               (OnlyNotHiddenNodes and not TV.IsVisible[Node])
                )  then
          SaveNNode(Node);

      // obtain next node, or bail out if NIL
      Node := TV.GetNext(Node);
      if OnlyCurrentNodeAndSubtree <> nil then begin
          if (OnlyCurrentNodeAndSubtree.NextSibling = Node) then
             Node := nil;
      end;
    end;

    Modified := false;

  finally
    if (OnlyCurrentNodeAndSubtree = nil) and not OnlyCheckedNodes and not OnlyNotHiddenNodes
       and ( nodessaved <> FNNodes.Count ) then
        raise EKntFolderError.CreateFmt(GetRS(sFld09), [FNNodes.Count, nodessaved]);

    Result:= nodesSaved;
  end;

end; // SaveToFile


procedure TKntFolder.GetNotesToBeSaved(NoteList: TNoteList;
                                      OnlyCurrentNodeAndSubtree: PVirtualNode= nil;
                                      OnlyNotHiddenNodes: boolean= false;
                                      OnlyCheckedNodes: boolean= false );
var
  Node : PVirtualNode;
  NNode: TNoteNode;

begin

  // obtain first node
  if OnlyCurrentNodeAndSubtree <> nil then
     Node := OnlyCurrentNodeAndSubtree
  else
     Node := TV.GetFirst;

  while assigned(Node) do begin
    if not ( (OnlyCheckedNodes   and (Node.CheckState <> csCheckedNormal)) or
             (OnlyNotHiddenNodes and not TV.IsVisible[Node])
              )  then begin
        NNode:= TreeUI.GetNNode(Node);
        if NoteList.IndexOf(NNode.Note) < 0 then
           NoteList.Add(NNode.Note);
    end;

    // obtain next node, or bail out if NIL
    Node := TV.GetNext(Node);
    if OnlyCurrentNodeAndSubtree <> nil then begin
        if (OnlyCurrentNodeAndSubtree.NextSibling = Node) then
           Node := nil;
    end;
  end;

end; // GetNotesToBeSaved




procedure TKntFolder.NoteFlagsStringToProperties(const FlagsStr : TFlagsString; NNode: TNoteNode);
var
  Checked, Bold: boolean;
begin
  if ( length( FlagsStr ) < FLAGS_STRING_LENGTH ) then exit;

  if FlagsStr[1] = BOOLEANSTR[true] then
     Include(NNode.States, nnsSaved_Checked);

  if FlagsStr[2] = BOOLEANSTR[true] then
     Include(NNode.States, nnsFlagged);

   NNode.Bold := FlagsStr[3] = BOOLEANSTR[true];

  case FlagsStr[10] of
    '1' : NNode.WordWrap:= TNodeWordWrap.wwYes;
    '2' : NNode.WordWrap:= TNodeWordWrap.wwNo;
  end;

  // backward-compatibility hassle:
  case FlagsStr[4] of
    '1'..'9' : begin
      if ( FlagsStr[5] = BOOLEANSTR[true] ) then // "bright font" flag
        NNode.NodeColor := _NODE_COLORS_LIGHT[strtoint( FlagsStr[4] )]
      else
        NNode.NodeColor := _NODE_COLORS_DARK[strtoint( FlagsStr[4] )];
    end;
  end;

  if FlagsStr[7] = BOOLEANSTR[true] then
     Include(NNode.States, nnsSaved_Expanded);

  NNode.ChildrenCheckbox:= FlagsStr[11] = BOOLEANSTR[true];
 //NNode.Filtered:= FlagsStr[12] = BOOLEANSTR[true];

end;


procedure TKntFolder.LoadFromFile( var tf : TTextFile; var FileExhausted : boolean; var NextBlock: TNextBlock; LoadOldSimpleNote: boolean= false);
var
  InRichText : boolean;
  InNoteNode : boolean;
  List : TStringList;
  s, key : AnsiString;
  p, linecount : integer;

  Note: TNote;
  NNode: TNoteNode;
  GID: Cardinal;
  VirtualFN, RelativeVirtualFN: string;
  C: TColor;
  clWindowIsNotWhite: boolean;
  KntFile: TKntFile;
  Level, LastLoadedLevel: Byte;

  NumNNodes, iNNode: integer;

    procedure AddNewNNode;
    begin
       if (NNode = nil) or (Note = nil) then exit;

       Note.AddNNode(NNode, Self);
    end;

    procedure AddTextToNewNode;          // => KntFile.Version.Major < 3
    var
      IsRTF: boolean;
    begin
      InRichText := false;

      if (VirtualFN <> '') or (RelativeVirtualFN <> '') then
         TKntFile(KntFile).LoadVirtualNote (Note, VirtualFN, RelativeVirtualFN, List)

      else begin
         TransferNEntryText(List, Note.Entries[0].Stream, IsRTF);       // transfer Text data (RTF or plain text) from list to Note Entry
         Note.Entries[0].IsRTF:= IsRTF;
      end;

      List.Clear;
      VirtualFN:= '';
      RelativeVirtualFN:= '';
    end;

   {
   In version 1.6.9, the creation and modification dates of nodes where saved in the NT= key:
   NT=20200113T115321.20241211T115236
   (Creation date.Last Modification date in format: YYYYMMDDtHHMMSS.YYYYMMDDtHHMMSS)
   Ref: https://github.com/dpradov/keynote-nf/discussions/767#discussioncomment-11531697
   }
   function OldCreationAndModificationDates(strCreationAndModifDates: string; var CreationDate: TDateTime; var ModificationDate: TDateTime): boolean;

      function GetDate(const strDT: string): TDateTime;
      var
        Year, Month, Day: Word;
        Hour, Min, Sec: Word;
      begin
        Year  := StrToInt(Copy(strDT, 1, 4));
        Month := StrToInt(Copy(strDT, 5, 2));
        Day   := StrToInt(Copy(strDT, 7, 2));
        Hour  := StrToInt(Copy(strDT, 10, 2));
        Min   := StrToInt(Copy(strDT, 12, 2));
        Sec   := StrToInt(Copy(strDT, 14, 2));
        Result:= EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);
      end;

   begin
     Result:= false;
     if (strCreationAndModifDates.Length <> 31) or (strCreationAndModifDates[16] <> '.') then exit;

     try
        CreationDate:= GetDate(strCreationAndModifDates);
        Delete(strCreationAndModifDates, 1, 16);
        ModificationDate:= GetDate(strCreationAndModifDates);

        Result:= True;
     except
     end;
   end;

begin
  KntFile:= TKntFile(Self.KntFile);

  FileExhausted := false;
  InRichText := false;
  InNoteNode := false;

  NNode:= nil;
  Note:= nil;
  iNNode:= -1;

  clWindowIsNotWhite:= not (ColorToRGB(clWindow) = ColorToRGB(clWhite));

  List := TStringList.Create;
  List.BeginUpdate;
  try
    while (not tf.eof()) do begin
       s:= tf.readln();

       if KntFile.Version.Major < '3' then begin
             if ( s = _NF_RTF ) then begin
               // RTF data begins
               InRichText := true;
               if LoadOldSimpleNote then begin
                  InNoteNode := true;
                  FTreeHidden:= true;
                  FIconKind := niCustom;
                  NNode:= TKntFile(KntFile).AddLoadedNote(Self);    // create a new blank node (=> TNote, TNoteEntry, TNoteNode)
                  Note:= NNode.Note;
                  Note.Name:= FName;
                  LoadingLevels.Add(0);
                  inc(iNNode);
               end;
               continue;
             end;
             if ( s = _NF_TRN ) then begin
               // new NoteNode begins
               if ( InNoteNode ) then AddTextToNewNode; // we were here previously, i.e. there a node to be added
               InNoteNode := true;
               NNode:= TKntFile(KntFile).AddLoadedNote(Self);
               Note:= NNode.Note;
               LoadingLevels.Add(0);
               inc(iNNode);
               continue;
             end;
             if ( s = _NF_TabFolder ) then begin
               NextBlock:= nbRTF;
               if assigned(NNode) then AddTextToNewNode;
               break; // New TabNote begins
             end;
             if ( s = _NF_TreeFolder ) then begin
               NextBlock:= nbTree;
               if assigned(NNode) then AddTextToNewNode;
               break; // New TreeNote begins
             end;
             if ( s = _NF_StoragesDEF ) then begin
               NextBlock:= nbImages;
               if assigned(NNode) then AddTextToNewNode;
               break; // Images definition begins
             end;
             if ( s = _NF_Bookmarks ) then begin
               NextBlock:= nbBookmarks;
               if assigned(NNode) then AddTextToNewNode;
               break; // Bookmarks begins
             end;
             if ( s = _NF_EOF ) then begin
               FileExhausted := true;
               if assigned(NNode) then AddTextToNewNode;
               break; // END OF FILE
             end;

             if InRichText then begin
               { can only be TRUE if the file uses the new "LI-less" format,
                 because we got the _NF_EOH token, above. Old format doesn't
                 have this token, so InRichText is never true }
               if FDefaultPlainText then
                 delete( s, 1, 1 ); // strip _NF_PLAINTEXTLEADER
               List.Add( s );
               continue;
             end;


             p := pos( '=', s );
             if ( p <> 3 ) then continue; // not a valid key=value format
             key := copy( s, 1, 2 );
             delete( s, 1, 3 );


             if InNoteNode and (not LoadOldSimpleNote) then begin
                if ( key = _NoteName ) then
                  Note.Name:= TryUTF8ToUnicodeString(s)
                else
                if ( key = _NoteGID ) then
                    Note.GID:= StrToUIntDef(s, 0)
                else
                if ( key = _NodeID ) then
                    NNode.ID := StrToUIntDef(s, 0)
                else
                if ( key = _NodeLevel ) then
                    LoadingLevels[iNNode]:= StrToIntDef(s, 0)
                else
                if ( key = _NodeFlags ) then
                    NoteFlagsStringToProperties(s, NNode)
                else
                if ( key = _NodeEditorBGColor ) then begin
                  try
                    { The default value that has been saved in Note.FRTFBGColor has been clWindow (a value that has always been persisted,
                      when when setting the color, it has never been possible to set clWindow but rather clWhite, because it has been set via RGB) }
                    C:= StringToColor(s);
                    if (C <> EditorChrome.BGColor) and (clWindowIsNotWhite or not ((C = clWindow) and (EditorChrome.BGColor = clWhite))) then
                       NNode.EditorBGColor:= C;
                  except
                  end;
                end
                else
                if ( key = _VirtualNode ) then
                   Note.SetLoadingAsOldMirror(s)
                else
                if ( key = _RelativeVirtualFN ) then
                  RelativeVirtualFN := TryUTF8ToUnicodeString(s)
                else
                if ( key = _VirtualFN ) then
                  VirtualFN := TryUTF8ToUnicodeString(s)
                else
                if ( key = _NEntrySelStart ) then begin
                    if _SAVE_RESTORE_CARETPOS then
                       Note.SelStart := StrToIntDef( s, 0 )
                    else
                       Note.SelStart := 0;
                end
                else
                if ( key = _NodeImageIndex ) then
                    NNode.ImageIndex:= StrToIntDef( s, -1 )
                else
                if ( key = _NodeColor ) then begin
                  try
                    C:= StringToColor(s);
                    if C <> TreeChrome.Font.Color then
                       NNode.NodeColor:= C;
                  except
                  end;
                end
                else
                if ( key = _NodeBGColor ) then begin
                  try
                    C:= StringToColor(s);
                    if (C <> TreeChrome.BGColor) then
                       NNode.NodeBGColor:= C;
                  except
                  end;
                end
                else
                if ( key = _NodeFontFace ) then begin
                  if s <> TreeChrome.Font.Name then
                     NNode.NodeFontFace:= s;
                end
                else
                if ( key = _NodeAlarm ) then
                    AlarmMng.ProcessAlarm(s, NNode, Self)
                else
                if ( key = _NodeDC_DM ) then begin
                    var DC,DM: TDateTime;
                    if OldCreationAndModificationDates( s, DC, DM) then begin
                       Note.Entries[0].Created:= DC;
                       Note.LastModified:= DM;
                    end;
                end;

                continue;
             end; // if InNoteNode ...

       end
       else begin
          // ---------------------   KntFile.Version.Major >= '3'

             if ( s = _NF_NNode ) then begin
               // new TNoteNode begins
               InNoteNode := true;
               NNode:= nil;
               Note:= nil;
               continue;
             end;

             if ( s = _NF_Folder ) then begin
               NextBlock:= nbTree;
               break; // New Folder begins
             end;
             if ( s = _NF_StoragesDEF ) then begin
               NextBlock:= nbImages;
               break; // Images definition begins
             end;
             if ( s = _NF_Bookmarks ) then begin
               NextBlock:= nbBookmarks;
               break; // Bookmarks begins
             end;
             if ( s = _NF_EOF ) then begin
               FileExhausted := true;
               break; // END OF FILE
             end;


             p := pos( '=', s );
             if ( p <> 3 ) then continue; // not a valid key=value format
             key := copy( s, 1, 2 );
             delete( s, 1, 3 );

             if Copy(s,1,2) = _NumNNodes then begin
                NumNNodes:= StrToIntDef(Copy(s,4), 0);
                fNNodes.Capacity:= NumNNodes;
                LoadingLevels.Capacity:= NumNNodes;
                continue;
             end;


             if InNoteNode  then begin
                if ( key = _NoteGID ) then begin
                    Note:= KntFile.GetNoteByGID(StrToUIntDef(s, 0));
                end
                else
                if ( key = _NodeGID ) then begin
                    GID:= StrToUIntDef(s, 0);
                    if Note = nil then
                       Note:= KntFile.GetNoteByGID(GID);

                    if Note <> nil then begin
                       NNode:= KntFile.AddLoadedNNode(Note, Self, GID);
                       LoadingLevels.Add(LastLoadedLevel);   // By default
                       inc(iNNode);
                    end;
                end
             end;

             if InNoteNode and (NNode <> nil) then begin
                if ( key = _NodeID ) then
                    NNode.ID := StrToUIntDef(s, 0)
                else
                if ( key = _NodeLevel ) then begin
                    Level:= StrToIntDef(s,0);
                    LoadingLevels[iNNode]:= Level;
                    LastLoadedLevel:= Level;
                end
                else
                if ( key = _NodeState ) then
                    NNode.StringToStates(s)
                else
                if ( key = _NodeEditorBGColor ) then begin
                  try
                    C:= StringToColor(s);
                    if (C <> EditorChrome.BGColor) and (clWindowIsNotWhite or not ((C = clWindow) and (EditorChrome.BGColor = clWhite))) then
                       NNode.EditorBGColor:= C;
                  except
                  end;
                end
                else
                if ( key = _NodeImageIndex ) then
                    NNode.ImageIndex:= StrToIntDef( s, -1 )
                else
                if ( key = _NodeColor ) then begin
                  try
                    C:= StringToColor(s);
                    if C <> TreeChrome.Font.Color then
                       NNode.NodeColor:= C;
                  except
                  end;
                end
                else
                if ( key = _NodeBGColor ) then begin
                  try
                    C:= StringToColor(s);
                    if (C <> TreeChrome.BGColor) then
                       NNode.NodeBGColor:= C;
                  except
                  end;
                end
                else
                if ( key = _NodeFontFace ) then begin
                  if s <> TreeChrome.Font.Name then
                     NNode.NodeFontFace:= s;
                end
                else
                if ( key = _NodeAlarm ) then
                    AlarmMng.ProcessAlarm(s, NNode, Self);

                continue;
             end; // if InNoteNode ...


       end;



       if ( key = _SelectedNode ) then
             FSavedSelectedIndex := StrToIntDef( s, -1 )
       else
       if ( key = _TreeWidth ) then
          FTreeWidth := StrToIntDef( s, 0)
       else
       if ( key = _TreeMaxWidth ) then
          FTreeMaxWidth := StrToIntDef( s, 0)
       else
       if ( key = _DefaultNoteName ) then begin
           if ( s <> '' ) then
              FDefaultNoteName := TryUTF8ToUnicodeString(s);
       end
       else
       if ( key = _CHTRBGColor ) then begin
           try
             FTreeChrome.BGColor := StringToColor( s );
           except
           end;
       end
       else
       if ( key = _CHTRFontCharset ) then
          FTreeChrome.Font.Charset := StrToIntDef( s, DEFAULT_CHARSET)
       else
       if ( key = _CHTRFontColor ) then
           FTreeChrome.Font.Color := StringToColor( s )
       else
       if ( key = _CHTRFontName ) then
           FTreeChrome.Font.Name := s
       else
       if ( key = _CHTRFontSize ) then
           FTreeChrome.Font.Size := StrToIntDef( s, 10)
       else
       if ( key = _CHTRFontStyle ) then
           FTreeChrome.Font.Style := StrToFontStyle( s )
       else
       if ( key = _CHBGColor ) then begin
           try
             FEditorChrome.BGColor := StringToColor( s );
           except
           end;
       end
       else
       if ( key = _CHFontCharset ) then
           FEditorChrome.Font.Charset := StrToIntDef( s, DEFAULT_CHARSET )
       else
       if ( key = _CHFontColor ) then
           FEditorChrome.Font.Color := StringToColor( s )
       else
       if ( key = _CHFontName ) then
           FEditorChrome.Font.Name := s
       else
       if ( key = _CHFontSize ) then
             FEditorChrome.Font.Size := StrtoIntDef( s, 10 )
       else
       if ( key = _CHFontStyle ) then
           FEditorChrome.Font.Style := StrToFontStyle( s )
       else
       if ( key = _CHLanguage ) then begin
           try
             FEditorChrome.Language := StrToInt( s );
           except
           end;
       end
       else
       if ( key = _DateCreated ) then
           FDateCreated := StrToDateTimeDef( s, Now, LongDateToFileSettings)
       else
       if ( key = _ImageIndex ) then
             FImageIndex := StrToIntDef( s, 0 )
       else
       if ( key = _LineCount ) then begin
          linecount := StrToIntDef( s, DEFAULT_CAPACITY );
          if ( List.Capacity < linecount ) then
             List.Capacity := succ( linecount );
       end
       else
       if ( key = _FolderName ) then
           FName := TryUTF8ToUnicodeString(s)
       else
       if ( key = _FolderID ) then
           FID := StrToUIntDef( s, 0 )        // 0 -> owning file will generate new ID when note is added
       else
       if ( key = _Flags ) then
           FlagsStringToProperties( s )
       else
       if ( key = _TabIndex ) then
           FSavedTabIndex := StrToIntDef( s, 0 )
       else
       if ( key = _TabSize ) then
          FTabSize := StrToIntDef( s, DEF_TAB_SIZE )
       else
       if ( key = _NodeAlarm ) then
           AlarmMng.ProcessAlarm(s, nil, Self);

    end; { while not eof( tf ) }

  finally
    List.EndUpdate;
    List.Free;
  end;

  FModified := false;

end; // LoadFromFile


procedure TKntFolder.LoadFromTreePadFile( const FN : string );
var
  s, nodeName : string;
  tf : TTextFile;
  InNode : boolean;
  level : integer;
  myNote : TNote;
  NNode: TNoteNode;
  Note: TNote;
  List : TStringList;

    procedure AddNewNode;
    var
      IsRTF: boolean;
    begin
      // transfer Text data (RTF or plain text) from list to Note Entry
      TransferNEntryText(List, Note.Entries[0].Stream, IsRTF);
      Note.Entries[0].IsRTF:= IsRTF;
      List.Clear;

      Note:= nil;
    end;

begin
  InNode := false;

  if ( not fileexists( FN )) then exit;

  tf:= TTextFile.Create();
  tf.AssignFile(FN);

  try
    tf.Reset;
  except
    App.ErrorPopup( GetRS(sFld11) + FN);
    exit;
  end;


  List := TStringList.Create;

  try

    while ( not tf.Eof) do begin
       s:= tf.readln;

       case InNode of

         false : begin
            if ( s = _TREEPAD_NODE ) then begin
              InNode := true;
              try
                nodeName:= TryUTF8ToUnicodeString(tf.Readln);
                s:= tf.Readln; // node level
                level := StrToIntDef(s, 0);
                NNode:= TKntFile(KntFile).AddLoadedNote(Self);
                Note:= NNode.Note;
                Note.Name:= nodeName;
                LoadingLevels.Add(level);

              except
                InNode := false;
              end;
              continue;
            end;
         end;

         true : begin
            if ( s = _TREEPAD_ENDNODE ) then begin
               InNode := false;
               AddNewNode;
               Continue;
            end;
            List.Add( s );
        end;

      end;

    end;

  finally
    TKntFile(KntFile).VerifyNoteGIDs;
    List.Free;
    tf.CloseFile;
  end;

end; // LoadFromTreePadFile


procedure LoadStreamInRTFAux(Stream: TMemoryStream; RTFAux: TAuxRichEdit);
begin
    Stream.Position := 0;
    RTFAux.Clear;
    RTFAux.StreamMode := [];

    if NodeStreamIsRTF (Stream) then
       RTFAux.StreamFormat:= sfRichText
    else
       RTFAux.StreamFormat:= sfPlainText;
    try
       RTFAux.Lines.LoadFromStream( Stream );
    except
    end;
end;


function TKntFolder.PropertiesToFlagsString : TFlagsString;
begin
  result := DEFAULT_FLAGS_STRING;
  result[1] := BOOLEANSTR[FVisible];
  result[2] := BOOLEANSTR[FReadOnly];
  result[3] := BOOLEANSTR[FWordWrap];
  result[4] := BOOLEANSTR[FURLDetect];
  result[5] := BOOLEANSTR[FUseTabChar];
  result[6] := BOOLEANSTR[FDefaultPlainText];
  if not FReadonly then
     result[7] := BOOLEANSTR[FFiltered];

  result[8] := BOOLEANSTR[FRTL];
  result[9] := BOOLEANSTR[FTagSelectorDisabled];

  result[13] := AnsiChar(inttostr( ord( FIconKind ))[1]);
  //result[14] := BOOLEANSTR[FAutoNumberNotes];
  result[15] := BOOLEANSTR[FCheckboxes];
  result[16] := BOOLEANSTR[FVerticalLayout];

  // added in 1.5.9:
  result[17] := BOOLEANSTR[FTreeHidden];
  result[18] := BOOLEANSTR[(( not FTreeHidden ) and ( FFocusMemory = focTree ))];

  // added in 1.7.0:          // [dpv]
  result[19] := BOOLEANSTR[FHideCheckedNodes];


  if FPosDateCol > 0 then
     FPosDateCol:= TV.Header.Columns[1].Position+1;
  if FPosFlaggedCol > 0 then
     FPosFlaggedCol:= TV.Header.Columns[2].Position+1;
  result[20] := AnsiChar(IntToStr(FPosDateCol)[1]);
  result[21] := AnsiChar(IntToStr(FPosFlaggedCol)[1]);

  result[22] := BOOLEANSTR[FEditorInfoPanelHidden];
end;


procedure TKntFolder.FlagsStringToProperties( const FlagsStr : TFlagsString );
begin
  if ( length( FlagsStr ) < FLAGS_STRING_LENGTH ) then exit;
  FVisible    := FlagsStr[1] = BOOLEANSTR[true];
  FReadOnly   := FlagsStr[2] = BOOLEANSTR[true];
  FWordWrap   := FlagsStr[3] = BOOLEANSTR[true];
  FURLDetect  := FlagsStr[4] = BOOLEANSTR[true];
  FUseTabChar := FlagsStr[5] = BOOLEANSTR[true];
  FDefaultPlainText  := FlagsStr[6] = BOOLEANSTR[true];
  FFiltered   := FlagsStr[7] = BOOLEANSTR[true];
  FRTL        := FlagsStr[8] = BOOLEANSTR[true];
  FTagSelectorDisabled  := FlagsStr[9] = BOOLEANSTR[true];

  case FlagsStr[13] of
    '0' : FIconKind := niNone;
    '1' : FIconKind := niStandard;
    '2' : FIconKind := niCustom;
  end;
  //FAutoNumberNotes := FlagsStr[14] = BOOLEANSTR[true];
  FCheckboxes      := FlagsStr[15] = BOOLEANSTR[true];
  FVerticalLayout  := FlagsStr[16] = BOOLEANSTR[true];

  FTreeHidden := FlagsStr[17] = BOOLEANSTR[true];
  if (( not FTreeHidden ) and ( FlagsStr[18] = BOOLEANSTR[true] )) then
    FFocusMemory := focTree
  else
    FFocusMemory := focRTF;

  FHideCheckedNodes      := FlagsStr[19] = BOOLEANSTR[true];

  FPosDateCol:= Ord(FlagsStr[20])-Ord('0');
  FPosFlaggedCol:= Ord(FlagsStr[21])-Ord('0');

  FEditorInfoPanelHidden := FlagsStr[22] = BOOLEANSTR[true];
end;

{$ENDREGION }



Initialization

end.
