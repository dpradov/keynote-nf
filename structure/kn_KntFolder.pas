unit kn_KntFolder;

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
   Vcl.Graphics,
   Vcl.FileCtrl,
   Vcl.Controls,
   Vcl.ComCtrls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.ExtCtrls,
   comctrls95,
   TreeNT,
   RxRichEd,

   gf_streams,
 {$IFDEF KNT_DEBUG}
   GFLog,
 {$ENDIF}
   kn_KntNote,
   kn_StyleObj,
   kn_Info,
   kn_Const,
   knt.ui.editor,
   kn_History
   {$IFDEF WITH_IE}
   ,SHDocVw_TLB
   {$ENDIF}
   ;


type
  EKntFolderError = class( Exception );


type
  TBeforeEditorLoadedEvent = procedure(Note: TKntNote) of object;
  TAfterEditorLoadedEvent  = procedure(Note: TKntNote) of object;

  TKntFolder = class( TPersistent )
  private
    FEditorChrome : TChrome; // user-defined fonts, colors etc.
    FID : Cardinal; // unique folder ID
    FKntFile: TObject;

    FName : TNoteNameStr; // user-defined name for the folder object
    FVisible : boolean; // UNUSED but may be useful later (with specialized folders)
    FReadOnly : boolean;
    FInfo : longint; // internal use only
    FTag : longint;
    FModified : boolean;
    FDateCreated : TDateTime;
    FImageIndex : integer;
    FPlainText : boolean; // if true, contents of editor are saved as plain text
    FFocusMemory : TFocusMemory; // which control was last focused

    FWordWrap : boolean;  // for RTF-type notes only
    FURLDetect : boolean; // highlight URLs; for RTF-type notes only
    FTabSize : byte;  // for RTF-type notes only
    FUseTabChar : boolean;  // for RTF-type notes only
    FIsInsertMode : boolean;  // for RTF-type notes only
    FCaretPos : TPoint; // caret position saved to data file (to be restored on Load; do not use otherwise because it is ONLY updated on saving file!)

    FAuxiliarAlarmList: TList;

    FTabIndexAux : integer;

    // VCL controls. By default, the folder does NOT access them in any way
    FTabSheet : TTab95Sheet; // the tabsheet which holds this note
    FEditor : TKntRichEdit;  // for RTF-type notes only

    FHistory : TKNTHistory;        // Folder (local) history

    FImagesMode: TImagesMode;
    fImagesReferenceCount: TImageIDs;

    //-----------
    FNotes : TKntNoteList;
    FSplitter : TSplitter;
    FTV : TTreeNT;
    FIconKind : TNodeIconKind;
    FTreeChrome : TChrome;
    FSelectedNote : TKntNote;
    FOldSelectedIndex : integer;
    FTreeHidden : boolean;
    FHideCheckedNodes: boolean;       // [dpv]
    FFiltered: boolean;               // [dpv]

    FDefaultNoteName : string;
    FAutoNumberNotes : boolean;
    FCheckboxes : boolean;
    FVerticalLayout : boolean;

    {$IFDEF WITH_IE}
    FMainPanel : TPanel;
    FWebBrowser : TWebBrowser;
    {$ENDIF}

    // state that needs to be recalled
    FTreeWidth : integer;
    FTreeMaxWidth: integer;
    //-----------


    // events
    FOnChange : TNotifyEvent;
    FOnBeforeEditorLoaded: TBeforeEditorLoadedEvent;
    FOnAfterEditorLoaded: TAfterEditorLoadedEvent;

    procedure SetName( AName : TNoteNameStr );
    procedure SetID( ID : Cardinal );
    procedure SetReadOnly( AReadOnly : boolean );
    procedure SetPlainText( APlainText : boolean );
    function GetTabIndex: integer;
    procedure SetTabIndex( ATabIndex : integer );
    procedure SetModified( AModified : boolean );
    function  GetModified : boolean;
    procedure SetImageIndex( AImgIdx : integer );
    procedure SetEditorChrome( AChrome : TChrome );
    procedure SetTabSheet( ATabSheet : TTab95Sheet );

    procedure SetWordWrap( AWordWrap : boolean );
    procedure SetURLDetect( AURLDetect : boolean );
    procedure SetTabSize( ATabSize : byte );
    procedure SetEditor( AEditor : TKntRichEdit );
    function CheckEditor : boolean;
    function CheckTabSheet : boolean;

    procedure BaseSaveProc( var tf : TTextFile );

    function PropertiesToFlagsString : TFlagsString;
    procedure FlagsStringToProperties( const FlagsStr : TFlagsString );

    procedure SaveAlarms(var tf : TTextFile; note: TKntNote = nil);
    procedure ProcessAlarm (s: AnsiString; note: TKntNote = nil);

    procedure SetImagesMode(ImagesMode: TImagesMode); overload;

    //-----------
    function GetCount : integer;
    function CheckTree : boolean;
    procedure SetTreeChrome( AChrome : TChrome );
    procedure SetSelectedNote( aNote : TKntNote );
    function InternalAddNote( const aNote : TKntNote ) : integer;
    procedure InternalInsertNote( const aIndex : integer; const aNote : TKntNote );
    procedure GenerateNoteID( const aNote : TKntNote );
    procedure VerifyNoteIDs;

  protected
    procedure BeforeEditorLoaded(Note: TKntNote); dynamic;
    procedure AfterEditorLoaded(Note: TKntNote); dynamic;

  public
    class function NewKntFolder(const DefaultFolder, CanFocus : boolean) : boolean;
    class procedure CreateNewKntFolder;
    class procedure DeleteKntFolder;
    class procedure RenameKntFolder;
    class procedure EditKntFolderProperties( const PropertiesAction : TPropertiesAction );


    property Editor : TKntRichEdit read FEditor write SetEditor;
    property ID : Cardinal read FID write SetID;
    property KntFile: TObject read FKntFile;

    property EditorChrome : TChrome read FEditorChrome write SetEditorChrome;
    property Name : TNoteNameStr read FName write SetName;
    property ImageIndex : integer read FImageIndex write SetImageIndex;
    property Visible : boolean read FVisible write FVisible;
    property ReadOnly : boolean read FReadOnly write SetReadOnly;
    property Info : longint read FInfo write FInfo;
    property Tag : longint read FTag write FTag;
    property Modified : boolean read GetModified write SetModified;
    property DateCreated : TDateTime read FDateCreated write FDateCreated;
    function GetSelectedNote : TKntNote;

    property History : TKNTHistory read FHistory;

    property ImagesMode: TImagesMode read FImagesMode write SetImagesMode;
    property ImagesReferenceCount: TImageIDs read fImagesReferenceCount write fImagesReferenceCount;
    property ImagesInstances: TImageIDs read fImagesReferenceCount;

    property PlainText : boolean read FPlainText write SetPlainText;
    property WordWrap : boolean read FWordWrap write SetWordWrap;
    property URLDetect : boolean read FURLDetect write SetURLDetect;
    property TabSize : byte read FTabSize write SetTabSize;
    property UseTabChar : boolean read FUseTabChar write FUseTabChar;
    property IsInsertMode : boolean read FIsInsertMode write FIsInsertMode;
    property CaretPos : TPoint read FCaretPos write FCaretPos;
    property FocusMemory : TFocusMemory read FFocusMemory write FFocusMemory;

    property TabIndex : integer read GetTabIndex write SetTabIndex;
    property TabSheet : TTab95Sheet read FTabSheet write SetTabSheet;

    // events
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnBeforeEditorLoaded: TBeforeEditorLoadedEvent read FOnBeforeEditorLoaded write FOnBeforeEditorLoaded;
    property OnAfterEditorLoaded: TAfterEditorLoadedEvent read FOnAfterEditorLoaded write FOnAfterEditorLoaded;

    constructor Create(KntFile: TObject);
    destructor Destroy; override;

    procedure SaveRTFToFile(var tf : TTextFile; DataStream : TMemoryStream; PlainText: Boolean; PlaintextLeader: AnsiString = _NF_PLAINTEXTLEADER);


{$IFDEF WITH_DART}
    procedure SaveDartNotesFormat( Stream : TStream ); virtual;
    procedure LoadDartNotesFormat( Stream : TStream ); virtual;
{$ENDIF}

    procedure SetEditorProperties( const aProps : TFolderEditorProperties );
    procedure GetEditorProperties( var aProps : TFolderEditorProperties );
    procedure SetTabProperties( const aProps : TFolderTabProperties; UpdateName: boolean= True );
    procedure GetTabProperties( var aProps : TFolderTabProperties );

    procedure UpdateEditor;
    procedure UpdateTabSheet;

    function PrepareTextPlain (myTreeNode: TTreeNTNode; RTFAux: TAuxRichEdit): string;
    function GetTextPlainFromNode (myTreeNode: TTreeNTNode; RTFAux: TAuxRichEdit): string;

    procedure ConfigureEditor;
    procedure DataStreamToEditor;
    function EditorToDataStream: TMemoryStream;

    procedure GetImagesIDInstances (Stream: TMemoryStream; TextPlain: String);
    procedure ResetImagesReferenceCount;
    function CheckSavingImagesOnMode (ImagesMode: TImagesMode;
                                      Stream: TMemoryStream;
                                      ExitIfAllImagesInSameModeDest: boolean = true): TImageIDs;
    procedure ReloadImagesOnEditor;
    procedure ReconsiderImageDimensionGoalsOnEditor (Selection: boolean);
    procedure SetImagesMode(ImagesMode: TImagesMode; ForceMode: boolean); overload;

    function GetAlarms(considerDiscarded: boolean): TList;
    function HasAlarms (considerDiscarded: boolean): boolean;

    procedure AddProcessedAlarms ();
    procedure AddProcessedAlarmsOfFolder (newFolder: TKntFolder);
    procedure AddProcessedAlarmsOfNote (note: TKntNote; newFolder: TKntFolder; newNote: TKntNote);


    //-----------
    property Notes : TKntNoteList read FNotes;
    property NoteCount : integer read GetCount;
    property Splitter : TSplitter read FSplitter write FSplitter;
    property TV : TTreeNT read FTV write FTV;
    property IconKind : TNodeIconKind read FIconKind write FIconKind;
    property TreeWidth : integer read FTreeWidth write FTreeWidth;
    property TreeMaxWidth : integer read FTreeMaxWidth write FTreeMaxWidth;            // [dpv]
    property HideCheckedNodes: Boolean read FHideCheckedNodes write FHideCheckedNodes; // [dpv]
    property Filtered: Boolean read FFiltered write FFiltered;                         // [dpv]
    property SelectedNote : TKntNote read GetSelectedNote write SetSelectedNote;
    property OldSelectedIndex : integer read FOldSelectedIndex;
    property Checkboxes : boolean read FCheckboxes write FCheckboxes;
    property TreeChrome : TChrome read FTreeChrome write SetTreeChrome;
    property DefaultNoteName : string read FDefaultNoteName write FDefaultNoteName;
    property AutoNumberNodes : boolean read FAutoNumberNotes write FAutoNumberNotes;
    property VerticalLayout : boolean read FVerticalLayout write FVerticalLayout;
    property TreeHidden : boolean read FTreeHidden write FTreeHidden;

    {$IFDEF WITH_IE}
    property MainPanel : TPanel read FMainPanel write FMainPanel;
    property WebBrowser : TWebBrowser read FWebBrowser write FWebBrowser;
    // property Grid : TStringAlignGrid read FGrid write FGrid;
    // property GridSplitter : TSplitter read FGridSplitter write FGridSplitter;
    {$ENDIF}


    function SaveToFile( var tf : TTextFile;  OnlyCurrentNodeAndSubtree: TTreeNTNode= nil;
                         OnlyNotHiddenNodes: boolean= false; OnlyCheckedNodes: boolean= false): integer;
    procedure LoadFromFile( var tf : TTextFile; var FileExhausted : boolean; var NextBlock: TNextBlock; LoadOldSimpleNote: boolean= false); //override;

    function NewNote( const AParent : TKntNote; AName : string; const AInheritProperties : boolean ) : TKntNote;
    function AddNote( const aNote : TKntNote ) : integer;
    procedure InsertNote( const aIndex : integer; const aNote : TKntNote );
    procedure RemoveNote( const aNote : TKntNote );

    function StreamFormatInNote: TRichStreamFormat;
    procedure SetTreeProperties( const aProps : TFolderTreeProperties );
    procedure GetTreeProperties( var aProps : TFolderTreeProperties );

    procedure UpdateTree;

    procedure LoadFromTreePadFile( const FN : string );

    function GetNoteByID( const aID : integer ) : TKntNote;
    function GetTreeNodeByID( const aID : integer ) : TTreeNTNode;

    function InitializeTextPlainVariables( nMax: integer; RTFAux: TAuxRichEdit ): boolean;
    function InitializeTextPlain(myNote: TKntNote; RTFAux: TAuxRichEdit): boolean;// overload;

  end; // TKntFolder


  TKntFolderList = class( TList )
  private
    function GetNote( index : integer ) : TKntFolder;
    procedure PutNote( index : integer; item : TKntFolder );
  public
    property Items[index:integer] : TKntFolder read GetNote write PutNote; default;
    constructor Create;
    destructor Destroy; override;
    function Remove( item : TKntFolder ) : integer;
    procedure Delete( index : integer );
    function IndexOf( item : TKntFolder ) : integer;
  end;


implementation
uses
   gf_misc,
   gf_strings,
   gf_miscvcl,
   kn_global,
   kn_AlertMng,
   kn_LinksMng,
   kn_Main,
   kn_Macro,
   kn_EditorUtils,
   kn_Defaults,
   kn_RTFUtils,
   kn_ImagesMng,
   kn_NoteFileMng,
   kn_TreeNoteMng,
   kn_KntFolder_New,
   kn_MacroMng,
   kn_ConfigMng,
   kn_VCLControlsMng,
   kn_FileMgr,
   kn_KntFile,
   knt.App
   ;


resourcestring

{$IFDEF WITH_DART}
  STR_02 = '"%s" is a %s note and cannot be saved using %s format';
  STR_03 = 'Stream not assigned in LoadDartNotesFormat';
  STR_04 = 'LoadDartNotes: file format error or file damaged.';
{$ENDIF}
  STR_05 = 'Problem while saving folder "%s": Note count mismatch (Folder: %d  Internal: %d) ' +
      'The note may not be saved correctly. Continue?';
  STR_06 = 'Warning: "%s"';
  STR_07 = 'Node count mismatch.';
  STR_08 = 'Virtual node "%s" in folder "%s" cannot write file ';
  STR_09 = 'Folder contains %d notes, but only %d were saved.';
  STR_10 = 'Could not load Virtual Node file:';
  STR_11 = 'Failed to open TreePad file ';

  STR_21 = ' New folder.';
  STR_22 = 'Are you sure you want to delete folder "%s"?' + #13 + 'This operation cannot be undone.';
  STR_23 = 'Confirm deleting folder';
  STR_24 = ' Folder deleted.';
  STR_25 = ' Folder renamed.';


  procedure LoadStreamInRTFAux(Stream: TMemoryStream; RTFAux: TAuxRichEdit); forward;


// ---------------- TKntFolderList -----------------

constructor TKntFolderList.Create;
begin
  inherited Create;
end;

destructor TKntFolderList.Destroy;
var
  i : integer;
begin
  if ( Count > 0 ) then
     for i := 0 to pred( Count ) do begin
        if assigned( Items[i] ) then
           Items[i].Free;  // Items[i] := nil;
     end;
  Clear;
  inherited Destroy;
end;

function TKntFolderList.GetNote( index : integer ) : TKntFolder;
begin
  result := TKntFolder( inherited Items[index] );
end;

procedure TKntFolderList.PutNote( index : integer; item : TKntFolder );
begin
  inherited Put( index, item );
end;

function TKntFolderList.Remove( item : TKntFolder ) : integer;
begin
  if assigned( item ) then Item.Free;
  result := inherited remove( item );
end;

procedure TKntFolderList.Delete( index : integer );
begin
  if (( index >= 0 ) and ( index < Count ) and assigned( items[index] )) then
    Items[index].Free;
  inherited Delete( index );
end;

function TKntFolderList.IndexOf( item : TKntFolder ) : integer;
begin
  result := inherited IndexOf( item );
end;


// ---------------- TKntFolder -----------------


// -- Class methods

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
    FileWasBusy := FileIsBusy;
    FileIsBusy := true;
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
            myTreeOptions := TreeOptions;
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
          end;
        finally
          Form_NewNote.Free;
        end;
      end;

      if assigned( myFolder ) then begin
        ActiveFile.AddFolder( myFolder );
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_21;
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

        TreeNewNode( myFolder, tnTop, nil, '', true );

        App.ActivateFolder(myFolder);

      end;

    except
      on E : Exception do
      begin
        {$IFDEF KNT_DEBUG}
         Log.Add( 'Exception in NewKntFolder: ' + E.Message );
        {$ENDIF}
         PopupMessage( E.Message, mtError, [mbOK], 0 );
      end;
    end;

  finally
    Form_Main.Timer.Enabled := TimerWasEnabled;
    FileIsBusy := FileWasBusy;
    ActiveFile.Modified := true;
    UpdateKntFileState( [fscModified] );
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
        if ( DoMessageBox(
            Format( STR_22, [RemoveAccelChar( Folder.Name )] ),
            STR_23,
            MB_YESNO+MB_ICONEXCLAMATION+MB_DEFBUTTON2+MB_APPLMODAL) <> ID_YES ) then exit;
      end;

      try
        TabIdx := Folder.TabSheet.TabIndex;

        DestroyVCLControlsForFolder( Folder, true );
        ActiveFile.RemoveImagesCountReferences(Folder);
        ActiveFile.DeleteFolder( Folder );
        AddToFileManager( ActiveFile.FileName, ActiveFile ); // update manager (number of notes has changed)

      finally
          App.FolderDeleted(Folder, TabIdx);
          App.ShowInfoInStatusBar(STR_24);
      end;
  end;
end; // DeleteKntFolder


class procedure TKntFolder.CreateNewKntFolder;
begin
  if assigned(ActiveFolder) then
     ActiveFolder.EditorToDataStream;
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
          StatusBar.Panels[PANEL_HINT].Text := STR_25;
        end;

      finally
        Form_NewNote.Free;
        ActiveFile.Modified := true;
        UpdateKntFileState( [fscModified] );
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

                myInheritBGColor:= TreeOptions.InheritNodeBG;
                if TreeOptions.InheritNodeBG  and assigned(SelectedNote) then
                   myEditorChrome.BGColor := SelectedNote.RTFBGColor;

                myTreeChrome := TreeChrome;
                GetTreeProperties( myTreeProperties );
                StartWithEditorTab := ( not TV.Focused );

                oldIconKind := myTreeProperties.IconKind;
                oldShowCheckboxes := myTreeProperties.CheckBoxes;
                oldHideChecked := myTreeProperties.HideChecked;       // [dpv]
              end;
              oldPlainText := ActiveFolder.PlainText;
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

            if (PropertiesAction = propThisFolder) and not myNoteIsReadOnly  then begin
                myFile.Modified:= True;
                myFolder.Modified:= True;
                UpdateKntFileState( [fscModified] );

                myFolder.SetTabProperties( myTabProperties, not (NewPropertiesAction = propDefaults));
                myFolder.SetEditorProperties( myEditorProperties );
                myFolder.EditorChrome := myEditorChrome;

                // reflect changes in controls
                myFolder.UpdateEditor;
                myFolder.UpdateTabSheet;

                EnsuredPlainText:= False;
                if myFolder.PlainText then
                   EnsuredPlainText:= myFile.EnsurePlainTextAndRemoveImages(myFolder);

                with myFolder do begin
                  // this will apply the selected BG color to current NODE
                  // besides setting the new default BG color for whole NOTE.
                  if TreeOptions.InheritNodeBG  and assigned(SelectedNote) then begin
                    SelectedNote.RTFBGColor := myFolder.EditorChrome.BGColor;
                    myFolder.Editor.Color := myFolder.EditorChrome.BGColor;
                  end;

                  TreeLayoutChanged := ( VerticalLayout <> myTreeProperties.VerticalLayout );
                  SetTreeProperties( myTreeProperties );
                  TreeChrome := myTreeChrome;
                end;

                // update changes to tree control
                if ( oldIconKind <> myTreeProperties.IconKind ) then
                   ShowOrHideIcons( myFolder, true );
                if ( oldShowCheckboxes <> myTreeProperties.CheckBoxes ) then
                   ShowOrHideCheckBoxes( myFolder);
                if ( oldHideChecked <> myTreeProperties.HideChecked ) then    // [dpv]
                   if myTreeProperties.HideChecked then
                      HideChildNodesUponCheckState ( myFolder, nil, csChecked)
                   else
                      ShowCheckedNodes ( myFolder, nil);

                UpdateTreeChrome(myFolder);

                // If we have changed to PlainText we will have already updated the editor from EnsurePlainTextAndRemoveImages
                if (not EnsuredPlainText) and (oldPlainText <> myFolder.PlainText)
                     and (not TreeLayoutChanged)   then begin  // This is done if TreeLayoutChanged too
                   myFolder.EditorToDataStream;  // Save the content of the editor according to the new formatting (Plain text / RTF)
                   myFolder.DataStreamToEditor;
                end;

            end;

            if ApplyTreeChromeToAllFolders and HaveKntFolders( false, true ) then begin
                for i := 0 to myFile.NoteCount -1 do begin
                   F:= myFile.Folders[i];
                   if ((PropertiesAction = propThisFolder) and (F = myFolder)) or (F.ReadOnly) then
                       continue;
                   F.Modified:= True;
                   F.TreeChrome := myTreeChrome;
                   UpdateTreeChrome(F);
                end;
                myFile.Modified:= True;
                UpdateKntFileState( [fscModified] );
            end;


            if (PropertiesAction = propDefaults) or (NewPropertiesAction = propDefaults) then begin

                // must update all richedits and trees with the modified EditorOptions and TreeOptions:
                if HaveKntFolders( false, true ) then begin
                    for i := 0 to myFile.NoteCount -1 do begin
                       F:= myFile.Folders[i];
                       F.Editor.WordSelection := EditorOptions.WordSelect;
                       F.Editor.UndoLimit := EditorOptions.UndoLimit;
                       UpdateTreeOptions(F);
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

          if _LastZoomValue <> 100 then
             myFolder.Editor.SetZoom(_LastZoomValue, '' );

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
          myFolder.EditorToDataStream;
          myFolder.Editor.Clear;
          myFolder.Editor.ClearUndo;
          DestroyVCLControlsForFolder( myFolder, false );
          CreateVCLControlsForFolder( myFolder );
          myFolder.DataStreamToEditor;
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


constructor TKntFolder.Create (KntFile: TObject);
begin
  inherited Create;

  FKntFile:= KntFile;

  FOnChange := nil;
  FTabSheet := nil;
  FTabIndexAux := 0;
  FName := DEFAULT_NEW_FOLDER_NAME;
  FID := 0;
  FVisible := true;
  FReadOnly := false;
  FPlainText := false;
  FInfo := 0;
  FTag := 0;
  FImageIndex := -1;
  FDateCreated := now;
  FModified := false;
  InitializeChrome( FEditorChrome );

  FEditor := nil;
  FWordWrap := false;
  FURLDetect := true;
  FTabSize := DEF_TAB_SIZE;
  FUseTabChar := false;
  FIsInsertMode := true;
  with FCaretPos do
  begin
    x := 0;
    y := 0;
  end;
  FAuxiliarAlarmList:= TList.Create;

  FHistory := TKNTHistory.Create;


  FSplitter := nil;
  FTV := nil;
  FTreeWidth := 0; // flag, so that default width will be used
  FTreeMaxWidth:= 0;
  FIconKind := niStandard;
  FCheckboxes := false;
  FSelectedNote := nil;
  FTreeHidden := false;
  FHideCheckedNodes:= false;  // [dpv]
  FFocusMemory := focTree; // initially focus tree
  FOldSelectedIndex := -1;
  FAutoNumberNotes := false;
  FVerticalLayout := false;
  InitializeChrome( FTreeChrome );
  FDefaultNoteName := DEFAULT_NEW_NOTE_NAME;
  FNotes := TKntNoteList.Create;

  {$IFDEF WITH_IE}
  FMainPanel := nil;
  FWebBrowser := nil;
  // FGrid := nil;
  // FGridSplitter := nil;
  {$ENDIF}
end; // CREATE

destructor TKntFolder.Destroy;
begin
  FOnChange := nil;
  AlarmMng.RemoveAlarmsOfNote(Self);
  if assigned (FAuxiliarAlarmList) then FAuxiliarAlarmList.Free;

  try
    if assigned( FNotes ) then FNotes.Free;
    FHistory.Free;
  except
  end;

  inherited Destroy;
end; // TTabNote .Destroy;


procedure TKntFolder.BeforeEditorLoaded(Note: TKntNote);
begin
   if assigned(FOnBeforeEditorLoaded) then OnBeforeEditorLoaded(Note);
end;

procedure TKntFolder.AfterEditorLoaded(Note: TKntNote);
begin
   if assigned(FOnAfterEditorLoaded) then OnAfterEditorLoaded(Note);
end;


function TKntFolder.PropertiesToFlagsString : TFlagsString;
begin
  result := DEFAULT_FLAGS_STRING;
  result[1] := BOOLEANSTR[FVisible];
  result[2] := BOOLEANSTR[FReadOnly];
  result[3] := BOOLEANSTR[FWordWrap];
  result[4] := BOOLEANSTR[FURLDetect];
  result[5] := BOOLEANSTR[FUseTabChar];
  result[6] := BOOLEANSTR[FPlainText];


  // values 1-12 are reserved for TTabNote
  result[TREENOTE_FLAG_BASE+1] := AnsiChar(inttostr( ord( FIconKind ))[1]);
  result[TREENOTE_FLAG_BASE+2] := BOOLEANSTR[FAutoNumberNotes];
  result[TREENOTE_FLAG_BASE+3] := BOOLEANSTR[FCheckboxes];
  result[TREENOTE_FLAG_BASE+4] := BOOLEANSTR[FVerticalLayout];

  // added in 1.5.9:
  result[TREENOTE_FLAG_BASE+5] := BOOLEANSTR[FTreeHidden];
  result[TREENOTE_FLAG_BASE+6] := BOOLEANSTR[(( not FTreeHidden ) and ( FFocusMemory = focTree ))];

  // added in 1.7.0:          // [dpv]
  result[TREENOTE_FLAG_BASE+7] := BOOLEANSTR[FHideCheckedNodes];

end; // PropertiesToFlagsString

procedure TKntFolder.FlagsStringToProperties( const FlagsStr : TFlagsString );
begin
  if ( length( FlagsStr ) < FLAGS_STRING_LENGTH ) then exit;
  FVisible    := FlagsStr[1] = BOOLEANSTR[true];
  FReadOnly   := FlagsStr[2] = BOOLEANSTR[true];
  FWordWrap   := FlagsStr[3] = BOOLEANSTR[true];
  FURLDetect  := FlagsStr[4] = BOOLEANSTR[true];
  FUseTabChar := FlagsStr[5] = BOOLEANSTR[true];
  FPlainText  := FlagsStr[6] = BOOLEANSTR[true];

  case FlagsStr[TREENOTE_FLAG_BASE+1] of
    '0' : FIconKind := niNone;
    '1' : FIconKind := niStandard;
    '2' : FIconKind := niCustom;
  end;
  FAutoNumberNotes := FlagsStr[TREENOTE_FLAG_BASE+2] = BOOLEANSTR[true];
  FCheckboxes      := FlagsStr[TREENOTE_FLAG_BASE+3] = BOOLEANSTR[true];
  FVerticalLayout  := FlagsStr[TREENOTE_FLAG_BASE+4] = BOOLEANSTR[true];

  FTreeHidden := FlagsStr[TREENOTE_FLAG_BASE+5] = BOOLEANSTR[true];
  if (( not FTreeHidden ) and ( FlagsStr[TREENOTE_FLAG_BASE+6] = BOOLEANSTR[true] )) then
    FFocusMemory := focTree
  else
    FFocusMemory := focRTF;

  FHideCheckedNodes      := FlagsStr[TREENOTE_FLAG_BASE+7] = BOOLEANSTR[true];    // [dpv]
end; // FlagsStringToProperties


procedure TKntFolder.UpdateTabSheet;
begin
  // CheckTabSheet;
  with FTabSheet do
  begin
    Caption := FName;
    ImageIndex := FImageIndex;
  end;
end; // UpdateTabSheetProperties


procedure TKntFolder.UpdateEditor;
var
//  tabstopcnt : integer;
  TextLen: integer;
  SS, SL: integer;

begin
  if ( not CheckEditor ) then exit;

  FEditor.BeginUpdate;
  try
       FEditor.WordWrap := FWordWrap;
       FEditor.TabSize := FTabSize;
       FEditor.AutoURLDetect := FURLDetect;

       FEditor.Color := FEditorChrome.BGColor;
       TextLen:= FEditor.TextLength;
       if (TextLen = 0) or PlainText then begin          // Solves the problem indicated in EditProperties...*1
          with FEditor.DefAttributes do begin
            Charset := FEditorChrome.Font.Charset;
            Name := FEditorChrome.Font.Name;
            Size := FEditorChrome.Font.Size;
            Style := FEditorChrome.Font.Style;
            Color := FEditorChrome.Font.Color;
            Language := FEditorChrome.Language;
          end;
       end;

       if PlainText and (TextLen > 0) then begin       // Related to *1. If PlainText then we do want it to always change the font format
          SS:= FEditor.SelStart;
          SL:= FEditor.SelLength;
          FEditor.SelectAll;
          FEditor.SelAttributes.Assign( FEditor.DefAttributes );
         {
         FEditor.Paragraph.TabCount := 8; // max is 32, but what the hell
         for tabstopcnt := 0 to 7 do
           FEditor.Paragraph.Tab[tabstopcnt] := (tabstopcnt+1) * (2*FTabSize); // [x] very rough!
         }
         FEditor.SetSelection(SS, SS+SL, True);
       end;
       FEditor.UseTabChar := FUseTabChar;
       FEditor.ReadOnly := FReadOnly;


       if assigned(FSelectedNote) then begin
          case FSelectedNote.WordWrap of
             wwAsFolder : FEditor.WordWrap := FWordWrap;
             wwYes : FEditor.WordWrap := true;
             wwNo : FEditor.WordWrap := false;
          end;

          FEditor.Color := FSelectedNote.RTFBGColor;
       end;

  finally
     FEditor.EndUpdate;
  end;
end; // UpdateEditor


procedure TKntFolder.SetEditorChrome( AChrome : TChrome );
begin
  FEditorChrome := AChrome;
  FModified := true;
end; // SetEditorChrome

procedure TKntFolder.SetEditorProperties( const aProps : TFolderEditorProperties );
begin
  FPlainText := aProps.PlainText;
  FTabSize := aProps.TabSize;
  FURLDetect := aProps.URLDetect;
  FUseTabChar := aProps.UseTabChar;
  FWordWrap := aProps.WordWrap;
  FModified := true;
end; // SetEditorProperties

procedure TKntFolder.GetEditorProperties( var aProps : TFolderEditorProperties );
begin
  aProps.PlainText := FPlainText;
  aProps.TabSize := FTabSize;
  aProps.URLDetect := FURLDetect;
  aProps.UseTabChar := FUseTabChar;
  aProps.WordWrap := FWordWrap;
end; // GetEditorProperties

procedure TKntFolder.SetTabProperties( const aProps : TFolderTabProperties; UpdateName: boolean= True);
begin
  if UpdateName then
     FName := aProps.Name;
  FImageIndex := aProps.ImageIndex;
  FModified := true;
end;

procedure TKntFolder.GetTabProperties( var aProps : TFolderTabProperties );
begin
  aProps.ImageIndex := FImageIndex;
  aProps.Name := FName;
end;

procedure TKntFolder.SetTabSheet( ATabSheet : TTab95Sheet );
begin
  if ( ATabSheet <> FTabSheet ) then
    FTabSheet := ATabSheet;
end;

procedure TKntFolder.SetEditor( AEditor : TKntRichEdit );
begin
  if ( AEditor <> FEditor ) then
    FEditor := AEditor;
end;

procedure TKntFolder.SetName( AName : TNoteNameStr );
begin
  AName := trim( AName );
  if (( FName = AName ) or ( AName = '' )) then exit;
  FName := copy( AName, 1, TABNOTE_NAME_LENGTH );
  FModified := true;
  if _ALLOW_VCL_UPDATES and assigned( FTabSheet ) then FTabSheet.Caption := FName;
end; // SetName

procedure TKntFolder.SetID( ID : Cardinal );
begin
  if ( FID = 0 ) then
    FID := ID;
  // otherwise, never allow the ID to be changed
end; // SetID


// myTreeNode must be active node in folder, so that the Editor is showing its content
function TKntFolder.PrepareTextPlain(myTreeNode: TTreeNTNode; RTFAux: TAuxRichEdit): string;
var
    myNote : TKntNote;
begin
   if FEditor.Modified then
      EditorToDataStream;

   myNote := TKntNote( myTreeNode.Data );
   Self.InitializeTextPlain(myNote, RTFAux);
   Result:= myNote.NoteTextPlain;
end;


function TKntFolder.GetTextPlainFromNode(myTreeNode: TTreeNTNode; RTFAux: TAuxRichEdit): string;
var
    myNote : TKntNote;
begin
   if TV.Selected = myTreeNode then
      Result:= Editor.TextPlain

   else begin
      myNote := TKntNote( myTreeNode.Data );
      LoadStreamInRTFAux (myNote.Stream, RTFAux);
      Result:= RTFAux.TextPlain;
   end;
end;



function TKntFolder.CheckSavingImagesOnMode (ImagesMode: TImagesMode; Stream: TMemoryStream;
                                           ExitIfAllImagesInSameModeDest: boolean = true): TImageIDs;
var
  strRTF: AnsiString;
  ContainsImgIDsRemoved: boolean;
  ContainsImages: boolean;

begin
    Result:= nil;

    strRTF:= ImageMng.ProcessImagesInRTF(Stream.Memory, Stream.Size, Self.Name, ImagesMode, '', 0, ContainsImgIDsRemoved, ContainsImages, ExitIfAllImagesInSameModeDest);
    if strRTF <> '' then begin                                // Changes in images must be reflected (=> ContainsImages=true)
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


procedure TKntFolder.GetImagesIDInstances (Stream: TMemoryStream; TextPlain: String);
begin
   if (TextPlain <> '') then
      fImagesReferenceCount:= ImageMng.GetImagesIDInstancesFromTextPlain (TextPlain)
   else
      fImagesReferenceCount:= ImageMng.GetImagesIDInstancesFromRTF (Stream);
end;

procedure TKntFolder.ResetImagesReferenceCount;
begin
    SetLength(fImagesReferenceCount, 0);
end;


procedure TKntFolder.ReloadImagesOnEditor;
var
   ImgeIDs: TImageIDs;
begin
   ImgeIDs:= ImageMng.GetImagesIDInstancesFromTextPlain (Editor.TextPlain);
   ImageMng.ReloadImages(ImgeIDs);

   EditorToDataStream;
   DataStreamToEditor;
end;

procedure TKntFolder.ReconsiderImageDimensionGoalsOnEditor(Selection: boolean);
var
  strRTF: AnsiString;
  SelectAll: boolean;
begin
   ImageMng.ReconsiderImageDimensionsGoal:= true;
   if ReadOnly then
      Selection:= False;      // If true -> The note would have to be modified, and since it is not possible, the images would disappear...

   try
      if Selection then begin
         SelectAll:= false;
         Editor.CheckToSelectLeftImageHiddenMark;
         strRTF:= Editor.RtfSelText;

         if strRTF = '' then begin
            strRTF:= Editor.RtfText;
            SelectAll:= true;
         end;

         if strRTF <> '' then begin
            strRTF:= ImageMng.ProcessImagesInRTF(strRTF, Self.Name, ImagesMode, '', 0, false);
            if strRTF <> '' then
               Editor.PutRtfText(strRTF, True, not SelectAll);
         end;
      end
      else begin
         EditorToDataStream;
         DataStreamToEditor;
      end;

      if _LastZoomValue <> 100 then
         FEditor.SetZoom(_LastZoomValue, '' );

   finally
      ImageMng.ReconsiderImageDimensionsGoal:= false;
   end;
end;



function TKntFolder.CheckEditor : boolean;
begin
  result := assigned( FEditor );
end; // CheckEditor

function TKntFolder.CheckTabSheet : boolean;
begin
  result := assigned( FTabSheet );
end; // CheckTabSheet

procedure TKntFolder.BaseSaveProc( var tf : TTextFile );
var
  HaveVCLControls : boolean;
begin
  HaveVCLControls := ( CheckTabSheet and CheckEditor );

  if HaveVCLControls then
  begin
    FTabIndexAux := FTabSheet.PageIndex; // remember tab position
    with FCaretPos do
    begin
      x := FEditor.SelStart;
      y := FEditor.SelLength;
    end;
  end;

  tf.WriteLine( _FolderName + '=' + FName,  True);
  tf.WriteLine( _FolderID + '=' + FID.ToString  );
  tf.WriteLine( _ImageIndex + '=' + FImageIndex.ToString  );
  tf.WriteLine( _DateCreated + '=' + FormatDateTime( _SHORTDATEFMT + #32 + _LONGTIMEFMT, FDateCreated ) );
  tf.WriteLine( _TabIndex + '=' + FTabIndexAux.ToString  );
  tf.WriteLine( _TabSize + '=' + inttostr( FTabSize )  );

  tf.WriteLine( _PosX + '=' + FCaretPos.X.ToString  );
  tf.WriteLine( _PosY + '=' + FCaretPos.Y.ToString  );
  tf.WriteLine( _CHBGColor + '=' + ColorToString( FEditorChrome.BGColor )  );
  tf.WriteLine( _CHFontCharset + '=' + inttostr( FEditorChrome.Font.Charset )  );
  tf.WriteLine( _CHFontColor + '=' + ColorToString( FEditorChrome.Font.Color )  );
  tf.WriteLine( _CHFontName + '=' + FEditorChrome.Font.Name  );
  tf.WriteLine( _CHFontSize + '=' + FEditorChrome.Font.Size.ToString  );
  tf.WriteLine( _CHLanguage + '=' + IntToStr(FEditorChrome.Language) );
  tf.WriteLine( _CHFontStyle + '=' + FontStyleToStr( FEditorChrome.Font.Style )  );
  tf.WriteLine( _Flags + '=' + PropertiesToFlagsString  );

  { // REMOVED: FlagsString is used instead
  writeln( tf, _UseTabChar, '=', BOOLEANSTR[FUseTabChar] );
  writeln( tf, _ReadOnly, '=', BOOLEANSTR[FReadOnly] );
  writeln( tf, _WordWrap, '=', BOOLEANSTR[FWordWrap] );
  writeln( tf, _URLDetect, '=', BOOLEANSTR[FURLDetect] );
  }
  if HasAlarms(true) then
     SaveAlarms(tf);

end; // BaseSaveProc

(*
Format of the serialized alarm:  NA=[D]Reminder[/Expiration][*Format][|subject]
Ej: NA=D10-06-2010 08:00:00/10-06-2010 07:55:00*B100/1200|Comment to the alarm

[] => optional
D: Discarded
Expiration or Reminder: DD-MM-YYYY HH:MM:SS
Format: BoldFormatFontColor/BackColor
BoldFormat: B or N   (Bold or Normal)
FontColor - BackColor: number (TColor)
subject: unicode text

*)
procedure TKntFolder.SaveAlarms(var tf : TTextFile; note: TKntNote = nil);
var
   I: integer;
   Alarms: TList;
   s: string;
   alarm: TAlarm;
   BoldStr: char;
begin
  try
     if assigned(note) then
        Alarms:= note.getAlarms(true)
     else
        Alarms:= Self.getAlarms(true);

     I:= 0;
     while I <= Alarms.Count - 1 do begin
        alarm:= TAlarm(Alarms[i]);
        s:= '';
        if alarm.ExpirationDate <> 0 then
           s:= '/' + FormatDateTime( _SHORTDATEFMT + #32 + _LONGTIMEFMT, alarm.ExpirationDate );

        if alarm.Bold or (alarm.FontColor <> clWindowText) or (alarm.BackColor <> clWindow) then begin
           if alarm.Bold then BoldStr:= 'B' else BoldStr:= 'N';
           s:= s + '*' + BoldStr + IntToStr(alarm.FontColor) + '/' + IntToStr(alarm.BackColor);
           end;

        if alarm.AlarmNote <> '' then
           s:= s + '|' + StringReplace(alarm.AlarmNote, #13#10, 'ªª', [rfReplaceAll]);
        s:= FormatDateTime( _SHORTDATEFMT + #32 + _LONGTIMEFMT, alarm.AlarmReminder ) + s;
        if alarm.Status = TAlarmDiscarded then
           s:= 'D' + s;
        tf.WriteLine( _NodeAlarm + '=' + s, True );

        I:= I + 1;
     end;

  except
  end;
end;


procedure TKntFolder.SaveRTFToFile(var tf : TTextFile; DataStream : TMemoryStream; PlainText: Boolean; PlaintextLeader: AnsiString = _NF_PLAINTEXTLEADER);
var
  List : TStringList;
  cnt: integer;
  i, pos : integer;
  addCRLF: boolean;
  StreamAux : TMemoryStream;
  ImagesIDs: TImageIDs;

begin
    if DataStream.Size = 0 then
       exit;

  StreamAux:= nil;
  try

    if (not PlainText) and ImageMng.ExportingMode then begin
       var RTFwithProccesedImages: AnsiString;
       var ContainsImgIDsRemoved: boolean;
       var ContainsImages: boolean;
       var ImgMode: TImagesMode;


       if KeyOptions.ImgStorageModeOnExport = smeEmbRTF then
          ImgMode:= imImage
       else
          ImgMode:= imLink;

       RTFwithProccesedImages:= ImageMng.ProcessImagesInRTF(DataStream.Memory, DataStream.Size, '', ImgMode, '', 0, ContainsImgIDsRemoved, ContainsImages, true);
       if (RTFwithProccesedImages = '') and ContainsImages and (KeyOptions.ImgStorageModeOnExport <> smeEmbKNT) then
          RTFwithProccesedImages:= MemoryStreamToString (DataStream);

       if (RTFwithProccesedImages <> '') then begin
          if (KeyOptions.ImgStorageModeOnExport <> smeEmbKNT) then
             RTFwithProccesedImages:= RemoveKNTHiddenCharactersInRTF(RTFwithProccesedImages, hmOnlyImages);

          StreamAux := TMemoryStream.Create;
          StreamAux.Write(RTFwithProccesedImages[1], ByteLength(RTFwithProccesedImages));

          DataStream:= StreamAux;
       end;

       if ContainsImages and (KeyOptions.ImgStorageModeOnExport = smeEmbKNT) then begin
          ImagesIDs:= ImageMng.GetImagesIDInstancesFromRTF (DataStream);
          ImageMng.RegisterImagesReferencesExported (ImagesIDs);
       end;
    end;


    DataStream.Position := 0;

    tf.WriteLine( _NF_RTF );

    if PlainText then begin
        // Looking for: ;[<BOM>]first line...
        //               ;second line...

        i:= 0;
        addCRLF:= false;
        repeat
           tf.write( PlaintextLeader );
           pos:= PosEx(AnsiString(#13#10), PAnsiChar(DataStream.Memory), i+1);       // The index is 1-based.
           if (pos=0) or (pos > DataStream.Size) then begin
               pos:= DataStream.Size-1;
               if (i = pos) and (PByte(DataStream.Memory)[i] = 0) then break;
               addCRLF:= true;
           end;
           tf.write(PByte(DataStream.Memory)[i], pos-i+1);
           i:= pos + 1;
        until i >= DataStream.Size;

        if addCRLF then
           tf.write(#13#10);

    end
    else begin
       DataStream.Position := 0;
       i:= 0;
       // When compiled in D2006 with RxRichEdit 2.75 not ocurred, but now, when saving the stream in RTF an extra #0 is added. But I checked it
       // The existence of these final #0s really suits me well. See comment to TTabNote.CheckSavingImagesOnMode
       if PByte(DataStream.Memory)[DataStream.Size-1] = 0 then i:= 1;
       tf.F.CopyFrom(DataStream, DataStream.Size - i);
    end;

  finally
     if StreamAux <> nil then
        StreamAux.Free;
  end;
end;


{$IFDEF WITH_DART}
procedure TTabNote.SaveDartNotesFormat( Stream : TStream );
var
  ds, ds1 : string;
  dsi : integer;
  HaveVCLControls : boolean;
begin

  // Only simple RTF notes are compatible with DartNotes format.
  // If FKind is not ntRTF, bail out
  if ( FKind <> ntRTF ) then
    raise ETabNoteError.CreateFmt( STR_02, [FName, TABNOTE_KIND_NAMES[FKind], FILE_FORMAT_NAMES[nffDartNotes]] );

  HaveVCLControls := ( CheckTabSheet and CheckEditor );
  if HaveVCLControls then
  begin
    with FDataStream do
    begin
      Clear;
      Position := 0;
    end;
    FTabIndex := FTabSheet.PageIndex; // remember tab position
    FEditor.Lines.SaveToStream( FDataStream );
  end;

  FDataStream.Position := 0;

  try
    ds := FName + _DART_STOP +
          inttostr( integer( FEditorChrome.BgColor )) + _DART_STOP +
          BOOLEANSTR[FWordWrap] + _DART_STOP;
    dsi := length( ds );
    ds := ds + inttostr( FDataStream.Size ) + _DART_STOP;
    ds1 := inttostr( length( ds ) + FDataStream.Size );
    ds := _DART_STOP + inttostr( dsi ) + _DART_STOP + ds;
    Stream.WriteBuffer( ds1[1], length( ds1 ));
    Stream.WriteBuffer( ds[1], length( ds ));

    Stream.CopyFrom( FDataStream, 0 );

  finally
    if HaveVCLControls then
      FDataStream.Clear;
  end;

  Modified := false; // triggers SetModified

end; // SaveDartNotesFormat
{$ENDIF}


function TKntFolder.HasAlarms(considerDiscarded: boolean): boolean;
begin
    Result:= AlarmMng.HasAlarms(Self, nil, considerDiscarded);
end;

function TKntFolder.GetAlarms(considerDiscarded: boolean): TList;
begin
   Result:= AlarmMng.GetAlarms(Self, nil, considerDiscarded);
end;


(*
Format of the serialized alarm:  NA=[D]Reminder[/Expiration][*Format][|subject]
Ej: NA=D10-06-2010 08:00:00/10-06-2010 07:55:00*B100/1200|Comment to the alarm

[] => optional
D: Discarded
Expiration or Reminder: DD-MM-YYYY HH:MM:SS
Format: BoldFormatFontColor/BackColor
BoldFormat: B or N   (Bold or Normal)
FontColor - BackColor: number (TColor)
subject: unicode text

*)
procedure TKntFolder.ProcessAlarm (s: AnsiString; note: TKntNote = nil);
var
    alarm: TAlarm;
    p, p2: integer;
    format: AnsiString;
begin
   try

      alarm:= TAlarm.Create;

      p := Pos( '|', s );
      if ( p > 0 ) then begin
          alarm.AlarmNote:= StringReplace(TryUTF8ToUnicodeString(copy(s, p+1, length(s))), 'ªª', #13#10, [rfReplaceAll]);
          delete( s, p, length(s));
      end;

      p := Pos( '*', s );
      if ( p > 0 ) then begin
          format:= copy(s, p+1, length(s));
          if format[1] = 'B' then
             alarm.Bold:= true;
          p2 := Pos( '/', format );
          alarm.FontColor := StrToInt(copy(format, 2, p2-2));
          alarm.BackColor := StrToInt(copy(format, p2+1, length(format)));
          delete( s, p, length(s));
      end;

      p := Pos( '/', s );
      if ( p > 0 ) then begin
          alarm.ExpirationDate:= strtodatetime(copy(s, p+1, length(s)));
          delete( s, p, length(s));
      end;
      if s[1] = 'D' then begin
         alarm.Status := TAlarmDiscarded;
         s:= Copy(s,2,MaxInt)
      end;
      alarm.AlarmReminder:= strtodatetime(s);
      if p <= 0  then
         alarm.ExpirationDate:= 0;

      alarm.Note:= note;
      alarm.Folder:= Self;

      FAuxiliarAlarmList.Add(alarm);

   except
   end;
end;

procedure TKntFolder.AddProcessedAlarms ();
var
  I: Integer;
  alarm: TAlarm;
begin
   if not assigned(FAuxiliarAlarmList) then exit;
   I:= 0;
   while I <= FAuxiliarAlarmList.Count - 1 do begin
      alarm:= TAlarm(FAuxiliarAlarmList[i]);
      AlarmMng.AddAlarm(alarm);
      I:= I + 1;
   end;

   FAuxiliarAlarmList.Clear;
end;

procedure TKntFolder.AddProcessedAlarmsOfFolder (newFolder: TKntFolder);
var
  I: Integer;
  alarm: TAlarm;
begin
   if not assigned(FAuxiliarAlarmList) then exit;

   I:= 0;
   while I <= FAuxiliarAlarmList.Count - 1 do begin
      alarm:= TAlarm(FAuxiliarAlarmList[i]);
      if (alarm.Folder = Self) and (alarm.Note= nil) then begin
         alarm.Folder := newFolder;
         AlarmMng.AddAlarm(alarm);
      end;
      I:= I + 1;
   end;
end;

procedure TKntFolder.AddProcessedAlarmsOfNote (note: TKntNote; newFolder: TKntFolder; newNote: TKntNote);
var
  I: Integer;
  alarm: TAlarm;
begin
   if not assigned(FAuxiliarAlarmList) then exit;

   I:= 0;
   while I <= FAuxiliarAlarmList.Count - 1 do begin
      alarm:= TAlarm(FAuxiliarAlarmList[i]);
      if (alarm.Folder = Self) and (alarm.Note = note) then begin
         alarm.Folder := newFolder;
         alarm.Note := newNote;
         AlarmMng.AddAlarm(alarm);
      end;
      I:= I + 1;
   end;
end;

procedure TransferRTFData(ListRTFStr : TStringList; StreamRTF: TMemoryStream);
var
   NewRTF: string;
begin
    if (ListRTFStr.Count = 0) then exit;

    if App.opt_Clean then begin
       if CleanRTF(ListRTFStr.Text, NewRTF) then begin
          //ListRTFStr.SaveToStream(StreamRTF);
          StreamRTF.LoadFromStream(TStringStream.Create(NewRTF));
          exit;
       end;
    end;

    ListRTFStr.WriteBOM:= False;
    ListRTFStr.SaveToStream(StreamRTF);
    if NodeStreamIsRTF(StreamRTF) then begin
      // In notes/nodes with RTF content we are interested in the buffer ending in #0 to be able to treat it as a string (accessing .Memory)
      assert((PByte(StreamRTF.Memory)[StreamRTF.Size-1] <> 0), 'The Stream already ends at 0');
      StreamRTF.WriteData(0);
    end;
end;


{$IFDEF WITH_DART}
procedure TTabNote.LoadDartNotesFormat( Stream : TStream );
var
  TextSize : longint;
  ds : string;
  ch : char;
  p, blocklen, rtfoffset : integer;
begin
  if ( not assigned( Stream )) then
    raise ETabNoteError.Create( STR_03 );

  with FDataStream do
  begin
    Clear;
    Position := 0;
  end;

  try
    ds := '';
    repeat
      Stream.ReadBuffer( ch, sizeof( ch ));
      if ( ch = _DART_STOP ) then break;
      ds := ds + ch;
    until ( Stream.Position = Stream.Size ); // safety check

    try
      blocklen := strtoint( ds );
      ds := '';
      repeat
        Stream.ReadBuffer( ch, sizeof( ch ));
        if ( ch = _DART_STOP ) then break;
        ds := ds + ch;
      until ( Stream.Position = Stream.Size ); // safety check
      rtfoffset := strtoint( ds );
      ds := '';
      SetLength( ds, rtfoffset );
      Stream.ReadBuffer( ds[1], rtfoffset );
      p := pos( _DART_STOP, ds );
      FName := copy( ds, 1, pred( p ));
      delete( ds, 1, p );
      p := pos( _DART_STOP, ds );
      FEditorChrome.BGColor := strtoint( copy( ds, 1, pred( p )));
      delete( ds, 1, p );
      if ( ds <> '' ) then
        FWordWrap := ( ds[1] = '1' );
      ds := '';
      repeat
        Stream.ReadBuffer( ch, sizeof( ch ));
        if ( ch = _DART_STOP ) then break;
        ds := ds + ch;
      until ( Stream.Position = Stream.Size ); // safety check
      TextSize := strtoint( ds );
      FDataStream.CopyFrom( Stream, TextSize );
    except
      on E : EConvertError do
      begin
        raise Exception.Create( STR_04 );
      end;
      on E : Exception do raise;
    end;

  finally
  end;

  FModified := false;

end; // LoadDartNotesFormat
{$ENDIF}


procedure TKntFolder.SetReadOnly( AReadOnly : boolean );
begin
  if ( AReadOnly <> FReadOnly ) then
  begin
    FReadOnly := AReadOnly;
    FModified := true;
    if _ALLOW_VCL_UPDATES and assigned( FEditor ) then FEditor.ReadOnly := FReadOnly;

    ConfigureEditor;
    App.FolderPropertiesModified(Self);
    App.EditorPropertiesModified(FEditor);
  end;
end; // TTabNote.SetReadOnly


procedure TKntFolder.SetPlainText( APlainText : boolean );
begin
  if ( APlainText <> FPlainText ) then begin
    FPlainText := APlainText;
    FModified := true;

    ConfigureEditor;
    App.FolderPropertiesModified(Self);
    App.EditorPropertiesModified(FEditor);
  end;
end;


function TKntFolder.GetModified : boolean;
begin
  result := FModified;
  {
  if assigned( FEditor ) then
    result := result or FEditor.Modified;
  }
end; // GetModified

procedure TKntFolder.SetModified( AModified : boolean );
var
  i : Cardinal;
begin
  FModified := AModified;
  if assigned( FEditor ) then
    FEditor.Modified := FModified;

  if not AModified and (FNotes.Count > 0) then begin
     for i := 0 to FNotes.Count-1 do
        FNotes[i].RTFModified:= False;
  end;

end; // SetModified


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
end; // TTabNote.SetTabIndex

procedure TKntFolder.SetImageIndex( AImgIdx : integer );
begin
  if ( FImageIndex = AImgIdx ) then exit;
  FImageIndex := AImgIdx;
  if _ALLOW_VCL_UPDATES and assigned( FTabSheet ) then
    FTabSheet.ImageIndex := FImageIndex;
end; // TTabNote.SetImgIdx


procedure TKntFolder.SetImagesMode(ImagesMode: TImagesMode; ForceMode: boolean);
var
   RTFIn, RTFOut: AnsiString;
   currentNoteModified, currentFileModified: boolean;
   SS: integer;
   myTreeNode: TTreeNTNode;
   RestoreRO: boolean;

begin
    if ForceMode or (FImagesMode <> ImagesMode) then begin

       myTreeNode:= nil;
       myTreeNode := Self.TV.Selected;
       SS:= Editor.SelStart;
       if ImagesMode = imLink then                                       // imImage --> imLink
          SS:= PositionInImLinkTextPlain (Self, myTreeNode, SS, True);   // True: Force calculation

       FImagesMode:= ImagesMode;

       RTFIn:= Editor.RtfText;
       RTFOut:= ImageMng.ProcessImagesInRTF(RTFIn, Self.Name, ImagesMode, '', 0, true);
       if RTFOut <> '' then begin
          Editor.BeginUpdate;
          try
             currentNoteModified:= FModified;
             currentFileModified:= TKntFile(KntFile).Modified;

             RestoreRO:= ReadOnly;
             try
                if ReadOnly then
                   Editor.ReadOnly:= False;                     // We must allow images to be shown or hidden even if the note is read only
                Editor.PutRtfText(RTFout,True,False);
                if not ReadOnly and (currentNoteModified <> FModified) then begin
                   FModified:=  currentNoteModified;  // <- false...
                   TKntFile(KntFile).Modified:= currentFileModified;
                   UpdateKntFileState( [fscModified] );
                end;
             finally
                if RestoreRO then begin
                   Editor.ReadOnly:= True;
                   Editor.Modified:= False;
                end;
             end;
             SearchCaretPos(Self.Editor, myTreeNode, SS, 0, true, Point(-1,-1));
          finally
            Editor.EndUpdate;
          end;
       end;
    end;
end;


procedure TKntFolder.SetImagesMode(ImagesMode: TImagesMode);
begin
   SetImagesMode(ImagesMode, false);
end;


procedure TKntFolder.SetWordWrap( AWordWrap : boolean );
begin
  if ( FWordWrap = AWordWrap ) then exit;
  FWordWrap := AWordWrap;
  FModified := true;
  if _ALLOW_VCL_UPDATES and assigned( FEditor ) then
    FEditor.WordWrap := FWordWrap;
end;

procedure TKntFolder.SetURLDetect( AURLDetect : boolean );
begin
  if ( FURLDetect = AURLDetect ) then exit;
  FURLDetect := AURLDetect;
  FModified := true;
  if _ALLOW_VCL_UPDATES and assigned( FEditor ) then
    FEditor.AutoURLDetect := FURLDetect;
end;

procedure TKntFolder.SetTabSize( ATabSize : byte );
begin
  if ( FTabSize = ATabSize ) then exit;
  FTabSize := ATabSize;
  FModified := true;
  if _ALLOW_VCL_UPDATES and assigned( FEditor ) then
    FEditor.TabSize := FTabSize;
end;


function TKntFolder.GetCount : integer;
begin
  if assigned( FNotes ) then
    result := FNotes.Count
  else
    result := 0;
end; // GetNodeCount


function TKntFolder.GetSelectedNote : TKntNote;
begin
   result := FSelectedNote;
end;

procedure TKntFolder.SetSelectedNote( aNote : TKntNote );
begin
  FSelectedNote := aNote;
end;


function TKntFolder.NewNote(
  const AParent : TKntNote;
  AName : string;
  const AInheritProperties : boolean ) : TKntNote;
var
  myNote : TKntNote;
begin
  myNote := TKntNote.Create;
  myNote.Name := AName;
  myNote.RTFBGColor := FEditorChrome.BGColor;

  if assigned( APArent ) then
  begin
    myNote.Level := succ( AParent.Level ); // else Node is already initialized to Level 0
    if AInheritProperties then
    begin
      myNote.Checked := AParent.Checked;
      myNote.Bold := AParent.Bold;
      myNote.HasNodeColor := AParent.HasNodeColor;
      myNote.HasNodeBGColor := APArent.HasNodeBGColor;
      myNote.NodeColor := AParent.NodeColor;
      myNote.NodeBGColor := AParent.NodeBGColor;
      myNote.ImageIndex := AParent.ImageIndex;
      // [x] inherit other properties here? (flagged?)
    end;
  end;

  AddNote( myNote );
  result := myNote;
end; // NewNode

function TKntFolder.AddNote( const aNote : TKntNote ) : integer;
begin
  FModified := true;
  result := InternalAddNote( aNote );
  if (( result >= 0 ) and ( aNote.ID = 0 )) then
     GenerateNoteID( aNote );
end; // AddNode

procedure TKntFolder.InsertNote( const aIndex : integer; const aNote : TKntNote );
begin
  FModified := true;
  InternalInsertNote( aIndex, aNote );
  if ( aNote.ID = 0 ) then
     GenerateNoteID( aNote );
end; // InternalInsertNode

procedure TKntFolder.InternalInsertNote( const aIndex : integer; const aNote : TKntNote );
begin
  if assigned( aNote ) then
    FNotes.Insert( aIndex, aNote );
end; // InternalInsertNode

function TKntFolder.InternalAddNote( const aNote : TKntNote ) : integer;
begin
  if assigned( aNote ) then
    result := FNotes.Add( aNote )
  else
    result := -1;
end; // InternalAddNode


procedure TKntFolder.GenerateNoteID( const aNote : TKntNote );
var
  i, hiID : Cardinal;
  myNote : TKntNote;
begin
  hiID := 0;

  for i := 0 to FNotes.Count-1 do begin
     myNote := FNotes[i];
     if ( myNote.ID > hiID ) then
        hiID := myNote.ID;  // find highest note ID
  end;

  inc( hiID ); // make it one higher
  aNote.ID := hiID;

end; // GenerateNodeID

procedure TKntFolder.VerifyNoteIDs;
var
  i: Cardinal;
  myNote : TKntNote;
begin
  if FNotes.Count = 0 then exit;
  for i := 0 to FNotes.Count-1 do begin
    myNote := FNotes[i];
    if ( myNote.ID <= 0 ) then
        GenerateNoteID( myNote );
  end;
end; // VerifyNodeIDs

procedure TKntFolder.RemoveNote( const aNote : TKntNote );
begin
  if ( not assigned( aNote )) then exit;

  TKntFile(KntFile).RemoveImagesCountReferences(aNote);

  FNotes.Remove( aNote );
  FModified := true;
end;

procedure TKntFolder.SetTreeChrome( AChrome : TChrome );
begin
  FTreeChrome := AChrome;
  FModified := true;
end; // SetTreeChrome

procedure TKntFolder.SetTreeProperties( const aProps : TFolderTreeProperties );
begin
  FCheckboxes := aProps.CheckBoxes;
  FIconKind := aProps.IconKind;
  FDefaultNoteName := aProps.DefaultName;
  FAutoNumberNotes := aProps.AutoNumberNodes;
  if FVerticalLayout <> aProps.VerticalLayout then
     FTreeMaxWidth:= -Abs(FTreeMaxWidth);
  FVerticalLayout := aProps.VerticalLayout;
  FHideCheckedNodes := aProps.HideChecked;           // [dpv]
end; // SetTreeProperties

procedure TKntFolder.GetTreeProperties( var aProps : TFolderTreeProperties );
begin
  aProps.CheckBoxes := FCheckboxes;
  aProps.IconKind := FIconKind;
  aProps.DefaultName := FDefaultNoteName;
  aProps.AutoNumberNodes := FAutoNumberNotes;
  aProps.VerticalLayout := FVerticalLayout;
  aProps.HideChecked:= FHideCheckedNodes;          // [dpv]
end; // GetTreeProperties


procedure TKntFolder.UpdateTree;
begin
  // CheckTree;
  // FTV.Items.BeginUpdate;
  try
    FTV.Color := FTreeChrome.BGColor;
    with FTreeChrome.Font do
    begin
      FTV.Font.Name := Name;
      FTV.Font.Size := Size;
      FTV.Font.Style := Style;
      FTV.Font.Charset := Charset;
      FTV.Font.Color := Color;
    end;
  finally
    // FTV.Items.EndUpdate;
  end;
end; // UpdateTree

procedure TKntFolder.ConfigureEditor;
var
   plainTxt: boolean;
begin
  if FEditor = nil then exit;

  if Self.SelectedNote = nil then begin
     FEditor.SupportsRegisteredImages:= false;
     FEditor.SupportsImages:= false;
     FEditor.SetVinculatedObjs(nil, nil, nil);
     FEditor.Enabled:= False;
     exit;
  end;

  plainTxt:= (StreamFormatInNote = sfPlainText);
  FEditor.SetVinculatedObjs(FKntFile, Self, Self.SelectedNote);
  FEditor.PlainText:= plainTxt;
  FEditor.Chrome:= Self.FEditorChrome;

  FEditor.SupportsRegisteredImages:= (ImageMng.StorageMode <> smEmbRTF) and
                           not plainTxt and (Self.SelectedNote.VirtualMode in [vmNone, vmKNTNode]);  // <- ?? o sólo NoteSupportsRegisteredImages
  FEditor.SupportsImages:= not plainTxt and (Self.SelectedNote.VirtualMode in [vmNone, vmKNTNode, vmRTF]);

end;

procedure TKntFolder.DataStreamToEditor;
var
  ReadOnlyBAK: boolean;
{$IFDEF WITH_IE}
  ov : OleVariant;
{$ENDIF}

{$IFDEF KNT_DEBUG}
 str: String;
 dataSize: integer;
{$ENDIF}

 strRTF: AnsiString;
 ContainsImgIDsRemoved: boolean;
 ContainsImages: boolean;

 OnEnterBak: TNotifyEvent;
 ControlWasFocused: TWinControl;

begin
  if not assigned(FEditor) then exit;
  if not assigned(FSelectedNote) then  begin
    FEditor.Clear;
    exit;
  end;

  BeforeEditorLoaded(FSelectedNote);

  ConfigureEditor;
  ControlWasFocused:= nil;

  case FSelectedNote.VirtualMode of
    vmNone, vmText, vmRTF, vmHTML, vmKNTNode : begin
      FEditor.BeginUpdate;                   // -> It will also ignore Enter and Change events

      if KeyOptions.FixScrollBars then begin
         if not ClipCapMng.IsBusy and (not FEditor.Focused) and FEditor.CanFocus then begin
            ControlWasFocused:= Form_Main.ActiveControl;
            FEditor.SetFocus;
         end;
      end;

      ReadOnlyBAK:= FEditor.ReadOnly;
      ContainsImgIDsRemoved:= false;
      try
        FEditor.ReadOnly:= false;   // To prevent the problem indicated in issue #537
        FEditor.Clear;

        FSelectedNote.Stream.Position := 0;
        strRTF:= '';

        if ( FPlainText or ( FSelectedNote.VirtualMode in [vmText, vmHTML] )) then
           UpdateEditor;

        fImagesReferenceCount:= nil;
        if NodeStreamIsRTF (FSelectedNote.Stream) then begin
           FEditor.StreamFormat:= sfRichText;
           if (ImageMng.StorageMode <> smEmbRTF) and (FSelectedNote.VirtualMode in [vmNone, vmKNTNode]) and (not FPlainText) then begin
              GetImagesIDInstances (FSelectedNote.Stream, FSelectedNote.NoteTextPlain);
              strRTF:= ImageMng.ProcessImagesInRTF(FSelectedNote.Stream.Memory, FSelectedNote.Stream.Size, Self.Name, ImageMng.ImagesMode, '', 0, ContainsImgIDsRemoved, ContainsImages, true);
           end;
        end
        else
           FEditor.StreamFormat:= sfPlainText;

        Log_StoreTick('TKntFolder.DataStreamToEditor - BEGIN', 4, +1);
       {$IFDEF KNT_DEBUG}
        if log.Active and  (log.MaxDbgLevel >= 4) then begin
           dataSize:= FSelectedNote.Stream.Size;
           str:= Copy(String(PAnsiChar(FSelectedNote.Stream.Memory)), 1, 250);
           Log.Add(string.format('sfRichText?:%s DataSize:%d  RTF:"%s"...', [BoolToStr(FEditor.StreamFormat=sfRichText), dataSize, str]),  4 );
        end;
       {$ENDIF}

        if StrRTF <> '' then begin
           FEditor.PutRtfText(strRTF,True,False);               // => ImageManager.StorageMode <> smEmbRTF
           FEditor.ClearUndo;
        end
        else
           FEditor.Lines.LoadFromStream( FSelectedNote.Stream );

        Log_StoreTick('TKntFolder.DataStreamToEditor - END', 4, -1);

        FEditor.Color := FSelectedNote.RTFBGColor;
        FEditor.SelStart := FSelectedNote.SelStart;
        FEditor.SelLength := FSelectedNote.SelLength;

        if FSelectedNote.Stream.Size = 0 then     // Ensures that new nodes are correctly updated based on default properties (font color, size, ...)
           UpdateEditor;

      finally
        FEditor.ReadOnly:= ReadOnlyBAK;
        FEditor.EndUpdate;

        if not ContainsImgIDsRemoved then
           FEditor.Modified := false;

        if KeyOptions.FixScrollBars and (ControlWasFocused <> nil) then
           ControlWasFocused.SetFocus;
      end;
    end;

    vmIELocal, vmIERemote : begin
      {$IFDEF WITH_IE}
      ov := 0;
      FWebBrowser.Navigate( FSelectedNode.VirtualFN, ov, ov, ov, ov );
      {$ENDIF}
    end;
  end;

  FEditor.Enabled:= true;
  AfterEditorLoaded(FSelectedNote);

  App.EditorLoaded(FEditor);

end; // DataStreamToEditor

{
  If Editor was modified then it will return the Stream associated to the node that will be updated
}
function TKntFolder.EditorToDataStream: TMemoryStream;
var
   KeepUTF8: boolean;
   Encoding: TEncoding;
   strRTF: AnsiString;
   ImagesIDs_New: TImageIDs;
   TextPlain: string;
begin
  Result:= nil;
  Encoding:= nil;
  if assigned(FEditor) and assigned(FSelectedNote) then begin
     FSelectedNote.SelStart  := FEditor.SelStart;
     FSelectedNote.SelLength := FEditor.SelLength;

     if FEditor.Modified then begin
        FEditor.BeginUpdate;
        try
           KeepUTF8:= False;
           if (FSelectedNote.VirtualMode in [vmText, vmHTML]) and NodeStreamIsUTF8WithBOM(FSelectedNote.Stream) then
               KeepUTF8:= True;

           FModified := FModified or FEditor.Modified;
           FSelectedNote.Stream.Clear;

           try
             FEditor.StreamFormat:= StreamFormatInNote();
             FEditor.StreamMode := [];
             if FEditor.StreamFormat = sfPlainText then begin
                // Si es un nodo virtual respetaremos la codificación UTF8 que pueda tener.
                // En caso contrario sólo se guardará como UTF8 si es necesario
                if KeepUTF8 or not CanSaveAsANSI(FEditor.Text) then
                   //FEditor.StreamMode := [smUnicode];
                   Encoding:= TEncoding.UTF8;
             end;

             FEditor.Lines.SaveToStream( FSelectedNote.Stream, Encoding);

             ImagesIDs_New:= nil;
             if (ImageMng.StorageMode <> smEmbRTF) and (FSelectedNote.VirtualMode in [vmNone, vmKNTNode]) and (FEditor.StreamFormat = sfRichText) then begin
                ImagesIDs_New:= CheckSavingImagesOnMode (imLink, FSelectedNote.Stream, true);
                ImageMng.UpdateImagesCountReferences (fImagesReferenceCount, ImagesIDs_New);
                fImagesReferenceCount:= ImagesIDs_New;
             end;

             if ImagesIDs_New = nil then
                FSelectedNote.NoteTextPlain:= FEditor.TextPlain
             else begin
                { If the node has images we will make sure that in TextPlain we save the version corresponding to imLink,
                  to facilitate search management. See notes on TImageMng.GetPositionOffset }
                FSelectedNote.NoteTextPlain := '';
                InitializeTextPlain(FSelectedNote, RTFAux_Note);
             end;
             FSelectedNote.Stream.Position := 0;
             Result:= FSelectedNote.Stream;
             FEditor.Modified:= false;

           finally
             FEditor.StreamFormat := sfRichText;
             FEditor.StreamMode := [];
           end;


        finally
          FEditor.EndUpdate;
        end;
     end
     else
       if (FSelectedNote.NoteTextPlain = '') then
          InitializeTextPlain(FSelectedNote, RTFAux_Note);

  end;
end; // EditorToDataStream


function TKntFolder.StreamFormatInNote: TRichStreamFormat;
begin
    case FSelectedNote.VirtualMode of
      vmNone, vmKNTNode : begin
        if FPlainText then
          Result:= sfPlainText
        else
          Result:= sfRichText;
      end;
      vmText, vmHTML : begin
        Result:= sfPlainText;
      end;
      vmRTF : begin
        Result:= sfRichText;
      end;
    end;

end;

function TKntFolder.CheckTree : boolean;
begin
  result := assigned( FTV );
end; // CheckTree


function TKntFolder.SaveToFile( var tf : TTextFile; OnlyCurrentNodeAndSubtree: TTreeNTNode= nil;
                               OnlyNotHiddenNodes: boolean= false; OnlyCheckedNodes: boolean= false ): integer;
var
  treeNode : TTreeNTNode;
  note : TKntNote;
  nodessaved, NodeCnt, NodeIdx : integer;
  wasmismatch : boolean;
  HaveVCLControls : boolean;
  // bakFN : string;
begin
  HaveVCLControls := CheckTree;
  nodessaved := 0;

  // sanity check

  wasmismatch := ( HaveVCLControls and (( FTV.Items.Count ) <> ( FNotes.Count )));
  if wasmismatch then begin
     if ( DoMessageBox(Format(STR_05, [FName,FTV.Items.Count,FNotes.Count]),
                       Format(STR_06, [FName]), MB_YESNO+MB_ICONEXCLAMATION+MB_DEFBUTTON1+MB_APPLMODAL ) <> ID_YES ) then
        raise EKntFolderError.Create(STR_07);
  end;


  if HaveVCLControls then begin
    {                           // Now updated from TForm_Main.SplitterNoteMoved
    if FVerticalLayout then
       FTreeWidth := FTV.Height
    else
       FTreeWidth := FTV.Width;
    }

    if assigned( FTV.Selected ) then
       FOldSelectedIndex := FTV.Selected.AbsoluteIndex
    else
       FOldSelectedIndex := -1;
  end;



  try
    tf.WriteLine( _NF_TreeNote ); // marks beginning of TTreeNote
    BaseSaveProc( tf );

    // basic treenote properties
    tf.WriteLine( _SelectedNode + '=' + FOldSelectedIndex.ToString );
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

    NodeCnt := FNotes.Count;
    NodeIdx := 0;

    note := nil;
    treeNode := nil; // initialize to eliminate compiler warning

    // obtain first node
    if HaveVCLControls then  begin
       if OnlyCurrentNodeAndSubtree <> nil then
          treeNode := OnlyCurrentNodeAndSubtree
       else
          treeNode := FTV.Items.GetFirstNode;
       if assigned( treeNode ) then begin
          note := TKntNote( treeNode.data );
          if assigned( note ) then
             note.Level := treeNode.Level;
       end;
    end
    else begin
       if ( NodeCnt > 0 ) then
          note := FNotes[0];
    end;

    while assigned( note ) do begin
      if not ( (OnlyCheckedNodes   and not note.Checked) or
               (OnlyNotHiddenNodes and Self.GetTreeNodeByID(note.ID).Hidden) )  then begin

          inc( nodessaved );

          tf.WriteLine( _NF_TRN  );
          tf.WriteLine( _NodeLevel + '=' + note.Level.ToString );

          tf.WriteLine( _NodeName + '=' + note.Name, True);
          tf.WriteLine( _NodeID + '=' + note.ID.ToString  );
          tf.WriteLine( _NodeFlags + '=' + note.PropertiesToFlagsString);
          tf.WriteLine( _NodeRTFBGColor + '=' + ColorToString(note.RTFBGColor) );
          tf.WriteLine( _NodeImageIndex + '=' + note.ImageIndex.ToString  );
          if note.HasNodeColor then
             tf.WriteLine( _NodeColor + '=' + ColorToString(note.NodeColor) );
          if note.HasNodeBGColor then
             tf.WriteLine( _NodeBGColor + '=' + ColorToString(note.NodeBGColor) );
          if note.HasNodeFontFace then
             tf.WriteLine( _NodeFontFace + '=' + note.NodeFontFace );

          if (note.VirtualMode <> vmKNTNode) and (note.HasAlarms(true)) then
             SaveAlarms(tf, note);

          if ( _SAVE_RESTORE_CARETPOS and ( note.SelStart > 0 )) then
             tf.WriteLine( _NodeSelStart + '=' + note.SelStart.ToString  );



          if (note.VirtualMode = vmNone ) then
             SaveRTFToFile(tf, note.Stream, FPlainText)

          else
          if note.VirtualMode = vmKNTNode  then
             tf.WriteLine( _VirtualNode + '=' + note.MirrorNodeID  )

          else begin
            if note.HasVNodeError then
               // there was an error when we tried to load this file, so don't try to save it (assume no valid data in node)
                tf.WriteLine( _VirtualFN + '=' + copy( note.VirtualFN, 2, length( note.VirtualFN )), True )

            else
                try
                   note.SaveVirtualFile;

                   tf.WriteLine( _RelativeVirtualFN + '=' + note.RelativeVirtualFN, True  ); // MUST be done AFTER NoteNode.SaveVirtualFile. MUST also be saved BERFORE notenode.VirtualFN.
                   tf.WriteLine( _VirtualFN + '=' + note.VirtualFN, True  );
                except
                  on E : Exception do
                    // [x] A note may have hundreds of nodes.We should allow user to ABORT here or to skip subsequent error messages
                    DoMessageBox(Format(STR_08 + #13+ '%s'+ #13#13+ '%s', [note.Name, self.Name, note.VirtualFN, E.Message]), mtError, [mbOK], 0 );
                end;
          end;

      end;

      // obtain next node, or bail out if NIL
      note := nil;
      if HaveVCLControls then  begin
         treeNode := treeNode.GetNext;
         if OnlyCurrentNodeAndSubtree <> nil then begin
             if (OnlyCurrentNodeAndSubtree.GetNextSibling = treeNode) then
                treeNode := nil;
         end;
         if assigned( treeNode ) then begin
            note := TKntNote( treeNode.data );
            if assigned( note ) then
               note.Level := treeNode.Level;
         end;
      end
      else begin
         inc( NodeIdx );
         if ( NodeIdx < NodeCnt ) then
            note := FNotes[NodeIdx];
      end;
    end;

    Modified := false;

  finally
    if (OnlyCurrentNodeAndSubtree = nil) and not OnlyCheckedNodes and not OnlyNotHiddenNodes
       and ( nodessaved <> FNotes.Count ) then
        raise EKntFolderError.CreateFmt(STR_09, [FNotes.Count, nodessaved]);

    Result:= nodesSaved;
  end;

end; // SaveToFile


procedure TKntFolder.LoadFromFile( var tf : TTextFile; var FileExhausted : boolean; var NextBlock: TNextBlock; LoadOldSimpleNote: boolean= false);
var
  InRichText : boolean;
  InNoteNode : boolean;
  List : TStringList;
  s, key : AnsiString;
  p, linecount : integer;
  myNote : TKntNote;

    procedure AddNewNode;
    begin
      // transfer RTF data from list to node
      InRichText := false;
      TransferRTFData(List, myNote.Stream);
      myNote.Stream.Position := 0;
      List.Clear;
      InternalAddNote( myNote );
      myNote := nil;
    end; // AddNewNode

begin

  FileExhausted := false;
  InRichText := false;
  InNoteNode := false;
  myNote := nil; // DO NOT REMOVE! (Otherwise we get an AV when loading a tree with zero nodes)

  List := TStringList.Create;
  List.BeginUpdate;
  try
    while (not tf.eof()) do begin
       s:= tf.readln();

       if ( s = _NF_RTF ) then begin
         // RTF data begins
         InRichText := true;
         if LoadOldSimpleNote then begin
            InNoteNode := true;
            FTreeHidden:= true;
            FIconKind := niCustom;
            // create a new blank node
            myNote := TKntNote.Create;
            myNote.RTFBGColor := EditorCHrome.BGColor;
            myNote.Level:= 0;
            myNote.ID := 0;
            myNote.Name := FName;
         end;
         continue;
       end;
       if ( s = _NF_TRN ) then begin
         // new NoteNode begins
         if ( InNoteNode ) then AddNewNode; // we were here previously, i.e. there a node to be added
         InNoteNode := true;
         // create a new blank node
         myNote := TKntNote.Create;
         myNote.RTFBGColor := EditorCHrome.BGColor;
         continue;
       end;
       if ( s = _NF_TabNote ) then begin
         NextBlock:= nbRTF;
         if assigned( myNote ) then AddNewNode;
         break; // New TabNote begins
       end;
       if ( s = _NF_TreeNote ) then begin
         NextBlock:= nbTree;
         if ( myNote <> nil ) then
           AddNewNode;
         break; // New TreeNote begins
       end;
       if ( s = _NF_StoragesDEF ) then begin
         NextBlock:= nbImages;
         if ( myNote <> nil ) then
           AddNewNode;
         break; // Images definition begins
       end;
       if ( s = _NF_Bookmarks ) then begin
         NextBlock:= nbBookmarks;
         if ( myNote <> nil ) then
           AddNewNode;
         break; // Bookmarks begins
       end;
       if ( s = _NF_EOF ) then begin
         FileExhausted := true;
         if assigned( myNote ) then AddNewNode;
         break; // END OF FILE
       end;


       if InRichText then begin
         { can only be TRUE if the file uses the new "LI-less" format,
           because we got the _NF_EOH token, above. Old format doesn't
           have this token, so InRichText is never true }
         if FPlainText then
           delete( s, 1, 1 ); // strip _NF_PLAINTEXTLEADER
         List.Add( s );
         continue;
       end;


       p := pos( '=', s );
       if ( p <> 3 ) then continue; // not a valid key=value format
       key := copy( s, 1, 2 );
       delete( s, 1, 3 );


       if InNoteNode and (not LoadOldSimpleNote) then begin
          if ( key = _NodeName ) then
            myNote.Name := TryUTF8ToUnicodeString(s)
          else
          if ( key = _NodeID ) then
              myNote.ID := StrToUIntDef( s, 0 )
          else
          if ( key = _NodeLevel ) then
              myNote.Level := StrToIntDef( s, 0 )
          else
          if ( key = _NodeFlags ) then
            myNote.FlagsStringToProperties( s )
          else
          if ( key = _NodeRTFBGColor ) then begin
            try
              myNote.RTFBGColor := StringToColor( s );
            except
            end;
          end
          else
          if ( key = _VirtualNode ) then
            myNote.MirrorNodeID := TryUTF8ToUnicodeString(s)
          else
          if ( key = _RelativeVirtualFN ) then
            myNote.RelativeVirtualFN := TryUTF8ToUnicodeString(s)
          else
          if ( key = _VirtualFN ) then begin
            myNote.VirtualFN := TryUTF8ToUnicodeString(s);
            try
              myNote.LoadVirtualFile;
            except
              on E : Exception do
              begin
                List.Add( STR_10 );
                List.Add( myNote.VirtualFN );
                List.Add( E.Message );
                myNote.VirtualFN := _VIRTUAL_NODE_ERROR_CHAR + myNote.VirtualFN;
              end;
            end;
          end
          else
          if ( key = _NodeSelStart ) then begin
              if _SAVE_RESTORE_CARETPOS then
                 myNote.SelStart := StrToIntDef( s, 0 )
              else
                 myNote.SelStart := 0;
          end
          else
          if ( key = _NodeImageIndex ) then
              myNote.ImageIndex := StrToIntDef( s, -1 )
          else
          if ( key = _NodeColor ) then begin
            try
              myNote.NodeColor := StringToColor( s );
            except
              myNote.HasNodeColor := false;
            end;
          end
          else
          if ( key = _NodeBGColor ) then begin
            try
              myNote.NodeBGColor := StringToColor( s );
            except
              myNote.HasNodeBGColor := false;
            end;
          end
          else
          if ( key = _NodeFontFace ) then
            myNote.NodeFontFace := s
          else
          if ( key = _NodeAlarm ) then      // [dpv*]
              ProcessAlarm(s, myNote);

          continue;
       end; // if InNoteNode ...



       if ( key = _SelectedNode ) then
             FOldSelectedIndex := StrToIntDef( s, -1 )
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
             FDateCreated := StrToDateTimeDef( s, Now )
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
       if ( key = _PosX ) then
           FCaretPos.X := StrToIntDef( s, 0 )
       else
       if ( key = _POSY ) then
           FCaretPos.Y := StrToIntDef( s, 0 )
       else
       if ( key = _TabIndex ) then
           FTabIndexAux := StrToIntDef( s, 0 )
       else
       if ( key = _TabSize ) then
          FTabSize := StrToIntDef( s, DEF_TAB_SIZE )

       else
       if ( key = _NodeAlarm ) then
           ProcessAlarm(s);

    end; { while not eof( tf ) }

  finally
    VerifyNoteIDs;
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
  myNote : TKntNote;
  List : TStringList;

    procedure AddNewNode;
    begin
      // transfer RTF data from list to node
      List.WriteBOM:= False;
      List.SaveToStream( myNote.Stream );
      myNote.Stream.Position := 0;
      List.Clear;
      InternalAddNote( myNote );
      myNote := nil;
    end; // AddNewNode

begin
  InNode := false;

  if ( not fileexists( FN )) then exit;

  tf:= TTextFile.Create();
  tf.AssignFile(FN);

  try
    tf.Reset;
  except
    messagedlg( STR_11 + FN, mtError, [mbOK], 0 );
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
                try
                  level := StrToInt( s );
                except
                  level := 0;
                end;
                myNote := TKntNote.Create;
                myNote.RTFBGColor := EditorChrome.BGColor;
                myNote.Level := Level;
                myNote.Name := nodeName;
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
    VerifyNoteIDs;
    List.Free;
    tf.CloseFile;
  end;

end; // LoadFromTreePadFile


function TKntFolder.GetNoteByID( const aID : integer ) : TKntNote;
var
  i: integer;
begin
  for i:= 0 to Notes.Count-1 do
     if Notes[i].ID = aID then
        exit(Notes[i]);

   Result := nil;
end; // GetNoteByID



function TKntFolder.GetTreeNodeByID( const aID : integer ) : TTreeNTNode;
var
  myTreeNode : TTreeNTNode;
begin
  result := nil;
  myTreeNode := FTV.Items.GetFirstNode;
  while assigned( myTreeNode ) do
  begin
    if assigned( myTreeNode.Data ) then
    begin
      if ( TKntNote( myTreeNode.Data ).ID = aID ) then
      begin
        result := myTreeNode;
        break;
      end;
    end;
    myTreeNode := myTreeNode.GetNext;
  end;
end; // GetTreeNodeByID


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


function TKntFolder.InitializeTextPlain(myNote: TKntNote; RTFAux: TAuxRichEdit): boolean;
begin
    Result:= False;  // Initialization was required?

    if myNote.NoteTextPlain = '' then begin
       LoadStreamInRTFAux (myNote.Stream, RTFAux);
       myNote.NoteTextPlain:= RTFAux.TextPlain;
       Result:= True;
    end;
end;


function TKntFolder.InitializeTextPlainVariables( nMax: integer; RTFAux: TAuxRichEdit): boolean;
var
  i, N: integer;
begin
  Result:= false;          // Returns True if all nodes have TextPlain initialized

  N:= 0;
  for i := 0 to FNotes.Count - 1 do  begin
     if (i mod 20) = 0 then begin
        Application.ProcessMessages;
        if (MillisecondsIdle <= 450) then Exit;
     end;

     if InitializeTextPlain (FNotes[i], RTFAux) then
        inc (N);

     if N >= nMax then Exit;
  end;

  Result:= true;
end;


Initialization

end.
