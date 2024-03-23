unit kn_NoteFileMng;

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
   System.SysUtils,
   System.StrUtils,
   System.DateUtils,
   System.Classes,
   System.IOUtils,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   TreeNT,

   kn_const,
   kn_Info
   ;


    function KntFileNew( FN : string ) : integer; // create new KNT file with 1 blank folder
    function KntFileOpen( FN : string ) : integer; // open KNT file on disk
    function KntFileSave( FN : string ) : integer; // save current KNT file
    function KntFileClose : boolean; // close current KNT file

    procedure KntFileCopy (var SavedFolders: integer; var SavedNodes: integer;
                            FN: string= '';
                            ExportingMode: boolean= false;
                            OnlyCurrentNodeAndSubtree: TTreeNTNode= nil;
                            OnlyNotHiddenNodes: boolean= false;
                            OnlyCheckedNodes: boolean= false);

    procedure MergeFromKNTFile( MergeFN : string );

    function CheckFolder( const name, folder : string; const AttemptCreate, Prompt : boolean ) : boolean;
    procedure FolderChanged;
    procedure SomeoneChangedOurFile;

    function CheckModified( const Warn : boolean; const closing: boolean ) : boolean;

    procedure ImportFiles;
    procedure ImportAsKntFolders( ImportFileList : TStringList; ImgLinkMode: boolean );
    procedure InsertContent( ImportFileList : TStringList; ImgLinkMode: boolean; const NameProposed: string = '' );

    procedure FileDropped( Sender : TObject; FileList : TStringList );
    function ConsistentFileType( const aList : TStringList ) : boolean;
    function PromptForFileAction( const FileList : TStringList; const aExt : string; var ImgLinkMode: boolean; var NewFileName: string; var RelativeLink: boolean ) : TDropFileAction;

    procedure KntFileProperties;
    procedure UpdateKntFileState( AState : TFileStateChangeSet );

    procedure AutoCloseKntFile;
    procedure RunFileManager;

    function CanRegisterFileType : boolean;
    procedure AssociateKeyKntFile;

    function ExtIsRTF( const aExt : string ) : boolean;
    function ExtIsHTML( const aExt : string ) : boolean;
    function ExtIsText( const aExt : string ) : boolean;
    function ExtIsImage( const aExt : string ) : boolean;


implementation

uses
   BrowseDr,
   ZLibEx,
   RxRichEd,
   gf_misc,
   gf_files,
   gf_FileAssoc,
   gf_streams,
   Kn_Global,
   kn_filemgr,
   kn_FileObj,
   kn_FileInfo,
   kn_NodeList,
   kn_NoteMng,
   kn_EditorUtils,
   kn_TabSelect,
   kn_FileDropAction,
   kn_ExportImport,
   kn_Main,
   kn_Cmd,
   kn_NoteObj,
   kn_Macro,
   kn_Chest,
   kn_ConfigMng,
   kn_VirtualNodeMng,
   kn_MacroMng,
   kn_TreeNoteMng,
   kn_VCLControlsMng,
   kn_BookmarksMng,
   kn_LinksMng,
   kn_PluginsMng,
   kn_ImagesMng,
   kn_FindReplaceMng
   ;




type
   TMergeFolders = record
       oldID: integer;
       newID: integer;
       newFolder: boolean;
   end;



resourcestring
  STR_01 = 'Cannot create a new file: ';
  STR_02 = ' New KNT file created.';
  STR_03 = '(none)';
  STR_04 = 'A new KNT file has been created. Would you like to save the new file now?' +#13#13+ '(The Auto Save function will not work until the file is named and saved first.)';
  STR_05 = 'Open Keynote file';
  STR_06 = ' Opening ';
  STR_07 = 'One or more errors occurred while loading the file. The file may not have loaded completely. To minimize the risk of data loss, ' +
                          'the file was opened in Read-Only mode. Use the "Save As..." command to save the file.';
  STR_08 = ' <unknown> ';
  STR_09 = ' diskette ';
  STR_10 = ' network ';
  STR_11 = ' CD-ROM ';
  STR_12 = ' RAM ';
  STR_13 = 'File "%s" was opened in Read-Only mode, because it resides on a %s drive "%s".';
  STR_14 = ' File opened.';
  STR_15 = ' Error.';
  STR_16 = 'Folder monitor error: ';
  STR_17 = ' ERROR %d opening file';
  STR_18 = 'This file will be saved as a %s file. This format does not support some features which are unique to %s.' + #13#13 +
                                     'OK to save the file in Dart Notes format? If you answer NO, the file will be saved as a %s file.';
  STR_19 = ' Saving ';
  STR_20 = 'Specified backup directory "%s" does not exist. Backup files will be created in the original file''s directory.';
  STR_21 = 'Cannot create backup file (error %d: %s). Current file will not be backed up. Proceed anyway?'+ #13#13 +' (Note: File was temporary saved in %s)';
  STR_22 = ' File saved (%d folders, %d notes)';
  STR_23 = ' Error %d while saving file.';

  STR_InfSaving = '* NOTE:' +  #13 +
                  '  - The .knt file in disk must not have been modified from last correct save.'  + #13 +
                  '  - You should have multiple backup files in the folder %s, specially if you selected the option "Backup at regular intervals" (highly recommended)';
  STR_24 = 'Error %d occurred while saving to a temporal folder (%s). The contents of the file in memory are perhaps partially corrupted.'  + #13#13 +
            '-> Please, retry, and if you can''nt save to a .knt file, try to recover the nodes/notes with unsaved changes using, for example, File -> Export...'  + #13#13#13;
  STR_25 = 'Failed to create output file "%s" (Error: %d)' + #13 + 'File was temporary saved in %s' + #13#13#13 ;
  STR_26 = 'The Auto-Save option was turned OFF, to prevent KeyNote from automatically saving the (perhaps) damaged file.';

  STR_27 = ' ERROR saving file';
  STR_28 = 'Saving "';
  STR_29 = 'Folder monitoring has been disabled due to the following error: ';
  STR_30 = ' File closed.';
  STR_31 = 'Revert to last saved version of' + #13 + '%s?';
  STR_32 = 'Select backup folder';
  STR_33 = 'Cannot copy file to its own directory.';
  STR_34 = 'The file %s already exists. OK to overwrite existing file?';
  STR_35 = ' Copying file...';
  STR_36 = ' File copied.';
  STR_37 = 'Successfully copied KNT file to';
  STR_38 = 'Copying failed (';
  STR_39 = 'Select file to merge notes from';
  STR_40 = 'There was an error while loading merge file.';
  STR_41 = 'The file you selected does not contain any folders.';
  STR_42 = 'Error while loading merge file: ';
  STR_43 = 'Notes in %s';
  STR_44 = 'You did not select any notes: nothing to merge.';
  STR_45 = ' Merging notes...';
  STR_46 = 'Error while adding folders: ';
  STR_47 = 'Merged %d notes from "%s"';
  STR_48 = 'No notes were merged';
  STR_49 = 'Another application has modified the note file %s. Reload the file from disk?';
  STR_50 = '%s folder "%s" does not exist';
  STR_51 = '. Create the folder now?';
  STR_52 = 'Could not create folder: %s';
  STR_53 = ' File modified by external application.';
  STR_54 = 'Notes were modified. Save file before continuing?' +#13+ 'If you answer No, you will lose all changes made since last save.';
  STR_55 = 'Current file has not been saved. If you continue, changes will be lost.'+ #13 + 'Proceed anyway?';
  STR_56 = 'Warning!';
  STR_57 = 'Select files for importing';
  STR_58 = 'The file "%s" does not appear to be a text file (nor image). The result of importing it may be unpredictable.' + #13#13 +
                'Import as a plain text file, anyway?';
  STR_59 = ' Importing ';
  STR_60 = 'Failed to convert HTML file "%s" to RTF';
  STR_61 = 'Error importing ';
  STR_62 = ' Finished importing.';
  STR_63 = 'Cannot select methods for handling files.';
  STR_64 = 'Files you are trying to import are of more than one type. Please select only files of one type for importing.';
  STR_65 = 'Cannot import a directory "%s"';
  STR_67 = 'Unknown or unexpected file action (%d)';
  STR_68 = 'Error while importing files: ';
  STR_69 = 'Untitled';
  STR_70 = ' No file ';
  STR_71 = ' (no file)';
  STR_72 = ' Auto';
  STR_73 = ' MOD';
  STR_74 = ' Saved';
  STR_75 = 'Successfully created %s registry entries';
  STR_76 = 'There was an error while creating file type associations: ';
  STR_77 = 'This file is Read-Only. Use "Save As" command to save it with a new name.';
  STR_78 = 'Backup at %s before any modification in "%s"';
  STR_79 = 'File is not modified. Nothing to save';
  STR_80 = #13#13 + 'Option "Autoregister file type" will be unchecked';
  STR_81 = 'Cannot insert images in a plain text folder';
  STR_82 = 'The file must first be saved (with Save or Save As)';

const
  IntervalBAKDay = '_BAK_Day.knt';
  IntervalPrefixBAK = '_BAK@';



function ExtIsHTML( const aExt : string ) : boolean;
begin
  result := (aExt <> '') and ( pos( ansilowercase( aExt )+'.', KeyOptions.ExtHTML ) > 0 )
end;

function ExtIsText( const aExt : string ) : boolean;
begin
  result := (aExt <> '') and ( pos( ansilowercase( aExt )+'.', KeyOptions.ExtText ) > 0 )
end;

function ExtIsRTF( const aExt : string ) : boolean;
begin
  // result := ( CompareText( aExt, ext_RTF ) = 0  );
  result := (aExt <> '') and ( pos( ansilowercase( aExt )+'.', KeyOptions.ExtRTF ) > 0 )
end;

function ExtIsImage( const aExt : string ) : boolean;
begin
  Result := (aExt <> '') and ( pos( AnsiUpperCase(aExt )+';', IMAGE_EXTENSIONS_RECOGNIZED ) > 0 )
end;



//=================================================================
// KntFileNew
//=================================================================
function KntFileNew( FN : string ) : integer;
begin
  if assigned( KntFile ) then
     if (not KntFileClose ) then exit(-2);

  MovingTreeNode:= nil;
  AlarmManager.Clear;
  ImagesManager.Clear;
  MirrorNodes.Clear;

  with Form_Main do begin
        result := -1;
        _REOPEN_AUTOCLOSED_FILE := false;
        if FileIsBusy then exit;
        Virtual_UnEncrypt_Warning_Done := false;
        try
          try
            result := 0;
            FolderMon.Active := false;
            FileIsBusy := true;
            Pages.MarkedPage := nil;

            if ( DEF_FN <> OrigDEF_FN ) then
            begin
              DEF_FN := OrigDEF_FN;
            end;

            LoadDefaults;
            LoadTabImages( false );

            KntFile := TKntFile.Create;
            KntFile.PageCtrl := Pages;
            KntFile.PassphraseFunc := GetFilePassphrase;
            KntFile.FileFormat := KeyOptions.SaveDefaultFormat;

            if ( KeyOptions.RunAutoMacros and fileexists( _MACRO_AUTORUN_NEW_FILE )) then begin
              Application.ProcessMessages;
              ExecuteMacro( _MACRO_AUTORUN_NEW_FILE, '' );
            end
            else
              NewKntFolder( true, false );

          except
            on E : Exception do
            begin
             {$IFDEF KNT_DEBUG}
              Log.Add( 'Exception in KntFileNew: ' + E.Message );
             {$ENDIF}
              Popupmessage( STR_01 + E.Message, mtError, [mbOK], 0 );
              result := 1;
              exit;
            end;
          end;
        finally
          LastEditCmd := ecNone;
          MMEditRepeat.Enabled := false;
          RTFMRepeatCmd.Enabled := false;
          TB_Repeat.ENabled := false;

          StatusBar.Panels[PANEL_HINT].Text := STR_02;

          UpdateKntFileState( [fscNew,fscModified] );
        {$IFDEF KNT_DEBUG}
          Log.Add( 'KntFileNew result: ' + inttostr( result ));
        {$ENDIF}
          FileIsBusy := false;
          if ( Pages.PageCount > 0 ) then
          begin
            ActiveKntFolder := TKntFolder(Pages.ActivePage.PrimaryObject);
            TAM_ActiveName.Caption := ActiveKntFolder.Name;
            FocusActiveKntFolder;
          end
          else
          begin
            ActiveKntFolder := nil;
            TAM_ActiveName.Caption := STR_03;
          end;

          ImagesManager.SetInitialImagesStorageMode(KeyOptions.ImgDefaultStorageMode, KeyOptions.ImgDefaultExternalStorage);

          UpdateNoteDisplay;

          if ( assigned( ActiveKntFolder ) and KeyOptions.RunAutoMacros ) then begin
             Application.ProcessMessages;
             ExecuteMacro( _MACRO_AUTORUN_NEW_TREE, '' );
          end;

        end;

        if ( KeyOptions.AutoSave and ( not KeyOptions.SkipNewFilePrompt )) then
        begin
          if ( PopupMessage( STR_04, mtConfirmation, [mbYes,mbNo], 0 ) = mrYes ) then
            KntFileSave( KntFile.FileName );
        end;
  end;

end; // KntFileNew



//=================================================================
// KntFileOpen
//=================================================================

function KntFileOpen( FN : string ) : integer;
var
  i : integer;
  OpenReadOnly : boolean;
  {$IFDEF KNT_DEBUG}OpenEnd: integer;{$ENDIF}
  opensuccess : boolean;
  FPath, NastyDriveType : string;
begin
  if assigned( KntFile ) then
    if ( not KntFileClose ) then exit(-2);

  with Form_Main do begin
        result := -1;
        _REOPEN_AUTOCLOSED_FILE := false;
        MovingTreeNode:= nil;
        AlarmManager.Clear;
        MirrorNodes.Clear;
        OpenReadOnly := false;
        opensuccess := false;
        Virtual_UnEncrypt_Warning_Done := false;
        if FileIsBusy then exit;
        try
          try
            FolderMon.Active := false;
            if ( FN = '' ) then
            begin
              with OpenDlg do
              begin
                Title := STR_05;
                Filter := FILTER_NOTEFILES + '|' + FILTER_DARTFILES + '|' + FILTER_ALLFILES;
                Options := Options - [ofHideReadOnly];
                Options := Options - [ofAllowMultiSelect];
                if ( KeyOptions.LastFile <> '' ) then
                  InitialDir := ExtractFilePath( KeyOptions.LastFile )
                else
                  InitialDir := GetFolderPath( fpPersonal );
              end;
              try
                if OpenDlg.Execute then
                begin
                  FN := OpenDlg.FileName;
                  OpenReadOnly := ( ofReadOnly in OpenDlg.Options );
                end
                else
                begin
                  exit;
                end;
              finally
                OpenDlg.Options := OpenDlg.Options + [ofHideReadOnly];
              end;
            end;
            FN := normalFN( FN );
            if ( ExtractFileExt( FN ) = '' ) then
              FN := FN + ext_KeyNote;

            StatusBar.Panels[PANEL_HINT].Text := STR_06 + FN;

            Timer.Enabled := false;
            screen.Cursor := crHourGlass;
            FileIsBusy := true;
            result := 0;
            KntFile := TKntFile.Create;
            KntFile.PassphraseFunc := GetFilePassphrase;
            KntFile.PageCtrl := Pages;

            // KntFile.OnNoteLoad := OnNoteLoaded;

            Log_Flush;
            Log_StoreTick('');
            Log_StoreTick( 'FileOpen (' + FN + ') - BEGIN', 0, +1);

            result := KntFile.Load( FN, ImagesManager );

            Log_StoreTick( 'After parsed .knt file', 1 );

            if KntFile.OpenAsReadOnly then
               OpenReadOnly:= True;

            for i := 0 to KntFile.Folders.Count -1 do
            begin
              KntFile.Folders[i].AddProcessedAlarms();
            end;

            if ( result <> 0 ) then
            begin
              KntFile.ReadOnly := true;
              result := 0;
              messagedlg( STR_07, mtWarning, [mbOK] , 0 );
            end;

            if FileExists( KntFile.FileName + ext_DEFAULTS ) then
              DEF_FN := KntFile.FileName + ext_DEFAULTS
            else
              DEF_FN := OrigDEF_FN;
            LoadDefaults;

            NastyDriveType := '';
            case GetDriveType( PChar( ExtractFileDrive( KntFile.FileName ) + '\' )) of
              0, 1 : begin
                NastyDriveType := STR_08;
              end;
              DRIVE_REMOVABLE : begin
                if KeyOptions.OpenFloppyReadOnly then
                  NastyDriveType := STR_09;
              end;
              DRIVE_REMOTE : begin
                if KeyOptions.OpenNetworkReadOnly then
                  NastyDriveType := STR_10;
              end;
              DRIVE_CDROM : begin
                NastyDriveType := STR_11;
              end;
              DRIVE_RAMDISK : begin
                NastyDriveType := STR_12;
              end;
            end;
            if ( NastyDriveType <> '' ) then
            begin
              KntFile.ReadOnly := true;
              if KeyOptions.OpenReadOnlyWarn then
                PopupMessage(Format(STR_13, [ExtractFilename(KntFile.FileName), NastyDriveType, ExtractFileDrive(KntFile.FileName)]), mtInformation, [mbOK], 0);
            end;

            LastEditCmd := ecNone;
            MMEditRepeat.Enabled := false;
            RTFMRepeatCmd.Enabled := false;
            TB_Repeat.Enabled := false;

            // LoadDefaults;
            LoadTabImages( false );

            Log_StoreTick( 'CreateVCLControls - BEGIN', 1, +1 );
            CreateVCLControls;
            Log_StoreTick( 'CreateVCLControls - END', 1, -1 );

            KntFile.SetupMirrorNodes(nil);
            Log_StoreTick( 'After SetupMirrorNodes', 1 );

            SetupAndShowVCLControls;
            Log_StoreTick( 'After SetupAndShowVCLControls', 1 );


            opensuccess := true;

            StatusBar.Panels[PANEL_HINT].Text := STR_14;

            try

              if ClipOptions.Recall then
              begin
                if assigned( KntFile.ClipCapFolder ) then begin
                  ToggleClipCap( true, KntFile.ClipCapFolder );
                  Log_StoreTick( 'After recall ClipCapFolder', 1 );
                end;
              end
              else
              begin
                KntFile.ClipCapFolder := nil;
              end;

              LoadTrayIcon( assigned( KntFile.ClipCapFolder ) and ClipOptions.SwitchIcon );

            except
            end;

          except
            on E : Exception do
            begin
              opensuccess := false;
              StatusBar.Panels[PANEL_HINT].Text := STR_15;
             {$IFDEF KNT_DEBUG}
              Log.Add( 'Error while opening file: ' + E.Message );
             {$ENDIF}
              if E.Message <> '' then
                 PopupMessage( E.Message, mtError, [mbOK,mbHelp], _HLP_KNTFILES );
              if assigned( KntFile ) then
              begin
                try
                  DestroyVCLControls;
                except
                end;
                KntFile.Free;
                KntFile := nil;
              end;
              result := 1;
            end;
          end;

          try
            if opensuccess then
            begin
              GetFileState( KntFile.FileName, FileState );
              FPath := extractfilepath( KntFile.FileName );
              FolderMon.FolderName := copy( FPath, 1, pred( length( FPath )));
              // prevent folder monitor if file resides on a read-only medium
              // diskette or network
              FolderMon.Active := (( not KeyOptions.DisableFileMon ) and ( NastyDriveType = '' ));
            end;
          except
            on E : Exception do
            begin
             {$IFDEF KNT_DEBUG}
               Log.Add( 'Folder monitor error: ' + E.Message );
             {$ENDIF}
               PopupMessage( STR_16 + E.Message, mtError, [mbOK], 0 );
            end;
          end;

        finally
         {$IFDEF KNT_DEBUG}
          Log.Add( 'KntFileOpen result: ' + inttostr( result ), 0);
         {$ENDIF}

          if opensuccess then
          begin
            if ( Pages.PageCount > 0 ) then
            begin
              ActiveKntFolder := TKntFolder(Pages.ActivePage.PrimaryObject);
              TAM_ActiveName.Caption := ActiveKntFolder.Name;
              FocusActiveKntFolder;
              Log_StoreTick( 'After GetFileState and FocusActiveKntFolder', 1 );
            end
            else
            begin
              TAM_ActiveName.Caption := STR_03;
              ActiveKntFolder := nil;
            end;
            if assigned( KntFile ) then
            begin
              KntFile.ReadOnly := ( OpenReadOnly or KntFile.ReadOnly );
              KntFile.Modified := false;
            end;
            UpdateNoteDisplay;
            UpdateKntFileState( [fscOpen,fscModified] );
          end;
          Log_StoreTick( 'After UpdateNoteDisplay, UpdateFileState', 1 );
          screen.Cursor := crDefault;
          FileIsBusy := false;
          Timer.Enabled := true;
        end;


        if ( result = 0 ) then
        begin
          KeyOptions.LastFile := FN;
          if KeyOptions.MRUUse then
            MRU.AddItem( FN );
          AddToFileManager( FN, KntFile );
        end
        else
        begin
          StatusBar.Panels[PANEL_HINT].Text := Format( STR_17, [result] );
        end;

        Log_StoreTick( 'FileOpen - END', 0, -1 );
        Log_Flush;

  end;
end; // KntFileOpen



//=================================================================
// KntFileSave
//=================================================================

function ExistingBackup(const Folder, FileName: string;
                        FileModificationDate: TDateTime;
                        ConsiderAlsoModifiedLater: Boolean) : string;
var
   DirInfo : TSearchRec;
   FindResult : Integer;
   Mask : string;
   Consider: Boolean;
   FileDateTime: TDateTime;
begin
   Result := '';
   Mask := ChangeFileExt(Folder + FileName, IntervalPrefixBAK + '*' + ext_KeyNote);

   FindResult := FindFirst(Mask, faAnyFile, DirInfo);
   Consider:= False;
   while FindResult = 0 do begin
     FileDateTime:= DirInfo.TimeStamp;

     if ConsiderAlsoModifiedLater then begin
        if FileDateTime >= FileModificationDate then Consider := True;
     end
     else begin
        if FileDateTime = FileModificationDate then Consider := True;
     end;

     if Consider then begin
        Result := DirInfo.Name;
        break;
        end
     else
        FindResult := FindNext(DirInfo);
   end;
   FindClose(DirInfo);
end;


//------ DoBackup ------------------------------------------------

{ Backup BEFORE save operation }

function DoBackup(const FN: string; var BakFN: string; var BakFolder: string;
                  var SUCCESS: LongBool; var LastError: Integer): Boolean;
var
  ext, bakFNfrom, bakFNto, FileName: string;
  DayBakFN, DayBakFN_Txt: string;
  myBackupLevel, bakindex : Integer;
  FirstSaveInDay: Boolean;

begin

   // Check if backup must be done
   //--------------------

   { If Backup = True (classic, cyclic way), or
     If BackupRegularIntervals = True and it is the first time we save this day
         (there is no Interval BAK day or it corresponds to other day)

     In both cases, it won't be necessary to create a new backup if there is
     one already available. This can occur if last save did match with a
     montly or weekly backup. (A copy of the saved file was taken)
   }

   Result := True;

   case GetDriveType(PChar(ExtractFileDrive(KntFile.FileName) + '\')) of
     DRIVE_REMOVABLE: Result := (not KeyOptions.OpenFloppyReadOnly);
     DRIVE_REMOTE:    Result := (not KeyOptions.OpenNetworkReadOnly);
     0, 1,
     DRIVE_CDROM,
     DRIVE_RAMDISK:   Result := false;
   end;


   if Result then begin
      FileName:= ExtractFilename(FN);

      // Check if alternate backup directory exists
      if KeyOptions.BackupDir <> '' then begin
        if not DirectoryExists(KeyOptions.BackupDir) then begin
          DoMessageBox(
            Format(STR_20, [KeyOptions.BackupDir]),
            mtWarning, [mbOK], 0);
          KeyOptions.BackupDir := '';
        end;
      end;
      if KeyOptions.BackupDir <> '' then
         bakFolder:= ProperFolderName(KeyOptions.BackupDir)
      else
         bakFolder:= ExtractFilePath(FN);

      FirstSaveInDay:= False;

      if KeyOptions.BackupRegularIntervals then begin
         // Is it the first time we save this day? (there is no Interval BAK day or it corresponds to other day)
         FirstSaveInDay:= False;
         DayBakFN := ChangeFileExt(bakFolder + FileName, IntervalBAKDay);
         DayBakFN_Txt:= DayBakFN + '.TXT';
         if not FileExists(DayBakFN_Txt) or
            (RecodeTime(GetFileDateStamp(DayBakFN_Txt), 0,0,0,0) <> Date) then
             FirstSaveInDay:= True;
      end;

      if not (KeyOptions.Backup or
             (KeyOptions.BackupRegularIntervals and FirstSaveInDay))
         or not FileExists(FN) then
         Result := false;
   end;

   if not Result then begin      // Backup needs not be done
      BakFN:= '';
      Exit;                      // EXIT
   end;


   // Backup must be done (if not already available)
   //---------------------------

   // Backup already available?
   BakFN:= ExistingBackup(bakFolder, fileName, GetFileDateStamp(FN), False);

   if BakFN <> '' then begin
      SUCCESS:= True;
      if KeyOptions.BackupRegularIntervals and FirstSaveInDay then begin
         DeleteFile(PChar(DayBakFN));
         TFile.WriteAllText(DayBakFN_Txt, Format(STR_78, [DateToStr(Date), BakFN]));
      end;
      Exit;                      // EXIT
   end;


   if KeyOptions.BackupRegularIntervals and FirstSaveInDay then
      BakFN:= DayBakFN

   else begin
       // Necessarily KeyOptions.Backup must be True at this point

       // Adjust how backup extension is added
       bakFN := bakFolder + FileName;
       if KeyOptions.BackupAppendExt then
          bakFN := bakFN + KeyOptions.BackupExt
       else
          bakFN := ChangeFileExt(bakFN, KeyOptions.BackupExt);


       myBackupLevel := KeyOptions.BackupLevel;
       if KntFile.NoMultiBackup then
          myBackupLevel := 1;

       if myBackupLevel > 1 then begin
         // recycle bak..bakN files, discarding the file
         // with the highest number (up to .bak9)
         for bakIndex := Pred(myBackupLevel) downto 1 do begin
            bakFNfrom:= bakFN;
            if bakIndex > 1 then
               bakFNfrom := bakFN + IntToStr(bakIndex);
            bakFNto := bakFN + IntToStr(Succ(bakIndex));
            if FileExists(bakFNfrom) then
                 MoveFileExW_n(bakFNfrom, bakFNto, 3);
         end; // for
       end;
   end;

   SUCCESS := MoveFileExW_n(FN, BakFN, 3);


   if KeyOptions.BackupRegularIntervals and FirstSaveInDay then
      TFile.WriteAllText(DayBakFN_Txt, Format(STR_78, [DateToStr(Date), BakFN]));

   if not SUCCESS then begin
      bakFN:= '';
      LastError := GetLastError;
     {$IFDEF KNT_DEBUG}
      Log.Add('Backup failed; code ' + IntToStr(LastError));
     {$ENDIF}
   end;

end;


//------ DoIntervalBackup ------------------------------------------------

{ Backup AFTER save operation }

procedure DoIntervalBackup(FN : string; BakFolder: string);
var
   Year, Month, Day: Word;
   dayOfW, bakIndex: Integer;
   BeginOfWeek: TDateTime;
   FileName, BakFN: string;
   bakFNfrom, bakFNTo: string;
begin
   if not KeyOptions.BackupRegularIntervals then Exit;

   DecodeDate(Date, Year, Month, Day);
   FileName:= ExtractFilename(FN);

   BakFN := Format(
               ChangeFileExt(BakFolder + '\' + FileName,
                  IntervalPrefixBAK + '%d_%.2d' + ext_KeyNote), [Year, Month]);

   if not FileExists(BakFN) then
      CopyFile(PChar(FN), PChar(BakFN), False)   // Monthly backup

   else begin
      dayOfW:= DayOfTheWeek(Date);
      BeginOfWeek:= IncDay(Date, -dayOfW + 1);

      BakFN:= ExistingBackup(bakFolder, FileName, BeginOfWeek, True);
      if BakFN = '' then begin
         // There is no backup (weekly of monthly) for this week. We'll create
         // a weekly one, cycling the others (up to 4)
         BakFN := ChangeFileExt(BakFolder + '\' + FileName, IntervalPrefixBAK + 'W');

         for bakIndex := Pred(4) downto 1 do begin
            bakFNfrom := BakFN + IntToStr(bakIndex)       + ext_KeyNote;
            bakFNto   := BakFN + IntToStr(Succ(bakIndex)) + ext_KeyNote;
            if FileExists(bakFNfrom) then
               MoveFileEx(
                  PChar(bakFNfrom), PChar(bakFNto),
                  MOVEFILE_REPLACE_EXISTING or MOVEFILE_COPY_ALLOWED);
         end; // for
         CopyFile(PChar(FN), PChar(BakFN + '1' + ext_KeyNote), False);   // Weekly backup
      end;

   end;

end;

//------ KntFileSave --------------------------------------------

function KntFileSave(FN : string) : integer;
var
  ErrStr, ext, BakFN, BakFolder, FPath: string;
  SUCCESS: LongBool;
  i, LastError: Integer;
  myFolder: TKntFolder;
  tempDirectory, tempFN : string;
  SavedNotes, SavedNodes: integer;

  procedure RenameTempFile;
  var
     Str: string;
  begin
     if not MoveFileExW_n (tempFN, FN, 5) then begin
        Str:= STR_25 + STR_InfSaving;
        raise EKeyKntFileError.CreateFmt(Str, [FN, GetLastError, tempFN, KeyOptions.BackupDir]);
     end;
  end;


begin
  with Form_Main do begin
     result := -1;
     if not HaveKntFolders(true, false) then Exit;
     if FileIsBusy then Exit;

     if (FN <> '') and KntFile.ReadOnly then begin
         DoMessageBox(STR_77, mtWarning, [mbOK], 0);
         Exit;
     end;

     if (FN <> '') and not KntFile.Modified then begin
         StatusBar.Panels[PANEL_HINT].Text := STR_79;
         Exit;
     end;

     ErrStr := '';

     SUCCESS := LongBool(0);
     try
       try
         FileIsBusy := true;
         FolderMon.Active := false;
         if not HaveKntFolders(true, false) then exit;

         if FN <> '' then begin
            FN := NormalFN(FN);
            case KntFile.FileFormat of
               nffKeyNote: FN := ChangeFileExt(FN, ext_KeyNote);
               nffEncrypted:
                  if KeyOptions.EncFileAltExt then
                     FN := ChangeFileExt(FN, ext_Encrypted)
                  else
                     FN := Changefileext(FN, ext_KeyNote);
{$IFDEF WITH_DART}
               nffDartNotes: FN := ChangeFileExt(FN, ext_DART);
{$ENDIF}
            end;
         end;

         if FN = '' then begin                                     // Save with a new name (Save As...)
           with SaveDlg do begin
              case KntFile.FileFormat of
{$IFDEF WITH_DART}
                nffDartNotes : Filter := FILTER_DARTFILES + '|' + FILTER_ALLFILES;
{$ENDIF}
                nffKeyNote   : Filter := FILTER_NOTEFILES + '|' + FILTER_ALLFILES;
                else
                  Filter := FILTER_NOTEFILES + '|' + FILTER_DARTFILES + '|' + FILTER_ALLFILES;
              end;
              FilterIndex := 1;
              if KntFile.FileName <> '' then begin
                 FileName  := ExtractFileName(KntFile.FileName);
                 InitialDir:= ExtractFilePath(KntFile.FileName);
              end
              else begin
                 InitialDir := GetFolderPath(fpPersonal);
                 FileName := '';
              end;
           end;

           if SaveDlg.Execute then begin
              KntFile.OpenAsReadOnly := False;

              FN := NormalFN(SaveDlg.FileName);
              ext := ExtractFileExt(FN);
              if ext = '' then begin
                 case KntFile.FileFormat of
                   nffKeyNote, nffKeyNoteZip:
                     FN := FN + ext_KeyNote;
                   nffEncrypted:
                     if KeyOptions.EncFileAltExt then
                        FN := FN + ext_Encrypted
                     else
                        FN := FN + ext_KeyNote;
{$IFDEF WITH_DART}
                   nffDartNotes:
                     FN := FN + ext_DART;
{$ENDIF}
                 end;
              end;

{$IFDEF WITH_DART}
              if (KntFile.FileFormat = nffDartNotes) and KeyOptions.SaveDARTWarn then begin
                 case PopupMessage(format(STR_18, [FILE_FORMAT_NAMES[nffDartNotes], Program_Name, FILE_FORMAT_NAMES[nffKeyNote]]),
                                   mtWarning, [mbYes,mbNo,mbCancel], 0 ) of
                    mrNo: KntFile.FileFormat := nffKeyNote;
                    mrCancel: Exit;
                 end;
              end;
{$ENDIF}
           end
           else
              Exit;
         end;

         // Initialize ImagesManager to reflect the FN in case it is a new file
         if not ImagesManager.PrepareImagesStorageToSave(FN) then
            exit;


         Screen.Cursor := crHourGlass;
         StatusBar.Panels[PANEL_HINT].text := STR_19 + FN;


         {
          *1
         To optimize the saving process in conjunction with making backup copies, if the currently saved file needs
         to be kept as a backup, we will not make a copy of it, instead we will simply move it to the backup folder,
         renaming it accordingly in the process.
           But it is not interesting to do this before saving the file to its final location (FN), because the elapsed
         time may be enough for programs that listen for changes to it (eg Dropbox, if we have our file in a
         synchronized folder with this tool) detect the movement of the file and ask us if we want to delete the file...

         E.g.: Dropbox can show a modal windose like this (translated from spanish):

           Do you want to remove "....knt" from everyone's Dropbox account and all devices?
           If you move this file to <backup folder>, it will stop being shared and will not be available in Dropbox on any other device
           => Cancel / Move out of Dropbox

           In the end, no matter what we do, we will most likely end up with multiple additional files, like
           "<myfile> (1).knt", "<myfile> (2).knt", ....

         To avoid this, we are going to perform the normal saving of the file first, but on another path and with a
         temporary name (we will use the user's temp folder), and once the backup we have been doing before is finished
         (and which will have considered the version of the existing file -- which continued to be there), we will move
         the file saved with the temporary name to the final folder, and we will continue with the rest of the actions.
         This way, the time during which the file "disappears" from the folder where Dropbox (e.g.) looks will be minimal
         }

         Log_Flush;
         Log_StoreTick('');
         Log_StoreTick( 'KntFileSave (' + FN + ') - BEGIN', 0, +1);


         // Get a random temp file name. For safety, we will write data to the temp file, and only overwrite the actual keynote file
         // after the save process is complete.
         tempDirectory:= GetTempDirectory;
         tempFN := tempDirectory + RandomFileName(tempDirectory, ext_Temp, 8);


        // if file has tree notes with virtual nodes, they should be backed up as well.
        // We use ugly global vars to pass backup options
        // Note: virtual nodes files will be backed (if configured) during the process of saving
        // each folder, while all the nodes are iterated.

        _VNDoBackup     := KeyOptions.Backup and KeyOptions.BackupVNodes;
        _VNBackupExt    := KeyOptions.BackupExt;
        _VNBackupAddExt := KeyOptions.BackupAppendExt;
        _VNBackupDir    := KeyOptions.BackupDir;


        // Virtual node relative paths are calculated using as base directory the location of the .knt file
        // and it can also be necessary to search this files using this relative path; so this chDir
        // Each .knt file (copied or not) must have the relative paths of the virtual node files, updated according to their location.
        _VNKeyKntFileName := FN;
        {$I-}
        ChDir(ExtractFilePath( _VNKeyKntFileName ));
        {$I+}



         // Get (and reflect) the expanded state of all the tree nodes
         try
           for i := 1 to KntFile.NoteCount do begin
              myFolder := KntFile.Folders[pred(i)];
              GetOrSetNodeExpandState(myFolder.TV, false, false);
           end;
         except
           // nothing
         end;


         // SAVE the file (and backup virtual nodes if it applies), on a temporal location, before initiate DoBackup (see *1)
         Result := KntFile.Save(tempFN, SavedNotes, SavedNodes);

         Log_StoreTick( 'After KntFile.Save (temporal location)', 1 );

         if Result = 0 then begin
            // BACKUP (using previous file, before this saving) of the file
            if DoBackup(FN, BakFN, BakFolder, SUCCESS, LastError) then begin
               if not SUCCESS then begin
                  if MessageDlg(Format(STR_21, [LastError, SysErrorMessage(LastError), tempFN]), mtWarning, [mbYes,mbNo], 0) <> mrYes then begin
                    Result := -2;
                    Exit;
                  end;
               end;
            end;
            Log_StoreTick( 'After DoBackup', 1 );

            RenameTempFile;                 // Now rename the temp file to the actual KeyNote file name
            KntFile.FileName := FN;
            KntFile.Modified:= False;      // Must be done here, not in TNotFile.Save, and of course, never before RenameTempFile

            StatusBar.Panels[PANEL_HINT].Text := Format(STR_22, [SavedNotes, SavedNodes]);
            KntFile.ReadOnly := False;    // We can do SaveAs from a Read-Only file (*)
                 { (*) In Windows XP is possible to select (with SaveDlg) the same file
                    as destination. In W10 it isn't }

            // Backup after saving
            DoIntervalBackup(FN, BakFolder);
            Log_StoreTick( 'After DoIntervalBackup', 1 );
         end
         else begin
            // ERROR on save
            StatusBar.Panels[PANEL_HINT].Text := Format(STR_23, [Result] );
            ErrStr := Format(STR_24 + STR_InfSaving, [Result, tempDirectory, KeyOptions.BackupDir] );

            if KeyOptions.AutoSave then begin
               KeyOptions.AutoSave := False;
               ErrStr := ErrStr + STR_26;
            end;

            DoMessageBox(ErrStr, mtError, [mbOK], 0);
         end;

       except
         on E: Exception do
         begin
           {$IFDEF KNT_DEBUG}
           Log.Add('Exception in KntFileSave: ' + E.Message);
           {$ENDIF}
           StatusBar.Panels[PANEL_HINT].Text := STR_27;
           DoMessageBox(STR_28 + ExtractFileName(FN) + '": ' + #13#13 + E.Message, mtError, [mbOK], 0 );
           Result := 1;
         end;
       end;

       try // folder monitor
         if not KeyOptions.DisableFileMon then begin
            GetFileState(KntFile.FileName, FileState);
            FPath := ExtractFilePath( KntFile.FileName );
            if (Length(FPath) > 1) and (FPath[Length(FPath)] = '\') then
               Delete(FPath, Length(FPath), 1);
            FolderMon.FolderName := FPath;
            FolderMon.Active := (not KeyOptions.DisableFileMon);
         end;
       except
         on E: Exception do
         begin
           FolderMon.Active := False;
           PopupMessage(STR_29 + E.Message, mtError, [mbOK], 0);
         end;
       end;

     finally
       Screen.Cursor := crDefault;
       FileIsBusy := False;
       UpdateKntFileState([fscSave,fscModified]);
      {$IFDEF KNT_DEBUG}
       Log.Add( 'KntFileSave result: ' + IntToStr(Result));
      {$ENDIF}

       Log_StoreTick( 'KntFileSave - END', 0, -1 );
       Log_Flush;
     end;

     if Result = 0 then begin
        KeyOptions.LastFile := FN;
        if KeyOptions.MRUUse then
           MRU.AddItem(FN);
        AddToFileManager(FN, KntFile);
     end;
  end;

end; // KntFileSave


//=================================================================
// KntFileClose
//=================================================================
function KntFileClose : boolean;
begin

  if ( not Form_Main.HaveKntFolders( false, false )) then exit;
  if ( not CheckModified( not KeyOptions.AutoSave, false )) then
  begin
    result := false;
    exit;
  end;


  with Form_Main do begin

      result := true;
      DEF_FN := OrigDEF_FN;

      try
        // close all non-modal forms that might be open
        CloseNonModalDialogs;
        ClearFindAllResults;
      except
      end;

      if IsRunningMacro then
      begin
        MacroAbortRequest := true;
      end
      else
      if IsRecordingMacro then
      begin
        TB_MacroRecordClick( TB_MacroRecord );
      end;

      screen.Cursor := crHourGlass;

      try
        FileIsBusy := true;
        FolderMon.Active := false;

        ClipCapActive := false;
        Pages.MarkedPage := nil;
        if ( KntFile.ClipCapFolder <> nil ) then
        begin
          TB_ClipCap.Down := false;
          ToggleClipCap( false, KntFile.ClipCapFolder ); // turn it OFF
        end;

        LastEditCmd := ecNone;
        UpdateLastCommand( ecNone );
        BookmarkInitializeAll;
        ShowAlarmStatus;
        
        if assigned( KntFile ) then
        begin
          try
            DestroyVCLControls;
          except
            // showmessage( 'BUG: error in DestroyVCLControls' );
          end;
          try
            try
              KntFile.Free;
            except
              // showmessage( 'BUG: error in KntFile.Free' );
            end;
          finally
            KntFile := nil;
          end;
        end;

        ActiveKntFolder := nil;

        History.Clear;
        UpdateHistoryCommands;


        TAM_ActiveName.Caption := '';
        UpdateKntFileState( [fscClose,fscModified] );
        StatusBar.Panels[PANEL_HINT].Text := STR_30;
      finally
        AlarmManager.Clear;
        ImagesManager.Clear;
        MirrorNodes.Clear;
        MovingTreeNode:= nil;

        FileIsBusy := false;
        PagesChange( Form_Main );
        screen.Cursor := crDefault;
       {$IFDEF KNT_DEBUG}
        Log.Add( 'KntFileClose result: ' + BOOLARRAY[result] );
       {$ENDIF}
        LoadTrayIcon( false );
      end;
  end;
end; // KntFileClose


procedure KntFileCopy (var SavedFolders: integer; var SavedNodes: integer;
                        FN: string= '';
                        ExportingMode: boolean= false;
                        OnlyCurrentNodeAndSubtree: TTreeNTNode= nil;
                        OnlyNotHiddenNodes: boolean= false;
                        OnlyCheckedNodes: boolean= false);
var
  currentFN, newFN : string;
  cr : integer;
  oldModified : boolean;
  DirDlg : TdfsBrowseDirectoryDlg;

begin
  with Form_Main do begin

        if ( not HaveKntFolders( true, false )) then exit;

        currentFN := KntFile.FileName;
        if (FN = '') and (currentFN = '') then begin
           PopupMessage( STR_82, mtError, [mbOK], 0 );
           exit;
        end;

        DirDlg := TdfsBrowseDirectoryDlg.Create( Form_Main );

        try

          if FN = '' then begin
              DirDlg.Root := idDesktop;
              DirDlg.ShowSelectionInStatus := true;
              DirDlg.Title := STR_32;
              DirDlg.Center := true;

              if ( KeyOptions.LastCopyPath <> '' ) then
                DirDlg.Selection := KeyOptions.LastCopyPath
              else
                DirDlg.Selection := GetFolderPath( fpPersonal );

                if ( not DirDlg.Execute ) then exit;
                if ( properfoldername( extractfilepath( currentFN )) = properfoldername( DirDlg.Selection )) then
                begin
                  PopupMessage( STR_33, mtError, [mbOK], 0 );
                  exit;
                end;

                KeyOptions.LastCopyPath := properfoldername( DirDlg.Selection );

                newFN := KeyOptions.LastCopyPath + ExtractFilename( currentFN );
                if FileExists( newFN ) then
                   if ( Popupmessage( Format(STR_34, [newFN]), mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;

          end
          else
             newFN:= FN;

          StatusBar.Panels[PANEL_HINT].Text := STR_35;


          // Virtual node relative paths are calculated using as base directory the location of the .knt file
          // and it can also be necessary to search this files using this relative path; so this ChDir
          // Each .knt file (copied or not) must have the relative paths of the virtual node files, updated according to their location.
          _VNKeyKntFileName := newFN;
          {$I-}
          ChDir(ExtractFilePath( _VNKeyKntFileName ));
          {$I+}

          oldModified := KntFile.Modified;
          screen.Cursor := crHourGlass;
          try
            try
              ImagesManager.ExportingMode:= true;
              try
                 cr := KntFile.Save( newFN, SavedFolders, SavedNodes, ExportingMode, OnlyCurrentNodeAndSubtree, OnlyNotHiddenNodes, OnlyCheckedNodes);
              finally
                 ImagesManager.ExportingMode:= false;
              end;

              if ( cr = 0 ) then begin
                StatusBar.Panels[PANEL_HINT].Text := STR_36;
                PopUpMessage( STR_37 +#13 + NewFN, mtInformation, [mbOK], 0 );
              end
              else begin
                Popupmessage( STR_38 + inttostr( cr ) + ')', mtError, [mbOK], 0 );
               {$IFDEF KNT_DEBUG}
                Log.Add( 'Copying failed (' + inttostr( cr ) + ')' );
               {$ENDIF}
              end;

            except
              on E : Exception do begin
                StatusBar.Panels[PANEL_HINT].Text := STR_15;
               {$IFDEF KNT_DEBUG}
                Log.Add( 'Exception in KntFileCopy: ' + E.Message );
               {$ENDIF}
                PopupMessage( E.Message, mtError, [mbOK], 0 );
              end;
            end;

          finally
            KntFile.FileName := currentFN;       // It shouldn't be necesary, because KntFile.Save doesn't modify KntFile.FileName
            KntFile.Modified := oldModified;
            screen.Cursor := crDefault;
          end;

        finally
          DirDlg.Free;
        end;
  end;

end; // KntFileCopy


//=================================================================
// MergeFromKNTFile
//=================================================================
procedure MergeFromKNTFile( MergeFN : string );
var
  MergeFile : TKntFile;
  ImgManagerMF: TImageManager;
  LoadResult : integer;
  TabSelector : TForm_SelectTab;
  mergecnt, i, n, p : integer;
  newFolder : TKntFolder;
  mergeFolder : TKntFolder;
  newNote, mergeNote : TKntNote;
  IDs: array of TMergeFolders;
  mirrorID: string;
  FolderID: integer;

  function getNewID(FolderID: integer): integer;
  var
    i: integer;
  begin
    Result:= 0;
    for i := 0 to High(IDs) do
        if IDs[i].oldID = FolderID then begin
           Result:= IDs[i].newID;
           exit;
        end;
  end;

begin
  with Form_Main do begin

        if ( not HaveKntFolders( true, false )) then exit;
        if FileIsBusy then exit;

        if ( MergeFN = '' ) then
        begin

          if ( KeyOptions.LastExportPath <> '' ) then
            OpenDlg.InitialDir := KeyOptions.LastExportPath;
          OpenDlg.Title := STR_39;

          if ( not OpenDlg.Execute ) then exit;
          MergeFN := OpenDlg.FileName;
          KeyOptions.LastExportPath := extractfilepath( MergeFN );

        end;

        MergeFN := normalFN( MergeFN );

        { this can be safely removed. User can want to copy a whole folder,
          and this is a neat way to do that.
        if ( MergeFN = KntFile.FileName ) then
        begin
          showmessage( Format( 'Cannot merge a file with itself (%s)', [ExtractFilename( MergeFN )] ));
          exit;
        end;
        }

        MergeFile := TKntFile.Create;
        MergeFile.PassphraseFunc := GetFilePassphrase;
        mergecnt := 0;

        ImgManagerMF:= TImageManager.Create;
        ImagesManager.ExternalImagesManager:= ImgManagerMF;

        try
          try
            LoadResult := MergeFile.Load( MergeFN, ImgManagerMF);
            if ( LoadResult <> 0 ) then
            begin
              messagedlg( STR_40, mtError, [mbOK], 0 );
              exit;
            end;

            if ( MergeFile.NoteCount = 0 ) then
            begin
              messagedlg( STR_41, mtInformation, [mbOK], 0 );
              exit;
            end;

            for i := 0 to pred( MergeFile.NoteCount ) do
            begin
              // initially, UNMARK ALL notes (i.e. no merging)
              MergeFile.Folders[i].Info := 0;
            end;

          except
            on E : Exception do
            begin
              messagedlg( STR_42 + E.Message, mtError, [mbOK], 0 );
              exit;
            end;
          end;

          TabSelector := TForm_SelectTab.Create( Form_Main );
          try
            TabSelector.myKntFile := MergeFile;
            TabSelector.Caption := Format( STR_43, [ExtractFilename( MergeFile.FileName )] );
            if ( not ( TabSelector.ShowModal = mrOK )) then exit;
          finally
            TabSelector.Free;
          end;

          SetLength(IDs, MergeFile.NoteCount);
          for i := 0 to pred( MergeFile.NoteCount ) do
          begin
            // see if user selected ANY notes for merge
            if ( MergeFile.Folders[i].Info > 0 ) then
              inc( mergecnt );
          end;

          if ( mergecnt = 0 ) then
          begin
            messagedlg( STR_44, mtInformation, [mbOK], 0 );
            exit;
          end;
          mergecnt := 0;

          StatusBar.Panels[PANEL_HINT].Text := STR_45;

          screen.Cursor := crHourGlass;

          try
            for i := 0 to pred( MergeFile.NoteCount ) do
            begin

              IDs[i].oldID:= MergeFile.Folders[i].ID;
              if ( MergeFile.Folders[i].Info = 0 ) then begin
                 IDs[i].newFolder:= false;
                 if MergeFN <> KntFile.FileName then
                    IDs[i].newID:= 0
                 else
                    IDs[i].newID:= IDs[i].oldID;
                  continue;
              end;


              newFolder := TKntFolder.Create;

              newFolder.Visible := true;
              newFolder.Modified := false;

              with MergeFile.Folders[i] do
              begin
                newFolder.EditorChrome := EditorCHrome;
                newFolder.Name := Name;
                newFolder.ImageIndex := ImageIndex;
                newFolder.ReadOnly := ReadOnly;
                newFolder.DateCreated := DateCreated;
                newFolder.WordWrap := WordWrap;
                newFolder.URLDetect := URLDetect;
                newFolder.TabSize := TabSize;
                newFolder.UseTabChar := UseTabChar;
              end;

              with MergeFile.Folders[i] do begin
                 newFolder.IconKind := IconKind;
                 newFolder.TreeWidth := TreeWidth;
                 newFolder.Checkboxes := CheckBoxes;
                 newFolder.TreeChrome := TreeChrome;
                 newFolder.DefaultNodeName := DefaultNodeName;
                 newFolder.AutoNumberNodes := AutoNumberNodes;
                 newFolder.VerticalLayout := VerticalLayout;
                 newFolder.HideCheckedNodes := HideCheckedNodes;
              end;

              MergeFile.Folders[i].AddProcessedAlarmsOfNote(newFolder);

              mergeFolder:= MergeFile.Folders[i];
              if ( mergeFolder.NodeCount > 0 ) then
              begin
                for n := 0 to pred( mergeFolder.NodeCount ) do
                begin
                  mergeNote:= mergeFolder.Nodes[n];
                  newNote := TKntNote.Create;
                  newNote.Assign( mergeNote );
                  newFolder.AddNode( newNote );
                  newNote.ForceID( mergeNote.ID);
                  mergeFolder.AddProcessedAlarmsOfNode(mergeNote, newFolder, newNote);
                end;
              end;

              if ( mergeFolder.NodeCount = 1 ) and (mergeFolder.Nodes[0].Name= mergeFolder.Name) then
                  newFolder.TreeHidden:= true;           // It was an old simple note


              KntFile.AddFolder( newFolder );
              inc( mergecnt );

              IDs[i].newID:= newFolder.ID;
              IDs[i].newFolder:= true;

              try
                CreateVCLControlsForFolder( newFolder );
                if ( MergeFN = KntFile.FileName ) then begin
                   KntFile.UpdateImagesCountReferences(newFolder);
                   newFolder.DataStreamToEditor;
                end
                else
                  { We have previously assigned "ImagesManager.ExtenalImagesManager:= ImgManagerMF", to search the Stream of the images
                    with the help of the ImageManager associated with the MergeFile file }
                  KntFile.UpdateImagesStorageModeInFile (ImagesManager.StorageMode, newFolder, false);
                  // newNote.DataStreamToEditor;     // From UpdateImagesStorageModeInFile) the call to DataStreamToEditor is ensured

                SetUpVCLControls( newFolder );
              finally
                newFolder.TabSheet.TabVisible := true; // was created hidden
              end;

            end;

            //Mirror nodes (if exists) references old Folder IDs. We must use new IDs
            for i := 0 to pred( MergeFile.NoteCount ) do
              if IDs[i].newFolder then begin
                 newFolder:= KntFile.GetFolderByID(IDs[i].newID);
                 for n := 0 to newFolder.NodeCount - 1 do begin
                    newNote:= newFolder.Nodes[n];
                    if newNote.VirtualMode = vmKNTNode then begin
                       mirrorID:= newNote.MirrorNodeID;
                       p := pos( KNTLINK_SEPARATOR, mirrorID );
                       FolderID:= StrToInt(AnsiLeftStr(mirrorID, p-1));
                       FolderID:= GetNewID(FolderID);
                       newNote.MirrorNodeID:= IntToStr(FolderID) + KNTLINK_SEPARATOR + AnsiMidStr (mirrorID, p+1,255);
                    end;
                 end;
              end;

            for i := 0 to pred( MergeFile.NoteCount ) do
                if IDs[i].newFolder then begin
                   newFolder:= KntFile.GetFolderByID(IDs[i].newID);
                   KntFile.SetupMirrorNodes(newFolder);
                end;


          except
            On E : Exception do
            begin
              messagedlg( STR_46 + E.Message, mtError, [mbOK], 0 );
              exit;
            end;
          end;

        finally
          MergeFile.Free;
          ImagesManager.ExternalImagesManager:= nil;
          ImgManagerMF.Free;
          PagesChange( Form_Main );
          screen.Cursor := crDefault;
          KntFile.Modified := true;
          UpdateKntFileState( [fscModified] );
          if ( mergecnt > 0 ) then
            StatusBar.Panels[PANEL_HINT].Text := Format( STR_47, [mergecnt, ExtractFilename( MergeFN )] )
          else
            StatusBar.Panels[PANEL_HINT].Text := STR_48;
        end;
  end;

end; // MergeFromKNTFile


//=================================================================
// SomeoneChangedOurFile
//=================================================================
procedure SomeoneChangedOurFile;
begin
  Application.BringToFront;
  Form_Main.FolderMon.Active := false;
  try
    case DoMessageBox( Format(STR_49, [FileState.Name]), mtWarning, [mbYes,mbNo], 0 ) of
      mrYes : begin
        KntFile.Modified := false;
        KntFileOpen( KntFile.FileName );
      end;
      mrNo : begin
        KntFile.Modified := true;
      end;
    end;
  finally
    Form_Main.FolderMon.Active := ( not KeyOptions.DisableFileMon );
  end;
end; // SomeoneChangedOurFile;



//=================================================================
// CheckFolder
//=================================================================
function CheckFolder( const name, folder : string; const AttemptCreate, Prompt : boolean ) : boolean;
begin
  result := false;
  if directoryexists( folder ) then begin
    result := true;
    exit;
  end;
  if ( not AttemptCreate ) then begin
    if Prompt then
      DoMessageBox( Format(STR_50, [name,folder]), mtError, [mbOK], 0 );
    exit;
  end;

  if Prompt then begin
    if DoMessageBox( Format(STR_50 + STR_51, [name,folder]), mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes then
      exit;
  end;

  try
    mkdir( folder );
    result := true;
  except
    on e : exception do begin
      result := false;
      if Prompt then
        messagedlg( Format(STR_52, [E.Message] ), mtError, [mbOK], 0 );
    end;
  end;

end; // CheckFolder

//=================================================================
// FolderChanged
//=================================================================
procedure FolderChanged;
var
  NewState : TFileState;
  s : string;
  Changed : boolean;
begin
  with Form_Main do begin
        if FileIsBusy then exit;
        if ( not HaveKntFolders( false, false )) then exit;
        if ( FileState.Name = '' ) then exit;
        Changed := false;
        s := '';
        GetFileState( FileState.Name, NewState );
        if ( NewState.Size < 0 ) then
        begin
          // means file does not exist (deleted or renamed)
          KntFile.Modified := true; // so that we save it
          exit;
        end;
        if ( FileState.Time <> NewState.Time ) then
        begin
          Changed := true;
          s := 'time stamp';
        end
        else
        begin
          if ( FileState.Size <> NewState.Size ) then
          begin
            Changed := true;
            s := 'file size';
          end;
        end;

        if Changed then
        begin
          FileChangedOnDisk := true;
          StatusBar.Panels[PANEL_HINT].Text := STR_53;
         {$IFDEF KNT_DEBUG}
          Log.Add( 'FileChangedOnDisk: ' + s );
         {$ENDIF}
          GetFileState( FileState.Name, FileState );
        end;
  end;

end; // FolderChanged


//=================================================================
// CheckModified
//=================================================================
function CheckModified( const Warn : boolean; const closing: boolean ) : boolean;
var
   wasMinimized: boolean;
begin
  wasMinimized:= False;
  with Form_Main do begin

      result := true;
      try
        if ( not HaveKntFolders( false, true )) then exit;
       {$IFDEF KNT_DEBUG}
        Log.Add( 'CheckModified: KntFile modified? ' + BOOLARRAY[KntFile.Modified], 1 );
       {$ENDIF}
        if ( not KntFile.Modified ) then exit;
        if Warn then
        begin
          case messagedlg( STR_54, mtConfirmation, [mbYes,mbNo,mbCancel], 0 ) of
            mrYes : begin
              // fall through and save file
            end;
            mrNo : begin
              result := true;
              exit;
            end;
            mrCancel : begin
              result := false;
              exit;
            end;
          end;
        end;
        if closing then begin
           wasMinimized:= (WindowState = wsMinimized);
           Application.Minimize;  // Liberate the screen to let the user do other things while keyNote is closing
        end;

       {$IFDEF KNT_DEBUG}
        Log.Add( '-- Saving on CHECKMODIFIED', 1 );
       {$ENDIF}
        if ( KntFileSave( KntFile.FileName ) = 0 ) then
          result := true
        else
          result := ( Application.MessageBox( PChar(STR_55), PChar(STR_56), MB_YESNO+MB_ICONEXCLAMATION+MB_DEFBUTTON2+MB_APPLMODAL) = ID_YES );

        if closing and not wasMinimized then begin
           Application.Restore;
        end;

      finally
        {$IFDEF KNT_DEBUG}
         Log.Add( 'CheckModified result: ' + BOOLARRAY[result], 1 );
        {$ENDIF}
      end;
  end;

end; // CheckModified


//=================================================================
// ImportFiles
//=================================================================
procedure ImportFiles;
var
  oldFilter : string;
  FilesToImport : TStringList;
begin
  with Form_Main do begin
      if ( not HaveKntFolders( true, true )) then exit;

      FilesToImport := TStringList.Create;

      try

        with OpenDlg do
        begin
          oldFilter := Filter;
          Filter := FILTER_IMPORT;
          FilterIndex := LastImportFilter;
          Title := STR_57;
          Options := Options + [ofAllowMultiSelect];
          OpenDlg.FileName := '';
          if ( KeyOptions.LastImportPath <> '' ) then
            InitialDir := KeyOptions.LastImportPath
          else
            InitialDir := GetFolderPath( fpPersonal );
        end;

        try
          if ( not OpenDlg.Execute ) then exit;
          KeyOptions.LastImportPath := properfoldername( extractfilepath( OpenDlg.FileName ));
        finally
          OpenDlg.Filter := oldFilter;
          OpenDlg.FilterIndex := 1;
          OpenDlg.Options := OpenDlg.Options - [ofAllowMultiSelect];
          LastImportFilter := OpenDlg.FilterIndex;
        end;

        FilesToImport.AddStrings( OpenDlg.Files );
        FileDropped( nil, FilesToImport );

      finally
        FilesToImport.Free;
      end;
  end;
end; // ImportFiles


//=================================================================
// ImportAsNotes
//=================================================================


function GetImportFileType(FN: string; var ImportFileType : TImportFileType; AllowTreePad: boolean): boolean;
var
  ext: string;
begin
  Result:= True;

  with Form_Main do begin
      ext := AnsiLowerCase( ExtractFileExt( FN ));
      ImportFileType := itText;

      if ext = ext_TXT then
        ImportFileType := itText
      else
      if ext = ext_RTF then
        ImportFileType := itRTF
      else
      if ExtIsHTML( ext ) then begin
        if ( KeyOptions.HTMLImportMethod = htmlSource ) then
          ImportFileType := itText
        else
          ImportFileType := itHTML;
      end
      else
      if AllowTreePad and (ext = ext_TreePad) then
        ImportFileType := itTreePad
      else
      if ExtIsText( ext ) then
        ImportFileType := itText

      else
      if ExtIsImage( ext ) then
        ImportFileType := itImage

      else begin
          if DirectoryExists( FN ) then begin
             DoMessageBox( Format( STR_65, [FN] ), mtWarning, [mbOk], 0 );
             Result:= False;
          end
          else
            case DoMessageBox( Format(STR_58, [ExtractFilename( FN )]), mtWarning, [mbYes,mbNo], 0 ) of
              mrYes : ImportFileType := itText;
            else
              Result:= False;
            end;
      end;
  end;

end;

procedure ImportAsKntFolders( ImportFileList : TStringList; ImgLinkMode: boolean );
var
  FN, s : string;
  myFolder : TKntFolder;
  filecnt : integer;
  ImportFileType : TImportFileType;
  OutStream: TMemoryStream;

  myNote : TKntNote;
  myTreeNode : TTreeNTNode;

begin

  with Form_Main do begin

        if ( not HaveKntFolders( true, false )) then exit;
        if (( not assigned( ImportFileList )) or ( ImportFileList.Count = 0 )) then exit;

        OutStream:= TMemoryStream.Create;
        try

          for filecnt := 0 to pred( ImportFileList.Count ) do begin
            FN := normalFN( ImportFileList[filecnt] );
            OutStream.Clear;

            if not GetImportFileType(FN, ImportFileType, true) then
               continue;


            myFolder := nil;
            screen.Cursor := crHourGlass;

            try

              StatusBar.Panels[PANEL_HINT].Text := STR_59 + ExtractFilename( FN );

              if ( ImportFileType = itHTML ) then   // first see if we can do the conversion, before we create a new folder for the file
              begin
                if not ConvertHTMLToRTF( FN, OutStream) then begin
                   DoMessageBox( Format(STR_60, [FN]), mtWarning, [mbOK], 0 );
                   exit;
                end;
              end;

              try
                myFolder := TKntFolder.Create;
                myFolder.SetEditorProperties( DefaultEditorProperties );
                myFolder.SetTabProperties( DefaultTabProperties );
                myFolder.EditorChrome := DefaultEditorChrome;
                if KeyOptions.ImportFileNamesWithExt then
                  s := ExtractFilename( FN )
                else
                  s := ExtractFilenameNoExt( FN );
                myFolder.Name := s;
                KntFile.AddFolder( myFolder );

                try
                  myNote:= nil;
                  if ImportFileType <> itTreePad then begin
                     myNote := TKntNote.Create;
                     myFolder.AddNode(myNote);
                     myNote.Name := s;
                     myFolder.TreeHidden:= true;
                  end;

                  case ImportFileType of
                    itText, itRTF : begin
                     {$IFDEF KNT_DEBUG}Log.Add('Import As Folder. (TXT or RTF)  FN:' + FN,  1 ); {$ENDIF}
                      LoadTxtOrRTFFromFile(myNote.Stream, FN);
                      end;
                    itHTML : begin
                     {$IFDEF KNT_DEBUG}Log.Add('Import As Folder. (HTML)  FN:' + FN,  1 ); {$ENDIF}
                      myNote.Stream.LoadFromStream(OutStream);
                      myNote.Stream.Position:= myNote.Stream.Size;
                      myNote.Stream.Write(AnsiString(#13#10#0), 3);
                      end;
                    itTreePad : begin
                     {$IFDEF KNT_DEBUG}Log.Add('Import As Folder. (TreePad)  FN:' + FN,  1 ); {$ENDIF}
                      myFolder.SetTreeProperties( DefaultTreeProperties );
                      myFolder.TreeChrome := DefaultTreeChrome;
                      myFolder.LoadFromTreePadFile( FN );
                      end;
                    end;


                  CreateVCLControlsForFolder( myFolder );
                  myFolder.DataStreamToEditor;
                  SetUpVCLControls( myFolder );

                  var Owned: boolean:= not ImgLinkMode;
                  if ImportFileType = itImage then
                     ImagesManager.InsertImage(FN, myFolder, Owned);

                finally
                  if assigned( myFolder.TabSheet ) then  begin
                    myFolder.TabSheet.TabVisible := true; // was created hidden
                    Pages.ActivePage := myFolder.TabSheet;
                  end;

                 { ---  Important: See comment * 1 in FileDropped
                    Was corrected in Commit ac672bb258... 11/06/23
                     * Fixed: Ensure that all nodes in a RTF folder are saved in RTF format,
                       and all nodes in a plain text only folder are saved in plain format.
                    but I forgot to apply it also for files imported as notes, not as nodes !!
                 }
                 if myFolder.PlainText or  ((myNote <> nil) and not NodeStreamIsRTF (myNote.Stream)) then
                    myFolder.Editor.Modified := True
                 else
                    myFolder.Editor.Modified := False;
                 //-------

                  ActiveKntFolder := myFolder;
                end;

              except
                on E : Exception do begin
                  DoMessageBox( STR_61 + FN + #13#13 + E.Message, mtError, [mbOK], 0 );
                  exit;
                end;
              end;

            finally
              screen.Cursor := crDefault;
              AddToFileManager( KntFile.FileName, KntFile ); // update manager (number of notes has changed)
              PagesChange( Form_Main );
              StatusBar.Panels[PANEL_HINT].text := STR_62;
              KntFile.Modified := true;
              UpdateKntFileState( [fscModified] );
            end;
          end;

        finally
            if assigned( OutStream ) then OutStream.Free;
            FreeConvertLibrary;
        end;
  end;
end; // ImportAsNotes



//=================================================================
// InsertContent
//=================================================================
procedure InsertContent( ImportFileList : TStringList; ImgLinkMode: boolean; const NameProposed: string = '' );
var
  FN, strContent : string;
  myFolder : TKntFolder;
  Editor: TRxRichEdit;
  filecnt : integer;
  ImportFileType : TImportFileType;
  Stream: TMemoryStream;
  InformedImgInPlain: boolean;

begin
 { *1
  With the control made from TForm_Main.RxRTFKeyDown (Left cursor) it does not seem possible to place the cursor between a hidden mark and the text or
  image it accompanies. Except if we use Drag and Drop, where it does seem to allow placing the cursor between the hidden mark and the item.
   => In most cases we only call CheckToSelectImageHiddenMark if there is selected text.
      Here we must call CheckToMoveLefOftHiddenMark if there is not one
 }

  with Form_Main do begin

        if not assigned(ActiveKntFolder.Editor) then exit;
        if (( not assigned( ImportFileList )) or ( ImportFileList.Count = 0 )) then exit;

        Stream:= TMemoryStream.Create;
        try
          if ActiveKntFolder.Editor.SelLength > 0 then
             CheckToSelectLeftImageHiddenMark (ActiveKntFolder.Editor)
          else
             CheckToMoveLefOftHiddenMark (ActiveKntFolder.Editor);       // *1

          InformedImgInPlain:= false;

          for filecnt := 0 to pred( ImportFileList.Count ) do begin
            FN := normalFN( ImportFileList[filecnt] );
            Stream.Clear;

            if not GetImportFileType(FN, ImportFileType, false) then
               continue;


            myFolder := nil;
            screen.Cursor := crHourGlass;

            try

              StatusBar.Panels[PANEL_HINT].Text := STR_59 + ExtractFilename( FN );

              try
                myFolder:= ActiveKntFolder;
                Editor:= myFolder.Editor;

                if ImportFileType <> itImage then begin
                   strContent:= ReadAllText(FN);           // gf_streams
                   if ImportFileList.Count > 1 then
                      Editor.AddText (FN + ' - - - - - -' + #13);
                end;

                case ImportFileType of
                  itText : begin
                   {$IFDEF KNT_DEBUG}Log.Add('Insert content (TXT)  FN:' + FN,  1 ); {$ENDIF}
                    Editor.AddText (StrContent);
                    end;
                  itHTML : begin
                   {$IFDEF KNT_DEBUG}Log.Add('Insert content (HTML)  FN:' + FN,  1 ); {$ENDIF}
                     if not ConvertHTMLToRTF( FN, Stream) then begin
                       DoMessageBox( Format(STR_60, [FN]), mtWarning, [mbOK], 0 );
                       exit;
                     end
                     else begin
                       strContent:= MemoryStreamToString(Stream);
                       Editor.SelText:= StrContent;
                       Editor.AddText (StrContent);
                     end;
                    end;
                  itRTF : begin
                   {$IFDEF KNT_DEBUG}Log.Add('Insert content (RTF)  FN:' + FN,  1 ); {$ENDIF}
                    ActiveKntFolder.Editor.PutRtfText(StrContent, true);
                    end;
                  itImage: begin
                     {$IFDEF KNT_DEBUG}Log.Add('Insert content (Image)  FN:' + FN,  1 ); {$ENDIF}
                     var Owned: boolean:= not ImgLinkMode;
                     if not myFolder.PlainText then
                        ImagesManager.InsertImage(FN, myFolder, Owned, NameProposed)
                     else begin
                         if not InformedImgInPlain then begin
                            DoMessageBox( Format(STR_81, [FN]), mtWarning, [mbOK], 0 );
                            InformedImgInPlain:= True;
                         end;
                         continue;
                     end;
                   end;

                end;
                if (ImportFileList.Count > 1) then begin
                   if (ImportFileType <> itImage) then
                      Editor.AddText (FN + ' - - - - - -' + #13#13)
                   else
                      Editor.AddText (#13#13);
                end;

              except
                on E : Exception do begin
                  DoMessageBox( STR_61 + FN + #13#13 + E.Message, mtError, [mbOK], 0 );
                  exit;
                end;
              end;

            finally
              screen.Cursor := crDefault;
              StatusBar.Panels[PANEL_HINT].text := STR_62;
              KntFile.Modified := true;
              UpdateKntFileState( [fscModified] );
            end;

          end;

        finally
            Stream.Free;
            FreeConvertLibrary;
        end;
  end;
end; // InsertContent



//=================================================================
// PromptForFileAction
//=================================================================
function PromptForFileAction( const FileList : TStringList; const aExt : string; var ImgLinkMode: boolean; var NewFileName: string; var RelativeLink: boolean) : TDropFileAction;
var
  Form_DropFile: TForm_DropFile;
  LastFact, fact : TDropFileAction;
  facts : TDropFileActions;
  actidx : integer;
  actionname : string;
  FileIsHTML, FileIsImage, ActiveNoteIsReadOnly : boolean;
  myTreeNode : TTreeNTNode;
  IsKnownFileFormat : boolean;
  i, iSelected: integer;
  FileCnt, j: integer;
begin

  FileCnt:= FileList.Count;

  with Form_Main do begin
        if (( aExt = ext_Plugin ) or ( aExt = ext_Macro )) then begin
          result := factExecute;
          exit;
        end;

        result := factUnknown;
        LastFact := FactUnknown;
        ActiveNoteIsReadOnly := NoteIsReadOnly( ActiveKntFolder, false );

        FileIsHTML  := ExtIsHTML( aExt );
        FileIsImage := ExtIsImage(aExt);

        IsKnownFileFormat := ( FileIsHTML or ExtIsText( aExt ) or ExtIsRTF( aExt ) or FileIsImage);

          // Select actions which can be performed depending on extension of the dropped file
          for fact := low( fact ) to high( fact ) do
             facts[fact] := false;

          facts[factHyperlink] := ( not ActiveNoteIsReadOnly ); // this action can always be perfomed unless current folder is read-only

          // .KNT, .KNE and DartNotes files can only be opened or merged, regardless of where they were dropped. This can only be done one file at a time.
          if ( aExt = ext_KeyNote ) or
             ( aExt = ext_Encrypted ) or
             ( aExt = ext_DART ) then
          begin
            facts[factOpen] := true;
            facts[factMerge] := ( not ActiveNoteIsReadOnly );
          end
          else
          if ( aExt = ext_TreePad ) then
             facts[factImport] := true

          else begin
            // all other files we can attempt to import...
            facts[factImport] := IsKnownFileFormat;
            facts[factInsertContent]:= not ActiveNoteIsReadOnly and (not FileIsImage or NoteSupportsRegisteredImages());
            if ( not ActiveNoteIsReadOnly) then begin
              myTreeNode := ActiveKntFolder.TV.Selected;
              if assigned( myTreeNode ) then begin
                 facts[factImportAsNode] := IsKnownFileFormat;
                 facts[factMakeVirtualNode] := IsKnownFileFormat;
                 {$IFDEF WITH_IE}
                 facts[factMakeVirtualIENode] := FileIsHTML;
                 {$ENDIF}
              end;
            end;
          end;


          if (( LastFact = factUnknown ) or ( not facts[LastFact] )) then begin
            Form_DropFile := TForm_DropFile.Create( Form_Main );

            try
              Form_DropFile.Btn_HTML.Enabled := FileIsHTML;
              Form_DropFile.Btn_HTML.Visible := FileIsHTML;
              if FileIsHTML then
                Form_DropFile.RG_HTML.ItemIndex := ord( KeyOptions.HTMLImportMethod );

              i:= 0;
              iSelected:= 0;
              Form_DropFile.chk_ImageLinkMode.Visible:= false;
              Form_DropFile.chk_ImageLinkMode.Checked := false;
              for fact := low( fact ) to high( fact ) do begin
                if facts[fact] then begin
                   Form_DropFile.RG_Action.Items.Add( FactStrings[fact] );
                   if FileIsImage and (fact = factInsertContent) then begin
                      iSelected:= i;
                      Form_DropFile.chk_ImageLinkMode.Visible:= true;
                      Form_DropFile.chk_ImageLinkMode.Checked := KeyOptions.ImgDefaultLinkMode;
                      if (FileCnt = 1) then begin
                         NewFileName:= ExtractFileName(FileList[0]);
                         if not ImagesManager.CheckUniqueName(NewFileName) then begin
                            Form_DropFile.ShowNewName:= true;
                            Form_DropFile.txtImgNewName.Text:= NewFileName;
                         end;
                      end
                      else begin
                         for j:= 0 to FileList.Count-1 do begin
                             NewFileName:= ExtractFileName(FileList[j]);
                             if not ImagesManager.CheckUniqueName(NewFileName) then begin
                                Form_DropFile.ShowWarningRenamedNames:= true;
                                break;
                             end;
                         end;
                      end;
                   end;
                   Inc(i);
                end;
              end;


              if ( Form_DropFile.RG_Action.Items.Count > 0 ) then begin
                Form_DropFile.RG_Action.ItemIndex := iSelected;
                Form_DropFile.NumberOfFiles := FileCnt;
                Form_DropFile.FileExt := aExt;

                case Form_DropFile.ShowModal of
                  mrOK :
                    begin
                      ImgLinkMode:= Form_DropFile.chk_ImageLinkMode.Checked;
                      RelativeLink:= Form_DropFile.chk_Relative.Checked;
                      // since we created the radio items dynamically, we can only figure out which one was selected thusly:
                      if FileIsHTML then
                         KeyOptions.HTMLImportMethod := THTMLImportMethod( Form_DropFile.RG_HTML.ItemIndex );

                      if FileIsImage and Form_DropFile.txtImgNewName.Visible then begin
                         NewFileName:= Form_DropFile.txtImgNewName.Text;
                      end;

                      actidx := Form_DropFile.RG_Action.ItemIndex;
                      LastFact := factUnknown;
                      if ( actidx >= 0 ) then begin
                        actionname := Form_DropFile.RG_Action.Items[actidx];
                        for fact := low( fact ) to high( fact ) do begin
                           if ( FactStrings[fact] = actionname ) then begin
                              LastFact := fact;
                              break;
                           end;
                        end;

                      end;
                    end; // mrOK

                  mrCancel :
                    begin
                      LastFact := factUnknown;
                      exit;
                    end;
                end;
              end
              else begin
                messagedlg( STR_63, mtError, [mbOK], 0 );
                exit;
              end;

            finally
               result := LastFact;
               Form_DropFile.Free;
            end;
          end;
  end;

end; // PromptForFileAction


//=================================================================
// ConsistentFileType
//=================================================================
function ConsistentFileType( const aList : TStringList ) : boolean;
var
  i, cnt : integer;
  ext : string;
  ift : TImportFileType;
begin
  with Form_Main do begin
        result := true;
        cnt := aList.Count;
        if ( cnt < 2 ) then exit;

        ext := extractfileext( aList[0] );
        if ExtIsRTF( ext ) then
          ift := itRTF
        else
        if ExtIsText( ext ) then
          ift := itText
        else
        if ExtisHTML( ext ) then
          ift := itHTML
        else
        if ExtIsImage( ext ) then
          ift := itImage
        else begin
          result := FilesAreOfSameType( aList );
          exit;
        end;

        for i := 1 to pred( cnt ) do begin
          ext := extractfileext( aList[i] );
          case ift of
            itRTF : if ( not ExtIsRTF( ext )) then begin
              result := false;
              break;
            end;
            itText : if ( not ExtIsText( ext )) then begin
              result := false;
              break;
            end;
            itHTML : if ( not ExtIsHTML( ext )) then begin
              result := false;
              break;
            end;
            itImage : if ( not ExtIsImage( ext )) then begin
              result := false;
              break;
            end;
          end;
        end;
  end;
end; // ConsistentFileType


//=================================================================
// FileDropped
//=================================================================
procedure FileDropped( Sender : TObject; FileList : TStringList );
var
  myTreeNode : TTreeNTNode;
  myNote : TKntNote;
  fName, fExt : string;
  myAction : TDropFileAction;
  i : integer;
  FileIsHTML, FileIsFolder : boolean;
  OutStream: TMemoryStream;
  ImgLinkMode: boolean;
  RelativeLink: boolean;
  NewFileName: string;

begin
  with Form_Main do begin
        if ( FileList.Count = 0 ) then exit;

        myAction := factUnknown;
        fName := FileList[0];
        fExt := extractfileext( fName );
        FileIsFolder := DirectoryExists( fName );
        NewFileName:= '';

        if ( not ( assigned( KntFile ) and assigned( ActiveKntFolder ))) then begin
           // no active folder; we can only OPEN a file
           if ((( fExt = ext_KeyNote ) or
              ( fExt = ext_Encrypted ) or
              ( fExt = ext_DART )) and ( not FileIsFolder )) then
             myAction := factOpen
           else begin
             HaveKntFolders( true, true );
             exit;
           end;
        end;

        myTreeNode := nil;

        WinOnTop.AlwaysOnTop := false;
        try
          Application.BringToFront;

          if ( myAction = factUnknown ) then begin

            if ( not ConsistentFileType( FileList )) then begin
              //Messagedlg( STR_64, mtError, [mbOK], 0 );
              //exit;
              fExt:= '.*';
            end;

            if FileIsFolder then begin
              myAction := factHyperlink;
              RelativeLink:= AltDown;
            end
            else
              myAction := PromptForFileAction( FileList, fExt, ImgLinkMode, NewFileName, RelativeLink);
          end;


          screen.Cursor := crHourGlass;
          try

            case myAction of
              factOpen :
                KntFileOpen( fName );

              factExecute :
                begin
                  if ( fExt = ext_Plugin ) then
                     ExecutePlugin( fName )
                  else
                  if ( fExt = ext_Macro ) then
                     ExecuteMacro( fName, '' );
                end;

              factMerge :
                MergeFromKNTFile( fName );

              factHyperlink :
                for i := 0 to pred( FileList.Count ) do begin
                  InsertFileOrLink( FileList[i], true, RelativeLink);
                  ActiveKntFolder.Editor.SelText:= #13#10;
                  ActiveKntFolder.Editor.SelLength:= 0;
                  ActiveKntFolder.Editor.SelStart:= ActiveKntFolder.Editor.SelStart+2;
                end;

              factImport :
                ImportAsKntFolders( FileList, ImgLinkMode );

              factInsertContent:
                InsertContent( FileList, ImgLinkMode, NewFileName);

              factImportAsNode :
                begin
                  ActiveKntFolder.Editor.OnChange := nil;
                  ActiveKntFolder.Editor.Lines.BeginUpdate;
                  SendMessage( ActiveKntFolder.Editor.Handle, WM_SetRedraw, 0, 0 ); // don't draw richedit yet
                  OutStream:= TMemoryStream.Create;
                  try
                    for i := 0 to pred( FileList.Count ) do begin
                      OutStream.Clear;
                      FName := FileList[i];

                      FileIsHTML := ExtIsHTML( fExt );

                      if DirectoryExists( FName ) then begin
                        if ( DoMessageBox( Format( STR_65, [FName] ), mtWarning, [mbOK,mbAbort], 0 ) = mrAbort ) then
                          exit
                        else
                          continue;
                      end;

                      {$IFDEF KNT_DEBUG}Log.Add('Import as Node: ' + FName,  1 ); {$ENDIF}

                      // first see if we can do the conversion, before we create a new folder for the file
                      if ( FileIsHTML and ( KeyOptions.HTMLImportMethod <> htmlSource )) then begin
                        if not ConvertHTMLToRTF( FName, OutStream) then begin
                           DoMessageBox( Format(STR_60, [FName]), mtWarning, [mbOK], 0 );
                           exit;
                        end;
                      end;

                      myTreeNode := TreeNoteNewNode( nil, tnAddLast, nil, '', true );
                      if assigned( myTreeNode ) then begin
                        myNote := TKntNote( myTreeNode.Data );
                        if assigned( myNote ) then begin
                            if ( FileIsHTML and ( KeyOptions.HTMLImportMethod <> htmlSource )) then begin
                              myNote.Stream.LoadFromStream(OutStream);
                              myNote.Stream.Position:= myNote.Stream.Size;
                              myNote.Stream.Write(AnsiString(#13#10#0), 3);
                            end
                            else if not ExtIsImage( fExt )  then
                              LoadTxtOrRTFFromFile(myNote.Stream, FName);

                            SelectIconForNode( myTreeNode, ActiveKntFolder.IconKind );
                            if KeyOptions.ImportFileNamesWithExt then
                              myNote.Name := ExtractFilename( FName )
                            else
                              myNote.Name := ExtractFilenameNoExt( FName );
                            myTreeNode.Text := myNote.Name;
                            ActiveKntFolder.DataStreamToEditor;
                            var Owned: boolean:= not ImgLinkMode;
                            if ExtIsImage( fExt )  then
                              ImagesManager.InsertImage(FName, ActiveKntFolder, Owned);
                        end;
                      end;
                    end;
                  finally
                    if assigned( OutStream ) then OutStream.Free;
                    FreeConvertLibrary;
                    KntFile.Modified := true;
                    SendMessage( ActiveKntFolder.Editor.Handle, WM_SetRedraw, 1, 0 ); // ok to draw now
                    ActiveKntFolder.Editor.Lines.EndUpdate;
                    ActiveKntFolder.Editor.Invalidate; // in fact, I insist on it
                    if _LastZoomValue <> 100 then
                       SetEditorZoom(ActiveKntFolder.Editor, _LastZoomValue, '' );

                    if ActiveKntFolder.TreeHidden then begin
                       ActiveKntFolder.TreeHidden:= false;
                       UpdateTreeVisible( ActiveKntFolder );
                    end;

                    UpdateKntFileState( [fscModified] );

                    // *1
                    // If myTreeNode is assigned it is because TreeNoteNewNode has returned ok.
                    // TreeNoteNewNode ends up calling TV.Items.Add, which ends up raising the TV.Change event,
                    // managed by FormMain.TVChange. The last one, if the editor has modifications, calls TKntFolder.EditorToDataStream, and
                    // the content of the editor is saved in node's stream.
                    // But, if there is an exception (or simply we exit) before TreeNoteNewNode, and enter in this finally section, we
                    // should not do Editor.Modified := False or the modifcations (existing and coming) in the Editor will be lost for the
                    // actual node.
                    // *2
                    // Also, if the new created node belongs to a normal, RTF tree, and the file we have loaded
                    // (with .LoadFromFile(FName) ) doesn't contain RTF, but ANSI or Unicode plain text, we could end up saving the node's
                    // stream content in that format to the .knt file when saving (could be problematic when reading the file).
                    // We must ensure that the node's stream is loaded with its RTF translating. If we mark the editor as modified then, when
                    // the user selects another node (os simply just before saving the .knt file), TKntFolder.EditorToDataStream will be called,
                    // and there, FEditor.Lines.SaveToStream will do that that translating. The node will contain RTF.
                    //
                    //    Similary, if the file is in RTF and the tree is plained, we should do the same. This case is less problematic,
                    // because the .knt file would be read ok, but the node could be persisted (if not modified) in an incorrect format.
                    //   Another case, that do could be problematic: if the file, not RTF, is dropped into a plained tree, and we do nothing, the
                    // node will be loaded plain (ok) in the node's stream, but with $D instead of $D$A after each line. With the last changes
                    // in TKntFolder.SaveToFile (use of new SaveRTFToFile, that doesn't rely on TStringList and it's conversions), the content
                    // will add only a ";" leading character on the first line. Instead of complicating that code, it is simple to mark this
                    // node as modified, as this will ensure that finally gets saved in the right way.
                    //  So, when dropping a file on a plained tree, we wil will mark the new node as modified. It is the more secure and simple way.
                    if assigned(myTreeNode) then begin
                       if ActiveKntFolder.PlainText or   (not NodeStreamIsRTF (myNote.Stream)) then
                           ActiveKntFolder.Editor.Modified := True                                              // *2
                       else
                           ActiveKntFolder.Editor.Modified := False;                                            // *1
                    end;
                    ActiveKntFolder.Editor.OnChange := RxRTFChange;
                  end;

                end;

              factMakeVirtualNode :
                begin
                  SendMessage( ActiveKntFolder.Editor.Handle, WM_SetRedraw, 0, 0 );
                  try
                    for i := 0 to pred( FileList.Count ) do begin
                      FName := FileList[i];
                      if DirectoryExists( FName ) then begin
                        if ( DoMessageBox( Format( STR_65, [FName] ), mtWarning, [mbOK,mbAbort], 0 ) = mrAbort ) then
                          exit
                        else
                          continue;
                      end;
                      myTreeNode := TreeNoteNewNode( nil, tnAddLast, nil, '', true );
                      VirtualNodeProc( vmNone, myTreeNode, FName );
                    end;

                  finally
                    SendMessage( ActiveKntFolder.Editor.Handle, WM_SetRedraw, 1, 0 ); // ok to draw now
                    ActiveKntFolder.Editor.Invalidate; // in fact, I insist on it
                    if _LastZoomValue <> 100 then
                       SetEditorZoom(ActiveKntFolder.Editor, _LastZoomValue, '' );
                  end;
                end;

              {$IFDEF WITH_IE}
              factMakeVirtualIENode :
                begin
                  SendMessage( ActiveKntFolder.Editor.Handle, WM_SetRedraw, 0, 0 );
                  try
                    for i := 0 to pred( FileList.Count ) do begin
                      FName := FileList[i];
                      if DirectoryExists( FName ) then begin
                        if ( DoMessageBox( Format( STR_65, [FName] ), mtWarning, [mbOK,mbAbort], 0 ) = mrAbort ) then
                          exit
                        else
                          continue;
                      end;
                      myTreeNode := TreeNoteNewNode( nil, tnAddLast, nil, '', true );
                      VirtualNodeProc( vmIELocal, myTreeNode, FName );
                    end;
                  finally
                    SendMessage( ActiveKntFolder.Editor.Handle, WM_SetRedraw, 1, 0 ); // ok to draw now
                    ActiveKntFolder.Editor.Invalidate; // in fact, I insist on it
                  end;
                end;
              {$ENDIF}

              factUnknown :
                begin
                  // MessageDlg( 'No action was taken: could not determine method for handling files.', mtWarning, [mbOK], 0 );
                  exit;
                end;

              else begin
                messagedlg( Format( STR_67, [ord( myAction )] ), mtError, [mbOK], 0 );
                exit;
              end;

            end; // case myAction

          except
            on E : Exception do begin
              messagedlg( STR_68 + E.Message, mtError, [mbOK], 0 );
              exit;
            end;
          end;

        finally
          screen.Cursor := crDefault;
          WinOnTop.AlwaysOnTop := KeyOptions.AlwaysOnTop;
        end;
  end;

end; // FileDropped


//=================================================================
// KntFileProperties
//=================================================================
procedure KntFileProperties;
var
  Form_FileInfo : TForm_KntFileInfo;
begin
  with Form_Main do begin
      // Edits properties for currently open file

      if ( not HaveKntFolders( true, false )) then exit;

      Form_FileInfo := TForm_KntFileInfo.Create( Form_Main );

      try
        Form_FileInfo.myKntFile := KntFile;

        if ( Form_FileInfo.ShowModal = mrOK ) then begin
          Virtual_UnEncrypt_Warning_Done := false;

          with Form_FileInfo do begin
            ShowHint := KeyOptions.ShowTooltips;

            KntFile.Comment := trim( Edit_Comment.Text );
            KntFile.Description := trim( Edit_Description.Text );
            KntFile.NoMultiBackup := CB_NoMultiBackup.Checked;
            KntFile.OpenAsReadOnly := CB_AsReadOnly.Checked;
            if ( not CB_AsReadOnly.Checked ) then KntFile.ReadOnly := false;
            KntFile.ShowTabIcons := CB_ShowTabIcons.Checked;
            KntFile.FileFormat := TKntFileFormat( Combo_Format.ItemIndex );
            KntFile.CompressionLevel := TZCompressionLevel( Combo_CompressLevel.ItemIndex );

            if ( CB_TrayIcon.Checked and ( Edit_TrayIcon.Text <> '' )) then
              KntFile.TrayIconFN := normalFN( Edit_TrayIcon.Text )
            else
              KntFile.TrayIconFN := '';

            if RB_TabImgDefault.Checked then
              KntFile.TabIconsFN := ''
            else
            if RB_TabImgBuiltIn.Checked then
              KntFile.TabIconsFN := _NF_Icons_BuiltIn
            else
            begin
              if ( Edit_TabImg.Text <> '' ) then
                KntFile.TabIconsFN := normalFN( Edit_TabImg.Text )
              else
                KntFile.TabIconsFN := '';
            end;

            if ( KntFile.FileFormat = nffEncrypted ) then begin
              KntFile.CryptMethod := TCryptMethod( Combo_Method.ItemIndex );
              if PassphraseChanged then
                KntFile.Passphrase := Edit_Pass.Text;
            end;

            if ( KntFile.FileName <> '' ) then
               case KntFile.FileFormat of
                 nffKeyNote : KntFile.FileName := ChangeFileExt( KntFile.FileName, ext_KeyNote );
                 nffEncrypted : if KeyOptions.EncFileAltExt then
                   KntFile.FileName := ChangeFileExt( KntFile.FileName, ext_Encrypted )
                 else
                   KntFile.FileName := ChangeFileExt( KntFile.FileName, ext_KeyNote );
   {$IFDEF WITH_DART}
                 nffDartNotes : KntFile.FileName := ChangeFileExt( KntFile.FileName, ext_DART );
   {$ENDIF}
               end;

              var NewStorageMode: TImagesStorageMode;
              NewStorageMode:= TImagesStorageMode(cbImgStorageMode.ItemIndex);
              Form_Main.MMShowImages.Enabled:= not (NewStorageMode = smEmbRTF);
              Form_Main.TB_Images.Enabled:= not (NewStorageMode = smEmbRTF);
              if NewStorageMode = smEmbRTF then
                 MMShowImages.Checked:= true;
              var ExtStorageLocation: string;
              if not ExtStorageLocationFake then
                 ExtStorageLocation:= txtExtStorageLocation.Text;

              ImagesManager.SetImagesStorage(NewStorageMode, TImagesExternalStorage(cbImgExtStorageType.ItemIndex), ExtStorageLocation,
                                             false, rbImagesStRelocate.Checked);
          end;

          KntFile.Modified := true;
          AddToFileManager( KntFile.FileName, KntFile ); // update manager (properties have changed)

          LoadTrayIcon( ClipOptions.SwitchIcon and assigned( KntFile.ClipCapFolder ));
          if _FILE_TABIMAGES_SELECTION_CHANGED then begin
            _FILE_TABIMAGES_SELECTION_CHANGED := false;
            if (( KntFile.TabIconsFN <> '' ) and ( KntFile.TabIconsFN <> _NF_Icons_BuiltIn )) then begin
              // user specified an "Other" file that does not exist.
              // This means: create this file and use it later
              // (otherwise, to use an "other" file, user would have
              // to copy the original file manually in Explorer)
              // In essense, we are creating the file the user requested.
              if ( not fileexists( KntFile.TabIconsFN )) then
                SaveCategoryBitmapsUser( KntFile.TabIconsFN );
            end;
            LoadTabImages( true );
          end;
        end;
      finally
        Form_FileInfo.Free;
      end;

      UpdateKntFileState( [fscSave,fscModified] );

      // [x] If passphrase changed or Encrypted state changed,
      // must SAVE FILE immediately.

   end;
end; // KntFileProperties


//=================================================================
// UpdateKntFileState
//=================================================================
procedure UpdateKntFileState( AState : TFileStateChangeSet );
var
  s, thisFN : string;
  NotesOK : boolean;
  WasModified : boolean;
begin
  with Form_Main do begin
      NotesOK := HaveKntFolders( false, false );
      if (( fscNew in AState ) or ( fscOpen in AState ) or ( fscSave in AState ) or ( fscClose in AState )) then
      begin
        if NotesOK then
        begin
          // Pages.OnMouseDown := TabMouseDown;
          Pages.OnDblClick := PagesDblClick;

          if ( KntFile.FileName <> '' ) then
          begin
            thisFN := ExtractFilename( KntFile.FileName );
            s := thisFN;
          end
          else
          begin
            s := STR_69;
          end;

          StatusBar.Panels.BeginUpdate;
          try
            SetFilenameInStatusbar(#32 + s + #32);
            StatusBar.Hint := #32 + KntFile.FileName;
            TrayIcon.Hint := Program_Name + ': ' + s;
            SelectStatusbarGlyph( true );
          finally
            Caption:= Format('%s  %s - %s', [Program_Name, Program_Version, s]);
            Application.Title := Format( '%s - %s', [s, Program_Name] );
            StatusBar.Panels.EndUpdate;
          end;
        end
        else
        begin
          // Pages.OnMouseDown := nil;
          Pages.OnDblClick := nil;
          StatusBar.Panels.BeginUpdate;
          try
            SetFilenameInStatusbar(STR_70);
            StatusBar.Panels[PANEL_CARETPOS].Text := '';
            StatusBar.Panels[PANEL_NOTEINFO].Text := '';
            StatusBar.Panels[PANEL_STATUS].Text := '';
            StatusBar.Panels[PANEL_FILEICON].Text := '';
            SelectStatusBarGlyph( false );

          finally
            StatusBar.Panels.EndUpdate;
          end;
          StatusBar.Hint := '';
          TrayIcon.Hint := Program_Name + STR_71;
          Caption:= Format('%s  %s -' + STR_71, [Program_Name, Program_Version]);
          TB_FileSave.Enabled := False;
          Application.Title := Program_Name;
        end;
        UpdateTabAndTreeIconsShow;
      end;

      if ( fscModified in AState ) then
      begin
        MMShiftTab_.Enabled := ( Pages.PageCount > 0 );
        if NotesOK then
        begin
          WasModified := KntFile.Modified;
          if WasModified then
             Statusbar.Panels[PANEL_STATUS].Text := STR_73      //MOD
          else
             if KeyOptions.AutoSave then
                Statusbar.Panels[PANEL_STATUS].Text := STR_72   //Auto
             else
                Statusbar.Panels[PANEL_STATUS].Text := STR_74;   //Saved
        end
        else
          Statusbar.Panels[PANEL_STATUS].Text := ' ---';
      end;
  end;

end; // UpdateKntFileState


//=================================================================
// AutoCloseKntFile
//=================================================================
procedure AutoCloseKntFile;
var
  i : integer;
begin

  // CAUTION: With KeyOptions.TimerCloseDialogs set,
  // we have a bug of major inconvenience.
  // AppLastActiveTime is NOT updated when a modal
  // dialog is open, so as long as a dialog is open,
  // KeyNote thinks it is inactive. It may lead to
  // a situation where we close file and minimize
  // while user is busy clicking stuff in a dialog box.
  // OTOH, if TimerCloseDialogs is set to false, the
  // file will not be autoclosed id any modal dialog
  // is open, leading to a potential security breach.

  if ( TransferNodes <> nil ) then
  begin
    try
      TransferNodes.Free;
    except
    end;
    TransferNodes := nil;
  end;

  if FileIsBusy then exit;
  if ( not ( KeyOptions.TimerClose and
             Form_Main.HaveKntFolders( false, false ) and
             KeyOptions.AutoSave
           )) then exit;


  // CloseNonModalDialogs;

  // Check if any modal dialog box is open, and close it, unless
  // config setting prevents us from doing so. If we cannot close
  // all modal forms, we will not auto-close the file.

  // Notes:
  // 1. We can close our own custom forms directly, by issuing
  //    a .Close to each form in Screen.Forms.
  // 2. The above won't let us close any standard Windows dialog
  //    boxes that may be open (FileOpen, FileSave, ColorDlg, etc.)
  //    For these, we can send a WM_CLOSE, but it won't work if
  //    the application (as a whole) is not active. So, once we
  //    know we are auto-closing anyway, we do the rude thing and
  //    grab focus for a short while, so that we can send WM_CLOSE
  //    to whatever dialog (belonging to us) is active. Then we
  //    minimize.

  // IsWindowEnabled( self.Handle )
  // when TRUE, we do not have any modal dialog open.
  // when FALSE, we do have one or more modal dialogs open.

  // GetActiveWindow = self.Handle
  // when TRUE, we do not have any modal dialog open, and
  //            the application is active (has focus)
  // when FALSE, we have one or more modal dialogs open,
  //             and/or the application is not active.


  if (( KntFile.FileFormat = nffEncrypted ) or ( not KeyOptions.TimerCloseEncOnly )) then
  begin
    // only under these conditions do we try to autoclose...

    // First, do our own forms
    if ( Screen.FormCount > 1 ) then
    begin
      if KeyOptions.TimerCloseDialogs then
      begin
        for i := pred( Screen.FormCount ) downto 0 do
          if ( Screen.Forms[i] <> Form_Main ) then
            Screen.Forms[i].Close;
      end
      else
        exit; // config setting prevents us from forcing
              // our forms to close, so we must bail out
    end;


    // now, if the main form is still not "enabled",
    // it means we have some system dialog open
    if ( not IsWindowEnabled( Form_Main.Handle )) then
    begin
      if KeyOptions.TimerCloseDialogs then
      begin
        // there can only be one system dialog open,
        // unlike our own forms, of which there may be a few
        // on top of one another. But first, we must be the
        // active application, otherwise we'll send WM_CLOSE
        // to nowhere.
        Application.BringToFront;
        // we KNOW we have a modal dialog open, so this is safe,
        // i.e. we won't be sending WM_CLOSE to main form
        SendMessage( GetActiveWindow, WM_CLOSE, 0, 0 ); // close the modal window!
      end
      else
        exit; // bail out, if we haven't already
    end;

    // if the file was encrypted, we optionally want to be able to
    // automatically prompt for password and reopen the file when
    // user returns to the program. So, set a flag here.
    if ( KntFile.FileFormat = nffEncrypted ) then
    begin
      _REOPEN_AUTOCLOSED_FILE := KeyOptions.TimerCloseAutoReopen;
    end;

    KntFileClose;
    Application.Minimize;
  end;

end; // AutoCloseKntFile

//=================================================================
// RunFileManager
//=================================================================
procedure RunFileManager;
var
  MGR : TForm_KntFileMgr;
  s, olds : string;
  MGROK : boolean;
begin
  try
    MGROK := false;
    s := '';
    MGR := TForm_KntFileMgr.Create( Form_Main );
    try
      with MGR do
      begin
        MgrFileName := MGR_FN;
        ShowFullPaths := KeyOptions.MgrFullPaths;
        ShowHint := KeyOptions.ShowTooltips;
        if assigned( KntFile ) then
          SelectedFileName := KntFile.FileName;
      end;
      MGROK := ( MGR.ShowModal = mrOK );
      s := MGR.SelectedFileName;
      KeyOptions.MgrFullPaths := MGR.ShowFullPaths;
    finally
      MGR.Free;
    end;

    if Form_Main.HaveKntFolders( false, false ) then
      olds := KntFile.Filename
    else
      olds := '';

    if MGROK then
    begin
      if (( s <> '' ) and ( s <> olds )) then
        KntFileOpen( s );
    end;

  except
    on E : Exception do // [xx]
    begin
      messagedlg( 'Debug message: Error in RunFileManager.', mtWarning, [mbOK], 0 );
    end;
  end;

end; // RunFileManager

function CanRegisterFileType : boolean;
begin
  result := true;
  if opt_RegExt then
    exit;
  if KeyOptions.AutoRegisterFileType then
    if ( not FiletypeIsRegistered( ext_KeyNote, _KNT_FILETYPE )) then
      if ( not IsDriveRemovable( ParamStr( 0 ))) then
        exit;
  result := false;
end; // CanRegisterFileType

procedure AssociateKeyKntFile;
begin

  if CanRegisterFileType then begin
    try
      RegisterFiletype( ext_KeyNote,   _KNT_FILETYPE, _KNT_FILETYPE, 'open', ParamStr( 0 ));
      RegisterFiletype( ext_Encrypted, _KNE_FILETYPE, _KNE_FILETYPE, 'open', ParamStr( 0 ));
      RegisterFiletype( ext_Macro,     _KNM_FILETYPE, _KNM_FILETYPE, 'open', ParamStr( 0 ));
      RegisterFiletype( ext_Macro,     _KNL_FILETYPE, _KNL_FILETYPE, 'open', ParamStr( 0 ));

      if KeyOptions.AutoRegisterPrompt then
         messagedlg( Format( STR_75, [ext_KeyNote] ), mtInformation, [mbOK], 0 );

    except
      on E : Exception do begin
        MessageDlg( STR_76 + e.Message + STR_80, mtWarning, [mbOK], 0 );
        KeyOptions.AutoRegisterFileType:= False;
      end;
    end;
  end;
end; // AssociateKeyKntFile

end.
