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
   VirtualTrees,

   kn_const,
   kn_Info,
   knt.ui.editor
   ;


    function KntFileNew( FN : string ) : integer; // create new KNT file with 1 blank folder
    function KntFileOpen( FN : string ) : integer; // open KNT file on disk
    function KntFileSave( FN : string ) : integer; // save current KNT file
    function KntFileClose : boolean; // close current KNT file

    procedure KntFileCopy (var SavedFolders: integer; var SavedNodes: integer;
                            FN: string= '';
                            ExportingMode: boolean= false;
                            OnlyCurrentNodeAndSubtree: PVirtualNode= nil;
                            OnlyNotHiddenNodes: boolean= false;
                            OnlyCheckedNodes: boolean= false);

    procedure EnsureNodeAndCaretVisibleInFolders;

    procedure MergeFromKNTFile( MergeFN : string );

    function CheckFolder( const name, folder : string; const AttemptCreate, Prompt : boolean ) : boolean;
    procedure FolderChanged;
    procedure SomeoneChangedOurFile;

    function CheckModified( const Warn : boolean; const closing: boolean ) : boolean;

    procedure ImportFiles;
    procedure ImportAsKntFolders( ImportFileList : TStringList; ImgLinkMode: boolean );
    procedure InsertContent( ImportFileList : TStringList; ImgLinkMode: boolean; const NameProposed: string = '' );

    procedure FileDropped( Sender : TObject; FileList : TStringList; Editor: TKntRichEdit = nil );
    function ConsistentFileType( const aList : TStringList ) : boolean;
    function PromptForFileAction( const FileList : TStringList; const aExt : string; var ImgLinkMode: boolean; var NewFileName: string; var RelativeLink: boolean ) : TDropFileAction;

    procedure KntFileProperties;

    procedure AutoCloseKntFile;
    procedure RunFileManager;

    function CanRegisterFileType : boolean;
    procedure AssociateKeyKntFile;


implementation

uses
   BrowseDr,
   ZLibEx,
   RxRichEd,
   gf_misc,
   gf_miscvcl,
   gf_files,
   gf_FileAssoc,
   gf_streams,
   Kn_Global,
   kn_filemgr,
   kn_KntFile,
   kn_FileInfo,
   knt.model.note,
   kn_EditorUtils,
   kn_TabSelect,
   kn_FileDropAction,
   kn_ExportImport,
   kn_Main,
   kn_Cmd,
   kn_KntFolder,
   kn_Macro,
   kn_Chest,
   kn_ConfigMng,
   kn_MacroMng,
   kn_VCLControlsMng,
   kn_BookmarksMng,
   kn_LinksMng,
   kn_PluginsMng,
   kn_ImagesMng,
   kn_FindReplaceMng,
   knt.ui.tree,
   knt.App,
   knt.RS
   ;


const
  IntervalBAKDay = '_BAK_Day';
  IntervalPrefixBAK = '_BAK@';




//=================================================================
// KntFileNew
//=================================================================
function KntFileNew( FN : string ) : integer;
var
   KntFile: TKntFile;
begin
  if assigned( ActiveFile ) then
     if (not KntFileClose ) then exit(-2);

  AlarmMng.Clear;
  ImageMng.Clear;
  TKntTreeUI.ClearGlobalData;

  with Form_Main do begin
        result := -1;
        _REOPEN_AUTOCLOSED_FILE := false;
        App.Virtual_UnEncrypt_Warning_Done:= false;
        try
          try
            result := 0;
            FolderMon.Active := false;
            Pages.MarkedPage := nil;

            if ( DEF_FN <> OrigDEF_FN ) then
            begin
              DEF_FN := OrigDEF_FN;
            end;

            LoadDefaults;
            LoadTabImages( false );

            KntFile := TKntFile.Create;
            App.FileNew(KntFile);
            KntFile.PageCtrl := Pages;
            KntFile.PassphraseFunc := GetFilePassphrase;
            KntFile.FileFormat := KeyOptions.SaveDefaultFormat;
            ImageMng.KntFile:= KntFile;
            ImageMng.SetInitialImagesStorageMode(KeyOptions.ImgDefaultStorageMode, KeyOptions.ImgDefaultExternalStorage);

            if ( KeyOptions.RunAutoMacros and fileexists( _MACRO_AUTORUN_NEW_FILE )) then begin
              Application.ProcessMessages;
              ExecuteMacro( _MACRO_AUTORUN_NEW_FILE, '' );
            end
            else
              TKntFolder.NewKntFolder( true, false );

          except
            on E : Exception do
            begin
             {$IFDEF KNT_DEBUG}
              Log.Add( 'Exception in KntFileNew: ' + E.Message );
             {$ENDIF}
              App.Popupmessage( GetRS(sFileM01) + E.Message, mtError, [mbOK] );
              result := 1;
              exit;
            end;
          end;
        finally
          LastEditCmd := ecNone;
          MMEditRepeat.Enabled := false;
          RTFMRepeatCmd.Enabled := false;
          TB_Repeat.ENabled := false;

          StatusBar.Panels[PANEL_HINT].Text := GetRS(sFileM02);
          KntFile.IsBusy := false;        // If FileIsBusy=true -> FileSetModified would be ignored
          App.FileSetModified;
          UpdateOpenFile;
          App.TagsUpdated;
        {$IFDEF KNT_DEBUG}
          Log.Add( 'KntFileNew result: ' + inttostr( result ));
        {$ENDIF}

          if ( assigned( ActiveFolder ) and KeyOptions.RunAutoMacros ) then begin
             Application.ProcessMessages;
             ExecuteMacro( _MACRO_AUTORUN_NEW_TREE, '' );
          end;

        end;

        if ( KeyOptions.AutoSave and ( not KeyOptions.SkipNewFilePrompt )) then
        begin
          if ( App.PopupMessage( GetRS(sFileM04), mtConfirmation, [mbYes,mbNo] ) = mrYes ) then
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
  opensuccess, linksModified: boolean;
  FPath, NastyDriveType : string;
  Folder: TKntFolder;
  ClipCapIdx: integer;
  Editor: TKntRichEdit;
  KntFile: TKntFile;
begin
  if assigned( ActiveFile ) then
    if ( not KntFileClose ) then exit(-2);

  with Form_Main do begin
        result := -1;
        _REOPEN_AUTOCLOSED_FILE := false;
        TKntTreeUI.ClearGlobalData;
        AlarmMng.Clear;
        OpenReadOnly := false;
        opensuccess := false;
        linksModified:= false;
        App.Virtual_UnEncrypt_Warning_Done:= false;
        KntFile := nil;

        try
          try
            FolderMon.Active := false;
            if ( FN = '' ) then begin
              with OpenDlg do begin
                Title := GetRS(sFileM05);
                Filter := FILTER_NOTEFILES {$IFDEF WITH_DART} + '|' + FILTER_DARTFILES {$ENDIF} + '|' + GetRS(FILTER_ALLFILES);
                Options := Options - [ofHideReadOnly];
                Options := Options - [ofAllowMultiSelect];
                if ( KeyOptions.LastFile <> '' ) then
                   InitialDir := ExtractFilePath( KeyOptions.LastFile )
                else
                   InitialDir := GetFolderPath( fpPersonal );
              end;

              try
                if OpenDlg.Execute then begin
                   FN := OpenDlg.FileName;
                   OpenReadOnly := ( ofReadOnly in OpenDlg.Options );
                end
                else
                  exit;

              finally
                OpenDlg.Options := OpenDlg.Options + [ofHideReadOnly];
              end;
            end;

            FN := normalFN( FN );
            if ( ExtractFileExt( FN ) = '' ) then
              FN := FN + ext_KeyNote;

            StatusBar.Panels[PANEL_HINT].Text := GetRS(sFileM06) + FN;

            Timer.Enabled := false;
            screen.Cursor := crHourGlass;
            result := 0;
            KntFile := TKntFile.Create;
            KntFile.PassphraseFunc := GetFilePassphrase;
            KntFile.PageCtrl := Pages;

            Log_Flush;
            Log_StoreTick('');
            Log_StoreTick( 'FileOpen (' + FN + ') - BEGIN', 0, +1);

            App.FileOpening(KntFile);
            result := KntFile.Load( FN, ImageMng, ClipCapIdx, true );

            Log_StoreTick( 'After parsed .knt file', 1 );

            if KntFile.OpenAsReadOnly then
               OpenReadOnly:= True;


            if ( result <> 0 ) then begin
              KntFile.ReadOnly := true;
              result := 0;
              App.WarningPopup( GetRS(sFileM07));
            end;

            if FileExists( KntFile.FileName + ext_DEFAULTS ) then
              DEF_FN := KntFile.FileName + ext_DEFAULTS
            else
              DEF_FN := OrigDEF_FN;
            LoadDefaults;

            NastyDriveType := '';
            case GetDriveType( PChar( ExtractFileDrive( KntFile.FileName ) + '\' )) of
              0, 1 : begin
                NastyDriveType := GetRS(sFileM08);
              end;
              DRIVE_REMOVABLE : begin
                if KeyOptions.OpenFloppyReadOnly then
                  NastyDriveType := GetRS(sFileM09);
              end;
              DRIVE_REMOTE : begin
                if KeyOptions.OpenNetworkReadOnly then
                  NastyDriveType := GetRS(sFileM10);
              end;
              DRIVE_CDROM : begin
                NastyDriveType := GetRS(sFileM11);
              end;
              DRIVE_RAMDISK : begin
                NastyDriveType := GetRS(sFileM12);
              end;
            end;

            if ( NastyDriveType <> '' ) then begin
              KntFile.ReadOnly := true;
              if KeyOptions.OpenReadOnlyWarn then
                App.PopupMessage(Format(GetRS(sFileM13), [ExtractFilename(KntFile.FileName), NastyDriveType, ExtractFileDrive(KntFile.FileName)]), mtInformation, [mbOK]);
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

            if App.opt_ConvKNTLinks then begin
               var GIDsNotConverted: integer:= 0;
               var FolderIDs: array of TMergeFolders;
               linksModified:= KntFile.ConvertKNTLinksToNewFormatInNotes(FolderIDs, nil, GIDsNotConverted);
               Log_StoreTick( 'After convert KntLinks to new format', 1 );
            end;

            SetupAndShowVCLControls;
            EnsureNodeAndCaretVisibleInFolders;                                // *1
            Log_StoreTick( 'After SetupAndShowVCLControls', 1 );


            opensuccess := true;

            StatusBar.Panels[PANEL_HINT].Text := GetRS(sFileM14);

            try
              with ClipCapMng do begin
                 Editor:= nil;
                 if ClipOptions.Recall and (ClipCapIdx >=0) then begin
                    if ClipCapIdx = Scratch_TabIdX then
                       Editor := Form_Main.Res_RTF
                    else begin
                       Folder:= KntFile.GetFolderByTabIndex (ClipCapIdx);
                       if not Folder.ReadOnly then
                          Editor := Folder.Editor;
                    end;
                    if assigned(Editor) then begin
                       ToggleOn(Editor);
                       Log_StoreTick( 'After recall ClipCapFolder', 1 );
                    end;
                 end;
                 LoadTrayIcon( ClipCapActive and ClipOptions.SwitchIcon, false );   // *1
              end;

            except
            end;

          except
             on E : Exception do begin
               opensuccess := false;
               StatusBar.Panels[PANEL_HINT].Text := GetRS(sFileM15);
              {$IFDEF KNT_DEBUG}
               Log.Add( 'Error while opening file: ' + E.Message );
              {$ENDIF}
               if E.Message <> '' then
                  App.PopupMessage( E.Message, mtError, [mbOK,mbHelp], def1,_HLP_KNTFILES );
               if assigned( KntFile ) then begin
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
            if opensuccess then begin
              GetFileState( KntFile.FileName, KntFile.State );
              FPath := extractfilepath( KntFile.FileName );
              FolderMon.FolderName := copy( FPath, 1, pred( length( FPath )));
              // prevent folder monitor if file resides on a read-only medium,  diskette or network
              FolderMon.Active := (( not KeyOptions.DisableFileMon ) and ( NastyDriveType = '' ));
            end;
          except
            on E : Exception do begin
             {$IFDEF KNT_DEBUG}
               Log.Add( 'Folder monitor error: ' + E.Message );
             {$ENDIF}
               App.PopupMessage( GetRS(sFileM16) + E.Message, mtError, [mbOK] );
            end;
          end;

        finally
         {$IFDEF KNT_DEBUG}
          Log.Add( 'KntFileOpen result: ' + inttostr( result ), 0);
         {$ENDIF}

         { *1
           It seems that it is only being visible that it correctly attends to the EM_SCROLLCARET message
           and updates the editor ensuring that the cursor remains visible.
           On the other hand, from the tests done it seems that it is when we establish Pages.ActivePage
           when the editor with its content becomes visible. We are interested in ensuring that when this happens,
           the editor displays the content taking into account the zoom to be applied and the possible alternative
           margins. In EnsureCaretVisibleInEditors, both the zoom and the alternative margins are set and it is asked to
           make the cursor visible.
           It is also indicated in the call to LoadTrayIcon, with the second parameter set to False, that Application.ProcessMessages
           is not called, all of this to try to ensure that the editor is displayed from the beginning with the content adapted to
           the zoom and the corresponding margins, with the cursor correctly positioned.
           Note: If this KntFileOpen ins called on initiating the application, then the above call to EnsureCaretVisibleInEditors
           and the below call to App.ActivateFolder will not ensure that the carets are visbile. For this reason there is also
           a call to these methods in Form_Main.Activate (they will run only the first time, when opening the app).
         }
          App.FileOpen(KntFile);     // KntFile can be nil. It will set ActiveFile and ActiveFileIsBusy (False)

          if opensuccess then begin
            if assigned( KntFile ) then begin
               KntFile.IsBusy := false;
               KntFile.ReadOnly := ( OpenReadOnly or KntFile.ReadOnly );
               KntFile.Modified := linksModified;
            end;
            App.ActivateFolder(nil);                   // *1  Activate (and focus) current active folder
            if ActiveEditor <> nil then
               App.ShowCurrentZoom(ActiveEditor.ZoomCurrent);       // If we don't do this and Scratchpad was visible, it will be displayed at its zoom (100)
            Log_StoreTick( 'After GetFileState and activate KntFolder', 1 );
            UpdateOpenFile;
            App.TagsUpdated;
            txtFilterTags.Text:= '';
          end;

          Log_StoreTick( 'After UpdateFolderDisplay, UpdateFileState', 1 );
          screen.Cursor := crDefault;
          Timer.Enabled := true;

          if assigned(Res_RTF) and (ImageMng.StorageMode <> smEmbRTF) then
             Res_RTF.ReconsiderImages(false, imImage);

        end;


        if ( result = 0 ) then begin
          KeyOptions.LastFile := FN;
          if KeyOptions.MRUUse then
            MRU.AddItem( FN );
          AddToFileManager( FN, KntFile );
        end
        else
          StatusBar.Panels[PANEL_HINT].Text := Format( GetRS(sFileM17), [result] );

        Log_StoreTick( 'FileOpen - END', 0, -1 );
        Log_Flush;

  end;
end; // KntFileOpen


procedure EnsureNodeAndCaretVisibleInFolders;
var
   i: integer;
   Editor: TKntRichEdit;
   Folder: TKntFolder;
begin
   if not assigned( ActiveFile ) then exit;

   for i := 0 to ActiveFile.Folders.Count -1 do begin
       Folder:= ActiveFile.Folders[i];
       Folder.TreeUI.TV.ScrollIntoView(Folder.TreeUI.FocusedNode, false);
       Editor:= Folder.Editor;
       Editor.SetMargins;
       SendMessage(Editor.Handle, EM_SCROLLCARET, 0, 0);
   end;
end;


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

   case GetDriveType(PChar(ExtractFileDrive(ActiveFile.FileName) + '\')) of
     DRIVE_REMOVABLE: Result := (not KeyOptions.OpenFloppyReadOnly);
     DRIVE_REMOTE:    Result := (not KeyOptions.OpenNetworkReadOnly);
     0, 1,
     DRIVE_CDROM,
     DRIVE_RAMDISK:   Result := false;
   end;


   if Result then begin
      FileName:= ExtractFilename(FN);
      ext:= ExtractFileExt(FN);

      // Check if alternate backup directory exists
      if KeyOptions.BackupDir <> '' then begin
        if not DirectoryExists(KeyOptions.BackupDir) then begin
          App.WarningPopup(Format(GetRS(sFileM20), [KeyOptions.BackupDir]));
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
         DayBakFN := ChangeFileExt(bakFolder + FileName, IntervalBAKDay + ext);
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
         TFile.WriteAllText(DayBakFN_Txt, Format(GetRS(sFileM78), [DateToStr(Date), BakFN]));
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
       if ActiveFile.NoMultiBackup then
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
      TFile.WriteAllText(DayBakFN_Txt, Format(GetRS(sFileM78), [DateToStr(Date), BakFN]));

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
   FileName, BakFN, ext: string;
   bakFNfrom, bakFNTo: string;
begin
   if not KeyOptions.BackupRegularIntervals then Exit;

   DecodeDate(Date, Year, Month, Day);
   FileName:= ExtractFilename(FN);
   ext:= ExtractFileExt(FN);

   BakFN := Format(
               ChangeFileExt(BakFolder + '\' + FileName,
                  IntervalPrefixBAK + '%d_%.2d' + ext), [Year, Month]);

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
            bakFNfrom := BakFN + IntToStr(bakIndex)       + ext;
            bakFNto   := BakFN + IntToStr(Succ(bakIndex)) + ext;
            if FileExists(bakFNfrom) then
               MoveFileEx(
                  PChar(bakFNfrom), PChar(bakFNto),
                  MOVEFILE_REPLACE_EXISTING or MOVEFILE_COPY_ALLOWED);
         end; // for
         CopyFile(PChar(FN), PChar(BakFN + '1' + ext), False);   // Weekly backup
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
  SavedFolders, SavedNotes: integer;
  KntFile: TKntFile;

  procedure RenameTempFile;
  var
     Str: string;
  begin
     if not MoveFileExW_n (tempFN, FN, 5) then begin
        Str:= GetRS(sFileM25) + GetRS(sFileMInfSaving);
        raise EKeyKntFileError.CreateFmt(Str, [FN, GetLastError, tempFN, KeyOptions.BackupDir]);
     end;
  end;


begin
  KntFile:= ActiveFile;

  with Form_Main do begin
     result := -1;
     if not HaveKntFolders(true, false) then Exit;
     if KntFile.IsBusy then Exit;

     if (FN <> '') and KntFile.ReadOnly then begin
         App.WarningPopup(GetRS(sFileM77));
         Exit;
     end;

     if (FN <> '') and not KntFile.Modified then begin
         StatusBar.Panels[PANEL_HINT].Text := GetRS(sFileM79);
         Exit;
     end;

     ErrStr := '';

     SUCCESS := LongBool(0);
     try
       try
         KntFile.IsBusy := true;
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
                nffKeyNote   : Filter := FILTER_NOTEFILES + '|' + GetRS(FILTER_ALLFILES);
                else
                  Filter := FILTER_NOTEFILES {$IFDEF WITH_DART} + '|' + FILTER_DARTFILES {$ENDIF} + '|' + GetRS(FILTER_ALLFILES);
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
                 end;
              end;

           end
           else
              Exit;
         end;

         // Initialize ImagesManager to reflect the FN in case it is a new file
         if not ImageMng.PrepareImagesStorageToSave(FN) then
            exit;


         Screen.Cursor := crHourGlass;
         StatusBar.Panels[PANEL_HINT].text := GetRS(sFileM19) + FN;


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



         // SAVE the file (and backup virtual nodes if it applies), on a temporal location, before initiate DoBackup (see *1)
         Result := KntFile.Save(tempFN, SavedFolders, SavedNotes);

         Log_StoreTick( 'After KntFile.Save (temporal location)', 1 );

         if Result = 0 then begin
            // BACKUP (using previous file, before this saving) of the file
            if DoBackup(FN, BakFN, BakFolder, SUCCESS, LastError) then begin
               if not SUCCESS then begin
                  if App.DoMessageBox(Format(GetRS(sFileM21), [LastError, SysErrorMessage(LastError), tempFN]), mtWarning, [mbYes,mbNo]) <> mrYes then begin
                    Result := -2;
                    Exit;
                  end;
               end;
            end;
            Log_StoreTick( 'After DoBackup', 1 );

            RenameTempFile;                 // Now rename the temp file to the actual KeyNote file name
            KntFile.FileName := FN;
            KntFile.Modified:= False;      // Must be done here, not in TNotFile.Save, and of course, never before RenameTempFile

            StatusBar.Panels[PANEL_HINT].Text := Format(GetRS(sFileM22), [SavedFolders, SavedNotes]);
            KntFile.ReadOnly := False;    // We can do SaveAs from a Read-Only file (*)
                 { (*) In Windows XP is possible to select (with SaveDlg) the same file
                    as destination. In W10 it isn't }

            // Backup after saving
            DoIntervalBackup(FN, BakFolder);
            Log_StoreTick( 'After DoIntervalBackup', 1 );
         end
         else begin
            // ERROR on save
            StatusBar.Panels[PANEL_HINT].Text := Format(GetRS(sFileM23), [Result] );
            ErrStr := Format(GetRS(sFileM24) + GetRS(sFileMInfSaving), [Result, tempDirectory, KeyOptions.BackupDir] );

            if KeyOptions.AutoSave then begin
               KeyOptions.AutoSave := False;
               ErrStr := ErrStr + GetRS(sFileM26);
            end;

            App.ErrorPopup(ErrStr);
         end;

       except
         on E: Exception do begin
           {$IFDEF KNT_DEBUG}
           Log.Add('Exception in KntFileSave: ' + E.Message);
           {$ENDIF}
           StatusBar.Panels[PANEL_HINT].Text := GetRS(sFileM27);
           App.ErrorPopup(E, GetRS(sFileM28) + ExtractFileName(FN));
           Result := 1;
         end;
       end;

       try // folder monitor
         if not KeyOptions.DisableFileMon then begin
            GetFileState(KntFile.FileName, KntFile.State);
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
           App.PopupMessage(GetRS(sFileM29) + E.Message, mtError, [mbOK]);
         end;
       end;

     finally
       Screen.Cursor := crDefault;
       KntFile.IsBusy := False;
       UpdateOpenFile;
      {$IFDEF KNT_DEBUG}
       Log.Add( 'KntFileSave result: ' + IntToStr(Result));
      {$ENDIF}

       Log_StoreTick( 'KntFileSave - END', 0, -1 );
       Log_Flush;

       if assigned(Res_RTF) and (ImageMng.StorageMode <> smEmbRTF) then begin
          Res_RTF.RemoveKNTHiddenCharacters(false);
          Res_RTF.ReconsiderImages(false, imImage);
       end;

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
var
   KntFile: TKntFile;
begin

  if ( not Form_Main.HaveKntFolders( false, false )) then exit;
  if ( not CheckModified( not KeyOptions.AutoSave, false )) then
  begin
    result := false;
    exit;
  end;

  KntFile:= ActiveFile;

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
        MacroAbortRequest := true
      else
      if IsRecordingMacro then
        TB_MacroRecordClick( TB_MacroRecord );

      screen.Cursor := crHourGlass;

      try
        KntFile.IsBusy := true;
        FolderMon.Active := false;

        Pages.MarkedPage := nil;
        if (ClipCapMng.ClipCapActive) then begin
           TB_ClipCap.Down := false;
           ClipCapMng.ToggleOff;
        end;

        LastEditCmd := ecNone;
        UpdateLastCommand( ecNone );
        BookmarkInitializeAll;
        UpdateAlarmStatus;

        if assigned( KntFile ) then begin
          try
            KntFile.ReleaseNoteUIs;     // Unbind interface before releasing object (which is done after TTab95Sheet.Free)
            DestroyVCLControls;
          except
            // showmessage( 'BUG: error in DestroyVCLControls' );
          end;
          try
            try
              App.FileClosed(KntFile);
              KntFile.Free;
            except
              // showmessage( 'BUG: error in KntFile.Free' );
            end;
          finally
            ActiveFile := nil;
          end;
        end;

        History.Clear;
        UpdateHistoryCommands;


        TAM_ActiveName.Caption := '';
        UpdateOpenFile;
        StatusBar.Panels[PANEL_HINT].Text := GetRS(sFileM30);
      finally
        if assigned(Res_RTF) and (ImageMng.StorageMode <> smEmbRTF) then
           Res_RTF.RemoveKNTHiddenCharacters(false);

        AlarmMng.Clear;
        ImageMng.Clear;
        TKntTreeUI.ClearGlobalData;
        App.TagsUpdated;
        txtFilterTags.Text:= '';

        KntFile.IsBusy := false;
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
                        OnlyCurrentNodeAndSubtree: PVirtualNode= nil;
                        OnlyNotHiddenNodes: boolean= false;
                        OnlyCheckedNodes: boolean= false);
var
  currentFN, newFN : string;
  cr : integer;
  oldModified : boolean;
  DirDlg : TdfsBrowseDirectoryDlg;
  KntFile: TKntFile;

begin
  KntFile:= ActiveFile;

  with Form_Main do begin

        if ( not HaveKntFolders( true, false )) then exit;

        currentFN := KntFile.FileName;
        if (FN = '') and (currentFN = '') then begin
           App.PopupMessage( GetRS(sFileM82), mtError, [mbOK] );
           exit;
        end;

        DirDlg := TdfsBrowseDirectoryDlg.Create( Form_Main );

        try

          if FN = '' then begin
              DirDlg.Root := idDesktop;
              DirDlg.ShowSelectionInStatus := true;
              DirDlg.Title := GetRS(sFileM32);
              DirDlg.Center := true;

              if ( KeyOptions.LastCopyPath <> '' ) then
                DirDlg.Selection := KeyOptions.LastCopyPath
              else
                DirDlg.Selection := GetFolderPath( fpPersonal );

                if ( not DirDlg.Execute ) then exit;
                if ( properfoldername( extractfilepath( currentFN )) = properfoldername( DirDlg.Selection )) then
                begin
                  App.PopupMessage( GetRS(sFileM33), mtError, [mbOK]);
                  exit;
                end;

                KeyOptions.LastCopyPath := properfoldername( DirDlg.Selection );

                newFN := KeyOptions.LastCopyPath + ExtractFilename( currentFN );
                if FileExists( newFN ) then
                   if ( App.Popupmessage( Format(GetRS(sFileM34), [newFN]), mtConfirmation, [mbYes,mbNo] ) <> mrYes ) then exit;

          end
          else
             newFN:= FN;

          StatusBar.Panels[PANEL_HINT].Text := GetRS(sFileM35);


          oldModified := KntFile.Modified;
          screen.Cursor := crHourGlass;
          try
            try
              ImageMng.ExportingMode:= true;
              try
                 cr := KntFile.Save( newFN, SavedFolders, SavedNodes, ExportingMode, OnlyCurrentNodeAndSubtree, OnlyNotHiddenNodes, OnlyCheckedNodes);
              finally
                 ImageMng.ExportingMode:= false;
              end;

              if ( cr = 0 ) then begin
                StatusBar.Panels[PANEL_HINT].Text := GetRS(sFileM36);
                App.PopUpMessage( GetRS(sFileM37) +#13 + NewFN, mtInformation, [mbOK] );
              end
              else begin
                App.Popupmessage( GetRS(sFileM38) + inttostr( cr ) + ')', mtError, [mbOK] );
               {$IFDEF KNT_DEBUG}
                Log.Add( 'Copying failed (' + inttostr( cr ) + ')' );
               {$ENDIF}
              end;

            except
              on E : Exception do begin
                StatusBar.Panels[PANEL_HINT].Text := GetRS(sFileM15);
               {$IFDEF KNT_DEBUG}
                Log.Add( 'Exception in KntFileCopy: ' + E.Message );
               {$ENDIF}
                App.PopupMessage( E.Message, mtError, [mbOK] );
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
  ImgManagerMF: TImageMng;
  LoadResult : integer;
  TabSelector : TForm_SelectTab;
  mergecnt, i, n, p : integer;
  newFolder, mergeFolder, TargetFolder : TKntFolder;
  newNNode, mergeNNode : TNoteNode;
  TargetNoteID_: Cardinal;
  FolderIDs: array of TMergeFolders;
  NoteGIDs: TMergedNotes;
  MergeNNodesCount: integer;
  mirrorID: string;
  FolderID: integer;
  GIDsNotConverted: integer;
  MergeNotesMultiNNodes: TNoteList;
  NewNotesMultiMergeNNodes: TNoteList;


  function GetNewFolderID(FolderID: integer): integer;
  var
    i: integer;
  begin
    Result:= 0;
    for i := 0 to High(FolderIDs) do
        if FolderIDs[i].oldID = FolderID then begin
           Result:= FolderIDs[i].newID;
           exit;
        end;
  end;


begin
  with Form_Main do begin

        if ( not HaveKntFolders( true, false )) then exit;
        if ActiveFileIsBusy then exit;

        if ( MergeFN = '' ) then begin

          if ( KeyOptions.LastExportPath <> '' ) then
            OpenDlg.InitialDir := KeyOptions.LastExportPath;
          OpenDlg.Title := GetRS(sFileM39);

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

        ImgManagerMF:= TImageMng.Create;
        ImageMng.ExternalImagesManager:= ImgManagerMF;

        try
          try
            var ClipCapIdx: integer;
            AFileIsLoading:= true;                  // -> MergeFile notes uploaded here will not be marked as modified (nor will their fLastModified field be overwritten)
            LoadResult := MergeFile.Load( MergeFN, ImgManagerMF, ClipCapIdx, false);
            if ( LoadResult <> 0 ) then begin
              App.ErrorPopup( GetRS(sFileM40));
              exit;
            end;

            if ( MergeFile.FolderCount = 0 ) then begin
              App.InfoPopup( GetRS(sFileM41));
              exit;
            end;

            for i := 0 to pred( MergeFile.FolderCount ) do begin
              // initially, UNMARK ALL notes (i.e. no merging)
              MergeFile.Folders[i].Info := 0;
            end;

          except
            on E : Exception do begin
              App.ErrorPopup( GetRS(sFileM42) + E.Message);
              exit;
            end;
          end;

          TabSelector := TForm_SelectTab.Create( Form_Main );
          try
            TabSelector.myKntFile := MergeFile;
            TabSelector.Caption := Format( GetRS(sFileM43), [ExtractFilename( MergeFile.FileName )] );
            if ( not ( TabSelector.ShowModal = mrOK )) then exit;
          finally
            TabSelector.Free;
          end;

          MergeNNodesCount:= 0;
          SetLength(FolderIDs, MergeFile.FolderCount);
          for i := 0 to pred( MergeFile.FolderCount ) do begin
            // see if user selected ANY notes for merge
            if ( MergeFile.Folders[i].Info > 0 ) then begin
              inc( mergecnt );
              inc(MergeNNodesCount, MergeFile.Folders[i].NNodes.Count);
            end;
          end;

          if ( mergecnt = 0 ) then begin
            App.InfoPopup( GetRS(sFileM44));
            exit;
          end;

          mergecnt := 0;
          GIDsNotConverted:= 0;
          NoteGIDs:= TMergedNotes.Create(MergeNNodesCount);
          NoteGIDs.SameFile := (MergeFN = ActiveFile.FileName);
          MergeNotesMultiNNodes:= TNoteList.Create;
          NewNotesMultiMergeNNodes:= TNoteList.Create;

          StatusBar.Panels[PANEL_HINT].Text := GetRS(sFileM45);

          screen.Cursor := crHourGlass;

          try
            for i := 0 to pred( ActiveFile.FolderCount ) do      // to convert KntLinks to new format only on new, merged, folders
               ActiveFile.Folders[i].Info:= 0;

            for i := 0 to pred( MergeFile.FolderCount ) do begin

              FolderIDs[i].oldID:= MergeFile.Folders[i].ID;
              if (MergeFile.Folders[i].Info = 0) then begin
                 FolderIDs[i].newFolder:= false;
                 if not NoteGIDs.SameFile then
                    FolderIDs[i].newID:= 0
                 else
                    FolderIDs[i].newID:= FolderIDs[i].oldID;
                  continue;
              end;


              newFolder := TKntFolder.Create(ActiveFile);

              newFolder.Visible := true;
              newFolder.Modified := false;
              newFolder.Info := 1;

              with MergeFile.Folders[i] do begin
                newFolder.DefaultPlainText := DefaultPlainText;

                newFolder.EditorChrome := EditorCHrome;
                newFolder.Name := Name;
                newFolder.ImageIndex := ImageIndex;
                newFolder.ReadOnly := ReadOnly;
                newFolder.DateCreated := DateCreated;
                newFolder.WordWrap := WordWrap;
                newFolder.URLDetect := URLDetect;
                newFolder.TabSize := TabSize;
                newFolder.UseTabChar := UseTabChar;

                newFolder.IconKind := IconKind;
                newFolder.TreeWidth := TreeWidth;
                newFolder.Checkboxes := CheckBoxes;
                newFolder.TreeChrome := TreeChrome;
                newFolder.DefaultNoteName := DefaultNoteName;
                //newFolder.AutoNumberNodes := AutoNumberNodes;
                newFolder.VerticalLayout := VerticalLayout;
                newFolder.HideCheckedNodes := HideCheckedNodes;

                // See comment *1 in PopulateTree (knt.ui.tree)
                // SavedSelectedIndex := -1 could be very problematic if TV.TotalCount is not called ...
                newFolder.SavedSelectedIndex := 0;
              end;

              newFolder.LoadingLevels.Assign(MergeFile.Folders[i].LoadingLevels);
              ActiveFile.AddFolder( newFolder );

              AlarmMng.AddProcessedAlarmsOfFolder(MergeFile.Folders[i], NewFolder);

              mergeFolder:= MergeFile.Folders[i];
              if (mergeFolder.NNodes.Count > 0 ) then begin
                  var Index: integer;
                  var NoteToAddNewNNode: TNote;
                  for n := 0 to mergeFolder.NNodes.Count - 1 do begin
                     mergeNNode:= mergeFolder.NNodes[n];

                     // We must import the nodes linked in merge file (mirror nodes) in the same way, as linked nodes
                     NoteToAddNewNNode:= nil;
                     if mergeNNode.Note.NumNNodes > 1 then begin
                        Index:= MergeNotesMultiNNodes.IndexOf(mergeNNode.Note);
                        if Index >=0 then
                           NoteToAddNewNNode:= NewNotesMultiMergeNNodes[Index]
                        else
                           MergeNotesMultiNNodes.Add(mergeNNode.Note);
                     end;

                     if NoteToAddNewNNode <> nil then
                        newNNode:= ActiveFile.AddNewNNode(NoteToAddNewNNode, newFolder)
                     else begin
                        if mergeNNode.Note.IsVirtual then begin
                           var ExistingVirtualNote: TNote;
                           ExistingVirtualNote:= ActiveFile.GetVirtualNoteByFileName(nil, mergeNNode.Note.VirtualFN);
                           if ExistingVirtualNote <> nil then
                              newNNode:= ActiveFile.AddNewNNode(ExistingVirtualNote, newFolder)
                           else
                              newNNode:= ActiveFile.AddNewVirtualNote(newFolder, mergeNNode, false);
                        end
                        else
                           newNNode:= ActiveFile.AddNewNote(newFolder, mergeNNode, false);      // false: we will count the images a little further down, in a controlled way, perhaps converting the storage mode
                        if mergeNNode.Note.NumNNodes > 1 then
                           NewNotesMultiMergeNNodes.Add(newNNode.Note);
                     end;

                     newNNode.ForceID(mergeNNode.ID);
                     NoteGIDs.AddOldNewGIDs(mergeNNode.GID, newNNode.GID);
                     AlarmMng.AddProcessedAlarmsOfNote(mergeNNode, mergeFolder, newFolder, newNNode);
                  end;
              end;

              if ( mergeFolder.NNodes.Count = 1 ) and (mergeFolder.NNodes[0].NoteName= mergeFolder.Name) then
                  newFolder.TreeHidden:= true;           // It was an old simple note


              inc( mergecnt );

              FolderIDs[i].newID:= newFolder.ID;
              FolderIDs[i].newFolder:= true;

              try
                CreateVCLControlsForFolder( newFolder );
                if ( MergeFN.ToUpper = ActiveFile.FileName.ToUpper) then
                   ActiveFile.UpdateImagesCountReferences(newFolder)
                else
                  { We have previously assigned "ImagesManager.ExtenalImagesManager:= ImgManagerMF", to search the Stream of the images
                    with the help of the ImageManager associated with the MergeFile file }
                  ActiveFile.UpdateImagesStorageModeInFile (ImageMng.StorageMode, newFolder, false);

                newFolder.LoadEditorFromNNode(newFolder.FocusedNNode, False);
                SetUpVCLControls( newFolder );
              finally
                newFolder.TabSheet.TabVisible := true; // was created hidden
                newFolder.TreeUI.TV.ScrollIntoView(newFolder.TreeUI.FocusedNode, false);
              end;

            end;

            { Mirror nodes (if exists).. will have been converted to NNodes when opening the file. 
              Even if we import mirror nodes to nodes in another folder that we do not import, it does not matter now.
              During the conversion carried out when opening the file we will have NNodes like others. The possibility
              of including several linked nodes has already been managed above, with the help of MergeNotesMultiNNodes
              and NewNotesMultiMergeNNodes }

            ActiveFile.ConvertKNTLinksToNewFormatInNotes(FolderIDs, NoteGIDs, GIDsNotConverted);      // only on selected folders (with .Info=1)
            if GIDsNotConverted > 0 then
               App.WarningPopup( Format(GetRS(sFileM83), [GIDsNotConverted, NoteGID_NotConverted]));

            for i := 0 to pred( MergeFile.FolderCount ) do
                if FolderIDs[i].newFolder then begin
                   newFolder:= ActiveFile.GetFolderByID(FolderIDs[i].newID);
                   newFolder.ReloadEditorFromDataModel;
                end;


          except
            On E : Exception do begin
              App.ErrorPopup( GetRS(sFileM46) + E.Message);
              exit;
            end;
          end;

        finally
          AFileIsLoading:= false;
          if assigned(NoteGIDs) then
             NoteGIDs.Free;
          if assigned(MergeNotesMultiNNodes) then begin
             MergeNotesMultiNNodes.Free;
             NewNotesMultiMergeNNodes.Free;
          end;
          MergeFile.Free;
          ImageMng.ExternalImagesManager:= nil;
          ImgManagerMF.Free;
          AlarmMng.ClearAuxiliarProcessedAlarms;
          PagesChange( Form_Main );
          screen.Cursor := crDefault;
          App.FileSetModified;
          if ( mergecnt > 0 ) then
            StatusBar.Panels[PANEL_HINT].Text := Format( GetRS(sFileM47), [mergecnt, ExtractFilename( MergeFN )] )
          else
            StatusBar.Panels[PANEL_HINT].Text := GetRS(sFileM48);
        end;
  end;

end; // MergeFromKNTFile


//=================================================================
// SomeoneChangedOurFile
//=================================================================
procedure SomeoneChangedOurFile;
begin
  Application_BringToFront;
  Form_Main.FolderMon.Active := false;
  try
    case App.DoMessageBox( Format(GetRS(sFileM49), [ActiveFile.State.Name]), mtWarning, [mbYes,mbNo] ) of
      mrYes : begin
        KntFileOpen( ActiveFile.FileName );
      end;
      mrNo : begin
        App.FileSetModified;
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
      App.ErrorPopup(Format(GetRS(sFileM50), [name,folder]));
    exit;
  end;

  if Prompt then begin
    if App.DoMessageBox( Format(GetRS(sFileM50) + GetRS(sFileM51), [name,folder]), mtConfirmation, [mbYes,mbNo] ) <> mrYes then
      exit;
  end;

  try
    mkdir( folder );
    result := true;
  except
    on e : exception do begin
      result := false;
      if Prompt then
        App.ErrorPopup( Format(GetRS(sFileM52), [E.Message] ));
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
        if ActiveFileIsBusy then exit;
        if ( not HaveKntFolders( false, false )) then exit;
        if ( ActiveFile.State.Name = '' ) then exit;
        Changed := false;
        s := '';
        GetFileState( ActiveFile.State.Name, NewState );
        if ( NewState.Size = -1 ) then begin
          // means file does not exist (deleted or renamed)
          // If Size = -99: an exception in GetFileStateCannot was raised, that could probably be:
          // <<Cannot open file "...". The process cannot access the file because it is being used by another process" -> Exception "The process cannot access the file because it is being used by another process>>
          App.FileSetModified;      // so that we save it
          exit;
        end;
        if ( ActiveFile.State.Time <> NewState.Time ) then begin
          Changed := true;
          s := 'time stamp';
        end
        else begin
          if ( ActiveFile.State.Size <> NewState.Size ) then begin
            Changed := true;
            s := 'file size';
          end;
        end;

        if Changed then begin
          ActiveFile.ChangedOnDisk := true;
          StatusBar.Panels[PANEL_HINT].Text := GetRS(sFileM53);
         {$IFDEF KNT_DEBUG}
          Log.Add( 'FileChangedOnDisk: ' + s );
         {$ENDIF}
          GetFileState( ActiveFile.State.Name, ActiveFile.State );
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
        Log.Add( 'CheckModified: KntFile modified? ' + BOOLARRAY[ActiveFile.Modified], 1 );
       {$ENDIF}
        App.HideNestedFloatingEditors;
        if FloatingEditorCannotBeSaved then exit(False);


        if ( not ActiveFile.Modified ) then exit;
        if Warn then begin
          case App.DoMessageBox( GetRS(sFileM54), mtConfirmation, [mbYes,mbNo,mbCancel] ) of
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
           Application_Minimize;  // Liberate the screen to let the user do other things while keyNote is closing
        end;

       {$IFDEF KNT_DEBUG}
        Log.Add( '-- Saving on CHECKMODIFIED', 1 );
       {$ENDIF}
        if ( KntFileSave(ActiveFile.FileName) = 0 ) then
          result := true
        else
          result := ( Application.MessageBox( PChar(GetRS(sFileM55)), PChar(GetRS(sFileM56)), MB_YESNO+MB_ICONEXCLAMATION+MB_DEFBUTTON2+MB_APPLMODAL) = ID_YES );

        if closing and not wasMinimized then begin
           Application_Restore;
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
          Title := GetRS(sFileM57);
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
             App.WarningPopup(Format(GetRS(sFileM65), [FN]));
             Result:= False;
          end
          else
            case App.DoMessageBox( Format(GetRS(sFileM58), [ExtractFilename( FN )]), mtWarning, [mbYes,mbNo] ) of
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

  NNode : TNoteNode;
  NEntry: TNoteEntry;
  myTreeNode : PVirtualNode;

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

              StatusBar.Panels[PANEL_HINT].Text := GetRS(sFileM59) + ExtractFilename( FN );

              if ( ImportFileType = itHTML ) then   // first see if we can do the conversion, before we create a new folder for the file
              begin
                if not ConvertHTMLToRTF( FN, OutStream) then begin
                   App.WarningPopup(Format(GetRS(sFileM60), [FN]));
                   exit;
                end;
              end;

              try
                myFolder := TKntFolder.Create(ActiveFile);
                myFolder.SetEditorProperties( DefaultEditorProperties );
                myFolder.SetTabProperties( DefaultTabProperties );
                myFolder.EditorChrome := DefaultEditorChrome;
                if KeyOptions.ImportFileNamesWithExt then
                  s := ExtractFilename( FN )
                else
                  s := ExtractFilenameNoExt( FN );
                myFolder.Name := s;
                ActiveFile.AddFolder( myFolder );

                try

                  NNode:= nil;
                  if ImportFileType <> itTreePad then begin
                     CreateVCLControlsForFolder( myFolder );
                     NNode:= myFolder.TreeUI.NewNode(tnTop, nil, s, true );
                     NEntry:= NNode.Note.Entries[0];        //%%%
                     myFolder.TreeHidden:= true;
                  end;

                  case ImportFileType of
                    itText, itRTF : begin
                     {$IFDEF KNT_DEBUG}Log.Add('Import As Folder. (TXT or RTF)  FN:' + FN,  1 ); {$ENDIF}
                      LoadTxtOrRTFFromFile(NEntry.Stream, FN);
                      NEntry.IsRTF:= (ImportFileType = itRTF);
                      end;
                    itHTML : begin
                     {$IFDEF KNT_DEBUG}Log.Add('Import As Folder. (HTML)  FN:' + FN,  1 ); {$ENDIF}
                      NEntry.Stream.LoadFromStream(OutStream);
                      NEntry.Stream.Position:= NEntry.Stream.Size;
                      NEntry.Stream.Write(AnsiString(#13#10#0), 3);
                      NEntry.IsHTML:= true;  // By default it will be tb. IsPlainTXT=True
                      end;
                    itTreePad : begin
                     {$IFDEF KNT_DEBUG}Log.Add('Import As Folder. (TreePad)  FN:' + FN,  1 ); {$ENDIF}
                      myFolder.SetTreeProperties( DefaultTreeProperties );
                      myFolder.TreeChrome := DefaultTreeChrome;
                      myFolder.LoadFromTreePadFile( FN );
                      CreateVCLControlsForFolder( myFolder );
                      end;
                    end;

                  myFolder.ReloadEditorFromDataModel(False);
                  SetUpVCLControls( myFolder );
                  UpdateTreeVisible( ActiveFolder );

                  var Owned: boolean:= not ImgLinkMode;
                  if ImportFileType = itImage then
                     ImageMng.InsertImage(FN, myFolder.Editor , Owned);

                finally
                  if assigned( myFolder.TabSheet ) then
                      myFolder.TabSheet.TabVisible := true; // was created hidden

                 { Starting with version 1.9.6, it will be controlled whether the notes (or more precisely, each possible
                   entry of each note) are RTF or Plain Text individually. At the folder level, only the default value to
                   be used in new notes/entries will be defined.  }

                end;

              except
                on E : Exception do begin
                  App.ErrorPopup(E, GetRS(sFileM61) + FN);
                  exit;
                end;
              end;

            finally
              screen.Cursor := crDefault;
              AddToFileManager( ActiveFile.FileName, ActiveFile ); // update manager (number of notes has changed)
              if assigned(myFolder) then
                 App.ActivateFolder(myFolder);
              StatusBar.Panels[PANEL_HINT].text := GetRS(sFileM62);
              App.FileSetModified;
            end;
          end;

        finally
            if assigned( OutStream ) then OutStream.Free;
            FreeConvertLibrary;
        end;
  end;
end; // ImportAsKntFolders



//=================================================================
// InsertContent
//=================================================================
procedure InsertContent( ImportFileList : TStringList; ImgLinkMode: boolean; const NameProposed: string = '' );
var
  FN, strContent : string;
  Editor: TKntRichEdit;
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

        if not assigned(ActiveEditor) then exit;
        if (( not assigned( ImportFileList )) or ( ImportFileList.Count = 0 )) then exit;

        Editor:= ActiveEditor;

        Stream:= TMemoryStream.Create;
        try
          if Editor.SelLength > 0 then
             Editor.CheckToSelectLeftImageHiddenMark
          else
             Editor.CheckToMoveLefOftHiddenMark;       // *1

          InformedImgInPlain:= false;

          for filecnt := 0 to pred( ImportFileList.Count ) do begin
            FN := normalFN( ImportFileList[filecnt] );
            Stream.Clear;

            if not GetImportFileType(FN, ImportFileType, false) then
               continue;


            screen.Cursor := crHourGlass;

            try
              App.ShowInfoInStatusBar(GetRS(sFileM59) + ExtractFilename( FN ));

              try
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
                       App.WarningPopup(Format(GetRS(sFileM60), [FN]));
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
                    Editor.PutRtfText(StrContent, true);
                    end;
                  itImage: begin
                     {$IFDEF KNT_DEBUG}Log.Add('Insert content (Image)  FN:' + FN,  1 ); {$ENDIF}
                     var Owned: boolean:= not ImgLinkMode;
                     if not Editor.PlainText then
                        ImageMng.InsertImage(FN, Editor, Owned, NameProposed)
                     else begin
                         if not InformedImgInPlain then begin
                            App.WarningPopup(Format(GetRS(sFileM81), [FN]));
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
                  App.ErrorPopup(E, GetRS(sFileM61) + FN);
                  exit;
                end;
              end;

            finally
              screen.Cursor := crDefault;
              App.ShowInfoInStatusBar(GetRS(sFileM62));
              Editor.Change;
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
  FileIsHTML, FileIsImage, EditorIsNotReadOnly, ActiveFileReady : boolean;
  myTreeNode : PVirtualNode;
  IsKnownFileFormat : boolean;
  i, iSelected: integer;
  FileCnt, j: integer;
  Editor: TKntRichEdit;
begin

  FileCnt:= FileList.Count;

  with Form_Main do begin
     if (( aExt = ext_Plugin ) or ( aExt = ext_Macro )) then begin
       result := factExecute;
       exit;
     end;

     result := factUnknown;
     LastFact := FactUnknown;
     Editor:= ActiveEditor;
     EditorIsNotReadOnly := not Editor.ReadOnly;
     ActiveFileReady:= assigned(ActiveFile);

     FileIsHTML  := ExtIsHTML( aExt );
     FileIsImage := ExtIsImage(aExt);

     IsKnownFileFormat := ( FileIsHTML or ExtIsText( aExt ) or ExtIsRTF( aExt ) or FileIsImage);

     // Select actions which can be performed depending on extension of the dropped file
     for fact := low( fact ) to high( fact ) do
        facts[fact] := false;

     facts[factHyperlink] := ( EditorIsNotReadOnly ); // this action can always be perfomed unless current editor is read-only

     // .KNT, .KNE and DartNotes files can only be opened or merged, regardless of where they were dropped. This can only be done one file at a time.
     if ( aExt = ext_KeyNote ) or
        ( aExt = ext_Encrypted ) or
        ( aExt = ext_DART ) then
     begin
       facts[factOpen] := true;
       facts[factMerge] := ActiveFileReady;
     end
     else
     if ( aExt = ext_TreePad ) then
        facts[factImportAsFolder] := ActiveFileReady

     else begin
       // all other files we can attempt to import...
       facts[factImportAsFolder] := ActiveFileReady and IsKnownFileFormat;
       facts[factInsertContent]:= EditorIsNotReadOnly and (not FileIsImage or Editor.SupportsImages);
       if ( ActiveFileReady and EditorIsNotReadOnly) then begin
         myTreeNode:= nil;
         if (ActiveFolder <> nil) then
            myTreeNode := ActiveFolder.TV.FocusedNode;
         if assigned( myTreeNode ) then begin
            facts[factImportAsNode] := IsKnownFileFormat;
            facts[factMakeVirtualNode] := IsKnownFileFormat and not FileIsImage;
         end;
       end;
     end;



     if (( LastFact = factUnknown ) or ( not facts[LastFact] )) then begin
       Form_DropFile := TForm_DropFile.Create( Form_Main );

       try
         if FileIsImage and (facts[factImportAsNode] or facts[factImportAsFolder] or facts[factInsertContent]) then begin
            Form_DropFile.chk_ImageLinkMode.Visible:= true;
            Form_DropFile.chk_ImageLinkMode.Checked := KeyOptions.ImgDefaultLinkMode;
            if (FileCnt = 1) then begin
               NewFileName:= ExtractFileName(FileList[0]);
               if not ImageMng.CheckUniqueName(NewFileName) then begin
                  Form_DropFile.ShowNewName:= true;
                  Form_DropFile.txtImgNewName.Text:= NewFileName;
               end;
            end
            else begin
               for j:= 0 to FileList.Count-1 do begin
                   NewFileName:= ExtractFileName(FileList[j]);
                   if not ImageMng.CheckUniqueName(NewFileName) then begin
                      Form_DropFile.ShowWarningRenamedNames:= true;
                      break;
                   end;
               end;
            end;
         end
         else begin
            Form_DropFile.chk_ImageLinkMode.Visible:= false;
            Form_DropFile.chk_ImageLinkMode.Checked := false;
         end;

         Form_DropFile.Btn_HTML.Enabled := FileIsHTML;
         Form_DropFile.Btn_HTML.Visible := FileIsHTML;
         if FileIsHTML then
           Form_DropFile.RG_HTML.ItemIndex := ord( KeyOptions.HTMLImportMethod );

         i:= 0;
         iSelected:= 0;
         for fact := low( fact ) to high( fact ) do begin
           if facts[fact] then begin
              Form_DropFile.RG_Action.Items.Add( FactStrings[fact] );
              if ((fact = factImportAsNode) and (TKntTreeUI.DropTargetNode <> nil)) or
                  (FileIsImage and (fact = factInsertContent) and (iSelected = 0)) then
                  iSelected:= i;
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
           App.ErrorPopup(GetRS(sFileM63));
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
procedure FileDropped( Sender : TObject; FileList : TStringList; Editor: TKntRichEdit = nil);
var
  myTreeNode : PVirtualNode;
  NNode : TNoteNode;
  NEntry: TNoteEntry;
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

     if not App.CheckActiveEditor then exit;


     myTreeNode := nil;
     if Editor = nil then
        Editor:= ActiveEditor;

     WinOnTop.AlwaysOnTop := false;
     try
       Application_BringToFront;

       if ( myAction = factUnknown ) then begin

         if ( not ConsistentFileType( FileList )) then begin
           //Messagedlg( GetRS(sFileM64, mtError, [mbOK], 0 );
           //exit;
           fExt:= '.*';
         end;

         if FileIsFolder and not Editor.ReadOnly  then begin
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
               Editor.SelText:= #13#10;
               Editor.SelLength:= 0;
               Editor.SelStart:= Editor.SelStart+2;
             end;

           factImportAsFolder :
             ImportAsKntFolders( FileList, ImgLinkMode );

           factInsertContent:
             InsertContent( FileList, ImgLinkMode, NewFileName);

           factImportAsNode :
             begin
               Editor.BeginUpdate;
               //SendMessage( Editor.Handle, WM_SetRedraw, 0, 0 ); // don't draw richedit yet           // commented: Using BeginUpdate
               OutStream:= TMemoryStream.Create;
               try
                 for i := 0 to FileList.Count - 1 do begin
                   OutStream.Clear;
                   FName := FileList[i];

                   FileIsHTML := ExtIsHTML( fExt );

                   if DirectoryExists( FName ) then begin
                     if ( App.DoMessageBox( Format( GetRS(sFileM65), [FName] ), mtWarning, [mbOK,mbAbort] ) = mrAbort ) then
                       exit
                     else
                       continue;
                   end;

                   {$IFDEF KNT_DEBUG}Log.Add('Import as Node: ' + FName,  1 ); {$ENDIF}

                   // first see if we can do the conversion, before we create a new folder for the file
                   if ( FileIsHTML and ( KeyOptions.HTMLImportMethod <> htmlSource )) then begin
                     if not ConvertHTMLToRTF( FName, OutStream) then begin
                        App.WarningPopup(Format(GetRS(sFileM60), [FName]));
                        exit;
                     end;
                   end;

                   NNode := ActiveTreeUI.NewNode(TKntTreeUI.DropTargetNodeInsMode, TKntTreeUI.DropTargetNode, '', true);
                   NEntry:= NNode.Note.Entries[0];       //%%%

                   if ( FileIsHTML and ( KeyOptions.HTMLImportMethod <> htmlSource )) then begin
                     NEntry.Stream.LoadFromStream(OutStream);
                     NEntry.Stream.Position:= NEntry.Stream.Size;
                     NEntry.Stream.Write(AnsiString(#13#10#0), 3);
                     NEntry.IsRTF:= True;
                   end
                   else if not ExtIsImage( fExt )  then begin
                     LoadTxtOrRTFFromFile(NEntry.Stream, FName);
                     NEntry.IsRTF:= ExtIsRTF( fExt );
                   end;

                   if KeyOptions.ImportFileNamesWithExt then
                     NNode.Note.Name := ExtractFilename( FName )
                   else
                     NNode.Note.Name := ExtractFilenameNoExt( FName );

                   if i = FileList.Count - 1 then begin
                      ActiveFolder.LoadEditorFromNNode(NNode, False);
                      if not ExtIsImage( fExt ) then
                         Editor.Modified:= False;
                   end;

                   var Owned: boolean:= not ImgLinkMode;
                   if ExtIsImage( fExt )  then begin
                     ImageMng.InsertImage(FName, Editor, Owned);
                     NEntry.IsRTF:= True;
                   end;


                   // *1
                   // If myTreeNode is assigned it is because TreeNewNode has returned ok.
                   // TreeNewNode ends up calling TV.Items.Add, which ends up raising the TV.Change event,
                   // managed by FormMain.TVChange. The last one, if the editor has modifications, calls TKntFolder.EditorToDataStream, and
                   // the content of the editor is saved in node's stream.
                   // But, if there is an exception (or simply we exit) before TreeNewNode, and enter in this finally section, we
                   // should not do Editor.Modified := False or the modifcations (existing and coming) in the Editor will be lost for the
                   // actual node.

                   // *3 WHAT IS INDICATED IN *2 IS NO LONGER NECESSARY, SINCE WE WILL NOW SAVE EACH NOTE ACCORDING TO ITS OWN FORMAT, 
                   // NOT THE ONE INDICATED BY THE FOLDER, WHICH IS ONLY USED AS THE DEFAULT VALUE FOR NEW EMPTY NOTES (IF WE DRAG A FILE THE
                   // NECESSARY FORMAT WILL BE RESPECTED , TXT OR RTF)

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
                   {  *3
                   if Editor.PlainText or   (not NodeStreamIsRTF (NNode.Stream)) then
                      Editor.Modified := True                                              // *2
                   else
                      Editor.Modified := False;                                            // *1
                   }

                   //Editor.Modified := False;                                            // *1
                 end;

               finally
                 if assigned( OutStream ) then OutStream.Free;
                 FreeConvertLibrary;
                 App.FileSetModified;
                 //SendMessage( Editor.Handle, WM_SetRedraw, 1, 0 ); // ok to draw now
                 Editor.EndUpdate;
                 Editor.Invalidate; // in fact, I insist on it
                 Editor.RestoreZoomGoal;

                 if ActiveFolder.TreeHidden then begin
                    ActiveFolder.TreeHidden:= false;
                    UpdateTreeVisible( ActiveFolder );
                 end;

                 App.EditorReloaded(Editor, Editor.Focused);
               end;

             end;

           factMakeVirtualNode :
             begin
               SendMessage( Editor.Handle, WM_SetRedraw, 0, 0 );
               try
                 for i := 0 to pred( FileList.Count ) do begin
                   FName := FileList[i];
                   if DirectoryExists( FName ) then begin
                     if ( App.DoMessageBox( Format( GetRS(sFileM65), [FName] ), mtWarning, [mbOK,mbAbort] ) = mrAbort ) then
                       exit
                     else
                       continue;
                   end;
                   NNode := ActiveTreeUI.NewNode(tnAddLast, nil, '', true );  // => New node will be focused -> Folder.NodeSelected
                   ActiveFolder.VirtualNoteProc(FName );
                 end;

               finally
                 SendMessage( Editor.Handle, WM_SetRedraw, 1, 0 ); // ok to draw now
                 Editor.Invalidate; // in fact, I insist on it
                 Editor.RestoreZoomGoal;
               end;
             end;

           factUnknown :
             begin
               // MessageDlg( 'No action was taken: could not determine method for handling files.', mtWarning, [mbOK], 0 );
               exit;
             end;

           else begin
             App.ErrorPopup( Format( GetRS(sFileM67), [ord( myAction )] ));
             exit;
           end;

         end; // case myAction

       except
         on E : Exception do begin
           App.ErrorPopup( GetRS(sFileM68) + E.Message);
           exit;
         end;
       end;

     finally
       screen.Cursor := crDefault;
       WinOnTop.AlwaysOnTop := KeyOptions.AlwaysOnTop;

       TKntTreeUI.DropTargetNode:= nil;
       TKntTreeUI.DropTargetNodeInsMode:= tnAddLast;
     end;
  end;

end; // FileDropped


//=================================================================
// KntFileProperties
//=================================================================
procedure KntFileProperties;
var
  Form_FileInfo : TForm_KntFileInfo;
  KntFile: TKntFile;
begin
  KntFile:= ActiveFile;

  with Form_Main do begin
      // Edits properties for currently open file

      if ( not HaveKntFolders( true, false )) then exit;

      Form_FileInfo := TForm_KntFileInfo.Create( Form_Main );

      try
        Form_FileInfo.myKntFile := KntFile;

        if ( Form_FileInfo.ShowModal = mrOK ) then begin
          App.Virtual_UnEncrypt_Warning_Done := false;

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

              ImageMng.SetImagesStorage(NewStorageMode, TImagesExternalStorage(cbImgExtStorageType.ItemIndex), ExtStorageLocation,
                                        KntFile.File_Path, false, rbImagesStRelocate.Checked);
          end;

          KntFile.Modified := true;
          AddToFileManager( KntFile.FileName, KntFile ); // update manager (properties have changed)

          LoadTrayIcon( ClipOptions.SwitchIcon and ClipCapMng.ClipCapActive);
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

      UpdateOpenFile;

      // [x] If passphrase changed or Encrypted state changed,
      // must SAVE FILE immediately.

   end;
end; // KntFileProperties



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

  TKntTreeUI.ClearGlobalData;

  if ActiveFileIsBusy then exit;
  if ( not ( KeyOptions.TimerClose and
             Form_Main.HaveKntFolders( false, false ) and
             KeyOptions.AutoSave
           )
       or (ActiveFile.FileName = '')
           ) then exit;


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


  if (( ActiveFile.FileFormat = nffEncrypted ) or ( not KeyOptions.TimerCloseEncOnly )) then begin
    // only under these conditions do we try to autoclose...

    Log_StoreTick('AutoCloseKntFile', 0);

    // First, do our own forms
    if ( Screen.FormCount > 1 ) then begin
      if KeyOptions.TimerCloseDialogs then begin
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
    if ( not IsWindowEnabled( Form_Main.Handle )) then begin
      if KeyOptions.TimerCloseDialogs then begin
        // there can only be one system dialog open,
        // unlike our own forms, of which there may be a few
        // on top of one another. But first, we must be the
        // active application, otherwise we'll send WM_CLOSE
        // to nowhere.
        Application_BringToFront;
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
    if ( ActiveFile.FileFormat = nffEncrypted ) then
      _REOPEN_AUTOCLOSED_FILE := KeyOptions.TimerCloseAutoReopen;

    KntFileClose;
    Application_Minimize;
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
        if assigned( ActiveFile ) then
          SelectedFileName := ActiveFile.FileName;
      end;
      MGROK := ( MGR.ShowModal = mrOK );
      s := MGR.SelectedFileName;
      KeyOptions.MgrFullPaths := MGR.ShowFullPaths;
    finally
      MGR.Free;
    end;

    if Form_Main.HaveKntFolders( false, false ) then
      olds := ActiveFile.Filename
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
      App.WarningPopup('Debug message: Error in RunFileManager.');
    end;
  end;

end; // RunFileManager

function CanRegisterFileType : boolean;
begin
  result := true;
  if App.opt_RegExt then
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
         App.InfoPopup( Format( GetRS(sFileM75), [ext_KeyNote] ));

    except
      on E : Exception do begin
        App.WarningPopup( GetRS(sFileM76) + e.Message + GetRS(sFileM80));
        KeyOptions.AutoRegisterFileType:= False;
      end;
    end;
  end;
end; // AssociateKeyKntFile

end.
