unit kn_PluginsMng;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface
uses
    Winapi.Windows,
    System.SysUtils,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.Controls,
    Vcl.StdCtrls,

    kn_Plugins
    ;


    // plugin-related routines
    procedure EnumeratePlugins;
    procedure DisplayPlugins;
    function ExecutePlugin (fn: string): longint;
    procedure ConfigurePlugin (fn: string);
    procedure UnloadResidentPlugin (const ID: longint);
    function GetPluginIconIndex (const Plugin: TPlugin): integer;
    function GetSelectedPlugin: TPlugin;
    procedure ShowPluginInfo;

var
    LastPluginFN : string;    // last plugin we ran, so we can re-run it on Shift+F12


implementation

uses
  cmpGFXListbox,
  kn_Global,
  kn_Info,
  kn_Const,
  kn_KntFolder,
  kn_PluginBase,
  kn_EditorUtils,
  knt.ui.editor,
  kn_Main,
  kn_NoteFileMng,
  kn_VCLControlsMng,
  knt.App,
  knt.RS
  ;


var
    Resident_Plugin_Counter : longint;


procedure EnumeratePlugins;
var
  DirInfo : TSearchRec;
  FindResult : integer;
  myPlugin : TPlugin;
  fn, aName, aInfo : string;
  aVersion : integer;
  aFeatures : TPluginFeatures;

begin
   ClearPluginList;

   FindResult := FindFirst( Plugin_Folder + '*' + ext_Plugin, faAnyFile, DirInfo );

   while ( FindResult = 0 ) do begin
     fn := Plugin_Folder + DirInfo.Name;

     if GetPluginInfo( fn, aName, aVersion, aInfo, aFeatures ) then begin
        myPlugin := TPlugin.Create;
        with myPlugin do begin
           FileName := DirInfo.Name;
           Name := aName;
           Version := aVersion;
           Info := aInfo;
           Features := aFeatures;
        end;
        Plugin_List.AddObject( DirInfo.Name, myPlugin );
     end;
     FindResult := FindNext( DirInfo );
   end;

   FindClose( DirInfo );

end; // EnumeratePlugins


function GetSelectedPlugin : TPlugin;
begin
  with Form_Main do begin
      result := nil;
      if ( not ( Pages_Res.Visible and ResTab_Plugins.TabVisible )) then exit;
      if (ListBox_ResPlugins.Items.Count = 0) or
         (ListBox_ResPlugins.ItemIndex < 0) then exit;

      result := TPlugin(TItemObject( ListBox_ResPlugins.Items.Objects[ListBox_ResPlugins.ItemIndex] ).Data);
  end;

end;


procedure ShowPluginInfo;
var
  myPlugin : TPlugin;
  pl : TPluginFeature;
  s : string;
begin
  myPlugin := GetSelectedPlugin;
  if ( not assigned( myPlugin )) then begin
     Form_Main.LB_PluginInfo.Caption := GetRS(sPlgM01);
     exit;
  end;

  s := '';
  for pl := low( TPluginFeature ) to high( TPluginFeature ) do
  begin
    if (pl in myPlugin.Features) then
       s:= s + PluginFeatureNames[pl] + '  ';
    {
    s := s + Format(
      '%s : %s ; ',
      [PluginFeatureNames[pl], BOOLARRAY[( pl in Plugin.Features )]]
    );
    }
  end;
  s := s + Format( GetRS(sPlgM02), [myPlugin.Version] );

  Form_Main.LB_PluginInfo.Caption := myPlugin.Info + #13 +
                           GetRS(sPlgM03) + myPlugin.Filename + #13 +
                           s;

end; // ShowPluginInfo


procedure DisplayPlugins;
var
  // Form_Plugins: TForm_Plugins;
  i, p : integer;
  myPlugin : TPlugin;
begin

  with Form_Main do begin
      if (not CheckFolder( 'Plugin', Plugin_Folder, true, true )) then begin
         ShowPluginInfo;
         exit;
      end;

      if ( Plugin_List.Count = 0 ) then begin
         // see if anything has been installed since we last checked
         EnumeratePlugins;
         if (Plugin_List.Count = 0 ) then begin
            StatusBar.Panels[PANEL_HINT].Text := GetRS(sPlgM04);
            ShowPluginInfo;
            exit;
         end;
      end;

      ListBox_ResPlugins.Items.BeginUpdate;
      try
        try
           for i := 1 to Plugin_List.Count do begin
              myPlugin := TPlugin( Plugin_List.Objects[pred( i )] );
              p := ListBox_ResPlugins.AddItem(
                {Format(
                 '%s (%s)',
                 [, myPlugin.Filename]
                 ),}
                 myPlugin.Name,
                 cbUnchecked, GetPluginIconIndex( myPlugin ));
              TItemObject( ListBox_ResPlugins.Items.Objects[p] ).Data := myPlugin;
          end;
          if ( ListBox_ResPlugins.Items.Count > 0 ) then
             ListBox_ResPlugins.ItemIndex := 0;

        except
          on E : Exception do
             MessageDlg( E.Message, mtError, [mbOK], 0 );
        end;

      finally
         ListBox_ResPlugins.Items.EndUpdate;
         ShowPluginInfo;
      end;
  end;

  (*
  Form_Plugins := TForm_Plugins.Create( self );
  try
    if ( Form_Plugins.ShowModal = mrOK ) then
    begin
      // execute selected

      if ( Form_Plugins.List.ItemIndex >= 0 ) then
      begin
        fn := TPlugin( Form_Plugins.List.Items.Objects[Form_Plugins.List.ItemIndex] ).FileName;
      end;

    end;
  finally
    Form_Plugins.Free;
  end;

  if ( fn = '' ) then exit;

  ExecutePlugin( fn );
  *)

end; // DisplayPlugins


procedure ConfigurePlugin( fn : string );
var
  myPlugin : TPlugin;
begin

  if (fn = '') then  begin
     myPlugin := GetSelectedPlugin;
     if ( not assigned( myPlugin )) then exit;
     fn:= myPlugin.Filename;
  end;

  if (pos('\', fn ) = 0 ) then
     fn := Plugin_Folder + fn;

  if (not ExecutePluginConfig( fn, Application.Handle )) then
     MessageDlg( GetRS(sPlgM05) + ExtractFileName(fn), mtError, [mbOK], 0 );

end; // ConfigurePlugin


type
  TOutDataToReturn = (odNone, odSelection, odAll);

function ExecutePlugin( fn : string ) : longint;
var
  Plugin : TPlugin;
  hDLLInst : THandle;
  KNTPluginExecute : KNTPluginExecuteProc;
  KNTPluginCleanup : KNTPluginCleanupProc;
  KNTSetPluginID : KNTSetPluginIDProc;
  Editor: TKntRichEdit;
  OutData : AnsiString;                            //  For compatibility, to be changed..
  Indata : Pointer;
  s, tmpstr : AnsiString;                          // ,,
  InsertPluginOutput, NoteWasReadOnly : boolean;
  LoadedPlugin : TLoadedPlugin;
  PluginReceivedSelection : boolean;
  OutDataToReturn: TOutDataToReturn;
  FolderName: string;


begin
  with Form_Main do begin
      result := 0;
      if not App.CheckActiveEditor then exit;
      Editor:= ActiveEditor;

      if Editor.ReadOnly and not assigned(ActiveFolder) then exit;

      if assigned(ActiveFolder) then
         FolderName:= ActiveFolder.Name
      else
         FolderName:= '';


      PluginReceivedSelection := false;

      if ( fn = '' ) then begin
         if ( not CheckResourcePanelVisible( true )) then exit;
         Plugin := GetSelectedPlugin;
         if ( not assigned( Plugin )) then begin
             MessageDlg( GetRS(sPlgM06), mtError, [mbOK], 0 );
             exit;
         end;
         fn := Plugin.Filename;
      end;

      if ( pos('\', fn ) = 0 ) then
         fn := Plugin_Folder + fn;

      if ( not fileexists( fn )) then begin
          MessageDlg( Format( GetRS(sPlgM07), [fn] ), mtError, [mbOK], 0 );
          exit;
      end;

      Plugin := TPlugin.Create;
      Plugin.Version := 0; // must be set to 1 via KNTGetPluginVersion

      Plugin.FileName := fn;
      LastPluginFN := fn;

      MMToolsPluginRunLast.Hint := Format(GetRS(sPlgM08),  [ExtractFileName( LastPluginFN )] );

      InsertPluginOutput := false;
      NoteWasReadOnly := false;

      try

        if (not GetPluginInfo(fn, Plugin.Name, Plugin.Version, Plugin.Info, Plugin.Features )) then begin
            MessageDlg( Format(GetRS(sPlgM09), [ExtractFileName( fn )] ), mtError, [mbOK], 0 );
            exit;
        end;

        if (Plugin.Version <> 1) then begin
            MessageDlg( Format(GetRS(sPlgM10), [Plugin.Name, ExtractFileName( fn ), Plugin.Version] ), mtError, [mbOK], 0 );
            exit;
        end;


        if (not (plOK in Plugin.Features)) then begin
            MessageDlg( Format(GetRS(sPlgM11), [Plugin.Name, ExtractFileName( fn )] ), mtError, [mbOK], 0 );
            exit;
        end;

        hDLLInst := LoadLibrary(PChar(FN ));
        if (hDLLInst <= 0) then begin
            MessageDlg( Format(GetRS(sPlgM12), [Plugin.Name, ExtractFileName( fn )] ), mtError, [mbOK], 0 );
            exit;
        end;

        try
          try
            // get pointer to execute proc
            @KNTPluginExecute := GetProcAddress( hDLLInst, 'KNTPluginExecute' );
            if ( not assigned( KNTPluginExecute )) then begin
               MessageDlg( Format(GetRS(sPlgM13), [Plugin.Name, ExtractFileName( fn ) ] ), mtError, [mbOK], 0 );
               exit;
            end;

            // get pointer to cleanup proc
            @KNTPluginCleanup := GetProcAddress( hDLLInst, 'KNTPluginCleanup' );

            OutData := '';
            InData := nil;

            // if plugin is resident, see if it is already running and prevent from starting another instance of the same plugin
            if ( plStaysResident in Plugin.Features ) then begin
                if ( Loaded_Plugins.IndexOf( Plugin.Name ) >= 0 ) then begin
                   MessageDlg( Format(GetRS(sPlgM14), [Plugin.Name]), mtInformation, [mbOK], 0 );
                   exit;
                end;
            end;

            if ( plStaysResident in Plugin.Features ) then begin
                inc( Resident_Plugin_Counter );
                @KNTSetPluginID := GetProcAddress( hDLLInst, 'KNTSetPluginID' );
                if assigned( KNTSetPluginID ) then
                   // assign D to plugin, so that we can recognize it when it shuts down and free the DLL properly
                   KNTSetPluginID( Resident_Plugin_Counter )
                else begin
                   // cannot allow plugin to go resident, because we won't be able to identify it when it shuts down
                   MessageDlg( Format(GetRS(sPlgM15), [Plugin.Name]), mtError, [mbOK], 0 );
                   exit;
                end;
            end;

            // Figure out what data type the plugin expects to receive
            OutDataToReturn:= odNone;

            if ( plGetsData in Plugin.Features ) then begin
              if ( plGetsSelection in Plugin.Features ) then begin
                  if ( Editor.SelLength > 0 ) then
                     OutDataToReturn := odSelection          // plugin will get selected text
                  else begin
                      // no text is selected in active note
                      if ( plNeedsSelection in Plugin.Features ) then begin
                          MessageDlg( Format(GetRS(sPlgM16), [Plugin.Name]), mtInformation, [mbOK], 0 );
                          exit;                             // selection is required, so we must abort
                      end
                      else
                          OutDataToReturn := odAll;         // selection is preferred but not required, so plugin gets all text
                  end;
              end
              else
                  OutDataToReturn := odAll;                 // plugin does not want selection, so it gets all text
            end;

            if OutDataToReturn = odSelection then begin
               PluginReceivedSelection := true;
               if ( plGetsRTF in Plugin.Features ) then
                  OutData := Editor.RtfSelText
               else
                  OutData := Editor.SelText;
            end
            else if OutDataToReturn = odAll then begin
                PluginReceivedSelection := false;
                if ( plGetsRTF in Plugin.Features ) then
                   OutData := Editor.RtfText
                else
                   OutData := Editor.Text;
            end;

            if OutData <> '' then begin
               if ( plGetsRTF in Plugin.Features ) then
                  OutData:= RemoveKNTHiddenCharactersInRTF (OutData, hmAll)
               else
                  OutData:= RemoveKNTHiddenCharactersInText (OutData);
            end;


            try
              with ActiveNNode do
                 tmpstr := Name;
            except
                tmpstr := FolderName
            end;


            result := KNTPluginExecute(
                        Application.Handle,
                        Form_Main.Handle,
                        Editor.Handle,
                        PAnsiChar(AnsiString(ActiveFile.FileName)),
                        PAnsiChar(AnsiString(tmpstr)),
                        PAnsiChar(OutData),
                        InData );

            try

              if ( plStaysResident in Plugin.Features ) then begin
                  // store the dll handle, so that we can unload the plugin later
                  LoadedPlugin := TLoadedPlugin.Create;
                  with LoadedPlugin do begin
                     DllInstance := hDllInst;
                     ID := Resident_Plugin_Counter;
                     ExecResult := result;
                  end;
                  Loaded_Plugins.AddObject( Plugin.Name, LoadedPlugin );
              end;

              if (result < 0) then begin
                  MessageDlg( Format(GetRS(sPlgM17),[result] ), mtWarning, [mbOK], 0 );
                  exit;
              end;


              if (plStaysResident in Plugin.Features) then
                 StatusBar.Panels[PANEL_HINT].Text:= Format(GetRS(sPlgM18),[ExtractFileName( Plugin.Filename )] )

              else begin

                  if (plReturnsData in Plugin.Features ) and (result > 0) then begin
                    // transfer data received from plugin
                    if ( not( plReturnsClipboard in Plugin.Features )) then begin
                       setlength( s, result );
                       move( InData^, s[1], result );
                    end;

                    if ( plWantsDlgBox in Plugin.Features ) then begin
                        InsertPluginOutput := Application.MessageBox(
                          PChar(string( copy(s, 1, _MAX_DLGBOX_MSG_LEN))),
                          PChar(Plugin.Name),
                          MB_OKCANCEL+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL
                        ) = ID_OK;
                    end
                    else
                    if ( plReturnsClipboard in Plugin.Features ) then begin
                        InsertPluginOutput := ( KeyOptions.AutoPastePlugin or
                          ( Application.MessageBox(
                            PChar(GetRS(sPlgM19)),
                            PChar( Plugin.Name ),
                            MB_OKCANCEL+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL
                          ) = ID_OK ));
                    end
                    else
                       InsertPluginOutput := true;
                  end;

                  // check for read-only note
                  if InsertPluginOutput then begin
                     NoteWasReadOnly := Editor.ReadOnly;
                     if NoteWasReadOnly then
                        InsertPluginOutput:= (MessageDlg(Format(GetRS(sPlgM20),[ActiveNNode.NoteName]), mtWarning, [mbOK, mbCancel], 0 ) = mrOK);
                  end;

                  if InsertPluginOutput then begin
                      if NoteWasReadOnly then
                         ActiveFolder.ReadOnly := false;       // ToDO: Contemplar que cada nota pueda ser ReadOnly individualmente
                      try
                        if Editor.SupportsRegisteredImages then begin
                           if Editor.SelLength > 0 then
                              Editor.CheckToSelectLeftImageHiddenMark;
                        end;

                        if ( plReturnsClipboard in Plugin.Features ) then
                           Editor.PasteBestAvailableFormat (FolderName, false)
                        else
                        if ( plReturnsRTF in Plugin.Features ) then
                           Editor.PutRtfText(s, true)
                        else begin
                           Editor.SelText := s;
                           Editor.SelLength:= 0;
                        end;

                      finally
                         if NoteWasReadOnly then
                            ActiveFolder.ReadOnly := true;
                      end;
                  end;

              end;


            finally
              // KNTPluginCleanup is guaranteed to run if KNTPLuginExecute ran
              try
                if ( assigned( KNTPluginCleanup ) and ( not ( plStaysResident in Plugin.Features ))) then
                   KNTPluginCleanup;
              except
                On E : Exception do
                   MessageDlg( GetRS(sPlgM21) + E.Message, mtError, [mbOK], 0 );
              end;

            end;

          except
              On E : Exception do begin
                 MessageDlg( GetRS(sPlgM22) + E.Message, mtError, [mbOK], 0 );
                 exit;
              end;
          end;

        finally
          if (not ( plStaysResident in Plugin.Features)) then
            FreeLibrary( hDLLInst );
        end;

      finally
         Plugin.Free;
      end;
  end;

end; // ExecutePlugin


function GetPluginIconIndex( const Plugin : TPlugin ) : integer;
begin
  result := PLUGIN_IMAGE_BASE;
  if ( plStaysResident in Plugin.Features ) then
     inc( result );
end;


procedure UnloadResidentPlugin( const ID : longint );
var
  LoadedPlugin : TLoadedPlugin;
  i, p : integer;
  DLLInstance : THandle;

begin
  LoadedPlugin := nil;
  DLLInstance := 0;
  p := -1;

  for i := 1 to Loaded_Plugins.Count do  begin
     LoadedPlugin := TLoadedPlugin( Loaded_Plugins.Objects[pred( i )] );
     if ( LoadedPlugin.ID = ID ) then begin
        DLLInstance := LoadedPlugin.DllInstance;
        p := pred( i );
        break;
     end;
  end;


  if (DLLInstance <> 0) then begin
    try
      try
        FreeLibrary( DLLInstance );
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format(GetRS(sPlgM23), [Loaded_Plugins[p]] );

      except
        on E : Exception do
          MessageDlg(Format(GetRS(sPlgM24), [Loaded_Plugins[p],E.Message]), mtError, [mbOK], 0 );
      end;

    finally
      Loaded_Plugins.Objects[p].Free;
      Loaded_Plugins.Delete( p );
    end;
  end;

end; // UnloadResidentPlugin


initialization
  Resident_Plugin_Counter:=0;
  LastPluginFN := '';

end.
