unit kn_ConfigFileMng;

interface

    // config file management
    procedure ReadCmdLine;
    procedure SaveOptions;
    procedure ReadOptions;
    procedure LoadToolbars;
    procedure SaveToolbars;
    procedure SaveDefaults;
    procedure LoadDefaults;
    procedure ReadFuncKeys;
    procedure SaveFuncKeys;
    procedure AdjustOptions;
//    procedure SetupToolbarButtons;
//    procedure ResolveToolbarRTFv3Dependencies;
    function LoadCustomKeyboard : boolean;
    procedure CustomizeKeyboard;


implementation

uses
   Windows, Forms, Classes, Controls, Dialogs, Menus, IniFiles, SysUtils,
   TB97Ctls, TB97Tlbr,
   kn_Main, dll_Keyboard, kn_Dllmng, kn_DLLinterface, gf_files,
   kn_global, kn_Info, kn_INI, kn_Const, kn_OptionsNew, kn_Chest,
   kn_FindReplaceMng, kn_MacroMng, kn_VCLControlsMng, kn_LanguagesMng,
   TntSysUtils;

resourcestring
  STR_KeybdError = 'Error in keyboard customization procedure: ';
  STR_TabIcons = ' Customize Tab icons (%s) ';


procedure ReadCmdLine;
var
  i : integer;
  s, ext, errstr : string;
begin
  errstr := '';
  for i := 1 to ParamCount do
  begin
    s := ansilowercase( ParamStr( i ));
    case s[1] of
      '-', '/' : begin // assume switch { BUG: if a filename begins with '-', we're screwed }
        delete( s, 1, 1 );


        if ( s = swMinimize ) then
          opt_Minimize := true
        else
        if ( s = swSetup ) then
          opt_Setup := true
        else
        if ( s = swDebug ) then
          opt_Debug := true
        else
        if ( s = swNoReadOpt ) then
          opt_NoReadOpt := true
        else
        if ( s = swNoSaveOpt ) then
          opt_NoSaveOpt := true
        else
        if ( s = swNoDefaults ) then
          opt_NoDefaults := true
        else
        if ( s = swNoReg ) then
          opt_NoRegistry := true
        else
        if ( s = swRegExt ) then
          opt_RegExt := true
        else
        if ( s = swSaveDefIcn ) then
          opt_SaveDefaultIcons := true
        else
        if ( s = swSaveToolbars ) then
          opt_SaveToolbars := true
        else
        if ( s = swSaveMenus ) then
          opt_SaveMenus := true
        else
        if ( s = swNoUserIcn ) then
          opt_NoUserIcons := true
        else
        if ( s = swUseOldFormat ) then
          _USE_OLD_KEYNOTE_FILE_FORMAT := true // GLOBAL var, used by TTabNote and TNoteFile
        else
        if ( s = swClean ) then
          opt_Clean := true
        else
        begin
          errstr := errstr + #13 + ParamStr( i );
        end;
      end;
      else
      begin
        // not a switch, so it's a filename.
        // Let's see what kind of file.
        ext := extractfileext( s );
        s:= GetAbsolutePath(WideExtractFilePath(Application.ExeName), s);
        if (( ext = ext_KeyNote ) or ( ext = ext_Encrypted ) or ( ext = ext_DART )) then
        begin
          NoteFileToLoad := s;
        end
        else
        if ( ext = ext_INI ) then
        begin
          INI_FN := s;
          opt_NoRegistry := true;
        end
        else
        if ( ext = ext_ICN ) then
        begin
          ICN_FN := s;
        end
        else
        if ( ext = ext_DEFAULTS ) then
        begin
          DEF_FN := s;
        end
        else
        if ( ext = ext_MGR ) then
        begin
          MGR_FN := s;
        end
        else
        if ( ext = ext_Macro ) then
        begin
          StartupMacroFile := s;
          CmdLineFileName := s; // will be passed to other instance
        end
        else
        if ( ext = ext_Plugin ) then
        begin
          StartupPluginFile := s;
          CmdLineFileName := s; // will be passed to other instance
        end
        else
          NoteFileToLoad := s;
      end;
    end;
  end;
  if ( errstr <> '' ) then
  begin
    messagedlg( 'Invalid command line arguments:' + #13 + errstr, mtWarning, [mbOK], 0 );
  end;
end; // ReadCmdLine

procedure ReadFuncKeys;
var
  IniFile : TMemIniFile;
  section : string;
  i : integer;
begin
  // load function key assignments.
  // This is rudimentary stuff.
  // to customize, run FuncKey plugin.

  if opt_NoReadOpt then exit;
  if ( not fileexists( KEY_FN )) then exit;

  IniFile := TMemIniFile.Create( KEY_FN );

  try
    with IniFile do
    begin
      section := 'Alt';
      for i := 1 to 12 do
        AltFKeys[i] := readstring( section, inttostr( i ), '' );
      section := 'ShiftAlt';
      for i := 1 to 12 do
        ShiftAltFKeys[i] := readstring( section, inttostr( i ), '' );
      section := 'CtrlAlt';
      for i := 1 to 12 do
        CtrlAltFKeys[i] := readstring( section, inttostr( i ), '' );
    end;
  finally
    IniFile.Free
  end;

end; // ReadFuncKeys

procedure SaveFuncKeys;
var
  IniFile : TMemIniFile;
  section : string;
  i : integer;
begin
  if opt_NoSaveOpt then exit;

  IniFile := TMemIniFile.Create( KEY_FN );

  try
    with IniFile do
    begin
      section := 'Alt';
      for i := 1 to 12 do
        if ( AltFKeys[i] <> '' ) then
          writestring( section, inttostr( i ), AltFKeys[i] );
      section := 'ShiftAlt';
      for i := 1 to 12 do
        if ( ShiftAltFKeys[i] <> '' ) then
          writestring( section, inttostr( i ), ShiftAltFKeys[i] );
      section := 'CtrlAlt';
      for i := 1 to 12 do
        if ( CtrlAltFKeys[i] <> '' ) then
          writestring( section, inttostr( i ), CtrlAltFKeys[i] );
    end;
    IniFile.UpdateFile;
  finally
    IniFile.Free
  end;

end; // SaveFuncKeys

procedure SaveOptions;
begin
  if opt_NoSaveOpt then exit;
  try
    SaveKeyNoteOptions( INI_FN,
      KeyOptions,
      TabOptions,
      FindOptions,
      EditorOptions,
      ClipOptions,
      TreeOptions,
      ResPanelOptions
      );
  except
  end;
end; // SaveOptions

procedure ReadOptions;
begin
  if opt_NoReadOpt then exit;
  if ( not fileexists( INI_FN )) then
  begin
    FirstTimeRun := true;
    exit;
  end;
  LoadKeyNoteOptions( INI_FN,
    KeyOptions,
    TabOptions,
    FindOptions,
    EditorOptions,
    ClipOptions,
    TreeOptions,
    ResPanelOptions
    );
end; // ReadOptions

procedure SaveDefaults;
begin
  if opt_NoDefaults then exit;

  SaveKeyNoteDefaults(
    DEF_FN,
    DefaultEditorProperties,
    DefaultEditorChrome,
    DefaultTabProperties,
    DefaultTreeProperties,
    DefaultTreeChrome
  );
end; // SaveDefaults

procedure LoadDefaults;
begin
  if ( opt_NoDefaults or ( not fileexists( DEF_FN ))) then exit;

  try

    LoadKeyNoteDefaults(
      false,
      DEF_FN,
      DefaultEditorProperties,
      DefaultEditorChrome,
      DefaultTabProperties,
      DefaultTreeProperties,
      DefaultTreeChrome
    );

  except
    on E : Exception do
    begin
      showmessage( 'There was a non-fatal error while loading defaults: ' + #13 +
        E.Message + #13#13 +
        'Some settings may have been reset to defaults.' );
    end;
  end;
end; // LoadDefaults


procedure SaveToolbars;
var
  IniFile : TMemIniFile;
  section : string;
  i, cnt : integer;
  tb : TToolbarButton97;
  ts : TToolbarSep97;
begin
  if opt_NoSaveOpt then exit;
  IniFile := TMemIniFile.Create( Toolbar_FN );
  try
    try
      with IniFile do
      begin
       with Form_Main do
       begin
        section := 'MainToolbar';
        cnt := pred( Toolbar_Main.ControlCount );
        for i := 0 to cnt do
        begin
          if ( Toolbar_Main.Controls[i] is TToolbarButton97 ) then
          begin
            tb := ( Toolbar_Main.Controls[i] as TToolbarButton97 );
            writebool( section, tb.name, tb.Visible );
          end
          else
          if ( Toolbar_Main.Controls[i] is TToolbarSep97 ) then
          begin
            ts := ( Toolbar_Main.Controls[i] as TToolbarSep97 );
            writebool( section, ts.name, ts.Visible );
          end
        end;

        section := 'FormatToolbar';
        cnt := pred( Toolbar_Format.ControlCount );
        for i := 0 to cnt do
        begin
          if ( Toolbar_Format.Controls[i] is TToolbarButton97 ) then
          begin
            tb := ( Toolbar_Format.Controls[i] as TToolbarButton97 );
            writebool( section, tb.name, tb.Visible );
          end
          else
          if ( Toolbar_Format.Controls[i] is TToolbarSep97 ) then
          begin
            ts := ( Toolbar_Format.Controls[i] as TToolbarSep97 );
            writebool( section, ts.name, ts.Visible );
          end
        end;

        section := 'Special';
        writebool( section, 'FontNameCombo', Combo_Font.Visible );
        writebool( section, 'FontSizeCombo', Combo_FontSize.Visible );
        writebool( section, 'ZoomCombo', Combo_Zoom.Visible );
        writebool( section, 'FontColorButton', TB_Color.Visible );
        writebool( section, 'FontHighlightButton', TB_Hilite.Visible );

      end;
     end;
     IniFile.UpdateFile;
    except
    end;
  finally
    IniFile.Free;
  end;

end; // SaveToolbars

procedure LoadToolbars;
var
  IniFile : TMemIniFile;
  section, compname : string;
  list : TStringList;
  i, cnt : integer;
  myC : TComponent;
begin
  if ( opt_NoReadOpt or ( not fileexists( Toolbar_FN ))) then exit;

  IniFile := TMemIniFile.Create( Toolbar_FN );
  list := TStringList.Create;

  try
    try
      with IniFile do
      begin
        section := 'MainToolbar';
        readsection( section, list );
        cnt := pred( list.Count );
        for i := 0 to cnt do
        begin
          compname := list[i];
          myC := Form_Main.findcomponent( compname );
          if assigned( myC ) then
          begin
            if ( myC is TToolbarButton97 ) then
              ( myC as TToolbarButton97 ).Visible := ReadBool( section, compname, true )
            else
            if ( myC is TToolbarSep97 ) then
              ( myC as TToolbarSep97 ).Visible := ReadBool( section, compname, true );
          end;
        end;

        list.Clear;
        section := 'FormatToolbar';
        readsection( section, list );
        cnt := pred( list.Count );
        for i := 0 to cnt do
        begin
          compname := list[i];
          myC := Form_Main.findcomponent( compname );
          if assigned( myC ) then
          begin
            if ( myC is TToolbarButton97 ) then
              ( myC as TToolbarButton97 ).Visible := ReadBool( section, compname, true )
            else
            if ( myC is TToolbarSep97 ) then
              ( myC as TToolbarSep97 ).Visible := ReadBool( section, compname, true );
          end;
        end;

        section := 'Special';
        with Form_Main do
        begin
          Combo_Font.Visible := readbool( section, 'FontNameCombo', true );
          Combo_FontSize.Visible := readbool( section, 'FontSizeCombo', true );
          Combo_Zoom.Visible := readbool( section, 'ZoomCombo', true );
          TB_Color.Visible := readbool( section, 'FontColorButton', true );
          TB_Hilite.Visible := readbool( section, 'FontHighlightButton', true );
        end;
      end;
    except
    end;
  finally
    IniFile.Free;
    list.Free;
  end;

end; // LoadToolbars

function LoadCustomKeyboard : boolean;
var
  IniFile : TMemIniFile;
  itemname, keyname : string;
  KeyList : TStringList;
  i, cnt, keyvalue : integer;
  menusection : TKeyMenuCategory;
  myMenuItem : TMenuItem;
begin
  result := false;
  if ( opt_NoReadOpt or ( not fileexists( Keyboard_FN ))) then exit;

  IniFile := TMemIniFile.Create( Keyboard_FN );
  KeyList := TStringList.Create;
  try
    try
      with IniFile do
      begin
        for menusection := low( KeyboardConfigSections ) to high( KeyboardConfigSections ) do
        begin

          Keylist.Clear;
          readsectionvalues( KeyboardConfigSections[menusection], KeyList );   // At this file this problem doesn't affect: TMemIniFile Doesn't Handle Quoted Strings Properly (http://qc.embarcadero.com/wc/qcmain.aspx?d=4519)

          cnt := KeyList.Count;
          for i := 1 to cnt do
          begin
            itemname := KeyList.Names[pred( i )];
            keyname := KeyList.Values[itemname];
            if ( keyname <> '' ) then
            begin
              try
                keyvalue := strtoint( keyname );
              except
                keyvalue := 0;
              end;

              // Don't allow shortcuts CTR-C, Ctrl-V, Ctrl-X. This combinations will be managed indepently
              if (keyvalue = 16451) or (keyvalue=16470) or (keyvalue = 16472) then
                 keyvalue:= 0;

              myMenuItem := TMenuItem( Form_Main.FindComponent( itemname ));
              if assigned( myMenuItem ) then
                 myMenuItem.ShortCut := keyvalue;
            end;
          end;
        end;
      end;
    except
      on E : Exception do
      begin
        messagedlg( Format(
          'Error while loading custom keyboard configuration from %s: "%s"',
          [Keyboard_FN, E.Message] ), mtError, [mbOK], 0 );
      end;
    end;
  finally
    IniFile.Free;
    KeyList.Free;
  end;

end; // LoadCustomKeyboard

procedure CustomizeKeyboard;
var
  KeyList : TList;
  KeyCustomMenus : TKeyCustomMenus;
  DlgCustomizeKeyboard : DlgCustomizeKeyboardProc;
  // DlgAboutKeyNote : DlgAboutKeyNoteProc;
  DllHandle : THandle;
begin

  DllHandle := ObtainDLLHandle;
  if ( DllHandle <= 0 ) then exit;

  //Restore these shortcuts momentarily to show them in the configuration screen
  if TMenuItem( Form_Main.FindComponent( 'MMEditPaste' )).ShortCut = 0 then
     TMenuItem( Form_Main.FindComponent( 'MMEditPaste' )).ShortCut:= ShortCut(Ord('V'), [ssCtrl]); // 16470;
  if TMenuItem( Form_Main.FindComponent( 'MMEditCopy' )).ShortCut = 0 then
     TMenuItem( Form_Main.FindComponent( 'MMEditCopy' )).ShortCut:= ShortCut(Ord('C'), [ssCtrl]); // 16451;
  if TMenuItem( Form_Main.FindComponent( 'MMEditCut' )).ShortCut = 0 then
     TMenuItem( Form_Main.FindComponent( 'MMEditCut' )).ShortCut:= ShortCut(Ord('X'), [ssCtrl]);  // 16472;

  KeyCustomMenus[ckmMain] := Form_Main.Menu_Main;
  KeyCustomMenus[ckmTree] := Form_Main.Menu_TV;

  KeyList := TList.Create;
  try
    try
      BuildKeyboardList( KeyCustomMenus, KeyList );

      @DlgCustomizeKeyboard := GetProcAddress( DllHandle, 'DlgCustomizeKeyboard' );
      if ( not assigned( DlgCustomizeKeyboard )) then
      begin
        DllProcNotFoundMsg( 'DlgCustomizeKeyboard' );
        exit;
      end;

      if DlgCustomizeKeyboard(
        Application.Handle,
        PChar( Keyboard_FN ),
        KeyList,
        KeyOptions.HotKey ) then
      begin
        screen.Cursor := crHourGlass;
        try
          LoadCustomKeyboard;
        finally
          screen.Cursor := crDefault;
        end;
      end;

      TMenuItem( Form_Main.FindComponent( 'MMEditPaste' )).ShortCut := 0;
      TMenuItem( Form_Main.FindComponent( 'MMEditCopy' )).ShortCut := 0;
      TMenuItem( Form_Main.FindComponent( 'MMEditCut' )).ShortCut := 0;

    except
      on E : Exception do
      begin
        messagedlg( STR_KeybdError + E.Message, mtError, [mbOK], 0 );
      end;
    end;
  finally
    FreeLibrary( DllHandle );
    ClearObjectList( KeyList );
    KeyList.Free;
  end;

end; // CustomizeKeyboard

procedure AdjustOptions;
var
  Form_Options : TForm_OptionsNew;
  tmpicnfn : string;
  oldHotKey : Word;
  oldLanguageUI : string;
  FN: string;
begin
  with Form_Options do
  begin
      Form_Options := TForm_OptionsNew.Create( Form_Main );
      try
          myOpts := KeyOptions;
          myTabOpts := TabOptions;
          myClipOpts := ClipOptions;
          myTreeOpts := TreeOptions;
          myEditorOptions := EditorOptions;
          myTreeOptions := TreeOptions;
          myFindOpts := FindOptions;
          ShowHint := KeyOptions.ShowTooltips;

          Icons_Change_Disable :=
            ( opt_NoUserIcons or
            ( assigned( NoteFile ) and ( NoteFile.TabIconsFN = _NF_Icons_BuiltIn )));

          if ( not Icons_Change_Disable ) then
          begin
            tmpicnfn := extractfilename( ICN_FN );
            if assigned( NoteFile ) then
            begin
              if (( NoteFile.TabIconsFN <> _NF_Icons_BuiltIn ) and
                 ( NoteFile.TabIconsFN <> '' )) then
                tmpicnfn := extractfilename( NoteFile.TabIconsFN );
            end;
            GroupBox_ICN.Caption :=
              Format( STR_TabIcons, [tmpicnfn] );
          end;

        if ( Form_Options.ShowModal = mrOK ) then
        begin
          screen.Cursor := crHourGlass;
          try

            oldHotKey := KeyOptions.HotKey; // save previous state
            oldLanguageUI := KeyOptions.LanguageUI;

            KeyOptions := Form_Options.myOpts;
            TabOptions := Form_Options.myTabOpts;
            ClipOptions := Form_Options.myClipOpts;
            TreeOptions := Form_Options.myTreeOpts;
            EditorOptions := Form_Options.myEditorOptions;
            TreeOptions := Form_Options.myTreeOptions;
            FindOptions := Form_Options.myFindOpts;

            // update hotkey only if settings changed
            if (( HotKeySuccess <> KeyOptions.HotKeyActivate ) or ( KeyOptions.HotKey <> oldHotKey )) then
            begin
              Form_Main.HotKeyProc( false );
              if KeyOptions.HotKeyActivate then
                Form_Main.HotKeyProc( true );
            end;

            if Form_Options.Icons_Changed then
            begin
              // icons were changed, save them
              if assigned( NoteFile ) then
              begin
                if ( NoteFile.TabIconsFN = '' ) then
                  SaveCategoryBitmapsUser( ICN_FN )
                else
                  SaveCategoryBitmapsUser( NoteFile.TabIconsFN );
              end
              else
              begin
                SaveCategoryBitmapsUser( ICN_FN );
              end;
            end;

            if oldLanguageUI <> KeyOptions.LanguageUI then
               ApplyLanguageUI (KeyOptions.LanguageUI);

            SaveOptions;
            with Form_Main do
            begin
             UpdateFormState;
             UpdateTabState;
             UpdateStatusBarState;
             UpdateResPanelState;
            end;

            if ( assigned( NoteFile ) and assigned( NoteFile.ClipCapNote )) then
            begin
              LoadTrayIcon( ClipOptions.SwitchIcon );
            end;

          finally
            screen.Cursor := crDefault;
          end;
        end
        else
        begin
          if Form_Options.Icon_Change_Canceled then
          begin
            LoadTabImages( true );
          end;
        end;
      finally
        Form_Options.Free;
      end;
  end;

end; // AdjustOptions

end.
