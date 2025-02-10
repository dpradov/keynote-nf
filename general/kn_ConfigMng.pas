unit kn_ConfigMng;

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
   System.Classes,
   System.IniFiles,
   System.SysUtils,
   Vcl.Forms,
   Vcl.Controls,
   Vcl.Dialogs,
   Vcl.Menus,
   TB97Ctls,
   TB97Tlbr
   ;

    // config management
    procedure ReadCmdLine;
    function CheckCallArgs(const Args: string; FromKntLauncher: boolean): boolean;
    procedure SaveOptions;
    procedure ReadOptions;
    procedure LoadToolbars;
    procedure SaveToolbars;
    procedure SaveDefaults;
    procedure LoadDefaults;
    procedure AdjustOptions;
//    procedure SetupToolbarButtons;
//    procedure ResolveToolbarRTFv3Dependencies;
    function LoadCustomKeyboard : boolean;
    procedure CustomizeKeyboard;


implementation

uses
   gf_files,  // Important. Needed (among other things) to use TMemIniFileHelper (.ReadString, .WriteString)
   gf_strings,
   kn_Info,
   kn_INI,
   kn_Const,
   kn_global,
   kn_Chest,
   kn_OptionsNew,
   dll_Keyboard,
{$IFDEF EMBED_UTILS_DLL}
   dll_Main,
{$ELSE}
   kn_Dllmng,
   kn_DLLinterface,
{$ENDIF}
   kn_Macro,
   kn_Plugins,
   kn_StyleObj,
   kn_LocationObj,
   kn_LanguagesMng,
   kn_MacroMng,
   kn_VCLControlsMng,
   kn_TemplateMng,
   kn_PluginsMng,
   kn_LinksMng,
   kn_Main,
   knt.ui.editor,
   knt.App,
   knt.RS
   ;


procedure ReadCmdLine;
var
  i : integer;
  s, ext, errstr : string;
begin

  errstr := '';
  for i := 1 to ParamCount do  begin
    s := AnsiLowerCase( ParamStr( i ));

    case s[1] of
       '-', '/' : begin // assume switch { BUG: if a filename begins with '-', we're screwed }
          delete( s, 1, 1 );


          if ( s = swMinimize ) then
             App.opt_Minimize := true
          else
          {if ( s = swSetup ) then
             App.opt_Setup := true
          else}
{$IFDEF KNT_DEBUG}
          if ( s.StartsWith(swDebug) ) then begin
             App.opt_Debug := true;
             delete( s, 1, swDebug.Length );
             if s <> '' then begin
                try
                   log.MaxDbgLevel:= StrToInt(s);
                except
                end;
             end
             else
                log.MaxDbgLevel:= 1;          /// default max dbg level = 1
          end
          else
{$ELSE}
          if ( s = swDebug ) then
             App.opt_Debug := true
          else
{$ENDIF}
          if ( s = swNoReadOpt ) then
             App.opt_NoReadOpt := true
          else
          if ( s = swNoSaveOpt ) then
             App.opt_NoSaveOpt := true
          else
          if ( s = swNoDefaults ) then
             App.opt_NoDefaults := true
          else
          if ( s = swNoReg ) then
             App.opt_NoRegistry := true
          else
          if ( s = swRegExt ) then
             App.opt_RegExt := true
          else
          if ( s = swSaveDefIcn ) then
             App.opt_SaveDefaultIcons := true
          else
          if ( s = swSaveToolbars ) then
             App.opt_SaveToolbars := true
          else
{$IFDEF KNT_DEBUG}
          if ( s = swSaveMenus ) then
             App.opt_SaveMenus := true
          else
{$ENDIF}
          if ( s = swNoUserIcn ) then
             App.opt_NoUserIcons := true
          else
          if ( s = swUseOldFormat ) then
             _USE_OLD_KEYNOTE_FILE_FORMAT := true // GLOBAL var, used by TTabNote and TKntFile
          else
          if ( s = swClean ) then
             App.opt_Clean := true
          else
          if  (s = swIgnoreSI) then
              _OTHER_INSTANCE_HANDLE:= 0
          else
          if  (s = swDoNotDisturb) then
              App.opt_DoNotDisturb:= true
          else
          if  (s.StartsWith(swTitle)) then            // -title"PROJECTX HELP" -> -titlePROJECTX HELP
              App.opt_Title:= Copy(ParamStr(i), Length(swTitle)+2)
          else
          if (s = swConvKNTLinks) then
              App.opt_ConvKNTLinks:= true
          else
          if ( s.StartsWith(swJmp) ) then begin
             // Jump to the KNT link indicated in quotes (in any of the recognized formats. Ex: "file:///*1|10|201|0")
             // Note: '-jmp"file:///*8|479|0|0"' is converted to '-jmpfile:///*8|479|0|0'
              _GLOBAL_URLText:= Copy(s, Length(swJmp)+1);
          end
          else
            errstr := errstr + #13 + ParamStr( i );

       end
       else begin
          // not a switch, so it's a filename.
          // Let's see what kind of file.
          ext := extractfileext(s);
          if ( ext = '.exe' ) then continue;

          s:= GetAbsolutePath(ExtractFilePath(Application.ExeName), ParamStr(i));

          if (( ext = ext_KeyNote ) or ( ext = ext_Encrypted ) or ( ext = ext_DART )) then
             KntFileToLoad := s
          else
          if ( ext = ext_INI ) then begin
             INI_FN := s;
             App.opt_NoRegistry := true;
          end
          else
          if ( ext = ext_ICN ) then
             ICN_FN := s
          else
          if ( ext = ext_DEFAULTS ) then
             DEF_FN := s
          else
          if ( ext = ext_MGR ) then
              MGR_FN := s
          else
          if ( ext = ext_Macro ) then begin
             StartupMacroFile := s;
             CmdLineFileName := s; // will be passed to other instance
          end
          else
          if ( ext = ext_Plugin ) then begin
             StartupPluginFile := s;
             CmdLineFileName := s; // will be passed to other instance
          end
          else
             KntFileToLoad := s;

       end;

    end;
  end;

  if (errstr <> '' ) then
      MessageDlg( GetRS(sCfg03) + #13 + errstr, mtWarning, [mbOK], 0 );

end; // ReadCmdLine




function CheckCallArgs(const Args: string; FromKntLauncher: boolean): boolean;
var
  i : integer;
  s, ext, FN: string;
  JmpLocation : string;
  Strs: TStrings;
  Location: TLocation;
begin
  Result:= false;
  if Initializing then exit;

  Strs:= TStringList.Create;
  try
     CommandLineToStrings(Args, Strs);
     KntFileToLoad := '';
     JmpLocation:= '';
     StartupMacroFile:= '';
     StartupPluginFile := '';
     _GLOBAL_URLText:= '';

     for i := 1 to Strs.Count-1 do  begin
       s := AnsiLowerCase( Strs[i]);
       if s= '' then continue;
       UnquoteString(s);

       case s[1] of
          '-', '/' : begin
             delete( s, 1, 1 );
             if ( s.StartsWith(swJmp) ) then begin
                // Jump to the KNT link indicated in quotes (in any of the recognized formats. Ex: "file:///*1|10|201|0")
                // Note: '-jmp"file:///*8|479|0|0"' is converted to '-jmpfile:///*8|479|0|0'
                JmpLocation:= Copy(s, Length(swJmp)+1);
             end;
          end
          else begin
             // not a switch, so it's a filename. Let's see what kind of file.
             ext := extractfileext(s);
             if ( ext = '.exe' ) then continue;

             s:= GetAbsolutePath(ExtractFilePath(Application.ExeName), s);

             if ( ext = ext_INI ) or ( ext = ext_ICN ) or ( ext = ext_DEFAULTS ) or ( ext = ext_MGR ) then
                 continue
             else
             if ( ext = ext_KeyNote ) or ( ext = ext_Encrypted ) then
                KntFileToLoad := s
             else
             if ( ext = ext_Macro ) then
                StartupMacroFile := s
             else
             if ( ext = ext_Plugin ) then
                StartupPluginFile := s
             else
                KntFileToLoad := s;
          end;

       end;
     end;

  finally
     Strs.Free;
  end;

  FN:= ActiveFile.FileName.ToUpper;

  if FromKntLauncher then begin
     if (KntFileToLoad <> '') and (KntFileToLoad.ToUpper = FN) then
         result:= true
     else if (JmpLocation <> '') then begin
         Location:= BuildLocationFromKntURL(JmpLocation);
         if (Location.FileName.ToUpper = FN) then
             Result:= true;
         Location.Free;
     end;
  end
  else
     Result:= true;

  if Result then begin
     _GLOBAL_URLText:= JmpLocation;
     if FromKntLauncher then
        App.opt_DoNotDisturb:= true;
  end;

end; // CheckCallArgs




procedure SaveOptions;
begin
  if App.opt_NoSaveOpt then exit;

  try
    SaveKeyNoteOptions( INI_FN,
      KeyOptions,
      TabOptions,
      FindOptions,
      EditorOptions,
      ClipOptions,
      KntTreeOptions,
      ResPanelOptions
      );
  except
  end;
end; // SaveOptions


procedure ReadOptions;
begin
  if App.opt_NoReadOpt then exit;
  if ( not fileexists( INI_FN )) then begin
    FirstTimeRun := true;
    exit;
  end;

  LoadKeyNoteOptions( INI_FN,
    KeyOptions,
    TabOptions,
    FindOptions,
    EditorOptions,
    ClipOptions,
    KntTreeOptions,
    ResPanelOptions
    );
end; // ReadOptions


procedure SaveDefaults;
begin
  if App.opt_NoDefaults then exit;

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
  if ( App.opt_NoDefaults or ( not fileexists( DEF_FN ))) then exit;

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
      showmessage( Format(GetRS(sCfg05) , [E.Message]) );
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

  if App.opt_NoSaveOpt then exit;

  IniFile := TMemIniFile.Create( Toolbar_FN );

  try
    try
      with IniFile do begin
         with Form_Main do begin
            section := 'MainToolbar';
            cnt := pred( Toolbar_Main.ControlCount );
            for i := 0 to cnt do begin
              if ( Toolbar_Main.Controls[i] is TToolbarButton97 ) then begin
                 tb := ( Toolbar_Main.Controls[i] as TToolbarButton97 );
                 writebool( section, tb.name, tb.Visible );
              end
              else
              if ( Toolbar_Main.Controls[i] is TToolbarSep97 ) then begin
                 ts := ( Toolbar_Main.Controls[i] as TToolbarSep97 );
                 writebool( section, ts.name, ts.Visible );
              end
          end;

          section := 'FormatToolbar';
          cnt := pred( Toolbar_Format.ControlCount );
          for i := 0 to cnt do begin
              if ( Toolbar_Format.Controls[i] is TToolbarButton97 ) then begin
                 tb := ( Toolbar_Format.Controls[i] as TToolbarButton97 );
                 writebool( section, tb.name, tb.Visible );
              end
              else
              if ( Toolbar_Format.Controls[i] is TToolbarSep97 ) then begin
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
  if ( App.opt_NoReadOpt or ( not fileexists( Toolbar_FN ))) then exit;

  IniFile := TMemIniFile.Create( Toolbar_FN );
  list := TStringList.Create;

  try
    try
      with IniFile do begin
          section := 'MainToolbar';
          readsection( section, list );
          cnt := pred( list.Count );

          for i := 0 to cnt do begin
            compname := list[i];
            myC := Form_Main.findcomponent( compname );
            if assigned(myC) then begin
               if (myC is TToolbarButton97 ) then
                  (myC as TToolbarButton97 ).Visible := ReadBool( section, compname, true )
               else
               if (myC is TToolbarSep97 ) then
                  (myC as TToolbarSep97 ).Visible := ReadBool( section, compname, true );
            end;
          end;

          list.Clear;
          section := 'FormatToolbar';
          readsection( section, list );
          cnt := pred( list.Count );

          for i := 0 to cnt do begin
            compname := list[i];
            myC := Form_Main.findcomponent( compname );
            if assigned( myC ) then begin
               if ( myC is TToolbarButton97 ) then
                  (myC as TToolbarButton97 ).Visible := ReadBool( section, compname, true )
               else
               if ( myC is TToolbarSep97 ) then
                  (myC as TToolbarSep97 ).Visible := ReadBool( section, compname, true );
            end;
          end;

          section := 'Special';
          with Form_Main do begin
             Combo_Font.Visible := ReadBool( section, 'FontNameCombo', true );
             Combo_FontSize.Visible := ReadBool( section, 'FontSizeCombo', true );
             Combo_Zoom.Visible := ReadBool( section, 'ZoomCombo', true );
             TB_Color.Visible := ReadBool( section, 'FontColorButton', true );
             TB_Hilite.Visible := ReadBool( section, 'FontHighlightButton', true );
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
  itemname, keyname : String;
  KeyList : TStringList;
  i, cnt, keyvalue : integer;
  Category : TCommandCategory;
  myMenuItem : TMenuItem;
  kOC: TKeyOtherCommandItem;
  Group: TGroupCommand;
  IsMenu: boolean;

begin
  result := false;
  if ( App.opt_NoReadOpt or ( not fileexists( Keyboard_FN ))) then exit;

  IniFile := TMemIniFile.Create( Keyboard_FN );
  KeyList := TStringList.Create;
  ClearObjectList(App.Kbd.OtherCommandsKeys);

  try
    try

      with IniFile do begin

         for Category := low( TCommandCategory ) to high( TCommandCategory ) do begin
           Keylist.Clear;
           ReadSectionValues( KeyboardConfigSections[Category], KeyList );   // At this file this problem doesn't affect: TMemIniFile Doesn't Handle Quoted Strings Properly (http://qc.embarcadero.com/wc/qcmain.aspx?d=4519)

           IsMenu:= Category in [ccMenuMain .. ccMenuTree];

           cnt := KeyList.Count;
           for i := 0 to cnt -1 do begin
              itemname := KeyList.Names[i];
              keyname  := KeyList.Values[itemname];
              if keyname = '' then continue;

              keyvalue := StrToIntDef( keyname, 0);

               // Don't allow shortcuts CTR-C, Ctrl-V, Ctrl-X. This combinations will be managed indepently
               if (keyvalue = 16451) or (keyvalue=16470) or (keyvalue = 16472) then
                  keyvalue:= 0;

               if IsMenu then begin
                  myMenuItem := TMenuItem( Form_Main.FindComponent( itemname ));
                  if assigned( myMenuItem ) then
                     myMenuItem.ShortCut := keyvalue;
               end
               else if keyvalue <> 0 then begin
                  kOC:= TKeyOtherCommandItem.Create;
                  kOC.Name := itemname;
                  kOC.Category:= Category;
                  kOC.Shortcut := keyvalue;
                  App.Kbd.OtherCommandsKeys.Add(kOC);
               end;

           end;
         end;
      end;

    except
      on E : Exception do
        MessageDlg(Format(GetRS(sCfg04), [Keyboard_FN, E.Message] ), mtError, [mbOK], 0 );
    end;

  finally
    IniFile.Free;
    KeyList.Free;
  end;

end; // LoadCustomKeyboard


procedure BuildOtherCommandsList (const OtherCommandsList: TList);

  function GetCurrentShortCut(Category: TOtherCommandCategory; Command: string): TShortCut;
  var
     i: integer;
     Koc: TKeyOtherCommandItem;
  begin
       for i:= 0 to App.Kbd.OtherCommandsKeys.Count-1 do begin
          Koc:= App.Kbd.OtherCommandsKeys[i];
          if (Koc.Category = Category) and Koc.Name.Equals(Command) then
             Exit(Koc.Shortcut)
       end;
       exit(0);
  end;

  procedure CreateItems (Categ: TCommandCategory; Strs: TStrings);
  var
     i: integer;
     kOC: TKeyCommandItem;
  begin
      for i := 0 to Strs.Count - 1 do begin
         kOC:= TKeyCommandItem.Create;
         kOC.Category:= Categ;
         kOC.Name:= Strs[i];
         kOC.Caption:= kOC.Name;
         kOC.Path:= kOC.Name;
         if Categ = ccMacro then
            kOC.Hint:= TMacro( Strs.Objects[i] ).Description;
         kOC.Shortcut:= GetCurrentShortCut(Categ, kOC.Name);
         OtherCommandsList.Add(kOC);
      end;
  end;

begin
   EnumerateMacros;
   CreateItems(ccMacro, Macro_List);

   LoadTemplateList;
   CreateItems(ccTemplate, Form_Main.ListBox_ResTpl.Items);

   EnumeratePlugins;
   CreateItems(ccPlugin, Plugin_List);

   LoadStyleManagerInfo( Style_FN );
   CreateItems(ccStyle, StyleManager);

   CreateItems(ccFont, Form_Main.Combo_Font.Items);
end;


{$IFDEF EMBED_UTILS_DLL}

procedure CustomizeKeyboard;
var
  KeyList : TList;
  KeyCustomMenus : TKeyCustomMenus;
begin

  //Restore these shortcuts momentarily to show them in the configuration screen
  if TMenuItem( Form_Main.FindComponent( 'MMEditPaste' )).ShortCut = 0 then
     TMenuItem( Form_Main.FindComponent( 'MMEditPaste' )).ShortCut:= ShortCut(Ord('V'), [ssCtrl]); // 16470;
  if TMenuItem( Form_Main.FindComponent( 'MMEditCopy' )).ShortCut = 0 then
     TMenuItem( Form_Main.FindComponent( 'MMEditCopy' )).ShortCut:= ShortCut(Ord('C'), [ssCtrl]); // 16451;
  if TMenuItem( Form_Main.FindComponent( 'MMEditCut' )).ShortCut = 0 then
     TMenuItem( Form_Main.FindComponent( 'MMEditCut' )).ShortCut:= ShortCut(Ord('X'), [ssCtrl]);  // 16472;

  KeyCustomMenus[ccMenuMain] := Form_Main.Menu_Main;
  KeyCustomMenus[ccMenuTree] := Form_Main.Menu_TV;

  KeyList := TList.Create;
  try
    try
      BuildKeyboardList( KeyCustomMenus, KeyList );
      BuildOtherCommandsList (KeyList);

      if DlgCustomizeKeyboard(Keyboard_FN, KeyList, KeyOptions.HotKey) then begin
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
        messagedlg( GetRS(sCfg01) + E.Message, mtError, [mbOK], 0 );
    end;

  finally
    ClearObjectList( KeyList );
    KeyList.Free;
  end;

end; // CustomizeKeyboard


{$ELSE}

procedure CustomizeKeyboard;
var
  KeyList : TList;
  KeyCustomMenus : TKeyCustomMenus;
  DlgCustomizeKeyboard : DlgCustomizeKeyboardProc;
  // DlgAboutKeyNote : DlgAboutKeyNoteProc;
  DllHandle : THandle;
begin
  DllHandle:= 0;
  @DlgCustomizeKeyboard := GetMethodInDLL(DLLHandle, 'DlgCustomizeKeyboard');
  if not assigned(DlgCustomizeKeyboard) then exit;

  //Restore these shortcuts momentarily to show them in the configuration screen
  if TMenuItem( Form_Main.FindComponent( 'MMEditPaste' )).ShortCut = 0 then
     TMenuItem( Form_Main.FindComponent( 'MMEditPaste' )).ShortCut:= ShortCut(Ord('V'), [ssCtrl]); // 16470;
  if TMenuItem( Form_Main.FindComponent( 'MMEditCopy' )).ShortCut = 0 then
     TMenuItem( Form_Main.FindComponent( 'MMEditCopy' )).ShortCut:= ShortCut(Ord('C'), [ssCtrl]); // 16451;
  if TMenuItem( Form_Main.FindComponent( 'MMEditCut' )).ShortCut = 0 then
     TMenuItem( Form_Main.FindComponent( 'MMEditCut' )).ShortCut:= ShortCut(Ord('X'), [ssCtrl]);  // 16472;

  KeyCustomMenus[ccMenuMain] := Form_Main.Menu_Main;
  KeyCustomMenus[ccMenuTree] := Form_Main.Menu_TV;

  KeyList := TList.Create;
  try
    try
      BuildKeyboardList( KeyCustomMenus, KeyList );
      BuildOtherCommandsList (KeyList);

      if DlgCustomizeKeyboard(Application.Handle, PChar( Keyboard_FN ), KeyList, KeyOptions.HotKey) then begin
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
        messagedlg( GetRS(sCfg01) + E.Message, mtError, [mbOK], 0 );
    end;

  finally
    FreeLibrary( DllHandle );
    ClearObjectList( KeyList );
    KeyList.Free;
  end;

end; // CustomizeKeyboard

{$ENDIF}


procedure AdjustOptions;
var
  Form_Options : TForm_OptionsNew;
  tmpicnfn : string;
  oldHotKey : Word;
  oldLanguageUI : string;
  FN: string;
begin
  with Form_Options do begin
      Form_Options := TForm_OptionsNew.Create( Form_Main );
      try
          myOpts := KeyOptions;
          myTabOpts := TabOptions;
          myClipOpts := ClipOptions;
          myTreeOpts := KntTreeOptions;
          myEditorOptions := EditorOptions;
          myTreeOptions := KntTreeOptions;
          myFindOpts := FindOptions;
          ShowHint := KeyOptions.ShowTooltips;

          Icons_Change_Disable :=
            ( App.opt_NoUserIcons or
            ( assigned( ActiveFile ) and ( ActiveFile.TabIconsFN = _NF_Icons_BuiltIn )));

          if ( not Icons_Change_Disable ) then begin
            tmpicnfn := extractfilename( ICN_FN );
            if assigned( ActiveFile ) then begin
              if (( ActiveFile.TabIconsFN <> _NF_Icons_BuiltIn ) and
                 ( ActiveFile.TabIconsFN <> '' )) then
                tmpicnfn := extractfilename( ActiveFile.TabIconsFN );
            end;
            GroupBox_TabIcons.Caption := Format( GetRS(sCfg02), [tmpicnfn] );
          end;

        if ( Form_Options.ShowModal = mrOK ) then begin
          screen.Cursor := crHourGlass;
          try
            oldHotKey := KeyOptions.HotKey; // save previous state
            oldLanguageUI := KeyOptions.LanguageUI;

            KeyOptions := Form_Options.myOpts;
            TabOptions := Form_Options.myTabOpts;
            ClipOptions := Form_Options.myClipOpts;
            KntTreeOptions := Form_Options.myTreeOpts;
            EditorOptions := Form_Options.myEditorOptions;
            KntTreeOptions := Form_Options.myTreeOptions;
            FindOptions := Form_Options.myFindOpts;

            // update hotkey only if settings changed
            if (( App.Kbd.HotKeySuccess <> KeyOptions.HotKeyActivate ) or ( KeyOptions.HotKey <> oldHotKey )) then begin
              Form_Main.HotKeyProc( false );
              if KeyOptions.HotKeyActivate then
                 Form_Main.HotKeyProc( true );
            end;

            if Form_Options.Icons_Changed then begin
              // icons were changed, save them
              if assigned( ActiveFile ) then begin
                if ( ActiveFile.TabIconsFN = '' ) then
                  SaveCategoryBitmapsUser( ICN_FN )
                else
                  SaveCategoryBitmapsUser( ActiveFile.TabIconsFN );
              end
              else
                 SaveCategoryBitmapsUser( ICN_FN );
            end;

            if Form_Options.FoldingBlocks_Changed then begin
               SaveFoldingBlockInfo(Form_Options.LVfb);
            end;

            if oldLanguageUI <> KeyOptions.LanguageUI then
               ApplyLanguageUI (KeyOptions.LanguageUI);

            SaveOptions;
            with Form_Main do begin
              UpdateFormState;
              UpdateTabState;
              UpdateStatusBarState;
              UpdateResPanelState;
            end;

            if ClipCapMng.ClipCapActive then
              LoadTrayIcon( ClipOptions.SwitchIcon );

          finally
            screen.Cursor := crDefault;
          end;
        end
        else
           if Form_Options.Icon_Change_Canceled then
              LoadTabImages( true );

      finally
        Form_Options.Free;
      end;
  end;

end; // AdjustOptions

end.
