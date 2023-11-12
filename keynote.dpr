(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************)


program keynote;



uses
  Forms,
  Windows,
  Messages,
  ShellAPI,
  SysUtils,
  IniFiles,

{
  TntClasses in '3rd_party\TntUnicodeControls\Source\TntClasses.pas',
  TntSystem  in '3rd_party\TntUnicodeControls\Source\TntSystem.pas',
  TntStdCtrls  in '3rd_party\TntUnicodeControls\Source\TntStdCtrls.pas',
  TntControls  in '3rd_party\TntUnicodeControls\Source\TntControls.pas',
  TntSysUtils  in '3rd_party\TntUnicodeControls\Source\TntSysUtils.pas',

  WSocket in '3rd_party\ICS_InternetComponentSuite\WSocket.pas',
  MD5 in '3rd_party\ICS_InternetComponentSuite\MD5.pas',
  SmtpProt in '3rd_party\ICS_InternetComponentSuite\SmtpProt.pas',
  WSockbuf in '3rd_party\ICS_InternetComponentSuite\WSockbuf.pas',

  DFSStatusBar in '3rd_party\DFSStatusBar\DFSStatusBar.pas',

  Placemnt in '3rd_party\rx275d2006\Units\Placemnt.pas',
  RxRichEd in '3rd_party\Rx275d2006\Units\RxRichEd.pas',

  uFreeLocalizer in '3rd_party\kdl32_Kryvich''s Delphi Localizer\uFreeLocalizer.pas',
  uStringUtils in '3rd_party\kdl32_Kryvich''s Delphi Localizer\uStringUtils.pas',
}

  TreeNT in '3rd_party\treent\TreeNT.pas',
  RxRichEd in '3rd_party\unRxLib\units\RxRichEd.pas',
  RxPlacemnt in '3rd_party\unRxLib\units\RxPlacemnt.pas',
  RichPrint in '3rd_party\richprint\RichPrint.pas',
  TB97Ctls in '3rd_party\tb97_178a\Source\TB97Ctls.pas',
  ColorPicker in '3rd_party\colorpicker\ColorPicker.pas',
  ComCtrls95 in '3rd_party\ComCtrls95\ComCtrls95.pas',
  cmpGFXListBox in '3rd_party\gfxlbcb\cmpGFXListBox.pas',
  cmpGFXComboBox in '3rd_party\gfxlbcb\cmpGFXComboBox.pas',
  SystemImageList in '3rd_party\sysimglist\SystemImageList.pas',
  Langs in '3rd_party\langcombo\Langs.pas',
  LCCombo in '3rd_party\langcombo\LCCombo.pas',
  TopWnd in '3rd_party\topmostwindow\TopWnd.pas',
  BrowseDr in '3rd_party\browsedr\BrowseDr.pas',
  MRUFList in '3rd_party\mruflist\MRUFList.pas',
  ZLIBEX in '3rd_party\Delphi Fast Zlib 1.2.3\ZLIBEX.PAS',
  UWebBrowserWrapper in '3rd_party\UWebBrowserWrapper\UWebBrowserWrapper.pas',
  FreeWordWeb in '3rd_party\wordweb\FreeWordWeb.pas',
  UAS in '3rd_party\UAS\TestUAS\UAS.pas',
  AJBSpeller in '3rd_party\ajbspeller\AJBSpeller.pas',
  Parser in '3rd_party\expression_evaluator\Parser.pas',
  crc32 in '3rd_party\CRCDelphi\crc32.pas',
  DCPcrypt in '3rd_party\dcpcrypt-1_3\DCPcrypt.pas',
  KDL.Localizer in '3rd_party\Kryvich Delphi Localizer\KDL.Localizer.pas',
  SynGdiPlus in '3rd_party\SynGdiPlus\SynGdiPlus.pas',
  gf_Bits in 'support\gf_Bits.pas',
  gf_fileassoc in 'support\gf_fileassoc.pas',
  gf_Files in 'support\gf_Files.pas',
  gf_Lang in 'support\gf_Lang.pas',
  gf_LangCombo in 'support\gf_LangCombo.pas',
  gf_misc in 'support\gf_misc.pas',
  gf_miscvcl in 'support\gf_miscvcl.pas',
  gf_streams in 'support\gf_streams.pas',
  gf_strings in 'support\gf_strings.pas',
  GFLog in 'support\GFLog.pas',
  kn_DLLmng in 'support\kn_DLLmng.pas',
  GFTipDlg in 'general\GFTipDlg.pas',
  GFTipDlgForm in 'general\GFTipDlgForm.pas' {GFTipForm},
  kn_about in 'general\kn_about.pas' {AboutBox},
  kn_Chest in 'general\kn_Chest.pas' {Chest: TDataModule},
  kn_Cmd in 'general\kn_Cmd.pas',
  kn_ConfigMng in 'general\kn_ConfigMng.pas',
  kn_Const in 'general\kn_Const.pas',
  kn_DateTime in 'general\kn_DateTime.pas',
  kn_Defaults in 'general\kn_Defaults.pas' {Form_Defaults},
  kn_Global in 'general\kn_Global.pas',
  kn_Info in 'general\kn_Info.pas',
  kn_INI in 'general\kn_INI.pas',
  kn_KBD in 'general\kn_KBD.pas',
  kn_Msgs in 'general\kn_Msgs.pas',
  kn_OptionsNew in 'general\kn_OptionsNew.pas' {Form_OptionsNew},
  kn_VCLControlsMng in 'general\kn_VCLControlsMng.pas',
  dll_Keyboard in 'general\dll_Keyboard.pas',
  kn_BookmarksMng in 'editor\kn_BookmarksMng.pas',
  kn_Chars in 'editor\kn_Chars.pas' {Form_Chars},
  kn_ClipUtils in 'editor\kn_ClipUtils.pas',
  kn_EditorUtils in 'editor\kn_EditorUtils.pas',
  kn_ExpTermDef in 'editor\kn_ExpTermDef.pas' {Form_TermDef},
  kn_FindReplace in 'editor\kn_FindReplace.pas' {Form_FindReplace},
  kn_FindReplaceMng in 'editor\kn_FindReplaceMng.pas',
  kn_Glossary in 'editor\kn_Glossary.pas' {Form_Glossary},
  kn_LanguageSel in 'editor\kn_LanguageSel.pas' {Form_Lang},
  kn_LinksMng in 'editor\kn_LinksMng.pas',
  kn_NewTemplate in 'editor\kn_NewTemplate.pas' {Form_Template},
  kn_Paragraph in 'editor\kn_Paragraph.pas' {Form_Para},
  kn_RTFUtils in 'editor\kn_RTFUtils.pas',
  kn_StyleMng in 'editor\kn_StyleMng.pas',
  kn_StyleObj in 'editor\kn_StyleObj.pas',
  kn_TemplateMng in 'editor\kn_TemplateMng.pas',
  kn_URL in 'editor\kn_URL.pas' {Form_URLAction},
  MSOfficeConverters in 'editor\MSOfficeConverters.pas',
  MSWordConverter in 'editor\MSWordConverter.pas',
  kn_LanguagesMng in 'Lang\kn_LanguagesMng.pas',
  kn_MacroMng in 'macros\kn_MacroMng.pas',
  kn_Macro in 'macros\kn_Macro.pas',
  kn_MacroCmd in 'macros\kn_MacroCmd.pas',
  kn_MacroCmdSelect in 'macros\kn_MacroCmdSelect.pas' {Form_MacroCmd},
  kn_MacroEdit in 'macros\kn_MacroEdit.pas' {Form_Macro},
  kn_PluginsMng in 'plugins\kn_PluginsMng.pas',
  kn_PluginBase in 'plugins\kn_PluginBase.pas',
  kn_Plugins in 'plugins\kn_Plugins.pas',
  kn_VirtualNodeMng in 'structure\kn_VirtualNodeMng.pas',
  kn_ExportImport in 'structure\kn_ExportImport.pas',
  kn_ExportNew in 'structure\kn_ExportNew.pas' {Form_ExportNew},
  kn_FileDropAction in 'structure\kn_FileDropAction.pas' {Form_DropFile},
  kn_FileInfo in 'structure\kn_FileInfo.pas' {Form_FileInfo},
  kn_filemgr in 'structure\kn_filemgr.pas' {Form_FileMgr},
  kn_FileObj in 'structure\kn_FileObj.pas',
  kn_History in 'structure\kn_History.pas',
  kn_ImagePicker in 'structure\kn_ImagePicker.pas' {Form_ImgPick},
  kn_LocationObj in 'structure\kn_LocationObj.pas',
  kn_NewNote in 'structure\kn_NewNote.pas' {Form_NewNote},
  kn_NodeList in 'structure\kn_NodeList.pas',
  kn_NodeNum in 'structure\kn_NodeNum.pas' {Form_NodeNum},
  kn_NoteFileMng in 'structure\kn_NoteFileMng.pas',
  kn_NoteMng in 'structure\kn_NoteMng.pas',
  kn_NoteObj in 'structure\kn_NoteObj.pas',
  kn_pass in 'structure\kn_pass.pas' {Form_Password},
  kn_TabSelect in 'structure\kn_TabSelect.pas' {Form_SelectTab},
  kn_TreeNoteMng in 'structure\kn_TreeNoteMng.pas',
  kn_VirtualNodeForm in 'structure\kn_VirtualNodeForm.pas' {Form_VNode},
  {$IFNDEF EXCLUDEEMAIL}
  kn_SendMail in 'various\kn_SendMail.pas' {Form_Mail},
  {$ENDIF }
  kn_AlertMng in 'various\kn_AlertMng.pas' {Form_Alarm},
  kn_FavExtDlg in 'various\kn_FavExtDlg.pas' {Form_FavExt},
  kn_FavoritesMng in 'various\kn_FavoritesMng.pas',
  kn_ImagesUtils in 'editor\kn_ImagesUtils.pas',
  kn_ImagesMng in 'editor\kn_ImagesMng.pas',
  kn_ImageForm in 'editor\kn_ImageForm.pas' {Form_Image},
  tom_TLB in 'various\tom_TLB.pas',
  kn_DLLinterface in 'kn_DLLinterface.pas',
  kn_Main in 'kn_Main.pas' {Form_Main}
;

{$R *.RES}

// var
  // AboutBox : TAboutBox;
  // INIFile : TIniFile;
  // ShowSplash : boolean;
  // f : textfile;

begin

  _OTHER_INSTANCE_HANDLE := FindWindow( UniqueAppName_KEYNOTE10, nil );                      // From version 1.8.0 KNT Class appears as "GFKeyNote10", not "GFKeyNote10.UnicodeClass"
  if (_OTHER_INSTANCE_HANDLE = 0) then
     _OTHER_INSTANCE_HANDLE := FindWindow( UniqueAppName_KEYNOTE10 + '.UnicodeClass', nil );

  Application.Initialize;
  // AboutBox := nil;


  (*
  This code would display splashbox if option turned ON.
  Removed, because splashbox was moved to DLL, and it cannot
  easily be shown as a non-modal window.
  if ( _OTHER_INSTANCE_HANDLE = 0 ) then
  begin
    // show splash screen only if KeyNote is not already running
    IniFile := TIniFile.Create( changefileext( Application.ExeName, ext_INI ));
    try
      ShowSplash := IniFile.ReadBool( 'KeyOptions', KeyOptionsIniStr.ShowSplash, true );
    finally
      IniFile.Free;
    end;
    if ShowSplash then
    begin
      AboutBox := TAboutBox.Create(Application);
      AboutBox.BTN_Close.Visible := false;
      AboutBox.Height := AboutBox.Height - ( AboutBox.BTN_Close.Height + 5 );
      AboutBox.Color := _GF_CLWINDOW;
      AboutBox.Show;
      AboutBox.Update;
    end;
  end;
  *)

  InitializeOptions;
  LoadRichEditLibrary;
  ApplyLanguageUI (KeyOptions.LanguageUI);

  Application.CreateForm(TChest, Chest);
  Application.CreateForm(TForm_Main, Form_Main);
  Application.Run;

  // shutdown UAS
  if ( GetUASWnd<>0 ) then SendMessage( GetUASWnd, WM_CLOSE, 0, 0);

end.
