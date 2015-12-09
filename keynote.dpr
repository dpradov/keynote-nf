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


program keynote;

{%TogetherDiagram 'ModelSupport_keynote\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Cmd\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Chars\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\LCCombo\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_INI\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_NoteObj\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_SendMail\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_TabSelect\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_ImagePicker\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\RxRichEd\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_StyleObj\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_MacroEdit\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\keynote\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_ExportNew\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_NewTemplate\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\Langs\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_filemgr\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_DLLinterface\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_tmpRTF\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_ClipUtils\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_NodeList\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_FavExtDlg\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Paragraph\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_LocationObj\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_NodeNum\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Macro\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_LanguageSel\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Msgs\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_MacroCmdSelect\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Const\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_about\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Info\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_History\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\knx_resourcebar\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\gf_const\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_MacroCmd\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_ExpTermDef\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_DateTime\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Main\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Chest\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_FileDropAction\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_URL\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_ExpandObj\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_OptionsNew\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_FileInfo\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Glossary\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Replace\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\UAS\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_PluginBase\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_RTFUtils\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Defaults\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_FileObj\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Plugins\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_pass\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_NewNote\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Find\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\gf_const\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\keynote\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\LCCombo\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\RxRichEd\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\UAS\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\knx_resourcebar\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_tmpRTF\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_DateTime\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Defaults\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_ClipUtils\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_DLLinterface\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Const\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Cmd\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Chars\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Main\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_MacroEdit\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_MacroCmd\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_NoteObj\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_NodeList\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_NodeNum\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_about\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_ExportNew\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_FindReplaceMng\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_PluginsMng\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_TreeNoteMng\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_ConfigFileMng\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_TemplateMng\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_FavoritesMng\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_StyleMng\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Global\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_MacroMng\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_NoteFileMng\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_DLLmng\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_NoteMng\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_BookmarksMng\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_VirtualNodeMng\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_keynote\kn_ConfigFileMng\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_FindReplaceMng\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Find\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_Replace\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_filemgr\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_LocationObj\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_TreeNoteMng\default.txvpck'}
{%TogetherDiagram 'ModelSupport_keynote\kn_AlertMng\default.txaPackage'}
{%File 'support\gf_base.inc'}

uses
  Forms,
  Windows,
  Messages,
  ShellAPI,
  SysUtils,
  IniFiles,
  kn_Main in 'kn_Main.pas' {Form_Main},
  TreeNT in '3rd_party\treent\TreeNT.pas',
  ZLIBEX in '3rd_party\Delphi Fast Zlib 1.2.3\ZLIBEX.PAS',
  Langs in '3rd_party\langcombo\Langs.pas',
  UWebBrowserWrapper in '3rd_party\_Others\UWebBrowserWrapper.pas',
  TB97Ctls in '3rd_party\tb97_178a\Source\TB97Ctls.pas',
  ColorPicker in '3rd_party\colorpicker\ColorPicker.pas',
  ComCtrls95 in '3rd_party\ComCtrls95\ComCtrls95.pas',
  MRUFList in '3rd_party\mruflist\MRUFList.pas',
  StreamIO in '3rd_party\_Others\StreamIO.pas',
  cmpGFXListBox in '3rd_party\gfxlbcb\cmpGFXListBox.pas',
  Parser in '3rd_party\expression_evaluator\Parser.pas',
  SystemImageList in '3rd_party\sysimglist\SystemImageList.pas',
  cmpGFXComboBox in '3rd_party\gfxlbcb\cmpGFXComboBox.pas',
  BrowseDr in '3rd_party\browsedr\BrowseDr.pas',
  DFSStatusBar in '3rd_party\DFSStatusBar\DFSStatusBar.pas',
  TopWnd in '3rd_party\topmostwindow\TopWnd.pas',
  LCCombo in '3rd_party\langcombo\LCCombo.pas',
  FreeWordWeb in '3rd_party\wordweb\FreeWordWeb.pas',
  UAS in '3rd_party\UAS\TestUAS\UAS.pas',
  AJBSpeller in '3rd_party\ajbspeller\AJBSpeller.pas',
  crc32 in '3rd_party\CRCDelphi\crc32.pas',
  DCPcrypt in '3rd_party\dcpcrypt-1_3\DCPcrypt.pas',
  RichPrint in '3rd_party\richprint\RichPrint.pas',
  TntStdCtrls in '3rd_party\TntUnicodeControls\Source\TntStdCtrls.pas',
  WSocket in '3rd_party\ICS_InternetComponentSuite\WSocket.pas',
  MD5 in '3rd_party\ICS_InternetComponentSuite\MD5.pas',
  SmtpProt in '3rd_party\ICS_InternetComponentSuite\SmtpProt.pas',
  WSockbuf in '3rd_party\ICS_InternetComponentSuite\WSockbuf.pas',
  Placemnt in '3rd_party\rx275d2006\Units\Placemnt.pas',
  kn_DLLmng in 'support\kn_DLLmng.pas',
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
  kn_VCLControlsMng in 'general\kn_VCLControlsMng.pas',
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
  kn_DLLinterface in 'kn_DLLinterface.pas',
  MSWordConverter in 'editor\MSWordConverter.pas',
  kn_BookmarksMng in 'editor\kn_BookmarksMng.pas',
  kn_Chars in 'editor\kn_Chars.pas' {Form_Chars},
  kn_ClipUtils in 'editor\kn_ClipUtils.pas',
  kn_EditorUtils in 'editor\kn_EditorUtils.pas',
  kn_ExpandObj in 'editor\kn_ExpandObj.pas',
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
  kn_SendMail in 'various\kn_SendMail.pas' {Form_Mail},
  kn_AlertMng in 'various\kn_AlertMng.pas' {Form_Alarm},
  kn_FavExtDlg in 'various\kn_FavExtDlg.pas' {Form_FavExt},
  kn_FavoritesMng in 'various\kn_FavoritesMng.pas',
  dll_Keyboard in 'general\dll_Keyboard.pas',
  uFreeLocalizer in '3rd_party\kdl32_Kryvich''s Delphi Localizer\uFreeLocalizer.pas',
  uStringUtils in '3rd_party\kdl32_Kryvich''s Delphi Localizer\uStringUtils.pas',
  RxRichEd in '3rd_party\Rx275d2006\Units\RxRichEd.pas';

{$R *.RES}

// var
  // AboutBox : TAboutBox;
  // INIFile : TIniFile;
  // ShowSplash : boolean;
  // f : textfile;

begin

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
