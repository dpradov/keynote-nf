
(* ************************************************************
 KEYNOTE: MOZILLA PUBLIC LICENSE STATEMENT.
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is KeyNote 1.0.

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.

 ===========
 13 Nov 2007
 ===========
 The adaptation of KeyNote to Delphi 2006 and the new functionalities
 added since version 1.7.0 corresponds to Daniel Prado Velasco
 <dprado.keynote@gmail.com> (Spain)
 Portions adapted by Daniel Prado are
 Copyright (C) 2007, 2008. All Rights Reserved.

  http://code.google.com/p/keynote-nf/
  http://groups.google.com/group/keynote-nf
  <dprado.keynote@gmail.com>

 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 Released: 30 June 2001
 -----------------------------------------------------------
 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <cicho@tenbit.pl>

************************************************************ *)

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
{%TogetherDiagram 'ModelSupport_keynote\dll_Keyboard\default.txaPackage'}
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
{%TogetherDiagram 'ModelSupport_keynote\dll_Keyboard\default.txvpck'}
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

uses
  Forms,
  Windows,
  Messages,
  ShellAPI,
  SysUtils,
  IniFiles,
  kn_Main in 'kn_Main.pas' {Form_Main},
  kn_Info in 'kn_Info.pas',
  kn_FileObj in 'kn_FileObj.pas',
  kn_NewNote in 'kn_NewNote.pas' {Form_NewNote},
  kn_FileInfo in 'kn_FileInfo.pas' {Form_FileInfo},
  kn_Const in 'kn_Const.pas',
  kn_NoteObj in 'kn_NoteObj.pas',
  kn_Chest in 'kn_Chest.pas' {Chest: TDataModule},
  kn_TabSelect in 'kn_TabSelect.pas' {Form_SelectTab},
  kn_Find in 'kn_Find.pas' {Form_Find},
  kn_URL in 'kn_URL.pas' {Form_URLAction},
  kn_filemgr in 'kn_filemgr.pas' {Form_FileMgr},
  kn_MacroCmdSelect in 'kn_MacroCmdSelect.pas' {Form_MacroCmd},
  kn_Glossary in 'kn_Glossary.pas' {Form_Glossary},
  kn_NodeList in 'kn_NodeList.pas',
  kn_INI in 'kn_INI.pas',
  kn_Defaults in 'kn_Defaults.pas' {Form_Defaults},
  kn_pass in 'kn_pass.pas' {Form_Password},
  kn_StyleObj in 'kn_StyleObj.pas',
  kn_ExpandObj in 'kn_ExpandObj.pas',
  kn_ExpTermDef in 'kn_ExpTermDef.pas' {Form_TermDef},
  kn_Chars in 'kn_Chars.pas' {Form_Chars},
  kn_Paragraph in 'kn_Paragraph.pas' {Form_Para},
  kn_Replace in 'kn_Replace.pas' {Form_Replace},
  kn_Plugins in 'kn_Plugins.pas',
  kn_PluginBase in 'kn_PluginBase.pas',
  kn_Cmd in 'kn_Cmd.pas',
  kn_Macro in 'kn_Macro.pas',
  kn_MacroEdit in 'kn_MacroEdit.pas' {Form_Macro},
  kn_MacroCmd in 'kn_MacroCmd.pas',
  kn_Msgs in 'kn_Msgs.pas',
  kn_RTFUtils in 'kn_RTFUtils.pas',
  kn_NewTemplate in 'kn_NewTemplate.pas' {Form_Template},
  kn_FileDropAction in 'kn_FileDropAction.pas' {Form_DropFile},
  kn_LanguageSel in 'kn_LanguageSel.pas' {Form_Lang},
  kn_DLLinterface in 'kn_DLLinterface.pas',
  kn_LocationObj in 'kn_LocationObj.pas',
  kn_NodeNum in 'kn_NodeNum.pas' {Form_NodeNum},
  kn_History in 'kn_History.pas',
  kn_ImagePicker in 'kn_ImagePicker.pas' {Form_ImgPick},
  kn_ExportNew in 'kn_ExportNew.pas' {Form_ExportNew},
  kn_FavExtDlg in 'kn_FavExtDlg.pas' {Form_FavExt},
  kn_SendMail in 'kn_SendMail.pas' {Form_Mail},
  kn_ClipUtils in 'kn_ClipUtils.pas',
  kn_DateTime in 'kn_DateTime.pas',
  kn_about in 'kn_about.pas' {AboutBox},
  dll_Keyboard in 'dll_Keyboard.pas',
  RxRichEd in '3rd_party\rx275d2006\Units\RxRichEd.pas',
  LCCombo in '3rd_party\langcombo\LCCombo.pas',
  Langs in '3rd_party\langcombo\Langs.pas',
  UAS in '3rd_party\UAS\TestUAS\UAS.pas',
  gf_Const in 'gf_Const.pas',
  kn_Global in 'kn_Global.pas',
  kn_FindReplaceMng in 'kn_FindReplaceMng.pas',
  kn_TemplateMng in 'kn_TemplateMng.pas',
  kn_PluginsMng in 'kn_PluginsMng.pas',
  kn_MacroMng in 'kn_MacroMng.pas',
  kn_TreeNoteMng in 'kn_TreeNoteMng.pas',
  kn_NoteMng in 'kn_NoteMng.pas',
  kn_NoteFileMng in 'kn_NoteFileMng.pas',
  kn_ConfigFileMng in 'kn_ConfigFileMng.pas',
  kn_DLLmng in 'kn_DLLmng.pas',
  kn_StyleMng in 'kn_StyleMng.pas',
  kn_FavoritesMng in 'kn_FavoritesMng.pas',
  kn_VirtualNodeMng in 'kn_VirtualNodeMng.pas',
  kn_BookmarksMng in 'kn_BookmarksMng.pas',
  kn_AlertMng in 'kn_AlertMng.pas' {Form_Alarm},
  kn_LinksMng in 'kn_LinksMng.pas',
  kn_EditorUtils in 'kn_EditorUtils.pas',
  kn_VCLControlsMng in 'kn_VCLControlsMng.pas',
  uStringUtils in '3rd_party\kdl32_Kryvich''s Delphi Localizer\uStringUtils.pas',
  uFreeLocalizer in '3rd_party\kdl32_Kryvich''s Delphi Localizer\uFreeLocalizer.pas',
  kn_LanguagesMng in 'kn_LanguagesMng.pas',
  kn_OptionsNew in 'kn_OptionsNew.pas' {Form_OptionsNew};

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
  ApplyLanguageUI (KeyOptions.LanguageUI);

  Application.CreateForm(TChest, Chest);
  Application.CreateForm(TForm_Main, Form_Main);
  Application.Run;

  // shutdown UAS
  if ( GetUASWnd<>0 ) then SendMessage( GetUASWnd, WM_CLOSE, 0, 0);

end.
