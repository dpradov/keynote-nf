unit knt.RS;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2024 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface

{
Using a resource file to avoid the problem of IDs assigned to strings being modified and invalidating translations in
.LNG files
This problem occurs because strings declared in the resourcestring section in Delphi are processed at compile time and
their order and identification (ID in the resource table) can change depending on the order in which the units (.pas)
are included and processed by the compiler.
When new units are introduced in the project, the compiler can modify the order in which it processes the strings in
the resourcestring section. This is what causes the string IDs to change and, consequently, the pre-existing translations
no longer match the original strings.
}

{$R resources\knt_RS.RES}

function GetRS(ID: Integer): string;

  // kn_about.pas - sAB
  // kn_UpdateVersion.pas - sUpd
  // kn_Const.pas - sINF
  // kn_Defaults.pas - sDef
  // kn_LanguagesMng.pas - sLng
  // kn_OptionsNew.pas - sOpt
  // GFTipDlg.pas - sTip
  // kn_DLLmng.pas - sDll
  // kn_INI.pas - sINI
  // kn_URL.pas - sURL
  // kn_pass.pas - sPass
  // kn_PluginBase.pas, knt_Plugins.pas - sPlg
  // kn_PluginsMng.pas.pas - sPlgM
  // kn_Macro.pas - sMac
  // kn_MacroEdit.pas - sMacE
  // kn_MacroCmd.pas - sMacC
  // kn_MacroMng.pas - sMacM
  // kn_StyleObj.pas - sSty
  // kn_StyleMng.pas - sStyM
  // kn_TemplateMng.pas - sTpl
  // kn_ConfigMng.pas - sCfg
  // kn_ImagePicker.pas - sImgP
  // kn_ImageForm.pas - sImgF
  // kn_ImagesUtils.pas - sImgU
  // kn_ImagesMng.pas - sImg
  // kn_FindReplaceMng.pas - sFnd
  // kn_Glossary.pas - sGlss
  // kn_BookmarksMng.pas - sBmk
  // kn_FavExtDlg.pas - sFavDlg
  // kn_FavoritesMng.pas - sFav
  // kn_AlertMng.pas - sAlrt
  // kn_LinksMng.pas - sLnk
  // knt.ui.editor.pas - sEdt
  // kn_EditorUtils.pas - sEdt
  // kn_ClipUtils.pas - sEdt
  // kn_CharsNew.pas - sChrs
  // kn_Chest.pas - sChest
  // knt.App.pas - sApp
  // kn_Main.pas - sMain
  // kn_VCLControlsMng.pas - sVCL
  // kn_FileMgr.pas - sFmg
  // kn_FileInfo.pas - sFinf
  // kn_FileDropAction.pas - sFDrp
  // kn_KntFile.pas - sFile
  // kn_NoteFileMng.pas - sFileM
  // kn_ExportImport.pas - sExp
  // kn_ExportNew.pas - sExpFrm
  // kn_KntFolder_New.pas - sFldN
  // kn_KntFolder.pas - sFld
  // knt.ui.tree.pas - sTree
  // knt.ui.note.pas sUInote
  // kn_Defaults.pas - sDef

const

   STR_minute = 55056;   // minute
   STR_minutes = 55057;   // minutes
   STR_hour = 55058;   // hour
   STR_hours = 55059;   // hours
   STR_day = 55060;   // day
   STR_days = 55061;   // days
   STR_week = 55062;   // week
   STR_weeks = 55063;   // weeks

   FILTER_ALLFILES = 55055;   // All files (*.*)|*.*
   LANGUAGE_DEFAULT = 55024;   // English (Internal)
   Program_Desc = 55051;   // Tabbed notebook for Windows

   STR_SE_ERR_DDETIMEOUT = 55040;   // The DDE transaction could not be completed because the request timed out.
   STR_SE_ERR_DLLNOTFOUND = 55041;   // The specified dynamic-link library was not found.
   STR_SE_ERR_NOASSOC = 55042;   // There is no application associated with the given filename extension.
   STR_SE_ERR_OOM = 55043;   // There was not enough memory to complete the operation.
   STR_SE_ERR_SHARE = 55044;   // A sharing violation occurred
   STR_UNKNOWN_ERROR = 55045;   // Unknown error.
   STR_ERR_OUTOFRESOURCES = 55064;   // The operating system is out of memory or resources.
   STR_ERROR_FILE_NOT_FOUND = 55065;   // The specified file was not found.
   STR_ERROR_PATH_NOT_FOUND = 55066;   // The specified path was not found.
   STR_ERROR_BAD_FORMAT = 55067;   // The .EXE file is invalid (non-Win32 .EXE or error in .EXE image).
   STR_SE_ERR_ACCESSDENIED = 55068;   // The operating system denied access to the specified URL.
   STR_SE_ERR_ASSOCINCOMPLETE = 55069;   // The filename association is incomplete or invalid.
   STR_SE_ERR_DDEBUSY = 55070;   // The DDE transaction could not be completed because other DDE transactions were being processed.
   STR_SE_ERR_DDEFAIL = 55071;   // The DDE transaction failed.


   sAB00 = 55046;   // About -
   sAB01 = 55047;   // Double-click to send email; Right-click to copy\^(No HTML-formatted email, PLEASE!)
   sAB02 = 55048;   // Double-click to visit home page; Right-click to copy
   sAB03 = 55049;   // Keynote was inspired by a fantastic freeware prog: DaRT Notes\^by Andre v.d. Merwe (See "dart.txt" for information)
   sAB04 = 55050;   // KeyNote NF is an evolution of KeyNote (by Marek)

   sAlrt01 = 54632;   // %d alarms selected
   sAlrt02 = 54633;   // Set Alarm
   sAlrt03 = 54634;   // %d Reminders
   sAlrt04 = 54635;   // All Alarms/Events (%d)
   sAlrt05 = 54636;   // Overdue Events (%d)
   sAlrt06 = 54637;   // Pending Reminders (%d)
   sAlrt07 = 54638;   // Discarded Events (%d)
   sAlrt08 = 54639;   // ALARM [%s] :  %s
   sAlrt09 = 54608;   // [Sound ON]
   sAlrt10 = 54609;   // [Sound OFF]
   sAlrt11 = 54610;   // %d pending reminders, %d overdue
   sAlrt12 = 54611;   // [Popup ON]
   sAlrt13 = 54612;   // [Popup OFF]
   sAlrt14 = 54613;   // Expiration/Start time and/or Reminder interval are not valid.\^Please, correct it
   sAlrt15 = 54614;   // OK to discard all this %d alarms?
   sAlrt16 = 54615;   // OK to remove all this %d alarms?
   sAlrt17 = 54616;   // OK to restore all this %d alarms?
   sAlrt18 = 54617;   // OK to remove this alarm?
   sAlrt19 = 54618;   // OK to apply pending changes?
   sAlrt20 = 54619;   // Today
   sAlrt21 = 54620;   // Tomorrow
   sAlrt22 = 54621;   // All
   sAlrt23 = 54622;   // Overdue
   sAlrt24 = 54623;   // Pending
   sAlrt25 = 54592;   // Discarded
   sAlrt26 = 54593;   // All (with discarded)
   sAlrt27 = 54594;   // Show all pending reminders (triggered and ignored, not postponed nor discarded)
   sAlrt28 = 54595;   // Show all overdue events
   sAlrt29 = 54596;   // Show all set alarms (not discarded)
   sAlrt30 = 54597;   // Show all set alarms, including discarded
   sAlrt31 = 54598;   // Show All Dates
   sAlrt32 = 54599;   // Filter on selected Days
   sAlrt33 = 54600;   // Filter on selected Week
   sAlrt34 = 54601;   // Filter on selected Month
   sAlrt35 = 54602;   // (Filter applied)
   sAlrt36 = 54603;   // (%s overdue)
   sAlrt37 = 54604;   // (%s left)
   sAlrt38 = 54605;   // (%s before)

   sApp01 = 54502;   //  Cannot perform operation: Editor is Read-Only
   sApp02 = 54503;   // There is no active editor
   sApp03 = 54504;   // Function not implemented.
   sApp04 = 54505;   // (none)
   sApp05 = 54506;   //  Select some text before issuing this command.
   sApp06 = 54507;   // Unexpected or not implemented command:
   sApp07 = 54508;   // Unexpected error.
   sApp08 = 54509;   // \^\^Number of nodes (notes) in tree: %d
   sApp09 = 54510;   // Chars: %d  Alph: %d  Words: %d
   sApp10 = 54511;   // \^\^Clik OK to copy information to clipboard.
   sApp11 = 54480;   // Cannot display Tip of the Day: file "%s" not found.
   sApp12 = 54481;   // : Tip of the Day

   sBmk01 = 54648;   //  Bookmark %d assigned.
   sBmk02 = 54649;   //  Bookmark %d not assigned!
   sBmk03 = 54650;   //  Cannot access bookmark %d - Cleared
   sBmk04 = 54651;   //  Jumped to bookmark %d

   sCfg01 = 54693;   // Error in keyboard customization procedure:
   sCfg02 = 54694;   //  Customize Tab icons (%s)
   sCfg03 = 54695;   // Invalid command line arguments:
   sCfg04 = 54696;   // Error while loading custom keyboard configuration from %s: "%s"
   sCfg05 = 54697;   // There was a non-fatal error while loading defaults: \^%s\^\^Some settings may have been reset to defaults.

   sChest01 = 54499;   // Failed to load built-in category images from resource.
   sChest02 = 54500;   // Failed to load category images from
   sChest03 = 54501;   // Failed to save category images to

   sChrs01 = 54497;   //  Edit
   sChrs02 = 54498;   //  Done

   sDef00 = 54957;   // OK
   sDef01 = 54959;   // Folder Properties: %s
   sDef02 = 54928;   // Close
   sDef03 = 54929;   // Folder is Read-Only: cannot change properties
   sDef04 = 54930;   //  [RO]
   sDef05 = 54931;   //  View properties for current folder
   sDef06 = 54932;   // Change properties for current folder
   sDef07 = 54933;   // &Save as default for "%s"
   sDef08 = 54934;   // Defaults for
   sDef09 = 54935;   // Change Defaults for NEW folders in THIS FILE
   sDef0B = 54958;   // Accept changes and close dialog box
   sDef10 = 54936;   // Defaults for all files
   sDef11 = 54937;   // Change default properties for all NEW folders
   sDef12 = 54938;   // Folder name cannot be blank. Please enter a name.
   sDef13 = 54939;   // Folder name cannot contain the "%s" character
   sDef14 = 54940;   // Node name cannot contain the "%s" character
   sDef15 = 54941;   // OK to reset Editor font and color settings to default values?
   sDef16 = 54942;   // OK to reset Tree font and color settings to default values?
   sDef17 = 54943;   // Tokens for autonaming tree nodes:
   sDef18 = 54912;   // (must be UPPERCASE)
   sDef19 = 54913;   //  = current date
   sDef20 = 54914;   //  = current time
   sDef21 = 54915;   //  = total number of nodes
   sDef22 = 54916;   //  = new node's level
   sDef23 = 54917;   //  = new node's index
   sDef24 = 54918;   //  = new node's absolute index
   sDef25 = 54919;   //  = parent node's name
   sDef26 = 54920;   //  = name of active folder
   sDef27 = 54921;   //  = name of currently open file
   sDef28 = 54922;   // <no icon>
   sDef29 = 54923;   // Invalid zoom ratio:
   sDef30 = 54924;   //  (and apply to "%s" folder)
   sDef31 = 54202;   // REMEMBER:\^\^- Folder settings apply only to NEW notes, except:\^   - 'Plain note only': modifies ALL the notes\^   - 'WordWrap': affects ALL the notes\^       (not explicitly set WordWrap previously)\^\^>> More info in Help File (F1)
   sDef32 = 54203;   // REMEMBER:\^\^- Font change affect only to NEW nodes (all if Plain note)\^- BG Color depends on 'Inherit BG color from active node':\^   - If set, background color of selected node is shown\^     (=> BG color of its new child nodes)\^   - If not set, default BG color for all NEW nodes is shown\^   * To edit this option -> F5 | General settings| Rich Text editor\^\^- BG Color can be changed for ALL nodes in a Folder:\^    [Shift] + "Format | Background color"\^\^>> More info in Help File (F1)
   sDef33 = 54204;   // REMEMBER:\^\^- BG Color sets backgroud color for the Tree Panel and\^  default BG color of tree nodes\^- Previous changes to individual nodes won't be affected\^- 'Inherit properties from active node' option is\^  considered in NEW nodes\^- Font and BG color can be changed for ALL tree panels at once:\^    "Apply to ALL folders"\^\^- Note: 'Inherit BG color from active node' option does NOT\^  affect (refers to Editor) \^\^>> More info in Help File (F1)

   sDll01 = 54867;   // Error while attempting to load runtime library "%s". Please reinstall KeyNote.
   sDll02 = 54868;   // Procedure "%s" not found in runtime library "%s". Please reinstall KeyNote.

   sEdt01 = 54575;   // Invalid zoom ratio:
   sEdt02 = 54544;   //  L %d / %d  C %d
   sEdt03 = 54545;   //  Sel: %d  W: %d
   sEdt04 = 54546;   //  Overwrite mode disabled through INI file
   sEdt05 = 54547;   // Convert decimal to Roman
   sEdt06 = 54548;   // Enter a decimal number:
   sEdt07 = 54549;   // %s is not a valid number
   sEdt08 = 54550;   // Convert Roman to decimal
   sEdt09 = 54551;   // Enter a Roman number:
   sEdt10 = 54552;   // %s is not a valid Roman number
   sEdt11 = 54553;   //  No valid bracket at cursor position
   sEdt12 = 54554;   //  Matching bracket FOUND
   sEdt13 = 54555;   //  Matching bracket NOT FOUND
   sEdt14 = 54556;   // OK to trim white space characters in whole note?
   sEdt15 = 54557;   // OK to compress white space characters in whole note?
   sEdt16 = 54558;   //  Result:
   sEdt17 = 54559;   // Paste last eval result:
   sEdt18 = 54528;   // Expression %s evaluates to: %s\^\^Result was copied to clipboard. Click OK to insert.
   sEdt19 = 54529;   // Select image to insert
   sEdt20 = 54530;   // All image files
   sEdt21 = 54531;   //  Function not available
   sEdt22 = 54532;   //  No word at cursor
   sEdt23 = 54533;   //  Word not in glossary. Use Shift+F7 to add.
   sEdt24 = 54534;   // Term expansion glossary "%s" is not loaded.
   sEdt25 = 54535;   // Glossary term already exists: "%s" -> "%s". OK to redefine term as "%s"?
   sEdt26 = 54536;   //  Added to glossary: "%s" -> "%s"
   sEdt27 = 54537;   // Replace editor contents with result from spellchecker?
   sEdt28 = 54538;   //  Calculating statistics... Please wait
   sEdt29 = 54539;   // Selected text
   sEdt30 = 54540;   // Folder text
   sEdt31 = 54541;   // %s statistics\^\^Characters: %s\^Alphabetic: %s\^Whitespace: %s\^\^Words: %s\^Lines: %s
   sEdt32 = 54542;   // Lookup in WordWeb
   sEdt33 = 54543;   // Enter word to look up:
   sEdt34 = 54512;   // Error loading WordWeb. The program may not be installed on your computer. See file "wordweb.txt" for more information.\^\^Error message:
   sEdt35 = 54513;   // UAS path
   sEdt36 = 54514;   // Please specify full path to uas.exe
   sEdt37 = 54515;   // KeyNote cannot find the location of uas.exe. UltimaShell Autocompletion Server will not be loaded.
   sEdt38 = 54516;   //  UltimaShell Autocompletion Server loaded.
   sEdt39 = 54517;   // Cannot load UltimaShell Autocompletion Server. It may not be installed. Would you like to go to the UAS website and download the application?
   sEdt40 = 54518;   //  UltimaShell Autocompletion Server unloaded.
   sEdt41 = 54519;   //  UltimaShell Autocompletion Server is not loaded.
   sEdt42 = 54520;   // A Read-Only folder cannot be used for clipboard capture.
   sEdt43 = 54521;   // a new node
   sEdt44 = 54522;   // whichever node is currently selected
   sEdt45 = 54523;   // Each copied item will be pasted into %s in the tree. Continue?
   sEdt46 = 54524;   //  Clipboard capture is now
   sEdt47 = 54525;   //  Capturing text from clipboard
   sEdt48 = 54526;   //  Clipboard capture done
   sEdt49 = 54527;   // Print all nodes in folder?\^\^0: Only selected node\^1: All, contiguous\^2: All, starting on new page 
   sEdt50 = 54496;   // CRC calculation error in clipboard capture, testing for duplicate clips will be turned off. Message:
   sEdt51 = 55129;   // Insert basic Table with R rows and C columns
   sEdt52 = 55134;   // ** UNAUTHORIZED CONTENT **   => View | Encrypted Content

   sExp01 = 54274;   // Error while importing HTML text:
   sExp02 = 54275;   // Error while exporting to HTML (method=
   sExpFrm00 = 54276;   // Export node content
   sExpFrm01 = 54277;   // Exporting is underway. OK to abort?
   sExpFrm02 = 54278;   // Please select a valid directory for exported files.
   sExpFrm03 = 54279;   // Specified output directory does not not exit. Please select a valid directory.
   sExpFrm04 = 54280;   // You did not select any foldersnotes for exporting.
   sExpFrm11 = 54281;   // Error while exporting folders:
   sExpFrm12 = 54282;   // Exported  %d folders (%d notes).
   sExpFrm13 = 54283;   // Exporting was aborted due to an error.
   sExpFrm14 = 54284;   // Exporting was aborted at user request.
   sExpFrm15 = 54285;   // The following token can be used in headings:\^\^%s%s - Filename\^%s%s - Folder name\^%s%s - Node name\^%s%s - Node level\^%s%s - Node index\^%s%s - Line break\^%s%s - Symbols, increasing\^%s%s - Symbols, decreasing\^\^F1 => More INFO and usage examples
   sExpFrm16 = 54286;   // No active tree node: select a node first.
   sExpFrm17 = 54287;   // Current node has no text: nothing to export.
   sExpFrm18 = 54256;   //  Node exported to
   sExpFrm19 = 54257;   // Error exporting node:
   sExpFrm20 = 54258;   // 'Current node' will be managed as 'Current node and subtree' for KeyNote format\^ Continue?
   sExpFrm21 = 55095;   // E&xport
   sExpFrm22 = 55096;   // Print
   sExpFrm23 = 55126;   // Nothing to export

   sFav01 = 54653;   // Error loading Favorites:
   sFav02 = 54654;   // Rename favorite location
   sFav03 = 54655;   // Enter new name:
   sFav04 = 54624;   // A favorite named "%s" already exists. Choose another name
   sFav05 = 54625;   // Error renaming favorite location:
   sFav06 = 54626;   // Favorite KeyNote location
   sFav07 = 54627;   // Enter location name:
   sFav08 = 54628;   //  or click Cancel to abort.
   sFav09 = 54629;   // Delete "%s" from Favorites?
   sFav10 = 54630;   // Error deleting Favorite:
   sFav11 = 54631;   // Favorites list error:
   sFavDlg01 = 54652;   // The specified file does not exist. Do you want to use the filename anyway?

   sFDrp01 = 54398;   // file
   sFDrp02 = 54399;   // files
   sFDrp03 = 54368;   // Select import method (%d *%s %s)
   sFDrp04 = 54369;   // &General options
   sFDrp06 = 54370;   // &HTML options
   sFDrp07 = 54371;   // Some files will be renamed

   sFile01 = 54372;   // Cannot open "%s": File not found
   sFile02 = 54373;   // Invalid file header in "%s" (not a KeyNote file)
   sFile03 = 54374;   // Access passphrase not specified
   sFile04 = 54375;   // The passphrase is invalid. Try again?
   sFile05 = 54376;   // %s: This file was created with a version of KeyNote later than the version you are using. Expected version ID: "%s.%s" This file version ID: "%s.%s"  You need the latest version of KeyNote to open this file.
   sFile06 = 54377;   // : This file was created with a version of KeyNote newer than the version you are using. The file can be opened, but some information can be lost or misinterpreted. As a safety measure, the file should be opened in Read-Only mode. Would you like to open the file as Read-Only?
   sFile07 = 54378;   // %s: Invalid file header or version, or corrupt file.
   sFile08 = 54379;   // Error loading folder
   sFile10 = 54380;   // This file contains notes which are not compatible with %s format. Only %s notes can be saved in this format.
   sFile12 = 54381;   // Error: Filename not specified.
   sFile13 = 54382;   // Error while saving folder "%s": %s
   sFile14 = 54383;   // Cannot save: Passphrase not set
   sFile15 = 54352;   // Stream size error: Encrypted file is invalid or corrupt.
   sFile16 = 54353;   // Invalid passphrase
   sFile18 = 54354;   // OK to convert to PLAIN TEXT current note?\^\^ALL IMAGES and FORMATTING will be REMOVED !!
   sFile19 = 54355;   // Exception trying to ensure plain text and removing of images:
   sFile20 = 54356;   // Virtual note "%s" cannot write file
   sFile21 = 54357;   // OK to deduce the missing date information?\^
   sFile22 = 54358;   // OK to remove date from note name?\^
   sFile23 = 54359;   // All (or selected) nodes will be considered
   sFile24 = 54360;   // \^\^Please read the help file before proceeding. Search for "Deduce Dates"
   sFile25 = 55130;   // The key transformation took %u ms
   sFile26 = 55132;   // : Cannot open encrypted file
   sFile27 = 55133;   // : Cannot show encrypted content
   sFile28 = 55136;   // Error loading encrypted content

   sFileM01 = 54361;   // Cannot create a new file:
   sFileM02 = 54362;   //  New KNT file created.
   sFileM04 = 54363;   // A new KNT file has been created. Would you like to save the new file now?\^\^(The Auto Save function will not work until the file is named and saved first.)
   sFileM05 = 54364;   // Open Keynote file
   sFileM06 = 54365;   //  Opening
   sFileM07 = 54366;   // One or more errors occurred while loading the file. The file may not have loaded completely. To minimize the risk of data loss, the file was opened in Read-Only mode. Use the "Save As..." command to save the file.
   sFileM08 = 54367;   //  <unknown>
   sFileM09 = 54336;   //  diskette
   sFileM10 = 54337;   //  network
   sFileM11 = 54338;   //  CD-ROM
   sFileM12 = 54339;   //  RAM
   sFileM13 = 54340;   // File "%s" was opened in Read-Only mode, because it resides on a %s drive "%s".
   sFileM14 = 54341;   //  File opened.
   sFileM15 = 54342;   //  Error.
   sFileM16 = 54343;   // Folder monitor error:
   sFileM17 = 54344;   //  ERROR %d opening file
   sFileM19 = 54345;   //  Saving
   sFileM20 = 54346;   // Specified backup directory "%s" does not exist. Backup files will be created in the original file's directory.
   sFileM21 = 54347;   // Cannot create backup file (error %d: %s). Current file will not be backed up. Proceed anyway?\^\^ (Note: File was temporary saved in %s)
   sFileM22 = 54348;   //  File saved (%d folders, %d notes)
   sFileM23 = 54349;   //  Error %d while saving file.
   sFileM24 = 54351;   // Error %d occurred while saving to a temporal folder (%s). The contents of the file in memory are perhaps partially corrupted.\^\^-> Please, retry, and if you can'nt save to a .knt file, try to recover the nodes/notes with unsaved changes using, for example, File -> Export...\^\^\^
   sFileM25 = 54320;   // Failed to create output file "%s" (Error: %d)\^File was temporary saved in %s\^\^\^
   sFileM26 = 54321;   // The Auto-Save option was turned OFF, to prevent KeyNote from automatically saving the (perhaps) damaged file.
   sFileM27 = 54322;   //  ERROR saving file
   sFileM28 = 54323;   // Saving "
   sFileM29 = 54324;   // Folder monitoring has been disabled due to the following error:
   sFileM30 = 54325;   //  File closed.
   sFileM32 = 54326;   // Select backup folder
   sFileM33 = 54327;   // Cannot copy file to its own directory.
   sFileM34 = 54328;   // The file %s already exists. OK to overwrite existing file?
   sFileM35 = 54329;   //  Copying file...
   sFileM36 = 54330;   //  File copied.
   sFileM37 = 54331;   // Successfully copied KNT file to
   sFileM38 = 54332;   // Copying failed (
   sFileM39 = 54333;   // Select file to merge folders from
   sFileM40 = 54334;   // There was an error while loading merge file.
   sFileM41 = 54335;   // The file you selected does not contain any folders.
   sFileM42 = 54304;   // Error while loading merge file:
   sFileM43 = 54305;   // Folders in %s
   sFileM44 = 54306;   // You did not select any folder: nothing to merge.
   sFileM45 = 54307;   //  Merging folders...
   sFileM46 = 54308;   // Error while adding folders:
   sFileM47 = 54309;   // Merged %d folders from "%s"
   sFileM48 = 54310;   // No folders were merged
   sFileM49 = 54312;   // Another application has modified the knt file %s. Reload the file from disk?
   sFileM50 = 54313;   // %s folder "%s" does not exist
   sFileM51 = 54314;   // . Create the folder now?
   sFileM52 = 54315;   // Could not create folder: %s
   sFileM53 = 54316;   //  File modified by external application.
   sFileM54 = 54317;   // Folders were modified. Save file before continuing?\^If you answer No, you will lose all changes made since last save.
   sFileM55 = 54318;   // Current file has not been saved. If you continue, changes will be lost.\^Proceed anyway?
   sFileM56 = 54319;   // Warning!
   sFileM57 = 54288;   // Select files for importing
   sFileM58 = 54289;   // The file "%s" does not appear to be a text file (nor image). The result of importing it may be unpredictable.\^\^Import as a plain text file, anyway?
   sFileM59 = 54290;   //  Importing
   sFileM60 = 54291;   // Failed to convert HTML file "%s" to RTF
   sFileM61 = 54292;   // Error importing
   sFileM62 = 54293;   //  Finished importing.
   sFileM63 = 54294;   // Cannot select methods for handling files.
   sFileM65 = 54295;   // Cannot import a directory "%s"
   sFileM67 = 54296;   // Unknown or unexpected file action (%d)
   sFileM68 = 54297;   // Error while importing files:
   sFileM75 = 54298;   // Successfully created %s registry entries
   sFileM76 = 54299;   // There was an error while creating file type associations:
   sFileM77 = 54300;   // This file is Read-Only. Use "Save As" command to save it with a new name.
   sFileM78 = 54301;   // Backup at %s before any modification in "%s"
   sFileM79 = 54302;   // File is not modified. Nothing to save
   sFileM80 = 54303;   // \^\^Option "Autoregister file type" will be unchecked
   sFileM81 = 54272;   // Cannot insert images in a plain text folder
   sFileM82 = 54273;   // The file must first be saved (with Save or Save As)
   sFileM83 = 54311;   // %d Links or Mirror nodes couldn't be adapted\^Links can be found searching for "file///<%d"
   sFileMInfSaving = 54350;   // * NOTE:\^  - The .knt file in disk must not have been modified from last correct save.\^  - You should have multiple backup files in the folder %s, specially if you selected the option "Backup at regular intervals" (highly recommended)
   sFileM84 = 55137;   // Do you want to merge the ENCRYPTED CONTENT?\^\^It will remain protected with the password of the current file (or the one you set *before* saving)

   sFInf01 = 54410;   //  file
   sFInf02 = 54411;   // File properties:
   sFInf03 = 54412;   //  bytes
   sFInf04 = 54413;   // (file not saved)
   sFInf05 = 54414;   // never
   sFInf06 = 54415;   // Open "%s" as &Read-Only
   sFInf07 = 54384;   // (none)
   sFInf09 = 54385;   // The passphrase you entered is too short: Minimum passphrase length is %d characters
   sFInf10 = 54386;   // The passphrases you entered do not match. Please enter the exact same passphrase twice.
   sFInf11 = 54387;   // You chose to encrypt a file that contains virtual nodes. Note that the disk files linked to virtual nodes and images saves in external storage (Zip or Folder) will NOT be encrypted.\^\^Continue?
   sFInf12 = 54388;   // File "%s" was open in READ-ONLY mode. If you uncheck this box, the read-only mode will be turned OFF. Continue?
   sFInf13 = 54389;   // Open images storage folder
   sFInf14 = 54390;   // Open images storage file
   sFInf15 = 54391;   // Set
   sFInf16 = 54392;   // Must save KNT before change images storage again
   sFInf17 = 54393;   // (*) Missing current external storage
   sFInf18 = 54394;   // New images will be saved provisionally [only] as Embedded KNT\^Deletions will be effective when it is available\^\^(It may be totally fine if you temporarily lose access to image storage)
   sFInf19 = 54395;   // Current Next ID (%d) cannot be reduced\^(Max ID in image list is %d)
   sFInf20 = 54396;   // Max ID in image list is %d and Next ID is %d\^\^Do you want the NEXT image to be saved with ID = %d \^\^* YOU MUST MAKE SURE there are no images with larger IDs on the external storage, perhaps referenced by other knt files (New images could override existing files)\^\^CONTINUE?
   sFInf21 = 54397;   // Next ID was changed ok
   sFInf22 = 55131;   // All notes will be saved unencrypted. CONTINUE?

   sFld01 = 54263;   //  Virtual:
   sFld05 = 54264;   // Problem while saving folder "%s": Note count mismatch (Folder: %d  Internal: %d) The note may not be saved correctly. Continue?
   sFld07 = 54265;   // Node count mismatch.
   sFld09 = 54266;   // Folder contains %d notes, but only %d were saved.
   sFld11 = 54267;   // Failed to open TreePad file
   sFld21 = 54268;   //  New folder.
   sFld22 = 54269;   // Are you sure you want to DELETE FOLDER "%s"?\^This operation cannot be undone.
   sFld24 = 54270;   //  Folder deleted.
   sFld25 = 54271;   //  Folder renamed.
   sFld31 = 54240;   // Virtual node "%s" is currently linked to file "%s". Do you want to link the node to a different file?
   sFld32 = 54241;   // Node "%s" contains text. Do you want to flush this text to a file and make the node virtual?
   sFld33 = 54242;   // This KeyNote file is encrypted, but disk files linked to virtual nodes will NOT be encrypted.\^\^Continue?
   sFld34 = 54243;   // Select file for virtual node
   sFld35 = 54244;   // Only RTF, Text and HTML files can be linked to virtual nodes.
   sFld36 = 54245;   // Cannot link virtual node to a file on removable drive %s:\
   sFld37 = 54246;   // You are creating a virtual node linked to file on removable drive %s\. The file may not be available at a later time. Continue anyway?
   sFld38 = 54247;   // Selected file is already linked to a virtual node\^(Note: You can create a linked node to it)
   sFld39 = 54248;   // Virtual node error:
   sFld40 = 54249;   // OK to reload the node from file %s?
   sFld41 = 54250;   // Unlink virtual node "%s"? The contents of the node will be retained, but the link with the file on disk (%s) will be removed.
   sFld42 = 54251;   // Virtual node %s HAS BEEN modified within KeyNote. If the node is refreshed, the changes will be lost\^
   sFld43 = 54252;   // Virtual node %s has NOT been modified within KeyNote\^
   sFld44 = 54253;   // Error refreshing virtual node:
   sFld45 = 54254;   //  Virtual node refreshed.
   sFld46 = 54255;   //  Error refreshing node
   sFld47 = 54224;   // Selected node "%s" is not a virtual node.
   sFldN01 = 54259;   // <no icon>
   sFldN02 = 54260;   // Rename folder
   sFldN03 = 54261;   // Folder name cannot be blank. Please enter a name.
   sFldN04 = 54262;   // Folder name cannot contain the "%s" character

   sFmg01 = 54402;   // Loading file manager from "
   sFmg02 = 54403;   // Error initializing FileManager:
   sFmg03 = 54404;   // Notes file manager: %d file(s)
   sFmg04 = 54405;   // This file cannot be selected because it does not exist or contains data in a format that %s does not support. Please select another file.
   sFmg05 = 54406;   // FileManager list is empty. This dialog box will now close.
   sFmg06 = 54407;   // never
   sFmg07 = 54408;   // No information is available about this file.
   sFmg08 = 54409;   // This file does not exist or cannot be accessed.
   sFnd01 = 54663;   // Replace this occurrence?
   sFnd02 = 54664;   // Pattern not found: "%s"
   sFnd05 = 54665;   // Search results are not available.
   sFnd06 = 54666;   // Options
   sFnd07 = 54667;   //  Searching - press ESC to abort.
   sFnd08 = 54668;   // An error occurred during search:
   sFnd09 = 54669;   //  Pattern found at pos %d (%d occurrence(s))
   sFnd10 = 54670;   //  Pattern not found.
   sFnd11 = 54671;   //  Replaced %d occurrence(s)
   sFnd13 = 54641;   //  matches
   sFnd14 = 55125;   // Ignored occurrences in %d folded blocks

   sGlss00 = 54642;   // No item selected.
   sGlss01 = 54643;   // Shortcut term and its expanded definition cannot be blank.
   sGlss02 = 54644;   // Glossary term already exists: "%s" -> "%s". OK to redefine term as "%s"?
   sGlss03 = 54645;   // Error saving Glossary list:
   sGlss04 = 54646;   // Glossary terms: %d
   sGlss05 = 54647;   // Error loading Glossary list:

   sFoldBl0 = 55101;  // No item selected
   sFoldBl1 = 55102;  // Opening and closing tokens cannot be blank
   sFoldBl2 = 55103;  // Opening token already exists: "%s" | "%s". OK to redefine?
   sFoldBl3 = 55104;  // Error saving Folding Block list:
   sFoldBl4 = 55105;  // Error loading Folding Block list:
   sFoldBl5 = 55128;  // Markers to be used when expanding already exist (uncheck them first)
   sFold1   = 55127;  // Cannot fold text with a table that has nested cells\^See help for possible options

   sImg01 = 54675;   // Invalid Storage definition:
   sImg02 = 54676;   // Invalid Image definition:
   sImg03 = 54677;   // Invalid Embedded Image:
   sImg04 = 54678;   // Image not found:
   sImg05 = 54680;   // External storage not ready?\^New images will be stored provisionally [only] as Embedded KNT when ext.storage not ready on save
   sImg07 = 54681;   // Folder "%s" is not empty or valid
   sImg08 = 54682;   // A file with that name already exists (%s)
   sImg09 = 54679;   //  | %d instances
   sImg10 = 54683;   // Error %d opening "%s": "%s"
   sImg11 = 54684;   // Folder "%s" does not exist or is empty
   sImg12 = 54685;   // File "%s" does not exist or is not a valid Zip
   sImg13 = 54686;   // All images will be adapted to the storage mode. If selected a new external storage, image files will only be added when you save the KNT file. Current external storage will not be modified.\^You can change the storage again after saving the KNT file.\^\^Continue?
   sImg14 = 54687;   // Current external storage is not available or is invalid\^If you moved the external storage, select "Relocated" and register its new location
   sImg15 = 54656;   // All images have been adapted OK to the storage mode. Changes will be confirmed when KNT file is saved\^(%d different images have been found)
   sImg16 = 54657;   // Exception creating ZIP archive:
   sImg17 = 54658;   // Exception adding file to ZIP archive:
   sImg18 = 54659;   // Exception opening image viewer:
   sImg20 = 54660;   // Exception processing image in RTF:
   sImg21 = 54661;   // Error saving image "%s" (ID:%d) :\^  Content lost\^  Will be removed from Images
   sImg22 = 54662;   // < Unregistered image >
   sImg23 = 55138;   //Error decrypting image
   sImgF01 = 54699;   // Image no available. Change in caption will not saved
   sImgF02 = 54700;   // Save image file as
   sImgF03 = 54701;   // All image files
   sImgF04 = 54702;   // Open image file  (Ctrl -> open file location)
   sImgP01 = 54698;   //  icon %d
   sImgU01 = 54703;   // Error creating RTF for image insertion on editor:
   sImgU02 = 54672;   // Error processing RTF visible image (\pict) :
   sImgU03 = 54673;   // Error processing RTF hidden image (hyperlink) :
   sImgU04 = 54674;   // Error converting image format:

   sINFClipNdNam1 = 54977;   // Default node name
   sINFClipNdNam2 = 54978;   // Use clipboard text
   sINFClipNdNam3 = 54979;   // Use current date and time
   sINFClipPlainTxt1 = 54975;   // Plain (without any formatting)
   sINFClipPlainTxt2 = 54944;   // Only hyperlinks (without other formatting)
   sINFClipPlainTxt3 = 54945;   // Only font style (bold, italic, ...)
   sINFClipPlainTxt4 = 54946;   // Only font (without paragraph formatting)
   sINFCompres1 = 54971;   // None
   sINFCompres2 = 54972;   // Fastest
   sINFCompres3 = 54973;   // Default
   sINFCompres4 = 54974;   // Max
   sINFCtrlUD1 = 54954;   // Moves cursor to prev or next paragraph
   sINFCtrlUD2 = 54955;   // Shift view one line up or down
   sINFCtrlUD3 = 54956;   // Smoothly moves scroll bar vertically
   sINFDefaults1 = 55008;   // New folder
   sINFDefaults2 = 55009;   // New node
   sINFDIR1 = 55032;   // Up
   sINFDIR2 = 55033;   // Down
   sINFDIR3 = 55034;   // Left
   sINFDIR4 = 55035;   // Right
   sINFDrop1 = 54980;   // Open file in KeyNote
   sINFDrop2 = 54981;   // Execute (macro or plugin)
   sINFDrop3 = 54982;   // Merge folders into current file
   sINFDrop4 = 54983;   // Import as a new folder
   sINFDrop5 = 54984;   // Create hyperlink to file
   sINFDrop6 = 54985;   // Import as tree nodes
   sINFDrop7 = 54986;   // Import as virtual tree nodes
   sINFDrop9 = 54987;   // Insert content at caret position
   sINFExpnd1 = 55004;   // Show tree fully collapsed
   sINFExpnd2 = 55005;   // Expand only last active node
   sINFExpnd3 = 55006;   // Expand only top level nodes
   sINFExpnd4 = 55007;   // Restore expanded state of all nodes
   sINFExpnd5 = 54976;   // Show tree fully expanded
   sINFExptFrmt1 = 54994;   // Plain text
   sINFExptFrmt2 = 54995;   // Rich text (RTF)
   sINFExptFrmt3 = 54996;   // KeyNote File (knt)
   sINFExptFrmt4 = 55094;  // Printer (PDF, ...)
   sINFFormats1 = 55010;   // Keynote native
   sINFFormats2 = 55011;   // Keynote encrypted
   sINFFormats3 = 55012;   // Keynote compressed
   sINFIconKind1 = 54997;   // None
   sINFIconKind2 = 54998;   // Standard icons
   sINFIconKind3 = 54999;   // Custom icons
   sINFImgExtSt1 = 54952;   // Folder
   sINFImgExtSt2 = 54953;   // ZIP
   sINFImgSM1 = 54947;   // Embedded RTF
   sINFImgSM2 = 54948;   // Embedded KNT
   sINFImgSM3 = 54949;   // External (Folder or Zip)
   sINFImgSM4 = 54950;   // External + Embedded KNT
   sINFImgSM5 = 54951;   // No export images
   sINFImpHTML1 = 54988;   // No conversion (HTML source)
   sINFImpHTML2 = 54989;   // Use Shared HTML Text Converter (html32.cnv + msconv97.dll)
   sINFImpHTML3 = 54990;   // Use MS Word Converter
   sINFImpHTML4 = 54991;   // Use Internet Explorer
   sINFImpHTML5 = 54960;   // Use Microsoft HTML Converter (html.iec)
   sINFLinkType1 = 55000;   // Internet address
   sINFLinkType2 = 55001;   // Email address
   sINFLinkType3 = 55002;   // File or folder
   sINFLinkType4 = 55003;   // KeyNote location
   sINFPOS1 = 55036;   // Top
   sINFPOS2 = 55037;   // Bottom
   sINFPOS3 = 55038;   // Left
   sINFPOS4 = 55039;   // Right
   sINFSrchChk1 = 55019;   // Only non checked nodes
   sINFSrchChk2 = 55020;   // Only checked nodes
   sINFSrchChk3 = 55021;   // All
   sINFSrchMode1 = 55013;   // Exact phrase
   sINFSrchMode2 = 55014;   // All the words
   sINFSrchMode3 = 55015;   // Any of the words
   sINFSrchScope1 = 55016;   // Only node names
   sINFSrchScope2 = 55017;   // Only note contents
   sINFSrchScope3 = 55018;   // All
   sINFSrchFolded1 = 55097;  // All
   sINFSrchFolded2 = 55098;  // Exclude folded
   sINFSrchFolded3 = 55099;  // Exclude tagged folded
   sINFSrchFolded4 = 55100;  // Only folded   
   sINFStyRg1 = 54205;   // Font
   sINFStyRg2 = 54206;   // Paragraph
   sINFStyRg3 = 54207;   // Font and paragraph
   sINFSymb0 = 54961;   // Euro
   sINFSymb1 = 54962;   // Copyright
   sINFSymb2 = 54963;   // Registered trademark
   sINFSymb3 = 54964;   // Trademark
   sINFSymb4 = 54965;   // Paragraph
   sINFSymb5 = 54966;   // Degree
   sINFSymb6 = 54967;   // Plus/minus
   sINFSymb7 = 54968;   // Dots
   sINFSymb8 = 54969;   // French parenthesis (left)
   sINFSymb9 = 54970;   // French parenthesis (right)
   sINFTreeSel1 = 55022;   // Current node
   sINFTreeSel2 = 55023;   // Current node and subtree
   sINFTreeSel3 = 54992;   // Checked nodes
   sINFTreeSel4 = 54993;   // Full tree
   sINFUrlAct1 = 55025;   // Open
   sINFUrlAct2 = 55026;   // Open in new window
   sINFUrlAct3 = 55027;   // Copy to clipboard
   sINFUrlAct4 = 55028;   // Both (open and copy)
   sINFUrlAct5 = 55029;   // Prompt
   sINFUrlAct6 = 55030;   // Do nothing
   sINFUrlAct7 = 55031;   // Create or Modify

   sLng01 = 54925;   // Internal Language (English) will be established next time you start KeyNote NF
   sLng02 = 54926;   // Language file not found:
   sLng03 = 54927;   // Tip file not found:
   sLng04 = 54896;   // Applying Language file  "

   sLnk01 = 54606;   // Folder ID not found: %d
   sLnk02 = 54607;   // Folder name not found: %s
   sLnk03 = 54576;   // Node ID not found: %d
   sLnk03b = 54577;   // Node GID not found: %d
   sLnk04 = 54578;   // Node name not found: %s
   sLnk05 = 54579;   // Select file to link to
   sLnk06 = 54580;   // Select file to insert
   sLnk07 = 54581;   // The file you selected is not a plain-text or RTF file and cannot be inserted.
   sLnk08 = 54582;   // Cannot insert link to a KeyNote location, because no location has been marked. First, mark a location to which you want to link.
   sLnk09 = 54583;   //  Location inserted
   sLnk10 = 54584;   //  Current location marked
   sLnk11 = 54585;   //  Failed to open location
   sLnk12 = 54586;   // Location does not exist or file cannot be opened: "%s"
   sLnk13 = 54587;   // Invalid location string: %s
   sLnk14 = 54588;   //  Invalid location
   sLnk15 = 54589;   // Error executing hyperlink: %s
   sLnk17 = 54590;   //  URL modified
   sLnk18 = 54591;   //  URL action canceled
   sLnk19 = 54560;   //  URL copied to clipboard
   sLnk20 = 54561;   // Error %d executing hyperlink "%s": "%s"
   sLnk21 = 54562;   //  History error
   sLnk22 = 54563;   //  Cannot navigate to history location
   sLnk23 = 54564;   //  History navigation error
   sLnk24 = 54565;   // Navigate backwards in history
   sLnk25 = 54566;   // Navigate backwards in folder ('local') history
   sLnk26 = 54567;   // Navigate backwards in global history
   sLnk27 = 54568;   // Navigate forward in history
   sLnk28 = 54569;   // Navigate forward in folder ('local') history
   sLnk29 = 54570;   // Navigate forward in global history
   sLnk30 = 54571;   //  (Ctrl+click: only in folder history)
   sLnk31 = 54572;   //  [Mark: %d]
   sLnk32 = 54573;   //    (Undo to remove new hidden markers)
   sLnk33 = 54574;   // Action canceled

   sMac01 = 54817;   // Invalid macro header
   sMac02 = 54818;   // Invalid macro version information
   sMac03 = 54819;   // Error while loading macro "%s": %s\^\^Continue loading macros?
   sMac04 = 54820;   // Unexpected error while loading macro "%s": %s
   sMacC01 = 54824;   // string
   sMacC10 = 54830;   // Inserts a line of text into active note
   sMacC11 = 54831;   // Pauses for a specified period (miliseconds)
   sMacC12 = 54800;   // Rewinds macro to beginning and starts again (macro will run in infinite loop until ESC key pressed)
   sMacC13 = 54801;   // Displays a dialog box with a message text
   sMacC14 = 54802;   // Displays message in status bar
   sMacC15 = 54803;   // Executes the specified plugin
   sMacC16 = 54804;   // Executes the specified macro
   sMacC17 = 54805;   // Displays an OK / CANCEL dialog box and aborts macro if CANCEL is pressed
   sMacC18 = 54806;   // Turns ON specified font style
   sMacC19 = 54807;   // Turns OFF specified font style
   sMacC20 = 54808;   // Toggles specified font style
   sMacC21 = 54809;   // Moves caret left
   sMacC22 = 54810;   // Moves caret right
   sMacC23 = 54811;   // Moves caret down
   sMacC24 = 54812;   // Moves caret up
   sMacC25 = 54813;   // Selects all text in active note
   sMacC26 = 54814;   // Selects new font color
   sMacC27 = 54815;   // Selects new background color
   sMacC28 = 54784;   // Selects new highlight color
   sMacC29 = 54785;   // Creates a new standard RTF note
   sMacC30 = 54786;   // Creates a new tree-type note
   sMacC31 = 54787;   // Applies named style to text
   sMacC32 = 54788;   // Sets a new bookmark
   sMacC33 = 54789;   // Jumps to previously set bookmark
   sMacCcn = 54828;   // color name
   sMacCfn = 54826;   // filename
   sMacCint = 54827;   // integer
   sMacCms = 54825;   // miliseconds
   sMacCstN = 54829;   // style name
   sMacE01 = 54821;   // New macro
   sMacE02 = 54822;   // Macro name cannot be blank.
   sMacE03 = 54823;   // Another macro with this name already exists. Macro names must be unique.
   sMacM01 = 54790;   // Stop recording macro
   sMacM02 = 54791;   // &Stop Recording Macro
   sMacM03 = 54792;   //  Recording macro "%s"
   sMacM04 = 54793;   // Command "%s" cannot be included in macros. This command has been executed but will not be recorded in the macro.
   sMacM05 = 54794;   // You have executed a command which opens a dialog box. KeyNote can remember the values you have just selected and use them when replaying the macro, OR KeyNote can display the dialog box again to let you select values each time the macro is executed. Do you want KeyNote to remember current values?\^\^Click YES to save current values. Click NO to always open the dialog box. Click CANCEL to skip this command and continue recording macro.
   sMacM06 = 54795;   //  Macro recording PAUSED
   sMacM07 = 54796;   //  Macro recording RESUMED
   sMacM08 = 54797;   // Macro "%s" contains no commands and will be discarded.
   sMacM09 = 54798;   // Save new macro "%s" (%d lines)?
   sMacM10 = 54799;   // Error saving macro "%s": %s
   sMacM11 = 54768;   //  Macro recorded and saved.
   sMacM12 = 54769;   //  Macro discarded.
   sMacM13 = 54770;   //  Macro error
   sMacM14 = 54771;   // Error adding new macro "%s": %s
   sMacM15 = 54772;   // Record a new macro
   sMacM16 = 54773;   // &Record Macro...
   sMacM17 = 54774;   // Active folder "%s" is Read-only. Running the macro may cause the folder to be modified. Do you want the macro to run anyway?
   sMacM19 = 54775;   // Running macro "%s"
   sMacM20 = 54776;   // Execute most recent macro "%s"
   sMacM21 = 54777;   // Error loading macro "%s": %s
   sMacM22 = 54778;   // Cannot execute macro "%s": This macro requires a newer version of KeyNote.
   sMacM23 = 54779;   //  Unexpected error while executing macro
   sMacM24 = 54780;   //  Macro was aborted by user
   sMacM25 = 54781;   //  Macro done
   sMacM26 = 54782;   // OK to delete selected macro "%s"?
   sMacM27 = 54783;   // Cannot delete macro file "%s"
   sMacM28 = 54752;   // Error while deleting macro:
   sMacM29 = 54753;   // Macro aborted on line %d: "%s"\^Reason: %s
   sMacM30 = 54754;   // unknown command
   sMacM31 = 54755;   // syntax error
   sMacM32 = 54756;   // unknown user command
   sMacM33 = 54757;   // string argument required
   sMacM34 = 54758;   // integer argument required
   sMacM35 = 54759;   // Cannot run embedded macro "%s". Reason: %s
   sMacM36 = 54760;   // Folder creation failed
   sMacM37 = 54761;   // Invalid font style argument
   sMacM38 = 54762;   // Unexpected error while executing macro: %s\^\^Last macro line was: "%s" (line %d)
   sMacM39 = 54763;   // This command cannot be executed while macro is being recorded or replayed.
   sMacM40 = 54764;   // Macro "%s" not found.
   sMacM41 = 54765;   // No macros available or none selected.
   sMacM42 = 54766;   // Could not access current macro.
   sMacM43 = 54767;   //  This command cannot be repeated
   sMacM44 = 54736;   // This action cannot be performed, because there is no active folder (%d)
   sMacM45 = 54737;   // This folder cannot be set as Read-only, because it is being used for clipboard capture.
   sMacM46 = 54738;   // Failed to assign font attributes.
   sMacM47 = 54739;   // Failed to assign paragraph attributes.
   sMacM48 = 54740;   // Go to line
   sMacM49 = 54741;   // Enter line number or increment (+-):
   sMacM51 = 54742;   // Cannot perform command:
   sMacM52 = 54743;   // No font attributes to paste from: Use "Copy font attributes" first.
   sMacM53 = 54744;   // No paragraph attributes to paste from: Use "Copy paragraph attributes" first.
   sMacM54 = 54745;   // "%s" is not a valid number
   sMacM55 = 54746;   // New background color will be assigned to ALL TREE NODES in folder %s\^Continue?
   sMacM56 = 54747;   // Repeat %s
   sMacM57 = 54748;   // Select macro to execute
   sMacM58 = 54749;   // Failed to copy text formatting

   sMain01 = 54482;   // Unable to assign "%s" as activation hotkey.
   sMain02 = 54483;   // Unexpected error while turning %s Activation hotkey "%s": %s
   sMain03 = 54484;   // &Restore (%s)
   sMain04 = 54485;   // &Restore
   sMain06 = 54486;   // Revert to last saved version of\^%s?
   sMain07 = 54487;   // OK to quit %s?
   sMain08 = 54488;   // Unexpected error:  %s\^\^This message may indicate a bug in KeyNote NF. If the problem persists, please submit a bug reports with the Issue Manager available in KeyNote NF website: %s\^\^You can continue working or terminate KeyNote NF. \^Terminate application?
   sMain09 = 54489;   // KeyNote NF Error
   sMain10 = 54490;   // Cannot perform operation:
   sMain11 = 54491;   //  INS
   sMain12 = 54492;   //  OVR
   sMain13 = 54493;   // KeyNote NF have been configured to allow only one instance at a time\^Closing this instance...
   sMain14 = 54494;   // There was a non-fatal error while loading program configuration: \^%s\^\^Some options may have been reset to factory default values. The application will now continue.
   sMain17 = 54495;   // You seem to have upgraded KeyNote from version %s to %s.\^Files "history.txt" and "%s" contain information about the latest changes and additions.\^\^Do you want to view the file "history.txt" now?
   sMain19 = 54464;   // Custom date formats reloaded (%d)
   sMain20 = 54465;   // Cannot load custom %s formats from %s. Check if the file exists.
   sMain20a = 54466;   // date
   sMain20b = 54467;   // time
   sMain21 = 54468;   // Custom time formats reloaded (%d)
   sMain25 = 54469;   // no file is open
   sMain26 = 54470;   // currently open file has no folders
   sMain27 = 54471;   // Folder is Read-Only
   sMain29 = 54472;   // Printing folder...
   sMain97 = 55091;   // Print Previewing...
   sMain98 = 55092;   // Print preview: Page %d of %d
   sMain99 = 55093;   // Page_%d.%s
   sMain30 = 54473;   //  Finished printing/previewing
   sMain34 = 54474;   // Set alarm... (Ctrl:Add  Shift:->Folder)
   sMain40 = 54475;   // Untitled
   sMain42 = 54476;   //  (no file)
   sMain43 = 54477;   //  Auto
   sMain44 = 54478;   //  MOD
   sMain45 = 54479;   //  Saved
   sMain51 = 54448;   // Search and register note dates: creation and last modified
   sMain52 = 54449;   // Remove date prefixes from node names
   sMain53 = 54450;   //  (Ctrl: Reconsider dates)
   sMain56 = 54451;   // Parser stack overflow
   sMain57 = 54452;   // Bad cell range
   sMain58 = 54453;   // Expected expression
   sMain59 = 54454;   // Expected operator
   sMain60 = 54455;   // Expected opening parenthesis
   sMain61 = 54456;   // Expected operator or closing parenthesis
   sMain62 = 54457;   // Invalid numeric expression
   sMain63 = 54458;   // Cannot evaluate:
   sMain64 = 54459;   // Error at position
   sMain65 = 54460;   // No notes in file
   sMain66 = 54461;   // Find tree node
   sMain67 = 54462;   // Find node containing text:
   sMain68 = 54463;   //  Node not found!
   sMain69 = 54432;   // The Style toolbar must be visible to use this command. Show the Style toolbar now?
   sMain70 = 54433;   // No style available or none selected
   sMain71 = 54434;   // Error: StyleManager does not exist.
   sMain72 = 54435;   // Save tree structure to file
   sMain81 = 54436;   // Could not open KeyNote file "%s"
   sMain82 = 54437;   // This command will start your browser and direct it to KeyNote NF website, where you can download the latest version of the program, read the FAQ, submit bug reports or feature requests with the Issue Manager. \^\^Continue?
   sMain83 = 54438;   // Hide &Resource Panel
   sMain84 = 54439;   // Show &Resource Panel
   sMain85 = 54440;   // Results
   sMain86 = 54441;   // Options
   sMain87 = 54442;   // Cannot hide the last visible tab. At least one tab must remain visible on the resource panel.
   sMain88 = 54443;   // Resource panel position will be updated after KeyNote is restarted.
   sMain89 = 54444;   // External: %s
   sMain90 = 54445;   //  File:
   sMain91 = 54446;   //  Node:
   sMain92 = 54447;   // %s Folder: %s%s
   sMain93 = 54416;   // Double-click to insert selected template
   sMain94 = 54417;   // Toolbar configuration file "%s" not found. Default toolbar configuration file has been created.
   sMain95 = 54418;   // Saved toolbar layout to "%s".
   sMain96 = 54419;   // Starting number for numbered paragraphs:

   sOpt01 = 54881;   //  Custom icons are DISABLED
   sOpt02 = 54882;   // Maximum size for clipboard capture text is not a valid integer value.
   sOpt03 = 54883;   // (invalid date format)
   sOpt04 = 54884;   // (invalid time format)
   sOpt05 = 54885;   // OK to reset tab fonts and colors to default state?
   sOpt06 = 54886;   //  icon %d
   sOpt07 = 54887;   // Icons: %d
   sOpt08 = 54888;   // Failed to get icon from
   sOpt09 = 54889;   // Failed to get bitmap from "%s"
   sOpt10 = 54890;   // Cannot delete this icon: List must contain at least 1 icon.
   sOpt11 = 54891;   // OK to delete the selected icon?
   sOpt13 = 54892;   // OK to restore factory default icons?
   sOpt14 = 54893;   // Divider string can contain the following tokens:\^(CASE SENSITIVE)\^\^%s = current date\^%s = current time\^%s = replaced with one line break\^%s = encloses what to show only if source is included\^%s = source URL (with title)\^%s = source URL (with title, limited waiting time (*))\^%s = source URL (without title)\^%s = source server/domain (e.g.: "[YouTube]")\^%s = delimits divider for 2nd+ (same URL)\^\^Remember: Source tokens will be ignored if source is not shown. Also, with %%| you can vary the effective divider depending on whether source URL is shown or not
   sOpt15 = 54894;   // The Auto-Close function will work ONLY if Auto-Save is turned ON, and if no dialog box is open at the time KeyNote tries to automatically close the file. (Auto-Save is currently DISABLED.)
   sOpt16 = 54895;   // Error in TVChange: PageIndex %d  Node.AbsIdx %d
   sOpt17 = 55087;   // Add text file extension
   sOpt18 = 55088;   // Enter new extension for text files:
   sOpt19 = 55089;   // Extension \"%s\" already listed.
   sOpt20 = 55090;   // Reset default text file extensions?

   sOptS00 = 54897;   // General Settings
   sOptS01 = 54898;   // Rich Text Editor
   sOptS02 = 54899;   // Images
   sOptS03 = 54900;   // Tree Panel
   sOptS04 = 54901;   // KeyNote Files
   sOptS05 = 54902;   // File Options
   sOptS06 = 54903;   // Backup Options
   sOptS07 = 54904;   // Actions
   sOptS08 = 54905;   // Confirmations
   sOptS09 = 54906;   // Chrome
   sOptS10 = 54907;   // Tab Icons
   sOptS11 = 54908;   // Advanced
   sOptS12 = 54909;   // Formats
   sOptS13 = 54910;   // Clipboard
   sOptS14 = 54911;   // File Types
   sOptS15 = 54880;   // Other
   sOptS16 = 55106;   // Folding Blocks

   sPass01 = 54873;   // Passphrase cannot be blank.
   sPass02 = 54874;   // File "%s" has content encrypted

   sPlg01 = 54875;   // StatusOK
   sPlg02 = 54876;   // Gets data
   sPlg03 = 54877;   // Gets RTF
   sPlg04 = 54878;   // Gets selection
   sPlg05 = 54879;   // Returns data
   sPlg06 = 54848;   // Returns RTF
   sPlg07 = 54849;   // Returns clipboard
   sPlg08 = 54850;   // Needs selection
   sPlg09 = 54851;   // Wants new note
   sPlg10 = 54852;   // Wants dialog box
   sPlg11 = 54853;   // Wants saved file
   sPlg12 = 54854;   // Reload file
   sPlg13 = 54855;   // Stays resident
   sPlg14 = 54856;   // Unexpected error from DLL:
   sPlgM01 = 54857;   // Select plugin to display information
   sPlgM02 = 54858;   // (version %d)
   sPlgM03 = 54859;   // Filename:
   sPlgM04 = 54860;   // No plugins available.
   sPlgM05 = 54861;   // Could not execute plugin
   sPlgM06 = 54862;   // No plugins available or none selected.
   sPlgM07 = 54863;   // Cannot execute plugin - file not found. (%s)
   sPlgM08 = 54832;   // Execute most recent plugin "%s"
   sPlgM09 = 54833;   // Could not obtain plugin information from "%s". Make sure that the file exists.
   sPlgM10 = 54834;   // Plugin "%s" (%s) reports wrong version number %d. A newer version of KeyNote is required to run this plugin.
   sPlgM11 = 54835;   // Plugin "%s" (%s) refuses to execute.
   sPlgM12 = 54836;   // Could not load plugin "%s" (%s).
   sPlgM13 = 54837;   // Could not execute plugin "%s" (%s).
   sPlgM14 = 54838;   // Resident plugin "%s" is already running. Shut down the running plugin first.
   sPlgM15 = 54839;   // Plugin "%s" tried to go resident, but attempt to set resident plugin ID failed. Plugin will be shut down.
   sPlgM16 = 54840;   // Plugin "%s" requires that text is selected in active note. Select some text and try again.
   sPlgM17 = 54841;   // Plugin returned error code "%d"
   sPlgM18 = 54842;   // Plugin "%s" is now running
   sPlgM19 = 54843;   // Plugin copied data to clipboard. Ok to paste into active note?
   sPlgM20 = 54844;   // Active note "%s" is Read-only. Inserting plugin output will modify the note. Insert anyway?
   sPlgM21 = 54845;   // Unexpected error during plugin cleanup:
   sPlgM22 = 54846;   // Unexpected plugin error:
   sPlgM23 = 54847;   // Resident plugin "%s" unloaded
   sPlgM24 = 54816;   // Unexpected error while shutting down resident plugin "%s": %s

   sSty01 = 54750;   // Face: %s\^Size: %s\^Style: %s\^Color: %s\^Other: %s
   sSty02 = 54751;   //  %s, %s space, %s, L:%d F:%d R:%d, Bef:%d Aft:%d
   sSty03 = 54720;   // Alignment: %s\^Line spacing: %s\^Numbering: %s\^Left indent: %s\^First indent: %s\^Right indent: %s\^Space before: %s\^Space after: %s
   sSty04 = 54721;   // Single
   sSty05 = 54722;   // Double
   sSty06 = 54723;   // other
   sSty07 = 54724;   // Bullets
   sSty08 = 54725;   // Left
   sSty09 = 54726;   // Right
   sSty10 = 54727;   // Center
   sSty11 = 54728;   // Justify
   sSty12 = 54729;   // superscript
   sSty13 = 54730;   // subscript
   sSty14 = 54731;   // Subscript
   sSty15 = 54732;   // Supercript
   sSty16 = 54733;   // Disabled
   sSty17 = 54734;   // Highlighted
   sStyM01 = 54735;   // Style in active note:
   sStyM02 = 54704;   // Font:
   sStyM03 = 54705;   // Paragraph:
   sStyM04 = 54706;   // Named style:
   sStyM05 = 54707;   // Range:
   sStyM06 = 54708;   // Error: StyleManager does not exist.
   sStyM07 = 54709;   // Create "%s" style
   sStyM08 = 54710;   // Enter name for new style:
   sStyM09 = 54711;   // %s style "%s" already exists. \^Redefine the existing style with new properties?
   sStyM10 = 54712;   //  Style "%s" created (%s)
   sStyM11 = 54713;   // Error creating style:
   sStyM12 = 54714;   // Error: Cannot access style information for "%s"
   sStyM13 = 54715;   // Rename style
   sStyM14 = 54716;   // Enter new name for style:
   sStyM15 = 54717;   // Cannot rename: a style by that name already exists.
   sStyM16 = 54718;   // Error renaming style
   sStyM17 = 54719;   // OK to delete %s style "%s"?
   sStyM18 = 54688;   // Error deleting style

   sTip01 = 54864;   // Tip of the day
   sTip02 = 54865;   // Did you know...
   sTip03 = 54866;   // (Tips not found.)

   sTpl01 = 54689;   // Template "%s" already exists. Overwrite existing template?
   sTpl02 = 54690;   // Template "%s" created.
   sTpl03 = 54691;   // Select template to insert
   sTpl04 = 54692;   // OK to delete selected template "%s"?

   sTree01 = 54225;   // Error creating node:
   sTree04 = 54226;   // Initial node not assigned - select a node and retry.
   sTree05 = 54227;   // cannot be
   sTree06 = 54228;   // Error moving node:
   sTree07 = 54229;   // Node "%s" %smoved %s
   sTree08 = 54230;   // \+This operation cannot be undone.
   sTree09 = 54231;   // Node "%s" has %d child nodes. Delete these child nodes too?
   sTree10 = 54232;   // OK to delete node "%s"?
   sTree11 = 54233;   // OK to delete %sALL SELECTED nodes?\^ (Confirmation will be requested for each node with children)\^
   sTree12 = 54234;   // OK to delete %d CHILD NODES of node "%s"?
   sTree13 = 54235;   // Selected node has no children.
   sTree14 = 54236;   // Error deleting node:
   sTree15 = 54237;   // No nodes available for copying or pasting data.
   sTree16 = 54238;   // OK to MOVE %d nodes/subtrees to current node "%s"?
   sTree17 = 54239;   //  No node is selected
   sTree18 = 54208;   // OK to forget %s?
   sTree19 = 54209;   // Target node is included in one of the subtrees to move
   sTree20 = 54210;   //  nodes/subtrees registered for transfer
   sTree21 = 54211;   // No data to paste. Select "Transfer|Copy/Cut Subtree" first.
   sTree22 = 54212;   // \^\^* One or more nodes being copied is a Virtual Node. They will be pasted as linked nodes\^\^Continue?
   sTree23 = 54213;   // OK to PASTE %d nodes/subtrees%s below current node "%s"?\+(Hidden nodes will ALSO be pasted)
   sTree24 = 54215;   //  Pasted %d nodes/subtrees
   sTree25 = 54216;   // %d virtual nodes have been copied as linked nodes
   sTree26 = 54214;   //  as LINKED nodes
   sTree49 = 54217;   // OK to sort the entire tree?
   sTree50 = 54218;   //  Node name cannot be blank!
   sTree51 = 54219;   //  Node renamed.
   sTree52 = 54220;   //  Cannot perform operation: '%s' folder is read-only
   sTree53 = 54221;   // Edit node name
   sTree54 = 54222;   // Enter new name:
   sTree55 = 54223;   // Disable Filter on tree
   sTree56 = 54192;   // Apply Filter on tree
   sTree57 = 54193;   //  (Ctrl: Clear Find filter)
   sTree59 = 54194;   // OK to remove all Flags in folder?
   sTree60 = 54195;   // Last modified >= "%s"
   sTree62 = 54196;   // Name
   sTree63 = 54197;   // Note name
   sTree64 = 54198;   // Date
   sTree65 = 54199;   // Note creation date
   sTree66 = 54200;   // Flagged
   sTree67 = 55135;   // Node (or its children) contains encrypted content! CONTINUE?

   sUInote01 = 54201;   // Created: %s  ==  Last modified: %s

   sUpd01 = 55052;   // You already have the latest version installed
   sUpd02 = 55053;   // There is a new version !
   sUpd03 = 55054;   // No Internet access

   sURL01 = 54869;   // OK
   sURL02 = 54870;   // Create Hyperlink
   sURL03 = 54871;   // Modify
   sURL04 = 54872;   // Choose Action for Hyperlink

   sVCL00 = 54420;   // Click and drag to resize panels (Ctrl: tree max width / Alt: Toggle fixed)
   sVCL01 = 54421;   // Error destroying tabsheet
   sVCL02 = 54422;   // Select text color
   sVCL03 = 54423;   // Select &Highlight...
   sVCL04 = 54424;   // Select highlight color
   sVCL05 = 54425;   // Apply current font color to text
   sVCL06 = 54426;   // Apply &Highlight
   sVCL07 = 54427;   // Apply current highlight color to text
   sVCL12 = 54428;   // Hide &Resource Panel
   sVCL13 = 54429;   // Show &Resource Panel
   sVCL14 = 54430;   // The Resource panel must be visible to use this command. Show the Resource panel now?
   sVCL15 = 54431;   // Use the mouse to apply the %s to another text or press Esc to cancel
   sVCL16 = 54400;   // paragraph formatting
   sVCL17 = 54401;   // font formatting

   sTag1  = 55107;   // Name
   sTag2  = 55108;   // Description / alias
   sTag3  = 55109;   // The tag name cannot be blank or include #, : or spaces
   sTag4  = 55110;   // OK to DELETE Tag "%s"%s?
   sTag5  = 55111;   // OK to DELETE ALL SELECTED tags%s?
   sTag6  = 55112;   // Creating new Tag
   sTag7  = 55113;   // Edit tags applied to the note
   sTag8  = 55114;   // OK to ALSO replace "#%s" with "#%s" within the EXISTING NOTES text?
   sTag9  = 55115;   //  (with ALL REFERENCES)
   sTag10 = 55116;   // OK to ALSO delete ALL REFERENCES within the EXISTING NOTES text?
   sTag11 = 55117;   // No tags selected!
   sTag12 = 55118;   // OK to APPLY / ADD the %d selected Tags to the %d %s NOTES?
   sTag13 = 55119;   // OK to REMOVE the %d selected Tags from the %d %s NOTES?
   sTag14 = 55120;   // visible
   sTag15 = 55121;   // selected
   sTag16 = 55122;   // Error exporting selected Tags:
   sTag17 = 55123;   // Error importing Tags:
   sTag18 = 55124;   // Importing Tags from file


implementation

uses
  KDL.Localizer;

function GetRS(ID: Integer): string;
var
  ResStringRec: TResStringRec;
  ModuleInstance: NativeUInt;
begin
  ResStringRec.Identifier := ID;
  ModuleInstance := NativeUInt(HInstance);
  ResStringRec.Module := @ModuleInstance;
  Result := MyLoadResString(@ResStringRec);
end;

end.
