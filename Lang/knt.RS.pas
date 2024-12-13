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

   STR_minute = 65056;   // minute
   STR_minutes = 65057;   // minutes
   STR_hour = 65058;   // hour
   STR_hours = 65059;   // hours
   STR_day = 65060;   // day
   STR_days = 65061;   // days
   STR_week = 65062;   // week
   STR_weeks = 65063;   // weeks

   FILTER_ALLFILES = 65055;   // All files (*.*)|*.*
   LANGUAGE_DEFAULT = 65024;   // English (Internal)
   Program_Desc = 65051;   // Tabbed notebook for Windows

   STR_SE_ERR_DDETIMEOUT = 65040;   // The DDE transaction could not be completed because the request timed out.
   STR_SE_ERR_DLLNOTFOUND = 65041;   // The specified dynamic-link library was not found.
   STR_SE_ERR_NOASSOC = 65042;   // There is no application associated with the given filename extension.
   STR_SE_ERR_OOM = 65043;   // There was not enough memory to complete the operation.
   STR_SE_ERR_SHARE = 65044;   // A sharing violation occurred
   STR_UNKNOWN_ERROR = 65045;   // Unknown error.
   STR_ERR_OUTOFRESOURCES = 65064;   // The operating system is out of memory or resources.
   STR_ERROR_FILE_NOT_FOUND = 65065;   // The specified file was not found.
   STR_ERROR_PATH_NOT_FOUND = 65066;   // The specified path was not found.
   STR_ERROR_BAD_FORMAT = 65067;   // The .EXE file is invalid (non-Win32 .EXE or error in .EXE image).
   STR_SE_ERR_ACCESSDENIED = 65068;   // The operating system denied access to the specified URL.
   STR_SE_ERR_ASSOCINCOMPLETE = 65069;   // The filename association is incomplete or invalid.
   STR_SE_ERR_DDEBUSY = 65070;   // The DDE transaction could not be completed because other DDE transactions were being processed.
   STR_SE_ERR_DDEFAIL = 65071;   // The DDE transaction failed.


   sAB00 = 65046;   // About -
   sAB01 = 65047;   // Double-click to send email; Right-click to copy\^(No HTML-formatted email, PLEASE!)
   sAB02 = 65048;   // Double-click to visit home page; Right-click to copy
   sAB03 = 65049;   // Keynote was inspired by a fantastic freeware prog: DaRT Notes\^by Andre v.d. Merwe (See "dart.txt" for information)
   sAB04 = 65050;   // KeyNote NF is an evolution of KeyNote (by Marek)

   sAlrt01 = 64632;   // %d alarms selected
   sAlrt02 = 64633;   // Set Alarm
   sAlrt03 = 64634;   // %d Reminders
   sAlrt04 = 64635;   // All Alarms/Events (%d)
   sAlrt05 = 64636;   // Overdue Events (%d)
   sAlrt06 = 64637;   // Pending Reminders (%d)
   sAlrt07 = 64638;   // Discarded Events (%d)
   sAlrt08 = 64639;   // ALARM [%s] :  %s
   sAlrt09 = 64608;   // [Sound ON]
   sAlrt10 = 64609;   // [Sound OFF]
   sAlrt11 = 64610;   // %d pending reminders, %d overdue
   sAlrt12 = 64611;   // [Popup ON]
   sAlrt13 = 64612;   // [Popup OFF]
   sAlrt14 = 64613;   // Expiration/Start time and/or Reminder interval are not valid.\^Please, correct it
   sAlrt15 = 64614;   // OK to discard all this %d alarms?
   sAlrt16 = 64615;   // OK to remove all this %d alarms?
   sAlrt17 = 64616;   // OK to restore all this %d alarms?
   sAlrt18 = 64617;   // OK to remove this alarm?
   sAlrt19 = 64618;   // OK to apply pending changes?
   sAlrt20 = 64619;   // Today
   sAlrt21 = 64620;   // Tomorrow
   sAlrt22 = 64621;   // All
   sAlrt23 = 64622;   // Overdue
   sAlrt24 = 64623;   // Pending
   sAlrt25 = 64592;   // Discarded
   sAlrt26 = 64593;   // All (with discarded)
   sAlrt27 = 64594;   // Show all pending reminders (triggered and ignored, not postponed nor discarded)
   sAlrt28 = 64595;   // Show all overdue events
   sAlrt29 = 64596;   // Show all set alarms (not discarded)
   sAlrt30 = 64597;   // Show all set alarms, including discarded
   sAlrt31 = 64598;   // Show All Dates
   sAlrt32 = 64599;   // Filter on selected Days
   sAlrt33 = 64600;   // Filter on selected Week
   sAlrt34 = 64601;   // Filter on selected Month
   sAlrt35 = 64602;   // (Filter applied)
   sAlrt36 = 64603;   // (%s overdue)
   sAlrt37 = 64604;   // (%s left)
   sAlrt38 = 64605;   // (%s before)

   sApp01 = 64502;   //  Cannot perform operation: Editor is Read-Only
   sApp02 = 64503;   // There is no active editor
   sApp03 = 64504;   // Function not implemented.
   sApp04 = 64505;   // (none)
   sApp05 = 64506;   //  Select some text before issuing this command.
   sApp06 = 64507;   // Unexpected or not implemented command:
   sApp07 = 64508;   // Unexpected error.
   sApp08 = 64509;   // \^\^Number of nodes (notes) in tree: %d
   sApp09 = 64510;   // Chars: %d  Alph: %d  Words: %d
   sApp10 = 64511;   // \^\^Clik OK to copy information to clipboard.
   sApp11 = 64480;   // Cannot display Tip of the Day: file "%s" not found.
   sApp12 = 64481;   // : Tip of the Day

   sBmk01 = 64648;   //  Bookmark %d assigned.
   sBmk02 = 64649;   //  Bookmark %d not assigned!
   sBmk03 = 64650;   //  Cannot access bookmark %d - Cleared
   sBmk04 = 64651;   //  Jumped to bookmark %d

   sCfg01 = 64693;   // Error in keyboard customization procedure:
   sCfg02 = 64694;   //  Customize Tab icons (%s)
   sCfg03 = 64695;   // Invalid command line arguments:
   sCfg04 = 64696;   // Error while loading custom keyboard configuration from %s: "%s"
   sCfg05 = 64697;   // There was a non-fatal error while loading defaults: \^%s\^\^Some settings may have been reset to defaults.

   sChest01 = 64499;   // Failed to load built-in category images from resource.
   sChest02 = 64500;   // Failed to load category images from
   sChest03 = 64501;   // Failed to save category images to

   sChrs01 = 64497;   //  Edit
   sChrs02 = 64498;   //  Done

   sDef00 = 64957;   // OK
   sDef01 = 64959;   // Folder Properties: %s
   sDef02 = 64928;   // Close
   sDef03 = 64929;   // Folder is Read-Only: cannot change properties
   sDef04 = 64930;   //  [RO]
   sDef05 = 64931;   //  View properties for current folder
   sDef06 = 64932;   // Change properties for current folder
   sDef07 = 64933;   // &Save as default for "%s"
   sDef08 = 64934;   // Defaults for
   sDef09 = 64935;   // Change Defaults for NEW folders in THIS FILE
   sDef0B = 64958;   // Accept changes and close dialog box
   sDef10 = 64936;   // Defaults for all files
   sDef11 = 64937;   // Change default properties for all NEW folders
   sDef12 = 64938;   // Folder name cannot be blank. Please enter a name.
   sDef13 = 64939;   // Folder name cannot contain the "%s" character
   sDef14 = 64940;   // Node name cannot contain the "%s" character
   sDef15 = 64941;   // OK to reset Editor font and color settings to default values?
   sDef16 = 64942;   // OK to reset Tree font and color settings to default values?
   sDef17 = 64943;   // Tokens for autonaming tree nodes:
   sDef18 = 64912;   // (must be UPPERCASE)
   sDef19 = 64913;   //  = current date
   sDef20 = 64914;   //  = current time
   sDef21 = 64915;   //  = total number of nodes
   sDef22 = 64916;   //  = new node's level
   sDef23 = 64917;   //  = new node's index
   sDef24 = 64918;   //  = new node's absolute index
   sDef25 = 64919;   //  = parent node's name
   sDef26 = 64920;   //  = name of active folder
   sDef27 = 64921;   //  = name of currently open file
   sDef28 = 64922;   // <no icon>
   sDef29 = 64923;   // Invalid zoom ratio:
   sDef30 = 64924;   //  (and apply to "%s" folder)
   sDef31 = 64202;   // REMEMBER:\^\^- Folder settings apply only to NEW notes, except:\^   - 'Plain note only': modifies ALL the notes\^   - 'WordWrap': affects ALL the notes\^       (not explicitly set WordWrap previously)\^\^>> More info in Help File (F1)
   sDef32 = 64203;   // REMEMBER:\^\^- Font change affect only to NEW nodes (all if Plain note)\^- BG Color depends on 'Inherit BG color from active node':\^   - If set, background color of selected node is shown\^     (=> BG color of its new child nodes)\^   - If not set, default BG color for all NEW nodes is shown\^   * To edit this option -> F5 | General settings| Rich Text editor\^\^- BG Color can be changed for ALL nodes in a Folder:\^    [Shift] + "Format | Background color"\^\^>> More info in Help File (F1)
   sDef33 = 64204;   // REMEMBER:\^\^- BG Color sets backgroud color for the Tree Panel and\^  default BG color of tree nodes\^- Previous changes to individual nodes won't be affected\^- 'Inherit properties from active node' option is\^  considered in NEW nodes\^- Font and BG color can be changed for ALL tree panels at once:\^    "Apply to ALL folders"\^\^- Note: 'Inherit BG color from active node' option does NOT\^  affect (refers to Editor) \^\^>> More info in Help File (F1)

   sDll01 = 64867;   // Error while attempting to load runtime library "%s". Please reinstall KeyNote.
   sDll02 = 64868;   // Procedure "%s" not found in runtime library "%s". Please reinstall KeyNote.

   sEdt01 = 64575;   // Invalid zoom ratio:
   sEdt02 = 64544;   //  L %d / %d  C %d
   sEdt03 = 64545;   //  Sel: %d  W: %d
   sEdt04 = 64546;   //  Overwrite mode disabled through INI file
   sEdt05 = 64547;   // Convert decimal to Roman
   sEdt06 = 64548;   // Enter a decimal number:
   sEdt07 = 64549;   // %s is not a valid number
   sEdt08 = 64550;   // Convert Roman to decimal
   sEdt09 = 64551;   // Enter a Roman number:
   sEdt10 = 64552;   // %s is not a valid Roman number
   sEdt11 = 64553;   //  No valid bracket at cursor position
   sEdt12 = 64554;   //  Matching bracket FOUND
   sEdt13 = 64555;   //  Matching bracket NOT FOUND
   sEdt14 = 64556;   // OK to trim white space characters in whole note?
   sEdt15 = 64557;   // OK to compress white space characters in whole note?
   sEdt16 = 64558;   //  Result:
   sEdt17 = 64559;   // Paste last eval result:
   sEdt18 = 64528;   // Expression %s evaluates to: %s\^\^Result was copied to clipboard. Click OK to insert.
   sEdt19 = 64529;   // Select image to insert
   sEdt20 = 64530;   // All image files
   sEdt21 = 64531;   //  Function not available
   sEdt22 = 64532;   //  No word at cursor
   sEdt23 = 64533;   //  Word not in glossary. Use Shift+F7 to add.
   sEdt24 = 64534;   // Term expansion glossary "%s" is not loaded.
   sEdt25 = 64535;   // Glossary term already exists: "%s" -> "%s". OK to redefine term as "%s"?
   sEdt26 = 64536;   //  Added to glossary: "%s" -> "%s"
   sEdt27 = 64537;   // Replace editor contents with result from spellchecker?
   sEdt28 = 64538;   //  Calculating statistics... Please wait
   sEdt29 = 64539;   // Selected text
   sEdt30 = 64540;   // Folder text
   sEdt31 = 64541;   // %s statistics\^\^Characters: %s\^Alphabetic: %s\^Whitespace: %s\^\^Words: %s\^Lines: %s
   sEdt32 = 64542;   // Lookup in WordWeb
   sEdt33 = 64543;   // Enter word to look up:
   sEdt34 = 64512;   // Error loading WordWeb. The program may not be installed on your computer. See file "wordweb.txt" for more information.\^\^Error message:
   sEdt35 = 64513;   // UAS path
   sEdt36 = 64514;   // Please specify full path to uas.exe
   sEdt37 = 64515;   // KeyNote cannot find the location of uas.exe. UltimaShell Autocompletion Server will not be loaded.
   sEdt38 = 64516;   //  UltimaShell Autocompletion Server loaded.
   sEdt39 = 64517;   // Cannot load UltimaShell Autocompletion Server. It may not be installed. Would you like to go to the UAS website and download the application?
   sEdt40 = 64518;   //  UltimaShell Autocompletion Server unloaded.
   sEdt41 = 64519;   //  UltimaShell Autocompletion Server is not loaded.
   sEdt42 = 64520;   // A Read-Only folder cannot be used for clipboard capture.
   sEdt43 = 64521;   // a new node
   sEdt44 = 64522;   // whichever node is currently selected
   sEdt45 = 64523;   // Each copied item will be pasted into %s in the tree. Continue?
   sEdt46 = 64524;   //  Clipboard capture is now
   sEdt47 = 64525;   //  Capturing text from clipboard
   sEdt48 = 64526;   //  Clipboard capture done
   sEdt49 = 64527;   // Current folder contains more than one node. Do you want to print all nodes? Answer No to only print the selected node.
   sEdt50 = 64496;   // CRC calculation error in clipboard capture, testing for duplicate clips will be turned off. Message:

   sExp01 = 64274;   // Error while importing HTML text:
   sExp02 = 64275;   // Error while exporting to HTML (method=
   sExpFrm00 = 64276;   // Export node content
   sExpFrm01 = 64277;   // Exporting is underway. OK to abort?
   sExpFrm02 = 64278;   // Please select a valid directory for exported files.
   sExpFrm03 = 64279;   // Specified output directory does not not exit. Please select a valid directory.
   sExpFrm04 = 64280;   // You did not select any foldersnotes for exporting.
   sExpFrm11 = 64281;   // Error while exporting folders:
   sExpFrm12 = 64282;   // Exported  %d folders (%d notes).
   sExpFrm13 = 64283;   // Exporting was aborted due to an error.
   sExpFrm14 = 64284;   // Exporting was aborted at user request.
   sExpFrm15 = 64285;   // The following token can be used in headings:\^\^%s%s - Filename\^%s%s - Folder name\^%s%s - Node name\^%s%s - Node level\^%s%s - Node index\^%s%s - Line break\^%s%s - Symbols, increasing\^%s%s - Symbols, decreasing\^\^F1 => More INFO and usage examples
   sExpFrm16 = 64286;   // No active tree node: select a node first.
   sExpFrm17 = 64287;   // Current node has no text: nothing to export.
   sExpFrm18 = 64256;   //  Node exported to
   sExpFrm19 = 64257;   // Error exporting node:
   sExpFrm20 = 64258;   // 'Current node' will be managed as 'Current node and subtree' for KeyNote format\^ Continue?

   sFav01 = 64653;   // Error loading Favorites:
   sFav02 = 64654;   // Rename favorite location
   sFav03 = 64655;   // Enter new name:
   sFav04 = 64624;   // A favorite named "%s" already exists. Choose another name
   sFav05 = 64625;   // Error renaming favorite location:
   sFav06 = 64626;   // Favorite KeyNote location
   sFav07 = 64627;   // Enter location name:
   sFav08 = 64628;   //  or click Cancel to abort.
   sFav09 = 64629;   // Delete "%s" from Favorites?
   sFav10 = 64630;   // Error deleting Favorite:
   sFav11 = 64631;   // Favorites list error:
   sFavDlg01 = 64652;   // The specified file does not exist. Do you want to use the filename anyway?

   sFDrp01 = 64398;   // file
   sFDrp02 = 64399;   // files
   sFDrp03 = 64368;   // Select import method (%d *%s %s)
   sFDrp04 = 64369;   // &General options
   sFDrp06 = 64370;   // &HTML options
   sFDrp07 = 64371;   // Some files will be renamed

   sFile01 = 64372;   // Cannot open "%s": File not found
   sFile02 = 64373;   // Invalid file header in "%s" (not a KeyNote file)
   sFile03 = 64374;   // Access passphrase not specified: cannot open encrypted file.
   sFile04 = 64375;   // The passphrase is invalid. Try again?
   sFile05 = 64376;   // %s: This file was created with a version of KeyNote later than the version you are using. Expected version ID: "%s.%s" This file version ID: "%s.%s"  You need the latest version of KeyNote to open this file.
   sFile06 = 64377;   // : This file was created with a version of KeyNote newer than the version you are using. The file can be opened, but some information can be lost or misinterpreted. As a safety measure, the file should be opened in Read-Only mode. Would you like to open the file as Read-Only?
   sFile07 = 64378;   // %s: Invalid file header or version, or corrupt file.
   sFile08 = 64379;   // Error loading folder
   sFile10 = 64380;   // This file contains notes which are not compatible with %s format. Only %s notes can be saved in this format.
   sFile12 = 64381;   // Error: Filename not specified.
   sFile13 = 64382;   // Error while saving folder "%s": %s
   sFile14 = 64383;   // Cannot save: Passphrase not set
   sFile15 = 64352;   // Stream size error: Encrypted file is invalid or corrupt.
   sFile16 = 64353;   // Invalid passphrase: Cannot open encrypted file.
   sFile18 = 64354;   // OK to convert to PLAIN TEXT current note?\^\^ALL IMAGES and FORMATTING will be REMOVED !!
   sFile19 = 64355;   // Exception trying to ensure plain text and removing of images:
   sFile20 = 64356;   // Virtual note "%s" cannot write file
   sFile21 = 64357;   // OK to deduce the missing date information?\^
   sFile22 = 64358;   // OK to remove date from note name?\^
   sFile23 = 64359;   // All (or selected) nodes will be considered
   sFile24 = 64360;   // \^\^Please read the help file before proceeding. Search for "Deduce Dates"

   sFileM01 = 64361;   // Cannot create a new file:
   sFileM02 = 64362;   //  New KNT file created.
   sFileM04 = 64363;   // A new KNT file has been created. Would you like to save the new file now?\^\^(The Auto Save function will not work until the file is named and saved first.)
   sFileM05 = 64364;   // Open Keynote file
   sFileM06 = 64365;   //  Opening
   sFileM07 = 64366;   // One or more errors occurred while loading the file. The file may not have loaded completely. To minimize the risk of data loss, the file was opened in Read-Only mode. Use the "Save As..." command to save the file.
   sFileM08 = 64367;   //  <unknown>
   sFileM09 = 64336;   //  diskette
   sFileM10 = 64337;   //  network
   sFileM11 = 64338;   //  CD-ROM
   sFileM12 = 64339;   //  RAM
   sFileM13 = 64340;   // File "%s" was opened in Read-Only mode, because it resides on a %s drive "%s".
   sFileM14 = 64341;   //  File opened.
   sFileM15 = 64342;   //  Error.
   sFileM16 = 64343;   // Folder monitor error:
   sFileM17 = 64344;   //  ERROR %d opening file
   sFileM19 = 64345;   //  Saving
   sFileM20 = 64346;   // Specified backup directory "%s" does not exist. Backup files will be created in the original file's directory.
   sFileM21 = 64347;   // Cannot create backup file (error %d: %s). Current file will not be backed up. Proceed anyway?\^\^ (Note: File was temporary saved in %s)
   sFileM22 = 64348;   //  File saved (%d folders, %d notes)
   sFileM23 = 64349;   //  Error %d while saving file.
   sFileM24 = 64351;   // Error %d occurred while saving to a temporal folder (%s). The contents of the file in memory are perhaps partially corrupted.\^\^-> Please, retry, and if you can'nt save to a .knt file, try to recover the nodes/notes with unsaved changes using, for example, File -> Export...\^\^\^
   sFileM25 = 64320;   // Failed to create output file "%s" (Error: %d)\^File was temporary saved in %s\^\^\^
   sFileM26 = 64321;   // The Auto-Save option was turned OFF, to prevent KeyNote from automatically saving the (perhaps) damaged file.
   sFileM27 = 64322;   //  ERROR saving file
   sFileM28 = 64323;   // Saving "
   sFileM29 = 64324;   // Folder monitoring has been disabled due to the following error:
   sFileM30 = 64325;   //  File closed.
   sFileM32 = 64326;   // Select backup folder
   sFileM33 = 64327;   // Cannot copy file to its own directory.
   sFileM34 = 64328;   // The file %s already exists. OK to overwrite existing file?
   sFileM35 = 64329;   //  Copying file...
   sFileM36 = 64330;   //  File copied.
   sFileM37 = 64331;   // Successfully copied KNT file to
   sFileM38 = 64332;   // Copying failed (
   sFileM39 = 64333;   // Select file to merge folders from
   sFileM40 = 64334;   // There was an error while loading merge file.
   sFileM41 = 64335;   // The file you selected does not contain any folders.
   sFileM42 = 64304;   // Error while loading merge file:
   sFileM43 = 64305;   // Folders in %s
   sFileM44 = 64306;   // You did not select any folder: nothing to merge.
   sFileM45 = 64307;   //  Merging folders...
   sFileM46 = 64308;   // Error while adding folders:
   sFileM47 = 64309;   // Merged %d folders from "%s"
   sFileM48 = 64310;   // No folders were merged
   sFileM49 = 64312;   // Another application has modified the knt file %s. Reload the file from disk?
   sFileM50 = 64313;   // %s folder "%s" does not exist
   sFileM51 = 64314;   // . Create the folder now?
   sFileM52 = 64315;   // Could not create folder: %s
   sFileM53 = 64316;   //  File modified by external application.
   sFileM54 = 64317;   // Folders were modified. Save file before continuing?\^If you answer No, you will lose all changes made since last save.
   sFileM55 = 64318;   // Current file has not been saved. If you continue, changes will be lost.\^Proceed anyway?
   sFileM56 = 64319;   // Warning!
   sFileM57 = 64288;   // Select files for importing
   sFileM58 = 64289;   // The file "%s" does not appear to be a text file (nor image). The result of importing it may be unpredictable.\^\^Import as a plain text file, anyway?
   sFileM59 = 64290;   //  Importing
   sFileM60 = 64291;   // Failed to convert HTML file "%s" to RTF
   sFileM61 = 64292;   // Error importing
   sFileM62 = 64293;   //  Finished importing.
   sFileM63 = 64294;   // Cannot select methods for handling files.
   sFileM65 = 64295;   // Cannot import a directory "%s"
   sFileM67 = 64296;   // Unknown or unexpected file action (%d)
   sFileM68 = 64297;   // Error while importing files:
   sFileM75 = 64298;   // Successfully created %s registry entries
   sFileM76 = 64299;   // There was an error while creating file type associations:
   sFileM77 = 64300;   // This file is Read-Only. Use "Save As" command to save it with a new name.
   sFileM78 = 64301;   // Backup at %s before any modification in "%s"
   sFileM79 = 64302;   // File is not modified. Nothing to save
   sFileM80 = 64303;   // \^\^Option "Autoregister file type" will be unchecked
   sFileM81 = 64272;   // Cannot insert images in a plain text folder
   sFileM82 = 64273;   // The file must first be saved (with Save or Save As)
   sFileM83 = 64311;   // %d Links or Mirror nodes couldn't be adapted\^Links can be found searching for "file///<%d"
   sFileMInfSaving = 64350;   // * NOTE:\^  - The .knt file in disk must not have been modified from last correct save.\^  - You should have multiple backup files in the folder %s, specially if you selected the option "Backup at regular intervals" (highly recommended)

   sFInf01 = 64410;   //  file
   sFInf02 = 64411;   // File properties:
   sFInf03 = 64412;   //  bytes
   sFInf04 = 64413;   // (file not saved)
   sFInf05 = 64414;   // never
   sFInf06 = 64415;   // Open "%s" as &Read-Only
   sFInf07 = 64384;   // (none)
   sFInf09 = 64385;   // The passphrase you entered is too short: Minimum passphrase length is %d characters
   sFInf10 = 64386;   // The passphrases you entered do not match. Please enter the exact same passphrase twice.
   sFInf11 = 64387;   // You chose to encrypt a file that contains virtual nodes. Note that the disk files linked to virtual nodes and images saves in external storage (Zip or Folder) will NOT be encrypted.\^\^Continue?
   sFInf12 = 64388;   // File "%s" was open in READ-ONLY mode. If you uncheck this box, the read-only mode will be turned OFF. Continue?
   sFInf13 = 64389;   // Open images storage folder
   sFInf14 = 64390;   // Open images storage file
   sFInf15 = 64391;   // Set
   sFInf16 = 64392;   // Must save KNT before change images storage again
   sFInf17 = 64393;   // (*) Missing current external storage
   sFInf18 = 64394;   // New images will be saved provisionally [only] as Embedded KNT\^Deletions will be effective when it is available\^\^(It may be totally fine if you temporarily lose access to image storage)
   sFInf19 = 64395;   // Current Next ID (%d) cannot be reduced\^(Max ID in image list is %d)
   sFInf20 = 64396;   // Max ID in image list is %d and Next ID is %d\^\^Do you want the NEXT image to be saved with ID = %d \^\^* YOU MUST MAKE SURE there are no images with larger IDs on the external storage, perhaps referenced by other knt files (New images could override existing files)\^\^CONTINUE?
   sFInf21 = 64397;   // Next ID was changed ok

   sFld01 = 64263;   //  Virtual:
   sFld05 = 64264;   // Problem while saving folder "%s": Note count mismatch (Folder: %d  Internal: %d) The note may not be saved correctly. Continue?
   sFld07 = 64265;   // Node count mismatch.
   sFld09 = 64266;   // Folder contains %d notes, but only %d were saved.
   sFld11 = 64267;   // Failed to open TreePad file
   sFld21 = 64268;   //  New folder.
   sFld22 = 64269;   // Are you sure you want to DELETE FOLDER "%s"?\^This operation cannot be undone.
   sFld24 = 64270;   //  Folder deleted.
   sFld25 = 64271;   //  Folder renamed.
   sFld31 = 64240;   // Virtual node "%s" is currently linked to file "%s". Do you want to link the node to a different file?
   sFld32 = 64241;   // Node "%s" contains text. Do you want to flush this text to a file and make the node virtual?
   sFld33 = 64242;   // This KeyNote file is encrypted, but disk files linked to virtual nodes will NOT be encrypted.\^\^Continue?
   sFld34 = 64243;   // Select file for virtual node
   sFld35 = 64244;   // Only RTF, Text and HTML files can be linked to virtual nodes.
   sFld36 = 64245;   // Cannot link virtual node to a file on removable drive %s:\
   sFld37 = 64246;   // You are creating a virtual node linked to file on removable drive %s\. The file may not be available at a later time. Continue anyway?
   sFld38 = 64247;   // Selected file is already linked to a virtual node\^(Note: You can create a linked node to it)
   sFld39 = 64248;   // Virtual node error:
   sFld40 = 64249;   // OK to reload the node from file %s?
   sFld41 = 64250;   // Unlink virtual node "%s"? The contents of the node will be retained, but the link with the file on disk (%s) will be removed.
   sFld42 = 64251;   // Virtual node %s HAS BEEN modified within KeyNote. If the node is refreshed, the changes will be lost\^
   sFld43 = 64252;   // Virtual node %s has NOT been modified within KeyNote\^
   sFld44 = 64253;   // Error refreshing virtual node:
   sFld45 = 64254;   //  Virtual node refreshed.
   sFld46 = 64255;   //  Error refreshing node
   sFld47 = 64224;   // Selected node "%s" is not a virtual node.
   sFldN01 = 64259;   // <no icon>
   sFldN02 = 64260;   // Rename folder
   sFldN03 = 64261;   // Folder name cannot be blank. Please enter a name.
   sFldN04 = 64262;   // Folder name cannot contain the "%s" character

   sFmg01 = 64402;   // Loading file manager from "
   sFmg02 = 64403;   // Error initializing FileManager:
   sFmg03 = 64404;   // Notes file manager: %d file(s)
   sFmg04 = 64405;   // This file cannot be selected because it does not exist or contains data in a format that %s does not support. Please select another file.
   sFmg05 = 64406;   // FileManager list is empty. This dialog box will now close.
   sFmg06 = 64407;   // never
   sFmg07 = 64408;   // No information is available about this file.
   sFmg08 = 64409;   // This file does not exist or cannot be accessed.
   sFnd01 = 64663;   // Replace this occurrence?
   sFnd02 = 64664;   // Pattern not found: "%s"
   sFnd05 = 64665;   // Search results are not available.
   sFnd06 = 64666;   // Options
   sFnd07 = 64667;   //  Searching - press ESC to abort.
   sFnd08 = 64668;   // An error occurred during search:
   sFnd09 = 64669;   //  Pattern found at pos %d (%d occurrence(s))
   sFnd10 = 64670;   //  Pattern not found.
   sFnd11 = 64671;   //  Replaced %d occurrence(s)
   sFnd12 = 64640;   // Information
   sFnd13 = 64641;   //  matches

   sGlss00 = 64642;   // No item selected.
   sGlss01 = 64643;   // Shortcut term and its expanded definition cannot be blank.
   sGlss02 = 64644;   // Glossary term already exists: "%s" -> "%s". OK to redefine term as "%s"?
   sGlss03 = 64645;   // Error saving Glossary list:
   sGlss04 = 64646;   // Glossary terms: %d
   sGlss05 = 64647;   // Error loading Glossary list:

   sImg01 = 64675;   // Invalid Storage definition:
   sImg02 = 64676;   // Invalid Image definition:
   sImg03 = 64677;   // Invalid Embedded Image:
   sImg04 = 64678;   // Image not found:
   sImg05 = 64680;   // External storage not ready?\^New images will be stored provisionally [only] as Embedded KNT when ext.storage not ready on save
   sImg07 = 64681;   // Folder "%s" is not empty or valid
   sImg08 = 64682;   // A file with that name already exists (%s)
   sImg09 = 64679;   //  | %d instances
   sImg10 = 64683;   // Error %d opening "%s": "%s"
   sImg11 = 64684;   // Folder "%s" does not exist or is empty
   sImg12 = 64685;   // File "%s" does not exist or is not a valid Zip
   sImg13 = 64686;   // All images will be adapted to the storage mode. If selected a new external storage, image files will only be added when you save the KNT file. Current external storage will not be modified.\^You can change the storage again after saving the KNT file.\^\^Continue?
   sImg14 = 64687;   // Current external storage is not available or is invalid\^If you moved the external storage, select "Relocated" and register its new location
   sImg15 = 64656;   // All images have been adapted OK to the storage mode. Changes will be confirmed when KNT file is saved\^(%d different images have been found)
   sImg16 = 64657;   // Exception creating ZIP archive:
   sImg17 = 64658;   // Exception adding file to ZIP archive:
   sImg18 = 64659;   // Exception opening image viewer:
   sImg20 = 64660;   // Exception processing image in RTF:
   sImg21 = 64661;   // Error saving image "%s" (ID:%d) :\^  Content lost\^  Will be removed from Images
   sImg22 = 64662;   // < Unregistered image >
   sImgF01 = 64699;   // Image no available. Change in caption will not saved
   sImgF02 = 64700;   // Save image file as
   sImgF03 = 64701;   // All image files
   sImgF04 = 64702;   // Open image file  (Ctrl -> open file location)
   sImgP01 = 64698;   //  icon %d
   sImgU01 = 64703;   // Error creating RTF for image insertion on editor:
   sImgU02 = 64672;   // Error processing RTF visible image (\pict) :
   sImgU03 = 64673;   // Error processing RTF hidden image (hyperlink) :
   sImgU04 = 64674;   // Error converting image format:

   sINFClipNdNam1 = 64977;   // Default node name
   sINFClipNdNam2 = 64978;   // Use clipboard text
   sINFClipNdNam3 = 64979;   // Use current date and time
   sINFClipPlainTxt1 = 64975;   // Plain (without any formatting)
   sINFClipPlainTxt2 = 64944;   // Only hyperlinks (without other formatting)
   sINFClipPlainTxt3 = 64945;   // Only font style (bold, italic, ...)
   sINFClipPlainTxt4 = 64946;   // Only font (without paragraph formatting)
   sINFCompres1 = 64971;   // None
   sINFCompres2 = 64972;   // Fastest
   sINFCompres3 = 64973;   // Default
   sINFCompres4 = 64974;   // Max
   sINFCtrlUD1 = 64954;   // Moves cursor to prev or next paragraph
   sINFCtrlUD2 = 64955;   // Shift view one line up or down
   sINFCtrlUD3 = 64956;   // Smoothly moves scroll bar vertically
   sINFDefaults1 = 65008;   // New folder
   sINFDefaults2 = 65009;   // New node
   sINFDIR1 = 65032;   // Up
   sINFDIR2 = 65033;   // Down
   sINFDIR3 = 65034;   // Left
   sINFDIR4 = 65035;   // Right
   sINFDrop1 = 64980;   // Open file in KeyNote
   sINFDrop2 = 64981;   // Execute (macro or plugin)
   sINFDrop3 = 64982;   // Merge folders into current file
   sINFDrop4 = 64983;   // Import as a new folder
   sINFDrop5 = 64984;   // Create hyperlink to file
   sINFDrop6 = 64985;   // Import as tree nodes
   sINFDrop7 = 64986;   // Import as virtual tree nodes
   sINFDrop9 = 64987;   // Insert content at caret position
   sINFExpnd1 = 65004;   // Show tree fully collapsed
   sINFExpnd2 = 65005;   // Expand only last active node
   sINFExpnd3 = 65006;   // Expand only top level nodes
   sINFExpnd4 = 65007;   // Restore expanded state of all nodes
   sINFExpnd5 = 64976;   // Show tree fully expanded
   sINFExptFrmt1 = 64994;   // Plain text
   sINFExptFrmt2 = 64995;   // Rich text (RTF)
   sINFExptFrmt3 = 64996;   // KeyNote File (knt)
   sINFFormats1 = 65010;   // Keynote native
   sINFFormats2 = 65011;   // Keynote encrypted
   sINFFormats3 = 65012;   // Keynote compressed
   sINFIconKind1 = 64997;   // None
   sINFIconKind2 = 64998;   // Standard icons
   sINFIconKind3 = 64999;   // Custom icons
   sINFImgExtSt1 = 64952;   // Folder
   sINFImgExtSt2 = 64953;   // ZIP
   sINFImgSM1 = 64947;   // Embedded RTF
   sINFImgSM2 = 64948;   // Embedded KNT
   sINFImgSM3 = 64949;   // External (Folder or Zip)
   sINFImgSM4 = 64950;   // External + Embedded KNT
   sINFImgSM5 = 64951;   // No export images
   sINFImpHTML1 = 64988;   // No conversion (HTML source)
   sINFImpHTML2 = 64989;   // Use Shared HTML Text Converter (html32.cnv + msconv97.dll)
   sINFImpHTML3 = 64990;   // Use MS Word Converter
   sINFImpHTML4 = 64991;   // Use Internet Explorer
   sINFImpHTML5 = 64960;   // Use Microsoft HTML Converter (html.iec)
   sINFLinkType1 = 65000;   // Internet address
   sINFLinkType2 = 65001;   // Email address
   sINFLinkType3 = 65002;   // File or folder
   sINFLinkType4 = 65003;   // KeyNote location
   sINFPOS1 = 65036;   // Top
   sINFPOS2 = 65037;   // Bottom
   sINFPOS3 = 65038;   // Left
   sINFPOS4 = 65039;   // Right
   sINFSrchChk1 = 65019;   // Only non checked nodes
   sINFSrchChk2 = 65020;   // Only checked nodes
   sINFSrchChk3 = 65021;   // All
   sINFSrchMode1 = 65013;   // Exact phrase
   sINFSrchMode2 = 65014;   // All the words
   sINFSrchMode3 = 65015;   // Any of the words
   sINFSrchScope1 = 65016;   // Only node names
   sINFSrchScope2 = 65017;   // Only note contents
   sINFSrchScope3 = 65018;   // All
   sINFStyRg1 = 64205;   // Font
   sINFStyRg2 = 64206;   // Paragraph
   sINFStyRg3 = 64207;   // Font and paragraph
   sINFSymb0 = 64961;   // Euro
   sINFSymb1 = 64962;   // Copyright
   sINFSymb2 = 64963;   // Registered trademark
   sINFSymb3 = 64964;   // Trademark
   sINFSymb4 = 64965;   // Paragraph
   sINFSymb5 = 64966;   // Degree
   sINFSymb6 = 64967;   // Plus/minus
   sINFSymb7 = 64968;   // Dots
   sINFSymb8 = 64969;   // French parenthesis (left)
   sINFSymb9 = 64970;   // French parenthesis (right)
   sINFTreeSel1 = 65022;   // Current node
   sINFTreeSel2 = 65023;   // Current node and subtree
   sINFTreeSel3 = 64992;   // Checked nodes
   sINFTreeSel4 = 64993;   // Full tree
   sINFUrlAct1 = 65025;   // Open
   sINFUrlAct2 = 65026;   // Open in new window
   sINFUrlAct3 = 65027;   // Copy to clipboard
   sINFUrlAct4 = 65028;   // Both (open and copy)
   sINFUrlAct5 = 65029;   // Prompt
   sINFUrlAct6 = 65030;   // Do nothing
   sINFUrlAct7 = 65031;   // Create or Modify

   sLng01 = 64925;   // Internal Language (English) will be established next time you start KeyNote NF
   sLng02 = 64926;   // Language file not found:
   sLng03 = 64927;   // Tip file not found:
   sLng04 = 64896;   // Applying Language file  "

   sLnk01 = 64606;   // Folder ID not found: %d
   sLnk02 = 64607;   // Folder name not found: %s
   sLnk03 = 64576;   // Node ID not found: %d
   sLnk03b = 64577;   // Node GID not found: %d
   sLnk04 = 64578;   // Node name not found: %s
   sLnk05 = 64579;   // Select file to link to
   sLnk06 = 64580;   // Select file to insert
   sLnk07 = 64581;   // The file you selected is not a plain-text or RTF file and cannot be inserted.
   sLnk08 = 64582;   // Cannot insert link to a KeyNote location, because no location has been marked. First, mark a location to which you want to link.
   sLnk09 = 64583;   //  Location inserted
   sLnk10 = 64584;   //  Current location marked
   sLnk11 = 64585;   //  Failed to open location
   sLnk12 = 64586;   // Location does not exist or file cannot be opened: "%s"
   sLnk13 = 64587;   // Invalid location string: %s
   sLnk14 = 64588;   //  Invalid location
   sLnk15 = 64589;   // Error executing hyperlink: %s
   sLnk17 = 64590;   //  URL modified
   sLnk18 = 64591;   //  URL action canceled
   sLnk19 = 64560;   //  URL copied to clipboard
   sLnk20 = 64561;   // Error %d executing hyperlink "%s": "%s"
   sLnk21 = 64562;   //  History error
   sLnk22 = 64563;   //  Cannot navigate to history location
   sLnk23 = 64564;   //  History navigation error
   sLnk24 = 64565;   // Navigate backwards in history
   sLnk25 = 64566;   // Navigate backwards in folder ('local') history
   sLnk26 = 64567;   // Navigate backwards in global history
   sLnk27 = 64568;   // Navigate forward in history
   sLnk28 = 64569;   // Navigate forward in folder ('local') history
   sLnk29 = 64570;   // Navigate forward in global history
   sLnk30 = 64571;   //  (Ctrl+click: only in folder history)
   sLnk31 = 64572;   //  [Mark: %d]
   sLnk32 = 64573;   //    (Undo to remove new hidden markers)
   sLnk33 = 64574;   // Action canceled

   sMac01 = 64817;   // Invalid macro header
   sMac02 = 64818;   // Invalid macro version information
   sMac03 = 64819;   // Error while loading macro "%s": %s\^\^Continue loading macros?
   sMac04 = 64820;   // Unexpected error while loading macro "%s": %s
   sMacC01 = 64824;   // string
   sMacC10 = 64830;   // Inserts a line of text into active note
   sMacC11 = 64831;   // Pauses for a specified period (miliseconds)
   sMacC12 = 64800;   // Rewinds macro to beginning and starts again (macro will run in infinite loop until ESC key pressed)
   sMacC13 = 64801;   // Displays a dialog box with a message text
   sMacC14 = 64802;   // Displays message in status bar
   sMacC15 = 64803;   // Executes the specified plugin
   sMacC16 = 64804;   // Executes the specified macro
   sMacC17 = 64805;   // Displays an OK / CANCEL dialog box and aborts macro if CANCEL is pressed
   sMacC18 = 64806;   // Turns ON specified font style
   sMacC19 = 64807;   // Turns OFF specified font style
   sMacC20 = 64808;   // Toggles specified font style
   sMacC21 = 64809;   // Moves caret left
   sMacC22 = 64810;   // Moves caret right
   sMacC23 = 64811;   // Moves caret down
   sMacC24 = 64812;   // Moves caret up
   sMacC25 = 64813;   // Selects all text in active note
   sMacC26 = 64814;   // Selects new font color
   sMacC27 = 64815;   // Selects new background color
   sMacC28 = 64784;   // Selects new highlight color
   sMacC29 = 64785;   // Creates a new standard RTF note
   sMacC30 = 64786;   // Creates a new tree-type note
   sMacC31 = 64787;   // Applies named style to text
   sMacC32 = 64788;   // Sets a new bookmark
   sMacC33 = 64789;   // Jumps to previously set bookmark
   sMacCcn = 64828;   // color name
   sMacCfn = 64826;   // filename
   sMacCint = 64827;   // integer
   sMacCms = 64825;   // miliseconds
   sMacCstN = 64829;   // style name
   sMacE01 = 64821;   // New macro
   sMacE02 = 64822;   // Macro name cannot be blank.
   sMacE03 = 64823;   // Another macro with this name already exists. Macro names must be unique.
   sMacM01 = 64790;   // Stop recording macro
   sMacM02 = 64791;   // &Stop Recording Macro
   sMacM03 = 64792;   //  Recording macro "%s"
   sMacM04 = 64793;   // Command "%s" cannot be included in macros. This command has been executed but will not be recorded in the macro.
   sMacM05 = 64794;   // You have executed a command which opens a dialog box. KeyNote can remember the values you have just selected and use them when replaying the macro, OR KeyNote can display the dialog box again to let you select values each time the macro is executed. Do you want KeyNote to remember current values?\^\^Click YES to save current values. Click NO to always open the dialog box. Click CANCEL to skip this command and continue recording macro.
   sMacM06 = 64795;   //  Macro recording PAUSED
   sMacM07 = 64796;   //  Macro recording RESUMED
   sMacM08 = 64797;   // Macro "%s" contains no commands and will be discarded.
   sMacM09 = 64798;   // Save new macro "%s" (%d lines)?
   sMacM10 = 64799;   // Error saving macro "%s": %s
   sMacM11 = 64768;   //  Macro recorded and saved.
   sMacM12 = 64769;   //  Macro discarded.
   sMacM13 = 64770;   //  Macro error
   sMacM14 = 64771;   // Error adding new macro "%s": %s
   sMacM15 = 64772;   // Record a new macro
   sMacM16 = 64773;   // &Record Macro...
   sMacM17 = 64774;   // Active folder "%s" is Read-only. Running the macro may cause the folder to be modified. Do you want the macro to run anyway?
   sMacM19 = 64775;   // Running macro "%s"
   sMacM20 = 64776;   // Execute most recent macro "%s"
   sMacM21 = 64777;   // Error loading macro "%s": %s
   sMacM22 = 64778;   // Cannot execute macro "%s": This macro requires a newer version of KeyNote.
   sMacM23 = 64779;   //  Unexpected error while executing macro
   sMacM24 = 64780;   //  Macro was aborted by user
   sMacM25 = 64781;   //  Macro done
   sMacM26 = 64782;   // OK to delete selected macro "%s"?
   sMacM27 = 64783;   // Cannot delete macro file "%s"
   sMacM28 = 64752;   // Error while deleting macro:
   sMacM29 = 64753;   // Macro aborted on line %d: "%s"\^Reason: %s
   sMacM30 = 64754;   // unknown command
   sMacM31 = 64755;   // syntax error
   sMacM32 = 64756;   // unknown user command
   sMacM33 = 64757;   // string argument required
   sMacM34 = 64758;   // integer argument required
   sMacM35 = 64759;   // Cannot run embedded macro "%s". Reason: %s
   sMacM36 = 64760;   // Folder creation failed
   sMacM37 = 64761;   // Invalid font style argument
   sMacM38 = 64762;   // Unexpected error while executing macro: %s\^\^Last macro line was: "%s" (line %d)
   sMacM39 = 64763;   // This command cannot be executed while macro is being recorded or replayed.
   sMacM40 = 64764;   // Macro "%s" not found.
   sMacM41 = 64765;   // No macros available or none selected.
   sMacM42 = 64766;   // Could not access current macro.
   sMacM43 = 64767;   //  This command cannot be repeated
   sMacM44 = 64736;   // This action cannot be performed, because there is no active folder (%d)
   sMacM45 = 64737;   // This folder cannot be set as Read-only, because it is being used for clipboard capture.
   sMacM46 = 64738;   // Failed to assign font attributes.
   sMacM47 = 64739;   // Failed to assign paragraph attributes.
   sMacM48 = 64740;   // Go to line
   sMacM49 = 64741;   // Enter line number or increment (+-):
   sMacM51 = 64742;   // Cannot perform command:
   sMacM52 = 64743;   // No font attributes to paste from: Use "Copy font attributes" first.
   sMacM53 = 64744;   // No paragraph attributes to paste from: Use "Copy paragraph attributes" first.
   sMacM54 = 64745;   // "%s" is not a valid number
   sMacM55 = 64746;   // New background color will be assigned to ALL TREE NODES in folder %s\^Continue?
   sMacM56 = 64747;   // Repeat %s
   sMacM57 = 64748;   // Select macro to execute
   sMacM58 = 64749;   // Failed to copy text formatting

   sMain01 = 64482;   // Unable to assign "%s" as activation hotkey.
   sMain02 = 64483;   // Unexpected error while turning %s Activation hotkey "%s": %s
   sMain03 = 64484;   // &Restore (%s)
   sMain04 = 64485;   // &Restore
   sMain06 = 64486;   // Revert to last saved version of\^%s?
   sMain07 = 64487;   // OK to quit %s?
   sMain08 = 64488;   // Unexpected error:  %s\^\^This message may indicate a bug in KeyNote NF. If the problem persists, please submit a bug reports with the Issue Manager available in KeyNote NF website: %s\^\^You can continue working or terminate KeyNote NF. \^Terminate application?
   sMain09 = 64489;   // KeyNote NF Error
   sMain10 = 64490;   // Cannot perform operation:
   sMain11 = 64491;   //  INS
   sMain12 = 64492;   //  OVR
   sMain13 = 64493;   // KeyNote NF have been configured to allow only one instance at a time\^Closing this instance...
   sMain14 = 64494;   // There was a non-fatal error while loading program configuration: \^%s\^\^Some options may have been reset to factory default values. The application will now continue.
   sMain17 = 64495;   // You seem to have upgraded KeyNote from version %s to %s.\^Files "history.txt" and "%s" contain information about the latest changes and additions.\^\^Do you want to view the file "history.txt" now?
   sMain19 = 64464;   // Custom date formats reloaded (%d)
   sMain20 = 64465;   // Cannot load custom %s formats from %s. Check if the file exists.
   sMain20a = 64466;   // date
   sMain20b = 64467;   // time
   sMain21 = 64468;   // Custom time formats reloaded (%d)
   sMain25 = 64469;   // no file is open
   sMain26 = 64470;   // currently open file has no folders
   sMain27 = 64471;   // Folder is Read-Only
   sMain29 = 64472;   //  Printing folder...
   sMain30 = 64473;   //  Finished printing folder.
   sMain34 = 64474;   // Set alarm... (Ctrl:Add  Shift:->Folder)
   sMain40 = 64475;   // Untitled
   sMain42 = 64476;   //  (no file)
   sMain43 = 64477;   //  Auto
   sMain44 = 64478;   //  MOD
   sMain45 = 64479;   //  Saved
   sMain51 = 64448;   // Search and register note dates: creation and last modified
   sMain52 = 64449;   // Remove date prefixes from node names
   sMain53 = 64450;   //  (Ctrl: Reconsider dates)
   sMain56 = 64451;   // Parser stack overflow
   sMain57 = 64452;   // Bad cell range
   sMain58 = 64453;   // Expected expression
   sMain59 = 64454;   // Expected operator
   sMain60 = 64455;   // Expected opening parenthesis
   sMain61 = 64456;   // Expected operator or closing parenthesis
   sMain62 = 64457;   // Invalid numeric expression
   sMain63 = 64458;   // Cannot evaluate:
   sMain64 = 64459;   // Error at position
   sMain65 = 64460;   // No notes in file
   sMain66 = 64461;   // Find tree node
   sMain67 = 64462;   // Find node containing text:
   sMain68 = 64463;   //  Node not found!
   sMain69 = 64432;   // The Style toolbar must be visible to use this command. Show the Style toolbar now?
   sMain70 = 64433;   // No style available or none selected
   sMain71 = 64434;   // Error: StyleManager does not exist.
   sMain72 = 64435;   // Save tree structure to file
   sMain81 = 64436;   // Could not open KeyNote file "%s"
   sMain82 = 64437;   // This command will start your browser and direct it to KeyNote NF website, where you can download the latest version of the program, read the FAQ, submit bug reports or feature requests with the Issue Manager. \^\^Continue?
   sMain83 = 64438;   // Hide &Resource Panel
   sMain84 = 64439;   // Show &Resource Panel
   sMain85 = 64440;   // Results
   sMain86 = 64441;   // Options
   sMain87 = 64442;   // Cannot hide the last visible tab. At least one tab must remain visible on the resource panel.
   sMain88 = 64443;   // Resource panel position will be updated after KeyNote is restarted.
   sMain89 = 64444;   // External: %s
   sMain90 = 64445;   //  File:
   sMain91 = 64446;   //  Node:
   sMain92 = 64447;   // %s Folder: %s%s
   sMain93 = 64416;   // Double-click to insert selected template
   sMain94 = 64417;   // Toolbar configuration file "%s" not found. Default toolbar configuration file has been created.
   sMain95 = 64418;   // Saved toolbar layout to "%s".
   sMain96 = 64419;   // Starting number for numbered paragraphs:

   sOpt01 = 64881;   //  Custom icons are DISABLED
   sOpt02 = 64882;   // Maximum size for clipboard capture text is not a valid integer value.
   sOpt03 = 64883;   // (invalid date format)
   sOpt04 = 64884;   // (invalid time format)
   sOpt05 = 64885;   // OK to reset tab fonts and colors to default state?
   sOpt06 = 64886;   //  icon %d
   sOpt07 = 64887;   // Icons: %d
   sOpt08 = 64888;   // Failed to get icon from
   sOpt09 = 64889;   // Failed to get bitmap from "%s"
   sOpt10 = 64890;   // Cannot delete this icon: List must contain at least 1 icon.
   sOpt11 = 64891;   // OK to delete the selected icon?
   sOpt13 = 64892;   // OK to restore factory default icons?
   sOpt14 = 64893;   // Divider string can contain the following tokens:\^(CASE SENSITIVE)\^\^%s = current date\^%s = current time\^%s = replaced with one line break\^%s = encloses what to show only if source is included\^%s = source URL (with title)\^%s = source URL (with title, limited waiting time (*))\^%s = source URL (without title)\^%s = source server/domain (e.g.: "[YouTube]")\^%s = delimits divider for 2nd+ (same URL)\^\^Remember: Source tokens will be ignored if source is not shown. Also, with %%| you can vary the effective divider depending on whether source URL is shown or not
   sOpt15 = 64894;   // The Auto-Close function will work ONLY if Auto-Save is turned ON, and if no dialog box is open at the time KeyNote tries to automatically close the file. (Auto-Save is currently DISABLED.)
   sOpt16 = 64895;   // Error in TVChange: PageIndex %d  Node.AbsIdx %d
   sOpt17 = 65087;   // Add text file extension
   sOpt18 = 65088;   // Enter new extension for text files:
   sOpt19 = 65089;   // Extension \"%s\" already listed.
   sOpt20 = 65090;   // Reset default text file extensions?

   sOptS00 = 64897;   // General Settings
   sOptS01 = 64898;   // Rich Text Editor
   sOptS02 = 64899;   // Images
   sOptS03 = 64900;   // Tree Panel
   sOptS04 = 64901;   // KeyNote Files
   sOptS05 = 64902;   // File Options
   sOptS06 = 64903;   // Backup Options
   sOptS07 = 64904;   // Actions
   sOptS08 = 64905;   // Confirmations
   sOptS09 = 64906;   // Chrome
   sOptS10 = 64907;   // Tab Icons
   sOptS11 = 64908;   // Advanced
   sOptS12 = 64909;   // Formats
   sOptS13 = 64910;   // Clipboard
   sOptS14 = 64911;   // File Types
   sOptS15 = 64880;   // Other

   sPass01 = 64873;   // Passphrase cannot be blank.
   sPass02 = 64874;   // File "%s" is encrypted

   sPlg01 = 64875;   // StatusOK
   sPlg02 = 64876;   // Gets data
   sPlg03 = 64877;   // Gets RTF
   sPlg04 = 64878;   // Gets selection
   sPlg05 = 64879;   // Returns data
   sPlg06 = 64848;   // Returns RTF
   sPlg07 = 64849;   // Returns clipboard
   sPlg08 = 64850;   // Needs selection
   sPlg09 = 64851;   // Wants new note
   sPlg10 = 64852;   // Wants dialog box
   sPlg11 = 64853;   // Wants saved file
   sPlg12 = 64854;   // Reload file
   sPlg13 = 64855;   // Stays resident
   sPlg14 = 64856;   // Unexpected error from DLL:
   sPlgM01 = 64857;   // Select plugin to display information
   sPlgM02 = 64858;   // (version %d)
   sPlgM03 = 64859;   // Filename:
   sPlgM04 = 64860;   // No plugins available.
   sPlgM05 = 64861;   // Could not execute plugin
   sPlgM06 = 64862;   // No plugins available or none selected.
   sPlgM07 = 64863;   // Cannot execute plugin - file not found. (%s)
   sPlgM08 = 64832;   // Execute most recent plugin "%s"
   sPlgM09 = 64833;   // Could not obtain plugin information from "%s". Make sure that the file exists.
   sPlgM10 = 64834;   // Plugin "%s" (%s) reports wrong version number %d. A newer version of KeyNote is required to run this plugin.
   sPlgM11 = 64835;   // Plugin "%s" (%s) refuses to execute.
   sPlgM12 = 64836;   // Could not load plugin "%s" (%s).
   sPlgM13 = 64837;   // Could not execute plugin "%s" (%s).
   sPlgM14 = 64838;   // Resident plugin "%s" is already running. Shut down the running plugin first.
   sPlgM15 = 64839;   // Plugin "%s" tried to go resident, but attempt to set resident plugin ID failed. Plugin will be shut down.
   sPlgM16 = 64840;   // Plugin "%s" requires that text is selected in active note. Select some text and try again.
   sPlgM17 = 64841;   // Plugin returned error code "%d"
   sPlgM18 = 64842;   // Plugin "%s" is now running
   sPlgM19 = 64843;   // Plugin copied data to clipboard. Ok to paste into active note?
   sPlgM20 = 64844;   // Active note "%s" is Read-only. Inserting plugin output will modify the note. Insert anyway?
   sPlgM21 = 64845;   // Unexpected error during plugin cleanup:
   sPlgM22 = 64846;   // Unexpected plugin error:
   sPlgM23 = 64847;   // Resident plugin "%s" unloaded
   sPlgM24 = 64816;   // Unexpected error while shutting down resident plugin "%s": %s

   sSty01 = 64750;   // Face: %s\^Size: %s\^Style: %s\^Color: %s\^Other: %s
   sSty02 = 64751;   //  %s, %s space, %s, L:%d F:%d R:%d, Bef:%d Aft:%d
   sSty03 = 64720;   // Alignment: %s\^Line spacing: %s\^Numbering: %s\^Left indent: %s\^First indent: %s\^Right indent: %s\^Space before: %s\^Space after: %s
   sSty04 = 64721;   // Single
   sSty05 = 64722;   // Double
   sSty06 = 64723;   // other
   sSty07 = 64724;   // Bullets
   sSty08 = 64725;   // Left
   sSty09 = 64726;   // Right
   sSty10 = 64727;   // Center
   sSty11 = 64728;   // Justify
   sSty12 = 64729;   // superscript
   sSty13 = 64730;   // subscript
   sSty14 = 64731;   // Subscript
   sSty15 = 64732;   // Supercript
   sSty16 = 64733;   // Disabled
   sSty17 = 64734;   // Highlighted
   sStyM01 = 64735;   // Style in active note:
   sStyM02 = 64704;   // Font:
   sStyM03 = 64705;   // Paragraph:
   sStyM04 = 64706;   // Named style:
   sStyM05 = 64707;   // Range:
   sStyM06 = 64708;   // Error: StyleManager does not exist.
   sStyM07 = 64709;   // Create "%s" style
   sStyM08 = 64710;   // Enter name for new style:
   sStyM09 = 64711;   // %s style "%s" already exists. \^Redefine the existing style with new properties?
   sStyM10 = 64712;   //  Style "%s" created (%s)
   sStyM11 = 64713;   // Error creating style:
   sStyM12 = 64714;   // Error: Cannot access style information for "%s"
   sStyM13 = 64715;   // Rename style
   sStyM14 = 64716;   // Enter new name for style:
   sStyM15 = 64717;   // Cannot rename: a style by that name already exists.
   sStyM16 = 64718;   // Error renaming style
   sStyM17 = 64719;   // OK to delete %s style "%s"?
   sStyM18 = 64688;   // Error deleting style

   sTip01 = 64864;   // Tip of the day
   sTip02 = 64865;   // Did you know...
   sTip03 = 64866;   // (Tips not found.)

   sTpl01 = 64689;   // Template "%s" already exists. Overwrite existing template?
   sTpl02 = 64690;   // Template "%s" created.
   sTpl03 = 64691;   // Select template to insert
   sTpl04 = 64692;   // OK to delete selected template "%s"?

   sTree01 = 64225;   // Error creating node:
   sTree04 = 64226;   // Initial node not assigned - select a node and retry.
   sTree05 = 64227;   // cannot be
   sTree06 = 64228;   // Error moving node:
   sTree07 = 64229;   // Node "%s" %smoved %s
   sTree08 = 64230;   // \+This operation cannot be undone.
   sTree09 = 64231;   // Node "%s" has %d child nodes. Delete these child nodes too?
   sTree10 = 64232;   // OK to delete node "%s"?
   sTree11 = 64233;   // OK to delete %sALL SELECTED nodes?\^ (Confirmation will be requested for each node with children)\^
   sTree12 = 64234;   // OK to delete %d CHILD NODES of node "%s"?
   sTree13 = 64235;   // Selected node has no children.
   sTree14 = 64236;   // Error deleting node:
   sTree15 = 64237;   // No nodes available for copying or pasting data.
   sTree16 = 64238;   // OK to MOVE %d nodes/subtrees to current node "%s"?
   sTree17 = 64239;   //  No node is selected
   sTree18 = 64208;   // OK to forget %s?
   sTree19 = 64209;   // Target node is included in one of the subtrees to move
   sTree20 = 64210;   //  nodes/subtrees registered for transfer
   sTree21 = 64211;   // No data to paste. Select "Transfer|Copy/Cut Subtree" first.
   sTree22 = 64212;   // \^\^* One or more nodes being copied is a Virtual Node. They will be pasted as linked nodes\^\^Continue?
   sTree23 = 64213;   // OK to PASTE %d nodes/subtrees%s below current node "%s"?\+(Hidden nodes will ALSO be pasted)
   sTree24 = 64215;   //  Pasted %d nodes/subtrees
   sTree25 = 64216;   // %d virtual nodes have been copied as linked nodes
   sTree26 = 64214;   //  as LINKED nodes
   sTree49 = 64217;   // OK to sort the entire tree?
   sTree50 = 64218;   //  Node name cannot be blank!
   sTree51 = 64219;   //  Node renamed.
   sTree52 = 64220;   //  Cannot perform operation: '%s' folder is read-only
   sTree53 = 64221;   // Edit node name
   sTree54 = 64222;   // Enter new name:
   sTree55 = 64223;   // Disable Filter on tree
   sTree56 = 64192;   // Apply Filter on tree
   sTree57 = 64193;   //  (Ctrl: Clear Find filter)
   sTree59 = 64194;   // OK to remove all Flags in folder?
   sTree60 = 64195;   // Last modified >= "%s"
   sTree62 = 64196;   // Name
   sTree63 = 64197;   // Note name
   sTree64 = 64198;   // Date
   sTree65 = 64199;   // Note creation date
   sTree66 = 64200;   // Flagged

   sUInote01 = 64201;   // Created: %s  ==  Last modified: %s

   sUpd01 = 65052;   // You already have the latest version installed
   sUpd02 = 65053;   // There is a new version !
   sUpd03 = 65054;   // No Internet access

   sURL01 = 64869;   // OK
   sURL02 = 64870;   // Create Hyperlink
   sURL03 = 64871;   // Modify
   sURL04 = 64872;   // Choose Action for Hyperlink

   sVCL00 = 64420;   // Click and drag to resize panels (Ctrl: tree max width / Alt: Toggle fixed)
   sVCL01 = 64421;   // Error destroying tabsheet
   sVCL02 = 64422;   // Select text color
   sVCL03 = 64423;   // Select &Highlight...
   sVCL04 = 64424;   // Select highlight color
   sVCL05 = 64425;   // Apply current font color to text
   sVCL06 = 64426;   // Apply &Highlight
   sVCL07 = 64427;   // Apply current highlight color to text
   sVCL12 = 64428;   // Hide &Resource Panel
   sVCL13 = 64429;   // Show &Resource Panel
   sVCL14 = 64430;   // The Resource panel must be visible to use this command. Show the Resource panel now?
   sVCL15 = 64431;   // Use the mouse to apply the %s to another text or press Esc to cancel
   sVCL16 = 64400;   // paragraph formatting
   sVCL17 = 64401;   // font formatting


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
