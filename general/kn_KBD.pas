unit kn_kbd;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 
 Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
 in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 


interface
uses
   System.Classes,
   System.IniFiles,
   Vcl.Menus;

{
  keys that are used internally (in OnKeyDown events
  and should not be reassigned:
  Ctrl+Tab (switch tabs)
  Ctrl+Shift+Tab (back-switch tabs)
  Ctrl+PageUp (switch tabs)
  Ctrl+PageDown (back-switch tabs)
  Alt+PageUp (switch resource panel tabs)
  Alt+PageDown (back-switch resource panel tabs)
  ESC (defined in options)
  ACTIVATION_HOTKEY (whatever it was set to)

  all single keys in editor (some are OK in tree)

}

(*
const
  // key shortcuts that cannot be assigned
  _InvalidKeys = [
    33, 34, // page up, page down
    37, 38, 29, 40, // arrow keys
    112,   // F1
    32883, // Alt+F4
    // 8829, 8830, 8831, 8832, // shift+arrow keys
    // anything with TAB in it!
    32801, 32802, // alt+pageup, alt+pagedown
    32805, 32806, 32807, 32808, // alt+arrow keys
    16421, 16422, 16423, 16424 // ctrl+arrow keys
  ];

*)

type
  TMainMenuCmd = (
    mmcEdit_CompressBlanks,
    mmcEdit_Copy,
    mmcEdit_CopyAllText,
    mmcEdit_Cut,
    mmcEdit_Delete,
    mmcEdit_Deleteline,
    mmcEdit_Eval,
    mmcEdit_EvalPaste,
    mmcEdit_InvertCase,
    mmcEdit_Joinlines,
    mmcEdit_Paste,
    mmcEdit_Pasteastext,
    mmcEdit_Pasteintonew,
    mmcEdit_PasteSpecial,
    mmcEdit_Redo,
    mmcEdit_RepeatCmd,
    mmcEdit_ReverseText,
    mmcEdit_Rot13,
    mmcEdit_Selectall,
    mmcEdit_Selectword,
    mmcEdit_Sortlines,
    mmcEdit_Tolowercase,
    mmcEdit_ToMixedCase,
    mmcEdit_TrimBoth,
    mmcEdit_TrimLeft,
    mmcEdit_TrimRight,
    mmcEdit_Undo,
    mmcEdit_UPPERCASE,
    mmcFile_Autosave,
    mmcFile_Close,
    mmcFile_Copyto,
    mmcFile_Exit,
    mmcFile_FileManager,
    mmcFile_Fileproperties,
    mmcFile_New,
    mmcFile_Open,
    mmcFile_Pagesetup,
    mmcFile_Save,
    mmcFile_Saveas,
    mmcFormat_ALCenter,
    mmcFormat_AlLeft,
    mmcFormat_AlRight,
    mmcFormat_ApplyStyle,
    mmcFormat_Backgroundcolor,
    mmcFormat_Bold,
    mmcFormat_Bullets,
    mmcFormat_ClearFontAttr,
    mmcFormat_ClearParaAttr,
    mmcFormat_CopyFormatFont,
    mmcFormat_CopyFormatPara,
    mmcFormat_FIndent,
    mmcFormat_FontDisabled,
    mmcFormat_FontSizeDecrease,
    mmcFormat_FontSizeIncrease,
    mmcFormat_FOutdent,
    mmcFormat_HighlightNone,
    mmcFormat_HighlightSel,
    mmcFormat_Indent,
    mmcFormat_Italics,
    mmcFormat_Language,
    mmcFormat_LS1,
    mmcFormat_LS15,
    mmcFormat_LS2,
    mmcFormat_Outdent,
    mmcFormat_Paragraph,
    mmcFormat_PasteFormatFont,
    mmcFormat_PasteFormatPara,
    mmcFormat_RIndent,
    mmcFormat_ROutDent,
    mmcFormat_Selectfont,
    mmcFormat_SpADecrease,
    mmcFormat_SpAIncrease,
    mmcFormat_SpBDecrease,
    mmcFormat_SpBIncrease,
    mmcFormat_Strikeout,
    mmcFormat_Subscript,
    mmcFormat_SuperScript,
    mmcFormat_Textcolor,
    mmcFormat_Underline,
    mmcFormat_WordWrap,
    mmcHelp_About,
    mmcHelp_EmailAuthor,
    mmcHelp_GeneralHelp,
    mmcHelp_HelpContents,
    mmcHelp_KeyboardReference,
    mmcHelp_Tipoftheday,
    mmcHelp_VisitWebsite,
    mmcInsert_ExpTerm,
    mmcInsert_InsChar,
    mmcInsert_Insertcurrentdate,
    mmcInsert_Insertcurrenttime,
    mmcInsert_InsertFileContents,
    mmcInsert_InsertKNTLink,
    mmcInsert_InsertURL,
    mmcInsert_InsHyperlink,
    mmcInsert_InsObject,
    mmcInsert_InsPicture,
    mmcInsert_LinkFile,
    mmcInsert_MarkLocation,
    mmcInsert_WordWeb,
    mmcNote_CheckSpelling,
    mmcNote_ClipCapEnable,
    mmcNote_Emailnote,
    mmcNote_PrintNote,
    mmcNote_Printpreview,
    mmcNote_Readonly,
    mmcNote_TabNew,
    mmcNote_TabProperties,
    mmcNote_TabRemove,
    mmcNote_TabRename,
    mmcSearch_BkmJ0,
    mmcSearch_BkmJ1,
    mmcSearch_BkmJ2,
    mmcSearch_BkmJ3,
    mmcSearch_BkmJ4,
    mmcSearch_BkmJ5,
    mmcSearch_BkmJ6,
    mmcSearch_BkmJ7,
    mmcSearch_BkmJ8,
    mmcSearch_BkmJ9,
    mmcSearch_BkmSet0,
    mmcSearch_BkmSet1,
    mmcSearch_BkmSet2,
    mmcSearch_BkmSet3,
    mmcSearch_BkmSet4,
    mmcSearch_BkmSet5,
    mmcSearch_BkmSet6,
    mmcSearch_BkmSet7,
    mmcSearch_BkmSet8,
    mmcSearch_BkmSet9,
    mmcSearch_Find,
    mmcSearch_Findagain,
    mmcSearch_FindNextNode,
    mmcSearch_FindNode,
    mmcSearch_Goto,
    mmcSearch_MatchBracket,
    mmcSearch_Replace,
    mmcSearch_ReplaceNext,
    mmcTools_AddTerm,
    mmcTools_Calculator,
    mmcTools_Defaults,
    mmcTools_EditGlossary,
    mmcTools_Export,
    mmcTools_Import,
    mmcTools_Mergewithfile,
    mmcTools_Options,
    mmcTools_Plugins,
    mmcTools_RunLastMacro,
    mmcTools_RunLastPlugin,
    mmcTools_RunMacro,
    mmcTools_SelectMacro,
    mmcTools_Stats,
    mmcTools_TplCreate,
    mmcTools_TplInsert,
    mmcTree_AsDate,
    mmcTree_AsDateandTime,
    mmcTree_AsTime,
    mmcTree_DeleteChildnodes,
    mmcTree_FromClipboard,
    mmcTree_MoveNodeDown,
    mmcTree_MoveNodeLeft,
    mmcTree_MoveNodeRight,
    mmcTree_MoveNodeUp,
    mmcTree_NodeAdd,
    mmcTree_NodeAddChild,
    mmcTree_NodeCollapseall,
    mmcTree_NodeDelete,
    mmcTree_NodeExpandall,
    mmcTree_NodeInsert,
    mmcTree_NodeRename,
    mmcTree_SaveTreetoFile,
    mmcTree_SortChildNodes,
    mmcTree_SortFullTree,
    mmcView_Alphabetizetabs,
    mmcView_Alwaysontop,
    mmcView_Checkboxes,
    mmcView_FormatToolbar,
    mmcView_FormatViewBoth,
    mmcView_FormatViewFont,
    mmcView_FormatViewNone,
    mmcView_FormatViewPara,
    mmcView_HideAllToolbars,
    mmcView_MacroToolbar,
    mmcView_Maintoolbar,
    mmcView_NodeIcons,
    mmcView_ShiftLeft,
    mmcView_ShiftRight,
    mmcView_ShowAllToolbars,
    mmcView_ShowIconsontabs,
    mmcView_ShowTree,
    mmcView_StyleToolbar,
    mmcView_TreeToolbar,
    mmcView_ViewResPanel
  );

  TTreeMenuCmd = (
    tvcAddChildNode,
    tvcAddNode,
    tvcAqua,
    tvcAsDate,
    tvcAsDateandTime,
    tvcAsTime,
    tvcBlack,
    tvcBlue,
    tvcBold,
    tvcChecked,
    tvcCopynodename,
    tvcCopySubtree,
    tvcDefault,
    tvcDeleteChildren,
    tvcDeletenode,
    tvcDown,
    tvcEraseMem,
    tvcExport,
    tvcFromClipboard,
    tvcFuchsia,
    tvcGray,
    tvcGreen,
    tvcInsertNode,
    tvcLeft,
    tvcLime,
    tvcMaroon,
    tvcNavy,
    tvcOlive,
    tvcPasteSubtree,
    tvcPurple,
    tvcRed,
    tvcRefresh,
    tvcRenamenode,
    tvcRight,
    tvcSilver,
    tvcSortchildnodes,
    tvcSortfullTree,
    tvcTeal,
    tvcUp,
    tvcVirtualNode,
    tvcWhite,
    tvcYellow
  );


type
  TKNTMenuCmd = record
    Name : string;
    ShortCut : TShortCut;
    Hint : string;
  end;

type
  TKNTMainMenuCmds = array[TMainMenuCmd] of TKNTMenuCmd;
  TKNTTreeMenuCmds = array[TTreeMenuCmd] of TKNTMenuCmd;


const
  KNTMainMenuCmds : TKNTMainMenuCmds = (
    ( Name : 'Edit_CompressBlanks'; Shortcut : 0; Hint : 'Compress white space' ), {  }
    ( Name : 'Edit_Copy'; Shortcut : 16451; Hint : 'Copy text to clipboard' ), { Ctrl+C }
    ( Name : 'Edit_CopyAllText'; Shortcut : 0; Hint : 'Select and copy all text in active note' ), {  }
    ( Name : 'Edit_Cut'; Shortcut : 16472; Hint : 'Cut text to clipboard' ), { Ctrl+X }
    ( Name : 'Edit_Delete'; Shortcut : 0; Hint : 'Remove selected text' ), {  }
    ( Name : 'Edit_Deleteline'; Shortcut : 16473; Hint : 'Delete line at cursor' ), { Ctrl+Y }
    ( Name : 'Edit_Eval'; Shortcut : 16571; Hint : 'Evaluate selected text as mathematical expression' ), { Ctrl+= }
    ( Name : 'Edit_EvalPaste'; Shortcut : 32813; Hint : 'Paste result of last expression evaluated' ), { Alt+Ins }
    ( Name : 'Edit_InvertCase'; Shortcut : 24649; Hint : 'Invert case of selected text' ), { Shift+Ctrl+I }
    ( Name : 'Edit_Joinlines'; Shortcut : 16458; Hint : 'Join selected lines (reformat pagagraph)' ), { Ctrl+J }
    ( Name : 'Edit_Paste'; Shortcut : 16470; Hint : 'Paste text from clipboard' ), { Ctrl+V }
    ( Name : 'Edit_Pasteastext'; Shortcut : 24621; Hint : 'Paste text from clipboard without formatting' ), { Shift+Ctrl+Ins }
    ( Name : 'Edit_Pasteintonew'; Shortcut : 0; Hint : 'Create a new note and paste text from clipboard' ), {  }
    ( Name : 'Edit_PasteSpecial'; Shortcut : 0; Hint : 'Special paste dialog box' ), {  }
    ( Name : 'Edit_Redo'; Shortcut : 16397; Hint : 'Redo the last Undone command' ), { Ctrl+Enter }
    ( Name : 'Edit_RepeatCmd'; Shortcut : 16575; Hint : 'Repeat last editing command' ), { Ctrl+/ }
    ( Name : 'Edit_ReverseText'; Shortcut : 24628; Hint : 'Order selected text backwards' ), { Shift+Ctrl+4 }
    ( Name : 'Edit_Rot13'; Shortcut : 24627; Hint : 'Apply ROT-13 encoding to selected text' ), { Shift+Ctrl+3 }
    ( Name : 'Edit_Selectall'; Shortcut : 16449; Hint : 'Select all text in note' ), { Ctrl+A }
    ( Name : 'Edit_Selectword'; Shortcut : 32855; Hint : 'Select current word' ), { Alt+W }
    ( Name : 'Edit_Sortlines'; Shortcut : 24659; Hint : 'Sort selected lines in active note' ), { Shift+Ctrl+S }
    ( Name : 'Edit_Tolowercase'; Shortcut : 24652; Hint : 'Sets selected text to lowercase' ), { Shift+Ctrl+L }
    ( Name : 'Edit_ToMixedCase'; Shortcut : 24653; Hint : 'Sets selected text to mixed (Title) case' ), { Shift+Ctrl+M }
    ( Name : 'Edit_TrimBoth'; Shortcut : 0; Hint : 'Left- and right-trim white space' ), {  }
    ( Name : 'Edit_TrimLeft'; Shortcut : 0; Hint : 'Left-trim white space' ), {  }
    ( Name : 'Edit_TrimRight'; Shortcut : 0; Hint : 'Right-trim white space' ), {  }
    ( Name : 'Edit_Undo'; Shortcut : 32776; Hint : 'Undo last change' ), { Alt+BkSp }
    ( Name : 'Edit_UPPERCASE'; Shortcut : 24661; Hint : 'Sets selected text to uppercase' ), { Shift+Ctrl+U }
    ( Name : 'File_Autosave'; Shortcut : 0; Hint : 'Toggle automatic saving' ), {  }
    ( Name : 'File_Close'; Shortcut : 16465; Hint : 'Close currently open file' ), { Ctrl+Q }
    ( Name : 'File_Copyto'; Shortcut : 0; Hint : 'Copy current file to another directory' ), {  }
    ( Name : 'File_Exit'; Shortcut : 0; Hint : 'Close KeyNote' ), {  }
    ( Name : 'File_FileManager'; Shortcut : 123; Hint : 'Open file manager' ), { F12 }
    ( Name : 'File_Fileproperties'; Shortcut : 32781; Hint : 'Edit Keynote file properties' ), { Alt+Enter }
    ( Name : 'File_New'; Shortcut : 24654; Hint : 'Create a new Keynote file' ), { Shift+Ctrl+N }
    ( Name : 'File_Open'; Shortcut : 16463; Hint : 'Open a Keynote file' ), { Ctrl+O }
    ( Name : 'File_Pagesetup'; Shortcut : 0; Hint : 'View or change page setup' ), {  }
    ( Name : 'File_Save'; Shortcut : 16467; Hint : 'Save Keynote file' ), { Ctrl+S }
    ( Name : 'File_Saveas'; Shortcut : 0; Hint : 'Save Keynote file with a new name' ), {  }
    ( Name : 'Format_ALCenter'; Shortcut : 49190; Hint : 'Center lines' ), { Ctrl+Alt+Up }
    ( Name : 'Format_AlLeft'; Shortcut : 49189; Hint : 'Align lines to the left' ), { Ctrl+Alt+Left }
    ( Name : 'Format_AlRight'; Shortcut : 49191; Hint : 'Align lines to the right' ), { Ctrl+Alt+Right }
    ( Name : 'Format_ApplyStyle'; Shortcut : 8307; Hint : 'Apply current style to selection' ), { Shift+F4 }
    ( Name : 'Format_Backgroundcolor'; Shortcut : 16452; Hint : 'Change background color' ), { Ctrl+D }
    ( Name : 'Format_Bold'; Shortcut : 16450; Hint : 'Apply bold attribute' ), { Ctrl+B }
    ( Name : 'Format_Bullets'; Shortcut : 16453; Hint : 'Apply bullet style to lines' ), { Ctrl+E }
    ( Name : 'Format_ClearFontAttr'; Shortcut : 16432; Hint : 'Restore default font style attributes' ), { Ctrl+0 }
    ( Name : 'Format_ClearParaAttr'; Shortcut : 24624; Hint : 'Restore default paragraph style attributes' ), { Shift+Ctrl+0 }
    ( Name : 'Format_CopyFormatFont'; Shortcut : 16503; Hint : 'Lift font attributes' ), { Ctrl+F8 }
    ( Name : 'Format_CopyFormatPara'; Shortcut : 16504; Hint : 'Lift paragraph attributes' ), { Ctrl+F9 }
    ( Name : 'Format_FIndent'; Shortcut : 16570; Hint : 'Increase first indent' ), { Ctrl+; }
    ( Name : 'Format_FontDisabled'; Shortcut : 16439; Hint : 'Apply disabled attribute' ), { Ctrl+7 }
    ( Name : 'Format_FontSizeDecrease'; Shortcut : 16603; Hint : 'Grow font' ), { Ctrl+[ }
    ( Name : 'Format_FontSizeIncrease'; Shortcut : 16605; Hint : 'Shrink font' ), { Ctrl+] }
    ( Name : 'Format_FOutdent'; Shortcut : 24762; Hint : 'Decrease first indent' ), { Shift+Ctrl+; }
    ( Name : 'Format_HighlightNone'; Shortcut : 24648; Hint : 'Remove text highlighting' ), { Shift+Ctrl+H }
    ( Name : 'Format_HighlightSel'; Shortcut : 16456; Hint : 'Apply current highlight color to text' ), { Ctrl+H }
    ( Name : 'Format_Indent'; Shortcut : 32954; Hint : 'Increase left indent' ), { Alt+; }
    ( Name : 'Format_Italics'; Shortcut : 16457; Hint : 'Apply italic attribute' ), { Ctrl+I }
    ( Name : 'Format_Language'; Shortcut : 0; Hint : 'Specify language for selected text' ), {  }
    ( Name : 'Format_LS1'; Shortcut : 16433; Hint : 'Select single line spacing' ), { Ctrl+1 }
    ( Name : 'Format_LS15'; Shortcut : 16437; Hint : 'Select one and a half line spacing' ), { Ctrl+5 }
    ( Name : 'Format_LS2'; Shortcut : 16434; Hint : 'Select double line spacing' ), { Ctrl+2 }
    ( Name : 'Format_Outdent'; Shortcut : 41146; Hint : 'Decrease left indent' ), { Shift+Alt+; }
    ( Name : 'Format_Paragraph'; Shortcut : 16464; Hint : 'View or change paragraph settings' ), { Ctrl+P }
    ( Name : 'Format_PasteFormatFont'; Shortcut : 8311; Hint : 'Apply copied font attributes' ), { Shift+F8 }
    ( Name : 'Format_PasteFormatPara'; Shortcut : 8312; Hint : 'Apply copied paragraph attributes' ), { Shift+F9 }
    ( Name : 'Format_RIndent'; Shortcut : 16606; Hint : 'Increase right indent' ), { Ctrl+' }
    ( Name : 'Format_ROutDent'; Shortcut : 24798; Hint : 'Decrease right indent' ), { Shift+Ctrl+' }
    ( Name : 'Format_Selectfont'; Shortcut : 16468; Hint : 'View or change font settings' ), { Ctrl+T }
    ( Name : 'Format_SpADecrease'; Shortcut : 24766; Hint : 'Decrease space after paragraph' ), { Shift+Ctrl+. }
    ( Name : 'Format_SpAIncrease'; Shortcut : 16574; Hint : 'Increase space after paragraph' ), { Ctrl+. }
    ( Name : 'Format_SpBDecrease'; Shortcut : 24764; Hint : 'Decrease space before paragraph' ), { Shift+Ctrl+, }
    ( Name : 'Format_SpBIncrease'; Shortcut : 16572; Hint : 'Increase space before paragraph' ), { Ctrl+, }
    ( Name : 'Format_Strikeout'; Shortcut : 16459; Hint : 'Apply strikeout attribute' ), { Ctrl+K }
    ( Name : 'Format_Subscript'; Shortcut : 24765; Hint : 'Apply subscript attribute' ), { Shift+Ctrl+- }
    ( Name : 'Format_SuperScript'; Shortcut : 16573; Hint : 'Apply superscript attribute' ), { Ctrl+- }
    ( Name : 'Format_Textcolor'; Shortcut : 16466; Hint : 'Apply current font color to text' ), { Ctrl+R }
    ( Name : 'Format_Underline'; Shortcut : 16469; Hint : 'Apply underline attribute' ), { Ctrl+U }
    ( Name : 'Format_WordWrap'; Shortcut : 16471; Hint : 'Toggle wrapping at end of lines' ), { Ctrl+W }
    ( Name : 'Help_About'; Shortcut : 0; Hint : 'Information about the program' ), {  }
    ( Name : 'Help_EmailAuthor'; Shortcut : 0; Hint : 'Start mail client and send email to keyNote author' ), {  }
    ( Name : 'Help_GeneralHelp'; Shortcut : 0; Hint : 'Display general help topics' ), {  }
    ( Name : 'Help_HelpContents'; Shortcut : 0; Hint : 'Display contents of online help' ), {  }
    ( Name : 'Help_KeyboardReference'; Shortcut : 0; Hint : 'Display keyboard reference' ), {  }
    ( Name : 'Help_Tipoftheday'; Shortcut : 0; Hint : 'Display the Tip of the Day dialog' ), {  }
    ( Name : 'Help_VisitWebsite'; Shortcut : 0; Hint : 'Start web browser and go to KeyNote website' ), {  }
    ( Name : 'Insert_ExpTerm'; Shortcut : 118; Hint : 'Replace current term with its Glossary definition' ), { F7 }
    ( Name : 'Insert_InsChar'; Shortcut : 24643; Hint : 'Insert special characters' ), { Shift+Ctrl+C }
    ( Name : 'Insert_Insertcurrentdate'; Shortcut : 24644; Hint : 'Insert date into the note' ), { Shift+Ctrl+D }
    ( Name : 'Insert_Insertcurrenttime'; Shortcut : 24660; Hint : 'Insert time into the note' ), { Shift+Ctrl+T }
    ( Name : 'Insert_InsertFileContents'; Shortcut : 0; Hint : 'Insert contents of a text file' ), {  }
    ( Name : 'Insert_InsertKNTLink'; Shortcut : 8309; Hint : 'Insert link to the previously marked place' ), { Shift+F6 }
    ( Name : 'Insert_InsertURL'; Shortcut : 0; Hint : 'Insert an Internet URL' ), {  }
    ( Name : 'Insert_InsHyperlink'; Shortcut : 24665; Hint : 'Insert hyperlink to URL, file or note' ), { Shift+Ctrl+Y }
    ( Name : 'Insert_InsObject'; Shortcut : 0; Hint : 'Inserts an OLE object' ), {  }
    ( Name : 'Insert_InsPicture'; Shortcut : 0; Hint : 'Insert an image' ), {  }
    ( Name : 'Insert_LinkFile'; Shortcut : 0; Hint : 'Insert a hyperlink to a local file' ), {  }
    ( Name : 'Insert_MarkLocation'; Shortcut : 16501; Hint : 'Mark place to which the link will jump' ), { Ctrl+F6 }
    ( Name : 'Insert_WordWeb'; Shortcut : 16506; Hint : 'Look up word in WordWeb thesaurus' ), { Ctrl+F11 }
    ( Name : 'Note_CheckSpelling'; Shortcut : 0; Hint : 'Check spelling in current note' ), {  }
    ( Name : 'Note_ClipCapEnable'; Shortcut : 122; Hint : 'Toggle clipboard capture for active note' ), { F11 }
    ( Name : 'Note_Emailnote'; Shortcut : 24645; Hint : 'Send current note via e-mail' ), { Shift+Ctrl+E }
    ( Name : 'Note_PrintNote'; Shortcut : 24656; Hint : 'Print text of current note' ), { Shift+Ctrl+P }
    ( Name : 'Note_Printpreview'; Shortcut : 0; Hint : 'View how the note will be printed' ), {  }
    ( Name : 'Note_Readonly'; Shortcut : 24658; Hint : 'Make current note read-only' ), { Shift+Ctrl+R }
    ( Name : 'Note_TabNew'; Shortcut : 16462; Hint : 'Add a new note' ), { Ctrl+N }
    ( Name : 'Note_TabProperties'; Shortcut : 115; Hint : 'Edit current note properties' ), { F4 }
    ( Name : 'Note_TabRemove'; Shortcut : 0; Hint : 'Delete current note' ), {  }
    ( Name : 'Note_TabRename'; Shortcut : 113; Hint : 'Rename current note' ), { F2 }
    ( Name : 'Search_BkmJ0'; Shortcut : 32816; Hint : 'Jump to bookmark' ), { Alt+0 }
    ( Name : 'Search_BkmJ1'; Shortcut : 32817; Hint : 'Jump to bookmark' ), { Alt+1 }
    ( Name : 'Search_BkmJ2'; Shortcut : 32818; Hint : 'Jump to bookmark' ), { Alt+2 }
    ( Name : 'Search_BkmJ3'; Shortcut : 32819; Hint : 'Jump to bookmark' ), { Alt+3 }
    ( Name : 'Search_BkmJ4'; Shortcut : 32820; Hint : 'Jump to bookmark' ), { Alt+4 }
    ( Name : 'Search_BkmJ5'; Shortcut : 32821; Hint : 'Jump to bookmark' ), { Alt+5 }
    ( Name : 'Search_BkmJ6'; Shortcut : 32822; Hint : 'Jump to bookmark' ), { Alt+6 }
    ( Name : 'Search_BkmJ7'; Shortcut : 32823; Hint : 'Jump to bookmark' ), { Alt+7 }
    ( Name : 'Search_BkmJ8'; Shortcut : 32824; Hint : 'Jump to bookmark' ), { Alt+8 }
    ( Name : 'Search_BkmJ9'; Shortcut : 32825; Hint : 'Jump to bookmark' ), { Alt+9 }
    ( Name : 'Search_BkmSet0'; Shortcut : 41008; Hint : 'Set bookmark ' ), { Shift+Alt+0 }
    ( Name : 'Search_BkmSet1'; Shortcut : 41009; Hint : 'Set bookmark ' ), { Shift+Alt+1 }
    ( Name : 'Search_BkmSet2'; Shortcut : 41010; Hint : 'Set bookmark ' ), { Shift+Alt+2 }
    ( Name : 'Search_BkmSet3'; Shortcut : 41011; Hint : 'Set bookmark ' ), { Shift+Alt+3 }
    ( Name : 'Search_BkmSet4'; Shortcut : 41012; Hint : 'Set bookmark ' ), { Shift+Alt+4 }
    ( Name : 'Search_BkmSet5'; Shortcut : 41013; Hint : 'Set bookmark ' ), { Shift+Alt+5 }
    ( Name : 'Search_BkmSet6'; Shortcut : 41014; Hint : 'Set bookmark ' ), { Shift+Alt+6 }
    ( Name : 'Search_BkmSet7'; Shortcut : 41015; Hint : 'Set bookmark ' ), { Shift+Alt+7 }
    ( Name : 'Search_BkmSet8'; Shortcut : 41016; Hint : 'Set bookmark ' ), { Shift+Alt+8 }
    ( Name : 'Search_BkmSet9'; Shortcut : 41017; Hint : 'Set bookmark ' ), { Shift+Alt+9 }
    ( Name : 'Search_Find'; Shortcut : 16454; Hint : 'Search for text in notes' ), { Ctrl+F }
    ( Name : 'Search_Findagain'; Shortcut : 114; Hint : 'Repeat last search command' ), { F3 }
    ( Name : 'Search_FindNextNode'; Shortcut : 8306; Hint : 'Repeat last Find node command' ), { Shift+F3 }
    ( Name : 'Search_FindNode'; Shortcut : 24646; Hint : 'Find tree node' ), { Shift+Ctrl+F }
    ( Name : 'Search_Goto'; Shortcut : 16455; Hint : 'Move to a specific line in note' ), { Ctrl+G }
    ( Name : 'Search_MatchBracket'; Shortcut : 16461; Hint : 'Find matching bracket' ), { Ctrl+M }
    ( Name : 'Search_Replace'; Shortcut : 16498; Hint : 'Replace text in notes' ), { Ctrl+F3 }
    ( Name : 'Search_ReplaceNext'; Shortcut : 0; Hint : 'Repeat last replace command' ), {  }
    ( Name : 'Tools_AddTerm'; Shortcut : 8310; Hint : 'Create a new glossary term' ), { Shift+F7 }
    ( Name : 'Tools_Calculator'; Shortcut : 0; Hint : 'Display a calculator' ), {  }
    ( Name : 'Tools_Defaults'; Shortcut : 117; Hint : 'Set defaults for all newly created files' ), { F6 }
    ( Name : 'Tools_EditGlossary'; Shortcut : 0; Hint : 'Edit glossary terms' ), {  }
    ( Name : 'Tools_Export'; Shortcut : 0; Hint : 'Export text to another format' ), {  }
    ( Name : 'Tools_Import'; Shortcut : 0; Hint : 'Import files as notes' ), {  }
    ( Name : 'Tools_MergeWithFile'; Shortcut : 0; Hint : 'Add notes from a file on disk' ), {  }
    ( Name : 'Tools_Options'; Shortcut : 116; Hint : 'Adjust program options' ), { F5 }
    ( Name : 'Tools_RunPlugin'; Shortcut : 16507; Hint : 'Execute selected plugin' ), { Ctrl+F12 }
    ( Name : 'Tools_RunLastMacro'; Shortcut : 16500; Hint : 'Execute most recent macro' ), { Ctrl+F5 }
    ( Name : 'Tools_RunLastPlugin'; Shortcut : 8315; Hint : 'Execute most recent plugin used' ), { Shift+F12 }
    ( Name : 'Tools_RunMacro'; Shortcut : 8308; Hint : 'Execute selected macro' ), { Shift+F5 }
    ( Name : 'Tools_SelectMacro'; Shortcut : 0; Hint : 'Select and run macro' ), {  }
    ( Name : 'Tools_Stats'; Shortcut : 0; Hint : 'Display note statistics' ), {  }
    ( Name : 'Tools_TplCreate'; Shortcut : 0; Hint : 'Create new template based on selected text' ), {  }
    ( Name : 'Tools_TplInsert'; Shortcut : 0; Hint : 'Insert template into current note' ), {  }
    ( Name : 'Tree_AsDate'; Shortcut : 0; Hint : 'Use current date as node name' ), {  }
    ( Name : 'Tree_AsDateandTime'; Shortcut : 0; Hint : 'Use current date and time as node name' ), {  }
    ( Name : 'Tree_AsTime'; Shortcut : 0; Hint : 'Use current time as node name' ), {  }
    ( Name : 'Tree_DeleteChildnodes'; Shortcut : 0; Hint : 'Delete children of selected node' ), {  }
    ( Name : 'Tree_FromClipboard'; Shortcut : 0; Hint : 'Use clipboard contents as node name' ), {  }
    ( Name : 'Tree_MoveNodeDown'; Shortcut : 0; Hint : 'Move node DOWN' ), {  }
    ( Name : 'Tree_MoveNodeLeft'; Shortcut : 0; Hint : 'Shift node LEFT by 1 level' ), {  }
    ( Name : 'Tree_MoveNodeRight'; Shortcut : 0; Hint : 'Shift node RIGHT by 1 level' ), {  }
    ( Name : 'Tree_MoveNodeUp'; Shortcut : 0; Hint : 'Move node UP' ), {  }
    ( Name : 'Tree_NodeAdd'; Shortcut : 0; Hint : 'Add new tree node' ), {  }
    ( Name : 'Tree_NodeAddChild'; Shortcut : 0; Hint : 'Add new tree node as child' ), {  }
    ( Name : 'Tree_NodeCollapseAll'; Shortcut : 0; Hint : 'Fully collapse tree' ), {  }
    ( Name : 'Tree_NodeDelete'; Shortcut : 0; Hint : 'Delete selected node' ), {  }
    ( Name : 'Tree_NodeExpandAll'; Shortcut : 0; Hint : 'Fully expand tree' ), {  }
    ( Name : 'Tree_NodeInsert'; Shortcut : 0; Hint : 'Insert new tree node' ), {  }
    ( Name : 'Tree_NodeRename'; Shortcut : 0; Hint : 'Rename node' ), {  }
    ( Name : 'Tree_SaveTreetoFile'; Shortcut : 0; Hint : 'Save tree structure to file' ), {  }
    ( Name : 'Tree_SortChildNodes'; Shortcut : 0; Hint : 'Sort child nodes (subtree)' ), {  }
    ( Name : 'Tree_SortFullTree'; Shortcut : 0; Hint : 'Sort all nodes in tree' ), {  }
    ( Name : 'View_Alphabetizetabs'; Shortcut : 0; Hint : 'Sort notes alphabetically by name' ), {  }
    ( Name : 'View_Alwaysontop'; Shortcut : 119; Hint : 'Keep program inwod on top of other windows' ), { F8 }
    ( Name : 'View_Checkboxes'; Shortcut : 0; Hint : 'Show or hide tree checkboxes' ), {  }
    ( Name : 'View_FormatToolbar'; Shortcut : 0; Hint : 'Display or hide the Formatting toolbar' ), {  }
    ( Name : 'View_FormatViewBoth'; Shortcut : 0; Hint : 'Display font and paragraph formatting' ), {  }
    ( Name : 'View_FormatViewFont'; Shortcut : 0; Hint : 'Display font formatting' ), {  }
    ( Name : 'View_FormatViewNone'; Shortcut : 0; Hint : 'Display no formatting' ), {  }
    ( Name : 'View_FormatViewPara'; Shortcut : 0; Hint : 'Display paragraph formatting' ), {  }
    ( Name : 'View_HideAllToolbars'; Shortcut : 0; Hint : 'Hide all toolbars' ), {  }
    ( Name : 'View_MacroToolbar'; Shortcut : 0; Hint : 'Display or hide the Macro toolbar' ), {  }
    ( Name : 'View_MainToolbar'; Shortcut : 0; Hint : 'Display or hide main toolbar' ), {  }
    ( Name : 'View_NodeIcons'; Shortcut : 0; Hint : 'Show or hide tree icons' ), {  }
    ( Name : 'View_ShiftLeft'; Shortcut : 0; Hint : 'Move active tab left' ), {  }
    ( Name : 'View_ShiftRight'; Shortcut : 0; Hint : 'Move active tab right' ), {  }
    ( Name : 'View_ShowAllToolbars'; Shortcut : 0; Hint : 'Display all toolbars' ), {  }
    ( Name : 'View_ShowIconsontabs'; Shortcut : 0; Hint : 'Show or hide icons on note tabs' ), {  }
    ( Name : 'View_ShowTree'; Shortcut : 8314; Hint : 'Show or hide tree panel' ), { Shift+F11 }
    ( Name : 'View_StyleToolbar'; Shortcut : 0; Hint : 'Display or hide the Style toolbar' ), {  }
    ( Name : 'View_TreeToolbar'; Shortcut : 0; Hint : 'Display or hide the Tree toolbar' ), {  }
    ( Name : 'View_ViewResPanel'; Shortcut : 120; Hint : 'Show or hide resource panel' ) { F9 }
  );


const
  KNTTreeMenuCmds : TKNTTreeMenuCmds = (
    ( Name : 'TVMAddChildNode'; Shortcut : 8205; Hint : 'Add node as child of current node' ), { Shift+Enter }
    ( Name : 'TVMAddNode'; Shortcut : 13; Hint : 'Add node as last sibling' ), { Enter }
    ( Name : 'TVMAqua'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMAsDate'; Shortcut : 0; Hint : 'Use current date as selected node name' ), {  }
    ( Name : 'TVMAsDateandTime'; Shortcut : 0; Hint : 'Use current date and time as selected node name' ), {  }
    ( Name : 'TVMAsTime'; Shortcut : 0; Hint : 'Use current time as selected node name' ), {  }
    ( Name : 'TVMBlack'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMBlue'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMBold'; Shortcut : 16450; Hint : 'Make selected node bold' ), { Ctrl+B }
    ( Name : 'TVMChecked'; Shortcut : 16576; Hint : 'Make selected node checked' ), { Ctrl+` }
    ( Name : 'TVMCopynodename'; Shortcut : 0; Hint : 'Copy node name to clipboard' ), {  }
    ( Name : 'TVMCopySubtree'; Shortcut : 0; Hint : 'Copy selected node and its children' ), {  }
    ( Name : 'TVMDefault'; Shortcut : 0; Hint : 'Reset node color to default' ), {  }
    ( Name : 'TVMDeleteChildren'; Shortcut : 8238; Hint : 'Delete child nodes of selected node' ), { Shift+Del }
    ( Name : 'TVMDeletenode'; Shortcut : 46; Hint : 'Delete selected node' ), { Del }
    ( Name : 'TVMDown'; Shortcut : 8232; Hint : 'Move node DOWN' ), { Shift+Down }
    ( Name : 'TVMEraseMem'; Shortcut : 0; Hint : 'Forget previously copied nodes' ), {  }
    ( Name : 'TVMExport'; Shortcut : 0; Hint : 'Export node contents to file' ), {  }
    ( Name : 'TVMFromClipboard'; Shortcut : 0; Hint : 'Use clipboard contents as selected node name' ), {  }
    ( Name : 'TVMFuchsia'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMGray'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMGreen'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMInsertNode'; Shortcut : 45; Hint : 'Add node before current node' ), { Ins }
    ( Name : 'TVMLeft'; Shortcut : 8229; Hint : 'Shift node LEFT by 1 level' ), { Shift+Left }
    ( Name : 'TVMLime'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMMaroon'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMNavy'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMOlive'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMPasteSubtree'; Shortcut : 0; Hint : 'Paste previously copied nodes at selected position' ), {  }
    ( Name : 'TVMPurple'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMRed'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMRefresh'; Shortcut : 0; Hint : 'Refresh contents from original file on disk' ), {  }
    ( Name : 'TVMRenamenode'; Shortcut : 8305; Hint : 'Rename node' ), { Shift+F2 }
    ( Name : 'TVMRight'; Shortcut : 8231; Hint : 'Shift node RIGHT by 1 level' ), { Shift+Right }
    ( Name : 'TVMSilver'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMSortchildnodes'; Shortcut : 0; Hint : 'Sort children of current node' ), {  }
    ( Name : 'TVMSortfullTree'; Shortcut : 0; Hint : 'Sort all nodes in tree' ), {  }
    ( Name : 'TVMTeal'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMUp'; Shortcut : 8230; Hint : 'Move node UP' ), { Shift+Up }
    ( Name : 'TVMVirtualNode'; Shortcut : 0; Hint : 'Link a file on disk to selected node' ), {  }
    ( Name : 'TVMWhite'; Shortcut : 0; Hint : 'Select color for node' ), {  }
    ( Name : 'TVMYellow'; Shortcut : 0; Hint : 'Select color for node' ) {  }
  );



procedure LoadKBD( const fn : string;
  var MMCmds : TKNTMainMenuCmds;
  var TVCmds : TKNTTreeMenuCmds
  );

procedure SaveKBD( const fn : string;
  const MMCmds : TKNTMainMenuCmds;
  const TVCmds : TKNTTreeMenuCmds
  );

implementation


procedure LoadKBD( const fn : string;
  var MMCmds : TKNTMainMenuCmds;
  var TVCmds : TKNTTreeMenuCmds
  );
var
  IniFile : TIniFile;
  section : string;
  mmcmd : TMainMenuCmd;
  tvcmd : TTreeMenuCmd;
begin
  IniFile := TIniFile.Create( fn );
  try
    try
      with IniFile do
      begin

        section := 'main';
        for mmcmd := low( mmcmd ) to high( mmcmd ) do
        begin
          MMCmds[mmcmd].ShortCut := readinteger( section, MMCmds[mmcmd].Name, 0 );
        end;

        section := 'tree';
        for tvcmd := low( tvcmd ) to high( tvcmd ) do
        begin
          TVCmds[tvcmd].ShortCut := readinteger( section, TVCmds[tvcmd].Name, 0 );
        end;

      end;
    except
    end;
  finally
    IniFile.Free;
  end;
end; // LoadKBD

procedure SaveKBD( const fn : string;
  const MMCmds : TKNTMainMenuCmds;
  const TVCmds : TKNTTreeMenuCmds
  );
var
  IniFile : TIniFile;
  section : string;
  mmcmd : TMainMenuCmd;
  tvcmd : TTreeMenuCmd;
begin
  IniFile := TIniFile.Create( fn );
  try
    try
      with IniFile do
      begin
        section := 'main';
        for mmcmd := low( mmcmd ) to high( mmcmd ) do
        begin
          writeinteger( section, MMCmds[mmcmd].Name, MMCmds[mmcmd].ShortCut );
        end;

        section := 'tree';
        for tvcmd := low( tvcmd ) to high( tvcmd ) do
        begin
          writeinteger( section, TVCmds[tvcmd].Name, TVCmds[tvcmd].ShortCut );
        end;
      end;
    except
    end;
  finally
    IniFile.Free;
  end;
end; // SaveKBD

end.
