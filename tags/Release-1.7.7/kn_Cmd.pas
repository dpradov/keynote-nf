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

unit kn_Cmd;

interface
uses Classes;

type
  TKeyCode = record
    Key : Word;
    Shift : TShiftState;
    Special : boolean;
  end;

type
  // RTF editor commands, in no particular order
  TEditCmd = (
    ecAlignCenter,
    ecAlignJustify,
    ecAlignLeft,
    ecAlignRight,
    ecBGColorDlg,
    ecBold,
    ecBullets,
    ecClearFontAttr,
    ecClearParaAttr,
    ecCopy,
    ecCut,
    ecCycleCase,
    ecDelete,
    ecDeleteLine,
    ecDisabled,
    ecEvaluateExpression,  // xxx
    ecExpandTerm, // xxx
    ecFindText, // xxx
    ecFirstIndent,
    ecFirstOutdent,
    ecFontColor,
    ecFontColorBtn,
    ecFontColorDlg,
    ecFontDlg,
    ecFontFormatCopy,
    ecFontFormatPaste,
    ecHighlightBtn,
    ecFontName,
    ecFontSize,
    ecFontSizeDec,
    ecFontSizeInc,
    ecGoTo,
    ecHidden,
    ecHighlight,
    ecHighlightDlg,
    ecIndent,
    ecInsCharacter,
    ecInsDate,
    ecInsTime,
    ecInsOvrToggle,
    ecInvertCase,
    ecItalics,
    ecJoinLines,
    ecLanguage,
    ecMatchBracket, // xxx
    ecNoHighlight,
    ecNone,
    ecNumbers,
    ecOutdent,
    ecParaDlg,
    ecParaFormatCopy,
    ecParaFormatPaste,
    ecPaste,
    ecPastePlain,
    ecReadOnly,
    ecRedo,
    ecReformat,
    ecReverseText,
    ecRightIndent,
    ecRightOutdent,
    ecROT13,
    ecSelectAll,
    ecSelectWord,
    ecSort,
    ecSpace1,
    ecSpace15,
    ecSpace2,
    ecSpaceAfterDec,
    ecSpaceAfterInc,
    ecSpaceBeforeDec,
    ecSpaceBeforeInc,
    ecStrikeOut,
    ecStyleApply, // xxx
    ecSubscript,
    ecSuperscript,
    ecToLowerCase,
    ecToMixedCase,
    ecToUpperCase,
    ecUnderline,
    ecUndo,
    ecWordWrap
  );

const
  EDITCMD_NAMES : array[TEditCmd] of string = (
    'Align Center',
    'Align Justify',
    'Align Left',
    'Align Right',
    'Background Color',
    'Bold',
    'Bullets',
    'Clear Font Attributes',
    'Clear Paragraph Attributes',
    'Copy',
    'Cut',
    'Cycle Case',
    'Delete',
    'Delete Line',
    'Disabled',
    'Evaluate Expr',
    'Expand Term',
    'Find Text',
    'First Indent',
    'First Outdent',
    'Font Color',
    'Font Color',
    'Font Color Dlg',
    'Font Select',
    'Font Format Copy',
    'Font Format Paste',
    'Highlight Apply',
    'Font Name',
    'Font Size',
    'Font Size Dec',
    'Font Size Inc',
    'Go To',
    'Hidden',
    'Highlight',
    'Highlight Dlg',
    'Indent',
    'Insert Character',
    'Insert Date',
    'Insert Time',
    'INS/OVR toggle',
    'Invert Case',
    'Italics',
    'Join Lines',
    'Language',
    'Match Bracket',
    'No Highlight',
    '(None)', // ecNone
    'Numbers',
    'Outdent',
    'Paragraph',
    'Para Format Copy',
    'Para Format Paste',
    'Paste',
    'Paste Plain',
    'Read Only',
    'Redo',
    'Reformat',
    'Reverse Text',
    'Right Indent',
    'Right Outdent',
    'ROT13',
    'Select All',
    'Select Word',
    'Sort',
    'Space 1',
    'Space 1.5',
    'Space 2',
    'Space After Dec',
    'Space After Inc',
    'Space Before Dec',
    'Space Before Inc',
    'Strikeout',
    'Apply Style',
    'Subscript',
    'Superscript',
    'Lowercase',
    'Mixed Case',
    'Uppercase',
    'Underline',
    'Undo',
    'Word Wrap'
  );

type
  TEditCmds = set of TEditCmd;

const
  // these commands do not modify note,
  // so they can be executed even if note is read-only
  EditCommandsEx : TEditCmds = [
    ecCopy, ecReadOnly,
    ecFontFormatCopy,
    ecParaFormatCopy,
    ecSelectWord,
    ecGoTo, ecInsOvrToggle,
    ecMatchBracket, ecFindText
  ];

const
  // these commands can be repeated by just
  // issuing the command
  RepeatableEditCommands : TEditCmds = [
    ecBold, ecUnderline, ecItalics, ecStrikeOut,
    ecCut,
    ecCycleCase,
    ecCopy,
    ecPaste, ecPastePlain, ecDelete,
    ecBullets, ecNumbers, ecIndent, ecOutdent,
    ecRightIndent, ecRightOutdent,
    ecFirstIndent, ecFirstOutdent,
    ecAlignLeft, ecAlignCenter, ecAlignRight,
    ecAlignJustify,
    ecFontSizeInc, ecFontSizeDec,
    ecFontColor,
    ecWordWrap,
    ecSelectWord,
    ecEvaluateExpression, ecExpandTerm,
    ecClearFontAttr, ecClearParaAttr,
    ecDeleteLine,
    ecJoinLines,
    ecToUpperCase, ecToLowerCase,
    ecToMixedCase, ecInvertCase,
    ecInsDate, ecInsTime,
    ecSort,
    ecReformat,
    ecFontFormatPaste,
    ecParaFormatPaste,
    ecDisabled, ecHidden,
    ecSubscript, ecSuperscript,
    ecSpaceBeforeInc, ecSpaceBeforeDec,
    ecSpaceAfterInc, ecSpaceAfterDec,
    ecSpace1, ecSpace15, ecSpace2,
    ecReverseText, ecROT13, ecNoHighlight
  ];

const
  // these commands can be repeated, but we
  // must store parameter used with the command
  RememberedEditCommands : TEditCmds = [
    ecBGColorDlg,
    ecFindText,
    ecFontColorBtn,
    ecFontDlg,
    ecFontName,
    ecFontSize,
    ecFontColorDlg,
    ecGoTo,
    ecHighlight,
    ecHighlightBtn,
    ecHighlightDlg,
    ecInsCharacter,
    ecLanguage,
    ecParaDlg,
    ecStyleApply
  ];

const
  // these commands cannot be stored in macros
  CommandsProhibitedInMacros : TEditCmds = [
    ecReadOnly, ecInsOvrToggle
  ];


const
  // these commands do not take any arguments
  EditCommandsWithNoArgs : TEditCmds = [
    ecAlignCenter,
    ecAlignJustify,
    ecAlignLeft,
    ecAlignRight,
    ecBold,
    ecBullets,
    ecClearFontAttr,
    ecClearParaAttr,
    ecCopy,
    ecCut,
    ecCycleCase,
    ecDelete,
    ecDeleteLine,
    ecDisabled,
    ecEvaluateExpression,
    ecExpandTerm,
    ecFirstIndent,
    ecFirstOutdent,
    ecFontFormatCopy,
    ecFontFormatPaste,
    ecFontSizeDec,
    ecFontSizeInc,
    ecHidden,
    ecIndent,
    ecInsDate,
    ecInsTime,
    ecInvertCase,
    ecItalics,
    ecJoinLines,
    ecMatchBracket,
    ecNoHighlight,
    ecNumbers,
    ecOutdent,
    ecParaFormatCopy,
    ecParaFormatPaste,
    ecPaste,
    ecPastePlain,
    ecRedo,
    ecReformat,
    ecReverseText,
    ecRightIndent,
    ecRightOutdent,
    ecROT13,
    ecSelectAll,
    ecSelectWord,
    ecSort,
    ecSpace1,
    ecSpace15,
    ecSpace2,
    ecSpaceAfterDec,
    ecSpaceAfterInc,
    ecSpaceBeforeDec,
    ecSpaceBeforeInc,
    ecStrikeOut,
    ecSubscript,
    ecSuperscript,
    ecToLowerCase,
    ecToMixedCase,
    ecToUpperCase,
    ecUnderline,
    ecUndo,
    ecWordWrap
  ];

const
  // these commands invoke dialog boxes
  EditCommandsWithDialogs : TEditCmds = [
    ecBGColorDlg,
    ecFontColorDlg,
    ecFontDlg,
    // ecInsCharacter, [x] cannot show a non-modal dialog while macro is running
    ecGoTo,
    ecHighlightDlg,
    ecLanguage,
    ecParaDlg
  ];

const
  EDITCMD_MACRO_NAMES : array[TEditCmd] of string = (
    'aligncenter',
    'alignjustify',
    'alignleft',
    'alignright',
    'bgcolor',
    'bold',
    'bullets',
    'clearfontattr',
    'clearparaattr',
    'copy',
    'cut',
    'cyclecase',
    'delete',
    'deleteline',
    'disabled',
    'eval',
    'expandterm',
    'findtext',
    'firstindent',
    'firstoutdent',
    'fontcolor',
    'fontcolorbtn',
    'fontcolordlg',
    'fontselect',
    'fontformatcopy',
    'fontformatpaste',
    'highlightapply',
    'fontname',
    'fontsize',
    'fontsizedec',
    'fontsizeinc',
    'goto',
    'hidden',
    'highlight',
    'highlightdlg',
    'indent',
    'insertchar',
    'insertdate',
    'inserttime',
    'insovr',
    'invertcase',
    'italics',
    'joinlines',
    'language',
    'matchbracket',
    'nohighlight',
    '(none)', // ecnone
    'numbers',
    'outdent',
    'paragraph',
    'paraformatcopy',
    'paraformatpaste',
    'paste',
    'pasteplain',
    'readonly',
    'redo',
    'reformat',
    'reversetext',
    'rightindent',
    'rightoutdent',
    'rot13',
    'selectall',
    'selectword',
    'sort',
    'space1',
    'space15',
    'space2',
    'spaceafterdec',
    'spaceafterinc',
    'spacebeforedec',
    'spacebeforeinc',
    'strikeout',
    'styleapply',
    'subscript',
    'superscript',
    'lowercase',
    'mixedcase',
    'uppercase',
    'underline',
    'undo',
    'wordwrap'
  );


implementation

end.
