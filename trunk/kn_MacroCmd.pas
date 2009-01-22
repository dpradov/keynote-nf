
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

unit kn_MacroCmd;

interface

const
  _MACRO_COMMENT_CHAR   = ';';
  _MACRO_CMD_CHAR       = '#';
  _MACRO_USERCMD_CHAR   = '@';
  _MACRO_DELIMITER_CHAR = '|';

  STR_style = 'bold|italic|underline|strike';

type
  TMacroCmd = (
    macInsert, macWait, macRewind, macMessage, macStatus,
    macPlugin, macMacro, macConfirm,
    macStyleOn, macStyleOff, macStyleFlip,
    macGoLeft, macGoRight, macGoDown, macGoUp,
    macSelectAll,
    macFontColor, macBGColor, macHighlightColor,
    macNoteNewRTF, macNoteNewTree,
    macApplyStyle, macBookmarkSet, macBookmarkJump
  );
  TMacroCommands = set of TMacroCmd;

type
  TArgType = ( argNone, argString, argChar, argInteger, argBoolean );

  TMacroCmdDef = record
    Name : string;
    Help : string;
    Syntax : string;
    ArgType : TArgType;
    DefArg : string;
  end;

  TMacroCmdDefs = array[TMacroCmd] of TMacroCmdDef;

var
  MACRO_DEFS : TMacroCmdDefs;

const
  MACRO_CMD_NAMES : array[TMacroCmd] of string = (
    'INSERT', 'WAIT', 'REWIND', 'MESSAGE', 'STATUS',
    'PLUGIN', 'MACRO', 'CONFIRM',
    'STYLEON', 'STYLEOFF', 'STYLEFLIP',
    'GOLEFT', 'GORIGHT', 'GODOWN', 'GOUP',
    'SELECTALL',
    'FONTCOLOR', 'BGCOLOR', 'HICOLOR',
    'NOTENEWRTF', 'NOTENEWTREE',
    'APPLYSTYLE', 'BOOKMARKSET', 'BOOKMARKJUMP'
  );

const

  MACRO_CMDS_WITH_STRING_ARGS : TMacroCommands = [
    macInsert, macMessage, macStatus,
    macPlugin, macMacro, macConfirm,
    macStyleOn, macStyleOff, macStyleFlip,
    macApplyStyle

  ];

  MACRO_CMDS_WITH_OPTIONAL_STRING_ARGS : TMacroCommands = [
    macStatus,
    macFontColor, macBGColor, macHighlightColor
  ];

  MACRO_CMDS_WITH_INTEGER_ARGS : TMacroCommands = [
    macWait,
    macGoLeft, macGoRight, macGoDown, macGoUp,
    macBookmarkSet, macBookmarkJump
  ];

  MACRO_CMDS_WITH_OPTIONAL_INTEGER_ARGS : TMacroCommands = [
    macWait,
    macGoLeft, macGoRight, macGoDown, macGoUp
  ];


implementation

resourcestring
  STR_Str = 'string';
  STR_ms = 'miliseconds';
  STR_fn = 'filename';
  STR_int = 'integer';
  STR_cn = 'color name';
  STR_stN = 'style name';
  STR_10 = 'Inserts a line of text into active note';
  STR_11 = 'Pauses for a specified period (miliseconds)';
  STR_12 = 'Rewinds macro to beginning and starts again (macro will run in infinite loop until ESC key pressed)';
  STR_13 = 'Displays a dialog box with a message text';
  STR_14 = 'Displays message in status bar';
  STR_15 = 'Executes the specified plugin';
  STR_16 = 'Executes the specified macro';
  STR_17 = 'Displays an OK / CANCEL dialog box and aborts macro if CANCEL is pressed';
  STR_18 = 'Turns ON specified font style';
  STR_19 = 'Turns OFF specified font style';
  STR_20 = 'Toggles specified font style';
  STR_21 = 'Moves caret left';
  STR_22 = 'Moves caret right';
  STR_23 = 'Moves caret down';
  STR_24 = 'Moves caret up';
  STR_25 = 'Selects all text in active note';
  STR_26 = 'Selects new font color';
  STR_27 = 'Selects new background color';
  STR_28 = 'Selects new highlight color';
  STR_29 = 'Creates a new standard RTF note';
  STR_30 = 'Creates a new tree-type note';
  STR_31 = 'Applies named style to text';
  STR_32 = 'Sets a new bookmark';
  STR_33 = 'Jumps to previously set bookmark';

var
  mcmd : TMacroCmd;

Initialization


  for mcmd := low( TMacroCmd ) to high( TMacroCmd ) do
  begin
    case mcmd of
      macInsert : with MACRO_DEFS[mcmd] do
      begin
         Name := 'INSERT';
         Help :=  STR_10;
         Syntax := STR_Str;
         ArgType := argString;
         DefArg := '';
       end;
      macWait : with MACRO_DEFS[mcmd] do
      begin
         Name := 'WAIT';
         Help := STR_11;
         Syntax := STR_ms;
         ArgType := argInteger;
         DefArg := '250';
       end;
      macRewind : with MACRO_DEFS[mcmd] do
      begin
         Name := 'REWIND';
         Help := STR_12;
         Syntax := '';
         ArgType := argNone;
         DefArg := '';
       end;
      macMessage : with MACRO_DEFS[mcmd] do
      begin
         Name := 'MESSAGE';
         Help := STR_13;
         Syntax := STR_Str;
         ArgType := argString;
         DefArg := '';
       end;
      macStatus : with MACRO_DEFS[mcmd] do
      begin
         Name := 'STATUS';
         Help := STR_14;
         Syntax := STR_Str;
         ArgType := argString;
         DefArg := '';
       end;
      macPlugin : with MACRO_DEFS[mcmd] do
      begin
         Name := 'PLUGIN';
         Help := STR_15;
         Syntax := STR_fn;
         ArgType := argString;
         DefArg := '';
       end;
      macMacro : with MACRO_DEFS[mcmd] do
      begin
         Name := 'MACRO';
         Help := STR_16;
         Syntax := STR_fn;
         ArgType := argString;
         DefArg := '';
       end;
      macConfirm : with MACRO_DEFS[mcmd] do
      begin
         Name := 'CONFIRM';
         Help := STR_17;
         Syntax := STR_Str;
         ArgType := argString;
         DefArg := '';
       end;
      macStyleOn : with MACRO_DEFS[mcmd] do
      begin
         Name := 'STYLEON';
         Help := STR_18;
         Syntax := STR_style;
         ArgType := argString;
         DefArg := '';
       end;
      macStyleOff : with MACRO_DEFS[mcmd] do
      begin
         Name := 'STYLEOFF';
         Help := STR_19;
         Syntax := STR_style;
         ArgType := argString;
         DefArg := '';
       end;
      macStyleFlip : with MACRO_DEFS[mcmd] do
      begin
         Name := 'STYLEFLIP';
         Help := STR_20;
         Syntax := STR_style;
         ArgType := argString;
         DefArg := '';
       end;
      macGoLeft : with MACRO_DEFS[mcmd] do
      begin
         Name := 'GOLEFT';
         Help := STR_21;
         Syntax := STR_int;
         ArgType := argInteger;
         DefArg := '1';
       end;
      macGoRight : with MACRO_DEFS[mcmd] do
      begin
         Name := 'GORIGHT';
         Help := STR_22;
         Syntax := STR_int;
         ArgType := argInteger;
         DefArg := '1';
       end;
      macGoDown : with MACRO_DEFS[mcmd] do
      begin
         Name := 'GODOWN';
         Help := STR_23;
         Syntax := STR_int;
         ArgType := argInteger;
         DefArg := '1';
       end;
      macGoUp : with MACRO_DEFS[mcmd] do
      begin
         Name := 'GOUP';
         Help := STR_24;
         Syntax := STR_int;
         ArgType := argInteger;
         DefArg := '1';
       end;
      macSelectAll : with MACRO_DEFS[mcmd] do
      begin
         Name := 'SELECTALL';
         Help := STR_25;
         Syntax := '';
         ArgType := argNone;
         DefArg := '';
       end;
      macFontColor : with MACRO_DEFS[mcmd] do
      begin
         Name := 'FONTCOLOR';
         Help := STR_26;
         Syntax := STR_cn;
         ArgType := argString;
         DefArg := '';
       end;
      macBGColor : with MACRO_DEFS[mcmd] do
      begin
         Name := 'BGCOLOR';
         Help := STR_27;
         Syntax := STR_cn;
         ArgType := argString;
         DefArg := '';
       end;
      macHighlightColor : with MACRO_DEFS[mcmd] do
      begin
         Name := 'HICOLOR';
         Help := STR_28;
         Syntax := STR_cn;
         ArgType := argString;
         DefArg := '';
       end;
      macNoteNewRTF : with MACRO_DEFS[mcmd] do
      begin
         Name := 'NOTENEWRTF';
         Help := STR_29;
         Syntax := '';
         ArgType := argNone;
         DefArg := '';
       end;
      macNoteNewTree : with MACRO_DEFS[mcmd] do
      begin
         Name := 'NOTENEWTREE';
         Help := STR_30;
         Syntax := '';
         ArgType := argNone;
         DefArg := '';
       end;
      macApplyStyle : with MACRO_DEFS[mcmd] do
      begin
         Name := 'APPLYSTYLE';
         Help := STR_31;
         Syntax := STR_stN;
         ArgType := argString;
         DefArg := '';
       end;
      macBookmarkSet : with MACRO_DEFS[mcmd] do
      begin
         Name := 'BOOKMARKSET';
         Help := STR_32;
         Syntax := '0..9';
         ArgType := argInteger;
         DefArg := '';
       end;
      macBookmarkJump : with MACRO_DEFS[mcmd] do
      begin
         Name := 'BOOKMARKJUMP';
         Help := STR_33;
         Syntax := '0..9';
         ArgType := argInteger;
         DefArg := '';
       end;
    end;
  end;






Finalization

end.
