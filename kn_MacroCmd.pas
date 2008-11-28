
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
var
  mcmd : TMacroCmd;

Initialization


  for mcmd := low( TMacroCmd ) to high( TMacroCmd ) do
  begin
    case mcmd of
      macInsert : with MACRO_DEFS[mcmd] do
      begin
         Name := 'INSERT';
         Help :=  'Inserts a line of text into active note';
         Syntax := 'string';
         ArgType := argString;
         DefArg := '';
       end;
      macWait : with MACRO_DEFS[mcmd] do
      begin
         Name := 'WAIT';
         Help := 'Pauses for a specified period (miliseconds)';
         Syntax := 'miliseconds';
         ArgType := argInteger;
         DefArg := '250';
       end;
      macRewind : with MACRO_DEFS[mcmd] do
      begin
         Name := 'REWIND';
         Help := 'Rewinds macro to beginning and starts again (macro will run in infinite loop until ESC key pressed)';
         Syntax := '';
         ArgType := argNone;
         DefArg := '';
       end;
      macMessage : with MACRO_DEFS[mcmd] do
      begin
         Name := 'MESSAGE';
         Help := 'Displays a dialog box with a message text';
         Syntax := 'string';
         ArgType := argString;
         DefArg := '';
       end;
      macStatus : with MACRO_DEFS[mcmd] do
      begin
         Name := 'STATUS';
         Help := 'Displays message in status bar';
         Syntax := 'string';
         ArgType := argString;
         DefArg := '';
       end;
      macPlugin : with MACRO_DEFS[mcmd] do
      begin
         Name := 'PLUGIN';
         Help := 'Executes the specified plugin';
         Syntax := 'filename';
         ArgType := argString;
         DefArg := '';
       end;
      macMacro : with MACRO_DEFS[mcmd] do
      begin
         Name := 'MACRO';
         Help := 'Executes the specified macro';
         Syntax := 'filename';
         ArgType := argString;
         DefArg := '';
       end;
      macConfirm : with MACRO_DEFS[mcmd] do
      begin
         Name := 'CONFIRM';
         Help := 'Displays an OK / CANCEL dialog box and aborts macro if CANCEL is pressed';
         Syntax := 'string';
         ArgType := argString;
         DefArg := '';
       end;
      macStyleOn : with MACRO_DEFS[mcmd] do
      begin
         Name := 'STYLEON';
         Help := 'Turns ON specified font style';
         Syntax := 'bold|italic|underline|strike';
         ArgType := argString;
         DefArg := '';
       end;
      macStyleOff : with MACRO_DEFS[mcmd] do
      begin
         Name := 'STYLEOFF';
         Help := 'Turns OFF specified font style';
         Syntax := 'bold|italic|underline|strike';
         ArgType := argString;
         DefArg := '';
       end;
      macStyleFlip : with MACRO_DEFS[mcmd] do
      begin
         Name := 'STYLEFLIP';
         Help := 'Toggles specified font style';
         Syntax := 'bold|italic|underline|strike';
         ArgType := argString;
         DefArg := '';
       end;
      macGoLeft : with MACRO_DEFS[mcmd] do
      begin
         Name := 'GOLEFT';
         Help := 'Moves caret left';
         Syntax := 'integer';
         ArgType := argInteger;
         DefArg := '1';
       end;
      macGoRight : with MACRO_DEFS[mcmd] do
      begin
         Name := 'GORIGHT';
         Help := 'Moves caret right';
         Syntax := 'integer';
         ArgType := argInteger;
         DefArg := '1';
       end;
      macGoDown : with MACRO_DEFS[mcmd] do
      begin
         Name := 'GODOWN';
         Help := 'Moves caret down';
         Syntax := 'integer';
         ArgType := argInteger;
         DefArg := '1';
       end;
      macGoUp : with MACRO_DEFS[mcmd] do
      begin
         Name := 'GOUP';
         Help := 'Moves caret up';
         Syntax := 'integer';
         ArgType := argInteger;
         DefArg := '1';
       end;
      macSelectAll : with MACRO_DEFS[mcmd] do
      begin
         Name := 'SELECTALL';
         Help := 'Selects all text in active note';
         Syntax := '';
         ArgType := argNone;
         DefArg := '';
       end;
      macFontColor : with MACRO_DEFS[mcmd] do
      begin
         Name := 'FONTCOLOR';
         Help := 'Selects new font color';
         Syntax := 'color name';
         ArgType := argString;
         DefArg := '';
       end;
      macBGColor : with MACRO_DEFS[mcmd] do
      begin
         Name := 'BGCOLOR';
         Help := 'Selects new background color';
         Syntax := 'color name';
         ArgType := argString;
         DefArg := '';
       end;
      macHighlightColor : with MACRO_DEFS[mcmd] do
      begin
         Name := 'HICOLOR';
         Help := 'Selects new highlight color';
         Syntax := 'color name';
         ArgType := argString;
         DefArg := '';
       end;
      macNoteNewRTF : with MACRO_DEFS[mcmd] do
      begin
         Name := 'NOTENEWRTF';
         Help := 'Creates a new standard RTF note';
         Syntax := '';
         ArgType := argNone;
         DefArg := '';
       end;
      macNoteNewTree : with MACRO_DEFS[mcmd] do
      begin
         Name := 'NOTENEWTREE';
         Help := 'Creates a new tree-type note';
         Syntax := '';
         ArgType := argNone;
         DefArg := '';
       end;
      macApplyStyle : with MACRO_DEFS[mcmd] do
      begin
         Name := 'APPLYSTYLE';
         Help := 'Applies named style to text';
         Syntax := 'style name';
         ArgType := argString;
         DefArg := '';
       end;
      macBookmarkSet : with MACRO_DEFS[mcmd] do
      begin
         Name := 'BOOKMARKSET';
         Help := 'Sets a new bookmark';
         Syntax := '0..9';
         ArgType := argInteger;
         DefArg := '';
       end;
      macBookmarkJump : with MACRO_DEFS[mcmd] do
      begin
         Name := 'BOOKMARKJUMP';
         Help := 'Jumps to previously set bookmark';
         Syntax := '0..9';
         ArgType := argInteger;
         DefArg := '';
       end;
    end;
  end;






Finalization

end.
