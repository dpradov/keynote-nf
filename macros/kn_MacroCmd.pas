unit kn_MacroCmd;

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
uses
  knt.RS;

var
  mcmd : TMacroCmd;

Initialization


  for mcmd := low( TMacroCmd ) to high( TMacroCmd ) do
  begin
    case mcmd of
      macInsert : with MACRO_DEFS[mcmd] do
      begin
         Name := 'INSERT';
         Help :=  sMacC10;
         Syntax := sMacC01;
         ArgType := argString;
         DefArg := '';
       end;
      macWait : with MACRO_DEFS[mcmd] do
      begin
         Name := 'WAIT';
         Help := sMacC11;
         Syntax := sMacCms;
         ArgType := argInteger;
         DefArg := '250';
       end;
      macRewind : with MACRO_DEFS[mcmd] do
      begin
         Name := 'REWIND';
         Help := sMacC12;
         Syntax := '';
         ArgType := argNone;
         DefArg := '';
       end;
      macMessage : with MACRO_DEFS[mcmd] do
      begin
         Name := 'MESSAGE';
         Help := sMacC13;
         Syntax := sMacC01;
         ArgType := argString;
         DefArg := '';
       end;
      macStatus : with MACRO_DEFS[mcmd] do
      begin
         Name := 'STATUS';
         Help := sMacC14;
         Syntax := sMacC01;
         ArgType := argString;
         DefArg := '';
       end;
      macPlugin : with MACRO_DEFS[mcmd] do
      begin
         Name := 'PLUGIN';
         Help := sMacC15;
         Syntax := sMacCfn;
         ArgType := argString;
         DefArg := '';
       end;
      macMacro : with MACRO_DEFS[mcmd] do
      begin
         Name := 'MACRO';
         Help := sMacC16;
         Syntax := sMacCfn;
         ArgType := argString;
         DefArg := '';
       end;
      macConfirm : with MACRO_DEFS[mcmd] do
      begin
         Name := 'CONFIRM';
         Help := sMacC17;
         Syntax := sMacC01;
         ArgType := argString;
         DefArg := '';
       end;
      macStyleOn : with MACRO_DEFS[mcmd] do
      begin
         Name := 'STYLEON';
         Help := sMacC18;
         Syntax := STR_style;
         ArgType := argString;
         DefArg := '';
       end;
      macStyleOff : with MACRO_DEFS[mcmd] do
      begin
         Name := 'STYLEOFF';
         Help := sMacC19;
         Syntax := STR_style;
         ArgType := argString;
         DefArg := '';
       end;
      macStyleFlip : with MACRO_DEFS[mcmd] do
      begin
         Name := 'STYLEFLIP';
         Help := sMacC20;
         Syntax := STR_style;
         ArgType := argString;
         DefArg := '';
       end;
      macGoLeft : with MACRO_DEFS[mcmd] do
      begin
         Name := 'GOLEFT';
         Help := sMacC21;
         Syntax := sMacCint;
         ArgType := argInteger;
         DefArg := '1';
       end;
      macGoRight : with MACRO_DEFS[mcmd] do
      begin
         Name := 'GORIGHT';
         Help := sMacC22;
         Syntax := sMacCint;
         ArgType := argInteger;
         DefArg := '1';
       end;
      macGoDown : with MACRO_DEFS[mcmd] do
      begin
         Name := 'GODOWN';
         Help := sMacC23;
         Syntax := sMacCint;
         ArgType := argInteger;
         DefArg := '1';
       end;
      macGoUp : with MACRO_DEFS[mcmd] do
      begin
         Name := 'GOUP';
         Help := sMacC24;
         Syntax := sMacCint;
         ArgType := argInteger;
         DefArg := '1';
       end;
      macSelectAll : with MACRO_DEFS[mcmd] do
      begin
         Name := 'SELECTALL';
         Help := sMacC25;
         Syntax := '';
         ArgType := argNone;
         DefArg := '';
       end;
      macFontColor : with MACRO_DEFS[mcmd] do
      begin
         Name := 'FONTCOLOR';
         Help := sMacC26;
         Syntax := sMacCcn;
         ArgType := argString;
         DefArg := '';
       end;
      macBGColor : with MACRO_DEFS[mcmd] do
      begin
         Name := 'BGCOLOR';
         Help := sMacC27;
         Syntax := sMacCcn;
         ArgType := argString;
         DefArg := '';
       end;
      macHighlightColor : with MACRO_DEFS[mcmd] do
      begin
         Name := 'HICOLOR';
         Help := sMacC28;
         Syntax := sMacCcn;
         ArgType := argString;
         DefArg := '';
       end;
      macNoteNewRTF : with MACRO_DEFS[mcmd] do
      begin
         Name := 'NOTENEWRTF';
         Help := sMacC29;
         Syntax := '';
         ArgType := argNone;
         DefArg := '';
       end;
      macNoteNewTree : with MACRO_DEFS[mcmd] do
      begin
         Name := 'NOTENEWTREE';
         Help := sMacC30;
         Syntax := '';
         ArgType := argNone;
         DefArg := '';
       end;
      macApplyStyle : with MACRO_DEFS[mcmd] do
      begin
         Name := 'APPLYSTYLE';
         Help := sMacC31;
         Syntax := sMacCstN;
         ArgType := argString;
         DefArg := '';
       end;
      macBookmarkSet : with MACRO_DEFS[mcmd] do
      begin
         Name := 'BOOKMARKSET';
         Help := sMacC32;
         Syntax := '0..9';
         ArgType := argInteger;
         DefArg := '';
       end;
      macBookmarkJump : with MACRO_DEFS[mcmd] do
      begin
         Name := 'BOOKMARKJUMP';
         Help := sMacC33;
         Syntax := '0..9';
         ArgType := argInteger;
         DefArg := '';
       end;
    end;
  end;






Finalization

end.
