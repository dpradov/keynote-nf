
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

unit kn_Msgs;

interface

const
  KeyNote_WinMsgIDStr = 'KeyNote1_WinMsgIdStr';

  // Internal KeyNote messages (do NOT use in plugins!!)
  KNT_MSG_SHOW                  = 50; // just restore window
  KNT_MSG_SHOW_AND_LOAD_FILE    = 51; // restore window and open .KNT file
  KNT_MSG_SHOW_AND_EXECUTE_FILE = 52; // restore window and run macro, plugin, etc.

  // Message IDs for resident plugins
  KNT_MSG_PERFORMKEY         = 100; // pass a key to TRichEdit
  KNT_MSG_INSERTTEXT         = 101; // insert plain text in active note
  KNT_MSG_INSERTRTFTEXT      = 102; // not implemented (0.999)
  KNT_MSG_MOVECARET          = 103; // move caret (use direction constants, below)
  KNT_MSG_NOTEFROMTEXT       = 104; //
  KNT_MSG_NOTEFROMRTFTEXT    = 105; // 

  KNT_MSG_RELOADCONFIG       = 500;

  KNT_MSG_PLUGIN_SHUTDOWN    = 1000; // notify KeyNote that plugin is about to shut down

const
  // caret motion direction constants
  _CARET_RIGHT = 1;
  _CARET_LEFT  = 2;
  _CARET_UP    = 3;
  _CARET_DOWN  = 4;

  // config
  _CFG_RELOAD_MAIN    = 1; // not implemented
  _CFG_RELOAD_KEYS    = 2; // KeyNote will reload custom Function key assignments

type
  // structure for passing information with WM_COPYDATA message
  PKeyNoteMsg = ^TKeyNoteMsg;
  TKeyNoteMsg = record
    intData1 : longint;
    intData2 : longint;
    strData : string[255];
  end;

const
  NO_FILENAME_TO_LOAD = '?';
  

implementation

end.
