unit kn_Msgs;

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

const
  KeyNote_WinMsgIDStr = 'KeyNote1_WinMsgIdStr';

  // Internal KeyNote messages (do NOT use in plugins!!)
  KNT_MSG_SHOW                  = 50; // just restore window
  KNT_MSG_SHOW_AND_LOAD_FILE    = 51; // restore window and open .KNT file
  KNT_MSG_SHOW_AND_EXECUTE_FILE = 52; // restore window and run macro, plugin, etc.
  KNT_MSG_SHOW_LOCATION         = 53; // Jump to the location indicated

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
