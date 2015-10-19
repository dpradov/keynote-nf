unit kn_Msgs;

interface

const
  KeyNote_WinMsgIDStr = 'KeyNote1_WinMsgIdStr';

  // Internal KeyNote messages (do NOT use in plugins!!)
  KNT_MSG_SHOW               = 50;
  KNT_MSG_SHOW_AND_LOAD_FILE = 51;

  // Message IDs for resident plugins
  KNT_MSG_PERFORMKEY         = 100; // pass a key to TRichEdit
  KNT_MSG_INSERTTEXT         = 101; // insert plain text in active note
  KNT_MSG_INSERTRTFTEXT      = 102; // not implemented (0.999)
  KNT_MSG_MOVECARET          = 103; // move caret (use direction constants, below)

  KNT_MSG_PLUGIN_SHUTDOWN    = 1000; // notify KeyNote that plugin is about to shut down

const
  // caret motion direction constants
  _CARET_RIGHT = 1;
  _CARET_LEFT  = 2;
  _CARET_UP    = 3;
  _CARET_DOWN  = 4;  

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
