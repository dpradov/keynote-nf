{.$DEFINE DFS_DEBUG}

{ The DFSKbMon.DLL library. }

{ Intended for use with the TDFSStatusBar component (DFSStatusBar.pas), and
  intended to be accessed by the DFSKb unit. }

{ This DLL provides a notification mechanism for system wide monitoring of the
  Caps, Num & Scroll lock keys.  Interested parties are notified via a custom
  windows message as returned by the RegisterKeyboardHook function. }
  
library DFSKbMon;

uses
  {$IFDEF DFS_DEBUG}
  Debug, SysUtils,
  {$ENDIF}
  Windows;

const
  MAX_CLIENTS = 256;

type
  PHookData = ^THookData;
  THookData = record
    KeyboardHookHandle: HHOOK;
    UsageCount: integer;
    ClientIndex: integer;
    Clients: array[0..MAX_CLIENTS-1] of HWND;
  end;

var
  MapFile: THandle;
  WM_INDICATORKEY: UINT;
  HookData: PHookData;


procedure MapFileMemory;
var
  ZeroMem: boolean;
begin
  MapFile := CreateFileMapping($FFFFFFFF, NIL, PAGE_READWRITE, 0,
     SizeOf(THookData), 'DFSKbMon Client List');
  if (MapFile = 0) then
  begin
    MessageBox(0, 'DFSKbMon DLL', 'Could not create file map object', MB_OK);
  end else begin
    ZeroMem := GetLastError <> ERROR_ALREADY_EXISTS;
    HookData := MapViewOfFile(MapFile, FILE_MAP_ALL_ACCESS, 0, 0,
       SizeOf(THookData));
    if (HookData = NIL) then
    begin
      CloseHandle(MapFile);
      MessageBox(0, 'DFSKbMon DLL', 'Could not map file', MB_OK);
    end else
      if ZeroMem then
        FillChar(HookData^, SizeOf(THookData), #0);
  end;
end;

procedure UnmapFileMemory;
begin
  if (HookData <> NIL) then
  begin
    UnMapViewOfFile(HookData);
    HookData := NIL;
  end;
  if (MapFile <> 0) then
  begin
    CloseHandle(MapFile);
    MapFile := 0;
  end;
end;

procedure AddClient(Wnd: HWND);
begin
  if (HookData <> NIL) then
  begin
    {$IFDEF DFS_DEBUG}
    Debug.Log(TRUE, 'DFSKbMon: Added ' + IntToHex(Wnd, 8) + ' at index ' +
       IntToStr(HookData^.ClientIndex));
    {$ENDIF}
    HookData^.Clients[HookData^.ClientIndex] := Wnd;
    inc(HookData^.ClientIndex);
  end;
end;

procedure RemoveClient(Wnd: HWND);
var
  x: integer;
begin
  if (HookData <> NIL) then
  begin
    {$IFDEF DFS_DEBUG}
    Debug.Log(TRUE, 'DFSKbMon: Trying to remove ' + IntToHex(Wnd, 8));
    {$ENDIF}
    for x := 0 to HookData^.ClientIndex-1 do
    begin
      if HookData^.Clients[x] = Wnd then
      begin
        {$IFDEF DFS_DEBUG}
        Debug.Log(TRUE, '   Found at index ' + IntToStr(HookData^.ClientIndex));
        {$ENDIF}
        if x < HookData^.ClientIndex-1 then
          Move(HookData^.Clients[x+1], HookData^.Clients[x],
             (HookData^.ClientIndex - x - 1) * SizeOf(HWND));
        dec(HookData^.ClientIndex);
        break;
      end;
    end;
  end;
end;



// Keyboard hook callback
function KeyboardHookCallBack(Code: integer; KeyCode: WPARAM;
   KeyInfo: LPARAM): LRESULT; stdcall;
var
  x: integer;
  State: ShortInt;
begin
  if (Code >= 0) and (HookData <> NIL) then
  begin
    // Is it one of the indicator keys, and is it not a repeat
    if ((KeyCode = VK_CAPITAL) or (KeyCode = VK_NUMLOCK) or
       (KeyCode = VK_SCROLL)) and
       // This checks to see if the key is being pressed (bit 31) and if it was
       // up before (bit 30).  We don't care about key releases or keys that
       // were already down.  That just makes us flicker...
       (((KeyInfo SHR 31) and 1) = 0) {and (((KeyInfo SHR 30) and 1) = 0)} then
    begin
      State := GetKeyState(KeyCode);
      for x := 0 to HookData^.ClientIndex-1 do
        PostMessage(HookData^.Clients[x], WM_INDICATORKEY, KeyCode, State);
    end;
  end;

  Result := CallNextHookEx(HookData^.KeyboardHookHandle, Code, KeyCode, KeyInfo);
end;

// Utility routins for installing the windows hook for keypresses
function RegisterKeyboardHook(Handle: HWND): UINT; stdcall;
  { This is really silly, but that's the way it goes.  The only way to get the }
  { module handle, *not* instance, is from the filename.  The Microsoft example}
  { just hard-codes the DLL filename.  I think this is a little bit better.    }
  function GetModuleHandleFromInstance: THandle;
  var
    s: array[0..512] of char;
  begin
    { Find the DLL filename from the instance value. }
    GetModuleFileName(hInstance, s, sizeof(s)-1);
    { Find the handle from the filename. }
    Result := GetModuleHandle(s);
  end;
begin
  if (HookData <> NIL) then
  begin
    if HookData^.KeyboardHookHandle = 0 then
      { See the Microsoft KnowledgeBase, PSS ID Number: Q92659, for a }
      { discussion of the Windows bug that requires GetModuleHandle to be used.}
      HookData^.KeyboardHookHandle := SetWindowsHookEx(WH_KEYBOARD,
         KeyboardHookCallBack, GetModuleHandleFromInstance, 0);

    inc(HookData^.UsageCount);
    AddClient(Handle);

    Result := WM_INDICATORKEY;
  end else
    Result := 0;
end;

procedure DeregisterKeyboardHook(Handle: HWND); stdcall;
begin
  if (HookData <> NIL) then
  begin
    dec(HookData^.UsageCount);
    RemoveClient(Handle);
    if HookData^.UsageCount < 1 then
    begin
      UnhookWindowsHookEx(HookData^.KeyboardHookHandle);
      HookData^.KeyboardHookHandle := 0;
    end;
  end;
end;

procedure LibraryProc(Reason: Integer);
begin
  case Reason of
    DLL_PROCESS_ATTACH:
    begin
      {$IFDEF DFS_DEBUG}
      Debug.Log(TRUE, 'DFSKbMon: loaded.');
      {$ENDIF}
      MapFile := 0;
      HookData := NIL;
      MapFileMemory;
    end;
    DLL_PROCESS_DETACH:
    begin
      UnmapFileMemory;
      {$IFDEF DFS_DEBUG}
      Debug.Log(TRUE, 'DFSKbMon: unloaded.');
      {$ENDIF}
    end;
  end;
end;

exports
  RegisterKeyboardHook,
  DeregisterKeyboardHook;


begin
  WM_INDICATORKEY := RegisterWindowMessage('DFSKbMon.WM_INDICATORKEY');
  DLLProc := @LibraryProc;
  LibraryProc(DLL_PROCESS_ATTACH);
end.
