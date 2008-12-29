{.$DEFINE DFS_DEBUG}

{ Interface to the DFSKbMon.DLL library. }
{ Intended for use with the TDFSStatusBar component (DFSStatusBar.pas) }

unit DFSKb;

interface

uses
  {$IFDEF DFS_DEBUG}
  DFSDebug,
  {$ENDIF}
  Windows;

{
  DFSKbDLLName contains the full filepath to the DLL to be loaded. It defaults
     to just 'DFSKbMon.dll' so that the path is searched.  Change before calling
     InitDFSKbDLL if you want to specify a location.
  DFSKbDLL_Loaded indicates whether the DLL was loaded or not.
}
var
  DFSKbDLLName: string;
  DFSKbDLL_Loaded: boolean;

{ Call before anything else to load the DLL and set up everything. }
procedure InitDFSKbDLL;
{ Call if you want to manually unload the DLL.  Don't normally need since it
  will do it automatically at app exit. }
procedure UnloadDFSKbDLL;
{ Ask to be notified of Caps, Num, Scroll lock changes.  Return value is the
  window message that will be sent to notify of change, or 0 if failed. }
function DLLRegisterKeyboardHook(Handle: HWND): UINT;
{ Remove from notification list. }
procedure DLLDeregisterKeyboardHook(Handle: HWND);

implementation

uses
  Classes, SysUtils;

var
  DLLRegisterKeyboardHookPtr: function (Handle: HWND): UINT; stdcall;
  DLLDeregisterKeyboardHookPtr: procedure (Handle: HWND); stdcall;
  hDFSKbDLL: THandle; { DLL handle }
  RegisteredClients: TList;

{ Load the DLL and get all the procedure addresses. }
function LoadDFSKbDLL: boolean;
var
  OldMode: UINT;
begin
  {$IFDEF DFS_DEBUG}
  DFSDebug.Log('DFSKb: LoadDFSKbDLL start', TRUE);
  {$ENDIF}
  if hDFSKbDLL <> 0 then
    FreeLibrary(hDFSKbDLL);
  OldMode := SetErrorMode(SEM_NOOPENFILEERRORBOX); { No system messages if can't load. }
  hDFSKbDLL := LoadLibrary(PChar(DFSKbDLLName));
  Result := hDFSKbDLL <> 0;
  SetErrorMode(OldMode);
  if not Result then exit;

  { Get all the function addresses }
  @DLLRegisterKeyboardHookPtr := GetProcAddress(hDFSKbDLL, 'RegisterKeyboardHook');
  @DLLDeregisterKeyboardHookPtr := GetProcAddress(hDFSKbDLL, 'DeregisterKeyboardHook');
  {$IFDEF DFS_DEBUG}
  DFSDebug.Log('DFSKb: LoadDFSKbDLL end', TRUE);
  {$ENDIF}
end;

{ Procedure called when unit is finished, i.e. app exiting. }
procedure CleanupDLL;
var
  x: integer;
begin
  {$IFDEF DFS_DEBUG}
  DFSDebug.Log('DFSKb: CleanupDLL start', TRUE);
  {$ENDIF}
  if hDFSKbDLL <> 0 then
  begin
    // Paranoia check
    if RegisteredClients.Count > 0 then
    begin
      {$IFDEF DFS_DEBUG}
      DFSDebug.Log('DFSKb: Paranoia failed', TRUE);
      {$ENDIF}
      for x := RegisteredClients.Count-1 downto 0 do
        DLLDeregisterKeyboardHook(HWND(RegisteredClients[x]));
    end;

    {$IFDEF DFS_DEBUG}
    DFSDebug.Log('DFSKb: attempting FreeLibrary', TRUE);
    {$ENDIF}
    if FreeLibrary(hDFSKbDLL) then
    begin
      {$IFDEF DFS_DEBUG}
      DFSDebug.Log('DFSKb: unloaded DLL', TRUE);
      {$ENDIF}
      hDFSKbDLL := 0;
      DFSKbDLL_Loaded := FALSE;
    end;
  end;
  {$IFDEF DFS_DEBUG}
  DFSDebug.Log('DFSKb: CleanupDLL end', TRUE);
  {$ENDIF}
end;

procedure InitDFSKbDLL;
begin
  DFSKbDLL_Loaded := LoadDFSKbDLL;
end;

procedure UnloadDFSKbDLL;
begin
  CleanupDLL;
end;

function DLLRegisterKeyboardHook(Handle: HWND): UINT;
begin
  if @DLLRegisterKeyboardHookPtr <> NIL then
  begin
    RegisteredClients.Add(Pointer(Handle));
    Result := DLLRegisterKeyboardHookPtr(Handle);
  end else
    Result := 0;
end;

procedure DLLDeregisterKeyboardHook(Handle: HWND);
begin
  if @DLLDeregisterKeyboardHookPtr <> NIL then
  begin
    RegisteredClients.Remove(Pointer(Handle));
    DLLDeregisterKeyboardHookPtr(Handle);
  end;
end;


initialization
  {$IFDEF DFS_DEBUG}
  DFSDebug.Log('DFSKb: init begin', TRUE);
  {$ENDIF}
  RegisteredClients := TList.Create;
  DFSKbDLLName := 'DFSKbMon.dll';
  hDFSKbDLL := 0;
  {$IFDEF DFS_DEBUG}
  DFSDebug.Log('DFSKb: init end.', TRUE);
  {$ENDIF}

finalization
  {$IFDEF DFS_DEBUG}
  DFSDebug.Log('DFSKb: finalization begin.', TRUE);
  {$ENDIF}
  CleanupDLL;
  RegisteredClients.Free;
  {$IFDEF DFS_DEBUG}
  DFSDebug.Log('DFSKb: finalization end.', TRUE);
  {$ENDIF}

end.


