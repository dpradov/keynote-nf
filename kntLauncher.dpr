program kntLauncher;

//{.$APPTYPE CONSOLE}
{$APPTYPE GUI}

{$R *.res}

uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.Messages,
  kn_Msgs;

var
  _OTHER_INSTANCE_HANDLE : hwnd = 0;

type
  PFindWindowInfo = ^TFindWindowInfo;
  TFindWindowInfo = record
    WindowHandle: HWND;
  end;

 function CallInstance: integer; forward;


   function EnumWindowsProc(hWnd: HWND; Info: PFindWindowInfo): BOOL; stdcall;
   var
     ClassName: array[0..255] of Char;
     ResultInstance: integer;
   begin
     Result := True;

     if GetClassName(hWnd, ClassName, SizeOf(ClassName)) > 0 then begin
       if (StrComp(ClassName, UniqueAppName_KEYNOTE10) = 0) or (StrComp(ClassName, UniqueAppName_KEYNOTE10_dnd) = 0) then begin
          _OTHER_INSTANCE_HANDLE:= hWnd;
          if CallInstance = 0 then begin
             Info^.WindowHandle := hWnd;
             Result:= False;
             //ShowWindow(hWnd, SW_RESTORE);      // See comment to TForm_Main.ExecuteCallArgs, in keynote project
             sleep(100);
             SetForegroundWindow(hWnd);
          end;
       end;
     end;
   end;


   function ActivateAValidKNTInstance: boolean;
   var
     Info: TFindWindowInfo;
   begin
      Info.WindowHandle := 0;
      EnumWindows(@EnumWindowsProc, LPARAM(@Info));
      Result:= (Info.WindowHandle <> 0);
   end;

   function CallInstance: integer;
   var
     CopyData : TCopyDataStruct;
     Args: string;
   begin
     Args:= GetCommandLine;
     copydata.dwData := KNT_MSG_LAUNCHER_CALL;
     copydata.cbData:= ByteLength(Args)+1;
     copydata.lpData := PChar(Args);

     Result:= SendMessage( _OTHER_INSTANCE_HANDLE, WM_COPYDATA, 0, integer( @copydata ));
   end;

   procedure LaunchNewKNTInstance();
   var
     StartupInfo: TStartupInfo;
     ProcessInfo: TProcessInformation;
     kntPath: string;
     commandLine: string;
   begin
     FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
     StartupInfo.cb := SizeOf(TStartupInfo);
     StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
     StartupInfo.wShowWindow := SW_HIDE;

     commandLine:= StringReplace(GetCommandLine, 'kntLauncher', 'keynote', []) + ' -ignSI -dnd';
     if CreateProcess(nil, PChar(CommandLine), nil, nil, False, 0, nil, nil, StartupInfo, ProcessInfo) then begin
        CloseHandle(ProcessInfo.hProcess);
        CloseHandle(ProcessInfo.hThread);
     end
     else
        MessageBox(0, PChar('Error launching: ' + #13#13 + commandLine), 'KntLauncher', MB_ICONWARNING or MB_OK);
   end;



begin
  try
    if ParamCount >= 1 then begin
      if not ActivateAValidKNTInstance() then
         LaunchNewKNTInstance();
    end
    else
       MessageBox(0, 'Usage:'+#13#13 +'kntLauncher <kenote args>' + #13 +
                      '(ex: kntLauncher myHelp.knt -jmp"file:///*3|16|5|0|1")', 'KntLauncher', MB_ICONINFORMATION or MB_OK);

  except
    on E: Exception do
       MessageBox(0, PChar('Error: ' + E.Message), 'KntLauncher', MB_ICONWARNING or MB_OK);

  end;


end.

