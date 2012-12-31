unit kn_DLLmng;

interface
uses
  kn_Const;

{ KNTUTIL.DLL access routines }
function GetMethodInDLL(var DLLHandle: THandle; ProcName: string): Pointer;


implementation
uses
  Windows, Forms, SysUtils, Dialogs,
  kn_DLLInterface, kn_Info;

resourcestring
  STR_01 = 'Error while attempting to load runtime library "%s". Please reinstall KeyNote.';
  STR_02 = 'Procedure "%s" not found in runtime library "%s". Please reinstall KeyNote.';


function ObtainDLLHandle : THandle;
begin
  result := LoadLibrary( PChar( _KNTUtilsDLL_FN ));
  if ( result <= 0 ) then
  begin
    Application.MessageBox(
      PChar( Format(
        STR_01, [extractfilename( _KNTUtilsDLL_FN )] )),
        'Failed to load library', MB_OK+MB_ICONHAND+MB_DEFBUTTON1+MB_APPLMODAL);
    exit;
  end;
end; // ObtainDLLHandle

procedure DllProcNotFoundMsg( const ProcName : string );
begin
  Application.MessageBox(
    PChar( Format(
      STR_02, [ProcName, extractfilename( _KNTUtilsDLL_FN )] )),
      'Procedure not in library', MB_OK+MB_ICONHAND+MB_DEFBUTTON1+MB_APPLMODAL);
end; // DllProcNotFoundMsg


function GetMethodInDLL(var DLLHandle: THandle; ProcName: string): Pointer;
begin
    Result:= nil;

    if DLLHandle <= 0 then begin
       DllHandle := ObtainDLLHandle;
       if ( DllHandle <= 0 ) then exit;
    end;

    Result := GetProcAddress( DllHandle, PChar(ProcName));
    if ( not assigned( Result )) then begin
       DllProcNotFoundMsg(ProcName);
       exit;
    end;
end;

end.
