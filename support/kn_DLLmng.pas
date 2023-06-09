unit kn_DLLmng;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 

interface
uses
   Winapi.Windows,
   System.SysUtils,
   Vcl.Forms,
   Vcl.Dialogs,
   kn_Info,
   kn_Const,
   kn_DLLInterface;


{ KNTUTIL.DLL access routines }
function GetMethodInDLL(var DLLHandle: THandle; ProcName: string): Pointer;


implementation

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
