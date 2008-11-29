//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("TB97_cb5.res");
USEUNIT("TB97.pas");
USEUNIT("TB97Tlbr.pas");
USEUNIT("TB97Tlwn.pas");
USEUNIT("TB97Ctls.pas");
USEUNIT("TB97Cmn.pas");
USEUNIT("TB97Cnst.pas");
USEUNIT("TB97Vers.pas");
USEUNIT("TB97Reg.pas");
USERES("TB97Reg.dcr");
USEPACKAGE("vcl50.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------

