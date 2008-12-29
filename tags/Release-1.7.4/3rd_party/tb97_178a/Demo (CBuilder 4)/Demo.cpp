//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("Demo.res");
USEFORM("Demo1.cpp", DemoForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TDemoForm), &DemoForm);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
//---------------------------------------------------------------------------

