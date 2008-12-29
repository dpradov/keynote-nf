//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include <stdlib.h>
#include "Demo1.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TDemoForm *DemoForm;
//---------------------------------------------------------------------------
__fastcall TDemoForm::TDemoForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::FormCreate(TObject *Sender)
{
    Memo->WordWrap = True;

    // Use the SetSlaveControl method of a TToolbar97 to configure a separate
    // top/bottom docked and left/right docked version of a control.
    // Please see the Toolbar97 documentation for more info on slave controls.

    // The line below tells it that FontCombo is the top/bottom docked version,
    // and FontButton is the left/right docked version.
    EditToolbar->SetSlaveControl (FontCombo, FontButton);
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::FExitClick(TObject *Sender)
{
    Close();
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::ToolbarPopupMenuPopup(TObject *Sender)
{
    TPMain->Checked = MainToolbar->Visible;
    TPEdit->Checked = EditToolbar->Visible;
    TPSample->Checked = SampleToolbar->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::VMenuClick(TObject *Sender)
{
    VStatusBar->Checked = StatusBar->Visible;
    VTMain->Checked = MainToolbar->Visible;
    VTEdit->Checked = EditToolbar->Visible;
    VTSample->Checked = SampleToolbar->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::VTMainClick(TObject *Sender)
{
    MainToolbar->Visible = !MainToolbar->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::VTEditClick(TObject *Sender)
{
    EditToolbar->Visible = !EditToolbar->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::VTSampleClick(TObject *Sender)
{
    SampleToolbar->Visible = !SampleToolbar->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::VStatusBarClick(TObject *Sender)
{
    // Force the StatusBar to always be at the bottom of the form. Without this
    // line of code, the status bar sometimes may appear above the bottom dock.
    // This is not a bug in Toolbar97, but rather is due to the design of the
    // VCL's alignment system.
    StatusBar->Top = ClientHeight;

    // Toggle the status bar's visibility
    StatusBar->Visible = !StatusBar->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::FontButtonClick(TObject *Sender)
{
    ShowMessage ("A font dialog could come up here.");
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::ToolWinButtonClick(TObject *Sender)
{
    SampleToolWindow->Visible = ToolWinButton->Down;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::SampleToolWindowVisibleChanged(TObject *Sender)
{
    ToolWinButton->Down = SampleToolWindow->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::ListBoxClick(TObject *Sender)
{
    DeleteButton->Enabled = ListBox->ItemIndex != -1;
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::AddButtonClick(TObject *Sender)
{
    ListBox->Items->Add (IntToStr(rand()));
}
//---------------------------------------------------------------------------
void __fastcall TDemoForm::DeleteButtonClick(TObject *Sender)
{
    int SaveItemIndex = ListBox->ItemIndex;
    ListBox->Items->Delete (ListBox->ItemIndex);
    ListBox->ItemIndex = SaveItemIndex;
    DeleteButton->Enabled = ListBox->ItemIndex != -1;
}
//---------------------------------------------------------------------------
