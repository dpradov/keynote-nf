//---------------------------------------------------------------------------
#ifndef Demo1H
#define Demo1H
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ComCtrls.hpp>
#include <vcl\ExtCtrls.hpp>
#include <vcl\Menus.hpp>
//---------------------------------------------------------------------------
class TDemoForm : public TForm
{
__published:	// IDE-managed Components
        TMemo *Memo;
        TPopupMenu *ToolbarPopupMenu;
        TDock97 *TopDock;
        TMainMenu *MainMenu;
        TMenuItem *FMenu;
        TMenuItem *VMenu;
        TToolbar97 *EditToolbar;
        TDock97 *BottomDock;
        TStatusBar *StatusBar;
        TToolbarButton97 *LeftButton;
        TToolbarButton97 *CenterButton;
        TToolbarButton97 *RightButton;
        TComboBox *FontCombo;
        TMenuItem *VToolbars;
        TMenuItem *VTMain;
        TMenuItem *VTEdit;
        TMenuItem *TPMain;
        TMenuItem *TPEdit;
        TMenuItem *FExit;
        TDock97 *LeftDock;
        TDock97 *RightDock;
        TToolbar97 *MainToolbar;
        TToolbarButton97 *NewButton;
        TToolbarButton97 *OpenButton;
        TToolbarButton97 *SaveButton;
        TToolbarButton97 *PrintButton;
        TToolbarButton97 *PrintPreviewButton;
        TToolbarButton97 *CutButton;
        TToolbarButton97 *CopyButton;
        TToolbarButton97 *PasteButton;
        TToolbarButton97 *FontButton;
        TMenuItem *VStatusBar;
        TToolbarSep97 *MainSep1;
        TToolbarSep97 *MainSep2;
        TToolbarSep97 *EditSep1;
        TToolbar97 *SampleToolbar;
        TEdit97 *SampleEdit1;
        TEdit97 *SampleEdit2;
        TMenuItem *TPSample;
        TMenuItem *VTSample;
        TToolbarSep97 *SampleSep1;
        TToolbarButton97 *DropdownButton;
        TPopupMenu *DropPopupMenu;
        TMenuItem *Sample1;
        TMenuItem *dropdown1;
        TMenuItem *menu1;
        TToolbarButton97 *UndoButton;
        TToolbarSep97 *MainSep3;
        TToolbarButton97 *RedoButton;
        TToolWindow97 *SampleToolWindow;
        TPanel *Panel1;
        TButton *AddButton;
        TButton *DeleteButton;
        TListBox *ListBox;
        TToolbarSep97 *SampleSep2;
        TToolbarButton97 *ToolWinButton;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall FExitClick(TObject *Sender);
        void __fastcall ToolbarPopupMenuPopup(TObject *Sender);
        void __fastcall VMenuClick(TObject *Sender);
        void __fastcall VTMainClick(TObject *Sender);
        void __fastcall VTEditClick(TObject *Sender);

        void __fastcall VTSampleClick(TObject *Sender);
        void __fastcall VStatusBarClick(TObject *Sender);
        void __fastcall FontButtonClick(TObject *Sender);
        void __fastcall ToolWinButtonClick(TObject *Sender);
        void __fastcall SampleToolWindowVisibleChanged(TObject *Sender);
        void __fastcall ListBoxClick(TObject *Sender);
        void __fastcall AddButtonClick(TObject *Sender);
        void __fastcall DeleteButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TDemoForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TDemoForm *DemoForm;
//---------------------------------------------------------------------------
#endif
