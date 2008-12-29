unit Demo1;

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls,
  Forms, StdCtrls, ExtCtrls, ComCtrls, Menus, Dialogs,
  TB97, TB97Tlbr, TB97Tlwn, TB97Ctls;

type
  TDemoForm = class(TForm)
    Memo: TMemo;
    ToolbarPopupMenu: TPopupMenu;
    TopDock: TDock97;
    MainMenu: TMainMenu;
    FMenu: TMenuItem;
    VMenu: TMenuItem;
    EditToolbar: TToolbar97;
    BottomDock: TDock97;
    StatusBar: TStatusBar;
    LeftButton: TToolbarButton97;
    CenterButton: TToolbarButton97;
    RightButton: TToolbarButton97;
    FontCombo: TComboBox;
    VToolbars: TMenuItem;
    VTMain: TMenuItem;
    VTEdit: TMenuItem;
    TPMain: TMenuItem;
    TPEdit: TMenuItem;
    FExit: TMenuItem;
    LeftDock: TDock97;
    RightDock: TDock97;
    MainToolbar: TToolbar97;
    NewButton: TToolbarButton97;
    OpenButton: TToolbarButton97;
    SaveButton: TToolbarButton97;
    PrintButton: TToolbarButton97;
    PrintPreviewButton: TToolbarButton97;
    CutButton: TToolbarButton97;
    CopyButton: TToolbarButton97;
    PasteButton: TToolbarButton97;
    FontButton: TToolbarButton97;
    VStatusBar: TMenuItem;
    MainSep1: TToolbarSep97;
    MainSep2: TToolbarSep97;
    EditSep1: TToolbarSep97;
    SampleToolbar: TToolbar97;
    SampleEdit1: TEdit97;
    SampleEdit2: TEdit97;
    TPSample: TMenuItem;
    VTSample: TMenuItem;
    SampleSep1: TToolbarSep97;
    DropdownButton: TToolbarButton97;
    DropPopupMenu: TPopupMenu;
    Sample1: TMenuItem;
    dropdown1: TMenuItem;
    menu1: TMenuItem;
    UndoButton: TToolbarButton97;
    MainSep3: TToolbarSep97;
    RedoButton: TToolbarButton97;
    SampleToolWindow: TToolWindow97;
    Panel1: TPanel;
    AddButton: TButton;
    DeleteButton: TButton;
    ListBox: TListBox;
    SampleSep2: TToolbarSep97;
    ToolWinButton: TToolbarButton97;
    procedure FExitClick(Sender: TObject);
    procedure VTMainClick(Sender: TObject);
    procedure VTEditClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FontButtonClick(Sender: TObject);
    procedure VStatusBarClick(Sender: TObject);
    procedure VMenuClick(Sender: TObject);
    procedure VTSampleClick(Sender: TObject);
    procedure ToolbarPopupMenuPopup(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure SampleToolWindowVisibleChanged(Sender: TObject);
    procedure ToolWinButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DemoForm: TDemoForm;

implementation

{$R *.DFM}

procedure TDemoForm.FormCreate(Sender: TObject);
begin
  Memo.WordWrap := True;

  { Use the SetSlaveControl method of a TToolbar97 to configure a separate
    top/bottom docked and left/right docked version of a control.
    Please see the Toolbar97 documentation for more info on slave controls.

    The line below tells it that FontCombo is the top/bottom docked version,
    and FontButton is the left/right docked version. }
  EditToolbar.SetSlaveControl (FontCombo, FontButton);
end;

procedure TDemoForm.FExitClick(Sender: TObject);
begin
  Close;
end;

procedure TDemoForm.ToolbarPopupMenuPopup(Sender: TObject);
begin
  TPMain.Checked := MainToolbar.Visible;
  TPEdit.Checked := EditToolbar.Visible;
  TPSample.Checked := SampleToolbar.Visible;
end;

procedure TDemoForm.VMenuClick(Sender: TObject);
begin
  VStatusBar.Checked := StatusBar.Visible;
  VTMain.Checked := MainToolbar.Visible;
  VTEdit.Checked := EditToolbar.Visible;
  VTSample.Checked := SampleToolbar.Visible;
end;

procedure TDemoForm.VTMainClick(Sender: TObject);
begin
  MainToolbar.Visible := not MainToolbar.Visible;
end;
procedure TDemoForm.VTEditClick(Sender: TObject);
begin
  EditToolbar.Visible := not EditToolbar.Visible;
end;
procedure TDemoForm.VTSampleClick(Sender: TObject);
begin
  SampleToolbar.Visible := not SampleToolbar.Visible;
end;

procedure TDemoForm.VStatusBarClick(Sender: TObject);
begin
  { Force the StatusBar to always be at the bottom of the form. Without this
    line of code, the status bar sometimes may appear above the bottom dock.
    This is not a bug in Toolbar97, but rather is due to the design of the
    VCL's alignment system. }
  StatusBar.Top := ClientHeight;

  { Toggle the status bar's visibility }
  StatusBar.Visible := not StatusBar.Visible;
end;

procedure TDemoForm.FontButtonClick(Sender: TObject);
begin
  ShowMessage ('A font dialog could come up here.');
end;

procedure TDemoForm.ToolWinButtonClick(Sender: TObject);
begin
  SampleToolWindow.Visible := ToolWinButton.Down;
end;

procedure TDemoForm.SampleToolWindowVisibleChanged(Sender: TObject);
begin
  ToolWinButton.Down := SampleToolWindow.Visible;
end;

procedure TDemoForm.ListBoxClick(Sender: TObject);
begin
  DeleteButton.Enabled := ListBox.ItemIndex <> -1;
end;

procedure TDemoForm.AddButtonClick(Sender: TObject);
begin
  ListBox.Items.Add (IntToStr(Random(10000)));
end;

procedure TDemoForm.DeleteButtonClick(Sender: TObject);
var
  SaveItemIndex: Integer;
begin
  SaveItemIndex := ListBox.ItemIndex;
  ListBox.Items.Delete (ListBox.ItemIndex);
  ListBox.ItemIndex := SaveItemIndex;
  DeleteButton.Enabled := ListBox.ItemIndex <> -1;
end;

end.
