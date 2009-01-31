unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, DFSStatusBar, StdCtrls, Spin, ExtCtrls;

type
  TForm2 = class(TForm)
    DFSStatusBar1: TDFSStatusBar;
    DFSStatusBar2: TDFSStatusBar;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    cmbAlignment: TComboBox;
    chkEnabled: TCheckBox;
    chkAutoFit: TCheckBox;
    btnGlyph: TButton;
    Label2: TLabel;
    edtDateFormat: TEdit;
    edtTimeFormat: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edtText: TEdit;
    Label5: TLabel;
    cmbPanel: TComboBox;
    Label6: TLabel;
    spnGaugePosition: TSpinEdit;
    ScrollBox1: TScrollBox;
    imgGlyph: TImage;
    OpenDlg: TOpenDialog;
    Label7: TLabel;
    cmbBevel: TComboBox;
    Label8: TLabel;
    spnWidth: TSpinEdit;
    Label9: TLabel;
    cmbGaugeStyle: TComboBox;
    Label10: TLabel;
    edtHint: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure cmbPanelChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtTextChange(Sender: TObject);
    procedure cmbAlignmentChange(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure chkAutoFitClick(Sender: TObject);
    procedure edtDateFormatChange(Sender: TObject);
    procedure edtTimeFormatChange(Sender: TObject);
    procedure spnGaugePositionChange(Sender: TObject);
    procedure btnGlyphClick(Sender: TObject);
    procedure cmbBevelChange(Sender: TObject);
    procedure spnWidthChange(Sender: TObject);
    procedure DFSStatusBar2Panels6DrawPanel(StatusBar: TDFSStatusBar;
      Panel: TDFSStatusPanel; const Rect: TRect);
    procedure DFSStatusBar2Panels6HintText(StatusBar: TDFSStatusBar;
      Panel: TDFSStatusPanel; var Hint: String);
    procedure cmbGaugeStyleChange(Sender: TObject);
    procedure edtHintChange(Sender: TObject);
  private
    function GetCurStatusPanel: TDFSStatusPanel;
  private
    procedure PopulateControls;
    property CurPanel: TDFSStatusPanel
       read GetCurStatusPanel;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  TypInfo, Registry;

{$R *.DFM}

const
  P_NORMAL       = 0;
  P_DATE         = 1;
  P_TIME         = 2;
  P_DATETIME     = 3;
  P_ELLIPSISTEXT = 4;
  P_GLYPH        = 0;
  P_GAUGE        = 1;
  P_INDTR_GAUGE  = 2;
  P_CAPSLOCK     = 3;
  P_NUMLOCK      = 4;
  P_SCROLLLOCK   = 5;
  P_OWNERDRAW    = 6;
  P_ELLIPSISPATH = 7;


procedure TForm2.FormCreate(Sender: TObject);
var
  x: integer;
  Reg: TRegIniFile;
  gs: TDFSGaugeStyle;
begin
  for x := 0 to DFSStatusBar1.Panels.Count-1 do
    cmbPanel.Items.AddObject(GetEnumName(TypeInfo(TDFSStatusPanelType),
      ord(DFSStatusBar1.Panels[x].PanelType)), DFSStatusBar1.Panels[x]);
  for x := 0 to DFSStatusBar2.Panels.Count-1 do
    cmbPanel.Items.AddObject(GetEnumName(TypeInfo(TDFSStatusPanelType),
      ord(DFSStatusBar2.Panels[x].PanelType)), DFSStatusBar2.Panels[x]);
  for gs := Low(TDFSGaugeStyle) to High(TDFSGaugeStyle) do
    cmbGaugeStyle.Items.Add(GetEnumName(TypeInfo(TDFSGaugeStyle),
      ord(gs)));
  Reg := TRegIniFile.Create('');
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey('\Software\Borland\', FALSE);
    OpenDlg.InitialDir := Reg.ReadString('Borland Shared', 'SharedFilesDir', '');
    if OpenDlg.InitialDir <> '' then
      OpenDlg.InitialDir := OpenDlg.InitialDir + '\Images'
    else
      OpenDlg.InitialDir := ExtractFilePath(Application.EXEName);
  finally
    Reg.Free;
  end;
end;

procedure TForm2.cmbPanelChange(Sender: TObject);
begin
  PopulateControls;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  cmbPanel.ItemIndex := 0;
  cmbPanelChange(cmbPanel);
end;

function TForm2.GetCurStatusPanel: TDFSStatusPanel;
begin
  Result := TDFSStatusPanel(cmbPanel.Items.Objects[cmbPanel.ItemIndex]);
  if Result = NIL then
    raise Exception.Create('Panel combo box is toasted');  
end;

procedure TForm2.PopulateControls;
begin
  with CurPanel do
  begin
    edtText.Text := Text;
    case Alignment of
      taRightJustify: cmbAlignment.ItemIndex := 1;
      taCenter:       cmbAlignment.ItemIndex := 2;
    else
      cmbAlignment.ItemIndex := 0;
    end;
    case Bevel of
      pbRaised:  cmbBevel.ItemIndex := 1;
      pbNone: cmbBevel.ItemIndex := 2;
    else
      cmbBevel.ItemIndex := 0;
    end;
    chkEnabled.Checked := Enabled;
    chkAutoFit.Checked := AutoFit;
    edtDateFormat.Text := DateFormat;
    edtTimeFormat.Text := TimeFormat;
    spnGaugePosition.Value := GaugeAttrs.Position;
    cmbGaugeStyle.ItemIndex := ord(GaugeAttrs.Style);
    spnWidth.Value := Width;
    edtHint.Text := Hint;
    imgGlyph.Picture := Glyph;
  end;
end;

procedure TForm2.edtTextChange(Sender: TObject);
begin
  CurPanel.Text := edtText.Text;
end;

procedure TForm2.cmbAlignmentChange(Sender: TObject);
begin
  case cmbAlignment.ItemIndex of
    1: CurPanel.Alignment := taRightJustify;
    2: CurPanel.Alignment := taCenter;
  else
    CurPanel.Alignment := taLeftJustify;
  end;
end;

procedure TForm2.cmbBevelChange(Sender: TObject);
begin
  case cmbBevel.ItemIndex of
    1: CurPanel.Bevel := pbRaised;
    2: CurPanel.Bevel := pbNone;
  else
    CurPanel.Bevel := pbLowered;
  end;
end;

procedure TForm2.chkEnabledClick(Sender: TObject);
begin
  CurPanel.Enabled := chkEnabled.Checked;
end;

procedure TForm2.chkAutoFitClick(Sender: TObject);
begin
  CurPanel.AutoFit := chkAutoFit.Checked;
end;

procedure TForm2.edtDateFormatChange(Sender: TObject);
begin
  CurPanel.DateFormat := edtDateFormat.Text;
end;

procedure TForm2.edtTimeFormatChange(Sender: TObject);
begin
  CurPanel.TimeFormat := edtTimeFormat.Text;
end;

procedure TForm2.spnGaugePositionChange(Sender: TObject);
begin
  CurPanel.GaugeAttrs.Position := spnGaugePosition.Value;
end;

procedure TForm2.spnWidthChange(Sender: TObject);
begin
  CurPanel.Width := spnWidth.Value;
end;

procedure TForm2.btnGlyphClick(Sender: TObject);
begin
  if OpenDlg.Execute then
  begin
    imgGlyph.Picture.LoadFromFile(OpenDlg.Filename);
    CurPanel.Glyph.LoadFromFile(OpenDlg.Filename);
  end;
end;


procedure TForm2.DFSStatusBar2Panels6DrawPanel(StatusBar: TDFSStatusBar;
  Panel: TDFSStatusPanel; const Rect: TRect);
var
  R: TRect;
begin
  R := Rect;
  dec(R.Bottom); // Otherwise you draw in the border area
  with StatusBar.Canvas do
  begin
    if Panel.Enabled then
      Pen.Color := clGreen
    else
      Pen.Color := clRed;
    MoveTo(R.Left, R.Top);
    LineTo(R.Right, R.Bottom);
    MoveTo(R.Left, R.Bottom);
    LineTo(R.Right, R.Top);
  end;
end;

procedure TForm2.DFSStatusBar2Panels6HintText(StatusBar: TDFSStatusBar;
  Panel: TDFSStatusPanel; var Hint: String);
begin
  if Panel.Enabled then
    Hint := 'Enabled'
  else
    Hint := 'Disabled';
end;

procedure TForm2.cmbGaugeStyleChange(Sender: TObject);
begin
  CurPanel.GaugeAttrs.Style := TDFSGaugeStyle(cmbGaugeStyle.ItemIndex);
end;

procedure TForm2.edtHintChange(Sender: TObject);
begin
  CurPanel.Hint := edtHint.Text;
end;

end.


