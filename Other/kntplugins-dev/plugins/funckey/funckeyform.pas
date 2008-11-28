unit funckeyform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,
  kn_Info,
  ExtCtrls, RXCombos;

type
  TForm_FuncKey = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    GroupBox1: TGroupBox;
    Combo_Keys: TComboBox;
    Button_Assign: TButton;
    Label1: TLabel;
    LB_Current: TLabel;
    GroupBox2: TGroupBox;
    Combo_Cmd: TComboBox;
    Pages: TNotebook;
    Label2: TLabel;
    Combo_Macro: TComboBox;
    Label3: TLabel;
    Combo_Plugin: TComboBox;
    Label4: TLabel;
    Combo_Style: TComboBox;
    Label5: TLabel;
    Combo_Font: TFontComboBox;
    Button_HLP: TButton;
    Button_Clear: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure Combo_KeysChange(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Combo_CmdChange(Sender: TObject);
    procedure Button_HLPClick(Sender: TObject);
    procedure Button_AssignClick(Sender: TObject);
    procedure Button_ClearClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    AltFKeys : TFuncKeys;
    ShiftAltFKeys : TFuncKeys;
    CtrlAltFKeys : TFuncKeys;
    KEY_FN : string;
    OK_Click : boolean;

    procedure AssignShortcut( const NewAssignment : boolean );
    procedure EnableAssignButton;
  end;


implementation
uses IniFiles, kn_Const, kn_StyleObj, gf_files;

{$R *.DFM}

procedure TForm_FuncKey.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if ( shift = [] ) then
    begin
      key := 0;
      OK_Click := false;
      modalresult := mrCancel;
    end;
  end;
end;

procedure TForm_FuncKey.FormCreate(Sender: TObject);
var
  i : integer;
  fn, s : string;
  IniFile : TIniFile;
begin
  OK_Click := false;
  LB_Current.Font.Color := clHighlight;

  Combo_Cmd.Items.Add( 'Execute macro' );
  Combo_Cmd.Items.Add( 'Execute plugin' );
  Combo_Cmd.Items.Add( 'Apply style' );
  Combo_Cmd.Items.Add( 'Apply font' );
  Combo_Cmd.ItemIndex := 0;
  Pages.PageIndex := 0;

  // fill shortcut combo
  for i := 1 to 12 do
    Combo_Keys.Items.Add( Format(
      'Alt+F%d', [i]
    ));
  for i := 1 to 12 do
    Combo_Keys.Items.Add( Format(
      'Shift+Alt+F%d', [i]
    ));
  for i := 1 to 12 do
    Combo_Keys.Items.Add( Format(
      'Ctrl+Alt+F%d', [i]
    ));
  Combo_Keys.ItemIndex := 0;
  Combo_Keys.OnChange := Combo_KeysChange;

  // figure out ini filename
  KEY_FN := '';
  for i := 1 to ParamCount do
  begin
    s := ansilowercase( ParamStr( i ));
    if ( extractfileext( s ) = ext_INI ) then
    begin
      KEY_FN := s;
      break;
    end;
  end;
  if ( KEY_FN = '' ) then
  begin
    KEY_FN := ansilowercase( changefileext( ParamStr( 0 ), ext_Keyboard ));
  end
  else
  begin
    KEY_FN := changefileext( KEY_FN, ext_Keyboard );
  end;

  // load existing keys
  if fileexists( KEY_FN ) then
  begin
    IniFile := TIniFile.Create( KEY_FN );

    try
      with IniFile do
      begin
        s := 'Alt';
        for i := 1 to 12 do
          AltFKeys[i] := readstring( s, inttostr( i ), '' );
        s := 'ShiftAlt';
        for i := 1 to 12 do
          ShiftAltFKeys[i] := readstring( s, inttostr( i ), '' );
        s := 'CtrlAlt';
        for i := 1 to 12 do
          CtrlAltFKeys[i] := readstring( s, inttostr( i ), '' );
      end;
    finally
      IniFile.Free
    end;
  end;

  // fill macro combo
  GetFilesInFolder(
    extractfilepath( application.exename )+_MACRO_FOLDER,
    '*'+ext_MACRO,
    true, false,
    Combo_Macro.Items
  );
  if ( Combo_Macro.Items.Count > 0 ) then
    Combo_Macro.ItemIndex := 0;

  // fill plugins combo
  GetFilesInFolder(
    extractfilepath( application.exename )+_PLUGIN_FOLDER,
    '*'+ext_Plugin,
    true, false,
    Combo_Plugin.Items
  );
  if ( Combo_Plugin.Items.Count > 0 ) then
    Combo_Plugin.ItemIndex := 0;

  // fill style combo
  fn := changefileext( KEY_FN, ext_Style );
  if fileexists( fn ) then
  begin
    LoadStyleManagerInfo( fn );
    for i := 1 to StyleManager.Count do
    begin
      Combo_Style.Items.Add( StyleManager[pred( i )] );
    end;
  end;
  if ( Combo_Style.Items.Count > 0 ) then
    Combo_Style.ItemIndex := 0;

  Combo_Font.FontName := 'Arial';

  Combo_CmdChange( Combo_Cmd );

end;

procedure TForm_FuncKey.Combo_KeysChange(Sender: TObject);
var
  i, m : integer;
  s, cmds : string;
  cmd : char;
begin
  Combo_Keys.OnChange := nil;
  try
    i := succ( Combo_Keys.ItemIndex );

    if ( i > 24 ) then // [ssCtrl,ssAlt]
    begin
      m := 2;
      i := i - 24;
    end
    else
    if ( i > 12 ) then // [ssShift,ssAlt]
    begin
      m := 1;
      i := i - 12;
    end
    else // [ssAlt]
    begin
      m := 0;
    end;

    case m of
      0 : begin
        s := AltFKeys[i];
      end;
      1 : begin
        s := ShiftAltFKeys[i];
      end;
      else
      begin
        s := CtrlAltFKeys[i];
      end;
    end;

    EnableAssignButton;

    if (( m = 0 ) and ( i = 4 )) then
    begin
      Button_Clear.Enabled := false;
      LB_Current.Caption := 'Key is reserved by Windows.';
      exit;
    end
    else
    begin
      Button_Clear.Enabled := ( s <> '' );
    end;

    if ( s = '' ) then
    begin
      LB_Current.Caption := '(nothing)';
      exit;
    end;

    i := pos( _KEY_FUNC_DELIMITER, s );
    cmd := s[1];
    delete( s, 1, i );
    cmds := '';

    case cmd of
      _KEY_FUNC_MACRO : cmds := 'Macro:';
      _KEY_FUNC_PLUGIN : cmds := 'Plugin:';
      _KEY_FUNC_STYLE : cmds := 'Style:';
      _KEY_FUNC_FONT : cmds := 'Font:';
      else
      begin
        LB_Current.Caption := '(nothing)';
        exit;
      end;
    end;

    LB_Current.Caption := Format( '%s "%s"', [cmds, s] );


  finally
    Combo_Keys.OnChange := Combo_KeysChange;
  end;
end; // Combo_KeysChange

procedure TForm_FuncKey.Button_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;

procedure TForm_FuncKey.Button_CancelClick(Sender: TObject);
begin
  OK_Click := false;
end;

procedure TForm_FuncKey.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  IniFile : TIniFile;
  section : string;
  i : integer;
begin

  if OK_Click then
  begin

    IniFile := TIniFile.Create( KEY_FN );

    try
      with IniFile do
      begin
        section := 'Alt';
        for i := 1 to 12 do
          if ( AltFKeys[i] <> '' ) then
            writestring( section, inttostr( i ), AltFKeys[i] );
        section := 'ShiftAlt';
        for i := 1 to 12 do
          if ( ShiftAltFKeys[i] <> '' ) then
            writestring( section, inttostr( i ), ShiftAltFKeys[i] );
        section := 'CtrlAlt';
        for i := 1 to 12 do
          if ( CtrlAltFKeys[i] <> '' ) then
            writestring( section, inttostr( i ), CtrlAltFKeys[i] );
      end;
    finally
      IniFile.Free
    end;

  end;
end; // FormCloseQuery

procedure TForm_FuncKey.Combo_CmdChange(Sender: TObject);
begin
  Pages.PageIndex := Combo_Cmd.ItemIndex;
  EnableAssignButton;
end;


procedure TForm_FuncKey.Button_HLPClick(Sender: TObject);
begin
  messagedlg(
    'You can assign function key shortcuts to several types of KeyNote commands:' + #13 +
    '- running a macro,' + #13 +
    '- running a plugin,' + #13 +
    '- applying a style,' + #13 +
    '- applying a font.' + #13 +
    'You can only assign Alt, Shift+Alt and Ctrl+Alt F-key shortcuts.' + #13 +
    'Select a key shortcut, then select an action to be performed when that shortcut is pressed, and then click "Assign".',
    mtInformation, [mbOK], 0 );
end;

procedure TForm_FuncKey.Button_AssignClick(Sender: TObject);
begin
  AssignShortcut( true );
end;

procedure TForm_FuncKey.AssignShortcut( const NewAssignment : boolean );
var
  i, m : integer;
  s : string;
begin

  s := '';
  if NewAssignment then
  begin
    case Pages.PageIndex of
      0 : s := _KEY_FUNC_MACRO + _KEY_FUNC_DELIMITER + Combo_Macro.Items[Combo_Macro.ItemIndex];
      1 : s := _KEY_FUNC_PLUGIN + _KEY_FUNC_DELIMITER + Combo_Plugin.Items[Combo_Plugin.ItemIndex];
      2 : s := _KEY_FUNC_STYLE + _KEY_FUNC_DELIMITER + Combo_Style.Items[Combo_Style.ItemIndex];
      3 : s := _KEY_FUNC_FONT + _KEY_FUNC_DELIMITER + Combo_Font.FontName;
    end;
  end;

  i := succ( Combo_Keys.ItemIndex );

  if ( i > 24 ) then // [ssCtrl,ssAlt]
  begin
    m := 2;
    i := i - 24;
  end
  else
  if ( i > 12 ) then // [ssShift,ssAlt]
  begin
    m := 1;
    i := i - 12;
  end
  else // [ssAlt]
  begin
    m := 0;
  end;

  case m of
    0 : begin
      AltFKeys[i] := s;
    end;
    1 : begin
      ShiftAltFKeys[i] := s;
    end;
    else
    begin
      CtrlAltFKeys[i] := s;
    end;
  end;

  Combo_KeysChange( Combo_Keys );

end; // AssignShortcut

procedure TForm_FuncKey.EnableAssignButton;
begin
  if ( Combo_Keys.ItemIndex = 3 ) then
  begin
    Button_Assign.Enabled := false; // Alt+F4
    exit;
  end;

  case Pages.PageIndex of
    0 : begin
      Button_Assign.Enabled := ( Combo_Macro.ItemIndex >= 0 );
    end;
    1 : begin
      Button_Assign.Enabled := ( Combo_Plugin.ItemIndex >= 0 );
    end;
    2 : begin
      Button_Assign.Enabled := ( Combo_Style.ItemIndex >= 0 );
    end;
    3 : begin
      Button_Assign.Enabled := ( Combo_Font.FontName <> '' );
    end;
  end;
end; // EnableAssignButton

procedure TForm_FuncKey.Button_ClearClick(Sender: TObject);
begin
  AssignShortcut( false );
end;

end.
