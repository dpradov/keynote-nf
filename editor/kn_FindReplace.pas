unit kn_FindReplace;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)

interface

uses
   Winapi.Windows,
   Winapi.Messages,
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   ComCtrls95,
   RxPlacemnt,
   kn_Info
   ;


type
  //TReplaceEvent = procedure( ReplaceAll : boolean ) of object;
  TReplaceEvent = procedure( ReplaceAll : boolean );
  TNotifyEvent_ = procedure(Sender: TObject);

type
  TForm_FindReplace = class(TForm)
    Button_Find: TButton;
    Button_Cancel: TButton;
    Combo_Text: TComboBox;
    GroupBox_Opts: TGroupBox;
    CheckBox_MatchCase: TCheckBox;
    CheckBox_EntireScope: TCheckBox;
    FormPlacement: TFormPlacement;
    CheckBox_WholeWordsOnly: TCheckBox;
    CheckBox_AllTabs: TCheckBox;
    CheckBox_AllNodes: TCheckBox;
    Combo_Replace: TComboBox;
    Button_Replace: TButton;
    Button_ReplaceAll: TButton;
    CheckBox_HiddenNodes: TCheckBox;
    CheckBox_SelectedText: TCheckBox;
    CheckBox_Confirm: TCheckBox;
    CheckBox_Wrap: TCheckBox;
    Pages: TPage95Control;
    Tab_Find: TTab95Sheet;
    Tab_Replace: TTab95Sheet;
    TntLabel1: TLabel;
    TntLabel2: TLabel;
    TntLabel3: TLabel;
    procedure PagesChange(Sender: TObject);
    procedure CheckBox_AllNodesClick(Sender: TObject);
    procedure CheckBox_ScopeChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Combo_TextChange(Sender: TObject);
    procedure Button_FindClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    function GetModeReplace: boolean;
    procedure SetModeReplace(value: boolean);

  public
    { Public declarations }
    OK_Click : boolean;
    Initializing : boolean;
    myFindOptions : TFindOptions;

    ReplaceEvent : TReplaceEvent;
    FormCloseEvent : TNotifyEvent_;
    FindEvent : TNotifyEvent_;

    myNotifyproc : TBooleanNotifyProc;

    procedure OptionsToForm;
    procedure FormToOptions;
    procedure HistoryToCombo;
    procedure ComboToHistory;
    property ModeReplace:Boolean read GetModeReplace write SetModeReplace;
  end;


implementation
uses
   gf_misc,
   gf_strings,
   kn_Const,
   kn_Ini,
   kn_Global,
   kn_NoteObj,
   kn_Main,
   kn_FindReplaceMng,
   kn_MacroMng
   ;


{$R *.DFM}

procedure TForm_FindReplace.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := _MainFormHandle;
end; // CreateParams

procedure TForm_FindReplace.FormCreate(Sender: TObject);
begin
  OK_Click := false;
  Initializing := true;
  myNotifyProc := nil;

  ReplaceEvent := nil;
  FormCloseEvent := nil;

  with FormPlacement do
  begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;

  InitializeFindOptions( myFindOptions );
end; // CREATE

procedure TForm_FindReplace.FormActivate(Sender: TObject);
var
  enableReplace: boolean;
begin
  if assigned( myNotifyProc ) then
    myNotifyProc( false );

  if Initializing then
  begin
    Initializing := false;
    OptionsToForm;
    Button_Find.Enabled := ( Combo_Text.Text <> '' );
    if IsRecordingMacro then begin     // Opciones limitadas
       CheckBox_AllTabs.Enabled := False;
       CheckBox_EntireScope.Enabled := False;
       CheckBox_AllNodes.Enabled := False;
       CheckBox_HiddenNodes.Enabled := False;
       CheckBox_Wrap.Enabled:= False;
       CheckBox_Confirm.Enabled := False;
    end;
  end
  else begin
      if not myFindOptions.AllTabs_FindReplace then
         if (ActiveKntFolder <> StartFolder) or
            (not myFindOptions.AllNodes) and (ActiveKntFolder.TV.Selected <> StartNode) then
              myFindOptions.FindNew := true;
  end;

  if Form_Main.NoteIsReadOnly( ActiveKntFolder, true) then begin
     modeReplace:= False;
     Tab_Replace.Enabled:= False;
  end
  else
     Tab_Replace.Enabled:= True;

  Pages.Refresh;

  enableReplace:= Tab_Replace.Enabled and Button_Find.Enabled;
  Button_Replace.Enabled := enableReplace;
  Button_ReplaceAll.Enabled := enableReplace;


  myFindOptions.SelectedText:= False;
  if ( ActiveKntFolder.Editor.SelLength > 0 ) then begin
      CheckBox_SelectedText.Enabled:= True;
      myFindOptions.SelectedText:= not IsWord(Trim(ActiveKntFolder.Editor.SelText));
  end
  else
      CheckBox_SelectedText.Enabled:= False;
  CheckBox_SelectedText.Checked:= myFindOptions.SelectedText;
  
end; // ACTIVATE

procedure TForm_FindReplace.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  OK_Click := false;
end; // CLOSE QUERY

procedure TForm_FindReplace.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if (( shift = [] ) and ( not
      ( Combo_Text.DroppedDown or Combo_Replace.DroppedDown )
      )) then
    begin
      key := 0;
      OK_Click := false;
      Close;
    end;
  end;
end; // KEY DOWN

procedure TForm_FindReplace.HistoryToCombo;
begin
  Combo_Text.Items.BeginUpdate;
  try
    DelimTextToStrs( Combo_Text.Items, myFindOptions.History, HISTORY_SEPARATOR );
  finally
    Combo_Text.Items.EndUpdate;
    Combo_Text.SetFocus;
  end;

  Combo_Replace.Items.BeginUpdate;
  try
    DelimTextToStrs( Combo_Replace.Items, myFindOptions.ReplaceHistory, HISTORY_SEPARATOR );
  finally
    Combo_Replace.Items.EndUpdate;
  end;

end;

// HistoryToCombo

procedure TForm_FindReplace.CheckBox_AllNodesClick(Sender: TObject);
begin
   CheckBox_HiddenNodes.Enabled := CheckBox_AllNodes.Checked;
   myFindOptions.FindNew := true;
end;

procedure TForm_FindReplace.CheckBox_ScopeChanged(Sender: TObject);
begin
     myFindOptions.FindNew := true;
end;

procedure TForm_FindReplace.ComboToHistory;
var
  i : integer;
begin
  if ( Combo_Text.Text <> '' ) then
  begin
    myFindOptions.History := AnsiQuotedStr( Combo_Text.Text, '"' );
    for i := 0 to pred( Combo_Text.Items.Count ) do
    begin
      if ( i >= myFindOptions.HistoryMaxCnt ) then break;
      if ( Combo_Text.Items[i] <> Combo_Text.Text ) then
        myFindOptions.History :=  myFindOptions.History + HISTORY_SEPARATOR + AnsiQuotedStr( Combo_Text.Items[i], '"' );
    end;
  end;
  if ( Combo_Replace.Text <> '' ) then
  begin
    myFindOptions.ReplaceHistory := AnsiQuotedStr( Combo_Replace.Text, '"' );
    for i := 0 to pred( Combo_Replace.Items.Count ) do
    begin
      if ( i >= myFindOptions.HistoryMaxCnt ) then break;
      if ( Combo_Replace.Items[i] <> Combo_Replace.Text ) then
        myFindOptions.ReplaceHistory :=  myFindOptions.ReplaceHistory + HISTORY_SEPARATOR + AnsiQuotedStr( Combo_Replace.Items[i], '"' );
    end;
  end;

end; // ComboToHistory

procedure TForm_FindReplace.OptionsToForm;
begin
  HistoryToCombo;
  with myFindOptions do
  begin
    CheckBox_MatchCase.Checked := MatchCase;
    CheckBox_WholeWordsOnly.Checked := WholeWordsOnly;

    Combo_Text.Text := Pattern;
    Combo_Replace.Text := ReplaceWith;
    Combo_Text.SelectAll;

    CheckBox_AllTabs.Checked := AllTabs_FindReplace and not IsRecordingMacro;
    CheckBox_EntireScope.Checked := EntireScope and not IsRecordingMacro;
    CheckBox_AllNodes.Checked := AllNodes and not IsRecordingMacro;
    CheckBox_HiddenNodes.Checked := HiddenNodes and not IsRecordingMacro;
    CheckBox_Wrap.Checked:= Wrap and not IsRecordingMacro;
    CheckBox_Confirm.Checked := ReplaceConfirm and not IsRecordingMacro;
  end;
end;

function TForm_FindReplace.GetModeReplace: boolean;
begin
    Result:= (Pages.ActivePage = Tab_Replace);
end;

procedure TForm_FindReplace.SetModeReplace(value: boolean);
begin
    if value then
       Pages.ActivePage := Tab_Replace
    else
       Pages.ActivePage := Tab_Find;

    PagesChange(nil);
end;

procedure TForm_FindReplace.PagesChange(Sender: TObject);
var
   showReplace: boolean;
begin
   showReplace:= modeReplace;

   Button_Replace.Visible := showReplace;
   Button_ReplaceAll.Visible := showReplace;
   Combo_Replace.Visible := showReplace;
   CheckBox_Confirm.Visible:= showReplace;
   CheckBox_SelectedText.Visible:= showReplace;
   if showReplace then
      Caption:= Tab_Replace.Caption
   else
      Caption:= Tab_Find.Caption;

end;

// OptionsToForm

procedure TForm_FindReplace.FormToOptions;
begin
  with myFindOptions do
  begin
    AllNodes := CheckBox_AllNodes.Checked;
    AllTabs_FindReplace := CheckBox_AllTabs.Checked;
    EntireScope := CheckBox_EntireScope.Checked;
    MatchCase := CheckBox_MatchCase.Checked;
    WholeWordsOnly := ( CheckBox_WholeWordsOnly.Enabled and CheckBox_WholeWordsOnly.Checked );
    ReplaceConfirm := CheckBox_Confirm.Checked;
    Pattern := Combo_Text.Text;
    ReplaceWith := Combo_Replace.Text;
    HiddenNodes:= CheckBox_HiddenNodes.Checked;
    SelectedText:= CheckBox_SelectedText.Checked;
    Wrap := CheckBox_Wrap.Checked;
  end;
  // ComboToHistory;
end; // FormToOptions

procedure TForm_FindReplace.Combo_TextChange(Sender: TObject);
var
  enableReplace: boolean;
begin
  Button_Find.Enabled := ( Combo_Text.Text <> '' );
  enableReplace:= Tab_Replace.Enabled and Button_Find.Enabled;
  Button_Replace.Enabled := enableReplace;
  Button_ReplaceAll.Enabled := enableReplace;
  CheckBox_WholeWordsOnly.Enabled := IsWord( Combo_Text.Text );
  myFindOptions.FindNew := True;
end;

procedure TForm_FindReplace.Button_FindClick(Sender: TObject);
begin
  if (( Combo_Text.Text <> '' ) and ( Combo_Text.Items.IndexOf( Combo_Text.Text ) < 0 )) then
    Combo_Text.Items.Insert( 0, Combo_Text.Text );
  if (( Combo_Replace.Text <> '' ) and ( Combo_Replace.Items.IndexOf( Combo_Replace.Text ) < 0 )) then
    Combo_Replace.Items.Insert( 0, Combo_Replace.Text );
  FormToOptions;
  OK_Click := true;

  case ( sender as TButton ).Tag of
    0 :  // Find
      if assigned( FindEvent ) then begin
        myFindOptions.SelectedText:= False;
        FindEvent( self );
      end;

    1 :  // Replace
      if assigned( ReplaceEvent ) then begin
        myFindOptions.SelectedText:= False;
        ReplaceEvent( false );
      end;

    2 :  // Replace All
      if assigned( ReplaceEvent ) then begin
        myFindOptions.FindNew := True;
        ReplaceEvent( true );
      end;
  end;

  myFindOptions.FindNew := False;
end;

procedure TForm_FindReplace.Button_CancelClick(Sender: TObject);
begin
  OK_Click := false;
  Close;
end;

procedure TForm_FindReplace.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  ComboToHistory;
  if assigned( FormCloseEvent ) then
     FormCloseEvent( self );
end;

procedure TForm_FindReplace.FormDeactivate(Sender: TObject);
begin
  if assigned( myNotifyProc ) then
     myNotifyProc( true );
end;

function TForm_FindReplace.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   if Pages.ActivePage = Tab_Replace then
      ActiveKeyNoteHelp(PChar(Tab_Replace.HelpKeyword))
   else
      ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

end.
