
(* ************************************************************
 KEYNOTE: MOZILLA PUBLIC LICENSE STATEMENT.
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is KeyNote 1.0.

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 Released: 30 June 2001
 -----------------------------------------------------------
 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <cicho@tenbit.pl>

************************************************************ *)

unit FrmFindReplace;

interface

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls,
  gf_misc, kn_Info, kn_Const,
  kn_NoteObj, kn_FileObj,
  Placemnt, kn_TabSelect,
  gf_strings, kn_INI, TntStdCtrls, ComCtrls95;

type
  //TReplaceEvent = procedure( ReplaceAll : boolean ) of object;
  TReplaceEvent = procedure( ReplaceAll : boolean );         //*1
  TNotifyEvent_ = procedure(Sender: TObject);   //*1

type
  TForm_FindReplace = class(TForm)
    Button_Find: TTntButton;
    Button_Cancel: TTntButton;
    Combo_Text: TTntComboBox;
    GroupBox_Opts: TTntGroupBox;
    CheckBox_MatchCase: TTntCheckBox;
    CheckBox_EntireScope: TTntCheckBox;
    FormPlacement: TFormPlacement;
    CheckBox_WholeWordsOnly: TTntCheckBox;
    CheckBox_AllTabs: TTntCheckBox;
    CheckBox_AllNodes: TTntCheckBox;
    Combo_Replace: TTntComboBox;
    Button_Replace: TTntButton;
    Button_ReplaceAll: TTntButton;
    CheckBox_HiddenNodes: TTntCheckBox;
    CheckBox_SelectedText: TTntCheckBox;
    CheckBox_Confirm: TTntCheckBox;
    CheckBox_Wrap: TTntCheckBox;
    Pages: TPage95Control;
    Tab_Find: TTab95Sheet;
    Tab_Replace: TTab95Sheet;
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    TntLabel3: TTntLabel;
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
uses WideStrUtils, kn_Global, kn_MacroMng, kn_FindReplaceMng, kn_Main;


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
      if not myFindOptions.AllTabs then
         if (ActiveNote <> StartNote) or
            (not myFindOptions.AllNodes) and ((ActiveNote.Kind = ntTree) and (TTreeNote(ActiveNote).TV.Selected <> StartNode)) then
              myFindOptions.FindNew := true;
  end;

  if Form_Main.NoteIsReadOnly( ActiveNote, true) then begin
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
  if ( ActiveNote.Editor.SelLength > 0 ) then begin
      CheckBox_SelectedText.Enabled:= True;
      myFindOptions.SelectedText:= not IsWord(Trim(ActiveNote.Editor.SelTextW));
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
    DelimTextToWStrs( Combo_Text.Items, myFindOptions.History, HISTORY_SEPARATOR );
  finally
    Combo_Text.Items.EndUpdate;
    Combo_Text.SetFocus;
  end;

  Combo_Replace.Items.BeginUpdate;
  try
    DelimTextToWStrs( Combo_Replace.Items, myFindOptions.ReplaceHistory, HISTORY_SEPARATOR );
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
    myFindOptions.History := WideQuotedStr( Combo_Text.Text, '"' );
    for i := 0 to pred( Combo_Text.Items.Count ) do
    begin
      if ( i >= myFindOptions.HistoryMaxCnt ) then break;
      if ( Combo_Text.Items[i] <> Combo_Text.Text ) then
        myFindOptions.History :=  myFindOptions.History + HISTORY_SEPARATOR + WideQuotedStr( Combo_Text.Items[i], '"' );
    end;
  end;
  if ( Combo_Replace.Text <> '' ) then
  begin
    myFindOptions.ReplaceHistory := WideQuotedStr( Combo_Replace.Text, '"' );
    for i := 0 to pred( Combo_Replace.Items.Count ) do
    begin
      if ( i >= myFindOptions.HistoryMaxCnt ) then break;
      if ( Combo_Replace.Items[i] <> Combo_Replace.Text ) then
        myFindOptions.ReplaceHistory :=  myFindOptions.ReplaceHistory + HISTORY_SEPARATOR + WideQuotedStr( Combo_Replace.Items[i], '"' );
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

    CheckBox_AllTabs.Checked := AllTabs and not IsRecordingMacro;
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
    AllTabs := CheckBox_AllTabs.Checked;
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

end.
