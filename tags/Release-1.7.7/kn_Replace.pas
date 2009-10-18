
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

unit kn_Replace;

interface

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls,
  gf_misc, kn_Info, kn_Const,
  kn_NoteObj, kn_FileObj,
  Placemnt, kn_TabSelect,
  gf_strings, kn_INI, TntStdCtrls;

type
  //TReplaceEvent = procedure( ReplaceAll : boolean ) of object;
  TReplaceEvent = procedure( ReplaceAll : boolean );         //*1
  TNotifyEvent_ = procedure(Sender: TObject);   //*1

type
  TForm_Replace = class(TForm)
    Button_Find: TTntButton;
    Button_Cancel: TTntButton;
    Label1: TTntLabel;
    Combo_Text: TTntComboBox;
    GroupBox_Opts: TTntGroupBox;
    CheckBox_MatchCase: TTntCheckBox;
    CheckBox_EntireScope: TTntCheckBox;
    FormPlacement: TFormPlacement;
    CheckBox_WholeWordsOnly: TTntCheckBox;
    CheckBox_AllTabs: TTntCheckBox;
    CheckBox_AllNodes: TTntCheckBox;
    Label2: TTntLabel;
    Combo_Replace: TTntComboBox;
    Button_Replace: TTntButton;
    Button_ReplaceAll: TTntButton;
    CheckBox_Confirm: TTntCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Combo_TextChange(Sender: TObject);
    procedure Button_FindClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure Button_FindPrevClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
    procedure CreateParams(var Params: TCreateParams); override;
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
  end;


implementation
uses WideStrUtils;

{$R *.DFM}

procedure TForm_Replace.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := _MainFormHandle;
end; // CreateParams

procedure TForm_Replace.FormCreate(Sender: TObject);
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

procedure TForm_Replace.FormActivate(Sender: TObject);
begin
  if assigned( myNotifyProc ) then
    myNotifyProc( false );

  if Initializing then
  begin
    Initializing := false;
    OptionsToForm;
    Button_Find.Enabled := ( Combo_Text.Text <> '' );
    Button_Replace.Enabled := Button_Find.Enabled;
    Button_ReplaceAll.Enabled := Button_Find.Enabled;
  end;
end; // ACTIVATE

procedure TForm_Replace.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  OK_Click := false;
end; // CLOSE QUERY

procedure TForm_Replace.FormKeyDown(Sender: TObject; var Key: Word;
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

procedure TForm_Replace.HistoryToCombo;
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

end; // HistoryToCombo

procedure TForm_Replace.ComboToHistory;
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

procedure TForm_Replace.OptionsToForm;
begin
  HistoryToCombo;
  with myFindOptions do
  begin
    CheckBox_AllTabs.Checked := AllTabs;
    CheckBox_EntireScope.Checked := EntireScope;
    CheckBox_MatchCase.Checked := MatchCase;
    CheckBox_WholeWordsOnly.Checked := WholeWordsOnly;
    CheckBox_AllNodes.Checked := AllNodes;
    Combo_Text.Text := ReplacePattern;
    Combo_Replace.Text := ReplaceWith;
    Combo_Text.SelectAll;
    CheckBox_Confirm.Checked := ReplaceConfirm;
  end;
end; // OptionsToForm

procedure TForm_Replace.FormToOptions;
begin
  with myFindOptions do
  begin
    AllNodes := CheckBox_AllNodes.Checked;
    AllTabs := CheckBox_AllTabs.Checked;
    EntireScope := CheckBox_EntireScope.Checked;
    MatchCase := CheckBox_MatchCase.Checked;
    WholeWordsOnly := ( CheckBox_WholeWordsOnly.Enabled and CheckBox_WholeWordsOnly.Checked );
    ReplaceConfirm := CheckBox_Confirm.Checked;
    ReplacePattern := Combo_Text.Text;
    ReplaceWith := Combo_Replace.Text;
    // Wrap := false; // must not wrap while replacing, otherwise we'll loop forever
  end;
  // ComboToHistory;
end; // FormToOptions

procedure TForm_Replace.Combo_TextChange(Sender: TObject);
begin
  Button_Find.Enabled := ( Combo_Text.Text <> '' );
  Button_Replace.Enabled := Button_Find.Enabled;
  Button_ReplaceAll.Enabled := Button_Find.Enabled;
  // Button_FindPrev.Enabled := Button_Find.Enabled;
  CheckBox_WholeWordsOnly.Enabled := IsWord( Combo_Text.Text );
end;

procedure TForm_Replace.Button_FindClick(Sender: TObject);
begin
  if (( Combo_Text.Text <> '' ) and ( Combo_Text.Items.IndexOf( Combo_Text.Text ) < 0 )) then
    Combo_Text.Items.Insert( 0, Combo_Text.Text );
  if (( Combo_Replace.Text <> '' ) and ( Combo_Replace.Items.IndexOf( Combo_Replace.Text ) < 0 )) then
    Combo_Replace.Items.Insert( 0, Combo_Replace.Text );
  FormToOptions;
  OK_Click := true;

  case ( sender as TButton ).Tag of
    0 : begin // Find
      if assigned( FindEvent ) then
        FindEvent( self );
    end;
    1 : begin // Replace
      if assigned( ReplaceEvent ) then
        ReplaceEvent( false );
    end;
    2 : begin // Replace All
      if assigned( ReplaceEvent ) then
        ReplaceEvent( true );
    end;
  end;

end;

procedure TForm_Replace.Button_CancelClick(Sender: TObject);
begin
  OK_Click := false;
  Close;
end;

procedure TForm_Replace.Button_FindPrevClick(Sender: TObject);
begin
  // myFindOptions.SearchDown := false;
  OK_Click := true;
end;

procedure TForm_Replace.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  ComboToHistory;
  if assigned( FormCloseEvent ) then
    FormCloseEvent( self );
end;

procedure TForm_Replace.FormDeactivate(Sender: TObject);
begin
  if assigned( myNotifyProc ) then
    myNotifyProc( true );
end;

end.
