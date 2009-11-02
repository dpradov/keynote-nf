
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

unit kn_Find;

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
   TNotifyEvent_ = procedure(Sender: TObject);

type
  TForm_Find = class(TForm)
    Button_Find: TTntButton;
    Button_Cancel: TTntButton;
    Label1: TTntLabel;
    Combo_Text: TTntComboBox;
    GroupBox_Opts: TTntGroupBox;
    CB_MatchCase: TTntCheckBox;
    CB_Wrap: TTntCheckBox;
    CB_EntireScope: TTntCheckBox;
    FormPlacement: TFormPlacement;
    CB_WholeWordsOnly: TTntCheckBox;
    CB_AllTabs: TTntCheckBox;
    CB_AllNodes: TTntCheckBox;
    CB_HiddenNodes: TTntCheckBox;
    procedure Label1Click(Sender: TObject);
    procedure CB_AllNodesClick(Sender: TObject);
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

    FindEvent : TNotifyEvent_;
    FormCloseEvent : TNotifyEvent_;

    myRestrictedOptions : boolean;

    myNotifyproc : TBooleanNotifyProc;

    procedure OptionsToForm;
    procedure FormToOptions;
    procedure HistoryToCombo;
    procedure ComboToHistory;
  end;


implementation
uses WideStrUtils;

{$R *.DFM}

procedure TForm_Find.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := _MainFormHandle;
end; // CreateParams


procedure TForm_Find.FormCreate(Sender: TObject);
begin
  OK_Click := false;
  Initializing := true;
  myRestrictedOptions := false;
  myNotifyProc := nil;

  FindEvent := nil;
  FormCloseEvent := nil;

  with FormPlacement do
  begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;

  InitializeFindOptions( myFindOptions );
end; // CREATE

procedure TForm_Find.FormActivate(Sender: TObject);
begin
  if assigned( myNotifyProc ) then
    myNotifyProc( false );

  if Initializing then // only do this once
  begin
    Initializing := false;
    OptionsToForm;
    if myRestrictedOptions then
    begin
      CB_EntireScope.Enabled := false;
      CB_Wrap.Enabled := false;
      CB_AllTabs.Enabled := false;
      CB_AllNodes.Enabled := false;
    end;
    Button_Find.Enabled := ( Combo_Text.Text <> '' );
  end;
end; // ACTIVATE

procedure TForm_Find.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  // ComboToHistory;
  OK_Click := false;
end; // CLOSE QUERY

procedure TForm_Find.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if (( shift = [] ) and ( not Combo_Text.DroppedDown )) then
    begin
      key := 0;
      OK_Click := false;
      Close;
    end;
  end;
end; // KEY DOWN

procedure TForm_Find.HistoryToCombo;
begin
  // Combo_Text.Enabled := false;
  Combo_Text.Items.BeginUpdate;
  try
      DelimTextToWStrs( Combo_Text.Items, myFindOptions.History, HISTORY_SEPARATOR );
  finally
    Combo_Text.Items.EndUpdate;
    // Combo_Text.Enabled := true;
    Combo_Text.SetFocus;
  end;
end; procedure TForm_Find.Label1Click(Sender: TObject);
begin

end;

// HistoryToCombo

procedure TForm_Find.CB_AllNodesClick(Sender: TObject);
begin
   CB_HiddenNodes.Enabled := CB_AllNodes.Checked;
end;

procedure TForm_Find.ComboToHistory;
var
  i : integer;
begin
  if ( Combo_Text.Text = '' ) then exit;
  myFindOptions.History := WideQuotedStr( Combo_Text.Text, '"' );
  for i := 0 to pred( Combo_Text.Items.Count ) do
  begin
    if ( i >= myFindOptions.HistoryMaxCnt ) then break;
    if ( Combo_Text.Items[i] <> Combo_Text.Text ) then
      myFindOptions.History :=  myFindOptions.History + HISTORY_SEPARATOR + WideQuotedStr( Combo_Text.Items[i], '"' );
  end;
end; // ComboToHistory

procedure TForm_Find.OptionsToForm;
begin
  HistoryToCombo;
  with myFindOptions do
  begin

    CB_MatchCase.Checked := MatchCase;
    CB_WholeWordsOnly.Checked := WholeWordsOnly;
    CB_EntireScope.Checked := ( EntireScope and ( not myRestrictedOptions ));
    CB_AllNodes.Checked := ( AllNodes and ( not myRestrictedOptions ));
    CB_HiddenNodes.Checked := ( HiddenNodes and ( not myRestrictedOptions ));  // [dpv]
    CB_AllTabs.Checked := ( AllTabs and ( not myRestrictedOptions ));
    CB_Wrap.Checked := ( Wrap and ( not myRestrictedOptions ));
    Combo_Text.Text := Pattern;
    Combo_Text.SelectAll;
  end;
end; // OptionsToForm

procedure TForm_Find.FormToOptions;
begin
  with myFindOptions do
  begin
    HiddenNodes:= CB_HiddenNodes.Checked;  // [dpv]
    AllNodes := CB_AllNodes.Checked;
    AllTabs := CB_AllTabs.Checked;
    EntireScope := CB_EntireScope.Checked;
    MatchCase := CB_MatchCase.Checked;
    Wrap := CB_Wrap.Checked;
    WholeWordsOnly := ( CB_WholeWordsOnly.Enabled and CB_WholeWordsOnly.Checked );
    Pattern := Combo_Text.Text;
  end;
end; // FormToOptions

procedure TForm_Find.Combo_TextChange(Sender: TObject);
begin
  Button_Find.Enabled := ( Combo_Text.Text <> '' );
  CB_WholeWordsOnly.Enabled := IsWord( Combo_Text.Text );
end;

procedure TForm_Find.Button_FindClick(Sender: TObject);
begin
  if (( Combo_Text.Text <> '' ) and ( Combo_Text.Items.IndexOf( Combo_Text.Text ) < 0 )) then
    Combo_Text.Items.Insert( 0, Combo_Text.Text );
  FormToOptions;
  OK_Click := true;
  if assigned( FindEvent ) then
    FindEvent( self );
end;

procedure TForm_Find.Button_CancelClick(Sender: TObject);
begin
  OK_Click := false;
  Close;
end;

procedure TForm_Find.Button_FindPrevClick(Sender: TObject);
begin
  // myFindOptions.SearchDown := false;
  OK_Click := true;
end;

procedure TForm_Find.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ComboToHistory;
  if assigned( FormCloseEvent ) then
    FormCloseEvent( self );
end;

procedure TForm_Find.FormDeactivate(Sender: TObject);
begin
  if assigned( myNotifyProc ) then
    myNotifyProc( true );
end;


end.
