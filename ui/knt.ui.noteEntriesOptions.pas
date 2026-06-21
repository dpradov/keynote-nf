unit knt.ui.noteEntriesOptions;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2026 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
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
   Vcl.Buttons,
   Vcl.ExtCtrls,
   Vcl.Menus,
   TB97Ctls,
   RxPlacemnt,
   kn_Info,
   kn_Const,
   knt.ui.info,
   knt.model.note
   ;


type
  TForm_NoteEntriesOptions = class(TForm)
    btn_OK: TButton;
    btn_Cancel: TButton;
    FormPlacement: TFormPlacement;
    btn_Help: TButton;
    gbDisplay: TGroupBox;
    cEntryCont: TComboBox;
    cb_CompHd: TCheckBox;
    CB_DescOrd: TCheckBox;
    cb_HTags: TCheckBox;
    cb_HDate: TCheckBox;
    cb_HLine: TCheckBox;
    lbl10: TLabel;
    lbl7: TLabel;
    gbFilter: TGroupBox;
    cbType: TComboBox;
    Label3: TLabel;
    txtText: TEdit;
    chkWholeWords: TCheckBox;
    chkCaseSens: TCheckBox;
    txtTagsExcl: TEdit;
    cbTagFindMode: TComboBox;
    txtTagsIncl: TEdit;
    lbl4: TLabel;
    lbl9: TLabel;
    lbl8: TLabel;
    chkExcerpts: TCheckBox;
    chkTagsText: TCheckBox;
    chkHidden: TCheckBox;
    btnRestoreDef: TButton;
    chkEnabled: TCheckBox;
    chkResetSizes: TCheckBox;
    chkApplyAll: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
    procedure btn_OKClick(Sender: TObject);
    procedure btn_CancelClick(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt; var CallHelp: Boolean): Boolean;
    procedure btn_HelpClick(Sender: TObject);
    procedure txtTagsInclEnter(Sender: TObject);
    procedure txtTagsExclEnter(Sender: TObject);
    procedure cbTagFindModeChange(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure btnRestoreDefClick(Sender: TObject);
    procedure cEntryContChange(Sender: TObject);

  private
    { Private declarations }
    FPrevActiveControlChange: TNotifyEvent;
    procedure ScreenActiveControlChange(Sender: TObject);

    procedure OnChangeFindTagsInclIntrod(FindTags: TFindTags; FindTagsNotRegistered: string; txtTags: TEdit);
    procedure OnChangeFindTagsExclIntrod(FindTags: TFindTags; FindTagsNotRegistered: string; txtTags: TEdit);
    procedure OnEndFindTagsInclIntrod(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string; txtTags: TEdit);
    procedure OnEndFindTagsExclIntrod(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string; txtTags: TEdit);
    procedure ChangeFindInclToModeOR;

  protected
    procedure DoShow; override;

  public
    { Public declarations }
    Initializing : boolean;
    OK_Click : boolean;

    Panel: TNEntriesPanelBase;
    Customiz: TMEPanelCustomization;
    QueryLayout: boolean;
    HeaderChanged: boolean;
    EntryContChanged: boolean;
    OrderChanged: boolean;
    ForceApplyFilter: boolean;
    ResetSizes: boolean;


    destructor Destroy; override;

    procedure FormToProps;
    procedure PropsToForm;
  end;


implementation
uses
   gf_misc,
   kn_global,
   kn_Ini,
   knt.App,
   knt.ui.TagMng,
   knt.ui.tagSelector,
   knt.RS
  ;

{$R *.DFM}



procedure TForm_NoteEntriesOptions.FormCreate(Sender: TObject);
var
  cont: TContentInMultiEntryMode_Selectable;
  sm : TSearchMode;

begin
  Initializing := true;

  with FormPlacement do begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;

  for cont := low(cont) to high(cont) do
     cEntryCont.Items.Add(CONTENT_IN_MULTIENTRY_MODE[cont]);

  for sm := low( TSearchMode ) to high( TSearchMode ) do
     cbType.Items.Add( SEARCH_MODES[sm] );

  ResetSizes:= false;
  ShowHint := KeyOptions.ShowTooltips;

  cEntryCont.OnChange:= cEntryContChange;

  OK_Click := false;

  App.ApplyBiDiModeOnForm(Self);
end;
// CREATE

procedure TForm_NoteEntriesOptions.DoShow;
begin
  inherited;
  FPrevActiveControlChange := Screen.OnActiveControlChange;   // We chain this in case any other code already uses this global event.
  Screen.OnActiveControlChange := ScreenActiveControlChange;
end;


destructor TForm_NoteEntriesOptions.Destroy;
var
  LMethod: TNotifyEvent;
begin
  LMethod := ScreenActiveControlChange;
  if TMethod(Screen.OnActiveControlChange).Code = TMethod(LMethod).Code then
     Screen.OnActiveControlChange := FPrevActiveControlChange;
  inherited;
end;

procedure TForm_NoteEntriesOptions.ScreenActiveControlChange(Sender: TObject);
begin
  if Assigned(FPrevActiveControlChange) then
     FPrevActiveControlChange(Sender);

  if (Screen.ActiveForm <> Self) then exit;

  btn_OK.Default:= (Screen.ActiveControl <> txtTagsIncl) and (Screen.ActiveControl <> txtTagsExcl);
end;

function TForm_NoteEntriesOptions.FormHelp(Command: Word; Data: NativeInt; var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;


procedure TForm_NoteEntriesOptions.FormActivate(Sender: TObject);
var
  tabName: string;
  strPanel, strLayout: string;

begin
  OnActivate := nil;
  if ( not Initializing ) then exit;
  App.SetTopMost(Handle, True);

  ModalFormWithTxtTagsVisible:= true;

  strPanel:= ENTRIES_PANELS[Panel];
  if QueryLayout then
     strLayout:= GetRS(sEntry18)
  else
     strLayout:= GetRS(sEntry19);

  Caption:= Format(Caption, [strPanel, strLayout]);
  chkResetSizes.Hint:= Format(chkResetSizes.Hint, [strLayout]);
  btnRestoreDef.Hint:= Format(btnRestoreDef.Hint, [strPanel, strLayout]);

  PropsToForm;
  if txtTagsIncl.CanFocus then
     txtTagsIncl.SetFocus;

  Initializing := false;
end; // ACTIVATE


procedure TForm_NoteEntriesOptions.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if OK_Click then begin
     OK_Click := false;

     FormToProps;
     ModalFormWithTxtTagsVisible:= false;
  end;
  OK_Click := false;
end;


procedure TForm_NoteEntriesOptions.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

  case key of
    27 : if (( shift = [] ) and ( not
      ( cEntryCont.DroppedDown or cbType.DroppedDown or cbTagFindMode.DroppedDown ))) then
    begin
      key := 0;
      if IntroducingTagsState = itWithTagSelector then begin
         IgnoreSelectorForTagSubsr := cTagSelector.SelectedTagName;
         cTagSelector.CloseTagSelector(false);
      end
      else begin
         OK_Click := false;
         Close;
      end;
    end;
  end;

end; // KEY DOWN


procedure TForm_NoteEntriesOptions.FormToProps;
begin

  with Customiz do begin
     HeaderChanged:= (ShowLineInHeader <> cb_HLine.Checked) or (ShowTagsInHeader <> cb_HTags.Checked) or (ShowDateInHeader <> cb_HDate.Checked) or (CompactHeader <> cb_CompHd.Checked);
     OrderChanged:=  (DescendingOrder <> CB_DescOrd.Checked);

     Content:=          TContentInMultiEntryMode(cEntryCont.ItemIndex+1);
     ShowLineInHeader:= cb_HLine.Checked;
     ShowTagsInHeader:= cb_HTags.Checked;
     ShowDateInHeader:= cb_HDate.Checked;
     DescendingOrder:=  CB_DescOrd.Checked;
     CompactHeader:=    cb_CompHd.Checked;

     Filter.Enabled:= chkEnabled.Checked;
     Filter.TagsText:= chkTagsText.Checked;
     Filter.TagsModeOR:= (cbTagFindMode.ItemIndex = 1);
     Filter.TextFilter:= txtText.Text;
     Filter.SearchMode:= TSearchMode(cbType.ItemIndex);
     Filter.MatchCase:=  chkCaseSens.Checked;
     Filter.WholeWordsOnly:= chkWholeWords.Checked;
     Filter.ConsiderHidden:= chkHidden.Checked;
     Filter.ShowExcerpts:= chkExcerpts.Checked;
     if Filter.Empty then
        Filter.Enabled:= false;
  end;

  ResetSizes:= chkResetSizes.Checked;
end;


procedure TForm_NoteEntriesOptions.PropsToForm;
begin
  with Customiz do begin
     cEntryCont.ItemIndex:= Ord(Content)-1;
     cb_HLine.Checked:=   ShowLineInHeader;
     cb_HTags.Checked:=   ShowTagsInHeader;
     cb_HDate.Checked:=   ShowDateInHeader;
     CB_DescOrd.Checked:= DescendingOrder;
     cb_CompHd.Checked:=  CompactHeader;

     chkEnabled.Checked:= (Filter.Enabled or Filter.Empty);
     if Filter.TagsModeOR then
        cbTagFindMode.ItemIndex:= 1
     else
        cbTagFindMode.ItemIndex:= 0;
     txtTagsIncl.Text:= TNoteTagArrayUtils.ToNames( TNoteTagArrayUtils.FindTagsANDToTags(Filter.FindTagsIncl) );
     txtTagsExcl.Text:= TNoteTagArrayUtils.ToNames( TNoteTagArrayUtils.FindTagsANDToTags(Filter.FindTagsExcl) );
     chkTagsText.Checked:= Filter.TagsText;
     txtText.Text:= Filter.TextFilter;
     cbType.ItemIndex:= Ord(Filter.SearchMode);
     chkCaseSens.Checked:=   Filter.MatchCase;
     chkWholeWords.Checked:= Filter.WholeWordsOnly;
     chkHidden.Checked:=     Filter.ConsiderHidden;
     chkExcerpts.Checked:=   Filter.ShowExcerpts;
  end;

end;


procedure TForm_NoteEntriesOptions.btn_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;


procedure TForm_NoteEntriesOptions.btn_CancelClick(Sender: TObject);
begin
  OK_Click := false;
end;

procedure TForm_NoteEntriesOptions.btn_HelpClick(Sender: TObject);
begin
  Application.HelpCommand( HELP_CONTEXT, HelpContext );
end;



procedure TForm_NoteEntriesOptions.chkEnabledClick(Sender: TObject);
var
  Enable: boolean;
begin
  Enable:= chkEnabled.Checked;

  txtTagsIncl.Enabled:= Enable;
  txtTagsExcl.Enabled:= Enable;
  cbTagFindMode.Enabled:= Enable;
  chkTagsText.Enabled:= Enable;

  txtText.Enabled:= Enable;
  chkWholeWords.Enabled:= Enable;
  chkCaseSens.Enabled:= Enable;
  cbType.Enabled:= Enable;

  chkExcerpts.Enabled:= Enable;
  chkHidden.Enabled:= Enable;

  if not Initializing then begin
     ForceApplyFilter:= True;
     if CtrlDown and not Enable then
        chkEnabled.Checked:= True;
  end;
end;


procedure TForm_NoteEntriesOptions.ChangeFindInclToModeOR;
begin
   with Customiz.Filter do begin
      FindTagsIncl:= TNoteTagArrayUtils.FindTagsToModeOR(FindTagsIncl);
      TagMng.UpdateTxtFindTagsHint(txtTagsIncl, txtTagsIncl.Text, FindTagsIncl, '', True);
   end;
end;


procedure TForm_NoteEntriesOptions.cbTagFindModeChange(Sender: TObject);
begin
   if Customiz.Filter.FindTagsIncl = nil then exit;
   if (cbTagFindMode.ItemIndex = 1) then
      ChangeFindInclToModeOR
   else begin
      // Ensure that the tags are interpreted according to ALL mode:
      txtTagsIncl.SetFocus;
      cbTagFindMode.SetFocus;
   end;
end;


procedure TForm_NoteEntriesOptions.txtTagsInclEnter(Sender: TObject);
begin
   if CtrlDown then begin
       txtTagsIncl.Text:= '';
       Customiz.Filter.FindTagsIncl:= nil;
   end;

   btn_OK.Default:= False;
   TagMng.StartTxtFindTagIntrod(txtTagsIncl, OnEndFindTagsInclIntrod, OnChangeFindTagsInclIntrod, false);
end;

procedure TForm_NoteEntriesOptions.OnChangeFindTagsInclIntrod(FindTags: TFindTags; FindTagsNotRegistered: string; txtTags: TEdit);
begin
   if cbTagFindMode.ItemIndex = 1 then
      ChangeFindInclToModeOR
   else
      Customiz.Filter.FindTagsIncl:= FindTags;
end;

procedure TForm_NoteEntriesOptions.OnEndFindTagsInclIntrod(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string; txtTags: TEdit);
begin
   OnChangeFindTagsInclIntrod(FindTags, FindTagsNotRegistered, txtTagsIncl);
   if PressedReturn then begin
      txtText.SetFocus;
      btn_OK.Default:= True;
   end;

   if txtTagsIncl.Focused then
      txtTagsInclEnter(nil);
end;


procedure TForm_NoteEntriesOptions.txtTagsExclEnter(Sender: TObject);
begin
   if CtrlDown then begin
       txtTagsExcl.Text:= '';
       Customiz.Filter.FindTagsExcl:= nil;
   end;

   btn_OK.Default:= False;
   TagMng.StartTxtFindTagIntrod(txtTagsExcl, OnEndFindTagsExclIntrod, OnChangeFindTagsExclIntrod, false);
end;


procedure TForm_NoteEntriesOptions.OnChangeFindTagsExclIntrod(FindTags: TFindTags; FindTagsNotRegistered: string; txtTags: TEdit);
begin
   Customiz.Filter.FindTagsExcl:= TNoteTagArrayUtils.FindTagsToModeOR(FindTags);
   TagMng.UpdateTxtFindTagsHint(txtTagsExcl, txtTagsExcl.Text, Customiz.Filter.FindTagsExcl, '', True);
end;



procedure TForm_NoteEntriesOptions.OnEndFindTagsExclIntrod(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string; txtTags: TEdit);
begin
   OnChangeFindTagsExclIntrod(FindTags, FindTagsNotRegistered, txtTagsExcl);
   if PressedReturn then begin
      txtText.SetFocus;
      btn_OK.Default:= True;
   end;

   if txtTagsExcl.Focused then
      txtTagsExclEnter(nil);
end;

procedure TForm_NoteEntriesOptions.cEntryContChange(Sender: TObject);
begin
   if Initializing then exit;

   EntryContChanged:= True;
end;


procedure TForm_NoteEntriesOptions.btnRestoreDefClick(Sender: TObject);
begin
   Customiz:= ActiveFolder.NoteAdvOptions.DefaultMECustomizForQL[Panel];
   PropsToForm;

   EntryContChanged:= True;
   OrderChanged:= True;
end;


end.
