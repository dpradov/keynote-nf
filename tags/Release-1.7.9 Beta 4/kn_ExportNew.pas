unit kn_ExportNew;
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

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, TreeNT, IniFiles,
  StdCtrls, Mask, ToolEdit, ExtCtrls, ComCtrls,
  kn_Info, kn_Const, kn_NoteObj, kn_NodeList,
  kn_Global,
  kn_FileObj, kn_Ini, Spin, ComCtrls95,
  gf_misc, gf_files, kn_RTFUtils, FileCtrl,
  kn_TabSelect, RxRichEd, RichEdit, Buttons,
  gf_strings, TntDialogs, TntStdCtrls, TntSysUtils, TB97Ctls, TntExtCtrls;

type
  TForm_ExportNew = class(TForm)
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    Button_Help: TTntButton;
    Pages: TPage95Control;
    Tab_Main: TTab95Sheet;
    Tab_Options: TTab95Sheet;
    Tab_TreePad: TTab95Sheet;
    GroupBox_Source: TTntGroupBox;
    RB_CurrentNote: TTntRadioButton;
    RB_AllNotes: TTntRadioButton;
    RB_SelectedNotes: TTntRadioButton;
    Button_Select: TTntButton;
    Combo_TreeSelection: TTntComboBox;
    GroupBox_Target: TTntGroupBox;
    Label1: TTntLabel;
    Label2: TTntLabel;
    Combo_Format: TTntComboBox;
    CheckBox_PromptOverwrite: TTntCheckBox;
    Edit_Folder: TTntEdit;
    CheckBox_Ask: TTntCheckBox;
    GroupBox1: TTntGroupBox;
    CB_IncNoteHeading: TTntCheckBox;
    CB_IncNodeHeading: TTntCheckBox;
    Edit_NodeHead: TTntComboBox;
    Edit_NoteHead: TTntComboBox;
    SaveDlg: TTntSaveDialog;
    RG_TreePadVersion: TTntRadioGroup;
    RG_TreePadMode: TTntRadioGroup;
    RG_NodeMode: TTntRadioGroup;
    Btn_TknHlp: TBitBtn;
    RG_TreePadMaster: TTntRadioGroup;
    CheckBox_ExcludeHiddenNodes: TTntCheckBox;
    TB_OpenDlgDir: TToolbarButton97;
    RG_HTML: TTntRadioGroup;
    procedure RG_HTMLClick(Sender: TObject);
    procedure TB_OpenDlgDirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RB_SelNotesClick(Sender: TObject);
    procedure Combo_FormatClick(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_SelectClick(Sender: TObject);
    procedure Btn_TknHlpClick(Sender: TObject);
    procedure Button_HelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    myNoteTree : TTreeNT;           
    myActiveNote : TTabNote;
    myNotes : TNoteFile;
    myINIFN : string;

    ExportOptions : TExportOptions;

    IsBusy : boolean;
    DoAbort : boolean;

    procedure ReadConfig;
    procedure WriteConfig;
    procedure PerformExport;
    procedure FormToOptions;
    procedure OptionsToForm;
    function Validate : boolean;

    function FlushExportFile( const RTF : TRxRichEdit; FN : wideString ) : boolean;
    procedure FlushTreePadData( var FileHandle : textfile; const Name : string; const Level : integer; const RTF : TRxRichEdit; const ClearRTF : boolean);
    function GetExportFilename( const FN : wideString ) : wideString;

    function ConfirmAbort : boolean;

  end;

function ExpandExpTokenString( const tpl, filename, notename, nodename : wideString; const nodelevel, nodeindex : integer ) : wideString;
function LoadRTFHeadingTemplate( const Filename : string ) : string;
function EscapeTextForRTF( const Txt : wideString ) : wideString;
function MergeHeadingWithRTFTemplate( const Heading, RTFTemplate : wideString ) : wideString;

procedure ExportNotesEx;
procedure ExportTreeNode;


var
  Form_ExportNew: TForm_ExportNew;

implementation
uses kn_Main, kn_DLLmng, kn_TreeNoteMng, WideStrings, TntSystem, TntClasses, TntFileCtrl,
     kn_ExportImport, gf_streams;



{$R *.DFM}

resourcestring
  STR_01 = 'Exporting is underway. OK to abort?';
  STR_02 = 'Please select a valid directory for exported files.';
  STR_03 = 'Specified output directory does not not exit. Please select a valid directory.';
  STR_04 = 'You did not select any notes for exporting.';
  STR_05 = 'Exported file: ';
  STR_06 = 'File description: ';
  STR_07 = 'File comment: ';
  STR_08 = 'Date created: ';
  STR_09 = 'Date exported: ';
  STR_10 = 'Exported note: ';
  STR_11 = 'Error while exporting notes: ';
  STR_12 = 'Exported  %d notes (%d nodes).';
  STR_13 = 'Exporting was aborted due to an error.';
  STR_14 = 'Exporting was aborted at user request.';
  STR_15 = 'The following token can be used in headings:' + #13#13 +
                  '%s%s - Filename'  + #13 +
                  '%s%s - Note name' + #13 +
                  '%s%s - Node name' + #13 +
                  '%s%s - Node level' + #13 +
                  '%s%s - Node index';
  STR_16 = 'No active tree node: select a node first.';
  STR_17 = 'Current node has no text: nothing to export.';
  STR_18 = ' Node exported to ';
  STR_19 = 'Error exporting node: ';


function ExpandExpTokenString(
  const tpl, filename, notename, nodename : wideString;
  const nodelevel, nodeindex : integer ) : wideString;
var
  i, len : integer;
  wastokenchar : boolean;
  thischar : wideChar;
begin
  result := '';
  len := length( tpl );

  wastokenchar := false;
  for i := 1 to len do
  begin
    thischar := tpl[i];
    case thischar of
      _TokenChar : begin
        if wastokenchar then
        begin
          result := result + _TokenChar;
          wastokenchar := false;
        end
        else
        begin
          wastokenchar := true;
        end;
      end;

      else
      begin
        if wastokenchar then
        begin
          wastokenchar := false;
          case thischar of
            EXP_NOTENAME : result := result + notename;
            EXP_NODENAME : result := result + nodename;
            EXP_NODELEVEL : if ( nodelevel > 0 ) then result := result + inttostr( nodelevel );
            EXP_NODEINDEX : if ( nodeindex > 0 ) then result := result + inttostr( nodeindex );
            EXP_FILENAME : result := result + WideExtractFilename( filename );
            else
            begin
              result := result + _TokenChar + thischar;
            end;
          end;
        end
        else
        begin
          result := result + thischar;
        end;
      end;
    end;
  end;

end; // ExpandExpTokenString

function MergeHeadingWithRTFTemplate( const Heading, RTFTemplate : wideString ) : wideString;
var
  p : integer;
begin
  result := RTFTemplate;
  p := pos( EXP_RTF_HEADING, result );
  if ( p > 0 ) then
  begin
    delete( result, p, length( EXP_RTF_HEADING ));
    insert( Heading, result, p );
  end
  else
  begin
    result := '';
  end;
end; // MergeHeadingWithRTFTemplate

function LoadRTFHeadingTemplate( const Filename : String ) : String;
var
  ls : TStringList;
begin
  result := '';
  if ( not Fileexists( Filename )) then exit;
  ls := TStringList.Create;
  try
    try
      ls.LoadFromFile( Filename );
      result := ls.Text;
    except
      result := '';
    end;
  finally
    ls.Free;
  end;
end; // LoadRTFHeadingTemplate

function EscapeTextForRTF( const Txt : wideString ) : wideString;
var
  i, len : integer;
  thisChar : wideChar;
begin
  result := '';
  len := length( Txt );
  for i := 1 to len do
  begin
    thisChar := Txt[i];
    case thisChar of
      '\', '{', '}', '<', '>' : begin
        result := result + '\' + thisChar;
      end;
      else
      begin
        result := result + thisChar;
      end;
    end;
  end;
end; // EscapeTextForRTF


procedure TForm_ExportNew.FormCreate(Sender: TObject);
var
  ts : TTreeSelection;
  f : TExportFmt;
  m : THTMLExportMethod;

begin

  Pages.ActivePage := Tab_Main;
  IsBusy := false;
  DoAbort := false;

  myNoteTree := nil;
  myActiveNote := nil;
  myNotes := nil;
  myINIFN := '';

  for f := low( TExportFmt ) to high( TExportFmt ) do
    Combo_Format.Items.Add( EXPORT_FORMAT_NAMES[f] );
  Combo_Format.ItemIndex := 0;

  for ts := low( ts ) to high( ts ) do
    Combo_TreeSelection.Items.Add( TREE_SELECTION_NAMES[ts] );
  Combo_TreeSelection.ItemIndex := 0;

  Edit_NodeHead.Items.Add( _DefaultNodeHeading );
  Edit_NoteHead.Items.Add( _DefaultNoteHeading );

  // Combo_IndentChar.ItemIndex := 0;

  for m := low( m ) to high( m ) do
  begin
     RG_HTML.Items.Add( HTMLExportMethods[m] );
  end;

  RG_HTML.ItemIndex:= ord(ExportOptions.HTMLExportMethod);
end; // CREATE

procedure TForm_ExportNew.FormActivate(Sender: TObject);
begin
  OnActivate := nil;

  ReadConfig;
  OptionsToForm;

  RB_SelNotesClick( RB_CurrentNote );
  RB_CurrentNote.OnClick := RB_SelNotesClick;
  RB_Allnotes.OnClick := RB_SelNotesClick;
  RB_SelectedNotes.OnClick := RB_SelNotesClick;

  Combo_FormatClick( Combo_Format );
  Combo_Format.OnClick := Combo_FormatClick;

end; // ACTIVATE


function TForm_ExportNew.ConfirmAbort : boolean;
begin
  result := ( messagedlg( STR_01, mtConfirmation, [mbOK,mbCancel], 0 ) = mrOK );
  DoAbort := result;
end; // ConfirmAbort

procedure TForm_ExportNew.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if IsBusy then
  begin
    CanClose := ConfirmAbort;
  end;
end; // CloseQuery

procedure TForm_ExportNew.RB_SelNotesClick(Sender: TObject);
begin
  Combo_TreeSelection.Enabled := ( RB_CurrentNote.Checked and ( myActiveNote.Kind = ntTree ) and ( TExportFmt( Combo_Format.ItemIndex ) in [xfPlainText, xfRTF, xfHTML] ));
  Button_Select.Enabled := RB_SelectedNotes.Checked;
end;

procedure TForm_ExportNew.Combo_FormatClick(Sender: TObject);
var
   format: TExportFmt;
begin
  Combo_TreeSelection.Enabled := ( RB_CurrentNote.Checked and ( myActiveNote.Kind = ntTree ) and ( TExportFmt( Combo_Format.ItemIndex ) in [xfPlainText, xfRTF, xfHTML] ));
  format:= TExportFmt( Combo_Format.ItemIndex );

  Tab_TreePad.TabVisible:=  (format  = xfTreePad);
  Tab_Options.TabVisible := (format <> xfTreePad);
  RG_NodeMode.Enabled :=    (format <> xfTreePad);
  RG_HTML.Visible := (format = xfHTML);
end;

procedure TForm_ExportNew.ReadConfig;
var
  IniFile : TWMemIniFile;
  section : string;
begin
  InitializeExportOptions( ExportOptions );
  if ( not fileexists( myINIFN )) then exit;

  IniFile := TWMemIniFile.Create( myIniFN );
  try
    with IniFile do
    begin
      section := ExportOptionsIniStr.section;
      ExportOptions.ConfirmFilenames := readbool( section, ExportOptionsIniStr.ConfirmFilenames, ExportOptions.ConfirmFilenames );
      ExportOptions.ConfirmOverwrite := readbool( section, ExportOptionsIniStr.ConfirmOverwrite, ExportOptions.ConfirmOverwrite );
      ExportOptions.ExportSource := TExportSource( readinteger( section, ExportOptionsIniStr.ExportSource, ord( ExportOptions.ExportSource )));
      ExportOptions.HTMLExportMethod := THTMLExportMethod(readinteger( section, ExportOptionsIniStr.HTMLExportMethod, ord(ExportOptions.HTMLExportMethod) ));
      ExportOptions.IncludeNodeHeadings := readbool( section, ExportOptionsIniStr.IncludeNodeHeadings, ExportOptions.IncludeNodeHeadings );
      ExportOptions.IncludeNoteHeadings := readbool( section, ExportOptionsIniStr.IncludeNoteHeadings, ExportOptions.IncludeNoteHeadings );
      ExportOptions.ExcludeHiddenNodes  := readbool( section, ExportOptionsIniStr.ExcludeHiddenNodes, ExportOptions.ExcludeHiddenNodes );  // [dpv]
      {
      ExportOptions.IndentNestedNodes := readbool( section, ExportOptionsIniStr.IndentNestedNodes, ExportOptions.IndentNestedNodes );
      ExportOptions.IndentUsingTabs := readbool( section, ExportOptionsIniStr.IndentUsingTabs, ExportOptions.IndentUsingTabs );
      ExportOptions.IndentValue := readinteger( section, ExportOptionsIniStr.IndentValue, ExportOptions.IndentValue );
      }
      ExportOptions.ExportPath := readstringW( section, ExportOptionsIniStr.ExportPath, ExportOptions.ExportPath );
      ExportOptions.NodeHeading := readstringW( section, ExportOptionsIniStr.NodeHeading, ExportOptions.NodeHeading );
      ExportOptions.NoteHeading := readstringW( section, ExportOptionsIniStr.NoteHeading, ExportOptions.NoteHeading );
      ExportOptions.SingleNodeFiles := readbool( section, ExportOptionsIniStr.SingleNodeFiles, ExportOptions.SingleNodeFiles );
      ExportOptions.TargetFormat := TExportFmt( readinteger( section, ExportOptionsIniStr.TargetFormat, ord( ExportOptions.TargetFormat )));
      ExportOptions.TreePadRTF := readbool( section, ExportOptionsIniStr.TreePadRTF, ExportOptions.TreePadRTF );
      ExportOptions.TreePadForceMaster := readbool( section, ExportOptionsIniStr.TreePadForceMaster, ExportOptions.TreePadForceMaster );
      ExportOptions.TreePadSingleFile := readbool( section, ExportOptionsIniStr.TreePadSingleFile, ExportOptions.TreePadSingleFile );
      ExportOptions.TreeSelection := TTreeSelection( readinteger( section, ExportOptionsIniStr.TreeSelection, ord( ExportOptions.TreeSelection )));
    end;
  finally
    IniFile.Free;
  end;

end;

procedure TForm_ExportNew.RG_HTMLClick(Sender: TObject);
begin
    ExportOptions.HTMLExportMethod := THTMLExportMethod(RG_HTML.ItemIndex);
end;

procedure TForm_ExportNew.TB_OpenDlgDirClick(Sender: TObject);
var
  Dir: wideString;
begin
  if WideSelectDirectory('','', Dir) then
     Edit_Folder.Text:= Dir;

  TB_OpenDlgDir.Down:= false;
end;

// ReadConfig

procedure TForm_ExportNew.WriteConfig;
var
  IniFile : TWMemIniFile;
  section : string;
begin
  IniFile := TWMemIniFile.Create( myIniFN );
  try
    with IniFile do
    begin
      section := ExportOptionsIniStr.section;
      writebool( section, ExportOptionsIniStr.ConfirmFilenames, ExportOptions.ConfirmFilenames );
      writebool( section, ExportOptionsIniStr.ConfirmOverwrite, ExportOptions.ConfirmOverwrite );
      writeinteger( section, ExportOptionsIniStr.ExportSource, ord( ExportOptions.ExportSource ));
      writeinteger( section, ExportOptionsIniStr.HTMLExportMethod, ord(ExportOptions.HTMLExportMethod) );
      writebool( section, ExportOptionsIniStr.IncludeNodeHeadings, ExportOptions.IncludeNodeHeadings );
      writebool( section, ExportOptionsIniStr.IncludeNoteHeadings, ExportOptions.IncludeNoteHeadings );
      writebool( section, ExportOptionsIniStr.ExcludeHiddenNodes, ExportOptions.ExcludeHiddenNodes );  // [dpv]
      {
      writebool( section, ExportOptionsIniStr.IndentNestedNodes, ExportOptions.IndentNestedNodes );
      writebool( section, ExportOptionsIniStr.IndentUsingTabs, ExportOptions.IndentUsingTabs );
      writeinteger( section, ExportOptionsIniStr.IndentValue, ExportOptions.IndentValue );
      }
      writestringW( section, ExportOptionsIniStr.ExportPath, ExportOptions.ExportPath );
      writestringW( section, ExportOptionsIniStr.NodeHeading, ExportOptions.NodeHeading );
      writestringW( section, ExportOptionsIniStr.NoteHeading, ExportOptions.NoteHeading );
      writebool( section, ExportOptionsIniStr.SingleNodeFiles, ExportOptions.SingleNodeFiles );
      writeinteger( section, ExportOptionsIniStr.TargetFormat, ord( ExportOptions.TargetFormat ));
      writebool( section, ExportOptionsIniStr.TreePadForceMaster, ExportOptions.TreePadForceMaster );
      writebool( section, ExportOptionsIniStr.TreePadRTF, ExportOptions.TreePadRTF );
      writebool( section, ExportOptionsIniStr.TreePadSingleFile, ExportOptions.TreePadSingleFile );
      writeinteger( section, ExportOptionsIniStr.TreeSelection, ord( ExportOptions.TreeSelection ));
    end;
    IniFile.UpdateFile;

  finally
    IniFile.Free;
  end;
end; // WriteConfig

procedure TForm_ExportNew.FormToOptions;
begin

  with ExportOptions do
  begin

    if RB_CurrentNote.Checked then
      ExportSource := expCurrentNote
    else
    if RB_AllNotes.Checked then
      ExportSource := expAllNotes
    else
      ExportSource := expSelectedNotes;

    ExcludeHiddenNodes:= CheckBox_ExcludeHiddenNodes.Checked;

    TreeSelection := TTreeSelection( Combo_TreeSelection.ItemIndex );
    TargetFormat := TExportFmt( Combo_Format.ItemIndex );
    SingleNodeFiles := ( RG_NodeMode.ItemIndex > 0 );
    HTMLExportMethod := THTMLExportMethod(RG_HTML.ItemIndex);

    ExportPath := ProperFolderName( Edit_Folder.Text );
    ConfirmOverwrite := CheckBox_PromptOverwrite.Checked;
    ConfirmFilenames := CheckBox_Ask.Checked;

    IncludeNodeHeadings := CB_IncNodeHeading.Checked;
    IncludeNoteHeadings := CB_IncNoteHeading.Checked;

    NodeHeading := Edit_NodeHead.Text;
    if ( NodeHeading = '' ) then
      NodeHeading := _TokenChar + EXP_NODENAME;
    NoteHeading := Edit_NoteHead.Text;
    if ( NoteHeading = '' ) then
      NoteHeading := _TokenChar + EXP_NOTENAME;

    {
    IndentNestedNodes := CB_IndentNodes.Checked;
    IndentValue := Spin_Indent.Value;
    IndentUsingTabs := ( Combo_IndentChar.ItemIndex > 0 );
    }

    TreePadRTF := ( RG_TreePadVersion.ItemIndex > 0 );
    TreePadSingleFile := ( RG_TreePadMode.ItemIndex > 0 );
    TreePadForceMaster := ( RG_TreePadMaster.ItemIndex > 0 );

  end;
end; // FormToOptions

procedure TForm_ExportNew.OptionsToForm;
begin
  with ExportOptions do
  begin
    case ExportSource of
      expCurrentNote : RB_CurrentNote.Checked := true;
      expAllNotes : RB_AllNotes.Checked := true;
      expSelectedNotes : RB_SelectedNotes.Checked := true;
    end;

    CheckBox_ExcludeHiddenNodes.Checked:= ExcludeHiddenNodes;

    Combo_TreeSelection.ItemIndex := ord( TreeSelection );
    Combo_Format.ItemIndex := ord( TargetFormat );
    if SingleNodeFiles then
      RG_NodeMode.ItemIndex := 1
    else
      RG_NodeMode.ItemIndex := 0;
    RG_HTML.ItemIndex := ord(HTMLExportMethod);
    Edit_Folder.Text := ExportPath;
    CheckBox_PromptOverwrite.Checked := ConfirmOverwrite;
    CheckBox_Ask.Checked := ConfirmFilenames;

    CB_IncNodeHeading.Checked := IncludeNodeHeadings;
    CB_IncNoteHeading.Checked := IncludeNoteHeadings;
    Edit_NodeHead.Text := NodeHeading;
    Edit_NoteHead.Text := NoteHeading;

    {
    CB_IndentNodes.Checked := IndentNestedNodes;
    Spin_Indent.Value := IndentValue;
    if IndentUsingTabs then
      Combo_IndentChar.ItemIndex := 1
    else
      Combo_IndentChar.ItemIndex := 0;
    }

    if TreePadRTF then
      RG_TreePadVersion.ItemIndex := 1
    else
      RG_TreePadVersion.ItemIndex := 0;

    if TreePadSingleFile then
      RG_TreePadMode.ItemIndex := 1
    else
      RG_TreePadMode.ItemIndex := 0;

    if TreePadForceMaster then
      RG_TreePadMaster.ItemIndex := 1
    else
      RG_TreePadMaster.ItemIndex := 0;

    {
    HTMLTemplateFN : string;
    HTMLWithFrames : boolean;
    HTMLWithTemplate : boolean;
    }
    // NodeLevelTemplate : string;

  end;
end; // OptionsToForm

procedure TForm_ExportNew.Button_OKClick(Sender: TObject);
begin
  PerformExport;
end;

function TForm_ExportNew.Validate : boolean;
begin
  result := false;
  if ( ExportOptions.ExportPath = '' ) then
  begin
    DoMessageBox( STR_02, mtError, [mbOK], 0 );
    exit;
  end;
  if ( not WideDirectoryExists( ExportOptions.ExportPath )) then
  begin
    DoMessageBox( STR_03, mtError, [mbOK], 0 );
    exit;
  end;
  result := true;
end; // Validate


procedure TForm_ExportNew.PerformExport;
var
  myNote : TTabNote;
  i, cnt, noteIdx : integer;
  RTFAux : TRxRichEdit;

  NoteHeading, NodeHeading : wideString;
  NoteHeadingRTF, NodeHeadingRTF : wideString;
  NoteHeadingTpl, NodeHeadingTpl : wideString;
  NodeText, ExitMessage : string;
  NodeTextSize : integer;
  StartLevel, ThisNodeIndex : integer;
  tmpStream : TTntMemoryStream;
  ExportedNotes, ExportedNodes, tmpExportedNodes : integer;
  WasError, WasAborted : boolean;
  StartTreeNode, myTreeNode : TTreeNTNode;
  myNoteNode : TNoteNode;
  TreePadFile : textfile;
  TreePadFN : wideString;
  TreePadNodeLevelInc : integer; // 0 or 1
  NodeStreamIsRTF : boolean;
begin

  FormToOptions;
  if ( not Validate ) then exit;
  WriteConfig;

  cnt := 0;
  for i := 1 to myNotes.Notes.Count do
  begin
    case ExportOptions.ExportSource of
      expCurrentNote : begin
        if ( myNotes.Notes[pred( i )] = myActiveNote ) then
          myNotes.Notes[pred( i )].Info := 1
        else
          myNotes.Notes[pred( i )].Info := 0;
      end;
      expAllNotes : begin
        myNotes.Notes[pred( i )].Info := 1;
      end;
      expSelectedNotes : begin
        // nothing, notes already tagged for exporting
      end;
    end;
    if ( myNotes.Notes[pred( i )].Info > 0 ) then
      inc( cnt );
  end;
  if ( cnt = 0 ) then
  begin
    messagedlg( STR_04, mtError, [mbOK], 0 );
    exit;
  end;

  IsBusy := true;
  Screen.Cursor := crHourGlass;
  DoAbort := false;
  ExportedNotes := 0;
  ExportedNodes := 0;
  NoteHeading := '';
  NodeHeading := '';
  WasError := false;
  WasAborted := false;
  NoteHeadingRTF := '';
  NodeHeadingRTF := '';
  StartLevel := 0;
  TreePadFN := '';
  TreePadNodeLevelInc := 0;
  RTFAux := TRxRichEdit.Create( ActiveNote.TabSheet);
  RTFAux.Visible:= False;
  RTFAux.Parent:=ActiveNote.TabSheet ;


  if ExportOptions.ConfirmOverwrite then
    SaveDlg.Options := [ofHideReadOnly,ofPathMustExist,ofOverwritePrompt]
  else
    SaveDlg.Options := [ofHideReadOnly,ofPathMustExist];
  SaveDlg.Filename := '';
  SaveDlg.InitialDir := ExportOptions.ExportPath;

  try
    try

      NoteHeadingTpl := LoadRTFHeadingTemplate( extractfilepath( application.exename ) + 'notehead.rtf' );
      if ( NoteHeadingTpl = '' ) then
        NoteHeadingTpl := _Default_NoteHeadingTpl;
      NodeHeadingTpl := LoadRTFHeadingTemplate( extractfilepath( application.exename ) + 'nodehead.rtf' );
      if ( NodeHeadingTpl = '' ) then
        NodeHeadingTpl := _Default_NodeHeadingTpl;

      for NoteIdx := 1 to myNotes.Notes.Count do
      begin
        Application.ProcessMessages;
        if DoAbort then break;

        myNote := myNotes.Notes[pred( NoteIdx )];
        if ( myNote.Info > 0 ) then
        begin
          // this note has been marked for exporting

          if ExportOptions.IncludeNoteHeadings then
          begin
            NoteHeading := ExpandExpTokenString( ExportOptions.NoteHeading, myNotes.Filename, RemoveAccelChar( myNote.Name ), '', 0, 0 );
            NoteHeadingRTF := MergeHeadingWithRTFTemplate( EscapeTextForRTF( NoteHeading ), NoteHeadingTpl );
          end;

          case ExportOptions.TargetFormat of
            xfPlainText, xfRTF, xfHTML : begin

              case myNote.Kind of

                ntRTF : begin

                  tmpStream := TTntMemoryStream.Create;
                  try
                    if not myNote.PlainText then
                       myNote.Editor.StreamFormat:= sfRichText
                    else begin
                       myNote.Editor.StreamFormat:= sfPlainText;
                       myNote.Editor.StreamMode := [smUnicode];
                    end;
                    myNote.Editor.Lines.SaveToStream( tmpStream );
                    tmpStream.Position := 0;
                    RTFAux.Lines.LoadFromStream( tmpStream );

                    if ( ExportOptions.IncludeNoteHeadings and ( NoteHeadingRTF <> '' )) then
                    begin
                      RTFAux.SelStart := 0;
                      PutRichTextW( NoteHeadingRTF, RTFAux, true, true );
                    end;

                    if FlushExportFile( RTFAux, RemoveAccelChar( myNote.Name )) then
                    begin
                      inc( ExportedNotes );
                    end;

                  finally
                    tmpStream.Free;
                  end;

                end;

                ntTree : begin
                  myNote.EditorToDataStream; // must flush contents of richedit to active node's internal stream

                  if (( ExportOptions.ExportSource = expCurrentNote ) and
                      ( ExportOptions.TreeSelection in [tsNode, tsSubtree] )) then
                    myTreeNode := TTreeNote( myNote ).TV.Selected
                  else
                    myTreeNode := TTreeNote( myNote ).TV.Items.GetFirstNode;

                  StartTreeNode := myTreeNode;
                  ThisNodeIndex := 1;

                  if assigned( myTreeNode ) then
                  begin
                    StartLevel := myTreeNode.Level;
                    inc( ExportedNotes );
                  end
                  else
                  begin
                    continue; // could not access starting node - perhaps tree has none
                  end;
                  tmpExportedNodes := 0;

                  while assigned( myTreeNode ) do
                  begin
                    myNoteNode := TNoteNode( myTreeNode.Data );

                    // check if we should export this node
                    if  (not myTreeNode.Hidden or not ExportOptions.ExcludeHiddenNodes) and   // [dpv]
                        (( ExportOptions.ExportSource <> expCurrentNote ) or
                        ( ExportOptions.TreeSelection in [tsNode, tsFullTree] ) or
                        (( ExportOptions.TreeSelection = tsCheckedNodes ) and myNoteNode.Checked ) or
                        (( ExportOptions.TreeSelection = tsSubtree ) and ( IsAParentOf( StartTreeNode, myTreeNode ) or ( StartTreeNode = myTreeNode )))) then
                    begin
                      myNoteNode.Stream.Position := 0;
                      inc( ThisNodeIndex );

                      if ExportOptions.IncludeNodeHeadings then
                      begin
                        NodeHeading := ExpandExpTokenString( ExportOptions.NodeHeading, myNotes.Filename, RemoveAccelChar( myNote.Name ), myNoteNode.Name, myNoteNode.Level, ThisNodeIndex );
                        NodeHeadingRTF := MergeHeadingWithRTFTemplate( EscapeTextForRTF( NodeHeading ), NodeHeadingTpl );
                      end;

                      case ExportOptions.SingleNodeFiles of
                        false : begin
                          // nodes are gathered into a single file
                          NodeTextSize := myNoteNode.Stream.Size;
                          SetLength( NodeText, NodeTextSize );
                          // transfer stream contents to temp string
                          move( myNoteNode.Stream.Memory^, NodeText[1], NodeTextSize );

                          // now for some treachery. In KeyNote, a user can mark a note
                          // as "plain text only". In such a node, all nodes are stored as
                          // plain text, not RTF. However, the change from RTF to text (or
                          // back) occurs only when a node is DISPLAYED. So, it is possible
                          // that user enabled "plain text only", but some tree nodes have
                          // not been viewed, hence are still in RTF. So, at this point
                          // we cannot know if the node data we're about to export is RTF
                          // or plain text data. Yet we must pass the correct information
                          // to PutRichText. Which is why we must check manually, like so:
                          NodeStreamIsRTF := ( copy( NodeText, 1, 6 ) = '{\rtf1' );

                          // now add the node data to temp RTF storage
                          if ( ExportOptions.IncludeNodeHeadings and ( NodeHeadingRTF <> '' )) then
                          begin
                            PutRichTextW( NodeHeadingRTF, RTFAux, true, false );
                          end;
                          PutRichText( NodeText, RTFAux, NodeStreamIsRTF, false ); // append to end of existing data
                          inc( tmpExportedNodes );
                        end;
                        true : begin
                          // each node is saved to its own file
                          // (Here we do not have to check if node stream is plain text or RTF,
                          // because LoadFromStream handles both cases automatically!)

                          RTFAux.Lines.LoadFromStream( myNoteNode.Stream );
                          if ( ExportOptions.IncludeNodeHeadings and ( NodeHeadingRTF <> '' )) then
                          begin
                            RTFAux.SelStart := 0;
                            PutRichTextW( NodeHeadingRTF, RTFAux, true, true );
                          end;
                          if FlushExportFile( RTFAux, myNoteNode.Name ) then
                          begin
                            inc( ExportedNodes );
                          end;
                        end;
                      end; // case ExportOptions.SingleNodeFiles
                    end;

                    // access next node
                    myTreeNode := myTreeNode.GetNext;

                    Application.ProcessMessages;
                    if DoAbort then break;

                    // check break conditions
                    if ( ExportOptions.ExportSource = expCurrentNote ) then
                    begin
                      if ( not assigned( myTreeNode )) then
                        break;
                      case ExportOptions.TreeSelection of
                        tsNode : break;
                        tsSubtree : if ( myTreeNode.Level <= StartLevel ) then break;
                      end;
                    end;
                  end; // while assigned( myTreeNode ) do

                  // if exporting all nodes to one file, flush nodes now
                  if ( not ExportOptions.SingleNodeFiles ) then
                  begin
                    // we have gathered all nodes, now flush the file
                    if ( ExportOptions.IncludeNoteHeadings and ( NoteHeadingRTF <> '' )) then
                    begin
                      RTFAux.SelStart := 0;
                      PutRichTextW( NoteHeadingRTF, RTFAux, true, true );
                    end;
                    if FlushExportFile( RTFAux, RemoveAccelChar( myNote.Name )) then
                    begin
                      inc( ExportedNodes, tmpExportedNodes );
                    end;
                  end;
                end; // ntTree
              end; // case myNote.Kind of
            end;

            xfTreePad : begin

              Application.ProcessMessages;
              if DoAbort then break;

              // we need to create a new file if no file has been created yet,
              // or if each note is saved to a separate TreePad file
              if (( TreePadFN = '' ) or ( not ExportOptions.TreePadSingleFile )) then
              begin
                if ( ExportOptions.TreePadSingleFile and ( ExportOptions.ExportSource <> expCurrentNote )) then
                  TreePadFN := GetExportFilename( WideExtractFilename( myNotes.FileName )) // use file name
                else
                  TreePadFN := GetExportFilename( RemoveAccelChar( myNote.Name )); // use note name

                if DoAbort then break;
                if ( TreePadFN = '' ) then continue;

                assignfile( TreePadFile, TreePadFN );
                rewrite( TreePadFile );

                TreePadNodeLevelInc := 0; // set initially

                try

                  // write TreePad header
                  if ExportOptions.TreePadRTF then
                    writeln( TreePadFile, _TREEPAD_HEADER_RTF ) // version 4.0
                  else
                    writeln( TreePadFile, _TREEPAD_HEADER_TXT ); // version 2.7

                  // TreePad allows only 1 "top level node", i.e. node with level 0.
                  // KeyNote has no such limit. So, we must, in most cases, create
                  // a dummy top level node, and put all other nodes as its children.
                  // This is not necessary only when exporting one non-tree note
                  // to a Treepad file (essentially, the TreePad file will only have
                  // one node.) We optimize this further by checking if the current
                  // note we're exporting conforms to TreePad's restriction (has only one
                  // top-level node) - if so, we don't need the dummy node. user can also
                  // force the creation of the dummy top-level node.
                  if ( ExportOptions.TreePadForceMaster or
                     ((( ExportOptions.TreePadSingleFile and ( ExportOptions.ExportSource <> expCurrentNote )) or
                     (( myNote.Kind = ntTree ) and ( TTreeNote( myNote ).TV.Items.GetFirstNode <> nil ) and ( TTreeNote( myNote ).TV.Items.GetFirstNode.GetNextSibling <> nil ))))) then
                  begin
                    // create a dummy top-level node

                    TreePadNodeLevelInc := 1; // must increase KeyNote's node levels by 1,
                    // because level 0 is the dummy node

                    writeln( TreePadFile, _TREEPAD_NODE ); // <node>
                    if ( ExportOptions.TreePadForceMaster or ( ExportOptions.TreePadSingleFile and ( ExportOptions.ExportSource <> expCurrentNote ))) then
                      writeln( TreePadFile, WideExtractFilename( myNotes.Filename )) // Node title is filename
                    else
                      writeln( TreePadFile, RemoveAccelChar( myNote.Name )); // Node title is note name
                    writeln( TreePadFile, '0' ); // top node level

                    // some node text:
                    writeln( TreePadFile, STR_05, WideExtractFilename( myNotes.Filename ));
                    if ( myNotes.Description <> '' ) then
                      writeln( TreePadFile, STR_06, myNotes.Description );
                    if ( myNotes.Comment <> '' ) then
                      writeln( TreePadFile, STR_07, myNotes.Comment );
                    writeln( TreePadFile, STR_08, DateTimeToStr( myNotes.DateCreated ));
                    writeln( TreePadFile, STR_09, DateTimeToStr( now ));
                    if ( not ExportOptions.TreePadSingleFile ) then
                      writeln( TreePadFile, STR_10, RemoveAccelChar( myNote.Name ));

                    writeln( TreePadFile, _TREEPAD_ENDNODE );

                  end;

                finally
                  // don't REALLY need to close, but if we get an exception
                  // in the code above, let's make sure the file is closed
                  closefile( TreePadFile );
                end;
              end;

              // we already have a file at this point - reopen it
              append( TreePadFile );

              try

                // export the whole note to the file.
                // At this point we only need to check the kind of note

                case myNote.Kind of
                  ntRTF : begin
                    FlushTreePadData( TreePadFile, RemoveAccelChar( myNote.Name ), TreePadNodeLevelInc, myNote.Editor, false );
                    inc( ExportedNotes );
                  end;

                  ntTree : begin
                    myNote.EditorToDataStream; // must flush contents of richedit to active node's internal stream
                    myTreeNode := TTreeNote( myNote ).TV.Items.GetFirstNode;

                    while assigned( myTreeNode ) do
                    begin
                      myNoteNode := TNoteNode( myTreeNode.Data );

                      myNoteNode.Stream.Position := 0;
                      RTFAux.Lines.LoadFromStream( myNoteNode.Stream );
                      FlushTreePadData( TreePadFile, myNoteNode.Name, myNoteNode.Level + TreePadNodeLevelInc, RTFAux, true );

                      inc( ExportedNodes );

                      myTreeNode := myTreeNode.GetNext;
                    end;

                    inc( ExportedNotes );

                  end;
                end; // case myNote.Kind

              finally
                // close file after every note, because
                // we might break out of the loop at next iteration
                closefile( TreePadFile );
              end;

            end; // xfTreePad

          end; // case ExportOptions.TargetFormat

        end;
      end; // for NoteIdx := 1 to myNotes.Notes.Count do

    except
      on E : Exception do
      begin
        WasError := true;
        messagedlg( STR_11 + E.Message, mtError, [mbOK], 0 );
      end;
    end;

  finally
    FreeConvertLibrary;
    IsBusy := false;
    Screen.Cursor := crDefault;
    RTFAux.Free;
    ExitMessage := Format(
        STR_12,
        [ExportedNotes, ExportedNodes] );
    if WasError then
      ExitMessage := ExitMessage + #13 + STR_13
    else
    if DoAbort then
      ExitMessage := ExitMessage + #13 + STR_14;

    if ( messagedlg( ExitMessage, mtInformation, [mbOK,mbCancel], 0 ) <> mrOK ) then
      ModalResult := mrCancel; // close dialog box is Cancel clicked

  end;

end; // PerformExport

procedure TForm_ExportNew.FlushTreePadData(
  var FileHandle : textfile;
  const Name : string;
  const Level : integer;
  const RTF : TRxRichEdit;
  const ClearRTF : boolean );
var
  tmpStream : TMemoryStream;
  RTFText : string;
  StreamSize : integer;
begin
  if ExportOptions.TreePadRTF then
    writeln( FileHandle, _TREEPAD_RTFNODE )
  else
    writeln( FileHandle, _TREEPAD_TXTNODE );
  writeln( FileHandle, _TREEPAD_NODE ); // <node>
  writeln( FileHandle, Name );
  writeln( FileHandle, Level );

  tmpStream := TMemoryStream.Create;

  try
    if ( not ExportOptions.TreePadRTF ) then
      RTF.StreamFormat := sfPlainText;
    RTF.Lines.SaveToStream( tmpStream );
    StreamSize := tmpStream.Size;
    SetLength( RTFText, StreamSize );
    move( tmpStream.Memory^, RTFText[1], StreamSize );
    writeln( FileHandle, RTFText );
    writeln( FileHandle, _TREEPAD_ENDNODE );

  finally
    tmpStream.Free;
    if ClearRTF then begin
      RTF.Clear;
    end;
    RTF.StreamFormat := sfRichText;
  end;


end; // FlushTreePadData

function TForm_ExportNew.GetExportFilename( const FN : wideString ) : wideString;
var
  ext : string;
begin
  result := '';

  case ExportOptions.TargetFormat of
    xfPlainText : begin
      SaveDlg.Filter := FILTER_TEXTFILES;
      ext := '.txt';
    end;
    xfRTF : begin
      SaveDlg.Filter := FILTER_RTFFILES;
      ext := '.rtf';
    end;
    xfHTML : begin
      SaveDlg.Filter := FILTER_HTMLFILES;
      ext := '.html';
    end;
    xfTreePad : begin
      SaveDlg.Filter := FILTER_HJTFILES;
      ext := '.hjt';
    end;
    else
    begin
      SaveDlg.Filter := FILTER_ALLFILES;
      ext := '.txt';
    end;
  end;

  result := ExportOptions.ExportPath + WideChangefileext( MakeValidFilename( FN, [' '], MAX_FILENAME_LENGTH ), ext );

  if ( ExportOptions.ConfirmFilenames or ( ExportOptions.ConfirmOverwrite and WideFileexists( result ))) then
  begin
    SaveDlg.Filename := result;
    if SaveDlg.Execute then
    begin
      result := SaveDlg.Filename;
    end
    else
    begin
      result := '';
      ConfirmAbort; // if user clicked Cancel in the SaveDlg, ask if user wants to abort the whole process
    end;
  end;

end; // GetExportFilename

function TForm_ExportNew.FlushExportFile( const RTF : TRxRichEdit; FN : wideString ) : boolean;
var
  tmpStream : TMemoryStream;
  RTFText : string;
  StreamSize : integer;
  ext: string;
begin
  result := false;

  FN := GetExportFilename( FN + '.EXT' );
  if ( FN = '' ) then exit; // break condition

  try
    ext := Extractfileext( FN );

    case ExportOptions.TargetFormat of
      xfPlainText : begin
        if ( ext = '' ) then FN := FN + ext_TXT;
        RTF.StreamFormat := sfPlainText;
        if RTF.TextW <> string(RTF.TextW) then
           RTF.StreamMode := [smUnicode];
        RTF.Lines.SaveToFile( WideStringToUTF8(FN) );
        result := true;
      end;
      xfRTF : begin
        if ( ext = '' ) then FN := FN + ext_RTF;
        RTF.Lines.SaveToFile( WideStringToUTF8(FN) );
        result := true;
      end;
      xfHTML : begin
        if ( ext = '' ) then FN := FN + ext_HTML;
        Result:= ConvertRTFToHTML( FN, GetRichText( RTF, true, false), ExportOptions.HTMLExportMethod);
      end;
    end;

  finally
    RTF.Clear;
    RTF.StreamFormat := sfRichText;
  end;


end; // FlushExportFile


procedure TForm_ExportNew.Button_SelectClick(Sender: TObject);
var
  TabSel : TForm_SelectTab;
begin
  TabSel := TForm_SelectTab.Create( self );
  try
    TabSel.ShowHint := ShowHint;
    TabSel.myNotes := myNotes;
    if ( TabSel.ShowModal = mrOK ) then
    begin
      RB_SelectedNotes.Checked := true;
      RB_SelNotesClick( RB_SelectedNotes );
    end;
  finally
    TabSel.Free;
  end;
end; // BUTTON SELECT CLICK


procedure TForm_ExportNew.Btn_TknHlpClick(Sender: TObject);
begin
  messagedlg(format(STR_15,[_TokenChar,EXP_FILENAME,
                                   _TokenChar,EXP_NOTENAME,
                                   _TokenChar,EXP_NODENAME,
                                   _TokenChar,EXP_NODELEVEL,
                                   _TokenChar,EXP_NODEINDEX ]),
    mtInformation, [mbOK], 0 );
end;

procedure TForm_ExportNew.Button_HelpClick(Sender: TObject);
begin
  Application.HelpCommand( HELP_CONTEXT, HelpContext );
end;


procedure ExportTreeNode;
var
  myTreeNode : TTreeNTNode;
  myNoteNode : TNoteNode;
  oldFilter, ext, RTFText: string;
  ExportFN : wideString;
  exportformat : TExportFmt;
  RTFAux : TRxRichEdit;

  ExportSelectionOnly : boolean;
begin
  myTreeNode := GetCurrentTreeNode;
  if ( not assigned( myTreeNode )) then
  begin
    showmessage( STR_16 );
    exit;
  end;

  if ( ActiveNote.Editor.Lines.Count = 0 ) then
  begin
    showmessage( STR_17 );
    exit;
  end;

  ActiveNote.EditorToDataStream;

  // {N}
  ExportFN := MakeValidFileName( myTreeNode.Text, [' '], MAX_FILENAME_LENGTH );

  with Form_Main.SaveDlg do
  begin
    try
      oldFilter := Filter;
      Filter := FILTER_EXPORT;
      FilterIndex := LastExportFilterIndex;
      if ( KeyOptions.LastExportPath <> '' ) then
        InitialDir := KeyOptions.LastExportPath
      else
        InitialDir := GetFolderPath( fpPersonal );
      FileName := KeyOptions.LastExportPath + ExportFN;
      {
      case FilterIndex of
        1 : FileName := KeyOptions.LastExportPath + ExportFN + ext_RTF;
        3 : FileName := KeyOptions.LastExportPath + ExportFN + ext_HTML;
        else
          FileName := KeyOptions.LastExportPath + ExportFN + ext_TXT;
      end;
      }
      if ( not execute ) then exit;
      LastExportFilterIndex := FilterIndex;
    finally
      Filter := oldFilter;
    end;
  end;

  ExportFN := normalFN( Form_Main.SaveDlg.FileName );
  KeyOptions.LastExportPath := extractfilepath( ExportFN );
  ext := extractfileext( ExportFN );
  case LastExportFilterIndex of
    1 : begin
      exportformat := xfRTF;
      if ( ext = '' ) then
        ExportFN := ExportFN + ext_RTF;
    end;
    3 : begin
      exportformat := xfHTML;
      if ( ext = '' ) then
        ExportFN := ExportFN + ext_HTML;
    end;
    else
    begin
      exportformat := xfPlainText;
      if ( ext = '' ) then
        ExportFN := ExportFN + ext_TXT;
    end;
  end;

  myNoteNode := TNoteNode( myTreeNode.Data );
  myNoteNode.Stream.Position := 0;

  try

    case exportformat of
      xfRTF : begin
        myNoteNode.Stream.SaveToFile( exportFN );
      end;
      xfHTML : begin
        try
          ExportSelectionOnly := ( ActiveNote.Editor.SelLength > 0 );
          RTFText:= GetRichText( ActiveNote.Editor, true, ExportSelectionOnly);
          ConvertRTFToHTML( ExportFN, RTFText, htmlExpMicrosoftHTMLConverter);
        finally
          FreeConvertLibrary;
        end;
      end;
      xfPlainText : begin
        RTFAux := TRxRichEdit.Create( ActiveNote.TabSheet);
        RTFAux.Visible:= False;
        RTFAux.Parent:=ActiveNote.TabSheet ;

        try
          RTFAux.Lines.LoadFromStream( myNoteNode.Stream );
          RTFAux.StreamFormat := sfPlainText;
          if RTFAux.TextW <> string(RTFAux.TextW) then
             RTFAux.StreamMode := [smUnicode];
          RTFAux.Lines.SaveToFile( WideStringToUTF8(ExportFN) );
        finally
          RTFAux.Free;
        end;
      end;
    end;

    Form_Main.Statusbar.Panels[PANEL_HINT].Text := STR_18 + WideExtractFilename( ExportFN );

  except
    on E : Exception do begin
      messagedlg( STR_19 + E.Message, mtError, [mbOK], 0 );
    end;
  end;

end; // ExportTreeNode


procedure ExportNotesEx;
var
  Form_Export : TForm_ExportNew;
begin
  if ( not Form_Main.HaveNotes( true, true )) then exit;
  Form_Export := TForm_ExportNew.Create( Form_Main );
  try
    with Form_Export do
    begin
      ShowHint := KeyOptions.ShowTooltips;
      myActiveNote := ActiveNote;
      myNotes := NoteFile;
      myINIFN := INI_FN;
    end;

    if ( Form_Export.ShowModal = mrOK ) then
    begin
    end;

  finally
    Form_Export.Free;
  end;
end; // ExportNotesEx


end.
