unit kn_ExportNew;

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
   System.Classes,
   System.IniFiles,
   System.IOUtils,
   System.SysUtils,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.Buttons,
   Vcl.StdCtrls,
   Vcl.ExtCtrls,
   Vcl.ComCtrls,
   Vcl.FileCtrl,
   Vcl.Samples.Spin,

   TB97Ctls,
   ComCtrls95,
   TreeNT,

   gf_streams,
   kn_Info,
   kn_FileObj,
   kn_NoteObj
   ;


type
  TForm_ExportNew = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Button_Help: TButton;
    Pages: TPage95Control;
    Tab_Main: TTab95Sheet;
    Tab_Options: TTab95Sheet;
    Tab_TreePad: TTab95Sheet;
    GroupBox_Source: TGroupBox;
    RB_CurrentNote: TRadioButton;
    RB_AllNotes: TRadioButton;
    RB_SelectedNotes: TRadioButton;
    Button_Select: TButton;
    Combo_TreeSelection: TComboBox;
    GroupBox_Target: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Combo_Format: TComboBox;
    CheckBox_PromptOverwrite: TCheckBox;
    Edit_Folder: TEdit;
    CheckBox_Ask: TCheckBox;
    GroupBox1: TGroupBox;
    CB_IncNoteHeading: TCheckBox;
    CB_IncNodeHeading: TCheckBox;
    Edit_NodeHead: TComboBox;
    Edit_NoteHead: TComboBox;
    SaveDlg: TSaveDialog;
    RG_TreePadVersion: TRadioGroup;
    RG_TreePadMode: TRadioGroup;
    RG_NodeMode: TRadioGroup;
    RG_TreePadMaster: TRadioGroup;
    CheckBox_ExcludeHiddenNodes: TCheckBox;
    TB_OpenDlgDir: TToolbarButton97;
    RG_HTML: TRadioGroup;
    CB_LevelTemplates: TCheckBox;
    CB_FontSizes: TCheckBox;
    Edit_FontSizes: TEdit;
    Edit_LengthHeading: TEdit;
    Btn_TknHlp: TBitBtn;
    Edit_Symbols: TEdit;
    lblSymbols: TLabel;
    lblLength: TLabel;
    CB_IndentNodes: TCheckBox;
    Spin_Indent: TSpinEdit;
    lblIndent: TLabel;
    CB_UseTab: TCheckBox;
    GB_Additional: TGroupBox;
    CB_ShowHiddenMarkers: TCheckBox;
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
    procedure CB_FontSizesClick(Sender: TObject);
    procedure CB_IndentNodesClick(Sender: TObject);
    procedure CB_IncNodeHeadingClick(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure Edit_SymbolsExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    myNoteTree : TTreeNT;
    myKntFolder : TKntFolder;
    myKntFile : TKntFile;
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

    function FlushExportFile( const RTF : TTabRichEdit; myFolder: TKntFolder; FN : string ) : boolean;
    procedure FlushTreePadData( var tf : TTextfile; const Name : string; const Level : integer; const RTF : TTabRichEdit; const ClearRTF : boolean);
    function GetExportFilename( const FN : string ) : string;
    procedure PrepareRTFforPlainText (RTFAux : TTabRichEdit; TabSize: integer; RTFIndentValue: integer);

    function ConfirmAbort : boolean;

    function ExpandExpTokenString( const tpl, filename, folderName, nodename : string; const nodelevel, nodeindex : integer; TabSize: integer ) : string;
  end;


function LoadRTFHeadingTemplate( const Filename : string ) : string;
function EscapeTextForRTF( const Txt : string ) : string;
function MergeHeadingWithRTFTemplate( const Heading, RTFTemplate : string ) : string;

procedure ExportNotesEx;
procedure ExportTreeNode;


var
  Form_ExportNew: TForm_ExportNew;

implementation
uses
   RxRichEd,
   gf_misc,
   gf_files,  // Important. Needed to use TMemIniFileHelper (.ReadString, .WriteString)
   gf_strings,
   kn_RTFUtils,
   kn_Const,
   kn_Ini,
   kn_Global,
   kn_NodeList,
   kn_TreeNoteMng,
   kn_ExportImport,
   kn_TabSelect,
   kn_Main,
   kn_NoteFileMng,
   kn_EditorUtils
   ;

var
  LengthsHeading_Max, LengthsHeading_Inc, LengthsHeading_Min: integer;
  FontSizes_Max, FontSizes_Inc, FontSizes_Min: integer;

{$R *.DFM}

resourcestring
  STR_01 = 'Exporting is underway. OK to abort?';
  STR_02 = 'Please select a valid directory for exported files.';
  STR_03 = 'Specified output directory does not not exit. Please select a valid directory.';
  STR_04 = 'You did not select any notes for exporting.';
  STR_11 = 'Error while exporting notes: ';
  STR_12 = 'Exported  %d notes (%d nodes).';
  STR_13 = 'Exporting was aborted due to an error.';
  STR_14 = 'Exporting was aborted at user request.';
  STR_15 = 'The following token can be used in headings:' + #13#13 +
                  '%s%s - Filename'  + #13 +
                  '%s%s - Folder name' + #13 +
                  '%s%s - Node name' + #13 +
                  '%s%s - Node level' + #13 +
                  '%s%s - Node index'  + #13 +
                  '%s%s - Line break'  + #13 +
                  '%s%s - Symbols, increasing'  + #13 +
                  '%s%s - Symbols, decreasing' + #13#13 +
                  'F1 => More INFO and usage examples';

  STR_16 = 'No active tree node: select a node first.';
  STR_17 = 'Current node has no text: nothing to export.';
  STR_18 = ' Node exported to ';
  STR_19 = 'Error exporting node: ';
  STR_20 = '''Current node'' will be managed as ''Current node and subtree'' for KeyNote format'+ #13 +' Continue?';


procedure PrepareExportOptions (SymbolsRepetition, FontSizesInHeading: string);

   procedure GetIntegers (Str: string; var i1, i2, i3: integer);
   var
      p1,p2: integer;

   begin
      p1:= pos(',', Str, 1);
      p2:= pos(',', Str, p1+1);

      i1:= -1;
      i2:= -1;
      i3:= -1;
      if TryStrToInt(Trim(Copy(Str, 1, p1-1)), i1) then
        if TryStrToInt(Trim(Copy(Str, p1+1, p2-p1-1)), i2) then
            TryStrToInt(Trim(Copy(Str, p2+1)), i3);
   end;

begin
    GetIntegers(SymbolsRepetition, LengthsHeading_Max, LengthsHeading_Inc, LengthsHeading_Min);
    if (LengthsHeading_Max <0) or (LengthsHeading_Inc<0) or (LengthsHeading_Min<0) then
       LengthsHeading_Max:= -1;


    GetIntegers(FontSizesInHeading, FontSizes_Max, FontSizes_Inc, FontSizes_Min);
    if (FontSizes_Max<0) or (FontSizes_Inc<0) or (FontSizes_Min <0) then
       FontSizes_Max := -1;
end;


procedure TForm_ExportNew.Edit_SymbolsExit(Sender: TObject);
begin
  if Edit_Symbols.Text = '' then
     Edit_Symbols.Text:= '#';
end;

function TForm_ExportNew.ExpandExpTokenString(
  const tpl, filename, folderName, nodename : string;
  const nodelevel, nodeindex : integer;
  TabSize: integer ) : string;
var
  i, len : integer;
  wastokenchar : boolean;
  thischar : Char;
  LenSymbolsLine, NumTabsLevel: integer;
  foundLineBreak: boolean;


  function ExpandSymbolsLevel (token: char; headerLength: integer): string;
  var
    LenSymbolsLevel, I: integer;
    Symbol: char;
  begin
     if nodelevel = 0 then exit('');

     //  Calculate length (repetition) of symbols level
      if token = EXP_NODELEVELSYMB_INC then
         LenSymbolsLevel:= 1 + (1*(nodelevel-1))

      else begin
         if LengthsHeading_Max < 0 then exit('');
         LenSymbolsLevel:= LengthsHeading_Max - (LengthsHeading_Inc* (nodelevel-1));
         if LenSymbolsLevel < LengthsHeading_Min then
            LenSymbolsLevel:= LengthsHeading_Min;

         if ExportOptions.IndentNestedNodes and not ExportOptions.SingleNodeFiles then begin
            NumTabsLevel:= 1;
            if ExportOptions.TargetFormat = xfPlainText then
               NumTabsLevel:= ExportOptions.IndentValue div EditorOptions.IndentInc;
            Dec(LenSymbolsLevel, NumTabsLevel*TabSize * (nodelevel-1));
         end;

         Dec(LenSymbolsLevel, headerLength);
      end;

     // Determine symbol to use
     { The first element of LevelSymbols corresponds to node level 1 }
     if nodeLevel > Length(ExportOptions.SymbolsInHeading) then
        Symbol:= ExportOptions.SymbolsInHeading[Length(ExportOptions.SymbolsInHeading)]
     else
        Symbol:= ExportOptions.SymbolsInHeading[nodeLevel];

     Result:= StringOfChar(Symbol, LenSymbolsLevel);
  end;

begin
  result := '';
  len := length( tpl );
  LenSymbolsLine:= 0;
  foundLineBreak:= false;

  wastokenchar := false;

  for i := 1 to len do begin
      thischar := tpl[i];

      case thischar of
        _TokenChar : begin
          if wastokenchar then begin
            result := result + _TokenChar;
            wastokenchar := false;
          end
          else
             wastokenchar := true;
        end;

        else begin
          if wastokenchar then begin
              wastokenchar := false;
              case thischar of
                EXP_FOLDERNAME : result := result + folderName;
                EXP_NODENAME : result := result + nodename;
                EXP_NODELEVEL : if ( nodelevel > 0 ) then result := result + inttostr( nodelevel );
                EXP_NODEINDEX : if ( nodeindex > 0 ) then result := result + inttostr( nodeindex );
                EXP_NODELEVELSYMB_INC: result := result + ExpandSymbolsLevel(thisChar, -1);
                EXP_FILENAME : result := result + ExtractFilename( filename );
                EXP_LINE_BREAK: begin
                   foundLineBreak:= true;
                   LenSymbolsLine:= 0;
                   result := result + _TokenChar + thischar;
                end;
                else
                  result := result + _TokenChar + thischar;
              end;
          end
          else
             result := result + thischar;
        end;

        if not foundLineBreak then
           LenSymbolsLine:= Length(result)
    end;

  end;

  Result:= StringReplace(Result, _TokenChar + EXP_NODELEVELSYMB_DEC, ExpandSymbolsLevel(thisChar, LenSymbolsLine), [rfReplaceAll]);

end; // ExpandExpTokenString


function MergeHeadingWithRTFTemplate( const Heading, RTFTemplate : string ) : string;
var
  p : integer;
begin
  result := RTFTemplate;
  p := pos( EXP_RTF_HEADING, result );
  if ( p > 0 ) then begin
    delete( result, p, length( EXP_RTF_HEADING ));
    insert( StringReplace(Heading, _TokenChar + EXP_LINE_BREAK, '\par ', [rfReplaceAll]),  result, p );
  end
  else
     result := '';
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


function EscapeTextForRTF( const Txt : string ) : string;
var
  i, len : integer;
  thisChar : char;
begin
  result := '';
  len := length( Txt );
  for i := 1 to len do begin
    thisChar := Txt[i];
    case thisChar of
      '\', '{', '}', '<', '>' : begin
        result := result + '\' + thisChar;
      end;
      else
         result := result + thisChar;
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
  myKntFolder := nil;
  myKntFile := nil;
  myINIFN := '';

  for f := low( TExportFmt ) to high( TExportFmt ) do
     Combo_Format.Items.Add( EXPORT_FORMAT_NAMES[f] );
     
  Combo_Format.ItemIndex := 0;

  for ts := low( ts ) to high( ts ) do
    Combo_TreeSelection.Items.Add( TREE_SELECTION_NAMES[ts] );
  Combo_TreeSelection.ItemIndex := 0;

  Edit_NodeHead.Items.Add( _DefaultNodeHeading );
  Edit_NoteHead.Items.Add( _DefaultNoteHeading );

  for m := low( m ) to high( m ) do
     RG_HTML.Items.Add( HTMLExportMethods[m] );

  RG_HTML.ItemIndex:= ord(ExportOptions.HTMLExportMethod);
end;


function TForm_ExportNew.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;

   // -1 -> 313-9 : Exporting Notes to Disk Files [313] / 9: Available Tokens, and related options
   if (Command = HELP_CONTEXT) and (Data < 0) then
       Command:= HELP_COMMAND;
       case Data of
         -1: Data:= NativeInt(PChar('313-9'));
       end;

   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

// CREATE


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
     CanClose := ConfirmAbort;
end; // CloseQuery


procedure TForm_ExportNew.RB_SelNotesClick(Sender: TObject);
begin
  Combo_TreeSelection.Enabled := ( RB_CurrentNote.Checked
                                  and ( TExportFmt(Combo_Format.ItemIndex) in [xfPlainText, xfRTF, xfHTML, xfKeyNote] ));
  Button_Select.Enabled := RB_SelectedNotes.Checked;
end;


procedure TForm_ExportNew.CB_FontSizesClick(Sender: TObject);
begin
   Edit_FontSizes.Enabled:= CB_FontSizes.Checked;
end;


procedure TForm_ExportNew.CB_IncNodeHeadingClick(Sender: TObject);
var
   Enabled: boolean;
begin
   Enabled:= CB_IncNodeHeading.Checked;
   CB_LevelTemplates.Enabled:= Enabled;
   Edit_Symbols.Enabled:= Enabled;
   Edit_LengthHeading.Enabled:= Enabled;
   lblSymbols.Enabled:= Enabled;
   lblLength.Enabled:= Enabled;
end;

procedure TForm_ExportNew.CB_IndentNodesClick(Sender: TObject);
var
   Enabled: boolean;
begin
   Enabled:= CB_IndentNodes.Checked;
   Spin_Indent.Enabled:= Enabled;
   LblIndent.Enabled:= Enabled;
   CB_UseTab.Enabled:= Enabled;
end;

procedure TForm_ExportNew.Combo_FormatClick(Sender: TObject);
var
   format: TExportFmt;
begin
  Combo_TreeSelection.Enabled := ( RB_CurrentNote.Checked
                                  and ( TExportFmt(Combo_Format.ItemIndex) in [xfPlainText, xfRTF, xfHTML, xfKeyNote] ));
  format:= TExportFmt( Combo_Format.ItemIndex );

  Tab_TreePad.TabVisible:=  (format  = xfTreePad);
  if format = xfKeyNote then
     Tab_Options.TabVisible:= false

  else begin
     Tab_Options.TabVisible := (format <> xfTreePad);
     RG_NodeMode.Enabled :=    (format <> xfTreePad);
     RG_HTML.Visible := (format = xfHTML);
     GB_Additional.Visible := (format = xfPlainText);
     CB_UseTab.Enabled:= (format = xfPlainText) and (CB_IndentNodes.Checked);
  end;
end;


procedure TForm_ExportNew.ReadConfig;
var
  IniFile : TMemIniFile;
  section : string;
begin
  InitializeExportOptions( ExportOptions );
  if ( not fileexists( myINIFN )) then exit;

  IniFile := TMemIniFile.Create( myIniFN );
  try
    with IniFile do begin
      section := ExportOptionsIniStr.section;
      ExportOptions.ConfirmFilenames := readbool( section, ExportOptionsIniStr.ConfirmFilenames, ExportOptions.ConfirmFilenames );
      ExportOptions.ConfirmOverwrite := readbool( section, ExportOptionsIniStr.ConfirmOverwrite, ExportOptions.ConfirmOverwrite );
      ExportOptions.ExportSource := TExportSource( readinteger( section, ExportOptionsIniStr.ExportSource, ord( ExportOptions.ExportSource )));
      ExportOptions.HTMLExportMethod := THTMLExportMethod(readinteger( section, ExportOptionsIniStr.HTMLExportMethod, ord(ExportOptions.HTMLExportMethod) ));
      ExportOptions.IncludeNodeHeadings := readbool( section, ExportOptionsIniStr.IncludeNodeHeadings, ExportOptions.IncludeNodeHeadings );
      ExportOptions.IncludeNoteHeadings := readbool( section, ExportOptionsIniStr.IncludeNoteHeadings, ExportOptions.IncludeNoteHeadings );
      ExportOptions.ExcludeHiddenNodes  := readbool( section, ExportOptionsIniStr.ExcludeHiddenNodes, ExportOptions.ExcludeHiddenNodes );  // [dpv]
      ExportOptions.NodeLevelTemplates := readbool( section, ExportOptionsIniStr.UseLevelTemplates, ExportOptions.NodeLevelTemplates );
      ExportOptions.SymbolsInHeading := readstring( section, ExportOptionsIniStr.SymbolsInHeading, ExportOptions.SymbolsInHeading );
      ExportOptions.LengthHeading := readstring( section, ExportOptionsIniStr.LengthsHeading, ExportOptions.LengthHeading );
      if ExportOptions.SymbolsInHeading = '' then
         ExportOptions.SymbolsInHeading:= '#';

      ExportOptions.AutoFontSizesInHeading := readbool( section, ExportOptionsIniStr.AutoFontSizesInHeading, ExportOptions.AutoFontSizesInHeading );
      ExportOptions.FontSizesInHeading := readstring( section, ExportOptionsIniStr.FontSizesInHeading, ExportOptions.FontSizesInHeading );
      ExportOptions.IndentNestedNodes := readbool( section, ExportOptionsIniStr.IndentNestedNodes, ExportOptions.IndentNestedNodes );
      ExportOptions.IndentValue := readinteger( section, ExportOptionsIniStr.IndentValue, ExportOptions.IndentValue );
      ExportOptions.IndentUsingTabs := readbool( section, ExportOptionsIniStr.IndentUsingTabs, ExportOptions.IndentUsingTabs );
      ExportOptions.NumbTabInPlainText := readstring( section, ExportOptionsIniStr.NumbTabInPlainText, ExportOptions.NumbTabInPlainText );
      ExportOptions.ExportPath := readstring( section, ExportOptionsIniStr.ExportPath, ExportOptions.ExportPath );
      ExportOptions.NodeHeading := readstring( section, ExportOptionsIniStr.NodeHeading, ExportOptions.NodeHeading );
      ExportOptions.FolderHeading := readstring( section, ExportOptionsIniStr.NoteHeading, ExportOptions.FolderHeading );
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
  Dir: string;
begin
  if SelectDirectory('','', Dir) then
     Edit_Folder.Text:= Dir;

  TB_OpenDlgDir.Down:= false;
end;


// ReadConfig

procedure TForm_ExportNew.WriteConfig;
var
  IniFile : TMemIniFile;
  section : string;
  
begin
  IniFile := TMemIniFile.Create( myIniFN );
  try
    with IniFile do begin
      section := ExportOptionsIniStr.section;
      writebool( section, ExportOptionsIniStr.ConfirmFilenames, ExportOptions.ConfirmFilenames );
      writebool( section, ExportOptionsIniStr.ConfirmOverwrite, ExportOptions.ConfirmOverwrite );
      writeinteger( section, ExportOptionsIniStr.ExportSource, ord( ExportOptions.ExportSource ));
      writeinteger( section, ExportOptionsIniStr.HTMLExportMethod, ord(ExportOptions.HTMLExportMethod) );
      writebool( section, ExportOptionsIniStr.IncludeNodeHeadings, ExportOptions.IncludeNodeHeadings );
      writebool( section, ExportOptionsIniStr.IncludeNoteHeadings, ExportOptions.IncludeNoteHeadings );
      writebool( section, ExportOptionsIniStr.ExcludeHiddenNodes, ExportOptions.ExcludeHiddenNodes );  // [dpv]

      writebool( section, ExportOptionsIniStr.UseLevelTemplates, ExportOptions.NodeLevelTemplates );
      writestring( section, ExportOptionsIniStr.SymbolsInHeading, ExportOptions.SymbolsInHeading );
      writestring( section, ExportOptionsIniStr.LengthsHeading, ExportOptions.LengthHeading );
      writebool( section, ExportOptionsIniStr.AutoFontSizesInHeading, ExportOptions.AutoFontSizesInHeading );
      writestring( section, ExportOptionsIniStr.FontSizesInHeading, ExportOptions.FontSizesInHeading );
      writebool( section, ExportOptionsIniStr.IndentNestedNodes, ExportOptions.IndentNestedNodes );
      writeinteger( section, ExportOptionsIniStr.IndentValue, ExportOptions.IndentValue );
      writebool( section, ExportOptionsIniStr.IndentUsingTabs, ExportOptions.IndentUsingTabs );
      writestring( section, ExportOptionsIniStr.NumbTabInPlainText, '"' + ExportOptions.NumbTabInPlainText + '"');
      writestring( section, ExportOptionsIniStr.ExportPath, ExportOptions.ExportPath );
      writestring( section, ExportOptionsIniStr.NodeHeading, ExportOptions.NodeHeading );
      writestring( section, ExportOptionsIniStr.NoteHeading, ExportOptions.FolderHeading );
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

  with ExportOptions do begin

    if RB_CurrentNote.Checked then
      ExportSource := expCurrentNote
    else
    if RB_AllNotes.Checked then
      ExportSource := expAllNotes
    else
      ExportSource := expSelectedNotes;

    ExcludeHiddenNodes:= CheckBox_ExcludeHiddenNodes.Checked;
    ShowHiddenMarkers:= CB_ShowHiddenMarkers.Checked;

    TreeSelection := TTreeSelection( Combo_TreeSelection.ItemIndex );
    TargetFormat := TExportFmt( Combo_Format.ItemIndex );
    SingleNodeFiles := ( RG_NodeMode.ItemIndex > 0 );
    HTMLExportMethod := THTMLExportMethod(RG_HTML.ItemIndex);

    ExportPath := ProperFolderName( Edit_Folder.Text );
    ConfirmOverwrite := CheckBox_PromptOverwrite.Checked;
    ConfirmFilenames := CheckBox_Ask.Checked;

    IncludeNodeHeadings := CB_IncNodeHeading.Checked;
    IncludeNoteHeadings := CB_IncNoteHeading.Checked;

    SymbolsInHeading:= Edit_Symbols.Text;
    LengthHeading:= Edit_LengthHeading.Text;
    FontSizesInHeading:=  Edit_FontSizes.Text;
    NodeLevelTemplates:= CB_LevelTemplates.Checked;
    AutoFontSizesInHeading:= CB_FontSizes.Checked;
    IndentNestedNodes := CB_IndentNodes.Checked;
    IndentValue := Spin_Indent.Value;
    IndentUsingTabs := CB_UseTab.Checked;

    NodeHeading := Edit_NodeHead.Text;
    if ( NodeHeading = '' ) then
      NodeHeading := _TokenChar + EXP_NODENAME;
    FolderHeading := Edit_NoteHead.Text;
    if ( FolderHeading = '' ) then
      FolderHeading := _TokenChar + EXP_FOLDERNAME;

    TreePadRTF := ( RG_TreePadVersion.ItemIndex > 0 );
    TreePadSingleFile := ( RG_TreePadMode.ItemIndex > 0 );
    TreePadForceMaster := ( RG_TreePadMaster.ItemIndex > 0 );

  end;
end; // FormToOptions


procedure TForm_ExportNew.OptionsToForm;
begin
  with ExportOptions do begin
    case ExportSource of
      expCurrentNote : RB_CurrentNote.Checked := true;
      expAllNotes : RB_AllNotes.Checked := true;
      expSelectedNotes : RB_SelectedNotes.Checked := true;
    end;

    CheckBox_ExcludeHiddenNodes.Checked:= ExcludeHiddenNodes;
    CB_ShowHiddenMarkers.Checked:= ShowHiddenMarkers;

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
    Edit_NoteHead.Text := FolderHeading;

    Edit_Symbols.Text:= SymbolsInHeading;
    Edit_LengthHeading.Text:= LengthHeading;
    Edit_FontSizes.Text:= FontSizesInHeading;
    CB_LevelTemplates.Checked:= NodeLevelTemplates;
    CB_FontSizes.Checked:= AutoFontSizesInHeading;
    CB_IndentNodes.Checked := IndentNestedNodes;
    Spin_Indent.Value:= IndentValue;
    CB_UseTab.Checked:= IndentUsingTabs;

    CB_IndentNodesClick(nil);
    CB_IncNodeHeadingClick(nil);

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
  if ( ExportOptions.ExportPath = '' ) then begin
    DoMessageBox( STR_02, mtError, [mbOK], 0 );
    exit;
  end;

  if ( not System.SysUtils.DirectoryExists( ExportOptions.ExportPath )) then begin
    DoMessageBox( STR_03, mtError, [mbOK], 0 );
    exit;
  end;

  result := true;
end; // Validate


procedure TForm_ExportNew.PerformExport;
var
  myFolder : TKntFolder;
  i, cnt, noteIdx : integer;
  RTFAux : TTabRichEdit;

  NoteHeading, NodeHeading : string;
  NoteHeadingRTF, NodeHeadingRTF : string;
  NoteHeadingTpl, NodeHeadingTpl : string;
  NodeLevelHeadingTpl : array of string;
  NodeHeadingTpl_Aux: string;
  NodeText: AnsiString;
  ExitMessage : string;
  NodeTextSize : integer;
  StartLevel, ThisNodeIndex : integer;
  tmpStream : TMemoryStream;
  ExportedNotes, ExportedNodes, tmpExportedNodes : integer;
  WasError, WasAborted : boolean;
  StartTreeNode, myTreeNode : TTreeNTNode;
  myNote : TKntNote;
  TreePadFile : TTextfile;
  TreePadFN : string;
  TreePadNodeLevelInc : integer; // 0 or 1
  NodeStreamIsRTF : boolean;
  Encoding: TEncoding;
  FN, ext: string;
  OnlyNotHiddenNodes, OnlyCheckedNodes: boolean;
  ContainsImages: boolean;
  ContainsImgIDsRemoved: boolean;
  RTFwithImages: AnsiString;
  FSize, SS, SL: integer;
  SSNode: integer;

  procedure LoadNodeLevelTemplates;
  var
    i: integer;
    FileName_Aux, FileName: string;
    Tpl: string;
  begin
      SetLength(NodeLevelHeadingTpl, 10);
      FileName_Aux:= ExtractFilePath(INI_FN) + 'nodehead';
      for i:= 1 to 10 do begin
         FileName:= Format('%s_%d%s', [FileName_Aux, i, ext_RTF]);
         Tpl := LoadRTFHeadingTemplate(FileName);
         NodeLevelHeadingTpl[i-1]:= Tpl;
      end;
  end;

  procedure IndentContent (level: integer);
  var
     SS: integer;
  begin
     SS:= RTFAux.SelStart;
     RTFAux.SetSelection(SSNode, SS, true);
     RTFAux.Paragraph.FirstIndentRelative := ExportOptions.IndentValue * (level-1);
     RTFAux.SelStart:= SS;
  end;

begin
  FormToOptions;
  if ( not Validate ) then exit;
  WriteConfig;

  if (ExportOptions.TargetFormat= xfKeyNote) and (ExportOptions.TreeSelection = tsNode) and (Combo_TreeSelection.Enabled) then
      if ( messagedlg( STR_20, mtInformation, [mbOK,mbCancel], 0 ) <> mrOK ) then
         Exit;

  cnt := 0;
  for i := 1 to myKntFile.Folders.Count do begin
      myFolder:= myKntFile.Folders[pred(i)];

      case ExportOptions.ExportSource of
          expCurrentNote : begin
            if ( myFolder = myKntFolder ) then
              myFolder.Info := 1
            else
              myFolder.Info := 0;
          end;

          expAllNotes : begin
            myFolder.Info := 1;
          end;

          expSelectedNotes : begin
            // nothing, notes already tagged for exporting
          end;
      end;

      if ( myFolder.Info > 0 ) then
         inc( cnt );
  end;
  
  if ( cnt = 0 ) then begin
    messagedlg( STR_04, mtError, [mbOK], 0 );
    exit;
  end;

  IsBusy := true;
  FileIsBusy := true;             // To avoid that kn_Main.TimerTimer, at the beginning can interfere calling to KntFile.UpdateTextPlainVariables(); (it also uses the same RTFAux, with GetAuxEditorControl)
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

  RTFAux:= CreateRTFAuxEditorControl;


  if ExportOptions.ConfirmOverwrite then
    SaveDlg.Options := [ofHideReadOnly,ofPathMustExist,ofOverwritePrompt]
  else
    SaveDlg.Options := [ofHideReadOnly,ofPathMustExist];
  SaveDlg.Filename := '';
  SaveDlg.InitialDir := ExportOptions.ExportPath;

  try

    try

      if ExportOptions.TargetFormat = xfKeyNote then begin                           // --- xfKeyNote
         myTreeNode := nil;
         OnlyNotHiddenNodes:= ExportOptions.ExcludeHiddenNodes;
         OnlyCheckedNodes:= false;

         if (ExportOptions.ExportSource = expCurrentNote ) then
            if (ExportOptions.TreeSelection in [tsNode, tsSubtree]) then
               myTreeNode := ActiveKntFolder.TV.Selected;
            if (ExportOptions.TreeSelection = tsCheckedNodes) then
               OnlyCheckedNodes:= true;

         FN := GetExportFilename('Export_' + ExtractFileName(KntFile.FileName) );
         if FN <> ''  then begin
            ext := Extractfileext( FN );
            if (ext = '') then FN := FN + ext_KeyNote;
            KntFileCopy (ExportedNotes, ExportedNodes, FN, true, myTreeNode, OnlyNotHiddenNodes, OnlyCheckedNodes );
         end;

         exit;                                                                    // ------------
      end;

      // load general, default templates
      NoteHeadingTpl := LoadRTFHeadingTemplate( NoteHeadingTpl_FN);
      if ( NoteHeadingTpl = '' ) then
         NoteHeadingTpl := _Default_NoteHeadingTpl;
      NodeHeadingTpl := LoadRTFHeadingTemplate( NodeHeadingTpl_FN);
      if ( NodeHeadingTpl = '' ) then
         NodeHeadingTpl := _Default_NodeHeadingTpl;

      // load level templates, if any
      if ExportOptions.NodeLevelTemplates then
         LoadNodeLevelTemplates;

      PrepareExportOptions (ExportOptions.LengthHeading, ExportOptions.FontSizesInHeading);


      for NoteIdx := 1 to myKntFile.Folders.Count do begin             // ----------------------------------------------------------- FOR EACH NOTE :
        Application.ProcessMessages;
        if DoAbort then break;

        myFolder := myKntFile.Folders[pred( NoteIdx )];
        if ( myFolder.Info > 0 ) then begin
          // this note has been marked for exporting

          RTFAux.Clear;
          PrepareRTFAuxForPlainText(RTFAux, myFolder);

          if ExportOptions.IncludeNoteHeadings then begin
             NoteHeading := ExpandExpTokenString( ExportOptions.FolderHeading, myKntFile.Filename, RemoveAccelChar( myFolder.Name ), '', 0, 0, myFolder.TabSize );
             if ShowHiddenMarkers then
                NoteHeading:= Format('%s [%d]', [NoteHeading, myFolder.ID]);
             NoteHeadingRTF := MergeHeadingWithRTFTemplate( EscapeTextForRTF( NoteHeading ), NoteHeadingTpl );
          end;

          case ExportOptions.TargetFormat of
            xfPlainText, xfRTF, xfHTML : begin                                // =============================    xfPlainText, xfRTF, xfHTML

              myFolder.EditorToDataStream; // must flush contents of richedit to active node's internal stream

              if (( ExportOptions.ExportSource = expCurrentNote ) and
                  ( ExportOptions.TreeSelection in [tsNode, tsSubtree] )) then
                myTreeNode := myFolder.TV.Selected
              else
                myTreeNode := myFolder.TV.Items.GetFirstNode;

              StartTreeNode := myTreeNode;
              ThisNodeIndex := 0;

              if assigned( myTreeNode ) then begin
                StartLevel := myTreeNode.Level;
                inc( ExportedNotes );
              end
              else
                 continue; // could not access starting node - perhaps tree has none

              tmpExportedNodes := 0;

              while assigned( myTreeNode ) do begin          // ---------------------- Iterate each node
                myNote := TKntNote( myTreeNode.Data );

                // check if we should export this node
                if  (not myTreeNode.Hidden or not ExportOptions.ExcludeHiddenNodes) and   // [dpv]
                    (( ExportOptions.ExportSource <> expCurrentNote ) or
                    ( ExportOptions.TreeSelection in [tsNode, tsFullTree] ) or
                    (( ExportOptions.TreeSelection = tsCheckedNodes ) and myNote.Checked ) or
                    (( ExportOptions.TreeSelection = tsSubtree ) and ( IsAParentOf( StartTreeNode, myTreeNode ) or ( StartTreeNode = myTreeNode )))) then
                begin
                  myNote.Stream.Position := 0;
                  inc( ThisNodeIndex );

                  if ExportOptions.IncludeNodeHeadings then begin
                     NodeHeading := ExpandExpTokenString( ExportOptions.NodeHeading, myKntFile.Filename, RemoveAccelChar( myFolder.Name ), myNote.Name, myNote.Level+1, ThisNodeIndex, myFolder.TabSize );
                     if ShowHiddenMarkers then
                        NodeHeading:= Format('%s [%d]', [NodeHeading, myNote.ID]);
                     NodeHeadingTpl_Aux := '';
                     if ExportOptions.NodeLevelTemplates then
                        NodeHeadingTpl_Aux:= NodeLevelHeadingTpl[myNote.Level];
                     if NodeHeadingTpl_Aux = '' then
                        NodeHeadingTpl_Aux:= NodeHeadingTpl;
                     NodeHeadingRTF := MergeHeadingWithRTFTemplate( EscapeTextForRTF( NodeHeading ), NodeHeadingTpl_Aux );
                  end;

                  case ExportOptions.SingleNodeFiles of
                      false : begin
                        // nodes are gathered into a single file
                        NodeTextSize := myNote.Stream.Size;
                        if NodeTextSize > 0 then begin
                          SetLength( NodeText, NodeTextSize );
                          // transfer stream contents to temp string
                          move( myNote.Stream.Memory^, NodeText[1], NodeTextSize );

                          // now for some treachery. In KeyNote, a user can mark a note as "plain text only". In such a node, all nodes are stored as
                          // plain text, not RTF. However, the change from RTF to text (or back) occurs only when a node is DISPLAYED. So, it is possible
                          // that user enabled "plain text only", but some tree nodes have not been viewed, hence are still in RTF. So, at this point
                          // we cannot know if the node data we're about to export is RTF or plain text data. Yet we must pass the correct information
                          // to PutRichText. Which is why we must check manually, like so:
                           //NodeStreamIsRTF := ( copy( NodeText, 1, 6 ) = '{\rtf1' );        // Not necessary with (*1)

                          SSNode:= RTFAux.SelStart;

                          // now add the node data to temp RTF storage
                          if ( ExportOptions.IncludeNodeHeadings and ( NodeHeadingRTF <> '' )) then begin
                             var ApplyAutoFontSizes: boolean := ExportOptions.AutoFontSizesInHeading and (FontSizes_Max > 0);
                             var StrAux: string;
                             RTFAux.PutRtfText(NodeHeadingRTF, true, true, ApplyAutoFontSizes);   // Keep selected if ApplyAutoFontSizes

                             if ApplyAutoFontSizes then begin
                                FSize:= FontSizes_Max - (FontSizes_Inc*(myNote.Level+1));
                                if FSize < FontSizes_Min then
                                   FSize := FontSizes_Min;
                                SS:= RTFAux.SelStart;
                                SL:= RTFAux.SelLength;
                                StrAux:= RTFAux.SelText;
                                if Copy(StrAux,SL-1,SL+1) = #13#13 then begin
                                   RTFAux.SelLength:= SL-2;                   // Not include line break
                                   RTFAux.SelAttributes.Size:= FSize;
                                   RTFAux.SetSelection(SS+SL-2, SS+SL, true); // Line break
                                   RTFAux.SelAttributes.Size:= FSize-4;
                                end
                                else
                                   RTFAux.SelAttributes.Size:= FSize;

                                RTFAux.SetSelection(SS+SL, SS+SL, true);
                             end;
                          end;

                          RTFwithImages:= '';
                          if not myFolder.PlainText then
                             RTFwithImages:= ImagesManager.ProcessImagesInRTF(NodeText, nil, imImage, '', 0, false);

                          if RTFwithImages <> '' then
                             RTFAux.PutRtfText(RTFwithImages, false)           // All hidden KNT characters are now removed from FlushExportFile
                          else
                             RTFAux.PutRtfText(NodeText, false);               // append to end of existing data

                          if ExportOptions.IndentNestedNodes then
                             IndentContent(myNote.Level+1);
                        end;
                        inc( tmpExportedNodes );
                      end;

                      true : begin
                        // each node is saved to its own file
                        // (Here we do not have to check if node stream is plain text or RTF, because LoadFromStream handles both cases automatically!)
                        RTFwithImages:= '';
                        if not myFolder.PlainText then
                           RTFwithImages:= ImagesManager.ProcessImagesInRTF(myNote.Stream.Memory, myNote.Stream.Size, nil, imImage, '', 0, ContainsImgIDsRemoved, ContainsImages, false);

                        if RTFwithImages <> '' then
                           RTFAux.PutRtfText(RTFwithImages,true,false)         // All hidden KNT characters are now removed from FlushExportFile
                        else begin
                           RTFAux.StreamFormat:= myFolder.Editor.StreamFormat;
                           RTFAux.Lines.LoadFromStream( myNote.Stream );
                        end;

                        if ( ExportOptions.IncludeNodeHeadings and ( NodeHeadingRTF <> '' )) then begin
                          RTFAux.SelStart := 0;
                          RTFAux.PutRtfText(NodeHeadingRTF, true);
                        end;
                        if FlushExportFile( RTFAux, myFolder, myNote.Name ) then
                          inc( ExportedNodes );
                      end;
                  end; // case ExportOptions.SingleNodeFiles
                end;

                // access next node
                myTreeNode := myTreeNode.GetNext;

                Application.ProcessMessages;
                if DoAbort then break;

                // check break conditions
                if ( ExportOptions.ExportSource = expCurrentNote ) then begin
                  if ( not assigned( myTreeNode )) then
                    break;
                  case ExportOptions.TreeSelection of
                    tsNode : break;
                    tsSubtree : if ( myTreeNode.Level <= StartLevel ) then break;
                  end;
                end;
              end;                                   // <<<<<------------------ Iterate each node  (xfPlainText, xfRTF, xfHTML / ntTree)
                    

              // if exporting all nodes to one file, flush nodes now
              if ( not ExportOptions.SingleNodeFiles ) then begin
                // we have gathered all nodes, now flush the file
                if ( ExportOptions.IncludeNoteHeadings and ( NoteHeadingRTF <> '' )) then begin
                   var ApplyAutoFontSizes: boolean := ExportOptions.AutoFontSizesInHeading and (FontSizes_Max > 0);
                   RTFAux.SelStart := 0;
                   RTFAux.PutRtfText(NoteHeadingRTF, true, true, ApplyAutoFontSizes);   // Keep selected if ApplyAutoFontSizes
                   if ApplyAutoFontSizes then begin
                      SL:= RTFAux.SelLength;
                      RTFAux.SelAttributes.Size:= FontSizes_Max;
                      RTFAux.SetSelection(SL, SL, true);
                   end;

                end;
                if FlushExportFile( RTFAux, myFolder, RemoveAccelChar( myFolder.Name )) then
                   inc( ExportedNodes, tmpExportedNodes );
              end;
            end;


            xfTreePad : begin                                                 // ============================= xfTreePad

              Application.ProcessMessages;
              if DoAbort then break;

              // we need to create a new file if no file has been created yet,
              // or if each note is saved to a separate TreePad file
              if (( TreePadFN = '' ) or ( not ExportOptions.TreePadSingleFile )) then begin
                if ( ExportOptions.TreePadSingleFile and ( ExportOptions.ExportSource <> expCurrentNote )) then
                   TreePadFN := GetExportFilename( ExtractFilename( myKntFile.FileName )) // use file name
                else
                   TreePadFN := GetExportFilename( RemoveAccelChar( myFolder.Name )); // use note name

                if DoAbort then break;
                if ( TreePadFN = '' ) then continue;

                TreePadFile:= TTextFile.Create();
                TreePadFile.AssignFile(TreePadFN);
                TreePadFile.Rewrite;

                TreePadNodeLevelInc := 0; // set initially

                try

                  // write TreePad header
                  if ExportOptions.TreePadRTF then
                     TreePadFile.WriteLine(_TREEPAD_HEADER_RTF )   // version 4.0
                  else
                     TreePadFile.WriteLine(_TREEPAD_HEADER_TXT );   // version 2.7

                  // TreePad allows only 1 "top level node", i.e. node with level 0.
                  // KeyNote has no such limit. So, we must, in most cases, create a dummy top level node, and put all other nodes as its children.
                  // This is not necessary only when exporting one non-tree note to a Treepad file (essentially, the TreePad file will only have
                  // one node.) We optimize this further by checking if the current note we're exporting conforms to TreePad's restriction (has
                  // only one top-level node) - if so, we don't need the dummy node. user can also force the creation of the dummy top-level node.

                  if ( ExportOptions.TreePadForceMaster or
                     ((( ExportOptions.TreePadSingleFile and ( ExportOptions.ExportSource <> expCurrentNote )) or
                     (( myFolder.TV.Items.GetFirstNode <> nil ) and ( myFolder.TV.Items.GetFirstNode.GetNextSibling <> nil ))))) then
                  begin
                    // create a dummy top-level node

                    TreePadNodeLevelInc := 1; // must increase KeyNote's node levels by 1,
                    // because level 0 is the dummy node

                    TreePadFile.WriteLine(_TREEPAD_NODE);     // <node>
                    if ( ExportOptions.TreePadForceMaster or ( ExportOptions.TreePadSingleFile and ( ExportOptions.ExportSource <> expCurrentNote ))) then
                       TreePadFile.WriteLine(ExtractFilename( myKntFile.Filename ), True)     // Node title is filename
                    else
                       TreePadFile.WriteLine(RemoveAccelChar( myFolder.Name ), True);  // Node title is note name
                    TreePadFile.WriteLine('0');  // top node level

                    // some node text:
                    TreePadFile.WriteLine('Exported file: ' + ExtractFilename( myKntFile.Filename ), True);
                    if ( myKntFile.Description <> '' ) then
                       TreePadFile.WriteLine('File description: ' +  myKntFile.Description, True);
                    if ( myKntFile.Comment <> '' ) then
                       TreePadFile.WriteLine('File comment: ' + myKntFile.Comment, True);
                    TreePadFile.WriteLine('Date created: ' + DateTimeToStr( myKntFile.DateCreated ));
                    TreePadFile.WriteLine('Date exported: ' + DateTimeToStr( now ));
                    if ( not ExportOptions.TreePadSingleFile ) then
                       TreePadFile.WriteLine('Exported note: ' + RemoveAccelChar( myFolder.Name ), True);

                     TreePadFile.WriteLine(_TREEPAD_ENDNODE);
                  end;

                finally
                  // don't REALLY need to close, but if we get an exception
                  // in the code above, let's make sure the file is closed
                  TreePadFile.CloseFile;
                end;
              end;

              // we already have a file at this point - reopen it
              TreePadFile.Append;

              try
                // export the whole note to the file.
                // At this point we only need to check the kind of note

                 myFolder.EditorToDataStream; // must flush contents of richedit to active node's internal stream
                 myTreeNode := myFolder.TV.Items.GetFirstNode;

                 while assigned( myTreeNode ) do begin
                   myNote := TKntNote( myTreeNode.Data );

                   NodeTextSize := myNote.Stream.Size;
                   NodeText:= '';
                   if NodeTextSize > 0 then begin
                     SetLength( NodeText, NodeTextSize );
                     move( myNote.Stream.Memory^, NodeText[1], NodeTextSize );
                     RTFAux.PutRtfText(NodeText, false);   // append to end of existing data
                   end;
                   FlushTreePadData( TreePadFile, myNote.Name, myNote.Level + TreePadNodeLevelInc, RTFAux, true );


                   inc( ExportedNodes );
                   myTreeNode := myTreeNode.GetNext;
                 end;

                 inc( ExportedNotes );

              finally
                 // close file after every note, because we might break out of the loop at next iteration
                 TreePadFile.CloseFile;
              end;

            end;                                                             // <<< --------------------    xfTreePad

          end; // case ExportOptions.TargetFormat

        end;
      end; // for NoteIdx := 1 to myKntFile.Notes.Count do

    except
      on E : Exception do begin
         WasError := true;
         messagedlg( STR_11 + E.Message, mtError, [mbOK], 0 );
      end;
    end;

  finally
    FreeConvertLibrary;
    FileIsBusy := false;
    IsBusy := false;
    Screen.Cursor := crDefault;
    RTFAux.Free;
    ExitMessage := Format(STR_12, [ExportedNotes, ExportedNodes] );
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
  var tf : TTextFile;
  const Name : string;
  const Level : integer;
  const RTF : TTabRichEdit;
  const ClearRTF : boolean );
var
  tmpStream : TMemoryStream;
  StreamSize : integer;
  Encoding: TEncoding;
  Editor: TTabRichEdit;

begin

  if ExportOptions.TreePadRTF then
     tf.WriteLine(_TREEPAD_RTFNODE)
  else
     tf.WriteLine(_TREEPAD_TXTNODE);
  tf.WriteLine(_TREEPAD_NODE);    // <node>

  tf.WriteLine(Name, True);
  tf.WriteLine(Level.ToString);


  tmpStream := TMemoryStream.Create;

  try
    Encoding:= nil;

    Editor:= RTF;
    if (ExportOptions.TreePadRTF) then begin
       if ClearRTF then
          Editor.RemoveKNTHiddenCharacters(false)
       else
          Editor:= GetEditorWithNoKNTHiddenCharacters(Editor, hmAll, false);
    end;

    if (not ExportOptions.TreePadRTF ) then begin
       Editor.StreamFormat := sfPlainText;
       if not CanSaveAsANSI(Editor.Text) then
          Encoding:= TEncoding.UTF8;
    end;

    Editor.Lines.SaveToStream( tmpStream, Encoding );
    StreamSize := tmpStream.Size;

    if StreamSize > 0 then begin
       if (ExportOptions.TreePadRTF) then begin
         // When compiled in D2006 with RxRichEdit 2.75 not ocurred, but now, when saving the stream in RTF an extra #0 is added
         if PByte(tmpStream.Memory)[StreamSize-1] = 0 then
            StreamSize:= StreamSize -1;
       end;

       tmpStream.Position := 0;
       tf.F.CopyFrom(tmpStream, StreamSize);
       tf.WriteLine('');
    end;

    tf.WriteLine(_TREEPAD_ENDNODE);


  finally
    tmpStream.Free;

    if Editor <> RTF then
       Editor.Free

    else begin
      if ClearRTF then
         Editor.Clear;
      Editor.StreamFormat := sfRichText;
    end;

  end;

end; // FlushTreePadData


function TForm_ExportNew.GetExportFilename( const FN : string ) : string;
var
  ext : string;
  Filename, FilePath: string;
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

    xfKeyNote : begin
      SaveDlg.Filter := FILTER_NOTEFILES;
      ext := '.knt';
    end;

    else begin
      SaveDlg.Filter := FILTER_ALLFILES;
      ext := '.txt';
    end;
  end;

  Filename:= ChangeFileExt( MakeValidFilename( FN, [' '], MAX_FILENAME_LENGTH ), ext );
  result := ExportOptions.ExportPath + Filename;

  if ( ExportOptions.ConfirmFilenames or ( ExportOptions.ConfirmOverwrite and FileExists( result ))) then begin
     SaveDlg.InitialDir := ExportOptions.ExportPath;
     SaveDlg.Filename := Filename;
     if SaveDlg.Execute then
        result := SaveDlg.Filename

    else begin
      result := '';
      ConfirmAbort; // if user clicked Cancel in the SaveDlg, ask if user wants to abort the whole process
    end;
  end;

end; // GetExportFilename


function TForm_ExportNew.FlushExportFile( const RTF : TTabRichEdit; myFolder: TKntFolder; FN : string ) : boolean;
var
  StreamSize : integer;
  ext: string;
  Txt: string;
  Encoding: TEncoding;

begin
  result := false;
  Encoding:= TEncoding.Default;
  FN := GetExportFilename( FN + '.EXT' );
  if ( FN = '' ) then exit; // break condition

  try
    ext := Extractfileext( FN );

    case ExportOptions.TargetFormat of
      xfPlainText : begin
        if ( ext = '' ) then FN := FN + ext_TXT;

        {if ExportOptions.ShowHiddenMarkers then
           RTF.MakeKNTHiddenMarksVisible;           // Necessary if we want to use SaveToStream, not making the hidden part of the hyperlinks visible
        }

        PrepareRTFforPlainText(RTF, myFolder.TabSize, EditorOptions.IndentInc);

        Txt:= RTF.Text;
        if not CanSaveAsANSI(Text) then
           Encoding:= TEncoding.UTF8;

        if ShowHiddenMarkers then begin
           Txt:= GetHumanizedKNTHiddenCharacters(Txt);
           TFile.WriteAllText(FN, Txt, Encoding);
        end
        else begin
           RTF.StreamFormat := sfPlainText;
           RTF.Lines.SaveToFile( FN, Encoding );
        end;

        result := true;
      end;

      xfRTF : begin
        if ( ext = '' ) then FN := FN + ext_RTF;
        RTF.RemoveKNTHiddenCharacters(false);
        RTF.StreamFormat:= sfRichText;

        RTF.Lines.SaveToFile( FN );
        result := true;
      end;

      xfHTML : begin
        if ( ext = '' ) then FN := FN + ext_HTML;
        RTF.RemoveKNTHiddenCharacters(false);
        RTF.StreamFormat:= sfRichText;
        Result:= ConvertRTFToHTML( FN, RTF.RtfText, ExportOptions.HTMLExportMethod);
      end;
    end;

  finally
    RTF.Clear;
    RTF.StreamFormat := sfRichText;
  end;


end; // FlushExportFile


procedure TForm_ExportNew.PrepareRTFforPlainText (RTFAux : TTabRichEdit; TabSize: integer; RTFIndentValue: integer);
var
  L, SS, SS_NL, STab: integer;
  FI, LI, LIndent: integer;
  StringIndent: string;
  NumTabs: integer;
  ParaAttrib: TRxParaAttributes;
  BulletsPT: string;

begin
   RTFAux.SuspendUndo;

   BulletsPT:= EditorOptions.BulletsInPlainText;
   if BulletsPT = '' then
      BulletsPT:= '- ';

   SS:= 0;
   for L := 0 to RTFAux.Lines.Count-1 do begin
      SS:= RTFAux.Perform(EM_LINEINDEX, L, 0);
      RTFAux.SelStart:= SS;
      ParaAttrib:= RTFAux.Paragraph;
      FI:= ParaAttrib.FirstIndent;
      LI:= ParaAttrib.LeftIndent;
      LIndent:= FI + LI;

      if LIndent > 0 then begin
         NumTabs:= LIndent div RTFIndentValue;

         if ParaAttrib.Numbering <> nsNone then
            Dec(NumTabs);

         if ExportOptions.IndentUsingTabs then
            StringIndent:= StringOfChar (#9, NumTabs)
         else
            StringIndent:= StringOfChar (' ', NumTabs * TabSize);

         if (ParaAttrib.Numbering in [nsNone, nsBullet]) then
            RTFAux.SelText := StringIndent;
      end;


      if ParaAttrib.Numbering = nsBullet then begin
         RTFAux.SelStart:= SS + RTFAux.SelLength;
         RTFAux.SelText := BulletsPT;
      end
      else
      if ParaAttrib.Numbering <> nsNone then begin
         SS_NL:= RTFAux.Perform(EM_LINEINDEX, L+1, 0);
         RTFAux.SelLength:= SS_NL - SS -1;
         RTFAux.CopyToClipboard;
         RTFAux.PasteIRichEditOLE(CF_TEXT);

         if ExportOptions.NumbTabInPlainText <> '' then begin
            STab:= RTFAux.FindText(#9, SS, 5, []);
            if (STab >= 0) then begin
               RTFAux.SetSelection(STab, STab+1, false);
               RTFAux.SelText := ExportOptions.NumbTabInPlainText;
            end;
         end;

         if LIndent > 0 then begin
            RTFAux.SelStart:= SS;
            RTFAux.SelText := StringIndent;
         end;

      end;
   end;

   RTFAux.ResumeUndo;
end;     // PrepareRTFforPlainText


procedure TForm_ExportNew.Button_SelectClick(Sender: TObject);
var
  TabSel : TForm_SelectTab;
begin
  TabSel := TForm_SelectTab.Create( self );
  try
    TabSel.ShowHint := ShowHint;
    TabSel.myKntFile := myKntFile;
    if ( TabSel.ShowModal = mrOK ) then begin
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
                                   _TokenChar,EXP_FOLDERNAME,
                                   _TokenChar,EXP_NODENAME,
                                   _TokenChar,EXP_NODELEVEL,
                                   _TokenChar,EXP_NODEINDEX,
                                   _TokenChar,EXP_LINE_BREAK,
                                   _TokenChar,EXP_NODELEVELSYMB_INC,
                                   _TokenChar,EXP_NODELEVELSYMB_DEC ]),
    mtInformation, [mbOK], -1 );     // -1 -> 313-9 : Exporting Notes to Disk Files [313] / 9: Available Tokens, and related options
end;


procedure TForm_ExportNew.Button_HelpClick(Sender: TObject);
begin
  ActiveKeyNoteHelp(Pages.ActivePage.HelpContext);  // Node
  //Application.HelpCommand( HELP_CONTEXT, HelpContext );
end;


procedure ExportTreeNode;
var
  myTreeNode : TTreeNTNode;
  myNote : TKntNote;
  oldFilter, ext, RTFText, Txt: string;
  ExportFN : string;
  exportformat : TExportFmt;
  Encoding: TEncoding;
  ExportSelectionOnly : boolean;
  RTFAux : TTabRichEdit;

begin

  myTreeNode := GetCurrentTreeNode;
  if ( not assigned( myTreeNode )) then begin
    showmessage( STR_16 );
    exit;
  end;

  if ( ActiveKntFolder.Editor.Lines.Count = 0 ) then begin
    showmessage( STR_17 );
    exit;
  end;

  ActiveKntFolder.EditorToDataStream;

  // {N}
  ExportFN := MakeValidFileName( myTreeNode.Text, [' '], MAX_FILENAME_LENGTH );

  with Form_Main.SaveDlg do begin
    try
      oldFilter := Filter;
      Filter := FILTER_EXPORT;
      FilterIndex := LastExportFilterIndex;
      if ( KeyOptions.LastExportPath <> '' ) then
        InitialDir := KeyOptions.LastExportPath
      else
        InitialDir := GetFolderPath( fpPersonal );

      FileName := ExportFN;
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

    else begin
      exportformat := xfPlainText;
      if ( ext = '' ) then
         ExportFN := ExportFN + ext_TXT;
    end;
  end;

  myNote := TKntNote( myTreeNode.Data );
  myNote.Stream.Position := 0;

  RTFAux:= nil;
 try
  try
    ExportSelectionOnly := ( ActiveKntFolder.Editor.SelLength > 0 );

    if exportformat in [xfRTF, xfHTML] then begin
       if ActiveKntFolder.PlainText then begin
          RTFAux:= CreateRTFAuxEditorControl;
          PrepareRTFAuxForPlainText(RTFAux, ActiveKntFolder);
          if ExportSelectionOnly then
             Txt:= ActiveKntFolder.Editor.SelText
          else
             Txt:= ActiveKntFolder.Editor.Text;
          RTFAux.PutRtfText(Txt, True, False);
          RTFText:= RTFAux.RtfText;
       end
       else
          if ExportSelectionOnly then
             RTFText:= ActiveKntFolder.Editor.RtfSelText
          else
             RTFText:= ActiveKntFolder.Editor.RtfText;

       RTFText:= RemoveKNTHiddenCharactersInRTF(RTFText, hmAll);
    end
    else begin
       if ExportSelectionOnly then
          RTFText:= ActiveKntFolder.Editor.SelText
       else
          RTFText:= ActiveKntFolder.Editor.Text;

       RTFText:= RemoveKNTHiddenCharacters(RTFText);
    end;

    case exportformat of
      xfRTF:
          TFile.WriteAllText(ExportFN, RTFText);

      xfHTML:
        try
          ConvertRTFToHTML(ExportFN, RTFText, htmlExpMicrosoftHTMLConverter);

        finally
          FreeConvertLibrary;
        end;

      xfPlainText : begin
          Encoding:= TEncoding.Default;
          if not CanSaveAsANSI(RTFText) then
             Encoding:= TEncoding.UTF8;

         TFile.WriteAllText(ExportFN, RTFText, Encoding);
      end;
    end;

    Form_Main.Statusbar.Panels[PANEL_HINT].Text := STR_18 + ExtractFilename( ExportFN );

  except
    on E : Exception do
       messagedlg( STR_19 + E.Message, mtError, [mbOK], 0 );
  end;
 finally
   if RTFAux <> nil then
      RTFAux.Free;
 end;

end; // ExportTreeNode


procedure ExportNotesEx;
var
  Form_Export : TForm_ExportNew;
begin
  if ( not Form_Main.HaveKntFolders( true, true )) then exit;
  Form_Export := TForm_ExportNew.Create( Form_Main );
  try
    with Form_Export do begin
      ShowHint := KeyOptions.ShowTooltips;
      myKntFolder := ActiveKntFolder;
      myKntFile := KntFile;
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
