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
   Vcl.Printers,

   TB97Ctls,
   ComCtrls95,
   VirtualTrees,
   RxRichEd,

   gf_streams,
   knt.ui.editor,
   kn_Info,
   kn_KntFile,
   kn_KntFolder,
   kn_LinksMng,
   knt.model.note
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
    gbHeadings: TGroupBox;
    CB_IncNoteHeading: TCheckBox;
    CB_IncNodeHeading: TCheckBox;
    Edit_NodeHead: TComboBox;
    Edit_NoteHead: TComboBox;
    SaveDlg: TSaveDialog;
    RG_TreePadVersion: TRadioGroup;
    RG_TreePadMode: TRadioGroup;
    RG_TreePadMaster: TRadioGroup;
    CheckBox_ExcludeHiddenNodes: TCheckBox;
    TB_OpenDlgDir: TToolbarButton97;
    CB_LevelTemplates: TCheckBox;
    CB_FontSizes: TCheckBox;
    Edit_FontSizes: TEdit;
    Edit_LengthHeading: TEdit;
    Btn_TknHlp: TBitBtn;
    Edit_Symbols: TEdit;
    lblSymbols: TLabel;
    lblLength: TLabel;
    btnPreview: TButton;
    FontDlg: TFontDialog;
    CB_Section: TCheckBox;
    Spin_SectDepth: TSpinEdit;
    CB_SaveImgDefWP: TCheckBox;
    CB_ShowHiddenMarkers: TCheckBox;
    Spin_TblMaxDepth: TSpinEdit;
    CB_TableCont: TCheckBox;
    gbOther: TGroupBox;
    Edit_Sample: TEdit;
    CB_IndentNodes: TCheckBox;
    Spin_Indent: TSpinEdit;
    CB_Font: TCheckBox;
    BTN_Font: TBitBtn;
    CB_UseTab: TCheckBox;
    lblIndent: TLabel;
    CB_Header: TCheckBox;
    Spin_PgHdDepth: TSpinEdit;
    CB_ShowPageNumber: TCheckBox;
    gbHTML: TGroupBox;
    RG_HTML: TRadioGroup;
    CB_SectionToFile: TCheckBox;
    CB_SectionNewPg: TCheckBox;
    btnPageSetup: TButton;
    CB_NoteNewPg: TCheckBox;
    CB_FolderNewFile: TCheckBox;
    cbFoldedText: TComboBox;
    lbl3: TLabel;
    PnlTags: TPanel;
    lbl4: TLabel;
    lbl9: TLabel;
    lbl8: TLabel;
    txtTagsIncl: TEdit;
    chkRemoveTags: TCheckBox;
    chkInhTagsFind: TCheckBox;
    chkTagsText: TCheckBox;
    chkTagsMetad: TCheckBox;
    txtTagsExcl: TEdit;
    cbTagFindMode: TComboBox;
    procedure RG_HTMLClick(Sender: TObject);
    procedure TB_OpenDlgDirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_SelectClick(Sender: TObject);
    procedure Btn_TknHlpClick(Sender: TObject);
    procedure Button_HelpClick(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure Edit_SymbolsExit(Sender: TObject);
    procedure btnPreviewClick(Sender: TObject);
    procedure BTN_FontClick(Sender: TObject);
    procedure CB_FontClick(Sender: TObject);
    procedure CB_SectionToFileClick(Sender: TObject);
    procedure CB_SectionNewPgClick(Sender: TObject);
    procedure btnPageSetupClick(Sender: TObject);

    procedure CtrlUI_Changed(Sender: TObject);
    procedure chkTagsMetadClick(Sender: TObject);
    procedure chkTagsTextClick(Sender: TObject);
    procedure txtTagsInclEnter(Sender: TObject);
    procedure txtTagsExclEnter(Sender: TObject);
    procedure cbTagFindModeChange(Sender: TObject);

  private
    { Private declarations }
    PrinterMode: boolean;
    LastPagePrintedHeight: integer;
    PreviewMode: boolean;
    IgnorePrinterOffset: boolean;
    FirstPrint: boolean;
    InfoExportedNotes: TInfoExportedNotesInRTF;
    IndexOfExportedFile: integer;
    PrinterSelected: boolean;
    ChangingFromCode: boolean;

  protected
    { Public declarations }
    myKntFolder : TKntFolder;
    myKntFile : TKntFile;
    myINIFN : string;

    ExportOptions : TExportOptions;

    IsBusy : boolean;
    DoAbort : boolean;


    UsingTags: boolean;
    ExportTextFragments: boolean;
    FilterNodesByTag: boolean;
    FindTagsIncl: TFindTags;
    FindTagsExcl: TFindTags;
    FindTagsIncl_NotRegistered: string;
    FindTagsExcl_NotRegistered: string;

    procedure WriteConfig;
    procedure PerformExport;
    procedure FormToOptions;
    procedure OptionsToForm;
    function ValidatePath : boolean;
    procedure CheckDependencies;

    function FlushExportFile( const RTF : TAuxRichEdit; myFolder: TKntFolder; FN : string; const PageHeader: string = '') : boolean;
    procedure FlushTreePadData( var tf : TTextfile; const Name : string; const Level : integer; const RTF : TAuxRichEdit; const ClearRTF : boolean);
    function GetExportFilename( const FN : string ) : string;
    procedure PrepareRTFforPlainText (RTFAux : TRxRichEdit; TabSize: integer; RTFIndentValue: integer);

    function ConfirmAbort : boolean;

    function ExpandExpTokenString( const tpl, filename, folderName, nodename : string; const nodelevel, nodeindex : integer; TabSize: integer ) : string;

    procedure UpdateSampleFont;

    function GetFilterInfUsingFindAll(OnlyNode: PVirtualNode = nil; FolderToUse: TKntFolder = nil; TextPlainToUse: string = ''): boolean;
    procedure OnChangeFindTagsInclIntrod(FindTags: TFindTags; FindTagsNotRegistered: string);
    procedure OnChangeFindTagsExclIntrod(FindTags: TFindTags; FindTagsNotRegistered: string);
    procedure OnEndFindTagsInclIntrod(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string);
    procedure OnEndFindTagsExclIntrod(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string);
    procedure ChangeFindInclToModeOR;
    procedure CheckTxtTagsEnabled;
    procedure EnableDefaultButton (Enable: boolean);

    procedure RxRTFProtectChangeEx(Sender: TObject; const Message: TMessage; StartPos, EndPos: Integer; var AllowChange: Boolean);
    procedure ExpandFoldedText(RTF : TAuxRichEdit);
    procedure RemoveFoldedText(RTF : TAuxRichEdit; OnlyIfTaggedFolded: boolean);
  end;


function LoadRTFHeadingTemplate( const Filename : string ) : string;
function EscapeTextForRTF( const Txt : string ) : string;
function MergeHeadingWithRTFTemplate( const Heading, RTFTemplate : string ) : string;

procedure ExportNotesEx (PrinterMode: boolean = false);
procedure ExportTreeNode;
procedure ReadConfig (var ExportOptions: TExportOptions; const myINIFN: string; PrinterMode: boolean);

var
  Form_Export: TForm_ExportNew;
  FontInfo: TFontInfo;

implementation
uses
   gf_misc,
   gf_miscvcl,
   gf_files,  // Important. Needed to use TMemIniFileHelper (.ReadString, .WriteString)
   gf_strings,
   kn_RTFUtils,
   kn_Const,
   kn_Ini,
   kn_Global,
   knt.ui.tree,
   kn_ExportImport,
   kn_TabSelect,
   kn_Main,
   kn_NoteFileMng,
   kn_EditorUtils,
   kn_LocationObj,
   kn_FindReplaceMng,
   knt.ui.TagMng,
   knt.App,
   knt.RS
   ;

var
  LengthsHeading_Max, LengthsHeading_Inc, LengthsHeading_Min: integer;
  FontSizes_Max, FontSizes_Inc, FontSizes_Min: integer;

  ExpFoundNodes: TNodeList;
  ExpFragmentsInNodes: TNoteFragmentsList;

{$R *.DFM}


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

         if ExportOptions.IndentNestedNodes and (nodelevel > ExportOptions.SectionOnDepth) then begin
            NumTabsLevel:= 1;
            if ExportOptions.TargetFormat = xfPlainText then
               NumTabsLevel:= ExportOptions.IndentValue div EditorOptions.IndentInc;
            Dec(LenSymbolsLevel, NumTabsLevel*TabSize * (nodelevel-1-ExportOptions.SectionOnDepth));
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
  App.ApplyBiDiModeOnForm(Self);

  Pages.ActivePage := Tab_Main;
  IsBusy := false;
  DoAbort := false;
  ChangingFromCode:= false;

  myKntFolder := nil;
  myKntFile := nil;
  myINIFN := '';
  PrinterMode:= false;
  PrinterSelected:= false;

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
  App.HideNestedFloatingEditors;

  ReadConfig (ExportOptions, myINIFN, PrinterMode);
  OptionsToForm;

  CheckDependencies;

  RB_CurrentNote.OnClick :=   CtrlUI_Changed;
  RB_Allnotes.OnClick :=      CtrlUI_Changed;
  RB_SelectedNotes.OnClick := CtrlUI_Changed;

  CB_FontSizes.OnClick:= CtrlUI_Changed;
  CB_IncNodeHeading.OnClick:= CtrlUI_Changed;
  CB_IndentNodes.OnClick:= CtrlUI_Changed;
  CB_TableCont.OnClick:= CtrlUI_Changed;
  CB_Header.OnClick:= CtrlUI_Changed;
  CB_Section.OnClick:= CtrlUI_Changed;
  Spin_PgHdDepth.OnChange:= CtrlUI_Changed;
  Spin_SectDepth.OnChange:= CtrlUI_Changed;

  Combo_Format.OnClick := CtrlUI_Changed;

  cbTagFindMode.ItemIndex:= 0;
  cbFoldedText.ItemIndex:= 0;        // 0:  Keep folded text unchanged

end; // ACTIVATE


function TForm_ExportNew.ConfirmAbort : boolean;
begin
  result := ( App.DoMessageBox( GetRS(sExpFrm01), mtConfirmation, [mbOK,mbCancel], Def2 ) = mrOK );
  DoAbort := result;
end; // ConfirmAbort


procedure TForm_ExportNew.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if IsBusy then
     CanClose := ConfirmAbort;

  if CanClose then
     Button_Cancel.SetFocus;
end; // CloseQuery


procedure TForm_ExportNew.CtrlUI_Changed(Sender: TObject);
begin
  CheckDependencies;
end;

procedure TForm_ExportNew.CB_SectionToFileClick(Sender: TObject);
var
   format: TExportFmt;
begin
   if ChangingFromCode then exit;

   format:= TExportFmt( Combo_Format.ItemIndex );
   if format= xfRtf then
      CB_SectionNewPg.Checked:= not CB_SectionToFile.Checked;
  CheckDependencies;
end;

procedure TForm_ExportNew.CB_SectionNewPgClick(Sender: TObject);
var
   format: TExportFmt;
begin
   if ChangingFromCode then exit;
   format:= TExportFmt( Combo_Format.ItemIndex );
   if format= xfRtf then
     CB_SectionToFile.Checked:= not CB_SectionNewPg.Checked;
   CheckDependencies;
end;


procedure TForm_ExportNew.CheckDependencies;
var
   format: TExportFmt;
   TreePadFmt, PrinterFmt, RtfFmt, NoPlainText, KntOrTreePadFmt, RtfOrPrinterFmt: boolean;
   NodeHeadingEnabled, IndentNodesChecked: boolean;
begin
  if ChangingFromCode then exit;

  ChangingFromCode:= True;
  format:= TExportFmt( Combo_Format.ItemIndex );

  TreePadFmt:=  (format = xfTreePad);
  PrinterFmt:=  (format = xfPrinter);
  RtfFmt:=      (format = xfRTF);
  NoPlainText:= not (format = xfPlainText);
  KntOrTreePadFmt:= format in [xfTreePad, xfKeyNote];
  RtfOrPrinterFmt:= (format in [xfPrinter, xfRTF]);

  Combo_TreeSelection.Enabled:= RB_CurrentNote.Checked and not TreePadFmt;
  Button_Select.Enabled := RB_SelectedNotes.Checked;

  Tab_TreePad.TabVisible:=  TreePadFmt;
  Tab_Options.TabVisible := not KntOrTreePadFmt;

  IgnorePrinterOffset:= not PrinterFmt;

  btnPreview.Visible := RtfOrPrinterFmt;
  btnPageSetup.Visible := RtfOrPrinterFmt;
  Button_Ok.Default:= not PrinterFmt;
  if PrinterFmt then begin
     Button_Ok.Caption:= GetRS(sExpFrm22);
     Button_Ok.ImageIndex:= 44;
     btnPreview.SetFocus;
  end
  else begin
     Button_Ok.Caption:= GetRS(sExpFrm21);
     Button_Ok.ImageIndex:= -1;
  end;

  CB_TableCont.Enabled := not KntOrTreePadFmt;
  Spin_TblMaxDepth.Enabled:= not KntOrTreePadFmt and CB_TableCont.Checked;
  if CB_TableCont.Checked then begin
     if Spin_TblMaxDepth.Value = 0 then
        Spin_TblMaxDepth.Value := 9;
  end;

  CB_ShowHiddenMarkers.Enabled := (format = xfPlainText);
  if NoPlainText then
     CB_ShowHiddenMarkers.Checked:= False;

  CheckBox_PromptOverwrite.Enabled:= not PrinterFmt;
  CheckBox_Ask.Enabled:=  not PrinterFmt;
  TB_OpenDlgDir.Enabled:= not PrinterFmt;
  Edit_Folder.Enabled:=   not PrinterFmt;

  CB_Section.Enabled := not KntOrTreePadFmt;
  Spin_SectDepth.Enabled:= CB_Section.Checked;
  CB_SectionToFile.Enabled := RtfFmt and CB_Section.Checked;
  CB_SectionNewPg.Enabled :=  RtfFmt and CB_Section.Checked;
  if KntOrTreePadFmt then
     CB_Section.Checked:= False;

  if CB_Section.Checked then begin
     if Spin_SectDepth.Value = 0 then
        Spin_SectDepth.Value := 9;
  end
  else begin
     CB_SectionToFile.Checked:= False;
     CB_SectionNewPg.Checked:= False;
  end;
  if CB_SectionToFile.Checked then
     CB_SectionNewPg.Checked:= False;
  if PrinterFmt then begin
     CB_SectionToFile.Checked:= False;
     CB_SectionNewPg.Checked:= CB_Section.Checked;
  end;
  if RtfFmt then begin
     if CB_Section.Checked and not CB_SectionToFile.Checked and not CB_SectionNewPg.Checked then
        CB_SectionNewPg.Checked:= True;
  end;
  if (format in [xfPlainText, xfHTML]) then begin
     CB_SectionToFile.Checked:= CB_Section.Checked;
     CB_SectionNewPg.Checked:= False;
  end;

  CB_NoteNewPg.Enabled := RtfOrPrinterFmt;
  CB_FolderNewFile.Enabled := (not CB_Section.Checked or not CB_SectionToFile.Checked) and not (format in [xfPrinter, xfTreePad, xfKeyNote]);
  if CB_Section.Checked and CB_SectionToFile.Checked then
     CB_FolderNewFile.Checked:= False;


  CB_SaveImgDefWP.Enabled := RtfFmt;

  CB_FontSizes.Enabled :=   NoPlainText;
  Edit_FontSizes.Enabled := NoPlainText and (CB_FontSizes.Checked);
  CB_UseTab.Enabled:= (format = xfPlainText) and (CB_IndentNodes.Checked);

  CB_Header.Enabled := RtfOrPrinterFmt and CB_Section.Checked;
  Spin_PgHdDepth.Enabled:= CB_Header.Enabled and CB_Header.Checked;
  if CB_Header.Checked then begin
     if Spin_PgHdDepth.Value = 0 then
        Spin_PgHdDepth.Value := 9;
  end;
  if Spin_PgHdDepth.Value > Spin_SectDepth.Value then
     Spin_PgHdDepth.Value:= Spin_SectDepth.Value;

  NodeHeadingEnabled:= CB_IncNodeHeading.Checked;
  CB_LevelTemplates.Enabled:=  NodeHeadingEnabled;
  Edit_Symbols.Enabled:=       NodeHeadingEnabled;
  Edit_LengthHeading.Enabled:= NodeHeadingEnabled;
  lblSymbols.Enabled:= NodeHeadingEnabled;
  lblLength.Enabled:=  NodeHeadingEnabled;

  IndentNodesChecked:= CB_IndentNodes.Checked;
  Spin_Indent.Enabled:= IndentNodesChecked;
  LblIndent.Enabled:=   IndentNodesChecked;
  CB_UseTab.Enabled:=   IndentNodesChecked;

  CB_ShowPageNumber.Enabled:= RtfOrPrinterFmt;
  CB_Font.Enabled :=     NoPlainText;
  BTN_Font.Enabled :=    NoPlainText and CB_Font.Checked;
  Edit_Sample.Enabled := NoPlainText and CB_Font.Checked;


  RG_HTML.Enabled:= (format = xfHTML);

  ChangingFromCode:= False;
end;


procedure ReadConfig (var ExportOptions: TExportOptions; const myINIFN: string; PrinterMode: boolean);
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
      ExportOptions.IncludeFolderHeadings := readbool( section, ExportOptionsIniStr.IncludeNoteHeadings, ExportOptions.IncludeFolderHeadings );
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
      ExportOptions.TargetFormat := TExportFmt( readinteger( section, ExportOptionsIniStr.TargetFormat, ord( ExportOptions.TargetFormat )));
      ExportOptions.TreePadRTF := readbool( section, ExportOptionsIniStr.TreePadRTF, ExportOptions.TreePadRTF );
      ExportOptions.TreePadForceMaster := readbool( section, ExportOptionsIniStr.TreePadForceMaster, ExportOptions.TreePadForceMaster );
      ExportOptions.TreePadSingleFile := readbool( section, ExportOptionsIniStr.TreePadSingleFile, ExportOptions.TreePadSingleFile );
      ExportOptions.TreeSelection := TTreeSelection( readinteger( section, ExportOptionsIniStr.TreeSelection, ord( ExportOptions.TreeSelection )));
      ExportOptions.RTFImgsWordPad := readbool( section, ExportOptionsIniStr.RTFImgsWordPad, ExportOptions.RTFImgsWordPad );
      ExportOptions.SectionOnDepth := readinteger( section, ExportOptionsIniStr.SectionOnDepth, ExportOptions.SectionOnDepth );
      ExportOptions.SectionToFile := readbool( section, ExportOptionsIniStr.SectionToFile, ExportOptions.SectionToFile );
      ExportOptions.EachNoteNewPg := readbool( section, ExportOptionsIniStr.EachNoteNewPg, ExportOptions.EachNoteNewPg );
      ExportOptions.FilePerFolder := readbool( section, ExportOptionsIniStr.FilePerFolder, ExportOptions.FilePerFolder );
      ExportOptions.ShowPageNumber := readbool( section, ExportOptionsIniStr.ShowPageNumber, ExportOptions.ShowPageNumber );
      ExportOptions.TopLvlAsPgHeader := readinteger( section, ExportOptionsIniStr.TopLvlAsPgHeader, ExportOptions.TopLvlAsPgHeader );
      ExportOptions.TableContMaxDepth := readinteger( section, ExportOptionsIniStr.TableContMaxDepth, ExportOptions.TableContMaxDepth );
      ExportOptions.SimpleFileName := readbool( section, ExportOptionsIniStr.SimpleFileName, ExportOptions.SimpleFileName );

      if PrinterMode then
         ExportOptions.TargetFormat:= xfPrinter;
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
  Dir:= ExportOptions.ExportPath;
  if SelectDirectory('','', Dir) then
     Edit_Folder.Text:= Dir;

  TB_OpenDlgDir.Down:= false;
end;




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
      writebool( section, ExportOptionsIniStr.IncludeNoteHeadings, ExportOptions.IncludeFolderHeadings );
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

      if not PrinterMode then
         writeinteger( section, ExportOptionsIniStr.TargetFormat, ord( ExportOptions.TargetFormat ));

      writebool( section, ExportOptionsIniStr.TreePadForceMaster, ExportOptions.TreePadForceMaster );
      writebool( section, ExportOptionsIniStr.TreePadRTF, ExportOptions.TreePadRTF );
      writebool( section, ExportOptionsIniStr.TreePadSingleFile, ExportOptions.TreePadSingleFile );
      writeinteger( section, ExportOptionsIniStr.TreeSelection, ord( ExportOptions.TreeSelection ));
      writebool( section, ExportOptionsIniStr.RTFImgsWordPad, ExportOptions.RTFImgsWordPad );
      writeinteger( section, ExportOptionsIniStr.SectionOnDepth, ExportOptions.SectionOnDepth );
      writebool( section, ExportOptionsIniStr.SectionToFile, ExportOptions.SectionToFile );
      writebool( section, ExportOptionsIniStr.EachNoteNewPg, ExportOptions.EachNoteNewPg );
      writebool( section, ExportOptionsIniStr.FilePerFolder, ExportOptions.FilePerFolder );
      writebool( section, ExportOptionsIniStr.ShowPageNumber, ExportOptions.ShowPageNumber );
      writeinteger( section, ExportOptionsIniStr.TopLvlAsPgHeader, ExportOptions.TopLvlAsPgHeader );
      writeinteger( section, ExportOptionsIniStr.TableContMaxDepth, ord( ExportOptions.TableContMaxDepth ));
      writebool( section, ExportOptionsIniStr.SimpleFileName, ExportOptions.SimpleFileName );
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
      ExportSource := expCurrentFolder
    else
    if RB_AllNotes.Checked then
      ExportSource := expAllFolders
    else
      ExportSource := expSelectedFolders;

    ExcludeHiddenNodes:= CheckBox_ExcludeHiddenNodes.Checked;
    ShowHiddenMarkers:= CB_ShowHiddenMarkers.Checked;

    TreeSelection := TTreeSelection( Combo_TreeSelection.ItemIndex );
    TargetFormat := TExportFmt( Combo_Format.ItemIndex );

    HTMLExportMethod := THTMLExportMethod(RG_HTML.ItemIndex);

    ExportPath := ProperFolderName( Edit_Folder.Text );
    ConfirmOverwrite := CheckBox_PromptOverwrite.Checked;
    ConfirmFilenames := CheckBox_Ask.Checked;

    IncludeNodeHeadings := CB_IncNodeHeading.Checked;
    IncludeFolderHeadings := CB_IncNoteHeading.Checked;

    SymbolsInHeading:= Edit_Symbols.Text;
    LengthHeading:= Edit_LengthHeading.Text;
    FontSizesInHeading:=  Edit_FontSizes.Text;
    NodeLevelTemplates:= CB_LevelTemplates.Checked;
    AutoFontSizesInHeading:= CB_FontSizes.Checked;
    IndentNestedNodes := CB_IndentNodes.Checked;
    IndentValue := Spin_Indent.Value;
    IndentUsingTabs := CB_UseTab.Checked;

    RTFImgsWordPad := CB_SaveImgDefWP.Checked;
    ShowPageNumber := CB_ShowPageNumber.Checked;

    SectionOnDepth:= 0;
    if CB_Section.Checked then
       SectionOnDepth:= Spin_SectDepth.Value;
    SectionToFile := (SectionOnDepth > 0) and CB_SectionToFile.Checked;

    FilePerFolder:= CB_FolderNewFile.Checked;
    EachNoteNewPg:= CB_NoteNewPg.Checked;

    TopLvlAsPgHeader:= 0;
    if CB_Header.Checked then
       TopLvlAsPgHeader:= Spin_PgHdDepth.Value;

    TableContMaxDepth:= 0;
    if CB_TableCont.Checked then
       TableContMaxDepth:= Spin_TblMaxDepth.Value;

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
  ChangingFromCode:= True;
  try
     with ExportOptions do begin
       case ExportSource of
         expCurrentFolder : RB_CurrentNote.Checked := true;
         expAllFolders : RB_AllNotes.Checked := true;
         expSelectedFolders : RB_SelectedNotes.Checked := true;
       end;

       CheckBox_ExcludeHiddenNodes.Checked:= ExcludeHiddenNodes;
       CB_ShowHiddenMarkers.Checked:= ShowHiddenMarkers;

       CB_TableCont.Checked := (TableContMaxDepth > 0);
       Spin_TblMaxDepth.Value:= TableContMaxDepth;

       CB_Header.Checked := (TopLvlAsPgHeader > 0);
       Spin_PgHdDepth.Value:= TopLvlAsPgHeader;

       CB_ShowPageNumber.Checked:= ShowPageNumber;

       Combo_TreeSelection.ItemIndex := ord( TreeSelection );
       Combo_Format.ItemIndex := ord( TargetFormat );

       CB_Section.Checked:= SectionOnDepth > 0;
       Spin_SectDepth.Value:= SectionOnDepth;
       CB_SectionToFile.Checked:= SectionToFile;

       CB_FolderNewFile.Checked:= FilePerFolder;
       CB_NoteNewPg.Checked:= EachNoteNewPg;

       RG_HTML.ItemIndex := ord(HTMLExportMethod);
       Edit_Folder.Text := ExportPath;
       CheckBox_PromptOverwrite.Checked := ConfirmOverwrite;
       CheckBox_Ask.Checked := ConfirmFilenames;

       CB_IncNodeHeading.Checked := IncludeNodeHeadings;
       CB_IncNoteHeading.Checked := IncludeFolderHeadings;
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

       CB_SaveImgDefWP.Checked:= RTFImgsWordPad;

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

     UpdateSampleFont;
  finally
     ChangingFromCode:= false;
  end;
end; // OptionsToForm


procedure TForm_ExportNew.Button_OKClick(Sender: TObject);
begin
  PreviewMode:= False;
  PerformExport;
end;

procedure TForm_ExportNew.btnPageSetupClick(Sender: TObject);
begin
   Form_Main.MMFilePageSetupClick (nil);
   PrinterSelected:= False;                   // To allow selecting printer again on previewing
end;

procedure TForm_ExportNew.btnPreviewClick(Sender: TObject);
var
  formatBak: TExportFmt;
  SectionToFileBak: boolean;
begin
  PreviewMode:= True;

  formatBak:= TExportFmt( Combo_Format.ItemIndex );
  try
     if formatBak <> xfPrinter then begin
        SectionToFileBak:= CB_SectionToFile.Checked;
        Combo_Format.ItemIndex := ord(xfPrinter);
        CheckDependencies;
        IgnorePrinterOffset:= True;
     end;
     PerformExport;

  finally
     if formatBak <> TExportFmt( Combo_Format.ItemIndex ) then begin
        Combo_Format.ItemIndex := ord(formatBak);
        CB_SectionToFile.Checked:= SectionToFileBak;
        CheckDependencies;
     end;
  end;

end;

function TForm_ExportNew.ValidatePath : boolean;
begin
  result := false;
  if ( ExportOptions.ExportPath = '' ) then begin
    App.WarningPopup(GetRS(sExpFrm02));
    exit;
  end;

  if ( not System.SysUtils.DirectoryExists( ExportOptions.ExportPath )) then begin
    App.WarningPopup(GetRS(sExpFrm03));
    exit;
  end;

  result := true;
end;


procedure TForm_ExportNew.PerformExport;
var
  myFolder : TKntFolder;
  TreeUI: TKntTreeUI;
  i, cnt, FolderIdx : integer;
  RTFAux : TAuxRichEdit;

  FolderHeading, NodeHeading : string;
  FolderHeadingRTF, NodeHeadingRTF : string;
  FolderHeadingTpl, NodeHeadingTpl : string;
  NodeLevelHeadingTpl : array of string;
  NodeHeadingTpl_Aux: string;
  NodeText: AnsiString;
  ExitMessage : string;
  NodeTextSize : integer;
  StartLevel, Level, ThisNodeIndex : integer;
  tmpStream : TMemoryStream;
  ExportedFolders, ExportedNotes, tmpExportedNodes : integer;
  WasError, WasAborted : boolean;
  StartTreeNode, myTreeNode : PVirtualNode;
  NNode : TNoteNode;
  NEntry: TNoteEntry;
  TreePadFile : TTextfile;
  TreePadFN : string;
  TreePadNodeLevelInc : integer; // 0 or 1
  NodeStreamIsRTF : boolean;
  Encoding: TEncoding;
  FN, ext: string;
  OnlyNotHiddenNodes, OnlyCheckedNodes: boolean;
  RTFwithImages: AnsiString;
  FSize, SS, SL: integer;
  SSNode, SSNodeContent: integer;
  ImgFormatInsideRTF_BAK: TImageFormatToRTF;
  FirstNodeExportedInFile, FirstNodeExportedInFolder, FirstNodeExportedInSection: boolean;
  TableOfContentsPendingToFlush: boolean;
  FolderNameExported: string;
  NumFoldersExported: integer;
  FormatCommnd: string;
  PageHeader: string;

  iNode: integer;
  NoteFragments: TNoteFragments;
  RTFAuxFrag : TAuxRichEdit;
  RTFFrag: AnsiString;
  ExportWholeNoteText: boolean;
  FoldedMode: TSearchFoldedMode;
  L: integer;


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
     RTFAux.Paragraph.FirstIndentRelative := ExportOptions.IndentValue * level;
     RTFAux.SelStart:= SS;
  end;

  procedure ChangeFont;
  var
     SS: integer;
  begin
     if not CB_Font.Checked then exit;

     SS:= RTFAux.SelStart;
     RTFAux.SetSelection(SSNodeContent, SS, true);
     RTFAux.SelAttributes.Name := FontInfo.Name;
     RTFAux.SelAttributes.Size:= FontInfo.Size;
     //RTFAux.SelAttributes.Style:= FontInfo.Style;
     RTFAux.SelStart:= SS;
  end;


  procedure InsertPageDimensions;
  begin
     RTFAux.PutRtfText(NodeHeadingRTF, true);
  end;


  procedure ApplyFontSizes (UseMaxSize: boolean);
  var
    ApplyAutoFontSizes: boolean;
    StrAux: string;
  begin
     ApplyAutoFontSizes:= ExportOptions.AutoFontSizesInHeading and (FontSizes_Max > 0);
     if not ApplyAutoFontSizes then exit;

     if UseMaxSize then
        FSize:= FontSizes_Max
     else
        FSize:= FontSizes_Max - (FontSizes_Inc*((Level+1)-StartLevel));

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

  procedure InsertNodeHeading;
  begin
     if ( ExportOptions.IncludeNodeHeadings and ( NodeHeadingRTF <> '' )) then begin
        var ApplyAutoFontSizes: boolean := ExportOptions.AutoFontSizesInHeading and (FontSizes_Max > 0);
        var StrAux: string;
        RTFAux.PutRtfText(NodeHeadingRTF, true, true, ApplyAutoFontSizes);   // Keep selected if ApplyAutoFontSizes
        ApplyFontSizes (False);
     end;
  end;

  procedure InsertFolderHeading;
  begin
     if ( ExportOptions.IncludeFolderHeadings and ( FolderHeadingRTF <> '' )) then begin
       var ApplyAutoFontSizes: boolean := ExportOptions.AutoFontSizesInHeading and (FontSizes_Max > 0);
       RTFAux.PutRtfText(FolderHeadingRTF, true, true, ApplyAutoFontSizes);   // Keep selected if ApplyAutoFontSizes
       ApplyFontSizes (True);
     end;
  end;


  function NodeToBeExported(Node, StartTreeNode: PVirtualNode; TreeUI: TKntTreeUI): boolean;
  var
     iNode: integer;
  begin
     Result:= False;
     with ExportOptions do begin
       if  (TreeUI.TV.IsVisible[Node] or not ExcludeHiddenNodes) and
           (( ExportSource <> expCurrentFolder ) or
           (  TreeSelection in [tsNode, tsFullTree] ) or
           (( TreeSelection = tsCheckedNodes ) and (Node.CheckState = csCheckedNormal)) or
           (( TreeSelection = tsSubtree ) and ( TreeUI.TV.HasAsParent(Node, StartTreeNode) or ( StartTreeNode = Node )))) then

         if FilterNodesByTag or ExportTextFragments then begin
             iNode:= ExpFoundNodes.IndexOf(Node);
             if iNode >= 0 then
                Result:= True;
         end
         else
            Result:= True;
     end;
  end;

  procedure InsertTableOfContents (FolderIdx: integer; AllExportedFolder: boolean);
  var
    i, SS, FontSize: integer;
    Node, StartTreeNode: PVirtualNode;
    Indent, S, SEP: string;
    myFolder : TKntFolder;
    TreeUI: TKntTreeUI;
    NNode: TNoteNode;
    Level, StartLevel, LastLevel, MaxLevel, SB: integer;

    procedure InsertCaption (S: String; Sz: Integer);
    begin
      RTFAux.Paragraph.FirstIndent:= 0;
      with RTFAux.SelAttributes do begin
        Name:= 'Tahoma';
        Size:= Sz;
        Style:= [fsBold];
      end;
      RTFAux.AddText(S);
      RTFAux.SelAttributes.Style:= [];
    end;

  begin
      if ExportOptions.TableContMaxDepth <= 0 then exit;

      InsertCaption(myKntFile.File_NameNoExt + #13#13, 20);

      SEP:= '';
      for i := 1 to myKntFile.Folders.Count do begin
        myFolder := myKntFile.Folders[i-1];
        TreeUI:= myFolder.TreeUI;
        if myFolder.Info <= 0 then continue;      // Folder has not been marked for exporting
        if not AllExportedFolder and (i <> FolderIdx) then continue;

        InsertCaption(SEP + myFolder.Name + #13, 18);
        RTFAux.SelAttributes.Size:= 10;
        RTFAux.AddText(#13);
        SEP:= #13#13;

        with ExportOptions do begin
           if ((ExportSource = expCurrentFolder) and (TreeSelection in [tsNode, tsSubtree])) then
              Node := TreeUI.FocusedNode
           else
              Node := TreeUI.GetFirstNode;

           if not assigned(Node) then continue;            // could not access starting node - perhaps tree has none

           StartTreeNode := Node;
           StartLevel := TreeUI.TV.GetNodeLevel(Node);
           Level:= StartLevel;
           LastLevel:= Level;
           MaxLevel:= StartLevel + ExportOptions.TableContMaxDepth-1;

           while assigned(Node) do begin
             if (Level <= MaxLevel) and NodeToBeExported(Node, StartTreeNode, TreeUI) then begin
                NNode:= TreeUI.GetNNode(Node);
                FontSize:= 16 - (2 * (Level-StartLevel));
                if FontSize < 10 then
                   FontSize := 10;
                SB:= 2;
                if Level < LastLevel then
                   SB:= 6;
                RTFAux.Paragraph.SpaceBefore := SB;
                RTFAux.SelAttributes.Size := FontSize;
                RTFAux.Paragraph.FirstIndent:= 16 * (level-StartLevel);
                InsertRtfHyperlink('file:///<' + NNode.GID.ToString, NNode.NoteName, RTFAux);
                RTFAux.AddText(#13);

                LastLevel:= Level;
             end;

             Node := TreeUI.TV.GetNext(Node);
             Level:= TreeUI.TV.GetNodeLevel(Node);

             // check break conditions
             if ( ExportOptions.ExportSource = expCurrentFolder ) then begin
               if ( not assigned( Node )) then break;
               case ExportOptions.TreeSelection of
                  tsNode : break;
                  tsSubtree : if (Level <= StartLevel ) then break;
               end;
             end;

           end;

        end;
      end;
      RTFAux.SelAttributes.Size:= 20;
      RTFAux.AddText(#13#13);

      if ExportOptions.SectionToFile or (ExportOptions.TargetFormat = xfPrinter) then begin
         FormatCommnd:= 'D+';
         if ExportOptions.ShowPageNumber then
            FormatCommnd:= FormatCommnd + 'F';
         InsertHiddenKntCommand(RTFAux, KNT_RTF_HIDDEN_FORMAT, FormatCommnd);
         FlushExportFile( RTFAux, myFolder, myKntFile.File_NameNoExt + ' - Contents' );
      end
      else
         TableOfContentsPendingToFlush:= true;

  end;

  procedure FreeInfoExportedNotes;
  var
     i: integer;
  begin
    if InfoExportedNotes <> nil then begin
       for i:= 0 to High(InfoExportedNotes) do
          if InfoExportedNotes[i].PosInKNTLinks <> nil then
             InfoExportedNotes[i].PosInKNTLinks.Free;
       InfoExportedNotes:= nil;
    end;
  end;

  function HasChildrenExported (ParentNode: PVirtualNode): boolean;
  var
    Node: PVirtualNode;
  begin
     Result:= False;
     Node:= ParentNode;
     while assigned(Node) do begin
       Node:= TreeUI.TV.GetNext(Node);
       if not TreeUI.TV.HasAsParent(Node, ParentNode) then
          break;
       if NodeToBeExported(Node, StartTreeNode, TreeUI) then
          Exit(True);
     end;
  end;

  procedure FlushNotesToFile;
  begin
      assert((tmpExportedNodes = Length(InfoExportedNotes)) or not (ExportOptions.TargetFormat in [xfRTF, xfPrinter]));
      if FlushExportFile( RTFAux, myFolder, FN, PageHeader) then
         inc(ExportedNotes, tmpExportedNodes );
      tmpExportedNodes:= 0;
      FreeInfoExportedNotes;
      RTFAux.Clear;
  end;

  {
  If we are exporting text fragments, we must check whether any are in folded blocks, as well as take into account the option indicated in 'Folded mode'.
  At this point, and from the start of PerformExport, from the call to GetFilterInfUsingFindAll, we will have in FragmentsInNodes the different fragments
  identified throughout the file according to the specified options. NoteFragments here points to the fragments of a specific note (node, really...).
  If that fragment is located wholly or partially within a folded block and we don't do anything special, even if we execute ExpandFoldedText or RemoveFoldedText
  from FlushExportFile, if applicable, the fragment will not be affected.Because the fragment does not include the beginning of the folded block, it will likely remain hidden.
  We also couldn't simply make the entire fragment visible, as in that case we could make links or images that are adapted to the folded format visible.

  We therefore need to expand or delete the folded blocks before processing the fragments, but we must keep in mind that the identified fragments are referencing
  positions based on the state of the note. For this reason, we will again use FindAllEx to locate the fragments after expanding or deleting folded blocks. 
  We will do this by indicating that a specific node should be processed (to consider metadata, if applicable), but passing the plain text we have here.

  If we expand or delete text, these positions will no longer be useful. Additionally, some fragments may be included in blocks that are deleted.
  Depending on the option indicated in FoldedMode, we could continue to keep some fragments within folded blocks.
  These fragments will simply be ignored because they will be invisible, or in any case, only the visible excerpt they include will be displayed.
  }
  procedure PreprocessFoldedFragments;
  var
     TxtPlain: string;
     FoldedMode: integer;
  begin
     TxtPlain:= RTFAuxFrag.TextPlain;
     if Pos(KNT_RTF_BEGIN_FOLDED_PREFIX_CHAR, TxtPlain) <= 0 then exit;

     FoldedMode:= cbFoldedText.ItemIndex;
     case FoldedMode of
        1: ExpandFoldedText(RTFAuxFrag);            // Unfold
        2: RemoveFoldedText(RTFAuxFrag, true);      // Remove "tagged"
        3: RemoveFoldedText(RTFAuxFrag, false);     // Remove all
     end;

     if not GetFilterInfUsingFindAll(myTreeNode, myFolder, RTFAuxFrag.TextPlain) or (FoundNodes.Count = 0) then
        NoteFragments.NumFrag:= 0
     else
        NoteFragments:= FragmentsInNodes[0];
  end;


begin
  FormToOptions;
  if ( not ValidatePath ) then exit;
  WriteConfig;

  UsingTags:= ((FindTagsIncl <> nil) or (FindTagsExcl <> nil) or (FindTagsIncl_NotRegistered <> '') or (FindTagsExcl_NotRegistered <> ''));
  ExportTextFragments:= UsingTags and chkTagsText.Checked;
  FilterNodesByTag:=    UsingTags and chkTagsMetad.Checked;

  RTFAuxFrag:= nil;

  if ((ExportOptions.TargetFormat= xfPrinter) and not IgnorePrinterOffset) then begin
     if not PreviewMode or not PrinterSelected then
        if not Form_Main.PrintDlg.Execute then exit;

     PrinterSelected:= true;
  end;


  if (ExportOptions.TargetFormat= xfKeyNote) and (ExportOptions.TreeSelection = tsNode) and (Combo_TreeSelection.Enabled) then
      if ( App.DoMessageBox( GetRS(sExpFrm20), mtInformation, [mbOK,mbCancel] ) <> mrOK ) then
         Exit;

  cnt := 0;
  for i := 1 to myKntFile.Folders.Count do begin
      myFolder:= myKntFile.Folders[pred(i)];

      case ExportOptions.ExportSource of
          expCurrentFolder : begin
            if ( myFolder = myKntFolder ) then
              myFolder.Info := 1
            else
              myFolder.Info := 0;
          end;

          expAllFolders : begin
            myFolder.Info := 1;
          end;

          expSelectedFolders : begin
            // nothing, notes already tagged for exporting
          end;
      end;

      if ( myFolder.Info > 0 ) then
         inc( cnt );
  end;

  if ( cnt = 0 ) then begin
    App.ErrorPopup(GetRS(sExpFrm04));
    exit;
  end;

  IsBusy := true;
  ActiveFile.IsBusy := true;             // To avoid that kn_Main.TimerTimer, at the beginning can interfere calling to KntFile.UpdateTextPlainVariables(); (it also uses the same RTFAux, with GetAuxEditorControl)
  Screen.Cursor := crHourGlass;
  DoAbort := false;
  ExportedFolders := 0;
  ExportedNotes := 0;
  FolderHeading := '';
  NodeHeading := '';
  WasError := false;
  WasAborted := false;
  FolderHeadingRTF := '';
  NodeHeadingRTF := '';
  StartLevel := 0;
  TreePadFN := '';
  TreePadNodeLevelInc := 0;
  LastPagePrintedHeight := -1;
  FirstPrint:= True;
  TableOfContentsPendingToFlush:= false;
  IndexOfExportedFile:= 0;
  tmpExportedNodes := 0;
  NumFoldersExported:= 0;
  PageHeader:= '';
  if ExportOptions.TableContMaxDepth = 0 then
     IndexOfExportedFile:= 1;

  RTFAux:= CreateAuxRichEdit;
  RTFAux.BeginUpdate;
  RTFAux.OnProtectChangeEx:= RxRTFProtectChangeEx;

  if ExportTextFragments or FilterNodesByTag then begin
     if not GetFilterInfUsingFindAll or (FoundNodes.Count = 0) then begin
        App.InfoPopup(GetRS(sExpFrm23));
        ActiveFile.IsBusy := false;
        IsBusy := false;
        exit;
     end;
     ExpFoundNodes:= FoundNodes;
     ExpFragmentsInNodes:= FragmentsInNodes;
     FoundNodes:= nil;                              // We'll free the list from here. We do this in case we need to call GetFilterInfUsingFindAll again with a single node.
     FragmentsInNodes:= nil;                        // ,,

     RTFAuxFrag:= CreateAuxRichEdit;
     RTFAuxFrag.BeginUpdate;
  end;


  if ExportOptions.ConfirmOverwrite then
    SaveDlg.Options := [ofHideReadOnly,ofPathMustExist,ofOverwritePrompt]
  else
    SaveDlg.Options := [ofHideReadOnly,ofPathMustExist];
  SaveDlg.Filename := '';
  SaveDlg.InitialDir := ExportOptions.ExportPath;

  try
    ImgFormatInsideRTF_BAK:= KeyOptions.ImgFormatInsideRTF;
    if ExportOptions.RTFImgsWordPad then
       KeyOptions.ImgFormatInsideRTF:= ifWmetafile8
    else
       KeyOptions.ImgFormatInsideRTF:= ifAccordingImage;


    try

      if ExportOptions.TargetFormat = xfKeyNote then begin                           // --- xfKeyNote
         myTreeNode := nil;
         OnlyNotHiddenNodes:= ExportOptions.ExcludeHiddenNodes;
         OnlyCheckedNodes:= false;

         if (ExportOptions.ExportSource = expCurrentFolder ) then
            if (ExportOptions.TreeSelection in [tsNode, tsSubtree]) then
               myTreeNode := ActiveFolder.TV.FocusedNode;
            if (ExportOptions.TreeSelection = tsCheckedNodes) then
               OnlyCheckedNodes:= true;

         FN := GetExportFilename('Export_' + ExtractFileName(myKntFile.FileName) );
         if FN <> ''  then begin
            ext := Extractfileext( FN );
            if (ext = '') then FN := FN + ext_KeyNote;
            KntFileCopy (ExportedFolders, ExportedNotes, FN, true, myTreeNode, OnlyNotHiddenNodes, OnlyCheckedNodes );
         end;

         exit;                                                                    // ------------
      end;

      // load general, default templates
      FolderHeadingTpl := LoadRTFHeadingTemplate( NoteHeadingTpl_FN);
      if ( FolderHeadingTpl = '' ) then
         FolderHeadingTpl := _Default_FolderHeadingTpl;
      NodeHeadingTpl := LoadRTFHeadingTemplate( NodeHeadingTpl_FN);
      if ( NodeHeadingTpl = '' ) then
         NodeHeadingTpl := _Default_NodeHeadingTpl;

      // load level templates, if any
      if ExportOptions.NodeLevelTemplates then
         LoadNodeLevelTemplates;

      PrepareExportOptions (ExportOptions.LengthHeading, ExportOptions.FontSizesInHeading);

      for FolderIdx := 1 to myKntFile.Folders.Count do begin             // ----------------------------------------------------------- FOR EACH FOLDER :
        Application.ProcessMessages;
        if DoAbort then break;

        myFolder := myKntFile.Folders[pred( FolderIdx )];
        TreeUI:= myFolder.TreeUI;
        if ( myFolder.Info > 0 ) then begin
          // this folder has been marked for exporting

          inc(NumFoldersExported);
          FirstNodeExportedInFolder:=  True;

          var AllExportedFolders: Boolean;
          AllExportedFolders:=  (ExportOptions.SectionToFile and (ExportOptions.SectionOnDepth > 0)) or
                                 not (ExportOptions.FilePerFolder);
          if (AllExportedFolders and (NumFoldersExported = 1)) or ExportOptions.FilePerFolder then
             InsertTableOfContents (FolderIdx, AllExportedFolders);


          if ExportOptions.IncludeFolderHeadings then begin
             FolderHeading := ExpandExpTokenString( ExportOptions.FolderHeading, myKntFile.Filename, RemoveAccelChar( myFolder.Name ), '', 0, 0, myFolder.TabSize );
             if ShowHiddenMarkers then
                FolderHeading:= Format('%s [%d]', [FolderHeading, myFolder.ID]);
             FolderHeadingRTF := MergeHeadingWithRTFTemplate( EscapeTextForRTF( FolderHeading ), FolderHeadingTpl );
          end;

          case ExportOptions.TargetFormat of
            xfPlainText, xfRTF, xfHTML, xfPrinter : begin                                // =============================    xfPlainText, xfRTF, xfHTML, xfPrinter

              myFolder.SaveEditorToDataModel; // must flush contents of Note editor to active model object's internal stream

              if (( ExportOptions.ExportSource = expCurrentFolder ) and
                  ( ExportOptions.TreeSelection in [tsNode, tsSubtree] )) then
                myTreeNode := TreeUI.FocusedNode
              else
                myTreeNode := TreeUI.GetFirstNode;

              StartTreeNode := myTreeNode;
              ThisNodeIndex := 0;

              if assigned( myTreeNode ) then begin
                StartLevel := TreeUI.TV.GetNodeLevel(myTreeNode);
                Level:= StartLevel;
                inc( ExportedFolders );
              end
              else
                 continue; // could not access starting node - perhaps tree has none


              while assigned( myTreeNode ) do begin          // ---------------------- Iterate each node
                NNode:= TreeUI.GetNNode(myTreeNode);

                // check if we should export this node
                if NodeToBeExported(myTreeNode, StartTreeNode, TreeUI) then begin
                  NEntry:= NNode.Note.Entries[0];          //%%%
                  NEntry.Stream.Position := 0;
                  inc( ThisNodeIndex );

                  if ExportOptions.IncludeNodeHeadings then begin
                     NodeHeading := ExpandExpTokenString( ExportOptions.NodeHeading, myKntFile.Filename, RemoveAccelChar( myFolder.Name ), NNode.NodeName(TreeUI), (Level+1) - StartLevel, ThisNodeIndex, myFolder.TabSize );
                     if ShowHiddenMarkers then begin
                        NodeHeading:= NodeHeading + Format(' [%u]', [NNode.GID]);
                        if NNode.ID > 0 then
                           NodeHeading:= NodeHeading + Format('(id:%d)', [NNode.ID]);
                     end;
                     NodeHeadingTpl_Aux := '';
                     if ExportOptions.NodeLevelTemplates then
                        NodeHeadingTpl_Aux:= NodeLevelHeadingTpl[Level];
                     if NodeHeadingTpl_Aux = '' then
                        NodeHeadingTpl_Aux:= NodeHeadingTpl;
                     NodeHeadingRTF := MergeHeadingWithRTFTemplate( EscapeTextForRTF( NodeHeading ), NodeHeadingTpl_Aux );
                  end;

                  NodeTextSize := NEntry.Stream.Size;
                  if NodeTextSize > 0 then begin
                     SetLength( NodeText, NodeTextSize );
                     move( NEntry.Stream.Memory^, NodeText[1], NodeTextSize );   // transfer stream contents to temp string
                     NodeStreamIsRTF := (copy(NodeText, 1, 6) = '{\rtf1');       // *2
                  end;

                  // now for some treachery. In KeyNote, a user can mark a note as "plain text only". In such a node, all nodes are stored as
                  // plain text, not RTF. However, the change from RTF to text (or back) occurs only when a node is DISPLAYED. So, it is possible
                  // that user enabled "plain text only", but some tree nodes have not been viewed, hence are still in RTF. So, at this point
                  // we cannot know if the node data we're about to export is RTF or plain text data. Yet we must pass the correct information
                  // to PutRichText. Which is why we must check manually, like so:
                  // NodeStreamIsRTF := ( copy( NodeText, 1, 6 ) = '{\rtf1' );        // Not necessary with (*1)
                  //
                  // *2 :
                  // <<the change from RTF to text (or back) occurs only when a node is DISPLAYED>> That is not true currently. However:
                  //  Folder can be PlainText and include a virtual node with RTF content
                  //  Or it can be a RTF folder and include a virtual node with plaint text content (eg. .txt)

                  FormatCommnd:= '';

                  FirstNodeExportedInSection:= FirstNodeExportedInFolder or ((Level - StartLevel) < ExportOptions.SectionOnDepth);
                  FirstNodeExportedInFile:= (FirstNodeExportedInFolder and ((NumFoldersExported=1) or ExportOptions.FilePerFolder))
                                           or
                                            (FirstNodeExportedInSection and ExportOptions.SectionToFile);


                  if (tmpExportedNodes > 0) and (
                      (FirstNodeExportedInSection and (ExportOptions.SectionToFile or (ExportOptions.TargetFormat = xfPrinter)) )
                      or
                      (ExportOptions.EachNoteNewPg and (ExportOptions.TargetFormat = xfPrinter)) ) then

                     FlushNotesToFile;


                  if FirstNodeExportedInFile then begin
                     FormatCommnd:= FormatCommnd + 'D+';
                     if ExportOptions.ShowPageNumber then
                        FormatCommnd:= FormatCommnd + 'F+';
                     FN:= RemoveAccelChar(myFolder.Name);
                     if ExportOptions.SectionToFile and (ExportOptions.SectionOnDepth > 0) or
                        ( (ExportOptions.ExportSource = expCurrentFolder) and (ExportOptions.TreeSelection <> tsFullTree)) then
                           if ExportOptions.SimpleFileName then
                              FN:= NNode.NodeName(TreeUI)
                           else
                              FN:= FN + ' - ' +  NNode.NodeName(TreeUI);
                     FolderNameExported:= myFolder.Name;
                  end;

                  if FirstNodeExportedInSection then begin
                     if (not FirstNodeExportedInFile or TableOfContentsPendingToFlush) then
                        FormatCommnd:= FormatCommnd + 'S+';

                     if CB_Header.Checked and CB_Header.Enabled and (ExportOptions.TopLvlAsPgHeader > 0) then begin
                        var MaxDepth: Integer:= ExportOptions.TopLvlAsPgHeader;
                        var relativeLevel: integer:= Level-StartLevel+1;              // beginning by 1
                        if (MaxDepth > 1) and (MaxDepth >= relativeLevel) then
                           MaxDepth:= relativeLevel-1;
                        if (MaxDepth < ExportOptions.TopLvlAsPgHeader) and HasChildrenExported(NNode.TVNode) then
                           inc(MaxDepth);
                        PageHeader:= TreeUI.GetNodeAncestorsPath(NNode.TVNode, ' - ', MaxDepth, StartLevel+1);
                        FormatCommnd:= FormatCommnd + 'H'+ PageHeader;
                     end;
                  end
                  else
                     if ExportOptions.EachNoteNewPg and not TableOfContentsPendingToFlush then
                        FormatCommnd:= FormatCommnd + 'P';


                  inc(tmpExportedNodes);
                  if ExportOptions.TargetFormat in [xfRTF, xfPrinter] then
                     SetLength(InfoExportedNotes, tmpExportedNodes);

                  if not FirstNodeExportedInFile then
                      SSNode:= RTFAux.SelStart
                  else
                      SSNode:= 0;


                  if FormatCommnd <> '' then
                     InsertHiddenKntCommand(RTFAux, KNT_RTF_HIDDEN_FORMAT, FormatCommnd);
                  if FirstNodeExportedInFolder then
                     InsertFolderHeading;
                  InsertNodeHeading;

                  if ExportOptions.TargetFormat = xfRTF then begin
                     InsertMarker(RTFAux, KNT_RTF_HIDDEN_DATA, NNode.GID);
                     InfoExportedNotes[tmpExportedNodes-1].NNodeGID:= NNode.GID;
                  end;

                  if NodeTextSize > 0 then begin
                    ExportWholeNoteText:= True;

                    if ExportTextFragments then begin
                       ExportWholeNoteText:= False;
                       iNode:= ExpFoundNodes.IndexOf(myTreeNode);
                       NoteFragments:= ExpFragmentsInNodes[iNode];
                       if (NoteFragments.NumFrag = 1) and (NoteFragments.Fragments[0].PosF < 0) then
                          ExportWholeNoteText:= True

                       else begin
                          RTFAuxFrag.Clear;
                          RTFAuxFrag.PutRtfText(NodeText, false);
                          SSNodeContent:= RTFAux.SelStart;

                          PreprocessFoldedFragments;

                          { *3  This can occur if the initially located fragments are in folded blocks, and FoldedMode = Unfold has not been selected.
                            In these cases (we are using fragments because we are searching for tags in the text of the notes), the note header will 
                            be exported, but without content. It should be noted that this is the reason: there are fragments with the desired tagging,
                            but they are in folded blocks. }
                          if NoteFragments.NumFrag = 0 then        // *3
                             RTFAux.AddText(#13)

                          else begin
                             for i:= 0 to NoteFragments.NumFrag - 1 do begin
                                RTFAuxFrag.SelStart:=  NoteFragments.Fragments[i].PosI;
                                RTFAuxFrag.SelLength:= NoteFragments.Fragments[i].PosF - NoteFragments.Fragments[i].PosI + 2;
                                RTFFrag:= RTFAuxFrag.RtfSelText;

                                RTFwithImages:= '';
                                if NodeStreamIsRTF then
                                   RTFwithImages:= ImageMng.ProcessImagesInRTF(RTFFrag, '', imImage, '', 0, false);

                                if RTFwithImages <> '' then
                                   RTFFrag:= RTFwithImages;

                                if ExportOptions.TargetFormat = xfRTF then
                                   GetInfoKNTLinksWithoutMarker(RTFFrag, InfoExportedNotes);

                                RTFAux.PutRtfText(RTFFrag, false);          // All hidden KNT characters are now removed from FlushExportFile.  Append to end of existing data

                                L:= RTFAux.TextLength;
                                if RTFAux.GetTextRange(L-1, L) <> #13 then
                                   RTFAux.AddText(#13);
                             end;
                          end;
                       end;
                    end;

                    if ExportWholeNoteText then begin
                       RTFwithImages:= '';
                       SSNodeContent:= RTFAux.SelStart;
                       if NodeStreamIsRTF then
                          RTFwithImages:= ImageMng.ProcessImagesInRTF(NodeText, '', imImage, '', 0, false);

                       if RTFwithImages <> '' then
                          NodeText:= RTFwithImages;

                       if ExportOptions.TargetFormat = xfRTF then
                          GetInfoKNTLinksWithoutMarker(NodeText, InfoExportedNotes);

                       RTFAux.PutRtfText(NodeText, false);          // All hidden KNT characters are now removed from FlushExportFile
                                                                    // Append to end of existing data
                    end;

                    ChangeFont;
                  end;

                  var Depth: integer:= ExportOptions.SectionOnDepth;
                  if Depth = 0 then Depth:= 1;
                  if ExportOptions.IndentNestedNodes and ((Level - StartLevel) + 1 > Depth) then
                     IndentContent((Level - StartLevel) + 1 - Depth);

                  FirstNodeExportedInFolder:= False;
                  FirstNodeExportedInSection:= False;
                  TableOfContentsPendingToFlush:= False;
                end;

                // access next node
                myTreeNode := TreeUI.TV.GetNext(myTreeNode);
                Level:= TreeUI.TV.GetNodeLevel(myTreeNode);

                Application.ProcessMessages;
                if DoAbort then break;

                // check break conditions
                if ( ExportOptions.ExportSource = expCurrentFolder ) then begin
                  if ( not assigned( myTreeNode )) then
                    break;
                  case ExportOptions.TreeSelection of
                    tsNode : break;
                    tsSubtree : if (Level <= StartLevel ) then break;
                  end;
                end;

              end;                                   // <<<<<------------------ Iterate each node  (xfPlainText, xfRTF, xfHTML, xfPrinter / ntTree)

              if (tmpExportedNodes > 0) and (ExportOptions.FilePerFolder) then begin
                  FN:= myKntFile.File_NameNoExt + ' - ' + FolderNameExported;
                  FlushNotesToFile;
              end;

            end;


            xfTreePad : begin                                                 // ============================= xfTreePad

              Application.ProcessMessages;
              if DoAbort then break;

              // we need to create a new file if no file has been created yet,
              // or if each note is saved to a separate TreePad file
              if (( TreePadFN = '' ) or ( not ExportOptions.TreePadSingleFile )) then begin
                if ( ExportOptions.TreePadSingleFile and ( ExportOptions.ExportSource <> expCurrentFolder )) then
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
                     ((( ExportOptions.TreePadSingleFile and ( ExportOptions.ExportSource <> expCurrentFolder )) or
                     (( TreeUI.GetFirstNode <> nil ) and ( TreeUI.GetFirstNode.NextSibling <> nil ))))) then
                  begin
                    // create a dummy top-level node

                    TreePadNodeLevelInc := 1; // must increase KeyNote's node levels by 1,
                    // because level 0 is the dummy node

                    TreePadFile.WriteLine(_TREEPAD_NODE);     // <node>
                    if ( ExportOptions.TreePadForceMaster or ( ExportOptions.TreePadSingleFile and ( ExportOptions.ExportSource <> expCurrentFolder ))) then
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

                 myFolder.SaveEditorToDataModel;
                 myTreeNode := TreeUI.GetFirstNode;

                 while assigned( myTreeNode ) do begin
                   NNode := TreeUI.GetNNode(myTreeNode);
                   NEntry:= NNode.Note.Entries[0];              //%%%

                   NodeTextSize := NEntry.Stream.Size;
                   NodeText:= '';
                   if NodeTextSize > 0 then begin
                     SetLength( NodeText, NodeTextSize );
                     move(NEntry.Stream.Memory^, NodeText[1], NodeTextSize );
                     RTFAux.PutRtfText(NodeText, false);   // append to end of existing data
                   end;
                   FlushTreePadData( TreePadFile, NNode.NodeName(TreeUI), TreeUI.TV.GetNodeLevel(myTreeNode) + TreePadNodeLevelInc, RTFAux, true );


                   inc( ExportedNotes );
                   myTreeNode := TreeUI.TV.GetNext(myTreeNode);
                 end;

                 inc( ExportedFolders );

              finally
                 // close file after every note, because we might break out of the loop at next iteration
                 TreePadFile.CloseFile;
              end;

            end;                                                             // <<< --------------------    xfTreePad

          end; // case ExportOptions.TargetFormat

        end;
      end; // for NoteIdx := 1 to myKntFile.Folders.Count do


      if tmpExportedNodes > 0 then begin
          if not ExportOptions.SectionToFile or (ExportOptions.SectionOnDepth = 0) then begin
             FN:= myKntFile.File_NameNoExt;
             if (NumFoldersExported = 1) and (myKntFile.Folders.Count > 1) then
                FN:= FN + ' - ' + FolderNameExported;
          end;
          FlushNotesToFile;
      end;


      if (ExportOptions.TargetFormat = xfPrinter) then begin
          if PreviewMode then
             ShowPrintPreview(RTFAux.PrnPreviews, Screen.PixelsPerInch)
          else
          if Printer.Printing then
             Printer.EndDoc;
      end;


    except
      on E : Exception do begin
         WasError := true;
         App.ErrorPopup(E, GetRS(sExpFrm11));
      end;
    end;

  finally
    KeyOptions.ImgFormatInsideRTF:= ImgFormatInsideRTF_BAK;
    FreeConvertLibrary;
    ActiveFile.IsBusy := false;
    IsBusy := false;
    Screen.Cursor := crDefault;
    RTFAux.Free;
    if RTFAuxFrag <> nil then
       RTFAuxFrag.Free;
    FreeInfoExportedNotes;

    ExitMessage := Format(GetRS(sExpFrm12), [ExportedFolders, ExportedNotes] );
    if WasError then
       ExitMessage := ExitMessage + #13 + GetRS(sExpFrm13)
    else
    if DoAbort then
       ExitMessage := ExitMessage + #13 + GetRS(sExpFrm14);

    if not ((ExportOptions.TargetFormat = xfPrinter) and PreviewMode) or DoAbort or WasError then
      if ( App.DoMessageBox( ExitMessage, mtInformation, [mbOK,mbCancel], Def2 ) <> mrOK ) then
          ModalResult := mrCancel; // close dialog box if Cancel clicked

  end;

end; // PerformExport



procedure TForm_ExportNew.FlushTreePadData(
  var tf : TTextFile;
  const Name : string;
  const Level : integer;
  const RTF : TAuxRichEdit;
  const ClearRTF : boolean );
var
  tmpStream : TMemoryStream;
  StreamSize : integer;
  Encoding: TEncoding;
  Editor: TRxRichEdit;

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
          RTF.RemoveKNTHiddenCharacters(false)
       else
          Editor:= RTF.GetRichEditorWithNoKNTHiddenCharacters(hmAll, false);
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
      SaveDlg.Filter := GetRS(FILTER_ALLFILES);
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


function TForm_ExportNew.FlushExportFile( const RTF : TAuxRichEdit; myFolder: TKntFolder; FN : string;
                                          const PageHeader: string = '') : boolean;
var
  StreamSize : integer;
  ext: string;
  Txt: string;
  Encoding: TEncoding;
  FirstPageMarginTop: integer;
  DPI: Integer;
  NodeText: AnsiString;

begin

  case cbFoldedText.ItemIndex of
     1: ExpandFoldedText(RTF);            // Unfold
     2: RemoveFoldedText(RTF, true);      // Remove "tagged"
     3: RemoveFoldedText(RTF, false);     // Remove all
  end;

  if chkRemoveTags.Checked then
     RemoveTags(RTF);


  result := false;
  Encoding:= TEncoding.Default;
  if ExportOptions.SectionToFile and not ExportOptions.SimpleFileName then
     FN := Format('%.2d - %s', [IndexOfExportedFile, FN]);

  if ExportOptions.TargetFormat <> xfPrinter then begin
     FN := GetExportFilename( FN + '.EXT' );
     if ( FN = '' ) then exit; // break condition
  end;

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
        if not CanSaveAsANSI(Txt) then
           Encoding:= TEncoding.UTF8;

        if ShowHiddenMarkers then begin
           Txt:= GetHumanizedKNTHiddenCharacters(Txt);
           TFile.WriteAllText(FN, Txt, Encoding);
        end
        else begin
           if Pos(KNT_RTF_HIDDEN_MARK_EndLink_CHAR, Txt) = 0 then begin
              RTF.StreamFormat := sfPlainText;
              RTF.Lines.SaveToFile( FN, Encoding );
           end
           else begin
              Txt:= RemoveKNTHiddenCharactersInText(Txt);
              Txt:= StringReplace(Txt, KNT_RTF_HIDDEN_MARK_EndLink_CHAR, '[]', [rfReplaceAll]);
              TFile.WriteAllText(FN, Txt, Encoding);
           end;
        end;

        result := true;
      end;

      xfPrinter : begin
        if RTF.TextLength <> 0 then begin
           DPI:= -1;
           if PreviewMode then
              DPI:= Screen.PixelsPerInch;
           RTF.PageRect:= GetPrintAreaInPixels(DPI, IgnorePrinterOffset);

           FirstPageMarginTop:= -1;
           if FirstPrint then begin
              if not PreviewMode then
                 Printer.BeginDoc;
           end
           else begin
              // From PerformExport, FlushExport will be called, if format=xfPrinter, every time a page break is required
              // Unlike what happens in PrintRtfFolder (kn_EditorUtils), where FirstPageMarginTop:= LastPagePrintedHeight; is used
              if True then begin
                 if not PreviewMode then
                    Printer.NewPage;
              end
              else
                 FirstPageMarginTop:= LastPagePrintedHeight;     // Adjust vertical position for next Note (RichEdit)
           end;
           if PreviewMode then
              LastPagePrintedHeight:= RTF.CreatePrnPrew(Caption, ExportOptions.ShowPageNumber, FirstPageMarginTop, DPI, PageHeader)
           else
              LastPagePrintedHeight:= RTF.Print(Caption, ExportOptions.ShowPageNumber, FirstPrint, false, FirstPageMarginTop, PageHeader);

           FirstPrint:= False;
        end;

        result := true;
      end;

      xfRTF : begin
        if ( ext = '' ) then FN := FN + ext_RTF;

        if InfoExportedNotes <> nil then                           // We can be writing only the table of contents
           InsertKNTLinksWithoutMarker(RTF, InfoExportedNotes);
        NodeText:= RTF.RtfText;
        NodeText:= ReplaceHyperlinksWithStandardBookmarks(NodeText);
        if InfoExportedNotes <> nil then
           NodeText:= ConvertToStandardBookmarks(NodeText, InfoExportedNotes, true, ExportOptions);
        TFile.WriteAllText(FN, NodeText);
        {
        RTF.RemoveKNTHiddenCharacters(false);
        RTF.StreamFormat:= sfRichText;
        RTF.Lines.SaveToFile( FN );
        }

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
    inc(IndexOfExportedFile);
    RTF.Clear;
    RTF.StreamFormat := sfRichText;
  end;


end; // FlushExportFile


procedure TForm_ExportNew.PrepareRTFforPlainText (RTFAux : TRxRichEdit; TabSize: integer; RTFIndentValue: integer);
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
         RTFAux.PasteIRichEditOLE(CF_UNICODETEXT);

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
      CheckDependencies;
    end;
    
  finally
    TabSel.Free;
  end;
end; // BUTTON SELECT CLICK


procedure TForm_ExportNew.CB_FontClick(Sender: TObject);
begin
   BTN_Font.Enabled:= CB_Font.Checked;
   Edit_Sample.Enabled := CB_Font.Checked;
   if CB_Font.Checked and (FontInfo.Name = '') then
      BTN_FontClick(nil);
end;


procedure TForm_ExportNew.UpdateSampleFont;
begin
   if FontInfo.Name = '' then begin
      Edit_Sample.Text := '';
      CB_Font.Checked:= False;
      BTN_Font.Enabled:= False;
   end
   else begin
      Edit_Sample.Font.Color := FontInfo.Color;
      //FontInfoToFont( FontInfo, Edit_Sample.Font );
      Edit_Sample.Text := FontInfo.Name + #32 + inttostr( FontInfo.Size ) + ' pt '; // + FontStyleToStr( FontInfo.Style );
   end;
end;


procedure TForm_ExportNew.BTN_FontClick(Sender: TObject);
var
  dpi: integer;
  FI: TFontInfo;
begin
  dpi:= GetSystemPixelsPerInch;
  FontDlg.Options := FontDlg.Options;
  FI:= FontInfo;
  if FontInfo.Name = '' then
     FontInfo:= ActiveFolder.EditorChrome.Font;

  FontInfoToFont( FontInfo, FontDlg.Font, dpi );
  if FontDlg.Execute then
     FontToFontInfo( FontDlg.Font, FontInfo, dpi )
  else
     FontInfo:= FI;

  UpdateSampleFont;
end;


procedure TForm_ExportNew.Btn_TknHlpClick(Sender: TObject);
begin
  MessageDlg(format(GetRS(sExpFrm15),[_TokenChar,EXP_FILENAME,
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
  ActiveKeyNoteHelp(PChar(Pages.ActivePage.HelpKeyword));  // Node+Marker
  //Application.HelpCommand( HELP_CONTEXT, HelpContext );
end;


procedure ExportTreeNode;
var
  NNode : TNoteNode;
  NEntry: TNoteEntry;
  oldFilter, ext, RTFText, Txt: string;
  ExportFN : string;
  exportformat : TExportFmt;
  Encoding: TEncoding;
  ExportSelectionOnly : boolean;
  RTFAux : TAuxRichEdit;
  Editor: TKntRichEdit;
  ImgFormatInsideRTF_BAK: TImageFormatToRTF;
  RTFwithImages: string;
  ExportOptions : TExportOptions;

begin
  NNode:= ActiveTreeUI.GetFocusedNNode;
  if not assigned(NNode) then begin
    App.InfoPopup(GetRS(sExpFrm16));
    exit;
  end;

  Editor:= ActiveFolder.Editor;
  if ( Editor.Lines.Count = 0 ) then begin
    App.InfoPopup(GetRS(sExpFrm17));
    exit;
  end;

  ActiveFolder.SaveEditorToDataModel;

  NEntry:= NNode.Note.Entries[0];          //%%%
  NEntry.Stream.Position := 0;

  // {N}
  ExportFN := MakeValidFileName(NNode.NodeName(ActiveTreeUI), [' '], MAX_FILENAME_LENGTH );

  with Form_Main.SaveDlg do begin
    try
      Title:= GetRS(sExpFrm00);
      oldFilter := Filter;
      Filter := FILTER_EXPORT;
      FilterIndex := KeyOptions.LastExportFormat;
      if NEntry.IsPlainTXT then
         FilterIndex := 2;  // Plain text

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
      KeyOptions.LastExportFormat := FilterIndex;

    finally
      Filter := oldFilter;
    end;
  end;

  ExportFN := normalFN( Form_Main.SaveDlg.FileName );
  KeyOptions.LastExportPath := extractfilepath( ExportFN );
  ext := extractfileext( ExportFN );

  case KeyOptions.LastExportFormat of
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

  RTFAux:= nil;
 try
  try
    ExportSelectionOnly := ( Editor.SelLength > 0 );

    if exportformat in [xfRTF, xfHTML] then begin
       if NEntry.IsPlainTXT then begin
          RTFAux:= CreateAuxRichEdit;
          RTFAux.PrepareEditorforPlainText(Editor.Chrome);
          if ExportSelectionOnly then
             Txt:= Editor.SelText
          else
             Txt:= Editor.Text;
          RTFAux.PutRtfText(Txt, True, False);
          RTFText:= RTFAux.RtfText;
       end
       else begin
          if ExportSelectionOnly then
             RTFText:= Editor.RtfSelText
          else
             RTFText:= Editor.RtfText;

          ReadConfig (ExportOptions, INI_FN, False);
          ImgFormatInsideRTF_BAK:= KeyOptions.ImgFormatInsideRTF;
          try
             if ExportOptions.RTFImgsWordPad then
                KeyOptions.ImgFormatInsideRTF:= ifWmetafile8
             else
                KeyOptions.ImgFormatInsideRTF:= ifAccordingImage;

             RTFwithImages:= ImageMng.ProcessImagesInRTF(RTFText, '', imLink, '', 0, false);
             RTFwithImages:= ImageMng.ProcessImagesInRTF(RTFwithImages, '', imImage, '', 0, false);
             if RTFwithImages <> ''  then
                RTFText:= RTFwithImages;
          finally
             KeyOptions.ImgFormatInsideRTF:= ImgFormatInsideRTF_BAK;
          end;
       end;

       RTFText:= RemoveKNTHiddenCharactersInRTF(RTFText, hmAll);
    end
    else begin
       if ExportSelectionOnly then
          RTFText:= Editor.SelText
       else
          RTFText:= Editor.Text;

       RTFText:= RemoveKNTHiddenCharactersInText(RTFText);
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

    App.ShowInfoInStatusBar(GetRS(sExpFrm18) + ExtractFilename( ExportFN ));

  except
    on E : Exception do
       App.ErrorPopup( GetRS(sExpFrm19) + E.Message);
  end;
 finally
   if RTFAux <> nil then
      RTFAux.Free;
 end;

end; // ExportTreeNode


procedure ExportNotesEx (PrinterMode: boolean = false);
begin
  if ( not Form_Main.HaveKntFolders( true, true )) then exit;
  Form_Export := TForm_ExportNew.Create( Form_Main );
  try
    TagMng.OfferTagSelectorInExport(True);

    with Form_Export do begin
      ShowHint := KeyOptions.ShowTooltips;
      myKntFolder := ActiveFolder;
      myKntFile := ActiveFile;
      myINIFN := INI_FN;
    end;
    Form_Export.PrinterMode:= PrinterMode;

    if ( Form_Export.ShowModal = mrOK ) then
    begin
    end;

  finally
    FreeFragments (ExpFoundNodes, ExpFragmentsInNodes);
    Form_Export.Free;
    Form_Export:= nil;

    TagMng.OfferTagSelectorInExport(False);
  end;
end; // ExportNotesEx


function TForm_ExportNew.GetFilterInfUsingFindAll (OnlyNode: PVirtualNode = nil; FolderToUse: TKntFolder = nil; TextPlainToUse: string = ''): boolean;
var
   myFindOptions: TFindOptions;

begin
  myFindOptions.MatchCase := False;
  myFindOptions.WholeWordsOnly := False;
  if OnlyNode <> nil then begin
     myFindOptions.AllTabs := False;
     myFindOptions.CurrentNodeAndSubtree:= True;
  end
  else begin
     myFindOptions.AllTabs := (ExportOptions.ExportSource <> expCurrentFolder);   // From RunFindAllEx Folder.Info will be taken into account, in case expSelectedFolders has been chosen,
     myFindOptions.CurrentNodeAndSubtree := (ExportOptions.ExportSource = expCurrentFolder) and (ExportOptions.TreeSelection in [tsSubtree, tsNode]);
     if (ExportOptions.ExportSource = expCurrentFolder) and (ExportOptions.TreeSelection = tsNode) then begin  // only current node
        OnlyNode:= ActiveTreeUI.TV.FocusedNode;
        FolderToUse:= ActiveFolder;
     end;
  end;

  myFindOptions.SearchScope := ssContentsAndNodeName;
  myFindOptions.SearchMode := smAll;
  myFindOptions.CheckMode := scAll;
  if (ExportOptions.ExportSource = expCurrentFolder) and (ExportOptions.TreeSelection = tsCheckedNodes) then
     myFindOptions.CheckMode := scOnlyChecked;

  myFindOptions.HiddenNodes:= not ExportOptions.ExcludeHiddenNodes;
  myFindOptions.Pattern := '';
  myFindOptions.FindTagsIncl:= FindTagsIncl;
  myFindOptions.FindTagsExcl:= FindTagsExcl;
  myFindOptions.FindTagsInclNotReg:= FindTagsIncl_NotRegistered;
  myFindOptions.FindTagsExclNotReg:= FindTagsExcl_NotRegistered;
  myFindOptions.TagsMetadata:= chkTagsMetad.Checked;
  myFindOptions.TagsText:= chkTagsText.Checked;
  myFindOptions.InheritedTags:= chkInhTagsFind.Checked;
  myFindOptions.TagsModeOR:= (cbTagFindMode.ItemIndex = 1);

  myFindOptions.LastModifFrom := 0;
  myFindOptions.LastModifUntil := 0;

  myFindOptions.CreatedFrom := 0;
  myFindOptions.CreatedUntil := 0;
  myFindOptions.EmphasizedSearch:= esNone;
  myFindOptions.FoldedMode:= sfAll;

  Result:= RunFindAllEx (myFindOptions, false, false, true, OnlyNode, FolderToUse, TextPlainToUse);
end;


procedure TForm_ExportNew.ChangeFindInclToModeOR;
begin
   FindTagsIncl:= FindTagsGetModeOR(FindTagsIncl);
   TagMng.UpdateTxtFindTagsHint(txtTagsIncl, txtTagsIncl.Text, FindTagsIncl, FindTagsIncl_NotRegistered);
end;

procedure TForm_ExportNew.cbTagFindModeChange(Sender: TObject);
begin
   if FindTagsIncl = nil then exit;
   if (cbTagFindMode.ItemIndex = 1) then
      ChangeFindInclToModeOR
   else begin
      // Ensure that the tags are interpreted according to ALL mode:
      txtTagsIncl.SetFocus;
      cbTagFindMode.SetFocus;
   end;
end;

procedure TForm_ExportNew.EnableDefaultButton (Enable: boolean);
var
  format: TExportFmt;
  PrinterFmt: boolean;
begin
  if not Enable then begin
     Button_Ok.Default:= False;
     btnPreview.Default:= False;
  end
  else begin
    format:= TExportFmt( Combo_Format.ItemIndex );
    PrinterFmt:=  (format = xfPrinter);
    Button_Ok.Default:= not PrinterFmt;
    btnPreview.Default:= PrinterFmt;
  end;
end;


procedure TForm_ExportNew.txtTagsInclEnter(Sender: TObject);
begin
   if CtrlDown then begin
       txtTagsIncl.Text:= '';
       FindTagsIncl:= nil;
   end;

   EnableDefaultButton(False);
   TagMng.StartTxtFindTagIntrod(txtTagsIncl, OnEndFindTagsInclIntrod, OnChangeFindTagsInclIntrod, true);
end;

procedure TForm_ExportNew.OnChangeFindTagsInclIntrod(FindTags: TFindTags; FindTagsNotRegistered: string);
begin
   if cbTagFindMode.ItemIndex = 1 then
      ChangeFindInclToModeOR
   else
      FindTagsIncl:= FindTags;

   FindTagsIncl_NotRegistered:= Trim(FindTagsNotRegistered);
end;

procedure TForm_ExportNew.OnEndFindTagsInclIntrod(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string);
begin
   OnChangeFindTagsInclIntrod(FindTags, FindTagsNotRegistered);
   if PressedReturn then
      txtTagsExcl.SetFocus;

   if txtTagsIncl.Focused then
      txtTagsInclEnter(nil);
end;


procedure TForm_ExportNew.txtTagsExclEnter(Sender: TObject);
begin
   if CtrlDown then begin
       txtTagsExcl.Text:= '';
       FindTagsExcl:= nil;
   end;

   EnableDefaultButton(False);
   TagMng.StartTxtFindTagIntrod(txtTagsExcl, OnEndFindTagsExclIntrod, OnChangeFindTagsExclIntrod, True);
end;


procedure TForm_ExportNew.OnChangeFindTagsExclIntrod(FindTags: TFindTags; FindTagsNotRegistered: string);
begin
   FindTagsExcl:= FindTagsGetModeOR(FindTags);
   FindTagsExcl_NotRegistered:= Trim(FindTagsNotRegistered);
   TagMng.UpdateTxtFindTagsHint(txtTagsExcl, txtTagsExcl.Text, FindTagsExcl, FindTagsExcl_NotRegistered);
end;



procedure TForm_ExportNew.OnEndFindTagsExclIntrod(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string);
begin
   OnChangeFindTagsExclIntrod(FindTags, FindTagsNotRegistered);
   if PressedReturn then
      chkTagsMetad.SetFocus;

   if txtTagsExcl.Focused then
      txtTagsExclEnter(nil);
end;


procedure TForm_ExportNew.chkTagsMetadClick(Sender: TObject);
begin
   CheckTxtTagsEnabled;
end;


procedure TForm_ExportNew.chkTagsTextClick(Sender: TObject);
begin
   CheckTxtTagsEnabled;
end;


procedure TForm_ExportNew.CheckTxtTagsEnabled;
var
  Enable: boolean;
begin
  Enable:= (chkTagsMetad.Checked or chkTagsText.Checked);
  txtTagsIncl.Enabled:= Enable;
  txtTagsExcl.Enabled:= Enable;
  lbl8.Enabled:= Enable;
  lbl9.Enabled:= Enable;
  cbTagFindMode.Enabled:= Enable;
end;


procedure TForm_ExportNew.ExpandFoldedText(RTF : TAuxRichEdit);
var
  TxtPlain: string;
  SS: integer;
begin
  SS:= 1;
  repeat
     TxtPlain:= RTF.TextPlain;
     SS:= Pos(KNT_RTF_BEGIN_FOLDED_PREFIX_CHAR, TxtPlain, SS);
     if SS > 0 then
        Unfold(RTF, TxtPlain, SS);
  until SS = 0;
end;


procedure TForm_ExportNew.RemoveFoldedText(RTF : TAuxRichEdit; OnlyIfTaggedFolded: boolean);
var
  TxtPlain: string;
  SS: integer;
begin
  SS:= 1;
  repeat
     TxtPlain:= RTF.TextPlain;
     SS:= Pos(KNT_RTF_BEGIN_FOLDED_PREFIX_CHAR, TxtPlain, SS);
     if SS > 0 then
        SS:= RemoveFoldedBlock(RTF, TxtPlain, SS, OnlyIfTaggedFolded);
  until SS = 0;
end;

procedure TForm_ExportNew.RxRTFProtectChangeEx(Sender: TObject; const Message: TMessage; StartPos, EndPos: Integer; var AllowChange: Boolean);
begin
   AllowChange:= True
end;



initialization
  Form_Export:= nil;
  FontInfo.Name:= '';

end.
