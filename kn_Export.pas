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

unit kn_Export;

interface

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls,
  TreeNT, kn_NodeList, // r2hconv,
  RxRichEd, RichEdit, comctrls, gf_strings,
  gf_misc, kn_Const, kn_Info, gf_files,
  FileCtrl, kn_FileObj, kn_NoteObj,
  kn_TabSelect, Mask, ToolEdit,
  kn_RTFUtils;


const
  EXPORT_CURRENT  = 1;
  EXPORT_ALL      = 2;
  EXPORT_SELECTED = 3;

type
  TForm_Export = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    GroupBox_Source: TGroupBox;
    RB_Current: TRadioButton;
    RB_All: TRadioButton;
    RB_Selected: TRadioButton;
    Button_Select: TButton;
    GroupBox_Target: TGroupBox;
    Label1: TLabel;
    Combo_Format: TComboBox;
    CheckBox_PromptOverwrite: TCheckBox;
    Edit_Folder: TDirectoryEdit;
    Label2: TLabel;
    CheckBox_Ask: TCheckBox;
    Label_Status: TLabel;
    SaveDlg: TSaveDialog;
    L_TreeHint: TLabel;
    Button_Help: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure RB_SelectedClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button_SelectClick(Sender: TObject);
    procedure Combo_FormatClick(Sender: TObject);
    procedure Button_HelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    OK_Click : boolean;
    myFormat : TExportFmt;
    mySource : integer;
    myCurNoteName : WideString;
    myNotes : TNoteFile;
    myActiveNote : TTabNote;
    myPrompt : boolean;
    myFolder : wideString;
    myAsk : boolean;
    IsBusy : boolean;
    AbortReq : boolean;
    NotesExported, NodesExported : integer;

    function CountExportedNotes : integer;
    procedure PerformExport;
    function Verify : boolean;
    function GetFilename( const aNoteName : wideString; var AFileName : wideString ) : boolean;
    function ExportAsTextOrRTFOrHTML( const aNote : TTabNote; FN : wideString ) : boolean;
    function ExportAsTreePad( const UseRTF : boolean ) : integer;

  end;

implementation
uses kn_Main;

{$R *.DFM}

procedure TForm_Export.FormCreate(Sender: TObject);
var
  f : TExportFmt;
begin
  OK_Click := false;
  IsBusy := false;
  AbortReq := false;
  myActiveNote := nil;
  for f := low( TExportFmt ) to high( TExportFmt ) do
    Combo_Format.Items.Add( EXPORT_FORMAT_NAMES[f] );
  Combo_Format.ItemIndex := 0;
  myFormat := low( TExportFmt );
  mySource := EXPORT_CURRENT;
  myCurNoteName := '';
  myNotes := nil;
  myPrompt := true;
  myAsk := false;
  // myFolder := ProperFolderName( extractfilepath( Application.ExeName ));
  myFolder := GetFolderPath( fpPersonal ) + '\';
end; // CREATE

procedure TForm_Export.FormActivate(Sender: TObject);
var
  i : integer;
begin
  OnActivate := nil;

  Combo_Format.itemindex := ord( myFormat );

  Combo_FormatClick( nil );
  Combo_Format.OnClick := Combo_FormatClick;

  if assigned( myActiveNote ) then
    myCurNoteName := RemoveAccelChar( myActiveNote.Name );
  if ( myCurNoteName <> '' ) then
  begin
    RB_Current.Caption := RB_Current.Caption + ': "' + myCurNoteName + '"';
  end
  else
  begin
    RB_Current.Enabled := false;
    RB_All.Checked := true;
  end;

  case mySource of
    EXPORT_CURRENT  : RB_Current.Checked := true;
    EXPORT_ALL      : RB_All.Checked := true;
    EXPORT_SELECTED : RB_Selected.Checked := true;
  end;

  Edit_Folder.Text := Copy( myFolder, 1, pred( length( myFolder )));
  CheckBox_PromptOverwrite.Checked := myPrompt;
  CheckBox_Ask.Checked := myAsk;

  if ( myNotes.Notes.Count > 0 ) then
  begin
    for i := 0 to pred( myNotes.Notes.Count ) do
      myNotes.Notes[i].Info := 0;
  end;

  RB_SelectedClick( RB_Selected );

end; // ACTIVATE

procedure TForm_Export.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin

  if IsBusy then
  begin
    showmessage( 'Cannot close while exporting data. Click ABORT to stop the process first.' );
    CanClose := false;
  end
  else
    begin
    if OK_Click then
    begin
    end;
  end;
  OK_Click := false;
end; // CLOSEQUERY

procedure TForm_Export.Button_OKClick(Sender: TObject);
begin
  if ( Button_OK.ModalResult = mrNone ) then
  try
    PerformExport;
  finally
    OK_Click := true;
  end;

  try
    Button_OK.SetFocus;
  except
  end;
end;

procedure TForm_Export.Button_CancelClick(Sender: TObject);
begin
  if IsBusy then
  begin
    AbortReq := ( messagedlg( 'Are you sure you want to abort the exporting process?', mtWarning, [mbYes,mbNo], 0 ) = mrYes );
  end;
  OK_Click := false;
end;

procedure TForm_Export.RB_SelectedClick(Sender: TObject);
begin
  Button_Select.Enabled := RB_Selected.Checked;
  Label_Status.Caption := Format( '%d note(s) selected', [CountExportedNotes] );
  if RB_Current.Checked then
    mySource := EXPORT_CURRENT
  else
  if RB_All.Checked then
    mySource := EXPORT_ALL
  else
    mySource := EXPORT_SELECTED;
end;

procedure TForm_Export.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if (( shift = [] ) and ( not Combo_Format.DroppedDown )) then
    begin
      key := 0;
      OK_Click := false;
      if IsBusy then
        RB_SelectedClick( RB_Selected )
      else
        Close;
    end;
  end;
end;

procedure TForm_Export.Button_SelectClick(Sender: TObject);
var
  TabSel : TForm_SelectTab;
begin
  TabSel := TForm_SelectTab.Create( self );
  try
    TabSel.ShowHint := ShowHint;
    TabSel.myNotes := myNotes;
    if ( TabSel.ShowModal = mrOK ) then
    begin
      RB_Selected.Checked := true;
      RB_SelectedClick( RB_Selected );
    end;
  finally
    TabSel.Free;
  end;
end; // BUTTON SELECT CLICK

function TForm_Export.CountExportedNotes : integer;
var
  i : integer;
begin

  if RB_Current.Checked then
  begin
    myActiveNote.Info := 1;
    result := 1;
    exit;
  end
  else
  if RB_All.Checked then
  begin
    if ( myNotes.Notes.Count > 0 ) then
    begin
      for i := 0 to pred( myNotes.Notes.Count ) do
        myNotes.Notes[i].Info := 1;
    end;
    result := myNotes.Notes.Count;
    exit;
  end;

  result := 0;
  if ( myNotes.Notes.Count > 0 ) then
  begin
    for i := 0 to pred( myNotes.Notes.Count ) do
      if ( myNotes.Notes[i].Info > 0 ) then
        inc( result );
  end;

end; // CountExportedNotes


function TForm_Export.ExportAsTextOrRTFOrHTML( const aNote : TTabNote; FN : wideString ) : boolean;
var
  Stream : TTntFileStream;
  tf : TextFile;
  myExt, TabStr, nodeFN : wideString;
  i, StreamSize : integer;
  tNote : TTreeNote;
  myTreeNode : TTreeNTNode;
  myNoteNode : TNoteNode;
  RichEdit : TRxRichEdit;
  List : TStringList;
  Level : integer;
  originalStreamFormat : TRichStreamFormat;
  RTFText : string;
begin
  result := false;
  if ( FN = '' ) then
    FN := MakeValidFileName( aNote.Name, TREENODE_NAME_LENGTH );

  case aNote.Kind of
    ntRTF : begin
      case myFormat of
        xfPlainText : begin
          Stream := TTntFileStream.Create( FN, fmCreate OR fmShareDenyWrite );
          try
            aNote.Editor.StreamFormat := sfPlainText;
            aNote.Editor.Lines.SaveToStream( Stream );
            result := true;
          finally
            aNote.Editor.StreamFormat := sfRichText;
            Stream.Free;
          end;
        end;
        xfHTML : begin
          result := DllConvertRTFToHTML( FN, PChar( GetRichText( aNote.Editor, true, false )));
        end;
        xfRTF : begin
          Stream := TTntFileStream.Create( FN, fmCreate OR fmShareDenyWrite );
          try
            aNote.Editor.Lines.SaveToStream( Stream );
            result := true;
          finally
            Stream.Free;
          end;
        end;
      end;
    end;

    ntTree : begin
      tNote := TTreeNote( aNote );
      Level := 0;

      case myFormat of

        xfPlainText : begin
          // when exporting TreeNote to a plaintext file,
          // a single file is created which contains all tree nodes
          assignfile( tf, FN );
          try
            rewrite( tf );
          except
            On E : Exception do
            begin
              messagedlg( 'Error creating file for ' + tNote.Name + #13#13 + E.Message, mtError, [mbOK], 0 );
              exit;
            end;
          end;

          List := TStringList.Create;
          RichEdit := TRxRichEdit.Create( self );
          with RichEdit do
          begin
            parent := self;
            Visible := false;
            WordWrap := false;
          end;

          try
            writeln( tf, tNote.Name );

            if ( tNote.TV.Items.Count > 0 ) then
            begin
              myTreeNode := tNote.TV.Items.GetFirstNode;
              while assigned( myTreeNode ) do
              begin
                myNoteNode := TNoteNode( myTreeNode.Data );
                if assigned( myNoteNode ) then
                begin
                  TabStr := '';
                  for i := 1 to myTreeNode.Level do
                    TabStr := TabStr + #9;
                  writeln( tf, TabStr, myNoteNode.Name );
                  writeln( tf, TabStr, myTreeNode.Level );

                  List.Clear;
                  myNoteNode.Stream.Position := 0;

                  RichEdit.Lines.Clear;
                  RichEdit.Lines.LoadFromStream( myNoteNode.Stream );

                  List.AddStrings( RichEdit.Lines );
                  if ( List.Count > 0 ) then
                    for i := 0 to pred( List.Count ) do
                      writeln( tf, TabStr, List[i] );
                  writeln( tf );

                end;
                inc( NodesExported );
                myTreeNode := myTreeNode.GetNext;
              end;
              result := true;
            end;

          finally
            closefile( tf );
            RichEdit.Free;
            List.Free;
          end;
        end;

        xfRTF, xfHTML : begin
          // when exporting TreeNote using RTF format,
          // each node is saved to a separate .RTF file
          // Unique filenames for nodes are determined
          // automatically
          if ( tNote.TV.Items.Count > 0 ) then
          begin
            myTreeNode := tNote.TV.Items.GetFirstNode;
            while assigned( myTreeNode ) do
            begin
              myNoteNode := TNoteNode( myTreeNode.Data );
              if assigned( myNoteNode ) then
              begin
                // determine unique file name for the node
                case myFormat of
                  xfRTF : begin
                    myExt := ext_RTF;
                  end;
                  xfHTML : begin
                    myExt := ext_HTML;
                  end;
                end;
                nodeFN := myFolder + MakeValidFileName( myNoteNode.Name, TREENODE_NAME_LENGTH ) + myExt;
                if WideFileexists( nodeFN ) then
                begin
                  i := 0;
                  repeat
                    inc( i );
                    nodeFN := WideFormat( '%s(%d)%s', [nodeFN, i, myExt] );
                  until ( not WideFileexists( nodeFN ));
                end;
              end;

              case myFormat of
                xfRTF : begin
                  myNoteNode.Stream.SaveToFile( nodeFN );
                  result := true;
                  inc( NodesExported );
                end;
                xfHTML : begin
                  StreamSize := myNoteNode.Stream.Size;
                  if ( StreamSize > 0 ) then
                  begin
                    myNoteNode.Stream.Position := 0;
                    SetLength( RTFText, StreamSize );
                    myNoteNode.Stream.ReadBuffer( RTFText[1], myNoteNode.Stream.Size );
                    result := DllConvertRTFToHTML( nodeFN, RTFText );
                    if result then
                      inc( NodesExported );
                  end;
                end;
              end;

              myTreeNode := myTreeNode.GetNext;

            end;
          end;
        end;
      end;
    end;
  end;

end; // ExportAsTextOrRTFOrHTML

function TForm_Export.ExportAsTreePad( const UseRTF : boolean ) : integer;
var
  cnt, level, i : integer;
  tf : textfile;
  FN : string;
  myNote : TTabNote;
  tNote : TTreeNote;
  myTreeNode : TTreeNTNode;
  myNoteNode : TNoteNode;
  RichEdit : TRxRichEdit;
  List : TStringList;
begin
  // TreePad export: all notes (if more than 1 was selected)
  // are stored in a single TreePad file. If only 1 note is
  // selected, the Root node is the name of the note; otherwise
  // the Root node is the name of the file, and each note
  // becomes a level-1 child of the root node. If a note is a tree,
  // all items become children of the level-1 node

  result := 0;
  cnt := 0;
  myNote := nil;
  for i := 0 to pred( myNotes.Notes.Count ) do
    if ( myNotes.Notes[i].Info > 0 ) then
    begin
      inc( cnt );
      myNote := myNotes.Notes[i];
    end;

  FN := '';

  // SaveDlg.Options := SaveDlg.Options + [ofOverwritePrompt];
  if ( cnt = 1 ) then
  begin
    GetFileName( myNote.Name, FN );
  end
  else
  begin
    GetFileName( WideChangefileext( WideExtractFilename( myNotes.FileName ), '' ), FN );
  end;

  if ( FN = '' ) then exit;

  assignfile( tf, FN );

  try
    rewrite( tf );
  except
    On E : Exception do
    begin
      messagedlg( 'Error creating TreePad file: ' + E.Message, mtError, [mbOK], 0 );
      exit;
    end;
  end;

  level := 0;
  try
    if UseRTF then
      writeln( tf, _TREEPAD_HEADER_RTF )
    else
      writeln( tf, _TREEPAD_HEADER );
    if ( cnt > 0 ) then
    begin
      writeln( tf, _TREEPAD_NODE );
      writeln( tf, WideExtractFilename( myNotes.FileName ));
      writeln( tf, level );
      writeln( tf, Format(
        'Notes exported by %s %s (%s)',
        [Program_Name, Program_Version, Program_License]
      ));
      writeln( tf, 'Original file: ', WideExtractFilename( myNotes.FileName ));
      if ( myNotes.Description <> '' ) then
        writeln( tf, 'Description: ', myNotes.Description );
      if ( myNotes.Comment <> '' ) then
        writeln( tf, 'Comment: ', myNotes.Comment );
      writeln( tf, 'Creation date: ', DateTimeToStr( myNotes.DateCreated ));
      writeln( tf, _TREEPAD_ENDNODE );
      inc( level );
    end;

    for i := 0 to pred( myNotes.Notes.Count ) do
    begin
      myNote := myNotes.Notes[i];
      if ( myNote.Info = 0 ) then continue;

      case myNote.Kind of

        ntRTF : begin
          writeln( tf, _TREEPAD_NODE );
          writeln( tf, RemoveAccelChar( myNote.Name ));
          writeln( tf, level );
          if UseRTF then
          begin
          end
          else
          begin
            writeln( tf, myNote.Editor.Text );
          end;
          writeln( tf, _TREEPAD_ENDNODE );
        end;

        ntTree : begin
          tNote := TTreeNote( myNote );
          writeln( tf, _TREEPAD_NODE );
          writeln( tf, RemoveAccelChar( myNote.Name ));
          writeln( tf, level );
          writeln( tf, _TREEPAD_ENDNODE );

          if ( tNote.TV.Items.Count > 0 ) then
          begin
            List := TStringList.Create;
            RichEdit := TRxRichEdit.Create( self );
            with RichEdit do
            begin
              parent := self;
              Visible := false;
              WordWrap := false;
            end;

            try
              myTreeNode := tNote.TV.Items.GetFirstNode;
              while assigned( myTreeNode ) do
              begin
                myNoteNode := TNoteNode( myTreeNode.Data );
                if assigned( myNoteNode ) then
                begin
                  writeln( tf, _TREEPAD_NODE );
                  writeln( tf, myNoteNode.Name );
                  writeln( tf, succ( level + myTreeNode.Level ));
                  myNoteNode.Stream.Position := 0;
                  List.Clear;
                  if UseRTF then
                  begin
                    List.LoadFromStream( myNoteNode.Stream );
                  end
                  else
                  begin
                    RichEdit.Lines.Clear;
                    RichEdit.Lines.LoadFromStream( myNoteNode.Stream );
                    List.AddStrings( RichEdit.Lines );
                  end;
                  writeln( tf, List.Text );

                  writeln( tf, _TREEPAD_ENDNODE );

                end;
                myTreeNode := myTreeNode.GetNext;
              end;

            finally
              RichEdit.Free;
              List.Free;
            end;
          end;
        end;
        
      end;
      inc( result );
    end;

  finally
    closefile( tf );
  end;

end; // ExportAsTreePad



function TForm_Export.GetFilename( const aNoteName : wideString; var AFileName : wideString ) : boolean;
var
  FN : wideString;
  tmpmyAsk, askdone : boolean;
begin
  result := true;
  tmpmyAsk := myAsk;
  FN := MakeValidFileName( aNoteName, TREENODE_NAME_LENGTH );
  SaveDlg.Title := 'Exporting: ' + aNoteName;
  case myFormat of
    xfPlainText : begin
      FN := FN + ext_TXT;
      SaveDlg.Filter := FILTER_TEXTFILES;
    end;
    xfRTF : begin
      FN := FN + ext_RTF;
      SaveDlg.Filter := FILTER_RTFFILES;
    end;
    xfHTML : begin
      FN := FN + ext_HTML;
      SaveDlg.Filter := FILTER_HTMLFILES;
    end;
    xfTreePad, xfTreePadRTF : begin
      FN := FN + ext_TreePad;
      SaveDlg.Filter := FILTER_HJTFILES;
    end;
  end;
  FN := myFolder + FN;
  SaveDlg.InitialDir := myFolder;

  askdone := false;
  repeat
    if tmpmyAsk then
    begin
      SaveDlg.FileName := FN;
      if SaveDlg.Execute then
      begin
        FN := SaveDlg.FileName;
        askdone := true;
      end
      else
      begin
        askdone := true;
        FN := '';
        result := false;
      end;
    end;

    if ( FN <> '' ) then FN := normalFN( FN );
    if (( myPrompt or tmpmyAsk ) and WideFileexists( FN )) then
    begin
      case DoMessageBox( 'This file aready exists: ' + FN + #13#10 +
                       'OK to overwrite the file?', mtWarning, [mbYes,mbNo,mbCancel], 0 ) of
        mrYes : begin
          askdone := true;
          result := true;
        end;
        mrNo : begin
          tmpmyAsk := true;
          result := false;
        end;
        mrCancel : begin
          FN := '';
          askdone := true;
          result := false;
        end;
      end;
    end
    else
    begin
      askdone := true;
    end;

  until askdone;

  AFileName := FN;

end; // GetFilename

procedure TForm_Export.PerformExport;
var
  i : integer;
  s, FN : string;
  myCount : integer;
begin
  NotesExported := 0;
  NodesExported := 0;

  myFormat := TExportFmt( Combo_Format.ItemIndex );
  myFolder := ProperFolderName( Edit_Folder.Text );
  myAsk := CheckBox_Ask.Checked;
  myPrompt := CheckBox_PromptOverwrite.Checked;

  if ( not Verify ) then exit;

  myCount := CountExportedNotes;
  IsBusy := true;
  screen.cursor := crAppStart;
  Button_OK.Enabled := false;
  Button_Cancel.ModalResult := mrNone;
  RB_Current.Enabled := false;
  RB_All.Enabled := false;
  RB_Selected.Enabled := false;
  Button_Select.Enabled := false;
  Combo_Format.Enabled := false;
  Edit_Folder.Enabled := false;
  CheckBox_PromptOverwrite.Enabled := false;
  CheckBox_Ask.Enabled := false;
  GroupBox_Target.Enabled := false;
  GroupBox_Source.Enabled := false;
  try
    try
      case myFormat of
        xfPlainText, xfRTF, xfHTML : begin
          for i := 0 to pred( myNotes.Notes.Count ) do
          begin
            Application.ProcessMessages;
            if ( myNotes.Notes[i].Info > 0 ) then
            begin
              with Label_Status do
              begin
                Caption := Format( 'Exporting note %d of %d', [succ( NotesExported ), myCount] );
                Update;
              end;
              if
              (( myNotes.Notes[i].Kind = ntTree ) and ( myFormat in [xfRTF,xfHTML] ))
                or
              GetFilename( myNotes.Notes[i].Name, FN ) then
              begin
                if ExportAsTextOrRTFOrHTML( myNotes.Notes[i], FN ) then
                  inc( NotesExported );
              end
              else
              begin
                DoMessageBox( WideFormat( 'Skipped "%s"', [myNotes.Notes[i].Name] ));
              end;
              Application.ProcessMessages;
              if AbortReq then
              begin
                raise Exception.Create( ' At user request' );
              end;
            end;
          end;
        end;
        xfTreePad : begin
          NotesExported := ExportAsTreePad( false );
        end;
        xfTreePadRTF : begin
          NotesExported := ExportAsTreePad( true );
        end;
      end;

    except
      on E : Exception do
      begin
        messagedlg( 'Process aborted: ' + E.Message, mtError, [mbOK], 0 );
        AbortReq := true;
        exit;
      end;
    end;

  finally
    IsBusy := false;
    Button_OK.ModalResult := mrOK;
    Button_OK.Caption := '&Close';
    Button_OK.Enabled := true;
    Button_Cancel.Visible := false;
    screen.cursor := crDefault;
    if AbortReq then
    begin
      Label_Status.Caption := 'Aborted.';
    end
    else
    begin
      s := Format( 'Exported notes: %d', [NotesExported] );
      if ( NodesExported > 0 ) then
        s := s + Format( ' (nodes: %d)', [NodesExported] );
      Label_Status.Caption := s;
    end;
  end;

end; // PerformExport

function TForm_Export.Verify : boolean;
var
  s : string;
begin
  s := '';
  result := false;
  if ( CountExportedNotes < 1 ) then
  begin
    s := 'You did not select any notes to export.';
  end
  else
  if ( myFolder = '' ) then
  begin
    Edit_Folder.SetFocus;
    s := 'You must specify the directory for exported files.';
  end
  else
  if ( not DirectoryExists( myFolder )) then
  begin
    Edit_Folder.SetFocus;
    s := 'Target directory does not exist.';
  end;

  if ( s <> '' ) then
    messagedlg( 'Cannot begin exporting:' + #13 + s, mtInformation, [mbOK], 0 )
  else
  result := true;
  
end; // VERIFY

procedure TForm_Export.Combo_FormatClick(Sender: TObject);
begin
  case Combo_Format.ItemIndex of
    ord( xfPlainText ), ord( xfTreePad ) : L_TreeHint.Caption := 'Tree notes: Whole tree as a single text file';
    ord( xfRTF ) : L_TreeHint.Caption := 'Tree notes: Each node to separate RTF file';
    else
      L_TreeHint.Caption := '';
  end;
end;


procedure TForm_Export.Button_HelpClick(Sender: TObject);
begin
  Application.HelpCommand( HELP_CONTEXT, self.HelpContext );
  
end;

end.
