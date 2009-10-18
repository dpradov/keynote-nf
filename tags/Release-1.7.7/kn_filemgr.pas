
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

unit kn_filemgr;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, WideStrings,
  StdCtrls, ComCtrls, gf_misc, kn_info,
  kn_Const, kn_NoteObj, kn_FileObj,
  kn_Chest, gf_files,
  IniFiles, ExtCtrls, TreeNT, Placemnt, TntStdCtrls;



type
  TFileInfo = class( TObject )
    Name : wideString;
    Comment : wideString;
    Description : wideString;
    Size : longint;
    Created : TDateTime;
    Modified : TDateTime;
    Count : integer;
    Format: TNoteFileFormat;
    Version : string;
    constructor Create;
  end;

type
  TForm_FileMgr = class(TForm)
    Panel_Btn: TPanel;
    TVmgr: TTreeNT;
    Label1: TTntLabel;
    Label2: TTntLabel;
    Label4: TTntLabel;
    Label5: TTntLabel;
    Label7: TTntLabel;
    Bevel1: TBevel;
    L_Desc: TTntLabel;
    L_Comment: TTntLabel;
    L_Fmt: TTntLabel;
    L_Date: TTntLabel;
    L_Count: TTntLabel;
    Label99: TTntLabel;
    L_Path: TTntLabel;
    Label3: TTntLabel;
    L_Modified: TTntLabel;
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    CheckBox_FullPaths: TTntCheckBox;
    FormPlacement: TFormPlacement;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TVChange(Sender: TObject; Node: TTreeNTNode );
    procedure TVmgrDblClick(Sender: TObject);
    procedure CheckBox_FullPathsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    OK_Click : boolean;
    Initializing : boolean;
    MgrFileName : string;
    SelectedFileName : wideString;
    HaveErrors : integer;
    ShowFullPaths : boolean;

    procedure LoadFileList;
    procedure RefreshFileList;
  end;

var
  FileManager : TWideStringList;

function SaveFileManagerInfo( FN : string ) : boolean;
function LoadFileManagerInfo( FN : string ) : boolean;
function AddToFileManager( const FN : wideString; const aFile : TNoteFile ) : boolean;
procedure ClearFileManager;

implementation
uses kn_main, TntSysUtils, gf_miscvcl, gf_streams;

{$R *.DFM}

resourcestring
  STR_01 = 'Loading file manager from "';
  STR_02 = 'Error initializing FileManager: ';
  STR_03 = 'Notes file manager: %d file(s)';
  STR_04 = 'This file cannot be selected because it does not exist or contains data in a format that %s does not support. Please select another file.';
  STR_05 = 'FileManager list is empty. This dialog box will now close.';
  STR_06 = 'never';
  STR_07 = 'No information is available about this file.';
  STR_08 = 'This file does not exist or cannot be accessed.';


constructor TFileInfo.Create;
begin
  inherited Create;
  Name := '';
  Comment := '';
  Description := '';
  Size := 0;
  Created := now;
  Modified := 0;
  Version := NFILEVERSION_MAJOR + '.' + NFILEVERSION_MINOR;
  Count := 0;
  Format := low( TNoteFileFormat );
end; // TFileInfo.CREATE

function SaveFileManagerInfo( FN : string ) : boolean;
var
  IniFile : TWIniFile;
  i, cnt : integer;
  Info : TFileInfo;
  section : string;
begin

  result := false;
  if (( not assigned( FileManager )) or ( FileManager.Count = 0 )) then exit;

  if ( FN = '' ) then
    FN := changefileext( application.exename, ext_MGR );
  FN := normalFN( FN );

  deletefile( FN ); // better this than removing sections one at a time

  IniFile := TWIniFile.Create( fn );
  cnt := 0;

  with IniFile do
  try
    try
      for i := 0 to pred( FileManager.Count ) do
      begin
        Info := TFileInfo( FileManager.Objects[i] );
        if ( assigned( Info ) and fileexists( Info.Name )) then
        begin
          inc( cnt );
          section := inttostr( cnt );
          writestringW( section, 'Name', '"' + Info.Name + '"' );
          writestringW( section, 'Comment', '"' + Info.Comment + '"' );
          writestringW( section, 'Description', '"' + Info.Description + '"' );
          writestring( section, 'Created', FloatToStr( Info.Created )); // save only date, not time
          writestring( section, 'Version', '"' + Info.Version + '"' );
          writeinteger( section, 'Count', Info.Count );
          writeinteger( section, 'Format', ord( Info.Format ));
        end;
      end;
    except
      on E : Exception do
      begin
        messagedlg( E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end;
  finally
    IniFile.Free;
  end;

  result := true;

end; // SaveFileManagerInfo

function LoadFileManagerInfo( FN : string ) : boolean;
var
  IniFile : TWIniFile;
  i : integer;
  Info : TFileInfo;
  s: wideString;
  section : string;
  sections : TStringList;
begin
  result := false;
  if ( not assigned( FileManager )) then exit;

  if ( FN = '' ) then
    FN := changefileext( application.exename, ext_MGR );
  FN := normalFN( FN );

  if ( not fileexists( FN )) then exit;

  ClearFileManager;

  IniFile := TWIniFile.Create( fn );
  sections := TStringList.Create;

  with IniFile do
  try
    try
      ReadSections( sections );
      if ( sections.Count > 0 ) then
      begin
        for i := 0 to pred( sections.count ) do
        begin
          section := sections[i];
          s := normalFN( readstringW( section, 'Name', '' ));
          if ( not WideFileexists( s )) then continue;

          Info := TFileInfo.Create;
          Info.Name := s;
          Info.Comment := readstringW( section, 'Comment', '' );
          Info.Description := readstringW( section, 'Description', '' );
          Info.Version := readstring( section, 'Version', '' );
          try
            Info.Created := StrToFloat( readstring( section, 'Created', '' ));
          except
            Info.Created := now;
          end;
          try
            Info.Count := readinteger( section, 'Count', 0 );
          except
            Info.Count := 0;
          end;
          try
            Info.Format := TNoteFileFormat( readinteger( section, 'Format', ord( low( TNoteFileFormat ))));
          except
            Info.Format := low( TNoteFileFormat );
          end;

          FileManager.AddObject( s, Info );

        end;
      end;

    except
      on E : Exception do
      begin
        DoMessageBox( STR_01 + FN + '"' + #13#13 + E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end;
  finally
    IniFile.Free;
    sections.Free;
  end;

  result := ( FileManager.Count > 0 );
end; // LoadFileManagerInfo

function AddToFileManager( const FN : wideString; const aFile : TNoteFile ) : boolean;
var
  Info : TFileInfo;
  i : integer;
begin
  result := false;
  if (( not assigned( FileManager )) or
      ( not assigned( aFile )) or
      ( not WideFileexists( FN ))) then exit;

  Info := nil;
  i := FileManager.IndexOf( FN );

  try
    if ( i >= 0 ) then
    begin
      // this file is already present in FileManager
      // so just reference the existing storage object
      Info := TFileInfo( FileManager.Objects[i] );
    end
    else
    begin
      // new file, create a new storage object
      Info := TFileInfo.Create;
    end;

    with Info do
    begin
      Name := FN;
      Comment := aFile.Comment;
      Description := aFile.Description;
      Size := 0; // don't keep this
      Created := aFile.DateCreated;
      // Modified := GetFileDateStamp( FN );
      Count := aFile.NoteCount;
      Version := aFile.Version.Major + '.' + aFile.Version.Minor;
      Format := aFile.FileFormat;
    end;

    if ( i < 0 ) then // new object, so add it
    begin
      FileManager.AddObject( FN, Info );
    end;

    result := true;

  except
    on E : Exception do
    begin
      if ( i < 0 ) then // new object, discard it
      begin
        if assigned( Info ) then Info.Free;
      end;
      messagedlg( E.Message, mtError, [mbOK], 0 );
    end;
  end;

end; // AddToFileManager


procedure TForm_FileMgr.FormCreate(Sender: TObject);
begin
  Initializing := true;
  with FormPlacement do
  begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;
  OK_Click := false;

  L_Desc.Font.Color := clHighlight;
  L_Comment.Font.Color := clHighlight;
  L_Path.Font.Color := clHighlight;
  L_Fmt.Font.Color := clHighlight;
  L_Date.Font.Color := clHighlight;
  L_Count.Font.Color := clHighlight;
  L_Modified.Font.Color := clHighlight;

  HaveErrors := 0;
  SelectedFileName := '';
  ShowFullPaths := false;
  with TVmgr do
    SetWindowLong( Handle,
      GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or $80 );
end; // FORM_CREATE

procedure TForm_FileMgr.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  Initializing := false;
  CheckBox_FullPaths.Checked := ShowFullPaths;

  screen.Cursor := crHourGlass;
  try
    try
      LoadFileList;
      TVmgr.AlphaSort;
    except
      on E : Exception do
      begin
        messagedlg( STR_02 + E.Message, mtError, [mbOK], 0 );
        ModalResult := mrCancel;
      end;
    end;
  finally
    screen.Cursor := crDefault;
    if ( TVmgr.Items.Count > 0 ) then
    begin
      TVmgr.OnChange := TVChange;
      CheckBox_FullPaths.OnClick := CheckBox_FullPathsClick;
    end
    else
    begin
      TVmgr.OnChange := nil;
      CheckBox_FullPaths.OnClick := nil;
    end;
    Caption := Format( STR_03, [TVmgr.Items.Count] );
  end;
end; // FORM_ACTIVATE

procedure TForm_FileMgr.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  Info : TFileInfo;
begin
  SelectedFileName := '';
  if OK_Click then
  begin
    if assigned( TVmgr.Selected ) then
    begin
      if ( TVmgr.Selected.ImageIndex = NODEIMG_INVALID ) then
      begin
        messagedlg( format(STR_04,[Program_Name]), mtInformation, [mbOK], 0 );
        CanClose := false;
      end
      else
      begin
        Info := TFileInfo( TVmgr.Selected.Data );
        if assigned ( Info ) then
          SelectedFileName := Info.Name;
      end;
    end;
  end;
  OK_Click := false;
end; // FORM_CLOSEQUERY

procedure TForm_FileMgr.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if ( shift = [] ) then
    begin
      key := 0;
      OK_Click := false;
      ModalResult := mrCancel;
    end;
    VK_BACK : begin
      key := 0;
      CheckBox_FullPaths.Checked := ( not CheckBox_FullPaths.Checked );
    end;
  end;
end; // FORM_KEYDOWN


procedure TForm_FileMgr.Button_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;

procedure TForm_FileMgr.Button_CancelClick(Sender: TObject);
begin
  OK_Click := false;
end;

procedure TForm_FileMgr.FormDestroy(Sender: TObject);
begin
  TVmgr.OnChange := nil;
end; // FORM_DESTROY

procedure TForm_FileMgr.RefreshFileList;
var
  Info : TFileInfo;
  Node : TTreeNTNode;
begin

  if ( TVmgr.Items.Count = 0 ) then exit;

  screen.Cursor := crHourGlass;
  TVmgr.Items.BeginUpdate;
  Node := TVmgr.Items.GetFirstNode;
  try
    while assigned( Node ) do
    begin
      Info := TFileInfo( Node.Data );
      if assigned( Info ) then
      begin
        if ShowFullPaths then
        begin
          Node.Text := Info.Name;
        end
        else
        begin
          Node.Text := WideExtractFilename( Info.Name );
        end;
      end;
      Node := Node.GetNext;
    end;
  finally
    TVmgr.AlphaSort;
    TVmgr.Items.EndUpdate;
    screen.Cursor := crDefault;
  end;

  // Caption := inttostr( i ) + ' out of ' + inttostr( TVmgr.Items.Count ) + ' nodes (' + inttostr( u ) + ' nils)';

end; // RefreshFileList

procedure TForm_FileMgr.LoadFileList;
var
  i : integer;
  TVSelNode, Node : TTreeNTNode;
  s : wideString;
  Info : TFileInfo;
begin

  Node := nil;
  TVSelNode := nil;

  if ( FileManager.Count = 0 ) then
  begin
    messagedlg( STR_05, mtInformation, [mbOK], 0 );
    PostMessage( Self.Handle, WM_CLOSE, 0, 0 );
    exit;
  end;

  TVmgr.Items.BeginUpdate;
  try
    try
      for i := 0 to pred( FileManager.Count ) do
      begin
        Info := TFileInfo( FileManager.Objects[i] );
        try
          if assigned( Info ) then
          begin
            if ShowFullPaths then
              s := Info.Name
            else
              s := WideExtractFilename( Info.Name );
            Node := TVmgr.Items.Add( nil, s );
            if ( SelectedFileName = Info.Name ) then
              TVSelNode := Node;
            if WideFileexists( Info.Name ) then
            begin
              case Info.Format of
                nffKeyNote : Node.ImageIndex := NODEIMG_TKN;
                nffEncrypted : Node.ImageIndex := NODEIMG_ENC;
                nffDartNotes : Node.ImageIndex := NODEIMG_DART;
              end;
              Node.Data := Info;
            end
            else
            begin
              Node.ImageIndex := NODEIMG_INVALID;
              Node.Data := nil;
              HaveErrors := 0;
            end;
            Node.SelectedIndex := Node.ImageIndex;
          end;
        except
          if assigned( Node ) then Node.Free;
          raise;
        end;
      end;
    except
      on E : Exception do
      begin
        messagedlg( E.Message, mtInformation, [mbOK], 0 );
        exit;
      end;
    end;
  finally
    TVmgr.Items.EndUpdate;
    if ( TVmgr.Items.Count > 0 ) then
    begin
      if assigned( TVSelNode ) then
      begin
        TVmgr.Selected := TVSelNode;
        TVmgr.Selected.Font.Style := [fsBold];
      end
      else
      begin
        TVmgr.Selected := TVmgr.Items[0];
      end;
    end;
    TVmgr.SetFocus;
  end;

  TVChange( self, TVmgr.Selected );


end;

// LoadFileList;

procedure TForm_FileMgr.TVChange(Sender: TObject; Node: TTreeNTNode);
var
  Info : TFileInfo;
  ModDate : TDateTime;
begin
  if ( not assigned( Node )) then exit;
  Info := TFileInfo( Node.Data );
  if assigned( Info ) then
  begin
    L_Desc.Caption := Info.Description;
    L_Comment.Caption := Info.Comment;
    L_Path.Caption := WideExtractfilepath( Info.Name );
    L_Fmt.Caption := FILE_FORMAT_NAMES[Info.Format];
    L_Date.Caption := FormatDateTime( ShortDateFormat {+ #32 + ShortTimeFormat}, Info.Created );
    L_Count.Caption := inttostr( Info.Count );

    ModDate := GetFileDateStamp( Info.Name );
    if ( ModDate <> 0 ) then
      L_Modified.Caption := FormatDateTime( ShortDateFormat + #32 + ShortTimeFormat, ModDate )
    else
      L_Modified.Caption := STR_06;

    {
    if ( Info.Version <> '' ) then
      L_Version.Caption := Info.Version
    else
      L_Version.Caption := 'unknown';
    }
    {
    if (( Info.Version <> '' ) and ( Info.Version[1] <> NFILEVERSION_MAJOR )) then
    begin
      L_Version.Font.Color := clWhite;
      L_Version.Font.Style := [fsBold];
      L_Version.Color := clRed;
    end
    else
    begin
      L_Version.ParentColor := true;
      L_Version.ParentFont := true;
      L_Version.Font.Color := clHighlight;
    end;
    }

  end
  else
  begin
    HaveErrors := 0;
    case Node.ImageIndex of
      NODEIMG_TKN, NODEIMG_DART, NODEIMG_ENC : begin
        L_Desc.Caption := STR_07;
      end;
      else
      begin
        L_Desc.Caption := STR_08;
      end;
    end;

    L_Comment.Caption := '';
    L_Path.Caption := '';
    L_Fmt.Caption := '';
    L_Date.Caption := '';
    L_Count.Caption := '';
    L_Modified.Caption := '';
  end;
end; // TVCHange


procedure TForm_FileMgr.TVmgrDblClick(Sender: TObject);
begin
  OK_Click := true;
  ModalResult := mrOK;
end;

procedure ClearFileManager;
var
  i : integer;
begin
  if assigned( FileManager ) then
  begin
    if ( FileManager.Count > 0 ) then
    begin
      for i := 0 to pred( FileManager.Count ) do
        FileManager.Objects[i].Free;
    end;
  FileManager.Clear;
  end;
end; // ClearFileManager

procedure FreeFileManager;
begin
  ClearFileManager;
  if assigned( FileManager ) then
    FileManager.Free;
end;

procedure TForm_FileMgr.CheckBox_FullPathsClick(Sender: TObject);
begin
  ShowFullPaths := CheckBox_FullPaths.Checked;
  RefreshFileList;
end;




Initialization
  FileManager := TWideStringList.Create;
  FileManager.Sorted := true;
  FileManager.Duplicates := dupIgnore;

Finalization
  FreeFileManager;

end.
