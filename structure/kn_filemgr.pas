unit kn_filemgr;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2024 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface

uses
   Winapi.Windows,
   Winapi.Messages,
   System.SysUtils,
   System.Classes,
   System.IniFiles,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.ComCtrls,
   Vcl.ExtCtrls,
   RxPlacemnt,
   VirtualTrees, VirtualTrees.BaseTree, VirtualTrees.BaseAncestorVCL, VirtualTrees.AncestorVCL,
   gf_misc,
   kn_Const,
   kn_KntFile
   ;




type
  TKntFileInfo = class( TObject )
    Name : string;
    Comment : string;
    Description : string;
    Size : longint;
    Created : TDateTime;
    Modified : TDateTime;
    Count : integer;
    Format: TKntFileFormat;
    Version : string;
    ImageIndex: integer;
    constructor Create;
  end;
  TKntFileInfoList = TSimpleObjList<TKntFileInfo>;


type
  TForm_KntFileMgr = class(TForm)
    Panel_Btn: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Bevel1: TBevel;
    L_Desc: TLabel;
    L_Comment: TLabel;
    L_Fmt: TLabel;
    L_Date: TLabel;
    L_Count: TLabel;
    Label99: TLabel;
    L_Path: TLabel;
    Label3: TLabel;
    L_Modified: TLabel;
    Button_OK: TButton;
    Button_Cancel: TButton;
    CheckBox_FullPaths: TCheckBox;
    FormPlacement: TFormPlacement;
    TV: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox_FullPathsClick(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt; var CallHelp: Boolean): Boolean;
    procedure TV_CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure TVGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
                        TextType: TVSTTextType; var CellText: string);
    procedure TVGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
                             Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure TVPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
                         Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure TVSelectionChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TVmgrDblClick(Sender: TObject);
    procedure TVDblClick(Sender: TObject);

  private
    function GetFileInfo(Node: PVirtualNode): TKntFileInfo;

  public
    OK_Click : boolean;
    Initializing : boolean;
    MgrFileName : string;
    SelectedFileName : string;
    HaveErrors : integer;
    ShowFullPaths : boolean;

    procedure LoadFileList;
  end;

var
  FileManager : TKntFileInfoList;

function SaveFileManagerInfo( FN : string ) : boolean;
function LoadFileManagerInfo( FN : string ) : boolean;
function AddToFileManager( const FN : string; const aFile : TKntFile ) : boolean;
procedure ClearFileManager;

implementation
uses
   gf_files,  // Important. Needed to use TMemIniFileHelper (.ReadString, .WriteString)
   kn_info,
   kn_Global,
   kn_Chest,
   knt.App,
   knt.RS
   ;

{$R *.DFM}


constructor TKntFileInfo.Create;
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
  Format := low( TKntFileFormat );
end;

function SaveFileManagerInfo( FN : string ) : boolean;
var
  IniFile : TMemIniFile;
  i, cnt : integer;
  Info : TKntFileInfo;
  section : string;
begin

  result := false;
  if (( not assigned( FileManager )) or ( FileManager.Count = 0 )) then exit;

  if ( FN = '' ) then
    FN := changefileext( application.exename, ext_MGR );
  FN := normalFN( FN );

  deletefile( FN ); // better this than removing sections one at a time

  IniFile := TMemIniFile.Create( fn, TEncoding.UTF8 );
  cnt := 0;

  with IniFile do
  try
    try
      for i := 0 to pred( FileManager.Count ) do begin
        Info := FileManager[i];
        if assigned(Info) and fileexists(Info.Name) then begin
          inc( cnt );
          section := inttostr( cnt );
          writestring( section, 'Name', '"' + Info.Name + '"' );
          writestring( section, 'Comment', '"' + Info.Comment + '"' );
          writestring( section, 'Description', '"' + Info.Description + '"' );
          writestring( section, 'Created', FloatToStr( Info.Created )); // save only date, not time
          writestring( section, 'Version', '"' + Info.Version + '"' );
          writeinteger( section, 'Count', Info.Count );
          writeinteger( section, 'Format', ord( Info.Format ));
        end;
      end;
      IniFile.UpdateFile;

    except
      on E : Exception do begin
        messagedlg( E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end;
  finally
    IniFile.Free;
  end;

  result := true;

end;


function LoadFileManagerInfo( FN : string ) : boolean;
var
  IniFile : TMemIniFile;
  i : integer;
  Info : TKntFileInfo;
  s: string;
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

  IniFile := TMemIniFile.Create( fn );
  sections := TStringList.Create;

  with IniFile do
  try
    try
      ReadSections( sections );
      if ( sections.Count > 0 ) then begin
        for i := 0 to pred( sections.count ) do begin
          section := sections[i];
          s := normalFN( readstring( section, 'Name', '' ));
          if ( not FileExists(s)) then continue;

          Info := TKntFileInfo.Create;
          Info.Name := s;
          Info.Comment := readstring( section, 'Comment', '' );
          Info.Description := readstring( section, 'Description', '' );
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
            Info.Format := TKntFileFormat( readinteger( section, 'Format', ord( low( TKntFileFormat ))));
          except
            Info.Format := low( TKntFileFormat );
          end;

          FileManager.Add(Info);

        end;
      end;

    except
      on E : Exception do begin
        App.DoMessageBox( GetRS(sFmg01) + FN + '"' + #13#13 + E.Message, mtError, [mbOK] );
        exit;
      end;
    end;

  finally
    IniFile.Free;
    sections.Free;
  end;

  result := (FileManager.Count > 0);
end;


function GetFileIndex(const FN : string): integer;
var
  i: integer;
begin
   for i:= 0 to FileManager.Count-1 do
      if FileManager[i].Name = FN then
         exit(i);

   Result:= -1;
end;


function AddToFileManager( const FN : string; const aFile : TKntFile ) : boolean;
var
  Info : TKntFileInfo;
  i : integer;
begin
  result := false;
  if not assigned(FileManager) or not assigned(aFile) or not FileExists(FN) then exit;

  Info := nil;
  i := GetFileIndex(FN);

  try
    if ( i >= 0 ) then
      Info := FileManager[i]        // this file is already present in FileManager so just reference the existing storage object
    else
      Info := TKntFileInfo.Create;  // new file, create a new storage object

    with Info do begin
      Name := FN;
      Comment := aFile.Comment;
      Description := aFile.Description;
      Size := 0; // don't keep this
      Created := aFile.DateCreated;
      // Modified := GetFileDateStamp( FN );
      Count := aFile.FolderCount;
      Version := aFile.Version.Major + '.' + aFile.Version.Minor;
      Format := aFile.FileFormat;
    end;

    if ( i < 0 ) then // new object, so add it
      FileManager.Add(Info);

    result := true;

  except
    on E : Exception do begin
      if ( i < 0 ) then // new object, discard it
      begin
        if assigned( Info ) then Info.Free;
      end;
      messagedlg( E.Message, mtError, [mbOK], 0 );
    end;
  end;

end;


procedure TForm_KntFileMgr.FormCreate(Sender: TObject);
begin
  Initializing := true;
  App.ApplyBiDiModeOnForm(Self);

  with FormPlacement do begin
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
  with TV do
    SetWindowLong( Handle,
      GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or $80 );
end;

procedure TForm_KntFileMgr.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  Initializing := false;
  CheckBox_FullPaths.Checked := ShowFullPaths;

  screen.Cursor := crHourGlass;
  try
    try
      LoadFileList;
      TV.SortTree(-1, sdAscending);
    except
      on E : Exception do begin
        messagedlg( GetRS(sFmg02) + E.Message, mtError, [mbOK], 0 );
        ModalResult := mrCancel;
      end;
    end;

  finally
    screen.Cursor := crDefault;
    if (TV.TotalCount > 0) then
      CheckBox_FullPaths.OnClick := CheckBox_FullPathsClick
    else
      CheckBox_FullPaths.OnClick := nil;

    Caption := Format( GetRS(sFmg03), [TV.TotalCount] );
  end;

end;


function TForm_KntFileMgr.GetFileInfo(Node: PVirtualNode): TKntFileInfo;
begin
  Result:= TV.GetNodeData<TKntFileInfo>(Node);
end;



procedure TForm_KntFileMgr.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Info : TKntFileInfo;
begin
  SelectedFileName := '';

  if OK_Click then begin
     if assigned(TV.FocusedNode) then begin
        Info := GetFileInfo(TV.FocusedNode);
        if (Info.ImageIndex = NODEIMG_INVALID ) then begin
          messagedlg( format(GetRS(sFmg04),[Program_Name]), mtInformation, [mbOK], 0 );
          CanClose := false;
        end
        else
          SelectedFileName := Info.Name;
     end;
  end;
  OK_Click := false;
end;

procedure TForm_KntFileMgr.FormKeyDown(Sender: TObject; var Key: Word;
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


procedure TForm_KntFileMgr.Button_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;

procedure TForm_KntFileMgr.Button_CancelClick(Sender: TObject);
begin
  OK_Click := false;
end;

procedure TForm_KntFileMgr.FormDestroy(Sender: TObject);
begin
  TV.OnChange := nil;
end;

function TForm_KntFileMgr.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;


procedure TForm_KntFileMgr.TV_CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  F1, F2: TKntFileInfo;
begin
  F1:= GetFileInfo(Node1);
  F2:= GetFileInfo(Node2);
  Result := CompareText(F1.Name, F2.Name);
end;


procedure TForm_KntFileMgr.LoadFileList;
var
  i : integer;
  TVSelNode, Node : PVirtualNode;
  Info : TKntFileInfo;
begin

  Node := nil;
  TVSelNode := nil;

  if ( FileManager.Count = 0 ) then begin
    messagedlg( GetRS(sFmg05), mtInformation, [mbOK], 0 );
    PostMessage( Self.Handle, WM_CLOSE, 0, 0 );
    exit;
  end;

  TV.BeginUpdate;

  try
    try
      for i := 0 to pred( FileManager.Count ) do begin
          Info := FileManager[i];
          if assigned(Info) then begin
            Node := TV.AddChild(nil);
            if (SelectedFileName = Info.Name) then
                TVSelNode := Node;

            if FileExists(Info.Name) then begin
              case Info.Format of
                nffKeyNote :    Info.ImageIndex := NODEIMG_TKN;
                nffKeyNoteZip : Info.ImageIndex := NODEIMG_TKNZIP;
                nffEncrypted :  Info.ImageIndex := NODEIMG_ENC;
              end;
            end
            else begin
              Info.ImageIndex := NODEIMG_INVALID;
              HaveErrors := 0;
            end;

            TV.SetNodeData<TKntFileInfo>(Node, Info);
          end;
      end;

    except
      on E : Exception do begin
        messagedlg( E.Message, mtInformation, [mbOK], 0 );
        exit;
      end;
    end;

  finally
    TV.EndUpdate;
    if (TV.TotalCount > 0) then begin
      if assigned(TVSelNode) then begin
        TV.FocusedNode := TVSelNode;
      end
      else
        TV.FocusedNode := TV.GetFirst;;
    end;
    TV.Selected[TV.FocusedNode]:= True;
    TV.SetFocus;
  end;

end;


procedure TForm_KntFileMgr.TVSelectionChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Info : TKntFileInfo;
  ModDate : TDateTime;
begin
  if ( not assigned( Node )) then exit;
  Info := GetFileInfo(Node);
  if assigned( Info ) then begin
    L_Desc.Caption := Info.Description;
    L_Comment.Caption := Info.Comment;
    L_Path.Caption := ExtractFilepath( Info.Name );
    L_Fmt.Caption := FILE_FORMAT_NAMES[Info.Format];
    L_Date.Caption := FormatDateTime( FormatSettings.ShortDateFormat {+ #32 + ShortTimeFormat}, Info.Created );
    L_Count.Caption := inttostr( Info.Count );

    ModDate := GetFileDateStamp( Info.Name );
    if ( ModDate <> 0 ) then
      L_Modified.Caption := FormatDateTime( FormatSettings.ShortDateFormat + #32 + FormatSettings.ShortTimeFormat, ModDate )
    else
      L_Modified.Caption := GetRS(sFmg06);
  end
  else begin
    HaveErrors := 0;
    case Info.ImageIndex of
      NODEIMG_TKN, NODEIMG_DART, NODEIMG_ENC : L_Desc.Caption := GetRS(sFmg07);
      else
        L_Desc.Caption := GetRS(sFmg08);
    end;

    L_Comment.Caption := '';
    L_Path.Caption := '';
    L_Fmt.Caption := '';
    L_Date.Caption := '';
    L_Count.Caption := '';
    L_Modified.Caption := '';
  end;
end;

procedure TForm_KntFileMgr.TVDblClick(Sender: TObject);
begin
  OK_Click := true;
  ModalResult := mrOK;
end;

procedure TForm_KntFileMgr.TVGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  Info: TKntFileInfo;
begin
  if not (Kind in [TVTImageKind.ikNormal, TVTImageKind.ikSelected]) then exit;
  Info:= GetFileInfo(Node);
  ImageIndex:= Info.ImageIndex;
end;

procedure TForm_KntFileMgr.TVGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Info: TKntFileInfo;
begin
  Info:= GetFileInfo(Node);

  if ShowFullPaths then
    CellText := Info.Name
  else
    CellText := ExtractFilename(Info.Name);
end;


procedure TForm_KntFileMgr.TVPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Info: TKntFileInfo;
begin
  Info:= GetFileInfo(Node);

  if (SelectedFileName = Info.Name) then
     TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
end;

procedure ClearFileManager;
var
  i : integer;
begin
  if assigned( FileManager ) then begin
    if ( FileManager.Count > 0 ) then
      for i := 0 to pred( FileManager.Count ) do
        FileManager[i].Free;
    FileManager.Clear;
  end;
end;

procedure FreeFileManager;
begin
  ClearFileManager;
  if assigned( FileManager ) then
    FileManager.Free;
end;

procedure TForm_KntFileMgr.CheckBox_FullPathsClick(Sender: TObject);
begin
  ShowFullPaths := CheckBox_FullPaths.Checked;
  TV.SortTree(-1, sdAscending);
  TV.Invalidate;
end;




Initialization
 FileManager := TKntFileInfoList.Create;

Finalization
  FreeFileManager;

end.
