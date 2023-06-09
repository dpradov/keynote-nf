{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1998 Master-Bank                }
{                                                       }
{*******************************************************}

unit RxDirFrm;

interface

uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF} Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, RXCtrls,
  Placemnt;

type
  TDirectoryListDialog = class(TForm)
    DirectoryList: TTextListBox;
    AddBtn: TButton;
    RemoveBtn: TButton;
    ModifyBtn: TButton;
    OKBtn: TButton;
    CancelBtn: TButton;
    Storage: TFormStorage;
    procedure AddBtnClick(Sender: TObject);
    procedure ModifyBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure DirectoryListClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DirectoryListDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure DirectoryListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure CheckButtons;
  public
    { Public declarations }
  end;

function EditFolderList(Folders: TStrings): Boolean;

implementation

uses FileUtil, BoxProcs, RxConst;

{$R *.DFM}

function EditFolderList(Folders: TStrings): Boolean;
begin
  with TDirectoryListDialog.Create(Application) do
  try
    if Assigned(Folders) then
      DirectoryList.Items.Assign(Folders);
    Result := ShowModal = mrOk;
    if Result and Assigned(Folders) then
      Folders.Assign(DirectoryList.Items);
  finally
    Free;
  end;
end;

{ TDirectoryListDialog }

procedure TDirectoryListDialog.CheckButtons;
begin
  ModifyBtn.Enabled := (DirectoryList.Items.Count > 0) and
    (DirectoryList.ItemIndex >= 0);
  RemoveBtn.Enabled := ModifyBtn.Enabled;
end;

procedure TDirectoryListDialog.AddBtnClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  S := '';
  if BrowseDirectory(S, '', 0) then begin
    I := DirectoryList.Items.Add(S);
    DirectoryList.ItemIndex := I;
    CheckButtons;
  end;
end;

procedure TDirectoryListDialog.ModifyBtnClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  I := DirectoryList.ItemIndex;
  if I >= 0 then begin
    S := DirectoryList.Items[I];
    if BrowseDirectory(S, '', 0) then begin
      DirectoryList.Items[I] := S;
    end;
  end;
end;

procedure TDirectoryListDialog.RemoveBtnClick(Sender: TObject);
var
  I: Integer;
begin
  I := DirectoryList.ItemIndex;
  if I >= 0 then begin
    DirectoryList.Items.Delete(I);
    CheckButtons;
  end;
end;

procedure TDirectoryListDialog.DirectoryListClick(Sender: TObject);
begin
  CheckButtons;
end;

procedure TDirectoryListDialog.FormShow(Sender: TObject);
begin
  CheckButtons;
end;

procedure TDirectoryListDialog.DirectoryListDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  BoxMoveFocusedItem(DirectoryList, DirectoryList.ItemAtPos(Point(X, Y), True));
  CheckButtons;
end;

procedure TDirectoryListDialog.DirectoryListDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(DirectoryList, Source, X, Y, State, Accept, DirectoryList.Sorted);
  CheckButtons;
end;

procedure TDirectoryListDialog.FormCreate(Sender: TObject);
begin
{$IFDEF WIN32}
  with Storage do begin
    UseRegistry := True;
    IniFileName := SDelphiKey;
  end;
{$ELSE}
  if not NewStyleControls then Font.Style := [fsBold];
{$ENDIF}
end;

end.
