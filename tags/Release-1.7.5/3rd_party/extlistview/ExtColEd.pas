{$I DFS.INC}

unit ExtColEd;
(******************************************************************************
*
* Description :  A Property Editor for the TdfsExtListColumns class
*
* Author      : Mike Lindre
* Edited for use with TdfsExtListView v3.00 with Delphi 2 by Brad Stowers
*
*
******************************************************************************)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtListView,
{$IFDEF DFS_NO_DSGNINTF}     // [dpv]
    DesignIntf,
    DesignEditors;
{$ELSE}
    DsgnIntf;
{$ENDIF}

type
  TfrmExtListColumns = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    edtImageIndex: TEdit;
    GroupBox3: TGroupBox;
    lbColumns: TListBox;
    btnNew: TButton;
    btnDelete: TButton;
    cmbAlignment: TComboBox;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lbColumnsClick(Sender: TObject);
    procedure edtImageIndexChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbAlignmentChange(Sender: TObject);
  private
    { Private declarations }
    Updating: Boolean;
    FColumns: TdfsExtListColumns;
//    Str: TStrings;
    function  GetExtColumns: TdfsExtListColumns;
    procedure SetExtColumns(Value: TdfsExtListColumns);
    procedure RefreshList;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateComboBox;
  public
    { Public declarations }
    property ExtColumns: TdfsExtListColumns read GetExtColumns write SetExtColumns;
  end;

  {The TClassProperty object is the default property editor for all
   properties which are themselves objects. Users cannot modify object-type
   properties directly, but the editor displays the name of the object type,
   and allows editing of the object's properties as subproperties of the property.}

  {This is the Animation Property Class
   Edit - Will Copy the Current TListColBitMaps Object in the Component
          and display it in an Editor.

          This Editor will allow the Object to be replaced or cleared

   GetAttributes - Select and Edit TFrame Object only using
                   a dialog box
  }

  TdfsExtListColumnsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

implementation

{$R *.DFM}

uses ComCtrls,CommCtrl;

Const
  STR_LEFT_JUST  = 0;
  STR_RIGHT_JUST = 1;
  ALIGN_STRS : array[STR_LEFT_JUST..STR_RIGHT_JUST] of string =
     ('Left of Text','Right of Text');


{  TdfsExtListColumnsProperty}
procedure TdfsExtListColumnsProperty.Edit;
var
  frmExtListColumns: TfrmExtListColumns;
  AListColumns: TdfsExtListColumns;
begin
  AListColumns := TdfsExtListColumns(GetOrdValue);
  frmExtListColumns := TfrmExtListColumns.Create(Application);
  try
    frmExtListColumns.ExtColumns := AListColumns;
    frmExtListColumns.ShowModal;
    {If OK is pressed re-Copy the Object from the Editor to the Component}
    If frmExtListColumns.ModalResult = mrOK then
    begin
       AListColumns.Assign(frmExtListColumns.ExtColumns);
       Modified;
    end;
  finally
    frmExtListColumns.Free;
  end;

end;

function TdfsExtListColumnsProperty.GetAttributes: TPropertyAttributes;
begin
  {The property Can only be Edited using a Dialog}
  Result := [paDialog];
end;

procedure TfrmExtListColumns.FormCreate(Sender: TObject);
begin
  FColumns := TdfsExtListColumns.Create(nil);
  Updating  := False;
end;

procedure TfrmExtListColumns.FormDestroy(Sender: TObject);
begin
  FColumns.Free;
end;

function  TfrmExtListColumns.GetExtColumns:TdfsExtListColumns;
begin
 Result := FColumns;
end;

procedure TfrmExtListColumns.SetExtColumns(Value:TdfsExtListColumns);
begin
  FColumns.Assign(Value);
  RefreshList;
end;

procedure TfrmExtListColumns.RefreshList;
var
  Count: integer;
  Align, Space:String;
begin
  lbColumns.Items.Clear;
  for Count := 0 to FColumns.Count - 1 do
  begin
    if FColumns[Count].ImageIndex <> -1 then
    begin
      if FColumns[Count].ImageAlignment = ciaLeftOfText then
        Align := ALIGN_STRS[STR_LEFT_JUST]
      else
        Align := ALIGN_STRS[STR_RIGHT_JUST];
      if FColumns[Count].ImageIndex > 9 then
        Space := '   '
      else
        Space := '     ';
      lbColumns.Items.Add(IntToStr(Count) + ' - ' + 'Image    Index No: ' +
         IntToStr(FColumns[Count].ImageIndex) + Space + 'Align: ' + Align);
    end else
      lbColumns.Items.Add(IntToStr(Count) + ' - ' + 'No Image');
  end;
end;

procedure TfrmExtListColumns.btnNewClick(Sender: TObject);
begin
  FColumns.Add;
  RefreshList;
  SendMessage(lbColumns.Handle,LB_SETCURSEL,lbColumns.Items.Count-1,0);
  lbColumnsClick(nil);
end;

procedure TfrmExtListColumns.btnDeleteClick(Sender: TObject);
begin
  if lbColumns.ItemIndex <> -1 then
  begin
    FColumns[lbColumns.ItemIndex].Free;
    RefreshList;
    SendMessage(lbColumns.Handle,LB_SETCURSEL,lbColumns.Items.Count-1,0);
    lbColumnsClick(nil);
  end;
end;

procedure TfrmExtListColumns.lbColumnsClick(Sender: TObject);
begin
  BeginUpdate;
  try
    if lbColumns.ItemIndex <> -1 then
    begin
      edtImageIndex.Text := InttoStr(FColumns[lbColumns.ItemIndex].ImageIndex);
      UpdateComboBox;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TfrmExtListColumns.UpdateComboBox;
begin
  if FColumns[lbColumns.ItemIndex].ImageIndex = -1 then
  begin
    cmbAlignment.Enabled := False;
    SendMessage(cmbAlignment.Handle,CB_SETCURSEL,0,0);
  end else begin
    cmbAlignment.Enabled := True;
    SendMessage(cmbAlignment.Handle,CB_SETCURSEL,
       longint(FColumns[lbColumns.ItemIndex].ImageAlignment),0);
  end;
end;

procedure TfrmExtListColumns.edtImageIndexChange(Sender: TObject);
var
  X, Index: Integer;
begin
  if not Updating and (lbColumns.ItemIndex <> -1) then
  begin
    try
      X := StrToInt(edtImageIndex.Text);
      Index := lbColumns.ItemIndex;
      FColumns[Index].ImageIndex := X;
      RefreshList;
      SendMessage(lbColumns.Handle,LB_SETCURSEL,Index,0);
      UpdateComboBox;
    except
      // do nothing
    end;
  end;
end;

procedure TfrmExtListColumns.BeginUpdate;
begin
  Updating := True;
end;

procedure TfrmExtListColumns.EndUpdate;
begin
  Updating := False;
end;

procedure TfrmExtListColumns.FormShow(Sender: TObject);
begin
  if lbColumns.Items.Count > 0 then
  begin
    SendMessage(lbColumns.Handle,LB_SETCURSEL,0,0);
    lbColumnsClick(nil);
  end;
end;

procedure TfrmExtListColumns.cmbAlignmentChange(Sender: TObject);
var
  Index: Integer;
  Align: TColumnImageAlign;
begin
  if not Updating and (lbColumns.ItemIndex <> -1) then
  begin
    try
      if CompareText(cmbAlignment.Text,ALIGN_STRS[STR_LEFT_JUST]) = 0 then
        Align := ciaLeftOfText
      else
        Align := ciaRightOfText;
      Index := lbColumns.ItemIndex;
      FColumns[Index].ImageAlignment := Align;
      RefreshList;
      SendMessage(lbColumns.Handle,LB_SETCURSEL,Index,0);
    except
      // do nothing
    end;
  end;
end;

end.
