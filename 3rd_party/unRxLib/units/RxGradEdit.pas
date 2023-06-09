{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX)                        }
{                                                       }
{     Copyright (c) 1998 Master-Bank                    }
{     Copyright (c) 1998 Ritting Information Systems    }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxGradEdit;

{$I RX.INC}

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, RxToolEdit, RxGrdCpt, RXCtrls, RxPlacemnt,
  {$IFDEF RX_D6}DesignIntf, DesignEditors{$ELSE}DsgnIntf{$ENDIF}; // Polaris

{$IFNDEF RX_D4}
type
  IDesigner = TDesigner;
  {$ENDIF}

type
  TGradCaptionsEditor = class(TForm)
    ApplyButton: TButton;
    CancelButton: TButton;
    OkButton: TButton;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    CaptionText: TEdit;
    CaptionInactiveColor: TComboBox;
    GroupBox1: TGroupBox;
    CaptionList: TTextListBox;
    NewButton: TButton;
    DeleteButton: TButton;
    CaptionParentFont: TCheckBox;
    CaptionGlueNext: TCheckBox;
    CaptionVisible: TCheckBox;
    Label2: TLabel;
    CaptionFont: TComboEdit;
    GradientCaption: TRxGradientCaption;
    FontDialog: TFontDialog;
    ColorDialog: TColorDialog;
    FormStorage: TFormStorage;
    procedure FormCreate(Sender: TObject);
    procedure CaptionListClick(Sender: TObject);
    procedure CaptionListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CaptionListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure NewButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure CaptionInactiveColorDblClick(Sender: TObject);
    procedure ControlExit(Sender: TObject);
    procedure CaptionTextChange(Sender: TObject);
    procedure CaptionFontButtonClick(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
  private
    { Private declarations }
    FComponent: TRxGradientCaption;
    FDesigner: IDesigner;
    FUpdating: Boolean;
    procedure AddColorItem(const ColorName: string);
    procedure EnableControls(Enable: Boolean);
    procedure UpdateCaptionList(Index: Integer);
    procedure ReadControls;
    procedure UpdateControls;
    procedure ClearControls;
    function GetActiveCaption: TRxCaption;
    procedure ApplyChanges;
  public
    { Public declarations }
    procedure SetGradientCaption(Component: TRxGradientCaption;
      Designer: IDesigner);
    property ActiveCaption: TRxCaption read GetActiveCaption;
  end;

{ TGradientCaptionEditor }

  TGradientCaptionEditor = class(TComponentEditor)
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {$IFNDEF RX_D3}

{ TGradientCaptionsProperty }

  TGradientCaptionsProperty = class(TClassProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  {$ENDIF}

function EditGradientCaption(Component: TRxGradientCaption;
  ADesigner: IDesigner): Boolean; // Polaris  Designer -> ADesigner

implementation

uses
  RxVCLUtils, RxBoxProcs, RxConst, RxResConst;

{$R *.DFM}

function EditGradientCaption(Component: TRxGradientCaption;
  ADesigner: IDesigner): Boolean;
var
  loc: TGradCaptionsEditor;
begin
  loc := TGradCaptionsEditor.Create(Application);
  try
    loc.SetGradientCaption(Component, ADesigner); // Polaris  Designer -> ADesigner
    Result := loc.ShowModal = mrOk;
  finally
    loc.Free;
  end;
end;

{ TGradientCaptionEditor }

procedure TGradientCaptionEditor.Edit;
begin
  EditGradientCaption(TRxGradientCaption(Component), Designer);
end;

procedure TGradientCaptionEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then Edit;
end;

function TGradientCaptionEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := RxLoadStr(srCaptionDesigner)
  else
    Result := '';
end;

function TGradientCaptionEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$IFNDEF RX_D3}

{ TGradientCaptionsProperty }

function TGradientCaptionsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TGradientCaptionsProperty.Edit;
begin
  if EditGradientCaption(TRxGradientCaption(GetComponent(0)), Designer) then
    Modified;
end;

{$ENDIF RX_D3}

{ TGradCaptionsEditor }

procedure TGradCaptionsEditor.UpdateCaptionList(Index: Integer);
var
  I, Save: Integer;
begin
  if Index >= 0 then
    Save := Index
  else
    Save := CaptionList.ItemIndex;
  CaptionList.Items.BeginUpdate;
  try
    CaptionList.Items.Clear;
    for I := 0 to GradientCaption.Captions.Count - 1 do
      CaptionList.Items.Add(Format('%s[%d]', [RxLoadStr(srGradientCaptions), I]));
    if Save < 0 then Save := 0;
    if Save >= CaptionList.Items.Count then
      Save := CaptionList.Items.Count - 1;
  finally
    CaptionList.Items.EndUpdate;
    CaptionList.ItemIndex := Save;
  end;
end;

function TGradCaptionsEditor.GetActiveCaption: TRxCaption;
var
  I: Integer;
begin
  Result := nil;
  I := CaptionList.ItemIndex;
  if (I >= 0) and (I < GradientCaption.Captions.Count) then
    Result := GradientCaption.Captions[I];
end;

procedure TGradCaptionsEditor.SetGradientCaption(Component: TRxGradientCaption;
  Designer: IDesigner);
begin
  FComponent := Component;
  FDesigner := Designer;
  if Component <> nil then
  begin
    with GradientCaption do
    begin
      Active := False;
      Font := Component.Font;
      DefaultFont := Component.DefaultFont;
      FontInactiveColor := Component.FontInactiveColor;
      GradientActive := Component.GradientActive;
      GradientInactive := Component.GradientInactive;
      StartColor := Component.StartColor;
      HideDirection := Component.HideDirection;
      GradientSteps := Component.GradientSteps;
      Captions := Component.Captions;
      if Component.Name <> '' then
        FormCaption := Format('%s.%s', [Component.Name,
          RxLoadStr(srGradientCaptions)])
      else
        FormCaption := Format('%s.%s', [Component.ClassName,
          RxLoadStr(srGradientCaptions)]);
      Active := True;
    end;
  end;
  UpdateCaptionList(-1);
  UpdateControls;
end;

procedure TGradCaptionsEditor.ApplyChanges;
begin
  ReadControls;
  if Assigned(FComponent) then
  begin
    FComponent.Captions := GradientCaption.Captions;
    if Assigned(FDesigner) then FDesigner.Modified;
  end;
end;

procedure TGradCaptionsEditor.AddColorItem(const ColorName: string);
begin
  CaptionInactiveColor.Items.Add(ColorName);
end;

procedure TGradCaptionsEditor.UpdateControls;
begin
  if ActiveCaption = nil then
  begin
    ClearControls;
    EnableControls(False);
  end
  else
    with ActiveCaption do
    begin
      FUpdating := True;
      try
        FontDialog.Font := Font;
        CaptionText.Text := Caption;
        CaptionInactiveColor.ItemIndex := -1;
        CaptionInactiveColor.Text := ColorToString(InactiveColor);
        CaptionFont.Text := Font.Name;
        CaptionParentFont.Checked := ParentFont;
        CaptionGlueNext.Checked := GlueNext;
        CaptionVisible.Checked := Visible;
        EnableControls(True);
      finally
        FUpdating := False;
      end;
    end;
end;

procedure TGradCaptionsEditor.EnableControls(Enable: Boolean);
begin
  CaptionText.Enabled := Enable;
  CaptionInactiveColor.Enabled := Enable;
  CaptionFont.Enabled := Enable;
  CaptionParentFont.Enabled := Enable;
  CaptionGlueNext.Enabled := Enable;
  CaptionVisible.Enabled := Enable;
  DeleteButton.Enabled := Enable;
end;

procedure TGradCaptionsEditor.ClearControls;
begin
  FUpdating := True;
  try
    CaptionText.Text := '';
    CaptionInactiveColor.ItemIndex := -1;
    CaptionInactiveColor.Text := '';
    CaptionFont.Text := '';
    CaptionParentFont.Checked := False;
    CaptionGlueNext.Checked := False;
    CaptionVisible.Checked := False;
  finally
    FUpdating := False;
  end;
end;

procedure TGradCaptionsEditor.ReadControls;
begin
  if not FUpdating and (ActiveCaption <> nil) then
  begin
    GradientCaption.Captions.BeginUpdate;
    FUpdating := True;
    try
      with ActiveCaption do
      begin
        Caption := CaptionText.Text;
        InactiveColor := StringToColor(CaptionInactiveColor.Text);
        ParentFont := CaptionParentFont.Checked;
        GlueNext := CaptionGlueNext.Checked;
        Visible := CaptionVisible.Checked;
      end;
    finally
      GradientCaption.Captions.EndUpdate;
      FUpdating := False;
    end;
  end;
end;

procedure TGradCaptionsEditor.FormCreate(Sender: TObject);
begin
  FormStorage.IniFileName := SDelphiKey;
  CaptionInactiveColor.Items.BeginUpdate;
  try
    GetColorValues(AddColorItem);
  finally
    CaptionInactiveColor.Items.EndUpdate;
  end;
end;

procedure TGradCaptionsEditor.CaptionListClick(Sender: TObject);
begin
  if not FUpdating then UpdateControls;
end;

procedure TGradCaptionsEditor.CaptionListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  I: Integer;
begin
  I := CaptionList.ItemAtPos(Point(X, Y), True);
  if (I >= 0) and (I < CaptionList.Items.Count) and
    (I <> CaptionList.ItemIndex) then
  begin
    GradientCaption.MoveCaption(CaptionList.ItemIndex, I);
    CaptionList.ItemIndex := I;
    if not FUpdating then UpdateControls;
  end;
end;

procedure TGradCaptionsEditor.CaptionListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(CaptionList, Source, X, Y, State, Accept, CaptionList.Sorted);
end;

procedure TGradCaptionsEditor.NewButtonClick(Sender: TObject);
begin
  if GradientCaption.Captions.Add <> nil then
  begin
    UpdateCaptionList(GradientCaption.Captions.Count - 1);
    UpdateControls;
    if CaptionText.CanFocus then ActiveControl := CaptionText;
  end;
end;

procedure TGradCaptionsEditor.DeleteButtonClick(Sender: TObject);
begin
  if ActiveCaption <> nil then
  begin
    ActiveCaption.Free;
    UpdateCaptionList(-1);
    UpdateControls;
  end;
end;

procedure TGradCaptionsEditor.OkButtonClick(Sender: TObject);
begin
  ApplyChanges;
  ModalResult := mrOk;
end;

procedure TGradCaptionsEditor.ApplyButtonClick(Sender: TObject);
begin
  ApplyChanges;
end;

procedure TGradCaptionsEditor.CaptionInactiveColorDblClick(
  Sender: TObject);
begin
  with ColorDialog do
  begin
    Color := StringToColor(CaptionInactiveColor.Text);
    if Execute then
    begin
      CaptionInactiveColor.Text := ColorToString(Color);
      if not FUpdating and (ActiveCaption <> nil) then
        ActiveCaption.InactiveColor := Color;
    end;
  end;
end;

procedure TGradCaptionsEditor.ControlExit(Sender: TObject);
begin
  if not FUpdating then ReadControls;
end;

procedure TGradCaptionsEditor.CaptionTextChange(Sender: TObject);
begin
  if not FUpdating and (ActiveCaption <> nil) then
    ActiveCaption.Caption := CaptionText.Text;
end;

procedure TGradCaptionsEditor.CaptionFontButtonClick(Sender: TObject);
begin
  if ActiveCaption <> nil then
  begin
    with FontDialog do
    begin
      Font := ActiveCaption.Font;
      Font.Color := ColorToRGB(ActiveCaption.Font.Color);
      if Execute then
      begin
        FUpdating := True;
        try
          CaptionFont.Text := Font.Name;
          ActiveCaption.Font := Font;
          CaptionParentFont.Checked := ActiveCaption.ParentFont;
        finally
          FUpdating := False;
        end;
      end;
    end;
  end
  else
    Beep;
end;

procedure TGradCaptionsEditor.CheckBoxClick(Sender: TObject);
begin
  if not FUpdating then ReadControls;
end;

end.