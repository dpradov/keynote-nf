{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxMinMaxEd;

interface

{$I RX.INC}

uses
  SysUtils, {$IFNDEF VER80}Windows, {$ELSE}WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Mask, RxCurrEdit, RxVCLUtils, RxPlacemnt, Consts,
  {$IFDEF RX_D6}DesignIntf, DesignEditors, {$ELSE}DsgnIntf, {$ENDIF}RxToolEdit; // Polaris

type
  TMinMaxInfoEditDialog = class(TForm)
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    MaxPosBtn: TSpeedButton;
    MaxSizeBtn: TSpeedButton;
    MaxTrackBtn: TSpeedButton;
    MinTrackBtn: TSpeedButton;
    MaxPosLeftEdit: TCurrencyEdit;
    MaxPosTopEdit: TCurrencyEdit;
    MaxSizeWidthEdit: TCurrencyEdit;
    MaxSizeHeightEdit: TCurrencyEdit;
    MaxTrackWidthEdit: TCurrencyEdit;
    MaxTrackHeightEdit: TCurrencyEdit;
    MinTrackWidthEdit: TCurrencyEdit;
    MinTrackHeightEdit: TCurrencyEdit;
    ClearBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SetCurrentBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
  private
    { Private declarations }
    FWinMinMaxInfo: TWinMinMaxInfo;
    FForm: TCustomForm;
    procedure SetWinMinMaxInfo(Value: TWinMinMaxInfo);
    procedure UpdateMinMaxInfo;
  public
    { Public declarations }
    property WinMinMaxInfo: TWinMinMaxInfo read FWinMinMaxInfo write SetWinMinMaxInfo;
  end;

{ TMinMaxProperty }

  TMinMaxProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

function EditMinMaxInfo(AComponent: TFormPlacement): Boolean;

implementation

{$R *.DFM}

{$IFNDEF VER80}
{$D-}
{$ENDIF}

function EditMinMaxInfo(AComponent: TFormPlacement): Boolean;
begin
  Result := False;
  if AComponent = nil then Exit;
  with TMinMaxInfoEditDialog.Create(Application) do
  try
    WinMinMaxInfo := AComponent.MinMaxInfo;
    if AComponent.Owner is TCustomForm then
      FForm := TCustomForm(AComponent.Owner);
    if AComponent.Name <> '' then
      Caption := Format('%s.MinMaxInfo', [AComponent.Name]);
    Result := ShowModal = mrOk;
    if Result then AComponent.MinMaxInfo := WinMinMaxInfo;
  finally
    Free;
  end;
end;

{ TMinMaxProperty }

function TMinMaxProperty.GetValue: string;
var
  WinMinMaxInfo: TWinMinMaxInfo;
begin
  WinMinMaxInfo := TWinMinMaxInfo(GetOrdValue);
  with WinMinMaxInfo do
  begin
    if DefaultMinMaxInfo then
      Result := ResStr(srNone)
    else
      Result := Format('(%d,%d),(%d,%d),(%d,%d),(%d,%d)',
        [MaxPosLeft, MaxPosTop, MaxSizeWidth, MaxSizeHeight,
        MaxTrackWidth, MaxTrackHeight, MinTrackWidth, MinTrackHeight]);
  end;
end;

function TMinMaxProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paReadOnly];
end;

procedure TMinMaxProperty.Edit;
begin
  if EditMinMaxInfo(GetComponent(0) as TFormPlacement) then Modified;
end;

{ TMinMaxInfoEditDialog }

procedure TMinMaxInfoEditDialog.SetWinMinMaxInfo(Value: TWinMinMaxInfo);
begin
  FWinMinMaxInfo.Assign(Value);
  with FWinMinMaxInfo do
  begin
    MaxPosLeftEdit.AsInteger := MaxPosLeft;
    MaxPosTopEdit.AsInteger := MaxPosTop;
    MaxSizeWidthEdit.AsInteger := MaxSizeWidth;
    MaxSizeHeightEdit.AsInteger := MaxSizeHeight;
    MaxTrackWidthEdit.AsInteger := MaxTrackWidth;
    MaxTrackHeightEdit.AsInteger := MaxTrackHeight;
    MinTrackWidthEdit.AsInteger := MinTrackWidth;
    MinTrackHeightEdit.AsInteger := MinTrackHeight;
  end;
end;

procedure TMinMaxInfoEditDialog.UpdateMinMaxInfo;
begin
  with FWinMinMaxInfo do
  begin
    MaxPosLeft := MaxPosLeftEdit.AsInteger;
    MaxPosTop := MaxPosTopEdit.AsInteger;
    MaxSizeWidth := MaxSizeWidthEdit.AsInteger;
    MaxSizeHeight := MaxSizeHeightEdit.AsInteger;
    MaxTrackWidth := MaxTrackWidthEdit.AsInteger;
    MaxTrackHeight := MaxTrackHeightEdit.AsInteger;
    MinTrackWidth := MinTrackWidthEdit.AsInteger;
    MinTrackHeight := MinTrackHeightEdit.AsInteger;
  end;
end;

procedure TMinMaxInfoEditDialog.FormCreate(Sender: TObject);
begin
  FWinMinMaxInfo := TWinMinMaxInfo.Create;
end;

procedure TMinMaxInfoEditDialog.FormDestroy(Sender: TObject);
begin
  FWinMinMaxInfo.Free;
end;

procedure TMinMaxInfoEditDialog.SetCurrentBtnClick(Sender: TObject);
begin
  if FForm <> nil then
    case TComponent(Sender).Tag of
      1:
        begin
          MaxPosLeftEdit.AsInteger := TForm(FForm).Left;
          MaxPosTopEdit.AsInteger := TForm(FForm).Top;
        end;
      2:
        begin
          MaxSizeWidthEdit.AsInteger := TForm(FForm).Width;
          MaxSizeHeightEdit.AsInteger := TForm(FForm).Height;
        end;
      3:
        begin
          MaxTrackWidthEdit.AsInteger := TForm(FForm).Width;
          MaxTrackHeightEdit.AsInteger := TForm(FForm).Height;
        end;
      4:
        begin
          MinTrackWidthEdit.AsInteger := TForm(FForm).Width;
          MinTrackHeightEdit.AsInteger := TForm(FForm).Height;
        end;
    else
      Exit;
    end;
end;

procedure TMinMaxInfoEditDialog.OkBtnClick(Sender: TObject);
begin
  UpdateMinMaxInfo;
end;

procedure TMinMaxInfoEditDialog.ClearBtnClick(Sender: TObject);
begin
  MaxPosLeftEdit.AsInteger := 0;
  MaxPosTopEdit.AsInteger := 0;
  MaxSizeWidthEdit.AsInteger := 0;
  MaxSizeHeightEdit.AsInteger := 0;
  MaxTrackWidthEdit.AsInteger := 0;
  MaxTrackHeightEdit.AsInteger := 0;
  MinTrackWidthEdit.AsInteger := 0;
  MinTrackHeightEdit.AsInteger := 0;
end;

end.
