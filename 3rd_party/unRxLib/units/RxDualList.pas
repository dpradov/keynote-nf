{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxDualList;

interface

{$I RX.INC}

uses
  Classes, Controls;

type

{ TDualListDialog }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDualListDialog = class(TComponent)
  private
    FCtl3D: Boolean;
    FSorted: Boolean;
    {$IFDEF RX_D4} // Polaris
    FTitle: string;
    {$ELSE}
    FTitle: PString;
    {$ENDIF}
    FLabel1Caption: TCaption;
    FLabel2Caption: TCaption;
    FOkBtnCaption: TCaption;
    FCancelBtnCaption: TCaption;
    FHelpBtnCaption: TCaption;
    FHelpContext: THelpContext;
    FList1: TStrings;
    FList2: TStrings;
    FShowHelp: Boolean;
    function GetTitle: string;
    procedure SetTitle(const ATitle: string);
    procedure SetList1(Value: TStrings);
    procedure SetList2(Value: TStrings);
    function IsLabel1Custom: Boolean;
    function IsLabel2Custom: Boolean;
    function IsOkBtnCustom: Boolean;
    function IsCancelBtnCustom: Boolean;
    function IsHelpBtnCustom: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property Ctl3D: Boolean read FCtl3D write FCtl3D default True;
    property Sorted: Boolean read FSorted write FSorted;
    property Title: string read GetTitle write SetTitle;
    property Label1Caption: TCaption read FLabel1Caption write FLabel1Caption
      stored IsLabel1Custom;
    property Label2Caption: TCaption read FLabel2Caption write FLabel2Caption
      stored IsLabel2Custom;
    property OkBtnCaption: TCaption read FOkBtnCaption write FOkBtnCaption
      stored IsOkBtnCustom;
    property CancelBtnCaption: TCaption read FCancelBtnCaption write FCancelBtnCaption
      stored IsCancelBtnCustom;
    property HelpBtnCaption: TCaption read FHelpBtnCaption write FHelpBtnCaption
      stored IsHelpBtnCustom;
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
    property List1: TStrings read FList1 write SetList1;
    property List2: TStrings read FList2 write SetList2;
    property ShowHelp: Boolean read FShowHelp write FShowHelp default True;
  end;

implementation

uses SysUtils, Forms, RxFDualLst, Consts, RxResConst, RxVCLUtils;

{ TDualListDialog }

constructor TDualListDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCtl3D := True;
  FShowHelp := True;
  {$IFDEF RX_D4} // Polaris
  FTitle := EmptyStr;
  {$ELSE}
  FTitle := NullStr;
  {$ENDIF}
  FList1 := TStringList.Create;
  FList2 := TStringList.Create;
  FLabel1Caption := RxLoadStr(SDualListSrcCaption);
  FLabel2Caption := RxLoadStr(SDualListDestCaption);
  OkBtnCaption := ResStr(SOKButton);
  CancelBtnCaption := ResStr(SCancelButton);
  HelpBtnCaption := ResStr(SHelpButton);
end;

destructor TDualListDialog.Destroy;
begin
  List1.Free;
  List2.Free;
  {$IFNDEF RX_D4} // Polaris
  DisposeStr(FTitle);
  {$ENDIF}
  inherited Destroy;
end;

function TDualListDialog.GetTitle: string;
begin
  {$IFDEF RX_D4} // Polaris
  Result := FTitle;
  {$ELSE}
  Result := FTitle^;
  {$ENDIF}
end;

procedure TDualListDialog.SetTitle(const ATitle: string);
begin
  {$IFDEF RX_D4} // Polaris
  FTitle := ATitle;
  {$ELSE}
  AssignStr(FTitle, ATitle);
  {$ENDIF}
end;

procedure TDualListDialog.SetList1(Value: TStrings);
begin
  FList1.Assign(Value);
end;

procedure TDualListDialog.SetList2(Value: TStrings);
begin
  FList2.Assign(Value);
end;

function TDualListDialog.IsLabel1Custom: Boolean;
begin
  Result := CompareStr(Label1Caption, RxLoadStr(SDualListSrcCaption)) <> 0;
end;

function TDualListDialog.IsLabel2Custom: Boolean;
begin
  Result := CompareStr(Label2Caption, RxLoadStr(SDualListDestCaption)) <> 0;
end;

function TDualListDialog.IsOkBtnCustom: Boolean;
begin
  Result := CompareStr(OkBtnCaption, ResStr(SOKButton)) <> 0;
end;

function TDualListDialog.IsCancelBtnCustom: Boolean;
begin
  Result := CompareStr(CancelBtnCaption, ResStr(SCancelButton)) <> 0;
end;

function TDualListDialog.IsHelpBtnCustom: Boolean;
begin
  Result := CompareStr(HelpBtnCaption, ResStr(SHelpButton)) <> 0;
end;

function TDualListDialog.Execute: Boolean;
var
  Form: TDualListForm;
begin
  Form := TDualListForm.Create(Application);
  try
    with Form do
    begin
      Ctl3D := Self.Ctl3D;
      {$IFNDEF VER80}
      if NewStyleControls then Font.Style := [];
      {$ENDIF}
      ShowHelp := Self.ShowHelp;
      SrcList.Sorted := Sorted;
      DstList.Sorted := Sorted;
      SrcList.Items := List1;
      DstList.Items := List2;
      if Self.Title <> '' then Form.Caption := Self.Title;
      if Label1Caption <> '' then SrcLabel.Caption := Label1Caption;
      if Label2Caption <> '' then DstLabel.Caption := Label2Caption;
      OkBtn.Caption := OkBtnCaption;
      CancelBtn.Caption := CancelBtnCaption;
      HelpBtn.Caption := HelpBtnCaption;
      HelpContext := Self.HelpContext;
      HelpBtn.HelpContext := HelpContext;
    end;
    Result := (Form.ShowModal = mrOk);
    if Result then
    begin
      List1 := Form.SrcList.Items;
      List2 := Form.DstList.Items;
    end;
  finally
    Form.Free;
  end;
end;

end.

