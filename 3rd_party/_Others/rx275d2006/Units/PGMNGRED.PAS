{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1996 AO ROSNO                   }
{                                                       }
{*******************************************************}

unit PgMngrEd;

{$I RX.INC}

interface

uses
{$IFDEF WIN32}
  Windows,
{$ELSE}
  WinTypes, WinProcs,
{$ENDIF}
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, Grids,
  RTLConsts, DesignIntf, DesignEditors, VCLEditors, PageMngr, StdCtrls, Placemnt, ExtCtrls,
  VCLUtils, DesignWindows;

type
  TProxyEditor = class(TDesignWindow)
    FormStorage: TFormStorage;
    BtnPanel: TPanel;
    CloseBtn: TButton;
    DeleteBtn: TButton;
    ProxyGrid: TDrawGrid;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ProxyGridDrawCell(Sender: TObject; Col, Row: Longint; Rect: TRect; State: TGridDrawState);
    procedure ProxyGridSelectCell(Sender: TObject; Col, Row: Longint; var CanSelect: Boolean);
    procedure CloseBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure ProxyGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPageManager: TPageManager;
    FDeleting: Boolean;
    procedure SetPageManager(Value: TPageManager);
    function GetForm: TCustomForm;
    procedure UpdateData;
    function CheckPageManager: Boolean;
    procedure SelectProxy(Proxy: TPageProxy);
    function ProxyByRow(Row: Integer): TPageProxy;
  protected
    function UniqueName(Component: TComponent): string; override;
    procedure Activated; override;
  public
    procedure NameProxy(Sender: TObject);
    procedure ItemsModified(const Designer: IDesigner); override;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    function GetEditState: TEditState; override;
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    property PageManager: TPageManager read FPageManager write SetPageManager;
    property OwnerForm: TCustomForm read GetForm;
  end;

{ TProxyListProperty }

  TProxyListProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

{ TPageManagerEditor }

  TPageManagerEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TPageNameProperty }

  TPageNameProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TPageBtnProperty }

  TPageBtnProperty = class(TComponentProperty)
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses Consts, Buttons, RxCtrls, RXConst, RXLConst, RxDsgn;

{$R *.DFM}

{$IFDEF WIN32}
 {$D-}
{$ENDIF}

{$IFDEF RX_D4}
type
  TDesigner = IDesigner;
  TFormDesigner = IDesigner;
{$ENDIF}

function FindEditor(Manager: TPageManager): TProxyEditor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is TProxyEditor then begin
      if TProxyEditor(Screen.Forms[I]).PageManager = Manager then
      begin
        Result := TProxyEditor(Screen.Forms[I]);
        Break;
      end;
    end;
  end;
end;

procedure ShowProxyEditor(Designer: TDesigner; Manager: TPageManager);
var
  Editor: TProxyEditor;
begin
  if Manager = nil then Exit;
  Editor := FindEditor(Manager);
  if Editor <> nil then begin
    Editor.Show;
    if Editor.WindowState = wsMinimized then Editor.WindowState := wsNormal;
  end
  else begin
    Editor := TProxyEditor.Create(Application);
    try
      Editor.Designer := TFormDesigner(Designer);
      Editor.PageManager := Manager;
      Editor.Show;
    except
      Editor.Free;
      raise;
    end;
  end;
end;

{ TProxyListProperty }

function TProxyListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TProxyListProperty.GetValue: string;
var
  List: TList;
begin
  List := TList(Pointer(GetOrdValue));
  if (List = nil) or (List.Count = 0) then
    Result := ResStr(srNone)
  else FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

procedure TProxyListProperty.Edit;
begin
  ShowProxyEditor(Designer, TPageManager(GetComponent(0)));
end;

{ TPageBtnProperty }

procedure TPageBtnProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Component: TComponent;
begin
  for I := 0 to Designer.Root.ComponentCount - 1 do begin
    Component := Designer.Root.Components[I];
    if (Component.InheritsFrom(TButtonControl) or 
      Component.InheritsFrom(TSpeedButton) or 
      Component.InheritsFrom(TRxSpeedButton)) and 
      (Component.Name <> '') then Proc(Component.Name);
  end;
end;

{ TPageNameProperty }

function TPageNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TPageNameProperty.GetValues(Proc: TGetStrProc);
var
  PageProxy: TPageProxy;
  I: Integer;
begin
  PageProxy := GetComponent(0) as TPageProxy;
  if (PageProxy <> nil) and (PageProxy.PageManager <> nil) and
    (PageProxy.PageManager.PageOwner <> nil) then
  begin
    for I := 0 to PageProxy.PageManager.PageCount - 1 do begin
      Proc(PageProxy.PageManager.PageNames[I]);
    end;
  end;
end;

{ TPageManagerEditor }

procedure TPageManagerEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowProxyEditor(Designer, TPageManager(Component));
  end;
end;

function TPageManagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := LoadStr(srProxyEditor);
  end;
end;

function TPageManagerEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TProxyEditor }

procedure TProxyEditor.SetPageManager(Value: TPageManager);
begin
  if FPageManager <> Value then begin
    if FPageManager <> nil then FPageManager.OnCheckProxy := nil;
    FPageManager := Value;
    if FPageManager <> nil then FPageManager.OnCheckProxy := NameProxy;
    UpdateData;
  end;
end;

function TProxyEditor.UniqueName(Component: TComponent): string;
var
  Temp: string;
{$IFNDEF WIN32}
  I: Integer;
  Comp: TComponent;
{$ENDIF}
begin
  Result := '';
  if (Component <> nil) then Temp := Component.ClassName
  else Temp := TPageProxy.ClassName;
  if (UpCase(Temp[1]) = 'T') and (Length(Temp) > 1) then
    System.Delete(Temp, 1, 1);
{$IFDEF WIN32}
  Result := Designer.UniqueName(Temp);
{$ELSE}
  I := 1;
  repeat
    Result := Temp + IntToStr(I);
    Comp := OwnerForm.FindComponent(Result);
    Inc(I);
  until (Comp = nil) or (Comp = Component);
{$ENDIF}
end;

function TProxyEditor.GetEditState: TEditState;
begin
  Result := [];
end;

procedure TProxyEditor.NameProxy(Sender: TObject);
begin
  if (Sender is TPageProxy) and (TPageProxy(Sender).Name = '') then
    TPageProxy(Sender).Name := UniqueName(TPageProxy(Sender));
end;

procedure TProxyEditor.DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean);
begin
  if Designer.Root = OwnerForm then Free;
end;

procedure TProxyEditor.ItemsModified(const Designer: IDesigner);
begin
  if not (csDestroying in ComponentState) then UpdateData;
end;

procedure TProxyEditor.Activated;
begin
  SelectProxy(ProxyByRow(ProxyGrid.Row - 1));
end;

procedure TProxyEditor.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
begin
  if Item = FPageManager then begin
    FPageManager := nil;
    Close;
  end;
end;

procedure TProxyEditor.UpdateData;
var
  ProxyCount: Integer;
begin
  if CheckPageManager then begin
    if not FDeleting then FPageManager.Resync;
    ProxyCount := FPageManager.PageProxies.Count;
    if ProxyCount = 0 then begin
      ProxyGrid.RowCount := 2;
      SelectProxy(nil);
    end
    else begin
      ProxyGrid.RowCount := 1 + ProxyCount;
    end;
    DeleteBtn.Enabled := ProxyCount > 0;
    ProxyGrid.Invalidate;
  end;
end;

function TProxyEditor.GetForm: TCustomForm;
begin
  Result := GetParentForm(BtnPanel); //Designer.Form;
end;

procedure TProxyEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  if FPageManager <> nil then FPageManager.OnCheckProxy := nil;
end;

procedure TProxyEditor.FormShow(Sender: TObject);
begin
  if FPageManager.PageOwner <> nil then begin
    Caption := Format(LoadStr(srPageProxies), [FPageManager.PageOwner.Name]);
  end;
end;

function TProxyEditor.CheckPageManager: Boolean;
begin
  Result := (FPageManager <> nil) and (FPageManager.Owner <> nil) and
    (Designer.Root <> nil);
end;

procedure TProxyEditor.SelectProxy(Proxy: TPageProxy);
var
  FComponents: IDesignerSelections;
begin
  if CheckPageManager and Active then begin
    FComponents := CreateSelectionList;
    if Proxy <> nil then
      FComponents.Add(Proxy)
    else
      FComponents.Add(FPageManager);
    SetSelection(FComponents);
  end;
end;

function TProxyEditor.ProxyByRow(Row: Integer): TPageProxy;
begin
  Result := nil;
  if CheckPageManager and (Row >= 0) and
    (Row < FPageManager.PageProxies.Count) then
  begin
    Result := FPageManager.PageProxies.Items[Row];
  end;
end;

procedure TProxyEditor.ProxyGridDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
var
  CellText: string;
  Proxy: TPageProxy;
begin
  CellText := '';
  if gdFixed in State then begin
    case Col of
      0: CellText := LoadStr(srProxyName);
      1: CellText := LoadStr(srPageName);
    end;
  end
  else begin
    Proxy := ProxyByRow(Row - 1);
    if Proxy <> nil then begin
      case Col of
        0: CellText := Proxy.Name;
        1: CellText := Proxy.PageName;
      end;
    end;
  end;
  DrawCellText(ProxyGrid, Col, Row, CellText, Rect, taLeftJustify, vaCenter);
end;

procedure TProxyEditor.ProxyGridSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  SelectProxy(ProxyByRow(Row - 1));
end;

procedure TProxyEditor.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TProxyEditor.DeleteBtnClick(Sender: TObject);
var
  Proxy: TPageProxy;
begin
  Proxy := ProxyByRow(ProxyGrid.Row - 1);
  if Proxy <> nil then begin
    Self.ValidateRename(Proxy, Proxy.Name, '');
    FDeleting := True;
    try
      Proxy.Free;
      Designer.Modified;
    finally
      FDeleting := False;
    end;
  end;
end;

procedure TProxyEditor.ProxyGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [] then begin
    case Key of
      VK_RETURN:
        if ProxyByRow(ProxyGrid.Row - 1) <> nil then begin
          ActivateInspector(#0);
        end;
      VK_DELETE:
        DeleteBtnClick(nil);
    end;
  end;
end;

procedure TProxyEditor.FormResize(Sender: TObject);
begin
  with ProxyGrid do begin
    DefaultColWidth := (ClientWidth - 1) div 2;
    ColWidths[1] := ClientWidth - ColWidths[0] - 1;
  end;
end;

procedure TProxyEditor.FormCreate(Sender: TObject);
begin
  if NewStyleControls then Font.Style := [];
{$IFDEF WIN32}
  with FormStorage do begin
    UseRegistry := True;
    IniFileName := SDelphiKey;
  end;
{$ENDIF}
end;

end.
