{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1998 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxDsgn;

{$I RX.INC}

interface

uses
  {$IFNDEF VER80}Windows, {$ELSE}WinTypes, {$ENDIF}Classes, SysUtils,
  Controls, Graphics, ExtCtrls, Menus, Forms,
  {$IFDEF RX_D6}DesignIntf, DesignEditors{$ELSE}DsgnIntf{$ENDIF}; // Polaris

type
  {$IFNDEF RX_D4}
  IDesigner = TDesigner;
  IFormDesigner = TFormDesigner;
  {$ENDIF}

  {$IFNDEF RX_D5}
  TDesignerSelectionList = TComponentList;
  {$ENDIF}

{ TFilenameProperty }

  TFilenameProperty = class(TStringProperty)
  protected
    function GetFilter: string; virtual;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TDirnameProperty }

  TDirnameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TProgressControlProperty }

  TProgressControlProperty = class(TComponentProperty)
  private
    FProc: TGetStrProc;
    procedure CheckComponent(const AName: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TRxDBStringProperty }

  TRxDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses Consts, Dialogs, RxResConst, RxFileUtil, RxVCLUtils, RxPrgrss;

{ TFilenameProperty }

function TFilenameProperty.GetFilter: string;
begin
  Result := RxLoadStr(SDefaultFilter);
end;

procedure TFilenameProperty.Edit;
var
  FileOpen: TOpenDialog;
begin
  FileOpen := TOpenDialog.Create(Application);
  try
    FileOpen.Filename := GetValue;
    FileOpen.InitialDir := ExtractFilePath(FileOpen.Filename);
    if (ExtractFileName(FileOpen.Filename) = '') or not
      ValidFileName(ExtractFileName(FileOpen.Filename)) then
      FileOpen.Filename := '';
    FileOpen.Filter := GetFilter;
    FileOpen.Options := FileOpen.Options + [ofHideReadOnly];
    if FileOpen.Execute then SetValue(FileOpen.Filename);
  finally
    FileOpen.Free;
  end;
end;

function TFilenameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog{$IFNDEF VER80}, paRevertable{$ENDIF}];
end;

{ TDirnameProperty }

procedure TDirnameProperty.Edit;
var
  FolderName: string;
begin
  FolderName := GetValue;
  if BrowseDirectory(FolderName, ResStr(SSelectDirCap), 0) then
    SetValue(FolderName);
end;

function TDirnameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog{$IFNDEF VER80}, paRevertable{$ENDIF}];
end;

{ TProgressControlProperty }

procedure TProgressControlProperty.CheckComponent(const AName: string);
var
  Component: TComponent;
begin
  {$IFNDEF VER80}
  Component := Designer.GetComponent(AName);
  {$ELSE}
  Component := Designer.Form.FindComponent(AName);
  {$ENDIF}
  if (Component <> nil) and (Component is TControl) and
    SupportsProgressControl(TControl(Component)) and Assigned(FProc) then
    FProc(AName);
end;

procedure TProgressControlProperty.GetValues(Proc: TGetStrProc);
begin
  FProc := Proc;
  try
    inherited GetValues(CheckComponent);
  finally
    FProc := nil;
  end;
end;

{ TRxDBStringProperty }

function TRxDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TRxDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do
      Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure TRxDBStringProperty.GetValueList(List: TStrings);
begin
end;

end.
