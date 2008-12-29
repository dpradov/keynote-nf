{$I DFS.INC}

unit BrowseDrReg;

interface

uses
  BrowseDr,
{$IFDEF DFS_NO_DSGNINTF}     // [dpv]
    DesignIntf,
    DesignEditors;
{$ELSE}
    DsgnIntf;
{$ENDIF}


type
  { A component editor (not really) to allow on-the-fly testing of the      }
  { dialog.  Right click the component and select 'Test Dialog', or simply  }
  { double click the component, and the browse dialog will be displayed     }
  { with the current settings.                                              }
  TBrowseDialogEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer): string; override;
    function GetVerbCount : Integer; override;
    procedure Edit; override;
  end;

procedure Register;


implementation

uses
  SysUtils, Dialogs, Classes, DFSAbout;

// Component Editor (not really) to allow on the fly testing of the dialog
procedure TBrowseDialogEditor.ExecuteVerb(Index: Integer);
begin
  {we only have one verb, so exit if this ain't it}
  if Index <> 0 then Exit;
  Edit;
end;

function TBrowseDialogEditor.GetVerb(Index: Integer): AnsiString;
begin
  Result := 'Test Dialog';
end;

function TBrowseDialogEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TBrowseDialogEditor.Edit;
begin
  with TdfsBrowseDirectoryDlg(Component) do
    if Execute then
      MessageDlg(Format('Item selected:'#13#13'%s', [Selection]),
                 mtInformation, [mbOk], 0);
end;



procedure Register;
begin
  RegisterComponents('DFS', [TdfsBrowseDirectoryDlg]);
  RegisterComponentEditor(TdfsBrowseDirectoryDlg, TBrowseDialogEditor);
  RegisterPropertyEditor(TypeInfo(string), TdfsBrowseDirectoryDlg, 'Version',
     TDFSVersionProperty);
end;


end.
