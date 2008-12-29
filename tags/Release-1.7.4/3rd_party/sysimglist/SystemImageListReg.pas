{$I DFS.INC}

unit SystemImageListReg;

interface

uses
  {$IFDEF DFS_NO_DSGNINTF}
  DesignIntf,
  DesignEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF}

type
  TdfsSystemImageListEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer): string; override;
    function GetVerbCount : Integer; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

uses
  SystemImageList, DFSAbout, Classes, Forms, Graphics, Dialogs;


procedure Register;
begin
  RegisterComponents('DFS', [TdfsSystemImageList]);
  RegisterPropertyEditor(TypeInfo(string), TdfsSystemImageList, 'Version',
     TDFSVersionProperty);
  RegisterComponentEditor(TdfsSystemImageList, TdfsSystemImageListEditor);
end;

{ TDFSStatusBarEditor }

procedure TdfsSystemImageListEditor.Edit;
var
  SaveDlg: TSaveDialog;
  Stream: TStream;
begin
  SaveDlg := TSaveDialog.Create(Application);
  try
    SaveDlg.DefaultExt := 'bmp';
    SaveDlg.Filter := 'Bitmaps (*.bmp)|*.bmp|All Files (*.*)|*.*';
    SaveDlg.Options := [ofOverwritePrompt, ofHideReadOnly
       {$IFDEF DFS_COMPILER_4_UP} , ofEnableSizing {$ENDIF} , ofPathMustExist];
    SaveDlg.Title := 'Save ' + Component.Name + ' as...';

    if SaveDlg.Execute then
    begin
      Stream := TFileStream.Create(SaveDlg.Filename, fmCreate);
      try
        TdfsSystemImageList(Component).SaveToStream(Stream);
      finally
        Stream.Free;
      end;
    end;
  finally
    SaveDlg.Free;
  end;
end;

procedure TdfsSystemImageListEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then  // We only have one verb...
    Edit;
end;

function TdfsSystemImageListEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Save &to bitmap...';
end;

function TdfsSystemImageListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
