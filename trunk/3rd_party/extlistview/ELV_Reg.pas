{$I DFS.INC}

unit ELV_Reg;

interface

procedure Register;

implementation

uses
  Classes, DFSAbout, EnhListView,
{$IFDEF DFS_COMPILER_2}
  ExtColEd,
{$ENDIF}
  ExtListView,
{$IFDEF DFS_NO_DSGNINTF}     // [dpv]
    DesignIntf,
    DesignEditors;
{$ELSE}
    DsgnIntf;
{$ENDIF}


procedure Register;
begin
  RegisterComponents('DFS', [TdfsEnhListView]);
  RegisterPropertyEditor(TypeInfo(TdfsEnhLVSaveSettings), NIL, '', TClassProperty);
  RegisterComponents('DFS', [TdfsExtListView]);
  RegisterPropertyEditor(TypeInfo(TdfsExtLVSaveSettings), NIL, '', TClassProperty);
{$IFDEF DFS_COMPILER_2}
  RegisterPropertyEditor(TypeInfo(TdfsExtListColumns), NIL, '',
     TdfsExtListColumnsProperty);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(string), TCustomEnhListView, 'Version',
     TDFSVersionProperty);
end;

end.
