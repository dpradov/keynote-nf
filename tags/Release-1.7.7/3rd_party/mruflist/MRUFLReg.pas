{$I DFS.INC}

unit MRUFLReg;

interface

{$IFDEF DFS_WIN32}
  {$R MRUFList.res}
{$ELSE}
  {$R MRUFList.r16}
{$ENDIF}

procedure Register;

implementation

uses
  MRUFList, DFSAbout, Classes,
{$IFDEF DFS_NO_DSGNINTF}     // [dpv]
    DesignIntf,
    DesignEditors;
{$ELSE}
    DsgnIntf;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('DFS', [TdfsMRUFileList]);
  RegisterPropertyEditor(TypeInfo(string), TdfsMRUFileList, 'Version',
     TDFSVersionProperty);
end;

end.
