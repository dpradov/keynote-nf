{$I DFS.INC}

unit MRUFLReg;

{------------------------------------------------------------------------------}
{ TdfsMRUFileList v2.65                                                        }
{------------------------------------------------------------------------------}
{ Copyright 1999, Brad Stowers.  All Rights Reserved.                          }
{-----------------------------------                                           }
(*
  + Changes by Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [dpv]
  
  > Changes to original source code available in KeyNote NF project.
  > Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
    in https://github.com/dpradov/keynote-nf   
	
 *******************************************************************************)


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
