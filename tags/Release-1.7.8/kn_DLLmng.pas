unit kn_DLLmng;

interface
uses
  kn_Const;

{ KNTUTIL.DLL access routines }
function ObtainDLLHandle : THandle;
procedure DllProcNotFoundMsg( const ProcName : string );
function DllConvertHTMLToRTF(
    const aFilename : string;
    var ConvertCode : integer;
    const aHTMLImportMethod : THTMLImportMethod;
    const HTML32CNVLocation : string
  ) : string; // returns name of converted file
function DllConvertRTFToHTML(
    const aFilename : string;
    const RTFText : string;
    const HTML32CNVLocation : string
  ) : boolean;

implementation
uses
  Windows, Forms, SysUtils, Dialogs,
  kn_DLLInterface, kn_Info ;

resourcestring
  STR_01 = 'Error while attempting to load runtime library "%s". Please reinstall KeyNote.';
  STR_02 = 'Procedure "%s" not found in runtime library "%s". Please reinstall KeyNote.';


function DllConvertRTFToHTML(
    const aFilename : string;
    const RTFText : string;
    const HTML32CNVLocation : string
  ) : boolean;
var
  DllHandle : THandle;
  ConvertRTFToHTML : ConvertRTFToHTMLProc;
begin
  result := false;
  DLLHandle := ObtainDLLHandle;
  if ( DllHandle <= 0 ) then exit;

  try

    try
      @ConvertRTFToHTML := GetProcAddress( DllHandle, 'ConvertRTFToHTML' );
      if ( not assigned( ConvertRTFToHTML )) then
      begin
        DllProcNotFoundMsg( 'ConvertRTFToHTML' );
        exit;
      end;

      result := ConvertRTFToHTML(
        Application.Handle,
        PChar( RTFText ),
        PChar( aFilename ),
        PChar( HTML32CNVLocation )
      );

    except
      result := false;
      raise;
    end;

  finally
    FreeLibrary( DllHandle );
  end;

end; // DllConvertRTFToHTML

function DllConvertHTMLToRTF(
      const aFilename : string;
      var ConvertCode : integer;
      const aHTMLImportMethod : THTMLImportMethod;
      const HTML32CNVLocation : string
    ) : string; // returns name of converted file
var
  DllHandle : THandle;
  ConvertHTMLToRTF : ConvertHTMLToRTFProc;
  outFN : TFilenameBuffer;
begin

  result := '';
  DLLHandle := ObtainDLLHandle;
  if ( DllHandle <= 0 ) then exit;

  try
    try

      @ConvertHTMLToRTF := GetProcAddress( DllHandle, 'ConvertHTMLToRTF' );
      if ( not assigned( ConvertHTMLToRTF )) then
      begin
        DllProcNotFoundMsg( 'ConvertHTMLToRTF' );
        exit;
      end;

      ConvertCode := ConvertHTMLToRTF(
        Application.Handle,
        aHTMLImportMethod,
        PChar( aFilename ),
        outFN,
        PChar( HTML32CNVLocation )
        );
      if ( ConvertCode = 0 ) then
      begin
        result := outFN;
      end
      else
      begin
        result := '';
        deletefile( outfn );
      end;


    except
      on E : Exception do
      begin
        result := '';
        messagedlg( E.Message, mtError, [mbOK], 0 );
      end;
    end;
  finally
    FreeLibrary( DllHandle );
  end;

end; // DllConvertHTMLToRTF

function ObtainDLLHandle : THandle;
begin
  result := LoadLibrary( PChar( _KNTUtilsDLL_FN ));
  if ( result <= 0 ) then
  begin
    Application.MessageBox(
      PChar( Format(
        STR_01, [extractfilename( _KNTUtilsDLL_FN )] )),
        'Failed to load library', MB_OK+MB_ICONHAND+MB_DEFBUTTON1+MB_APPLMODAL);
    exit;
  end;
end; // ObtainDLLHandle

procedure DllProcNotFoundMsg( const ProcName : string );
begin
  Application.MessageBox(
    PChar( Format(
      STR_02, [ProcName, extractfilename( _KNTUtilsDLL_FN )] )),
      'Procedure not in library', MB_OK+MB_ICONHAND+MB_DEFBUTTON1+MB_APPLMODAL);
end; // DllProcNotFoundMsg


end.
