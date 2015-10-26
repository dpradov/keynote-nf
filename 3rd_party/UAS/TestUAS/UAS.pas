unit UAS;

// UAS, UltimaShell Autocompletion Server, (flashpeak@yifan.net)
(* ----------------------------------- 
  + Changes by Marek Jedlinski <marek@tranglos.com> (Poland) [mj]

   >> Changes to original source code available in KeyNote NF project.
   >> Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
      in https://github.com/dpradov/keynote-nf 
  
 ****************************************************************)


interface
uses Forms,StrUtils,SysUtils,Windows,Dialogs,Messages,ShellAPI,Registry;

function GetUASPath:string;
function GetAppPath:string;
function LoadUAS( const aPath : string ):boolean;  // [mj] added aPath argument
function GetUASWnd:integer;
function GoDownloadUAS:integer;


implementation

function GoDownloadUAS(): integer ;
begin
	GoDownloadUAS:=ShellExecute(0,'open','http://www.flashpeak.com/ushell/ushell.htm',nil,nil,SW_SHOWNORMAL);
end;

function GetUASPath():string ;
var
	reg:TRegistry;
  s:string;
begin
	Reg := TRegistry.Create;
  s:='';
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\Software\FlashPeak\UAS', false) then
   	begin
      s:=Reg.ReadString('')+'\uas.exe';
      Reg.CloseKey;
    end
  finally
    Reg.Free;
  end;
	GetUASPath:=s;
end;

function GetAppPath(): string;
var
	path:string;
begin
	path:=Application.ExeName;
	getapppath:=LeftStr(path,LastDelimiter('\',path));
end;

function LoadUAS( const aPath : string ): boolean;  // [mj]
var
	sPath:string;
  pi:TPROCESSINFORMATION;
  si:TSTARTUPINFO;
  r:boolean;
  hWnd:integer;
begin
	FillChar(si,sizeof(si),0);
  si.wShowWindow:=SW_SHOWNOACTIVATE;
  si.cb:=sizeof(TSTARTUPINFO);
  if ( aPath = '' ) then // [mj]
    sPath:=GetUASPath
  else
    sPath := aPath;
  r:=CreateProcess(PChar(sPath),nil,nil,nil,false,NORMAL_PRIORITY_CLASS,nil,nil,si,pi);
	if r=false then
  begin
  	LoadUAS:=false;
    exit;
  end;
  if pi.hProcess<>0 then WaitForInputIdle(pi.hProcess,3000);
	hWnd:=GetUASWnd();
  if hWnd=0 then
  begin
  	LoadUAS:=false;
    exit;
  end;
  SendMessage(hWnd,WM_APP+$2002,GetCurrentProcessId,0);
  LoadUAS:=true;
end;

function GetUASWnd(): integer;
begin
	GetUASWnd:=FindWindow('#32770','UltimaShell Main Window');
end;


end.
