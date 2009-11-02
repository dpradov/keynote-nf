unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, StdCtrls, ToolWin, Menus, UAS,ShellAPI;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    Memo1: TMemo;
    UltimaShell: TToolButton;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    item11: TMenuItem;
    procedure UltimaShellClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.UltimaShellClick(Sender: TObject);
var
	hWnd:integer;
  btn:TToolButton;
  pt:TPoint;
  r:Word;
  sPath:string;
begin
	sPath:=GetUASPath;
	if (sPath='') or (FileExists(sPath)=false) then
  begin
  	r:=MessageDlg('UltimaShell Autocompletion Server is not installed!'#13#10'Do you wish to download it now?',mtConfirmation,mbOKCancel,0);
    if (r=mrOk) then GoDownloadUAS;
    exit;
	end;
	hWnd:=GetUASWnd;
	if (hWnd=0) then LoadUAS('');
	hWnd:=GetUASWnd;
	if (hWnd=0) then exit;
  btn:=TToolButton(Sender);
  pt.x:=btn.left;
  pt.y:=btn.top+btn.Height;
  pt:=ToolBar1.ClientToScreen(pt);
  SetForegroundWindow(hWnd);
  PostMessage(hWnd,WM_APP+$2001,pt.x,pt.y);
end;

end.
