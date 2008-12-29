program TestUAS;

uses
  Forms,
  Windows,
  Messages,
  MainFrm in 'MainFrm.pas' {Form1},
  UAS in 'UAS.pas';

{$R *.res}
begin
  Application.Initialize;
  LoadUAS;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
  if GetUASWnd<>0 then SendMessage(GetUASWnd,WM_CLOSE,0,0);
end.
