program Test;

uses
  Forms,
  Main in 'Main.pas' {TestDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTestDlg, TestDlg);
  Application.Run;
end.
