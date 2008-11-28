program Demo;

uses
  Forms,
  Main in 'MAIN.PAS' {frmMain},
  About in 'ABOUT.PAS' {frmAbout};

{$R *.RES}

begin
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.
