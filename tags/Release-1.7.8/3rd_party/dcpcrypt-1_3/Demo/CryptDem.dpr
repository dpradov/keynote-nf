program CryptDem;

uses
  Forms,
  Main in 'Main.pas' {MainFrm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'CryptDem';
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
