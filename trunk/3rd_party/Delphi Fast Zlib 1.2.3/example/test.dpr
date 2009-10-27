program test;

uses
  Forms,
  Unit_test in 'Unit_test.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
