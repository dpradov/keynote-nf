program Demo;

uses
  Controls,
  Forms,
  Demo1 in 'Demo1.pas' {DemoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Toolbar97 Demo';
  Application.CreateForm(TDemoForm, DemoForm);
  Application.Run;
end.
