unit AJBSpellerReg;

interface

uses Classes,AJBSpeller;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('AJB', [TAJBSpell]);
end;

end.
 