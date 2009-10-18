unit erisdateconfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TForm_ErisCfg = class(TForm)
    RB_What: TRadioGroup;
    GroupBox1: TGroupBox;
    CB_DateLong: TCheckBox;
    CB_TimeLong: TCheckBox;
    Button_OK: TButton;
    Button_Cancel: TButton;
    RG_OnExec: TRadioGroup;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.DFM}


procedure TForm_ErisCfg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (( key = 27 ) and ( Shift = [] )) then
  begin
    key := 0;
    Close;
  end;
end;

procedure TForm_ErisCfg.FormActivate(Sender: TObject);
begin
  Randomize;
end;

end.
