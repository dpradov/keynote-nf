unit KNCalendarform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Grids, Calendar;

type
  TForm_Cal = class(TForm)
    GroupBox1: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    DTP: TDateTimePicker;
    CB_LongDate: TCheckBox;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CB_LongDateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.DFM}

procedure TForm_Cal.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if ( shift = [] ) then
    begin
      key := 0;
      modalresult := mrCancel;
    end;
  end;
end;

procedure TForm_Cal.CB_LongDateClick(Sender: TObject);
begin
  if CB_LongDate.Checked then
    DTP.DateFormat := dfLong
  else
    DTP.DateFormat := dfShort;
end;

procedure TForm_Cal.FormCreate(Sender: TObject);
begin
  DTP.Date := now;
end;

end.
