// CRC Calculator
//
// (C) Copyright 1996, 1998, 1999 Earl F. Glynn, Overland Park, KS.
// All Rights Reserved.

unit ScreenCRCCalculator;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TCRCCalc = class(TForm)
    CRCText: TEdit;
    CRC16Decimal: TLabel;
    CRC16hex: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CRC32Decimal: TLabel;
    Label4: TLabel;
    CRC32Hex: TLabel;
    LabelLabURL1: TLabel;
    LabelLabURL2: TLabel;
    procedure CRCTextChange(Sender: TObject);
    procedure LabelLabURL2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CRCCalc: TCRCCalc;

implementation

  USES
    CRC16,
    CRC32,
    ShellAPI;  // ShellExecute

{$R *.DFM}

procedure TCRCCalc.CRCTextChange(Sender: TObject);
  VAR
    CRC16:  WORD;
    CRC32:  DWORD;
    s    :  STRING;
begin
  s := CRCText.Text;
  CRC16 := 0;          // Could use $FFFF or other initial value
  IF   LENGTH(s) > 0   // Avoid access violation in D4
  THEN CalcCRC16 (Addr(s[1]), LENGTH(s), CRC16);
  CRC16Decimal.Caption := IntToStr(CRC16);
  CRC16Hex.Caption := IntToHex(CRC16,4);

  CRC32 := $FFFFFFFF;   // To match PKZIP
  IF   LENGTH(s) > 0    // Avoid access violation in D4
  THEN CalcCRC32 (Addr(s[1]), LENGTH(s), CRC32);
  CRC32 := NOT CRC32;   // TO match PKZIP
  CRC32Decimal.Caption := IntToStr(CRC32);
  CRC32Hex.Caption := IntToHex(CRC32,8)
end;


procedure TCRCCalc.LabelLabURL2Click(Sender: TObject);
begin
  ShellExecute(0, 'open', pchar('http://www.efg2.com/Lab'),
               NIL, NIL, SW_NORMAL)
end;

end.
