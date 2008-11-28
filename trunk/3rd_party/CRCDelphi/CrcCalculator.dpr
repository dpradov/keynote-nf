// CRC Calculator
//
// (C) Copyright 1996, 1998 Earl F. Glynn, Overland Park, KS.
// All Rights Reserved.

program CrcCalculator;

uses
  Forms,
  ScreenCRCCalculator in 'ScreenCRCCalculator.pas' {CRCCalc},
  CRC32 in 'crc32.pas';

{$R *.RES}

begin
  Application.CreateForm(TCRCCalc, CRCCalc);
  Application.Run;
end.
