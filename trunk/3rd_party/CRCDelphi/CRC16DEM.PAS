{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}

{Compile from command line:

   DCC   CRC16Dem         (Delphi 1)
or
   DCC32 CRC16Dem         (Delphi 2 or 3)
}

PROGRAM CRC16Demo;

 {CRC16Demo demonstrates the use of the CRC16 UNIT.

  (C) Copyright 1989, 1996, Earl F. Glynn, Overland Park, KS.
  All Rights Reserved.}

  USES
{$IFNDEF WIN32}
    WinCRT,    {Delphi 1 only}
{$ENDIF}
    CRC16,     {CalcCRC16, CalcFileCRC}
    SysUtils;  {IntToHex}

  CONST
    BufferSize = 16384;
    nElements  = 4;
    x:  ARRAY[1..nElements] OF DOUBLE = (1.0,2.5,6.02E23, 7.0);

    CRCinit:  WORD = 0;  {Other values are possible, e.g., $FFFF}

  TYPE
    Buffer = ARRAY[1..BufferSize] OF BYTE;

  VAR
    b         :  BYTE;
    CRCvalue  :  WORD;
    error     :  WORD;
    IOBuffer  :  ^Buffer;
    s         :  STRING;

  PROCEDURE FileChk (f:  STRING);
  BEGIN {FileChk}
    CalcFileCRC16 (f, CRCvalue, Pointer(IOBuffer), BufferSize, error);
    IF   error = 0
    THEN WRITELN ('  ', IntToHex(CRCvalue,4), '    ', f)
    ELSE BEGIN
      WRITE (' ':10, f);
      CASE error OF
        2:   WRITELN ('  <File not found>');
        3:   WRITELN ('  <Path not found>');
        5:   WRITELN ('  <File access denied>');
        ELSE WRITELN ('  <other error>', error)
      END
    END

  END {FileChk};


BEGIN {CRC16Demo}

  b := $AC;              {See "A Tutorial on CRC Computations", IEEE Micro,}
  CRCvalue := CRCinit;   {August 1988, p. 64, example calculation for bit  }
                         {string 101011 has CRC-16 value of $7D00 = 32000  }
  CalcCRC16 (Addr(b),1,CRCvalue);
  WRITELN ('CRC-16 code of bit string 101011 is ', IntToHex(CRCvalue,4));

  s := 'This is a sample string';
  CRCvalue := CRCinit;
  CalcCRC16 (Addr(s[1]), LENGTH(s), CRCvalue);  {exclude length byte}
  WRITELN ('CRC-16 code of ''',s,''' is ', IntToHex(CRCvalue,4));

  Delete (s,11,7);
  CRCvalue := CRCinit;
  CalcCRC16 (Addr(s[1]), LENGTH(s), CRCvalue);  {exclude length byte}
  WRITELN ('CRC-16 code of ''',s,''' is ', IntToHex(CRCvalue,4));

  CRCvalue := CRCinit;
  CalcCRC16 (Addr(x), nElements*SizeOf(x[1]), CRCvalue);
  WRITELN ('CRC-16 code of real array 1 is ', IntToHex(CRCvalue,4));

  x[2] := 0.0; x[3] := 9.11E-31;
  CRCvalue := CRCinit;
  CalcCRC16 (Addr(x), nElements*SizeOf(x[1]), CRCvalue);
  WRITELN ('CRC-16 code of real array 2 is ', IntToHex(CRCvalue,4));

  x[1] := 0.0; x[2] := 0.0; x[3] := 0.0; x[4] := 0.0;
  CRCvalue := CRCinit;
  CalcCRC16 (Addr(x), nElements*SizeOf(x[1]), CRCvalue);
  WRITELN ('CRC-16 code of real array 3 is ', IntToHex(CRCvalue,4));

  NEW (IOBuffer);

  WRITELN;
  WRITELN ('File CRC-16 Codes:');
  WRITELN;
  WRITELN (' CRC-16       F i l e n a m e');
  WRITELN ('--------  ------------------------');

  FileChk ('C:\COMMAND.COM');
  FileChk ('C:\MSDOS.SYS');
  FileChk ('C:\IO.SYS');
  FileChk (ParamStr(0));
  FileChk ('()[]{}.,/?*&$');

  DISPOSE (IOBuffer)

END {CRC16Demo}.
