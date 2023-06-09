{$IFDEF WIN32}
{$APPTYPE CONSOLE}
{$ENDIF}

{Compile from command line:

   DCC   CRC32Dem         (Delphi 1)
or
   DCC32 CRC32Dem         (Delphi 2/3/4/5)
}

PROGRAM CRC32Demo;

 {CRC32Demo demonstrates the use of the CRC32 UNIT.
  Modified from CRC16Demo.

  (C) Copyright 1989, 1996, 1999, Earl F. Glynn, Overland Park, KS.
  All Rights Reserved.}

  uses
   {$IFNDEF WIN32}
   WinCRT {Delphi 1 only},
   {$ENDIF}
   Winapi.Windows,   // DWORD
   System.SysUtils,
   CRC32 {CalcCRC32, CalcFileCRC32};  {IntToHex, pByteArray}

  CONST
    BufferSize = 16384;
    nElements  = 4;
    x:  ARRAY[1..nElements] OF DOUBLE = (1.0,2.5,6.02E23, 7.0);

    CRCinit:  DWORD = $FFFFFFFF;


  VAR
    b         :  BYTE;
    CRCvalue  :  DWORD;
    error     :  WORD;
    s         :  STRING;
    TotalBytes:  TInteger8;

  PROCEDURE FileChk (f:  STRING);
  BEGIN {FileChk}
    CalcFileCRC32 (f, CRCvalue,TotalBytes, error);

    IF   error = 0
    THEN WRITELN (IntToHex(CRCvalue,8),
                 FormatFloat('#########0', TotalBytes):10,'  ', f)
    ELSE BEGIN
      WRITE (' ':20, f);
      CASE error OF
        2:   WRITELN ('  <File not found>');
        3:   WRITELN ('  <Path not found>');
        5:   WRITELN ('  <File access denied>');
        ELSE WRITELN ('  <other error>', error)
      END
    END

  END {FileChk};


BEGIN {CRC32Demo}

  b := $AC;              {See "A Tutorial on CRC Computations", IEEE Micro,}
  CRCvalue := CRCinit;   {August 1988, p. 64, example calculation for bit  }
                         {string 101011 has CRC-16 value of $7D00 = 32000  }
  CalcCRC32 (Addr(b),1,CRCvalue);
  WRITELN ('CRC-32 code of bit string 101011 is ', IntToHex(CRCvalue,8));

  s := 'This is a sample string';
  CRCvalue := CRCinit;
  CalcCRC32 (Addr(s[1]), LENGTH(s), CRCvalue);  {exclude length byte}
  CRCvalue := NOT CRCvalue;
  WRITELN ('CRC-32 code of ''',s,''' is ', IntToHex(CRCvalue,8));

  Delete (s,11,7);
  CRCvalue := CRCinit;
  CalcCRC32 (Addr(s[1]), LENGTH(s), CRCvalue);  {exclude length byte}
  CRCvalue := NOT CRCvalue;
  WRITELN ('CRC-32 code of ''',s,''' is ', IntToHex(CRCvalue,8));

  CRCvalue := CRCinit;
  CalcCRC32 (Addr(x), nElements*SizeOf(x[1]), CRCvalue);
  CRCvalue := NOT CRCvalue;
  WRITELN ('CRC-32 code of real array 1 is ', IntToHex(CRCvalue,8));

  x[2] := 0.0; x[3] := 9.11E-31;
  CRCvalue := CRCinit;
  CalcCRC32 (Addr(x), nElements*SizeOf(x[1]), CRCvalue);
  CRCvalue := NOT CRCvalue;
  WRITELN ('CRC-32 code of real array 2 is ', IntToHex(CRCvalue,8));

  x[1] := 0.0; x[2] := 0.0; x[3] := 0.0; x[4] := 0.0;
  CRCvalue := CRCinit;
  CalcCRC32 (Addr(x), nElements*SizeOf(x[1]), CRCvalue);
  CRCvalue := NOT CRCvalue;
  WRITELN ('CRC-32 code of real array 3 is ', IntToHex(CRCvalue,8));


  WRITELN;
  WRITELN ('File CRC-32 Codes:');
  WRITELN;
  WRITELN (' CRC-32     Bytes        F i l e n a m e');
  WRITELN ('--------  --------  ------------------------');

  FileChk ('test.dat');
  FileChk ('C:\COMMAND.COM');
  FileChk ('C:\MSDOS.SYS');
  FileChk ('C:\IO.SYS');
  FileChk ('C:\WINP\PROJ\DDS\CLAIMMAN.EXE');
  FileChk ('abcLower.TXT');
  FileChk ('ABCUpper.TXT');
  FileChk ('ThisIsAString.TXT');
  FileChk (ParamStr(0));
  FileChk ('D:\EXISTS.NOT');
  FileChk ('C:\DOS');
  FileChk ('()[]{}.,/?*&$');

END {CRC32Demo}.
