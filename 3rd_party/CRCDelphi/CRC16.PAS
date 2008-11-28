UNIT CRC16;

 {CRC16 calculates a cyclic redundancy code (CRC), known as CRC-16, using
  a byte-wise algorithm.

  (C) Copyright 1989, 1996 Earl F. Glynn, Overland Park, KS.
  All Rights Reserved.

  This UNIT was derived from the CRCT FORTRAN 77 program given in
  "Byte-wise CRC Calculations" by Aram Perez in IEEE Micro, June 1983,
  pp. 40-50.  The constants here are for the CRC-16 generator polynomial,
  X^16 + X^15 + X^2 + 1.  While the constants are specific to the
  CRC-16 polynomial, the algorithm is general and will calculate the
  CRC for whatever set of constants is present.

  This CRC algorithm emphasizes speed at the expense of the 512 element
  lookup table.}

INTERFACE

  PROCEDURE CalcCRC16 (p:  pointer; nbyte:  WORD; VAR CRCvalue:  WORD);
  PROCEDURE CalcFileCRC16 (FromName:  STRING; VAR CRCvalue:  WORD;
              VAR IOBuffer:  pointer;  BufferSize:  WORD; VAR error:  WORD);

IMPLEMENTATION

  CONST
    table:  ARRAY[0..255] OF WORD =
     ($0000,$C0C1,$C181,$0140,$C301,$03C0,$0280,$C241,$C601,$06C0,$0780,
      $C741,$0500,$C5C1,$C481,$0440,$CC01,$0CC0,$0D80,$CD41,$0F00,$CFC1,
      $CE81,$0E40,$0A00,$CAC1,$CB81,$0B40,$C901,$09C0,$0880,$C841,$D801,
      $18C0,$1980,$D941,$1B00,$DBC1,$DA81,$1A40,$1E00,$DEC1,$DF81,$1F40,
      $DD01,$1DC0,$1C80,$DC41,$1400,$D4C1,$D581,$1540,$D701,$17C0,$1680,
      $D641,$D201,$12C0,$1380,$D341,$1100,$D1C1,$D081,$1040,$F001,$30C0,
      $3180,$F141,$3300,$F3C1,$F281,$3240,$3600,$F6C1,$F781,$3740,$F501,
      $35C0,$3480,$F441,$3C00,$FCC1,$FD81,$3D40,$FF01,$3FC0,$3E80,$FE41,
      $FA01,$3AC0,$3B80,$FB41,$3900,$F9C1,$F881,$3840,$2800,$E8C1,$E981,
      $2940,$EB01,$2BC0,$2A80,$EA41,$EE01,$2EC0,$2F80,$EF41,$2D00,$EDC1,
      $EC81,$2C40,$E401,$24C0,$2580,$E541,$2700,$E7C1,$E681,$2640,$2200,
      $E2C1,$E381,$2340,$E101,$21C0,$2080,$E041,$A001,$60C0,$6180,$A141,
      $6300,$A3C1,$A281,$6240,$6600,$A6C1,$A781,$6740,$A501,$65C0,$6480,
      $A441,$6C00,$ACC1,$AD81,$6D40,$AF01,$6FC0,$6E80,$AE41,$AA01,$6AC0,
      $6B80,$AB41,$6900,$A9C1,$A881,$6840,$7800,$B8C1,$B981,$7940,$BB01,
      $7BC0,$7A80,$BA41,$BE01,$7EC0,$7F80,$BF41,$7D00,$BDC1,$BC81,$7C40,
      $B401,$74C0,$7580,$B541,$7700,$B7C1,$B681,$7640,$7200,$B2C1,$B381,
      $7340,$B101,$71C0,$7080,$B041,$5000,$90C1,$9181,$5140,$9301,$53C0,
      $5280,$9241,$9601,$56C0,$5780,$9741,$5500,$95C1,$9481,$5440,$9C01,
      $5CC0,$5D80,$9D41,$5F00,$9FC1,$9E81,$5E40,$5A00,$9AC1,$9B81,$5B40,
      $9901,$59C0,$5880,$9841,$8801,$48C0,$4980,$8941,$4B00,$8BC1,$8A81,
      $4A40,$4E00,$8EC1,$8F81,$4F40,$8D01,$4DC0,$4C80,$8C41,$4400,$84C1,
      $8581,$4540,$8701,$47C0,$4680,$8641,$8201,$42C0,$4380,$8341,$4100,
      $81C1,$8081,$4040);

  TYPE
    buffer = ARRAY[1..65521] OF BYTE;  {largest buffer that can be}
                                       {allocated on heap in TP/Delphi 1}

  PROCEDURE CalcCRC16 (p:  pointer; nbyte:  WORD; VAR CRCvalue:  WORD);
    VAR
      i:  WORD;
      q:  ^buffer;

   {The following is a little cryptic (but executes very quickly).
    The algorithm is as follows:
      1.  exclusive-or the input byte with the low-order byte of
          the CRC register to get an INDEX
      2.  shift the CRC register eight bits to the right
      3.  exclusive-or the CRC register with the contents of
          Table[INDEX]
      4.  repeat steps 1 through 3 for all bytes}
  BEGIN
    q := p;
    FOR   i := 1 TO nBYTE DO
      CRCvalue := Hi(CRCvalue)  XOR  Table[ q^[i] XOR Lo(CRCvalue) ]
  END {CalcCRC16};

  PROCEDURE CalcFileCRC16 (FromName:  STRING; VAR CRCvalue:  WORD;
              VAR IOBuffer:  pointer;  BufferSize:  WORD; VAR error:  WORD);
    VAR
      BytesRead:  INTEGER;
      FromFile :  FILE;
  BEGIN
    FileMode := 0;  {Turbo default is 2 for R/W; 0 is for R/O}
    CRCValue := $FFFF;
    ASSIGN (FromFile,FromName);
    {$I-} RESET (FromFile,1); {$I+}
    error := IOResult;
    IF   error = 0
    THEN BEGIN
      REPEAT
        BlockRead (FromFile,IOBuffer^,BufferSize,BytesRead);
        CalcCRC16 (IOBuffer,BytesRead,CRCvalue)
      UNTIL BytesRead = 0;
      CLOSE (FromFile)
    END
  END {CalcFileCRC16};

END {CRC}.
