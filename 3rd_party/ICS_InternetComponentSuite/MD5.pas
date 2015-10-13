{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Franois PIETTE. Based on work given by Louis S. Berman from
              BrainTree Ltd, lsb@braintree.com
Description:  MD5 is an implmentation for the MD5 Message-Digest Algorithm
              as described in RFC-1321
Creation:     October 11, 1997
Version:      1.03
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997, 1998 by Franois PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

Updates:
Oct 26, 1997 Changed MD5Final form function to procedure to be compatible
             with C++Builder.
Jul 09, 1998 V1.01 Adapted for Delphi 4
Aug 06, 1998 V1.02 Added R- Q- directive
Jun 05, 1999 V1.03 Wolfgang Klein found a bug in MD5Update.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit MD5;

interface

uses
    SysUtils;

const
    MD5Version         = 102;
    CopyRight : String = ' MD5 Message-Digest (c) 97-98 F. Piette V1.02 ';

{$Q-}
{$R-}

type
    TMD5Context = record
        State: array[0..3] of LongInt;
        Count: array[0..1] of LongInt;
        case Integer of
        0: (BufChar: array[0..63] of Byte);
        1: (BufLong: array[0..15] of LongInt);
    end;
    TMD5Digest = array[0..15] of Char;

procedure MD5Init(var MD5Context: TMD5Context);
procedure MD5Update(var MD5Context: TMD5Context;
                    const Data;
                    Len: Integer);
procedure MD5Transform(var Buf: array of LongInt;
                       const Data: array of LongInt);
procedure MD5UpdateBuffer(var MD5Context: TMD5Context;
                          Buffer: Pointer;
                          BufSize: Integer);
procedure MD5Final(var Digest: TMD5Digest; var MD5Context: TMD5Context);

function GetMD5(Buffer: Pointer; BufSize: Integer): string;
function StrMD5(Buffer : String): string;

implementation

const
    MaxBufSize = 16384;

type
    PMD5Buffer = ^TMD5Buffer;
    TMD5Buffer = array[0..(MaxBufSize - 1)] of Char;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ MD5 initialization. Begins an MD5 operation, writing a new context.         }
procedure MD5Init(var MD5Context: TMD5Context);
begin
    FillChar(MD5Context, SizeOf(TMD5Context), #0);
    with MD5Context do begin
        { Load magic initialization constants. }
        State[0] := LongInt($67452301);
        State[1] := LongInt($EFCDAB89);
        State[2] := LongInt($98BADCFE);
        State[3] := LongInt($10325476);
    end
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ MD5 block update operation. Continues an MD5 message-digest operation,      }
{ processing another message block, and updating the context.                 }
procedure MD5Update(
    var MD5Context: TMD5Context;            { Context                         }
    const Data;                             { Input block                     }
    Len: Integer);                          { Length of input block           }
type
    TByteArray = array[0..0] of Byte;
var
    Index: Word;
    T: LongInt;
begin
    with MD5Context do begin
        T := Count[0];
        Inc(Count[0], LongInt(Len) shl 3);
        if Count[0] < T then
            Inc(Count[1]);
        Inc(Count[1], Len shr 29);
        T := (T shr 3) and $3F;
        Index := 0;
        if T <> 0 then begin
            Index := T;
            T := 64 - T;
            if Len < T then begin
                Move(Data, BufChar[Index], Len);
                Exit;
            end;
            Move(Data, BufChar[Index], T);
            MD5Transform(State, BufLong);
            Dec(Len, T);
            Index := T;  { Wolfgang Klein, 05/06/99 }
        end;
        while Len >= 64 do begin
            Move(TByteArray(Data)[Index], BufChar, 64);
            MD5Transform(State, BufLong);
            Inc(Index, 64);
            Dec(Len, 64);
        end;
        Move(TByteArray(Data)[Index], BufChar, Len);
    end
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ MD5 finalization. Ends an MD5 message-digest operation, writing the message }
{ digest and zeroizing the context.                                           }
procedure MD5Final(var Digest: TMD5Digest; var MD5Context: TMD5Context);
var
    Cnt : Word;
    P   : Byte;
begin
    with MD5Context do begin
        Cnt := (Count[0] shr 3) and $3F;
        P := Cnt;
        BufChar[P] := $80;
        Inc(P);
        Cnt := 64 - 1 - Cnt;
        if Cnt < 8 then begin
            FillChar(BufChar[P], Cnt, #0);
            MD5Transform(State, BufLong);
            FillChar(BufChar, 56, #0);
        end
        else
            FillChar(BufChar[P], Cnt - 8, #0);
        BufLong[14] := Count[0];
        BufLong[15] := Count[1];
        MD5Transform(State, BufLong);
        Move(State, Digest, 16)
    end;
    FillChar(MD5Context, SizeOf(TMD5Context), #0)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ MD5 basic transformation. Transforms state based on block.                  }
procedure MD5Transform(
    var Buf: array of LongInt;
    const Data: array of LongInt);
var
    A, B, C, D: LongInt;

    procedure Round1(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
    begin
        Inc(W, (Z xor (X and (Y xor Z))) + Data);
        W := (W shl S) or (W shr (32 - S));
        Inc(W, X)
    end;

    procedure Round2(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
    begin
        Inc(W, (Y xor (Z and (X xor Y))) + Data);
        W := (W shl S) or (W shr (32 - S));
        Inc(W, X)
    end;

    procedure Round3(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
    begin
        Inc(W, (X xor Y xor Z) + Data);
        W := (W shl S) or (W shr (32 - S));
        Inc(W, X)
    end;

    procedure Round4(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
    begin
        Inc(W, (Y xor (X or not Z)) + Data);
        W := (W shl S) or (W shr (32 - S));
        Inc(W, X)
    end;
begin
    A := Buf[0];
    B := Buf[1];
    C := Buf[2];
    D := Buf[3];

    Round1(A, B, C, D, Data[ 0] + LongInt($d76aa478),  7);
    Round1(D, A, B, C, Data[ 1] + LongInt($e8c7b756), 12);
    Round1(C, D, A, B, Data[ 2] + LongInt($242070db), 17);
    Round1(B, C, D, A, Data[ 3] + LongInt($c1bdceee), 22);
    Round1(A, B, C, D, Data[ 4] + LongInt($f57c0faf),  7);
    Round1(D, A, B, C, Data[ 5] + LongInt($4787c62a), 12);
    Round1(C, D, A, B, Data[ 6] + LongInt($a8304613), 17);
    Round1(B, C, D, A, Data[ 7] + LongInt($fd469501), 22);
    Round1(A, B, C, D, Data[ 8] + LongInt($698098d8),  7);
    Round1(D, A, B, C, Data[ 9] + LongInt($8b44f7af), 12);
    Round1(C, D, A, B, Data[10] + LongInt($ffff5bb1), 17);
    Round1(B, C, D, A, Data[11] + LongInt($895cd7be), 22);
    Round1(A, B, C, D, Data[12] + LongInt($6b901122),  7);
    Round1(D, A, B, C, Data[13] + LongInt($fd987193), 12);
    Round1(C, D, A, B, Data[14] + LongInt($a679438e), 17);
    Round1(B, C, D, A, Data[15] + LongInt($49b40821), 22);

    Round2(A, B, C, D, Data[ 1] + LongInt($f61e2562),  5);
    Round2(D, A, B, C, Data[ 6] + LongInt($c040b340),  9);
    Round2(C, D, A, B, Data[11] + LongInt($265e5a51), 14);
    Round2(B, C, D, A, Data[ 0] + LongInt($e9b6c7aa), 20);
    Round2(A, B, C, D, Data[ 5] + LongInt($d62f105d),  5);
    Round2(D, A, B, C, Data[10] + LongInt($02441453),  9);
    Round2(C, D, A, B, Data[15] + LongInt($d8a1e681), 14);
    Round2(B, C, D, A, Data[ 4] + LongInt($e7d3fbc8), 20);
    Round2(A, B, C, D, Data[ 9] + LongInt($21e1cde6),  5);
    Round2(D, A, B, C, Data[14] + LongInt($c33707d6),  9);
    Round2(C, D, A, B, Data[ 3] + LongInt($f4d50d87), 14);
    Round2(B, C, D, A, Data[ 8] + LongInt($455a14ed), 20);
    Round2(A, B, C, D, Data[13] + LongInt($a9e3e905),  5);
    Round2(D, A, B, C, Data[ 2] + LongInt($fcefa3f8),  9);
    Round2(C, D, A, B, Data[ 7] + LongInt($676f02d9), 14);
    Round2(B, C, D, A, Data[12] + LongInt($8d2a4c8a), 20);

    Round3(A, B, C, D, Data[ 5] + LongInt($fffa3942),  4);
    Round3(D, A, B, C, Data[ 8] + LongInt($8771f681), 11);
    Round3(C, D, A, B, Data[11] + LongInt($6d9d6122), 16);
    Round3(B, C, D, A, Data[14] + LongInt($fde5380c), 23);
    Round3(A, B, C, D, Data[ 1] + LongInt($a4beea44),  4);
    Round3(D, A, B, C, Data[ 4] + LongInt($4bdecfa9), 11);
    Round3(C, D, A, B, Data[ 7] + LongInt($f6bb4b60), 16);
    Round3(B, C, D, A, Data[10] + LongInt($bebfbc70), 23);
    Round3(A, B, C, D, Data[13] + LongInt($289b7ec6),  4);
    Round3(D, A, B, C, Data[ 0] + LongInt($eaa127fa), 11);
    Round3(C, D, A, B, Data[ 3] + LongInt($d4ef3085), 16);
    Round3(B, C, D, A, Data[ 6] + LongInt($04881d05), 23);
    Round3(A, B, C, D, Data[ 9] + LongInt($d9d4d039),  4);
    Round3(D, A, B, C, Data[12] + LongInt($e6db99e5), 11);
    Round3(C, D, A, B, Data[15] + LongInt($1fa27cf8), 16);
    Round3(B, C, D, A, Data[ 2] + LongInt($c4ac5665), 23);

    Round4(A, B, C, D, Data[ 0] + LongInt($f4292244),  6);
    Round4(D, A, B, C, Data[ 7] + LongInt($432aff97), 10);
    Round4(C, D, A, B, Data[14] + LongInt($ab9423a7), 15);
    Round4(B, C, D, A, Data[ 5] + LongInt($fc93a039), 21);
    Round4(A, B, C, D, Data[12] + LongInt($655b59c3),  6);
    Round4(D, A, B, C, Data[ 3] + LongInt($8f0ccc92), 10);
    Round4(C, D, A, B, Data[10] + LongInt($ffeff47d), 15);
    Round4(B, C, D, A, Data[ 1] + LongInt($85845dd1), 21);
    Round4(A, B, C, D, Data[ 8] + LongInt($6fa87e4f),  6);
    Round4(D, A, B, C, Data[15] + LongInt($fe2ce6e0), 10);
    Round4(C, D, A, B, Data[ 6] + LongInt($a3014314), 15);
    Round4(B, C, D, A, Data[13] + LongInt($4e0811a1), 21);
    Round4(A, B, C, D, Data[ 4] + LongInt($f7537e82),  6);
    Round4(D, A, B, C, Data[11] + LongInt($bd3af235), 10);
    Round4(C, D, A, B, Data[ 2] + LongInt($2ad7d2bb), 15);
    Round4(B, C, D, A, Data[ 9] + LongInt($eb86d391), 21);

    Inc(Buf[0], A);
    Inc(Buf[1], B);
    Inc(Buf[2], C);
    Inc(Buf[3], D);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure MD5UpdateBuffer(
    var MD5Context: TMD5Context;
    Buffer: Pointer;
    BufSize: Integer);
var
    BufTmp : PMD5Buffer;
    BufPtr : PChar;
    Bytes  : Word;
begin
    New(BufTmp);
    BufPtr := Buffer;
    try
        repeat
            if BufSize > MaxBufSize then
                Bytes := MaxBufSize
            else
                Bytes := BufSize;
            Move(BufPtr^, BufTmp^, Bytes);
            Inc(BufPtr, Bytes);
            Dec(BufSize, Bytes);
            if Bytes > 0 then
                MD5Update(MD5Context, BufTmp^, Bytes);
        until Bytes < MaxBufSize;
    finally
        Dispose(BufTmp);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetMD5(Buffer: Pointer; BufSize: Integer): string;
var
    I          : Integer;
    MD5Digest  : TMD5Digest;
    MD5Context : TMD5Context;
begin
    for I := 0 to 15 do
        Byte(MD5Digest[I]) := I + 1;
    MD5Init(MD5Context);
    MD5UpdateBuffer(MD5Context, Buffer, BufSize);
    MD5Final(MD5Digest, MD5Context);
    Result := '';
    for I := 0 to 15 do
        Result := Result + IntToHex(Byte(MD5Digest[I]), 2);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StrMD5(Buffer : String): string;
begin
    Result := GetMD5(@Buffer[1], Length(Buffer));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.