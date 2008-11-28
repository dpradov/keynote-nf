{******************************************************************************}
{** A Base64 encoding unit ****************************************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}
unit Base64;

interface
uses
  Sysutils;

{ Base64 encode and decode a string }
function B64Encode(const S: string): string;
function B64Decode(const S: string): string;

{******************************************************************************}
{******************************************************************************}
implementation

const
  B64Table= 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

function B64Encode;
var
  i: integer;
  InBuf: array[0..2] of byte;
  OutBuf: array[0..3] of char;
begin
  SetLength(Result,((Length(S)+2) div 3)*4);
  for i:= 1 to ((Length(S)+2) div 3) do
  begin
    if Length(S)< (i*3) then
      Move(S[(i-1)*3+1],InBuf,Length(S)-(i-1)*3)
    else
      Move(S[(i-1)*3+1],InBuf,3);
    OutBuf[0]:= B64Table[((InBuf[0] and $FC) shr 2) + 1];
    OutBuf[1]:= B64Table[(((InBuf[0] and $03) shl 4) or ((InBuf[1] and $F0) shr 4)) + 1];
    OutBuf[2]:= B64Table[(((InBuf[1] and $0F) shl 2) or ((InBuf[2] and $C0) shr 6)) + 1];
    OutBuf[3]:= B64Table[(InBuf[2] and $3F) + 1];
    Move(OutBuf,Result[(i-1)*4+1],4);
  end;
  if (Length(S) mod 3)= 1 then
  begin
    Result[Length(Result)-1]:= '=';
    Result[Length(Result)]:= '=';
  end
  else if (Length(S) mod 3)= 2 then
    Result[Length(Result)]:= '=';
end;

function B64Decode;
var
  i: integer;
  InBuf: array[0..3] of byte;
  OutBuf: array[0..2] of byte;
begin
  if (Length(S) mod 4)<> 0 then
    raise Exception.Create('Base64: Incorrect string format');
  SetLength(Result,((Length(S) div 4)-1)*3);
  for i:= 1 to ((Length(S) div 4)-1) do
  begin
    Move(S[(i-1)*4+1],InBuf,4);
    if (InBuf[0]> 64) and (InBuf[0]< 91) then
      Dec(InBuf[0],65)
    else if (InBuf[0]> 96) and (InBuf[0]< 123) then
      Dec(InBuf[0],71)
    else if (InBuf[0]> 47) and (InBuf[0]< 58) then
      Inc(InBuf[0],4)
    else if InBuf[0]= 43 then
      InBuf[0]:= 62
    else
      InBuf[0]:= 63;
    if (InBuf[1]> 64) and (InBuf[1]< 91) then
      Dec(InBuf[1],65)
    else if (InBuf[1]> 96) and (InBuf[1]< 123) then
      Dec(InBuf[1],71)
    else if (InBuf[1]> 47) and (InBuf[1]< 58) then
      Inc(InBuf[1],4)
    else if InBuf[1]= 43 then
      InBuf[1]:= 62
    else
      InBuf[1]:= 63;
    if (InBuf[2]> 64) and (InBuf[2]< 91) then
      Dec(InBuf[2],65)
    else if (InBuf[2]> 96) and (InBuf[2]< 123) then
      Dec(InBuf[2],71)
    else if (InBuf[2]> 47) and (InBuf[2]< 58) then
      Inc(InBuf[2],4)
    else if InBuf[2]= 43 then
      InBuf[2]:= 62
    else
      InBuf[2]:= 63;
    if (InBuf[3]> 64) and (InBuf[3]< 91) then
      Dec(InBuf[3],65)
    else if (InBuf[3]> 96) and (InBuf[3]< 123) then
      Dec(InBuf[3],71)
    else if (InBuf[3]> 47) and (InBuf[3]< 58) then
      Inc(InBuf[3],4)
    else if InBuf[3]= 43 then
      InBuf[3]:= 62
    else
      InBuf[3]:= 63;
    OutBuf[0]:= (InBuf[0] shl 2) or ((InBuf[1] shr 4) and $03);
    OutBuf[1]:= (InBuf[1] shl 4) or ((InBuf[2] shr 2) and $0F);
    OutBuf[2]:= (InBuf[2] shl 6) or (InBuf[3] and $3F);
    Move(OutBuf,Result[(i-1)*3+1],3);
  end;
  if Length(S)<> 0 then
  begin
    Move(S[Length(S)-3],InBuf,4);
    if InBuf[2]= 61 then
    begin
      if (InBuf[0]> 64) and (InBuf[0]< 91) then
        Dec(InBuf[0],65)
      else if (InBuf[0]> 96) and (InBuf[0]< 123) then
        Dec(InBuf[0],71)
      else if (InBuf[0]> 47) and (InBuf[0]< 58) then
        Inc(InBuf[0],4)
      else if InBuf[0]= 43 then
        InBuf[0]:= 62
      else
        InBuf[0]:= 63;
      if (InBuf[1]> 64) and (InBuf[1]< 91) then
        Dec(InBuf[1],65)
      else if (InBuf[1]> 96) and (InBuf[1]< 123) then
        Dec(InBuf[1],71)
      else if (InBuf[1]> 47) and (InBuf[1]< 58) then
        Inc(InBuf[1],4)
      else if InBuf[1]= 43 then
        InBuf[1]:= 62
      else
        InBuf[1]:= 63;
      OutBuf[0]:= (InBuf[0] shl 2) or ((InBuf[1] shr 4) and $03);
      Result:= Result + char(OutBuf[0]);
    end
    else if InBuf[3]= 61 then
    begin
      if (InBuf[0]> 64) and (InBuf[0]< 91) then
        Dec(InBuf[0],65)
      else if (InBuf[0]> 96) and (InBuf[0]< 123) then
        Dec(InBuf[0],71)
      else if (InBuf[0]> 47) and (InBuf[0]< 58) then
        Inc(InBuf[0],4)
      else if InBuf[0]= 43 then
        InBuf[0]:= 62
      else
        InBuf[0]:= 63;
      if (InBuf[1]> 64) and (InBuf[1]< 91) then
        Dec(InBuf[1],65)
      else if (InBuf[1]> 96) and (InBuf[1]< 123) then
        Dec(InBuf[1],71)
      else if (InBuf[1]> 47) and (InBuf[1]< 58) then
        Inc(InBuf[1],4)
      else if InBuf[1]= 43 then
        InBuf[1]:= 62
      else
        InBuf[1]:= 63;
      if (InBuf[2]> 64) and (InBuf[2]< 91) then
        Dec(InBuf[2],65)
      else if (InBuf[2]> 96) and (InBuf[2]< 123) then
        Dec(InBuf[2],71)
      else if (InBuf[2]> 47) and (InBuf[2]< 58) then
        Inc(InBuf[2],4)
      else if InBuf[2]= 43 then
        InBuf[2]:= 62
      else
        InBuf[2]:= 63;
      OutBuf[0]:= (InBuf[0] shl 2) or ((InBuf[1] shr 4) and $03);
      OutBuf[1]:= (InBuf[1] shl 4) or ((InBuf[2] shr 2) and $0F);
      Result:= Result + char(OutBuf[0]) + char(OutBuf[1]);
    end
    else
    begin
      if (InBuf[0]> 64) and (InBuf[0]< 91) then
        Dec(InBuf[0],65)
      else if (InBuf[0]> 96) and (InBuf[0]< 123) then
        Dec(InBuf[0],71)
      else if (InBuf[0]> 47) and (InBuf[0]< 58) then
        Inc(InBuf[0],4)
      else if InBuf[0]= 43 then
        InBuf[0]:= 62
      else
        InBuf[0]:= 63;
      if (InBuf[1]> 64) and (InBuf[1]< 91) then
        Dec(InBuf[1],65)
      else if (InBuf[1]> 96) and (InBuf[1]< 123) then
        Dec(InBuf[1],71)
      else if (InBuf[1]> 47) and (InBuf[1]< 58) then
        Inc(InBuf[1],4)
      else if InBuf[1]= 43 then
        InBuf[1]:= 62
      else
        InBuf[1]:= 63;
      if (InBuf[2]> 64) and (InBuf[2]< 91) then
        Dec(InBuf[2],65)
      else if (InBuf[2]> 96) and (InBuf[2]< 123) then
        Dec(InBuf[2],71)
      else if (InBuf[2]> 47) and (InBuf[2]< 58) then
        Inc(InBuf[2],4)
      else if InBuf[2]= 43 then
        InBuf[2]:= 62
      else
        InBuf[2]:= 63;
      if (InBuf[3]> 64) and (InBuf[3]< 91) then
        Dec(InBuf[3],65)
      else if (InBuf[3]> 96) and (InBuf[3]< 123) then
        Dec(InBuf[3],71)
      else if (InBuf[3]> 47) and (InBuf[3]< 58) then
        Inc(InBuf[3],4)
      else if InBuf[3]= 43 then
        InBuf[3]:= 62
      else
        InBuf[3]:= 63;
      OutBuf[0]:= (InBuf[0] shl 2) or ((InBuf[1] shr 4) and $03);
      OutBuf[1]:= (InBuf[1] shl 4) or ((InBuf[2] shr 2) and $0F);
      OutBuf[2]:= (InBuf[2] shl 6) or (InBuf[3] and $3F);
      Result:= Result + Char(OutBuf[0]) + Char(OutBuf[1]) + Char(OutBuf[2]);
    end;
  end;
end;

end.
