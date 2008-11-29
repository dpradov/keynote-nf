{******************************************************************************}
{** A UU encoding unit ********************************************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}
unit UUEnc;

interface
uses
  Sysutils;

{ UU encode and decode a string - standard but imposes a length restriction }
function UUEncode(const S: string): string;
function UUDecode(const S: string): string;

{******************************************************************************}
{******************************************************************************}
implementation

function _UUEncode(const S: string): string;
var
  i: integer;
  InBuf: array[0..2] of byte;
  OutBuf: array[0..3] of byte;
begin
  SetLength(Result,((Length(S)+2) div 3)*4);
  for i:= 1 to ((Length(S)+2) div 3) do
  begin
    if Length(S)< (i*3) then
      Move(S[(i-1)*3+1],InBuf,Length(S)-(i-1)*3)
    else
      Move(S[(i-1)*3+1],InBuf,3);
    OutBuf[0]:= ((InBuf[0] and $FC) shr 2) + 32;
    OutBuf[1]:= (((InBuf[0] and $03) shl 4) or ((InBuf[1] and $F0) shr 4)) + 32;
    OutBuf[2]:= (((InBuf[1] and $0F) shl 2) or ((InBuf[2] and $C0) shr 6)) + 32;
    OutBuf[3]:= (InBuf[2] and $3F) + 32;
    if OutBuf[0]= 32 then
      OutBuf[0]:= 96;
    if OutBuf[1]= 32 then
      OutBuf[1]:= 96;
    if OutBuf[2]= 32 then
      OutBuf[2]:= 96;
    if OutBuf[3]= 32 then
      OutBuf[3]:= 96;
    Move(OutBuf,Result[(i-1)*4+1],4);
  end;
end;

function _UUDecode(const S: string): string;
var
  i: integer;
  InBuf: array[0..3] of byte;
  OutBuf: array[0..2] of byte;
begin
  SetLength(Result,((Length(S)+2) div 4)*3);
  for i:= 1 to ((Length(S)+2) div 4) do
  begin
    if Length(S)< (i*4) then
      Move(S[(i-1)*4+1],InBuf,Length(S)-(i-1)*4)
    else
      Move(S[(i-1)*4+1],InBuf,4);
    if InBuf[0]= 96 then
      InBuf[0]:= 0
    else
      Dec(InBuf[0],32);
    if InBuf[1]= 96 then
      InBuf[1]:= 0
    else
      Dec(InBuf[1],32);
    if InBuf[2]= 96 then
      InBuf[2]:= 0
    else
      Dec(InBuf[2],32);
    if InBuf[3]= 96 then
      InBuf[3]:= 0
    else
      Dec(InBuf[3],32);
    OutBuf[0]:= (InBuf[0] shl 2) or ((InBuf[1] shr 4) and $03);
    OutBuf[1]:= (InBuf[1] shl 4) or ((InBuf[2] shr 2) and $0F);
    OutBuf[2]:= (InBuf[2] shl 6) or (InBuf[3] and $3F);
    Move(OutBuf,Result[(i-1)*3+1],3);
  end;
end;

function UUEncode;
begin
  if Length(S)> 45 then
    raise Exception.Create('UUEncode: String too long');
  if Length(S)= 0 then
    Result:= #96
  else
  begin
    Result:= char(Length(S)+32);
    Result:= Result + _UUEncode(S);
  end;
end;

function UUDecode;
begin
  if (S[1]< #33) or (S[1]> #97) then
    raise Exception.Create('UUDecode: Incorrect string format');
  Result:= _UUDecode(Copy(S,2,Length(S)-1));
  if S[1]<> #96 then
    SetLength(Result,byte(S[1])-32);
end;


end.
