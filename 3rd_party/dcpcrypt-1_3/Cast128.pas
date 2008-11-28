{******************************************************************************}
{** A binary compatible implementation of Cast128 *****************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{** Based on C source written by Steve Reid (sreid@sea-to-sky.net) ************}
{******************************************************************************}
unit Cast128;

interface
{$I DCPcrypt.inc}
uses
  {$IFDEF CFORM}Classes, Sysutils, {$ENDIF}DCPcrypt;

{$IFDEF CFORM}
type
  TDCP_cast128= class(TDCP_blockcipher)
  protected
    IV, LB: array[0..7] of byte;
    KeyData: array[0..31] of DWord;
    Rounds: longint;
    procedure Encrypt(const InBlock; var OutBlock);
    procedure Decrypt(const InBlock; var OutBlock);
  public
    procedure Init(var Key; Size: longint; IVector: pointer); override;
    procedure Burn; override;
    procedure Reset; override;
    procedure EncryptECB(const InBlock; var OutBlock); override;
    procedure DecryptECB(const InBlock; var OutBlock); override;
    procedure EncryptCBC(const InData; var OutData; Size: longint); override;
    procedure DecryptCBC(const InData; var OutData; Size: longint); override;
    procedure EncryptCFB(const InData; var OutData; Size: longint); override;
    procedure DecryptCFB(const InData; var OutData; Size: longint); override;
    constructor Create(AOwner: TComponent); override;
  end;
{$ELSE}
type
  TCast128Data= record
    IV, LB: array[0..7] of byte;
    KeyData: array[0..31] of DWord;
    Rounds: longint;
  end;

procedure Cast128Init(var Data: TCast128Data; var Key; Size: longint; IVector: pointer);
procedure Cast128Reset(var Data: TCast128Data);
procedure Cast128Burn(var Data:  TCast128Data);
procedure Cast128EncryptECB(var Data: TCast128Data; const InBlock; var OutBlock);
procedure Cast128DecryptECB(var Data: TCast128Data; const InBlock; var OutBlock);
procedure Cast128EncryptCBC(var Data: TCast128Data; const InData; var OutData; Size: longint);
procedure Cast128DecryptCBC(var Data: TCast128Data; const InData; var OutData; Size: longint);
procedure Cast128EncryptCFB(var Data: TCast128Data; const InData; var OutData; Size: longint);
procedure Cast128DecryptCFB(var Data: TCast128Data; const InData; var OutData; Size: longint);
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
implementation

{$I Cast128.Inc}

{$IFDEF CFORM}
constructor TDCP_cast128.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAlgorithm:= 'Cast128';
  fBlockSize:= 64;
  fMaxKeySize:= 128;
  fID:= 7;
  Burn;
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_cast128.Encrypt(const InBlock; var OutBlock);
{$ELSE}
procedure Cast128EncryptECB;
{$ENDIF}
var
  t, l, r: DWord;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,l,Sizeof(l));
  Move(pointer(longint(@InBlock)+4)^,r,Sizeof(r));
  l:= (l shr 24) or ((l shr 8) and $FF00) or ((l shl 8) and $FF0000) or (l shl 24);
  r:= (r shr 24) or ((r shr 8) and $FF00) or ((r shl 8) and $FF0000) or (r shl 24);
  t:= LRot32(KeyData[0]+r, KeyData[0+16]);
  l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[1] xor l, KeyData[1+16]);
  r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[2]-r, KeyData[2+16]);
  l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[3]+l, KeyData[3+16]);
  r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[4] xor r, KeyData[4+16]);
  l:= l xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[5]-l, KeyData[5+16]);
  r:= r xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[6]+r, KeyData[6+16]);
  l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[7] xor l, KeyData[7+16]);
  r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[8]-r, KeyData[8+16]);
  l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[9]+l, KeyData[9+16]);
  r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[10] xor r, KeyData[10+16]);
  l:= l xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[11]-l, KeyData[11+16]);
  r:= r xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  if Rounds> 12 then
  begin
    t:= LRot32(KeyData[12]+r, KeyData[12+16]);
    l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
      cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[13] xor l, KeyData[13+16]);
    r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
      cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[14]-r, KeyData[14+16]);
    l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
      cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[15]+l, KeyData[15+16]);
    r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
      cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  end;
  l:= (l shr 24) or ((l shr 8) and $FF00) or ((l shl 8) and $FF0000) or (l shl 24);
  r:= (r shr 24) or ((r shr 8) and $FF00) or ((r shl 8) and $FF0000) or (r shl 24);
  Move(r,OutBlock,Sizeof(r));
  Move(l,pointer(longint(@OutBlock)+4)^,Sizeof(l));
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_cast128.Decrypt(const InBlock; var OutBlock);
{$ELSE}
procedure Cast128DecryptECB;
{$ENDIF}
var
  t, l, r: DWord;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,r,Sizeof(l));
  Move(pointer(longint(@InBlock)+4)^,l,Sizeof(l));
  l:= (l shr 24) or ((l shr 8) and $FF00) or ((l shl 8) and $FF0000) or (l shl 24);
  r:= (r shr 24) or ((r shr 8) and $FF00) or ((r shl 8) and $FF0000) or (r shl 24);
  if Rounds> 12 then
  begin
    t:= LRot32(KeyData[15]+l, KeyData[15+16]);
    r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
      cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[14]-r, KeyData[14+16]);
    l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
      cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[13] xor l, KeyData[13+16]);
    r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
      cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[12]+r, KeyData[12+16]);
    l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
      cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  end;
  t:= LRot32(KeyData[11]-l, KeyData[11+16]);
  r:= r xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[10] xor r, KeyData[10+16]);
  l:= l xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[9]+l, KeyData[9+16]);
  r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[8]-r, KeyData[8+16]);
  l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[7] xor l, KeyData[7+16]);
  r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[6]+r, KeyData[6+16]);
  l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[5]-l, KeyData[5+16]);
  r:= r xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[4] xor r, KeyData[4+16]);
  l:= l xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[3]+l, KeyData[3+16]);
  r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[2]-r, KeyData[2+16]);
  l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[1] xor l, KeyData[1+16]);
  r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[0]+r, KeyData[0+16]);
  l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  l:= (l shr 24) or ((l shr 8) and $FF00) or ((l shl 8) and $FF0000) or (l shl 24);
  r:= (r shr 24) or ((r shr 8) and $FF00) or ((r shl 8) and $FF0000) or (r shl 24);
  Move(l,OutBlock,Sizeof(l));
  Move(r,pointer(longint(@OutBlock)+4)^,Sizeof(r));
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_cast128.Init(var Key; Size: longint; IVector: pointer);
{$ELSE}
procedure Cast128Init;
{$ENDIF}
var
  x, t, z: array[0..3] of DWord;
  i: longint;
begin
  {$IFDEF CFORM}
  if fInitialized then
    Burn;
  if (Size> fMaxKeySize) or (Size<= 0) or ((Size mod 8)<> 0) then
    raise Exception.Create(Format('Cast128: Invalid key size - %d',[Size]));
  {$ELSE}
  if (Size> 128) or (Size<= 0) or ((Size mod 8)<> 0) then
    Exit;
  with Data do begin
  {$ENDIF}
  Size:= Size div 8;
  if Size<= 10 then
    Rounds:= 12
  else
    Rounds:= 16;
  FillChar(x,Sizeof(x),0);
  Move(Key,x,Size);
  x[0]:= (x[0] shr 24) or ((x[0] shr 8) and $FF00) or ((x[0] shl 8) and $FF0000) or (x[0] shl 24);
  x[1]:= (x[1] shr 24) or ((x[1] shr 8) and $FF00) or ((x[1] shl 8) and $FF0000) or (x[1] shl 24);
  x[2]:= (x[2] shr 24) or ((x[2] shr 8) and $FF00) or ((x[2] shl 8) and $FF0000) or (x[2] shl 24);
  x[3]:= (x[3] shr 24) or ((x[3] shr 8) and $FF00) or ((x[3] shl 8) and $FF0000) or (x[3] shl 24);
  i:= 0;
  while i< 32 do
  begin
    case (i and 4) of
      0:
        begin
          z[0]:= x[0] xor cast_sbox5[(x[3] shr 16) and $FF] xor
           cast_sbox6[x[3] and $FF] xor cast_sbox7[x[3] shr 24] xor
           cast_sbox8[(x[3] shr 8) and $FF] xor cast_sbox7[x[2] shr 24];
          t[0]:= z[0];
          z[1]:= x[2] xor cast_sbox5[z[0] shr 24] xor
           cast_sbox6[(z[0] shr 8) and $FF] xor cast_sbox7[(z[0] shr 16) and $FF] xor
           cast_sbox8[z[0] and $FF] xor cast_sbox8[(x[2] shr 8) and $FF];
          t[1]:= z[1];
          z[2]:= x[3] xor cast_sbox5[z[1] and $FF] xor
           cast_sbox6[(z[1] shr 8) and $FF] xor cast_sbox7[(z[1] shr 16) and $FF] xor
           cast_sbox8[z[1] shr 24] xor cast_sbox5[(x[2] shr 16) and $FF];
          t[2]:= z[2];
          z[3]:= x[1] xor cast_sbox5[(z[2] shr 8) and $FF] xor
           cast_sbox6[(z[2] shr 16) and $FF] xor cast_sbox7[z[2] and $FF] xor
           cast_sbox8[z[2] shr 24] xor cast_sbox6[x[2] and $FF];
          t[3]:= z[3];
        end;
      4:
        begin
          x[0]:= z[2] xor cast_sbox5[(z[1] shr 16) and $FF] xor
           cast_sbox6[z[1] and $FF] xor cast_sbox7[z[1] shr 24] xor
           cast_sbox8[(z[1] shr 8) and $FF] xor cast_sbox7[z[0] shr 24];
          t[0]:= x[0];
          x[1]:= z[0] xor cast_sbox5[x[0] shr 24] xor
           cast_sbox6[(x[0] shr 8) and $FF] xor cast_sbox7[(x[0] shr 16) and $FF] xor
           cast_sbox8[x[0] and $FF] xor cast_sbox8[(z[0] shr 8) and $FF];
          t[1]:= x[1];
          x[2]:= z[1] xor cast_sbox5[x[1] and $FF] xor
           cast_sbox6[(x[1] shr 8) and $FF] xor cast_sbox7[(x[1] shr 16) and $FF] xor
           cast_sbox8[x[1] shr 24] xor cast_sbox5[(z[0] shr 16) and $FF];
          t[2]:= x[2];
          x[3]:= z[3] xor cast_sbox5[(x[2] shr 8) and $FF] xor
           cast_sbox6[(x[2] shr 16) and $FF] xor cast_sbox7[x[2] and $FF] xor
           cast_sbox8[x[2] shr 24] xor cast_sbox6[z[0] and $FF];
          t[3]:= x[3];
        end;
    end;
    case (i and 12) of
      0,12:
        begin
          KeyData[i+0]:= cast_sbox5[t[2] shr 24] xor cast_sbox6[(t[2] shr 16) and $FF] xor
           cast_sbox7[t[1] and $FF] xor cast_sbox8[(t[1] shr 8) and $FF];
          KeyData[i+1]:= cast_sbox5[(t[2] shr 8) and $FF] xor cast_sbox6[t[2] and $FF] xor
           cast_sbox7[(t[1] shr 16) and $FF] xor cast_sbox8[t[1] shr 24];
          KeyData[i+2]:= cast_sbox5[t[3] shr 24] xor cast_sbox6[(t[3] shr 16) and $FF] xor
           cast_sbox7[t[0] and $FF] xor cast_sbox8[(t[0] shr 8) and $FF];
          KeyData[i+3]:= cast_sbox5[(t[3] shr 8) and $FF] xor cast_sbox6[t[3] and $FF] xor
           cast_sbox7[(t[0] shr 16) and $FF] xor cast_sbox8[t[0] shr 24];
        end;
      4,8:
        begin
          KeyData[i+0]:= cast_sbox5[t[0] and $FF] xor cast_sbox6[(t[0] shr 8) and $FF] xor
           cast_sbox7[t[3] shr 24] xor cast_sbox8[(t[3] shr 16) and $FF];
          KeyData[i+1]:= cast_sbox5[(t[0] shr 16) and $FF] xor cast_sbox6[t[0] shr 24] xor
           cast_sbox7[(t[3] shr 8) and $FF] xor cast_sbox8[t[3] and $FF];
          KeyData[i+2]:= cast_sbox5[t[1] and $FF] xor cast_sbox6[(t[1] shr 8) and $FF] xor
           cast_sbox7[t[2] shr 24] xor cast_sbox8[(t[2] shr 16) and $FF];
          KeyData[i+3]:= cast_sbox5[(t[1] shr 16) and $FF] xor cast_sbox6[t[1] shr 24] xor
           cast_sbox7[(t[2] shr 8) and $FF] xor cast_sbox8[t[2] and $FF];
        end;
    end;
    case (i and 12) of
      0:
        begin
          KeyData[i+0]:= KeyData[i+0] xor cast_sbox5[(z[0] shr 8) and $FF];
          KeyData[i+1]:= KeyData[i+1] xor cast_sbox6[(z[1] shr 8) and $FF];
          KeyData[i+2]:= KeyData[i+2] xor cast_sbox7[(z[2] shr 16) and $FF];
          KeyData[i+3]:= KeyData[i+3] xor cast_sbox8[z[3] shr 24];
        end;
      4:
        begin
          KeyData[i+0]:= KeyData[i+0] xor cast_sbox5[x[2] shr 24];
          KeyData[i+1]:= KeyData[i+1] xor cast_sbox6[(x[3] shr 16) and $FF];
          KeyData[i+2]:= KeyData[i+2] xor cast_sbox7[x[0] and $FF];
          KeyData[i+3]:= KeyData[i+3] xor cast_sbox8[x[1] and $FF];
        end;
      8:
        begin
          KeyData[i+0]:= KeyData[i+0] xor cast_sbox5[(z[2] shr 16) and $FF];
          KeyData[i+1]:= KeyData[i+1] xor cast_sbox6[z[3] shr 24];
          KeyData[i+2]:= KeyData[i+2] xor cast_sbox7[(z[0] shr 8) and $FF];
          KeyData[i+3]:= KeyData[i+3] xor cast_sbox8[(z[1] shr 8) and $FF];
        end;
      12:
        begin
          KeyData[i+0]:= KeyData[i+0] xor cast_sbox5[x[0] and $FF];
          KeyData[i+1]:= KeyData[i+1] xor cast_sbox6[x[1] and $FF];
          KeyData[i+2]:= KeyData[i+2] xor cast_sbox7[x[2] shr 24];
          KeyData[i+3]:= KeyData[i+3] xor cast_sbox8[(x[3] shr 16) and $FF];
        end;
    end;
    if (i >= 16) then
    begin
      KeyData[i+0]:= KeyData[i+0] and 31;
      KeyData[i+1]:= KeyData[i+1] and 31;
      KeyData[i+2]:= KeyData[i+2] and 31;
      KeyData[i+3]:= KeyData[i+3] and 31;
    end;
    Inc(i,4);
  end;

  if IVector= nil then
  begin
    FillChar(IV,Sizeof(IV),$FF);
    {$IFDEF CFORM}Encrypt(IV,IV){$ELSE}Cast128EncryptECB(Data,IV,IV){$ENDIF};
    Move(IV,LB,Sizeof(LB));
  end
  else
  begin
    Move(IVector^,IV,Sizeof(IV));
    Move(IV,LB,Sizeof(IV));
  end;
  {$IFDEF CFORM}
  fInitialized:= true;
  {$ELSE}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_cast128.Burn;
{$ELSE}
procedure Cast128Burn;
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  FillChar(KeyData,Sizeof(KeyData),$FF);
  FillChar(IV,Sizeof(IV),$FF);
  FillChar(LB,Sizeof(LB),$FF);
  {$IFDEF CFORM}
  fInitialized:= false;
  {$ELSE}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_cast128.Reset;
{$ELSE}
procedure Cast128Reset;
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do{$ENDIF}
  Move(IV,LB,Sizeof(LB));
end;

{$IFDEF CFORM}
procedure TDCP_cast128.EncryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('Cast128: Not initialized');
  Encrypt(InBlock,OutBlock);
end;

procedure TDCP_cast128.DecryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('Cast128: Not initialized');
  Decrypt(InBlock,OutBlock);
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_cast128.EncryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure Cast128EncryptCBC;
{$ENDIF}
var
  TB: array[0..7] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Cast128: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 8) do
  begin
    XorBlock(pointer(longint(@InData)+((i-1)*8)),@LB,@TB,Sizeof(TB));
    {$IFDEF CFORM}Encrypt(TB,TB){$ELSE}Cast128EncryptECB(Data,TB,TB){$ENDIF};
    Move(TB,pointer(longint(@OutData)+((i-1)*8))^,Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 8)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}Cast128EncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 8))^,@pointer(longint(@OutData)+Size-(Size mod 8))^,Size mod 8);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_cast128.DecryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure Cast128DecryptCBC;
{$ENDIF}
var
  TB: array[0..7] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Cast128: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 8) do
  begin
    Move(pointer(longint(@InData)+((i-1)*8))^,TB,Sizeof(TB));
    {$IFDEF CFORM}
    Decrypt(pointer(longint(@InData)+((i-1)*8))^,pointer(longint(@OutData)+((i-1)*8))^);
    {$ELSE}
    Cast128DecryptECB(Data,pointer(longint(@InData)+((i-1)*8))^,pointer(longint(@OutData)+((i-1)*8))^);
    {$ENDIF}
    XorBlock(@LB,pointer(longint(@OutData)+((i-1)*8)),pointer(longint(@OutData)+((i-1)*8)),Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 8)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}Cast128EncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 8))^,@pointer(longint(@OutData)+Size-(Size mod 8))^,Size mod 8);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_cast128.EncryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure Cast128EncryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..7] of byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Cast128: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}Cast128EncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],7);
    LB[7]:= PByteArray(@OutData)^[i];
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_cast128.DecryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure Cast128DecryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..7] of byte;
  b: byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Cast128: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    b:= PByteArray(@InData)^[i];
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}Cast128EncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],7);
    LB[7]:= b;
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;


end.
