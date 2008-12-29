{******************************************************************************}
{** A binary compatible implementation of Mars ********************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{** Based on C source by Dr Brian Gladman (gladman@seven77.demon.co.uk) *******}
{******************************************************************************}
unit Mars;

interface
{$I DCPcrypt.Inc}
uses
  {$IFDEF CFORM}Classes, Sysutils, {$ENDIF}DCPcrypt;

{$IFDEF CFORM}
type
  TDCP_mars= class(TDCP_blockcipher)
  protected
    IV, LB: array[0..15] of byte;
    KeyData: array[0..39] of DWord;
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
  TMarsData= record
    IV, LB: array[0..15] of byte;
    KeyData: array[0..39] of DWord;
  end;

procedure MarsInit(var Data: TMarsData; var Key; Size: longint; IVector: pointer);
procedure MarsReset(var Data: TMarsData);
procedure MarsBurn(var Data:  TMarsData);
procedure MarsEncryptECB(var Data: TMarsData; const InBlock; var OutBlock);
procedure MarsDecryptECB(var Data: TMarsData; const InBlock; var OutBlock);
procedure MarsEncryptCBC(var Data: TMarsData; const InData; var OutData; Size: longint);
procedure MarsDecryptCBC(var Data: TMarsData; const InData; var OutData; Size: longint);
procedure MarsEncryptCFB(var Data: TMarsData; const InData; var OutData; Size: longint);
procedure MarsDecryptCFB(var Data: TMarsData; const InData; var OutData; Size: longint);
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
implementation

{$I Mars.Inc}

{$IFDEF CFORM}
constructor TDCP_mars.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAlgorithm:= 'Mars';
  fBlockSize:= 128;
  fMaxKeySize:= 1248;
  fID:= 13;
  Burn;
end;
{$ENDIF}

procedure gen_mask(var x, m: DWord);
var
  u: DWord;
begin
  u:= x and (x shr 1); u:= u and (u shr 2);
  u:= u and (u shr 4); u:= u and (u shr 1) and (u shr 2);
  m:= u;
  u:= (x xor $FFFFFFFF) and ((x xor $FFFFFFFF) shr 1); u:= u and (u shr 2);
  u:= u and (u shr 4); u:= u and (u shr 1) and (u shr 2);
  u:= u or m;
  m:= (u shl 1) or (u shl 2) or (u shl 3)
       or (u shl 4) or (u shl 5) or (u shl 6)
       or (u shl 7) or (u shl 8);
  m:= (m or u or (u shl 9)) and ((x xor $FFFFFFFF) xor (x shl 1)) and ((x xor $FFFFFFFF) xor (x shr 1));
  m:= m and $FFFFFFFC;
end;

{$IFDEF CFORM}
procedure TDCP_mars.Encrypt(const InBlock; var OutBlock);
{$ELSE}
procedure MarsEncryptECB;
{$ENDIF}
var
  l, m, r, t: DWord;
  blk: array[0..3] of DWord;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,Blk,Sizeof(Blk));
  blk[0]:= blk[0] + KeyData[0]; blk[1]:= blk[1] + KeyData[1];
  blk[2]:= blk[2] + KeyData[2]; blk[3]:= blk[3] + KeyData[3];
  blk[1]:= blk[1] xor s_box[  blk[0]         and $FF];
  blk[1]:= blk[1]  +  s_box[((blk[0] shr  8) and $FF) + 256];
  blk[2]:= blk[2]  +  s_box[ (blk[0] shr 16) and $FF];
  blk[3]:= blk[3] xor s_box[((blk[0] shr 24) and $FF) + 256];
  blk[0]:= RRot32(blk[0], 24); blk[0]:= blk[0] + blk[3];
  blk[2]:= blk[2] xor s_box[  blk[1]         and $FF];
  blk[2]:= blk[2]  +  s_box[((blk[1] shr  8) and $FF) + 256];
  blk[3]:= blk[3]  +  s_box[ (blk[1] shr 16) and $FF];
  blk[0]:= blk[0] xor s_box[((blk[1] shr 24) and $FF) + 256];
  blk[1]:= RRot32(blk[1], 24); blk[1]:= blk[1] + blk[2];
  blk[3]:= blk[3] xor s_box[  blk[2]         and $FF];
  blk[3]:= blk[3]  +  s_box[((blk[2] shr  8) and $FF) + 256];
  blk[0]:= blk[0]  +  s_box[ (blk[2] shr 16) and $FF];
  blk[1]:= blk[1] xor s_box[((blk[2] shr 24) and $FF) + 256];
  blk[2]:= RRot32(blk[2], 24);
  blk[0]:= blk[0] xor s_box[  blk[3]         and $FF];
  blk[0]:= blk[0]  +  s_box[((blk[3] shr  8) and $FF) + 256];
  blk[1]:= blk[1]  +  s_box[ (blk[3] shr 16) and $FF];
  blk[2]:= blk[2] xor s_box[((blk[3] shr 24) and $FF) + 256];
  blk[3]:= RRot32(blk[3], 24);
  blk[1]:= blk[1] xor s_box[  blk[0]         and $FF];
  blk[1]:= blk[1]  +  s_box[((blk[0] shr  8) and $FF) + 256];
  blk[2]:= blk[2]  +  s_box[ (blk[0] shr 16) and $FF];
  blk[3]:= blk[3] xor s_box[((blk[0] shr 24) and $FF) + 256];
  blk[0]:= RRot32(blk[0], 24); blk[0]:= blk[0] + blk[3];
  blk[2]:= blk[2] xor s_box[  blk[1]         and $FF];
  blk[2]:= blk[2]  +  s_box[((blk[1] shr  8) and $FF) + 256];
  blk[3]:= blk[3]  +  s_box[ (blk[1] shr 16) and $FF];
  blk[0]:= blk[0] xor s_box[((blk[1] shr 24) and $FF) + 256];
  blk[1]:= RRot32(blk[1], 24); blk[1]:= blk[1] + blk[2];
  blk[3]:= blk[3] xor s_box[  blk[2]         and $FF];
  blk[3]:= blk[3]  +  s_box[((blk[2] shr  8) and $FF) + 256];
  blk[0]:= blk[0]  +  s_box[ (blk[2] shr 16) and $FF];
  blk[1]:= blk[1] xor s_box[((blk[2] shr 24) and $FF) + 256];
  blk[2]:= RRot32(blk[2], 24);
  blk[0]:= blk[0] xor s_box[  blk[3]         and $FF];
  blk[0]:= blk[0]  +  s_box[((blk[3] shr  8) and $FF) + 256];
  blk[1]:= blk[1]  +  s_box[ (blk[3] shr 16) and $FF];
  blk[2]:= blk[2] xor s_box[((blk[3] shr 24) and $FF) + 256];
  blk[3]:= RRot32(blk[3], 24);
  m:= blk[0] + KeyData[4];
  r:= LRot32(blk[0],13) * KeyData[5];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= LRot32(blk[0],13);
  blk[1]:= blk[1] + l;
  blk[2]:= blk[2] + m;
  blk[3]:= blk[3] xor r;
  m:= blk[1] + KeyData[6];
  r:= LRot32(blk[1],13) * KeyData[7];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= LRot32(blk[1],13);
  blk[2]:= blk[2] + l;
  blk[3]:= blk[3] + m;
  blk[0]:= blk[0] xor r;
  m:= blk[2] + KeyData[8];
  r:= LRot32(blk[2],13) * KeyData[9];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= LRot32(blk[2],13);
  blk[3]:= blk[3] + l;
  blk[0]:= blk[0] + m;
  blk[1]:= blk[1] xor r;
  m:= blk[3] + KeyData[10];
  r:= LRot32(blk[3],13) * KeyData[11];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= LRot32(blk[3],13);
  blk[0]:= blk[0] + l;
  blk[1]:= blk[1] + m;
  blk[2]:= blk[2] xor r;
  m:= blk[0] + KeyData[12];
  r:= LRot32(blk[0],13) * KeyData[13];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= LRot32(blk[0],13);
  blk[1]:= blk[1] + l;
  blk[2]:= blk[2] + m;
  blk[3]:= blk[3] xor r;
  m:= blk[1] + KeyData[14];
  r:= LRot32(blk[1],13) * KeyData[15];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= LRot32(blk[1],13);
  blk[2]:= blk[2] + l;
  blk[3]:= blk[3] + m;
  blk[0]:= blk[0] xor r;
  m:= blk[2] + KeyData[16];
  r:= LRot32(blk[2],13) * KeyData[17];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= LRot32(blk[2],13);
  blk[3]:= blk[3] + l;
  blk[0]:= blk[0] + m;
  blk[1]:= blk[1] xor r;
  m:= blk[3] + KeyData[18];
  r:= LRot32(blk[3],13) * KeyData[19];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= LRot32(blk[3],13);
  blk[0]:= blk[0] + l;
  blk[1]:= blk[1] + m;
  blk[2]:= blk[2] xor r;
  m:= blk[0] + KeyData[20];
  r:= LRot32(blk[0],13) * KeyData[21];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= LRot32(blk[0],13);
  blk[3]:= blk[3] + l;
  blk[2]:= blk[2] + m;
  blk[1]:= blk[1] xor r;
  m:= blk[1] + KeyData[22];
  r:= LRot32(blk[1],13) * KeyData[23];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= LRot32(blk[1],13);
  blk[0]:= blk[0] + l;
  blk[3]:= blk[3] + m;
  blk[2]:= blk[2] xor r;
  m:= blk[2] + KeyData[24];
  r:= LRot32(blk[2],13) * KeyData[25];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= LRot32(blk[2],13);
  blk[1]:= blk[1] + l;
  blk[0]:= blk[0] + m;
  blk[3]:= blk[3] xor r;
  m:= blk[3] + KeyData[26];
  r:= LRot32(blk[3],13) * KeyData[27];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= LRot32(blk[3],13);
  blk[2]:= blk[2] + l;
  blk[1]:= blk[1] + m;
  blk[0]:= blk[0] xor r;
  m:= blk[0] + KeyData[28];
  r:= LRot32(blk[0],13) * KeyData[29];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= LRot32(blk[0],13);
  blk[3]:= blk[3] + l;
  blk[2]:= blk[2] + m;
  blk[1]:= blk[1] xor r;
  m:= blk[1] + KeyData[30];
  r:= LRot32(blk[1],13) * KeyData[31];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= LRot32(blk[1],13);
  blk[0]:= blk[0] + l;
  blk[3]:= blk[3] + m;
  blk[2]:= blk[2] xor r;
  m:= blk[2] + KeyData[32];
  r:= LRot32(blk[2],13) * KeyData[33];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= LRot32(blk[2],13);
  blk[1]:= blk[1] + l;
  blk[0]:= blk[0] + m;
  blk[3]:= blk[3] xor r;
  m:= blk[3] + KeyData[34];
  r:= LRot32(blk[3],13) * KeyData[35];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= LRot32(blk[3],13);
  blk[2]:= blk[2] + l;
  blk[1]:= blk[1] + m;
  blk[0]:= blk[0] xor r;
  blk[1]:= blk[1] xor s_box[ (blk[0]         and $FF) + 256];
  blk[2]:= blk[2]  -  s_box[ (blk[0] shr 24) and $FF];
  blk[3]:= blk[3]  -  s_box[((blk[0] shr 16) and $FF) + 256];
  blk[3]:= blk[3] xor s_box[ (blk[0] shr  8) and $FF];
  blk[0]:= LRot32(blk[0], 24);
  blk[2]:= blk[2] xor s_box[ (blk[1]         and $FF) + 256];
  blk[3]:= blk[3]  -  s_box[ (blk[1] shr 24) and $FF];
  blk[0]:= blk[0]  -  s_box[((blk[1] shr 16) and $FF) + 256];
  blk[0]:= blk[0] xor s_box[ (blk[1] shr  8) and $FF];
  blk[1]:= LRot32(blk[1], 24); blk[2]:= blk[2] - blk[1];
  blk[3]:= blk[3] xor s_box[ (blk[2]         and $FF) + 256];
  blk[0]:= blk[0]  -  s_box[ (blk[2] shr 24) and $FF];
  blk[1]:= blk[1]  -  s_box[((blk[2] shr 16) and $FF) + 256];
  blk[1]:= blk[1] xor s_box[ (blk[2] shr  8) and $FF];
  blk[2]:= LRot32(blk[2], 24); blk[3]:= blk[3] - blk[0];
  blk[0]:= blk[0] xor s_box[ (blk[3]         and $FF) + 256];
  blk[1]:= blk[1]  -  s_box[ (blk[3] shr 24) and $FF];
  blk[2]:= blk[2]  -  s_box[((blk[3] shr 16) and $FF) + 256];
  blk[2]:= blk[2] xor s_box[ (blk[3] shr  8) and $FF];
  blk[3]:= LRot32(blk[3], 24);
  blk[1]:= blk[1] xor s_box[ (blk[0]         and $FF) + 256];
  blk[2]:= blk[2]  -  s_box[ (blk[0] shr 24) and $FF];
  blk[3]:= blk[3]  -  s_box[((blk[0] shr 16) and $FF) + 256];
  blk[3]:= blk[3] xor s_box[ (blk[0] shr  8) and $FF];
  blk[0]:= LRot32(blk[0], 24);
  blk[2]:= blk[2] xor s_box[ (blk[1]         and $FF) + 256];
  blk[3]:= blk[3]  -  s_box[ (blk[1] shr 24) and $FF];
  blk[0]:= blk[0]  -  s_box[((blk[1] shr 16) and $FF) + 256];
  blk[0]:= blk[0] xor s_box[ (blk[1] shr  8) and $FF];
  blk[1]:= LRot32(blk[1], 24); blk[2]:= blk[2] - blk[1];
  blk[3]:= blk[3] xor s_box[ (blk[2]         and $FF) + 256];
  blk[0]:= blk[0]  -  s_box[ (blk[2] shr 24) and $FF];
  blk[1]:= blk[1]  -  s_box[((blk[2] shr 16) and $FF) + 256];
  blk[1]:= blk[1] xor s_box[ (blk[2] shr  8) and $FF];
  blk[2]:= LRot32(blk[2], 24); blk[3]:= blk[3] - blk[0];
  blk[0]:= blk[0] xor s_box[ (blk[3]         and $FF) + 256];
  blk[1]:= blk[1]  -  s_box[ (blk[3] shr 24) and $FF];
  blk[2]:= blk[2]  -  s_box[((blk[3] shr 16) and $FF) + 256];
  blk[2]:= blk[2] xor s_box[ (blk[3] shr  8) and $FF];
  blk[3]:= LRot32(blk[3], 24);
  blk[0]:= blk[0] - KeyData[36]; blk[1]:= blk[1] - KeyData[37];
  blk[2]:= blk[2] - KeyData[38]; blk[3]:= blk[3] - KeyData[39];
  Move(Blk,OutBlock,Sizeof(Blk));
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_mars.Decrypt(const InBlock; var OutBlock);
{$ELSE}
procedure MarsDecryptECB;
{$ENDIF}
var
  l, m, r, t: DWord;
  blk: array[0..3] of DWord;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,Blk,Sizeof(Blk));
  blk[0]:= blk[0] + KeyData[36]; blk[1]:= blk[1] + KeyData[37];
  blk[2]:= blk[2] + KeyData[38]; blk[3]:= blk[3] + KeyData[39];
  blk[3]:= RRot32(blk[3], 24);
  blk[2]:= blk[2] xor s_box[ (blk[3] shr  8) and $FF];
  blk[2]:= blk[2]  +  s_box[((blk[3] shr 16) and $FF) + 256];
  blk[1]:= blk[1]  +  s_box[ (blk[3] shr 24) and $FF];
  blk[0]:= blk[0] xor s_box[ (blk[3]         and $FF) + 256];
  blk[3]:= blk[3] + blk[0]; blk[2]:= RRot32(blk[2], 24);
  blk[1]:= blk[1] xor s_box[ (blk[2] shr  8) and $FF];
  blk[1]:= blk[1]  +  s_box[((blk[2] shr 16) and $FF) + 256];
  blk[0]:= blk[0]  +  s_box[ (blk[2] shr 24) and $FF];
  blk[3]:= blk[3] xor s_box[ (blk[2]         and $FF) + 256];
  blk[2]:= blk[2] + blk[1]; blk[1]:= RRot32(blk[1], 24);
  blk[0]:= blk[0] xor s_box[ (blk[1] shr  8) and $FF];
  blk[0]:= blk[0]  +  s_box[((blk[1] shr 16) and $FF) + 256];
  blk[3]:= blk[3]  +  s_box[ (blk[1] shr 24) and $FF];
  blk[2]:= blk[2] xor s_box[ (blk[1]         and $FF) + 256];
  blk[0]:= RRot32(blk[0], 24);
  blk[3]:= blk[3] xor s_box[ (blk[0] shr  8) and $FF];
  blk[3]:= blk[3]  +  s_box[((blk[0] shr 16) and $FF) + 256];
  blk[2]:= blk[2]  +  s_box[ (blk[0] shr 24) and $FF];
  blk[1]:= blk[1] xor s_box[ (blk[0]         and $FF) + 256];
  blk[3]:= RRot32(blk[3], 24);
  blk[2]:= blk[2] xor s_box[ (blk[3] shr  8) and $FF];
  blk[2]:= blk[2]  +  s_box[((blk[3] shr 16) and $FF) + 256];
  blk[1]:= blk[1]  +  s_box[ (blk[3] shr 24) and $FF];
  blk[0]:= blk[0] xor s_box[ (blk[3]         and $FF) + 256];
  blk[3]:= blk[3] + blk[0]; blk[2]:= RRot32(blk[2], 24);
  blk[1]:= blk[1] xor s_box[ (blk[2] shr  8) and $FF];
  blk[1]:= blk[1]  +  s_box[((blk[2] shr 16) and $FF) + 256];
  blk[0]:= blk[0]  +  s_box[ (blk[2] shr 24) and $FF];
  blk[3]:= blk[3] xor s_box[ (blk[2]         and $FF) + 256];
  blk[2]:= blk[2] + blk[1]; blk[1]:= RRot32(blk[1], 24);
  blk[0]:= blk[0] xor s_box[ (blk[1] shr  8) and $FF];
  blk[0]:= blk[0]  +  s_box[((blk[1] shr 16) and $FF) + 256];
  blk[3]:= blk[3]  +  s_box[ (blk[1] shr 24) and $FF];
  blk[2]:= blk[2] xor s_box[ (blk[1]         and $FF) + 256];
  blk[0]:= RRot32(blk[0], 24);
  blk[3]:= blk[3] xor s_box[ (blk[0] shr  8) and $FF];
  blk[3]:= blk[3]  +  s_box[((blk[0] shr 16) and $FF) + 256];
  blk[2]:= blk[2]  +  s_box[ (blk[0] shr 24) and $FF];
  blk[1]:= blk[1] xor s_box[ (blk[0]         and $FF) + 256];
  blk[3]:= RRot32(blk[3],13);
  m:= blk[3] + KeyData[34];
  r:= LRot32(blk[3],13) * KeyData[35];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= blk[2] - l;
  blk[1]:= blk[1] - m;
  blk[0]:= blk[0] xor r;
  blk[2]:= RRot32(blk[2],13);
  m:= blk[2] + KeyData[32];
  r:= LRot32(blk[2],13) * KeyData[33];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= blk[1] - l;
  blk[0]:= blk[0] - m;
  blk[3]:= blk[3] xor r;
  blk[1]:= RRot32(blk[1],13);
  m:= blk[1] + KeyData[30];
  r:= LRot32(blk[1],13) * KeyData[31];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= blk[0] - l;
  blk[3]:= blk[3] - m;
  blk[2]:= blk[2] xor r;
  blk[0]:= RRot32(blk[0],13);
  m:= blk[0] + KeyData[28];
  r:= LRot32(blk[0],13) * KeyData[29];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= blk[3] - l;
  blk[2]:= blk[2] - m;
  blk[1]:= blk[1] xor r;
  blk[3]:= RRot32(blk[3],13);
  m:= blk[3] + KeyData[26];
  r:= LRot32(blk[3],13) * KeyData[27];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= blk[2] - l;
  blk[1]:= blk[1] - m;
  blk[0]:= blk[0] xor r;
  blk[2]:= RRot32(blk[2],13);
  m:= blk[2] + KeyData[24];
  r:= LRot32(blk[2],13) * KeyData[25];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= blk[1] - l;
  blk[0]:= blk[0] - m;
  blk[3]:= blk[3] xor r;
  blk[1]:= RRot32(blk[1],13);
  m:= blk[1] + KeyData[22];
  r:= LRot32(blk[1],13) * KeyData[23];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= blk[0] - l;
  blk[3]:= blk[3] - m;
  blk[2]:= blk[2] xor r;
  blk[0]:= RRot32(blk[0],13);
  m:= blk[0] + KeyData[20];
  r:= LRot32(blk[0],13) * KeyData[21];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= blk[3] - l;
  blk[2]:= blk[2] - m;
  blk[1]:= blk[1] xor r;
  blk[3]:= RRot32(blk[3],13);
  m:= blk[3] + KeyData[18];
  r:= LRot32(blk[3],13) * KeyData[19];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= blk[0] - l;
  blk[1]:= blk[1] - m;
  blk[2]:= blk[2] xor r;
  blk[2]:= RRot32(blk[2],13);
  m:= blk[2] + KeyData[16];
  r:= LRot32(blk[2],13) * KeyData[17];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= blk[3] - l;
  blk[0]:= blk[0] - m;
  blk[1]:= blk[1] xor r;
  blk[1]:= RRot32(blk[1],13);
  m:= blk[1] + KeyData[14];
  r:= LRot32(blk[1],13) * KeyData[15];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= blk[2] - l;
  blk[3]:= blk[3] - m;
  blk[0]:= blk[0] xor r;
  blk[0]:= RRot32(blk[0],13);
  m:= blk[0] + KeyData[12];
  r:= LRot32(blk[0],13) * KeyData[13];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= blk[1] - l;
  blk[2]:= blk[2] - m;
  blk[3]:= blk[3] xor r;
  blk[3]:= RRot32(blk[3],13);
  m:= blk[3] + KeyData[10];
  r:= LRot32(blk[3],13) * KeyData[11];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= blk[0] - l;
  blk[1]:= blk[1] - m;
  blk[2]:= blk[2] xor r;
  blk[2]:= RRot32(blk[2],13);
  m:= blk[2] + KeyData[8];
  r:= LRot32(blk[2],13) * KeyData[9];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= blk[3] - l;
  blk[0]:= blk[0] - m;
  blk[1]:= blk[1] xor r;
  blk[1]:= RRot32(blk[1],13);
  m:= blk[1] + KeyData[6];
  r:= LRot32(blk[1],13) * KeyData[7];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= blk[2] - l;
  blk[3]:= blk[3] - m;
  blk[0]:= blk[0] xor r;
  blk[0]:= RRot32(blk[0],13);
  m:= blk[0] + KeyData[4];
  r:= LRot32(blk[0],13) * KeyData[5];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= blk[1] - l;
  blk[2]:= blk[2] - m;
  blk[3]:= blk[3] xor r;
  blk[3]:= LRot32(blk[3], 24);
  blk[2]:= blk[2] xor s_box[((blk[3] shr 24) and $FF) + 256];
  blk[1]:= blk[1]  -  s_box[ (blk[3] shr 16) and $FF];
  blk[0]:= blk[0]  -  s_box[((blk[3] shr  8) and $FF) + 256];
  blk[0]:= blk[0] xor s_box[  blk[3]         and $FF];
  blk[2]:= LRot32(blk[2], 24);
  blk[1]:= blk[1] xor s_box[((blk[2] shr 24) and $FF) + 256];
  blk[0]:= blk[0]  -  s_box[ (blk[2] shr 16) and $FF];
  blk[3]:= blk[3]  -  s_box[((blk[2] shr  8) and $FF) + 256];
  blk[3]:= blk[3] xor s_box[  blk[2]         and $FF];
  blk[1]:= blk[1] - blk[2]; blk[1]:= LRot32(blk[1], 24);
  blk[0]:= blk[0] xor s_box[((blk[1] shr 24) and $FF) + 256];
  blk[3]:= blk[3]  -  s_box[ (blk[1] shr 16) and $FF];
  blk[2]:= blk[2]  -  s_box[((blk[1] shr  8) and $FF) + 256];
  blk[2]:= blk[2] xor s_box[  blk[1]         and $FF];
  blk[0]:= blk[0] - blk[3]; blk[0]:= LRot32(blk[0], 24);
  blk[3]:= blk[3] xor s_box[((blk[0] shr 24) and $FF) + 256];
  blk[2]:= blk[2]  -  s_box[ (blk[0] shr 16) and $FF];
  blk[1]:= blk[1]  -  s_box[((blk[0] shr  8) and $FF) + 256];
  blk[1]:= blk[1] xor s_box[  blk[0]         and $FF];
  blk[3]:= LRot32(blk[3], 24);
  blk[2]:= blk[2] xor s_box[((blk[3] shr 24) and $FF) + 256];
  blk[1]:= blk[1]  -  s_box[ (blk[3] shr 16) and $FF];
  blk[0]:= blk[0]  -  s_box[((blk[3] shr  8) and $FF) + 256];
  blk[0]:= blk[0] xor s_box[  blk[3]         and $FF];
  blk[2]:= LRot32(blk[2], 24);
  blk[1]:= blk[1] xor s_box[((blk[2] shr 24) and $FF) + 256];
  blk[0]:= blk[0]  -  s_box[ (blk[2] shr 16) and $FF];
  blk[3]:= blk[3]  -  s_box[((blk[2] shr  8) and $FF) + 256];
  blk[3]:= blk[3] xor s_box[  blk[2]         and $FF];
  blk[1]:= blk[1] - blk[2]; blk[1]:= LRot32(blk[1], 24);
  blk[0]:= blk[0] xor s_box[((blk[1] shr 24) and $FF) + 256];
  blk[3]:= blk[3]  -  s_box[ (blk[1] shr 16) and $FF];
  blk[2]:= blk[2]  -  s_box[((blk[1] shr  8) and $FF) + 256];
  blk[2]:= blk[2] xor s_box[  blk[1]         and $FF];
  blk[0]:= blk[0] - blk[3]; blk[0]:= LRot32(blk[0], 24);
  blk[3]:= blk[3] xor s_box[((blk[0] shr 24) and $FF) + 256];
  blk[2]:= blk[2]  -  s_box[ (blk[0] shr 16) and $FF];
  blk[1]:= blk[1]  -  s_box[((blk[0] shr  8) and $FF) + 256];
  blk[1]:= blk[1] xor s_box[  blk[0]         and $FF];
  blk[0]:= blk[0] - KeyData[0]; blk[1]:= blk[1] - KeyData[1];
  blk[2]:= blk[2] - KeyData[2]; blk[3]:= blk[3] - KeyData[3];
  Move(Blk,OutBlock,Sizeof(Blk));
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_mars.Init(var Key; Size: longint; IVector: pointer);
{$ELSE}
procedure MarsInit;
{$ENDIF}
var
  i, j, m, u, w: DWord;
  t: array[-7..39] of DWord;
  KeyB: array[0..39] of DWord;
begin
  {$IFDEF CFORM}
  if fInitialized then
    Burn;
  if (Size> fMaxKeySize) or (Size<= 0) or ((Size mod 8)<> 0) then
    raise Exception.Create(Format('Mars: Invalid key size - %d',[Size]));
  {$ELSE}
  if (Size> 1248) or (Size<= 0) or ((Size mod 8)<> 0) then
    Exit;
  with Data do begin
  {$ENDIF}

  Size:= Size div 8;

  FillChar(KeyB,Sizeof(KeyB),0);
  Move(Key,KeyB,Size);
  Size:= Size div 4;
  Move(vk,t,Sizeof(vk));
  for i:= 0 to 38 do
  begin
    u:= t[i-7] xor t[i-2];
    t[i]:= LRot32(u,3) xor KeyB[i mod DWord(Size)] xor i;
  end;
  t[39]:= Size;
  for j:= 0 to 6 do
  begin
    for i:= 1 to 39 do
    begin
      u:= t[i] + s_box[t[i-1] and $1FF];
      t[i]:= LRot32(u,9);
    end;
    u:= t[0] + s_box[t[39] and $1FF];
    t[0]:= LRot32(u,9);
  end;
  for i:= 0 to 39 do
    KeyData[(7*i) mod 40]:= t[i];
  i:= 5;
  repeat
    u:= s_box[265+(KeyData[i] and $3)];
    j:= KeyData[i+3] and $1f;
    w:= KeyData[i] or $3;
    gen_mask(w,m);
    KeyData[i]:= w xor (LRot32(u,j) and m);
    Inc(i,2);
  until i>= 37;

  if IVector= nil then
  begin
    FillChar(IV,Sizeof(IV),$FF);
    {$IFDEF CFORM}Encrypt(IV,IV){$ELSE}MarsEncryptECB(Data,IV,IV){$ENDIF};
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
procedure TDCP_mars.Burn;
{$ELSE}
procedure MarsBurn;
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
procedure TDCP_mars.Reset;
{$ELSE}
procedure MarsReset;
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do{$ENDIF}
  Move(IV,LB,Sizeof(LB));
end;

{$IFDEF CFORM}
procedure TDCP_mars.EncryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('Mars: Not initialized');
  Encrypt(InBlock,OutBlock);
end;

procedure TDCP_mars.DecryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('Mars: Not initialized');
  Decrypt(InBlock,OutBlock);
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_mars.EncryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure MarsEncryptCBC;
{$ENDIF}
var
  TB: array[0..15] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Mars: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 16) do
  begin
    XorBlock(pointer(longint(@InData)+((i-1)*16)),@LB,@TB,Sizeof(TB));
    {$IFDEF CFORM}Encrypt(TB,TB){$ELSE}MarsEncryptECB(Data,TB,TB){$ENDIF};
    Move(TB,pointer(longint(@OutData)+((i-1)*16))^,Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 16)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}MarsEncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 16))^,@pointer(longint(@OutData)+Size-(Size mod 16))^,Size mod 16);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_mars.DecryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure MarsDecryptCBC;
{$ENDIF}
var
  TB: array[0..15] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Mars: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 16) do
  begin
    Move(pointer(longint(@InData)+((i-1)*16))^,TB,Sizeof(TB));
    {$IFDEF CFORM}
    Decrypt(pointer(longint(@InData)+((i-1)*16))^,pointer(longint(@OutData)+((i-1)*16))^);
    {$ELSE}
    MarsDecryptECB(Data,pointer(longint(@InData)+((i-1)*16))^,pointer(longint(@OutData)+((i-1)*16))^);
    {$ENDIF}
    XorBlock(@LB,pointer(longint(@OutData)+((i-1)*16)),pointer(longint(@OutData)+((i-1)*16)),Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 16)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}MarsEncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 16))^,@pointer(longint(@OutData)+Size-(Size mod 16))^,Size mod 16);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_mars.EncryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure MarsEncryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..15] of byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Mars: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}MarsEncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],15);
    LB[15]:= PByteArray(@OutData)^[i];
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_mars.DecryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure MarsDecryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..15] of byte;
  b: byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Mars: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    b:= PByteArray(@InData)^[i];
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}MarsEncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],15);
    LB[15]:= b;
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

end.
