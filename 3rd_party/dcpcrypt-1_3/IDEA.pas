{******************************************************************************}
{** A binary compatible implementation of IDEA ********************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}

(* -------------------------------------------------------------------------------
  + Changes by Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [dpv]

   >> Changes to original source code available in KeyNote NF project.
   >> Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
      in https://github.com/dpradov/keynote-nf
 ---------------
NOTE:
*1 IDEA (International Data Encryption Algorithm) (https://en.wikipedia.org/wiki/International_Data_Encryption_Algorithm) :
  The cipher was designed under a research contract with the Hasler Foundation, which became part of Ascom-Tech AG. The cipher was patented in a number
  of countries but was freely available for non-commercial use. The name "IDEA" is also a trademark. The last patents expired in 2012, and IDEA is now
  patent-free and thus completely free for all uses.[2]
  (https://worldwide.espacenet.com/patent/search/family/004216333/publication/EP0482154A1?q=pn%3DEP0482154)

*2 At least, from version 1.6.5, the implementation of IDEA in KeyNote was using a MaxKeySize=256 and a BlockSize=128
   This values were shared with Blowfish implementation.
 ------------------------------------------------------------------------------ *)


unit IDEA;

interface
{$I DCPcrypt.Inc}
uses
   {$IFDEF CFORM}
   System.Classes,
   System.SysUtils,
   {$ENDIF}
   DCPcrypt;

{$IFDEF CFORM}
type
  TDCP_idea= class(TDCP_blockcipher)
  protected
    IV, LB: array[0..7] of byte;
    EK: array[0..51] of word;
    DK: array[0..51] of word;
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
  TIDEAData= record
    IV, LB: array[0..7] of byte;
    EK: array[0..51] of word;
    DK: array[0..51] of word;
    Rounds: longint;
  end;

procedure IDEAInit(var Data: TIDEAData; var Key; Size: longint; IVector: pointer);
procedure IDEAReset(var Data: TIDEAData);
procedure IDEABurn(var Data:  TIDEAData);
procedure IDEAEncryptECB(var Data: TIDEAData; const InBlock; var OutBlock);
procedure IDEADecryptECB(var Data: TIDEAData; const InBlock; var OutBlock);
procedure IDEAEncryptCBC(var Data: TIDEAData; const InData; var OutData; Size: longint);
procedure IDEADecryptCBC(var Data: TIDEAData; const InData; var OutData; Size: longint);
procedure IDEAEncryptCFB(var Data: TIDEAData; const InData; var OutData; Size: longint);
procedure IDEADecryptCFB(var Data: TIDEAData; const InData; var OutData; Size: longint);
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
implementation

{$IFDEF CFORM}
constructor TDCP_idea.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAlgorithm:= 'IDEA';
  fBlockSize:= 128;  // 64;        // [dpv] See note *2 at the beginning
  fMaxKeySize:= 256; // 128;       // [dpv]
  fID:= 12;
  Burn;
end;
{$ENDIF}

procedure Mul(var x: word; const y: word);
var
  p: DWord;
  t16: word;
begin
  p:= DWord(x)*y;
  if p= 0 then
    x:= 1 - x - y
  else
  begin
    x:= p shr 16;
    t16:= p and $FFFF;
    x:= t16 - x;
    if (t16 < x) then
      Inc(x);
  end;
end;

function MulInv(x: word): word;
var
  t0, t1, q, y: word;
begin
  if x<= 1 then
  begin
    Result:= x;
    Exit;
  end;
  t1:= DWord($10001) div x;
  y:= DWord($10001) mod x;
  if y= 1 then
  begin
    Result:= (1 - t1) and $FFFF;
    Exit;
  end;
  t0:= 1;
  repeat
    q:= x div y;
    x:= x mod y;
    t0:= t0 + (q*t1);
    if x= 1 then
    begin
      Result:= t0;
      Exit;
    end;
    q:= y div x;
    y:= y mod x;
    t1:= t1 + (q*t0);
  until y= 1;
  Result:= (1-t1) and $FFFF;
end;

{$IFDEF CFORM}
procedure TDCP_idea.Encrypt(const InBlock; var OutBlock);
{$ELSE}
procedure IDEAEncryptECB;
{$ENDIF}
var
  x: array[1..4] of word;
  s3, s2: word;
  i: longint;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,x,Sizeof(x));
  for i:= 1 to 4 do
    x[i]:= (x[i] shl 8) or (x[i] shr 8);
  for i:= 0 to 7 do
  begin
    Mul(x[1],EK[(i*6)+0]);
    Inc(x[2],EK[(i*6)+1]);
    Inc(x[3],EK[(i*6)+2]);
    Mul(x[4],EK[(i*6)+3]);
    s3:= x[3];
    x[3]:= x[3] xor x[1];
    Mul(x[3],EK[(i*6)+4]);
    s2:= x[2];
    x[2]:= x[2] xor x[4];
    Inc(x[2],x[3]);
    Mul(x[2],EK[(i*6)+5]);
    Inc(x[3],x[2]);
    x[1]:= x[1] xor x[2];
    x[4]:= x[4] xor x[3];
    x[2]:= x[2] xor s3;
    x[3]:= x[3] xor s2;
  end;
  Mul(x[1],EK[48]);
  Inc(x[3],EK[49]);
  Inc(x[2],EK[50]);
  Mul(x[4],EK[51]);
  x[1]:= (x[1] shl 8) or (x[1] shr 8);
  s2:= (x[3] shl 8) or (x[3] shr 8);
  x[3]:= (x[2] shl 8) or (x[2] shr 8);
  x[4]:= (x[4] shl 8) or (x[4] shr 8);
  x[2]:= s2;
  Move(x,OutBlock,Sizeof(x));
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_idea.Decrypt(const InBlock; var OutBlock);
{$ELSE}
procedure IDEADecryptECB;
{$ENDIF}
var
  x: array[1..4] of word;
  s3, s2: word;
  i: longint;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,x,Sizeof(x));
  for i:= 1 to 4 do
    x[i]:= (x[i] shl 8) or (x[i] shr 8);
  for i:= 0 to 7 do
  begin
    Mul(x[1],DK[(i*6)+0]);
    Inc(x[2],DK[(i*6)+1]);
    Inc(x[3],DK[(i*6)+2]);
    Mul(x[4],DK[(i*6)+3]);
    s3:= x[3];
    x[3]:= x[3] xor x[1];
    Mul(x[3],DK[(i*6)+4]);
    s2:= x[2];
    x[2]:= x[2] xor x[4];
    Inc(x[2],x[3]);
    Mul(x[2],DK[(i*6)+5]);
    Inc(x[3],x[2]);
    x[1]:= x[1] xor x[2];
    x[4]:= x[4] xor x[3];
    x[2]:= x[2] xor s3;
    x[3]:= x[3] xor s2;
  end;
  Mul(x[1],DK[48]);
  Inc(x[3],DK[49]);
  Inc(x[2],DK[50]);
  Mul(x[4],DK[51]);
  x[1]:= (x[1] shl 8) or (x[1] shr 8);
  s2:= (x[3] shl 8) or (x[3] shr 8);
  x[3]:= (x[2] shl 8) or (x[2] shr 8);
  x[4]:= (x[4] shl 8) or (x[4] shr 8);
  x[2]:= s2;
  Move(x,OutBlock,Sizeof(x));
  {$IFNDEF CFORM}end;{$ENDIF}
end;

procedure InvertKey(EK, DK: PWord);
var
  i: longint;
  t1, t2, t3: word;
  temp: array[0..51] of word;
  p: PWord;
begin
  p:= pointer(NativeInt(@temp)+Sizeof(Temp));
  Dec(p);
  t1:= MulInv(EK^);
  Inc(EK);
  t2:= -EK^;
  Inc(EK);
  t3:= -EK^;
  Inc(EK);
  p^:= MulInv(EK^);
  Inc(EK);
  Dec(p);
  p^:= t3;
  Dec(p);
  p^:= t2;
  Dec(p);
  p^:= t1;
  Dec(p);
  for i:= 0 to 6 do
  begin
    t1:= EK^;
    Inc(EK);
    p^:= EK^;
    Inc(EK);
    Dec(p);
    p^:= t1;
    Dec(p);
    t1:= MulInv(EK^);
    Inc(EK);
    t2:= -EK^;
    Inc(EK);
    t3:= -EK^;
    Inc(EK);
    p^:= MulInv(EK^);
    Inc(EK);
    Dec(p);
    p^:= t2;
    Dec(p);
    p^:= t3;
    Dec(p);
    p^:= t1;
    Dec(p);
  end;
  t1:= EK^;
  Inc(EK);
  p^:= EK^;
  Dec(p);
  Inc(EK);
  p^:= t1;
  Dec(p);

  t1:= MulInv(EK^);
  Inc(EK);
  t2:= -EK^;
  Inc(EK);
  t3:= -EK^;
  Inc(EK);
  p^:= MulInv(EK^);
  Dec(p);
  p^:= t3;
  Dec(p);
  p^:= t2;
  Dec(p);
  p^:= t1;
  Move(Temp,DK^,Sizeof(Temp));
  FillChar(Temp,Sizeof(Temp),0);
end;

{$IFDEF CFORM}
procedure TDCP_idea.Init(var Key; Size: longint; IVector: pointer);
{$ELSE}
procedure IDEAInit;
{$ENDIF}
var
  i: longint;
begin
  {$IFDEF CFORM}
  if fInitialized then
    Burn;
  if (Size> fMaxKeySize) or (Size<= 0) or ((Size mod 8)<> 0) then
    raise Exception.Create(Format('IDEA: Invalid key size - %d',[Size]));
  {$ELSE}
  if (Size> 128) or (Size<= 0) or ((Size mod 8)<> 0) then
    Exit;
  with Data do begin
  {$ENDIF}
  Size:= Size div 8;
  FillChar(EK,Sizeof(EK),0);
  Move(Key,EK,Size);
  for i:= 0 to 7 do
    EK[i]:= (EK[i] shl 8) or (EK[i] shr 8);
  for i:= 1 to 5 do
  begin
    EK[(i*8)+0]:= (EK[((i-1)*8)+1] shl 9) or (EK[((i-1)*8)+2] shr 7);
    EK[(i*8)+1]:= (EK[((i-1)*8)+2] shl 9) or (EK[((i-1)*8)+3] shr 7);
    EK[(i*8)+2]:= (EK[((i-1)*8)+3] shl 9) or (EK[((i-1)*8)+4] shr 7);
    EK[(i*8)+3]:= (EK[((i-1)*8)+4] shl 9) or (EK[((i-1)*8)+5] shr 7);
    EK[(i*8)+4]:= (EK[((i-1)*8)+5] shl 9) or (EK[((i-1)*8)+6] shr 7);
    EK[(i*8)+5]:= (EK[((i-1)*8)+6] shl 9) or (EK[((i-1)*8)+7] shr 7);
    EK[(i*8)+6]:= (EK[((i-1)*8)+7] shl 9) or (EK[((i-1)*8)+0] shr 7);
    EK[(i*8)+7]:= (EK[((i-1)*8)+0] shl 9) or (EK[((i-1)*8)+1] shr 7);
  end;
  EK[48]:= (EK[41] shl 9) or (EK[42] shr 7);
  EK[49]:= (EK[42] shl 9) or (EK[43] shr 7);
  EK[50]:= (EK[43] shl 9) or (EK[44] shr 7);
  EK[51]:= (EK[44] shl 9) or (EK[45] shr 7);
  InvertKey(@EK,@DK);

  if IVector= nil then
  begin
    FillChar(IV,Sizeof(IV),$FF);
    {$IFDEF CFORM}Encrypt(IV,IV){$ELSE}IDEAEncryptECB(Data,IV,IV){$ENDIF};
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
procedure TDCP_idea.Burn;
{$ELSE}
procedure IDEABurn;
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  FillChar(EK,Sizeof(EK),$FF);
  FillChar(DK,Sizeof(DK),$FF);
  FillChar(IV,Sizeof(IV),$FF);
  FillChar(LB,Sizeof(LB),$FF);
  {$IFDEF CFORM}
  fInitialized:= false;
  {$ELSE}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_idea.Reset;
{$ELSE}
procedure IDEAReset;
{$ENDIF}
begin
  {$IFNDEF CFORM} with Data do{$ENDIF}
  Move(IV,LB,Sizeof(LB));
end;

{$IFDEF CFORM}
procedure TDCP_idea.EncryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('IDEA: Not initialized');
  Encrypt(InBlock,OutBlock);
end;

procedure TDCP_idea.DecryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('IDEA: Not initialized');
  Decrypt(InBlock,OutBlock);
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_idea.EncryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure IDEAEncryptCBC;
{$ENDIF}
var
  TB: array[0..7] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('IDEA: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 8) do
  begin
    XorBlock(pointer(NativeInt(@InData)+((i-1)*8)),@LB,@TB,Sizeof(TB));
    {$IFDEF CFORM}Encrypt(TB,TB){$ELSE}IDEAEncryptECB(Data,TB,TB){$ENDIF};
    Move(TB,pointer(NativeInt(@OutData)+((i-1)*8))^,Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 8)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}IDEAEncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(NativeInt(@InData)+Size-(Size mod 8))^,@pointer(NativeInt(@OutData)+Size-(Size mod 8))^,Size mod 8);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_idea.DecryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure IDEADecryptCBC;
{$ENDIF}
var
  TB: array[0..7] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('IDEA: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 8) do
  begin
    Move(pointer(NativeInt(@InData)+((i-1)*8))^,TB,Sizeof(TB));
    {$IFDEF CFORM}
    Decrypt(pointer(NativeInt(@InData)+((i-1)*8))^,pointer(NativeInt(@OutData)+((i-1)*8))^);
    {$ELSE}
    IDEADecryptECB(Data,pointer(NativeInt(@InData)+((i-1)*8))^,NativeInt(NativeInt(@OutData)+((i-1)*8))^);
    {$ENDIF}
    XorBlock(@LB,pointer(NativeInt(@OutData)+((i-1)*8)),pointer(NativeInt(@OutData)+((i-1)*8)),Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 8)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}IDEAEncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(NativeInt(@InData)+Size-(Size mod 8))^,@pointer(NativeInt(@OutData)+Size-(Size mod 8))^,Size mod 8);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_idea.EncryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure IDEAEncryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..7] of byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('IDEA: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}IDEAEncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],7);
    LB[7]:= PByteArray(@OutData)^[i];
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_idea.DecryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure IDEADecryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..7] of byte;
  b: byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('IDEA: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    b:= PByteArray(@InData)^[i];
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}IDEAEncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],7);
    LB[7]:= b;
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;


end.
