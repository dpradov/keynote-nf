{******************************************************************************}
{** A binary compatible implementation of RC5 *********************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}
unit RC5;

interface
{$I DCPcrypt.Inc}
uses
  {$IFDEF CFORM}Classes, Sysutils, {$ENDIF}DCPcrypt;

const
  NUMROUNDS= 12;    { number of rounds must be between 12-16 }

{$IFDEF CFORM}
type
  TDCP_rc5= class(TDCP_blockcipher)
  protected
    IV, LB: array[0..7] of byte;
    KeyData: array[0..((NUMROUNDS*2)+1)] of DWord;
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
  TRC5Data= record
    IV, LB: array[0..7] of byte;
    KeyData: array[0..31] of DWord;
    Rounds: longint;
  end;

procedure RC5Init(var Data: TRC5Data; var Key; Size: longint; IVector: pointer);
procedure RC5Reset(var Data: TRC5Data);
procedure RC5Burn(var Data:  TRC5Data);
procedure RC5EncryptECB(var Data: TRC5Data; const InBlock; var OutBlock);
procedure RC5DecryptECB(var Data: TRC5Data; const InBlock; var OutBlock);
procedure RC5EncryptCBC(var Data: TRC5Data; const InData; var OutData; Size: longint);
procedure RC5DecryptCBC(var Data: TRC5Data; const InData; var OutData; Size: longint);
procedure RC5EncryptCFB(var Data: TRC5Data; const InData; var OutData; Size: longint);
procedure RC5DecryptCFB(var Data: TRC5Data; const InData; var OutData; Size: longint);
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
implementation

const
   sBox: array[0..33] of dword= (
    $B7E15163,$5618CB1C,$F45044D5,$9287BE8E,$30BF3847,$CEF6B200,
    $6D2E2BB9,$0B65A572,$A99D1F2B,$47D498E4,$E60C129D,$84438C56,
    $227B060F,$C0B27FC8,$5EE9F981,$FD21733A,$9B58ECF3,$399066AC,
    $D7C7E065,$75FF5A1E,$1436D3D7,$B26E4D90,$50A5C749,$EEDD4102,
    $8D14BABB,$2B4C3474,$C983AE2D,$67BB27E6,$05F2A19F,$A42A1B58,
    $42619511,$E0990ECA,$7ED08883,$1D08023C);

{$IFDEF CFORM}
constructor TDCP_rc5.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAlgorithm:= 'RC5';
  fBlockSize:= 64;
  fMaxKeySize:= 2048;
  fID:= 3;
  Burn;
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_rc5.Encrypt(const InBlock; var OutBlock);
{$ELSE}
procedure RC5EncryptECB;
{$ENDIF}
var
  x: array[0..1] of DWord;
  i: longint;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,x,Sizeof(x));
  x[0]:= x[0] + KeyData[0];
  x[1]:= x[1] + KeyData[1];
  for i:= 1 to NUMROUNDS do
  begin
    x[0]:= x[0] xor x[1];
    x[0]:= LRot32(x[0],x[1])+KeyData[2*i];
    x[1]:= x[1] xor x[0];
    x[1]:= LRot32(x[1],x[0])+KeyData[(2*i)+1];
  end;
  Move(x,OutBlock,Sizeof(x));
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc5.Decrypt(const InBlock; var OutBlock);
{$ELSE}
procedure RC5DecryptECB;
{$ENDIF}
var
  x: array[0..1] of DWord;
  i: longint;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,x,Sizeof(x));
  for i:= NUMROUNDS downto 1 do
  begin
    x[1]:= RRot32(x[1]-KeyData[(2*i)+1],x[0]);
    x[1]:= x[1] xor x[0];
    x[0]:= RRot32(x[0]-KeyData[2*i],x[1]);
    x[0]:= x[0] xor x[1];
  end;
  x[1]:= x[1] - KeyData[1];
  x[0]:= x[0] - KeyData[0];
  Move(x,OutBlock,Sizeof(x));
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc5.Init(var Key; Size: longint; IVector: pointer);
{$ELSE}
procedure RC5Init;
{$ENDIF}
var
  xKeyD: array[0..63] of DWord;
  i, j, k, xKeyLen: longint;
  A, B: DWord;
begin
  {$IFDEF CFORM}
  if fInitialized then
    Burn;
  if (Size> fMaxKeySize) or (Size<= 0) or ((Size mod 8)<> 0) then
    raise Exception.Create(Format('RC5: Invalid key size - %d',[Size]));
  {$ELSE}
  if (Size> 2048) or (Size<= 0) or ((Size mod 8)<> 0) then
    Exit;
  with Data do begin
  {$ENDIF}
  FillChar(xKeyD,Sizeof(xKeyD),0);
  Size:= Size div 8;
  Move(Key,xKeyD,Size);
  xKeyLen:= Size div 4;
  if (Size mod 4)<> 0 then
    Inc(xKeyLen);
  Move(sBox,KeyData,(NUMROUNDS+1)*8);
  i:= 0; j:= 0;
  A:= 0; B:= 0;
  if xKeyLen> ((NUMROUNDS+1)*2) then
    k:= xKeyLen*3
  else
    k:= (NUMROUNDS+1)*6;
  for k:= k downto 1 do
  begin
    A:= LRot32(KeyData[i]+A+B,3);
    KeyData[i]:= A;
    B:= LRot32(xKeyD[j]+A+B,A+B);
    xKeyD[j]:= B;
    i:= (i+1) mod ((NUMROUNDS+1)*2);
    j:= (j+1) mod xKeyLen;
  end;
  FillChar(xKeyD,Sizeof(xKeyD),0);

  if IVector= nil then
  begin
    FillChar(IV,Sizeof(IV),$FF);
    {$IFDEF CFORM}Encrypt(IV,IV){$ELSE}RC5EncryptECB(Data,IV,IV){$ENDIF};
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
procedure TDCP_rc5.Burn;
{$ELSE}
procedure RC5Burn;
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
procedure TDCP_rc5.Reset;
{$ELSE}
procedure RC5Reset;
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do{$ENDIF}
  Move(IV,LB,Sizeof(LB));
end;

{$IFDEF CFORM}
procedure TDCP_rc5.EncryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('RC5: Not initialized');
  Encrypt(InBlock,OutBlock);
end;

procedure TDCP_rc5.DecryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('RC5: Not initialized');
  Decrypt(InBlock,OutBlock);
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_rc5.EncryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure RC5EncryptCBC;
{$ENDIF}
var
  TB: array[0..7] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('RC5: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 8) do
  begin
    XorBlock(pointer(longint(@InData)+((i-1)*8)),@LB,@TB,Sizeof(TB));
    {$IFDEF CFORM}Encrypt(TB,TB){$ELSE}RC5EncryptECB(Data,TB,TB){$ENDIF};
    Move(TB,pointer(longint(@OutData)+((i-1)*8))^,Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 8)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RC5EncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 8))^,@pointer(longint(@OutData)+Size-(Size mod 8))^,Size mod 8);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc5.DecryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure RC5DecryptCBC;
{$ENDIF}
var
  TB: array[0..7] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('RC5: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 8) do
  begin
    Move(pointer(longint(@InData)+((i-1)*8))^,TB,Sizeof(TB));
    {$IFDEF CFORM}
    Decrypt(pointer(longint(@InData)+((i-1)*8))^,pointer(longint(@OutData)+((i-1)*8))^);
    {$ELSE}
    RC5DecryptECB(Data,pointer(longint(@InData)+((i-1)*8))^,pointer(longint(@OutData)+((i-1)*8))^);
    {$ENDIF}
    XorBlock(@LB,pointer(longint(@OutData)+((i-1)*8)),pointer(longint(@OutData)+((i-1)*8)),Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 8)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RC5EncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 8))^,@pointer(longint(@OutData)+Size-(Size mod 8))^,Size mod 8);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc5.EncryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure RC5EncryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..7] of byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('RC5: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RC5EncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],7);
    LB[7]:= PByteArray(@OutData)^[i];
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc5.DecryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure RC5DecryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..7] of byte;
  b: byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('RC5: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    b:= PByteArray(@InData)^[i];
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RC5EncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],7);
    LB[7]:= b;
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;


end.
