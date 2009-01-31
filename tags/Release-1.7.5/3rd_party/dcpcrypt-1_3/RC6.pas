{******************************************************************************}
{** A binary compatible implementation of RC6 *********************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}
unit RC6;

interface
{$I DCPcrypt.Inc}
uses
  {$IFDEF CFORM}Classes, Sysutils, {$ENDIF}DCPcrypt;

const
  NUMROUNDS= 20; { number of rounds must be between 16-24 }

{$IFDEF CFORM}
type
  TDCP_rc6= class(TDCP_blockcipher)
  protected
    IV, LB: array[0..15] of byte;
    KeyData: array[0..((NUMROUNDS*2)+3)] of DWord;
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
  TRC6Data= record
    IV, LB: array[0..15] of byte;
    KeyData: array[0..((NUMROUNDS*2)+3)] of DWord;
  end;

procedure RC6Init(var Data: TRC6Data; var Key; Size: longint; IVector: pointer);
procedure RC6Reset(var Data: TRC6Data);
procedure RC6Burn(var Data:  TRC6Data);
procedure RC6EncryptECB(var Data: TRC6Data; const InBlock; var OutBlock);
procedure RC6DecryptECB(var Data: TRC6Data; const InBlock; var OutBlock);
procedure RC6EncryptCBC(var Data: TRC6Data; const InData; var OutData; Size: longint);
procedure RC6DecryptCBC(var Data: TRC6Data; const InData; var OutData; Size: longint);
procedure RC6EncryptCFB(var Data: TRC6Data; const InData; var OutData; Size: longint);
procedure RC6DecryptCFB(var Data: TRC6Data; const InData; var OutData; Size: longint);
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
implementation

const
  sBox: array[0..51] of DWord= (
    $B7E15163,$5618CB1C,$F45044D5,$9287BE8E,$30BF3847,$CEF6B200,
    $6D2E2BB9,$0B65A572,$A99D1F2B,$47D498E4,$E60C129D,$84438C56,
    $227B060F,$C0B27FC8,$5EE9F981,$FD21733A,$9B58ECF3,$399066AC,
    $D7C7E065,$75FF5A1E,$1436D3D7,$B26E4D90,$50A5C749,$EEDD4102,
    $8D14BABB,$2B4C3474,$C983AE2D,$67BB27E6,$05F2A19F,$A42A1B58,
    $42619511,$E0990ECA,$7ED08883,$1D08023C,$BB3F7BF5,$5976F5AE,
    $F7AE6F67,$95E5E920,$341D62D9,$D254DC92,$708C564B,$0EC3D004,
    $ACFB49BD,$4B32C376,$E96A3D2F,$87A1B6E8,$25D930A1,$C410AA5A,
    $62482413,$007F9DCC,$9EB71785,$3CEE913E);

{$IFDEF CFORM}
constructor TDCP_rc6.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAlgorithm:= 'RC6';
  fBlockSize:= 128;
  fMaxKeySize:= 2048;
  fID:= 4;
  Burn;
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_rc6.Encrypt(const InBlock; var OutBlock);
{$ELSE}
procedure RC6EncryptECB;
{$ENDIF}
var
  x: array[0..3] of DWord;
  u, t: DWord;
  i: longint;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,x,Sizeof(x));
  x[1]:= x[1] + KeyData[0];
  x[3]:= x[3] + KeyData[1];
  for i:= 1 to NUMROUNDS do
  begin
    t:= Lrot32(x[1] * (2*x[1] + 1),5);
    u:= Lrot32(x[3] * (2*x[3] + 1),5);
    x[0]:= Lrot32(x[0] xor t,u) + KeyData[2*i];
    x[2]:= Lrot32(x[2] xor u,t) + KeyData[2*i+1];
    t:= x[0]; x[0]:= x[1]; x[1]:= x[2]; x[2]:= x[3]; x[3]:= t;
  end;
  x[0]:= x[0] + KeyData[(2*NUMROUNDS)+2];
  x[2]:= x[2] + KeyData[(2*NUMROUNDS)+3];
  Move(x,OutBlock,Sizeof(x));
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc6.Decrypt(const InBlock; var OutBlock);
{$ELSE}
procedure RC6DecryptECB;
{$ENDIF}
var
  x: array[0..3] of DWord;
  u, t: DWord;
  i: longint;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,x,Sizeof(x));
  x[2]:= x[2] - KeyData[(2*NUMROUNDS)+3];
  x[0]:= x[0] - KeyData[(2*NUMROUNDS)+2];
  for i:= NUMROUNDS downto 1 do
  begin
    t:= x[0]; x[0]:= x[3]; x[3]:= x[2]; x[2]:= x[1]; x[1]:= t;
    u:= Lrot32(x[3] * (2*x[3] + 1),5);
    t:= Lrot32(x[1] * (2*x[1] + 1),5);
    x[2]:= Rrot32(x[2] - KeyData[2*i+1],t) xor u;
    x[0]:= Rrot32(x[0] - KeyData[2*i],u) xor t;
  end;
  x[3]:= x[3] - KeyData[1];
  x[1]:= x[1] - KeyData[0];
  Move(x,OutBlock,Sizeof(x));
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc6.Init(var Key; Size: longint; IVector: pointer);
{$ELSE}
procedure RC6Init;
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
    raise Exception.Create(Format('RC6: Invalid key size - %d',[Size]));
  {$ELSE}
  if (Size> 2048) or (Size<= 0) or ((Size mod 8)<> 0) then
    Exit;
  with Data do begin
  {$ENDIF}
  Size:= Size div 8;
  FillChar(xKeyD,Sizeof(xKeyD),0);
  Move(Key,xKeyD,Size);
  xKeyLen:= Size div 4;
  if (Size mod 4)<> 0 then
    Inc(xKeyLen);
  Move(sBox,KeyData,((NUMROUNDS*2)+4)*4);
  i:= 0; j:= 0;
  A:= 0; B:= 0;
  if xKeyLen> ((NUMROUNDS*2)+4) then
    k:= xKeyLen*3
  else
    k:= ((NUMROUNDS*2)+4)*3;
  for k:= 1 to k do
  begin
    A:= LRot32(KeyData[i]+A+B,3);
    KeyData[i]:= A;
    B:= LRot32(xKeyD[j]+A+B,A+B);
    xKeyD[j]:= B;
    i:= (i+1) mod ((NUMROUNDS*2)+4);
    j:= (j+1) mod xKeyLen;
  end;
  FillChar(xKeyD,Sizeof(xKeyD),0);

  if IVector= nil then
  begin
    FillChar(IV,Sizeof(IV),$FF);
    {$IFDEF CFORM}Encrypt(IV,IV){$ELSE}RC6EncryptECB(Data,IV,IV){$ENDIF};
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
procedure TDCP_rc6.Burn;
{$ELSE}
procedure RC6Burn;
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
procedure TDCP_rc6.Reset;
{$ELSE}
procedure RC6Reset;
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do{$ENDIF}
  Move(IV,LB,Sizeof(LB));
end;

{$IFDEF CFORM}
procedure TDCP_rc6.EncryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('RC6: Not initialized');
  Encrypt(InBlock,OutBlock);
end;

procedure TDCP_rc6.DecryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('RC6: Not initialized');
  Decrypt(InBlock,OutBlock);
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_rc6.EncryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure RC6EncryptCBC;
{$ENDIF}
var
  TB: array[0..15] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('RC6: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 16) do
  begin
    XorBlock(pointer(longint(@InData)+((i-1)*16)),@LB,@TB,Sizeof(TB));
    {$IFDEF CFORM}Encrypt(TB,TB){$ELSE}RC6EncryptECB(Data,TB,TB){$ENDIF};
    Move(TB,pointer(longint(@OutData)+((i-1)*16))^,Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 16)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RC6EncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 16))^,@pointer(longint(@OutData)+Size-(Size mod 16))^,Size mod 16);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc6.DecryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure RC6DecryptCBC;
{$ENDIF}
var
  TB: array[0..15] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('RC6: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 16) do
  begin
    Move(pointer(longint(@InData)+((i-1)*16))^,TB,Sizeof(TB));
    {$IFDEF CFORM}
    Decrypt(pointer(longint(@InData)+((i-1)*16))^,pointer(longint(@OutData)+((i-1)*16))^);
    {$ELSE}
    RC6DecryptECB(Data,pointer(longint(@InData)+((i-1)*16))^,pointer(longint(@OutData)+((i-1)*16))^);
    {$ENDIF}
    XorBlock(@LB,pointer(longint(@OutData)+((i-1)*16)),pointer(longint(@OutData)+((i-1)*16)),Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 16)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RC6EncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 16))^,@pointer(longint(@OutData)+Size-(Size mod 16))^,Size mod 16);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc6.EncryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure RC6EncryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..15] of byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('RC6: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RC6EncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],15);
    LB[15]:= PByteArray(@OutData)^[i];
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc6.DecryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure RC6DecryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..15] of byte;
  b: byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('RC6: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    b:= PByteArray(@InData)^[i];
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RC6EncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],15);
    LB[15]:= b;
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;


end.
