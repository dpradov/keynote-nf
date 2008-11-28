{******************************************************************************}
{** A binary compatible implementation of Gost ********************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}
unit Gost;

interface
{$I DCPcrypt.Inc}
uses
  {$IFDEF CFORM}Classes, Sysutils, {$ENDIF}DCPcrypt;

{$IFDEF CFORM}
type
  TDCP_gost= class(TDCP_blockcipher)
  protected
    IV, LB: array[0..7] of byte;
    KeyData: array[0..7] of DWord;
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
  TGostData= record
    IV, LB: array[0..7] of byte;
    KeyData: array[0..7] of DWord;
  end;

procedure GostInit(var Data: TGostData; var Key; Size: longint; IVector: pointer);
procedure GostReset(var Data: TGostData);
procedure GostBurn(var Data:  TGostData);
procedure GostEncryptECB(var Data: TGostData; const InBlock; var OutBlock);
procedure GostDecryptECB(var Data: TGostData; const InBlock; var OutBlock);
procedure GostEncryptCBC(var Data: TGostData; const InData; var OutData; Size: longint);
procedure GostDecryptCBC(var Data: TGostData; const InData; var OutData; Size: longint);
procedure GostEncryptCFB(var Data: TGostData; const InData; var OutData; Size: longint);
procedure GostDecryptCFB(var Data: TGostData; const InData; var OutData; Size: longint);
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
implementation

{$I Gost.Inc}

{$IFDEF CFORM}
constructor TDCP_gost.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAlgorithm:= 'Gost';
  fBlockSize:= 64;
  fMaxKeySize:= 256;
  fID:= 8;
  Burn;
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_gost.Encrypt(const InBlock; var OutBlock);
{$ELSE}
procedure GostEncryptECB;
{$ENDIF}
var
  n1, n2: DWord;
  i: longint;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,n1,4);
  Move(pointer(longint(@InBlock)+4)^,n2,4);
  for i:= 0 to 2 do
  begin
    n2:= n2 xor (sTable[3,(n1+KeyData[0]) shr 24] xor sTable[2,((n1+KeyData[0]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[0]) shr 8) and $FF] xor sTable[0,(n1+KeyData[0]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[1]) shr 24] xor sTable[2,((n2+KeyData[1]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[1]) shr 8) and $FF] xor sTable[0,(n2+KeyData[1]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[2]) shr 24] xor sTable[2,((n1+KeyData[2]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[2]) shr 8) and $FF] xor sTable[0,(n1+KeyData[2]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[3]) shr 24] xor sTable[2,((n2+KeyData[3]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[3]) shr 8) and $FF] xor sTable[0,(n2+KeyData[3]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[4]) shr 24] xor sTable[2,((n1+KeyData[4]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[4]) shr 8) and $FF] xor sTable[0,(n1+KeyData[4]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[5]) shr 24] xor sTable[2,((n2+KeyData[5]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[5]) shr 8) and $FF] xor sTable[0,(n2+KeyData[5]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[6]) shr 24] xor sTable[2,((n1+KeyData[6]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[6]) shr 8) and $FF] xor sTable[0,(n1+KeyData[6]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[7]) shr 24] xor sTable[2,((n2+KeyData[7]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[7]) shr 8) and $FF] xor sTable[0,(n2+KeyData[7]) and $FF]);
  end;
  n2:= n2 xor (sTable[3,(n1+KeyData[7]) shr 24] xor sTable[2,((n1+KeyData[7]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[7]) shr 8) and $FF] xor sTable[0,(n1+KeyData[7]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[6]) shr 24] xor sTable[2,((n2+KeyData[6]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[6]) shr 8) and $FF] xor sTable[0,(n2+KeyData[6]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[5]) shr 24] xor sTable[2,((n1+KeyData[5]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[5]) shr 8) and $FF] xor sTable[0,(n1+KeyData[5]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[4]) shr 24] xor sTable[2,((n2+KeyData[4]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[4]) shr 8) and $FF] xor sTable[0,(n2+KeyData[4]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[3]) shr 24] xor sTable[2,((n1+KeyData[3]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[3]) shr 8) and $FF] xor sTable[0,(n1+KeyData[3]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[2]) shr 24] xor sTable[2,((n2+KeyData[2]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[2]) shr 8) and $FF] xor sTable[0,(n2+KeyData[2]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[1]) shr 24] xor sTable[2,((n1+KeyData[1]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[1]) shr 8) and $FF] xor sTable[0,(n1+KeyData[1]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[0]) shr 24] xor sTable[2,((n2+KeyData[0]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[0]) shr 8) and $FF] xor sTable[0,(n2+KeyData[0]) and $FF]);
  Move(n2,OutBlock,4);
  Move(n1,pointer(longint(@OutBlock)+4)^,4);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_gost.Decrypt(const InBlock; var OutBlock);
{$ELSE}
procedure GostDecryptECB;
{$ENDIF}
var
  n1, n2: DWord;
  i: longint;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,n1,4);
  Move(pointer(longint(@InBlock)+4)^,n2,4);
  n2:= n2 xor (sTable[3,(n1+KeyData[0]) shr 24] xor sTable[2,((n1+KeyData[0]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[0]) shr 8) and $FF] xor sTable[0,(n1+KeyData[0]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[1]) shr 24] xor sTable[2,((n2+KeyData[1]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[1]) shr 8) and $FF] xor sTable[0,(n2+KeyData[1]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[2]) shr 24] xor sTable[2,((n1+KeyData[2]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[2]) shr 8) and $FF] xor sTable[0,(n1+KeyData[2]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[3]) shr 24] xor sTable[2,((n2+KeyData[3]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[3]) shr 8) and $FF] xor sTable[0,(n2+KeyData[3]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[4]) shr 24] xor sTable[2,((n1+KeyData[4]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[4]) shr 8) and $FF] xor sTable[0,(n1+KeyData[4]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[5]) shr 24] xor sTable[2,((n2+KeyData[5]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[5]) shr 8) and $FF] xor sTable[0,(n2+KeyData[5]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[6]) shr 24] xor sTable[2,((n1+KeyData[6]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[6]) shr 8) and $FF] xor sTable[0,(n1+KeyData[6]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[7]) shr 24] xor sTable[2,((n2+KeyData[7]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[7]) shr 8) and $FF] xor sTable[0,(n2+KeyData[7]) and $FF]);
  for i:= 0 to 2 do
  begin
    n2:= n2 xor (sTable[3,(n1+KeyData[7]) shr 24] xor sTable[2,((n1+KeyData[7]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[7]) shr 8) and $FF] xor sTable[0,(n1+KeyData[7]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[6]) shr 24] xor sTable[2,((n2+KeyData[6]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[6]) shr 8) and $FF] xor sTable[0,(n2+KeyData[6]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[5]) shr 24] xor sTable[2,((n1+KeyData[5]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[5]) shr 8) and $FF] xor sTable[0,(n1+KeyData[5]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[4]) shr 24] xor sTable[2,((n2+KeyData[4]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[4]) shr 8) and $FF] xor sTable[0,(n2+KeyData[4]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[3]) shr 24] xor sTable[2,((n1+KeyData[3]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[3]) shr 8) and $FF] xor sTable[0,(n1+KeyData[3]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[2]) shr 24] xor sTable[2,((n2+KeyData[2]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[2]) shr 8) and $FF] xor sTable[0,(n2+KeyData[2]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[1]) shr 24] xor sTable[2,((n1+KeyData[1]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[1]) shr 8) and $FF] xor sTable[0,(n1+KeyData[1]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[0]) shr 24] xor sTable[2,((n2+KeyData[0]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[0]) shr 8) and $FF] xor sTable[0,(n2+KeyData[0]) and $FF]);
  end;
  Move(n2,OutBlock,4);
  Move(n1,pointer(longint(@OutBlock)+4)^,4);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_gost.Init(var Key; Size: longint; IVector: pointer);
{$ELSE}
procedure GostInit;
{$ENDIF}
var
  i: longint;
  userkey: array[0..31] of byte;
begin
  {$IFDEF CFORM}
  if fInitialized then
    Burn;
  if (Size> fMaxKeySize) or (Size<= 0) or ((Size mod 8)<> 0) then
    raise Exception.Create(Format('Gost: Invalid key size - %d',[Size]));
  {$ELSE}
  if (Size> 256) or (Size<= 0) or ((Size mod 8)<> 0) then
    Exit;
  with Data do begin
  {$ENDIF}
  Size:= Size div 8;
  FillChar(userkey,Sizeof(userkey),0);
  Move(Key,userkey,Size);
  for i:= 0 to 7 do
    KeyData[i]:= (dword(UserKey[4*i+3]) shl 24) or (dword(UserKey[4*i+2]) shl 16) or
      (dword(UserKey[4*i+1]) shl 8) or (dword(UserKey[4*i+0]));

  if IVector= nil then
  begin
    FillChar(IV,Sizeof(IV),$FF);
    {$IFDEF CFORM}Encrypt(IV,IV){$ELSE}GostEncryptECB(Data,IV,IV){$ENDIF};
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
procedure TDCP_gost.Burn;
{$ELSE}
procedure GostBurn;
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
procedure TDCP_gost.Reset;
{$ELSE}
procedure GostReset;
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do{$ENDIF}
  Move(IV,LB,Sizeof(LB));
end;

{$IFDEF CFORM}
procedure TDCP_gost.EncryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('Gost: Not initialized');
  Encrypt(InBlock,OutBlock);
end;

procedure TDCP_gost.DecryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('Gost: Not initialized');
  Decrypt(InBlock,OutBlock);
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_gost.EncryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure GostEncryptCBC;
{$ENDIF}
var
  TB: array[0..7] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Gost: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 8) do
  begin
    XorBlock(pointer(longint(@InData)+((i-1)*8)),@LB,@TB,Sizeof(TB));
    {$IFDEF CFORM}Encrypt(TB,TB){$ELSE}GostEncryptECB(Data,TB,TB){$ENDIF};
    Move(TB,pointer(longint(@OutData)+((i-1)*8))^,Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 8)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}GostEncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 8))^,@pointer(longint(@OutData)+Size-(Size mod 8))^,Size mod 8);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_gost.DecryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure GostDecryptCBC;
{$ENDIF}
var
  TB: array[0..7] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Gost: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 8) do
  begin
    Move(pointer(longint(@InData)+((i-1)*8))^,TB,Sizeof(TB));
    {$IFDEF CFORM}
    Decrypt(pointer(longint(@InData)+((i-1)*8))^,pointer(longint(@OutData)+((i-1)*8))^);
    {$ELSE}
    GostDecryptECB(Data,pointer(longint(@InData)+((i-1)*8))^,pointer(longint(@OutData)+((i-1)*8))^);
    {$ENDIF}
    XorBlock(@LB,pointer(longint(@OutData)+((i-1)*8)),pointer(longint(@OutData)+((i-1)*8)),Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 8)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}GostEncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 8))^,@pointer(longint(@OutData)+Size-(Size mod 8))^,Size mod 8);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_gost.EncryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure GostEncryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..7] of byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Gost: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}GostEncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],7);
    LB[7]:= PByteArray(@OutData)^[i];
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_gost.DecryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure GostDecryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..7] of byte;
  b: byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Gost: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    b:= PByteArray(@InData)^[i];
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}GostEncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],7);
    LB[7]:= b;
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;


end.
