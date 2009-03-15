{******************************************************************************}
{** A binary compatible implementation of Misty1 ******************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}
unit Misty1;

interface
{$I DCPcrypt.Inc}
uses
  {$IFDEF CFORM}Classes, Sysutils, {$ENDIF}DCPcrypt;

const
  NUMROUNDS= 8;

{$IFDEF CFORM}
type
  TDCP_misty1= class(TDCP_blockcipher)
  protected
    IV, LB: array[0..7] of byte;
    KeyData: array[0..31] of DWord;
    function FI(const FI_IN, FI_KEY: DWord): DWord;
    function FO(const FO_IN: DWord; const k: longint): DWord;
    function FL(const FL_IN: DWord; const k: longint): DWord;
    function FLINV(const FL_IN: DWord; const k: longint): DWord;
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
  TMisty1Data= record
    IV, LB: array[0..7] of byte;
    KeyData: array[0..31] of DWord;
  end;

procedure Misty1Init(var Data: TMisty1Data; var Key; Size: longint; IVector: pointer);
procedure Misty1Reset(var Data: TMisty1Data);
procedure Misty1Burn(var Data:  TMisty1Data);
procedure Misty1EncryptECB(var Data: TMisty1Data; const InBlock; var OutBlock);
procedure Misty1DecryptECB(var Data: TMisty1Data; const InBlock; var OutBlock);
procedure Misty1EncryptCBC(var Data: TMisty1Data; const InData; var OutData; Size: longint);
procedure Misty1DecryptCBC(var Data: TMisty1Data; const InData; var OutData; Size: longint);
procedure Misty1EncryptCFB(var Data: TMisty1Data; const InData; var OutData; Size: longint);
procedure Misty1DecryptCFB(var Data: TMisty1Data; const InData; var OutData; Size: longint);
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
implementation

{$I Misty1.Inc}

{$IFDEF CFORM}
constructor TDCP_misty1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAlgorithm:= 'Misty1';
  fBlockSize:= 64;
  fMaxKeySize:= 128;
  fID:= 11;
  Burn;
end;
{$ENDIF}

{$IFDEF CFORM}
function TDCP_misty1.FI(const FI_IN, FI_KEY: DWord): DWord;
{$ELSE}
function FI(const FI_IN, FI_KEY: DWord): DWord;
{$ENDIF}
var
  d7, d9: DWord;
begin
  d9:= (FI_IN shr 7) and $1ff;
  d7:= FI_IN and $7f;
  d9:= S9Table[d9] xor d7;
  d7:= (S7Table[d7] xor d9) and $7f;
  d7:= d7 xor ((FI_KEY shr 9) and $7f);
  d9:= d9 xor (FI_KEY and $1ff);
  d9:= S9Table[d9] xor d7;
  Result:= (d7 shl 9) or d9;
end;

{$IFDEF CFORM}
function TDCP_misty1.FO(const FO_IN: DWord; const k: longint): DWord;
{$ELSE}
function FO(const Data: TMisty1Data; const FO_IN: DWord; const k: longint): DWord;
{$ENDIF}
var
  t0, t1: DWord;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  t0:= FO_IN shr 16;
  t1:= FO_IN and $FFFF;
  t0:= t0 xor KeyData[k];
  t0:= FI(t0,KeyData[((k+5) mod 8) + 8]);
  t0:= t0 xor t1;
  t1:= t1 xor KeyData[(k+2) mod 8];
  t1:= FI(t1,KeyData[((k+1) mod 8) + 8]);
  t1:= t1 xor t0;
  t0:= t0 xor KeyData[(k+7) mod 8];
  t0:= FI(t0,KeyData[((k+3) mod 8) + 8]);
  t0:= t0 xor t1;
  t1:= t1 xor KeyData[(k+4) mod 8];
  Result:= (t1 shl 16) or t0;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
function TDCP_misty1.FL(const FL_IN: DWord; const k: longint): DWord;
{$ELSE}
function FL(const Data: TMisty1Data; const FL_IN: DWord; const k: longint): DWord;
{$ENDIF}
var
  d0, d1: DWord;
  t: byte;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  d0:= FL_IN shr 16;
  d1:= FL_IN and $FFFF;
  if (k mod 2)<> 0 then
  begin
    t:= (k-1) div 2;
    d1:= d1 xor (d0 and KeyData[((t + 2) mod 8) + 8]);
    d0:= d0 xor (d1 or KeyData[(t + 4) mod 8]);
  end
  else
  begin
    t:= k div 2;
    d1:= d1 xor (d0 and KeyData[t]);
    d0:= d0 xor (d1 or KeyData[((t+6) mod 8) + 8]);
  end;
  Result:= (d0 shl 16) or d1;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
function TDCP_misty1.FLINV(const FL_IN: DWord; const k: longint): DWord;
{$ELSE}
function FLINV(const Data: TMisty1Data; const FL_IN: DWord; const k: longint): DWord;
{$ENDIF}
var
  d0, d1: DWord;
  t: byte;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  d0:= FL_IN shr 16;
  d1:= FL_IN and $FFFF;
  if (k mod 2)<> 0 then
  begin
    t:= (k-1) div 2;
    d0:= d0 xor (d1 or KeyData[(t+4) mod 8]);
    d1:= d1 xor (d0 and KeyData[((t+2) mod 8) + 8]);
  end
  else
  begin
    t:= k div 2;
    d0:= d0 xor (d1 or KeyData[((t+6) mod 8) + 8]);
    d1:= d1 xor (d0 and KeyData[t]);
  end;
  Result:= (d0 shl 16) or d1;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_misty1.Encrypt(const InBlock; var OutBlock);
{$ELSE}
procedure Misty1EncryptECB;
{$ENDIF}
var
  d0, d1: DWord;
  i: longint;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,d0,4);
  Move(pointer(longint(@InBlock)+4)^,d1,4);
  for i:= 0 to NUMROUNDS-1 do
  begin
    if (i mod 2)= 0 then
    begin
      d0:= FL({$IFNDEF CFORM}Data,{$ENDIF}D0,i);
      d1:= FL({$IFNDEF CFORM}Data,{$ENDIF}D1,i+1);
      d1:= d1 xor FO({$IFNDEF CFORM}Data,{$ENDIF}d0,i);
    end
    else
      d0:= d0 xor FO({$IFNDEF CFORM}Data,{$ENDIF}d1,i);
  end;
  d0:= FL({$IFNDEF CFORM}Data,{$ENDIF}d0,NUMROUNDS);
  d1:= FL({$IFNDEF CFORM}Data,{$ENDIF}d1,NUMROUNDS+1);
  Move(d1,OutBlock,4);
  Move(d0,pointer(longint(@OutBlock)+4)^,4);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_misty1.Decrypt(const InBlock; var OutBlock);
{$ELSE}
procedure Misty1DecryptECB;
{$ENDIF}
var
  d0, d1: DWord;
  i: longint;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,d1,4);
  Move(pointer(longint(@InBlock)+4)^,d0,4);
  d1:= FLINV({$IFNDEF CFORM}Data,{$ENDIF}d1,NUMROUNDS+1);
  d0:= FLINV({$IFNDEF CFORM}Data,{$ENDIF}d0,NUMROUNDS);
  for i:= NUMROUNDS-1 downto 0 do
  begin
    if (i mod 2)= 0 then
    begin
      d1:= d1 xor FO({$IFNDEF CFORM}Data,{$ENDIF}d0,i);
      d0:= FLINV({$IFNDEF CFORM}Data,{$ENDIF}D0,i);
      d1:= FLINV({$IFNDEF CFORM}Data,{$ENDIF}D1,i+1);
    end
    else
      d0:= d0 xor FO({$IFNDEF CFORM}Data,{$ENDIF}d1,i);
  end;
  Move(d0,OutBlock,4);
  Move(d1,pointer(longint(@OutBlock)+4)^,4);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_misty1.Init(var Key; Size: longint; IVector: pointer);
{$ELSE}
procedure Misty1Init;
{$ENDIF}
var
  KeyB: array[0..15] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if fInitialized then
    Burn;
  if (Size> fMaxKeySize) or (Size<= 0) or ((Size mod 8)<> 0) then
    raise Exception.Create(Format('Misty1: Invalid key size - %d',[Size]));
  {$ELSE}
  if (Size> 128) or (Size<= 0) or ((Size mod 8)<> 0) then
    Exit;
  with Data do begin
  {$ENDIF}

  FillChar(KeyB,Sizeof(KeyB),0);
  Move(Key,KeyB,Size div 8);
  for i:= 0 to 7 do
    KeyData[i]:= (KeyB[i*2] * 256) + KeyB[i*2+1];
  for i:= 0 to 7 do
  begin
    KeyData[i+8]:= FI(KeyData[i],KeyData[(i+1) mod 8]);
    KeyData[i+16]:= KeyData[i+8] and $1FF;
    KeyData[i+24]:= KeyData[i+8] shr 9;
  end;

  if IVector= nil then
  begin
    FillChar(IV,Sizeof(IV),$FF);
    {$IFDEF CFORM}Encrypt(IV,IV){$ELSE}Misty1EncryptECB(Data,IV,IV){$ENDIF};
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
procedure TDCP_misty1.Burn;
{$ELSE}
procedure Misty1Burn;
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
procedure TDCP_misty1.Reset;
{$ELSE}
procedure Misty1Reset;
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do{$ENDIF}
  Move(IV,LB,Sizeof(LB));
end;

{$IFDEF CFORM}
procedure TDCP_misty1.EncryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('Misty1: Not initialized');
  Encrypt(InBlock,OutBlock);
end;

procedure TDCP_misty1.DecryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('Misty1: Not initialized');
  Decrypt(InBlock,OutBlock);
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_misty1.EncryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure Misty1EncryptCBC;
{$ENDIF}
var
  TB: array[0..7] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Misty1: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 8) do
  begin
    XorBlock(pointer(longint(@InData)+((i-1)*8)),@LB,@TB,Sizeof(TB));
    {$IFDEF CFORM}Encrypt(TB,TB){$ELSE}Misty1EncryptECB(Data,TB,TB){$ENDIF};
    Move(TB,pointer(longint(@OutData)+((i-1)*8))^,Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 8)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}Misty1EncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 8))^,@pointer(longint(@OutData)+Size-(Size mod 8))^,Size mod 8);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_misty1.DecryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure Misty1DecryptCBC;
{$ENDIF}
var
  TB: array[0..7] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Misty1: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 8) do
  begin
    Move(pointer(longint(@InData)+((i-1)*8))^,TB,Sizeof(TB));
    {$IFDEF CFORM}
    Decrypt(pointer(longint(@InData)+((i-1)*8))^,pointer(longint(@OutData)+((i-1)*8))^);
    {$ELSE}
    Misty1DecryptECB(Data,pointer(longint(@InData)+((i-1)*8))^,pointer(longint(@OutData)+((i-1)*8))^);
    {$ENDIF}
    XorBlock(@LB,pointer(longint(@OutData)+((i-1)*8)),pointer(longint(@OutData)+((i-1)*8)),Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 8)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}Misty1EncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 8))^,@pointer(longint(@OutData)+Size-(Size mod 8))^,Size mod 8);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_misty1.EncryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure Misty1EncryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..7] of byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Misty1: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}Misty1EncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],7);
    LB[7]:= PByteArray(@OutData)^[i];
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_misty1.DecryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure Misty1DecryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..7] of byte;
  b: byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Misty1: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    b:= PByteArray(@InData)^[i];
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}Misty1EncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],7);
    LB[7]:= b;
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;


end.
