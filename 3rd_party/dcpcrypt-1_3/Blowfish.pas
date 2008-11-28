{******************************************************************************}
{** A binary compatible implementation of Blowfish ****************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}
unit Blowfish;

interface
{$I DCPcrypt.inc}
uses
  {$IFDEF CFORM}Classes, Sysutils, {$ENDIF}DCPcrypt;

{$IFDEF CFORM}
type
  TDCP_blowfish= class(TDCP_blockcipher)
  protected
    IV, LB: array[0..7] of byte;
    SBox: array[0..3,0..255] of DWord;
    PBox: array[0..17] of DWord;
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
  TBlowfishData= record
    IV, LB: array[0..7] of byte;
    SBox: array[0..3,0..255] of DWord;
    PBox: array[0..17] of DWord;
  end;

procedure BlowfishInit(var Data: TBlowfishData; var Key; Size: longint; IVector: pointer);
procedure BlowfishReset(var Data: TBlowfishData);
procedure BlowfishBurn(var Data:  TBlowfishData);
procedure BlowfishEncryptECB(var Data: TBlowfishData; const InBlock; var OutBlock);
procedure BlowfishDecryptECB(var Data: TBlowfishData; const InBlock; var OutBlock);
procedure BlowfishEncryptCBC(var Data: TBlowfishData; const InData; var OutData; Size: longint);
procedure BlowfishDecryptCBC(var Data: TBlowfishData; const InData; var OutData; Size: longint);
procedure BlowfishEncryptCFB(var Data: TBlowfishData; const InData; var OutData; Size: longint);
procedure BlowfishDecryptCFB(var Data: TBlowfishData; const InData; var OutData; Size: longint);
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
implementation

{$I Blowfish.Inc}

{$IFDEF CFORM}
constructor TDCP_blowfish.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAlgorithm:= 'Blowfish';
  fBlockSize:= 64;
  fMaxKeySize:= 448;
  fID:= 5;
  Burn;
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_blowfish.Encrypt(const InBlock; var OutBlock);
{$ELSE}
procedure BlowfishEncryptECB;
{$ENDIF}
var
  xL, xR: DWord;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,xL,4);
  Move(pointer(longint(@InBlock)+4)^,xR,4);
  xL:= (xL shr 24) or ((xL shr 8) and $FF00) or ((xL shl 8) and $FF0000) or (xL shl 24);
  xR:= (xR shr 24) or ((xR shr 8) and $FF00) or ((xR shl 8) and $FF0000) or (xR shl 24);
  xL:= xL xor PBox[0];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[1];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[2];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[3];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[4];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[5];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[6];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[7];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[8];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[9];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[10];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[11];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[12];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[13];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[14];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[15];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[16];
  xR:= xR xor PBox[17];
  xL:= (xL shr 24) or ((xL shr 8) and $FF00) or ((xL shl 8) and $FF0000) or (xL shl 24);
  xR:= (xR shr 24) or ((xR shr 8) and $FF00) or ((xR shl 8) and $FF0000) or (xR shl 24);
  Move(xR,OutBlock,4);
  Move(xL,pointer(longint(@OutBlock)+4)^,4);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_blowfish.Decrypt(const InBlock; var OutBlock);
{$ELSE}
procedure BlowfishDecryptECB;
{$ENDIF}
var
  xL, xR: DWord;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,xL,4);
  Move(pointer(longint(@InBlock)+4)^,xR,4);
  xL:= (xL shr 24) or ((xL shr 8) and $FF00) or ((xL shl 8) and $FF0000) or (xL shl 24);
  xR:= (xR shr 24) or ((xR shr 8) and $FF00) or ((xR shl 8) and $FF0000) or (xR shl 24);
  xL:= xL xor PBox[17];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[16];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[15];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[14];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[13];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[12];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[11];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[10];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[9];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[8];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[7];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[6];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[5];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[4];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[3];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[2];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[1];
  xR:= xR xor PBox[0];
  xL:= (xL shr 24) or ((xL shr 8) and $FF00) or ((xL shl 8) and $FF0000) or (xL shl 24);
  xR:= (xR shr 24) or ((xR shr 8) and $FF00) or ((xR shl 8) and $FF0000) or (xR shl 24);
  Move(xR,OutBlock,4);
  Move(xL,pointer(longint(@OutBlock)+4)^,4);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_blowfish.Init(var Key; Size: longint; IVector: pointer);
{$ELSE}
procedure BlowfishInit;
{$ENDIF}
var
  i, k: longint;
  A: DWord;
  KeyB: PByteArray;
  Block: array[0..7] of byte;
begin
  {$IFDEF CFORM}
  if fInitialized then
    Burn;
  if (Size> fMaxKeySize) or (Size<= 0) or ((Size mod 8)<> 0) then
    raise Exception.Create(Format('Blowfish: Invalid key size - %d',[Size]));
  {$ELSE}
  if (Size> 448) or (Size<= 0) or ((Size mod 8)<> 0) then
    Exit;
  with Data do begin
  {$ENDIF}
  Size:= Size div 8;
  KeyB:= @Key;
  Move(SBoxOrg,SBox,Sizeof(SBox));
  Move(PBoxOrg,PBox,Sizeof(PBox));
  k:= 0;
  for i:= 0 to 17 do
  begin
    A:= dword(KeyB^[(k+3) mod Size]);
    A:= A + (dword(KeyB^[(k+2) mod Size]) shl 8);
    A:= A + (dword(KeyB^[(k+1) mod Size]) shl 16);
    A:= A + (dword(KeyB^[k]) shl 24);
    PBox[i]:= PBox[i] xor A;
    k:= (k+4) mod Size;
  end;
  FillChar(Block,Sizeof(Block),0);
  for i:= 0 to 8 do
  begin
    {$IFDEF CFORM}Encrypt(Block,Block){$ELSE}BlowfishEncryptECB(Data,Block,Block){$ENDIF};
    PBox[i*2]:= dword(Block[3]) + (dword(Block[2]) shl 8) + (dword(Block[1]) shl 16) + (dword(Block[0]) shl 24);
    PBox[i*2+1]:= dword(Block[7]) + (dword(Block[6]) shl 8) + (dword(Block[5]) shl 16) + (dword(Block[4]) shl 24);
  end;
  for k:= 0 to 3 do
  begin
    for i:= 0 to 127 do
    begin
      {$IFDEF CFORM}Encrypt(Block,Block){$ELSE}BlowfishEncryptECB(Data,Block,Block){$ENDIF};
      SBox[k,i*2]:= dword(Block[3]) + (dword(Block[2]) shl 8) + (dword(Block[1]) shl 16) + (dword(Block[0]) shl 24);
      SBox[k,i*2+1]:= dword(Block[7]) + (dword(Block[6]) shl 8) + (dword(Block[5]) shl 16) + (dword(Block[4]) shl 24);
    end;
  end;

  if IVector= nil then
  begin
    FillChar(IV,Sizeof(IV),$FF);
    {$IFDEF CFORM}Encrypt(IV,IV){$ELSE}BlowfishEncryptECB(Data,IV,IV){$ENDIF};
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
procedure TDCP_blowfish.Burn;
{$ELSE}
procedure BlowfishBurn;
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  FillChar(SBox,Sizeof(SBox),$FF);
  FillChar(PBox,Sizeof(PBox),$FF);
  FillChar(IV,Sizeof(IV),$FF);
  FillChar(LB,Sizeof(LB),$FF);
  {$IFDEF CFORM}
  fInitialized:= false;
  {$ELSE}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_blowfish.Reset;
{$ELSE}
procedure BlowfishReset;
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do{$ENDIF}
  Move(IV,LB,Sizeof(LB));
end;

{$IFDEF CFORM}
procedure TDCP_blowfish.EncryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('Blowfish: Not initialized');
  Encrypt(InBlock,OutBlock);
end;

procedure TDCP_blowfish.DecryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('Blowfish: Not initialized');
  Decrypt(InBlock,OutBlock);
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_blowfish.EncryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure BlowfishEncryptCBC;
{$ENDIF}
var
  TB: array[0..7] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Blowfish: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 8) do
  begin
    XorBlock(pointer(longint(@InData)+((i-1)*8)),@LB,@TB,Sizeof(TB));
    {$IFDEF CFORM}Encrypt(TB,TB){$ELSE}BlowfishEncryptECB(Data,TB,TB){$ENDIF};
    Move(TB,pointer(longint(@OutData)+((i-1)*8))^,Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 8)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}BlowfishEncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 8))^,@pointer(longint(@OutData)+Size-(Size mod 8))^,Size mod 8);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_blowfish.DecryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure BlowfishDecryptCBC;
{$ENDIF}
var
  TB: array[0..7] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Blowfish: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 8) do
  begin
    Move(pointer(longint(@InData)+((i-1)*8))^,TB,Sizeof(TB));
    {$IFDEF CFORM}
    Decrypt(pointer(longint(@InData)+((i-1)*8))^,pointer(longint(@OutData)+((i-1)*8))^);
    {$ELSE}
    BlowfishDecryptECB(Data,pointer(longint(@InData)+((i-1)*8))^,pointer(longint(@OutData)+((i-1)*8))^);
    {$ENDIF}
    XorBlock(@LB,pointer(longint(@OutData)+((i-1)*8)),pointer(longint(@OutData)+((i-1)*8)),Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 8)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}BlowfishEncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 8))^,@pointer(longint(@OutData)+Size-(Size mod 8))^,Size mod 8);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_blowfish.EncryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure BlowfishEncryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..7] of byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Blowfish: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}BlowfishEncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],7);
    LB[7]:= PByteArray(@OutData)^[i];
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_blowfish.DecryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure BlowfishDecryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..7] of byte;
  b: byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Blowfish: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    b:= PByteArray(@InData)^[i];
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}BlowfishEncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],7);
    LB[7]:= b;
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;


end.
