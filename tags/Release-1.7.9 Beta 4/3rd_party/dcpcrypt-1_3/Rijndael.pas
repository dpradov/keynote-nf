{******************************************************************************}
{** A binary compatible implementation of Rijndael ****************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}
unit Rijndael;

interface
{$I DCPcrypt.Inc}
uses
  {$IFDEF CFORM}Classes, Sysutils, {$ENDIF}DCPcrypt;

const
  BC= 4;
  MAXROUNDS= 14;

{$IFDEF CFORM}
type
  TDCP_rijndael= class(TDCP_blockcipher)
  protected
    IV, LB: array[0..15] of byte;
    numrounds: longint;
    rk, drk: array[0..MAXROUNDS,0..7] of DWord;
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
  TRijndaelData= record
    IV, LB: array[0..15] of byte;
    numrounds: longint;
    rk, drk: array[0..MAXROUNDS,0..7] of DWord;
  end;

procedure RijndaelInit(var Data: TRijndaelData; var Key; Size: longint; IVector: pointer);
procedure RijndaelReset(var Data: TRijndaelData);
procedure RijndaelBurn(var Data:  TRijndaelData);
procedure RijndaelEncryptECB(var Data: TRijndaelData; const InBlock; var OutBlock);
procedure RijndaelDecryptECB(var Data: TRijndaelData; const InBlock; var OutBlock);
procedure RijndaelEncryptCBC(var Data: TRijndaelData; const InData; var OutData; Size: longint);
procedure RijndaelDecryptCBC(var Data: TRijndaelData; const InData; var OutData; Size: longint);
procedure RijndaelEncryptCFB(var Data: TRijndaelData; const InData; var OutData; Size: longint);
procedure RijndaelDecryptCFB(var Data: TRijndaelData; const InData; var OutData; Size: longint);
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
implementation

{$I Rijndael.Inc}

const
  MAXBC= 8;
  MAXKC= 8;

{$IFDEF CFORM}
constructor TDCP_rijndael.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAlgorithm:= 'Rijndael';
  fBlockSize:= 128;
  fMaxKeySize:= 256;
  fID:= 9;
  Burn;
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_rijndael.Encrypt(const InBlock; var OutBlock);
{$ELSE}
procedure RijndaelEncryptECB;
{$ENDIF}
var
  r: longint;
  temp: array[0..MAXBC-1,0..3] of byte;
  a: array[0..MAXBC,0..3] of byte;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,a,16);
  for r:= 0 to (numrounds-2) do
  begin
    PDWord(@temp[0])^:= PDWord(@a[0])^ xor rk[r,0];
    PDWord(@temp[1])^:= PDWord(@a[1])^ xor rk[r,1];
    PDWord(@temp[2])^:= PDWord(@a[2])^ xor rk[r,2];
    PDWord(@temp[3])^:= PDWord(@a[3])^ xor rk[r,3];
    PDWord(@a[0])^:= PDWord(@T1[temp[0,0]])^ xor
                     PDWord(@T2[temp[1,1]])^ xor
                     PDWord(@T3[temp[2,2]])^ xor
                     PDWord(@T4[temp[3,3]])^;
    PDWord(@a[1])^:= PDWord(@T1[temp[1,0]])^ xor
                     PDWord(@T2[temp[2,1]])^ xor
                     PDWord(@T3[temp[3,2]])^ xor
                     PDWord(@T4[temp[0,3]])^;
    PDWord(@a[2])^:= PDWord(@T1[temp[2,0]])^ xor
                     PDWord(@T2[temp[3,1]])^ xor
                     PDWord(@T3[temp[0,2]])^ xor
                     PDWord(@T4[temp[1,3]])^;
    PDWord(@a[3])^:= PDWord(@T1[temp[3,0]])^ xor
                     PDWord(@T2[temp[0,1]])^ xor
                     PDWord(@T3[temp[1,2]])^ xor
                     PDWord(@T4[temp[2,3]])^;
  end;
  PDWord(@temp[0])^:= PDWord(@a[0])^ xor rk[numrounds-1,0];
  PDWord(@temp[1])^:= PDWord(@a[1])^ xor rk[numrounds-1,1];
  PDWord(@temp[2])^:= PDWord(@a[2])^ xor rk[numrounds-1,2];
  PDWord(@temp[3])^:= PDWord(@a[3])^ xor rk[numrounds-1,3];
  a[0,0]:= T1[temp[0,0],1];
  a[0,1]:= T1[temp[1,1],1];
  a[0,2]:= T1[temp[2,2],1];
  a[0,3]:= T1[temp[3,3],1];
  a[1,0]:= T1[temp[1,0],1];
  a[1,1]:= T1[temp[2,1],1];
  a[1,2]:= T1[temp[3,2],1];
  a[1,3]:= T1[temp[0,3],1];
  a[2,0]:= T1[temp[2,0],1];
  a[2,1]:= T1[temp[3,1],1];
  a[2,2]:= T1[temp[0,2],1];
  a[2,3]:= T1[temp[1,3],1];
  a[3,0]:= T1[temp[3,0],1];
  a[3,1]:= T1[temp[0,1],1];
  a[3,2]:= T1[temp[1,2],1];
  a[3,3]:= T1[temp[2,3],1];
  PDWord(@a[0])^:= PDWord(@a[0])^ xor rk[numrounds,0];
  PDWord(@a[1])^:= PDWord(@a[1])^ xor rk[numrounds,1];
  PDWord(@a[2])^:= PDWord(@a[2])^ xor rk[numrounds,2];
  PDWord(@a[3])^:= PDWord(@a[3])^ xor rk[numrounds,3];
  Move(a,OutBlock,16);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rijndael.Decrypt(const InBlock; var OutBlock);
{$ELSE}
procedure RijndaelDecryptECB;
{$ENDIF}
var
  r: longint;
  temp: array[0..MAXBC-1,0..3] of byte;
  a: array[0..MAXBC,0..3] of byte;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,a,16);
  for r:= NumRounds downto 2 do
  begin
    PDWord(@temp[0])^:= PDWord(@a[0])^ xor drk[r,0];
    PDWord(@temp[1])^:= PDWord(@a[1])^ xor drk[r,1];
    PDWord(@temp[2])^:= PDWord(@a[2])^ xor drk[r,2];
    PDWord(@temp[3])^:= PDWord(@a[3])^ xor drk[r,3];
    PDWord(@a[0])^:= PDWord(@T5[temp[0,0]])^ xor
                     PDWord(@T6[temp[3,1]])^ xor
                     PDWord(@T7[temp[2,2]])^ xor
                     PDWord(@T8[temp[1,3]])^;
    PDWord(@a[1])^:= PDWord(@T5[temp[1,0]])^ xor
                     PDWord(@T6[temp[0,1]])^ xor
                     PDWord(@T7[temp[3,2]])^ xor
                     PDWord(@T8[temp[2,3]])^;
    PDWord(@a[2])^:= PDWord(@T5[temp[2,0]])^ xor
                     PDWord(@T6[temp[1,1]])^ xor
                     PDWord(@T7[temp[0,2]])^ xor
                     PDWord(@T8[temp[3,3]])^;
    PDWord(@a[3])^:= PDWord(@T5[temp[3,0]])^ xor
                     PDWord(@T6[temp[2,1]])^ xor
                     PDWord(@T7[temp[1,2]])^ xor
                     PDWord(@T8[temp[0,3]])^;
  end;
  PDWord(@temp[0])^:= PDWord(@a[0])^ xor drk[1,0];
  PDWord(@temp[1])^:= PDWord(@a[1])^ xor drk[1,1];
  PDWord(@temp[2])^:= PDWord(@a[2])^ xor drk[1,2];
  PDWord(@temp[3])^:= PDWord(@a[3])^ xor drk[1,3];
  a[0,0]:= S5[temp[0,0]];
  a[0,1]:= S5[temp[3,1]];
  a[0,2]:= S5[temp[2,2]];
  a[0,3]:= S5[temp[1,3]];
  a[1,0]:= S5[temp[1,0]];
  a[1,1]:= S5[temp[0,1]];
  a[1,2]:= S5[temp[3,2]];
  a[1,3]:= S5[temp[2,3]];
  a[2,0]:= S5[temp[2,0]];
  a[2,1]:= S5[temp[1,1]];
  a[2,2]:= S5[temp[0,2]];
  a[2,3]:= S5[temp[3,3]];
  a[3,0]:= S5[temp[3,0]];
  a[3,1]:= S5[temp[2,1]];
  a[3,2]:= S5[temp[1,2]];
  a[3,3]:= S5[temp[0,3]];
  PDWord(@a[0])^:= PDWord(@a[0])^ xor drk[0,0];
  PDWord(@a[1])^:= PDWord(@a[1])^ xor drk[0,1];
  PDWord(@a[2])^:= PDWord(@a[2])^ xor drk[0,2];
  PDWord(@a[3])^:= PDWord(@a[3])^ xor drk[0,3];
  Move(a,OutBlock,16);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

procedure InvMixColumn(a: PByteArray; BC: byte);
var
  j: longint;
begin
  for j:= 0 to (BC-1) do
    PDWord(@(a^[j*4]))^:= PDWord(@U1[a^[j*4+0]])^ xor
                       PDWord(@U2[a^[j*4+1]])^ xor
                       PDWord(@U3[a^[j*4+2]])^ xor
                       PDWord(@U4[a^[j*4+3]])^;
end;

{$IFDEF CFORM}
procedure TDCP_rijndael.Init(var Key; Size: longint; IVector: pointer);
{$ELSE}
procedure RijndaelInit;
{$ENDIF}
var
  KC, ROUNDS, j, r, t, rconpointer: longint;
  tk: array[0..MAXKC-1,0..3] of byte;
begin
  {$IFDEF CFORM}
  if fInitialized then
    Burn;
  if (Size> fMaxKeySize) or (Size<= 0) or ((Size mod 8)<> 0) then
    raise Exception.Create(Format('Rijndael: Invalid key size - %d',[Size]));
  {$ELSE}
  if (Size> 256) or (Size<= 0) or ((Size mod 8)<> 0) then
    Exit;
  with Data do begin
  {$ENDIF}

  Size:= Size div 8;

  FillChar(tk,Sizeof(tk),0);
  Move(Key,tk,Size);
  if Size<= 16 then
  begin
    KC:= 4;
    Rounds:= 10;
  end
  else if Size<= 24 then
  begin
    KC:= 6;
    Rounds:= 12;
  end
  else
  begin
    KC:= 8;
    Rounds:= 14;
  end;
  numrounds:= rounds;
  r:= 0;
  t:= 0;
  j:= 0;
  while (j< KC) and (r< (rounds+1)) do
  begin
    while (j< KC) and (t< BC) do
    begin
      rk[r,t]:= PDWord(@tk[j])^;
      Inc(j);
      Inc(t);
    end;
    if t= BC then
    begin
      t:= 0;
      Inc(r);
    end;
  end;
  rconpointer:= 0;
  while (r< (rounds+1)) do
  begin
    tk[0,0]:= tk[0,0] xor S[tk[KC-1,1]];
    tk[0,1]:= tk[0,1] xor S[tk[KC-1,2]];
    tk[0,2]:= tk[0,2] xor S[tk[KC-1,3]];
    tk[0,3]:= tk[0,3] xor S[tk[KC-1,0]];
    tk[0,0]:= tk[0,0] xor rcon[rconpointer];
    Inc(rconpointer);
    if KC<> 8 then
    begin
      for j:= 1 to (KC-1) do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
    end
    else
    begin
      for j:= 1 to ((KC div 2)-1) do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
      tk[KC div 2,0]:= tk[KC div 2,0] xor S[tk[KC div 2 - 1,0]];
      tk[KC div 2,1]:= tk[KC div 2,1] xor S[tk[KC div 2 - 1,1]];
      tk[KC div 2,2]:= tk[KC div 2,2] xor S[tk[KC div 2 - 1,2]];
      tk[KC div 2,3]:= tk[KC div 2,3] xor S[tk[KC div 2 - 1,3]];
      for j:= ((KC div 2) + 1) to (KC-1) do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
    end;
    j:= 0;
    while (j< KC) and (r< (rounds+1)) do
    begin
      while (j< KC) and (t< BC) do
      begin
        rk[r,t]:= PDWord(@tk[j])^;
        Inc(j);
        Inc(t);
      end;
      if t= BC then
      begin
        Inc(r);
        t:= 0;
      end;
    end;
  end;
  Move(rk,drk,Sizeof(rk));
  for r:= 1 to (numrounds-1) do
    InvMixColumn(@drk[r],BC);

  if IVector= nil then
  begin
    FillChar(IV,Sizeof(IV),$FF);
    {$IFDEF CFORM}Encrypt(IV,IV){$ELSE}RijndaelEncryptECB(Data,IV,IV){$ENDIF};
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
procedure TDCP_rijndael.Burn;
{$ELSE}
procedure RijndaelBurn;
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  FillChar(rk,Sizeof(rk),$FF);
  FillChar(drk,Sizeof(drk),$FF);
  FillChar(IV,Sizeof(IV),$FF);
  FillChar(LB,Sizeof(LB),$FF);
  NumRounds:= 0;
  {$IFDEF CFORM}
  fInitialized:= false;
  {$ELSE}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rijndael.Reset;
{$ELSE}
procedure RijndaelReset;
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do{$ENDIF}
  Move(IV,LB,Sizeof(LB));
end;

{$IFDEF CFORM}
procedure TDCP_rijndael.EncryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('Rijndael: Not initialized');
  Encrypt(InBlock,OutBlock);
end;

procedure TDCP_rijndael.DecryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('Rijndael: Not initialized');
  Decrypt(InBlock,OutBlock);
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_rijndael.EncryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure RijndaelEncryptCBC;
{$ENDIF}
var
  TB: array[0..15] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Rijndael: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 16) do
  begin
    XorBlock(pointer(longint(@InData)+((i-1)*16)),@LB,@TB,Sizeof(TB));
    {$IFDEF CFORM}Encrypt(TB,TB){$ELSE}RijndaelEncryptECB(Data,TB,TB){$ENDIF};
    Move(TB,pointer(longint(@OutData)+((i-1)*16))^,Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 16)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RijndaelEncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 16))^,@pointer(longint(@OutData)+Size-(Size mod 16))^,Size mod 16);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rijndael.DecryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure RijndaelDecryptCBC;
{$ENDIF}
var
  TB: array[0..15] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Rijndael: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 16) do
  begin
    Move(pointer(longint(@InData)+((i-1)*16))^,TB,Sizeof(TB));
    {$IFDEF CFORM}
    Decrypt(pointer(longint(@InData)+((i-1)*16))^,pointer(longint(@OutData)+((i-1)*16))^);
    {$ELSE}
    RijndaelDecryptECB(Data,pointer(longint(@InData)+((i-1)*16))^,pointer(longint(@OutData)+((i-1)*16))^);
    {$ENDIF}
    XorBlock(@LB,pointer(longint(@OutData)+((i-1)*16)),pointer(longint(@OutData)+((i-1)*16)),Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 16)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RijndaelEncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 16))^,@pointer(longint(@OutData)+Size-(Size mod 16))^,Size mod 16);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rijndael.EncryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure RijndaelEncryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..15] of byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Rijndael: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RijndaelEncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],15);
    LB[15]:= PByteArray(@OutData)^[i];
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rijndael.DecryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure RijndaelDecryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..15] of byte;
  b: byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Rijndael: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    b:= PByteArray(@InData)^[i];
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RijndaelEncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],15);
    LB[15]:= b;
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;


end.
