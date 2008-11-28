{******************************************************************************}
{** A binary compatible implementation of RC2 *********************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}
unit RC2;

interface
{$I DCPcrypt.Inc}
uses
  {$IFDEF CFORM}Classes, Sysutils, {$ENDIF}DCPcrypt;

{$IFDEF CFORM}
type
  TDCP_rc2= class(TDCP_blockcipher)
  protected
    IV, LB: array[0..7] of byte;
    KeyData: array[0..63] of word;
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
  TRC2Data= record
    IV, LB: array[0..7] of byte;
    KeyData: array[0..63] of word;
  end;

procedure RC2Init(var Data: TRC2Data; var Key; Size: longint; IVector: pointer);
procedure RC2Reset(var Data: TRC2Data);
procedure RC2Burn(var Data:  TRC2Data);
procedure RC2EncryptECB(var Data: TRC2Data; const InBlock; var OutBlock);
procedure RC2DecryptECB(var Data: TRC2Data; const InBlock; var OutBlock);
procedure RC2EncryptCBC(var Data: TRC2Data; const InData; var OutData; Size: longint);
procedure RC2DecryptCBC(var Data: TRC2Data; const InData; var OutData; Size: longint);
procedure RC2EncryptCFB(var Data: TRC2Data; const InData; var OutData; Size: longint);
procedure RC2DecryptCFB(var Data: TRC2Data; const InData; var OutData; Size: longint);
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
implementation

{$I RC2.Inc}

{$IFDEF CFORM}
constructor TDCP_rc2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fID:= 1;
  fAlgorithm:= 'RC2';
  fBlockSize:= 64;
  fMaxKeySize:= 1024;
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_rc2.Encrypt(const InBlock; var OutBlock);
{$ELSE}
procedure RC2EncryptECB;
{$ENDIF}
var
  i, j: longint;
  w: array[0..3] of word;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,w,Sizeof(w));
  for i:= 0 to 15 do
  begin
    j:= i*4;
    w[0]:= LRot16((w[0]+(w[1] and (not w[3]))+(w[2] and w[3])+KeyData[j+0]),1);
    w[1]:= LRot16((w[1]+(w[2] and (not w[0]))+(w[3] and w[0])+KeyData[j+1]),2);
    w[2]:= LRot16((w[2]+(w[3] and (not w[1]))+(w[0] and w[1])+KeyData[j+2]),3);
    w[3]:= LRot16((w[3]+(w[0] and (not w[2]))+(w[1] and w[2])+KeyData[j+3]),5);
    if (i= 4) or (i= 10) then
    begin
      w[0]:= w[0]+KeyData[w[3] and 63];
      w[1]:= w[1]+KeyData[w[0] and 63];
      w[2]:= w[2]+KeyData[w[1] and 63];
      w[3]:= w[3]+KeyData[w[2] and 63];
    end;
  end;
  Move(w,OutBlock,Sizeof(w));
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc2.Decrypt(const InBlock; var OutBlock);
{$ELSE}
procedure RC2DecryptECB;
{$ENDIF}
var
  i, j: longint;
  w: array[0..3] of word;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(InBlock,w,Sizeof(w));
  for i:= 15 downto 0 do
  begin
    j:= i*4;
    w[3]:= RRot16(w[3],5)-(w[0] and (not w[2]))-(w[1] and w[2])-KeyData[j+3];
    w[2]:= RRot16(w[2],3)-(w[3] and (not w[1]))-(w[0] and w[1])-KeyData[j+2];
    w[1]:= RRot16(w[1],2)-(w[2] and (not w[0]))-(w[3] and w[0])-KeyData[j+1];
    w[0]:= RRot16(w[0],1)-(w[1] and (not w[3]))-(w[2] and w[3])-KeyData[j+0];
    if (i= 5) or (i= 11) then
    begin
      w[3]:= w[3]-KeyData[w[2] and 63];
      w[2]:= w[2]-KeyData[w[1] and 63];
      w[1]:= w[1]-KeyData[w[0] and 63];
      w[0]:= w[0]-KeyData[w[3] and 63];
    end;
  end;
  Move(w,OutBlock,Sizeof(w));
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc2.Init(var Key; Size: longint; IVector: pointer);
{$ELSE}
procedure RC2Init;
{$ENDIF}
var
  i: longint;
  KeyB: array[0..127] of byte;
begin
  {$IFDEF CFORM}
  if fInitialized then
    Burn;
  if (Size> fMaxKeySize) or (Size<= 0) or ((Size mod 8)<> 0) then
    raise Exception.Create(Format('RC2: Invalid key size - %d',[Size]));
  {$ELSE}
  if (Size> 1024) or (Size<= 0) or ((Size mod 8)<> 0) then
    Exit;
  with Data do begin
  {$ENDIF}
  Move(Key,KeyB,Size div 8);
  for i:= (Size div 8) to 127 do
    KeyB[i]:= sBox[(KeyB[i-(Size div 8)]+KeyB[i-1]) and $FF];
  KeyB[0]:= sBox[KeyB[0]];
  Move(KeyB,KeyData,Sizeof(KeyData));

  if IVector= nil then
  begin
    FillChar(IV,Sizeof(IV),$FF);
    {$IFDEF CFORM}Encrypt(IV,IV){$ELSE}RC2EncryptECB(Data,IV,IV){$ENDIF};
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
procedure TDCP_rc2.Burn;
{$ELSE}
procedure RC2Burn;
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
procedure TDCP_rc2.Reset;
{$ELSE}
procedure RC2Reset;
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do{$ENDIF}
  Move(IV,LB,Sizeof(LB));
end;

{$IFDEF CFORM}
procedure TDCP_rc2.EncryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('RC2: Not initialized');
  Encrypt(InBlock,OutBlock);
end;

procedure TDCP_rc2.DecryptECB(const InBlock; var OutBlock);
begin
  if not fInitialized then
    raise Exception.Create('RC2: Not initialized');
  Decrypt(InBlock,OutBlock);
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_rc2.EncryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure RC2EncryptCBC;
{$ENDIF}
var
  TB: array[0..7] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('RC2: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 8) do
  begin
    XorBlock(pointer(longint(@InData)+((i-1)*8)),@LB,@TB,Sizeof(TB));
    {$IFDEF CFORM}Encrypt(TB,TB){$ELSE}RC2EncryptECB(Data,TB,TB){$ENDIF};
    Move(TB,pointer(longint(@OutData)+((i-1)*8))^,Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 8)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RC2EncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 8))^,@pointer(longint(@OutData)+Size-(Size mod 8))^,Size mod 8);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc2.DecryptCBC(const InData; var OutData; Size: longint);
{$ELSE}
procedure RC2DecryptCBC;
{$ENDIF}
var
  TB: array[0..7] of byte;
  i: longint;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('RC2: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 1 to (Size div 8) do
  begin
    Move(pointer(longint(@InData)+((i-1)*8))^,TB,Sizeof(TB));
    {$IFDEF CFORM}
    Decrypt(pointer(longint(@InData)+((i-1)*8))^,pointer(longint(@OutData)+((i-1)*8))^);
    {$ELSE}
    RC2DecryptECB(Data,pointer(longint(@InData)+((i-1)*8))^,pointer(longint(@OutData)+((i-1)*8))^);
    {$ENDIF}
    XorBlock(@LB,pointer(longint(@OutData)+((i-1)*8)),pointer(longint(@OutData)+((i-1)*8)),Sizeof(TB));
    Move(TB,LB,Sizeof(TB));
  end;
  if (Size mod 8)<> 0 then
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RC2EncryptECB(Data,LB,TB){$ENDIF};
    XorBlock(@TB,@pointer(longint(@InData)+Size-(Size mod 8))^,@pointer(longint(@OutData)+Size-(Size mod 8))^,Size mod 8);
  end;
  FillChar(TB,Sizeof(TB),$FF);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc2.EncryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure RC2EncryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..7] of byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('RC2: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RC2EncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],7);
    LB[7]:= PByteArray(@OutData)^[i];
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rc2.DecryptCFB(const InData; var OutData; Size: longint);
{$ELSE}
procedure RC2DecryptCFB;
{$ENDIF}
var
  i: longint;
  TB: array[0..7] of byte;
  b: byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('RC2: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  for i:= 0 to Size-1 do
  begin
    b:= PByteArray(@InData)^[i];
    {$IFDEF CFORM}Encrypt(LB,TB){$ELSE}RC2EncryptECB(Data,LB,TB){$ENDIF};
    PByteArray(@OutData)^[i]:= PByteArray(@InData)^[i] xor TB[0];
    Move(LB[1],LB[0],7);
    LB[7]:= b;
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;


end.
