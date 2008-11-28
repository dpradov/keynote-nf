{******************************************************************************}
{** A binary compatible implementation of SHA1 ********************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}
unit SHA1;

interface
{$I DCPcrypt.Inc}
uses
  {$IFDEF CFORM}Classes, Sysutils, {$ENDIF}DCPcrypt;

{$IFDEF CFORM}
type
  TDCP_sha1= class(TDCP_hash)
  protected
    LenHi, LenLo: DWord;
    Index: DWord;
    CurrentHash: array[0..4] of DWord;
    HashBuffer: array[0..63] of byte;
    procedure Compress;
    procedure UpdateLen(Len: DWord);
  public
    procedure Init; override;
    procedure Burn; override;
    procedure Update(const Buffer; Size: longint); override;
    procedure Final(var Digest); override;
    constructor Create(AOwner: TComponent); override;
  end;
{$ELSE}
type
  TSHA1Data= record
    LenHi, LenLo: DWord;
    Index: DWord;
    CurrentHash: array[0..4] of DWord;
    HashBuffer: array[0..63] of byte;
  end;

procedure SHA1Init(var Data: TSHA1Data);
procedure SHA1Update(var Data: TSHA1Data; const Buffer; Size: longint);
procedure SHA1Final(var Data: TSHA1Data; var Digest);
procedure SHA1Burn(var Data: TSHA1Data);
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
implementation

{$IFDEF CFORM}
constructor TDCP_sha1.Create;
begin
  inherited Create(AOwner);
  fAlgorithm:= 'SHA1';
  fHashSize:= 160;
  fID:= 2;
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_sha1.Compress;
{$ELSE}
procedure SHA1Compress(var Data: TSHA1Data);
{$ENDIF}
var
  A, B, C, D, E, T: DWord;
  W: array[0..79] of DWord;
  i: longint;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Index:= 0;
  Move(HashBuffer,W,Sizeof(HashBuffer));
  for i:= 0 to 15 do
    W[i]:= (W[i] shr 24) or ((W[i] shr 8) and $FF00) or ((W[i] shl 8) and $FF0000) or (W[i] shl 24);
  for i:= 16 to 79 do
    W[i]:= LRot32(W[i-3] xor W[i-8] xor W[i-14] xor W[i-16],1);
  A:= CurrentHash[0]; B:= CurrentHash[1]; C:= CurrentHash[2]; D:= CurrentHash[3]; E:= CurrentHash[4];
  for i:= 0 to 19 do
  begin
    T:= LRot32(A,5) + (D xor (B and (C xor D))) + E + W[i] + $5A827999;
    E:= D; D:= C; C:= LRot32(B,30); B:= A; A:= T;
  end;
  for i:= 20 to 39 do
  begin
    T:= LRot32(A,5) + (B xor C xor D) + E + W[i] + $6ED9EBA1;
    E:= D; D:= C; C:= LRot32(B,30); B:= A; A:= T;
  end;
  for i:= 40 to 59 do
  begin
    T:= LRot32(A,5) + ((B and C) or (D and (B or C))) + E + W[i] + $8F1BBCDC;
    E:= D; D:= C; C:= LRot32(B,30); B:= A; A:= T;
  end;
  for i:= 60 to 79 do
  begin
    T:= LRot32(A,5) + (B xor C xor D) + E + W[i] + $CA62C1D6;
    E:= D; D:= C; C:= LRot32(B,30); B:= A; A:= T;
  end;
  CurrentHash[0]:= CurrentHash[0] + A;
  CurrentHash[1]:= CurrentHash[1] + B;
  CurrentHash[2]:= CurrentHash[2] + C;
  CurrentHash[3]:= CurrentHash[3] + D;
  CurrentHash[4]:= CurrentHash[4] + E;
  FillChar(W,Sizeof(W),0);
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_sha1.UpdateLen;
{$ELSE}
procedure SHA1UpdateLen(var Data: TSHA1Data; Len: DWord);
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Inc(LenLo,(Len shl 3));
  if LenLo< (Len shl 3) then
    Inc(LenHi);
  Inc(LenHi,Len shr 29);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_sha1.Init;
{$ELSE}
procedure SHA1Init;
{$ENDIF}
begin
  {$IFDEF CFORM}
  Burn;
  {$ELSE}
  with Data do begin
  SHA1Burn(Data);
  {$ENDIF}
  CurrentHash[0]:= $67452301;
  CurrentHash[1]:= $EFCDAB89;
  CurrentHash[2]:= $98BADCFE;
  CurrentHash[3]:= $10325476;
  CurrentHash[4]:= $C3D2E1F0;
  {$IFDEF CFORM}
  fInitialized:= true;
  {$ELSE}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_sha1.Burn;
{$ELSE}
procedure SHA1Burn;
{$ENDIF}
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  LenHi:= 0; LenLo:= 0;
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
  FillChar(CurrentHash,Sizeof(CurrentHash),0);
  {$IFDEF CFORM}
  fInitialized:= false;
  {$ELSE}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_sha1.Update(const Buffer; Size: longint);
{$ELSE}
procedure SHA1Update;
{$ENDIF}
var
  PBuf: ^byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('SHA1: Not initialized');
  UpdateLen(Size);
  {$ELSE}
  with Data do begin
  SHA1UpdateLen(Data,Size);
  {$ENDIF}
  PBuf:= @Buffer;
  while Size> 0 do
  begin
    if (Sizeof(HashBuffer)-Index)<= DWord(Size) then
    begin
      Move(PBuf^,HashBuffer[Index],Sizeof(HashBuffer)-Index);
      Dec(Size,Sizeof(HashBuffer)-Index);
      Inc(PBuf,Sizeof(HashBuffer)-Index);
      {$IFDEF CFORM}Compress;{$ELSE}SHA1Compress(Data);{$ENDIF}
    end
    else
    begin
      Move(PBuf^,HashBuffer[Index],Size);
      Inc(Index,Size);
      Size:= 0;
    end;
  end;
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_sha1.Final(var Digest);
{$ELSE}
procedure SHA1Final;
{$ENDIF}
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('SHA1: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  HashBuffer[Index]:= $80;
  if Index>= 56 then
    {$IFDEF CFORM}Compress;{$ELSE}SHA1Compress(Data);{$ENDIF}
  PDWord(@HashBuffer[56])^:= (LenHi shr 24) or ((LenHi shr 8) and $FF00) or ((LenHi shl 8) and $FF0000) or (LenHi shl 24);
  PDWord(@HashBuffer[60])^:= (LenLo shr 24) or ((LenLo shr 8) and $FF00) or ((LenLo shl 8) and $FF0000) or (LenLo shl 24);
  {$IFDEF CFORM}Compress;{$ELSE}SHA1Compress(Data);{$ENDIF}
  CurrentHash[0]:= (CurrentHash[0] shr 24) or ((CurrentHash[0] shr 8) and $FF00)
    or ((CurrentHash[0] shl 8) and $FF0000) or (CurrentHash[0] shl 24);
  CurrentHash[1]:= (CurrentHash[1] shr 24) or ((CurrentHash[1] shr 8) and $FF00)
    or ((CurrentHash[1] shl 8) and $FF0000) or (CurrentHash[1] shl 24);
  CurrentHash[2]:= (CurrentHash[2] shr 24) or ((CurrentHash[2] shr 8) and $FF00)
    or ((CurrentHash[2] shl 8) and $FF0000) or (CurrentHash[2] shl 24);
  CurrentHash[3]:= (CurrentHash[3] shr 24) or ((CurrentHash[3] shr 8) and $FF00)
    or ((CurrentHash[3] shl 8) and $FF0000) or (CurrentHash[3] shl 24);
  CurrentHash[4]:= (CurrentHash[4] shr 24) or ((CurrentHash[4] shr 8) and $FF00)
    or ((CurrentHash[4] shl 8) and $FF0000) or (CurrentHash[4] shl 24);
  Move(CurrentHash,Digest,Sizeof(CurrentHash));
  {$IFDEF CFORM}
  Burn;
  {$ELSE}SHA1Burn(Data); end;{$ENDIF}
end;

end.
