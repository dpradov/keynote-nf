{******************************************************************************}
{** A binary compatible implementation of Haval *******************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}
unit Haval;

interface
{$I DCPcrypt.Inc}
uses
  {$IFDEF CFORM}Classes, Sysutils, {$ENDIF}DCPcrypt;

{$IFDEF CFORM}
type
  TDCP_haval= class(TDCP_hash)
  protected
    LenHi, LenLo: DWord;
    Index: DWord;
    CurrentHash: array[0..7] of DWord;
    HashBuffer: array[0..127] of byte;
    procedure Compress;
    procedure UpdateLen(Len: DWord);
    procedure SetHashSize(Value: longint); override;
  public
    procedure Init; override;
    procedure Burn; override;
    procedure Update(const Buffer; Size: longint); override;
    procedure Final(var Digest); override;
    constructor Create(AOwner: TComponent); override;
  end;
{$ELSE}
type
  THavalData= record
    fHashSize: DWord;
    LenHi, LenLo: DWord;
    Index: DWord;
    CurrentHash: array[0..7] of DWord;
    HashBuffer: array[0..127] of byte;
  end;

procedure HavalInit(var Data: THavalData);
procedure HavalUpdate(var Data: THavalData; const Buffer; Size: longint);
procedure HavalFinal(var Data: THavalData; var Digest);
procedure HavalBurn(var Data: THavalData);
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
implementation

{$IFDEF CFORM}
constructor TDCP_haval.Create;
begin
  inherited Create(AOwner);
  fAlgorithm:= 'Haval';
  fHashSize:= 256;
  fID:= 14;
end;
{$ENDIF}

procedure FF_1(var x7, x6, x5, x4, x3, x2, x1, x0: dword; const w: dword);
var
  Temp: dword;
begin
  Temp:= ((x2) and ((x6) xor (x1)) xor (x5) and (x4) xor (x0) and (x3) xor (x6));
  x7:= RRot32(temp,7) + RRot32(x7,11) + w;
end;
procedure FF_2(var x7, x6, x5, x4, x3, x2, x1, x0: dword; const w, c: dword);
var
  Temp: dword;
begin
  Temp:= (x3 and (x4 and (not x0) xor x1 and x2 xor x6 xor x5) xor x1 and (x4 xor x2) xor x0 and x2 xor x5);
  x7:= RRot32(temp,7) + RRot32(x7,11) + w + c;
end;
procedure FF_3(var x7, x6, x5, x4, x3, x2, x1, x0: dword; const w, c: dword);
var
  Temp: dword;
begin
  Temp:= ((x4) and ((x1) and (x3) xor (x2) xor (x5)) xor (x1) and (x0) xor (x3) and (x6) xor (x5));
  x7:= RRot32(temp,7) + RRot32(x7,11) + w + c;
end;
procedure FF_4(var x7, x6, x5, x4, x3, x2, x1, x0: dword; const w, c: dword);
var
  Temp: dword;
begin
  Temp:= (x3 and (x5 and (not x0) xor x2 and (not x1) xor x4 xor x1 xor x6) xor x2 and
    (x4 and x0 xor x5 xor x1) xor x0 and x1 xor x6);
  x7:= RRot32(temp,7) + RRot32(x7,11) + w + c;
end;
procedure FF_5(var x7, x6, x5, x4, x3, x2, x1, x0: dword; const w, c: dword);
var
  Temp: dword;
begin
  Temp:= (x1 and (x3 and x4 and x6 xor (not x5)) xor x3 and x0 xor x4 and x5 xor x6 and x2);
  x7:= RRot32(temp,7) + RRot32(x7,11) + w + c;
end;

{$IFDEF CFORM}
procedure TDCP_haval.Compress;
{$ELSE}
procedure HavalCompress(var Data: THavalData);
{$ENDIF}
var
  t7, t6, t5, t4, t3, t2, t1, t0: DWord;
  W: array[0..31] of DWord;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  t0:= CurrentHash[0];
  t1:= CurrentHash[1];
  t2:= CurrentHash[2];
  t3:= CurrentHash[3];
  t4:= CurrentHash[4];
  t5:= CurrentHash[5];
  t6:= CurrentHash[6];
  t7:= CurrentHash[7];
  Move(HashBuffer,W,Sizeof(W));
  FF_1(t7, t6, t5, t4, t3, t2, t1, t0, w[ 0]);
  FF_1(t6, t5, t4, t3, t2, t1, t0, t7, w[ 1]);
  FF_1(t5, t4, t3, t2, t1, t0, t7, t6, w[ 2]);
  FF_1(t4, t3, t2, t1, t0, t7, t6, t5, w[ 3]);
  FF_1(t3, t2, t1, t0, t7, t6, t5, t4, w[ 4]);
  FF_1(t2, t1, t0, t7, t6, t5, t4, t3, w[ 5]);
  FF_1(t1, t0, t7, t6, t5, t4, t3, t2, w[ 6]);
  FF_1(t0, t7, t6, t5, t4, t3, t2, t1, w[ 7]);

  FF_1(t7, t6, t5, t4, t3, t2, t1, t0, w[ 8]);
  FF_1(t6, t5, t4, t3, t2, t1, t0, t7, w[ 9]);
  FF_1(t5, t4, t3, t2, t1, t0, t7, t6, w[10]);
  FF_1(t4, t3, t2, t1, t0, t7, t6, t5, w[11]);
  FF_1(t3, t2, t1, t0, t7, t6, t5, t4, w[12]);
  FF_1(t2, t1, t0, t7, t6, t5, t4, t3, w[13]);
  FF_1(t1, t0, t7, t6, t5, t4, t3, t2, w[14]);
  FF_1(t0, t7, t6, t5, t4, t3, t2, t1, w[15]);

  FF_1(t7, t6, t5, t4, t3, t2, t1, t0, w[16]);
  FF_1(t6, t5, t4, t3, t2, t1, t0, t7, w[17]);
  FF_1(t5, t4, t3, t2, t1, t0, t7, t6, w[18]);
  FF_1(t4, t3, t2, t1, t0, t7, t6, t5, w[19]);
  FF_1(t3, t2, t1, t0, t7, t6, t5, t4, w[20]);
  FF_1(t2, t1, t0, t7, t6, t5, t4, t3, w[21]);
  FF_1(t1, t0, t7, t6, t5, t4, t3, t2, w[22]);
  FF_1(t0, t7, t6, t5, t4, t3, t2, t1, w[23]);

  FF_1(t7, t6, t5, t4, t3, t2, t1, t0, w[24]);
  FF_1(t6, t5, t4, t3, t2, t1, t0, t7, w[25]);
  FF_1(t5, t4, t3, t2, t1, t0, t7, t6, w[26]);
  FF_1(t4, t3, t2, t1, t0, t7, t6, t5, w[27]);
  FF_1(t3, t2, t1, t0, t7, t6, t5, t4, w[28]);
  FF_1(t2, t1, t0, t7, t6, t5, t4, t3, w[29]);
  FF_1(t1, t0, t7, t6, t5, t4, t3, t2, w[30]);
  FF_1(t0, t7, t6, t5, t4, t3, t2, t1, w[31]);

  FF_2(t7, t6, t5, t4, t3, t2, t1, t0, w[ 5], $452821E6);
  FF_2(t6, t5, t4, t3, t2, t1, t0, t7, w[14], $38D01377);
  FF_2(t5, t4, t3, t2, t1, t0, t7, t6, w[26], $BE5466CF);
  FF_2(t4, t3, t2, t1, t0, t7, t6, t5, w[18], $34E90C6C);
  FF_2(t3, t2, t1, t0, t7, t6, t5, t4, w[11], $C0AC29B7);
  FF_2(t2, t1, t0, t7, t6, t5, t4, t3, w[28], $C97C50DD);
  FF_2(t1, t0, t7, t6, t5, t4, t3, t2, w[ 7], $3F84D5B5);
  FF_2(t0, t7, t6, t5, t4, t3, t2, t1, w[16], $B5470917);

  FF_2(t7, t6, t5, t4, t3, t2, t1, t0, w[ 0], $9216D5D9);
  FF_2(t6, t5, t4, t3, t2, t1, t0, t7, w[23], $8979FB1B);
  FF_2(t5, t4, t3, t2, t1, t0, t7, t6, w[20], $D1310BA6);
  FF_2(t4, t3, t2, t1, t0, t7, t6, t5, w[22], $98DFB5AC);
  FF_2(t3, t2, t1, t0, t7, t6, t5, t4, w[ 1], $2FFD72DB);
  FF_2(t2, t1, t0, t7, t6, t5, t4, t3, w[10], $D01ADFB7);
  FF_2(t1, t0, t7, t6, t5, t4, t3, t2, w[ 4], $B8E1AFED);
  FF_2(t0, t7, t6, t5, t4, t3, t2, t1, w[ 8], $6A267E96);

  FF_2(t7, t6, t5, t4, t3, t2, t1, t0, w[30], $BA7C9045);
  FF_2(t6, t5, t4, t3, t2, t1, t0, t7, w[ 3], $F12C7F99);
  FF_2(t5, t4, t3, t2, t1, t0, t7, t6, w[21], $24A19947);
  FF_2(t4, t3, t2, t1, t0, t7, t6, t5, w[ 9], $B3916CF7);
  FF_2(t3, t2, t1, t0, t7, t6, t5, t4, w[17], $0801F2E2);
  FF_2(t2, t1, t0, t7, t6, t5, t4, t3, w[24], $858EFC16);
  FF_2(t1, t0, t7, t6, t5, t4, t3, t2, w[29], $636920D8);
  FF_2(t0, t7, t6, t5, t4, t3, t2, t1, w[ 6], $71574E69);

  FF_2(t7, t6, t5, t4, t3, t2, t1, t0, w[19], $A458FEA3);
  FF_2(t6, t5, t4, t3, t2, t1, t0, t7, w[12], $F4933D7E);
  FF_2(t5, t4, t3, t2, t1, t0, t7, t6, w[15], $0D95748F);
  FF_2(t4, t3, t2, t1, t0, t7, t6, t5, w[13], $728EB658);
  FF_2(t3, t2, t1, t0, t7, t6, t5, t4, w[ 2], $718BCD58);
  FF_2(t2, t1, t0, t7, t6, t5, t4, t3, w[25], $82154AEE);
  FF_2(t1, t0, t7, t6, t5, t4, t3, t2, w[31], $7B54A41D);
  FF_2(t0, t7, t6, t5, t4, t3, t2, t1, w[27], $C25A59B5);

  FF_3(t7, t6, t5, t4, t3, t2, t1, t0, w[19], $9C30D539);
  FF_3(t6, t5, t4, t3, t2, t1, t0, t7, w[ 9], $2AF26013);
  FF_3(t5, t4, t3, t2, t1, t0, t7, t6, w[ 4], $C5D1B023);
  FF_3(t4, t3, t2, t1, t0, t7, t6, t5, w[20], $286085F0);
  FF_3(t3, t2, t1, t0, t7, t6, t5, t4, w[28], $CA417918);
  FF_3(t2, t1, t0, t7, t6, t5, t4, t3, w[17], $B8DB38EF);
  FF_3(t1, t0, t7, t6, t5, t4, t3, t2, w[ 8], $8E79DCB0);
  FF_3(t0, t7, t6, t5, t4, t3, t2, t1, w[22], $603A180E);

  FF_3(t7, t6, t5, t4, t3, t2, t1, t0, w[29], $6C9E0E8B);
  FF_3(t6, t5, t4, t3, t2, t1, t0, t7, w[14], $B01E8A3E);
  FF_3(t5, t4, t3, t2, t1, t0, t7, t6, w[25], $D71577C1);
  FF_3(t4, t3, t2, t1, t0, t7, t6, t5, w[12], $BD314B27);
  FF_3(t3, t2, t1, t0, t7, t6, t5, t4, w[24], $78AF2FDA);
  FF_3(t2, t1, t0, t7, t6, t5, t4, t3, w[30], $55605C60);
  FF_3(t1, t0, t7, t6, t5, t4, t3, t2, w[16], $E65525F3);
  FF_3(t0, t7, t6, t5, t4, t3, t2, t1, w[26], $AA55AB94);

  FF_3(t7, t6, t5, t4, t3, t2, t1, t0, w[31], $57489862);
  FF_3(t6, t5, t4, t3, t2, t1, t0, t7, w[15], $63E81440);
  FF_3(t5, t4, t3, t2, t1, t0, t7, t6, w[ 7], $55CA396A);
  FF_3(t4, t3, t2, t1, t0, t7, t6, t5, w[ 3], $2AAB10B6);
  FF_3(t3, t2, t1, t0, t7, t6, t5, t4, w[ 1], $B4CC5C34);
  FF_3(t2, t1, t0, t7, t6, t5, t4, t3, w[ 0], $1141E8CE);
  FF_3(t1, t0, t7, t6, t5, t4, t3, t2, w[18], $A15486AF);
  FF_3(t0, t7, t6, t5, t4, t3, t2, t1, w[27], $7C72E993);

  FF_3(t7, t6, t5, t4, t3, t2, t1, t0, w[13], $B3EE1411);
  FF_3(t6, t5, t4, t3, t2, t1, t0, t7, w[ 6], $636FBC2A);
  FF_3(t5, t4, t3, t2, t1, t0, t7, t6, w[21], $2BA9C55D);
  FF_3(t4, t3, t2, t1, t0, t7, t6, t5, w[10], $741831F6);
  FF_3(t3, t2, t1, t0, t7, t6, t5, t4, w[23], $CE5C3E16);
  FF_3(t2, t1, t0, t7, t6, t5, t4, t3, w[11], $9B87931E);
  FF_3(t1, t0, t7, t6, t5, t4, t3, t2, w[ 5], $AFD6BA33);
  FF_3(t0, t7, t6, t5, t4, t3, t2, t1, w[ 2], $6C24CF5C);

  FF_4(t7, t6, t5, t4, t3, t2, t1, t0, w[24], $7A325381);
  FF_4(t6, t5, t4, t3, t2, t1, t0, t7, w[ 4], $28958677);
  FF_4(t5, t4, t3, t2, t1, t0, t7, t6, w[ 0], $3B8F4898);
  FF_4(t4, t3, t2, t1, t0, t7, t6, t5, w[14], $6B4BB9AF);
  FF_4(t3, t2, t1, t0, t7, t6, t5, t4, w[ 2], $C4BFE81B);
  FF_4(t2, t1, t0, t7, t6, t5, t4, t3, w[ 7], $66282193);
  FF_4(t1, t0, t7, t6, t5, t4, t3, t2, w[28], $61D809CC);
  FF_4(t0, t7, t6, t5, t4, t3, t2, t1, w[23], $FB21A991);

  FF_4(t7, t6, t5, t4, t3, t2, t1, t0, w[26], $487CAC60);
  FF_4(t6, t5, t4, t3, t2, t1, t0, t7, w[ 6], $5DEC8032);
  FF_4(t5, t4, t3, t2, t1, t0, t7, t6, w[30], $EF845D5D);
  FF_4(t4, t3, t2, t1, t0, t7, t6, t5, w[20], $E98575B1);
  FF_4(t3, t2, t1, t0, t7, t6, t5, t4, w[18], $DC262302);
  FF_4(t2, t1, t0, t7, t6, t5, t4, t3, w[25], $EB651B88);
  FF_4(t1, t0, t7, t6, t5, t4, t3, t2, w[19], $23893E81);
  FF_4(t0, t7, t6, t5, t4, t3, t2, t1, w[ 3], $D396ACC5);

  FF_4(t7, t6, t5, t4, t3, t2, t1, t0, w[22], $0F6D6FF3);
  FF_4(t6, t5, t4, t3, t2, t1, t0, t7, w[11], $83F44239);
  FF_4(t5, t4, t3, t2, t1, t0, t7, t6, w[31], $2E0B4482);
  FF_4(t4, t3, t2, t1, t0, t7, t6, t5, w[21], $A4842004);
  FF_4(t3, t2, t1, t0, t7, t6, t5, t4, w[ 8], $69C8F04A);
  FF_4(t2, t1, t0, t7, t6, t5, t4, t3, w[27], $9E1F9B5E);
  FF_4(t1, t0, t7, t6, t5, t4, t3, t2, w[12], $21C66842);
  FF_4(t0, t7, t6, t5, t4, t3, t2, t1, w[ 9], $F6E96C9A);

  FF_4(t7, t6, t5, t4, t3, t2, t1, t0, w[ 1], $670C9C61);
  FF_4(t6, t5, t4, t3, t2, t1, t0, t7, w[29], $ABD388F0);
  FF_4(t5, t4, t3, t2, t1, t0, t7, t6, w[ 5], $6A51A0D2);
  FF_4(t4, t3, t2, t1, t0, t7, t6, t5, w[15], $D8542F68);
  FF_4(t3, t2, t1, t0, t7, t6, t5, t4, w[17], $960FA728);
  FF_4(t2, t1, t0, t7, t6, t5, t4, t3, w[10], $AB5133A3);
  FF_4(t1, t0, t7, t6, t5, t4, t3, t2, w[16], $6EEF0B6C);
  FF_4(t0, t7, t6, t5, t4, t3, t2, t1, w[13], $137A3BE4);

  FF_5(t7, t6, t5, t4, t3, t2, t1, t0, w[27], $BA3BF050);
  FF_5(t6, t5, t4, t3, t2, t1, t0, t7, w[ 3], $7EFB2A98);
  FF_5(t5, t4, t3, t2, t1, t0, t7, t6, w[21], $A1F1651D);
  FF_5(t4, t3, t2, t1, t0, t7, t6, t5, w[26], $39AF0176);
  FF_5(t3, t2, t1, t0, t7, t6, t5, t4, w[17], $66CA593E);
  FF_5(t2, t1, t0, t7, t6, t5, t4, t3, w[11], $82430E88);
  FF_5(t1, t0, t7, t6, t5, t4, t3, t2, w[20], $8CEE8619);
  FF_5(t0, t7, t6, t5, t4, t3, t2, t1, w[29], $456F9FB4);

  FF_5(t7, t6, t5, t4, t3, t2, t1, t0, w[19], $7D84A5C3);
  FF_5(t6, t5, t4, t3, t2, t1, t0, t7, w[ 0], $3B8B5EBE);
  FF_5(t5, t4, t3, t2, t1, t0, t7, t6, w[12], $E06F75D8);
  FF_5(t4, t3, t2, t1, t0, t7, t6, t5, w[ 7], $85C12073);
  FF_5(t3, t2, t1, t0, t7, t6, t5, t4, w[13], $401A449F);
  FF_5(t2, t1, t0, t7, t6, t5, t4, t3, w[ 8], $56C16AA6);
  FF_5(t1, t0, t7, t6, t5, t4, t3, t2, w[31], $4ED3AA62);
  FF_5(t0, t7, t6, t5, t4, t3, t2, t1, w[10], $363F7706);

  FF_5(t7, t6, t5, t4, t3, t2, t1, t0, w[ 5], $1BFEDF72);
  FF_5(t6, t5, t4, t3, t2, t1, t0, t7, w[ 9], $429B023D);
  FF_5(t5, t4, t3, t2, t1, t0, t7, t6, w[14], $37D0D724);
  FF_5(t4, t3, t2, t1, t0, t7, t6, t5, w[30], $D00A1248);
  FF_5(t3, t2, t1, t0, t7, t6, t5, t4, w[18], $DB0FEAD3);
  FF_5(t2, t1, t0, t7, t6, t5, t4, t3, w[ 6], $49F1C09B);
  FF_5(t1, t0, t7, t6, t5, t4, t3, t2, w[28], $075372C9);
  FF_5(t0, t7, t6, t5, t4, t3, t2, t1, w[24], $80991B7B);

  FF_5(t7, t6, t5, t4, t3, t2, t1, t0, w[ 2], $25D479D8);
  FF_5(t6, t5, t4, t3, t2, t1, t0, t7, w[23], $F6E8DEF7);
  FF_5(t5, t4, t3, t2, t1, t0, t7, t6, w[16], $E3FE501A);
  FF_5(t4, t3, t2, t1, t0, t7, t6, t5, w[22], $B6794C3B);
  FF_5(t3, t2, t1, t0, t7, t6, t5, t4, w[ 4], $976CE0BD);
  FF_5(t2, t1, t0, t7, t6, t5, t4, t3, w[ 1], $04C006BA);
  FF_5(t1, t0, t7, t6, t5, t4, t3, t2, w[25], $C1A94FB6);
  FF_5(t0, t7, t6, t5, t4, t3, t2, t1, w[15], $409F60C4);

  Inc(CurrentHash[0],t0);
  Inc(CurrentHash[1],t1);
  Inc(CurrentHash[2],t2);
  Inc(CurrentHash[3],t3);
  Inc(CurrentHash[4],t4);
  Inc(CurrentHash[5],t5);
  Inc(CurrentHash[6],t6);
  Inc(CurrentHash[7],t7);
  FillChar(W,Sizeof(W),0);
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_haval.UpdateLen;
{$ELSE}
procedure HavalUpdateLen(var Data: THavalData; Len: DWord);
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
procedure TDCP_haval.SetHashSize(Value: longint);
begin
  if Value= fHashSize then
    Exit;
  if Value<= 128 then
    fHashSize:= 128
  else if Value<= 160 then
    fHashSize:= 160
  else if Value<= 192 then
    fHashSize:= 192
  else if Value<= 224 then
    fHashSize:= 224
  else
    fHashSize:= 256;
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_haval.Init;
{$ELSE}
procedure HavalInit;
{$ENDIF}
begin
  {$IFDEF CFORM}
  Burn;
  {$ELSE}
  HavalBurn(Data);
  with Data do begin
  fHashSize:= 256;
  {$ENDIF}
  CurrentHash[0]:= $243F6A88;
  CurrentHash[1]:= $85A308D3;
  CurrentHash[2]:= $13198A2E;
  CurrentHash[3]:= $03707344;
  CurrentHash[4]:= $A4093822;
  CurrentHash[5]:= $299F31D0;
  CurrentHash[6]:= $082EFA98;
  CurrentHash[7]:= $EC4E6C89;
  {$IFDEF CFORM}
  fInitialized:= true;
  {$ELSE}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_haval.Burn;
{$ELSE}
procedure HavalBurn;
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
procedure TDCP_haval.Update(const Buffer; Size: longint);
{$ELSE}
procedure HavalUpdate;
{$ENDIF}
var
  PBuf: ^byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Haval: Not initialized');
  UpdateLen(Size);
  {$ELSE}
  HavalUpdateLen(Data,Size);
  with Data do begin
  {$ENDIF}
  PBuf:= @Buffer;
  while Size> 0 do
  begin
    if (Sizeof(HashBuffer)-Index)<= DWord(Size) then
    begin
      Move(PBuf^,HashBuffer[Index],Sizeof(HashBuffer)-Index);
      Dec(Size,Sizeof(HashBuffer)-Index);
      Inc(PBuf,Sizeof(HashBuffer)-Index);
      {$IFDEF CFORM}
      Compress;
      {$ELSE}
      HavalCompress(Data);
      {$ENDIF}
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
procedure TDCP_haval.Final(var Digest);
{$ELSE}
procedure HavalFinal;
{$ENDIF}
var
  Temp: DWord;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('Haval: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  HashBuffer[Index]:= $80;
  if Index> 118 then
    {$IFDEF CFORM}Compress;{$ELSE}HavalCompress(Data);{$ENDIF}
  HashBuffer[118]:= ((fHashSize and 3) shl 6) or (5 shl 3) or 1;
  HashBuffer[119]:= (fHashSize shr 2) and $FF;
  Move(LenLo,HashBuffer[120],Sizeof(LenLo));
  Move(LenHi,HashBuffer[124],Sizeof(LenHi));
  {$IFDEF CFORM}Compress;{$ELSE}HavalCompress(Data);{$ENDIF}
  if fHashSize= 128 then
  begin
    temp:= (CurrentHash[7] and $000000FF) or
           (CurrentHash[6] and $FF000000) or
           (CurrentHash[5] and $00FF0000) or
           (CurrentHash[4] and $0000FF00);
    Inc(CurrentHash[0],RRot32(temp,  8));
    temp:= (CurrentHash[7] and $0000FF00) or
           (CurrentHash[6] and $000000FF) or
           (CurrentHash[5] and $FF000000) or
           (CurrentHash[4] and $00FF0000);
    Inc(CurrentHash[1],RRot32(temp, 16));
    temp := (CurrentHash[7] and $00FF0000) or
            (CurrentHash[6] and $0000FF00) or
            (CurrentHash[5] and $000000FF) or
            (CurrentHash[4] and $FF000000);
    Inc(CurrentHash[2],RRot32(temp, 24));
    temp:= (CurrentHash[7] and $FF000000) or
           (CurrentHash[6] and $00FF0000) or
           (CurrentHash[5] and $0000FF00) or
           (CurrentHash[4] and $000000FF);
    Inc(CurrentHash[3],temp);
  end
  else if fHashSize= 160 then
  begin
    temp:= (CurrentHash[7] and  $3F) or
           (CurrentHash[6] and ($7F shl 25)) or
           (CurrentHash[5] and ($3F shl 19));
    Inc(CurrentHash[0],RRot32(temp, 19));
    temp:= (CurrentHash[7] and ($3F shl  6)) or
           (CurrentHash[6] and  $3F) or
           (CurrentHash[5] and ($7F shl 25));
    Inc(CurrentHash[1],RRot32(temp, 25));
    temp:= (CurrentHash[7] and ($7F shl 12)) or
           (CurrentHash[6] and ($3F shl  6)) or
           (CurrentHash[5] and  $3F);
    Inc(CurrentHash[2],temp);
    temp:= (CurrentHash[7] and ($3F shl 19)) or
           (CurrentHash[6] and ($7F shl 12)) or
           (CurrentHash[5] and ($3F shl  6));
    Inc(CurrentHash[3],temp shr 6);
    temp:= (CurrentHash[7] and ($7F shl 25)) or
           (CurrentHash[6] and ($3F shl 19)) or
           (CurrentHash[5] and ($7F shl 12));
    Inc(CurrentHash[4],temp shr 12);
  end
  else if fHashSize= 192 then
  begin
    temp:= (CurrentHash[7] and  $1F) or
           (CurrentHash[6] and ($3F shl 26));
    Inc(CurrentHash[0],RRot32(temp, 26));
    temp:= (CurrentHash[7] and ($1F shl  5)) or
           (CurrentHash[6] and  $1F);
    Inc(CurrentHash[1],temp);
    temp:= (CurrentHash[7] and ($3F shl 10)) or
           (CurrentHash[6] and ($1F shl  5));
    Inc(CurrentHash[2],temp shr 5);
    temp:= (CurrentHash[7] and ($1F shl 16)) or
           (CurrentHash[6] and ($3F shl 10));
    Inc(CurrentHash[3],temp shr 10);
    temp:= (CurrentHash[7] and ($1F shl 21)) or
           (CurrentHash[6] and ($1F shl 16));
    Inc(CurrentHash[4],temp shr 16);
    temp:= (CurrentHash[7] and ($3F shl 26)) or
           (CurrentHash[6] and ($1F shl 21));
    Inc(CurrentHash[5],temp shr 21);
  end
  else if fHashSize= 224 then
  begin
    Inc(CurrentHash[0],(CurrentHash[7] shr 27) and $1F);
    Inc(CurrentHash[1],(CurrentHash[7] shr 22) and $1F);
    Inc(CurrentHash[2],(CurrentHash[7] shr 18) and $0F);
    Inc(CurrentHash[3],(CurrentHash[7] shr 13) and $1F);
    Inc(CurrentHash[4],(CurrentHash[7] shr  9) and $0F);
    Inc(CurrentHash[5],(CurrentHash[7] shr  4) and $1F);
    Inc(CurrentHash[6], CurrentHash[7]         and $0F);
  end;
  Move(CurrentHash,Digest,fHashSize div 8);
  {$IFDEF CFORM}Burn;{$ELSE}HavalBurn(Data); end;{$ENDIF}
end;

end.
