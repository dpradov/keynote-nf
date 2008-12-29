{******************************************************************************}
{** A binary compatible implementation of RipeMD-160 **************************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}
unit RMD160;

interface
{$I DCPcrypt.Inc}
uses
  {$IFDEF CFORM}Classes, Sysutils, {$ENDIF}DCPcrypt;

{$IFDEF CFORM}
type
  TDCP_rmd160= class(TDCP_hash)
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
  TRmd160Data= record
    LenHi, LenLo: DWord;
    Index: DWord;
    CurrentHash: array[0..4] of DWord;
    HashBuffer: array[0..63] of byte;
  end;

procedure Rmd160Init(var Data: TRmd160Data);
procedure Rmd160Update(var Data: TRmd160Data; const Buffer; Size: longint);
procedure Rmd160Final(var Data: TRmd160Data; var Digest);
procedure Rmd160Burn(var Data: TRmd160Data);
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
implementation

{$IFDEF CFORM}
constructor TDCP_rmd160.Create;
begin
  inherited Create(AOwner);
  fAlgorithm:= 'RipeMD-160';
  fHashSize:= 160;
  fID:= 10;
end;
{$ENDIF}

{$IFDEF CFORM}
procedure TDCP_rmd160.Compress;
{$ELSE}
procedure Rmd160Compress(var Data: TRmd160Data);
{$ENDIF}
var
  aa, bb, cc, dd, ee, aaa, bbb, ccc, ddd, eee: DWord;
  X: array[0..15] of DWord;
begin
  {$IFNDEF CFORM}with Data do begin{$ENDIF}
  Move(HashBuffer,X,Sizeof(X));
  aa:= CurrentHash[0];
  aaa:= CurrentHash[0];
  bb:= CurrentHash[1];
  bbb:= CurrentHash[1];
  cc:= CurrentHash[2];
  ccc:= CurrentHash[2];
  dd:= CurrentHash[3];
  ddd:= CurrentHash[3];
  ee:= CurrentHash[4];
  eee:= CurrentHash[4];

  aa:= aa + (bb xor cc xor dd) + X[ 0];
  aa:= LRot32(aa,11) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + (aa xor bb xor cc) + X[ 1];
  ee:= LRot32(ee,14) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + (ee xor aa xor bb) + X[ 2];
  dd:= LRot32(dd,15) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + (dd xor ee xor aa) + X[ 3];
  cc:= LRot32(cc,12) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + (cc xor dd xor ee) + X[ 4];
  bb:= LRot32(bb, 5) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + (bb xor cc xor dd) + X[ 5];
  aa:= LRot32(aa, 8) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + (aa xor bb xor cc) + X[ 6];
  ee:= LRot32(ee, 7) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + (ee xor aa xor bb) + X[ 7];
  dd:= LRot32(dd, 9) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + (dd xor ee xor aa) + X[ 8];
  cc:= LRot32(cc,11) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + (cc xor dd xor ee) + X[ 9];
  bb:= LRot32(bb,13) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + (bb xor cc xor dd) + X[10];
  aa:= LRot32(aa,14) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + (aa xor bb xor cc) + X[11];
  ee:= LRot32(ee,15) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + (ee xor aa xor bb) + X[12];
  dd:= LRot32(dd, 6) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + (dd xor ee xor aa) + X[13];
  cc:= LRot32(cc, 7) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + (cc xor dd xor ee) + X[14];
  bb:= LRot32(bb, 9) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + (bb xor cc xor dd) + X[15];
  aa:= LRot32(aa, 8) + ee;
  cc:= LRot32(cc,10);

  ee:= ee + ((aa and bb) or ((not aa) and cc)) + X[ 7] + $5a827999;
  ee:= LRot32(ee, 7) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + ((ee and aa) or ((not ee) and bb)) + X[ 4] + $5a827999;
  dd:= LRot32(dd, 6) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + ((dd and ee) or ((not dd) and aa)) + X[13] + $5a827999;
  cc:= LRot32(cc, 8) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + ((cc and dd) or ((not cc) and ee)) + X[ 1] + $5a827999;
  bb:= LRot32(bb,13) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + ((bb and cc) or ((not bb) and dd)) + X[10] + $5a827999;
  aa:= LRot32(aa,11) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + ((aa and bb) or ((not aa) and cc)) + X[ 6] + $5a827999;
  ee:= LRot32(ee, 9) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + ((ee and aa) or ((not ee) and bb)) + X[15] + $5a827999;
  dd:= LRot32(dd, 7) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + ((dd and ee) or ((not dd) and aa)) + X[ 3] + $5a827999;
  cc:= LRot32(cc,15) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + ((cc and dd) or ((not cc) and ee)) + X[12] + $5a827999;
  bb:= LRot32(bb, 7) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + ((bb and cc) or ((not bb) and dd)) + X[ 0] + $5a827999;
  aa:= LRot32(aa,12) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + ((aa and bb) or ((not aa) and cc)) + X[ 9] + $5a827999;
  ee:= LRot32(ee,15) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + ((ee and aa) or ((not ee) and bb)) + X[ 5] + $5a827999;
  dd:= LRot32(dd, 9) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + ((dd and ee) or ((not dd) and aa)) + X[ 2] + $5a827999;
  cc:= LRot32(cc,11) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + ((cc and dd) or ((not cc) and ee)) + X[14] + $5a827999;
  bb:= LRot32(bb, 7) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + ((bb and cc) or ((not bb) and dd)) + X[11] + $5a827999;
  aa:= LRot32(aa,13) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + ((aa and bb) or ((not aa) and cc)) + X[ 8] + $5a827999;
  ee:= LRot32(ee,12) + dd;
  bb:= LRot32(bb,10);

  dd:= dd + ((ee or (not aa)) xor bb) + X[ 3] + $6ed9eba1;
  dd:= LRot32(dd,11) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + ((dd or (not ee)) xor aa) + X[10] + $6ed9eba1;
  cc:= LRot32(cc,13) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + ((cc or (not dd)) xor ee) + X[14] + $6ed9eba1;
  bb:= LRot32(bb, 6) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + ((bb or (not cc)) xor dd) + X[ 4] + $6ed9eba1;
  aa:= LRot32(aa, 7) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + ((aa or (not bb)) xor cc) + X[ 9] + $6ed9eba1;
  ee:= LRot32(ee,14) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + ((ee or (not aa)) xor bb) + X[15] + $6ed9eba1;
  dd:= LRot32(dd, 9) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + ((dd or (not ee)) xor aa) + X[ 8] + $6ed9eba1;
  cc:= LRot32(cc,13) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + ((cc or (not dd)) xor ee) + X[ 1] + $6ed9eba1;
  bb:= LRot32(bb,15) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + ((bb or (not cc)) xor dd) + X[ 2] + $6ed9eba1;
  aa:= LRot32(aa,14) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + ((aa or (not bb)) xor cc) + X[ 7] + $6ed9eba1;
  ee:= LRot32(ee, 8) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + ((ee or (not aa)) xor bb) + X[ 0] + $6ed9eba1;
  dd:= LRot32(dd,13) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + ((dd or (not ee)) xor aa) + X[ 6] + $6ed9eba1;
  cc:= LRot32(cc, 6) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + ((cc or (not dd)) xor ee) + X[13] + $6ed9eba1;
  bb:= LRot32(bb, 5) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + ((bb or (not cc)) xor dd) + X[11] + $6ed9eba1;
  aa:= LRot32(aa,12) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + ((aa or (not bb)) xor cc) + X[ 5] + $6ed9eba1;
  ee:= LRot32(ee, 7) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + ((ee or (not aa)) xor bb) + X[12] + $6ed9eba1;
  dd:= LRot32(dd, 5) + cc;
  aa:= LRot32(aa,10);

  cc:= cc + ((dd and aa) or (ee and (not aa))) + X[ 1] + $8f1bbcdc;
  cc:= LRot32(cc,11) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + ((cc and ee) or (dd and (not ee))) + X[ 9] + $8f1bbcdc;
  bb:= LRot32(bb,12) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + ((bb and dd) or (cc and (not dd))) + X[11] + $8f1bbcdc;
  aa:= LRot32(aa,14) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + ((aa and cc) or (bb and (not cc))) + X[10] + $8f1bbcdc;
  ee:= LRot32(ee,15) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + ((ee and bb) or (aa and (not bb))) + X[ 0] + $8f1bbcdc;
  dd:= LRot32(dd,14) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + ((dd and aa) or (ee and (not aa))) + X[ 8] + $8f1bbcdc;
  cc:= LRot32(cc,15) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + ((cc and ee) or (dd and (not ee))) + X[12] + $8f1bbcdc;
  bb:= LRot32(bb, 9) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + ((bb and dd) or (cc and (not dd))) + X[ 4] + $8f1bbcdc;
  aa:= LRot32(aa, 8) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + ((aa and cc) or (bb and (not cc))) + X[13] + $8f1bbcdc;
  ee:= LRot32(ee, 9) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + ((ee and bb) or (aa and (not bb))) + X[ 3] + $8f1bbcdc;
  dd:= LRot32(dd,14) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + ((dd and aa) or (ee and (not aa))) + X[ 7] + $8f1bbcdc;
  cc:= LRot32(cc, 5) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + ((cc and ee) or (dd and (not ee))) + X[15] + $8f1bbcdc;
  bb:= LRot32(bb, 6) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + ((bb and dd) or (cc and (not dd))) + X[14] + $8f1bbcdc;
  aa:= LRot32(aa, 8) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + ((aa and cc) or (bb and (not cc))) + X[ 5] + $8f1bbcdc;
  ee:= LRot32(ee, 6) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + ((ee and bb) or (aa and (not bb))) + X[ 6] + $8f1bbcdc;
  dd:= LRot32(dd, 5) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + ((dd and aa) or (ee and (not aa))) + X[ 2] + $8f1bbcdc;
  cc:= LRot32(cc,12) + bb;
  ee:= LRot32(ee,10);

  bb:= bb + (cc xor (dd or (not ee))) + X[ 4] + $a953fd4e;
  bb:= LRot32(bb, 9) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + (bb xor (cc or (not dd))) + X[ 0] + $a953fd4e;
  aa:= LRot32(aa,15) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + (aa xor (bb or (not cc))) + X[ 5] + $a953fd4e;
  ee:= LRot32(ee, 5) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + (ee xor (aa or (not bb))) + X[ 9] + $a953fd4e;
  dd:= LRot32(dd,11) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + (dd xor (ee or (not aa))) + X[ 7] + $a953fd4e;
  cc:= LRot32(cc, 6) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + (cc xor (dd or (not ee))) + X[12] + $a953fd4e;
  bb:= LRot32(bb, 8) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + (bb xor (cc or (not dd))) + X[ 2] + $a953fd4e;
  aa:= LRot32(aa,13) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + (aa xor (bb or (not cc))) + X[10] + $a953fd4e;
  ee:= LRot32(ee,12) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + (ee xor (aa or (not bb))) + X[14] + $a953fd4e;
  dd:= LRot32(dd, 5) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + (dd xor (ee or (not aa))) + X[ 1] + $a953fd4e;
  cc:= LRot32(cc,12) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + (cc xor (dd or (not ee))) + X[ 3] + $a953fd4e;
  bb:= LRot32(bb,13) + aa;
  dd:= LRot32(dd,10);
  aa:= aa + (bb xor (cc or (not dd))) + X[ 8] + $a953fd4e;
  aa:= LRot32(aa,14) + ee;
  cc:= LRot32(cc,10);
  ee:= ee + (aa xor (bb or (not cc))) + X[11] + $a953fd4e;
  ee:= LRot32(ee,11) + dd;
  bb:= LRot32(bb,10);
  dd:= dd + (ee xor (aa or (not bb))) + X[ 6] + $a953fd4e;
  dd:= LRot32(dd, 8) + cc;
  aa:= LRot32(aa,10);
  cc:= cc + (dd xor (ee or (not aa))) + X[15] + $a953fd4e;
  cc:= LRot32(cc, 5) + bb;
  ee:= LRot32(ee,10);
  bb:= bb + (cc xor (dd or (not ee))) + X[13] + $a953fd4e;
  bb:= LRot32(bb, 6) + aa;
  dd:= LRot32(dd,10);

  aaa:= aaa + (bbb xor (ccc or (not ddd))) + X[ 5] + $50a28be6;
  aaa:= LRot32(aaa, 8) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + (aaa xor (bbb or (not ccc))) + X[14] + $50a28be6;
  eee:= LRot32(eee, 9) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + (eee xor (aaa or (not bbb))) + X[ 7] + $50a28be6;
  ddd:= LRot32(ddd, 9) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + (ddd xor (eee or (not aaa))) + X[ 0] + $50a28be6;
  ccc:= LRot32(ccc,11) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + (ccc xor (ddd or (not eee))) + X[ 9] + $50a28be6;
  bbb:= LRot32(bbb,13) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + (bbb xor (ccc or (not ddd))) + X[ 2] + $50a28be6;
  aaa:= LRot32(aaa,15) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + (aaa xor (bbb or (not ccc))) + X[11] + $50a28be6;
  eee:= LRot32(eee,15) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + (eee xor (aaa or (not bbb))) + X[ 4] + $50a28be6;
  ddd:= LRot32(ddd, 5) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + (ddd xor (eee or (not aaa))) + X[13] + $50a28be6;
  ccc:= LRot32(ccc, 7) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + (ccc xor (ddd or (not eee))) + X[ 6] + $50a28be6;
  bbb:= LRot32(bbb, 7) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + (bbb xor (ccc or (not ddd))) + X[15] + $50a28be6;
  aaa:= LRot32(aaa, 8) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + (aaa xor (bbb or (not ccc))) + X[ 8] + $50a28be6;
  eee:= LRot32(eee,11) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + (eee xor (aaa or (not bbb))) + X[ 1] + $50a28be6;
  ddd:= LRot32(ddd,14) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + (ddd xor (eee or (not aaa))) + X[10] + $50a28be6;
  ccc:= LRot32(ccc,14) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + (ccc xor (ddd or (not eee))) + X[ 3] + $50a28be6;
  bbb:= LRot32(bbb,12) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + (bbb xor (ccc or (not ddd))) + X[12] + $50a28be6;
  aaa:= LRot32(aaa, 6) + eee;
  ccc:= LRot32(ccc,10);

  eee:= eee + ((aaa and ccc) or (bbb and (not ccc))) + X[ 6] + $5c4dd124;
  eee:= LRot32(eee, 9) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + ((eee and bbb) or (aaa and (not bbb))) + X[11] + $5c4dd124;
  ddd:= LRot32(ddd,13) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + ((ddd and aaa) or (eee and (not aaa))) + X[ 3] + $5c4dd124;
  ccc:= LRot32(ccc,15) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + ((ccc and eee) or (ddd and (not eee))) + X[ 7] + $5c4dd124;
  bbb:= LRot32(bbb, 7) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + ((bbb and ddd) or (ccc and (not ddd))) + X[ 0] + $5c4dd124;
  aaa:= LRot32(aaa,12) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + ((aaa and ccc) or (bbb and (not ccc))) + X[13] + $5c4dd124;
  eee:= LRot32(eee, 8) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + ((eee and bbb) or (aaa and (not bbb))) + X[ 5] + $5c4dd124;
  ddd:= LRot32(ddd, 9) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + ((ddd and aaa) or (eee and (not aaa))) + X[10] + $5c4dd124;
  ccc:= LRot32(ccc,11) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + ((ccc and eee) or (ddd and (not eee))) + X[14] + $5c4dd124;
  bbb:= LRot32(bbb, 7) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + ((bbb and ddd) or (ccc and (not ddd))) + X[15] + $5c4dd124;
  aaa:= LRot32(aaa, 7) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + ((aaa and ccc) or (bbb and (not ccc))) + X[ 8] + $5c4dd124;
  eee:= LRot32(eee,12) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + ((eee and bbb) or (aaa and (not bbb))) + X[12] + $5c4dd124;
  ddd:= LRot32(ddd, 7) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + ((ddd and aaa) or (eee and (not aaa))) + X[ 4] + $5c4dd124;
  ccc:= LRot32(ccc, 6) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + ((ccc and eee) or (ddd and (not eee))) + X[ 9] + $5c4dd124;
  bbb:= LRot32(bbb,15) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + ((bbb and ddd) or (ccc and (not ddd))) + X[ 1] + $5c4dd124;
  aaa:= LRot32(aaa,13) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + ((aaa and ccc) or (bbb and (not ccc))) + X[ 2] + $5c4dd124;
  eee:= LRot32(eee,11) + ddd;
  bbb:= LRot32(bbb,10);

  ddd:= ddd + ((eee or (not aaa)) xor bbb) + X[15] + $6d703ef3;
  ddd:= LRot32(ddd, 9) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + ((ddd or (not eee)) xor aaa) + X[ 5] + $6d703ef3;
  ccc:= LRot32(ccc, 7) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + ((ccc or (not ddd)) xor eee) + X[ 1] + $6d703ef3;
  bbb:= LRot32(bbb,15) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + ((bbb or (not ccc)) xor ddd) + X[ 3] + $6d703ef3;
  aaa:= LRot32(aaa,11) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + ((aaa or (not bbb)) xor ccc) + X[ 7] + $6d703ef3;
  eee:= LRot32(eee, 8) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + ((eee or (not aaa)) xor bbb) + X[14] + $6d703ef3;
  ddd:= LRot32(ddd, 6) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + ((ddd or (not eee)) xor aaa) + X[ 6] + $6d703ef3;
  ccc:= LRot32(ccc, 6) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + ((ccc or (not ddd)) xor eee) + X[ 9] + $6d703ef3;
  bbb:= LRot32(bbb,14) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + ((bbb or (not ccc)) xor ddd) + X[11] + $6d703ef3;
  aaa:= LRot32(aaa,12) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + ((aaa or (not bbb)) xor ccc) + X[ 8] + $6d703ef3;
  eee:= LRot32(eee,13) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + ((eee or (not aaa)) xor bbb) + X[12] + $6d703ef3;
  ddd:= LRot32(ddd, 5) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + ((ddd or (not eee)) xor aaa) + X[ 2] + $6d703ef3;
  ccc:= LRot32(ccc,14) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + ((ccc or (not ddd)) xor eee) + X[10] + $6d703ef3;
  bbb:= LRot32(bbb,13) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + ((bbb or (not ccc)) xor ddd) + X[ 0] + $6d703ef3;
  aaa:= LRot32(aaa,13) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + ((aaa or (not bbb)) xor ccc) + X[ 4] + $6d703ef3;
  eee:= LRot32(eee, 7) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + ((eee or (not aaa)) xor bbb) + X[13] + $6d703ef3;
  ddd:= LRot32(ddd, 5) + ccc;
  aaa:= LRot32(aaa,10);

  ccc:= ccc + ((ddd and eee) or ((not ddd) and aaa)) + X[ 8] + $7a6d76e9;
  ccc:= LRot32(ccc,15) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + ((ccc and ddd) or ((not ccc) and eee)) + X[ 6] + $7a6d76e9;
  bbb:= LRot32(bbb, 5) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + ((bbb and ccc) or ((not bbb) and ddd)) + X[ 4] + $7a6d76e9;
  aaa:= LRot32(aaa, 8) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + ((aaa and bbb) or ((not aaa) and ccc)) + X[ 1] + $7a6d76e9;
  eee:= LRot32(eee,11) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + ((eee and aaa) or ((not eee) and bbb)) + X[ 3] + $7a6d76e9;
  ddd:= LRot32(ddd,14) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + ((ddd and eee) or ((not ddd) and aaa)) + X[11] + $7a6d76e9;
  ccc:= LRot32(ccc,14) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + ((ccc and ddd) or ((not ccc) and eee)) + X[15] + $7a6d76e9;
  bbb:= LRot32(bbb, 6) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + ((bbb and ccc) or ((not bbb) and ddd)) + X[ 0] + $7a6d76e9;
  aaa:= LRot32(aaa,14) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + ((aaa and bbb) or ((not aaa) and ccc)) + X[ 5] + $7a6d76e9;
  eee:= LRot32(eee, 6) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + ((eee and aaa) or ((not eee) and bbb)) + X[12] + $7a6d76e9;
  ddd:= LRot32(ddd, 9) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + ((ddd and eee) or ((not ddd) and aaa)) + X[ 2] + $7a6d76e9;
  ccc:= LRot32(ccc,12) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + ((ccc and ddd) or ((not ccc) and eee)) + X[13] + $7a6d76e9;
  bbb:= LRot32(bbb, 9) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + ((bbb and ccc) or ((not bbb) and ddd)) + X[ 9] + $7a6d76e9;
  aaa:= LRot32(aaa,12) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + ((aaa and bbb) or ((not aaa) and ccc)) + X[ 7] + $7a6d76e9;
  eee:= LRot32(eee, 5) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + ((eee and aaa) or ((not eee) and bbb)) + X[10] + $7a6d76e9;
  ddd:= LRot32(ddd,15) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + ((ddd and eee) or ((not ddd) and aaa)) + X[14] + $7a6d76e9;
  ccc:= LRot32(ccc, 8) + bbb;
  eee:= LRot32(eee,10);

  bbb:= bbb + (ccc xor ddd xor eee) + X[12];
  bbb:= LRot32(bbb, 8) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + (bbb xor ccc xor ddd) + X[15];
  aaa:= LRot32(aaa, 5) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + (aaa xor bbb xor ccc) + X[10];
  eee:= LRot32(eee,12) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + (eee xor aaa xor bbb) + X[ 4];
  ddd:= LRot32(ddd, 9) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + (ddd xor eee xor aaa) + X[ 1];
  ccc:= LRot32(ccc,12) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + (ccc xor ddd xor eee) + X[ 5];
  bbb:= LRot32(bbb, 5) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + (bbb xor ccc xor ddd) + X[ 8];
  aaa:= LRot32(aaa,14) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + (aaa xor bbb xor ccc) + X[ 7];
  eee:= LRot32(eee, 6) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + (eee xor aaa xor bbb) + X[ 6];
  ddd:= LRot32(ddd, 8) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + (ddd xor eee xor aaa) + X[ 2];
  ccc:= LRot32(ccc,13) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + (ccc xor ddd xor eee) + X[13];
  bbb:= LRot32(bbb, 6) + aaa;
  ddd:= LRot32(ddd,10);
  aaa:= aaa + (bbb xor ccc xor ddd) + X[14];
  aaa:= LRot32(aaa, 5) + eee;
  ccc:= LRot32(ccc,10);
  eee:= eee + (aaa xor bbb xor ccc) + X[ 0];
  eee:= LRot32(eee,15) + ddd;
  bbb:= LRot32(bbb,10);
  ddd:= ddd + (eee xor aaa xor bbb) + X[ 3];
  ddd:= LRot32(ddd,13) + ccc;
  aaa:= LRot32(aaa,10);
  ccc:= ccc + (ddd xor eee xor aaa) + X[ 9];
  ccc:= LRot32(ccc,11) + bbb;
  eee:= LRot32(eee,10);
  bbb:= bbb + (ccc xor ddd xor eee) + X[11];
  bbb:= LRot32(bbb,11) + aaa;
  ddd:= LRot32(ddd,10);

  ddd:= ddd + cc + CurrentHash[1];
  CurrentHash[1]:= CurrentHash[2] + dd + eee;
  CurrentHash[2]:= CurrentHash[3] + ee + aaa;
  CurrentHash[3]:= CurrentHash[4] + aa + bbb;
  CurrentHash[4]:= CurrentHash[0] + bb + ccc;
  CurrentHash[0]:= ddd;
  FillChar(X,Sizeof(X),0);
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
  {$IFNDEF CFORM}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rmd160.UpdateLen;
{$ELSE}
procedure Rmd160UpdateLen(var Data: TRmd160Data; Len: DWord);
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
procedure TDCP_rmd160.Init;
{$ELSE}
procedure Rmd160Init;
{$ENDIF}
begin
  {$IFDEF CFORM}
  Burn;
  {$ELSE}
  with Data do begin
  Rmd160Burn(Data);{$ENDIF}
  CurrentHash[0]:= $67452301;
  CurrentHash[1]:= $efcdab89;
  CurrentHash[2]:= $98badcfe;
  CurrentHash[3]:= $10325476;
  CurrentHash[4]:= $c3d2e1f0;
  {$IFDEF CFORM}
  fInitialized:= true;
  {$ELSE}end;{$ENDIF}
end;

{$IFDEF CFORM}
procedure TDCP_rmd160.Burn;
{$ELSE}
procedure Rmd160Burn;
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
procedure TDCP_rmd160.Update(const Buffer; Size: longint);
{$ELSE}
procedure Rmd160Update;
{$ENDIF}
var
  PBuf: ^byte;
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('RMD160: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  {$IFNDEF CFORM}
  Rmd160UpdateLen(Data,Size);
  {$ELSE}
  UpdateLen(Size);
  {$ENDIF}
  PBuf:= @Buffer;
  while Size> 0 do
  begin
    if (Sizeof(HashBuffer)-Index)<= DWord(Size) then
    begin
      Move(PBuf^,HashBuffer[Index],Sizeof(HashBuffer)-Index);
      Dec(Size,Sizeof(HashBuffer)-Index);
      Inc(PBuf,Sizeof(HashBuffer)-Index);
      {$IFDEF CFORM}Compress;{$ELSE}Rmd160Compress(Data);{$ENDIF}
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
procedure TDCP_rmd160.Final(var Digest);
{$ELSE}
procedure Rmd160Final;
{$ENDIF}
begin
  {$IFDEF CFORM}
  if not fInitialized then
    raise Exception.Create('RMD160: Not initialized');
  {$ELSE}with Data do begin{$ENDIF}
  HashBuffer[Index]:= $80;
  if Index>= 56 then
    {$IFDEF CFORM}Compress;{$ELSE}Rmd160Compress(Data);{$ENDIF}
  PDWord(@HashBuffer[56])^:= LenLo;
  PDWord(@HashBuffer[60])^:= LenHi;
  {$IFDEF CFORM}Compress;{$ELSE}Rmd160Compress(Data);{$ENDIF}
  Move(CurrentHash,Digest,Sizeof(CurrentHash));
  {$IFDEF CFORM}Burn;{$ELSE}Rmd160Burn(Data); end;{$ENDIF}
end;

end.
