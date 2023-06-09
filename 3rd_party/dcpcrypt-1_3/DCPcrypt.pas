{******************************************************************************}
{** Base classes for block cipher and hash algorithm implementations **********}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}

(* -------------------------------------------------------------------------------
  + Changes by Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [dpv]

   >> Changes to original source code available in KeyNote NF project.
   >> Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
      in https://github.com/dpradov/keynote-nf
 ---------------
 - Changes for adpting to the use with Unicode versions of Delphi
    => string -> AnsiString
 ------------------------------------------------------------------------------ *)


unit DCPcrypt;

interface
{$I DCPcrypt.inc}
{$IFDEF CFORM}

{$IFDEF VER200} { Codegear Delphi 2009 12.x }
  {$DEFINE DELPHI_2009_UP}
{$ENDIF}


uses
   System.Classes;
{$ENDIF}

const
  DCPpage= 'DCPcrypt';

type
  PWord= ^Word;
  PDWord= ^DWord;
{$IFDEF VER120}
  DWord= longword;
{$ELSE}
  DWord= longint;
{$ENDIF}
  PDWordArray= ^TDWordArray;
  TDWordArray= array[0..1023] of DWord;
  PByteArray= ^TByteArray;
  TByteArray= array[0..4095] of byte;


{******************************************************************************}
{******************************************************************************}
{$IFDEF CFORM}
type
  TDCP_blockcipher= class(TComponent)
  protected
    fID: longint;
    fInitialized: boolean;
    fAlgorithm: AnsiString;     // [dpv]
    fBlockSize: longint;
    fMaxKeySize: longint;
    fNullStr: AnsiString;       // [dpv]
    fNullInt: longint;
  public
    property ID: longint
      read fID;
    property Initialized: boolean
      read fInitialized;
    procedure Init(var Key; Size: longint; IV: pointer); virtual; abstract;
    procedure InitStr(const Key: AnsiString);                                           // [dpv]
    procedure Burn; virtual; abstract;
    procedure Reset; virtual; abstract;
    procedure EncryptECB(const InBlock; var OutBlock); virtual; abstract;
    procedure DecryptECB(const InBlock; var OutBlock); virtual; abstract;
    procedure EncryptCBC(const InData; var OutData; Size: longint); virtual; abstract;
    procedure DecryptCBC(const InData; var OutData; Size: longint); virtual; abstract;
    procedure EncryptCFB(const InData; var OutData; Size: longint); virtual; abstract;
    procedure DecryptCFB(const InData; var OutData; Size: longint); virtual; abstract;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Algorithm: AnsiString
      read fAlgorithm write fNullStr;
    property BlockSize: longint
      read fBlockSize write fNullInt;
    property MaxKeySize: longint
      read fMaxKeySize write fNullInt;
  end;
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
{$IFDEF CFORM}
type
  TDCP_hash= class(TComponent)
  protected
    fID: longint;
    fInitialized: boolean;
    fAlgorithm: AnsiString;                                             // [dpv]
    fHashSize: longint;
    fNullStr: AnsiString;                                               // [dpv]
    procedure SetHashSize(Value: longint); virtual;
  public
    property ID: longint
      read fID;
    property Initialized: boolean
      read fInitialized;
    procedure Init; virtual; abstract;
    procedure Burn; virtual; abstract;
    procedure Update(const Buffer; Size: longint); virtual; abstract;
    procedure UpdateStr(const Buffer: RawByteString);                      // [dpv]
    procedure Final(var Digest); virtual; abstract;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Algorithm: AnsiString
      read fAlgorithm write fNullStr;
    property HashSize: longint
      read fHashSize write SetHashSize;
  end;
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
{$IFDEF WIN32}
function LRot16(X: Word; c: longint): Word; assembler;
function RRot16(X: Word; c: longint): Word; assembler;
function LRot32(X: DWord; c: longint): DWord; assembler;
function RRot32(X: DWord; c: longint): DWord; assembler;
function SwapDWord(X: DWord): DWord; assembler;
{$ELSE}
function LRot16(X: Word; c: longint): Word;
function RRot16(X: Word; c: longint): Word;
function LRot32(X: DWord; c: longint): DWord;
function RRot32(X: DWord; c: longint): DWord;
function SwapDWord(X: DWord): DWord; 
{$ENDIF}
procedure XorBlock(I1, I2, O1: PByteArray; Len: longint);

{******************************************************************************}
{******************************************************************************}
implementation
{$IFDEF CFORM}
uses
   Base64,
   SHA1;

type
  TDCP_defaulthash= TDCP_sha1;

{******************************************************************************}
{******************************************************************************}
constructor TDCP_blockcipher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Burn;
end;

destructor TDCP_blockcipher.Destroy;
begin
  if fInitialized then
    Burn;
  inherited Destroy;
end;

procedure TDCP_blockcipher.InitStr;
var
  Hash: TDCP_defaulthash;
  KeyHash: pointer;
begin
  Hash:= TDCP_defaulthash.Create(Self);
  Hash.Init;
  Hash.Update(Key[1],Length(Key));
  GetMem(KeyHash,Hash.HashSize div 8);
  Hash.Final(KeyHash^);
  if Hash.HashSize> fMaxKeySize then
    Init(KeyHash^,fMaxKeySize,nil)
  else
    Init(KeyHash^,Hash.HashSize,nil);
  FillChar(KeyHash^,Hash.HashSize div 8,$FF);
  FreeMem(KeyHash,Hash.HashSize div 8);
  Hash.Free;
end;

{******************************************************************************}
{******************************************************************************}
constructor TDCP_hash.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Burn;
end;

destructor TDCP_hash.Destroy;
begin
  if fInitialized then
    Burn;
  inherited Destroy;
end;

procedure TDCP_hash.SetHashSize(Value: longint);
begin
end;

procedure TDCP_hash.UpdateStr(const Buffer: RawByteString);
begin
  Update(Buffer[1],Length(Buffer));
end;
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
{$IFDEF WIN32}
function LRot16(X: Word; c: longint): Word; assembler;
asm
  mov ecx,&c
  mov ax,&X
  rol ax,cl
  mov &Result,ax
end;

function RRot16(X: Word; c: longint): Word; assembler;
asm
  mov ecx,&c
  mov ax,&X
  ror ax,cl
  mov &Result,ax
end;

function LRot32(X: DWord; c: longint): DWord; register; assembler;
asm
  mov ecx, edx
  rol eax, cl
end;

function RRot32(X: DWord; c: longint): DWord; register; assembler;
asm
  mov ecx, edx
  ror eax, cl
end;

function SwapDWord(X: DWord): DWord; register; assembler;
asm
  xchg al,ah
  rol  eax,16
  xchg al,ah
end;

{$ELSE}
function LRot16(X: Word; c: longint): Word;
begin
  LRot16:= (X shl c) or (X shr (16 - c));
end;

function RRot16(X: Word; c: longint): Word;
begin
  RRot16:= (X shr c) or (X shl (16 - c));
end;

function LRot32(X: DWord; c: longint): DWord;
begin
  LRot32:= (X shl c) or (X shr (32 - c));
end;

function RRot32(X: DWord; c: longint): DWord;
begin
  RRot32:= (X shr c) or (X shl (32 - c));
end;

function SwapDWord(X: DWord): DWord;
begin
  Result:= (X shr 24) or ((X shr 8) and $FF00) or ((X shl 8) and $FF0000) or (X shl 24);
end;
{$ENDIF}

procedure XorBlock(I1, I2, O1: PByteArray; Len: longint);
var
  i: longint;
begin
  for i:= 0 to Len-1 do
    O1^[i]:= I1^[i] xor I2^[i];
end;

end.
