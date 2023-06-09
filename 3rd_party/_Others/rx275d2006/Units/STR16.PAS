{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{         For 16-bit applications only                  }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{                                                       }
{*******************************************************}

unit Str16;

interface

{$IFNDEF WIN32}

{$I RX.INC}

type
  ShortString = string;
  PShortString = ^ShortString;
  AnsiChar = Char;
  PAnsiChar = ^AnsiChar;

{ System32 unit functions and procedures }
procedure SetLength(var S: string; NewLength: Byte);
procedure SetString(var S: string; Buffer: PChar; Len: Byte);
procedure UniqueString(var S: string);

{ SysUtils32 unit functions and procedures }
function Trim(const S: string): string;
function TrimLeft(const S: string): string;
function TrimRight(const S: string): string;
function QuotedStr(const S: string): string;

{$ENDIF WIN32}

implementation

{$IFNDEF WIN32}

uses SysUtils;

procedure SetLength(var S: string; NewLength: Byte);
begin
  S[0] := Char(NewLength);
end;

procedure SetString(var S: string; Buffer: PChar; Len: Byte);
begin
  S[0] := Char(Len);
  if Buffer <> nil then begin
    if StrLen(Buffer) < Len then Len := StrLen(Buffer);
    Move(Buffer^, S[1], Len);
  end;
end;

procedure UniqueString(var S: string);
begin
end;

function Trim(const S: string): string;
var
  I, L: Byte;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := ''
  else begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function TrimLeft(const S: string): string;
var
  I, L: Byte;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  Result := Copy(S, I, 255);
end;

function TrimRight(const S: string): string;
var
  I: Byte;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do Dec(I);
  Result := Copy(S, 1, I);
end;

function QuotedStr(const S: string): string;
var
  I: Byte;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if Result[I] = '''' then Insert('''', Result, I);
  Result := '''' + Result + '''';
end;

{$ENDIF WIN32}

end.