{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1996 AO ROSNO                   }
{                                                       }
{ Revision and functions added by JB.                   }
{*******************************************************}

{$I RX.INC}
{$N+}

unit RxMaxMin;

interface

function Max(A, B: LongInt): LongInt; {$IFDEF RX_D9}inline; {$ENDIF}
function Min(A, B: LongInt): LongInt; {$IFDEF RX_D9}inline; {$ENDIF}
function MaxInteger(const Values: array of LongInt): LongInt;
function MinInteger(const Values: array of LongInt): LongInt;
{$IFDEF RX_D4}
function MaxInt64(const Values: array of Int64): Int64;
function MinInt64(const Values: array of Int64): Int64;
{$ENDIF}

function MaxFloat(const Values: array of Extended): Extended;
function MinFloat(const Values: array of Extended): Extended;
function MaxDateTime(const Values: array of TDateTime): TDateTime;
function MinDateTime(const Values: array of TDateTime): TDateTime;
{$IFNDEF VER80}
function MaxOf(const Values: array of Variant): Variant;
function MinOf(const Values: array of Variant): Variant;
{$ENDIF}

procedure SwapLong(var Int1, Int2: LongInt); {$IFDEF RX_D9}inline; {$ENDIF}
procedure SwapInt(var Int1, Int2: Integer); {$IFDEF RX_D9}inline; {$ENDIF}
{$IFDEF RX_D4}
procedure SwapInt64(var Int1, Int2: Int64); {$IFDEF RX_D9}inline; {$ENDIF}
{$ENDIF}

{$IFDEF VER80}
function MakeWord(A, B: Byte): Word; {$IFDEF RX_D9}inline; {$ENDIF}
{$ENDIF}

function MaxIntArr(const Values: array of Integer): Integer;
function MinIntArr(const Values: array of Integer): Integer;

implementation

function MaxIntArr(const Values: array of Integer): Integer;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] > Result then Result := Values[I];
end;

function MinIntArr(const Values: array of Integer): Integer;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] < Result then Result := Values[I];
end;

{$IFDEF VER80}

function MakeWord(A, B: Byte): Word;
begin
  Result := A or B shl 8;
end;
{$ENDIF}

procedure SwapInt(var Int1, Int2: Integer);
var
  I: Integer;
begin
  I := Int1; Int1 := Int2; Int2 := I;
end;

{$IFDEF RX_D4}

procedure SwapInt64(var Int1, Int2: Int64);
var
  I: Int64;
begin
  I := Int1; Int1 := Int2; Int2 := I;
end;
{$ENDIF}

procedure SwapLong(var Int1, Int2: LongInt);
var
  I: LongInt;
begin
  I := Int1; Int1 := Int2; Int2 := I;
end;

function Max(A, B: LongInt): LongInt;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Min(A, B: LongInt): LongInt;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function MaxInteger(const Values: array of LongInt): LongInt;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] > Result then Result := Values[I];
end;

function MinInteger(const Values: array of LongInt): LongInt;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] < Result then Result := Values[I];
end;

{$IFDEF RX_D4}

function MaxInt64(const Values: array of Int64): Int64;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] > Result then Result := Values[I];
end;

function MinInt64(const Values: array of Int64): Int64;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] < Result then Result := Values[I];
end;

{$ENDIF RX_D4}

function MaxFloat(const Values: array of Extended): Extended;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] > Result then Result := Values[I];
end;

function MinFloat(const Values: array of Extended): Extended;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] < Result then Result := Values[I];
end;

function MaxDateTime(const Values: array of TDateTime): TDateTime;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] < Result then Result := Values[I];
end;

function MinDateTime(const Values: array of TDateTime): TDateTime;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] < Result then Result := Values[I];
end;

{$IFNDEF VER80}

function MaxOf(const Values: array of Variant): Variant;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] > Result then Result := Values[I];
end;

function MinOf(const Values: array of Variant): Variant;
var
  I: Cardinal;
begin
  Result := Values[0];
  for I := 0 to High(Values) do
    if Values[I] < Result then Result := Values[I];
end;
{$ENDIF}

end.