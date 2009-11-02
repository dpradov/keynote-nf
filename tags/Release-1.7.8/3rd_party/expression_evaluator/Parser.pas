unit Parser;

{==========================================================================}
{ Expression Evaluator v1.4 for Delphi                                     }
{ (16 & 32 bits)                                                           }
{                                                                          }
{ Copyright © 1997 by BitSoft Development, L.L.C.                          }
{ All rights reserved                                                      }
{                                                                          }
{ Web:     http://www.bitsoft.com                                          }
{ E-mail:  info@bitsoft.com                                                }
{ Support: tech-support@bitsoft.com                                        }
{--------------------------------------------------------------------------}
{ Portions Copyright © 1992 by Borland International, Inc.                 }
{ All rights reserved                                                      }
{--------------------------------------------------------------------------}
{ This file is distributed as freeware and without warranties of any kind. }
{ You can use it in your own applications at your own risk.                }
{ See the License Agreement for more information.                          }
{==========================================================================}

interface

uses
  {$ifdef Win32}
  Windows,
  {$else}
  WinProcs, Wintypes,
  {$endif}
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs;

type
  TGetVarEvent = procedure(Sender : TObject; VarName : string; var
    Value : Extended; var Found : Boolean) of object;

  TParseErrorEvent = procedure(Sender : TObject; ParseError : Integer)
    of object;

const
  ParserStackSize = 15;
  MaxFuncNameLen = 5;
  ExpLimit = 11356;
  SqrLimit = 1E2466;
  MaxExpLen = 4;
  TotalErrors = 7;
  ErrParserStack = 1;
  ErrBadRange = 2;
  ErrExpression = 3;
  ErrOperator = 4;
  ErrOpenParen = 5;
  ErrOpCloseParen = 6;
  ErrInvalidNum = 7;

type
  ErrorRange = 0..TotalErrors;

  TokenTypes = (Plus, Minus, Times, Divide, Expo, OParen, CParen, Num,
                Func, EOL, Bad, ERR, Modu);

  TokenRec = record
    State : Byte;
    case Byte of
      0 : (Value : Extended);
      2 : (FuncName : String[MaxFuncNameLen]);
  end; { TokenRec }

type
  TMathParser = class(TComponent)
  private
    { Private declarations }
      FInput : string;
      FOnGetVar : TGetVarEvent;
      FOnParseError : TParseErrorEvent;
  protected
    { Protected declarations }
      CurrToken : TokenRec;
      MathError : Boolean;
      Stack : array[1..ParserStackSize] of TokenRec;
      StackTop : 0..ParserStackSize;
      TokenError : ErrorRange;
      TokenLen : Word;
      TokenType : TokenTypes;
    function GotoState(Production : Word) : Word;
    function IsFunc(S : String) : Boolean;
    function IsVar(var Value : Extended) : Boolean;
    function NextToken : TokenTypes;
    procedure Push(Token : TokenRec);
    procedure Pop(var Token : TokenRec);
    procedure Reduce(Reduction : Word);
    procedure Shift(State : Word);
  public
    { Public declarations }
      Position : Word;
      ParseError : Boolean;
      ParseValue : Extended;
    constructor Create(AOwner: TComponent);
    procedure Parse;
  published
    { Published declarations }
    property OnGetVar : TGetVarEvent read FOnGetVar write FOnGetVar;
    property OnParseError : TParseErrorEvent read FOnParseError
      write FOnParseError;
    property ParseString : string read FInput write FInput;
  end;

procedure Register;

{$ifdef Win32}
  {$R *.D32}
{$else}
  {$R *.D16}
{$endif}

implementation

const
  Letters : set of Char = ['A'..'Z', 'a'..'z'];
  Numbers : set of Char = ['0'..'9'];

constructor TMathParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { defaults }
  FInput := '';
end;

function TMathParser.GotoState(Production : Word) : Word;
{ Finds the new state based on the just-completed production and the
   top state. }
var
  State : Word;
begin
  State := Stack[StackTop].State;
  if (Production <= 3) then
  begin
    case State of
      0 : GotoState := 1;
      9 : GotoState := 19;
      20 : GotoState := 28;
    end; { case }
  end
  else if Production <= 6 then
  begin
    case State of
      0, 9, 20 : GotoState := 2;
      12 : GotoState := 21;
      13 : GotoState := 22;
    end; { case }
  end
  else if (Production <= 8) or (Production = 100) then
  begin
    case State of
      0, 9, 12, 13, 20 : GotoState := 3;
      14 : GotoState := 23;
      15 : GotoState := 24;
      16 : GotoState := 25;
      40 : GotoState := 80;
    end; { case }
  end
  else if Production <= 10 then
  begin
    case State of
      0, 9, 12..16, 20, 40 : GotoState := 4;
    end; { case }
  end
  else if Production <= 12 then
  begin
    case State of
      0, 9, 12..16, 20, 40 : GotoState := 6;
      5 : GotoState := 17;
    end; { case }
  end
  else begin
    case State of
      0, 5, 9, 12..16, 20, 40 : GotoState := 8;
    end; { case }
  end;
end; { GotoState }

function TMathParser.IsFunc(S : String) : Boolean;
{ Checks to see if the parser is about to read a function }
var
  P, SLen : Word;
  FuncName : string;
begin
  P := Position;
  FuncName := '';
  while (P <= Length(FInput)) and (FInput[P] in ['A'..'Z', 'a'..'z', '0'..'9',
    '_']) do
  begin
    FuncName := FuncName + FInput[P];
    Inc(P);
  end; { while }
  if Uppercase(FuncName) = S
    then begin
           SLen := Length(S);
           CurrToken.FuncName := UpperCase(Copy(FInput, Position, SLen));
           Inc(Position, SLen);
           IsFunc := True;
         end { if }
    else IsFunc := False;
end; { IsFunc }

function TMathParser.IsVar(var Value : Extended) : Boolean;
var
  VarName : string;
  VarFound : Boolean;
begin
  VarFound := False;
  VarName := '';
  while (Position <= Length(FInput)) and (FInput[Position] in ['A'..'Z',
    'a'..'z', '0'..'9', '_']) do
  begin
    VarName := VarName + FInput[Position];
    Inc(Position);
  end; { while }
  if Assigned(FOnGetVar)
    then FOnGetVar(Self, VarName, Value, VarFound);
  IsVar := VarFound;
end; { IsVar }

function TMathParser.NextToken : TokenTypes;
{ Gets the next Token from the Input stream }
var
  NumString : String[80];
  FormLen, Place, TLen, NumLen : Word;
  Check : Integer;
  Ch, FirstChar : Char;
  Decimal : Boolean;
begin
   while (Position <= Length(FInput)) and (FInput[Position] = ' ') do
     Inc(Position);
   TokenLen := Position;
   if Position > Length(FInput) then
   begin
     NextToken := EOL;
     TokenLen := 0;
     Exit;
   end; { if }
   Ch := UpCase(FInput[Position]);
   if Ch in ['!'] then
   begin
      NextToken := ERR;
      TokenLen := 0;
      Exit;
   end; { if }
   if Ch in ['0'..'9', '.'] then
   begin
     NumString := '';
     TLen := Position;
     Decimal := False;
     while (TLen <= Length(FInput)) and
           ((FInput[TLen] in ['0'..'9']) or
            ((FInput[TLen] = '.') and (not Decimal))) do
     begin
       NumString := NumString + FInput[TLen];
       if Ch = '.' then
         Decimal := True;
       Inc(TLen);
     end; { while }
     if (TLen = 2) and (Ch = '.') then
     begin
       NextToken := BAD;
       TokenLen := 0;
       Exit;
     end; { if }
     if (TLen <= Length(FInput)) and (UpCase(FInput[TLen]) = 'E') then
     begin
       NumString := NumString + 'E';
       Inc(TLen);
       if FInput[TLen] in ['+', '-'] then
       begin
         NumString := NumString + FInput[TLen];
         Inc(TLen);
       end; { if }
       NumLen := 1;
       while (TLen <= Length(FInput)) and (FInput[TLen] in ['0'..'9']) and
             (NumLen <= MaxExpLen) do
       begin
         NumString := NumString + FInput[TLen];
         Inc(NumLen);
         Inc(TLen);
       end; { while }
     end; { if }
     if NumString[1] = '.' then
       NumString := '0' + NumString;
     Val(NumString, CurrToken.Value, Check);
     if Check <> 0 then
       begin
         MathError := True;
         TokenError := ErrInvalidNum;
         Inc(Position, Pred(Check));
       end { if }
     else
       begin
         NextToken := NUM;
         Inc(Position, System.Length(NumString));
         TokenLen := Position - TokenLen;
       end; { else }
     Exit;
   end { if }
   else if Ch in Letters then
   begin
     if IsFunc('ABS') or
        IsFunc('ATAN') or
        IsFunc('COS') or
        IsFunc('EXP') or
        IsFunc('LN') or
        IsFunc('ROUND') or
        IsFunc('SIN') or
        IsFunc('SQRT') or
        IsFunc('SQR') or
        IsFunc('TRUNC') then
     begin
       NextToken := FUNC;
       TokenLen := Position - TokenLen;
       Exit;
     end; { if }
     if IsFunc('MOD') then
     begin
       NextToken := MODU;
       TokenLen := Position - TokenLen;
       Exit;
     end; { if }
     if IsVar(CurrToken.Value)
       then begin
              NextToken := NUM;
              TokenLen := Position - TokenLen;
              Exit;
            end { if }
       else begin
              NextToken := BAD;
              TokenLen := 0;
              Exit;
            end; { else }
   end { if }
   else begin
     case Ch of
       '+' : NextToken := PLUS;
       '-' : NextToken := MINUS;
       '*' : NextToken := TIMES;
       '/' : NextToken := DIVIDE;
       '^' : NextToken := EXPO;
       '(' : NextToken := OPAREN;
       ')' : NextToken := CPAREN;
       else begin
         NextToken := BAD;
         TokenLen := 0;
         Exit;
       end; { case else }
     end; { case }
     Inc(Position);
     TokenLen := Position - TokenLen;
     Exit;
   end; { else if }
end; { NextToken }

procedure TMathParser.Pop(var Token : TokenRec);
{ Pops the top Token off of the stack }
begin
  Token := Stack[StackTop];
  Dec(StackTop);
end; { Pop }

procedure TMathParser.Push(Token : TokenRec);
{ Pushes a new Token onto the stack }
begin
  if StackTop = ParserStackSize then
    TokenError := ErrParserStack
  else begin
    Inc(StackTop);
    Stack[StackTop] := Token;
  end; { else }
end; { Push }

procedure TMathParser.Parse;
{ Parses an input stream }
var
  FirstToken : TokenRec;
  Accepted : Boolean;
begin
  Position := 1;
  StackTop := 0;
  TokenError := 0;
  MathError := False;
  ParseError := False;
  Accepted := False;
  FirstToken.State := 0;
  FirstToken.Value := 0;
  Push(FirstToken);
  TokenType := NextToken;
  repeat
    case Stack[StackTop].State of
      0, 9, 12..16, 20, 40 : begin
        if TokenType = NUM then
          Shift(10)
        else if TokenType = FUNC then
          Shift(11)
        else if TokenType = MINUS then
          Shift(5)
        else if TokenType = OPAREN then
          Shift(9)
        else if TokenType = ERR then
          begin
             MathError := True;
             Accepted := True;
          end { else if }
        else begin
          TokenError := ErrExpression;
          Dec(Position, TokenLen);
        end; { else }
      end; { case of }
      1 : begin
        if TokenType = EOL then
          Accepted := True
        else if TokenType = PLUS then
          Shift(12)
        else if TokenType = MINUS then
          Shift(13)
        else begin
          TokenError := ErrOperator;
          Dec(Position, TokenLen);
        end; { else }
      end; { case of }
      2 : begin
        if TokenType = TIMES then
          Shift(14)
        else if TokenType = DIVIDE then
          Shift(15)
        else
          Reduce(3);
      end; { case of }
      3 : begin
       if TokenType = MODU then
         Shift(40)
       else
         Reduce(6);
      end; { case of }
      4 : begin
       if TokenType = EXPO then
         Shift(16)
       else
         Reduce(8);
      end; { case of }
      5 : begin
        if TokenType = NUM then
          Shift(10)
        else if TokenType = FUNC then
          Shift(11)
        else if TokenType = OPAREN then
          Shift(9)
        else
          begin
            TokenError := ErrExpression;
            Dec(Position, TokenLen);
          end; { else }
      end; { case of }
      6 : Reduce(10);
      7 : Reduce(13);
      8 : Reduce(12);
      10 : Reduce(15);
      11 : begin
        if TokenType = OPAREN then
          Shift(20)
        else
          begin
            TokenError := ErrOpenParen;
            Dec(Position, TokenLen);
          end; { else }
      end; { case of }
      17 : Reduce(9);
      18 : raise Exception.Create('Bad token state');
      19 : begin
        if TokenType = PLUS then
          Shift(12)
        else if TokenType = MINUS then
          Shift(13)
        else if TokenType = CPAREN then
          Shift(27)
        else
          begin
            TokenError := ErrOpCloseParen;
            Dec(Position, TokenLen);
          end;
      end; { case of }
      21 : begin
        if TokenType = TIMES then
          Shift(14)
        else if TokenType = DIVIDE then
          Shift(15)
        else
          Reduce(1);
      end; { case of }
      22 : begin
        if TokenType = TIMES then
          Shift(14)
        else if TokenType = DIVIDE then
          Shift(15)
        else
          Reduce(2);
      end; { case of }
      23 : Reduce(4);
      24 : Reduce(5);
      25 : Reduce(7);
      26 : Reduce(11);
      27 : Reduce(14);
      28 : begin
        if TokenType = PLUS then
          Shift(12)
        else if TokenType = MINUS then
          Shift(13)
        else if TokenType = CPAREN then
          Shift(29)
        else
          begin
            TokenError := ErrOpCloseParen;
            Dec(Position, TokenLen);
          end; { else }
      end; { case of }
      29 : Reduce(16);
      80 : Reduce(100);
    end; { case }
  until Accepted or (TokenError <> 0);
  if TokenError <> 0 then
  begin
      if TokenError = ErrBadRange then
        Dec(Position, TokenLen);
      if Assigned(FOnParseError)
        then FOnParseError(Self, TokenError);
  end; { if }
  if MathError or (TokenError <> 0) then
  begin
    ParseError := True;
    ParseValue := 0;
    Exit;
  end; { if }
  ParseError := False;
  ParseValue := Stack[StackTop].Value;
end; { Parse }

procedure TMathParser.Reduce(Reduction : Word);
{ Completes a reduction }
var
  Token1, Token2 : TokenRec;
begin
  case Reduction of
    1 : begin
      Pop(Token1);
      Pop(Token2);
      Pop(Token2);
      CurrToken.Value := Token1.Value + Token2.Value;
    end;
    2 : begin
      Pop(Token1);
      Pop(Token2);
      Pop(Token2);
      CurrToken.Value := Token2.Value - Token1.Value;
    end;
    4 : begin
      Pop(Token1);
      Pop(Token2);
      Pop(Token2);
      CurrToken.Value := Token1.Value * Token2.Value;
    end;
    5 : begin
      Pop(Token1);
      Pop(Token2);
      Pop(Token2);
      if Token1.Value = 0 then
        MathError := True
      else
        CurrToken.Value := Token2.Value / Token1.Value;
    end;

    { MOD operator }
    100 : begin
      Pop(Token1);
      Pop(Token2);
      Pop(Token2);
      if Token1.Value = 0 then
        MathError := True
      else
        CurrToken.Value := Round(Token2.Value) mod Round(Token1.Value);
    end;

    7 : begin
      Pop(Token1);
      Pop(Token2);
      Pop(Token2);
      if Token2.Value <= 0 then
        MathError := True
      else if (Token1.Value * Ln(Token2.Value) < -ExpLimit) or
              (Token1.Value * Ln(Token2.Value) > ExpLimit) then
        MathError := True
      else
        CurrToken.Value := Exp(Token1.Value * Ln(Token2.Value));
    end;
    9 : begin
      Pop(Token1);
      Pop(Token2);
      CurrToken.Value := -Token1.Value;
    end;
    11 : raise Exception.Create('Invalid reduction');
    13 : raise Exception.Create('Invalid reduction');
    14 : begin
      Pop(Token1);
      Pop(CurrToken);
      Pop(Token1);
    end;
    16 : begin
      Pop(Token1);
      Pop(CurrToken);
      Pop(Token1);
      Pop(Token1);
      if Token1.FuncName = 'ABS' then
        CurrToken.Value := Abs(CurrToken.Value)
      else if Token1.FuncName = 'ATAN' then
        CurrToken.Value := ArcTan(CurrToken.Value)
      else if Token1.FuncName = 'COS' then
      begin
         if (CurrToken.Value < -9E18) or (CurrToken.Value > 9E18) then
            MathError := True
         else
            CurrToken.Value := Cos(CurrToken.Value)
      end {...if Token1.FuncName = 'SIN' }
      else if Token1.FuncName = 'EXP' then
      begin
        if (CurrToken.Value < -ExpLimit) or (CurrToken.Value > ExpLimit) then
          MathError := True
        else
          CurrToken.Value := Exp(CurrToken.Value);
      end
      else if Token1.FuncName = 'LN' then
      begin
        if CurrToken.Value <= 0 then
          MathError := True
        else
          CurrToken.Value := Ln(CurrToken.Value);
      end
      else if Token1.FuncName = 'ROUND' then
      begin
        if (CurrToken.Value < -1E9) or (CurrToken.Value > 1E9) then
          MathError := True
        else
          CurrToken.Value := Round(CurrToken.Value);
      end
      else if Token1.FuncName = 'SIN' then
      begin
         if (CurrToken.Value < -9E18) or (CurrToken.Value > 9E18) then
            MathError := True
         else
            CurrToken.Value := Sin(CurrToken.Value)
      end {...if Token1.FuncName = 'SIN' }
      else if Token1.FuncName = 'SQRT' then
      begin
        if CurrToken.Value < 0 then
          MathError := True
        else
          CurrToken.Value := Sqrt(CurrToken.Value);
      end
      else if Token1.FuncName = 'SQR' then
      begin
        if (CurrToken.Value < -SQRLIMIT) or (CurrToken.Value > SQRLIMIT) then
          MathError := True
        else
          CurrToken.Value := Sqr(CurrToken.Value);
      end
      else if Token1.FuncName = 'TRUNC' then
      begin
        if (CurrToken.Value < -1E9) or (CurrToken.Value > 1E9) then
          MathError := True
        else
          CurrToken.Value := Trunc(CurrToken.Value);
      end;
    end;
    3, 6, 8, 10, 12, 15 : Pop(CurrToken);
  end; { case }
  CurrToken.State := GotoState(Reduction);
  Push(CurrToken);
end; { Reduce }

procedure TMathParser.Shift(State : Word);
{ Shifts a Token onto the stack }
begin
  CurrToken.State := State;
  Push(CurrToken);
  TokenType := NextToken;
end; { Shift }

procedure Register;
begin
  RegisterComponents('BitSoft', [TMathParser]);
end;

end.
