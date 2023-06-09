{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995 AO ROSNO                   }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxDataConv;

interface

{$I RX.INC}

uses
  SysUtils, {$IFNDEF VER80}Windows, {$ELSE}WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Graphics, Controls, Forms, Dialogs, RxDateUtil;

type

  TDataType = (dtString, dtInteger, dtFloat, dtDateTime, dtDate,
    dtTime, dtBoolean);

  TTimeFormat = (tfHHMMSS, tfHMMSS, tfHHMM, tfHMM);

{ TDateTimeFormat }

  TDateTimeFormat = class(TPersistent)
  private
    FAMString: string;
    FPMString: string;
    FDateOrder: TDateOrder;
    FTimeFormat: TTimeFormat;
    FTimeSeparator: Char;
    FDateSeparator: Char;
    FLongDate: Boolean;
    FFourDigitYear: Boolean;
    FLeadingZero: Boolean;
    function GetAMString: string;
    procedure SetAMString(const Value: string);
    function GetPMString: string;
    procedure SetPMString(const Value: string);
  protected
    function GetDateMask: string; virtual;
    function GetTimeMask: string; virtual;
    function GetMask: string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ResetDefault; virtual;
    property DateMask: string read GetDateMask;
    property TimeMask: string read GetTimeMask;
    property Mask: string read GetMask;
  published
    property AMString: string read GetAMString write SetAMString;
    property PMString: string read GetPMString write SetPMString;
    property DateOrder: TDateOrder read FDateOrder write FDateOrder;
    property TimeFormat: TTimeFormat read FTimeFormat write FTimeFormat;
    property TimeSeparator: Char read FTimeSeparator write FTimeSeparator;
    property DateSeparator: Char read FDateSeparator write FDateSeparator;
    property LongDate: Boolean read FLongDate write FLongDate default False;
    property FourDigitYear: Boolean read FFourDigitYear write FFourDigitYear default True;
    property LeadingZero: Boolean read FLeadingZero write FLeadingZero default False;
  end;

{ TConverter }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TConverter = class(TComponent)
  private
    { Private declarations }
    {$IFDEF RX_D4} // Polaris
    FData: string;
    {$ELSE}
    FData: PString;
    {$ENDIF}
    FTextValues: array[Boolean] of string;
    FDataType: TDataType;
    FDateTimeFormat: TDateTimeFormat;
    FFloatFormat: TFloatFormat;
    FPrecision, FDigits: Integer;
    FRaiseOnError: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetDataType(Value: TDataType);
    procedure SetDateTimeFormat(Value: TDateTimeFormat);
    function GetDateTimeFormat: TDateTimeFormat;
    function GetString: string;
    procedure SetString(const Value: string);
    function GetDateTime: TDateTime;
    function GetBoolValues(Index: Integer): string;
    procedure SetBoolValues(Index: Integer; const Value: string);
    procedure CheckDataType;
    function BoolToStr(Value: Boolean): string;
    function FloatToString(Value: Double): string;
    function DateTimeToString(Value: TDateTime): string;
  protected
    { Protected declarations }
    procedure Change; dynamic;
    function GetAsBoolean: Boolean; virtual;
    function GetAsDateTime: TDateTime; virtual;
    function GetAsDate: TDateTime; virtual;
    function GetAsTime: TDateTime; virtual;
    function GetAsFloat: Double; virtual;
    function GetAsInteger: LongInt; virtual;
    function GetAsString: string; virtual;
    procedure SetAsBoolean(Value: Boolean); virtual;
    procedure SetAsDateTime(Value: TDateTime); virtual;
    procedure SetAsDate(Value: TDateTime); virtual;
    procedure SetAsTime(Value: TDateTime); virtual;
    procedure SetAsFloat(Value: Double); virtual;
    procedure SetAsInteger(Value: LongInt); virtual;
    procedure SetAsString(const Value: string); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function IsValidChar(Ch: Char): Boolean; virtual;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: LongInt read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
  published
    { Published declarations }
    property DataType: TDataType read FDataType write SetDataType default dtString;
    property DateTimeFormat: TDateTimeFormat read GetDateTimeFormat write SetDateTimeFormat;
    property Digits: Integer read FDigits write FDigits default 2;
    property DisplayFalse: string index 0 read GetBoolValues write SetBoolValues;
    property DisplayTrue: string index 1 read GetBoolValues write SetBoolValues;
    property FloatFormat: TFloatFormat read FFloatFormat write FFloatFormat default ffGeneral;
    property Precision: Integer read FPrecision write FPrecision default 15;
    property RaiseOnError: Boolean read FRaiseOnError write FRaiseOnError default False;
    property Text: string read GetString write SetAsString;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses RxStrUtils;

{ TDateTimeFormat }

constructor TDateTimeFormat.Create;
begin
  inherited Create;
  ResetDefault;
end;

destructor TDateTimeFormat.Destroy;
begin
  inherited Destroy;
end;

procedure TDateTimeFormat.ResetDefault;
begin
  FAMString := {$IFDEF RX_D15}FormatSettings.{$ENDIF}TimeAMString;
  FPMString := {$IFDEF RX_D15}FormatSettings.{$ENDIF}TimePMString;
  FTimeSeparator := {$IFDEF RX_D15}FormatSettings.{$ELSE}SysUtils.{$ENDIF}TimeSeparator;
  FDateSeparator := {$IFDEF RX_D15}FormatSettings.{$ELSE}SysUtils.{$ENDIF}DateSeparator;
  FDateOrder := doDMY;
  FTimeFormat := tfHHMMSS;
  FLongDate := False;
  FFourDigitYear := True;
  FLeadingZero := False;
end;

procedure TDateTimeFormat.Assign(Source: TPersistent);
begin
  if Source is TDateTimeFormat then
  begin
    FAMString := TDateTimeFormat(Source).AMString;
    FPMString := TDateTimeFormat(Source).PMString;
    FDateOrder := TDateTimeFormat(Source).DateOrder;
    FTimeFormat := TDateTimeFormat(Source).TimeFormat;
    FTimeSeparator := TDateTimeFormat(Source).TimeSeparator;
    FDateSeparator := TDateTimeFormat(Source).DateSeparator;
    FLongDate := TDateTimeFormat(Source).LongDate;
    FFourDigitYear := TDateTimeFormat(Source).FourDigitYear;
    FLeadingZero := TDateTimeFormat(Source).LeadingZero;
    Exit;
  end;
  inherited Assign(Source);
end;

function TDateTimeFormat.GetAMString: string;
begin
  Result := FAMString;
end;

procedure TDateTimeFormat.SetAMString(const Value: string);
begin
  if Value = '' then
    FAMString := {$IFDEF RX_D15}FormatSettings.{$ENDIF}TimeAMString
  else
    FAMString := Value;
end;

function TDateTimeFormat.GetPMString: string;
begin
  Result := FPMString;
end;

procedure TDateTimeFormat.SetPMString(const Value: string);
begin
  if Value = '' then
    FPMString := {$IFDEF RX_D15}FormatSettings.{$ENDIF}TimePMString
  else
    FPMString := Value;
end;

function TDateTimeFormat.GetDateMask: string;
var
  S: array[1..3] of string;
  Separator: string;
begin
  Result := '';
  if LeadingZero then
  begin
    S[1] := 'dd';
    S[2] := 'mm';
  end
  else
  begin
    S[1] := 'd';
    S[2] := 'm';
  end;
  if LongDate then
  begin
    S[2] := 'mmmm';
    Separator := ' ';
  end
  else
    Separator := '"' + DateSeparator + '"';
  if FourDigitYear then
    S[3] := 'yyyy'
  else
    S[3] := 'yy';
  case DateOrder of
    doDMY: Result := S[1] + Separator + S[2] + Separator + S[3];
    doMDY: Result := S[2] + Separator + S[1] + Separator + S[3];
    doYMD: Result := S[3] + Separator + S[2] + Separator + S[1];
  end;
end;

function TDateTimeFormat.GetTimeMask: string;
var
  S: array[1..3] of string;
  Separator: string;
  AMPM: string;
begin
  Separator := '"' + TimeSeparator + '"';
  AMPM := ' ' + AMString + '/' + PMString;
  if LeadingZero then
  begin
    S[1] := 'hh';
    S[2] := 'nn';
    S[3] := 'ss';
  end
  else
  begin
    S[1] := 'h';
    S[2] := 'n';
    S[3] := 's';
  end;
  case TimeFormat of
    tfHHMMSS: Result := S[1] + Separator + S[2] + Separator + S[3];
    tfHMMSS: Result := S[1] + Separator + S[2] + Separator + S[3] + AMPM;
    tfHHMM: Result := S[1] + Separator + S[2];
    tfHMM: Result := S[1] + Separator + S[2] + AMPM;
  end;
end;

function TDateTimeFormat.GetMask: string;
begin
  Result := GetDateMask + ' ' + GetTimeMask;
end;

{ TConverter }

constructor TConverter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF RX_D4} // Polaris
  FData := EmptyStr;
  {$ELSE}
  FData := NullStr;
  {$ENDIF}
  FDataType := dtString;
  FPrecision := 15;
  FDigits := 2;
  FDateTimeFormat := TDateTimeFormat.Create;
  FTextValues[False] := 'False';
  FTextValues[True] := 'True';
  FRaiseOnError := False;
end;

destructor TConverter.Destroy;
begin
  FDataType := dtString;
  {$IFNDEF RX_D4} // Polaris
  DisposeStr(FData);
  {$ENDIF}
  FDateTimeFormat.Free;
  inherited Destroy;
end;

procedure TConverter.Clear;
begin
  {$IFNDEF RX_D4} // Polaris
  DisposeStr(FData);
  FData := NullStr;
  {$ELSE}
  FData := EmptyStr;
  {$ENDIF}
  Change;
end;

procedure TConverter.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TConverter.GetString: string;
begin
  {$IFDEF RX_D4} // Polaris
  Result := FData;
  {$ELSE}
  Result := FData^;
  {$ENDIF}
end;

procedure TConverter.SetString(const Value: string);
begin
  {$IFDEF RX_D4} // Polaris
  FData := Value;
  {$ELSE}
  AssignStr(FData, Value);
  {$ENDIF}
end;

function TConverter.GetDateTimeFormat: TDateTimeFormat;
begin
  Result := FDateTimeFormat;
end;

procedure TConverter.SetDateTimeFormat(Value: TDateTimeFormat);
begin
  FDateTimeFormat.Assign(Value);
end;

function TConverter.GetBoolValues(Index: Integer): string;
begin
  Result := string(FTextValues[Boolean(Index)]);
end;

procedure TConverter.SetBoolValues(Index: Integer; const Value: string);
begin
  FTextValues[Boolean(Index)] := Value;
end;

function TConverter.BoolToStr(Value: Boolean): string;
begin
  Result := GetBoolValues(Integer(Value));
end;

function TConverter.FloatToString(Value: Double): string;
begin
  Result := FloatToStrF(Value, FloatFormat, Precision, Digits);
end;

function TConverter.DateTimeToString(Value: TDateTime): string;
begin
  case FDataType of
    dtDate: Result := FormatDateTime(DateTimeFormat.DateMask, Value);
    dtTime: Result := FormatDateTime(DateTimeFormat.TimeMask, Value);
  else
    Result := FormatDateTime(DateTimeFormat.Mask, Value);
  end;
end;

procedure TConverter.SetDataType(Value: TDataType);
begin
  if Value <> FDataType then
  begin
    FDataType := Value;
    try
      CheckDataType;
      Change;
    except
      Clear;
      if RaiseOnError then raise;
    end;
  end;
end;

function TConverter.IsValidChar(Ch: Char): Boolean;
begin
  case FDataType of
    dtString: Result := True;
    dtInteger: Result := CharInSet(Ch, ['+', '-', '0'..'9']);
    dtFloat: Result := CharInSet(Ch, [{$IFDEF RX_D15}FormatSettings.{$ENDIF}DecimalSeparator, '+', '-', '0'..'9', 'E', 'e']);
    dtDateTime, dtDate, dtTime: Result := True;
    dtBoolean: Result := True;
  else
    Result := False;
  end;
end;

procedure TConverter.CheckDataType;
begin
  case FDataType of
    dtInteger, dtFloat: StrToFloat(GetString);
    dtDateTime, dtDate, dtTime: GetDateTime;
  end;
end;

function TConverter.GetAsBoolean: Boolean;
var
  S: string;
begin
  S := GetString;
  Result := (Length(S) > 0) and (CharInSet(S[1], ['T', 't', 'Y', 'y']) or
    (S = string(FTextValues[True])));
end;

function TConverter.GetDateTime: TDateTime;
var
  S: string;
  I: Integer;
  DateS, TimeS: set of AnsiChar;
begin
  S := GetString;
  DateS := ['/', '.'] + [DateTimeFormat.DateSeparator] -
    [DateTimeFormat.TimeSeparator];
  TimeS := [':', '-'] - [DateTimeFormat.DateSeparator] +
    [DateTimeFormat.TimeSeparator];
  for I := 1 to Length(S) do
  begin
    if CharInSet(S[I], DateS) then
      S[I] := {$IFDEF RX_D15}FormatSettings.{$ENDIF}DateSeparator
    else if CharInSet(S[I], TimeS) then
      S[I] := {$IFDEF RX_D15}FormatSettings.{$ENDIF}TimeSeparator;
  end;
  Result := StrToDateTime(S);
end;

function TConverter.GetAsDateTime: TDateTime;
begin
  try
    Result := GetDateTime;
  except
    Result := NullDate;
  end;
end;

function TConverter.GetAsDate: TDateTime;
var
  Year, Month, Day: Word;
begin
  try
    Result := GetAsDateTime;
    DecodeDate(Result, Year, Month, Day);
    Result := EncodeDate(Year, Month, Day);
  except
    Result := NullDate;
  end;
end;

function TConverter.GetAsTime: TDateTime;
var
  Hour, Min, Sec, MSec: Word;
begin
  try
    Result := GetAsDateTime;
    DecodeTime(Result, Hour, Min, Sec, MSec);
    Result := EncodeTime(Hour, Min, Sec, MSec);
  except
    Result := NullDate;
  end;
end;

function TConverter.GetAsFloat: Double;
begin
  try
    case FDataType of
      dtDateTime: Result := GetAsDateTime;
      dtDate: Result := GetAsDate;
      dtTime: Result := GetAsTime;
    else
      Result := StrToFloat(GetString);
    end;
  except
    Result := 0.0;
  end;
end;

function TConverter.GetAsInteger: LongInt;
begin
  Result := Round(GetAsFloat);
end;

function TConverter.GetAsString: string;
begin
  case FDataType of
    dtString: Result := GetString;
    dtInteger: Result := IntToStr(GetAsInteger);
    dtFloat: Result := FloatToString(GetAsFloat);
    dtDateTime: Result := DateTimeToString(GetAsDateTime);
    dtDate: Result := DateTimeToString(GetAsDate);
    dtTime: Result := DateTimeToString(GetAsTime);
    dtBoolean: Result := BoolToStr(GetAsBoolean);
  end;
end;

procedure TConverter.SetAsBoolean(Value: Boolean);
begin
  SetAsString(BoolToStr(Value));
end;

procedure TConverter.SetAsDateTime(Value: TDateTime);
begin
  SetAsString(DateTimeToStr(Value));
end;

procedure TConverter.SetAsDate(Value: TDateTime);
begin
  SetAsDateTime(Value);
end;

procedure TConverter.SetAsTime(Value: TDateTime);
begin
  SetAsDateTime(Value);
end;

procedure TConverter.SetAsFloat(Value: Double);
begin
  if FDataType in [dtDateTime, dtDate, dtTime] then
    SetAsDateTime(Value)
  else
    SetAsString(FloatToStr(Value));
end;

procedure TConverter.SetAsInteger(Value: LongInt);
begin
  if FDataType = dtInteger then
    SetAsString(IntToStr(Value))
  else
    SetAsFloat(Value);
end;

procedure TConverter.SetAsString(const Value: string);
var
  S: string;
begin
  S := GetString;
  SetString(Value);
  try
    CheckDataType;
    Change;
  except
    SetString(S);
    if RaiseOnError then raise;
  end;
end;

end.

