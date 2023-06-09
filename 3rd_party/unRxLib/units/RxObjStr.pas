{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxObjStr;

interface

{$I RX.INC}

uses
  {$IFDEF VER80}WinTypes, WinProcs, {$ENDIF}SysUtils, Classes;

type

{ TObjectStrings }

  TDestroyEvent = procedure(Sender, AObject: TObject) of object;
  TObjectSortCompare = function(const S1, S2: string;
    Item1, Item2: TObject): Integer of object;

  TObjectStrings = class(TStringList)
  private
    FOnDestroyObject: TDestroyEvent;
  protected
    procedure DestroyObject(AObject: TObject); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Remove(Index: Integer);
    procedure ParseStrings(const Values: string);
    procedure SortList(Compare: TObjectSortCompare);
    property OnDestroyObject: TDestroyEvent read FOnDestroyObject
      write FOnDestroyObject;
  end;

{ THugeList class }

const
  {$IFNDEF VER80}
  MaxHugeListSize = {$IFDEF RX_D16}MaxInt div 16{$ELSE}MaxListSize{$ENDIF};
  {$ELSE}
  MaxHugeListSize = (MaxLongInt div SizeOf(Pointer)) - 4;
  {$ENDIF}

type
  {$IFNDEF VER80}
  THugeList = class(TList);
  {$ELSE}
  THugeList = class(TObject)
  private
    FList: TMemoryStream;
    FCount: LongInt;
    FCapacity: LongInt;
  protected
    function Get(Index: LongInt): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: LongInt; Item: Pointer);
    procedure SetCapacity(NewCapacity: LongInt);
    procedure SetCount(NewCount: LongInt);
  public
    destructor Destroy; override;
    function Add(Item: Pointer): LongInt;
    procedure Clear;
    procedure Delete(Index: LongInt);
    procedure Exchange(Index1, Index2: LongInt);
    function Expand: THugeList;
    function First: Pointer;
    function IndexOf(Item: Pointer): LongInt;
    procedure Insert(Index: LongInt; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: LongInt);
    function Remove(Item: Pointer): LongInt;
    procedure Pack;
    property Capacity: LongInt read FCapacity write SetCapacity;
    property Count: LongInt read FCount write SetCount;
    property Items[Index: LongInt]: Pointer read Get write Put; default;
  end;
  {$ENDIF}

  {$IFNDEF VER80}

{ TSortCollection }

type
  TItemSortCompare = function(Item1, Item2: TCollectionItem): Integer of object;

  TSortCollection = class(TCollection)
  protected
    procedure QuickSort(L, R: Integer; Compare: TItemSortCompare); virtual;
  public
    procedure Sort(Compare: TItemSortCompare);
  end;

  {$ENDIF}

implementation

uses
  {$IFDEF VER80}VCLUtils, {$ENDIF}Consts,
  {$IFDEF RX_D6}RTLConsts, {$ENDIF}RxStrUtils; // Polaris

{ TObjectStrings }

procedure QuickSort(SortList: TStrings; L, R: Integer;
  SCompare: TObjectSortCompare);
var
  I, J: Integer;
  P: TObject;
  S: string;
begin
  repeat
    I := L;
    J := R;
    P := SortList.Objects[(L + R) shr 1];
    S := SortList[(L + R) shr 1];
    repeat
      while SCompare(SortList[I], S, SortList.Objects[I], P) < 0 do
        Inc(I);
      while SCompare(SortList[J], S, SortList.Objects[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        SortList.Exchange(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TObjectStrings.DestroyObject(AObject: TObject);
begin
  if Assigned(FOnDestroyObject) then
    FOnDestroyObject(Self, AObject)
  else if AObject <> nil then
    AObject.Free;
end;

procedure TObjectStrings.Clear;
var
  I: Integer;
begin
  if Count > 0 then
  begin
    Changing;
    for I := 0 to Count - 1 do
      Objects[I] := nil;
    BeginUpdate;
    try
      inherited Clear;
    finally
      EndUpdate;
    end;
    Changed;
  end;
end;

procedure TObjectStrings.Delete(Index: Integer);
begin
  Objects[Index] := nil;
  inherited Delete(Index);
end;

procedure TObjectStrings.Remove(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TObjectStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  if CurIndex <> NewIndex then
  begin
    TempString := Get(CurIndex);
    TempObject := GetObject(CurIndex);
    inherited Delete(CurIndex);
    try
      InsertObject(NewIndex, TempString, TempObject);
    except
      DestroyObject(TempObject);
      raise;
    end;
  end;
end;

procedure TObjectStrings.PutObject(Index: Integer; AObject: TObject);
begin
  Changing;
  BeginUpdate;
  try
    if (Index < Self.Count) and (Index >= 0) then
      DestroyObject(Objects[Index]);
    inherited PutObject(Index, AObject);
  finally
    EndUpdate;
  end;
  Changed;
end;

procedure TObjectStrings.ParseStrings(const Values: string);
var
  Pos: Integer;
begin
  Pos := 1;
  BeginUpdate;
  try
    while Pos <= Length(Values) do
      Add(ExtractSubstr(Values, Pos, [';']));
  finally
    EndUpdate;
  end;
end;

procedure TObjectStrings.SortList(Compare: TObjectSortCompare);
begin
  if Sorted then
    {$IFDEF RX_D3}
    Error(SSortedListError, 0);
  {$ELSE}
    raise EListError.Create({LoadStr(}SSortedListError {)});
  {$ENDIF}
  if Count > 0 then
  begin
    BeginUpdate;
    try
      QuickSort(Self, 0, Count - 1, Compare);
    finally
      EndUpdate;
    end;
  end;
end;

{$IFDEF VER80}

{ THugeList }

function ReturnAddr: Pointer; assembler;
asm
        MOV     AX,[BP].Word[2]
        MOV     DX,[BP].Word[4]
end;

procedure ListError(Index: LongInt);
begin
  raise EListError.Create({LoadStr(}SListIndexError {)} +
    Format(' (%d)', [Index]))at ReturnAddr;
end;

destructor THugeList.Destroy;
begin
  Clear;
end;

function THugeList.Add(Item: Pointer): LongInt;
begin
  Result := FCount;
  if Result = FCapacity then Grow;
  FList.Position := Result * SizeOf(Pointer);
  FList.WriteBuffer(Item, SizeOf(Pointer));
  Inc(FCount);
end;

procedure THugeList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure THugeList.Delete(Index: LongInt);
begin
  if (Index < 0) or (Index >= FCount) then ListError(Index);
  Dec(FCount);
  if Index < FCount then
    HugeMove(FList.Memory, Index, Index + 1, FCount - Index);
end;

function THugeList.Get(Index: LongInt): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then ListError(Index);
  FList.Position := Index * SizeOf(Pointer);
  FList.ReadBuffer(Result, SizeOf(Pointer));
end;

procedure THugeList.Put(Index: LongInt; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then ListError(Index);
  FList.Position := Index * SizeOf(Pointer);
  FList.WriteBuffer(Item, SizeOf(Pointer));
end;

procedure THugeList.Exchange(Index1, Index2: LongInt);
var
  Item: Pointer;
begin
  Item := Get(Index1);
  Put(Index1, Get(Index2));
  Put(Index2, Item);
end;

function THugeList.Expand: THugeList;
begin
  if FCount = FCapacity then Grow;
  Result := Self;
end;

function THugeList.First: Pointer;
begin
  Result := Get(0);
end;

procedure THugeList.Grow;
var
  Delta: LongInt;
begin
  if FCapacity > 8 then
    Delta := 16
  else if FCapacity > 4 then
    Delta := 8
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function THugeList.IndexOf(Item: Pointer): LongInt;
begin
  Result := 0;
  while (Result < FCount) and (Get(Result) <> Item) do
    Inc(Result);
  if Result = FCount then Result := -1;
end;

procedure THugeList.Insert(Index: LongInt; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then ListError(Index);
  if FCount = FCapacity then Grow;
  if Index < FCount then
    HugeMove(FList.Memory, Index + 1, Index, FCount - Index);
  FList.Position := Index * SizeOf(Pointer);
  FList.WriteBuffer(Item, SizeOf(Pointer));
  Inc(FCount);
end;

function THugeList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure THugeList.Move(CurIndex, NewIndex: LongInt);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then ListError(NewIndex);
    Item := Get(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end;

function THugeList.Remove(Item: Pointer): LongInt;
begin
  Result := IndexOf(Item);
  if Result <> -1 then Delete(Result);
end;

procedure THugeList.Pack;
var
  I: LongInt;
begin
  for I := FCount - 1 downto 0 do
    if Items[I] = nil then Delete(I);
end;

procedure THugeList.SetCapacity(NewCapacity: LongInt);
var
  NewList: TMemoryStream;
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxHugeListSize) then
    ListError(NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    if NewCapacity = 0 then
      NewList := nil
    else
    begin
      NewList := TMemoryStream.Create;
      NewList.SetSize(NewCapacity * SizeOf(Pointer));
      if FCount <> 0 then
      begin
        FList.Position := 0;
        FList.ReadBuffer(NewList.Memory^, FCount * SizeOf(Pointer));
      end;
    end;
    if FCapacity <> 0 then FList.Free;
    FList := NewList;
    FCapacity := NewCapacity;
  end;
end;

procedure THugeList.SetCount(NewCount: LongInt);
begin
  if (NewCount < 0) or (NewCount > MaxHugeListSize) then
    ListError(NewCount);
  if NewCount > FCapacity then SetCapacity(NewCount);
  FCount := NewCount;
end;

{$ENDIF}

{$IFNDEF VER80}

{ TSortCollection }

procedure TSortCollection.QuickSort(L, R: Integer; Compare: TItemSortCompare);
var
  I, J: Integer;
  P, P1, P2: TCollectionItem;
begin
  repeat
    I := L;
    J := R;
    P := Items[(L + R) shr 1];
    repeat
      while Compare(Items[I], P) < 0 do
        Inc(I);
      while Compare(Items[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        P1 := Items[I];
        P2 := Items[J];
        P1.Index := J;
        P2.Index := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, Compare);
    L := I;
  until I >= R;
end;

procedure TSortCollection.Sort(Compare: TItemSortCompare);
begin
  if Count > 0 then
  begin
    BeginUpdate;
    try
      QuickSort(0, Count - 1, Compare);
    finally
      EndUpdate;
    end;
  end;
end;

{$ENDIF}

end.