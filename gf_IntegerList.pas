unit gf_IntegerList;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 
 Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
 in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 

{$I gf_base.inc}

interface
uses Classes;

type
  TInteger = class( TObject )
  private
    FValue : integer;
  public
    property Value : integer read FValue write FValue;
    constructor Create;
  end;

type
  TIntegerList = class( TList )
  private

    FOwnsObjects : boolean;

    function GetItems(Index: Integer): TInteger;
    procedure SetItems(Index: Integer; AInteger : TInteger );

    procedure ClearItems;

  public
    property Items[Index: Integer]: TInteger read GetItems write SetItems; default;
    property OwnsObjects : boolean read FOwnsObjects write FOwnsObjects;

    constructor Create;
    destructor Destroy; override;

    function Add(AInteger: TInteger ): Integer;
    function Remove(AInteger: TInteger ): Integer;
    procedure Delete( AIndex : integer );
    function IndexOf(AInteger: TInteger): Integer;
    procedure Insert(Index: Integer; AInteger: TInteger);
    procedure Clear;

  end;



implementation

{ TInteger }

constructor TInteger.Create;
begin
  FValue := 0;
end; // Create

{ TIntegerList }

constructor TIntegerList.Create;
begin
  Inherited Create;
  FOwnsObjects := true;
end; // CREATE

destructor TIntegerList.Destroy;
begin
  if FOwnsObjects then
    ClearItems;
  inherited Destroy;
end; // DESTROY

procedure TIntegerList.ClearItems;
var
  i : integer;
begin
  for i := 1 to Count do
  begin
    Items[pred( i )].Free;
    Items[pred( i )] := nil;
  end;
end; // ClearItems

function TIntegerList.Add(AInteger: TInteger): Integer;
begin
  Result := inherited Add(AInteger);
end;

function TIntegerList.GetItems(Index: Integer): TInteger;
begin
  Result := TInteger(inherited Items[Index]);
end;

function TIntegerList.IndexOf(AInteger: TInteger): Integer;
begin
  Result := inherited IndexOf(AInteger);
end;

procedure TIntegerList.Insert(Index: Integer; AInteger: TInteger);
begin
  inherited Insert(Index, AInteger);
end;

function TIntegerList.Remove(AInteger: TInteger): Integer;
var
  myInteger : TInteger;
begin
  myInteger := AInteger;
  Result := inherited Remove(AInteger);
  if ( FOwnsObjects and assigned( myInteger )) then
    myInteger.Free;
end;

procedure TIntegerList.Delete( AIndex : integer );
begin
  if (( AIndex >= 0 ) and ( AIndex < Count )) then
  begin
    if FOwnsObjects then
      Items[AIndex].Free;
  end;
  inherited Delete( AIndex );
end; // Delete

procedure TIntegerList.SetItems(Index: Integer; AInteger: TInteger);
begin
  inherited Items[Index] := AInteger;
end;

procedure TIntegerList.Clear;
begin
  if FOwnsObjects then
    ClearItems;
  inherited Clear;
end; // Clear

end.

