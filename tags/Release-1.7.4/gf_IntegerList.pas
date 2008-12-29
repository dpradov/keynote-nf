unit gf_IntegerList;
{$I gf_base.inc}

(* ************************************************************
 MOZILLA PUBLIC LICENSE STATEMENT
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is "gf_HTML.pas".

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 To do:
 -----------------------------------------------------------
 Released: 20 August 2001
 -----------------------------------------------------------
 URLs:

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <cicho@tenbit.pl>

************************************************************ *)


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

