unit kn_History;

(* ************************************************************
 KEYNOTE: MOZILLA PUBLIC LICENSE STATEMENT.
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is KeyNote 1.0.

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.      
 -----------------------------------------------------------
 Contributor(s):                      
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 Released: 30 June 2001
 -----------------------------------------------------------
 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <cicho@tenbit.pl>

************************************************************ *)

(*
How this works:

- FIndex always points to the location to which we will go
  when moving BACK in navigation history. Hence, FIndex+1
  is the current item (we don't _go_ to it), and FIndex+2
  is the item to which we will go when moving FORWARD.

- When FIndex is -1, there are no history items at all

*)

interface
uses Windows, Classes, Graphics,
  SysUtils, kn_LocationObj,
  gf_misc, kn_Const, kn_Info, WideStrings;

const
  _MAX_NAV_HISTORY = 500;

type
  TkntHistory = class( TObject )
  private
    FHistory : TWideStringList;
    FIndex : integer;

  public
    property Index : integer read FIndex;

    constructor Create;
    destructor Destroy; override;

    function CanGoBack : boolean;
    function CanGoForward : boolean;

    function GoBack : TLocation;
    function GoForward : TLocation;

    procedure Clear;

    procedure AddLocation( const aLocation : TLocation );


  end;


implementation

constructor TkntHistory.Create;
begin
  inherited Create;
  FIndex := -1;
  FHistory := TWideStringList.Create;
end; // CREATE

destructor TkntHistory.Destroy;
begin
  Clear;
  FHistory.Free;
  inherited Destroy;
end; // DESTROY

function TkntHistory.CanGoBack : boolean;
begin
  result := (( FIndex >= 0 ) and ( FHistory.Count > 0 ));
end; // CanGoBack

function TkntHistory.CanGoForward : boolean;
begin
  result := (( FHistory.Count > 0 ) and
      ( FIndex < pred( pred( FHistory.Count ))));
end; // CanGoForward

function TkntHistory.GoBack : TLocation;
begin
  result := nil;
  if ( not CanGoBack ) then exit;
  result := TLocation( FHistory.objects[FIndex] );
  dec( FIndex );
end; // GoBack

function TkntHistory.GoForward : TLocation;
begin
  result := nil;
  if ( not CanGoForward ) then exit;
  inc( FIndex );
  result := TLocation( FHistory.objects[succ( FIndex )] );
end; // GoForward

procedure TkntHistory.Clear;
begin
  FIndex := -1;
  ClearLocationList( FHistory );
end; // Clear

procedure TkntHistory.AddLocation( const aLocation : TLocation );
var
  i, cnt : integer;
begin
  if ( not assigned( aLocation )) then exit;
  inc( FIndex );
  if ( FIndex = FHistory.Count ) then
  begin
    FHistory.AddObject( aLocation.NoteName, aLocation );
  end
  else
  begin
    FHistory.Objects[FIndex].Free; // remove existing
    FHistory.Objects[FIndex] := ALocation; // store

    // remove locations beyond current index
    if ( FIndex < pred( FHistory.Count )) then
    begin
      for i := pred( FHistory.Count ) downto succ( FIndex ) do
      begin
        FHistory.Objects[i].Free;
        FHistory.Delete( i );
      end;
    end;
  end;

  cnt := FHistory.Count;
  if ( cnt > _MAX_NAV_HISTORY ) then
  begin
    i := 0;
    repeat
      FHistory.Objects[i].Free;
      FHistory.Delete( i );
      inc( i );
      dec( FIndex );
    until ( FHistory.Count = _MAX_NAV_HISTORY );
  end;

end; // AddLocation

end.
