unit kn_History;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 


(*
How this works:

- FIndex always points to the location to which we will go
  when moving BACK in navigation history. Hence, FIndex+1
  is the current item (we don't _go_ to it), and FIndex+2
  is the item to which we will go when moving FORWARD.

- When FIndex is -1, there are no history items at all

*)

interface
uses
   Winapi.Windows,
   System.Classes,
   System.SysUtils,
   Vcl.Graphics,
   kn_LocationObj;


const
  _MAX_NAV_HISTORY = 500;

type
  TkntHistory = class( TObject )
  private
    FHistory : TStringList;
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
  FHistory := TStringList.Create;
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
