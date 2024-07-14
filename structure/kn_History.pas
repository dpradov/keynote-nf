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

{.$DEFINE DEBUG_HISTORY}

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
  _MAX_NAV_HISTORY = 20;
  _MAX_GLOBAL_NAV_HISTORY = 60;

type
  THistoryDirection = (hdBack, hdForward);


  TKntHistory = class( TObject )
  private
    fHistory : TLocationList;
    fIndex : integer;
    fMaxNavHistory: Integer;

  public
    property Index : integer read fIndex;

    constructor Create (MaxNavHistory: Integer= _MAX_NAV_HISTORY);
    destructor Destroy; override;

    function CanGoBack : boolean;
    function CanGoForward : boolean;

    function GoBack: TLocation;
    function GoForward: TLocation;
    function SyncWithLocation (const LocationToSync: TLocation; const Direction: THistoryDirection; const IterateAll: boolean): TLocation;

    function PickCurrent: TLocation;
    function PickBack : TLocation;
    function PickForward : TLocation;

    procedure Clear;

    procedure AddLocation( const aLocation : TLocation );
    procedure AddLocationMaintainingIndex( const aLocation : TLocation );

{$IFDEF DEBUG_HISTORY}
    function GetSummary: string;
    property Summary: string read GetSummary;
{$ENDIF}
  end;


implementation

constructor TkntHistory.Create (MaxNavHistory: Integer= _MAX_NAV_HISTORY);
begin
  inherited Create;
  fIndex := -1;
  fHistory := TLocationList.Create;
  fMaxNavHistory := MaxNavHistory;
end; // CREATE

destructor TkntHistory.Destroy;
begin
  Clear;
  fHistory.Free;
  inherited Destroy;
end; // DESTROY

function TkntHistory.CanGoBack : boolean;
begin
  result := (( fIndex >= 0 ) and ( fHistory.Count > 0 ));
end; // CanGoBack

function TkntHistory.CanGoForward : boolean;
begin
  result := (( fHistory.Count > 0 ) and
      ( fIndex < pred( pred( fHistory.Count ))));
end; // CanGoForward

function TkntHistory.GoBack : TLocation;
begin
  result := nil;
  if ( not CanGoBack ) then exit;
  result:= fHistory[fIndex];
  dec( fIndex );
end; // GoBack

function TkntHistory.GoForward : TLocation;
begin
  result := nil;
  if ( not CanGoForward ) then exit;
  result := fHistory[fIndex+2];

  inc( fIndex );
end; // GoForward


function TkntHistory.PickCurrent: TLocation;
begin
  result := nil;
  if not ((fHistory.Count > 0) and (fIndex+1 <= fHistory.Count-1)) then exit;
  result := fHistory[fIndex+1];
end;


function TkntHistory.PickBack: TLocation;
begin
  result := nil;
  if ( not CanGoBack ) then exit;
  result := fHistory[fIndex];
end;


function TkntHistory.PickForward : TLocation;
begin
  result := nil;
  if ( not CanGoForward ) then exit;
  result := fHistory[fIndex+2];
end;



function TKntHistory.SyncWithLocation (const LocationToSync: TLocation; const Direction: THistoryDirection; const IterateAll: boolean): TLocation;
var
  i: integer;
  sync: boolean;
  currentLoc: TLocation;

  {
   If the element to synchronize with is nil => we will act as a normal action (GoBack or GoForward)

   If there is indeed an item to sync with:
      First, we will advance in the same direction as indicated in the parameter if we will reach the element to be synchronized.
      If that is not the case:
       - IterateAll=False (we are going synchronizing the global history from a local history)
          We will not move in the global history, and will return nil or the current element (between the previous, back, and the subsequent,
          forward) if it corresponds to the same note and node of the element with which to synchronize.


      - IterateAll: true => The history is local to a note and we must synchronize it with the 'LocationToSync' element of the global history,
                            iterating over all its elements. In this case 'Direction' corresponds to the direction taken within the global history.

           We will go in the same direction indicated to its end, looking for an element equal to the one with which we are trying to synchronize,
           therefore considering node and caret position. If we do not find it, we will make another travel starting at the opposite end, until
           the position from which we started. In this second traversal, it is enough to find an element that matches at the node level; does not have
           why be in the same caret position.
  }


begin
    if LocationToSync = nil then begin
       if Direction = hdBack then
          Exit(GoBack)
       else
          Exit(GoForward);
    end;


    Result:= nil;

    if Direction = hdBack then begin
       if CanGoBack then
          if LocationToSync.Equal(PickBack) then
             Exit(GoBack);
    end
    else
    if CanGoForward then
        if LocationToSync.Equal(PickForward) then
            Exit(GoForward);

    if not IterateAll then begin
       currentLoc:= PickCurrent;
       if (currentLoc = nil) or (currentLoc.FolderID <> LocationToSync.FolderID) then Exit;
       if (currentLoc.NNodeID = LocationToSync.NNodeID) then Exit(currentLoc);
    end

    else begin              // IterateAll
        i:= fIndex;
        sync:= false;

        if Direction = hdBack then begin
           dec(i);
           while not sync and (i >= 0) do begin
              result := fHistory[i];
              if Result.FolderID <> LocationToSync.FolderID then Exit(nil);
              sync:= LocationToSync.Equal(Result);
              dec(i);
           end;
           if not sync then begin
              i:= fHistory.Count - 1;
              while not sync and (i > fIndex) do begin
                 result := fHistory[i];
                 if Result.FolderID <> LocationToSync.FolderID then Exit(nil);
                 sync:= LocationToSync.Equal(Result, false);                      // false: ignore caret
                 dec(i);
              end;
           end;

        end
        else begin   // Forward
           inc(i);

           while not sync and (i < pred(pred(fHistory.Count))) do begin
              result := fHistory[i+2];
              if Result.FolderID <> LocationToSync.FolderID then Exit(nil);
              sync:= LocationToSync.Equal(Result);
              inc(i);
           end;
           if not sync then begin
              i:= - 2;
              while not sync and (i < fIndex+2) and (i < fHistory.Count-2) do begin
                 result := fHistory[i+2];
                 if Result.FolderID <> LocationToSync.FolderID then Exit(nil);
                 sync:= LocationToSync.Equal(Result, false);
                 inc(i);
              end;
           end;

        end;

        if sync then
           fIndex:= i
        else
           Result:= nil;

    end;

end;



procedure TkntHistory.Clear;
begin
  fIndex := -1;
  ClearLocationList( fHistory );
end; // Clear

procedure TkntHistory.AddLocation( const aLocation : TLocation );
var
  i, cnt : integer;
begin
  if ( not assigned( aLocation )) then exit;
  inc( fIndex );

  if ( fIndex = fHistory.Count ) then
     fHistory.Add(aLocation )

  else begin
    fHistory[fIndex].Free; // remove existing
    fHistory[fIndex] := ALocation; // store

    // remove locations beyond current index
    if ( fIndex < pred( fHistory.Count )) then
       for i := pred( fHistory.Count ) downto succ( fIndex ) do begin
          fHistory[i].Free;
          fHistory.Delete(i);
       end;
  end;

  cnt := fHistory.Count;
  if ( cnt > FMaxNavHistory ) then begin
     i := 0;
     repeat
       fHistory[i].Free;
       fHistory.Delete( i );
       inc( i );
       dec( fIndex );
     until ( fHistory.Count = FMaxNavHistory );
  end;

end; // AddLocation

procedure TkntHistory.AddLocationMaintainingIndex( const aLocation : TLocation );
var
  i, cnt : integer;
begin
  if ( not assigned( aLocation )) then exit;
  inc( fIndex );

  if ( fIndex = fHistory.Count ) then
     fHistory.Add(aLocation )

  else begin
    fHistory[fIndex].Free; // remove existing
    fHistory[fIndex] := ALocation; // store
  end;

  dec( fIndex );

end;


{$IFDEF DEBUG_HISTORY}
function TkntHistory.GetSummary: string;
var
  I: Integer;
  SEP: String;
  BackItem: string;
begin
   SEP:= '';
   Result:= '';
   for I := 0 to fHistory.Count -1 do begin
      BackItem:= '';
      if fIndex = i then
         BackItem:= ' #';

      Result:= Result + SEP + BackItem + fHistory[i].NoteName + '(' + fHistory[i].CaretPos.ToString  + ')';
      SEP:= ', ';
   end;

end;
{$ENDIF}


end.
