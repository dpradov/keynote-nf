unit GFLog;

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

uses
   Winapi.Windows,
   Winapi.ShellAPI,
   Winapi.Messages,
   System.SysUtils,
   System.Classes,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   gf_misc;

type
  TGFLogOnAdding = procedure( Sender : TObject; var ALine : string; var OKToAdd : boolean ) of object;
  TGFLogOnAdded = procedure( Sender : TObject; const ALine : string ) of object;

type
  TGFLog = class( TComponent )
  private
    fActive : boolean;
    fMaxDbgLevel: integer;
    fMaxLines : integer;
    fLines : TStringList;
    fFileName : string;
    fDateStamp : boolean;
    fTimeStamp : boolean;
    fUniqueFileName : boolean;
    fAppendToFile : boolean;
    fModified : boolean;
    fDisplayControl : TCustomMemo; // TMemo, TRichEdit are descended from this
    fOnAdding : TGFLogOnAdding;
    fOnAdded : TGFLogOnAdded;
    fIDString : string;
    fLogWasSaved : boolean;
    fLastError : string;
    fShowErrors : boolean;
    fDeactivateOnError : boolean;
    //fSeparator : string;

    fAppStartTime: integer;
    fLastTick : integer;
    fIndentLevel: integer;
    fTicks: TList;


    procedure SetMaxLines( const AMaxLines : integer );
    procedure SetDisplayControl( const AControl : TCustomMemo );
    function AddLine( AStr : string; const DbgLevel: integer= 0 ) : integer;
    function GetLastError : string;

  public
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
    function Add( AStr : string; const DbgLevel: integer= 0 ) : integer;
    function AddEx( Sender : TObject; AStr : string; const DbgLevel: integer= 0 ) : integer;
    procedure Flush( const AndSave : boolean );
    procedure Save( const aFileName : string );
    // function SenderAdd( Sender : TObject; AStr : string ) : integer;

    procedure StoreTick( const Msg : string; DbgLevel: integer= 0; DetailLevel: integer = 0);

  published
    // properties
    property Active : boolean read fActive write fActive;
    property MaxDbgLevel : integer read fMaxDbgLevel write fMaxDbgLevel;
    property MaxLines : integer read fMaxLines write SetMaxLines;
    property Lines : TStringList read fLines;
    property FileName : string read fFileName write fFileName;
    property DateStamp : boolean read fDateStamp write fDateStamp;
    property TimeStamp : boolean read fTimeStamp write fTimeStamp;
    property UniqueFileName : boolean read fUniqueFileName write fUniqueFileName;
    property AppendToFile : boolean read fAppendToFile write fAppendToFile;
    property Modified : boolean read fModified;
    property DisplayControl : TCustomMemo read fDisplayControl write SetDisplayControl;
    property IDString : String read fIDString write fIDString;
    property LastError : string read GetLastError;
    property ShowErrors : boolean read fShowErrors write fShowErrors;
    property DeactivateOnError : boolean read fDeactivateOnError write fDeactivateOnError;
    //property Separator : string read fSeparator write fSeparator;
    // events
    property OnAdding : TGFLogOnAdding read fOnAdding write fOnAdding;
    property OnAdded : TGFLogOnAdded read fOnAdded write fOnAdded;
  end;

procedure Register;

implementation

uses knt.App;

const
  _MAXLOGLINES = 1024;

constructor TGFLog.Create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  fMaxLines := _MAXLOGLINES;
  fMaxDbgLevel:= integer.MaxValue;
  fLines := TStringList.Create;
  fAppStartTime:= GetTickCount;
  fLastTick:= fAppStartTime;
  fTicks:= TList.Create;

  with fLines do begin
    Sorted := false;
    Duplicates := dupAccept;
    Capacity := succ( fMaxLines );
  end;

  fLines.Add(#13 + 'LOG SESSION BEGINS ' + datetimetostr( now ) + ' (' + Name + ')' );
  fActive := true;
  fFileName := '';
  fDateStamp := true;
  fTimeStamp := true;
  fUniqueFileName := false;
  fAppendToFile := true;
  fModified := false;
  fDisplayControl := nil;
  fOnAdding := nil;
  fOnAdded := nil;
  fIDString := '';
  fLogWasSaved := false;
  fLastError := '';
  fShowErrors := true;
  fDeactivateOnError := false;
  fIndentLevel:= 0;
  //fSeparator := '----- LOG SESSION ENDS -----';
end; // CREATE

destructor TGFLog.Destroy;
begin
  // [x] save if necessary
  Flush( true );
  if assigned( fLines ) then fLines.Free;
  if assigned( fTicks ) then fTicks.Free;

  inherited Destroy;
end; // DESTROY

procedure TGFLog.SetMaxLines( const AMaxLines : integer );
begin
  if ( fMaxLines = AMaxLines ) then exit;
  fMaxLines := AMaxLines;
  // [x] if already has more lines than Max, update
end; // SetMaxLines

procedure TGFLog.SetDisplayControl( const AControl : TCustomMemo );
begin
  if ( fDisplayControl = AControl ) then exit;
  fDisplayControl := AControl;
  // [x]
end; // SetDisplayControl

function TGFLog.AddEx( Sender : TObject; AStr : string; const DbgLevel: integer= 0 ) : integer;
begin
  if DbgLevel > fMaxDbgLevel then exit;

  if assigned( Sender ) then
    result := AddLine( '(' + Sender.ClassName + ') ' + AStr, DbgLevel )
  else
    result := AddLine( '(nil object) ' + AStr, DbgLevel )
end; // AddEx

function TGFLog.Add( AStr : string; const DbgLevel: integer= 0 ) : integer;
begin
  result := AddLine( AStr, DbgLevel )
end; // ADD

{
function TGFLog.SenderAdd( Sender : TObject; AStr : string ) : integer;
begin
  result := AddLine( 'From ' + Sender.ClassName + ': ' + AStr )
end; // SenderAdd;
}

function TGFLog.AddLine( AStr : string; const DbgLevel: integer= 0  ) : integer;
var
  OKToAdd : boolean;
  myStr : string;
begin
  result := -1;
  if ( not fActive ) then exit;
  if DbgLevel > fMaxDbgLevel then exit;

  OKToAdd := true;

  if assigned( fOnAdding ) then
     fOnAdding( self, AStr, OkToAdd );

  if OKToAdd then begin
     myStr := fIDString;
     if fDateStamp then
        myStr := myStr + datetostr( now ) + ' ';

     if fTimeStamp then
        myStr := myStr + timetostr( now ) + '  ';

     result := fLines.Add( myStr + string.Create(' ', fIndentLevel*2) + AStr );
     if ( assigned( fOnAdded ) and ( result >= 0 )) then
        fOnAdded( self, fLines[result] );

     fModified := true;
     if (( fMaxLines > 0 ) and ( fLines.Count > fMaxLines )) then
        Flush( true );
  end;
end; // AddLine;



procedure TGFLog.StoreTick( const Msg : string; DbgLevel: integer= 0; DetailLevel: integer = 0);
const
  tab = #9;
var
  Tick, TickBeginDetailLevel, Duration, TotalDuration : integer;
  str: string;
begin
  // *1 Correct thing is and I will (and want) tend to, to use correctly typed lists (for example, with Generics), but I don't want to cause code bloat..

  if not fActive then exit;

  if DbgLevel <= fMaxDbgLevel then begin
     if Msg = '' then begin
        fLastTick:= GetTickCount;
        exit;
     end;
     Tick:= GetTickCount;
     Duration := Tick - fLastTick;
     fLastTick := Tick;

     str:= Format( '%s:%s%d ms', [Msg, tab, Duration] );

     if DetailLevel > 0 then
        fTicks.Add(Pointer(fLastTick))               // *1

     else if (DetailLevel <0) and (fIndentLevel > 0) then begin
        dec(fIndentLevel);
        TickBeginDetailLevel:= integer(fTicks.Last);    // *1
        TotalDuration := Tick - TickBeginDetailLevel;
        fTicks.Delete(fTicks.Count-1);
        str:= str + ' [Total Time (ms): ' + TotalDuration.ToString + ']';
     end;

     AddLine(str, DbgLevel);

     if DetailLevel > 0 then
        inc(fIndentLevel);
  end;
end;



procedure TGFLog.Flush( const AndSave : boolean );
begin
  if AndSave then
    Save( '' )
  else
    fModified := false;
  if ( not fModified ) then
    fLines.Clear;
end; // Flush


procedure TGFLog.Save( const aFileName : string );
var
  LFile : textfile;
  i : integer;
begin
  if (( not fActive ) or ( not fModified )) then exit;

  if ( fFileName = '' ) then
    fFileName := aFileName;

  if ( fFileName = '' ) then
    fFileName := normalFN( changefileext( ParamStr( 0 ), '.log' ));

  assignfile( LFile, fFileName );

  try
    if fileexists( fFileName ) then begin
      if ( fLogWasSaved or fAppendToFile ) then
        append( LFile )
      else
        rewrite( LFile );
    end
    else
      rewrite( LFile );
  except
    on E : Exception do begin
      fLastError := E.Message;
      if fDeactivateOnError then
        fActive := false;
      if fShowErrors then
        App.ErrorPopup( Name + ' Log error: Cannot REWRITE' +#13+ fLastError);
    end;
  end;

  try
    try
      if ( fLines.Count > 0 ) then begin
        for i := 0 to pred( fLines.Count ) do
          writeln( LFile, fLines[i] );
        //writeln( LFile, fSeparator );
        //writeln( LFile );
      end;

    except
      on E : Exception do begin
        fLastError := E.Message;
        if fDeactivateOnError then
          fActive := false;
        if fShowErrors then
          App.ErrorPopup( Name + ' Log error: Cannot SAVE' +#13+ fLastError);
      end;
    end;

  finally
    fLogWasSaved := true;
    closefile( LFile );
  end;

  fModified := false;
end; // Save


function TGFLog.GetLastError : string;
begin
  result := fLastError;
  fLastError := '';
end; // GetLastError



procedure Register;
begin
  RegisterComponents( 'Samples', [TGFLog] );
end;



end.
