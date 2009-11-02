
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

unit GFLog;
{$I gf_base.inc}

interface

uses
  Windows, ShellAPI, Messages,
  Dialogs, SysUtils, Classes,
  StdCtrls, gf_misc;

type
  TGFLogOnAdding = procedure( Sender : TObject; var ALine : string; var OKToAdd : boolean ) of object;
  TGFLogOnAdded = procedure( Sender : TObject; const ALine : string ) of object;

type
  TGFLog = class( TComponent )
  private
    fActive : boolean;
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
    fSeparator : string;

    procedure SetMaxLines( const AMaxLines : integer );
    procedure SetDisplayControl( const AControl : TCustomMemo );
    function AddLine( AStr : string ) : integer;
    function GetLastError : string;

  public
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
    function Add( AStr : string ) : integer;
    function AddEx( Sender : TObject; AStr : string ) : integer;
    procedure Flush( const AndSave : boolean );
    procedure Save( const aFileName : string );
    // function SenderAdd( Sender : TObject; AStr : string ) : integer;

  published
    // properties
    property Active : boolean read fActive write fActive;
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
    property Separator : string read fSeparator write fSeparator;
    // events
    property OnAdding : TGFLogOnAdding read fOnAdding write fOnAdding;
    property OnAdded : TGFLogOnAdded read fOnAdded write fOnAdded;
  end;

procedure Register;

implementation

const
  _MAXLOGLINES = 1024;

constructor TGFLog.Create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  fMaxLines := _MAXLOGLINES;
  fLines := TStringList.Create;
  with fLines do
  begin
    Sorted := false;
    Duplicates := dupAccept;
    Capacity := succ( fMaxLines );
  end;
  fLines.Add( 'LOG SESSION BEGINS ' + datetimetostr( now ) + ' (' + Name + ')' );
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
  fSeparator := '----- LOG SESSION ENDS -----';
end; // CREATE

destructor TGFLog.Destroy;
begin
  // [x] save if necessary
  Flush( true );
  if assigned( fLines ) then fLines.Free;
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

function TGFLog.AddEx( Sender : TObject; AStr : string ) : integer;
begin
  if assigned( Sender ) then
    result := AddLine( '(' + Sender.ClassName + ') ' + AStr )
  else
    result := AddLine( '(nil object) ' + AStr )
end; // AddEx

function TGFLog.Add( AStr : string ) : integer;
begin
  result := AddLine( AStr )
end; // ADD

{
function TGFLog.SenderAdd( Sender : TObject; AStr : string ) : integer;
begin
  result := AddLine( 'From ' + Sender.ClassName + ': ' + AStr )
end; // SenderAdd;
}

function TGFLog.AddLine( AStr : string ) : integer;
var
  OKToAdd : boolean;
  myStr : string;
begin
  result := -1;
  if ( not fActive ) then exit;
  OKToAdd := true;
  if assigned( fOnAdding ) then
    fOnAdding( self, AStr, OkToAdd );
  if OKToAdd then
  begin
    myStr := fIDString;
    if fDateStamp then
      myStr := myStr + datetostr( now ) + #32;
    if fTimeStamp then
      myStr := myStr + timetostr( now ) + #32;
    result := fLines.Add( myStr + AStr );
    if ( assigned( fOnAdded ) and ( result >= 0 )) then
      fOnAdded( self, fLines[result] );
    fModified := true;
    if (( fMaxLines > 0 ) and ( fLines.Count > fMaxLines )) then
    begin
      Flush( true );
    end;
  end;
end; // AddLine;

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
    if fileexists( fFileName ) then
    begin
      if ( fLogWasSaved or fAppendToFile ) then
        append( LFile )
      else
        rewrite( LFile );
    end
    else
    begin
      rewrite( LFile );
    end;
  except
    on E : Exception do
    begin
      fLastError := E.Message;
      if fDeactivateOnError then
        fActive := false;
      if fShowErrors then
        messagedlg( Name + ' Log error: Cannot REWRITE' +#13+ fLastError, mtError, [mbOK], 0 );
    end;
  end;

  try
    try
      if ( fLines.Count > 0 ) then
      begin
        for i := 0 to pred( fLines.Count ) do
          writeln( LFile, fLines[i] );
        writeln( LFile, fSeparator );
        writeln( LFile );
      end;
    except
      on E : Exception do
      begin
        fLastError := E.Message;
        if fDeactivateOnError then
          fActive := false;
        if fShowErrors then
          messagedlg( Name + ' Log error: Cannot SAVE' +#13+ fLastError, mtError, [mbOK], 0 );
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
