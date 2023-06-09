unit gf_LangCombo;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland) [mj]
 
 Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
 in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 

{$I gf_base.inc}

(*
 ----------------------------------------------------------
  Module    : gf_LangCombo
  Purpose   : TCustomComboBox descendant to display list
              of installed/supported languages.
              Partly based on "langs.pas" by Alexander Obukhov
              <alex@niiomr.belpak.minsk.by>. Removed bitmap support
              (AV when compiled with D5, which I have no time to fix).
              Removed design-time property editor (incompatible with D6).
  Version   : 1.0
  Created   : 31-08-2002 
 -----------------------------------------------------------
  REQUIREMENTS:
  - gf_Lang.pas
 -----------------------------------------------------------
  TO DO:
  - would be nice to restore the optional flag bitmaps
 -----------------------------------------------------------
*)

interface

uses
   Winapi.Windows,
   Winapi.Messages,
   System.SysUtils,
   System.Classes,
   System.AnsiStrings,
   Vcl.Controls,
   Vcl.StdCtrls,
   Vcl.Graphics,
   gf_Lang;


type
  TgfLangCombo = class(TCustomComboBox)
  private
    { Private declarations }
    FLanguageType: TgfLanguageType;
    FOnChange: TNotifyEvent;
    FLanguageKind: TgfLanguageKind;

    procedure Reset;
    function GetLanguage: TgfLanguage;
    procedure SetLanguage(const Value: TgfLanguage);
    function GetLanguageCount: Integer;
    procedure SetLanguageType(const Value: TgfLanguageType);
    function GetLanguageName: String;
    procedure SetLanguageKind(const Value: TgfLanguageKind);
    procedure SetLanguageName(const Value: String);

  protected
    { Protected declarations }
    procedure Change; override;
    procedure BuildList; virtual;
    procedure CreateWnd; override;
    procedure Click; override;
  public
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
  published
    { Published declarations }
    property Language : TgfLanguage read GetLanguage write SetLanguage;
    property LanguageName : String read GetLanguageName write SetLanguageName;
    property LanguageCount : Integer read GetLanguageCount;
    property LanguageType : TgfLanguageType read FLanguageType write SetLanguageType;
    property LanguageKind : TgfLanguageKind read FLanguageKind write SetLanguageKind;

    {$IFDEF DELPHI_5_UP}
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property Constraints;
    {$ENDIF}
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property HelpContext;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Custom', [TgfLangCombo]);
end;

{ TgfLangCombo }

constructor TgfLangCombo.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );
  FLanguageType := ltSupported;
  FLanguageKind := lkLocalized;
  Style := csDropDownList;
  Sorted := True;
  FOnChange := nil;
end;

procedure TgfLangCombo.Click;
begin
  inherited Click;
  Change;
end;

procedure TgfLangCombo.CreateWnd;
var
  OldLocale : TgfLanguage;
begin
  inherited CreateWnd;
  OldLocale := Language;
  BuildList;
  SetLanguage( OldLocale );
end;

function TgfLangCombo.GetLanguage: TgfLanguage;
begin
  if ( ItemIndex > -1 ) then
    result := Integer( Items.Objects[ItemIndex] )
  else
    result := LOCALE_SYSTEM_DEFAULT;
end;

function TgfLangCombo.GetLanguageCount: Integer;
begin
  result := Items.Count;
end;


function TgfLangCombo.GetLanguageName: String;
begin
  result := Text;
end;

procedure TgfLangCombo.SetLanguage(const Value: TgfLanguage);
var
  i : integer;
  name : string;
begin
  name := SysLanguageName( Value, FLanguageKind );
  i :=  Items.IndexOf( Name );
  ItemIndex := i;
  Change;
end;


procedure TgfLangCombo.SetLanguageKind(const Value: TgfLanguageKind);
begin
  if ( Value <> FLanguageKind ) then
  begin
   FLanguageKind := Value;
   Reset;
  end;
end;

procedure TgfLangCombo.SetLanguageName(const Value: String);
begin
  ItemIndex := Items.IndexOf( Value );
end;

procedure TgfLangCombo.SetLanguageType(const Value: TgfLanguageType);
begin
  if ( FLanguageType <> Value ) then
  begin
    FLanguageType := Value;
    Reset;
  end;
end;

procedure TgfLangCombo.Change;
begin
  if assigned( FOnChange ) then
    FOnChange( self );
end;

procedure TgfLangCombo.Reset;
var
  SaveLCID: TgfLanguage;
begin
  SaveLCID := 0;
  try
    SaveLCID := Language;
    BuildList;
    Language := SaveLCID;
  finally
    if ( Language <> SaveLCID ) then
      Change;
  end;
end;

var
  SList: TStringList;
  LCType: Integer;
  PrimaryOnly: Boolean;

function EnumLocalesProc( LocaleId : LPStr ) : Integer; stdcall;
var
  Locale: LCID;
  Lang: String;
  Z: Integer;
  Buf: array[0..255] of Char;
begin
  Val( '$'+ System.AnsiStrings.StrPas( LocaleId ), Locale, Z );
  if (( Locale < 2048 ) or ( not PrimaryOnly )) then
    begin
      SetLength( Lang, 255 );
      GetLocaleInfo( Locale, LCType, Buf, 255 );
      SetString( Lang, Buf, StrLen( Buf ));
      SList.AddObject( Lang, Pointer( Locale ));
    end;
  Result:= 1;
end;

procedure TgfLangCombo.BuildList;
var
  LT : Integer;
begin
  LT := 0;
  Items.Clear;

  case FLanguageType of
    ltInstalled, ltPrimary : LT := LCID_Installed;
    ltSupported : LT := LCID_Supported;
  end;

  case FLanguageKind of
    lkLocalized : LCType := LOCALE_SLanguage;
    lkEnglish   : LCType := LOCALE_SEngLanguage;
    lkNative    : LCType := LOCALE_SNativeLangName;
    lkAbbrev    : LCType := LOCALE_SAbbrevLangName;
  end;

  PrimaryOnly := ( LanguageType = ltPrimary );
  SList := TStringList.Create;
  try
    EnumSystemLocales( @EnumLocalesProc, LT );
  except
  end;
  Items.Assign( SList );
  SList.Free;
end;


end.


