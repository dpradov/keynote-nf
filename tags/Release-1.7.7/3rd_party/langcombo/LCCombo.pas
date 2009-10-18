unit LCCombo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Langs;

type
  TLanguageType = (ltInstalled, ltSupported, ltPrimary);
  TLanguageViewType = (lvtLocalized, lvtEnglish, lvtNative, lvtAbbrev);

  TLanguagesCombo = class(TCustomComboBox)
  private
    { Private declarations }
    FLanguageType: TLanguageType;
    FOnChange: TNotifyEvent;
    FViewType: TLanguageViewType;
    FShowFlag: Boolean;
    FlagBmp: TBitmap;
    procedure Reset;
    procedure Change; dynamic;
    procedure SetLanguage(Value: TLanguage);
    function GetLanguage: TLanguage;
    procedure SetLanguageName(const NewName: String);
    function GetLanguageName: String;
    procedure SetViewType(VT: TLanguageViewType);
    procedure SetLanguageType(LT: TLanguageType);
    function GetLanguageCount: Integer;
    procedure SetShowFlag(Value: Boolean);
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
  protected
    { Protected declarations }
    procedure BuildList; virtual;
    procedure CreateWnd; override;
    procedure Click; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Language: TLanguage read GetLanguage write SetLanguage;
    property LanguageName: String read GetLanguageName write SetLanguageName;
    property LanguageCount: Integer read GetLanguageCount;
    property LanguageType: TLanguageType read FLanguageType write SetLanguageType;
    property ViewType: TLanguageViewType read FViewType write SetViewType;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowFlag: Boolean read FShowFlag write SetShowFlag;
    property ShowHint;
    property TabOrder;
    property TabStop;
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

{$R Flags.res}

procedure Register;
begin
  RegisterComponents('Additional', [TLanguagesCombo]);
end;

constructor TLanguagesCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLanguageType := ltInstalled;
  FViewType := lvtLocalized;
  Style := csOwnerDrawFixed;
  FlagBmp:= TBitmap.Create;
  FlagBmp.Transparent:= True;
  FlagBmp.TransparentColor:= clFuchsia;;
  Sorted := True;
end;

destructor TLanguagesCombo.Destroy;
begin
  if Assigned(FlagBmp) then
    FlagBmp.Free;
  inherited;
end;

procedure TLanguagesCombo.CreateWnd;
var
 OldLocale: TLanguage;
begin
  inherited CreateWnd;
  OldLocale:= Language;
  BuildList;
  SetLanguage(OldLocale);
end;

procedure TLanguagesCombo.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  L: Integer;
  C: TColor;
begin
  with Canvas do
    begin
      FillRect(Rect);
      C:= Font.Color;
      if ShowFlag then
        L:= Rect.Left+24
      else
        L:= Rect.Left+2;
      if ViewType=lvtNative then
        Font.Charset:= CharSetFromLocale(Integer(Items.Objects[Index]));
      Font.Color:= C;
      TextOut(L, Rect.Top, Items[Index]);
      if ShowFlag then
        begin
          FlagBmp.LoadFromResourceID(HInstance, Integer(Items.Objects[Index]));
          Draw(Rect.Left+2, Rect.Top+(Rect.Bottom-Rect.Top-FlagBmp.Height) div 2, FlagBmp);
        end;
    end;
end;

procedure TLanguagesCombo.SetLanguage(Value: TLanguage);
var
  LT: Integer;
begin
  case LanguageType of
  ltInstalled:
    LT:= LCID_Installed;
  ltSupported:
    LT:= LCID_Supported;
  end;
  if IsValidLocale(Value, LT) and (Value<>Language) then
    begin
      ItemIndex:=  Items.IndexOfObject(Pointer(Value));
      Change;
    end;
end;

function TLanguagesCombo.GetLanguage: TLanguage;
begin
  if ItemIndex>-1 then
    Result:= Integer(Items.Objects[ItemIndex])
  else
    Result:= LOCALE_SYSTEM_DEFAULT;
end;

procedure TLanguagesCombo.SetLanguageName(const NewName: String);
begin
  ItemIndex:= Items.IndexOf(NewName);
end;

function TLanguagesCombo.GetLanguageName: String;
begin
  Result:= Text;
end;

function TLanguagesCombo.GetLanguageCount: Integer;
begin
  Result:= Items.Count;
end;

var
  SList: TStringList;
  LCType: Integer;
  PrimaryOnly: Boolean;

function EnumLocalesProc(LocaleId: LPStr): Integer;
stdcall;
var
  Locale: LCID;
  Lang: String;
  Z: Integer;
  Buf: array[0..255] of Char;
begin
  Val('$'+StrPas(LocaleId), Locale, Z);
  if (Locale<2048) or not PrimaryOnly then
    begin
      SetLength(Lang, 255);
      GetLocaleInfo(Locale, LCType, Buf, 255);
      SetString(Lang, Buf, StrLen(Buf));
      SList.AddObject(Lang, Pointer(Locale));
    end;
  Result:= 1;
end;

procedure TLanguagesCombo.BuildList;
var
  LT: Integer;
  Proc: TFarProc;
begin
  Items.Clear;
  case LanguageType of
  ltInstalled,
  ltPrimary:
    LT:= LCID_Installed;
  ltSupported:
    LT:= LCID_Supported;
  end;
  case ViewType of
  lvtLocalized:
    LCType:= LOCALE_SLanguage;
  lvtEnglish:
    LCType:= LOCALE_SEngLanguage;
  lvtNative:
    LCType:= LOCALE_SNativeLangName;
  lvtAbbrev:
    LCType:= LOCALE_SAbbrevLangName;
  end;
  PrimaryOnly:= LanguageType=ltPrimary;
  SList:= TStringList.Create;
  try
    EnumSystemLocales(@EnumLocalesProc, LT);
  except
  end;
  Items.Assign(SList);
  SList.Free;
end;

procedure TLanguagesCombo.Click;
begin
  inherited Click;
  Change;
end;

procedure TLanguagesCombo.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TLanguagesCombo.Reset;
var
  SaveLCID: TLanguage;
begin
  try
    SaveLCID := Language;
    BuildList;
    Language := SaveLCID;
  finally
    if Language <> SaveLCID then
      Change;
  end;
end;

procedure TLanguagesCombo.SetViewType(VT: TLanguageViewType);
begin
  if VT <> ViewType then begin
    FViewType:= VT;
    Reset;
  end;
end;

procedure TLanguagesCombo.SetLanguageType(LT: TLanguageType);
begin
  if LT <> LanguageType then begin
    FLanguageType:= LT;
    Reset;
  end;
end;

procedure TLanguagesCombo.SetShowFlag(Value: Boolean);
begin
  if FShowFlag<>Value then
    begin
      FShowFlag:= Value;
      Refresh;
    end;
end;

end.
