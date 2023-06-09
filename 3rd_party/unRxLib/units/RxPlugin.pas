{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 2000 UIL                        }
{                                                       }
{ Adopted Plug-ins tool by UIL                          }
{*******************************************************}
unit RxPlugin;

{$I RX.INC}

interface

uses
  SysUtils, Forms, Graphics, Classes{$IFDEF RX_D6}, Types{$ENDIF};

type
  TPluginMessageEvent = procedure(Sender: TObject; APluginMessage: LongInt; AMessageText: string) of object;
  TPluginInitializeEvent = procedure(Sender: TObject; var AllowLoad: Boolean) of object;
  TRxPluginCommand = class;
  TRxPluginCommands = class;

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxPlugIn = class(TDataModule)
  private
    FPluginID: string;
    FAuthor: string;
    FCopyright: string;
    FDescription: string;
    FFileName: TFileName;
    FCommands: TRxPluginCommands;
    FHostApplication: TApplication;
    FManager: TComponent;
    FInstanceCount: Integer;
    FOnPluginMessage: TPluginMessageEvent;
    FOnInitialize: TPluginInitializeEvent;
    FOnConfigure: TNotifyEvent;
    FPluginVersion: string;
  protected
    procedure SetCommands(NewValue: TRxPluginCommands); virtual;
    procedure TriggerPluginMessageEvent(APluginMessage: LongInt; AMessageText: string); virtual;
    procedure TriggerInitializeEvent(var AllowLoad: Boolean); virtual;
    procedure TriggerConfigureEvent; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Configure; virtual; stdcall;
    function Initialize(Manager: TComponent; HostApplication: TApplication; FileName: string): Boolean; virtual;
      stdcall;
    procedure SendPluginMessage(APluginMessage: LongInt; AMessageText: string);
    property HostApplication: TApplication read FHostApplication;
    property Manager: TComponent read FManager;
    property FileName: TFileName read FFileName;
  published
    property Author: string read FAuthor write FAuthor;
    property Commands: TRxPluginCommands read FCommands write SetCommands;
    property Description: string read FDescription write FDescription;
    property Copyright: string read FCopyright write FCopyright;
    property InstanceCount: Integer read FInstanceCount write FInstanceCount default 1;
    property PluginID: string read FPluginID write FPluginID;
    property PluginVersion: string read FPluginVersion write FPluginVersion;
    property OnPluginMessage: TPluginMessageEvent read FOnPluginMessage write FOnPluginMessage;
    property OnInitialize: TPluginInitializeEvent read FOnInitialize write FOnInitialize;
    property OnConfigure: TNotifyEvent read FOnConfigure write FOnConfigure;
  end;

  TRxPluginCommand = class(TCollectionItem)
  private
    FName: string;
    FCaption: string;
    FHint: string;
    FData: string;
    FShortCut: TShortCut;
    FBitmap: TBitmap;
    FOnExecute: TNotifyEvent;
    procedure SetBitmap(Value: TBitmap);
  protected
    function GetDisplayName: string; override;
  published
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Caption: string read FCaption write FCaption;
    property Hint: string read FHint write FHint;
    property Data: string read FData write FData;
    property Name: string read FName write FName;
    property ShortCut: TShortCut read FShortCut write FShortCut;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;

  TRxPluginCommands = class(TCollection)
  private
    FPlugin: TRxPlugIn;
  protected
    function GetOwner: TPersistent; override;
    procedure SetItemName(AItem: TCollectionItem); override;
  public
    constructor Create(APlugIn: TRxPlugIn);
  end;

implementation

resourcestring
  RsEFmtResNotFound = 'Resource not found: %s';

{ TRxPlugin }

constructor TRxPlugIn.Create(AOwner: TComponent);
begin
  try
    //as Datamodule
    CreateNew(AOwner);
    DesignSize := Point(100, 100);

    //do commands-collection
    FCommands := TRxPluginCommands.Create(Self);

    FInstanceCount := 1;
    if (ClassType <> TRxPlugIn) and not (csDesigning in ComponentState) then
    begin
      if not InitInheritedComponent(Self, TRxPlugIn) then
        raise EResNotFound.CreateFmt(RsEFmtResNotFound, [ClassName]);

      if Assigned(OnCreate) then
        OnCreate(Self);
    end;
  finally
  end;
end;

destructor TRxPlugIn.Destroy;
begin
  Commands.Free;
  inherited Destroy;
end;

procedure TRxPlugIn.SetCommands(NewValue: TRxPluginCommands);
begin
  FCommands.Assign(NewValue);
end;

function TRxPlugIn.Initialize(Manager: TComponent; HostApplication: TApplication; FileName: string): Boolean;
begin
  Result := True;
  FHostApplication := HostApplication;
  FFileName := FileName;
  FManager := Manager;
  TriggerInitializeEvent(Result);
end;

procedure TRxPlugIn.Configure;
begin
  TriggerConfigureEvent;
end;

procedure TRxPlugIn.TriggerPluginMessageEvent(APluginMessage: LongInt; AMessageText: string);
begin
  if Assigned(FOnPluginMessage) then
    FOnPluginMessage(Self, APluginMessage, AMessageText);
end;

procedure TRxPlugIn.TriggerInitializeEvent(var AllowLoad: Boolean);
begin
  if Assigned(FOnInitialize) then
    FOnInitialize(Self, AllowLoad);
end;

procedure TRxPlugIn.TriggerConfigureEvent;
begin
  if Assigned(FOnConfigure) then
    FOnConfigure(Self);
end;

procedure TRxPlugIn.SendPluginMessage(APluginMessage: Integer; AMessageText: string);
begin
  TriggerPluginMessageEvent(APluginMessage, AMessageText);
end;

{ TRxPluginCommand }

constructor TRxPluginCommand.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FBitmap := TBitmap.Create;
  FShortCut := 0;
end;

destructor TRxPluginCommand.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

function TRxPluginCommand.GetDisplayName: string;
begin
  Result := Name;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TRxPluginCommand.SetBitmap(Value: TBitmap);
begin
  FBitmap.Assign(Value)
end;

{ TRxPluginCommands }

constructor TRxPluginCommands.Create(APlugIn: TRxPlugIn);
begin
  inherited Create(TRxPluginCommand);
  FPlugin := APlugIn;
end;

function TRxPluginCommands.GetOwner: TPersistent;
begin
  Result := FPlugin;
end;

procedure TRxPluginCommands.SetItemName(AItem: TCollectionItem);
var
  I: Integer;
  J: Integer;

  function NameUsed: Boolean;
  var
    AName: string;
  begin
    AName := Format('Command%d', [I]);
    J := AItem.Collection.Count - 1;
    while (J > -1) and not AnsiSameText(TRxPluginCommand(AItem.Collection.Items[J]).Name, AName) do
      Dec(J);
    Result := J > -1;
  end;

  procedure FindCmdIdx;
  begin
    I := 1;
    while (I < MaxInt) and NameUsed do
      Inc(I);
  end;

begin
  with TRxPluginCommand(AItem) do
    if Name = '' then
    begin
      FindCmdIdx;
      Name := Format('Command%d', [I]);
    end;
end;

end.