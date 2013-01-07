{$I DFS.INC}  { Standard defines for all Delphi Free Stuff components }

{------------------------------------------------------------------------------}
{ TdfsMRUFileList v2.65                                                        }
{------------------------------------------------------------------------------}
{ A Most Recently Used (MRU) File List component for Delphi.                   }
{                                                                              }
{ Copyright 1999, Brad Stowers.  All Rights Reserved.                          }
{                                                                              }
{ Copyright:                                                                   }
{ All Delphi Free Stuff (hereafter "DFS") source code is copyrighted by        }
{ Bradley D. Stowers (hereafter "author"), and shall remain the exclusive      }
{ property of the author.                                                      }
{                                                                              }
{ Distribution Rights:                                                         }
{ You are granted a non-exlusive, royalty-free right to produce and distribute }
{ compiled binary files (executables, DLLs, etc.) that are built with any of   }
{ the DFS source code unless specifically stated otherwise.                    }
{ You are further granted permission to redistribute any of the DFS source     }
{ code in source code form, provided that the original archive as found on the }
{ DFS web site (http://www.delphifreestuff.com) is distributed unmodified. For }
{ example, if you create a descendant of TDFSColorButton, you must include in  }
{ the distribution package the colorbtn.zip file in the exact form that you    }
{ downloaded it from http://www.delphifreestuff.com/mine/files/colorbtn.zip.   }
{                                                                              }
{ Restrictions:                                                                }
{ Without the express written consent of the author, you may not:              }
{   * Distribute modified versions of any DFS source code by itself. You must  }
{     include the original archive as you found it at the DFS site.            }
{   * Sell or lease any portion of DFS source code. You are, of course, free   }
{     to sell any of your own original code that works with, enhances, etc.    }
{     DFS source code.                                                         }
{   * Distribute DFS source code for profit.                                   }
{                                                                              }
{ Warranty:                                                                    }
{ There is absolutely no warranty of any kind whatsoever with any of the DFS   }
{ source code (hereafter "software"). The software is provided to you "AS-IS", }
{ and all risks and losses associated with it's use are assumed by you. In no  }
{ event shall the author of the softare, Bradley D. Stowers, be held           }
{ accountable for any damages or losses that may occur from use or misuse of   }
{ the software.                                                                }
{                                                                              }
{ Support:                                                                     }
{ Support is provided via the DFS Support Forum, which is a web-based message  }
{ system.  You can find it at http://www.delphifreestuff.com/discus/           }
{ All DFS source code is provided free of charge. As such, I can not guarantee }
{ any support whatsoever. While I do try to answer all questions that I        }
{ receive, and address all problems that are reported to me, you must          }
{ understand that I simply can not guarantee that this will always be so.      }
{                                                                              }
{ Clarifications:                                                              }
{ If you need any further information, please feel free to contact me directly.}
{ This agreement can be found online at my site in the "Miscellaneous" section.}
{------------------------------------------------------------------------------}
{ The lateset version of my components are always available on the web at:     }
{   http://www.delphifreestuff.com/                                            }
{ See MRUFList.txt for notes, known issues, and revision history.              }
{------------------------------------------------------------------------------}
{ Date last modified:  December 30, 1999                                       }
{------------------------------------------------------------------------------}


unit MRUFList;

interface

uses
  Classes, SysUtils, WideStrings,
  {$IFDEF DFS_WIN32}
  Registry, Windows,
  {$ENDIF}
  Menus, TntMenus;


const
  { This shuts up C++Builder 3 about the redefiniton being different. There
    seems to be no equivalent in C1.  Sorry. }
  {$IFDEF DFS_CPPB_3_UP}
  {$EXTERNALSYM DFS_COMPONENT_VERSION}
  {$ENDIF}
  DFS_COMPONENT_VERSION = 'TdfsMRUFileList v2.65';

type
  { Registry root values }
  TRootKey = (rkClassesRoot, rkCurrentUser, rkLocalMachine, rkUsers,
     rkCurrentConfig, rkDynData);
  { How to display the item on the menu.  mdCustom gets the display string
    from the OnGetDisplayName event. }
  TMRUDisplay = (mdFullPath, mdFileNameExt, mdFileNameOnly, mdCustom);

{$IFDEF DFS_COMPILER_3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  SClearItemCaption      = '&Clear MRU List';
  SRemoveObsoleteCaption = '&Remove Obsolete';
  { Defaults for component properties }
  DEF_SUBMENUNAME        = 'Reopen';

const
  { Defaults for component properties }
  DEF_ADDTOTOP        = TRUE;
  DEF_MAXIMUM         = 5;
  DEF_REMOVEONCLICK   = TRUE;
  DEF_USESUBMENU      = FALSE;
  DEF_MAXCAPTIONWIDTH = 200;
  {$IFDEF DFS_WIN32}
  DEF_USEREGISTRY     = TRUE;
  DEF_ROOTKEY         = rkCurrentUser;
  {$ELSE}
  DEF_USEREGISTRY     = FALSE;
  {$ENDIF}
  DEF_MRUDISPLAY      = mdFullPath;

type
  TdfsMRUFileList = class;  { Forward declaration }

  { A simple TMenuItem descendant to be used for RTTI }
  TMRUMenuItem = class(TTntMenuItem)
  private
    FFullCaption: WideString;
    FOwningList: TdfsMRUFileList;
  public
    ItemNumber: byte;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FullCaption: WideString read FFullCaption write FFullCaption;
  end;

  { Event procedure for MRU item click.  Passes filename for easy us }
  TMRUClick = procedure(Sender: TObject; AFilename: WideString) of object;
  { Event for programatically determining if an MRU item is obsolete }
  TMRURemoveObsolete = procedure(Sender: TObject; AnItem: WideString;
     var Remove: boolean) of object;
  { Event for getting the display name of an item for MRUDisplay = mdCustom }
  TMRUGetDisplayName = procedure(Sender: TObject; AFilename: WideString;
     var ADisplayName: WideString) of object;
  { Events for creation/destruction of MRU menu items }
  TMRUOnCreateDestroyMRUItem = procedure(Sender: TObject; Item: TMRUMenuItem)
     of object;

  TdfsMRUFileList = class(TComponent)
  private
    { Property variables }
    FAddToTop: boolean;
    FMaximum: byte;
    FRemoveOnClick: boolean;
    FUseSubmenu: boolean;
    FInsertSeparator : Boolean;
    FSubmenuName: string;
    FFileMenu: TTntMenuItem;
    FPopupMenu: TTntPopupMenu;
    FMenuItems: TWideStringList;
    FAutoSave: boolean;
    FAutoSaveName: string;
    FAutoSaveKey: string;
    FMaxCaptionWidth: integer;
    FClearItemName : String;
    FShowClearItem : Boolean;
    FShowRemoveObsolete : Boolean;
    FRemoveObsoleteName : String;
    FOnRemoveObsolete: TMRURemoveObsolete;
    { Event variables }
    FOnMRUItemClick: TMRUClick;
    { Internal use }
    FInhibitUpdate: boolean;
    FUseRegistry: boolean;
    {$IFDEF DFS_WIN32}
    FRegistryKey: HKEY;
    {$ENDIF}
    FMRUDisplay: TMRUDisplay;
    FOnGetDisplayName: TMRUGetDisplayName;
    FOnCreateMRUItem: TMRUOnCreateDestroyMRUItem;
    FOnDestroyMRUItem: TMRUOnCreateDestroyMRUItem;

    { Property methods }
    procedure SetMaximum(Val: byte);
    procedure SetFileMenu(Val: TTntMenuItem);
    procedure SetPopupMenu(const Val: TTntPopupMenu);
    procedure SetUseSubmenu(Val: boolean);
    procedure SetInsertSeparator(Val: boolean);
    procedure SetSubmenuName(Val: string);
    procedure SetMaxCaptionWidth(Val: integer);
    procedure SetAutoSaveName(const Val: string);
    procedure SetAutoSavekey(const Val: string);
    {$IFDEF DFS_WIN32}
    procedure SetAutoSaveRootKey(Val: TRootKey);
    function GetAutoSaveRootKey: TRootKey;
    {$ENDIF}
    function GetVersion: string;
    procedure SetVersion(const Val: string);
    { MenuItem OnClick handler }
    procedure SetMRUDisplay(Val: TMRUDisplay);
    function GetMRUDisplay: TMRUDisplay;
    procedure MRUClicked(Sender: TObject);
    procedure ClearClicked(Sender : TObject);
    procedure RemoveObsoleteClicked(Sender : TObject);
  protected
    { Method to place items on menu }
    procedure PopulateMenu; virtual;
    { We need to know if our menu item is deleted. }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { Procedures for calling event handlers }
    procedure GetDisplayName(AFilename: WideString; var ADisplayName: WideString); virtual;
    procedure RemoveObsolete(AFilename: WideString; var Remove: boolean); virtual;
    procedure MRUItemClick(AFilename: WideString); virtual;
    procedure CreateMRUItem(AnItem: TMRUMenuItem); virtual;
    procedure DestroyMRUItem(AnItem: TMRUMenuItem); virtual;
    procedure Loaded; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    { Methods to add items to the MRU list }
    procedure InsertItem(Index: integer; aFile: WideString);
    procedure ReplaceItem(OldItem, NewItem: WideString);
    procedure AddItem(aFile: WideString);
    procedure AddStringList(Files: TWideStringList);
    procedure AddStrings(Files: TWideStrings);
    { Methods to load and save items. }
    function Load: boolean;
    function Save: boolean;
    { Method to remove all MRU items from the menu, but NOT from the internal }
    { list.  You probably want ClearAllItems. }
    procedure RemoveAllItems;
    { Method to clear a single item by name from the MRU items. }
    procedure ClearItem (aFile: WideString);
    { Method to clear all current MRU items. }
    procedure ClearAllItems; virtual;
    { Method to remove all "obsolete" items. }
    procedure RemoveObsoleteItems; virtual;

    { The MRU Items.  Read Only. }
    property Items: TWideStringList
       read FMenuItems;
  published
    {$IFDEF DFS_WIN32}
    property UseRegistry: boolean
       read FUseRegistry
       write FUseRegistry
       nodefault;
    {$ENDIF}
    property Version: string
       read GetVersion
       write SetVersion
       stored FALSE;
    property AddToTop: boolean
       read FAddToTop
       write FAddToTop
       default DEF_ADDTOTOP;
    property Maximum: byte             { Maximum number of items on MRU list }
       read FMaximum
       write SetMaximum
       default DEF_MAXIMUM;
    property RemoveOnClick: boolean    { Remove MRU item when selected? }
       read FRemoveOnClick
       write FRemoveOnClick
       default DEF_REMOVEONCLICK;
    property UseSubmenu: boolean       { MRU items placed on a submenu? }
       read FUseSubmenu
       write SetUseSubmenu
       default DEF_USESUBMENU;
    property InsertSeparator : boolean
       read FInsertSeparator
       write SetInsertSeparator
       default True;
    property SubmenuName: string       { Caption of submenu item, if needed }
       read FSubmenuName
       write SetSubmenuName;
    property ClearItemName : String    { caption of the ClearMenuItem }
        read FClearItemName
        write FClearItemName;
    property ShowClearItem :boolean    
        read FShowClearItem
        write FShowClearItem
        default TRUE;
    property ShowRemoveObsolete : boolean
        read FShowRemoveObsolete
        write FShowRemoveObsolete
        default TRUE;
    property RemoveObsoleteName : string
        read FRemoveObsoleteName
        write FRemoveObsoleteName;
    property OnMRUItemClick: TMRUClick { Event for MRU item selection }
       read FOnMRUItemClick
       write FOnMRUItemClick;
    property OnRemoveObsolete: TMRURemoveObsolete 
       read FOnRemoveObsolete
       write FOnRemoveObsolete;
    property FileMenu: TTntMenuItem       { Menu to place MRU items on. }
       read FFileMenu
       write SetFileMenu;
    property PopupMenu: TTntPopupMenu
       read FPopupMenu
       write SetPopupMenu;
    property AutoSave: boolean         { Save and restore MRU items automatically. }
       read FAutoSave
       write FAutoSave
       default TRUE;
    property AutoSaveName: string      { The filename (INI) or key (registry) to save to.}
       read FAutoSaveName
       write SetAutoSaveName;
    property AutoSaveKey: string       { The section to save to. }
       read FAutoSaveKey
       write SetAutoSavekey;
    {$IFDEF DFS_WIN32}
    property AutoSaveRootKey: TRootKey { Root registry key for AutoSaveName registry path }
       read GetAutoSaveRootKey
       write SetAutoSaveRootKey
       default DEF_ROOTKEY;
    {$ENDIF}
    property MaxCaptionWidth: integer  { Maximum width of an MRU item, 0 = no maximum.}
       read FMaxCaptionWidth
       write SetMaxCaptionWidth
       default DEF_MAXCAPTIONWIDTH;
    property MRUDisplay: TMRUDisplay { How to display itmes on the menu }
       read GetMRUDisplay
       write SetMRUDisplay
       default DEF_MRUDISPLAY;
    property OnGetDisplayName: TMRUGetDisplayName
       read FOnGetDisplayName
       write FOnGetDisplayName;
    property OnCreateMRUItem: TMRUOnCreateDestroyMRUItem
       read FOnCreateMRUItem
       write FOnCreateMRUItem;
    property OnDestroyMRUItem: TMRUOnCreateDestroyMRUItem
       read FOnDestroyMRUItem
       write FOnDestroyMRUItem;
  end;

implementation

uses
  WinTypes, WinProcs, Graphics, FileCtrl, INIFiles,
  TntSysUtils, TntSystem, gf_files;

var
  MenuBmp: TBitmap;


{ Simple TMenuItem descendant mainly for RTTI, but also knows it's index     }
{ into the FMenuItems list.                                                  }
constructor TMRUMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ItemNumber := 0;
  FFullCaption := inherited Caption;
end;

destructor TMRUMenuItem.Destroy;
begin
  if FOwningList <> NIL then
    FOwningList.DestroyMRUItem(Self);

  inherited Destroy;
end;



{ Needs to do nothing more than initialize properties to defaults and create }
{ the list variable.                                                         }
constructor TdfsMRUFileList.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  {$IFDEF DFS_WIN32}
  AutoSaveRootKey := rkCurrentUser;
  {$ENDIF}
  FAddToTop := DEF_ADDTOTOP;
  FMaximum := DEF_MAXIMUM;
  FRemoveOnClick := DEF_REMOVEONCLICK;
  FUseSubmenu := DEF_USESUBMENU;
  FInsertSeparator:=True;
  SubmenuName := DEF_SUBMENUNAME;
  FMaxCaptionWidth := DEF_MAXCAPTIONWIDTH;
  FMenuItems := TWideStringList.Create;
  FMenuItems.Sorted := FALSE;
  FMRUDisplay := mdFullPath;
  FInhibitUpdate := FALSE;
  FShowClearItem := True;
  FShowRemoveObsolete := True;
  FClearItemName := SClearItemCaption;
  FRemoveObsoleteName := SRemoveObsoleteCaption;
  FAutoSave := TRUE;
  FUseRegistry := DEF_USEREGISTRY;
  if FUseRegistry then
    {$IFDEF DFS_DELPHI}
    FAutoSaveName := '\Software\My Application'
    {$ELSE}
    FAutoSaveName := '\Software\My Application\'
    {$ENDIF}
  else
    FAutoSaveName := 'MyINI.INI';
  FAutoSaveKey := 'MRU Items';
end;

destructor TdfsMRUFileList.Destroy;
begin
  if FAutoSave then
    Save;
  RemoveAllItems;
  { Cleanup the list variable }
  FMenuItems.Free;
  inherited Destroy;
end;

procedure TdfsMRUFileList.SetMaximum(Val: byte);
begin
  { Value not different or invalid, do nothing. }
  if (FMaximum = Val) then exit;
  if Val < FMaximum then begin    { If new less than old value, remove some. }
    while FMenuItems.Count > Val do { Remove extra items }
      if FAddToTop then
        FMenuItems.Delete(FMenuItems.Count-1)
      else
        FMenuItems.Delete(0);
    PopulateMenu;                 { Redo the MRU menu. }
  end;
  { Note: an ELSE clause is not needed since if new value is more than old,  }
  {       nothing needs to be done.                                          }
  FMaximum := Val;
end;

procedure TdfsMRUFileList.SetFileMenu(Val: TTntMenuItem);
begin
  RemoveAllItems;           { Remove MRU items from old menu. }
  FFileMenu := Val;
  PopulateMenu;             { Add MRU items to new menu.      }
end;

procedure TdfsMRUFileList.SetPopupMenu(const Val: TTntPopupMenu);
begin
  RemoveAllItems;           { Remove MRU items from old menu. }
  FPopupMenu := Val;
  PopulateMenu;             { Add MRU items to new menu.      }
end;

procedure TdfsMRUFileList.SetUseSubmenu(Val: boolean);
begin
  if FUseSubmenu = Val then exit; { Value not different, do nothing . }
  FUseSubmenu := Val;
  PopulateMenu;                   { Redo the menu according to new value. }
end;

procedure TdfsMRUFileList.SetInsertSeparator(Val: boolean);
begin
  If Val=FInsertSeparator then exit;
  FInsertSeparator:=Val;
  PopulateMenu;
end;

procedure TdfsMRUFileList.SetSubmenuName(Val: string);
begin
  if FSubmenuName = Val then exit; { Value not different, do nothing . }
  FSubmenuName := Val;
  if FUseSubmenu then         { Don't bother if we're not using the submenu. }
    PopulateMenu;             { Redo the menu according to new value. }
end;

procedure TdfsMRUFileList.SetMaxCaptionWidth(Val: integer);
begin
  if Val = FMaxCaptionWidth then exit; { Value not different, do nothing. }
  FMaxCaptionWidth := Val;
  PopulateMenu;
end;

{$IFDEF DFS_WIN32}
procedure TdfsMRUFileList.SetAutoSaveRootKey(Val: TRootKey);
const
  ORD_TO_VAL : array[TRootKey] of HKEY = (HKEY_CLASSES_ROOT, HKEY_CURRENT_USER,
     HKEY_LOCAL_MACHINE, HKEY_USERS, HKEY_CURRENT_CONFIG, HKEY_DYN_DATA);
begin
  FRegistryKey := ORD_TO_VAL[Val];
  if FAutoSave then
    Load;
end;

function TdfsMRUFileList.GetAutoSaveRootKey: TRootKey;
begin
  case FRegistryKey of
    HKEY_CLASSES_ROOT:   Result := rkClassesRoot;
    HKEY_LOCAL_MACHINE:  Result := rkLocalMachine;
    HKEY_USERS:          Result := rkUsers;
    HKEY_CURRENT_CONFIG: Result := rkCurrentConfig;
    HKEY_DYN_DATA:       Result := rkDynData;
  else
    Result := rkCurrentUser;
  end;
end;
{$ENDIF}

procedure TdfsMRUFileList.SetAutoSaveName(const Val: string);
begin
  if FAutoSaveName = Val then
    exit;
  FAutoSaveName := Val;
  {$IFDEF DFS_WIN32}
  // Causes wierd problems if it doesn't begin with a '\' character.
  if FUseRegistry and (FAutoSaveName <> '') then
  begin
    if FAutoSaveName[1] <> '\' then
      FAutoSaveName := '\' + FAutoSaveName;
    {$IFDEF DFS_CPPB}
    // C++Builder doesn't like it if the key doesn't end with a \ char.
    if FAutoSaveName[Length(FAutoSaveName)] <> '\' then
      FAutoSaveName := FAutoSaveName + '\';
    {$ENDIF}
  end;

  {$ENDIF}
  if FAutoSave and (not (csLoading in ComponentState)) then
    Load;
end;

procedure TdfsMRUFileList.SetAutoSaveKey(const Val: string);
begin
  if FAutoSaveKey = Val then
    exit;
  FAutoSaveKey := Val;
  if FAutoSave and (not (csLoading in ComponentState)) then
    Load;
end;

procedure TdfsMRUFileList.SetMRUDisplay(Val: TMRUDisplay);
begin
  FMRUDisplay := Val;
  if FAutoSave and (not (csLoading in ComponentState)) then
    Load;
end;

function TdfsMRUFileList.GetMRUDisplay: TMRUDisplay;
begin
  Result := FMRUDisplay;
end;

procedure TdfsMRUFileList.ClearClicked(Sender : TObject);
begin
  ClearAllItems;
end;

procedure TdfsMRUFileList.RemoveObsoleteClicked(Sender : TObject);
begin
  RemoveObsoleteItems;
end;

procedure TdfsMRUFileList.MRUClicked(Sender: TObject);
var
  ClickItem: WideString;
begin
  with Sender as TMRUMenuItem do begin
    if assigned(FOnMRUItemClick) then       { Save the clicked item's filename }
      ClickItem := FMenuItems[ItemNumber-1]
    else
      ClickItem := '';
    if FRemoveOnClick then begin        { Remove the item, if desired. }
      FMenuItems.Delete(ItemNumber-1);
      PopulateMenu;
    end;
    MRUItemClick(ClickItem);                  { Call the users event handler. }
  end;
end;

procedure TdfsMRUFileList.InsertItem(Index: integer; aFile: WideString);
var
  i: integer;
begin
  i := FMenuItems.IndexOf(aFile);        { Search list for item being added. }
  if i > -1 then                         { Find it? }
    FMenuItems.Move(i, Index)            { Yes, move it to the top. }
  else begin
    while FMenuItems.Count > (FMaximum-1) do { Remove extra items. }
      if FAddToTop then
        FMenuItems.Delete(FMenuItems.Count-1)
      else
        FMenuItems.Delete(0);
    FMenuItems.Insert(Index, aFile);     { No, add it. }
  end;
  if not FInhibitUpdate then             { Should we update the menu now? }
    PopulateMenu;                        { Yes, redo the menu. }
end;

procedure TdfsMRUFileList.ReplaceItem(OldItem, NewItem: WideString);
var
  i: integer;
begin
  i := FMenuItems.IndexOf(OldItem);      { Search list for item being added. }
  if i = -1 then                         { Find it? }
    exit                                 { No, get out. }
  else begin
    FMenuItems.Delete(i);                { Yes, remove it }
    FMenuItems.Insert(i, NewItem);       { and replace with the new one. }
  end;
  if not FInhibitUpdate then             { Should we update the menu now? }
    PopulateMenu;                        { Yes, redo the menu. }
end;

procedure TdfsMRUFileList.AddItem(aFile: WideString);
var
  i: integer;
begin
  i := FMenuItems.IndexOf(aFile);        { Search list for item being added. }
  if i > -1 then                         { Find it? }
  begin
    if FAddToTop then
      FMenuItems.Move(i, 0)              { Yes, move it to the top. }
    else
      FMenuItems.Move(i, FMenuItems.Count-1);
  end else begin
    if FAddToTop then
      FMenuItems.Insert(0, aFile)
    else
      FMenuItems.Add(aFile);             { No, add it to the bottom. }

    while FMenuItems.Count > FMaximum do { Remove extra items. }
      if FAddToTop then
        FMenuItems.Delete(FMenuItems.Count-1)
      else
        FMenuItems.Delete(0);
  end;
  if not FInhibitUpdate then             { Should we update the menu now? }
    PopulateMenu;                        { Yes, redo the menu. }
end;

procedure TdfsMRUFileList.AddStringList(Files: TWideStringList);
var
  x: integer;
begin
  FInhibitUpdate := TRUE;      { Don't let AddItem method call PopulateMenu. }
  for x := 0 to Files.Count - 1 do  { Add each item. }
    AddItem(Files[x]);
  FInhibitUpdate := FALSE;     { Clear inhibit flag. }
  PopulateMenu;                { Update menu now that all are added. }
end;

procedure TdfsMRUFileList.AddStrings(Files: TWideStrings);
var
  x: integer;
begin
  FInhibitUpdate := TRUE;      { Don't let AddItem method call PopulateMenu. }
  for x := 0 to Files.Count - 1 do  { Add each item. }
    AddItem(Files[x]);
  FInhibitUpdate := FALSE;     { Clear inhibit flag. }
  PopulateMenu;                { Update menu now that all are added. }
end;

procedure TdfsMRUFileList.PopulateMenu;
  function MakeAmpShortcut(i: integer): string;
  const
    sChars : array[0..35] of char = ('1','2','3','4','5','6','7','8','9','0',
                                     'A','B','C','D','E','F','G','H','I','J',
                                     'K','L','M','N','O','P','Q','R','S','T',
                                     'U','V','W','X','Y','Z');
  begin
    if i < 36 then
      Result := '&' + SChars[i] + ' '
    else
      Result := '';
  end;
var
  Offset,
  x, y: integer;
  NewItem: TMRUMenuItem;
  ParentMenu,
  AddMenu,
  CurMenu,
  NewMenuItem : TMenuItem;
  s, t: WideString;
begin
  { No menus assigned, nothing to do. }
  if (FFileMenu = NIL) and (FPopupMenu = NIL) then exit;
  RemoveAllItems;                        { Remove all old items. }
  if (FMenuItems.Count = 0) then exit;   { Don't have any items, we're done. }

  if FFileMenu <> NIL then
  begin
    { If FFileMenu is an item, insert before it.  If not, it's a submenu }
    { so just add to the end of it                                       }
    if FFileMenu.Count <> 0 then
    begin
      Offset := FFileMenu.Count;
      ParentMenu := FFileMenu;
    end else begin
  {$IFDEF DFS_WIN32}
      Offset := FFileMenu.MenuIndex;
  {$ELSE}
      Offset := FFileMenu.Parent.IndexOf(FFileMenu);
  {$ENDIF}
      ParentMenu := FFileMenu.Parent;
    end;

    { Create separator item. }
    if FInsertSeparator then
    begin
      NewItem := TMRUMenuItem.Create(ParentMenu);
      NewItem.Caption := '-';
      NewItem.FOwningList := Self;
      CreateMRUItem(NewItem);
      ParentMenu.Insert(Offset, NewItem);
      inc(Offset);
    end;

    { Create submenu if needed }
    if FUseSubmenu then
    begin
      AddMenu := TMRUMenuItem.Create(ParentMenu);
      AddMenu.Caption := FSubmenuName;
      TMRUMenuItem(AddMenu).FOwningList := Self;
      CreateMRUItem(TMRUMenuItem(AddMenu));
      ParentMenu.Insert(Offset, AddMenu);
      Offset := 0;
    end else
      AddMenu := ParentMenu; { Don't need submenu, just set to the file menu. }
  end else begin
    AddMenu := NIL;
    Offset := 0;
  end;

  { Create MRU items }
  for y := 0 to 1 do
  begin
    CurMenu := NIL;
    if (y = 0) then
    begin
      if assigned(AddMenu) then
        CurMenu := AddMenu
    end else begin
      Offset := 0;
      if assigned(FPopupMenu) then
        CurMenu := FPopupMenu.Items
    end;
    if CurMenu = NIL then continue;

    for x := 0 to FMenuItems.Count - 1 do
    begin
      NewItem := TMRUMenuItem.Create(CurMenu);
      NewItem.FullCaption := MakeAmpShortcut(x) + FMenuItems[x];
      NewItem.FOwningList := Self;
      case FMRUDisplay of
        mdFullPath:
          if FMaxCaptionWidth = 0 then
            NewItem.Caption := NewItem.FullCaption
          else
            NewItem.Caption := MakeAmpShortcut(x) + WideMinimizeName(FMenuItems[x],
              MenuBmp.Canvas, FMaxCaptionWidth);
        mdFileNameExt:
          { Can't minimize a filename only, so don't bother with MaxCaptionWidth }
          NewItem.Caption := WideExtractFileName(NewItem.FullCaption);
        mdFileNameOnly:
          begin
            { Can't minimize a filename only, so don't bother with MaxCaptionWidth }
            s := ExtractFileName(NewItem.FullCaption);
            t := ExtractFileExt(s);
            if (Length(t) > 0) then
              Delete(s, Length(s) - Length(t) + 1, Length(t));
            NewItem.Caption := s;
          end;
        mdCustom:
          begin
            s := FMenuItems[x];
            t := NewItem.FullCaption;
            GetDisplayName(s, t);
            NewItem.Caption := t;
          end;
      end;
      NewItem.ItemNumber := x + 1;                { Index into FMenuItems list }
      NewItem.OnClick := MRUClicked;              { Set event handler }
      CreateMRUItem(NewItem);
      CurMenu.Insert(Offset, NewItem);            { Add to the menu }
      inc(Offset);
    end;

    if (y = 0) then
    begin
      { this is the seperator near the bottom of the menu, above the Clear MRU item }
      if (FShowClearItem) or (FShowRemoveObsolete) then
      begin
        NewMenuItem := TMRUMenuItem.Create(AddMenu);
        NewMenuItem.Caption := '-';
        TMRUMenuItem(NewMenuItem).FOwningList := Self;
        CreateMRUItem(TMRUMenuItem(NewMenuItem));
        AddMenu.Insert(Offset, NewMenuItem);
        Inc(Offset);
      end;

      { this is the Clear MRU item }
      if (FShowClearItem) then
      begin
        NewMenuItem := TMRUMenuItem.Create(AddMenu);
        if FClearItemName = '' then
          NewMenuItem.Caption := SClearItemCaption
        else
          NewMenuItem.Caption := FClearItemName;
        TMRUMenuItem(NewMenuItem).FOwningList := Self;
        NewMenuItem.OnClick := ClearClicked;
        CreateMRUItem(TMRUMenuItem(NewMenuItem));
        AddMenu.Insert(Offset, NewMenuItem);
        Inc(Offset);
      end;

      { this is the Remove Obsolete item }
      if (FShowRemoveObsolete) then
      begin
        NewMenuItem := TMRUMenuItem.Create(AddMenu);
        if FRemoveObsoleteName = '' then
          NewMenuItem.Caption := SRemoveObsoleteCaption
        else
          NewMenuItem.Caption := FRemoveObsoleteName;
        TMRUMenuItem(NewMenuItem).FOwningList := Self;
        NewMenuItem.OnClick := RemoveObsoleteClicked;
        CreateMRUItem(TMRUMenuItem(NewMenuItem));
        AddMenu.Insert(Offset, NewMenuItem);
      end;
    end;
  end;
end;

procedure TdfsMRUFileList.RemoveAllItems;
var
  i, x: integer;
  DeleteItem,
  ParentMenu: TMenuItem;
begin
  { No menu, nothing to delete. }
  if (FFileMenu = NIL) and (FPopupMenu = NIL) then exit;

  for i := 0 to 1 do
  begin
    if (i = 0) and (FFileMenu <> NIL) then
    begin
      if FFileMenu.Count <> 0 then
        ParentMenu := FFileMenu
      else
        ParentMenu := FFileMenu.Parent;
    end else if (i = 1) and (FPopupMenu <> NIL) then
      ParentMenu := FPopupMenu.Items
    else
      ParentMenu := NIL;

    if ParentMenu = NIL then continue;           { No menu, nothing to delete. }

    { We don't know exactly which items are ours, so we have to check them all }
    for x := ParentMenu.Count-1 downto 0 do begin
      { Use RTTI to determine if item is of our special descenadant type. }
      if (ParentMenu[x] is TMRUMenuItem) and
         (TMRUMenuItem(ParentMenu[x]).FOwningList = Self) then
      begin
        DeleteItem := ParentMenu[x];
        ParentMenu.Delete(x);   { Yes, it is, delete it. }
        DeleteItem.Free;        { Don't forget the object, too! - RGL }
      end;
    end;
  end;
end;

procedure TdfsMRUFileList.ClearItem(aFile: WideString);
var
  i: integer;
begin
  i := FMenuItems.IndexOf(aFile);        { Search list for item being removed. }
  if i > -1 then                         { Find it? }
  begin
    FMenuItems.Delete(i);                { Yes, delete it. }
    PopulateMenu;                        { redo the menu. }
  end;
end;

function TdfsMRUFileList.Load: boolean;
  procedure StripIdents(Items: TWideStringList);
  var
    p: byte;
    x: integer;
  begin
    for x := 0 to Items.Count-1 do begin
      p := Pos('=',Items[x])+1;
      Items[x] := copy(Items[x], p, Length(Items[x])-p+1);
    end;
  end;
var
  {$IFDEF DFS_WIN32}
  RegSettings: TRegIniFile;
  {$ENDIF}
  IniSettings: TIniFile;
  FAnsiMenuItems: TStringList;
begin
  Result := FALSE;
  if csDesigning in ComponentState then
    exit;

  ClearAllItems;
  if (FAutoSaveName = '') or (FAutoSaveKey = '') then exit;

  FAnsiMenuItems:= TStringList.Create;
  try
      {$IFDEF DFS_WIN32}
      if FUseRegistry then
      begin
        RegSettings := TRegIniFile.Create(FAutoSaveName);
        try
          RegSettings.RootKey := FRegistryKey;
          RegSettings.OpenKey(FAutoSaveName, TRUE);
          RegSettings.ReadSectionValues(FAutoSaveKey, FAnsiMenuItems);
          FMenuItems.Text:= FAnsiMenuItems.Text;
        finally
          RegSettings.Free;
        end;
      end else
      {$ENDIF}
      begin
        IniSettings := TIniFile.Create(FAutoSaveName);
        try
          IniSettings.ReadSectionValues(FAutoSaveKey, FAnsiMenuItems);
          FMenuItems.Text:= UTF8ToWideString(FAnsiMenuItems.Text);
        finally
          IniSettings.Free;
        end;
      end;
      StripIdents(FMenuItems);
      PopulateMenu;
      Result := TRUE;
  finally
      FAnsiMenuItems.Free;
  end;
end;

function TdfsMRUFileList.Save: boolean;
var
  {$IFDEF DFS_WIN32}
  RegSettings: TRegIniFile;
  {$ENDIF}
  IniSettings: TIniFile;
  x: integer;
begin
  Result := FALSE;
  if (FAutoSaveName = '') or (FAutoSaveKey = '') or
    (csDesigning in ComponentState) then
    exit;
    
  {$IFDEF DFS_WIN32}
  if FUseRegistry then
  begin
    RegSettings := TRegIniFile.Create(FAutoSaveName);
    try
      RegSettings.RootKey := FRegistryKey;
      RegSettings.OpenKey(FAutoSaveName, TRUE);
      RegSettings.EraseSection(FAutoSaveKey);
      for x := 0 to Items.Count-1 do
        RegSettings.WriteString(FAutoSaveKey, 'F'+IntToStr(x), WideStringToUTF8(Items[x]));
      Result := TRUE;
    finally
      RegSettings.Free;
    end;
  end else
  {$ENDIF}
  begin
    IniSettings := TIniFile.Create(FAutoSaveName);
    try
      IniSettings.EraseSection(FAutoSaveKey);
      for x := 0 to Items.Count-1 do
        IniSettings.WriteString(FAutoSaveKey, 'F'+IntToStr(x), WideStringToUTF8(Items[x]));
      Result := TRUE;
    finally
      IniSettings.Free;
    end;
  end;
end;

procedure TdfsMRUFileList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (AComponent = FFileMenu) then
      { Our placement menu item has been deleted. }
      FFileMenu := NIL
    else if (AComponent = FPopupMenu) then
      FPopupMenu := NIL;
  end;
end;

procedure TdfsMRUFileList.ClearAllItems;
begin
  RemoveAllItems;
  FMenuItems.Clear;
end;

procedure TdfsMRUFileList.RemoveObsoleteItems;
var
  i : integer;
  Dirty: boolean;
  RemoveItem: boolean;
begin
  Dirty := FALSE;
  for i := FMenuItems.Count - 1 downto 0 do
  begin
    RemoveItem := FALSE;
    if assigned(FOnRemoveObsolete) then
      RemoveObsolete(FMenuItems[i], RemoveItem)
    else
      RemoveItem := not FileExists(FMenuItems[i]);
    if RemoveItem then
    begin
      FMenuItems.Delete(i);
      Dirty := TRUE;
    end;
  end;

  if Dirty then
    PopulateMenu;
end;

function TdfsMRUFileList.GetVersion: string;
begin
  Result := DFS_COMPONENT_VERSION;
end;

procedure TdfsMRUFileList.SetVersion(const Val: string);
begin
  { empty write method, just needed to get it to show up in Object Inspector }
end;

procedure TdfsMRUFileList.GetDisplayName(AFilename: WideString; var ADisplayName: WideString);
begin
  if assigned(FOnGetDisplayName) then
    FOnGetDisplayName(Self, AFilename, ADisplayName);
end;

procedure TdfsMRUFileList.RemoveObsolete(AFilename: WideString; var Remove: boolean);
begin
  if assigned(FOnRemoveObsolete) then
    FOnRemoveObsolete(Self, AFilename, Remove);
end;

procedure TdfsMRUFileList.MRUItemClick(AFilename: WideString);
begin
  if assigned(FOnMRUItemClick) then
    FOnMRUItemClick(Self, AFilename);
end;

procedure TdfsMRUFileList.CreateMRUItem(AnItem: TMRUMenuItem);
begin
  if assigned(FOnCreateMRUItem) then
    FOnCreateMRUItem(Self, AnItem);
end;

procedure TdfsMRUFileList.DestroyMRUItem(AnItem: TMRUMenuItem);
begin
  if assigned(FOnDestroyMRUItem) then
    FOnDestroyMRUItem(Self, AnItem);
end;

procedure TdfsMRUFileList.Loaded;
begin
  inherited Loaded;
  if FAutoSave then
    Load;
end;


{$IFNDEF DFS_WIN32}
procedure FreeMemoryBmp; far;
begin
  MenuBmp.Free;
end;
{$ENDIF}

var
{$IFDEF DFS_WIN32}
  NCM: TNonClientMetrics;
{$ELSE}
  LF: TLogFont;
{$ENDIF}

initialization
  MenuBmp:= TBitmap.Create;
  {$IFDEF DFS_WIN32}
  NCM.cbSize := SizeOf(TNonClientMetrics);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NCM, 0);
  MenuBmp.Canvas.Font.Handle := CreateFontIndirect(NCM.lfMenuFont);
  {$ELSE}
  GetObject(GetStockObject(SYSTEM_FONT), SizeOf(TLogFont), @LF);
  MenuBmp.Canvas.Font.Handle := CreateFontIndirect(LF);
  {$ENDIF}

{$IFDEF DFS_WIN32}
finalization
  MenuBmp.Free;
{$ELSE}
  AddExitProc(FreeMemoryBmp);
{$ENDIF}

end.

