unit dll_Keyboard;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 

interface
uses
   Winapi.Windows,
   System.Classes,
   System.SysUtils,
   System.IniFiles,
   Vcl.Forms,
   Vcl.Menus,
   Vcl.Dialogs,
   gf_strings;

type
  TKeyMenuCategory = (
    ckmMain, ckmTree
  );

  TKeyCustomMenus = array[TKeyMenuCategory] of TMenu;

const
  KeyboardConfigSections : array[TKeyMenuCategory] of string = (
    'MainMenu', 'TreeContextMenu'
  );

  KeyboardConfigMenuNames : array[TKeyMenuCategory] of string = (
    'Main Menu', 'Tree Panel Context Menu'
  );

const
  _KBD_DLG_SIZEABLE : boolean = false;

type
  TMenuString = string[127];
  TKeyMenuItem = class( TObject )
    Name : TMenuString;
    Caption : string;
    Hint : string;
    Shortcut : TShortCut;
    Category : TKeyMenuCategory;
    Path: string;
  end;


procedure BuildKeyboardList( const KeyCustomMenus : TKeyCustomMenus; const MenuList : TList );

function CompareKeyItemsByPath( item1, item2 : pointer ) : integer;
function CompareKeyItemsByCaption( item1, item2 : pointer ) : integer;

procedure SaveKeyboardList( const FN : string; const MenuList : TList );
procedure ListKeyboardShortcuts( const FN : string; const IncludeUnassigned : boolean; const MenuList : TList );


implementation

function CompareKeyItemsByPath( item1, item2 : pointer ) : integer;
var
  k1, k2 : TKeyMenuItem;
begin
  k1 := TKeyMenuItem( item1 );
  k2 := TKeyMenuItem( item2 );
  if ( k1.Category = k2.Category ) then
    result := CompareText( k1.Path, k2.Path )
  else begin
    if ( k1.Category > k2.Category ) then
      result := 1
    else
      result := -1;
  end;
end; // CompareKeyItemsByName


function CompareKeyItemsByCaption( item1, item2 : pointer ) : integer;
var
  k1, k2 : TKeyMenuItem;
begin
  k1 := TKeyMenuItem( item1 );
  k2 := TKeyMenuItem( item2 );

  if ( k1.Category = k2.Category ) then
    result := AnsiCompareText( k1.Caption, k2.Caption )
  else begin
    if ( k1.Category > k2.Category ) then
      result := 1
    else
      result := -1;
  end;
end; // CompareKeyItemsByCaption


function IsValidMenuItem( const aItem : TMenuItem ) : boolean;
begin
  result := ( assigned( aItem ) and
    ( aItem.Count = 0 ) and // does not have submenu
    // ( aItem.Caption <> '' ) and
    ( aItem.Caption <> '-' ) and
    ( aItem.Name <> '' ) and
    ( aItem.Name[length( aItem.Name )] <> '_' ));
end;

function GetParentCaption( aItem : TMenuItem ) : string;
begin
  while assigned( aItem.Parent ) do
    aItem := aItem.Parent;

  result := aItem.Name;
end;

procedure ListMenuItems( const Path: string; const StartItem : TMenuItem; const MenuCategory : TKeyMenuCategory; const MenuList : TList );
var
  i, cnt : integer;
  myItem : TMenuItem;
  KeyMenuItem : TKeyMenuItem;
  newPath: string;
begin

  cnt := StartItem.Count;
  if ( cnt = 0 ) then begin
      if IsValidMenuItem( StartItem ) then begin
          KeyMenuItem := TKeyMenuItem.Create;
          KeyMenuItem.Name := TMenuString(StartItem.Name);
          KeyMenuItem.Caption := RemoveAccelChar( StartItem.Caption );
          KeyMenuItem.Path := KeyMenuItem.Caption;
          if Path <> '' then
             KeyMenuItem.Path:= Path + '\' + KeyMenuItem.Path;
          KeyMenuItem.Hint := StartItem.Hint;
          KeyMenuItem.Shortcut := StartItem.ShortCut;
          KeyMenuItem.Category := MenuCategory;
          MenuList.Add( KeyMenuItem );
      end;
  end
  else begin
      newPath:= RemoveAccelChar(StartItem.Caption);
      if Path <> '' then
         newPath:= Path + '\' + newPath;

      for i := 1 to cnt do begin
         myItem := StartItem.Items[pred( i )];
         ListMenuItems( newPath, myItem, MenuCategory, MenuList ); // RECURSIVE CALL
      end;
  end;

end; // ListMenuItems

procedure BuildKeyboardList( const KeyCustomMenus : TKeyCustomMenus; const MenuList : TList );
var
  thisMenu : TKeyMenuCategory;
  myMenuObj : TMenu;
  i, cnt : integer;
begin

  for thisMenu := low( thisMenu ) to high( thisMenu ) do begin
    myMenuObj := TMainMenu( KeyCustomMenus[thisMenu] );

    cnt := myMenuObj.Items.Count;
    for i := 1 to cnt do begin
       ListMenuItems( '', myMenuObj.Items[pred( i )], thisMenu, MenuList );
    end;
  end;

end; // BuildKeyboardList


procedure SaveKeyboardList( const FN : string; const MenuList : TList );
var
  IniFile : TIniFile;
  i, cnt : integer;
  myItem : TKeyMenuItem;
begin
  IniFile := TiniFile.Create( FN );
  try
    try
      cnt := MenuList.Count;
      with IniFile do begin
         for i := 1 to cnt do begin
            myItem := TKeyMenuItem( MenuList.Items[pred( i )] );

            if assigned( myItem ) then
               WriteInteger( KeyboardConfigSections[myItem.Category], myItem.Name, myItem.Shortcut );
         end;
      end;

    except
    end;
  finally
    IniFile.Free;
  end;
end; // SaveKeyboardList


procedure ListKeyboardShortcuts( const FN : string; const IncludeUnassigned : boolean; const MenuList : TList );
const
  cTitle = 'KeyNote keyboard configuration listing';
var
  i, cnt : integer;
  myItem : TKeyMenuItem;
  myList : TStringList;
  category : TKeyMenuCategory;
  ShortcutText : string;
begin

  myList := TStringList.Create;

  myList.Add( '<HTML><HEAD>' );
  myList.Add( Format(
    '<TITLE>%s</TITLE>',
    [cTitle] ));
  myList.Add( '<LINK REL=StyleSheet HREF="keyboard.css" TYPE="text/css" MEDIA=screen>' );
  myList.Add( '</HEAD>' );
  myList.Add( '<BODY>' );
  myList.Add( Format(
    '<H2 ALIGN="left">%s</H2>',
    [cTitle] ));

  try

    MenuList.Sort( CompareKeyItemsByCaption );
    cnt := MenuList.Count;

    myList.Add( '<TABLE WIDTH="99%" VALIGN="top" CELLPADDING=2 CELLSPACING=2 BORDER=0>' );

    for category := low( category ) to high( category ) do
    begin

      myList.Add( Format(
        '<TR><TH COLSPAN=4>%s</TH></TR>',
        [KeyboardConfigMenuNames[category]] ));

      for i := 1 to cnt do
      begin
        myItem := TKeyMenuItem( MenuList.Items[pred( i )] );
        if ( assigned( myItem ) and ( myItem.Category = category )) then
        begin
          case myItem.Shortcut of
            0 : begin
              if IncludeUnassigned then
                ShortcutText := 'Not assigned'
              else
                continue;
            end;
            else
            begin
              ShortcutText := ShortcutToText( myItem.Shortcut );
            end;
          end;
          myList.Add( Format(
            '<TR><TD CLASS="keyname">%s</TD><TD CLASS="keycmd">%s</TD><TD CLASS="key">%s</TD><TD CLASS="keyhint">%s</TD></TR>',
            [myItem.Caption, myItem.Name, ShortcutText, myItem.Hint]
          ));
        end;
      end;
    end;

    myList.Add( '</TABLE>' );
    myList.Add( '<BR>' );

    myList.Add( '</BODY></HTML>' );

    myList.SaveToFile( FN );
  finally
    myList.Free;
  end;

end; // ListKeyboardShortcuts

end.
