unit dll_Keyboard;
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
 <marekjed@users.sourceforge.net>

************************************************************ *)

interface
uses Forms, Classes, Windows, Menus,
  Dialogs, SysUtils, gf_misc,
  gf_strings, IniFiles;

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
    Caption : TMenuString;
    Hint : TMenuString;
    Shortcut : TShortCut;
    Category : TKeyMenuCategory;
    // ParentCaption : TMenuString;
  end;

procedure BuildKeyboardList( const KeyCustomMenus : TKeyCustomMenus; const MenuList : TList );

function CompareKeyItemsByName( item1, item2 : pointer ) : integer;
function CompareKeyItemsByCaption( item1, item2 : pointer ) : integer;

procedure SaveKeyboardList( const FN : string; const MenuList : TList );

procedure ListKeyboardShortcuts( const FN : string; const IncludeUnassigned : boolean; const MenuList : TList );

implementation

function CompareKeyItemsByName( item1, item2 : pointer ) : integer;
var
  k1, k2 : TKeyMenuItem;
begin
  k1 := TKeyMenuItem( item1 );
  k2 := TKeyMenuItem( item2 );
  if ( k1.Category = k2.Category ) then
  begin
    result := CompareText( k1.Name, k2.Name );
  end
  else
  begin
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
  begin
    result := AnsiCompareText( k1.Caption, k2.Caption );
  end
  else
  begin
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
end; // IsValidMenuItem

function GetParentCaption( aItem : TMenuItem ) : TMenuString;
begin
  while assigned( aItem.Parent ) do
  begin
    aItem := aItem.Parent;
  end;
  result := aItem.Name;
end; // GetParentCaption

procedure ListMenuItems( const StartItem : TMenuItem; const MenuCategory : TKeyMenuCategory; const MenuList : TList );
var
  i, cnt : integer;
  myItem : TMenuItem;
  KeyMenuItem : TKeyMenuItem;
begin

  cnt := StartItem.Count;
  if ( cnt = 0 ) then
  begin
    if IsValidMenuItem( StartItem ) then
    begin
      KeyMenuItem := TKeyMenuItem.Create;
      KeyMenuItem.Name := StartItem.Name;
      KeyMenuItem.Caption := RemoveAccelChar( StartItem.Caption );
      KeyMenuItem.Hint := StartItem.Hint;
      KeyMenuItem.Shortcut := StartItem.ShortCut;
      KeyMenuItem.Category := MenuCategory;
      // KeyMenuItem.ParentCaption := GetParentCaption( StartItem );
      MenuList.Add( KeyMenuItem );
    end;
  end
  else
  begin
    for i := 1 to cnt do
    begin
      myItem := StartItem.Items[pred( i )];
      ListMenuItems( myItem, MenuCategory, MenuList ); // RECURSIVE CALL
    end;
  end;

end; // ListMenuItems

procedure BuildKeyboardList( const KeyCustomMenus : TKeyCustomMenus; const MenuList : TList );
var
  thisMenu : TKeyMenuCategory;
  myMenuObj : TMenu;
  i, cnt : integer;
begin

  for thisMenu := low( thisMenu ) to high( thisMenu ) do
  begin
    myMenuObj := TMainMenu( KeyCustomMenus[thisMenu] );

    cnt := myMenuObj.Items.Count;
    for i := 1 to cnt do
    begin
      ListMenuItems( myMenuObj.Items[pred( i )], thisMenu, MenuList );
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
      with IniFile do
      begin
        for i := 1 to cnt do
        begin
          myItem := TKeyMenuItem( MenuList.Items[pred( i )] );

          if assigned( myItem ) then
            writeinteger( KeyboardConfigSections[myItem.Category], myItem.Name, myItem.Shortcut );

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
