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
   System.IOUtils,
   Vcl.Forms,
   Vcl.Menus,
   Vcl.Dialogs,
   gf_strings;

type
  TGroupCommand = (
     cgMenuMain, cgMenuTree, cgOther
  );

  TCommandCategory = (
     ccMenuMain, ccMenuTree, ccMacro, ccPlugin, ccTemplate, ccStyle, ccFont
  );

  TKeyMenuCategory = ccMenuMain .. ccMenuTree;

  TKeyCustomMenus = array[TKeyMenuCategory] of TMenu;

  TOtherCommandCategory = ccMacro .. ccFont;

const
  KeyboardConfigSections : array[TCommandCategory] of string = (
    'MainMenu', 'TreeContextMenu', 'Macros', 'Plugins', 'Templates', 'Styles', 'Fonts'
  );

  KeyboardConfigMenuNames : array[TCommandCategory] of string = (
    'Main Menu', 'Tree Panel Context Menu', 'Execute Macro', 'Execute Plugin', 'Insert Template', 'Apply Style', 'Apply Font'
  );


const
  _KBD_DLG_SIZEABLE : boolean = false;

type
  TMenuString = string[127];
  TKeyCommandItem = class( TObject )
    Name : string;
    Caption : string;
    Hint : string;
    Shortcut : TShortCut;
    Category : TCommandCategory;
    Path: string;

    function Group: TGroupCommand;
  end;

  TKeyOtherCommandItem = class( TObject )
   	Name : String;
    Shortcut : TShortCut;
    Category : TOtherCommandCategory;
  end;


procedure BuildKeyboardList( const KeyCustomMenus : TKeyCustomMenus; const MenuList : TList );

function CompareKeyItemsByPath( item1, item2 : pointer ) : integer;
function CompareKeyItemsByCaption( item1, item2 : pointer ) : integer;

procedure SaveKeyboardList( const FN : string; const CommandList : TList );
procedure ListKeyboardShortcuts( const FN : string; const IncludeUnassigned : boolean; const CommandList : TList );


implementation
uses
  gf_files;

function TKeyCommandItem.Group: TGroupCommand;
begin
   if Category = ccMenuMain then
      exit(cgMenuMain)
   else
   if Category = ccMenuTree then
      exit(cgMenuTree);

   exit(cgOther);
end;


function CompareKeyItemsByPath( item1, item2 : pointer ) : integer;
var
  k1, k2 : TKeyCommandItem;
begin
  k1 := TKeyCommandItem( item1 );
  k2 := TKeyCommandItem( item2 );
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
  k1, k2 : TKeyCommandItem;
begin
  k1 := TKeyCommandItem( item1 );
  k2 := TKeyCommandItem( item2 );

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
  KeyMenuItem : TKeyCommandItem;
  newPath: string;
begin

  cnt := StartItem.Count;
  if ( cnt = 0 ) then begin
      if IsValidMenuItem( StartItem ) then begin
          KeyMenuItem := TKeyCommandItem.Create;
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


procedure SaveKeyboardList( const FN : string; const CommandList : TList );
var
  IniFile : TMemIniFile;
  i, cnt : integer;
  myItem : TKeyCommandItem;
  OCCateg: TOtherCommandCategory;
begin
  { The names of the macros, templates, etc. may contain Unicode characters. If a keyboard.ini file already exists it could be in ANSI.
    If we create the TMemIniFile object indicating TEncoding.UTF8 an error will occur.
    We could delete the existing file without further ado, but for security reasons I think it's better to rename the current one
  }

  IniFile:= nil;
  if TFile.Exists(FN) then begin
     IniFile := TMemIniFile.Create( FN );
     if IniFile.Encoding <> TEncoding.UTF8 then begin
        MoveFileExW_n (FN, FN + '.bak', 3);
        FreeAndNil(IniFile);
     end;
  end;

  if IniFile = nil then
     IniFile := TMemIniFile.Create( FN, TEncoding.UTF8);
  try
    try
      cnt := CommandList.Count;

      for OCCateg := low( TOtherCommandCategory ) to high( TOtherCommandCategory ) do
          IniFile.EraseSection(KeyboardConfigSections[OCCateg]);

      with IniFile do begin
         for i := 1 to cnt do begin
            myItem := TKeyCommandItem( CommandList.Items[pred( i )] );

            if assigned( myItem ) then
               if (myItem.Group <> cgOther) or (myItem.Shortcut <> 0) then
                  WriteInteger( KeyboardConfigSections[myItem.Category], myItem.Name, myItem.Shortcut );
         end;
      end;

      IniFile.UpdateFile;
    except
    end;
  finally
    IniFile.Free;
  end;
end; // SaveKeyboardList


procedure ListKeyboardShortcuts( const FN : string; const IncludeUnassigned : boolean; const CommandList : TList );
const
  cTitle = 'KeyNote keyboard configuration listing';
var
  i, cnt : integer;
  myItem : TKeyCommandItem;
  myList : TStringList;
  category : TCommandCategory;
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

    CommandList.Sort( CompareKeyItemsByCaption );
    cnt := CommandList.Count;

    myList.Add( '<TABLE WIDTH="99%" VALIGN="top" CELLPADDING=2 CELLSPACING=2 BORDER=0>' );

    for category := low( category ) to high( category ) do
    begin

      myList.Add( Format(
        '<TR><TH COLSPAN=4>%s</TH></TR>',
        [KeyboardConfigMenuNames[category]] ));

      for i := 1 to cnt do begin
        myItem := TKeyCommandItem( CommandList.Items[pred( i )] );
        if ( assigned( myItem ) and ( myItem.Category = category )) then begin
          case myItem.Shortcut of
            0 : begin
              if IncludeUnassigned then
                 ShortcutText := 'Not assigned'
              else
                 continue;
            end;
            else
              ShortcutText := ShortcutToText( myItem.Shortcut );
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

    myList.SaveToFile( FN, TEncoding.UTF8 );
  finally
    myList.Free;
  end;

end; // ListKeyboardShortcuts

end.
