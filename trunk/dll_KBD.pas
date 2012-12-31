unit dll_KBD;
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

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Menus,
  gf_strings,
  dll_HotKey, ShellAPI,
  dll_Keyboard;

type
  TForm_KBD = class(TForm)
    Btn_OK: TButton;
    Btn_Cancel: TButton;
    GroupBox1: TGroupBox;
    LB_Cmd: TLabel;
    LB_Shortcut: TLabel;
    Label4: TLabel;
    List_Commands: TListBox;
    Btn_Assign: TButton;
    Btn_Remove: TButton;
    Btn_ResetAll: TButton;
    Btn_List: TButton;
    Label5: TLabel;
    GroupBox2: TGroupBox;
    LB_CurrentlyAssignedTo: TLabel;
    LB_CmdHint: TLabel;
    Edit_Current: TEdit;
    
    Menu_Items: TPopupMenu;
    MMSortbyName: TMenuItem;
    MMSortbyMenuText: TMenuItem;
    N1: TMenuItem;
    MMListUnassigned: TMenuItem;
    Btn_Help: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure List_CommandsClick(Sender: TObject);
    procedure MMSortbyNameClick(Sender: TObject);
    procedure MMSortbyMenuTextClick(Sender: TObject);
    procedure Edit_HotKeyKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Btn_RemoveClick(Sender: TObject);
    procedure Btn_AssignClick(Sender: TObject);
    procedure Btn_ListClick(Sender: TObject);
    procedure Btn_ResetAllClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Edit_HotKeyEnter(Sender: TObject);
    procedure Edit_HotKeyExit(Sender: TObject);
    procedure MMListUnassignedClick(Sender: TObject);
    procedure Btn_HelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    myKeyList : TList;
    SortByName : boolean;

    KeyNoteActivationHotkey : TShortCut;
    myKBD_FN : string;

    HaveNewKey : boolean;
    UserShortcut : TShortCut;

    IsModified : boolean;

    Edit_HotKey: TKNTHotKey;

    procedure DisplayCommands;
    procedure ShowCommandInfo;

    function GetSelectedItem : TKeyMenuItem;
    function GetItemByShortcut( SelItem : TKeyMenuItem; const aShortcut : TShortcut ) : TKeyMenuItem;
    function IsValidShortcut( const aItem : TKeyMenuItem; const aShortcut : TShortcut ) : boolean;

    procedure EnableAccelerators( const DoEnable : boolean );

  end;



implementation

{$R *.DFM}

function IsValidShortcut( const AShortcut : TShortCut ) : boolean;
begin
  result := true;
end; // IsValidShortcut

procedure TForm_KBD.FormCreate(Sender: TObject);
begin
  myKeyList := nil;
  SortByName := false;
  HaveNewKey := false;
  UserShortcut := 0;
  myKBD_FN := '';
  Edit_HotKey := TKNTHotKey.Create( self );
  Edit_HotKey.Parent := GroupBox1;
  Edit_HotKey.Left := LB_Shortcut.Left;
  Edit_HotKey.Top := Edit_Current.Top;
  Edit_HotKey.InvalidKeys := [hcNone];
  Edit_HotKey.Modifiers := [hkCtrl];
  Edit_HotKey.OnEnter := Edit_HotKeyEnter;
  Edit_HotKey.OnExit := Edit_HotKeyExit;
  Edit_HotKey.OnKeyUp := Edit_HotKeyKeyUp;
  Edit_HotKey.Visible := true;

  Edit_Hotkey.HotKey := 0;
  LB_CurrentlyAssignedTo.Font.Style := [fsBold];
end; // Create

procedure TForm_KBD.FormActivate(Sender: TObject);
begin
  OnActivate := nil;

  self.Constraints.MinHeight := self.Height;

  if assigned( myKeyList ) then
  begin
    if SortByName then
      myKeyList.Sort( CompareKeyItemsByName )
    else
      myKeyList.Sort( CompareKeyItemsByCaption );
    DisplayCommands;
    try
      List_Commands.SetFocus;
    except
    end;
  end
  else
  begin
    postmessage( self.handle, WM_CLOSE, 0, 0 );
  end;

  IsModified := false;

end; // Activate

procedure TForm_KBD.DisplayCommands;
var
  i, cnt : integer;
  item : TKeyMenuItem;
  tmpname : string;
begin

  cnt := myKeyList.Count;

  List_Commands.Items.BeginUpdate;
  try
    List_Commands.Items.Clear;

    for i := 1 to cnt do
    begin
      item := TKeyMenuItem( myKeyList[pred( i )] );

      tmpname := item.name;
      delete( tmpname, 1, 2 );

      if SortByName then
      begin
        List_Commands.Items.AddObject( Format(
          '%s - %s',
          [tmpname, item.Caption]
          ), item );
      end
      else
      begin
        List_Commands.Items.AddObject( Format(
          '%s - %s',
          [item.Caption, tmpname]
          ), item );
      end;
    end;

    if ( List_Commands.Items.Count > 0 ) then
      List_Commands.ItemIndex := 0;

    ShowCommandInfo;

  finally
    List_Commands.Items.EndUpdate;
  end;

end; // DisplayCommands

procedure TForm_KBD.ShowCommandInfo;
var
  item : TKeyMenuItem;
begin

  Edit_HotKey.Text := '';

  LB_CurrentlyAssignedTo.Caption := 'None';
  LB_CurrentlyAssignedTo.Hint := '';

  item := GetSelectedItem;
  if assigned( item ) then
  begin
    LB_CmdHint.Caption := item.Hint;
    Edit_Current.Text := ShortCutToText( item.ShortCut );
  end
  else
  begin
    LB_CmdHint.Caption := '';
    Edit_Current.Text := '';
  end;

end; // ShowCommandInfo

procedure TForm_KBD.List_CommandsClick(Sender: TObject);
begin
  ShowCommandInfo;
end;


procedure TForm_KBD.MMSortbyNameClick(Sender: TObject);
begin
  if MMSortbyName.Checked then exit;
  SortByName := true;
  MMSortbyName.Checked := true;
  myKeyList.Sort( CompareKeyItemsByName );
  DisplayCommands;
end;

procedure TForm_KBD.MMSortbyMenuTextClick(Sender: TObject);
begin
  if MMSortbyMenuText.Checked then exit;
  SortByName := false;
  MMSortbyMenuText.Checked := true;
  myKeyList.Sort( CompareKeyItemsByCaption );
  DisplayCommands;
end;

procedure TForm_KBD.Edit_HotKeyKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  item : TKeyMenuItem;
begin
    LB_CurrentlyAssignedTo.Hint := '';
    UserShortcut := Edit_HotKey.HotKey;

    if ( UserShortcut <> 0 ) then
    begin
      // Edit_HotKey.Text := ShortcutToText( UserShortcut );
      if IsValidShortcut( GetSelectedItem, UserShortcut ) then
      begin
        item := GetItemByShortcut( nil, UserShortcut );
        if assigned( item ) then
        begin
          LB_CurrentlyAssignedTo.Caption := Item.Caption;
          LB_CurrentlyAssignedTo.Hint := item.Hint;
        end
        else
        begin
          LB_CurrentlyAssignedTo.Caption := 'None';
        end;
      end
      else
      begin
        LB_CurrentlyAssignedTo.Caption := 'Invalid shortcut';
      end;
    end
    else
    begin
      Edit_HotKey.Text := '<None>';
      LB_CurrentlyAssignedTo.Caption := 'None';
    end;
end;

function TForm_KBD.GetSelectedItem: TKeyMenuItem;
var
  i : integer;
begin
  i := List_Commands.ItemIndex;
  if ( i >= 0 ) then
    result := TKeyMenuItem( List_Commands.Items.Objects[i] )
  else
    result := nil;
end; // GetSelectedItem


function TForm_KBD.GetItemByShortcut( SelItem: TKeyMenuItem;
  const aShortcut: TShortcut): TKeyMenuItem;
var
  i, cnt : integer;
  Item : TKeyMenuItem;
begin
  result := nil;
  cnt := List_Commands.Items.Count;

  if ( SelItem = nil ) then
    SelItem := GetSelectedItem;

  if ( not assigned( SelItem )) then
    exit;

  for i := 1 to cnt do
  begin
    Item := TKeyMenuItem( List_Commands.Items.Objects[pred( i )] );
    if (( Item <> SelItem ) and ( Item.Category = SelItem.Category )) then
    begin
      if ( Item.Shortcut = aShortcut ) then
      begin
        result := Item;
        break;
      end;
    end;
  end;
end; // GetItemByShortcut

function TForm_KBD.IsValidShortcut(const aItem: TKeyMenuItem;
  const aShortcut: TShortcut): boolean;
var
  Key : Word;
  Shift : TShiftState;
begin
  result := false;

  if ( aShortcut = 0 ) then exit;

  ShortcutToKey( aShortcut, Key, Shift );

  if ( key = 0 ) then exit;

  if (( ssAlt in Shift ) or ( ssCtrl in Shift )) then
  begin
    result := true;
    exit;
  end;

  case aItem.Category of
    ckmMain : begin
      if ( key in [VK_F1..VK_F12, VK_INSERT, VK_DELETE, VK_HOME, VK_END, VK_PRIOR, VK_NEXT] ) then
      begin
        result := true;
      end;
    end;
    else
    begin
      result := true;
    end;
  end;

end; // IsValidShortcut

procedure TForm_KBD.Btn_RemoveClick(Sender: TObject);
var
  item : TKeyMenuItem;
begin
  item := GetSelectedItem;
  if ( assigned( item ) and ( item.Shortcut <> 0 )) then
  begin
    IsModified := true;
    item.Shortcut := 0;
    ShowCommandInfo;
  end;
end;

procedure TForm_KBD.Btn_AssignClick(Sender: TObject);
var
  item, olditem : TKeyMenuItem;
  newShortcut : TShortcut;
begin
  item := GetSelectedItem;

  if ( not assigned( item )) then exit;

  newShortcut := Edit_Hotkey.Hotkey;
  if ( not IsValidShortcut( item, newShortcut )) then
  begin
    messagedlg( Format(
      '"%s" is not a valid keyboard shortcut for the selected command.',
      [ShortcutToText( newShortcut )] ), mtError, [mbOK], 0 );
    exit;
  end;

  oldItem := GetItemByShortcut( item, newShortcut );

  if assigned( oldItem ) then
    oldItem.Shortcut := 0;

  IsModified := true;
  item.Shortcut := newShortcut;
  ShowCommandInfo;
end;

procedure TForm_KBD.Btn_ListClick(Sender: TObject);
var
  kbdlist_fn : string;
begin
  screen.Cursor := crHourGlass;
  try
    try
      kbdlist_fn := extractfilepath( myKBD_FN ) + 'keyboard.html';
      ListKeyboardShortcuts( kbdlist_fn, MMListUnassigned.Checked, myKeyList );
      shellexecute( 0, 'open', PChar( kbdlist_fn ), nil, nil, SW_NORMAL );
    except
      on E : Exception do
      begin
        messagedlg( E.Message, mtError, [mbOK], 0 );
      end;
    end;
  finally
    screen.Cursor := crDefault;
  end;
end;

procedure TForm_KBD.Btn_ResetAllClick(Sender: TObject);
begin
  if ( not fileexists( myKBD_FN )) then
  begin
    messagedlg( Format(
      'Keyboard configuration file "%s" does not exist. KeyNote is currently using default keyboard configuration.',
      [myKBD_FN] ), mtInformation, [mbOK], 0 );
    exit;
  end;
  if ( messagedlg(
    'Existing keyboard configuration file will be deleted. Original keyboard shortcuts will be restored after you restart KeyNote. Continue?', mtWarning, [mbOK,mbCancel], 0 ) = mrOK ) then
  begin
    if deletefile( myKBD_FN ) then
    begin
      IsModified := false;
      ModalResult := mrCancel;
    end
    else
    begin
      messagedlg( Format( 'Could not delete keyboard configuration file "%s"', [myKBD_FN] ), mtError, [mbOK], 0 )
    end;
  end;

end;

procedure TForm_KBD.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ( IsModified and ( ModalResult = mrCancel )) then
    CanClose := ( messagedlg(
      'Keyboard configuration has changed. Are you sure you want to discard these changes?', mtConfirmation, [mbOK, mbCancel], 0 )  = mrOK );
end;

procedure TForm_KBD.EnableAccelerators(const DoEnable: boolean);
begin
  if DoEnable then
  begin
    LB_Cmd.Caption := '&Commands:';
    LB_Shortcut.Caption := '&New keyboard shortcut:';
    Btn_Assign.Caption := '&Assign';
    Btn_Remove.Caption := '&Remove';
    Btn_ResetAll.Caption := 'R&eset All';
    Btn_List.Caption := '&List';
  end
  else
  begin
    LB_Cmd.Caption := RemoveAccelChar( LB_Cmd.Caption );
    LB_Shortcut.Caption := RemoveAccelChar( LB_Shortcut.Caption );
    Btn_Assign.Caption := RemoveAccelChar( Btn_Assign.Caption );
    Btn_Remove.Caption := RemoveAccelChar( Btn_Remove.Caption );
    Btn_ResetAll.Caption := RemoveAccelChar( Btn_ResetAll.Caption );
    Btn_List.Caption := RemoveAccelChar( Btn_List.Caption );
  end;
end; // EnableAccelerators

procedure TForm_KBD.Edit_HotKeyEnter(Sender: TObject);
begin
  EnableAccelerators( false );
end;

procedure TForm_KBD.Edit_HotKeyExit(Sender: TObject);
begin
  EnableAccelerators( true );
end;

procedure TForm_KBD.MMListUnassignedClick(Sender: TObject);
begin
  MMListUnassigned.Checked := ( not MMListUnassigned.Checked );
end;

procedure TForm_KBD.Btn_HelpClick(Sender: TObject);
begin
  Application.HelpCommand( HELP_CONTEXT, 610 );
end;

end.
