unit kn_FavoritesMng;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 


interface
uses
   Winapi.Windows,
   Winapi.ShellAPI,
   System.SysUtils,
   Vcl.Controls,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.Forms,
   cmpGFXListBox,
   kn_LocationObj
   ;


    // favorites (global bookmarks, "keynote.fvr")
    procedure DisplayFavorites;
    procedure JumpToFavorite;
    procedure AddFavorite( const AsExternal : boolean );
    procedure FavoriteEditProperties;
    procedure DeleteFavorite;
    procedure RefreshFavorites;
    function GetFavoriteIconIndex( const myLocation : TLocation ) : integer;
    function GetSelectedFavorite : TLocation;
    function AbsolutePath(const PathRelativeToKNTSetup: string): string;


implementation
uses
   gf_misc,
   gf_strings,
   gf_files,
   kn_Global,
   kn_Info,
   kn_KntNote,
   kn_Main,
   kn_FavExtDlg,
   kn_TreeNoteMng,
   kn_LinksMng;


resourcestring
  STR_01 = 'Error loading Favorites: ';
  STR_02 = 'Rename favorite location';
  STR_03 = 'Enter new name:';
  STR_04 = 'A location named "%s" already exists. Choose another name and try again.';
  STR_05 = 'Error renaming favorite location: ';
  STR_06 = 'Favorite KeyNote location';
  STR_07 = 'Enter location name:';
  STR_08 = 'Location named "%s" already exists. Click OK to choose another name, or click Cancel to abort.';
  STR_09 = 'Delete selected location "%s" from Favorites?';
  STR_10 = 'Error deleting Favorite: ';
  STR_11 = 'Favorites list error: ';




function AbsolutePath(const PathRelativeToKNTSetup: string): string;
begin
    Result:= GetAbsolutePath(ExtractFilePath(Application.ExeName), PathRelativeToKNTSetup);
end;



procedure DisplayFavorites;
var
  i, cnt, p : integer;
begin
  cnt := Favorites_List.Count;
  if ( cnt = 0 ) then
  begin
    try
      LoadFavorites( FAV_FN );
    except
      On E : Exception do
      begin
        showmessage( STR_01 + E.Message );
        exit;
      end;
    end;
    cnt := Favorites_List.Count;
    if ( cnt = 0 ) then
      exit;
  end;

  with Form_Main do
  begin
    ListBox_ResFav.Items.BeginUpdate;
    try
      for i := 0 to pred( cnt ) do
      begin
        p := ListBox_ResFav.AddItem(
              Favorites_List[i],
              cbUnchecked, GetFavoriteIconIndex( TLocation( Favorites_List.Objects[i] )));
        TItemObject( ListBox_ResFav.Items.Objects[p] ).Data := Favorites_List.Objects[i];
      end;
      if ( ListBox_ResFav.Items.Count > 0 ) then
        ListBox_ResFav.ItemIndex := 0;
    finally
      ListBox_ResFav.Items.EndUpdate;
    end;
  end;
end; // DisplayFavorites

procedure JumpToFavorite;
var
  myFav : TLocation;
  exresult : integer;
  Args: string;
begin
  myFav := GetSelectedFavorite;
  if ( not assigned( myFav )) then exit;

  myFav := myFav.Clone;
  myFav.FileName:= AbsolutePath(myFav.FileName);

  if myFav.ExternalDoc then
  begin
    // .LNK shortcuts need special handling:
    if ( Comparetext( ExtractFileExt( myFav.FileName ), ext_Shortcut ) = 0 ) then
      exresult := ShellExecute( 0, nil, PChar( myFav.FileName ), PChar( myFav.Params ), nil, SW_NORMAL )
    else
      exresult := ShellExecute( 0, 'open', PChar( myFav.FileName ), PChar( myFav.Params ), nil, SW_NORMAL );

    if ( exresult <= 32 ) then
      messagedlg( TranslateShellExecuteError( exresult ), mtError, [mbOK], 0 );
    exit;
  end;

  if (( not Form_Main.HaveKntFolders( false, false )) or
     ( CompareText( KntFile.FileName, myFav.FileName ) <> 0 )) then
  begin
    // Location to another file

    if KeyOptions.ExtKNTLnkInNewInst or CtrlDown then begin
       OpenLocationInOtherInstance(myFav);
    end
    else begin
      // open a file in this instance (closing current knt file)
      _Global_Location := myFav;
      postmessage( Form_Main.Handle, WM_JumpToLocation, 0, 0 );
    end;

    exit;
  end
  else
  begin
    // location points to the file which is currently open
    JumpToLocation( myFav );
  end;

end; // JumpToFavorite

procedure FavoriteEditProperties;
var
  myFav : TLocation;
  newname : string;
  cnt, originalidx : integer;
  Form_FavExt : TForm_FavExt;
begin
  cnt := Form_Main.ListBox_ResFav.Items.Count;
  originalidx := Form_Main.ListBox_ResFav.ItemIndex;
  if (( originalidx < 0 ) or ( originalidx >= cnt )) then exit;

  myFav := GetSelectedFavorite;
  if ( not assigned( myFav )) then exit;
  newname := myFav.Name;

  case myFav.ExternalDoc of
    false : begin

      if InputQuery( STR_02, STR_03, newname ) then
      begin
        newname := trim( newname );
        if (( newname = '' ) or ( AnsiCompareText( newname, myFav.Name ) = 0 )) then exit;
        if ( Form_Main.ListBox_ResFav.Items.IndexOf( newname ) >= 0 ) then
        begin
          messagedlg( Format(
            STR_04,
            [newname] ), mtError, [mbOK], 0 );
          exit;
        end;
        myFav.Name := newname;
      end;
    end; // false

    true : begin
      Form_FavExt := TForm_FavExt.Create( Form_Main );
      try
        with Form_FavExt do
        begin
          Edit_FN.Text := myFav.Filename;
          Edit_Params.Text := myFav.Params;
          Edit_Name.Text := myFav.Name;
        end;

        if ( Form_FavExt.ShowModal = mrOK ) then
        begin
          myFav.Filename := NormalFN( Form_FavExt.Edit_FN.Text );
          if ( myFav.Filename = '' ) then exit;
          myFav.Params := Form_FavExt.Edit_Params.Text;
          myFav.Name := trim( Form_FavExt.Edit_Name.Text );
          if ( myFav.Name = '' ) then
            myFav.Name := ExtractFilename( myFav.Filename );
        end
        else
        begin
          exit;
        end;
      finally
        Form_FavExt.Free;
      end;
    end; // true;
  end; // case myFav.ExternalDoc

  try

      Favorites_List.Sorted := false;
      try
        Favorites_List[originalidx] := myFav.name;
      finally
        Favorites_List.Sorted := true;
      end;

      Form_Main.ListBox_ResFav.Items.Delete( originalidx );
      originalidx := Form_Main.ListBox_ResFav.AddItem(
              myFav.Name,
              cbUnchecked, GetFavoriteIconIndex( myFav ));
      TItemObject( Form_Main.ListBox_ResFav.Items.Objects[originalidx] ).Data := myFav;
      Form_Main.ListBox_ResFav.ItemIndex := originalidx;

    SaveFavorites( FAV_FN );

  except
    on E : Exception do
    begin
      messagedlg( STR_05 + E.Message, mtError, [mbOK], 0 );
    end;
  end;



end; // FavoriteEditProperties


procedure AddFavorite( const AsExternal : boolean );
var
  i : integer;
  myFav : TLocation;
  name : string;
  myNote : TKntNote;
  Form_FavExt : TForm_FavExt;
  extFilename, extParams : string;


    function GetFavName( const AName : string ) : string;
    begin
      result := AName;
      if ( not InputQuery( STR_06, STR_07, result )) then
        result := '';
    end; // GetFavName

begin

  myNote := nil; // eliminate compiler warning

  if AsExternal then
  begin

    Form_FavExt := TForm_FavExt.Create( Form_Main );
    try
      if ( Form_FavExt.ShowModal = mrOK ) then
      begin
        extFilename := NormalFN( Form_FavExt.Edit_FN.Text );
        if ( extFilename = '' ) then exit;
        extParams := Form_FavExt.Edit_Params.Text;
        name := Form_FavExt.Edit_Name.Text;
        if ( name = '' ) then
          name := ExtractFilename( extFilename );
      end
      else
      begin
        exit;
      end;
    finally
      Form_FavExt.Free;
    end;

  end
  else
  begin
    // adding a KNT location, so we must have an active note:
    if ( not Form_Main.HaveKntFolders( true, true )) then exit;
    if ( not assigned( ActiveKntFolder )) then exit;

    myNote := GetCurrentNote;
    if assigned( myNote ) then
      name := myNote.Name
    else
      name := RemoveAccelChar( ActiveKntFolder.Name );

    // confirm the name
    name := GetFavName( name );
  end;

  repeat
    // check if the name already exists and prompt to re-enter
    name := trim( name );
    if ( name = '' ) then exit;
    i := Favorites_List.IndexOf( name );
    if ( i >= 0 ) then
    begin
      case DoMessageBox( Format(
        STR_08,
        [name] ), mtError, [mbOK,mbCancel], 0 ) of
        mrOK : name := GetFavName( name );
        else
          exit;
      end;
    end
    else
      break; // name is OK, so continue
  until false;


    myFav := TLocation.Create;
    myFav.ExternalDoc := AsExternal;

    myFav.Name := name;

    if AsExternal then
    begin
      myFav.FileName := extFilename;
      myFav.Params := extParams;
    end
    else
    begin
      myFav.FileName := KntFile.FileName;
      myFav.FolderName := RemoveAccelChar( ActiveKntFolder.Name );
      myFav.FolderID := ActiveKntFolder.ID;
      if assigned( myNote ) then
      begin
        myFav.NoteName := myNote.Name;
        myFav.NoteID := myNote.ID;
      end;
      myFav.CaretPos := ActiveKntFolder.Editor.SelStart;
    end;

    Favorites_List.AddObject( name, myFav );
    i := Form_Main.ListBox_ResFav.AddItem(
            name,
            cbUnchecked, GetFavoriteIconIndex( myFav ));
    TItemObject( Form_Main.ListBox_ResFav.Items.Objects[i] ).Data := myFav;

    SaveFavorites( FAV_FN );

    try
      Form_Main.ListBox_ResFav.ItemIndex := i;
    except
    end;

end; // AddFavorite

function GetFavoriteIconIndex( const myLocation : TLocation ) : integer;
begin
  result := Form_Main.IMG_System.GetImageIndex( AbsolutePath(myLocation.Filename), true, true, [] );
end; // GetFavoriteIconIndex

function GetSelectedFavorite : TLocation;
begin
  result := nil;
  if ( not ( Form_Main.Pages_Res.Visible and Form_Main.ResTab_Favorites.TabVisible )) then exit;
  if (( Form_Main.ListBox_ResFav.Items.Count = 0 ) or
    ( Form_Main.ListBox_ResFav.ItemIndex < 0 )) then exit;
  result := TLocation( TItemObject( Form_Main.ListBox_ResFav.Items.Objects[Form_Main.ListBox_ResFav.ItemIndex] ).Data );
end; // GetSelectedFavorite

procedure DeleteFavorite;
var
  i, cnt : integer;
  myFav : TLocation;
  name : string;
begin
  cnt := Form_Main.ListBox_ResFav.Items.Count;
  i := Form_Main.ListBox_ResFav.ItemIndex;
  if (( i < 0 ) or ( i >= cnt )) then exit;

  myFav := GetSelectedFavorite;
  if ( not assigned( myFav )) then exit;
  name := Form_Main.ListBox_ResFav.Items[i];

  if ( messagedlg( Format(
    STR_09,
    [name] ), mtConfirmation, [mbOK, mbCancel], 0 ) = mrOK ) then
  begin
    try
      myFav.Free;
      Form_Main.ListBox_ResFav.Items.Delete( i );
      if ( Form_Main.ListBox_ResFav.Items.Count > 0 ) then
      begin
        if ( i > 0 ) then
          dec( i );
        Form_Main.ListBox_ResFav.ItemIndex := i;
      end;
      i := Favorites_List.IndexOf( name );
      if ( i >= 0 ) then
      begin
        Favorites_List.Delete( i );
      end;
      SaveFavorites( FAV_FN );
    except
      on E : Exception do
      begin
        messagedlg( STR_10 + E.Message, mtError, [mbOK], 0 );
      end;
    end;
  end;
end; // DeleteFavorite

procedure RefreshFavorites;
begin
  Form_Main.ListBox_ResFav.Items.BeginUpdate;
  try
    try
      Form_Main.ListBox_ResFav.Clear;
      ClearLocationList( Favorites_List );
    except
      on E : Exception do
      begin
        messagedlg( STR_11 + E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end;
  finally
    Form_Main.ListBox_ResFav.Items.EndUpdate;
  end;
  DisplayFavorites;
end; // RefreshFavorites

end.
