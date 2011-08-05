unit kn_FavoritesMng;

interface
Uses
   kn_LocationObj;

    // favorites (global bookmarks, "keynote.fvr")
    procedure DisplayFavorites;
    procedure JumpToFavorite;
    procedure AddFavorite( const AsExternal : boolean );
    procedure FavoriteEditProperties;
    procedure DeleteFavorite;
    procedure RefreshFavorites;
    function GetFavoriteIconIndex( const myLocation : TLocation ) : integer;
    function GetSelectedFavorite : TLocation;


implementation
Uses Windows, SysUtils, Controls, Dialogs, ShellAPI, StdCtrls,
     gf_misc, gf_strings, cmpGFXListBox,
     kn_Global, kn_Main, kn_Info, kn_NoteObj, kn_NodeList, kn_FavExtDlg,
     kn_TreeNoteMng, TntSysUtils, TntDialogs, kn_LinksMng;

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
begin
  myFav := GetSelectedFavorite;
  if ( not assigned( myFav )) then exit;

  if myFav.ExternalDoc then
  begin
    // .LNK shortcuts need special handling:
    if ( Comparetext( WideExtractfileext( myFav.Filename ), ext_Shortcut ) = 0 ) then
      exresult := ShellExecuteW( 0, nil, PWideChar( myFav.Filename ), PWideChar( myFav.Params ), nil, SW_NORMAL )
    else
      exresult := ShellExecuteW( 0, 'open', PWideChar( myFav.Filename ), PWideChar( myFav.Params ), nil, SW_NORMAL );

    if ( exresult <= 32 ) then
      messagedlg( TranslateShellExecuteError( exresult ), mtError, [mbOK], 0 );
    exit;
  end;

  if (( not Form_Main.HaveNotes( false, false )) or
     ( CompareText( NoteFile.FileName, myFav.FileName ) <> 0 )) then
  begin
    // open a file
    _Global_Location := myFav;
    postmessage( Form_Main.Handle, WM_JumpToLocation, 0, 0 );
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
  newname : wideString;
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

      if WideInputQuery( STR_02, STR_03, newname ) then
      begin
        newname := trim( newname );
        if (( newname = '' ) or ( WideCompareText( newname, myFav.Name ) = 0 )) then exit;
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
            myFav.Name := WideExtractFilename( myFav.Filename );
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
  name : wideString;
  myNoteNode : TNoteNode;
  Form_FavExt : TForm_FavExt;
  extFilename, extParams : wideString;


    function GetFavName( const AName : wideString ) : wideString;
    begin
      result := AName;
      if ( not WideInputQuery( STR_06, STR_07, result )) then
        result := '';
    end; // GetFavName

begin

  myNoteNode := nil; // eliminate compiler warning

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
          name := WideExtractFilename( extFilename );
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
    if ( not Form_Main.HaveNotes( true, true )) then exit;
    if ( not assigned( ActiveNote )) then exit;

    myNoteNode := GetCurrentNoteNode;
    if assigned( myNoteNode ) then
      name := myNoteNode.Name
    else
      name := RemoveAccelChar( ActiveNote.Name );

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
      case DoMessageBox( WideFormat(
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
      myFav.FileName := NoteFile.FileName;
      myFav.NoteName := RemoveAccelChar( ActiveNote.Name );
      myFav.NoteID := ActiveNote.ID;
      if assigned( myNoteNode ) then
      begin
        myFav.NodeName := myNoteNode.Name;
        myFav.NodeID := myNoteNode.ID;
      end;
      myFav.CaretPos := ActiveNote.Editor.SelStart;
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
  result := Form_Main.IMG_System.GetImageIndex( myLocation.Filename, true, true, [] );
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
