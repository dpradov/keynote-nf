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
   gf_miscvcl,
   gf_strings,
   gf_files,
   kn_Global,
   kn_Info,
   knt.model.note,
   kn_Main,
   kn_FavExtDlg,
   kn_LinksMng,
   knt.App,
   knt.RS;



function AbsolutePath(const PathRelativeToKNTSetup: string): string;
begin
    Result:= GetAbsolutePath(ExtractFilePath(Application.ExeName), PathRelativeToKNTSetup);
end;



procedure DisplayFavorites;
var
  i, cnt, p : integer;
  Loc: TLocation;
begin
  cnt := Favorites_List.Count;
  if ( cnt = 0 ) then begin
    try
      LoadFavorites( FAV_FN );
    except
      On E : Exception do begin
        App.ErrorPopup(E, GetRS(sFav01));
        exit;
      end;
    end;
    cnt := Favorites_List.Count;
    if ( cnt = 0 ) then
      exit;
  end;

  with Form_Main do begin
    ListBox_ResFav.Items.BeginUpdate;
    try
      for i := 0 to pred( cnt ) do begin
         Loc:=  Favorites_List[i];
         p := ListBox_ResFav.AddItem(Loc.Name, cbUnchecked, GetFavoriteIconIndex(Loc));
         TItemObject( ListBox_ResFav.Items.Objects[p] ).Data := Favorites_List[i];
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

  if myFav.ExternalDoc then begin
    // .LNK shortcuts need special handling:
    if ( Comparetext( ExtractFileExt( myFav.FileName ), ext_Shortcut ) = 0 ) then
      exresult := ShellExecute( 0, nil, PChar( myFav.FileName ), PChar( myFav.Params ), nil, SW_NORMAL )
    else
      exresult := ShellExecute( 0, 'open', PChar( myFav.FileName ), PChar( myFav.Params ), nil, SW_NORMAL );

    if ( exresult <= 32 ) then
       App.ErrorPopup(TranslateShellExecuteError(exresult));

    exit;
  end;

  if (( not Form_Main.HaveKntFolders( false, false )) or
     ( CompareText( ActiveFile.FileName, myFav.FileName ) <> 0 )) then begin
                                                                        // Location to another file
    if KeyOptions.ExtKNTLnkInNewInst or CtrlDown then
       OpenLocationInOtherInstance(myFav)
    else begin                                                          // open a file in this instance (closing current knt file)
       _Global_Location := myFav;
       PostMessage( Form_Main.Handle, WM_JumpToLocation, 0, 0 );
    end;

    exit;
  end
  else
    JumpToLocation( myFav );           // location points to the file which is currently open

end;


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

      if InputQuery( GetRS(sFav02), GetRS(sFav03), newname ) then begin
        newname := trim( newname );
        if (( newname = '' ) or ( AnsiCompareText( newname, myFav.Name ) = 0 )) then exit;
        if ( Form_Main.ListBox_ResFav.Items.IndexOf( newname ) >= 0 ) then begin
          App.ErrorPopup(Format(GetRS(sFav04), [newname]));
          exit;
        end;
        myFav.Name := newname;
      end;
    end; // false

    true : begin
      Form_FavExt := TForm_FavExt.Create( Form_Main );
      try
        with Form_FavExt do begin
          Edit_FN.Text := myFav.Filename;
          Edit_Params.Text := myFav.Params;
          Edit_Name.Text := myFav.Name;
        end;

        if ( Form_FavExt.ShowModal = mrOK ) then begin
          myFav.Filename := NormalFN( Form_FavExt.Edit_FN.Text );
          if ( myFav.Filename = '' ) then exit;
          myFav.Params := Form_FavExt.Edit_Params.Text;
          myFav.Name := trim( Form_FavExt.Edit_Name.Text );
          if ( myFav.Name = '' ) then
            myFav.Name := ExtractFilename( myFav.Filename );
        end
        else
          exit;

      finally
        Form_FavExt.Free;
      end;
    end; // true;
  end; // case myFav.ExternalDoc

  try


     with Form_Main do begin
       ListBox_ResFav.Items.Delete(originalidx);
       originalidx := ListBox_ResFav.AddItem(myFav.Name, cbUnchecked, GetFavoriteIconIndex(myFav));
       TItemObject(ListBox_ResFav.Items.Objects[originalidx]).Data := myFav;
       ListBox_ResFav.ItemIndex := originalidx;
     end;

     SaveFavorites( FAV_FN );

  except
    on E : Exception do
	  App.ErrorPopup(E, GetRS(sFav05));
  end;



end; // FavoriteEditProperties


function GetIndexOfFavorite (Name: string): integer;
var
  i: integer;
  F: TLocation;
begin
   for i := 0 to Favorites_List.Count-1 do begin
      F:= Favorites_List[i];
      if F.Name = Name then
         exit(i);
   end;
   Result:= -1;
end;


procedure AddFavorite( const AsExternal : boolean );
var
  i : integer;
  myFav : TLocation;
  Name : string;
  NNode : TNoteNode;
  Form_FavExt : TForm_FavExt;
  ExternalFile, ExternalParams : string;

    function GetFavName( const AName : string ) : string;
    begin
      result := AName;
      if ( not InputQuery( GetRS(sFav06), GetRS(sFav07), result )) then
        result := '';
    end;

begin

  NNode := nil; // eliminate compiler warning

  if AsExternal then begin

    Form_FavExt := TForm_FavExt.Create( Form_Main );
    try
      if ( Form_FavExt.ShowModal = mrOK ) then begin
         ExternalFile := NormalFN( Form_FavExt.Edit_FN.Text );
         if ( ExternalFile = '' ) then exit;
         ExternalParams := Form_FavExt.Edit_Params.Text;
         Name := Form_FavExt.Edit_Name.Text;
         if (Name = '') then
            Name := ExtractFilename(ExternalFile);
      end
      else
        exit;

    finally
      Form_FavExt.Free;
    end;
  end
  else begin
    // adding a KNT location, so we must have an active note:
    if ( not Form_Main.HaveKntFolders( true, true )) then exit;
    if ( not assigned( ActiveFolder )) then exit;

    NNode := ActiveNNode;
    if assigned( NNode ) then
      Name := NNode.NoteName
    else
      Name := RemoveAccelChar( ActiveFolder.Name );

    // confirm the name
    Name := GetFavName( Name );
  end;

  repeat
     // check if the name already exists and prompt to re-enter
     Name := trim(Name);
     if (Name = '') then exit;
     i := GetIndexOfFavorite(Name);
     if (i >= 0) then begin
       case App.DoMessageBox(Format(GetRS(sFav04) + GetRS(sFav08), [Name] ), mtError, [mbOK,mbCancel] ) of
          mrOK : Name := GetFavName( Name );
          else
            exit;
       end;
     end
     else
        break; // name is OK, so continue
  until false;


  myFav := TLocation.Create;
  myFav.ExternalDoc := AsExternal;

  myFav.Name := Name;

  if AsExternal then begin
    myFav.FileName := ExternalFile;
    myFav.Params := ExternalParams;
  end
  else begin
    myFav.FileName := ActiveFile.FileName;
    myFav.FolderID := ActiveFolder.ID;
    myFav.FolderName := RemoveAccelChar( ActiveFolder.Name );
    if assigned( NNode ) then begin
       myFav.NNodeID := NNode.ID;
       myFav.NoteName := NNode.NoteName;
    end;
    myFav.CaretPos := ActiveFolder.Editor.SelStart;
  end;

  Favorites_List.Add(myFav );
  i := Form_Main.ListBox_ResFav.AddItem(Name, cbUnchecked, GetFavoriteIconIndex( myFav ));
  TItemObject( Form_Main.ListBox_ResFav.Items.Objects[i] ).Data := myFav;

  SaveFavorites( FAV_FN );

  try
    Form_Main.ListBox_ResFav.ItemIndex := i;
  except
  end;

end;


function GetFavoriteIconIndex( const myLocation : TLocation ) : integer;
begin
  result := Form_Main.IMG_System.GetImageIndex( AbsolutePath(myLocation.Filename), true, true, [] );
end;


function GetSelectedFavorite : TLocation;
var
  ItemIndex: integer;

begin
  result := nil;
  with Form_Main do begin
     if not (Pages_Res.Visible and ResTab_Favorites.TabVisible) then exit;
     ItemIndex:= ListBox_ResFav.ItemIndex;
     if ((ListBox_ResFav.Items.Count = 0) or (ItemIndex < 0)) then exit;

     result := TLocation(TItemObject(ListBox_ResFav.Items.Objects[ItemIndex]).Data);
  end;
end;


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

  if (App.DoMessageBox(Format(GetRS(sFav09), [name] ), mtConfirmation, [mbOK, mbCancel], Def2 ) = mrOK) then begin
    try
      Form_Main.ListBox_ResFav.Items.Delete( i );
      if ( Form_Main.ListBox_ResFav.Items.Count > 0 ) then begin
        if ( i > 0 ) then
          dec( i );
        Form_Main.ListBox_ResFav.ItemIndex := i;
      end;
      i := GetIndexOfFavorite(name);
      if ( i >= 0 ) then begin
         Favorites_List.Delete( i );
         myFav.Free;
      end;
      SaveFavorites( FAV_FN );

    except
      on E : Exception do
	     App.ErrorPopup(E, GetRS(sFav10));
    end;
  end;
end;


procedure RefreshFavorites;
begin
  Form_Main.ListBox_ResFav.Items.BeginUpdate;
  try
    try
      Form_Main.ListBox_ResFav.Clear;
      ClearLocationList( Favorites_List );
    except
      on E : Exception do begin
   	    App.ErrorPopup(E, GetRS(sFav11));
        exit;
      end;
    end;
  finally
    Form_Main.ListBox_ResFav.Items.EndUpdate;
  end;
  DisplayFavorites;
end;

end.
