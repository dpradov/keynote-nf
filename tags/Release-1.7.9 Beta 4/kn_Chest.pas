
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
 <cicho@tenbit.pl>

************************************************************ *)

unit kn_Chest;

interface

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls,
  Forms, Dialogs,
  gf_misc, kn_Const, kn_Info, ImgList;

type
  TChest = class(TDataModule)
    IMG_Categories : TImageList;
    MGRImages: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Chest: TChest;
  // _CATEGORY_COUNT : integer;

const
  NODEIMG_TKN      = 0;
  NODEIMG_ENC      = 1;
  NODEIMG_DART     = 2;
  NODEIMG_TKN_RO   = 3; // readonly
  NODEIMG_ENC_RO   = 4;
  NODEIMG_DART_RO  = 5;
  NODEIMG_INVALID  = 6;
  NODEIMG_BLANK    = 7;
  NODEIMG_MACROREC = 8;
  NODEIMG_MACRORUN = 9;
  NODEIMG_TKNZIP   = 10;
  NODEIMG_TKNZIP_RO = 11;

procedure SaveDefaultBitmaps;
function LoadCategoryBitmapsBuiltIn : boolean;
function LoadCategoryBitmapsUser( const FN : string ) : boolean;
procedure SaveCategoryBitmapsUser( const FN : string );

var
  _LOADED_ICON_FILE : string;

implementation

{$R *.DFM}

resourcestring
  STR_FailedLoadBuiltin = 'Failed to load built-in category images from resource.';
  STR_FailedLoad = 'Failed to load category images from ';
  STR_FailedSave  = 'Failed to save category images to ';


procedure SaveDefaultBitmaps;
var
  b : TBitmap;
  i : integer;
begin
  b := TBitmap.Create;

  try
    b.width := Chest.IMG_Categories.Width * Chest.IMG_Categories.Count;
    b.Height := Chest.IMG_Categories.Height;
    with b.canvas do
    begin
      brush.color := clOlive;
      brush.style := bsSolid;
      fillrect( cliprect );
    end;
    if Chest.IMG_Categories.Count > 0 then
      for i := 0 to pred( Chest.IMG_Categories.Count ) do
      begin
        Chest.IMG_Categories.Draw( b.canvas, i*Chest.IMG_Categories.Width, 0, i );
      end;
    b.SaveToFile( extractfilepath( application.exename ) + 'catimages.bmp' );
  finally
    b.free;
  end;

end; // SaveDefaultBitmaps

function LoadCategoryBitmapsBuiltIn : boolean;
begin
  result := false;
  Chest.IMG_Categories.Clear;
  if ( Chest.IMG_Categories.ResInstLoad( HInstance, rtBitmap, 'CATIMAGES',  clOlive )) then
  begin
    result := true;
    _LOADED_ICON_FILE := _NF_Icons_BuiltIn; // means: DEFAULT icons loaded from resource
  end
  else
  begin
    _LOADED_ICON_FILE := '';
    Messagedlg( STR_FailedLoadBuiltin, mtError, [mbOK], 0 );
  end;
end; // LoadCategoryBitmapsBuiltIn

function LoadCategoryBitmapsUser( const FN : string ) : boolean;
var
  s : TFileStream;
begin
  result := false;
  if ( not fileexists( fn )) then exit;

  Chest.IMG_Categories.Clear;

  s := TFileStream.Create( fn, fmOpenRead );
  try
    try
      s.ReadComponent( Chest.IMG_Categories );
      _LOADED_ICON_FILE := FN;
    except
      on E : Exception do
      begin
        _LOADED_ICON_FILE := '';
        result := false;
        Messagedlg( STR_FailedLoad + fn + #13#13 + E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end
  finally
    s.Free;
  end;

  result := true;

end; // LoadCategoryBitmapsUser

procedure SaveCategoryBitmapsUser( const FN : string );
var
  s : TFileStream;
begin

  s := TFileStream.Create( fn, fmCreate );
  try
    try
      s.WriteComponent( Chest.IMG_Categories );
    except
      on E : Exception do
      begin
        Messagedlg( STR_FailedSave + fn + #13#13 + E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end;
  finally
    s.Free;
  end;

end; // SaveCategoryBitmapsUser

Initialization
  _LOADED_ICON_FILE := '';

end.
