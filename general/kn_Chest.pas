unit kn_Chest;

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
   Winapi.Messages,
   System.SysUtils,
   System.Classes,
   System.ImageList,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.ImgList
   ;



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
uses
   gf_miscvcl,
   kn_Const,
   knt.App,
   knt.RS;

{$R *.DFM}


procedure SaveDefaultBitmaps;
var
  IMGList: TImageList;
  b : TBitmap;
  i : integer;
  BmpFN: String;
begin
  b := TBitmap.Create;
  {
  IMGList:= Form_Main.IMG_Toolbar;
  BmpFN:= 'ToolbarMain.bmp';

  IMGList:= Form_Main.IMG_Format;
  BmpFN:= 'ToolbarFormat.bmp';

  IMGList:= Form_Main.IMG_TV;
  BmpFN:= 'TV.bmp';
  }

  IMGList:= Chest.IMG_Categories;
  BmpFN:= 'catimages.bmp';


  try

    b.width := IMGList.Width * IMGList.Count;
    b.Height := IMGList.Height;
    with b.canvas do begin
      brush.color := clFuchsia;
      brush.style := bsSolid;
      fillrect( cliprect );
    end;

    if IMGList.Count > 0 then
      for i := 0 to pred( IMGList.Count ) do
         IMGList.Draw( b.canvas, i*IMGList.Width, 0, i );

    b.SaveToFile( extractfilepath( application.exename ) + BmpFN );

  finally
    b.free;
  end;

end; // SaveDefaultBitmaps

function LoadCategoryBitmapsBuiltIn : boolean;
begin
  if _LOADED_ICON_FILE = _NF_Icons_BuiltIn then exit(true);

  result := false;
  Chest.IMG_Categories.Clear;
  try
     LoadGifFromResource(Chest.IMG_Categories,  'CATIMAGES');
     result := true;
     _LOADED_ICON_FILE := _NF_Icons_BuiltIn; // means: DEFAULT icons loaded from resource

  except
    _LOADED_ICON_FILE := '';
    Messagedlg( GetRS(sChest01), mtError, [mbOK], 0 );
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
      on E : Exception do begin
        _LOADED_ICON_FILE := '';
        result := false;
        Messagedlg( GetRS(sChest02) + fn + #13#13 + E.Message, mtError, [mbOK], 0 );
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
      on E : Exception do begin
        Messagedlg( GetRS(sChest03) + fn + #13#13 + E.Message, mtError, [mbOK], 0 );
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
