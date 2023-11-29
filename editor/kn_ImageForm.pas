unit kn_ImageForm;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)

interface

uses
   System.SysUtils,
   System.Classes,
   System.AnsiStrings,
   System.IOUtils,
   System.Math,
   Winapi.Windows,
   Winapi.ShellAPI,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.ExtCtrls,
   SynGdiPlus,
   TB97Ctls,
   TopWnd,
   kn_ImagesMng,
   kn_const,
   kn_global,
   kn_info,
   kn_FileObj
   ;


type
  TForm_Image = class(TForm)
    Button_Cancel: TButton;
    txtCaption: TEdit;
    Button_Modify: TButton;
    lblDetails: TLabel;
    bGray: TToolbarButton97;
    bWhite: TToolbarButton97;
    bBlack: TToolbarButton97;
    cImage: TImage;
    btnOpenFolder: TToolbarButton97;
    btnCreateFile: TToolbarButton97;
    cScrollBox: TScrollBox;
    btnZoomOut: TToolbarButton97;
    btnZoomIn: TToolbarButton97;
    lblZoom: TLabel;
    chkExpand: TCheckBox;
    btnZoomReset: TToolbarButton97;
    lblLinked: TLabel;
    btnPrevImage: TToolbarButton97;
    btnNextImage: TToolbarButton97;
    txtID: TEdit;
    WinOnTop: TTopMostWindow;
    btnAlwaysVisible: TToolbarButton97;
    chkCompact: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure bGrayClick(Sender: TObject);
    procedure bBlackClick(Sender: TObject);
    procedure bWhiteClick(Sender: TObject);
    procedure Button_ModifyClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure btnOpenFolderClick(Sender: TObject);
    procedure btnCreateFileClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure cScrollBoxResize(Sender: TObject);
    procedure chkExpandClick(Sender: TObject);
    procedure btnZoomResetClick(Sender: TObject);
    procedure txtIDExit(Sender: TObject);
    procedure btnPrevImageClick(Sender: TObject);
    procedure btnNextImageClick(Sender: TObject);
    procedure txtIDKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure txtIDEnter(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnAlwaysVisibleClick(Sender: TObject);
    procedure chkCompactClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fCurrentNoteFile: TNoteFile;
    fImageID: integer;
    fImagePath: string;
    fImage : TKntImage;
    fZoomFactor: Double;
    fChangingInCode: boolean;
    fImageConfigured: boolean;

    cScrollBoxTopInitial: integer;

    procedure SetImage(value: TKntImage);
    procedure ChangeImage;
    procedure ConfigureAndShowImage (KeepWindowSize: boolean);
    procedure ResizeImage;
    procedure DoExpand(Expand: boolean);
    procedure UpdatePositionAndZoom;
    procedure CheckUpdateCaption;

  public
    { Public declarations }
    property Image : TKntImage read fImage write SetImage;

  end;

{
  kn_ImageForm.LastFormImageOpened is used to ensure that the newly opened viewer window gets focus when launched
  by clicking on a hyperlink. It is used from TForm_Main.RxRTFMouseUp(), where it will be set to nil right after.
  ImgViewerInstance, NewImageViewer and ClearImgViewerInstances are used when per configuration (KeyOptions.ImgSingleViewerInstance) only
  it is allowed to have an open instance.
}


function ImgViewerInstance: TForm_Image;
procedure NewImageViewer(Instance: TForm_Image);
procedure ClearImgViewerInstances;

var
   LastFormImageOpened: TForm_Image;
   CompactMode: boolean;


implementation

uses
  gf_misc,
  gf_miscvcl,
  kn_Main,
  kn_NoteFileMng;

{$R *.DFM}

resourcestring
  STR_01 = 'Image no available. Change in caption will not saved';
  STR_Img_01 = 'Save image file as';
  STR_Img_02 = 'All image files';
  STR_02 = 'Open image file  (Ctrl -> open file location)';


var
  ImgViewerInstances: TList;
  RemovingAll: boolean;

  
function ImgViewerInstance: TForm_Image;
begin
   if ImgViewerInstances.Count > 0 then
      Result:= ImgViewerInstances[ImgViewerInstances.Count-1]
   else
      Result:= nil;
end;


procedure NewImageViewer(Instance: TForm_Image);
begin
    ImgViewerInstances.Add(Instance);
end;

procedure ClearImgViewerInstances;
var
  i: integer;
  Form: TForm_Image;
begin
   RemovingAll:= true;
   if ImgViewerInstances.Count > 0 then begin
      for i := 0 to ImgViewerInstances.Count -1 do begin
          Form:= TForm_Image(ImgViewerInstances[i]);
          try
             Form.Free;
          except
          end;
      end;
      ImgViewerInstances.Clear;
   end;
   RemovingAll:= false;

end;

procedure RemoveClosedImgViewerInstance (ImgViewerInstance: TForm_Image);
begin
   try
      ImgViewerInstances.Remove(ImgViewerInstance);
   except
   end;
end;


procedure TForm_Image.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Free;
end;

procedure TForm_Image.FormCreate(Sender: TObject);
begin
  fChangingInCode:= false;
  fImageConfigured:= false;
  cScrollBoxTopInitial:= cScrollBox.Top;
end;

procedure TForm_Image.FormDestroy(Sender: TObject);
begin
   if not RemovingAll then
      RemoveClosedImgViewerInstance(Self);
end;

procedure TForm_Image.Button_CancelClick(Sender: TObject);
begin
   if txtID.Focused then begin
      txtID.Text:= fImageID.ToString;
      Button_Cancel.SetFocus;
   end
   else
      Close;
end;

procedure TForm_Image.Button_ModifyClick(Sender: TObject);
begin
  CheckUpdateCaption;
  close;
end;


procedure TForm_Image.CheckUpdateCaption;
var
  ok: boolean;
begin
  ok:= true;
  try
    if Image.Caption <> txtCaption.Text then begin
       ok:= false;
       if (fCurrentNoteFile = NoteFile) and (Image.ID = fImageID) and ((Image.ReferenceCount > 0))  then begin
          Image.Caption:= txtCaption.Text;
          NoteFile.Modified:= true;
          UpdateNoteFileState( [fscModified] );
          ok:= true;
       end;
    end;
  except
    ok:= false;
  end;

  if not ok then begin
     btnAlwaysVisible.Down:= false;
     btnAlwaysVisibleClick(nil);
     kn_Main.DoMessageBox(STR_01, mtWarning, [mbOK], 0);
  end;
end;

procedure TForm_Image.SetImage(value: TKntImage);
begin
   if value = nil then exit;

   fImage:= value;
   fCurrentNoteFile:= NoteFile;
   fImageID:= fImage.ID;
   if Visible then begin
     ConfigureAndShowImage (true);
     WindowState:= wsNormal;
   end;
end;


procedure TForm_Image.FormShow(Sender: TObject);
begin
    Button_Modify.Default := true;
    Button_Cancel.SetFocus;

    chkCompact.Checked:= CompactMode;

    ConfigureAndShowImage (false);

    // By default, always visible
    WinOnTop.AlwaysOnTop:= true;
    btnAlwaysVisible.Down:= true;
end;

procedure TForm_Image.ConfigureAndShowImage (KeepWindowSize: boolean);
var
  Pic: TSynPicture;
  W, H, MaxW, MaxH: integer;
  Ratio: Single;
  ImageSizerThanWindow: boolean;

begin
   if Image = nil then exit;

   txtID.Text:= Image.ID.ToString;
   lblDetails.Caption:= Image.Details;
   lblDetails.Hint:= StringReplace(Image.Details, '|', ' -- ', [rfReplaceAll]);
   txtCaption.Text:= Image.Caption;
   lblLinked.Visible:= not Image.IsOwned;
   cScrollBox.Color:= KeyOptions.ImgViewerBGColor;

   fImagePath:= ImagesManager.GetImagePath(Image);
   if fImagePath <> '' then begin
      btnOpenFolder.Enabled:= true;
      btnOpenFolder.Hint:= STR_02 + '   ' + fImagePath;
   end;

   if Image.Caption <> '' then
      Caption:= Image.Name + ' - ' + Image.Caption
   else
      Caption:= Image.Name;


   W:= Image.Width;
   H:= Image.Height;
   
   if KeepWindowSize then begin
      ImageSizerThanWindow:= true;
      if (W <= cScrollBox.Width) and (H <= cScrollBox.Height) then
          ImageSizerThanWindow:= false;
   end
   else begin
      ImageSizerThanWindow:= false;
      
      Ratio:= H / W;
      MaxW:= Screen.Width  - (Self.Width - cImage.Width)   -70;
      MaxH:= Screen.Height - (Self.Height - cImage.Height) -70;

      if W > MaxW then begin
         W:= MaxW;
         H:= Round(W * Ratio);
         if H > MaxH then
            H:= MaxH;
      end
      else if H > MaxH then begin
         H:= MaxH;
         W:= Round(H / Ratio);
         if W > MaxW then
            W:= MaxW;
      end;

      Self.Width:=  W +  (Self.Width - cImage.Width) + 35;
      Self.Height:= H +  (Self.Height - cImage.Height) + 35;
   end;

   fZoomFactor:= 1.0;
   fChangingInCode:= true;
   
   Pic:= TSynPicture.Create;
   try
      Pic.LoadFromStream(Image.ImageStream);
      if KeepWindowSize then
         cImage.Picture:= nil;
        
      cImage.Picture.Assign(Pic);

      cImage.AutoSize:= false;
      cImage.Stretch:= true;

      fImageConfigured:= true;

      if (W = Image.Width) and not ImageSizerThanWindow then begin
         if not chkExpand.Checked then
            DoExpand (false);
         chkExpand.Checked:= false;
         fChangingInCode:= false;
         UpdatePositionAndZoom;

      end
      else begin
        fChangingInCode:= false;
        chkExpand.Checked:= true;
        UpdatePositionAndZoom;
      end;

   finally
      Pic.Free;
   end;
end;



procedure TForm_Image.btnZoomInClick(Sender: TObject);
begin
  fZoomFactor := SimpleRoundTo(fZoomFactor + 0.1, -1);
  ResizeImage;
end;

procedure TForm_Image.btnZoomOutClick(Sender: TObject);
begin
  if fZoomFactor > 0.1 then begin
     fZoomFactor := SimpleRoundTo(fZoomFactor - 0.1, -1);
     ResizeImage;
  end;

end;

procedure TForm_Image.btnZoomResetClick(Sender: TObject);
begin
   fZoomFactor:= 1.0;
   ResizeImage;
end;

procedure TForm_Image.cScrollBoxResize(Sender: TObject);
begin
   UpdatePositionAndZoom;
end;


procedure TForm_Image.chkCompactClick(Sender: TObject);
var
   Offset: integer;
begin
   CompactMode:= chkCompact.Checked;
   lblDetails.Visible := not CompactMode;
   lblLinked.Visible := not CompactMode;
   txtCaption.Visible := not CompactMode;

   if CompactMode then begin
      Offset:= cScrollBoxTopInitial - lblDetails.Top;
      cScrollBox.SetBounds(cScrollBox.Left, lblDetails.Top, cScrollBox.Width, cScrollBox.Height + Offset );
   end
   else begin
      Offset:= cScrollBoxTopInitial - cScrollBox.Top;
      cScrollBox.SetBounds(cScrollBox.Left, cScrollBoxTopInitial, cScrollBox.Width, cScrollBox.Height - Offset );
   end;
end;

procedure TForm_Image.chkExpandClick(Sender: TObject);
begin
   DoExpand (chkExpand.Checked);
   UpdatePositionAndZoom;
end;


procedure TForm_Image.DoExpand(Expand: boolean);
begin
   fChangingInCode:= true;

   if Expand then
      cImage.Align:= alClient

   else begin
      cImage.Align:= alNone;
      if not fChangingInCode then
         fZoomFactor := SimpleRoundTo(fZoomFactor - 0.05, -1);
      cImage.Width  := Round(Image.Width  * fZoomFactor);
      cImage.Height := Round(Image.Height * fZoomFactor);
   end;

   fChangingInCode:= false;
end;



procedure TForm_Image.ResizeImage;
begin
   if not fImageConfigured then exit;

   fChangingInCode:= true;

   if chkExpand.Checked then
      chkExpand.Checked:= False;

   cImage.Width :=  Round(Image.Width * fZoomFactor);
   cImage.Height := Round(Image.Height * fZoomFactor);

   fChangingInCode:= false;

   UpdatePositionAndZoom;
end;


procedure TForm_Image.UpdatePositionAndZoom;
var
  L,T: integer;
begin
  if (Image = nil) or (not fImageConfigured) then exit;
  if fChangingInCode then exit;

  if chkExpand.Checked then begin
     fZoomFactor:=  Min( (cScrollBox.Width-4)  / Image.Width,
                         (cScrollBox.Height-4) / Image.Height  );
     L:= (cScrollbox.Width  - Round(Image.Width * fZoomFactor)  -4) div 2;
     T:= (cScrollbox.Height - Round(Image.Height * fZoomFactor) -4) div 2;
     cImage.Margins.SetBounds(L,T,L,T);
  end
  else begin
     cImage.Left := Max(0, (cScrollbox.Width  - cImage.Width  -4) div 2);
     cImage.Top  := Max(0, (cScrollbox.Height - cImage.Height -4) div 2);
  end;

  lblZoom.Caption:= Round(fZoomFactor * 100).ToString + ' %';
end;






procedure TForm_Image.bBlackClick(Sender: TObject);
begin
  cScrollBox.Color:= clBlack;
end;

procedure TForm_Image.bGrayClick(Sender: TObject);
begin
  cScrollBox.Color:= clGray;
end;

procedure TForm_Image.bWhiteClick(Sender: TObject);
begin
   cScrollBox.Color:= clWhite;
end;


procedure TForm_Image.btnCreateFileClick(Sender: TObject);
var
  oldFilter: string;
  FN: string;
begin

  with Form_Main.SaveDlg do begin
    try
      oldFilter := Filter;
      Title:= STR_Img_01;
      Filter:= STR_Img_02 + FILTER_IMAGES;
      if ( KeyOptions.LastExportPath <> '' ) then
        InitialDir := KeyOptions.LastExportPath
      else
        InitialDir := GetFolderPath( fpPersonal );

      FileName := Image.Name;

      if ( not execute ) then exit;

      FN := normalFN( Form_Main.SaveDlg.FileName );

    finally
      Filter := oldFilter;
      KeyOptions.LastExportPath := extractfilepath( FN );
    end;
  end;

  Image.ImageStream.SaveToFile(FN);
end;


procedure TForm_Image.btnOpenFolderClick(Sender: TObject);
var
  FilePath: string;
begin
  if CtrlDown then
     FilePath:= ExtractFilePath(fImagePath)
  else
     FilePath:= fImagePath;

  ImagesManager.OpenImageFile(FilePath);
end;


procedure TForm_Image.txtIDEnter(Sender: TObject);
begin
   Button_Modify.Default:= false;
end;

procedure TForm_Image.txtIDExit(Sender: TObject);
begin
  Button_Modify.Default:= true;
  ChangeImage;
end;


procedure TForm_Image.txtIDKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if Key = VK_RETURN then begin
      Key:= 0;
      Button_Cancel.SetFocus;
   end;
end;

procedure TForm_Image.ChangeImage;
var
  Img: TKntImage;
  ID: integer;
begin
   if TryStrToInt(txtID.Text, ID) and (ID <> fImageID) then begin
      Img:= ImagesManager.GetImageFromID(ID);
      if (Img <> nil) then begin
         CheckUpdateCaption;
         Image:= Img;
      end;
   end;
   txtID.Text:= fImageID.ToString;
end;


procedure TForm_Image.btnPrevImageClick(Sender: TObject);
var
  Img: TKntImage;
begin
    Img:= ImagesManager.GetPrevImage (fImageID);
    if (Img = nil) then
       Img:= ImagesManager.GetPrevImage (ImagesManager.NextTempImageID);

    CheckUpdateCaption;
    Image:= Img;
    Button_Cancel.SetFocus;
end;

procedure TForm_Image.btnNextImageClick(Sender: TObject);
var
  Img: TKntImage;
begin
    Img:= ImagesManager.GetNextImage (fImageID);
    if (Img = nil) then
       Img:= ImagesManager.GetNextImage (0);

    CheckUpdateCaption;
    Image:= Img;
    Button_Cancel.SetFocus;
end;


procedure TForm_Image.btnAlwaysVisibleClick(Sender: TObject);
var
  i: integer;
  Form: TForm_Image;

begin
    WinOnTop.AlwaysOnTop:= btnAlwaysVisible.Down;

   { Setting AlwaysOnTop to False, will affect to all viewers open (if any); they will behave as non top
    (It is the behaviour in TopWnd although the call to SetWindowPos(FHWindow, HWND_NOTOPMOST, x, y, cx, cy, Flags)
    pass the current handle. By the oppsite, setting AlwaysOnTop to True afects individually to each viewer )
    To show it visually in the UI, I will reflect it in the btnAlwaysVisible buttom }

   if (not btnAlwaysVisible.Down) and (ImgViewerInstances.Count > 0) then begin
      for i := 0 to ImgViewerInstances.Count -1 do begin
          Form:= TForm_Image(ImgViewerInstances[i]);
          try
             Form.btnAlwaysVisible.Down:= false;
             Form.WinOnTop.AlwaysOnTop:= false;
          except
          end;
      end;
   end;

end;


initialization
  LastFormImageOpened:= nil;
  ImgViewerInstances:= TList.Create;
  CompactMode:= False;

end.
