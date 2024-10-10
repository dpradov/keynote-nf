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
   Vcl.Buttons,

   TB97Ctls,
   TopWnd,

   kn_ImagesMng,
   kn_Const,
   kn_KntFile,
   kn_KntFolder
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
    btnHelp: TBitBtn;
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
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure btnHelpClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
    fCurrentKntFile: TKntFile;
    fImageID: integer;
    fImagePath: string;
    fImage : TKntImage;
    fZoomFactor: Double;
    fChangingInCode: boolean;
    fImageConfigured: boolean;
    fFolder: TKntFolder;              // To allow to scroll through the images in the note
    fImagesInNote: TImageIDs;
    fIndexInNote: integer;

    cScrollBoxTopInitial: integer;

    procedure SetImage(value: TKntImage);
    procedure ChangeImage;
    procedure ConfigureAndShowImage (KeepWindowSize: boolean);
    procedure CheckClearUnregisteredImage;
    procedure ResizeImage;
    procedure DoExpand(Expand: boolean);
    procedure UpdatePositionAndZoom;
    procedure CheckUpdateCaption;

    procedure GetImagesInNote;
    procedure SetFolder(value: TKntFolder);

    procedure Zoom (Ratio: Single);

  public
    { Public declarations }
    property Image : TKntImage read fImage write SetImage;
    property Folder: TKntFolder read fFolder write SetFolder;

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
   LastImgViewerOpen: TForm_Image;
   CompactMode: boolean;


implementation

uses
  SynGdiPlus,
  gf_misc,
  gf_miscvcl,
  kn_Main,
  kn_NoteFileMng,
  kn_global,
  kn_info,
  knt.App
  ;

{$R *.DFM}

resourcestring
  STR_01 = 'Image no available. Change in caption will not saved';
  STR_Img_01 = 'Save image file as';
  STR_Img_02 = 'All image files';
  STR_02 = 'Open image file  (Ctrl -> open file location)';


var
  ImgViewerInstances: TList;
  RemovingAll: boolean;

const
   MOVE_INC = 50;
   RATIO_ZOOM_IN = 0.5;
   RATIO_ZOOM_OUT = -0.1;

  
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
   CheckClearUnregisteredImage;
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

function TForm_Image.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
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
       if (fCurrentKntFile = ActiveFile) and (Image.ID = fImageID) and ((Image.ReferenceCount > 0))  then begin
          Image.Caption:= txtCaption.Text;
          App.FileSetModified;
          ok:= true;
       end;
    end;
  except
    ok:= false;
  end;

  if not ok then begin
     btnAlwaysVisible.Down:= false;
     btnAlwaysVisibleClick(nil);
     App.DoMessageBox(STR_01, mtWarning, [mbOK]);
  end;
end;

procedure TForm_Image.SetImage(value: TKntImage);
begin
   if value = nil then exit;

   CheckClearUnregisteredImage;

   fImage:= value;
   fCurrentKntFile:= ActiveFile;
   fImageID:= fImage.ID;

   if Visible then begin
     ConfigureAndShowImage (true);
     WindowState:= wsNormal;
   end;
end;

procedure TForm_Image.CheckClearUnregisteredImage;
begin
   if assigned(fImage) and (fImage.ID = 0) then begin
      ImageMng.FreeImage(fImage);
      fImage.Free;
   end;
end;

procedure TForm_Image.FormShow(Sender: TObject);
begin
    Button_Modify.Default := true;
    Button_Cancel.SetFocus;
    cScrollBox.SetFocus;

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
  RegisteredImg: boolean;

begin
   if Image = nil then exit;

   txtID.Text:= Image.ID.ToString;
   lblDetails.Caption:= Image.Details;
   lblDetails.Hint:= StringReplace(Image.Details, '|', ' -- ', [rfReplaceAll]);
   txtCaption.Text:= Image.Caption;
   lblLinked.Visible:= not Image.IsOwned;
   cScrollBox.Color:= KeyOptions.ImgViewerBGColor;

   RegisteredImg:= (fImageID <> 0);
   txtCaption.Enabled:= RegisteredImg;

   fImagePath:= ImageMng.GetImagePath(Image);
   if RegisteredImg and (fImagePath <> '') then begin
      btnOpenFolder.Enabled:= true;
      btnOpenFolder.Hint:= STR_02 + '   ' + fImagePath;
   end
   else begin
      btnOpenFolder.Enabled:= false;
      btnOpenFolder.Hint:= '';
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

procedure TForm_Image.Zoom (Ratio: Single);
var
   HPos, VPos, RH, RV: integer;

begin
  HPos:= cScrollBox.HorzScrollBar.Position;
  VPos:= cScrollBox.VertScrollBar.Position;
  RH:= cScrollBox.HorzScrollBar.Range;
  RV:= cScrollBox.VertScrollBar.Range;

  if CtrlDown then
     Ratio:= Ratio * 2;

  cScrollBox.HorzScrollBar.Position:= 0;
  cScrollBox.VertScrollBar.Position:= 0;

  fZoomFactor := SimpleRoundTo(fZoomFactor + Ratio, -1);
  ResizeImage;

  if (Ratio > 0) and (not cScrollBox.HorzScrollBar.IsScrollBarVisible) and ((cImage.Width > cScrollBox.Width) or (cImage.Height > cScrollBox.Height))  then begin
    cImage.Align:= alClient;
    cImage.Align:= alNone;
  end;

  if RH > 0 then
    cScrollBox.HorzScrollBar.Position:= Round(cScrollBox.HorzScrollBar.Range/RH * HPos);

  if RV > 0 then
    cScrollBox.VertScrollBar.Position:= Round(cScrollBox.VertScrollBar.Range/RV * VPos);
end;


procedure TForm_Image.btnZoomInClick(Sender: TObject);
begin
  Zoom (RATIO_ZOOM_IN);
end;

procedure TForm_Image.btnZoomOutClick(Sender: TObject);
begin
  Zoom (RATIO_ZOOM_OUT);
end;

procedure TForm_Image.btnZoomResetClick(Sender: TObject);
begin
   fZoomFactor:= 1.0;
   cScrollBox.HorzScrollBar.Position:= 0;
   cScrollBox.VertScrollBar.Position:= 0;
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

  ImageMng.OpenImageFile(FilePath);
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
      Img:= ImageMng.GetImageFromID(ID);
      if (Img <> nil) then begin
         CheckUpdateCaption;
         Image:= Img;
      end;
   end;
   txtID.Text:= fImageID.ToString;
end;

procedure TForm_Image.SetFolder(value: TKntFolder);
begin
   fFolder:= value;
   fIndexInNote:= -1;
   fImagesInNote:= nil;
end;

procedure TForm_Image.btnPrevImageClick(Sender: TObject);
var
  Img: TKntImage;
  ImgID: integer;
begin
    if CtrlDown then begin
       Img:= ImageMng.GetPrevImage (fImageID);
       if (Img = nil) then
          Img:= ImageMng.GetPrevImage (ImageMng.NextTempImageID);
    end
    else begin
       GetImagesInNote;
       if fImagesInNote = nil then exit;
       Dec(fIndexInNote);
       if fIndexInNote < 0 then
          fIndexInNote:= Length(fImagesInNote)-1;

       fImageID:= fImagesInNote[fIndexInNote];
       Img:= ImageMng.GetImageFromID(fImageID);
    end;


    CheckUpdateCaption;
    Image:= Img;
    Button_Cancel.SetFocus;
end;

procedure TForm_Image.btnNextImageClick(Sender: TObject);
var
  Img: TKntImage;
begin
    if CtrlDown then begin
       Img:= ImageMng.GetNextImage (fImageID);
       if (Img = nil) then
          Img:= ImageMng.GetNextImage (0);
    end
    else begin
       GetImagesInNote;
       if fImagesInNote = nil then exit;
       Inc(fIndexInNote);
       if fIndexInNote >= Length(fImagesInNote) then
          fIndexInNote:= 0;

       fImageID:= fImagesInNote[fIndexInNote];
       Img:= ImageMng.GetImageFromID(fImageID);
    end;

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

// https://stackoverflow.com/questions/5064349/onkeydown-event-not-fired-when-pressing-vk-left-on-tcombobox-or-tbutton
procedure TForm_Image.CMDialogKey(var Message: TCMDialogKey);
var
   Inc, HPos, VPos, RH, RV: integer;
begin
  if GetKeyState(VK_MENU) >= 0 then begin
     if Message.CharCode in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then begin
        HPos:= cScrollBox.HorzScrollBar.Position;
        VPos:= cScrollBox.VertScrollBar.Position;
        Inc:= MOVE_INC;
        if CtrlDown then
           Inc:= Inc * 8;
        Message.Result := 1;
     end;

     case Message.CharCode of
         VK_LEFT: begin   // Left cursor
            if HPos > 0 then
               cScrollBox.HorzScrollBar.Position:= Max(HPos-Inc, 0);
         end;
         VK_RIGHT: begin
            RH:= cScrollBox.HorzScrollBar.Range;
            if HPos < RH then
               cScrollBox.HorzScrollBar.Position:= Min(HPos+Inc, RH);
         end;
         VK_UP: begin
            if VPos > 0 then
               cScrollBox.VertScrollBar.Position:= Max(VPos-Inc, 0);
         end;
         VK_DOWN: begin
            RV:= cScrollBox.VertScrollBar.Range;
            if VPos < RV then
               cScrollBox.VertScrollBar.Position:= Min(VPos+Inc, RV);
         end;
     end;
  end;

  if Message.Result = 1 then exit;

  inherited;
end;

procedure TForm_Image.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  case Key of
     VK_HOME: begin
        cScrollBox.HorzScrollBar.Position:= 0;
        if shift = [ssCtrl] then
           cScrollBox.VertScrollBar.Position:= 0;
        key:= 0;
     end;
     VK_END: begin
        cScrollBox.HorzScrollBar.Position:= cScrollBox.HorzScrollBar.Range;
        if shift = [ssCtrl] then
           cScrollBox.VertScrollBar.Position:= cScrollBox.VertScrollBar.Range;
        key:= 0;
     end;
     VK_PRIOR: begin
        cScrollBox.VertScrollBar.Position:= 0;
        key:= 0;
     end;
     VK_NEXT: begin
        cScrollBox.VertScrollBar.Position:= cScrollBox.VertScrollBar.Range;
        key:= 0;
     end;

     VK_ADD: begin
       Zoom (RATIO_ZOOM_IN);
       key:= 0;
     end;
     VK_SUBTRACT: begin
       Zoom (RATIO_ZOOM_OUT);
       key:= 0;
     end;
     VK_MULTIPLY: begin
        btnZoomResetClick(nil);
        key:= 0;
     end;
     VK_DIVIDE: begin
        chkExpand.Checked:= not chkExpand.Checked;
        key:= 0;
     end;
  end;

end;

procedure TForm_Image.FormResize(Sender: TObject);
var
  Visible: boolean;
begin
  Visible:= Self.Width >= 382;
  bGray.Visible:= Visible;
  bWhite.Visible:= Visible;
  bBlack.Visible:= Visible;
end;

procedure TForm_Image.btnHelpClick(Sender: TObject);
begin
  Messagedlg( btnHelp.Hint, mtInformation, [mbOK], 0 );
end;

procedure TForm_Image.GetImagesInNote;
var
  i: integer;
begin
   if fImagesInNote <> nil then exit;
   if fFolder = nil then exit;

   fImagesInNote:= ImageMng.GetImagesIDInstancesFromTextPlain (fFolder.Editor.TextPlain);

   for i := Low(fImagesInNote) to High(fImagesInNote) do
      if fImagesInNote[i] = fImageID then begin
         fIndexInNote:= i;
         exit;
      end;

end;


initialization
  LastImgViewerOpen:= nil;
  ImgViewerInstances:= TList.Create;
  CompactMode:= False;

end.
