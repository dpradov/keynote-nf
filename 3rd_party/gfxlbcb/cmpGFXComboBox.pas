//TGFXComboBox Version 1.0
//Copyright© 1997 Wim Coetzee. All Rights Reserved.
//Author: Wim Coetzee
//Email : wim.c@usa.net

(* ----------------------------------- 
  + Changes by Marek Jedlinski <marek@tranglos.com> (Poland) [mj]

   >> Changes to original source code available in KeyNote NF project.
   >> Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
      in https://github.com/dpradov/keynote-nf 
  
 ****************************************************************)

unit cmpGFXComboBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, StdCtrls;


type
	TItemObject = class
	public
    f_ImageIndex: Integer;
    Data : pointer;
	end;

type
  TGFXComboBox = class(TCustomComboBox)
  private
  	f_Extended: Boolean;
    f_ImageList: TImageList;
    function CreateItemObject(p_Index: Integer): LongInt;
    procedure SetImageIndex(p_Index: Integer; const p_ImageIndex: Integer);
		function GetImageIndex(p_Index: Integer): Integer;
    procedure SetImageList(p_ImageList: TImageList);
  protected
  	procedure WndProc(var Message: TMessage); override;
		procedure DrawItem(p_Index: Integer; p_Rect: TRect; p_State: TOwnerDrawState); override;
  public
		constructor Create(p_Owner: TComponent); override;
    procedure Loaded; override;
    { Custom Methods }
    function AddItem(const p_Text: String; p_ImageIndex: Integer): Integer;
		procedure InsertItem(p_Index: Integer; const p_Text: String; p_ImageIndex: Integer);
		{ Custom Properties }
		property ImageIndex[Index: Integer]: Integer read GetImageIndex write SetImageIndex;
  published
  	{ Custom Properties }
    property Extended: Boolean read f_Extended write f_Extended;
    property ImageList: TImageList read f_ImageList write SetImageList;
		{ Republished Properties }
    property Color;
    property Ctl3D;
    property Cursor;
		property DragCursor;
    property DragMode;
    property DropDownCount;
		property Enabled;
    property Font;
    property ItemHeight;
    property Items;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property Text;
    property Visible;
		{ Republished Events }
    property OnChange;
		property OnClick;
		property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
    property OnStartDrag;
  end;

procedure Register;

implementation

const
	Offset = 0;

constructor TGFXComboBox.Create(p_Owner: TComponent);
begin
	inherited Create(p_Owner);
  {Set property default values}
  Style := csOwnerDrawFixed;
  ItemHeight := 16;
end;

procedure TGFXComboBox.Loaded;
begin
  inherited Loaded;
  {Set some properties}
  if not(csDesigning in ComponentState) then
		Perform(CB_SetExtendedUI, LongInt(Extended), 0); //Extended property
end;

procedure TGFXComboBox.WndProc(var Message: TMessage);
begin
	case Message.Msg of
  	CB_AddString, CB_InsertString:
      begin
        inherited WndProc(Message);
        {Assign a TItemObject to the new item}
        if (Message.Result <> LB_Err) or (Message.Result <> LB_ErrSpace) then
					Perform(CB_SetItemData, Message.Result, CreateItemObject(Message.Result));
      end;
		CB_SetItemData:
      begin
      	{When a TStringList is assigned to the ComboBox this message is sent twice,
        the second time the lParam(pointer to TItemData Object) is set to zero there
        fore we bypass this message because the Item Data was already set}
        if Message.lParam <> 0 then
        	inherited WndProc(Message);
      end;
  else
	 	inherited WndProc(Message);
  end;
end;

function TGFXComboBox.CreateItemObject(p_Index: Integer): LongInt;
var
	v_ItemObject: TItemObject;
begin
	Result := 0;
	if Items.Objects[p_Index] = nil then
  begin
  	{Create a TItemObject for the new Item}
	  v_ItemObject := TItemObject.Create;
		Result := LongInt(v_ItemObject);
	end else
  begin
		{Return a pointer to the item's Object}
  	Result := LongInt(Items.Objects[p_Index]);
  end;
end;

procedure TGFXComboBox.SetImageIndex(p_Index: Integer; const p_ImageIndex: Integer);
begin
  with Items.Objects[p_Index] as TItemObject do
  begin
    if f_ImageIndex <> p_ImageIndex then
    begin
      f_ImageIndex := p_ImageIndex;
      Invalidate;
    end;
  end;
end;

function TGFXComboBox.GetImageIndex(p_Index: Integer): Integer;
begin
  with Items.Objects[p_Index] as TItemObject do
		Result := f_ImageIndex;
end;

procedure TGFXComboBox.SetImageList(p_ImageList: TImageList);
var
	v_Height: Integer;
begin
	if f_ImageList <> p_ImageList then
  begin
		f_ImageList := p_ImageList;
    if p_ImageList <> nil then
    begin
      v_Height := f_ImageList.Height; // + 2*Offset;
      if v_Height > ItemHeight then
        ItemHeight := v_Height;
    end;
    Invalidate;
  end;
end;

procedure TGFXComboBox.DrawItem(p_Index: Integer; p_Rect: TRect; p_State: TOwnerDrawState);
var
  v_ImagePos: TPoint;
  v_TextRect: TRect;
  v_NewOffset: Integer;
  v_Image: TIcon;
begin
  with Canvas do
  begin
    FillRect(p_Rect);
    {Draw the Image}
    v_NewOffset := 0;
    if Assigned(ImageList) then
    begin
      v_Image := TIcon.Create;
      ImageList.GetIcon(ImageIndex[p_Index], v_Image);
      if v_Image <> nil then
      begin
        v_ImagePos := Point( 5 {3*Offset}, p_Rect.Top + ((p_Rect.Bottom - p_Rect.Top - ImageList.Height) div 2));
        DrawIconEx(Handle, v_ImagePos.X, v_ImagePos.Y, v_Image.Handle, ImageList.Width, ImageList.Height, 0, Brush.Handle, DI_Normal);
        v_NewOffset := v_ImagePos.X+5  + ImageList.Width;
      end;
      v_Image.Free;
    end
    else
    begin
      v_NewOffset := 5;
    end;
    {Draw the Text}
    SetRect(v_TextRect, v_NewOffset {+ 2*Offset}, p_Rect.Top, p_Rect.Right, p_Rect.Bottom);
    DrawText(Handle, PChar(Items[p_Index]), Length(Items[p_Index]), v_TextRect, DT_SingleLine or DT_VCenter);
  end;
  if not(csDesigning in ComponentState) and Assigned(OnDrawItem) then inherited;
end;


{** Additional Wrapper routines **}

function TGFXComboBox.AddItem(const p_Text: String; p_ImageIndex: Integer): Integer;
begin
  Result := Items.Add(p_Text);
  ImageIndex[Result] := p_ImageIndex;
end;

procedure TGFXComboBox.InsertItem(p_Index: Integer; const p_Text: String; p_ImageIndex: Integer);
begin
  Items.Insert(p_Index, p_Text);
  ImageIndex[p_Index] := p_ImageIndex;
end;

procedure Register;
begin
  RegisterComponents('Custom', [TGFXComboBox]);
end;

end.
