//TGFXListBox Version 1.0
//Copyright© 1997 Wim Coetzee. All Rights Reserved.
//Author: Wim Coetzee
//Email : wim.c@usa.net

(* ----------------------------------- 
  + Changes by Marek Jedlinski <marek@tranglos.com> (Poland) [mj]
  + Changes by Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [dpv]

   >> Changes to original source code available in KeyNote NF project.
   >> Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
      in https://github.com/dpradov/keynote-nf 
  
 ****************************************************************)


unit cmpGFXListBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, StdCtrls,
  TntStdCtrls;

const
   CheckBoxSize	= 13;
   Offset 				= 2;

type
   TItemObject = class
   public
    f_State: TCheckBoxState;
    f_ImageIndex: Integer;
    f_Locked: Boolean;
    Data : pointer; // [x] MJ added this line 23.12.2000 (PhoneDeck, KeyNote)
   end;

type
  TStateChangeEvent = procedure(p_Index: Integer; var p_Cancel: Boolean) of Object;

type
  TGFXListBox = class(TTntCustomListBox)
  private
    f_AllowGrayed: Boolean;
    f_ImageList: TImageList;
    f_QuickPaint: Boolean;
    f_OnStateChange: TStateChangeEvent;
    f_CheckBoxes : boolean; // [x] MJ added this line 25.06.2001 (KeyNote)
    function CreateItemObject(p_Index: Integer): LongInt;
    procedure InvalidateItem(p_Index: Integer);
    procedure ChangeState(p_Index: Integer);
    procedure SetChecked(p_Index: Integer; const p_Checked: Boolean);
    function GetChecked(p_Index: Integer): Boolean;
    procedure SetState(p_Index: Integer; const p_State: TCheckBoxState);
    function GetState(p_Index: Integer): TCheckBoxState;
    procedure SetImageIndex(p_Index: Integer; const p_ImageIndex: Integer);
    function GetImageIndex(p_Index: Integer): Integer;
    procedure SetImageList(p_ImageList: TImageList);
    function GetLocked(p_Index: Integer): Boolean;
    procedure SetLocked(p_Index: Integer; p_Locked: Boolean);
    procedure SetCheckboxes( aCheckboxes : boolean ); // [x] MJ
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure DrawItem(p_Index: Integer; p_Rect: TRect; p_State: TOwnerDrawState); override;
    procedure KeyDown(var p_Key: Word; p_Shift: TShiftState); override;
    procedure MouseDown(p_Button: TMouseButton; p_Shift: TShiftState; p_X, p_Y: Integer); override;
    { Custom Events }
    procedure StateChange(p_Index: Integer; var p_Cancel: Boolean);
  public
    constructor Create(p_Owner: TComponent); override;
    { Custom Methods }
    function AddItem(const p_Text: wideString; p_State: TCheckBoxState; p_ImageIndex: Integer): Integer;
    procedure InsertItem(p_Index: Integer; const p_Text: wideString; p_State: TCheckBoxState; p_ImageIndex: Integer);
    procedure SetSelected(p_State: TCheckBoxState);
    procedure SetAll(p_State: TCheckBoxState);
    procedure LockSelected(p_Locked: Boolean);
    procedure LockAll(p_Locked: Boolean);
    { Custom Properties }
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
    property ImageIndex[Index: Integer]: Integer read GetImageIndex write SetImageIndex;
    property Locked[Index: Integer]: Boolean read GetLocked write SetLocked;
  published
    { Custom Properties }
    property AllowGrayed: Boolean read f_AllowGrayed write f_AllowGrayed default False;
    property ImageList: TImageList read f_ImageList write SetImageList;
    property QuickPaint: Boolean read f_QuickPaint write f_QuickPaint default True;
    property CheckBoxes : boolean read f_CheckBoxes write SetCheckboxes default true; // [x] MJ
    { Custom Methods }
    property OnStateChange: TStateChangeEvent read f_OnStateChange write f_OnStateChange;
    { Republished Properties }
    property Align;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabWidth;
    property Visible;
    { Republished Events }
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation
uses TntControls;

{$R GFXReg.dcr}

constructor TGFXListBox.Create(p_Owner: TComponent);
begin
	inherited Create(p_Owner);
  {Set property default values}
  Style := lbOwnerDrawFixed;
  ItemHeight := 17;
  QuickPaint := True;
  f_CheckBoxes := true; // [x] MJ
end;

procedure TGFXListBox.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    LB_AddString, LB_InsertString:
      begin
        inherited WndProc(Message);
        {Assign a TItemObject to the new item}
        if (Message.Result <> LB_Err) or (Message.Result <> LB_ErrSpace) then
    	   Perform(LB_SetItemData, Message.Result, CreateItemObject(Message.Result));
      end;
    LB_SetItemData:
      begin
      	{When a TStringList is assigned to the ListBox this message is sent twice,
        the second time the lParam(pointer to TItemData Object) is set to zero there
        fore we bypass this message because the Item Data was already set}
        if Message.lParam <> 0 then
           inherited WndProc(Message);
      end;
  else
    inherited WndProc(Message);
  end;
end;
function TGFXListBox.CreateItemObject(p_Index: Integer): LongInt;
var
  v_ItemObject: TItemObject;
begin
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

procedure TGFXListBox.InvalidateItem(p_Index: Integer);
var
  v_Rect: TRect;
begin
  if not QuickPaint then Invalidate else
  begin
    {
    Look ma no flashing...
    This routine will only repaint the item indicated by p_Index,
    thus eliminating the the flashing effect of the Invalidate method.
    }
    v_Rect := ItemRect(p_Index); //Get the rectangle that needs painting
    InvalidateRect(Handle, @v_Rect, True); //Add the rectangle to the Windows update region (see Win32 API help)
    UpdateWindow(Handle);
  end;
end;

procedure TGFXListBox.ChangeState(p_Index: Integer);
begin
  case State[p_Index] of
    cbUnchecked:
      if AllowGrayed then
        State[p_Index] := cbGrayed else
        State[p_Index] := cbChecked;
    cbChecked:
      State[p_Index] := cbUnchecked;
    cbGrayed:
      State[p_Index] := cbChecked;
  end;
end;

procedure TGFXListBox.SetCheckboxes( aCheckboxes : boolean ); // [x] MJ added this, 25.06.2001 (KeyNote)
begin
  if ( f_CheckBoxes = aCheckboxes ) then exit;
  f_CheckBoxes := aCheckboxes;
  Invalidate;
end;

procedure TGFXListBox.SetChecked(p_Index: Integer; const p_Checked: Boolean);
begin
  if p_Checked then
    State[p_Index] := cbChecked else
    State[p_Index] := cbUnchecked;
end;

function TGFXListBox.GetChecked(p_Index: Integer): Boolean;
begin
  if State[p_Index] = cbChecked then
    Result := True else
    Result := False;
end;

procedure TGFXListBox.SetState(p_Index: Integer; const p_State: TCheckBoxState);
var
   v_Cancel: Boolean;
begin
  with Items.Objects[p_Index] as TItemObject do
  begin
    if not f_Locked then
    begin
      if f_State <> p_State then
      begin
        v_Cancel := False;
        StateChange(p_Index, v_Cancel);
        if not v_Cancel then
        begin
          f_State := p_State;
          InvalidateItem(p_Index);
        end;
      end;
    end;
  end;
end;

function TGFXListBox.GetState(p_Index: Integer): TCheckBoxState;
begin
  with Items.Objects[p_Index] as TItemObject do
     Result := f_State;
end;

procedure TGFXListBox.SetImageIndex(p_Index: Integer; const p_ImageIndex: Integer);
begin
  with Items.Objects[p_Index] as TItemObject do
  begin
    if f_ImageIndex <> p_ImageIndex then
    begin
      f_ImageIndex := p_ImageIndex;
      InvalidateItem(p_Index);
    end;
  end;
end;

function TGFXListBox.GetImageIndex(p_Index: Integer): Integer;
begin
    with Items.Objects[p_Index] as TItemObject do
            Result := f_ImageIndex;
end;

procedure TGFXListBox.SetImageList(p_ImageList: TImageList);
var
  v_Height: Integer;
begin
  if f_ImageList <> p_ImageList then
  begin
    f_ImageList := p_ImageList;
    if p_ImageList <> nil then
    begin
      v_Height := f_ImageList.Height + 2*Offset;
      if v_Height > ItemHeight then
        ItemHeight := v_Height;
    end;
    Invalidate;
  end;
end;

function TGFXListBox.GetLocked(p_Index: Integer): Boolean;
begin
    with Items.Objects[p_Index] as TItemObject do
            Result := f_Locked;
end;

procedure TGFXListBox.SetLocked(p_Index: Integer; p_Locked: Boolean);
begin
  with Items.Objects[p_Index] as TItemObject do
  begin
    if f_Locked <> p_Locked then
    	f_Locked := p_Locked;
  end;
end;

procedure TGFXListBox.DrawItem(p_Index: Integer; p_Rect: TRect; p_State: TOwnerDrawState);
const
  csUnchecked = DFCS_ButtonCheck;
  csChecked 	= DFCS_ButtonCheck or DFCS_Checked;
  csGrayed 		= DFCS_ButtonCheck or DFCS_Checked or DFCS_Inactive;
  CheckBoxState: array[TCheckBoxState] of Integer = (csUnchecked, csChecked, csGrayed);
var
  v_CheckBoxRect: TRect;
  v_ImagePos: TPoint;
  v_TextRect: TRect;
  v_NewOffset: Integer;
  v_Image: TIcon;
begin
  with Canvas do
  begin
    FillRect(p_Rect);
    if f_CheckBoxes then // [x] MJ
    begin
      {Draw the CheckBox}
      SetRect(v_CheckBoxRect, p_Rect.Left + Offset, p_Rect.Top, p_Rect.Left + Offset + CheckBoxSize, p_Rect.Bottom);
      DrawFrameControl(Handle, v_CheckBoxRect, DFC_Button, CheckBoxState[State[p_Index]]);
      v_NewOffset := v_CheckBoxRect.Right;
    end
    else
    begin
      v_NewOffset := p_Rect.Left + Offset;
    end;
    {Draw the Image}
    if Assigned(ImageList) then
    begin
      v_Image := TIcon.Create;
      ImageList.GetIcon(ImageIndex[p_Index], v_Image);
      if v_Image <> nil then
      begin
        v_ImagePos := Point(v_NewOffset + 2*Offset, p_Rect.Top + ((p_Rect.Bottom - p_Rect.Top - ImageList.Height) div 2));
        DrawIconEx(Handle, v_ImagePos.X, v_ImagePos.Y, v_Image.Handle, ImageList.Width, ImageList.Height, 0, Brush.Handle, DI_Normal);
        v_NewOffset := v_ImagePos.X + ImageList.Width;
      end;
      v_Image.Free;
    end;
    {Draw the Text}
    SetRect(v_TextRect, v_NewOffset + Offset, p_Rect.Top, p_Rect.Right, p_Rect.Bottom);
    DrawTextW(Handle, PWideChar(Items[p_Index]), Length(Items[p_Index]), v_TextRect, DT_SingleLine or DT_VCenter);
  end;
  if not(csDesigning in ComponentState) and Assigned(OnDrawItem) then inherited;
end;

procedure TGFXListBox.KeyDown(var p_Key: Word; p_Shift: TShiftState);
begin
  if p_Key = VK_Space then
    ChangeState(ItemIndex);
	inherited;
end;

procedure TGFXListBox.MouseDown(p_Button: TMouseButton; p_Shift: TShiftState; p_X, p_Y: Integer);
const
  TargetWidth = Offset + CheckBoxSize;
var
  v_Index: Integer;
  v_Rect: TRect;
begin
  if p_Button = mbLeft then
  begin
    v_Index := ItemAtPos(Point(p_X, p_Y), True);
    if v_Index <> -1 then
    begin
      v_Rect := ItemRect(v_Index);
      if p_X < v_Rect.Left + TargetWidth then
      	ChangeState(v_Index);
    end;
  end;
  inherited;
end;

procedure TGFXListBox.StateChange(p_Index: Integer; var p_Cancel: Boolean);
begin
    if Assigned(f_OnStateChange) then
    f_OnStateChange(p_Index, p_Cancel);
end;

{** Additional Wrapper routines **}

function TGFXListBox.AddItem(const p_Text: wideString; p_State: TCheckBoxState; p_ImageIndex: Integer): Integer;
begin
  Result := Items.Add(p_Text);
  State[Result] := p_State;
  ImageIndex[Result] := p_ImageIndex;
  Locked[Result] := False;
end;

procedure TGFXListBox.InsertItem(p_Index: Integer; const p_Text: wideString; p_State: TCheckBoxState; p_ImageIndex: Integer);
begin
  Items.Insert(p_Index, p_Text);
  State[p_Index] := p_State;
  ImageIndex[p_Index] := p_ImageIndex;
  Locked[p_Index] := False;
end;

procedure TGFXListBox.SetSelected(p_State: TCheckBoxState);
var
  i: Integer;
begin
  for i := 0 to Items.Count-1 do
     if Selected[i] then State[i] := p_State;
end;

procedure TGFXListBox.SetAll(p_State: TCheckBoxState);
var
   i: Integer;
begin
   for i := 0 to Items.Count-1 do
      State[i] := p_State;
end;

procedure TGFXListBox.LockSelected(p_Locked: Boolean);
var
   i: Integer;
begin
   for i := 0 to Items.Count-1 do
      if Selected[i] then Locked[i] := p_Locked;
end;

procedure TGFXListBox.LockAll(p_Locked: Boolean);
var
   i: Integer;
begin
   for i := 0 to Items.Count-1 do
      Locked[i] := p_Locked;
end;


procedure Register;
begin
  RegisterComponents('Custom', [TGFXListBox]);
end;

end.
