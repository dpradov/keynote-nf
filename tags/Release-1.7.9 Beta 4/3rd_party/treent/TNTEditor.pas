unit TNTEditor;

interface

{$I DFS.inc}

{$ifdef DFS_COMPILER_3_UP}
  {$WEAKPACKAGEUNIT ON}
{$endif}

uses Windows, SysUtils, Graphics, Forms, Menus, ExtCtrls, StdCtrls, 
     Dialogs, Controls, Classes, Buttons, ComCtrls, TreeNT;
             
type
  TTreeNTEditor = class(TForm)
    CancelButton: TSpeedButton;
    OKButton: TSpeedButton;
    PopupMenu: TPopupMenu;
    InsertNode: TMenuItem;
    N1: TMenuItem;
    DeleteNode: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    ImageBox: TScrollBox;
    DuplicateNode: TMenuItem;
    StateImageBox: TScrollBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    FontButton: TSpeedButton;
    FontDialog: TFontDialog;
    ColorDialog: TColorDialog;
    Label4: TLabel;
    ColorButton: TSpeedButton;
    Label5: TLabel;
    ParentFontBox: TCheckBox;
    ParentColorBox: TCheckBox;
    SImageBox: TScrollBox;
    Label6: TLabel;
    AddChildItem: TMenuItem;
    IntegralLabel: TLabel;
    IntegralEdit: TEdit;
    StatusBar: TStatusBar;
    CheckTypeLabel: TLabel;
    Label8: TLabel;
    EnabledBox: TCheckBox;
    CheckStyleCombo: TComboBox;
    Panel3: TPanel;
    Tree: TTreeNT;
    N2: TMenuItem;
    Expand: TMenuItem;
    Collapse: TMenuItem;
    SelectAll: TMenuItem;
    procedure CancelButtonClick(Sender: TObject);
    procedure CopyChildren(Source, Destination: TTreeNTNode);
    procedure OKButtonClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure InsertNodeClick(Sender: TObject);
    procedure SelectedImageClick(Sender: TObject);
    procedure StateImageClick(Sender: TObject);
    procedure UnselectedImageClick(Sender: TObject);
    procedure DuplicateNodeClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNTNode);
    procedure TreeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure FontButtonClick(Sender: TObject);
    procedure ColorButtonClick(Sender: TObject);
    procedure ParentFontBoxClick(Sender: TObject);
    procedure ParentColorBoxClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DeleteNodeClick(Sender: TObject);
    procedure AddChildItemClick(Sender: TObject);
    procedure IntegralEditChange(Sender: TObject);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure IntegralEditEnter(Sender: TObject);
    procedure CheckStyleComboChange(Sender: TObject);
    procedure EnabledBoxClick(Sender: TObject);
    procedure TreeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ExpandClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FirstSImageButton,
    FirstImageButton,
    FirstStateButton   : TSpeedButton;
    FChanging          : Boolean;
  public
    procedure DisplayHint(Sender: TObject);
  end;

function EditTreeViewItems(EditItems: TTreeNTNodes): Boolean;
    
//------------------------------------------------------------------------------

implementation

uses Messages;

{$R *.DFM}  // D2 user: copy TNTEditor.dfm from the folder 'Delphi 2 Editor DFM' into the
            // TreeNT folder before you compile the component!

//------------------------------------------------------------------------------
   
function EditTreeViewItems(EditItems: TTreeNTNodes): Boolean;

var Editor       : TTreeNTEditor;
    OldHintEvent : TNotifyEvent;

begin
  Screen.Cursor:=crHourGlass;
  Application.CreateForm(TTreeNTEditor,Editor);
  OldHintEvent:=Application.OnHint;
  try
    with Editor.Tree do
    begin
      Color:=TTreeNT(EditItems.Owner).Color;
      ColorDropSelected:=TTreeNT(EditItems.Owner).ColorDropSelected;
      ColorSelected:=TTreeNT(EditItems.Owner).ColorSelected;
      ColorUnfocusedSelected:=TTreeNT(EditItems.Owner).ColorUnfocusedSelected;
      Options:=TTreeNT(EditItems.Owner).Options + [toRightClickSelect, toMultiSelect, toNoEraseBkGnd] - [toHideSelection, toNoEraseBkgnd];
      Indent:=TTreeNT(EditItems.Owner).Indent;
      ItemHeight:=TTreeNT(EditItems.Owner).ItemHeight;
      Font:=TTreeNT(EditItems.Owner).Font;
      Images:=TTreeNT(EditItems.Owner).Images;
      StateImages:=TTreeNT(EditItems.Owner).StateImages;
      if EditItems.Count > 0 then
      begin
        Items.BeginUpdate;
        Items.Assign(EditItems);
        Items.EndUpdate;
      end
      else Items.TopLevelCheckType:=EditItems.TopLevelCheckType;
      TopItem:=TTreeNT(EditItems.Owner).TopItem;                         
      Selected:=nil;
      OnChange(Editor.Tree,nil);
    end;
    Application.OnHint:=Editor.DisplayHint;
    if Editor.ShowModal = mrOK then
    begin
      EditItems.BeginUpdate;
      EditItems.Assign(Editor.Tree.Items);
      TTreeNT(EditItems.Owner).TopItem:=Editor.Tree.TopItem;
      EditItems.EndUpdate;
      Result:=True;
    end
    else Result:=False;
  finally
    Editor.Free;
    Application.OnHint:=OldHintEvent;
    Screen.Cursor:=crDefault;
  end;
end;

//----------------- helper routines --------------------------------------------

type TEqualType = (etParentFont,etParentColor,etIntegral,etImageIndex,
                   etSelectedIndex,etStateIndex,etFont,etColor,etCheckType,etEnabled);

function AllEqual(Items: TTreeNTNodes; AType: TEqualType): Boolean;

// determines whether all selected nodes in Items have the same value
// returns True if so, else False
// this function is only called if at least one node is selected

var Node       : TTreeNTNode;
    FirstState : Boolean;
    FirstValue : Integer;
    FirstStyle : TCheckType;

begin
  Result:=True;
  Node:=Items.GetFirstSelectedNode;
  case AType of
    etParentFont : begin
                     FirstState:=Node.ParentFont;
                     repeat
                       if Node.ParentFont <> FirstState then
                       begin
                         Result:=False;
                         Break;
                       end;
                       Node:=Node.GetNextSelected;
                     until Node = nil;
                   end;
    etParentColor: begin
                     FirstState:=Node.ParentColor;
                     repeat
                       if Node.ParentColor <> FirstState then
                       begin
                         Result:=False;
                         Break;
                       end;
                       Node:=Node.GetNextSelected;
                     until Node = nil;
                   end;
    etIntegral   : begin
                     FirstValue:=Node.IntegralHeight;
                     repeat
                       if Node.IntegralHeight <> FirstValue then
                       begin
                         Result:=False;
                         Break;
                       end;
                       Node:=Node.GetNextSelected;
                     until Node = nil;
                   end;
    etSelectedIndex : begin
                        FirstValue:=Node.SelectedIndex;
                        repeat
                          if Node.SelectedIndex <> FirstValue then
                          begin
                            Result:=False;
                            Break;
                          end;
                          Node:=Node.GetNextSelected;
                        until Node = nil;
                      end;
    etImageIndex : begin
                     FirstValue:=Node.ImageIndex;
                     repeat
                       if Node.ImageIndex <> FirstValue then
                       begin
                         Result:=False;
                         Break;
                       end;
                       Node:=Node.GetNextSelected;
                     until Node = nil;
                   end;
    etStateIndex : begin
                     FirstValue:=Node.StateIndex;
                     repeat
                       if Node.StateIndex <> FirstValue then
                       begin
                         Result:=False;
                         Break;
                       end;
                       Node:=Node.GetNextSelected;
                     until Node = nil;
                   end;
    etCheckType : begin
                     FirstStyle:=Node.CheckType;
                     repeat
                       if Node.CheckType <> FirstStyle then
                       begin
                         Result:=False;
                         Break;
                       end;
                       Node:=Node.GetNextSelected;
                     until Node = nil;
                   end;
    etEnabled : begin
                  FirstState:=Node.Enabled;
                  repeat
                    if Node.Enabled <> FirstState then
                    begin
                      Result:=False;
                      Break;
                    end;
                    Node:=Node.GetNextSelected;
                  until Node = nil;
                end;
  end;
end;

//------------------------------------------------------------------------------

procedure AssignValueToSelection(Items: TTreeNTNodes; AType: TEqualType; Value: Integer);

// assigns the given Value to the to AType correlating property of all
// selected nodes

var Node  : TTreeNTNode;

begin
  Node:=Items.GetFirstSelectedNode;
  case AType of
    etParentFont : while assigned(Node) do
                   begin
                     Node.ParentFont:=Value <> 0;
                     Node:=Node.GetNextSelected;
                   end;
    etParentColor: while assigned(Node) do
                   begin
                     Node.ParentColor:=Value <> 0;
                     Node:=Node.GetNextSelected;
                   end;
    etIntegral   : while assigned(Node) do
                   begin
                     Node.IntegralHeight:=Value;
                     Node:=Node.GetNextSelected;
                   end;
    etSelectedIndex : while assigned(Node) do
                      begin
                        Node.SelectedIndex:=Value;
                        Node:=Node.GetNextSelected;
                      end;
    etImageIndex : while assigned(Node) do
                   begin
                     Node.ImageIndex:=Value;
                     Node:=Node.GetNextSelected;
                   end;
    etStateIndex : while assigned(Node) do
                   begin
                     Node.StateIndex:=Value;
                     Node:=Node.GetNextSelected;
                   end;
    etFont : while assigned(Node) do
             begin
               Node.Font:=TFont(Value);
               Node:=Node.GetNextSelected;
             end;
    etColor : while assigned(Node) do
              begin
                Node.Color:=Value;
                Node:=Node.GetNextSelected;
              end;
    etCheckType : while assigned(Node) do
                  begin
                    Node.CheckType:=TCheckType(Value);
                    Node:=Node.GetNextSelected;
                  end;
    etEnabled : while assigned(Node) do
                begin
                  Node.Enabled:=Value <> 0;
                  Node:=Node.GetNextSelected;
                end;
  end;
end;

//----------------- editor routines --------------------------------------------

procedure TTreeNTEditor.AddChildItemClick(Sender: TObject);

var Node : TTreeNTNode;

begin
  Node:=Tree.Items.GetFirstSelectedNode;
  while assigned(Node) do
  begin
    Tree.Items.AddChild(Node,'New Child').MakeVisible;
    Node:=Node.GetNextSelected;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.CancelButtonClick(Sender: TObject);

begin
  ModalResult:=mrCancel;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.OKButtonClick(Sender: TObject);

begin
  ModalResult:=mrOK;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.FormKeyPress(Sender: TObject; var Key: Char);

begin
  if Key = #27 then
  begin
    Key:=#0;
    ModalResult:=mrCancel;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.InsertNodeClick(Sender: TObject);

var Node : TTreeNTNode;

begin
  Node:=Tree.Items.GetFirstSelectedNode;
  if assigned(Node) then 
  begin
    while assigned(Node) do
    begin
      if Node.Parent = nil then Tree.Items.Insert(Node,'New Node').MakeVisible
                           else Tree.Items.Insert(Node,'New Child').MakeVisible;
      Node:=Node.GetNextSelected;
    end;
  end
  else Tree.Items.Add(nil,'New Node');
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.CopyChildren(Source, Destination: TTreeNTNode);

var I : Integer;
    Node : TTreeNTNode;

begin
  if Source.Count > 0 then
    with Tree.Items do
      for I:=0 to Source.Count-1 do
      begin
        Node:=AddChild(Destination,'');
        Node.Assign(Source[I]);
        CopyChildren(Source[I],Node);
      end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.DuplicateNodeClick(Sender: TObject);

var Node, NewNode : TTreeNTNode;

begin
  Tree.Items.BeginUpdate;
  Tree.Items.LockSelection;
  Node:=Tree.Items.GetFirstSelectedNode;
  while assigned(Node) do
  begin
    NewNode:=Tree.Items.Insert(Node,'');
    NewNode.Assign(Node);
    CopyChildren(Node,NewNode);
    Node:=Node.GetNextSelected;
  end;
  Tree.Items.UnlockSelection;
  Tree.Items.EndUpdate;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.PopupMenuPopup(Sender: TObject);

var SelCount : Integer;

begin
  SelCount:=Tree.Items.SelectedCount;
  if SelCount > 0 then
  begin
    DuplicateNode.Enabled:=True;
    DeleteNode.Enabled:=True;
    if SelCount > 1 then DeleteNode.Caption:='D&elete '+IntToStr(SelCount)+' Items + children'
                    else DeleteNode.Caption:='D&elete Item + children';
    AddChildItem.Enabled:=True;
  end
  else
  begin
    DuplicateNode.enabled:=False;
    DeleteNode.Enabled:=False;
    AddChildItem.Enabled:=False;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.SelectedImageClick(Sender: TObject);

begin
  with Sender as TSpeedButton do
  if not FChanging then 
    if assigned(Tree.Selected) then
    begin
      if Down then AssignValueToSelection(Tree.Items,etSelectedIndex,Tag)
              else AssignValueToSelection(Tree.Items,etSelectedIndex,-1);
      Tree.Refresh;
    end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.StateImageClick(Sender: TObject);

begin
  with Sender as TSpeedButton do
  if not FChanging then
    if assigned(Tree.Selected) then
      if Down then AssignValueToSelection(Tree.Items,etStateIndex,Tag)
              else AssignValueToSelection(Tree.Items,etStateIndex,-1);
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.UnselectedImageClick(Sender: TObject);

begin
  with Sender as TSpeedButton do
  if not FChanging then
    if assigned(Tree.Selected) then
      if Down then AssignValueToSelection(Tree.Items,etImageIndex,Tag)
              else AssignValueToSelection(Tree.Items,etImageIndex,-1);
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.TreeChange(Sender: TObject; Node: TTreeNTNode);

// Handles selection changes in the editor tree view.
// Node is the currently focused node, which doesn't have necessarily to
// be (one of) the selected node(s).

var AButton   : TSpeedButton;
    SelCount  : Integer;
    FirstNode : TTreeNTNode;
                                
begin
  FChanging:=True;
  try
    // determine number of selected nodes
    SelCount:=Tree.Items.SelectedCount;
    FirstNode:=Tree.Items.GetFirstSelectedNode;
    Panel3.Caption:=Format('selected nodes:  %d',[SelCount]);

    // handle selected/unselected image indices
    if assigned(Tree.Images) and (Tree.Images.Count > 0) then
    begin
      AButton:=nil;
      if assigned(FirstNode) and AllEqual(Tree.Items,etImageIndex) then
        AButton:=FindComponent('I'+IntToStr(FirstNode.ImageIndex)) as TSpeedButton;
      if assigned(AButton) then AButton.Down:=True
                           else
        if assigned(FirstImageButton) then
        begin
          // bring all buttons (beside the first) in Up state
          FirstImageButton.Down:=True;
          // now do the same with the first button, so no button is finally down
          FirstImageButton.Down:=False;
        end;

      AButton:=nil;
      if assigned(FirstNode) and AllEqual(Tree.Items,etSelectedIndex) then
        AButton:=FindComponent('SI'+IntToStr(FirstNode.SelectedIndex)) as TSpeedButton;
      if assigned(AButton) then AButton.Down:=True
                           else
        if assigned(FirstSImageButton) then
        begin
          FirstSImageButton.Down:=True;
          FirstSImageButton.Down:=False;
        end
    end;

    // handle state image indices
    if assigned(Tree.StateImages) and (Tree.StateImages.Count > 0) then
    begin
      AButton:=nil;
      if assigned(FirstNode) and AllEqual(Tree.Items,etStateIndex) then
        AButton:=FindComponent('STI'+IntToStr(FirstNode.StateIndex)) as TSpeedButton;
      if assigned(AButton) then AButton.Down:=True
                           else
        if assigned(FirstStateButton) then
        begin
          FirstStateButton.Down:=True;
          FirstStateButton.Down:=False;
        end
    end;

    if SelCount > 0 then
    begin
      FontButton.Enabled:=True;
      ParentFontBox.Enabled:=True;
      if AllEqual(Tree.Items,etParentFont) then ParentFontBox.Checked:=FirstNode.ParentFont
                                           else ParentFontBox.State:=cbGrayed;

      ColorButton.enabled:=True;
      ParentColorBox.Enabled:=True;
      if AllEqual(Tree.Items,etParentColor) then ParentColorBox.Checked:=FirstNode.ParentColor
                                            else ParentColorBox.State:=cbGrayed;

      EnabledBox.Enabled:=True;
      if AllEqual(Tree.Items,etEnabled) then
        if FirstNode.Enabled then EnabledBox.State:=cbChecked
                             else EnabledBox.State:=cbUnchecked
                                         else EnabledBox.State:=cbGrayed;

      CheckTypeLabel.Caption:='Children check type:';
      if AllEqual(Tree.Items,etCheckType) then CheckStyleCombo.ItemIndex:=Ord(FirstNode.CheckType)
                                          else CheckStyleCombo.ItemIndex:=-1;
        
      IntegralEdit.Enabled:=True;
      IntegralEdit.Color:=clWindow;
      if AllEqual(Tree.Items,etIntegral) then IntegralEdit.Text:=IntToStr(FirstNode.IntegralHeight)
                                         else IntegralEdit.Text:='';
    end
    else
    begin
      FontButton.enabled:=False;                  
      ParentFontBox.Enabled:=False;
      ColorButton.enabled:=False;
      ParentColorBox.Enabled:=False;
      EnabledBox.Enabled:=False;
      CheckTypeLabel.Caption:='Top level check type:';
      CheckStyleCombo.ItemIndex:=Ord(Tree.Items.TopLevelCheckType);
      IntegralEdit.Text:='';
      IntegralEdit.Enabled:=False;
      IntegralEdit.Color:=clBtnHighlight;
    end;
  finally
    FChanging:=False;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.TreeDragOver(Sender, Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);

// determines whether we could accept a drop operation

var Target, Node : TTreeNTNode;

begin
  Accept:=False;
  // something to drop to?
  // the drop target is assigned after this procedure returns (and only if we
  // pass Accept = True back), so we need to determine the potential drop target
  // instead of just using Tree.DropTarget
  Target:=Tree.GetNodeAt(X,Y);
  if assigned(Target) then
  begin
    // determine whether at least one node could be dropped
    Node:=Tree.Items.GetFirstSelectedNode;
    while assigned(Node) do
    begin
      Accept:=Accept or not Target.HasAsParent(Node);
      if Accept then Break;
      Node:=Node.GetNextSelected;
    end;
  end;  
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.TreeDragDrop(Sender, Source: TObject; X, Y: Integer);

// accomplishs a drop operation

var Target, Node : TTreeNTNode;

begin                                       
  Tree.Items.BeginUpdate;
  Tree.Items.LockSelection;
  try
    // something to drop to?
    Target:=Tree.GetNodeAt(X,Y);
    if assigned(Target) then
    begin
      // move only nodes which are not a parent of the drop target
      Node:=Tree.Items.GetFirstSelectedNode;
      while assigned(Node) do
      begin
        if not Target.HasAsParent(Node) then Node.MoveTo(Target,naAddChild);
        Node:=Node.GetNextSelected;
      end;
    end;
    TreeChange(nil,Tree.Selected);
  finally
    Tree.Items.UnlockSelection;
    Tree.Items.EndUpdate;
  end;                         
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.TreeStartDrag(Sender: TObject; var DragObject: TDragObject);

begin
  if assigned(Tree.Selected) then
    if (Tree.Selected.Count > 1) or
       (Tree.Items.SelectedCount > 1) then Tree.DragCursor:=crMultiDrag
                                      else Tree.DragCursor:=crDrag;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.FontButtonClick(Sender: TObject);

begin
  FontDialog.Font:=Tree.Items.GetFirstSelectedNode.Font;
  if FontDialog.Execute then
  begin
    AssignValueToSelection(Tree.Items,etFont,Integer(FontDialog.Font));
    ParentFontBox.Checked:=False;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.ColorButtonClick(Sender: TObject);

begin
  with Tree.Selected do
  begin
    if ParentColor then ColorDialog.Color:=Tree.Color
                   else ColorDialog.Color:=Color;
    if ColorDialog.Execute then
    begin
      AssignValueToSelection(Tree.Items,etColor,Integer(ColorDialog.Color));
      ParentColorBox.Checked:=False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.ParentFontBoxClick(Sender: TObject);

begin
  if not FChanging then
    AssignValueToSelection(Tree.Items,etParentFont,Ord(ParentFontBox.Checked));
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.ParentColorBoxClick(Sender: TObject);

begin
  if not FChanging then
    AssignValueToSelection(Tree.Items,etParentColor,Ord(ParentColorBox.Checked));
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.FormShow(Sender: TObject);

var Button1,
    Button2  : TSpeedButton;
    I        : Integer;
    ABitmap  : TBitmap;

begin
  ABitmap:=TBitmap.Create;
  if assigned(Tree.Images) and (Tree.Images.Count > 0) then
    with Tree.Images do
    begin
      Button1:=nil; // just to satisfy the compiler
      Button2:=nil;
      for I:=0 to Count-1 do
      begin
        // the form will own the buttons and destroy them
        // buttons for unselected images
        Button1:=TSpeedButton.Create(Self);
        if I = 0 then FirstImageButton:=Button1;
        GetBitmap(I,ABitmap);
        with Button1 do
        begin
          Parent:=ImageBox;
          Width:=32;
          Height:=32;
          Glyph:=ABitmap;
          Top:=2;
          Left:=I*Width;
          GroupIndex:=1;
          Tag:=I;
          Name:='I'+IntToStr(I);
          AllowAllUp:=True;
          {$ifdef DFS_COMPILER_3_UP}
            Flat:=True;
          {$endif}
          OnClick:=UnselectedImageClick;
        end;
        // buttons for selected images
        Button2:=TSpeedButton.Create(Self);
        if I = 0 then FirstSImageButton:=Button2;
        with Button2 do
        begin
          Parent:=SImageBox;
          Width:=32;
          Height:=32;
          Glyph:=ABitmap;
          Top:=2;
          Left:=I*Width;
          GroupIndex:=2;
          Tag:=I;
          Name:='SI'+IntToStr(I);
          AllowAllUp:=True;
          {$ifdef DFS_COMPILER_3_UP}
            Flat:=True;
          {$endif}
          OnClick:=SelectedImageClick;
        end;
        ABitmap.Assign(nil);
      end;
      ImageBox.HorzScrollBar.Range:=Count*Button1.Width;
      SImageBox.HorzScrollBar.Range:=Count*Button2.Width;
    end;

  if assigned(Tree.StateImages) and (Tree.StateImages.Count > 0) then
    with Tree.StateImages do
    begin
      Button1:=nil; // just to satisfy the compiler
      for I:=0 to Count-1 do
      begin
        // the form will own the buttons and destroy them
        // buttons for state images
        Button1:=TSpeedButton.Create(Self);
        if I = 0 then FirstStateButton:=Button1;
        GetBitmap(I,ABitmap);
        with Button1 do
        begin
          Parent:=StateImageBox;
          Width:=32;
          Height:=32;
          Glyph:=ABitmap;
          Top:=2;
          Left:=I*Width;
          GroupIndex:=3;
          Tag:=I;
          Name:='STI'+IntToStr(I);
          AllowAllUp:=True;
          {$ifdef DFS_COMPILER_3_UP}
            Flat:=True;
          {$endif}
          OnClick:=StateImageClick;
        end;
        ABitmap.Assign(nil);
      end;
      StateImageBox.HorzScrollBar.Range:=Count*Button1.Width;
    end;
  ABitmap.Free;
  ImageBox.Color:=Tree.Color;
  SImageBox.Color:=Tree.Color;
  StateImageBox.Color:=Tree.Color;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.DeleteNodeClick(Sender: TObject);

begin
  Tree.Items.DeleteSelectedNodes;
end;
              
//------------------------------------------------------------------------------

procedure TTreeNTEditor.IntegralEditChange(Sender: TObject);

var Value : Integer;

begin
  if not FChanging then
    if IntegralEdit.Text > '' then
    begin
      Value:=StrToInt(IntegralEdit.Text);
      if Value < 1 then Abort;
      AssignValueToSelection(Tree.Items,etIntegral,Value);
    end;
end;
      
//------------------------------------------------------------------------------

procedure TTreeNTEditor.TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if not Tree.IsEditing then
    case Key of
      VK_DELETE : DeleteNodeClick(nil);
      VK_INSERT : InsertNodeClick(nil);
    end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.IntegralEditEnter(Sender: TObject);

begin
  IntegralEdit.SelectAll;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.DisplayHint(Sender: TObject);

begin
  StatusBar.SimpleText:=GetLongHint(Application.Hint);
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.CheckStyleComboChange(Sender: TObject);

begin
  with Sender as TComboBox do
  if not FChanging then
    if Tree.Items.SelectedCount > 0 then AssignValueToSelection(Tree.Items,etCheckType,ItemIndex)
                                    else Tree.Items.TopLevelCheckType:=TCheckType(ItemIndex);
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.EnabledBoxClick(Sender: TObject);

begin
  with Sender as TCheckBox do
  if not FChanging then
      AssignValueToSelection(Tree.Items,etEnabled,Ord(Checked));
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.TreeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

var Node : TTreeNTNode;

begin
  if (Button = mbRight) and (Shift = []) then
  begin
    Node:=Tree.GetNodeAt(X,Y);
    if assigned(Node) then
    begin
      Node.Selected:=True;
      Node.Focused:=True;
    end
    else Tree.Selected:=nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.ExpandClick(Sender: TObject);

begin
  if Tree.Items.Count > 0 then
  begin
    Tree.Items.BeginUpdate;
    try
      if Sender = Collapse then Tree.FullCollapse
                           else Tree.FullExpand;
    finally
      Tree.Items.EndUpdate;
    end;
  end;  
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.SelectAllClick(Sender: TObject);

begin
  Tree.Items.SelectAll;
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.FormCreate(Sender: TObject);

begin
  Tree.ControlStyle:=Tree.ControlStyle+[csFramed];
end;

//------------------------------------------------------------------------------

end.
                      
