unit kn_AlertMng;
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

 The Original Code is KeyNote 1.7.1

 The Initial Developer of the Original Code is Daniel Prado
 <dprado.keynote@gmail.com> (Spain).
 Portions created by Daniel Prado are
 Copyright (C) 2007. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 Released: 15 Nov 2007
 -----------------------------------------------------------
 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net

 Email address
<dprado.keynote@gmail.com>

************************************************************ *)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, TreeNT,
  StdCtrls, ComCtrls,
  kn_NoteObj, kn_NodeList, TB97Ctls, ExtCtrls;

type
  TForm_Alarm = class(TForm)
    TV: TTreeNT;
    Panel1: TPanel;
    Tomorrow_8AM: TToolbarButton97;
    Today_5min: TToolbarButton97;
    Today_10min: TToolbarButton97;
    Today_15min: TToolbarButton97;
    Today_30min: TToolbarButton97;
    Today_1h: TToolbarButton97;
    Today_2h: TToolbarButton97;
    Tomorrow_12AM: TToolbarButton97;
    Today_3h: TToolbarButton97;
    Today_3PM: TToolbarButton97;
    Today_6PM: TToolbarButton97;
    CB_Date: TDateTimePicker;
    Tomorrow_3PM: TToolbarButton97;
    Tomorrow_6PM: TToolbarButton97;
    CB_Time: TDateTimePicker;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button_DiscardAll: TButton;
    Button_Discard: TButton;
    Button_Postpone: TButton;
    Label_Selected: TLabel;
    Label_Selected_Alarm: TLabel;
    Button_Show: TButton;
    UpDown1: TUpDown;
    Button_ShowALL: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button_ShowALLClick(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure Button_ShowClick(Sender: TObject);
    procedure TVDblClick(Sender: TObject);
    procedure Button_DiscardAllClick(Sender: TObject);
    procedure Button_DiscardClick(Sender: TObject);
    procedure TVChange(Sender: TObject; Node: TTreeNTNode);
    procedure Button_PostponeClick(Sender: TObject);
    procedure TB_NoAlarmClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CB_TimeChange(Sender: TObject);
    procedure CB_DateChange(Sender: TObject);

  private
    { Private declarations }
    FAlarmList: TList;          // All alarms
    FSelectedAlarmList: TList;
    FOptionSelected: TObject;
    NodeSelected: TTreeNTNode;
    FNumberAlarms: integer;

    procedure EnableControls (Value: Boolean);
    function nodeOfNote (node: TTreeNTNode): TTreeNTNode;
    procedure RemoveNode;

  public
    { Public declarations }
    ButtonOK: Boolean;
    modeEdit: Boolean;
    property SelectedAlarmList: TList read FSelectedAlarmList write FSelectedAlarmList;
    property AlarmList: TList read FAlarmList write FAlarmList;
  end;


  TAlarmManager = class
  private
    FAlarmList: TList;
    FEnabled: Boolean;
    FSelectedAlarmList: TList;

    FCanceledAt: TDateTime;

    procedure SetEnabled (Value: boolean);
    procedure ShowFormAlarm (modeEdit: Boolean);
    //procedure checkCanceledAt ( node : TTreeNTNode );
  protected

  public
    property Enabled: Boolean read FEnabled write SetEnabled;
    procedure EditAlarm (node: TTreeNTNode);
    procedure checkAlarms;
    procedure AddAlarmNode( node : TTreeNTNode );
    procedure RemoveAlarmNode( node : TTreeNTNode );
    procedure ModifyAlarmNode( node : TTreeNTNode );
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;


var
  Form_Alarm: TForm_Alarm;

implementation
uses  DateUtils, ComCtrls95,
      kn_Main, kn_Global, kn_TreeNoteMng, kn_Const, kn_LocationObj, kn_FindReplaceMng;

{$R *.DFM}

resourcestring
  STR_Num = '%d Alarms';
  STR_NoSelected = '0 alarms selected';
  STR_Apply = '&Apply';
  STR_Postpone = '&Postpone';
  STR_CaptionSet = 'Set Alarm  (%d Alarms created)';


constructor TAlarmManager.Create;
begin
   inherited Create;
   FEnabled:= false;
   FAlarmList:= TList.Create;
   FAlarmList.Capacity:= 10;
   FSelectedAlarmList:= TList.Create;
   FCanceledAt:= 0;
end;

destructor TAlarmManager.Destroy;
begin
   if assigned (FAlarmList) then begin
      FAlarmList.Free;
   end;
   if assigned (FSelectedAlarmList) then begin
      FSelectedAlarmList.Free;
   end;
   inherited Destroy;
end;

procedure TAlarmManager.Clear;
begin
   if assigned (FAlarmList) then begin
      FAlarmList.Clear;
   end;
   if assigned (FSelectedAlarmList) then begin
      FSelectedAlarmList.Clear;
   end;
end;

function compareAlarmNotes (node1, node2: Pointer): integer;
begin
   if TNoteNode(TTreeNTNode(node1).Data).AlarmF = TNoteNode(TTreeNTNode(node2).Data).AlarmF  then
      Result:= 0
   else if TNoteNode(TTreeNTNode(node1).Data).AlarmF > TNoteNode(TTreeNTNode(node2).Data).AlarmF then
      Result:= 1
   else
      Result:= -1;
end;

procedure TAlarmManager.SetEnabled (Value: boolean);
begin
     if Value <> FEnabled then
     begin
       FEnabled:= Value;
       if FEnabled then
          FAlarmList.Sort(compareAlarmNotes);
     end;
end;

procedure TAlarmManager.EditAlarm (node: TTreeNTNode);
begin
    Form_Main.Timer.Enabled := False;
    try
      FSelectedAlarmList.Clear;
      FSelectedAlarmList.Add(node);

      ShowFormAlarm (true);
      Form_Main.TB_AlarmNode.Down:= (TNoteNode(TTreeNTNode(FSelectedAlarmList[0]).Data).AlarmF <> 0);

      FSelectedAlarmList.Clear;
    finally
      Form_Main.Timer.Enabled := true;
    end;
end;

procedure TAlarmManager.ShowFormAlarm (modeEdit: Boolean);
begin
  if ( Form_Alarm = nil ) then
  begin
    Form_Alarm := TForm_Alarm.Create( Form_Main );
  end;

  try
    Form_Alarm.SelectedAlarmList:= FSelectedAlarmList;
    Form_Alarm.AlarmList:= FAlarmList;
    Form_Alarm.modeEdit:= modeEdit;

    Form_Alarm.CB_Date.MinDate:= Today;
    Form_Alarm.Today_5min.Down:= true;
    Form_Alarm.CB_Date.DateTime := incMinute(now(), 5);
    Form_Alarm.CB_Time.DateTime := Form_Alarm.CB_Date.DateTime;
    Form_Alarm.CB_Date.Checked:= false;
    Form_Alarm.CB_Time.Checked:= false;

    Form_Alarm.ShowModal;

  except
    on E : Exception do
    begin
      messagedlg( E.Message, mtError, [mbOK], 0 );
    end;
  end;

end;

procedure TAlarmManager.AddAlarmNode( node : TTreeNTNode );
begin
    FAlarmList.Add(node);
    if FEnabled then
       FAlarmList.Sort(compareAlarmNotes);
    //checkCanceledAt (node);
end;

(*
procedure TAlarmManager.checkCanceledAt ( node : TTreeNTNode );
var
  alarm: TDateTime;
begin
    if FCanceledAt <> 0 then begin
       alarm:= TNoteNode(node.Data).Alarm;
       if alarm < incMinute(FCanceledAt,5) then
          FCanceledAt:= incMinute(alarm, -5);
    end;
end;
*)

procedure TAlarmManager.RemoveAlarmNode( node : TTreeNTNode );
begin
    FAlarmList.Remove(node);
end;

procedure TAlarmManager.ModifyAlarmNode( node : TTreeNTNode );
begin
    if FEnabled then
       FAlarmList.Sort(compareAlarmNotes);

    //checkCanceledAt (node);
end;

procedure TAlarmManager.checkAlarms;
var
  I: Integer;
  node: TNoteNode;
  limit: TDateTime;
begin
   if FCanceledAt <> 0 then
      limit:= incMinute(FCanceledAt, 5);

   Form_Main.Timer.Enabled := False;
   try
     I:= 0;
     FSelectedAlarmList.Clear;

     while I <= FAlarmList.Count - 1 do begin
        node:= TTreeNTNode(FAlarmList[i]).Data;
        if now() >= node.AlarmF then begin
           if (FCanceledAt = 0) or (node.AlarmF > FCanceledAt) or (now > limit) then
              FSelectedAlarmList.Add(FAlarmList[i]);
           I:= I + 1;
        end
        else
           break;
     end;

     if FSelectedAlarmList.Count > 0 then begin
         if IsIconic( Application.Handle ) then begin
            Application.Restore;
            Application.BringToFront;
         end;

         ShowFormAlarm (false);
         FSelectedAlarmList.Clear;
         if assigned(ActiveNote) and (ActiveNote.Kind=ntTree) and assigned(TTreeNote(ActiveNote).TV.Selected)  then
            Form_Main.TB_AlarmNode.Down:= (TNoteNode(TTreeNote(ActiveNote).TV.Selected.Data).Alarm <> 0);
         if Form_Alarm.ButtonOK then
            FCanceledAt:= 0
         else
            FCanceledAt:= now;
     end;

   finally
     Form_Main.Timer.Enabled := true;
   end;

end;

procedure TForm_Alarm.Button_ShowALLClick(Sender: TObject);
begin
    //Button_PostponeClick(nil);
    FSelectedAlarmList:= FAlarmList;
    FormShow(nil);
end;

procedure TForm_Alarm.Button_ShowClick(Sender: TObject);
begin
     TVDblClick(nil);
end;

procedure TForm_Alarm.Button_DiscardAllClick(Sender: TObject);
var
  node: TTreeNTNode;
  nodeAlarm: TTreeNTNode;
begin
    ButtonOK:= true;
    NoteFile.Modified := true;
    with TV.Items do
    begin
      BeginUpdate;
      node:= GetFirstNode;
      while assigned(node) do begin
          nodeAlarm:= TTreeNTNode(Node.Data);
          if assigned(nodeAlarm) then begin
             TNoteNode(nodeAlarm.Data).Alarm:= 0;
             AlarmManager.RemoveAlarmNode (nodeAlarm);
          end;
          node:= node.GetNext;
      end;
      Clear;
      EndUpdate;
    end;
    Close;
end;

procedure TForm_Alarm.Button_DiscardClick(Sender: TObject);
begin
   if assigned(NodeSelected) then begin
      NoteFile.Modified := true;
      TNoteNode(NodeSelected.Data).Alarm:= 0;
      AlarmManager.RemoveAlarmNode (NodeSelected);
      RemoveNode;
   end;
end;

procedure TForm_Alarm.Button_PostponeClick(Sender: TObject);
var
   AlarmOld: TDateTime;
   myNode: TNoteNode;
begin
   if assigned(NodeSelected) then begin
      myNode:= TNoteNode(NodeSelected.Data);
      AlarmOld:= myNode.AlarmF;
      myNode.Alarm:= RecodeTime(CB_Date.DateTime, HourOf(CB_Time.Time), MinuteOf(CB_Time.Time), 0, 0);;
      NoteFile.Modified := true;
      if AlarmOld = 0 then
        AlarmManager.AddAlarmNode(NodeSelected)
      else
        AlarmManager.ModifyAlarmNode(NodeSelected);

      RemoveNode;
   end;
end;

procedure TForm_Alarm.RemoveNode;
var
   nodeParent: TTreeNTNode;
   nodePrev: TTreeNTNode;
begin
  nodeParent:= TV.Selected.Parent;
  nodePrev:= TV.Selected.GetPrev;
  if nodePrev.Level = 0 then begin
     nodePrev:= nodePrev.GetPrev;
     if nodePrev = nil then begin
        nodePrev:= TV.Selected.GetNext;
        if assigned(nodePrev) and (nodePrev.Level = 0) then
          nodePrev:= nodePrev.GetNext;
     end;
  end;
  TV.Selected.Delete;
  if not nodeParent.HasChildren then
     nodeParent.Delete;

  if TV.Items.Count = 0 then begin
     ButtonOK:= True;
     Close;
  end
  else begin
     TV.Selected:= nodePrev;
     FNumberAlarms:= FNumberAlarms-1;
     Caption:= Format(STR_Num, [FNumberAlarms]);
  end;
end;


procedure TForm_Alarm.CB_DateChange(Sender: TObject);
begin
    if not CB_Date.Checked  then begin
      Today_5min.Down:= true;
      CB_Date.DateTime := incMinute(now(), 5);
      CB_Time.DateTime := CB_Date.DateTime;
      CB_Date.Checked:= false;
      CB_Time.Checked:= false;
    end
    else begin
      CB_Time.Checked:= true;
      Today_5min.Down:= false;
      //RecodeTime(CB_Date.DateTime, HourOf(CB_Time.Time), MinuteOf(CB_Time.Time), 0, 0);
      //CB_Time.DateTime:= CB_Date.DateTime;
      if FOptionSelected <> nil then begin
         TToolbarButton97(FOptionSelected).Down:= false;
         FOptionSelected:= nil;
      end;
    end;
end;

procedure TForm_Alarm.CB_TimeChange(Sender: TObject);
begin
    if not CB_Time.Checked  then
      CB_Date.Checked:= false
    else begin
      CB_Date.Checked:= true;
    end;
    CB_DateChange(CB_Date);
end;

function TForm_Alarm.nodeOfNote (node: TTreeNTNode): TTreeNTNode;
var
  I: Integer;
  note: TTabNote;

begin
  note:= NoteFile.GetNoteByTreeNode(node);
  Result:= nil;
  with TV.Items do begin
      node:= GetFirstNode;
      while assigned(node) do begin
          if node.Text = note.Name then begin
             Result:= node;
             break;
          end;
          node:= node.GetNext;
      end;
      if Result = nil then begin
         Result := Add(nil, note.Name );
         Result.Font.Color := clBlue;
      end;
  end;

end;


procedure TForm_Alarm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_ESCAPE then begin
     ButtonOK:= False;
     Close;
  end;

end;

procedure TForm_Alarm.FormShow(Sender: TObject);
var
  node: TTreeNTNode;
  Child : TTreeNTNode;
  I: Integer;
  nodeNote: TTreeNTNode;
begin
   if (FSelectedAlarmList.Count = 0) then begin
      if Sender <> nil then
        Close
      else begin
        NodeSelected:= nil;
        TV.Items.Clear;
        Label_Selected.Caption := STR_NoSelected;
        Label_Selected_Alarm.Caption := '';
        EnableControls (false);
      end;
   end
   else begin
      UpDown1.Position:= 500;
      FNumberAlarms:= FSelectedAlarmList.Count;
      ButtonOK:= False;
      with TV.Items do
      begin
        BeginUpdate;
        Clear;
        for I := 0 to FSelectedAlarmList.Count - 1 do
        begin
          node:= TTreeNTNode (FSelectedAlarmList[I]);
          nodeNote:= nodeOfNote(node);
          Child := AddChild(nodeNote, TNoteNode(node.Data).Name );
          Child.Data:= node;
        end;
        TV.FullExpand;
        EndUpdate;

        TV.Selected := GetFirstNode.GetNext;

        if modeEdit then begin
          Button_Postpone.Caption:= STR_Apply;
          Caption:= Format(STR_CaptionSet, [FAlarmList.Count]);
        end
        else begin
          Button_Postpone.Caption:= STR_Postpone;
          Caption:= Format(STR_Num, [FSelectedAlarmList.Count]);
        end;
      end;
   end;
end;

procedure TForm_Alarm.TB_NoAlarmClick(Sender: TObject);
var
   minInc: integer;
   Alarm: TDateTime;
begin
    if Sender = nil then exit;

    if not TToolbarButton97(Sender).Down then begin
       TToolbarButton97(Sender).Down:= true;
       exit;
    end;

    minInc:= 0;
    Alarm := 0;
    FOptionSelected:= Sender;

    if Today_5min.Down then
       minInc:= 5
    else if Today_10min.Down then
       minInc:= 10
    else if Today_15min.Down then
       minInc:= 15
    else if Today_30min.Down then
       minInc:= 30
    else if Today_1h.Down then
       minInc:= 60
    else if Today_2h.Down then
       minInc:= 120
    else if Today_3h.Down then
       minInc:= 180;

    if minInc <> 0 then
       Alarm:= incMinute(now(), minInc)
    else if Today_3PM.Down then
       Alarm:= incHour(Today(), 15)
    else if Today_6PM.Down then
       Alarm:= incHour(Today(), 18)
    else if Tomorrow_8AM.Down then
       Alarm:= incHour(Tomorrow(), 8)
    else if Tomorrow_12AM.Down then
       Alarm:= incHour(Tomorrow(), 12)
    else if Tomorrow_3PM.Down then
       Alarm:= incHour(Tomorrow(), 15)
    else if Tomorrow_6PM.Down then
       Alarm:= incHour(Tomorrow(), 18);

    CB_Date.DateTime := Alarm;
    CB_Time.DateTime := Alarm;
    CB_Date.Checked:= false;
    CB_Time.Checked:= false;
end;


procedure TForm_Alarm.TVChange(Sender: TObject; Node: TTreeNTNode);
var
   myNode: TNoteNode;
begin
    FOptionSelected:= nil;
    myNode:= nil;
    if not assigned(Node) or not assigned(Node.Data) then begin
       NodeSelected:= nil;
       Label_Selected.Caption := STR_NoSelected;
       Label_Selected_Alarm.Caption := '';
       EnableControls (false);
    end
    else
    begin
        NodeSelected:= TTreeNTNode(Node.Data);
        EnableControls (true);
        if assigned(NodeSelected) then begin
           myNode:= TNoteNode(NodeSelected.Data);
           Label_Selected.Caption :=  myNode.Name;
           if myNode.AlarmF <> 0 then
              Label_Selected_Alarm.Caption := FormatDateTime( 'dddd, d MMMM yyyy ' + #32 + 'HH:mm', myNode.AlarmF ) + ' :'
           else
              Label_Selected_Alarm.Caption := '';
        end;
    end;

    TB_NoAlarmClick(nil);
end;

procedure TForm_Alarm.TVDblClick(Sender: TObject);
var
  Location: TLocation;
  note: TTabNote;
begin
    if assigned(NodeSelected) then begin
      note:= NoteFile.GetNoteByTreeNode(NodeSelected);
      Location := TLocation.Create;
      Location.FileName := notefile.FileName;
      Location.NoteName := note.Name;
      Location.NodeName := TNoteNode(NodeSelected.Data).Name;
      Location.CaretPos := 0;
      Location.SelLength := 0;
      Location.NoteID := Note.ID;
      Location.NodeID := TNoteNode(NodeSelected.Data).ID;

      JumpToLocation (Location);
    end;

end;

procedure TForm_Alarm.UpDown1Click(Sender: TObject; Button: TUDBtnType);
var
   inc: integer;
begin
    if Button = btNext then
       inc:= 1
    else
       inc:= -1;

    CB_Time.DateTime:= incMinute(CB_Time.DateTime,inc);
    CB_Date.Checked := true;
end;

procedure TForm_Alarm.EnableControls (Value: Boolean);
begin
   Today_5min.Enabled:= Value;
   Today_10min.Enabled:= Value;
   Today_15min.Enabled:= Value;
   Today_30min.Enabled:= Value;
   Today_1h.Enabled:= Value;
   Today_2h.Enabled:= Value;
   Today_3h.Enabled:= Value;
   Today_3PM.Enabled:= Value;
   Today_6PM.Enabled:= Value;
   Tomorrow_8AM.Enabled:= Value;
   Tomorrow_12AM.Enabled:= Value;
   Tomorrow_3PM.Enabled:= Value;
   Tomorrow_6PM.Enabled:= Value;
   CB_Date.Enabled:= Value;
   CB_Time.Enabled:= Value;
   Button_Postpone.Enabled:= Value;
   Button_Discard.Enabled:= Value;
   Button_DiscardAll.Enabled:= Value;
   Button_Show.Enabled:= Value;
   Button_ShowAll.Enabled:= Value;
end;

end.
