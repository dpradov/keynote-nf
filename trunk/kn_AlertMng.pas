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
  kn_NoteObj, kn_NodeList, TB97Ctls, ExtCtrls, TntStdCtrls, Grids, TntGrids,
  TntComCtrls;

type
  TShowMode = (TShowReminders, TShowSet, TShowAll, TShowOverdue, TShowPending);

  TForm_Alarm = class(TForm)
    Label_Selected: TTntLabel;
    Label_Selected_Alarm: TTntLabel;
    Button_Sound: TToolbarButton97;
    Edit_AlarmNote: TTntMemo;
    TV: TTreeNT;
    Panel3: TPanel;
    Label4: TTntLabel;
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
    Tomorrow_3PM: TToolbarButton97;
    Tomorrow_6PM: TToolbarButton97;
    Label1: TTntLabel;
    Label3: TTntLabel;
    Tomorrow_8PM: TToolbarButton97;
    Today_8PM: TToolbarButton97;
    TntLabel1: TTntLabel;
    Combo_Reminder: TTntComboBox;
    rb_Before: TTntRadioButton;
    rb_FromNow: TTntRadioButton;
    Button_DiscardAll: TTntButton;
    Button_Discard: TTntButton;
    Button_Show: TTntButton;
    Button_ShowALL: TTntButton;
    Button_ShowPending: TTntButton;
    CB_Date: TDateTimePicker;
    Button_Apply: TTntButton;
    CB_Time: TTntComboBox;
    Button_ShowOverdue: TTntButton;
    cTime: TTntEdit;
    procedure cTimeExit(Sender: TObject);
    procedure CB_TimeCloseUp(Sender: TObject);
    procedure CB_TimeSelect(Sender: TObject);
    procedure CB_TimeDropDown(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button_ShowOverdueClick(Sender: TObject);
    procedure rb_FromNowClick(Sender: TObject);
    procedure Combo_ReminderChange(Sender: TObject);
    procedure Combo_ReminderExit(Sender: TObject);
    procedure Button_ApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TVChanging(Sender: TObject; Node: TTreeNTNode;
      var AllowChange: Boolean);
    procedure Edit_AlarmNote_Change(Sender: TObject);
    procedure Button_ShowPendingClick(Sender: TObject);
    procedure Button_SoundClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button_ShowALLClick(Sender: TObject);
    procedure Button_ShowClick(Sender: TObject);
    procedure TVDblClick(Sender: TObject);
    procedure Button_DiscardAllClick(Sender: TObject);
    procedure Button_DiscardClick(Sender: TObject);
    procedure TVChange(Sender: TObject; Node: TTreeNTNode);
    procedure TB_NoAlarmClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CB_DateChange(Sender: TObject);

  private
    { Private declarations }
    FModeEdit: TShowMode;
    FAlarmList: TList;          // All alarms
    FSelectedAlarmList: TList;
    FPendingAlarmList: TList;
    FOptionSelected: TObject;
    NodeSelected: TTreeNTNode;
    FNumberAlarms: integer;

    procedure EnableControls (Value: Boolean);
    procedure DisableTimeButtonsInThePast ();
    procedure PressEquivalentTimeButton(alarm: TDateTime; str: wideString);
    procedure ReleaseTimeButtons();
    function nodeOfNote (node: TTreeNTNode): TTreeNTNode;
    procedure TryToApplyChanges(myNode: TNoteNode; newExpiration, newAlarm: TDateTime);
    procedure RemoveNode;
    procedure UpdateIntervalReminder(ExpirationDate, AlarmReminder: TDateTime);
    procedure SetModeEdit (Value: TShowMode);
    procedure UpdateCaption;
    procedure CheckExpirationOverdue();

  public
    { Public declarations }
    ButtonOK: Boolean;
    property ModeEdit: TShowMode read FModeEdit write SetModeEdit;
    property SelectedAlarmList: TList read FSelectedAlarmList write FSelectedAlarmList;
    property AlarmList: TList read FAlarmList write FAlarmList;
  end;


  TAlarmManager = class
  private
    FAlarmList: TList;
    FEnabled: Boolean;
    FSelectedAlarmList: TList;

    FPendingAlarmList: TList;            // Reminders triggered but not managed yet
    FOverdueAlarmList: TList;            // Alarms whose events are overdue
    FForCommunicateAlarmList: TList;     // New alarms addded to pending list
    Timer: TTimer;

    FCanceledAt: TDateTime;

    procedure SetEnabled (Value: boolean);
    procedure ShowFormAlarm (modeEdit: TShowMode);

    procedure CommunicateAlarm (node : TTreeNTNode);
    procedure UpdatePendingAlarmList;
    procedure FlashAlarmMode;
    procedure TimerTimer(Sender: TObject);
    function GetOverdueAlarmList: TList;
  protected

  public
    property Enabled: Boolean read FEnabled write SetEnabled;
    procedure EditAlarm (node: TTreeNTNode);
    function GetAlarmModeHint: string;
    procedure checkAlarms;
    property OverdueAlarmList: TList read GetOverdueAlarmList;  // Alarms whose events are overdue
    procedure AddAlarmNode( node : TTreeNTNode );
    procedure RemoveAlarmNode( node : TTreeNTNode );
    procedure ModifyAlarmNode( node : TTreeNTNode );
    procedure ShowAlarms (const showOnlyPendings: boolean);
    procedure StopFlashMode;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  function FormatAlarmInstant (instant: TDateTime): string;

var
  Form_Alarm: TForm_Alarm;

implementation
uses  DateUtils, ComCtrls95, MMSystem, WideStrUtils,
      gf_misc, kn_Main, kn_Global, kn_TreeNoteMng, kn_Const, kn_LocationObj, kn_FindReplaceMng, kn_Info;

{$R *.DFM}

resourcestring
  STR_NoSelected = '0 alarms selected';
  STR_Apply = '&Apply';
  STR_Postpone = '&Postpone';
  STR_CaptionSet = 'Set Alarm';
  STR_CaptionReminders = '%d Reminders';
  STR_CaptionAll = 'All Alarms/Events (%d)';
  STR_CaptionOverdue = 'Overdue Events (%d)';
  STR_CaptionPending = 'Pending Reminders (%d)';
  STR_Triggered = 'ALARM [%s] :  %s';
  STR_SoundOn = '[Sound ON]';
  STR_SoundOff = '[Sound OFF]';
  STR_PendingAndOverdue = '%d pending reminders, %d overdue ';
  STR_PopupOn = '[Popup ON]';
  STR_PopupOff = '[Popup OFF]';
  STR_ReminderIntervalError = 'Expiration/Start time or Reminder interval are not valid. Please, revise it';
  STR_ConfirmDiscardALL = 'OK to discard ALL this %d nodes?';


constructor TAlarmManager.Create;
begin
   inherited Create;
   FEnabled:= false;
   FAlarmList:= TList.Create;
   FAlarmList.Capacity:= 10;
   FSelectedAlarmList:= TList.Create;
   FPendingAlarmList:= TList.Create;
   FOverdueAlarmList:= TList.Create;
   FForCommunicateAlarmList:= TList.Create;
   Timer:= TTimer.Create(nil);
   Timer.Interval:= 1500;
   Timer.Enabled := false;
   Timer.OnTimer:= TimerTimer;
   FCanceledAt:= 0;

   UpdatePendingAlarmList;
end;

destructor TAlarmManager.Destroy;
begin
   if assigned (FAlarmList) then
      FAlarmList.Free;
   if assigned (FSelectedAlarmList) then
      FSelectedAlarmList.Free;
   if assigned (FPendingAlarmList) then
      FPendingAlarmList.Free;
   if assigned (FOverdueAlarmList) then
      FOverdueAlarmList.Free;
   if assigned (FForCommunicateAlarmList) then
      FForCommunicateAlarmList.Free;
   if assigned (Timer) then
      Timer.Free;
   inherited Destroy;
end;

procedure TAlarmManager.Clear;
begin
   if assigned (FAlarmList) then
      FAlarmList.Clear;
   if assigned (FSelectedAlarmList) then
      FSelectedAlarmList.Clear;
   if assigned (FPendingAlarmList) then
      FPendingAlarmList.Clear;
   if assigned (FOverdueAlarmList) then
      FOverdueAlarmList.Clear;
   if assigned (FForCommunicateAlarmList) then
      FForCommunicateAlarmList.Clear;

   FCanceledAt:= 0;
   UpdatePendingAlarmList;
end;

function compareAlarmNotes (node1, node2: Pointer): integer;
begin
   if TNoteNode(TTreeNTNode(node1).Data).AlarmReminderF = TNoteNode(TTreeNTNode(node2).Data).AlarmReminderF  then
      Result:= 0
   else if TNoteNode(TTreeNTNode(node1).Data).AlarmReminderF > TNoteNode(TTreeNTNode(node2).Data).AlarmReminderF then
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

      ShowFormAlarm (TShowSet);
      UpdatePendingAlarmList;
      Form_Main.TB_AlarmNode.Down:= (TNoteNode(node.Data).AlarmReminderF <> 0);

      FSelectedAlarmList.Clear;
    finally
      Form_Main.Timer.Enabled := true;
    end;
end;

procedure TAlarmManager.ShowFormAlarm (modeEdit: TShowMode);
begin
  if ( Form_Alarm = nil ) then
  begin
    Form_Alarm := TForm_Alarm.Create( Form_Main );
  end;

  try
    Form_Alarm.SelectedAlarmList:= FSelectedAlarmList;
    Form_Alarm.FPendingAlarmList:= FPendingAlarmList;
    Form_Alarm.AlarmList:= FAlarmList;
    Form_Alarm.modeEdit:= modeEdit;

    Form_Alarm.EnableControls(false);
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
end;

procedure TAlarmManager.RemoveAlarmNode( node : TTreeNTNode );
begin
    FAlarmList.Remove(node);
    FPendingAlarmList.Remove(node);
    FOverdueAlarmList.Remove(node);
    FForCommunicateAlarmList.Remove(node);
end;

procedure TAlarmManager.ModifyAlarmNode( node : TTreeNTNode );
begin
    if FEnabled then
       FAlarmList.Sort(compareAlarmNotes);

    UpdatePendingAlarmList;
end;

procedure TAlarmManager.ShowAlarms (const showOnlyPendings: boolean);
var
   modeEdit: TShowMode;
begin
    Form_Main.Timer.Enabled := False;
    try
      if ( Form_Alarm = nil ) then
      begin
        Form_Alarm := TForm_Alarm.Create( Form_Main );
      end;

      FSelectedAlarmList.Clear;
      UpdatePendingAlarmList;
      if showOnlyPendings then begin
         FSelectedAlarmList.Assign(FPendingAlarmList);
         modeEdit:= TShowPending;
         end
      else begin
         FSelectedAlarmList.Assign(FAlarmList);
         modeEdit:= TShowAll;
      end;

      ShowFormAlarm (modeEdit);
      UpdatePendingAlarmList;
      FSelectedAlarmList.Clear;
      if assigned(ActiveNote) and (ActiveNote.Kind=ntTree) and assigned(TTreeNote(ActiveNote).TV.Selected)  then
         Form_Main.TB_AlarmNode.Down:= (TNoteNode(TTreeNote(ActiveNote).TV.Selected.Data).AlarmReminder <> 0);
    finally
      Form_Main.Timer.Enabled := true;
    end;
end;

function FormatAlarmInstant (instant: TDateTime): string;
begin
    if instant = 0 then
       Result:= ''
    else
        if IsToday(instant) then
           Result:= FormatDateTime( 'HH:mm', instant )
        else
           Result:= FormatDateTime( 'dddd, d MMMM yyyy - HH:mm', instant );
end;

procedure TAlarmManager.CommunicateAlarm (node : TTreeNTNode);
var
   myNode: TNoteNode;
   cad: wideString;
begin
   myNode:= TNoteNode(node.Data);
   if myNode.AlarmNoteF <> '' then
      cad:= ' [' + WideStringReplace(myNode.AlarmNoteF, #13#10, ' // ', [rfReplaceAll]) + ']'
   else
      cad:= '';
   Form_Main.StatusBar.Panels[PANEL_HINT].Text := WideFormat(STR_Triggered, [FormatAlarmInstant(myNode.ExpirationDateF), myNode.Name + cad]);
end;

procedure TAlarmManager.TimerTimer(Sender: TObject);
begin
   FlashAlarmMode;
   if (FForCommunicateAlarmList.Count >0 ) and (FForCommunicateAlarmList[0]<> nil) then begin
      CommunicateAlarm(FForCommunicateAlarmList[0]);
      FForCommunicateAlarmList.Delete(0);
   end;

   if FPendingAlarmList.Count=0 then
      Timer.Enabled := false;
end;

procedure TAlarmManager.StopFlashMode;
begin
    Timer.Enabled := false;
    if FPendingAlarmList.Count = 0 then
       Form_Main.TB_AlarmMode.ImageIndex:= 51
    else
       Form_Main.TB_AlarmMode.ImageIndex:= 52;
end;

function TAlarmManager.GetAlarmModeHint: string;
var
  soundState, popupState: string;
begin
   if KeyOptions.PlaySoundOnAlarm then
      soundState:= STR_SoundOn
   else
      soundState:= STR_SoundOff;

   if KeyOptions.DisableAlarmPopup then
      popupState:= STR_PopupOff
   else
      popupState:= STR_PopupOn;

   Result:= Format(STR_PendingAndOverdue, [FPendingAlarmList.Count, OverdueAlarmList.Count]) + popupState + soundState;
end;

procedure TAlarmManager.UpdatePendingAlarmList;
var
  I: Integer;
  node: TNoteNode;
begin
   I:= 0;
   while I <= FPendingAlarmList.Count - 1 do begin
      node:= TTreeNTNode(FPendingAlarmList[i]).Data;
      if (node.AlarmReminderF = 0) or (now() < node.AlarmReminderF) then
         FPendingAlarmList.Delete(i);
      I:= I + 1;
   end;
   Form_Main.TB_AlarmMode.Hint:= GetAlarmModeHint;

   if FPendingAlarmList.Count = 0 then
      Form_Main.TB_AlarmMode.ImageIndex:= 51
   else
      if Form_Main.TB_AlarmMode.ImageIndex = 51 then begin
        // Each triggered alarm (there may be more than one) will be notified for a second and image of button TB_AlarmMode will alternate
        Timer.Enabled := True;
        Form_Main.TB_AlarmMode.ImageIndex:= 52;
     end;
end;

procedure TAlarmManager.FlashAlarmMode;
begin
   if FPendingAlarmList.Count = 0 then exit;
   if Form_Main.TB_AlarmMode.ImageIndex = 51 then
      Form_Main.TB_AlarmMode.ImageIndex:= 52
   else
      Form_Main.TB_AlarmMode.ImageIndex:= 51;
end;

function TAlarmManager.GetOverdueAlarmList: TList;
var
  I: Integer;
  node: TNoteNode;
begin
   I:= 0;
   FOverdueAlarmList.Clear;

   while I <= FAlarmList.Count - 1 do begin
      node:= TTreeNTNode(FAlarmList[i]).Data;
      if now() >= node.ExpirationDateF  then
         FOverdueAlarmList.Add(FAlarmList[i]);
      I:= I + 1;
   end;

   Result:= FOverdueAlarmList;
end;


procedure TAlarmManager.checkAlarms;

  procedure FillSelectedAlarmList;
  var
    I: Integer;
    node: TNoteNode;
    limit: TDateTime;
  begin
     if FCanceledAt <> 0 then
        limit:= incMinute(FCanceledAt, 5);

     I:= 0;
     FSelectedAlarmList.Clear;

     while I <= FAlarmList.Count - 1 do begin
        node:= TTreeNTNode(FAlarmList[i]).Data;
        if now() >= node.AlarmReminderF then begin
           if (FCanceledAt = 0) or (node.AlarmReminderF > FCanceledAt) or (now > limit) or ((now > node.ExpirationDateF) and (DateTimeDiff(now, node.ExpirationDateF)<15)) then begin
              FSelectedAlarmList.Add(FAlarmList[i]);
              if (FPendingAlarmList.IndexOf(FAlarmList[i])<0) then begin
                 FPendingAlarmList.Add(FAlarmList[i]);
                 if KeyOptions.DisableAlarmPopup then
                    FForCommunicateAlarmList.Add(FAlarmList[i]);
              end;
           end;
           I:= I + 1;
        end
        else
           break;
     end;
  end;

  procedure PlaySound;
  var
     soundfn: string;
  begin
     soundfn := extractfilepath( application.exename ) + 'alert.wav';
     if ( KeyOptions.PlaySoundOnAlarm and fileexists( soundfn )) then
         sndplaysound( PChar( soundfn ), SND_FILENAME or SND_ASYNC or SND_NOWAIT );
  end;

begin
   Form_Main.Timer.Enabled := False;
   try
     FillSelectedAlarmList;

     if FSelectedAlarmList.Count > 0 then begin

         if not KeyOptions.DisableAlarmPopup then begin
             PlaySound;

             if IsIconic( Application.Handle ) then begin
                Application.Restore;
                Application.BringToFront;
             end;

             ShowFormAlarm (TShowReminders);
             UpdatePendingAlarmList;

             if assigned(ActiveNote) and (ActiveNote.Kind=ntTree) and assigned(TTreeNote(ActiveNote).TV.Selected)  then
                Form_Main.TB_AlarmNode.Down:= (TNoteNode(TTreeNote(ActiveNote).TV.Selected.Data).AlarmReminder <> 0);
             if Form_Alarm.ButtonOK then
                FCanceledAt:= 0
             else
                FCanceledAt:= now;
         end
         else begin    // KeyOptions.DisableAlarmPopup = True
            FCanceledAt:= now;
            if FForCommunicateAlarmList.Count > 0 then begin      // there has been new triggerd alarms
               PlaySound;
               CommunicateAlarm(FForCommunicateAlarmList[0]);
               FForCommunicateAlarmList.Delete(0);
               Timer.Enabled := True;   // Each new triggered alarm will be notified for a few seconds, and will active flash mode of TB_AlarmMode

               UpdatePendingAlarmList;
            end;
         end;

     end;

   finally
     FSelectedAlarmList.Clear;
     Form_Main.Timer.Enabled := true;
   end;

end;

procedure TForm_Alarm.Button_ShowALLClick(Sender: TObject);
begin
    FSelectedAlarmList:= FAlarmList;
    modeEdit:= TShowAll;
    FormShow(nil);
end;

procedure TForm_Alarm.Button_ShowClick(Sender: TObject);
begin
     TVDblClick(nil);
end;

procedure TForm_Alarm.Button_ShowOverdueClick(Sender: TObject);
begin
    FSelectedAlarmList:= AlarmManager.OverdueAlarmList;
    modeEdit:= TShowOverdue;
    FormShow(nil);
end;

procedure TForm_Alarm.Button_ShowPendingClick(Sender: TObject);
begin
    FSelectedAlarmList:= FPendingAlarmList;
    modeEdit:= TShowPending;
    FormShow(nil);
end;

procedure TForm_Alarm.Button_SoundClick(Sender: TObject);
begin
    KeyOptions.PlaySoundOnAlarm:= Button_Sound.Down;
end;

procedure TForm_Alarm.Button_DiscardAllClick(Sender: TObject);
var
  node: TTreeNTNode;
  nodeAlarm: TTreeNTNode;
begin
    if ( DoMessageBox( WideFormat( STR_ConfirmDiscardALL, [FNumberAlarms] ), mtWarning, [mbYes,mbNo], 0, Handle ) <> mrYes ) then exit;

    ButtonOK:= true;
    NoteFile.Modified := true;
    with TV.Items do
    begin
      BeginUpdate;
      node:= GetFirstNode;
      while assigned(node) do begin
          nodeAlarm:= TTreeNTNode(Node.Data);
          if assigned(nodeAlarm) then begin
             TNoteNode(nodeAlarm.Data).AlarmReminder:= 0;
             TNoteNode(nodeAlarm.Data).ExpirationDate:= 0;
             TNoteNode(nodeAlarm.Data).AlarmNote:= '';
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
      TNoteNode(NodeSelected.Data).AlarmReminder:= 0;
      TNoteNode(NodeSelected.Data).AlarmNote:= '';
      AlarmManager.RemoveAlarmNode (NodeSelected);
      RemoveNode;
   end;
end;


procedure TForm_Alarm.TryToApplyChanges(myNode: TNoteNode; newExpiration, newAlarm: TDateTime);
begin
      if newAlarm = 0 then begin
         DoMessageBox( STR_ReminderIntervalError, mtError, [mbOK], 0 );
         Combo_reminder.SetFocus;
         end

      else begin
          if myNode.AlarmReminderF = 0 then
            AlarmManager.AddAlarmNode(NodeSelected)
          else
            AlarmManager.ModifyAlarmNode(NodeSelected);

          myNode.ExpirationDate:= newExpiration;
          myNode.AlarmReminder:= newAlarm;
          myNode.AlarmNote:= Edit_AlarmNote.Text;
          NoteFile.Modified := true;

          RemoveNode;
      end;
end;

procedure TForm_Alarm.Button_ApplyClick(Sender: TObject);
var
   newAlarm, newExpiration, myTime: TDateTime;
   myNode: TNoteNode;
begin
   if assigned(NodeSelected) then begin
      myNode:= TNoteNode(NodeSelected.Data);
      try
        myTime:= StrToTime(cTime.Text);
        newExpiration:= RecodeTime(CB_Date.DateTime, HourOf(myTime), MinuteOf(myTime), 0, 0);
        if rb_FromNow.Checked then
           newAlarm:= IncStrInterval(Combo_Reminder.Text, Now, true )
        else
           newAlarm:= IncStrInterval(Combo_Reminder.Text, newExpiration, false );

      except
         newAlarm:= 0;
      end;
      TryToApplyChanges(myNode, newExpiration, newAlarm);
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
     UpdateCaption;       // Force update of Caption according state (modeEdit)
  end;
end;

procedure TForm_Alarm.CB_TimeDropDown(Sender: TObject);
var
   t: TDateTime;
   selectedIndex, i: integer;
   strTime: WideString;
begin
    strTime:= TimeRevised(cTime.Text);
    cTime.Text:= strTime;
    try
       t:= StrToTime(strTime);
    except
       t:= StrToTime('00:00');
    end;

    selectedIndex:= 0;
    for i:= CB_Time.Items.Count-1 downto 0 do
         if t >= StrToTime(CB_Time.Items[i]) then begin
            selectedIndex:= i;
            break;
         end;
    CB_Time.ItemIndex:= selectedIndex;
end;

procedure TForm_Alarm.CB_TimeCloseUp(Sender: TObject);
begin
    if CB_Time.Focused  then
       cTime.SetFocus;
    cTime.SelStart:= length(cTime.Text);
    cTime.SelLength:= 0;
end;

procedure TForm_Alarm.CB_TimeSelect(Sender: TObject);
begin
    cTime.Text := CB_Time.Text;
    cTime.SelStart:= length(cTime.Text);
    cTime.SelLength:= 0;
end;


procedure TForm_Alarm.cTimeExit(Sender: TObject);
begin
   cTime.Text:= TimeRevised(cTime.Text);
   CheckExpirationOverdue;
end;

procedure TForm_Alarm.CB_DateChange(Sender: TObject);
begin
    if not rb_Before.Checked then begin
       Combo_Reminder.Text:= '15 ' + STR_minutes;
       rb_Before.Checked := true;
    end;
    CheckExpirationOverdue;
end;

procedure TForm_Alarm.CheckExpirationOverdue();
var
  color: TColor;
  t,newExp: TDateTime;
begin
    try
       t:= StrToTime(cTime.Text);
       newExp:= RecodeTime(CB_Date.DateTime, HourOf(t), MinuteOf(t), 0, 0);
       if newExp < now then
          color:= clYellow
       else
          color:= clWhite;
       CB_Date.Color:= color;
       cTime.Color:= color;
       CB_Time.Color:= color;
    except
    end;
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
   Button_Sound.Down:= KeyOptions.PlaySoundOnAlarm;

   if (FSelectedAlarmList.Count = 0) then begin
       NodeSelected:= nil;
       TV.Items.Clear;
       Label_Selected.Caption := STR_NoSelected;
       Label_Selected_Alarm.Caption := '';
       EnableControls (false);
   end
   else begin
      ButtonOK:= False;
      with TV.Items do
      begin
        BeginUpdate;
        Clear;
        for I := 0 to FNumberAlarms - 1 do
        begin
          node:= TTreeNTNode (FSelectedAlarmList[I]);
          nodeNote:= nodeOfNote(node);
          Child := AddChild(nodeNote, WideFormat('[%s] %s [%s]', [FormatAlarmInstant(TNoteNode(node.Data).AlarmReminderF), TNoteNode(node.Data).Name, WideStringReplace(TNoteNode(node.Data).AlarmNoteF, #13#10, ' // ', [rfReplaceAll])]) );
          Child.Data:= node;
        end;
        TV.FullExpand;
        EndUpdate;

        TV.Selected := GetFirstNode.GetNext;

        if modeEdit <> TShowReminders then begin
           if FNumberAlarms = 1 then
              Edit_AlarmNote.SetFocus
           else
              TV.SetFocus;
        end
        else begin
           TV.SetFocus;
        end;
      end;
   end;

   Label_Selected.Left:= Label_Selected_Alarm.Left + Label_Selected_Alarm.Width + 10;
end;

procedure TForm_Alarm.FormResize(Sender: TObject);
begin
   TV.Width := Width - 30;
   Edit_AlarmNote.Width:= TV.Width;
   Button_Sound.Left := TV.Width + TV.Left - Button_Sound.Width;
end;


procedure TForm_Alarm.TB_NoAlarmClick(Sender: TObject);
var
   minInc: integer;
   Alarm: TDateTime;
   myNode: TNoteNode;
   setFromNow: boolean;
begin
    if Sender = nil then exit;

    if not TToolbarButton97(Sender).Down then begin
       TToolbarButton97(Sender).Down:= true;
       exit;
    end;

    setFromNow:= false;
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

    else begin
        setFromNow:= true;
        if Today_3PM.Down then
           Alarm:= incHour(Today(), 15)
        else if Today_6PM.Down then
           Alarm:= incHour(Today(), 18)
        else if Today_8PM.Down then
           Alarm:= incHour(Today(), 20)
        else if Tomorrow_8AM.Down then
           Alarm:= incHour(Tomorrow(), 8)
        else if Tomorrow_12AM.Down then
           Alarm:= incHour(Tomorrow(), 12)
        else if Tomorrow_3PM.Down then
           Alarm:= incHour(Tomorrow(), 15)
        else if Tomorrow_6PM.Down then
           Alarm:= incHour(Tomorrow(), 18)
        else if Tomorrow_8PM.Down then
           Alarm:= incHour(Tomorrow(), 20);
    end;

    Combo_Reminder.Text:= GetTimeIntervalStr(Now, Alarm);
    if setFromNow then
       rb_FromNow.Checked := true;
end;


procedure TForm_Alarm.TVChange(Sender: TObject; Node: TTreeNTNode);
var
   myNode: TNoteNode;
begin
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
           if myNode.AlarmReminderF <> 0 then begin             // We are MODIFYING an existent ALARM
              Label_Selected_Alarm.Caption := FormatAlarmInstant(myNode.ExpirationDateF) + ' <' + FormatAlarmInstant(myNode.AlarmReminderF) + '> :';
              Edit_AlarmNote.Text:= myNode.AlarmNoteF;
              UpdateIntervalReminder(myNode.ExpirationDate, myNode.AlarmReminder);

              CB_Date.DateTime := myNode.ExpirationDateF;
              cTime.Text := FormatDateTime('hh:nn', myNode.ExpirationDateF);
              end
           else begin                                           // We are creating a NEW ALARM
              Label_Selected_Alarm.Caption := '';
              Edit_AlarmNote.Text:= '';
              CB_Date.DateTime:= now;
              cTime.Text := FormatDateTime('hh:nn', now);
              UpdateIntervalReminder(now, IncMinute(now, 5));
           end;
        end;
    end;
    CheckExpirationOverdue;
    Label_Selected.Left:= Label_Selected_Alarm.Left + Label_Selected_Alarm.Width + 10;
end;


procedure TForm_Alarm.TVChanging(Sender: TObject; Node: TTreeNTNode;
  var AllowChange: Boolean);
var
   myNode: TNoteNode;
begin
   if assigned(TV.Selected) and assigned(NodeSelected) then begin
      myNode:= TNoteNode(NodeSelected.Data);
      if myNode.AlarmReminderF = 0 then
         node.Text:= WideFormat('[%s] %s', [FormatAlarmInstant(myNode.ExpirationDateF), myNode.Name]);
   end;
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

procedure TForm_Alarm.Edit_AlarmNote_Change(Sender: TObject);
var
   myNode: TNoteNode;
   node: TTreeNTNode;
begin
   node:= TV.Selected;
   if assigned(node) and assigned(NodeSelected) then begin
      myNode:= TNoteNode(NodeSelected.Data);
      node.Text:= WideFormat('[%s] %s [%s]', [FormatAlarmInstant(myNode.ExpirationDateF), myNode.Name, WideStringReplace(Edit_AlarmNote.Text, #13#10, '··', [rfReplaceAll])]);
      if myNode.AlarmReminderF <> 0 then begin
         myNode.AlarmNote:= Edit_AlarmNote.Text;
         NoteFile.Modified := true;
      end;
   end;
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
   Today_8PM.Enabled:= Value;
   Tomorrow_8AM.Enabled:= Value;
   Tomorrow_12AM.Enabled:= Value;
   Tomorrow_3PM.Enabled:= Value;
   Tomorrow_6PM.Enabled:= Value;
   Tomorrow_8PM.Enabled:= Value;

   CB_Date.Enabled:= Value;
   cTime.Enabled:= Value;
   CB_Time.Enabled:= Value;
   Combo_Reminder.Enabled:= Value;
   rb_FromNow.Enabled:= Value;
   rb_Before.Enabled:= Value;
   Button_Discard.Enabled:= Value;
   Button_Apply.Enabled:= Value;
   Button_Show.Enabled:= Value;
   Edit_AlarmNote.Visible:= Value;

   if Value then
      DisableTimeButtonsInThePast
   else begin
      ReleaseTimeButtons;
      CB_Date.Color:= clWhite;
      CB_Date.DateTime:= now;
      cTime.Color:= clWhite;
      cTime.Text := '';
      Combo_Reminder.Text := '';
   end;

end;

procedure TForm_Alarm.FormCreate(Sender: TObject);
var
   i: integer;
begin
  with Combo_Reminder.Items do begin
      Add('0 ' + STR_minutes);
      Add('5 ' + STR_minutes);
      Add('10 ' + STR_minutes);
      Add('15 ' + STR_minutes);
      Add('30 ' + STR_minutes);

      Add('1 ' + STR_hour);
      for i := 2 to 10 do
          Add(intTostr(i) + ' ' + STR_hours);
      Add('0.5 ' + STR_days);
      Add('18 ' + STR_hours);
      Add('1 ' + STR_day);
      for i := 2 to 4 do
          Add(intTostr(i) + ' ' + STR_days);
      Add('1 ' + STR_week);
      Add('2 ' + STR_weeks);
  end;
  Combo_Reminder.ItemIndex := 0;

  with CB_Time.Items do begin
      for i := 0 to 23 do begin
          Add(Format('%.2d:00', [i]));
          Add(Format('%.2d:30', [i]));
      end;
  end;
  CB_Time.Text := '';
end;

procedure TForm_Alarm.SetModeEdit (Value: TShowMode);
var
  str: wideString;
  n: integer;
begin
    FModeEdit:= Value;
    FNumberAlarms:= FSelectedAlarmList.Count;
    UpdateCaption;
end;

procedure TForm_Alarm.UpdateCaption;
var
  str: wideString;
  n: integer;
begin
    case FModeEdit of
      TShowReminders: str:= STR_CaptionReminders;
      TShowSet:       str:= STR_CaptionSet;
      TShowAll:       str:= STR_CaptionAll;
      TShowOverdue:   str:= STR_CaptionOverdue;
      TShowPending:   str:= STR_CaptionPending;
    end;
    Caption:= WideFormat(str, [FNumberAlarms]);
end;


procedure TForm_Alarm.UpdateIntervalReminder(ExpirationDate, AlarmReminder: TDateTime);
var
    showAsBefore: boolean;
begin
     showAsBefore:= true;
     if (AlarmReminder <= Now) or (Abs(AlarmReminder - now) <= Abs(AlarmReminder - ExpirationDate)) then
        showAsBefore:= false;

     if showAsBefore then begin
        Combo_Reminder.Text:= GetTimeIntervalStr(ExpirationDate, AlarmReminder);
        rb_Before.Checked := true;
        end
     else begin
        if AlarmReminder > Now then
           Combo_Reminder.Text:= GetTimeIntervalStr(Now, AlarmReminder)
        else
           Combo_Reminder.Text:= '5 ' + STR_minutes;

        rb_FromNow.Checked := true;
     end;
     Combo_ReminderChange(nil);
end;

procedure TForm_Alarm.Combo_ReminderChange(Sender: TObject);
var
   myNode: TNoteNode;
   alarm: TDateTime;
   str: wideString;
begin
     alarm:= IncStrInterval(Combo_Reminder.Text, Now);
     if alarm <> 0 then begin
        str:= GetTimeIntervalStr(Now, alarm);
        PressEquivalentTimeButton(alarm, str);
     end;
end;

procedure TForm_Alarm.Combo_ReminderExit(Sender: TObject);
var
   str: wideString;
   alarm: TDateTime;
begin
    alarm:= IncStrInterval(Combo_Reminder.Text, Now);
    if alarm <> 0 then
       Combo_Reminder.Text:= GetTimeIntervalStr(Now, alarm);  // Loads the combo with the clean value, interpreted. Example: "2m" --> "2 minutes"
end;

procedure TForm_Alarm.ReleaseTimeButtons();
begin
   if FOptionSelected <> nil then begin
      TToolbarButton97(FOptionSelected).Down:= false;
      FOptionSelected:= nil;
   end;
end;

// Disable time buttons that imply a notify in the past
procedure TForm_Alarm.DisableTimeButtonsInThePast ();

   procedure DisableButton(button: TObject);
   begin
      TToolbarButton97(button).Enabled:= false;
      if button = FOptionSelected then begin
         TToolbarButton97(FOptionSelected).Down:= false;
         FOptionSelected:= nil;
      end;
   end;

begin
   if incHour(Today(), 20) <= now then begin
      DisableButton(Today_8PM);
      DisableButton(Today_6PM);
      DisableButton(Today_3PM);
      end
   else if incHour(Today(), 18) <= now then begin
      DisableButton(Today_6PM);
      DisableButton(Today_3PM);
      end
   else if incHour(Today(), 15) <= now then
      DisableButton(Today_3PM);
end;


procedure TForm_Alarm.PressEquivalentTimeButton(alarm: TDateTime; str: wideString);
var
  difFromNow: int64;
begin
   ReleaseTimeButtons;

   difFromNow:= DateTimeDiff(now, alarm);

   if (str= '5 ' + STR_minutes) or (difFromNow >= 4*60) and (difFromNow <= 6*60) then FOptionSelected:= Today_5min
   else if (str= '10 ' + STR_minutes) or (difFromNow >= 9*60) and (difFromNow <= 11*60) then FOptionSelected:= Today_10min
   else if (str= '15 ' + STR_minutes) or (difFromNow >= 13.5*60) and (difFromNow <= 16.5*60)then FOptionSelected:= Today_15min
   else if (str= '30 ' + STR_minutes) or (difFromNow >= 25.5*60) and (difFromNow <= 33.5*60) then FOptionSelected:= Today_30min
   else if (str= '1 ' + STR_hour)  or (difFromNow >= 54.5*60) and (difFromNow <= 65.5*60) then FOptionSelected:= Today_1h
   else if (str= '2 ' + STR_hours) or (difFromNow >= 114.5*60) and (difFromNow <= 126.5*60) then FOptionSelected:= Today_2h
   else if (str= '3 ' + STR_hours) or (difFromNow >= 170.5*60) and (difFromNow <= 190.5*60) then FOptionSelected:= Today_3h
   else if DateTimeDiff(incHour(Today(), 15), alarm) < 60*15 then FOptionSelected:= Today_3PM
   else if DateTimeDiff(incHour(Today(), 18), alarm) < 60*15 then FOptionSelected:= Today_6PM
   else if DateTimeDiff(incHour(Today(), 20), alarm) < 60*15 then FOptionSelected:= Today_8PM
   else if DateTimeDiff(incHour(Tomorrow(), 8), alarm) < 60*20 then FOptionSelected:= Tomorrow_8AM
   else if DateTimeDiff(incHour(Tomorrow(), 12), alarm) < 60*20 then FOptionSelected:= Tomorrow_12AM
   else if DateTimeDiff(incHour(Tomorrow(), 15), alarm) < 60*20 then FOptionSelected:= Tomorrow_3PM
   else if DateTimeDiff(incHour(Tomorrow(), 18), alarm) < 60*20 then FOptionSelected:= Tomorrow_6PM
   else if DateTimeDiff(incHour(Tomorrow(), 20), alarm) < 60*20 then FOptionSelected:= Tomorrow_8PM;

   if FOptionSelected <> nil then begin
      TToolbarButton97(FOptionSelected).Down:= true;
   end;
end;

procedure TForm_Alarm.rb_FromNowClick(Sender: TObject);
begin
    ReleaseTimeButtons;
    Combo_ReminderChange(nil);
end;


end.
