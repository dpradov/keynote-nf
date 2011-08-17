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
  TB97Ctls, ExtCtrls, TntStdCtrls, Grids, TntGrids,
  TntComCtrls, ColorPicker,
  kn_NoteObj, kn_NodeList, kn_LocationObj;

type
  TShowMode = (TShowReminders, TShowSet, TShowAll, TShowOverdue, TShowPending, TShowDiscarded, TShowNew, TShowAllWithDiscarded);
  TFilterDate = (TSelectAllDates, TSelectDays, TSelectWeek, TSelectMonth);
  TSortedColumn = (TColumnExpiration, TColumnNodeName, TColumnNoteName, TColumnAlarmNote, TColumnReminder, TColumnDiscarded);
  TDaysArray = array of LongWord;

  TAlarmStatus = (TAlarmUnsaved, TAlarmNormal, TAlarmDiscarded, TAlarmPendingKnown, TAlarmPendingUnknown);

  TAlarm = class
  private
    function GetOverdue: boolean;
    function GetPending: boolean;

  public
    AlarmReminder: TDateTime;     // Reminder instant
    ExpirationDate: TDateTime;    // Expiration/start instant
    AlarmNote: WideString;
    Node: TNoteNode;
    Note: TTabNote;
    Bold: boolean;
    FontColor: TColor;
    BackColor: TColor;

    Status: TAlarmStatus;

    property Pending: boolean read GetPending;
    property Overdue: boolean read GetOverdue;    // Alarm whose event is overdue

    constructor Create;
  end;

  
  //---------------------------------------------------------------------
  
  TAlarmManager = class
  private
    FAlarmList: TList;

    FDiscardedAlarmList: TList;          // Alarms discarded. Can be removed or restored
    FSelectedAlarmList: TList;           // Selection of alarms to show (from reminders triggered, or alarms related to node or note, if editing or adding)
    FUnfilteredAlarmList: TList;

    Timer: TTimer;
    FTicks: integer;

    FCanceledAt: TDateTime;

    FNumberPending, FNumberOverdue: Integer;

    procedure ShowFormAlarm (modeEdit: TShowMode);

    procedure CommunicateAlarm (alarm: TAlarm);
    procedure UpdateAlarmsState;
    procedure FlashAlarmState();
    procedure TimerTimer(Sender: TObject);
    procedure MoveAlarmsInList( List: TList; noteFrom: TTabNote; nodeFrom : TNoteNode; noteTo: TTabNote; nodeTo: TNoteNode );

  protected

  public
    procedure EditAlarms (node: TNoteNode; note: TTabNote; forceAdd: boolean= false);
    function GetAlarmModeHint: string;
    procedure CheckAlarms;

    property AlarmList: TList read FAlarmList;
    property DiscardedAlarmList: TList read FDiscardedAlarmList;
    property SelectedAlarmList: TList read FSelectedAlarmList;
    property UnfilteredAlarmList: TList read FUnfilteredAlarmList write FUnfilteredAlarmList;

    function HasAlarms( note: TTabNote; node : TNoteNode; considerDiscarded: boolean ): boolean;
    function GetAlarms( note: TTabNote; node : TNoteNode; considerDiscarded: boolean ): TList;
    
    property NumberPendingAlarms: integer read FNumberPending;
    property NumberOverdueAlarms: integer read FNumberOverdue;
    
    function GetNextPendingAlarmForCommunicate: TAlarm;
    
    procedure AddAlarm( alarm : TAlarm );
    procedure MoveAlarms( noteFrom: TTabNote; nodeFrom : TNoteNode; noteTo: TTabNote; nodeTo: TNoteNode );
    procedure RemoveAlarmsOfNode( node : TNoteNode );
    procedure RemoveAlarmsOfNote( note : TTabNote );
    procedure RemoveAlarm( alarm : TAlarm );
    procedure DiscardAlarm( alarm : TAlarm; MaintainInUnfilteredList: boolean );
    procedure RestoreAlarm( alarm : TAlarm; MaintainInUnfilteredList: boolean  );
    procedure ModifyAlarm( alarm: TAlarm );

    procedure UpdateFormMain (alarm: TAlarm);
    procedure ShowAlarms (const showOnlyPendings: boolean);
    procedure StopFlashMode;
    procedure CancelReminders(value: Boolean);

    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;


  //---------------------------------------------------------------------

  TForm_Alarm = class(TForm)
    Panel3: TPanel;
    Bevel1: TBevel;

    Grid: TTntListView;

    Button_New: TTntButton;
    Button_Discard: TTntButton;
    Button_Apply: TTntButton;
    Button_SelectAll: TTntButton;
    Button_Remove: TTntButton;
    Button_Show: TTntButton;
    Button_Restore: TTntButton;
    Button_Sound: TToolbarButton97;

    PanelCalendar: TPanel;
    cCalendar: TTntMonthCalendar;
    cIdentifier: TTntEdit;

    lblSubject: TTntLabel;
    txtSubject: TTntMemo;
    TB_Color: TColorBtn;
    TB_Hilite: TColorBtn;
    TB_Bold: TToolbarButton97;

    CB_FilterDates: TTntComboBox;
    cFilter: TTntEdit;
    lblFilter: TTntLabel;
    Button_ClearFilter: TToolbarButton97;

    TntLabel2: TTntLabel;
    CB_ShowMode: TTntComboBox;
    TB_ClipCap: TToolbarButton97;

    lblExpiration: TTntLabel;
    CB_ExpirationDate: TDateTimePicker;
    CB_ExpirationTime: TTntComboBox;
    cExpirationTime: TTntEdit;
    chk_Expiration: TCheckBox;

    PanelProposedReminder: TPanel;
    CB_ProposedIntervalReminder: TTntComboBox;
    rb_Before: TTntRadioButton;
    rb_FromNow: TTntRadioButton;
    Today_5min: TToolbarButton97;
    Today_10min: TToolbarButton97;
    Today_15min: TToolbarButton97;
    Today_30min: TToolbarButton97;
    Today_1h: TToolbarButton97;
    Today_2h: TToolbarButton97;
    Today_3h: TToolbarButton97;
    Today_5h: TToolbarButton97;
    Today_12AM: TToolbarButton97;
    Today_8AM: TToolbarButton97;
    Today_3PM: TToolbarButton97;
    Today_6PM: TToolbarButton97;
    Today_8PM: TToolbarButton97;
    Tomorrow_8AM: TToolbarButton97;
    Tomorrow_12AM: TToolbarButton97;
    Tomorrow_3PM: TToolbarButton97;
    Tomorrow_6PM: TToolbarButton97;
    Tomorrow_8PM: TToolbarButton97;

    Label1: TTntLabel;
    Label3: TTntLabel;
    cReminder: TEdit;
    lblExpirationStatus: TTntLabel;
    lblReminder: TTntLabel;
    TntLabel3: TTntLabel;
    lblProposedReminder: TTntLabel;
    lblReminderStatus: TTntLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure Today_5minClick(Sender: TObject);
    procedure Today_5minDblClick(Sender: TObject);

    procedure Button_NewClick(Sender: TObject);
    procedure Button_SelectAllClick(Sender: TObject);
    procedure Button_RemoveClick(Sender: TObject);
    procedure Button_ApplyClick(Sender: TObject);
    procedure Button_SoundClick(Sender: TObject);
    procedure Button_ShowClick(Sender: TObject);
    procedure Button_DiscardClick(Sender: TObject);
    procedure Button_RestoreClick(Sender: TObject);

    procedure CB_ShowModeChange(Sender: TObject);

    procedure TB_ClipCapClick(Sender: TObject);
    procedure TB_HiliteClick(Sender: TObject);
    procedure TB_ColorClick(Sender: TObject);
    procedure TB_BoldClick(Sender: TObject);
    procedure txtSubjectChange(Sender: TObject);

    procedure Button_ClearFilterClick(Sender: TObject);
    procedure cFilterChange(Sender: TObject);
    procedure cFilterExit(Sender: TObject);

    procedure CB_FilterDatesChange(Sender: TObject);
    procedure cCalendarClick(Sender: TObject);
    procedure cCalendarExit(Sender: TObject);
    procedure cCalendarGetMonthInfo(Sender: TObject; Month: Cardinal; var MonthBoldInfo: Cardinal);

    procedure GridEnter(Sender: TObject);
    procedure GridColumnClick(Sender: TObject; Column: TListColumn);
    procedure GridDblClick(Sender: TObject);
    procedure GridSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure GridAdvancedCustomDrawSubItem(Sender: TCustomListView;
                Item: TListItem; SubItem: Integer; State: TCustomDrawState;
                Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure GridAdvancedCustomDrawItem(Sender: TCustomListView;
                Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
                var DefaultDraw: Boolean);

    procedure chk_ExpirationClick(Sender: TObject);
    procedure CB_ExpirationDateChange(Sender: TObject);
    procedure CB_ExpirationTimeCloseUp(Sender: TObject);
    procedure CB_ExpirationTimeSelect(Sender: TObject);
    procedure CB_ExpirationTimeDropDown(Sender: TObject);
    procedure rb_FromNowClick(Sender: TObject);

    procedure cExpirationTimeExit(Sender: TObject);

    procedure CB_ProposedIntervalReminderChange(Sender: TObject);
    procedure CB_ProposedIntervalReminderExit(Sender: TObject);

  private
    { Private declarations }
    FModeEdit: TShowMode;
    FFilteredAlarmList: TList;
    FOptionSelected: TObject;
    FNumberAlarms: integer;

    FNewExpirationDate: TDateTime;
    FReminder: TDateTime;
    FProposedReminder: TDateTime;
    FExpirationDateModified: Boolean;
    FSubjectModified: Boolean;
    FBoldModified: Boolean;
    FFontColorModified, FBackColorModified: Boolean;
    FIntervalDirectlyChanged: Boolean;

    FCalendarMonthChanged: Boolean;
    FInitializingControls: Integer;

    procedure CreateParams(var Params: TCreateParams); override;

    procedure EnableControls (Value: Boolean);

    procedure ConfirmApplyPendingChanges();
    procedure ResetModifiedState;

    procedure CleanProposedReminderPanel();
    procedure UpdateProposedReminderDate (KeepInterval: boolean = false);
    procedure SetProposedReminderDate(ReferenceDate: TDateTime; IntervalStr: WideString; BeforeReference: Boolean; KeepInterval: boolean = false);
    procedure ShowReminderDate(Instant: TDateTime);
    procedure ShowReminderStatus(Instant: TDateTime);
    procedure ShowProposedReminderDate(ReminderDate: TDateTime; IntervalStr: WideString; ReferenceDate: TDateTime; KeepInterval: Boolean = false);
    procedure ReleaseTimeButtons();
    procedure DisableTimeButtonsInThePast ();
    procedure PressEquivalentTimeButton(ReminderDate: TDateTime; ReferenceDate: TDateTime);

    procedure SetModeEdit (Value: TShowMode);
    procedure UpdateCaption;
    procedure UpdateAlarmOnGrid(alarm: TAlarm; item: TTntListItem);

    procedure ShowExpirationDate(Instant: TDateTime);
    procedure ShowExpirationStatus(Instant: TDateTime);
    function  GetSelectedExpirationDate: TDateTime;
    procedure SetNewExpirationDate(Instant: TDateTime);

    function  AlarmSelected: TAlarm;
    procedure TrySelectItem(i: integer);
    procedure HideAlarm(ls: TListItem);

    procedure ShowCommonProperties();

    procedure CopyAlarmList();
    function UnfilteredAlarmList: TList;

    function CreateLocation(alarm: TAlarm): TLocation;

  public
    property ModeEdit: TShowMode read FModeEdit write SetModeEdit;
    property NewExpirationDate: TDateTime read FNewExpirationDate write SetNewExpirationDate;
    function ChangesToApply: Boolean;

    procedure FilterAlarmList;
  end;


  function FormatAlarmInstant (instant: TDateTime): string;

var
  Form_Alarm: TForm_Alarm;
  SortedColumn: TSortedColumn;
  AscOrder: integer;  // 1: ASC  -1: DESC

implementation
uses  DateUtils, ComCtrls95, MMSystem, WideStrUtils,
      gf_misc, kn_Main, kn_Global, kn_TreeNoteMng, kn_Const, kn_FindReplaceMng, kn_Info, kn_clipUtils,
      kn_RTFUtils, kn_LinksMng, kn_VCLControlsMng;

{$R *.DFM}

resourcestring
  STR_NumberSelected = '%d alarms selected';
  STR_Apply = '&Apply';
  STR_CaptionSet = 'Set Alarm';
  STR_CaptionReminders = '%d Reminders';
  STR_CaptionAll = 'All Alarms/Events (%d)';
  STR_CaptionOverdue = 'Overdue Events (%d)';
  STR_CaptionPending = 'Pending Reminders (%d)';
  STR_CaptionDiscarded = 'Discarded Events (%d)';
  STR_Triggered = 'ALARM [%s] :  %s';
  STR_SoundOn = '[Sound ON]';
  STR_SoundOff = '[Sound OFF]';
  STR_PendingAndOverdue = '%d pending reminders, %d overdue ';
  STR_PopupOn = '[Popup ON]';
  STR_PopupOff = '[Popup OFF]';
  STR_ReminderorDateINC = 'Expiration/Start time and/or Reminder interval are not valid.'+ #13+ 'Please, correct it';
  STR_ConfirmDiscardALL = 'OK to discard all this %d alarms?';
  STR_ConfirmRemoveALL = 'OK to remove all this %d alarms?';
  STR_ConfirmRestoreALL = 'OK to restore all this %d alarms?';
  STR_ConfirmRemove = 'OK to remove this alarm?';
  STR_ConfirmApplyPendingChanges = 'OK to apply pending changes?';
  STR_Today = 'Today';
  STR_Tomorrow = 'Tomorrow';
  STR_All = 'All';
  STR_Overdue = 'Overdue';
  STR_Pending = 'Pending';
  STR_Discarded = 'Discarded';
  STR_AllWithDiscarded = 'All (with discarded)';
  STR_Hint_Pending = 'Show all pending reminders (triggered and ignored, not postponed nor discarded)';
  STR_Hint_Overdue = 'Show all overdue events';
  STR_Hint_All = 'Show all set alarms (not discarded)';
  STR_Hint_All_With_Discarded = 'Show all set alarms, including discarded';
  STR_SelectAllDates = 'Show All Dates';
  STR_SelectDays = 'Filter on selected Days';
  STR_SelectWeek = 'Filter on selected Week';
  STR_SelectMonth = 'Filter on selected Month';
  STR_FilterApplied = '(Filter applied)';
  STR_IntervalOverdue = '(%s overdue)';
  STR_IntervalLeft = '(%s left)';
  STR_IntervalBefore = '(%s before)';

  //const COLUMN_NoteName = 0; // item
  const COLUMN_NodeName = 0;        // subitems
  const COLUMN_ExpirationDate = 1;
  const COLUMN_ExpirationTime = 2;
  const COLUMN_AlarmNote = 3;
  const COLUMN_ReminderDate = 4;
  const COLUMN_ReminderTime = 5;
  const COLUMN_Discarded = 6;

  const WAIT_TIME_ON_ESCAPE = 5;
  const IMG_INDEX_NO_OVERDUE_NOR_PENDING = 51;
  const IMG_INDEX_OVERDUE_OR_PENDING = 52;

  

//==========================================================================================
//                                         TAlarm  
//==========================================================================================

constructor TAlarm.Create;
begin
   inherited Create;
   Status := TAlarmNormal;
   AlarmReminder:= 0;
   ExpirationDate:= 0;
   AlarmNote:= '';
   node:= nil;
   note:= nil;
   Bold:= False;
   FontColor:= clWindowText;
   BackColor:= clWindow;
end;

function TAlarm.GetOverdue: boolean;
begin
   Result:= (ExpirationDate <>0) and ( now() >= ExpirationDate);
end;

function TAlarm.GetPending: boolean;
begin
   Result:= (Status = TAlarmPendingKnown) or (Status = TAlarmPendingUnknown);
end;


//==========================================================================================
//                                   TAlarmManager
//==========================================================================================

//----------------------------------
//          Creation / Destruction
//----------------------------------

constructor TAlarmManager.Create;
begin
   inherited Create;
   //FEnabled:= false;
   FAlarmList:= TList.Create;
   FAlarmList.Capacity:= 10;
   FSelectedAlarmList:= TList.Create;
   FDiscardedAlarmList:= TList.Create;
   FUnfilteredAlarmList:= nil;
   Timer:= TTimer.Create(nil);
   Timer.Interval:= 350;
   Timer.Enabled := false;
   Timer.OnTimer:= TimerTimer;
   FCanceledAt:= 0;

   FNumberPending:= 0;
   FNumberOverdue:= 0;

   UpdateAlarmsState;
end;

destructor TAlarmManager.Destroy;
begin
   if assigned (FAlarmList) then
      FAlarmList.Free;
   if assigned (FSelectedAlarmList) then
      FSelectedAlarmList.Free;
   if assigned (FDiscardedAlarmList) then
      FDiscardedAlarmList.Free;
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
   if assigned(FDiscardedAlarmList) then
      FDiscardedAlarmList.Clear;

   FCanceledAt:= 0;
   UpdateAlarmsState;
end;


//----------------------------------
//       Node / Note Information
//----------------------------------

// Only one of the first two parameters (note, node) is needed, the other can be set to nil
function TAlarmManager.HasAlarms( note: TTabNote;  node : TNoteNode; considerDiscarded: boolean ): boolean;

    function HasAlarmsInList (list: TList): boolean;
    var
       i: integer;
       alarm: TAlarm;
    begin
       Result:= false;
       i:= 0;
       while I <= list.Count - 1 do begin
           alarm:= TAlarm(list[i]);
           if assigned(node) then begin
                if node = alarm.node then begin
                   Result:= true;
                   break;
                end;
           end
           else if (alarm.node = nil) and (note = alarm.note) then begin
                   Result:= true;
                   break;
                end;
           I:= I + 1;
       end;
    end;

begin
   if assigned(node) then
      node:= node.NonVirtualNode;

   Result:= HasAlarmsInList(FAlarmList);
   if (not Result) and considerDiscarded then
       Result:= HasAlarmsInList(FDiscardedAlarmList);
end;


// Only one of the first two parameters (note, node) is needed, the other can be set to nil
function TAlarmManager.GetAlarms( note: TTabNote; node : TNoteNode; considerDiscarded: boolean ): TList;

    procedure AddAlarmsFromList (list: TList);
    var
       i: Integer;
       alarm: TAlarm;
    begin
       i:= 0;
       while i <= list.Count - 1 do begin
          alarm:= TAlarm(list[i]);
          if assigned(node) then begin
             if node = alarm.node then
                Result.Add(alarm)
          end
             else if (alarm.node = nil) and (note = alarm.note) then
                Result.Add(alarm);

          i:= i + 1;
       end;
    end;

begin
   Result:= TList.Create;

   if assigned(node) then
      node:= node.NonVirtualNode;

   AddAlarmsFromList (FAlarmList);
   if considerDiscarded then
      AddAlarmsFromList (FDiscardedAlarmList);
end;


//----------------------------------
//       Auxiliar methods
//----------------------------------

function FormatDate (instant: TDateTime): string;
begin
    if instant = 0 then
       Result:= ''
    else
        if IsToday(instant) then
           Result:= STR_Today
        else if IsToday(IncDay(instant,-1)) then
           Result:= STR_Tomorrow
        else
           Result:= FormatDateTime( 'dd/MMM/yyyy', instant );
end;

function FormatAlarmInstant (instant: TDateTime): string;
begin
    if instant = 0 then
       Result:= ''
    else
        if IsToday(instant) then
           Result:= STR_Today + ' ' + FormatDateTime( 'HH:mm', instant )
        else if IsToday(IncDay(instant,-1)) then
           Result:= STR_Tomorrow + ' ' + FormatDateTime( 'HH:mm', instant )
        else
           Result:= FormatDateTime( 'dd MMM yyyy - HH:mm', instant );
end;

function compareAlarms_Reminder (node1, node2: Pointer): integer;
begin
   if TAlarm(node1).AlarmReminder = TAlarm(node2).AlarmReminder  then
      Result:= 0
   else if TAlarm(node1).AlarmReminder > TAlarm(node2).AlarmReminder then
      Result:= 1
   else
      Result:= -1;
end;

function compareAlarms_selectedColumn (node1, node2: Pointer): integer;
var
   nodeName1, nodeName2: WideString;
   discarded1, discarded2: Boolean;
begin
    case SortedColumn of
        TColumnExpiration:
               if TAlarm(node1).ExpirationDate = TAlarm(node2).ExpirationDate  then
                  Result:= compareAlarms_Reminder (node1, node2)
               else if TAlarm(node1).ExpirationDate > TAlarm(node2).ExpirationDate then
                  Result:= 1
               else
                  Result:= -1;

        TColumnNoteName, TColumnNodeName:
               begin
                   if assigned (TAlarm(node1).node) then nodeName1:= TAlarm(node1).node.Name else nodeName1:= '';
                   if assigned (TAlarm(node2).node) then nodeName2:= TAlarm(node2).node.Name else nodeName2:= '';
                   nodeName1:= TAlarm(node1).note.Name + nodeName1;
                   nodeName2:= TAlarm(node2).note.Name + nodeName2;

                   if nodeName1  = nodeName2 then
                      Result:= compareAlarms_Reminder (node1, node2)
                   else if nodeName1 > nodeName2 then
                      Result:= 1
                   else
                      Result:= -1;
               end;

        TColumnAlarmNote:
               if TAlarm(node1).AlarmNote  = TAlarm(node2).AlarmNote then
                  Result:= compareAlarms_Reminder (node1, node2)
               else if TAlarm(node1).AlarmNote > TAlarm(node2).AlarmNote then
                  Result:= 1
               else
                  Result:= -1;

        TColumnReminder:
               if TAlarm(node1).AlarmReminder  = TAlarm(node2).AlarmReminder then
                  Result:= 0
               else if TAlarm(node1).AlarmReminder > TAlarm(node2).AlarmReminder then
                  Result:= 1
               else
                  Result:= -1;

        TColumnDiscarded:
             begin
               discarded1:= (TAlarm(node1).Status = TAlarmDiscarded);
               discarded2:= (TAlarm(node2).Status = TAlarmDiscarded);
               if discarded1 = discarded2 then
                  Result:= 0
               else if discarded1 then
                  Result:= 1
               else
                  Result:= -1;
             end;
    end;
    Result:= Result * AscOrder;

end;


//----------------------------------
//       Alarms Check
//----------------------------------


procedure TAlarmManager.CheckAlarms;
var
  ShowRemindersInModalWindow: boolean;
  TriggeredAlarmList: TList;
  alarm: TAlarm;

  procedure FillTriggeredAlarmList;
  var
    I: Integer;
    alarm: TAlarm;
    limit: TDateTime;
  begin
     if FCanceledAt <> 0 then
        limit:= incMinute(FCanceledAt, WAIT_TIME_ON_ESCAPE);

     I:= 0;
     while I <= FAlarmList.Count - 1 do begin
        alarm:= TAlarm(FAlarmList[I]);
        if ( (now >= alarm.AlarmReminder) and ((FCanceledAt = 0) or (now > limit) or (alarm.AlarmReminder > FCanceledAt)) ) or
           (alarm.Overdue and (alarm.ExpirationDate > FCanceledAt) and (DateTimeDiff(now, alarm.ExpirationDate)<5*60))
        then begin
            TriggeredAlarmList.Add(alarm);
            if not alarm.Pending then begin
               if not ShowRemindersInModalWindow then
                  alarm.Status:= TAlarmPendingUnknown
               else
                  alarm.Status:= TAlarmPendingKnown;
            end;
        end;
        
        I:= I + 1;
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
   TriggeredAlarmList:= TList.Create;
   ShowRemindersInModalWindow:= not (KeyOptions.DisableAlarmPopup or (assigned(Form_Alarm) and Form_Alarm.Visible ));

   try
     FillTriggeredAlarmList;

     if TriggeredAlarmList.Count > 0 then begin

         if ShowRemindersInModalWindow then begin
             PlaySound;

             if IsIconic( Application.Handle ) then begin
                Application.Restore;
                Application.BringToFront;
             end;

             FSelectedAlarmList.Assign(TriggeredAlarmList);
             ShowFormAlarm (TShowReminders);
         end
         else begin    // ShowRemindersInModalWindow = False
            alarm:= GetNextPendingAlarmForCommunicate;
            if assigned(alarm) then begin      // there has been new triggerd alarms
               PlaySound;
               CommunicateAlarm(alarm);
               alarm.Status := TAlarmPendingKnown;
               Timer.Enabled := True;   // Each new triggered alarm will be notified for a few seconds, and will active flash mode of TB_AlarmMode
               FTicks:= 0;
               UpdateAlarmsState;
            end;
         end;

     end;

   finally
     TriggeredAlarmList.Clear;
   end;

end;


procedure TAlarmManager.UpdateAlarmsState;
var
  I: Integer;
  alarm: TAlarm;
begin
   I:= 0;
   FNumberOverdue:= 0;
   FNumberPending:= 0;

   while I <= FAlarmList.Count - 1 do begin
      alarm:= TAlarm(FAlarmList[i]);
      if alarm.Overdue then
         FNumberOverdue:= FNumberOverdue + 1;

      if alarm.Pending then
         if (alarm.AlarmReminder = 0) or (now() < alarm.AlarmReminder) then
             alarm.Status := TAlarmNormal
         else
             FNumberPending:= FNumberPending + 1;
      I:= I + 1;
   end;


   Form_Main.TB_AlarmMode.Hint:= GetAlarmModeHint;

   if (FNumberPending = 0) and (FNumberOverdue = 0) then begin
      Form_Main.TB_AlarmMode.ImageIndex:= IMG_INDEX_NO_OVERDUE_NOR_PENDING;
      SelectStatusbarGlyph( NoteFile<>nil );      // Reset icon on status bar
   end
   else
      if Form_Main.TB_AlarmMode.ImageIndex = IMG_INDEX_NO_OVERDUE_NOR_PENDING then begin
        // Each triggered alarm (there may be more than one) will be notified for a second and image of button TB_AlarmMode will alternate
        Timer.Enabled := True;
        FTicks:= 0;
        Form_Main.TB_AlarmMode.ImageIndex:= IMG_INDEX_OVERDUE_OR_PENDING;
     end;
end;


//----------------------------------
//       Alarm List Management
//----------------------------------


procedure TAlarmManager.AddAlarm( alarm : TAlarm );
begin
    if not assigned(alarm) then exit;

    if alarm.Status = TAlarmDiscarded then
       FDiscardedAlarmList.Add(alarm)
    else
       FAlarmList.Add(alarm);
    UpdateFormMain (alarm);
end;

procedure TAlarmManager.MoveAlarms( noteFrom: TTabNote; nodeFrom : TNoteNode; noteTo: TTabNote; nodeTo: TNoteNode );
begin
    MoveAlarmsInList(FAlarmList, noteFrom, nodeFrom,  noteTo, nodeTo);
    MoveAlarmsInList(FUnfilteredAlarmList, noteFrom, nodeFrom,  noteTo, nodeTo);
    MoveAlarmsInList(FDiscardedAlarmList, noteFrom, nodeFrom,  noteTo, nodeTo);
    MoveAlarmsInList(FSelectedAlarmList, noteFrom, nodeFrom,  noteTo, nodeTo);
    // TODO: Refrescar el nodo actual
end;

procedure TAlarmManager.MoveAlarmsInList( List: TList; noteFrom: TTabNote; nodeFrom : TNoteNode; noteTo: TTabNote; nodeTo: TNoteNode );
var
  I: Integer;
  alarm: TAlarm;
begin
   I:= 0;
   while I <= List.Count - 1 do begin
      alarm:= TAlarm(List[i]);
      if (alarm.note = noteFrom) and assigned(alarm.node) and (alarm.node.ID = nodeFrom.ID) then begin
         alarm.note:= noteTo;
         alarm.node:= nodeTo;
      end;
      I:= I + 1;
   end;
end;

procedure TAlarmManager.RemoveAlarmsOfNode( node : TNoteNode );
var
  I: Integer;
  alarm: TAlarm;
begin
   I:= 0;
   while I <= FAlarmList.Count - 1 do begin
      alarm:= TAlarm(FAlarmList[i]);
      if node = alarm.node then
         RemoveAlarm(alarm);
      I:= I + 1;
   end;
end;

procedure TAlarmManager.RemoveAlarmsOfNote( note : TTabNote );
var
  I: Integer;
  alarm: TAlarm;
begin
   I:= 0;
   while I <= FAlarmList.Count - 1 do begin
      alarm:= TAlarm(FAlarmList[i]);
      if (alarm.node = nil) and (note = alarm.note) then
         RemoveAlarm(FAlarmList[i]);
      I:= I + 1;
   end;
end;


procedure TAlarmManager.ModifyAlarm( alarm: TAlarm );
begin
    UpdateAlarmsState;
    UpdateFormMain(alarm);
end;

procedure TAlarmManager.DiscardAlarm( alarm : TAlarm; MaintainInUnfilteredList: boolean );
begin
    alarm.Status:= TAlarmDiscarded;
    FDiscardedAlarmList.Add(alarm);
    FAlarmList.Remove(alarm);
    FSelectedAlarmList.Remove(alarm);
    if not MaintainInUnfilteredList and assigned(FUnfilteredAlarmList) then
       FUnfilteredAlarmList.Remove(alarm);

    UpdateAlarmsState;
    UpdateFormMain(alarm);
end;

procedure TAlarmManager.RemoveAlarm( alarm : TAlarm );
begin
    FAlarmList.Remove(alarm);
    FDiscardedAlarmList.Remove(alarm);
    FSelectedAlarmList.Remove(alarm);
    if assigned(FUnfilteredAlarmList) then
       FUnfilteredAlarmList.Remove(alarm);

    UpdateAlarmsState;
    UpdateFormMain(alarm);
end;

procedure TAlarmManager.RestoreAlarm( alarm : TAlarm; MaintainInUnfilteredList: boolean  );
begin
    alarm.Status:= TAlarmNormal;
    FDiscardedAlarmList.Remove(alarm);
    FAlarmList.Add(alarm);
    FSelectedAlarmList.Remove(alarm);
    if not MaintainInUnfilteredList and assigned(FUnfilteredAlarmList) then
       FUnfilteredAlarmList.Remove(alarm);

    UpdateAlarmsState;
    UpdateFormMain(alarm);
end;


procedure TAlarmManager.CancelReminders (value: boolean);
begin
   if Value then
      FCanceledAt:= now
   else
      FCanceledAt:= 0;
end;


procedure TAlarmManager.EditAlarms (node: TNoteNode; note: TTabNote; forceAdd: boolean= false);
var
   alarm: TAlarm;
begin
      FSelectedAlarmList.Clear;
      FSelectedAlarmList:= GetAlarms(note, node, false);
      if forceAdd or (FSelectedAlarmList.Count = 0) then begin
         alarm:= TAlarm.Create;
         alarm.node:= node;
         alarm.note:= note;
         alarm.Status:= TAlarmUnsaved;
         FSelectedAlarmList.Add(alarm);
      end;

      ShowFormAlarm (TShowSet);
end;


procedure TAlarmManager.ShowAlarms (const showOnlyPendings: boolean);
var
   modeEdit: TShowMode;
begin
      if showOnlyPendings and (NumberPendingAlarms > 0) then
         modeEdit:= TShowPending
      else
         modeEdit:= TShowAll;

      ShowFormAlarm (modeEdit);
end;

procedure TAlarmManager.ShowFormAlarm (modeEdit: TShowMode);
begin
  if ( Form_Alarm = nil ) then
  begin
    Form_Alarm := TForm_Alarm.Create( Form_Main );
  end;

  try
    Form_Alarm.modeEdit:= modeEdit;

    Form_Alarm.EnableControls(false);
    Form_Alarm.cFilter.Text:= '';
    if (modeEdit = TShowReminders) then
       Form_Alarm.CB_FilterDates.ItemIndex:= 0;
    Form_Alarm.FilterAlarmList;

    UpdateAlarmsState;
    if not Form_Alarm.Visible then
       Form_Alarm.Show
    else
       Form_Alarm.FormShow(nil);

  except
    on E : Exception do
    begin
      messagedlg( E.Message, mtError, [mbOK], 0 );
    end;
  end;

end;



//-----------------------------------------------------
//      Inform Alarms State / Reminders triggered / Overdue events
//-----------------------------------------------------

procedure TAlarmManager.UpdateFormMain ( alarm: TAlarm );
begin
    if alarm.note <> ActiveNote then exit;

    if assigned(alarm.node) then begin
       if assigned(ActiveNote) and (assigned(TTreeNote(ActiveNote).TV.Selected)) and (alarm.node = TTreeNote(ActiveNote).TV.Selected.Data) then
          Form_Main.TB_AlarmNode.Down:= TNoteNode(TTreeNote(ActiveNote).TV.Selected.Data).HasAlarms(false);
    end
    else begin
        Form_Main.MMSetAlarm.Checked:= ActiveNote.HasAlarms(false);
        Form_Main.TAM_SetAlarm.Checked:= Form_Main.MMSetAlarm.Checked;
    end;
end;


procedure TAlarmManager.FlashAlarmState();
var
  Glyph : TPicture;
  Index: integer;
begin
   if (FNumberPending = 0) and (FNumberOverdue = 0) then
      Index:= IMG_INDEX_NO_OVERDUE_NOR_PENDING
   else begin
       Index:= Form_Main.TB_AlarmMode.ImageIndex;
       if Index = IMG_INDEX_OVERDUE_OR_PENDING then
          Index:= IMG_INDEX_NO_OVERDUE_NOR_PENDING
       else
          Index:= IMG_INDEX_OVERDUE_OR_PENDING;
   end;
  Form_Main.TB_AlarmMode.ImageIndex:= Index;

  Glyph := TPicture.Create;
  try
    Form_Main.IMG_Toolbar.GetBitmap(Index, Glyph.Bitmap);
    Form_Main.StatusBar.Panels[PANEL_FILEICON].Glyph := Glyph;
  finally
    Glyph.Free;
  end;
end;

procedure TAlarmManager.StopFlashMode;
begin
    UpdateAlarmsState;
    Timer.Enabled := false;
    SelectStatusbarGlyph( true );      // Reset icon on status bar
end;


procedure TAlarmManager.CommunicateAlarm (alarm : TAlarm);
var
   cad: WideString;
   idAlarm: WideString;
begin
   if alarm.AlarmNote <> '' then
      cad:= ' [' + WideStringReplace(alarm.AlarmNote, #13#10, ' // ', [rfReplaceAll]) + ']'
   else
      cad:= '';

   if assigned(alarm.node) then
      idAlarm:= alarm.node.Name
   else
      idAlarm:= alarm.note.Name;
   Form_Main.StatusBar.Panels[PANEL_HINT].Text := WideFormat(STR_Triggered, [FormatAlarmInstant(alarm.ExpirationDate), idAlarm + cad]);
end;

procedure TAlarmManager.TimerTimer(Sender: TObject);
var
  alarm: TAlarm;
begin
   FlashAlarmState();
   FTicks:= FTicks + 1;
   if FTicks > 14 then begin   // Aprox. 5 seconds
     alarm:= GetNextPendingAlarmForCommunicate;
     if assigned(alarm) then
        CommunicateAlarm(alarm);

     if (FNumberPending = 0) and (FNumberOverdue = 0) then
        StopFlashMode;
   end;
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

   Result:= Format(STR_PendingAndOverdue, [NumberPendingAlarms, NumberOverdueAlarms]) + popupState + soundState;
end;


function TAlarmManager.GetNextPendingAlarmForCommunicate: TAlarm;
var
  I: Integer;
  alarm: TAlarm;
begin
   I:= 0;
   while I <= FAlarmList.Count - 1 do begin
      alarm:= TAlarm(FAlarmList[i]);
      if alarm.Status = TAlarmPendingUnknown then begin
         Result:= alarm;
         exit;
      end;
      I:= I + 1;
   end;

   Result:= nil;
end;




//==========================================================================================
//                                   TForm_Alarm
//==========================================================================================

//----------------------------------
//          Creation / Close
//----------------------------------

procedure TForm_Alarm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := _MainFormHandle;
end; // CreateParams


procedure TForm_Alarm.FormCreate(Sender: TObject);
var
   i: integer;
begin
  FFilteredAlarmList:= TList.Create;
  FCalendarMonthChanged:= false;
  FInitializingControls:= 0;
  FIntervalDirectlyChanged:= False;

  with CB_ProposedIntervalReminder.Items do begin
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
  CB_ProposedIntervalReminder.ItemIndex := 0;

  with CB_ExpirationTime.Items do begin
      for i := 0 to 23 do begin
          Add(Format('%.2d:00', [i]));
          Add(Format('%.2d:30', [i]));
      end;
  end;
  CB_ExpirationTime.Text := '';

  with CB_ShowMode.Items do begin
      Add(STR_All);
      Add(STR_Pending);
      Add(STR_Overdue);
      Add(STR_Discarded);
      Add(STR_AllWithDiscarded);
  end;
  CB_ShowMode.ItemIndex:= -1;

  with CB_FilterDates.Items do begin
      Add(STR_SelectAllDates);
      Add(STR_SelectDays);
      Add(STR_SelectWeek);
      Add(STR_SelectMonth);
  end;
  CB_FilterDates.ItemIndex:= 0;

  cCalendar.Date:= now;
  cCalendar.EndDate:= now;
end;


procedure TForm_Alarm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   ConfirmApplyPendingChanges();

   if (modeEdit = TShowReminders) or (modeEdit = TShowAll) or (modeEdit = TShowPending) or (modeEdit = TShowAllWithDiscarded) then
        AlarmManager.CancelReminders(true);

   AlarmManager.StopFlashMode;
end;


//----------------------------------
//          Show / Resize
//----------------------------------


procedure TForm_Alarm.FormShow(Sender: TObject);
var
  alarm, alarm_selected: TAlarm;
  I, iNewAlarm, iAlarmSelected: Integer;
  nodeNote: TTreeNTNode;
  myNode: TNoteNode;

  procedure AddAlarm (Alarm: TAlarm);
  var
     item : TTntListItem;
  begin
      item := Grid.Items.Add;
      UpdateAlarmOnGrid(alarm, item);
  end;

begin
   ResetModifiedState();
   AlarmManager.CancelReminders(false);
   Button_Sound.Down:= KeyOptions.PlaySoundOnAlarm;
   if (FFilteredAlarmList.Count = 0) then begin
       Grid.Items.Clear;
       cIdentifier.Text := WideFormat( STR_NumberSelected, [0] );
       EnableControls (false);
   end
   else begin
      alarm_selected:= AlarmSelected;
      Grid.Items.BeginUpdate;
      Grid.Items.Clear;
      FFilteredAlarmList.Sort(compareAlarms_selectedColumn);
      iNewAlarm:= -1;
      for I := 0 to FNumberAlarms - 1 do
      begin
        alarm:= TAlarm (FFilteredAlarmList[I]);
        AddAlarm (alarm);
        if alarm.AlarmReminder = 0 then
           iNewAlarm:= i;
      end;
      Grid.Items.EndUpdate;

      Grid.SetFocus;
      if Grid.ItemFocused = nil then
         Grid.ItemFocused:= Grid.Items[0];

      // select new alarm, if exists, or the alarm previously selected, if any
      if iNewAlarm >= 0 then
         Grid.Selected := Grid.Items[iNewAlarm]
      else begin
         i:= FFilteredAlarmList.IndexOf(alarm_selected);
         TrySelectItem(i);
      end;

      if (modeEdit = TShowSet) or (modeEdit = TShowNew) then begin
         txtSubject.Enabled:= true;
         txtSubject.SetFocus;
      end;
   end;

   ccalendar.ForceGetMonthInfo;
   FCalendarMonthChanged:= False;
end;


procedure TForm_Alarm.UpdateCaption;
var
  str: WideString;
  n: integer;
begin
    case FModeEdit of
        TShowReminders:         str:= STR_CaptionReminders;
        TShowSet, TShowNew:     str:= STR_CaptionSet;
        TShowAll:               str:= STR_CaptionAll;
        TShowOverdue:           str:= STR_CaptionOverdue;
        TShowPending:           str:= STR_CaptionPending;
        TShowDiscarded:         str:= STR_CaptionDiscarded;
        TShowAllWithDiscarded:  str:= STR_CaptionAll;
    end;
    if (cFilter.Text <> '') or (CB_FilterDates.ItemIndex <> 0) then
       str:= str + ' ' + STR_FilterApplied;

    Caption:= WideFormat(str, [FNumberAlarms]);
end;


procedure TForm_Alarm.FormResize(Sender: TObject);
var
   formWidth: integer;
begin
    formWidth:= Self.Width;
    Grid.Width := formWidth - 30;

    Button_SelectAll.Left:= formWidth - 341;
    Button_Discard.Left:= formWidth - 230;
    Button_Apply.Left:= formWidth - 113;
    PanelProposedReminder.Left:= formWidth - 498;
    cIdentifier.Width:= formWidth - 211;
    txtSubject.Width:= formWidth - 522;

    TB_Hilite.Left:= formWidth - 441;
    TB_Color.Left:= formWidth - 475;
    TB_Bold.Left:= formWidth - 499;
    Bevel1.Width:= PanelCalendar.Left - 11;

    lblFilter.Left := formWidth - 363;
    cFilter.Left := formWidth - 298;
    Button_ClearFilter.Left := formWidth - 100;
    TB_ClipCap.Left:= formWidth - 70;
    Button_Sound.Left:= formWidth - 46;

end;

//----------------------------------
//          Keyboard
//----------------------------------

procedure TForm_Alarm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_ESCAPE then begin
     if (modeEdit = TShowReminders) or (modeEdit = TShowAll) or (modeEdit = TShowPending) or (modeEdit = TShowAllWithDiscarded) then
        AlarmManager.CancelReminders(true);
     Close;
     end
  else
      if (key = 13) and (cFilter.Focused ) then begin
         ConfirmApplyPendingChanges();
         FilterAlarmList();
         FormShow(nil);
         cFilter.SetFocus;
      end;

end;


//----------------------------------
//           Edition. General
//----------------------------------

procedure TForm_Alarm.CB_ShowModeChange(Sender: TObject);
var
  EnableRemoveButtons: boolean;
begin
    ConfirmApplyPendingChanges();

    EnableRemoveButtons:= false;

    case CB_ShowMode.ItemIndex of
        0: modeEdit:= TShowAll;
        1: modeEdit:= TShowPending;
        2: modeEdit:= TShowOverdue;
        3: modeEdit:= TShowDiscarded;
        4: modeEdit:= TShowAllWithDiscarded;
    end;

    FilterAlarmList();
    FormShow(nil);
end;


procedure TForm_Alarm.EnableControls (Value: Boolean);
var
   AllSelectedAlarmsAreDiscarded: boolean;
   AllSelectedAlarmsAreNotDiscarded: boolean;
   i: integer;
   alarm: TAlarm;
   ls: TListItem;
   severalSelected: boolean;
begin
   FInitializingControls:= FInitializingControls + 1;

   Today_5min.Enabled:= Value;
   Today_10min.Enabled:= Value;
   Today_15min.Enabled:= Value;
   Today_30min.Enabled:= Value;
   Today_1h.Enabled:= Value;
   Today_2h.Enabled:= Value;
   Today_3h.Enabled:= Value;
   Today_5h.Enabled:= Value;
   Today_8AM.Enabled:= Value;
   Today_12AM.Enabled:= Value;
   Today_3PM.Enabled:= Value;
   Today_6PM.Enabled:= Value;
   Today_8PM.Enabled:= Value;
   Tomorrow_8AM.Enabled:= Value;
   Tomorrow_12AM.Enabled:= Value;
   Tomorrow_3PM.Enabled:= Value;
   Tomorrow_6PM.Enabled:= Value;
   Tomorrow_8PM.Enabled:= Value;

   SeveralSelected:= Grid.SelCount > 1;

   alarm:= AlarmSelected;
   chk_Expiration.Enabled:= Value;
   if not Value or SeveralSelected or (alarm.ExpirationDate = 0) then
      chk_Expiration.Checked:= false
   else
      chk_Expiration.Checked:= true;

   CB_ExpirationDate.Enabled:= chk_Expiration.Checked;
   cExpirationTime.Enabled:=   chk_Expiration.Checked;
   CB_ExpirationTime.Enabled:= chk_Expiration.Checked;

   rb_FromNow.Enabled:= Value;
   rb_Before.Enabled:= Value and chk_Expiration.Checked;
   if not rb_Before.Enabled then
      rb_FromNow.Checked:= True;

   CB_ProposedIntervalReminder.Enabled:= Value;
   Button_Apply.Enabled:= Value;
   Button_Show.Enabled:= Value;
   txtSubject.Enabled:= Value;
   TB_Bold.Enabled:= Value;
   TB_Color.Enabled:= Value;
   TB_Hilite.Enabled:= Value;


    AllSelectedAlarmsAreDiscarded:= true;
    AllSelectedAlarmsAreNotDiscarded:= true;

    ls := Grid.Selected;
    while Assigned(ls) and (AllSelectedAlarmsAreDiscarded or AllSelectedAlarmsAreNotDiscarded) do
    begin
       alarm:= TAlarm(ls.Data);
       if alarm.Status= TAlarmDiscarded then
          AllSelectedAlarmsAreNotDiscarded:= false
       else
          AllSelectedAlarmsAreDiscarded:= false;
       ls := Grid.GetNextItem(ls, sdAll, [isSelected]);
    end;


   Button_Remove.Enabled:=     Value and AllSelectedAlarmsAreDiscarded;
   Button_Restore.Enabled:=    Value and AllSelectedAlarmsAreDiscarded;

   Button_Discard.Enabled:=    Value and AllSelectedAlarmsAreNotDiscarded;

   if (not Value) or AllSelectedAlarmsAreDiscarded then
      lblExpiration.Font.Color:= clBlack;

   if Value then
      DisableTimeButtonsInThePast
   else begin
      ReleaseTimeButtons;
      CB_ExpirationDate.DateTime:= now;
      cExpirationTime.Text := '';
      CB_ProposedIntervalReminder.Text := '';
      txtSubject.Text := '';
      cReminder.Text:= '';
      cReminder.Font.Color:= clBlack;
      TB_Bold.Down:= false;
      TB_Color.ActiveColor:= clWindowText;
      TB_Hilite.ActiveColor:= clWindow;
   end;

   FInitializingControls:= FInitializingControls - 1;
end;


procedure TForm_Alarm.SetModeEdit (Value: TShowMode);
var
  str: WideString;
  i: integer;
begin
    FModeEdit:= Value;

    if modeEdit <> TShowNew then begin
        i:= -1;
        case FModeEdit of
          TShowReminders: str:= '';
          TShowSet:       str:= '';
          TShowAll:       begin
                          str:= STR_Hint_All;
                          i:= 0;
                          end;
          TShowPending:   begin
                          str:= STR_Hint_Pending;
                          i:= 1;
                          end;
          TShowOverdue:   begin
                          str:= STR_Hint_Overdue;
                          i:= 2;
                          end;
          TShowDiscarded: begin
                          str:= '';
                          i:= 3;
                          end;
          TShowAllWithDiscarded:
                          begin
                          str:= STR_Hint_All_With_Discarded;
                          i:= 4;
                          end;

        end;
        CB_ShowMode.Hint:= str;
        CB_ShowMode.ItemIndex:= i;
    end;

    if assigned (AlarmManager.UnfilteredAlarmList) then begin
       AlarmManager.UnfilteredAlarmList.Free;
       AlarmManager.UnfilteredAlarmList:= nil;
    end;

    UpdateCaption;
end;

function TForm_Alarm.ChangesToApply: Boolean;
begin
     Result:= FExpirationDateModified or (FProposedReminder >= 0)
              or FSubjectModified or FBoldModified or FFontColorModified or FBackColorModified;
end;

procedure TForm_Alarm.ResetModifiedState;
begin
    FExpirationDateModified:= False;
    FSubjectModified:= False;
    FProposedReminder:= -1;
    FBoldModified:= False;
    FFontColorModified:= False;
    FBackColorModified:= False;
end;


procedure TForm_Alarm.ConfirmApplyPendingChanges();
begin
    if ChangesToApply then begin
       if DoMessageBox( STR_ConfirmApplyPendingChanges, mtConfirmation, [mbYes,mbNo], 0, Handle ) = mrYes  then
          Button_ApplyClick(nil);

       ResetModifiedState;

       // Como consecuencia de DoMessageBox (no ocurre con MessageBox) al retornar de esa ventana modal parece como si no
       // estuviera el foco en el grid, pues la selección se destaca en gris, no en azul. Sin embargo sí lo tiene realmente
       // Prefiero dejar de momento esto así para que se pueda ver bien el mensaje, pues MessageBox no admite Unicode.
       // Al menos no con esta versión de Delphi (2006). Cuando pase a otra más avanzada (en breve) no habrá problema.
    end;
end;


procedure TForm_Alarm.ShowCommonProperties();
var
  Subject: WideString; 
  Bold: boolean; 
  FontColor: TColor;
  backColor: TColor;
  Alarm: TAlarm;
  ls: TListItem;
begin
  Subject:= '';
  bold:= false;
  fontColor:= clWindowText;
  backColor:= clWindow;

  ls := Grid.Selected;
  if assigned(ls) then begin
     Subject:= TAlarm(ls.Data).AlarmNote;
     bold:= TAlarm(ls.Data).Bold;
     fontColor:= TAlarm(ls.Data).FontColor;
     backColor:= TAlarm(ls.Data).BackColor;
  end;

  while Assigned(ls) do
  begin
     alarm:= TAlarm(ls.Data);
     if alarm.AlarmNote <> Subject then
        Subject:= '';
     if alarm.Bold <> bold then
        bold:= false;
     if alarm.FontColor <> fontColor then
        fontColor:= clWindowText;
     if alarm.BackColor <> backColor then
        backColor:= clWindow;

     ls := Grid.GetNextItem(ls, sdAll, [isSelected]);
  end;

  txtSubject.Text:= Subject;
  TB_Bold.Down:= bold;
  TB_Color.ActiveColor:= FontColor;
  TB_Hilite.ActiveColor:= BackColor;
end;


//-------------------------------------------
//     Unfiltered / Filtered    Alarm List
//-------------------------------------------

function TForm_Alarm.UnfilteredAlarmList: TList;
var
   I: Integer;
   alarm: TAlarm;
begin
   if not assigned(AlarmManager.UnfilteredAlarmList) then
   begin
       AlarmManager.UnfilteredAlarmList:= TList.Create;
       if modeEdit <> TShowAllWithDiscarded then
       begin
           case modeEdit of
                TShowAll:       AlarmManager.UnfilteredAlarmList.Assign(AlarmManager.AlarmList);
                TShowDiscarded: AlarmManager.UnfilteredAlarmList.Assign(AlarmManager.DiscardedAlarmList);
                TShowPending:
                     for I := 0 to AlarmManager.AlarmList.Count - 1 do begin
                        alarm:= AlarmManager.AlarmList[I];
                        if alarm.Pending then
                           AlarmManager.UnfilteredAlarmList.Add(alarm);
                     end;

                TShowOverdue:
                     for I := 0 to AlarmManager.AlarmList.Count - 1 do begin
                        alarm:= AlarmManager.AlarmList[I];
                        if alarm.Overdue then
                           AlarmManager.UnfilteredAlarmList.Add(alarm);
                     end;

                else
                    AlarmManager.UnfilteredAlarmList.Assign(AlarmManager.SelectedAlarmList);
           end;
       end
       else
       begin
           AlarmManager.UnfilteredAlarmList.Assign(AlarmManager.AlarmList);
           for I := 0 to AlarmManager.DiscardedAlarmList.Count - 1 do
              AlarmManager.UnfilteredAlarmList.Add( TAlarm (AlarmManager.DiscardedAlarmList[I]) );
       end;
   end;

   Result:= AlarmManager.UnfilteredAlarmList;
end;

procedure TForm_Alarm.FilterAlarmList();
var
   i: integer;
   alarm: TAlarm;
   myExpDate: TDateTime;
   text: WideString;
   Date, EndDate: TDateTime;
   FAuxAlarmList: TList;
begin
   text:= ansilowercase(cFilter.Text);
   Date:= cCalendar.Date;
   EndDate:= cCalendar.EndDate;

   cFilter.Tag := 0;

   FAuxAlarmList:= UnfilteredAlarmList;
   FFilteredAlarmList.Clear;

   if (text <> '') or (CB_FilterDates.ItemIndex <> 0) then begin   // 0 -> All Dates
       for I := 0 to FAuxAlarmList.Count - 1 do
       begin
          alarm:= TAlarm (FAuxAlarmList[I]);

          if (modeEdit in [TShowSet, TShowNew]) and (alarm.AlarmReminder = 0) then
              FFilteredAlarmList.Add(alarm)
          else
              if (text = '') or
                 (pos(text, ansilowercase(alarm.AlarmNote)) > 0) or
                 (assigned(alarm.node) and (pos(text, ansilowercase(alarm.node.Name)) > 0) ) or
                 (pos(text, ansilowercase(alarm.note.Name)) > 0) then begin

                 myExpDate:= RecodeTime(alarm.ExpirationDate, 0,0,0,0);
                 if (CB_FilterDates.ItemIndex = 0) or (myExpDate >= Date ) and (myExpDate <= EndDate ) then
                     FFilteredAlarmList.Add(alarm);
              end;

       end;

   end
   else
       FFilteredAlarmList.Assign(FAuxAlarmList);

   FNumberAlarms:= FFilteredAlarmList.Count;
   UpdateCaption;
end;



//----------------------------------
//          Action Buttons
//----------------------------------

procedure TForm_Alarm.Button_NewClick(Sender: TObject);
var
   alarm: TAlarm;
   ls: TListItem;
   note: TTabNote;
   node: TNoteNode;
   i: integer;
begin
    ls := Grid.Selected;
    if assigned(ls) then begin
       note:= TAlarm(ls.Data).note;
       node:= TAlarm(ls.Data).node;
    end
    else begin
       note:= ActiveNote;
       node:= nil;
    end;

    alarm:= TAlarm.Create;
    alarm.node:= node;
    alarm.note:= note;
    alarm.Status:= TAlarmUnsaved;

    if modeEdit <> TShowNew then
       AlarmManager.SelectedAlarmList.Assign(AlarmManager.UnfilteredAlarmList);  // Make a copy over SelectedAlarmList
    AlarmManager.SelectedAlarmList.Add(alarm);

    // A change in modeEdit will trigger a change in CB_ShowMode, except with TShowNew:
    // CB_ShowMode will keep its value to resalt that the alarms visible comes from that selection, but we need to know
    // that actually we are creating a new alarm to show this alarm although it may not satisfy the filter criterium
    modeEdit:= TShowNew;

    FilterAlarmList;
    FormShow(nil);
end;

procedure TForm_Alarm.Button_ShowClick(Sender: TObject);
begin
     GridDblClick(nil);
end;

procedure TForm_Alarm.Button_ApplyClick(Sender: TObject);
var
  alarm: TAlarm;
  ls: TTntListItem;
  ls2: TListItem;
  i: integer;
  
    procedure ApplyChangesOnAlarm(ls: TTntListItem; alarm: TAlarm);
    begin
        if FExpirationDateModified then
           alarm.ExpirationDate:= NewExpirationDate;

        if FProposedReminder > 0 then
           alarm.AlarmReminder:= FProposedReminder;

        if FSubjectModified then
           alarm.AlarmNote := txtSubject.Text;

        if FBoldModified then
           alarm.Bold:= TB_Bold.Down;

        if FFontColorModified then
           alarm.FontColor:= TB_Color.ActiveColor;

        if FBackColorModified then
           alarm.BackColor:= TB_Hilite.ActiveColor;


        if Alarm.Status= TAlarmUnsaved then begin
           alarm.Status:= TAlarmNormal;
           AlarmManager.AddAlarm(alarm);
           end
        else
           AlarmManager.ModifyAlarm(alarm);
            
        UpdateAlarmOnGrid(alarm, ls);
    end;

begin
    if not ChangesToApply then exit;

    if (FProposedReminder >= 0) and (FProposedReminder < Now) then begin
       CB_ProposedIntervalReminder.SetFocus;
       DoMessageBox( STR_ReminderorDateINC, mtError, [mbOK], 0, Handle );
       exit;
    end;

    for i:= 0 to Grid.Items.Count -1 do begin
        ls:= Grid.Items[i];
        if ls.Selected then begin
           alarm:= TAlarm(ls.Data);
           ApplyChangesOnAlarm(ls, alarm);
        end;
    end;

    NoteFile.Modified := true;

    if ( (FProposedReminder > now) and ((modeEdit = TShowReminders) or (modeEdit = TShowPending)) ) or
       ( FExpirationDateModified and (FNewExpirationDate > now) and (modeEdit = TShowOverdue) )
    then begin
        ls2 := Grid.Selected;
        while Assigned(ls2) do begin
           alarm:= TAlarm(ls2.Data);
           AlarmManager.UnfilteredAlarmList.Remove(alarm);
           ls2 := Grid.GetNextItem(ls2, sdAll, [isSelected]);
        end;

      if (modeEdit = TShowReminders) and (AlarmManager.UnfilteredAlarmList.Count = 0) then
         Close;
    end;

  ResetModifiedState;       // => ChangesToApply -> False

  FilterAlarmList;
  FormShow(nil);
end;

procedure TForm_Alarm.Button_DiscardClick(Sender: TObject);
var
  alarm: TAlarm;
  ls, lsWork: TListItem;
  i: integer;
begin
  if Grid.SelCount > 1 then
     if ( DoMessageBox( WideFormat( STR_ConfirmDiscardALL, [Grid.SelCount] ), mtWarning, [mbYes,mbNo], 0, Handle ) <> mrYes ) then exit;

  ls := Grid.Selected;
  while Assigned(ls) do
  begin
     lsWork:= ls;
     ls := Grid.GetNextItem(ls, sdAll, [isSelected]);

     alarm:= TAlarm(lsWork.Data);
     NoteFile.Modified := true;
     i:= Grid.Items.IndexOf(lsWork);
     AlarmManager.DiscardAlarm (alarm, (modeEdit = TShowAllWithDiscarded));
     if modeEdit <> TShowAllWithDiscarded then
        HideAlarm(lsWork);
  end;

  if (modeEdit = TShowReminders) and (Grid.Items.Count = 0) then
     Close

  else begin
     ccalendar.ForceGetMonthInfo;
     FCalendarMonthChanged:= False;

     Grid.SetFocus;
     if modeEdit <> TShowAllWithDiscarded then
        TrySelectItem(i)
     else begin
        FilterAlarmList;
        FormShow(nil);
     end;
  end;

end;


procedure TForm_Alarm.Button_RestoreClick(Sender: TObject);
var
  alarm: TAlarm;
  ls, lsWork: TListItem;
  i: integer;
begin
   if Grid.SelCount > 1 then
      if ( DoMessageBox( WideFormat( STR_ConfirmRestoreALL, [Grid.SelCount] ), mtWarning, [mbYes,mbNo], 0, Handle ) <> mrYes ) then exit;

  ls := Grid.Selected;
  while Assigned(ls) do
  begin
     lsWork:= ls;
     ls := Grid.GetNextItem(ls, sdAll, [isSelected]);

     alarm:= TAlarm(lsWork.Data);
     if alarm.Status = TAlarmDiscarded then begin
        NoteFile.Modified := true;
        i:= Grid.Items.IndexOf(lsWork);
        AlarmManager.RestoreAlarm (alarm, (modeEdit = TShowAllWithDiscarded));
        if modeEdit <> TShowAllWithDiscarded then
           HideAlarm(lsWork);
     end;
  end;

  ccalendar.ForceGetMonthInfo;
  FCalendarMonthChanged:= False;
  Grid.SetFocus;
  if modeEdit <> TShowAllWithDiscarded then
     TrySelectItem(i)
  else begin
     FilterAlarmList;
     FormShow(nil);
  end;
end;

procedure TForm_Alarm.Button_RemoveClick(Sender: TObject);
var
  alarm: TAlarm;
  ls, lsWork: TListItem;
  i: integer;
  strConfirm: WideString;
begin
  if Grid.SelCount > 1 then
     strConfirm := STR_ConfirmRemoveALL
  else
     strConfirm := STR_ConfirmRemove;

  if ( DoMessageBox( WideFormat( strConfirm, [Grid.SelCount] ), mtWarning, [mbYes,mbNo], 0, Handle ) <> mrYes ) then exit;

  ls := Grid.Selected;
  while Assigned(ls) do
  begin
     lsWork:= ls;
     ls := Grid.GetNextItem(ls, sdAll, [isSelected]);

     alarm:= TAlarm(lsWork.Data);
     if alarm.Status = TAlarmDiscarded then begin
        NoteFile.Modified := true;
        i:= Grid.Items.IndexOf(lsWork);
        AlarmManager.RemoveAlarm (alarm);
        HideAlarm(lsWork);
     end;
  end;

  ccalendar.ForceGetMonthInfo;
  FCalendarMonthChanged:= False;
  Grid.SetFocus;
  TrySelectItem(i);
end;


procedure TForm_Alarm.Button_SelectAllClick(Sender: TObject);
begin
    Grid.SelectAll;
end;

procedure TForm_Alarm.Button_SoundClick(Sender: TObject);
begin
    KeyOptions.PlaySoundOnAlarm:= Button_Sound.Down;
end;


//----------------------------------
//          Alarm Subject
//----------------------------------

procedure TForm_Alarm.txtSubjectChange(Sender: TObject);
begin
   if FInitializingControls > 0 then exit;
   FSubjectModified:= True;
end;


//----------------------------------
//          Expiration/Event Date
//----------------------------------

procedure TForm_Alarm.chk_ExpirationClick(Sender: TObject);
var
   enabled: boolean;
begin
   if FInitializingControls > 0 then exit;

   FInitializingControls:= FInitializingControls + 1;
   enabled:= chk_Expiration.checked;

   CB_ExpirationDate.Enabled:= enabled;
   CB_ExpirationTime.Enabled:= enabled;
   cExpirationTime.Enabled:= enabled;
   rb_Before.Enabled:= enabled;

   if enabled then begin
      rb_Before.Checked:= enabled;
      NewExpirationDate:= Now;
   end
   else begin
      rb_FromNow.Checked:= true;
      NewExpirationDate:= 0;
   end;

   FInitializingControls:= FInitializingControls - 1;
end;

procedure TForm_Alarm.CB_ExpirationDateChange(Sender: TObject);
begin
    NewExpirationDate:= GetSelectedExpirationDate;
end;

procedure TForm_Alarm.cExpirationTimeExit(Sender: TObject);
begin
   cExpirationTime.Text:= TimeRevised(cExpirationTime.Text);
   NewExpirationDate:= GetSelectedExpirationDate;
end;

procedure TForm_Alarm.CB_ExpirationTimeDropDown(Sender: TObject);
var
   t: TDateTime;
   selectedIndex, i: integer;
   strTime: WideString;
begin
    strTime:= TimeRevised(cExpirationTime.Text);
    cExpirationTime.Text:= strTime;
    try
       t:= StrToTime(strTime);
    except
       t:= StrToTime('00:00');
    end;

    selectedIndex:= 0;
    for i:= CB_ExpirationTime.Items.Count-1 downto 0 do
         if t >= StrToTime(CB_ExpirationTime.Items[i]) then begin
            selectedIndex:= i;
            break;
         end;
    CB_ExpirationTime.ItemIndex:= selectedIndex;
end;

procedure TForm_Alarm.CB_ExpirationTimeCloseUp(Sender: TObject);
begin
    if CB_ExpirationTime.Focused  then
       cExpirationTime.SetFocus;
    cExpirationTime.SelStart:= length(cExpirationTime.Text);
    cExpirationTime.SelLength:= 0;
end;

procedure TForm_Alarm.CB_ExpirationTimeSelect(Sender: TObject);
begin
    cExpirationTime.Text := CB_ExpirationTime.Text;
    cExpirationTime.SelStart:= length(cExpirationTime.Text);
    cExpirationTime.SelLength:= 0;
end;



function TForm_Alarm.GetSelectedExpirationDate: TDateTime;
var
   myTime: TDateTime;
begin
   try
      myTime:= StrToTime(cExpirationTime.Text);
      Result:= RecodeTime(CB_ExpirationDate.DateTime, HourOf(myTime), MinuteOf(myTime), 0, 0);
   except
      Result:= 0;
   end;
end;

procedure TForm_Alarm.SetNewExpirationDate(Instant: TDateTime);
var
   myTime: TDateTime;
begin
    FNewExpirationDate:= Instant;
    FExpirationDateModified:= True;
    ShowExpirationDate(Instant);
    ShowReminderStatus(FReminder);
    UpdateProposedReminderDate;
end;

procedure TForm_Alarm.ShowExpirationDate(Instant: TDateTime);
begin
   if Instant = 0 then begin
      CB_ExpirationDate.DateTime := Now;
      cExpirationTime.Text := '';
   end
   else begin
      CB_ExpirationDate.DateTime := Instant;
      cExpirationTime.Text := FormatDateTime('hh:nn', Instant);
   end;
   ShowExpirationStatus(Instant);
end;

procedure TForm_Alarm.ShowExpirationStatus(Instant: TDateTime);
var
   IntervalStr: WideString;
begin
     if Instant <= 0 then
       lblExpirationStatus.Caption:= ''

     else begin
         IntervalStr:= GetTimeIntervalStr(Instant, Now);
         if Instant < now then begin
            lblExpirationStatus.Caption:= '(' + IntervalStr + ' overdue)';
            lblExpirationStatus.Font.Color:= clRed;
            end
         else begin
            lblExpirationStatus.Caption:= '(' + IntervalStr + ' left)';
            lblExpirationStatus.Font.Color:= clGray;
         end;
     end;
end;



//----------------------------------
//     Reminder Instant
//----------------------------------

procedure TForm_Alarm.ShowReminderDate(Instant: TDateTime);
var
   str: WideString;
begin
     str:= ' ';
     if Instant <> 0 then
          str:= FormatAlarmInstant(Instant);

    cReminder.Text := str;
    ShowReminderStatus(Instant);
end;

procedure TForm_Alarm.ShowReminderStatus(Instant: TDateTime);
var
   showAsBefore: Boolean;
   IntervalStr: WideString;
begin
     if Instant <= 0 then
       lblReminderStatus.Caption:= ''

     else begin

         if Instant < now then begin
            IntervalStr:= GetTimeIntervalStr(Instant, Now);
            lblReminderStatus.Caption:= WideFormat(STR_IntervalOverdue, [IntervalStr]);
            lblReminderStatus.Font.Color:= clRed;
            end
         else begin
            if (NewExpirationDate = 0) or (Instant > NewExpirationDate) or (Abs(Instant - now) <= Abs(Instant - NewExpirationDate)) then begin
                IntervalStr:= GetTimeIntervalStr(Instant, Now);
                lblReminderStatus.Caption:= WideFormat(STR_IntervalLeft, [IntervalStr]);
            end
            else begin
                IntervalStr:= GetTimeIntervalStr(Instant, NewExpirationDate);
                lblReminderStatus.Caption:= WideFormat(STR_IntervalBefore, [IntervalStr]);
            end;
            if (NewExpirationDate <> 0) and (NewExpirationDate > Now) and (Instant > NewExpirationDate) then
               lblReminderStatus.Font.Color:= clRed
            else
               lblReminderStatus.Font.Color:= clGray;
         end;
     end;
end;


procedure TForm_Alarm.CleanProposedReminderPanel();
begin
    lblProposedReminder.Caption:= '';
    CB_ProposedIntervalReminder.Text:= '';
    ReleaseTimeButtons;
    if not chk_Expiration.Checked then
        rb_FromNow.Checked:= true;

    FProposedReminder:= -1;
end;

procedure TForm_Alarm.UpdateProposedReminderDate (KeepInterval: boolean = false);
var
   ReferenceDate: TDateTime;
begin
   if CB_ProposedIntervalReminder.Text = '' then exit;

   if rb_FromNow.Checked then
      ReferenceDate:= Now
   else
      ReferenceDate:= NewExpirationDate;

   SetProposedReminderDate(ReferenceDate, CB_ProposedIntervalReminder.Text, (rb_FromNow.Checked = False), KeepInterval);
end;

procedure TForm_Alarm.SetProposedReminderDate(ReferenceDate: TDateTime; IntervalStr: WideString; BeforeReference: Boolean; KeepInterval: boolean= false);
begin
    if IntervalStr <> '' then begin
        try
            FProposedReminder:= IncStrInterval(ReferenceDate, IntervalStr, not BeforeReference);
            if not KeepInterval then
               IntervalStr:= GetTimeIntervalStr(ReferenceDate, FProposedReminder);  // To clean, interpret the value.  Example: "2m" --> "2 minutes"
        except
            FProposedReminder:= 0;
        end;

        ShowProposedReminderDate(FProposedReminder, IntervalStr, ReferenceDate, KeepInterval);
    end
    else
        CleanProposedReminderPanel;
end;


procedure TForm_Alarm.ShowProposedReminderDate(ReminderDate: TDateTime; IntervalStr: WideString; ReferenceDate: TDateTime; KeepInterval: Boolean = false);
begin
     FInitializingControls:= FInitializingControls + 1;

     if ReminderDate = 0 then begin
        lblProposedReminder.Caption:= '';
        ReleaseTimeButtons;
        end
     else begin
        lblProposedReminder.Caption:= FormatAlarmInstant(ReminderDate);
        if ReminderDate < now then
           lblProposedReminder.Font.Color:= clRed
        else
           lblProposedReminder.Font.Color:= clBlue;

        CB_ProposedIntervalReminder.Text:= IntervalStr;
        PressEquivalentTimeButton(ReminderDate, ReferenceDate);
     end;

   FInitializingControls:= FInitializingControls - 1;
end;


procedure TForm_Alarm.CB_ProposedIntervalReminderChange(Sender: TObject);
begin
     if FInitializingControls > 0 then exit;

     UpdateProposedReminderDate(true);
     FIntervalDirectlyChanged:= true;
end;


procedure TForm_Alarm.CB_ProposedIntervalReminderExit(Sender: TObject);
var
   Reminder: TDateTime;
begin
    if FIntervalDirectlyChanged then begin
       UpdateProposedReminderDate;
       FIntervalDirectlyChanged:= False;
    end;
end;

procedure TForm_Alarm.rb_FromNowClick(Sender: TObject);
begin
    if FInitializingControls > 0 then exit;
    UpdateProposedReminderDate
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

   if incHour(Today(), 8) <= now then
      DisableButton(Today_8AM);
   if incHour(Today(), 12) <= now then
      DisableButton(Today_12AM);
   if incHour(Today(), 15) <= now then
      DisableButton(Today_3PM);
   if incHour(Today(), 18) <= now then
      DisableButton(Today_6PM);
   if incHour(Today(), 20) <= now then
      DisableButton(Today_8PM);     
end;


procedure TForm_Alarm.PressEquivalentTimeButton(ReminderDate: TDateTime; ReferenceDate: TDateTime);
var
  difFromNow: int64;
begin
   ReleaseTimeButtons;

   difFromNow:= DateTimeDiff(ReferenceDate, ReminderDate);

   if      (difFromNow >= 4*60) and (difFromNow <= 6*60) then FOptionSelected:= Today_5min
   else if (difFromNow >= 9*60) and (difFromNow <= 11*60) then FOptionSelected:= Today_10min
   else if (difFromNow >= 13.5*60) and (difFromNow <= 16.5*60)then FOptionSelected:= Today_15min
   else if (difFromNow >= 25.5*60) and (difFromNow <= 33.5*60) then FOptionSelected:= Today_30min
   else if (difFromNow >= 54.5*60) and (difFromNow <= 65.5*60) then FOptionSelected:= Today_1h
   else if (difFromNow >= 114.5*60) and (difFromNow <= 126.5*60) then FOptionSelected:= Today_2h
   else if (difFromNow >= 169.5*60) and (difFromNow <= 190.5*60) then FOptionSelected:= Today_3h
   else if (difFromNow >= 284.5*60) and (difFromNow <= 315.5*60) then FOptionSelected:= Today_5h
   else if DateTimeDiff(incHour(Today(), 8), ReminderDate) < 60*15 then FOptionSelected:= Today_8AM
   else if DateTimeDiff(incHour(Today(), 12), ReminderDate) < 60*15 then FOptionSelected:= Today_12AM
   else if DateTimeDiff(incHour(Today(), 15), ReminderDate) < 60*15 then FOptionSelected:= Today_3PM
   else if DateTimeDiff(incHour(Today(), 18), ReminderDate) < 60*15 then FOptionSelected:= Today_6PM
   else if DateTimeDiff(incHour(Today(), 20), ReminderDate) < 60*15 then FOptionSelected:= Today_8PM
   else if DateTimeDiff(incHour(Tomorrow(), 8), ReminderDate) < 60*20 then FOptionSelected:= Tomorrow_8AM
   else if DateTimeDiff(incHour(Tomorrow(), 12), ReminderDate) < 60*20 then FOptionSelected:= Tomorrow_12AM
   else if DateTimeDiff(incHour(Tomorrow(), 15), ReminderDate) < 60*20 then FOptionSelected:= Tomorrow_3PM
   else if DateTimeDiff(incHour(Tomorrow(), 18), ReminderDate) < 60*20 then FOptionSelected:= Tomorrow_6PM
   else if DateTimeDiff(incHour(Tomorrow(), 20), ReminderDate) < 60*20 then FOptionSelected:= Tomorrow_8PM;

   if FOptionSelected <> nil then begin
      TToolbarButton97(FOptionSelected).Down:= true;
   end;
end;


procedure TForm_Alarm.Today_5minClick(Sender: TObject);
var
   minInc: integer;
   Alarm: TDateTime;
   myNode: TNoteNode;
   setFromNow: boolean;
   IntervalStr: WideString;
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
       minInc:= 180
    else if Today_5h.Down then
       minInc:= 300;


    if minInc <> 0 then
       Alarm:= incMinute(now(), minInc)

    else begin
        setFromNow:= true;

        if Today_8AM.Down then
           Alarm:= incHour(Today(), 8)
        else if Today_12AM.Down then
           Alarm:= incHour(Today(), 12)
        else if Today_3PM.Down then
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
    
    IntervalStr:= GetTimeIntervalStr(Now, Alarm);
    CB_ProposedIntervalReminder.Text:= IntervalStr;
    if SetFromNow and (not rb_FromNow.Checked) then begin
       FInitializingControls:= FInitializingControls + 1;
       rb_FromNow.Checked := true;
       FInitializingControls:= FInitializingControls - 1;
    end;

    if rb_FromNow.Checked then begin
        FProposedReminder:= Alarm;
        ShowProposedReminderDate(Alarm, IntervalStr, Now)
        end
    else
        SetProposedReminderDate(NewExpirationDate, IntervalStr, True);

    CB_ProposedIntervalReminder.SetFocus;
    FIntervalDirectlyChanged:= False;
end;

procedure TForm_Alarm.Today_5minDblClick(Sender: TObject);
begin
    Today_5minClick(Sender);
    Button_ApplyClick(nil);
end;

//--------------------------------------------------
//       Alarm Style: Bold, Font color, Back color
//--------------------------------------------------

procedure TForm_Alarm.TB_BoldClick(Sender: TObject);
begin
   FBoldModified:= True;
   txtSubject.SetFocus;
end;

procedure TForm_Alarm.TB_ColorClick(Sender: TObject);
begin
   FFontColorModified:= True;
   txtSubject.SetFocus;
end;

procedure TForm_Alarm.TB_HiliteClick(Sender: TObject);
begin
   FBackColorModified:= True;
   txtSubject.SetFocus;
end;



//----------------------------------
//          Text Filter
//----------------------------------

procedure TForm_Alarm.cFilterChange(Sender: TObject);
begin
    cFilter.Tag := 1;
end;

procedure TForm_Alarm.cFilterExit(Sender: TObject);
begin
    if cFilter.Tag = 1 then begin
       ConfirmApplyPendingChanges();
       FilterAlarmList();
       FormShow(nil);
    end;
end;

procedure TForm_Alarm.Button_ClearFilterClick(Sender: TObject);
begin
    if cFilter.Text <> '' then begin
       cFilter.Text:= '';

      FilterAlarmList();
      FormShow(nil);
    end;
end;


//----------------------------------
//          Calendar
//----------------------------------

procedure TForm_Alarm.CB_FilterDatesChange(Sender: TObject);
begin
   ConfirmApplyPendingChanges();

   if CB_FilterDates.ItemIndex = 0 then begin
      cCalendar.EndDate:= cCalendar.Date;
      FilterAlarmList();
      FormShow(nil);
   end
   else
      cCalendarClick(nil);

   FCalendarMonthChanged:= false;
end;


procedure TForm_Alarm.cCalendarClick(Sender: TObject);
var
   year, month, day : Word;
   beginDate, endDate: TDateTime;
   dayOfWeek: Word;
begin
   ConfirmApplyPendingChanges();

   beginDate:= cCalendar.Date;
   endDate:= cCalendar.EndDate;

   DecodeDate(beginDate, year, month, day);
   case CB_FilterDates.ItemIndex of
        0: endDate:= beginDate;         // AllDates
        2: begin        // Week
           dayOfWeek:= DayOfTheWeek(beginDate);
           endDate:= incDay(beginDate, 7 -dayOfWeek);
           beginDate:= incDay(beginDate, -dayOfWeek+1);
           end;
        3: begin        // Month
           beginDate:= EncodeDate(year, month, 1);
           endDate:= EncodeDate(year, month, DaysInAMonth(year, month));
           end;
   end;

   cCalendar.EndDate:= cCalendar.Date;
   cCalendar.Date:= beginDate;
   cCalendar.EndDate:= endDate;

   if CB_FilterDates.ItemIndex <> 0 then begin
      FilterAlarmList();
      FormShow(nil);
   end
   else if not FCalendarMonthChanged then begin
      CB_FilterDates.ItemIndex:= 1;
      CB_FilterDatesChange(nil);
   end;
   FCalendarMonthChanged:= false;
end;

procedure TForm_Alarm.cCalendarExit(Sender: TObject);
begin
   FCalendarMonthChanged:= false;
end;

procedure TForm_Alarm.cCalendarGetMonthInfo(Sender: TObject; Month: Cardinal;
  var MonthBoldInfo: Cardinal);
var
   days: array of LongWord;
   i, x, day: integer;
   allDays: array[0..31] of boolean;
   alarm: TAlarm;
   dateCalendar: string;
   myList: TList;
begin
  FCalendarMonthChanged:= true;

  myList:= AlarmManager.UnfilteredAlarmList;

  for i:= 0 to 31 do
      allDays[i]:= false;

   dateCalendar:=  IntToStr(Month) + FormatDateTime('yyyy', cCalendar.Date );
   for I := 0 to myList.Count - 1 do
   begin
      alarm:= TAlarm (myList[I]);
      if FormatDateTime('Myyyy', alarm.ExpirationDate ) = dateCalendar then begin
         day:= StrToInt(FormatDateTime( 'd', alarm.ExpirationDate ));
         allDays[day]:= true;
      end;
   end;

   SetLength(days, 31);
   x:= 0;
   for i:= 0 to 31 do
      if allDays[i] then begin
         days[x]:= i;
         x:= x + 1;
      end;      

   if x > 0 then begin
     SetLength(days, x);
     cCalendar.BoldDays(days, MonthBoldInfo);
   end;
end;



//----------------------------------
//            Grid
//----------------------------------

procedure TForm_Alarm.GridEnter(Sender: TObject);
begin
   ConfirmApplyPendingChanges();
end;

function TForm_Alarm.AlarmSelected: TAlarm;
begin
   Result:= nil;
   if assigned(Grid.Selected) then
      Result:= TAlarm(Grid.Selected.Data);
end;

procedure TForm_Alarm.GridSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
   alarm: TAlarm;
   Subject: WideString;
begin
    if not Selected then exit;

    FInitializingControls:= FInitializingControls + 1;

    ResetModifiedState;
    CleanProposedReminderPanel;
    
    alarm:= AlarmSelected;
    if not assigned(alarm) then begin
       cIdentifier.Text := WideFormat( STR_NumberSelected, [0] );
       EnableControls (false);
    end
    else begin
         EnableControls (true);
         
         if Grid.SelCount > 1 then begin                   // Several alarms selected
             cIdentifier.Text:= WideFormat( STR_NumberSelected, [Grid.SelCount] );
             ShowCommonProperties();
             FReminder:= 0;
             ShowReminderDate(0);
         end
         else begin
             if assigned(alarm.Node) then
                cIdentifier.Text := alarm.Note.Name + ' / ' + alarm.Node.Name
             else
                cIdentifier.Text := alarm.Note.Name;

             ShowExpirationDate(alarm.ExpirationDate);
             FNewExpirationDate:= alarm.ExpirationDate;
             FReminder:= alarm.AlarmReminder;
             ShowReminderDate(alarm.AlarmReminder);

             if alarm.Status <> TAlarmUnsaved then begin             // Existent ALARM
                txtSubject.Text:= alarm.AlarmNote;
                TB_Bold.Down:= alarm.Bold;
                TB_Color.ActiveColor:= alarm.FontColor;
                TB_Hilite.ActiveColor:= alarm.BackColor;
                end
             else begin                                              // NEW ALARM
                txtSubject.Text:= '';
                TB_Bold.Down:= false;
                TB_Color.ActiveColor:= clWindowText;
                TB_Hilite.ActiveColor:= clWindow;
                SetProposedReminderDate(Now, '5 ' + STR_minutes, false);
             end;
         end;

    end;

    FInitializingControls:= FInitializingControls - 1;
end;

procedure TForm_Alarm.TrySelectItem(i: integer);
var
  n: integer;
begin
     // Select next alarm, if exists. Otherwise select the previous one
     n:= Grid.Items.Count;
     if i < 0 then begin
        if n >= 1 then
           Grid.Selected:= Grid.Items[0]
        end
     else if i <= n-1 then
        Grid.Selected := Grid.Items[i]
     else if n > 0 then
        Grid.Selected := Grid.Items[n-1];
end;


procedure TForm_Alarm.HideAlarm (ls: TListItem);
var
   i: integer;
begin
   i:= Grid.Items.IndexOf(ls);
   Grid.Items.Delete(i);

   FNumberAlarms:= FNumberAlarms -1;
   UpdateCaption;
end;


procedure TForm_Alarm.GridDblClick(Sender: TObject);
var
  Location: TLocation;
begin
    Location:= CreateLocation(AlarmSelected);
    if assigned(Location) then
       JumpToLocation (Location);
end;

procedure TForm_Alarm.GridColumnClick(Sender: TObject; Column: TListColumn);
var
  index: integer;
  oldSortedColumn: TSortedColumn;
begin
    index:= Column.Index;

    oldSortedColumn:= SortedColumn;
    if index = 0  then
       SortedColumn := TColumnNoteName
    else if index = 1  then
       SortedColumn := TColumnNodeName
    else if (index = 2) or (index = 3) then
       SortedColumn := TColumnExpiration
    else if index = 4  then
       SortedColumn := TColumnAlarmNote
    else if (index = 5) or (index = 6)  then
       SortedColumn := TColumnReminder
    else if (index = 7) then
       SortedColumn := TColumnDiscarded;

    if oldSortedColumn = SortedColumn then
       AscOrder:= AscOrder * -1;

    FormShow(nil);
end;


procedure TForm_Alarm.UpdateAlarmOnGrid(alarm: TAlarm; item: TTntListItem);
  var
     NodeName, NoteName: WideString;
     Discarded: string;
  begin
      if assigned(alarm.node) then
         NodeName:= alarm.Node.Name
      else
         NodeName:= '';
      NoteName:= alarm.Note.Name;
      Discarded:= '';
      if alarm.Status = TAlarmDiscarded then Discarded := 'D';

      item.Data:= Alarm;
      item.subitems.Clear;
      item.caption := NoteName;                                            // COLUMN_NoteName
      item.subitems.Add( NodeName);                                        // COLUMN_NodeName
      item.subitems.Add( FormatDate(alarm.ExpirationDate));                // COLUMN_ExpirationDate
      if alarm.ExpirationDate <> 0 then
          item.subitems.Add( FormatDateTime('hh:nn', alarm.ExpirationDate) )  // COLUMN_ExpirationTime
      else
          item.subitems.Add( '');
      item.subitems.Add( WideStringReplace(alarm.AlarmNote, #13#10, '··', [rfReplaceAll]) );  // COLUMN_AlarmNote
      item.subitems.Add( FormatDate(alarm.AlarmReminder) );               // COLUMN_ReminderDate
      item.subitems.Add( FormatDateTime('hh:nn', alarm.AlarmReminder) );  // COLUMN_ReminderTime
      item.subitems.Add( Discarded);                                      // COLUMN_Discarded
end;


procedure TForm_Alarm.GridAdvancedCustomDrawItem(
   Sender: TCustomListView;
   Item: TListItem;
   State: TCustomDrawState;
   Stage: TCustomDrawStage;
   var DefaultDraw: Boolean) ;
begin
    Sender.Canvas.Brush.Color:= TAlarm(item.Data).BackColor;
    Sender.Canvas.Font.Color := TAlarm(item.Data).FontColor;
end;

procedure TForm_Alarm.GridAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin

    if (TAlarm(item.Data).Bold) and (subitem>=2) and (subitem <=4)  then begin
       Sender.Canvas.Brush.Color:= TAlarm(item.Data).BackColor;
       Sender.Canvas.Font.Color := TAlarm(item.Data).FontColor;
       Sender.Canvas.Font.Style:= [fsBold];
    end;
    if TAlarm(item.Data).AlarmReminder = 0 then
       Sender.Canvas.Font.Style:= [fsItalic, fsBold];
end;



//----------------------------------
//     Copy to the CLIPBOARD
//----------------------------------

procedure TForm_Alarm.TB_ClipCapClick(Sender: TObject);
begin
    CopyAlarmList();
end;

function TForm_Alarm.CreateLocation(alarm: TAlarm): TLocation;
var
  note: TTabNote;
begin
    Result:= nil;

    if assigned(alarm) then begin
      note:= alarm.note;

      Result := TLocation.Create;
      Result.FileName := notefile.FileName;
      Result.NoteName := note.Name;
      Result.NoteID := alarm.note.ID;
      Result.CaretPos := 0;
      Result.SelLength := 0;
      if assigned(alarm.node) then begin
         Result.NodeName := alarm.node.Name;
         Result.NodeID := alarm.node.ID
         end
      else
         Result.NodeID := -1;
    end;
end;


procedure TForm_Alarm.CopyAlarmList();
var
  alarm: TAlarm;
  ls: TListItem;
  i, pos: integer;
  ColorsTbl: array of TColor;
  iColorsTbl: integer;
  rtf, rtfColorsTbl: string;

  function GetPositionInRTFTable(Color: TColor): integer;
  var
      i: integer;
  begin
      Result:= -1;  // By default, the color is not available

      for I := 0 to length(ColorsTbl) - 1 do
           if ColorsTbl[i] = Color then
           begin
              Result:= i;
              exit;
           end
  end;

  procedure ProcessColor(Color: TColor; IgnoreIfEqual: TColor);
  begin
     if (Color <> IgnoreIfEqual) then
     begin
         pos:= GetPositionInRTFTable(Color);
         if pos < 0  then
         begin
            if iColorsTbl = length(ColorsTbl) then begin
               SetLength(ColorsTbl, iColorsTbl + 10);
               end;
            ColorsTbl[iColorsTbl]:= Color;
            iColorsTbl:= iColorsTbl + 1;
            rtfColorsTbl:= rtfColorsTbl + GetRTFColor(Color);
         end;
     end;
  end;

  //  References: Table definitions in RTF: http://www.biblioscape.com/rtf15_spec.htm#Heading40
  function GenerateHeader(): string;
    begin
        Result:= '\trowd\trgaph30 \cellx500\cellx1500\cellx3500\cellx4700\cellx5400\cellx8900\cellx9900\cellx10400\cellx10900 \pard\b\cf0\intbl ' +
             'Lnk \cell ' +
             'Note \cell ' +
             'Node \cell ' +
             'Expiration \cell ' +
             'Time \cell ' +
             '\qc\ Subject \cell ' +
             'Reminder \cell ' +
             'Time \cell ' +
             'Disc. \cell ' +
             '\row\b0' +#13#10;
  end;

  function GenerateAlarmRow(alarm: TAlarm): string;
    var
       NodeName, NoteName: WideString;
       Discarded: string;
       colorFont, bgColor: string;
       pos: integer;
       hyperlink: string;
       location: TLocation;
    begin
        if assigned(alarm.node) then
           NodeName:= alarm.Node.Name
        else
           NodeName:= '';
        NoteName:= alarm.Note.Name;
        Discarded:= '';
        if alarm.Status = TAlarmDiscarded then Discarded := 'D';

        location:= CreateLocation(alarm);
        hyperlink:= '';
        if assigned(location) then begin
            hyperlink:= '{\field{\*\fldinst{HYPERLINK "'
              + URLToRTF(BuildKNTLocationText(location, true), false ) + '"}}{\fldrslt{\cf1\ul lnk}}}\cf0\ulnone';
        end;

        colorFont:= ColorToString(alarm.BackColor);

        colorFont:= '\cf0';
        pos:= GetPositionInRTFTable(alarm.FontColor);
        if pos > 0 then
            colorFont:= '\cf' + IntToStr(pos);

        bgColor:= '';
        pos:= GetPositionInRTFTable(alarm.BackColor);
        if pos > 0 then
           bgColor:= '\clcbpat' + IntToStr(pos);

        Result:= '\trowd\trgaph30 ' +
                   bgColor + '\cellx500'+
                   bgColor + '\cellx1500'+
                   bgColor + '\cellx3500'+
                   bgColor + '\cellx4700'+
                   bgColor + '\cellx5400'+
                   bgColor + '\cellx8900'+
                   bgColor + '\cellx9900'+
                   bgColor + '\cellx10400'+
                   bgColor + '\cellx10900' +
                   '\pard\intbl' + colorFont + ' ';

        Result:= Result + hyperlink + '\cell ';                                       // Hyperlink
        Result:= Result + TextToUseInRTF(NoteName) + '\cell ';                        // NoteName
        Result:= Result + TextToUseInRTF(NodeName) + '\cell ';                        // NodeName

        if alarm.Bold then
           Result:= Result + '\b ';

        Result:= Result + FormatDate(alarm.ExpirationDate) + '\cell ';                // ExpirationDate
        if alarm.ExpirationDate <> 0 then
           Result:= Result + FormatDateTime('hh:nn', alarm.ExpirationDate) + '\cell ' // ExpirationTime
        else
           Result:= Result + '\cell ';

        Result:= Result + TextToUseInRTF(alarm.AlarmNote) + '\cell ';                 // AlarmNote

        if alarm.Bold then
           Result:= Result + '\b0 ';

        Result:= Result + FormatDateTime('dd/MMM/yyyy', alarm.AlarmReminder) + '\cell ';  // ReminderDate
        Result:= Result + FormatDateTime('hh:nn', alarm.AlarmReminder) + '\cell ';        // ReminderTime
        Result:= Result + Discarded + '\cell ';                                           // Discarded
        Result:= Result + '\row' + #10#13;
  end;


begin
  rtf:= '';

  // Prepare table of colors
  rtfColorsTbl:= '{\colortbl ;\red0\green0\blue255;';
  SetLength(ColorsTbl, 10);
  ColorsTbl[0]:= -1;
  ColorsTbl[1]:= RGB(0,0,255);
  iColorsTbl:= 2;

  ls := Grid.Selected;
  while Assigned(ls) do
  begin
     alarm:= TAlarm(ls.Data);
     ProcessColor(alarm.FontColor, clWindowText);
     ProcessColor(alarm.BackColor, clWindow);
     ls := Grid.GetNextItem(ls, sdAll, [isSelected]);
  end;
  rtfColorsTbl:= rtfColorsTbl + '} ';


  // Include RTF representation of selected alarms
  ls := Grid.Selected;
  while Assigned(ls) do
  begin
     alarm:= TAlarm(ls.Data);
     rtf:= rtf + GenerateAlarmRow(alarm);
     ls := Grid.GetNextItem(ls, sdAll, [isSelected]);
  end;

  if rtf <> '' then begin
     rtf:= '{\rtf1\ansi' + rtfColorsTbl + generateHeader() + rtf + '}';
     Clipboard.AsRTF:= rtf;
  end;

end;



initialization
  AscOrder:= 1;
  SortedColumn := TColumnExpiration;
end.