
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

unit kn_SendMail;

// - log only when MJ_DEBUG is set
// - if sending "all" or "file", and the file is encrypted, WARN.
//   (actually: disable "file" when file is encrypted!)

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, SmtpProt, ComCtrls,
  kn_Info, kn_Const, IniFiles,
  gf_misc, gf_strings, wsocket,
  kn_FileObj, kn_NoteObj, ExtCtrls,
  RxRichEd, TreeNT, kn_NodeList,
  gf_files, kn_INI, GFLog, TntStdCtrls;

type
  TForm_Mail = class(TForm)
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    Pages: TPageControl;
    Tab_Send: TTabSheet;
    Tab_SMTP: TTabSheet;
    GroupBox_Source: TTntGroupBox;
    RB_Current: TTntRadioButton;
    RB_All: TTntRadioButton;
    RB_File: TTntRadioButton;
    GroupBox1: TTntGroupBox;
    RB_PlainText: TTntRadioButton;
    RB_RTF: TTntRadioButton;
    GroupBox2: TTntGroupBox;
    Label1: TTntLabel;
    Combo_TO: TTntComboBox;
    Label2: TTntLabel;
    Edit_Subject: TTntEdit;
    Label3: TTntLabel;
    Combo_CC: TTntComboBox;
    GroupBox3: TTntGroupBox;
    SmtpCli: TSmtpCli;
    Label4: TTntLabel;
    Edit_SMTPServer: TTntEdit;
    Label5: TTntLabel;
    Edit_Port: TTntEdit;
    Label6: TTntLabel;
    Edit_From: TTntEdit;
    Label7: TTntLabel;
    Combo_Charset: TTntComboBox;
    Label_Status: TTntLabel;
    GFLog: TGFLog;
    CheckBox_Log: TTntCheckBox;
    Label8: TTntLabel;
    Edit_FirstLine: TTntEdit;
    Button_Help: TTntButton;
    CheckBox_ExcludeHiddenNodes: TTntCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure RB_CurrentClick(Sender: TObject);
    procedure SmtpCliHeaderLine(Sender: TObject; Msg: PChar;
      Size: Integer);
    procedure SmtpCliRequestDone(Sender: TObject; RqType: TSmtpRequest;
      Error: Word);
    procedure TimerTimer(Sender: TObject);
    procedure SmtpCliDisplay(Sender: TObject; Msg: String);
    procedure RB_FileClick(Sender: TObject);
    procedure Button_HelpClick(Sender: TObject);
  private
    { Private declarations }
    procedure ShowException( Sender : TObject; E : Exception );
  public
    { Public declarations }
    OK_Click : boolean;
    myINI_FN : string;
    myADR_FN : string;
    mySIG_FN : string;

    MailOptions : TMailOptions;
    IsBusy : boolean;
    myNotes : TNoteFile;
    myActiveNote : TTabNote;
    myCurNoteName : string;
    UserFieldsAdded : boolean;
    HadError : boolean;
    TimerTick : integer;
    Padding : string;
    InHeader : boolean;

    procedure SendEmail;
    procedure BeforeMail;
    procedure AfterMail;

    function Verify : boolean;
    function ExpandTokenLine( aLine : string ) : string;
    procedure AddNoteToMailMessage( const aNote : TTabNote );

  end;

function SMTPErrorDesc( Error : integer ) : string;

implementation
uses TntSystem, TntSysUtils;
{$R *.DFM}

resourcestring
  STR_01 = 'Service not available';
  STR_02 = 'Requested mail action not taken: mailbox unavailable';
  STR_03 = 'Requested action aborted: local error in processing';
  STR_04 = 'Requested action not taken: insufficient system storage';
  STR_05 = 'Syntax error, command unrecognized';
  STR_06 = 'Command not implemented';
  STR_07 = 'Bad sequence of commands';
  STR_08 = 'Command parameter not implemented';
  STR_09 = 'Requested action not taken: mailbox unavailable';
  STR_10 = 'User not local';
  STR_11 = 'Requested mail action aborted: exceeded storage allocation';
  STR_12 = 'Requested action not taken: mailbox name not allowed';
  STR_13 = 'Transaction failed';
  STR_14 = 'Unknown error or no error condition';
  STR_15 = 'Transaction aborted by user';
  STR_16 = 'SMTP server must be specified';
  STR_17 = 'SMTP port must be specified';
  STR_18 = '"From" address (your email address) must be specified';
  STR_19 = '"To" address (recipient) must be specified';
  STR_20 = 'Cannot send email message: ';
  STR_21 = 'The Subject line is empty. Send anyway?';
  STR_22 = 'File format and note selection conflict. Bug?';
  STR_23 = 'Connecting...';
  STR_24 = '&Abort';
  STR_25 = 'Close';
  STR_26 = 'Error';
  STR_28 = 'Error: code ';
  STR_29 = 'Sending...';
  STR_30 = 'Closing connection...';
  STR_31 = 'Mail sent.';
  STR_32 = 'Connection timed out';
  STR_33 = 'Tree-type notes cannot be sent as RTF files and will be skipped.';
  STR_34 = 'Ready.';

function SMTPErrorDesc( Error : integer ) : string;
begin
  case Error of
    421 : result := STR_01;
    450 : result := STR_02;
    451 : result := STR_03;
    452 : result := STR_04;
    500 : result := STR_05;
    502 : result := STR_06;
    503 : result := STR_07;
    504 : result := STR_08;
    550 : result := STR_09;
    551 : result := STR_10;
    552 : result := STR_11;
    553 : result := STR_12;
    554 : result := STR_13;
    else
      result := STR_14;
  end;
end; // SMTPErrorDesc


procedure TForm_Mail.FormCreate(Sender: TObject);
begin

  OK_Click := false;
  IsBusy := false;
  myActiveNote := nil;
  myNotes := nil;
  myCurNoteName := '';
  UserFieldsAdded := false;
  Application.OnException := ShowException;
  HadError := false;
  TimerTick := 0;
  // Timer.Enabled := false;
  Padding := '-----------';
  InHeader := true;

  Pages.ActivePage := Tab_Send;

  myINI_FN := normalFN( extractfilepath( Application.exename ) + 'keymail.ini' );

  InitializeMailOptions( MailOptions );
  Combo_Charset.ItemIndex := 0;

end; // CREATE

procedure TForm_Mail.FormActivate(Sender: TObject);
begin
  OnActivate := nil;

  myADR_FN := changefileext( myINI_FN, ext_ADR );
  mySIG_FN := changefileext( myINI_FN, ext_SIG );

  GFLog.FileName := changefileext( myINI_FN, ext_LOG );
  if fileexists( myADR_FN ) then
  begin
    Combo_TO.items.LoadFromFile( myADR_FN );
    Combo_CC.items.LoadFromFile( myADR_FN );
  end;

  LoadMailOptions( myINI_FN, MailOptions );

  CheckBox_Log.Checked := MailOptions.KeepLog;
  Edit_FirstLine.Text := MailOptions.FirstLine;

  if MailOptions.TextCharSet <> '' then
    Combo_Charset.Text := MailOptions.TextCharSet;

  if MailOptions.AsPlainText then
    RB_PlainText.Checked := true
  else
    RB_RTF.Checked := true;

  Edit_SMTPServer.Text := MailOptions.SMTPServer;
  Edit_From.Text := MailOptions.FromAddr;

  if MailOptions.SMTPPort <> '' then
    Edit_Port.Text := MailOptions.SMTPPort
  else
    Edit_Port.Text := 'smtp';

  if assigned( myActiveNote ) then
    myCurNoteName := RemoveAccelChar( myActiveNote.Name );
  if ( myCurNoteName <> '' ) then
  begin
    RB_Current.Caption := RB_Current.Caption + ': "' + myCurNoteName + '"';
    Edit_Subject.Text := MailOptions.SubjectPrefix + myCurNoteName;
  end
  else
  begin
    RB_Current.Enabled := false;
    RB_All.Checked := true;
  end;

  CheckBox_ExcludeHiddenNodes.Checked:= True;    // [dpv]

  RB_CurrentClick( self );

  RB_Current.OnClick := RB_CurrentClick;
  RB_All.OnClick := RB_CurrentClick;
  RB_File.OnClick := RB_CurrentClick;

  Combo_TO.SetFocus;

end; // ACTIVATE

procedure TForm_Mail.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ( IsBusy and ( not( SmtpCli.State in [smtpReady,smtpAbort] ))) then
  begin
    SMTPCli.Abort;
    AfterMail;
    Label_Status.Caption := STR_15;
    CanClose := false;
  end
  else
  begin
    CanClose := true;
  end;
end;

procedure TForm_Mail.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if (( shift = [] ) and ( not ( Combo_TO.DroppedDown or Combo_CC.DroppedDown or Combo_Charset.DroppedDown )))
      then begin
      OK_Click := false;
      ModalResult := mrCancel;
    end;
  end;
end;


procedure TForm_Mail.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if MailOptions.KeepLog then
    GFLog.Save( '' );
end;

procedure TForm_Mail.Button_OKClick(Sender: TObject);
begin

  SendEmail;

end;

procedure TForm_Mail.Button_CancelClick(Sender: TObject);
begin
  // CANCEL click
  OK_Click := false;
  MOdalResult := mrCancel;
end;

function TForm_Mail.Verify : boolean;
var
  s : string;
begin
  result := false;
  s := '';

  if Edit_SMTPServer.text = '' then
    s := STR_16
  else
  if Edit_Port.text = '' then
    s := STR_17
  else
  if Edit_From.text = '' then
    s := STR_18
  else
  if Combo_To.text = '' then
    s := STR_19;

  if s <> ''  then
  begin
    case messagedlg( STR_20 + #13 + s, mtError, [mbOK, mbCancel], 0 ) of
      mrOK : begin end;
      else
        OK_Click := false;
        ModalResult := mrCancel; // close form
    end;
  exit;
  end;

  if Edit_Subject.Text = '' then
  begin
    case messagedlg( STR_21, mtConfirmation, [mbYes,mbNo], 0 ) of
      mrOK : begin end;
      else
      begin
        Pages.ActivePage := Tab_Send;
        Edit_Subject.SetFocus;
        OK_Click := false;
        exit;
      end;
    end;
  end;


  result := ( s = '' );

end; // Verify

procedure TForm_Mail.RB_CurrentClick(Sender: TObject);
begin
  if RB_Current.Checked then
  begin
    RB_RTF.Enabled := ( myActiveNote.Kind = ntRTF );
    RB_PlainText.Enabled := true;
  end
  else
  if RB_All.Checked then
  begin
    RB_PlainText.Checked := true;
    RB_RTF.Enabled := false;
    RB_PlainText.Enabled := true;
  end
  else
  begin
    RB_RTF.Checked := true;
    RB_PlainText.Enabled := false;
  end;
end;

function TForm_Mail.ExpandTokenLine( aLine : string ) : string;
var
  p : integer;
begin
  result := aLine;

  p := pos( MAILFILENAME, result );
  if ( p > 0 ) then
  begin
    delete( result, p, 2 );
    insert( myNotes.FileName, result, p )
  end;

  p := pos( MAILNOTENAME, result );
  if ( p > 0 ) then
  begin
    delete( result, p, 2 );
    insert( MyCurNoteName, result, p );
  end;

  p := pos( MAILKEYNOTEVERSION, result );
  if ( p > 0 ) then
  begin
    delete( result, p, 2 );
    insert( Program_Name + #32 + Program_Version, result, p );
  end;

  p := pos( MAILNOTECOUNT, result );
  if ( p > 0 ) then
  begin
    delete( result, p, 2 );
    if RB_Current.Checked then
      insert( '1', result, p )
    else
      insert( inttostr( myNotes.Notes.Count ), result, p );
  end;

end; // ExpandTokenLine

procedure TForm_Mail.AddNoteToMailMessage( const aNote : TTabNote );
var
  tNote : TTreeNote;
  myTreeNode : TTreeNTNode;
  myNoteNode : TNoteNode;
  RichEdit : TRxRichEdit;
begin

  // SMTPCli.MailMessage.Add( Padding + ' Note: ' + aNote.Name + ' ' + Padding );
  SMTPCli.MailMessage.Add( '' );

  case aNote.Kind of

    ntRTF : begin
      SMTPCli.MailMessage.AddStrings( aNote.Editor.Lines );
    end;

    ntTree : begin
      tNote := TTreeNote( aNote );
      // List := TStringList.Create;
      RichEdit := TRxRichEdit.Create( self );
      with RichEdit do
      begin
        parent := self;
        Visible := false;
        WordWrap := false;
      end;

      try
        myTreeNode := tNote.TV.Items.GetFirstNode;
        if myTreeNode.Hidden and CheckBox_ExcludeHiddenNodes.Checked then  // [dpv]
           myTreeNode := myTreeNode.GetNextNotHidden;
        while assigned( myTreeNode ) do
        begin
          myNoteNode := TNoteNode( myTreeNode.Data );
          if assigned( myNoteNode ) then
          begin
            SMTPCli.MailMessage.Add( '' );
            SMTPCli.MailMessage.Add( '+ Node ' + myNoteNode.Name + '  Level ' + inttostr( myTreeNode.Level ));

            myNoteNode.Stream.Position := 0;
            RichEdit.Lines.LoadFromStream( myNoteNode.Stream );
            SMTPCli.MailMessage.AddStrings( RichEdit.Lines );

          end;
          if CheckBox_ExcludeHiddenNodes.Checked then  // [dpv]
             myTreeNode := myTreeNode.GetNextNotHidden
          else
             myTreeNode := myTreeNode.GetNext;
        end;
      finally
        RichEdit.Free;
      end;

    end;

  end;

  SMTPCli.MailMessage.Add( '' );

end; // AddNoteToMailMessage

procedure TForm_Mail.SendEmail;
var
  s: wideString;
  buf : string;
  i, cnt : Integer;
  Lines : TStringList;
begin
  if ( not Verify ) then exit;

  OK_Click := true;

  MailOptions.AsPlainText := RB_PlainText.checked or ( not RB_RTF.ENabled );
  MailOptions.CCAddr := Combo_CC.Text;
  MailOptions.FromAddr := Edit_From.Text;
  MailOptions.SMTPPort := trim( Edit_Port.Text );
  MailOptions.SMTPServer := trim( Edit_SMTPServer.Text );
  MailOptions.Subject := trim( Edit_Subject.Text );
  MailOptions.TextCharSet := Combo_Charset.Text;
  MailOptions.ToAddr := Combo_To.Text;
  MailOptions.CCAddr := Combo_CC.Text;
  MailOptions.KeepLog := CheckBox_Log.Checked;
  MailOptions.FirstLine := trim( Edit_FirstLine.Text );

  SaveMailOptions( myINI_FN, MailOptions );

  GFLog.Active := MailOptions.KeepLog;

  BeforeMail;
  Lines := TStringList.Create;

  try
    try
      SMTPCli.Host := MailOptions.SMTPServer;
      SMTPCli.Port := MailOptions.SMTPPort;
      SMTPCli.SignOn := LocalHostName;
      SMTPCli.FromName := MailOptions.FromAddr;
      SMTPCli.HdrFrom := MailOptions.FromAddr;
      SMTPCli.Charset := MailOptions.TextCharSet;
      SMTPCli.HdrSubject := ExpandTokenLine( MailOptions.Subject );
      SMTPCli.HdrTo := MailOptions.ToAddr;

      SmtpCli.RcptName.Clear;

      buf := MailOptions.ToAddr;
      while TRUE do
      begin
        i := pos( ',', buf );
        if ( i = 0 ) then
        begin
          s := trim( buf );
          if ( s <> '' ) then
            SMTPCli.RcptName.Add( s );
          break;
        end
        else
        begin
          s := trim( copy( buf, 1, i-1 ));
          if ( s <> '' ) then
            SMTPCli.RcptName.Add( s );
          delete( buf, 1, i );
        end;
      end;

      for i := 0 to pred( SMTPCli.RcptName.Count ) do
        GFLog.Add( 'To ' + inttostr( succ( i )) + ' = ' + SMTPCli.RcptName[i] );

      buf := MailOptions.CCAddr;
      while TRUE do
      begin
        i := pos( ',', buf );
        if ( i = 0 ) then
        begin
          s := trim( buf );
          if ( s <> '' ) then
            SMTPCli.RcptName.Add( s );
          break;
        end
        else
        begin
          s := trim( copy( buf, 1, i-1 ));
          if ( s <> '' ) then
            SMTPCli.RcptName.Add( s );
          delete( buf, 1, i );
        end;
      end;

      SMTPCli.MailMessage.Clear;
      if MailOptions.AsPlainText then  // plain text in body of email
      begin
        if RB_Current.Checked then
        begin
          SMTPCli.MailMessage.Add( ExpandTokenLine( MailOptions.FirstLine ));
          AddNoteToMailMessage( myActiveNote );
        end
        else
        if RB_All.Checked then
        begin
          SMTPCli.MailMessage.Add( ExpandTokenLine( MailOptions.FirstLine ));
          if ( myNotes.Notes.Count > 0 ) then
          begin
            for cnt := 0 to pred( myNotes.Notes.Count ) do
            begin
              AddNoteToMailMessage( myNotes.Notes[cnt] );
              SMTPCli.MailMessage.Add( '' );
            end;
          end;
        end
        else
        begin
          // nothing
        end;

      end
      else // RTF file as attachment (body empty apart from itro line and sig)
      begin
        if RB_Current.Checked then
        begin
          s := WideExtractfilepath( myINI_FN ) + MakeValidFileName( myCurNoteName, [], MAX_FILENAME_LENGTH ) + ext_RTF;
          GFLog.Add( 'Attaching: ' + s );
          myActiveNote.Editor.Lines.SaveToFile( WideStringToUTF8(s) );
          SMTPCli.MailMessage.Add( ExpandTokenLine( MailOptions.FirstLine ));
          // SMTPCli.MailMessage.Add( Padding + ' Note: ' + myCurNoteName + ' ' + Padding );
          SMTPCli.EmailFiles.Add( s );
        end
        else
        if RB_File.Checked then
        begin
          SMTPCli.MailMessage.Add( ExpandTokenLine( MailOptions.FirstLine ));
          // SMTPCli.MailMessage.Add( Padding + ' Note: ' + myCurNoteName + ' ' + Padding );
          SMTPCli.EmailFiles.Add( myNotes.FileName );
          GFLog.Add( 'Attaching: ' + myNotes.FileName );
        end
        else
        begin
          ShowMessage( STR_22 );
        end;
      end;

      // see if we can attach a sig to the message
      if fileexists( mySIG_FN ) then
      begin
        Lines.Clear;
        Lines.LoadFromFile( mySIG_FN );
        SMTPCli.MailMessage.AddStrings( Lines );
      end;
      SMTPCli.MailMessage.Add( '' );

    except
        on E : Exception do
        begin
          messagedlg( E.Message, mtError, [mbOK], 0 );
          AfterMail;
          exit;
        end;
    end;
  finally
    Lines.Free;
  end;

  Label_Status.Caption := STR_23;
  // Timer.Enabled := true;

  SMTPCli.Open;

end; // SendEmail

procedure TForm_Mail.SmtpCliHeaderLine(Sender: TObject; Msg: PChar;
  Size: Integer);
begin
  if UserFieldsAdded then exit;
  if StrLIComp(Msg, 'Subject:', 8) = 0 then
  begin
    StrCat(Msg, PChar( #13#10 + 'X-Mailer: ' +  MailOptions.XMailer ));
    if ( MailOptions.CCAddr <> '' ) then
      StrCat(Msg, PChar( #13#10 + 'CC: ' +  MailOptions.CCAddr ));
    UserFieldsAdded := true;
  end;

end;

procedure TForm_Mail.BeforeMail;
begin
  IsBusy := true;
  Button_OK.Enabled := false;
  Pages.Enabled := false;
  Button_Cancel.Caption := STR_24;
  Button_Cancel.SetFocus;
  TimerTick := 0;

end; // BeforeMail

procedure TForm_Mail.AfterMail;
begin
  // Timer.Enabled := false;
  IsBusy := false;
  Button_OK.Enabled := true;
  Pages.Enabled := true;
  Button_Cancel.Caption := STR_25;
  UserFieldsAdded := false;
  HadError := false;
  TimerTick := 0;
  InHeader := true;

end; // AfterMail

procedure TForm_Mail.ShowException( Sender : TObject; E : Exception );
begin
  Label_Status.Caption := STR_26 + ': ' + E.Message;
  if ( IsBusy and ( not( SmtpCli.State in [smtpReady,smtpAbort] ))) then
    SMTPCli.Abort;
  HadError := true;
end; // ShowException


procedure TForm_Mail.SmtpCliRequestDone(Sender: TObject;
  RqType: TSmtpRequest; Error: Word);
begin
  if ( Error <> 0 ) then
  begin
    if ( Error >= 1000 ) then
      Label_Status.Caption := STR_26 + ' ' + inttostr( Error ) +
        ' (' + WSocketErrorDesc( Error ) + ')'
    else
    if ( Error >= 400 ) then
      Label_Status.Caption := STR_26 + ' ' + inttostr( Error ) +
        ' (' + SMTPErrorDesc( Error ) + ')'
    else
      Label_Status.Caption := STR_28 + inttostr( Error );
    AfterMail;
    exit;
  end;

  case RqType of
    smtpOpen : begin
      Label_Status.Caption := STR_29;
      SmtpCli.Mail;
    end;
    smtpMail : begin
      Label_Status.Caption := STR_30;
      SMTPCli.Quit;
    end;
    smtpQuit : begin
      Label_Status.Caption := STR_31;
      Aftermail;
    end;

  end;

end;

procedure TForm_Mail.TimerTimer(Sender: TObject);
begin
  if ( not IsBusy ) then exit;
  inc( TimerTick );
  if TimerTick >= MailOptions.TimeOut then
  begin
    if ( IsBusy and ( not( SmtpCli.State in [smtpReady,smtpAbort] ))) then
      SMTPCli.Abort;
    Label_Status.Caption := STR_32;
    AfterMail;
  end;
end;

procedure TForm_Mail.SmtpCliDisplay(Sender: TObject; Msg: String);
begin
  if ( not InHeader ) then exit;
  GFLog.Add( Msg );
  if ( length( Msg ) <= 2 ) then
    InHeader := false;
end;

procedure TForm_Mail.RB_FileClick(Sender: TObject);
begin
  if ( RB_all.Checked and RB_RTF.Checked ) then
  begin
    if ( assigned( myNotes ) and myNotes.HasExtendedNotes ) then
    begin
      Label_Status.Caption := STR_33;
    end
    else
    begin
      Label_Status.Caption := STR_34
    end;
  end
  else
  begin
    Label_Status.Caption := STR_34
  end;
end;



procedure TForm_Mail.Button_HelpClick(Sender: TObject);
begin
  Application.HelpCommand( HELP_CONTEXT, self.HelpContext );
end;

end.
