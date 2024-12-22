unit kn_UpdateVersion;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2024 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 
 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf
   
 *****************************************************************************) 


interface

uses
   Winapi.Windows,
   Winapi.Messages,
   Winapi.ShellAPI,
   System.SysUtils,
   System.DateUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.ExtCtrls,
   Vcl.Buttons
   ;

type
  TUpdateVersion = class(TForm)
    chkCheckUpd: TCheckBox;
    Button_OK: TButton;
    Label1: TLabel;
    Label2: TLabel;
    lblDonations: TLabel;
    lblVisitWeb: TLabel;
    lblInstalledVersion: TLabel;
    lblCurrentVersion: TLabel;
    lblStatus: TLabel;
    Label4: TLabel;
    txtChanges: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lblVisitWebMouseEnter(Sender: TObject);
    procedure lblVisitWebMouseLeave(Sender: TObject);
    procedure lblVisitWebClick(Sender: TObject);
    procedure lblDonationsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;

  private
    { Private declarations }
    procedure OpenURL(URL: String);
  public
    { Public declarations }
    OriginalCaptionText : string;
  end;

 function IsLaterVersion(const CurrentVersion, NewVersion: string): Boolean;
 function CheckForUpdate (ShowOnlyIfNewVersionToNotify: boolean): boolean;

var
  UpdateVersion: TUpdateVersion;


implementation
uses
   WinInet,
   gf_strings,
   kn_const,
   kn_Global,
   knt.App,
   knt.RS
   ;


{$R *.DFM}

var
 UV: TUpdateVersion;

function IsLaterVersion(const CurrentVersion, NewVersion: string): Boolean;
var
  i: Integer;
  StrsC, StrsL: TStrings;
begin
  Result:= false;
  if (CurrentVersion = NewVersion) or (NewVersion = '') or (CurrentVersion = '') then exit;

  try
     StrsC:= TStringList.Create;
     StrsL:= TStringList.Create;
     try
        SplitString(StrsC, CurrentVersion, '.', false);
        SplitString(StrsL, NewVersion, '.', false);
        for i := 0 to StrsC.Count - 1 do begin
           if StrToInt(StrsC[i]) < StrToInt(StrsL[i]) then
              Exit(true);
        end;
     finally
        StrsC.Free;
        StrsL.Free;
     end;
  except
  end;
end;


function NotifyVersionUpdate(const CurrentVersion, NewVersion: string; const IgnoreVersion: string= ''): Boolean;
begin
  Result:= false;

  if IsLaterVersion(CurrentVersion, NewVersion) then begin
     if IgnoreVersion = '' then
        Exit (true)
     else
        Result:= IsLaterVersion(IgnoreVersion, NewVersion);
  end;

end;


function GetLatestVersionInfo (const LastVersionInformed: string;
                                 var CurrentVersion: string; var Changes: string;
                                 ForceGetChanges: boolean;
                                 var WithoutInternetAccess: boolean): boolean;
var
  hInet, hConnect: HINTERNET;
  bytesRead: Cardinal;
  buffer: array [0..500] of AnsiChar;
  FoundLogInstalledVersion: boolean;

  function GetCurrentVersion: string;
  var
     p1, p2:integer;
     str: string;
  begin
      // "html_url": "https://github.com/dpradov/keynote-nf/releases/tag/v1.8.5.1",
      // "tag_name": "v1.8.5.1"    (it would need to 1600 bytes approx. to get that tag)
      p1:= Pos('/tag/v', buffer);
      if p1 > 0 then begin
        p2:= Pos('",', buffer, p1);
        Result:= Copy(buffer, p1+6, p2-p1-6);
      end;
  end;

  procedure ExtractHistoryChanges;
  var
     p1, p2: integer;
  begin
     p1:= Pos('v ' + CurrentVersion, Changes);
     p2:= Pos('v ' + Program_Version_Number, Changes);
     if p2 > 0 then
        Changes:= Copy(Changes, p1, p2-p1)
     else
        Delete(Changes,1, p1-1);
  end;


begin
  Result := false;
  CurrentVersion:='';
  Changes:= '';
  FoundLogInstalledVersion:= false;
  WithoutInternetAccess:= true;

  hInet := InternetOpen('KeyNoteNF_Updater', INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
  if Assigned(hInet) then begin
    try
      hConnect := InternetOpenUrl(hInet, PChar(Program_URL_API_LatestRelease), nil, 0, 0, 0);
      if Assigned(hConnect) then begin
        try
          if InternetReadFile(hConnect, @buffer, 450, bytesRead) then begin
              WithoutInternetAccess:= false;
              CurrentVersion:= GetCurrentVersion;
              Result:= NotifyVersionUpdate(Program_Version_Number, CurrentVersion, LastVersionInformed);
              if not Result and (not ForceGetChanges or (Program_Version_Number = CurrentVersion)) then
                 exit;
          end;
        finally
          InternetCloseHandle(hConnect);
        end;
      end;

      if WithoutInternetAccess then exit;

      hConnect := InternetOpenUrl(hInet, PChar(Program_URL_RawFiles + Program_URL_History_txt), nil, 0, 0, 0);
      if Assigned(hConnect) then begin
        try
           repeat
              if InternetReadFile(hConnect, @buffer, 500, bytesRead) and (bytesRead > 0)then begin
                 if bytesRead < 500 then
                    buffer[bytesRead]:= #0;
                 Changes:= Changes + AnsiString(buffer);
                 FoundLogInstalledVersion:= (Pos('v ' + Program_Version_Number, Changes) > 0);
              end
              else
                 break;
           until FoundLogInstalledVersion;
           ExtractHistoryChanges;
        finally
          InternetCloseHandle(hConnect);
        end;
      end;


    finally
      InternetCloseHandle(hInet);
    end;
  end;
end;


function CheckForUpdate (ShowOnlyIfNewVersionToNotify: boolean): boolean;
var
  CurrentVersion: string;
  Changes: string;
  newVersionToNotify: boolean;
  LastInformedVersion: string;
  WithoutInternetAccess: boolean;
begin
  result := false;
  if UV <> nil then exit;


  if ShowOnlyIfNewVersionToNotify and (KeyOptions.VersionLastChecked >= Today()) then
     exit;

  LastInformedVersion:= '';
  if ShowOnlyIfNewVersionToNotify then
     LastInformedVersion:= KeyOptions.LastInformedVersion;

  try
     KeyOptions.VersionLastChecked := Today();
     newVersionToNotify:= GetLatestVersionInfo (KeyOptions.LastInformedVersion, CurrentVersion, Changes, not ShowOnlyIfNewVersionToNotify, WithoutInternetAccess);

     if ShowOnlyIfNewVersionToNotify and not newVersionToNotify then
        exit;

     UV := TUpdateVersion.Create( Application );
     try
        UV.lblInstalledVersion.Caption:= Program_Version_Number;
        UV.lblCurrentVersion.Caption:= CurrentVersion;
        UV.txtChanges.Text:= Changes;
        if WithoutInternetAccess then
           UV.lblStatus.Caption:= GetRS(sUpd03)
        else begin
           if IsLaterVersion(Program_Version_Number, CurrentVersion) then begin
              UV.lblStatus.Caption:= GetRS(sUpd02);
              UV.lblStatus.Font.Color:= clRed;
           end
           else
              UV.lblStatus.Caption:= GetRS(sUpd01);
        end;

        UV.ShowModal;
        KeyOptions.LastInformedVersion:= CurrentVersion;
        KeyOptions.VersionLastChecked := Today();

     finally
       FreeAndNil(UV);
     end;

  except
    On E : Exception do
      messagedlg( E.Message, mtError, [mbOK], 0 );
  end;
end;


procedure TUpdateVersion.FormCreate(Sender: TObject);
begin
   chkCheckUpd.Checked:=  KeyOptions.CheckUpdOnStartup;
   lblVisitWeb.Hint:= Program_URL + Program_URL_LatestRelease;
   lblDonations.Hint:= Hint_Support;

   App.ApplyBiDiModeOnForm(Self);
end;

function TUpdateVersion.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

procedure TUpdateVersion.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   KeyOptions.CheckUpdOnStartup:= chkCheckUpd.Checked;
end;


procedure TUpdateVersion.Button_OKClick(Sender: TObject);
begin
  Close;
end;

procedure TUpdateVersion.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ( key = 27 ) and ( shift = [] ) then begin
    key := 0;
    Close;
  end;
end;

procedure TUpdateVersion.lblDonationsClick(Sender: TObject);
begin
  OpenURL (Program_URL + Program_URL_Donations);
end;

procedure TUpdateVersion.lblVisitWebClick(Sender: TObject);
begin
   OpenURL(Program_URL + Program_URL_LatestRelease);
end;

procedure TUpdateVersion.lblVisitWebMouseEnter(Sender: TObject);
begin
  ( sender as TLabel ).Font.Color := clRed;
end;

procedure TUpdateVersion.lblVisitWebMouseLeave(Sender: TObject);
begin
  ( sender as TLabel ).Font.Color := clBlue;
end;


procedure TUpdateVersion.OpenURL(URL: String);
begin
  screen.Cursor := crHourGlass;
  ShellExecute( 0, 'open', PChar( URL ), nil, nil, SW_NORMAL );
  screen.Cursor := crDefault;
end;

initialization
 UV:= nil;

end.
