unit kn_URL;

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
 <marekjed@pobox.com> (Poland).
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


interface

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls,
  registry, gf_misc, kn_Info, kn_const, TntStdCtrls;

type
  TForm_URLAction = class(TForm)
    Button_Copy: TTntButton;
    Button_Cancel: TTntButton;
    Label1: TTntLabel;
    Button_Open: TTntButton;
    Button_OpenNew: TTntButton;
    Edit_URL: TTntEdit;
    Label2: TTntLabel;
    Edit_TextURL: TTntEdit;
    Button_Modify: TTntButton;
    procedure Edit_URLExit(Sender: TObject);
    procedure Button_ModifyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button_CopyClick(Sender: TObject);
    procedure Button_OpenClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button_OpenNewClick(Sender: TObject);
    procedure Label_URLClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    URLAction : TURLAction;
    AllowURLModification: boolean;      // URL, not the text associated
  end;


function FileNameToURL( fn : wideString ) : wideString;
function HTTPDecode(const AStr: wideString): wideString;
function HTTPEncode(const AStr: wideString): wideString;

function StripFileURLPrefix( const AStr : wideString ) : wideString;

implementation
uses
  RxRichEd, kn_Global, kn_LinksMng;

{$R *.DFM}

resourcestring
  STR_01 = 'OK';
  STR_02 = 'Create Hyperlink';
  STR_03 = 'Modify';
  STR_04 = 'Choose Action for Hyperlink';
  STR_05 = '(KNT Location)';


function FileNameToURL( fn : wideString ) : wideString;
var
  i : integer;
begin
  result := '';
  for i := 1 to length( fn ) do
  begin
    if  ( fn[i] in [' ', '%', '|'] ) then
    begin
      result := result + '%' + IntToHex( ord( fn[i] ), 2 );
    end
    else
    begin
      result := result + fn[i];
    end;
  end;
end; // FileNameToURL


function HTTPDecode(const AStr: wideString): wideString;
// source: Borland Delphi 5
var
  Sp, Rp, Cp: PWideChar;
begin
  try
      SetLength(Result, Length(AStr));
      Sp := PWideChar(AStr);
      Rp := PWideChar(Result);
      while Sp^ <> #0 do
      begin
        if not (Sp^ in ['+','%']) then
          Rp^ := Sp^
        else
          begin
            inc(Sp);
            if Sp^ = '%' then
              Rp^ := '%'
            else
            begin
              Cp := Sp;
              Inc(Sp);
              Rp^ := WideChar(Chr(StrToInt(WideFormat('$%s%s',[Cp^, Sp^]))));
            end;
          end;
        Inc(Rp);
        Inc(Sp);
      end;
      SetLength(Result, Rp - PWideChar(Result));

  except
      Result:= AStr;   // It will be assumed that the filename includes plus sign but it is not URL encoded.
  end;
end;

function HTTPEncode(const AStr: wideString): wideString;
// source: Borland Delphi 5, **modified**
const
  NoConversion = ['A'..'Z','a'..'z','*','@','.','_','-', '/', '?',
                  '0'..'9','$','!','''','(',')'];
var
  Sp, Rp: PWideChar;
begin
  SetLength(Result, Length(AStr) * 3);
  Sp := PWideChar(AStr);
  Rp := PWideChar(Result);
  while Sp^ <> #0 do
  begin
    if Sp^ in NoConversion then
      Rp^ := Sp^
    else
      begin
        FormatBuf(Rp^, 3, '%%%.2x', 6, [Ord(Sp^)]);
        Inc(Rp,2);
      end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PWideChar(Result));
end;



procedure TForm_URLAction.FormCreate(Sender: TObject);
begin
  URLAction := low( urlOpen );
  // Label_URL.Font.Color := clBlue;
  // Label_URL.Font.Style := [fsUnderline];
  Edit_URL.Font.Name := 'Tahoma';
  // Edit_URL.Font.Style := [fsBold];
  if ( Edit_URL.Font.Size < 10 ) then
      Edit_URL.Font.Size := 10;

  Edit_TextURL.Font.Name := 'Tahoma';
  if ( Edit_TextURL.Font.Size < 10 ) then
      Edit_TextURL.Font.Size := 10;

  if RichEditVersion < 4 then begin
    Edit_TextURL.Enabled := false;
    label2.Enabled := false;
  end;
  AllowURLModification:= True;
end;

procedure TForm_URLAction.Button_CopyClick(Sender: TObject);
begin
  URLAction := urlCopy;
end;

procedure TForm_URLAction.Button_ModifyClick(Sender: TObject);
begin
     if Edit_TextURL.Text = '' then
        Edit_URLExit(nil);
     URLAction := urlCreateOrModify;
end;

procedure TForm_URLAction.Button_OpenClick(Sender: TObject);
begin
  URLAction := urlOpen;
end;

procedure TForm_URLAction.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if ( shift = [] ) then
    begin
      key := 0;
      Close;
    end;
  end;
end;

procedure TForm_URLAction.FormShow(Sender: TObject);
begin

   // Look to the default, initial action
     if URLAction = urlCreateOrModify then begin
        Button_Copy.Visible := false;
        Button_Open.Visible := false;
        Button_OpenNew.Visible := false;
        Button_Modify.Caption := STR_01;
        Button_Modify.Default := true;
        Caption:= STR_02;
     end
     else begin
        Button_Copy.Visible := true;
        Button_Open.Visible := true;
        Button_OpenNew.Visible := true;
        Button_Modify.Caption := STR_03;
        Button_Open.Default := true;
        Caption:= STR_04;
     end;

      if AllowURLModification then begin
        Edit_URL.ReadOnly:= False;
        Edit_URL.SetFocus;
        Edit_URL.SelectAll;
      end
      else begin
        Edit_URL.Text:= STR_05 + ' ' + Edit_URL.Text;
        Edit_URL.ReadOnly:= True;
        Edit_TextURL.SetFocus;
        Edit_TextURL.SelectAll;
      end;

      if (RichEditVersion < 4) or (ActiveNote.PlainText) then begin
        Edit_TextURL.Text:= '';
        Label2.Enabled:= false;
        Edit_TextURL.Enabled:= false;
        if not AllowURLModification then Button_Modify.Enabled:= false;
      end;

end;

// KEY DOWN


procedure TForm_URLAction.Button_OpenNewClick(Sender: TObject);
begin
  URLAction := urlOpenNew;
end;


procedure TForm_URLAction.Edit_URLExit(Sender: TObject);
var
  url: wideString;
  InterpretedUrl: wideString;
  KNTlocation: boolean;
begin
 if not Edit_TextURL.Enabled then exit;

 if Edit_TextURL.Text = '' then begin
    url:= Edit_URL.Text;
    if ( pos(STR_05, url) = 1 ) then
        delete( url, 1, length( STR_05 ));

     url:= trim(url);
     InterpretedUrl:= url;
     if TypeURL( InterpretedUrl, KNTlocation) = urlOTHER then
         // Si el texto es igual a la URL, y no es de los reconocidos por el control RichEdit (http://msdn.microsoft.com/en-us/library/windows/desktop/bb787991%28v=vs.85%29.aspx)
         // no lo tratará como hiperlenlace
        url:= '<' + url + '>'
     else
         if url <> interpretedUrl then begin
            Edit_URL.Text:= interpretedUrl;
         end;

    Edit_TextURL.Text:= url;
 end;
end;

procedure TForm_URLAction.Label_URLClick(Sender: TObject);
begin
  if ShiftDown then
    URLAction := urlOpenNew
  else
    URLAction := urlOpen;
  ModalResult := mrOK;
end;

function StripFileURLPrefix( const AStr : WideString ) : wideString;
const
  FILEPREFIX = 'file:';
begin
  result := AStr;
  if ( pos( FILEPREFIX, wideLowerCase( result )) = 1 ) then
  begin
    delete( result, 1, length( FILEPREFIX ));
    while ( result <> '' ) and ( result[1] = '/' ) do
      delete( result, 1, 1 );
  end;
end; // StripFileURLPrefix

end.
