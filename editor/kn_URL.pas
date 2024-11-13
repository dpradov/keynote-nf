unit kn_URL;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)

interface

uses
   System.SysUtils,
   System.Classes,
   System.AnsiStrings,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   kn_Info
   ;


type
  TForm_URLAction = class(TForm)
    Button_Copy: TButton;
    Button_Cancel: TButton;
    Label1: TLabel;
    Button_Open: TButton;
    Button_OpenNew: TButton;
    Edit_URL: TEdit;
    Label2: TLabel;
    Edit_TextURL: TEdit;
    Button_Modify: TButton;
    procedure CheckURL(OnlyEnsureLink: boolean; const DecodedURL: string);
    procedure Edit_URLExit(Sender: TObject);
    procedure Button_ModifyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button_CopyClick(Sender: TObject);
    procedure Button_OpenClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
    procedure Button_OpenNewClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
    fNoDecodeURL: boolean;
  public
    { Public declarations }
    URLAction : TURLAction;
    AllowURLModification: boolean;      // URL, not the text associated
  end;


function FileNameToURL( fn : string ) : string;
function HTTPDecode(const AStr: string): string;
function HTTPEncode(const AStr: string): string;

function StripFileURLPrefix( const AStr : string ) : string;

implementation
uses
   gf_strings,
   gf_misc,
   RxRichEd,
   kn_const,
   kn_Global,
   kn_LinksMng,
   knt.App,
   knt.RS
   ;


{$R *.DFM}


function FileNameToURL( fn : string ) : string;
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


function HTTPDecode(const AStr: string): string;
// source: Borland Delphi 5
var
  Sp, Rp, Cp: PChar;
begin
  try
      SetLength(Result, Length(AStr));
      Sp := PChar(AStr);
      Rp := PChar(Result);
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
              Rp^ := Char(Chr(StrToInt(Format('$%s%s',[Cp^, Sp^]))));
            end;
          end;
        Inc(Rp);
        Inc(Sp);
      end;
      SetLength(Result, Rp - PChar(Result));

  except
      Result:= AStr;   // It will be assumed that the filename includes plus sign but it is not URL encoded.
  end;
end;

function HTTPEncode(const AStr: string): string;
// source: Borland Delphi 5, **modified**
const
  NoConversion = ['A'..'Z','a'..'z','*','@','.','_','-', '/', '?',
                  '0'..'9','$','!','''','(',')'];
var
  Sp, Rp: PChar;
begin
  SetLength(Result, Length(AStr) * 3);
  Sp := PChar(AStr);
  Rp := PChar(Result);
  while Sp^ <> #0 do
  begin
    if Sp^ in NoConversion then
      Rp^ := Sp^
    else
      begin
        System.AnsiStrings.FormatBuf(Rp^, 3, '%%%.2x', 6, [Ord(Sp^)]);
        Inc(Rp,2);
      end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PChar(Result));
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
  fNoDecodeURL:= false;
end;

function TForm_URLAction.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

procedure TForm_URLAction.Button_CopyClick(Sender: TObject);
begin
  URLAction := urlCopy;
end;

procedure TForm_URLAction.Button_ModifyClick(Sender: TObject);
begin
   Edit_URLExit (nil);
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

procedure TForm_URLAction.FormKeyPress(Sender: TObject; var Key: Char);
begin
    if not Button_Modify.Default then begin
       Button_Modify.Default := true;
       Button_Open.Default := false;
    end;

end;

procedure TForm_URLAction.FormShow(Sender: TObject);
begin

   // Look to the default, initial action
     if URLAction = urlCreateOrModify then begin
        Button_Copy.Visible := false;
        Button_Open.Visible := false;
        Button_OpenNew.Visible := false;
        Button_Modify.Caption := sURL01;
        Button_Modify.Default := true;
        Caption:= sURL02;
     end
     else begin
        Button_Copy.Visible := true;
        Button_Open.Visible := true;
        Button_OpenNew.Visible := true;
        Button_Modify.Caption := sURL03;
        Button_Open.Default := true;
        Caption:= sURL04;
     end;

      if AllowURLModification then begin
        Edit_URL.ReadOnly:= False;
        if not Edit_TextURL.Enabled then
           Edit_URL.SelectAll;
      end
      else begin
        Edit_URL.ReadOnly:= True;
        Edit_TextURL.SetFocus;
        Edit_TextURL.SelectAll;
      end;

      if (Edit_URL.Text = '') or (not Edit_TextURL.Enabled) then
          Edit_URL.SetFocus
      else
          Edit_TextURL.SetFocus;

end;

// KEY DOWN


procedure TForm_URLAction.Button_OpenNewClick(Sender: TObject);
begin
  URLAction := urlOpenNew;
end;

{
 Revisa el campo TextURL, igualándolo al campo URL si está vacío
 OnlyEnsureLink: Si True, sólo modifica el campo si no es posible dejar únicamente el campo
    URL sin texto asociado (vía HYPERLINK "...) al tratarse de tipo de URL no reconocido por RichEdit o
    tener algún problema con su último carácter.
}
procedure TForm_URLAction.CheckURL(OnlyEnsureLink: boolean; const DecodedURL: string);
var
  url: string;
  InterpretedUrl: string;
  KNTlocation, EnsureLink: boolean;
  URLType: TKNTURL;
  lastChar: Char;
begin
 if not Edit_TextURL.Enabled then exit;

 if (Edit_TextURL.Text = '') and not ActiveEditor.PlainText then begin
    EnsureLink:= False;
    url:= DecodedURL;

     url:= trim(url);
     if url='' then exit;

     InterpretedUrl:= url;
     URLType:= TypeURL( InterpretedUrl, KNTlocation);
     lastChar:= url[length(url)];
     if (URLType = urlOTHER) or (( URLType in [urlHTTP, urlHTTPS]) and ( lastChar in [')', ']'] ))  then begin
         // Si el texto es igual a la URL, y no es de los reconocidos por el control RichEdit (http://msdn.microsoft.com/en-us/library/windows/desktop/bb787991%28v=vs.85%29.aspx)
         // no lo tratará como hiperlenlace
         // También deja sin reconocer el último carácter si éste es ) o ], como mínimo
        url:= '<' + url + '>';
        EnsureLink:= True;
     end
     else
         if url <> interpretedUrl then begin
            Edit_URL.Text:= interpretedUrl;
         end;

    if (Not OnlyEnsureLink) or (EnsureLink= True) then
       Edit_TextURL.Text:= url;
 end;
end;

procedure TForm_URLAction.Edit_URLExit(Sender: TObject);
var
  URL: string;
  KNTlocation: boolean;
  URLType: TKNTURL;

begin
  if ShiftDown then
     fNoDecodeURL:= true;

  URL:= Edit_URL.Text;
  if Edit_TextURL.Text = URL then
     Edit_TextURL.Text:= '';

  URLType:= TypeURL(URL, KNTlocation);
  case URLType of
     urlHTTP, urlHTTPS: URL:= DecodeURLWebUTF8Characters(URL);
  end;

  if KeyOptions.URLWebDecode and not fNoDecodeURL then
     Edit_URL.Text:= URL;

  CheckURL(false, URL);
end;


function StripFileURLPrefix( const AStr : string ) : string;
const
  FILEPREFIX = 'file:';
var
  l, i, n: integer;
begin
  // In URLs like file://192.168.1.1/..... we must preserve the prefix
  // We can eliminate it in URLs like file:///C:.....
  result := AStr;

  if ( pos( FILEPREFIX, LowerCase( result )) = 1 ) then
  begin
    i:= length( FILEPREFIX)+1;
    l:= length(result);
    n:= 0;
    while (i <= l) and ( result[i] = '/' ) do begin
        i:= i + 1;
        n:= n + 1;
    end;

    if (n <> 2) or ( ( l>i+1 ) and (result[i+1]=':')) then
       delete( result, 1, i-1);
  end;
end; // StripFileURLPrefix

end.
