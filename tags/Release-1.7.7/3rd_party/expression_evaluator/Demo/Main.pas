unit Main;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, Parser;

type
  TfrmMain = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Memo2: TMemo;
    edtFunction: TEdit;
    Label3: TLabel;
    MathParser: TMathParser;
    Label4: TLabel;
    Memo3: TMemo;
    Label5: TLabel;
    edtResult: TEdit;
    procedure BitBtn3Click(Sender: TObject);
    procedure MathParserParseError(Sender: TObject; ParseError: Integer);
    procedure BitBtn1Click(Sender: TObject);
    procedure MathParserGetVar(Sender: TObject; VarName: String;
      var Value: Extended; var Found: Boolean);
    procedure BitBtn2Click(Sender: TObject);
    procedure edtFunctionKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses About;

procedure TfrmMain.BitBtn3Click(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.MathParserParseError(Sender: TObject; ParseError: Integer);
var
  Msg : string;
begin
  case ParseError of
    1 : Msg := 'Parser stack overflow.';
    2 : Msg := 'Bad cell range.';
    3 : Msg := 'Expected expression.';
    4 : Msg := 'Expected operator.';
    5 : Msg := 'Expected opening parenthesis.';
    6 : Msg := 'Expected operator or closing parenthesis.';
    7 : Msg := 'Invalidad numeric expression.';
  end; { case }
  Msg := Msg + ' Position in string: ' + IntToStr(MathParser.Position);
  MessageDlg(Msg, mtError, [mbOk], 0);
  edtFunction.SelStart := Pred(MathParser.Position);
  edtFunction.SelLength := 0;
end;

procedure TfrmMain.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.MathParserGetVar(Sender: TObject; VarName: String;
  var Value: Extended; var Found: Boolean);
begin
 Found := True;
 VarName := Uppercase(VarName);
 if (VarName = 'DAYSPERWEEK') then
   Value := 7
 else if (VarName = 'SECONDSPERHOUR') then
   Value := 3600
 else if (VarName = 'AVGWEEKSPERMONTH') then
   Value := 4.333
 else
   Found := False;
end;

procedure TfrmMain.BitBtn2Click(Sender: TObject);
begin
  with MathParser do
  begin
    ParseString := edtFunction.Text;
    Parse;
    if ParseError then
      edtResult.Text := '#Error'
    else
      edtResult.Text := FloatToStrF(ParseValue, ffGeneral, 15, 2);
  end; { with }
end;

procedure TfrmMain.edtFunctionKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    BitBtn2Click(Sender);
  end;
end;

end.
