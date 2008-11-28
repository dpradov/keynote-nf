unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, TopWnd;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    TopMostWindow1: TTopMostWindow;
    Exit1: TMenuItem;
    Options1: TMenuItem;
    AlwaysonTop1: TMenuItem;
    procedure OnClickExit(Sender: TObject);
    procedure OnFormCreate(Sender: TObject);
    procedure OnClickAlwaysOnTop1(Sender: TObject);
    procedure OnAfterChangeTopMostWindow1(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.OnClickExit(Sender: TObject);
begin
  PostQuitMessage(0);
end;

procedure TForm1.OnFormCreate(Sender: TObject);
begin
  try
    AlwaysOnTop1.Checked := False;
    Self.Caption := 'I''m so sorry because I''m not a topmost window' +
     ' - please click <Options> and then <Always on Top>';
  finally
  end;
end;

procedure TForm1.OnClickAlwaysOnTop1(Sender: TObject);
begin
  try
    AlwaysOnTop1.Checked := not AlwaysOnTop1.Checked;
    if AlwaysOnTop1.Checked then TopMostWindow1.AlwaysOnTop := True
    else TopMostWindow1.AlwaysOnTop := False;
  finally
  end;
end;

procedure TForm1.OnAfterChangeTopMostWindow1(Sender: TObject);
begin
  try
    if (Sender as TTopMostWindow).AlwaysOnTop then
      Self.Caption := 'Now I''m feeling good, I''m a topmost window'
    else
      Self.Caption := 'What have you done? Put me immediately to the top!'; 
  finally
  end;
end;

end.
