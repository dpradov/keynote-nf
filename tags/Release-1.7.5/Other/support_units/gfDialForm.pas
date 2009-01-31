unit gfDialForm;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Animate, GIFCtrl;

const
  _DIALFORMTITLE = 'Call in progress';
  _SLEEPTIME = 750;     

const
  WM_DFSHOW = WM_USER+1024;
  WM_DFHIDE = WM_DFSHOW+1;
  WM_DFCALLINIT = WM_DFHIDE+1;
  WM_DFCALLPROG = WM_DFCALLINIT+1;
  WM_DFCONNECT = WM_DFCALLPROG+1;
  WM_DFCALLFAIL = WM_DFCONNECT+1;
  WM_DFCALLHUP  = WM_DFCALLFAIL+1;
  WM_DFCLEAR = WM_DFCALLHUP+1;
  WM_DFREPLYOK = WM_DFCLEAR+1;
  WM_DFDISCONNECT = WM_DFREPLYOK+1;
  WM_DFBUSY = WM_DFDISCONNECT+1;

type
  TForm_Dial = class(TForm)
    Button_Status: TButton;
    Label1: TLabel;
    Label_CallName: TLabel;
    Label2: TLabel;
    Label_CallNumber: TLabel;
    AniIcon: TRxGIFAnimator;
    Label_Status: TLabel;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);

  private
    { Private declarations }

    procedure WndProc( var M : TMessage ); override;

    procedure SetIconVisible( const AVisible : boolean );
    procedure SetIconAnimate( const aAnim : boolean );
    procedure SetStatusText( const AText : string );
    procedure SetCallNumber( const ANumber : string );
    procedure SetCallName( const AName : string );

  public
    { Public declarations }
    MessagesReceived : string;
    myAutoClose : boolean;
    property IconVisible : boolean write SetIconVisible;
    property IconAnimate : boolean write SetIconAnimate;
    property StatusText : string write SetStatusText;
    property CallNumber : string write SetCallNumber;
    property CallName : string write SetCallName;
  end;

var
  Form_Dial: TForm_Dial;

implementation

{$R *.DFM}

procedure TForm_Dial.FormCreate(Sender: TObject);
begin
  MessagesReceived := '';
  self.Visible := false;
  Caption := _DIALFORMTITLE;
  AniIcon.Visible := true;
  AniIcon.Animate := false;
end; // CREATE

procedure TForm_Dial.FormActivate(Sender: TObject);
begin
  Button_Status.Enabled := false;
end; // ACTIVATE

procedure TForm_Dial.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  // showmessage( MessagesReceived );
end; // CLOSE QUERY

procedure TForm_Dial.FormDestroy(Sender: TObject);
begin

end; // DESTROY

procedure TForm_Dial.FormShow(Sender: TObject);
begin

end; // SHOW

procedure TForm_Dial.FormHide(Sender: TObject);
begin

end; // HIDE

procedure TForm_Dial.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if ( shift = [] ) then
    begin
      key := 0;
      Close;
    end;
  end;
end; // KEY DOWN

procedure TForm_Dial.SetIconVisible( const AVisible : boolean );
begin
  AniIcon.Visible := AVisible;
end; // SetIconVisible

procedure TForm_Dial.SetIconAnimate( const aAnim : boolean );
begin
  AniIcon.Animate := aAnim;
end; // SetIconAnimate

procedure TForm_Dial.SetStatusText( const AText : string );
begin
  Label_Status.Caption := AText;
  Label_Status.Update;
  Button_Status.Enabled := true;
  // MessagesReceived := MessagesReceived + #13 + AText;
end; // SetStatusText

procedure TForm_Dial.SetCallNumber( const ANumber : string );
begin
  Label_CallNumber.Caption := ANUmber;
end; // SetCallNumber

procedure TForm_Dial.SetCallName( const AName : string );
begin
  Label_CallName.Caption := AName;
end; // SetCallName

procedure TForm_Dial.WndProc( var M : TMessage );
begin
  case M.Msg of
    WM_DFSHOW : begin
      self.Show;
    end;
    WM_DFHIDE : begin
      self.Hide;
    end;
    WM_DFCALLINIT : begin
      StatusText := 'Dialing...';
    end;
    WM_DFCALLPROG : begin
      StatusText := 'Call in progress.';
    end;
    WM_DFCALLFAIL : begin
      StatusText := 'Call failed.';
    end;
    WM_DFCALLHUP : begin
      StatusText := 'Hung up.'
    end;
    WM_DFCLEAR : begin
      StatusText := '';
      CallNumber := '';
      CallName := '';
    end;
    WM_DFCONNECT : begin
      StatusText := 'Connected.';
    end;
    WM_DFREPLYOK : begin
      StatusText := 'Line reply OK.';
    end;
    WM_DFDISCONNECT : begin
      StatusText := 'Disconnected';
      if myAutoClose then
      begin
        sleep( _SLEEPTIME );
        Close;
      end;
    end;
    WM_DFBUSY : begin
      StatusText := 'Line busy.';
      if myAutoClose then
      begin
        sleep( _SLEEPTIME );
        Close;
      end;
    end;
    else
    begin
      inherited WndProc( M );
    end;
  end;
end; // WndProc

end.
