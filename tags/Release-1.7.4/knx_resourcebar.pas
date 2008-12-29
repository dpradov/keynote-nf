unit knx_resourcebar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, cmpGFXListBox, ComCtrls, EnhListView, ExtListView, ComCtrls95,
  ExtCtrls, RxRichEd;

type
  TForm1 = class(TForm)
    Pages_Res: TPage95Control;
    Tab_ResSearch: TTab95Sheet;
    Tab_ResFav: TTab95Sheet;
    dfsExtListView1: TdfsExtListView;
    Tab_ResMacros: TTab95Sheet;
    GFXListBox1: TGFXListBox;
    Templates: TTab95Sheet;
    Scratchpad: TTab95Sheet;
    Plugins: TTab95Sheet;
    Panel1: TPanel;
    Btn_ResSearch: TButton;
    Btn_ResSearchOptions: TButton;
    Combo_ResSearch: TComboBox;
    Label1: TLabel;
    Notebook1: TNotebook;
    LV_ResSearch: TdfsExtListView;
    Button1: TButton;
    RxRichEdit1: TRxRichEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}





end.
