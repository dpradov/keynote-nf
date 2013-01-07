unit Unit_test;

// A Zlib little example by Roberto Della Pasqua, www.dellapasqua.com  C.Ott2002
// I hope to be util for the people working for the first time with streams, strings, and zlib
// Use the same techniques for decompressing streams too...
// Use ZFastCompressString and ZFastDecompressString for little strings (avoid using streams)
// Example: MyString it's the variable -> ZFastCompressString(MyString,zcDefault) -> now MyString it's compressed
// Use ZSendToBrowser to send compressed text to browsers http1.1 compliant
// Have fun!

interface

uses
  Windows,
  Messages,
  Classes,
  Forms,
  SysUtils,
  Dialogs,
  StdCtrls,
  Controls,
  ZlibEx;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Compress: TButton;
    OpenDialog1: TOpenDialog;
    Open: TButton;
    Browser: TButton;
    procedure OpenClick(Sender: TObject);
    procedure CompressClick(Sender: TObject);
    procedure BrowserClick(Sender: TObject);
  private
    InBuffer: string;
    OutBuffer: string;
    CompressedStream: TZCompressionStream;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.OpenClick(Sender: TObject);
var
  MyStream: TMemoryStream;
begin
  if OpenDialog1.Execute then
    with OpenDialog1.Files do
    begin
      MyStream := TMemoryStream.Create;
      MyStream.LoadFromFile(Strings[0]);  // load the text file from the stringlist of the dialog
      SetString(InBuffer, PChar(MyStream.Memory), MyStream.Size); // setstring the stream memory -> InBuffer
      MyStream.Free;
    end;
  Memo1.Text := InBuffer; // copy the InBuffer to Memo for example, remember: for good speed avoid useless copies of mem
end;

procedure TForm1.CompressClick(Sender: TObject);
var
  MyStream: TMemoryStream;
begin
  MyStream := TMemoryStream.Create;
  CompressedStream := TZCompressionStream.Create(MyStream, zcDefault); // create the compression stream
  CompressedStream.Write(pointer(InBuffer)^, Length(InBuffer)); // move and compress the InBuffer string -> destination stream  (MyStream)
  CompressedStream.Free;
  SetString(OutBuffer, PChar(MyStream.Memory), MyStream.Size);
  Memo2.text := OutBuffer;
  MyStream.Free;
end;

procedure TForm1.BrowserClick(Sender: TObject);
begin
ZSendToBrowser(InBuffer);
Memo2.text:=InBuffer;  // instead of this can be: ECB.WriteClient(Pchar(InBuffer)...etc... or similar webserver put routine
end;

end.

