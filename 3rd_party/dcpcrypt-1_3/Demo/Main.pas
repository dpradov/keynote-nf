unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, SHA1, DCPcrypt, RMD160, IDEA, Misty1, Twofish,
  Rijndael, Gost, Cast128, Blowfish, Haval, ExtCtrls;

type
  TMainFrm = class(TForm)
    CryptGrp: TGroupBox;
    CInFileBox: TEdit;
    CInBrowseBtn: TSpeedButton;
    CAlgorithmCBx: TComboBox;
    CInFileLbl: TLabel;
    COutFileLbl: TLabel;
    COutFileBox: TEdit;
    COutBrowseBtn: TSpeedButton;
    CAlgorithmLbl: TLabel;
    CPassphraseLbl: TLabel;
    CPassphraseBox: TEdit;
    CEncryptBtn: TButton;
    CDecryptBtn: TButton;
    HashGrp: TGroupBox;
    HInFileBox: TEdit;
    HInBrowseBtn: TSpeedButton;
    HInFileLbl: TLabel;
    HAlgorithmLbl: TLabel;
    HAlgorithmCBx: TComboBox;
    HOutputLbl: TLabel;
    HOutputBox: TEdit;
    HHashBtn: TButton;
    WebLbl: TLabel;
    WrittenLbl: TLabel;
    EmailLbl: TLabel;
    DCP_blowfish1: TDCP_blowfish;
    DCP_cast1281: TDCP_cast128;
    DCP_gost1: TDCP_gost;
    DCP_rijndael1: TDCP_rijndael;
    DCP_twofish1: TDCP_twofish;
    DCP_misty11: TDCP_misty1;
    DCP_idea1: TDCP_idea;
    DCP_rmd1601: TDCP_rmd160;
    DCP_sha11: TDCP_sha1;
    OpenDlg: TOpenDialog;
    SaveDlg: TSaveDialog;
    DCP_haval1: TDCP_haval;
    Label1: TLabel;
    Bevel1: TBevel;
    CEncryptStrBtn: TButton;
    CDecryptStrBtn: TButton;
    CStrings: TMemo;
    procedure EmailLblClick(Sender: TObject);
    procedure WebLblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CInBrowseBtnClick(Sender: TObject);
    procedure COutBrowseBtnClick(Sender: TObject);
    procedure HInBrowseBtnClick(Sender: TObject);
    procedure CEncryptBtnClick(Sender: TObject);
    procedure CDecryptBtnClick(Sender: TObject);
    procedure HHashBtnClick(Sender: TObject);
    procedure CEncryptStrBtnClick(Sender: TObject);
    procedure CDecryptStrBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation
uses
  ShellApi, Base64;

{$R *.DFM}

function CompareMem(I1, I2: PByte; Size: integer): boolean;
begin
  Result:= true;
  repeat
    if I1^<> I2^ then
    begin
      Result:= false;
      Exit;
    end;
    Inc(I1);
    Inc(I2);
    Dec(Size);
  until Size= 0;
end;

procedure TMainFrm.EmailLblClick(Sender: TObject);
begin
  ShellExecute(Handle,'Open','mailto:davebarton@bigfoot.com',nil,nil,SW_SHOWNORMAL);
    // email me!
end;

procedure TMainFrm.WebLblClick(Sender: TObject);
begin
  ShellExecute(Handle,'Open','http://www.scramdisk.clara.net/',nil,nil,SW_SHOWNORMAL);
    // visit Sam Simpson's Scramdisk page and my DCP page
end;

procedure TMainFrm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i:= 0 to ComponentCount-1 do         // cycle through all the form components add the
  begin                                    // appropriate components to the combo boxes
    if Components[i] is TDCP_blockcipher then
      CAlgorithmCBx.Items.AddObject(TDCP_blockcipher(Components[i]).Algorithm,Components[i])
    else if Components[i] is TDCP_hash then
      HAlgorithmCBx.Items.AddObject(TDCP_hash(Components[i]).Algorithm,Components[i]);
  end;
  CAlgorithmCBx.ItemIndex:= 0;            // set the first item as the default
  HAlgorithmCBx.ItemIndex:= 0;
end;

procedure TMainFrm.CInBrowseBtnClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    CInFileBox.Text:= OpenDlg.Filename;
end;

procedure TMainFrm.COutBrowseBtnClick(Sender: TObject);
begin
  if SaveDlg.Execute then
    COutFileBox.Text:= SaveDlg.Filename;
end;

procedure TMainFrm.HInBrowseBtnClick(Sender: TObject);
begin
  if OpenDlg.Execute then
    HInFileBox.Text:= OpenDlg.Filename;
end;

procedure TMainFrm.CEncryptBtnClick(Sender: TObject);
{
  Open the files
  Hash the passphrase
  Init the cipher with the hash
  Encrypt the hash
  Write the hash out to dest file
  Encrypt the source file
  Write out to dest file
  Close the files
}
var
  Source, Dest: file;
  Buffer: array[0..8191] of byte;
  Hash: TDCP_sha1;
  HashDigest: array[0..31] of byte;
  Encrypt: TDCP_blockcipher;
  Read: integer;
begin
  if CInFileBox.Text= '' then
  begin
    MessageDlg('Please enter an in filename',mtInformation,[mbOK],0);
    CInFileBox.SetFocus;
    Exit;
  end
  else if COutFileBox.Text= '' then
  begin
    MessageDlg('Please enter an out filename',mtInformation,[mbOK],0);
    COutFileBox.SetFocus;
    Exit;
  end
  else if COutFileBox.Text= CInFileBox.Text then
  begin
    MessageDlg('The out filename must be different from the in filename',mtInformation,[mbOK],0);
    COutFileBox.SetFocus;
    COutFileBox.SelectAll;
    Exit;
  end;
  AssignFile(Source,CInFileBox.Text);
  try
    Reset(Source,1);
  except
    MessageDlg('Unable to open the in file',mtInformation,[mbOK],0);
    Exit;
  end;
  AssignFile(Dest,COutFileBox.Text);
  try
    Rewrite(Dest,1);
  except
    CloseFile(Source);
    MessageDlg('Unable to open then out file',mtInformation,[mbOK],0);
    Exit;
  end;
  FillChar(HashDigest,Sizeof(HashDigest),$FF);   // fill the digest with $FF as the actual digest may not fill the entire digest
  Hash:= TDCP_sha1.Create(Self);
  Hash.Init;
  Hash.UpdateStr(CPassphraseBox.Text);           // hash the passphrase to get the key
  Hash.Final(HashDigest);
  Hash.Free;
  Encrypt:= TDCP_blockcipher(CAlgorithmCBx.Items.Objects[CAlgorithmCBx.ItemIndex]);  // get the component from the combo box
  if (Sizeof(HashDigest)*8)> Encrypt.MaxKeySize then
    Encrypt.Init(HashDigest,Encrypt.MaxKeySize,nil)        // make sure the key isn't too big
  else
    Encrypt.Init(HashDigest,Sizeof(HashDigest)*8,nil);     // initialize the cipher with the digest, IV= nil to generate one automatically (note: size in bits ie. sizeof(x)*8)
  Encrypt.EncryptCBC(HashDigest,HashDigest,Sizeof(HashDigest));  // encrypt the hash and write it to the file to use as passphrase
  Encrypt.Reset;                                                 // confirmation
  BlockWrite(Dest,HashDigest,Sizeof(HashDigest));
  repeat
    BlockRead(Source,Buffer,Sizeof(Buffer),Read);
    Encrypt.EncryptCBC(Buffer,Buffer,Read);                      // read from the source encrypt and write out the result
    BlockWrite(Dest,Buffer,Read);
  until Read<> Sizeof(Buffer);
  Encrypt.Burn;                                                  // burn the key data (note: don't free it as we placed it on the form at design time)
  CloseFile(Source);
  CloseFile(Dest);
  MessageDlg('File encrypted',mtInformation,[mbOK],0);
end;

procedure TMainFrm.CDecryptBtnClick(Sender: TObject);
{
  Open the files
  Hash the passphrase
  Init the cipher with the hash
  Encrypt the hash
  Validate the passphrase by comparing the encrypted hash with encrypted hash at
    the start of the source file
  Decrypt the source file
  Write out to dest file
  Close the files
}
var
  Source, Dest: file;
  Buffer: array[0..8191] of byte;
  Hash: TDCP_sha1;
  HashDigest, HashRead: array[0..31] of byte;
  Decrypt: TDCP_blockcipher;
  Read: integer;
begin
  if CInFileBox.Text= '' then
  begin
    MessageDlg('Please enter an in filename',mtInformation,[mbOK],0);
    CInFileBox.SetFocus;
    Exit;
  end
  else if COutFileBox.Text= '' then
  begin
    MessageDlg('Please enter an out filename',mtInformation,[mbOK],0);
    COutFileBox.SetFocus;
    Exit;
  end
  else if COutFileBox.Text= CInFileBox.Text then
  begin
    MessageDlg('The out filename must be different from the in filename',mtInformation,[mbOK],0);
    COutFileBox.SetFocus;
    COutFileBox.SelectAll;
    Exit;
  end;
  AssignFile(Source,CInFileBox.Text);
  try
    Reset(Source,1);
  except
    MessageDlg('Unable to open the in file',mtInformation,[mbOK],0);
    Exit;
  end;
  AssignFile(Dest,COutFileBox.Text);
  try
    Rewrite(Dest,1);
  except
    CloseFile(Source);
    MessageDlg('Unable to open then out file',mtInformation,[mbOK],0);
    Exit;
  end;
  FillChar(HashDigest,Sizeof(HashDigest),$FF);         // fill the digest with $FF as the actual digest may not fill the entire digest
  Hash:= TDCP_sha1.Create(Self);
  Hash.Init;                                           // hash the passphrase to get the key
  Hash.UpdateStr(CPassphraseBox.Text);
  Hash.Final(HashDigest);
  Hash.Free;
  Decrypt:= TDCP_blockcipher(CAlgorithmCBx.Items.Objects[CAlgorithmCBx.ItemIndex]); // get the component from the combo box
  if (Sizeof(HashDigest)*8)> Decrypt.MaxKeySize then
    Decrypt.Init(HashDigest,Decrypt.MaxKeySize,nil)                    // make sure the key isn't too big
  else
  Decrypt.Init(HashDigest,Sizeof(HashDigest)*8,nil);                   // initialize the cipher with the digest, IV= nil to generate one automatically (note: size in bits ie. sizeof(x)*8)
  Decrypt.EncryptCBC(HashDigest,HashDigest,Sizeof(HashDigest));        // encrypt the hash to use as confirmation
  Decrypt.Reset;
  BlockRead(Source,HashRead,Sizeof(HashRead));                         // read the other hash from the file and compare it
  if not CompareMem(@HashRead,@HashDigest,Sizeof(HashRead)) then
  begin
    CloseFile(Source);
    CloseFile(Dest);
    Decrypt.Burn;
    MessageDlg('Incorrect passphrase',mtInformation,[mbOK],0);
    Exit;
  end;
  repeat
    BlockRead(Source,Buffer,Sizeof(Buffer),Read);
    Decrypt.DecryptCBC(Buffer,Buffer,Read);                            // read from the source decrypt and write out the result
    BlockWrite(Dest,Buffer,Read);
  until Read<> Sizeof(Buffer);
  Decrypt.Burn;                                                        // burn the key data (note: don't free it as we placed it on the form at design time)
  CloseFile(Source);
  CloseFile(Dest);
  MessageDlg('File decrypted',mtInformation,[mbOK],0);
end;

procedure TMainFrm.HHashBtnClick(Sender: TObject);
{
  Open the file
  Read into buffer
  Hash the buffer
  Repeat until all data read
  Close file
  Write out hash in hex
}
var
  Source: file;
  Buffer: array[0..8191] of byte;
  Read: integer;
  Hash: TDCP_hash;
  HashDigest: array[0..31] of byte;
  S: string;
begin
  if HInFileBox.Text= '' then
  begin
    MessageDlg('Please enter a in filename',mtInformation,[mbOK],0);
    CInFileBox.SetFocus;
    Exit;
  end;
  Hash:= TDCP_hash(HAlgorithmCBx.Items.Objects[HAlgorithmCBx.ItemIndex]);
  AssignFile(Source,HInFileBox.Text);
  try
    Reset(Source,1);
  except
    MessageDlg('Unable to open file',mtInformation,[mbOK],0);
    Exit;
  end;
  Hash.Init;                                            // initialize the hash
  repeat
    BlockRead(Source,Buffer,Sizeof(Buffer),Read);       // update the hash with the data
    Hash.Update(Buffer,Read);
  until Read<> Sizeof(Buffer);
  Hash.Final(HashDigest);                               // produce the hash
  CloseFile(Source);
  s:= '$';
  for Read:= 0 to ((Hash.HashSize div 8)-1) do          // write out in hex
    s:= s + IntToHex(HashDigest[Read],2);
  HOutputBox.Text:= s;
end;

procedure TMainFrm.CEncryptStrBtnClick(Sender: TObject);
var
  i: integer;
  Cipher: TDCP_blockcipher;
  s: string;
begin
  Cipher:= TDCP_blockcipher(CAlgorithmCBx.Items.Objects[CAlgorithmCBx.ItemIndex]);
  Cipher.InitStr(CPassphraseBox.Text);    // initialize the cipher with the key
  for i:= 0 to CStrings.Lines.Count-1 do
  begin
    s:= CStrings.Lines[i];
    Cipher.EncryptCFB(s[1],s[1],Length(s));  // encrypt all of the strings
    CStrings.Lines[i]:= B64Encode(s);        // Base64 encode the string to ensure all characters are printable
  end;
  Cipher.Reset;         // we are using CFB chaining mode so we must reset after each block of encrypted/decrypts
  Cipher.Burn;
end;

procedure TMainFrm.CDecryptStrBtnClick(Sender: TObject);
var
  i: integer;
  Cipher: TDCP_blockcipher;
  s: string;
begin
  Cipher:= TDCP_blockcipher(CAlgorithmCBx.Items.Objects[CAlgorithmCBx.ItemIndex]);
  Cipher.InitStr(CPassphraseBox.Text);    // initialize the cipher with the key
  for i:= 0 to CStrings.Lines.Count-1 do
  begin
    s:= B64Decode(CStrings.Lines[i]);        // decode the Base64 encoded string
    Cipher.DecryptCFB(S[1],S[1],Length(S));  // decrypt all of the strings
    CStrings.Lines[i]:= s;
  end;
  Cipher.Reset;            // we are using CFB chaining mode so we must reset after each block of encrypted/decrypts
  Cipher.Burn;
end;

end.
