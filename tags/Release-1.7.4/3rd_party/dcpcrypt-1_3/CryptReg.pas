{******************************************************************************}
{** Registration unit for the DCP cryptographic components ********************}
{******************************************************************************}
{** Written by David Barton (davebarton@bigfoot.com) **************************}
{** http://www.scramdisk.clara.net/ *******************************************}
{******************************************************************************}
unit CryptReg;

interface
{$I DCPcrypt.inc}
uses
  {$IFDEF CFORM}Classes,{$ENDIF}
  DCPcrypt, Blowfish, Cast128, Cast256, Gost, Haval, IDEA, Mars, Misty1, RC2,
  RC5, RC6, Rijndael, Rmd160, SHA1, Twofish;

{$IFDEF CFORM}
procedure Register;
{$ENDIF}

{******************************************************************************}
{******************************************************************************}
implementation

{$IFDEF CFORM}
{$IFDEF WIN32}
{$R Dcr32\Blowfish.dcr}
{$R Dcr32\Cast128.dcr}
{$R Dcr32\Cast256.dcr}
{$R Dcr32\Gost.dcr}
{$R Dcr32\Haval.dcr}
{$R Dcr32\IDEA.dcr}
{$R Dcr32\Mars.dcr}
{$R Dcr32\Misty1.dcr}
{$R Dcr32\RC2.dcr}
{$R Dcr32\RC5.dcr}
{$R Dcr32\RC6.dcr}
{$R Dcr32\Rijndael.dcr}
{$R Dcr32\Rmd160.dcr}
{$R Dcr32\SHA1.dcr}
{$R Dcr32\Twofish.dcr}
{$ELSE}
{$R Dcr16\Blowfish.dcr}
{$R Dcr16\Cast128.dcr}
{$R Dcr16\Cast256.dcr}
{$R Dcr16\Gost.dcr}
{$R Dcr16\Haval.dcr}
{$R Dcr16\IDEA.dcr}
{$R Dcr16\Mars.dcr}
{$R Dcr16\Misty1.dcr}
{$R Dcr16\RC2.dcr}
{$R Dcr16\RC5.dcr}
{$R Dcr16\RC6.dcr}
{$R Dcr16\Rijndael.dcr}
{$R Dcr16\Rmd160.dcr}
{$R Dcr16\SHA1.dcr}
{$R Dcr16\Twofish.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents(DCPpage,[TDCP_blowfish, TDCP_cast128, TDCP_cast256,
    TDCP_gost, TDCP_idea, TDCP_mars, TDCP_misty1, TDCP_rc2, TDCP_rc5, TDCP_rc6,
    TDCP_rijndael, TDCP_twofish]);
  RegisterComponents(DCPpage,[TDCP_haval, TDCP_rmd160, TDCP_sha1]);
end;

{$ENDIF}

end.
