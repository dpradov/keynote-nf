
KeyNote NF uses several third party components. I have created a Packed to easily
install most of them (Components.dpk). The rest must be installed separately:

1. Tnt Unicode Controls  (TntUnicodeVcl)  
    => Compile: 3rd_party\TntUnicodeControls\Delphi\bds4\TntUnicodeVcl.dpk  
    => Install: 3rd_party\TntUnicodeControls\Delphi\bds4\TntUnicodeVcl_Design.dpk  

2. Most 3rd Party Components  
    => Install: Packages\Components.dpk

3. DCPcrypt v1.3  
    => Install: 3rd_party\dcpcrypt-1_3\DCP_d4.dpk

4. RX Library 2.75 port to Delphi 2006 (Win32), v1.0  
    => Compile: 3rd_party\rx275d2006\Units\rxctl2006.dpk    
    => Install: 3rd_party\rx275d2006\Units\dclrx2006.dpk

   
More information in [README_SourceCode](..\README_SourceCode.txt)