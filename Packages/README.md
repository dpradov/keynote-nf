
KeyNote NF uses several third party components. I have created a Packed to easily
install most of them (Components.dpk). The rest must be installed separately:

1 - Most 3rd Party Components
   => Install: Packages\Components.dpk

2 - DCPcrypt v1.3
   => Install: 3rd_party\dcpcrypt-1_3\DCP_d4.dpk

3 - unRxLib: Unofficial version Rx library for Delphi 2005 .. Alexandria   (v1.21)    
   => Build:   3rd_party\unRxLib\units\packages_D11_Alexandria\RtmRxCtl280.dproj
   => Install: 3rd_party\unRxLib\units\packages_D11_Alexandria\DclRxCtl280.dproj

4 - ICS-Internet Component Suite v8.70
   Unzip the file 3rd_party\ICS_v8\_icsv870.zip in some location (e.g. "<somePath>\icsv870)")
   For Delphi 11:
   => Open icsv870\Install\D110Install.groupproj           ( VCL only, no FireMonkey components )
   => Build:    icsv870\Packages\OverbyteIcsD110Run.dproj
   => Install:  icsv870\Packages\OverbyteIcsD110Design.dproj     
   => Add "<somePath>\icsv870\source" and "<somePath>\icsv870\source\include" to Library path:
        Options -> Language -> Delphi -> Library -> Library path     
      
   Notes:
   - Detailed instructions of installation in icsv870\ReadMe8.txt
   - In 3rd_party\ICS_v8\_Readme.txt in indicated the URL where you can download that file or a newer version
   - It is also possible to install the component using GetIt (Tools -> GetIt Package Manager...)
       => "ICS for FMX and VCL for Delphi 8.70, by Overbyte"
          "ICS for VCL for Delphi 8.70, by Overbyte"
          
        Once installed (first one, e.g.) the following configuration will be defined in  Options -> Language -> Delphi -> Library -> Library path:
            $(BDSCatalogRepository)\ICS_FMX-8.70-11\source\
            $(BDSCatalogRepository)\ICS_FMX-8.70-11\source\include

   
More information in [README_SourceCode](..\README_SourceCode.txt)