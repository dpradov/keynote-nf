<p align="right"><sup>Thanks for using KeyNote NF. You can show your appreciation and support future development by <a href="https://github.com/dpradov/keynote-nf#donations">donating!</a></sup></p>

<p align="center">
<img width="650" src="https://github.com/dpradov/keynote-nf/blob/master/doc/Images/Img1.png">
<p/>


## KeyNote NF

Tabbed notebook with RichText editor, multi-level notes and strong encryption. 


This project is an evolution of Tranglos Keynote (of Marek Jedlinski), with new features like: 

 * *Unicode compliant*  

 * *Improvement in image management*
   * Support for GIF, PNG, JPG, TIF, BMP, WMF, EMF and ICO   
   * Several image storage options: EmbeddedRTF, EmbeddedKNT, External (Zip or Folder), ExternalAndEmbeddedKNT
   * Images can be saved in its own format (binary), and shown in RTF in pngblip and jpegblip, not only wmetafile8 or emfblip
   * Included internal image viewer (also possible to open with external viewer)
   * It is possible to change the visibility of the images in the file

   A detailed explanation of the image management changes is available in [Images_Readme.txt](doc/Images_Readme.txt)

 * *Alternative to .chm files*: can be used to generate help files for other applications
 
 * *Redesigned navigation history mechanism*

 * *New KeyNote file format: compressed*

 * *Improved Find All* <br>
   * Search is now much faster
   * Excerpts from the note of the matches found are displayed, where the searched words are highlighted
   * The treatment of the options 'All the words' and 'Any of the words' is extended
   * Added new option: "Current node and subtree"
   * Added new options to restrict search depending on checked status of nodes
   * Added new options to define search scope, allowing to restrict search to node names
   * Syntax for additional/advanced search

 * *Combined 'Tree panel' and 'Find All' filtering*

 * *Improved tree operations* 
 
 * *Improved treatment of links, and new internal KNT links*, vinculated to markers, not only to caret position
 
 * *Improved Clipboard Capture / Web Clip* 

 * *Better treatment of tables*  
 
 * *Improved Export* 
   * Added new tokens and options for note/node heading
   * Added new options for indenting nested nodes
 
 * *Checkboxes on children of selected nodes* <br>
   * Selecting checkboxes for all nodes (View/Tree Checkboxes -- now View/All nodes Checkboxes) is still posible. Besides, checkboxes can be shown only on children of selected nodes (Children Checkbox)
   * Added options in treeview context menu to hide/show child nodes based on checked status

 * *Hidden nodes* <br> Capacity to work with hidden nodes. Nodes can be hidden in two ways:
   * Activating a mode wich automatically hides checked nodes (Show or Hide checked nodes)
   * Filtering one note's nodes or all notes under a searching criterion (Filter Tree Note)

 * *Linked nodes*: share the content of a same note <br>
     
 * *New button: 'Copy Format'*

 * *Added 'KeyNote' as target format in File | Export*

 * *Alarms on nodes*  

 * *Added a new property, "Default Zoom"*, and new action: View | Alternative Margins

 * *Multilanguage support*  
<br>

### References, documentation and contact information
In [References.md](doc/References.md) you will get information about where to find documentation and help about KeyNote, and 
relevant addresses and contact information.
<br><br>

### Name. Compatibility
Since version 1.7.4 the program has been renamed from "KeyNote" to "KeyNote NF". That way I hope it will be easier to locate it searching the web.

To implement some of the new features included in KeyNote NF I had to slightly extend the format of KeyNote (.knt) files. All new elements are optional so that new version can open without problems a file corresponding to version 1.6.5.
Also, with KeyNote 1.6.5 it is possible to open a file created with a later version, but in this case, if the file is saved, the alarms and information about checked nodes will be lost.

_Important_: From version 1.7.7, certain metainformation is encoded in UTF-8 without BOM, not in ASCII. They are the corresponding to the following keys: NN, ND, VN, RV, VF, NA, EN  (note name, node name, etc. Fore more information: [fileformat.txt](doc/kn_fileformat/fileformat.txt)  
So, it is important to maintain a backup of the .knt file in older version if you are testing the new version.
<br>
### Backups
Not only because of possible compatibilty issues. It is always *strongly recommended* to conserve backups of .knt files.  
Currently KeyNote NF (as version 1.6.5 did) has an option to keep backup files. But don't forget that they are created when saving changes and that only last recent files (up to 9 max.) will be kept, in a cyclic way. If for some reason you save to file very frequently you might find that all the backup files are too recent (perhaps of the same day or few hours ago...). If you deleted something you shouldn't or the application did something wrong, you could not find a correct backup.  
So, you should always maintain custom copies (backup) of your files, perhaps one per day in the last week, for example.

From version 1.7.9 Beta 7, a new option have been added that offer the possibility of maintaining backups regularly. See [#544: New option to keep backups at regular intervals](https://github.com/dpradov/keynote-nf/issues/544)
<br><br>

### Installation
Since version 1.8.1 there is a setup program (based in Inno Setup, by Jordan Russell)
Download the <b>[latest release](https://github.com/dpradov/keynote-nf/releases/latest)</b>
<br><br>

### KeyNote NF Release Key
Since the release of version 1.8.1 KeyNote NF is signed using GPG with the following key:

    Signer: KeyNote NF (Daniel Prado Velasco)
    E-mail: dprado.keynote@gmail.com
    Key ID: 0xFDBC8364
    Key fingerprint: EB6F 9FED 0F62 7568 201C 2117 909F E709 FDBC 8364
    Created: 2023-12-11
    Expires: 2026-12-11
	
    https://github.com/dpradov/keynote-nf/blob/master/KeyNoteNF_0xFDBC8364_public.asc

It is also included SHA256 hash file (.sh256)

More info in [VerifyingReleases.txt](doc/VerifyingReleases.txt)
<br><br>

### Bugs, new Features
For any bug report, patch proposal or feature request, add an entry into the [Issue tracker](https://github.com/dpradov/keynote-nf/issues).  
Please, be specific; it is preferable to create several issues instead of only one very 
heterogeneous, with many questions. That way it can be managed much better.  
<br><br>

### Source Code
In case you fork the repository or clone it to your desktop, in the [README_SourceCode.txt](doc/README_SourceCode.txt) you will get information about how to compile the program, and notes about the components and libraries required by KeyNote NF.
<br><br>

### Copyright and license
The initial developer of KeyNote is Marek Jedlinski, copyright 2000-2005.  

The adaptation to Delphi 2006 and the new functionalities added since version 1.7.0 corresponds to Daniel Prado Velasco, copyright 2007-2024.  
In august 2023, with the version 1.8.0, the code was migrated from Borland Delphi 2006 to to Embarcadero Delphi CE 11.3 (Community Edition 11.3  Alexandria).

Code released under the [Mozilla Public License 2.0](./LICENSE.txt). This license applies only to units written by the authors of KeyNote and KeyNote NF: Marek Jedlinski and Daniel Prado, respectively.
Note: KeyNote was originally licensed by MPL 1.1. I have contacted with Marek and have obtained his approval to update to MPL 2.0.

Third-party units may be regulated by their own licenses.
Please, see file [README_SourceCode.txt](doc/README_SourceCode.txt) and [acknowledgments.txt](doc/acknowledgments.txt) for more information about third-party code.
<br><br>

### Donations
Thanks for using KeyNote NF. You can show your appreciation and support future development by donating!

[![](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/donate/?hosted_button_id=PZB5EZS3V7VY4)

<sup>Legal notice: By making a donation to the KeyNote NF project you signify that you acknowledged, understood, accepted, and agreed to the terms and conditions contained in this notice. Your donation to the KeyNote NF project is voluntary and is not a fee for any services, goods, or advantages, and making this donation does not entitle you to any services, goods, or advantages. I have the right to  use the money you donate to the KeyNote NF project in any lawful way and for any lawful purpose I see fit and I are not obligated to disclose the way and purpose to any party unless required by applicable law. Although KeyNote NF is free software, to my best knowledge the KeyNote NF project does not have any tax exempt status (the KeyNote NF project is neither a registered non-profit corporation nor a registered charity in any country). Your donation may or may not be tax-deductible; please consult this with your tax advisor. I will not publish/disclose your name and e-mail address without your consent, unless required by applicable law. Your donation is non-refundable.</sup>
