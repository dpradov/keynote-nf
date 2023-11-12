
KEYNOTE 1.8.1. NEW IMAGE MANAGEMENT
==========================================

The new version of KNT, 1.8.1, offers the following improvements in relation to image management:

   
 - On systems with recent versions of RichEdit (> 4) KNT makes use of tags that allow images to be embedded in various formats. 
 In addition to wmetafile8 and emfblip, IT ALLOWS YOU TO USE PNGBLIP OR JPEGBLIP. These last two are much more convenient with images
 in formats such as BMP, PNG, JPG, GIF, TIF or PNG. The most obvious thing is that a very significant reduction in the size of the KNT
 file is achieved since the RTF format is considerably smaller using pngblip or jpegblip
    Remember that in RTF (at least in the implementation available in the RichEdit controls) images are saved in RTF code as Hexadecimal
	in ASCII. Something like:   
     {\pict{\*\picprop}\wmetafile8\picw3888\pich5185\picwgoal2204\pichgoal2940 0100090000038c0000000000760000....}
   	   
 	 - Note: In WordPad, which also uses the RichEdit control, the pngblip or jpegblip formats are not used by default, it does not 
	  incorporate them, although it recognizes them when an RTF file that includes them is opened; yes, giving a warning because by
	  default it discards them.
	  
  All of these formats are used in KeyNote and, from this version, all images are programmatically inserted into the RichEdit control
  using the most appropriate format.	   
      
 - It is possible to maintain behavior similar to previous versions, where images continue to be saved embedded in the RTF and therefore
   as Hexadecimal in ASCII. But even in this case the situation improves compared to previous versions since:
   
 - IMAGES ARE ALWAYS INSERTED BY CODE and NOT THROUGH THE CLIPBOARD. In recent versions, only images inserted through drag and drop from 
   the browser were in png or jpg formats; The WMF and EMF files were not inserted as images but as objects, which when opened were
   displayed in Paint. (Older versions of RichEdit didn't even offer png or jpg that way)

 - The menu entry INSERT | PICTURE... now allows you to insert THE MOST COMMON IMAGE FORMATS, although it is more convenient from this
   version to use the DRAG AND DROP mechanism, since it is possible to DRAG ANY FILE INTO THE EDITOR'S OWN CONTENT, in a specific position.
      - A new option is offered from the import window: "INSERT CONTENT AT CARET POSITION"
       If the file (or files) are images, they will be inserted as such, and it will optionally be possible to do so in Link mode.

  
 - Images embedded in RTF can continue to be saved for COMPATIBILITY WITH PREVIOUS VERSIONS, but using the new version it is advisable
   to use the new storage formats, which make it possible to use other functionalities.
   Furthermore, at any time it is possible to change the storage format (towards 'classic', now through the option called Embedded RTF)
   on the current file (Save) or on another (Save As), or export all or part of the KNT file to another file, with the embedded RTF images.   

 - It is possible to configure one of the following STORAGE MODES for a given file:
 
	  - Embedded RTF: As indicated, this is equivalent to the usual placement of images in RTF files: within the RTF content
                 	  itself, as hexadecimal values saved in ASCII. The format of these images is usually wmetafile8, very
					  voluminous. It would correspond to the image management functionality being disabled.

	  - Embedded KNT: Images are located at the end of the KNT file, in binary
					  This type of storage can be interesting for images to be kept in encrypted KNT files, or when you want
					  themto be available without depending on another separate file (or folder).
					  It is more optimal than embedding in RTF code (like ASCII in HEX mode)					  
					  
 	  - External:     Allows you to select an external storage of one of two types, Zip or Folder.
				  	  - Zip: image files are saved in a Zip compressed file format.
                        Within the configuration options you can select the type of compression (ImgDefaultCompression) that will be used 
                        when adding each new image: Store, Deflate, Deflate64. The first option stores them without applying any compression.
                        The second option, Deflate, is the most common when adding files to a Zip.					  
					  
				     - Folder: Images will be saved in folders and subfolders in a file system.

				     In both cases the images will be created within a subfolder corresponding to the note from which the new image was 
                     inserted for the first time.					  
					  
      - External + EmbeddedKNT: 
	                 It allows to combine the use of images embedded in binary in the KNT file with that of external storage, which 
                     can be Zip or Folder.
					 
                 
   
     * Note. Embedded KNT
	  In the case of using this type of storage mode with a KNT save format other than the native one, that is, compressed or encrypted,
      the following must be taken into account:
 	  - Compression/decompression will not be applied to the final block of the file in which the binary images are embedded.
	    This speeds up saving and opening the file, especially the first thing, because it is always more expensive to compress 
        than to decompress.	   
			   
	  - In encrypted mode, images embedded in binary (Embedded KNT), as well as those embedded in RTF (Embedded RTF), YES they are encrypted.
        In the event that it is required to ensure image encryption, one of these two modes must be used, since *images saved in external 
        storage, Zip or Folder, will NOT be encrypted*.


 - The application allows you to CHANGE THE STORAGE MODE of images of a given file, as well as change from an external Zip storage to
   another Folder and vice versa, or point to a new storage location.   
 
   When modifying the storage mode, this change will be made immediatel on all notes and nodes, although it will only become really 
   effective once the KNT file is saved. At that time the images will be serialized to the KNT file and/or the files will be added to
   possible external storage. Once a storage change has been made, the file must be saved to be able to modify it again.
    
  (It is advisable to have saved existing modifications before changing the storage mode, to ensure you have a complete backup --text and images). 
   Of course, it is STRONGLY RECOMMENDED to maintain a backup policy, for example by activating the 'Backup at regular intervals' option.	 

   
 - Any EXISTING FILE created from a previous version will be opened using Embedded RTF mode 
   
 - It is possible to define the STORAGE MODE that will be used BY DEFAULT in new KNT files.   
    
 - Operation with INACCESSIBLE EXTERNAL STORAGE
    If external storage is used (with External, or External + EmbeddedKNT modes) and it is not available, when trying to view the images
    a link will be displayed indicating that it is not available (*). It will still be possible to add new images although provisionally
    [only] in the EmbeddedKNT format (although the general mode used is only External) 
	
    This will allow you to work with the KNT file from a location where the storage is temporarily inaccessible.
    This could happen, for example, if we use a folder on our work computer as external storage and we want to use the KNT file from home
    without having to take all the images with us at the same time. Back on the work computer, at the time of saving, if you already have
    access to the folder (or Zip) the images previously incorporated will be added to it. If the storage mode is External (and not 
    External + EmbeddedKNT), that image will no longer be saved embedded in binary at the end of the KNT file.         
	  Something similar will happen with the deletion of images. If all instances of an image are removed and external storage is not available
    to delete the file, the deletion will be done when access to the storage is resumed.
   
	In addition to a situation like the one described, this behavior can be used to allow it to function even if there are temporary network
    outages that could prevent access to external storage.   
      
   (*) If External + EmbeddedKNT is used, the image can always be displayed
   
   
 - WHEN EXPORTING to RTF format the images will be included embedded in RTF (usual way)
 - When exporting to KNT format, three possibilities will be considered, as established in the configuration options:
    - Include images in 'classic' mode: embedded in RTF
    - Include images in Embedded KNT mode
    - Do not include images. A link will be displayed instead   
      
 - WHEN IMPORTING NOTES from a KNT FILE on disk (with Tools | Merge Notes) or dragging KNT files, the application incorporates the images
   of the new notes, adapting them to the storage mode of the current file.   
   
 
 - In notes configured as PLAIN TEXT AND IN VIRTUAL CONTENT NODES *NOT* RTF, the insertion of images is prevented
 
 - In VIRTUAL NODES LINKED TO RTF FILES images are allowed, but exclusively the Embedded RTF format will be used, to ensure that it can 
   continue to be used from outside KNT as usual.  
  
  
 - IMAGES and options available
   -------------------------------
 - It is possible to include multiple INSTANCES of the same image.
    
 - The application allows you to CHANGE THE VISIBILITY of the IMAGES of the file (which becomes effective only when it is required to
   show each individual note/node). If the option to hide the images is selected, the application hides their content and instead offers
   a link from which they can be consulted (with the help of a viewer).
      * A similar link will appear next to a referenced image that is not available (storage is not available or the file has been moved
      or deleted)
   For this purpose, a new menu entry (View | Show Images) is included, as well as a new button in one of the toolbars.   
  

 - A simple IMAGE VIEWER is incorporated that allows one or more images to be opened from independent windows.
    With the viewer it is possible:
    - Check image details, such as format, dimensions, size (Kb), path or number of instances.
	- Modify the image title
	- Zoom/change background color  (There is also an option that allows to set a default background color for the viewer )
	- Dynamically enlarge the image adjusting it to the size of the window (respecting the proportions)
	- Open the image path or the file itself (in case of external images). The latter allows to view the image in a specialized image viewer.
	- Save the image to a file
	- Scroll through all the images or show the one corresponding to a specific ID.

   Opening the image viewer is done as follows:
	- If the image is visible:
		- Double clicking on it opens the internal KNT viewer with that image loaded, displayed at 100%
          (or reduced if it cannot be displayed in full on the screen)
	    - CTR + double click directly opens the image from outside KNT, with the program that is associated with its extension.
		  (This is only possible with Linked images or images saved in external Folder storage)

	- If the image is not visible and a link is being displayed instead:
	 - The general options defined for URL actions are applied, taking into account that Open corresponds to the opening of the 
       internal viewer, and Open New to the opening outside of KNT. 
       Right clicking on the link will always open the internal viewer.


 - When inserting/registering a new image, the application differentiates between the FORMAT IN WHICH IT WILL KEEP the image, which 
   will be the format it presents in the case of inserting (or dragging) from a file; and the FORMAT USED TO DISPLAY IT in the RichEdit
   control, that is, the one that will be used together with the RTF \pict tag.   
      
   In the first case it is possible to use the GIF, PNG, JPG, BMP, TIF, WMF, EMF and ICO formats. If the image is added as Linked
   (not 'owned'), only a reference to the original file will be maintained, not the content. If the image is incorporated into the
   KNT (owned) file, either through internal storage (EmbeddedKNT) or external storage (EmbeddedExternal: Zip or Folder), the BMP
   and ICO formats will be stored previously converted to JPG or PNG; the rest will be stored in the original format, including 
   WMF or EMF.   
     
   In images obtained from the clipboard, usually from screenshots, the image is found as a Bitmap or Metafile. 
   In these cases, the format in which it will be saved will be JPG or PNG depending on the ImgDefaultFormatFromClipb and 
   ImgRatioSizePngVsJPG configuration options. 
   In the case of identifying new images to incorporate from RTF text processing that includes images, these are mostly found as Metafiles,
   with the WMF or EMF formats (normally the first, wmetafile8). These images will be saved converted to JPG or PNG depending on the 
   configuration options. Normally the images identified in WMF format (wmetafile8) come from clipboard captures, and that is not their
   original format, taking up much more size than necessary. Hence the conversion to PNG (preferably) or JPG.   
   
   
 * Options: ImgDefaultFormatFromClipb, ImgRatioSizePngVsJPG, ImgCompressionQuality, ImgBmpPixelFormat
    The ImgDefaultFormatFromClipb option allows you to choose between PNG or JPG.
    The ImgCompressionQuality option allows you to define a quality format to be used with the JPG format: 0 - 100
    The ImgRatioSizePngVsJPG option allows you to select the most appropriate format taking into account the size resulting from the 
    conversion to both:   
   
     A value > 0  => A conversion will be performed to both formats (Png and Jpg). 
	 If the default format (set by ImgDefaultFormatFromClipb, eg. PNG) is sizer than the alternative format, in a proportion >= to 
	 the value indicated, the alternative format (eg. JPG) will be used
	 
	 The ImgBmpPixelFormat option allows you to select the color depth to be considered on the starting bitmap:
    pf15bit, pf24bit, pf32bit
    
    - - - 
	When the content of a screenshot is directly pasted from the clipboard (for example with Greesnhot), an image (CF_BITMAP)
	is detected in the clipboard, and in that case the conversion to PNG is identical and the result takes up much less space.
	In these cases, the conversion to PNG probably takes up less space than its conversion to JPEG.
	As an example, an image captured from an Explorer window can take up the following (in binary, viewing Stream.Size), approx.:	
	
	- BMP: 775.000   - WMF: 582.000   - PNG: 30700   - JPG: 39.000    (JPG occupies 1.3 times more than the PNG version here)

	( Images are available on the clipboard on other occasions, for example when selecting an image in a browser and using Copy Image )
	
	If that same image is pasted into WordPad and from there it is copied back to the clipboard, at the time of pasting we 
	will no longer have a bitmap, but a Picture (Metafile). In this case, an image (CF_BITMAP) is not detected in the clipboard,
	but rather an embedded object, which gives rise to an image and which we can detect through the generated RTF. We can convert
	the image extracted from the RTF. But that conversion, even if it is good, is not as perfect as the one that it occurs when 
	we directly detect the image, and they take up slightly more.
	(Note: A Picture (Device independent bitmap) is offered as CF_BITMAP)	
	
	When what is pasted is the content of a photograph, in that case it is clearly more favorable to use the JPEG format over PNG.
	As an example, a relatively small image, of a photograph with much of the image in a soft gradient, without much detail, can 
    occupy 538,499 in PNG and 34,067 in JPEG (15.8 times more in PNG)	
	- - - 
    
    A typical screenshot (from an application, file explorer, etc.) can usually be saved without any noticeable color loss, 
	with pf15bit (with pf16bit it doesn't look good). If the capture includes photographs, or images with color gradients 
	that we need to maintain, then we must select pf24bit. Under normal circumstances, at least from screenshots, it doesn't make
	much sense to use pf32bit, as it will take up more space and you won't see any difference.
    It is true that the captures, taken in bitmaps, will be converted to png (or jpg), so the difference between the formats
    will not be so big, but there will be one. The difference can be seen when the image has gradients, etc., but in that
	case the normal thing will be not to want to lose quality.
      If we have quality images then what we should do is drag (or insert) the files to the editor. The image will be displayed 
	in the best quality allowed by the editor. And we will not touch the file format.
    - - -

	
  * Option: ImgFormatInsideRTF  
	In order to make the images visible in the editor, in RTF, the formats currently available in the RichEdit control will be used:
    \wmetafile8 \emfblip \pngblip or \jpegblip
    If we insert the image programmatically, the format defined by configuration will be used:   
   
      ImgFormatInsideRTF: (ifWmetafile8, ifAccordingImage )
	  
	The first option (ifWmetafile8) will be the only one offered on systems with an older version of RichEdit (<= 4 ), in which 
    all images are embedded in RTF with the format \wmetafile8.
    With the ifAccordingImage option, the application will use jpegblip for JPG images, emfblip for images that come from EMF *files*,
    wmetafile8 for those that come from WMF *files*, and pngblip for the rest: PNG, GIF, TIF, BMP and ICO.
  

  * Option: ImgMaxAutoWidthGoal	 
	When inserting an image, the initial visible size will depend on the value set in the ImgMaxAutoWidthGoal property
	   
	   0   It will be shown with its actual size
	  -1   Will be displayed with a maximum width equal to the visible width of the editor at the time of insertion
  	  >0   A maximum width equal to the indicated one will be displayed (in pixels)

	Of course, once inserted, the image can be resized. This property has no effect on existing, already dimensioned instances	 
   
  
  * Options: ImgDefaultLinkMode, ImgLinkRelativePath					  					  
	Images can be incorporated as a 'copy', adding to the defined storage (or storages): 'owned'.
	But they can also be added in Link mode (not owned), in which case only the file path is registered as a reference, its content
    is not stored nor is the file deleted if all references to it are removed.
     - ImgDefaultLinkMode 
        0: Owned by KNT file. Added to one storage. Can be deleted when unused
	    1: Linked. Image is independent. It is obtained from its original path and will never be deleted by KNT										  
											  										  
  * Option: ImgUseRecycleBin
	Allows you to move deleted image files to a subfolder '_RecycleBin' within the Folder mode folder, or to a Zip file located next 
    to the main one, and called "...Deleted.zip" 
 
 
 - A record of added and deleted images is kept in external storage through a LOG FILE. 
 
   
 - Related to the possibility of showing or hiding IMAGES, the application will maintain in memory, as content (stream) associated 
   with each node, the version without images, in which its content is hidden and a link is included in its place.
   The content of the nodes/notes will only be 'expanded' with the content of the images within the RTF (\pict tag) in the notes 
   and the selected nodes (visible), and only if the images are being shown (View | Show Images)   
   
   This same format is used when saving: NOTES / NODES DO NOT SAVE THE CONTENT OF THE IMAGES BUT A REFERENCE to them.
   This speeds up opening and saving and reduces the size of the KNT file, especially if images are saved only on external storage.
   But even if they are saved as Embedded KNT, the size will also be reduced very noticeably because RTF images, even using the 
   pngblip and jpegblip formats, take up more than twice as much space in ASCII + Hexadecimal as they do directly in binary.  
  
  
 - The content of the images is recovered and kept in memory. In the case of external images (linked or not), the content is kept in
   memory for a set of minutes since the last access (to view it). After this time without being accessed, the content of the image
   is released from memory.
    * If at the time of verification, the application detects that the storage is not available, the content of these images 
      will not be released.	 
 
  
  
 Registration in KNT of the information associated with each image. Serialization/deserialization  
 ---------------------------
 - In the case of using a storage mode other than Embedded RTF, the application will use an EXTENDED KNT FORMAT when saving in
   which new blocks have been incorporated at the end of the file, to allow including the registration of stores and images. Ex:
  

   %S
   SM=2
   SD=1|TestIMG_img
   %I
   II=5
   PD=1|NOTE1\|1_Image 24oct.png|1|32|32|343790583||1|6||0
   PD=2|NOTE2\|2_Image 24oct.jpg|1|770|649|1481786765||1|3|CORONA|0
   PD=3|NOTE2\|3_sample.wmf|5|770|649|1976212572|E:\sample.wmf|1|2||0
   PD=4|||1|120|171|1006143828|E:\Cocobill_avatar.png|0|2||0
   %EI
   EI=1|1_Image 24oct.png|<size>
   <--image in binary-->
   ##END_IMAGE##
   EI=<IDImage>|<FileName>|<size>
   <--image in binary-->
   ##END_IMAGE##
   ...
   %%

    %S: Storage section
       SM= Reflects the storage mode.. 0:EmbeddedRTF 1:EmbeddedKNT 2:External 3:ExternalAndEmbeddedKNT	 
		  (Mode 0 will not actually be displayed, since in that case these new blocks will not be used...)
		 
	   SD= Type(0:Zip 1:Folder)|Path
		  (Only included with modes 2 and 3)
	
	%I: Image section. Lists information about the images used in the KNT file:
      
	   II= Shows the ID to be used for the following image
	   PD= IDImg|Path|Name|Format|Width|Height|crc32|OriginalPath|Owned|RefCount|Caption|MustBeSavedExt
			 - IDImg: Each image has a unique numerical ID (>=1)
			 - Path: Relative path to storage, this will normally correspond to the name of an existing note
			 - Name: Unique name of the file within the storage.
			 - Format: 0:GIF, 1:PNG, 2:JPG, 3:BMP, 4:TIF, 5:WMF, 6:EMF
			 - Width, Height: Width and height of the image. Each instance may be using different values.
			 - crc32: Value obtained from the image content. Used to identify if an added image is new or is another instance
                      of one already registered.
			 - Original path: Obtained in the case of insertion from existing files
			 - Owned: 1:Owned by KNT file. Added to one storage. Can be deleted when unused
			          0:Linked. Image is independent. The image is obtained from its original path and neve will be deleted by KNT
			 - ReferenceCount: Number of instances of the image within the KNT file.					  
					When the number of references becomes 0, the image will be deleted (when saving changes) from the %I block
                    and the %EI block (the latter if the External or ExternalAndEmbKNT modes are used).
					In the case of using the External or ExternalAndEmbKNT modes (the image being 'owned'), the corresponding
                    file will be deleted from the external storage.
					A line in KNT with this field set to 0 indicates that the image has been deleted but is missing from external
                    storage, not accessible at the time of saving the KNT file.
					  
			 - Caption: Image title
			 - MustBeSavedExt: 1: Indicates that the image is waiting to be saved to its external storage.
				Its content will be found in the %EI block					  
			                    
    %EI:  Embedded Images		  
		In the case of using smEmbKNT or smExternalAndEmbKNT (fStorageMode: TImagesStorageMode) as storage mode
		The content of each image (except linked ones) will be saved within this block		  
	



================================================================================================================


//---------------------------------------------------------------------
//  TFilesStorage
//--------------------------------------------------------------------- 
  TFilesStorage:
  Manages the saving of image files from a specific storage
  There will be various storage types, depending on TStorageType = (stZIP, stFolder, stEmbeddedKNT, stEmbeddedRTF), 
  implemented by the corresponding classes:   
    TZipStorage, TFolderStorage, TEmbeddedKNTStorage

  Images will be saved in storages of different types:
  - stZIP:       ZIP compressed files
  - stFolder:    In folders and subfolders in a file system
  - EmbeddedRTF: Equivalent to the usual placement of images: within the RTF content itself, as hexadecimal saved in ASCII.
  - EmbeddedKNT: Images located at the end of the KNT file, in binary
                 
   It is possible to use at the same time, in a KNT file, an external storage (ZIP or folder) with the internal EmbeddedKNT
 
  TStorageType = (stZIP, stFolder, stEmbeddedKNT, stEmbeddedRTF);



//---------------------------------------------------------------------
//  TKntImage
//---------------------------------------------------------------------

  When image management is done (ImagesManger.StorageMode <> smEmbRTF) the ImagesManager object will have a list of images (TKntImage)
  where the attributes required for management are recorded, such as an ID, the location within the storage or the height and width of
  the image, for example.  
   
  There can be multiple instances of the same image, in any note/node. This will not result in any increase in file size since the 
  image will only be saved once. The only thing that is maintained for each instance is the visible width and height, within a hyperlink.
  All image data is saved in TKntImage objects. Only the attributes of each instance, the visible height and width, will be saved in the
  RTF content itself.  

  When a new instance is created, the number of references of the associated image will increase, and will decrease when it is deleted.
  Something similar to how the "String" type works in Delphi.
    
   If at the time of saving the file an image has 0 references, it can be deleted if:
     - This is an image that belongs to the KNT (owned) file, not Linked
     - In case of external storage, if it is available	
	
//---------------------------------------------------------------------
//  TImageManager
//---------------------------------------------------------------------

 It takes care of all image management, also including mechanisms to allow image insertion, even for the 'classic' storage mode, Embedded RTF. 
  
  
 * Image visibility 
 ------------------------

 The instances or uses of the different images may be in one of the following two states: TImagesMode = (imImage, imLink)

 - 1. imImage
   -----------  
   It is the equivalent of the current situation of an image, where it is visible and its content is embedded in the RTF.
    Ex: {\pict{\*\picprop}\wmetafile8\picw3888\pich5185\picwgoal2204\pichgoal2940 0100090000038c0000000000760000....}

    We will not include the image ID inside the \pict tag (we cannot use tags like \bliptab and \blipuid, because they are ignored),
    but we can do it right next to it with a hidden field, similar to the one used with the recent internal links:

   \v\'11I<IMG>\'12\v0{\pict{\*\picprop}\wmetafile8\picw3888\pich5185\picwgoal2204\pichgoal2940 0100090000038c0000000000760000....}


 - 2. imlink
   --------------
	It corresponds to the image in a state in which it does not include the content but only information that identifies the image itself (an ID)
    and other information specific to the instance: visible width and height.
    It is based on the use of RTF hyperlinks that include a link with a specific format (analogous to what is already done with KN's internal
    links, also its own), preceded by a hidden label located to the left of the hyperlink:   

    \v\'11I<ImgID>\'12\v0{\field{\*\fldinst{HYPERLINK "img:ImgID,WGoal,HGoal"}}{\fldrslt{\ul\cf1 Caption}}}

     - "Caption" will show the title of the image, if it has one, and if not, the name of the file (if the image has been obtained from a file)
        or a string constructed at the time of registering the image.
   	 - "img:ImgID,WGoal,HGoal".   Image ID along with the visible width and height of the instance
   
    It is the format used to save images (in storage modes <> smEmbRTF). If the storage mode is smExternal, the size of the KNT file will be
    significantly reduced because it will not contain the content of any image, only a list with the list of registered images.   

    The application allows you to change the visibility of all the images in the file (which becomes effective only when the note/node is
    required to be shown). If the option to hide the images is selected, the application displays them in this imLink format, hiding their
    content and instead offering a link from which you can consult and edit their title.


 * Using hidden tags, monitoring actions
  ---------------------------------------- 
  The application monitors all actions that could result in image insertions or deletions. To do this, it processes the content pasted from
  the clipboard looking for the presence of images and analyzes RTF content in search of RTF tags associated with the existence of images.
  It makes use of its own, hidden labels, analogous to those already used to maintain the reference to the destination in recent bookmarks.
  These labels contain only the ID of the image and are placed to the left of them, whether they are in their visible (imImage) or 'hidden'
  (imLink) mode. This processing is optimized and does not affect the normal operation of the application at all.  
  
  The use of these tags (e.g. "\v\'11I3\'12\v0") simplifies the identification of already registered images. But since they cannot be added
  'inside' the image tags themselves, it is the application's responsibility to ensure that they are always kept next to the image they accompany.
  
  To do this, for example, when content is inserted on the editor, it is ensured that, if it acts on an image, it is also done on its associated
  hidden tag, extending the selection to include it. When an image is deleted (with DELETE/DELETE or BACKPSPACE, or by cutting to the clipboard)
  it is controlled whether to also delete its label, etc.  
  
  Copying an individual image to the clipboard does not also send the tag, to ensure that the content is served as an image and not just RTF
  content. But the copied ID is noted, in case it is detected that this copied content is then pasted into the KNT itself.   
  
  * It has been used to optimize the method that allows us to discriminate whether the current content on the clipboard has been copied by this
    instance of the KNT application or another application. This was necessary, for example, to determine whether to paste the content as
    Plain Text or not, depending on the corresponding configuration.  
  
 * Eliminations
   --------------
  Image deletions (TKntImage) are processed on save. They could be done immediately but would have the problem that it would not allow the UNDO
  mechanism that the RichEdit control offers.
   
 * Image IDs
  ----------------
  Each image has a unique ID within the KNT file, whether it is Linked or not. When the KNT file is saved to disk, the number of the next ID
  that can be used is recorded. This number is determined from the ID of the last saved image. To do this, a distinction is made between 
  NextImageID and NextTempImageID. Images that are added but removed before saving the file do not 'consume' ID.


  * Image insertions by code
  -----------------------
  When inserting an image by code, instead of using the clipboard, as has been done until now, it is necessary to generate all the image 
  information necessary to construct the RTF sentence that the RichEdit control expects.  

    Note. Insert->Picture has been adding them through the clipboard, but that causes them to be saved in WMF format (wmetafile8).
     See my notes for Release 1.8.0 Beta 6
     We must avoid using the clipboard for this, and instead insert a string with the necessary RTF command.
     EYE. What I indicated in the Release is true when dragging on the editor in KeyNote. But if it is done on a file opened with WordPad,
     it is always created as wmetafile8, and even doing so loses quality (it seems to handle fewer colors). Furthermore, if you try to open
     a file with a format other than wmetafile8 from WordPad, it gives a security notice after which, depending on whether it is accepted
     or not, it will display the content. In this link they explain the reason:   
	 https://forums.malwarebytes.com/topic/259359-wordpad-ms-word-security-warning-on-images/

