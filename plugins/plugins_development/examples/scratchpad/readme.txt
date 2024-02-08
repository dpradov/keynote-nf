-----------------------------------------------------------
SCRATCHPAD.KNL
RichEdit "scratchpad" plugin for KeyNote
by Marek Jedlinski
<eristic@lodz.pdi.net>
http://www.lodz.pdi.net/~eristic/free/keynote.html
22 June 2001
-----------------------------------------------------------

This is a sample plugin for KeyNote.
This plugin requires KeyNote version 0.999 or higher.

WARNING:
This plugin cannot be used if version 3.0 of the "riched20.dll" 
library is installed on your system. With that version of the 
richedit library, KeyNote will crash when you drag and drop
text from KeyNote to the scratchpad or vice versa. I have no
idea why.


BUG WARNING! You can set the scratchpad window to "stay on top" 
of other windows (via the right-click menu). If you do this, 
and if the scratchpad window covers the KeyNote window, AND if 
KeyNote displays a message or a dialog box, you will not be able 
to access either KeyNote or the scratchpad. It's not a lockup, 
it's just that the scratchpad stays on top, so you can't see 
KeyNote's dialog box, and you cannot move the scratchpad because 
it is disabled while KeyNote has a modal window open. 
If you see this happen, just click the KeyNote titlebar once 
(it will not seem to respond), then hit ESC or Enter to close 
the (invisible) dialog box. This will be fixed for 1.0 release
of KeyNote.

TO INSTALL:
Unzip the plugin file into the "\plugins" subdirectory, 
below the directory where KeyNote is installed. For example, 
if KeyNote is installed in
	c:\Program Files\Keynote
then the plugin file should be placed in
	c:\Program Files\Keynote\Plugins
This subdirectory is created by the Setup program. You can
create it manually, if necessary.

See "plugins.txt" in KeyNote's "\doc" subdirectory
for information on installing and using plugins.

This is a "resident" plugin.

-----------------------------------------------------------
