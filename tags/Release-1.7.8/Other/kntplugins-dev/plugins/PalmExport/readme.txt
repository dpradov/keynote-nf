-----------------------------------------------------------
PALMEXPORT.KNL
Export text to PalmOS "doc" format - plugin for KeyNote
by Marek Jedlinski
<eristic@lodz.pdi.net>
http://www.lodz.pdi.net/~eristic/free/keynote.html
25 April 2002
-----------------------------------------------------------

This is a plugin for KeyNote.
This plugin requires KeyNote version 1.0 or higher.

PURPOSE:
This is a very simple proof-of-concept plugin that exports
text from KeyNote to the "doc" format file, commonly used
on PalmOS-enabled handheld computers. To use the plugin,
select the text you want to export and run the plugin.

If you enable the "Automatically install on handheld" option
in the plugin dialog box, the exported file will be transferred
to your handheld the next time you perform a hotsync.

ACKNOWLEDGMENT:
The plugin uses Delphi code for "DocReader" by
Mike Pickering (http://www.alltel.net/~mpicker0/).

TO INSTALL:
Unzip the plugin file into the "\plugins" subdirectory,
below the directory where KeyNote is installed. For example,
if KeyNote is installed in
	c:\Program Files\Keynote
then the plugin file should be placed in
	c:\Program Files\Keynote\Plugins
This subdirectory is created by the Setup program. You can
create it manually, if necessary.

TO RUN:
If the Resource Panel is not visible in KeyNote, display it
by clicking the "View" menu and selecting the "Resource Panel"
command. In the Resource Panel, click the "Plugins" tab.

(If the Resource Panel is already displayed when you install
the plugin, you may have to tell KeyNote to refresh the list
of plugins. To do so, right-click the list of plugins and
select the "Reload plugins" command.)

Double-click the "Export to Palm" plugin to run it.

See the "Plugins" topic in KeyNote's Help file
for information on installing and using plugins.


NOTES:
The plugin only allows you to export text from KeyNote to your
handheld. At the moment (May 2002), there is no facility to import
Palm databases into KeyNote. This may be addressed later; please
check the website occasionally for updates.

Most new Palms come with at least one viewer capable of dislaying
DOC files. Additional free or commercial viewer applications may
easily be found on the Internet. Two freeware programs that support
DOC files are:

CSpotRun
http://pdacentral.planetmirror.com/palm/preview/163124.html
Supports only VIEWING doc files; very functional.

ZDoc
http://www.geocities.com/Area51/7689/pilot.html
Supports viewing AND EDITING doc files.


-----------------------------------------------------------
