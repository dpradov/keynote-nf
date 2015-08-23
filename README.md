## keynote-nf

Tabbed notebook with RichText editor, multi-level notes and strong encryption. 

This project is an evolution of Tranglos Keynote (of Marek Jedlinski), with new features like: 
 * *Checkboxes on children of selected nodes* <br>
    Selecting checkboxes for all nodes (View/Tree Checkboxes -- now View/All nodes Checkboxes) is still posible. Besides, checkboxes can be shown only on children of selected nodes (Children Checkbox)

 * *Hidden nodes* <br> Capacity to work with hidden nodes. Nodes can be hidden in two ways:
   * Activating a mode wich automatically hides checked nodes (Show or Hide checked nodes)
   * Filtering one note's nodes or all notes under a searching criterion (Filter Tree Note)

 * *Alarms on nodes*
 * *Better treatment of tables*
 * *Improved treatment of links*
 * *Multilanguage support*
 * *New kind of virtual nodes: links to other nodes (Mirror nodes)* <br>
     Allow to organize the information in different ways, because nodes can be simultaneously in different notes. It will be possible to sort, rank and structuring in a free tree hierarchy, independent of the hierarchy in wich 'real' nodes reside.
 * *Unicode compliant*
 * *New KeyNote file format: compressed*

Original program can be found in http://www.tranglos.com/free/keynote.html

The repository of this project has been exported from Google Code. The binaries of the project can be downloaded from [SourceForge](https://http://sourceforge.net/projects/keynote-newfeat/files/) or [Google Code](https://code.google.com/p/keynote-nf/downloads/list)
<br><br>

###Name. Compatibility
The program has been be renamed from "KeyNote" to "KeyNote NF". That way I hope it will be easier to locate it searching the web.

To implement some of the new features included in KeyNote NF I had to extend sligthly the format of KeyNote (.knt) files. All new elements are optional so that new version can open without problems a file corresponding to version 1.6.5.
Also, with KeyNote 1.6.5 it is possible to open a file created with a later version, but in this case, if the file is saved, the alarms and information about checked nodes will be lost.
<br><br>

###Bugs, new Features
For any bug report, patch proposal or feature request, add an entry into the [Issue tracker](https://github.com/dpradov/keynote-nf/issues). 

Please, be specific; it is preferable to create several issues instead of only one very 
heterogeneous, with many questions. That way it can be managed much better

*Before report any new issue, please read the topic [HOW TO: Report Bugs / Add Feature Requests](http://sourceforge.net/apps/phpbb/keynote-newfeat/viewtopic.php?f=15&t=20) in the [forum of KeyNote NF](http://sourceforge.net/apps/phpbb/keynote-newfeat/index.php)*

There you can discuss about issues itselves, too.
<br><br>

###Installation
There is no installation project at this moment. You can copy all the files in archives "Release_XXXXXX.zip" in a new folder as KeyNote NF doesn't need installation to work.
If you prefer you can install original [KeyNote (version 1.6.5)](http://www.tranglos.com/free/files/kntsetup.exe) and then copy only the following files (contained in zip) in the installed folder, replacing the original ones:

`keynote.exe`, `keynote.chm`, `alert.wav`, `keynote.tip`, `keynote.lan`, `doc\*`,  `lang\*`
