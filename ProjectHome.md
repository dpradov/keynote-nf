### I'm going to move the project to GitHub and revise the most import issues in order to prepare a new version. Please, don't add new issues by now ###


---


Tabbed notebook with RichText editor, multi-level notes and strong encryption.

This project is an evolution of Tranglos Keynote (of Marek Jedlinski), with new features like:
  * **Checkboxes on children of selected nodes** <br> Selecting checkboxes for all nodes (View/Tree Checkboxes -- now View/All nodes Checkboxes) is still posible. Besides, checkboxes can be shown only on children of selected nodes (Children Checkbox)</li></ul>

<ul><li><b>Hidden nodes</b> <br> Capacity to work with hidden nodes. Nodes can be hidden in two ways:<br>
<ul><li>Activating a mode wich automatically hides checked nodes (Show or Hide checked nodes)<br>
</li><li>Filtering one note's nodes or all notes under a searching criterion (Filter Tree Note)</li></ul></li></ul>

  * **Alarms on nodes**
  * **Better treatment of tables**
  * **Improved treatment of links**
  * **Multilanguage support**
  * **New kind of virtual nodes: links to other nodes (Mirror nodes)** <br> Allow to organize the information in different ways, because nodes can be simultaneously in different notes. It will be possible to sort, rank and structuring in a free tree hierarchy, independent of the hierarchy in wich 'real' nodes reside.<br>
<ul><li><b>Unicode compliant</b>
</li><li><b>New KeyNote file format: compressed</b></li></ul>

Original program can be found in <a href='http://www.tranglos.com/free/keynote.html'>http://www.tranglos.com/free/keynote.html</a>

This project in Google Code complements the one located in <a href='https://sourceforge.net/projects/keynote-newfeat/'>SourceForge</a>, hosting the source version control (Subversion) and the Issue Tracker.<br>
<br><br>

<h3>Intention</h3>
I'm working in a totally new application, based in .NET and db4o object oriented database. My intention is to add to this new application basically the same functionality of today's KeyNote and all that things that I'm really interested in. I have a very clear idea of what I want and there are many things that are very more difficult to implement in current KeyNote.<br>
This new application will be open source too. Soon I will create a new project in Google Code for that new application.<br>
<br>
As I keep using KeyNote NF intensively at work and at home, I will correct errors detected, specially in the new functionality I added, and I will incorporate new features depending on their complexity.<br>
<br>
Also, I have decided to upload source code to Subversion so that any person interested in helping or just giving suggestions can easily do it, by analizing the code, working locally with it, and suggesting patches to be applied.<br>
Of course, new members are welcome.<br>
<br><br>

<h3>Name. Compatibility</h3>
The program will be renamed from "KeyNote" to "KeyNote NF". That way I hope it will be easier to locate it searching the web.<br>
<br>
To implement some of the new features included in KeyNote NF I had to extend sligthly the format of KeyNote (.knt) files. All new elements are optional so that new version can open without problems a file corresponding to version 1.6.5.<br>
Also, with KeyNote 1.6.5 it is possible to open a file created with a later version, but in this case, if the file is saved, the alarms and information about checked nodes will be lost.<br>
<br><br>

<h3>Bugs, new Features</h3>
For any bug report, patch proposal or feature request, add an entry into the <a href='http://code.google.com/p/keynote-nf/issues/list'>Issue Tracker</a>.<br>
<br>
Please, be specific; it is preferable to create several issues instead of only one very<br>
heterogeneous, with many questions. That way it can be managed much better<br>
<br>
<b>Before report any new issue, please read the topic <a href='http://sourceforge.net/apps/phpbb/keynote-newfeat/viewtopic.php?f=15&t=20'>HOW TO: Report Bugs / Add Feature Requests </a> in the <a href='http://sourceforge.net/apps/phpbb/keynote-newfeat/index.php'>new forum of KeyNote NF</a></b>

There you can discuss about issues itselves, too.<br>
<br><br>

<h3>Installation</h3>
There is no installation project at this moment. You can copy all the files in archives "Release_XXXXXX.zip" in a new folder as KeyNote NF doesn't need installation to work.<br>
If you prefer you can install original <a href='http://www.tranglos.com/free/files/kntsetup.exe'>KeyNote (version 1.6.5)</a> and then copy only the following files (contained in zip) in the installed folder, replacing the original ones:<br>
<br>
keynote.exe, keynote.chm, alert.wav, keynote.tip, keynote.lan, doc\<code>*</code> lang\<code>*</code>