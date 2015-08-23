
---

Version: 1.7.8 -  date: 02/nov/2009

Enhancements:
  * [Issue #175](https://code.google.com/p/keynote-nf/issues/detail?id=#175): New KeyNote file format: compressed
  * [Issue #72](https://code.google.com/p/keynote-nf/issues/detail?id=#72): Ability to add notes to an alarm event, and other minor changes

Bugfixes:
  * [Issue #162](https://code.google.com/p/keynote-nf/issues/detail?id=#162): Changes to last node are not saved in encrypted tree notes if focus is not moved to another node
  * [Issue #163](https://code.google.com/p/keynote-nf/issues/detail?id=#163): Paste as web clip is not showing source URL
  * [Issue #166](https://code.google.com/p/keynote-nf/issues/detail?id=#166): International input is not working when renaming nodes in tree
  * [Issue #169](https://code.google.com/p/keynote-nf/issues/detail?id=#169): Searching Next with Enter Key pressed could modify note content
  * [Issue #178](https://code.google.com/p/keynote-nf/issues/detail?id=#178): Program freezes when saving/closing
  * [Issue #189](https://code.google.com/p/keynote-nf/issues/detail?id=#189): Changes on a node are lost if a new note is created before moving to another node or note



---

Version: 1.7.7 -  date: 18/oct/2009

Enhancements:
  * [Issue #139](https://code.google.com/p/keynote-nf/issues/detail?id=#139): Make KeyNote NF Unicode compliant <br> Before, within the same version:<br>
<ul><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#40'>Issue #40</a>: Find and Replace operations don't work with Unicode characters (Chinese, for example)<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#54'>Issue #54</a>: 'Paste as Text' and 'Paste as Web Clip' don't work with Unicode characters <br>
</li></ul><ul><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#73'>Issue #73</a>: Allow to enable or disable alarm popups<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#29'>Issue #29</a>: Sound for Alarm<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#135'>Issue #135</a>: Insert URL auto-clipboard-paste<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#158'>Issue #158</a>: Add support for Notes URL Protocol<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#118'>Issue #118</a>: KeyNote should admit relative paths in config file and command-line</li></ul>

<ul><li>Included a partial French translation by Picou (picou.keynote@gmail.com)<br>
</li><li>Included a partial Chinese Simplified translation by xbeta (<a href='http://xbeta.info'>http://xbeta.info</a>) <br>    In both cases .tip file isn't translated</li></ul>

Bugfixes:<br>
<ul><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#155'>Issue #155</a>: Clipboard capture error on non active (visible) note<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#141'>Issue #141</a>: Clipboard capture repeats twice the text copied, when the copy is made from certain programs<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#62'>Issue #62</a>: KeyNote NF won't open in maximized window state<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#58'>Issue #58</a>: Minimizing button does not work.<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#153'>Issue #153</a>: Display Problem on clicking Hyperlink<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#63'>Issue #63</a>: Hyperlinks not working with Unicode characters<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#115'>Issue #115</a>: Right click on URL problem<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#149'>Issue #149</a>: Add sibling command doesn't always add a sibling node<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#127'>Issue #127</a>: StyleApply macro doesn't apply specified style. Modifiers keys don't work in macro, too<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#125'>Issue #125</a>: Cursor jumps to last textual hyperlink in note each time note is saved.<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#122'>Issue #122</a>: Keynote links to another file don't work<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#116'>Issue #116</a>: Changes in mirror node get lost after saving if original node is selected (in other note)<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#101'>Issue #101</a>: Import files by dropping them on the tree<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#96'>Issue #96</a>: Search -> Find... only works on one note, won't search all notes.<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#74'>Issue #74</a>: The treeview in the options dialog is editable<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#71'>Issue #71</a>: Node can become unchecked when moving, in a special situation<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#70'>Issue #70</a>: Status bar doesn't reflect correctly the modified state of the file</li></ul>

<blockquote>While the Beta 2 was available were identified the following issues, now resolved:<br>
</blockquote><ul><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#152'>Issue #152</a>: "Allow only one instance" is not working in 1.7.7 Beta2<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#150'>Issue #150</a>: Exporting tree to file does not include headers<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#140'>Issue #140</a>: Note tabs no longer appear in 1.7.7 Beta 2 for encrypted notes files</li></ul>


<hr />
Version: 1.7.6 -  date: 27/Apr/2009<br>
<br>
<ul><li>Added a complete German translation by Klaus Utech  (<a href='http://www.klausutech.com'>http://www.klausutech.com</a>).<br>
<blockquote>Just download the file "German_Translation_Release_1.7.6.1.rar" and copy the files keynote.german.lng and keynote.german.tip to "Lang" folder,and replace keynote.lan with the one included in the zip file. <br>  Note: OPTION - menu isn't still translated. Also, .tip file (Tip of the Day) isn't translated yet, it's just a copy from the english version.</blockquote></li></ul>

<hr />
Version: 1.7.6 -  date: 15/Mar/2009<br>
<br>
Enhancements:<br>
<br>
<ul><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#57'>Issue #57</a>: New kind of virtual nodes: links to other nodes (Mirror nodes)<br>
<ul><li>You can create mirror nodes so that they share the same content, alarm and checked state. That way you can organize your information simultaneously in different ways.<br>
</li><li>With mirror nodes you can have task spread over many tabs, and at the same time include in another tabs. You will be able to have a global vision of the main things to do, sorting, ranking and structuring in a free tree hierarchy, independent of the hierarchy in wich 'real' nodes reside.<br>
</li></ul></li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#48'>Issue #48</a>: Ctr+Shift+TAB, before selecting previous page, focus on tree<br>
</li><li>Included a complete Dutch translation by Ennovy and Plankje (<a href='http://forum.goeiedageven.nl/'>http://forum.goeiedageven.nl/</a>)</li></ul>

Bugfixes:<br>
<ul><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#59'>Issue #59</a>: Certain hyperlinks may cause knt file to grow in size suddenly (geometrical increase..)<br>
<ul><li>There is a new command line option (-clean) to clean/repair a file with that problem (perhaps latent..)<br>
</li></ul></li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#50'>Issue #50</a>: Tabs context menu and Export...</li></ul>

<hr />
Version: 1.7.5 -  date: 31/Jan/2009<br>
<br>
Enhancements:<br>
<br>
<ul><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#23'>Issue #23</a>: Multilanguage version of KeyNote NF  (new configuration option). In this release is included a file with a partial translation to spanish (castilian). This serves as an example of how to translate. In the content of the issue there is more information.<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#31'>Issue #31</a>: TAB and SHIFT-TAB over multiple lines behaves like notepad++<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#32'>Issue #32</a>: Holding Ctrl+UP/DOWN allow to shift document<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#38'>Issue #38</a>: Pressing HOME key moves caret to first non-space position of line.</li></ul>

Bugfixes:<br>
<br>
<ul><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#24'>Issue #24</a>: Couldn't see the content of the nodes (DLL different from MSFTEDIT.DLL didn't work)<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#26'>Issue #26</a>: New format of links (<a href='https://code.google.com/p/keynote-nf/issues/detail?id=12'>issue 12</a>) didn't work with notes checked as "Plain text only"<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#27'>Issue #27</a>: Converting old links with the prefix "<a href='file:///?'>file:///?</a>" to the new format didn't work<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#42'>Issue #42</a>: First line indentation didn't work as should. Now indentation behaves like in MS Word: First indent is relative to left indent, not vice versa. And can be positive or negative.<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#36'>Issue #36</a>: Shift+Ctr+` (Decrease first indent) didn't work.<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#33'>Issue #33</a>: AltGr+C/V/X was ignored<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#28'>Issue #28</a>: Starting 2nd instance crashed if option "One instance only" was checked.<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#41'>Issue #41</a>: File names were always saved in lower case<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#39'>Issue #39</a>: In version 1.7.4 compiled from SVN search may not work.</li></ul>

<hr />
Version: 1.7.4 Rev.2 -  date: 02/Jan/2009<br>
<br>
Bugfixes:<br>
<br>
<ul><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#20'>Issue #20</a>: Paste with Shift-Insert: as Text<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#21'>Issue #21</a>: Changing font style of a selection</li></ul>

<hr />
Version: 1.7.4 -  date: 29/Dec/2008<br>
<br>
Enhancements:<br>
<br>
<ul><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#12'>Issue #12</a>: Improving the way links are managed. In new versions of Rich Edit is used the command RTF: <code> {\field{\*\fldinst{HYPERLINK "hyperlink"}}{\fldrslt{\cf1\ul textOfHyperlink</code> }}} Now hyperlinks can show a text different to the URL.<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#7'>Issue #7</a>: More general and improved use of usual clipboard's shortcuts: Ctrl-C, CTRL-V, Ctrl-X  / Ctrl-Ins, Shift-Ins, Shift-Supr. That way is possible to copy and paste nodes and subtrees, and move (cut and paste) inside the file.</li></ul>

Bugfixes:<br>
<br>
<ul><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#10'>Issue #10</a>: Changing First Indent of selection with several lines. Resolved in a better way, using PFM_OFFSETINDENT in RichEdit, instead of PFM_STARTINDENT. Now works ok, and don't consume the 'undo' mechanism.<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#9'>Issue #9</a>: Pressing Intro in an empty line with bullet or numbering list style, should cause list style to be removed..<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#13'>Issue #13</a>: Behaviour of lists styles: When applying list styles, the first indent of list (bullet or numbering) is not homogeneous<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#14'>Issue #14</a>: Context menu for Note tabs didn't appear under cursor<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#15'>Issue #15</a>: It is allowing RETURN even in read-only mode<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#16'>Issue #16</a>: Failed to save after erasing a node with hidden childs<br>
</li><li><a href='https://code.google.com/p/keynote-nf/issues/detail?id=#18'>Issue #18</a>: Inserting a node doesn't always shows ok if the previous one is hidden.</li></ul>

<hr />
03 october 2008 - version 1.7.3.3<br>
<br>
<ul><li>Fixed problem with function "Insert URL..." and "Insert Link to file...": it didn't  work. It just transported the first letter of the url. On correcting the cause of this error it has been also resolved the problem with option "Show word count in status bar". The behaviour of the status bar is now the usual in KeyNote.</li></ul>

<hr />
02 october 2008 - version 1.7.3.2<br>
<br>
<ul><li>Issue related to Paste action was not correctly resolved. It could generate exceptions in certain situations caused by the behaviour of "msftedit.dll". I have implemented a different solution that never needs to open modal window PasteSpecial.</li></ul>

<hr />
30 september 2008 - version 1.7.3<br>
<br>
<ul><li>Fixed problem with searches: using "msftedit.dll" messages EM_FINDTEXTEX doesn't work correctly. Instead I have used EM_FINDTEXTEXW, that uses search strings in Unicode.</li></ul>

<ul><li>Fixed issue related to Paste action: sometimes Paste was ignored, while Paste special still worked. Now that situation is detected and the actions followed by the program are:<br>
<ul><li>first to try to paste in Rich Text Format (like PasteSpecial but without UI, and selecting directly the format)<br>
</li><li>If action one didn't work (in very rare ocassions) then KeyNote will open modal window of PasteSpecial. And this allways work.</li></ul></li></ul>

<hr />
28 september 2007 - version 1.7.2<br>
<br>
<ul><li>Added: Now, you can use and show tables correctly inside KeyNote<br>
<ul><li>This improvement is available from Windows XP SP1, by means of "msftedit.dll"<br>
</li><li>At the moment, to control the design of these tables you must use other programs like Excel or Word and paste then into your keynote file<br>
</li><li>This functionality is actually incompatible with option "Show word count in status bar" The option is simply ignored.</li></ul></li></ul>

<ul><li>Minor improvements in alarm management:<br>
<ul><li>You can close modal window with ESC<br>
</li><li>When you postpone an alarm, next visible alarm is selected and option applied is mantained as default for next one.<br>
</li></ul></li><li>Fixed exception on Delete Style Definition. This error appeared when migration to Delphi 2006.</li></ul>


<hr />
23 july 2008 - version 1.7.1 (Rev. 4)<br>
<ul><li>Fixed exception raised when opening KeyNote with impresion queue stopped (server RPC stopped)</li></ul>

<hr />
16 july 2008 - version 1.7.1 (Rev. 3)<br>
<ul><li>Fixed error relative to alarm setting: it raised an error on opening a document with alarms from another document with alarms, too (also, on reopening a document when was modified outside KeyNote)</li></ul>

<hr />
25 November 2007 - version 1.7.1<br>
<ul><li>Added: setting of alarms, associated to nodes.</li></ul>

<hr />
13 November 2007 - version 1.7.0<br>
<br>
<ul><li>Added: Capacity to work with hidden nodes. Nodes can be hidden in two ways:<br>
<ul><li>Activating a mode wich automatically hides checked nodes (Show or Hide checked nodes)<br>
</li><li>Filtering one note's nodes or all notes under a searching criterion (Filter Tree Note)<br>
</li></ul><blockquote>In searches (and filters) it is possible to select if to consider or not hidden nodes.<br>
If you click on a result relative of a hidden node, that node will be shown.<br>
</blockquote></li><li>Added: Selecting checkboxes for all nodes is still posible. (View/Tree Checkboxes -- now View/All nodes Checkboxes) Besides, checkboxes can be shown only on children of selected nodes: (Children Checkbox)</li></ul>

<ul><li>Fixed error that could affect searches, exports and sending of mail: in rare ocassions one node's content could prevent next nodes to be considered.<br>
</li><li>Fixed: checked state of a node is now mantained when moved (by drag and drop or by Shift and cursors) and also when nodo or subtree is transferred. At the moment, if a node with visible checkbox is moved to a parent node that doesn't show his children checkboxes, this moved node will not show its checkbox (but the checked state is mantained, it isn't lost)</li></ul>

<ul><li>Changed behaviour of ESC in Scratch window: instead of focusing note's control editor, keynote is minimized.