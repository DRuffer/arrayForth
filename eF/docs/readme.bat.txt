Windows XP
==========

Create an environmental variable, named "SFdir", pointing to your
 SwiftForth installation.

To create an environmental variable:
+ Start (menu)
+ Settings
+ Control Panel
+ System
+ Advanced (tab)
+ Environmental Variables
+ System variables
+ New
+ Variable name: SFdir
+ Variable value: <path to your installation of SwiftForth>
= example: c:\forthinc\swiftforth

--OR--

! assuming 'My Computer' is on the Windows desktop
+ right click 'My Computer'
+ Properties
+ Advanced (tab)
+ Environmental Variables
+ System variables
+ New
+ Variable name: SFdir
+ Variable value: <path to your installation of SwiftForth>
= example: c:\forthinc\swiftforth

The batch file  make.sf.bat  looks like:

start "SwiftForth" "%SFdir%\bin\SF.exe" load.sf

Linux
=====

No changes needed.

make.gf.sh
make.sf.sh

