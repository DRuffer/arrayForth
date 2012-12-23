start "SwiftForth"  "%SFdir%\bin\SF.exe"  include "%SFdir%\lib\options\win32\sio.f"  include e4term.f  n,8,1  921600 comport com16  help

rem --useful settings--
rem com1 thru com31 or higher
rem 9600 19200 38400 57600 115200
rem 230400 460800 921600
rem 921600 fails to autobaud on SRAM board

