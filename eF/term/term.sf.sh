#!/bin/sh

# cd to the directory this .sh file is in
# the quotes allow spaces in the path name
# the /* means back up to the last /
# it removes the filename from the path

cd "${0%/*}"

sf \
  requires sio.f \
  include e4term.f \
  n,8,1  115200 comport /dev/ttyUSB0 \
  help \

# --useful settings--
# 9600 19200 38400 57600 115200
# 230400 460800 921600
# 921600 fails to autobaud on SRAM board
# /dev/ttyS0
# /dev/ttyS1
# /dev/ttyUSB0
# /dev/ttyUSB1


