stty -F /dev/ttyUSB0 921600 -parenb cs8 -cstopb -crtscts raw -echo
stty -F /dev/ttyUSB2 921600 -parenb cs8 -cstopb -crtscts raw -echo
./Okad2-42c-pd.exe
