Loading source files
--------------------
A program such as HyperTerm on Windows or minicom on Linux can
be used to upload source files.

An appropriate character and line delay must be set.
These work at 115200 baud.

    1 ms chardelay
  111 ms linedelay

HyperTerm does not know what to do when an eForth compile error
occurs. You will need to manually stop the upload.

e4term -- terminal emulator
---------------------------
Currently, the terminal emulator compiles only with SwiftForth.
A gforth version is planned.

The comport and baudrate are set in a batch file on Windows, and
a shell script on Linux. Edit as needed.

When e4term starts the HELP menu is displayed
Type 'T' to enter terminal mode.
Once in terminal mode, all commands are preceeded by the 'escape' key.

   *----------------------------------------+
   | Terminal              Escape and...    |
   +----------------------------------------+
   |  H  Help          ||  S  Status        |
   |  P  Pacer load    ||  R  Re-load       |
   |  E  Edit [file]   ||  L  List files    |
   |  D  Display path  ||  C  Change path   |
   |  U  Uppercase     ||  V  Verbosity     |
   |  X  eXit          ||  A  App load      |
   +----------------------------------------+
   |  Host                 T  Terminal      |
   +----------------------------------------*

Help
  display the above menu.

Status
  display the current comport, baudrate,
  chardelay and linedelay, upper and verbosity.

NOTE:
  The character delay, 'chardelay', is a "short delay"
  approximating 100 microseconds.
  chardelay is used by the Pacer load, linedelay is not.

Pacer load
  upload the selected file.
  The host uploads a line of source then waits for eForth
  to respond when done compiling the line.
  If an error occurs, eForth notifies e4term to stop sending source.
  While an upload is in progress, pressing the esc key will
  terminate the upload.

Re-load
  re-load the last uploaded file.

Edit [file]
  edit the selected file.
  After an error, edit the offending file. bbb select editor

List files
  list the files in the current directory.
  The listing is Windows or Linux specific.

Display path
  display the current path.
  The listing is Windows or Linux specific.

Change path
  type the relative path to the new directory.

Uppercase
  convert user key strokes to uppercase.

Verbosity
  when uploading a file, display:
  none -- no progress status displayed
  dots -- one dot per line uploaded
  text -- all source as uploaded

eXit
  exit terminal mode to SwiftForth or gforth

App load
  upload all the demo applications.

Running the demo applications
-----------------------------
When in e4term terminal mode, esc-a will load all demo apps.

lib-demo.e4 -- Adds the DO LOOP structure words needed for the
               following demos. It MUST  be loaded once prior
               to loading any demo.

hanoi.e4    -- Move disks between three poles
random.e4   -- Random positioning of character and color
sieve.e4    -- Eratosthenes Sieve
tetris.e4   -- Tetris the game
led-task.e4 -- pulse an LED via the iobus

Each demo displays the word to type to start that demo.

To run individual demos (for example, HANOI)
-----------------------
While in the terminal application:

1) Press esc-p to open a file select dialog.
   Select lib-demo.e4
   As the file loads you will see dots as a progress indicator.
   When the load is complete, an "ok" prompt will be displayed.

2) After lib.e4 is loaded, press esc-p again and select hanoi.e4
   As before, you will see dots to show progress, and "ok" when done.
   After the demo is loaded, type "HANOI" to run.
