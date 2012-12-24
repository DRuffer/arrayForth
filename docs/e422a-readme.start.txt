Follow the Green Arrays, Inc. instructions to start up the GA144
EVB001 board. Connect your PC with a standard USB cable to the
'B-USB' connector (the middle USB connector).

Start a terminal emulator such as HyperTerm on Windows or minicom
on Linux, set to use the comport connected to 'B-USB'.

Then press 'spacebar' in the terminal to have eForth autobaud.
You should see the eForth sign-on message and 'ok' prompt:

eForth btc:20110826 ga144:20110826
ok

Or if you have SwiftForth, start the  e4term.f  program with
 term.sf.bat  (Windows) or  term.sf.sh  (Linux). SwiftForth
will start and compile the terminal application.

At the prompt type: T <enter>
Then press 'spacebar' to have eForth autobaud.
See  readme.e4term.txt  for more info.

Using eForth
------------

Forth is an interactive interpreted language.

All commands are entered with spaces between words and executed
by pressing the return key.

When commands are successfully executed, forth will reply with
'ok', otherwise an error message.

To add two numbers and print the result type: 1 2 + . <return>

The display will look like:

ok 1 2 + . 3
ok

The forth word  .  will print one item from the data stack.

Example: put 4 numbers on the data stack, type: 1 2 3 4 <return>

To display all items on the data stack,
without changing or deleting them, type: .s <return>

The display will look like:

ok .s 1 2 3 4
ok

To display a region of memory type: 0 32 dump <return>

The word  dump  takes two numbers on the data stack,
an address and a count. The result will look like:

ok 0 32 dump
   0:0  1342    0 1331    0  9E2  93B  969 D040   B   1     ; i @
   8:0  D044    0    1    2    3    0   24   45   D           $ E
  10:0    58   49   54 C00E    E   27   45   58   X I T     ' E X
  18:0    45   43   55   54   45 C029   15   23   E C U T E )   #
  20:0    52   50   40 C004   1F   63   52   50   R P @     c R P
^    ^  ^                                        ^
|    |  |                                        |
|    |  |                                        + text equivalent
|    |  + 8 cells of 16-bit data
|    + 4-bit page number
+ 16-bit address

Two good forth tutorials available online:

Starting FORTH
The online edition:
http://www.forth.com/starting-forth/

Thinking FORTH -- A Language and Philosophy for Solving Problems
Available as a downloadable pdf:
http://thinking-forth.sourceforge.net/

eForth files and folders
------------------------

ef/                 -- eForth distribution folder
  eforth.bin          -- Green Arrays eForth SRAM image file
  docs/               -- documents, reference materials
    readme.start.txt    -- this file
    readme.bat.txt      -- configure batch files
    readme.e4term.txt   -- e4term terminal emulator reference
    readme.eForth.txt   -- eForth reference
    FTDI USB Driver.txt -- configure FTDI software for Windows

  demo/               -- demo source folder
    hanoi.e4            -- demo: Move disks between three poles
    led-task.e4         -- demo: pulse an LED using the iobus
    lib-demo.e4         -- demo: extensions DO-LOOP and color
    random.e4           -- demo: Random position char and color
    sieve.e4            -- demo: Eratosthenes Sieve
    tetris.e4           -- demo: Tetris the game

  term/               -- terminal distribution folder
    e4term.f            -- terminal load file
    term.ga.bat         -- start e4term from Windows SwiftForth
    term.sf.sh          -- start e4term from Linux SwiftForth



