\ ExeHdr.fth - Facility to display contents of executable headers

\ This file supports the display and manipulation of the entries in
\ Microsoft's Portable Executable Files.

\ The Tool Interface Standards Formats Specification for Windows, Version
\ 1.0 is the result of the work of the TIS Committee, an association of
\ members of the microcomputer industry formed to work toward
\ standardization of the software interfaces visible to development tools
\ for 32-bit Intel X86 operating environments.

\ TIS Committee members include representatives from Borland
\ International, IBM, Intel, Lotus, MetaWare, Microsoft, The Santa Cruz
\ Operation, and WATCOM International. PharLap Software and Symantec also
\ participated in the specification definition efforts.

\ PE File Structures:

\ exeHeader    MS-DOS 2.0 program header.
\ exeImage     MSDOS executable image.
\ peHeader     PE Header pointers.
\ objHeader    Object Table.
\ IMAGE        Redefined for each Object.
\ expDirectory Export Directory Table.
\ impDirectory Import Directory Table.

\ The offset and size of these structures will be adjusted when the
\ initialization chain is executed.

INCLUDE ../../File.fth  FALSE REVERSE !  LITTLE-ENDIAN  TRUE BIND-FIELDS !

APP" Tool Interface Standards Formats Specification"

INCLUDE Support.fth     \ Support for EXE display
INCLUDE DosHdr.fth      \ MS-DOS header support
INCLUDE PeHdr.fth       \ PE Header support
INCLUDE Objects.fth     \ Object Table support
INCLUDE Images.fth      \ Image Page support
INCLUDE Exports.fth     \ Export Directory Table
INCLUDE Imports.fth     \ Import Directory Table
INCLUDE Relocate.fth    \ Relocation Table support
INCLUDE Resource.fth    \ Resource Directory Table

[R Executable Header \ ] CONSTANT EXE-HEADER

\ .HEADER display all the header fields.

: .HEADER ( -- )
   MS-DOS-FILE IF
      EXE-HEADER LAYOUT  .EXEHEADER  .PEHEADER  .OBJECTS
      .EXPORTS  .IMPORTS  .RESOURCES  .FIXUPS
   ELSE 1 ABORT" Unknown file type"
   THEN ;
