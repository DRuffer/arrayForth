\	File:		$Id: PeHdr.fth,v 1.4 2005/03/31 00:23:27 druffer Exp $
\
\	Contains:	Microsoft Portable Executable and Common Object File Format Specification
\				Revision 6.0  - February 1999
\
\	Copyright:	© 2003-2005 by Apple Computer, Inc., all rights reserved.
\
\		$Log: PeHdr.fth,v $
\		Revision 1.4  2005/03/31 00:23:27  druffer
\		Use B@ (S.) where the equivalent is being done
\		
\		Revision 1.3  2005/03/24 00:12:52  druffer
\		Start updating to PE Spec Rev. 6.0
\		
\		Revision 1.2  2003/01/31 23:14:49  druffer
\		Add file support to repository
\		

248 1 0 STRUCTURE peHeader	\ holds the PE Header pointers.

\ At location 0x3c, the stub has the file offset to the Portable Executable (PE) signature
0  4 BYTES peSignature		\ PE Signature; PE followed by two zeros.
							\ The VXD header is LE and 32 bytes shorter.

\ Common Object File Format header

   NUMERIC peMachine		\ Number identifying type of target machine.
   NUMERIC peObjects		\ number of entries in Object Table.
      LONG peTime/Date		\ time and date file was created or modified.
      LONG peSymbolTable	\ offset of the Symbol Table.
      LONG peSymbols		\ number of entries in Symbol Table.
   NUMERIC peNTHeaderSize	\ number of bytes in Optional Header.
   NUMERIC peFlags			\ characteristic flag bits for the image.

\ Optional Header - Standard Fields

   NUMERIC peState			\ magic number, 010B = Normal, 0107 = ROM.
   NUMERIC peLinker			\ linker version number, Major/Minor.
      LONG peCodeSize		\ total size of code sections.
      LONG peDataSize		\ total size of initialized data sections.
      LONG peRamSize		\ total size of unitialized data sections.
      LONG peEntryPoint		\ starting RVA of the program.
      LONG peCodeBase		\ base RVA of code section.
      LONG peDataBase		\ base RVA of data section.

\ Optional Header - NT Specific Fields

      LONG peImageBase		\ preferred Virtual address of DOS Header.
      LONG peObjectAlign	\ Object alignment 2**512-256M.
      LONG peFileAlign		\ file alignment 2**512-64K.
      LONG peOSVersion		\ OS version number, Major/Minor.
      LONG peUserVersion	\ user version number, Major/Minor.
      LONG peSubsysVersion	\ subsystem version number, Major/Minor.
   4 BYTES peReserved
      LONG peImageSize		\ virtual size of image.
      LONG peHeaderSize		\ size of DOS, PE and Object headers.
      LONG peFileChecksum	\ checksum for entire file.
   NUMERIC peSubsystem		\ NT subsystem required to run.
   NUMERIC peDLLFlags		\ special loader requirements, obsolete.
      LONG peStackReserve	\ total stack space needed.
      LONG peStackCommit	\ stack space committed.
      LONG peHeapReserve	\ total heap space needed.
      LONG peHeapCommit		\ heap space committed.
      LONG peLoaderFlags	\ loader flags, obsolete.
      LONG peDataDirs		\ size of following Data Directories array.

\ Optional Header - Data Directories

    LONG peExport        LONG peExportSize		\ export table.
    LONG peImport        LONG peImportSize		\ import table.
    LONG peResource      LONG peResourceSize	\ resource table.
    LONG peException     LONG peExceptionSize	\ exception table.
    LONG peSecurity      LONG peSecuritySize	\ security table.
    LONG peRelocate      LONG peRelocateSize	\ relocation table.
    LONG peDebug         LONG peDebugSize		\ debug table.
    LONG peDesc          LONG peDescSize		\ image copyright.
    LONG peGlobalPtr     LONG peGlobalPtrSize	\ global pointer, Size=0.
    LONG peTLS           LONG peTLSSize			\ Thread Local Storage.
    LONG peConfig        LONG peConfigSize		\ load Configure table.
    LONG peResData1      LONG peResSize1		\ reserved Data 1.
    LONG peResData2      LONG peResSize2		\ reserved Data 2.
    LONG peResData3      LONG peResSize3		\ reserved Data 3.
    LONG peResData4      LONG peResSize4		\ reserved Data 4.
    LONG peResData5      LONG peResSize5		\ reserved Data 5.

CONSTANT peObjectTable

HEX

IMAGE-CONSTANTS IMAGE_FILE_MACHINE		\ describes the CPU types.

0000 IMAGE-CONSTANT IMAGE_FILE_MACHINE_UNKNOWN  	," Unknown"
014C IMAGE-CONSTANT IMAGE_FILE_MACHINE_I386     	," Intel 386"
014D IMAGE-CONSTANT IMAGE_FILE_MACHINE_I486     	," 80486"    \ Obsolete
014E IMAGE-CONSTANT IMAGE_FILE_MACHINE_PENTIUM  	," Pentium"  \ Obsolete
0162 IMAGE-CONSTANT IMAGE_FILE_MACHINE_MIPS-I   	," MIPS I"
0163 IMAGE-CONSTANT IMAGE_FILE_MACHINE_MIPS-II  	," MIPS II"  \ Obsolete
0166 IMAGE-CONSTANT IMAGE_FILE_MACHINE_R4000    	," MIPS little endian"
0168 IMAGE-CONSTANT IMAGE_FILE_MACHINE_R10000		," MIPS 10000"
0184 IMAGE-CONSTANT IMAGE_FILE_MACHINE_ALPHA    	," Alpha AXP"
01A2 IMAGE-CONSTANT IMAGE_FILE_MACHINE_SH3			," Hitachi SH3"
01A6 IMAGE-CONSTANT IMAGE_FILE_MACHINE_SH4			," Hitachi SH4"
01C0 IMAGE-CONSTANT IMAGE_FILE_MACHINE_ARM			," ARM"
01C2 IMAGE-CONSTANT IMAGE_FILE_MACHINE_THUMB		," Thumb"
01F0 IMAGE-CONSTANT IMAGE_FILE_MACHINE_POWERPC  	," Power PC little endian"
0200 IMAGE-CONSTANT IMAGE_FILE_MACHINE_IA64			," Intel IA64"
0266 IMAGE-CONSTANT IMAGE_FILE_MACHINE_MIPS16		," MIPS16"
0268 IMAGE-CONSTANT IMAGE_FILE_MACHINE_M68K     	," Motorola 68000 series"
0284 IMAGE-CONSTANT IMAGE_FILE_MACHINE_ALPHA64		," Alpha AXP 64-bit"
0290 IMAGE-CONSTANT IMAGE_FILE_MACHINE_PARISC   	," PA Risc"
0366 IMAGE-CONSTANT IMAGE_FILE_MACHINE_MIPSFPU		," MIPS with FPU"
0466 IMAGE-CONSTANT IMAGE_FILE_MACHINE_MIPSFPU16	," MIPS16 with FPU"

014C IMAGE-CONSTANT EFI_IMAGE_MACHINE_IA32		," 80386"
0200 IMAGE-CONSTANT EFI_IMAGE_MACHINE_IA64		," Itanium"
0EBC IMAGE-CONSTANT EFI_IMAGE_MACHINE_EBC		," EFI Byte Code"

DROP

\ .CPU display type of CPU this executable is intended for.

: .CPU ( n -- )
   ."  For "  peMachine NU@  IMAGE_FILE_MACHINE COMPARE-CONSTANTS  ."  CPUs" ;

\ .VERSIONS display version numbers from header.

: .VERSIONS ( -- )
   ." OS Version = "          peOSVersion  L@ 10000 /MOD .MAJOR/MINOR
   ." User Version = "      peUserVersion  L@ 10000 /MOD .MAJOR/MINOR  CR
   ." Subsys Version = "  peSubsysVersion  L@ 10000 /MOD .MAJOR/MINOR
   ." Linker version = "         peLinker NU@   100 /MOD .MAJOR/MINOR  CR ;

DECIMAL

IMAGE-CONSTANTS IMAGE_SUBSYSTEM		\ describes the OS Subsystems.

0 IMAGE-CONSTANT IMAGE_SUBSYSTEM_UNKNOWN      ," Unknown"
1 IMAGE-CONSTANT IMAGE_SUBSYSTEM_NATIVE       ," Native"
2 IMAGE-CONSTANT IMAGE_SUBSYSTEM_WINDOWS_GUI  ," Windows Graphics"
3 IMAGE-CONSTANT IMAGE_SUBSYSTEM_WINDOWS_CUI  ," Windows Character"
5 IMAGE-CONSTANT IMAGE_SUBSYSTEM_OS2_CUI      ," OS/2 Character" \ Obsolete
7 IMAGE-CONSTANT IMAGE_SUBSYSTEM_POSIX_CUI    ," Posix Character"

10 IMAGE-CONSTANT EFI_IMAGE_SUBSYSTEM_EFI_APPLICATION			," EFI Application"
11 IMAGE-CONSTANT EFI_IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER	," EFI Boot Driver"
12 IMAGE-CONSTANT EFI_IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER		," EFI Runtime Driver"

DROP

\ .SUBSYSTEM display which OS subsystem is required.

: .SUBSYSTEM ( -- )
   2 SPACES  peSubsystem NU@  IMAGE_SUBSYSTEM COMPARE-CONSTANTS
   ."  subsystem required."  CR ;

IMAGE-CONSTANTS DLL_FLAGS		\ these are obsolete.

1 IMAGE-CONSTANT ProcessInitialization ," Per-Process Initialization"
2 IMAGE-CONSTANT ProcessTermination    ," Per-Process Termination"
4 IMAGE-CONSTANT ThreadInitialization  ," Per-Thread Initialization"
8 IMAGE-CONSTANT ThreadTermination     ," Per-Thread Termination"

DROP

\ .DLLFLAGS display what type of DLL this is.

: .DLLFLAGS ( -- )
   peDLLFlags NU@ DLL_FLAGS OVER IF
      ." DLL " TEST-CONSTANTS
   ELSE  2DROP  THEN ;

\ .SIZES display given memory sizes.

: .SIZES ( c r -- )   ." Reserve " H. ." bytes, commit " H. ;

\ .STACK display stack sizes.

: .STACKS ( -- )
   peStackCommit L@ peStackReserve L@ .SIZES ." for Stack"  CR ;

\ .HEAP display heap sizes.

: .HEAP ( -- )
   peHeapCommit L@  peHeapReserve L@ .SIZES ." for Heap"  CR ;

\ .DATADIRS display Data Directory array size.

: .DATADIRS ( -- )
   ." Data Directory array size = " peDataDirs L?  CR ;

\ .RVA display Relative Virtual Address.

: .RVA ( name size c-addr len -- )
   BASE @ >R  HEX  2>R  2DUP OR IF
      2R> TYPE  ."  table "  SWAP 5 .R "h"
      ."  size " 4 .R "h"  ."  bytes."  CR
   ELSE  2R> 2DROP 2DROP  THEN  R> BASE ! ;

: .EXPORT       peExport    L@ peExportSize    L@ S" Export   " .RVA ;
: .IMPORT       peImport    L@ peImportSize    L@ S" Import   " .RVA ;
: .RESOURCE     peResource  L@ peResourceSize  L@ S" Resource " .RVA ;
: .EXCEPTIONS   peException L@ peExceptionSize L@ S" Exception" .RVA ;
: .SECURITY     peSecurity  L@ peSecuritySize  L@ S" Security " .RVA ;
: .RELOCATE     peRelocate  L@ peRelocateSize  L@ S" Relocate " .RVA ;
: .DEBUG        peDebug     L@ peDebugSize     L@ S" Debug    " .RVA ;
: .DESC         peDesc      L@ peDescSize      L@ S" Desc.    " .RVA ;
: .GLOBALPTR    peGlobalPtr L@ peGlobalPtrSize L@ S" GlobalPtr" .RVA ;
: .TLS          peTLS       L@ peTLSSize       L@ S" TLS      " .RVA ;
: .CONFIG       peConfig    L@ peConfigSize    L@ S" Config.  " .RVA ;
: .RESDATA1     peResData1  L@ peResSize1      L@ S" ResData1 " .RVA ;
: .RESDATA2     peResData2  L@ peResSize2      L@ S" ResData2 " .RVA ;
: .RESDATA3     peResData3  L@ peResSize3      L@ S" ResData3 " .RVA ;
: .RESDATA4     peResData4  L@ peResSize4      L@ S" ResData4 " .RVA ;
: .RESDATA5     peResData5  L@ peResSize5      L@ S" ResData5 " .RVA ;

HEX

IMAGE-CONSTANTS IMAGE_FILE

0001 IMAGE-CONSTANT IMAGE_FILE_RELOCS_STRIPPED     ," no relocatable items"
0002 IMAGE-CONSTANT IMAGE_FILE_EXECUTABLE_IMAGE    ," executable image"
0004 IMAGE-CONSTANT IMAGE_FILE_LINE_NUMS_STRIPPED  ," no line numbers"
0008 IMAGE-CONSTANT IMAGE_FILE_LOCAL_SYMS_STRIPPED ," no local symbols"
0010 IMAGE-CONSTANT IMAGE_FILE_MINIMAL_OBJECT      ," reserved for minimum"
0020 IMAGE-CONSTANT IMAGE_FILE_UPDATE_OBJECT       ," reserved for update"
0040 IMAGE-CONSTANT IMAGE_FILE_16BIT_MACHINE       ," 16 bit machine"
0080 IMAGE-CONSTANT IMAGE_FILE_BYTES_REVERSED_LO   ," byte swapped"
0100 IMAGE-CONSTANT IMAGE_FILE_32BIT_MACHINE       ," 32 bit machine"
0200 IMAGE-CONSTANT IMAGE_FILE_DEBUG_STRIPPED      ," no debug"
0400 IMAGE-CONSTANT IMAGE_FILE_PATCH               ," reserved for patch"
1000 IMAGE-CONSTANT IMAGE_FILE_SYSTEM              ," system file"
2000 IMAGE-CONSTANT IMAGE_FILE_DLL                 ," DLL"
8000 IMAGE-CONSTANT IMAGE_FILE_BYTES_REVERSED_HI   ," word swapped"

DECIMAL DROP

\ .FLAGS display flags.

: .FLAGS ( -- )   peFlags NU@ IMAGE_FILE TEST-CONSTANTS ;

\ .MEMORY display memory block sizes.

: .MEMORY ( n a -- )
   BASE @ >R  HEX  5 U.R "h"  ."  for "
   4 U.R "h"  ."  bytes."  R> BASE ! ;

\ .LAYOUT display memory layout.

: .LAYOUT ( -- )
   ." Entry point = "  peEntryPoint L@ H.
   ." at Image Base "  peImageBase L@ H.  CR
   ." Base of Code = "  peCodeSize L@  peCodeBase L@  .MEMORY  CR
   ." Base of Data = "  peDataSize L@  peDataBase L@  .MEMORY
   ."   Uninitialized = "  peRamSize L@ H.  ." bytes."  CR ;

\ /PEHEADER set offset of PE Header.

: /PEHEADER ( -- )
   MS-DOS-FILE IF
      PEHEADER-PTR L@ peHeader ORG !
   THEN ;

' /PEHEADER <FILE

\ .RESERVED display contents of reserved fields.

: .RESERVED ( -- )
   CR ." peReserved:" peReserved 2DUP B@ (S.) DUMP CR CR ;

\ .PEHEADER1 display first part of PE Header.

: .PEHEADER1 ( -- )
   peHeader
   ." Signature = "  peSignature B?  .CPU
   ."  with "  peObjects NU?  ." object entries."  CR
   ." Produced on "  peTime/Date L@ .TIME/DATE  CR
   ." NT Header size = " peNTHeaderSize NU@ H.
   ." bytes with State: "  peState NU@ H.  CR  .FLAGS  .LAYOUT
   ." Objects aligned on "  peObjectAlign L@ H.  ." bytes.  "
   ." File aligned on "  peFileAlign L@ H. ." bytes."  CR  .VERSIONS
   ." Image size = "  peImageSize L@ H.  ." bytes.  "
   ." Header size = "  peHeaderSize L@ H.  ." bytes."  CR
   ." Checksum = "  peFileChecksum L@ H. ;

\ .PEREST display rest of PE Header.

: .PEREST ( -- )
   .SUBSYSTEM  .DLLFLAGS  .STACKS  .HEAP  .DATADIRS
   .EXPORT  .IMPORT  .RESOURCE  .EXCEPTIONS  .SECURITY  .RELOCATE
   .DEBUG  .DESC  .GLOBALPTR  .TLS  .CONFIG  .RESDATA1  .RESDATA2
   .RESDATA3  .RESDATA4  .RESDATA5  .RESERVED ;

[R Portable Executable Header \ ] CONSTANT PE-HEADER

\ .PEHEADER display PE Header.

: .PEHEADER ( -- )   PE-HEADER HEADING  .PEHEADER1  .PEREST ;
