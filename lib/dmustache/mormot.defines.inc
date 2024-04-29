{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

  Define a centralized set of conditional defines, included in all our
  framework units, and could be used also for your own private units.
}


(********************** User-Trigerred Conditionals **********************)

{  Those conditionals below can be enabled in your project Options,
   to tune the compilation depending your setup or your project expectations. }

{.$define USEPACKAGES}
// define this if you compile the unit within a Delphi package
// - it will avoid error like "[DCC Error] E2201 Need imported data reference
// ($G) to access 'VarCopyProc'"
// - shall be set at the package options level, and left untouched by default
// - note: you should probably also set "Generate DCUs only" in Project Options
// -> Delphi Compiler -> Output C/C++ -> C/C++ output file generation

{.$define PUREMORMOT2}
// if defined, no mORMot 1.18 compatilibity types nor functions would be enabled
// - by default, existing projects should (almost) compile with mORMot 2
// - you should eventually define this conditional to make a perfect code
// conversion to the new types and methods definitions

{.$define NEWRTTINOTUSED}
// if defined, the new RTTI (available since Delphi 2010) won't be linked to
// the executable: resulting file size will be much smaller, and mORMot won't
// be affected (unless you use the enhanced RTTI for record/dynamic array JSON
// serialization) - left undefined by default to ensure minimal impact

{.$define FPCUSEVERSIONINFO}
// link low-level fileinfo/machoreader/elfreader for TFileVersion/TExeVersion
// from mormot.core.os.pas => disabled by default, to preserve code size

{.$define NOSETTHREADNAME}
// if defined, SetThreadName() would not raise the exception used to set the
// thread name: to be defined if you have issues when debugging your application

{.$define NODIRECTTHREADMANAGER}
// on POSIX, omit direct GetThreadManager() API calls and just use RTL functions

{.$define NOEXCEPTIONINTERCEPT}
// if defined, exceptions shall not be intercepted nor logged

{.$define NOPATCHVMT}
// disable the vmtAutoTable slot runtime patch, replacing it with Rtti.FindType
// - is likely to be defined with the NOPATCHRTL conditional
// - could be used e.g. when in-memory executables can't be patched (e.g. on
// security constrained systems, or on OS without any mmap support)

{.$define NOPATCHRTL}
// if defined, FPC RTL won't be patched on x86_64 with optimized asm
// - is likely to be defined with the NOPATCHVMT conditional
// - you can enable it if you find out some compatibility problem

{$ifdef CPUX86}
  {.$define HASNOSSE2} // force x87 code on very very old CPU
{$endif CPUX86}

{.$define NOSYNDBZEOS}
// make mormot.db.sql.zeos.pas a "void" unit - defined for FPC/Lazarus packages only

{.$define NOSYNDBIBX}
// make mormot.db.sql.ibx.pas a "void" unit - defined for FPC/Lazarus packages only

{.$define MONGO_OLDPROTOCOL}
// may be used with old MongoDB instances < 3.6 with no OP_MSG in Wire protocol

{.$define DISABLEAPPSQL}
// mormot.db.sql external DB won't be linked to the executable by mormot.app

{.$define DISABLEAPPNOSQL}
// Mongo DB client won't be linked to the executable by mormot.app

{.$define NOSSPIAUTH}
// disable Windows mormot.lib.sspi support in mormot.rest.client/server units

{.$define NOGSSAPIAUTH}
// disable Posix mormot.lib.gsssapi support in mormot.rest.client/server units

{.$define NOPOINTEROFFSET}
// disable TOrmTable offsets on 64-bit if your memory is huge or fragmented
// note: FPCMM_MEDIUM32BIT may need this for data >256KB

{.$define NOTORMTABLELEN}
// disable TOrmTable internal fLen[] pre-computed table - slower but less memory

{.$define NOSQLITE3STATIC}
// disable static SQlite3 linking - to force use external dll/so library

{.$define NOSQLITE3FPUSAVE}
// disable SetFpuFlags() calls during SQlite3 engine - may enhance performance

{.$define DEBUGSTORAGELOCK}
// could be defined to force verbose log of StorageLock/StorageUnLock

{.$define DEBUGSQLVIRTUALTABLE}
// could be defined to force verbose log of SQlite3 virtual table query planner costs

{.$define ONLYUSEHTTPSOCKET}
// is defined on POSIX but may be enabled on Windows if socket+SChannel is enough

{.$define OLDLIBC}
// use only oldest libc API, e.g. accept() instead of accept4()

{.$define OLDLINUXKERNEL}
// use only oldest Linux syscalls - as in FPC RTL

{.$define USE_OPENSSL}
// is defined on POSIX (with late binding) but may be enabled on Windows too

{.$define USELIBCURL}
// define cross-platform libcurl for https
// (when our native socket+OpenSSL/SChannel doesn't seem to be good enough)

{.$define LIBCURLMULTI}
// enable the more advanced "multi session" API functions of mormot.lib.curl
// see https://curl.haxx.se/libcurl/c/libcurl-multi.html interface


{$ifdef FPC}

(********************** FPC Conditionals **********************)

  // -----------
  // -- global code generation conditionals

  // note: you may remove all unexpexted hints by setting in your project options
  // -vm11047,6058,5092,5091,5060,5058,5057,5028,5024,5023,4081,4079,4055,3187,3124,3123

  // disable some no-brainer warnings - let FPC align with Delphi in that matter
  {$WARN 5089 off} // uninitialized managed variables 1
  {$WARN 5091 off} // uninitialized managed variables 2
  {$WARN 5093 off} // function result variable of a managed uninitialized 1
  {$WARN 5094 off} // function result variable of a managed uninitialized 2
  {$WARN 6058 off} // call to subroutine marked as inline is not inlined

  {$ifndef FPC_DELPHI}
    {$MODE DELPHI} // e.g. for asm syntax - disabled for FPC 2.6 compatibility
  {$endif FPC_DELPHI}

  {$INLINE ON}
  {$MINENUMSIZE 1}
  {$PACKRECORDS DEFAULT} // force normal alignment
  {$PACKSET 1}
  {$PACKENUM 1}
  {$CODEPAGE UTF8} // otherwise unexpected behavior occurs in most cases
  {$OBJECTCHECKS OFF} // as expected e.g. when hooking classes

  {$define HASINLINE}
  {$define HASINLINEWINAPI} // Delphi has troubles inlining Windows API calls
  {$define HASSAFEINLINE}   // Delphi 2007-2009 have troubles inlining
  {$define NODELPHIASM}     // avoid e.g. low-level System.@LStrFromPCharLen
  {$define HASDYNARRAYTYPE} // eltype2 field, used e.g. for T*ObjArray
  {$define HASITERATORS}
  {$define HASIMPLICITOPERATOR}
  {$define HASDBFTWIDE}
  {$define HASTTHREADSTART}
  {$define HASINTERFACEASTOBJECT}
  {$define EXTENDEDTOSHORT_USESTR} // FloatToText uses str() in FPC
  {$define DOUBLETOSHORT_USEGRISU} // fast DoubleToAscii()
  {$define USERECORDWITHMETHODS}   // use "object" only for Delphi 7
  {$define FPC_OR_UNICODE}
  {$define FPC_OR_DELPHIXE}        // to circumvent Delphi internal errors
  {$define FPC_OR_DELPHIXE4}


  // -----------
  // -- identify FPC versions

  // $if FPC_FULLVERSION>20700 breaks Delphi 6-7 and SynProject :(
  {$ifdef VER2_7}
    {$define ISFPC27}
  {$endif VER2_7}
  {$ifdef VER3_0}
    {$define ISFPC27}
    {$define ISFPC30}
    {$define HASDIRECTTYPEINFO}
    // PTypeInfo would be stored with no pointer de-reference
    // => Delphi and newer FPC uses a pointer reference to ease exe linking
  {$endif VER3_0}
  {$ifdef VER3_1} // trunk before 3.2
    {$define ISFPC27}
    {$define ISFPC30}
    {.$define HASDIRECTTYPEINFO}
    // define this for trunk revisions older than June 2016 - see
    // http://wiki.freepascal.org/User_Changes_Trunk#RTTI_Binary_format_change
  {$endif VER3_1}
  {$ifdef VER3_1_1}
    {$define ISFPC32}
  {$endif VER3_1_1}
  {$ifdef VER3_2}
    {$define ISFPC27}
    {$define ISFPC30}
    {$define ISFPC32}
    {$define HASGETTYPEKIND}
    {$ifdef VER3_2_2}
      {$define HASTTHREADTERMINATESET} // introduced TThread.TerminateSet
    {$endif VER3_2_2}
    {$ifdef VER3_2_3}
      {$define HASTTHREADTERMINATESET}
    {$endif VER3_2_3}
  {$endif VER3_2}
  {$ifdef VER3_3} // trunk before 3.2
    {$define ISFPC27}
    {$define ISFPC30}
    {$define ISFPC32}
    {$define HASTTHREADTERMINATESET} // introduced TThread.TerminateSet
  {$endif VER3_3}
  {$ifdef VER3_4}
    {$define ISFPC27}
    {$define ISFPC30}
    {$define ISFPC32}
    {$define ISFPC34}
    {$define FPC_PROVIDE_ATTR_TABLE} // introducing TTypeData.AttributeTable
    {$define STRCNT32}               // 32-bit TAnsiRec.Ref even on 64-bit CPU
    // see https://gitlab.com/freepascal.org/fpc/source/-/issues/38018
    {$define HASTTHREADTERMINATESET} // introduced TThread.TerminateSet
  {$endif VER3_4}
  {$if not defined(VER3_0) and not defined(VER3_2) and not defined(VER2)}
    {.$define FPC_PROVIDE_ATTR_TABLE} // to be defined since SVN 42356-42411
    // on compilation error in mormot.core.Rtti, undefine the above conditional
    // see https://lists.freepascal.org/pipermail/fpc-announce/2019-July/000612.html
    {$define STRCNT32} // new trunk feature: 32-bit TAnsiRec.Ref even on 64-bit CPU
  {$ifend}


  // -----------
  // -- identify Operating Systems

  // mainly OSWINDOWS or OSPOSIX (OSLINUX, OSBSD, OSDARWIN, OSANDROID)

  {$ifdef MSWINDOWS}
    // conditionals for Windows
    {$define OSWINDOWS}
    {$define FPCWINDOWS}
  {$else}
    {$define OSPOSIX} // a POSIX/BSD system
    {$define FPCPOSIX}
    {$ifdef BSD}
      // conditionals for Darwin and BSD family like OpenBSD/FreeBSD
      {$define OSBSDDARWIN}     // OSBSDDARWIN = OSBSD + OSDARWIN
      {$ifdef DARWIN}
        {$define OSDARWIN}
        {$define FPCDARWIN}
        {$ifdef CPUINTEL}
          {$define FPC_PIC}     // may have not be defined in compiler options
        {$endif CPUINTEL}
        {$define NOPATCHRTL}    // don't mess with asm stuff
        {$define DISABLE_SSE42} // circumvent clang asm bugs
      {$else}
        {$define OSBSD}         // OSX has some non-standard API calls
        {$define FPCBSD}
        {$ifdef OPENBSD}
          {$define OSOPENBSD}   // OSBDS = OSOPENBSD + OSFREEBSD
          {$ifdef CPUX86}
            {$define FPC_PIC}
          {$endif CPUX86}
        {$endif OPENBSD}
        {$ifdef FREEBSD}
          {$define OSFREEBSD}
        {$endif FREEBSD}
      {$endif DARWIN}
    {$else}
      {$ifdef LINUX}
        // conditionals for Linux
        {$define OSLINUX}       // e.g. to disable epoll API
        {$define FPCLINUX}
        {$define OSLINUXANDROID}
        {$ifdef CPUX64}
          {$define OSLINUXINTEL}
          {$define OSLINUXX64}
        {$endif CPUX64}
        {$ifdef CPUX86}
          {$define OSLINUXINTEL}
          {$define OSLINUXX86}
        {$endif CPUX86}
      {$else}
        {$ifdef ANDROID}
          // conditionals for Android
          {$define OSANDROID}
          {$define FPCANDROID}
          {$define OSLINUXANDROID}
          {$define NOPATCHRTL}  // don't mess with asm stuff
          {$ifdef CPUX86}
            {$define FPC_PIC}
          {$endif CPUX86}
          {$ifdef CPUAARCH64}
            {$define OSANDROIDARM64}
          {$endif CPUAARCH64}
        {$else}
        'Unsupported Operating System - yet'
        {$endif ANDROID}
      {$endif LINUX}
    {$endif BSD}
  {$endif MSWINDOWS}


  // -----------
  // -- identify CPU Architectures

  {$define FPC_SINGLEABI}      // only on i386 default ABI <> cdecl
  {$ifdef CPU64}
    // 64-bit Architecture
    {$define FPC_64}
    {$ifdef CPUX64}
      {$ASMMODE INTEL}         // as Delphi expects
      {$define CPUINTEL}
      {$define FPC_CPUINTEL}
      {$define FPC_CPUX64}
      {$ifndef OSDARWIN}       // MachOS has troubles with our asm
        {$define FPC_ASMX64}
        {$define ASMX64}       // supports asm with globals
        {$define ASMINTEL}     // either ASMX86 or ASMX64
        {$define ASMX64AVX}    // supports AVX/AVX2/AVX512
        {$define ASMX64AVXNOCONST} // supports AVX with no align32 constant load
        {$define CPUX64ASM}    // FPC has no problem (not Delphi prior XE7)
        {$define HASAESNI}     // mormot.crypt.core rejected by Darwin asm
      {$endif OSDARWIN}
    {$endif CPUX64}
    {$ifdef CPUAARCH64}
      {$define CPUARM3264}
    {$endif CPUAARCH64}
  {$else}
    // 32-bit Architecture
    {$define FPC_32}
    {$ifdef CPUX86}
      {$ASMMODE INTEL}         // as Delphi expects
      {$define CPUINTEL}
      {$define FPC_CPUINTEL}
      {$define FPC_X86}
      {$ifndef FPC_PIC}        // MachOS/OpenBSD/Android require PIC on i386
        {$define ASMX86}       // supports asm with globals
        {$define ASMINTEL}     // either ASMX86 or ASMX64
        {$define CPUX86NOTPIC} // use "absolute" instead of local register
        {$define HASAESNI}
      {$endif FPC_PIC}
      {$define TSYNEXTENDED80} // only 32-bit has a true x87 extended type
      {$undef FPC_SINGLEABI}   // on i386, cdecl <> register convention
    {$endif CPUX86}
    {$ifdef CPUARM}
      {$define CPUARM3264}
    {$endif CPUARM}
    {$define STRCNT32}         // 32-bit TAnsiRec.Ref
    {$define DACNT32}          // 32-bit TDynArrayRec.refCnt
  {$endif CPU64}
  {$ifndef CPUX64}
    {$undef FPC_X64MM}         // x86_64 only unit
  {$endif CPUX64}


  // -----------
  // -- compiler-specific code generation conditionals

  {$ifdef ISFPC32}
    // FPC has its own RTTI layout only since late 3.x
    {$define FPC_NEWRTTI}
    // when http://bugs.freepascal.org/view.php?id=26774 has been fixed
    {$define HASINTERFACERTTI}
    // generics support seems good in FPC 3.2+ but triggers linking issues
    {$define HASGENERICS}
    {$define ORMGENERICS}
    {$define FPCGENERICS}
  {$endif}
  // FPC generics (aka parameterized types) are available since 2.6
  {$define HASGENERICSSYNTAX}

  {$ifdef FPC_NEWRTTI}
    {$define ISDELPHI2010_OR_FPC_NEWRTTI}
  {$else}
    {$define DELPHI_OR_FPC_OLDRTTI}
    {$define FPC_OLDRTTI}
  {$endif}

  {$ifdef FPC_HAS_CPSTRING}
    // see http://wiki.freepascal.org/FPC_Unicode_support
    {$define HASCODEPAGE} // UNICODE means {$mode delphiunicode}
  {$endif FPC_HAS_CPSTRING}

  {$ifdef ISFPC27}
    {$define ISFPC271}
    {$define HASVARUSTRING}
    {$define HASVARUSTRARG}
    // defined if the http://mantis.freepascal.org/view.php?id=26773 bug is fixed
    // you should use 2.7.1/trunk branch in revision 28995 from 2014-11-05T22:17:54
    // => this will change the TInvokeableVariantType.SetProperty() signature
    {$define FPC_VARIANTSETVAR}
  {$endif ISFPC27}

  {$if defined(FPC_USE_WIN32_SEH) or defined(FPC_USE_WIN64_SEH)}
    {.$define HASFASTTRYFINALLY}
    // FPC SEH is not fully efficient and generate some additional code/calls
  {$ifend}

{$else FPC}

(********************** Delphi Conditionals **********************)

  {$ifndef MSWINDOWS}
  'Kylix or Delphi for MacOS/Linux/Mobile are unsupported'
  '-> we recommend using FPC for POSIX platforms'
  {$endif MSWINDOWS}

  {$define OSWINDOWS}

  {$ifndef CONDITIONALEXPRESSIONS}
  'Delphi 2-5 are not supported'
  {$endif CONDITIONALEXPRESSIONS}
  {$ifdef VER140}
  'Delphi 6 is not supported'
  {$endif VER140}

  {$A+} // force normal alignment, as expected by our units

  {$ifdef UNICODE}
    {$define HASVARUSTRING}
    {$define HASCODEPAGE}
    {$define FPC_OR_UNICODE}
    {$define USERECORDWITHMETHODS}
    {$define HASGENERICSSYNTAX}     // Delphi 2009+ compiler has TArray<>
    { due to a bug in Delphi 2009+, we need to fake inheritance of record,
      since TDynArrayHashed = object(TDynArray) fails to initialize
      http://blog.synopse.info/post/2011/01/29/record-and-object-issue-in-Delphi-2010 }
    {$define UNDIRECTDYNARRAY}
  {$else}
    {$define HASNOSTATICRTTI}        // Delphi 7/2007 has no TypeInfo(TGuid)
  {$endif}

  {$define ISDELPHI}
  {$define CPUINTEL}          // Delphi only for Intel by now
  {$define ASMINTEL}          // either ASMX86 or ASMX64
  {$undef FPC_X64MM}          // FPC only unit
  {$ifdef CPUX64}
    {$undef CPU32}
    {$define CPU64}           // Delphi compiler for 64 bit CPU
    {$define CPU64DELPHI}
    {$define ASMX64}          // supports asm with globals
    {$define EXTENDEDTOSHORT_USESTR} // FloatToText() slower in Delphi Win64
    {$define DOUBLETOSHORT_USEGRISU} // fast DoubleToAscii() - not Delphi Win32
  {$else CPUX64}
    {$define CPU32}           // Delphi compiler for 32 bit CPU
    {$undef CPU64}
    {$define CPU32DELPHI}
    {$define CPUX86}          // for compatibility with older versions of Delphi
    {$define ASMX86}          // supports asm with globals
    {$define CPUX86NOTPIC}    // use "absolute" instead of local register
    {$define TSYNEXTENDED80}  // only 32-bit has a true x87 extended type
  {$endif CPUX64}

  {$define DELPHI_OR_FPC_OLDRTTI}
  {$define HASINTERFACERTTI}  // interface RTTI (not oldest FPC)
  {$define HASFASTTRYFINALLY} // Delphi Win32/Win64 efficiently uses SEH
  {$define STRCNT32}          // 32-bit TStrRec.refCnt even on 64-bit CPU
  {$define DACNT32}           // 32-bit TDynArrayRec.refCnt even on 64-bit CPU
  {$warn UNSAFE_CODE OFF}     // Delphi for .Net does not exist any more!
  {$warn UNSAFE_TYPE OFF}
  {$warn UNSAFE_CAST OFF}
  {$warn DUPLICATE_CTOR_DTOR OFF} // avoid W1029 unneeded hints
  {$warn SYMBOL_PLATFORM OFF}
  {$warn SYMBOL_DEPRECATED OFF}   // for faVolumeID
  {$warn UNIT_PLATFORM OFF}

  {$if CompilerVersion >= 17}     // = Delphi 2005
    {$define ISDELPHI2005ANDUP}
    {$if CompilerVersion >= 18}
      {$define ISDELPHI2006ANDUP} // = Delphi 2006
      {$define HASNEWFILEAGE}
      {$define HASINLINE}
      {$define HASINLINEDELPHI}
      {$define HASREGION}
      {$define HASFASTMM4}
      // you can define this so that GetMemoryInfo/TSynMonitorMemory returns
      // low-level FastMM4 information
      {.$define WITH_FASTMM4STATS}
    {$ifend}
    {$ifdef VER180}               // = Delphi 2006
      {$define ISDELPHI20062007}  // to circumvent some specific bugs
    {$endif}
    {$ifdef VER185}               // = Delphi 2007
      {$define ISDELPHI20062007}
    {$endif}
    {$if CompilerVersion > 18}
      {$define ISDELPHI2007ANDUP} // = Delphi 2007 or newer
      {$define HASITERATORS}
      {$define HASDBFTWIDE}
    {$ifend}
    {$if CompilerVersion = 20}    // = Delphi 2009
      {$define ISDELPHI2009}      // to circumvent some specific bugs
      {$define ISDELPHI20092010}
      {$define HASNOSTATICRTTI}   // has no TypeInfo(TGuid)
    {$ifend}
    {$if CompilerVersion = 21}    // = Delphi 2010
      {$define ISDELPHI20092010}  // to circumvent some specific bugs
    {$ifend}
    {$if CompilerVersion >= 21.0}
      {$define HASSAFEINLINE}     // Delphi 2007-2009 have troubles inlining :(
      {$define ISDELPHI2010}
      {$define HASDYNARRAYTYPE}   // eltype2 field, used e.g. for T*ObjArray
      {$define HASEXTRECORDRTTI}
      {$define HASIMPLICITOPERATOR}  // Delphi 2010+ "implicit operator" is ok
      {$define ISDELPHI2010_OR_FPC_NEWRTTI}
      {$define HASTTHREADSTART}
      {$define HASINTERFACEASTOBJECT}
      {$ifdef NEWRTTINOTUSED}     // to reduce EXE size by disabling some RTTI
        {$WEAKLINKRTTI ON}
        {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
      {$endif NEWRTTINOTUSED}
    {$ifend}
    {$if CompilerVersion >= 22.0} // = Delphi XE
      {$define FPC_OR_DELPHIXE}   // Delphi 2007/2009/2010 inlining bugs
      {$define ISDELPHIXE}
      {$define HASGENERICS}       // somewhat unusable generics (?)
      {$define ORMGENERICS}
    {$ifend}
    {$if CompilerVersion >= 23.0} // = Delphi XE2
      {$define ISDELPHIXE2}
      // Delphi XE2 has some cross-platform features
      // e.g. {$ifdef NEEDVCLPREFIX}VCL.Graphics{$else}Graphics{$endif}
      {$define NEEDVCLPREFIX}
      {$define HASVARUSTRARG}
      {$define HASTTHREADTERMINATESET} // introduced TThread.TerminateSet
    {$ifend}
    {$if CompilerVersion >= 24.0} // = Delphi XE3
      {$define ISDELPHIXE3}
    {$ifend}
    {$if CompilerVersion >= 25.0} // = Delphi XE4
      {$define ISDELPHIXE4}
      {$define FPC_OR_DELPHIXE4}  // circumvent Internal Error: C2130 on XE3
      {$define HASAESNI}
      {$define HASALIGN}          // .align ### inline assembler directive
    {$ifend}
    {$if CompilerVersion >= 26.0} // = Delphi XE5
      {$define ISDELPHIXE5}
      {$define PUBLISHRECORD}
      // if defined, will handle RTTI available only since Delphi XE5 for
      // record published properties
    {$ifend}
    {$if CompilerVersion >= 27.0} // = Delphi XE6
      {$define ISDELPHIXE6}
    {$ifend}
    {$if (CompilerVersion = 27.0) or
         (CompilerVersion = 28.0)} // = Delphi XE6 or XE7
      // avoid internal error G2515 or F2084 AV0044FF4E-R00000008-0
      {$undef HASGENERICS}
      {$undef ORMGENERICS}
    {$ifend}
    {$if CompilerVersion >= 28.0}
      {$define ISDELPHIXE7}
      {$ifdef CPU64}
        {$define CPUX64ASM}       // e.g. XE4 SSE asm is buggy :(
      {$endif CPU64}
    {$ifend}
    {$if CompilerVersion >= 29.0} // = Delphi XE8
      {$define ISDELPHIXE8}
      {$define HASGETTYPEKIND}    // generics intrinsics are buggy before XE8
    {$ifend}
    {$if CompilerVersion >= 30.0} // = Delphi 10
      {$define ISDELPHI10}
    {$ifend}
    {$if CompilerVersion >= 31.0} // = Delphi 10.1
      {$define ISDELPHI101}
    {$ifend}
    {$if CompilerVersion >= 32.0} // = Delphi 10.2
      {$define ISDELPHI102}
      {$ifdef CPUX64}
      {$ifdef VER320withoutAprilUpdate}
        // circumvent early Delphi 10.2 Tokyo Win64 compiler bug
        {$undef HASINLINE}
      {$endif}
      {$endif}
    {$ifend}
    {$if CompilerVersion >= 33.0} // = Delphi 10.3
      {$define ISDELPHI103}
    {$ifend}
    {$if CompilerVersion >= 34.0} // = Delphi 10.4
      {$define ISDELPHI104}
    {$ifend}
    {$if CompilerVersion >= 35.0} // = Delphi 11.x Alexandria
      {$define ISDELPHI11}
      {$ifdef CPU64}
        {.$define ASMX64AVX}      // initial AVX/AVX2/AVX512 support - but broken
        {$define ASMX64AVXNOCONST} // supports AVX with no align32 constant load
      {$endif CPU64}
    {$ifend}
    {$if CompilerVersion >= 36.0} // = Delphi 12 Athens
      {$define ISDELPHI12}
    {$ifend}
    {$if CompilerVersion >= 37.0} // = Delphi 13 Next
      {$define ISDELPHI13}
    {$ifend}
  {$else}
     {$define ISDELPHI7}
  {$ifend CompilerVersion >= 17}

{$endif FPC}


(********************** Shared Conditionals **********************)

// -----------
// -- about pascal code expectations

  {$H+} // we use long strings
  {$R-} // disable Range checking in our code
  {$S-} // disable Stack checking in our code
  {$X+} // expect extended syntax
  {$W-} // disable stack frame generation
  {$Q-} // disable overflow checking in our code
  {$B-} // expect short circuit boolean
  {$V-} // disable Var-String Checking
  {$T-} // Typed @ operator
  {$Z1} // enumerators stored as byte by default
  {$P+} // Open string params

  {$ifdef DEBUG}
    {$assertions on} // assert() may be disabled, e.g. on FPC
  {$endif DEBUG}


// -----------
// -- CPU specific conditionals

{$ifdef CPUINTEL}
  {$ifdef OSWINDOWS}
    {$ifdef CPUX64}
      {$define WIN64ABI}    // for asm on x86_64
      {$define OSWINDOWS64} // Win64
    {$else}
      {$define OSWINDOWS32} // Win32
    {$endif CPUX64}
    {$define THREADID32}    // TThreadID = 32-bit DWORD on Win32 and Win64
  {$endif OSWINDOWS}
  {$ifdef OSPOSIX}
    {$ifdef CPUX64}
      {$define SYSVABI}     // for asm on x86_64
    {$else}
      {$define THREADID32}  // TThreadID = PtrUInt/pointer on pthread
    {$endif CPUX64}
  {$endif OSPOSIX}
  {$define CPUINTELARM}
{$else}
  {$undef HASAESNI}         // AES-NI is an Intel-specific feature
  {$ifdef CPUARM3264}
    {$define CPUINTELARM}
  {$endif CPUARM3264}
  {$ifdef CPU32}
    {$define THREADID32}    // TThreadID = PtrUInt/pointer on pthread
  {$endif CPU32}
{$endif CPUINTEL}

{$ifdef CPU32}
  {$define NOPOINTEROFFSET} // 32-bit CPU will always store pointers
{$endif CPU32}


  // -----------
  // -- Libraries linking

  // some static linked files are to be downloaded from
  // https://github.com/synopse/mORMot2/releases

{$ifdef FPC}
  // Delphi doesn't accept GCC object files and libdeflate requires GCC
  {$if defined(OSOPENBSD) and defined(FPC_CROSSCOMPILING)}
    {$define NOSQLITE3STATIC} // OpenBSD problems with fpcupdeluxe libgcc.a
  {$ifend}
  {$define LIZARD_EXTERNALONLY}     // Lizard is disabled but on some targets
  {$ifdef OSLINUX}
    {$ifdef CPUINTEL}
      {$define LIBDEFLATESTATIC}    // libdeflate static binding
      {$define LIBQUICKJSSTATIC}    // quickjs static binding
      {$undef LIZARD_EXTERNALONLY}  // static liblizard.a
    {$endif CPUINTEL}
    {$ifdef CPUARM}
      {.$undef LIZARD_EXTERNALONLY} // static liblizard.a is not tested
      {.$define LIBDEFLATESTATIC}   // compiles, but untested
      {.$define LIBQUICKJSSTATIC}   // compiles, but untested
    {$endif CPUARM}
    {$ifdef CPUAARCH64}
      {$define LIBDEFLATESTATIC}
      {$undef LIZARD_EXTERNALONLY}  // static liblizard.a seems OK
      {.$define LIBQUICKJSSTATIC}   // compiles, but access violations
    {$endif CPUAARCH64}
  {$endif OSLINUX}
  {$ifdef OSWINDOWS}
    {$undef LIZARD_EXTERNALONLY}    // static liblizard.a
    {$ifdef CPUX86}
      {$define LIBDEFLATESTATIC}
      {$define LIBQUICKJSSTATIC}
    {$endif CPUX86}
    {$ifdef CPUX64}
      {.$define LIBDEFLATESTATIC}   // Win64 + FPC 3.2 = internal error 200603061
      {$define LIBQUICKJSSTATIC}
    {$endif CPUX64}
  {$endif OSWINDOWS}
  {$ifdef CPUARM3264}
    {$ifdef OSDARWIN}               // unsupported arch (e.g. Aarch64-Darwin)
      {$define OSDARWINARM}
      {$define LIZARD_EXTERNALONLY}
      {$define NOLIBCSTATIC}
    {$endif OSDWARWIN}
  {$endif CPUARM3264}
{$else}
  {$define LIZARD_EXTERNALONLY}     // no static .obj for Delphi Win32/Win64 yet
  {$ifdef CPUX86}
    {$define LIBQUICKJSSTATIC}      // our quickjs.obj seems fine on Win32 :)
  {$endif CPUX86}
  // there is a linking bug with Delphi XE4 on Win64
  {$ifdef CPUX64}
    {$if (CompilerVersion = 25.0) or
         (CompilerVersion = 28.0) or 
         (CompilerVersion = 29.0)}  // exactly XE4, XE7 or XE8 are known to GPF
      // other Win32/Win64 Delphi platforms "should work" (tm) as expected
      {$define NOSQLITE3STATIC}
    {$ifend}
    {$define LIBQUICKJSSTATIC}      // seems fine BUT on Delphi 10.4+ Win64
    {$if CompilerVersion >= 34.0}    // = Delphi 10.4 and later
      {$undef LIBQUICKJSSTATIC}
    {$ifend}
  {$endif}
{$endif FPC}

{$ifdef OSWINDOWS}
  // on Windows: enable Microsoft AES Cryptographic Provider (XP SP3 and up)
  // - even if those AES engines are slower and closed source (so should better
  // be avoided), we use it for TAesPrng.GetEntropy, as it can't hurt
  {$define USE_PROV_RSA_AES}
  // define at your own risk, if you have the good libraries ;)
  {.$define USE_OPENSSL}
{$else}
  {$ifndef OSANDROID}
    // try OpenSSL on POSIX systems where likely to be unique and maintained
    {$define USE_OPENSSL}
  {$endif OSANDROID}
{$endif OSWINDOWS}
{$ifdef FORCE_OPENSSL}
  {$define USE_OPENSSL} // if you think you are a lucky enough guy
{$endif FORCE_OPENSSL}


// -----------
// -- Per-platform Client-Server abilities

{$ifdef OSWINDOWS}

  {$ifndef ONLYUSEHTTPSOCKET}
    {$define USEWININET}      // publish TWinINet/TWinHttp/TWinHttpAPI classes
    {$define USEHTTPSYS}      // enable http.sys kernel-mode Web server
  {$endif ONLYUSEHTTPSOCKET}

  {$define USE_WINIOCP}     // I/O completion ports API is fine under Windows
  // (as used by mormot.core.threads and mormot.net.async)
  // (under Linux/POSIX, we fallback to a classical event-driven pool)

  {$ifndef NOSSPIAUTH}        // from mormot.lib.sspi
    {$define DOMAINRESTAUTH}  // enable SSPI in mormot.rest.client/server
  {$endif NOSSPIAUTH}

{$endif OSWINDOWS}

{$ifdef OSPOSIX}

  {$define ONLYUSEHTTPSOCKET} // efficient cross-platform Socket + OpenSSL API
  {$undef USE_WINIOCP}        // disable any Windows-specific code

  {$ifdef OSANDROID}

    // for Android, consider using https://github.com/gcesarmza/curl-android-ios
    // static libraries and force USELIBCURL in the project conditionals
    {$define LIBCURLSTATIC}

  {$else}

    {$ifndef USE_OPENSSL}       // if OpenSSL is not available on this platform
      {$define USELIBCURL}      // try cross-platform libcurl for https
    {$endif USE_OPENSSL}

    {$ifndef NOGSSAPIAUTH}      // mormot.lib.gssapi is not Android compatible
      {$define DOMAINRESTAUTH}  // enable libgss in mormot.rest.client/server
    {$endif NOGSSAPIAUTH}

  {$endif OSANDROID}

{$endif OSPOSIX}


