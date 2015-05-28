// General

// Make this $DEFINE to use the 16 color icons required by Borland
// or DEFINE to use the 256 color Indy versions
{.$DEFINE Borland}

// S.G. 4/9/2002: IPv4/IPv6 general switch (for defaults only)
{$DEFINE IdIPv4}

{$DEFINE INDY100}
{$DEFINE 10_5_8}  //so developers can IFDEF for this specific version

{$IFDEF BCB}
  {$DEFINE CBUILDER}
{$ELSE}
  {$DEFINE DELPHI}
{$ENDIF}

{$UNDEF USE_OPENSSL}
{$UNDEF USE_ZLIB_UNIT}
{$UNDEF USE_SSPI}

// $DEFINE the following if the global objects in the IdStack and IdThread
// units should be freed on finalization
{.$DEFINE FREE_ON_FINAL}
{$UNDEF FREE_ON_FINAL}

// Make sure the following is $DEFINE'd only for suitable environments
// as specified further below.  This works in conjunction with the
// FREE_ON_FINAL define above.
{$UNDEF REGISTER_EXPECTED_MEMORY_LEAK}

// FastMM is natively available in BDS 2006 and higher.  $DEFINE the
// following if FastMM has been installed manually
{.$DEFINE USE_FASTMM4}
{$UNDEF USE_FASTMM4}

// Make sure the following is $DEFINE'd only for Delphi/C++Builder 2009 onwards
// as specified further below.  The VCL is fully Unicode, where the 'String'
// type maps to System.UnicodeString, not System.AnsiString anymore
{$UNDEF STRING_IS_UNICODE}
{$UNDEF STRING_IS_ANSI}

// Make sure the following is $DEFINE'd only for suitable environments
// as specified further below.  This works in conjunction with the
// STRING_IS_ANSI and STRING_IS_UNICODE defines above.
{$UNDEF UNICODE_BUT_STRING_IS_ANSI}

// Make sure the following are $DEFINE'd only for suitable environments
// as specified further below.
{$UNDEF HAS_TEncoding}
{$UNDEF HAS_TCharacter}
{$UNDEF HAS_TInterlocked}
{$UNDEF TIdTextEncoding_IS_NATIVE}

// Make sure that this is defined only for environments where we are using
// the iconv library to charactor conversions.
{.$UNDEF USE_ICONV}

//Define for Delphi cross-compiler targetting Posix
{$UNDEF USE_VCL_POSIX}

// detect compiler versions

// Delphi 4
{$IFDEF VER120}
  {$DEFINE DCC}
  {$DEFINE VCL_40}
  {$DEFINE DELPHI_4}
{$ENDIF}

// C++Builder 4
{$IFDEF VER125}
  {$DEFINE DCC}
  {$DEFINE VCL_40}
  {$DEFINE CBUILDER_4}
{$ENDIF}

// Delphi & C++Builder 5
{$IFDEF VER130}
  {$DEFINE DCC}
  {$DEFINE VCL_50}
  {$IFDEF CBUILDER}
    {$DEFINE CBUILDER_5}
  {$ELSE}
    {$DEFINE DELPHI_5}
  {$ENDIF}
{$ENDIF}

//Delphi & C++Builder 6
{$IFDEF VER140}
  {$DEFINE DCC}
  {$DEFINE VCL_60}
  {$IFDEF CBUILDER}
    {$DEFINE CBUILDER_6}
  {$ELSE}
    {$DEFINE DELPHI_6}
  {$ENDIF}
{$ENDIF}

//Delphi 7
{$IFDEF VER150}
  {$DEFINE DCC}
  {$DEFINE VCL_70}
  {$DEFINE DELPHI_7} // there was no C++ Builder 7
{$ENDIF}

//Delphi 8
{$IFDEF VER160}
  {$DEFINE DCC}
  {$DEFINE VCL_80}
  {$DEFINE DELPHI_8} // there was no C++ Builder 8
{$ENDIF}

//Delphi 2005
{$IFDEF VER170}
  {$DEFINE DCC}
  {$DEFINE VCL_2005}
  {$DEFINE DELPHI_2005} // there was no C++Builder 2005
{$ENDIF}

// NOTE: CodeGear decided to make Highlander be a non-breaking release
// (no interface changes, thus fully backwards compatible without any
// end user code changes), so VER180 applies to both BDS 2006 and
// Highlander prior to the release of RAD Studio 2007.  Use VER185 to
// identify Highlanger specifically.

//Delphi & C++Builder 2006
//Delphi & C++Builder 2007 (Highlander)
{$IFDEF VER180}
  {$DEFINE DCC}
  {$DEFINE VCL_2006}
  {$IFDEF CBUILDER}
    {$DEFINE CBUILDER_2006}
  {$ELSE}
    {$DEFINE DELPHI_2006}
  {$ENDIF}
{$ENDIF}

//Delphi & C++Builder 2007 (Highlander)
{$IFDEF VER185}
  {$DEFINE DCC}
  {$UNDEF VCL_2006}
  {$DEFINE VCL_2007}
  {$IFDEF CBUILDER}
    {$UNDEF CBUILDER_2006}
    {$DEFINE CBUILDER_2007}
  {$ELSE}
    {$UNDEF DELPHI_2006}
    {$DEFINE DELPHI_2007}
  {$ENDIF}
{$ENDIF}

// BDS 2007 NET personality uses VER190 instead of 185.
//Delphi .NET 2007
{$IFDEF VER190}
  {$DEFINE DCC}
  {$IFDEF CIL}
    //Delphi 2007
    {$DEFINE VCL_2007}
    {$DEFINE DELPHI_2007}
  {$ENDIF}
{$ENDIF}

//Delphi & C++Builder 2009 (Tiburon)
{$IFDEF VER200}
  {$DEFINE DCC}
  {$DEFINE VCL_2009}
  {$IFDEF CBUILDER}
    {$DEFINE CBUILDER_2009}
  {$ELSE}
    {$DEFINE DELPHI_2009}
  {$ENDIF}
{$ENDIF}

//Delphi & C++Builder 2010 (Weaver)
{$IFDEF VER210}
  {$DEFINE DCC}
  {$DEFINE VCL_2010}
  {$IFDEF CBUILDER}
    {$DEFINE CBUILDER_2010}
  {$ELSE}
    {$DEFINE DELPHI_2010}
  {$ENDIF}
{$ENDIF}

//Delphi & C++Builder XE (Fulcrum)
{$IFDEF VER220}
//REMOVE DCC DEFINE after the next Fulcrum beta.
//It will be defined there.
  {$DEFINE DCC}
  {$DEFINE VCL_XE}
  {$IFDEF CBUILDER}
    {$DEFINE CBUILDER_XE}
  {$ELSE}
    {$DEFINE DELPHI_XE}
  {$ENDIF}
{$ENDIF}

//Delphi & CBuilder XE2 (Pulsar)
{$IFDEF VER230}
  // DCC is now defined by the compiler
  {$DEFINE VCL_XE2}
  {$IFDEF CBUILDER}
    {$DEFINE CBUILDER_XE2}
  {$ELSE}
    {$DEFINE DELPHI_XE2}
  {$ENDIF}
{$ENDIF}

//Delphi & CBuilder XE3 & Above
{$if COMPILERVERSION >= 24}
  // DCC is now defined by the compiler
  {$DEFINE VCL_XE2}
  {$IFDEF CBUILDER}
    {$DEFINE CBUILDER_XE2}
  {$ELSE}
    {$DEFINE DELPHI_XE2}
  {$ENDIF}
{$ifend}

// Delphi.NET
// Covers D8+
{$IFDEF CIL}
  // Platform specific conditional. Used for platform specific code.
  {$DEFINE DOTNET}
  {$DEFINE STRING_IS_UNICODE}
{$ENDIF}

// Kylix
//
//Important:  Don't use CompilerVersion here as IF's are evaluated before
//IFDEF's and Kylix 1 does not have CompilerVersion defined at all.
{$IFNDEF FPC}
  {$IFDEF LINUX}
    {$DEFINE UNIX}
    {$IFDEF CONDITIONALEXPRESSIONS}
      {$IF (RTLVersion >= 14.0) and (RTLVersion <= 14.5) }
        {$DEFINE KYLIX}
        {$IF RTLVersion = 14.5}
          {$DEFINE KYLIX_3}
        {$ELSEIF RTLVersion >= 14.2}
          {$DEFINE KYLIX_2}
        {$ELSE}
          {$DEFINE KYLIX_1}
        {$IFEND}
      {$IFEND}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF KYLIX}
  {$DEFINE VCL_60}
  {$DEFINE INT_THREAD_PRIORITY}
  {$DEFINE CPUI386}
  {$UNDEF USE_BASEUNIX}

  {$IFDEF KYLIX_3}
    {$DEFINE KYLIX_3_OR_ABOVE}
  {$ENDIF}

  {$IFDEF KYLIX_3_OR_ABOVE}
    {$DEFINE KYLIX_2_OR_ABOVE}
  {$ELSE}
    {$IFDEF KYLIX_2}
      {$DEFINE KYLIX_2_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF KYLIX_2_OR_ABOVE}
    {$DEFINE KYLIX_1_OR_ABOVE}
  {$ELSE}
    {$IFDEF KYLIX_1}
      {$DEFINE KYLIX_1_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}

  {$IFNDEF KYLIX_3_OR_ABOVE}
    {$DEFINE KYLIXCOMPAT}
  {$ENDIF}

  {$IFDEF KYLIX_2_OR_ABOVE}
    {$DEFINE USE_ZLIB_UNIT}
  {$ENDIF}
{$ENDIF}

// FPC (2+)

{$IFDEF FPC}
  {$MODE Delphi}
  //note that we may need further defines for widget types depending on
  //what we do and what platforms we support in FPC.
  //I'll let Marco think about that one.
  {$IFDEF UNIX}
    {$DEFINE USE_BASEUNIX}
    {$IFDEF LINUX}
      //In Linux for I386, you can choose between a Kylix-libc API or
      //the standard RTL Unix API.  Just pass -dKYLIXCOMPAT to the FPC compiler.
      //I will see what I can do about the Makefile.
      {$IFDEF KYLIXCOMPAT}
        {$IFDEF CPUI386}
          {$UNDEF USE_BASEUNIX}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
    {$IFDEF USE_BASEUNIX}
      {$UNDEF KYLIXCOMPAT}
    {$ENDIF}
  {$ENDIF}
  {$DEFINE VCL_70}
  {$DEFINE DELPHI_7}

  // FPC_FULLVERSION was added in FPC 2.2.4
  // Have to use Declared() or else Delphi compiler chokes, since it
  // evaluates $IF statements before $IFDEF statements...

  {$MACRO ON} // must be on in order to use versioning macros
  {$IF DECLARED(FPC_FULLVERSION) and (FPC_FULLVERSION >= 20402)}
    {$DEFINE FPC_2_4_2_OR_ABOVE}
  {$IFEND}

  {$IFDEF FPC_2_4_2_OR_ABOVE}
    {$DEFINE FPC_2_2_0_OR_ABOVE}
  {$ELSE}
    {$IFDEF VER2_2}
      {$DEFINE FPC_2_2_0_OR_ABOVE}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

// end FPC

{$IFDEF VCL_XE2}
  {$DEFINE VCL_XE2_OR_ABOVE}
{$ENDIF}

{$IFDEF VCL_XE2_OR_ABOVE}
  {$DEFINE VCL_XE_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_XE}
    {$DEFINE VCL_XE_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_XE_OR_ABOVE}
  {$DEFINE VCL_2010_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_2010}
    {$DEFINE VCL_2010_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2010_OR_ABOVE}
  {$DEFINE VCL_2009_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_2009}
    {$DEFINE VCL_2009_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2009_OR_ABOVE}
  {$DEFINE VCL_2007_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_2007}
    {$DEFINE VCL_2007_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2007_OR_ABOVE}
  {$DEFINE VCL_2006_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_2006}
    {$DEFINE VCL_2006_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2006_OR_ABOVE}
  {$DEFINE VCL_2005_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_2005}
    {$DEFINE VCL_2005_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2005_OR_ABOVE}
  {$DEFINE VCL_8_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_80}
    {$DEFINE VCL_8_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_8_OR_ABOVE}
  {$DEFINE VCL_7_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_70}
    {$DEFINE VCL_7_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_7_OR_ABOVE}
  {$DEFINE VCL_6_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_60}
    {$DEFINE VCL_6_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_6_OR_ABOVE}
  {$DEFINE VCL_5_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_50}
    {$DEFINE VCL_5_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_5_OR_ABOVE}
  {$DEFINE VCL_4_OR_ABOVE}
{$ELSE}
  {$IFDEF VCL_40}
    {$DEFINE VCL_4_OR_ABOVE}
  {$ENDIF}
{$ENDIF}

// Normalize Delphi compiler defines to match FPC for consistency:
//
// CPU32 - any 32-bit CPU
// CPU64 - any 64-bit CPU
// WINDOWS - any Windows platform (32-bit, 64-bit, CE)
// WIN32 - Windows 32-bit
// WIN64 - Windows 64-bit
// WINCE - Windows CE
//
// Consult the "Free Pascal Programmer's Guide", Appendix G for the complete
// list of defines that are used. Do not work on this unless you understand
// what the FreePascal developers are doing.  Not only do you have to
// descriminate with operating systems, but also with chip architectures
// are well.
//
// DCC Pulsar+ define the following values:
// ASSEMBLER
// DCC
// CONDITIONALEXPRESSIONS
// NATIVECODE
// UNICODE
// MACOS
// MACOS32
// MACOS64
// MSWINDOWS
// WIN32
// WIN64
// LINUX
// POSIX
// POSIX32
// CPU386
// CPUX86
// CPUX64
//
// Kylix defines the following values:
// LINUX
// (others??)
//

{$IFNDEF FPC}
  // TODO: We need to use ENDIAN_BIG for big endian chip architectures,
  // such as 680x0, PowerPC, Sparc, and MIPS, once DCC supports them,
  // provided it does not already define its own ENDIAN values by then...
  {$DEFINE ENDIAN_LITTLE}
  {$IFNDEF VCL_6_OR_ABOVE}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    {$DEFINE WINDOWS}
  {$ENDIF}
  // TODO: map Pulsar's non-Windows platform defines...
  {$IFDEF VCL_XE2_OR_ABOVE}
    {$IFDEF CPU386}
      //any 32-bit CPU
      {$DEFINE CPU32}
      //Intel 386 compatible chip architecture
      {$DEFINE CPUI386}
    {$ENDIF}
    {$IFDEF CPUX86}
      {$DEFINE CPU32}
    {$ENDIF}
    {$IFDEF CPUX64}
      //any 64-bit CPU
      {$DEFINE CPU64}
      //AMD64 compatible chip architecture 
      {$DEFINE CPUX86_64}  //historical name for AMD64
      {$DEFINE CPUAMD64}
    {$ENDIF}
  {$ELSE}
    {$IFNDEF DOTNET}
      {$IFNDEF KYLIX}
        {$DEFINE I386}
      {$ENDIF}
    {$ENDIF}
    {$DEFINE CPU32}
  {$ENDIF}
{$ENDIF}

{$IFDEF DOTNET}
  //differences in DotNET Framework versions.
  {$IFDEF VCL_2007_OR_ABOVE}
    {$DEFINE DOTNET_2}
    {$DEFINE DOTNET_2_OR_ABOVE}
  {$ELSE}
    {$DEFINE DOTNET_1_1}
  {$ENDIF}
  {$DEFINE DOTNET_1_1_OR_ABOVE}
  // Extra include used in D7 for testing. Remove later when all comps are
  // ported. Used to selectively exclude non ported parts. Allowed in places
  // IFDEFs are otherwise not permitted.
  {$DEFINE DOTNET_EXCLUDE}
{$ENDIF}

// Check for available features

{$IFDEF VCL_5_OR_ABOVE}
  {$IFNDEF FPC}
    {$IFNDEF KYLIX}
      {$DEFINE HAS_REMOVEFREENOTIFICATION}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_6_OR_ABOVE}
  {$DEFINE HAS_TSelectionEditor}
  {$DEFINE HAS_PCardinal}
  {$DEFINE HAS_PByte}
  {$DEFINE HAS_PWord}
  {$DEFINE HAS_PPointer}
  {$DEFINE HAS_TList_Assign}
  {$DEFINE HAS_sLineBreak}
  {$DEFINE HAS_RaiseLastOSError}
  {$DEFINE HAS_SysUtils_IncludeExcludeTrailingPathDelimiter}
  {$DEFINE HAS_SysUtils_DirectoryExists}
  {$DEFINE HAS_UNIT_DateUtils}
  {$DEFINE HAS_UNIT_Types}
  {$DEFINE HAS_TryStrToInt}
  {$DEFINE HAS_TryStrToInt64}
  {$DEFINE HAS_ENUM_ELEMENT_VALUES}
  {$IFNDEF FPC}
    {$DEFINE HAS_TStringList_CaseSensitive}
    {$IFNDEF KYLIX}
      {$DEFINE HAS_DEPRECATED}
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF DOTNET}
    //Widget defines are omitted in .NET
    {$DEFINE VCL_60_PLUS}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_7_OR_ABOVE}
  {$IFNDEF FPC}
    {$DEFINE HAS_UInt64}
    {$DEFINE HAS_NAMED_THREADS}
  {$ENDIF}
  {$DEFINE HAS_TFormatSettings}
  {$IFNDEF VCL_70}
    // not implemented in D7
    {$DEFINE HAS_STATIC_TThread_Queue}
  {$ENDIF}
  {$IFNDEF VCL_80}
    // not implemented in D8
    {$DEFINE HAS_STATIC_TThread_Synchronize}
  {$ENDIF}
{$ELSE}
  {$IFDEF CBUILDER_6}
    {$DEFINE HAS_NAMED_THREADS}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2006_OR_ABOVE}
  {$DEFINE USE_INLINE}
  {$DEFINE HAS_2PARAM_FileAge}
  {$DEFINE HAS_System_RegisterExpectedMemoryLeak}
  {$IFNDEF FREE_ON_FINAL}
    {$IFNDEF DOTNET}
      {$DEFINE REGISTER_EXPECTED_MEMORY_LEAK}
    {$ENDIF}
  {$ENDIF}
{$ELSE}
  {$DEFINE HAS_InterlockedCompareExchange_Pointers}
{$ENDIF}

{$IFDEF VCL_2007_OR_ABOVE}
  {$IFNDEF CBUILDER_2007}
    // class properties are broken in C++Builder 2007, causing AVs at compile-time
    {$DEFINE HAS_CLASSPROPERTIES}
  {$ENDIF}
  // Native(U)Int existed but were buggy, so do not use them yet
  {.$DEFINE HAS_NativeInt}
  {.$DEFINE HAS_NativeUInt}
  {$DEFINE HAS_StrToInt64Def}
  {$DEFINE HAS_ULONG_PTR}
  {$DEFINE HAS_PGUID}
  {$DEFINE HAS_PPAnsiChar}
  {$DEFINE HAS_CurrentYear}
  {$IFNDEF DOTNET}
    {$DEFINE HAS_TIMEUNITS}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_2009_OR_ABOVE}
  {$IFNDEF DOTNET}
    {$DEFINE STRING_IS_UNICODE}
    {$DEFINE HAS_TEncoding}
    {$DEFINE HAS_TCharacter}
    {$DEFINE HAS_InterlockedCompareExchangePointer}
    {$DEFINE HAS_WIDE_TCharArray}
    {$IFDEF VCL_2009}
      // TODO: need to differentiate between RTM and Update 1
      // FmtStr() is broken in RTM but was fixed in Update 1
      {$DEFINE BROKEN_FmtStr}
    {$ENDIF}
    {$IFNDEF VCL_XE_OR_ABOVE}
      // TEncoding.GetEncoding() and TMBCSEncoding support for codepage 20127
      // have some bugs in 2009 and 2010 that were finally fixed in XE
      {$DEFINE BROKEN_TEncoding_GetEncoding}
      {$DEFINE BROKEN_TMBCSEncoding_CP20127}
    {$ENDIF}
  {$ENDIF}
  {$DEFINE HAS_CLASSVARS}
  {$DEFINE HAS_DEPRECATED_MSG}
  {$DEFINE HAS_TBytes}
  {$DEFINE HAS_NativeInt}
  {$DEFINE HAS_NativeUInt}
{$ENDIF}

{$IFDEF VCL_2010_OR_ABOVE}
  {$DEFINE HAS_CLASSCONSTRUCTOR}
  {$DEFINE HAS_CLASSDESTRUCTOR}
  {$DEFINE HAS_DELAYLOAD}
  {$DEFINE HAS_TStrings_ValueFromIndex}
  {$DEFINE HAS_TThread_NameThreadForDebugging}
  {$DEFINE DEPRECATED_TThread_SuspendResume}
{$ENDIF}

{$IFDEF VCL_XE_OR_ABOVE}
  {$DEFINE HAS_TFormatSettings_Object}
  {$IFNDEF DOTNET}
    {$DEFINE HAS_TInterlocked}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_XE2_OR_ABOVE}
  {$DEFINE HAS_SIZE_T}
{$ENDIF}

// Delphi XE+ cross-compiling
{$IFNDEF FPC}
  {$IFDEF POSIX}
    {$IF RTLVersion >= 22.0}
      {$DEFINE UNIX}
      {$UNDEF USE_BASEUNIX}
      {$DEFINE VCL_CROSS_COMPILE}
      {$DEFINE USE_VCL_POSIX}
    {$IFEND}
  {$ENDIF}
  {$IFDEF LINUX}
    {$IFDEF CONDITIONALEXPRESSIONS}
      {$IF RTLVersion >= 22.0}
        {$DEFINE VCL_CROSS_COMPILE}
        {$DEFINE USE_VCL_POSIX}
      {$IFEND}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL_CROSS_COMPILE}
  {$UNDEF KYLIXCOMPAT}
{$ELSE}
  {$IFDEF KYLIXCOMPAT}
    {$linklib c}
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE USE_INLINE}
  {$DEFINE USE_CLASSINLINE}
  {$DEFINE USE_TBitBtn} //use Bit Buttons instead of Buttons
  {$DEFINE FPC_REINTRODUCE_BUG}
  {$DEFINE FPC_CIRCULAR_BUG}
  {$DEFINE NO_REDECLARE}
  {$DEFINE BYTE_COMPARE_SETS}
  // FreePascal 2.2.0 has an overload for InterlockedCompareExchange that takes pointers.
  {$IFDEF FPC_2_2_0_OR_ABOVE}
    {$DEFINE HAS_InterlockedCompareExchange_Pointers}
  {$ENDIF}
  {$DEFINE HAS_PtrInt}
  {$DEFINE HAS_PtrUInt}
  {$DEFINE HAS_PGUID}
  {$DEFINE HAS_LPGUID}
  {$DEFINE HAS_PPAnsiChar}
  {$DEFINE HAS_ENUM_ELEMENT_VALUES}
  {$IFDEF WINDOWS}
    {$DEFINE HAS_ULONG_PTR}
  {$ENDIF}
  {$DEFINE HAS_UNIT_ctypes}
  // FreePascal 2.4.2 has size_t
  {$IFDEF FPC_2_4_2_OR_ABOVE}
    {$DEFINE HAS_SIZE_T}
  {$ENDIF}
{$ENDIF}

{$IFDEF DOTNET}
  {$DEFINE WIDGET_WINFORMS}
{$ELSE}
  {$DEFINE WIDGET_VCL_LIKE} // LCL included.
  {$DEFINE WIDGET_VCL_LIKE_OR_KYLIX}
  {$IFDEF FPC}
    {$DEFINE WIDGET_LCL}
  {$ELSE}
    {$IFDEF KYLIX}
      {$DEFINE WIDGET_KYLIX}
    {$ELSE}
      {$DEFINE WIDGET_VCL}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

// .NET and Delphi 2009+ support UNICODE strings natively!
// NOTE: Do not define UNICODE here.  The compiler defines
// the symbol automatically.
{$IFNDEF STRING_IS_UNICODE}
  {$DEFINE STRING_IS_ANSI}
  {$IFDEF UNICODE}
    {$DEFINE UNICODE_BUT_STRING_IS_ANSI}
  {$ENDIF}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE WIN32_OR_WIN64}
{$ENDIF}
{$IFDEF WIN64}
  {$DEFINE WIN32_OR_WIN64}
{$ENDIF}

{$IFDEF WIN32_OR_WIN64}
  {$DEFINE USE_OPENSSL}
  {$DEFINE USE_ZLIB_UNIT}
  {$DEFINE USE_SSPI}
  {$IFDEF STRING_IS_UNICODE}
    {$DEFINE SSPI_UNICODE}
  {$ENDIF}
{$ENDIF}

// High-performance counters are not reliable on multi-core systems, and have
// been known to cause problems with TIdIOHandler.ReadLn() timeouts in Windows
// XP SP3, both 32-bit and 64-bit.  Refer to these discussions for more info:
//
// http://www.virtualdub.org/blog/pivot/entry.php?id=106
// http://blogs.msdn.com/oldnewthing/archive/2008/09/08/8931563.aspx
//
// Do not enable thus unless you know it will work correctly on your systems!
{$IFDEF WINDOWS}
  {.$DEFINE USE_HI_PERF_COUNTER_FOR_TICKS}
{$ENDIF}

{$IFDEF UNIX}
  {$DEFINE USE_OPENSSL}
  {$DEFINE USE_ZLIB_UNIT}
{$ENDIF}

{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  {$DEFINE REQUIRES_PROPER_ALIGNMENT}
{$ENDIF}

//
//iconv defines section.
{$DEFINE USE_ICONV_UNIT}
{$DEFINE USE_ICONV_ENC}
{$IFDEF UNIX}
  {$DEFINE USE_ICONV}
  {$IFDEF USE_BASEUNIX}
    {$IFDEF FPC}
      {$UNDEF USE_ICONV_UNIT}
    {$ELSE}
      {$UNDEF USE_ICONV_ENC}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF KYLIXCOMPAT}
    //important!!  Iconv functions are defined in the libc.pas Kylix compatible unit.
    {$UNDEF USE_ICONV_ENC}
    {$UNDEF USE_ICONV_UNIT}
  {$ENDIF}
{$ENDIF}
{$IFDEF NETWARELIBC}
  {$DEFINE USE_ICONV}
  //important!!! iconv functions are defined in the libc.pas Novell Netware header.
  //Do not define USE_ICONVUNIT
  {$UNDEF USE_ICONV_UNIT}
  {$UNDEF USE_ICONV_ENC}
{$ENDIF}
{$IFNDEF USE_ICONV}
  {$UNDEF USE_ICONV_UNIT}
  {$UNDEF USE_ICONV_ENC}
{$ENDIF}
{$UNDEF USE_SAFELOADLIBRARY}
{$IFDEF WINDOWS}
  {$UNDEF USE_ICONV_ENC}
  {$DEFINE USE_SAFELOADLIBRARY}
{$ENDIF}

{$UNDEF USE_INVALIDATE_MOD_CACHE}
{$UNDEF USE_SAFELOADLIBRARY}
//This must come after the iconv defines because this compiler targets a Unix-like
//operating system.  One key difference is that it does have a TEncoding class.
//If this comes before the ICONV defines, it creates problems.
//This also must go before the THandle size calculations.
{$IFDEF VCL_CROSS_COMPILE}
  {$IFDEF MACOS}
    {$DEFINE BSD}
    {$DEFINE DARWIN}
    {$DEFINE USE_SAFELOADLIBRARY}
    {$DEFINE USE_INVALIDATE_MOD_CACHE}
  {$ENDIF}
  //important!!! iconv functions are defined in the libc.pas Novell Netware header.
  //Do not define USE_ICONVUNIT
  {$UNDEF USE_ICONV}
  {$UNDEF USE_ICONV_UNIT}
  {$UNDEF USE_ICONV_ENC}
  {$DEFINE INT_THREAD_PRIORITY}
{$ENDIF}

//IMPORTANT!!!!
//
//Do not remove this!!!  This is to work around a conflict.  In DCC, MACOS
//will mean OS X.  In FreePascal, the DEFINE MACOS means MacIntosh System OS Classic.
{$IFDEF DCC}
  {$IFDEF MACOS}
    {$DEFINE DARWIN}
  {$ENDIF}
{$ENDIF}
{$IFDEF FPC}
  {$IFDEF MACOS}
    {$DEFINE MACOS_CLASSIC}
  {$ENDIF}
{$ENDIF}

{
BSD 4.4 introduced a minor API change.  sa_family was changed from a 16bit
word to an 8 bit byte and an 8 bit byte field named sa_len was added.
}
//Place this only after DARWIN has been defined for Delphi MACOS
{$IFDEF FREEBSD}
  {$DEFINE SOCK_HAS_SINLEN}
{$ENDIF}
{$IFDEF DARWIN}
  {$DEFINE SOCK_HAS_SINLEN}
{$ENDIF}
{$IFDEF HAIKU}
  {$DEFINE SOCK_HAS_SINLEN}
{$ENDIF}
{$IFDEF MORPHOS}
  {$DEFINE SOCK_HAS_SINLEN}
{$ENDIF}

// Do NOT remove these IFDEF's.  They are here because InterlockedExchange
// only handles 32bit values.  Some Operating Systems may have 64bit  
// THandles.  This is not always tied to the platform architecture.

{$IFDEF AMIGA}
  {$DEFINE THANDLE_CPUBITS}
{$ENDIF}
{$IFDEF ATARI}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF BEOS}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF BSD}
  //I think BSD might handle FreeBSD, NetBSD, OpenBSD, and Darwin
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF EMBEDDED}
  {$DEFINE THANDLE_CPUBITS}
{$ENDIF}
{$IFDEF EMX}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF GBA}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF GO32}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF HAIKU}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF LINUX}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF MACOS_CLASSIC}
  {$DEFINE THANDLE_CPUBITS}
{$ENDIF}
{$IFDEF MORPHOS}
  {$DEFINE THANDLE_CPUBITS}
{$ENDIF}
{$IFDEF NATIVENT} //Native NT for kernel level drivers
  {$DEFINE THANDLE_CPUBITS}
{$ENDIF}
{$IFDEF NDS}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF NETWARE}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF NETWARELIBC}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF OS2}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF PALMOS}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF SOLARIS}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF SYMBIAN}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF WII}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF WATCOM}
  {$DEFINE THANDLE_32}
{$ENDIF}
{$IFDEF WINDOWS}
  {$DEFINE THANDLE_CPUBITS}
{$ENDIF}

// end platform specific stuff for THandle size

{$IFDEF THANDLE_CPUBITS}
  {$IFDEF CPU64}
    {$DEFINE THANDLE_64}
  {$ELSE}
    {$DEFINE THANDLE_32}
  {$ENDIF}
{$ENDIF}

{$IFDEF DOTNET}
  {$DEFINE DOTNET_OR_ICONV}
  {$DEFINE TIdTextEncoding_IS_NATIVE}
{$ELSE}
  {$IFDEF HAS_TEncoding}
    {$IFNDEF USE_ICONV}
      {$DEFINE TIdTextEncoding_IS_NATIVE}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$IFDEF USE_ICONV}
  {$DEFINE DOTNET_OR_ICONV}
{$ENDIF}

{$UNDEF STREAM_SIZE_64}
{$IFDEF FPC}
  {$DEFINE STREAM_SIZE_64}
{$ELSE}
  {$IFDEF VCL_6_OR_ABOVE}
    {$DEFINE STREAM_SIZE_64}
  {$ENDIF}
{$ENDIF}

{$IFNDEF FREE_ON_FINAL}
  {$IFNDEF REGISTER_EXPECTED_MEMORY_LEAK}
    {$IFDEF USE_FASTMM4}
      {$DEFINE REGISTER_EXPECTED_MEMORY_LEAK}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF REGISTER_EXPECTED_MEMORY_LEAK}
  {$IFDEF DOTNET}
    {$UNDEF REGISTER_EXPECTED_MEMORY_LEAK}
  {$ENDIF}
  {$IFDEF VCL_CROSS_COMPILE}
    // RLebeau: should this be enabled for Windows, at least?
    {$UNDEF REGISTER_EXPECTED_MEMORY_LEAK}
  {$ENDIF}
{$ENDIF}

{
We must determine what the SocketType parameter is for the Socket function.
In DotNET, it's SocketType.  In Kylix and the libc.pas Kylix-compatibility
library, it's a __socket_type.  In BaseUnix, it's a C-type Integer.  In Windows,
it's a LongInt.

}
{$UNDEF SOCKETTYPE_IS_SOCKETTYPE}
{$UNDEF SOCKETTYPE_IS_CINT}
{$UNDEF SOCKETTYPE_IS___SOCKETTYPE}
{$UNDEF SOCKETTYPE_IS_LONGINT}
{$UNDEF SOCKETTYPE_IS_NUMERIC}
{$UNDEF SOCKET_LEN_IS_socklen_t}
{$IFDEF DOTNET}
  {$DEFINE SOCKETTYPE_IS_SOCKETTYPE}
{$ENDIF}
{$IFDEF USE_BASEUNIX}
  {$DEFINE SOCKETTYPE_IS_CINT}
  {$DEFINE SOCKETTYPE_IS_NUMERIC}
{$ENDIF}
{$IFDEF KYLIXCOMPAT}
  {$DEFINE SOCKETTYPE_IS___SOCKETTYPE}
{$ENDIF}
{$IFDEF USE_VCL_POSIX}
  {$DEFINE SOCKETTYPE_IS_NUMERIC}
  {$DEFINE SOCKETTYPE_IS_LONGINT}
  {$DEFINE SOCKET_LEN_IS_socklen_t}
{$ENDIF}
{$IFDEF WINDOWS}
  {$DEFINE SOCKETTYPE_IS_LONGINT}
  {$DEFINE SOCKETTYPE_IS_NUMERIC}
{$ENDIF}
{$IFDEF OS2}
  {$DEFINE SOCKETTYPE_IS_LONGINT}
  {$DEFINE SOCKETTYPE_IS_NUMERIC}
{$ENDIF}
{$IFDEF NETWARE}
  {$DEFINE SOCKETTYPE_IS_LONGINT}
  {$DEFINE SOCKETTYPE_IS_NUMERIC}
{$ENDIF}

{Take advantage of some TCP features specific to some stacks.
They work somewhat similarly but there's a key difference.
In Linux, TCP_CORK is turned on to send fixed packet sizes and
when turned-off (uncorked), any remaining data is sent. With 
TCP_NOPUSH, this might not happen and remaining data is only sent
before disconnect.}
{$UNDEF HAS_TCP_NOPUSH}
{$UNDEF HAS_TCP_CORK}
{$IFDEF BSD}
  {$DEFINE HAS_TCP_NOPUSH}
{$ENDIF}
{$IFDEF LINUX}
  {$DEFINE HAS_TCP_CORK}
{$ENDIF}
{$IFDEF SOLARIS}
  {$DEFINE HAS_TCP_CORK}
{$ENDIF}

{$IFDEF DEBUG}
  {$UNDEF USE_INLINE}
{$ENDIF}
