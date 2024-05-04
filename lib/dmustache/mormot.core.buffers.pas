/// Framework Core Low-Level Memory Buffer Process
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.buffers;

{
  *****************************************************************************

   Low-Level Memory Buffers Processing Functions shared by all framework units
   - Variable Length Integer Encoding / Decoding
   - TAlgoCompress Compression/Decompression Classes - with AlgoSynLZ AlgoRleLZ
   - TFastReader / TBufferWriter Binary Streams
   - Base64, Base64Uri, Base58 and Baudot Encoding / Decoding
   - URI-Encoded Text Buffer Process
   - Basic MIME Content Types Support
   - Text Memory Buffers and Files
   - TStreamRedirect and other Hash process
   - Markup (e.g. HTML or Emoji) process
   - RawByteString Buffers Aggregation via TRawByteStringGroup

  *****************************************************************************
}

interface

{$I mormot.defines.inc}

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.rtti;


{ ************ Variable Length Integer Encoding / Decoding }

/// convert a cardinal into a 32-bit variable-length integer buffer
function ToVarUInt32(Value: cardinal; Dest: PByte): PByte;

/// return the number of bytes necessary to store a 32-bit variable-length integer
// - i.e. the ToVarUInt32() buffer size
function ToVarUInt32Length(Value: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// return the number of bytes necessary to store some data with a its
// 32-bit variable-length integer length
function ToVarUInt32LengthWithData(Value: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an integer into a 32-bit variable-length integer buffer
// - store negative values as cardinal two-complement, i.e.
// 0=0,1=1,2=-1,3=2,4=-2...
function ToVarInt32(Value: PtrInt; Dest: PByte): PByte;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 32-bit variable-length integer buffer into a cardinal
// - fast inlined process for any number < 128
// - use overloaded FromVarUInt32() or FromVarUInt32Safe() with a SourceMax
// pointer to avoid any potential buffer overflow
// - use FromVarUInt32Big() is the content is likely to be >= 128
function FromVarUInt32(var Source: PByte): cardinal; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// safely convert a 32-bit variable-length integer buffer into a cardinal
// - slower but safer process checking out of boundaries memory access in Source
// - SourceMax is expected to be not nil, and to point to the first byte
// just after the Source memory buffer
// - returns nil on error, or point to next input data on successful decoding
function FromVarUInt32Safe(Source, SourceMax: PByte; out Value: cardinal): PByte;

/// convert a 32-bit variable-length integer buffer into a cardinal
// - will call FromVarUInt32() if SourceMax=nil, or FromVarUInt32Safe() if set
// - returns false on error, true if Value has been set properly
function FromVarUInt32(var Source: PByte; SourceMax: PByte;
  out Value: cardinal): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 32-bit variable-length integer buffer into a cardinal
// - this version could be called if number is likely to be > $7f, so if
// inlining the first byte won't make any benefit
function FromVarUInt32Big(var Source: PByte): cardinal;

/// convert a 32-bit variable-length integer buffer into a cardinal
// - used e.g. when inlining FromVarUInt32()
// - this version must be called if Source^ has already been checked to be > $7f
// ! result := Source^;
// ! inc(Source);
// ! if result>$7f then
// !   result := (result and $7F) or FromVarUInt32Up128(Source);
function FromVarUInt32Up128(var Source: PByte): cardinal;

/// convert a 32-bit variable-length integer buffer into a cardinal
// - this version must be called if Source^ has already been checked to be > $7f
function FromVarUInt32High(var Source: PByte): cardinal;

/// convert a 32-bit variable-length integer buffer into an integer
// - decode negative values from cardinal two-complement, i.e.
// 0=0,1=1,2=-1,3=2,4=-2...
function FromVarInt32(var Source: PByte): integer;

/// convert a UInt64 into a 64-bit variable-length integer buffer
function ToVarUInt64(Value: QWord; Dest: PByte): PByte;

/// convert a 64-bit variable-length integer buffer into a UInt64
function FromVarUInt64(var Source: PByte): QWord; overload;

/// safely convert a 64-bit variable-length integer buffer into a UInt64
// - slower but safer process checking out of boundaries memory access in Source
// - SourceMax is expected to be not nil, and to point to the first byte
// just after the Source memory buffer
// - returns nil on error, or point to next input data on successful decoding
function FromVarUInt64Safe(Source, SourceMax: PByte; out Value: QWord): PByte;

/// convert a 64-bit variable-length integer buffer into a UInt64
// - will call FromVarUInt64() if SourceMax=nil, or FromVarUInt64Safe() if set
// - returns false on error, true if Value has been set properly
function FromVarUInt64(var Source: PByte; SourceMax: PByte;
  out Value: Qword): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a Int64 into a 64-bit variable-length integer buffer
function ToVarInt64(Value: Int64; Dest: PByte): PByte;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 64-bit variable-length integer buffer into a Int64
function FromVarInt64(var Source: PByte): Int64;

/// convert a 64-bit variable-length integer buffer into a Int64
// - this version won't update the Source pointer
function FromVarInt64Value(Source: PByte): Int64;

/// jump a value in the 32-bit or 64-bit variable-length integer buffer
function GotoNextVarInt(Source: PByte): pointer;
 {$ifdef HASINLINE}inline;{$endif}

/// convert a RawUtf8 into an UTF-8 encoded variable-length buffer
function ToVarString(const Value: RawUtf8; Dest: PByte): PByte;

/// jump a value in variable-length text buffer
function GotoNextVarString(Source: PByte): pointer;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve a variable-length UTF-8 encoded text buffer in a newly allocation RawUtf8
function FromVarString(var Source: PByte): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// safe retrieve a variable-length UTF-8 encoded text buffer in a newly allocation RawUtf8
// - supplied SourceMax value will avoid any potential buffer overflow
function FromVarString(var Source: PByte; SourceMax: PByte): RawUtf8; overload;

/// retrieve a variable-length UTF-8 encoded text buffer in a newly allocation RawUtf8
procedure FromVarString(var Source: PByte; var Value: RawUtf8); overload;

/// retrieve a variable-length text buffer
// - this overloaded function will set the supplied code page to the AnsiString
procedure FromVarString(var Source: PByte; var Value: RawByteString;
  CodePage: integer); overload;

/// retrieve a variable-length text buffer
// - this overloaded function will set the supplied code page to the AnsiString
// and will also check for the SourceMax end of buffer
// - returns TRUE on success, or FALSE on any buffer overload detection
function FromVarString(var Source: PByte; SourceMax: PByte;
  var Value: RawByteString; CodePage: integer): boolean; overload;

/// retrieve a variable-length UTF-8 encoded text buffer in a temporary buffer
// - caller should call Value.Done after use of the Value.buf memory
// - this overloaded function would include a trailing #0, so Value.buf could
// be parsed as a valid PUtf8Char buffer (e.g. containing JSON)
procedure FromVarString(var Source: PByte; var Value: TSynTempBuffer); overload;

/// retrieve a variable-length UTF-8 encoded text buffer in a temporary buffer
// - caller should call Value.Done after use of the Value.buf memory
// - this overloaded function will also check for the SourceMax end of buffer,
// returning TRUE on success, or FALSE on any buffer overload detection
function FromVarString(var Source: PByte; SourceMax: PByte;
  var Value: TSynTempBuffer): boolean; overload;

type
  /// kind of result returned by FromVarBlob() function
  TValueResult = record
    /// start of data value
    Ptr: PAnsiChar;
    /// value length (in bytes)
    Len: PtrInt;
  end;

/// retrieve pointer and length to a variable-length text/blob buffer
function FromVarBlob(Data: PByte): TValueResult;
  {$ifdef HASINLINE}inline;{$endif}



{ ************ TAlgoCompress Compression/Decompression Classes }

type
  /// exception raised by TAlgoCompress classes
  EAlgoCompress = class(ESynException);

  /// define the implementation used by TAlgoCompress.Decompress()
  TAlgoCompressLoad = (
    aclNormal,
    aclSafeSlow,
    aclNoCrcFast);

  /// abstract low-level parent class for generic compression/decompression algorithms
  // - will encapsulate the compression algorithm with crc32c hashing
  // - all Algo* abstract methods should be overriden by inherited classes
  // - don't inherit from TSynPersistent since we don't need any of it
  TAlgoCompress = class
  protected
    fAlgoID: byte;
    fAlgoHasForcedFormat: boolean;
    fAlgoFileExt: TFileName;
    procedure EnsureAlgoHasNoForcedFormat(const caller: shortstring);
  public
    /// computes by default the crc32c() digital signature of the buffer
    function AlgoHash(Previous: cardinal;
      Data: pointer; DataLen: integer): cardinal; overload; virtual;
    /// computes the digital signature of the buffer, or Hash32() if specified
    function AlgoHash(ForceHash32: boolean;
      Data: pointer; DataLen: integer): cardinal; overload;
    /// get maximum possible (worse) compressed size for the supplied length
    function AlgoCompressDestLen(PlainLen: integer): integer; virtual; abstract;
    /// this method will compress the supplied data
    function AlgoCompress(Plain: pointer; PlainLen: integer;
      Comp: pointer): integer; virtual; abstract;
    /// this method will return the size of the decompressed data
    function AlgoDecompressDestLen(Comp: pointer): integer; virtual; abstract;
    /// this method will decompress the supplied data
    function AlgoDecompress(Comp: pointer; CompLen: integer;
      Plain: pointer): integer; virtual; abstract;
    /// this method will partially and safely decompress the supplied data
    // - expects PartialLen <= result < PartialLenMax, depending on the algorithm
    function AlgoDecompressPartial(Comp: pointer; CompLen: integer;
      Partial: pointer; PartialLen, PartialLenMax: integer): integer; virtual; abstract;
    /// contains a genuine byte identifier for this algorithm
    // - 0 is reserved for stored, 1 for TAlgoSynLz, 2/3 for TAlgoDeflate/Fast
    // (in mormot.core.zip.pas), 4/5/6 for TAlgoLizard/Fast/Huffman
    // (in mormot.lib.lizard.pas), 7/8 for TAlgoRleLZ/TAlgoRle, 9/10 for limited
    // TAlgoGZ/TAlgoGZFast (in mormot.core.zip.pas)
    property AlgoID: byte
      read fAlgoID;
    /// the usual file extension of this algorithm
    // - e.g. '.synlz' or '.synz' or '.synliz' for SynLZ, Deflate or Lizard
    property AlgoFileExt: TFileName
      read fAlgoFileExt;
    /// if this algorithm does not supports our custom storage format
    // - e.g. AlgoGZ set true and only supports plain buffers and files methods
    // and would raise EAlgoCompress when stream methods are used
    property AlgoHasForcedFormat: boolean
      read fAlgoHasForcedFormat;
  public
    /// will register AlgoID in the global list, for Algo() class methods
    // - no need to free this instance, since it will be owned by the global list
    // - raise a EAlgoCompress if the class or its AlgoID are already registered
    // - you should never have to call this constructor, but define a global
    // variable holding a reference to a shared instance
    constructor Create; virtual;
    /// finalize this algorithm
    destructor Destroy; override;
    /// get maximum possible (worse) compressed size for the supplied length
    // - including the crc32c + algo 9 bytes header
    function CompressDestLen(PlainLen: integer): integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// compress a memory buffer with crc32c hashing to a RawByteString
    function Compress(const Plain: RawByteString;
      CompressionSizeTrigger: integer = 100;
      CheckMagicForCompressed: boolean = false;
      BufferOffset: integer = 0): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// compress a memory buffer with crc32c hashing to a RawByteString
    function Compress(Plain: PAnsiChar; PlainLen: integer;
      CompressionSizeTrigger: integer = 100;
      CheckMagicForCompressed: boolean = false;
      BufferOffset: integer = 0): RawByteString; overload; virtual;
    /// compress a memory buffer with crc32c hashing
    // - supplied Comp buffer should contain at least CompressDestLen(PlainLen) bytes
    function Compress(Plain, Comp: PAnsiChar; PlainLen, CompLen: integer;
      CompressionSizeTrigger: integer = 100;
      CheckMagicForCompressed: boolean = false): integer; overload; virtual;
    /// compress a memory buffer with crc32c hashing to a TByteDynArray
    function CompressToBytes(const Plain: RawByteString;
      CompressionSizeTrigger: integer = 100;
      CheckMagicForCompressed: boolean = false): TByteDynArray; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// compress a memory buffer with crc32c hashing to a TByteDynArray
    function CompressToBytes(Plain: PAnsiChar; PlainLen: integer;
      CompressionSizeTrigger: integer = 100;
      CheckMagicForCompressed: boolean = false): TByteDynArray; overload;
    /// uncompress a RawByteString memory buffer with crc32c hashing
    function Decompress(const Comp: RawByteString; Load: TAlgoCompressLoad = aclNormal;
      BufferOffset: integer = 0): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// uncompress a RawByteString memory buffer with crc32c hashing
    // - returns TRUE on success
    function TryDecompress(const Comp: RawByteString; out Dest: RawByteString;
      Load: TAlgoCompressLoad = aclNormal): boolean;
    /// uncompress a memory buffer with crc32c hashing
    procedure Decompress(Comp: PAnsiChar; CompLen: integer; out result: RawByteString;
      Load: TAlgoCompressLoad = aclNormal; BufferOffset: integer = 0); overload;
    /// uncompress a RawByteString memory buffer with crc32c hashing
    function Decompress(const Comp: TByteDynArray): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// uncompress a RawByteString memory buffer with crc32c hashing
    // - returns nil if crc32 hash failed, i.e. if the supplied Comp is not correct
    // - returns a pointer to the uncompressed data and fill PlainLen variable,
    // after crc32c hash
    // - avoid any memory allocation in case of a stored content - otherwise, would
    // uncompress to the tmp variable, and return pointer(tmp) and length(tmp)
    function Decompress(const Comp: RawByteString; out PlainLen: integer;
      var tmp: RawByteString; Load: TAlgoCompressLoad = aclNormal): pointer; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// uncompress a RawByteString memory buffer with crc32c hashing
    // - returns nil if crc32 hash failed, i.e. if the supplied Data is not correct
    // - returns a pointer to an uncompressed data buffer of PlainLen bytes
    // - avoid any memory allocation in case of a stored content - otherwise, would
    // uncompress to the tmp variable, and return pointer(tmp) and length(tmp)
    function Decompress(Comp: PAnsiChar; CompLen: integer; out PlainLen: integer;
      var tmp: RawByteString; Load: TAlgoCompressLoad = aclNormal): pointer; overload;
    /// decode the header of a memory buffer compressed via the Compress() method
    // - validates the crc32c of the compressed data (unless Load=aclNoCrcFast),
    // then return the uncompressed size in bytes, or 0 if the crc32c does not match
    // - should call DecompressBody() later on to actually retrieve the content
    function DecompressHeader(Comp: PAnsiChar; CompLen: integer;
      Load: TAlgoCompressLoad = aclNormal): integer; virtual;
    /// decode the content of a memory buffer compressed via the Compress() method
    // - PlainLen has been returned by a previous call to DecompressHeader()
    function DecompressBody(Comp, Plain: PAnsiChar; CompLen, PlainLen: integer;
      Load: TAlgoCompressLoad = aclNormal): boolean; virtual;
    /// partial decoding of a memory buffer compressed via the Compress() method
    // - returns 0 on error, or how many bytes have been written to Partial
    // - will call virtual AlgoDecompressPartial() which is slower, but expected
    // to avoid any buffer overflow on the Partial destination buffer
    // - some algorithms (e.g. Lizard) may need some additional bytes in the
    // decode buffer, so PartialLenMax bytes should be allocated in Partial^,
    // with PartialLenMax > expected PartialLen, and returned bytes may be >
    // PartialLen, but always <= PartialLenMax
    function DecompressPartial(Comp, Partial: PAnsiChar; CompLen,
      PartialLen, PartialLenMax: integer): integer; virtual;
    /// compress a Stream content using this compression algorithm
    // - source Stream may be read and compressed by ChunkBytes = 4MB chunks
    // - a 32-bit Magic number identifies the compressed content chunks
    // - WithTrailer would finish the Dest output with a trailer block to locate
    // the position of the compressed data, to be used e.g. when it is appended
    // - follow the StreamSynLZ() deprecated function format, if ForceHash32=true
    // and WithTrailer=true so that Hash32() is used instead of AlgoHash()
    function StreamCompress(Source, Dest: TStream; Magic: cardinal;
      ForceHash32: boolean = false; WithTrailer: boolean = true;
      ChunkBytes: PtrInt = 4 shl 20): Int64; overload;
    /// compress a Stream content using this compression algorithm into a file
    // - just a wrapper around the overloaded StreamCompress() method
    function StreamCompress(Source: TStream; const DestFile: TFileName;
      Magic: cardinal; ForceHash32: boolean = false;
      WithTrailer: boolean = true; ChunkBytes: PtrInt = 4 shl 20): Int64; overload;
    /// uncompress a Stream previously compressed via StreamCompress()
    // - you should specify a Magic number to be used to identify the compressed
    // Stream format
    // - if Source is not positioned at compressed data beginning, a trailer is
    // searched at the end of the Source stream to get the proper location
    // - returns true on success, and false on decoding error - but some chunks
    // may have been decompressed in Dest even if false is returned
    function StreamUnCompress(Source, Dest: TStream; Magic: cardinal;
      ForceHash32: boolean = false): boolean; overload;
    /// uncompress a Stream previously compressed via StreamCompress()
    // - return nil on decompression error, or a new TMemoryStream instance
    // - follow the StreamUnSynLZ() deprecated function format, if ForceHash32=true
    // so that Hash32() is used instead of the AlgoHash() of this instance
    function StreamUnCompress(Source: TStream; Magic: cardinal;
      ForceHash32: boolean = false): TMemoryStream; overload;
    /// uncompress a File previously compressed via StreamCompress() as TStream
    // - you should specify a Magic number to be used to identify the compressed
    // Stream format
    // - follow the StreamUnSynLZ() deprecated function format, if ForceHash32=true
    // so that Hash32() is used instead of the AlgoHash() of this instance
    // - if the compressed data is not at Source file beginning, a trailer is
    // searched at the end of the Source content to get the proper location
    function StreamUnCompress(const Source: TFileName; Magic: cardinal;
      ForceHash32: boolean = false): TMemoryStream; overload;
    /// compute the length of a given StreamCompress() buffer from its trailer
    // - allows to replace an existing appended content, for instance
    // - expects StreamCompress(WithTrailer=true) format
    function StreamComputeLen(P: PAnsiChar; Len: PtrUInt; Magic: cardinal): integer;
    /// returns TRUE if the supplied file name is a compressed file,
    // matching the Magic number as supplied to FileCompress() function
    // - follow the FileIsSynLZ() deprecated function format
    // - expects the compressed data to be at file beginning (not appended)
    // - may be overriden to support a standard file layout (e.g. AlgoGZ)
    class function FileIsCompressed(const Name: TFileName;
      Magic: cardinal): boolean; virtual;
    /// compress a file content using this compression algorithm
    // - source file is split into ChunkBytes blocks (128 MB by default) for
    // fast in-memory compression of any file size, then compressed and
    // including checksums of Source/Dest data
    // - it is not compatible with StreamCompress format, which has no chunking
    // - you should specify a Magic number to be used to identify the compressed
    // file format
    // - follow the FileSynLZ() deprecated function format, if ForceHash32=true
    // so that Hash32() is used instead of the AlgoHash() of this instance
    // - may be overriden to support a standard file layout (e.g. AlgoGZ)
    function FileCompress(const Source, Dest: TFileName; Magic: cardinal;
      ForceHash32: boolean = false; ChunkBytes: Int64 = 128 shl 20;
      WithTrailer: boolean = false): boolean; virtual;
    /// uncompress a file previously compressed via FileCompress()
    // - you should specify a Magic number to be used to identify the compressed
    // file format
    // - follow the FileUnSynLZ() deprecated function format, if ForceHash32=true
    // so that Hash32() is used instead of the AlgoHash() of this instance
    // - may be overriden to support a standard file layout (e.g. AlgoGZ)
    function FileUnCompress(const Source, Dest: TFileName; Magic: cardinal;
      ForceHash32: boolean = false): boolean; virtual;

    /// get the TAlgoCompress instance corresponding to the AlgoID stored
    // in the supplied compressed buffer
    // - returns nil if no algorithm was identified
    class function Algo(Comp: PAnsiChar; CompLen: integer): TAlgoCompress; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the TAlgoCompress instance corresponding to the AlgoID stored
    // in the supplied compressed buffer
    // - returns nil if no algorithm was identified
    // - also identifies "stored" content in IsStored variable
    class function Algo(Comp: PAnsiChar; CompLen: integer;
      out IsStored: boolean): TAlgoCompress; overload;
    /// get the TAlgoCompress instance corresponding to the AlgoID stored
    // in the supplied compressed buffer
    // - returns nil if no algorithm was identified
    class function Algo(const Comp: RawByteString): TAlgoCompress; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the TAlgoCompress instance corresponding to the AlgoID stored
    // in the supplied compressed buffer
    // - returns nil if no algorithm was identified
    class function Algo(const Comp: TByteDynArray): TAlgoCompress; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the TAlgoCompress instance corresponding to the supplied AlgoID
    // - returns nil if no algorithm was identified
    // - stored content is identified as TAlgoSynLZ
    class function Algo(aAlgoID: byte): TAlgoCompress; overload;
    /// quickly validate a compressed buffer content, without uncompression
    // - extract the TAlgoCompress, and call DecompressHeader() to check the
    // hash of the compressed data, and return then uncompressed size
    // - returns 0 on error (e.g. unknown algorithm or incorrect hash)
    class function UncompressedSize(const Comp: RawByteString): integer;
    /// returns the algorithm name, from its classname
    // - e.g. TAlgoSynLZ->'synlz' TAlgoLizard->'lizard' nil->'none'
    // TAlgoDeflateFast->'deflatefast'
    function AlgoName: TShort16;
  end;

  /// implement our fast SynLZ compression as a TAlgoCompress class
  // - please use the AlgoSynLZ global variable methods instead of the deprecated
  // SynLZCompress/SynLZDecompress wrapper functions
  TAlgoSynLZ = class(TAlgoCompress)
  public
    /// set AlgoID = 1 as genuine byte identifier for SynLZ
    constructor Create; override;
    /// get maximum possible (worse) SynLZ compressed size for the supplied length
    function AlgoCompressDestLen(PlainLen: integer): integer; override;
    /// compress the supplied data using SynLZ
    function AlgoCompress(Plain: pointer; PlainLen: integer; Comp: pointer): integer; override;
    /// return the size of the SynLZ decompressed data
    function AlgoDecompressDestLen(Comp: pointer): integer; override;
    /// decompress the supplied data using SynLZ
    function AlgoDecompress(Comp: pointer; CompLen: integer; Plain: pointer): integer; override;
    /// partial (and safe) decompression of the supplied data using SynLZ
    function AlgoDecompressPartial(Comp: pointer; CompLen: integer;
      Partial: pointer; PartialLen, PartialLenMax: integer): integer; override;
  end;

  TAlgoCompressWithNoDestLenProcess = (
    doCompress,
    doUnCompress,
    doUncompressPartial);

  /// abstract class storing the plain length before calling compression API
  // - some libraries (e.g. Deflate or Lizard) don't provide the uncompressed
  // length from its output buffer - inherit from this class to store this value
  // as ToVarUInt32, and override the RawProcess abstract protected method
  TAlgoCompressWithNoDestLen = class(TAlgoCompress)
  protected
    /// inherited classes should implement this single method for the actual process
    // - dstMax is mainly used for doUncompressPartial
    function RawProcess(src, dst: pointer; srcLen, dstLen, dstMax: integer;
      process: TAlgoCompressWithNoDestLenProcess): integer; virtual; abstract;
  public
    /// performs the compression, storing PlainLen and calling protected RawProcess
    function AlgoCompress(Plain: pointer; PlainLen: integer; Comp: pointer): integer; override;
    /// return the size of the decompressed data (using FromVarUInt32)
    function AlgoDecompressDestLen(Comp: pointer): integer; override;
    /// performs the decompression, retrieving PlainLen and calling protected RawProcess
    function AlgoDecompress(Comp: pointer; CompLen: integer; Plain: pointer): integer; override;
    /// performs the decompression, retrieving PlainLen and calling protected RawProcess
    function AlgoDecompressPartial(Comp: pointer; CompLen: integer;
      Partial: pointer; PartialLen, PartialLenMax: integer): integer; override;
  end;

  /// implement our SynLZ compression with RLE preprocess as a TAlgoCompress class
  // - SynLZ is very good with JSON or text, but not so efficient when its input
  // has a lot of padding (e.g. a database file, or unoptimized raw binary)
  // - this class would make a first pass with RleCompress() before SynLZ
  // - if RLE has no effect during compression, will fallback to plain SynLZ
  // with no RLE pass during decompression
  TAlgoRleLZ = class(TAlgoCompressWithNoDestLen)
  protected
    function RawProcess(src, dst: pointer; srcLen, dstLen, dstMax: integer;
      process: TAlgoCompressWithNoDestLenProcess): integer; override;
  public
    /// set AlgoID = 7 as genuine byte identifier for RLE + SynLZ
    constructor Create; override;
    /// get maximum possible (worse) compressed size for the supplied length
    function AlgoCompressDestLen(PlainLen: integer): integer; override;
  end;

  /// implement RLE compression as a TAlgoCompress class
  // - if RLE has no effect during compression, will fallback to plain store
  TAlgoRle = class(TAlgoCompressWithNoDestLen)
  protected
    function RawProcess(src, dst: pointer; srcLen, dstLen, dstMax: integer;
      process: TAlgoCompressWithNoDestLenProcess): integer; override;
  public
    /// set AlgoID = 8 as genuine byte identifier for RLE
    constructor Create; override;
    /// get maximum possible (worse) compressed size for the supplied length
    function AlgoCompressDestLen(PlainLen: integer): integer; override;
  end;

var
  /// our fast SynLZ compression as a TAlgoCompress class
  // - please use this global variable methods instead of the deprecated
  // SynLZCompress/SynLZDecompress wrapper functions
  AlgoSynLZ: TAlgoCompress;

  /// SynLZ compression with RLE preprocess as a TAlgoCompress class
  // - SynLZ is not efficient when its input has a lot of identical characters
  // (e.g. a database content, or a raw binary buffer) - try with this class
  // which is slower than AlgoSynLZ but may have better ratio on such content
  AlgoRleLZ: TAlgoCompress;

  /// Run-Length-Encoding (RLE) compression as a TAlgoCompress class
  // - if RLE has no effect during compression, will fallback to plain store
  AlgoRle: TAlgoCompress;

var
  /// define how files are compressed by TSynLog.PerformRotation
  // - as used within mormot.core.log.pas unit, and defined in this unit to be
  // available wihout any dependency to it (e.g. in compression units)
  // - assigned to AlgoSynLZ by default for .synlz which is the fastest for logs
  // - you may set AlgoGZFast from mormot.core.zip.pas to generate .gz standard
  // files during TSynLog file rotation (with libdeflate if available)
  // - you may set AlgoLizardFast or AlgoLizardHuffman as non-standard
  // alternatives (default AlgoLizard is much slower and less efficient on logs)
  // - if you set nil, no compression will take place during rotation
  // - note that compression itself is run in the logging background thread
  LogCompressAlgo: TAlgoCompress;

  /// internal wrapper function used by TSynLogArchiveEvent handlers to compress
  // and delete older .log files using our proprietary FileCompress format for
  // a given algorithm
  // - as used within mormot.core.log.pas unit, and defined in this unit to be
  // available wihout any dependency to it (e.g. in compression units)
  // - called by EventArchiveLizard/EventArchiveSynLZ to implement
  // .synlz/.synliz archival
  LogCompressAlgoArchive: function(aAlgo: TAlgoCompress; aMagic: cardinal;
    const aOldLogFileName, aDestinationPath: TFileName): boolean;

const
  /// CompressionSizeTrigger parameter SYNLZTRIG[true] will disable then
  // SynLZCompress() compression
  SYNLZTRIG: array[boolean] of integer = (
    100, maxInt);

  /// used e.g. as when ALGO_SAFE[SafeDecompression] for TAlgoCompress.Decompress
  ALGO_SAFE: array[boolean] of TAlgoCompressLoad = (
    aclNormal, aclSafeSlow);

  COMPRESS_STORED = #0;
  COMPRESS_SYNLZ = 1;


/// fast concatenation of several AnsiStrings
function RawByteStringArrayConcat(const Values: array of RawByteString): RawByteString;

/// creates a TBytes from a RawByteString memory buffer
procedure RawByteStringToBytes(const buf: RawByteString; out bytes: TBytes);

/// creates a RawByteString memory buffer from a TBytes content
procedure BytesToRawByteString(const bytes: TBytes; out buf: RawByteString);
  {$ifdef HASINLINE}inline;{$endif}

/// creates a RawByteString memory buffer from an embedded resource
// - returns '' if the resource is not found
// - warning: resources size may be rounded up to alignment
// - you can specify a library (dll) resource instance handle, if needed
procedure ResourceToRawByteString(const ResName: string; ResType: PChar;
  out buf: RawByteString; Instance: TLibHandle = 0);

/// creates a RawByteString memory buffer from an SynLZ-compressed embedded resource
// - returns '' if the resource is not found
// - this method would use SynLZDecompress() after ResourceToRawByteString(),
// with a ResType=PChar(10) (i.e. RC_DATA)
// - you can specify a library (dll) resource instance handle, if needed
procedure ResourceSynLZToRawByteString(const ResName: string;
  out buf: RawByteString; Instance: TLibHandle = 0);

{$ifndef PUREMORMOT2}

function StreamSynLZComputeLen(P: PAnsiChar;
  Len, Magic: cardinal): integer; deprecated;
function StreamSynLZ(Source: TCustomMemoryStream; Dest: TStream;
  Magic: cardinal): integer; overload; deprecated;
function StreamSynLZ(Source: TCustomMemoryStream; const DestFile: TFileName;
  Magic: cardinal): integer; overload; deprecated;
function FileSynLZ(const Source, Dest: TFileName; Magic: cardinal): boolean; deprecated;
function FileUnSynLZ(const Source, Dest: TFileName; Magic: cardinal): boolean; deprecated;
function FileIsSynLZ(const Name: TFileName; Magic: cardinal): boolean; deprecated;
function StreamUnSynLZ(const Source: TFileName; Magic: cardinal): TMemoryStream; overload; deprecated;
function StreamUnSynLZ(Source: TStream; Magic: cardinal): TMemoryStream; overload; deprecated;
function SynLZCompress(const Data: RawByteString; CompressionSizeTrigger: integer = 100;
  CheckMagicForCompressed: boolean = false): RawByteString; overload;
procedure SynLZCompress(P: PAnsiChar; PLen: integer; out Result: RawByteString;
  CompressionSizeTrigger: integer = 100; CheckMagicForCompressed: boolean = false); overload;
function SynLZCompress(P, Dest: PAnsiChar; PLen, DestLen: integer;
  CompressionSizeTrigger: integer = 100; CheckMagicForCompressed: boolean = false): integer; overload;
function SynLZDecompress(const Data: RawByteString): RawByteString; overload;
procedure SynLZDecompress(P: PAnsiChar; PLen: integer; out Result: RawByteString;
  SafeDecompression: boolean = false); overload;
function SynLZCompressToBytes(const Data: RawByteString;
  CompressionSizeTrigger: integer = 100): TByteDynArray; overload;
function SynLZCompressToBytes(P: PAnsiChar; PLen: integer;
  CompressionSizeTrigger: integer = 100): TByteDynArray; overload;
function SynLZDecompress(const Data: TByteDynArray): RawByteString; overload;
function SynLZDecompress(const Data: RawByteString; out Len: integer;
  var tmp: RawByteString): pointer; overload;
function SynLZDecompress(P: PAnsiChar; PLen: integer; out Len: integer;
  var tmp: RawByteString): pointer; overload;
function SynLZDecompressHeader(P: PAnsiChar; PLen: integer): integer;
function SynLZDecompressBody(P,Body: PAnsiChar; PLen, BodyLen: integer;
  SafeDecompression: boolean = false): boolean;
function SynLZDecompressPartial(P, Partial: PAnsiChar; PLen, PartialLen: integer): integer;

{$endif PUREMORMOT2}


{ ****************** TFastReader / TBufferWriter Binary Streams }

type
  /// exception raised during TFastReader decoding
  EFastReader = class(ESynException);

  /// event signature to customize TFastReader.ErrorOverflow notification
  TOnFastReaderErrorOverflow = procedure of object;

  /// event signature to customize TFastReader.ErrorData notification
  TOnFastReaderErrorData = procedure(const fmt: RawUtf8;
    const args: array of const) of object;

  /// safe decoding of a TBufferWriter content from an in-memory buffer
  // - raise a EFastReader exception on decoding error (e.g. if a buffer
  // overflow may occur) or call OnErrorOverflow/OnErrorData event handlers
  {$ifdef USERECORDWITHMETHODS}
  TFastReader = record
  {$else}
  TFastReader = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the current position in the memory
    P: PAnsiChar;
    /// the last position in the buffer
    Last: PAnsiChar;
    /// use this event to customize the ErrorOverflow process
    OnErrorOverflow: TOnFastReaderErrorOverflow;
    /// use this event to customize the ErrorData process
    OnErrorData: TOnFastReaderErrorData;
    /// when used to unserialize variants, stores options for TDocVariant creation
    // - contains a PDocVariantOptions reference pointer as defined in the
    // mormot.core.data unit
    CustomVariants: pointer;
    /// some opaque value, e.g. a version number to define the binary layout
    Tag: PtrInt;
    /// initialize the reader from a memory block
    procedure Init(Buffer: pointer; Len: PtrInt); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// initialize the reader from a RawByteString content
    procedure Init(const Buffer: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// raise a EFastReader with "Reached End of Input" error message
    procedure ErrorOverflow;
    /// raise a EFastReader with "Incorrect Data: ...." error message
    procedure ErrorData(const fmt: RawUtf8; const args: array of const); overload;
    /// raise a EFastReader with "Incorrect Data: ...." error message
    procedure ErrorData(const msg: shortstring); overload;
    /// read the next 32-bit signed value from the buffer
    function VarInt32: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next 32-bit unsigned value from the buffer
    function VarUInt32: cardinal;
    /// try to read the next 32-bit signed value from the buffer
    // - don't change the current position
    function PeekVarInt32(out value: PtrInt): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// try to read the next 32-bit unsigned value from the buffer
    // - don't change the current position
    function PeekVarUInt32(out value: PtrUInt): boolean;
    /// read the next 32-bit unsigned value from the buffer
    // - this version won't call ErrorOverflow, but return false on error
    // - returns true on read success
    function VarUInt32Safe(out Value: cardinal): boolean;
    /// read the next 64-bit signed value from the buffer
    function VarInt64: Int64;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next 64-bit unsigned value from the buffer
    function VarUInt64: QWord;
    /// read the next RawUtf8 value from the buffer
    function VarUtf8: RawUtf8; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next RawUtf8 value from the buffer
    procedure VarUtf8(out result: RawUtf8); overload;
    /// read the next RawUtf8 value from the buffer
    // - this version won't call ErrorOverflow, but return false on error
    // - returns true on read success
    function VarUtf8Safe(out Value: RawUtf8): boolean;
    /// read the next RawByteString value from the buffer
    function VarString: RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next RawByteString value from the buffer
    function VarString(CodePage: integer): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next pointer and length value from the buffer
    procedure VarBlob(out result: TValueResult); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next pointer and length value from the buffer
    function VarBlob: TValueResult; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// copy the next VarBlob value from the buffer into a TSynTempBuffer
    procedure VarBlob(out Value: TSynTempBuffer); overload;
    /// read the next pointer and length value from the buffer
    // - this version won't call ErrorOverflow, but return false on error
    // - returns true on read success
    function VarBlobSafe(out Value: TValueResult): boolean;
    /// read the next ShortString value from the buffer
    function VarShortString: ShortString;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast ignore the next VarUInt32/VarInt32/VarUInt64/VarInt64 value
    // - don't raise any exception, so caller could check explicitly for any EOF
    procedure VarNextInt; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast ignore the next count VarUInt32/VarInt32/VarUInt64/VarInt64 values
    // - don't raise any exception, so caller could check explicitly for any EOF
    procedure VarNextInt(count: integer); overload;
    /// read the next byte from the buffer
    function NextByte: byte;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next byte from the buffer, checking
    function NextByteSafe(dest: pointer): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next 2 bytes from the buffer as a 16-bit unsigned value
    function Next2: cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next 2 bytes from the buffer as a 16-bit big-endian value
    function Next2BigEndian: cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next 4 bytes from the buffer as a 32-bit unsigned value
    function Next4: cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    /// read the next 8 bytes from the buffer as a 64-bit unsigned value
    function Next8: Qword;
      {$ifdef HASINLINE}inline;{$endif}
    /// consumes the next byte from the buffer, if matches a given value
    function NextByteEquals(Value: byte): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the current position, and move ahead the specified bytes
    function Next(DataLen: PtrInt): pointer;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the current position, and move ahead the specified bytes
    function NextSafe(out Data: Pointer; DataLen: PtrInt): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// copy data from the current position, and move ahead the specified bytes
    procedure Copy(Dest: Pointer; DataLen: PtrInt);
      {$ifdef HASINLINE}inline;{$endif}
    /// copy data from the current position, and move ahead the specified bytes
    // - this version won't call ErrorOverflow, but return false on error
    // - returns true on read success
    function CopySafe(Dest: Pointer; DataLen: PtrInt): boolean;
    /// retrieved cardinal values encoded with TBufferWriter.WriteVarUInt32Array
    // - Values[] will be resized only if it is not long enough, to spare heap
    // - returns decoded count in Values[], which may not be length(Values)
    // - wkFakeMarker will return -count and the caller should make the decoding
    function ReadVarUInt32Array(var Values: TIntegerDynArray): PtrInt;
    /// retrieved Int64 values encoded with TBufferWriter.WriteVarUInt64DynArray
    // - Values[] will be resized only if it is not long enough, to spare heap
    // - returns decoded count in Values[], which may not be length(Values)
    function ReadVarUInt64Array(var Values: TInt64DynArray): PtrInt;
    /// retrieved RawUtf8 values encoded with TFileBufferWriter.WriteRawUtf8DynArray
    // - returns the number of items read into Values[] (may differ from length(Values))
    function ReadVarRawUtf8DynArray(var Values: TRawUtf8DynArray): PtrInt;
    /// retrieve some TAlgoCompress buffer, appended via Write()
    // - BufferOffset could be set to reserve some bytes before the uncompressed buffer
    function ReadCompressed(Load: TAlgoCompressLoad = aclNormal;
      BufferOffset: integer = 0): RawByteString;
    /// returns TRUE if the current position is the end of the input stream
    function EOF: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns remaining length (difference between Last and P)
    function RemainingLength: PtrUInt;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// exception raised during buffer processing
  EBufferException = class(ESynException);

  /// available kind of integer array storage, corresponding to the data layout
  // of TBufferWriter
  // - wkUInt32 will write the content as "plain" 4 bytes binary (this is the
  // prefered way if the integers can be negative)
  // - wkVarUInt32 will write the content using our 32-bit variable-length integer
  // encoding
  // - wkVarInt32 will write the content using our 32-bit variable-length integer
  // encoding and the by-two complement (0=0,1=1,2=-1,3=2,4=-2...)
  // - wkSorted will write an increasing array of integers, handling the special
  // case of a difference of similar value (e.g. 1) between two values - note
  // that this encoding is efficient only if the difference is mainly < 253
  // - wkOffsetU and wkOffsetI will write the difference between two successive
  // values, with detection of any constant difference (unsigned or signed)
  // - wkFakeMarker won't be used by WriteVarUInt32Array, but to notify a
  // custom encoding
  TBufferWriterKind = (
    wkUInt32,
    wkVarUInt32,
    wkVarInt32,
    wkSorted,
    wkOffsetU,
    wkOffsetI,
    wkFakeMarker);

  /// this class can be used to speed up writing to a file or a stream
  // - big speed up if data is written in small blocks
  // - also handle optimized storage of any integer/Int64/RawUtf8 values
  // - use TFileBufferReader or TFastReader for decoding of the stored binary
  TBufferWriter = class
  protected
    fPos: PtrInt;
    fBufLen, fBufLen16: PtrInt;
    fBuffer: PByteArray;
    fStream: TStream;
    fTotalFlushed: Int64;
    fBufferInternal: pointer;
    fInternalStream: boolean;
    fTag: PtrInt;
    procedure InternalFlush;
    function GetTotalWritten: Int64;
      {$ifdef HASINLINE}inline;{$endif}
    procedure InternalWrite(Data: pointer; DataLen: PtrInt);
    procedure FlushAndWrite(Data: pointer; DataLen: PtrInt);
    procedure Setup(aStream: TStream; aBuf: pointer; aLen: integer);
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// initialize the buffer, and specify a file handle to use for writing
    // - define an internal buffer of the specified size
    constructor Create(aFile: THandle; BufLen: integer = 65536); overload;
    /// initialize the buffer, and specify a TStream to use for writing
    // - define an internal buffer of the specified size
    constructor Create(aStream: TStream; BufLen: integer = 65536); overload;
    /// initialize the buffer, and specify a file to use for writing
    // - define an internal buffer of the specified size
    // - would replace any existing file by default, unless Append is TRUE
    constructor Create(const aFileName: TFileName; BufLen: integer = 65536;
      Append: boolean = false); overload;
    /// initialize with a specified buffer and an existing TStream instance
    // - use a specified external buffer (which may be allocated on stack),
    // to avoid a memory allocation
    constructor Create(aStream: TStream; aTempBuf: pointer; aTempLen: integer); overload;
    /// initialize the buffer, using an owned TStream instance
    // - parameter could be e.g. TMemoryStream or TRawByteStringStream
    // - use Flush then TMemoryStream(Stream) to retrieve its content, or
    // FlushTo if TRawByteStringStream was used
    // - Write() fails over 800MB (_STRMAXSIZE) for a TRawByteStringStream
    constructor Create(aClass: TStreamClass; BufLen: integer = 4096); overload;
    /// initialize with a specified buffer and an owned TStream
    // - use a specified external buffer (which may be allocated on stack),
    // to avoid a memory allocation
    // - aClass could be e.g. TMemoryStream or TRawByteStringStream
    constructor Create(aClass: TStreamClass; aTempBuf: pointer; aTempLen: integer); overload;
    /// initialize with a stack-allocated 8KB of buffer
    // - destination stream is an owned TRawByteStringStream - so you can
    // call FlushTo to retrieve all written data
    // - Write() fails over 800MB (_STRMAXSIZE) for a TRawByteStringStream
    // - convenient to reduce heap presure, when writing a few KB of data
    constructor Create(const aStackBuffer: TTextWriterStackBuffer); overload;
    /// release internal TStream (after AssignToHandle call)
    // - warning: an explicit call to Flush is needed to write the data pending
    // in internal buffer
    destructor Destroy; override;
    /// append 1 byte of data at the current position
    procedure Write1(Data: byte);
      {$ifdef HASINLINE}inline;{$endif}
    /// append 2 bytes of data at the current position
    procedure Write2(Data: cardinal);
      {$ifdef HASINLINE}inline;{$endif}
    /// append 2 bytes of data, encoded as BigEndian,  at the current position
    procedure Write2BigEndian(Data: cardinal);
      {$ifdef HASINLINE}inline;{$endif}
    /// append 4 bytes of data at the current position
    procedure Write4(Data: integer);
      {$ifdef HASINLINE}inline;{$endif}
    /// append 4 bytes of data, encoded as BigEndian, at the current position
    procedure Write4BigEndian(Data: integer);
      {$ifdef HASINLINE}inline;{$endif}
    /// append 8 bytes of data at the current position
    procedure Write8(Data8Bytes: pointer);
      {$ifdef HASINLINE}inline;{$endif}
    /// append 8 bytes of 64-bit integer at the current position
    procedure WriteI64(Data: Int64);
      {$ifdef HASINLINE}inline;{$endif}
    /// append the same byte a given number of occurrences at the current position
    procedure WriteN(Data: byte; Count: integer);
    /// append some content (may be text or binary) prefixed by its encoded length
    // - will write DataLen as VarUInt32, then the Data content, as expected
    // by FromVarString/FromVarBlob functions
    procedure WriteVar(Data: pointer; DataLen: PtrInt); overload;
    /// append some TTempUtf8 text content prefixed by its encoded length
    // - will also release any memory stored in Item.TempRawUtf8
    procedure WriteVar(var Item: TTempUtf8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append some UTF-8 encoded text at the current position
    // - will write the string length (as VarUInt32), then the string content
    // - is just a wrapper around WriteVar()
    procedure WriteShort(const Text: ShortString);
      {$ifdef HASINLINE}inline;{$endif}
    /// append some length-prefixed UTF-8 text at the current position
    // - will write the string length (as VarUInt32), then the string content, as expected
    // by the FromVarString() function
    // - is just a wrapper around WriteVar()
    procedure Write(const Text: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append some data at the current position
    // - will be inlined as a MoveFast() most of the time
    procedure Write(Data: pointer; DataLen: PtrInt); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append some content at the current position
    // - will write the binary data, without any length prefix
    procedure WriteBinary(const Data: RawByteString);
    /// append "New[0..Len-1] xor Old[0..Len-1]" bytes
    // - as used e.g. by ZeroCompressXor/TSynBloomFilterDiff.SaveTo
    procedure WriteXor(New, Old: PAnsiChar; Len: PtrInt; crc: PCardinal = nil);
    /// append a cardinal value using 32-bit variable-length integer encoding
    procedure WriteVarUInt32(Value: PtrUInt);
    /// append an integer value using 32-bit variable-length integer encoding of
    // the by-two complement of the given value
    procedure WriteVarInt32(Value: PtrInt);
    /// append an integer value using 64-bit variable-length integer encoding of
    // the by-two complement of the given value
    procedure WriteVarInt64(Value: Int64);
    /// append an unsigned integer value using 64-bit variable-length encoding
    procedure WriteVarUInt64(Value: QWord);
    /// append cardinal values (NONE must be negative!) using 32-bit
    // variable-length integer encoding or other specialized algorithm,
    // depending on the data layout
    // - could be decoded later on via TFastReader.ReadVarUInt32Array
    procedure WriteVarUInt32Array(const Values: TIntegerDynArray;
      ValuesCount: integer; DataLayout: TBufferWriterKind);
    /// append cardinal values (NONE must be negative!) using 32-bit
    // variable-length integer encoding or other specialized algorithms,
    // depending on the data layout
    // - could be decoded later on via TFastReader.ReadVarUInt32Array
    procedure WriteVarUInt32Values(Values: PIntegerArray; ValuesCount: integer;
      DataLayout: TBufferWriterKind);
    /// append UInt64 values using 64-bit variable length integer encoding
    // - if Offset is TRUE, then it will store the difference between
    // two values using 64-bit variable-length integer encoding (in this case,
    // a fixed-sized record storage is also handled separately)
    // - could be decoded later on via TFastReader.ReadVarUInt64Array
    procedure WriteVarUInt64DynArray(const Values: TInt64DynArray;
      ValuesCount: integer; Offset: boolean);
    /// append the RawUtf8 dynamic array
    // - handled the fixed size strings array case in a very efficient way
    procedure WriteRawUtf8DynArray(const Values: TRawUtf8DynArray; ValuesCount: integer);
    /// append a RawUtf8 array of values, from its low-level memory pointer
    // - handled the fixed size strings array case in a very efficient way
    procedure WriteRawUtf8Array(Values: PPtrUIntArray; ValuesCount: integer);
    /// append a TStream content
    // - is StreamSize is left as -1, the Stream.Size is used
    // - the size of the content is stored in the resulting stream
    procedure WriteStream(aStream: TCustomMemoryStream; aStreamSize: integer = -1);
    /// allows to write directly to a memory buffer
    // - caller should specify the maximum possible number of bytes to be written
    // - then write the data to the returned pointer, and call DirectWriteFlush
    // - if len is bigger than the internal buffer, tmp will be used instead
    function DirectWritePrepare(maxlen: PtrInt; var tmp: RawByteString): PAnsiChar;
    /// finalize a direct write to a memory buffer
    // - by specifying the number of bytes written to the buffer
    procedure DirectWriteFlush(len: PtrInt; const tmp: RawByteString);
    /// allows to write directly to a memory buffer
    // - caller should specify the maximum possible number of bytes to be written
    // - len should be smaller than the internal buffer size (not checked)
    function DirectWriteReserve(maxlen: PtrInt): PByte;
      {$ifdef HASINLINE}inline;{$endif}
    /// flush DirectWriteReserve() content
    procedure DirectWriteReserved(pos: PByte);
      {$ifdef HASINLINE}inline;{$endif}
    /// write any pending data in the internal buffer to the stream
    // - after a Flush, it's possible to call FileSeek64(aFile,....)
    // - returns the number of bytes written between two FLush method calls
    function Flush: Int64;
    /// write any pending data, then create a RawByteString from the content
    // - raise an exception if internal Stream is not a TRawByteStringStream
    function FlushTo: RawByteString;
    /// write any pending data, then create a TBytes array from the content
    // - raise an exception if the size exceeds 800MB (_DAMAXSIZE)
    function FlushToBytes: TBytes;
    /// write any pending data, then call algo.Compress() on the buffer
    // - if algo is left to its default nil, will use global AlgoSynLZ
    // - features direct compression from internal buffer, if stream was not used
    // - BufferOffset could be set to reserve some bytes before the compressed buffer
    // - raise an exception if internal Stream is not a TRawByteStringStream
    function FlushAndCompress(nocompression: boolean = false;
      algo: TAlgoCompress = nil; BufferOffset: integer = 0): RawByteString;
    /// rewind the Stream to the position when Create() was called
    // - note that this does not clear the Stream content itself, just
    // move back its writing position to its initial place
    procedure CancelAll; virtual;
    /// the associated writing stream
    property Stream: TStream
      read fStream;
    /// the current position in the internal buffer
    property BufferPosition: PtrInt
      read fPos;
    /// get the byte count written since last Flush
    property TotalWritten: Int64
      read GetTotalWritten;
    /// simple property used to store some integer content
    property Tag: PtrInt
      read fTag write fTag;
  end;

{$ifndef PUREMORMOT2}

  /// deprecated alias to TBufferWriter binary serializer
  TFileBufferWriter = TBufferWriter;
  TFileBufferWriterKind = TBufferWriterKind;

const
  woHideSynPersistentPassword = woHideSensitivePersonalInformation;

{$endif PUREMORMOT2}


{ ************ Base64, Base64Uri, Base58 and Baudot Encoding / Decoding }

const
  /// UTF-8 encoded \uFFF0 special code to mark Base64 binary content in JSON
  // - Unicode special char U+FFF0 is UTF-8 encoded as EF BF B0 bytes
  // - as generated by BinToBase64WithMagic() functions, and expected by
  // the TExtractInlineParameters decoder
  // - used e.g. when transmitting TDynArray.SaveTo() content
  JSON_BASE64_MAGIC_C = $b0bfef;

  /// UTF-8 encoded \uFFF0 special code to mark Base64 binary content in JSON
  // - Unicode special char U+FFF0 is UTF-8 encoded as EF BF B0 bytes
  // - as generated by BinToBase64WithMagic() functions, and expected by
  // the TExtractInlineParameters decoder
  // - used e.g. when transmitting TDynArray.SaveTo() content
  JSON_BASE64_MAGIC_S: string[3] = #$ef#$bf#$b0;

  /// '"' + UTF-8 encoded \uFFF0 special code to mark Base64 binary in JSON
  JSON_BASE64_MAGIC_QUOTE_C = ord('"') + cardinal(JSON_BASE64_MAGIC_C) shl 8;

  /// '"' + UTF-8 encoded \uFFF0 special code to mark Base64 binary in JSON
  // - defined as a ShortString constant to be used as:
  // ! AddShorter(JSON_BASE64_MAGIC_QUOTE_S);
  JSON_BASE64_MAGIC_QUOTE_S: string[4] = '"'#$ef#$bf#$b0;

/// just a wrapper around Base64ToBin() for in-place decode of JSON_BASE64_MAGIC_C
// '\uFFF0base64encodedbinary' content into binary
// - input ParamValue shall have been checked to match the expected pattern
procedure Base64MagicDecode(var ParamValue: RawUtf8);

/// check and decode '\uFFF0base64encodedbinary' content into binary
// - this method will check the supplied value to match the expected
// JSON_BASE64_MAGIC_C pattern, decode and set Blob and return TRUE
function Base64MagicCheckAndDecode(Value: PUtf8Char; var Blob: RawByteString): boolean; overload;

/// decode '\uFFF0base64encodedbinary' or 'base64encodedbinary' into binary
// - same as Base64MagicCheckAndDecode(), but will detect and ignore the magic
// and not require it
function Base64MagicTryAndDecode(Value: PUtf8Char; ValueLen: integer;
  var Blob: RawByteString): boolean;

/// check and decode '\uFFF0base64encodedbinary' content into binary
// - this method will check the supplied value to match the expected
// JSON_BASE64_MAGIC_C pattern, decode and set Blob and return TRUE
function Base64MagicCheckAndDecode(Value: PUtf8Char; ValueLen: integer;
  var Blob: RawByteString): boolean; overload;

/// check and decode '\uFFF0base64encodedbinary' content into binary
// - this method will check the supplied value to match the expected
// JSON_BASE64_MAGIC_C pattern, decode and set Blob and return TRUE
function Base64MagicCheckAndDecode(Value: PUtf8Char;
  var Blob: TSynTempBuffer; ValueLen: integer = 0): boolean; overload;

/// fast conversion from binary data into Base64 encoded UTF-8 text
function BinToBase64(const s: RawByteString): RawUtf8; overload;

/// fast conversion from binary data into Base64 encoded UTF-8 text
function BinToBase64(Bin: PAnsiChar; BinBytes: integer): RawUtf8; overload;

/// fast conversion from a small binary data into Base64 encoded UTF-8 text
function BinToBase64Short(const s: RawByteString): ShortString; overload;

/// fast conversion from a small binary data into Base64 encoded UTF-8 text
function BinToBase64Short(Bin: PAnsiChar; BinBytes: integer): ShortString; overload;

/// fast conversion from binary data into prefixed/suffixed Base64 encoded UTF-8 text
// - with optional JSON_BASE64_MAGIC_C prefix (UTF-8 encoded \uFFF0 special code)
// - may use AVX2 optimized asm (10GB/s) on FPC x86_64
function BinToBase64(const data, Prefix, Suffix: RawByteString; WithMagic: boolean): RawUtf8; overload;

/// fast conversion from binary into prefixed/suffixed Base64 with 64 chars per line
function BinToBase64Line(sp: PAnsiChar; len: PtrUInt; const Prefix: RawUtf8 = '';
  const Suffix: RawUtf8 = ''): RawUtf8;

/// fast conversion from binary data into Base64 encoded UTF-8 text
// with JSON_BASE64_MAGIC_C prefix (UTF-8 encoded \uFFF0 special code)
function BinToBase64WithMagic(const data: RawByteString): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into Base64 encoded UTF-8 text
// with JSON_BASE64_MAGIC_C prefix (UTF-8 encoded \uFFF0 special code)
function BinToBase64WithMagic(Data: pointer; DataLen: integer): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into Base64 encoded UTF-8 text
// with JSON_BASE64_MAGIC_C prefix (UTF-8 encoded \uFFF0 special code)
procedure BinToBase64WithMagic(Data: pointer; DataLen: integer;
  var Result: RawUtf8); overload;

/// raw function for efficient binary to Base64 encoding of the last bytes
// - don't use this function, but rather the BinToBase64() overloaded functions
procedure Base64EncodeTrailing(rp, sp: PAnsiChar; len: cardinal);
  {$ifdef FPC}inline;{$endif}

/// raw function for efficient binary to Base64 encoding
// - just a wrapper around Base64EncodeMain() + Base64EncodeTrailing()
procedure Base64Encode(rp, sp: PAnsiChar; len: cardinal);

/// fast conversion from Base64 encoded text into binary data
// - is now just an alias to Base64ToBinSafe() overloaded function
// - returns '' if s was not a valid Base64-encoded input
function Base64ToBin(const s: RawByteString): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - is now just an alias to Base64ToBinSafe() overloaded function
// - returns '' if sp/len buffer was not a valid Base64-encoded input
function Base64ToBin(sp: PAnsiChar; len: PtrInt): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - is now just an alias to Base64ToBinSafe() overloaded function
// - returns false and data='' if sp/len buffer was invalid
function Base64ToBin(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - returns TRUE on success, FALSE if sp/len buffer was invvalid
function Base64ToBin(sp: PAnsiChar; len: PtrInt; var Blob: TSynTempBuffer): boolean; overload;

/// fast conversion from Base64 encoded text into binary data
// - returns TRUE on success, FALSE if base64 does not match binlen
// - nofullcheck is deprecated and not used any more, since nofullcheck=false
// is now processed with no performance cost
function Base64ToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt
  {$ifndef PUREMORMOT2} ; nofullcheck: boolean = true {$endif}): boolean; overload;

/// fast conversion from Base64 encoded text into binary data
// - returns TRUE on success, FALSE if base64 does not match binlen
// - nofullcheck is deprecated and not used any more, since nofullcheck=false
// is now processed with no performance cost
function Base64ToBin(const base64: RawByteString; bin: PAnsiChar; binlen: PtrInt
  {$ifndef PUREMORMOT2} ; nofullcheck: boolean = true {$endif}): boolean; overload;

/// fast conversion from Base64 encoded text into binary data
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinSafe(const s: RawByteString): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean; overload;

/// fast conversion from Base64 encoded text into binary data
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt; out data: TBytes): boolean; overload;

/// conversion from Base64 encoded text into binary data, ignoring spaces
// - returns '' if s was not a valid Base64-encoded input once spaces are trimmed
// - consider PemToDer() from mormot.crypt.secure if you need to read PEM content
function Base64ToBinTrim(const s: RawByteString): RawByteString;

/// raw function for efficient binary to Base64 encoding of the main block
// - don't use this function, but rather the BinToBase64() overloaded functions
// - on FPC x86_64, detect and use AVX2 asm for very high throughput (11GB/s)
var Base64EncodeMain: function(rp, sp: PAnsiChar; len: cardinal): integer;

/// raw function for efficient Base64 to binary decoding of the main block
// - don't use this function, but rather the Base64ToBin() overloaded functions
// - on FPC x86_64, detect and use AVX2 asm for very high throughput (9GB/s)
var Base64DecodeMain: function(sp, rp: PAnsiChar; len: PtrInt): boolean;

/// check if the supplied text is a valid Base64 encoded stream
function IsBase64(const s: RawByteString): boolean; overload;

/// check if the supplied text is a valid Base64 encoded stream
function IsBase64(sp: PAnsiChar; len: PtrInt): boolean; overload;

/// retrieve the expected encoded length after Base64 process
function BinToBase64Length(len: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the expected undecoded length of a Base64 encoded buffer
// - here len is the number of bytes in sp
function Base64ToBinLength(sp: PAnsiChar; len: PtrInt): PtrInt;

/// retrieve the expected undecoded length of a Base64 encoded buffer
// - here len is the number of bytes in sp
// - will check supplied text is a valid Base64 encoded stream
function Base64ToBinLengthSafe(sp: PAnsiChar; len: PtrInt): PtrInt;

/// direct low-level decoding of a Base64 encoded buffer
// - here len is the number of 4 chars chunks in sp input
// - deprecated low-level function: use Base64ToBin/Base64ToBinSafe instead
function Base64Decode(sp, rp: PAnsiChar; len: PtrInt): boolean;

/// fast conversion from binary data into Base64-like URI-compatible encoded text
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function BinToBase64uri(const s: RawByteString): RawUtf8; overload;

/// fast conversion from a binary buffer into Base64-like URI-compatible encoded text
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function BinToBase64uri(Bin: PAnsiChar; BinBytes: integer): RawUtf8; overload;

/// fast conversion from a binary buffer into Base64-like URI-compatible encoded ShortString
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - returns '' if BinBytes void or too big for the resulting ShortString
function BinToBase64uriShort(Bin: PAnsiChar; BinBytes: integer): ShortString;

/// conversion from any Base64 encoded value into URI-compatible encoded text
// - warning: will modify the supplied base64 string in-place
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
procedure Base64ToUri(var base64: RawUtf8);

/// low-level conversion from a binary buffer into Base64-like URI-compatible encoded text
// - you should rather use the overloaded BinToBase64uri() functions
procedure Base64uriEncode(rp, sp: PAnsiChar; len: cardinal);

/// retrieve the expected encoded length after Base64-URI process
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function BinToBase64uriLength(len: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the expected undecoded length of a Base64-URI encoded buffer
// - here len is the number of bytes in sp
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBinLength(len: PtrInt): PtrInt;

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBin(sp: PAnsiChar; len: PtrInt): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBin(sp: PAnsiChar; len: PtrInt;
  var bin: RawByteString): boolean; overload;

/// fast conversion from Base64-URI encoded text into binary data
// - caller should always execute temp.Done when finished with the data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBin(sp: PAnsiChar; len: PtrInt;
  var temp: TSynTempBuffer): boolean; overload;

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
function Base64uriToBin(const s: RawByteString): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - will check supplied text is a valid Base64-URI encoded stream
function Base64uriToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt): boolean; overload;

/// fast conversion from Base64-URI encoded text into binary data
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - will check supplied text is a valid Base64-URI encoded stream
function Base64uriToBin(const base64: RawByteString;
  bin: PAnsiChar; binlen: PtrInt): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct low-level decoding of a Base64-URI encoded buffer
// - the buffer is expected to be at least Base64uriToBinLength() bytes long
// - returns true if the supplied sp[] buffer has been successfully decoded
// into rp[] - will break at any invalid character, so is always safe to use
// - in comparison to Base64 standard encoding, will trim any right-sided '='
// unsignificant characters, and replace '+' or '/' by '_' or '-'
// - you should better not use this, but Base64uriToBin() overloaded functions
function Base64uriDecode(sp, rp: PAnsiChar; len: PtrInt): boolean;

/// conversion from a binary buffer into Base58 encoded text as TSynTempBuffer
// - Bitcoin' Base58 was defined as alphanumeric chars without misleading 0O I1
// - Base58 is much slower than Base64, performing in O(n^2) instead of O(n),
// and should not be used on big buffers
// - returns the number of encoded chars encoded into Dest.buf
// - caller should call Dest.Done once it is finished with the output text
function BinToBase58(Bin: PAnsiChar; BinLen: integer;
  var Dest: TSynTempBuffer): integer; overload;

/// conversion from a binary buffer into Base58 encoded text as RawUtf8
// - Bitcoin' Base58 was defined as alphanumeric chars without misleading 0O I1
// - Base58 is much slower than Base64, performing in O(n^2) instead of O(n),
// and should not be used on big buffers
function BinToBase58(Bin: PAnsiChar; BinLen: integer): RawUtf8; overload;

/// conversion from a binary buffer into Base58 encoded text as RawUtf8
// - Bitcoin' Base58 was defined as alphanumeric chars without misleading 0O I1
// - Base58 is much slower than Base64, performing in O(n^2) instead of O(n),
// and should not be used on big buffers
function BinToBase58(const Bin: RawByteString): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// conversion from Base58 encoded text into a binary buffer
// - Bitcoin' Base58 was defined as alphanumeric chars without misleading 0O I1
// - Base58 is much slower than Base64, performing in O(n^2) instead of O(n),
// and should not be used on big buffers
// - returns the number of decoded chars encoded into Dest.buf
// - caller should call Dest.Done once it is finished with the output binary
function Base58ToBin(B58: PAnsiChar; B58Len: integer;
  var Dest: TSynTempBuffer): integer; overload;

/// conversion from Base58 encoded text into a binary string
// - Bitcoin' Base58 was defined as alphanumeric chars without misleading 0O I1
// - Base58 is much slower than Base64, and should not be used on big buffers
// - returns '' if input was not valid Base58 encoded
function Base58ToBin(B58: PAnsiChar; B58Len: integer): RawByteString; overload;

/// conversion from Base58 encoded text into a binary string
// - Bitcoin' Base58 was defined as alphanumeric chars without misleading 0O I1
// - Base58 is much slower than Base64, and should not be used on big buffers
// - returns '' if input was not valid Base58 encoded
function Base58ToBin(const base58: RawUtf8): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// compute the length resulting of Base32 encoding of a binary buffer
// - RFC4648 Base32 is defined as upper alphanumeric without misleading 0O 1I 8B
function BinToBase32Length(BinLen: cardinal): cardinal;
  {$ifdef HASINLINE}inline;{$endif}

/// conversion from a binary buffer into Base32 encoded text  buffer
// - default b32enc is RFC4648 upper alphanumeric without misleading 0O 1I 8B
procedure BinToBase32(Bin: PByteArray; Dest: PAnsiChar; BinLen: PtrInt;
  b32enc: PAnsiChar); overload;

/// conversion from a binary buffer into Base32 encoded text as RawUtf8
// - RFC4648 Base32 is defined as upper alphanumeric without misleading 0O 1I 8B
function BinToBase32(Bin: PAnsiChar; BinLen: PtrInt): RawUtf8; overload;

/// conversion from a binary buffer into Base32 encoded text as RawUtf8
// - RFC4648 Base32 is defined as upper alphanumeric without misleading 0O 1I 8B
function BinToBase32(const Bin: RawByteString): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// conversion from Base32 encoded text into a binary string
// - RFC4648 Base32 is defined as upper alphanumeric without misleading 0O 1I 8B
// - returns '' if input was not valid Base32 encoded
function Base32ToBin(B32: PAnsiChar; B32Len: integer): RawByteString; overload;

/// conversion from Base32 encoded text into a binary string
// - RFC4648 Base32 is defined as upper alphanumeric without misleading 0O 1I 8B
// - returns '' if input was not valid Base32 encoded
function Base32ToBin(const base32: RawUtf8): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a RawBlob from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
function BlobToRawBlob(P: PUtf8Char; Len: integer = 0): RawBlob; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fill a RawBlob from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
procedure BlobToRawBlob(P: PUtf8Char; var result: RawBlob; Len: integer = 0); overload;

/// fill a RawBlob from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
function BlobToRawBlob(const Blob: RawByteString): RawBlob; overload;

/// create a TBytes from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
function BlobToBytes(P: PUtf8Char): TBytes;

/// create a memory stream from TEXT-encoded blob data
// - blob data can be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.) or
// or Base64 encoded content ('\uFFF0base64encodedbinary') or plain TEXT
// - the caller must free the stream instance after use
function BlobToStream(P: PUtf8Char): TStream;

/// creates a TEXT-encoded version of blob data from a RawBlob
// - TEXT will be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.)
function RawBlobToBlob(const RawBlob: RawBlob): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// creates a TEXT-encoded version of blob data from a memory data
// - same as RawBlob, but with direct memory access via a pointer/byte size pair
// - TEXT will be encoded as SQLite3 BLOB literals (X'53514C697465' e.g.)
function RawBlobToBlob(RawBlob: pointer; RawBlobLength: integer): RawUtf8; overload;

/// convert a Base64-encoded content into binary hexadecimal ready for SQL
// - returns e.g. X'53514C697465'
procedure Base64MagicToBlob(Base64: PUtf8Char; var result: RawUtf8);

/// return true if the TEXT is encoded as SQLite3 BLOB literals (X'53514C697465' e.g.)
function isBlobHex(P: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}


type
  /// used by MultiPartFormDataDecode() to return one item of its data
  TMultiPart = record
    Name: RawUtf8;
    FileName: RawUtf8;
    ContentType: RawUtf8;
    Encoding: RawUtf8;
    Content: RawByteString;
  end;
  /// used by MultiPartFormDataDecode() to return all its data items
  TMultiPartDynArray = array of TMultiPart;

/// decode multipart/form-data POST request content into memory
// - following RFC 1867
// - decoded sections are appended to MultiPart[] existing array
function MultiPartFormDataDecode(const MimeType, Body: RawUtf8;
  var MultiPart: TMultiPartDynArray): boolean;

/// used e.g. by MultiPartFormDataEncode and THttpMultiPartStream.Add
function MultiPartFormDataNewBound(var boundaries: TRawUtf8DynArray): RawUtf8;

/// encode multipart fields and files
// - only one of them can be used because MultiPartFormDataDecode must implement
// both decodings
// - MultiPart: parts to build the multipart content from, which may be created
// using MultiPartFormDataAddFile/MultiPartFormDataAddField
// - MultiPartContentType: variable returning
// $ Content-Type: multipart/form-data; boundary=xxx
// where xxx is the first generated boundary
// - MultiPartContent: generated multipart content
// - Rfc2388NestedFiles will force the deprecated nested "multipart/mixed" format
// - consider THttpMultiPartStream from mormot.net.client for huge file content
function MultiPartFormDataEncode(const MultiPart: TMultiPartDynArray;
  var MultiPartContentType, MultiPartContent: RawUtf8;
  Rfc2388NestedFiles: boolean = false): boolean;

/// encode a file in a multipart array
// - FileName: file to encode
// - Multipart: where the part is added
// - Name: name of the part, is empty the name 'File###' is generated
// - consider THttpMultiPartStream from mormot.net.client for huge file content
function MultiPartFormDataAddFile(const FileName: TFileName;
  var MultiPart: TMultiPartDynArray; const Name: RawUtf8 = '';
  const ForcedContentType: RawUtf8 = ''): boolean;

/// encode a field in a multipart array
// - FieldName: field name of the part
// - FieldValue: value of the field
// - Multipart: where the part is added
// - consider THttpMultiPartStream from mormot.net.client for huge file content
function MultiPartFormDataAddField(const FieldName, FieldValue: RawUtf8;
  var MultiPart: TMultiPartDynArray; const ForcedContentType: RawUtf8 = ''): boolean;


/// convert some ASCII-7 text into binary, using Emile Baudot code
// - as used in telegraphs, covering #10 #13 #32 a-z 0-9 - ' , ! : ( + ) $ ? @ . / ;
// charset, following a custom static-huffman-like encoding with 5-bit masks
// - any upper case char will be converted into lowercase during encoding
// - other characters (e.g. UTF-8 accents, or controls chars) will be ignored
// - resulting binary will consume 5 (or 10) bits per character
// - reverse of the BaudotToAscii() function
// - the "baud" symbol rate measurement comes from Emile's name ;)
function AsciiToBaudot(P: PAnsiChar; len: PtrInt): RawByteString; overload;

/// convert some ASCII-7 text into binary, using Emile Baudot code
// - as used in telegraphs, covering #10 #13 #32 a-z 0-9 - ' , ! : ( + ) $ ? @ . / ;
// charset, following a custom static-huffman-like encoding with 5-bit masks
// - any upper case char will be converted into lowercase during encoding
// - other characters (e.g. UTF-8 accents, or controls chars) will be ignored
// - resulting binary will consume 5 (or 10) bits per character
// - reverse of the BaudotToAscii() function
// - the "baud" symbol rate measurement comes from Emile's name ;)
function AsciiToBaudot(const Text: RawUtf8): RawByteString; overload;

/// convert some Baudot code binary, into ASCII-7 text
// - reverse of the AsciiToBaudot() function
// - any uppercase character would be decoded as lowercase - and some characters
// may have disapeared outside of a-z 0-9 - ' , ! : ( + ) $ ? @ . / ; range
// - the "baud" symbol rate measurement comes from Emile's name ;)
function BaudotToAscii(Baudot: PByteArray; len: PtrInt): RawUtf8; overload;

/// convert some Baudot code binary, into ASCII-7 text
// - reverse of the AsciiToBaudot() function
// - any uppercase character would be decoded as lowercase - and some characters
// may have disapeared outside of a-z 0-9 - ' , ! : ( + ) $ ? @ . / ; range
// - the "baud" symbol rate measurement comes from Emile's name ;)
function BaudotToAscii(const Baudot: RawByteString): RawUtf8; overload;



{ ***************** URI-Encoded Text Buffer Process }

/// encode a string as URI parameter encoding, i.e. ' ' as '+'
function UrlEncode(const svar: RawUtf8): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// encode a string as URI parameter encoding, i.e. ' ' as '+'
function UrlEncode(Text: PUtf8Char): RawUtf8; overload;

/// encode a string as URI network name encoding, i.e. ' ' as %20
// - only parameters - i.e. after '?' - should replace spaces by '+'
function UrlEncodeName(const svar: RawUtf8): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// encode a string as URI network name encoding, i.e. ' ' as %20
// - only parameters - i.e. after '?' - should replace spaces by '+'
function UrlEncodeName(Text: PUtf8Char): RawUtf8; overload;

/// encode supplied parameters to be compatible with URI encoding
// - parameters must be supplied two by two, as Name,Value pairs, e.g.
// ! url := UrlEncode(['select','*','where','ID=12','offset',23,'object',aObject]);
// - parameters names should be plain ASCII-7 RFC compatible identifiers
// (0..9a..zA..Z_.~), otherwise their values are skipped
// - parameters values can be either textual, integer or extended, or any TObject
// - TObject serialization into UTF-8 will be processed with ObjectToJson()
function UrlEncode(const NameValuePairs: array of const;
  TrimLeadingQuestionMark: boolean = false): RawUtf8; overload;

/// decode a UrlEncode() URI encoded parameter into its original value
function UrlDecode(U: PUtf8Char): RawUtf8; overload;

/// decode a UrlEncode() URI encoded parameter into its original value
function UrlDecode(const s: RawUtf8): RawUtf8; overload;

/// decode a UrlEncodeName() URI encoded network name into its original value
// - only parameters - i.e. after '?' - should replace spaces by '+'
function UrlDecodeName(U: PUtf8Char): RawUtf8; overload;

/// decode a UrlEncodeName() URI encoded network name into its original value
// - only parameters - i.e. after '?' - should replace spaces by '+'
function UrlDecodeName(const s: RawUtf8): RawUtf8; overload;

/// decode a UrlEncode/UrlEncodeName() URI encoded string into its original value
// - name=false for parameters (after ?), to replace spaces by '+'
procedure UrlDecodeVar(U: PUtf8Char; L: PtrInt; var result: RawUtf8; name: boolean);

/// decode a specified parameter compatible with URI encoding into its original
// textual value
// - UrlDecodeValue('select=%2A&where=LastName%3D%27M%C3%B4net%27','SELECT=',V,@Next)
// will return Next^='where=...' and V='*'
// - Upper should be already uppercased, and end with a '=' character
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeValue(U: PUtf8Char; const Upper: RawUtf8;
  var Value: RawUtf8; Next: PPUtf8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// integer numerical value
// - UrlDecodeInteger('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
// will return Next^='where=...' and O=20
// - Upper should be already uppercased, and end with a '=' character
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeInteger(U: PUtf8Char; const Upper: RawUtf8;
  var Value: integer; Next: PPUtf8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// cardinal numerical value
// - UrlDecodeCardinal('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
// will return Next^='where=...' and O=20
// - Upper should be already uppercased, and end with a '=' character
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeCardinal(U: PUtf8Char; const Upper: RawUtf8;
  var Value: cardinal; Next: PPUtf8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// Int64 numerical value
// - UrlDecodeInt64('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
// will return Next^='where=...' and O=20
// - Upper should be already uppercased, and end with a '=' character
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeInt64(U: PUtf8Char; const Upper: RawUtf8;
  var Value: Int64; Next: PPUtf8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// floating-point value
// - UrlDecodeExtended('price=20.45&where=LastName%3D%27M%C3%B4net%27','PRICE=',P,@Next)
// will return Next^='where=...' and P=20.45
// - Upper should be already uppercased, and end with a '=' character
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeExtended(U: PUtf8Char; const Upper: RawUtf8;
  var Value: TSynExtended; Next: PPUtf8Char = nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// floating-point value
// - UrlDecodeDouble('price=20.45&where=LastName%3D%27M%C3%B4net%27','PRICE=',P,@Next)
// will return Next^='where=...' and P=20.45
// - Upper should be already uppercased, and end with a '=' character
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeDouble(U: PUtf8Char; const Upper: RawUtf8;
  var Value: double; Next: PPUtf8Char = nil): boolean;

/// returns TRUE if all supplied parameters do exist in the URI encoded text
// - CsvNames parameter shall provide as a CSV list of names
// - e.g. UrlDecodeNeedParameters('price=20.45&where=LastName%3D','price,where')
// will return TRUE
function UrlDecodeNeedParameters(U, CsvNames: PUtf8Char): boolean;

/// decode the next Name=Value&.... pair from input URI
// - Name is returned directly (should be plain ASCII 7-bit text)
// - Value is returned after URI decoding (from %.. patterns)
// - if a pair is decoded, return a PUtf8Char pointer to the next pair in
// the input buffer, or points to #0 if all content has been processed
// - if a pair is not decoded, return nil
function UrlDecodeNextNameValue(U: PUtf8Char;
  var Name, Value: RawUtf8): PUtf8Char;

/// decode a URI-encoded Value from an input buffer
// - decoded value is set in Value out variable
// - returns a pointer just after the decoded value (may points e.g. to
// #0 or '&') - it is up to the caller to continue the process or not
function UrlDecodeNextValue(U: PUtf8Char; out Value: RawUtf8): PUtf8Char;

/// decode a URI-encoded Name from an input buffer
// - decoded value is set in Name out variable
// - returns a pointer just after the decoded name, after the '='
// - returns nil if there was no name=... pattern in U
function UrlDecodeNextName(U: PUtf8Char; out Name: RawUtf8): PUtf8Char;

/// checks if the supplied UTF-8 text don't need URI encoding
// - returns TRUE if all its chars are non-void plain ASCII-7 RFC compatible
// identifiers (0..9a..zA..Z-_.~)
function IsUrlValid(P: PUtf8Char): boolean;

/// checks if the supplied UTF-8 text values don't need URI encoding
// - returns TRUE if all its chars of all strings are non-void plain ASCII-7 RFC
// compatible identifiers (0..9a..zA..Z-_.~)
function AreUrlValid(const Url: array of RawUtf8): boolean;

/// ensure the supplied URI contains a trailing '/' charater
function IncludeTrailingUriDelimiter(const URI: RawByteString): RawByteString;


{ *********** Basic MIME Content Types Support }

type
  /// some of the best-known mime types
  // - subset of the whole IANA list which can be quite huge (>1500 items)
  TMimeType = (
    mtUnknown,
    mtPng,
    mtGif,
    mtTiff,
    mtJpg,
    mtBmp,
    mtDoc,
    mtPpt,
    mtXls,
    mtHtml,
    mtCss,
    mtJS,
    mtXIcon,
    mtFont,
    mtText,
    mtSvg,
    mtXml,
    mtWebp,
    mtManifest,
    mtJson,
    mtOgg,
    mtMp4,
    mtMp2,
    mtMpeg,
    mtH264,
    mtWma,
    mtWmv,
    mtAvi,
    mtGzip,
    mtWebm,
    mtRar,
    mt7z,
    mtZip,
    mtBz2,
    mtPdf,
    mtSQlite3,
    mtXcomp);
  PMimeType = ^TMimeType;

const
  /// the known mime types text representation
  MIME_TYPE: array[TMimeType] of RawUtf8 = (
    '',                              // mtUnknown
    'image/png',                     // mtPng
    'image/gif',                     // mtGif
    'image/tiff',                    // mtTiff
    JPEG_CONTENT_TYPE,               // mtJpg
    'image/bmp',                     // mtBmp
    'application/msword',            // mtDoc
    'application/vnd.ms-powerpoint', // mtPpt
    'application/vnd.ms-excel',      // mtXls
    HTML_CONTENT_TYPE,               // mtHtml
    'text/css',                      // mtCss
    'text/javascript',               // mtJS RFC 9239
    'image/x-icon',                  // mtXIcon
    'font/woff',                     // mtFont RFC 8081
    TEXT_CONTENT_TYPE,               // mtText
    'image/svg+xml',                 // mtSvg
    XML_CONTENT_TYPE,                // mtXml
    'image/webp',                    // mtWebp
    'text/cache-manifest',           // mtManifest
    JSON_CONTENT_TYPE,               // mtJson
    'video/ogg',                     // mtOgg RFC 5334
    'video/mp4',                     // mtMp4 RFC 4337 6381
    'video/mp2',                     // mtMp2
    'audio/mpeg',                    // mtMpeg RFC 3003
    'video/H264',                    // mtH264  RFC 6184
    'audio/x-ms-wma',                // mtWma
    'video/x-ms-wmv',                // mtWmv
    'video/x-msvideo',               // mtAvi
    'application/gzip',              // mtGzip
    'video/webm',                    // mtWebm
    'application/x-rar-compressed',  // mtRar
    'application/x-7z-compressed',   // mt7z
    'application/zip',               // mtZip
    'application/bzip2',             // mtBz2
    'application/pdf',               // mtPdf
    'application/x-sqlite3',         // mtSQlite3
    'application/x-compress');       // mtXcomp

/// retrieve the MIME content type from its file name
function GetMimeContentTypeFromExt(const FileName: TFileName;
  FileExt: PRawUtf8 = nil): TMimeType;

/// retrieve the MIME content type from its file extension text (without '.')
function GetMimeTypeFromExt(const Ext: RawUtf8): TMimeType;

/// retrieve the MIME content type from a supplied binary buffer
function GetMimeContentTypeFromMemory(Content: Pointer; Len: PtrInt): TMimeType;

/// retrieve the MIME content type from a supplied binary buffer
// - inspect the first bytes, to guess from standard known headers
// - return the MIME type, ready to be appended to a 'Content-Type: ' HTTP header
// - returns DefaultContentType if the binary buffer has an unknown layout
function GetMimeContentTypeFromBuffer(Content: Pointer; Len: PtrInt;
  const DefaultContentType: RawUtf8; Mime: PMimeType = nil): RawUtf8;

/// retrieve the MIME content type from its file name or a supplied binary buffer
// - will first check for known file extensions, then inspect the binary content
// - return the MIME type, ready to be appended to a 'Content-Type: ' HTTP header
// - default is DefaultContentType or 'application/octet-stream' (BINARY_CONTENT_TYPE)
// or 'application/fileextension' if FileName was specified
// - see @http://en.wikipedia.org/wiki/Internet_media_type for most common values
function GetMimeContentType(Content: Pointer; Len: PtrInt; const FileName: TFileName = '';
  const DefaultContentType: RawUtf8 = BINARY_CONTENT_TYPE; Mime: PMimeType = nil): RawUtf8;

/// retrieve the HTTP header for MIME content type from a supplied binary buffer
// - just append HEADER_CONTENT_TYPE and GetMimeContentType() result
// - can be used as such:
// !  Call.OutHead := GetMimeContentTypeHeader(Call.OutBody,aFileName);
function GetMimeContentTypeHeader(const Content: RawByteString;
  const FileName: TFileName = ''; Mime: PMimeType = nil): RawUtf8;

const
  /// the "magic" number used to identify .log.synlz compressed files, as
  // created by EventArchiveSynLZ / EventArchiveLizard callbacks
  LOG_MAGIC = $ABA51051;

/// retrieve if some content is compressed, from a supplied binary buffer
// - returns TRUE, if the header in binary buffer "may" be compressed (this
// method can trigger false positives), e.g. begin with most common already
// compressed zip/gz/gif/png/jpeg/avi/mp3/mp4 markers (aka "magic numbers")
function IsContentCompressed(Content: Pointer; Len: PtrInt): boolean;

/// fast guess of the size, in pixels, of a JPEG memory buffer
// - will only scan for basic JPEG structure, up to the StartOfFrame (SOF) chunk
// - returns TRUE if the buffer is likely to be a JPEG picture, and set the
// Height + Width + Bits variable with its dimensions - but there may be false
// positive recognition, and no waranty that the memory buffer is a valid JPEG
// - returns FALSE if the buffer does not have any expected SOI/SOF markers
function GetJpegSize(jpeg: PAnsiChar; len: PtrInt;
  out Height, Width, Bits: integer): boolean; overload;


{ ************* Text Memory Buffers and Files }

type
  {$M+}
  /// able to read a UTF-8 text file using memory map
  // - much faster than TStringList.LoadFromFile()
  // - will ignore any trailing UTF-8 BOM in the file content, but will not
  // expect one either
  TMemoryMapText = class
  protected
    fLines: PPointerArray;
    fLinesMax: integer;
    fCount: integer;
    fMapEnd: PUtf8Char;
    fMap: TMemoryMap;
    fFileName: TFileName;
    fAppendedLines: TRawUtf8DynArray;
    fAppendedLinesCount: integer;
    function GetLine(aIndex: integer): RawUtf8;
      {$ifdef HASINLINE}inline;{$endif}
    function GetString(aIndex: integer): string;
      {$ifdef HASINLINE}inline;{$endif}
    /// call once by Create constructors when fMap has been initialized
    procedure LoadFromMap(AverageLineLength: integer = 32); virtual;
    /// call once per line, from LoadFromMap method
    // - default implementation will set  fLines[fCount] := LineBeg;
    // - override this method to add some per-line process at loading: it will
    // avoid reading the entire file more than once
    procedure ProcessOneLine(LineBeg, LineEnd: PUtf8Char); virtual;
  public
    /// initialize the memory mapped text file
    // - this default implementation just do nothing but is called by overloaded
    // constructors so may be overriden to initialize an inherited class
    constructor Create; overload; virtual;
    /// read an UTF-8 encoded text file
    // - every line beginning is stored into LinePointers[]
    constructor Create(const aFileName: TFileName); overload;
    /// read an UTF-8 encoded text file content
    // - every line beginning is stored into LinePointers[]
    // - this overloaded constructor accept an existing memory buffer (some
    // uncompressed data e.g.)
    constructor Create(aFileContent: PUtf8Char; aFileSize: integer); overload;
    /// release the memory map and internal LinePointers[]
    destructor Destroy; override;
    /// save the whole content into a specified stream
    // - including any runtime appended values via AddInMemoryLine()
    procedure SaveToStream(Dest: TStream; const Header: RawUtf8);
    /// save the whole content into a specified file
    // - including any runtime appended values via AddInMemoryLine()
    // - an optional header text can be added to the beginning of the file
    procedure SaveToFile(FileName: TFileName; const Header: RawUtf8 = '');
    /// add a new line to the already parsed content
    // - this line won't be stored in the memory mapped file, but stay in memory
    // and appended to the existing lines, until this instance is released
    procedure AddInMemoryLine(const aNewLine: RawUtf8); virtual;
    /// clear all in-memory appended rows
    procedure AddInMemoryLinesClear; virtual;
    /// retrieve the number of UTF-8 chars of the given line
    // - warning: no range check is performed about supplied index
    function LineSize(aIndex: integer): integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// check if there is at least a given number of UTF-8 chars in the given line
    // - this is faster than LineSize(aIndex)<aMinimalCount for big lines
    function LineSizeSmallerThan(aIndex, aMinimalCount: integer): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns TRUE if the supplied text is contained in the corresponding line
    function LineContains(const aUpperSearch: RawUtf8; aIndex: integer): boolean; virtual;
    /// retrieve a line content as UTF-8
    // - a temporary UTF-8 string is created
    // - will return '' if aIndex is out of range
    property Lines[aIndex: integer]: RawUtf8
      read GetLine;
    /// retrieve a line content as RTL string type
    // - a temporary RTL string is created (after conversion for UNICODE Delphi)
    // - will return '' if aIndex is out of range
    property Strings[aIndex: integer]: string
      read GetString;
    /// direct access to each text line
    // - use LineSize() method to retrieve line length, since end of line will
    // NOT end with #0, but with #13 or #10
    // - warning: no range check is performed about supplied index
    property LinePointers: PPointerArray
      read fLines;
    /// the memory map used to access the raw file content
    property Map: TMemoryMap
      read fMap;
  published
    /// the file name which was opened by this instance
    property FileName: TFileName
      read fFileName write fFileName;
    /// the number of text lines
    property Count: integer
      read fCount;
  end;
  {$M-}

{$ifndef PUREMORMOT2} // just redirect to mormot.core.text Append(...) overloads
procedure AppendBufferToRawByteString(var Content: RawByteString;
  const Buffer; BufferLen: PtrInt); overload;
procedure AppendBufferToRawByteString(var Content: RawByteString;
  const Buffer: RawByteString); overload; {$ifdef HASINLINE} inline; {$endif}
procedure AppendToRawUtf8(var Text: RawUtf8; const After: RawByteString); overload;
  {$ifdef HASINLINE} inline; {$endif}
procedure AppendBufferToRawUtf8(var Text: RawUtf8;
  Buffer: PUtf8Char; BufferLen: PtrInt); {$ifdef HASINLINE} inline; {$endif}
procedure AppendCharToRawUtf8(var Text: RawUtf8; Ch: AnsiChar);
  {$ifdef HASINLINE} inline; {$endif}
procedure AppendToRawUtf8(var Text: RawUtf8; const After1, After2: RawByteString);
  overload; {$ifdef HASINLINE} inline; {$endif}
{$endif PUREMORMOT2}

/// fast add one character to a RawUtf8 string, if not already present
// - avoid a temporary memory allocation of a string, so faster alternative to
// ! if (Text<>'') and (Text[length(Text)]<>Ch) then Text := Text + ch;
procedure AppendCharOnceToRawUtf8(var Text: RawUtf8; Ch: AnsiChar);

/// fast add some characters to a RawUtf8 string
// - faster than Text := Text+RawUtf8(Buffers[0])+RawUtf8(Buffers[0])+...
// - will handle up to 64 Buffers[] - raise an ESynException on too many Buffers
procedure AppendBuffersToRawUtf8(var Text: RawUtf8; const Buffers: array of PUtf8Char);

/// fast add some characters from a RawUtf8 string into a given buffer
// - warning: the Buffer should contain enough space to store the Text, otherwise
// you may encounter buffer overflows and random memory errors
function AppendRawUtf8ToBuffer(Buffer: PUtf8Char; const Text: RawUtf8): PUtf8Char;

/// fast add some characters from ane buffer into another buffer
// - warning: the Buffer should contain enough space to store the Text, otherwise
// you may encounter buffer overflows and random memory errors
function AppendBufferToBuffer(Buffer: PUtf8Char; Text: pointer; Len: PtrInt): PUtf8Char;
  {$ifdef HASINLINE} inline; {$endif}

/// fast add text conversion of a 32-bit signed integer value into a given buffer
// - warning: the Buffer should contain enough space to store the text, otherwise
// you may encounter buffer overflows and random memory errors
function AppendUInt32ToBuffer(Buffer: PUtf8Char; Value: PtrUInt): PUtf8Char;

/// fast add text conversion of 0-999 integer value into a given buffer
// - warning: it won't check that Value is in 0-999 range
// - up to 4 bytes may be written to the buffer (including trailing #0)
function Append999ToBuffer(Buffer: PUtf8Char; Value: PtrUInt): PUtf8Char;
  {$ifdef HASINLINE}inline;{$endif}

const
  /// can be used to append to most English nouns to form a plural
  // - as used by the Plural() function
  PLURAL_FORM: array[boolean] of RawUtf8 = (
    '', 's');

/// write count number and append 's' (if needed) to form a plural English noun
// - for instance, Plural('row',100) returns '100 rows' with no heap allocation
function Plural(const itemname: ShortString; itemcount: cardinal): ShortString;

/// low-level fast conversion from binary data to escaped text
// - non printable characters will be written as $xx hexadecimal codes
// - will be #0 terminated, with '...' characters trailing on dmax overflow
// - ensure the destination buffer contains at least dmax bytes, which is
// always the case when using LogEscape() and its local TLogEscape variable
function EscapeBuffer(s: PAnsiChar; slen: integer;
  d: PAnsiChar; dmax: integer): PAnsiChar;

type
  /// 512 bytes buffer to be allocated on stack when using LogEscape()
  TLogEscape = array[0..511] of AnsiChar;

/// fill TLogEscape stack buffer with the (hexadecimal) chars of the input binary
// - up to 512 bytes will be escaped and appended to a local temp: TLogEscape
// variable, using the EscapeBuffer() low-level function
// - you can then log the resulting escaped text by passing the returned
// PAnsiChar as % parameter to a TSynLog.Log() method
// - the "enabled" parameter can be assigned from a process option, avoiding to
// process the escape if verbose logs are disabled
// - used e.g. to implement logBinaryFrameContent option for WebSockets
function LogEscape(source: PAnsiChar; sourcelen: integer; var temp: TLogEscape;
  enabled: boolean = true): PAnsiChar;
  {$ifdef HASINLINE}inline;{$endif}

/// returns a text buffer with the (hexadecimal) chars of the input binary
// - is much slower than LogEscape/EscapeToShort, but has no size limitation
function LogEscapeFull(source: PAnsiChar; sourcelen: integer): RawUtf8; overload;

/// returns a text buffer with the (hexadecimal) chars of the input binary
// - is much slower than LogEscape/EscapeToShort, but has no size limitation
function LogEscapeFull(const source: RawByteString): RawUtf8; overload;

/// fill a ShortString with the (hexadecimal) chars of the input text/binary
function EscapeToShort(source: PAnsiChar; sourcelen: integer): ShortString; overload;

/// fill a ShortString with the (hexadecimal) chars of the input text/binary
function EscapeToShort(const source: RawByteString): ShortString; overload;


/// generate some pascal source code holding some data binary as constant
// - can store sensitive information (e.g. certificates) within the executable
// - generates a source code snippet of the following format:
// ! const
// !   // Comment
// !   ConstName: array[0..2] of byte = (
// !     $01, $02, $03);
procedure BinToSource(Dest: TTextWriter; const ConstName, Comment: RawUtf8;
  Data: pointer; Len: integer; PerLine: integer = 16); overload;

/// generate some pascal source code holding some data binary as constant
// - can store sensitive information (e.g. certificates) within the executable
// - generates a source code snippet of the following format:
// ! const
// !   // Comment
// !   ConstName: array[0..2] of byte = (
// !     $01, $02, $03);
function BinToSource(const ConstName, Comment: RawUtf8; Data: pointer;
  Len: integer; PerLine: integer = 16; const Suffix: RawUtf8 = ''): RawUtf8; overload;

/// generate some pascal source code holding some data binary as constant
function BinToSource(const ConstName, Comment: RawUtf8; const Data: RawByteString;
  PerLine: integer = 16; const Suffix: RawUtf8 = ''): RawUtf8; overload;

/// generate some 'xx:xx:xx:xx' output buffer with left and right margins
// - used e.g. by ParsedToText() to output X509 public key content in PeerInfo
function BinToHumanHex(Data: PByte; Len: integer; PerLine: integer = 16;
  LeftTab: integer = 0; SepChar: AnsiChar = ':'): RawUtf8; overload;

/// generate some 'xx:xx:xx:xx' output buffer with left and right margins
procedure BinToHumanHex(W: TTextWriter; Data: PByte; Len: integer;
  PerLine: integer = 16; LeftTab: integer = 0; SepChar: AnsiChar = ':'); overload;


{ *************************** TStreamRedirect and other Hash process }

/// compute the 32-bit default hash of a file content
// - you can specify your own hashing function if DefaultHasher is not what you expect
function HashFile(const FileName: TFileName; Hasher: THasher = nil): cardinal; overload;

/// compare two files by content, reading them by blocks
function SameFileContent(const One, Another: TFileName): boolean;

type
  /// prototype of a file hashing function, returning its hexadecimal hash
  // - match HashFileCrc32c() below, HashFileCrc32() in mormot.core.zip,
  // and HashFileMd5/HashFileSha* in mormot.crypt.secure functions signature
  THashFile = function(const FileName: TFileName): RawUtf8;

  TStreamRedirect = class;

  /// TStreamHasher.Write optional progression callback
  // - see Sender properties like Context/Size/PerSecond and ExpectedSize
  // (which may be 0 if the download size is unknown)
  // - see e.g. TStreamRedirect.ProgressStreamToConsole
  TOnStreamProgress = procedure(Sender: TStreamRedirect) of object;

  /// optional callback as used e.g. by THttpClientSocketWGet.OnStreamCreate
  TOnStreamCreate = function(const FileName: string; Mode: cardinal): TStream of object;

  PProgressInfo = ^TProgressInfo;

  /// callback definition to notify some TProgressInfo process
  // - see e.g. TStreamRedirect.ProgressInfoToConsole
  TOnInfoProgress = procedure(Sender: TObject; Info: PProgressInfo) of object;

  /// information about the progression of a process, e.g. for TStreamRedirect
  // - can also compute user-level text information from raw numbers
  {$ifdef USERECORDWITHMETHODS}
  TProgressInfo = record
  {$else}
  TProgressInfo = object
  {$endif USERECORDWITHMETHODS}
  private
    StartTix, ReportTix: Int64;
    ExpectedWrittenSize: Int64;
    ConsoleLen: integer;
    LastProgress: RawUtf8;
  public
    /// optional process context, e.g. a download URI, used for logging/progress
    Context: RawUtf8;
    /// number of bytes for the processed size
    CurrentSize: Int64;
    /// number of bytes for the final processed size
    // - may equal 0 if not known
    ExpectedSize: Int64;
    /// how many bytes have processed yet
    ProcessedSize: Int64;
    /// percentage of CurrentSize versus ExpectedSize
    // - equals 0 if ExpectedSize is 0
    Percent: integer;
    /// number of milliseconds elasped since process beginning
    Elapsed: Int64;
    /// number of milliseconds remaining for full process, as estimated
    // - equals 0 if ExpectedSize is 0
    // - is just an estimation based on the average PerSecond speed
    Remaining: Int64;
    /// number of bytes processed per second
    PerSecond: PtrInt;
    /// number of milliseconds between each DoReport notification
    // - default is 1000, i.e. to notify OnLog/OnProgress every second
    ReportDelay: Int64;
    /// can be assigned from TSynLog.DoLog class method for low-level logging
    // - at least at process startup and finish, and every second (ReportDelay)
    OnLog: TSynLogProc;
    /// can be assigned to a TOnInfoProgress callback for high-level logging
    // - at least at process startup and finish, and every second (ReportDelay)
    OnProgress: TOnInfoProgress;
    /// initialize the information, especially start the timing
    procedure Init;
    /// called during process to setup ExpectedSize/ExpectedWrittenSize fields
    procedure SetExpectedSize(SizeExpected, Position: Int64);
    /// retrieve the current status as simple text
    function GetProgress: RawUtf8;
    /// initialize the information for a new process
    // - once expected size and ident are set, caller should call DoAfter()
    procedure DoStart(Sender: TObject; SizeExpected: Int64; const Ident: string);
    /// can be called
    procedure DoAfter(Sender: TObject; ChunkSize: Int64);
    /// update the computed fields according to the curent state
    // - will be updated only every ReportDelay ms (default 1000 = every second)
    // - return false and compute nothing if ReportDelay has not been reached
    // - optionally call OnLog and OnProgress callbacks
    function DoReport(Sender: TObject; ReComputeElapsed: boolean): boolean;
  end;

  /// exception raised during TStreamRedirect processing
  EStreamRedirect = class(ESynException);

  /// an abstract pipeline stream able to redirect and hash read/written content
  // - can be used either Read() or Write() calls during its livetime
  // - hashing is performed on the fly during the Read/Write process
  // - it features also a callback to mark its progress
  // - can sleep during Read/Write to reach a LimitPerSecond average bandwidth
  TStreamRedirect = class(TStreamWithPosition)
  protected
    fRedirected: TStream;
    fInfo: TProgressInfo;
    fLastTix, fTimeOut: Int64;
    fLimitPerSecond: PtrInt;
    fOnStreamProgress: TOnStreamProgress;
    fTerminated: boolean;
    fMode: (mUnknown, mRead, mWrite);
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
    function GetProgress: RawUtf8;
    procedure DoReport(ReComputeElapsed: boolean);
    procedure DoHash(data: pointer; len: integer); virtual; // do nothing
    procedure SetExpectedSize(Value: Int64);
    procedure ReadWriteHash(const Buffer; Count: integer); virtual;
    procedure ReadWriteReport(const Caller: ShortString); virtual;
  public
    /// initialize the internal structure, and start the timing
    // - before calling Read/Write, you should set the Redirected property or
    // specify aRedirected here - which will be owned by this instance
    // - if aRead is true, ExpectedSize is set from aRedirected.Size
    constructor Create(aRedirected: TStream; aRead: boolean = false); reintroduce; virtual;
    /// release the associated Redirected stream
    destructor Destroy; override;
    /// can be used as TOnStreamProgress callback writing into the console
    class procedure ProgressStreamToConsole(Sender: TStreamRedirect);
    /// can be used as TOnInfoProgress callback writing into the console
    class procedure ProgressInfoToConsole(Sender: TObject; Info: PProgressInfo);
    /// notify a TOnStreamProgress callback that a process ended
    // - create a fake TStreamRedirect and call Ended with the supplied info
    class procedure NotifyEnded(
      const OnStream: TOnStreamProgress; const OnInfo: TOnInfoProgress;
      const Fmt: RawUtf8; const Args: array of const; Size, StartedMs: Int64);
    /// update the hash and redirect the data to the associated TStream
    // - also trigger OnProgress at least every second
    // - will raise an error if Write() (or Append) have been called before
    function Read(var Buffer; Count: Longint): Longint; override;
    /// update the hash and redirect the data to the associated TStream
    // - also trigger OnProgress at least every second
    // - will raise an error if Read() has been called before
    function Write(const Buffer; Count: Longint): Longint; override;
    /// update the hash of the existing Redirected stream content
    // - ready to Write() some new data after the existing
    procedure Append;
    /// notify end of process
    // - should be called explicitly when all Read()/Write() has been done
    procedure Ended;
    /// could be set from another thread to abort the streaming process
    // - will raise an exception at the next Read()/Write() call
    procedure Terminate;
    /// return the current state of the hash as lower hexadecimal
    // - by default, will return '' meaning that no hashing algorithm was set
    function GetHash: RawUtf8; virtual;
    /// current algorithm name as file/url extension, e.g. '.md5' or '.sha256'
    // - by default, will return '' meaning that no hashing algorithm was set
    class function GetHashFileExt: RawUtf8; virtual;
    /// current algorithm name, from GetHashFileExt, e.g. 'md5' or 'sha256'
    class function GetHashName: RawUtf8;
    /// apply the internal hash algorithm to the supplied file content
    // - could be used ahead of time to validate a cached file
    class function HashFile(const FileName: TFileName;
      const OnProgress: TOnStreamProgress = nil): RawUtf8;
    /// specify a TStream to which any Read()/Write() will be redirected
    // - this TStream instance will be owned by the TStreamRedirect
    property Redirected: TStream
      read fRedirected write fRedirected;
    /// you can specify a number of bytes for the final Redirected size
    // - will be used for the callback progress - could be left to 0 for Write()
    // if size is unknown
    property ExpectedSize: Int64
      read fInfo.ExpectedSize write SetExpectedSize;
    /// how many bytes have passed through Read() or Write()
    // - may not equal Size or Position after an Append - e.g. on resumed
    // download from partial file
    property ProcessedSize: Int64
      read fInfo.ProcessedSize;
    /// percentage of Size versus ExpectedSize
    // - equals 0 if ExpectedSize is 0
    property Percent: integer
      read fInfo.Percent;
    /// number of milliseconds elasped since beginning, as set by Read/Write
    property Elapsed: Int64
      read fInfo.Elapsed;
    /// number of milliseconds remaining for full process, as set by Read/Write
    // - equals 0 if ExpectedSize is 0
    // - is just an estimation based on the average PerSecond speed
    property Remaining: Int64
      read fInfo.Remaining;
    /// number of bytes processed per second, since initialization of this instance
    property PerSecond: PtrInt
      read fInfo.PerSecond;
    /// can limit the Read/Write bytes-per-second bandwidth used, if not 0
    // - sleep so that PerSecond will keep close to this LimitPerSecond value
    property LimitPerSecond: PtrInt
      read fLimitPerSecond write fLimitPerSecond;
    /// Read/Write will raise an exception if not finished after TimeOut milliseconds
    property TimeOut: Int64
      read fTimeOut write fTimeOut;
    /// optional process context, e.g. a download URI, used for logging/progress
    property Context: RawUtf8
      read fInfo.Context write fInfo.Context;
    /// number of milliseconds between each notification
    // - default is 1000, i.e. notify OnLog/OnProgress/OnInfoProgress every second
    property ReportDelay: Int64
      read fInfo.ReportDelay write fInfo.ReportDelay;
    /// can be assigned from TSynLog.DoLog class method for low-level logging
    property OnLog: TSynLogProc
      read fInfo.OnLog write fInfo.OnLog;
    /// optional TOnStreamProgress callback triggered during Read/Write
    // - at least at process startup and finish, and every second / ReportDelay
    property OnProgress: TOnStreamProgress
      read fOnStreamProgress write fOnStreamProgress;
    /// optional TOnInfoProgress callback triggered during Read/Write
    // - at least at process startup and finish, and every second / ReportDelay
    property OnInfoProgress: TOnInfoProgress
      read fInfo.OnProgress write fInfo.OnProgress;
  published
    /// the current progression as text, as returned by ProgressStreamToConsole
    property Progress: RawUtf8
      read GetProgress;
  end;

  /// meta-class of TStreamRedirect hierarchy
  TStreamRedirectClass = class of TStreamRedirect;

  /// TStreamRedirect with 32-bit THasher checksum
  TStreamRedirectHasher = class(TStreamRedirect)
  protected
    fHash: cardinal;
  public
    function GetHash: RawUtf8; override;
  end;

  /// TStreamRedirect with crc32c 32-bit checksum
  TStreamRedirectCrc32c = class(TStreamRedirectHasher)
  protected
    procedure DoHash(data: pointer; len: integer); override;
  public
    class function GetHashFileExt: RawUtf8; override;
  end;

  /// a fake TStream, which will just count the number of bytes written
  TFakeWriterStream = class(TStream)
  protected
    fWritten: Int64;
    {$ifdef FPC}
    function GetPosition: Int64; override;
    {$endif FPC}
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  TNestedStream = record
    Stream: TStream;
    Start, Stop: Int64;
  end;

  /// TStream allowing to read from some nested TStream instances
  TNestedStreamReader = class(TStreamWithPositionAndSize)
  protected
    fNested: array of TNestedStream;
    fContentRead: ^TNestedStream;
  public
    /// overriden method to call Flush on rewind, i.e. if position is set to 0
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    /// finalize the nested TStream instance
    destructor Destroy; override;
    /// append a nested TStream instance
    // - you could use a TFileStreamEx here for efficient chunked reading
    function NewStream(Stream: TStream): TStream;
    /// get the last TRawByteStringStream, or append a new one if needed
    function ForText: TRawByteStringStream;
    /// append some text or content to an internal TRawByteStringStream
    // - is the easy way to append some text or data to the internal buffers
    procedure Append(const Content: RawByteString);
    /// you should call this method before any Read() call
    // - is also called when you execute Seek(0, soBeginning)
    procedure Flush; virtual;
    /// will read up to Count bytes from the internal nested TStream
    function Read(var Buffer; Count: Longint): Longint; override;
    /// this TStream is read-only: calling this method will raise an exception
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  /// TStream with an internal memory buffer
  // - can be beneficial e.g. reading from a file by small chunks
  TBufferedStreamReader = class(TStreamWithPositionAndSize)
  protected
    fBuffer: RawByteString;
    fSource: TStream;
    fBufferPos: PAnsiChar;
    fBufferLeft: integer;
    fOwnStream: TStream;
  public
    /// initialize the source TStream and the internal buffer
    // - will also rewind the aSource position to its beginning, and retrieve
    // its size
    constructor Create(aSource: TStream;
      aBufSize: integer = 65536); reintroduce; overload;
    /// initialize a source file and the internal buffer
    constructor Create(const aSourceFileName: TFileName;
      aBufSize: integer = 65536); reintroduce; overload;
    /// finalize this instance and its buffer
    destructor Destroy; override;
    /// overriden method to flush buffer on rewind
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    /// will read up to Count bytes from the internal buffer or source TStream
    function Read(var Buffer; Count: Longint): Longint; override;
    /// this TStream is read-only: calling this method will raise an exception
    function Write(const Buffer; Count: Longint): Longint; override;
  end;


/// compute the crc32c checksum of a given file
// - this function maps the THashFile signature
function HashFileCrc32c(const FileName: TFileName): RawUtf8;

/// retrieve the memory buffer of a TCustomMemoryStream/TRawByteStringStream
// - returns nil if the instance is not of those classes
function GetStreamBuffer(S: TStream): pointer;

/// check if class is a TCustomMemoryStream/TRawByteStringStream
function IsStreamBuffer(S: TStream): boolean;


{ ************* Markup (e.g. HTML or Emoji) process }

type
  /// tune AddHtmlEscapeWiki/AddHtmlEscapeMarkdown wrapper functions process
  // - heHtmlEscape will escape any HTML special chars, e.g. & into &amp;
  // - heEmojiToUtf8 will convert any Emoji text into UTF-8 Unicode character,
  // recognizing e.g. :joy: or :) in the text
  TTextWriterHtmlEscape = set of (
    heHtmlEscape,
    heEmojiToUtf8);

/// convert some wiki-like text into proper HTML
// - convert all #13#10 into <p>...</p>, *..* into <em>..</em>, +..+ into
// <strong>..</strong>, `..` into <code>..</code>, and http://... as
// <a href=http://...>
// - escape any HTML special chars, and Emoji tags as specified with esc
procedure AddHtmlEscapeWiki(W: TTextWriter; P: PUtf8Char;
  esc: TTextWriterHtmlEscape = [heHtmlEscape, heEmojiToUtf8]);

/// convert minimal Markdown text into proper HTML
// - see https://enterprise.github.com/downloads/en/markdown-cheatsheet.pdf
// - convert all #13#10 into <p>...</p>, *..* into <em>..</em>, **..** into
// <strong>..</strong>, `...` into <code>...</code>, backslash espaces \\
// \* \_ and so on, [title](http://...) and detect plain http:// as
// <a href=...>
// - create unordered lists from trailing * + - chars, blockquotes from
// trailing > char, and code line from 4 initial spaces
// - as with default Markdown, won't escape HTML special chars (i.e. you can
// write plain HTML in the supplied text) unless esc is set otherwise
// - only inline-style links and images are supported yet (not reference-style);
// tables aren't supported either
procedure AddHtmlEscapeMarkdown(W: TTextWriter; P: PUtf8Char;
  esc: TTextWriterHtmlEscape = [heEmojiToUtf8]);

/// escape some wiki-marked text into HTML
// - just a wrapper around AddHtmlEscapeWiki() process
function HtmlEscapeWiki(const wiki: RawUtf8;
  esc: TTextWriterHtmlEscape = [heHtmlEscape, heEmojiToUtf8]): RawUtf8;

/// escape some Markdown-marked text into HTML
// - just a wrapper around AddHtmlEscapeMarkdown() process
function HtmlEscapeMarkdown(const md: RawUtf8;
  esc: TTextWriterHtmlEscape = [heEmojiToUtf8]): RawUtf8;

type
  /// map the first Unicode page of Emojis, from U+1F600 to U+1F64F
  // - naming comes from github/Markdown :identifiers:
  TEmoji = (
    eNone,
    eGrinning,
    eGrin,
    eJoy,
    eSmiley,
    eSmile,
    eSweat_smile,
    eLaughing,
    eInnocent,
    eSmiling_imp,
    eWink,
    eBlush,
    eYum,
    eRelieved,
    eHeart_eyes,
    eSunglasses,
    eSmirk,
    eNeutral_face,
    eExpressionless,
    eUnamused,
    eSweat,
    ePensive,
    eConfused,
    eConfounded,
    eKissing,
    eKissing_heart,
    eKissing_smiling_eyes,
    eKissing_closed_eyes,
    eStuck_out_tongue,
    eStuck_out_tongue_winking_eye,
    eStuck_out_tongue_closed_eyes,
    eDisappointed,
    eWorried,
    eAngry,
    ePout,
    eCry,
    ePersevere,
    eTriumph,
    eDisappointed_relieved,
    eFrowning,
    eAnguished,
    eFearful,
    eWeary,
    eSleepy,
    eTired_face,
    eGrimacing,
    eSob,
    eOpen_mouth,
    eHushed,
    eCold_sweat,
    eScream,
    eAstonished,
    eFlushed,
    eSleeping,
    eDizzy_face,
    eNo_mouth,
    eMask,
    eSmile_cat,
    eJoy_cat,
    eSmiley_cat,
    eHeart_eyes_cat,
    eSmirk_cat,
    eKissing_cat,
    ePouting_cat,
    eCrying_cat_face,
    eScream_cat,
    eSlightly_frowning_face,
    eSlightly_smiling_face,
    eUpside_down_face,
    eRoll_eyes,
    eNo_good,
    oOk_woman,
    eBow,
    eSee_no_evil,
    eHear_no_evil,
    eSpeak_no_evil,
    eRaising_hand,
    eRaised_hands,
    eFrowning_woman,
    ePerson_with_pouting_face,
    ePray);

var
  /// github/Markdown compatible text of Emojis
  // - e.g. 'grinning' or 'person_with_pouting_face'
  EMOJI_TEXT: array[TEmoji] of RawUtf8;

  /// github/Markdown compatible tag of Emojis, including trailing and ending :
  // - e.g. ':grinning:' or ':person_with_pouting_face:'
  EMOJI_TAG: array[TEmoji] of RawUtf8;

  /// the Unicode character matching a given Emoji, after UTF-8 encoding
  EMOJI_UTF8: array[TEmoji] of RawUtf8;

  /// low-level access to TEmoji RTTI - used when inlining EmojiFromText()
  EMOJI_RTTI: PShortString;

  /// to recognize simple :) :( :| :/ :D :o :p :s characters as smilleys
  EMOJI_AFTERDOTS: array['('..'|'] of TEmoji;

/// recognize github/Markdown compatible text of Emojis
// - for instance 'sunglasses' text buffer will return eSunglasses
// - returns eNone if no case-insensitive match was found
function EmojiFromText(P: PUtf8Char; len: PtrInt): TEmoji;
  {$ifdef HASINLINE}inline;{$endif}

/// low-level parser of github/Markdown compatible text of Emojis
// - supplied P^ should point to ':'
// - will append the recognized UTF-8 Emoji if P contains e.g. :joy: or :)
// - will append ':' if no Emoji text is recognized, and return eNone
// - will try both EMOJI_AFTERDOTS[] and EMOJI_RTTI[] reference set
// - if W is nil, won't append anything, but just return the recognized TEmoji
function EmojiParseDots(var P: PUtf8Char; W: TTextWriter = nil): TEmoji;

/// low-level conversion of UTF-8 Emoji sequences into github/Markdown :identifiers:
procedure EmojiToDots(P: PUtf8Char; W: TTextWriter); overload;

/// conversion of UTF-8 Emoji sequences into github/Markdown :identifiers:
function EmojiToDots(const text: RawUtf8): RawUtf8; overload;

/// low-level conversion of github/Markdown :identifiers: into UTF-8 Emoji sequences
procedure EmojiFromDots(P: PUtf8Char; W: TTextWriter); overload;

/// conversion of github/Markdown :identifiers: into UTF-8 Emoji sequences
function EmojiFromDots(const text: RawUtf8): RawUtf8; overload;


{ ************ RawByteString Buffers Aggregation via TRawByteStringGroup }

type
  /// item as stored in a TRawByteStringGroup instance
  TRawByteStringGroupValue = record
    Position: integer;
    Value: RawByteString;
  end;

  PRawByteStringGroupValue = ^TRawByteStringGroupValue;

  /// items as stored in a TRawByteStringGroup instance
  TRawByteStringGroupValueDynArray = array of TRawByteStringGroupValue;

  /// store several RawByteString content with optional concatenation
  {$ifdef USERECORDWITHMETHODS}
  TRawByteStringGroup = record
  {$else}
  TRawByteStringGroup = object
  {$endif USERECORDWITHMETHODS}
  public
    /// actual list storing the data
    Values: TRawByteStringGroupValueDynArray;
    /// how many items are currently stored in Values[]
    Count: integer;
    /// the current size of data stored in Values[]
    Position: integer;
    /// naive but efficient cache for Find()
    LastFind: integer;
    /// add a new item to Values[]
    procedure Add(const aItem: RawByteString); overload;
    /// add a new item to Values[]
    procedure Add(aItem: pointer; aItemLen: integer); overload;
    /// add another TRawByteStringGroup to Values[]
    procedure Add(const aAnother: TRawByteStringGroup); overload;
    /// low-level method to abort the latest Add() call
    // - warning: will work only once, if an Add() has actually been just called:
    // otherwise, the behavior is unexpected, and may wrongly truncate data
    procedure RemoveLastAdd;
    /// compare two TRawByteStringGroup instance stored text
    function Equals(const aAnother: TRawByteStringGroup): boolean;
    /// clear any stored information
    procedure Clear;
    /// append stored information into another RawByteString, and clear content
    procedure AppendTextAndClear(var aDest: RawByteString);
    // compact the Values[] array into a single item
    // - is also used by AsText to compute a single RawByteString
    procedure Compact;
    /// return all content as a single RawByteString
    // - will also compact the Values[] array into a single item (which is returned)
    function AsText: RawByteString;
    /// return all content as a single TByteDynArray
    function AsBytes: TByteDynArray;
    /// save all content into a TJsonWriter instance
    procedure Write(W: TTextWriter; Escape: TTextWriterKind = twJsonEscape); overload;
    /// save all content into a TBufferWriter instance
    procedure WriteBinary(W: TBufferWriter); overload;
    /// save all content as a string into a TBufferWriter instance
    // - storing the length as WriteVarUInt32() prefix
    procedure WriteString(W: TBufferWriter);
    /// add another TRawByteStringGroup previously serialized via WriteString()
    procedure AddFromReader(var aReader: TFastReader);
    /// returns a pointer to Values[] containing a given position
    // - returns nil if not found
    function Find(aPosition: integer): PRawByteStringGroupValue; overload;
    /// returns a pointer to Values[].Value containing a given position and length
    // - returns nil if not found
    function Find(aPosition, aLength: integer): pointer; overload;
    /// returns the text at a given position in Values[]
    // - text should be in a single Values[] entry
    procedure FindAsText(aPosition, aLength: integer; out aText: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the text at a given position in Values[]
    // - text should be in a single Values[] entry
    function FindAsText(aPosition, aLength: integer): RawByteString; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the text at a given position in Values[] via RawUtf8ToVariant()
    // - text should be in a single Values[] entry
    // - explicitly returns null if the supplied text was not found
    procedure FindAsVariant(aPosition, aLength: integer; out aDest: variant);
      {$ifdef HASINLINE}inline;{$endif}
    /// append the text at a given position in Values[], JSON escaped by default
    // - text should be in a single Values[] entry
    procedure FindWrite(aPosition, aLength: integer; W: TTextWriter;
      Escape: TTextWriterKind = twJsonEscape; TrailingCharsToIgnore: integer = 0);
      {$ifdef HASINLINE}inline;{$endif}
    /// append the blob at a given position in Values[], Base64 encoded
    // - text should be in a single Values[] entry
    procedure FindWriteBase64(aPosition, aLength: integer; W: TTextWriter;
      withMagic: boolean);
      {$ifdef HASINLINE}inline;{$endif}
    /// copy the text at a given position in Values[]
    // - text should be in a single Values[] entry
    procedure FindMove(aPosition, aLength: integer; aDest: pointer);
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// pointer reference to a TRawByteStringGroup
  PRawByteStringGroup = ^TRawByteStringGroup;

  /// thread-safe reusable set of constant RawByteString instances
  // - use internally its own TLockedList O(1) efficient structure
  // - warning: any call to New() should manually be followed by one Release()
  TRawByteStringCached = class
  protected
    fLength: integer;
    fOne: TLockedList;
  public
    /// initialize the internal cache for a given length
    constructor Create(aLength: integer);
    /// return a new RawByteString of a given length, with refcount = -2
    // - may be allocated or returned from its internal cache
    procedure New(var aDest: RawByteString;
      aCodePage: integer = CP_RAWBYTESTRING); overload;
    /// return a new RawUtf8 of a given length, with refcount = -2
    procedure New(var aDest: RawUtf8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// return a new RawUtf8 of a given length into a pointer, with refcount = -2
    procedure NewUtf8(var aDest: pointer);
      {$ifdef HASINLINE}inline;{$endif}
    /// put back a RawByteString acquired from New() into the internal cache
    procedure Release(var aDest: RawByteString); overload;
    /// put back a RawUtf8 acquired from New() into the internal cache
    procedure Release(var aDest: RawUtf8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// put back a RawByteString acquired from NewUtf8() into the internal cache
    procedure Release(var aDest: pointer); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// release the RawByteString instances in the cache bin
    // - keep any existing New() instances intact
    // - returns how many memory has been released to the heap
    function Clean: PtrInt;
    /// release all cached instances
    destructor Destroy; override;
    /// how many New() calls are currently active
    property Count: integer
      read fOne.Count;
    /// the length() of RawByteString returned by New()
    property Length: integer
      read fLength;
  end;

  /// store one RawByteString content with an associated length
  // - to be used e.g. as a convenient reusable memory buffer
  {$ifdef USERECORDWITHMETHODS}
  TRawByteStringBuffer = record
  {$else}
  TRawByteStringBuffer = object
  {$endif USERECORDWITHMETHODS}
  private
    fBuffer: RawUtf8; /// actual storage, with length(fBuffer) as Capacity
    fLen: PtrInt;
    procedure RawAppend(P: pointer; PLen: PtrInt);
      {$ifdef HASINLINE}inline;{$endif}
    procedure RawRealloc(needed: PtrInt);
  public
    /// set Len to 0, but doesn't clear/free the Buffer itself
    procedure Reset;
      {$ifdef HASINLINE}inline;{$endif}
    /// release/free the internal Buffer storage
    procedure Clear;
      {$ifdef HASINLINE}inline;{$endif}
    /// a convenient wrapper to pointer(fBuffer) for direct Buffer/Len use
    function Buffer: pointer;
      {$ifdef HASINLINE}inline;{$endif}
    /// how many bytes are currently allocated in the Buffer
    function Capacity: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// add some UTF-8 buffer content to the Buffer, resizing it if needed
    procedure Append(P: pointer; PLen: PtrInt); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// add some UTF-8 string content to the Buffer, resizing it if needed
    procedure Append(const Text: RawUtf8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// add some number as text content to the Buffer, resizing it if needed
    procedure Append(Value: QWord); overload;
    /// add some UTF-8 shortstring content to the Buffer, resizing it if needed
    procedure AppendShort(const Text: ShortString);
      {$ifdef HASINLINE}inline;{$endif}
    /// add some UTF-8 string(s) content to the Buffer, resizing it if needed
    procedure Append(const Text: array of RawUtf8); overload;
    /// just after Append/AppendShort, append a #13#10 end of line
    procedure AppendCRLF;
      {$ifdef HASINLINE}inline;{$endif}
    /// just after Append/AppendShort, append one single character
    procedure Append(Ch: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// add some UTF-8 buffer content to the Buffer, without resizing it
    function TryAppend(P: pointer; PLen: PtrInt): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// ensure the internal Buffer has at least MaxSize bytes
    // - also reset the internal Len to 0
    procedure Reserve(MaxSize: PtrInt); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// use a specified string buffer as start
    procedure Reserve(const WorkingBuffer: RawByteString); overload;
    /// similar to delete(fBuffer, 1, FirstBytes)
    procedure Remove(FirstBytes: PtrInt);
    /// move up to Count bytes from the internal Buffer into another place
    // - returns how many bytes were available to be copied into Dest^
    // - then remove the copied bytes from the internal Buffer/Len storage
    function Extract(Dest: pointer; Count: PtrInt): PtrInt;
    /// move up to Count bytes from the internal Buffer into another place
    // - returns how many bytes were available to be copied into Dest^
    // - don't move any byte, but just update the given Pos index
    function ExtractAt(var Dest: PAnsiChar; var Count: PtrInt;
      var Pos: PtrInt): PtrInt;
    /// similar to insert(P/PLen, fBuffer, Position + 1)
    // - could optionally include a #13#10 pattern between the two
    procedure Insert(P: pointer; PLen: PtrInt; Position: PtrInt = 0;
      CRLF: boolean = false);
    /// retrieve the current Buffer/Len content as RawUtf8 text
    // - with some optional overhead for faster reallocmem at concatenation
    // - won't force Len to 0: caller should call Reset if done with it
    // - UseMainBuffer=true will return a copy of fBuffer into Text
    procedure AsText(out Text: RawUtf8; Overhead: PtrInt = 0;
      UseMainBuffer: boolean = false);
    /// how many bytes are currently used in the Buffer
    property Len: PtrInt
      read fLen write fLen;
  end;

  /// pointer reference to a TRawByteStringBuffer
  PRawByteStringBuffer = ^TRawByteStringBuffer;


implementation


{ ************ Variable Length Integer Encoding / Decoding }

function ToVarInt32(Value: PtrInt; Dest: PByte): PByte;
begin
  // 0=0,1=1,2=-1,3=2,4=-2...
  if Value < 0 then
    // -1->2, -2->4..
    Value := (-Value) shl 1
  else if Value > 0 then
    // 1->1, 2->3..
    Value := (Value shl 1) - 1;
    // 0->0
  result := ToVarUInt32(Value, Dest);
end;

function ToVarUInt32(Value: cardinal; Dest: PByte): PByte;
label
  _1, _2, _3; // ugly but fast
begin
  if Value > $7f then
  begin
    if Value < $80 shl 7 then
      goto _1
    else if Value < $80 shl 14 then
      goto _2
    else if Value < $80 shl 21 then
      goto _3;
    Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
_3: Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
_2: Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
_1: Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
  end;
  Dest^ := Value;
  inc(Dest);
  result := Dest;
end;

function ToVarUInt32Length(Value: PtrUInt): PtrUInt;
begin
  if Value <= $7f then
    result := 1
  else if Value < $80 shl 7 then
    result := 2
  else if Value < $80 shl 14 then
    result := 3
  else if Value < $80 shl 21 then
    result := 4
  else
    result := 5;
end;

function ToVarUInt32LengthWithData(Value: PtrUInt): PtrUInt;
begin
  if Value <= $7f then
    result := Value + 1
  else if Value < $80 shl 7 then
    result := Value + 2
  else if Value < $80 shl 14 then
    result := Value + 3
  else if Value < $80 shl 21 then
    result := Value + 4
  else
    result := Value + 5;
end;

function FromVarUInt32(var Source: PByte): cardinal;
begin
  result := Source^;
  inc(Source);
  if result > $7f then
    result := (result and $7F) or FromVarUInt32Up128(Source);
end;

function FromVarUInt32Big(var Source: PByte): cardinal;
var
  c: cardinal;
  p: PByte;
begin
  p := Source;
  result := p^;
  inc(p);
  if result > $7f then
  begin
    // Values between 128 and 16256
    c := p^;
    c := c shl 7;
    result := result and $7F or c;
    inc(p);
    if c > $7f shl 7 then
    begin
      // Values between 16257 and 2080768
      c := p^;
      c := c shl 14;
      inc(p);
      result := result and $3FFF or c;
      if c > $7f shl 14 then
      begin
        // Values between 2080769 and 266338304
        c := p^;
        c := c shl 21;
        inc(p);
        result := result and $1FFFFF or c;
        if c > $7f shl 21 then
        begin
          // Values above 266338304
          c := p^;
          c := c shl 28;
          inc(p);
          result := result and $FFFFFFF or c;
        end;
      end;
    end;
  end;
  Source := p;
end;

function FromVarUInt32Up128(var Source: PByte): cardinal;
var
  c: cardinal;
  p: PByte;
begin
  // Values above 127
  p := Source;
  result := p^ shl 7;
  inc(p);
  if result > $7f shl 7 then
  begin
    // Values above 16256
    c := p^;
    c := c shl 14;
    inc(p);
    result := result and $3FFF or c;
    if c > $7f shl 14 then
    begin
      // Values above 2080768
      c := p^;
      c := c shl 21;
      inc(p);
      result := result and $1FFFFF or c;
      if c > $7f shl 21 then
      begin
        // Values above 266338304
        c := p^;
        c := c shl 28;
        inc(p);
        result := result and $FFFFFFF or c;
      end;
    end;
  end;
  Source := p;
end;

function FromVarUInt32(var Source: PByte; SourceMax: PByte;
  out Value: cardinal): boolean;
begin
  if SourceMax = nil then
  begin
    Value := FromVarUInt32(Source);
    result := true;
  end
  else
  begin
    Source := FromVarUInt32Safe(Source, SourceMax, Value);
    result := Source <> nil;
  end;
end;

function FromVarUInt32Safe(Source, SourceMax: PByte; out Value: cardinal): PByte;
var
  c: cardinal;
begin
  result := nil; // error
  if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
    exit;
  c := Source^;
  inc(Source);
  Value := c;
  if c > $7f then
  begin
    // Values between 128 and 16256
    if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
      exit;
    c := Source^;
    c := c shl 7;
    Value := Value and $7F or c;
    inc(Source);
    if c > $7f shl 7 then
    begin
      // Values between 16257 and 2080768
      if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
        exit;
      c := Source^;
      c := c shl 14;
      inc(Source);
      Value := Value and $3FFF or c;
      if c > $7f shl 14 then
      begin
        // Values between 2080769 and 266338304
        if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
          exit;
        c := Source^;
        c := c shl 21;
        inc(Source);
        Value := Value and $1FFFFF or c;
        if c > $7f shl 21 then
        begin
          // Values above 266338304
          if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
            exit;
          c := Source^;
          c := c shl 28;
          inc(Source);
          Value := Value and $FFFFFFF or c;
        end;
      end;
    end;
  end;
  result := Source; // safely decoded
end;

function FromVarInt32(var Source: PByte): integer;
var
  c: cardinal;
  p: PByte;
begin
  // fast stand-alone function with no FromVarUInt32 call
  p := Source;
  result := p^;
  inc(p);
  if result > $7f then
  begin
    c := p^;
    c := c shl 7;
    result := result and $7F or integer(c);
    inc(p);
    if c > $7f shl 7 then
    begin
      c := p^;
      c := c shl 14;
      inc(p);
      result := result and $3FFF or integer(c);
      if c > $7f shl 14 then
      begin
        c := p^;
        c := c shl 21;
        inc(p);
        result := result and $1FFFFF or integer(c);
        if c > $7f shl 21 then
        begin
          c := p^;
          c := c shl 28;
          inc(p);
          result := result and $FFFFFFF or integer(c);
        end;
      end;
    end;
  end;
  Source := p;
  // 0=0,1=1,2=-1,3=2,4=-2...
  if result and 1 <> 0 then
    // 1->1, 3->2..
    result := result shr 1 + 1
  else
    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

function FromVarUInt32High(var Source: PByte): cardinal;
var
  c: cardinal;
begin
  result := Source^;
  inc(Source);
  c := Source^ shl 7;
  inc(Source);
  result := result and $7F or c;
  if c <= $7f shl 7 then
    exit;
  c := Source^ shl 14;
  inc(Source);
  result := result and $3FFF or c;
  if c <= $7f shl 14 then
    exit;
  c := Source^ shl 21;
  inc(Source);
  result := result and $1FFFFF or c;
  if c <= $7f shl 21 then
    exit;
  c := Source^ shl 28;
  inc(Source);
  result := result and $FFFFFFF or c;
end;

function ToVarInt64(Value: Int64; Dest: PByte): PByte;
begin
  // 0=0,1=1,2=-1,3=2,4=-2...
{$ifdef CPU32}
  if Value <= 0 then
    // 0->0, -1->2, -2->4..
    result := ToVarUInt64((-Value) shl 1, Dest)
  else
     // 1->1, 2->3..
    result := ToVarUInt64((Value shl 1) - 1, Dest);
{$else}
  if Value <= 0 then
    // 0->0, -1->2, -2->4..
    Value := (-Value) shl 1
  else
    // 1->1, 2->3..
    Value := (Value shl 1) - 1;
  result := ToVarUInt64(Value, Dest);
{$endif CPU32}
end;

function ToVarUInt64(Value: QWord; Dest: PByte): PByte;
var
  c: cardinal;
label
  _1, _2, _4; // ugly but fast
begin
  repeat
    c := Value;
    {$ifdef CPU32}
    if PCardinalArray(@Value)^[1] = 0 then
    {$else}
    if Value shr 32 = 0 then
    {$endif CPU32}
      begin
        if c > $7f then
        begin
          if c < $80 shl 7 then
            goto _1
          else if c < $80 shl 14 then
            goto _2
          else if c >= $80 shl 21 then
            goto _4;
          Dest^ := (c and $7F) or $80;
          c := c shr 7;
          inc(Dest);
_2:       Dest^ := (c and $7F) or $80;
          c := c shr 7;
          inc(Dest);
_1:       Dest^ := (c and $7F) or $80;
          c := c shr 7;
          inc(Dest);
        end;
        Dest^ := c;
        inc(Dest);
        result := Dest;
        exit;
      end;
_4: PCardinal(Dest)^ := (c and $7F) or (((c shr 7) and $7F) shl 8) or
      (((c shr 14) and $7F) shl 16) or (((c shr 21) and $7F) shl 24) or $80808080;
    inc(Dest, 4);
    Value := Value shr 28;
  until false;
end;

function FromVarUInt64(var Source: PByte): QWord;
var
  c, n: PtrUInt;
  p: PByte;
begin
  p := Source;
  {$ifdef CPU64}
  result := p^;
  if result > $7f then
  begin
    result := result and $7F;
  {$else}
  if p^ > $7f then
  begin
    result := PtrUInt(p^) and $7F;
  {$endif CPU64}
    n := 0;
    inc(p);
    repeat
      c := p^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (QWord(c and $7f) shl n);
      inc(p);
    until false;
    result := result or (QWord(c) shl n);
  end
  {$ifdef CPU32}
  else
    result := p^
  {$endif CPU32};
  inc(p);
  Source := p;
end;

function FromVarUInt64Safe(Source, SourceMax: PByte; out Value: QWord): PByte;
var
  c, n: PtrUInt;
begin
  result := nil; // error
  if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
    exit;
  c := Source^;
  inc(Source);
  if c > $7f then
  begin
    Value := c and $7F;
    n := 7;
    repeat
      if PAnsiChar(Source) >= PAnsiChar(SourceMax) then
        exit;
      c := Source^;
      inc(Source);
      if c <= $7f then
        break;
      c := c and $7f;
      Value := Value or (QWord(c) shl n);
      inc(n, 7);
    until false;
    Value := Value or (QWord(c) shl n);
  end
  else
    Value := c;
  result := Source; // safely decoded
end;

function FromVarUInt64(var Source: PByte; SourceMax: PByte; out Value: QWord): boolean;
begin
  if SourceMax = nil then
  begin
    Value := FromVarUInt64(Source);
    result := true;
  end
  else
  begin
    Source := FromVarUInt64Safe(Source, SourceMax, Value);
    result := Source <> nil;
  end;
end;

function FromVarInt64(var Source: PByte): Int64;
var
  c, n: PtrUInt;
begin
  // 0=0,1=1,2=-1,3=2,4=-2...
{$ifdef CPU64}
  result := Source^;
  if result > $7f then
  begin
    result := result and $7F;
    n := 0;
    inc(Source);
    repeat
      c := Source^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (Int64(c and $7f) shl n);
      inc(Source);
    until false;
    result := result or (Int64(c) shl n);
  end;
  if result and 1 <> 0 then
    // 1->1, 3->2..
    result := result shr 1 + 1
  else
    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
{$else}
  c := Source^;
  if c > $7f then
  begin
    result := c and $7F;
    n := 0;
    inc(Source);
    repeat
      c := Source^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (Int64(c and $7f) shl n);
      inc(Source);
    until false;
    result := result or (Int64(c) shl n);
    if PCardinal(@result)^ and 1 <> 0 then
      // 1->1, 3->2..
      result := result shr 1 + 1
    else
      // 0->0, 2->-1, 4->-2..
      result := -(result shr 1);
  end
  else
  begin
    if c = 0 then
      result := 0
    else if c and 1 = 0 then
      // 0->0, 2->-1, 4->-2..
      result := -Int64(c shr 1)
    else
      // 1->1, 3->2..
      result := (c shr 1) + 1;
  end;
{$endif CPU64}
  inc(Source);
end;

function FromVarInt64Value(Source: PByte): Int64;
var
  c, n: PtrUInt;
begin
// 0=0,1=1,2=-1,3=2,4=-2...
  c := Source^;
  if c > $7f then
  begin
    result := c and $7F;
    n := 0;
    inc(Source);
    repeat
      c := Source^;
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (Int64(c and $7f) shl n);
      inc(Source);
    until false;
    result := result or (Int64(c) shl n);
    {$ifdef CPU64}
    if result and 1 <> 0 then
    {$else}
    if PCardinal(@result)^ and 1 <> 0 then
    {$endif CPU64}
      // 1->1, 3->2..
      result := result shr 1 + 1
    else
      // 0->0, 2->-1, 4->-2..
      result := -Int64(result shr 1);
  end
  else if c = 0 then
    result := 0
  else if c and 1 = 0 then
    // 0->0, 2->-1, 4->-2..
    result := -Int64(c shr 1)
  else
    // 1->1, 3->2..
    result := (c shr 1) + 1;
end;

function GotoNextVarInt(Source: PByte): pointer;
begin
  if Source <> nil then
  begin
    if Source^ > $7f then
      repeat
        inc(Source)
      until Source^ <= $7f;
    inc(Source);
  end;
  result := Source;
end;


function ToVarString(const Value: RawUtf8; Dest: PByte): PByte;
var
  Len: integer;
begin
  Len := Length(Value);
  Dest := ToVarUInt32(Len, Dest);
  if Len > 0 then
  begin
    MoveFast(pointer(Value)^, Dest^, Len);
    result := pointer(PAnsiChar(Dest) + Len);
  end
  else
    result := Dest;
end;

function GotoNextVarString(Source: PByte): pointer;
begin
  result := Pointer(PtrUInt(Source) + FromVarUInt32(Source));
end;

function FromVarString(var Source: PByte): RawUtf8;
begin
  FromVarString(Source, result{%H-});
end;

procedure FromVarString(var Source: PByte; var Value: RawUtf8);
var
  len: PtrUInt;
begin
  len := FromVarUInt32(Source);
  FastSetString(Value, Source, len);
  inc(Source, len);
end;

function FromVarString(var Source: PByte; SourceMax: PByte): RawUtf8;
var
  len: cardinal;
begin
  Source := FromVarUInt32Safe(Source, SourceMax, len);
  if (Source = nil) or
      (PAnsiChar(Source) + len > PAnsiChar(SourceMax)) then
    len := 0;
  FastSetString(result{%H-}, Source, len);
  inc(Source, len);
end;

procedure FromVarString(var Source: PByte; var Value: TSynTempBuffer);
var
  len: integer;
begin
  len := FromVarUInt32(Source);
  Value.Init(Source, len);
  PByteArray(Value.buf)[len] := 0; // include trailing #0
  inc(Source, len);
end;

function FromVarString(var Source: PByte; SourceMax: PByte;
  var Value: TSynTempBuffer): boolean;
var
  len: cardinal;
begin
  if SourceMax = nil then
    len := FromVarUInt32(Source)
  else
  begin
    Source := FromVarUInt32Safe(Source, SourceMax, len);
    if (Source = nil) or
       (PAnsiChar(Source) + len > PAnsiChar(SourceMax)) then
    begin
      result := false;
      exit;
    end;
  end;
  Value.Init(Source, len);
  PByteArray(Value.buf)[len] := 0; // include trailing #0
  inc(Source, len);
  result := true;
end;

procedure FromVarString(var Source: PByte; var Value: RawByteString;
  CodePage: integer);
var
  Len: PtrUInt;
begin
  Len := FromVarUInt32(Source);
  FastSetStringCP(Value, Source, Len, CodePage);
  inc(Source, Len);
end;

function FromVarString(var Source: PByte; SourceMax: PByte;
  var Value: RawByteString; CodePage: integer): boolean;
var
  len: cardinal;
begin
  if SourceMax = nil then
    len := FromVarUInt32(Source)
  else
  begin
    Source := FromVarUInt32Safe(Source, SourceMax, len);
    if (Source = nil) or
       (PAnsiChar(Source) + len > PAnsiChar(SourceMax)) then
    begin
      result := false;
      exit;
    end;
  end;
  FastSetStringCP(Value, Source, len, CodePage);
  inc(Source, len);
  result := true;
end;

function FromVarBlob(Data: PByte): TValueResult;
begin
  result.Len := FromVarUInt32(Data);
  result.Ptr := pointer(Data);
end;


{ ****************** TFastReader / TBufferWriter Binary Streams }

{ TFastReader }

procedure TFastReader.Init(Buffer: pointer; Len: PtrInt);
begin
  P := Buffer;
  Last := PAnsiChar(Buffer) + Len;
  OnErrorOverflow := nil;
  OnErrorData := nil;
  CustomVariants := nil;
end;

procedure TFastReader.Init(const Buffer: RawByteString);
begin
  Init(pointer(Buffer), length(Buffer));
end;

procedure TFastReader.ErrorOverflow;
begin
  if Assigned(OnErrorOverflow) then
    OnErrorOverflow
  else
    raise EFastReader.Create('Reached End of Input');
end;

procedure TFastReader.ErrorData(const fmt: RawUtf8; const args: array of const);
begin
  if Assigned(OnErrorData) then
    OnErrorData(fmt, args)
  else
    raise EFastReader.CreateUtf8('Incorrect Data: ' + fmt, args);
end;

procedure TFastReader.ErrorData(const msg: shortstring);
begin
  ErrorData('%', [msg]);
end;

function TFastReader.EOF: boolean;
begin
  result := P >= Last;
end;

function TFastReader.RemainingLength: PtrUInt;
begin
  result := PtrUInt(Last) - PtrUInt(P);
end;

function TFastReader.NextByte: byte;
begin
  if P >= Last then
    ErrorOverflow;
  result := ord(P^);
  inc(P);
end;

function TFastReader.NextByteSafe(dest: pointer): boolean;
begin
  if P >= Last then
    result := false
  else
  begin
    PAnsiChar(dest)^ := P^;
    inc(P);
    result := true;
  end;
end;

function TFastReader.Next2: cardinal;
begin
  if P + 1 >= Last then
    ErrorOverflow;
  result := PWord(P)^;
  inc(P, 2);
end;

function TFastReader.Next2BigEndian: cardinal;
begin
  if P + 1 >= Last then
    ErrorOverflow;
  result := swap(PWord(P)^);
  inc(P, 2);
end;

function TFastReader.Next4: cardinal;
begin
  if P + 3 >= Last then
    ErrorOverflow;
  result := PCardinal(P)^;
  inc(P, 4);
end;

function TFastReader.Next8: Qword;
begin
  if P + 7 >= Last then
    ErrorOverflow;
  result := PQWord(P)^;
  inc(P, 8);
end;

function TFastReader.NextByteEquals(Value: byte): boolean;
begin
  if P >= Last then
    ErrorOverflow;
  if ord(P^) = Value then
  begin
    inc(P);
    result := true;
  end
  else
    result := false;
end;

function TFastReader.Next(DataLen: PtrInt): pointer;
begin
  if P + DataLen > Last then
    ErrorOverflow;
  result := P;
  inc(P, DataLen);
end;

function TFastReader.NextSafe(out Data: Pointer; DataLen: PtrInt): boolean;
begin
  if P + DataLen > Last then
    result := false
  else
  begin
    Data := P;
    inc(P, DataLen);
    result := true;
  end;
end;

procedure TFastReader.Copy(Dest: Pointer; DataLen: PtrInt);
begin
  if P + DataLen > Last then
    ErrorOverflow;
  MoveFast(P^, Dest^, DataLen);
  inc(P, DataLen);
end;

function TFastReader.CopySafe(Dest: Pointer; DataLen: PtrInt): boolean;
begin
  if P + DataLen > Last then
    result := false
  else
  begin
    MoveFast(P^, Dest^, DataLen);
    inc(P, DataLen);
    result := true;
  end;
end;

function TFastReader.VarInt32: integer;
begin
  result := VarUInt32;
  if result and 1 <> 0 then
    // 1->1, 3->2..
    result := result shr 1 + 1
  else    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

function TFastReader.VarInt64: Int64;
begin
  result := VarUInt64;
  if result and 1 <> 0 then
    // 1->1, 3->2..
    result := result shr 1 + 1
  else    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

{$ifdef CPUX86} // not enough CPU registers

function TFastReader.VarUInt32: cardinal;
var
  c: cardinal;
label
  e;
begin
  if P >= Last then
    goto e;
  result := ord(P^);
  inc(P);
  if result <= $7f then
    exit;
  if P >= Last then
    goto e;
  c := ord(P^) shl 7;
  inc(P);
  result := result and $7F or c;
  if c <= $7f shl 7 then
    exit; // Values between 128 and 16256
  if P >= Last then
    goto e;
  c := ord(P^) shl 14;
  inc(P);
  result := result and $3FFF or c;
  if c <= $7f shl 14 then
    exit; // Values between 16257 and 2080768
  if P >= Last then
    goto e;
  c := ord(P^) shl 21;
  inc(P);
  result := result and $1FFFFF or c;
  if c <= $7f shl 21 then
    exit; // Values between 2080769 and 266338304
  if P >= Last then
e:begin
    {$ifdef ISDELPHI}
    result := 0; // avoid paranoid compiler hint
    {$endif ISDELPHI}
    ErrorOverflow;
  end;
  c := ord(P^) shl 28;
  inc(P);
  result := result {%H-}and $FFFFFFF or c;
end;

procedure TFastReader.VarNextInt;
begin
  repeat
    if P >= Last then
      break;  // reached end of input
    if P^ <= #$7f then
      break; // reached end of VarUInt32/VarUInt64
    inc(P);
  until false;
  inc(P);
end;

procedure TFastReader.VarNextInt(count: integer);
begin
  if count = 0 then
    exit;
  repeat
    if P >= Last then
      break;  // reached end of input
    if P^ > #$7f then
    begin
      inc(P);
      continue; // didn't reach end of VarUInt32/VarUInt64
    end;
    inc(P);
    dec(count);
    if count = 0 then
      break;
  until false;
end;

{$else not CPUX86} // on x86_64 and ARM, use registers for P/Last values

function TFastReader.VarUInt32: cardinal;
var
  c: cardinal;
  s, l: PByte;
label
  e, f;
begin
  s := pointer(P);
  l := pointer(Last);
  if PAnsiChar(s) >= PAnsiChar(l) then
    goto e;
  result := s^;
  inc(s);
  if result <= $7f then
    goto f;
  if PAnsiChar(s) >= PAnsiChar(l) then
    goto e;
  c := s^ shl 7;
  inc(s);
  result := result and $7F or c;
  if c <= $7f shl 7 then
    goto f; // Values between 128 and 16256
  if PAnsiChar(s) >= PAnsiChar(l) then
    goto e;
  c := s^ shl 14;
  inc(s);
  result := result and $3FFF or c;
  if c <= $7f shl 14 then
    goto f; // Values between 16257 and 2080768
  if PAnsiChar(s) >= PAnsiChar(l) then
    goto e;
  c := s^ shl 21;
  inc(s);
  result := result and $1FFFFF or c;
  if c <= $7f shl 21 then
    goto f; // Values between 2080769 and 266338304
  if PAnsiChar(s) >= PAnsiChar(l) then
e:begin
    {$ifdef ISDELPHI}
    result := 0; // avoid hint
    {$endif ISDELPHI}
    ErrorOverflow;
  end;
  c := s^ shl 28;
  inc(s);
  result := result {%H-}and $FFFFFFF or c;
f:P := pointer(s);
end;

procedure TFastReader.VarNextInt;
var
  s, l: PAnsiChar;
begin
  s := P;
  l := Last;
  repeat
    if s >= l then
      break;  // reached end of input
    if s^ <= #$7f then
      break; // reached end of VarUInt32/VarUInt64
    inc(s);
  until false;
  P := s + 1;
end;

procedure TFastReader.VarNextInt(count: integer);
var
  s, l: PAnsiChar;
begin
  if count = 0 then
    exit;
  s := P;
  l := Last;
  repeat
    if s >= l then
      break;  // reached end of input
    if s^ > #$7f then
    begin
      inc(s);
      continue; // didn't reach end of VarUInt32/VarUInt64
    end;
    inc(s);
    dec(count);
    if count = 0 then
      break;
  until false;
  P := s;
end;

{$endif CPUX86}

function TFastReader.PeekVarInt32(out value: PtrInt): boolean;
begin
  result := PeekVarUInt32(PtrUInt(value));
  if result then
    if value and 1 <> 0 then
      // 1->1, 3->2..
      value := value shr 1 + 1
    else      // 0->0, 2->-1, 4->-2..
      value := -(value shr 1);
end;

function TFastReader.PeekVarUInt32(out value: PtrUInt): boolean;
var
  s: PAnsiChar;
begin
  result := false;
  s := P;
  repeat
    if s >= Last then
      exit  // reached end of input -> returns false
    else if s^ <= #$7f then
      break; // reached end of VarUInt32
    inc(s);
  until false;
  s := P;
  value := VarUInt32; // fast value decode
  P := s; // rewind
  result := true;
end;

function TFastReader.VarUInt32Safe(out Value: cardinal): boolean;
var
  c, n, v: cardinal;
begin
  result := false;
  if P >= Last then
    exit;
  v := ord(P^);
  inc(P);
  if v > $7f then
  begin
    n := 0;
    v := v and $7F;
    repeat
      if P >= Last then
        exit;
      c := ord(P^);
      inc(P);
      inc(n, 7);
      if c <= $7f then
        break;
      v := v or ((c and $7f) shl n);
    until false;
    v := v or (c shl n);
  end;
  Value := v;
  result := true; // success
end;

function TFastReader.VarUInt64: QWord;
label
  e;
var
  c, n: PtrUInt;
begin
  if P >= Last then
e:  ErrorOverflow;
  c := ord(P^);
  inc(P);
  if c > $7f then
  begin
    result := c and $7F;
    n := 0;
    repeat
      if P >= Last then
        goto e;
      c := ord(P^);
      inc(P);
      inc(n, 7);
      if c <= $7f then
        break;
      result := result or (QWord(c and $7f) shl n);
    until false;
    result := result or (QWord(c) shl n);
  end
  else
    result := c;
end;

procedure TFastReader.VarBlob(out result: TValueResult);
var
  len: PtrUInt;
begin
  len := VarUInt32;
  if P + len > Last then
    ErrorOverflow;
  result.Ptr := P;
  result.Len := len;
  inc(P, len);
end;

function TFastReader.VarBlobSafe(out Value: TValueResult): boolean;
var
  len: PtrUInt;
begin
  len := VarUInt32;
  if P + len > Last then
  begin
    result := false;
    exit;
  end;
  Value.Ptr := P;
  Value.Len := len;
  inc(P, len);
  result := true;
end;

procedure TFastReader.VarBlob(out Value: TSynTempBuffer);
var
  len: PtrUInt;
begin
  len := VarUInt32;
  if P + len > Last then
    ErrorOverflow;
  Value.Init(P, len);
  inc(P, len);
end;

function TFastReader.VarBlob: TValueResult;
var
  len: PtrUInt;
label
  e;
{%H-}begin
  if P >= Last then
    goto e;
  len := ord(P^);
  if len < $80 then
    inc(P)
  else
    len := VarUInt32;
  result.Ptr := P;
  result.Len := len;
  if P + len <= Last then
    inc(P, len)
  else
e:  ErrorOverflow;
end;

function TFastReader.VarString: RawByteString;
begin
  with VarBlob do
    FastSetRawByteString(result{%H-}, Ptr, Len);
end;

function TFastReader.VarString(CodePage: integer): RawByteString;
begin
  with VarBlob do
    FastSetStringCP(result{%H-}, Ptr, Len, CodePage)
end;

procedure TFastReader.VarUtf8(out result: RawUtf8);
var
  len: PtrUInt;
label
  e;
begin
  if P >= Last then
    goto e;
  len := ord(P^);
  if len < $80 then
    inc(P)
  else
    len := VarUInt32;
  if P + len <= Last then
  begin
    FastSetString(result, P, len);
    inc(P, len);
  end
  else
e:  ErrorOverflow;
end;

function TFastReader.VarUtf8: RawUtf8;
begin
  VarUtf8(result);
end;

function TFastReader.VarShortString: ShortString;
var
  len: cardinal;
  s: PAnsiChar;
label
  e, r;
{%H-}begin
  s := P;
  if s >= Last then
    goto e;
  len := ord(s^);
  if len <= $7f then
  begin
    inc(s);
r:  P := s;
    inc(s, len);
    if s >= Last then
      goto e;
    result[0] := AnsiChar(len);
    MoveFast(P^, result[1], len);
    P := s;
    exit;
  end;
  len := (len and $7F) or (ord(s^) shl 7); // 2nd byte of VarUInt32 decoding
  inc(s);
  if len <= 255 then
    goto r;
e:ErrorOverflow;
end;

function TFastReader.VarUtf8Safe(out Value: RawUtf8): boolean;
var
  len: cardinal;
begin
  if VarUInt32Safe(len) then
    if len = 0 then
      result := true
    else if P + len <= Last then
    begin
      FastSetString(Value, P, len);
      inc(P, len);
      result := true;
    end
    else
      result := false
  else
    result := false;
end;

function CleverReadInteger(p, e: PAnsiChar; V: PInteger): PtrUInt;
// Clever = decode Values[i+1]-Values[i] storage (with special diff=1 count)
var
  i, n: PtrUInt;
begin
  result := PtrUInt(V);
  i := PInteger(p)^;
  inc(p, 4); // integer: firstValue
  V^ := i;
  inc(V);
  if PtrUInt(p) < PtrUInt(e) then
    repeat
      case p^ of
        #0:
          begin
            // B:0 W:difference with previous
            inc(i, PWord(p + 1)^);
            inc(p, 3);
            V^ := i;
            inc(V);
            if PtrUInt(p) < PtrUInt(e) then
              continue
            else
              break;
          end;
        #254:
          begin
            // B:254 W:byOne
            for n := 1 to PWord(p + 1)^ do
            begin
              inc(i);
              V^ := i;
              inc(V);
            end;
            inc(p, 3);
            if PtrUInt(p) < PtrUInt(e) then
              continue
            else
              break;
          end;
        #255:
          begin
            // B:255 B:byOne
            for n := 1 to PByte(p + 1)^ do
            begin
              inc(i);
              V^ := i;
              inc(V);
            end;
            inc(p, 2);
            if PtrUInt(p) < PtrUInt(e) then
              continue
            else
              break;
          end
      else
        begin
          // B:1..253 = difference with previous
          inc(i, ord(p^));
          inc(p);
          V^ := i;
          inc(V);
          if PtrUInt(p) < PtrUInt(e) then
            continue
          else
            break;
        end;
      end; // case p^ of
    until false;
  result := (PtrUInt(V) - result) shr 2; // returns count of stored integers
end;

function TFastReader.ReadVarUInt32Array(var Values: TIntegerDynArray): PtrInt;
var
  i: PtrInt;
  k: TBufferWriterKind;
  pi: PInteger;
  n, diff: integer;
  chunk, chunkend: PtrUInt;
begin
  result := VarUInt32;
  if result = 0 then
    exit;
  if result > length(Values) then // only set length is not big enough
    SetLength(Values, result);
  k := TBufferWriterKind(NextByte);
  pi := pointer(Values);
  n := result;
  case k of
    wkUInt32:
      begin
        Copy(pointer(Values), n * 4);
        exit;
      end;
    wkOffsetU,
    wkOffsetI:
      begin
        pi^ := VarUInt32;
        dec(n);
        if n = 0 then
          exit;
        diff := VarUInt32;
        if diff <> 0 then
        begin
          // all items have a fixed offset
          for i := 0 to n - 1 do
            PIntegerArray(pi)[i + 1] := PIntegerArray(pi)[i] + diff;
          exit;
        end
      end;
    wkFakeMarker:
      begin
        // caller should make the decoding: notify by returning the count as <0
        result := -result;
        exit;
      end;
  end;
  repeat
    // chunked format: Isize+values
    chunkend := Next4;
    chunk := PtrUInt(Next(chunkend));
    inc(chunkend, chunk);
    case k of
      wkVarInt32:
        repeat
          pi^ := FromVarInt32(PByte(chunk));
          inc(pi);
          dec(n);
        until (n = 0) or
              (chunk >= chunkend);
      wkVarUInt32:
        repeat
          pi^ := FromVarUInt32Big(PByte(chunk));
          inc(pi);
          dec(n);
        until (n = 0) or
              (chunk >= chunkend);
      wkSorted:
        begin
          diff := CleverReadInteger(pointer(chunk), pointer(chunkend), pi);
          dec(n, diff);
          inc(pi, diff);
        end;
      wkOffsetU:
        repeat
          PIntegerArray(pi)[1] := pi^ + integer(FromVarUInt32(PByte(chunk)));
          inc(pi);
          dec(n);
        until (n = 0) or
              (chunk >= chunkend);
      wkOffsetI:
        repeat
          PIntegerArray(pi)[1] := pi^ + FromVarInt32(PByte(chunk));
          inc(pi);
          dec(n);
        until (n = 0) or
              (chunk >= chunkend);
    else
      ErrorData('ReadVarUInt32Array got kind=%', [ord(k)]);
    end;
  until n = 0;
end;

type
  TBufferWriterKind64 = (
    wkVarUInt64, wkOffset64);

function TFastReader.ReadVarUInt64Array(var Values: TInt64DynArray): PtrInt;
var
  i, n: PtrInt;
  k: TBufferWriterKind64;
  pi: PQWord;
  diff: QWord;
  chunk, chunkend: PtrUInt;
begin
  result := VarUInt32;
  if result = 0 then
    exit;
  if result > length(Values) then // only set length is not big enough
    SetLength(Values, result);
  k := TBufferWriterKind64(NextByte);
  pi := pointer(Values);
  n := result;
  if k = wkOffset64 then
  begin
    pi^ := VarUInt64;
    dec(n);
    diff := VarUInt32;
    if diff <> 0 then
    begin
      // all items have a fixed offset
      for i := 0 to n - 1 do
        PQwordArray(pi)[i + 1] := PQwordArray(pi)[i] + diff;
      exit;
    end
  end;
  repeat
    // chunked format: Isize+values
    chunkend := Next4;
    chunk := PtrUInt(Next(chunkend));
    inc(chunkend, chunk);
    case k of
      wkVarUInt64:
        repeat
          pi^ := FromVarUInt64(PByte(chunk));
          inc(pi);
          dec(n);
        until (n = 0) or
              (chunk >= chunkend);
      wkOffset64:
        repeat
          PQwordArray(pi)[1] := pi^ + FromVarUInt64(PByte(chunk));
          inc(pi);
          dec(n);
        until (n = 0) or
              (chunk >= chunkend);
      else
        ErrorData('ReadVarUInt64Array got kind=%', [ord(k)]);
    end;
  until n = 0;
end;

function TFastReader.ReadVarRawUtf8DynArray(var Values: TRawUtf8DynArray): PtrInt;
var
  count, len: integer;
  fixedsize, chunk, chunkend: PtrUInt;
  PI: PRawUtf8;
begin
  result := VarUInt32;
  if result = 0 then
    exit;
  count := result;
  if count > length(Values) then // change Values[] length only if not big enough
    SetLength(Values, count);
  PI := pointer(Values);
  fixedsize := VarUInt32;
  repeat
    // chunked format: Isize+values
    chunkend := Next4;
    chunk := PtrUInt(Next(chunkend));
    inc(chunkend, chunk);
    if fixedsize = 0 then
      // variable size strings
      while (count > 0) and
            (chunk < chunkend) do
      begin
        len := FromVarUInt32(PByte(chunk));
        if len > 0 then
        begin
          FastSetString(PI^, pointer(chunk), len);
          inc(chunk, len);
        end
        else if PI^<>'' then
          PI^ := '';
        dec(count);
        inc(PI);
      end
    else
      // fixed size strings
      while (count > 0) and
            (chunk < chunkend) do
      begin
        FastSetString(PI^, pointer(chunk), fixedsize);
        inc(chunk, fixedsize);
        dec(count);
        inc(PI);
      end;
  until count <= 0;
  if PI <> @Values[result] then
    ErrorOverflow; // paranoid check
end;

function TFastReader.ReadCompressed(Load: TAlgoCompressLoad;
  BufferOffset: integer): RawByteString;
var
  comp: PAnsiChar;
  complen: PtrUInt;
begin
  complen := VarUInt32;
  comp := Next(complen);
  TAlgoCompress.Algo(comp, complen).Decompress(
    comp, complen, result, Load, BufferOffset);
end;


{ TBufferWriter }

constructor TBufferWriter.Create(aFile: THandle; BufLen: integer);
begin
  Create(THandleStream.Create(aFile), BufLen);
  fInternalStream := true;
end;

constructor TBufferWriter.Create(const aFileName: TFileName;
  BufLen: integer; Append: boolean);
var
  s: TStream;
begin
  if Append and
     FileExists(aFileName) then
  begin
    s := TFileStreamEx.Create(aFileName, fmOpenWrite);
    s.Seek(0, soEnd);
  end
  else
    s := TFileStreamEx.Create(aFileName, fmCreate);
  Create(s, BufLen);
  fInternalStream := true;
end;

procedure TBufferWriter.Setup(aStream: TStream; aBuf: pointer; aLen: integer);
begin
  fBufLen := aLen;
  fBufLen16 := aLen - 16;
  fBuffer := aBuf;
  fStream := aStream;
end;

constructor TBufferWriter.Create(aStream: TStream; BufLen: integer);
begin
  if BufLen > 1 shl 22 then
    BufLen := 1 shl 22 // 4 MB sounds right enough
  else if BufLen < 128 then
    raise EBufferException.CreateUtf8('%.Create(BufLen=%)', [self, BufLen]);
  GetMem(fBufferInternal, BufLen);
  Setup(aStream, fBufferInternal, BufLen);
end;

constructor TBufferWriter.Create(aStream: TStream;
  aTempBuf: pointer; aTempLen: integer);
begin
  Setup(aStream, aTempBuf, aTempLen);
end;

constructor TBufferWriter.Create(aClass: TStreamClass; BufLen: integer);
begin
  Create(aClass.Create, BufLen);
  fInternalStream := true;
end;

constructor TBufferWriter.Create(aClass: TStreamClass;
  aTempBuf: pointer; aTempLen: integer);
begin
  Setup(aClass.Create, aTempBuf, aTempLen);
  fInternalStream := true;
end;

constructor TBufferWriter.Create(const aStackBuffer: TTextWriterStackBuffer);
begin
  Setup(TRawByteStringStream.Create, @aStackBuffer, SizeOf(aStackBuffer));
  fInternalStream := true;
end;

destructor TBufferWriter.Destroy;
begin
  if fInternalStream then
    fStream.Free;
  if fBufferInternal <> nil then
    FreeMem(fBufferInternal);
  inherited;
end;

procedure TBufferWriter.InternalFlush;
begin
  if fPos > 0 then
  begin
    InternalWrite(fBuffer, fPos);
    fPos := 0;
  end;
end;

procedure TBufferWriter.InternalWrite(Data: pointer; DataLen: PtrInt);
begin
  inc(fTotalFlushed, DataLen);
  if fStream.InheritsFrom(TRawByteStringStream) and
     (fTotalFlushed > _STRMAXSIZE) then
    // Delphi strings have a 32-bit length so you should change your algorithm
    raise EBufferException.CreateUtf8('%.Write: % overflow (%)',
      [self, fStream, KBNoSpace(fTotalFlushed)]);
  fStream.WriteBuffer(Data^, DataLen);
end;

function TBufferWriter.GetTotalWritten: Int64;
begin
  result := fTotalFlushed + fPos;
end;

function TBufferWriter.Flush: Int64;
begin
  if fPos > 0 then
    InternalFlush;
  result := GetTotalWritten;
  fTotalFlushed := 0;
end;

procedure TBufferWriter.CancelAll;
begin
  fTotalFlushed := 0;
  fPos := 0;
  if fStream.ClassType = TRawByteStringStream then
    TRawByteStringStream(fStream).Size := 0
  else
    fStream.Seek(0, soBeginning);
end;

procedure TBufferWriter.FlushAndWrite(Data: pointer; DataLen: PtrInt);
begin
  if DataLen < 0 then
    exit;
  if fPos > 0 then
    InternalFlush;
  if DataLen > fBufLen then
    InternalWrite(Data, DataLen)
  else
  begin
    MoveFast(Data^, fBuffer^[fPos], DataLen);
    inc(fPos, DataLen);
  end;
end;

procedure TBufferWriter.Write(Data: pointer; DataLen: PtrInt);
var
  p: PtrUInt;
begin
  p := fPos;
  if p + PtrUInt(DataLen) <= PtrUInt(fBufLen) then
  begin
    MoveFast(Data^, fBuffer^[p], DataLen);
    inc(fPos, DataLen);
  end
  else
    FlushAndWrite(Data, DataLen); // will also handle DataLen<0
end;

procedure TBufferWriter.WriteN(Data: byte; Count: integer);
var
  len: integer;
begin
  while Count > 0 do
  begin
    if Count > fBufLen then
      len := fBufLen
    else
      len := Count;
    if fPos + len > fBufLen then
      InternalFlush;
    FillCharFast(fBuffer^[fPos], len, Data);
    inc(fPos, len);
    dec(Count, len);
  end;
end;

procedure TBufferWriter.Write1(Data: byte);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  fBuffer^[fPos] := Data;
  inc(fPos);
end;

procedure TBufferWriter.Write2(Data: cardinal);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  PWord(@fBuffer^[fPos])^ := Data;
  inc(fPos, SizeOf(Word));
end;

procedure TBufferWriter.Write2BigEndian(Data: cardinal);
begin
  Write2(swap(word(Data)));
end;

procedure TBufferWriter.Write4(Data: integer);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  PInteger(@fBuffer^[fPos])^ := Data;
  inc(fPos, SizeOf(integer));
end;

procedure TBufferWriter.Write4BigEndian(Data: integer);
begin
  Write4(bswap32(Data));
end;

procedure TBufferWriter.Write8(Data8Bytes: pointer);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  PInt64(@fBuffer^[fPos])^ := PInt64(Data8Bytes)^;
  inc(fPos, SizeOf(Int64));
end;

procedure TBufferWriter.WriteI64(Data: Int64);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  PInt64(@fBuffer^[fPos])^ := Data;
  inc(fPos, SizeOf(Data));
end;

procedure TBufferWriter.WriteVar(Data: pointer; DataLen: PtrInt);
label
  wr;
begin
  if fPos + DataLen <= fBufLen16 then // could fit in buffer (most common case)
  begin
    if DataLen < $80 then // e.g. small strings
    begin
      fBuffer^[fPos] := DataLen;
      inc(fPos);
      if DataLen = 0 then
        exit;
wr:   MoveFast(Data^, fBuffer^[fPos], DataLen);
      inc(fPos, DataLen);
      exit;
    end;
    fPos := PtrUInt(ToVarUInt32(DataLen, @fBuffer^[fPos])) - PtrUInt(fBuffer);
    goto wr;
  end;
  // Data wouldn't fit in memory buffer -> write as two explicit calls
  WriteVarUInt32(DataLen);
  Write(Data, DataLen);
end;

procedure TBufferWriter.WriteVar(var Item: TTempUtf8);
begin
  WriteVar(Item.Text, Item.Len);
  if Item.TempRawUtf8 <> nil then
    {$ifdef FPC}
    FastAssignNew(Item.TempRawUtf8);
    {$else}
    RawUtf8(Item.TempRawUtf8) := '';
    {$endif FPC}
end;

procedure TBufferWriter.Write(const Text: RawByteString);
begin
  WriteVar(pointer(Text), length(Text));
end;

procedure TBufferWriter.WriteShort(const Text: ShortString);
begin
  WriteVar(@Text[1], ord(Text[0]));
end;

procedure TBufferWriter.WriteBinary(const Data: RawByteString);
begin
  Write(pointer(Data), Length(Data));
end;

function TBufferWriter.DirectWritePrepare(maxlen: PtrInt;
  var tmp: RawByteString): PAnsiChar;
begin
  if (maxlen <= fBufLen) and
     (fPos + maxlen > fBufLen) then
    InternalFlush;
  if fPos + maxlen > fBufLen then
  begin
    if maxlen > length(tmp) then
      FastNewRawByteString(tmp, maxlen); // don't reallocate buffer (reuse)
    result := pointer(tmp);
  end
  else
    result := @fBuffer^[fPos]; // write directly into the buffer
end;

procedure TBufferWriter.DirectWriteFlush(len: PtrInt; const tmp: RawByteString);
begin
  if tmp = '' then
    inc(fPos, len)
  else
    Write(pointer(tmp), len);
end;

function TBufferWriter.DirectWriteReserve(maxlen: PtrInt): PByte;
begin
  if fPos + maxlen > fBufLen then
    InternalFlush;
  result := @fBuffer^[fPos]; // write directly into the buffer
end;

procedure TBufferWriter.DirectWriteReserved(pos: PByte);
begin
  fPos := PAnsiChar(pos) - pointer(fBuffer);
end;

procedure TBufferWriter.WriteXor(New, Old: PAnsiChar; Len: PtrInt;
  crc: PCardinal);
var
  L: integer;
  Dest: PAnsiChar;
begin
  if (New = nil) or
     (Old = nil) then
    exit;
  while Len > 0 do
  begin
    Dest := pointer(fBuffer);
    if fPos + Len > fBufLen then
      InternalFlush
    else
      inc(Dest, fPos);
    if Len > fBufLen then
      L := fBufLen
    else
      L := Len;
    XorMemory(pointer(Dest), pointer(New), pointer(Old), L);
    if crc <> nil then
      crc^ := crc32c(crc^, Dest, L);
    inc(Old, L);
    inc(New, L);
    dec(Len, L);
    inc(fPos, L);
  end;
end;

procedure TBufferWriter.WriteRawUtf8DynArray(const Values: TRawUtf8DynArray;
  ValuesCount: integer);
begin
  WriteRawUtf8Array(pointer(Values), ValuesCount);
end;

procedure TBufferWriter.WriteRawUtf8Array(Values: PPtrUIntArray;
  ValuesCount: integer);
var
  n: integer;
  i: PtrInt;
  fixedsize, len: PtrUInt;
  P, PEnd: PByte;
  PBeg: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  if ValuesCount = 0 then
    exit;
  fixedsize := Values^[0];
  if fixedsize <> 0 then
  begin
    fixedsize := {%H-}PStrLen(fixedsize - _STRLEN)^;
    for i := 1 to ValuesCount - 1 do
      if (Values^[i] = 0) or
         ({%H-}PStrLen(Values^[i] - _STRLEN)^ <> TStrLen(fixedsize)) then
      begin
        fixedsize := 0;
        break;
      end;
  end;
  WriteVarUInt32(fixedsize);
  repeat
    P := @fBuffer^[fPos];
    PEnd := @fBuffer^[fBufLen16];
    if PtrUInt(P) < PtrUInt(PEnd) then
    begin
      n := ValuesCount;
      PBeg := PAnsiChar(P); // leave space for chunk size
      inc(P, 4);
      if fixedsize = 0 then
        for i := 0 to ValuesCount - 1 do
          if Values^[i] = 0 then
          begin
            P^ := 0; // store length=0
            inc(P);
            if PtrUInt(P) >= PtrUInt(PEnd) then
            begin
              n := i + 1;
              break; // avoid buffer overflow
            end;
          end
          else
          begin
            len := {%H-}PStrLen(Values^[i] - _STRLEN)^;
            if PtrUInt(PEnd) - PtrUInt(P) <= len then
            begin
              n := i;
              break; // avoid buffer overflow
            end;
            P := ToVarUInt32(len, P);
            MoveFast(pointer(Values^[i])^, P^, len); // here len>0
            inc(P, len);
          end
      else // fixedsize<>0:
        for i := 0 to ValuesCount - 1 do
        begin
          if PtrUInt(PEnd) - PtrUInt(P) <= fixedsize then
          begin
            n := i;
            break; // avoid buffer overflow
          end;
          MoveFast(pointer(Values^[i])^, P^, fixedsize);
          inc(P, fixedsize);
        end;
      len := PAnsiChar(P) - PBeg; // format: Isize+varUInt32s*strings
      PInteger(PBeg)^ := len - 4;
      inc(fPos, len);
      inc(PByte(Values), n * SizeOf(PtrInt));
      dec(ValuesCount, n);
      if ValuesCount = 0 then
        break;
    end;
    InternalFlush;
  until false;
end;

procedure TBufferWriter.WriteStream(aStream: TCustomMemoryStream;
  aStreamSize: integer);
begin
  if aStreamSize < 0 then
    if aStream = nil then
      aStreamSize := 0
    else
      aStreamSize := aStream.Size;
  WriteVarUInt32(aStreamSize);
  if aStreamSize > 0 then
    Write(aStream.Memory, aStreamSize);
end;

procedure TBufferWriter.WriteVarInt32(Value: PtrInt);
begin
  if Value <= 0 then
    // 0->0, -1->2, -2->4..
    Value := (-Value) shl 1
  else    // 1->1, 2->3..
    Value := (Value shl 1) - 1;
  WriteVarUInt32(Value);
end;

procedure TBufferWriter.WriteVarUInt32(Value: PtrUInt);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  fPos := PtrUInt(ToVarUInt32(Value, @fBuffer^[fPos])) - PtrUInt(fBuffer);
end;

procedure TBufferWriter.WriteVarInt64(Value: Int64);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  fPos := PtrUInt(ToVarInt64(Value, @fBuffer^[fPos])) - PtrUInt(fBuffer);
end;

procedure TBufferWriter.WriteVarUInt64(Value: QWord);
begin
  if fPos > fBufLen16 then
    InternalFlush;
  fPos := PtrUInt(ToVarUInt64(Value, @fBuffer^[fPos])) - PtrUInt(fBuffer);
end;

function CleverStoreInteger(p: PInteger; V, VEnd: PAnsiChar; pCount: integer;
  var StoredCount: integer): PAnsiChar;
// Clever = store Values[i+1]-Values[i] (with special diff=1 count)
// format:  integer: firstValue, then:
//          B:0 W:difference with previous
//          B:1..253 = difference with previous
//          B:254 W:byOne
//          B:255 B:byOne
var
  i, d, byOne: integer;
begin
  StoredCount := pCount;
  if pCount <= 0 then
  begin
    result := V;
    exit;
  end;
  i := p^;
  PInteger(V)^ := p^;
  inc(V, 4);
  dec(pCount);
  inc(p);
  byOne := 0;
  if pCount > 0 then
    repeat
      d := p^ - i;
      i := p^;
      inc(p);
      if d = 1 then
      begin
        dec(pCount);
        inc(byOne);
        if pCount > 0 then
          continue;
      end
      else if d < 0 then
      begin
        result := nil;
        exit;
      end;
      if byOne <> 0 then
      begin
        case byOne of
          1:
            begin
              V^ := #1;
              inc(V);
            end; // B:1..253 = difference with previous
          2:
            begin
              PWord(V)^ := $0101;
              inc(V, 2);
            end; // B:1..253 = difference
        else
          if byOne > 255 then
          begin
            while byOne > 65535 do
            begin
              PInteger(V)^ := $fffffe;
              inc(V, 3); // store as many len=$ffff as necessary
              dec(byOne, $ffff);
            end;
            PInteger(V)^ := byOne shl 8 + $fe;
            inc(V, 3); // B:254 W:byOne
          end
          else
          begin
            PWord(V)^ := byOne shl 8 + $ff;
            inc(V, 2); // B:255 B:byOne
          end;
        end; // case byOne of
        if pCount = 0 then
          break;
        byOne := 0;
      end;
      if (d = 0) or
         (d > 253) then
      begin
        while cardinal(d) > 65535 do
        begin
          PInteger(V)^ := $ffff00;
          inc(V, 3); // store as many len=$ffff as necessary
          dec(cardinal(d), $ffff);
        end;
        dec(pCount);
        PInteger(V)^ := d shl 8;
        inc(V, 3); // B:0 W:difference with previous
        if (V < VEnd) and
           (pCount > 0) then
          continue
        else
          break;
      end
      else
      begin
        dec(pCount);
        V^ := AnsiChar(d);
        inc(V); // B:1..253 = difference with previous
        if (V < VEnd) and
           (pCount > 0) then
          continue
        else
          break;
      end;
      if V >= VEnd then
        break; // avoid GPF
    until false;
  dec(StoredCount, pCount);
  result := V;
end;

procedure TBufferWriter.WriteVarUInt32Array(const Values: TIntegerDynArray;
  ValuesCount: integer; DataLayout: TBufferWriterKind);
begin
  WriteVarUInt32Values(pointer(Values), ValuesCount, DataLayout);
end;

procedure TBufferWriter.WriteVarUInt32Values(Values: PIntegerArray;
  ValuesCount: integer; DataLayout: TBufferWriterKind);
var
  diff, v, vp, n: integer;
  i: PtrInt;
  P: PByte;
  PBeg, PEnd: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  if ValuesCount = 0 then
    exit;
  fBuffer^[fPos] := ord(DataLayout);
  inc(fPos);
  vp := Values^[0];
  if DataLayout in [wkOffsetU, wkOffsetI] then
  begin
    fPos := PtrUInt(ToVarUInt32(vp, @fBuffer^[fPos])) - PtrUInt(fBuffer);
    diff := Values^[1] - vp;
    inc(PInteger(Values));
    dec(ValuesCount);
    if ValuesCount = 0 then
      exit;
    if diff > 0 then
    begin
      for i := 1 to ValuesCount - 1 do
        if Values^[i] - Values^[i - 1] <> diff then
        begin
          diff := 0; // not always the same offset
          break;
        end;
    end
    else
      diff := 0;
    fPos := PtrUInt(ToVarUInt32(diff, @fBuffer^[fPos])) - PtrUInt(fBuffer);
    if diff <> 0 then
      exit; // same offset for all items (fixed sized records) -> quit now
  end;
  repeat
    P := @fBuffer^[fPos];
    PEnd := @fBuffer^[fBufLen16];
    if PtrUInt(P) < PtrUInt(PEnd) then
    begin
      case DataLayout of
        wkUInt32:
          // format: uncompressed array of cardinals
          begin
            n := (fBufLen - fPos) shr 2;
            if ValuesCount < n then
              n := ValuesCount;
            MoveFast(Values^, P^, n * 4);
            inc(P, n * 4);
          end;
        wkVarInt32,
        wkVarUInt32,
        wkOffsetU,
        wkOffsetI:
          begin
            // format: Isize + varUInt32s
            PBeg := PAnsiChar(P); // leave space for chunk size
            inc(P, 4);
            n := ValuesCount;
            for i := 0 to ValuesCount - 1 do
            begin
              v := Values^[i];
              case DataLayout of
                wkVarInt32:
                  P := ToVarInt32(v, P);
                wkVarUInt32:
                  P := ToVarUInt32(v, P);
                wkOffsetU:
                  P := ToVarUInt32(v - vp, P);
                wkOffsetI:
                  P := ToVarInt32(v - vp, P);
              end;
              vp := v;
              if PtrUInt(P) >= PtrUInt(PEnd) then
              begin
                n := i + 1;
                break; // avoid buffer overflow
              end;
            end;
            PInteger(PBeg)^ := PAnsiChar(P) - PBeg - 4;
          end;
        wkSorted:
          begin
            // format: Isize + cleverStorage
            PBeg := PAnsiChar(P) + 4; // leave space for chunk size
            P := PByte(CleverStoreInteger(pointer(Values), PBeg, PEnd, ValuesCount, n));
            if P = nil then
              raise EBufferException.CreateUtf8(
                '%.WriteVarUInt32Array: data not sorted', [self]);
            PInteger(PBeg - 4)^ := PAnsiChar(P) - PBeg;
          end;
      end;
      inc(PByte(Values), n * 4);
      fPos := PtrUInt(P) - PtrUInt(fBuffer);
      dec(ValuesCount, n);
      if ValuesCount = 0 then
        break;
    end;
    InternalFlush;
  until false;
end;

procedure TBufferWriter.WriteVarUInt64DynArray(const Values: TInt64DynArray;
  ValuesCount: integer; Offset: boolean);
var
  n: integer;
  i: PtrInt;
  diff: Int64;
  P, PEnd: PByte;
  PI: PInt64Array;
  PBeg: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  if ValuesCount = 0 then
    exit;
  PI := pointer(Values);
  if Offset then
  begin
    fBuffer^[fPos] := ord(wkOffset64);
    fPos := PtrUInt(ToVarUInt64(PI^[0], @fBuffer^[fPos + 1])) - PtrUInt(fBuffer);
    diff := PI^[1] - PI^[0];
    inc(PByte(PI), 8);
    dec(ValuesCount);
    if ValuesCount = 0 then
      exit;
    if (diff > 0) and
       (diff < MaxInt) then
    begin
      for i := 1 to ValuesCount - 1 do
        if PI^[i] - PI^[i - 1] <> diff then
        begin
          diff := 0; // not always the same offset
          break;
        end;
    end
    else
      diff := 0;
    fPos := PtrUInt(ToVarUInt32(diff, @fBuffer^[fPos])) - PtrUInt(fBuffer);
    if diff <> 0 then
      exit; // same offset for all items (fixed sized records) -> quit now
  end
  else
  begin
    fBuffer^[fPos] := ord(wkVarUInt64);
    inc(fPos);
  end;
  repeat
    P := @fBuffer^[fPos];
    PEnd := @fBuffer^[fBufLen16];
    if PtrUInt(P) < PtrUInt(PEnd) then
    begin
      PBeg := PAnsiChar(P); // leave space for chunk size
      inc(P, 4);
      n := ValuesCount;
      for i := 0 to ValuesCount - 1 do
      begin
        if Offset then
          P := ToVarUInt64(PI^[i] - PI^[i - 1], P) // store diffs
        else
          P := ToVarUInt64(PI^[i], P);
        if PtrUInt(P) >= PtrUInt(PEnd) then
        begin
          n := i + 1;
          break; // avoid buffer overflow
        end;
      end;
      PInteger(PBeg)^ := PAnsiChar(P) - PBeg - 4; // format: Isize+varUInt32/64s
      inc(PByte(PI), n * 8);
      fPos := PtrUInt(P) - PtrUInt(fBuffer);
      dec(ValuesCount, n);
      if ValuesCount = 0 then
        break;
    end;
    InternalFlush;
  until false;
end;

function TBufferWriter.FlushTo: RawByteString;
begin
  InternalFlush;
  result := (fStream as TRawByteStringStream).DataString;
end;

function TBufferWriter.FlushToBytes: TBytes;
var
  siz: Int64;
begin
  result := nil;
  siz := GetTotalWritten;
  if siz > _DAMAXSIZE then
    raise EBufferException.CreateUtf8('%.FlushToBytes: overflow (%)', [KB(siz)]);
  SetLength(result, siz);
  if fStream.Position = 0 then
    // direct assignment from internal buffer
    MoveFast(fBuffer[0], pointer(result)^, fPos)
  else
  begin
    // from temporary allocation in TRawByteStringStream.DataString
    Flush;
    MoveFast(pointer((fStream as TRawByteStringStream).DataString)^,
      pointer(result)^, TotalWritten);
  end;
end;

function TBufferWriter.FlushAndCompress(nocompression: boolean;
  algo: TAlgoCompress; BufferOffset: integer): RawByteString;
var
  trig: integer;
begin
  if algo = nil then
    algo := AlgoSynLZ;
  trig := SYNLZTRIG[nocompression];
  if fStream.Position = 0 then
    // direct compression from internal buffer
    result := algo.Compress(PAnsiChar(fBuffer), fPos, trig, false, BufferOffset)
  else
    // from temporary allocation in TRawByteStringStream.DataString
    result := algo.Compress(FlushTo, trig, false, BufferOffset);
end;



{ ************ TAlgoCompress Compression/Decompression Classes }

{ TAlgoCompress }

var
  // don't use TObjectList before mormot.core.json registered TRttiJson
  SynCompressAlgos: array of TAlgoCompress;

constructor TAlgoCompress.Create;
var
  existing: TAlgoCompress;
begin
  existing := Algo(fAlgoID);
  if existing <> nil then
    raise EAlgoCompress.CreateUtf8('%.Create: AlgoID=% already registered by %',
      [self, fAlgoID, existing]);
  ObjArrayAdd(SynCompressAlgos, self);
  RegisterGlobalShutdownRelease(self);
end;

destructor TAlgoCompress.Destroy;
begin
  if LogCompressAlgo = self then
    LogCompressAlgo := nil; // avoid GPF at shutdown
  inherited Destroy;
end;

class function TAlgoCompress.Algo(const Comp: RawByteString): TAlgoCompress;
begin
  result := Algo(Pointer(Comp), Length(Comp));
end;

class function TAlgoCompress.Algo(const Comp: TByteDynArray): TAlgoCompress;
begin
  result := Algo(Pointer(Comp), Length(Comp));
end;

class function TAlgoCompress.Algo(Comp: PAnsiChar; CompLen: integer): TAlgoCompress;
begin
  if (Comp <> nil) and
     (CompLen > 9) then
    if ord(Comp[4]) <= 1 then // inline-friendly Comp[4]<=COMPRESS_SYNLZ
      result := AlgoSynLZ
    else // COMPRESS_STORED is also handled as SynLZ
      result := Algo(ord(Comp[4]))
  else
    result := nil;
end;

class function TAlgoCompress.Algo(Comp: PAnsiChar; CompLen: integer;
  out IsStored: boolean): TAlgoCompress;
begin
  if (Comp <> nil) and
     (CompLen > 9) then
  begin
    IsStored := Comp[4] = COMPRESS_STORED;
    result := Algo(ord(Comp[4]));
  end
  else
  begin
    IsStored := false;
    result := nil;
  end;
end;

class function TAlgoCompress.Algo(aAlgoID: byte): TAlgoCompress;
var
  n: integer;
  ptr: ^TAlgoCompress;
begin
  if aAlgoID <= COMPRESS_SYNLZ then // COMPRESS_STORED is handled as SynLZ
    result := AlgoSynLZ
  else
  begin
    ptr := pointer(SynCompressAlgos);
    if ptr <> nil then
    begin
      n := PDALen(PAnsiChar(ptr) - _DALEN)^ + ( _DAOFF - 1 ); // - 1 for List[0]
      if n > 0 then
        repeat
          inc(ptr); // ignore List[0] = AlgoSynLZ
          result := ptr^;
          if result.fAlgoID = aAlgoID then
            exit;
          dec(n);
        until n = 0;
    end;
    result := nil;
  end;
end;

class function TAlgoCompress.UncompressedSize(const Comp: RawByteString): integer;
begin
  result := Algo(Comp).DecompressHeader(pointer(Comp), length(Comp));
end;

function TAlgoCompress.AlgoName: TShort16;
var
  s: PShortString;
  i: integer;
begin
  if self = nil then
    result := 'none'
  else
  begin
    s := ClassNameShort(self);
    if IdemPChar(@s^[1], 'TALGO') then
    begin
      result[0] := AnsiChar(ord(s^[0]) - 5);
      inc(PByte(s), 5);
    end
    else
      result[0] := s^[0];
    if result[0] > #16 then
      result[0] := #16;
    for i := 1 to ord(result[0]) do
      result[i] := NormToLower[s^[i]];
  end;
end;

procedure TAlgoCompress.EnsureAlgoHasNoForcedFormat(const caller: shortstring);
begin
  if fAlgoHasForcedFormat then
    raise EAlgoCompress.CreateUtf8('%.% is unsupported', [self, caller]);
end;

function TAlgoCompress.AlgoHash(Previous: cardinal;
  Data: pointer; DataLen: integer): cardinal;
begin
  result := crc32c(Previous, Data, DataLen);
end;

function TAlgoCompress.AlgoHash(ForceHash32: boolean;
  Data: pointer; DataLen: integer): cardinal;
begin
  if ForceHash32 then
    result := Hash32(Data, DataLen)
  else
    result := AlgoHash(0, Data, DataLen);
end;

function TAlgoCompress.Compress(const Plain: RawByteString;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean;
  BufferOffset: integer): RawByteString;
begin
  result := Compress(pointer(Plain), Length(Plain), CompressionSizeTrigger,
    CheckMagicForCompressed, BufferOffset);
end;

function TAlgoCompress.Compress(Plain: PAnsiChar; PlainLen: integer;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean;
  BufferOffset: integer): RawByteString;
var
  len: integer;
  R: PAnsiChar;
  crc: cardinal;
  tmp: array[0..16383] of AnsiChar;  // big enough to resize result in-place
begin
  if (PlainLen = 0) or
     (Plain = nil) then
  begin
    result := '';
    exit;
  end;
  EnsureAlgoHasNoForcedFormat('Compress');
  crc := AlgoHash(0, Plain, PlainLen);
  if (PlainLen < CompressionSizeTrigger) or
     (CheckMagicForCompressed and
      IsContentCompressed(Plain, PlainLen)) then
  begin
    FastNewRawByteString(result, PlainLen + BufferOffset + 9);
    R := pointer(result);
    inc(R, BufferOffset);
    PCardinal(R)^ := crc;
    R[4] := COMPRESS_STORED;
    PCardinal(R + 5)^ := crc;
    MoveFast(Plain^, R[9], PlainLen);
  end
  else
  begin
    len := CompressDestLen(PlainLen) + BufferOffset;
    if len > SizeOf(tmp) then
    begin
      FastNewRawByteString(result, len);
      R := pointer(result);
    end
    else
      R := @tmp;
    inc(R, BufferOffset);
    PCardinal(R)^ := crc;
    len := AlgoCompress(Plain, PlainLen, R + 9);
    if len >= PlainLen then
    begin
      // store if compression was not worth it
      R[4] := COMPRESS_STORED;
      PCardinal(R + 5)^ := crc;
      MoveFast(Plain^, R[9], PlainLen);
      len := PlainLen;
    end
    else
    begin
      R[4] := AnsiChar(AlgoID);
      PCardinal(R + 5)^ := AlgoHash(0, R + 9, len);
    end;
    inc(len, BufferOffset + 9);
    if R = @tmp[BufferOffset] then
      FastSetRawByteString(result, @tmp, len)
    else
      FakeLength(result, len);
  end;
end;

function TAlgoCompress.Compress(Plain, Comp: PAnsiChar; PlainLen, CompLen: integer;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean): integer;
var
  len: integer;
begin
  result := 0;
  if (PlainLen = 0) or
     (CompLen < PlainLen + 9) then
    exit;
  EnsureAlgoHasNoForcedFormat('Compress');
  PCardinal(Comp)^ := AlgoHash(0, Plain, PlainLen);
  if (PlainLen >= CompressionSizeTrigger) and
     not (CheckMagicForCompressed and
          IsContentCompressed(Plain, PlainLen)) then
  begin
    len := CompressDestLen(PlainLen);
    if CompLen < len then
      exit;
    len := AlgoCompress(Plain, PlainLen, Comp + 9);
    if len < PlainLen then
    begin
      Comp[4] := AnsiChar(AlgoID);
      PCardinal(Comp + 5)^ := AlgoHash(0, Comp + 9, len);
      result := len + 9;
      exit;
    end;
  end;
  Comp[4] := COMPRESS_STORED;
  PCardinal(Comp + 5)^ := PCardinal(Comp)^;
  MoveFast(Plain^, Comp[9], PlainLen);
  result := PlainLen + 9;
end;

function TAlgoCompress.CompressDestLen(PlainLen: integer): integer;
begin
  if self = nil then
    result := 0
  else
    result := AlgoCompressDestLen(PlainLen) + 9;
end;

function TAlgoCompress.CompressToBytes(Plain: PAnsiChar; PlainLen: integer;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean): TByteDynArray;
var
  len: integer;
  R: PAnsiChar;
  crc: cardinal;
begin
  Finalize(result);
  if (self = nil) or
     (PlainLen = 0) then
    exit;
  EnsureAlgoHasNoForcedFormat('CompressToBytes');
  crc := AlgoHash(0, Plain, PlainLen);
  if PlainLen < CompressionSizeTrigger then
  begin
    SetLength(result, PlainLen + 9);
    R := pointer(result);
    PCardinal(R)^ := crc;
    R[4] := COMPRESS_STORED;
    PCardinal(R + 5)^ := crc;
    MoveFast(Plain^, R[9], PlainLen);
  end
  else
  begin
    SetLength(result, CompressDestLen(PlainLen));
    R := pointer(result);
    PCardinal(R)^ := crc;
    len := AlgoCompress(Plain, PlainLen, R + 9);
    if len >= PlainLen then
    begin
      // store if compression not worth it
      R[4] := COMPRESS_STORED;
      PCardinal(R + 5)^ := crc;
      MoveFast(Plain^, R[9], PlainLen);
      len := PlainLen;
    end
    else
    begin
      R[4] := AnsiChar(AlgoID);
      PCardinal(R + 5)^ := AlgoHash(0, R + 9, len);
    end;
    SetLength(result, len + 9);
  end;
end;

function TAlgoCompress.CompressToBytes(const Plain: RawByteString;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean): TByteDynArray;
begin
  result := CompressToBytes(pointer(Plain), Length(Plain),
    CompressionSizeTrigger, CheckMagicForCompressed);
end;

function TAlgoCompress.Decompress(const Comp: TByteDynArray): RawByteString;
begin
  Decompress(pointer(Comp), length(Comp), result);
end;

procedure TAlgoCompress.Decompress(Comp: PAnsiChar; CompLen: integer;
  out result: RawByteString; Load: TAlgoCompressLoad; BufferOffset: integer);
var
  len: integer;
  dec: PAnsiChar;
begin
  len := DecompressHeader(Comp, CompLen, Load);
  if len = 0 then
    exit;
  FastSetString(RawUtf8(result), len + BufferOffset); // CP_UTF8 for FPC RTL bug
  dec := pointer(result);
  if not DecompressBody(Comp, dec + BufferOffset, CompLen, len, Load) then
    result := '';
end;

function TAlgoCompress.Decompress(const Comp: RawByteString; Load: TAlgoCompressLoad;
  BufferOffset: integer): RawByteString;
begin
  Decompress(pointer(Comp), length(Comp), result, Load, BufferOffset);
end;

function TAlgoCompress.TryDecompress(const Comp: RawByteString;
  out Dest: RawByteString; Load: TAlgoCompressLoad): boolean;
var
  len: integer;
begin
  result := Comp = '';
  if result then
    exit;
  len := DecompressHeader(pointer(Comp), length(Comp), Load);
  if len = 0 then
    exit; // invalid crc32c
  FastSetString(RawUtf8(Dest), len); // assume CP_UTF8 for FPC RTL bug
  if DecompressBody(pointer(Comp), pointer(Dest), length(Comp), len, Load) then
    result := true
  else
    Dest := '';
end;

function TAlgoCompress.Decompress(const Comp: RawByteString;
  out PlainLen: integer; var tmp: RawByteString;
  Load: TAlgoCompressLoad): pointer;
begin
  result := Decompress(pointer(Comp), length(Comp), PlainLen, tmp, Load);
end;

function TAlgoCompress.Decompress(Comp: PAnsiChar; CompLen: integer;
  out PlainLen: integer; var tmp: RawByteString; Load: TAlgoCompressLoad): pointer;
begin
  result := nil;
  if self = nil then
    exit;
  EnsureAlgoHasNoForcedFormat('Decompress');
  PlainLen := DecompressHeader(Comp, CompLen, Load);
  if PlainLen = 0 then
    exit;
  if Comp[4] = COMPRESS_STORED then
    result := Comp + 9
  else
  begin
    if PlainLen > length(tmp) then
      FastSetString(RawUtf8(tmp), PlainLen); // assume CP_UTF8 for FPC RTL bug
    if DecompressBody(Comp, pointer(tmp), CompLen, PlainLen, Load) then
      result := pointer(tmp);
  end;
end;

function TAlgoCompress.DecompressPartial(Comp, Partial: PAnsiChar;
  CompLen, PartialLen, PartialLenMax: integer): integer;
var
  BodyLen: integer;
begin
  result := 0;
  if (self = nil) or
     (CompLen <= 9) or
     (Comp = nil) or
     (PartialLenMax < PartialLen) then
    exit;
  EnsureAlgoHasNoForcedFormat('DecompressPartial');
  if Comp[4] = COMPRESS_STORED then
    if PCardinal(Comp)^ = PCardinal(Comp + 5)^ then
      BodyLen := CompLen - 9
    else
      exit
  else if Comp[4] = AnsiChar(AlgoID) then
    BodyLen := AlgoDecompressDestLen(Comp + 9)
  else
    exit;
  if PartialLen > BodyLen then
    PartialLen := BodyLen;
  if Comp[4] = COMPRESS_STORED then
    MoveFast(Comp[9], Partial[0], PartialLen)
  else if AlgoDecompressPartial(Comp + 9, CompLen - 9,
           Partial, PartialLen, PartialLenMax) < PartialLen then
    exit;
  result := PartialLen;
end;

type
  // disk header of TAlgoCompress chunk
  TAlgoCompressHead = packed record
    Magic: cardinal;
    CompressedSize: integer;
    CompressedHash: cardinal;
    UnCompressedSize: integer;
    UncompressedHash: cardinal;
  end;
  PAlgoCompressHead = ^TAlgoCompressHead;

  TAlgoCompressTrailer = packed record
    HeaderRelativeOffset: cardinal;
    Magic: cardinal;
  end;
  PAlgoCompressTrailer = ^TAlgoCompressTrailer;

function TAlgoCompress.StreamCompress(Source, Dest: TStream; Magic: cardinal;
  ForceHash32, WithTrailer: boolean; ChunkBytes: PtrInt): Int64;
var
  count: Int64;
  S, D: pointer;
  head: TAlgoCompressHead;
  trail: TAlgoCompressTrailer;
  tmps, tmpd: RawByteString;
begin
  result := 0;
  if (self = nil) or
     (Dest = nil) or
     (Source = nil) then
    exit;
  EnsureAlgoHasNoForcedFormat('StreamCompress');
  count := Source.Size;
  if count = 0 then
    exit;
  S := GetStreamBuffer(Source);
  head.Magic := Magic;
  repeat
    // compress Source into Dest with proper chunking
    if count > ChunkBytes then
      head.UnCompressedSize := ChunkBytes
    else
      head.UnCompressedSize := count;
    if S = nil then
    begin
      FastNewRawByteString(tmps, head.UnCompressedSize);
      S := pointer(tmps); // here S is a temporary buffer
    end;
    if {%H-}tmpd = '' then
      FastNewRawByteString(tmpd, AlgoCompressDestLen(head.UnCompressedSize));
    dec(count, head.UnCompressedSize); // supports premature end of input
    if S = pointer(tmps) then
      head.UnCompressedSize := Source.Read(S^, head.UnCompressedSize);
    if head.UnCompressedSize <= 0 then
      exit; // read error
    head.UncompressedHash := AlgoHash(ForceHash32, S, head.UnCompressedSize);
    D := pointer(tmpd);
    head.CompressedSize := AlgoCompress(S, head.UnCompressedSize, D);
    if head.CompressedSize >= head.UnCompressedSize then
    begin
      D := S; // compression is not worth it -> store
      head.CompressedSize := head.UnCompressedSize;
      head.CompressedHash := head.UncompressedHash;
    end
    else
      head.CompressedHash := AlgoHash(ForceHash32, D, head.CompressedSize);
    Dest.WriteBuffer(head, SizeOf(head));
    Dest.WriteBuffer(D^, head.CompressedSize);
    if S <> pointer(tmps) then
      inc(PByte(S), head.UnCompressedSize); // move ahead to next chunk
    inc(result, SizeOf(head) + head.CompressedSize);
  until count = 0;
  if not WithTrailer then
    exit;
  inc(result, SizeOf(trail));
  trail.Magic := Magic;
  trail.HeaderRelativeOffset := result;        // Int64 into cardinal
  if trail.HeaderRelativeOffset <> result then // max 4GB compressed size
    RaiseStreamError(self, 'StreamCompress trail overflow');
  Dest.WriteBuffer(trail, SizeOf(trail));
end;

function TAlgoCompress.StreamCompress(Source: TStream;
  const DestFile: TFileName; Magic: cardinal; ForceHash32, WithTrailer: boolean;
  ChunkBytes: PtrInt): Int64;
var
  F: TStream;
begin
  F := TFileStreamEx.Create(DestFile, fmCreate);
  try
    result := StreamCompress(Source, F, Magic, ForceHash32, WithTrailer, ChunkBytes);
  finally
    F.Free;
  end;
end;

function TAlgoCompress.StreamUnCompress(Source: TStream; Magic: cardinal;
  ForceHash32: boolean): TMemoryStream;
begin
  result := TMemoryStream.Create;
  if not StreamUncompress(Source, result, Magic, ForceHash32) then
    FreeAndNil(result);
end;

function TAlgoCompress.StreamUnCompress(Source, Dest: TStream; Magic: cardinal;
  ForceHash32: boolean): boolean;
var
  S, D: PAnsiChar;
  sourcePosition, resultSize, sourceSize: Int64;
  Head: TAlgoCompressHead;
  offs, rd: cardinal;
  Trailer: TAlgoCompressTrailer absolute Head;
  tmps, tmpd: RawByteString;
  stored: boolean;

  function MagicSeek: boolean;
  // Source not positioned as expected -> try from the TAlgoCompressTrailer end
  var
    t: PAlgoCompressTrailer;
    tmplen: PtrInt;
    tmp: array[word] of byte;
    Trailer: TAlgoCompressTrailer absolute tmp;
  begin
    result := false;
    Source.Position := sourceSize - SizeOf(Trailer);
    if (Source.Read(Trailer, SizeOf(Trailer)) <> SizeOf(Trailer)) or
       (Trailer.Magic <> Magic) then
    begin
      // may have been appended before a digital signature -> try last 64KB
      tmplen := SizeOf(tmp);
      if sourcesize < tmplen then
        tmplen := sourcesize;
      Source.Position := sourceSize - tmplen;
      if Source.Read(tmp, tmplen) <> tmplen then
        exit;
      dec(tmplen, SizeOf(TAlgoCompressTrailer));
      t := @tmp[tmplen];
      repeat
        dec(PByte(t)); // search backward
        if PtrUInt(t) < PtrUInt(@tmp) then
          exit;
      until t^.Magic = Magic;
      dec(sourceSize, PtrUInt(@tmp[tmplen]) - PtrUInt(t));    // adjust
      sourcePosition := sourceSize - t^.HeaderRelativeOffset; // found
    end
    else
      sourcePosition := sourceSize - Trailer.HeaderRelativeOffset;
    Source.Position := sourcePosition;
    if (Source.Read(Head, SizeOf(Head)) <> SizeOf(Head)) or
       (Head.Magic <> Magic) then
      exit;
    result := true;
  end;

begin
  result := false;
  if (self = nil) or
     (Source = nil) then
    exit;
  EnsureAlgoHasNoForcedFormat('StreamUnCompress');
  sourceSize := Source.Size;
  sourcePosition := Source.Position;
  if Source.Read(Head, SizeOf(Head)) <> SizeOf(Head) then
    exit;
  if (Head.Magic <> Magic) and
     not MagicSeek then
    exit;
  offs := 0;
  resultSize := 0;
  repeat
    // read next chunk from Source
    inc(sourcePosition, SizeOf(Head));
    S := GetStreamBuffer(Source);
    if S <> nil then
    begin
      if sourcePosition + Head.CompressedSize > sourceSize then
        break;
      inc(S, sourcePosition);
      Source.Seek(Head.CompressedSize, soCurrent);
    end
    else
    begin
      if Head.CompressedSize > length({%H-}tmps) then
        FastNewRawByteString(tmps, Head.CompressedSize);
      S := pointer(tmps);
      if Source.Read(S^, Head.CompressedSize) <> Head.CompressedSize then
        break;
    end;
    inc(sourcePosition, Head.CompressedSize);
    // decompress chunk into Dest
    stored := (Head.CompressedSize = Head.UnCompressedSize) and
              (Head.CompressedHash = Head.UncompressedHash);
    if not stored then
      if AlgoDecompressDestLen(S) <> Head.UnCompressedSize then
        break;
    if AlgoHash(ForceHash32, S, Head.CompressedSize) <> Head.CompressedHash then
      break;
    if IsStreamBuffer(Dest) then
    begin
      Dest.Size := resultSize + Head.UnCompressedSize;    // resize output
      D := PAnsiChar(GetStreamBuffer(Dest)) + resultSize; // in-place decompress
      inc(resultSize, Head.UnCompressedSize);
    end
    else
    begin
      if Head.UnCompressedSize > length({%H-}tmpd) then
        FastNewRawByteString(tmpd, Head.UnCompressedSize);
      D := pointer(tmpd);
    end;
    if stored then
      MoveFast(S^, D^, Head.CompressedSize)
    else if (AlgoDecompress(S, Head.CompressedSize, D) <> Head.UnCompressedSize) or
       (AlgoHash(ForceHash32, D, Head.UnCompressedSize) <> Head.UncompressedHash) then
      break; // invalid decompression
    if D = pointer({%H-}tmpd) then
      Dest.WriteBuffer(D^, Head.UnCompressedSize);
    result := true; // if we reached here, we uncompressed a block
    // try if we have some other pending chunk(s)
    if (sourceSize <> 0) and
       (sourcePosition = sourceSize) then
      break; // end of source with no trailer or next block
    inc(offs, Head.CompressedSize + SizeOf(Head));
    rd := Source.Read(Trailer, SizeOf(Trailer));
    if rd <> SizeOf(Trailer) then
    begin
      if rd <> 0 then
        Source.Position := sourcePosition; // rewind source
      break; // valid uncompressed data with no more chunk
    end;
    if (Trailer.Magic = Magic) and
       (Trailer.HeaderRelativeOffset = offs + SizeOf(Trailer)) then
      break; // we reached the end trailer
    if (Source.Read(PByteArray(@Head)[SizeOf(Trailer)],
         SizeOf(Head) - SizeOf(Trailer)) <> SizeOf(Head) - SizeOf(Trailer)) or
       (Head.Magic <> Magic) then
    begin
      Source.Position := sourcePosition; // rewind source
      break; // valid uncompressed data with no more chunk
    end;
    result := false; // any decompression error on next chunk should be notified
  until false;
end;

function TAlgoCompress.StreamUnCompress(const Source: TFileName;
  Magic: cardinal; ForceHash32: boolean): TMemoryStream;
var
  S: TStream;
begin
  try
    S := FileStreamSequentialRead(Source);
    try
      result := StreamUnCompress(S, Magic, ForceHash32);
    finally
      S.Free;
    end;
  except
    on E: Exception do
      result := nil;
  end;
end;

function TAlgoCompress.StreamComputeLen(P: PAnsiChar; Len: PtrUInt;
  Magic: cardinal): integer;
var
  trailer: PAlgoCompressTrailer;
begin
  if (P = nil) or
     (Len <= SizeOf(TAlgoCompressTrailer)) then
    result := 0
  else
  begin
    if fAlgoHasForcedFormat then
      EnsureAlgoHasNoForcedFormat('StreamComputeLen');
    trailer := PAlgoCompressTrailer(P + Len - SizeOf(TAlgoCompressTrailer));
    if (Magic = trailer^.Magic) and
       (trailer^.HeaderRelativeOffset < Len) and
       (PAlgoCompressHead(P + Len - trailer^.HeaderRelativeOffset)^.Magic = Magic) then
      // trim existing content
      result := Len - trailer^.HeaderRelativeOffset
    else
      result := Len;
  end;
end;

class function TAlgoCompress.FileIsCompressed(const Name: TFileName;
  Magic: cardinal): boolean;
var
  f: THandle;
  l: integer;
  h: TAlgoCompressHead;
begin
  result := false;
  f := FileOpen(Name, fmOpenReadShared);
  if not ValidHandle(f) then
    exit;
  l := FileRead(f, h, SizeOf(h));
  FileClose(f);
  result := (l = SizeOf(h)) and
            (h.Magic = Magic); // only check the magic of first chunk header
end;

function TAlgoCompress.FileCompress(const Source, Dest: TFileName; Magic: cardinal;
  ForceHash32: boolean; ChunkBytes: Int64; WithTrailer: boolean): boolean;
var
  S, D: THandleStream;
begin
  EnsureAlgoHasNoForcedFormat('FileCompres'); // should be overriden
  result := false;
  if (ChunkBytes > 0) and
     FileExists(Source) then
  try
    S := FileStreamSequentialRead(Source);
    try
      DeleteFile(Dest);
      D := TFileStreamEx.Create(Dest, fmCreate);
      try
        StreamCompress(S, D, Magic, ForceHash32, WithTrailer, ChunkBytes);
      finally
        D.Free;
      end;
      result := FileSetDateFrom(Dest, S.Handle);
    finally
      S.Free;
    end;
  except
    on Exception do
      result := false;
  end;
end;

function TAlgoCompress.FileUnCompress(const Source, Dest: TFileName;
  Magic: cardinal; ForceHash32: boolean): boolean;
var
  S, D: THandleStream;
begin
  EnsureAlgoHasNoForcedFormat('FileUnCompress'); // should be overriden
  result := false;
  if FileExists(Source) then
  try
    S := FileStreamSequentialRead(Source);
    try
      DeleteFile(Dest);
      D := TFileStreamEx.Create(Dest, fmCreate);
      try
        if not StreamUnCompress(S, D, Magic, ForceHash32) then
          exit;
      finally
        D.Free;
      end;
      result := FileSetDateFrom(Dest, S.Handle);
    finally
      S.Free;
    end;
  except
    on Exception do
      result := false;
  end;
end;

function TAlgoCompress.DecompressHeader(Comp: PAnsiChar; CompLen: integer;
  Load: TAlgoCompressLoad): integer;
begin
  result := 0;
  if (CompLen <= 9) or
     (Comp = nil) then
    exit;
  EnsureAlgoHasNoForcedFormat('Decompress');
  if ((Load <> aclNoCrcFast) and
      (AlgoHash(0, Comp + 9, CompLen - 9) <> PCardinal(Comp + 5)^)) then
    exit;
  if Comp[4] = COMPRESS_STORED then
  begin
    if PCardinal(Comp)^ = PCardinal(Comp + 5)^ then
      result := CompLen - 9;
  end
  else if Comp[4] = AnsiChar(AlgoID) then
    result := AlgoDecompressDestLen(Comp + 9);
end;

function TAlgoCompress.DecompressBody(Comp, Plain: PAnsiChar;
  CompLen, PlainLen: integer; Load: TAlgoCompressLoad): boolean;
begin
  result := false;
  if PlainLen <= 0 then
    exit;
  if Comp[4] = COMPRESS_STORED then
    MoveFast(Comp[9], Plain[0], PlainLen)
  else if Comp[4] = AnsiChar(AlgoID) then
    case Load of
      aclNormal:
        if (AlgoDecompress(Comp + 9, CompLen - 9, Plain) <> PlainLen) or
           (AlgoHash(0, Plain, PlainLen) <> PCardinal(Comp)^) then
          exit;
      aclSafeSlow:
        if (AlgoDecompressPartial(Comp + 9, CompLen - 9,
            Plain, PlainLen, PlainLen) <> PlainLen) or
           (AlgoHash(0, Plain, PlainLen) <> PCardinal(Comp)^) then
          exit;
      aclNoCrcFast:
        if AlgoDecompress(Comp + 9, CompLen - 9, Plain) <> PlainLen then
          exit;
    end;
  result := true;
end;



{ TAlgoSynLZ }

constructor TAlgoSynLZ.Create;
begin
  fAlgoID := COMPRESS_SYNLZ; // =1
  fAlgoFileExt := '.synlz';
  inherited Create;
end;

function TAlgoSynLZ.AlgoCompress(Plain: pointer; PlainLen: integer; Comp: pointer): integer;
begin
  result := SynLZcompress1(Plain, PlainLen, Comp);
end;

function TAlgoSynLZ.AlgoCompressDestLen(PlainLen: integer): integer;
begin
  result := SynLZcompressdestlen(PlainLen);
end;

function TAlgoSynLZ.AlgoDecompress(Comp: pointer; CompLen: integer; Plain: pointer): integer;
begin
  result := SynLZdecompress1(Comp, CompLen, Plain);
end;

function TAlgoSynLZ.AlgoDecompressDestLen(Comp: pointer): integer;
begin
  result := SynLZdecompressdestlen(Comp);
end;

function TAlgoSynLZ.AlgoDecompressPartial(Comp: pointer; CompLen: integer;
  Partial: pointer; PartialLen, PartialLenMax: integer): integer;
begin
  result := SynLZdecompress1partial(Comp, CompLen, Partial, PartialLen);
end;


{ TAlgoCompressWithNoDestLen }

function TAlgoCompressWithNoDestLen.AlgoCompress(Plain: pointer; PlainLen: integer;
  Comp: pointer): integer;
begin
  Comp := ToVarUInt32(PlainLen, Comp); // e.g. deflate don't store PlainLen
  result := RawProcess(Plain, Comp, PlainLen, AlgoCompressDestLen(PlainLen), 0, doCompress);
  if result > 0 then
    inc(result, ToVarUInt32Length(PlainLen));
end;

function TAlgoCompressWithNoDestLen.AlgoDecompress(Comp: pointer;
  CompLen: integer; Plain: pointer): integer;
var
  start: PAnsiChar;
begin
  start := Comp;
  result := FromVarUInt32(PByte(Comp));
  if RawProcess(Comp, Plain, CompLen + (start - Comp), result, 0, doUnCompress) <> result then
    result := 0;
end;

function TAlgoCompressWithNoDestLen.AlgoDecompressDestLen(Comp: pointer): integer;
begin
  if Comp = nil then
    result := 0
  else
    result := FromVarUInt32(PByte(Comp));
end;

function TAlgoCompressWithNoDestLen.AlgoDecompressPartial(Comp: pointer;
  CompLen: integer; Partial: pointer; PartialLen, PartialLenMax: integer): integer;
var
  start: PAnsiChar;
begin
  start := Comp;
  result := FromVarUInt32(PByte(Comp));
  if PartialLenMax > result then
    PartialLenMax := result;
  result := RawProcess(Comp, Partial, CompLen + (start - Comp),
    PartialLen, PartialLenMax, doUncompressPartial);
end;


{ TAlgoRleLZ }

function TAlgoRleLZ.RawProcess(src, dst: pointer; srcLen, dstLen, dstMax: integer;
  process: TAlgoCompressWithNoDestLenProcess): integer;
var
  tmp: TSynTempBuffer;
  rle: integer;
begin
  case process of
    doCompress:
      begin
        tmp.Init(srcLen - srcLen shr 3); // try to reduce at least by 1/8
        rle := RleCompress(src, tmp.buf, srcLen, tmp.Len);
        if rle < 0 then
          // RLE was not worth it (no 1/8 reduction) -> apply only SynLZ
          PByte(dst)^ := 0
        else
        begin
          // the RLE first pass did reduce the size
          PByte(dst)^ := 1;
          src := tmp.buf;
          srcLen := rle;
        end;
        inc(PByte(dst));
        result := SynLZcompress1(src, srcLen, dst) + 1;
        tmp.Done;
      end;
    doUnCompress:
      begin
        rle := PByte(src)^;
        inc(PByte(src));
        dec(srcLen);
        if rle <> 0 then
        begin
          // process SynLZ with final RLE pass
          tmp.Init(SynLZdecompressdestlen(src));
          rle := SynLZdecompress1(src, srcLen, tmp.buf);
          result := RleUnCompress(tmp.buf, dst, rle);
          tmp.Done;
        end
        else
          // only SynLZ was used
          result := SynLZdecompress1(src, srcLen, dst);
      end;
    doUncompressPartial:
      begin
        rle := PByte(src)^;
        inc(PByte(src));
        dec(srcLen);
        if rle <> 0 then
        begin
          // process full SynLZ with partial RLE pass (not optimal, but works)
          tmp.Init(SynLZdecompressdestlen(src));
          rle := SynLZdecompress1(src, srcLen, tmp.buf);
          result := RleUnCompressPartial(tmp.buf, dst, rle, dstLen);
          tmp.Done;
        end
        else
          // only SynLZ was used
          result := SynLZDecompress1Partial(src, srcLen, dst, dstLen);
      end;
  else
    result := 0;
  end;
end;

constructor TAlgoRleLZ.Create;
begin
  fAlgoID := 7;
  fAlgoFileExt := '.synrlz';
  inherited Create;
end;

function TAlgoRleLZ.AlgoCompressDestLen(PlainLen: integer): integer;
begin
  result := SynLZcompressdestlen(PlainLen);
end;


{ TAlgoRle }

function TAlgoRle.RawProcess(src, dst: pointer; srcLen, dstLen, dstMax: integer;
  process: TAlgoCompressWithNoDestLenProcess): integer;
begin
  case process of
    doCompress:
      begin
        // try to reduce at least by 1/8
        result := RleCompress(src, dst, srcLen, srcLen - srcLen shr 3);
        if result < 0 then
          // RLE was not worth it -> caller would fallback to plain store
          result := dstLen; // to indicate no compression
      end;
    doUnCompress:
      result := RleUnCompress(src, dst, srcLen);
    doUncompressPartial:
      result := RleUnCompressPartial(src, dst, srcLen, dstLen);
  else
    result := 0;
  end;
end;

constructor TAlgoRle.Create;
begin
  fAlgoID := 8;
  fAlgoFileExt := '.synrle';
  inherited Create;
end;

function TAlgoRle.AlgoCompressDestLen(PlainLen: integer): integer;
begin
  result := PlainLen + 16;
end;


function RawByteStringArrayConcat(const Values: array of RawByteString): RawByteString;
var
  i, L: PtrInt;
  P: PAnsiChar;
begin
  L := 0;
  for i := 0 to high(Values) do
    inc(L, length(Values[i]));
  FastNewRawByteString(result{%H-}, L);
  P := pointer(result);
  for i := 0 to high(Values) do
  begin
    L := length(Values[i]);
    MoveFast(pointer(Values[i])^, P^, L);
    inc(P, L);
  end;
end;

procedure RawByteStringToBytes(const buf: RawByteString; out bytes: TBytes);
var
  L: integer;
begin
  L := Length(buf);
  if L <> 0 then
  begin
    SetLength(bytes, L);
    MoveFast(pointer(buf)^, pointer(bytes)^, L);
  end;
end;

procedure BytesToRawByteString(const bytes: TBytes; out buf: RawByteString);
begin
  FastSetRawByteString(buf, pointer(bytes), Length(bytes));
end;

procedure ResourceToRawByteString(const ResName: string; ResType: PChar;
  out buf: RawByteString; Instance: TLibHandle);
var
  res: TExecutableResource;
begin
  if res.Open(ResName, ResType, Instance) then
  begin
    FastSetRawByteString(buf, res.Buffer, res.Size);
    res.Close;
  end;
end;

procedure ResourceSynLZToRawByteString(const ResName: string;
  out buf: RawByteString; Instance: TLibHandle);
var
  res: TExecutableResource;
begin
  if res.Open(ResName, PChar(10), Instance) then
  begin
    AlgoSynLZ.Decompress(res.Buffer, res.Size, buf);
    res.Close;
  end;
end;

{$ifndef PUREMORMOT2}

function StreamSynLZComputeLen(P: PAnsiChar; Len, Magic: cardinal): integer;
begin
  result := AlgoSynLZ.StreamComputeLen(P, Len, Magic);
end;

function StreamSynLZ(Source: TCustomMemoryStream; Dest: TStream; Magic: cardinal): integer;
begin
  result := AlgoSynLZ.StreamCompress(Source, Dest, Magic, {hash32=}true);
end;

function StreamSynLZ(Source: TCustomMemoryStream; const DestFile: TFileName;
  Magic: cardinal): integer;
begin
  result := AlgoSynLZ.StreamCompress(Source, DestFile, Magic, {hash32=}true);
end;

function FileSynLZ(const Source, Dest: TFileName; Magic: cardinal): boolean;
begin
  result := AlgoSynLZ.FileCompress(Source, Dest, Magic, {hash32=}true);
end;

function FileUnSynLZ(const Source, Dest: TFileName; Magic: cardinal): boolean;
begin
  result := AlgoSynLZ.FileUnCompress(Source, Dest, Magic, {hash32=}true);
end;

function FileIsSynLZ(const Name: TFileName; Magic: cardinal): boolean;
begin
  result := AlgoSynLZ.FileIsCompressed(Name, Magic);
end;

function StreamUnSynLZ(const Source: TFileName; Magic: cardinal): TMemoryStream;
begin
  result := AlgoSynLZ.StreamUnCompress(Source, Magic, {hash32=}true);
end;

function StreamUnSynLZ(Source: TStream; Magic: cardinal): TMemoryStream;
begin
  result := AlgoSynLZ.StreamUnCompress(Source, Magic, {hash32=}true);
end;

function SynLZCompress(const Data: RawByteString; CompressionSizeTrigger: integer;
  CheckMagicForCompressed: boolean): RawByteString;
begin
  result := AlgoSynLZ.Compress(pointer(Data), length(Data),
    CompressionSizeTrigger, CheckMagicForCompressed);
end;

procedure SynLZCompress(P: PAnsiChar; PLen: integer; out Result: RawByteString;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean);
begin
  Result := AlgoSynLZ.Compress(P, PLen, CompressionSizeTrigger, CheckMagicForCompressed);
end;

function SynLZCompress(P, Dest: PAnsiChar; PLen, DestLen: integer;
  CompressionSizeTrigger: integer; CheckMagicForCompressed: boolean): integer;
begin
  result := AlgoSynLZ.Compress(P, Dest, PLen, DestLen,
    CompressionSizeTrigger, CheckMagicForCompressed);
end;

function SynLZDecompress(const Data: RawByteString): RawByteString;
begin
  AlgoSynLZ.Decompress(pointer(Data), Length(Data), result);
end;

function SynLZDecompressHeader(P: PAnsiChar; PLen: integer): integer;
begin
  result := AlgoSynLZ.DecompressHeader(P, PLen);
end;

function SynLZDecompressBody(P, Body: PAnsiChar; PLen, BodyLen: integer;
  SafeDecompression: boolean): boolean;
begin
  result := AlgoSynLZ.DecompressBody(P, Body, PLen, BodyLen,
    ALGO_SAFE[SafeDecompression]);
end;

function SynLZDecompressPartial(P, Partial: PAnsiChar; PLen, PartialLen: integer): integer;
begin
  result := AlgoSynLZ.DecompressPartial(P, Partial, PLen, PartialLen, PartialLen);
end;

procedure SynLZDecompress(P: PAnsiChar; PLen: integer; out Result: RawByteString;
  SafeDecompression: boolean);
begin
  AlgoSynLZ.Decompress(P, PLen, Result);
end;

function SynLZDecompress(const Data: RawByteString; out Len: integer;
  var tmp: RawByteString): pointer;
begin
  result := AlgoSynLZ.Decompress(pointer(Data), length(Data), Len, tmp);
end;

function SynLZDecompress(P: PAnsiChar; PLen: integer; out Len: integer;
  var tmp: RawByteString): pointer;
begin
  result := AlgoSynLZ.Decompress(P, PLen, Len, tmp);
end;

function SynLZCompressToBytes(const Data: RawByteString;
  CompressionSizeTrigger: integer): TByteDynArray;
begin
  result := AlgoSynLZ.CompressToBytes(pointer(Data), length(Data),
    CompressionSizeTrigger);
end;

function SynLZCompressToBytes(P: PAnsiChar;
  PLen, CompressionSizeTrigger: integer): TByteDynArray;
begin
  result := AlgoSynLZ.CompressToBytes(P, PLen, CompressionSizeTrigger);
end;

function SynLZDecompress(const Data: TByteDynArray): RawByteString;
begin
  AlgoSynLZ.Decompress(pointer(Data), length(Data), result);
end;

procedure AppendBufferToRawByteString(
  var Content: RawByteString; const Buffer; BufferLen: PtrInt);
begin
  Append(Content, @Buffer, BufferLen);
end;

procedure AppendBufferToRawByteString(var Content: RawByteString; const Buffer: RawByteString);
begin
  Append(Content, Buffer);
end;

procedure AppendToRawUtf8(var Text: RawUtf8; const After: RawByteString);
begin
  Append(Text, After);
end;

procedure AppendBufferToRawUtf8(var Text: RawUtf8; Buffer: PUtf8Char; BufferLen: PtrInt);
begin
  Append(Text, Buffer, BufferLen);
end;

procedure AppendCharToRawUtf8(var Text: RawUtf8; Ch: AnsiChar);
begin
  Append(Text, @Ch, 1);
end;

procedure AppendToRawUtf8(var Text: RawUtf8; const After1, After2: RawByteString);
begin
  Append(Text, After1, After2);
end;

{$endif PUREMORMOT2}


{ ************ Base64, Base64Uri, Base58 and Baudot Encoding / Decoding }

type
  TBase64Enc = array[0..63] of AnsiChar;
  PBase64Enc = ^TBase64Enc;
  TBase64Dec = array[AnsiChar] of shortint;
  PBase64Dec = ^TBase64Dec;

const
  b64enc: TBase64Enc =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  b64Urienc: TBase64Enc =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';

var
  /// a conversion table from Base64 text into binary data
  // - used by Base64ToBin/IsBase64 functions
  // - has -1 (255) for invalid char, -2 (254) for '=', 0..63 for valid char
  ConvertBase64ToBin, ConvertBase64UriToBin: TBase64Dec;


{ --------- Base64 encoding/decoding }

function Base64AnyDecode(const decode: TBase64Dec; sp, rp: PAnsiChar; len: PtrInt): boolean;
var
  c, ch: PtrInt;
begin // FPC emits suboptimal asm but Base64DecodeMainAvx2() will run on server
  result := false;
  while len >= 4 do
  begin
    c := decode[sp[0]];
    if c < 0 then
      exit;
    c := c shl 6;
    ch := decode[sp[1]];
    if ch < 0 then
      exit;
    c := (c or ch) shl 6;
    ch := decode[sp[2]];
    if ch < 0 then
      exit;
    c := (c or ch) shl 6;
    ch := decode[sp[3]];
    if ch < 0 then
      exit;
    c := c or ch;
    rp[2] := AnsiChar(c);
    c := c shr 8;
    rp[1] := AnsiChar(c);
    c := c shr 8;
    rp[0] := AnsiChar(c);
    dec(len, 4);
    inc(rp, 3);
    inc(sp, 4);
  end;
  if len >= 2 then
  begin
    c := decode[sp[0]];
    if c < 0 then
      exit;
    c := c shl 6;
    ch := decode[sp[1]];
    if ch < 0 then
      exit;
    if len = 2 then
      rp[0] := AnsiChar((c or ch) shr 4)
    else
    begin
      c := (c or ch) shl 6;
      ch := decode[sp[2]];
      if ch < 0 then
        exit;
      c := (c or ch) shr 2;
      rp[1] := AnsiChar(c);
      rp[0] := AnsiChar(c shr 8);
    end;
  end;
  result := true;
end;

function Base64DecodeMainPas(sp, rp: PAnsiChar; len: PtrInt): boolean;
begin
  result := Base64AnyDecode(ConvertBase64ToBin, sp, rp, len);
end;

function Base64Decode(sp, rp: PAnsiChar; len: PtrInt): boolean;
  {$ifdef FPC} inline;{$endif}
var
  tab: PBase64Dec; // use local register
begin
  tab := @ConvertBase64ToBin;
  len := len shl 2; // len was the number of 4 chars chunks in sp
  if (len > 0) and
     (tab[sp[len - 2]] >= 0) then
    if tab[sp[len - 1]] >= 0 then
      // no trim
    else
      dec(len)
  else
    dec(len, 2); // Base64AnyDecode() algorithm ignores the trailing '='
  {$ifdef ASMX64AVXNOCONST}
  result := Base64DecodeMain(sp, rp, len); // may be Base64DecodeMainAvx2
  {$else}
  result := Base64AnyDecode(tab^, sp, rp, len);
  {$endif ASMX64AVXNOCONST}
end;

procedure Base64EncodeLoop(rp, sp: PAnsiChar; len: cardinal; enc: PBase64Enc);
  {$ifdef HASINLINE} inline; {$endif}
var
  c: cardinal;
begin // this loop is faster than mORMot 1 manual x86 asm, even on Delphi 7
  repeat
    c := (ord(sp[0]) shl 16) or (ord(sp[1]) shl 8) or ord(sp[2]);
    rp[0] := enc[(c shr 18) and $3f];
    rp[1] := enc[(c shr 12) and $3f];
    rp[2] := enc[(c shr 6) and $3f];
    rp[3] := enc[c and $3f];
    inc(rp, 4);
    inc(sp, 3);
    dec(len, 3)
  until len = 0;
end;

{$ifdef ASMX64AVXNOCONST} // AVX2 ASM not available on Delphi < 11
function Base64EncodeMainAvx2(rp, sp: PAnsiChar; len: cardinal): integer;
var
  blen: PtrUInt;
begin
  result := len div 3;
  if result = 0 then
    exit;
  blen := result * 3;
  Base64EncodeAvx2(sp, blen, rp); // handle >=32 bytes of data using AVX2
  Base64EncodeLoop(rp, sp, blen, @b64enc); // good inlining code generation
end;

function Base64DecodeMainAvx2(sp, rp: PAnsiChar; len: PtrInt): boolean;
begin
  Base64DecodeAvx2(sp, len, rp);
  // on error, AVX2 code let sp point to the faulty input so result=false
  result := Base64AnyDecode(ConvertBase64ToBin, sp, rp, len);
end;
{$endif ASMX64AVXNOCONST}

function Base64EncodeMainPas(rp, sp: PAnsiChar; len: cardinal): integer;
var
  enc: PBase64Enc; // use local register
begin
  enc := @b64enc;
  result := len div 3;
  if result <> 0 then
    Base64EncodeLoop(rp, sp, result * 3, enc);
end;

procedure Base64EncodeTrailing(rp, sp: PAnsiChar; len: cardinal);
var
  c: cardinal;
  enc: PBase64Enc; // use local register
begin
  enc := @b64enc;
  case len of
    1:
      begin
        c := ord(sp[0]) shl 4;
        rp[0] := enc[(c shr 6) and $3f];
        rp[1] := enc[c and $3f];
        PWord(rp + 2)^ := ord('=') + ord('=') shl 8;
      end;
    2:
      begin
        c := (ord(sp[0]) shl 10) or (ord(sp[1]) shl 2);
        rp[0] := enc[(c shr 12) and $3f];
        rp[1] := enc[(c shr 6) and $3f];
        rp[2] := enc[c and $3f];
        rp[3] := '=';
      end;
  end;
end;

procedure Base64Encode(rp, sp: PAnsiChar; len: cardinal);
var
  main: cardinal;
begin
  main := Base64EncodeMain(rp, sp, len); // may use AVX2 on FPC x86_64
  Base64EncodeTrailing(rp + main * 4, sp + main * 3, len - main * 3);
end;

function BinToBase64Length(len: PtrUInt): PtrUInt;
begin
  result := ((len + 2) div 3) * 4;
end;

function BinToBase64(const s: RawByteString): RawUtf8;
var
  len: integer;
begin
  result := '';
  len := length(s);
  if len = 0 then
    exit;
  FastSetString(result, BinToBase64Length(len));
  Base64Encode(pointer(result), pointer(s), len);
end;

function BinToBase64Short(Bin: PAnsiChar; BinBytes: integer): ShortString;
var
  destlen: integer;
begin
  result := '';
  if BinBytes = 0 then
    exit;
  destlen := BinToBase64Length(BinBytes);
  if destlen > 255 then
    exit; // avoid buffer overflow
  result[0] := AnsiChar(destlen);
  Base64Encode(@result[1], Bin, BinBytes);
end;

function BinToBase64Line(sp: PAnsiChar; len: PtrUInt; const Prefix, Suffix: RawUtf8): RawUtf8;
const
  PERLINE = (64 * 3) div 4; // = 48 bytes for 64 chars per line
var
  p: PAnsiChar;
  outlen, last: PtrUInt;
begin
  outlen := BinToBase64Length(len);
  inc(outlen, 2 * (outlen shr 6) + 2); // one CRLF per line
  FastSetString(result{%H-}, PtrInt(outlen) + length(Prefix) + length(Suffix));
  p := pointer(result);
  if Prefix <> '' then
  begin
    MoveFast(pointer(Prefix)^, p^, PStrLen(PAnsiChar(pointer(Prefix)) - _STRLEN)^);
    inc(p, PStrLen(PAnsiChar(pointer(Prefix)) - _STRLEN)^);
  end;
  while len >= PERLINE do
  begin
    Base64EncodeLoop(p, sp, PERLINE, @b64enc); // better inlining than AVX2 here
    inc(sp, PERLINE);
    PWord(p + 64)^ := $0a0d; // CR + LF on all systems for safety
    inc(p, 66);
    dec(len, PERLINE);
  end;
  if len > 0 then
  begin
    last := len div 3;
    if last <> 0 then
      Base64EncodeLoop(p, sp, last * 3, @b64enc);
    inc(p, last * 4);
    last := last * 3;
    inc(sp, last);
    dec(len, last);
    if len <> 0 then
    begin
      Base64EncodeTrailing(p, sp, len); // 1/2 bytes as 4 chars with trailing =
      inc(p, 4);
    end;
    PWord(p)^ := $0a0d;
    inc(p, 2);
  end;
  if Suffix <> '' then
  begin
    MoveFast(pointer(Suffix)^, p^, PStrLen(PAnsiChar(pointer(Suffix)) - _STRLEN)^);
    inc(p, PStrLen(PAnsiChar(pointer(Suffix)) - _STRLEN)^);
  end;
  FakeLength(result, pointer(p));
end;

function BinToBase64Short(const s: RawByteString): ShortString;
begin
  result := BinToBase64Short(pointer(s), length(s));
end;

function BinToBase64(Bin: PAnsiChar; BinBytes: integer): RawUtf8;
begin
  result := '';
  if BinBytes = 0 then
    exit;
  FastSetString(result, BinToBase64Length(BinBytes));
  Base64Encode(pointer(result), Bin, BinBytes);
end;

function BinToBase64(const data, Prefix, Suffix: RawByteString; WithMagic: boolean): RawUtf8;
var
  lendata, lenprefix, lensuffix, len: integer;
  res: PByteArray absolute result;
begin
  result := '';
  lendata := length(data);
  lenprefix := length(Prefix);
  lensuffix := length(Suffix);
  if lendata + lenprefix + lensuffix = 0 then
    exit;
  len := ((lendata + 2) div 3) * 4 + lenprefix + lensuffix;
  if WithMagic then
    inc(len, 3);
  FastSetString(result, len);
  if lenprefix > 0 then
    MoveFast(pointer(Prefix)^, res^, lenprefix);
  if WithMagic then
  begin
    PInteger(@res[lenprefix])^ := JSON_BASE64_MAGIC_C;
    inc(lenprefix, 3);
  end;
  Base64Encode(@res[lenprefix], pointer(data), lendata);
  if lensuffix > 0 then
    MoveFast(pointer(Suffix)^, res[len - lensuffix], lensuffix);
end;

function BinToBase64WithMagic(const data: RawByteString): RawUtf8;
begin
  BinToBase64WithMagic(pointer(data), length(data), result{%H-});
end;

function BinToBase64WithMagic(Data: pointer; DataLen: integer): RawUtf8;
begin
  BinToBase64WithMagic(Data, DataLen, result{%H-});
end;

procedure BinToBase64WithMagic(Data: pointer; DataLen: integer;
  var Result: RawUtf8);
begin
  Result := '';
  if DataLen <= 0 then
    exit;
  FastSetString(Result, ((DataLen + 2) div 3) * 4 + 3);
  PInteger(pointer(Result))^ := JSON_BASE64_MAGIC_C;
  Base64Encode(PAnsiChar(pointer(Result)) + 3, Data, DataLen);
end;

function IsBase64Internal(sp: PAnsiChar; len: PtrInt; dec: PBase64Dec): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (len = 0) or
     (len and 3 <> 0) then
    exit;
  for i := 0 to len - 5 do
    if dec[sp[i]] < 0 then
      exit;
  inc(sp, len - 4);
  if (dec[sp[0]] = -1) or
     (dec[sp[1]] = -1) or
     (dec[sp[2]] = -1) or
     (dec[sp[3]] = -1) then
    exit;
  result := true; // layout seems correct
end;

function IsBase64(sp: PAnsiChar; len: PtrInt): boolean;
begin
  result := IsBase64Internal(sp, len, @ConvertBase64ToBin);
end;

function IsBase64(const s: RawByteString): boolean;
begin
  result := IsBase64Internal(pointer(s), length(s), @ConvertBase64ToBin);
end;

function Base64Length(sp: PAnsiChar; len: PtrInt; dec: PBase64Dec): PtrInt;
  {$ifdef HASINLINE} inline; {$endif}
begin
  result := 0;
  if (len = 0) or
     (len and 3 <> 0) then
    exit;
  if dec[sp[len - 2]] >= 0 then
    if dec[sp[len - 1]] >= 0 then
      result := 0
    else
      result := 1
  else
    result := 2;
  result := (len shr 2) * 3 - result;
end;

function Base64ToBinLengthSafe(sp: PAnsiChar; len: PtrInt): PtrInt;
var
  dec: PBase64Dec;
begin
  dec := @ConvertBase64ToBin;
  if IsBase64Internal(sp, len, dec) then
    result := Base64Length(sp, len, dec)
  else
    result := 0;
end;

function Base64ToBinLength(sp: PAnsiChar; len: PtrInt): PtrInt;
begin
  result := Base64Length(sp, len, @ConvertBase64ToBin);
end;

function Base64ToBin(const s: RawByteString): RawByteString;
begin
  Base64ToBinSafe(pointer(s), length(s), result{%H-});
end;

function Base64ToBin(sp: PAnsiChar; len: PtrInt): RawByteString;
begin
  Base64ToBinSafe(sp, len, result{%H-});
end;

function Base64ToBin(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean;
begin
  result := Base64ToBinSafe(sp, len, data);
end;

function Base64ToBinSafe(const s: RawByteString): RawByteString;
begin
  if s = '' then
    result := ''
  else
    Base64ToBinSafe(pointer(s), length(s), result);
end;

function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt): RawByteString;
begin
  Base64ToBinSafe(sp, len, result{%H-});
end;

function Base64LengthAdjust(sp: PAnsiChar; var len: PtrInt): PtrInt;
  {$ifdef HASINLINE} inline; {$endif}
var
  tab: PBase64Dec;
begin
  result := len; // for better code generation
  if (result = 0) or
     (result and 3 <> 0) then
  begin
    result := 0;
    exit;
  end;
  tab := @ConvertBase64ToBin;
  if tab[sp[result - 2]] >= 0 then
    if tab[sp[result - 1]] >= 0 then
      result := 0
    else
      result := 1
  else
    result := 2;
  sp := pointer(result);
  result := (len shr 2) * 3 - result;
  dec(len, PtrInt(sp)); // adjust for Base64AnyDecode() algorithm
end;

function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt; var data: RawByteString): boolean;
var
  resultLen: PtrInt;
begin
  result := false;
  resultLen := Base64LengthAdjust(sp, len);
  if resultLen <> 0 then
  begin
    FastNewRawByteString(data, resultLen);
    result := Base64DecodeMain(sp, pointer(data), len); // may use AVX2
  end;
  if not result then
    data := '';
end;

function Base64ToBinSafe(sp: PAnsiChar; len: PtrInt; out data: TBytes): boolean;
var
  resultLen: PtrInt;
begin
  result := false;
  resultLen := Base64LengthAdjust(sp, len);
  if resultLen = 0 then
    exit;
  SetLength(data, resultLen);
  result := Base64DecodeMain(sp, pointer(data), len); // may use AVX2
  if not result then
    data := nil;
end;

function Base64ToBin(sp: PAnsiChar; len: PtrInt; var blob: TSynTempBuffer): boolean;
begin
  blob.Init(Base64ToBinLength(sp, len));
  result := (blob.len > 0) and
            Base64Decode(sp, blob.buf, len shr 2); // may use AVX2
end;

function Base64ToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt
  {$ifndef PUREMORMOT2} ; nofullcheck: boolean {$endif}): boolean;
begin
  // nofullcheck is just ignored and deprecated
  result := (bin <> nil) and
            (Base64ToBinLength(base64, base64len) = binlen) and
            Base64Decode(base64, bin, base64len shr 2); // may use AVX2
end;

function Base64ToBinTrim(const s: RawByteString): RawByteString;
begin
  result := Base64ToBin(TrimControlChars(s));
end;

function Base64ToBin(const base64: RawByteString; bin: PAnsiChar; binlen: PtrInt
  {$ifndef PUREMORMOT2} ; nofullcheck: boolean {$endif}): boolean;
begin
  result := Base64ToBin(pointer(base64), bin, length(base64), binlen);
end;


{ --------- Base64 URI encoding/decoding }

procedure Base64uriEncode(rp, sp: PAnsiChar; len: cardinal);
var
  main, c: cardinal;
  enc: PBase64Enc; // faster especially on x86_64 and PIC
begin
  enc := @b64Urienc;
  main := len div 3;
  if main <> 0 then
  begin
    dec(len, main * 3); // fast modulo
    repeat // inlined Base64EncodeLoop()
      c := (ord(sp[0]) shl 16) or (ord(sp[1]) shl 8) or ord(sp[2]);
      rp[0] := enc[(c shr 18) and $3f];
      rp[1] := enc[(c shr 12) and $3f];
      rp[2] := enc[(c shr 6) and $3f];
      rp[3] := enc[c and $3f];
      inc(rp, 4);
      inc(sp, 3);
      dec(main)
    until main = 0;
  end;
  case len of
    1:
      begin
        c := ord(sp[0]) shl 4;
        rp[0] := enc[(c shr 6) and $3f];
        rp[1] := enc[c and $3f];
      end;
    2:
      begin
        c := (ord(sp[0]) shl 10) or (ord(sp[1]) shl 2);
        rp[0] := enc[(c shr 12) and $3f];
        rp[1] := enc[(c shr 6) and $3f];
        rp[2] := enc[c and $3f];
      end;
  end;
end;

function BinToBase64uriLength(len: PtrUInt): PtrUInt;
begin
  result := (len div 3) * 4;
  case len - (result shr 2) * 3 of // fast len mod 3
    1:
      inc(result, 2);
    2:
      inc(result, 3);
  end;
end;

function BinToBase64uri(const s: RawByteString): RawUtf8;
var
  len: integer;
begin
  result := '';
  len := length(s);
  if len = 0 then
    exit;
  FastSetString(result, BinToBase64uriLength(len));
  Base64uriEncode(pointer(result), pointer(s), len);
end;

function BinToBase64uri(Bin: PAnsiChar; BinBytes: integer): RawUtf8;
begin
  result := '';
  if BinBytes <= 0 then
    exit;
  FastSetString(result, BinToBase64uriLength(BinBytes));
  Base64uriEncode(pointer(result), Bin, BinBytes);
end;

function BinToBase64uriShort(Bin: PAnsiChar; BinBytes: integer): ShortString;
var
  len: integer;
begin
  result := '';
  if BinBytes <= 0 then
    exit;
  len := BinToBase64uriLength(BinBytes);
  if len > 255 then
    exit;
  byte(result[0]) := len;
  Base64uriEncode(@result[1], Bin, BinBytes);
end;

function Base64uriToBinLength(len: PtrInt): PtrInt;
begin
  if len = 0 then
    result := 0
  else
  begin
    result := (len shr 2) * 3;
    case len and 3 of
      1:
        result := 0;
      2:
        inc(result, 1);
      3:
        inc(result, 2);
    end;
  end;
end;

function Base64uriDecode(sp, rp: PAnsiChar; len: PtrInt): boolean;
begin
  result := Base64AnyDecode(ConvertBase64UriToBin, sp, rp, len);
end;

function Base64uriToBin(sp: PAnsiChar; len: PtrInt): RawByteString;
begin
  Base64uriToBin(sp, len, result{%H-});
end;

function Base64uriToBin(const s: RawByteString): RawByteString;
begin
  Base64uriToBin(pointer(s), length(s), result{%H-});
end;

function Base64uriToBin(sp: PAnsiChar; len: PtrInt; var bin: RawByteString): boolean;
var
  resultLen: PtrInt;
begin
  result := false;
  resultLen := Base64uriToBinLength(len);
  if resultLen <> 0 then
  begin
    FastNewRawByteString(bin, resultLen);
    result := Base64AnyDecode(ConvertBase64UriToBin, sp, pointer(bin), len);
  end;
  if not result then
    bin := '';
end;

function Base64uriToBin(sp: PAnsiChar; len: PtrInt; var temp: TSynTempBuffer): boolean;
begin
  temp.Init(Base64uriToBinLength(len));
  result := (temp.len > 0) and
            Base64AnyDecode(ConvertBase64UriToBin, sp, temp.buf, len);
end;

function Base64uriToBin(const base64: RawByteString; bin: PAnsiChar; binlen: PtrInt): boolean;
begin
  result := Base64uriToBin(pointer(base64), bin, length(base64), binlen);
end;

function Base64uriToBin(base64, bin: PAnsiChar; base64len, binlen: PtrInt): boolean;
var
  resultLen: PtrInt;
begin
  resultLen := Base64uriToBinLength(base64len);
  result := (resultLen = binlen) and
            Base64AnyDecode(ConvertBase64UriToBin, base64, bin, base64len);
end;

procedure Base64ToUri(var base64: RawUtf8);
var
  P: PUtf8Char;
begin
  P := UniqueRawUtf8(base64);
  if P <> nil then
    repeat
      case P^ of
        #0:
          break;
        '+':
          P^ := '-';
        '/':
          P^ := '_';
        '=':
          begin
            // trim unsignificant trailing '=' characters
            FakeLength(base64, P - pointer(base64));
            break;
          end;
      end;
      inc(P);
    until false;
end;

procedure Base64MagicDecode(var ParamValue: RawUtf8);
var
  tmp: RawUtf8;
begin
  tmp := ParamValue;
  if not Base64ToBinSafe(PAnsiChar(pointer(tmp)) + 3, length(tmp) - 3,
          RawByteString(ParamValue)) then
    ParamValue := '';
end;

function Base64MagicCheckAndDecode(Value: PUtf8Char; var Blob: RawByteString): boolean;
var
  ValueLen: integer;
begin
  if (Value = nil) or
     (Value[0] = #0) or
     (Value[1] = #0) or
     (Value[2] = #0) or
     (PCardinal(Value)^ and $ffffff <> JSON_BASE64_MAGIC_C) then
    result := false
  else
  begin
    ValueLen := StrLen(Value) - 3;
    if ValueLen > 0 then
      result := Base64ToBinSafe(PAnsiChar(Value) + 3, ValueLen, Blob)
    else
      result := false;
  end;
end;

function Base64MagicCheckAndDecode(Value: PUtf8Char; var Blob: TSynTempBuffer;
  ValueLen: integer): boolean;
begin
  if (Value = nil) or
     (Value[0] = #0) or
     (Value[1] = #0) or
     (Value[2] = #0) or
     (PCardinal(Value)^ and $ffffff <> JSON_BASE64_MAGIC_C) then
    result := false
  else
  begin
    if ValueLen = 0 then
      ValueLen := StrLen(Value);
    dec(ValueLen, 3);
    if ValueLen > 0 then
      result := Base64ToBin(PAnsiChar(Value) + 3, ValueLen, Blob)
    else
      result := false;
  end;
end;

function Base64MagicTryAndDecode(Value: PUtf8Char; ValueLen: integer;
  var Blob: RawByteString): boolean;
begin
  if (ValueLen >= 4) and
     (PCardinal(Value)^ and $ffffff = JSON_BASE64_MAGIC_C) then
  begin
    inc(Value, 3); // just ignore the magic trailer
    dec(ValueLen, 3);
  end;
  result := Base64ToBinSafe(PAnsiChar(Value), ValueLen, Blob);
end;

function Base64MagicCheckAndDecode(Value: PUtf8Char; ValueLen: integer;
  var Blob: RawByteString): boolean;
begin
  if (ValueLen < 4) or
     (PCardinal(Value)^ and $ffffff <> JSON_BASE64_MAGIC_C) then
    result := false
  else
    result := Base64ToBinSafe(PAnsiChar(Value) + 3, ValueLen - 3, Blob);
end;


{ --------- Base58 encoding/decoding }

type
  TBase58Enc = array[0..57] of AnsiChar;
  PBase58Enc = ^TBase58Enc;
  TBase58Dec = array[AnsiChar] of shortint;
  PBase58Dec = ^TBase58Dec;

const
  b58enc: TBase58Enc =
    '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';
var
  /// a conversion table from Base58 text into binary data
  ConvertBase58ToBin: TBase58Dec;

function BinToBase58(Bin: PAnsiChar; BinLen: integer; var Dest: TSynTempBuffer): integer;
var
  P, PEnd, P2: PByte;
  len, c, carry, i: cardinal;
begin
  result := 0;
  if (Bin = nil) or
     (BinLen <= 0) then
  begin
    Dest.buf := nil;
    exit;
  end;
  while Bin^ = #0 do
  begin
    inc(result); // any leading zero is stored as '1' -> result = num of zeros
    inc(Bin);
    dec(BinLen);
    if BinLen = 0 then
      break;
  end;
  P := Dest.InitZero(result + integer(cardinal(BinLen * 138) div 100));
  PEnd := @PByteArray(P)[Dest.len];
  if result <> 0 then
  begin
    FillcharFast(P^, result, ord('1'));
    inc(P, result);
  end;
  if BinLen = 0 then
    exit;
  len := 0;
  repeat
    // this loop is O(n2) by definition so BinLen should remain small
    i := 0;
    P2 := PEnd;
    carry := PByte(Bin)^;
    while (PtrUInt(P2) >= PtrUInt(P)) and
          ((carry <> 0) or
           (i < len)) do
    begin
      inc(carry, cardinal(P2^) shl 8);
      c := carry div 58;   // FPC will use fast reciprocal mul by 0x8d3dcb09
      dec(carry, c * 58);
      P2^ := carry;        // P2^ := carry mod 58
      carry := c;
      dec(P2);
      inc(i);
    end;
    len := i;
    inc(Bin);
    dec(BinLen);
  until BinLen = 0;
  inc(PEnd);
  P2 := P;
  while (P2 <> PEnd) and
        (P2^ = 0) do
    inc(P2);
  inc(result, PtrUInt(PEnd) - PtrUInt(P2));
  while P2 <> PEnd do
  begin
    P^ := ord(b58enc[P2^]);
    inc(P);
    inc(P2);
  end;
end;

function BinToBase58(Bin: PAnsiChar; BinLen: integer): RawUtf8;
var
  temp: TSynTempBuffer;
  len: integer;
begin
  len := BinToBase58(Bin, BinLen, temp);
  FastSetString(result{%H-}, temp.buf, len);
  temp.Done;
end;

function BinToBase58(const Bin: RawByteString): RawUtf8;
begin
  result := BinToBase58(pointer(Bin), length(Bin));
end;

function Base58ToBin(B58: PAnsiChar; B58Len: integer;
  var Dest: TSynTempBuffer): integer;
var
  P: PByteArray;
  PEnd, P2: PByte;
  zeros, carry: integer;
begin
  result := 0; // means void or error
  if (B58 = nil) or
     (B58Len <= 0) then
  begin
    Dest.buf := nil;
    exit;
  end;
  zeros := 0;
  while B58^ = '1' do
  begin
    inc(zeros);
    inc(B58);
    dec(B58Len);
    if B58Len = 0 then
      break;
  end;
  P := Dest.InitZero(zeros + integer(cardinal(B58Len * 733) div 1000));
  PEnd := @P[Dest.len];
  if B58Len = 0 then
  begin
    result := zeros;
    exit;
  end;
  repeat
    // this loop is O(n2) by definition so B58Len should remain small
    carry := ConvertBase58ToBin[B58^];
    inc(B58);
    if carry < 0 then
      exit; // invalid input
    P2 := PEnd;
    while PtrUInt(P2) >= PtrUInt(P) do
    begin
      inc(carry, 58 * P2^);
      P2^ := carry;
      carry := carry shr 8;
      dec(P2);
    end;
    dec(B58Len);
  until B58Len = 0;
  P2 := pointer(P);
  while (P2 <> PEnd) and
        (P2^ = 0) do
    inc(P2);
  result := PtrUInt(PEnd) - PtrUInt(P2) + 1;
  if result + zeros <> Dest.len + 1 then
    MoveFast(P[PtrUInt(P2) - PtrUInt(P)], P[zeros], result);
  inc(result, zeros);
end;

function Base58ToBin(B58: PAnsiChar; B58Len: integer): RawByteString;
var
  temp: TSynTempBuffer;
  len: integer;
begin
  len := Base58ToBin(B58, B58Len, temp);
  FastSetRawByteString(result{%H-}, temp.buf, len);
  temp.Done;
end;

function Base58ToBin(const base58: RawUtf8): RawByteString;
begin
  result := Base58ToBin(pointer(base58), length(base58));
end;

function BinToBase32Length(BinLen: cardinal): cardinal;
begin
  if integer(BinLen) <= 0 then
    result := 0
  else
    result := ((BinLen div 5) + cardinal(ord((BinLen mod 5) <> 0))) shl 3;
end;

procedure BinToBase32(Bin: PByteArray; Dest: PAnsiChar; BinLen: PtrInt; b32enc: PAnsiChar);
const
  b32pad: array[0..4] of byte = (8, 6, 4, 3, 1);
var
  c, d: PtrInt; // optimized for x86_64 and ARM/AARCH64
begin
  while BinLen >= 5 do // handle whole blocks of 5 input bytes as 8 text chars
  begin
    c := Bin[0];
    d := Bin[1];
    Dest[0] := b32enc[(c and $f8) shr 3];
    Dest[1] := b32enc[((d and $c0) shr 6) or ((c and $07) shl 2)];
    Dest[2] := b32enc[(d and $3e) shr 1];
    c := Bin[2];
    Dest[3] := b32enc[((c and $f0) shr 4) or ((d and $01) shl 4)];
    d := Bin[3];
    Dest[4] := b32enc[((d and $80) shr 7) or ((c and $0f) shl 1)];
    Dest[5] := b32enc[(d and $7c) shr 2];
    c := Bin[4];
    Dest[6] := b32enc[((c and $e0) shr 5) or ((d and $03) shl 3)];
    Dest[7] := b32enc[c and $1f];
    dec(BinLen, 5);
    if BinLen = 0 then
      exit;
    Bin := @Bin[5];
    inc(Dest, 8);
  end;
  repeat // remaining 1..4 bytes in a "repeat until true" block to avoid goto
    c := Bin[0];
    Dest[0] := b32enc[(c and $f8) shr 3];
    c := (c and $07) shl 2;
    if BinLen < 2 then
    begin
      Dest[1] := b32enc[c];
      break;
    end;
    d := Bin[1];
    Dest[1] := b32enc[((d and $c0) shr 6) or c];
    Dest[2] := b32enc[(d and $3e) shr 1];
    c := (d and $01) shl 4;
    if BinLen < 3 then
    begin
      Dest[3] := b32enc[c];
      break;
    end;
    d := Bin[2];
    Dest[3] := b32enc[((d and $f0) shr 4) or c];
    c := (d and $0f) shl 1;
    if BinLen < 4 then
    begin
      Dest[4] := b32enc[c];
      break;
    end;
    d := Bin[3];
    Dest[4] := b32enc[((d and $80) shr 7) or c];
    Dest[5] := b32enc[(d and $7c) shr 2];
    Dest[6] := b32enc[(d and $03) shl 3];
  until true;
  BinLen := b32pad[BinLen];
  inc(Dest, 7 - BinLen);
  repeat
    Dest[BinLen] := '='; // padding
    dec(BinLen);
  until BinLen = 0;
end;

const
  b32enc: array[0..31] of AnsiChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
var
  ConvertBase32ToBin: TBase64Dec;

function BinToBase32(Bin: PAnsiChar; BinLen: PtrInt): RawUtf8;
begin
  FastSetString(result, BinToBase32Length(BinLen));
  if result <> '' then
    BinToBase32(pointer(Bin), pointer(result), BinLen, @b32enc);
end;

function BinToBase32(const Bin: RawByteString): RawUtf8;
begin
  result := BinToBase32(pointer(Bin), length(Bin));
end;

function Base32Decode(decode: PBase64Dec; sp: PAnsiChar; rp: PByteArray;
  len: PtrInt): pointer;
var
  c, d, e: integer;
begin
  result := nil;
  while len > 8 do  // handle whole blocks of 8 input text chars into 5 bytes
  begin
    c := decode[sp[0]];
    d := decode[sp[1]];
    if (c < 0) or
       (d < 0) then
      exit;
    rp[0] := ((c and $1f) shl 3) or ((d and $1c) shr 2);
    c := decode[sp[2]];
    e := decode[sp[3]];
    if (c < 0) or
       (e < 0) then
      exit;
    rp[1] := ((d and $03) shl 6) or ((c and $1f) shl 1) or ((e and $10) shr 4);
    c := decode[sp[4]];
    if c < 0 then
      exit;
    rp[2] := ((e and $0f) shl 4) or ((c and $1e) shr 1);
    d := decode[sp[5]];
    e := decode[sp[6]];
    if (d < 0) or
       (e < 0) then
      exit;
    rp[3] := ((c and $01) shl 7) or ((d and $1f) shl 2) or ((e and $18) shr 3);
    c := decode[sp[7]];
    if c < 0 then
      exit;
    rp[4] := ((e and $07) shl 5) or (c and $1f);
    rp := @rp[5];
    dec(len, 8);
    inc(sp, 8);
  end;
  c := decode[sp[0]]; // decode trailing text chars into 1..4 bytes
  d := decode[sp[1]];
  if (c < 0) or
     (d < 0) then
    exit;
  rp[0] := ((c and $1f) shl 3) or ((d and $1c) shr 2);
  rp := @rp[1];
  repeat
    if sp[2] = '=' then
      break;
    c := decode[sp[2]];
    e := decode[sp[3]];
    if (c < 0) or
       (e < 0) then
      exit;
    rp[0] := ((d and $03) shl 6) or ((c and $1f) shl 1) or ((e and $10) shr 4);
    rp := @rp[1];
    if sp[4] = '=' then
      break;
    c := decode[sp[4]];
    if c < 0 then
      exit;
    rp[0] := ((e and $0f) shl 4) or ((c and $1e) shr 1);
    rp := @rp[1];
    if sp[5] = '=' then
      break;
    d := decode[sp[5]];
    e := decode[sp[6]];
    if (d < 0) or
       (e < 0) then
      exit;
    rp[0] := ((c and $01) shl 7) or ((d and $1f) shl 2) or ((e and $18) shr 3);
    rp := @rp[1];
    if sp[7] = '=' then
      break;
    c := decode[sp[7]];
    if c < 0 then
      exit;
    rp[0] := ((e and $07) shl 5) or (c and $1f);
    rp := @rp[1];
  until true;
  result := rp;
end;

function Base32ToBin(B32: PAnsiChar; B32Len: integer): RawByteString;
var
  p: PAnsiChar;
begin
  if (B32Len > 0) and
     ((B32Len and 7) = 0) then
  begin
    FastNewRawByteString(result, (B32Len shr 3) * 5);
    p := Base32Decode(@ConvertBase32ToBin, B32, pointer(result), B32Len);
    if p <> nil then
    begin
      FakeLength(result, p - pointer(result));
      exit;
    end;
  end;
  result := '';
end;

function Base32ToBin(const base32: RawUtf8): RawByteString;
begin
  result := Base32ToBin(pointer(base32), length(base32));
end;

function BlobToRawBlob(P: PUtf8Char; Len: integer): RawBlob;
begin
  BlobToRawBlob(P, result{%H-}, Len);
end;

procedure BlobToRawBlob(P: PUtf8Char; var result: RawBlob; Len: integer);
var
  LenHex: integer;
begin
  result := '';
  if Len = 0 then
    Len := StrLen(P);
  if Len = 0 then
    exit;
  if Len >= 3 then
    if (P[0] in ['x', 'X']) and
       (P[1] = '''') and
       (P[Len - 1] = '''') then
    begin
      // BLOB literals are string literals containing hexadecimal data and
      // preceded by a single "x" or "X" character. For example: X'53514C697465'
      LenHex := (Len - 3) shr 1;
      pointer(result) := FastNewString(LenHex, CP_RAWBYTESTRING);
      if mormot.core.text.HexToBin(@P[2], pointer(result), LenHex) then
        exit; // valid hexa data
    end
    else if (PInteger(P)^ and $00ffffff = JSON_BASE64_MAGIC_C) and
       Base64ToBinSafe(@P[3], Len - 3, RawByteString(result)) then
      exit; // safe decode Base64 content ('\uFFF0base64encodedbinary')
  // TEXT format
  FastSetStringCP(result, P, Len, CP_RAWBYTESTRING);
end;

function BlobToRawBlob(const Blob: RawByteString): RawBlob;
var
  Len, LenHex: integer;
  P: PUtf8Char;
begin
  result := '';
  if Blob = '' then
    exit;
  Len := length(Blob);
  P := pointer(Blob);
  if Len >= 3 then
    if (P[0] in ['x', 'X']) and
       (P[1] = '''') and
       (P[Len - 1] = '''') then
    begin
      // BLOB literals are string literals containing hexadecimal data and
      // preceded by a single "x" or "X" character. For example: X'53514C697465'
      LenHex := (Len - 3) shr 1;
      pointer(result) := FastNewString(LenHex, CP_RAWBYTESTRING);
      if mormot.core.text.HexToBin(@P[2], pointer(result), LenHex) then
        exit; // valid hexa data
    end
    else if (PInteger(P)^ and $00ffffff = JSON_BASE64_MAGIC_C) and
        Base64ToBinSafe(@P[3], Len - 3, RawByteString(result)) then
      exit; // safe decode Base64 content ('\uFFF0base64encodedbinary')
  // TEXT format
  result := Blob;
end;

function BlobToStream(P: PUtf8Char): TStream;
begin
  result := TRawByteStringStream.Create(BlobToRawBlob(P));
end;

function BlobToBytes(P: PUtf8Char): TBytes;
var
  Len, LenResult: integer;
begin
  result := nil;
  Len := StrLen(P);
  if Len = 0 then
    exit;
  if Len >= 3 then
    if (P[0] in ['x', 'X']) and
       (P[1] = '''') and
       (P[Len - 1] = '''') then
    begin
      // BLOB literals format
      LenResult := (Len - 3) shr 1;
      SetLength(result, LenResult);
      if mormot.core.text.HexToBin(@P[2], pointer(result), LenResult) then
        exit; // valid hexa data
    end
    else if (PInteger(P)^ and $00ffffff = JSON_BASE64_MAGIC_C) and
            Base64ToBinSafe(@P[3], Len - 3, result) then
       exit; // safe decode Base64 content ('\uFFF0base64encodedbinary')
  // TEXT format
  SetLength(result, Len);
  MoveFast(P^, pointer(result)^, Len);
end;

function RawBlobToBlob(const RawBlob: RawBlob): RawUtf8;
// BLOB literals are string literals containing hexadecimal data and
//  preceded by a single "x" or "X" character. For example: X'53514C697465'
begin
  result := RawBlobToBlob(pointer(RawBlob), length(RawBlob));
end;

function RawBlobToBlob(RawBlob: pointer; RawBlobLength: integer): RawUtf8;
// BLOB literals are string literals containing hexadecimal data and
//  preceded by a single "x" or "X" character. For example: X'53514C697465'
var
  P: PAnsiChar;
begin
  result := '';
  if RawBlobLength <> 0 then
  begin
    pointer(result) := FastNewString(RawBlobLength * 2 + 3, CP_UTF8);
    P := pointer(result);
    P[0] := 'X';
    P[1] := '''';
    BinToHex(RawBlob, P + 2, RawBlobLength);
    P[RawBlobLength * 2 + 2] := '''';
  end;
end;

function isBlobHex(P: PUtf8Char): boolean;
// BLOB literals are string literals containing hexadecimal data and
// preceded by a single "x" or "X" character. For example: X'53514C697465'
var
  Len: integer;
begin
  if P = nil then
  begin
    result := false;
    exit;
  end;
  while (P^ <= ' ') and
        (P^ <> #0) do
    inc(P);
  if (P[0] in ['x', 'X']) and
     (P[1] = '''') then
  begin
    Len := (StrLen(P) - 3) shr 1;
    result := (P[Len - 1] = '''') and
              mormot.core.text.HexToBin(@P[2], nil, Len);
    exit;
  end
  else
  begin
    result := false;
    exit;
  end;
end;

procedure Base64MagicToBlob(Base64: PUtf8Char; var result: RawUtf8);
begin
  // do not escape the result: returns e.g. X'53514C697465'
  result := RawBlobToBlob(Base64ToBin(PAnsiChar(Base64), StrLen(Base64)));
end;



{ --------- MultiPart encoding/decoding }

function MultiPartFormDataDecode(const MimeType, Body: RawUtf8;
  var MultiPart: TMultiPartDynArray): boolean;
var
  boundary, endBoundary: RawUtf8;
  i, j, n: integer;
  P: PUtf8Char;
  part: TMultiPart;

  function GetBoundary(const line: RawUtf8): boolean;
  var
    i: integer;
  begin
    result := false;
    i := PosEx('boundary=', line);
    if i = 0 then
      exit;
    TrimCopy(line, i + 9, 200, boundary);
    if (boundary <> '') and
       (boundary[1] = '"') then
      TrimChars(boundary, 1, 1); // "boundary" -> boundary
    Make(['--', boundary, '--'#13#10], endBoundary);
    boundary := Make(['--', boundary, #13#10]);
    result := true;
  end;

begin
  result := false;
  if not GetBoundary(MimeType) then
    exit;
  i := PosEx(boundary{%H-}, Body);
  if i <> 0 then
    repeat
      inc(i, length(boundary));
      if i = length(Body) then
        exit; // reached the (premature) end
      P := PUtf8Char(Pointer(Body)) + i - 1;
      Finalize(part);
      // decode section header
      repeat
        if IdemPChar(P, 'CONTENT-DISPOSITION: ') then
        begin
          inc(P, 21);
          if IdemPCharAndGetNextItem(P, 'FORM-DATA; NAME="', part.Name, '"') then
            IdemPCharAndGetNextItem(P, '; FILENAME="', part.FileName, '"')
          else if IdemPChar(P, 'FILE; ') then
          begin
            inc(P, 6);
            IdemPCharAndGetNextItem(P, 'NAME="', part.Name, '"');
            if P^ = ';' then
              P := GotoNextNotSpace(P + 1);
            IdemPCharAndGetNextItem(P, 'FILENAME="', part.FileName, '"');
          end;
        end
        else if IdemPCharAndGetNextItem(P, 'CONTENT-TYPE: ', part.ContentType) then
        begin
          if IdemPChar(pointer(part.ContentType), 'MULTIPART/MIXED') then
            if GetBoundary(part.ContentType) then
              part.ContentType := 'files'
            else
              exit;
        end
        else
          IdemPCharAndGetNextItem(P, 'CONTENT-TRANSFER-ENCODING: ', part.Encoding);
        P := GotoNextLine(P);
        if P = nil then
          exit;
      until PWord(P)^ = 13 + 10 shl 8;
      // decode section content
      i := P - PUtf8Char(Pointer(Body)) + 3; // i = just after header
      j := PosEx(boundary, Body, i);
      if j = 0 then
      begin
        j := PosEx(endBoundary{%H-}, Body, i); // try last boundary
        if j = 0 then
          exit;
        result := true; // content seems well formatted enough
      end;
      if part.ContentType <> 'files' then
      begin
        part.Content := copy(Body, i, j - i - 2); // -2 to ignore trailing #13#10
        if (part.ContentType = '') or
           (PosEx('-8', part.ContentType) > 0) then
        begin
          if IdemPChar(pointer(part.ContentType), JSON_CONTENT_TYPE_UPPER) then
            part.ContentType := JSON_CONTENT_TYPE
          else
            part.ContentType := TEXT_CONTENT_TYPE;
          FakeCodePage(part.Content, CP_UTF8); // ensure value is UTF-8
        end;
        if PropNameEquals(part.Encoding, 'base64') then
          part.Content := Base64ToBin(part.Content);
        // note: "quoted-printable" not yet handled here
        n := length(MultiPart);
        SetLength(MultiPart, n + 1);
        MultiPart[n] := part;
      end;
      i := j;
    until result;
end;

function MultiPartFormDataNewBound(var boundaries: TRawUtf8DynArray): RawUtf8;
var
  random: array[0..2] of cardinal;
begin
  RandomBytes(@random, SizeOf(random));
  result := BinToBase64uri(@random, SizeOf(random));
  AddRawUtf8(boundaries, result);
end;

function MultiPartFormDataEncode(const MultiPart: TMultiPartDynArray;
  var MultiPartContentType, MultiPartContent: RawUtf8;
  Rfc2388NestedFiles: boolean): boolean;
var
  len, filescount, i: integer;
  boundaries: TRawUtf8DynArray;
  bound: RawUtf8;
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  result := false;
  len := length(MultiPart);
  if len = 0 then
    exit;
  filescount := 0;
  W := TTextWriter.CreateOwnedStream(temp);
  try
    // header - see https://www.w3.org/Protocols/rfc1341/7_2_Multipart.html
    bound := MultiPartFormDataNewBound(boundaries);
    MultiPartContentType :=
      'Content-Type: multipart/form-data; boundary=' + bound;
    for i := 0 to len - 1 do
      with MultiPart[i] do
      begin
        if FileName = '' then
          // simple name/value form section
          W.Add('--%'#13#10'Content-Disposition: form-data; name="%"'#13#10 +
            'Content-Type: %'#13#10#13#10'%'#13#10,
            [bound, Name, ContentType, Content])
        else
        begin
          // if this is the first file, create the RFC 2388 nested "files"
          if Rfc2388NestedFiles and
             (filescount = 0) then
          begin
            W.Add('--%'#13#10, [bound]);
            bound := MultiPartFormDataNewBound(boundaries);
            W.Add('Content-Disposition: form-data; name="files"'#13#10 +
              'Content-Type: multipart/mixed; boundary=%'#13#10#13#10, [bound]);
            W.Add('--%'#13#10'Content-Disposition: file; filename="%"'#13#10 +
              'Content-Type: %'#13#10, [bound, FileName, ContentType]);
          end
          else
            // see https://tools.ietf.org/html/rfc7578#appendix-A
            W.Add('--%'#13#10 +
              'Content-Disposition: form-data; name="%"; filename="%"'#13#10 +
              'Content-Type: %'#13#10,
              [bound, Name, FileName, ContentType]);
          if Encoding <> '' then
            W.Add('Content-Transfer-Encoding: %'#13#10, [Encoding]);
          W.AddCR;
          W.AddString(MultiPart[i].Content);
          W.AddCR;
          inc(filescount);
        end;
      end;
    // footer multipart
    for i := length(boundaries) - 1 downto 0 do
      W.Add('--%--'#13#10, [boundaries[i]]);
    W.SetText(MultiPartContent);
    result := True;
  finally
    W.Free;
  end;
end;

function MultiPartFormDataAddFile(const FileName: TFileName;
  var MultiPart: TMultiPartDynArray; const Name: RawUtf8;
  const ForcedContentType: RawUtf8): boolean;
var
  part: TMultiPart;
  newlen: integer;
  content: RawByteString;
begin
  result := false;
  content := StringFromFile(FileName);
  if content = '' then
    exit;
  newlen := length(MultiPart) + 1;
  if Name = '' then
    FormatUtf8('File%', [newlen], part.Name)
  else
    part.Name := Name;
  part.FileName := StringToUtf8(ExtractFileName(FileName));
  if ForcedContentType <> '' then
    part.ContentType := ForcedContentType
  else
    part.ContentType := GetMimeContentType(
      pointer(content), length(content), FileName);
  part.Encoding := 'base64';
  part.Content := BinToBase64(content);
  SetLength(MultiPart, newlen);
  MultiPart[newlen - 1] := part;
  result := true;
end;

function MultiPartFormDataAddField(const FieldName, FieldValue: RawUtf8;
  var MultiPart: TMultiPartDynArray; const ForcedContentType: RawUtf8): boolean;
var
  part: TMultiPart;
  newlen: integer;
begin
  result := false;
  if FieldName = '' then
    exit;
  newlen := length(MultiPart) + 1;
  part.Name := FieldName;
  if ForcedContentType <> '' then
    part.ContentType := ForcedContentType
  else
    part.ContentType := GetMimeContentTypeFromBuffer(
      pointer(FieldValue), length(FieldValue), TEXT_CONTENT_TYPE);
  part.Content := FieldValue;
  SetLength(MultiPart, newlen);
  MultiPart[newlen - 1] := part;
  result := true;
end;


{ --------- Baudot encoding/decoding }

const
  // see https://en.wikipedia.org/wiki/Baudot_code
  Baudot2Char: array[0..63] of AnsiChar =
   #0'e'#10'a siu'#13'drjnfcktzlwhypqobg'#254'mxv'#255+
   #0'3'#10'- ''87'#13#0'4'#0',!:(5+)2$6019?@'#254'./;'#255;
var
  Char2Baudot: array[AnsiChar] of byte;

function AsciiToBaudot(const Text: RawUtf8): RawByteString;
begin
  result := AsciiToBaudot(pointer(Text), length(Text));
end;

function AsciiToBaudot(P: PAnsiChar; len: PtrInt): RawByteString;
var
  i: PtrInt;
  c, d, bits: integer;
  shift: boolean;
  dest: PByte;
  tmp: TSynTempBuffer;
begin
  result := '';
  if (P = nil) or
     (len = 0) then
    exit;
  shift := false;
  dest := tmp.Init((len * 10) shr 3);
  d := 0;
  bits := 0;
  for i := 0 to len - 1 do
  begin
    c := Char2Baudot[P[i]];
    if c > 32 then
    begin
      if not shift then
      begin
        d := (d shl 5) or 27;
        inc(bits, 5);
        shift := true;
      end;
      d := (d shl 5) or (c - 32);
      inc(bits, 5);
    end
    else if c > 0 then
    begin
      if shift and
         (P[i] >= ' ') then
      begin
        d := (d shl 5) or 31;
        inc(bits, 5);
        shift := false;
      end;
      d := (d shl 5) or c;
      inc(bits, 5);
    end;
    while bits >= 8 do
    begin
      dec(bits, 8);
      dest^ := d shr bits;
      inc(dest);
    end;
  end;
  if bits > 0 then
  begin
    dest^ := d shl (8 - bits);
    inc(dest);
  end;
  FastSetRawByteString(result, tmp.buf, PAnsiChar(dest) - PAnsiChar(tmp.buf));
  tmp.Done;
end;

function BaudotToAscii(const Baudot: RawByteString): RawUtf8;
begin
  result := BaudotToAscii(pointer(Baudot), length(Baudot));
end;

function BaudotToAscii(Baudot: PByteArray; len: PtrInt): RawUtf8;
var
  i: PtrInt;
  c, b, bits, shift: integer;
  tmp: TSynTempBuffer;
  dest: PAnsiChar;
begin
  result := '';
  if (Baudot = nil) or
     (len <= 0) then
    exit;
  dest := tmp.Init((len shl 3) div 5);
  try
    shift := 0;
    b := 0;
    bits := 0;
    for i := 0 to len - 1 do
    begin
      b := (b shl 8) or Baudot[i];
      inc(bits, 8);
      while bits >= 5 do
      begin
        dec(bits, 5);
        c := (b shr bits) and 31;
        case c of
          27:
            if shift <> 0 then
              exit
            else
              shift := 32;
          31:
            if shift <> 0 then
              shift := 0
            else
              exit;
        else
          begin
            c := ord(Baudot2Char[c + shift]);
            if c = 0 then
              if Baudot[i + 1] = 0 then // allow triming of last 5 bits
                break
              else
                exit;
            dest^ := AnsiChar(c);
            inc(dest);
          end;
        end;
      end;
    end;
  finally
    tmp.Done(dest, result);
  end;
end;



{ ***************** URI-Encoded Text Buffer Process }

function UrlEncode(const svar: RawUtf8): RawUtf8;
begin
  result := UrlEncode(pointer(svar));
end;

function UrlEncodeName(const svar: RawUtf8): RawUtf8;
begin
  result := UrlEncodeName(pointer(svar));
end;

// two sub-functions for better code generation of UrlEncode()

procedure _UrlEncode_Write(s, p: PByte; tab: PTextByteSet; space2plus: cardinal);
var
  c: cardinal;
  hex: PByteToWord;
begin
  hex := @TwoDigitsHexWB;
  repeat
    c := s^;
    inc(s);
    if tcUriUnreserved in tab[c] then
    begin
      // was ['_', '-', '.', '~', '0'..'9', 'a'..'z', 'A'..'Z']
      p^ := c;
      inc(p);
    end
    else if c = 0 then
      exit
    else if c = space2plus then // space2plus=32 for parameter, =48 for URI
    begin
      p^ := ord('+');
      inc(p);
    end
    else
    begin
      p^ := ord('%');
      inc(p);
      PWord(p)^ := hex[c];
      inc(p, 2);
    end;
  until false;
end;

function _UrlEncode_ComputeLen(s: PByte; tab: PTextByteSet; space2plus: cardinal): PtrInt;
var
  c: cardinal;
begin
  result := 0;
  repeat
    c := s^;
    inc(s);
    if (tcUriUnreserved in tab[c]) or
       (c = space2plus) then // =32 for parameter, =48 for URI
    begin
      inc(result);
      continue;
    end;
    if c = 0 then
      exit;
    inc(result, 3);
  until false;
end;

function UrlEncode(Text: PUtf8Char): RawUtf8;
begin
  result := '';
  if Text = nil then
    exit;
  FastSetString(result, _UrlEncode_ComputeLen(pointer(Text), @TEXT_CHARS, 32));
  _UrlEncode_Write(pointer(Text), pointer(result), @TEXT_BYTES, 32);
end;

function UrlEncodeName(Text: PUtf8Char): RawUtf8;
begin
  result := '';
  if Text = nil then
    exit;
  FastSetString(result, _UrlEncode_ComputeLen(pointer(Text), @TEXT_CHARS, 48));
  _UrlEncode_Write(pointer(Text), pointer(result), @TEXT_BYTES, 48);
end;

function UrlEncode(const NameValuePairs: array of const;
  TrimLeadingQuestionMark: boolean): RawUtf8;
// (['select','*','where','ID=12','offset',23,'object',aObject]);
var
  a, n: PtrInt;
  name, value: RawUtf8;
  p: PVarRec;
begin
  result := '';
  n := high(NameValuePairs);
  if (n > 0) and
     (n and 1 = 1) then
  begin
    for a := 0 to n shr 1 do
    begin
      VarRecToUtf8(NameValuePairs[a * 2], name);
      if not IsUrlValid(pointer(name)) then
        continue; // just skip invalid names
      p := @NameValuePairs[a * 2 + 1];
      if p^.VType = vtObject then
        value := ObjectToJson(p^.VObject, [])
      else
        VarRecToUtf8(p^, value);
      result := result + '&' + name + '=' + UrlEncode(value);
    end;
    if TrimLeadingQuestionMark then
      delete(result, 1, 1)
    else
      result[1] := '?';
  end;
end;

function IsUrlValid(P: PUtf8Char): boolean;
var
  tab: PTextCharSet;
begin
  result := false;
  if P = nil then
    exit;
  tab := @TEXT_CHARS;
  repeat
    if tcUriUnreserved in tab[P^] then
      inc(P) // was  ['_', '-', '.', '~', '0'..'9', 'a'..'z', 'A'..'Z']
    else
      exit;
  until P^ = #0;
  result := true;
end;

function AreUrlValid(const Url: array of RawUtf8): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to high(Url) do
    if not IsUrlValid(pointer(Url[i])) then
      exit;
  result := true;
end;

function IncludeTrailingUriDelimiter(const URI: RawByteString): RawByteString;
begin
  if (URI <> '') and
     (Uri[length(URI)] <> '/') then
    result := URI + '/'
  else
    result := URI;
end;

procedure UrlDecodeVar(U: PUtf8Char; L: PtrInt; var result: RawUtf8; name: boolean);
var
  P: PUtf8Char;
  tmp: TSynTempBuffer;
begin
  if L = 0 then
  begin
    result := '';
    exit;
  end;
  P := tmp.Init(L);
  repeat
    case U^ of
      #0:
        break; // reached end of URI
      '%':
        if not HexToChar(PAnsiChar(U + 1), P) then
          P^ := U^ // browsers may not follow the RFC (e.g. encode % as % !)
        else
          inc(U, 2);
      '+':
        if name then
          P^ := '+'
        else
          P^ := ' ';
    else
      P^ := U^;
    end;
    inc(U);
    inc(P);
  until false;
  tmp.Done(P, result);
end;

function UrlDecode(U: PUtf8Char): RawUtf8;
begin
  UrlDecodeVar(U, StrLen(U), result, {name=}false);
end;

function UrlDecode(const s: RawUtf8): RawUtf8;
begin
  UrlDecodeVar(pointer(s), length(s), result, {name=}false);
end;

function UrlDecodeName(U: PUtf8Char): RawUtf8;
begin
  UrlDecodeVar(U, StrLen(U), result, {name=}true);
end;

function UrlDecodeName(const s: RawUtf8): RawUtf8;
begin
  UrlDecodeVar(pointer(s), length(s), result, {name=}true);
end;

function UrlDecodeNextValue(U: PUtf8Char; out Value: RawUtf8): PUtf8Char;
var
  Beg, V: PUtf8Char;
  len: PtrInt;
  {$ifndef CPUX86NOTPIC}
  tab: PByteArray; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  if U <> nil then
  begin
    // compute resulting length of value
    {$ifndef CPUX86NOTPIC}
    tab := @ConvertHexToBin;
    {$endif CPUX86NOTPIC}
    Beg := U;
    len := 0;
    while (U^ <> #0) and
          (U^ <> '&') do
    begin
      if (U^ = '%') and
         HexToCharValid(pointer(U + 1) {$ifndef CPUX86NOTPIC}, tab{$endif}) then
        inc(U, 3)
      else
        inc(U);
      inc(len);
    end;
    // decode value content
    if len <> 0 then
    begin
      FastSetString(Value, len);
      V := pointer(Value);
      U := Beg;
      repeat
        if (U^ = '%') and
           HexToChar(pointer(U + 1), V {$ifndef CPUX86NOTPIC}, tab{$endif}) then
        begin
          inc(V);
          inc(U, 3);
        end
        else
        begin
          if U^ = '+' then
            V^ := ' '
          else
            V^ := U^;
          inc(V);
          inc(U);
        end;
        dec(len);
      until len = 0;
    end;
  end;
  result := U;
end;

function UrlDecodeNextName(U: PUtf8Char; out Name: RawUtf8): PUtf8Char;
var
  Beg, V: PUtf8Char;
  len: PtrInt;
  {$ifndef CPUX86NOTPIC}
  tab: PByteArray; // faster on PIC, ARM and x86_64
  {$endif CPUX86NOTPIC}
begin
  result := nil;
  if U = nil then
    exit;
  // compute resulting length of name
  {$ifndef CPUX86NOTPIC}
  tab := @ConvertHexToBin;
  {$endif CPUX86NOTPIC}
  Beg := U;
  len := 0;
  repeat
    case U^ of
      #0:
        exit;
      '=':
        begin
          result := U + 1;
          break;
        end;
      '%':
        if (U[1] = '3') and
           (U[2] in ['D', 'd']) then
        begin
          result := U + 3;
          break;  // %3d means ending = according to the RFC
        end
        else if HexToCharValid(pointer(U + 1) {$ifndef CPUX86NOTPIC}, tab{$endif}) then
          inc(U, 3)
        else
          inc(U);
    else
      inc(U);
    end;
    inc(len);
  until false;
  if len = 0 then
    exit;
  // decode name content
  FastSetString(Name, len);
  V := pointer(Name);
  U := Beg;
  repeat
    if (U^ = '%') and
       HexToChar(pointer(U + 1), V {$ifndef CPUX86NOTPIC}, tab{$endif}) then
    begin
      inc(V);
      inc(U, 3);
    end
    else
    begin
      if U^ = '+' then
        V^ := ' '
      else
        V^ := U^;
      inc(V);
      inc(U);
    end;
    dec(len);
  until len = 0;
end;

function UrlDecodeNextNameValue(U: PUtf8Char; var Name, Value: RawUtf8): PUtf8Char;
begin
  result := nil;
  if U = nil then
    exit;
  U := UrlDecodeNextName(U, Name);
  if U = nil then
    exit;
  U := UrlDecodeNextValue(U, Value);
  if U^ = #0 then
    result := U
  else
    result := U + 1; // jump '&' to let decode the next name=value pair
end;

procedure UrlDecodeEnd(Next: PPUtf8Char; U: PUtf8Char); {$ifdef HASINLINE} inline; {$endif}
var
  c: AnsiChar;
begin
  if Next = nil then
    exit;
  repeat
    c := U^;
    inc(U);
    if c <> #0 then
      if c = '&' then
        break // jump '&'
      else
        continue;
    U := nil; // return nil when end of URI is reached
    break;
  until false;
  Next^ := U;
end;

function UrlDecodeValue(U: PUtf8Char; const Upper: RawUtf8;
  var Value: RawUtf8; Next: PPUtf8Char): boolean;
begin
  result := false; // mark value not modified by default
  if U = nil then
  begin
    if Next <> nil then
      Next^ := U;
    exit;
  end;
  if IdemPChar(U, pointer(Upper)) then
  begin
    result := true;
    inc(U, length(Upper));
    U := UrlDecodeNextValue(U, Value);
  end;
  UrlDecodeEnd(Next, U);
end;

function UrlDecodeInteger(U: PUtf8Char; const Upper: RawUtf8;
  var Value: integer; Next: PPUtf8Char): boolean;
var
  v, sign: PtrInt;
begin
  result := false; // mark value not modified by default
  if U = nil then
  begin
    if Next <> nil then
      Next^ := U;
    exit;
  end;
  if IdemPChar(U, pointer(Upper)) then
  begin
    inc(U, length(Upper));
    if U^ = '-' then
    begin
      sign := -1;
      inc(U);
    end
    else
      sign := 1;
    if U^ in ['0'..'9'] then
    begin
      v := 0;
      repeat
        v := (v * 10) + ord(U^) - 48;
        inc(U);
      until not (U^ in ['0'..'9']);
      Value := v * sign;
      result := true;
    end;
  end;
  UrlDecodeEnd(Next, U);
end;

function UrlDecodeCardinal(U: PUtf8Char; const Upper: RawUtf8;
  var Value: cardinal; Next: PPUtf8Char): boolean;
var
  v: PtrInt;
begin
  result := false; // mark value not modified by default
  if U = nil then
  begin
    if Next <> nil then
      Next^ := U;
    exit;
  end;
  if IdemPChar(U, pointer(Upper)) then
  begin
    inc(U, length(Upper));
    if U^ in ['0'..'9'] then
    begin
      v := 0;
      repeat
        v := (v * 10) + ord(U^) - 48;
        inc(U);
      until not (U^ in ['0'..'9']);
      Value := v;
      result := true;
    end;
  end;
  UrlDecodeEnd(Next, U);
end;

function UrlDecodeInt64(U: PUtf8Char; const Upper: RawUtf8;
  var Value: Int64; Next: PPUtf8Char): boolean;
var
  v, sign: Int64;
begin
  result := false; // mark value not modified by default
  if U = nil then
  begin
    if Next <> nil then
      Next^ := U;
    exit;
  end;
  if IdemPChar(U, pointer(Upper)) then
  begin
    inc(U, length(Upper));
    if U^ = '-' then
    begin
      sign := 1;
      inc(U);
    end
    else
      sign := -1;
    if U^ in ['0'..'9'] then
    begin
      v := 0;
      repeat
        v := (v * 10) + ord(U^) - 48;
        inc(U);
      until not (U^ in ['0'..'9']);
      Value := v * sign;
      result := true;
    end;
  end;
  UrlDecodeEnd(Next, U);
end;

function UrlDecodeExtended(U: PUtf8Char; const Upper: RawUtf8;
  var Value: TSynExtended; Next: PPUtf8Char): boolean;
var
  tmp: RawUtf8;
  err: integer;
begin
  result := UrlDecodeValue(U, Upper, tmp, Next);
  if result then
  begin
    Value := GetExtended(pointer(tmp), err);
    if err <> 0 then
      result := false;
  end;
end;

function UrlDecodeDouble(U: PUtf8Char; const Upper: RawUtf8;
  var Value: double; Next: PPUtf8Char): boolean;
var
  tmp: RawUtf8;
  err: integer;
begin
  result := UrlDecodeValue(U, Upper, tmp, Next);
  if result then
  begin
    Value := GetExtended(pointer(tmp), err);
    if err <> 0 then
      result := false;
  end;
end;

function UrlDecodeNeedParameters(U, CsvNames: PUtf8Char): boolean;
var
  tmp: array[byte] of AnsiChar;
  L: integer;
  Beg: PUtf8Char;
// UrlDecodeNeedParameters('price=20.45&where=LastName%3D','price,where') will
// return TRUE
begin
  result := (CsvNames = nil);
  if result or
     (U = nil) then
    exit; // no parameter to check -> success; no input data -> error
  repeat
    L := 0;
    while (CsvNames^ <> #0) and
          (CsvNames^ <> ',') do
    begin
      tmp[L] := NormToUpper[CsvNames^];
      if L = high(tmp) then
        exit
      else // invalid CSV parameter
        inc(L);
      inc(CsvNames);
    end;
    if L = 0 then
      exit; // invalid CSV parameter
    PWord(@tmp[L])^ := ord('=');
    Beg := U;
    repeat
      if IdemPChar(U, tmp) then
        break;
      while not (U^ in [#0, '&']) do
        inc(U);
      if U^ = #0 then
        exit
      else // didn't find tmp in U
        inc(U); // Jump &
    until false;
    U := Beg;
    if CsvNames^ = #0 then
      Break
    else // no more parameter to check
      inc(CsvNames); // jump &
  until false;
  result := true; // all parameters found
end;


{ *********** Basic MIME Content Types Support }

const
  MIME_MAGIC: array[0..17] of cardinal = (
     $04034b50 + 1, $46445025 + 1, $21726152 + 1, $afbc7a37 + 1,
     $694c5153 + 1, $75b22630 + 1, $9ac6cdd7 + 1, $474e5089 + 1,
     $38464947 + 1, $46464f77 + 1, $a3df451a + 1, $002a4949 + 1,
     $2a004d4d + 1, $2b004d4d + 1, $46464952 + 1, $e011cfd0 + 1,
     $5367674f + 1, $1c000000 + 1);
  MIME_MAGIC_TYPE: array[0..high(MIME_MAGIC)] of TMimeType = (
     mtZip, mtPdf, mtRar, mt7z, mtSQlite3, mtWma, mtWmv, mtPng, mtGif, mtFont,
     mtWebm, mtTiff, mtTiff, mtTiff, mtWebp{=riff}, mtDoc, mtOgg, mtMp4);

function GetMimeContentTypeFromMemory(Content: Pointer; Len: PtrInt): TMimeType;
var
  i: PtrInt;
begin
  result := mtUnknown;
  // see http://www.garykessler.net/library/file_sigs.html for magic numbers
  if (Content <> nil) and
     (Len > 4) then
  begin
    i := IntegerScanIndex(@MIME_MAGIC, length(MIME_MAGIC), PCardinal(Content)^ + 1);
    // + 1 to avoid finding it in the exe - may use SSE2
    if i >= 0 then
      result := MIME_MAGIC_TYPE[i];
    case result of // identify some partial matches
      mtUnknown:
        case PCardinal(Content)^ and $00ffffff of
          $685a42:
            result := mtBz2;  // 42 5A 68
          $088b1f:
            result := mtGzip; // 1F 8B 08
          $492049:
            result := mtTiff; // 49 20 49
          $ffd8ff:
            result := mtJpg;  // FF D8 FF DB/E0/E1/E2/E3/E8
        else
          case PWord(Content)^ of
            $4D42:
              result := mtBmp; // 42 4D
          end;
        end;
      mtWebp:
        if Len > 16 then // RIFF
          case PCardinalArray(Content)^[2] of
            $50424557:
              result := mtWebp;
            $20495641:
              if PCardinalArray(Content)^[3] = $5453494c then
                result := mtAvi; // Windows Audio Video Interleave file
          else
            result := mtUnknown;
          end
        else
          result := mtUnknown;
      mtDoc: // Microsoft Office applications D0 CF 11 E0=DOCFILE
        if Len > 600 then
          case PWordArray(Content)^[256] of // at offset 512
            $a5ec:
              result := mtDoc; // EC A5 C1 00
            $fffd: // FD FF FF
              case PByteArray(Content)^[516] of
                $0E, $1c, $43:
                  result := mtPpt;
                $10, $1f, $20, $22, $23, $28, $29:
                  result := mtXls;
                else
                  result := mtUnknown;
              end
            else
              result := mtUnknown;
          end
        else
          result := mtUnknown;
      mtOgg:
        if (Len < 14) or
           (PCardinalArray(Content)^[1] <> $00000200) or
           (PCardinalArray(Content)^[2] <> $00000000) or
           (PWordArray(Content)^[6] <> $0000) then
            result := mtUnknown;
      mtMp4:
        if (Len < 12) or
           (PCardinalArray(Content)^[1] <> $70797466) then  // ftyp
            case PCardinalArray(Content)^[2] of
              $6d6f7369, // isom: ISO Base Media file (MPEG-4) v1
              $3234706d, // mp42: MPEG-4 video/QuickTime file
              $35706733: // 3gp5: MPEG-4 video files
                ;
            else
              result := mtUnknown
            end
       else
         result := mtUnknown;
    end;
  end;
end;

function GetMimeContentTypeFromBuffer(Content: Pointer; Len: PtrInt;
  const DefaultContentType: RawUtf8; Mime: PMimeType): RawUtf8;
var
  m: TMimeType;
begin
  m := GetMimeContentTypeFromMemory(Content, Len);
  if Mime <> nil then
    Mime^ := m;
  if m = mtUnknown then
    result := DefaultContentType
  else
    result := MIME_TYPE[m];
end;

const
  MIME_EXT: array[0..46] of PUtf8Char = ( // for IdemPPChar() start check
    'PNG',  'GIF',  'TIF',  'JP',  'BMP', 'DOC',  'HTM',  'CSS',
    'JSON', 'ICO',  'WOF', 'TXT', 'SVG',  'ATOM', 'RDF', 'RSS',
    'WEBP', 'APPC', 'MANI', 'XML', 'JS',  'MJS',  'WOFF', 'OGG',
    'OGV',  'MP4',  'M2V',  'M2P', 'MP3', 'H264', 'TEXT', 'LOG',
    'GZ',  'WEBM', 'MKV',  'RAR',  '7Z',  'BZ2', 'WMA',  'WMV',
    'AVI', 'PPT',  'XLS',  'PDF',  'SQLITE', 'DB3', nil);
  MIME_EXT_TYPE: array[0 .. high(MIME_EXT) - 1] of TMimeType = (
    mtPng,  mtGif,  mtTiff,  mtJpg,  mtBmp,  mtDoc,  mtHtml, mtCss,
    mtJson, mtXIcon, mtFont, mtText, mtSvg,  mtXml,  mtXml,  mtXml,
    mtWebp, mtManifest, mtManifest,  mtXml,  mtJS,   mtJS,   mtFont, mtOgg,
    mtOgg,  mtMp4,  mtMp2,   mtMp2,  mtMpeg, mtH264, mtText, mtText,
    mtGzip, mtWebm, mtWebm,  mtRar,  mt7z,   mtBz2,  mtWma,  mtWmv,
    mtAvi,  mtPpt,  mtXls,  mtPdf,   mtSQlite3, mtSQlite3);

function GetMimeTypeFromExt(const Ext: RawUtf8): TMimeType;
var
  i: PtrInt;
begin
  result := mtUnknown;
  case length(Ext) of
    0: ;
    1: // IdemPPChar() requires 2 chars len minimum
      case ext[1] of
        'x', 'X':
          result := mtXcomp;
      end;
  else
    begin
      i := IdemPPChar(pointer(Ext), @MIME_EXT);
      if i >= 0 then
        result := MIME_EXT_TYPE[i]
    end;
  end;
end;

function GetMimeContentTypeFromExt(const FileName: TFileName; FileExt: PRawUtf8): TMimeType;
var
  ext: RawUtf8;
begin
  StringToUtf8(ExtractExt(FileName, {withoutdot=}true), ext);
  result := GetMimeTypeFromExt(ext);
  if FileExt <> nil then
    FileExt^ := {%H-}ext;
end;

function GetMimeContentType(Content: Pointer; Len: PtrInt; const FileName: TFileName;
  const DefaultContentType: RawUtf8; Mime: PMimeType): RawUtf8;
var
  ext: RawUtf8;
  m: TMimeType;
begin
  if FileName <> '' then
  begin
    // file extension is more precise -> check first
    m := GetMimeContentTypeFromExt(FileName, @ext);
    if m <> mtUnknown then
    begin
      result := MIME_TYPE[m];
      if Mime <> nil then
        Mime^ := m;
      exit;
    end;
    // fallback to content check
    if (ext <> '') and
       (ext[1] in ['a'..'z']) then
      // e.g. 'application/zip' or 'application/pdf'
      result := 'application/' + LowerCase(ext)
    else
      result := DefaultContentType;
  end
  else
    result := DefaultContentType;
  result := GetMimeContentTypeFromBuffer(Content, Len, result, Mime);
end;

function GetMimeContentTypeHeader(const Content: RawByteString;
  const FileName: TFileName; Mime: PMimeType): RawUtf8;
begin
  result := HEADER_CONTENT_TYPE + GetMimeContentType(
      Pointer(Content), length(Content), FileName, BINARY_CONTENT_TYPE, Mime);
end;

const
  MIME_COMPRESSED: array[0..38] of cardinal = ( // may use SSE2
    $04034b50, // 'application/zip' = 50 4B 03 04
    $474e5089, // 'image/png' = 89 50 4E 47 0D 0A 1A 0A
    $e0ffd8ff, $e1ffd8ff, // 'image/jpeg' FF D8 FF E0/E1
    $002a4949, $2a004d4d, $2b004d4d, // 'image/tiff'
    $184d2204, // LZ4 stream format = 04 22 4D 18
    $21726152, // 'application/x-rar-compressed' = 52 61 72 21 1A 07 00
    $28635349, // cab = 49 53 63 28
    $38464947, // 'image/gif' = 47 49 46 38
    $43614c66, // FLAC = 66 4C 61 43 00 00 00 22
    $4643534d, // cab = 4D 53 43 46 [MSCF]
    $46464952, // avi,webp,wav = 52 49 46 46 [RIFF]
    $46464f77, // 'application/font-woff' = wOFF in BigEndian
    $4d5a4cff, // LZMA = FF 4C 5A 4D 41 00
    $72613c21, // .ar/.deb package file = '!<arch>' (assuming compressed)
    $75b22630, // 'audio/x-ms-wma' = 30 26 B2 75 8E 66
    $766f6f6d, // mov = 6D 6F 6F 76 [....moov]
    $89a8275f, // jar = 5F 27 A8 89
    $9ac6cdd7, // 'video/x-ms-wmv' = D7 CD C6 9A 00 00
    $a5a5a5a5, // mORMot 1 .mab file
    $a5a5a55a, // .mab file = MAGIC_MAB in mormot.core.log.pas
    $a5aba5a5, // .data = TRESTSTORAGEINMEMORY_MAGIC in mormot.orm.server.pas
    LOG_MAGIC, // .log.synlz/.log.synliz compression = $aba51051
    $aba5a5ab, $aba5a5ab + 1, $aba5a5ab + 2, $aba5a5ab + 3, $aba5a5ab + 4,
    $aba5a5ab + 5, $aba5a5ab + 6, $aba5a5ab + 7, // .dbsynlz = SQLITE3_MAGIC
    $afbc7a37, // 'application/x-7z-compressed' = 37 7A BC AF 27 1C
    $b7010000, $ba010000, // mpeg = 00 00 01 Bx
    $cececece, // jceks = CE CE CE CE
    $dbeeabed, // .rpm package file
    $e011cfd0); // msi = D0 CF 11 E0 A1 B1 1A E1

function IsContentCompressed(Content: Pointer; Len: PtrInt): boolean;
begin
  // see http://www.garykessler.net/library/file_sigs.html
  result := false;
  if (Content <> nil) and
     (Len > 8) then
    if IntegerScanExists(@MIME_COMPRESSED, length(MIME_COMPRESSED), PCardinal(Content)^) then
      result := true
    else
      case PCardinal(Content)^ and $00ffffff of // 24-bit magic
        $088b1f, // 'application/gzip' = 1F 8B 08
        $334449, // mp3 = 49 44 33 [ID3]
        $492049, // 'image/tiff' = 49 20 49
        $535746, // swf = 46 57 53 [FWS]
        $535743, // swf = 43 57 53 [zlib]
        $53575a, // zws/swf = 5A 57 53 [FWS]
        $564c46, // flv = 46 4C 56 [FLV]
        $685a42, // 'application/bzip2' = 42 5A 68
        $ffd8ff: // JPEG_CONTENT_TYPE = FF D8 FF DB/E0/E1/E2/E3/E8
          result := true;
      else
        case PCardinalArray(Content)^[1] of // ignore variable 4 byte offset
          $70797466, // mp4,mov = 66 74 79 70 [33 67 70 35/4D 53 4E 56..]
          $766f6f6d: // mov = 6D 6F 6F 76
            result := true;
        end;
      end;
end;

function GetJpegSize(jpeg: PAnsiChar; len: PtrInt;
  out Height, Width, Bits: integer): boolean;
var
  je: PAnsiChar;
begin
  // see https://en.wikipedia.org/wiki/JPEG#Syntax_and_structure
  result := false;
  if (jpeg = nil) or
     (len < 100) or
     (PWord(jpeg)^ <> $d8ff) then // SOI
    exit;
  je := jpeg + len - 8;
  inc(jpeg, 2);
  while jpeg < je do
  begin
    if jpeg^ <> #$ff then
      exit;
    inc(jpeg);
    case ord(jpeg^) of
      $c0..$c3, $c5..$c7, $c9..$cb, $cd..$cf: // SOF
        begin
          Height := swap(PWord(jpeg + 4)^);
          Width  := swap(PWord(jpeg + 6)^);
          Bits   := PByte(jpeg + 8)^ * 8;
          result := (Height > 0) and
                    (Height < 20000) and
                    (Width > 0) and
                    (Width < 20000);
          exit;
        end;
      $d0..$d8, $01: // RST, SOI
        inc(jpeg);
      $d9: // EOI
        break;
      $ff: // padding
        ;
    else
      inc(jpeg, swap(PWord(jpeg + 1)^) + 1);
    end;
  end;
end;


{ ************* Text Memory Buffers and Files }

{ TMemoryMapText }

constructor TMemoryMapText.Create;
begin
end;

constructor TMemoryMapText.Create(aFileContent: PUtf8Char; aFileSize: integer);
begin
  Create;
  fMap.Map(aFileContent, aFileSize);
  LoadFromMap;
end;

constructor TMemoryMapText.Create(const aFileName: TFileName);
begin
  Create;
  fFileName := aFileName;
  if fMap.Map(aFileName) then
    LoadFromMap;
end; // invalid file or unable to memory map its content -> Count := 0

destructor TMemoryMapText.Destroy;
begin
  Freemem(fLines);
  fMap.UnMap;
  inherited;
end;

procedure TMemoryMapText.SaveToStream(Dest: TStream; const Header: RawUtf8);
var
  i: PtrInt;
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  i := length(Header);
  if i > 0 then
    Dest.WriteBuffer(pointer(Header)^, i);
  if fMap.Size > 0 then
    Dest.WriteBuffer(fMap.Buffer^, fMap.Size);
  if fAppendedLinesCount = 0 then
    exit;
  W := TTextWriter.Create(Dest, @temp, SizeOf(temp));
  try
    if (fMap.Size > 0) and
       (fMap.Buffer[fMap.Size - 1] >= ' ') then
      W.Add(#10);
    for i := 0 to fAppendedLinesCount - 1 do
    begin
      W.AddString(fAppendedLines[i]);
      W.Add(#10);
    end;
    W.FlushFinal;
  finally
    W.Free;
  end;
end;

procedure TMemoryMapText.SaveToFile(FileName: TFileName; const Header: RawUtf8);
var
  FS: TStream;
begin
  FS := TFileStreamEx.Create(FileName, fmCreate);
  try
    SaveToStream(FS, Header);
  finally
    FS.Free;
  end;
end;

function TMemoryMapText.GetLine(aIndex: integer): RawUtf8;
begin
  if (self = nil) or
     (cardinal(aIndex) >= cardinal(fCount)) then
    result := ''
  else
    FastSetString(result, fLines[aIndex], GetLineSize(fLines[aIndex], fMapEnd));
end;

function TMemoryMapText.GetString(aIndex: integer): string;
begin
  if (self = nil) or
     (cardinal(aIndex) >= cardinal(fCount)) then
    result := ''
  else
    Utf8DecodeToString(fLines[aIndex], GetLineSize(fLines[aIndex], fMapEnd), result);
end;

function TMemoryMapText.LineContains(const aUpperSearch: RawUtf8;
  aIndex: integer): boolean;
begin
  if (self = nil) or
     (cardinal(aIndex) >= cardinal(fCount)) or
     (aUpperSearch = '') then
    result := false
  else
    result := GetLineContains(fLines[aIndex], fMapEnd, pointer(aUpperSearch));
end;

function TMemoryMapText.LineSize(aIndex: integer): integer;
begin
  result := GetLineSize(fLines[aIndex], fMapEnd);
end;

function TMemoryMapText.LineSizeSmallerThan(aIndex, aMinimalCount: integer): boolean;
begin
  result := GetLineSizeSmallerThan(fLines[aIndex], fMapEnd, aMinimalCount);
end;

procedure TMemoryMapText.ProcessOneLine(LineBeg, LineEnd: PUtf8Char);
begin
  if fCount = fLinesMax then
  begin
    fLinesMax := NextGrow(fLinesMax);
    ReallocMem(fLines, fLinesMax * SizeOf(pointer));
  end;
  fLines[fCount] := LineBeg;
  inc(fCount);
end;

procedure ParseLines(P, PEnd: PUtf8Char; Map: TMemoryMapText);
var
  PBeg: PUtf8Char;
begin
  // generated asm is much better with a local proc
  if P < PEnd then
  repeat
    PBeg := P;
    {$ifdef CPUX64}
    inc(P, BufferLineLength(P, PEnd)); // use branchless SSE2 on x86_64
    {$else}
    while (P < PEnd) and
          (P^ <> #13) and
          (P^ <> #10) do
      inc(P);
    {$endif CPUX64}
    Map.ProcessOneLine(PBeg, P);
    if P + 1 < PEnd then
      if PWord(P)^ = 13 + 10 shl 8 then
      begin
        inc(P, 2); // ignore #13#10
        if P < PEnd then
          continue;
      end
      else
      begin
        inc(P);    // ignore #13 or #10
        if P < PEnd then
          continue;
      end;
    break;
  until false;
end;

procedure TMemoryMapText.LoadFromMap(AverageLineLength: integer = 32);
var
  P: PUtf8Char;
begin
  if fMap.Buffer = nil then
    exit;
  fLinesMax := fMap.FileSize div AverageLineLength + 8;
  GetMem(fLines, fLinesMax * SizeOf(pointer));
  P := pointer(fMap.Buffer);
  fMapEnd := P + fMap.Size;
  if (PWord(P)^ = $BBEF) and
     (P[2] = #$BF) then
    inc(P, 3); // ignore UTF-8 BOM
  ParseLines(P, fMapEnd, self);
  if fLinesMax > fCount + 16384 then
    Reallocmem(fLines, fCount * SizeOf(pointer)); // size down only if worth it
end;

procedure TMemoryMapText.AddInMemoryLine(const aNewLine: RawUtf8);
var
  P: PUtf8Char;
begin
  if aNewLine = '' then
    exit;
  AddRawUtf8(fAppendedLines, fAppendedLinesCount, aNewLine);
  P := pointer(fAppendedLines[fAppendedLinesCount - 1]);
  ProcessOneLine(P, P + StrLen(P));
end;

procedure TMemoryMapText.AddInMemoryLinesClear;
begin
  dec(fCount, fAppendedLinesCount);
  fAppendedLinesCount := 0;
  fAppendedLines := nil;
end;

procedure AppendCharOnceToRawUtf8(var Text: RawUtf8; Ch: AnsiChar);
var
  L: PtrInt;
begin
  L := length(Text);
  if (L <> 0) and
     (Text[L] = Ch) then
    exit;
  SetLength(Text, L + 1);
  PByteArray(Text)[L] := ord(Ch);
end;

procedure AppendBuffersToRawUtf8(var Text: RawUtf8; const Buffers: array of PUtf8Char);
var
  i, len, TextLen: PtrInt;
  lens: array[0..63] of integer;
  P: PUtf8Char;
begin
  if high(Buffers) > high(lens) then
    raise EBufferException.Create('Too many params in AppendBuffersToRawUtf8()');
  len := 0;
  for i := 0 to high(Buffers) do
  begin
    lens[i] := StrLen(Buffers[i]);
    inc(len, lens[i]);
  end;
  TextLen := Length(Text);
  SetLength(Text, TextLen + len);
  P := pointer(Text);
  inc(P, TextLen);
  for i := 0 to high(Buffers) do
    if Buffers[i] <> nil then
    begin
      MoveFast(Buffers[i]^, P^, {%H-}lens[i]);
      inc(P, lens[i]);
    end;
end;

function AppendRawUtf8ToBuffer(Buffer: PUtf8Char; const Text: RawUtf8): PUtf8Char;
var
  L: PtrInt;
begin
  L := length(Text);
  if L <> 0 then
  begin
    MoveFast(Pointer(Text)^, Buffer^, L);
    inc(Buffer, L);
  end;
  result := Buffer;
end;

function Append999ToBuffer(Buffer: PUtf8Char; Value: PtrUInt): PUtf8Char;
var
  L: PtrInt;
  P: PAnsiChar;
begin
  P := pointer(SmallUInt32Utf8[Value]);
  L := PStrLen(P - _STRLEN)^;
  MoveByOne(P, Buffer, L);
  result := Buffer + L;
end;

function AppendBufferToBuffer(Buffer: PUtf8Char; Text: pointer; Len: PtrInt): PUtf8Char;
begin
  MoveFast(Text^, Buffer^, Len);
  result := Buffer + Len;
end;

function AppendUInt32ToBuffer(Buffer: PUtf8Char; Value: PtrUInt): PUtf8Char;
var
  L: PtrInt;
  P: PAnsiChar;
  tmp: array[0..23] of AnsiChar;
begin
  {$ifndef ASMINTEL} // our StrUInt32 asm has less CPU cache pollution
  if Value <= high(SmallUInt32Utf8) then
  begin
    P := pointer(SmallUInt32Utf8[Value]);
    L := PStrLen(P - _STRLEN)^;
    MoveByOne(P, Buffer, L);
  end
  else
  {$endif ASMINTEL}
  begin
    P := StrUInt32(@tmp[23], Value);
    L := @tmp[23] - P;
    MoveFast(P^, Buffer^, L);
  end;
  result := Buffer + L;
end;

function Plural(const itemname: ShortString; itemcount: cardinal): ShortString;
var
  len, L: PtrInt;
begin
  len := (AppendUInt32ToBuffer(@result[1], itemcount) - PUtf8Char(@result[1])) + 1;
  result[len] := ' ';
  L := ord(itemname[0]);
  if (L > 0) and
     (L <= 240) then
  begin
    // avoid buffer overflow
    MoveFast(itemname[1], result[len + 1], L);
    inc(len, L);
    if itemcount > 1 then
    begin
      inc(len);
      result[len] := 's';
    end;
  end;
  result[0] := AnsiChar(len);
end;

function EscapeBuffer(s: PAnsiChar; slen: integer;
  d: PAnsiChar; dmax: integer): PAnsiChar;
var
  c: AnsiChar;
  tab: PWordArray;
begin
  if (slen > 0) and
     (dmax > 7) then
  begin
    tab := @TwoDigitsHexWBLower;
    repeat
      c := s^;
      inc(s);
      if (c >= ' ') and
         (c <= #126) then
      begin
        d^ := c;
        inc(d);
        dec(dmax);
      end
      else
      begin
        d^ := '$';
        inc(d);
        PWord(d)^ := tab[ord(c)];
        inc(d, 2);
        dec(dmax, 3);
      end;
      if dmax <= 7 then // mark truncated
      begin
        PCardinal(d)^ := ord('.') + ord('.') shl 8 + ord('.') shl 16;
        inc(d, 3);
        break;
      end;
      dec(slen);
    until slen = 0;
  end;
  d^ := #0;
  result := d;
end;

function LogEscape(source: PAnsiChar; sourcelen: integer;
  var temp: TLogEscape; enabled: boolean): PAnsiChar;
begin
  if enabled then
  begin
    temp[0] := ' ';
    EscapeBuffer(source, sourcelen, @temp[1], SizeOf(temp) - 1);
  end
  else
    temp[0] := #0;
  result := @temp;
end;

function LogEscapeFull(const source: RawByteString): RawUtf8;
begin
  result := LogEscapeFull(pointer(source), length(source));
end;

function LogEscapeFull(source: PAnsiChar; sourcelen: integer): RawUtf8;
begin
  FastSetString(result{%H-}, sourcelen * 3); // worse case
  if sourcelen <> 0 then
    FakeLength(result, pointer(EscapeBuffer(
      pointer(result), sourcelen, pointer(result), sourcelen * 3)));
end;

function EscapeToShort(source: PAnsiChar; sourcelen: integer): ShortString;
begin
  result[0] := AnsiChar(
    EscapeBuffer(source, sourcelen, @result[1], 255) - @result[1]);
end;

function EscapeToShort(const source: RawByteString): ShortString;
begin
  result[0] := AnsiChar(
    EscapeBuffer(pointer(source), length(source), @result[1], 255) - @result[1]);
end;

function BinToSource(const ConstName, Comment: RawUtf8;
  Data: pointer; Len, PerLine: integer; const Suffix: RawUtf8): RawUtf8;
var
  W: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  if (Data = nil) or
     (Len <= 0) or
     (PerLine <= 0) then
    result := ''
  else
  begin
    W := TTextWriter.CreateOwnedStream(temp,
      Len * 5 + 50 + length(Comment) + length(Suffix));
    try
      BinToSource(W, ConstName, Comment, Data, Len, PerLine);
      if Suffix <> '' then
      begin
        W.AddString(Suffix);
        W.AddCR;
      end;
      W.SetText(result);
    finally
      W.Free;
    end;
  end;
end;

function BinToSource(const ConstName, Comment: RawUtf8;
  const Data: RawByteString; PerLine: integer; const Suffix: RawUtf8): RawUtf8;
begin
  result := BinToSource(ConstName, Comment, pointer(Data), length(Data), PerLine, Suffix);
end;

procedure BinToSource(Dest: TTextWriter; const ConstName, Comment: RawUtf8;
  Data: pointer; Len, PerLine: integer);
var
  line, i: integer;
  P: PByte;
begin
  if (Dest = nil) or
     (Data = nil) or
     (Len <= 0) or
     (PerLine <= 0) then
    exit;
  Dest.AddShorter('const');
  if Comment <> '' then
    Dest.Add(#13#10'  // %', [Comment]);
  Dest.Add(#13#10'  %: array[0..%] of byte = (', [ConstName, Len - 1]);
  P := pointer(Data);
  repeat
    if len > PerLine then
      line := PerLine
    else
      line := Len;
    Dest.AddShorter(#13#10'   ');
    for i := 1 to line do
    begin
      Dest.Add(' ', '$');
      Dest.AddByteToHexLower(P^);
      inc(P);
      Dest.AddComma;
    end;
    dec(Len,line);
  until Len = 0;
  Dest.CancelLastComma;
  Dest.Add(');'#13#10'  %_LEN = SizeOf(%);'#13#10, [ConstName, ConstName]);
end;

function BinToHumanHex(Data: PByte; Len, PerLine, LeftTab: integer;
  SepChar: AnsiChar): RawUtf8;
var
  w: TTextWriter;
  temp: TTextWriterStackBuffer;
begin
  w := TTextWriter.CreateOwnedStream(temp);
  try
    BinToHumanHex(w, Data, Len, PerLine, LeftTab, SepChar);
    w.SetText(result);
  finally
    w.Free;
  end;
end;

procedure BinToHumanHex(W: TTextWriter; Data: PByte;
  Len, PerLine, LeftTab: integer; SepChar: AnsiChar);
var
  n: integer;
begin
  if Data <> nil then
    while Len > 0 do
    begin
      W.AddChars(' ', LeftTab);
      n := PerLine;
      repeat
        W.AddByteToHexLower(Data^);
        inc(Data);
        W.Add(SepChar);
        dec(Len);
        if Len = 0 then
          break;
        dec(n);
      until n = 0;
      W.CancelLastChar;
      W.AddCR;
    end;
end;


{ *************************** TStreamRedirect and other Hash process }

{ TProgressInfo }

procedure TProgressInfo.Init;
begin
  Finalize(Context);
  FillCharFast(self, SizeOf(self), 0); // warning: would overlap custom options
  StartTix := GetTickCount64;
  ReportDelay := 1000; // DoReport() will notify every second
end;

procedure TProgressInfo.DoStart(
  Sender: TObject; SizeExpected: Int64; const Ident: string);
begin
  CurrentSize := 0; // no Init because would overlap customized options
  ProcessedSize := 0;
  ExpectedSize := SizeExpected;
  ExpectedWrittenSize := SizeExpected;
  StringToUtf8(Ident, Context);
  StartTix := GetTickCount64;
  ReportTix := 0;
  Elapsed := 0;
  DoReport(Sender, {computeelapsed=}false);
end;

procedure TProgressInfo.DoAfter(Sender: TObject; ChunkSize: Int64);
begin
  inc(CurrentSize, ChunkSize);
  inc(ProcessedSize, ChunkSize);
  DoReport(Sender, {computeelapsed=}true);
end;

procedure TProgressInfo.SetExpectedSize(SizeExpected, Position: Int64);
begin
  ExpectedSize := SizeExpected;
  ExpectedWrittenSize := SizeExpected - Position;
end;

function TProgressInfo.DoReport(Sender: TObject; ReComputeElapsed: boolean): boolean;
begin
  if ReComputeElapsed then
    Elapsed := GetTickCount64 - StartTix; // may have changed in-between
  if (CurrentSize <> ExpectedSize) and
     (Elapsed < ReportTix) then
  begin
    result := false; // nothing to report yet
    exit;
  end;
  LastProgress := '';
  ReportTix := Elapsed + ReportDelay; // notify once per second or when finished
  if ExpectedSize = 0 then
    Percent := 0
  else if CurrentSize >= ExpectedSize then
  begin
    Percent := 100;
    Remaining := 0;
  end
  else
  begin
    if (Elapsed <> 0) and
       (ProcessedSize <> 0) then
      Remaining :=
        (Elapsed * (ExpectedWrittenSize - ProcessedSize)) div ProcessedSize;
    Percent := (CurrentSize * 100) div ExpectedSize;
  end;
  if Elapsed = 0 then
    PerSecond := 0
  else
    PerSecond := (ProcessedSize * 1000) div Elapsed;
  if Assigned(OnLog) then
    OnLog(sllTrace, '%', [GetProgress], Sender);
  if Assigned(OnProgress) then
    OnProgress(Sender, @self);
  result := true;
end;

function TProgressInfo.GetProgress: RawUtf8;
var
  ctx, remain: ShortString;
  persec, expect, curr: TShort16;
begin
  result := LastProgress;
  if result <> '' then
    exit;
  Ansi7StringToShortString(Context, ctx);
  if ctx[0] > #30 then
  begin
    ctx[0] := #33; // truncate to keep information on a single line
    PCardinal(@ctx[30])^ := ord('.') + ord('.') shl 8 + ord('.') shl 16;
  end;
  persec := '';
  if PerSecond <> 0 then
    FormatShort16(' %/s', [KBNoSpace(PerSecond)], persec);
  KB(CurrentSize, curr, {nospace=}true);
  if ExpectedSize = 0 then
    // size may not be known (e.g. server-side chunking)
    FormatUtf8('% % read% ...', [ctx, curr, persec], result)
  else
  begin
    KB(ExpectedSize, expect, {nospace=}true);
    if CurrentSize < ExpectedSize then
    begin
      // we can state the current progression ratio
      remain := '';
      if Remaining > 0 then
        FormatShort(' remaining:%', [MilliSecToString(Remaining)], remain);
      FormatUtf8('% %% %/%%%',
        [ctx, Percent, '%', curr, expect, persec, remain], result)
    end
    else
      // process is finished
      if (Elapsed = 0) or
         (PerSecond = 0) then
        FormatUtf8('% % done' + CRLF, [Context, expect], result)
      else
        FormatUtf8('% % done in % (% )' + CRLF,
          [Context, expect, MilliSecToString(Elapsed), persec], result);
  end;
  LastProgress := result;
end;


{ TStreamRedirect }

constructor TStreamRedirect.Create(aRedirected: TStream; aRead: boolean);
begin
  fInfo.Init;
  fRedirected := aRedirected;
  if aRead and
     Assigned(aRedirected) then
    SetExpectedSize(aRedirected.Size); // needed e.g. to upload a file
end;

destructor TStreamRedirect.Destroy;
begin
  fRedirected.Free;
  inherited Destroy;
end;

function TStreamRedirect.GetProgress: RawUtf8;
begin
  if (self = nil) or
     fTerminated then
    result := ''
  else
    result := fInfo.GetProgress;
end;

function TStreamRedirect.GetSize: Int64;
begin
  if (fMode <> mWrite) and
     (fInfo.ExpectedSize <> 0) then
    result := fInfo.ExpectedSize
  else
    result := fInfo.CurrentSize;
end;

procedure TStreamRedirect.SetSize(NewSize: Longint);
begin
  raise EStreamRedirect.CreateUtf8('%.Size is read/only', [self]);
end;

procedure TStreamRedirect.SetSize(const NewSize: Int64);
begin
  raise EStreamRedirect.CreateUtf8('%.Size is read/only', [self]);
end;

class procedure TStreamRedirect.ProgressStreamToConsole(Sender: TStreamRedirect);
begin
  if (Sender <> nil) and
     Sender.InheritsFrom(TStreamRedirect) then
    ProgressInfoToConsole(Sender, @Sender.fInfo);
end;

{$I-}
class procedure TStreamRedirect.ProgressInfoToConsole(
  Sender: TObject; Info: PProgressInfo);
var
  eraseline: ShortString;
  msg: RawUtf8;
begin
  eraseline[0] := AnsiChar(Info.ConsoleLen + 2);
  eraseline[1] := #13;
  FillCharFast(eraseline[2], ord(eraseline[0]) - 2, 32);
  eraseline[ord(eraseline[0])] := #13;
  system.write(eraseline);
  msg := Info.GetProgress;
  if length(msg) > 250 then
    FakeLength(msg, 250); // paranoid overflow check
  Info.ConsoleLen := length(msg); // to properly erase previous line
  system.write(msg);
  ioresult;
end;

class procedure TStreamRedirect.NotifyEnded(
  const OnStream: TOnStreamProgress; const OnInfo: TOnInfoProgress;
  const Fmt: RawUtf8; const Args: array of const; Size, StartedMs: Int64);
var
  tmp: TStreamRedirect;
  stop: Int64;
begin
  if not Assigned(OnStream) and
     not Assigned(OnInfo) then
    exit;
  QueryPerformanceMicroSeconds(stop);
  tmp := TStreamRedirect.Create(nil);
  try
    tmp.OnProgress := OnStream;
    tmp.OnInfoProgress := OnInfo;
    FormatUtf8(Fmt, Args, tmp.fInfo.Context);
    tmp.fInfo.ProcessedSize := Size;
    tmp.fInfo.CurrentSize := Size;
    if StartedMs <> 0 then
    begin
      tmp.fInfo.Elapsed := stop - StartedMs;
      dec(tmp.fInfo.StartTix, tmp.fInfo.Elapsed shr 10); // fake time
    end;
    tmp.Ended;
  finally
    tmp.Free;
  end;
end;

{$I+}

procedure TStreamRedirect.DoReport(ReComputeElapsed: boolean);
begin
  if fInfo.DoReport(self, ReComputeElapsed) then
    // DoReport did notify OnLog + OnInfoProgress
    if Assigned(fOnStreamProgress) then
      fOnStreamProgress(self);
end;

procedure TStreamRedirect.DoHash(data: pointer; len: integer);
begin // no associated hasher on this parent class
end;

procedure TStreamRedirect.SetExpectedSize(Value: Int64);
begin
  fInfo.SetExpectedSize(Value, fPosition);
end;

function TStreamRedirect.GetHash: RawUtf8;
begin
  result := ''; // no associated hasher on this parent class
end;

class function TStreamRedirect.GetHashFileExt: RawUtf8;
begin
  result := ''; // no associated hasher on this parent class
end;

class function TStreamRedirect.GetHashName: RawUtf8;
begin
  result := copy(GetHashFileExt, 2, 10);
end;

class function TStreamRedirect.HashFile(const FileName: TFileName;
  const OnProgress: TOnStreamProgress): RawUtf8;
var
  hasher: TStreamRedirect;
  f: THandle;
begin
  result := '';
  if GetHashFileExt = '' then
    exit; // no hash function defined
  f := FileOpenSequentialRead(FileName);
  if not ValidHandle(f) then
    exit;
  hasher := Create(TFileStreamFromHandle.Create(f));
  try
    if Assigned(OnProgress) then
    begin
      hasher.fInfo.ExpectedSize := FileSize(f);
      hasher.OnProgress := OnProgress;
    end;
    hasher.Append;
    result := hasher.GetHash;
  finally
    hasher.Free; // includes FileClose(f)
  end;
end;

procedure TStreamRedirect.Append;
var
  buf: RawByteString;
  read: PtrInt;
begin
  if fRedirected = nil then
    raise EStreamRedirect.CreateUtf8('%.Append(%): Redirected=nil',
      [self, fInfo.Context]);
  if fMode = mRead then
    raise EStreamRedirect.CreateUtf8('%.Append(%) after Read()',
      [self, fInfo.Context]);
  fMode := mWrite;
  if GetHashFileExt = '' then // DoHash() does nothing
  begin
    // no hash involved: just move to the end of partial content
    fInfo.CurrentSize := fRedirected.Seek(0, soEnd);
    fPosition := fInfo.CurrentSize;
  end
  else
  begin
    // compute the hash of the existing partial content
    FastNewRawByteString(buf, 1 shl 20); // 1MB temporary buffer
    repeat
      read := fRedirected.Read(pointer(buf)^, length(buf));
      if read <= 0 then
        break;
      DoHash(pointer(buf), read);
      inc(fInfo.CurrentSize, read);
      inc(fPosition, read);
      if Assigned(fOnStreamProgress) or
         Assigned(fInfo.OnProgress) or
         Assigned(fInfo.OnLog) then
        if (fInfo.ExpectedSize <> 0) and
           (fInfo.CurrentSize <> read) then
          DoReport(true);
    until false;
  end;
end;

procedure TStreamRedirect.Ended;
begin
  if fInfo.CurrentSize = fInfo.ExpectedSize then
    exit; // nothing to report
  fInfo.ExpectedSize := fInfo.CurrentSize; // reached 100%
  if Assigned(fOnStreamProgress) or
     Assigned(fInfo.OnProgress) or
     Assigned(fInfo.OnLog) then
    DoReport(true); // notify finished
end;

procedure TStreamRedirect.Terminate;
begin
  fTerminated := true;
end;

procedure TStreamRedirect.ReadWriteHash(const Buffer; Count: integer);
begin
  DoHash(@Buffer, Count);
  inc(fInfo.CurrentSize, Count);
  inc(fInfo.ProcessedSize, Count);
  inc(fPosition, Count);
end;

procedure TStreamRedirect.ReadWriteReport(const Caller: ShortString);
var
  tix, tosleep, endsleep: Int64;
begin
  tix := GetTickCount64;
  fInfo.Elapsed := tix - fInfo.StartTix;
  if (fLimitPerSecond <> 0) or
     (fTimeOut <> 0) then
  begin
    if tix shr 7 <> fLastTix shr 7 then // checking every 128 ms is good enough
    begin
      fLastTix := tix;
      if fInfo.Elapsed > 0 then
      begin
        if (fTimeOut <> 0) and
           (fInfo.Elapsed > fTimeOut) then
          raise EStreamRedirect.CreateUtf8('%.%(%) timeout after %',
            [self, Caller, fInfo.Context, MilliSecToString(fInfo.Elapsed)]);
        if fLimitPerSecond > 0 then
        begin
          // adjust bandwidth limit every 128 ms by adding some sleep() steps
          tosleep := ((fInfo.ProcessedSize * 1000) div fLimitPerSecond) - fInfo.Elapsed;
          if tosleep > 10 then // on Windows, typical resolution is 16ms
          begin
            if tosleep > 300 then
            begin
              endsleep := tix + tosleep;
              repeat
                SleepHiRes(300); // show progress on very low bandwidth
                if Assigned(fOnStreamProgress) or
                   Assigned(fInfo.OnProgress) or
                   Assigned(fInfo.OnLog) then
                  DoReport({ReComputeElapsed=}true);
                tosleep := endsleep - GetTickCount64;
              until tosleep < 300;
            end;
            if tosleep > 10 then
              SleepHiRes(tosleep);
          end;
        end;
      end;
    end;
  end;
  if Assigned(fOnStreamProgress) or
     Assigned(fInfo.OnProgress) or
     Assigned(fInfo.OnLog) then
    DoReport(false);
  if fTerminated then
    raise EStreamRedirect.CreateUtf8('%.%(%) Terminated',
      [self, Caller, fInfo.Context]);
end;

function TStreamRedirect.Read(var Buffer; Count: Longint): Longint;
begin
  if fMode = mWrite then
    raise EStreamRedirect.CreateUtf8('%.Read(%) in Write() mode',
      [self, fInfo.Context]);
  fMode := mRead;
  if fRedirected = nil then
    raise EStreamRedirect.CreateUtf8('%.Read(%) with Redirected=nil',
      [self, fInfo.Context]);
  result := fRedirected.Read(Buffer, Count);
  ReadWriteHash(Buffer, result);
  ReadWriteReport('Read');
end;

function TStreamRedirect.Write(const Buffer; Count: Longint): Longint;
begin
  if fMode = mRead then
    raise EStreamRedirect.CreateUtf8('%.Write(%) in Read() mode',
      [self, fInfo.Context]);
  fMode := mWrite;
  ReadWriteHash(Buffer, Count);
  result := Count;
  if fRedirected = nil then
    exit; // we may just want the hash
  fRedirected.WriteBuffer(Buffer, Count);
  ReadWriteReport('Write');
end;


{ TStreamRedirectHasher }

function TStreamRedirectHasher.GetHash: RawUtf8;
begin
  result := CardinalToHexLower(fHash);
end;


{ TStreamRedirectCrc32c }

procedure TStreamRedirectCrc32c.DoHash(data: pointer; len: integer);
begin
  fHash := crc32c(fHash, data, len);
end;

class function TStreamRedirectCrc32c.GetHashFileExt: RawUtf8;
begin
  result := '.crc32c';
end;


{ TFakeWriterStream }

function TFakeWriterStream.Read(var Buffer; Count: Longint): Longint;
begin
  // do nothing
  result := Count;
end;

function TFakeWriterStream.Write(const Buffer; Count: Longint): Longint;
begin
  // do nothing
  inc(fWritten, Count);
  result := Count;
end;

{$ifdef FPC}
function TFakeWriterStream.GetPosition: Int64;
begin
  result := fWritten;
end;
{$endif FPC}

function TFakeWriterStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  result := Seek(Offset, TSeekOrigin(Origin));
end;

function TFakeWriterStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      result := Offset;
    soEnd:
      result := fWritten - Offset;
    else
      result := fWritten + Offset;
  end;
  if result > fWritten then
    result := fWritten
  else if result < 0 then
    result := 0
  else if result < fWritten then
    fWritten := result;
end;


{ TNestedStreamReader }

destructor TNestedStreamReader.Destroy;
var
  i: PtrInt;
begin
  inherited Destroy;
  for i := 0 to length(fNested) - 1 do
    fNested[i].Stream.Free;
end;

function TNestedStreamReader.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Offset = 0) and
     (Origin = soBeginning) then
    Flush; // allow to read the file again, and set nested stream sizes
  result := inherited Seek(Offset, Origin);
end;

function TNestedStreamReader.NewStream(Stream: TStream): TStream;
var
  n: PtrInt;
begin
  n := length(fNested);
  SetLength(fNested, n + 1);
  fNested[n].Stream := Stream;
  result := Stream; // allow simple fluent calls
end;

function TNestedStreamReader.ForText: TRawByteStringStream;
var
  n: PtrInt;
begin
  n := length(fNested);
  if n <> 0 then
  begin
    result := pointer(fNested[n - 1].Stream);
    if PClass(result)^ = TRawByteStringStream then
      exit;
  end;
  result := TRawByteStringStream.Create;
  NewStream(result);
end;

procedure TNestedStreamReader.Append(const Content: RawByteString);
begin
  with ForText do
    DataString := DataString + Content; // the fast and easy way
end;

procedure TNestedStreamReader.Flush;
var
  i, n: PtrInt;
begin
  fContentRead := pointer(fNested);
  fSize := 0;
  n := length(fNested);
  for i := 0 to n - 1 do
    with fNested[i] do
    begin
      Stream.Seek(0, soBeginning);
      Start := fSize;
      inc(fSize, Stream.Size); // to allow proper Seek() + Read()
      Stop := fSize;
    end;
end;

function TNestedStreamReader.Read(var Buffer; Count: Longint): Longint;
var
  s, m: ^TNestedStream;
  P: PByte;
  rd: PtrInt;
begin
  result := 0;
  s := pointer(fContentRead);
  if s = nil then
    exit; // Flush was not called
  P := @Buffer;
  m := @fNested[length(fNested)];
  while (Count > 0) and
        (fPosition < fSize) do
  begin
    if (PtrUInt(s) >= PtrUInt(m)) or
       (fPosition >= s^.Stop) or
       (fPosition < s^.Start) then
    begin
      inc(s); // optimize forward reading (most common case)
      if (PtrUInt(s) >= PtrUInt(m)) or
         (fPosition >= s^.Stop) or
         (fPosition < s^.Start) then
      begin
        // handle random Seek() call - brute force is enough (seldom used)
        s := pointer(fNested);
        repeat
          if fPosition >= s^.Start then
            break;
          inc(s);
        until PtrUInt(s) >= PtrUInt(m);
        if PtrUInt(s) >= PtrUInt(m) then
          break; // paranoid (we know fPosition < fSize)
      end;
    end;
    rd := s^.Stream.Read(P^, Count);
    if rd <= 0 then
    begin
      // read from next section(s) until we got Count bytes
      inc(s);
      if PtrUInt(s) >= PtrUInt(m) then
        break;
      continue;
    end;
    dec(Count, rd);
    inc(P, rd);
    inc(fPosition, rd);
    inc(result, rd);
  end;
  fContentRead := pointer(s);
end;

function TNestedStreamReader.Write(const Buffer; Count: Longint): Longint;
begin
  result := RaiseStreamError(self, 'Write');
end;


{ TBufferedStreamReader }

constructor TBufferedStreamReader.Create(aSource: TStream; aBufSize: integer);
begin
  FastNewRawByteString(fBuffer, aBufSize);
  fSource := aSource;
  fSize := fSource.Size; // get it once
  fSource.Seek(0, soBeginning);
end;

constructor TBufferedStreamReader.Create(const aSourceFileName: TFileName;
  aBufSize: integer);
begin
  Create(TFileStreamEx.Create(aSourceFileName, fmOpenReadShared));
  fOwnStream := fSource;
end;

destructor TBufferedStreamReader.Destroy;
begin
  inherited Destroy;
  fOwnStream.Free;
end;

function TBufferedStreamReader.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  prev: Int64;
begin
  prev := fPosition;
  result := inherited Seek(Offset, Origin);
  if prev <> result then
  begin
    fSource.Seek(result, soBeginning);
    fBufferLeft := 0; // deprecate buffer content
  end;
end;

function TBufferedStreamReader.Read(var Buffer; Count: Longint): Longint;
var
  dest: PAnsiChar;
  avail: integer;
begin
  result := 0;
  if Count <= 0 then
    exit;
  if fPosition + Count > fSize then
    Count := fSize - fPosition;
  dest := @Buffer;
  while Count <> 0 do
  begin
    avail := fBufferLeft;
    if avail > Count then
      avail := Count;
    if avail <> 0 then
    begin
      MoveFast(fBufferPos^, dest^, avail);
      inc(fBufferPos, avail);
      dec(fBufferLeft, avail);
      inc(result, avail);
      dec(Count, avail);
      if Count = 0 then
        break;
      inc(dest, avail);
    end;
    if Count > length(fBuffer) then
    begin // big requests would read directly from stream
      inc(result, fSource.Read(dest^, Count));
      break;
    end;
    fBufferPos := pointer(fBuffer); // fill buffer and retry
    fBufferLeft := fSource.Read(fBufferPos^, length(fBuffer));
    if fBufferLeft <= 0 then
      break;
  end;
  inc(fPosition, result);
end;

function TBufferedStreamReader.Write(const Buffer; Count: Longint): Longint;
begin
  result := RaiseStreamError(self, 'Write');
end;



function HashFile(const FileName: TFileName; Hasher: THasher): cardinal;
var
  buf: array[word] of cardinal; // 256KB of buffer
  read: integer;
  f: THandle;
begin
  if not Assigned(Hasher) then
    Hasher := DefaultHasher;
  result := 0;
  f := FileOpenSequentialRead(FileName);
  if ValidHandle(f) then
  begin
    repeat
      read := FileRead(f, buf, SizeOf(buf));
      if read <= 0 then
        break;
      result := Hasher(result, @buf, read);
    until false;
    FileClose(f);
  end;
end;

function SameFileContent(const One, Another: TFileName): boolean;
var
  b1, b2: array[word] of word; // 2 * 128KB of buffers
  r1, r2: integer;
  f1, f2: THandle;
begin
  f1 := FileOpenSequentialRead(One);
  f2 := FileOpenSequentialRead(Another);
  result := false;
  if ValidHandle(f1) and
     ValidHandle(f2) and
     (FileSize(f1) = FileSize(f2)) then
    repeat
      r1 := FileRead(f1, b1, SizeOf(b1));
      r2 := FileRead(f2, b2, SizeOf(b2));
      if (r1 <= 0) or (r2 <= 0) then
      begin
        result := (r1 <= 0) = (r2 <= 0);
        break;
      end;
    until (r1 <> r2) or
          not CompareMem(@b1, @b2, r1);
  if ValidHandle(f2) then
    FileClose(f2);
  if ValidHandle(f1) then
    FileClose(f1);
end;


function HashFileCrc32c(const FileName: TFileName): RawUtf8;
begin
  result := CardinalToHexLower(HashFile(FileName, crc32c));
end;

function GetStreamBuffer(S: TStream): pointer;
begin
  if S.InheritsFrom(TRawByteStringStream) then
    result := pointer(TRawByteStringStream(S).DataString)
  else if S.InheritsFrom(TCustomMemoryStream) then
    result := TCustomMemoryStream(S).Memory
  else
    result := nil;
end;

function IsStreamBuffer(S: TStream): boolean;
begin
  result := S.InheritsFrom(TRawByteStringStream) or
            S.InheritsFrom(TCustomMemoryStream);
end;


{ ************* Markup (e.g. HTML or Emoji) process }

{ internal TTextWriterEscape class }

type
  TTextWriterEscapeStyle = (
    tweBold,
    tweItalic,
    tweCode);

  TTextWriterEscapeLineStyle = (
    twlNone,
    twlParagraph,
    twlOrderedList,
    twlUnorderedList,
    twlBlockquote,
    twlCode4,
    twlCode3);

  {$ifdef USERECORDWITHMETHODS}
  TTextWriterEscape = record
  {$else}
  TTextWriterEscape = object
  {$endif USERECORDWITHMETHODS}
  public
    P, B, P2, B2: PUtf8Char;
    W: TTextWriter;
    st: set of TTextWriterEscapeStyle;
    fmt: TTextWriterHtmlFormat;
    esc: TTextWriterHtmlEscape;
    lst: TTextWriterEscapeLineStyle;
    procedure Start(dest: TTextWriter; src: PUtf8Char; escape: TTextWriterHtmlEscape);
    function ProcessText(const stopchars: TSynByteSet): AnsiChar;
    procedure ProcessHRef;
    function ProcessLink: boolean;
    procedure ProcessEmoji;
      {$ifdef HASINLINE}inline;{$endif}
    procedure Toggle(style: TTextWriterEscapeStyle);
    procedure SetLine(style: TTextWriterEscapeLineStyle);
    procedure EndOfParagraph;
    procedure NewMarkdownLine;
    procedure AddHtmlEscapeWiki(dest: TTextWriter; src: PUtf8Char;
      escape: TTextWriterHtmlEscape);
    procedure AddHtmlEscapeMarkdown(dest: TTextWriter; src: PUtf8Char;
      escape: TTextWriterHtmlEscape);
  end;

procedure TTextWriterEscape.Start(dest: TTextWriter; src: PUtf8Char;
  escape: TTextWriterHtmlEscape);
begin
  P := src;
  W := dest;
  st := [];
  if heHtmlEscape in escape then
    fmt := hfOutsideAttributes
  else
    fmt := hfNone;
  esc := escape;
  lst := twlNone;
end;

function IsHttpOrHttps(P: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := (PCardinal(P)^ =
             ord('h') + ord('t') shl 8 + ord('t') shl 16 + ord('p') shl 24) and
            ((PCardinal(P + 4)^ and $ffffff =
             ord(':') + ord('/') shl 8 + ord('/') shl 16) or
             (PCardinal(P + 4)^ =
             ord('s') + ord(':') shl 8 + ord('/') shl 16 + ord('/') shl 24));
end;

function TTextWriterEscape.ProcessText(const stopchars: TSynByteSet): AnsiChar;
begin
  if P = nil then
  begin
    result := #0;
    exit;
  end;
  B := P;
  while not (ord(P^) in stopchars) and
        not IsHttpOrHttps(P) do
    inc(P);
  W.AddHtmlEscape(B, P - B, fmt);
  result := P^;
end;

procedure TTextWriterEscape.ProcessHRef;
begin
  B := P;
  while P^ > ' ' do
    inc(P);
  W.AddShort('<a href="');
  W.AddHtmlEscape(B, P - B, hfWithinAttributes);
  W.AddShort('" rel="nofollow">');
  W.AddHtmlEscape(B, P - B);
  W.AddShorter('</a>');
end;

function TTextWriterEscape.ProcessLink: boolean;
begin
  inc(P);
  B2 := P;
  while not (P^ in [#0, ']']) do
    inc(P);
  P2 := P;
  if PWord(P)^ = ord(']') + ord('(') shl 8 then
  begin
    inc(P, 2);
    B := P;
    while not (P^ in [#0, ')']) do
      inc(P);
    if P^ = ')' then
    begin
      // [GitHub](https://github.com)
      result := true;
      exit;
    end;
  end;
  P := B2; // rollback
  result := false;
end;

procedure TTextWriterEscape.ProcessEmoji;
begin
  if heEmojiToUtf8 in esc then
    EmojiParseDots(P, W)
  else
  begin
    W.Add(':');
    inc(P);
  end;
end;

procedure TTextWriterEscape.Toggle(style: TTextWriterEscapeStyle);
const
  HTML: array[tweBold..tweCode] of string[7] = (
    'strong>', 'em>', 'code>');
begin
  W.Add('<');
  if style in st then
  begin
    W.Add('/');
    exclude(st, style);
  end
  else
    include(st, style);
  W.AddShorter(HTML[style]);
end;

procedure TTextWriterEscape.EndOfParagraph;
begin
  if tweBold in st then
    Toggle(tweBold);
  if tweItalic in st then
    Toggle(tweItalic);
  if P <> nil then
    if PWord(P)^ = $0a0d then
      inc(P, 2)
    else
      inc(P);
end;

procedure TTextWriterEscape.SetLine(style: TTextWriterEscapeLineStyle);
const
  HTML: array[twlParagraph..twlCode3] of string[5] = (
    'p>', 'li>', 'li>', 'p>', 'code>', 'code>');
  HTML2: array[twlOrderedList..twlCode3] of string[11] = (
    'ol>', 'ul>', 'blockquote>', 'pre>', 'pre>');
begin
  if lst >= low(HTML) then
  begin
    if (lst < twlCode4) or
       (lst <> style) then
    begin
      W.Add('<', '/');
      W.AddShorter(HTML[lst]);
    end;
    if (lst >= low(HTML2)) and
       (lst <> style) then
    begin
      W.Add('<', '/');
      W.AddShort(HTML2[lst]);
    end;
  end;
  if style >= low(HTML) then
  begin
    if (style >= low(HTML2)) and
       (lst <> style) then
    begin
      W.Add('<');
      W.AddShort(HTML2[style]);
    end;
    if (style < twlCode4) or
       (lst <> style) then
    begin
      W.Add('<');
      W.AddShorter(HTML[style]);
    end;
  end;
  lst := style;
end;

procedure TTextWriterEscape.NewMarkdownLine;
label
  none;
var
  c: cardinal;
begin
  if P = nil then
    exit;
  c := PCardinal(P)^;
  if c and $ffffff = ord('`') + ord('`') shl 8 + ord('`') shl 16 then
  begin
    inc(P, 3);
    if lst = twlCode3 then
    begin
      lst := twlCode4; // to close </code></pre>
      NewMarkdownLine;
      exit;
    end;
    SetLine(twlCode3);
  end;
  if lst = twlCode3 then
    exit; // no prefix process within ``` code blocks
  if c = $20202020 then
  begin
    SetLine(twlCode4);
    inc(P, 4);
    exit;
  end;
  P := GotoNextNotSpaceSameLine(P); // don't implement nested levels yet
  case P^ of
    '*',
    '+',
    '-':
      if P[1] = ' ' then
        SetLine(twlUnorderedList)
      else
        goto none;
    '1'..'9':
      begin
        // first should be 1. then any ##. number to continue
        B := P;
        repeat
          inc(P)
        until not (P^ in ['0'..'9']);
        if (P^ = '.') and
           ((lst = twlOrderedList) or
            (PWord(B)^ = ord('1') + ord('.') shl 8)) then
          SetLine(twlOrderedList)
        else
        begin
          P := B;
none:     if lst = twlParagraph then
          begin
            c := PWord(P)^; // detect blank line to separate paragraphs
            if c = $0a0d then
              inc(P, 2)
            else if c and $ff = $0a then
              inc(P)
            else
            begin
              W.AddOnce(' ');
              exit;
            end;
          end;
          SetLine(twlParagraph);
          exit;
        end;
      end;
    '>':
      if P[1] = ' ' then
        SetLine(twlBlockquote)
      else
        goto none;
  else
    goto none;
  end;
  P := GotoNextNotSpaceSameLine(P + 1);
end;

procedure TTextWriterEscape.AddHtmlEscapeWiki(dest: TTextWriter;
  src: PUtf8Char; escape: TTextWriterHtmlEscape);
begin
  Start(dest, src, escape);
  SetLine(twlParagraph);
  repeat
    case ProcessText([0, 10, 13,
                      ord('*'), ord('+'), ord('`'), ord('\'), ord(':')]) of
      #0:
        break;
      #10,
      #13:
        begin
          EndOfParagraph;
          SetLine(twlParagraph);
          continue;
        end;
      '\':
        if P[1] in ['\', '`', '*', '+'] then
        begin
          inc(P);
          W.Add(P^);
        end
        else
          W.Add('\');
      '*':
        Toggle(tweItalic);
      '+':
        Toggle(tweBold);
      '`':
        Toggle(tweCode);
      'h':
        begin
          ProcessHRef;
          continue;
        end;
      ':':
        begin
          ProcessEmoji;
          continue;
        end;
    end;
    inc(P);
  until false;
  EndOfParagraph;
  SetLine(twlNone);
end;

procedure TTextWriterEscape.AddHtmlEscapeMarkdown(dest: TTextWriter;
  src: PUtf8Char; escape: TTextWriterHtmlEscape);
begin
  Start(dest, src, escape);
  NewMarkDownLine;
  repeat
    if lst >= twlCode4 then // no Markdown tags within code blocks
      if ProcessText([0, 10, 13]) = #0 then
        break
      else
      begin
        if PWord(P)^ = $0a0d then
          inc(P, 2)
        else
          inc(P);
        W.AddCR; // keep LF within <pre>
        NewMarkdownLine;
        continue;
      end
    else
      case ProcessText([0, 10, 13, ord('*'), ord('_'), ord('`'),
                        ord('\'), ord('['), ord('!'), ord(':')]) of
        #0:
          break;
        #10,
        #13:
          begin
            EndOfParagraph;
            NewMarkdownLine;
            continue;
          end;
        '\':
          if P[1] in ['\', '`', '*', '_', '[', ']', '{', '}',
                      '(', ')', '#', '+', '-', '.', '!'] then
          begin
            // backslash escape
            inc(P);
            W.Add(P^);
          end
          else
            W.Add('\');
        '*',
        '_':
          if P[1] = P[0] then
          begin
            // **This text will be bold** or __This text will be bold__
            inc(P);
            Toggle(tweBold);
          end
          else
            // *This text will be italic* or _This text will be italic_
            Toggle(tweItalic);
        '`':
          // `This text will be code`
          Toggle(tweCode);
        '[':
          if ProcessLink then
          begin
            // [GitHub](https://github.com)
            W.AddShort('<a href="');
            W.AddHtmlEscape(B, P - B, hfWithinAttributes);
            if IsHttpOrHttps(B) then
              W.AddShort('" rel="nofollow">')
            else
              W.Add('"', '>');
            W.AddHtmlEscape(B2, P2 - B2, fmt);
            W.AddShorter('</a>'); // no continune -> need inc(P) over ending )
          end
          else
            // not a true link -> just append
            W.Add('[');
        '!':
          begin
            if P[1] = '[' then
            begin
              inc(P);
              if ProcessLink then
              begin
                W.AddShort('<img alt="');
                W.AddHtmlEscape(B2, P2 - B2, hfWithinAttributes);
                W.AddShorter('" src="');
                W.AddShort(B, P - B);
                W.AddShorter('">');
                inc(P);
                continue;
              end;
              dec(P);
            end;
            W.Add('!'); // not a true image
          end;
        'h':
          begin
            ProcessHRef;
            continue;
          end;
        ':':
          begin
            ProcessEmoji;
            continue;
          end;
      end;
    inc(P);
  until false;
  EndOfParagraph;
  SetLine(twlNone);
end;

function HtmlEscapeWiki(const wiki: RawUtf8; esc: TTextWriterHtmlEscape): RawUtf8;
var
  temp: TTextWriterStackBuffer;
  W: TTextWriter;
begin
  W := TTextWriter.CreateOwnedStream(temp);
  try
    AddHtmlEscapeWiki(W, pointer(wiki), esc);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function HtmlEscapeMarkdown(const md: RawUtf8; esc: TTextWriterHtmlEscape): RawUtf8;
var
  temp: TTextWriterStackBuffer;
  W: TTextWriter;
begin
  W := TTextWriter.CreateOwnedStream(temp);
  try
    AddHtmlEscapeMarkdown(W, pointer(md), esc);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure AddHtmlEscapeWiki(W: TTextWriter; P: PUtf8Char; esc: TTextWriterHtmlEscape);
var
  doesc: TTextWriterEscape;
begin
  doesc.AddHtmlEscapeWiki(W, P, esc);
end;

procedure AddHtmlEscapeMarkdown(W: TTextWriter; P: PUtf8Char; esc: TTextWriterHtmlEscape);
var
  doesc: TTextWriterEscape;
begin
  doesc.AddHtmlEscapeMarkdown(W, P, esc);
end;

function EmojiFromText(P: PUtf8Char; len: PtrInt): TEmoji;
begin
  // RTTI has shortstrings in adjacent L1 cache lines -> faster than EMOJI_TEXT[]
  result := TEmoji(FindShortStringListTrimLowerCase(
                     EMOJI_RTTI, ord(high(TEmoji)) - 1, P, len) + 1);
  // note: we may enhance performance by using FastFindPUtf8CharSorted()
end;

function EmojiParseDots(var P: PUtf8Char; W: TTextWriter): TEmoji;
var
  c: PUtf8Char;
begin
  result := eNone;
  inc(P); // ignore trailing ':'
  c := P;
  if c[-2] <= ' ' then
  begin
    if (c[1] <= ' ') and
       (c^ in ['('..'|']) then
      result := EMOJI_AFTERDOTS[c^]; // e.g. :)
    if result = eNone then
    begin
      while c^ in ['a'..'z', 'A'..'Z', '_'] do
        inc(c);
      if (c^ = ':') and
         (c[1] <= ' ') then // try e.g. :joy_cat:
        result := EmojiFromText(P, c - P);
    end;
    if result <> eNone then
    begin
      P := c + 1; // continue parsing after the Emoji text
      if W <> nil then
        W.AddShort(pointer(EMOJI_UTF8[result]), 4);
      exit;
    end;
  end;
  if W <> nil then
    W.Add(':');
end;

procedure EmojiToDots(P: PUtf8Char; W: TTextWriter);
var
  B: PUtf8Char;
  c: cardinal;
begin
  if (P <> nil) and
     (W <> nil) then
    repeat
      B := P;
      while (P^ <> #0) and
            (PWord(P)^ <> $9ff0) do
        inc(P);
      W.AddNoJsonEscape(B, P - B);
      if P^ = #0 then
        break;
      B := P;
      c := NextUtf8Ucs4(P) - $1f5ff;
      if c <= cardinal(high(TEmoji)) then
        W.AddNoJsonEscapeUtf8(EMOJI_TAG[TEmoji(c)])
      else
        W.AddNoJsonEscape(B, P - B);
    until P^ = #0;
end;

function EmojiToDots(const text: RawUtf8): RawUtf8;
var
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  if PosExChar(#$f0, text) = 0 then
  begin
    result := text; // no UTF-8 smiley for sure
    exit;
  end;
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    EmojiToDots(pointer(text), W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure EmojiFromDots(P: PUtf8Char; W: TTextWriter);
var
  B: PUtf8Char;
begin
  if (P <> nil) and
     (W <> nil) then
    repeat
      B := P;
      while not (P^ in [#0, ':']) do
        inc(P);
      W.AddNoJsonEscape(B, P - B);
      if P^ = #0 then
        break;
      EmojiParseDots(P, W);
    until P^ = #0;
end;

function EmojiFromDots(const text: RawUtf8): RawUtf8;
var
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    EmojiFromDots(pointer(text), W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ ************ RawByteString Buffers Aggregation via TRawByteStringGroup }

{ TRawByteStringGroup }

procedure TRawByteStringGroup.Add(const aItem: RawByteString);
begin
  if Values = nil then
    Clear; // ensure all fields are initialized, even if on stack
  if Count = Length(Values) then
    SetLength(Values, NextGrow(Count));
  with Values[Count] do
  begin
    Position := self.Position;
    Value := aItem;
  end;
  LastFind := Count;
  inc(Count);
  inc(Position, Length(aItem));
end;

procedure TRawByteStringGroup.Add(aItem: pointer; aItemLen: integer);
var
  tmp: RawByteString;
begin
  FastSetRawByteString(tmp, aItem, aItemLen);
  Add(tmp);
end;

procedure TRawByteStringGroup.Add(const aAnother: TRawByteStringGroup);
var
  i: integer;
  s, d: PRawByteStringGroupValue;
begin
  if aAnother.Values = nil then
    exit;
  if Values = nil then
    Clear; // ensure all fields are initialized, even if on stack
  if Count + aAnother.Count > Length(Values) then
    SetLength(Values, Count + aAnother.Count);
  s := pointer(aAnother.Values);
  d := @Values[Count];
  for i := 1 to aAnother.Count do
  begin
    d^.Position := Position;
    d^.Value := s^.Value;
    inc(Position, length(s^.Value));
    inc(s);
    inc(d);
  end;
  inc(Count, aAnother.Count);
  LastFind := Count - 1;
end;

procedure TRawByteStringGroup.RemoveLastAdd;
begin
  if Count > 0 then
  begin
    dec(Count);
    dec(Position, Length(Values[Count].Value));
    Values[Count].Value := ''; // release memory
    LastFind := Count - 1;
  end;
end;

function TRawByteStringGroup.Equals(const aAnother: TRawByteStringGroup): boolean;
begin
  if ((Values = nil) and
      (aAnother.Values <> nil)) or
     ((Values <> nil) and
      (aAnother.Values = nil)) or
     (Position <> aAnother.Position) then
    result := false
  else if (Count <> 1) or
          (aAnother.Count <> 1) or
          (Values[0].Value <> aAnother.Values[0].Value) then
    result := AsText = aAnother.AsText
  else
    result := true;
end;

procedure TRawByteStringGroup.Clear;
begin
  Values := nil;
  Position := 0;
  Count := 0;
  LastFind := 0;
end;

procedure TRawByteStringGroup.AppendTextAndClear(var aDest: RawByteString);
var
  d, i: integer;
  v: PRawByteStringGroupValue;
begin
  d := length(aDest);
  SetLength(aDest, d + Position);
  v := pointer(Values);
  for i := 1 to Count do
  begin
    MoveFast(pointer(v^.Value)^, PByteArray(aDest)[d + v^.Position], length(v^.Value));
    inc(v);
  end;
  Clear;
end;

function TRawByteStringGroup.AsText: RawByteString;
begin
  if Values = nil then
    result := ''
  else
  begin
    if Count > 1 then
      Compact;
    result := Values[0].Value;
  end;
end;

procedure TRawByteStringGroup.Compact;
var
  i: integer;
  v: PRawByteStringGroupValue;
  tmp: RawUtf8;
begin
  if (Values <> nil) and
     (Count > 1) then
  begin
    FastSetString(tmp, Position); // assume CP_UTF8 for FPC RTL bug
    v := pointer(Values);
    for i := 1 to Count do
    begin
      MoveFast(pointer(v^.Value)^, PByteArray(tmp)[v^.Position], length(v^.Value));
      {$ifdef FPC}
      FastAssignNew(v^.Value);
      {$else}
      v^.Value := '';
      {$endif FPC}
      inc(v);
    end;
    Values[0].Value := tmp; // use result for absolute compaction ;)
    if Count > 128 then
      SetLength(Values, 128);
    Count := 1;
    LastFind := 0;
  end;
end;

function TRawByteStringGroup.AsBytes: TByteDynArray;
var
  i: integer;
begin
  result := nil;
  if Values = nil then
    exit;
  SetLength(result, Position);
  for i := 0 to Count - 1 do
    with Values[i] do
      MoveFast(pointer(Value)^, PByteArray(result)[Position], length(Value));
end;

procedure TRawByteStringGroup.Write(W: TTextWriter; Escape: TTextWriterKind);
var
  i: integer;
begin
  if Values <> nil then
    for i := 0 to Count - 1 do
      with Values[i] do
        W.Add(PUtf8Char(pointer(Value)), length(Value), Escape);
end;

procedure TRawByteStringGroup.WriteBinary(W: TBufferWriter);
var
  i: integer;
begin
  if Values <> nil then
    for i := 0 to Count - 1 do
      W.WriteBinary(Values[i].Value);
end;

procedure TRawByteStringGroup.WriteString(W: TBufferWriter);
begin
  if Values = nil then
  begin
    W.Write1(0);
    exit;
  end;
  W.WriteVarUInt32(Position);
  WriteBinary(W);
end;

procedure TRawByteStringGroup.AddFromReader(var aReader: TFastReader);
var
  complexsize: integer;
begin
  complexsize := aReader.VarUInt32;
  if complexsize > 0 then
    // directly create a RawByteString from aReader buffer
    Add(aReader.Next(complexsize), complexsize);
end;

function TRawByteStringGroup.Find(aPosition: integer): PRawByteStringGroupValue;
var
  i: integer;
begin
  result := nil;
  if (pointer(Values) = nil) or
     (cardinal(aPosition) >= cardinal(Position)) then
    exit;
  result := @Values[LastFind]; // this cache is very efficient in practice
  if (aPosition >= result^.Position) and
     (aPosition < result^.Position + length(result^.Value)) then
    exit;
  result := @Values[1]; // seldom O(n) brute force search (in CPU L1 cache)
  for i := 0 to Count - 2 do
    if result^.Position > aPosition then
    begin
      dec(result);
      LastFind := i;
      exit;
    end
    else
      inc(result);
  dec(result);
  LastFind := Count - 1;
end;

function TRawByteStringGroup.Find(aPosition, aLength: integer): pointer;
var
  P: PRawByteStringGroupValue;
  i: integer;
label
  found;
begin
  result := nil;
  if (pointer(Values) = nil) or
     (cardinal(aPosition) >= cardinal(Position)) then
    exit;
  P := @Values[LastFind]; // this cache is very efficient in practice
  i := aPosition - P^.Position;
  if (i >= 0) and
     (i + aLength < length(P^.Value)) then
  begin
    result := @PByteArray(P^.Value)[i];
    exit;
  end;
  P := @Values[1]; // seldom O(n) brute force search (in CPU L1 cache)
  for i := 0 to Count - 2 do
    if P^.Position > aPosition then
    begin
      LastFind := i;
found:  dec(P);
      dec(aPosition, P^.Position);
      if aLength - aPosition <= length(P^.Value) then
        result := @PByteArray(P^.Value)[aPosition];
      exit;
    end
    else
      inc(P);
  LastFind := Count - 1;
  goto found;
end;

procedure TRawByteStringGroup.FindAsText(aPosition, aLength: integer;
  out aText: RawByteString);
var
  P: PRawByteStringGroupValue;
begin
  P := Find(aPosition);
  if P = nil then
    exit;
  dec(aPosition, P^.Position);
  if (aPosition = 0) and
     (length(P^.Value) = aLength) then
    aText := P^.Value
  else
  // direct return if not yet compacted
  if aLength - aPosition <= length(P^.Value) then
    FastSetRawByteString(aText, @PByteArray(P^.Value)[aPosition], aLength);
end;

function TRawByteStringGroup.FindAsText(aPosition, aLength: integer): RawByteString;
{%H-}begin
  {%H-}FindAsText(aPosition, aLength, result);
end;

procedure TRawByteStringGroup.FindAsVariant(aPosition, aLength: integer;
  out aDest: variant);
var
  tmp: RawByteString;
begin
  FindAsText(aPosition, aLength, tmp);
  if {%H-}tmp <> '' then
    RawUtf8ToVariant(tmp, aDest);
end;

procedure TRawByteStringGroup.FindWrite(aPosition, aLength: integer;
  W: TTextWriter; Escape: TTextWriterKind; TrailingCharsToIgnore: integer);
var
  P: pointer;
begin
  P := Find(aPosition, aLength);
  if P <> nil then
    W.Add(PUtf8Char(P) + TrailingCharsToIgnore, aLength - TrailingCharsToIgnore, Escape);
end;

procedure TRawByteStringGroup.FindWriteBase64(aPosition, aLength: integer;
  W: TTextWriter; withMagic: boolean);
var
  P: pointer;
begin
  P := Find(aPosition, aLength);
  if P <> nil then
    W.WrBase64(P, aLength, withMagic);
end;

procedure TRawByteStringGroup.FindMove(aPosition, aLength: integer;
  aDest: pointer);
var
  P: pointer;
begin
  P := Find(aPosition, aLength);
  if P <> nil then
    MoveFast(P^, aDest^, aLength);
end;


{ TRawByteStringCached }

type
  TRawByteStringCacheOne = record
    header: TLockedListOne;
    strrec: TStrRec;
  end;
  PRawByteStringCacheOne = ^TRawByteStringCacheOne;

constructor TRawByteStringCached.Create(aLength: integer);
begin
  fLength := aLength;
  fOne.Init(aLength + (SizeOf(TRawByteStringCacheOne) + 1));
end;

procedure TRawByteStringCached.New(var aDest: RawByteString; aCodePage: integer);
var
  one: PRawByteStringCacheOne;
begin
  one := fOne.New;
  {$ifdef HASCODEPAGE}
  one^.strrec.codePage := aCodePage;
  one^.strrec.elemSize := 1;
  {$endif HASCODEPAGE}
  one^.strrec.refCnt := -2;
  one^.strrec.length := fLength;
  inc(one);
  FastAssignNew(aDest, one);
end;

procedure TRawByteStringCached.New(var aDest: RawUtf8);
begin
  New(RawByteString(aDest), CP_UTF8);
end;

procedure TRawByteStringCached.NewUtf8(var aDest: pointer);
begin
  New(PRawByteString(@aDest)^, CP_UTF8);
end;

procedure TRawByteStringCached.Release(var aDest: RawByteString);
var
  one: PRawByteStringCacheOne;
begin
  if self <> nil then
  begin
    one := pointer(aDest);
    dec(one);
    if (one^.strrec.refCnt = -2) and
       (one^.strrec.length = TStrLen(fLength)) and
       fOne.Free(one) then
    begin
      pointer(aDest) := nil;
      exit;
    end;
  end;
  FastAssignNew(aDest) // this was a regular RawByteString
end;

procedure TRawByteStringCached.Release(var aDest: RawUtf8);
begin
  Release(RawByteString(aDest));
end;

procedure TRawByteStringCached.Release(var aDest: pointer);
begin
  Release(PRawByteString(@aDest)^);
end;

function TRawByteStringCached.Clean: PtrInt;
begin
  result := fOne.EmptyBin * fOne.Size;
end;

destructor TRawByteStringCached.Destroy;
begin
  fOne.Done;
  inherited Destroy;
end;


{ TRawByteStringBuffer }

procedure TRawByteStringBuffer.Reset;
begin
  fLen := 0;
end;

procedure TRawByteStringBuffer.Clear;
begin
  fLen := 0;
  FastAssignNew(fBuffer);
end;

function TRawByteStringBuffer.Buffer: pointer;
begin
  result := pointer(fBuffer);
end;

function TRawByteStringBuffer.Capacity: PtrInt;
begin
  result := length(fBuffer);
end;

procedure TRawByteStringBuffer.RawRealloc(needed: PtrInt);
begin
  if fLen = 0 then // buffer from scratch (fBuffer may be '' or not)
  begin
    inc(needed, 128); // small overhead at first
    FastSetString(fBuffer, needed); // no realloc
  end
  else
  begin
    inc(needed, needed shr 3 + 2048); // generous overhead on resize
    SetLength(fBuffer, needed); // realloc = move existing data
  end;
end;

procedure TRawByteStringBuffer.RawAppend(P: pointer; PLen: PtrInt);
var
  needed: PtrInt;
begin
  needed := fLen + PLen + 2;
  if needed > length(fBuffer) then
    RawRealloc(needed);
  MoveFast(P^, PByteArray(fBuffer)[fLen], PLen);
  inc(fLen, PLen);
end;

procedure TRawByteStringBuffer.Append(P: pointer; PLen: PtrInt);
begin
  if PLen > 0 then
    RawAppend(P, PLen);
end;

procedure TRawByteStringBuffer.Append(const Text: RawUtf8);
var
  P: PAnsiChar;
begin
  P := pointer(Text);
  if P <> nil then
    RawAppend(P, PStrLen(P - _STRLEN)^);
end;

procedure TRawByteStringBuffer.Append(Value: QWord);
var
  tmp: array[0..23] of AnsiChar;
  P: PAnsiChar;
begin
  {$ifndef ASMINTEL} // our StrUInt64 asm has less CPU cache pollution
  if Value <= high(SmallUInt32Utf8) then
    Append(SmallUInt32Utf8[Value])
  else
  {$endif ASMINTEL}
  begin
    P := StrUInt64(@tmp[23], Value);
    RawAppend(P, @tmp[23] - P);
  end;
end;

procedure TRawByteStringBuffer.AppendCRLF;
begin
  PWord(@PByteArray(fBuffer)[fLen])^ := $0a0d;
  inc(fLen, 2);
end;

procedure TRawByteStringBuffer.Append(Ch: AnsiChar);
begin
  PByteArray(fBuffer)[fLen] := ord(Ch);
  inc(fLen);
end;

procedure TRawByteStringBuffer.AppendShort(const Text: ShortString);
begin
  RawAppend(@Text[1], ord(Text[0]));
end;

procedure TRawByteStringBuffer.Append(const Text: array of RawUtf8);
var
  i: PtrInt;
begin
  for i := 0 to high(Text) do
    Append(Text[i]);
end;

function TRawByteStringBuffer.TryAppend(P: pointer; PLen: PtrInt): boolean;
begin
  if fLen + PLen <= length(fBuffer) then
  begin
    MoveFast(P^, PByteArray(fBuffer)[fLen], PLen);
    inc(fLen, PLen);
    result := true;
  end
  else
    result := false;
end;

procedure TRawByteStringBuffer.Reserve(MaxSize: PtrInt);
begin
  fLen := 0;
  if MaxSize > length(fBuffer) then
    RawRealloc(MaxSize);
end;

procedure TRawByteStringBuffer.Reserve(const WorkingBuffer: RawByteString);
begin
  fLen := 0;
  if pointer(fBuffer) <> pointer(WorkingBuffer) then
    fBuffer := WorkingBuffer;
end;

procedure TRawByteStringBuffer.Remove(FirstBytes: PtrInt);
begin
  if FirstBytes > 0 then
    if FirstBytes >= fLen then
      fLen := 0
    else
    begin
      dec(fLen, FirstBytes);
      MoveFast(PByteArray(fBuffer)[FirstBytes], pointer(fBuffer)^, fLen);
    end;
end;

function TRawByteStringBuffer.Extract(Dest: pointer; Count: PtrInt): PtrInt;
begin
  result := fLen;
  if Count < result then
    result := Count;
  if result <= 0 then
    exit;
  MoveFast(pointer(fBuffer)^, Dest^, result);
  dec(fLen, result);
  if fLen <> 0 then // keep trailing bytes for next call
    MoveFast(PByteArray(fBuffer)[result], pointer(fBuffer)^, fLen);
end;

function TRawByteStringBuffer.ExtractAt(
  var Dest: PAnsiChar; var Count: PtrInt; var Pos: PtrInt): PtrInt;
begin
  result := fLen - Pos;
  if (result = 0) or
     (Count = 0) then
    exit;
  if result > Count then
    result := Count;
  MoveFast(PByteArray(fBuffer)[Pos], Dest^, result);
  inc(Pos, result);
  if Pos = fLen then
  begin
    Reset; // all pending content has been read
    Pos := 0;
  end;
  inc(Dest, result);
  dec(Count, result);
end;

procedure TRawByteStringBuffer.Insert(P: pointer; PLen: PtrInt;
  Position: PtrInt; CRLF: boolean);
begin
  inc(PLen, 2 * ord(CRLF));
  if PLen + fLen > length(fBuffer) then
    RawRealloc(PLen + fLen); // need more space
  MoveFast(pointer(fBuffer)^, PByteArray(fBuffer)[PLen], fLen);
  dec(PLen, 2 * ord(CRLF));
  MoveFast(P^, pointer(fBuffer)^, PLen);
  if CRLF then
    PWord(@PByteArray(fBuffer)[PLen])^ := $0a0d;
end;

procedure TRawByteStringBuffer.AsText(out Text: RawUtf8; Overhead: PtrInt;
  UseMainBuffer: boolean);
begin
  if (Len = 0) or
     (fBuffer = '') or
     (OverHead < 0) then
    exit;
  if UseMainBuffer and
     (PStrCnt(PAnsiChar(pointer(fBuffer)) - _STRCNT)^ = 1) and
     (Len + Overhead <= length(fBuffer)) then
  begin
    pointer(Text) := pointer(fBuffer); // fast pointer move for refcount=1
    pointer(fBuffer) := nil;
  end
  else
  begin
    pointer(Text) := FastNewString(Len + Overhead, CP_UTF8);
    MoveFast(pointer(fBuffer)^, pointer(Text)^, Len);
    if OverHead = 0 then
      exit;
  end;
  // keep OverHead allocated, but SetLength(Len) and put #0 at right position
  FakeLength(Text, Len);
end;


procedure InitializeUnit;
var
  i: PtrInt;
  e: TEmoji;
begin
  // initialize Base64/Base64Uri/Base58/Base32/Baudot encoding/decoding tables
  FillcharFast(ConvertBase64ToBin, SizeOf(ConvertBase64ToBin), 255); // -1 = invalid
  FillcharFast(ConvertBase64uriToBin, SizeOf(ConvertBase64uriToBin), 255);
  FillcharFast(ConvertBase58ToBin, SizeOf(ConvertBase58ToBin), 255);
  FillcharFast(ConvertBase32ToBin, SizeOf(ConvertBase32ToBin), 255);
  for i := 0 to high(b64enc) do
    ConvertBase64ToBin[b64enc[i]] := i;
  ConvertBase64ToBin['='] := -2; // special value for '='
  for i := 0 to high(b64urienc) do
    ConvertBase64uriToBin[b64urienc[i]] := i;
  for i := 0 to high(b58enc) do
    ConvertBase58ToBin[b58enc[i]] := i;
  for i := 0 to high(b32enc) do
    ConvertBase32ToBin[b32enc[i]] := i;
  for i := high(Baudot2Char) downto 0 do
    if Baudot2Char[i]<#128 then
      Char2Baudot[Baudot2Char[i]] := i;
  for i := ord('a') to ord('z') do
    Char2Baudot[AnsiChar(i - 32)] := Char2Baudot[AnsiChar(i)]; // A-Z -> a-z
  // HTML/Emoji Efficient Parsing
  Assert(ord(high(TEmoji)) = $4f + 1);
  EMOJI_RTTI := GetEnumName(TypeInfo(TEmoji), 1); // ignore eNone=0
  GetEnumTrimmedNames(TypeInfo(TEmoji), @EMOJI_TEXT);
  EMOJI_TEXT[eNone] := '';
  for e := succ(low(e)) to high(e) do
  begin
    LowerCaseSelf(EMOJI_TEXT[e]);
    EMOJI_TAG[e] := ':' + EMOJI_TEXT[e] + ':';
    SetLength(EMOJI_UTF8[e], 4); // order matches U+1F600 to U+1F64F codepoints
    Ucs4ToUtf8(ord(e) + $1f5ff, pointer(EMOJI_UTF8[e]));
  end;
  EMOJI_AFTERDOTS[')'] := eSmiley;
  EMOJI_AFTERDOTS['('] := eFrowning;
  EMOJI_AFTERDOTS['|'] := eExpressionless;
  EMOJI_AFTERDOTS['/'] := eConfused;
  EMOJI_AFTERDOTS['D'] := eLaughing;
  EMOJI_AFTERDOTS['o'] := eOpen_mouth;
  EMOJI_AFTERDOTS['O'] := eOpen_mouth;
  EMOJI_AFTERDOTS['p'] := eYum;
  EMOJI_AFTERDOTS['P'] := eYum;
  EMOJI_AFTERDOTS['s'] := eScream;
  EMOJI_AFTERDOTS['S'] := eScream;
  // setup internal lists and function wrappers
  AlgoSynLZ := TAlgoSynLZ.Create;
  AlgoRleLZ := TAlgoRleLZ.Create;
  AlgoRle := TAlgoRle.Create;
  Base64EncodeMain := @Base64EncodeMainPas;
  Base64DecodeMain := @Base64DecodeMainPas;
  {$ifdef ASMX64AVXNOCONST} // focus on x86_64 server performance
  if cfAVX2 in CpuFeatures then
  begin // our AVX2 asm code is almost 10x faster than the pascal version
    Base64EncodeMain := @Base64EncodeMainAvx2; // 11.5 GB/s vs 1.3 GB/s
    Base64DecodeMain := @Base64DecodeMainAvx2; //  8.7 GB/s vs 0.9 GB/s
  end;
  {$endif ASMX64AVXNOCONST}
end;


initialization
  InitializeUnit;


end.
