/// Framework Core Low-Level Wrappers to the Operating-System API
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.os;

{
  *****************************************************************************

  Cross-platform functions shared by all framework units
  - Some Cross-System Type and Constant Definitions
  - Gather Operating System Information
  - Operating System Specific Types (e.g. TWinRegistry)
  - Unicode, Time, File, Console, Library process
  - Cross-Platform Charset and CodePage Support
  - Per Class Properties O(1) Lookup via vmtAutoTable Slot (e.g. for RTTI cache)
  - TSynLocker/TSynLocked and Low-Level Threading Features
  - Unix Daemon and Windows Service Support

   Aim of this unit is to centralize most used OS-specific API calls, like a
  SysUtils unit on steroids, to avoid $ifdef/$endif in "uses" clauses.
   In practice, no "Windows", nor "Linux/Posix" reference should be needed in
  regular units, once mormot.core.os is included. :)
   This unit only refers to mormot.core.base so can be used almost stand-alone.

  *****************************************************************************
}

interface

{$I mormot.defines.inc}

uses
  {$ifdef OSWINDOWS}
  Windows, // needed here e.g. for redefinition/redirection of standard types
  Messages,
  {$endif OSWINDOWS}
  classes,
  contnrs,
  types,
  sysutils,
  mormot.core.base;


{ ****************** Some Cross-System Type and Constant Definitions }

const
  {$ifdef OSWINDOWS}
  /// operating-system dependent Line Feed characters (#13#10 or #10)
  CRLF = #13#10;
  /// operating-system dependent wildchar to match all files in a folder
  FILES_ALL = '*.*';
  /// operating-system dependent "inverted" delimiter for NormalizeFileName()
  InvertedPathDelim = '/';
  /// operating-system dependent boolean if paths are case-insensitive
  PathCaseInsensitive = true;
  {$else}
  /// operating-system dependent Line Feed characters
  CRLF = #10;
  /// operating-system dependent wildchar to match all files in a folder
  FILES_ALL = '*';
  /// operating-system dependent "inverted" delimiter for NormalizeFileName()
  InvertedPathDelim = '\';
  /// operating-system dependent boolean if paths are case-insensitive
  PathCaseInsensitive = false;
  {$endif OSWINDOWS}

  /// human-friendly alias to open a file for exclusive writing
  fmShareRead      = fmShareDenyWrite;
  /// human-friendly alias to open a file for exclusive reading
  fmShareWrite     = fmShareDenyRead;
  /// human-friendly alias to open a file with no read/write exclusion
  fmShareReadWrite = fmShareDenyNone;

  /// a convenient constant to open a file for reading without exclusion
  fmOpenReadShared = fmOpenRead or fmShareReadWrite;

  /// a convenient constant to open a file for writing without exclusion
  fmOpenWriteShared = fmOpenReadWrite or fmShareReadWrite;

  /// a convenient constant to create a file without exclusion
  fmCreateShared = fmCreate or fmShareReadWrite;

  /// a convenient array constant to open a file for writing without exclusion
  fmCreateOrRewrite: array[{rewrite=}boolean] of cardinal = (
   fmCreateShared,
   fmOpenWriteShared);

const
  /// void HTTP Status Code (not a standard value, for internal use only)
  HTTP_NONE = 0;
  /// HTTP Status Code for "Continue"
  HTTP_CONTINUE = 100;
  /// HTTP Status Code for "Switching Protocols"
  HTTP_SWITCHINGPROTOCOLS = 101;
  /// HTTP Status Code for "Success"
  HTTP_SUCCESS = 200;
  /// HTTP Status Code for "Created"
  HTTP_CREATED = 201;
  /// HTTP Status Code for "Accepted"
  HTTP_ACCEPTED = 202;
  /// HTTP Status Code for "Non-Authoritative Information"
  HTTP_NONAUTHORIZEDINFO = 203;
  /// HTTP Status Code for "No Content"
  HTTP_NOCONTENT = 204;
  /// HTTP Status Code for "Reset Content"
  HTTP_RESETCONTENT = 205;
  /// HTTP Status Code for "Partial Content"
  HTTP_PARTIALCONTENT = 206;
  /// HTTP Status Code for "Multiple Choices"
  HTTP_MULTIPLECHOICES = 300;
  /// HTTP Status Code for "Moved Permanently"
  HTTP_MOVEDPERMANENTLY = 301;
  /// HTTP Status Code for "Found"
  HTTP_FOUND = 302;
  /// HTTP Status Code for "See Other"
  HTTP_SEEOTHER = 303;
  /// HTTP Status Code for "Not Modified"
  HTTP_NOTMODIFIED = 304;
  /// HTTP Status Code for "Use Proxy"
  HTTP_USEPROXY = 305;
  /// HTTP Status Code for "Temporary Redirect"
  HTTP_TEMPORARYREDIRECT = 307;
  /// HTTP Status Code for "Permanent Redirect"
  HTTP_PERMANENTREDIRECT = 308;
  /// HTTP Status Code for "Bad Request"
  HTTP_BADREQUEST = 400;
  /// HTTP Status Code for "Unauthorized"
  HTTP_UNAUTHORIZED = 401;
  /// HTTP Status Code for "Forbidden"
  HTTP_FORBIDDEN = 403;
  /// HTTP Status Code for "Not Found"
  HTTP_NOTFOUND = 404;
  // HTTP Status Code for "Method Not Allowed"
  HTTP_NOTALLOWED = 405;
  // HTTP Status Code for "Not Acceptable"
  HTTP_NOTACCEPTABLE = 406;
  // HTTP Status Code for "Proxy Authentication Required"
  HTTP_PROXYAUTHREQUIRED = 407;
  /// HTTP Status Code for "Request Time-out"
  HTTP_TIMEOUT = 408;
  /// HTTP Status Code for "Conflict"
  HTTP_CONFLICT = 409;
  /// HTTP Status Code for "Payload Too Large"
  HTTP_PAYLOADTOOLARGE = 413;
  /// HTTP Status Code for "Range Not Satisfiable"
  HTTP_RANGENOTSATISFIABLE = 416;
  /// HTTP Status Code for "I'm a teapot"
  HTTP_TEAPOT = 418;
  /// HTTP Status Code for "Internal Server Error"
  HTTP_SERVERERROR = 500;
  /// HTTP Status Code for "Not Implemented"
  HTTP_NOTIMPLEMENTED = 501;
  /// HTTP Status Code for "Bad Gateway"
  HTTP_BADGATEWAY = 502;
  /// HTTP Status Code for "Service Unavailable"
  HTTP_UNAVAILABLE = 503;
  /// HTTP Status Code for "Gateway Timeout"
  HTTP_GATEWAYTIMEOUT = 504;
  /// HTTP Status Code for "HTTP Version Not Supported"
  HTTP_HTTPVERSIONNONSUPPORTED = 505;

  /// clearly wrong response code, used by THttpServerRequest.SetAsyncResponse
  // - for internal THttpAsyncServer asynchronous process
  HTTP_ASYNCRESPONSE = 777;

  /// the successful HTTP response codes after a GET request
  HTTP_GET_OK = [HTTP_SUCCESS, HTTP_NOCONTENT, HTTP_PARTIALCONTENT];

/// retrieve the HTTP reason text from its integer code as PRawUtf8
// - e.g. StatusCodeToText(200)^='OK'
// - as defined in http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
// - returns the generic 'Invalid Request' for any unknown Code
function StatusCodeToText(Code: cardinal): PRawUtf8;

/// retrieve the HTTP reason text from its integer code
// - as defined in http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
procedure StatusCodeToReason(Code: cardinal; var Reason: RawUtf8);

/// convert any HTTP_* constant to an integer status code and its English text
// - returns e.g. '200 OK' or '404 Not Found', calling StatusCodeToText()
function StatusCodeToShort(Code: cardinal): TShort47;

/// returns true for successful HTTP status codes, i.e. in 200..399 range
// - will map mainly SUCCESS (200), CREATED (201), NOCONTENT (204),
// PARTIALCONTENT (206), NOTMODIFIED (304) or TEMPORARYREDIRECT (307) codes
// - any HTTP status not part of this range will be identified as erronous
// request in the internal server statistics
function StatusCodeIsSuccess(Code: integer): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check the supplied HTTP header to not contain more than one EOL
// - to avoid unexpected HTTP body injection, e.g. from unsafe business code
function IsInvalidHttpHeader(head: PUtf8Char; headlen: PtrInt): boolean;


const
  /// HTTP header name for the content type, as defined in the corresponding RFC
  HEADER_CONTENT_TYPE = 'Content-Type: ';

  /// HTTP header name for the content type, in upper case
  // - as defined in the corresponding RFC
  // - could be used e.g. with IdemPChar() to retrieve the Content-Type value
  HEADER_CONTENT_TYPE_UPPER = 'CONTENT-TYPE: ';

  /// HTTP header name for the client IP, in upper case
  // - as defined in our HTTP server classes
  // - could be used e.g. with IdemPChar() to retrieve the remote IP address
  HEADER_REMOTEIP_UPPER = 'REMOTEIP: ';

  /// HTTP header name for the authorization token, in upper case
  // - could be used e.g. with IdemPChar() to retrieve a JWT value
  // - will detect header computed e.g. by motmot.net.http's
  // AuthorizationBearer()
  HEADER_BEARER_UPPER = 'AUTHORIZATION: BEARER ';

  /// MIME content type used for JSON communication (as used by the Microsoft
  // WCF framework and the YUI framework)
  // - no 'charset=UTF-8' encoding is necessary, as by specified by RFC 7159
  JSON_CONTENT_TYPE = 'application/json';

  /// HTTP header for MIME content type used for plain JSON
  // - i.e. 'Content-Type: application/json'
  JSON_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE + JSON_CONTENT_TYPE;

  /// MIME content type used for plain JSON, in upper case
  // - could be used e.g. with IdemPChar() to retrieve the Content-Type value
  JSON_CONTENT_TYPE_UPPER = 'APPLICATION/JSON';

  /// HTTP header for MIME content type used for plain JSON, in upper case
  // - could be used e.g. with IdemPChar() to retrieve the Content-Type value
  JSON_CONTENT_TYPE_HEADER_UPPER =
    HEADER_CONTENT_TYPE_UPPER + JSON_CONTENT_TYPE_UPPER;

  /// MIME content type used for plain UTF-8 text
  TEXT_CONTENT_TYPE = 'text/plain; charset=UTF-8';

  /// HTTP header for MIME content type used for plain UTF-8 text
  TEXT_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE + TEXT_CONTENT_TYPE;

  /// MIME content type used for UTF-8 encoded HTML
  HTML_CONTENT_TYPE = 'text/html; charset=UTF-8';

  /// HTTP header for MIME content type used for UTF-8 encoded HTML
  HTML_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE + HTML_CONTENT_TYPE;

  /// MIME content type used for UTF-8 encoded XML
  XML_CONTENT_TYPE = 'text/xml';

  /// HTTP header for MIME content type used for UTF-8 encoded XML
  XML_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE + XML_CONTENT_TYPE;

  /// MIME content type used for raw binary data
  BINARY_CONTENT_TYPE = 'application/octet-stream';

  /// MIME content type used for raw binary data, in upper case
  BINARY_CONTENT_TYPE_UPPER = 'APPLICATION/OCTET-STREAM';

  /// HTTP header for MIME content type used for raw binary data
  BINARY_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE + BINARY_CONTENT_TYPE;

  /// MIME content type used for a JPEG picture
  JPEG_CONTENT_TYPE = 'image/jpeg';

  /// a IdemPPChar() compatible array of textual MIME content types
  // - as used e.g. by IsHtmlContentTypeTextual()
  CONTENT_TYPE_TEXTUAL: array[0..7] of PAnsiChar = (
    JSON_CONTENT_TYPE_UPPER,
    'TEXT/',
    'APPLICATION/XML',
    'APPLICATION/JSON',
    'APPLICATION/JAVASCRIPT',
    'APPLICATION/X-JAVASCRIPT',
    'IMAGE/SVG+XML',
    nil);

  /// internal HTTP content-type for efficient static file sending
  // - detected e.g. by http.sys' THttpApiServer.Request or via the NGINX
  // X-Accel-Redirect header's THttpServer.Process (see
  // THttpServer.NginxSendFileFrom) for direct sending with no local bufferring
  // - the OutCustomHeader should contain the proper 'Content-type: ....'
  // corresponding to the file (e.g. by calling GetMimeContentType() function)
  STATICFILE_CONTENT_TYPE = '!STATICFILE';

  /// internal HTTP content-type Header for efficient static file sending
  STATICFILE_CONTENT_TYPE_HEADER =
    HEADER_CONTENT_TYPE + STATICFILE_CONTENT_TYPE;

  /// uppercase version of HTTP header for static file content serving
  STATICFILE_CONTENT_TYPE_HEADER_UPPPER =
    HEADER_CONTENT_TYPE_UPPER + STATICFILE_CONTENT_TYPE;

  /// used to notify e.g. the THttpServerRequest not to wait for any response
  // from the client
  // - is not to be used in normal HTTP process, but may be used e.g. by
  // TWebSocketProtocolRest.ProcessFrame() to avoid to wait for an incoming
  // response from the other endpoint
  NORESPONSE_CONTENT_TYPE = '!NORESPONSE';

  /// HTTP body following RFC 2324 standard e.g. for banned IP
  HTTP_BANIP_RESPONSE: string[201] =
    'HTTP/1.0 418 I''m a teapot'#13#10 +
    'Content-Length: 125'#13#10 +
    'Content-Type: text/plain'#13#10#13#10 +
    'Server refuses to brew coffee because it is currently a teapot.'#13#10 +
    'Do not mess with it and retry from this IP in a few seconds.';

  /// JSON compatible representation of a boolean value, i.e. 'false' and 'true'
  // - can be used e.g. in logs, or anything accepting a ShortString
  BOOL_STR: array[boolean] of string[7] = (
    'false', 'true');

  /// the JavaScript-like values of non-number IEEE constants
  // - as recognized by FloatToShortNan, and used by TTextWriter.Add()
  // when serializing such single/double/extended floating-point values
  JSON_NAN: array[TFloatNan] of string[11] = (
    '0', '"NaN"', '"Infinity"', '"-Infinity"');

var
  /// MIME content type used for JSON communication
  // - i.e. 'application/json' as stated by datatracker.ietf.org/doc/html/rfc7159
  // - this global will be initialized with JSON_CONTENT_TYPE constant, to
  // avoid a memory allocation each time it is assigned to a variable
  JSON_CONTENT_TYPE_VAR: RawUtf8;

  /// HTTP header for MIME content type used for plain JSON
  // - this global will be initialized with JSON_CONTENT_TYPE_HEADER constant,
  // to avoid a memory allocation each time it is assigned to a variable
  JSON_CONTENT_TYPE_HEADER_VAR: RawUtf8;

  /// can be used to avoid a memory allocation for res := 'null'
  // - this global will be initialized with 'null' constant, to
  // avoid a memory allocation each time it is assigned to a variable
  NULL_STR_VAR: RawUtf8;

  /// JSON compatible representation of a boolean value, i.e. 'false' and 'true'
  // - can be used when a RawUtf8 string is expected
  // - this global will be initialized with 'false' and 'true' constants, to
  // avoid a memory allocation each time it is assigned to a variable
  BOOL_UTF8: array[boolean] of RawUtf8;


type
  /// Security IDentifier (SID) Authority, encoded as 48-bit binary
  TSidAuth = array[0..5] of byte;
  PSidAuth = ^TSidAuth;

  /// Security IDentifier (SID) binary format, as retrieved e.g. by Windows API
  // - this definition is not detailed on oldest Delphi, and not available on
  // POSIX, whereas it makes sense to also have it, e.g. for server process
  TSid = packed record
     Revision: byte;
     SubAuthorityCount: byte;
     IdentifierAuthority: TSidAuth;
     SubAuthority: array[byte] of cardinal;
  end;
  PSid = ^TSid;
  PSids = array of PSid;

  /// define a list of well-known Security IDentifier (SID) groups
  // - for instance, wksBuiltinAdministrators is set for local administrators
  // - warning: does not exactly match winnt.h WELL_KNOWN_SID_TYPE enumeration
  TWellKnownSid = (
    wksNull,
    wksWorld,
    wksLocal,
    wksConsoleLogon,
    wksCreatorOwner,
    wksCreatorGroup,
    wksCreatorOwnerServer,
    wksCreatorGroupServer,
    wksIntegrityUntrusted,
    wksIntegrityLow,
    wksIntegrityMedium,
    wksIntegrityMediumPlus,
    wksIntegrityHigh,
    wksIntegritySystem,
    wksIntegrityProtectedProcess,
    wksIntegritySecureProcess,
    wksAuthenticationAuthorityAsserted,
    wksAuthenticationServiceAsserted,
    wksAuthenticationFreshKeyAuth,
    wksAuthenticationKeyTrust,
    wksAuthenticationKeyPropertyMfa,
    wksAuthenticationKeyPropertyAttestation,
    wksNtAuthority,
    wksDialup,
    wksNetwork,
    wksBatch,
    wksInteractive,
    wksService,
    wksAnonymous,
    wksProxy,
    wksEnterpriseControllers,
    wksSelf,
    wksAuthenticatedUser,
    wksRestrictedCode,
    wksTerminalServer,
    wksRemoteLogonId,
    wksThisOrganisation,
    wksIisUser,
    wksLocalSystem,
    wksLocalService,
    wksNetworkService,
    wksLocalAccount,
    wksLocalAccountAndAdministrator,
    wksBuiltinDomain,
    wksBuiltinAdministrators,
    wksBuiltinUsers,
    wksBuiltinGuests,
    wksBuiltinPowerUsers,
    wksBuiltinAccountOperators,
    wksBuiltinSystemOperators,
    wksBuiltinPrintOperators,
    wksBuiltinBackupOperators,
    wksBuiltinReplicator,
    wksBuiltinRasServers,
    wksBuiltinPreWindows2000CompatibleAccess,
    wksBuiltinRemoteDesktopUsers,
    wksBuiltinNetworkConfigurationOperators,
    wksBuiltinIncomingForestTrustBuilders,
    wksBuiltinPerfMonitoringUsers,
    wksBuiltinPerfLoggingUsers,
    wksBuiltinAuthorizationAccess,
    wksBuiltinTerminalServerLicenseServers,
    wksBuiltinDcomUsers,
    wksBuiltinIUsers,
    wksBuiltinCryptoOperators,
    wksBuiltinUnknown,
    wksBuiltinCacheablePrincipalsGroups,
    wksBuiltinNonCacheablePrincipalsGroups,
    wksBuiltinEventLogReadersGroup,
    wksBuiltinCertSvcDComAccessGroup,
    wksBuiltinRdsRemoteAccessServers,
    wksBuiltinRdsEndpointServers,
    wksBuiltinRdsManagementServers,
    wksBuiltinHyperVAdmins,
    wksBuiltinAccessControlAssistanceOperators,
    wksBuiltinRemoteManagementUsers,
    wksBuiltinDefaultSystemManagedGroup,
    wksBuiltinStorageReplicaAdmins,
    wksBuiltinDeviceOwners,
    wksCapabilityInternetClient,
    wksCapabilityInternetClientServer,
    wksCapabilityPrivateNetworkClientServer,
    wksCapabilityPicturesLibrary,
    wksCapabilityVideosLibrary,
    wksCapabilityMusicLibrary,
    wksCapabilityDocumentsLibrary,
    wksCapabilityEnterpriseAuthentication,
    wksCapabilitySharedUserCertificates,
    wksCapabilityRemovableStorage,
    wksCapabilityAppointments,
    wksCapabilityContacts,
    wksBuiltinAnyPackage,
    wksBuiltinAnyRestrictedPackage,
    wksNtlmAuthentication,
    wksSChannelAuthentication,
    wksDigestAuthentication);

  /// define a set of well-known SID
  TWellKnownSids = set of TWellKnownSid;

  /// custom binary buffer type used as convenient Windows SID storage
  RawSid = type RawByteString;

  /// a dynamic array of binary SID storage buffers
  RawSidDynArray = array of RawSid;


/// a wrapper around MemCmp() on two Security IDentifier binary buffers
// - will first compare by length, then by content
function SidCompare(a, b: PSid): integer;

/// compute the actual binary length of a Security IDentifier buffer, in bytes
function SidLength(sid: PSid): PtrInt;
  {$ifdef HASINLINE} inline; {$endif}

/// allocate a RawSid instance from a PSid raw handler
procedure ToRawSid(sid: PSid; out result: RawSid);

/// check if a RawSid binary buffer has the expected length of a valid SID
function IsValidRawSid(const sid: RawSid): boolean;

/// search within SID dynamic array for a given SID
function HasSid(const sids: PSids; sid: PSid): boolean;

/// search within SID dynamic array for a given dynamic array of SID buffers
function HasAnySid(const sids: PSids; const sid: RawSidDynArray): boolean;

/// append a SID buffer pointer to a dynamic array of SID buffers
procedure AddRawSid(var sids: RawSidDynArray; sid: PSid);

/// convert a Security IDentifier as text, following the standard representation
procedure SidToTextShort(sid: PSid; var result: shortstring);

/// convert a Security IDentifier as text, following the standard representation
function SidToText(sid: PSid): RawUtf8;

/// convert several Security IDentifier as text dynamic array
function SidsToText(sids: PSids): TRawUtf8DynArray;

/// convert a Security IDentifier as text, following the standard representation
function RawSidToText(const sid: RawSid): RawUtf8;

/// parse a Security IDentifier text, following the standard representation
// - won't support hexadecimal IdentifierAuthority, i.e. S-1-0x######-....
function TextToSid(P: PUtf8Char; out sid: TSid): boolean;

/// parse a Security IDentifier text, following the standard representation
function TextToRawSid(const text: RawUtf8): RawSid; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// parse a Security IDentifier text, following the standard representation
function TextToRawSid(const text: RawUtf8; out sid: RawSid): boolean; overload;

/// returns a Security IDentifier of a well-known SID as binary
// - is using an internal cache for the returned RawSid instances
function KnownRawSid(wks: TWellKnownSid): RawSid;

/// returns a Security IDentifier of a well-known SID as standard text
// - e.g. wksBuiltinAdministrators as 'S-1-5-32-544'
function KnownSidToText(wks: TWellKnownSid): PShortString;

/// recognize most well-known SID from a Security IDentifier binary buffer
// - returns wksNull if the supplied buffer was not recognized
function SidToKnown(sid: PSid): TWellKnownSid; overload;

/// recognize most well-known SID from a Security IDentifier standard text
// - returns wksNull if the supplied text was not recognized
function SidToKnown(const text: RawUtf8): TWellKnownSid; overload;

/// recognize some well-known SIDs from the supplied SID dynamic array
function SidToKnownGroups(const sids: PSids): TWellKnownSids;


{ ****************** Gather Operating System Information }

type
  /// Exception types raised by this mormot.core.os unit
  EOSException = class(ExceptionWithProps);

  /// the known operating systems
  // - it will also recognize most Linux distributions
  TOperatingSystem = (
    osUnknown,
    osWindows,
    osLinux,
    osOSX,
    osBSD,
    osPOSIX,
    osArch,
    osAurox,
    osDebian,
    osFedora,
    osGentoo,
    osKnoppix,
    osMint,
    osMandrake,
    osMandriva,
    osNovell,
    osUbuntu,
    osSlackware,
    osSolaris,
    osSuse,
    osSynology,
    osTrustix,
    osClear,
    osUnited,
    osRedHat,
    osLFS,
    osOracle,
    osMageia,
    osCentOS,
    osCloud,
    osXen,
    osAmazon,
    osCoreOS,
    osAlpine,
    osAndroid);

  /// the recognized Windows versions
  // - defined even outside OSWINDOWS to access e.g. from monitoring tools
  TWindowsVersion = (
    wUnknown,
    w2000,
    wXP,
    wXP_64,
    wServer2003,
    wServer2003_R2,
    wVista,
    wVista_64,
    wServer2008,
    wServer2008_64,
    wSeven,
    wSeven_64,
    wServer2008_R2,
    wServer2008_R2_64,
    wEight,
    wEight_64,
    wServer2012,
    wServer2012_64,
    wEightOne,
    wEightOne_64,
    wServer2012R2,
    wServer2012R2_64,
    wTen,
    wTen_64,
    wServer2016,
    wServer2016_64,
    wEleven,
    wEleven_64,
    wServer2019_64,
    wServer2022_64);

  /// the running Operating System, encoded as a 32-bit integer
  TOperatingSystemVersion = packed record
    case os: TOperatingSystem of
    osUnknown: (
      b: array[0..2] of byte);
    osWindows: (
      win: TWindowsVersion;
      winbuild: word);
    osLinux: (
      utsrelease: array[0..2] of byte);
  end;

const
  /// the recognized MacOS versions, as plain text
  // - indexed from OSVersion32.utsrelease[2] kernel revision
  MACOS_NAME: array[8 .. 24] of RawUtf8 = (
    '10.4 Tiger',
    '10.5 Leopard',
    '10.6 Snow Leopard',
    '10.7 Lion',
    '10.8 Mountain Lion',
    '10.9 Mavericks',
    '10.10 Yosemite',
    '10.11 El Capitan',
    '10.12 Sierra',
    '10.13 High Sierra',
    '10.14 Mojave',
    '10.15 Catalina',
    '11 Big Sur',
    '12 Monterey',
    '13 Ventura',
    '14 Sonoma',
    '15 Glow'); // use known internal codename for upcoming version

  /// the recognized Windows versions, as plain text
  // - defined even outside OSWINDOWS to allow process e.g. from monitoring tools
  WINDOWS_NAME: array[TWindowsVersion] of RawUtf8 = (
    '',
    '2000',
    'XP',
    'XP 64bit',
    'Server 2003',
    'Server 2003 R2',
    'Vista',
    'Vista 64bit',
    'Server 2008',
    'Server 2008 64bit',
    '7',
    '7 64bit',
    'Server 2008 R2',
    'Server 2008 R2 64bit',
    '8',
    '8 64bit',
    'Server 2012',
    'Server 2012 64bit',
    '8.1',
    '8.1 64bit',
    'Server 2012 R2',
    'Server 2012 R2 64bit',
    '10',
    '10 64bit',
    'Server 2016',
    'Server 2016 64bit',
    '11',
    '11 64bit',
    'Server 2019 64bit',
    'Server 2022 64bit');

  /// the recognized Windows versions which are 32-bit
  WINDOWS_32 = [
     w2000,
     wXP,
     wServer2003,
     wServer2003_R2,
     wVista,
     wServer2008,
     wSeven,
     wServer2008_R2,
     wEight,
     wServer2012,
     wEightOne,
     wServer2012R2,
     wTen,
     wServer2016,
     wEleven];

  /// translate one operating system (and distribution) into a its common name
  OS_NAME: array[TOperatingSystem] of RawUtf8 = (
    'Unknown',
    'Windows',
    'Linux',
    'OSX',
    'BSD',
    'POSIX',
    'Arch',
    'Aurox',
    'Debian',
    'Fedora',
    'Gentoo',
    'Knoppix',
    'Mint',
    'Mandrake',
    'Mandriva',
    'Novell',
    'Ubuntu',
    'Slackware',
    'Solaris',
    'Suse',
    'Synology',
    'Trustix',
    'Clear',
    'United',
    'RedHat',
    'LFS',
    'Oracle',
    'Mageia',
    'CentOS',
    'Cloud',
    'Xen',
    'Amazon',
    'CoreOS',
    'Alpine',
    'Android');

  /// translate one operating system (and distribution) into a single character
  // - may be used internally e.g. for a HTTP User-Agent header, as with
  // TFileVersion.UserAgent and UserAgentParse()
  OS_INITIAL: array[TOperatingSystem] of AnsiChar = (
    '?', // Unknown
    'W', // Windows
    'L', // Linux
    'X', // OSX
    'B', // BSD
    'P', // POSIX
    'A', // Arch
    'a', // Aurox
    'D', // Debian
    'F', // Fedora
    'G', // Gentoo
    'K', // Knoppix
    'M', // Mint
    'm', // Mandrake
    'n', // Mandriva
    'N', // Novell
    'U', // Ubuntu
    'S', // Slackware
    's', // Solaris
    'u', // Suse
    'Y', // Synology
    'T', // Trustix
    'C', // Clear
    't', // United
    'R', // RedHat
    'l', // LFS
    'O', // Oracle
    'G', // Mageia
    'c', // CentOS
    'd', // Cloud
    'x', // Xen
    'Z', // Amazon
    'r', // CoreOS
    'p', // Alpine
    'J'  // Android (J=JVM)
    );

  /// the operating systems items which actually have a Linux kernel
  OS_LINUX = [
    osLinux,
    osArch .. osAndroid];

  /// the compiler family used
  COMP_TEXT = {$ifdef FPC}'Fpc'{$else}'Delphi'{$endif};

  /// the target Operating System used for compilation, as short text
  OS_TEXT =
    {$ifdef OSWINDOWS}
      'Win';
    {$else} {$ifdef OSDARWIN}
      'OSX';
    {$else}{$ifdef OSBSD}
      'BSD';
    {$else} {$ifdef OSANDROID}
      'Android';
    {$else} {$ifdef OSLINUX}
      'Linux';
    {$else}
      'Posix';
    {$endif OSLINUX}
    {$endif OSANDROID}
    {$endif OSBSD}
    {$endif OSDARWIN}
    {$endif OSWINDOWS}

  /// the CPU architecture used for compilation
  CPU_ARCH_TEXT =
    {$ifdef CPUX86}
      'x86'
    {$else} {$ifdef CPUX64}
      'x64'
    {$else} {$ifdef CPUARM}
      'arm' +
    {$else} {$ifdef CPUAARCH64}
      'aarch' +
    {$ifdef CPUPOWERPC}
      'ppc' +
    {$else} {$ifdef CPUSPARC}
      'sparc' +
    {$endif CPUSPARC}
    {$endif CPUPOWERPC}
    {$endif CPUARM}
    {$endif CPUAARCH64}
    {$ifdef CPU32}
      '32'
    {$else}
      '64'
    {$endif CPU32}
    {$endif CPUX64}
    {$endif CPUX86};

var
  /// the target Operating System used for compilation, as TOperatingSystem
  // - a specific Linux distribution may be detected instead of plain osLinux
  OS_KIND: TOperatingSystem =
    {$ifdef OSWINDOWS}
      osWindows
    {$else} {$ifdef OSDARWIN}
      osOSX
    {$else} {$ifdef OSBSD}
      osBSD
    {$else} {$ifdef OSANDROID}
      osAndroid
    {$else} {$ifdef OSLINUX}
      osLinux
    {$else}
      osPOSIX
    {$endif OSLINUX}
    {$endif OSANDROID}
    {$endif OSBSD}
    {$endif OSDARWIN}
    {$endif OSWINDOWS};

  /// the current Operating System version, as retrieved for the current process
  // - contains e.g. 'Windows Seven 64 SP1 (6.1.7601)' or 'Windows XP SP3 (5.1.2600)' or
  // 'Windows 10 64bit 22H2 (10.0.19045.4046)' or 'macOS 13 Ventura (Darwin 22.3.0)' or
  // 'Ubuntu 16.04.5 LTS - Linux 3.13.0 110 generic#157 Ubuntu SMP Mon Feb 20 11:55:25 UTC 2017'
  OSVersionText: RawUtf8;
  /// some addition system information as text, e.g. 'Wine 1.1.5'
  // - also always appended to OSVersionText high-level description
  // - use if PosEx('Wine', OSVersionInfoEx) > 0 then to check for Wine presence
  OSVersionInfoEx: RawUtf8;
  /// the current Operating System version, as retrieved for the current process
  // and computed by ToTextOS(OSVersionInt32)
  // - contains e.g. 'Windows Vista' or 'Ubuntu Linux 5.4.0' or
  // 'macOS 13 Ventura 22.3.0'
  OSVersionShort: RawUtf8;

  {$ifdef OSWINDOWS}
  /// on Windows, the Update Build Revision as shown with the "ver/winver" command
  // - to track the current update state of the system
  WindowsUbr: integer;
  /// on Windows, the ready-to-be-displayed text version of the system
  // - e.g. 'Windows 10 Entreprise N'
  WindowsProductName: RawUtf8;
  /// on Windows, the ready-to-be-displayed text version of the system
  // - e.g. '22H2'
  WindowsDisplayVersion: RawUtf8;
  {$endif OSWINDOWS}

  /// some textual information about the current CPU and its known cache
  // - contains e.g. '4 x Intel(R) Core(TM) i5-7300U CPU @ 2.60GHz [3MB]'
  CpuInfoText: RawUtf8;
  /// the on-chip cache size, in bytes, as returned by the OS
  // - retrieved from /proc/cpuinfo "cache size" entry (L3 cache) on Linux or
  // CpuCache[3/4].Size (from GetLogicalProcessorInformation) on Windows
  CpuCacheSize: cardinal;
  /// the available cache information as returned by the OS
  // - e.g. 'L1=2*32KB  L2=256KB  L3=3MB' on Windows or '3072 KB' on Linux
  CpuCacheText: RawUtf8;
  /// some textual information about the current computer hardware, from BIOS
  // - contains e.g. 'LENOVO 20HES23B0U ThinkPad T470'
  BiosInfoText: RawUtf8;

  /// how many hardware CPU sockets are defined on this system
  // - i.e. the number of physical CPU slots, not the number of logical CPU
  // cores as returned by SystemInfo.dwNumberOfProcessors
  // - as used e.g. by SetThreadAffinity()
  CpuSockets: integer;

  /// Level 1 to 4 CPU caches as returned by GetLogicalProcessorInformation
  // - yes, Intel introduced a Level 4 cache (eDRAM) with some Haswell/Iris CPUs
  // - this information is not retrieved on all Linux / POSIX systems yet
  // - only Unified or Data caches are include (not Instruction or Trace)
  // - note: some CPU - like the Apple M1 - have 128 bytes of LineSize
  CpuCache: array[1..4] of record
    Count, Size, LineSize: cardinal;
  end;

  {$ifdef OSLINUXANDROID}
  /// contains the Flags: or Features: value of Linux /proc/cpuinfo
  CpuInfoFeatures: RawUtf8;
  {$endif OSLINUXANDROID}

  /// the running Operating System
  OSVersion32: TOperatingSystemVersion;
  /// the running Operating System, encoded as a 32-bit integer
  OSVersionInt32: integer absolute OSVersion32;

/// convert an Operating System type into its text representation
// - returns e.g. 'Windows Vista' or 'Ubuntu' or 'macOS 13 Ventura'
function ToText(const osv: TOperatingSystemVersion): RawUtf8; overload;

/// convert an Operating System type into its one-word text representation
// - returns e.g. 'Vista' or 'Ubuntu' or 'OSX'
function ToTextShort(const osv: TOperatingSystemVersion): RawUtf8;

/// convert a 32-bit Operating System type into its full text representation
// - including the kernel revision (not the distribution version) on POSIX systems
// - returns e.g. 'Windows Vista', 'Windows 11 64-bit 22000' or 'Ubuntu Linux 5.4.0'
function ToTextOS(osint32: integer): RawUtf8;

/// check if the current OS (i.e. OS_KIND value) match a description
// - will handle osPosix and osLinux as generic detection of those systems
// - osUnknown will always return true
function MatchOS(os: TOperatingSystem): boolean;

type
  /// the recognized ARM/AARCH64 CPU types
  // - https://github.com/karelzak/util-linux/blob/master/sys-utils/lscpu-arm.c
  // - is defined on all platforms for cross-system use
  TArmCpuType = (
    actUnknown,
    actARM810,
    actARM920,
    actARM922,
    actARM926,
    actARM940,
    actARM946,
    actARM966,
    actARM1020,
    actARM1022,
    actARM1026,
    actARM11MPCore,
    actARM1136,
    actARM1156,
    actARM1176,
    actCortexA5,
    actCortexA7,
    actCortexA8,
    actCortexA9,
    actCortexA12,
    actCortexA15,
    actCortexA17,
    actCortexR4,
    actCortexR5,
    actCortexR7,
    actCortexR8,
    actCortexM0,
    actCortexM1,
    actCortexM3,
    actCortexM4,
    actCortexM7,
    actCortexM0P,
    actCortexA32,
    actCortexA53,
    actCortexA35,
    actCortexA55,
    actCortexA65,
    actCortexA57,
    actCortexA72,
    actCortexA73,
    actCortexA75,
    actCortexA76,
    actNeoverseN1,
    actCortexA77,
    actCortexA76AE,
    actCortexR52,
    actCortexM23,
    actCortexM33,
    actNeoverseV1,
    actCortexA78,
    actCortexA78AE,
    actCortexX1,
    actCortex510,
    actCortex710,
    actCortexX2,
    actNeoverseN2,
    actNeoverseE1,
    actCortexA78C,
    actCortexX1C,
    actCortexA715,
    actCortexX3,
    actNeoverseV2,
    actCortexA520,
    actCortexA720,
    actCortexX4,
    actNeoverseV3,
    actNeoverseN3);
  /// a set of recognized ARM/AARCH64 CPU types
  TArmCpuTypes = set of TArmCpuType;

  /// the recognized ARM/AARCH64 CPU hardware implementers
  // - https://github.com/karelzak/util-linux/blob/master/sys-utils/lscpu-arm.c
  TArmCpuImplementer = (
    aciUnknown,
    aciARM,
    aciBroadcom,
    aciCavium,
    aciDEC,
    aciFUJITSU,
    aciHiSilicon,
    aciInfineon,
    aciMotorola,
    aciNVIDIA,
    aciAPM,
    aciQualcomm,
    aciSamsung,
    aciMarvell,
    aciApple,
    aciFaraday,
    aciIntel,
    aciMicrosoft,
    aciPhytium,
    aciAmpere);
  /// a set of recognized ARM/AARCH64 CPU hardware implementers
  TArmCpuImplementers = set of TArmCpuImplementer;

/// recognize a given ARM/AARCH64 CPU from its 12-bit hardware ID
function ArmCpuType(id: word): TArmCpuType;

/// recognize a given ARM/AARCH64 CPU type name from its 12-bit hardware ID
function ArmCpuTypeName(act: TArmCpuType; id: word): RawUtf8;

/// recognize a given ARM/AARCH64 CPU implementer from its 8-bit hardware ID
function ArmCpuImplementer(id: byte): TArmCpuImplementer;

/// recognize a given ARM/AARCH64 CPU implementer name from its 8-bit hardware ID
function ArmCpuImplementerName(aci: TArmCpuImplementer; id: word): RawUtf8;


const
  /// contains the Delphi/FPC Compiler Version as text
  // - e.g. 'Delphi 10.3 Rio', 'Delphi 2010' or 'Free Pascal 3.3.1'
  COMPILER_VERSION: RawUtf8 =
  {$ifdef FPC}
    'Free Pascal'
    {$ifdef VER2_6_4} + ' 2.6.4'{$endif}
    {$ifdef VER3_0}   + ' 3.0'
      {$ifdef VER3_0_4}   + '.4' {$else}
        {$ifdef VER3_0_2} + '.2' {$endif}
      {$endif VER3_0_4}
    {$endif VER3_0}
    {$ifdef VER3_1}   + ' 3.1'
       {$ifdef VER3_1_1} + '.1' {$endif}
    {$endif VER3_1}
    {$ifdef VER3_2}   + ' 3.2'
      {$ifdef VER3_2_4}     + '.4' {$else}
        {$ifdef VER3_2_3}   + '.3' {$else}
          {$ifdef VER3_2_2} + '.2' {$endif}
        {$endif VER3_2_3}
      {$endif VER3_2_4}
    {$endif VER3_2}
    {$ifdef VER3_3}   + ' 3.3'
       {$ifdef VER3_3_1} + '.1' {$endif}
    {$endif VER3_3}
    {$ifdef VER3_4}   + ' 3.4'  {$endif}
  {$else}
    'Delphi'
    {$if     defined(VER140)} + ' 6'
    {$elseif defined(VER150)} + ' 7'
    {$elseif defined(VER160)} + ' 8'
    {$elseif defined(VER170)} + ' 2005'
    {$elseif defined(VER185)} + ' 2007'
    {$elseif defined(VER180)} + ' 2006'
    {$elseif defined(VER200)} + ' 2009'
    {$elseif defined(VER210)} + ' 2010'
    {$elseif defined(VER220)} + ' XE'
    {$elseif defined(VER230)} + ' XE2'
    {$elseif defined(VER240)} + ' XE3'
    {$elseif defined(VER250)} + ' XE4'
    {$elseif defined(VER260)} + ' XE5'
    {$elseif defined(VER265)} + ' AppMethod 1'
    {$elseif defined(VER270)} + ' XE6'
    {$elseif defined(VER280)} + ' XE7'
    {$elseif defined(VER290)} + ' XE8'
    {$elseif defined(VER300)} + ' 10 Seattle'
    {$elseif defined(VER310)} + ' 10.1 Berlin'
    {$elseif defined(VER320)} + ' 10.2 Tokyo'
    {$elseif defined(VER330)} + ' 10.3 Rio'
    {$elseif defined(VER340)} + ' 10.4 Sydney'
    {$elseif defined(VER350)} + ' 11'
      {$if declared(RTLVersion113)} + '.3' {$else}
      {$if declared(RTLVersion112)} + '.2' {$else}
      {$if declared(RTLVersion111)} + '.1' {$ifend} {$ifend} {$ifend}
                              + ' Alexandria'
    {$elseif defined(VER360)} + ' 12'
      {$if declared(RTLVersion122)} + '.2' {$else}
      {$if declared(RTLVersion121)} + '.1' {$ifend} {$ifend}
                              + ' Athens'
    {$elseif defined(VER370)} + ' 13 Next'
    {$ifend}
  {$endif FPC}
  {$ifdef CPU64} + ' 64 bit' {$else} + ' 32 bit' {$endif};

{$ifndef PUREMORMOT2}
const
  HTTP_RESP_STATICFILE = STATICFILE_CONTENT_TYPE;

/// deprecated function: use COMPILER_VERSION constant instead
function GetDelphiCompilerVersion: RawUtf8; deprecated;
{$endif PUREMORMOT2}

{$ifdef OSWINDOWS}

{$ifdef UNICODE}

const
  /// a global constant to be appended for Windows Ansi or Wide API names
  // - match the Wide API on Delphi, since String=UnicodeString
  _AW = 'W';

{$else}

const
  /// a global constant to be appended for Windows Ansi or Wide API names
  // - match the Ansi API on FPC or oldest Delphi, where String=AnsiString
  _AW = 'A';

type
  /// low-level API structure, not defined in old Delphi versions
  TOSVersionInfoEx = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of char;
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;

{$endif UNICODE}

var
  /// is set to TRUE if the current process is a 32-bit image running under WOW64
  // - WOW64 is the x86 emulator that allows 32-bit Windows-based applications
  // to run seamlessly on 64-bit Windows
  // - equals always FALSE if the current executable is a 64-bit image
  IsWow64: boolean;
  /// the current System information, as retrieved for the current process
  // - under a WOW64 process, it will use the GetNativeSystemInfo() new API
  // to retrieve the real top-most system information
  // - note that the lpMinimumApplicationAddress field is replaced by a
  // more optimistic/realistic value ($100000 instead of default $10000)
  // - under BSD/Linux, only contain dwPageSize and dwNumberOfProcessors fields
  SystemInfo: TSystemInfo;
  /// low-level Operating System information, as retrieved for the current process
  OSVersionInfo: TOSVersionInfoEx;
  /// the current Windows edition, as retrieved for the current process
  OSVersion: TWindowsVersion;

{$else OSWINDOWS}

var
  /// emulate only some used fields of Windows' TSystemInfo
  SystemInfo: record
    /// retrieved from libc's getpagesize() - is expected to not be 0
    dwPageSize: cardinal;
    /// the number of available logical CPUs
    // - retrieved from HW_NCPU (BSD) or /proc/cpuinfo (Linux)
    // - see CpuSockets for the number of physical CPU sockets
    dwNumberOfProcessors: cardinal;
    /// meaningful system information, as returned by fpuname()
    uts: record
      sysname, release, version: RawUtf8;
    end;
    /// Linux Distribution release name, retrieved from /etc/*-release
    release: RawUtf8;
  end;

{$endif OSWINDOWS}

  /// the number of physical memory bytes available to the process
  // - equals TMemoryInfo.memtotal as retrieved from GetMemoryInfo() at startup
  SystemMemorySize: PtrUInt;

{$M+} // to have existing RTTI for published properties

type
  /// used to retrieve version information from any EXE
  // - under Linux, all version numbers are set to 0 by default, unless
  // you define the FPCUSEVERSIONINFO conditional and information is
  // extracted from executable resources
  // - for the main executable, do not create once instance of this class, but
  // call GetExecutableVersion / SetExecutableVersion and access the Executable
  // global variable
  TFileVersion = class
  protected
    fDetailed: string;
    fFileName: TFileName;
    fBuildDateTime: TDateTime;
    fVersionInfo, fUserAgent: RawUtf8;
    // change the version - returns true if supplied values are actually new
    function SetVersion(aMajor, aMinor, aRelease, aBuild: integer): boolean;
  public
    /// executable major version number
    Major: integer;
    /// executable minor version number
    Minor: integer;
    /// executable release version number
    Release: integer;
    /// executable release build number
    Build: integer;
    /// build year of this exe file
    BuildYear: word;
    /// version info of the exe file as '3.1'
    // - return "string" type, i.e. UnicodeString for Delphi 2009+
    Main: string;
    /// associated CompanyName string version resource
    CompanyName: RawUtf8;
    /// associated FileDescription string version resource
    FileDescription: RawUtf8;
    /// associated FileVersion string version resource
    FileVersion: RawUtf8;
    /// associated InternalName string version resource
    InternalName: RawUtf8;
    /// associated LegalCopyright string version resource
    LegalCopyright: RawUtf8;
    /// associated OriginalFileName string version resource
    OriginalFilename: RawUtf8;
    /// associated ProductName string version resource
    ProductName: RawUtf8;
    /// associated ProductVersion string version resource
    ProductVersion: RawUtf8;
    /// associated Comments string version resource
    Comments: RawUtf8;
    /// associated Language Translation string version resource
    LanguageInfo: RawUtf8;
    /// initialize the version information, with optional custom values
    // - will set the version numbers, and get BuildDateTime/BuildYear
    // - call RetrieveInformationFromFileName to parse its internal resources
    // - for the main executable, do not use this constructor, but call
    // GetExecutableVersion / SetExecutableVersion and access the Executable
    // global variable
    constructor Create(const aFileName: TFileName; aMajor: integer = 0;
      aMinor: integer = 0; aRelease: integer = 0; aBuild: integer = 0);
    /// open and extract file information from the executable FileName
    // - note that resource extraction is not available on POSIX, unless the
    // FPCUSEVERSIONINFO conditional has been specified in the project options
    function RetrieveInformationFromFileName: boolean;
    /// retrieve the version as a 32-bit integer with Major.Minor.Release
    // - following Major shl 16+Minor shl 8+Release bit pattern
    function Version32: integer;
    /// build date and time of this exe file, as plain text
    function BuildDateTimeString: string;
    /// version info of the exe file as '3.1.0.123' or ''
    // - this method returns '' if Detailed is '0.0.0.0'
    function DetailedOrVoid: string;
    /// returns the version information of this exe file as text
    // - includes FileName (without path), Detailed and BuildDateTime properties
    // - e.g. 'myprogram.exe 3.1.0.123 (2016-06-14 19:07:55)'
    function VersionInfo: RawUtf8;
    /// returns a ready-to-use User-Agent header with exe name, version and OS
    // - e.g. 'myprogram/3.1.0.123W32' for myprogram running on Win32
    // - here OS_INITIAL[] character is used to identify the OS, with '32'
    // appended on Win32 only (e.g. 'myprogram/3.1.0.2W', is for Win64)
    // - use UserAgentParse() to decode this text into meaningful information
    function UserAgent: RawUtf8;
    /// returns the version information of a specified exe file as text
    // - includes FileName (without path), Detailed and BuildDateTime properties
    // - e.g. 'myprogram.exe 3.1.0.123 2016-06-14 19:07:55'
    class function GetVersionInfo(const aFileName: TFileName): RawUtf8;
  published
    /// version info of the exe file as '3.1.0.123'
    // - return "string" type, i.e. UnicodeString for Delphi 2009+
    // - under Linux, always return '0.0.0.0' if no custom version number
    // has been defined
    // - consider using DetailedOrVoid method if '0.0.0.0' is not expected
    property Detailed: string
      read fDetailed write fDetailed;
    /// build date and time of this exe file
    property BuildDateTime: TDateTime
      read fBuildDateTime write fBuildDateTime;
  end;

{$M-}

/// quickly parse the TFileVersion.UserAgent content
// - identify e.g. 'myprogram/3.1.0.2W' or 'myprogram/3.1.0.2W32' text
function UserAgentParse(const UserAgent: RawUtf8;
  out ProgramName, ProgramVersion: RawUtf8;
  out OS: TOperatingSystem): boolean;

type
  /// the command line switches supported by TExecutableCommandLine
  // - clkArg is for "exename arg1 arg2 arg3" main indexed arguments
  // - clkOption is for "exename -o --opt1" boolean flags
  // - clkParam is for "exename -n value --name value --name2=value2" pairs
  TExecutableCommandLineKind = (
    clkUndefined,
    clkArg,
    clkOption,
    clkParam);

  /// implements command-line arguments parsing e.g. for TExecutable.Command
  // - call Arg() Options() and Get/Param() to define and retrieve the flags
  // from their names and supply some description text, then call
  // DetectUnknown and/or FullDescription to interact with the user
  // - by default, will use -/-- switches on POSIX, and / on Windows
  TExecutableCommandLine = class
  protected
    fNames: array[clkArg .. clkParam] of TRawUtf8DynArray;
    fRawParams, fValues: TRawUtf8DynArray; // for clkParam
    fDesc, fDescDetail: array[clkArg .. clkParam] of RawUtf8;
    fRetrieved: array[clkArg .. clkParam] of TBooleanDynArray;
    fDescArg: TRawUtf8DynArray;
    fCaseSensitiveNames: boolean;
    fSwitch: array[{long=}boolean] of RawUtf8;
    fLineFeed, fExeDescription: RawUtf8;
    procedure Describe(const v: array of RawUtf8;
      k: TExecutableCommandLineKind; d, def: RawUtf8; argindex: integer);
    function Find(const v: array of RawUtf8;
      k: TExecutableCommandLineKind = clkUndefined; const d: RawUtf8 = '';
      const def: RawUtf8 = ''; f: PtrInt = 0): PtrInt;
  public
    /// mark and describe an "arg" value by 0-based index in Args[]
    function Arg(index: integer; const description: RawUtf8 = '';
      optional: boolean = true): boolean; overload;
    /// mark and describe a string/TFileName "arg" value by 0-based index in Args[]
    function ArgString(index: integer; const description: RawUtf8 = '';
      optional: boolean = true): string;
    /// mark and describe an "arg" value in Args[]
    function Arg(const name: RawUtf8;
      const description: RawUtf8 = ''): boolean; overload;
    /// mark and describe or or several "arg" value(s) in Args[]
    function Arg(const name: array of RawUtf8;
      const description: RawUtf8 = ''): boolean; overload;
    /// search for "-optionname" switches in Options[]
    function Option(const name: RawUtf8;
      const description: RawUtf8 = ''): boolean; overload;
    /// search for "-optionname" switches in Options[]
    function Option(const name: array of RawUtf8;
      const description: RawUtf8 = ''): boolean; overload;
    /// search for "-parametername" and return its RawUtf8 "parametervalue"
    function Get(const name: RawUtf8; out value: RawUtf8;
      const description: RawUtf8 = ''; const default: RawUtf8 = ''): boolean; overload;
    /// search for "-parametername" and return its RawUtf8 "parametervalue"
    function Get(const name: array of RawUtf8; out value: RawUtf8;
      const description: RawUtf8 = ''; const default: RawUtf8 = ''): boolean; overload;
    /// search for "-parametername" and return all RawUtf8 "parametervalue" occurrences
    function Get(const name: array of RawUtf8; out value: TRawUtf8DynArray;
      const description: RawUtf8 = ''): boolean; overload;
    /// search for "-parametername" and return its plain string "parametervalue"
    function Get(const name: RawUtf8; out value: string;
      const description: RawUtf8 = ''; const default: string = ''): boolean; overload;
    /// search for "-parametername" and return all string "parametervalue" occurrences
    function Get(const name: array of RawUtf8; out value: TStringDynArray;
      const description: RawUtf8 = ''): boolean; overload;
    /// search for "-parametername" and return its plain string "parametervalue"
    function Get(const name: array of RawUtf8; out value: string;
      const description: RawUtf8 = ''; const default: string = ''): boolean; overload;
    /// search for "-parametername" and return all string "parametervalue" occurrences
    function Get(const name: RawUtf8; out value: TStringDynArray;
      const description: RawUtf8 = ''): boolean; overload;
    /// search for "-parametername" and return its integer "parametervalue"
    function Get(const name: RawUtf8; out value: integer;
      const description: RawUtf8 = ''; default: integer = maxInt): boolean; overload;
    /// search for "-parametername" and return its integer "parametervalue"
    function Get(const name: array of RawUtf8; out value: integer;
      const description: RawUtf8 = ''; default: integer = maxInt): boolean; overload;
    /// search for "-parametername" and return its integer "parametervalue"
    function Get(const name: RawUtf8; min, max: integer; out value: integer;
      const description: RawUtf8 = ''; default: integer = maxInt): boolean; overload;
    /// search for "-parametername" and return its integer "parametervalue"
    function Get(const name: array of RawUtf8; min, max: integer;
      out value: integer; const description: RawUtf8 = '';
      default: integer = -1): boolean; overload;
    /// search for "-parametername" parameter in Names[]
    function Has(const name: RawUtf8): boolean; overload;
    /// search for "-parametername" parameter in Names[]
    function Has(const name: array of RawUtf8): boolean; overload;
    /// search for "-parametername" and return '' or its RawUtf8 "parametervalue"
    function Param(const name: RawUtf8; const description: RawUtf8 = '';
      const default: RawUtf8 = ''): RawUtf8; overload;
    /// search for "-parametername" and return '' or its string "parametervalue"
    function ParamS(const name: array of RawUtf8; const description: RawUtf8 = '';
      const default: string = ''): string;
    /// search for "-parametername" and return '' or its RawUtf8 "parametervalue"
    function Param(const name: array of RawUtf8; const description: RawUtf8 = '';
      const default: RawUtf8 = ''): RawUtf8; overload;
    /// search for "-parametername" and return its integer "parametervalue" or default
    function Param(const name: RawUtf8; default: integer;
      const description: RawUtf8 = ''): integer; overload;
    /// search for "-parametername" and return its integer "parametervalue" or default
    function Param(const name: array of RawUtf8; default: integer;
      const description: RawUtf8 = ''): integer; overload;
    /// generate the text from all Arg() Options() and Get/Param() descriptions
    // and the supplied high-level description of the program
    // - the parameter <name> would be extracted from any #word in the
    // description text,
    // - for instance:
    // ! with Executable.Command do
    // ! begin
    // !   ExeDescription := 'An executable to test mORMot Execute.Command';
    // !   verbose := Option(['v', 'verbose'], 'generate verbose output');
    // !   Get(['t', 'threads'], threads, '#number of threads to run', 5);
    // !   ConsoleWrite(FullDescription);
    // ! end;
    // will fill "verbose" and "threads" local variables, and output on Linux:
    // $ An executable to test mORMot Execute.Command
    // $
    // $ Usage: mormot2tests [options] [params]
    // $
    // $ Options:
    // $   -v, --verbose       generate verbose output
    // $
    // $ Params:
    // $   -t, --threads <number> (default 5)
    // $                       number of threads to run
    function FullDescription(const customexedescription: RawUtf8 = '';
      const exename: RawUtf8 = ''; const onlyusage: RawUtf8 = ''): RawUtf8;
    /// check if the supplied parameters were all registered from previous
    // Arg() Options() and Get/Param() calls
    // - return '' if no unexpected flag has been supplied
    // - return an error message like 'Unexpected --name option' otherwise
    function DetectUnknown: RawUtf8;
    /// call DetectUnknown and output any error message to the console
    // - return false if the parameters are valid
    // - otherwise, return true and caller should exit the process
    function ConsoleWriteUnknown(const exedescription: RawUtf8 = ''): boolean;
    /// define 'h help' and call ConsoleWriteUnknown()
    // - caller should exit the process if this method returned true
    function ConsoleHelpFailed(const exedescription: RawUtf8 = ''): boolean;
    /// fill the stored arguments and options from executable parameters
    // - called e.g. at unit inialization to set Executable.CommandLine variable
    // - you can execute it again e.g. to customize the switches characters
    function Parse(const DescriptionLineFeed: RawUtf8 = CRLF;
      const ShortSwitch: RawUtf8 = {$ifdef OSWINDOWS} '/' {$else} '-' {$endif};
      const LongSwitch: RawUtf8 = {$ifdef OSWINDOWS} '/' {$else} '--' {$endif}): boolean;
    /// remove all recognized arguments and switches
    procedure Clear;
    /// internal method returning a switch text from its identifier
    function SwitchAsText(const v: RawUtf8): RawUtf8;
    /// the ParamStr(1..ParamCount) arguments as RawUtf8, excluding Options[]
    // switches and Params[]/Values[] parameters
    property Args: TRawUtf8DynArray
      read fNames[clkArg];
    /// the "-optionname" boolean switches as stored in ParamStr()
    property Options: TRawUtf8DynArray
      read fNames[clkOption];
    /// the names of "-parametername parametervalue" as stored in ParamStr()
    // - mapping the Values[] associated array
    property Names: TRawUtf8DynArray
      read fNames[clkParam];
    /// the values of "-parametername parametervalue" as stored in ParamStr()
    // - mapping the Names[] associated array
    property Values: TRawUtf8DynArray
      read fValues;
    /// if search within Args[] Options[] or Names[] should be case-sensitive
    property CaseSensitiveNames: boolean
      read fCaseSensitiveNames write fCaseSensitiveNames;
    /// set a text which describes the executable
    // - as used by default by FullDescription() and ConsoleWriteUnknown()
    property ExeDescription: RawUtf8
      read fExeDescription write fExeDescription;
    /// DescriptionLineFeed value from TExecutableCommandLine.Parse()
    property LineFeed: RawUtf8
      read fLineFeed write fLineFeed;
    /// map ParamStr(1 .. ParamCount) values, encoded as RawUtf8
    // - may be used e.g. for regression tests instead of ParamStr()
    property RawParams: TRawUtf8DynArray
      read fRawParams write fRawParams;
  end;

  /// stores some global information about the current executable and computer
  // - as set at unit initialization into the Executable global variable
  TExecutable = record
    /// the main executable name, without any path nor extension
    // - e.g. 'Test' for 'c:\pathto\Test.exe'
    ProgramName: RawUtf8;
    /// the main executable details, as used e.g. by TSynLog
    // - e.g. 'C:\Dev\lib\SQLite3\exe\TestSQL3.exe 1.2.3.123 (2011-03-29 11:09:06)'
    // - you should have called GetExecutableVersion or SetExecutableVersion
    // to populate this field
    ProgramFullSpec: RawUtf8;
    /// the main executable file name (including full path)
    // - same as paramstr(0)
    ProgramFileName: TFileName;
    /// the main executable full path (excluding .exe file name)
    // - same as ExtractFilePath(paramstr(0))
    ProgramFilePath: TFileName;
    /// the full path of the running executable or library
    // - for an executable, same as paramstr(0)
    // - for a library, will contain the whole .dll file name
    InstanceFileName: TFileName;
    /// the current executable version
    // - you should have called GetExecutableVersion or SetExecutableVersion
    // to populate this field
    Version: TFileVersion;
    /// the current computer host name
    Host: RawUtf8;
    /// the current computer user name
    User: RawUtf8;
    /// some hash representation of this information
    // - the very same executable on the very same computer run by the very
    // same user will always have the same Hash value
    // - is computed from the crc32c of this TExecutable fields: c0 from
    // Version32, CpuFeatures and Host, c1 from User, c2 from ProgramFullSpec
    // and c3 from InstanceFileName
    // - may be used as an entropy seed, or to identify a process execution
    Hash: THash128Rec;
    /// the Command Line arguments, parsed during unit initialization
    Command: TExecutableCommandLine;
  end;

var
  /// global information about the current executable and computer
  // - this structure is initialized in this unit's initialization block below
  // but you need to call GetExecutableVersion to initialize its Version fields
  // from the executable version resource (if any)
  // - you can call SetExecutableVersion() with a custom version, if needed
  Executable: TExecutable;

  {$ifndef PUREMORMOT2}
  /// deprecated global: use Executable variable instead
  ExeVersion: TExecutable absolute Executable;
  {$endif PUREMORMOT2}

/// initialize Executable global variable, from the program version resources
// - is not retrieved at startup, unless this function is especially called
// - on POSIX, requires FPCUSEVERSIONINFO conditional to be set for the project
// - use SetExecutableVersion() if you want to force a custom version
// - is in fact just a wrapper around SetExecutableVersion(0, 0, 0, 0)
procedure GetExecutableVersion;

/// initialize Executable global variable with custom version numbers
// - GetExecutableVersion will retrieve version information from the
// executable itself (if it was included at build time and FPCUSEVERSIONINFO
// conditional was specified for the project)
// - but you can use this function to set any custom version number
procedure SetExecutableVersion(aMajor, aMinor, aRelease, aBuild: integer); overload;

/// initialize Executable global variable, supplying the version as text
// - e.g. SetExecutableVersion('7.1.2.512');
procedure SetExecutableVersion(const aVersionText: RawUtf8); overload;

/// return a function/method location according to the supplied code address
// - returns the address as hexadecimal by default, e.g. '004cb765'
// - if mormot.core.log.pas is defined in the project, will redirect to
// TDebugFile.FindLocationShort() method using .map/.dbg/.mab information, and
// return filename, symbol name and line number (if any) as plain text, e.g.
// '4cb765 ../src/core/mormot.core.base.pas statuscodeissuccess (11183)' on FPC
var
  GetExecutableLocation: function(aAddress: pointer): ShortString;

/// try to retrieve the file name of the executable/library holding a function
// - calls dladdr() on POSIX, or GetModuleFileName() on Windows
function GetExecutableName(aAddress: pointer): TFileName;

var
  /// retrieve the MAC addresses of all hardware network adapters
  // - mormot.net.sock.pas will inject here its own cross-platform version
  // - this unit will include a simple parser of /sys/class/net/* for Linux only
  // - as used e.g. by GetComputerUuid() fallback if SMBIOS is not available
  GetSystemMacAddress: function: TRawUtf8DynArray;


type
  /// identify an operating system folder for GetSystemPath()
  // - on Windows, spCommonData maps e.g. 'C:\ProgramData',
  // spUserData points to 'C:\Users\<user>\AppData\Local',
  // spCommonDocuments to 'C:\Users\Public\Documents',
  // spUserDocuments to 'C:\Users\<user>\Documents',
  // spTemp will call GetTempPath() or read the $TEMP environment variable,
  // pointing typically to 'C:\Users\<user>\AppData\Local\Temp\',
  // and spLog either to '<exepath>\log' or
  // 'C:\Users\<user>\AppData\Local\<exename>-log' (the first writable)
  // - on POSIX, spTemp will use $TMPDIR/$TMP environment variables,
  // spCommonData, spCommonDocuments and spUserDocuments point to $HOME,
  // spUserData maps $XDG_CACHE_HOME or '$HOME/.cache' or '$TMP/<user>', and
  // spLog maps '/var/log/<exename>' or '<exepath>/log' or '$TMP/<exename>-log'
  // - on all systems, returned spTemp, spLog and spUserData folders are always
  // writable by the current user
  TSystemPath = (
    spCommonData,
    spUserData,
    spCommonDocuments,
    spUserDocuments,
    spTemp,
    spLog);

{$ifndef PUREMORMOT2}
const
  spTempFolder = spTemp;
{$endif PUREMORMOT2}

/// returns an operating system folder
// - will return the full path of a given kind of private or shared folder,
// depending on the underlying operating system
// - will use SHGetFolderPath and the corresponding CSIDL constant under Windows
// - under POSIX, will return the proper environment variable
// - spLog is a writable sub-folder specific to mORMot, always created if needed
// - returned folder name contains the trailing path delimiter (\ or /)
function GetSystemPath(kind: TSystemPath): TFileName;

/// force an operating system folder
// - if the default location is not good enough for your project
// - will just check that the directory exists, not that it is writable
function SetSystemPath(kind: TSystemPath; const path: TFileName): boolean;

type
  /// identify the (Windows) system certificate stores for GetSystemStoreAsPem()
  // - ignored on POSIX systems, in which the main cacert.pem file is used
  // - scsCA contains known Certification Authority certificates, i.e. from
  // entities entrusted to issue certificates that assert that the recipient
  // individual, computer, or organization requesting the certificate fulfills
  // the conditions of an established policy
  // - scsMY holds certificates with associated private keys (Windows only)
  // - scsRoot contains known Root certificates, i.e. self-signed CA certificates
  // which are the root of the whole certificates trust tree
  // - scsSpc contains Software Publisher Certificates (Windows only)
  TSystemCertificateStore = (
    scsCA,
    scsMY,
    scsRoot,
    scsSpc);
  TSystemCertificateStores = set of TSystemCertificateStore;

var
  /// the local PEM file name to be searched by GetSystemStoreAsPem() to
  // override the OS certificates store
  // - a relative file name (i.e. with no included path, e.g. 'cacert.pem') will
  // be searched in the Executable.ProgramFilePath folder
  // - an absolute file name (e.g. 'C:\path\to\file.pem' or '/posix/path') could
  // also be specified
  // - set by default to '' to disable this override (for security purposes)
  GetSystemStoreAsPemLocalFile: TFileName;

/// retrieve the OS certificates store as PEM text
// - first search for [Executable.ProgramFilePath+]GetSystemStoreAsPemLocalFile,
// then for a file pointed by a 'SSL_CA_CERT_FILE' environment variable - unless
// OnlySystemStore is forced to true
// - if no such file exists, or if OnlySystemStore is true, will concatenate the
// supplied CertStores values via individual GetOneSystemStoreAsPem() calls
// - return CA + ROOT certificates by default, ready to validate a certificate
// - Darwin specific API is not supported yet, and is handled as a BSD system
// - an internal cache is refreshed every 4 minutes unless FlushCache is set
function GetSystemStoreAsPem(
  CertStores: TSystemCertificateStores = [scsCA, scsRoot];
  FlushCache: boolean = false; OnlySystemStore: boolean = false): RawUtf8;

/// retrieve all certificates of a given system store as PEM text
// - on Windows, will use the System Crypt API
// - on POSIX, scsRoot loads the main CA file of the known system file, and
// scsCA the additional certificate files which may not be part of the main file
// - GetSystemStoreAsPemLocalFile file and 'SSL_CA_CERT_FILE' environment
// variables are ignored: call GetSystemStoreAsPem() instead for the global store
// - an internal cache is refreshed every 4 minutes unless FlushCache is set
function GetOneSystemStoreAsPem(CertStore: TSystemCertificateStore;
  FlushCache: boolean = false; now: cardinal = 0): RawUtf8;

type
  /// the raw SMBIOS information as filled by GetRawSmbios
  // - first 4 bytes are $010003ff on POSIX if read from /var/tmp/.synopse.smb
  TRawSmbiosInfo = record
    /// some flag only set by GetSystemFirmwareTable() Windows API
    Reserved: byte;
    /// typically 2-3
    SmbMajorVersion: byte;
    /// typically 0-1
    SmbMinorVersion: byte;
    /// typically 0 for SMBIOS 2.1, 1 for SMBIOS 3.0
    DmiRevision: byte;
    /// the length of encoded binary in data
    Length: DWORD;
    /// low-level binary of the SMBIOS Structure Table
    Data: RawByteString;
  end;

var
  /// global variable filled by GetRawSmbios from SMBIOS binary information
  RawSmbios: TRawSmbiosInfo;

/// retrieve the SMBIOS raw information as a single RawSmbios gloabl binary blob
// - will try the Windows API if available, or search and parse the main system
// memory with UEFI redirection if needed - via /systab system file on Linux, or
// kenv() on FreeBSD (only fully tested to work on Windows XP+ and Linux)
// - follow DSP0134 3.6.0 System Management BIOS (SMBIOS) Reference Specification
// with both SMBIOS 2.1 (32-bit) or SMBIOS 3.0 (64-bit) entry points
// - the current user should have enough rights to read the main system memory,
// which means it should be root on most POSIX Operating Systems - so we persist
// this raw binary in /var/tmp/.synopse.smb to retrieve it from non-root user
function GetRawSmbios: boolean;

type
  /// the basic SMBIOS fields supported by GetSmbios/DecodeSmbios functions
  // - only include the first occurrence for board/cpu/battery types
  // - see TSmbiosInfo in mormot.core.perf.pas for more complete decoding
  TSmbiosBasicInfo = (
    sbiUndefined,
    sbiBiosVendor,
    sbiBiosVersion,
    sbiBiosFirmware,
    sbiBiosRelease,
    sbiBiosDate,
    sbiManufacturer,
    sbiProductName,
    sbiVersion,
    sbiSerial,
    sbiUuid,
    sbiSku,
    sbiFamily,
    sbiBoardManufacturer,
    sbiBoardProductName,
    sbiBoardVersion,
    sbiBoardSerial,
    sbiBoardAssetTag,
    sbiBoardLocation,
    sbiCpuManufacturer,
    sbiCpuVersion,
    sbiCpuSerial,
    sbiCpuAssetTag,
    sbiCpuPartNumber,
    sbiBatteryLocation,
    sbiBatteryManufacturer,
    sbiBatteryName,
    sbiBatteryVersion,
    sbiBatteryChemistry,
    sbiOem
  );

  /// the text fields stored by GetSmbios/DecodeSmbios functions
  TSmbiosBasicInfos = array[TSmbiosBasicInfo] of RawUtf8;

/// decode basic SMBIOS information as text from a TRawSmbiosInfo binary blob
// - see DecodeSmbiosInfo() in mormot.core.perf.pas for a more complete decoder
// - returns the total size of DMI/SMBIOS information in raw.data (may be lower)
// - will also adjust raw.Length and truncate raw.Data to the actual useful size
function DecodeSmbios(var raw: TRawSmbiosInfo; out info: TSmbiosBasicInfos): PtrInt;

// some global definitions for proper caching and inlining of GetSmbios()
procedure ComputeGetSmbios;
procedure DecodeSmbiosUuid(src: PGuid; out dest: RawUtf8; const raw: TRawSmbiosInfo);
var
  _Smbios: TSmbiosBasicInfos;
  _SmbiosRetrieved: boolean;

  /// customize how DecodeSmbiosUuid() handle endianess of its first bytes
  // - sduDirect will directly use GUIDToString() layout (seems expected on
  // Windows to match "wmic csproduct get uuid" value)
  // - sduInvert will force first values inversion (mandatory on MacOS)
  // - sduVersion will invert for SMBios version < 2.6 (set outside Windows)
  _SmbiosDecodeUuid: (sduDirect, sduInvert, sduVersion)
    {$ifdef OSDARWIN}  = sduInvert  {$else}
      {$ifdef OSPOSIX} = sduVersion {$endif} {$endif};

/// retrieve SMBIOS information as text
// - only the main values are decoded - see GetSmbiosInfo in mormot.core.perf
// for a more complete DMI/SMBIOS decoder
// - on POSIX, requires root to access full SMBIOS information - will fallback
// reading /sys/class/dmi/id/* on Linux or kenv() on FreeBSD for most entries
// if we found no previous root-retrieved cache in local /var/tmp/.synopse.smb
// - see _SmbiosDecodeUuid global flag for UUID decoding
function GetSmbios(info: TSmbiosBasicInfo): RawUtf8;
  {$ifdef HASINLINE} inline; {$endif}

/// retrieve a genuine 128-bit UUID identifier for this computer
// - first try GetSmbios(sbiUuid), i.e. the SMBIOS System UUID
// - otherwise, will compute a genuine hash from known hardware information
// (CPU, Bios, MAC) and store it in a local file for the next access, e.g. into
// '/var/tmp/.synopse.uid' on POSIX
// - on Mac, include the mormot.core.os.mac unit to properly read this UUID
// - note: some BIOS have no UUID, so we fallback to our hardware hash on those
procedure GetComputerUuid(out uuid: TGuid);


{ ****************** Operating System Specific Types (e.g. TWinRegistry) }

{$ifdef OSWINDOWS}

type
  TThreadID     = DWORD;
  TMessage      = Messages.TMessage;
  HWND          = Windows.HWND;
  BOOL          = Windows.BOOL;
  LARGE_INTEGER = Windows.LARGE_INTEGER;
  TFileTime     = Windows.FILETIME;
  PFileTime     = ^TFileTime;

  /// the known Windows Registry Root key used by TWinRegistry.ReadOpen
  TWinRegistryRoot = (
    wrClasses,
    wrCurrentUser,
    wrLocalMachine,
    wrUsers);

  /// direct access to the Windows Registry
  // - could be used as alternative to TRegistry, which doesn't behave the same on
  // all Delphi versions, and is enhanced on FPC (e.g. which supports REG_MULTI_SZ)
  // - is also Unicode ready for text, using UTF-8 conversion on all compilers
  {$ifdef USERECORDWITHMETHODS}
  TWinRegistry = record
  {$else}
  TWinRegistry = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the opened HKEY handle
    key: HKEY;
    /// start low-level read access to a Windows Registry node
    // - on success (returned true), Close method should be eventually called
    function ReadOpen(root: TWinRegistryRoot; const keyname: RawUtf8;
      closefirst: boolean = false): boolean;
    /// finalize low-level read access to the Windows Registry after ReadOpen()
    procedure Close;
    /// read a UTF-8 string from the Windows Registry after ReadOpen()
    // - in respect to Delphi's TRegistry, will properly handle REG_MULTI_SZ
    // (return the first value of the multi-list) - use ReadData to retrieve
    // all REG_MULTI_SZ values as one blob
    // - we don't use string here since it would induce a dependency to
    // mormot.core.unicode
    function ReadString(const entry: SynUnicode; andtrim: boolean = true): RawUtf8;
    /// read a Windows Registry content after ReadOpen()
    // - works with any kind of key, but was designed for REG_BINARY
    function ReadData(const entry: SynUnicode): RawByteString;
    /// read a Windows Registry 32-bit REG_DWORD value after ReadOpen()
    function ReadDword(const entry: SynUnicode): cardinal;
    /// read a Windows Registry 64-bit REG_QWORD value after ReadOpen()
    function ReadQword(const entry: SynUnicode): QWord;
    /// read a Windows Registry content as binary buffer after ReadOpen()
    // - just a wrapper around RegQueryValueExW() API call
    function ReadBuffer(const entry: SynUnicode; Data: pointer; DataLen: DWORD): boolean;
    /// retrieve a Windows Registry content size as binary bytes after ReadOpen()
    // - returns -1 if the entry is not found
    function ReadSize(const entry: SynUnicode): integer;
    /// enumeration of all sub-entries names of a Windows Registry key
    function ReadEnumEntries: TRawUtf8DynArray;
  end;

  /// TSynWindowsPrivileges enumeration synchronized with WinAPI
  // - see https://docs.microsoft.com/en-us/windows/desktop/secauthz/privilege-constants
  TWinSystemPrivilege = (
    wspCreateToken,
    wspAssignPrimaryToken,
    wspLockMemory,
    wspIncreaseQuota,
    wspUnsolicitedInput,
    wspMachineAccount,
    wspTCP,
    wspSecurity,
    wspTakeOwnership,
    wspLoadDriver,
    wspSystemProfile,
    wspSystemTime,
    wspProfSingleProcess,
    wspIncBasePriority,
    wspCreatePageFile,
    wspCreatePermanent,
    wspBackup,
    wspRestore,
    wspShutdown,
    wspDebug,
    wspAudit,
    wspSystemEnvironment,
    wspChangeNotify,
    wspRemoteShutdown,
    wspUndock,
    wspSyncAgent,
    wspEnableDelegation,
    wspManageVolume,
    wspImpersonate,
    wspCreateGlobal,
    wspTrustedCredmanAccess,
    wspRelabel,
    wspIncWorkingSet,
    wspTimeZone,
    wspCreateSymbolicLink);

  /// TSynWindowsPrivileges set synchronized with WinAPI
  TWinSystemPrivileges = set of TWinSystemPrivilege;

  /// define which WinAPI token is to be retrieved
  // - define the execution context, i.e. if the token is used for the current
  // process or the current thread
  // - used e.g. by TSynWindowsPrivileges or CurrentSid()
  TWinTokenType = (
    wttProcess,
    wttThread);

  /// manage available privileges on Windows platform
  // - not all available privileges are active for all process
  // - for usage of more advanced WinAPI, explicit enabling of privilege is
  // sometimes needed
  {$ifdef USERECORDWITHMETHODS}
  TSynWindowsPrivileges = record
  {$else}
  TSynWindowsPrivileges = object
  {$endif USERECORDWITHMETHODS}
  private
    fAvailable: TWinSystemPrivileges;
    fEnabled: TWinSystemPrivileges;
    fDefEnabled: TWinSystemPrivileges;
    fToken: THandle;
    function SetPrivilege(wsp: TWinSystemPrivilege; on: boolean): boolean;
    procedure LoadPrivileges;
  public
    /// initialize the object dedicated to management of available privileges
    // - aTokenPrivilege can be used for current process or current thread
    procedure Init(aTokenPrivilege: TWinTokenType = wttProcess;
      aLoadPrivileges: boolean = true);
    /// finalize the object and relese Token handle
    // - aRestoreInitiallyEnabled parameter can be used to restore initially
    // state of enabled privileges
    procedure Done(aRestoreInitiallyEnabled: boolean = true);
    /// enable privilege
    // - if aPrivilege is already enabled return true, if operation is not
    // possible (required privilege doesn't exist or API error) return false
    function Enable(aPrivilege: TWinSystemPrivilege): boolean;
    /// disable privilege
    // - if aPrivilege is already disabled return true, if operation is not
    // possible (required privilege doesn't exist or API error) return false
    function Disable(aPrivilege: TWinSystemPrivilege): boolean;
    /// set of available privileges for current process/thread
    property Available: TWinSystemPrivileges
      read fAvailable;
    /// set of enabled privileges for current process/thread
    property Enabled: TWinSystemPrivileges
      read fEnabled;
    /// low-level access to the privileges token handle
    property Token: THandle
      read fToken;
  end;

  /// which information was returned by GetProcessInfo() overloaded functions
  // - wpaiPID is set when PID was retrieved
  // - wpaiBasic with ParentPID/BasePriority/ExitStatus/PEBBaseAddress/AffinityMask
  // - wpaiPEB with SessionID/BeingDebugged
  // - wpaiCommandLine and wpaiImagePath when CommandLine and ImagePath are set
  TWinProcessAvailableInfos = set of (
    wpaiPID,
    wpaiBasic,
    wpaiPEB,
    wpaiCommandLine,
    wpaiImagePath);

  /// information returned by GetProcessInfo() overloaded functions
  TWinProcessInfo = record
    /// which information was returned within this structure
    AvailableInfo: TWinProcessAvailableInfos;
    /// the Process ID
    PID: cardinal;
    /// the Parent Process ID
    ParentPID: cardinal;
    /// Terminal Services session identifier associated with this process
    SessionID: cardinal;
    /// points to the low-level internal PEB structure
    // - you can not directly access this memory, unless ReadProcessMemory()
    // with proper wspDebug priviledge API is called
    PEBBaseAddress: pointer;
    /// GetProcessAffinityMask-like value
    AffinityMask: cardinal;
    /// process priority
    BasePriority: integer;
    /// GetExitCodeProcess-like value
    ExitStatus: integer;
    /// indicates whether the specified process is currently being debugged
    BeingDebugged: byte;
    /// command-line string passed to the process
    CommandLine: SynUnicode;
    /// path of the image file for the process
    ImagePath: SynUnicode;
  end;

  PWinProcessInfo = ^TWinProcessInfo;
  TWinProcessInfoDynArray = array of TWinProcessInfo;

  /// the SID types, as recognized by LookupSid()
  TSidType = (
    stUndefined,
    stTypeUser,
    stTypeGroup,
    stTypeDomain,
    stTypeAlias,
    stTypeWellKnownGroup,
    stTypeDeletedAccount,
    stTypeInvalid,
    stTypeUnknown,
    stTypeComputer,
    stTypeLabel,
    stTypeLogonSession);


function ToText(p: TWinSystemPrivilege): PShortString; overload;

/// calls OpenProcessToken() or OpenThreadToken() to get the current token
// - caller should then run CloseHandle() once done with the Token handle
function RawTokenOpen(wtt: TWinTokenType; access: cardinal): THandle;

/// low-level retrieveal of raw binary information for a given token
// - returns the number of bytes retrieved into buf.buf
// - caller should then run buf.Done to release the buf result memory
function RawTokenGetInfo(tok: THandle; tic: TTokenInformationClass;
  var buf: TSynTempBuffer): cardinal;

/// return the SID of a given token, nil if none found
// - the returned PSid is located within buf temporary buffer
// - so caller should call buf.Done once this PSid value is not needed any more
function RawTokenSid(tok: THandle; var buf: TSynTempBuffer): PSid;

/// return the group SIDs of a given token, nil if none found
// - the returned PSid is located within buf temporary buffer
// - so caller should call buf.Done once this PSid value is not needed any more
function RawTokenGroups(tok: THandle; var buf: TSynTempBuffer): PSids;

/// return the group SIDs of a given token as text dynamic array
function TokenGroupsText(tok: THandle): TRawUtf8DynArray;

/// check if a group SID is part of a given token
function TokenHasGroup(tok: THandle; sid: PSid): boolean;

/// check if any group SID is part of a given token
function TokenHasAnyGroup(tok: THandle; const sid: RawSidDynArray): boolean;

/// return the SID of the current user, from process or thread, as text
// - e.g. 'S-1-5-21-823746769-1624905683-418753922-1000'
// - optionally returning the name and domain via LookupSid()
function CurrentSid(wtt: TWinTokenType = wttProcess;
  name: PRawUtf8 = nil; domain: PRawUtf8 = nil): RawUtf8; overload;

/// return the SID of the current user, from process or thread, as raw binary
procedure CurrentRawSid(out sid: RawSid; wtt: TWinTokenType = wttProcess;
  name: PRawUtf8 = nil; domain: PRawUtf8 = nil); overload;

/// return the SID of the current user groups, from process or thread, as text
function CurrentGroupsSid(wtt: TWinTokenType = wttProcess): TRawUtf8DynArray;

/// recognize the well-known SIDs from the current user, from process or thread
// - for instance, for an user with administrator rights on Windows, returns
// $ [wksWorld, wksLocal, wksConsoleLogon, wksIntegrityHigh, wksInteractive,
// $  wksAuthenticatedUser, wksThisOrganisation, wksBuiltinAdministrators,
// $  wksBuiltinUsers, wksNtlmAuthentication]
function CurrentKnownGroups(wtt: TWinTokenType = wttProcess): TWellKnownSids;

/// fast check if the current user, from process or thread, has a well-known group SID
// - e.g. CurrentUserHasGroup(wksLocalSystem) returns true for LOCAL_SYSTEM user
function CurrentUserHasGroup(wks: TWellKnownSid;
  wtt: TWinTokenType = wttProcess): boolean; overload;

/// fast check if the current user, from process or thread, has a given group SID
function CurrentUserHasGroup(const sid: RawUtf8;
  wtt: TWinTokenType = wttProcess): boolean; overload;

/// fast check if the current user, from process or thread, has a given group SID
function CurrentUserHasGroup(sid: PSid;
  wtt: TWinTokenType = wttProcess): boolean; overload;

/// fast check if the current user, from process or thread, has any given group SID
function CurrentUserHasAnyGroup(const sid: RawSidDynArray;
  wtt: TWinTokenType = wttProcess): boolean;

/// fast check if the current user, from process or thread, match a group by name
// - calls LookupSid() on each group SID of this user, and filter with name/domain
function CurrentUserHasGroup(const name, domain, server: RawUtf8;
  wtt: TWinTokenType = wttProcess): boolean; overload;

/// just a wrapper around CurrentUserHasGroup(wksBuiltinAdministrators)
function CurrentUserIsAdmin: boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// rough detection of 'c:\windows' and 'c:\program files' folders
function IsSystemFolder(const Folder: TFileName): boolean;

// check if a folder may be affected by UAC folder virtualization
// - on Win32 Vista+, detects 'c:\windows' and 'c:\program files' UAC folders
// - returns always false on Win64
function IsUacVirtualFolder(const Folder: TFileName): boolean;
  {$ifdef CPU64} inline; {$endif}

/// check if UAC folder/registry virtualization is enabled for this process
// - returns always false on Win64 - by design
// - calls GetTokenInformation(TokenVirtualizationEnabled) on Win32
// - if you include {$R src\mormot.win.default.manifest.res} in your project,
// UAC virtualization is disabled and this function returns false
function IsUacVirtualizationEnabled: boolean;
  {$ifdef CPU64} inline; {$endif}

/// retrieve the name and domain of a given SID
// - returns stUndefined if the SID could not be resolved by LookupAccountSid()
function LookupSid(sid: PSid; out name, domain: RawUtf8;
  const server: RawUtf8 = ''): TSidType; overload;

/// retrieve the name and domain of a given SID, encoded from text
// - returns stUndefined if the SID could not be resolved by LookupAccountSid()
function LookupSid(const sid: RawUtf8; out name, domain: RawUtf8;
  const server: RawUtf8 = ''): TSidType; overload;

/// retrieve the name and domain of a given Token
function LookupToken(tok: THandle; out name, domain: RawUtf8;
  const server: RawUtf8 = ''): boolean; overload;

/// retrieve the 'domain\name' combined value of a given Token
function LookupToken(tok: THandle; const server: RawUtf8 = ''): RawUtf8; overload;

/// retrieve low-level process information, from the Windows API
procedure GetProcessInfo(aPid: cardinal; out aInfo: TWinProcessInfo); overload;

/// retrieve low-level process(es) information, from the Windows API
procedure GetProcessInfo(const aPidList: TCardinalDynArray;
  out aInfo: TWinProcessInfoDynArray); overload;

/// quickly retrieve a Text value from Registry
// - could be used if TWinRegistry is not needed, e.g. for a single value
function ReadRegString(Key: THandle; const Path, Value: string): string;

/// convenient late-binding of any external library function
// - just wrapper around LoadLibray + GetProcAddress once over a pointer
function DelayedProc(var api; var lib: THandle;
  libname: PChar; procname: PAnsiChar): boolean;

type
  HCRYPTPROV = pointer;
  HCRYPTKEY = pointer;
  HCRYPTHASH = pointer;
  HCERTSTORE = pointer;

  CRYPTOAPI_BLOB = record
    cbData: DWORD;
    pbData: PByteArray;
  end;
  CRYPT_INTEGER_BLOB = CRYPTOAPI_BLOB;
  CERT_NAME_BLOB     = CRYPTOAPI_BLOB;
  CRYPT_OBJID_BLOB   = CRYPTOAPI_BLOB;

  CRYPT_BIT_BLOB = record
    cbData: DWORD;
    pbData: PByteArray;
    cUnusedBits: DWORD;
  end;

  CRYPT_ALGORITHM_IDENTIFIER = record
    pszObjId: PAnsiChar;
    Parameters: CRYPT_OBJID_BLOB;
  end;

  CERT_PUBLIC_KEY_INFO = record
    Algorithm: CRYPT_ALGORITHM_IDENTIFIER;
    PublicKey: CRYPT_BIT_BLOB;
  end;

  CERT_EXTENSION = record
    pszObjId: PAnsiChar;
    fCritical: BOOL;
    Blob: CRYPT_OBJID_BLOB;
  end;
  PCERT_EXTENSION = ^CERT_EXTENSION;
  CERT_EXTENSIONS = array[word] of CERT_EXTENSION;
  PCERT_EXTENSIONS = ^CERT_EXTENSIONS;

  CERT_INFO = record
    dwVersion: DWORD;
    SerialNumber: CRYPT_INTEGER_BLOB;
    SignatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Issuer: CERT_NAME_BLOB;
    NotBefore: TFileTime;
    NotAfter: TFileTime;
    Subject: CERT_NAME_BLOB;
    SubjectPublicKeyInfo: CERT_PUBLIC_KEY_INFO;
    IssuerUniqueId: CRYPT_BIT_BLOB;
    SubjectUniqueId: CRYPT_BIT_BLOB;
    cExtension: DWORD;
    rgExtension: PCERT_EXTENSIONS;
  end;
  PCERT_INFO = ^CERT_INFO;

  CERT_CONTEXT = record
    dwCertEncodingType: DWORD;
    pbCertEncoded: PByte;
    cbCertEncoded: DWORD;
    pCertInfo: PCERT_INFO;
    hCertStore: HCERTSTORE;
  end;
  PCCERT_CONTEXT = ^CERT_CONTEXT;
  PPCCERT_CONTEXT = ^PCCERT_CONTEXT;

  CRYPT_KEY_PROV_PARAM = record
    dwParam: DWORD;
    pbData: PByte;
    cbData: DWORD;
    dwFlags: DWORD;
  end;
  PCRYPT_KEY_PROV_PARAM = ^CRYPT_KEY_PROV_PARAM;

  CRYPT_KEY_PROV_INFO = record
    pwszContainerName: PWideChar;
    pwszProvName: PWideChar;
    dwProvType: DWORD;
    dwFlags: DWORD;
    cProvParam: DWORD;
    rgProvParam: PCRYPT_KEY_PROV_PARAM;
    dwKeySpec: DWORD;
  end;
  PCRYPT_KEY_PROV_INFO = ^CRYPT_KEY_PROV_INFO;

  CRYPT_OID_INFO = record
    cbSize: DWORD;
    pszOID: PAnsiChar;
    pwszName: PWideChar;
    dwGroupId: DWORD;
    Union: record
      case integer of
        0: (dwValue: DWORD);
        1: (Algid: DWORD);
        2: (dwLength: DWORD);
    end;
    ExtraInfo: CRYPTOAPI_BLOB;
  end;
  PCRYPT_OID_INFO = ^CRYPT_OID_INFO;

  PCCRL_CONTEXT = pointer;
  PPCCRL_CONTEXT = ^PCCRL_CONTEXT;
  PCRYPT_ATTRIBUTE = pointer;

  CRYPT_SIGN_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgEncodingType: DWORD;
    pSigningCert: PCCERT_CONTEXT;
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvHashAuxInfo: Pointer;
    cMsgCert: DWORD;
    rgpMsgCert: PPCCERT_CONTEXT;
    cMsgCrl: DWORD;
    rgpMsgCrl: PPCCRL_CONTEXT;
    cAuthAttr: DWORD;
    rgAuthAttr: PCRYPT_ATTRIBUTE;
    cUnauthAttr: DWORD;
    rgUnauthAttr: PCRYPT_ATTRIBUTE;
    dwFlags: DWORD;
    dwInnerContentType: DWORD;
    HashEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvHashEncryptionAuxInfo: Pointer;
  end;

  PFN_CRYPT_GET_SIGNER_CERTIFICATE = function(pvGetArg: Pointer;
    dwCertEncodingType: DWORD; pSignerId: PCERT_INFO;
    hMsgCertStore: HCERTSTORE): PCCERT_CONTEXT; stdcall;
  CRYPT_VERIFY_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgAndCertEncodingType: DWORD;
    hCryptProv: HCRYPTPROV;
    pfnGetSignerCertificate: PFN_CRYPT_GET_SIGNER_CERTIFICATE;
    pvGetArg: Pointer;
  end;

  /// direct access to the Windows CryptoApi
  {$ifdef USERECORDWITHMETHODS}
  TWinCryptoApi = record
  {$else}
  TWinCryptoApi = object
  {$endif USERECORDWITHMETHODS}
  private
    /// if the presence of this API has been tested
    Tested: boolean;
    /// if this API has been loaded
    Handle: THandle;
    /// used when inlining Available method
    procedure Resolve;
  public
    /// acquire a handle to a particular key container within a
    // particular cryptographic service provider (CSP)
    AcquireContextA: function(var phProv: HCRYPTPROV; pszContainer: PAnsiChar;
      pszProvider: PAnsiChar; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall;
    /// releases the handle of a cryptographic service provider (CSP) and a
    // key container
    ReleaseContext: function(hProv: HCRYPTPROV; dwFlags: PtrUInt): BOOL; stdcall;
    /// transfers a cryptographic key from a key BLOB into a cryptographic
    // service provider (CSP)
    ImportKey: function(hProv: HCRYPTPROV; pbData: pointer; dwDataLen: DWORD;
      hPubKey: HCRYPTKEY; dwFlags: DWORD; var phKey: HCRYPTKEY): BOOL; stdcall;
    /// customizes various aspects of a session key's operations
    SetKeyParam: function(hKey: HCRYPTKEY; dwParam: DWORD; pbData: pointer;
      dwFlags: DWORD): BOOL; stdcall;
    /// releases the handle referenced by the hKey parameter
    DestroyKey: function(hKey: HCRYPTKEY): BOOL; stdcall;
    /// encrypt the data designated by the key held by the CSP module
    // referenced by the hKey parameter
    Encrypt: function(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL;
      dwFlags: DWORD; pbData: pointer; var pdwDataLen: DWORD; dwBufLen: DWORD): BOOL; stdcall;
    /// decrypts data previously encrypted by using the CryptEncrypt function
    Decrypt: function(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL;
      dwFlags: DWORD; pbData: pointer; var pdwDataLen: DWORD): BOOL; stdcall;
    /// fills a buffer with cryptographically random bytes
    // - since Windows Vista with Service Pack 1 (SP1), an AES counter-mode
    // based PRNG specified in NIST Special Publication 800-90 is used
    GenRandom: function(hProv: HCRYPTPROV; dwLen: DWORD; pbBuffer: Pointer): BOOL; stdcall;
    /// sign a message (not resolved yet - in crypt32.dll)
    SignMessage: function(var pSignPara: CRYPT_SIGN_MESSAGE_PARA;
      fDetachedSignature: BOOL; cToBeSigned: DWORD; rgpbToBeSigned: pointer;
      var rgcbToBeSigned: DWORD; pbSignedBlob: pointer; var pcbSignedBlob: DWORD): BOOL; stdcall;
    /// verify a signed message (not resolved yet - in crypt32.dll)
    VerifyMessageSignature: function(var pVerifyPara: CRYPT_VERIFY_MESSAGE_PARA;
      dwSignerIndex: DWORD; pbSignedBlob: PByte; cbSignedBlob: DWORD;
      pbDecoded: PByte; pcbDecoded: LPDWORD; ppSignerCert: PPCCERT_CONTEXT): BOOL; stdcall;
    /// try to load the CryptoApi on this system
    function Available: boolean;
      {$ifdef HASINLINE}inline;{$endif}
  end;

const
  NO_ERROR  = Windows.NO_ERROR;

  ERROR_ACCESS_DENIED      = Windows.ERROR_ACCESS_DENIED;
  ERROR_INVALID_PARAMETER  = Windows.ERROR_INVALID_PARAMETER;
  ERROR_HANDLE_EOF         = Windows.ERROR_HANDLE_EOF;
  ERROR_ALREADY_EXISTS     = Windows.ERROR_ALREADY_EXISTS;
  ERROR_MORE_DATA          = Windows.ERROR_MORE_DATA;
  ERROR_CONNECTION_INVALID = Windows.ERROR_CONNECTION_INVALID;
  ERROR_OLD_WIN_VERSION    = Windows.ERROR_OLD_WIN_VERSION;
  ERROR_IO_PENDING         = Windows.ERROR_IO_PENDING;
  ERROR_OPERATION_ABORTED  = Windows.ERROR_OPERATION_ABORTED;
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa383770
  ERROR_WINHTTP_TIMEOUT                 = 12002;
  ERROR_WINHTTP_CANNOT_CONNECT          = 12029;
  ERROR_WINHTTP_INVALID_SERVER_RESPONSE = 12152;
  ERROR_MUI_FILE_NOT_FOUND              = 15100;

  INVALID_HANDLE_VALUE = Windows.INVALID_HANDLE_VALUE; // = HANDLE(-1)
  ENGLISH_LANGID       = $0409;

  PROV_RSA_FULL        = 1;
  PROV_RSA_AES         = 24;
  CRYPT_NEWKEYSET      = 8;
  CRYPT_VERIFYCONTEXT  = DWORD($F0000000);
  PLAINTEXTKEYBLOB     = 8;
  CUR_BLOB_VERSION     = 2;
  KP_IV                = 1;
  KP_MODE              = 4;
  CALG_AES_128         = $660E;
  CALG_AES_192         = $660F;
  CALG_AES_256         = $6610;
  CRYPT_MODE_CBC       = 1;
  CRYPT_MODE_ECB       = 2;
  CRYPT_MODE_OFB       = 3;
  CRYPT_MODE_CFB       = 4;
  CRYPT_MODE_CTS       = 5;
  HCRYPTPROV_NOTTESTED = HCRYPTPROV(-1);
  NTE_BAD_KEYSET       = HRESULT($80090016);

var
  CryptoApi: TWinCryptoApi;

/// protect some data for the current user, using Windows DPAPI
// - the application can specify a secret salt text, which should reflect the
// current execution context, to ensure nobody could decrypt the data without
// knowing this application-specific AppSecret value
// - will use CryptProtectData DPAPI function call under Windows
// - see https://msdn.microsoft.com/en-us/library/ms995355
// - this function is Windows-only, could be slow, and you don't know which
// algorithm is really used on your system, so using our mormot.crypt.core.pas
// CryptDataForCurrentUser() is probably a safer (and cross-platform) alternative
// - also note that DPAPI has been closely reverse engineered - see e.g.
// https://www.passcape.com/index.php?section=docsys&cmd=details&id=28
function CryptDataForCurrentUserDPAPI(const Data, AppSecret: RawByteString;
  Encrypt: boolean): RawByteString;

const
  WINDOWS_CERTSTORE: array[TSystemCertificateStore] of PWideChar = (
    'CA', 'MY', 'ROOT', 'SPC');

/// this global procedure should be called from each thread needing to use OLE
// - it is called e.g. by TOleDBConnection.Create when an OleDb connection
// is instantiated for a new thread
// - every call of CoInit shall be followed by a call to CoUninit
// - implementation will maintain some global counting, to call the CoInitialize
// API only once per thread
// - only made public for user convenience, e.g. when using custom COM objects
procedure CoInit;

/// this global procedure should be called at thread termination
// - it is called e.g. by TOleDBConnection.Destroy, when thread associated
// to an OleDb connection is terminated
// - every call of CoInit shall be followed by a call to CoUninit
// - only made public for user convenience, e.g. when using custom COM objects
procedure CoUninit;

/// retrieves the current executable module handle, i.e.  its memory load address
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function GetModuleHandle(lpModuleName: PChar): HMODULE;

/// post a message to the Windows message queue
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function PostMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;

/// retrieves the current stack trace
// - only available since Windows XP
// - FramesToSkip + FramesToCapture should be <= 62
function RtlCaptureStackBackTrace(FramesToSkip, FramesToCapture: cardinal;
  BackTrace, BackTraceHash: pointer): byte; stdcall;

/// retrieves the current thread ID
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function GetCurrentThreadId: DWORD; stdcall;

/// retrieves the current process ID
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function GetCurrentProcessId: DWORD; stdcall;

/// retrieves the current process ID
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function GetCurrentProcess: THandle; stdcall;

/// redefined in mormot.core.os to avoid dependency to the Windows unit
function WaitForSingleObject(hHandle: THandle; dwMilliseconds: DWORD): DWORD; stdcall;

/// redefined in mormot.core.os to avoid dependency to the Windows unit
function GetEnvironmentStringsW: PWideChar; stdcall;

/// redefined in mormot.core.os to avoid dependency to the Windows unit
function FreeEnvironmentStringsW(EnvBlock: PWideChar): BOOL; stdcall;

/// expand any embedded environment variables, i.e %windir%
function ExpandEnvVars(const aStr: string): string;

/// try to enter a Critical Section (Lock)
// - returns 1 if the lock was acquired, or 0 if the mutex is already locked
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - under Delphi/Windows, directly call the homonymous Win32 API
function TryEnterCriticalSection(var cs: TRTLCriticalSection): integer; stdcall;

/// enter a Critical Section (Lock)
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - under Delphi/Windows, directly call the homonymous Win32 API
procedure EnterCriticalSection(var cs: TRTLCriticalSection); stdcall;

/// leave a Critical Section (UnLock)
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - under Delphi/Windows, directly call the homonymous Win32 API
procedure LeaveCriticalSection(var cs: TRTLCriticalSection); stdcall;

/// initialize Windows IOCP instance
// - renamed in mormot.core.os to avoid dependency to the Windows unit
function IocpCreate(FileHandle, ExistingCompletionPort: THandle;
  CompletionKey: pointer; NumberOfConcurrentThreads: DWORD): THandle; stdcall;

/// retrieve Windows IOCP instance status
// - renamed in mormot.core.os to avoid dependency to the Windows unit
function IocpGetQueuedStatus(CompletionPort: THandle;
  var lpNumberOfBytesTransferred: DWORD; var lpCompletionKey: pointer;
  var lpOverlapped: pointer; dwMilliseconds: DWORD): BOOL; stdcall;

/// trigger a Windows IOCP instance
// - renamed in mormot.core.os to avoid dependency to the Windows unit
function IocpPostQueuedStatus(CompletionPort: THandle;
  NumberOfBytesTransferred: DWORD; dwCompletionKey: pointer;
  lpOverlapped: POverlapped): BOOL; stdcall;

/// finalize a Windows resource (e.g. IOCP instance)
// - redefined in mormot.core.os to avoid dependency to the Windows unit
function CloseHandle(hObject: THandle): BOOL; stdcall;

/// redefined here to avoid warning to include "Windows" in uses clause
// - why did Delphi define this slow RTL function as inlined in SysUtils.pas?
// - also supports aFileName longer than MAX_PATH
// - on Windows, aRights parameter is just ignored, and on POSIX aRights = 0
// will set the default octal 644 file access attributes (-rw-r-r--)
// - warning: this function replaces ALL SysUtils.FileCreate() overloads,
// putting aMode as the SECOND parameter, just like with FileOpen()
function FileCreate(const aFileName: TFileName; aMode: integer = 0;
  aRights: integer = 0): THandle;

/// redefined here to call CreateFileW() on non-Unicode RTL and support
// aFileName longer than MAX_PATH
function FileOpen(const aFileName: TFileName; aMode: integer): THandle;

/// redefined here to avoid warning to include "Windows" in uses clause
// - why did Delphi define this slow RTL function as inlined in SysUtils.pas?
procedure FileClose(F: THandle); stdcall;

/// redefined here to support FileName longer than MAX_PATH
// - as our FileOpen/FileCreate redefinitions
// - CheckAsDir = true is used by DirectoryExists()
function FileExists(const FileName: TFileName; FollowLink: boolean = true;
  CheckAsDir: boolean = false): boolean;

/// redefined here to support FileName longer than MAX_PATH
function DirectoryExists(const FileName: TFileName;
  FollowLink: boolean = true): boolean; {$ifdef HASINLINE} inline; {$endif}

/// redefined here to avoid warning to include "Windows" in uses clause
// and support FileName longer than MAX_PATH
// - why did Delphi define this slow RTL function as inlined in SysUtils.pas?
function DeleteFile(const aFileName: TFileName): boolean;

/// redefined here to avoid warning to include "Windows" in uses clause
// and support FileName longer than MAX_PATH
// - why did Delphi define this slow RTL function as inlined in SysUtils.pas?
function RenameFile(const OldName, NewName: TFileName): boolean;

/// redirection to Windows SetFileTime() of a file name from Int64(TFileTime)
// - if any Int64 is 0, the proper value will be guess from the non-0 values
function FileSetTime(const FileName: TFileName;
  const Created, Accessed, Written: Int64): boolean;

{$else}

/// faster cross-platform alternative to sysutils homonymous function
// - will directly use fpstat() so is slightly faster than default FPC RTL
function FileExists(const FileName: TFileName): boolean;

/// redefined from FPC RTL sysutils for consistency
// - warning: this function replaces ALL SysUtils.FileCreate() overloads,
// putting aMode as the SECOND parameter, just like with FileOpen()
// - on POSIX, aRights = 0 will set default octal 644 attributes (-rw-r-r--)
function FileCreate(const aFileName: TFileName; aMode: integer = 0;
  aRights: integer = 0): THandle;

/// returns how many files could be opened at once on this POSIX system
// - hard=true is for the maximum allowed limit, false for the current process
// - returns -1 if the getrlimit() API call failed
function GetFileOpenLimit(hard: boolean = false): integer;

/// changes how many files could be opened at once on this POSIX system
// - hard=true is for the maximum allowed limit (requires root priviledges),
// false for the current process
// - returns the new value set (may not match the expected max value on error)
// - returns -1 if the getrlimit().setrlimit() API calls failed
// - for instance, to set the limit of the current process to its highest value:
// ! SetFileOpenLimit(GetFileOpenLimit(true));
function SetFileOpenLimit(max: integer; hard: boolean = false): integer;

/// read /proc/pid/status to ensure pid is of a real process, not a thread
function IsValidPid(pid: cardinal): boolean;

type
  /// Low-level access to the ICU library installed on this system
  // - "International Components for Unicode" (ICU) is an open-source set of
  // libraries for Unicode support, internationalization and globalization
  // - used by Unicode_CompareString, Unicode_AnsiToWide, Unicode_WideToAnsi,
  // Unicode_InPlaceUpper and Unicode_InPlaceLower function from this unit
  TIcuLibrary = packed object
  protected
    icu, icudata, icui18n: pointer;
    Loaded: boolean;
    procedure DoLoad(const LibName: TFileName = ''; Version: string = '');
    procedure Done;
  public
    /// Initialize an ICU text converter for a given encoding
    ucnv_open: function (converterName: PAnsiChar; var err: SizeInt): pointer; cdecl;
    /// finalize the ICU text converter for a given encoding
    ucnv_close: procedure (converter: pointer); cdecl;
    /// customize the ICU text converter substitute char
    ucnv_setSubstChars: procedure (converter: pointer;
      subChars: PAnsiChar; len: byte; var err: SizeInt); cdecl;
    /// enable the ICU text converter fallback
    ucnv_setFallback: procedure (cnv: pointer; usesFallback: LongBool); cdecl;
    /// ICU text conversion from UTF-16 to a given encoding
    ucnv_fromUChars: function (cnv: pointer; dest: PAnsiChar; destCapacity: cardinal;
      src: PWideChar; srcLength: cardinal; var err: SizeInt): cardinal; cdecl;
    /// ICU text conversion from a given encoding to UTF-16
    ucnv_toUChars: function (cnv: pointer; dest: PWideChar; destCapacity: cardinal;
      src: PAnsiChar; srcLength: cardinal; var err: SizeInt): cardinal; cdecl;
    /// ICU UTF-16 text conversion to uppercase
    u_strToUpper: function (dest: PWideChar; destCapacity: cardinal;
      src: PWideChar; srcLength: cardinal; locale: PAnsiChar;
      var err: SizeInt): cardinal; cdecl;
    /// ICU UTF-16 text conversion to lowercase
    u_strToLower: function (dest: PWideChar; destCapacity: cardinal;
      src: PWideChar; srcLength: cardinal; locale: PAnsiChar;
      var err: SizeInt): cardinal; cdecl;
    /// ICU UTF-16 text comparison
    u_strCompare: function (s1: PWideChar; length1: cardinal;
      s2: PWideChar; length2: cardinal; codePointOrder: LongBool): cardinal; cdecl;
    /// ICU UTF-16 text comparison with options, e.g. for case-insensitivity
    u_strCaseCompare: function (s1: PWideChar; length1: cardinal;
      s2: PWideChar; length2: cardinal; options: cardinal;
      var err: SizeInt): cardinal; cdecl;
    /// get the ICU data folder
    u_getDataDirectory: function: PAnsiChar; cdecl;
    /// set the ICU data folder
    u_setDataDirectory: procedure(directory: PAnsiChar); cdecl;
    /// initialize the ICU library
    u_init: procedure(var status: SizeInt); cdecl;
    /// try to initialize a specific version of the ICU library
    // - first finalize any existing loaded instance
    // - returns true if was successfully loaded and setup
    function ForceLoad(const LibName: TFileName; const Version: string): boolean;
    /// returns TRUE if a ICU library is available on this system
    // - will thread-safely load and initialize it if necessary
    function IsAvailable: boolean; inline;
    /// Initialize an ICU text converter for a given codepage
    // - returns nil if ICU is not available on this system
    // - wrapper around ucnv_open/ucnv_setSubstChars/ucnv_setFallback calls
    // - caller should make ucnv_close() once done with the returned instance
    function ucnv(codepage: cardinal): pointer;
  end;

var
  /// low-level late-binding access to any installed ICU library
  // - typical use is to check icu.IsAvailable then the proper icu.*() functions
  // - this unit will make icu.Done in its finalization section
  icu: TIcuLibrary;

  /// contains the current POSIX kernel revision, as one 24-bit integer
  // - allow quick comparison mainly for kernel feature checking
  // - e.g. on Linux, may equal $030d02 for 3.13.2, or $020620 for 2.6.32
  KernelRevision: cardinal;


{$ifdef OSLINUX} { some Linux-specific APIs (e.g. systemd or eventfd) }

const
  /// The first passed file descriptor is fd 3
  SD_LISTEN_FDS_START = 3;

  /// low-level libcurl library file name, depending on the running OS
  LIBSYSTEMD_PATH = 'libsystemd.so.0';

  ENV_INVOCATION_ID: PAnsiChar = 'INVOCATION_ID';

type
  /// low-level systemd parameter to sd.journal_sendv() function
  TIoVec = record
    iov_base: PAnsiChar;
    iov_len: PtrUInt;
  end;

  /// implements late-binding of the systemd library
  // - about systemd: see https://www.freedesktop.org/wiki/Software/systemd
  // and http://0pointer.de/blog/projects/socket-activation.html - to get headers
  // on debian: `sudo apt install libsystemd-dev && cd /usr/include/systemd`
  TSystemD = record
  private
    systemd: pointer;
    tested: boolean;
    procedure DoLoad;
  public
    /// returns how many file descriptors have been passed to process
    // - if result=1 then socket for accepting connection is LISTEN_FDS_START
    listen_fds: function(unset_environment: integer): integer; cdecl;
    /// returns 1 if the file descriptor is an AF_UNIX socket of the specified type and path
    is_socket_unix: function(fd, typr, listening: integer;
      var path: TFileName; pathLength: PtrUInt): integer; cdecl;
    /// systemd: submit simple, plain text log entries to the system journal
    // - priority value can be obtained using integer(LOG_TO_SYSLOG[logLevel])
    journal_print: function(priority: integer; args: array of const): integer; cdecl;
    /// systemd: submit array of iov structures instead of the format string to the system journal.
    // - each structure should reference one field of the entry to submit
    // - the second argument specifies the number of structures in the array
    journal_sendv: function(var iov: TIoVec; n: integer): integer; cdecl;
    /// sends notification to systemd
    // - see https://www.freedesktop.org/software/systemd/man/notify.html
    // status notification sample: sd.notify(0, 'READY=1');
    // watchdog notification: sd.notify(0, 'WATCHDOG=1');
    notify: function(unset_environment: integer; state: PUtf8Char): integer; cdecl;
    /// check whether the service manager expects watchdog keep-alive
    // notifications from a service
    // - if result > 0 then usec contains the notification interval (app should
    // notify every usec/2)
    watchdog_enabled: function(unset_environment: integer; usec: Puint64): integer; cdecl;
    /// returns true in case the current process was started by systemd
    // - For systemd v232+
    function ProcessIsStartedBySystemd: boolean;
    /// returns TRUE if a systemd library is available
    // - will thread-safely load and initialize it if necessary
    function IsAvailable: boolean; inline;
    /// release the systemd library
    procedure Done;
  end;

var
  /// low-level late-binding of the systemd library
  // - typical use is to check sd.IsAvailable then the proper sd.*() functions
  // - this unit will make sd.Done in its finalization section
  sd: TSystemD;

/// a wrapper to the eventfd() syscall
// - returns 0 if the kernel does not support eventfd2 (before 2.6.27) or
// if the platform is not supported (only validated on Linux x86_64 by now)
// - returns a file descriptor handle on success, which should be fpclose()
function LinuxEventFD(nonblocking, semaphore: boolean): integer;

/// wrapper to read from a eventfd() file
// - return 1 and decrement the counter by 1 in semaphore mode
// - return the current counter value and set it to 0 in non-semaphor mode
// - may be blocking or not blocking, depending on how LinuxEventFD() was called
// - return -1 on error
function LinuxEventFDRead(fd: integer): Int64;

/// wrapper to write to a eventfd() file
procedure LinuxEventFDWrite(fd: integer; count: QWord);

/// wrapper to wait for a eventfd() file read
// - return true if was notified for reading, or false on timeout
function LinuxEventFDWait(fd: integer; ms: integer): boolean; inline;

{$endif OSLINUX}

var
  /// allow runtime-binding of complex OS API calls
  // - used e.g. by mormot.core.os.mac.pas to inject its own methods
  PosixInject: record
    GetSmbios: function(info: TSmbiosBasicInfo): RawUtf8;
    GetSmbiosData: function: RawByteString;
  end;

{$endif OSWINDOWS}


{ ****************** Unicode, Time, File, Console, Library process }

{$ifdef OSWINDOWS}

type
  /// redefined as our own mormot.core.os type to avoid dependency to Windows
  // - warning: do not use this type directly, but rather TSynSystemTime as
  // defined in mormot.core.datetime which is really cross-platform, and has
  // consistent field order (FPC POSIX/Windows fields do not match!)
  TSystemTime = Windows.TSystemTime;
  PSystemTime = Windows.PSystemTime;

  /// system-specific type returned by FileAge(): local 32-bit bitmask on Windows
  TFileAge = integer;

{$ifdef ISDELPHI}

  /// redefined as our own mormot.core.os type to avoid dependency to Windows
  TRTLCriticalSection = Windows.TRTLCriticalSection;

  /// defined as in FPC RTL, to avoid dependency to Windows.pas unit
  // - note that on POSIX, a THandle is a 32-bit integer, but library or
  // resource handles are likely to map pointers, i.e. up to a 64-bit integer
  TLibHandle = THandle;

{$endif ISDELPHI}

  /// handle for Slim Reader/Writer (SRW) locks in exclusive mode
  TOSLightMutex = pointer;

/// a wrapper around FileTimeToLocalFileTime/FileTimeToSystemTime Windows APIs
// - only used by mormot.lib.static for proper SQlite3 linking on Windows
procedure UnixTimeToLocalTime(I64: TUnixTime; out Local: TSystemTime);

/// convert an Unix seconds time to a Win32 64-bit FILETIME value
procedure UnixTimeToFileTime(I64: TUnixTime; out FT: TFileTime);

/// convert an Unix milliseconds time to a Win32 64-bit FILETIME value
procedure UnixMSTimeToFileTime(I64: TUnixMSTime; out FT: TFileTime);

/// convert a TDateTime to a Win32 64-bit FILETIME value
procedure DateTimeToFileTime(dt: TDateTime; out FT: TFileTime);

/// convert a Win32 64-bit FILETIME value into an Unix seconds time
function FileTimeToUnixTime(const FT: TFileTime): TUnixTime;
  {$ifdef FPC} inline; {$endif}

/// convert a Win32 64-bit FILETIME value into a TDateTime
function FileTimeToDateTime(const FT: TFileTime): TDateTime;

/// convert a Win32 64-bit FILETIME value into an Unix milliseconds time
function FileTimeToUnixMSTime(const FT: TFileTime): TUnixMSTime;
  {$ifdef FPC} inline; {$endif}

var
  // Slim Reader/Writer (SRW) API exclusive mode - fallback to TLightLock on XP
  InitializeSRWLock,
  AcquireSRWLockExclusive,
  ReleaseSRWLockExclusive: procedure(var P: TOSLightMutex); stdcall;
  TryAcquireSRWLockExclusive: function (var P: TOSLightMutex): BOOL; stdcall;

{$else}

const
  /// a cross-platform incorrect THandle value, as defined in Windows unit
  INVALID_HANDLE_VALUE = THandle(-1);

  /// allow to assign proper signed symbol table name for a libc.so.6 method
  {$ifdef OSLINUXX64}
  LIBC_SUFFIX = '@GLIBC_2.2.5';
  {$else}
  {$ifdef OSLINUXX86}
  LIBC_SUFFIX = '@GLIBC_2.0';
  {$else}
  LIBC_SUFFIX = ''; // no suffix seems needed outside of Intel/AMD systems
  {$endif OSLINUXX86}
  {$endif OSLINUXX64}

type
  /// system-specific type returned by FileAge(): UTC 64-bit Epoch on POSIX
  TFileAge = TUnixTime;

  /// system-specific structure holding a non-recursive mutex
  TOSLightMutex = TRTLCriticalSection;

{$ifdef OSLINUX}
  {$define OSPTHREADSLIB}    // direct pthread calls were tested on Linux only
{$endif OSLINUX}
{$ifdef OSDARWIN}
  {$define OSPTHREADSSTATIC} // direct pthread calls from the 'c' library
{$endif OSDARWIN}
{$ifdef OSBSD}
  {$define OSPTHREADSSTATIC} // direct pthread calls from the c library
{$endif OSBSD}

// some pthread_mutex_*() API defined here for proper inlining
{$ifdef OSPTHREADSLIB}
var
  {%H-}pthread: pointer; // access to pthread.so e.g. for mormot.lib.static
  pthread_mutex_lock:    function(mutex: pointer): integer; cdecl;
  pthread_mutex_trylock: function(mutex: pointer): integer; cdecl;
  pthread_mutex_unlock:  function(mutex: pointer): integer; cdecl;
{$endif OSPTHREADSLIB}
{$ifdef OSPTHREADSSTATIC}
function pthread_mutex_lock(mutex: pointer): integer; cdecl;
function pthread_mutex_trylock(mutex: pointer): integer; cdecl;
function pthread_mutex_unlock(mutex: pointer): integer; cdecl;
{$endif OSPTHREADSSTATIC}

{$endif OSWINDOWS}

/// raw cross-platform library loading function
// - alternative to LoadLibrary() and SafeLoadLibrary() Windows API and RTL
// - on Windows, set the SEM_NOOPENFILEERRORBOX and SEM_FAILCRITICALERRORS flags
// to avoid unexpected message boxes (which should not happen e.g. on a service)
// - on Win32, reset the FPU flags after load as required with some libraries
// - consider inheriting TSynLibrary if you want to map a set of API functions
function LibraryOpen(const LibraryName: TFileName): TLibHandle;

/// raw cross-platform library unloading function
// - alternative to FreeLibrary() Windows API and FPC RTL
procedure LibraryClose(Lib: TLibHandle);

/// raw cross-platform library resolution function, as defined in FPC RTL
// - alternative to GetProcAddr() Windows API and FPC RTL
function LibraryResolve(Lib: TLibHandle; ProcName: PAnsiChar): pointer;
  {$ifdef OSWINDOWS} stdcall; {$endif}

/// raw cross-platform library resolution error, e.g. after LibraryOpen
function LibraryError: string;


const
  /// redefined here to avoid dependency to the Windows or SyncObjs units
  INFINITE = cardinal(-1);

/// initialize a Critical Section (for Lock/UnLock)
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - under Delphi/Windows, directly call the homonymous Win32 API
procedure InitializeCriticalSection(var cs : TRTLCriticalSection);
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// finalize a Critical Section (for Lock/UnLock)
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - under Delphi/Windows, directly call the homonymous Win32 API
procedure DeleteCriticalSection(var cs : TRTLCriticalSection);
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

{$ifdef OSPOSIX}

{$ifndef OSLINUX} // try to stabilize MacOS/BSD pthreads API calls
  {$define NODIRECTTHREADMANAGER}
{$endif OSLINUX}

{$ifdef NODIRECTTHREADMANAGER} // try to stabilize MacOS pthreads API calls
function GetCurrentThreadId: TThreadID; inline;
function TryEnterCriticalSection(var cs: TRTLCriticalSection): integer; inline;
procedure EnterCriticalSection(var cs: TRTLCriticalSection); inline;
procedure LeaveCriticalSection(var cs: TRTLCriticalSection); inline;
{$else}

/// returns the unique ID of the current running thread
// - defined in mormot.core.os for inlined FpcCurrentThreadManager call
var GetCurrentThreadId: function: TThreadID;

/// enter a Critical Section (Lock)
// - defined in mormot.core.os for inlined FpcCurrentThreadManager call
var EnterCriticalSection: procedure(var cs: TRTLCriticalSection);

/// leave a Critical Section (UnLock)
// - defined in mormot.core.os for inlined FpcCurrentThreadManager call
var LeaveCriticalSection: procedure(var cs: TRTLCriticalSection);

/// try to acquire and lock a Critical Section (Lock)
// - returns 1 if the lock was acquired, or 0 if the mutex is already locked
// - defined in mormot.core.os for inlined FpcCurrentThreadManager call
var TryEnterCriticalSection: function(var cs: TRTLCriticalSection): integer;

{$endif NODIRECTTHREADMANAGER}

{$endif OSPOSIX}

/// returns TRUE if the supplied mutex has been initialized
// - will check if the supplied mutex is void (i.e. all filled with 0 bytes)
function IsInitializedCriticalSection(var cs: TRTLCriticalSection): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// on need initialization of a mutex, then enter the lock
// - if the supplied mutex has been initialized, do nothing
// - if the supplied mutex is void (i.e. all filled with 0), initialize it
procedure InitializeCriticalSectionIfNeededAndEnter(var cs: TRTLCriticalSection);
  {$ifdef HASINLINEWINAPI}inline;{$endif}

/// on need finalization of a mutex
// - if the supplied mutex has been initialized, delete it
// - if the supplied mutex is void (i.e. all filled with 0), do nothing
procedure DeleteCriticalSectionIfNeeded(var cs: TRTLCriticalSection);

/// returns the current UTC time as TSystemTime from the OS
// - under Delphi/Windows, directly call the homonymous Win32 API
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - under Linux/POSIX, calls clock_gettime(CLOCK_REALTIME_COARSE) if available
// or fpgettimeofday() on Darwin/MacOS
// - warning: do not call this function directly, but rather mormot.core.datetime
// TSynSystemTime.FromNowUtc cross-platform method instead
procedure GetSystemTime(out result: TSystemTime);
  {$ifdef OSWINDOWS} stdcall; {$endif}

/// set the current system time as UTC timestamp
// - we define two functions with diverse signature to circumvent the FPC RTL
// TSystemTime field order inconsistency
// - warning: do not call this function directly, but rather mormot.core.datetime
// TSynSystemTime.ChangeOperatingSystemTime cross-platform method instead
{$ifdef OSWINDOWS}
function SetSystemTime(const utctime: TSystemTime): boolean;
{$else}
function SetSystemTime(utctime: TUnixTime): boolean;
{$endif OSWINDOWS}

/// returns the current Local time as TSystemTime from the OS
// - under Delphi/Windows, directly call the homonymous Win32 API
// - redefined in mormot.core.os to avoid dependency to the Windows unit
// - under Linux/POSIX, calls clock_gettime(CLOCK_REALTIME_COARSE) if available
// or fpgettimeofday() on Darwin/MacOS, with FPC RTL TZSeconds adjustment (so
// will be fixed for the whole process lifetime and won't change at daylight)
// - warning: do not call this function directly, but rather mormot.core.datetime
// TSynSystemTime.FromNowLocal cross-platform method instead
procedure GetLocalTime(out result: TSystemTime);
  {$ifdef OSWINDOWS} stdcall; {$endif}

/// compatibility function, wrapping Win32 API file truncate at current position
procedure SetEndOfFile(F: THandle);
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// compatibility function, wrapping Win32 API file flush to disk
procedure FlushFileBuffers(F: THandle);
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// compatibility function, wrapping Win32 API last error code
function GetLastError: integer;
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// check if the last error reporting by the system is a file access violation
// - call GetLastError is no ErrorCode is supplied
function IsSharedViolation(ErrorCode: integer = 0): boolean;

/// compatibility function, wrapping Win32 API last error code
procedure SetLastError(error: integer);
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// returns a given error code as plain text
// - redirects to WinErrorText(error, nil) on Windows, or StrError() on POSIX
function GetErrorText(error: integer): RawUtf8;
  {$ifdef HASINLINE} inline; {$endif}

{$ifdef OSWINDOWS}

/// return the error message of a given Module
// - first try WinErrorConstant() for system error constants (if ModuleName=nil),
// then call FormatMessage() and override the RTL function to force the
// ENGLISH_LANGID flag first
// - if ModuleName does support this Code, will try it as system error
// - replace SysErrorMessagePerModule() and SysErrorMessage() from mORMot 1
function WinErrorText(Code: cardinal; ModuleName: PChar): RawUtf8;

/// return the best known ERROR_* system error message constant texts
// - without the 'ERROR_' prefix
// - as used by WinErrorText()
function WinErrorConstant(Code: cardinal): PUtf8Char;

/// raise an EOSException from the last system error using WinErrorText()
procedure RaiseLastError(const Context: shortstring;
  RaisedException: ExceptClass = nil);

/// raise an Exception from the last module error using WinErrorText()
procedure RaiseLastModuleError(ModuleName: PChar; ModuleException: ExceptClass);

{$endif OSWINDOWS}

/// compatibility function, wrapping Win32 API function
// - returns the current main Window handle on Windows, or 0 on POSIX/Linux
function GetDesktopWindow: PtrInt;
  {$ifdef OSWINDOWS} stdcall; {$else} inline; {$endif}

/// returns the curent system code page for AnsiString types
// - as used to initialize CurrentAnsiConvert in mormot.core.unicode unit
// - calls GetACP() Win32 API value on Delphi, or DefaultSystemCodePage on FPC -
// i.e. GetSystemCodePage() on POSIX (likely to be UTF-8) or the value used
// by the LCL for its "string" types (also typically UTF-8 even on Windows)
function Unicode_CodePage: integer;
  {$ifdef FPC} inline; {$endif}

/// compatibility function, wrapping CompareStringW() Win32 API text comparison
// - returns 1 if PW1>PW2, 2 if PW1=PW2, 3 if PW1<PW2 - so substract 2 to have
// -1,0,1 as regular StrCompW/StrICompW comparison function result
// - will compute StrLen(PW1/PW2) if L1 or L2 < 0
// - on POSIX, use the ICU library, or fallback to FPC RTL widestringmanager
// with a temporary variable - you would need to include cwstring unit
// - in practice, is seldom called, unless our proprietary WIN32CASE collation
// is used in mormot.db.raw.sqlite3
// - consider Utf8ILCompReference() from mormot.core.unicode.pas for an
// operating-system-independent Unicode 10.0 comparison function
function Unicode_CompareString(
  PW1, PW2: PWideChar; L1, L2: PtrInt; IgnoreCase: boolean): integer;

/// compatibility function, wrapping MultiByteToWideChar() Win32 API call
// - returns the number of WideChar written into W^ destination buffer
// - on POSIX, use the ICU library, or fallback to FPC RTL widestringmanager
// with a temporary variable - you would need to include cwstring unit
// - raw function called by TSynAnsiConvert.AnsiBufferToUnicode from
// mormot.core.unicode unit
function Unicode_AnsiToWide(
  A: PAnsiChar; W: PWideChar; LA, LW, CodePage: PtrInt): integer;

/// compatibility function, wrapping WideCharToMultiByte() Win32 API call
// - returns the number of AnsiChar written into A^ destination buffer
// - on POSIX, use the ICU library, or fallback to FPC RTL widestringmanager
// with a temporary variable - you would need to include cwstring unit
// - raw function called by TSynAnsiConvert.UnicodeBufferToAnsi from
// mormot.core.unicode unit
function Unicode_WideToAnsi(
  W: PWideChar; A: PAnsiChar; LW, LA, CodePage: PtrInt): integer;

/// conversion of some UTF-16 buffer into a temporary Ansi ShortString
// - used when mormot.core.unicode is an overkill, e.g. TCrtSocket.SockSend()
procedure Unicode_WideToShort(
  W: PWideChar; LW, CodePage: PtrInt; var res: ShortString);

/// compatibility function, wrapping Win32 API CharUpperBuffW()
// - on POSIX, use the ICU library, or fallback to 'a'..'z' conversion only
// - raw function called by UpperCaseUnicode() from mormot.core.unicode unit
function Unicode_InPlaceUpper(W: PWideChar; WLen: integer): integer;
  {$ifdef OSWINDOWS} stdcall; {$endif}

/// compatibility function, wrapping Win32 API CharLowerBuffW()
// - on POSIX, use the ICU library, or fallback to 'A'..'Z' conversion only
// - raw function called by LowerCaseUnicode() from mormot.core.unicode unit
function Unicode_InPlaceLower(W: PWideChar; WLen: integer): integer;
  {$ifdef OSWINDOWS} stdcall; {$endif}

/// returns a system-wide current monotonic timestamp as milliseconds
// - will use the corresponding native API function under Vista+, or will be
// redirected to a custom wrapper function for older Windows versions (XP)
// to avoid the 32-bit overflow/wrapping issue of GetTickCount
// - warning: FPC's SysUtils.GetTickCount64 or TThread.GetTickCount64 don't
// handle properly 49 days wrapping under XP -> always use this safe version
// - warning: FPC's SysUtils.GetTickCount64 may call fpgettimeofday() e.g.
// on Darwin, which is not monotonic -> always use this more coherent version
// - on POSIX, will call (via vDSO) the very fast CLOCK_MONOTONIC_COARSE if
// available, or the low-level mach_absolute_time() monotonic Darwin API
// - do not expect exact millisecond resolution - steps may rather be e.g.
// within the 15-16 ms range on Windows, and 4-5 ms range on Linux
{$ifdef OSWINDOWS}
var
  GetTickCount64: function: Int64; stdcall;
{$else}
function GetTickCount64: Int64;
{$endif OSWINDOWS}

/// returns how many seconds the system was up, accouting for time when
// the computer is asleep
// - on Windows, computes GetTickCount64 div 1000
// - on Linux/BSD, will use CLOCK_BOOTTIME/CLOCK_UPTIME clock
// - on MacOS, will use mach_continuous_time() API
function GetUptimeSec: cardinal;

/// returns the current UTC time
// - wrap UnixMSTimeUtcFast, so use e.g. clock_gettime(CLOCK_REALTIME_COARSE)
// under Linux, or GetSystemTimeAsFileTime under Windows
function NowUtc: TDateTime;

/// returns the current UTC date/time as a second-based c-encoded time
// - i.e. current number of seconds elapsed since Unix epoch 1/1/1970
// - use e.g. fast clock_gettime(CLOCK_REALTIME_COARSE) under Linux,
// or GetSystemTimeAsFileTime under Windows
// - returns a 64-bit unsigned value, so is "Year2038bug" free
function UnixTimeUtc: TUnixTime;

/// returns the current UTC date/time as a millisecond-based c-encoded time
// - i.e. current number of milliseconds elapsed since Unix epoch 1/1/1970
// - will use e.g. fast clock_gettime(CLOCK_REALTIME_COARSE) under Linux,
// or GetSystemTimePreciseAsFileTime under Windows 8 and later
// - on Windows, is slightly more accurate, but slower than UnixMSTimeUtcFast
function UnixMSTimeUtc: TUnixMSTime;

/// returns the current UTC date/time as a millisecond-based c-encoded time
// - under Linux/POSIX, is the very same than UnixMSTimeUtc (inlined call)
// - under Windows 8+, will call GetSystemTimeAsFileTime instead of
// GetSystemTimePreciseAsFileTime, which has higher precision, but is slower
// - prefer it under Windows, if a dozen of ms resolution is enough for your task
function UnixMSTimeUtcFast: TUnixMSTime;
  {$ifdef OSPOSIX} inline; {$endif}

const
  /// number of days offset between the Unix Epoch (1970) and TDateTime origin
  UnixDelta = 25569;
  /// number of Windows TFileTime ticks (100ns) from year 1601 to 1970
  UnixFileTimeDelta = 116444736000000000;

/// the number of minutes bias in respect to UTC/GMT date/time
// - as retrieved via -GetLocalTimeOffset() at startup, so may not be accurate
// after a time shift during the process execution - but any long-running
// process (like a service) should use UTC timestamps only
var
  TimeZoneLocalBias: integer;

{$ifndef NOEXCEPTIONINTERCEPT}

type
  /// calling context when intercepting exceptions
  // - used e.g. for TSynLogExceptionToStr or RawExceptionIntercept() handlers
  {$ifdef USERECORDWITHMETHODS}
  TSynLogExceptionContext = record
  {$else}
  TSynLogExceptionContext = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the raised exception class
    EClass: ExceptClass;
    /// the Delphi Exception instance
    // - may be nil for external/OS exceptions
    EInstance: Exception;
    /// the OS-level exception code
    // - could be $0EEDFAE0 of $0EEDFADE for Delphi-generated exceptions
    ECode: DWord;
    /// = FPC's RaiseProc() FrameCount if EStack is Frame: PCodePointer
    EStackCount: integer;
    /// the address where the exception occurred
    EAddr: PtrUInt;
    /// the optional stack trace
    EStack: PPtrUIntArray;
    /// timestamp of this exception, as number of seconds since UNIX Epoch
    // - UnixTimeUtc is faster than NowUtc or GetSystemTime
    // - use UnixTimeToDateTime() to convert it into a regular TDateTime
    ETimestamp: TUnixTime;
    /// the logging level corresponding to this exception
    // - may be either sllException or sllExceptionOS
    ELevel: TSynLogLevel;
    /// retrieve some extended information about a given Exception
    // - on Windows, recognize most DotNet CLR Exception Names
    function AdditionalInfo(out ExceptionNames: TPUtf8CharDynArray): cardinal;
  end;

  /// the global function signature expected by RawExceptionIntercept()
  // - assigned e.g. to SynLogException() in mormot.core.log.pas
  TOnRawLogException = procedure(const Ctxt: TSynLogExceptionContext);

/// setup Exception interception for the whole process
// - call RawExceptionIntercept(nil) to disable custom exception handling
procedure RawExceptionIntercept(const Handler: TOnRawLogException);

{$endif NOEXCEPTIONINTERCEPT}

/// returns a high-resolution system-wide monotonic timestamp as microseconds
// - under Linux/POSIX, has true microseconds resolution, calling e.g.
// CLOCK_MONOTONIC on Linux/BSD
// - under Windows, calls QueryPerformanceCounter / QueryPerformanceFrequency
procedure QueryPerformanceMicroSeconds(out Value: Int64);

/// cross-platform check if the supplied THandle is not invalid
function ValidHandle(Handle: THandle): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check for unsafe '..' '/xxx' 'c:xxx' '~/xxx' or '\\' patterns in a path
function SafePathName(const Path: TFileName): boolean;

/// check for unsafe '..' '/xxx' 'c:xxx' '~/xxx' or '\\' patterns in a RawUtf8 path
function SafePathNameU(const Path: RawUtf8): boolean;

/// check for unsafe '..' '/xxx' 'c:xxx' '~/xxx' or '\\' patterns in a filename
function SafeFileName(const FileName: TFileName): boolean;

/// check for unsafe '..' '/xxx' 'c:xxx' '~/xxx' or '\\' patterns in a RawUtf8 filename
function SafeFileNameU(const FileName: RawUtf8): boolean;

/// ensure all \ / path delimiters are normalized into the current OS expectation
// - i.e. normalize file name to use '\' on Windows, or '/' on POSIX
// - see MakePath() from mormot.core.text.pas to concatenate path items
function NormalizeFileName(const FileName: TFileName): TFileName;

/// add some " before and after if FileName has some space within
// - could be used when generating command line parameters
function QuoteFileName(const FileName: TFileName): TFileName;

/// faster cross-platform alternative to sysutils homonymous function
// - on Windows, just redirect to WindowsFileTimeToDateTime() since FileDate
// is already expected to be in local time from FileAge()
// - on POSIX, FileDate is a 64-bit UTC value as returned from OS stat API, and
// will be converted into a local TDateTime
// - note: FPC FileAge(TDateTime) is wrong and truncates 1-2 seconds on Windows
function FileDateToDateTime(const FileDate: TFileAge): TDateTime;
  {$ifdef HASINLINE}{$ifdef OSWINDOWS}inline;{$endif}{$endif}

/// get a file date and time, from its name
// - returns 0 if file doesn't exist
// - returns the local file age, encoded as TDateTime
// - under Windows, will use GetFileAttributesEx fast API
function FileAgeToDateTime(const FileName: TFileName): TDateTime;

/// get a file date and time, from its name, as seconds since Unix Epoch
// - returns 0 if file (or folder if AllowDir is true) doesn't exist
// - returns the system API file age (not converted local), encoded as TUnixTime
// - under Windows, will use GetFileAttributesEx and FileTimeToUnixTime
// - under POSIX, will call directly the stat syscall
// - faster than FileAgeToDateTime() since don't convert to local time
function FileAgeToUnixTimeUtc(const FileName: TFileName;
  AllowDir: boolean = false): TUnixTime;

/// get the date and time of one file into a Windows File 32-bit TimeStamp
// - this cross-system function is used e.g. by mormot.core.zip which expects
// Windows TimeStamps in its headers
function FileAgeToWindowsTime(const FileName: TFileName): integer;

/// copy the date of one file to another
// - FileSetDate(THandle, Age) is not implemented on POSIX: filename is needed
function FileSetDateFrom(const Dest: TFileName; SourceHandle: THandle): boolean; overload;

/// copy the date of one file to another
// - FileSetDate(THandle, Age) is not implemented on POSIX: filename is needed
function FileSetDateFrom(const Dest, Source: TFileName): boolean; overload;

/// copy the date of one file from a Windows File 32-bit TimeStamp
// - this cross-system function is used e.g. by mormot.core.zip which expects
// Windows TimeStamps in its headers
// - FileSetDate(THandle, Age) is not implemented on POSIX: filename is needed
function FileSetDateFromWindowsTime(const Dest: TFileName; WinTime: integer): boolean;

/// set the file date/time from a supplied UTC TUnixTime value
// - avoid any temporary conversion to local time
// - Time may come from FileAgeToUnixTimeUtc()
function FileSetDateFromUnixUtc(const Dest: TFileName; Time: TUnixTime): boolean;

/// convert a Windows API File 32-bit TimeStamp into a regular TDateTime
// - returns 0 if the conversion failed
// - used e.g. by FileSetDateFromWindowsTime() on POSIX
function WindowsFileTimeToDateTime(WinTime: integer): TDateTime;

/// convert a Windows API File 64-bit TimeStamp into a regular TUnixMSTime
// - i.e. a FILETIME value as returned by GetFileTime() Win32 API
// - some binary formats (e.g. ISO 9660 or LDAP) have such FILETIME fields
function WindowsFileTime64ToUnixMSTime(WinTime: QWord): TUnixMSTime;

/// low-level conversion of a TDateTime into a Windows File 32-bit TimeStamp
// - returns 0 if the conversion failed
function DateTimeToWindowsFileTime(DateTime: TDateTime): integer;

/// check if a file exists and can be written
// - on POSIX, call fpaccess() and check for the W_OK attribute
// - on Windows, supports aFileName longer than MAX_PATH
function FileIsWritable(const FileName: TFileName): boolean;

/// reduce the visibility of a given file, and set its read/write attributes
// - on POSIX, change attributes for the the owner, and reset group/world flags
// so that it is accessible by the current user only; under POSIX, there is
// no "hidden" file attribute, but you should define a FileName starting by '.'
// - on Windows, will set the "hidden" file attribue
procedure FileSetHidden(const FileName: TFileName; ReadOnly: boolean);

/// set the "sticky bit" on a file or directory
// - on POSIX, a "sticky" folder will ensure that its nested files will be
// deleted by their owner; and a "sticky" file will ensure e.g. that no
// /var/tmp file is deleted by systemd during its clean up phases
// - on Windows, will set the Hidden and System file attributes
procedure FileSetSticky(const FileName: TFileName);

/// get a file size, from its name
// - returns 0 if file doesn't exist, or is a directory
// - under Windows, will use GetFileAttributesEx fast API
// - on POSIX, will use efficient fpStat() single call but not FileOpen/FileClose
function FileSize(const FileName: TFileName): Int64; overload;

/// get a file size, from its handle
// - returns 0 if file doesn't exist
// - under Windows, will use the GetFileSizeEx fast API
// - on POSIX, will use efficient FpFStat() single call and no file seek
function FileSize(F: THandle): Int64; overload;

/// FileSeek() overloaded function, working with huge files
// - Delphi FileSeek() is buggy -> use this function to safely access files
// bigger than 2 GB (thanks to sanyin for the report)
function FileSeek64(Handle: THandle; const Offset: Int64;
  Origin: cardinal = soFromBeginning): Int64;

/// get a file size and its UTC Unix timestamp in milliseconds resolution
// - return false if FileName was not found
// - return true and set FileSize and FileTimestampUtc if found - note that
// no local time conversion is done, so timestamp won't match FileAge()
// - use a single Operating System call, so is faster than FileSize + FileAge
function FileInfoByName(const FileName: TFileName; out FileSize: Int64;
  out FileTimestampUtc: TUnixMSTime): boolean;

/// get low-level file information, in a cross-platform way
// - returns true on success
// - you can specify nil for any returned value if you don't need
// - here file write/creation time are given as TUnixMSTime values, for better
// cross-platform process - note that FileCreateDateTime may not be supported
// by most Linux file systems, so the oldest timestamp available is returned
// as failover on such systems (probably the latest file metadata writing)
function FileInfoByHandle(aFileHandle: THandle; FileId, FileSize: PInt64;
  LastWriteAccess, FileCreateDateTime: PUnixMSTime): boolean;

/// check if a given file is likely to be an executable
// - will check the DOS/WinPE executable header in its first bytes on Windows
// - will call fpStat() on POSIX to check the File and Executable bits
function FileIsExecutable(const FileName: TFileName): boolean;

/// compute the size of a directory's files, optionally with nested folders
// - basic implementation using FindFirst/FindNext so won't be the fastest
// available, nor fully accurate when files are actually (hard) links
function DirectorySize(const FileName: TFileName; Recursive: boolean = false;
  const Mask: TFileName = FILES_ALL): Int64;

/// copy one file to another, similar to the Windows API
function CopyFile(const Source, Target: TFileName;
  FailIfExists: boolean): boolean;

/// prompt the user for an error message to notify an unexpected issue
// - in practice, text encoding is expected to be plain 7-bit ASCII
// - on Windows, will use Writeln() on a (newly allocated if needed) console
// - on POSIX, will use Writeln(StdErr)
procedure DisplayFatalError(const title, msg: RawUtf8);

/// prompt the user for an error message to notify an unexpected issue
// - redirect to DisplayFatalError() without any title
// - expects the regular Format() layout with %s %d - not the FormatUtf8() %
procedure DisplayError(const fmt: string; const args: array of const);

/// get a file date and time, from a FindFirst/FindNext search
// - the returned timestamp is in local time, not UTC
// - this method would use the F.Timestamp field available since Delphi XE2
function SearchRecToDateTime(const F: TSearchRec): TDateTime;
  {$ifdef HASINLINE}inline;{$endif}

/// get a file UTC date and time, from a FindFirst/FindNext search
// - SearchRecToDateTime(), SearchRecToWindowsTime() and F.TimeStamp, which have
// local time and require a conversion, may appear less useful on server side
// - is implemented as a wrapper around SearchRecToUnixTimeUtc()
function SearchRecToDateTimeUtc(const F: TSearchRec): TDateTime;

/// get a file UTC date and time, from a FindFirst/FindNext search, as Unix time
// - SearchRecToDateTime(), SearchRecToWindowsTime() and F.TimeStamp, which have
// local time and require a conversion, may appear less useful on server side
function SearchRecToUnixTimeUtc(const F: TSearchRec): TUnixTime;
  {$ifdef OSPOSIX}inline;{$endif}

/// get a file date and time, from a FindFirst/FindNext search, as Windows time
// - this cross-system function is used e.g. by mormot.core.zip which expects
// Windows TimeStamps in its headers
function SearchRecToWindowsTime(const F: TSearchRec): integer;

/// check if a FindFirst/FindNext found instance is actually a file
function SearchRecValidFile(const F: TSearchRec): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a FindFirst/FindNext found instance is actually a folder
function SearchRecValidFolder(const F: TSearchRec): boolean;
  {$ifdef HASINLINE}inline;{$endif}

type
  /// FPC TFileStream miss a Create(aHandle) constructor like Delphi
  TFileStreamFromHandle = class(THandleStream)
  protected
    fDontReleaseHandle: boolean;
  public
    /// explictely close the handle if needed
    destructor Destroy; override;
    /// Destroy calls FileClose(Handle) unless this property is true
    property DontReleaseHandle: boolean
      read fDontReleaseHandle write fDontReleaseHandle;
  end;

  /// a TFileStream replacement which supports FileName longer than MAX_PATH,
  // and a proper Create(aHandle) constructor in FPC
  TFileStreamEx = class(TFileStreamFromHandle)
  protected
    fFileName : TFileName;
    function GetSize: Int64; override; // faster (1 API call instead of 3)
  public
    /// open or create the file from its name, depending on the supplied Mode
    // - Mode is typically fmCreate / fmOpenReadShared
    constructor Create(const aFileName: TFileName; Mode: cardinal);
    /// can use this class from a low-level file OS handle
    constructor CreateFromHandle(const aFileName: TFileName; aHandle: THandle);
    /// open for writing or create a non-existing file from its name
    // - use fmCreate if aFileName does not exists, or fmOpenWrite otherwise
    constructor CreateWrite(const aFileName: TFileName);
    /// the file name assigned to this class constructor
    property FileName : TFileName
      read fFilename;
  end;

  /// file stream which ignores I/O write errors
  // - in case disk space is exhausted, TFileStreamNoWriteError.WriteBuffer
  // won't throw any exception, so application will continue to work
  // - used e.g. by TSynLog to let the application continue with no exception,
  // even in case of a disk/partition full of logs
  TFileStreamNoWriteError = class(TFileStreamEx)
  public
    /// open for writing, potentially with alternate unlocked file names
    // - use fmCreate if aFileName does not exists, or fmOpenWrite otherwise
    // - on error, will try up to aAliases alternate '<filename>-locked<#>.<ext>'
    constructor CreateAndRenameIfLocked(
      var aFileName: TFileName; aAliases: integer = 3);
    /// this overriden function returns Count, as if it was always successful
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

/// a wrapper around FileRead() to ensure a whole memory buffer is retrieved
// - expects Size to be up to 2GB (seems like a big enough memory buffer)
// - on Windows, will read by 16MB chunks to avoid ERROR_NO_SYSTEM_RESOURCES
// - will call FileRead() and retry up to Size bytes are filled in the buffer
// - return true if all memory buffer has been read, or false on error
function FileReadAll(F: THandle; Buffer: pointer; Size: PtrInt): boolean;

/// a wrapper around FileWrite() to ensure a whole memory buffer is retrieved
// - will call FileWrite() and retry up to Size bytes are written from the buffer
// - return true if all memory buffer has been written, or false on error
function FileWriteAll(F: THandle; Buffer: pointer; Size: PtrInt): boolean;

/// overloaded function optimized for one pass reading of a (huge) file
// - will use e.g. the FILE_FLAG_SEQUENTIAL_SCAN flag under Windows, as stated
// by http://blogs.msdn.com/b/oldnewthing/archive/2012/01/20/10258690.aspx
// - on POSIX, calls fpOpen(pointer(FileName),O_RDONLY) with no fpFlock() call
// - is used e.g. by StringFromFile() or HashFile() functions
// - note: you could better use FileReadAll() to retrieve a whole data buffer
function FileOpenSequentialRead(const FileName: TFileName): integer;

/// returns a TFileStreamFromHandle optimized for one pass file reading
// - will use FileOpenSequentialRead(), i.e. FILE_FLAG_SEQUENTIAL_SCAN on Windows
// - on POSIX, calls fpOpen(pointer(FileName),O_RDONLY) with no fpFlock() call
// - is used e.g. by TRestOrmServerFullMemory and TAlgoCompress
function FileStreamSequentialRead(const FileName: TFileName): THandleStream;

/// try to open the file from its name, as fmOpenReadShared
// - on Windows, calls CreateFileW(aFileName,GENERIC_READ) then CloseHandle
// - on POSIX, calls fpOpen(pointer(aFileName),O_RDONLY) with no fpFlock() call
function FileIsReadable(const aFileName: TFileName): boolean;

/// copy all Source content into Dest from current position
// - on Delphi, Dest.CopyFrom(Source, 0) uses GetSize and ReadBuffer which is
// not compatible e.g. with TAesPkcs7Reader padding - and has a small buffer
// - returns the number of bytes copied from Source to Dest
function StreamCopyUntilEnd(Source, Dest: TStream): Int64;

/// read a File content into a string
// - content can be binary or text
// - returns '' if file was not found or any read error occurred
// - wil use GetFileSize() API by default, unless HasNoSize is defined,
// and read will be done using a buffer (required e.g. for POSIX char files)
// - uses RawByteString for byte storage, whatever the codepage is
function StringFromFile(const FileName: TFileName;
  HasNoSize: boolean = false): RawByteString;

/// read a File content from a list of potential files
// - returns '' if no file was found, or the first matching FileName[] content
function StringFromFirstFile(const FileName: array of TFileName): RawByteString;

/// read all Files content from a list of file names
// - returns '' if no FileName[] file was found, or the read content
function StringFromFiles(const FileName: array of TFileName): TRawByteStringDynArray;

/// read all Files content from a list of folders names
// - returns the content of every file contained in the supplied Folders[]
// - with optionally the FileNames[] corresponding to each result[] content
function StringFromFolders(const Folders: array of TFileName;
  const Mask: TFileName = FILES_ALL;
  FileNames: PFileNameDynArray = nil): TRawByteStringDynArray;

/// create a File from a string content
// - uses RawByteString for byte storage, whatever the codepage is
// - can optionaly force flush all write buffers to disk
function FileFromString(const Content: RawByteString; const FileName: TFileName;
  FlushOnDisk: boolean = false): boolean;

/// create a File from a memory buffer content
function FileFromBuffer(Buf: pointer; Len: PtrInt; const FileName: TFileName): boolean;

/// create or append a string content to a File
// - can optionally rotate the file to a FileName+'.bak'  over a specific size
function AppendToFile(const Content: RawUtf8; const FileName: TFileName;
  BackupOverMaxSize: Int64 = 0): boolean;

/// compute an unique temporary file name
// - following 'exename_123.tmp' pattern, in the system temporary folder
function TemporaryFileName: TFileName;

/// extract a path from a file name like ExtractFilePath function
// - but cross-platform, i.e. detect both '\' and '/' on all platforms
function ExtractPath(const FileName: TFileName): TFileName;

/// extract a path from a RawUtf8 file name like ExtractFilePath function
// - but cross-platform, i.e. detect both '\' and '/' on all platforms
function ExtractPathU(const FileName: RawUtf8): RawUtf8;

/// extract a name from a file name like ExtractFileName function
// - but cross-platform, i.e. detect both '\' and '/' on all platforms
function ExtractName(const FileName: TFileName): TFileName;

/// extract a name from a file name like ExtractFileName function
// - but cross-platform, i.e. detect both '\' and '/' on all platforms
function ExtractNameU(const FileName: RawUtf8): RawUtf8;

/// extract an extension from a file name like ExtractFileExt function
// - but cross-platform, i.e. detect both '\' and '/' on all platforms
function ExtractExt(const FileName: TFileName; WithoutDot: boolean = false): TFileName;

// defined here for proper ExtractExtP() inlining
function GetLastDelimU(const FileName: RawUtf8; OtherDelim: AnsiChar): PtrInt;

/// extract an extension from a file name like ExtractFileExt function
// - but cross-platform, i.e. detect both '\' and '/' on all platforms
function ExtractExtU(const FileName: RawUtf8; WithoutDot: boolean = false): RawUtf8;

/// extract an extension from a file name like ExtractFileExt function
// - but cross-platform, i.e. detect both '\' and '/' on all platforms
function ExtractExtP(const FileName: RawUtf8; WithoutDot: boolean = false): PUtf8Char;
  {$ifdef HASINLINE} inline; {$endif}

/// compute the file name, including its path if supplied, but without its extension
// - e.g. GetFileNameWithoutExt('/var/toto.ext') = '/var/toto'
// - may optionally return the extracted extension, as '.ext'
// - will be cross-platform, i.e. detect both '\' and '/' on all platforms
function GetFileNameWithoutExt(const FileName: TFileName;
  Extension: PFileName = nil): TFileName;

/// extract the file name without any path nor extension, as UTF-8
// - e.g. GetFileNameWithoutExt('/var/toto.ext') = 'toto'
// - used e.g. to compute Executable.ProgramName
function GetFileNameWithoutExtOrPath(const FileName: TFileName): RawUtf8;

/// compare two "array of TFileName" elements, grouped by file extension
// - i.e. with no case sensitivity on Windows
// - the expected string type is the RTL string, i.e. TFileName
// - calls internally GetFileNameWithoutExt() and AnsiCompareFileName()
function SortDynArrayFileName(const A, B): integer;

{$ifdef ISDELPHI20062007}
/// compatibility function defined to avoid hints on buggy Delphi 2006/2007
function AnsiCompareFileName(const S1, S2 : TFileName): integer;
{$endif ISDELPHI20062007}

/// creates a directory if not already existing
// - returns the full expanded directory name, including trailing path delimiter
// - returns '' on error, unless RaiseExceptionOnCreationFailure is set
function EnsureDirectoryExists(const Directory: TFileName;
  RaiseExceptionOnCreationFailure: ExceptionClass = nil): TFileName;

/// just a wrapper around EnsureDirectoryExists(NormalizeFileName(Directory))
function NormalizeDirectoryExists(const Directory: TFileName;
  RaiseExceptionOnCreationFailure: ExceptionClass = nil): TFileName;

/// delete the content of a specified directory
// - only one level of file is deleted within the folder: no recursive deletion
// is processed by this function (for safety)
// - if DeleteOnlyFilesNotDirectory is TRUE, it won't remove the folder itself,
// but just the files found in it
function DirectoryDelete(const Directory: TFileName;
  const Mask: TFileName = FILES_ALL; DeleteOnlyFilesNotDirectory: boolean = false;
  DeletedCount: PInteger = nil): boolean;

/// delete the files older than a given age in a specified directory
// - for instance, to delete all files older than one day:
// ! DirectoryDeleteOlderFiles(FolderName, 1);
// - only one level of file is deleted within the folder: no recursive deletion
// is processed by this function, unless Recursive is TRUE
// - if Recursive=true, caller should set TotalSize^=0 to have an accurate value
// - return false if any deprecated DeleteFile() did fail during the process
function DirectoryDeleteOlderFiles(const Directory: TFileName;
  TimePeriod: TDateTime; const Mask: TFileName = FILES_ALL;
  Recursive: boolean = false; TotalSize: PInt64 = nil): boolean;

type
  /// defines how IsDirectoryWritable() verifies a folder
  // - on Win32 Vista+, idwExcludeWinUac will check IsUacVirtualFolder()
  // - on Windows, idwExcludeWinSys will check IsSystemFolder()
  // - on Windows, idwTryWinExeFile will try to generate a 'xxxxx.exe' file
  // - idwWriteSomeContent will also try to write some bytes into the file
  TIsDirectoryWritable = set of (
    idwExcludeWinUac,
    idwExcludeWinSys,
    idwTryWinExeFile,
    idwWriteSomeContent);

/// check if the directory is writable for the current user
// - try to write and delete a void file with a random name in this folder
function IsDirectoryWritable(const Directory: TFileName;
  Flags: TIsDirectoryWritable = []): boolean;

type
  /// cross-platform memory mapping of a file content
  {$ifdef USERECORDWITHMETHODS}
  TMemoryMap = record
  {$else}
  TMemoryMap = object
  {$endif USERECORDWITHMETHODS}
  private
    fBuf: PAnsiChar;
    fBufSize: PtrUInt;
    fFile: THandle;
    {$ifdef OSWINDOWS}
    fMap: THandle;
    {$endif OSWINDOWS}
    fFileSize: Int64;
    fFileLocal, fLoadedNotMapped: boolean;
    function DoMap(aCustomOffset: Int64): boolean;
    procedure DoUnMap;
  public
    /// map the corresponding file handle
    // - if aCustomSize and aCustomOffset are specified, the corresponding
    // map view if created (by default, will map whole file)
    function Map(aFile: THandle; aCustomSize: PtrUInt = 0;
      aCustomOffset: Int64 = 0; aFileOwned: boolean = false;
      aFileSize: Int64 = -1): boolean; overload;
    /// map the file specified by its name
    // - file will be closed when UnMap will be called
    function Map(const aFileName: TFileName): boolean; overload;
    /// set a fixed buffer for the content
    // - emulates memory-mapping over an existing buffer
    procedure Map(aBuffer: pointer; aBufferSize: PtrUInt); overload;
    /// unmap the file
    procedure UnMap;
    /// retrieve the memory buffer mapped to the file content
    property Buffer: PAnsiChar
      read fBuf;
    /// retrieve the buffer size
    property Size: PtrUInt
      read fBufSize;
    /// retrieve the mapped file size
    property FileSize: Int64
      read fFileSize;
    /// access to the low-level associated File handle (if any)
    property FileHandle: THandle
      read fFile;
  end;

  /// a TStream created from a file content, using fast memory mapping
  TSynMemoryStreamMapped = class(TSynMemoryStream)
  protected
    fMap: TMemoryMap;
    fFileStream: THandleStream;
    fFileName: TFileName;
  public
    /// create a TStream from a file content using fast memory mapping
    // - if aCustomSize and aCustomOffset are specified, the corresponding
    // map view if created (by default, will map whole file)
    constructor Create(const aFileName: TFileName;
      aCustomSize: PtrUInt = 0; aCustomOffset: Int64 = 0); overload;
    /// create a TStream from a file content using fast memory mapping
    // - if aCustomSize and aCustomOffset are specified, the corresponding
    // map view if created (by default, will map whole file)
    constructor Create(aFile: THandle;
      aCustomSize: PtrUInt = 0; aCustomOffset: Int64 = 0); overload;
    /// release any internal mapped file instance
    destructor Destroy; override;
    /// the file name, if created from such Create(aFileName) constructor
    property FileName: TFileName
      read fFileName;
  end;

  /// low-level access to a resource bound to the executable
  // - so that Windows is not required in your unit uses clause
  {$ifdef USERECORDWITHMETHODS}
  TExecutableResource = record
  {$else}
  TExecutableResource = object
  {$endif USERECORDWITHMETHODS}
  private
    // note: we can't use THandle which is 32-bit on 64-bit POSIX
    HResInfo: TLibHandle;
    HGlobal: TLibHandle;
  public
    /// the resource memory pointer, after successful Open()
    Buffer: pointer;
    /// the resource memory size in bytes, after successful Open()
    Size: PtrInt;
    /// locate and lock a resource
    // - use the current executable if Instance is left to its 0 default value
    // - returns TRUE if the resource has been found, and Buffer/Size are set
    function Open(const ResourceName: string; ResType: PChar;
      Instance: TLibHandle = 0): boolean;
    /// unlock and finalize a resource
    procedure Close;
  end;


type
  /// store CPU and RAM usage for a given process
  // - as used by TSystemUse class
  TSystemUseData = packed record
    /// when the data has been sampled
    Timestamp: TDateTime;
    /// percent of current Kernel-space CPU usage for this process
    Kernel: single;
    /// percent of current User-space CPU usage for this process
    User: single;
    /// how many KB of working memory are used by this process
    WorkKB: cardinal;
    /// how many KB of virtual memory are used by this process
    VirtualKB: cardinal;
  end;

  /// store CPU and RAM usage history for a given process
  // - as returned by TSystemUse.History
  TSystemUseDataDynArray = array of TSystemUseData;

  /// low-level structure used to compute process memory and CPU usage
  {$ifdef USERECORDWITHMETHODS}
  TProcessInfo = record
  {$else}
  TProcessInfo = object
  {$endif USERECORDWITHMETHODS}
  private
    {$ifdef OSWINDOWS}
    fSysPrevIdle, fSysPrevKernel, fSysPrevUser,
    fDiffIdle, fDiffKernel, fDiffUser, fDiffTotal: Int64;
    {$endif OSWINDOWS}
  public
    /// initialize the system/process resource tracking
    function Init: boolean;
    /// to be called before PerSystem() or PerProcess() iteration
    function Start: boolean;
    /// percent of current Idle/Kernel/User CPU usage for all processes
    function PerSystem(out Idle, Kernel, User: single): boolean;
    /// retrieve CPU and RAM usage for a given process
    function PerProcess(PID: cardinal; Now: PDateTime;
      out Data: TSystemUseData; var PrevKernel, PrevUser: Int64): boolean;
  end;

  /// hold low-level information about current memory usage
  // - as filled by GetMemoryInfo()
  TMemoryInfo = record
    memtotal, memfree, filetotal, filefree,
    vmtotal, vmfree, allocreserved, allocused: QWord;
    percent: integer;
  end;

  /// stores information about a disk partition
  TDiskPartition = packed record
    /// the name of this partition
    // - is the Volume name under Windows, or the Device name under POSIX
    name: RawUtf8;
    /// where this partition has been mounted
    // - e.g. 'C:' or '/home'
    // - you can use GetDiskInfo(mounted) to retrieve current space information
    mounted: TFileName;
    /// total size (in bytes) of this partition
    size: QWord;
  end;

  /// stores information about several disk partitions
  TDiskPartitions = array of TDiskPartition;


{$ifdef CPUARM}
var
  /// internal wrapper address for ReserveExecutableMemory()
  // - set to @TInterfacedObjectFake.ArmFakeStub by mormot.core.interfaces.pas
  ArmFakeStubAddr: pointer;
{$endif CPUARM}


/// cross-platform reserve some executable memory
// - using PAGE_EXECUTE_READWRITE flags on Windows, and PROT_READ or PROT_WRITE
// or PROT_EXEC on POSIX
// - this function maintain an internal list of 64KB memory pages for efficiency
// - memory blocks can not be released (don't try to use fremeem on them) and
// will be returned to the system at process finalization
function ReserveExecutableMemory(size: cardinal): pointer;

/// to be called after ReserveExecutableMemory() when you want to actually write
// the memory blocks
// - affect the mapping flags of the first memory page (4KB) of the Reserved
// buffer, so its size should be < 4KB
// - do nothing on Windows and Linux, but may be needed on OpenBSD
procedure ReserveExecutableMemoryPageAccess(Reserved: pointer; Exec: boolean);

/// check if the supplied pointer is actually pointing to some memory page
// - will call slow but safe VirtualQuery API on Windows, or try a fpaccess()
// syscall on POSIX systems (validated on Linux only)
function SeemsRealPointer(p: pointer): boolean;

/// fill a buffer with a copy of some low-level system memory
// - used e.g. by GetRawSmbios on XP or Linux/POSIX
// - will allow to read up to 4MB of memory
// - use low-level ntdll.dll API on Windows, or reading /dev/mem on POSIX - so
// expect sudo/root rights on most systems
function ReadSystemMemory(address, size: PtrUInt): RawByteString;

/// return the PIDs of all running processes
// - under Windows, is a wrapper around EnumProcesses() PsAPI call
// - on Linux, will enumerate /proc/* pseudo-files
function EnumAllProcesses: TCardinalDynArray;

/// return the process name of a given process  ID
// - under Windows, is a wrapper around
// QueryFullProcessImageNameW/GetModuleFileNameEx PsAPI call
// - on Linux, will query /proc/[pid]/exe or /proc/[pid]/cmdline pseudo-file
function EnumProcessName(PID: cardinal): RawUtf8;

/// return the process ID of the parent of a given PID
// - by default (PID = 0), will search for the parent of the current process
// - returns 0 if the PID was not found
function GetParentProcess(PID: cardinal = 0): cardinal;

/// check if this process is currently running into the debugger
// - redirect to the homonymous WinAPI function on Windows, or check if the
// /proc/self/status "TracerPid:" value is non zero on Linux, or search if
// "lazarus" is part of the parent process name for BSD
{$ifdef OSWINDOWS}
function IsDebuggerPresent: BOOL; stdcall;
{$else}
function IsDebuggerPresent: boolean;
{$endif ODWINDOWS}

/// return the time and memory usage information about a given process
// - under Windows, is a wrapper around GetProcessTimes/GetProcessMemoryInfo
function RetrieveProcessInfo(PID: cardinal; out KernelTime, UserTime: Int64;
  out WorkKB, VirtualKB: cardinal): boolean;

/// return the system-wide time usage information
// - under Windows, is a wrapper around GetSystemTimes() kernel API call
// - return false on POSIX system - call RetrieveLoadAvg() instead
function RetrieveSystemTimes(out IdleTime, KernelTime, UserTime: Int64): boolean;

/// return the system-wide time usage information
// - on LINUX, retrieve /proc/loadavg or on OSX/BSD call libc getloadavg()
// - return '' on Windows - call RetrieveSystemTimes() instead
function RetrieveLoadAvg: RawUtf8;

/// retrieve low-level information about current memory usage
// - as used by TSynMonitorMemory
// - under BSD, only memtotal/memfree/percent are properly returned
// - allocreserved and allocused are set only if withalloc is TRUE
function GetMemoryInfo(out info: TMemoryInfo; withalloc: boolean): boolean;

/// retrieve some human-readable text from GetMemoryInfo
// - numbers are rounded up to a single GB number with no decimals
// - returns e.g. 'used 6GB/16GB (35% free)' text
function GetMemoryInfoText: RawUtf8;

/// retrieve some human-readable text about the current system in several lines
// - includes UTC timestamp, memory and disk availability, and exe/OS/CPU info
function GetSystemInfoText: RawUtf8;

/// retrieve low-level information about a given disk partition
// - as used by TSynMonitorDisk and GetDiskPartitionsText()
// - aDriveFolderOrFile is a directory on disk (no need to specify a raw drive
// name like 'c:\' on Windows)
// - warning: aDriveFolderOrFile may be modified at input
// - only under Windows the Quotas are applied separately to aAvailableBytes
// in respect to global aFreeBytes
function GetDiskInfo(var aDriveFolderOrFile: TFileName;
  out aAvailableBytes, aFreeBytes, aTotalBytes: QWord
  {$ifdef OSWINDOWS}; aVolumeName: PSynUnicode = nil{$endif}): boolean;

/// retrieve how many bytes are currently available on a given folder
// - returns 0 if the function fails
function GetDiskAvailable(aDriveFolderOrFile: TFileName): QWord;

/// retrieve low-level information about all mounted disk partitions of the system
// - returned partitions array is sorted by "mounted" ascending order
function GetDiskPartitions: TDiskPartitions;

/// call several Operating System APIs to gather 512-bit of entropy information
procedure XorOSEntropy(var e: THash512Rec);

/// low-level function returning some random binary from the Operating System
// - will call /dev/urandom or /dev/random under POSIX, and CryptGenRandom API
// on Windows then return TRUE, or fallback to mormot.core.base gsl_rng_taus2
// generator and return FALSE if the system API failed
// - on POSIX, only up to 32 bytes (256 bits) bits are retrieved from /dev/urandom
// or /dev/random as stated by "man urandom" Usage - then RandomBytes() padded
// - so you may consider that the output Buffer is always filled with random
// - you should not have to call this procedure, but faster and safer TAesPrng
// from mormot.crypt.core - also consider the TSystemPrng class
function FillSystemRandom(Buffer: PByteArray; Len: integer;
  AllowBlocking: boolean): boolean;

type
  /// available console colors
  TConsoleColor = (
    ccBlack,
    ccBlue,
    ccGreen,
    ccCyan,
    ccRed,
    ccMagenta,
    ccBrown,
    ccLightGray,
    ccDarkGray,
    ccLightBlue,
    ccLightGreen,
    ccLightCyan,
    ccLightRed,
    ccLightMagenta,
    ccYellow,
    ccWhite);

var
  /// low-level handle used for console writing
  // - may be overriden when console is redirected
  // - on Windows, is initialized when AllocConsole or TextColor() are called
  StdOut: THandle;

  {$ifdef OSPOSIX}
  /// set at initialization if StdOut has the TTY flag and env has a known TERM
  StdOutIsTTY: boolean;
  {$endif OSPOSIX}

  /// global flag to modify the code behavior at runtime when run from TSynTests
  // - e.g. TSynDaemon.AfterCreate won't overwrite TSynTests.RunAsConsole logs
  RunFromSynTests: boolean;

/// similar to Windows AllocConsole API call, to be truly cross-platform
// - do nothing on Linux/POSIX, but set StdOut propertly from StdOutputHandle
// - on Windows, will call the corresponding API, and set StdOut global variable
procedure AllocConsole;

/// change the console text writing color
procedure TextColor(Color: TConsoleColor);

/// change the console text background color
procedure TextBackground(Color: TConsoleColor);

/// write some text to the console using a given color
// - this method is protected by its own CriticalSection for output consistency
procedure ConsoleWrite(const Text: RawUtf8; Color: TConsoleColor = ccLightGray;
  NoLineFeed: boolean = false; NoColor: boolean = false); overload;

/// will wait for the ENTER key to be pressed, with all needed waiting process
// - on the main thread, will call Synchronize() for proper work e.g. with
// interface-based service implemented as optExecInMainThread
// - on Windows, from a non-main Thread, respond to PostThreadMessage(WM_QUIT)
// - on Windows, also properly respond to Ctrl-C or closing console events
// - on POSIX, will call SynDaemonIntercept first, so that Ctrl-C or SIG_QUIT
// will also be intercepted and let this procedure return
procedure ConsoleWaitForEnterKey;

/// read all available content from stdin
// - could be used to retrieve some file piped to the command line
// - the content is not converted, so will follow the encoding used for storage
function ConsoleReadBody: RawByteString;

{$ifdef OSWINDOWS}

/// low-level access to the keyboard state of a given key
function ConsoleKeyPressed(ExpectedKey: Word): boolean;

/// local RTL wrapper function to avoid linking mormot.core.unicode.pas
procedure Win32PWideCharToUtf8(P: PWideChar; Len: PtrInt;
  out res: RawUtf8); overload;

/// local RTL wrapper function to avoid linking mormot.core.unicode.pas
procedure Win32PWideCharToUtf8(P: PWideChar; out res: RawUtf8); overload;

/// local RTL wrapper function to avoid linking mormot.core.unicode.pas
// - returns dest.buf as PWideChar result, and dest.len as length
// - caller should always call dest.Done to release (unlikely) temporary memory
function Utf8ToWin32PWideChar(const Text: RawUtf8;
  var dest: TSynTempBuffer): PWideChar;

/// ask the Operating System to convert a file URL to a local file path
// - only Windows has a such a PathCreateFromUrl() API
// - POSIX define this in mormot.net.http.pas, where TUri is available
// - used e.g. by TNetClientProtocolFile to implement the 'file://' protocol
function GetFileNameFromUrl(const Uri: string): TFileName;

{$else}

/// internal function to avoid linking mormot.core.buffers.pas
function PosixParseHex32(p: PAnsiChar): integer;

/// internal function to avoid linking mormot.core.buffers.pas
procedure ParseHex(p: PAnsiChar; b: PByte; n: integer);

/// internal function just wrapping fppoll(POLLIN or POLLPRI)
function WaitReadPending(fd, timeout: integer): boolean;

/// POSIX-only function calling directly getdents/getdents64 syscall
// - could be used when FindFirst/FindNext are an overkill, e.g. to quickly
// cache all file names of a folder in memory, optionally with its sub-folders
// - used e.g. by TPosixFileCaseInsensitive from mormot.core.unicode
// - warning: the file system has to support d_type (e.g. btrfs, ext2-ext4) so
// that Recursive is handled and only DT_REG files are retrieved; non-compliant
// file systems (or Linux Kernel older than 2.6.4) won't support the Recursive
// search, and may return some false positives, like symlinks or nested folders
function PosixFileNames(const Folder: TFileName; Recursive: boolean): TRawUtf8DynArray;

{$endif OSWINDOWS}

/// internal function to avoid linking mormot.core.buffers.pas
// - will output the value as one number with one decimal and KB/MB/GB/TB suffix
function _oskb(Size: QWord): shortstring;

/// direct conversion of a UTF-8 encoded string into a console OEM-encoded string
// - under Windows, will use the CP_OEM encoding
// - under Linux, will expect the console to be defined with UTF-8 encoding
// - we don't propose any ConsoleToUtf8() function because Windows depends on
// the running program itself: most should generates CP_OEM (e.g. 850) as expected,
// but some could use the system code page or even UTF-16 binary with BOM (!) -
// so you may consider using AnsiToUtf8() with the proper code page
function Utf8ToConsole(const S: RawUtf8): RawByteString;


type
  /// encapsulate cross-platform loading of library files
  // - this generic class can be used for any external library (.dll/.so)
  TSynLibrary = class
  protected
    fHandle: TLibHandle;
    fLibraryPath: TFileName;
    fTryFromExecutableFolder: boolean;
    {$ifdef OSPOSIX}
    fLibraryPathTested: boolean;
    {$endif OSPOSIX}
  public
    /// cross-platform resolution of a function entry in this library
    // - if RaiseExceptionOnFailure is set, missing entry will call FreeLib then raise it
    // - ProcName can be a space-separated list of procedure names, to try
    // alternate API names (e.g. for OpenSSL 1.1.1/3.x compatibility)
    // - if ProcName starts with '?' then RaiseExceptionOnFailure = nil is set
    function Resolve(const Prefix, ProcName: RawUtf8; Entry: PPointer;
      RaiseExceptionOnFailure: ExceptionClass = nil): boolean;
    /// cross-platform resolution of all function entries in this library
    // - will search and fill Entry^ for all ProcName^ until ProcName^=nil
    // - return true on success, false and call FreeLib if any entry is missing
    function ResolveAll(ProcName: PPAnsiChar; Entry: PPointer): boolean;
    /// cross-platform call to FreeLibrary() + set fHandle := 0
    // - as called by Destroy, but you can use it directly to reload the library
    procedure FreeLib;
    /// same as SafeLoadLibrary() but setting fLibraryPath and cwd on Windows
    function TryLoadLibrary(const aLibrary: array of TFileName;
      aRaiseExceptionOnFailure: ExceptionClass): boolean; virtual;
    /// release associated memory and linked library
    destructor Destroy; override;
    /// return TRUE if the library and all procedures were found
    function Exists: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// the associated library handle
    property Handle: TLibHandle
      read fHandle write fHandle;
    /// the loaded library path
    // - on POSIX, contains the full path (via dladdr) once Resolve() is called
    property LibraryPath: TFileName
      read fLibraryPath;
    /// if set, and no path is specified, will try from Executable.ProgramFilePath
    property TryFromExecutableFolder: boolean
      read fTryFromExecutableFolder write fTryFromExecutableFolder;
  end;


{ *************** Per Class Properties O(1) Lookup via vmtAutoTable Slot }

/// self-modifying code - change some memory buffer in the code segment
// - if Backup is not nil, it should point to a Size array of bytes, ready
// to contain the overridden code buffer, for further hook disabling
// - some systems do forbid such live patching: consider setting NOPATCHVMT
// and NOPATCHRTL conditionals for such projects
procedure PatchCode(Old, New: pointer; Size: PtrInt; Backup: pointer = nil;
  LeaveUnprotected: boolean = false);

/// self-modifying code - change one PtrUInt in the code segment
procedure PatchCodePtrUInt(Code: PPtrUInt; Value: PtrUInt;
  LeaveUnprotected: boolean = false);

{$ifdef CPUINTEL}
/// low-level i386/x86_64 asm routine patch and redirection
procedure RedirectCode(Func, RedirectFunc: Pointer);
{$endif CPUINTEL}


{ ************** Cross-Platform Charset and CodePage Support }

{$ifdef OSPOSIX}
const
  ANSI_CHARSET = 0;
  DEFAULT_CHARSET = 1;
  SYMBOL_CHARSET = 2;
  SHIFTJIS_CHARSET = $80;
  HANGEUL_CHARSET = 129;
  GB2312_CHARSET = 134;
  CHINESEBIG5_CHARSET = 136;
  OEM_CHARSET = 255;
  JOHAB_CHARSET = 130;
  HEBREW_CHARSET = 177;
  ARABIC_CHARSET = 178;
  GREEK_CHARSET = 161;
  TURKISH_CHARSET = 162;
  VIETNAMESE_CHARSET = 163;
  THAI_CHARSET = 222;
  EASTEUROPE_CHARSET = 238;
  RUSSIAN_CHARSET = 204;
  BALTIC_CHARSET = 186;
{$else}
{$ifdef FPC} // a missing declaration
const
  VIETNAMESE_CHARSET = 163;
{$endif FPC}
{$endif OSPOSIX}

/// convert a char set to a code page
function CharSetToCodePage(CharSet: integer): cardinal;

/// convert a code page to a char set
function CodePageToCharSet(CodePage: cardinal): integer;


{ **************** TSynLocker/TSynLocked and Low-Level Threading Features }

type
  /// a lightweight exclusive non-rentrant lock, stored in a PtrUInt value
  // - calls SwitchToThread after some spinning, but don't use any R/W OS API
  // - warning: methods are non rentrant, i.e. calling Lock twice in a raw would
  // deadlock: use TRWLock or TSynLocker/TOSLock for reentrant methods
  // - several lightlocks, each protecting a few variables (e.g. a list), may
  // be more efficient than a more global TOSLock/TRWLock
  // - our light locks are expected to be kept a very small amount of time (some
  // CPU cycles): use TOSLightLock if the lock may block too long
  // - TryLock/UnLock can be used to thread-safely acquire a shared resource
  // - only consume 4 bytes on CPU32, 8 bytes on CPU64
  {$ifdef USERECORDWITHMETHODS}
  TLightLock = record
  {$else}
  TLightLock = object
  {$endif USERECORDWITHMETHODS}
  private
    Flags: PtrUInt;
    // low-level function called by the Lock method when inlined
    procedure LockSpin;
  public
    /// to be called if the instance has not been filled with 0
    // - e.g. not needed if TLightLock is defined as a class field
    procedure Init;
      {$ifdef HASINLINE} inline; {$endif}
    /// could be called to finalize the instance as a TOSLock
    // - does nothing - just for compatibility with TOSLock
    procedure Done;
      {$ifdef HASINLINE} inline; {$endif}
    /// enter an exclusive non-rentrant lock
    procedure Lock;
      {$ifdef HASINLINE} inline; {$endif}
    /// try to enter an exclusive non-rentrant lock
    // - if returned true, caller should eventually call UnLock()
    // - could also be used to thread-safely acquire a shared resource
    function TryLock: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// check if the non-rentrant lock has been acquired
    function IsLocked: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// leave an exclusive non-rentrant lock
    procedure UnLock;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  /// a lightweight multiple Reads / exclusive Write non-upgradable lock
  // - calls SwitchToThread after some spinning, but don't use any R/W OS API
  // - warning: ReadLocks are reentrant and allow concurrent acccess, but calling
  // WriteLock within a ReadLock, or within another WriteLock, would deadlock
  // - consider TRWLock if you need an upgradable lock - but for mostly reads,
  // TRWLightLock.ReadLock/ReadUnLock/WriteLock pattern is faster than upgrading
  // - our light locks are expected to be kept a very small amount of time (some
  // CPU cycles): use TSynLocker or TOSLock if the lock may block too long
  // - several lightlocks, each protecting a few variables (e.g. a list), may
  // be more efficient than a more global TOSLock/TRWLock
  // - only consume 4 bytes on CPU32, 8 bytes on CPU64
  {$ifdef USERECORDWITHMETHODS}
  TRWLightLock = record
  {$else}
  TRWLightLock = object
  {$endif USERECORDWITHMETHODS}
  private
    Flags: PtrUInt; // bit 0 = WriteLock, >0 = ReadLock
    // low-level functions called by the Lock methods when inlined
    procedure ReadLockSpin;
    procedure WriteLockSpin;
  public
    /// to be called if the instance has not been filled with 0
    // - e.g. not needed if TRWLightLock is defined as a class field
    procedure Init;
      {$ifdef HASINLINE} inline; {$endif}
    /// enter a non-upgradable multiple reads lock
    // - read locks maintain a thread-safe counter, so are reentrant and non blocking
    // - warning: nested WriteLock call after a ReadLock would deadlock
    procedure ReadLock;
      {$ifdef HASINLINE} inline; {$endif}
    /// try to enter a non-upgradable multiple reads lock
    // - if returned true, caller should eventually call ReadUnLock
    // - read locks maintain a thread-safe counter, so are reentrant and non blocking
    // - warning: nested WriteLock call after a ReadLock would deadlock
    function TryReadLock: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// leave a non-upgradable multiple reads lock
    procedure ReadUnLock;
      {$ifdef HASINLINE} inline; {$endif}
    /// enter a non-rentrant non-upgradable exclusive write lock
    // - warning: nested WriteLock call after a ReadLock or another WriteLock
    // would deadlock
    procedure WriteLock;
      {$ifdef HASINLINE} inline; {$endif}
    /// try to enter a non-rentrant non-upgradable exclusive write lock
    // - if returned true, caller should eventually call WriteUnLock
    // - warning: nested TryWriteLock call after a ReadLock or another WriteLock
    // would deadlock
    function TryWriteLock: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// leave a non-rentrant non-upgradable exclusive write lock
    procedure WriteUnLock;
      {$ifdef HASINLINE} inline; {$endif}
  end;

type
  /// how TRWLock.Lock and TRWLock.UnLock high-level wrapper methods are called
  TRWLockContext = (
    cReadOnly,
    cReadWrite,
    cWrite);

  /// a lightweight multiple Reads / exclusive Write reentrant lock
  // - calls SwitchToThread after some spinning, but don't use any R/W OS API
  // - our light locks are expected to be kept a very small amount of time (some
  // CPU cycles): use TSynLocker or TOSLock if the lock may block too long
  // - warning: all methods are reentrant, but WriteLock/ReadWriteLock would
  // deadlock if called after a ReadOnlyLock
  {$ifdef USERECORDWITHMETHODS}
  TRWLock = record
  {$else}
  TRWLock = object
  {$endif USERECORDWITHMETHODS}
  private
    Flags: PtrUInt; // bit 0 = WriteLock, 1 = ReadWriteLock, >1 = ReadOnlyLock
    LastReadWriteLockThread, LastWriteLockThread: TThreadID; // to be reentrant
    LastReadWriteLockCount,  LastWriteLockCount: cardinal;
    {$ifndef FPC_ASMX64}
    procedure ReadOnlyLockSpin;
    {$endif FPC_ASMX64}
  public
    /// initialize the R/W lock
    // - not needed if TRWLock is part of a class - i.e. if was filled with 0
    procedure Init;
      {$ifdef HASINLINE} inline; {$endif}
    /// could be called at shutdown to ensure that the R/W lock is in neutral state
    procedure AssertDone;
    /// wait for the lock to be available for reading, but not upgradable to write
    // - several readers could acquire the lock simultaneously
    // - ReadOnlyLock is reentrant since there is a thread-safe internal counter
    // - warning: calling ReadWriteLock/WriteLock after ReadOnlyLock would deadlock
    // - typical usage is the following:
    // ! rwlock.ReadOnlyLock; // won't block concurrent ReadOnlyLock
    // ! try
    // !   result := Exists(value);
    // ! finally
    // !   rwlock.ReadOnlyUnLock;
    // ! end;
    procedure ReadOnlyLock;
      {$ifdef HASINLINE} {$ifndef FPC_ASMX64} inline; {$endif} {$endif}
    /// release a previous ReadOnlyLock call
    procedure ReadOnlyUnLock;
      {$ifdef HASINLINE} inline; {$endif}
    /// wait for the lock to be accessible for reading - later upgradable to write
    // - will mark the lock with the current thread so that a nested WriteLock
    // would be possible, but won't block concurrent ReadOnlyLock
    // - several readers could acquire ReadOnlyLock simultaneously, but only a
    // single thread could acquire a ReadWriteLock
    // - reentrant method, and nested WriteLock is allowed
    // - typical usage is the following:
    // ! rwlock.ReadWriteLock;      // won't block concurrent ReadOnlyLock
    // ! try                        // but block other ReadWriteLock/WriteLock
    // !   result := Exists(value);
    // !   if not result then
    // !   begin
    // !     rwlock.WriteLock; // block any ReadOnlyLock/ReadWriteLock/WriteLock
    // !     try
    // !       Add(value);
    // !     finally
    // !       rwlock.WriteUnLock;
    // !     end;
    // !   end;
    // ! finally
    // !   rwlock.ReadWriteUnLock;
    // ! end;
    procedure ReadWriteLock;
    /// release a previous ReadWriteLock call
    procedure ReadWriteUnLock;
      {$ifdef HASINLINE} inline; {$endif}
    /// wait for the lock to be accessible for writing
    // - the write lock is exclusive
    // - calling WriteLock within a ReadWriteLock is allowed and won't block
    // - but calling WriteLock within a ReadOnlyLock would deaadlock
    // - this method is rentrant from a single thread
    // - typical usage is the following:
    // ! rwlock.WriteLock; // block any ReadOnlyLock/ReadWriteLock/WriteLock
    // ! try
    // !   Add(value);
    // ! finally
    // !   rwlock.WriteUnLock;
    // ! end;
    procedure WriteLock;
    /// release a previous WriteLock call
    procedure WriteUnlock;
      {$ifdef FPC_OR_DELPHIXE4} inline; {$endif} // circumvent weird Delphi bug
    /// a high-level wrapper over ReadOnlyLock/ReadWriteLock/WriteLock methods
    procedure Lock(context: TRWLockContext {$ifndef PUREMORMOT2} = cWrite {$endif});
      {$ifdef HASINLINE} inline; {$endif}
    /// a high-level wrapper over ReadOnlyUnLock/ReadWriteUnLock/WriteUnLock methods
    procedure UnLock(context: TRWLockContext {$ifndef PUREMORMOT2} = cWrite {$endif});
      {$ifdef HASINLINE} inline; {$endif}
  end;
  PRWLock = ^TRWLock;

  /// the standard rentrant lock supplied by the Operating System
  // - maps TRTLCriticalSection, i.e. calls Win32 API or pthreads library
  // - don't forget to call Init and Done to properly initialize the structure
  // - if you do require a non-rentrant/recursive lock, consider TOSLightLock
  // - same signature as TLightLock/TOSLightLock, usable as compile time alternatives
  {$ifdef USERECORDWITHMETHODS}
  TOSLock = record
  {$else}
  TOSLock = object
  {$endif USERECORDWITHMETHODS}
  private
    CS: TRTLCriticalSection;
  public
    /// to be called to setup the instance
    // - mandatory in all cases, even if TOSLock is part of a class
    procedure Init;
    /// to be called to finalize the instance
    procedure Done;
    /// enter an OS lock
    // - notice: this method IS reentrant/recursive
    procedure Lock;
      {$ifdef FPC} inline; {$endif}
    /// try to enter an OS lock
    // - if returned true, caller should eventually call UnLock()
    function TryLock: boolean;
      {$ifdef FPC} inline; {$endif}
    /// leave an OS lock
    procedure UnLock;
      {$ifdef FPC} inline; {$endif}
  end;

  /// the fastest non-rentrant lock supplied by the Operating System
  // - calls Slim Reader/Writer (SRW) Win32 API in exclusive mode or directly
  // the pthread_mutex_*() library calls in non-recursive/fast mode on Linux
  // - on XP, where SRW are not available, fallback to a TLightLock
  // - on non-Linux POSIX, fallback to regular cthreads/TRTLCriticalSection
  // - don't forget to call Init and Done to properly initialize the structure
  // - to protect a very small code section of a few CPU cycles with no Init/Done
  // needed, and a lower footprint, you may consider our TLightLock
  // - same signature as TOSLock/TLightLock, usable as compile time alternatives
  // - warning: non-rentrant, i.e. nested Lock calls would block, as TLightLock
  // - no TryLock is defined on Windows, because TryAcquireSRWLockExclusive()
  // raised some unexpected EExternalException C000026 NT_STATUS_RESOURCE_NOT_OWNED
  // ("Attempt to release mutex not owned by caller") during testing
  {$ifdef USERECORDWITHMETHODS}
  TOSLightLock = record
  {$else}
  TOSLightLock = object
  {$endif USERECORDWITHMETHODS}
  private
    fMutex: TOSLightMutex;
  public
    /// to be called to setup the instance
    // - mandatory in all cases, even if TOSLock is part of a class
    procedure Init;
    /// to be called to finalize the instance
    procedure Done;
    /// enter an OS lock
    // - warning: this method is NOT reentrant/recursive, so any nested call
    // would deadlock
    procedure Lock;
      {$ifdef HASINLINE} inline; {$endif}
    {$ifdef OSPOSIX}
    /// access to raw pthread_mutex_trylock() method
    // - TryAcquireSRWLockExclusive() seems not stable on all Windows revisions
    function TryLock: boolean;
     {$ifdef HASINLINE} inline; {$endif}
    {$endif OSPOSIX}
    /// leave an OS lock
    procedure UnLock;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  /// points to one data entry in TLockedList
  PLockedListOne = ^TLockedListOne;
  /// abstract parent of one data entry in TLockedList, storing two PLockedListOne
  // - TLockedList should store unmanaged records starting with those fields
  // - sequence field contains an incremental random-seeded 30-bit integer > 65535,
  // to avoid ABA problems when instances are recycled
  TLockedListOne = record
    next, prev: pointer;
    sequence: PtrUInt;
  end;
  /// optional callback event to finalize one TLockedListOne instance
  TOnLockedListOne = procedure(one: PLockedListOne) of object;

  /// thread-safe dual-linked list of TLockedListOne descendants with recycling
  {$ifdef USERECORDWITHMETHODS}
  TLockedList = record
  {$else}
  TLockedList = object
  {$endif USERECORDWITHMETHODS}
  private
    fHead, fBin: pointer;
    fSize: integer;
    fSequence: PtrUInt;
    fOnFree: TOnLockedListOne;
  public
    /// thread-safe access to the list
    Safe: TLightLock;
    /// how many TLockedListOne instances are currently stored in this list
    // - excluding the instances in the recycle bin
    Count: integer;
    /// initialize the storage for an inherited TLockedListOne size
    procedure Init(onesize: PtrUInt; const onefree: TOnLockedListOne = nil);
    /// release all stored memory
    procedure Done;
    /// allocate a new PLockedListOne data instance in threadsafe O(1) process
    function New: pointer;
    /// release one PLockedListOne used data instance in threadsafe O(1) process
    function Free(one: pointer): boolean;
    /// release all TLockedListOne instances currently stored in this list
    // - without moving any of those instances into the internal recycle bin
    procedure Clear;
    /// release all to-be-recycled items available in the internal bin
    // - returns how many items have been released from the internal collector
    function EmptyBin: integer;
    /// raw access to the stored items as PLockedListOne dual-linked list
    property Head: pointer
      read fHead;
    /// the size of one stored instance, including its TLockedListOne header
    property Size: integer
      read fSize;
  end;

type
  /// how TSynLocker handles its thread processing
  // - by default, uSharedLock will use the main TRTLCriticalSection
  // - you may set uRWLock and call overloaded RWLock/RWUnLock() to use our
  // lighter TRWLock - but be aware that cReadOnly followed by cReadWrite/cWrite
  // would deadlock - regular Lock/UnLock will use cWrite exclusive lock
  // - uNoLock will disable the whole locking mechanism
  TSynLockerUse = (
    uSharedLock,
    uRWLock,
    uNoLock);

  /// allow to add cross-platform locking methods to any class instance
  // - typical use is to define a Safe: TSynLocker property, call Safe.Init
  // and Safe.Done in constructor/destructor methods, and use Safe.Lock/UnLock
  // methods in a try ... finally section
  // - in respect to the TCriticalSection class, fix a potential CPU cache line
  // conflict which may degrade the multi-threading performance, as reported by
  // @http://www.delphitools.info/2011/11/30/fixing-tcriticalsection
  // - internal padding is used to safely store up to 7 values protected
  // from concurrent access with a mutex, so that SizeOf(TSynLocker)>128
  // - for object-level locking, see TSynPersistentLock which owns one such
  // instance, or call low-level fSafe := NewSynLocker in your constructor,
  // then fSafe^.DoneAndFreemem in your destructor
  // - RWUse property could replace the TRTLCriticalSection by a lighter TRWLock
  // - see also TRWLock and TSynPersistentRWLock if the multiple read / exclusive
  // write lock is better (only if the locked process does not take too much time)
  {$ifdef USERECORDWITHMETHODS}
  TSynLocker = record
  {$else}
  TSynLocker = object
  {$endif USERECORDWITHMETHODS}
  private
    fSection: TRTLCriticalSection;
    fRW: TRWLock;
    fPaddingUsedCount: byte;
    fInitialized: boolean;
    fRWUse: TSynLockerUse;
    fLockCount: integer;
    function GetVariant(Index: integer): Variant;
    procedure SetVariant(Index: integer; const Value: Variant);
    function GetInt64(Index: integer): Int64;
    procedure SetInt64(Index: integer; const Value: Int64);
    function GetBool(Index: integer): boolean;
    procedure SetBool(Index: integer; const Value: boolean);
    function GetUnlockedInt64(Index: integer): Int64;
    procedure SetUnlockedInt64(Index: integer; const Value: Int64);
    function GetPointer(Index: integer): Pointer;
    procedure SetPointer(Index: integer; const Value: Pointer);
    function GetUtf8(Index: integer): RawUtf8;
    procedure SetUtf8(Index: integer; const Value: RawUtf8);
    function GetIsLocked: boolean;
    // - if RWUse=uSharedLock, calls EnterCriticalSection (no parallel readings)
    // - warning: if RWUse=uRWLock, this method will use the internal TRWLock
    // - defined in protected section for better inlining and to fix a Delphi
    // compiler bug about warning a missing Windows unit in the uses classes
    procedure RWLock(context: TRWLockContext);
      {$ifdef HASINLINE} inline; {$endif}
    procedure RWUnLock(context: TRWLockContext);
      {$ifdef HASINLINE} inline; {$endif}
  public
    /// internal padding data, also used to store up to 7 variant values
    // - this memory buffer will ensure no CPU cache line mixup occurs
    // - you should not use this field directly, but rather the Locked[],
    // LockedInt64[], LockedUtf8[] or LockedPointer[] methods
    // - if you want to access those array values, ensure you protect them
    // using a Safe.Lock; try ... Padding[n] ... finally Safe.Unlock structure,
    // and maintain the PaddingUsedCount property accurately
    Padding: array[0..6] of TVarData;
    /// initialize the mutex
    // - calling this method is mandatory (e.g. in the class constructor owning
    // the TSynLocker instance), otherwise you may encounter unexpected
    // behavior, like access violations or memory leaks
    procedure Init;
    /// finalize the mutex
    // - calling this method is mandatory (e.g. in the class destructor owning
    // the TSynLocker instance), otherwise you may encounter unexpected
    // behavior, like access violations or memory leaks
    procedure Done;
    /// finalize the mutex, and call FreeMem() on the pointer of this instance
    // - should have been initiazed with a NewSynLocker call
    procedure DoneAndFreeMem;
    /// low-level acquisition of the lock, as RWLock(cReadOnly)
    // - if RWUse=uSharedLock, calls EnterCriticalSection (no parallel readings)
    // - warning: with RWUse=uRWLock, a nested Lock call would deadlock, but not
    // nested ReadLock calls
    procedure ReadLock;
    /// low-level release of the lock, as RWUnLock(cReadOnly)
    procedure ReadUnLock;
    /// low-level acquisition of the lock, as RWLock(cReadWrite)
    // - if RWUse=uSharedLock, calls EnterCriticalSection (no parallel readings)
    // - with RWUse=uRWLock, a nested Lock call would not deadlock
    procedure ReadWriteLock;
    /// low-level release of the lock, as RWUnLock(cReadWrite)
    procedure ReadWriteUnLock;
    /// lock the instance for exclusive access, as RWLock(cWrite)
    // - is re-entrant from the same thread i.e. you can nest Lock/UnLock calls
    // - warning: with RWUse=uRWLock, would deadlock after a nested ReadLock,
    // but not after ReadWriteLock
    // - use as such to avoid race condition (from a Safe: TSynLocker property):
    // ! Safe.Lock;
    // ! try
    // !   ...
    // ! finally
    // !   Safe.Unlock;
    // ! end;
    procedure Lock;
    /// will try to acquire the mutex
    // - do nothing and return false if RWUse is not the default uSharedLock
    // - use as such to avoid race condition (from a Safe: TSynLocker property):
    // ! if Safe.TryLock then
    // !   try
    // !     ...
    // !   finally
    // !     Safe.Unlock;
    // !   end;
    function TryLock: boolean;
    /// will try to acquire the mutex for a given time
    // - just wait and return false if RWUse is not the default uSharedLock
    // - use as such to avoid race condition (from a Safe: TSynLocker property):
    // ! if Safe.TryLockMS(100) then
    // !   try
    // !     ...
    // !   finally
    // !     Safe.Unlock;
    // !   end;
    function TryLockMS(retryms: integer; terminated: PBoolean = nil): boolean;
    /// release the instance for exclusive access, as RWUnLock(cWrite)
    // - each Lock/TryLock should have its exact UnLock opposite, so a
    // try..finally block is mandatory for safe code
    procedure UnLock; overload;
    /// will enter the mutex until the IUnknown reference is released
    // - could be used as such under Delphi:
    // !begin
    // !  ... // unsafe code
    // !  Safe.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // local hidden IUnknown will release the lock for the method
    // - warning: under FPC, you should assign its result to a local variable -
    // see bug http://bugs.freepascal.org/view.php?id=26602
    // !var
    // !  LockFPC: IUnknown;
    // !begin
    // !  ... // unsafe code
    // !  LockFPC := Safe.ProtectMethod;
    // !  ... // thread-safe code
    // !end; // LockFPC will release the lock for the method
    // or
    // !begin
    // !  ... // unsafe code
    // !  with Safe.ProtectMethod do
    // !  begin
    // !    ... // thread-safe code
    // !  end; // local hidden IUnknown will release the lock for the method
    // !end;
    function ProtectMethod: IUnknown;
    /// number of values stored in the internal Padding[] array
    // - equals 0 if no value is actually stored, or a 1..7 number otherwise
    // - you should not have to use this field, but for optimized low-level
    // direct access to Padding[] values, within a Lock/UnLock safe block
    property PaddingUsedCount: byte
      read fPaddingUsedCount write fPaddingUsedCount;
    /// returns true if the mutex is currently locked by another thread
    // - with RWUse=uRWLock, any lock (even ReadOnlyLock) would return true
    property IsLocked: boolean
      read GetIsLocked;
    /// returns true if the Init method has been called for this mutex
    // - is only relevant if the whole object has been previously filled with 0,
    // i.e. as part of a class or as global variable, but won't be accurate
    // when allocated on stack
    property IsInitialized: boolean
      read fInitialized;
    /// safe locked access to a Variant value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // LockedBool, LockedInt64, LockedPointer and LockedUtf8 array properties
    // - returns null if the Index is out of range
    // - allow concurrent thread reading if RWUse was set to uRWLock
    property Locked[Index: integer]: Variant
      read GetVariant write SetVariant;
    /// safe locked access to a Int64 value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUtf8 array properties
    // - Int64s will be stored internally as a varInt64 variant
    // - returns nil if the Index is out of range, or does not store a Int64
    // - allow concurrent thread reading if RWUse was set to uRWLock
    property LockedInt64[Index: integer]: Int64
      read GetInt64 write SetInt64;
    /// safe locked access to a boolean value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked, LockedInt64, LockedPointer and LockedUtf8 array properties
    // - value will be stored internally as a varboolean variant
    // - returns nil if the Index is out of range, or does not store a boolean
    // - allow concurrent thread reading if RWUse was set to uRWLock
    property LockedBool[Index: integer]: boolean
      read GetBool write SetBool;
    /// safe locked access to a pointer/TObject value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked, LockedBool, LockedInt64 and LockedUtf8 array properties
    // - pointers will be stored internally as a varUnknown variant
    // - returns nil if the Index is out of range, or does not store a pointer
    // - allow concurrent thread reading if RWUse was set to uRWLock
    property LockedPointer[Index: integer]: Pointer
      read GetPointer write SetPointer;
    /// safe locked access to an UTF-8 string value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedPointer array properties
    // - UTF-8 string will be stored internally as a varString variant
    // - returns '' if the Index is out of range, or does not store a string
    // - allow concurrent thread reading if RWUse was set to uRWLock
    property LockedUtf8[Index: integer]: RawUtf8
      read GetUtf8 write SetUtf8;
    /// safe locked in-place increment to an Int64 value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUtf8 array properties
    // - Int64s will be stored internally as a varInt64 variant
    // - returns the newly stored value
    // - if the internal value is not defined yet, would use 0 as default value
    function LockedInt64Increment(Index: integer; const Increment: Int64): Int64;
    /// safe locked in-place exchange of a Variant value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUtf8 array properties
    // - returns the previous stored value, or null if the Index is out of range
    function LockedExchange(Index: integer; const Value: variant): variant;
    /// safe locked in-place exchange of a pointer/TObject value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUtf8 array properties
    // - pointers will be stored internally as a varUnknown variant
    // - returns the previous stored value, nil if the Index is out of range,
    // or does not store a pointer
    function LockedPointerExchange(Index: integer; Value: pointer): pointer;
    /// unsafe access to a Int64 value
    // - you may store up to 7 variables, using an 0..6 index, shared with
    // Locked and LockedUtf8 array properties
    // - Int64s will be stored internally as a varInt64 variant
    // - returns nil if the Index is out of range, or does not store a Int64
    // - you should rather call LockedInt64[] property, or use this property
    // with a Lock; try ... finally UnLock block
    property UnlockedInt64[Index: integer]: Int64
      read GetUnlockedInt64 write SetUnlockedInt64;
    /// how RWLock/RWUnLock would be processed
    property RWUse: TSynLockerUse
      read fRWUse write fRWUse;
  end;

  /// a pointer to a TSynLocker mutex instance
  // - see also NewSynLocker and TSynLocker.DoneAndFreemem functions
  PSynLocker = ^TSynLocker;

  /// raw class used by TAutoLocker.ProtectMethod and TSynLocker.ProtectMethod
  // - defined here for use by TAutoLocker in mormot.core.data.pas
  TAutoLock = class(TInterfacedObject)
  protected
    fLock: PSynLocker;
  public
    constructor Create(aLock: PSynLocker);
    destructor Destroy; override;
  end;

  /// our lightweight cross-platform TEvent-like component
  // - on Windows, calls directly the CreateEvent/ResetEvent/SetEvent API
  // - on Linux, will use eventfd() in blocking and non-semaphore mode
  // - on other POSIX, will use PRTLEvent which is lighter than TEvent BasicEvent
  // - only limitation is that we don't know if WaitFor is signaled or timeout,
  // but this is not a real problem in practice since most code don't need this
  // information or has already its own flag in its implementation logic
  TSynEvent = class
  protected
    fHandle: pointer; // Windows THandle or FPC PRTLEvent
    fFD: integer;     // for eventfd()
  public
    /// initialize an instance of cross-platform event
    constructor Create;
    /// finalize this instance of cross-platform event
    destructor Destroy; override;
    /// ignore any pending events, so that WaitFor will be set on next SetEvent
    procedure ResetEvent;
      {$ifdef OSPOSIX} inline; {$endif}
    /// trigger any pending event, releasing the WaitFor/WaitForEver methods
    procedure SetEvent;
      {$ifdef OSPOSIX} inline; {$endif}
    /// wait until SetEvent is called from another thread, with a maximum time
    // - does not return if it was signaled or timeout
    // - WARNING: you should wait from a single thread at once
    procedure WaitFor(TimeoutMS: integer);
      {$ifdef OSPOSIX} inline; {$endif}
    /// wait until SetEvent is called from another thread, with no maximum time
    procedure WaitForEver;
      {$ifdef OSPOSIX} inline; {$endif}
    /// calls SleepHiRes() in steps while checking terminated flag and this event
    function SleepStep(var start: Int64; terminated: PBoolean): Int64;
    /// could be used to tune your algorithm if the eventfd() API is used
    function IsEventFD: boolean;
      {$ifdef HASINLINE} inline; {$endif}
  end;


/// initialize a TSynLocker instance from heap
// - call DoneandFreeMem to release the associated memory and OS mutex
// - is used e.g. in TSynPersistentLock to reduce class instance size
function NewSynLocker: PSynLocker;

type
  {$M+}

  /// a persistent-agnostic alternative to TSynPersistentLock
  // - can be used as base class when custom JSON persistence is not needed
  // - consider a TRWLock field as a lighter multi read / exclusive write option
  TSynLocked = class
  protected
    fSafe: PSynLocker; // TSynLocker would increase inherited fields offset
  public
    /// initialize the instance, and its associated lock
    // - is defined as virtual, just like TObjectWithCustomCreate/TSynPersistent
    constructor Create; virtual;
    /// finalize the instance, and its associated lock
    destructor Destroy; override;
    /// access to the associated instance critical section
    // - call Safe.Lock/UnLock to protect multi-thread access on this storage
    property Safe: PSynLocker
      read fSafe;
  end;

  {$M-}

  /// meta-class definition of the TSynLocked hierarchy
  TSynLockedClass = class of TSynLocked;

  /// a thread-safe Pierre L'Ecuyer software random generator
  // - just wrap TLecuyer with a TLighLock
  // - should not be used, unless may be slightly faster than a threadvar
  {$ifdef USERECORDWITHMETHODS}
  TLecuyerThreadSafe = record
  {$else}
  TLecuyerThreadSafe = object
  {$endif USERECORDWITHMETHODS}
  public
    Safe: TLightLock;
    Generator: TLecuyer;
    /// compute the next 32-bit generated value
    function Next: cardinal; overload;
    /// compute a 64-bit floating point value
    function NextDouble: double;
    /// XOR some memory buffer with random bytes
    procedure Fill(dest: pointer; count: integer);
    /// fill some string[31] with 7-bit ASCII random text
    procedure FillShort31(var dest: TShort31);
  end;

  TThreadIDDynArray = array of TThreadID;

var
  /// a global thread-safe Pierre L'Ecuyer software random generator
  // - should not be used, unless may be slightly faster than a threadvar
  SharedRandom: TLecuyerThreadSafe;

{$ifdef OSPOSIX}
  /// could be set to TRUE to force SleepHiRes(0) to call the POSIX sched_yield
  // - in practice, it has been reported as buggy under POSIX systems
  // - even Linus Torvald himself raged against its usage - see e.g.
  // https://www.realworldtech.com/forum/?threadid=189711&curpostid=189752
  // - you may tempt the devil and try it by yourself
  SleepHiRes0Yield: boolean = false;
{$endif OSPOSIX}

/// similar to Windows sleep() API call, to be truly cross-platform
// - using millisecond resolution
// - SleepHiRes(0) calls ThreadSwitch on Windows, but POSIX version will
// wait 10 microsecond unless SleepHiRes0Yield is forced to true (bad idea)
// - in respect to RTL's Sleep() function, it will return on ESysEINTR if was
// interrupted by any OS signal
// - warning: wait typically for the next system timer interrupt on Windows,
// which is every 16ms by default; as a consequence, never rely on the ms
// supplied value to guess the elapsed time, but call GetTickCount64
procedure SleepHiRes(ms: cardinal); overload;

/// similar to Windows sleep() API call, but truly cross-platform and checking
// the Terminated flag during its wait for quick abort response
// - returns true if terminated^ was set to true (terminatedvalue)
function SleepHiRes(ms: cardinal; var terminated: boolean;
  terminatedvalue: boolean = true): boolean; overload;

/// call SleepHiRes() taking count of the activity, in 0/1/5/50/120-250 ms steps
// - range is agressively designed burning some CPU in favor of responsiveness
// - should reset start := 0 when some activity occurred, or start := -1 on
// Windows to avoid any SleepHiRes(0) = SwitchToThread call
// - would optionally return if terminated^ is set, or event is signaled
// - returns the current GetTickCount64 value
function SleepStep(var start: Int64; terminated: PBoolean = nil): Int64;

/// compute optimal sleep time as 0/1/5/50 then 120-250 ms steps
// - is agressively designed burning some CPU in favor of responsiveness
function SleepDelay(elapsed: PtrInt): PtrInt;

/// compute optimal sleep time as SleepStep, in 0/1/5/50/120-250 ms steps
// - is agressively designed burning some CPU in favor of responsiveness
// - start=0 would fill its value with tix; start<0 would fill its value with
// tix-50 so that SleepDelay() would never call SleepHiRes(0)
function SleepStepTime(var start, tix: Int64; endtix: PInt64 = nil): PtrInt;

/// similar to Windows SwitchToThread API call, to be truly cross-platform
// - call fpnanosleep(10) on POSIX systems, or the homonymous API on Windows
procedure SwitchToThread;
  {$ifdef OSWINDOWS} stdcall; {$endif}

/// try LockedExc() in a loop, calling SwitchToThread after some spinning
procedure SpinExc(var Target: PtrUInt; NewValue, Comperand: PtrUInt);

/// wrapper to implement a thread-safe T*ObjArray dynamic array storage
function ObjArrayAdd(var aObjArray; aItem: TObject;
  var aSafe: TLightLock; aCount: PInteger = nil): PtrInt; overload;

/// wrapper to implement a thread-safe pointer dynamic array storage
function PtrArrayDelete(var aPtrArray; aItem: pointer; var aSafe: TLightLock;
  aCount: PInteger = nil): PtrInt; overload;

/// try to kill/cancel a thread
// - on Windows, calls the TerminateThread() API
// - under Linux/FPC, calls pthread_cancel() API which is asynchronous
function RawKillThread(Thread: TThread): boolean;

type
  /// store a bitmask of logical CPU cores, as used by SetThreadMaskAffinity
  // - has 32/64-bit pointer-size on Windows, or 1024 bits on POSIX
  TCpuSet = {$ifdef OSWINDOWS} PtrUInt {$else} array[0..127] of byte {$endif};
var
  /// low-level bitmasks of logical CPU cores hosted on each hardware CPU socket
  // - filled at process startup as CpuSocketsMask[0 .. CpuSockets - 1] range
  CpuSocketsMask: array of TCpuSet;

/// fill a bitmask of CPU cores with zeros
procedure ResetCpuSet(out CpuSet: TCpuSet);
  {$ifdef HASINLINE} inline; {$endif}

/// set a particular bit in a mask of CPU cores
function SetCpuSet(var CpuSet: TCpuSet; CpuIndex: cardinal): boolean;

/// retrieve the current CPU cores masks available of the system
// - the current process may have been tuned to use only a sub-set of the cores
// e.g. via "taskset -c" on Linux
// - return the number of accessible CPU cores - i.e. GetBitsCount(CpuSet) or
// 0 if the function failed
function CurrentCpuSet(out CpuSet: TCpuSet): integer;

/// try to assign a given thread to a specific set of logical CPU core(s)
// - on Windows, calls the SetThreadAffinityMask() API
// - under Linux/FPC, calls pthread_setaffinity_np() API
function SetThreadMaskAffinity(Thread: TThread; const Mask: TCpuSet): boolean;

/// try to assign a given thread to a specific logical CPU core
// - CpuIndex should be in 0 .. SystemInfo.dwNumberOfProcessors - 1 range
function SetThreadCpuAffinity(Thread: TThread; CpuIndex: cardinal): boolean;

/// try to assign a given thread to a specific hardware CPU socket
// - SocketIndex should be in 0 .. CpuSockets - 1 range, and will use the
// CpuSocketsMask[] information retrieved during process startup
function SetThreadSocketAffinity(Thread: TThread; SocketIndex: cardinal): boolean;

/// low-level naming of a thread
// - on Windows, will raise a standard "fake" exception to notify the thread name
// - under Linux/FPC, calls pthread_setname_np() API which truncates to 16 chars
procedure RawSetThreadName(ThreadID: TThreadID; const Name: RawUtf8);

/// name the current thread so that it would be easily identified in the IDE debugger
// - could then be retrieved by CurrentThreadNameShort/GetCurrentThreadName
// - just a wrapper around SetThreadName(GetCurrentThreadId, ...)
procedure SetCurrentThreadName(const Format: RawUtf8; const Args: array of const); overload;

/// name the current thread so that it would be easily identified in the IDE debugger
// - could also be retrieved by CurrentThreadNameShort/GetCurrentThreadName
// - just a wrapper around SetThreadName(GetCurrentThreadId, ...)
procedure SetCurrentThreadName(const Name: RawUtf8); overload;

var
  /// name a thread so that it would be easily identified in the IDE debugger
  // - default implementation does nothing, unless mormot.core.log is included
  // - you can force this function to do nothing by setting the NOSETTHREADNAME
  // conditional, if you have issues with this feature when debugging your app
  // - most meaningless patterns (like 'TSql') are trimmed to reduce the
  // resulting length - which is convenient e.g. with POSIX truncation to 16 chars
  // - you can retrieve the name later on using CurrentThreadNameShort
  // - this method will register TSynLog.LogThreadName(), so threads calling it
  // should also call TSynLogFamily.OnThreadEnded/TSynLog.NotifyThreadEnded
  SetThreadName: procedure(ThreadID: TThreadID; const Format: RawUtf8;
    const Args: array of const);

/// low-level access to the thread name, as set by SetThreadName()
// - since threadvar can't contain managed strings, it is defined as TShort31,
// so is limited to 31 chars, which is enough since POSIX truncates to 16 chars
// and SetThreadName does trim meaningless patterns
function CurrentThreadNameShort: PShortString;

/// retrieve the thread name, as set by SetThreadName()
// - if possible, direct CurrentThreadNameShort function is slightly faster
// - will return the CurrentThreadNameShort^ threadvar 31 chars value
function GetCurrentThreadName: RawUtf8;

/// returns the thread id and the thread name as a ShortString
// - returns e.g. 'Thread 0001abcd [shortthreadname]'
// - for convenient use when logging or raising an exception
function GetCurrentThreadInfo: ShortString;

/// enter a process-wide giant lock for thread-safe shared process
// - shall be protected as such:
// ! GlobalLock;
// ! try
// !   .... do something thread-safe but as short as possible
// ! finally
// !  GlobalUnLock;
// ! end;
// - you should better not use such a giant-lock, but an instance-dedicated
// critical section/TSynLocker or TRWLock - these functions are just here to be
// convenient, for non time-critical process (e.g. singleton initialization
// of external libraries, or before RegisterGlobalShutdownRelease() which will
// use it anyway)
procedure GlobalLock;

/// release the giant lock for thread-safe shared process
procedure GlobalUnLock;

/// framework will register here some instances to be released eventually
// - better in this root unit than in each finalization section
// - its use is protected by the GlobalLock
function RegisterGlobalShutdownRelease(Instance: TObject;
  SearchExisting: boolean = false): pointer;


{ ****************** Unix Daemon and Windows Service Support }

type
  /// all possible states of a Windows service
  // - on POSIX, will identify only if the daemon is ssRunning or ssStopped
  TServiceState = (
    ssNotInstalled,
    ssStopped,
    ssStarting,
    ssStopping,
    ssRunning,
    ssResuming,
    ssPausing,
    ssPaused,
    ssFailed,
    ssErrorRetrievingState);
  PServiceState = ^TServiceState;
  TServiceStateDynArray = array of TServiceState;

/// return the ready to be displayed text of a TServiceState value
function ToText(st: TServiceState): PShortString; overload;

const
  /// could be used with ConsoleWrite() to notify a Windows service state
  SERVICESTATE_COLOR: array[TServiceState] of TConsoleColor = (
    ccBlue,       // NotInstalled
    ccLightRed,   // Stopped
    ccGreen,      // Starting
    ccRed,        // Stopping
    ccLightGreen, // Running
    ccGreen,      // Resuming
    ccBrown,      // Pausing
    ccWhite,      // Paused
    ccMagenta,    // Failed
    ccYellow);    // ErrorRetrievingState


{$ifdef OSWINDOWS}

{ *** some minimal Windows API definitions, replacing WinSvc.pas missing for FPC }

const
  SERVICE_QUERY_CONFIG         = $0001;
  SERVICE_CHANGE_CONFIG        = $0002;
  SERVICE_QUERY_STATUS         = $0004;
  SERVICE_ENUMERATE_DEPENDENTS = $0008;
  SERVICE_START                = $0010;
  SERVICE_STOP                 = $0020;
  SERVICE_PAUSE_CONTINUE       = $0040;
  SERVICE_INTERROGATE          = $0080;
  SERVICE_USER_DEFINED_CONTROL = $0100;
  SERVICE_ALL_ACCESS           = STANDARD_RIGHTS_REQUIRED or
                                 SERVICE_QUERY_CONFIG or
                                 SERVICE_CHANGE_CONFIG or
                                 SERVICE_QUERY_STATUS or
                                 SERVICE_ENUMERATE_DEPENDENTS or
                                 SERVICE_START or
                                 SERVICE_STOP or
                                 SERVICE_PAUSE_CONTINUE or
                                 SERVICE_INTERROGATE or
                                 SERVICE_USER_DEFINED_CONTROL;

  SC_MANAGER_CONNECT            = $0001;
  SC_MANAGER_CREATE_SERVICE     = $0002;
  SC_MANAGER_ENUMERATE_SERVICE  = $0004;
  SC_MANAGER_LOCK               = $0008;
  SC_MANAGER_QUERY_LOCK_STATUS  = $0010;
  SC_MANAGER_MODIFY_BOOT_CONFIG = $0020;
  SC_MANAGER_ALL_ACCESS         = STANDARD_RIGHTS_REQUIRED or
                                  SC_MANAGER_CONNECT or
                                  SC_MANAGER_CREATE_SERVICE or
                                  SC_MANAGER_ENUMERATE_SERVICE or
                                  SC_MANAGER_LOCK or
                                  SC_MANAGER_QUERY_LOCK_STATUS or
                                  SC_MANAGER_MODIFY_BOOT_CONFIG;

  SERVICE_CONFIG_DESCRIPTION    = $0001;

  SERVICE_WIN32_OWN_PROCESS     = $00000010;
  SERVICE_WIN32_SHARE_PROCESS   = $00000020;
  SERVICE_INTERACTIVE_PROCESS   = $00000100;

  SERVICE_BOOT_START            = $00000000;
  SERVICE_SYSTEM_START          = $00000001;
  SERVICE_AUTO_START            = $00000002;
  SERVICE_DEMAND_START          = $00000003;
  SERVICE_DISABLED              = $00000004;
  SERVICE_ERROR_IGNORE          = $00000000;
  SERVICE_ERROR_NORMAL          = $00000001;
  SERVICE_ERROR_SEVERE          = $00000002;
  SERVICE_ERROR_CRITICAL        = $00000003;

  SERVICE_CONTROL_STOP          = $00000001;
  SERVICE_CONTROL_PAUSE         = $00000002;
  SERVICE_CONTROL_CONTINUE      = $00000003;
  SERVICE_CONTROL_INTERROGATE   = $00000004;
  SERVICE_CONTROL_SHUTDOWN      = $00000005;

  SERVICE_STOPPED               = $00000001;
  SERVICE_START_PENDING         = $00000002;
  SERVICE_STOP_PENDING          = $00000003;
  SERVICE_RUNNING               = $00000004;
  SERVICE_CONTINUE_PENDING      = $00000005;
  SERVICE_PAUSE_PENDING         = $00000006;
  SERVICE_PAUSED                = $00000007;

  ERROR_FAILED_SERVICE_CONTROLLER_CONNECT = 1063;

type
  PServiceStatus = ^TServiceStatus;
  TServiceStatus = record
    dwServiceType: cardinal;
    dwCurrentState: cardinal;
    dwControlsAccepted: cardinal;
    dwWin32ExitCode: cardinal;
    dwServiceSpecificExitCode: cardinal;
    dwCheckPoint: cardinal;
    dwWaitHint: cardinal;
  end;

  PServiceStatusProcess = ^TServiceStatusProcess;
  TServiceStatusProcess = record
    Service: TServiceStatus;
    dwProcessId: cardinal;
    dwServiceFlags: cardinal;
  end;

  SC_HANDLE = THandle;
  SERVICE_STATUS_HANDLE = THandle;
  TServiceTableEntry = record
    lpServiceName: PWideChar;
    lpServiceProc: procedure(ArgCount: cardinal; Args: PPWideChar); stdcall;
  end;
  PServiceTableEntry = ^TServiceTableEntry;
  TServiceDescription = record
    lpDestription: PWideChar;
  end;

  {$Z4}
  SC_STATUS_TYPE = (SC_STATUS_PROCESS_INFO);
  {$Z1}

function OpenSCManagerW(lpMachineName, lpDatabaseName: PWideChar;
  dwDesiredAccess: cardinal): SC_HANDLE; stdcall; external advapi32;
function ChangeServiceConfig2W(hService: SC_HANDLE; dwsInfoLevel: cardinal;
  lpInfo: Pointer): BOOL; stdcall; external advapi32;
function StartServiceW(hService: SC_HANDLE; dwNumServiceArgs: cardinal;
  lpServiceArgVectors: PPWideChar): BOOL; stdcall; external advapi32;
function CreateServiceW(hSCManager: SC_HANDLE;
  lpServiceName, lpDisplayName: PWideChar;
  dwDesiredAccess, dwServiceType, dwStartType, dwErrorControl: cardinal;
  lpBinaryPathName, lpLoadOrderGroup: PWideChar; lpdwTagId: LPDWORD; lpDependencies,
  lpServiceStartName, lpPassword: PWideChar): SC_HANDLE; stdcall; external advapi32;
function OpenServiceW(hSCManager: SC_HANDLE; lpServiceName: PWideChar;
  dwDesiredAccess: cardinal): SC_HANDLE; stdcall; external advapi32;
function DeleteService(hService: SC_HANDLE): BOOL; stdcall; external advapi32;
function CloseServiceHandle(hSCObject: SC_HANDLE): BOOL; stdcall; external advapi32;
function QueryServiceStatus(hService: SC_HANDLE;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall; external advapi32;
function QueryServiceStatusEx(hService: SC_HANDLE;
  InfoLevel: SC_STATUS_TYPE; lpBuffer: Pointer; cbBufSize: cardinal;
  var pcbBytesNeeded: cardinal): BOOL; stdcall; external advapi32;
function ControlService(hService: SC_HANDLE; dwControl: cardinal;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall; external advapi32;
function SetServiceStatus(hServiceStatus: SERVICE_STATUS_HANDLE;
  var lpServiceStatus: TServiceStatus): BOOL; stdcall; external advapi32;
function RegisterServiceCtrlHandlerW(lpServiceName: PWideChar;
  lpHandlerProc: TFarProc): SERVICE_STATUS_HANDLE; stdcall; external advapi32;
function StartServiceCtrlDispatcherW(
  lpServiceStartTable: PServiceTableEntry): BOOL; stdcall; external advapi32;

function OpenServiceManager(const TargetComputer, DatabaseName: RawUtf8;
  dwDesiredAccess: cardinal): SC_HANDLE;
function OpenServiceInstance(hSCManager: SC_HANDLE; const ServiceName: RawUtf8;
  dwDesiredAccess: cardinal): SC_HANDLE;


{ *** high level classes to define and manage Windows Services }

var
  /// can be assigned from TSynLog.DoLog class method for
  // TServiceController/TService logging
  // - default is nil, i.e. disabling logging, since it may interfere with the
  // logging process of the Windows Service itself
  WindowsServiceLog: TSynLogProc;

type
  /// TServiceControler class is intended to create a new Windows Service instance
  // or to maintain (that is start, stop, pause, resume...) an existing Service
  // - to provide the service itself, use the TService class
  TServiceController = class
  protected
    fSCHandle: THandle;
    fHandle: THandle;
    fStatus: TServiceStatus;
    fName: RawUtf8;
  protected
    function GetStatus: TServiceStatus;
    function GetState: TServiceState;
  public
    /// create a new Windows Service and control it and/or its configuration
    // - TargetComputer - set it to empty string if local computer is the target.
    // - DatabaseName - set it to empty string if the default database is supposed
    // ('ServicesActive').
    // - Name - name of a service.
    // - DisplayName - display name of a service.
    // - Path - a path to binary (executable) of the service created.
    // - OrderGroup - an order group name (unnecessary)
    // - Dependencies - string containing a list with names of services, which must
    // start before this service (every name should be separated with ';' or an
    // empty string can be passed if there is no dependency).
    // - Username - login name. For service type SERVICE_WIN32_OWN_PROCESS, the
    // account name in the form of "DomainName\Username"; If the account
    // belongs to the built-in domain, ".\Username" can be specified;
    // Services of type SERVICE_WIN32_SHARE_PROCESS are not allowed to
    // specify an account other than LocalSystem. If '' is specified, the
    // service will be logged on as the 'LocalSystem' account, in which
    // case, the Password parameter must be empty too.
    // - Password - a password for login name. If the service type is
    // SERVICE_KERNEL_DRIVER or SERVICE_FILE_SYSTEM_DRIVER,
    // this parameter is ignored.
    // - DesiredAccess - a combination of following flags:
    // SERVICE_ALL_ACCESS (default value), SERVICE_CHANGE_CONFIG,
    // SERVICE_ENUMERATE_DEPENDENTS, SERVICE_INTERROGATE, SERVICE_PAUSE_CONTINUE,
    // SERVICE_QUERY_CONFIG, SERVICE_QUERY_STATUS, SERVICE_START, SERVICE_STOP,
    // SERVICE_USER_DEFINED_CONTROL
    // - ServiceType - a set of following flags:
    // SERVICE_WIN32_OWN_PROCESS (default value, which specifies a Win32 service
    // that runs in its own process), SERVICE_WIN32_SHARE_PROCESS,
    // SERVICE_KERNEL_DRIVER, SERVICE_FILE_SYSTEM_DRIVER,
    // SERVICE_INTERACTIVE_PROCESS (default value, which enables a Win32 service
    // process to interact with the desktop)
    // - StartType - one of following values:
    // SERVICE_BOOT_START, SERVICE_SYSTEM_START,
    // SERVICE_AUTO_START (which specifies a device driver or service started by
    // the service control manager automatically during system startup),
    // SERVICE_DEMAND_START (default value, which specifies a service started by
    // a service control manager when a process calls the StartService function,
    // that is the TServiceController.Start method), SERVICE_DISABLED
    // - ErrorControl - one of following:
    // SERVICE_ERROR_IGNORE, SERVICE_ERROR_NORMAL (default value, by which
    // the startup program logs the error and displays a message but continues
    // the startup operation), SERVICE_ERROR_SEVERE,
    // SERVICE_ERROR_CRITICAL
    constructor CreateNewService(
      const TargetComputer, DatabaseName, Name, DisplayName: RawUtf8;
      const Path: TFileName;
      const OrderGroup: RawUtf8 = ''; const Dependencies: RawUtf8 = '';
      const Username: RawUtf8 = ''; const Password: RawUtf8 = '';
      DesiredAccess: cardinal = SERVICE_ALL_ACCESS;
      ServiceType: cardinal = SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
      StartType: cardinal = SERVICE_DEMAND_START;
      ErrorControl: cardinal = SERVICE_ERROR_NORMAL);
    /// wrapper around CreateNewService() to install the current executable as service
    class function Install(const Name, DisplayName, Description: RawUtf8;
      AutoStart: boolean; ExeName: TFileName = '';
      const Dependencies: RawUtf8 = ''; const UserName: RawUtf8 = '';
      const Password: RawUtf8 = ''): TServiceState;
    /// wrapper around CreateOpenService(SERVICE_QUERY_STATUS) and GetState
    class function CurrentState(const Name: RawUtf8): TServiceState;
    /// open an existing service, in order to control it or its configuration
    // from your application
    // - TargetComputer - set it to empty string if local computer is the target.
    // - DatabaseName - set it to empty string if the default database is supposed
    // ('ServicesActive').
    // - Name - name of a service.
    // - DesiredAccess - a combination of following flags:
    // SERVICE_ALL_ACCESS, SERVICE_CHANGE_CONFIG, SERVICE_ENUMERATE_DEPENDENTS,
    // SERVICE_INTERROGATE, SERVICE_PAUSE_CONTINUE, SERVICE_QUERY_CONFIG,
    // SERVICE_QUERY_STATUS, SERVICE_START, SERVICE_STOP, SERVICE_USER_DEFINED_CONTROL
    constructor CreateOpenService(
      const TargetComputer, DataBaseName, Name: RawUtf8;
      DesiredAccess: cardinal = SERVICE_ALL_ACCESS);
    /// release memory and handles
    destructor Destroy; override;
    /// Handle of SC manager
    property SCHandle: THandle
      read fSCHandle;
    /// Handle of service opened or created
    // - its value is 0 if something failed in any Create*() method
    property Handle: THandle
      read fHandle;
    /// Retrieve the Current status of the service
    property Status: TServiceStatus
      read GetStatus;
    /// Retrieve the Current state of the service
    property State: TServiceState
      read GetState;
    /// Requests the service to stop
    function Stop: boolean;
    /// Requests the service to pause
    function Pause: boolean;
    /// Requests the paused service to resume
    function Resume: boolean;
    /// Requests the service to update immediately its current status information
    // to the service control manager
    function Refresh: boolean;
    /// Request the service to shutdown
    // - this function always return false
    function Shutdown: boolean;
    /// Removes service from the system, i.e. close the Service
    function Delete: boolean;
    /// starts the execution of a service with some specified arguments
    // - this version expect PWideChar pointers, i.e. UTF-16 strings
    function Start(const Args: array of PWideChar): boolean;
    /// try to define the description text of this service
    function SetDescription(const Description: RawUtf8): boolean;
    /// this class method will check the command line parameters, and will let
    //  control the service according to it
    // - MyServiceSetup.exe /install will install the service
    // - MyServiceSetup.exe /start   will start the service
    // - MyServiceSetup.exe /stop    will stop the service
    // - MyServiceSetup.exe /uninstall will uninstall the service
    // - so that you can write in the main block of your .dpr:
    // !CheckParameters('MyService.exe',HTTPSERVICENAME,HTTPSERVICEDISPLAYNAME);
    // - if ExeFileName='', it will install the current executable
    // - optional Description and Dependencies text may be specified
    class procedure CheckParameters(const ExeFileName: TFileName;
      const ServiceName, DisplayName, Description: RawUtf8;
      const Dependencies: RawUtf8 = '');
  end;

  {$M+}
  TService = class;
  {$M-}

  /// callback procedure for Windows Service Controller
  TServiceControlHandler = procedure(CtrlCode: cardinal); stdcall;

  /// event triggered for Control handler
  TServiceControlEvent = procedure(Sender: TService; Code: cardinal) of object;

  /// event triggered to implement the Service functionality
  TServiceEvent = procedure(Sender: TService) of object;

  /// abstract class to let an executable implement a Windows Service
  // - do not use this class directly, but TServiceSingle
  TService = class
  protected
    fServiceName: RawUtf8;
    fDisplayName: RawUtf8;
    fStartType: cardinal;
    fServiceType: cardinal;
    fData: cardinal;
    fControlHandler: TServiceControlHandler;
    fOnControl: TServiceControlEvent;
    fOnInterrogate: TServiceEvent;
    fOnPause: TServiceEvent;
    fOnShutdown: TServiceEvent;
    fOnStart: TServiceEvent;
    fOnExecute: TServiceEvent;
    fOnResume: TServiceEvent;
    fOnStop: TServiceEvent;
    fStatusRec: TServiceStatus;
    fArgsList: TRawUtf8DynArray;
    fStatusHandle: THandle;
    function GetArgCount: Integer;
    function GetArgs(Idx: Integer): RawUtf8;
    function GetInstalled: boolean;
    procedure SetStatus(const Value: TServiceStatus);
    procedure CtrlHandle(Code: cardinal);
    function GetControlHandler: TServiceControlHandler;
    procedure SetControlHandler(const Value: TServiceControlHandler);
    procedure ServiceProc(ArgCount: integer; Args: PPWideChar);
  public
    /// internal method redirecting to WindowsServiceLog global variable
    class procedure DoLog(Level: TSynLogLevel; const Fmt: RawUtf8;
      const Args: array of const; Instance: TObject);
    /// Creates the service
    // - the service is added to the internal registered services
    // - main application must instantiate the TServiceSingle class, then call
    // the global ServiceSingleRun procedure to actually start the services
    // - caller must free the TService instance when it's no longer used
    constructor Create(const aServiceName, aDisplayName: RawUtf8); reintroduce; virtual;
    /// this method is the main service entrance, from the OS point of view
    // - it will call OnControl/OnStop/OnPause/OnResume/OnShutdown events
    // - and report the service status to the system (via ReportStatus method)
    procedure DoCtrlHandle(Code: cardinal); virtual;
    /// Reports new status to the system
    function ReportStatus(dwState, dwExitCode, dwWait: cardinal): BOOL;
    /// Installs the service in the database
    // - return true on success
    // - create a local TServiceController with the current executable file,
    // with the supplied command line parameters
    // - you can optionally append some parameters, which will be appended
    // to the
    function Install(const Params: TFileName = ''): boolean;
    /// Removes the service from database
    //  - uses a local TServiceController with the current Service Name
    procedure Remove;
    /// Starts the service
    //  - uses a local TServiceController with the current Service Name
    procedure Start;
    /// Stops the service
    // - uses a local TServiceController with the current Service Name
    procedure Stop;
    /// this is the main method, in which the Service should implement its run
    procedure Execute; virtual;

    /// Number of arguments passed to the service by the service controler
    property ArgCount: Integer
      read GetArgCount;
    /// List of arguments passed to the service by the service controler
    // - Idx is in range 0..ArgCount - 1
    property Args[Idx: Integer]: RawUtf8
      read GetArgs;
    /// Any data You wish to associate with the service object
    property Data: cardinal
      read FData write FData;
    /// Whether service is installed in DataBase
    // - uses a local TServiceController to check if the current Service Name exists
    property Installed: boolean
      read GetInstalled;
    /// Current service status
    // - To report new status to the system, assign another
    // value to this record, or use ReportStatus method (preferred)
    property Status: TServiceStatus
      read fStatusRec write SetStatus;
    /// Callback handler for Windows Service Controller
    // - if handler is not set, then auto generated handler calls DoCtrlHandle
    // (note that this auto-generated stubb is... not working yet - so you should
    // either set your own procedure to this property, or use TServiceSingle)
    // - a typical control handler may be defined as such:
    // ! var MyGlobalService: TService;
    // !
    // ! procedure MyServiceControlHandler(Opcode: LongWord); stdcall;
    // ! begin
    // !   if MyGlobalService<>nil then
    // !     MyGlobalService.DoCtrlHandle(Opcode);
    // ! end;
    // !
    // ! ...
    // ! MyGlobalService := TService.Create(...
    // ! MyGlobalService.ControlHandler := MyServiceControlHandler;
    property ControlHandler: TServiceControlHandler
      read GetControlHandler write SetControlHandler;
    /// Start event is executed before the main service thread (i.e. in the Execute method)
    property OnStart: TServiceEvent
      read fOnStart write fOnStart;
    /// custom Execute event
    // - launched in the main service thread (i.e. in the Execute method)
    property OnExecute: TServiceEvent
      read fOnExecute write fOnExecute;
    /// custom event triggered when a Control Code is received from Windows
    property OnControl: TServiceControlEvent
      read fOnControl write fOnControl;
    /// custom event triggered when the service is stopped
    property OnStop: TServiceEvent
      read fOnStop write fOnStop;
    /// custom event triggered when the service is paused
    property OnPause: TServiceEvent
      read fOnPause write fOnPause;
    /// custom event triggered when the service is resumed
    property OnResume: TServiceEvent
      read fOnResume write fOnResume;
    /// custom event triggered when the service receive an Interrogate command
    // - could call ReportStatus() e.g. to notify a problem
    property OnInterrogate: TServiceEvent
      read fOnInterrogate write fOnInterrogate;
    /// custom event triggered when the service is shut down
    property OnShutdown: TServiceEvent
      read fOnShutdown write fOnShutdown;
  published
    /// Name of the service. Must be unique
    property ServiceName: RawUtf8
      read fServiceName;
    /// Display name of the service
    property DisplayName: RawUtf8
      read fDisplayName write fDisplayName;
    /// Type of service
    property ServiceType: cardinal
      read fServiceType write fServiceType;
    /// Type of start of service
    property StartType: cardinal
      read fStartType write fStartType;
  end;

  /// inherit from this class if your application has a single Windows Service
  // - note that only this single-service implementation is available by now
  // - the regular way of executing services is to instantiate a TServiceSingle
  // instance (which will fill the ServiceSingle variable) and its methods,
  // then eventually call ServiceSingleRun
  TServiceSingle = class(TService)
  public
    /// will set a global function as service controller
    constructor Create(const aServiceName, aDisplayName: RawUtf8); override;
    /// will release the global service controller
    destructor Destroy; override;
  end;

var
  /// the main TServiceSingle instance running in the current executable
  // - the regular way of executing services is to instantiate a TServiceSingle
  // instance (which will fill this ServiceSingle variable) and its methods,
  // then eventually call ServiceSingleRun
  ServiceSingle: TServiceSingle = nil;

/// launch the registered Service execution
// - ServiceSingle provided by this application (most probably from
// TServiceSingle.Create) is sent to the operating system
// - returns TRUE on success
// - returns FALSE on error (to get extended information, call GetLastError)
function ServiceSingleRun: boolean;

/// convert the Control Code retrieved from Windows into a service state
// enumeration item
function CurrentStateToServiceState(CurrentState: cardinal): TServiceState;

/// return the ProcessID of a given service, by name
function GetServicePid(const aServiceName: RawUtf8;
  aServiceState: PServiceState = nil): cardinal;

/// try to gently stop a given Windows console app from its ProcessID
// - will send a Ctrl-C event (acquiring the process console)
function CancelProcess(pid: cardinal; waitseconds: integer): boolean;

/// try to gently quit a Windows process from its ProcessID
// - will send a WM_QUIT message to all its threads
function QuitProcess(pid: cardinal; waitseconds: integer): boolean;

/// forcibly terminate a Windows process from its ProcessID
// - call TerminateProcess() and wait for its ending
function KillProcess(pid: cardinal; waitseconds: integer = 30): boolean;

/// install a Windows event handler for Ctrl+C pressed on the Console
function HandleCtrlC(const OnClose: TThreadMethod): boolean;

/// define a Windows Job to close associated processes together
// - warning: main process should include the CREATE_BREAKAWAY_FROM_JOB flag
// - you should later call CloseHandle() on the returned handle, if not 0 
function CreateJobToClose(parentpid: cardinal): THandle;

/// associate a process to a Windows Job created by CreateJobToClose()
function AssignJobToProcess(job, process: THandle; const ctxt: ShortString): boolean;

{$else}

/// low-level function able to properly run or fork the current process
// then execute the start/stop methods of a TSynDaemon / TDDDDaemon instance
// - fork will create a local /run/[ProgramName]-[ProgramPathHash].pid file name
// - onLog can be assigned from TSynLog.DoLog for proper logging
procedure RunUntilSigTerminated(daemon: TObject; dofork: boolean;
  const start, stop: TThreadMethod; const onlog: TSynLogProc = nil;
  const servicename: string = '');

/// kill a process previously created by RunUntilSigTerminated(dofork=true)
// - will lookup a local /run/[ProgramName]-[ProgramPathHash].pid file name to
// retrieve the actual PID to be killed, then send a SIGTERM, and wait
// waitseconds for the .pid file to disapear
// - returns true on success, false on error (e.g. no valid .pid file or
// the file didn't disappear, which may mean that the daemon is broken)
function RunUntilSigTerminatedForKill(waitseconds: integer = 30): boolean;

var
  /// optional folder where the .pid is created
  // - should include a trailing '/' character
  // - to be used if the current executable folder is read/only
  RunUntilSigTerminatedPidFilePath: TFileName;

/// local .pid file name as created by RunUntilSigTerminated(dofork=true)
function RunUntilSigTerminatedPidFile(ensureWritable: boolean = false): TFileName;

/// check the local .pid file to return either ssRunning or ssStopped
function RunUntilSigTerminatedState: TServiceState;

var
  /// once SynDaemonIntercept has been called, this global variable
  // contains the SIGQUIT / SIGTERM / SIGINT received signal
  SynDaemonTerminated: integer;

/// enable low-level interception of executable stop signals
// - any SIGQUIT / SIGTERM / SIGINT signal will set appropriately the global
// SynDaemonTerminated variable, with an optional logged entry to log
// - called e.g. by RunUntilSigTerminated() or ConsoleWaitForEnterKey()
// - you can call this method several times with no issue
// - onLog can be assigned from TSynLog.DoLog for proper logging
procedure SynDaemonIntercept(const onlog: TSynLogProc = nil);

/// disable SIGPIPE signal for the current process
// - is called e.g. by NewOpenSslNetTls since the OpenSsl TLS layer does not
// (yet) use MSG_NOSIGNAL when accessing the socket
procedure SigPipeIntercept;

{$endif OSWINDOWS}

/// change the current UID/GID to another user, by name
// - only implemented on POSIX by now
function DropPriviledges(const UserName: RawUtf8 = 'nobody'): boolean;

/// changes the root directory of the calling process
// - only implemented on POSIX by now
function ChangeRoot(const FolderName: RawUtf8): boolean;

type
  /// command line patterns recognized by ParseCommandArgs()
  TParseCommand = (
    pcHasRedirection,
    pcHasSubCommand,
    pcHasParenthesis,
    pcHasJobControl,
    pcHasWildcard,
    pcHasShellVariable,
    pcUnbalancedSingleQuote,
    pcUnbalancedDoubleQuote,
    pcTooManyArguments,
    pcInvalidCommand,
    pcHasEndingBackSlash);

  TParseCommands = set of TParseCommand;
  PParseCommands = ^TParseCommands;

  /// used to store references of arguments recognized by ParseCommandArgs()
  TParseCommandsArgs = array[0..31] of PAnsiChar;
  PParseCommandsArgs = ^TParseCommandsArgs;

const
  /// identifies some bash-specific processing
  PARSECOMMAND_BASH =
    [pcHasRedirection .. pcHasShellVariable];

  /// identifies obvious invalid content
  PARSECOMMAND_ERROR =
    [pcUnbalancedSingleQuote .. pcHasEndingBackSlash];

  PARSCOMMAND_POSIX = {$ifdef OSWINDOWS} false {$else} true {$endif};

/// low-level parsing of a RunCommand() execution command
// - parse and fills argv^[0..argc^-1] with corresponding arguments, after
// un-escaping and un-quoting if applicable, using temp^ to store the content
// - if argv=nil, do only the parsing, not the argument extraction - could be
// used for fast validation of the command line syntax
// - you can force arguments OS flavor using the posix parameter - note that
// Windows parsing is not consistent by itself (e.g. double quoting or
// escaping depends on the actual executable called) so returned flags
// should be considered as indicative only with posix=false
function ParseCommandArgs(const cmd: RawUtf8; argv: PParseCommandsArgs = nil;
  argc: PInteger = nil; temp: PRawUtf8 = nil;
  posix: boolean = PARSCOMMAND_POSIX): TParseCommands;

/// low-level extration of the executable of a RunCommand() execution command
// - returns the first parameter returned by ParseCommandArgs()
function ExtractExecutableName(const cmd: RawUtf8;
  posix: boolean = PARSCOMMAND_POSIX): RawUtf8;

type
  /// callback used by RunRedirect() to notify of console output at runtime
  // - newly console output text is given as raw bytes sent by the application,
  // with no conversion: on POSIX, it is likely to be UTF-8 but on Windows it
  // depends on the actual program so is likely to be CP_OEM but others could
  // use the system code page or even UTF-16 binary with BOM (!) - so you
  // may consider using AnsiToUtf8() with the proper code page
  // - should return true to stop the execution, or false to continue
  // - is called once when the process is started, with text='', ignoring its return
  // - on idle state (each 200ms), is called with text='' to allow execution abort
  // - the raw process ID (dword on Windows, cint on POSIX) is also supplied
  TOnRedirect = function(const text: RawByteString; pid: cardinal): boolean of object;

  /// define how RunCommand() and RunRedirect() run their sub-process
  // - roEnvAddExisting is used when the env pairs should be added to the
  // existing system environment variable
  // - roWinJobCloseChildren will setup a Windows Job to close any child
  // process(es) when the created process quits
  // - roWinNoProcessDetach will avoid creating a Windows sub-process and group
  TRunOptions = set of (
    roEnvAddExisting,
    roWinJobCloseChildren,
    roWinNoProcessDetach);

/// like SysUtils.ExecuteProcess, but allowing not to wait for the process to finish
// - optional env value follows 'n1=v1'#0'n2=v2'#0'n3=v3'#0#0 Windows layout
function RunProcess(const path, arg1: TFileName; waitfor: boolean;
  const arg2: TFileName = ''; const arg3: TFileName = '';
  const arg4: TFileName = ''; const arg5: TFileName = '';
  const env: TFileName = ''; options: TRunOptions = []): integer;

/// like fpSystem, but cross-platform
// - under POSIX, calls bash only if needed, after ParseCommandArgs() analysis
// - under Windows (especially Windows 10), creating a process can be dead slow
// https://randomascii.wordpress.com/2019/04/21/on2-in-createprocess
// - waitfordelayms/processhandle/redirected/onoutput exist on Windows only -
// and redirected is the raw byte output, which may be OEM, WinAnsi or UTF-16
// depending on the program itself
// - parsed is implemented on POSIX only
// - optional env should be encoded as 'n1=v1'#0'n2=v2'#0#0 pairs
function RunCommand(const cmd: TFileName; waitfor: boolean;
  const env: TFileName = ''; options: TRunOptions = [];
  {$ifdef OSWINDOWS}
  waitfordelayms: cardinal = INFINITE; processhandle: PHandle = nil;
  redirected: PRawByteString = nil; const onoutput: TOnRedirect = nil;
  const wrkdir: TFileName = ''
  {$else}
  parsed: PParseCommands = nil
  {$endif OSWINDOWS}): integer;

/// execute a command, returning its output console as UTF-8 text
// - calling CreateProcessW on Windows (i.e. our RunCommand), and FPC RTL
// popen/pclose on POSIX
// - return '' on cmd execution error, or the whole output console content
// with no conversion: on POSIX, it is likely to be UTF-8 but on Windows it
// depends on the actual program so is likely to be CP_OEM but others could
// use the system code page or even UTF-16 binary with BOM (!) - so you
// may consider using AnsiToUtf8() with the proper code page
// - will optionally call onoutput() to notify the new output state
// - aborts if onoutput() callback returns true, or waitfordelayms expires
// - optional env is Windows only, (FPC popen does not support it), and should
// be encoded as name=value#0 pairs
// - you can specify a wrkdir if the path specified by cmd is not good enough
function RunRedirect(const cmd: TFileName; exitcode: PInteger = nil;
  const onoutput: TOnRedirect = nil; waitfordelayms: cardinal = INFINITE;
  setresult: boolean = true; const env: TFileName = '';
  const wrkdir: TFileName = ''; options: TRunOptions = []): RawByteString;

var
  /// how many seconds we should wait for gracefull termination of a process
  // in RunRedirect() - or RunCommand() on Windows
  // - set 0 to disable gracefull exit, and force hard SIGKILL/TerminateProcess
  RunAbortTimeoutSecs: integer = 5;

{$ifdef OSWINDOWS}
type
  /// how RunRedirect() or RunCommand() should try to gracefully terminate
  // - ramCtrlC calls CancelProcess(), i.e. send CTRL_C_EVENT
  // - ramQuit calls QuitProcess(), i.e. send WM_QUIT on all the process threads
  // - note that TerminateProcess is always called after RunAbortTimeoutSecs
  // timeout, or if this set of methods is void
  TRunAbortMethods = set of (ramCtrlC, ramQuit);
var
  /// RunRedirect/RunCommand methods to gracefully terminate before TerminateProcess
  RunAbortMethods: TRunAbortMethods = [ramCtrlC, ramQuit];
{$else}
type
  /// how RunRedirect() should try to gracefully terminate
  // - ramSigTerm send a fpkill(pid, SIGTERM) to the process
  // - note that SIGKILL is always sent after RunAbortTimeoutSecs timeout,
  // or if ramSigTerm was not supplied
  TRunAbortMethods = set of (ramSigTerm);
var
  /// RunRedirect() methods to gracefully terminate before SIGKILL
  RunAbortMethods: TRunAbortMethods = [ramSigTerm];
{$endif OSWINDOWS}


implementation

// those include files hold all OS-specific functions
// note: the *.inc files start with their own "uses" clause, so both $include
// should remain here, just after the "implementation" clause

{$ifdef OSPOSIX}
  {$include mormot.core.os.posix.inc}
{$endif OSPOSIX}

{$ifdef OSWINDOWS}
  {$include mormot.core.os.windows.inc}
{$endif OSWINDOWS}


{ ****************** Some Cross-System Type and Constant Definitions }

const
  // StatusCodeToReason() StatusCodeToText() table to avoid memory allocations
  // - roughly sorted by actual usage order for WordScanIndex()
  HTTP_REASON: array[0..43] of RawUtf8 = (
   'OK',                                // HTTP_SUCCESS - should be first
   'No Content',                        // HTTP_NOCONTENT
   'Temporary Redirect',                // HTTP_TEMPORARYREDIRECT
   'Permanent Redirect',                // HTTP_PERMANENTREDIRECT
   'Moved Permanently',                 // HTTP_MOVEDPERMANENTLY
   'Bad Request',                       // HTTP_BADREQUEST
   'Unauthorized',                      // HTTP_UNAUTHORIZED
   'Forbidden',                         // HTTP_FORBIDDEN
   'Not Found',                         // HTTP_NOTFOUND
   'Method Not Allowed',                // HTTP_NOTALLOWED
   'Not Modified',                      // HTTP_NOTMODIFIED
   'Not Acceptable',                    // HTTP_NOTACCEPTABLE
   'Partial Content',                   // HTTP_PARTIALCONTENT
   'Payload Too Large',                 // HTTP_PAYLOADTOOLARGE
   'Created',                           // HTTP_CREATED
   'See Other',                         // HTTP_SEEOTHER
   'Continue',                          // HTTP_CONTINUE
   'Switching Protocols',               // HTTP_SWITCHINGPROTOCOLS
   'Accepted',                          // HTTP_ACCEPTED
   'Non-Authoritative Information',     // HTTP_NONAUTHORIZEDINFO
   'Reset Content',                     // HTTP_RESETCONTENT
   'Multi-Status',                      // 207
   'Multiple Choices',                  // HTTP_MULTIPLECHOICES
   'Found',                             // HTTP_FOUND
   'Use Proxy',                         // HTTP_USEPROXY
   'Proxy Authentication Required',     // HTTP_PROXYAUTHREQUIRED
   'Request Timeout',                   // HTTP_TIMEOUT
   'Conflict',                          // HTTP_CONFLICT
   'Gone',                              // 410
   'Length Required',                   // 411
   'Precondition Failed',               // 412
   'URI Too Long',                      // 414
   'Unsupported Media Type',            // 415
   'Requested Range Not Satisfiable',   // HTTP_RANGENOTSATISFIABLE
   'I''m a teapot',                     // HTTP_TEAPOT
   'Upgrade Required',                  // 426
   'Internal Server Error',             // HTTP_SERVERERROR
   'Not Implemented',                   // HTTP_NOTIMPLEMENTED
   'Bad Gateway',                       // HTTP_BADGATEWAY
   'Service Unavailable',               // HTTP_UNAVAILABLE
   'Gateway Timeout',                   // HTTP_GATEWAYTIMEOUT
   'HTTP Version Not Supported',        // HTTP_HTTPVERSIONNONSUPPORTED
   'Network Authentication Required',   // 511
   'Invalid Request');                  // 513 - should be last

  HTTP_CODE: array[0..43] of word = (
    HTTP_SUCCESS,
    HTTP_NOCONTENT,
    HTTP_TEMPORARYREDIRECT,
    HTTP_PERMANENTREDIRECT,
    HTTP_MOVEDPERMANENTLY,
    HTTP_BADREQUEST,
    HTTP_UNAUTHORIZED,
    HTTP_FORBIDDEN,
    HTTP_NOTFOUND,
    HTTP_NOTALLOWED,
    HTTP_NOTMODIFIED,
    HTTP_NOTACCEPTABLE,
    HTTP_PARTIALCONTENT,
    HTTP_PAYLOADTOOLARGE,
    HTTP_CREATED,
    HTTP_SEEOTHER,
    HTTP_CONTINUE,
    HTTP_SWITCHINGPROTOCOLS,
    HTTP_ACCEPTED,
    HTTP_NONAUTHORIZEDINFO,
    HTTP_RESETCONTENT,
    207,
    HTTP_MULTIPLECHOICES,
    HTTP_FOUND,
    HTTP_USEPROXY,
    HTTP_PROXYAUTHREQUIRED,
    HTTP_TIMEOUT,
    HTTP_CONFLICT,
    410,
    411,
    412,
    414,
    415,
    HTTP_RANGENOTSATISFIABLE,
    HTTP_TEAPOT,
    426,
    HTTP_SERVERERROR,
    HTTP_NOTIMPLEMENTED,
    HTTP_BADGATEWAY,
    HTTP_UNAVAILABLE,
    HTTP_GATEWAYTIMEOUT,
    HTTP_HTTPVERSIONNONSUPPORTED,
    511,
    513);

function StatusCodeToText(Code: cardinal): PRawUtf8;
var
  i: PtrInt;
begin
  if Code <> 200 then // optimistic approach :)
    if (Code < 513) and
       (Code >= 100) then
    begin
      i := WordScanIndex(@HTTP_CODE, length(HTTP_CODE), Code); // may use SSE2
      if i < 0 then
        i := high(HTTP_CODE); // returns cached 513 'Invalid Request'
    end
    else
      i := high(HTTP_CODE)
  else
    i := 0;
  result := @HTTP_REASON[i];
end;

procedure StatusCodeToReason(Code: cardinal; var Reason: RawUtf8);
begin
  Reason := StatusCodeToText(Code)^;
end;

function StatusCodeToShort(Code: cardinal): TShort47;
begin
  if Code > 599 then
    Code := 999; // ensure stay in TShort47
  result[0] := #0;
  AppendShortCardinal(Code, result);
  AppendShortChar(' ', result);
  AppendShortAnsi7String(StatusCodeToText(Code)^, result);
end;

function StatusCodeIsSuccess(Code: integer): boolean;
begin
  result := (Code >= HTTP_SUCCESS) and
            (Code < HTTP_BADREQUEST); // 200..399
end;

function IsInvalidHttpHeader(head: PUtf8Char; headlen: PtrInt): boolean;
var
  i: PtrInt;
  c: cardinal;
begin
  result := true;
  for i := 0 to headlen - 3 do
  begin
    c := PCardinal(head + i)^;
    if (c = $0a0d0a0d) or
       (Word(c) = $0d0d) or
       (Word(c) = $0a0a) then
      exit;
  end;
  result := false;
end;

function _oskb(Size: QWord): shortstring;
const
  _U: array[0..3] of AnsiChar = 'TGMK';
var
  u: PtrInt;
  b: QWord;
begin
  u := 0;
  b := Qword(1) shl 40;
  repeat
    if Size > b shr 1 then
      break;
    b := b shr 10;
    inc(u);
  until u = high(_u);
  str(Size / b : 1 : 1, result); // let the FPU + RTL do the conversion for us
  if (result[0] <= #2) or
     (result[ord(result[0]) - 1] <> '.') or
     (result[ord(result[0])] <> '0') then
    inc(result[0], 2);
  result[ord(result[0]) - 1] := _U[u];
  result[ord(result[0])] := 'B';
end;

function SidLength(sid: PSid): PtrInt;
begin
  if sid = nil then
    result := 0
  else
    result := integer(sid^.SubAuthorityCount) shl 2 + 8;
end;

function SidCompare(a, b: PSid): integer;
var
  l: PtrInt;
begin
  l := SidLength(a);
  result := l - SidLength(b);
  if result = 0 then
    result := MemCmp(pointer(a), pointer(b), l);
end;

procedure ToRawSid(sid: PSid; out result: RawSid);
begin
  if sid <> nil then
    FastSetRawByteString(RawByteString(result), sid, SidLength(sid));
end;

procedure SidToTextShort(sid: PSid; var result: shortstring);
var
  a: PSidAuth;
  i: PtrInt;
begin // faster than ConvertSidToStringSidA(), and cross-platform
  if (sid = nil ) or
     (sid^.Revision <> 1) then
  begin
    result[0] := #0; // invalid SID
    exit;
  end;
  a := @sid^.IdentifierAuthority;
  if (a^[0] <> 0) or
     (a^[1] <> 0) then
  begin
    result := 'S-1-0x';
    for i := 0 to 5 do
      AppendShortByteHex(a^[i], result)
  end
  else
  begin
    result := 'S-1-';
    AppendShortCardinal(bswap32(PCardinal(@a^[2])^), result);
  end;
  for i := 0 to integer(sid^.SubAuthorityCount) - 1 do
  begin
    AppendShortChar('-', result);
    AppendShortCardinal(sid^.SubAuthority[i], result);
  end;
end;

function SidToText(sid: PSid): RawUtf8;
var
  tmp: shortstring;
begin
  SidToTextShort(sid, tmp);
  FastSetString(result, @tmp[1], ord(tmp[0]));
end;

function SidsToText(sids: PSids): TRawUtf8DynArray;
var
  i: PtrInt;
begin
  result := nil;
  SetLength(result, length(sids));
  for i := 0 to length(sids) - 1 do
    result[i] := SidToText(sids[i]);
end;

function IsValidRawSid(const sid: RawSid): boolean;
var
  l: PtrInt;
begin
  l := length(sid);
  result := (l >= SizeOf(TSidAuth) + 2) and
            (SidLength(pointer(sid)) = l)
end;

function HasSid(const sids: PSids; sid: PSid): boolean;
var
  i: PtrInt;
begin
  result := true;
  if sid <> nil then
    for i := 0 to length(sids) - 1 do
      if SidCompare(sid, sids[i]) = 0 then
        exit;
  result := false;
end;

function HasAnySid(const sids: PSids; const sid: RawSidDynArray): boolean;
var
  i: PtrInt;
begin
  result := true;
  for i := 0 to length(sid) - 1 do
    if HasSid(sids, pointer(sid[i])) then
      exit;
  result := false;
end;

procedure AddRawSid(var sids: RawSidDynArray; sid: PSid);
var
  n: PtrInt;
begin
  if sid = nil then
    exit;
  n := length(sids);
  SetLength(sids, n + 1);
  ToRawSid(sid, sids[n]);
end;

function RawSidToText(const sid: RawSid): RawUtf8;
begin
  if IsValidRawSid(sid) then
    result := SidToText(pointer(sid))
  else
    result := '';
end;

// GetNextCardinal() on POSIX does not ignore trailing '-'
function GetNextUInt32(var P: PUtf8Char): cardinal;
var
  c: cardinal;
begin
  result := 0;
  if P = nil then
    exit;
  repeat
    c := ord(P^) - 48;
    if c > 9 then
      break
    else
      result := result * 10 + c;
    inc(P);
  until false;
  while P^ in ['.', '-', ' '] do
    inc(P);
end;

function TextToSid(P: PUtf8Char; out sid: TSid): boolean;
begin
  result := false;
  if (P = nil) or
     (PCardinal(P)^ <>
        ord('S') + ord('-') shl 8 + ord('1') shl 16 + ord('-') shl 24) then
    exit;
  inc(P, 4);
  if not (P^ in ['1'..'9']) then
    exit;
  PInt64(@sid)^ := 1;
  PCardinal(@sid.IdentifierAuthority[2])^ := bswap32(GetNextUInt32(P));
  while P^ in ['0'..'9'] do
  begin
    sid.SubAuthority[sid.SubAuthorityCount] := GetNextUInt32(P);
    inc(sid.SubAuthorityCount);
    if sid.SubAuthorityCount = 0 then
      exit; // avoid any overflow
  end;
  result := P^ = #0
end;

function TextToRawSid(const text: RawUtf8): RawSid;
begin
  TextToRawSid(text, result);
end;

function TextToRawSid(const text: RawUtf8; out sid: RawSid): boolean;
var
  tmp: TSid; // maximum size possible on stack (1032 bytes)
begin
  result := TextToSid(pointer(text), tmp);
  if result then
    ToRawSid(@tmp, sid)
end;

var
  KNOWN_SID_SAFE: TLightLock; // lighter than GlobalLock/GlobalUnLock
  KNOWN_SID: array[TWellKnownSid] of RawSid;
  KNOWN_SID_TEXT: array[TWellKnownSid] of string[15];
const
  INTEGRITY_SID: array[0..7] of word = ( // S-1-16-x known values
    0, 4096, 8192, 8448, 12288, 16384, 20480, 28672);

procedure ComputeKnownSid(wks: TWellKnownSid);
var
  sid: TSid;
begin
  PInt64(@sid)^ := $0101; // sid.Revision=1, sid.SubAuthorityCount=1
  if wks <= wksLocal then
  begin // S-1-1-0
    sid.IdentifierAuthority[5] := ord(wks);
    sid.SubAuthority[0] := 0;
  end
  else if wks = wksConsoleLogon then
  begin // S-1-2-1
    sid.IdentifierAuthority[5] := 2;
    sid.SubAuthority[0] := 1;
  end
  else if wks <= wksCreatorGroupServer then
  begin // S-1-3-0
    sid.IdentifierAuthority[5] := 3;
    sid.SubAuthority[0] := ord(wks) - ord(wksCreatorOwner);
  end
  else if wks <= wksIntegritySecureProcess then
  begin
    sid.IdentifierAuthority[5] := 16; // S-1-16-x
    sid.SubAuthority[0] := INTEGRITY_SID[ord(wks) - ord(wksIntegrityUntrusted)];
  end
  else if wks <= wksAuthenticationKeyPropertyAttestation then
  begin // S-1-18-1
    sid.IdentifierAuthority[5] := 18;
    sid.SubAuthority[0] := ord(wks) - (ord(wksAuthenticationAuthorityAsserted) - 1)
  end
  else
  begin // S-1-5-x
    sid.IdentifierAuthority[5] := 5;
    if wks = wksNtAuthority then
      sid.SubAuthorityCount := 0
    else if wks <= wksInteractive then
      sid.SubAuthority[0] := ord(wks) - ord(wksNtAuthority)
    else if wks <= wksThisOrganisation then
      sid.SubAuthority[0] := ord(wks) - (ord(wksNtAuthority) - 1)
    else if wks <= wksNetworkService then
      sid.SubAuthority[0] := ord(wks) - (ord(wksNtAuthority) - 2)
    else if wks <= wksLocalAccountAndAdministrator then //  S-1-5-113
      sid.SubAuthority[0] := ord(wks) - (ord(wksLocalAccount) - 113)
    else
    begin
      sid.SubAuthority[0] := 32;
      if wks <> wksBuiltinDomain then
      begin
        sid.SubAuthorityCount := 2;
        if wks <= wksBuiltinDcomUsers then
          sid.SubAuthority[1] := ord(wks) - (ord(wksBuiltinAdministrators) - 544)
        else if wks <= wksBuiltinDeviceOwners then // S-1-5-32-583
          sid.SubAuthority[1] := ord(wks) - (ord(wksBuiltinIUsers) - 568)
        else if wks <= wksCapabilityContacts then
        begin // S-1-15-3-1
          sid.IdentifierAuthority[5] := 15;
          sid.SubAuthority[0] := 3;
          sid.SubAuthority[1] := ord(wks) - (ord(wksCapabilityInternetClient) - 1)
        end
        else if wks <= wksBuiltinAnyRestrictedPackage then
        begin // S-1-15-2-1
          sid.IdentifierAuthority[5] := 15;
          sid.SubAuthority[0] := 2;
          sid.SubAuthority[1] := ord(wks) - (ord(wksBuiltinAnyPackage) - 1)
        end
        else if wks <= wksDigestAuthentication then
        begin
          sid.SubAuthority[0] := 64;
          case wks of
            wksNtlmAuthentication:
              sid.SubAuthority[1] := 10; // S-1-5-64-10
            wksSChannelAuthentication:
              sid.SubAuthority[1] := 14;
            wksDigestAuthentication:
              sid.SubAuthority[1] := 21;
          end;
        end;
      end;
    end;
  end;
  KNOWN_SID_SAFE.Lock;
  if KNOWN_SID[wks] = '' then
  begin
    SidToTextShort(@sid, KNOWN_SID_TEXT[wks]);
    ToRawSid(@sid, KNOWN_SID[wks]); // to be set last
  end;
  KNOWN_SID_SAFE.UnLock;
end;

function KnownRawSid(wks: TWellKnownSid): RawSid;
begin
  if (wks <> wksNull) and
     (KNOWN_SID[wks] = '') then
    ComputeKnownSid(wks);
  result := KNOWN_SID[wks];
end;

function KnownSidToText(wks: TWellKnownSid): PShortString;
begin
  if (wks <> wksNull) and
     (KNOWN_SID[wks] = '') then
    ComputeKnownSid(wks);
  result := @KNOWN_SID_TEXT[wks];
end;

// https://learn.microsoft.com/en-us/windows/win32/secauthz/well-known-sids
// https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dtyp/81d92bba-d22b-4a8c-908a-554ab29148ab

function SidToKnown(sid: PSid): TWellKnownSid;
var
  c: integer;
begin
  result := wksNull; // not recognized
  if (sid = nil) or
     (sid.Revision <> 1) or
     (PCardinal(@sid.IdentifierAuthority)^ <> 0) or
     (sid.IdentifierAuthority[4] <> 0) then
    exit;
  case sid.SubAuthorityCount of // very fast O(1) SID binary recognition
    0:
      if sid.IdentifierAuthority[5] = 5 then
        result := wksNtAuthority; // S-1-5
    1:
      begin
        c := sid.SubAuthority[0];
        case sid.IdentifierAuthority[5] of
          1:
            if c = 0 then
              result := wksWorld; // S-1-1-0
          2:
            if c in [0 .. 1] then // S-1-2-x
              result := TWellKnownSid(ord(wksLocal) + c);
          3:
            if c in [0 .. 3] then // S-1-3-x
              result := TWellKnownSid(ord(wksCreatorOwner) + c);
          5:
            case c of // S-1-5-x
              1 .. 4:
                result := TWellKnownSid((ord(wksDialup) - 1) + c);
              6 .. 15:
                result := TWellKnownSid((ord(wksService) - 6) + c);
              17 .. 20:
                result := TWellKnownSid((ord(wksIisUser) - 17) + c);
              32:
                result := wksBuiltinDomain;
              113 .. 114:
                result := TWellKnownSid(integer(ord(wksLocalAccount) - 113) + c);
            end;
          16:
            begin // S-1-16-x
              c := WordScanIndex(@INTEGRITY_SID, length(INTEGRITY_SID), c);
              if c >= 0 then
                result := TWellKnownSid(ord(wksIntegrityUntrusted) + c);
            end;
          18:
            if c in [1 .. 6] then // S-1-18-x
              result :=
                TWellKnownSid((ord(wksAuthenticationAuthorityAsserted) - 1) + c);
        end;
      end;
    2:
      begin
        c := sid.SubAuthority[1];
        case sid.IdentifierAuthority[5] of
          5:
            case sid.SubAuthority[0] of
              32: // S-1-5-32-544
                case c of
                  544 .. 562:
                    result := TWellKnownSid(ord(wksBuiltinAdministrators) + c - 544);
                  568 .. 583:
                    result := TWellKnownSid(ord(wksBuiltinIUsers) + c - 568);
                end;
              64: // S-1-5-64-10
                case c of
                  10:
                    result := wksNtlmAuthentication;
                  14:
                    result := wksSChannelAuthentication;
                  21:
                    result := wksDigestAuthentication;
                end;
            end;
          15:
            case sid.SubAuthority[0] of
              2:
                if c in [1 .. 2] then // S-1-15-2-x
                  result := TWellKnownSid(ord(pred(wksBuiltinAnyPackage)) + c);
              3:
                if c in [1 .. 12] then // S-1-15-3-x
                  result := TWellKnownSid(ord(pred(wksCapabilityInternetClient)) + c);
            end;
        end;
      end;
  end;
end;

function SidToKnown(const text: RawUtf8): TWellKnownSid;
var
  sid: TSid;
begin
  if TextToSid(pointer(text), sid) then
    result := SidToKnown(@sid)
  else
    result := wksNull;
end;

function SidToKnownGroups(const sids: PSids): TWellKnownSids;
var
  k: TWellKnownSid;
  i: PtrInt;
begin
  result := [];
  for i := 0 to length(sids) - 1 do
  begin
    k := SidToKnown(sids[i]);
    if k <> wksNull then
      include(result, k);
  end;
end;


{ ****************** Gather Operating System Information }

function ToText(const osv: TOperatingSystemVersion): RawUtf8;
begin
  result := OS_NAME[osv.os];
  case osv.os of
    osWindows:
      result := 'Windows ' + WINDOWS_NAME[osv.win];
    osOSX:
      if osv.utsrelease[2] in [low(MACOS_NAME) .. high(MACOS_NAME)] then
        result := 'macOS ' + MACOS_NAME[osv.utsrelease[2]];
  end;
end;

function ToTextShort(const osv: TOperatingSystemVersion): RawUtf8;
begin
  result := OS_NAME[osv.os];
  case osv.os of
    osWindows:
      result := WINDOWS_NAME[osv.win];
    osOSX:
      if osv.utsrelease[2] in [low(MACOS_NAME) .. high(MACOS_NAME)] then
        result := MACOS_NAME[osv.utsrelease[2]];
  end;
end;

const
  LINUX_TEXT: array[boolean] of string[7] = (
    '', 'Linux ');

function ToTextOS(osint32: integer): RawUtf8;
var
  osv: TOperatingSystemVersion absolute osint32;
begin
  if osint32 = 0 then
  begin
    result := '';
    exit;
  end;
  result := ToText(osv);
  if (osv.os = osWindows) and
     (osv.winbuild <> 0) then
    // include the Windows build number, e.g. 'Windows 11 64bit 22000'
    result := _fmt('%s %d', [result, osv.winbuild]);
  if (osv.os >= osLinux) and
     (osv.utsrelease[2] <> 0) then
    // include kernel number to the distribution name, e.g. 'Ubuntu Linux 5.4.0'
    result := _fmt('%s %s%d.%d.%d', [result, LINUX_TEXT[osv.os in OS_LINUX],
      osv.utsrelease[2], osv.utsrelease[1], osv.utsrelease[0]]);
end;

function MatchOS(os: TOperatingSystem): boolean;
var
  current: TOperatingSystem;
begin
  current := OS_KIND;
  if (os = osUnknown) or
     (current = osUnknown) or
     (os = current) then
    result := true // exact match
  else
    case os of // search by family
      osPosix:
        result := current <> osWindows;
      osLinux:
        result := current in OS_LINUX;
    else
      result := false;
    end;
end;

const
  // https://github.com/karelzak/util-linux/blob/master/sys-utils/lscpu-arm.c
  ARMCPU_ID: array[TArmCpuType] of word = (
    0,      // actUnknown
    $0810,  // actARM810
    $0920,  // actARM920
    $0922,  // actARM922
    $0926,  // actARM926
    $0940,  // actARM940
    $0946,  // actARM946
    $0966,  // actARM966
    $0a20,  // actARM1020
    $0a22,  // actARM1022
    $0a26,  // actARM1026
    $0b02,  // actARM11MPCore
    $0b36,  // actARM1136
    $0b56,  // actARM1156
    $0b76,  // actARM1176
    $0c05,  // actCortexA5
    $0c07,  // actCortexA7
    $0c08,  // actCortexA8
    $0c09,  // actCortexA9
    $0c0d,  // actCortexA12
    $0c0f,  // actCortexA15
    $0c0e,  // actCortexA17
    $0c14,  // actCortexR4
    $0c15,  // actCortexR5
    $0c17,  // actCortexR7
    $0c18,  // actCortexR8
    $0c20,  // actCortexM0
    $0c21,  // actCortexM1
    $0c23,  // actCortexM3
    $0c24,  // actCortexM4
    $0c27,  // actCortexM7
    $0c60,  // actCortexM0P
    $0d01,  // actCortexA32
    $0d03,  // actCortexA53
    $0d04,  // actCortexA35
    $0d05,  // actCortexA55
    $0d06,  // actCortexA65
    $0d07,  // actCortexA57
    $0d08,  // actCortexA72
    $0d09,  // actCortexA73
    $0d0a,  // actCortexA75
    $0d0b,  // actCortexA76
    $0d0c,  // actNeoverseN1
    $0d0d,  // actCortexA77
    $0d0e,  // actCortexA76AE
    $0d13,  // actCortexR52
    $0d20,  // actCortexM23
    $0d21,  // actCortexM33
    $0d40,  // actNeoverseV1
    $0d41,  // actCortexA78
    $0d42,  // actCortexA78AE
    $0d44,  // actCortexX1
    $0d46,  // actCortex510
    $0d47,  // actCortex710
    $0d48,  // actCortexX2
    $0d49,  // actNeoverseN2
    $0d4a,  // actNeoverseE1
    $0d4b,  // actCortexA78C
    $0d4c,  // actCortexX1C
    $0d4d,  // actCortexA715
    $0d4e,  // actCortexX3
    $0d4f,  // actNeoverseV2
    $0d80,  // actCortexA520
    $0d81,  // actCortexA720
    $0d82,  // actCortexX4
    $0d84,  // actNeoverseV3
    $0d8e); // actNeoverseN3

  ARMCPU_IMPL: array[TArmCpuImplementer] of byte = (
    0,    // aciUnknown
    $41,  // aciARM
    $42,  // aciBroadcom
    $43,  // aciCavium
    $44,  // aciDEC
    $46,  // aciFUJITSU
    $48,  // aciHiSilicon
    $49,  // aciInfineon
    $4d,  // aciMotorola
    $4e,  // aciNVIDIA
    $50,  // aciAPM
    $51,  // aciQualcomm
    $53,  // aciSamsung
    $56,  // aciMarvell
    $61,  // aciApple
    $66,  // aciFaraday
    $69,  // aciIntel
    $6d,  // aciMicrosoft
    $70,  // aciPhytium
    $c0); // aciAmpere

  ARMCPU_ID_TXT: array[TArmCpuType] of string[15] = (
     '',
     'ARM810', 'ARM920', 'ARM922', 'ARM926', 'ARM940', 'ARM946', 'ARM966',
     'ARM1020', 'ARM1022', 'ARM1026', 'ARM11 MPCore', 'ARM1136', 'ARM1156',
     'ARM1176', 'Cortex-A5', 'Cortex-A7', 'Cortex-A8', 'Cortex-A9',
     'Cortex-A17',{Originally A12} 'Cortex-A15', 'Cortex-A17', 'Cortex-R4',
     'Cortex-R5', 'Cortex-R7', 'Cortex-R8', 'Cortex-M0', 'Cortex-M1',
     'Cortex-M3', 'Cortex-M4', 'Cortex-M7', 'Cortex-M0+', 'Cortex-A32',
     'Cortex-A53', 'Cortex-A35', 'Cortex-A55', 'Cortex-A65', 'Cortex-A57',
     'Cortex-A72', 'Cortex-A73', 'Cortex-A75', 'Cortex-A76', 'Neoverse-N1',
     'Cortex-A77', 'Cortex-A76AE', 'Cortex-R52', 'Cortex-M23', 'Cortex-M33',
     'Neoverse-V1', 'Cortex-A78', 'Cortex-A78AE', 'Cortex-X1', 'Cortex-510',
     'Cortex-710', 'Cortex-X2', 'Neoverse-N2', 'Neoverse-E1', 'Cortex-A78C',
     'Cortex-X1C', 'Cortex-A715', 'Cortex-X3', 'Neoverse-V2', 'Cortex-A520',
     'Cortex-A720', 'Cortex-X4', 'Neoverse-V3', 'Neoverse-N3');
  ARMCPU_IMPL_TXT: array[TArmCpuImplementer] of string[18] = (
      '',
      'ARM', 'Broadcom', 'Cavium', 'DEC', 'FUJITSU', 'HiSilicon', 'Infineon',
      'Motorola/Freescale', 'NVIDIA', 'APM', 'Qualcomm', 'Samsung', 'Marvell',
      'Apple', 'Faraday', 'Intel', 'Microsoft', 'Phytium', 'Ampere');

function ArmCpuType(id: word): TArmCpuType;
begin
  for result := low(TArmCpuType) to high(TArmCpuType) do
    if ARMCPU_ID[result] = id then
      exit;
  result := actUnknown;
end;

function ArmCpuTypeName(act: TArmCpuType; id: word): RawUtf8;
begin
  if act = actUnknown then
    result := 'ARM 0x' + RawUtf8(IntToHex(id, 3))
  else
    ShortStringToAnsi7String(ARMCPU_ID_TXT[act], result);
end;

function ArmCpuImplementer(id: byte): TArmCpuImplementer;
begin
  for result := low(TArmCpuImplementer) to high(TArmCpuImplementer) do
    if ARMCPU_IMPL[result] = id then
      exit;
  result := aciUnknown;
end;

function ArmCpuImplementerName(aci: TArmCpuImplementer; id: word): RawUtf8;
begin
  if aci = aciUnknown then
    result := 'HW 0x' + RawUtf8(IntToHex(id, 2))
  else
    ShortStringToAnsi7String(ARMCPU_IMPL_TXT[aci], result);
end;


{ *************** Per Class Properties O(1) Lookup via vmtAutoTable Slot }

procedure PatchCodePtrUInt(Code: PPtrUInt; Value: PtrUInt; LeaveUnprotected: boolean);
begin
  PatchCode(Code, @Value, SizeOf(Code^), nil, LeaveUnprotected);
end;

{$ifdef CPUINTEL}
procedure RedirectCode(Func, RedirectFunc: Pointer);
var
  rel: PtrInt;
  NewJump: packed record
    Code: byte;        // $e9 = jmp {relative}
    Distance: integer; // relative jump is 32-bit even on CPU64
  end;
begin
  if (Func = nil) or
     (RedirectFunc = nil) or
     (Func = RedirectFunc) then
    exit; // nothing to redirect to
  NewJump.Code := $e9; // on both i386 and x86_64
  rel := PtrInt(PtrUInt(RedirectFunc) - PtrUInt(Func) - SizeOf(NewJump));
  NewJump.Distance := rel;
  {$ifdef CPU64}
  if NewJump.Distance <> rel then
    exit; // RedirectFunc is too far away from the original code :(
  {$endif CPU64}
  PatchCode(Func, @NewJump, SizeOf(NewJump));
  assert(PByte(Func)^ = $e9);
end;
{$endif CPUINTEL}



{ ************** Cross-Platform Charset and CodePage Support }

function CharSetToCodePage(CharSet: integer): cardinal;
begin
  case CharSet of
    SHIFTJIS_CHARSET:
      result := 932;
    HANGEUL_CHARSET:
      result := 949;
    GB2312_CHARSET:
      result := 936;
    HEBREW_CHARSET:
      result := 1255;
    ARABIC_CHARSET:
      result := 1256;
    GREEK_CHARSET:
      result := 1253;
    TURKISH_CHARSET:
      result := 1254;
    VIETNAMESE_CHARSET:
      result := 1258;
    THAI_CHARSET:
      result := 874;
    EASTEUROPE_CHARSET:
      result := 1250;
    RUSSIAN_CHARSET:
      result := 1251;
    BALTIC_CHARSET:
      result := 1257;
  else
    result := CP_WINANSI; // default ANSI_CHARSET = iso-8859-1 = windows-1252
  end;
end;

function CodePageToCharSet(CodePage: cardinal): integer;
begin
  case CodePage of
    932:
      result := SHIFTJIS_CHARSET;
    949:
      result := HANGEUL_CHARSET;
    936:
      result := GB2312_CHARSET;
    1255:
      result := HEBREW_CHARSET;
    1256:
      result := ARABIC_CHARSET;
    1253:
      result := GREEK_CHARSET;
    1254:
      result := TURKISH_CHARSET;
    1258:
      result := VIETNAMESE_CHARSET;
    874:
      result := THAI_CHARSET;
    1250:
      result := EASTEUROPE_CHARSET;
    1251:
      result := RUSSIAN_CHARSET;
    1257:
      result := BALTIC_CHARSET;
  else
    result := ANSI_CHARSET; // default is iso-8859-1 = windows-1252
  end;
end;


{ ****************** Unicode, Time, File, Console, Library process }

procedure InitializeCriticalSectionIfNeededAndEnter(var cs: TRTLCriticalSection);
begin
  if not IsInitializedCriticalSection(cs) then
    InitializeCriticalSection(cs);
  mormot.core.os.EnterCriticalSection(cs);
end;

procedure DeleteCriticalSectionIfNeeded(var cs: TRTLCriticalSection);
begin
  if IsInitializedCriticalSection(cs) then
    DeleteCriticalSection(cs);
end;

function Unicode_CodePage: integer;
begin
  {$ifdef FPC}
  // = GetSystemCodePage on POSIX, Lazarus may override to UTF-8 on Windows
  result := DefaultSystemCodePage;
  {$else}
  // Delphi always uses the main Windows System Code Page
  result := GetACP;
  {$endif FPC}
end;

function Unicode_CompareString(PW1, PW2: PWideChar; L1, L2: PtrInt;
  IgnoreCase: boolean): integer;
const
  _CASEFLAG: array[boolean] of DWORD = (0, NORM_IGNORECASE);
begin
  result := CompareStringW(LOCALE_USER_DEFAULT, _CASEFLAG[IgnoreCase], PW1, L1, PW2, L2);
end;

procedure Unicode_WideToShort(W: PWideChar; LW, CodePage: PtrInt;
  var res: ShortString);
var
  i: PtrInt;
begin
  if LW <= 0 then
    res[0] := #0
  else if (LW <= 255) and
          IsAnsiCompatibleW(W, LW) then
  begin
    // fast handling of pure English content
    res[0] := AnsiChar(LW);
    i := 1;
    repeat
      res[i] := AnsiChar(W^);
      if i = LW then
        break;
      inc(W);
      inc(i);
    until false;
  end
  else
    // use WinAPI, ICU or cwstring/RTL for accurate conversion
    res[0] := AnsiChar(
      Unicode_WideToAnsi(W, PAnsiChar(@res[1]), LW, 255, CodePage));
end;

function NowUtc: TDateTime;
begin
  result := UnixMSTimeUtcFast / Int64(MSecsPerDay) + Int64(UnixDelta);
end;

function DateTimeToWindowsFileTime(DateTime: TDateTime): integer;
var
  YY, MM, DD, H, m, s, ms: word;
begin
  DecodeDate(DateTime, YY, MM, DD);
  DecodeTime(DateTime, h, m, s, ms);
  if (YY < 1980) or
     (YY > 2099) then
    result := 0
  else
    result := (s shr 1) or (m shl 5) or (h shl 11) or
      cardinal((DD shl 16) or (MM shl 21) or (cardinal(YY - 1980) shl 25));
end;

function WindowsFileTimeToDateTime(WinTime: integer): TDateTime;
var
  date, time: TDateTime;
begin
  with PLongRec(@WinTime)^ do
  if TryEncodeDate(Hi shr 9 + 1980, Hi shr 5 and 15, Hi and 31, date) and
     TryEncodeTime(Lo shr 11, Lo shr 5 and 63, Lo and 31 shl 1, 0, time) then
    result := date + time
  else
    result := 0;
end;

const
  FileTimePerMs = 10000; // a tick is 100ns

function WindowsFileTime64ToUnixMSTime(WinTime: QWord): TUnixMSTime;
begin
  result := (Int64(WinTime) - UnixFileTimeDelta) div FileTimePerMs;
end;

function DirectorySize(const FileName: TFileName; Recursive: boolean;
  const Mask: TFileName): Int64;
var
  SR: TSearchRec;
  dir: TFileName;
begin
  result := 0;
  dir := IncludeTrailingPathDelimiter(FileName);
  if FindFirst(dir + Mask, faAnyFile, SR) <> 0 then
    exit;
  repeat
   if SearchRecValidFile(SR) then
     inc(result, SR.Size)
   else if Recursive and
           SearchRecValidFolder(SR) then
     inc(result, DirectorySize(dir + SR.Name, true));
  until FindNext(SR) <> 0;
  FindClose(SR);
end;

function SafePathName(const Path: TFileName): boolean;
var
  i, o: PtrInt;
begin
  if Path <> '' then
  begin
    result := false;
    if (Path[1] = '/') or
       (PosExString(':', Path) <> 0) or
       (PosExString('\\', Path) <> 0) then
      exit;
    o := 1;
    repeat
      i := PosExString('..', Path, o);
      if i = 0 then
        break;
      o := i + 2; // '..test' or 'test..' are valid folder names
      if cardinal(Path[o]) in [0, ord('\'), ord('/')] then
        if (i = 1) or
           (cardinal(Path[i - 1]) in [ord('\'), ord('/')]) then
          exit;
    until false;
  end;
  result := true;
end;

function SafePathNameU(const Path: RawUtf8): boolean;
var
  i, o: PtrInt;
begin
  if Path <> '' then
  begin
    result := false;
    if (Path[1] = '/') or
       (PosExChar(':', Path) <> 0) or
       (PosEx('\\', Path) <> 0) then
      exit;
    o := 1;
    repeat
      i := PosEx('..', Path, o);
      if i = 0 then
        break;
      o := i + 2;
      if Path[o] in [#0, '\', '/'] then
        if (i = 1) or
           (Path[i - 1] in ['\', '/']) then
          exit;
    until false;
  end;
  result := true;
end;

function SafeFileName(const FileName: TFileName): boolean;
begin
  result := SafePathName(ExtractPath(FileName));
end;

function SafeFileNameU(const FileName: RawUtf8): boolean;
begin
  result := SafePathNameU(ExtractPathU(FileName));
end;

function NormalizeFileName(const FileName: TFileName): TFileName;
begin
  result := StringReplace(FileName, InvertedPathDelim, PathDelim, [rfReplaceAll]);
end;

function QuoteFileName(const FileName: TFileName): TFileName;
begin
  if (FileName <> '') and
     (PosExString(' ', FileName) <> 0) and
     (FileName[1] <> '"') then
    result := '"' + FileName + '"'
  else
    result := FileName;
end;

procedure DisplayError(const fmt: string; const args: array of const);
var
  msg: string;
begin
  msg := Format(fmt, args);
  DisplayFatalError('', RawUtf8(msg));
end;

function SearchRecToDateTime(const F: TSearchRec): TDateTime;
begin
  {$ifdef ISDELPHIXE}
  result := F.Timestamp; // use new API
  {$else}
  result := FileDateToDateTime(F.Time);
  {$endif ISDELPHIXE}
end;

function SearchRecToDateTimeUtc(const F: TSearchRec): TDateTime;
begin
  result := SearchRecToUnixTimeUtc(F) / Int64(SecsPerDay) + Int64(UnixDelta);
end;

function SearchRecValidFile(const F: TSearchRec): boolean;
begin
  result := (F.Name <> '') and
            (F.Attr and faInvalidFile = 0);
end;

function SearchRecValidFolder(const F: TSearchRec): boolean;
begin
  result := (F.Attr and faDirectoryMask = faDirectory) and
            (F.Name <> '') and
            (F.Name <> '.') and
            (F.Name <> '..');
end;

{ TFileStreamFromHandle }

destructor TFileStreamFromHandle.Destroy;
begin
  if not fDontReleaseHandle then
    FileClose(Handle); // otherwise file remains opened (FPC RTL inconsistency)
end;

{ TFileStreamEx }

function TFileStreamEx.GetSize: Int64;
begin
  result := FileSize(Handle); // faster than 3 FileSeek() calls
end;

constructor TFileStreamEx.Create(const aFileName: TFileName; Mode: cardinal);
var
  h: THandle;
begin
  if Mode and fmCreate = fmCreate then
    h := FileCreate(aFileName, Mode and (not fmCreate))
  else
    h := FileOpen(aFileName, Mode);
  CreateFromHandle(aFileName, h);
end;

constructor TFileStreamEx.CreateFromHandle(const aFileName: TFileName; aHandle: THandle);
begin
  if not ValidHandle(aHandle) then
    raise EOSException.CreateFmt('%s.Create(%s) failed as %s',
      [ClassNameShort(self)^, aFileName, GetErrorText(GetLastError)]);
  inherited Create(aHandle); // TFileStreamFromHandle constructor which own it 
  fFileName := aFileName;
end;

constructor TFileStreamEx.CreateWrite(const aFileName: TFileName);
var
  h: THandle;
begin
  h := FileOpen(aFileName, fmOpenReadWrite or fmShareRead);
  if not ValidHandle(h) then // we may need to create the file
    h := FileCreate(aFileName, fmShareRead);
  CreateFromHandle(aFileName, h);
end;


{ TFileStreamNoWriteError }

constructor TFileStreamNoWriteError.CreateAndRenameIfLocked(
  var aFileName: TFileName; aAliases: integer);
var
  h: THandle;
  fn, ext: TFileName;
  err, retry: integer;

  function CanOpenWrite: boolean;
  begin
    h := FileOpen(aFileName, fmOpenReadWrite or fmShareRead);
    result := ValidHandle(h);
    if not result then
      err := GetLastError;
  end;

begin
  // logic similar to TSynLog.CreateLogWriter
  h := 0;
  err := 0;
  if not CanOpenWrite then
    if not FileExists(aFileName) then
      // immediately raise EOSException if this new file could not be created
      h := FileCreate(aFileName, fmShareRead)
    else
    begin
      fn := aFileName;
      ext := ExtractFileExt(aFileName);
      for retry := 1 to aAliases do
      begin
        if IsSharedViolation(err) then
        begin
          // file was locked: wait a little for a background process and retry
          SleepHiRes(50);
          if CanOpenWrite then
            break;
        end;
        // file can't be opened: try '<filename>-locked<#>.<ext>' alternatives
        aFileName := ChangeFileExt(fn, '-locked' + IntToStr(retry) + ext);
        if CanOpenWrite then
          break;
      end;
    end;
  CreateFromHandle(aFileName, h);
end;

function TFileStreamNoWriteError.Write(const Buffer; Count: Longint): Longint;
begin
  FileWriteAll(Handle, @Buffer, Count); // and ignore any I/O error
  result := Count; //
end;


function FileStreamSequentialRead(const FileName: TFileName): THandleStream;
begin
  result := TFileStreamFromHandle.Create(FileOpenSequentialRead(FileName));
end;

function StreamCopyUntilEnd(Source, Dest: TStream): Int64;
var
  tmp: array[word] of word; // 128KB stack buffer
  read: integer;
begin
  result := 0;
  if (Source <> nil) and
     (Dest <> nil) then
    repeat
      read := Source.Read(tmp, SizeOf(tmp));
      if read <= 0 then
        break;
      Dest.WriteBuffer(tmp, read);
      inc(result, read);
    until false;
end;

function FileReadAll(F: THandle; Buffer: pointer; Size: PtrInt): boolean;
var
  chunk, read: PtrInt;
begin
  result := false;
  if Size > 0 then
    repeat
      chunk := Size;
      {$ifdef OSWINDOWS}
      if chunk > 16 shl 20 then
        chunk := 16 shl 20; // to avoid ERROR_NO_SYSTEM_RESOURCES errors
      {$endif OSWINDOWS}
      read := FileRead(F, Buffer^, chunk);
      if read <= 0 then
        exit; // error reading Size bytes
      inc(PByte(Buffer), read);
      dec(Size, read);
    until Size = 0;
  result := true;
end;

function FileWriteAll(F: THandle; Buffer: pointer; Size: PtrInt): boolean;
var
  written: PtrInt;
begin
  result := false;
  if Size > 0 then
    repeat
      written := FileWrite(F, Buffer^, Size);
      if written <= 0 then
        exit; // fatal error
      inc(PByte(Buffer), written); // e.g. may have been interrrupted
      dec(Size, written);
    until Size = 0;
  result := true;
end;

function StringFromFile(const FileName: TFileName; HasNoSize: boolean): RawByteString;
var
  F: THandle;
  size: Int64;
  read, pos: integer;
  tmp: array[0..$7fff] of AnsiChar; // 32KB stack buffer
begin
  result := '';
  if FileName = '' then
    exit;
  F := FileOpenSequentialRead(FileName); // = plain fpOpen() on POSIX
  if ValidHandle(F) then
  begin
    if HasNoSize then
    begin
      pos := 0;
      repeat
        read := FileRead(F, tmp, SizeOf(tmp)); // fill per 32KB local buffer
        if read <= 0 then
          break;
        SetLength(result, pos + read); // in-place resize
        MoveFast(tmp, PByteArray(result)^[pos], read);
        inc(pos, read);
      until false;
    end
    else
    begin
      size := FileSize(F);
      if (size < MaxInt) and // 2GB seems big enough for a RawByteString
         (size > 0) then
      begin
        FastSetString(RawUtf8(result), size); // assume CP_UTF8 for FPC RTL bug
        if not FileReadAll(F, pointer(result), size) then
          result := ''; // error reading
      end;
    end;
    FileClose(F);
  end;
end;

function StringFromFirstFile(const FileName: array of TFileName): RawByteString;
var
  f: PtrInt;
begin
  for f := 0 to high(FileName) do
  begin
    result := StringFromFile(FileName[f]);
    if result <> '' then
      exit;
  end;
  result := '';
end;

function StringFromFiles(const FileName: array of TFileName): TRawByteStringDynArray;
var
  f: PtrInt;
begin
  result := nil;
  SetLength(result, length(FileName));
  for f := 0 to high(FileName) do
    result[f] := StringFromFile(FileName[f]);
end;

function StringFromFolders(const Folders: array of TFileName;
  const Mask: TFileName; FileNames: PFileNameDynArray): TRawByteStringDynArray;
var
  dir, fn: TFileName;
  sr: TSearchRec;
  f, n: PtrInt;
  one: RawUtf8;
begin
  result := nil;
  if FileNames <> nil then
    FileNames^ := nil;
  n := 0;
  for f := 0 to high(Folders) do
    if DirectoryExists(Folders[f]) then
    begin
      dir := IncludeTrailingPathDelimiter(Folders[f]);
      if FindFirst(dir + Mask, faAnyFile - faDirectory, sr) = 0 then
      begin
        repeat
          if SearchRecValidFile(sr) then
          begin
            fn := dir + sr.Name;
            one := StringFromFile(fn);
            if one <> '' then
            begin
              if length(result) = n then
              begin
                SetLength(result, NextGrow(n));
                if FileNames <> nil then
                  SetLength(FileNames^, length(result));
              end;
              result[n] := one;
              if FileNames <> nil then
                FileNames^[n] := fn;
              inc(n);
            end;
          end;
        until FindNext(sr) <> 0;
        FindClose(sr);
      end;
    end;
  if n = 0 then
    exit;
  DynArrayFakeLength(result, n);
  if FileNames <> nil then
    DynArrayFakeLength(FileNames^, n);
end;

function FileFromString(const Content: RawByteString;
  const FileName: TFileName; FlushOnDisk: boolean): boolean;
var
  h: THandle;
begin
  result := false;
  h := FileCreate(FileName);
  if not ValidHandle(h) then
    exit;
  if not FileWriteAll(h, pointer(Content), length(Content)) then
  begin
    FileClose(h); // abort on write error
    exit;
  end;
  if FlushOnDisk then
    FlushFileBuffers(h);
  FileClose(h);
  result := true;
end;

function FileFromBuffer(Buf: pointer; Len: PtrInt; const FileName: TFileName): boolean;
var
  h: THandle;
begin
  result := false;
  h := FileCreate(FileName);
  if not ValidHandle(h) then
    exit;
  result := FileWriteAll(h, Buf, Len);
  FileClose(h);
end;

function AppendToFile(const Content: RawUtf8; const FileName: TFileName;
  BackupOverMaxSize: Int64): boolean;
var
  h: THandle;
  bak: TFileName;
begin
  result := Content = '';
  if result then
    exit;
  if (BackupOverMaxSize > 0) and
     (FileSize(FileName) > BackupOverMaxSize) then
  begin
    bak := FileName + '.bak';
    DeleteFile(bak);
    RenameFile(FileName, bak);
    h := 0;
  end
  else
    h := FileOpen(FileName, fmOpenWriteShared);
  if ValidHandle(h) then
    FileSeek64(h, 0, soFromEnd) // append
  else
  begin
    h := FileCreate(FileName, fmShareReadWrite);
    if not ValidHandle(h) then
      exit;
  end;
  result := FileWriteAll(h, pointer(Content), Length(Content));
  FileClose(h);
end;

var
  _TmpCounter: integer;

function TemporaryFileName: TFileName;
var
  folder: TFileName;
  retry: integer;
begin
  // fast cross-platform implementation
  folder := GetSystemPath(spTemp);
  if _TmpCounter = 0 then
    _TmpCounter := Random32;
  retry := 10;
  repeat
    // thread-safe unique file name generation
    result := Format('%s%s_%x.tmp',
      [folder, Executable.ProgramName, InterlockedIncrement(_TmpCounter)]);
    if not FileExists(result) then
      exit;
    dec(retry); // no endless loop
  until retry = 0;
  raise EOSException.Create('TemporaryFileName failed');
end;

function GetLastDelim(const FileName: TFileName; OtherDelim: cardinal): PtrInt;
var
  {$ifdef UNICODE}
  p: PWordArray absolute FileName;
  {$else}
  p: PByteArray absolute FileName;
  {$endif UNICODE}
begin
  result := length(FileName);
  while (result > 0) and
        not (p[result - 1] in [ord('\'), ord('/'), ord(':'), OtherDelim]) do
    dec(result);
end;

function GetLastDelimU(const FileName: RawUtf8; OtherDelim: AnsiChar): PtrInt;
begin
  result := length(FileName);
  while (result > 0) and
        not (FileName[result] in ['\', '/', ':', OtherDelim]) do
    dec(result);
end;

function ExtractPath(const FileName: TFileName): TFileName;
begin
  SetString(result, PChar(pointer(FileName)), GetLastDelim(FileName, 0));
end;

function ExtractName(const FileName: TFileName): TFileName;
begin
  result := copy(FileName, GetLastDelim(FileName, 0) + 1, maxInt);
end;

function ExtractNameU(const FileName: RawUtf8): RawUtf8;
begin
  result := copy(FileName, GetLastDelimU(FileName, #0) + 1, maxInt);
end;

function ExtractPathU(const FileName: RawUtf8): RawUtf8;
begin
  FastSetString(result, pointer(FileName), GetLastDelimU(FileName, #0));
end;

function ExtractExt(const FileName: TFileName; WithoutDot: boolean): TFileName;
var
  i: PtrInt;
begin
  result := '';
  i := GetLastDelim(FileName, ord('.'));
  if (i <= 1) or
     (FileName[i] <> '.') then
    exit;
  if WithoutDot then
    inc(i);
  result := copy(FileName, i, 100);
end;

function ExtractExtU(const FileName: RawUtf8; WithoutDot: boolean): RawUtf8;
var
  i: PtrInt;
begin
  result := '';
  i := GetLastDelimU(FileName, '.');
  if (i <= 1) or
     (FileName[i] <> '.') then
    exit;
  if WithoutDot then
    inc(i);
  result := copy(FileName, i, 100);
end;

function ExtractExtP(const FileName: RawUtf8; WithoutDot: boolean): PUtf8Char;
var
  i: PtrInt;
begin
  result := nil;
  i := GetLastDelimU(FileName, '.') - 1;
  if i <= 0 then
    exit;
  result := PUtf8Char(pointer(FileName)) + i;
  if result^ <> '.' then
    result := nil
  else if WithoutDot then
    inc(result);
end;

function GetFileNameWithoutExt(const FileName: TFileName; Extension: PFileName): TFileName;
var
  i, max: PtrInt;
begin
  i := length(FileName);
  max := i - 16; // a file .extension is unlikely to be more than 16 chars
  while (i > 0) and
        not (cardinal(FileName[i]) in [ord('\'), ord('/'), ord('.'), ord(':')]) and
        (i >= max) do
    dec(i);
  if (i = 0) or
     (FileName[i] <> '.') then
  begin
    result := FileName;
    if Extension <> nil then
      Extension^ := '';
  end
  else
  begin
    result := copy(FileName, 1, i - 1);
    if Extension <> nil then
      Extension^ := copy(FileName, i, 100);
  end;
end;

function GetFileNameWithoutExtOrPath(const FileName: TFileName): RawUtf8;
begin
  result := RawUtf8(GetFileNameWithoutExt(ExtractFileName(FileName)));
end;

{$ifdef ISDELPHI20062007} // circumvent Delphi 2007 RTL inlining issue
function AnsiCompareFileName(const S1, S2 : TFileName): integer;
begin
  result := SysUtils.AnsiCompareFileName(S1,S2);
end;
{$endif ISDELPHI20062007}

function SortDynArrayFileName(const A, B): integer;
var
  Aname, Aext, Bname, Bext: TFileName;
begin
  // code below is not very fast, but correct ;)
  Aname := GetFileNameWithoutExt(string(A), @Aext);
  Bname := GetFileNameWithoutExt(string(B), @Bext);
  result := AnsiCompareFileName(Aext, Bext);
  if result = 0 then
    // if both extensions matches, compare by filename
    result := AnsiCompareFileName(Aname, Bname);
end;

function EnsureDirectoryExists(const Directory: TFileName;
  RaiseExceptionOnCreationFailure: ExceptionClass): TFileName;
begin
  if Directory = '' then
    if RaiseExceptionOnCreationFailure <> nil then
      raise RaiseExceptionOnCreationFailure.Create('EnsureDirectoryExists('''')')
    else
      result := ''
  else
  begin
    result := IncludeTrailingPathDelimiter(ExpandFileName(Directory));
    if not DirectoryExists(result) then
      if not ForceDirectories(result) then
        if RaiseExceptionOnCreationFailure <> nil then
          raise RaiseExceptionOnCreationFailure.CreateFmt(
            'EnsureDirectoryExists(%s) failed as %s',
            [result, GetErrorText(GetLastError)])
        else
          result := '';
  end;
end;

function NormalizeDirectoryExists(const Directory: TFileName;
  RaiseExceptionOnCreationFailure: ExceptionClass): TFileName;
begin
  result := EnsureDirectoryExists(NormalizeFileName(Directory),
    RaiseExceptionOnCreationFailure);
end;

function DirectoryDelete(const Directory: TFileName; const Mask: TFileName;
  DeleteOnlyFilesNotDirectory: boolean; DeletedCount: PInteger): boolean;
var
  F: TSearchRec;
  Dir: TFileName;
  n: integer;
begin
  n := 0;
  result := true;
  if DirectoryExists(Directory) then
  begin
    Dir := IncludeTrailingPathDelimiter(Directory);
    if FindFirst(Dir + Mask, faAnyFile - faDirectory, F) = 0 then
    begin
      repeat
        if SearchRecValidFile(F) then
          if DeleteFile(Dir + F.Name) then
            inc(n)
          else
            result := false;
      until FindNext(F) <> 0;
      FindClose(F);
    end;
    if not DeleteOnlyFilesNotDirectory and
       not RemoveDir(Dir) then
      result := false;
  end;
  if DeletedCount <> nil then
    DeletedCount^ := n;
end;

function DirectoryDeleteOlderFiles(const Directory: TFileName;
  TimePeriod: TDateTime; const Mask: TFileName; Recursive: boolean;
  TotalSize: PInt64): boolean;
var
  F: TSearchRec;
  Dir: TFileName;
  old: TDateTime;
begin
  if not Recursive and
     (TotalSize <> nil) then
    TotalSize^ := 0;
  result := true;
  if (Directory = '') or
     not DirectoryExists(Directory) then
    exit;
  Dir := IncludeTrailingPathDelimiter(Directory);
  if FindFirst(Dir + Mask, faAnyFile, F) = 0 then
  begin
    old := NowUtc - TimePeriod;
    repeat
      if SearchRecValidFolder(F) then
      begin
        if Recursive then
          DirectoryDeleteOlderFiles(
            Dir + F.Name, TimePeriod, Mask, true, TotalSize);
      end
      else if SearchRecValidFile(F) and
              (SearchRecToDateTimeUtc(F) < old) then
        if not DeleteFile(Dir + F.Name) then
          result := false
        else if TotalSize <> nil then
          inc(TotalSize^, F.Size);
    until FindNext(F) <> 0;
    FindClose(F);
  end;
end;

var
  lastIsDirectoryWritable: TFileName; // naive but efficient cache

function IsDirectoryWritable(const Directory: TFileName;
  Flags: TIsDirectoryWritable): boolean;
var
  dir, last, fmt, fn: TFileName;
  f: THandle;
  retry: integer;
begin
  // check the Directory itself
  result := false;
  if Directory = '' then
    exit;                       
  dir := ExcludeTrailingPathDelimiter(Directory);
  if Flags = [] then
  begin
    last := lastIsDirectoryWritable;
    result := (last <> '') and
              (dir = last);
    if result then
      exit; // we just tested this folder
  end;
  if not FileIsWritable(dir) then
    exit; // the folder does not exist or is read-only for the current user
  {$ifdef OSWINDOWS}
  // ensure is not a system/virtual folder
  if ((idwExcludeWinUac in Flags) and
      IsUacVirtualFolder(dir)) or
     ((idwExcludeWinSys in Flags) and
      IsSystemFolder(dir)) then
    exit;
  // compute a non existing temporary file name in this Directory
  if idwTryWinExeFile in Flags then
    fmt := '%s\%x.exe'  // may trigger the anti-virus heuristic
  else
    fmt := '%s\%x.test'; // neutral file name
    // we tried .crt which triggered UAC heuristic but also some anti-viruses :(
  {$else}
  // compute a non existing temporary file name in this Directory
  fmt := '%s/.%x.test'; // make the file "invisible"
  {$endif OSWINDOWS}
  retry := 10;
  repeat
    fn := Format(fmt, [dir, Random32]);
    if not FileExists(fn) then
      break;
    dec(retry); // never loop forever
    if retry = 0 then
      exit;
  until false;
  // ensure we can create this temporary file
  f := FileCreate(fn);
  if not ValidHandle(f) then
    exit; // a file can't be created
  result := true;
  if (idwWriteSomeContent in flags) and // some pointers and hash
     (FileWrite(f, Executable, SizeOf(Executable)) <> SizeOf(Executable)) then
    result := false;
  FileClose(f);
  if not DeleteFile(fn) then // success if the file can be created and deleted
    result := false
  else if result then
    lastIsDirectoryWritable := dir
end;


{$ifndef NOEXCEPTIONINTERCEPT}

{$ifdef WITH_RAISEPROC} // for FPC on Win32 + Linux (Win64=WITH_VECTOREXCEPT)
var
  OldRaiseProc: TExceptProc;

procedure SynRaiseProc(Obj: TObject; Addr: CodePointer;
  FrameCount: integer; Frame: PCodePointer);
var
  ctxt: TSynLogExceptionContext;
  backuplasterror: DWORD;
  backuphandler: TOnRawLogException;
begin
  if (Obj <> nil) and
     Obj.InheritsFrom(Exception) then
  begin
    backuplasterror := GetLastError;
    backuphandler := _RawLogException;
    if Assigned(backuphandler) then
      try
        _RawLogException := nil; // disable nested exception
        ctxt.EClass := PPointer(Obj)^;
        ctxt.EInstance := Exception(Obj);
        ctxt.EAddr := PtrUInt(Addr);
        if Obj.InheritsFrom(EExternal) then
          ctxt.ELevel := sllExceptionOS
        else
          ctxt.ELevel := sllException;
        ctxt.ETimestamp := UnixTimeUtc;
        ctxt.EStack := pointer(Frame);
        ctxt.EStackCount := FrameCount;
        backuphandler(ctxt);
      except
        { ignore any nested exception }
      end;
    _RawLogException := backuphandler;
    SetLastError(backuplasterror); // may have changed above
  end;
  if Assigned(OldRaiseProc) then
    OldRaiseProc(Obj, Addr, FrameCount, Frame);
end;

{$endif WITH_RAISEPROC}

var
  RawExceptionIntercepted: boolean;

procedure RawExceptionIntercept(const Handler: TOnRawLogException);
begin
  _RawLogException := Handler;
  if RawExceptionIntercepted or
     not Assigned(Handler) then
    exit;
  RawExceptionIntercepted := true; // intercept once
  {$ifdef WITH_RAISEPROC}
  // FPC RTL redirection function
  if not Assigned(OldRaiseProc) then
  begin
    OldRaiseProc := RaiseProc;
    RaiseProc := @SynRaiseProc;
  end;
  {$endif WITH_RAISEPROC}
  {$ifdef WITH_VECTOREXCEPT} // SEH32/SEH64 official API
  // RemoveVectoredContinueHandler() is available under 64 bit editions only
  if Assigned(AddVectoredExceptionHandler) then
  begin
    AddVectoredExceptionHandler(0, @SynLogVectoredHandler);
    AddVectoredExceptionHandler := nil;
  end;
  {$endif WITH_VECTOREXCEPT}
  {$ifdef WITH_RTLUNWINDPROC}
  // Delphi x86 RTL redirection function
  if not Assigned(OldUnWindProc) then
  begin
    OldUnWindProc := RTLUnwindProc;
    RTLUnwindProc := @SynRtlUnwind;
  end;
  {$endif WITH_RTLUNWINDPROC}
end;

{$endif NOEXCEPTIONINTERCEPT}


{ TMemoryMap }

function TMemoryMap.Map(aFile: THandle; aCustomSize: PtrUInt;
  aCustomOffset: Int64; aFileOwned: boolean; aFileSize: Int64): boolean;
var
  Available: Int64;
begin
  fBuf := nil;
  fBufSize := 0;
  {$ifdef OSWINDOWS}
  fMap := 0;
  {$endif OSWINDOWS}
  fFileLocal := aFileOwned;
  fFile := aFile;
  if aFileSize < 0 then
    aFileSize := mormot.core.os.FileSize(fFile);
  fFileSize := aFileSize;
  if aFileSize = 0 then
  begin
    result := true; // handle 0 byte file without error (but no memory map)
    exit;
  end;
  result := false;
  if (fFileSize <= 0)
     {$ifdef CPU32} or (fFileSize > maxInt){$endif} then
    // maxInt = $7FFFFFFF = 1.999 GB (2GB would induce PtrInt errors on CPU32)
    exit;
  if aCustomSize = 0 then
    fBufSize := fFileSize
  else
  begin
    Available := fFileSize - aCustomOffset;
    if Available < 0 then
      exit;
    if aCustomSize > Available then
      fBufSize := Available;
    fBufSize := aCustomSize;
  end;
  fLoadedNotMapped := fBufSize < 1 shl 20;
  if fLoadedNotMapped then
  begin
    // mapping is not worth it for size < 1MB which can be just read at once
    GetMem(fBuf, fBufSize);
    FileSeek64(fFile, aCustomOffset);
    if FileReadAll(fFile, fBuf, fBufSize) then
      result := true
    else
    begin
      Freemem(fBuf);
      fBuf := nil;
      fLoadedNotMapped := false;
    end;
  end
  else
    // call actual Windows/POSIX memory mapping API
    result := DoMap(aCustomOffset);
end;

procedure TMemoryMap.Map(aBuffer: pointer; aBufferSize: PtrUInt);
begin
  fBuf := aBuffer;
  fFileSize := aBufferSize;
  fBufSize := aBufferSize;
  {$ifdef OSWINDOWS}
  fMap := 0;
  {$endif OSWINDOWS}
  fFile := 0;
  fFileLocal := false;
end;

function TMemoryMap.Map(const aFileName: TFileName): boolean;
var
  F: THandle;
begin
  result := false;
  // Memory-mapped file access does not go through the cache manager so
  // using FileOpenSequentialRead() is pointless here
  F := FileOpen(aFileName, fmOpenReadShared);
  if not ValidHandle(F) then
    exit;
  result := Map(F);
  if not result then
    FileClose(F);
  fFileLocal := result;
end;

procedure TMemoryMap.UnMap;
begin
  if fLoadedNotMapped then
    // mapping was not worth it
    Freemem(fBuf)
  else
    // call actual Windows/POSIX map API
    DoUnMap;
  fBuf := nil;
  fBufSize := 0;
  if fFile <> 0 then
  begin
    if fFileLocal then
      FileClose(fFile);
    fFile := 0;
  end;
end;



{ TSynMemoryStreamMapped }

constructor TSynMemoryStreamMapped.Create(const aFileName: TFileName;
  aCustomSize: PtrUInt; aCustomOffset: Int64);
begin
  fFileName := aFileName;
  // Memory-mapped file access does not go through the cache manager so
  // using FileOpenSequentialRead() is pointless here
  fFileStream := TFileStreamEx.Create(aFileName, fmOpenReadShared);
  Create(fFileStream.Handle, aCustomSize, aCustomOffset);
end;

constructor TSynMemoryStreamMapped.Create(aFile: THandle;
  aCustomSize: PtrUInt; aCustomOffset: Int64);
begin
  if not fMap.Map(aFile, aCustomSize, aCustomOffset) then
    raise EOSException.CreateFmt('%s.Create(%s) mapping error',
      [ClassNameShort(self)^, fFileName]);
  inherited Create(fMap.fBuf, fMap.fBufSize);
end;

destructor TSynMemoryStreamMapped.Destroy;
begin
  fMap.UnMap;
  fFileStream.Free;
  inherited;
end;


{ TExecutableResource }

function TExecutableResource.Open(const ResourceName: string; ResType: PChar;
  Instance: TLibHandle): boolean;
begin
  result := false;
  if Instance = 0 then
    Instance := HInstance;
  HResInfo := FindResource(Instance, PChar(ResourceName), ResType);
  if HResInfo = 0 then
    exit;
  HGlobal := LoadResource(Instance, HResInfo);
  if HGlobal = 0 then // direct decompression from memory mapped .exe content
    exit;
  Buffer := LockResource(HGlobal);
  Size := SizeofResource(Instance, HResInfo);
  if Size > 0 then
    result := true
  else
    Close; // paranoid check
end;

procedure TExecutableResource.Close;
begin
  if HGlobal <> 0 then
  begin
    UnlockResource(HGlobal); // only needed outside of Windows
    FreeResource(HGlobal);
    HGlobal := 0;
  end;
end;


{ ReserveExecutableMemory() / TFakeStubBuffer }

type
  // internal memory buffer created with PAGE_EXECUTE_READWRITE flags
  TFakeStubBuffer = class
  public
    Stub: PByteArray;
    StubUsed: cardinal;
    constructor Create;
    destructor Destroy; override;
    function Reserve(size: cardinal): pointer;
  end;

var
  CurrentFakeStubBuffer: TFakeStubBuffer;
  CurrentFakeStubBuffers: array of TFakeStubBuffer;
  CurrentFakeStubBufferLock: TLightLock;
  {$ifdef UNIX}
  MemoryProtection: boolean = false; // set to true if PROT_EXEC seems to fail
  {$endif UNIX}

constructor TFakeStubBuffer.Create;
begin
  {$ifdef OSWINDOWS}
  Stub := VirtualAlloc(nil, STUB_SIZE, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if Stub = nil then
  {$else OSWINDOWS}
  if not MemoryProtection then
    Stub := StubCallAllocMem(STUB_SIZE, PROT_READ or PROT_WRITE or PROT_EXEC);
  if (Stub = MAP_FAILED) or
     MemoryProtection then
  begin
    // i.e. on OpenBSD or OSX M1, we can not have w^x protection
    Stub := StubCallAllocMem(STUB_SIZE, PROT_READ OR PROT_WRITE);
    if Stub <> MAP_FAILED then
      MemoryProtection := True;
  end;
  if Stub = MAP_FAILED then
  {$endif OSWINDOWS}
    raise EOSException.Create('ReserveExecutableMemory(): OS mmap failed');
  PtrArrayAdd(CurrentFakeStubBuffers, self);
end;

destructor TFakeStubBuffer.Destroy;
begin
  {$ifdef OSWINDOWS}
  VirtualFree(Stub, 0, MEM_RELEASE);
  {$else}
  fpmunmap(Stub, STUB_SIZE);
  {$endif OSWINDOWS}
  inherited;
end;

function TFakeStubBuffer.Reserve(size: cardinal): pointer;
begin
  result := @Stub[StubUsed];
  while size and 15 <> 0 do
    inc(size); // ensure the returned buffers are 16 bytes aligned
  inc(StubUsed, size);
end;

function ReserveExecutableMemory(size: cardinal): pointer;
begin
  if size > STUB_SIZE then
    raise EOSException.CreateFmt('ReserveExecutableMemory(size=%d>%d)',
      [size, STUB_SIZE]);
  CurrentFakeStubBufferLock.Lock;
  try
    if (CurrentFakeStubBuffer = nil) or
       (CurrentFakeStubBuffer.StubUsed + size > STUB_SIZE) then
      CurrentFakeStubBuffer := TFakeStubBuffer.Create;
    result := CurrentFakeStubBuffer.Reserve(size);
  finally
    CurrentFakeStubBufferLock.UnLock;
  end;
end;

{$ifdef UNIX}
procedure ReserveExecutableMemoryPageAccess(Reserved: pointer; Exec: boolean);
var
  PageAlignedFakeStub: pointer;
  flags: cardinal;
begin
  if not MemoryProtection then
    // nothing to be done on this platform
    exit;
  // toggle execution permission of memory to be able to write into memory
  PageAlignedFakeStub := Pointer(
    (PtrUInt(Reserved) div SystemInfo.dwPageSize) * SystemInfo.dwPageSize);
  if Exec then
    flags := PROT_READ OR PROT_EXEC
  else
    flags := PROT_READ or PROT_WRITE;
  if SynMProtect(PageAlignedFakeStub, SystemInfo.dwPageSize shl 1, flags) < 0 then
     raise EOSException.Create('ReserveExecutableMemoryPageAccess: mprotect fail');
end;
{$else}
procedure ReserveExecutableMemoryPageAccess(Reserved: pointer; Exec: boolean);
begin
  // nothing to be done
end;
{$endif UNIX}

{$ifndef PUREMORMOT2}
function GetDelphiCompilerVersion: RawUtf8;
begin
  result := COMPILER_VERSION;
end;
{$endif PUREMORMOT2}

function GetMemoryInfoText: RawUtf8;
var
  info: TMemoryInfo;
begin
  if GetMemoryInfo(info, false) then
    _fmt('used %s/%s (%d%s free)', [_oskb(info.memtotal - info.memfree),
      _oskb(info.memtotal), info.percent, '%'], result)
  else
    result := '';
end;

function GetDiskAvailable(aDriveFolderOrFile: TFileName): QWord;
var
  free, total: QWord; // dummy values
begin
  if not GetDiskInfo(aDriveFolderOrFile, result, free, total) then
    result := 0;
end;

function GetSystemInfoText: RawUtf8;
var
  avail, free, total: QWord;
begin
  GetDiskInfo(Executable.ProgramFilePath, avail, free, total);
  result := _fmt('Current UTC date is %s (%d)'#13#10'Memory %s'#13#10 +
                 'Executable free disk %s/%s'#13#10 +
                 {$ifdef OSPOSIX} 'LoadAvg is %s'#13#10 + {$endif OSPOSIX}
                 '%s'#13#10'%s'#13#10'%s'#13#10'%s'#13#10,
    [FormatDateTime('yyyy"-"mm"-"dd" "hh":"nn":"ss', NowUtc), UnixTimeUtc,
     GetMemoryInfoText, _oskb(avail), _oskb(total),
     {$ifdef OSPOSIX} RetrieveLoadAvg, {$endif} Executable.Version.VersionInfo,
     OSVersionText, CpuInfoText, BiosInfoText]);
end;

function ConsoleReadBody: RawByteString;
var
  len, n: integer;
  P: PByte;
begin
  len := ConsoleStdInputLen;
  FastNewRawByteString(result, len);
  P := pointer(result);
  while len > 0 do
  begin
    n := FileRead(StdInputHandle, P^, len);
    if n <= 0 then
    begin
      result := ''; // read error
      break;
    end;
    dec(len, n);
    inc(P, n);
  end;
end;

var
  GlobalCriticalSection: TOSLock;

{ TSynLibrary }

function TSynLibrary.Resolve(const Prefix, ProcName: RawUtf8; Entry: PPointer;
  RaiseExceptionOnFailure: ExceptionClass): boolean;
var
  P: PAnsiChar;
  name, search: RawUtf8;
{$ifdef OSPOSIX}
  dlinfo: dl_info;
{$endif OSPOSIX}
begin
  result := false;
  if (Entry = nil) or
     (fHandle = 0) or
     (ProcName = '') then
    exit; // avoid GPF
  P := pointer(ProcName);
  repeat
    name := GetNextItem(P); // try all alternate names
    if name = '' then
      break;
    if name[1] = '?' then
    begin
      RaiseExceptionOnFailure := nil;
      delete(name, 1, 1);
    end;
    search := Prefix + name;
    Entry^ := LibraryResolve(fHandle, pointer(search));
    if (Entry^ = nil) and
       (Prefix <> '') then // try without the prefix
      Entry^ := LibraryResolve(fHandle, pointer(name));
    result := Entry^ <> nil;
  until result;
  {$ifdef OSPOSIX}
  if result and
     not fLibraryPathTested then
  begin
    fLibraryPathTested := true;
    FillCharFast(dlinfo, SizeOf(dlinfo), 0);
    dladdr(Entry^, @dlinfo);
    if dlinfo.dli_fname <> nil then
      fLibraryPath := dlinfo.dli_fname;
  end;
  {$endif OSPOSIX}
  if (RaiseExceptionOnFailure <> nil) and
     not result then
  begin
    FreeLib;
    raise RaiseExceptionOnFailure.CreateFmt(
      '%s.Resolve(''%s%s''): not found in %s',
      [ClassNameShort(self)^, Prefix, ProcName, LibraryPath]);
  end;
end;

function TSynLibrary.ResolveAll(ProcName: PPAnsiChar; Entry: PPointer): boolean;
var
  tmp: RawUtf8;
begin
  repeat
    if ProcName^ = nil then
      break;
    FastSetString(tmp, ProcName^, StrLen(ProcName^));
    if not Resolve('', tmp, Entry) then
    begin
      FreeLib;
      result := false;
      exit;
    end;
    inc(ProcName);
    inc(Entry);
  until false;
  result := true;
end;

destructor TSynLibrary.Destroy;
begin
  FreeLib;
  inherited Destroy;
end;

procedure TSynLibrary.FreeLib;
begin
  if fHandle = 0 then
    exit; // nothing to free
  LibraryClose(fHandle);
  fHandle := 0;
end;

function TSynLibrary.TryLoadLibrary(const aLibrary: array of TFileName;
  aRaiseExceptionOnFailure: ExceptionClass): boolean;
var
  i, j: PtrInt;
  {$ifdef OSWINDOWS}
  cwd,
  {$endif OSWINDOWS}
  lib, libs, nwd: TFileName;
  err: string;
begin
  for i := 0 to high(aLibrary) do
  begin
    // check library name
    lib := aLibrary[i];
    if lib = '' then
      continue;
    result := true;
    for j := 0 to i - 1 do
      if aLibrary[j] = lib then
      begin
        result := false;
        break;
      end;
    if not result then
      continue; // don't try twice the same library name
    // open the library
    nwd := ExtractFilePath(lib);
    if fTryFromExecutableFolder  and
       (nwd = '') and
       FileExists(Executable.ProgramFilePath + lib) then
    begin
      lib := Executable.ProgramFilePath + lib;
      nwd := Executable.ProgramFilePath;
    end;
    {$ifdef OSWINDOWS}
    if nwd <> '' then
    begin
      cwd := GetCurrentDir;
      SetCurrentDir(nwd); // change the current folder at loading on Windows
    end;
    fHandle := LibraryOpen(lib); // preserve x87 flags and prevent msg box 
    if nwd <> '' then
      SetCurrentDir(cwd{%H-});
    {$else}
    fHandle := LibraryOpen(lib); // use regular .so loading behavior
    {$endif OSWINDOWS}
    if fHandle <> 0 then
    begin
      {$ifdef OSWINDOWS} // on POSIX, will call dladdr() in Resolve()
      fLibraryPath := GetModuleName(fHandle);
      if length(fLibraryPath) < length(lib) then
      {$endif OSWINDOWS}
        fLibraryPath := lib;
      exit;
    end;
    // handle any error
    if {%H-}libs = '' then
      libs := lib
    else
      libs := libs + ', ' + lib;
    err := LibraryError;
    if err <> '' then
      libs := libs + ' [' + err + ']';
  end;
  result := false;
  if aRaiseExceptionOnFailure <> nil then
    raise aRaiseExceptionOnFailure.CreateFmt('%s.TryLoadLibray failed' +
      ' - searched in %s', [ClassNameShort(self)^, libs]);
end;

function TSynLibrary.Exists: boolean;
begin
  result := (self <> nil) and
            (fHandle <> 0);
end;


{ TFileVersion }

constructor TFileVersion.Create(const aFileName: TFileName;
  aMajor, aMinor, aRelease, aBuild: integer);
var
  M, D: word;
begin
  fFileName := aFileName;
  SetVersion(aMajor, aMinor, aRelease, aBuild);
  if fBuildDateTime = 0 then // get build date from file age
    fBuildDateTime := FileAgeToDateTime(aFileName);
  if fBuildDateTime <> 0 then
    DecodeDate(fBuildDateTime, BuildYear, M, D);
end;

function TFileVersion.Version32: integer;
begin
  if self = nil then
    result := 0
  else
    result := Major shl 16 + Minor shl 8 + Release;
end;

function TFileVersion.SetVersion(aMajor, aMinor, aRelease, aBuild: integer): boolean;
begin
  result := (Major <> aMajor) or
            (Minor <> aMinor) or
            (Release <> aRelease) or
            (Build <> aBuild);
  if not result then
    exit;
  Major := aMajor;
  Minor := aMinor;
  Release := aRelease;
  Build := aBuild;
  Main := Format('%d.%d', [Major, Minor]);
  if Build <> 0 then
    fDetailed := Format('%s.%d.%d', [Main, Release, Build])
  else if Release <> 0 then
    fDetailed := Format('%s.%d', [Main, Release])
  else
    fDetailed := Main;
  fVersionInfo :=  '';
  fUserAgent := '';
end;

function TFileVersion.BuildDateTimeString: string;
begin
  result := DateTimeToIsoString(fBuildDateTime);
end;

function TFileVersion.DetailedOrVoid: string;
begin
  if (self = nil) or
     (Major or Minor or Release or Build = 0) then
    result := ''
  else
    result := fDetailed;
end;

function TFileVersion.VersionInfo: RawUtf8;
begin
  if self = nil then
    result := ''
  else
  begin
    if fVersionInfo = '' then
      _fmt('%s %s (%s)', [ExtractFileName(fFileName),
        DetailedOrVoid, BuildDateTimeString], fVersionInfo);
    result := fVersionInfo;
  end;
end;

function TFileVersion.UserAgent: RawUtf8;
begin
  if self = nil then
    result := ''
  else
  begin
    if fUserAgent = '' then
    begin
      _fmt('%s/%s%s', [GetFileNameWithoutExtOrPath(fFileName), DetailedOrVoid,
        OS_INITIAL[OS_KIND]], fUserAgent);
      {$ifdef OSWINDOWS}
      if OSVersion in WINDOWS_32 then
        fUserAgent := fUserAgent + '32';
      {$endif OSWINDOWS}
    end;
    result := fUserAgent;
  end;
end;

class function TFileVersion.GetVersionInfo(const aFileName: TFileName): RawUtf8;
begin
  with Create(aFileName, 0, 0, 0, 0) do
  try
    result := VersionInfo;
  finally
    Free;
  end;
end;

function UserAgentParse(const UserAgent: RawUtf8;
  out ProgramName, ProgramVersion: RawUtf8;
  out OS: TOperatingSystem): boolean;
var
  i, v, vlen, o: PtrInt;
begin
  result := false;
  ProgramName := Split(UserAgent, '/');
  v := length(ProgramName);
  if (v = 0) or
     (UserAgent[v + 1] <> '/') then
    exit;
  inc(v, 2);
  vlen := 0;
  o := -1;
  for i := v to length(UserAgent) do
    if not (UserAgent[i] in ['0' .. '9', '.']) then
    begin
      vlen := i - v; // vlen may be 0 if DetailedOrVoid was ''
      if UserAgent[i + 1] in [#0, '3'] then // end with OS_INITIAL or '32' suffix
        o := ByteScanIndex(pointer(@OS_INITIAL),
          ord(high(TOperatingSystem)) + 1, ord(UserAgent[i]));
      break;
    end;
  if o < 0 then
    exit; // should end with OS_INITIAL[OS_KIND]]
  os := TOperatingSystem(o);
  ProgramVersion := copy(UserAgent, v, vlen);
  result := true;
end;

procedure SetExecutableVersion(const aVersionText: RawUtf8);
var
  P: PUtf8Char;
  i: integer;
  ver: array[0 .. 3] of integer;
begin
  P := pointer(aVersionText);
  for i := 0 to 3 do
    ver[i] := GetNextUInt32(P);
  SetExecutableVersion(ver[0], ver[1], ver[2], ver[3]);
end;

procedure ComputeExecutableHash;
begin
  with Executable do
  begin
    _fmt('%s %s (%s)', [ProgramFileName,
      Version.DetailedOrVoid, Version.BuildDateTimeString], ProgramFullSpec);
    Hash.c0 := Version.Version32;
    {$ifdef OSLINUXANDROID}
    Hash.c0 := crc32c(Hash.c0, pointer(CpuInfoFeatures), length(CpuInfoFeatures));
    {$else}
    {$ifdef CPUINTELARM}
    Hash.c0 := crc32c(Hash.c0, @CpuFeatures, SizeOf(CpuFeatures));
    {$else}
    Hash.c0 := crc32c(Hash.c0, pointer(CpuInfoText), length(CpuInfoText));
    {$endif OSLINUXANDROID}
    {$endif CPUINTELARM}
    Hash.c0 := crc32c(Hash.c0, pointer(Host), length(Host));
    Hash.c1 := crc32c(Hash.c0, pointer(User), length(User));
    Hash.c2 := crc32c(Hash.c1, pointer(ProgramFullSpec), length(ProgramFullSpec));
    Hash.c3 := crc32c(Hash.c2, pointer(InstanceFileName), length(InstanceFileName));
  end;
end;

procedure GetExecutableVersion;
begin
  if Executable.Version.RetrieveInformationFromFileName then
    ComputeExecutableHash;
end;

procedure InitializeExecutableInformation; // called once at startup
begin
  with Executable do
  begin
    {$ifdef OSWINDOWS}
    ProgramFileName := ParamStr(0); // RTL seems just fine here
    {$else}
    ProgramFileName := GetExecutableName(@InitializeExecutableInformation);
    if (ProgramFileName = '') or
       not FileExists(ProgramFileName) then
      ProgramFileName := ExpandFileName(ParamStr(0));
    {$endif OSWINDOWS}
    ProgramFilePath := ExtractFilePath(ProgramFileName);
    if IsLibrary then
      InstanceFileName := GetModuleName(HInstance)
    else
      InstanceFileName := ProgramFileName;
    ProgramName := GetFileNameWithoutExtOrPath(ProgramFileName);
    GetUserHost(User, Host);
    if Host = '' then
      Host := 'unknown';
    if User = '' then
      User := 'unknown';
    Version := TFileVersion.Create(ProgramFileName); // with versions=0
    Command := TExecutableCommandLine.Create;
    Command.ExeDescription := ProgramName;
    Command.Parse;
  end;
  ComputeExecutableHash;
end;

procedure SetExecutableVersion(aMajor, aMinor, aRelease, aBuild: integer);
begin
  if Executable.Version.SetVersion(aMajor, aMinor, aRelease, aBuild) then
    ComputeExecutableHash; // re-compute if changed
end;


{ TExecutableCommandLine }

function TExecutableCommandLine.SwitchAsText(const v: RawUtf8): RawUtf8;
begin
  result := fSwitch[length(v) > 1] + v;
end;

procedure TExecutableCommandLine.Describe(const v: array of RawUtf8;
  k: TExecutableCommandLineKind; d, def: RawUtf8; argindex: integer);
var
  i, j: PtrInt;
  desc, param, pnames, sp: RawUtf8;
begin
  if (self = nil) or
     (d = '') then
    exit;
  if k <> clkArg then
  begin
    if high(v) < 0 then
      exit;
    desc := SwitchAsText(v[0]);
    if length(v[0]) <> 1 then
      desc := '    ' + desc; // right align --#
    for i := 1 to high(v) do
      desc := desc + ', ' + SwitchAsText(v[i]);
  end;
  if k <> clkOption then
  begin
    i := PosExChar('#', d); // #valuename in description -> <valuename>
    if i > 0 then
    begin
      j := 1;
      while d[i + j] > ' ' do
        inc(j);
      delete(d, i, 1); // remove #
      if d[i] <> '#' then
        param := copy(d, i, j - 1) // extract '<valuename>'
      else
      begin
        param := copy(d, i + 1, j - 2); // ##type
        delete(d, i, j);                // not included in description
      end;
    end
    else if k = clkArg then
      if high(v) = 0 then
        param := v[0]
      else if argindex > 0 then
        param := _fmt('arg%d', [argindex])
      else
        param := 'arg'
    else
    begin
      i := PosEx(' - values: ', d); // see SetObjectFromExecutableCommandLine()
      if i > 0 then
      begin
        inc(i, 11);
        j := 1;
        if copy(d, i, 7) = 'set of ' then
          inc(j, 7);
        while d[i + j] > ' ' do
          inc(j);
        param := copy(d, i, j);
        dec(i, 11);
        delete(d, i, j + 11);
        if j > 50 then
        begin
          j := 50;
          for i := 50 downto 1 do
            if param[i] = '|' then
            begin
              j := i;
              break;
            end;
          insert(fLineFeed + '         ', param, j + 1);
        end;
      end
      else
        param := 'value';
    end;
    desc := desc + ' <' + param + '>';
    if (k = clkArg) and
       (argindex > 0) then
    begin
      if argindex > length(fDescArg) then
        SetLength(fDescArg, argindex);
      fDescArg[argindex - 1] := param;
    end;
  end;
  fDesc[k] := fDesc[k] + ' ' + desc;
  j := 1;
  if fSwitch[true] <> '--' then
    repeat
      i := PosEx('--', d, j); // e.g. '--switch' -> '/switch' on Windows
      if i = 0 then
        break;
      delete(d, i, 2);
      insert(fSwitch[true], d, i);
      j := i;
    until false;
  if def <> '' then
    def := ' (default ' + def + ')';
  pnames := _fmt('  %0:-20s', [desc + def]);
  if (length(pnames) > 22) or
     (length(d) > 80) then
  begin
    // write description on next line(s)
    sp := fLineFeed + '                      ';
    while length(d) > 57 do
    begin
      j := 57;
      for i := 57 downto 1 do
        if d[i] = ' ' then
        begin
          j := i;
          break;
        end;
      if j = 57 then
        for i := 57 downto 1 do
          if d[i] in [',', ';', '|'] then
          begin
            j := i;
            break;
          end;
      pnames := pnames + sp + copy(d, 1, j);
      delete(d, 1, j);
    end;
    pnames := pnames + sp + d;
  end
  else
    pnames := pnames + d; // we can put everything on the same line
  fDescDetail[k] := fDescDetail[k] + pnames + fLineFeed;
end;

function TExecutableCommandLine.Find(const v: array of RawUtf8;
  k: TExecutableCommandLineKind; const d, def: RawUtf8; f: PtrInt): PtrInt;
var
  i: PtrInt;
begin
  if self <> nil then
  begin
    if k <> clkUndefined then
      Describe(v, k, d, def, -1);
    if (high(v) >= 0) and
       (fNames[k] <> nil) then
      for i := 0 to high(v) do
      begin
        result := FindNonVoid[fCaseSensitiveNames](
          @fNames[k][f], pointer(v[i]), length(v[i]), length(fNames[k]) - f);
        if result >= 0 then
        begin
          inc(result, f);
          fRetrieved[k][result] := true;
          exit;
        end;
      end;
  end;
  result := -1
end;

function TExecutableCommandLine.Arg(index: integer; const description: RawUtf8;
  optional: boolean): boolean;
var
  n: PtrUInt;
begin
  result := self <> nil;
  if not result then
    exit;
  n := length(fNames[clkArg]);
  result := PtrUInt(index) < n;
  if result then
    fRetrieved[clkArg][index] := true
  else
  begin
    SetLength(fRetrieved[clkArg], n + 1); // to notify missing <arg>
    if optional then
      fRetrieved[clkArg][index] := true;
  end;
  Describe([], clkArg, description, '', index + 1);
end;

function TExecutableCommandLine.ArgString(index: integer;
  const description: RawUtf8; optional: boolean): string;
begin
  result := '';
  if Arg(index, description, optional) then
    result := string(Args[0]);
end;

function TExecutableCommandLine.Arg(const name, description: RawUtf8): boolean;
begin
  result := Arg([name], description);
end;

function TExecutableCommandLine.Arg(const name: array of RawUtf8;
  const description: RawUtf8): boolean;
begin
  result := Find(name, clkArg, description) >= 0;
end;

function TExecutableCommandLine.Option(const name, description: RawUtf8): boolean;
begin
  result := Find([name], clkOption, description) >= 0;
end;

function TExecutableCommandLine.Option(const name: array of RawUtf8;
  const description: RawUtf8): boolean;
begin
  result := Find(name, clkOption, description) >= 0;
end;

function TExecutableCommandLine.Get(const name: RawUtf8; out value: RawUtf8;
  const description, default: RawUtf8): boolean;
begin
  result := Get([name], value, description, default);
end;

procedure AddRawUtf8(var Values: TRawUtf8DynArray; const Value: RawUtf8);
var
  n: PtrInt;
begin
  n := length(Values);
  SetLength(Values, n + 1);
  Values[n] := Value;
end;

function TExecutableCommandLine.Get(const name: array of RawUtf8;
  out value: TRawUtf8DynArray; const description: RawUtf8): boolean;
var
  first, i: PtrInt;
begin
  result := false;
  if self = nil then
    exit;
  Describe(name, clkParam, description, '', -1);
  first := 0;
  repeat
    i := Find(name, clkParam, '', '', first);
    if i < 0 then
      break;
    AddRawUtf8(value, fValues[i]);
    result := true;
    first := i + 1;
  until first >= length(fValues);
end;

function TExecutableCommandLine.Get(const name: array of RawUtf8;
  out value: RawUtf8; const description, default: RawUtf8): boolean;
var
  i: PtrInt;
begin
  if self = nil then
    i := -1
  else
    i := Find(name, clkParam, description, default);
  if i >= 0 then
  begin
    value := Values[i];
    result := true;
  end
  else
  begin
    value := default;
    result := false;
  end;
end;

function TExecutableCommandLine.Get(const name: RawUtf8; out value: string;
  const description: RawUtf8; const default: string): boolean;
begin
  result := Get([name], value, description, default);
end;

function TExecutableCommandLine.Get(const name: array of RawUtf8;
  out value: string; const description: RawUtf8; const default: string): boolean;
var
  tmp: RawUtf8;
begin
  result := Get(name, tmp, description);
  if result then
    value := string(tmp)
  else
    value := default; // no conversion needed
end;

function TExecutableCommandLine.Get(const name: RawUtf8;
  out value: TStringDynarray; const description: RawUtf8): boolean;
begin
  result := Get([name], value, description);
end;

function TExecutableCommandLine.Get(const name: array of RawUtf8;
  out value: TStringDynarray; const description: RawUtf8): boolean;
var
  tmp: TRawUtf8DynArray;
  i: PtrInt;
begin
  result := Get(name, tmp, description);
  SetLength(value, length(tmp));
  for i := 0 to length(tmp) - 1 do
    value[i] := string(tmp[i]);
end;

function TExecutableCommandLine.Get(const name: RawUtf8;
  out value: integer; const description: RawUtf8; default: integer): boolean;
begin
  result := Get([name], value, description, default);
end;

function defI(default: integer): RawUtf8;
begin
  if default = maxInt then
    result := ''
  else
    result := RawUtf8(IntToStr(default));
end;

function TExecutableCommandLine.Get(const name: array of RawUtf8;
  out value: integer; const description: RawUtf8; default: integer): boolean;
var
  i: PtrInt;
begin
  if self = nil then
    i := -1
  else
    i := Find(name, clkParam, description, defI(default));
  result := (i >= 0) and
            ToInteger(Values[i], value);
  if not result and
     (default <> maxInt) then
    value := default;
end;

function TExecutableCommandLine.Get(const name: RawUtf8; min, max: integer;
  out value: integer; const description: RawUtf8; default: integer): boolean;
begin
  result := Get([name], min, max, value, description, default);
end;

function TExecutableCommandLine.Get(const name: array of RawUtf8;
  min, max: integer; out value: integer; const description: RawUtf8;
  default: integer): boolean;
begin
  result := Get(name, value, description, default) and
            (value >= min) and
            (value <= max);
end;

function TExecutableCommandLine.Has(const name: RawUtf8): boolean;
begin
  result := Find([name], clkParam) >= 0;
end;

function TExecutableCommandLine.Has(const name: array of RawUtf8): boolean;
begin
  result := Find(name, clkParam) >= 0;
end;

function TExecutableCommandLine.Param(
  const name, description, default: RawUtf8): RawUtf8;
begin
  Get([name], result, description, default);
end;

function TExecutableCommandLine.Param(const name: array of RawUtf8;
  const description, default: RawUtf8): RawUtf8;
begin
  Get(name, result, description, default);
end;

function TExecutableCommandLine.ParamS(const name: array of RawUtf8;
  const description: RawUtf8; const default: string): string;
begin
  Get(name, result, description, default);
end;

function TExecutableCommandLine.Param(const name: RawUtf8;
  default: integer; const description: RawUtf8): integer;
begin
  Get([name], result, description, default);
end;

function TExecutableCommandLine.Param(const name: array of RawUtf8;
  default: integer;const description: RawUtf8): integer;
begin
  Get(name, result, description, default);
end;

const
  CLK_TXT: array[clkOption .. clkParam] of RawUtf8 = (
    ' [options]', ' [params]');
  CLK_DESCR: array[clkOption .. clkParam] of RawUtf8 = (
    'Options', 'Params');
  CASE_DESCR: array[boolean] of RawUtf8 = (
    ':', ' (case-sensitive):');

function TExecutableCommandLine.FullDescription(
  const customexedescription, exename, onlyusage: RawUtf8): RawUtf8;
var
  clk: TExecutableCommandLineKind;
begin
  if customexedescription <> '' then
    fExeDescription := customexedescription;
  result := fExeDescription + fLineFeed + fLineFeed + 'Usage: ';
  if exename = '' then
    result := result + Executable.ProgramName
  else
    result := result + exename;
  result := result + fDesc[clkArg];
  for clk := low(CLK_TXT) to high(CLK_TXT) do
    if fDesc[clk] <> '' then
      result := result + CLK_TXT[clk];
  result := result + fLineFeed;
  if onlyusage <> '' then
    result := result + onlyusage
  else
    for clk := low(fDescDetail) to high(fDescDetail) do
      if fDescDetail[clk] <> '' then
      begin
        if clk in [low(CLK_TXT) .. high(CLK_TXT)] then
          result := result + fLineFeed +
                    CLK_DESCR[clk] + CASE_DESCR[CaseSensitiveNames];
        result := result + fLineFeed + fDescDetail[clk];
      end;
end;

function TExecutableCommandLine.DetectUnknown: RawUtf8;
var
  clk: TExecutableCommandLineKind;
  i: PtrInt;
begin
  result := '';
  for clk := low(fRetrieved) to high(fRetrieved) do
    for i := 0 to length(fRetrieved[clk]) - 1 do
      if not fRetrieved[clk][i] then
        if clk = clkArg then
          result := result + 'Missing <' + fDescArg[i] + '> argument' + fLineFeed
        else
        begin
          result := result + 'Unexpected ' + SwitchAsText(fNames[clk][i]) + ' ';
          case clk of
            clkOption:
              result := result + 'option';
            clkParam:
              result := result + fValues[i] + ' parameter';
          end;
          result := result + fLineFeed;
        end;
end;

function TExecutableCommandLine.ConsoleWriteUnknown(
  const exedescription: RawUtf8): boolean;
var
  err: RawUtf8;
begin
  err := DetectUnknown;
  result := err <> '';
  if not result then
    exit;
  ConsoleWrite(FullDescription(exedescription));
  ConsoleWrite(err, ccLightRed);
  TextColor(ccLightGray);
end;

function TExecutableCommandLine.ConsoleHelpFailed(
  const exedescription: RawUtf8): boolean;
begin
  if exedescription <> '' then
    fExeDescription := exedescription;
  result := Option(['h', 'help'], 'display this help');
  if result then
    ConsoleWrite(FullDescription)
  else
    result := ConsoleWriteUnknown(exedescription);
end;

procedure TExecutableCommandLine.Clear;
begin
  CleanupInstance; // finalize all TRawUtf8DynArray fields
end;

function TExecutableCommandLine.Parse(
  const DescriptionLineFeed, ShortSwitch, LongSwitch: RawUtf8): boolean;
var
  i, j, n: PtrInt;
  swlen: TByteDynArray;
  s: RawUtf8;
begin
  result := false;
  fLineFeed := DescriptionLineFeed;
  if (ShortSwitch = '') or
     (LongSwitch  = '') then
    exit;
  fSwitch[false] := ShortSwitch;
  fSwitch[true]  := LongSwitch;
  if fRawParams = nil then
  begin
    n := ParamCount;
    if n <= 0 then
      exit; // may equal -1 e.g. from a .so on MacOS
    SetLength(fRawParams, n);
    for i := 0 to n - 1 do
      fRawParams[i] := RawUtf8(ParamStr(i + 1));
  end;
  n := length(fRawParams);
  if n = 0 then
  begin
    result := true;
    exit;
  end;
  SetLength(swlen, n);
  for i := 0 to n - 1 do
  begin
    s := fRawParams[i];
    if s <> '' then
      if CompareMemSmall(pointer(s), pointer(LongSwitch), length(LongSwitch)) then
        swlen[i] := length(LongSwitch)
      else if CompareMemSmall(pointer(s), pointer(ShortSwitch), length(ShortSwitch)) then
        swlen[i] := length(ShortSwitch)
      {$ifdef OSWINDOWS}
      else while s[swlen[i] + 1] = '-' do
        inc(swlen[i]); // allow -v --verbose on Windows for cross-platform run
      {$endif OSWINDOWS}
  end;
  i := 0;
  repeat
    s := fRawParams[i];
    if s <> '' then
      if swlen[i] <> 0 then
      begin
        delete(s, 1, swlen[i]);
        if s <> '' then
        begin
          j := PosExChar('=', s);
          if j <> 1 then
            if j <> 0 then
            begin
              AddRawUtf8(fNames[clkParam], copy(s, 1, j - 1));
              AddRawUtf8(fValues, copy(s, j + 1, MaxInt));
            end
            else if (i + 1 = n) or
                    (swlen[i + 1] <> 0) then
              AddRawUtf8(fNames[clkOption], s)
            else
            begin
              AddRawUtf8(fNames[clkParam], s);
              inc(i);
              AddRawUtf8(fValues, fRawParams[i]);
            end;
          end;
      end
      else
        AddRawUtf8(fNames[clkArg], s);
    inc(i);
  until i = n;
  SetLength(fRetrieved[clkArg],    length(fNames[clkArg]));
  SetLength(fRetrieved[clkOption], length(fNames[clkOption]));
  SetLength(fRetrieved[clkParam],  length(fNames[clkParam]));
  result := true;
end;

var
  _SystemPath: array[TSystemPath] of TFileName; // GetSystemPath() cache

function GetSystemPath(kind: TSystemPath): TFileName;
begin
  result := _SystemPath[kind];
  if result <> '' then
    exit;
  _ComputeSystemPath(kind, result); // in os.posix.inc or os.windows.inc
  _SystemPath[kind] := result;
end;

function SetSystemPath(kind: TSystemPath; const path: TFileName): boolean;
var
  full: TFileName;
begin
  full := ExpandFileName(ExcludeTrailingPathDelimiter(path));
  result := DirectoryExists(full);
  if result then
    _SystemPath[kind] := IncludeTrailingPathDelimiter(full);
end;

function _GetExecutableLocation(aAddress: pointer): ShortString;
var
  i: PtrInt;
begin // return the address as hexadecimal - hexstr() is not available on Delphi
  result[0] := #0;
  for i := SizeOf(aAddress) - 1 downto 0 do
    AppendShortByteHex(PByteArray(aAddress)[i], result);
end; // mormot.core.log.pas will properly decode debug info - and handle .mab

var
  _SystemStoreAsPemSafe: TLightLock;
  _OneSystemStoreAsPem: array[TSystemCertificateStore] of record
    Tix: cardinal;
    Pem: RawUtf8;
  end;
  _SystemStoreAsPem: record
    Tix: cardinal;
    Scope: TSystemCertificateStores;
    Pem: RawUtf8;
  end;

function GetOneSystemStoreAsPem(CertStore: TSystemCertificateStore;
  FlushCache: boolean; now: cardinal): RawUtf8;
begin
  if now = 0 then
    now := GetTickCount64 shr 18 + 1; // div 262.144 seconds = every 4.4 min
  _SystemStoreAsPemSafe.Lock;
  try
    // first search if not already in cache
    with _OneSystemStoreAsPem[CertStore] do
    begin
      if not FlushCache then
        if Tix = now then
        begin
          result := Pem; // quick retrieved from cache
          exit;
        end;
      // fallback search depending on the POSIX / Windows specific OS
      result := _GetSystemStoreAsPem(CertStore); // implemented in each .inc
      Tix := now;
      Pem := result;
    end;
  finally
    _SystemStoreAsPemSafe.UnLock;
  end;
end;

function GetSystemStoreAsPem(CertStores: TSystemCertificateStores;
  FlushCache, OnlySystemStore: boolean): RawUtf8;
var
  now: cardinal;
  s: TSystemCertificateStore;
  v: RawUtf8;
begin
  result := '';
  now := GetTickCount64 shr 18 + 1;
  _SystemStoreAsPemSafe.Lock;
  try
    // first search if not already in cache
    if not FlushCache then
      with _SystemStoreAsPem do
        if (Tix = now) and
           (Scope = CertStores) and
           (Pem <> '') then
        begin
          result := Pem; // quick retrieved from cache
          exit;
        end;
    // load from a file, bounded within the application or from env variable
    if not OnlySystemStore then
    begin
      if GetSystemStoreAsPemLocalFile <> '' then
        {$ifdef OSPOSIX}
        if GetSystemStoreAsPemLocalFile[1] = '/' then // full /posix/path
        {$else}
        if GetSystemStoreAsPemLocalFile[2] = ':' then // 'C:\path\to\file.pem'
        {$endif OSPOSIX}
          result := StringFromFile(GetSystemStoreAsPemLocalFile)
        else
          result := StringFromFile(
            Executable.ProgramFilePath + GetSystemStoreAsPemLocalFile);
      if result = '' then
        result := StringFromFile(GetEnvironmentVariable('SSL_CA_CERT_FILE'));
    end;
  finally
    _SystemStoreAsPemSafe.UnLock; // GetOneSystemStoreAsPem() blocks
  end;
  // fallback to search depending on the POSIX / Windows specific OS stores
  if result = '' then
    for s := low(s) to high(s) do
      if s in CertStores then
      begin
        v := GetOneSystemStoreAsPem(s, FlushCache, now);
        if v <> '' then
          result := result + v + #13#10;
      end;
  if result <> '' then
  begin
    _SystemStoreAsPemSafe.Lock;
    try
      with _SystemStoreAsPem do
      begin
        Tix := now;
        Scope := CertStores;
        Pem := result;
      end;
    finally
      _SystemStoreAsPemSafe.UnLock;
    end;
  end;
end;

{$ifdef CPUINTEL} // don't mess with raw SMBIOS encoding outside of Intel/AMD

// from DSP0134 3.6.0 System Management BIOS (SMBIOS) Reference Specification
const
  SMB_ANCHOR  = $5f4d535f;  // _SM_
  SMB_INT4    = $494d445f;  // _DMI
  SMB_INT5    = $5f;        // _
  SMB_ANCHOR4 = $334d535f;  // _SM3
  SMB_ANCHOR5 = $5f;        // _

type
  TSmbEntryPoint32 = packed record
    Anchor: cardinal;  // = SMB_ANCHOR
    Checksum: byte;
    Length: byte;
    MajVers: byte;
    MinVers: byte;
    MaxSize: word;
    Revision: byte;
    PadTo16: array[1 .. 5] of byte;
    IntAnch4: cardinal; // = SMB_INT4
    IntAnch5: byte;     // = SMB_INT5
    IntChecksum: byte;
    StructLength: word;
    StructAddr: cardinal;
    NumStruct: word;
    BcdRevision: byte;
  end;
  PSmbEntryPoint32 = ^TSmbEntryPoint32;

  TSmbEntryPoint64 = packed record
    Anch4: cardinal; // = SMB_ANCHOR4
    Anch5: byte;     // = SMB_ANCHOR5
    Checksum: byte;
    Length: byte;
    MajVers: byte;
    MinVers: byte;
    DocRev: byte;
    Revision: byte;
    Reserved: byte;
    StructMaxLength: cardinal;
    StructAddr: QWord;
  end;
  PSmbEntryPoint64 = ^TSmbEntryPoint64;

function GetRawSmbios32(p: PSmbEntryPoint32; var info: TRawSmbiosInfo): PtrUInt;
var
  cs: byte;
  i: PtrInt;
begin
  cs := 0;
  for i := 0 to p^.Length - 1 do
    inc(cs, PByteArray(p)[i]);
  if cs <> 0 then
  begin
    result := 0; // invalid checksum
    exit;
  end;
  result := p^.StructAddr;
  info.SmbMajorVersion := p^.MajVers;
  info.SmbMinorVersion := p^.MinVers;
  info.DmiRevision := p^.Revision; // 0 = SMBIOS 2.1
  info.Length := p^.StructLength;
end;

function GetRawSmbios64(p: PSmbEntryPoint64; var info: TRawSmbiosInfo): PtrUInt;
var
  cs: byte;
  i: PtrInt;
begin
  cs := 0;
  for i := 0 to p^.Length - 1 do
    inc(cs, PByteArray(p)[i]);
  if cs <> 0 then
  begin
    result := 0;
    exit;
  end;
  result := p^.StructAddr;
  info.SmbMajorVersion := p^.MajVers;
  info.SmbMinorVersion := p^.MinVers;
  info.DmiRevision := p^.Revision; // 1 = SMBIOS 3.0
  info.Length := p^.StructMaxLength;
end;

// caller should then try to decode SMB from pointer(result) + info.Len
function SearchSmbios(const mem: RawByteString; var info: TRawSmbiosInfo): PtrUInt;
var
  p, pend: PSmbEntryPoint32;
begin
  result := 0;
  if mem = '' then
    exit;
  p := pointer(mem);
  pend := @PByteArray(mem)[length(mem) - SizeOf(p^)];
  repeat
    if (p^.Anchor = SMB_ANCHOR) and
       (p^.IntAnch4 = SMB_INT4) and
       (p^.IntAnch5 = SMB_INT5) then
    begin
      result := GetRawSmbios32(p, info);
      if result <> 0 then
        exit;
    end
    else if (p^.Anchor = SMB_ANCHOR4) and
            (p^.Checksum = SMB_ANCHOR5) then
    begin
      result := GetRawSmbios64(pointer(p), info);
      if result <> 0 then
        exit; // here info.Length = max length
    end;
    inc(PHash128(p)); // search on 16-byte (paragraph) boundaries
  until PtrUInt(p) >= PtrUInt(pend);
end;

{$endif CPUINTEL}

procedure ComputeGetSmbios;
begin
  GlobalLock; // thread-safe retrieval
  try
    if not _SmbiosRetrieved then
    begin
      _SmbiosRetrieved := true;
      Finalize(RawSmbios.Data);
      FillCharFast(RawSmbios, SizeOf(RawSmbios), 0);
      if _GetRawSmbios(RawSmbios) then // OS specific call
         if DecodeSmbios(RawSmbios, _Smbios) <> 0 then
         begin
           // we were able to retrieve and decode SMBIOS information
           {$ifdef OSPOSIX}
           _AfterDecodeSmbios(RawSmbios); // persist in SMB_CACHE for non-root
           {$endif OSPOSIX}
           exit;
         end;
      // if not root on POSIX, SMBIOS is not available
      // -> try to get what the OS exposes (Linux, MacOS or FreeBSD)
      DirectSmbiosInfo(_Smbios);
    end;
  finally
    GlobalUnLock;
  end;
end;

function GetRawSmbios: boolean;
begin
  if not _SmbiosRetrieved then
    ComputeGetSmbios; // fill both RawSmbios and _Smbios[]
  result := RawSmbios.Data <> '';
end;

function GetSmbios(info: TSmbiosBasicInfo): RawUtf8;
begin
  if not _SmbiosRetrieved then
    ComputeGetSmbios; // fill both RawSmbios and _Smbios[]
  result := _Smbios[info];
end;

{$ifdef ISDELPHI} // missing convenient RTL function in Delphi
function TryStringToGUID(const s: string; var uuid: TGuid): boolean;
begin
  try
    uuid := StringToGUID(s);
    result := true;
  except
    result := false;
  end;
end;
{$endif ISDELPHI}

procedure GetComputerUuid(out uuid: TGuid);
var
  n, i: PtrInt;
  u: THash128Rec absolute uuid;
  s: RawByteString;
  fn: TFileName;
  mac: TRawUtf8DynArray;

  procedure crctext(const s: RawUtf8);
  begin
    if s = '' then
      exit;
    u.c[n] := crc32c(u.c[n], pointer(s), length(s));
    n := (n + 1) and 3; // update only 32-bit of UUID per crctext() call
  end;

begin
  // first try to retrieve the Machine BIOS UUID
  if not _SmbiosRetrieved then
    ComputeGetSmbios; // maybe from local SMB_CACHE file for non-root
  if (_Smbios[sbiUuid] <> '') and
     TryStringToGUID('{' + string(_Smbios[sbiUuid]) + '}', uuid) then
    exit;
  // did we already compute this UUID?
  fn := UUID_CACHE;
  s := StringFromFile(fn);
  if length(s) = SizeOf(uuid) then
  begin
    uuid := PGuid(s)^; // seems to be a valid UUID binary blob
    exit;
  end;
  // no known UUID: compute and store a 128-bit hash from HW specs
  // which should remain identical even between full OS reinstalls
  // note: /etc/machine-id is no viable alternative since it is from SW random
  {$ifdef CPUINTELARM}
  crc128c(@CpuFeatures, SizeOf(CpuFeatures), u.b);
  {$else}
  s := CPU_ARCH_TEXT;
  crc128c(pointer(s), length(s), u.b); // rough starting point
  {$endif CPUINTELARM}
  if RawSmbios.Data <> '' then // some bios have no uuid but some HW info
    crc32c128(@u.b, pointer(RawSmbios.Data), length(RawSmbios.Data));
  n := 0;
  for i := 0 to length(_Smbios) - 1 do // some of _Smbios[] may be set
    crctext(PRawUtf8Array(@_Smbios)[i]);
  crctext(CpuCacheText);
  crctext(BiosInfoText);
  crctext(CpuInfoText);
  if Assigned(GetSystemMacAddress) then
    // from mormot.net.sock or mormot.core.os.posix.inc for Linux only
    mac := GetSystemMacAddress;
  if mac <> nil then
  begin
    // MAC should make it unique at least over the local network
    for i := 0 to high(mac) do
      crctext(mac[i]);
    // we have enough unique HW information to store it locally for next startup
    // note: RawSmbios.Data may not be genuine e.g. between VMs
    if FileFromBuffer(@u, SizeOf(u), fn) then
      FileSetSticky(fn); // use S_ISVTX so that file is not removed from /var/tmp
  end
  else
    // unpersisted fallback if mormot.net.sock is not included (very unlikely)
    crctext(Executable.Host);
end;

procedure DecodeSmbiosUuid(src: PGuid; out dest: RawUtf8; const raw: TRawSmbiosInfo);
var
  uid: TGuid;
begin
  uid := src^;
  // reject full $00 = unsupported or full $ff = not set
  if IsZero(@uid, SizeOf(uid)) or
     ((PCardinalArray(@uid)[0] = $ffffffff) and
      (PCardinalArray(@uid)[1] = $ffffffff) and
      (PCardinalArray(@uid)[2] = $ffffffff) and
      (PCardinalArray(@uid)[3] = $ffffffff)) then
    exit;
  // GUIDToString() already displays the first 4 bytes as little-endian
  // - we don't need to swap those bytes as dmi_system_uuid() in dmidecode.c
  // on Windows, to match "wmic csproduct get uuid" official value
  // - on MacOs, sduInvert is set to match IOPlatformUUID value from ioreg :(
  if (_SmbiosDecodeUuid = sduInvert) or
  // - dmi_save_uuid() from the Linux kernel do check for SMBIOS 2.6 version
  // https://elixir.bootlin.com/linux/latest/source/drivers/firmware/dmi_scan.c
     ((_SmbiosDecodeUuid = sduVersion) and
      (raw.SmbMajorVersion shl 8 + raw.SmbMinorVersion < $0206)) then
  begin
    uid.D1 := bswap32(uid.D1);
    uid.D2 := swap(uid.D2);
    uid.D3 := swap(uid.D3);
  end;
  dest := RawUtf8(UpperCase(copy(GUIDToString(uid), 2, 36)));
end;

function DecodeSmbios(var raw: TRawSmbiosInfo; out info: TSmbiosBasicInfos): PtrInt;
var
  lines: array[byte] of TSmbiosBasicInfo; // single pass efficient decoding
  len, trimright: PtrInt;
  cur: ^TSmbiosBasicInfo;
  s, sEnd: PByteArray;
begin
  result := 0;
  Finalize(info);
  s := pointer(raw.Data);
  if s = nil then
    exit;
  sEnd := @s[length(raw.Data)];
  FillCharFast(lines, SizeOf(lines), 0);
  repeat
    if (s[0] = 127) or // type (127=EOT)
       (s[1] < 4) or   // length
       (PtrUInt(@s[s[1]]) > PtrUInt(sEnd)) then
    begin
      s := @s[2]; // truncate to the exact end of DMI/SMBIOS input
      break;
    end;
    case s[0] of
      0: // Bios Information (type 0)
        begin
          lines[s[4]] := sbiBiosVendor;
          lines[s[5]] := sbiBiosVersion;
          lines[s[8]] := sbiBiosDate;
          if s[1] >= $17 then // 2.4+
          begin
            _fmt('%d.%d', [s[$14], s[$15]], info[sbiBiosRelease]);
            _fmt('%d.%d', [s[$16], s[$17]], info[sbiBiosFirmware]);
          end;
        end;
      1: // System Information (type 1)
        begin
          lines[s[4]] := sbiManufacturer;
          lines[s[5]] := sbiProductName;
          lines[s[6]] := sbiVersion;
          lines[s[7]] := sbiSerial;
          if s[1] >= $18 then // 2.1+
          begin
            DecodeSmbiosUuid(@s[8], info[sbiUuid], raw);
            if s[1] >= $1a then // 2.4+
            begin
              lines[s[$19]] := sbiSku;
              lines[s[$1a]] := sbiFamily;
            end;
          end;
        end;
      2: // Baseboard (or Module) Information (type 2) - keep only the first
        begin
          lines[s[4]] := sbiBoardManufacturer;
          lines[s[5]] := sbiBoardProductName;
          lines[s[6]] := sbiBoardVersion;
          lines[s[7]] := sbiBoardSerial;
          lines[s[8]] := sbiBoardAssetTag;
          lines[s[10]] := sbiBoardLocation;
        end;
      4: // Processor Information (type 4) - keep only the first
        begin
          lines[s[7]] := sbiCpuManufacturer;
          lines[s[$10]] := sbiCpuVersion;
          if s[1] >= $22 then // 2.3+
          begin
            lines[s[$20]] := sbiCpuSerial;
            lines[s[$21]] := sbiCpuAssetTag;
            lines[s[$22]] := sbiCpuPartNumber;
          end;
        end;
      11: // OEM Strings (Type 11) - keep only the first
        if s[4] <> 0 then
          lines[1] := sbiOem; // e.g. 'vboxVer_6.1.36'
      22: // Portable Battery (type 22) - keep only the first
        if s[1] >= $0f then // 2.1+
        begin
          lines[s[4]] := sbiBatteryLocation;
          lines[s[5]] := sbiBatteryManufacturer;
          lines[s[8]] := sbiBatteryName;
          lines[s[$0e]] := sbiBatteryVersion;
          if s[1] >= $14 then // 2.2+
            lines[s[$14]] := sbiBatteryChemistry;
        end;
    end;
    s := @s[s[1]]; // go to string table
    cur := @lines[1];
    if s[0] = 0 then
      inc(PByte(s)) // no string table
    else
      repeat
        len := StrLen(s);
        if cur^ <> sbiUndefined then
        begin
          if info[cur^] = '' then // only set the first occurrence if multiple
          begin
            trimright := len;
            while (trimright <> 0) and
                  (s[trimright - 1] <= ord(' ')) do
              dec(trimright);
            FastSetString(info[cur^], s, trimright);
          end;
          cur^ := sbiUndefined; // reset slot in lines[]
        end;
        s := @s[len + 1]; // next string
        inc(cur);
      until s[0] = 0; // end of string table
    inc(PByte(s)); // go to next structure
  until false;
  // compute the exact DMI/SMBIOS size, and adjust the raw.Data length
  result := PtrUInt(s) - PtrUInt(raw.Data);
  raw.Length := result;
  if length(raw.Data) <> result then
    FakeSetLength(raw.Data, result);
end;


{ **************** TSynLocker Threading Features }

// as reference, take a look at Linus insight
// from https://www.realworldtech.com/forum/?threadid=189711&curpostid=189755
{$ifdef CPUINTEL}
procedure DoPause; {$ifdef FPC} assembler; nostackframe; {$endif}
asm
      pause
end;
{$endif CPUINTEL}

const
  {$ifdef CPUINTEL}
  SPIN_COUNT = 1000;
  {$else}
  SPIN_COUNT = 100; // since DoPause does nothing, switch to thread sooner
  {$endif CPUINTEL}

function DoSpin(spin: PtrUInt): PtrUInt;
  {$ifdef CPUINTEL} {$ifdef HASINLINE} inline; {$endif} {$endif}
  // on Intel, the pause CPU instruction would relax the core
  // on ARM/AARCH64, the not-inlined function call makes a small delay
begin
  {$ifdef CPUINTEL}
  DoPause;
  {$endif CPUINTEL}
  dec(spin);
  if spin = 0 then
  begin
    SwitchToThread; // fpnanosleep on POSIX
    spin := SPIN_COUNT;
  end;
  result := spin;
end;


{ TLightLock }

procedure TLightLock.Init;
begin
  Flags := 0;
end;

procedure TLightLock.Done;
begin // just for compatibility with TOSLock
end;

procedure TLightLock.Lock;
begin
  // we tried a dedicated asm but it was slower: inlining is preferred
  if not LockedExc(Flags, 1, 0) then
    LockSpin;
end;

procedure TLightLock.UnLock;
begin
  {$ifdef CPUINTEL}
  Flags := 0; // non reentrant locks need no additional thread safety
  {$else}
  LockedExc(Flags, 0, 1); // ARM can be weak-ordered
  // https://preshing.com/20121019/this-is-why-they-call-it-a-weakly-ordered-cpu
  {$endif CPUINTEL}
end;

function TLightLock.TryLock: boolean;
begin
  result := (Flags = 0) and // first check without any (slow) atomic opcode
            LockedExc(Flags, 1, 0);
end;

function TLightLock.IsLocked: boolean;
begin
  result := Flags <> 0;
end;

procedure TLightLock.LockSpin;
var
  spin: PtrUInt;
begin
  spin := SPIN_COUNT;
  repeat
    spin := DoSpin(spin);
  until TryLock;
end;


{ TRWLightLock }

procedure TRWLightLock.Init;
begin
  Flags := 0; // bit 0=WriteLock, >0=ReadLock counter
end;

procedure TRWLightLock.ReadLock;
var
  f: PtrUInt;
begin
  // if not writing, atomically increase the RD counter in the upper flag bits
  f := Flags and not 1; // bit 0=WriteLock, >0=ReadLock counter
  if not LockedExc(Flags, f + 2, f) then
    ReadLockSpin;
end;

function TRWLightLock.TryReadLock: boolean;
var
  f: PtrUInt;
begin
  // if not writing, atomically increase the RD counter in the upper flag bits
  f := Flags and not 1; // bit 0=WriteLock, >0=ReadLock counter
  result := LockedExc(Flags, f + 2, f);
end;

procedure TRWLightLock.ReadUnLock;
begin
  LockedDec(Flags, 2);
end;

procedure TRWLightLock.ReadLockSpin;
var
  spin: PtrUInt;
begin
  spin := SPIN_COUNT;
  repeat
    spin := DoSpin(spin);
  until TryReadLock;
end;

function TRWLightLock.TryWriteLock: boolean;
var
  f: PtrUInt;
begin
  f := Flags and not 1; // bit 0=WriteLock, >0=ReadLock
  result := (Flags = f) and
            LockedExc(Flags, f + 1, f);
end;

procedure TRWLightLock.WriteLock;
begin
  if not TryWriteLock then
    WriteLockSpin;
end;

procedure TRWLightLock.WriteUnLock;
begin
  LockedDec(Flags, 1);
end;

procedure TRWLightLock.WriteLockSpin;
var
  spin: PtrUInt;
begin
  spin := SPIN_COUNT;
  repeat
    spin := DoSpin(spin);
  until TryWriteLock;
end;


{ TRWLock }

procedure TRWLock.Init;
begin
  // bit 0 = WriteLock, 1 = ReadWriteLock, 2.. = ReadOnlyLock counter
  Flags := 0;
  // no need to set the other fields because they will be reset if Flags=0
end;

procedure TRWLock.AssertDone;
begin
  if Flags <> 0 then
    raise EOSException.CreateFmt('TRWLock Flags=%x', [Flags]);
end;

// dedicated asm for this most simple (and used) method
{$ifdef FPC_ASMX64} // some Delphi version was reported to fail with no clue why

procedure TRWLock.ReadOnlyLock;
asm     // stack frame is required since we may call SwitchToThread
        {$ifdef SYSVABI}
        mov     rcx, rdi      // rcx = self
        {$endif SYSVABI}
@retry: mov     r8d, SPIN_COUNT
@spin:  mov     rax, qword ptr [rcx + TRWLock.Flags]
        and     rax, not 1
        lea     rdx, [rax + 4]
   lock cmpxchg qword ptr [rcx + TRWLock.Flags], rdx
        jz      @done
        pause
        dec     r8d
        jnz     @spin
        push    rcx
        call    SwitchToThread
        pop     rcx
        jmp     @retry
@done:  // restore the stack frame
end;

{$else}

procedure TRWLock.ReadOnlyLock;
var
  f: PtrUInt;
begin
  // if not writing, atomically increase the RD counter in the upper flag bits
  f := Flags and not 1; // bit 0=WriteLock, 1=ReadWriteLock, >1=ReadOnlyLock
  if not LockedExc(Flags, f + 4, f) then
    ReadOnlyLockSpin;
end;

procedure TRWLock.ReadOnlyLockSpin;
var
  spin, f: PtrUInt;
begin
  spin := SPIN_COUNT;
  repeat
    spin := DoSpin(spin);
    f := Flags and not 1; // retry ReadOnlyLock
  until (Flags = f) and
        LockedExc(Flags, f + 4, f);
end;

{$endif FPC_ASMX64}

procedure TRWLock.ReadOnlyUnLock;
begin
  LockedDec(Flags, 4);
end;

procedure TRWLock.ReadWriteLock;
var
  spin, f: PtrUInt;
  tid: TThreadID;
begin
  tid := GetCurrentThreadId;
  if (Flags and 2 = 2) and
     (LastReadWriteLockThread = tid) then
  begin
    inc(LastReadWriteLockCount); // allow ReadWriteLock to be reentrant
    exit;
  end;
  // if not writing, atomically acquire the upgradable RD flag bit
  spin := SPIN_COUNT;
  repeat
    f := Flags and not 3; // bit 0=WriteLock, 1=ReadWriteLock, >1=ReadOnlyLock
    if (Flags = f) and
       LockedExc(Flags, f + 2, f) then
      break;
    spin := DoSpin(spin);
  until false;
  LastReadWriteLockThread := tid;
  LastReadWriteLockCount := 0;
end;

procedure TRWLock.ReadWriteUnLock;
begin
  if LastReadWriteLockCount <> 0 then
  begin
    dec(LastReadWriteLockCount);
    exit;
  end;
  LastReadWriteLockThread := TThreadID(0);
  LockedDec(Flags, 2);
end;

procedure TRWLock.WriteLock;
var
  spin, f: PtrUInt;
  tid: TThreadID;
begin
  tid := GetCurrentThreadId;
  if (Flags and 1 = 1) and
     (LastWriteLockThread = tid) then
  begin
    inc(LastWriteLockCount); // allow WriteLock to be reentrant
    exit;
  end;
  spin := SPIN_COUNT;
  // acquire the WR flag bit
  repeat
    f := Flags and not 1; // bit 0=WriteLock, 1=ReadWriteLock, >1=ReadOnlyLock
    if (Flags = f) and
       LockedExc(Flags, f + 1, f) then
      if (Flags and 2 = 2) and
         (LastReadWriteLockThread <> tid) then
        // there is a pending ReadWriteLock but not on this thread
        LockedDec(Flags, 1) // try again
      else
        // we exclusively acquired the WR lock
        break;
    spin := DoSpin(spin);
  until false;
  LastWriteLockThread := tid;
  LastWriteLockCount := 0;
  // wait for all readers to have finished their job
  while Flags > 3 do
    spin := DoSpin(spin);
end;

procedure TRWLock.WriteUnlock;
begin
  if LastWriteLockCount <> 0 then
  begin
    dec(LastWriteLockCount); // reentrant call
    exit;
  end;
  LastWriteLockThread := TThreadID(0);
  LockedDec(Flags, 1);
end;

procedure TRWLock.Lock(context: TRWLockContext);
begin
  if context = cReadOnly then
    ReadOnlyLock
  else if context = cReadWrite then
    ReadWriteLock
  else
    WriteLock;
end;

procedure TRWLock.UnLock(context: TRWLockContext);
begin
  if context = cReadOnly then
    ReadOnlyUnLock
  else if context = cReadWrite then
    ReadWriteUnLock
  else
    WriteUnLock;
end;


{ TOSLock }

procedure TOSLock.Init;
begin
  mormot.core.os.InitializeCriticalSection(CS);
end;

procedure TOSLock.Done;
begin
  DeleteCriticalSectionIfNeeded(CS);
end;

procedure TOSLock.Lock;
begin
  mormot.core.os.EnterCriticalSection(CS);
end;

function TOSLock.TryLock: boolean;
begin
  result := mormot.core.os.TryEnterCriticalSection(CS) <> 0;
end;

procedure TOSLock.UnLock;
begin
  mormot.core.os.LeaveCriticalSection(CS);
end;


{ TLockedList }

procedure TLockedList.Init(onesize: PtrUInt; const onefree: TOnLockedListOne);
begin
  FillCharFast(self, SizeOf(self), 0);
  fSize := onesize;
  fOnFree := onefree;
  fSequence := (Random32 shr 2) + 65536; // 65535 < sequence < MaxInt
end;

function LockedListFreeAll(o: PLockedListOne; const OnFree: TOnLockedListOne): integer;
var
  next: PLockedListOne;
begin
  result := 0;
  while o <> nil do
  begin
    inc(result);
    next := o.next;
    if Assigned(OnFree) then
      OnFree(o);
    FreeMem(o);
    o := next;
  end;
end;

procedure TLockedList.Done;
begin
  Clear;
  EmptyBin;
end;

procedure TLockedList.Clear;
begin
  Safe.Lock;
  try
    LockedListFreeAll(fHead, fOnFree);
    fHead := nil;
    Count := 0;
  finally
    Safe.UnLock;
  end;
end;

function TLockedList.EmptyBin: integer;
begin
  Safe.Lock;
  try
    result := LockedListFreeAll(fBin, nil);
    fBin := nil;
  finally
    Safe.UnLock;
  end;
end;

function TLockedList.New: pointer;
begin
  Safe.Lock;
  try
    // try to recycle from single-linked list bin, or allocate
    result := fBin;
    if result <> nil then
      fBin := PLockedListOne(result).next
    else
      result := AllocMem(fSize);
    PLockedListOne(result).sequence := fSequence;
    inc(fSequence); // protected by Safe.Lock
    // insert at beginning of the main double-linked list
    PLockedListOne(result).next := fHead;
    if fHead <> nil then
      PLockedListOne(fHead).prev := result;
    fHead := result;
    inc(Count);
  finally
    Safe.UnLock;
  end;
end;

function TLockedList.Free(one: pointer): boolean;
var
  o: PLockedListOne absolute one;
begin
  result := false;
  if (o = nil) or
     (o^.sequence = 0) then
    exit;
  Safe.Lock;
  try
    // remove from main double-linked list
    if o = fHead then
      fHead := o.next;
    if o.next <> nil then
      PLockedListOne(o.next).prev := o.prev;
    if o.prev <> nil then
      PLockedListOne(o.prev).next := o.next;
    // release internals and add to the recycle bin
    if Assigned(fOnFree) then
      fOnFree(o);
    FillCharFast(o^, fSize, 0); // garbage collect as void
    o.next := fBin;
    fBin := o;
    dec(Count);
  finally
    Safe.UnLock;
  end;
  result := true;
end;


{ TAutoLock }

constructor TAutoLock.Create(aLock: PSynLocker);
begin
  fLock := aLock;
  fLock^.Lock;
end;

destructor TAutoLock.Destroy;
begin
  fLock^.UnLock;
end;


{ TSynLocker }

function NewSynLocker: PSynLocker;
begin
  result := AllocMem(SizeOf(TSynLocker));
  InitializeCriticalSection(result^.fSection);
  result^.fInitialized := true;
end;

procedure TSynLocker.Init;
begin
  InitializeCriticalSection(fSection);
  fLockCount := 0;
  fPaddingUsedCount := 0;
  fInitialized := true;
  fRW.Init;
end;

procedure TSynLocker.Done;
var
  i: PtrInt;
begin
  for i := 0 to fPaddingUsedCount - 1 do
    if not (integer(Padding[i].VType) in VTYPE_SIMPLE) then
      VarClearProc(Padding[i]);
  DeleteCriticalSection(fSection);
  fInitialized := false;
end;

procedure TSynLocker.DoneAndFreeMem;
begin
  Done;
  FreeMem(@self);
end;

function TSynLocker.GetIsLocked: boolean;
begin
  case fRWUse of
    uSharedLock:
      result := fLockCount <> 0; // only updated by uSharedLock
    uRWLock:
      result := fRW.Flags = 0;   // no lock at all
  else
    result := false;             // uNoLock will never lock
  end;
end;

procedure TSynLocker.RWLock(context: TRWLockContext);
begin
  case fRWUse of
    uSharedLock:
      begin
        mormot.core.os.EnterCriticalSection(fSection);
        inc(fLockCount);
      end;
    uRWLock:
      fRW.Lock(context);
  end; // uNoLock will just do nothing
end;

procedure TSynLocker.RWUnLock(context: TRWLockContext);
begin
  case fRWUse of
    uSharedLock:
      begin
        dec(fLockCount);
        mormot.core.os.LeaveCriticalSection(fSection);
      end;
    uRWLock:
      fRW.UnLock(context);
  end; // uNoLock will just do nothing
end;

procedure TSynLocker.ReadLock;
begin
  RWLock(cReadOnly); // will be properly inlined
end;

procedure TSynLocker.ReadUnLock;
begin
  RWUnLock(cReadOnly);
end;

procedure TSynLocker.ReadWriteLock;
begin
  RWLock(cReadWrite);
end;

procedure TSynLocker.ReadWriteUnLock;
begin
  RWUnLock(cReadWrite);
end;

procedure TSynLocker.Lock;
begin
  RWLock(cWrite);
end;

procedure TSynLocker.UnLock;
begin
  RWUnLock(cWrite);
end;

function TSynLocker.TryLock: boolean;
begin
  result := (fRWUse = uSharedLock) and
            (mormot.core.os.TryEnterCriticalSection(fSection) <> 0);
  if result then
    inc(fLockCount);
end;

function TSynLocker.TryLockMS(retryms: integer; terminated: PBoolean): boolean;
var
  ms: integer;
  endtix: Int64;
begin
  result := TryLock;
  if result or
     (fRWUse <> uSharedLock) or
     (retryms <= 0) then
    exit;
  ms := 0;
  endtix := GetTickCount64 + retryms;
  repeat
    SleepHiRes(ms);
    result := TryLock;
    if result or
       ((terminated <> nil) and
        terminated^) then
      exit;
    ms := ms xor 1; // 0,1,0,1... seems to be good for scaling
  until GetTickCount64 > endtix;
end;

function TSynLocker.ProtectMethod: IUnknown;
begin
  result := TAutoLock.Create(@self);
end;

function TSynLocker.GetVariant(Index: integer): Variant;
begin
  if cardinal(Index) < cardinal(fPaddingUsedCount) then
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    RWLock(cReadOnly);
    result := variant(Padding[Index]);
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    RWUnLock(cReadOnly);
  end
  else
    VarClear(result);
end;

procedure TSynLocker.SetVariant(Index: integer; const Value: Variant);
begin
  if cardinal(Index) <= high(Padding) then
  try
    RWLock(cWrite);
    if Index >= fPaddingUsedCount then
      fPaddingUsedCount := Index + 1;
    variant(Padding[Index]) := Value;
  finally
    RWUnLock(cWrite);
  end;
end;

function TSynLocker.GetInt64(Index: integer): Int64;
begin
  if cardinal(Index) < cardinal(fPaddingUsedCount) then
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    RWLock(cReadOnly);
    if not VariantToInt64(variant(Padding[Index]), result) then
      result := 0;
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    RWUnLock(cReadOnly);
  end
  else
    result := 0;
end;

procedure TSynLocker.SetInt64(Index: integer; const Value: Int64);
begin
  SetVariant(Index, Value);
end;

function TSynLocker.GetBool(Index: integer): boolean;
begin
  if cardinal(Index) < cardinal(fPaddingUsedCount) then
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    RWLock(cReadOnly);
    if not VariantToBoolean(variant(Padding[Index]), result) then
      result := false;
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    RWUnLock(cReadOnly);
  end
  else
    result := false;
end;

procedure TSynLocker.SetBool(Index: integer; const Value: boolean);
begin
  SetVariant(Index, Value);
end;

function TSynLocker.GetUnlockedInt64(Index: integer): Int64;
begin
  if (cardinal(Index) >= cardinal(fPaddingUsedCount)) or
     not VariantToInt64(variant(Padding[Index]), result) then
    result := 0;
end;

procedure TSynLocker.SetUnlockedInt64(Index: integer; const Value: Int64);
begin
  if cardinal(Index) <= high(Padding) then
  begin
    if Index >= fPaddingUsedCount then
      fPaddingUsedCount := Index + 1;
    variant(Padding[Index]) := Value;
  end;
end;

function TSynLocker.GetPointer(Index: integer): Pointer;
begin
  if cardinal(Index) < cardinal(fPaddingUsedCount) then
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    RWLock(cReadOnly);
    with Padding[Index] do
      if VType = varUnknown then
        result := VUnknown
      else
        result := nil;
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    RWUnLock(cReadOnly);
  end
  else
    result := nil;
end;

procedure TSynLocker.SetPointer(Index: integer; const Value: Pointer);
begin
  if cardinal(Index) <= high(Padding) then
  try
    RWLock(cWrite);
    if Index >= fPaddingUsedCount then
      fPaddingUsedCount := Index + 1;
    with Padding[Index] do
    begin
      VarClearAndSetType(PVariant(@VType)^, varUnknown);
      VUnknown := Value;
    end;
  finally
    RWUnLock(cWrite);
  end;
end;

function TSynLocker.GetUtf8(Index: integer): RawUtf8;
begin
  if cardinal(Index) < cardinal(fPaddingUsedCount) then
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    RWLock(cReadOnly);
    VariantStringToUtf8(variant(Padding[Index]), result);
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    RWUnLock(cReadOnly);
  end
  else
    result := '';
end;

procedure TSynLocker.SetUtf8(Index: integer; const Value: RawUtf8);
begin
  if cardinal(Index) <= high(Padding) then
  try
    RWLock(cWrite);
    if Index >= fPaddingUsedCount then
      fPaddingUsedCount := Index + 1;
    RawUtf8ToVariant(Value, variant(Padding[Index]));
  finally
    RWUnLock(cWrite);
  end;
end;

function TSynLocker.LockedInt64Increment(Index: integer; const Increment: Int64): Int64;
begin
  if cardinal(Index) <= high(Padding) then
  try
    RWLock(cWrite);
    result := 0;
    if Index < fPaddingUsedCount then
      VariantToInt64(variant(Padding[Index]), result)
    else
      fPaddingUsedCount := Index + 1;
    variant(Padding[Index]) := Int64(result + Increment);
  finally
    RWUnLock(cWrite);
  end
  else
    result := 0;
end;

function TSynLocker.LockedExchange(Index: integer; const Value: variant): variant;
begin
  VarClear(result);
  if cardinal(Index) <= high(Padding) then
  try
    RWLock(cWrite);
    with Padding[Index] do
    begin
      if Index < fPaddingUsedCount then
        result := PVariant(@VType)^
      else
        fPaddingUsedCount := Index + 1;
      PVariant(@VType)^ := Value;
    end;
  finally
    RWUnLock(cWrite);
  end;
end;

function TSynLocker.LockedPointerExchange(Index: integer; Value: pointer): pointer;
begin
  if cardinal(Index) <= high(Padding) then
  try
    RWLock(cWrite);
    with Padding[Index] do
    begin
      if Index < fPaddingUsedCount then
        if VType = varUnknown then
          result := VUnknown
        else
        begin
          VarClear(PVariant(@VType)^);
          result := nil;
        end
      else
      begin
        fPaddingUsedCount := Index + 1;
        result := nil;
      end;
      VType := varUnknown;
      VUnknown := Value;
    end;
  finally
    RWUnLock(cWrite);
  end
  else
    result := nil;
end;



{ TSynLocked }

constructor TSynLocked.Create;
begin
  fSafe := NewSynLocker;
end;

destructor TSynLocked.Destroy;
begin
  inherited Destroy;
  fSafe^.DoneAndFreeMem;
end;


{ TSynEvent }

function TSynEvent.SleepStep(var start: Int64; terminated: PBoolean): Int64;
var
  ms: integer;
  endtix: Int64;
begin
  ms := SleepStepTime(start, result, @endtix);
  if (ms < 10) or
     (terminated = nil) then
    if ms = 0 then
      SleepHiRes(0) // < 16 ms is a pious wish on Windows anyway
    else
      WaitFor(ms)
  else
    repeat
      WaitFor(10);
      if terminated^ then
        exit;
      result := GetTickCount64;
    until result >= endtix;
end;

function TSynEvent.IsEventFD: boolean;
begin
  {$ifdef HASEVENTFD}
  result := fFD <> 0;
  {$else}
  result := false;
  {$endif HASEVENTFD}
end;


{ TLecuyerThreadSafe }

function TLecuyerThreadSafe.Next: cardinal;
begin
  Safe.Lock;
  result := Generator.Next;
  Safe.UnLock;
end;

function TLecuyerThreadSafe.NextDouble: double;
begin
  Safe.Lock;
  result := Generator.NextDouble;
  Safe.UnLock;
end;

procedure TLecuyerThreadSafe.Fill(dest: pointer; count: integer);
begin
  Safe.Lock;
  Generator.Fill(dest, count);
  Safe.UnLock;
end;

procedure TLecuyerThreadSafe.FillShort31(var dest: TShort31);
begin
  Fill(@dest, 32);
  FillAnsiStringFromRandom(@dest, 32);
end;


procedure GlobalLock;
begin
  mormot.core.os.EnterCriticalSection(GlobalCriticalSection.CS);
end;

procedure GlobalUnLock;
begin
  mormot.core.os.LeaveCriticalSection(GlobalCriticalSection.CS);
end;

var
  InternalGarbageCollection: record // RegisterGlobalShutdownRelease() list
    Instances:  TObjectDynArray;
    Count: integer;
    Shutdown: boolean; // paranoid check to avoid messing with Instances[]
  end;

function RegisterGlobalShutdownRelease(Instance: TObject;
  SearchExisting: boolean): pointer;
begin
  if not InternalGarbageCollection.Shutdown then
  begin
    GlobalLock;
    try
      with InternalGarbageCollection do
        if not SearchExisting or
           not PtrUIntScanExists(pointer(Instances), Count, PtrUInt(Instance)) then
          PtrArrayAdd(Instances, Instance, Count);
    finally
      GlobalUnLock;
    end;
  end;
  result := Instance;
end;

function SleepDelay(elapsed: PtrInt): PtrInt;
begin
  if elapsed < 50 then
    result := 0 // 10us on POSIX, SwitchToThread on Windows
  else if elapsed < 200 then
    result := 1
  else if elapsed < 500 then
    result := 5
  else if elapsed < 2000 then
    result := 50
  else
    result := 120 + Random32(130); // random 120-250 ms
end;

function SleepStepTime(var start, tix: Int64; endtix: PInt64): PtrInt;
begin
  tix := GetTickCount64;
  if (start = 0) or
     (tix < 50) then
    start := tix
  else if start < 0 then
    start := tix - 50; // ensure tix - start = elapsed is not < 50
  result := SleepDelay(tix - start);
  if endtix <> nil then
    endtix^ := tix + result;
end;

function SleepStep(var start: Int64; terminated: PBoolean): Int64;
var
  ms: integer;
  endtix: Int64;
begin
  ms := SleepStepTime(start, result, @endtix);
  if (ms < 10) or
     (terminated = nil) then
    SleepHiRes(ms) // < 16 ms is a pious wish on Windows anyway
  else
    repeat
      SleepHiRes(10); // on Windows, HW clock resolution is around 16 ms
      result := GetTickCount64;
    until (ms = 0) or
          terminated^ or
          (result >= endtix);
end;

function SleepHiRes(ms: cardinal; var terminated: boolean;
  terminatedvalue: boolean): boolean;
var
  start, endtix: Int64;
begin
  if terminated <> terminatedvalue then
    if ms < 20 then
      SleepHiRes(ms) // below HW clock resolution
    else
    begin
      start := GetTickCount64;
      endtix := start + ms;
      repeat
      until (terminated = terminatedvalue) or
            (SleepStep(start, @terminated) > endtix);
    end;
  result := terminated = terminatedvalue;
end;

procedure SpinExc(var Target: PtrUInt; NewValue, Comperand: PtrUInt);
var
  spin: PtrUInt;
begin
  spin := SPIN_COUNT;
  while (Target <> Comperand) or
        not LockedExc(Target, NewValue, Comperand) do
    spin := DoSpin(spin);
end;

function ObjArrayAdd(var aObjArray; aItem: TObject;
  var aSafe: TLightLock; aCount: PInteger): PtrInt;
begin
  aSafe.Lock;
  if aCount <> nil then
    result := PtrArrayAdd(aObjArray, aItem, aCount^)
  else
    result := PtrArrayAdd(aObjArray, aItem);
  aSafe.UnLock;
end;

function PtrArrayDelete(var aPtrArray; aItem: pointer; var aSafe: TLightLock;
  aCount: PInteger): PtrInt;
begin
  if pointer(aPtrArray) = nil then
  begin
    result := -1; // no need to lock anything
    exit;
  end;
  aSafe.Lock;
  result := PtrArrayDelete(aPtrArray, aItem, aCount);
  aSafe.UnLock;
end;

function SetCpuSet(var CpuSet: TCpuSet; CpuIndex: cardinal): boolean;
begin
  result := false;
  if (CpuIndex >= SizeOf(CpuSet) shl 3) or
     (CpuIndex >= SystemInfo.dwNumberOfProcessors) then
    exit;
  SetBitPtr(@CpuSet, CpuIndex);
  result := true;
end;

function CurrentCpuSet(out CpuSet: TCpuSet): integer;
begin
  ResetCpuSet(CpuSet);
  if GetMaskAffinity(CpuSet) then
    result := GetBitsCount(CpuSet, SizeOf(CpuSet) shl 3)
  else
    result := 0;
end;

function SetThreadCpuAffinity(Thread: TThread; CpuIndex: cardinal): boolean;
var
  mask: TCpuSet;
begin
  ResetCpuSet(mask);
  result := SetCpuSet(mask, CpuIndex) and
            SetThreadMaskAffinity(Thread, mask);
end;

function SetThreadSocketAffinity(Thread: TThread; SocketIndex: cardinal): boolean;
begin
  result := (SocketIndex < cardinal(length(CpuSocketsMask))) and
            SetThreadMaskAffinity(Thread, CpuSocketsMask[SocketIndex]);
end;

procedure _SetThreadName(ThreadID: TThreadID; const Format: RawUtf8;
  const Args: array of const);
begin
  // do nothing - properly implemented in mormot.core.log
end;

procedure SetCurrentThreadName(const Format: RawUtf8; const Args: array of const);
begin
  SetThreadName(GetCurrentThreadId, Format, Args);
end;

procedure SetCurrentThreadName(const Name: RawUtf8);
begin
  SetThreadName(GetCurrentThreadId, '%', [Name]);
end;

threadvar // do not publish for compilation within Delphi packages
  _CurrentThreadName: TShort31; // 31 chars is enough for our debug purpose

function CurrentThreadNameShort: PShortString;
begin
  result := @_CurrentThreadName;
end;

function GetCurrentThreadName: RawUtf8;
begin
  ShortStringToAnsi7String(_CurrentThreadName, result);
end;

function GetCurrentThreadInfo: ShortString;
begin
  result := ShortString(format('Thread %x [%s]',
    [PtrUInt(GetCurrentThreadId), _CurrentThreadName]));
end;


{ ****************** Unix Daemon and Windows Service Support }

const
  // hardcoded to avoid linking mormot.core.rtti for GetEnumName()
  _SERVICESTATE: array[TServiceState] of string[12] = (
    'NotInstalled',
    'Stopped',
    'Starting',
    'Stopping',
    'Running',
    'Resuming',
    'Pausing',
    'Paused',
    'Failed',
    'Error');

function ToText(st: TServiceState): PShortString; overload;
begin
  result := @_SERVICESTATE[st];
end;

function ExtractExecutableName(const cmd: RawUtf8; posix: boolean): RawUtf8;
var
  temp: RawUtf8;
  argv: TParseCommandsArgs;
  argc: integer;
begin
  if (pcInvalidCommand in ParseCommandArgs(cmd, @argv, @argc, @temp, posix)) or
     ({%H-}argc = 0) then
    result := ''
  else
    FastSetString(result, argv[0], StrLen(argv[0]));
end;

function ParseCommandArgs(const cmd: RawUtf8; argv: PParseCommandsArgs;
  argc: PInteger; temp: PRawUtf8; posix: boolean): TParseCommands;
var
  n: PtrInt;
  state: set of (sWhite, sInArg, sInSQ, sInDQ, sSpecial, sBslash);
  c: AnsiChar;
  D, P: PAnsiChar;
begin
  result := [pcInvalidCommand];
  if argv <> nil then
    argv[0] := nil;
  if argc <> nil then
    argc^ := 0;
  if cmd = '' then
    exit;
  if argv = nil then
    D := nil
  else
  begin
    if temp = nil then
      exit;
    SetLength(temp^, length(cmd));
    D := pointer(temp^);
  end;
  state := [];
  n := 0;
  P := pointer(cmd);
  repeat
    c := P^;
    if D <> nil then
      D^ := c;
    inc(P);
    case c of
      #0:
        begin
          if sInSQ in state then
            include(result, pcUnbalancedSingleQuote);
          if sInDQ in state then
            include(result, pcUnbalancedDoubleQuote);
          exclude(result, pcInvalidCommand);
          if argv <> nil then
            argv[n] := nil;
          if argc <> nil then
            argc^ := n;
          exit;
        end;
      #1 .. ' ':
        begin
         if state = [sInArg] then
         begin
           state := [];
           if D <> nil then
           begin
             D^ := #0;
             inc(D);
           end;
           continue;
         end;
         if state * [sInSQ, sInDQ] = [] then
           continue;
        end;
      '\':
        if posix and
           (state * [sInSQ, sBslash] = []) then
          if sInDQ in state then
          begin
            case P^ of
              '"', '\', '$', '`':
                begin
                  include(state, sBslash);
                  continue;
                end;
            end;
          end
          else if P^ = #0 then
          begin
            include(result, pcHasEndingBackSlash);
            exit;
          end
          else
          begin
            if D <> nil then
              D^ := P^;
            inc(P);
          end;
      '^':
        if not posix and
           (state * [sInSQ, sInDQ, sBslash] = []) then
          if PWord(P)^ = $0a0d then
          begin
            inc(P, 2);
            continue;
          end
          else if P^ = #0 then
          begin
            include(result, pcHasEndingBackSlash);
            exit;
          end
          else
          begin
            if D <> nil then
              D^ := P^;
            inc(P);
          end;
      '''':
        if posix and
           not(sInDQ in state) then
          if sInSQ in state then
          begin
            exclude(state, sInSQ);
            continue;
          end
          else if state = [] then
          begin
            if argv <> nil then
            begin
              argv[n] := D;
              inc(n);
              if n = high(argv^) then
                exit;
            end;
            state := [sInSQ, sInArg];
            continue;
          end
          else if state = [sInArg] then
          begin
            state := [sInSQ, sInArg];
            continue;
          end;
      '"':
        if not(sInSQ in state) then
          if sInDQ in state then
          begin
            exclude(state, sInDQ);
            continue;
          end
          else if state = [] then
          begin
            if argv <> nil then
            begin
              argv[n] := D;
              inc(n);
              if n = high(argv^) then
                exit;
            end;
            state := [sInDQ, sInArg];
            continue;
          end
          else if state = [sInArg] then
          begin
            state := [sInDQ, sInArg];
            continue;
          end;
      '|',
      '<',
      '>':
        if state * [sInSQ, sInDQ] = [] then
          include(result, pcHasRedirection);
      '&',
      ';':
        if posix and
           (state * [sInSQ, sInDQ] = []) then
        begin
          include(state, sSpecial);
          include(result, pcHasJobControl);
        end;
      '`':
        if posix and
           (state * [sInSQ, sBslash] = []) then
           include(result, pcHasSubCommand);
      '(',
      ')':
        if posix and
           (state * [sInSQ, sInDQ] = []) then
          include(result, pcHasParenthesis);
      '$':
        if posix and
           (state * [sInSQ, sBslash] = []) then
          if p^ = '(' then
            include(result, pcHasSubCommand)
          else
            include(result, pcHasShellVariable);
      '*',
      '?':
        if posix and
           (state * [sInSQ, sInDQ] = []) then
          include(result, pcHasWildcard);
    end;
    exclude(state, sBslash);
    if state = [] then
    begin
      if argv <> nil then
      begin
        argv[n] := D;
        inc(n);
        if n = high(argv^) then
          exit;
      end;
      state := [sInArg];
    end;
    if D <> nil then
      inc(D);
  until false;
end;

procedure TrimDualSpaces(var s: RawUtf8);
var
  f, i: integer;
begin
  f := 1;
  repeat
    i := PosEx('  ', s, f);
    if i = 0 then
      break;
    delete(s, i, 1); // dual space -> single space
    f := i;
  until false;
  TrimSelf(s);
end;


procedure InitializeUnit;
begin
  {$ifdef ISFPC27}
  SetMultiByteConversionCodePage(CP_UTF8);
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
  {$endif ISFPC27}
  GlobalCriticalSection.Init;
  ConsoleCriticalSection.Init;
  InitializeSpecificUnit; // in mormot.core.os.posix/windows.inc files
  TrimDualSpaces(OSVersionText);
  TrimDualSpaces(OSVersionInfoEx);
  TrimDualSpaces(BiosInfoText);
  TrimDualSpaces(CpuInfoText);
  OSVersionShort := ToTextOS(OSVersionInt32);
  InitializeExecutableInformation;
  JSON_CONTENT_TYPE_VAR := JSON_CONTENT_TYPE;
  JSON_CONTENT_TYPE_HEADER_VAR := JSON_CONTENT_TYPE_HEADER;
  NULL_STR_VAR := 'null';
  BOOL_UTF8[false] := 'false';
  BOOL_UTF8[true]  := 'true';
  // minimal stubs which will be properly implemented in mormot.core.log.pas
  GetExecutableLocation := _GetExecutableLocation;
  SetThreadName := _SetThreadName;
end;

procedure FinalizeUnit;
var
  i: PtrInt;
begin
  with InternalGarbageCollection do
  begin
    Shutdown := true; // avoid nested initialization at shutdown
    for i := Count - 1 downto 0 do
      FreeAndNilSafe(Instances[i]); // before GlobalCriticalSection deletion
  end;
  ObjArrayClear(CurrentFakeStubBuffers);
  Executable.Version.Free;
  Executable.Command.Free;
  FinalizeSpecificUnit; // in mormot.core.os.posix/windows.inc files
  ConsoleCriticalSection.Done;
  GlobalCriticalSection.Done;
  {$ifndef NOEXCEPTIONINTERCEPT}
  _RawLogException := nil;
  RawExceptionIntercepted := true;
  {$endif NOEXCEPTIONINTERCEPT}
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;

end.

