{
  $Project$
  $Workfile$
  $Revision$
  $DateUTC$
  $Id$

  This file is part of the Indy (Internet Direct) project, and is offered
  under the dual-licensing agreement described on the Indy website.
  (http://www.indyproject.org/)

  Copyright:
   (c) 1993-2005, Chad Z. Hower and the Indy Pit Crew. All rights reserved.
}
{
  $Log$
}
{
  Rev 1.0    2004.02.03 3:14:52 PM  czhower
  Move and updates

  Rev 1.2    10/15/2003 9:43:20 PM  DSiders
  Added localization comments.

  Rev 1.1    1-10-2003 19:44:28  BGooijen
  fixed leak in CloseLibrary()

  Rev 1.0    11/13/2002 09:03:24 AM  JPMugaas
}

unit Iocp.Wship6;

interface

{$I Iocp.CompilerDefines.inc}

{$IFDEF FPC}
  {$IFDEF WIN32}
    {$ALIGN OFF}
  {$ELSE}
    //It turns out that Win64 and WinCE require record alignment
    {$PACKRECORDS C}
  {$ENDIF}
{$ELSE}
  {$IFDEF WIN64}
    {$ALIGN ON}
    {$MINENUMSIZE 4}
  {$ELSE}
    {$MINENUMSIZE 4}
    {$IFDEF REQUIRES_PROPER_ALIGNMENT}
      {$ALIGN ON}
    {$ELSE}
      {$ALIGN OFF}
      {$WRITEABLECONST OFF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

uses
  {$IFDEF HAS_TInterlocked}
  syncobjs, //here to facilitate inlining with Delphi
  {$ENDIF}
  Windows,
  Iocp.Winsock2;

const
  Wship6_dll =   'Wship6.dll';    {do not localize}
  iphlpapi_dll = 'iphlpapi.dll';  {do not localize}
  fwpuclnt_dll = 'Fwpuclnt.dll'; {Do not localize}

  // Error codes from getaddrinfo().

  //JPM
  //Note that I am adding a GIA_ prefix on my own because
  //some names here share some names defined in Iocp.Winsock2 causing
  //an unpredictible problem. The values are not defined the same in Iocp.Winsock2
  {$EXTERNALSYM GIA_EAI_ADDRFAMILY}
  GIA_EAI_ADDRFAMILY = 1  ; // Address family for nodename not supported.
  {$EXTERNALSYM GIA_EAI_AGAIN}
  GIA_EAI_AGAIN      = 2  ; // Temporary failure in name resolution.
  {$EXTERNALSYM GIA_EAI_BADFLAGS}
  GIA_EAI_BADFLAGS   = 3  ; // Invalid value for ai_flags.
  {$EXTERNALSYM GIA_EAI_FAIL}
  GIA_EAI_FAIL       = 4  ; // Non-recoverable failure in name resolution.
  {$EXTERNALSYM GIA_EAI_FAMILY}
  GIA_EAI_FAMILY     = 5  ; // Address family ai_family not supported.
  {$EXTERNALSYM GIA_EAI_MEMORY}
  GIA_EAI_MEMORY     = 6  ; // Memory allocation failure.
  {$EXTERNALSYM GIA_EAI_NODATA}
  GIA_EAI_NODATA     = 7  ; // No address associated with nodename.
  {$EXTERNALSYM GIA_EAI_NONAME}
  GIA_EAI_NONAME     = 8  ; // Nodename nor servname provided, or not known.
  {$EXTERNALSYM GIA_EAI_SERVICE}
  GIA_EAI_SERVICE    = 9  ; // Servname not supported for ai_socktype.
  {$EXTERNALSYM GIA_EAI_SOCKTYPE}
  GIA_EAI_SOCKTYPE   = 10 ; // Socket type ai_socktype not supported.
  {$EXTERNALSYM GIA_EAI_SYSTEM}
  GIA_EAI_SYSTEM     = 11 ; // System error returned in errno.

  {$EXTERNALSYM NI_MAXHOST}
  NI_MAXHOST  = 1025;      // Max size of a fully-qualified domain name.
  {$EXTERNALSYM NI_MAXSERV}
  NI_MAXSERV  =   32;      // Max size of a service name.

  // Flags for getnameinfo().

  {$EXTERNALSYM NI_NOFQDN}
  NI_NOFQDN       =   $1  ;  // Only return nodename portion for local hosts.
  {$EXTERNALSYM NI_NUMERICHOST}
  NI_NUMERICHOST  =   $2  ;  // Return numeric form of the host's address.
  {$EXTERNALSYM NI_NAMEREQD}
  NI_NAMEREQD     =   $4  ;  // Error if the host's name not in DNS.
  {$EXTERNALSYM NI_NUMERICSERV}
  NI_NUMERICSERV  =   $8  ;  // Return numeric form of the service (port #).
  {$EXTERNALSYM NI_DGRAM}
  NI_DGRAM        =   $10 ;  // Service is a datagram service.

  //JPM - These may not be supported in WinCE 4.2
  {$EXTERNALSYM PROTECTION_LEVEL_RESTRICTED}
  PROTECTION_LEVEL_RESTRICTED   = 30;  //* for Intranet apps      /*
  {$EXTERNALSYM PROTECTION_LEVEL_DEFAULT}
  PROTECTION_LEVEL_DEFAULT      = 20;  //* default level          /*
  {$EXTERNALSYM PROTECTION_LEVEL_UNRESTRICTED}
  PROTECTION_LEVEL_UNRESTRICTED = 10;  //* for peer-to-peer apps  /*

  {$EXTERNALSYM SOCKET_SETTINGS_GUARANTEE_ENCRYPTION}
  SOCKET_SETTINGS_GUARANTEE_ENCRYPTION = $00000001;
  {$EXTERNALSYM SOCKET_SETTINGS_ALLOW_INSECURE}
  SOCKET_SETTINGS_ALLOW_INSECURE = $00000002;
  
  {$EXTERNALSYM SOCKET_INFO_CONNECTION_SECURED}
  SOCKET_INFO_CONNECTION_SECURED =  $00000001;
  {$EXTERNALSYM SOCKET_INFO_CONNECTION_ENCRYPTED}
  SOCKET_INFO_CONNECTION_ENCRYPTED = $00000002;

type
  // RLebeau: find a better place for this
  {$IFNDEF HAS_UInt64}
  {$EXTERNALSYM UINT64}
  UINT64 = Int64;
  {$ENDIF}

  {$NODEFINE PPaddrinfo}
  PPaddrinfo = ^PAddrInfo;
  {$NODEFINE PPaddrinfoW}
  PPaddrinfoW = ^PAddrInfoW;

  {$IFNDEF WINCE}
  {$EXTERNALSYM SOCKET_SECURITY_PROTOCOL}
  {$EXTERNALSYM SOCKET_SECURITY_PROTOCOL_DEFAULT}
  {$EXTERNALSYM SOCKET_SECURITY_PROTOCOL_IPSEC}
  {$EXTERNALSYM SOCKET_SECURITY_PROTOCOL_INVALID}
  SOCKET_SECURITY_PROTOCOL = (
    SOCKET_SECURITY_PROTOCOL_DEFAULT, SOCKET_SECURITY_PROTOCOL_IPSEC, SOCKET_SECURITY_PROTOCOL_INVALID
    );

  {$EXTERNALSYM SOCKET_SECURITY_SETTINGS_IPSEC}
  SOCKET_SECURITY_SETTINGS_IPSEC = record
    SecurityProtocol : SOCKET_SECURITY_PROTOCOL;
    SecurityFlags : ULONG;
    IpsecFlags : ULONG;
    AuthipMMPolicyKey : TGUID;
    AuthipQMPolicyKey : TGUID;
    Reserved : TGUID;
    Reserved2 : UINT64;
    UserNameStringLen : ULONG;
    DomainNameStringLen : ULONG;
    PasswordStringLen : ULONG;
  //  wchar_t AllStrings[0];
  end;
  {$EXTERNALSYM PSOCKET_SECURITY_SETTINGS_IPSEC}
  PSOCKET_SECURITY_SETTINGS_IPSEC = ^SOCKET_SECURITY_SETTINGS_IPSEC;

  {$EXTERNALSYM SOCKET_PEER_TARGET_NAME}
  SOCKET_PEER_TARGET_NAME = record
    SecurityProtocol : SOCKET_SECURITY_PROTOCOL;
    PeerAddress : SOCKADDR_STORAGE;
    PeerTargetNameStringLen : ULONG;
    //wchar_t AllStrings[0];
  end;
  {$EXTERNALSYM PSOCKET_PEER_TARGET_NAME}
  PSOCKET_PEER_TARGET_NAME = ^SOCKET_PEER_TARGET_NAME;

  {$EXTERNALSYM SOCKET_SECURITY_QUERY_INFO}
  SOCKET_SECURITY_QUERY_INFO = record
     SecurityProtocol : SOCKET_SECURITY_PROTOCOL;
     Flags : ULONG;
     PeerApplicationAccessTokenHandle : UINT64;
     PeerMachineAccessTokenHandle : UINT64;
  end;
  {$EXTERNALSYM PSOCKET_SECURITY_QUERY_INFO}
  PSOCKET_SECURITY_QUERY_INFO = ^SOCKET_SECURITY_QUERY_INFO;
  {$EXTERNALSYM SOCKET_SECURITY_QUERY_TEMPLATE}
  SOCKET_SECURITY_QUERY_TEMPLATE = record
    SecurityProtocol : SOCKET_SECURITY_PROTOCOL;
    PeerAddress : SOCKADDR_STORAGE;
    PeerTokenAccessMask : ULONG;
  end;
  {$EXTERNALSYM PSOCKET_SECURITY_QUERY_TEMPLATE}
  PSOCKET_SECURITY_QUERY_TEMPLATE = ^SOCKET_SECURITY_QUERY_TEMPLATE;

//callback defs
type
  {$EXTERNALSYM LPLOOKUPSERVICE_COMPLETION_ROUTINE}
  LPLOOKUPSERVICE_COMPLETION_ROUTINE = procedure (const dwError, dwBytes : DWORD; lpOverlapped : LPWSAOVERLAPPED); stdcall;
{$ENDIF}

type
  {$EXTERNALSYM LPFN_GETADDRINFO}
  LPFN_GETADDRINFO = function(NodeName: PAnsiChar; ServiceName: PAnsiChar; Hints: Paddrinfo; ppResult: PPaddrinfo): Integer; stdcall;
  {$EXTERNALSYM LPFN_GETADDRINFOW}
  LPFN_GETADDRINFOW = function(NodeName: PWideChar; ServiceName: PWideChar; Hints: PaddrinfoW; ppResult: PPaddrinfoW): Integer; stdcall;
  {$EXTERNALSYM LPFN_GETNAMEINFO}
  //The IPv6 preview for Win2K defines hostlen and servelen as size_t but do not use them
  //for these definitions as the newer SDK's define those as DWORD.
  LPFN_GETNAMEINFO = function(sa: psockaddr; salen: u_int; host: PAnsiChar; hostlen: u_int; serv: PAnsiChar; servlen: u_int; flags: Integer): Integer; stdcall;
  {$EXTERNALSYM LPFN_GETNAMEINFOW}
  LPFN_GETNAMEINFOW = function(sa: psockaddr; salen: u_int; host: PWideChar; hostlen: u_int; serv: PWideChar; servlen: u_int; flags: Integer): Integer; stdcall;
  {$EXTERNALSYM LPFN_FREEADDRINFO}
  LPFN_FREEADDRINFO = procedure(ai: Paddrinfo); stdcall;
  {$EXTERNALSYM LPFN_FREEADDRINFOW}
  LPFN_FREEADDRINFOW = procedure(ai: PaddrinfoW); stdcall;

//function GetAdaptersAddresses( Family:cardinal; Flags:cardinal; Reserved:pointer; pAdapterAddresses: PIP_ADAPTER_ADDRESSES; pOutBufLen:pcardinal):cardinal;stdcall;  external iphlpapi_dll;

{ the following are not used, nor tested}
{function getipnodebyaddr(const src:pointer;  len:integer; af:integer;var error_num:integer) :phostent;stdcall; external Wship6_dll;
procedure freehostent(ptr:phostent);stdcall; external Wship6_dll;
function inet_pton(af:integer; const src:pchar; dst:pointer):integer;stdcall; external Wship6_dll;
function inet_ntop(af:integer; const src:pointer; dst:pchar;size:integer):pchar;stdcall; external Wship6_dll;
}
 {$IFNDEF WINCE}
  {$EXTERNALSYM LPFN_INET_PTON}
  LPFN_INET_PTON = function (af: Integer; const src: PAnsiChar; dst: Pointer): Integer; stdcall;
  {$EXTERNALSYM LPFN_INET_PTONW}
  LPFN_INET_PTONW = function (af: Integer; const src: PWideChar; dst: Pointer): Integer; stdcall;
  {$EXTERNALSYM LPFN_INET_NTOP}
  LPFN_INET_NTOP = function (af: Integer; const src: Pointer; dst: PAnsiChar; size: size_t): PAnsiChar; stdcall;
  {$EXTERNALSYM LPFN_INET_NTOPW}
  LPFN_INET_NTOPW = function (af: Integer; const src: Pointer; dst: PWideChar; size: size_t): PAnsiChar; stdcall;

{ end the following are not used, nor tested}
//These are provided in case we need them later
//Windows Vista
  {$EXTERNALSYM LPFN_GETADDRINFOEXA}
  LPFN_GETADDRINFOEXA = function(pName : PAnsiChar; pServiceName : PAnsiChar;
    const dwNameSpace: DWord; lpNspId : LPGUID; hints : PADDRINFOEXA;
    ppResult : PADDRINFOEXA; timeout : Ptimeval; lpOverlapped : LPWSAOVERLAPPED;
    lpCompletionRoutine : LPLOOKUPSERVICE_COMPLETION_ROUTINE;
    var lpNameHandle : THandle) : Integer; stdcall;
  {$EXTERNALSYM LPFN_GETADDRINFOEXW}
  LPFN_GETADDRINFOEXW = function(pName : PWideChar; pServiceName : PWideChar;
    const dwNameSpace: DWord; lpNspId : LPGUID;hints : PADDRINFOEXW;
    ppResult : PADDRINFOEXW; timeout : Ptimeval; lpOverlapped : LPWSAOVERLAPPED;
    lpCompletionRoutine : LPLOOKUPSERVICE_COMPLETION_ROUTINE;
    var lpNameHandle : THandle) : Integer; stdcall;
  {$EXTERNALSYM LPFN_SETADDRINFOEXA}
  LPFN_SETADDRINFOEXA= function(pName : PAnsiChar; pServiceName : PAnsiChar;
    pAddresses : PSOCKET_ADDRESS; const dwAddressCount : DWord; lpBlob : LPBLOB;
    const dwFlags : DWord; const dwNameSpace : DWord; lpNspId : LPGUID;
    timeout : Ptimeval;
    lpOverlapped : LPWSAOVERLAPPED;
    lpCompletionRoutine : LPLOOKUPSERVICE_COMPLETION_ROUTINE; var lpNameHandle : THandle) : Integer; stdcall;
  {$EXTERNALSYM LPFN_SETADDRINFOEXW}
  LPFN_SETADDRINFOEXW= function(pName : PWideChar; pServiceName : PWideChar;
    pAddresses : PSOCKET_ADDRESS; const dwAddressCount : DWord; lpBlob : LPBLOB;
    const dwFlags : DWord; const dwNameSpace : DWord; lpNspId : LPGUID;
    timeout : Ptimeval;
    lpOverlapped : LPWSAOVERLAPPED;
    lpCompletionRoutine : LPLOOKUPSERVICE_COMPLETION_ROUTINE; var lpNameHandle : THandle) : Integer; stdcall;

  {$EXTERNALSYM LPFN_FREEADDRINFOEX}
  LPFN_FREEADDRINFOEX = procedure(pAddrInfoEx : PADDRINFOEXA) ; stdcall;
  {$EXTERNALSYM LPFN_FREEADDRINFOEXW}
  LPFN_FREEADDRINFOEXW = procedure(pAddrInfoEx : PADDRINFOEXW) ; stdcall;

  {$EXTERNALSYM LPFN_GETADDRINFOEX}
  {$EXTERNALSYM LPFN_SETADDRINFOEX}
  {$IFDEF UNICODE}
  LPFN_GETADDRINFOEX = LPFN_GETADDRINFOEXW;
  LPFN_SETADDRINFOEX = LPFN_SETADDRINFOEXW;
  {$ELSE}
  LPFN_GETADDRINFOEX = LPFN_GETADDRINFOEXA;
  LPFN_SETADDRINFOEX = LPFN_SETADDRINFOEXA;
  {$ENDIF}

  //  Fwpuclnt.dll - API
  {$EXTERNALSYM LPFN_WSADELETESOCKETPEERTARGETNAME}
  LPFN_WSADELETESOCKETPEERTARGETNAME = function (Socket : TSocket;
    PeerAddr : Psockaddr; PeerAddrLen : ULONG;
    Overlapped : LPWSAOVERLAPPED;  CompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE): Integer; stdcall;
  {$EXTERNALSYM LPFN_WSASETSOCKETPEERTARGETNAME}
  LPFN_WSASETSOCKETPEERTARGETNAME = function (Socket : TSocket;
    PeerTargetName : PSOCKET_PEER_TARGET_NAME; PeerTargetNameLen : ULONG;
    Overlapped : LPWSAOVERLAPPED; CompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE) : Integer; stdcall;
  {$EXTERNALSYM LPFN_WSAIMPERSONATESOCKETPEER}
  LPFN_WSAIMPERSONATESOCKETPEER = function (Socket : TSocket;
    PeerAddress : Psockaddr;  peerAddressLen :  ULONG) : Integer; stdcall;
  {$EXTERNALSYM LPFN_WSAQUERYSOCKETSECURITY}
  LPFN_WSAQUERYSOCKETSECURITY = function (Socket : TSocket;
    SecurityQueryTemplate : PSOCKET_SECURITY_QUERY_TEMPLATE; const SecurityQueryTemplateLen : ULONG;
    var SecurityQueryInfo : PSOCKET_SECURITY_QUERY_INFO; var SecurityQueryInfoLen : ULONG;
     Overlapped : LPWSAOVERLAPPED;  CompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE) : Integer; stdcall;
  {$EXTERNALSYM LPFN_WSAREVERTIMPERSONATION}
  LPFN_WSAREVERTIMPERSONATION = function : Integer; stdcall;
{$ENDIF}

const
  {$NODEFINE fn_GetAddrInfoEx}
  {$NODEFINE fn_SetAddrInfoEx}
  {$NODEFINE fn_FreeAddrInfoEx}
  {$NODEFINE fn_GetAddrInfo}
  {$NODEFINE fn_getnameinfo}
  {$NODEFINE fn_freeaddrinfo}
  {$NODEFINE fn_inet_pton}
  {$NODEFINE fn_inet_ntop}
  {$IFDEF UNICODE}
     {$IFNDEF WINCE}
  fn_GetAddrInfoEx = 'GetAddrInfoExW';
  fn_SetAddrInfoEx = 'SetAddrInfoExW';
  fn_FreeAddrInfoEx = 'FreeAddrInfoExW';
    {$ENDIF}
  fn_GetAddrInfo = 'GetAddrInfoW';
  fn_getnameinfo = 'GetNameInfoW';
  fn_freeaddrinfo = 'FreeAddrInfoW';
     {$IFNDEF WINCE}
  fn_inet_pton = 'InetPtonW';
  fn_inet_ntop = 'InetNtopW';
    {$ENDIF}
  {$ELSE}
     {$IFNDEF WINCE}
  fn_GetAddrInfoEx = 'GetAddrInfoExA';
  fn_SetAddrInfoEx = 'SetAddrInfoExA';
  fn_FreeAddrInfoEx = 'FreeAddrInfoEx';
    {$ENDIF}
  fn_GetAddrInfo = 'getaddrinfo';
  fn_getnameinfo = 'getnameinfo';
  fn_freeaddrinfo = 'freeaddrinfo';
   {$IFNDEF WINCE}
  fn_inet_pton = 'inet_pton';
  fn_inet_ntop = 'inet_ntop';
    {$ENDIF}
  {$ENDIF}

var
  {$EXTERNALSYM getaddrinfo}
  {$EXTERNALSYM getnameinfo}
  {$EXTERNALSYM freeaddrinfo}
  {$EXTERNALSYM inet_pton}
  {$EXTERNALSYM inet_ntop}
  {$IFDEF UNICODE}
  getaddrinfo: LPFN_GETADDRINFOW = nil;
  getnameinfo: LPFN_GETNAMEINFOW = nil;
  freeaddrinfo: LPFN_FREEADDRINFOW = nil;
    {$IFNDEF WINCE}
  //These are here for completeness
  inet_pton : LPFN_inet_ptonW = nil;
  inet_ntop : LPFN_inet_ntopW = nil;
    {$ENDIF}
  {$ELSE}
  getaddrinfo: LPFN_GETADDRINFO = nil;
  getnameinfo: LPFN_GETNAMEINFO = nil;
  freeaddrinfo: LPFN_FREEADDRINFO = nil;
      {$IFNDEF WINCE}
  //These are here for completeness
  inet_pton : LPFN_inet_pton = nil;
  inet_ntop : LPFN_inet_ntop = nil;
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF WINCE}
  {
  IMPORTANT!!!

  These are Windows Vista functions and there's no guarantee that you will have
  them so ALWAYS check the function pointer before calling them.
  }
  {$EXTERNALSYM GetAddrInfoEx}
  GetAddrInfoEx :  LPFN_GETADDRINFOEX = nil;
  {$EXTERNALSYM SetAddrInfoEx}
  SetAddrInfoEx : LPFN_SETADDRINFOEX = nil;
  {$EXTERNALSYM FreeAddrInfoEx}
  //You can't alias the LPFN for this because the ASCII version of this
  //does not end with an "a"
  {$IFDEF UNICODE}
  FreeAddrInfoEx : LPFN_FREEADDRINFOEX = nil;
  {$ELSE}
  FreeAddrInfoEx : LPFN_FREEADDRINFOEXW = nil;
  {$ENDIF}

  //Fwpuclnt.dll available for Windows Vista and later
  {$EXTERNALSYM WSASETSOCKETPEERTARGETNAME}
  WSASetSocketPeerTargetName : LPFN_WSASETSOCKETPEERTARGETNAME = nil;
  {$EXTERNALSYM WSADELETESOCKETPEERTARGETNAME}
  WSADeleteSocketPeerTargetName : LPFN_WSADELETESOCKETPEERTARGETNAME = nil;
  {$EXTERNALSYM WSAImpersonateSocketPeer}
  WSAImpersonateSocketPeer : LPFN_WSAIMPERSONATESOCKETPEER = nil;
  {$EXTERNALSYM WSAQUERYSOCKETSECURITY}
  WSAQUERYSOCKETSECURITY : LPFN_WSAQUERYSOCKETSECURITY = nil;
  {$EXTERNALSYM WSAREVERTIMPERSONATION}
  WSARevertImpersonation : LPFN_WSAREVERTIMPERSONATION = nil;
  {$ENDIF}
  
var
  GIdIPv6FuncsAvailable: Boolean = False;

function gaiErrorToWsaError(const gaiError: Integer): Integer;

//We want to load this library only after loading Winsock and unload immediately
//before unloading Winsock.
procedure InitLibrary;
procedure CloseLibrary;

implementation

uses
  SysUtils;

var
  hWship6Dll : THandle = 0; // Wship6.dll handle
  //Use this instead of hWship6Dll because this will point to the correct lib.
  hProcHandle : THandle = 0;
  {$IFNDEF WINCE}
  hfwpuclntDll : THandle = 0;
  {$ENDIF}

function gaiErrorToWsaError(const gaiError: Integer): Integer;
begin
  case gaiError of
    GIA_EAI_ADDRFAMILY: Result := 0;                                      
    GIA_EAI_AGAIN:      Result := WSATRY_AGAIN;
    GIA_EAI_BADFLAGS:   Result := WSAEINVAL;
    GIA_EAI_FAIL:       Result := WSANO_RECOVERY;
    GIA_EAI_FAMILY:     Result := WSAEAFNOSUPPORT;
    GIA_EAI_MEMORY:     Result := WSA_NOT_ENOUGH_MEMORY;
    GIA_EAI_NODATA:     Result := WSANO_DATA;
    GIA_EAI_NONAME:     Result := WSAHOST_NOT_FOUND;
    GIA_EAI_SERVICE:    Result := WSATYPE_NOT_FOUND;
    GIA_EAI_SOCKTYPE:   Result := WSAESOCKTNOSUPPORT;
    GIA_EAI_SYSTEM:
      begin
        Result := 0; // avoid warning
        RaiseLastOSError;
      end;
    else
      Result := gaiError;
  end;
end;

function InterlockedExchangeTHandle(var VTarget: THandle; const AValue: THandle): THandle;
{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF HAS_TInterlocked}
    {$IFDEF THANDLE_32}
  Result := THandle(TInterlocked.Exchange(LongInt(VTarget), LongInt(AValue)));
    {$ENDIF}
  //Temporary workaround.  TInterlocked for Emb really should accept 64 bit unsigned values as set of parameters
  //for TInterlocked.Exchange since 64-bit wide integers are common on 64 bit platforms.
    {$IFDEF THANDLE_64}
  Result := THandle(TInterlocked.Exchange(Int64(VTarget), Int64(AValue)));
    {$ENDIF}
  {$ELSE}
    {$IFDEF THANDLE_32}
  Result := THandle(InterlockedExchange(LongInt(VTarget), LongInt(AValue)));
    {$ENDIF}
    {$IFDEF THANDLE_64}
  Result := THandle(InterlockedExchange64(Int64(VTarget), Int64(AValue)));
    {$ENDIF}
  {$ENDIF}
end;

procedure CloseLibrary;
var
  h : THandle;
begin
  {$IFNDEF WINCE}
    {$IFNDEF WIN64}
  //Only unload the IPv6 functions for Windows NT (2000 or greater).
  //Note that Win64 was introduced after Windows XP.  That was based on Windows
  //Server code so we'll skip this in Win64.
  //I'm just doing this as a minor shortcut.
  if (Win32Platform <> VER_PLATFORM_WIN32_NT) or (Win32MajorVersion < 5) then begin
    Exit;
  end;
    {$ENDIF}
  {$ENDIF}
  h := InterlockedExchangeTHandle(hWship6Dll, 0);
  if h <> 0 then begin
    FreeLibrary(h);
  end;
  {$IFNDEF WINCE}
  h := InterlockedExchangeTHandle(hfwpuclntDll, 0);
  if h <> 0 then begin
    FreeLibrary(h);
  end;
  {$ENDIF}
  GIdIPv6FuncsAvailable := False;

  getaddrinfo := nil;
  getnameinfo := nil;
  freeaddrinfo := nil;
  {$IFNDEF WINCE}
  WSASetSocketPeerTargetName := nil;
  WSADeleteSocketPeerTargetName := nil;
  WSAImpersonateSocketPeer := nil;
  WSAQuerySocketSecurity := nil;
  WSARevertImpersonation := nil;
  {$ENDIF}
end;

procedure InitLibrary;
begin
  GIdIPv6FuncsAvailable := False;
  {$IFNDEF WINCE}
    {$IFNDEF WIN64}
  //Only attempt to load the IPv6 functions for Windows NT (2000 or greater).
  //Note that Win64 was introduced after Windows XP.  That was based on Windows
  //Server code so we'll skip this in Win64.
  if (Win32Platform <> VER_PLATFORM_WIN32_NT) or (Win32MajorVersion < 5) then begin
    Exit;
  end;
    {$ENDIF}
  {$ENDIF}
{
IMPORTANT!!!

I am doing things this way because the functions we want are probably in
the Winsock2 dll.  If they are not there, only then do you actually want
to try the Wship6.dll.   I know it's a mess but I found that the functions
may not load if they aren't in Wship6.dll (and they aren't there in some
versions of Windows).

hProcHandle provides a transparant way of managing the two possible library
locations.  hWship6Dll is kept so we can unload the Wship6.dll if necessary.
}
  //Winsock2 has to be loaded by IdWinsock first.
  if not Iocp.Winsock2.Winsock2Loaded then
  begin
    Iocp.Winsock2.InitializeWinSock;
  end;
  hProcHandle := Iocp.Winsock2.WinsockHandle;
  getaddrinfo := GetProcAddress(hProcHandle, fn_getaddrinfo);
  if not Assigned(getaddrinfo) then
  begin
    hWship6Dll := SafeLoadLibrary(Wship6_dll);
    hProcHandle := hWship6Dll;
    getaddrinfo := GetProcAddress(hProcHandle, fn_getaddrinfo);  {do not localize}
  end;

  if Assigned(getaddrinfo) then
  begin
    getnameinfo := GetProcAddress(hProcHandle, fn_getnameinfo);  {do not localize}
    if Assigned(getnameinfo) then
    begin
      freeaddrinfo := GetProcAddress(hProcHandle, fn_freeaddrinfo);  {do not localize}
      if Assigned(freeaddrinfo) then
      begin
        GIdIPv6FuncsAvailable := True;

        //Additional functions should be initialized here.
        {$IFNDEF WINCE}
        inet_pton := GetProcAddress(hProcHandle, fn_inet_pton);  {do not localize}
        inet_ntop := GetProcAddress(hProcHandle, fn_inet_ntop);  {do not localize}
        GetAddrInfoEx := GetProcAddress(hProcHandle, fn_GetAddrInfoEx); {Do not localize}
        SetAddrInfoEx := GetProcAddress(hProcHandle, fn_SetAddrInfoEx); {Do not localize}
        FreeAddrInfoEx := GetProcAddress(hProcHandle, fn_FreeAddrInfoEx); {Do not localize}
        hfwpuclntDll := SafeLoadLibrary(fwpuclnt_dll);
        if hfwpuclntDll <> 0 then
        begin
          WSASetSocketPeerTargetName := GetProcAddress(hfwpuclntDll, 'WSASetSocketPeerTargetName'); {Do not localize}
          WSADeleteSocketPeerTargetName := GetProcAddress(hfwpuclntDll, 'WSADeleteSocketPeerTargetName');  {Do not localize}
          WSAImpersonateSocketPeer := GetProcAddress(hfwpuclntDll, 'WSAImpersonateSocketPeer'); {Do not localize}
          WSAQuerySocketSecurity := GetProcAddress(hfwpuclntDll, 'WSAQuerySocketSecurity'); {Do not localize}
          WSARevertImpersonation := GetProcAddress(hfwpuclntDll, 'WSARevertImpersonation'); {Do not localize}
        end;
        {$ENDIF}
        Exit;
      end;
    end;
  end;

  CloseLibrary;
end;

initialization
  InitLibrary;

finalization
  CloseLibrary;

end.
