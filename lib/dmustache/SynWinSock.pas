/// low level access to network Sockets for the Win32 platform
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynWinSock;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2023 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synapse library.

  The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).
  Portions created by Lukas Gebauer are Copyright (C) 2003.
  All Rights Reserved.

  Portions created by Arnaud Bouchez are Copyright (C) 2023 Arnaud Bouchez.
  All Rights Reserved.

  Contributor(s):
  - Arnaud Bouchez, Jan 2009, for SynCrtSock: see https://synopse.info
    Delphi 2009/2010 compatibility (Jan 2010): the WinSock library
      expects Ansi encoded parameters
  - Svetozar Belic (transmogrifix)

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

}

{.$DEFINE WINSOCK1}
{If you activate this compiler directive, then socket interface level 1.1 is
used instead default level 2.2. Level 2.2 is not available on old W95, however
you can install an update from microsoft}

{.$DEFINE FORCEOLDAPI}
{If you activate this compiler directive, then is allways used old socket API
for name resolution. If you leave this directive inactive, then the new API
is used, when running system allows it. For IPv6 support you must have the new API! }

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface
{$ifdef MSWINDOWS}
uses
  SysUtils,
  Classes,
  Windows;

function InitSocketInterface(const stack: TFileName=''): Boolean;
function DestroySocketInterface: Boolean;

const
{$ifdef WINSOCK1}
  WinsockLevel = $0101;
{$ELSE}
  WinsockLevel = $0202;
{$endif}

type
  u_char = AnsiChar;
  u_short = Word;
  u_int = integer;
  u_long = Longint;
  pu_long = ^u_long;
  pu_short = ^u_short;
  {$ifdef FPC}
  TSocket = PtrInt;
  {$else}
    {$ifdef UNICODE}
    TSocket = NativeInt;
    {$else}
    TSocket = integer;
    {$endif UNICODE}
  {$endif}


const
  {$ifdef WINSOCK1}
  DLLStackName: PChar = 'wsock32.dll';
  {$ELSE}
  DLLStackName: PChar = 'ws2_32.dll';
  {$endif}
  DLLwship6: PChar = 'wship6.dll';
  DLLSecur32: PChar = 'secur32.dll';

  cLocalhost = '127.0.0.1';
  cAnyHost = '0.0.0.0';
  cBroadcast = '255.255.255.255';
  c6Localhost = '::1';
  c6AnyHost = '::0';
  c6Broadcast = 'ffff::1';
  cAnyPort = '0';


const
  FD_SETSIZE     =   64;

type
  PFDSet = ^TFDSet;
  TFDSet = record
    fd_count: u_int;
    fd_array: array[0..FD_SETSIZE-1] of TSocket;
  end;

const
  FIONREAD     = $4004667f;
  FIONBIO      = $8004667e;
  FIOASYNC     = $8004667d;

type
  PTimeVal = ^TTimeVal;
  TTimeVal = record
    tv_sec: Longint;
    tv_usec: Longint;
  end;

const
  IPPROTO_IP     =   0;		{ Dummy					}
  IPPROTO_ICMP   =   1;		{ Internet Control Message Protocol }
  IPPROTO_IGMP   =   2;		{ Internet Group Management Protocol}
  IPPROTO_TCP    =   6;		{ TCP           			}
  IPPROTO_UDP    =   17;	{ User Datagram Protocol		}
  IPPROTO_IPV6   =   41;
  IPPROTO_ICMPV6 =   58;

  IPPROTO_RAW    =   255;
  IPPROTO_MAX    =   256;

type

  PInAddr = ^TInAddr;
  TInAddr = packed record
    case integer of
      0: (S_bytes: packed array [0..3] of byte);
      1: (S_addr: u_long);
  end;

  PSockAddrIn = ^TSockAddrIn;
  TSockAddrIn = packed record
    case integer of
      0: (sin_family: u_short;
          sin_port: u_short;
          sin_addr: TInAddr;
          sin_zero: array[0..7] of AnsiChar);
      1: (sa_family: u_short;
          sa_data: array[0..13] of AnsiChar)
  end;

  TIP_mreq = record
    imr_multiaddr: TInAddr;     { IP multicast address of group }
    imr_interface: TInAddr;     { local IP address of interface }
  end;

  PInAddr6 = ^TInAddr6;
  TInAddr6 = packed record
    case integer of
      0: (S6_addr: packed array [0..15] of byte);
      1: (u6_addr8: packed array [0..15] of byte);
      2: (u6_addr16: packed array [0..7] of word);
      3: (u6_addr32: packed array [0..3] of integer);
  end;

  PSockAddrIn6 = ^TSockAddrIn6;
  TSockAddrIn6 = packed record
		sin6_family:   u_short;     // AF_INET6
		sin6_port:     u_short;     // Transport level port number
		sin6_flowinfo: u_long;	    // IPv6 flow information
		sin6_addr:     TInAddr6;    // IPv6 address
		sin6_scope_id: u_long;      // Scope Id: IF number for link-local
                                //           SITE id for site-local
  end;

  TIPv6_mreq = record
    ipv6mr_multiaddr: TInAddr6; // IPv6 multicast address.
    ipv6mr_interface: integer;   // Interface index.
    padding: integer;
  end;

  PHostEnt = ^THostEnt;
  THostEnt = packed record
    h_name: PAnsiChar;
    h_aliases: ^PAnsiChar;
    h_addrtype: Smallint;
    h_length: Smallint;
    case integer of
     0: (h_addr_list: ^PAnsiChar);
     1: (h_addr: ^PInAddr);
  end;

  PNetEnt = ^TNetEnt;
  TNetEnt = packed record
    n_name: PAnsiChar;
    n_aliases: ^PAnsiChar;
    n_addrtype: Smallint;
    n_net: u_long;
  end;

  PServEnt = ^TServEnt;
  TServEnt = packed record
    s_name: PAnsiChar;
    s_aliases: ^PAnsiChar;
    s_port: Smallint;
    s_proto: PAnsiChar;
  end;

  PProtoEnt = ^TProtoEnt;
  TProtoEnt = packed record
    p_name: PAnsiChar;
    p_aliases: ^PAnsiChar;
    p_proto: Smallint;
  end;

const
  INADDR_ANY       = $00000000;
  INADDR_LOOPBACK  = $7F000001;
  INADDR_BROADCAST = $FFFFFFFF;
  INADDR_NONE      = $FFFFFFFF;
  ADDR_ANY		 = INADDR_ANY;
  INVALID_SOCKET		= TSocket(NOT(0));
  SOCKET_ERROR			= -1;

Const
  {$ifdef WINSOCK1}
    IP_OPTIONS          = 1;
    IP_MULTICAST_IF     = 2;           { set/get IP multicast interface   }
    IP_MULTICAST_TTL    = 3;           { set/get IP multicast timetolive  }
    IP_MULTICAST_LOOP   = 4;           { set/get IP multicast loopback    }
    IP_ADD_MEMBERSHIP   = 5;           { add  an IP group membership      }
    IP_DROP_MEMBERSHIP  = 6;           { drop an IP group membership      }
    IP_TTL              = 7;           { set/get IP Time To Live          }
    IP_TOS              = 8;           { set/get IP Type Of Service       }
    IP_DONTFRAGMENT     = 9;           { set/get IP Don't Fragment flag   }
  {$ELSE}
    IP_OPTIONS          = 1;
    IP_HDRINCL          = 2;
    IP_TOS              = 3;           { set/get IP Type Of Service       }
    IP_TTL              = 4;           { set/get IP Time To Live          }
    IP_MULTICAST_IF     = 9;           { set/get IP multicast interface   }
    IP_MULTICAST_TTL    = 10;           { set/get IP multicast timetolive  }
    IP_MULTICAST_LOOP   = 11;           { set/get IP multicast loopback    }
    IP_ADD_MEMBERSHIP   = 12;           { add  an IP group membership      }
    IP_DROP_MEMBERSHIP  = 13;           { drop an IP group membership      }
    IP_DONTFRAGMENT     = 14;           { set/get IP Don't Fragment flag   }
  {$endif}

  IP_DEFAULT_MULTICAST_TTL   = 1;    { normally limit m'casts to 1 hop  }
  IP_DEFAULT_MULTICAST_LOOP  = 1;    { normally hear sends if a member  }
  IP_MAX_MEMBERSHIPS         = 20;   { per socket; must fit in one mbuf }

  SOL_SOCKET      = $ffff;          {options for socket level }
{ Option flags per-socket. }
  SO_DEBUG        = $0001;          { turn on debugging info recording }
  SO_ACCEPTCONN   = $0002;          { socket has had listen() }
  SO_REUSEADDR    = $0004;          { allow local address reuse }
  SO_KEEPALIVE    = $0008;          { keep connections alive }
  SO_DONTROUTE    = $0010;          { just use interface addresses }
  SO_BROADCAST    = $0020;          { permit sending of broadcast msgs }
  SO_USELOOPBACK  = $0040;          { bypass hardware when possible }
  SO_LINGER       = $0080;          { linger on close if data present }
  SO_OOBINLINE    = $0100;          { leave received OOB data in line }
  SO_DONTLINGER  =   $ff7f;
{ Additional options. }
  SO_SNDBUF       = $1001;          { send buffer size }
  SO_RCVBUF       = $1002;          { receive buffer size }
  SO_SNDLOWAT     = $1003;          { send low-water mark }
  SO_RCVLOWAT     = $1004;          { receive low-water mark }
  SO_SNDTIMEO     = $1005;          { send timeout }
  SO_RCVTIMEO     = $1006;          { receive timeout }
  SO_ERROR        = $1007;          { get error status and clear }
  SO_TYPE         = $1008;          { get socket type }
{ WinSock 2 extension -- new options }
  SO_GROUP_ID       = $2001; { ID of a socket group}
  SO_GROUP_PRIORITY = $2002; { the relative priority within a group}
  SO_MAX_MSG_SIZE   = $2003; { maximum message size }
  SO_PROTOCOL_INFOA = $2004; { WSAPROTOCOL_INFOA structure }
  SO_PROTOCOL_INFOW = $2005; { WSAPROTOCOL_INFOW structure }
  SO_PROTOCOL_INFO  = SO_PROTOCOL_INFOA;
  PVD_CONFIG        = $3001; {configuration info for service provider }
{ Option for opening sockets for synchronous access. }
  SO_OPENTYPE     = $7008;
  SO_SYNCHRONOUS_ALERT    = $10;
  SO_SYNCHRONOUS_NONALERT = $20;
{ Other NT-specific options. }
  SO_MAXDG        = $7009;
  SO_MAXPATHDG    = $700A;
  SO_UPDATE_ACCEPT_CONTEXT     = $700B;
  SO_CONNECT_TIME = $700C;

  SOMAXCONN       = $7fffffff;

  IPV6_UNICAST_HOPS      = 8;  // ???
  IPV6_MULTICAST_IF      = 9;  // set/get IP multicast i/f
  IPV6_MULTICAST_HOPS    = 10; // set/get IP multicast ttl
  IPV6_MULTICAST_LOOP    = 11; // set/get IP multicast loopback
  IPV6_JOIN_GROUP        = 12; // add an IP group membership
  IPV6_LEAVE_GROUP       = 13; // drop an IP group membership

  MSG_NOSIGNAL  = 0;

  // getnameinfo constants
  NI_MAXHOST	   = 1025;
  NI_MAXSERV	   = 32;
  NI_NOFQDN 	   = $1;
  NI_NUMERICHOST = $2;
  NI_NAMEREQD	   = $4;
  NI_NUMERICSERV = $8;
  NI_DGRAM       = $10;


const
  SOCK_STREAM     = 1;               { stream socket }
  SOCK_DGRAM      = 2;               { datagram socket }
  SOCK_RAW        = 3;               { raw-protocol interface }
  SOCK_RDM        = 4;               { reliably-delivered message }
  SOCK_SEQPACKET  = 5;               { sequenced packet stream }

{ TCP options. }
  TCP_NODELAY     = $0001;

{ Address families. }

  AF_UNSPEC       = 0;               { unspecified }
  AF_INET         = 2;               { internetwork: UDP, TCP, etc. }
  AF_INET6        = 23;              { Internetwork Version 6 }
  AF_MAX          = 24;

{ Protocol families, same as address families for now. }
  PF_UNSPEC       = AF_UNSPEC;
  PF_INET         = AF_INET;
  PF_INET6        = AF_INET6;
  PF_MAX          = AF_MAX;

type
  { Structure used by kernel to store most addresses. }
  PSockAddr = ^TSockAddr;
  TSockAddr = TSockAddrIn;

  { Structure used by kernel to pass protocol information in raw sockets. }
  PSockProto = ^TSockProto;
  TSockProto = packed record
    sp_family: u_short;
    sp_protocol: u_short;
  end;

type
  PAddrInfo = ^TAddrInfo;
  TAddrInfo = record
                ai_flags: integer;    // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST.
                ai_family: integer;   // PF_xxx.
                ai_socktype: integer; // SOCK_xxx.
                ai_protocol: integer; // 0 or IPPROTO_xxx for IPv4 and IPv6.
                ai_addrlen: u_int;    // Length of ai_addr.
                ai_canonname: PAnsiChar;  // Canonical name for nodename.
                ai_addr: PSockAddr;   // Binary address.
                ai_next: PAddrInfo;     // Next structure in linked list.
              end;

const
  // Flags used in "hints" argument to getaddrinfo().
  AI_PASSIVE     = $1;  // Socket address will be used in bind() call.
  AI_CANONNAME   = $2;  // Return canonical name in first ai_canonname.
  AI_NUMERICHOST = $4;  // Nodename must be a numeric address AnsiString.

type
{ Structure used for manipulating linger option. }
  PLinger = ^TLinger;
  TLinger = packed record
    l_onoff: u_short;
    l_linger: u_short;
  end;

const

  MSG_OOB       = $01;                  // Process out-of-band data.
  MSG_PEEK      = $02;                  // Peek at incoming messages.

const

{ All Windows Sockets error constants are biased by WSABASEERR from the "normal" }
  WSABASEERR              = 10000;

{ Windows Sockets definitions of regular Microsoft C error constants }

  WSAEINTR                = (WSABASEERR+4); // legacy error
  WSAEBADF                = (WSABASEERR+9);
  WSAEACCES               = (WSABASEERR+13);
  WSAEFAULT               = (WSABASEERR+14);
  WSAEINVAL               = (WSABASEERR+22);
  WSAEMFILE               = (WSABASEERR+24);

{ Windows Sockets definitions of regular Berkeley error constants }

  WSAEWOULDBLOCK          = (WSABASEERR+35);
  WSAEINPROGRESS          = (WSABASEERR+36);
  WSAEALREADY             = (WSABASEERR+37);
  WSAENOTSOCK             = (WSABASEERR+38);
  WSAEDESTADDRREQ         = (WSABASEERR+39);
  WSAEMSGSIZE             = (WSABASEERR+40);
  WSAEPROTOTYPE           = (WSABASEERR+41);
  WSAENOPROTOOPT          = (WSABASEERR+42);
  WSAEPROTONOSUPPORT      = (WSABASEERR+43);
  WSAESOCKTNOSUPPORT      = (WSABASEERR+44);
  WSAEOPNOTSUPP           = (WSABASEERR+45);
  WSAEPFNOSUPPORT         = (WSABASEERR+46);
  WSAEAFNOSUPPORT         = (WSABASEERR+47);
  WSAEADDRINUSE           = (WSABASEERR+48);
  WSAEADDRNOTAVAIL        = (WSABASEERR+49);
  WSAENETDOWN             = (WSABASEERR+50);
  WSAENETUNREACH          = (WSABASEERR+51);
  WSAENETRESET            = (WSABASEERR+52);
  WSAECONNABORTED         = (WSABASEERR+53);
  WSAECONNRESET           = (WSABASEERR+54);
  WSAENOBUFS              = (WSABASEERR+55);
  WSAEISCONN              = (WSABASEERR+56);
  WSAENOTCONN             = (WSABASEERR+57);
  WSAESHUTDOWN            = (WSABASEERR+58);
  WSAETOOMANYREFS         = (WSABASEERR+59);
  WSAETIMEDOUT            = (WSABASEERR+60);
  WSAECONNREFUSED         = (WSABASEERR+61);
  WSAELOOP                = (WSABASEERR+62);
  WSAENAMETOOLONG         = (WSABASEERR+63);
  WSAEHOSTDOWN            = (WSABASEERR+64);
  WSAEHOSTUNREACH         = (WSABASEERR+65);
  WSAENOTEMPTY            = (WSABASEERR+66);
  WSAEPROCLIM             = (WSABASEERR+67);
  WSAEUSERS               = (WSABASEERR+68);
  WSAEDQUOT               = (WSABASEERR+69);
  WSAESTALE               = (WSABASEERR+70);
  WSAEREMOTE              = (WSABASEERR+71);

{ Extended Windows Sockets error constant definitions }

  WSASYSNOTREADY          = (WSABASEERR+91);
  WSAVERNOTSUPPORTED      = (WSABASEERR+92);
  WSANOTINITIALISED       = (WSABASEERR+93);
  WSAEDISCON              = (WSABASEERR+101);
  WSAENOMORE              = (WSABASEERR+102);
  WSAECANCELLED           = (WSABASEERR+103);
  WSAEEINVALIDPROCTABLE   = (WSABASEERR+104);
  WSAEINVALIDPROVIDER     = (WSABASEERR+105);
  WSAEPROVIDERFAILEDINIT  = (WSABASEERR+106);
  WSASYSCALLFAILURE       = (WSABASEERR+107);
  WSASERVICE_NOT_FOUND    = (WSABASEERR+108);
  WSATYPE_NOT_FOUND       = (WSABASEERR+109);
  WSA_E_NO_MORE           = (WSABASEERR+110);
  WSA_E_CANCELLED         = (WSABASEERR+111);
  WSAEREFUSED             = (WSABASEERR+112);

{ Error return codes from gethostbyname() and gethostbyaddr()
  (when using the resolver). Note that these errors are
  retrieved via WSAGetLastError() and must therefore follow
  the rules for avoiding clashes with error numbers from
  specific implementations or language run-time systems.
  For this reason the codes are based at WSABASEERR+1001.
  Note also that [WSA]NO_ADDRESS is defined only for
  compatibility purposes. }

{ Authoritative Answer: Host not found }
  WSAHOST_NOT_FOUND       = (WSABASEERR+1001);
  HOST_NOT_FOUND          = WSAHOST_NOT_FOUND;
{ Non-Authoritative: Host not found, or SERVERFAIL }
  WSATRY_AGAIN            = (WSABASEERR+1002);
  TRY_AGAIN               = WSATRY_AGAIN;
{ Non recoverable errors, FORMERR, REFUSED, NOTIMP }
  WSANO_RECOVERY          = (WSABASEERR+1003);
  NO_RECOVERY             = WSANO_RECOVERY;
{ Valid name, no data record of requested type }
  WSANO_DATA              = (WSABASEERR+1004);
  NO_DATA                 = WSANO_DATA;
{ no address, look for MX record }
  WSANO_ADDRESS           = WSANO_DATA;
  NO_ADDRESS              = WSANO_ADDRESS;

  EWOULDBLOCK        =  WSAEWOULDBLOCK;
  EINPROGRESS        =  WSAEINPROGRESS;
  EALREADY           =  WSAEALREADY;
  ENOTSOCK           =  WSAENOTSOCK;
  EDESTADDRREQ       =  WSAEDESTADDRREQ;
  EMSGSIZE           =  WSAEMSGSIZE;
  EPROTOTYPE         =  WSAEPROTOTYPE;
  ENOPROTOOPT        =  WSAENOPROTOOPT;
  EPROTONOSUPPORT    =  WSAEPROTONOSUPPORT;
  ESOCKTNOSUPPORT    =  WSAESOCKTNOSUPPORT;
  EOPNOTSUPP         =  WSAEOPNOTSUPP;
  EPFNOSUPPORT       =  WSAEPFNOSUPPORT;
  EAFNOSUPPORT       =  WSAEAFNOSUPPORT;
  EADDRINUSE         =  WSAEADDRINUSE;
  EADDRNOTAVAIL      =  WSAEADDRNOTAVAIL;
  ENETDOWN           =  WSAENETDOWN;
  ENETUNREACH        =  WSAENETUNREACH;
  ENETRESET          =  WSAENETRESET;
  ECONNABORTED       =  WSAECONNABORTED;
  ECONNRESET         =  WSAECONNRESET;
  ENOBUFS            =  WSAENOBUFS;
  EISCONN            =  WSAEISCONN;
  ENOTCONN           =  WSAENOTCONN;
  ESHUTDOWN          =  WSAESHUTDOWN;
  ETOOMANYREFS       =  WSAETOOMANYREFS;
  ETIMEDOUT          =  WSAETIMEDOUT;
  ECONNREFUSED       =  WSAECONNREFUSED;
  ELOOP              =  WSAELOOP;
  ENAMETOOLONG       =  WSAENAMETOOLONG;
  EHOSTDOWN          =  WSAEHOSTDOWN;
  EHOSTUNREACH       =  WSAEHOSTUNREACH;
  ENOTEMPTY          =  WSAENOTEMPTY;
  EPROCLIM           =  WSAEPROCLIM;
  EUSERS             =  WSAEUSERS;
  EDQUOT             =  WSAEDQUOT;
  ESTALE             =  WSAESTALE;
  EREMOTE            =  WSAEREMOTE;

  EAI_ADDRFAMILY  = 1;   // Address family for nodename not supported.
  EAI_AGAIN       = 2;   // Temporary failure in name resolution.
  EAI_BADFLAGS    = 3;   // Invalid value for ai_flags.
  EAI_FAIL        = 4;   // Non-recoverable failure in name resolution.
  EAI_FAMILY      = 5;   // Address family ai_family not supported.
  EAI_MEMORY      = 6;   // Memory allocation failure.
  EAI_NODATA      = 7;   // No address associated with nodename.
  EAI_NONAME      = 8;   // Nodename nor servname provided, or not known.
  EAI_SERVICE     = 9;   // Servname not supported for ai_socktype.
  EAI_SOCKTYPE    = 10;  // Socket type ai_socktype not supported.
  EAI_SYSTEM      = 11;  // System error returned in errno.

const
  WSADESCRIPTION_LEN     =   256;
  WSASYS_STATUS_LEN      =   128;

  SHUT_RD = 0;
  SHUT_WR = 1;
  SHUT_RDWR = 2;

type
  PWSAData = ^TWSAData;
  TWSAData = packed record
    wVersion: Word;
    wHighVersion: Word;
    szDescription: array[0..WSADESCRIPTION_LEN] of AnsiChar;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of AnsiChar;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PAnsiChar;
  end;

  function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
  function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6):boolean;
  procedure SET_IN6_IF_ADDR_ANY (const a: PInAddr6);
  procedure SET_LOOPBACK_ADDR6 (const a: PInAddr6);
var
  in6addr_any, in6addr_loopback : TInAddr6;

function FD_ISSET(Socket: TSocket; const FDSet: TFDSet): boolean;
procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
procedure FD_ZERO(var FDSet: TFDSet);


// poll() emulation via WSAPoll() extension API available since Vista

const
  // poll/WSAPoll flag when normal data may be read
  POLLRDNORM  = $0100;
  // poll/WSAPoll flag when priority data may be read
  POLLRDBAND  = $0200;
  // poll/WSAPoll flag when there is data to read
  POLLIN       = POLLRDNORM or POLLRDBAND;
  // poll/WSAPoll flag when there is urgent data to read
  POLLPRI      = $0400;
  // poll/WSAPoll flag when writing now will not block
  POLLOUT      = $0010;
  // poll/WSAPoll flag error condition (always implicitly polled for)
  POLLERR      = $0001;
  // poll/WSAPoll flag hung up (always implicitly polled for)
  POLLHUP      = $0002;
  // poll/WSAPoll flag invalid polling request (always implicitly polled for)
  POLLNVAL     = $0004;
  // poll/WSAPoll flag when writing now will not block
  POLLWRNORM   = $0010;
  // poll/WSAPoll flag when priority data may be written
  POLLWRBAND   = $0020;

type
  /// polling request data structure for poll/WSAPoll
  TPollFD = record
    /// file descriptor to poll
    fd: TSocket;
    /// types of events poller cares about
    // - mainly POLLIN and/or POLLOUT
    events: SHORT;
    /// types of events that actually occurred
    // - caller could just reset revents := 0 to reuse the structure
    revents: SHORT;
  end;
  PPollFD = ^TPollFD;
  TPollFDDynArray = array of TPollFD;

/// Poll the file descriptors described by the NFDS structures starting at fds
// - under Windows, will call WSAPoll() emulation API - see
// https://blogs.msdn.microsoft.com/wndp/2006/10/26
// - if TIMEOUT is nonzero and not -1, allow TIMEOUT milliseconds for
// an event to occur; if TIMEOUT is -1, block until an event occurs
// - returns the number of file descriptors with events, zero if timed out,
// or -1 for errors
// - before Vista, will return -1 since the API extension was not yet defined
// - in practice, this API is actually slightly SLOWER than optimized Select() :(
function poll(fds: PPollFD; nfds, timeout: integer): integer;


type
  TWSAStartup = function(wVersionRequired: Word; var WSData: TWSAData): integer; stdcall;
  TWSACleanup = function: integer; stdcall;
  TWSAGetLastError = function: integer; stdcall;
  TGetServByName = function(name, proto: PAnsiChar): PServEnt; stdcall;
  TGetServByPort = function(port: integer; proto: PAnsiChar): PServEnt; stdcall;
  TGetProtoByName = function(name: PAnsiChar): PProtoEnt; stdcall;
  TGetProtoByNumber = function(proto: integer): PProtoEnt; stdcall;
  TGetHostByName = function(name: PAnsiChar): PHostEnt; stdcall;
  TGetHostByAddr = function(addr: Pointer; len, Struc: integer): PHostEnt; stdcall;
  TGetHostName = function(name: PAnsiChar; len: integer): integer; stdcall;
  TShutdown = function(s: TSocket; how: integer): integer; stdcall;
  TSetSockOpt = function(s: TSocket; level, optname: integer; optval: PAnsiChar;
    optlen: integer): integer; stdcall;
  TGetSockOpt = function(s: TSocket; level, optname: integer; optval: PAnsiChar;
    var optlen: integer): integer; stdcall;
  TSendTo = function(s: TSocket; Buf: pointer; len, flags: integer; addrto: PSockAddr;
    tolen: integer): integer; stdcall;
  TSend = function(s: TSocket; Buf: pointer; len, flags: integer): integer; stdcall;
  TRecv = function(s: TSocket; Buf: pointer; len, flags: integer): integer; stdcall;
  TRecvFrom = function(s: TSocket; Buf: pointer; len, flags: integer; from: PSockAddr;
    fromlen: PInteger): integer; stdcall;
  Tntohs = function(netshort: u_short): u_short; stdcall;
  Tntohl = function(netlong: u_long): u_long; stdcall;
  TListen = function(s: TSocket; backlog: integer): integer; stdcall;
  TIoctlSocket = function(s: TSocket; cmd: DWORD; var arg: integer): integer; stdcall;
  TInet_ntoa = function(inaddr: TInAddr): PAnsiChar; stdcall;
  TInet_addr = function(cp: PAnsiChar): u_long; stdcall;
  Thtons = function(hostshort: u_short): u_short; stdcall;
  Thtonl = function(hostlong: u_long): u_long; stdcall;
  TGetSockName = function(s: TSocket; name: PSockAddr; var namelen: integer): integer; stdcall;
  TGetPeerName = function(s: TSocket; name: PSockAddr; var namelen: integer): integer; stdcall;
  TConnect = function(s: TSocket; name: PSockAddr; namelen: integer): integer; stdcall;
  TCloseSocket = function(s: TSocket): integer; stdcall;
  TBind = function(s: TSocket; addr: PSockAddr; namelen: integer): integer; stdcall;
  TAccept = function(s: TSocket; addr: PSockAddr; var addrlen: integer): TSocket; stdcall;
  TTSocket = function(af, Struc, Protocol: integer): TSocket; stdcall;
  TSelect = function(nfds: integer; readfds, writefds, exceptfds: PFDSet;
    timeout: PTimeVal): Longint; stdcall;
  TGetAddrInfo = function(NodeName: PAnsiChar; ServName: PAnsiChar; Hints: PAddrInfo;
    var Addrinfo: PAddrInfo): integer; stdcall;
  TFreeAddrInfo = procedure(ai: PAddrInfo); stdcall;
  TGetNameInfo = function( addr: PSockAddr; namelen: integer; host: PAnsiChar;
    hostlen: DWORD; serv: PAnsiChar; servlen: DWORD; flags: integer): integer; stdcall;
  T__WSAFDIsSet = function (s: TSocket; var FDSet: TFDSet): Bool; stdcall;
  TWSAIoctl = function (s: TSocket; dwIoControlCode: DWORD; lpvInBuffer: Pointer;
    cbInBuffer: DWORD; lpvOutBuffer: Pointer; cbOutBuffer: DWORD;
    lpcbBytesReturned: PDWORD; lpOverlapped: Pointer;
    lpCompletionRoutine: pointer): u_int; stdcall;
  TWSAPoll = function(fds: PPollFD; nfds, timeout: integer): integer; stdcall;

var
  WSAStartup: TWSAStartup;
  WSACleanup: TWSACleanup;
  WSAGetLastError: TWSAGetLastError;
  GetServByName: TGetServByName;
  GetServByPort: TGetServByPort;
  GetProtoByName: TGetProtoByName;
  GetProtoByNumber: TGetProtoByNumber;
  GetHostByName: TGetHostByName;
  GetHostByAddr: TGetHostByAddr;
  ssGetHostName: TGetHostName;
  Shutdown: TShutdown;
  SetSockOpt: TSetSockOpt;
  GetSockOpt: TGetSockOpt;
  SendTo: TSendTo;
  Send: TSend;
  Recv: TRecv;
  RecvFrom: TRecvFrom;
  ntohs: Tntohs;
  ntohl: Tntohl;
  Listen: TListen;
  IoctlSocket: TIoctlSocket;
  Inet_ntoa: TInet_ntoa;
  Inet_addr: TInet_addr;
  htons: Thtons;
  htonl: Thtonl;
  ssGetSockName: TGetSockName;
  ssGetPeerName: TGetPeerName;
  ssConnect: TConnect;
  CloseSocket: TCloseSocket;
  ssBind: TBind;
  ssAccept: TAccept;
  Socket: TTSocket;
  Select: TSelect;
  GetAddrInfo: TGetAddrInfo;
  FreeAddrInfo: TFreeAddrInfo;
  GetNameInfo: TGetNameInfo;
  __WSAFDIsSet: T__WSAFDIsSet;
  WSAIoctl: TWSAIoctl;
  WSAPoll: TWSAPoll;

var
  SynSockCS: TRTLCriticalSection;
  SockEnhancedApi: Boolean;
  SockWship6Api: Boolean;
  SockSChannelApi: Boolean;

type
  PVarSin = ^TVarSin;
  TVarSin = packed record
    case integer of
      0: (AddressFamily: u_short);
      1: (
        case sin_family: u_short of
          AF_INET: (sin_port: u_short;
                    sin_addr: TInAddr;
                    sin_zero: array[0..7] of AnsiChar);
          AF_INET6: (sin6_port:    u_short;
                     sin6_flowinfo: u_long;
                     sin6_addr:     TInAddr6;
                     sin6_scope_id: u_long);
          );
  end;

function SizeOfVarSin(const sin: TVarSin): integer;
 {$ifdef UNICODE}inline;{$endif}

function GetSockName(s: TSocket; var name: TVarSin): integer;
function GetPeerName(s: TSocket; var name: TVarSin): integer;
function GetHostName: AnsiString;
function Bind(s: TSocket; const addr: TVarSin): integer;
function Connect(s: TSocket; const name: TVarSin): integer;
function Accept(s: TSocket; var addr: TVarSin): TSocket;

function IsNewApi(Family: integer): Boolean;
 {$ifdef UNICODE}inline;{$endif}
function SetVarSin(var Sin: TVarSin; const IP, Port: AnsiString; Family, SockProtocol, SockType: integer; PreferIP4: Boolean): integer;
function GetSinIP(const Sin: TVarSin): AnsiString;
procedure GetSinIPShort(const Sin: TVarSin; var result: shortstring);
function GetSinPort(const Sin: TVarSin): integer;
procedure ResolveNameToIP(const Name: AnsiString; Family, SockProtocol, SockType: integer;
  IPList: TStrings; IPListClear: boolean = true);
function ResolveIPToName(const IP: AnsiString; Family, SockProtocol, SockType: integer): AnsiString;
function ResolvePort(const Port: AnsiString; Family, SockProtocol, SockType: integer): Word;


{ SChannel low-level API  }

type
  TCredHandle = record
    dwLower: pointer;
    dwUpper: pointer;
  end;
  PCredHandle = ^TCredHandle;

  TCtxtHandle = type TCredHandle;
  PCtxtHandle = ^TCtxtHandle;

  {$ifdef DELPHI5OROLDER}
  PCardinal = ^Cardinal;
  {$endif}

  TSChannelCred = record
    dwVersion: cardinal;
    cCreds: cardinal;
    paCred: pointer;
    hRootStore: THandle;
    cMappers: cardinal;
    aphMappers: pointer;
    cSupportedAlgs: cardinal;
    palgSupportedAlgs: PCardinal;
    grbitEnabledProtocols: cardinal;
    dwMinimumCipherStrength: cardinal;
    dwMaximumCipherStrength: cardinal;
    dwSessionLifespan: cardinal;
    dwFlags: cardinal;
    dwCredFormat: cardinal;
  end;
  PSChannelCred = ^TSChannelCred;

  TSecBuffer = record
    cbBuffer: cardinal;
    BufferType: cardinal;
    pvBuffer: pointer;
  end;
  PSecBuffer = ^TSecBuffer;

  TSecBufferDesc = record
    ulVersion: cardinal;
    cBuffers: cardinal;
    pBuffers: PSecBuffer;
  end;
  PSecBufferDesc = ^TSecBufferDesc;

  TTimeStamp = record
    dwLowDateTime: cardinal;
    dwHighDateTime: cardinal;
  end;
  PTimeStamp = ^TTimeStamp;

  TSecPkgContextStreamSizes = record
    cbHeader: cardinal;
    cbTrailer: cardinal;
    cbMaximumMessage: cardinal;
    cBuffers: cardinal;
    cbBlockSize: cardinal;
  end;
  PSecPkgContextStreamSizes = ^TSecPkgContextStreamSizes;

  ESChannel = class(Exception);

  {$ifdef USERECORDWITHMETHODS}TSChannelClient = record
    {$else}TSChannelClient = object{$endif}
  private
    Cred: TCredHandle;
    Ctxt: TCtxtHandle;
    Sizes: TSecPkgContextStreamSizes;
    Data, Input: AnsiString;
    InputSize, DataPos, DataCount, InputCount: integer;
    SessionClosed: boolean;
    procedure HandshakeLoop(aSocket: THandle);
    procedure AppendData(const aBuffer: TSecBuffer);
  public
    Initialized: boolean;
    procedure AfterConnection(aSocket: THandle; aAddress: PAnsiChar);
    procedure BeforeDisconnection(aSocket: THandle);
    function Receive(aSocket: THandle; aBuffer: pointer; aLength: integer): integer;
    function Send(aSocket: THandle; aBuffer: pointer; aLength: integer): integer;
  end;

var
  AcquireCredentialsHandle: function(pszPrincipal: PAnsiChar;
    pszPackage: PAnsiChar; fCredentialUse: cardinal; pvLogonID: PInt64;
    pAuthData: PSChannelCred; pGetKeyFn: pointer; pvGetKeyArgument: pointer;
    phCredential: PCredHandle; ptsExpiry: PTimeStamp): cardinal; stdcall;
  FreeCredentialsHandle: function(phCredential: PCredHandle): cardinal; stdcall;
  InitializeSecurityContext: function(phCredential: PCredHandle;
    phContext: PCtxtHandle; pszTargetName: PAnsiChar; fContextReq: cardinal;
    Reserved1: cardinal; TargetDataRep: cardinal; pInput: PSecBufferDesc;
    Reserved2: cardinal; phNewContext: PCtxtHandle; pOutput: PSecBufferDesc;
    pfContextAttr: PCardinal; ptsExpiry: PTimeStamp): cardinal; stdcall;
  DeleteSecurityContext: function(phContext: PCtxtHandle): cardinal; stdcall;
  ApplyControlToken: function(phContext: PCtxtHandle;
    pInput: PSecBufferDesc): cardinal; stdcall;
  QueryContextAttributes: function(phContext: PCtxtHandle;
    ulAttribute: cardinal; pBuffer: pointer): cardinal; stdcall;
  FreeContextBuffer: function(pvContextBuffer: pointer): cardinal; stdcall;
  EncryptMessage: function(phContext: PCtxtHandle; fQOP: cardinal;
    pMessage: PSecBufferDesc; MessageSeqNo: cardinal): cardinal; stdcall;
  DecryptMessage: function(phContext: PCtxtHandle; pMessage: PSecBufferDesc;
    MessageSeqNo: cardinal; pfQOP: PCardinal): cardinal; stdcall;

const
  SP_PROT_TLS1 = $0C0;
  SP_PROT_TLS1_SERVER = $040;
  SP_PROT_TLS1_CLIENT = $080;
  SP_PROT_TLS1_1 = $300;
  SP_PROT_TLS1_1_SERVER = $100;
  SP_PROT_TLS1_1_CLIENT = $200;
  SP_PROT_TLS1_2 = $C00;
  SP_PROT_TLS1_2_SERVER = $400;
  SP_PROT_TLS1_2_CLIENT = $800;

  SECPKG_CRED_INBOUND = 1;
  SECPKG_CRED_OUTBOUND = 2;

  ISC_REQ_DELEGATE = $00000001;
  ISC_REQ_MUTUAL_AUTH = $00000002;
  ISC_REQ_REPLAY_DETECT = $00000004;
  ISC_REQ_SEQUENCE_DETECT = $00000008;
  ISC_REQ_CONFIDENTIALITY = $00000010;
  ISC_REQ_USE_SESSION_KEY = $00000020;
  ISC_REQ_PROMPT_FOR_CREDS = $00000040;
  ISC_REQ_USE_SUPPLIED_CREDS = $00000080;
  ISC_REQ_ALLOCATE_MEMORY = $00000100;
  ISC_REQ_USE_DCE_STYLE = $00000200;
  ISC_REQ_DATAGRAM = $00000400;
  ISC_REQ_CONNECTION = $00000800;
  ISC_REQ_CALL_LEVEL = $00001000;
  ISC_REQ_FRAGMENT_SUPPLIED = $00002000;
  ISC_REQ_EXTENDED_ERROR = $00004000;
  ISC_REQ_STREAM = $00008000;
  ISC_REQ_INTEGRITY = $00010000;
  ISC_REQ_IDENTIFY = $00020000;
  ISC_REQ_NULL_SESSION = $00040000;
  ISC_REQ_MANUAL_CRED_VALIDATION = $00080000;
  ISC_REQ_RESERVED1 = $00100000;
  ISC_REQ_FRAGMENT_TO_FIT = $00200000;
  ISC_REQ_FLAGS =
    ISC_REQ_SEQUENCE_DETECT or ISC_REQ_REPLAY_DETECT or
    ISC_REQ_CONFIDENTIALITY or ISC_REQ_EXTENDED_ERROR or
    ISC_REQ_ALLOCATE_MEMORY or ISC_REQ_STREAM or
    ISC_REQ_MANUAL_CRED_VALIDATION;

  SECBUFFER_VERSION = 0;
  SECBUFFER_EMPTY = 0;
  SECBUFFER_DATA = 1;
  SECBUFFER_TOKEN = 2;
  SECBUFFER_EXTRA = 5;
  SECBUFFER_STREAM_TRAILER = 6;
  SECBUFFER_STREAM_HEADER = 7;

  SEC_E_OK = 0;
  SEC_I_CONTINUE_NEEDED = $00090312;
  SEC_I_INCOMPLETE_CREDENTIALS = $00090320;
  SEC_I_RENEGOTIATE = $00090321;
  SEC_I_CONTEXT_EXPIRED	= $00090317;
  SEC_E_INCOMPLETE_MESSAGE = $80090318;
  SEC_E_INVALID_TOKEN = $80090308;

  UNISP_NAME = 'Microsoft Unified Security Protocol Provider';
  SECPKG_ATTR_STREAM_SIZES = 4;
  SECURITY_NATIVE_DREP = $10;
  SCHANNEL_SHUTDOWN = 1;
{$endif}
implementation
{$ifdef MSWINDOWS}
var
  SynSockCount: integer;
  LibHandle: {$ifdef FPC}TLibHandle{$else}HMODULE{$endif};
  Libwship6Handle: {$ifdef FPC}TLibHandle{$else}HMODULE{$endif};
  LibSecurHandle: {$ifdef FPC}TLibHandle{$else}HMODULE{$endif};

function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
begin
  result := ((a^.u6_addr32[0] = 0) and (a^.u6_addr32[1] = 0) and
             (a^.u6_addr32[2] = 0) and (a^.u6_addr32[3] = 0));
end;

function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
begin
  result := ((a^.u6_addr32[0] = 0) and (a^.u6_addr32[1] = 0) and
             (a^.u6_addr32[2] = 0) and
             (a^.u6_addr8[12] = 0) and (a^.u6_addr8[13] = 0) and
             (a^.u6_addr8[14] = 0) and (a^.u6_addr8[15] = 1));
end;

function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
begin
  result := ((a^.u6_addr8[0] = $FE) and (a^.u6_addr8[1] = $80));
end;

function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
begin
  result := ((a^.u6_addr8[0] = $FE) and (a^.u6_addr8[1] = $C0));
end;

function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
begin
  result := (a^.u6_addr8[0] = $FF);
end;

function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6): boolean;
begin
  result := (CompareMem(a, b, sizeof(TInAddr6)));
end;

procedure SET_IN6_IF_ADDR_ANY(const a: PInAddr6);
begin
  FillChar(a^, sizeof(TInAddr6), 0);
end;

procedure SET_LOOPBACK_ADDR6(const a: PInAddr6);
begin
  FillChar(a^, sizeof(TInAddr6), 0);
  a^.u6_addr8[15] := 1;
end;

// faster purepascal versions of FD_ISSET/FD_CLR/FD_SET/FD_ZERO API functions

function FD_ISSET(Socket: TSocket; const FDSet: TFDSet): boolean;
var
  i: integer;
begin
  result := true;
  for i := 0 to FDSet.fd_count - 1 do
    if FDSet.fd_array[i] = Socket then
      exit; // found item
  result := false;
end;

procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
var
  i: integer;
begin
  for i := 0 to FDSet.fd_count - 1 do
    if FDSet.fd_array[i] = Socket then begin
      dec(FDSet.fd_count);
      if i < FDSet.fd_count then
        move(FDSet.fd_array[i + 1], FDSet.fd_array[i], (FDSet.fd_count - i) * sizeof(TSocket));
      break;
    end;
end;

procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
var
  i: integer;
begin
  if FDSet.fd_count >= FD_SETSIZE then
    exit;
  for i := 0 to FDSet.fd_count - 1 do
    if FDSet.fd_array[i] = Socket then
      exit; // already set
  FDSet.fd_array[FDSet.fd_count] := Socket;
  inc(FDSet.fd_count);
end;

procedure FD_ZERO(var FDSet: TFDSet);
begin
  FDSet.fd_count := 0;
end;

function SizeOfVarSin(const sin: TVarSin): integer;
begin
  case sin.sin_family of
    AF_INET:
      result := SizeOf(TSockAddrIn);
    AF_INET6:
      result := SizeOf(TSockAddrIn6);
  else
    result := 0;
  end;
end;

function GetSockName(s: TSocket; var name: TVarSin): integer;
var
  len: integer;
begin
  len := SizeOf(name);
  FillChar(name, len, 0);
  result := ssGetSockName(s, @name, len);
end;

function GetPeerName(s: TSocket; var name: TVarSin): integer;
var
  len: integer;
begin
  len := SizeOf(name);
  FillChar(name, len, 0);
  result := ssGetPeerName(s, @name, len);
end;

function GetHostName: AnsiString;
var
  s: array[0..255] of AnsiChar;
begin
  ssGetHostName(@s, 255);
  result := s;
end;

function Accept(s: TSocket; var addr: TVarSin): TSocket;
var
  x: integer;
begin
  x := SizeOf(addr);
  result := ssAccept(s, @addr, x);
end;

function Bind(s: TSocket; const addr: TVarSin): integer;
begin
  result := ssBind(s, @addr, SizeOfVarSin(addr));
end;

function Connect(s: TSocket; const name: TVarSin): integer;
begin
  result := ssConnect(s, @name, SizeOfVarSin(name));
end;

function IsNewApi(Family: integer): Boolean;
begin
  result := SockEnhancedApi;
  if not result then
    result := (Family = AF_INET6) and SockWship6Api;
end;

function SetVarSin(var Sin: TVarSin; const IP, Port: AnsiString; Family, SockProtocol, SockType: integer; PreferIP4: Boolean): integer;
type
  pu_long = ^u_long;
var
  ProtoEnt: PProtoEnt;
  ServEnt: PServEnt;
  HostEnt: PHostEnt;
  r: integer;
  Hints1, Hints2: TAddrInfo;
  Sin1, Sin2: TVarSin;
  TwoPass: boolean;

  function GetAddr(const IP, port: AnsiString; var Hints: TAddrInfo; var Sin: TVarSin): integer;
  var
    Addr: PAddrInfo;
  begin
    Addr := nil;
    try
      FillChar(Sin, Sizeof(Sin), 0);
      if Hints.ai_socktype = SOCK_RAW then begin
        Hints.ai_socktype := 0;
        Hints.ai_protocol := 0;
        result := GetAddrInfo(pointer(IP), nil, @Hints, Addr);
      end
      else begin
        if (IP = cAnyHost) or (IP = c6AnyHost) then begin
          Hints.ai_flags := AI_PASSIVE;
          result := GetAddrInfo(nil, pointer(port), @Hints, Addr);
        end
        else if (IP = cLocalhost) or (IP = c6Localhost) then
          result := GetAddrInfo(nil, pointer(port), @Hints, Addr)
        else
          result := GetAddrInfo(pointer(IP), pointer(port), @Hints, Addr);
      end;
      if result = 0 then
        if (Addr <> nil) then
          Move(Addr^.ai_addr^, Sin, Addr^.ai_addrlen);
    finally
      if Assigned(Addr) then
        FreeAddrInfo(Addr);
    end;
  end;

begin
  result := 0;
  FillChar(Sin, Sizeof(Sin), 0);
  if not IsNewApi(Family) then begin
    EnterCriticalSection(SynSockCS);
    try
      Sin.sin_family := AF_INET;
      ProtoEnt := GetProtoByNumber(SockProtocol);
      ServEnt := nil;
      if ProtoEnt <> nil then
        ServEnt := GetServByName(pointer(Port), ProtoEnt^.p_name);
      if ServEnt = nil then
        Sin.sin_port := htons(StrToIntDef(string(Port), 0))
      else
        Sin.sin_port := ServEnt^.s_port;
      if IP = cBroadcast then
        Sin.sin_addr.s_addr := u_long(INADDR_BROADCAST)
      else begin
        Sin.sin_addr.s_addr := inet_addr(pointer(IP));
        if Sin.sin_addr.s_addr = u_long(INADDR_NONE) then begin
          HostEnt := GetHostByName(pointer(IP));
          result := WSAGetLastError;
          if HostEnt <> nil then
            Sin.sin_addr.S_addr := u_long(Pu_long(HostEnt^.h_addr_list^)^);
        end;
      end;
    finally
      LeaveCriticalSection(SynSockCS);
    end;
  end
  else begin
    FillChar(Hints1, Sizeof(Hints1), 0);
    FillChar(Hints2, Sizeof(Hints2), 0);
    TwoPass := False;
    if Family = AF_UNSPEC then begin
      if PreferIP4 then begin
        Hints1.ai_family := AF_INET;
        Hints2.ai_family := AF_INET6;
        TwoPass := True;
      end
      else begin
        Hints2.ai_family := AF_INET;
        Hints1.ai_family := AF_INET6;
        TwoPass := True;
      end;
    end
    else
      Hints1.ai_family := Family;
    Hints1.ai_socktype := SockType;
    Hints2.ai_socktype := Hints1.ai_socktype;
    Hints1.ai_protocol := SockProtocol;
    Hints2.ai_protocol := Hints1.ai_protocol;
    r := GetAddr(IP, Port, Hints1, Sin1);
    result := r;
    Sin := Sin1;
    if r <> 0 then
      if TwoPass then begin
        r := GetAddr(IP, Port, Hints2, Sin2);
        result := r;
        if r = 0 then
          Sin := Sin2;
      end;
  end;
end;

function GetSinIP(const Sin: TVarSin): AnsiString;
var
  p: PAnsiChar;
  host: array[0..NI_MAXHOST] of AnsiChar;
  serv: array[0..NI_MAXSERV] of AnsiChar;
  hostlen, servlen: integer;
  r: integer;
begin
  result := '';
  if not IsNewApi(Sin.AddressFamily) then begin
    p := inet_ntoa(Sin.sin_addr);
    if p <> nil then
      result := p;
  end
  else begin
    hostlen := NI_MAXHOST;
    servlen := NI_MAXSERV;
    r := getnameinfo(@Sin, SizeOfVarSin(Sin), host, hostlen, serv, servlen,
      NI_NUMERICHOST + NI_NUMERICSERV);
    if r = 0 then
      result := host;
  end;
end;

function StrLen255(S: PAnsiChar): integer;
begin
  for result := 0 to 254 do
    if S[result] = #0 then
      exit;
  result := 255;
end;

procedure GetSinIPShort(const Sin: TVarSin; var result: shortstring);
var
  p: PAnsiChar;
  host: array[0..NI_MAXHOST] of AnsiChar;
  serv: array[0..NI_MAXSERV] of AnsiChar;
  hostlen, servlen: integer;
  r: integer;
begin
  result[0] := #0;
  if not IsNewApi(Sin.AddressFamily) then begin
    p := inet_ntoa(Sin.sin_addr);
    if p <> nil then
      SetString(result, p, StrLen255(p));
  end
  else begin
    hostlen := NI_MAXHOST;
    servlen := NI_MAXSERV;
    r := getnameinfo(@Sin, SizeOfVarSin(Sin), host, hostlen, serv, servlen,
      NI_NUMERICHOST + NI_NUMERICSERV);
    if r = 0 then
      SetString(result, PAnsiChar(@host), StrLen255(host));
  end;
end;

function GetSinPort(const Sin: TVarSin): integer;
begin
  if (Sin.sin_family = AF_INET6) then
    result := ntohs(Sin.sin6_port)
  else
    result := ntohs(Sin.sin_port);
end;

procedure ResolveNameToIP(const Name: AnsiString; Family, SockProtocol,
  SockType: integer; IPList: TStrings; IPListClear: boolean);
type
  TaPInAddr = array[0..250] of PInAddr;
var
  Hints: TAddrInfo;
  Addr: PAddrInfo;
  AddrNext: PAddrInfo;
  r: integer;
  host: array[0..NI_MAXHOST] of AnsiChar;
  serv: array[0..NI_MAXSERV] of AnsiChar;
  hostlen, servlen: integer;
  RemoteHost: PHostEnt;
  IP: u_long;
  PAdrPtr: ^TaPInAddr;
  i: integer;
  InAddr: TInAddr;
begin
  if IPListClear then
    IPList.Clear;
  if not IsNewApi(Family) then begin
    IP := inet_addr(pointer(Name));
    if IP = u_long(INADDR_NONE) then begin
      EnterCriticalSection(SynSockCS);
      try
        RemoteHost := GetHostByName(pointer(Name));
        if RemoteHost <> nil then begin
          PAdrPtr := pointer(RemoteHost^.h_addr_list);
          i := 0;
          while PAdrPtr^[i] <> nil do begin
            InAddr := PAdrPtr^[i]^;
            IPList.Add(Format('%d.%d.%d.%d', [InAddr.S_bytes[0],
              InAddr.S_bytes[1], InAddr.S_bytes[2], InAddr.S_bytes[3]]));
            Inc(i);
          end;
        end;
      finally
        LeaveCriticalSection(SynSockCS);
      end;
    end
    else
      IPList.Add(string(Name));
  end
  else begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_socktype := SockType;
      Hints.ai_protocol := SockProtocol;
      r := GetAddrInfo(pointer(Name), nil, @Hints, Addr);
      if r = 0 then begin
        AddrNext := Addr;
        while not (AddrNext = nil) do begin
          if not (((Family = AF_INET6) and (AddrNext^.ai_family = AF_INET)) or
             ((Family = AF_INET) and (AddrNext^.ai_family = AF_INET6))) then begin
            hostlen := NI_MAXHOST;
            servlen := NI_MAXSERV;
            r := getnameinfo(AddrNext^.ai_addr, AddrNext^.ai_addrlen, host, hostlen,
              serv, servlen, NI_NUMERICHOST + NI_NUMERICSERV);
            if r = 0 then
              IPList.Add(string(host));
          end;
          AddrNext := AddrNext^.ai_next;
        end;
      end;
    finally
      if Assigned(Addr) then
        FreeAddrInfo(Addr);
    end;
  end;
  if IPList.Count = 0 then
    IPList.Add(cAnyHost);
end;

function ResolvePort(const Port: AnsiString; Family, SockProtocol, SockType: integer): Word;
var
  ProtoEnt: PProtoEnt;
  ServEnt: PServEnt;
  Hints: TAddrInfo;
  Addr: PAddrInfo;
  r: integer;
begin
  result := 0;
  if not IsNewApi(Family) then begin
    EnterCriticalSection(SynSockCS);
    try
      ProtoEnt := GetProtoByNumber(SockProtocol);
      ServEnt := nil;
      if ProtoEnt <> nil then
        ServEnt := GetServByName(pointer(Port), ProtoEnt^.p_name);
      if ServEnt = nil then
        result := StrToIntDef(string(Port), 0)
      else
        result := htons(ServEnt^.s_port);
    finally
      LeaveCriticalSection(SynSockCS);
    end;
  end
  else begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_socktype := SockType;
      Hints.ai_protocol := SockProtocol;
      Hints.ai_flags := AI_PASSIVE;
      r := GetAddrInfo(nil, pointer(Port), @Hints, Addr);
      if (r = 0) and Assigned(Addr) then begin
        if Addr^.ai_family = AF_INET then
          result := htons(Addr^.ai_addr^.sin_port);
        if Addr^.ai_family = AF_INET6 then
          result := htons(PSockAddrIn6(Addr^.ai_addr)^.sin6_port);
      end;
    finally
      if Assigned(Addr) then
        FreeAddrInfo(Addr);
    end;
  end;
end;

function ResolveIPToName(const IP: AnsiString; Family, SockProtocol, SockType: integer): AnsiString;
var
  Hints: TAddrInfo;
  Addr: PAddrInfo;
  r: integer;
  host: array[0..NI_MAXHOST] of AnsiChar;
  serv: array[0..NI_MAXSERV] of AnsiChar;
  hostlen, servlen: integer;
  RemoteHost: PHostEnt;
  IPn: u_long;
begin
  result := IP;
  if not IsNewApi(Family) then begin
    IPn := inet_addr(pointer(IP));
    if IPn <> u_long(INADDR_NONE) then begin
      EnterCriticalSection(SynSockCS);
      try
        RemoteHost := GetHostByAddr(@IPn, SizeOf(IPn), AF_INET);
        if RemoteHost <> nil then
          result := RemoteHost^.h_name;
      finally
        LeaveCriticalSection(SynSockCS);
      end;
    end;
  end
  else begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_socktype := SockType;
      Hints.ai_protocol := SockProtocol;
      r := GetAddrInfo(pointer(IP), nil, @Hints, Addr);
      if (r = 0) and Assigned(Addr) then begin
        hostlen := NI_MAXHOST;
        servlen := NI_MAXSERV;
        r := getnameinfo(Addr^.ai_addr, Addr^.ai_addrlen, host, hostlen,
          serv, servlen, NI_NUMERICSERV);
        if r = 0 then
          result := host;
      end;
    finally
      if Assigned(Addr) then
        FreeAddrInfo(Addr);
    end;
  end;
end;

function poll(fds: PPollFD; nfds, timeout: integer): integer;
begin
  if Assigned(WSAPoll) then
    result := WSAPoll(fds, nfds, timeout)
  else
    result := -1; // not available on XP/2K
end;

function InitSocketInterface(const Stack: TFileName = ''): Boolean;
begin
  result := False;
  EnterCriticalSection(SynSockCS);
  try
    if SynSockCount = 0 then begin
      SockEnhancedApi := false;
      SockSChannelApi := false;
      SockWship6Api := false;
      if Stack = '' then
        LibHandle := LoadLibrary(DLLStackName)
      else
        LibHandle := LoadLibrary(pointer(Stack));
      if LibHandle <> 0 then begin
        WSAPoll := GetProcAddress(LibHandle, 'WSAPoll');
        WSAIoctl := GetProcAddress(LibHandle, 'WSAIoctl');
        __WSAFDIsSet := GetProcAddress(LibHandle, '__WSAFDIsSet');
        CloseSocket := GetProcAddress(LibHandle, 'closesocket');
        IoctlSocket := GetProcAddress(LibHandle, 'ioctlsocket');
        WSAGetLastError := GetProcAddress(LibHandle, 'WSAGetLastError');
        WSAStartup := GetProcAddress(LibHandle, 'WSAStartup');
        WSACleanup := GetProcAddress(LibHandle, 'WSACleanup');
        ssAccept := GetProcAddress(LibHandle, 'accept');
        ssBind := GetProcAddress(LibHandle, 'bind');
        ssConnect := GetProcAddress(LibHandle, 'connect');
        ssGetPeerName := GetProcAddress(LibHandle, 'getpeername');
        ssGetSockName := GetProcAddress(LibHandle, 'getsockname');
        GetSockOpt := GetProcAddress(LibHandle, 'getsockopt');
        Htonl := GetProcAddress(LibHandle, 'htonl');
        Htons := GetProcAddress(LibHandle, 'htons');
        Inet_Addr := GetProcAddress(LibHandle, 'inet_addr');
        Inet_Ntoa := GetProcAddress(LibHandle, 'inet_ntoa');
        Listen := GetProcAddress(LibHandle, 'listen');
        Ntohl := GetProcAddress(LibHandle, 'ntohl');
        Ntohs := GetProcAddress(LibHandle, 'ntohs');
        Recv := GetProcAddress(LibHandle, 'recv');
        RecvFrom := GetProcAddress(LibHandle, 'recvfrom');
        Select := GetProcAddress(LibHandle, 'select');
        Send := GetProcAddress(LibHandle, 'send');
        SendTo := GetProcAddress(LibHandle, 'sendto');
        SetSockOpt := GetProcAddress(LibHandle, 'setsockopt');
        ShutDown := GetProcAddress(LibHandle, 'shutdown');
        Socket := GetProcAddress(LibHandle, 'socket');
        GetHostByAddr := GetProcAddress(LibHandle, 'gethostbyaddr');
        GetHostByName := GetProcAddress(LibHandle, 'gethostbyname');
        GetProtoByName := GetProcAddress(LibHandle, 'getprotobyname');
        GetProtoByNumber := GetProcAddress(LibHandle, 'getprotobynumber');
        GetServByName := GetProcAddress(LibHandle, 'getservbyname');
        GetServByPort := GetProcAddress(LibHandle, 'getservbyport');
        ssGetHostName := GetProcAddress(LibHandle, 'gethostname');
        {$ifndef FORCEOLDAPI}
        GetAddrInfo := GetProcAddress(LibHandle, 'getaddrinfo');
        FreeAddrInfo := GetProcAddress(LibHandle, 'freeaddrinfo');
        GetNameInfo := GetProcAddress(LibHandle, 'getnameinfo');
        SockEnhancedApi := Assigned(GetAddrInfo) and
          Assigned(FreeAddrInfo) and Assigned(GetNameInfo);
        if not SockEnhancedApi then begin
          LibWship6Handle := LoadLibrary(DLLWship6);
          if LibWship6Handle <> 0 then begin
            GetAddrInfo := GetProcAddress(LibWship6Handle, 'getaddrinfo');
            FreeAddrInfo := GetProcAddress(LibWship6Handle, 'freeaddrinfo');
            GetNameInfo := GetProcAddress(LibWship6Handle, 'getnameinfo');
            SockWship6Api := Assigned(GetAddrInfo) and
              Assigned(FreeAddrInfo) and Assigned(GetNameInfo);
          end;
        end;
        {$endif}
        if not SockSChannelApi then begin
          LibSecurHandle := LoadLibrary(DLLSecur32);
          if LibSecurHandle <> 0 then begin
            AcquireCredentialsHandle := GetProcAddress(LibSecurHandle, 'AcquireCredentialsHandleA');
            FreeCredentialsHandle := GetProcAddress(LibSecurHandle, 'FreeCredentialsHandle');
            InitializeSecurityContext := GetProcAddress(LibSecurHandle, 'InitializeSecurityContextA');
            DeleteSecurityContext := GetProcAddress(LibSecurHandle, 'DeleteSecurityContext');
            ApplyControlToken := GetProcAddress(LibSecurHandle, 'ApplyControlToken');
            QueryContextAttributes := GetProcAddress(LibSecurHandle, 'QueryContextAttributesA');
            FreeContextBuffer := GetProcAddress(LibSecurHandle, 'FreeContextBuffer');
            EncryptMessage := GetProcAddress(LibSecurHandle, 'EncryptMessage');
            DecryptMessage := GetProcAddress(LibSecurHandle, 'DecryptMessage');
            SockSChannelApi := Assigned(AcquireCredentialsHandle) and
              Assigned(InitializeSecurityContext) and
              Assigned(QueryContextAttributes) and
              Assigned(EncryptMessage) and Assigned(DecryptMessage);
          end;
        end;
        result := True;
      end;
    end
    else
      result := True;
    if result then
      Inc(SynSockCount);
  finally
    LeaveCriticalSection(SynSockCS);
  end;
end;

function DestroySocketInterface: Boolean;
begin
  EnterCriticalSection(SynSockCS);
  try
    Dec(SynSockCount);
    if SynSockCount < 0 then
      SynSockCount := 0;
    if SynSockCount = 0 then begin
      if LibHandle <> 0 then begin
        FreeLibrary(libHandle);
        LibHandle := 0;
        // HH reset routine pointers to avoid jumping into limbo
        WSAPoll := nil;
        WSAIoctl := nil;
        __WSAFDIsSet := nil;
        CloseSocket := nil;
        IoctlSocket := nil;
        WSAGetLastError := nil;
        WSAStartup := nil;
        WSACleanup := nil;
        ssAccept := nil;
        ssBind := nil;
        ssConnect := nil;
        ssGetPeerName := nil;
        ssGetSockName := nil;
        GetSockOpt := nil;
        Htonl := nil;
        Htons := nil;
        Inet_Addr := nil;
        Inet_Ntoa := nil;
        Listen := nil;
        Ntohl := nil;
        Ntohs := nil;
        Recv := nil;
        RecvFrom := nil;
        Select := nil;
        Send := nil;
        SendTo := nil;
        SetSockOpt := nil;
        ShutDown := nil;
        Socket := nil;
        GetHostByAddr := nil;
        GetHostByName := nil;
        GetProtoByName := nil;
        GetProtoByNumber := nil;
        GetServByName := nil;
        GetServByPort := nil;
        ssGetHostName := nil;
        {$ifndef FORCEOLDAPI}
        GetAddrInfo := nil;
        FreeAddrInfo := nil;
        GetNameInfo := nil;
        GetAddrInfo := nil;
        FreeAddrInfo := nil;
        GetNameInfo := nil;
        {$endif}
        AcquireCredentialsHandle := nil;
        FreeCredentialsHandle := nil;
        InitializeSecurityContext := nil;
        DeleteSecurityContext := nil;
        ApplyControlToken := nil;
        QueryContextAttributes := nil;
        FreeContextBuffer := nil;
        EncryptMessage := nil;
        DecryptMessage := nil;
      end;
      if LibWship6Handle <> 0 then begin
        FreeLibrary(LibWship6Handle);
        LibWship6Handle := 0;
      end;
    end;
  finally
    LeaveCriticalSection(SynSockCS);
  end;
  result := True;
end;




{ TSChannel }

procedure RaiseLastError; // not defined e.g. with Delphi 5
var
  LastError: Integer;
begin
  LastError := GetLastError;
  raise ESChannel.CreateFmt('System Error %d [%s]', [LastError, SysErrorMessage(LastError)]);
end;

function CheckSEC_E_OK(res: integer): cardinal;
begin
  if res <> SEC_E_OK then
    RaiseLastError;
  result := res;
end;

function CheckSocket(res: integer): cardinal;
begin
  if res = SOCKET_ERROR then
    raise ESChannel.CreateFmt('Socket Error %d', [WSAGetLastError]);
  if res = 0 then
    raise ESChannel.Create('Handshake aborted');
  result := res;
end;

const
  TLSRECMAXSIZE = 19000; // stack buffers for TSChannelClient.Receive/Send

type
  {$ifdef USERECORDWITHMETHODS}THandshakeBuf = record
    {$else}THandshakeBuf = object{$endif}
  public
    buf: array[0..2] of TSecBuffer;
    input, output: TSecBufferDesc;
    procedure Init;
  end;

procedure THandshakeBuf.Init;
begin
  input.ulVersion := SECBUFFER_VERSION;
  input.cBuffers := 2;
  input.pBuffers := @buf[0];
  buf[0].cbBuffer := 0;
  buf[0].BufferType := SECBUFFER_TOKEN;
  buf[0].pvBuffer := nil;
  buf[1].cbBuffer := 0;
  buf[1].BufferType := SECBUFFER_EMPTY;
  buf[1].pvBuffer := nil;
  output.ulVersion := SECBUFFER_VERSION;
  output.cBuffers := 1;
  output.pBuffers := @buf[2];
  buf[2].cbBuffer := 0;
  buf[2].BufferType := SECBUFFER_TOKEN;
  buf[2].pvBuffer := nil;
end;

procedure TSChannelClient.AppendData(const aBuffer: TSecBuffer);
var
  newlen: integer;
begin
  newlen := DataCount + integer(aBuffer.cbBuffer);
  if newlen > Length(Data) then
    SetLength(Data, newlen);
  Move(aBuffer.pvBuffer^, PByteArray(Data)[DataCount], aBuffer.cbBuffer);
  inc(DataCount, aBuffer.cbBuffer);
end;

procedure TSChannelClient.AfterConnection(aSocket: THandle; aAddress: PAnsiChar);
var
  buf: THandshakeBuf;
  res, f: cardinal;
begin
  if not SockSChannelApi then
    raise ESChannel.Create('SChannel API not available');
  CheckSEC_E_OK(AcquireCredentialsHandle(nil, UNISP_NAME, SECPKG_CRED_OUTBOUND,
    nil, nil, nil, nil, @Cred, nil));
  DataPos := 0;
  DataCount := 0;
  buf.Init;
  res := InitializeSecurityContext(@Cred, nil, aAddress, ISC_REQ_FLAGS, 0,
    SECURITY_NATIVE_DREP, nil, 0, @Ctxt, @buf.output, @f, nil);
  if res <> SEC_I_CONTINUE_NEEDED then
    RaiseLastError;
  CheckSocket(SynWinSock.Send(aSocket, buf.buf[2].pvBuffer, buf.buf[2].cbBuffer, 0));
  CheckSEC_E_OK(FreeContextBuffer(buf.buf[2].pvBuffer));
  SetLength(Data, TLSRECMAXSIZE);
  HandshakeLoop(aSocket);
  CheckSEC_E_OK(QueryContextAttributes(@Ctxt, SECPKG_ATTR_STREAM_SIZES, @Sizes));
  InputSize := Sizes.cbHeader + Sizes.cbMaximumMessage + Sizes.cbTrailer;
  if InputSize > TLSRECMAXSIZE then
    raise ESChannel.CreateFmt('InputSize=%d>%d', [InputSize, TLSRECMAXSIZE]);
  SetLength(Input, InputSize);
  InputCount := 0;
  Initialized := true;
end;

procedure TSChannelClient.HandshakeLoop(aSocket: THandle);
var
  buf: THandshakeBuf;
  res, f: cardinal;
begin
  res := SEC_I_CONTINUE_NEEDED;
  while (res = SEC_I_CONTINUE_NEEDED) or (res = SEC_E_INCOMPLETE_MESSAGE) do begin
    inc(DataCount, CheckSocket(Recv(aSocket,
      @PByteArray(Data)[DataCount], length(Data) - DataCount, 0)));
    buf.Init;
    buf.buf[0].cbBuffer := DataCount;
    buf.buf[0].BufferType := SECBUFFER_TOKEN;
    buf.buf[0].pvBuffer := pointer(Data);
    res := InitializeSecurityContext(@Cred, @Ctxt, nil, ISC_REQ_FLAGS, 0,
      SECURITY_NATIVE_DREP, @buf.input, 0, @Ctxt, @buf.output, @f, nil);
    if res = SEC_I_INCOMPLETE_CREDENTIALS then
      // check https://stackoverflow.com/a/47479968/458259
      res := InitializeSecurityContext(@Cred, @Ctxt, nil, ISC_REQ_FLAGS, 0,
        SECURITY_NATIVE_DREP, @buf.input, 0, @Ctxt, @buf.output, @f, nil);
    if (res = SEC_E_OK) or (res = SEC_I_CONTINUE_NEEDED) or
       ((f and ISC_REQ_EXTENDED_ERROR) <> 0) then begin
      if (buf.buf[2].cbBuffer <> 0) and (buf.buf[2].pvBuffer <> nil) then begin
        CheckSocket(
          SynWinSock.Send(aSocket, buf.buf[2].pvBuffer, buf.buf[2].cbBuffer, 0));
        CheckSEC_E_OK(FreeContextBuffer(buf.buf[2].pvBuffer));
      end;
    end;
    if buf.buf[1].BufferType = SECBUFFER_EXTRA then begin
      // reuse pending Data bytes to avoid SEC_E_INVALID_TOKEN
      Move(PByteArray(Data)[cardinal(DataCount) - buf.buf[1].cbBuffer],
           PByteArray(Data)[0], buf.buf[1].cbBuffer);
      DataCount := buf.buf[1].cbBuffer;
    end else
    if res <> SEC_E_INCOMPLETE_MESSAGE then
      DataCount := 0;
  end;
  // TODO: handle SEC_I_INCOMPLETE_CREDENTIALS ?
  // see https://github.com/curl/curl/blob/master/lib/vtls/schannel.c
  CheckSEC_E_OK(res);
end;

procedure TSChannelClient.BeforeDisconnection(aSocket: THandle);
var
  desc: TSecBufferDesc;
  buf: TSecBuffer;
  dt, f: cardinal;
begin
  if Initialized then
  try
    if aSocket > 0 then begin
      desc.ulVersion := SECBUFFER_VERSION;
      desc.cBuffers := 1;
      desc.pBuffers := @buf;
      buf.cbBuffer := 4;
      buf.BufferType := SECBUFFER_TOKEN;
      dt := SCHANNEL_SHUTDOWN;
      buf.pvBuffer := @dt;
      if ApplyControlToken(@Ctxt, @desc) = SEC_E_OK then begin
        buf.cbBuffer := 0;
        buf.BufferType := SECBUFFER_TOKEN;
        buf.pvBuffer := nil;
        if InitializeSecurityContext(@Cred, @Ctxt, nil, ISC_REQ_FLAGS, 0,
           SECURITY_NATIVE_DREP, nil, 0, @Ctxt, @desc, @f, nil) = SEC_E_OK then begin
          SynWinSock.Send(aSocket, buf.pvBuffer, buf.cbBuffer, 0);
          FreeContextBuffer(buf.pvBuffer);
        end;
      end;
    end;
    DeleteSecurityContext(@Ctxt);
    FreeCredentialsHandle(@Cred);
  finally
    Cred.dwLower := nil;
    Cred.dwUpper := nil;
    Initialized := false;
  end;
end;

function TSChannelClient.Receive(aSocket: THandle;
  aBuffer: pointer; aLength: integer): integer;
var
  desc: TSecBufferDesc;
  buf: array[0..3] of TSecBuffer;
  res: cardinal;
  read, i: integer;
  needsRenegotiate: boolean;
  function DecryptInput: cardinal;
  begin
    buf[0].cbBuffer := InputCount;
    buf[0].BufferType := SECBUFFER_DATA;
    buf[0].pvBuffer := pointer(Input);
    buf[1].cbBuffer := 0;
    buf[1].BufferType := SECBUFFER_EMPTY;
    buf[1].pvBuffer := nil;
    buf[2].cbBuffer := 0;
    buf[2].BufferType := SECBUFFER_EMPTY;
    buf[2].pvBuffer := nil;
    buf[3].cbBuffer := 0;
    buf[3].BufferType := SECBUFFER_EMPTY;
    buf[3].pvBuffer := nil;
    result := DecryptMessage(@Ctxt, @desc, 0, nil);
  end;
begin
  if not Initialized then begin // use plain socket API
    result := Recv(aSocket, aBuffer, aLength, MSG_NOSIGNAL);
    exit;
  end;
  result := 0;
  if not SessionClosed then
    while DataCount = 0 do
    try
      DataPos := 0;
      desc.ulVersion := SECBUFFER_VERSION;
      desc.cBuffers := 4;
      desc.pBuffers := @buf[0];
      repeat
        read := Recv(aSocket, @PByteArray(Input)[InputCount],
          InputSize - InputCount, MSG_NOSIGNAL);
        if read <= 0 then begin
          result := read; // return socket error (may be WSATRY_AGAIN)
          exit;
        end;
        inc(InputCount, read);
        res := DecryptInput;
      until res <> SEC_E_INCOMPLETE_MESSAGE;
      needsRenegotiate := false;
      repeat
        case res of
          SEC_I_RENEGOTIATE: needsRenegotiate := true;
          SEC_I_CONTEXT_EXPIRED: SessionClosed := true;
          SEC_E_INCOMPLETE_MESSAGE: break;
          else CheckSEC_E_OK(res);
        end;
        InputCount := 0;
        for i := 1 to 3 do
          case buf[i].BufferType of
            SECBUFFER_DATA: AppendData(buf[i]);
            SECBUFFER_EXTRA: begin
              Move(buf[i].pvBuffer^, pointer(Input)^, buf[i].cbBuffer);
              InputCount := buf[i].cbBuffer;
            end;
          end;
        if InputCount = 0 then
          break;
        res := DecryptInput;
      until false;
      if needsRenegotiate then
        HandshakeLoop(aSocket);
    except
      exit; // shutdown the connection on ESChannel fatal error
    end;
  result := DataCount;
  if aLength < result then
    result := aLength;
  Move(PByteArray(Data)[DataPos], aBuffer^, result);
  inc(DataPos, result);
  dec(DataCount, result);
end;

function TSChannelClient.Send(aSocket: THandle; aBuffer: pointer; aLength: integer): integer;
var
  desc: TSecBufferDesc;
  buf: array[0..3] of TSecBuffer;
  res, sent, s, len, trailer, pending, templen: cardinal;
  temp: array[0..TLSRECMAXSIZE] of byte;
begin
  if not Initialized then begin // use plain socket API
    result := SynWinSock.Send(aSocket, aBuffer, aLength, MSG_NOSIGNAL);
    exit;
  end;
  result := 0;
  desc.ulVersion := SECBUFFER_VERSION;
  desc.cBuffers := 4;
  desc.pBuffers := @buf[0];
  pending := aLength;
  while pending > 0 do begin
    templen := pending;
    if templen > Sizes.cbMaximumMessage then
      templen := Sizes.cbMaximumMessage;
    Move(aBuffer^, temp[Sizes.cbHeader], templen);
    inc(PByte(aBuffer), templen);
    dec(pending, templen);
    trailer := Sizes.cbHeader + templen;
    buf[0].cbBuffer := Sizes.cbHeader;
    buf[0].BufferType := SECBUFFER_STREAM_HEADER;
    buf[0].pvBuffer := @temp;
    buf[1].cbBuffer := templen;
    buf[1].BufferType := SECBUFFER_DATA;
    buf[1].pvBuffer := @temp[Sizes.cbHeader];
    buf[2].cbBuffer := Sizes.cbTrailer;
    buf[2].BufferType := SECBUFFER_STREAM_TRAILER;
    buf[2].pvBuffer := @temp[trailer];
    buf[3].cbBuffer := 0;
    buf[3].BufferType := SECBUFFER_EMPTY;
    buf[3].pvBuffer := nil;
    if EncryptMessage(@Ctxt, 0, @desc, 0) <> SEC_E_OK then
      exit; // shutdown the connection on SChannel error
    len := buf[0].cbBuffer + buf[1].cbBuffer + buf[2].cbBuffer;
    sent := 0;
    repeat
      s := SynWinSock.Send(aSocket, @temp[sent], len, MSG_NOSIGNAL);
      if s = len then
        break; // whole message sent
      if s = 0 then
        exit;  // report connection closed
      if integer(s) < 0 then begin
        res := WSAGetLastError;
        if res <> WSATRY_AGAIN then begin
          result := s;
          exit; // report socket fatal error
        end;
      end
      else begin
        dec(len, s);
        inc(sent, s);
      end;
      Sleep(1); // try again
    until false;
  end;
  result := aLength;
end;

initialization
  assert(SizeOf(TInAddr) = SizeOf(cardinal));
  assert(SizeOf(TSockAddrIn) = 16);
  assert(SizeOf(TInAddr6) = 16);

  InitializeCriticalSection(SynSockCS);
  SET_IN6_IF_ADDR_ANY(@in6addr_any);
  SET_LOOPBACK_ADDR6(@in6addr_loopback);

finalization
  SynSockCount := -254; // force release library
  DestroySocketInterface;
  DeleteCriticalSection(SynSockCS);
{$endif}
end.

