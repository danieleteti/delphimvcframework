unit Iocp.OpenSSL;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils, System.SyncObjs;

const
  SSLEAY_DLL = {$IFDEF MACOS} '/usr/lib/libssl.dylib'; {$ELSE} 'SSLEAY32.DLL'; {$ENDIF}
  LIBEAY_DLL = {$IFDEF MACOS} '/usr/lib/libcrypto.dylib'; {$ELSE} 'LIBEAY32.DLL'; {$ENDIF}

  SSL_ERROR_NONE                              = 0;
  SSL_ERROR_SSL                               = 1;
  SSL_ERROR_WANT_READ                         = 2;
  SSL_ERROR_WANT_WRITE                        = 3;
  SSL_ERROR_WANT_X509_LOOKUP                  = 4;
  SSL_ERROR_SYSCALL                           = 5;
  SSL_ERROR_ZERO_RETURN                       = 6;
  SSL_ERROR_WANT_CONNECT                      = 7;
  SSL_ERROR_WANT_ACCEPT                       = 8;

  SSL_ST_CONNECT                              = $1000;
  SSL_ST_ACCEPT                               = $2000;
  SSL_ST_MASK                                 = $0FFF;
  SSL_ST_INIT                                 = (SSL_ST_CONNECT or SSL_ST_ACCEPT);
  SSL_ST_BEFORE                               = $4000;
  SSL_ST_OK                                   = $03;
  SSL_ST_RENEGOTIATE                          = ($04 or SSL_ST_INIT);

  BIO_CTRL_INFO		 = 3;  // opt - extra tit-bits
  BIO_CTRL_PENDING = 10; // opt - is their more data buffered
  SSL_VERIFY_NONE  = $00;

  CRYPTO_LOCK		= 1;
  CRYPTO_UNLOCK	= 2;
  CRYPTO_READ   = 4;
  CRYPTO_WRITE  = 8;

  BIO_FLAGS_READ                          = 1;
  BIO_FLAGS_WRITE                         = 2;
  BIO_FLAGS_IO_SPECIAL                    = 4;
  BIO_FLAGS_RWS                           = (BIO_FLAGS_READ or
                                             BIO_FLAGS_WRITE or
                                             BIO_FLAGS_IO_SPECIAL);
  BIO_FLAGS_SHOULD_RETRY                  = 8;

type
  size_t = NativeUInt;

  {$REGION 'SSL'}
  TSSL_METHOD_st = packed record
    Dummy: array [0..0] of Byte;
  end;
  PSSL_METHOD = ^TSSL_METHOD_st;

  TSSL_CTX_st = packed record
    Dummy: array [0..0] of Byte;
  end;
  PSSL_CTX = ^TSSL_CTX_st;

  TBIO_st = packed record
    Dummy: array [0..0] of Byte;
  end;
  PBIO = ^TBIO_st;
  PPBIO = ^PBIO;

  TSSL_st = packed record
    Dummy: array [0..0] of Byte;
  end;
  PSSL = ^TSSL_st;

  TX509_STORE_CTX_st = packed record
    Dummy: array [0..0] of Byte;
  end;
  PX509_STORE_CTX = ^TX509_STORE_CTX_st;

  TEVP_PKEY_st = packed record
    Dummy: array [0..0] of Byte;
  end;
  PEVP_PKEY = ^TEVP_PKEY_st;
  PPEVP_PKEY = ^PEVP_PKEY;

  TX509_st = packed record
    Dummy: array [0..0] of Byte;
  end;
  PX509 = ^TX509_st;
  PPX509 = ^PX509;

  // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
  TASN1_STRING_st = record
    length : Integer;
    type_  : Integer;
    data   : PAnsiChar;
    //* The value of the following field depends on the type being
    //* held.  It is mostly being used for BIT_STRING so if the
    //* input data has a non-zero 'unused bits' value, it will be
    //* handled correctly */
    flags  : Longword;
  end;
  PASN1_STRING       = ^TASN1_STRING_st;
  TASN1_OCTET_STRING = TASN1_STRING_st;
  PASN1_OCTET_STRING = ^TASN1_OCTET_STRING;
  TASN1_BIT_STRING   = TASN1_STRING_st;
  PASN1_BIT_STRING   = ^TASN1_BIT_STRING;

  TSetVerify_cb = function(Ok: Integer; StoreCtx: PX509_STORE_CTX): Integer; cdecl;
  {$ENDREGION}

  {$REGION 'LIBEAY'}
  TCRYPTO_THREADID_st = packed record
    Dummy: array [0..0] of Byte;
  end;
  PCRYPTO_THREADID = ^TCRYPTO_THREADID_st;

  TCRYPTO_dynlock_value_st = record
    Mutex: TCriticalSection;
  end;
  PCRYPTO_dynlock_value = ^TCRYPTO_dynlock_value_st;
  CRYPTO_dynlock_value  = TCRYPTO_dynlock_value_st;

  TBIO_METHOD_st = packed record
    Dummy: array [0..0] of Byte;
  end;
  PBIO_METHOD = ^TBIO_METHOD_st;

  TX509_NAME_st = packed record
    Dummy: array [0..0] of Byte;
  end;
  PX509_NAME = ^TX509_NAME_st;

  {TASN1_BIT_STRING_st = packed record
    Dummy: array [0..0] of Byte;
  end;
  PASN1_BIT_STRING = ^TASN1_BIT_STRING_st; }

  TSTACK_st = packed record
    Dummy : array [0..0] of Byte;
  end;
  PSTACK = ^TSTACK_st;

  TASN1_OBJECT_st = packed record
    Dummy : array [0..0] of Byte;
  end;
  PASN1_OBJECT = ^TASN1_OBJECT_st;

  TStatLockLockCallback   = procedure(Mode: Integer; N: Integer; const _File: PAnsiChar; Line: Integer); cdecl;
  TStatLockIDCallback     = function: Longword; cdecl;
  TCryptoThreadIDCallback = procedure(ID: PCRYPTO_THREADID) cdecl;

  TDynLockCreateCallback  = function(const _file: PAnsiChar; Line: Integer): PCRYPTO_dynlock_value; cdecl;
  TDynLockLockCallback    = procedure(Mode: Integer; L: PCRYPTO_dynlock_value; _File: PAnsiChar; Line: Integer); cdecl;
  TDynLockDestroyCallback = procedure(L: PCRYPTO_dynlock_value; _File: PAnsiChar; Line: Integer); cdecl;
  pem_password_cb         = function(buf: Pointer; size: Integer; rwflag: Integer; userdata: Pointer): Integer; cdecl;
  {$ENDREGION}

  {$REGION 'SSLTools'}
  TSSLTools = class
  private class var
    FRef: Integer;
  public
    class procedure LoadSSL;
    class procedure UnloadSSL;
    class function NewCTX: PSSL_CTX;
    class procedure FreeCTX(var ctx: PSSL_CTX);
    class procedure SetCert(Context: PSSL_CTX; CertPKey: Pointer; CertPKeySize: Integer); overload;
    class procedure SetCert(Context: PSSL_CTX; Cert: Pointer; CertSize: Integer; PKey: Pointer; PKeySize: Integer); overload;
    class procedure SetCert(Context: PSSL_CTX; const CertPKeyFile: string); overload;
    class procedure SetCert(Context: PSSL_CTX; const CertFile, PKeyFile: string); overload;
  end;
  {$ENDREGION}

var
  {$REGION 'SSL'}
  SSL_library_init:                       function: Integer; cdecl = nil;
  SSL_load_error_strings:                 procedure; cdecl = nil;
  SSLv23_method:                          function: PSSL_METHOD; cdecl = nil;
  SSL_CTX_new:                            function(meth: PSSL_METHOD): PSSL_CTX; cdecl = nil;
  SSL_CTX_free:                           procedure(ctx: PSSL_CTX); cdecl = nil;
  SSL_CTX_set_verify:                     procedure(ctx: PSSL_CTX; mode: Integer; callback: TSetVerify_cb); cdecl = nil;
  SSL_CTX_use_PrivateKey:                 function(ctx: PSSL_CTX; pkey: PEVP_PKEY): Integer; cdecl = nil;
  SSL_CTX_use_certificate:                function(ctx: PSSL_CTX; x: PX509): Integer; cdecl = nil;
  SSL_CTX_check_private_key:              function(ctx: PSSL_CTX): Integer; cdecl = nil;

  SSL_new:                                function(ctx: PSSL_CTX): PSSL; cdecl = nil;
  SSL_set_bio:                            procedure(s: PSSL; rbio, wbio: PBIO); cdecl = nil;
  SSL_get_peer_certificate:               function(s: PSSL): PX509; cdecl = nil;
  SSL_get_error:                          function(s: PSSL; ret_code: Integer): Integer; cdecl = nil;

  SSL_shutdown:                           function(s: PSSL): Integer; cdecl = nil;
  SSL_free:                               procedure(s: PSSL); cdecl = nil;

  SSL_set_connect_state:                  procedure(s: PSSL); cdecl = nil;
  SSL_set_accept_state:                   procedure(s: PSSL); cdecl = nil;
  SSL_read:                               function(s: PSSL; buf: Pointer; num: Integer): Integer; cdecl = nil;
  SSL_write:                              function(s: PSSL; const buf: Pointer; num: Integer): Integer; cdecl = nil;
  SSL_state:                              function(s: PSSL): Integer; cdecl = nil;
  SSL_pending:                            function(s: PSSL): Integer; cdecl = nil;
  {$ENDREGION}

  {$REGION 'LIBEAY'}
  CRYPTO_num_locks:                       function: Integer; cdecl = nil;
  CRYPTO_set_locking_callback:            procedure(callback: TStatLockLockCallback); cdecl = nil;
  CRYPTO_set_dynlock_create_callback:     procedure(callback: TDynLockCreateCallBack); cdecl = nil;
  CRYPTO_set_dynlock_lock_callback:       procedure(callback: TDynLockLockCallBack); cdecl = nil;
  CRYPTO_set_dynlock_destroy_callback:    procedure(callback: TDynLockDestroyCallBack); cdecl = nil;
  CRYPTO_cleanup_all_ex_data:             procedure; cdecl = nil;

  ERR_remove_state:                       procedure(tid: Cardinal); cdecl = nil;
  ERR_free_strings:                       procedure; cdecl = nil; // thread-unsafe, Application-global cleanup functions
  ERR_error_string_n:                     procedure(err: Cardinal; buf: PAnsiChar; len: size_t); cdecl = nil;
  ERR_get_error:                          function: Cardinal; cdecl = nil;

  EVP_cleanup:                            procedure; cdecl = nil;
  EVP_PKEY_free:                          procedure(pkey: PEVP_PKEY); cdecl = nil;

  BIO_new:                                function(BioMethods: PBIO_METHOD): PBIO; cdecl = nil;
  BIO_ctrl:                               function(bp: PBIO; cmd: Integer; larg: Longint; parg: Pointer): Longint; cdecl = nil;
  BIO_new_mem_buf:                        function(buf: Pointer; len: Integer): PBIO; cdecl = nil;
  BIO_free:                               function(b: PBIO): Integer; cdecl = nil;
  BIO_s_mem:                              function: PBIO_METHOD; cdecl = nil;
  BIO_read:                               function(b: PBIO; Buf: Pointer; Len: Integer): Integer; cdecl = nil;
  BIO_write:                              function(b: PBIO; Buf: Pointer; Len: Integer): Integer; cdecl = nil;

  X509_get_issuer_name:                   function(cert: PX509): PX509_NAME; cdecl = nil;
  X509_get_subject_name:                  function(cert: PX509): PX509_NAME; cdecl = nil;
  X509_free:                              procedure(cert: PX509); cdecl = nil;
  X509_NAME_print_ex:                     function(bout: PBIO; nm: PX509_NAME; indent: Integer; flags: Cardinal): Integer; cdecl = nil;
  X509_get_ext_d2i:                       function(x: PX509; nid: Integer; var crit, idx: Integer): Pointer; cdecl = nil;

  sk_num:                                 function(stack: PSTACK): Integer; cdecl = nil;
  sk_pop:                                 function(stack: PSTACK): Pointer; cdecl = nil;

  ASN1_BIT_STRING_get_bit:                function(a: PASN1_BIT_STRING; n: Integer): Integer; cdecl = nil;
  OBJ_obj2nid:                            function(o: PASN1_OBJECT): Integer; cdecl = nil;
  OBJ_nid2sn:                             function(n: Integer): PAnsiChar; cdecl = nil;
  ASN1_STRING_data:                       function(x: PASN1_STRING): Pointer; cdecl = nil;
  PEM_read_bio_X509:                      function(bp: PBIO; x: PPX509; cb: pem_password_cb; u: Pointer): PX509; cdecl = nil;
  PEM_read_bio_PrivateKey:                function(bp: PBIO; x: PPEVP_PKEY; cb: pem_password_cb; u: Pointer): PEVP_PKEY; cdecl = nil;
  {$ENDREGION}

function BIO_pending(bp: PBIO): Integer; inline;
function BIO_get_mem_data(bp: PBIO; parg: Pointer): Integer; inline;
function BIO_get_flags(b: PBIO): Integer; inline;
function BIO_should_retry(b: PBIO): Boolean; inline;

function SSL_is_init_finished(s: PSSL): Boolean; inline;

function ssl_is_fatal_error(ssl_error: Integer): Boolean;
function ssl_error(ssl: PSSL; ret_code: Integer): Integer;

function sk_ASN1_OBJECT_num(stack: PSTACK): Integer; inline;
function sk_GENERAL_NAME_num(stack: PSTACK): Integer; inline;
function sk_GENERAL_NAME_pop(stack: PSTACK): Pointer; inline;

implementation

uses
  System.IOUtils;

var
  _SsleayHandle, _LibeayHandle: HMODULE;
  _FSslLocks: TArray<TCriticalSection>;

function BIO_pending(bp: PBIO): Integer;
begin
  Result := BIO_ctrl(bp, BIO_CTRL_PENDING, 0, nil);
end;

function BIO_get_mem_data(bp: PBIO; parg: Pointer): Integer;
begin
  Result := BIO_ctrl(bp, BIO_CTRL_INFO, 0, parg);
end;

function sk_ASN1_OBJECT_num(stack: PSTACK): Integer;
begin
  Result := sk_num(stack);
end;

function sk_GENERAL_NAME_num(stack: PSTACK): Integer;
begin
  Result := sk_num(stack);
end;

function sk_GENERAL_NAME_pop(stack: PSTACK): Pointer;
begin
  Result := sk_pop(stack);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_get_flags(b: PBIO): Integer;
begin
  // This is a hack : BIO structure has not been defined. But I know
  // flags member is the 6th field in the structure (index is 5)
  // This could change when OpenSSL is updated. Check "struct bio_st".
  Result := PInteger(PAnsiChar(b) + 3 * SizeOf(Pointer) + 2 * SizeOf(Integer))^;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_should_retry(b: PBIO): Boolean;
begin
  Result := ((BIO_get_flags(b) and BIO_FLAGS_SHOULD_RETRY) <> 0);
end;

function SSL_is_init_finished(s: PSSL): Boolean; inline;
begin
  Result := (SSL_state(s) = SSL_ST_OK);
end;

function ssl_is_fatal_error(ssl_error: Integer): Boolean;
begin
	case ssl_error of
		SSL_ERROR_NONE,
		SSL_ERROR_WANT_READ,
		SSL_ERROR_WANT_WRITE,
		SSL_ERROR_WANT_CONNECT,
		SSL_ERROR_WANT_ACCEPT: Result := False;
  else
    Result := True;
	end;
end;

function ssl_error(ssl: PSSL; ret_code: Integer): Integer;
var
  error, error_log: Integer;
  msg: array [0..511] of Byte;
  s: AnsiString;
begin
	error := SSL_get_error(ssl, ret_code);
	if(error <> SSL_ERROR_NONE) then
	begin
		error_log := error;
		while (error_log <> SSL_ERROR_NONE) do
    begin
			ERR_error_string_n(error_log, PAnsiChar(@msg[0]), Length(msg));
			if (ssl_is_fatal_error(error_log)) then
			begin
        SetLength(s, 512);
        Move(msg[0], s[1], 512);
        SetLength(s, StrLen(PAnsiChar(@msg[0])));
				// print error message to console or logs
			end;
			error_log := ERR_get_error();
		end;
	end;
	Result := error;
end;

function LoadLib(const ALibFile: string): HMODULE;
begin
  Result := {$IFDEF MSWINDOWS}Winapi.Windows.{$ELSE}System.SysUtils.{$ENDIF}LoadLibrary(PChar(ALibFile));
  if (Result = 0) then
    raise Exception.CreateFmt('load %s failed', [ALibFile]);
end;

function FreeLib(ALibModule: HMODULE): Boolean;
begin
  Result := {$IFDEF MSWINDOWS}Winapi.Windows.{$ELSE}System.SysUtils.{$ENDIF}FreeLibrary(ALibModule);
end;

function GetProc(AModule: HMODULE; const AProcName: string): FARPROC;
begin
  Result := {$IFDEF MSWINDOWS}Winapi.Windows.{$ELSE}System.SysUtils.{$ENDIF}GetProcAddress(AModule, PChar(AProcName));
  if (Result = nil) then
    raise Exception.CreateFmt('%s is not found', [AProcName]);
end;

procedure LoadSsleay;
  function GetSslProc(const AProcName: string): FARPROC;
  begin
    Result := GetProc(_SsleayHandle, AProcName);
  end;
begin
  if (_SsleayHandle <> 0) then Exit;
  _SsleayHandle := LoadLib(SSLEAY_DLL);
  if (_SsleayHandle = 0) then
  begin
    raise Exception.CreateFmt('Load %s failed', [SSLEAY_DLL]);
    Exit;
  end;

  SSL_library_init         := GetSslProc('SSL_library_init');
  SSL_load_error_strings   := GetSslProc('SSL_load_error_strings');
  SSLv23_method            := GetSslProc('SSLv23_method');
  SSL_CTX_new              := GetSslProc('SSL_CTX_new');
  SSL_CTX_free             := GetSslProc('SSL_CTX_free');
  SSL_CTX_set_verify       := GetSslProc('SSL_CTX_set_verify');
  SSL_CTX_use_PrivateKey   := GetSslProc('SSL_CTX_use_PrivateKey');
  SSL_CTX_use_certificate  := GetSslProc('SSL_CTX_use_certificate');
  SSL_CTX_check_private_key:= GetSslProc('SSL_CTX_check_private_key');

  SSL_new                  := GetSslProc('SSL_new');
  SSL_set_bio              := GetSslProc('SSL_set_bio');
  SSL_get_peer_certificate := GetSslProc('SSL_get_peer_certificate');
  SSL_get_error            := GetSslProc('SSL_get_error');

  SSL_shutdown             := GetSslProc('SSL_shutdown');
  SSL_free                 := GetSslProc('SSL_free');

  SSL_set_connect_state    := GetSslProc('SSL_set_connect_state');
  SSL_set_accept_state     := GetSslProc('SSL_set_accept_state');
  SSL_read                 := GetSslProc('SSL_read');
  SSL_write                := GetSslProc('SSL_write');
  SSL_state                := GetSslProc('SSL_state');
  SSL_pending              := GetSslProc('SSL_pending');
end;

procedure UnloadSsleay;
begin
  if (_SsleayHandle = 0) then Exit;
  FreeLib(_SsleayHandle);
  _SsleayHandle := 0;
end;

procedure LoadLibeay;
  function GetEayProc(const AProcName: string): FARPROC;
  begin
    Result := GetProc(_LibeayHandle, AProcName);
  end;
begin
  if (_LibeayHandle <> 0) then Exit;
  _LibeayHandle := LoadLib(LIBEAY_DLL);
  if (_LibeayHandle = 0) then
  begin
    raise Exception.CreateFmt('Load %s failed', [LIBEAY_DLL]);
    Exit;
  end;

  CRYPTO_num_locks                    := GetEayProc('CRYPTO_num_locks');
  CRYPTO_set_locking_callback         := GetEayProc('CRYPTO_set_locking_callback');
  CRYPTO_set_dynlock_create_callback  := GetEayProc('CRYPTO_set_dynlock_create_callback');
  CRYPTO_set_dynlock_lock_callback    := GetEayProc('CRYPTO_set_dynlock_lock_callback');
  CRYPTO_set_dynlock_destroy_callback := GetEayProc('CRYPTO_set_dynlock_destroy_callback');
  CRYPTO_cleanup_all_ex_data          := GetEayProc('CRYPTO_cleanup_all_ex_data');

  ERR_remove_state                    := GetEayProc('ERR_remove_state');
  ERR_free_strings                    := GetEayProc('ERR_free_strings');
  ERR_error_string_n                  := GetEayProc('ERR_error_string_n');
  ERR_get_error                       := GetEayProc('ERR_get_error');

  EVP_cleanup                         := GetEayProc('EVP_cleanup');
  EVP_PKEY_free                       := GetEayProc('EVP_PKEY_free');

  BIO_new                             := GetEayProc('BIO_new');
  BIO_ctrl                            := GetEayProc('BIO_ctrl');
  BIO_new_mem_buf                     := GetEayProc('BIO_new_mem_buf');
  BIO_free                            := GetEayProc('BIO_free');
  BIO_s_mem                           := GetEayProc('BIO_s_mem');
  BIO_read                            := GetEayProc('BIO_read');
  BIO_write                           := GetEayProc('BIO_write');

  X509_get_issuer_name                := GetEayProc('X509_get_issuer_name');
  X509_get_subject_name               := GetEayProc('X509_get_subject_name');
  X509_free                           := GetEayProc('X509_free');
  X509_NAME_print_ex                  := GetEayProc('X509_NAME_print_ex');
  X509_get_ext_d2i                    := GetEayProc('X509_get_ext_d2i');

  sk_num                              := GetEayProc('sk_num');
  sk_pop                              := GetEayProc('sk_pop');

  ASN1_BIT_STRING_get_bit             := GetEayProc('ASN1_BIT_STRING_get_bit');
  OBJ_obj2nid                         := GetEayProc('OBJ_obj2nid');
  OBJ_nid2sn                          := GetEayProc('OBJ_nid2sn');
  ASN1_STRING_data                    := GetEayProc('ASN1_STRING_data');
  PEM_read_bio_X509                   := GetEayProc('PEM_read_bio_X509');
  PEM_read_bio_PrivateKey             := GetEayProc('PEM_read_bio_PrivateKey');
end;

procedure UnloadLibeay;
begin
  if (_LibeayHandle = 0) then Exit;
  FreeLib(_LibeayHandle);
  _LibeayHandle := 0;
end;

procedure ssl_lock_callback(Mode, N: Integer;
  const _File: PAnsiChar; Line: Integer); cdecl;
begin
	if(mode and CRYPTO_LOCK <> 0) then
    _FSslLocks[N].Enter
	else
    _FSslLocks[N].Leave;
end;

procedure ssl_lock_dyn_callback(Mode: Integer;
  L: PCRYPTO_dynlock_value; _File: PAnsiChar; Line: Integer); cdecl;
begin
  if (Mode and CRYPTO_LOCK <> 0) then
    L.Mutex.Enter
  else
    L.Mutex.Leave;
end;

function ssl_lock_dyn_create_callback(
  const _file: PAnsiChar; Line: Integer): PCRYPTO_dynlock_value; cdecl;
begin
  New(Result);
  Result.Mutex := TCriticalSection.Create;
end;

procedure ssl_lock_dyn_destroy_callback(
  L: PCRYPTO_dynlock_value; _File: PAnsiChar; Line: Integer); cdecl;
begin
  L.Mutex.Free;
  Dispose(L);
end;

procedure SslInit;
var
  LNumberOfLocks, I: Integer;
begin
  if (_SsleayHandle = 0) or (_LibeayHandle = 0) then Exit;

  LNumberOfLocks := CRYPTO_num_locks();
	if(LNumberOfLocks > 0) then
  begin
    SetLength(_FSslLocks, LNumberOfLocks);
    for I := Low(_FSslLocks) to High(_FSslLocks) do
      _FSslLocks[I] := TCriticalSection.Create;
	end;

	CRYPTO_set_locking_callback(ssl_lock_callback);
  CRYPTO_set_dynlock_create_callback(ssl_lock_dyn_create_callback);
	CRYPTO_set_dynlock_lock_callback(ssl_lock_dyn_callback);
  CRYPTO_set_dynlock_destroy_callback(ssl_lock_dyn_destroy_callback);

  SSL_load_error_strings();
  SSL_library_init();
end;

procedure SslUninit;
var
  I: Integer;
begin
  if (_SsleayHandle = 0) or (_LibeayHandle = 0) then Exit;

	CRYPTO_set_locking_callback(nil);
	CRYPTO_set_dynlock_create_callback(nil);
	CRYPTO_set_dynlock_lock_callback(nil);
	CRYPTO_set_dynlock_destroy_callback(nil);

  EVP_cleanup();
  CRYPTO_cleanup_all_ex_data();
  ERR_remove_state(0);
  ERR_free_strings();

  for I := Low(_FSslLocks) to High(_FSslLocks) do
    _FSslLocks[I].Free;
  _FSslLocks := nil;
end;

{ TSSLTools }

class function TSSLTools.NewCTX: PSSL_CTX;
begin
  if not Assigned(SSL_CTX_new) then Exit(nil);

  Result := SSL_CTX_new(SSLv23_method());
  SSL_CTX_set_verify(Result, SSL_VERIFY_NONE, nil);
end;

class procedure TSSLTools.FreeCTX(var ctx: PSSL_CTX);
begin
  if not Assigned(SSL_CTX_free) then Exit;

  SSL_CTX_free(ctx);
  ctx := nil;
end;

class procedure TSSLTools.LoadSSL;
begin
  if (TInterlocked.Increment(FRef) = 1) then
  begin
    LoadLibeay;
    LoadSsleay;
    SslInit;
  end;
end;

class procedure TSSLTools.UnloadSSL;
begin
  if (TInterlocked.Decrement(FRef) = 0) then
  begin
    SslUninit;
    UnloadSsleay;
    UnloadLibeay;
  end;
end;

class procedure TSSLTools.SetCert(Context: PSSL_CTX; CertPKey: Pointer;
  CertPKeySize: Integer);
var
  bio_cert: PBIO;
  ssl_cert: PX509;
  ssl_pkey: PEVP_PKEY;
begin
	bio_cert := BIO_new_mem_buf(CertPKey, CertPKeySize);
	ssl_cert := PEM_read_bio_X509(bio_cert, nil, nil, nil);
	ssl_pkey := PEM_read_bio_PrivateKey(bio_cert, nil, nil, nil);
	SSL_CTX_use_certificate(Context, ssl_cert);
	SSL_CTX_use_PrivateKey(Context, ssl_pkey);
	X509_free(ssl_cert);
	EVP_PKEY_free(ssl_pkey);
	BIO_free(bio_cert);

  if (SSL_CTX_check_private_key(Context) = 0) then
    raise Exception.Create('Private key does not match the certificate public key');
end;

class procedure TSSLTools.SetCert(Context: PSSL_CTX; Cert: Pointer;
  CertSize: Integer; PKey: Pointer; PKeySize: Integer);
var
  bio_cert, bio_pkey: PBIO;
  ssl_cert: PX509;
  ssl_pkey: PEVP_PKEY;
begin
	bio_cert := BIO_new_mem_buf(Cert, CertSize);
	bio_pkey := BIO_new_mem_buf(PKey, PKeySize);
	ssl_cert := PEM_read_bio_X509(bio_cert, nil, nil, nil);
	ssl_pkey := PEM_read_bio_PrivateKey(bio_pkey, nil, nil, nil);
	SSL_CTX_use_certificate(Context, ssl_cert);
	SSL_CTX_use_PrivateKey(Context, ssl_pkey);
	X509_free(ssl_cert);
	EVP_PKEY_free(ssl_pkey);
	BIO_free(bio_cert);
	BIO_free(bio_pkey);

  if (SSL_CTX_check_private_key(Context) = 0) then
    raise Exception.Create('Private key does not match the certificate public key');
end;

class procedure TSSLTools.SetCert(Context: PSSL_CTX;
  const CertPKeyFile: string);
var
  LCertPKeyBytes: TBytes;
begin
  LCertPKeyBytes := TFile.ReadAllBytes(CertPKeyFile);
  SetCert(Context, Pointer(LCertPKeyBytes), Length(LCertPKeyBytes));
end;

class procedure TSSLTools.SetCert(Context: PSSL_CTX; const CertFile,
  PKeyFile: string);
var
  LCertBytes, LPKeyBytes: TBytes;
begin
  LCertBytes := TFile.ReadAllBytes(CertFile);
  LPKeyBytes := TFile.ReadAllBytes(PKeyFile);
  SetCert(Context, Pointer(LCertBytes), Length(LCertBytes),
    Pointer(LPKeyBytes), Length(LPKeyBytes));
end;

end.
