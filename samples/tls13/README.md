# TLS 1.3

HTTPS served by **Indy Direct** with **TaurusTLS** (OpenSSL 1.1.1+ / 3.x), with the
protocol floor pinned to **TLS 1.3**.

TLS is configured on `IMVCServer` — `UseHTTPS`, `CertFile`, `KeyFile` and the
`HTTPSConfigurator` that installs the TLS stack. The caller never touches the
IOHandler of the Indy component directly.

```delphi
LServer := TMVCServerFactory.CreateIndyDirect(LEngine);
LServer.HTTPSConfigurator := TaurusTLSIndyConfigurator();
LServer.UseHTTPS := True;
LServer.CertFile := '...\localhost.crt';
LServer.KeyFile  := '...\localhost.key';
LServer.Listen(8443);
```

That is all HTTPS needs. This sample adds one thing on top: TaurusTLS defaults its
floor to TLS 1.2, so `TLS13OnlyConfigurator` wraps the framework configurator and
sets `SSLOptions.MinTLSVersion := TLSv1_3` on the IOHandler it just built. Wrapping
the configurator is the extension point for any TaurusTLS knob the framework does
not surface.

The port is **8443, not 443, on purpose**. Indy negotiates TLS only on 443 unless
told otherwise; `UseHTTPS` forces it on every port for you.

## Requirements

**TaurusTLS** — the `DCC_UnitSearchPath` in `TLS13Sample.dproj` points at
`C:\DLib\indy_extras\TaurusTLS\Source`. Adjust it to your checkout, or drop the two
entries if you installed TaurusTLS through GetIt (its path is then already global).

**OpenSSL 1.1.1+ or 3.x DLLs** next to `bin\TLS13Sample.exe` (or on the `PATH`):
`libssl-3.dll` + `libcrypto-3.dll` for Win32, `libssl-3-x64.dll` +
`libcrypto-3-x64.dll` for Win64. Copies live in
`unittests\general\TestClient\bin32\` and `bin64\`.

**Certificates** — `bin\certificates\localhost.crt` and `localhost.key` are
committed: a self-signed development certificate for `localhost`, valid until
June 2027. To regenerate them (and get the CA into the Windows trusted root store,
so browsers stop warning), run as Administrator:

```
tools\certificatesgenerator\create-localhost-certificate.bat
```

then copy `localhost.crt` and `localhost.key` into `bin\certificates\`.

## Run

```
TLS13Sample.exe              serves https://localhost:8443/hello until Ctrl+C
TLS13Sample.exe selftest     connects to itself and reports what was negotiated
```

`selftest` exits 1 unless the connection is TLS 1.3:

```
response : {"message":"Hello over TLS 1.3"}
protocol : TLSv1.3
cipher   : TLS_AES_256_GCM_SHA384
PASS: TLS 1.3 negotiated
```

## Verify the floor from outside

With the server running:

```
openssl s_client -connect localhost:8443 -tls1_3    ->  Protocol: TLSv1.3
openssl s_client -connect localhost:8443 -tls1_2    ->  tlsv1 alert protocol version
```

A TLS 1.2 client is refused at the handshake — there is no downgrade.

## Production notes

- Drop `TLS13OnlyConfigurator` and use `TaurusTLSIndyConfigurator()` directly if you
  still need to serve TLS 1.2 clients.
- Use a real certificate. Read paths and the password from `.env` via `dotEnv`
  (`LServer.CertPassword := dotEnv.Env('https.cert.password', '')`) rather than
  hard-coding them; never commit a production key.
- The same properties work on a WebBroker host — swap the configurator for
  `TaurusTLSWebBrokerConfigurator()`. On HTTP.sys, TLS is bound outside the process
  with `netsh http add sslcert` and the certificate properties are ignored.
