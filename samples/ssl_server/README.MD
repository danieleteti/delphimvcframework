# DMVCFRAMEWORK SSL SAMPLE

This sample uses the classic `IdSSLOpenSSL` IOHandler, which needs OpenSSL 1.0.2
(`libeay32.dll` / `ssleay32.dll`) and tops out at **TLS 1.2**.
For TLS 1.3 on the 3.5 hosting API see [`samples/tls13`](../tls13/README.md), which
configures HTTPS on `IMVCServer` with TaurusTLS and OpenSSL 3.x.

In order to generate a valid self-signed certificate to use https, follow these instructions:
- Download an OpenSSL version compatible with INDY components from `https://github.com/IndySockets/OpenSSL-Binaries` (the old `indy.fulgan.com/SSL/` archive is gone)
    - Take care of the bitness! 
- Download a compatible version of Apache Httpd from `https://www.apachelounge.com/download/`
- Unzip Apache into a folder (e.g. `C:\Tools\Apache`)
- Open GENERATE_CERTIFICATES.BAT and change the APACHEPATH variable according with your apache path and Save it.
- Open a command prompt in samples\sslserver and run `GENERATE_CERTIFICATES.BAT`
- Copy `libeay32.dll` and `ssleay32.dll` (from the openssl zip) into the `samples\sslserver\bin` folder
- Run the DMVCFramework ssl sample
- Navigate to `https://localhost`