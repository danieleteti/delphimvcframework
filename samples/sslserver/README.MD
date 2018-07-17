# DMVCFRAMEWORK SSL SAMPLE

In order to generate a valid self-signed certificate to use https, follow these instructions:
- Download openssl version compatibile with INDY components from `http://indy.fulgan.com/SSL/`
    - Take care of the bitness! 
- Download a compatible version of Apache Httpd from `https://www.apachelounge.com/download/`
- Unzip Apache into a folder (e.g. `C:\Tools\Apache`)
- Open GENERATE_CERTIFICATES.BAT and change the APACHEPATH variable according with your apache path and Save it.
- Open a command prompt in samples\sslserver and run `GENERATE_CERTIFICATES.BAT`
- Copy `libeay32.dll` and `ssleay32.dll` (from the openssl zip) into the `samples\sslserver\bin` folder
- Run the DMVCFramework ssl sample
- Navitate to `https://localhost`