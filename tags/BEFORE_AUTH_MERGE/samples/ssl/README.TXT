In order to generate a valid self-signed certificate to use https, follow these instructions:

- Download openssl and executable compatibile with INDY components from http://indy.fulgan.com/SSL/
- Unzip into a folder (e.g. C:\OpenSLLINDY)
- Within the created folder, run the following command
openssl.exe genrsa -out privkey.pem
openssl.exe req -new -x509 -config .\openssl.cfg -key privkey.pem -out cacert.pem -days 1095
[WARNING! READ THE OPENSSL DOCUMENTATION TO UNDERSTAND AND MODIFY THESE PARAMETERS]

- Put the libeay32.dll and ssleay32.dll into the Windows PATH or in the same folder of your executable
- Run the DMVCFramework ssl sample