@echo off

set APACHEPATH=D:\Tools\Apache24

IF EXIST %APACHEPATH% (
echo Using Apache at path %APACHEPATH%
%APACHEPATH%\bin\openssl genrsa -out privkey.pem 4096
%APACHEPATH%\bin\openssl.exe req -new -x509 -config %APACHEPATH%\conf\openssl.cnf -key privkey.pem -out cacert.pem -days 1095
) ELSE (
echo Please set APACHEPATH to a valid Apache httpd distribution (download it from https://www.apachelounge.com/download/)
)

