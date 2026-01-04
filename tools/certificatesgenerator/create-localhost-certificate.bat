@echo off
:: =====================================================
:: Script: create-localhost-certificate.bat
:: Purpose: Automate creation of a trusted localhost HTTPS certificate
::          for the DMVCFramework project.
:: Requirements: OpenSSL installed, run as Administrator
:: Output: localhost.crt, localhost.key, rootCA.crt (trusted)
:: =====================================================

setlocal enabledelayedexpansion

:: --- Configuration ---
set "BASE_DIR=%~dp0"
set "OPENSSL_CONF=%BASE_DIR%openssl-minimal.cnf"
set "ROOT_CA_KEY=%BASE_DIR%rootCA.key"
set "ROOT_CA_CRT=%BASE_DIR%rootCA.crt"
set "LOCALHOST_CRT=%BASE_DIR%localhost.crt"
set "LOCALHOST_KEY=%BASE_DIR%localhost.key"
set "LOCALHOST_CSR=%BASE_DIR%localhost.csr"
set "MINI_CONF=%BASE_DIR%localhost-conf.cnf"

cls
echo =====================================================
echo  DMVCFramework Local HTTPS Certificate Generator
echo =====================================================
echo.

:: --- Check for Administrator privileges ---
net session >nul 2>&1
if %errorLevel% neq 0 (
    echo ERROR: This script requires Administrator privileges.
    echo Please run as Administrator.
    echo.
    pause
    exit /b 1
)

:: --- Create minimal OpenSSL config for CA ---
echo Generating minimal OpenSSL configuration for CA...
(
echo [req]
echo default_bits = 4096
echo prompt = no
echo default_md = sha256
echo distinguished_name = dn
echo x509_extensions = v3_ca
echo.
echo [dn]
echo C = IT
echo ST = Italy
echo L = Local
echo O = DMVCFramework CA
echo CN = DMVCFramework Local Trusted CA
echo.
echo [v3_ca]
echo basicConstraints = critical,CA:true
echo keyUsage = critical,keyCertSign,cRLSign
echo subjectKeyIdentifier = hash
) > "%OPENSSL_CONF%"

:: --- Generate Root CA (only if not exists) ---
if exist "%ROOT_CA_KEY%" (
    echo.
    echo The DMVCFramework CA private key already exists: %ROOT_CA_KEY%
    echo Do you want to skip CA generation? [S]kip / [R]egenerate
    choice /c SR /n
    if errorlevel 2 goto generate_root_ca
    if errorlevel 1 goto skip_root_ca
)

:generate_root_ca
echo.
echo Generating DMVCFramework Root CA private key (rootCA.key)...
openssl genrsa -out "%ROOT_CA_KEY%" 4096
if %errorlevel% neq 0 goto openssl_error

echo Generating self-signed DMVCFramework Root CA certificate (rootCA.crt)...
openssl req -x509 -new -nodes -key "%ROOT_CA_KEY%" -sha256 -days 3650 ^
    -out "%ROOT_CA_CRT%" -config "%OPENSSL_CONF%" ^
    -subj "/C=IT/ST=Italy/L=Local/O=DMVCFramework CA/CN=DMVCFramework Local Trusted CA"
if %errorlevel% neq 0 goto openssl_error

echo DMVCFramework Root CA created successfully.
:skip_root_ca

:: --- Create config for localhost certificate ---
echo.
echo Generating configuration for localhost certificate...
(
echo [req]
echo default_bits = 2048
echo prompt = no
echo default_md = sha256
echo distinguished_name = dn
echo req_extensions = v3_req
echo.
echo [dn]
echo C = IT
echo ST = Italy
echo L = Local
echo O = DMVCFramework
echo OU = Development
echo CN = localhost
echo.
echo [v3_req]
echo keyUsage = keyEncipherment, dataEncipherment, digitalSignature
echo extendedKeyUsage = serverAuth
echo subjectAltName = @alt_names
echo.
echo [alt_names]
echo DNS.1 = localhost
echo DNS.2 = ^127.0.0.1
echo IP.1 = 127.0.0.1
) > "%MINI_CONF%"

:: --- Generate localhost certificate ---
echo.
echo Generating private key for localhost (localhost.key)...
openssl genrsa -out "%LOCALHOST_KEY%" 2048
if %errorlevel% neq 0 goto openssl_error

echo Generating Certificate Signing Request (CSR) for localhost...
openssl req -new -key "%LOCALHOST_KEY%" -out "%LOCALHOST_CSR%" -config "%MINI_CONF%"
if %errorlevel% neq 0 goto openssl_error

echo Signing localhost certificate with your DMVCFramework CA...
openssl x509 -req -in "%LOCALHOST_CSR%" ^
    -CA "%ROOT_CA_CRT%" -CAkey "%ROOT_CA_KEY%" -CAcreateserial ^
    -out "%LOCALHOST_CRT%" -days 365 -sha256 -extfile "%MINI_CONF%" -extensions v3_req
if %errorlevel% neq 0 goto openssl_error

echo Successfully generated: %LOCALHOST_CRT%

:: --- Install CA into Windows Trusted Root Certification Authorities ---
echo.
echo Installing DMVCFramework CA certificate into Windows Trusted Root Store...
certutil -addstore -f "ROOT" "%ROOT_CA_CRT%"
if %errorlevel% equ 0 (
    echo OK: DMVCFramework CA certificate installed in "Trusted Root Certification Authorities".
) else (
    echo WARNING: Failed to install CA. Ensure 'certutil' is available and you are running as Admin.
)

:: --- Cleanup temporary config files ---
if exist "%OPENSSL_CONF%" del "%OPENSSL_CONF%"
if exist "%MINI_CONF%" del "%MINI_CONF%"

:: --- Final message ---
echo.
echo =====================================================
echo SUCCESS: DMVCFramework Local HTTPS Setup completed!
echo.
echo Certificate files generated:
echo =============================
echo   - localhost.key  : Server private key (KEEP SECURE)
echo   - localhost.crt  : Server certificate (PUBLIC)
echo   - rootCA.crt     : Root CA certificate (PUBLIC)
echo   - rootCA.key     : Root CA private key (KEEP SECURE)
echo.
echo Certificate usage:
echo =================
echo The certificate is valid for:
echo   - https://localhost
echo   - https://127.0.0.1
echo.
echo File deployment guidelines:
echo ==========================
echo SERVER FILES (deploy to your web server):
echo   - localhost.crt  : Server certificate
echo   - localhost.key  : Server private key
echo.
echo KEEP SECURE (store in a secure location, NEVER on production servers):
echo   - rootCA.key     : Root CA private key (used to sign other certificates)
echo.
echo PUBLIC FILES (can be distributed):
echo   - rootCA.crt     : Root CA certificate (needed for manual trust configuration)
echo.
echo For Firefox:
echo   Go to Settings ^> Privacy ^& Security ^> Certificates ^> View Certificates
echo   Import rootCA.crt under "Authorities" and check:
echo     "Trust this CA to identify websites"
echo.
echo The DMVCFramework Root CA is now trusted system-wide (Chrome, Edge, Windows).
echo =====================================================
echo.
pause
exit /b 0

:: --- Error handler ---
:openssl_error
echo.
echo ERROR: OpenSSL command failed.
echo Make sure OpenSSL is installed and added to your PATH.
echo Download from: https://slproweb.com/products/Win32OpenSSL.html  
echo.
pause
exit /b 1