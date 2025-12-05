# DMVCFramework - Local HTTPS Certificate Configuration

This document provides detailed instructions for generating and using local HTTPS certificates for development with the DMVCFramework.

## Prerequisites

1. **OpenSSL**: OpenSSL must be installed on the system.
   - Download the latest version from: [https://slproweb.com/products/Win32OpenSSL.html](https://slproweb.com/products/Win32OpenSSL.html)
   - For most users, the "Light" version is sufficient
   - Ensure OpenSSL is added to the system PATH during installation

2. **Administrator Privileges**: The script must be run with administrator privileges to install the CA certificate in the system store.

## Certificate Generation

### Step 1: Running the Script

1. Open Windows Command Prompt **as Administrator**
2. Navigate to the directory `$(DMVC)\tools\certificatesgenerator\` containing the `create-localhost-certificate.bat` script
3. Execute the script:
   ```cmd
   create-localhost-certificate.bat
   ```

### Step 2: Handling Existing CA

If a CA has already been generated previously, the script will ask:
```
Do you want to skip CA generation? [S]kip / [R]egenerate
```

- Press **S** to skip regeneration (recommended if you want to keep existing certificates compatible)
- Press **R** to regenerate the CA (necessary if the previous CA has been compromised or you want to create a new one)

### Step 3: CA Installation

The script will automatically install the Root CA in the Windows "Trusted Root Certification Authorities" store, making it trusted for all system browsers (Chrome, Edge, IE).

## Generated Files

After execution, the following files will be created in the script directory:

### Server Files (to deploy)
- `localhost.crt` - Server certificate (public)
- `localhost.key` - Server private key (**keep secure**)

### Certificate Authority Files
- `rootCA.crt` - Root CA certificate (public)
- `rootCA.key` - Root CA private key (**keep EXTREMELY secure**)

## DMVCFramework Server Configuration

### Files to Copy to Server

Copy the following files to your server configuration directory:

1. `localhost.crt` - Server certificate
2. `localhost.key` - Server private key

### Example Server Configuration

In case you want a fixed path for your certificates (not a configurable path)

In your DMVCFramework project `.dpr file`, configure the certificate paths:

```pascal
// HTTPS configuration example
lTaurusTLSHandler.DefaultCert.PublicKey := dotEnv.Env('https.cert.cacert', 'certificates\localhost.crt');
lTaurusTLSHandler.DefaultCert.PrivateKey := dotEnv.Env('https.cert.privkey', 'certificates\localhost.key');
```

If you want a more flexible configuration externalizing the certificates path, create a .env file (acording with your configuration) and set the following values.

```pascal
// HTTPS configuration example
https.enabled = true
https.cert.cacert = mycertificatespath\localhost.crt
https.cert.privkey = mycertificatespath\localhost.key
```

### 

### File Security

#### Files to Keep Secure (Never on production servers)
- `rootCA.key` - This key can be used to sign any certificate and represents a huge security risk if compromised

#### Public Files
- `rootCA.crt` - Can be freely distributed for manual configuration
- `localhost.crt` - Server certificate, public by definition

## Specific Browser Configuration

### Firefox
Firefox maintains its own certificate store:

1. Open Firefox
2. Go to `Settings` > `Privacy & Security` > `Certificates` > `View Certificates`
3. In the `Authorities` tab, click `Import`
4. Select the `rootCA.crt` file
5. Check "Trust this CA to identify websites"

## Certificate Usage

Once configured, the server will be accessible via HTTPS:

- `https://localhost:port`
- `https://127.0.0.1:port`

Modern browsers will automatically recognize the certificate as trusted thanks to the CA installation in the system store.

## Security Notes

1. **Keep the CA key secure**: The `rootCA.key` file must NEVER be distributed or uploaded to production servers
2. **Development only**: These certificates are intended exclusively for local development and should not be used in production environments
3. **Renewal**: Generated certificates have a validity of 365 days for the server and 3650 days for the CA

## Troubleshooting

### Certificate not recognized as trusted
- Verify the script was run as administrator
- Check in Windows "Trusted Root Certification Authorities" store that the "DMVCFramework Local Trusted CA" certificate is present

### OpenSSL Error
- Verify that OpenSSL is properly installed and in the PATH
- Ensure you're using a recent version of OpenSSL (3.x recommended)

### Server won't start
- Verify that the paths to `localhost.crt` and `localhost.key` files are correct in the server configuration
