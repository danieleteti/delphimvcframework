# Installation Guide

This guide provides detailed instructions for installing DelphiMVCFramework in your development environment.

## System Requirements

### Supported Delphi Versions

| Delphi Version | Package Group |
|---|---|
| **Delphi 13 Athens** | `packages\d130\dmvcframework_group.groupproj` |
| **Delphi 12 Athens** | `packages\d120\dmvcframework_group.groupproj` |
| **Delphi 11.3 Alexandria** | `packages\d113\dmvcframework_group.groupproj` |
| **Delphi 11 Alexandria** | `packages\d110\dmvcframework_group.groupproj` |
| **Delphi 10.4 Sydney** | `packages\d104\dmvcframework_group.groupproj` |
| **Delphi 10.3 Rio** | `packages\d103\dmvcframework_group.groupproj` |
| **Delphi 10.2 Tokyo** | `packages\d102\dmvcframework_group.groupproj` |
| **Delphi 10.1 Berlin** | `packages\d101\dmvcframework_group.groupproj` |
| **Delphi 10.0 Seattle** | `packages\d100\dmvcframework_group.groupproj` |

### Operating Systems
- Windows (32-bit and 64-bit)
- Linux (64-bit) - Delphi 10.2 Tokyo or newer
- Android (experimental)

## Installation Methods

### Method 1: Release Package (Recommended)

1. **Download the Latest Release**
   - Go to [GitHub Releases](https://github.com/danieleteti/delphimvcframework/releases/latest)
   - Download the main framework zip file
   - Download the samples zip file (optional but recommended)

2. **Extract Files**
   ```
   Extract to: C:\DMVC (or your preferred location)
   ```

3. **Open Package Group**
   - Launch RAD Studio
   - Open the appropriate package group for your Delphi version (see table above)

4. **Build and Install**
   - Right-click on the package group → **Build All**
   - Right-click on `dmvcframeworkDT` → **Install**

5. **Configure Library Paths**
   - Go to **Tools → Options → Environment Options → Delphi Options → Library**
   - Add these paths to **Library Path**:
     ```
     C:\DMVC\sources
     C:\DMVC\lib\loggerpro
     C:\DMVC\lib\swagdoc\Source
     C:\DMVC\lib\dmustache
     ```

### Method 2: Git Clone (For Contributors)

```bash
git clone https://github.com/danieleteti/delphimvcframework.git
cd delphimvcframework
```

Then follow steps 3-5 from Method 1.

## Verification

### Test Installation

1. **IDE Wizard**
   - Go to **File → New → Other**
   - Verify **Delphi Project → DMVC → DelphiMVCFramework Project** is available

2. **Create Test Project**
   - Use the wizard to create a new project
   - Try to compile (Ctrl+F9)
   - If successful, installation is complete

### Sample Projects

1. **Extract Samples** (if downloaded)
   ```
   Extract samples to: C:\DMVC\samples
   ```

2. **Test Sample Project**
   ```
   Open: C:\DMVC\samples\hello_world\HelloWorld.dproj
   Compile and run (F9)
   ```

## Dependencies

DelphiMVCFramework includes the following dependencies:

### Core Dependencies (Included)
- **LoggerPro** - High-performance logging framework
- **dmustache** - Mustache template engine for Delphi
- **SwagDoc** - Swagger/OpenAPI documentation generator

### Optional Dependencies
- **FireDAC** - For database connectivity (comes with Delphi)
- **Indy** - HTTP server implementation (comes with Delphi)

## Configuration Options

### Global Configuration

You can configure global settings in your main unit:

```pascal
uses
  MVCFramework.Commons;

// Global serialization settings
MVCSerializeNulls := True; // Serialize null values
```

### dotEnv Support

Create a `.env` file in your application directory:

```bash
# Database configuration
DB_HOST=localhost
DB_NAME=myapp_db
DB_USER=admin
DB_PASS=secret123

# Server settings
SERVER_PORT=8080
DEBUG_MODE=true
```

Load in your application:

```pascal
uses
  MVCFramework.DotEnv;

var
  dotEnv := NewDotEnv
    .WithStrategy(TMVCDotEnvPriority.EnvThenFile)
    .UseProfile('production')
    .Build();
```

## Deployment Options

### Console Application (Standalone)

```pascal
program MyServer;

{$APPTYPE CONSOLE}

uses
  MVCFramework,
  Web.WebBroker,
  IdHTTPWebBrokerBridge;

procedure RunServer;
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := 8080;
    LServer.Active := True;
    Writeln('Server started on port 8080');
    ReadLn;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  RunServer;
end.
```

### Windows Service

```pascal
// Use the DMVCFramework Service template from the wizard
// Or inherit from TMVCServiceBase
```

### Apache Module

```pascal
library MyApacheModule;

uses
  Web.WebBroker,
  Web.ApacheHTTP,
  MyWebModuleU in 'MyWebModuleU.pas' {MyWebModule: TWebModule};

{$R *.res}

// Apache module entry point
exports
  apache_module name 'dmvc_module';

begin
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end.
```

### IIS ISAPI

```pascal
library MyISAPI;

uses
  Web.WebBroker,
  Web.Win.ISAPIApp,
  MyWebModuleU in 'MyWebModuleU.pas' {MyWebModule: TWebModule};

{$R *.res}

// ISAPI entry point
exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end.
```

## Troubleshooting

### Common Issues

#### "Unit not found" errors
```
Solution: Verify all library paths are correctly configured
Check: Tools → Options → Library → Library Path
```

#### "dmvcframeworkDT.bpl" cannot be installed
```
Solution: Build the runtime package first
1. Build dmvcframework.bpl
2. Install dmvcframeworkDT.bpl
```

#### Access Violation on startup
```
Solution: Check for memory leaks
Enable: ReportMemoryLeaksOnShutdown := True;
```

#### Port already in use
```
Solution: Change the port or stop the conflicting service
Check: netstat -an | findstr :8080
```

### Debug Configuration

Enable debug information in your project:

```pascal
{$IFDEF DEBUG}
  // Enable debug logging
  LogI('Application starting in DEBUG mode');
  
  // Enable detailed error messages
  EMVCException.SetDetailedMessage(True);
{$ENDIF}
```

### Performance Optimization

For production environments:

```pascal
// Disable debug features
{$DEFINE RELEASE}
{$O+} // Enable optimizations
{$R-} // Disable range checking
{$Q-} // Disable overflow checking

// Configure thread pool
WebRequestHandlerProc.MaxConnections := 1024;
```

## Additional Resources

- **[Quick Start Guide](QUICKSTART.md)** - Get up and running quickly
- **[Samples](SAMPLES.md)** - Example projects and code
- **[FAQ](FAQ.md)** - Frequently asked questions
- **[Community Support](https://www.facebook.com/groups/delphimvcframework)** - Get help from the community

## Need Help?

If you encounter issues during installation:

1. **Check the FAQ** for common solutions
2. **Search existing issues** on GitHub
3. **Ask the community** in our Facebook group
4. **Create a new issue** on GitHub with detailed information

---

**Next Step:** Follow the [Quick Start Guide](QUICKSTART.md) to create your first DelphiMVCFramework application!