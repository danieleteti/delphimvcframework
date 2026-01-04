# Quick Start Guide

Get up and running with DelphiMVCFramework in just 5 minutes!

## Prerequisites

- RAD Studio (Delphi 10+ or newer)
- Basic knowledge of Object Pascal/Delphi

## Installation

1. **Download the latest release** from [GitHub Releases](https://github.com/danieleteti/delphimvcframework/releases/latest)
2. **Extract** the zip file to `C:\DMVC` (or your preferred location)
3. **Open** the appropriate package group in RAD Studio:
   - Delphi 13: `packages\d130\dmvcframework_group.groupproj`
   - Delphi 12: `packages\d120\dmvcframework_group.groupproj`
   - Delphi 11.3: `packages\d113\dmvcframework_group.groupproj`
   - (See [INSTALLATION.md](INSTALLATION.md) for all versions)
4. **Build and Install** the design-time package
5. **Add library paths** in Tools → Options → Language → Delphi → Library:
   - `C:\DMVC\sources`
   - `C:\DMVC\lib\loggerpro`
   - `C:\DMVC\lib\swagdoc\Source`
   - `C:\DMVC\lib\dmustache`

## Create Your First Project

### Using the IDE Wizard

1. Go to **File → New → Other**
1. Select **Delphi Project → DMVC → DelphiMVCFramework Project**
1. Configure your project settings
1. Click **OK**
1. **Compile** your project (Ctrl+F9)
1. **Run** your application (F9)
1. **Open your browser** and navigate to:
    - `http://localhost:8080/api` → Get a "Hello World" response

   - `http://localhost:8080/api/people` → Get a JSON response

## Next Steps

### Add Database Support with MVCActiveRecord

Check [ActiveRecord Middleware Sample](https://github.com/danieleteti/delphimvcframework/tree/master/samples/middleware_activerecord)

### Add Authentication

Check [JWT Sample](https://github.com/danieleteti/delphimvcframework/tree/master/samples/jsonwebtoken_livevaliditywindow)

### Explore Samples
All DMVCFramework features have an example in the [samples](https://github.com/danieleteti/delphimvcframework/tree/master/samples) folder.

## Useful Resources

- **[Samples](SAMPLES.md)** - 40+ examples covering all features
- **[Installation Guide](INSTALLATION.md)** - Detailed installation instructions
- **[Official Guide](http://www.danieleteti.it/books/)** - Comprehensive documentation
- **[Community](https://www.facebook.com/groups/delphimvcframework)** - Get help from 6000+ members

## Common Issues

**Q: I get "Unit not found" errors**
A: Make sure you've added all library paths correctly in Tools → Options

**Q: The server doesn't start**
A: Check if port 8080 is already in use, or try with a different port

**Q: JSON serialization doesn't work**
A: Ensure your classes have public properties (not just fields)

---

**🎉 Congratulations!** You now have a working DelphiMVCFramework server. Explore the [samples](https://github.com/danieleteti/delphimvcframework/tree/master/samples) and buy the [Official Guide](https://leanpub.com/delphimvcframework) to learn about advanced features!