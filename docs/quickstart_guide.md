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
2. Select **Delphi Project → DMVC → DelphiMVCFramework Project**
3. Configure your project settings
4. Click **OK**

### Manual Setup

Create a new console application and modify it:

```pascal
program MyFirstMVCApp;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.DotEnv,
  ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  MyControllerU in 'MyControllerU.pas',
  MyWebModuleU in 'MyWebModuleU.pas' {MyWebModule: TWebModule};

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln('** DelphiMVCFramework Server **');
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.Active := True;
    Writeln('Server running on port ', APort);
    Writeln('Press RETURN to stop');
    ReadLn;
  finally
    LServer.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    RunServer(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
```

## Create Your First Controller

Create `MyControllerU.pas`:

```pascal
unit MyControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  System.Classes;

type
  [MVCPath('/api')]
  TMyController = class(TMVCController)
  public
    [MVCPath('/hello')]
    [MVCHTTPMethod([httpGET])]
    procedure HelloWorld;

    [MVCPath('/hello/($name)')]
    [MVCHTTPMethod([httpGET])]
    procedure HelloName(name: string);

    [MVCPath('/users')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    procedure GetUsers;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TUser = class
  private
    FName: string;
    FEmail: string;
  public
    constructor Create(const AName, AEmail: string);
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
  end;

{ TMyController }

procedure TMyController.HelloWorld;
begin
  Render('Hello World!');
end;

procedure TMyController.HelloName(name: string);
begin
  Render('Hello ' + name + '!');
end;

procedure TMyController.GetUsers;
var
  LUsers: TObjectList<TUser>;
begin
  LUsers := TObjectList<TUser>.Create;
  try
    LUsers.Add(TUser.Create('John Doe', 'john@example.com'));
    LUsers.Add(TUser.Create('Jane Smith', 'jane@example.com'));
    Render(LUsers);
  finally
    LUsers.Free;
  end;
end;

{ TUser }

constructor TUser.Create(const AName, AEmail: string);
begin
  inherited Create;
  FName := AName;
  FEmail := AEmail;
end;

end.
```

## Create the WebModule

Create `MyWebModuleU.pas`:

```pascal
unit MyWebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TMyWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TMyWebModule;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  MVCFramework.Commons,
  MVCFramework.Middleware.CORS,
  MyControllerU;

procedure TMyWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // Session timeout (0 means session cookie)
      Config[TMVCConfigKey.SessionTimeout] := '0';
      // Default content-type
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      // Default content charset
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      // Unhandled actions are permitted?
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      // Default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      // View path
      Config[TMVCConfigKey.ViewPath] := 'templates';
      // Max Record Count for automatic Entities CRUD
      Config[TMVCConfigKey.MaxEntitiesRecordCount] := '20';
      // Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
    end);
  FMVC.AddController(TMyController);
  FMVC.AddMiddleware(TMVCCORSMiddleware.Create);
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
```

## Run Your Server

1. **Compile** your project (Ctrl+F9)
2. **Run** your application (F9)
3. **Open your browser** and navigate to:
   - `http://localhost:8080/api/hello` → "Hello World!"
   - `http://localhost:8080/api/hello/YourName` → "Hello YourName!"
   - `http://localhost:8080/api/users` → JSON array of users

## Next Steps

### Add Database Support with MVCActiveRecord

```pascal
uses
  MVCFramework.ActiveRecord,
  FireDAC.Comp.Client;

type
  [MVCNameCase(ncLowerCase)]
  [MVCTable('customers')]
  TCustomer = class(TMVCActiveRecord)
  private
    [MVCTableField('id', [foPrimaryKey, foAutoGenerated])]
    fID: Integer;
    [MVCTableField('company_name')]
    fCompanyName: string;
    [MVCTableField('city')]
    fCity: string;
  public
    property ID: Integer read fID write fID;
    property CompanyName: string read fCompanyName write fCompanyName;
    property City: string read fCity write fCity;
  end;

// In your controller
procedure TMyController.GetCustomers;
var
  LCustomers: TObjectList<TCustomer>;
begin
  LCustomers := TMVCActiveRecord.SelectRQL<TCustomer>('sort(+company_name)', 50);
  try
    Render(LCustomers);
  finally
    LCustomers.Free;
  end;
end;
```

### Add Authentication

```pascal
uses
  MVCFramework.Middleware.Authentication,
  MVCFramework.Middleware.JWT;

// In WebModule
FMVC.AddMiddleware(TMVCJWTAuthenticationMiddleware.Create(
  TMVCDefaultAuthenticationHandler.New
    .SetOnAuthentication(
      procedure(const AUserName, APassword: string; AUserRoles: TList<string>; 
        var IsValid: Boolean; const ASessionData: TDictionary<String, String>)
      begin
        IsValid := (AUserName = 'admin') and (APassword = 'secret');
        if IsValid then
          AUserRoles.Add('admin');
      end)
    .SetOnAuthorization(
      procedure(AUserRoles: TList<string>; const AControllerQualifiedClassName: string; 
        const AActionName: string; var IsAuthorized: Boolean)
      begin
        IsAuthorized := AUserRoles.Contains('admin');
      end)));
```

### Add Swagger Documentation

```pascal
uses
  MVCFramework.Swagger.Commons;

// Decorate your controller methods
[MVCDoc('Returns a greeting message')]
[MVCSwagSummary('Hello World', 'Simple greeting endpoint')]
[MVCSwagResponses(200, 'Success', 'string')]
procedure TMyController.HelloWorld;
```

## Useful Resources

- **[Samples](SAMPLES.md)** - 40+ examples covering all features
- **[Installation Guide](INSTALLATION.md)** - Detailed installation instructions
- **[Official Guide](http://www.danieleteti.it/books/)** - Comprehensive documentation
- **[Community](https://www.facebook.com/groups/delphimvcframework)** - Get help from 6000+ members

## Common Issues

**Q: I get "Unit not found" errors**
A: Make sure you've added all library paths correctly in Tools → Options

**Q: The server doesn't start**
A: Check if port 8080 is already in use, or change to a different port

**Q: JSON serialization doesn't work**
A: Ensure your classes have public properties (not just fields)

---

**🎉 Congratulations!** You now have a working DelphiMVCFramework server. Explore the [samples](SAMPLES.md) to learn about advanced features!