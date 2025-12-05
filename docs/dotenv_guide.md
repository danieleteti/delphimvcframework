# dotEnv Configuration Guide

Since version 3.4.0-neon, DelphiMVCFramework supports dotEnv configuration files for managing application settings.

## Overview

dotEnv provides a simple way to manage configuration through plain text files by:
- Loading configuration from .env files into an internal dictionary
- Mixing file-based configuration with system environment variables based on strategy
- Providing a unified API to access configuration values
- Separating configuration from source code
- Keeping sensitive information out of version control

**Critical Concept**: dotEnv does NOT modify system environment variables. It maintains its own internal configuration dictionary accessible through `dotEnv.Env()` methods, completely separate from `GetEnvironmentVariable()`.

## File Format and Syntax

### Basic Key-Value Pairs

```bash
# Basic assignment (spaces around = are allowed)
db_host = myserver
db_port = 5432
dbname=my_product_db_dev

# Values are read to end of line
dbpassword=XYZdteti!$
```

### Comments

```bash
# This is a comment
db_host = myserver  # inline comments supported

#############################################
# Section headers using comments
#############################################
```

### Variable Names Rules

Variable names can:
- Start with: **letters (a-z)**, **dots (.)**, **underscores (_)**
- Contain: **letters**, **numbers (0-9)**, **dots (.)**, **underscores (_)**

```bash
# Valid variable names
valid.varname = OK
.also.valid = OK
valid_name = OK
.123 = OK
_.123 = OK

# Invalid variable names (will cause parsing errors)
# 3notvalid = KO        # cannot start with number
# name*invalid = KO     # asterisks not allowed
# name@invalid = KO     # special chars not allowed
```

### String Values

```bash
# Simple values (no quotes needed for single words)
mode=dev
dbuser=my_user

# Values with spaces (no quotes needed - value extends to end of line)
value_with_spaces = Value with spaces
user_preferences_path = C:\Users\dteti\AppData\Roaming

# Multi-line string values (requires quotes)
email_template="This is a dev email template
second template email line
third template email line"
```

### Variable Expansion

Reference other variables using `${variable_name}` syntax:

```bash
# Define base variables first
db_host = myserver
db_port = 5432

# Reference them in other variables
db_url = pg://${db_host}:${db_port}

# Complex paths
base_path = /opt/myapp
log_path = ${base_path}/logs
config_path = ${base_path}/config
```

### Built-in Variables

DelphiMVCFramework provides special built-in variables with `__name__` syntax:

```bash
# Built-in variables in .env files
app_title = ${__os__} Application v${__dmvc.version__}
data_path = ${__home__}/data
log_message = Running on ${__os__} with DMVCFramework ${__dmvc.version__}
```

| Variable | Description | Example Value |
|----------|-------------|---------------|
| `__os__` | Operating system name | "Windows 10", "Linux" |
| `__home__` | Application directory path | "C:\MyApp\" |
| `__dmvc.version__` | DMVCFramework version | "3.4.2-magnesium" |

## Complete Example File

```bash
#############################################
# Sample .env file for DelphiMVCFramework
#############################################

# Database Configuration
db_host = myserver
db_port = 5432
db_url = pg://${db_host}:${db_port}
dbhostname = 127.0.0.1
dbname = my_product_db_dev
dbpassword = XYZdteti!$
dbuser = my_user

# Application Settings
mode = dev
value_with_spaces = Value with spaces

# Multi-line configuration
email_template="This is a dev email template
second template email line
third template email line"

# Path Configuration
user_preferences_path = C:\Users\dteti\AppData\Roaming

# Variable names with allowed special characters
valid.varname = OK
.also.valid = OK
valid_name__ = OK
.123 = OK
_.123 = OK

#############################################
# DelphiMVCFramework Specific Configuration
#############################################

# Logging configuration
# loglevel: debug, normal (default), warning, error, exception
loglevel = error

# Profiler settings
dmvc.profiler.enabled = false
dmvc.profiler.warning_threshold = 3000

#############################################
# Email Configuration
#############################################
email.sender = peter.parker@bittime.com
email.smtpserver = mail.bittime.com
email.smtpserverport = 25

#############################################
# FireDAC Configuration
#############################################
# Enable or disable tracing
firedac.trace = true

# Trace file configuration
firedac.trace_filename = firedaclog.log

# Append or replace trace file contents
firedac.trace_file_append = false
```

## Loading dotEnv Files

### Basic Usage

```pascal
uses
  MVCFramework.DotEnv;

var
  dotEnv: IMVCDotEnv;
begin
  // Load and parse .env files into internal dictionary
  dotEnv := NewDotEnv.UseStrategy(TMVCDotEnvPriority.EnvThenFile).Build();
  
  // Access configuration through dotEnv.Env() - NOT GetEnvironmentVariable()
  WriteLn('Database Host: ' + dotEnv.Env('db_host'));
  WriteLn('Database URL: ' + dotEnv.Env('db_url'));
  WriteLn('Debug Mode: ' + BoolToStr(dotEnv.Env('debug_mode', False)));
  
  // System environment variables are accessed separately
  WriteLn('System PATH: ' + GetEnvironmentVariable('PATH'));
  WriteLn('System User: ' + GetEnvironmentVariable('USERNAME'));
end;
```

### Advanced Configuration Methods

```pascal
var
  dotEnv: IMVCDotEnv;
begin
  dotEnv := NewDotEnv
    .UseStrategy(TMVCDotEnvPriority.EnvThenFile)    // Set priority strategy
    .UseProfile('production')                       // Load .env.production
    .UseProfile(function: String begin Result := 'dynamic'; end) // Dynamic profile
    .SkipDefaultEnv                                // Skip default .env file
    .UseLogger(procedure(const Msg: String) begin WriteLn('[dotEnv] ' + Msg); end) // Custom logger
    .ClearProfiles                                 // Clear all profiles if needed
    .Build('Config');                              // Custom subdirectory
    
  // Validate required keys exist
  dotEnv.RequireKeys(['db_host', 'db_user', 'db_password']);
end;
```

## Core API Methods

### Strategy Options

```pascal
TMVCDotEnvPriority = (FileThenEnv, EnvThenFile, OnlyFile, OnlyEnv);
```

| Strategy | Behavior |
|----------|----------|
| `FileThenEnv` | File values override environment variables |
| `EnvThenFile` | Environment variables override file values |
| `OnlyFile` | Only load from .env files, ignore environment |
| `OnlyEnv` | Only use existing environment variables |

### Builder Methods

#### `UseStrategy(TMVCDotEnvPriority)`
Sets the priority strategy for resolving configuration values:

```pascal
// Environment variables take precedence over file values
dotEnv := NewDotEnv.UseStrategy(TMVCDotEnvPriority.EnvThenFile).Build();

// File values take precedence over environment variables  
dotEnv := NewDotEnv.UseStrategy(TMVCDotEnvPriority.FileThenEnv).Build();

// Only use .env files, ignore system environment
dotEnv := NewDotEnv.UseStrategy(TMVCDotEnvPriority.OnlyFile).Build();

// Only use system environment, ignore .env files
dotEnv := NewDotEnv.UseStrategy(TMVCDotEnvPriority.OnlyEnv).Build();
```

#### `UseProfile(string)` and `UseProfile(TFunc<String>)`
Load profile-specific configuration files:

```pascal
// Static profile name
dotEnv := NewDotEnv.UseProfile('production').Build();
// Loads: .env, then .env.production

// Dynamic profile using delegate
dotEnv := NewDotEnv
  .UseProfile(function: String 
              begin 
                // Use system environment to determine profile
                Result := GetEnvironmentVariable('APP_PROFILE'); 
                if Result.IsEmpty then Result := 'development';
              end)
  .Build();
```

#### `SkipDefaultEnv`
Skip loading the default `.env` file, only load profile-specific files:

```pascal
// Only loads .env.prod, skips default .env entirely
dotEnv := NewDotEnv
  .SkipDefaultEnv
  .UseProfile('prod')
  .Build();
```

#### `UseLogger(TProc<String>)`
Set custom logger for dotEnv operations:

```pascal
dotEnv := NewDotEnv
  .UseLogger(procedure(const Msg: String) 
             begin 
               WriteLn('[dotEnv] ' + Msg); 
             end)
  .Build();
// Outputs: [dotEnv] Path = C:\MyApp
// Outputs: [dotEnv] Applied file C:\MyApp\.env
```

#### `ClearProfiles`
Remove all previously set profiles:

```pascal
dotEnv := NewDotEnv
  .UseProfile('dev')
  .UseProfile('test')
  .ClearProfiles          // Removes both 'dev' and 'test'
  .UseProfile('prod')     // Now only 'prod' is active
  .Build();
```

#### `Build()` and `Build(DotEnvPath)`
Build the dotEnv instance, optionally specifying subdirectory:

```pascal
// Load from executable directory
dotEnv := NewDotEnv.Build();

// Load from subdirectory relative to executable
dotEnv := NewDotEnv.Build('Config');  // Looks in .\Config\ subdirectory
```

### Configuration Access Methods

#### Type-Safe `Env` Methods
Access configuration values with automatic type conversion and validation:

```pascal
var dotEnv := NewDotEnv.Build();

// String values
var dbHost := dotEnv.Env('db_host');                    // Returns string, empty if not found
var dbHost := dotEnv.Env('db_host', 'localhost');       // Returns string with default

// Integer values with validation
var dbPort := dotEnv.Env('db_port', 5432);              // Returns Integer, validates format
// Raises EMVCDotEnv if value exists but is not valid integer

// Boolean values with flexible parsing
var debugMode := dotEnv.Env('debug_mode', False);       // Returns Boolean
// Accepts: true/false, yes/no, 1/0 (case-insensitive)

// Double values with US format parsing
var timeout := dotEnv.Env('timeout', 30.5);             // Returns Double
// Uses en-US format for decimal parsing
```

#### `RequireKeys(TArray<String>)`
Validate that essential configuration keys are present:

```pascal
var dotEnv := NewDotEnv.Build();

try
  // Validate required configuration exists
  dotEnv.RequireKeys(['db_host', 'db_user', 'db_password']);
  WriteLn('Configuration validation passed');
except
  on E: EMVCDotEnv do
    WriteLn('Missing configuration: ' + E.Message);
    // Example: "Required keys not found: db_password, api_key"
end;
```

#### `ToArray`
Get all loaded configuration as sorted key=value pairs:

```pascal
var dotEnv := NewDotEnv.Build();
var AllVars := dotEnv.ToArray;

// Returns sorted array like:
// ['db_host=localhost', 'db_port=5432', 'debug_mode=true', ...]
for var EnvVar in AllVars do
  WriteLn('Config: ' + EnvVar);
```

#### `Rebuild`
Reload all .env files from disk:

```pascal
var dotEnv := NewDotEnv.Build();

// Initial configuration
WriteLn('DB Host: ' + dotEnv.Env('db_host'));

// ... .env files modified on disk ...

// Reload configuration from files
dotEnv.Rebuild();

// Updated configuration
WriteLn('DB Host: ' + dotEnv.Env('db_host')); // Reflects new values
```

#### `SaveToFile(FileName)`
Export current configuration to a file:

```pascal
var dotEnv := NewDotEnv.Build();
dotEnv.SaveToFile('exported-config.env');
// Creates sorted .env file with all current variables
```

## Configuration vs System Environment Variables

**Critical Understanding**: DelphiMVCFramework dotEnv maintains its own configuration dictionary completely separate from system environment variables.

### Two Independent Systems

```pascal
var dotEnv := NewDotEnv.Build();

// 1. System Environment Variables (OS level) - NEVER modified by dotEnv
var SystemPath := GetEnvironmentVariable('PATH');        // Always from OS
var SystemUser := GetEnvironmentVariable('USERNAME');    // Always from OS
var SystemTemp := GetEnvironmentVariable('TEMP');        // Always from OS

// 2. dotEnv Configuration (internal dictionary from .env files)  
var DbHost := dotEnv.Env('db_host');                    // From .env files
var DbPort := dotEnv.Env('db_port', 5432);             // From .env files with default
var AppMode := dotEnv.Env('mode', 'production');       // From .env files with default

// 3. Mixed Access (strategy determines priority)
var MixedValue := dotEnv.Env('HOME');                   // Strategy determines source
```

### Strategy Behavior Examples

```pascal
// .env file contains: 
// custom_setting=file_value
// PATH=/custom/path

// System environment contains:
// custom_setting=system_value  
// PATH=C:\Windows\System32

var dotEnv := NewDotEnv.UseStrategy(TMVCDotEnvPriority.FileThenEnv).Build();

// Strategy affects dotEnv.Env() behavior only:
var Setting1 := dotEnv.Env('custom_setting');  // Returns "file_value" (file wins)
var Setting2 := dotEnv.Env('PATH');             // Returns "/custom/path" (file wins)

// System environment completely unchanged:
var SystemPath := GetEnvironmentVariable('PATH');         // Still "C:\Windows\System32"
var SystemSetting := GetEnvironmentVariable('custom_setting'); // Still "system_value"
```

## Integration with DelphiMVCFramework

### WebModule Configuration

```pascal
procedure TMyWebModule.WebModuleCreate(Sender: TObject);
var
  dotEnv: IMVCDotEnv;
  ProfileName: string;
begin
  // Determine profile from system environment
  ProfileName := GetEnvironmentVariable('APP_ENV');
  if ProfileName.IsEmpty then ProfileName := 'development';
  
  // Load configuration from .env files
  dotEnv := NewDotEnv
    .UseStrategy(TMVCDotEnvPriority.EnvThenFile)
    .UseProfile(ProfileName)
    .Build();

  FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // Use dotEnv for application configuration
      Config[TMVCConfigKey.SessionTimeout] := dotEnv.Env('session_timeout', '30');
      Config[TMVCConfigKey.DefaultContentType] := dotEnv.Env('default_content_type', 'application/json');
      Config['LogLevel'] := dotEnv.Env('loglevel', 'normal');
      
      // Use system environment for OS-specific settings
      Config['TempDirectory'] := GetEnvironmentVariable('TEMP');
    end);
end;
```

### Database Configuration with FireDAC

```pascal
procedure ConfigureDatabase(dotEnv: IMVCDotEnv);
var
  Connection: TFDConnection;
begin
  Connection := TFDConnection.Create(nil);
  try
    // Use dotEnv.Env() for application configuration
    Connection.Params.Values['DriverID'] := dotEnv.Env('db_driver', 'PG');
    Connection.Params.Values['Server'] := dotEnv.Env('db_host', 'localhost');
    Connection.Params.Values['Port'] := dotEnv.Env('db_port', '5432');
    Connection.Params.Values['Database'] := dotEnv.Env('dbname');
    Connection.Params.Values['User_Name'] := dotEnv.Env('dbuser');
    Connection.Params.Values['Password'] := dotEnv.Env('dbpassword');
    
    // FireDAC settings from dotEnv
    Connection.Params.Values['Trace'] := BoolToStr(dotEnv.Env('firedac.trace', False));
    if dotEnv.Env('firedac.trace', False) then
      Connection.Params.Values['TraceFile'] := dotEnv.Env('firedac.trace_filename', 'trace.log');
    
    Connection.Connected := True;
  finally
    Connection.Free;
  end;
end;
```

### ActiveRecord Configuration

```pascal
uses
  MVCFramework.ActiveRecord,
  MVCFramework.DotEnv;

procedure ConfigureActiveRecord;
var
  dotEnv: IMVCDotEnv;
begin
  dotEnv := NewDotEnv.UseProfile('database').Build();
  
  // Configure connection using dotEnv configuration
  ActiveRecordConnectionRegistry.AddConnection('default',
    procedure(Conn: TFDConnection)
    begin
      Conn.Params.Values['DriverID'] := dotEnv.Env('db_driver', 'PG');
      Conn.Params.Values['Server'] := dotEnv.Env('db_host', 'localhost');
      Conn.Params.Values['Database'] := dotEnv.Env('dbname');
      Conn.Params.Values['User_Name'] := dotEnv.Env('dbuser');
      Conn.Params.Values['Password'] := dotEnv.Env('dbpassword');
    end);
end;
```

### Profiler Configuration

```pascal
uses
  MVCFramework.Commons;

procedure ConfigureProfiler;
var
  dotEnv: IMVCDotEnv;
begin
  dotEnv := NewDotEnv.Build();
  
  // Configure profiler using dotEnv configuration
  if dotEnv.Env('dmvc.profiler.enabled', False) then
  begin
    Profiler.WarningThreshold := dotEnv.Env('dmvc.profiler.warning_threshold', 1000);
  end;
end;
```

## Configuration Validation and Reloading

### RequireKeys - Configuration Validation

Validate essential configuration keys and raise `EMVCDotEnv` exception if any are missing:

```pascal
procedure ValidateApplicationConfig;
var
  dotEnv: IMVCDotEnv;
begin
  dotEnv := NewDotEnv
    .UseStrategy(TMVCDotEnvPriority.EnvThenFile)
    .Build();

  try
    // Critical application settings
    dotEnv.RequireKeys([
      'db_host', 'db_user', 'db_password',    // Database
      'jwt_secret', 'api_key',                // Security  
      'smtp_server', 'smtp_user'              // Email
    ]);
    
    // Optional: validate specific profiles
    if dotEnv.Env('mode') = 'production' then
      dotEnv.RequireKeys(['ssl_cert', 'ssl_key']);
      
  except
    on E: EMVCDotEnv do
    begin
      WriteLn('FATAL: Missing required configuration');
      WriteLn('Error: ' + E.Message);
      WriteLn('Please check your .env files');
      Halt(1); // Exit application
    end;
  end;
end;
```

### Rebuild - Dynamic Configuration Reloading

Reload all .env files from disk without restarting the application:

```pascal
procedure ConfigurationReloadExample;
var
  dotEnv: IMVCDotEnv;
begin
  dotEnv := NewDotEnv
    .UseLogger(procedure(const Msg: String) begin WriteLn('[Config] ' + Msg); end)
    .Build();

  // Initial configuration
  WriteLn('Initial DB Host: ' + dotEnv.Env('db_host'));

  // Simulate configuration file changes...
  WriteLn('Modifying configuration files...');
  Sleep(2000);

  try
    // Reload configuration from disk
    dotEnv.Rebuild();
    WriteLn('Configuration reloaded successfully');
    
    // Updated configuration
    WriteLn('Updated DB Host: ' + dotEnv.Env('db_host'));
    
    // Re-validate after reload
    dotEnv.RequireKeys(['db_host', 'db_user']);
    
  except
    on E: EMVCDotEnv do
      WriteLn('Configuration reload failed: ' + E.Message);
  end;
end;
```

## Error Handling and Validation

### Configuration Loop Detection

```pascal
// This .env configuration will cause an error:
// db_host = ${db_host}  // Circular reference

var dotEnv := NewDotEnv.Build();
try
  var Host := dotEnv.Env('db_host');
except
  on E: EMVCDotEnv do
    WriteLn('Error: ' + E.Message); // "Configuration loop detected with key "db_host""
end;
```

### Type Validation Errors

```pascal
// .env file contains: db_port=invalid_number

var dotEnv := NewDotEnv.Build();
try
  var Port := dotEnv.Env('db_port', 5432); // Try to parse as Integer
except
  on E: EMVCDotEnv do
    // Error: 'Env "db_port" is not a valid integer [Current Value: "invalid_number"]'
    WriteLn(E.Message);
end;
```

### Boolean Value Parsing

Boolean values are parsed flexibly:

```bash
# Valid boolean values in .env
debug_mode_1 = true      # Standard boolean
debug_mode_2 = yes       # Human readable
debug_mode_3 = 1         # Numeric
debug_mode_4 = false     # Standard boolean
debug_mode_5 = no        # Human readable  
debug_mode_6 = 0         # Numeric
```

```pascal
var dotEnv := NewDotEnv.Build();

// All return appropriate boolean values
var Debug1 := dotEnv.Env('debug_mode_1', False); // True
var Debug2 := dotEnv.Env('debug_mode_2', False); // True  
var Debug3 := dotEnv.Env('debug_mode_3', False); // True
var Debug4 := dotEnv.Env('debug_mode_4', True);  // False
var Debug5 := dotEnv.Env('debug_mode_5', True);  // False
var Debug6 := dotEnv.Env('debug_mode_6', True);  // False
```

## Practical Usage Examples

### Application Startup Configuration
```pascal
procedure InitializeApplication;
var
  dotEnv: IMVCDotEnv;
  AppProfile: string;
begin
  // Determine profile from system environment
  AppProfile := GetEnvironmentVariable('APP_PROFILE');
  if AppProfile.IsEmpty then AppProfile := 'development';
  
  // Load configuration
  dotEnv := NewDotEnv
    .UseStrategy(TMVCDotEnvPriority.EnvThenFile)
    .UseProfile(AppProfile)
    .Build();
  
  // Validate essential configuration
  dotEnv.RequireKeys(['db_host', 'db_user', 'db_password']);
  
  // Configure application components
  ConfigureDatabase(dotEnv);
  ConfigureLogging(dotEnv);
  ConfigureProfiler(dotEnv);
end;
```

### Production vs Development Configuration
```pascal
procedure ConfigureForEnvironment;
var
  dotEnv: IMVCDotEnv;
  IsProduction: Boolean;
begin
  dotEnv := NewDotEnv.Build();
  IsProduction := SameText(dotEnv.Env('mode'), 'production');
  
  if IsProduction then
  begin
    // Production: strict validation, security-first
    dotEnv.RequireKeys([
      'db_host', 'db_user', 'db_password', 'db_name',
      'jwt_secret', 'api_key', 'ssl_cert', 'ssl_key'
    ]);
    
    // Use environment-first strategy for production
    dotEnv := NewDotEnv
      .UseStrategy(TMVCDotEnvPriority.EnvThenFile)
      .UseProfile('production')
      .Build();
  end
  else
  begin
    // Development: file-first, more flexible
    dotEnv := NewDotEnv
      .UseStrategy(TMVCDotEnvPriority.FileThenEnv)
      .UseProfile('development')
      .UseLogger(procedure(const Msg: String) begin WriteLn('[Dev] ' + Msg); end)
      .Build();
  end;
end;
```

## Best Practices

### File Organization and Security

```
project/
├── .env                    # Base configuration (committed with defaults)
├── .env.development        # Development settings (committed)
├── .env.testing           # Testing settings (committed)  
├── .env.staging           # Staging settings (DO NOT COMMIT)
├── .env.production        # Production settings (DO NOT COMMIT)
├── .env.local             # Local overrides (DO NOT COMMIT)
└── .env.example           # Template file (committed)
```

**.gitignore**:
```
# Environment files with sensitive data
.env.local
.env.production
.env.staging
.env.*.local

# Keep template and development files
!.env.example
!.env.development
!.env.testing
```

### Variable Naming Conventions

```bash
# Use consistent naming patterns
# Prefer lowercase with underscores or dots
db_host = localhost
db.connection.timeout = 30

# Group related settings with prefixes
email.sender = user@domain.com
email.smtp.server = smtp.domain.com
email.smtp.port = 587

# Use descriptive names
dmvc.profiler.enabled = true
dmvc.profiler.warning_threshold = 1000
```

### Documentation Template

**.env.example**:
```bash
#############################################
# DelphiMVCFramework Configuration Template
# Copy this file to .env and customize
#############################################

# Database Configuration
# PostgreSQL connection settings
db_host=localhost
db_port=5432
dbname=your_app_name
dbuser=your_db_user
dbpassword=your_secure_password

# Application Mode
# Values: development, testing, staging, production
mode=development

# Logging Level  
# Values: debug, normal, warning, error, exception
loglevel=normal

# Profiler Settings
# Enable performance profiling (true/false)
dmvc.profiler.enabled=true
# Warning threshold in milliseconds
dmvc.profiler.warning_threshold=1000

# Email Configuration
email.sender=noreply@yourcompany.com
email.smtpserver=smtp.yourcompany.com
email.smtpserverport=587

# Security
# JWT secret key for token signing
jwt.secret=your_jwt_secret_key_here
# JWT expiration time in seconds
jwt.expiration=3600

# FireDAC Settings
# Enable connection tracing (true/false)
firedac.trace=false
firedac.trace_filename=firedac.log
```

## API Reference

### Core Methods

| Method | Description | Example |
|--------|-------------|---------|
| `NewDotEnv` | Create new dotEnv builder | `NewDotEnv.Build()` |
| `UseStrategy(TMVCDotEnvPriority)` | Set loading strategy | `.UseStrategy(TMVCDotEnvPriority.EnvThenFile)` |
| `UseProfile(string)` | Load specific profile | `.UseProfile('prod')` |
| `UseProfile(TFunc<String>)` | Dynamic profile resolution | `.UseProfile(function: String begin Result := 'prod'; end)` |
| `SkipDefaultEnv` | Skip default .env file | `.SkipDefaultEnv` |
| `UseLogger(TProc<String>)` | Set custom logger | `.UseLogger(procedure(s: string) begin WriteLn(s); end)` |
| `ClearProfiles` | Remove all profiles | `.ClearProfiles` |
| `Build()` | Build in executable directory | `.Build()` |
| `Build(DotEnvPath)` | Build in subdirectory | `.Build('Config')` |
| `Env(string)` | Get string value | `dotEnv.Env('db_host')` |
| `Env(string, string)` | Get string with default | `dotEnv.Env('db_host', 'localhost')` |
| `Env(string, Integer)` | Get integer with validation | `dotEnv.Env('db_port', 5432)` |
| `Env(string, Boolean)` | Get boolean with flexible parsing | `dotEnv.Env('debug', false)` |
| `Env(string, Double)` | Get double with US format | `dotEnv.Env('timeout', 30.5)` |
| `RequireKeys(TArray<String>)` | Validate required keys | `dotEnv.RequireKeys(['db_host'])` |
| `ToArray` | Get all variables as key=value | `dotEnv.ToArray` |
| `Rebuild` | Rebuild configuration | `dotEnv.Rebuild` |
| `SaveToFile(string)` | Export to file | `dotEnv.SaveToFile('config.env')` |

### Strategy Options

| Strategy | Behavior |
|----------|----------|
| `FileThenEnv` | File values override environment variables |
| `EnvThenFile` | Environment variables override file values |
| `OnlyFile` | Only load from .env files, ignore environment |
| `OnlyEnv` | Only use existing environment variables |

### Built-in Variables

| Variable | Value | Description |
|----------|-------|-------------|
| `__os__` | OS name | Operating system name |
| `__home__` | Application path | Executable directory path |
| `__dmvc.version__` | Framework version | DMVCFramework version string |

### Exception Types

| Exception | When Raised |
|-----------|-------------|
| `EMVCDotEnv` | Configuration errors, type validation failures, missing required keys, circular references |

## Troubleshooting

### Common Syntax Errors

**Invalid variable name**:
```bash
# Wrong - starts with number
3invalid = value

# Wrong - contains invalid characters
name*invalid = value
name@invalid = value

# Correct - valid characters only
valid3 = value
.valid = value
_valid = value
```

**Multi-line strings without quotes**:
```bash
# Wrong - only captures first line
template=Line 1
Line 2

# Correct - use quotes for multi-line
template="Line 1
Line 2"
```

**Variable expansion errors**:
```bash
# Wrong - undefined variable reference
db_url=pg://${undefined_host}:5432

# Correct - define variables before referencing
db_host=localhost
db_url=pg://${db_host}:5432
```

### Debugging Configuration

```pascal
procedure DebugConfiguration;
var
  dotEnv: IMVCDotEnv;
  ConfigVars: TArray<string>;
begin
  try
    dotEnv := NewDotEnv
      .UseLogger(procedure(const Msg: String) begin WriteLn('[DEBUG] ' + Msg); end)
      .Build();
      
    WriteLn('dotEnv loaded successfully');
    
    // List all loaded variables
    ConfigVars := dotEnv.ToArray;
    WriteLn('Loaded ' + Length(ConfigVars).ToString + ' configuration variables:');
    for var ConfigVar in ConfigVars do
      WriteLn('  ' + ConfigVar);
      
  except
    on E: EMVCDotEnv do
      WriteLn('Error loading dotEnv: ' + E.Message);
  end;
end;
```

---

dotEnv configuration provides a powerful, type-safe way to manage application settings with support for variable expansion, multi-line strings, profile-based environments, and complete separation from system environment variables while maintaining DelphiMVCFramework integration patterns.