# Sample Projects Guide

DelphiMVCFramework comes with 40+ sample projects that demonstrate all framework features. Each sample is self-contained and focuses on specific functionality.

## 📁 Sample Structure

All samples are located in the `samples/` directory. Each sample is a complete, runnable project that demonstrates specific features of the framework.

## 🚀 Core Samples

### Hello World
**Location**: `samples/hello_world/`
The simplest possible DMVCFramework server demonstrating basic setup and routing.

### Renders
**Location**: `samples/renders/`
Comprehensive demonstration of all rendering capabilities:
- Object serialization
- List rendering
- Dataset to JSON conversion
- Custom serialization attributes
- HATEOAS support

### Functional Actions Showcase
**Location**: `samples/function_actions_showcase/`
Demonstrates the new functional actions feature where functions can be used as controller actions:

```pascal
[MVCPath('/sum/($A)/($B)')]
function GetSum(const A, B: Integer): Integer;

[MVCPath('/person')]
function GetPerson: TPerson;
```

## 🛡️ Middleware Samples

### CORS Middleware
**Location**: `samples/middleware_cors/`
Cross-Origin Resource Sharing configuration and handling.

### Static Files Middleware
**Location**: `samples/middleware_staticfiles/`
Serving static content and SPA applications:
- File serving configuration
- SPA routing support
- Custom file filtering
- Security considerations

### JWT Blacklist Middleware
**Location**: `samples/middleware_jwtblacklist/`
JWT token blacklisting and logout functionality for JWT-based authentication.

### Analytics Middleware
**Location**: `samples/middleware_analytics/`
API usage analytics with automatic CSV generation for monitoring and analysis.

### Compression Middleware
**Location**: `samples/middleware_compression/`
Response compression with gzip and deflate support.

## 💾 ActiveRecord and Database Samples

### ActiveRecord Showcase
**Location**: `samples/activerecord_showcase/`
Complete demonstration of the MVCActiveRecord ORM:
- Entity mapping and attributes
- CRUD operations
- RQL queries
- Relationships
- Validation
- Nullable types
- Connection management

### ActiveRecord CRUD
**Location**: `samples/activerecord_crud/`
RESTful CRUD operations using MVCActiveRecord with automatic endpoint generation.

### Articles CRUD VCL Client Meta
**Location**: `samples/articles_crud_vcl_client_meta/`
VCL client application demonstrating metadata-driven dataset initialization and JSON data loading.

## 🌐 JSON-RPC Samples

### JSON-RPC with Published Objects
**Location**: `samples/jsonrpc_with_published_objects/`
Complete JSON-RPC 2.0 implementation:
- Exposing regular Delphi objects
- Automatic method publishing
- Async JSON-RPC calls (new feature)
- Parameter marshalling
- Error handling

### Simple JSON-RPC
Basic JSON-RPC server implementation with method calls and notifications.

## 🔐 Security and Authentication Samples

### JWT Authentication
**Location**: `samples/jsonwebtoken/`
Complete JWT implementation:
- Token generation and validation
- Claims handling
- Token expiration
- Authentication middleware

### Basic Authentication
HTTP Basic Authentication implementation with custom authentication handlers.

### Custom Authentication
Implementing custom authentication schemes and authorization logic.

## 🎨 Server-Side Views

### Mustache Templates
**Location**: `samples/serversideviews_mustache/`
Server-side rendering using Mustache templating engine:
- Template syntax
- Partial templates
- Data binding
- Dynamic content generation

### eLua Server Side Views
**Location**: `samples/serversideviews_lua/`
Server-side view support using eLua scripting engine (requires Lua DLLs).

### TemplatePro
Advanced templating with TemplatePro engine for complex template scenarios.

## 📋 Advanced Features

### Server-Sent Events
**Location**: `samples/serversentevents/`
Real-time push notifications using Server-Sent Events:
- Event streaming
- Connection management
- Real-time updates
- Client-server communication

### File Upload
**Location**: `samples/fileupload/`
File handling operations:
- Multipart form data handling
- File upload processing
- Progress tracking
- Security considerations

### HTMX Support
**Location**: `samples/htmx/`
HTMX server-side support through `MVCFramework.HTMX.pas`:
- HTMX request handling
- Dynamic content updates
- Server-side rendering for HTMX

### Profiling
**Location**: `samples/profiling/`
Built-in profiler demonstration:
```pascal
procedure ProfiledAction;
begin
  begin var Prof := Profiler.Start('DataProcessing');
    ProcessData();
  end; // Automatically logged
end;
```

### Spring4D Nullables
**Location**: `samples/renders_spring4d_nullables/`
Support for Spring4D nullable types in serialization and rendering.

## 🛠️ Specialized Samples

### Custom Exception Handling
**Location**: `samples/custom_exception_handling/`
Implementing custom exception handling mechanisms for better error management.

### Entity Processor
Advanced entity processing and validation with lifecycle hooks and business rules.

### Avoid Mid-Air Collisions
**Location**: `samples/avoid_mid_air_collisions_sample/`
Demonstrates ETag-based conflict detection and resolution for safe concurrent updates.

### Swagger API Versioning Primer
**Location**: `samples/swagger_api_versioning_primer/`
API versioning strategies with Swagger/OpenAPI documentation.

### Custom Type Serializers
Implementing custom serialization logic for complex data types.

## 📱 Platform-Specific Samples

### Server in DLL
Demonstrates packaging DMVCFramework server as a Windows DLL.

### Linux Daemon
Running DMVCFramework applications as Linux daemons with proper signal handling.

### Apache Module
Deploying DMVCFramework applications as Apache modules.

## 🧪 Client Samples

### REST Client Showcase
Demonstrates the built-in REST client capabilities:
- HTTP method support
- Authentication
- Response handling
- Error management

### VCL Client Applications
Desktop client applications consuming DMVCFramework APIs.

## 🚀 Running the Samples

### Prerequisites
1. DelphiMVCFramework installed and configured
2. Appropriate Delphi version (see compatibility matrix)
3. Database connections configured (for database samples)

### Basic Steps
1. **Navigate** to the desired sample directory
2. **Open** the project file (.dproj) in Delphi
3. **Configure** database connections if required (check documentation in sample)
4. **Compile and run** (F9)
5. **Test** the endpoints using browser, Postman, or provided client

### Database Setup for ActiveRecord Samples

Some samples require database setup. Connection configurations are typically found in:
- Configuration files within the sample directory
- WebModule creation procedures
- Dedicated database setup units

Common databases used in samples:
- **PostgreSQL** - Full feature samples
- **SQLite** - Embedded database samples
- **Firebird** - Cross-platform samples
- **MySQL/MariaDB** - Alternative SQL database samples

### Testing the Samples

**Web Browser**: For simple GET requests and web interfaces
```
http://localhost:8080/api/hello
http://localhost:8080/api/customers
```

**Postman/Insomnia**: For comprehensive REST API testing with different HTTP methods

**curl**: Command-line testing
```bash
curl -X GET http://localhost:8080/api/customers
curl -X POST http://localhost:8080/api/customers -H "Content-Type: application/json" -d '{"name":"John Doe"}'
```

**Built-in Swagger UI**: Many samples include automatic API documentation
```
http://localhost:8080/api/system/describeserver.info
```

## 📖 Learning Path

### Beginner (Start Here)
1. **hello_world** - Basic framework concepts
2. **renders** - Data serialization and response handling
3. **middleware_staticfiles** - Static content serving
4. **jsonwebtoken** - Basic authentication

### Intermediate
1. **activerecord_showcase** - ORM and database integration
2. **jsonrpc_with_published_objects** - JSON-RPC services
3. **serversideviews_mustache** - Server-side rendering
4. **serversentevents** - Real-time features

### Advanced
1. **custom_exception_handling** - Error handling strategies
2. **profiling** - Performance optimization
3. **middleware_jwtblacklist** - Advanced security
4. **htmx** - Modern web development patterns

## 🤝 Sample Guidelines

Each sample includes:
- **README.md** - Setup instructions and feature explanation
- **Complete source code** - Fully functional project
- **Database scripts** (if needed) - Setup and sample data
- **Documentation comments** - Code explanation
- **Configuration examples** - Different deployment scenarios

### Sample Structure
```
sample_name/
├── README.md              # Setup and usage instructions
├── ProjectName.dpr        # Main project file
├── ProjectName.dproj      # Delphi project file
├── WebModuleU.pas         # Web module with configuration
├── ControllerU.pas        # Sample controller(s)
├── EntityU.pas           # Data models (if applicable)
├── templates/            # Template files (if applicable)
├── www/                  # Static files (if applicable)
└── sql/                  # Database scripts (if applicable)
```

## 💡 Tips for Using Samples

1. **Start Simple**: Begin with hello_world and gradually progress to more complex samples
2. **Read Documentation**: Each sample has specific setup requirements
3. **Database Configuration**: Check connection strings and database requirements
4. **Port Conflicts**: Default port is usually 8080, change if needed
5. **Dependencies**: Some samples require additional libraries or services
6. **Learning Approach**: Study the code, modify it, experiment with changes

## 🔧 Common Issues

**Port Already in Use**
```
Change the port in the main program file or WebModule configuration
```

**Database Connection Errors**
```
Verify connection strings, database server status, and credentials
```

**Missing Dependencies**
```
Check if additional libraries or services are required (Redis, specific database drivers, etc.)
```

**Compilation Errors**
```
Ensure all library paths are correctly configured in Delphi IDE
```

## 🎯 Sample Categories Summary

| Category | Sample Count | Key Features |
|----------|-------------|--------------|
| **Core Framework** | 5+ | Basic setup, routing, rendering |
| **Middleware** | 8+ | CORS, authentication, static files, compression |
| **Database/ORM** | 6+ | ActiveRecord, CRUD, multiple databases |
| **JSON-RPC** | 3+ | RPC services, async calls, object publishing |
| **Security** | 4+ | JWT, Basic Auth, custom authentication |
| **Views/Templates** | 3+ | Mustache, eLua, TemplatePro |
| **Advanced Features** | 10+ | SSE, profiling, HTMX, file handling |
| **Client Applications** | 3+ | REST clients, VCL applications |
| **Platform Specific** | 4+ | DLL, Linux, Apache module |

---

**📚 Remember**: Each sample is designed to be educational and practical. Use them as starting points for your own applications and don't hesitate to modify and experiment with the code to better understand the framework's capabilities.