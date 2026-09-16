# SwagDoc Migration Guide: Swagger 2.0 to OpenAPI 3.2.1

From `svSwagger2` to `svOpenApi3`: what changed in the specification, what to change in your Delphi code, which files to deploy and how to validate the result.

SwagDoc library - release that introduces `TSwagDoc.SpecVersion` (September 2026). Delphi RTL only, usable in VCL, FMX, console and server applications.

## 1. Overview

SwagDoc now writes the same object model as a Swagger 2.0 document (`swagger.json`) or as an OpenAPI 3 document (`openapi.json`). The family is selected by the `SpecVersion` property of `TSwagDoc`.

Key facts before you start:

- `SpecVersion` defaults to `svSwagger2`. An application that does not touch the new property keeps producing the Swagger 2.0 document it produced before.
- One line switches the output to OpenAPI 3.2.1: `vSwagDoc.SpecVersion := svOpenApi3;`.
- The Swagger 2.0 classes and properties keep working when the document is generated as OpenAPI 3. Host, base path, schemes, consumes, produces, body and formData parameters are translated automatically.
- The OpenAPI 3 objects (servers, request bodies, media types, links, callbacks, webhooks, security requirements with scopes, extensions and all reusable components) are available in the object model. They are translated or omitted when a Swagger 2.0 document is generated.
- `LoadFromFile` reads Swagger 2.0 and OpenAPI 3.0, 3.1 and 3.2 documents, so an existing `swagger.json` can be converted.
- OpenAPI 3.2 documents need Swagger UI 5. The `Deploy` folder now has one folder per family.

### Quick answers

| Question | Answer |
|----------|--------|
| Do I have to change my code after updating the library? | No, unless your code has one of the source level cases of section 4.3 (extended enumerations, custom security definitions or an explicit list of units in a package). |
| Does my Swagger 2.0 output change? | Documents built in code keep the same output. Documents loaded from a swagger.json file are now written with more information (section 4.2). |
| What is the smallest migration? | Set `SpecVersion := svOpenApi3`, publish `openapi.json` with the Swagger UI 5 files of `Deploy\OpenApi3`. |
| Can I publish both versions? | Yes. Generate the document twice from the same model, changing `SpecVersion` (section 6.15). |
| Which OpenAPI release is written? | The latest 3.x supported by SwagDoc, currently 3.2.1. `SwaggerVersion` returns the exact value. |

### Migration strategies

1. **Switch only.** Keep the Swagger 2.0 model and set `SpecVersion := svOpenApi3`. SwagDoc translates everything. Recommended as the first step.
2. **Transition period.** Publish `swagger.json` and `openapi.json` side by side while the API consumers move to the new document.
3. **Full adoption.** Replace the Swagger 2.0 constructions by the OpenAPI 3 objects (servers, request bodies, content per media type, security requirements with scopes) and start using the objects that only exist in OpenAPI 3.

## 2. The version model

| SpecVersion | Root field of the document | Default file name | Schema dialect |
|-------------|----------------------------|-------------------|----------------|
| `svSwagger2` (default) | `"swagger": "2.0"` | swagger.json | Swagger 2.0 schema object (JSON Schema draft 4 subset) |
| `svOpenApi3` | `"openapi": "3.2.1"` | openapi.json | JSON Schema 2020-12 |

`SpecVersion` names a **family** of the specification, not a single release:

- When a newer 3.x release is supported, `svOpenApi3` starts writing it and your code does not change.
- A new value is only added for a new family (OpenAPI 4) or for a release that is incompatible with the documents already produced by its family.
- The exact release written in the document is returned by `SwaggerVersion` ("2.0" or "3.2.1").

```delphi
uses
  Swag.Common.Types, Swag.Doc;

vSwagDoc.SpecVersion := svOpenApi3;
vSwagDoc.GenerateSwaggerJson;                 // vSwagDoc.SwaggerVersion = '3.2.1'
vSwagDoc.SwaggerFilesFolder := 'C:\MyApi\Help';
vSwagDoc.SaveSwaggerJsonToFile;               // writes C:\MyApi\Help\openapi.json
```

The file name follows the family unless `SwaggerFileName` is assigned. Assign `SwaggerFileName := 'swagger.json'` when the published URL must not change.

## 3. What changed in the specification

### 3.1 Document structure: Swagger 2.0 compared with OpenAPI 3.2.1

| Area | Swagger 2.0 | OpenAPI 3.2.1 | SwagDoc object model |
|------|-------------|---------------|----------------------|
| Version field | `swagger: "2.0"` | `openapi: "3.2.1"` | `SpecVersion` |
| Document identity | none | `$self`, `jsonSchemaDialect` | `SelfUri`, `JsonSchemaDialect` |
| Target server | `host`, `basePath`, `schemes` | `servers` with URL templates, variables and name | `Servers`, `AddServer`, `TSwagServer.Name` |
| Media types | `consumes`, `produces` | `content` map in each request body, response, parameter and header | `AddMediaType` of `RequestBody`, `TSwagResponse`, `TSwagRequestParameter`, `TSwagHeaders` |
| Request payload | `body` and `formData` parameters | `requestBody` | `TSwagPathOperation.RequestBody` |
| Reusable objects | `definitions`, `parameters`, `responses`, `securityDefinitions` | `components` with schemas, responses, parameters, examples, requestBodies, headers, securitySchemes, links, callbacks, pathItems, mediaTypes | `Definitions`, `Responses`, `Parameters`, `Examples`, `RequestBodies`, `Headers`, `SecurityDefinitions`, `Links`, `Callbacks`, `PathItems`, `MediaTypes` |
| Schemas | Schema object, `x-nullable` extension | JSON Schema 2020-12 (`type` arrays, numeric exclusive limits, `examples` array) | Converted automatically |
| Parameter locations | query, header, path, formData, body | query, header, path, cookie, querystring | `rpiCookie`, `rpiQueryString` |
| Parameter serialization | `collectionFormat` | `style`, `explode`, `allowReserved`, `content` | `Style`, `Explode`, `AllowReserved`, `Content` |
| HTTP methods | get, put, post, delete, options, head, patch | adds trace, query and `additionalOperations` | `ohvTrace`, `ohvQuery`, `AddAdditionalOperation` |
| Examples | `examples` of a response, by MIME type | Example Object with `dataValue`, `serializedValue`, `externalValue` | `TSwagExample` |
| Headers | type and format | Header Object with schema, content, examples, required, deprecated | `TSwagHeaders` |
| Relations between operations | none | links and callbacks | `TSwagLink`, `TSwagCallback` |
| Events sent by the API | none | `webhooks` | `Webhooks` |
| Security schemes | basic, apiKey, oauth2 (one flow) | http, apiKey (also cookie), oauth2 (several flows), openIdConnect, mutualTLS | `TSwagSecurityDefinitionHttp`, `...ApiKey`, `...OAuth2`, `...OpenIdConnect`, `...MutualTls` |
| Security requirements | scopes only for OAuth2 | scopes for every scheme, AND and OR combinations | `TSwagSecurityRequirement` |
| Tags | name, description, externalDocs | adds summary, parent (nested tags) and kind | `Summary`, `Parent`, `Kind` |
| Extensions | `x-` fields | `x-` fields on every object | `Extensions` property |

### 3.2 Additions by release

**OpenAPI 3.0**

- Servers, components, request bodies, content and media types, encoding of multipart and form contents.
- Callbacks, links, cookie parameters, `style` and `explode`.
- HTTP and OpenID Connect security schemes, the Example Object and the Header Object with schema.

**OpenAPI 3.1**

- Full alignment with JSON Schema 2020-12: nullable values are written as `type` arrays, `exclusiveMinimum` and `exclusiveMaximum` are numbers, schemas accept an `examples` array.
- Webhooks, `info.summary`, `license.identifier` (SPDX), `components.pathItems`, the mutual TLS security scheme and `jsonSchemaDialect`.
- `summary` and `description` next to a `$ref` override the referenced values, and security requirements accept roles for every scheme.

**OpenAPI 3.2**

- `$self`, the QUERY method, `additionalOperations` and the `querystring` parameter location.
- Tag `summary`, `parent` and `kind`, server `name` and response `summary`.
- Media type `itemSchema` for sequential media types (JSON Lines, server-sent events), `prefixEncoding`, `itemEncoding` and `components.mediaTypes`.
- Example `dataValue` and `serializedValue`.
- OAuth2 `deviceAuthorization` flow, `oauth2MetadataUrl` and the `deprecated` field of security schemes.

**OpenAPI 3.2.1** is a patch release of 3.2 with clarifications and corrections. It adds no new objects.

## 4. Compatibility and breaking changes

### 4.1 What stays the same

- `TSwagDoc` starts with `SpecVersion = svSwagger2`, so `GenerateSwaggerJson` writes Swagger 2.0 as before.
- The documents built in code by the demos of the repository (`SampleApi`, `GenerateSwaggerJsonFromCode`) are byte-identical to the output of the previous release. The Swagger 2.0 generator only gained code paths for properties that did not exist before.
- The units used by existing applications are still available. `Swag.Doc.Path` and `Swag.Doc.Path.Operation.ResponseHeaders` keep declaring `TSwagPath` and `TSwagHeaders` as aliases of the classes that moved (section 4.3).
- `TJsonSchema` and its fields produce the same schemas. The `Nullable` property and the `Format` of string fields are new and optional.

### 4.2 Output changes for documents loaded from a swagger.json file

When a Swagger 2.0 file is loaded with `LoadFromFile` and generated again as Swagger 2.0, the previous release lost part of the document. These are fixes, but the generated file is different:

| Content of the loaded file | Previous release | Current release |
|----------------------------|------------------|-----------------|
| `securityDefinitions` | Not written | Written |
| `security` of the document and of the operations | Not written, or written without scopes | Written with the scopes (for example `["write:pets", "read:pets"]`) |
| `externalDocs` of the document | Not written | Written |
| `items` and `enum` of array parameters | Lost | Written |
| `allowEmptyValue` | Written as `true` for query parameters that did not declare it | Written only when declared |
| Document `responses` and `x-` extensions | Lost | Written |

If a downstream process compares the generated file with a stored copy, refresh the stored copy after the update.

### 4.3 Source level changes that may require code updates

#### Extended enumerations

| Enumeration | Values added |
|-------------|--------------|
| `TSwagSecurityDefinitionType` | `ssdHttp`, `ssdOpenIdConnect`, `ssdMutualTls` |
| `TSwagRequestParameterInLocation` | `rpiCookie`, `rpiQueryString` |
| `TSwagPathTypeOperation` | `ohvQuery` |
| `TSwagSecurityDefinitionApiKeyInLocation` | `kilCookie` |
| New types | `TSwagVersion`, `TSwagRequestParameterStyle`, `TSwagOAuth2FlowType` |

Check your code for:

- Constant arrays indexed by one of these enumerations, such as `array[TSwagRequestParameterInLocation] of string`. They no longer compile (E2072, number of elements differs) and need the new values.
- Loops from `Low` to `High` of these enumerations. They now visit the new values; for example a client code generator would receive the `query` method.
- `case` statements without an `else` branch that are expected to handle every value.

#### Custom security definitions

`TSwagSecurityDefinition` gained overloads and new virtual members. A class that inherits from it keeps compiling, but add the `overload` directive so the new overloads stay visible, and call `inherited` in its destructor, because the base class now owns the `Extensions` object.

```delphi
// Previous release
TMySecurityDefinition = class(TSwagSecurityDefinition)
protected
  function GetTypeSecurity: TSwagSecurityDefinitionType; override;
public
  function GenerateJsonObject: TJSONObject; override;
  procedure Load(pJson: TJSONObject); override;
end;

// Current release
TMySecurityDefinition = class(TSwagSecurityDefinition)
protected
  function GetTypeSecurity: TSwagSecurityDefinitionType; override;
public
  function GenerateJsonObject: TJSONObject; overload; override;
  function GenerateJsonObject(const pVersion: TSwagVersion): TJSONObject; overload; override;
  procedure Load(pJson: TJSONObject); overload; override;
  procedure Load(pJson: TJSONObject; const pVersion: TSwagVersion); overload; override;
  function SupportsVersion(const pVersion: TSwagVersion): Boolean; override;
end;
```

- `GenerateJsonObject(pVersion)` and `Load(pJson, pVersion)` call the version-less methods by default, so override them only when the OpenAPI 3 representation is different.
- Return `False` from `SupportsVersion` for a family that cannot represent the scheme. The scheme and the requirements that use it are then removed from that family.
- `Description`, `Deprecated` and `Extensions` are available to every scheme.

#### Classes that moved to other units

| Class | Previous unit | Current unit | Previous unit still works? |
|-------|---------------|--------------|----------------------------|
| `TSwagPath` | `Swag.Doc.Path` | `Swag.Doc.Path.Operation` | Yes, `Swag.Doc.Path` declares an alias |
| `TSwagHeaders` | `Swag.Doc.Path.Operation.ResponseHeaders` | `Swag.Doc.Path.Operation.Content` | Yes, `ResponseHeaders` declares an alias |

The aliases are the same type, so `is`, `as` and existing variables keep working.

#### New units

Applications that add the `Source` folder to the library path need nothing else. Packages or projects that list the SwagDoc units explicitly must add:

| Unit | Content |
|------|---------|
| `Swag.Common.Json` | JSON reading helpers |
| `Swag.Doc.Extensions` | Specification extensions (`TSwagExtensions`) |
| `Swag.Doc.Example` | Example Object (`TSwagExample`) |
| `Swag.Doc.Link` | Link Object (`TSwagLink`) |
| `Swag.Doc.SecurityRequirement` | Security requirements with scopes |
| `Swag.Doc.Server` | Server Object and server variables |
| `Swag.Doc.Path.Operation.Content` | Media types, encodings and headers |
| `Swag.Doc.Path.Operation.RequestBody` | Request Body Object |
| `Swag.Doc.SecurityDefinitionHttp` | HTTP security scheme (basic, bearer and others) |
| `Swag.Doc.SecurityDefinitionOpenIdConnect` | OpenID Connect security scheme |
| `Swag.Doc.SecurityDefinitionMutualTls` | Mutual TLS security scheme |
| `Swag.Doc.JsonConverter` | Conversion of references and schema keywords between the families |
| `Swag.Doc.OpenApi.Generator` | OpenAPI 3 writer |
| `Swag.Doc.OpenApi.Loader` | OpenAPI 3 reader |

#### Other behaviors to know

- `LoadFromFile` sets `SpecVersion` according to the loaded file. After loading an OpenAPI 3 file, set `SpecVersion := svSwagger2` if a Swagger 2.0 document is expected.
- `Extensions.Add` raises `ESwagErrorExtensionName` when the name does not start with `x-`.
- Properties that receive JSON values (`Example`, `DataValue`, `Value`, `Items`, `RequestBody` of a link, `ItemEncoding`) take ownership of the assigned object. Do not free it.
- `Deploy\index.html`, `Deploy\swagger.json` and the Swagger UI files of the root of `Deploy` moved to `Deploy\Swagger2`. Update scripts that copy them.

## 5. Step by step migration

1. Update the SwagDoc sources and rebuild. If you install `Source\SwagDoc.dpk`, recompile the package and remove old `.dcu` and `.bpl` files from your output folders.
2. Fix the source level cases of section 4.3, if any.
3. Build the application without changing `SpecVersion` and compare the generated `swagger.json` with the previous one (section 9.1).
4. Set `SpecVersion := svOpenApi3`, generate `openapi.json` and validate it (section 9.2).
5. Deploy the Swagger UI 5 files of `Deploy\OpenApi3` and configure the URL of the document (section 8).
6. Test the page in the browser: operations, schemas, the Authorize dialog and the Try it out requests.
7. Optionally publish both documents for a transition period (section 6.15).
8. Move the model to the OpenAPI 3 objects where they improve the documentation (section 6).
9. Update the API consumers: client generators, gateways and contract tests that read the document.

## 6. Code migration

Each topic shows the Swagger 2.0 construction, which is still valid, and the OpenAPI 3 construction. The uses clauses list only the units needed by the example.

### 6.1 Switching the version and keeping the URL

```delphi
vSwagDoc.SpecVersion := svOpenApi3;
vSwagDoc.GenerateSwaggerJson;
vSwagDoc.SwaggerFilesFolder := 'C:\MyApi\Help';
vSwagDoc.SwaggerFileName := 'swagger.json'; // optional: keep the previous file name
vSwagDoc.SaveSwaggerJsonToFile;
```

### 6.2 Host, base path and schemes become servers

Swagger 2.0 construction:

```delphi
vSwagDoc.Host := 'api.example.com';
vSwagDoc.BasePath := '/v1';
vSwagDoc.Schemes := [tpsHttps];
```

When the `Servers` list is empty, OpenAPI 3 receives one server per scheme: `https://api.example.com/v1`. With servers:

```delphi
uses
  Swag.Doc.Server;

var
  vServer: TSwagServer;
  vVariable: TSwagServerVariable;
begin
  vServer := vSwagDoc.AddServer('https://{environment}.example.com/v1', 'Cloud server');
  vServer.Name := 'cloud';
  vVariable := vServer.AddVariable('environment', 'api', 'The environment of the API.');
  vVariable.Enum.Add('api');
  vVariable.Enum.Add('sandbox');

  vSwagDoc.AddServer('http://localhost:8080/v1', 'Local development server');
end;
```

> The servers are not written in Swagger 2.0. If you also publish `swagger.json`, keep `Host`, `BasePath` and `Schemes`. When `Servers` has items, OpenAPI 3 ignores those three properties. `TSwagPath` and `TSwagPathOperation` also have a `Servers` list.

### 6.3 Body parameters become a request body

Swagger 2.0 construction:

```delphi
vParameter := TSwagRequestParameter.Create;
vParameter.Name := 'employee';
vParameter.InLocation := rpiBody;
vParameter.Required := True;
vParameter.Schema.Name := 'Employee';
vOperation.Parameters.Add(vParameter);
vOperation.Consumes.Add('application/json');
```

OpenAPI 3 receives a `requestBody` with one media type per consumed MIME type. With the request body object, each media type can have its own schema:

```delphi
vOperation.RequestBody.Description := 'The employee data.';
vOperation.RequestBody.Required := True;
vOperation.RequestBody.AddMediaType('application/json').Schema.Name := 'Employee';
vOperation.RequestBody.AddMediaType('application/xml').Schema.Name := 'Employee';
```

In Swagger 2.0 the request body is written as a body parameter with the schema of the first media type, and its media types go to `consumes`. This happens only when the operation has no body or formData parameter.

### 6.4 formData parameters become a form or multipart request body

Swagger 2.0 construction:

```delphi
vParameter := TSwagRequestParameter.Create;
vParameter.Name := 'file';
vParameter.InLocation := rpiFormData;
vParameter.TypeParameter := stpFile;
vParameter.Required := True;
vOperation.Parameters.Add(vParameter);
```

OpenAPI 3 receives a `multipart/form-data` request body with an object schema, and the `file` type becomes `type: string, format: binary`. With the request body object:

```delphi
uses
  Swag.Doc.Path.Operation.Content;

var
  vMediaType: TSwagMediaType;
begin
  vMediaType := vOperation.RequestBody.AddMediaType('multipart/form-data');
  vMediaType.Schema.JsonSchema := TJSONObject.ParseJSONValue(
    '{"type":"object","properties":{"title":{"type":"string"},' +
    '"file":{"type":"string","contentMediaType":"application/pdf"}},"required":["file"]}') as TJSONObject;
  vMediaType.AddEncoding('file').ContentType := 'application/pdf';
end;
```

In Swagger 2.0 a form or multipart request body is written as formData parameters. A property with `format: binary` or `contentMediaType` becomes a `file` parameter and an `object` property becomes a `string` parameter.

### 6.5 Produces and response schemas become content

Swagger 2.0 construction:

```delphi
vOperation.Produces.Add('application/json');

vResponse := TSwagResponse.Create;
vResponse.StatusCode := '200';
vResponse.Description := 'The employee.';
vResponse.Schema.Name := 'Employee';
vOperation.Responses.Add(vResponse.StatusCode, vResponse);
```

OpenAPI 3 receives the schema in the `content` of the response for every produced media type (`application/json` when nothing is produced). With content per media type:

```delphi
vResponse.AddMediaType('application/json').Schema.Name := 'Employee';
vResponse.AddMediaType('application/jsonl').ItemSchema.Name := 'Employee'; // one employee per line
```

- When `Content` has items, `Produces` and `Schema` are not used for that response in OpenAPI 3.
- In Swagger 2.0 the schema of the first media type is written when `Schema` is empty.
- `Summary` and `Links` of a response are written in OpenAPI 3 only.

### 6.6 Response headers

The Swagger 2.0 construction works in both families:

```delphi
vHeader := vResponse.AddHeader('X-Rate-Limit-Remaining');
vHeader.Description := 'The number of requests left in the current period.';
vHeader.ValueType := 'integer';
```

The OpenAPI 3 Header Object accepts a schema, examples, `Required`, `Deprecated`, `Style`, `Explode` and `Content`:

```delphi
vHeader := vResponse.AddHeader('X-Rate-Limit-Remaining');
vHeader.Description := 'The number of requests left in the current period.';
vHeader.Schema.JsonSchema := TJSONObject.Create.AddPair('type', 'integer');
vHeader.Example := TJSONNumber.Create(99);
```

In Swagger 2.0 `ValueType` and `Format` are taken from the schema when they are empty. Headers that are references are not written in Swagger 2.0.

### 6.7 Examples

Swagger 2.0 construction:

```delphi
vResponse.Examples.Add('application/json', TJSONObject.ParseJSONValue('{"id":42,"name":"John Smith"}') as TJSONObject);
```

OpenAPI 3 receives one Example Object per entry, inside the content of each produced media type. With the Example Object:

```delphi
uses
  Swag.Doc.Example, Swag.Doc.Path.Operation.Content;

var
  vMediaType: TSwagMediaType;
  vExample: TSwagExample;
begin
  vMediaType := vResponse.AddMediaType('application/json');
  vMediaType.Schema.Name := 'Employee';
  vExample := vMediaType.AddExample('johnSmith');
  vExample.Summary := 'An employee';
  vExample.DataValue := TJSONObject.ParseJSONValue('{"id":42,"name":"John Smith"}');

  vMediaType := vResponse.AddMediaType('text/csv');
  vMediaType.AddExample('csv').SerializedValue := 'id,name'#10'42,John Smith';
end;
```

- Use one of `DataValue`, `SerializedValue` and `ExternalValue`. `Value` is still accepted, but it must not be combined with `DataValue` or `SerializedValue`.
- Reusable examples go to `vSwagDoc.Examples` and are referenced with `Ref := '#/components/examples/Name'`.
- In Swagger 2.0 the first example of each media type is written in the `examples` of the response.

### 6.8 Reusable components and references

References written for Swagger 2.0 are rewritten in both directions:

| Reference in Swagger 2.0 | Reference in OpenAPI 3 |
|--------------------------|------------------------|
| `#/definitions/Name` | `#/components/schemas/Name` |
| `#/parameters/Name` | `#/components/parameters/Name`, or `#/components/requestBodies/Name` for body and formData parameters |
| `#/responses/Name` | `#/components/responses/Name` |

Prefer `Schema.Name` to references written by hand: SwagDoc writes the correct path for each family. The components that do not exist in Swagger 2.0 (examples, headers, links, callbacks, path items and media types) must be referenced with the OpenAPI 3 path.

```delphi
vResponse := TSwagResponse.Create;
vResponse.Name := 'notFound';                       // key under components/responses
vResponse.Description := 'The employee was not found.';
vResponse.AddMediaType('application/problem+json').Schema.Name := 'Problem';
vSwagDoc.Responses.Add(vResponse);

vResponse := TSwagResponse.Create;
vResponse.StatusCode := '404';
vResponse.Ref := '#/components/responses/notFound';
vResponse.Summary := 'Unknown employee';             // overrides the referenced summary
vOperation.Responses.Add(vResponse.StatusCode, vResponse);
```

| TSwagDoc property | `components` field | Key of each item |
|-------------------|--------------------|------------------|
| `Definitions` | `schemas` | `Name` |
| `Responses` | `responses` | `Name` |
| `Parameters` | `parameters` | `Name` |
| `Examples` | `examples` | `Name` |
| `RequestBodies` | `requestBodies` | `Name` |
| `Headers` | `headers` | `Name` |
| `SecurityDefinitions` | `securitySchemes` | `SchemeName` |
| `Links` | `links` | `Name` |
| `Callbacks` | `callbacks` | `Name` |
| `PathItems` | `pathItems` | `Uri` |
| `MediaTypes` | `mediaTypes` | `Name` |

The `Description` written with the `Ref` of a parameter, request body, response or header overrides the referenced one in OpenAPI 3, and so does the `Summary` of a response.

### 6.9 Security schemes and requirements

#### Basic and bearer authentication

```delphi
// Swagger 2.0 construction, still valid: written as http/basic in OpenAPI 3
vBasic := TSwagSecurityDefinitionBasic.Create;
vBasic.SchemeName := 'basicAuth';
vSwagDoc.SecurityDefinitions.Add(vBasic);

// Swagger 2.0 workaround for bearer tokens
vApiKey := TSwagSecurityDefinitionApiKey.Create;
vApiKey.SchemeName := 'bearerAuth';
vApiKey.InLocation := kilHeader;
vApiKey.Name := 'Authorization';
vSwagDoc.SecurityDefinitions.Add(vApiKey);

// OpenAPI 3: HTTP bearer scheme (written as the API key above in Swagger 2.0)
vBearer := TSwagSecurityDefinitionHttp.Create;
vBearer.SchemeName := 'bearerAuth';
vBearer.Scheme := 'bearer';
vBearer.BearerFormat := 'JWT';
vSwagDoc.SecurityDefinitions.Add(vBearer);
```

#### OAuth2 flows

Swagger 2.0 construction, with one flow:

```delphi
vOAuth2 := TSwagSecurityDefinitionOAuth2.Create;
vOAuth2.SchemeName := 'petstore_auth';
vOAuth2.Flow := 'accessCode';
vOAuth2.AuthorizationUrl := 'https://auth.example.com/authorize';
vOAuth2.TokenUrl := 'https://auth.example.com/token';
vOAuth2.AddScope('read:pets', 'Read your pets');
vSwagDoc.SecurityDefinitions.Add(vOAuth2);
```

The flow names are translated between the families:

| Swagger 2.0 | OpenAPI 3 | TSwagOAuth2FlowType |
|-------------|-----------|---------------------|
| `implicit` | `implicit` | `oftImplicit` |
| `password` | `password` | `oftPassword` |
| `application` | `clientCredentials` | `oftClientCredentials` |
| `accessCode` | `authorizationCode` | `oftAuthorizationCode` |
| none | `deviceAuthorization` | `oftDeviceAuthorization` |

OpenAPI 3 construction, with several flows:

```delphi
uses
  Swag.Doc.SecurityDefinitionOAuth2;

var
  vOAuth2: TSwagSecurityDefinitionOAuth2;
  vFlow: TSwagSecurityDefinitionOAuth2Flow;
begin
  vOAuth2 := TSwagSecurityDefinitionOAuth2.Create;
  vOAuth2.SchemeName := 'oauth2Auth';
  vOAuth2.OAuth2MetadataUrl := 'https://auth.example.com/.well-known/oauth-authorization-server';

  vFlow := vOAuth2.AddFlow(oftAuthorizationCode);
  vFlow.AuthorizationUrl := 'https://auth.example.com/authorize';
  vFlow.TokenUrl := 'https://auth.example.com/token';
  vFlow.AddScope('employees:write', 'Creates and updates the employees');

  vFlow := vOAuth2.AddFlow(oftClientCredentials);
  vFlow.TokenUrl := 'https://auth.example.com/token';
  vFlow.AddScope('employees:read', 'Reads the employees');

  vSwagDoc.SecurityDefinitions.Add(vOAuth2);
end;
```

In Swagger 2.0 the first flow that exists in Swagger 2.0 is written. When `Flows` is empty, the single flow properties are used in both families.

#### Security requirements

Swagger 2.0 construction, without scopes:

```delphi
vOperation.Security.Add('petstore_auth');
```

OpenAPI 3 construction, with scopes and combinations:

```delphi
uses
  Swag.Doc.SecurityRequirement;

var
  vRequirement: TSwagSecurityRequirement;
begin
  // document: bearer token OR the read scope of OAuth2
  vSwagDoc.AddSecurityRequirement.AddScheme('bearerAuth', []);
  vSwagDoc.AddSecurityRequirement.AddScheme('oauth2Auth', ['employees:read']);

  // operation: the write scope AND a client certificate
  vRequirement := vOperation.AddSecurityRequirement;
  vRequirement.AddScheme('oauth2Auth', ['employees:write']);
  vRequirement.AddScheme('mutualTlsAuth', []);

  // operation without authentication: security: []
  vHealthOperation.DisableSecurity := True;
end;
```

Each `AddSecurityRequirement` is an alternative (OR) and the schemes of a requirement are all required (AND). The requirements are written in both families. The precedence is:

| Level | First choice | Second choice | Third choice |
|-------|--------------|---------------|--------------|
| Document | `SecurityRequirements` | `DisableSecurity` writes `[]` | Every security definition as an alternative without scopes (behavior of the previous release) |
| Operation | `SecurityRequirements` | `Security` list, without scopes | `DisableSecurity` writes `[]` |

> The third choice of the document level makes every scheme an alternative way to call the whole API. When the API has more than one scheme, declare the requirements explicitly.

#### Schemes in a Swagger 2.0 document

| Scheme in the model | Swagger 2.0 output |
|---------------------|--------------------|
| HTTP basic | `type: basic` |
| HTTP bearer or other HTTP scheme | `type: apiKey`, `in: header`, `name: Authorization` |
| OpenID Connect | `type: apiKey` in the `Authorization` header with the `x-openIdConnectUrl` extension |
| API key in a cookie | Removed, with the requirements that use it |
| Mutual TLS | Removed, with the requirements that use it |
| OAuth2 with only the device authorization flow | Removed, with the requirements that use it |
| `Deprecated` of any scheme | Not written |

### 6.10 Schemas and nullable values

```delphi
vPhone := vSchema.AddField<string>('phone', 'The employee phone number.');
vPhone.Nullable := True;
```

The schema keywords are converted for the family being generated, including the JSON schemas assigned by hand to `JsonSchema`:

| Construction | Swagger 2.0 output | OpenAPI 3.2.1 output |
|--------------|--------------------|----------------------|
| `Nullable` field, `nullable: true` or `x-nullable: true` | `x-nullable: true` | `type` array that includes `"null"` |
| Nullable reference | `allOf` with the reference and `x-nullable` | `anyOf` with the reference and `type: "null"` |
| `exclusiveMinimum` and `exclusiveMaximum` | Boolean, together with `minimum` and `maximum` | Numeric limits |
| `type: file` | Kept | `type: string`, `format: binary` |
| Schema `examples` array | First item as `example` | Kept |

An OpenAPI 3.0 document loaded with `LoadFromFile` is upgraded when it is generated again.

### 6.11 Parameters

| Property | Use | Swagger 2.0 output |
|----------|-----|--------------------|
| `InLocation := rpiCookie` | Parameter sent in a cookie | Not written |
| `InLocation := rpiQueryString` | The whole query string as one parameter, with `Content` (default `application/x-www-form-urlencoded`) | Not written |
| `Style`, `Explode`, `AllowReserved` | Serialization of arrays and objects | Not written |
| `Deprecated` | Parameter being retired | Not written |
| `Example`, `Examples` | Examples of the value | Not written |
| `Content` | Complex values, for example JSON in a query parameter | Not written |
| `Schema` | Any schema, in any location | Written only for body parameters |
| `Description` with `Ref` | Overrides the referenced description | Not written |

```delphi
vParameter := TSwagRequestParameter.Create;
vParameter.Name := 'department';
vParameter.InLocation := rpiQuery;
vParameter.TypeParameter := stpString;
vParameter.Style := rpsForm;
vParameter.Explode := True;
vExample := vParameter.AddExample('sales');
vExample.Summary := 'Sales department';
vExample.DataValue := TJSONString.Create('sales');
vOperation.Parameters.Add(vParameter);
```

### 6.12 Objects that exist only in OpenAPI 3

| Feature | Code | Swagger 2.0 output |
|---------|------|--------------------|
| Document URI | `vSwagDoc.SelfUri := 'https://api.example.com/v1/openapi.json'` | Not written |
| Schema dialect | `vSwagDoc.JsonSchemaDialect := '...'` | Not written |
| Info summary, SPDX license | `Info.Summary`, `Info.License.Identifier` | Summary not written; the identifier becomes the license URL when `Url` is empty |
| Nested tags | `Tag.Summary`, `Tag.Parent`, `Tag.Kind` | Not written |
| QUERY method | `vPath.AddOperation(ohvQuery)` | Operation not written |
| Other methods | `vPath.AddAdditionalOperation('LINK')` | Not written |
| Reusable path item | `vSwagDoc.PathItems` and `vPath.Ref := '#/components/pathItems/Name'` | The referenced path item is written inline |
| Webhooks | `vSwagDoc.Webhooks.Add(vWebhook)` | Not written |
| Callbacks | `vOperation.AddCallback('Name').AddPathItem('{$request.body#/callbackUrl}')` | Not written |
| Links | `vResponse.AddLink('Name')` | Not written |
| Sequential media types | `vMediaType.ItemSchema` | Not written |
| Encodings | `AddEncoding`, `AddPrefixEncoding`, `ItemEncoding` | Not written |
| Specification extensions | `Extensions.Add('x-name', vValue)` | Written for the objects that exist in Swagger 2.0 |

```delphi
vLink := vResponse.AddLink('GetEmployeeById');
vLink.OperationId := 'getEmployee';
vLink.AddParameter('id', '$response.body#/id');

vCallback := vOperation.AddCallback('exportCompleted');
vCallbackOperation := vCallback.AddPathItem('{$request.body#/callbackUrl}').AddOperation(ohvPost);
vCallbackOperation.RequestBody.AddMediaType('application/json').Schema.Name := 'ExportResult';

vWebhook := TSwagPath.Create;
vWebhook.Uri := 'employeeHired';
vWebhook.AddOperation(ohvPost).RequestBody.AddMediaType('application/json').Schema.Name := 'Employee';
vSwagDoc.Webhooks.Add(vWebhook);
```

### 6.13 Converting an existing swagger.json file

```delphi
procedure ConvertToOpenApi3(const pSwaggerFile, pOutputFolder: string);
var
  vSwagDoc: TSwagDoc;
begin
  vSwagDoc := TSwagDoc.Create;
  try
    vSwagDoc.LoadFromFile(pSwaggerFile);   // SpecVersion becomes svSwagger2
    vSwagDoc.SpecVersion := svOpenApi3;
    vSwagDoc.GenerateSwaggerJson;
    vSwagDoc.SwaggerFilesFolder := pOutputFolder;
    vSwagDoc.SaveSwaggerJsonToFile;         // openapi.json
  finally
    vSwagDoc.Free;
  end;
end;
```

The opposite direction also works. When an OpenAPI 3 document is loaded, `Host`, `BasePath` and `Schemes` are filled from the first server, replacing its variables by their default values.

### 6.14 Custom security definitions

See section 4.3. A scheme that has a different representation in OpenAPI 3 overrides `GenerateJsonObject(pVersion)`:

```delphi
function TMySecurityDefinition.GenerateJsonObject(const pVersion: TSwagVersion): TJSONObject;
begin
  if pVersion <> svOpenApi3 then
    Exit(GenerateJsonObject);

  Result := TJSONObject.Create;
  Result.AddPair('type', 'http');
  Result.AddPair('scheme', 'digest');
  if not fDescription.IsEmpty then
    Result.AddPair('description', fDescription);
end;
```

### 6.15 Publishing both families from the same model

```delphi
procedure TApiDocumentation.Publish(const pRootFolder: string);
begin
  fSwagDoc.SpecVersion := svSwagger2;
  fSwagDoc.GenerateSwaggerJson;
  fSwagDoc.SwaggerFilesFolder := TPath.Combine(pRootFolder, 'v2');
  fSwagDoc.SaveSwaggerJsonToFile;           // v2\swagger.json

  fSwagDoc.SpecVersion := svOpenApi3;
  fSwagDoc.GenerateSwaggerJson;
  fSwagDoc.SwaggerFilesFolder := TPath.Combine(pRootFolder, 'v3');
  fSwagDoc.SaveSwaggerJsonToFile;           // v3\openapi.json
end;
```

`TPath` is declared in `System.IOUtils`. Keep `Host`, `BasePath` and `Schemes` in the model for the Swagger 2.0 document when you use `Servers`.

## 7. What a Swagger 2.0 document does not receive

When the model uses OpenAPI 3 objects and the document is generated as Swagger 2.0, SwagDoc writes a valid Swagger 2.0 document by translating or omitting what Swagger 2.0 cannot represent:

| Object model | Swagger 2.0 behavior |
|--------------|----------------------|
| `RequestBody` | Body parameter, or formData parameters for form and multipart contents |
| `Content` of a response | Schema and first example of the first media type |
| HTTP, OpenID Connect schemes | API key in the `Authorization` header (section 6.9) |
| OAuth2 with several flows | First flow that exists in Swagger 2.0 |
| Security requirements with unsupported schemes | Removed |
| Cookie and querystring parameters | Not written |
| QUERY method, additional operations | Not written |
| Webhooks, callbacks, links | Not written |
| Reusable examples, headers, links, callbacks, media types | Not written |
| Reusable path items referenced by a path | Written inline in the path |
| `Servers` | Not written; `Host`, `BasePath` and `Schemes` are used |
| `SelfUri`, `JsonSchemaDialect`, tag summary, parent and kind, server name, response summary | Not written |
| JSON Schema 2020-12 keywords | Converted as described in section 6.10 |

## 8. Deployment

### 8.1 Library files

SwagDoc depends only on the Delphi RTL (`System.JSON`, `System.Generics.Collections`, `System.RegularExpressions` and related units). It was built and tested with Delphi 12 (Studio 23.0). Two ways to use it:

- **Library path.** Add the `Source` folder to the library path or to the search path of the project. New units are found automatically.
- **Runtime package.** Install `Source\SwagDoc.dpk`. After updating, rebuild the package and remove old `.dcu` and `.bpl` files, so the new units are compiled.

Units of the current release (N marks the units that are new in this release):

| Group | Units |
|-------|-------|
| JSON Schema builder | `Json.Common.Helpers`, `Json.Schema`, `Json.Schema.Common.Types`, `Json.Schema.Field`, `Json.Schema.Field.Arrays`, `Json.Schema.Field.Booleans`, `Json.Schema.Field.DateTimes`, `Json.Schema.Field.Enums`, `Json.Schema.Field.Numbers`, `Json.Schema.Field.Objects`, `Json.Schema.Field.Strings` |
| Common | `Swag.Common.Consts`, `Swag.Common.Types`, `Swag.Common.Types.Helpers`, `Swag.Common.Json` (N) |
| Document | `Swag.Doc`, `Swag.Doc.Info`, `Swag.Doc.Info.Contact`, `Swag.Doc.Info.License`, `Swag.Doc.Tags`, `Swag.Doc.Definition`, `Swag.Doc.Server` (N), `Swag.Doc.Extensions` (N), `Swag.Doc.Example` (N), `Swag.Doc.Link` (N) |
| Paths and operations | `Swag.Doc.Path` (alias), `Swag.Doc.Path.Operation`, `Swag.Doc.Path.Operation.RequestParameter`, `Swag.Doc.Path.Operation.RequestBody` (N), `Swag.Doc.Path.Operation.Response`, `Swag.Doc.Path.Operation.ResponseHeaders` (alias), `Swag.Doc.Path.Operation.Content` (N) |
| Security | `Swag.Doc.SecurityDefinition`, `Swag.Doc.SecurityDefinitionBasic`, `Swag.Doc.SecurityDefinitionApiKey`, `Swag.Doc.SecurityDefinitionOAuth2`, `Swag.Doc.SecurityDefinitionHttp` (N), `Swag.Doc.SecurityDefinitionOpenIdConnect` (N), `Swag.Doc.SecurityDefinitionMutualTls` (N), `Swag.Doc.SecurityRequirement` (N) |
| Reading, writing and conversion | `Swag.Doc.FileLoader`, `Swag.Doc.JsonConverter` (N), `Swag.Doc.OpenApi.Generator` (N), `Swag.Doc.OpenApi.Loader` (N) |

> The security definition classes register themselves in their `initialization` section. `LoadFromFile` finds them only when their units are linked, which `Swag.Doc.FileLoader` already guarantees. Keep the units in the project when the SwagDoc units are listed explicitly.

### 8.2 Swagger UI files

The `Deploy` folder has one folder per family. Copy the folder of the family you publish to the web server and add the generated document.

**Deploy\Swagger2 - Swagger UI 3.3.1, for swagger.json**

| File | Required | Role |
|------|----------|------|
| `index.html` | Yes | Page and configuration. The `url` option is `/api/help/swagger.json`; change it to the address of your document |
| `swagger-ui.css` | Yes | Styles |
| `swagger-ui-bundle.js` | Yes | Swagger UI |
| `swagger-ui-standalone-preset.js` | Yes | Top bar layout |
| `oauth2-redirect.html` | For OAuth2 | Receives the token of the Authorize dialog |
| `favicon-16x16.png`, `favicon-32x32.png` | No | Icons |
| `swagger-ui.js` | No | Module version of Swagger UI, not used by `index.html` |
| `*.map` | No | Source maps, used only to debug Swagger UI |
| `swagger.json` | Yes | Generated by SwagDoc with `svSwagger2` |
| `readme.txt` | No | Instructions |

The page also loads fonts from `fonts.googleapis.com`. Without internet access the page falls back to local fonts.

**Deploy\OpenApi3 - Swagger UI 5.32.15, for openapi.json**

| File | Required | Role |
|------|----------|------|
| `index.html` | Yes | Page |
| `index.css` | Yes | Page styles |
| `swagger-initializer.js` | Yes | Configuration. The `url` option is `./openapi.json` |
| `swagger-ui.css` | Yes | Styles |
| `swagger-ui-bundle.js` | Yes | Swagger UI |
| `swagger-ui-standalone-preset.js` | Yes | Top bar layout |
| `oauth2-redirect.html` | For OAuth2 | Receives the token of the Authorize dialog |
| `favicon-16x16.png`, `favicon-32x32.png` | No | Icons |
| `LICENSE`, `NOTICE`, `*.LICENSE.txt` | When redistributing | Apache License 2.0 notices of Swagger UI |
| `openapi.json` | Yes | Generated by SwagDoc with `svOpenApi3` |
| `readme.txt` | No | Instructions |

> Swagger UI 3.x cannot render OpenAPI 3.1 or 3.2 documents. Publish `openapi.json` with Swagger UI 5 (the files of `Deploy\OpenApi3`, or a newer 5.x release). Swagger UI 5 also renders Swagger 2.0 documents.

### 8.3 Configuring the page

`swagger-initializer.js` of `Deploy\OpenApi3`:

```javascript
window.onload = function() {
  window.ui = SwaggerUIBundle({
    url: "/api/help/openapi.json",        // address of the document on your server
    validatorUrl: null,
    dom_id: '#swagger-ui',
    deepLinking: true,
    presets: [SwaggerUIBundle.presets.apis, SwaggerUIStandalonePreset],
    plugins: [SwaggerUIBundle.plugins.DownloadUrl],
    layout: "StandaloneLayout"
  });
};
```

To offer both documents on one Swagger UI 5 page, replace `url` by `urls`:

```javascript
urls: [
  { url: "/api/help/openapi.json", name: "OpenAPI 3.2.1" },
  { url: "/api/help/swagger.json", name: "Swagger 2.0" }
],
"urls.primaryName": "OpenAPI 3.2.1",
```

### 8.4 Web server checklist

- Serve the page over HTTP or HTTPS. Browsers block the request that loads the document when `index.html` is opened from the file system.
- Serve `.json` files with `Content-Type: application/json` and UTF-8.
- When the document is served by another origin than the page, send the CORS headers (`Access-Control-Allow-Origin`).
- Regenerate the document when the API changes (at startup or in the build) and avoid stale caches: send `Cache-Control: no-cache` for the document or add a version to its URL.
- For OAuth2, register the address of `oauth2-redirect.html` as a redirect URI in the authorization server.
- Protect the documentation route when the API is not public.

### 8.5 Transition layout

```text
/api/help/v2/   files of Deploy\Swagger2 + swagger.json   (existing consumers)
/api/help/v3/   files of Deploy\OpenApi3 + openapi.json   (new consumers)
```

Keep both until the consumers (client generators, API gateways, contract tests) read the OpenAPI 3 document, then remove the v2 route.

### 8.6 Repository layout changes

| Previous release | Current release |
|------------------|-----------------|
| `Deploy\index.html`, `Deploy\swagger.json` and the Swagger UI files in the root of `Deploy` | `Deploy\Swagger2` |
| `SampleApi` demo writes its executable and `swagger.json` in `Deploy` | Writes them in `Deploy\Swagger2` |
| none | `Deploy\OpenApi3` with Swagger UI 5.32.15 and `openapi.json` |
| none | `Demos\SampleOpenApi3`: the OpenAPI 3.2.1 version of `SampleApi`, writing to `Deploy\OpenApi3` |

## 9. Validation and testing

### 9.1 Swagger 2.0 regression

Generate `swagger.json` with the previous release and with the current release, without changing `SpecVersion`, and compare the files:

```bat
fc /b before\swagger.json after\swagger.json
```

Documents built in code must be identical. Documents loaded from files may differ as described in section 4.2.

### 9.2 OpenAPI 3.2.1 validation

- Validate `openapi.json` against the JSON Schema of the OpenAPI 3.2 specification published at https://spec.openapis.org, with any JSON Schema 2020-12 validator.
- Open the document in Swagger UI 5 and check the operations, the schemas, the Authorize dialog and the Try it out requests.
- Load the generated file with `LoadFromFile` and generate it again: the result must be identical to the original file.

### 9.3 Swagger 2.0 validation

- Validate `swagger.json` in Swagger Editor or with a Swagger 2.0 validator.
- Delphi writes the slash of JSON strings as `\/`. It is valid JSON, but tools that parse the text as YAML (Swagger Editor 4, for example) report "unknown escape character". Reformat the JSON before pasting it in those tools. The previous release had the same output.

## 10. Known limitations

- Swagger UI 5.32.15 does not list the webhooks of OpenAPI 3.2 documents (only of 3.1 documents) and does not show `additionalOperations`. Both are present in `openapi.json`.
- Swagger UI 5.32.15 shows a warning when `jsonSchemaDialect` is different from `https://spec.openapis.org/oas/3.1/dialect/base`. Leave `JsonSchemaDialect` empty unless the schemas use another dialect.
- The conversion to Swagger 2.0 omits the objects listed in section 7.
- `Servers` are not converted to `host`, `basePath` and `schemes` when a document built in code is generated as Swagger 2.0.
- A custom security definition class cannot be read by `LoadFromFile`, because the scheme types are a closed enumeration.

## 11. Migration checklist

- [ ] SwagDoc sources updated, package rebuilt and old `.dcu` and `.bpl` files removed.
- [ ] Arrays, loops and `case` statements over the extended enumerations reviewed.
- [ ] Custom security definitions updated with `overload` and `SupportsVersion`.
- [ ] New units added to packages that list the SwagDoc units.
- [ ] Swagger 2.0 output compared with the previous release.
- [ ] `SpecVersion := svOpenApi3` set, and `SwaggerFileName` assigned if the URL must not change.
- [ ] `openapi.json` validated against the OpenAPI 3.2 schema.
- [ ] Swagger UI 5 files of `Deploy\OpenApi3` published and `swagger-initializer.js` pointing to the document.
- [ ] Web server serving JSON with the right content type, CORS and cache headers.
- [ ] OAuth2 redirect URI registered for `oauth2-redirect.html`.
- [ ] Scripts updated for the `Deploy\Swagger2` and `Deploy\OpenApi3` folders.
- [ ] `Host`, `BasePath` and `Schemes` kept if `swagger.json` is still published.
- [ ] Security requirements declared explicitly, with scopes.
- [ ] API consumers informed and moved to the OpenAPI 3 document.

## 12. References

- Swagger 2.0 specification: https://github.com/OAI/OpenAPI-Specification/blob/main/versions/2.0.md
- OpenAPI 3.2.1 specification: https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.2.1.md
- JSON Schema 2020-12: https://json-schema.org/draft/2020-12
- Swagger UI distribution: https://github.com/swagger-api/swagger-ui/tree/master/dist
- SwagDoc repository: https://github.com/marcelojaloto/SwagDoc
- SwagDoc demos: `Demos\SampleApi` (Swagger 2.0) and `Demos\SampleOpenApi3` (OpenAPI 3.2.1)
