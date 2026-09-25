{******************************************************************************}
{                                                                              }
{  Delphi SwagDoc Library                                                      }
{  Copyright (c) 2018 Marcelo Jaloto                                           }
{  https://github.com/marcelojaloto/SwagDoc                                    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}

unit Sample.Api.Employee;

interface

uses
  System.JSON,
  Json.Schema,
  Swag.Doc,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path.Operation.Response;

type
  /// <summary>
  /// Documents the employee operations of the sample API with the objects of OpenAPI 3: reusable components,
  /// request bodies, media types with examples and encodings, links, callbacks, the QUERY method, additional
  /// operations and webhooks.
  /// </summary>
  TFakeApiEmployee = class(TObject)
  strict private
    const
      c_EmployeeTagName = 'Employees';
      c_EmployeeDocumentsTagName = 'Employee documents';
      c_NotificationsTagName = 'Notifications';
      c_EmployeeSchemaName = 'employee';
      c_EmployeeResponseSchemaName = 'employeeResponse';
      c_ProblemSchemaName = 'problem';
      c_ParameterEmployeeId = 'id';
      c_ParameterLimit = 'limit';
      c_HeaderRateLimitRemaining = 'X-Rate-Limit-Remaining';
      c_MimeTypeJson = 'application/json';
      c_MimeTypeJsonLines = 'application/jsonl';
      c_MimeTypeProblemJson = 'application/problem+json';
      c_MimeTypeCsv = 'text/csv';
      c_MimeTypeFormUrlEncoded = 'application/x-www-form-urlencoded';
      c_MimeTypeMultipartFormData = 'multipart/form-data';
      c_MimeTypeMultipartMixed = 'multipart/mixed';
      c_MimeTypeMultipartRelated = 'multipart/related';
      c_MimeTypePdf = 'application/pdf';
      c_ScopeRead = 'employees:read';
      c_ScopeWrite = 'employees:write';
      c_RefParameterEmployeeId = '#/components/parameters/id';
      c_RefParameterLimit = '#/components/parameters/limit';
      c_RefHeaderRateLimitRemaining = '#/components/headers/X-Rate-Limit-Remaining';
      c_RefExampleEmployeeList = '#/components/examples/employeeList';
      c_RefExampleEmployeeListCsv = '#/components/examples/employeeListCsv';
      c_RefRequestBodyEmployee = '#/components/requestBodies/employee';
      c_RefResponseNotFound = '#/components/responses/notFound';
      c_RefResponseProblem = '#/components/responses/problem';
      c_RefLinkGetEmployeeById = '#/components/links/GetEmployeeById';
      c_RefCallbackExportCompleted = '#/components/callbacks/exportCompleted';
      c_RefMediaTypeEmployeeJson = '#/components/mediaTypes/employeeJson';
      c_SchemaEmployeeList = '{"type":"array","items":{"$ref":"#/components/schemas/employeeResponse"}}';
  strict private
    fSwagDoc: TSwagDoc;

    procedure DocumentSchemas;
    procedure DocumentComponentParameters;
    procedure DocumentComponentHeaders;
    procedure DocumentComponentExamples;
    procedure DocumentComponentRequestBodies;
    procedure DocumentComponentResponses;
    procedure DocumentComponentLinks;
    procedure DocumentComponentCallbacks;
    procedure DocumentComponentMediaTypes;

    procedure DocumentPathEmployees;
    procedure DocumentPathEmployee;
    procedure DocumentPathEmployeesFilter;
    procedure DocumentPathEmployeesSearch;
    procedure DocumentPathEmployeesExports;
    procedure DocumentPathEmployeeDocuments;
    procedure DocumentWebhooks;

    procedure DocumentGetEmployeesList(pPath: TSwagPath);
    procedure DocumentPostEmployee(pPath: TSwagPath);
    procedure DocumentGetEmployee(pPath: TSwagPath);
    procedure DocumentPutEmployee(pPath: TSwagPath);
    procedure DocumentDeleteEmployee(pPath: TSwagPath);
    procedure DocumentUploadEmployeeDocument(pPath: TSwagPath);
    procedure DocumentLinkEmployeeDocument(pPath: TSwagPath);

    function DocumentEmployeeModelSchema: TJsonSchema;
    function DocumentEmployeeResponseSchema: TJsonSchema;

    function CreatePath(const pRoute: string): TSwagPath;
    function CreateResponse(pOperation: TSwagPathOperation; const pStatusCode, pDescription: string): TSwagResponse;
    function CreateResponseReference(pOperation: TSwagPathOperation; const pStatusCode, pRef: string): TSwagResponse;
    procedure AddEmployeeIdParameter(pPath: TSwagPath);
    function ExtractJsonFromSchema(pSchema: TJsonSchema): TJsonObject;
    function ParseJsonValue(const pJson: string): TJSONValue;
    function ParseJsonObject(const pJson: string): TJSONObject;
  public
    {$REGION 'Fake API Methods'}
    // POST /api/v1/employees
    procedure AddEmployee;

    // GET /api/v1/employees
    procedure GetEmployees;

    // GET /api/v1/employees/filter?department=engineering&skills=delphi,sql
    procedure FilterEmployees;

    // QUERY /api/v1/employees/search
    procedure SearchEmployees;

    // POST /api/v1/employees/exports
    procedure ExportEmployees;

    // GET /api/v1/employees/{id}
    procedure GetEmployee(const pId: Int64);

    // PUT /api/v1/employees/{id}
    procedure UpdateEmployee(const pId: Int64);

    // DELETE /api/v1/employees/{id}
    procedure DeleteEmployee(const pId: Int64);

    // POST /api/v1/employees/{id}/documents
    procedure UploadEmployeeDocument(const pId: Int64);

    // LINK /api/v1/employees/{id}/documents
    procedure LinkEmployeeDocument(const pId: Int64);
    {$ENDREGION}

    /// <summary>
    /// Adds the components, paths and webhooks of the employee operations to the document.
    /// </summary>
    procedure DocumentApi(pSwagDoc: TSwagDoc);
  end;

implementation

uses
  Json.Schema.Field.Strings,
  Json.Schema.Field.Enums,
  Swag.Common.Types,
  Swag.Doc.Definition,
  Swag.Doc.Example,
  Swag.Doc.Link,
  Swag.Doc.SecurityRequirement,
  Swag.Doc.Path.Operation.Content,
  Swag.Doc.Path.Operation.RequestBody,
  Swag.Doc.Path.Operation.RequestParameter;

{ TFakeApiEmployee }

{$REGION 'Fake methods not implemented'}
procedure TFakeApiEmployee.AddEmployee;
begin
  { TODO : Fake method not implemented }
end;

procedure TFakeApiEmployee.GetEmployees;
begin
  { TODO : Fake method not implemented }
end;

procedure TFakeApiEmployee.FilterEmployees;
begin
  { TODO : Fake method not implemented }
end;

procedure TFakeApiEmployee.SearchEmployees;
begin
  { TODO : Fake method not implemented }
end;

procedure TFakeApiEmployee.ExportEmployees;
begin
  { TODO : Fake method not implemented }
end;

procedure TFakeApiEmployee.GetEmployee(const pId: Int64);
begin
  { TODO : Fake method not implemented }
end;

procedure TFakeApiEmployee.UpdateEmployee(const pId: Int64);
begin
  { TODO : Fake method not implemented }
end;

procedure TFakeApiEmployee.DeleteEmployee(const pId: Int64);
begin
  { TODO : Fake method not implemented }
end;

procedure TFakeApiEmployee.UploadEmployeeDocument(const pId: Int64);
begin
  { TODO : Fake method not implemented }
end;

procedure TFakeApiEmployee.LinkEmployeeDocument(const pId: Int64);
begin
  { TODO : Fake method not implemented }
end;
{$ENDREGION}

procedure TFakeApiEmployee.DocumentApi(pSwagDoc: TSwagDoc);
begin
  fSwagDoc := pSwagDoc;

  DocumentSchemas;
  DocumentComponentParameters;
  DocumentComponentHeaders;
  DocumentComponentExamples;
  DocumentComponentRequestBodies;
  DocumentComponentResponses;
  DocumentComponentLinks;
  DocumentComponentCallbacks;
  DocumentComponentMediaTypes;

  DocumentPathEmployees;
  DocumentPathEmployeesFilter;
  DocumentPathEmployeesSearch;
  DocumentPathEmployeesExports;
  DocumentPathEmployee;
  DocumentPathEmployeeDocuments;
  DocumentWebhooks;
end;

procedure TFakeApiEmployee.DocumentSchemas;
var
  vDefinition: TSwagDefinition;
begin
  vDefinition := TSwagDefinition.Create;
  vDefinition.Name := c_EmployeeSchemaName;
  vDefinition.JsonSchema := ExtractJsonFromSchema(DocumentEmployeeModelSchema);
  fSwagDoc.Definitions.Add(vDefinition);

  vDefinition := TSwagDefinition.Create;
  vDefinition.Name := c_EmployeeResponseSchemaName;
  vDefinition.JsonSchema := ExtractJsonFromSchema(DocumentEmployeeResponseSchema);
  fSwagDoc.Definitions.Add(vDefinition);

  vDefinition := TSwagDefinition.Create;
  vDefinition.Name := c_ProblemSchemaName;
  vDefinition.JsonSchema := ParseJsonObject(
    '{"type":"object","description":"Problem details as defined by RFC 9457.",' +
    '"properties":{"type":{"type":"string","format":"uri-reference"},"title":{"type":"string"},' +
    '"status":{"type":"integer","format":"int32"},"detail":{"type":"string"}},"required":["title","status"]}');
  fSwagDoc.Definitions.Add(vDefinition);
end;

procedure TFakeApiEmployee.DocumentComponentParameters;
var
  vParameter: TSwagRequestParameter;
begin
  vParameter := TSwagRequestParameter.Create;
  vParameter.Name := c_ParameterEmployeeId;
  vParameter.InLocation := rpiPath;
  vParameter.Description := 'The employee identification code.';
  vParameter.Required := True;
  vParameter.TypeParameter := stpInteger;
  vParameter.Format := 'int64';
  vParameter.Example := TJSONNumber.Create(42);
  fSwagDoc.Parameters.Add(vParameter);

  vParameter := TSwagRequestParameter.Create;
  vParameter.Name := c_ParameterLimit;
  vParameter.InLocation := rpiQuery;
  vParameter.Description := 'The maximum number of employees returned.';
  vParameter.TypeParameter := stpInteger;
  vParameter.Format := 'int32';
  vParameter.Default := '20';
  vParameter.Style := rpsForm;
  vParameter.Explode := True;
  fSwagDoc.Parameters.Add(vParameter);
end;

procedure TFakeApiEmployee.DocumentComponentHeaders;
var
  vHeader: TSwagHeaders;
begin
  vHeader := TSwagHeaders.Create;
  vHeader.Name := c_HeaderRateLimitRemaining;
  vHeader.Description := 'The number of requests left in the current period.';
  vHeader.Schema.JsonSchema := ParseJsonObject('{"type":"integer","minimum":0}');
  vHeader.Example := TJSONNumber.Create(99);
  fSwagDoc.Headers.Add(vHeader);
end;

procedure TFakeApiEmployee.DocumentComponentExamples;
var
  vExample: TSwagExample;
begin
  vExample := TSwagExample.Create;
  vExample.Name := 'employeeList';
  vExample.Summary := 'Two employees';
  vExample.DataValue := ParseJsonValue(
    '[{"id":42,"name":"John Smith","gender":"male"},{"id":43,"name":"Mary Jones","gender":"female"}]');
  fSwagDoc.Examples.Add(vExample);

  vExample := TSwagExample.Create;
  vExample.Name := 'employeeListCsv';
  vExample.Summary := 'Two employees as CSV';
  vExample.SerializedValue := 'id,name,gender'#10'42,John Smith,male'#10'43,Mary Jones,female';
  fSwagDoc.Examples.Add(vExample);
end;

procedure TFakeApiEmployee.DocumentComponentRequestBodies;
var
  vRequestBody: TSwagRequestBody;
  vMediaType: TSwagMediaType;
  vExample: TSwagExample;
begin
  vRequestBody := TSwagRequestBody.Create;
  vRequestBody.Name := c_EmployeeSchemaName;
  vRequestBody.Description := 'The employee data.';
  vRequestBody.Required := True;

  vMediaType := vRequestBody.AddMediaType(c_MimeTypeJson);
  vMediaType.Schema.Name := c_EmployeeSchemaName;
  vExample := vMediaType.AddExample('newEmployee');
  vExample.Summary := 'A new employee';
  vExample.Value := ParseJsonValue('{"name":"John Smith","gender":"male","hireDate":"2026-09-01","salary":5200.5}');
  fSwagDoc.RequestBodies.Add(vRequestBody);
end;

procedure TFakeApiEmployee.DocumentComponentResponses;
var
  vResponse: TSwagResponse;
begin
  vResponse := TSwagResponse.Create;
  vResponse.Name := 'notFound';
  vResponse.Description := 'The employee was not found.';
  vResponse.AddMediaType(c_MimeTypeProblemJson).Schema.Name := c_ProblemSchemaName;
  fSwagDoc.Responses.Add(vResponse);

  vResponse := TSwagResponse.Create;
  vResponse.Name := c_ProblemSchemaName;
  vResponse.Description := 'Unexpected error.';
  vResponse.AddMediaType(c_MimeTypeProblemJson).Schema.Name := c_ProblemSchemaName;
  fSwagDoc.Responses.Add(vResponse);
end;

procedure TFakeApiEmployee.DocumentComponentLinks;
var
  vLink: TSwagLink;
begin
  vLink := TSwagLink.Create;
  vLink.Name := 'GetEmployeeById';
  vLink.OperationId := 'getEmployee';
  vLink.AddParameter(c_ParameterEmployeeId, '$response.body#/id');
  vLink.Description := 'The id returned in the response can be used to read the employee.';
  fSwagDoc.Links.Add(vLink);
end;

procedure TFakeApiEmployee.DocumentComponentCallbacks;
var
  vCallback: TSwagCallback;
  vOperation: TSwagPathOperation;
  vMediaType: TSwagMediaType;
begin
  vCallback := TSwagCallback.Create;
  vCallback.Name := 'exportCompleted';

  vOperation := vCallback.AddPathItem('{$request.body#/callbackUrl}').AddOperation(ohvPost);
  vOperation.Summary := 'Notifies that the export was completed.';
  vOperation.Tags.Add(c_NotificationsTagName);
  vOperation.RequestBody.Required := True;
  vMediaType := vOperation.RequestBody.AddMediaType(c_MimeTypeJson);
  vMediaType.Schema.JsonSchema := ParseJsonObject(
    '{"type":"object","properties":{"exportId":{"type":"string","format":"uuid"},' +
    '"fileUrl":{"type":"string","format":"uri"}},"required":["exportId"]}');
  CreateResponse(vOperation, '204', 'The notification was received.');
  fSwagDoc.Callbacks.Add(vCallback);
end;

procedure TFakeApiEmployee.DocumentComponentMediaTypes;
var
  vMediaType: TSwagMediaType;
begin
  vMediaType := TSwagMediaType.Create;
  vMediaType.Name := 'employeeJson';
  vMediaType.Schema.Name := c_EmployeeResponseSchemaName;
  vMediaType.Example := ParseJsonValue('{"id":42,"name":"John Smith","gender":"male"}');
  fSwagDoc.MediaTypes.Add(vMediaType);
end;

procedure TFakeApiEmployee.DocumentPathEmployees;
var
  vPath: TSwagPath;
begin
  vPath := CreatePath('/employees');
  vPath.Summary := 'Employees collection';
  vPath.Extensions.Add('x-resource', 'employee');
  DocumentGetEmployeesList(vPath);
  DocumentPostEmployee(vPath);
end;

procedure TFakeApiEmployee.DocumentPathEmployee;
var
  vPath: TSwagPath;
begin
  vPath := CreatePath('/employees/{' + c_ParameterEmployeeId + '}');
  AddEmployeeIdParameter(vPath);
  DocumentGetEmployee(vPath);
  DocumentPutEmployee(vPath);
  DocumentDeleteEmployee(vPath);
end;

procedure TFakeApiEmployee.DocumentPathEmployeesFilter;
var
  vOperation: TSwagPathOperation;
  vParameter: TSwagRequestParameter;
  vMediaType: TSwagMediaType;
  vEncoding: TSwagEncoding;
  vExample: TSwagExample;
  vResponse: TSwagResponse;
begin
  vOperation := CreatePath('/employees/filter').AddOperation(ohvGet);
  vOperation.OperationId := 'filterEmployees';
  vOperation.Summary := 'Filters the employees';
  vOperation.Description := 'Receives the whole query string as a single form encoded parameter.';
  vOperation.Tags.Add(c_EmployeeTagName);

  vParameter := TSwagRequestParameter.Create;
  vParameter.Name := 'filter';
  vParameter.InLocation := rpiQueryString;
  vParameter.Description := 'The filter criteria.';
  vMediaType := vParameter.AddMediaType(c_MimeTypeFormUrlEncoded);
  vMediaType.Schema.JsonSchema := ParseJsonObject(
    '{"type":"object","properties":{"department":{"type":"string"},' +
    '"skills":{"type":"array","items":{"type":"string"}}}}');
  vEncoding := vMediaType.AddEncoding('skills');
  vEncoding.Style := rpsForm;
  vEncoding.Explode := False;
  vExample := vParameter.AddExample('bySkills');
  vExample.Summary := 'Engineers who know Delphi and SQL';
  vExample.SerializedValue := 'department=engineering&skills=delphi,sql';
  vOperation.Parameters.Add(vParameter);

  vResponse := CreateResponse(vOperation, '200', 'Successfully returns data');
  vResponse.AddMediaType(c_MimeTypeJson).Schema.JsonSchema := ParseJsonObject(c_SchemaEmployeeList);
end;

procedure TFakeApiEmployee.DocumentPathEmployeesSearch;
var
  vOperation: TSwagPathOperation;
  vResponse: TSwagResponse;
begin
  vOperation := CreatePath('/employees/search').AddOperation(ohvQuery);
  vOperation.OperationId := 'searchEmployees';
  vOperation.Summary := 'Searches the employees';
  vOperation.Description := 'The QUERY method sends the search criteria in the request body and is safe and idempotent.';
  vOperation.Tags.Add(c_EmployeeTagName);

  vOperation.RequestBody.Required := True;
  vOperation.RequestBody.AddMediaType(c_MimeTypeJson).Schema.JsonSchema := ParseJsonObject(
    '{"type":"object","properties":{"name":{"type":"string"},"hiredAfter":{"type":"string","format":"date"}}}');

  vResponse := CreateResponse(vOperation, '200', 'Successfully returns data');
  vResponse.AddMediaType(c_MimeTypeJson).Schema.JsonSchema := ParseJsonObject(c_SchemaEmployeeList);
end;

procedure TFakeApiEmployee.DocumentPathEmployeesExports;
var
  vOperation: TSwagPathOperation;
  vCallback: TSwagCallback;
begin
  vOperation := CreatePath('/employees/exports').AddOperation(ohvPost);
  vOperation.OperationId := 'exportEmployees';
  vOperation.Summary := 'Exports the employees';
  vOperation.Description := 'Starts the export and calls the callback URL when the file is ready.';
  vOperation.Tags.Add(c_EmployeeTagName);

  vOperation.RequestBody.Required := True;
  vOperation.RequestBody.AddMediaType(c_MimeTypeJson).Schema.JsonSchema := ParseJsonObject(
    '{"type":"object","properties":{"callbackUrl":{"type":"string","format":"uri"}},"required":["callbackUrl"]}');

  vCallback := vOperation.AddCallback('exportCompleted');
  vCallback.Ref := c_RefCallbackExportCompleted;

  CreateResponse(vOperation, '202', 'The export was started.');
end;

procedure TFakeApiEmployee.DocumentPathEmployeeDocuments;
var
  vPath: TSwagPath;
begin
  vPath := CreatePath('/employees/{' + c_ParameterEmployeeId + '}/documents');
  AddEmployeeIdParameter(vPath);
  DocumentUploadEmployeeDocument(vPath);
  DocumentLinkEmployeeDocument(vPath);
end;

procedure TFakeApiEmployee.DocumentWebhooks;
var
  vWebhook: TSwagPath;
  vOperation: TSwagPathOperation;
begin
  vWebhook := TSwagPath.Create;
  vWebhook.Uri := 'employeeHired';

  vOperation := vWebhook.AddOperation(ohvPost);
  vOperation.OperationId := 'employeeHiredWebhook';
  vOperation.Summary := 'An employee was hired';
  vOperation.Description := 'Sent to the client applications when an employee is created.';
  vOperation.Tags.Add(c_NotificationsTagName);
  vOperation.RequestBody.Required := True;
  vOperation.RequestBody.AddMediaType(c_MimeTypeJson).Schema.Name := c_EmployeeResponseSchemaName;
  CreateResponse(vOperation, '200', 'The client received the notification.');

  fSwagDoc.Webhooks.Add(vWebhook);
end;

procedure TFakeApiEmployee.DocumentGetEmployeesList(pPath: TSwagPath);
var
  vOperation: TSwagPathOperation;
  vParameter: TSwagRequestParameter;
  vExample: TSwagExample;
  vResponse: TSwagResponse;
  vMediaType: TSwagMediaType;
begin
  vOperation := pPath.AddOperation(ohvGet);
  vOperation.OperationId := 'listEmployees';
  vOperation.Summary := 'Lists the employees';
  vOperation.Description := 'Returns the employees as a JSON array, as JSON Lines or as a CSV file.';
  vOperation.Tags.Add(c_EmployeeTagName);
  vOperation.Extensions.Add('x-rate-limit-tier', 'standard');

  vParameter := TSwagRequestParameter.Create;
  vParameter.Ref := c_RefParameterLimit;
  vOperation.Parameters.Add(vParameter);

  vParameter := TSwagRequestParameter.Create;
  vParameter.Name := 'department';
  vParameter.InLocation := rpiQuery;
  vParameter.Description := 'Returns only the employees of the department.';
  vParameter.TypeParameter := stpString;
  vExample := vParameter.AddExample('sales');
  vExample.Summary := 'Sales department';
  vExample.DataValue := TJSONString.Create('sales');
  vExample := vParameter.AddExample('engineering');
  vExample.Summary := 'Engineering department';
  vExample.DataValue := TJSONString.Create('engineering');
  vOperation.Parameters.Add(vParameter);

  vParameter := TSwagRequestParameter.Create;
  vParameter.Name := 'preferredView';
  vParameter.InLocation := rpiCookie;
  vParameter.Description := 'The layout remembered by the web client. Use the Accept header instead.';
  vParameter.TypeParameter := stpString;
  vParameter.Deprecated := True;
  vOperation.Parameters.Add(vParameter);

  vResponse := CreateResponse(vOperation, '200', 'Successfully returns data');
  vResponse.AddHeader(c_HeaderRateLimitRemaining).Ref := c_RefHeaderRateLimitRemaining;

  vMediaType := vResponse.AddMediaType(c_MimeTypeJson);
  vMediaType.Schema.JsonSchema := ParseJsonObject(c_SchemaEmployeeList);
  vMediaType.AddExample('employeeList').Ref := c_RefExampleEmployeeList;

  vMediaType := vResponse.AddMediaType(c_MimeTypeJsonLines);
  vMediaType.ItemSchema.Name := c_EmployeeResponseSchemaName;

  vMediaType := vResponse.AddMediaType(c_MimeTypeCsv);
  vMediaType.Schema.JsonSchema := ParseJsonObject('{"type":"string"}');
  vMediaType.AddExample('employeeListCsv').Ref := c_RefExampleEmployeeListCsv;
  vExample := vMediaType.AddExample('fullExport');
  vExample.Summary := 'Complete export';
  vExample.ExternalValue := 'https://example.com/samples/employees.csv';

  CreateResponseReference(vOperation, 'default', c_RefResponseProblem);
end;

procedure TFakeApiEmployee.DocumentPostEmployee(pPath: TSwagPath);
var
  vOperation: TSwagPathOperation;
  vRequirement: TSwagSecurityRequirement;
  vResponse: TSwagResponse;
  vHeader: TSwagHeaders;
  vLink: TSwagLink;
begin
  vOperation := pPath.AddOperation(ohvPost);
  vOperation.OperationId := 'createEmployee';
  vOperation.Summary := 'Creates an employee';
  vOperation.Tags.Add(c_EmployeeTagName);
  vOperation.RequestBody.Ref := c_RefRequestBodyEmployee;

  vRequirement := vOperation.AddSecurityRequirement;
  vRequirement.AddScheme('oauth2Auth', [c_ScopeWrite]);

  vResponse := CreateResponse(vOperation, '201', 'Successfully creates data');
  vHeader := vResponse.AddHeader('Location');
  vHeader.Description := 'The address of the new employee.';
  vHeader.Required := True;
  vHeader.Schema.JsonSchema := ParseJsonObject('{"type":"string","format":"uri-reference"}');
  vResponse.AddMediaType(c_MimeTypeJson).Ref := c_RefMediaTypeEmployeeJson;

  vResponse.AddLink('GetEmployeeById').Ref := c_RefLinkGetEmployeeById;
  vLink := vResponse.AddLink('UpdateEmployee');
  vLink.OperationId := 'updateEmployee';
  vLink.AddParameter(c_ParameterEmployeeId, '$response.body#/id');
  vLink.Description := 'The id returned in the response can be used to update the employee.';

  vResponse := CreateResponseReference(vOperation, '400', c_RefResponseProblem);
  vResponse.Description := 'The employee data is invalid.';
end;

procedure TFakeApiEmployee.DocumentGetEmployee(pPath: TSwagPath);
var
  vOperation: TSwagPathOperation;
  vResponse: TSwagResponse;
begin
  vOperation := pPath.AddOperation(ohvGet);
  vOperation.OperationId := 'getEmployee';
  vOperation.Summary := 'Returns an employee';
  vOperation.Tags.Add(c_EmployeeTagName);

  vResponse := CreateResponse(vOperation, '200', 'Successfully returns data');
  vResponse.AddMediaType(c_MimeTypeJson).Ref := c_RefMediaTypeEmployeeJson;

  vResponse := CreateResponseReference(vOperation, '404', c_RefResponseNotFound);
  vResponse.Summary := 'Unknown employee';
end;

procedure TFakeApiEmployee.DocumentPutEmployee(pPath: TSwagPath);
var
  vOperation: TSwagPathOperation;
  vRequirement: TSwagSecurityRequirement;
  vResponse: TSwagResponse;
begin
  vOperation := pPath.AddOperation(ohvPut);
  vOperation.OperationId := 'updateEmployee';
  vOperation.Summary := 'Updates an employee';
  vOperation.Tags.Add(c_EmployeeTagName);
  vOperation.RequestBody.Ref := c_RefRequestBodyEmployee;
  vOperation.RequestBody.Description := 'The new data of the employee.';

  vRequirement := vOperation.AddSecurityRequirement;
  vRequirement.AddScheme('oauth2Auth', [c_ScopeWrite]);

  vResponse := CreateResponse(vOperation, '200', 'Successfully updates data');
  vResponse.AddMediaType(c_MimeTypeJson).Ref := c_RefMediaTypeEmployeeJson;

  CreateResponseReference(vOperation, '404', c_RefResponseNotFound);
end;

procedure TFakeApiEmployee.DocumentDeleteEmployee(pPath: TSwagPath);
var
  vOperation: TSwagPathOperation;
  vRequirement: TSwagSecurityRequirement;
begin
  vOperation := pPath.AddOperation(ohvDelete);
  vOperation.OperationId := 'deleteEmployee';
  vOperation.Summary := 'Deletes an employee';
  vOperation.Description := 'Requires the write scope and the client certificate at the same time.';
  vOperation.Tags.Add(c_EmployeeTagName);

  vRequirement := vOperation.AddSecurityRequirement;
  vRequirement.AddScheme('oauth2Auth', [c_ScopeWrite]);
  vRequirement.AddScheme('mutualTlsAuth', []);

  CreateResponse(vOperation, '204', 'Successfully deletes data');
  CreateResponseReference(vOperation, '404', c_RefResponseNotFound);
end;

procedure TFakeApiEmployee.DocumentUploadEmployeeDocument(pPath: TSwagPath);
var
  vOperation: TSwagPathOperation;
  vMediaType: TSwagMediaType;
  vEncoding: TSwagEncoding;
  vHeader: TSwagHeaders;
begin
  vOperation := pPath.AddOperation(ohvPost);
  vOperation.OperationId := 'uploadEmployeeDocument';
  vOperation.Summary := 'Uploads a document of the employee';
  vOperation.Tags.Add(c_EmployeeDocumentsTagName);
  vOperation.RequestBody.Required := True;

  vMediaType := vOperation.RequestBody.AddMediaType(c_MimeTypeMultipartFormData);
  vMediaType.Schema.JsonSchema := ParseJsonObject(
    '{"type":"object","properties":{"metadata":{"type":"object","properties":{"title":{"type":"string"}}},' +
    '"file":{"type":"string","contentMediaType":"application/pdf"}},"required":["file"]}');
  vEncoding := vMediaType.AddEncoding('metadata');
  vEncoding.ContentType := c_MimeTypeJson;
  vEncoding := vMediaType.AddEncoding('file');
  vEncoding.ContentType := c_MimeTypePdf;
  vHeader := vEncoding.AddHeader('X-Document-Checksum');
  vHeader.Description := 'The SHA-256 checksum of the file.';
  vHeader.Schema.JsonSchema := ParseJsonObject('{"type":"string"}');

  vMediaType := vOperation.RequestBody.AddMediaType(c_MimeTypeMultipartRelated);
  vMediaType.Schema.JsonSchema := ParseJsonObject(
    '{"type":"array","prefixItems":[{"type":"object"},{"type":"string","contentMediaType":"application/pdf"}]}');
  vMediaType.AddPrefixEncoding.ContentType := c_MimeTypeJson;
  vMediaType.AddPrefixEncoding.ContentType := c_MimeTypePdf;

  vMediaType := vOperation.RequestBody.AddMediaType(c_MimeTypeMultipartMixed);
  vMediaType.ItemSchema.JsonSchema := ParseJsonObject('{"type":"string","contentMediaType":"application/pdf"}');
  vMediaType.ItemEncoding := TSwagEncoding.Create;
  vMediaType.ItemEncoding.ContentType := c_MimeTypePdf;

  CreateResponse(vOperation, '201', 'The document was stored.');
end;

procedure TFakeApiEmployee.DocumentLinkEmployeeDocument(pPath: TSwagPath);
var
  vOperation: TSwagPathOperation;
  vParameter: TSwagRequestParameter;
begin
  vOperation := pPath.AddAdditionalOperation('LINK');
  vOperation.OperationId := 'linkEmployeeDocument';
  vOperation.Summary := 'Links an existing document to the employee';
  vOperation.Tags.Add(c_EmployeeDocumentsTagName);

  vParameter := TSwagRequestParameter.Create;
  vParameter.Name := 'Link';
  vParameter.InLocation := rpiHeader;
  vParameter.Description := 'The address of the document, as defined by RFC 8288.';
  vParameter.Required := True;
  vParameter.TypeParameter := stpString;
  vOperation.Parameters.Add(vParameter);

  CreateResponse(vOperation, '204', 'The document was linked.');
end;

function TFakeApiEmployee.DocumentEmployeeModelSchema: TJsonSchema;
var
  vName: TJsonFieldString;
  vAddressSchema: TJsonSchema;
  vGender: TJsonFieldEnum;
begin
  Result := TJsonSchema.Create;
  Result.Root.Description := 'Employee request data';

  vName := TJsonFieldString(Result.AddField<string>('name', 'The employee full name.'));
  vName.Required := True;
  vName.MaxLength := 80;

  Result.AddField<string>('phone', 'The employee phone number.');
  Result.AddField<TDate>('hireDate', 'The employee hire date.');
  Result.AddField<Double>('salary', 'The employee gross salary.');

  vGender := Result.AddFieldAsType<TJsonFieldEnum>('gender', 'The employee gender.');
  vGender.Required := True;
  vGender.EnumType := etString;
  vGender.AddItems(['male', 'female']);

  vAddressSchema := TJsonSchema.Create;
  try
    vAddressSchema.Root.Name := 'address';
    vAddressSchema.Root.Description := 'The employee full address.';
    vAddressSchema.AddField<string>('description', 'The employee address description.');
    vAddressSchema.AddField<string>('city', 'The employee address city.');
    vAddressSchema.AddField<string>('region', 'The employee address region.');
    vAddressSchema.AddField<string>('country', 'The employee address country.');
    vAddressSchema.AddField<string>('postalCode', 'The employee address postal code.');

    Result.AddField(vAddressSchema);
  finally
    vAddressSchema.Free;
  end;
end;

function TFakeApiEmployee.DocumentEmployeeResponseSchema: TJsonSchema;
var
  vSchemaEmployeeModel: TJsonSchema;
begin
  Result := TJsonSchema.Create;
  Result.Root.Description := 'Employee response data';
  Result.AddField<Int64>('id', 'The employee identification code.');

  vSchemaEmployeeModel := DocumentEmployeeModelSchema;
  try
    Result.Root.CopyFields(vSchemaEmployeeModel.Root);
  finally
    vSchemaEmployeeModel.Free;
  end;
end;

function TFakeApiEmployee.CreatePath(const pRoute: string): TSwagPath;
begin
  Result := TSwagPath.Create;
  Result.Uri := pRoute;
  fSwagDoc.Paths.Add(Result);
end;

function TFakeApiEmployee.CreateResponse(pOperation: TSwagPathOperation;
  const pStatusCode, pDescription: string): TSwagResponse;
begin
  Result := TSwagResponse.Create;
  Result.StatusCode := pStatusCode;
  Result.Description := pDescription;
  pOperation.Responses.Add(pStatusCode, Result);
end;

function TFakeApiEmployee.CreateResponseReference(pOperation: TSwagPathOperation;
  const pStatusCode, pRef: string): TSwagResponse;
begin
  Result := CreateResponse(pOperation, pStatusCode, '');
  Result.Ref := pRef;
end;

procedure TFakeApiEmployee.AddEmployeeIdParameter(pPath: TSwagPath);
var
  vParameter: TSwagRequestParameter;
begin
  vParameter := TSwagRequestParameter.Create;
  vParameter.Ref := c_RefParameterEmployeeId;
  pPath.Parameters.Add(vParameter);
end;

function TFakeApiEmployee.ExtractJsonFromSchema(pSchema: TJsonSchema): TJsonObject;
begin
  try
    Result := pSchema.ToJson;
  finally
    pSchema.Free;
  end;
end;

function TFakeApiEmployee.ParseJsonValue(const pJson: string): TJSONValue;
begin
  Result := TJSONObject.ParseJSONValue(pJson);
end;

function TFakeApiEmployee.ParseJsonObject(const pJson: string): TJSONObject;
begin
  Result := ParseJsonValue(pJson) as TJSONObject;
end;

end.
