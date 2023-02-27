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
  Swag.Doc.Definition,
  Swag.Doc.Path,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.Path.Operation.Response;

type
  TFakeApiEmployee = class(TObject)
  strict private
    const c_EmployeeTagName = 'API Employee';
    const c_EmployeeSchemaName = 'employee';
    ///const c_EmployeeSchemaNameResponse = 'employeeResponse';
    const c_ParameterEmployeeId = 'id';

    function DocumentGetEmployeesList: TSwagPathOperation;
    function DocumentGetEmployee: TSwagPathOperation;
    function DocumentPostEmployee: TSwagPathOperation;
    function DocumentPutEmployee: TSwagPathOperation;
    function DocumentDeleteEmployee: TSwagPathOperation;

    function DocumentRequestParameterEmployeeId: TSwagRequestParameter;
    function DocumentRequestBodyEmployee: TSwagRequestParameter;

    function DocumentEmployeeModelSchema: TJsonSchema;
    function DocumentEmployeeResponseSchema: TJsonSchema;
    function DocumentEmployeesListResponseSchema: TJsonSchema;

    function CreatePath(const pRoute: string; pOperations: array of TSwagPathOperation): TSwagPath;
    function CreateModel(const pSchemaName: string; pJsonSchema: TJsonObject): TSwagDefinition;
    function CreateResponse(const pStatusCode, pDescription: string; pJsonSchema: TJsonObject): TSwagResponse;
    function ExtractJsonFromSchema(pSchema: TJsonSchema): TJsonObject;
  public
    {$REGION 'Fake API Methods'}
    // POST /api/employees
    procedure AddEmployee;

    // GET /api/employees
    procedure GetEmployees;

    // GET /api/employees/{id}
    procedure GetEmployee(const pId: Int64);

    // PUT /api/employees/{id}
    procedure UpdateEmployee(const pId: Int64);

    // DELETE /api/employees/{id}
    procedure DeleteEmployee(const pId: Int64);
    {$ENDREGION}

    procedure DocumentApi(pSwagDoc: TSwagDoc);
  end;

implementation

uses
  Json.Schema.Field.Strings,
  Json.Schema.Field.Arrays,
  Json.Schema.Field.Enums,
  Swag.Common.Types;

{ TApiEmployee }

{$REGION 'Fake methods not implemented'}
procedure TFakeApiEmployee.AddEmployee;
begin
  { TODO : Fake method not implemented }
end;

procedure TFakeApiEmployee.GetEmployee(const pId: Int64);
begin
  { TODO : Fake method not implemented }
end;

procedure TFakeApiEmployee.GetEmployees;
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
{$ENDREGION}

procedure TFakeApiEmployee.DocumentApi(pSwagDoc: TSwagDoc);
var
  vRoute: TSwagPath;
  vModel: TSwagDefinition;
begin
  vModel := CreateModel(c_EmployeeSchemaName, ExtractJsonFromSchema(DocumentEmployeeModelSchema));
  pSwagDoc.Definitions.Add(vModel);

  ///vModel := CreateModel(c_EmployeeSchemaNameResponse, ExtractJsonFromSchema(DocumentEmployeeResponseSchema));
  ///pSwagDoc.Definitions.Add(vModel);

  vRoute := CreatePath('/employees',
    [DocumentGetEmployeesList,
     DocumentPostEmployee]);
  pSwagDoc.Paths.Add(vRoute);

  vRoute := CreatePath('/employees/{' + c_ParameterEmployeeId + '}',
    [DocumentGetEmployee,
     DocumentPutEmployee,
     DocumentDeleteEmployee]);
  pSwagDoc.Paths.Add(vRoute);
end;

function TFakeApiEmployee.CreateModel(const pSchemaName: string; pJsonSchema: TJsonObject): TSwagDefinition;
begin
  Result := TSwagDefinition.Create;
  Result.Name := pSchemaName;
  Result.JsonSchema := pJsonSchema;
end;

function TFakeApiEmployee.DocumentEmployeeModelSchema: TJsonSchema;
var
  vName: TJsonFieldString;
  vAddressSchema: TJsonSchema;
  vGender: TJsonFieldEnum;
begin
  Result := TJsonSchema.Create;
  Result.Root.Description := 'Employee response data';

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
  vSchema: TJsonSchema;
  vSchemaEmployeeModel: TJsonSchema;
begin
  vSchema := TJsonSchema.Create;
  try
    vSchema.Root.Name := 'employee';
    vSchema.Root.Description := 'Employee request data';
    vSchema.AddField<Int64>('id', 'The employee identification code.');

    vSchemaEmployeeModel := DocumentEmployeeModelSchema;
    try
      vSchema.Root.CopyFields(vSchemaEmployeeModel.Root);
    finally
      vSchemaEmployeeModel.Free;
    end;

    Result  := TJsonSchema.Create;
    Result.AddField(vSchema);
  finally
    vSchema.Free;
  end;
end;

function TFakeApiEmployee.DocumentEmployeesListResponseSchema: TJsonSchema;
var
  vSchema: TJsonSchema;
  vFieldArray: TJsonFieldArray;
begin
  vSchema := DocumentEmployeeResponseSchema;
  try
    Result  := TJsonSchema.Create;
    vFieldArray := TJsonFieldArray(Result.AddField<TJsonFieldArray>('employees', 'The employee datas.'));
    vFieldArray.ItemFieldType := vSchema.Root.Clone;
  finally
    vSchema.Free;
  end;
end;

function TFakeApiEmployee.DocumentGetEmployee: TSwagPathOperation;
var
  vResponse: TSwagResponse;
  vResponseJson: TJsonObject;
begin
  vResponseJson := ExtractJsonFromSchema(DocumentEmployeeResponseSchema);
  vResponse := CreateResponse('200', 'Successfully returns data', vResponseJson);

  Result := TSwagPathOperation.Create;
  Result.Operation := ohvGet;
  Result.OperationId := '{2DDE05B6-C01A-4EB8-B7CD-2041C51C97C7}';
  Result.Description := 'Returns a employee.';
  Result.Parameters.Add(DocumentRequestParameterEmployeeId);
  Result.Responses.Add('200', vResponse);
  Result.Tags.Add(c_EmployeeTagName);
end;

function TFakeApiEmployee.DocumentGetEmployeesList: TSwagPathOperation;
var
  vResponse: TSwagResponse;
  vResponseJson: TJsonObject;
begin
  vResponseJson := ExtractJsonFromSchema(DocumentEmployeesListResponseSchema);
  vResponse := CreateResponse('200', 'Successfully returns data', vResponseJson);

  Result := TSwagPathOperation.Create;
  Result.Operation := ohvGet;
  Result.OperationId := '{A9A8D343-00EB-402A-8248-94BDA7B6ECD4}';
  Result.Description := 'Returns a employee list.';
  Result.Responses.Add('200', vResponse);
  Result.Tags.Add(c_EmployeeTagName);
end;

function TFakeApiEmployee.DocumentPostEmployee: TSwagPathOperation;
var
  vResponse: TSwagResponse;
  vResponseJson: TJsonObject;
begin
  vResponseJson := ExtractJsonFromSchema(DocumentEmployeeResponseSchema);
  vResponse := CreateResponse('201', 'Successfully creates data', vResponseJson);

  Result := TSwagPathOperation.Create;
  Result.Operation := ohvPost;
  Result.OperationId := '{C450E1E0-341D-4947-A156-9C167BE021D5}';
  Result.Description := 'Creates a employee.';
  Result.Parameters.Add(DocumentRequestBodyEmployee);
  Result.Responses.Add('201', vResponse);
  Result.Tags.Add(c_EmployeeTagName);
end;

function TFakeApiEmployee.DocumentPutEmployee: TSwagPathOperation;
var
  vResponse: TSwagResponse;
  vResponseJson: TJsonObject;
begin
  vResponseJson := ExtractJsonFromSchema(DocumentEmployeeResponseSchema);
  vResponse := CreateResponse('200', 'Successfully updates data', vResponseJson);

  Result := TSwagPathOperation.Create;
  Result.Operation := ohvPut;
  Result.OperationId := '{28E989FB-0225-40BD-A97E-8D1EA80D09AF}';
  Result.Description := 'Updates a employee.';
  Result.Parameters.Add(DocumentRequestParameterEmployeeId);
  Result.Parameters.Add(DocumentRequestBodyEmployee);
  Result.Responses.Add('200', vResponse);
  Result.Tags.Add(c_EmployeeTagName);
end;

function TFakeApiEmployee.DocumentDeleteEmployee: TSwagPathOperation;
var
  vResponse: TSwagResponse;
begin
  vResponse := CreateResponse('204', 'Successfully deletes data', nil);

  Result := TSwagPathOperation.Create;
  Result.Operation := ohvDelete;
  Result.OperationId := '{F47F38F3-2B99-4481-AFDD-ECD89893FEA0}';
  Result.Description := 'Deletes a employee.';
  Result.Parameters.Add(DocumentRequestParameterEmployeeId);
  Result.Responses.Add('204', vResponse);
  Result.Tags.Add(c_EmployeeTagName);
end;

function TFakeApiEmployee.CreateResponse(const pStatusCode, pDescription: string;
  pJsonSchema: TJsonObject): TSwagResponse;
begin
  Result := TSwagResponse.Create;
  Result.StatusCode := pStatusCode;
  Result.Description := pDescription;
  ///vResponse.Schema.Name := c_EmployeeSchemaNameResponse;
  if Assigned(pJsonSchema) then
    Result.Schema.JsonSchema := pJsonSchema;
end;

function TFakeApiEmployee.CreatePath(const pRoute: string; pOperations: array of TSwagPathOperation): TSwagPath;
var
  vOperation: TSwagPathOperation;
begin
  Result := TSwagPath.Create;
  Result.Uri := pRoute;
  for vOperation in pOperations do
    Result.Operations.Add(vOperation);
end;

function TFakeApiEmployee.DocumentRequestBodyEmployee: TSwagRequestParameter;
begin
  Result := TSwagRequestParameter.Create;
  Result.Name := 'employee';
  Result.InLocation := rpiBody;
  Result.Required := True;
  Result.Schema.Name := c_EmployeeSchemaName;
end;

function TFakeApiEmployee.DocumentRequestParameterEmployeeId: TSwagRequestParameter;
begin
  Result := TSwagRequestParameter.Create;
  Result.Name := c_ParameterEmployeeId;
  Result.InLocation := rpiPath;
  Result.Required := True;
  Result.TypeParameter := stpInteger;
end;

function TFakeApiEmployee.ExtractJsonFromSchema(pSchema: TJsonSchema): TJsonObject;
begin
  try
    Result := pSchema.ToJson;
  finally
    pSchema.Free;
  end;
end;

end.
