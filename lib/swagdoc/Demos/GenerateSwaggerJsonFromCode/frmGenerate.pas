unit frmGenerate;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Json,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    btnGenerate: TButton;
    procedure btnGenerateClick(Sender: TObject);
  private
    function CreateJsonSomeSubType: TJsonObject;
    function CreateJsonSomeType(pJsonObjectSubType: TJsonObject): TJsonObject;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Json.Common.Helpers,
  Swag.Common.Types,
  Swag.Doc.Path,
  Swag.Doc.Path.Operation,
  Swag.Doc.Path.Operation.RequestParameter,
  Swag.Doc.Path.Operation.Response,
  Swag.Doc.Path.Operation.ResponseHeaders,
  Swag.Doc.Definition,
  Swag.Doc;

procedure TForm1.btnGenerateClick(Sender: TObject);
var
  vSwagDoc: TSwagDoc;
  vPath: TSwagPath;
  vOperation: TSwagPathOperation;
  vParam: TSwagRequestParameter;
  vResponse: TSwagResponse;
  vDefinitionSomeType: TSwagDefinition;
  vDefinitionResponseSomeType: TSwagDefinition;
  vDefinitionSomeSubType: TSwagDefinition;
  vResponseHeader: TSwagHeaders;
begin
  vSwagDoc := TSwagDoc.Create;
  try
    vSwagDoc.Info.Title := 'Sample API';
    vSwagDoc.Info.Version := 'v1.2';
    vSwagDoc.Info.TermsOfService := 'https://example.com/someurl/tos';
    vSwagDoc.Info.Description := 'Sample API Description';
    vSwagDoc.Info.Contact.Name := 'John Smith';
    vSwagDoc.Info.Contact.Email := 'jsmith@example.com';
    vSwagDoc.Info.Contact.Url := 'https://example.com/contact';
    vSwagDoc.Info.License.Name := 'Some License';
    vSwagDoc.Info.License.Url := 'https://example.com/license';

    vSwagDoc.Host := 'example.com';
    vSwagDoc.BasePath := '/basepath';

    vSwagDoc.Consumes.Add('application/json');

    vSwagDoc.Produces.Add('text/xml');
    vSwagDoc.Produces.Add('application/json');

    vSwagDoc.Schemes := [tpsHttps];

    vDefinitionSomeSubType := TSwagDefinition.Create;
    vDefinitionSomeSubType.Name := 'SomeSubType';
    vDefinitionSomeSubType.JsonSchema := CreateJsonSomeSubType;
    vSwagDoc.Definitions.Add(vDefinitionSomeSubType);

    vDefinitionSomeType := TSwagDefinition.Create;
    vDefinitionSomeType.Name := 'SomeType';
    vDefinitionSomeType.JsonSchema := CreateJsonSomeType(vDefinitionSomeSubType.GenerateJsonRefDefinition);
    vSwagDoc.Definitions.Add(vDefinitionSomeType);

    vPath := TSwagPath.Create;
    vPath.Uri := '/path/request/{param1}';

    vOperation := TSwagPathOperation.Create;
    vOperation.Operation := ohvPost;
    vOperation.OperationId := 'RequestData';
    vOperation.Description := 'Requests some data';

    vParam := TSwagRequestParameter.Create;
    vParam.Name := 'param1';
    vParam.InLocation := rpiPath;
    vParam.Description := 'A param required';
    vParam.Required := True;
    vParam.TypeParameter := stpString;
    vOperation.Parameters.Add(vParam);

    vParam := TSwagRequestParameter.Create;
    vParam.Name := 'param2';
    vParam.InLocation := rpiQuery;
    vParam.Description := 'A param that is not required';
    vParam.Required := False;
    vParam.TypeParameter := stpString;
    vOperation.Parameters.Add(vParam);

    vParam := TSwagRequestParameter.Create;
    vParam.Name := 'param3';
    vParam.InLocation := rpiBody;
    vParam.Required := True;
    vParam.Schema.Name := 'SomeType';
    vOperation.Parameters.Add(vParam);

    vResponse := TSwagResponse.Create;
    vResponse.StatusCode := '200';
    vResponse.Description := 'Successfully retrieved data';
    vResponse.Schema.Name := 'SomeType';
    vOperation.Responses.Add('200', vResponse);

    vResponseHeader := TSwagHeaders.Create;
    vResponseHeader.Name := 'X-Rate-Limit-Limit';
    vResponseHeader.Description := 'The number of allowed requests in the current period';
    vResponseHeader.ValueType := 'integer';
    vResponse.Headers.Add(vResponseHeader);
    
    vResponse := TSwagResponse.Create;
    vResponse.StatusCode := 'default';
    vResponse.Description := 'Error occured';

    vOperation.Responses.Add('default',vResponse);

    vOperation.Tags.Add('TagName');
        
    vPath.Operations.Add(vOperation);
    vSwagDoc.Paths.Add(vPath);

    vSwagDoc.GenerateSwaggerJson;
    Memo1.Lines.Add(vSwagDoc.SwaggerJson.Format);
  finally
    FreeAndNil(vSwagDoc);
  end;
end;

function TForm1.CreateJsonSomeSubType: TJsonObject;
var
  vJsonType: TJsonObject;
  vJsonProperities: TJsonObject;
begin
  Result := TJsonObject.Create;

  Result.AddPair('type','object');

  vJsonType := TJsonObject.Create;
  vJsonType.AddPair('type', 'string');

  vJsonProperities := TJsonObject.Create;
  vJsonProperities.AddPair('id', vJsonType);

  Result.AddPair('properties', vJsonProperities);
end;

function TForm1.CreateJsonSomeType(pJsonObjectSubType: TJsonObject): TJsonObject;
var
  vJsonId: TJsonObject;
  vJsonCost: TJsonObject;
  vJsonProperities: TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.AddPair('type', 'object');

  vJsonId := TJsonObject.Create;
  vJsonId.AddPair('type', 'integer');
  vJsonId.AddPair('format', 'int64');

  vJsonProperities := TJsonObject.Create;
  vJsonProperities.AddPair('id', vJsonId);

  vJsonProperities.AddPair('subType', pJsonObjectSubType);

  vJsonCost := TJsonObject.Create;
  vJsonCost.AddPair('type', 'string');
  vJsonCost.AddPair('format', 'decimel');
  vJsonCost.AddPair('multipleOf', TJsonNumber.Create(0.01));
  vJsonCost.AddPair('minimum', TJsonNumber.Create(-9999999999.99));
  vJsonCost.AddPair('maximum', TJsonNumber.Create(9999999999.99));
  vJsonCost.AddPair('title', 'Total Cost');
  vJsonCost.AddPair('description', 'Total Cost');
  vJsonCost.AddPair('example', TJsonNumber.Create(9999999999.99));
  vJsonProperities.AddPair('cost', vJsonCost);

  Result.AddPair('properties', vJsonProperities);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
