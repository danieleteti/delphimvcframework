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

unit Sample.SwagDoc;

interface

uses
  Swag.Doc;

type
  TSampleApiSwagDocBuilder = class(TObject)
  strict private
    fSwagDoc: TSwagDoc;
    fDeployFolder: string;

    procedure DocumentApiInfo;
    procedure DocumentApiSettings;
    procedure DocumentApi;
    procedure DocumentApiEmployee;

    procedure SaveSwaggerJson;
  private
    procedure SetDeployFolder(const Value: string);
  public
    function Generate: string;
    property DeployFolder: string read fDeployFolder write SetDeployFolder;
  end;

implementation

uses
  Json.Common.Helpers,
  Swag.Common.Types,
  Sample.Api.Employee;

{ TSampleApiSwagDocBuilder }

function TSampleApiSwagDocBuilder.Generate: string;
begin
  fSwagDoc := TSwagDoc.Create;
  try
    DocumentApiInfo;
    DocumentApiSettings;
    DocumentApi;
    fSwagDoc.GenerateSwaggerJson;
    SaveSwaggerJson;
    Result := fSwagDoc.SwaggerJson.Format;
  finally
    fSwagDoc.Free;
  end;
end;

procedure TSampleApiSwagDocBuilder.DocumentApiInfo;
begin
  fSwagDoc.Info.Title := 'Sample API';
  fSwagDoc.Info.Version := 'v1';
  fSwagDoc.Info.TermsOfService := 'http://www.apache.org/licenses/LICENSE-2.0.txt';
  fSwagDoc.Info.Description := 'Sample API Description';
  fSwagDoc.Info.Contact.Name := 'Marcelo Jaloto';
  fSwagDoc.Info.Contact.Email := 'marcelojaloto@gmail.com';
  fSwagDoc.Info.Contact.Url := 'https://github.com/marcelojaloto/SwagDoc';
  fSwagDoc.Info.License.Name := 'Apache License - Version 2.0, January 2004';
  fSwagDoc.Info.License.Url := 'http://www.apache.org/licenses/LICENSE-2.0';
end;

procedure TSampleApiSwagDocBuilder.DocumentApiSettings;
begin
  fSwagDoc.Host := 'localhost';
  fSwagDoc.BasePath := '/api';

  fSwagDoc.Consumes.Add('application/json');
  fSwagDoc.Produces.Add('application/json');

  fSwagDoc.Schemes := [tpsHttp];
end;

procedure TSampleApiSwagDocBuilder.DocumentApi;
begin
  DocumentApiEmployee;
end;

procedure TSampleApiSwagDocBuilder.DocumentApiEmployee;
var
  vApiEmployee: TFakeApiEmployee;
begin
  vApiEmployee := TFakeApiEmployee.Create;
  try
    vApiEmployee.DocumentApi(fSwagDoc);
  finally
    vApiEmployee.Free;
  end;
end;

procedure TSampleApiSwagDocBuilder.SaveSwaggerJson;
begin
  fSwagDoc.SwaggerFilesFolder := fDeployFolder;
  fSwagDoc.SaveSwaggerJsonToFile;
end;

procedure TSampleApiSwagDocBuilder.SetDeployFolder(const Value: string);
begin
  fDeployFolder := Value;
end;

end.
