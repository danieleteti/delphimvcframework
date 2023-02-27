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

unit Swag.Doc.SecurityDefinition;

interface

uses
  System.Classes,
  System.JSON,
  System.SysUtils,
  System.Generics.Collections,
  Swag.Common.Types;

type
  TSwagSecurityDefinitionClass = class of TSwagSecurityDefinition;

  TSwagGetClassSecurityDefinition = class(TObject)
  strict private
    fClassFound: TPersistentClass;
    fSecurityType: TSwagSecurityDefinitionType;
    fSecurityTypes: TDictionary<TSwagSecurityDefinitionType, TSwagSecurityDefinitionClass>;
    procedure DoGettingSecurityDefinitionClass(pClass: TPersistentClass);
    procedure GetClasses;
  public
    constructor Create(const pSecurityType: TSwagSecurityDefinitionType);
    destructor Destroy; override;

    class function Find(const pSecurityType: TSwagSecurityDefinitionType): TPersistentClass;
    property ClassFound: TPersistentClass read fClassFound;
  end;

  /// <summary>
  /// A declaration of the security schemes available to be used in the specification.
  /// This does not enforce the security schemes on the operations and only serves to provide the relevant details for each scheme.
  /// </summary>
  TSwagSecurityDefinition = class abstract(TPersistent)
  protected
    fSchemaName: TSwagSecuritySchemeName;
    fDescription: string;
    function GetTypeSecurity: TSwagSecurityDefinitionType; virtual; abstract;
    function ReturnTypeSecurityToString: string; virtual;
  public
    constructor Create; virtual;

    function GenerateJsonObject: TJSONObject; virtual; abstract;
    procedure Load(pJson: TJSONObject); virtual; abstract;

    /// <summary>
    /// A single security scheme definition, mapping a "name" to the scheme it defines.
    /// </summary>
    property SchemeName: TSwagSecuritySchemeName read fSchemaName write fSchemaName;

    /// <summary>
    /// Required. The type of the security scheme. Valid values are "basic", "apiKey" or "oauth2".
    /// </summary>
    property TypeSecurity: TSwagSecurityDefinitionType read GetTypeSecurity;

    /// <summary>
    /// A short description for security scheme.
    /// </summary>
    property Description: string read fDescription write fDescription;
  end;


implementation

uses
  System.Rtti,
  Swag.Common.Consts,
  Swag.Common.Types.Helpers;

{ TSwagGetClassSecurityDefinition }

constructor TSwagGetClassSecurityDefinition.Create(const pSecurityType: TSwagSecurityDefinitionType);
begin
  inherited Create;
  fSecurityType := pSecurityType;
  fSecurityTypes :=  TDictionary<TSwagSecurityDefinitionType, TSwagSecurityDefinitionClass>.Create;
end;

destructor TSwagGetClassSecurityDefinition.Destroy;
begin
  fSecurityTypes.Free;
  inherited Destroy;
end;

procedure TSwagGetClassSecurityDefinition.DoGettingSecurityDefinitionClass(pClass: TPersistentClass);
var
  vContext: TRttiContext;
  vType: TRttiType;
  vAttribute: TCustomAttribute;
begin
  vContext := TRttiContext.Create;
  vType := vContext.GetType(pClass);
  for vAttribute in vType.GetAttributes do
    if (vAttribute is ASecurityDefinition) and
      (fSecurityType = ASecurityDefinition(vAttribute).Definition) then
    begin
      fClassFound := pClass;
      Break;
    end;
end;

class function TSwagGetClassSecurityDefinition.Find(const pSecurityType: TSwagSecurityDefinitionType): TPersistentClass;
var
  vGetClass: TSwagGetClassSecurityDefinition;
begin
  vGetClass := TSwagGetClassSecurityDefinition.Create(pSecurityType);
  try
    vGetClass.GetClasses;
    Result := vGetClass.ClassFound;
  finally
    vGetClass.Free;
  end;
end;

procedure TSwagGetClassSecurityDefinition.GetClasses;
var
  vClassFinder: TClassFinder;
begin
  vClassFinder := TClassFinder.Create(TSwagSecurityDefinition, False);
  try
    vClassFinder.GetClasses(DoGettingSecurityDefinitionClass);
  finally
    vClassFinder.Free;
  end;
end;

{ TSwagSecurityDefinition }

constructor TSwagSecurityDefinition.Create;
begin
  inherited Create;
end;

function TSwagSecurityDefinition.ReturnTypeSecurityToString: string;
begin
  Result := c_SwagSecurityDefinitionType[GetTypeSecurity];
end;

end.

