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
  Swag.Common.Types,
  Swag.Doc.Extensions;

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
  /// In a Swagger 2.0 document the schemes are written under securityDefinitions and in an OpenAPI 3 document
  /// they are written under components/securitySchemes.
  /// </summary>
  TSwagSecurityDefinition = class abstract(TPersistent)
  protected
    fSchemaName: TSwagSecuritySchemeName;
    fDescription: string;
    fDeprecated: Boolean;
    fExtensions: TSwagExtensions;
    function GetTypeSecurity: TSwagSecurityDefinitionType; virtual; abstract;
    function ReturnTypeSecurityToString: string; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    /// <summary>
    /// Returns True when the security scheme can be written in a document of the given specification family.
    /// The schemes that have no equivalent in Swagger 2.0 are not written in a Swagger 2.0 document.
    /// </summary>
    function SupportsVersion(const pVersion: TSwagVersion): Boolean; virtual;

    /// <summary>
    /// Generates the Swagger 2.0 security scheme object.
    /// </summary>
    function GenerateJsonObject: TJSONObject; overload; virtual; abstract;

    /// <summary>
    /// Generates the security scheme object for the given specification version.
    /// The default implementation writes the Swagger 2.0 object, so the descendants only need to override it
    /// when the OpenAPI 3 representation is different.
    /// </summary>
    function GenerateJsonObject(const pVersion: TSwagVersion): TJSONObject; overload; virtual;

    /// <summary>
    /// Loads the Swagger 2.0 security scheme object.
    /// </summary>
    procedure Load(pJson: TJSONObject); overload; virtual; abstract;

    /// <summary>
    /// Loads the security scheme object written for the given specification version.
    /// The default implementation reads the Swagger 2.0 object.
    /// </summary>
    procedure Load(pJson: TJSONObject; const pVersion: TSwagVersion); overload; virtual;

    /// <summary>
    /// A single security scheme definition, mapping a "name" to the scheme it defines.
    /// </summary>
    property SchemeName: TSwagSecuritySchemeName read fSchemaName write fSchemaName;

    /// <summary>
    /// Required. The type of the security scheme. Valid values are "basic", "apiKey" or "oauth2" in Swagger 2.0
    /// and "http", "apiKey", "oauth2", "openIdConnect" or "mutualTLS" in OpenAPI 3.
    /// </summary>
    property TypeSecurity: TSwagSecurityDefinitionType read GetTypeSecurity;

    /// <summary>
    /// A short description for security scheme.
    /// </summary>
    property Description: string read fDescription write fDescription;

    /// <summary>
    /// Declares this security scheme to be deprecated. Consumers SHOULD refrain from usage of the declared scheme.
    /// Default value is false. Available in OpenAPI 3 only.
    /// </summary>
    property Deprecated: Boolean read fDeprecated write fDeprecated;

    /// <summary>
    /// The Specification Extensions of the security scheme.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
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
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagSecurityDefinition.Destroy;
begin
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

function TSwagSecurityDefinition.GenerateJsonObject(const pVersion: TSwagVersion): TJSONObject;
begin
  Result := GenerateJsonObject;
end;

procedure TSwagSecurityDefinition.Load(pJson: TJSONObject; const pVersion: TSwagVersion);
begin
  Load(pJson);
end;

function TSwagSecurityDefinition.SupportsVersion(const pVersion: TSwagVersion): Boolean;
begin
  Result := True;
end;

function TSwagSecurityDefinition.ReturnTypeSecurityToString: string;
begin
  Result := c_SwagSecurityDefinitionType[GetTypeSecurity];
end;

end.

