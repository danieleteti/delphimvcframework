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

unit Swag.Doc.SecurityRequirement;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.JSON,
  Swag.Common.Types;

type
  /// <summary>
  /// A security scheme required by a Security Requirement, with the scopes required for the execution.
  /// For oauth2 and openIdConnect schemes the scopes are scope names; for other schemes they are role names.
  /// </summary>
  TSwagSecurityRequirementScheme = class(TObject)
  private
    fSchemeName: TSwagSecuritySchemeName;
    fScopes: TList<string>;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    /// <summary>
    /// The name of a security scheme declared in the security definitions of the document.
    /// </summary>
    property SchemeName: TSwagSecuritySchemeName read fSchemeName write fSchemeName;

    /// <summary>
    /// The scopes required by the scheme. The list MAY be empty.
    /// </summary>
    property Scopes: TList<string> read fScopes;
  end;

  /// <summary>
  /// Lists the security schemes required to execute an operation. All the schemes of a requirement MUST be satisfied
  /// (logical AND). When a list of requirements is declared, only one of them needs to be satisfied (logical OR).
  /// A requirement without schemes indicates that anonymous access is supported.
  /// </summary>
  TSwagSecurityRequirement = class(TObject)
  private
    fSchemes: TObjectList<TSwagSecurityRequirementScheme>;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONObject);

    /// <summary>
    /// Adds a required security scheme with its scopes and returns it. Pass an empty array when no scope is required.
    /// </summary>
    function AddScheme(const pSchemeName: TSwagSecuritySchemeName; const pScopes: array of string): TSwagSecurityRequirementScheme;

    /// <summary>
    /// Returns True when the requirement contains the security scheme.
    /// </summary>
    function ContainsScheme(const pSchemeName: TSwagSecuritySchemeName): Boolean;

    /// <summary>
    /// Generates the array of Security Requirement Objects of the list.
    /// </summary>
    class function GenerateJsonArray(pRequirements: TObjectList<TSwagSecurityRequirement>): TJSONArray;

    /// <summary>
    /// Loads an array of Security Requirement Objects into the list.
    /// </summary>
    class procedure LoadArray(pJson: TJSONArray; pRequirements: TObjectList<TSwagSecurityRequirement>);

    /// <summary>
    /// The security schemes that are all required by this requirement.
    /// </summary>
    property Schemes: TObjectList<TSwagSecurityRequirementScheme> read fSchemes;
  end;

implementation

{ TSwagSecurityRequirementScheme }

constructor TSwagSecurityRequirementScheme.Create;
begin
  inherited Create;
  fScopes := TList<string>.Create;
end;

destructor TSwagSecurityRequirementScheme.Destroy;
begin
  FreeAndNil(fScopes);
  inherited Destroy;
end;

{ TSwagSecurityRequirement }

constructor TSwagSecurityRequirement.Create;
begin
  inherited Create;
  fSchemes := TObjectList<TSwagSecurityRequirementScheme>.Create;
end;

destructor TSwagSecurityRequirement.Destroy;
begin
  FreeAndNil(fSchemes);
  inherited Destroy;
end;

function TSwagSecurityRequirement.AddScheme(const pSchemeName: TSwagSecuritySchemeName;
  const pScopes: array of string): TSwagSecurityRequirementScheme;
var
  vScope: string;
begin
  Result := TSwagSecurityRequirementScheme.Create;
  Result.SchemeName := pSchemeName;
  for vScope in pScopes do
    Result.Scopes.Add(vScope);
  fSchemes.Add(Result);
end;

function TSwagSecurityRequirement.ContainsScheme(const pSchemeName: TSwagSecuritySchemeName): Boolean;
var
  vScheme: TSwagSecurityRequirementScheme;
begin
  Result := False;
  for vScheme in fSchemes do
    if SameStr(vScheme.SchemeName, pSchemeName) then
      Exit(True);
end;

function TSwagSecurityRequirement.GenerateJsonObject: TJSONObject;
var
  vScheme: TSwagSecurityRequirementScheme;
  vJsonScopes: TJSONArray;
  vScope: string;
begin
  Result := TJSONObject.Create;
  for vScheme in fSchemes do
  begin
    vJsonScopes := TJSONArray.Create;
    for vScope in vScheme.Scopes do
      vJsonScopes.Add(vScope);
    Result.AddPair(vScheme.SchemeName, vJsonScopes);
  end;
end;

procedure TSwagSecurityRequirement.Load(pJson: TJSONObject);
var
  vIndex: Integer;
  vScopeIndex: Integer;
  vScheme: TSwagSecurityRequirementScheme;
  vJsonScopes: TJSONArray;
begin
  if not Assigned(pJson) then
    Exit;

  for vIndex := 0 to pJson.Count - 1 do
  begin
    vScheme := AddScheme(pJson.Pairs[vIndex].JsonString.Value, []);
    if pJson.Pairs[vIndex].JsonValue is TJSONArray then
    begin
      vJsonScopes := TJSONArray(pJson.Pairs[vIndex].JsonValue);
      for vScopeIndex := 0 to vJsonScopes.Count - 1 do
        vScheme.Scopes.Add(vJsonScopes.Items[vScopeIndex].Value);
    end;
  end;
end;

class function TSwagSecurityRequirement.GenerateJsonArray(pRequirements: TObjectList<TSwagSecurityRequirement>): TJSONArray;
var
  vRequirement: TSwagSecurityRequirement;
begin
  Result := TJSONArray.Create;
  for vRequirement in pRequirements do
    Result.AddElement(vRequirement.GenerateJsonObject);
end;

class procedure TSwagSecurityRequirement.LoadArray(pJson: TJSONArray; pRequirements: TObjectList<TSwagSecurityRequirement>);
var
  vIndex: Integer;
  vRequirement: TSwagSecurityRequirement;
begin
  if not Assigned(pJson) then
    Exit;

  for vIndex := 0 to pJson.Count - 1 do
  begin
    if not (pJson.Items[vIndex] is TJSONObject) then
      Continue;

    vRequirement := TSwagSecurityRequirement.Create;
    vRequirement.Load(TJSONObject(pJson.Items[vIndex]));
    pRequirements.Add(vRequirement);
  end;
end;

end.
