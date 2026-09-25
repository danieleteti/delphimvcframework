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

unit Swag.Doc.Tags;

interface

uses
  System.SysUtils,
  System.JSON,
  Swag.Common.Types,
  Swag.Doc.Extensions;

type
  /// <summary>
  /// Allows referencing an external resource for extended documentation.
  /// </summary>
  TSwagExternalDocs = class(TObject)
  strict private
    fDescription: string;
    fUrl: string;
    fExtensions: TSwagExtensions;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONObject);

    property Description: string read fDescription write fDescription;
    property Url: string read fUrl write FUrl;

    /// <summary>
    /// The Specification Extensions of the external documentation object.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

  /// <summary>
  /// Adds metadata to a single tag that is used by the operations. Tags group the operations in the documentation.
  /// In OpenAPI 3 the tags can also be nested and categorized.
  /// </summary>
  TSwagTag = class(TObject)
  strict private
    fName: string;
    fSummary: string;
    fDescription: string;
    fExternalDocs: TSwagExternalDocs;
    fParent: string;
    fKind: string;
    fExtensions: TSwagExtensions;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Generates the Swagger 2.0 tag object.
    /// </summary>
    function GenerateJsonObject: TJSONObject; overload;

    /// <summary>
    /// Generates the tag object for the given specification family. The OpenAPI 3 object also has the summary,
    /// parent and kind fields.
    /// </summary>
    function GenerateJsonObject(const pVersion: TSwagVersion): TJSONObject; overload;

    procedure Load(pJson: TJSONObject);

    property Name: string read fName write fName;

    /// <summary>
    /// A short summary of the tag, used for display purposes. Available in OpenAPI 3 only.
    /// </summary>
    property Summary: string read fSummary write fSummary;

    property Description: string read fDescription write fDescription;
    property ExternalDocs: TSwagExternalDocs read fExternalDocs write fExternalDocs;

    /// <summary>
    /// The name of a tag that this tag is nested under. The named tag MUST exist in the document and circular
    /// references between parent and child tags MUST NOT be used. Available in OpenAPI 3 only.
    /// </summary>
    property Parent: string read fParent write fParent;

    /// <summary>
    /// A machine-readable string to categorize what sort of tag it is. Common values are nav for navigation,
    /// badge for visible badges and audience for APIs used by different groups. Available in OpenAPI 3 only.
    /// </summary>
    property Kind: string read fKind write fKind;

    /// <summary>
    /// The Specification Extensions of the tag.
    /// </summary>
    property Extensions: TSwagExtensions read fExtensions;
  end;

implementation

const
  c_SwagTagName = 'name';
  c_SwagTagSummary = 'summary';
  c_SwagTagDescription = 'description';
  c_SwagTagExternalDocs = 'externalDocs';
  c_SwagTagParent = 'parent';
  c_SwagTagKind = 'kind';
  c_SwagTagNameUrl = 'url';

{ TSwagTag }

constructor TSwagTag.Create;
begin
  inherited Create;
  fExternalDocs := TSwagExternalDocs.Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagTag.Destroy;
begin
  FreeAndNil(fExternalDocs);
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

function TSwagTag.GenerateJsonObject: TJSONObject;
var
  vExternalDocs: TJSONObject;
begin
  Result := TJsonObject.Create;
  if fName.Length > 0 then
    Result.AddPair(c_SwagTagName, fName);
  if fDescription.Length > 0 then
    Result.AddPair(c_SwagTagDescription, fDescription);

  vExternalDocs := fExternalDocs.GenerateJsonObject;
  if Assigned(vExternalDocs) then
    Result.AddPair(c_SwagTagExternalDocs, vExternalDocs);

  fExtensions.WriteTo(Result);
end;

function TSwagTag.GenerateJsonObject(const pVersion: TSwagVersion): TJSONObject;
var
  vExternalDocs: TJSONObject;
begin
  if pVersion <> svOpenApi3 then
    Exit(GenerateJsonObject);

  Result := TJsonObject.Create;
  if fName.Length > 0 then
    Result.AddPair(c_SwagTagName, fName);
  if fSummary.Length > 0 then
    Result.AddPair(c_SwagTagSummary, fSummary);
  if fDescription.Length > 0 then
    Result.AddPair(c_SwagTagDescription, fDescription);

  vExternalDocs := fExternalDocs.GenerateJsonObject;
  if Assigned(vExternalDocs) then
    Result.AddPair(c_SwagTagExternalDocs, vExternalDocs);

  if fParent.Length > 0 then
    Result.AddPair(c_SwagTagParent, fParent);
  if fKind.Length > 0 then
    Result.AddPair(c_SwagTagKind, fKind);

  fExtensions.WriteTo(Result);
end;

procedure TSwagTag.Load(pJson: TJSONObject);
begin
  if not Assigned(pJson) then
    Exit;
  if Assigned(pJson.Values[c_SwagTagDescription]) then
    fDescription := pJson.Values[c_SwagTagDescription].Value;
  if Assigned(pJson.Values[c_SwagTagName]) then
    fName := pJson.Values[c_SwagTagName].Value;
  if Assigned(pJson.Values[c_SwagTagSummary]) then
    fSummary := pJson.Values[c_SwagTagSummary].Value;
  if Assigned(pJson.Values[c_SwagTagParent]) then
    fParent := pJson.Values[c_SwagTagParent].Value;
  if Assigned(pJson.Values[c_SwagTagKind]) then
    fKind := pJson.Values[c_SwagTagKind].Value;
  if Assigned(pJson.Values[c_SwagTagExternalDocs]) then
    fExternalDocs.Load(pJson.Values[c_SwagTagExternalDocs] as TJSONObject);
  fExtensions.ReadFrom(pJson);
end;

{ TSwagExternalDocs }

constructor TSwagExternalDocs.Create;
begin
  inherited Create;
  fExtensions := TSwagExtensions.Create;
end;

destructor TSwagExternalDocs.Destroy;
begin
  FreeAndNil(fExtensions);
  inherited Destroy;
end;

function TSwagExternalDocs.GenerateJsonObject: TJSONObject;
begin
  Result := nil;
  if (fDescription.Length = 0) and (fUrl.Length = 0) and (fExtensions.Count = 0) then
    Exit;
  Result := TJsonObject.Create;
  if fDescription.Length > 0 then
    Result.AddPair(c_SwagTagDescription, fDescription);
  if fUrl.Length > 0 then
    Result.AddPair(c_SwagTagNameUrl, fUrl);
  fExtensions.WriteTo(Result);
end;

procedure TSwagExternalDocs.Load(pJson: TJSONObject);
begin
  if Assigned(pJson.Values[c_SwagTagDescription]) then
    fDescription := pJson.Values[c_SwagTagDescription].Value;
  if Assigned(pJson.Values[c_SwagTagNameUrl]) then
    fUrl := pJson.Values[c_SwagTagNameUrl].Value;
  fExtensions.ReadFrom(pJson);
end;

end.
