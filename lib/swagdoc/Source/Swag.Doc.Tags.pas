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
  System.JSON;

type
  TSwagExternalDocs = class(TObject)
  strict private
    fDescription: string;
    fUrl: string;
  public
    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONObject);

    property Description: string read fDescription write fDescription;
    property Url: string read fUrl write FUrl;
  end;

  TSwagTag = class(TObject)
  strict private
    fName: string;
    fDescription: string;
    fExternalDocs: TSwagExternalDocs;
  public
    constructor Create;
    destructor Destroy; override;

    function GenerateJsonObject: TJSONObject;
    procedure Load(pJson: TJSONObject);

    property Name: string read fName write fName;
    property Description: string read fDescription write fDescription;
    property ExternalDocs: TSwagExternalDocs read fExternalDocs write fExternalDocs;
  end;

implementation

const
  c_SwagTagName = 'name';
  c_SwagTagDescription = 'description';
  c_SwagTagExternalDocs = 'externalDocs';
  c_SwagTagNameUrl = 'url';

{ TSwagTag }

constructor TSwagTag.Create;
begin
  inherited Create;
  fExternalDocs := TSwagExternalDocs.Create;
end;

destructor TSwagTag.Destroy;
begin
  FreeAndNil(fExternalDocs);
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
end;

procedure TSwagTag.Load(pJson: TJSONObject);
begin
  if not Assigned(pJson) then
    Exit;
  if Assigned(pJson.Values[c_SwagTagDescription]) then
    fDescription := pJson.Values[c_SwagTagDescription].Value;
  if Assigned(pJson.Values[c_SwagTagName]) then
    fName := pJson.Values[c_SwagTagName].Value;
  if Assigned(pJson.Values[c_SwagTagExternalDocs]) then
    fExternalDocs.Load(pJson.Values[c_SwagTagExternalDocs] as TJSONObject);
end;

{ TSwagExternalDocs }

function TSwagExternalDocs.GenerateJsonObject: TJSONObject;
begin
  Result := nil;
  if (fDescription.Length = 0) and (fUrl.Length = 0) then
    Exit;
  Result := TJsonObject.Create;
  if fDescription.Length > 0 then
    Result.AddPair(c_SwagTagDescription, fDescription);
  if fUrl.Length > 0 then
    Result.AddPair(c_SwagTagNameUrl, fUrl);
end;

procedure TSwagExternalDocs.Load(pJson: TJSONObject);
begin
  if Assigned(pJson.Values[c_SwagTagDescription]) then
    fDescription := pJson.Values[c_SwagTagDescription].Value;
  if Assigned(pJson.Values[c_SwagTagNameUrl]) then
    fUrl := pJson.Values[c_SwagTagNameUrl].Value;
end;

end.
