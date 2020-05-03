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

unit Swag.Doc.Info.Contact;

interface

uses
  System.JSON;

type
  /// <summary>
  /// Contact information for the exposed API.
  /// </summary>
  TSwagInfoContact = class(TObject)
  private
    fName: string;
    fEmail: string;
    fUrl: string;
  public
    function GenerateJsonObject: TJSONObject;
    function IsEmpty: Boolean;
    procedure Load(pJson: TJSONObject);

    /// <summary>
    /// The identifying name of the contact person/organization.
    /// </summary>
    property Name: string read fName write fName;

    /// <summary>
    /// The URL pointing to the contact information. MUST be in the format of a URL.
    /// </summary>
    property Email: string read fEmail write fEmail;

    /// <summary>
    /// The email address of the contact person/organization. MUST be in the format of an email address.
    /// </summary>
    property Url: string read fUrl write fUrl;
  end;

implementation

uses System.SysUtils;

const
  c_SwagInfoContactName = 'name';
  c_SwagInfoContactEmail = 'email';
  c_SwagInfoContactUrl = 'url';

{ TSwagInfoContact }

function TSwagInfoContact.GenerateJsonObject: TJSONObject;
begin
  Result := TJsonObject.Create;
  Result.AddPair(c_SwagInfoContactName, fName);
  Result.AddPair(c_SwagInfoContactEmail, fEmail);
  Result.AddPair(c_SwagInfoContactUrl, fUrl);
end;

function TSwagInfoContact.IsEmpty: Boolean;
begin
  Result := fName.IsEmpty and fEmail.IsEmpty and fUrl.IsEmpty;
end;

procedure TSwagInfoContact.Load(pJson: TJSONObject);
begin
  if Assigned(pJson.Values[c_SwagInfoContactName]) then
    fName := pJson.Values[c_SwagInfoContactName].Value;

  if Assigned(pJson.Values[c_SwagInfoContactEmail]) then
    fEmail := pJson.Values[c_SwagInfoContactEmail].Value;

  if Assigned(pJson.Values[c_SwagInfoContactUrl]) then
    fUrl := pJson.Values[c_SwagInfoContactUrl].Value;
end;

end.
