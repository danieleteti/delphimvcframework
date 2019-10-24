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

unit Swag.Common.Types.Helpers;

interface

uses
  Swag.Common.Types;

type
  TSwagPathTypeOperationHelper = record helper for TSwagPathTypeOperation
  public
    procedure ToType(const pHttpVerbString: string);
  end;

  TSwagRequestParameterInLocationHelper = record helper for TSwagRequestParameterInLocation
  public
    procedure ToType(const pInLocationString: string);
  end;

  TSwagTransferProtocolSchemeHelper = record helper for TSwagTransferProtocolScheme
  public
    procedure ToType(const pTransferProtocolSchemeString: string);
  end;

  TSwagSecurityDefinitionTypeHelper = record helper for TSwagSecurityDefinitionType
  public
    procedure ToType(const pSecurityDefinitionTypeString: string);
  end;

  TSwagTransferProtocolSchemesHelper = record helper for TSwagTransferProtocolSchemes
  public
    procedure Add(const pScheme: TSwagTransferProtocolScheme); overload;
    procedure Add(const pSchemeString: string); overload;
  end;

  TSwagTypeParameterHelper = record helper for TSwagTypeParameter
  public
    procedure ToType(const pTypeParameter: string);
  end;

implementation

uses
  System.SysUtils,
  Swag.Common.Consts;

{ TSwagPathTypeOperationHelper }

procedure TSwagPathTypeOperationHelper.ToType(const pHttpVerbString: string);
var
  vPathTypeOperation: TSwagPathTypeOperation;
begin
  Self := ohvNotDefined;
  for vPathTypeOperation := Low(TSwagPathTypeOperation) to High(TSwagPathTypeOperation) do
    if (LowerCase(c_SwagPathOperationHttpVerbs[vPathTypeOperation]) = LowerCase(pHttpVerbString)) then
    begin
      Self := vPathTypeOperation;
      Break;
    end;
end;

{ TSwagRequestParameterInLocationHelper }

procedure TSwagRequestParameterInLocationHelper.ToType(const pInLocationString: string);
var
  vRequestParameterInLocation: TSwagRequestParameterInLocation;
begin
  Self := rpiNotDefined;
  for vRequestParameterInLocation := Low(TSwagRequestParameterInLocation) to High(TSwagRequestParameterInLocation) do
    if (LowerCase(c_SwagRequestParameterInLocation[vRequestParameterInLocation]) = LowerCase(pInLocationString)) then
    begin
      Self := vRequestParameterInLocation;
      Break;
    end;
end;

{ TSwagTransferProtocolSchemeHelper }

procedure TSwagTransferProtocolSchemeHelper.ToType(const pTransferProtocolSchemeString: string);
var
  vTransferProtocolScheme: TSwagTransferProtocolScheme;
begin
  Self := tpsNotDefined;
  for vTransferProtocolScheme := Low(TSwagTransferProtocolScheme) to High(TSwagTransferProtocolScheme) do
    if (LowerCase(c_SwagTransferProtocolScheme[vTransferProtocolScheme]) = LowerCase(pTransferProtocolSchemeString)) then
    begin
      Self := vTransferProtocolScheme;
      Break;
    end;
end;

{ TSwagTransferProtocolSchemesHelper }

procedure TSwagTransferProtocolSchemesHelper.Add(const pScheme: TSwagTransferProtocolScheme);
begin
  Self := Self + [pScheme];
end;

procedure TSwagTransferProtocolSchemesHelper.Add(const pSchemeString: string);
var
  vScheme: TSwagTransferProtocolScheme;
begin
  vScheme.ToType(pSchemeString);
  Self.Add(vScheme);
end;

{ TSwagTypeParameterHelper }

procedure TSwagTypeParameterHelper.ToType(const pTypeParameter: string);
var
  vSwagTypeParameter: TSwagTypeParameter;
begin
  Self := stpNotDefined;
  for vSwagTypeParameter := Low(TSwagTypeParameter) to High(TSwagTypeParameter) do
    if (LowerCase(c_SwagTypeParameter[vSwagTypeParameter]) = LowerCase(pTypeParameter)) then
    begin
      Self := vSwagTypeParameter;
      Break;
    end;
end;

{ TSwagSecurityDefinitionTypeHelper }

procedure TSwagSecurityDefinitionTypeHelper.ToType(const pSecurityDefinitionTypeString: string);
var
  vSecurityDefinitionType: TSwagSecurityDefinitionType;
begin
  Self := ssdNotDefined;
  for vSecurityDefinitionType := Low(TSwagSecurityDefinitionType) to High(TSwagSecurityDefinitionType) do
    if (LowerCase(c_SwagSecurityDefinitionType[vSecurityDefinitionType]) = LowerCase(pSecurityDefinitionTypeString.Trim)) then
    begin
      Self := vSecurityDefinitionType;
      Break;
    end;
end;

end.
