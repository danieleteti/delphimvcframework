// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

unit MVCFramework.Tests.Serializer.EntitiesModule;

interface

uses
  System.SysUtils,
  System.Classes,
  Data.DB,
  Datasnap.DBClient,
  MVCFramework.Serializer.Commons,
  MidasLib;

type

  TEntitiesModule = class(TDataModule)
    Entity: TClientDataSet;
    EntityId: TLargeintField;
    EntityCode: TIntegerField;
    EntityName: TStringField;
    EntitySalary: TCurrencyField;
    EntityBirthday: TDateField;
    EntityAccessDateTime: TDateTimeField;
    EntityAccessTime: TTimeField;
    EntityActive: TBooleanField;
    EntityAmount: TFloatField;
    EntityIgnored: TStringField;
    EntityBlobFld: TBlobField;

    [MVCDoNotSerialize]
    EntityIgnoredAtt: TStringField;

    [MVCDataSetField(dtArray)]
    EntityItems: TDataSetField;
    Item: TClientDataSet;
    ItemId: TLargeintField;
    ItemName: TStringField;

    [MVCDataSetField(dtObject)]
    EntityDepartament: TDataSetField;
    Departament: TClientDataSet;
    DepartamentName: TStringField;

    [MVCNameCase(ncLowerCase)]
    EntityLowerCase: TClientDataSet;
    EntityLowerCaseId: TLargeintField;
    EntityLowerCaseName: TStringField;

    [MVCNameCase(ncUpperCase)]
    EntityUpperCase: TClientDataSet;
    EntityUpperCaseId: TLargeintField;
    EntityUpperCaseName: TStringField;

    EntityUpperCase2: TClientDataSet;
    EntityUpperCase2Id: TLargeintField;
    EntityUpperCase2Name: TStringField;

    EntityAsIs: TClientDataSet;

    [MVCNameAs('Id_Id')]
    EntityAsIsId: TLargeintField;

    [MVCNameAs('Name_Name')]
    EntityAsIsName: TStringField;
    EntityGUID: TGuidField;

    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{ %CLASSGROUP 'System.Classes.TPersistent' }

{$R *.dfm}

procedure TEntitiesModule.DataModuleCreate(Sender: TObject);
begin
  Entity.CreateDataSet;
  EntityLowerCase.CreateDataSet;
  EntityUpperCase.CreateDataSet;
  EntityUpperCase2.CreateDataSet;
  EntityAsIs.CreateDataSet;
end;

end.
