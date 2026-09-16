// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
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

unit ex03_softdelete;

interface

uses
  MVCFramework.Serializer.Commons,
  MVCFramework.Nullables,
  MVCFramework.ActiveRecord,
  MVCFramework.Validators,
  System.Classes;

type

  [MVCNameCase(ncLowerCase)]
  [MVCTable('soft_delete_demo')]
  TSoftDeleteDemo = class(TMVCActiveRecord)
  private
    [MVCTableField('id', [foPrimaryKey])]
    [MVCRequired]
    fID: Int32;
    [MVCTableField('description')]
    [MVCMaxLength(200)]
    fDescription: NullableString;
    [MVCTableField('deleted_at')]
    [MVCSoftDeleted]
    fDeletedAt: NullableTDateTime {dtDateTimeStamp};
  public
    property ID: Int32 read fID write fID;
    property Description: NullableString read fDescription write fDescription;
    property DeletedAt: NullableTDateTime {dtDateTimeStamp} read fDeletedAt write fDeletedAt;
  end;

implementation

end.