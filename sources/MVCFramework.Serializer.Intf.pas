// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
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

unit MVCFramework.Serializer.Intf;

{$I dmvcframework.inc}

interface

uses
  System.Rtti,
  System.TypInfo,
  Data.DB,
  MVCFramework.Serializer.Commons;

const
  DMVC_CLASSNAME = '$dmvc_classname';

type

  IMVCTypeSerializer = interface
    ['{806EC547-D1CB-4DA9-92D3-A8A7C0BD4009}']
    procedure Serialize(
      const AElementValue: TValue;
      var ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>
      );

    procedure Deserialize(
      const ASerializedObject: TObject;
      var AElementValue: TValue;
      const AAttributes: TArray<TCustomAttribute>
      );
  end;

  IMVCSerializer = interface
    ['{1ECA942A-E3C4-45DD-9D23-C00363B5E334}']
    procedure RegisterTypeSerializer(const ATypeInfo: PTypeInfo; AInstance: IMVCTypeSerializer);

    function SerializeObject(
      const AObject: TObject;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []
      ): string;

    function SerializeCollection(
      const AList: TObject;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []
      ): string;

    function SerializeDataSet(
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs
      ): string;

    function SerializeDataSetRecord(
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs
      ): string;

    procedure DeserializeObject(
      const ASerializedObject: string;
      const AObject: TObject;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []
      );

    procedure DeserializeCollection(
      const ASerializedList: string;
      const AList: TObject;
      const AClazz: TClass;
      const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []
      );

    procedure DeserializeDataSet(
      const ASerializedDataSet: string;
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs
      );

    procedure DeserializeDataSetRecord(
      const ASerializedDataSetRecord: string;
      const ADataSet: TDataSet;
      const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs
      );
  end;

implementation

end.
