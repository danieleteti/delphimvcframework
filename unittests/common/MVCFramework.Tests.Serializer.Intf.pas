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

unit MVCFramework.Tests.Serializer.Intf;

interface

type

  IMVCTestSerializer = interface
    ['{9043E0F4-0896-4388-B53C-1A0831A1E320}']
    { serialize declarations }
    procedure TestSerializeEntity;
    procedure TestSerializeEntityUpperCaseNames;
    procedure TestSerializeEntityLowerCaseNames;
    procedure TestSerializeEntityNameAs;
    procedure TestSerializeEntityCustomSerializer;
    procedure TestSerializeEntityCustomMemberSerializer;
    procedure TestSerializeEntitySerializationType;
    procedure TestSerializeCollection;
    procedure TestSerializeDataSet;
    { deserialize declarations }
    procedure TestDeserializeEntity;
    procedure TestDeserializeEntityCustomSerializer;
    procedure TestDeserializeEntityCustomMemberSerializer;
    procedure TestDeserializeEntitySerializationType;
    procedure TestDeserializeCollection;
    procedure TestDeserializeDataSet;
  end;

implementation

end.
