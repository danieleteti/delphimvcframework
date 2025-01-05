// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
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

unit WebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  Data.DB,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  FireDAC.Phys.IBBase,
  FireDAC.Phys.IB,
  FireDAC.Phys.FB,
  FireDAC.Phys.FBDef;

type
  TWebModule1 = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
  private
    DMVC: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}


uses
  MVCFramework.Commons,
  RenderSampleControllerU,
  CustomTypesU,
  CustomTypesSerializersU,
  MVCFramework.Serializer.Intf,
  System.Generics.Collections,
  MVCFramework.View.Renderers.Mustache,
  MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes,
  MVCFramework.Logger;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  DMVC := TMVCEngine.Create(self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.ViewPath] := 'templates';
    end);

  // Now the default json serializer is set, but how to set a different
  // serializer? Check the lines below!
  // ------------------------------------
  // DMVC
  // .Serializers
  // .AddOrSetValue(TMVCMediaType.APPLICATION_JSON, TMVCJSONSerializer.Create);
  // ------------------------------------

  DMVC.AddController(TRenderSampleController);
  DMVC.SetViewEngine(TMVCMustacheViewEngine);

  // Register a custom serializer for TUserRoles (is compatible only with the default serializer)
   DMVC
     .Serializer(TMVCMediaType.APPLICATION_JSON)
     .RegisterTypeSerializer(TypeInfo(TUserRoles), TUserRolesSerializer.Create);

   DMVC
     .Serializer(TMVCMediaType.APPLICATION_JSON)
     .RegisterTypeSerializer(TypeInfo(TSysUser), TSysUserSerializer.Create);

  // You can check how this custom type serializer works
  // calling http://localhost:8080/customserializationtype

  DMVC
    .Serializer(TMVCMediaType.APPLICATION_JSON)
    .RegisterTypeSerializer(TypeInfo(TNullableRecordAlias), TNullableAliasSerializer.Create);

  // This line registers custom serializers for TBitmap, TPngImage (Only MSWindows) and TJPEGImage (Only MSWindows)
  RegisterOptionalCustomTypesSerializers(DMVC.Serializer(TMVCMediaType.APPLICATION_JSON));
end;

end.
