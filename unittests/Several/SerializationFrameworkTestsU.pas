// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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
// *************************************************************************** }

unit SerializationFrameworkTestsU;

interface


uses
  TestFramework,
  MVCFramework.Router,
  System.Generics.Collections,
  MVCFramework, Data.DB, System.SysUtils, MVCFramework.JWT,
  MVCFramework.TypesAliases, MVCFramework.Serializer.Intf;

type
  TTestJSONSerializer = class(TTestCase)
  protected
    FSerUnSer: IMVCSerUnSer;
    procedure Setup; override;
  published
    procedure TestObjectToJSONObject;
  end;

implementation

{$WARN SYMBOL_DEPRECATED OFF}


uses BOs, MVCFramework.Serializer.JSON;

{ TTestJSONSerializer }

procedure TTestJSONSerializer.Setup;
begin
  FSerUnSer := TMVCJSONSerUnSer.Create;
end;

procedure TTestJSONSerializer.TestObjectToJSONObject;
var
  Obj: TMyObject;
  JSON: string;
  Obj2: TMyObject;
begin
  Obj := GetMyObject;
  try
    JSON := FSerUnSer.SerializeObject(Obj, []);
    Obj2 := TMyObject.Create;
    try
      FSerUnSer.DeserializeObject(JSON, Obj2);
      CheckTrue(Obj.Equals(Obj2));
    finally
      Obj2.Free;
    end;
  finally
    Obj.Free;
  end;
end;

initialization

RegisterTest(TTestJSONSerializer.suite);

finalization

end.
