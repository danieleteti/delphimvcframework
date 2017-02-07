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
  MVCFramework.TypesAliases, MVCFramework.Serializer.Intf, FrameworkTestsU;

type
  TTestJSONSerializer = class(TMVCSerUnSerTestCase)
  protected
    procedure Setup; override;
  published
    procedure TestSerUnSerObject; override;
    procedure TestSerUnSerObjectList; override;
  end;

implementation

{$WARN SYMBOL_DEPRECATED OFF}


uses BOs, MVCFramework.Serializer.JSON, MVCFramework.DuckTyping;

{ TTestJSONSerializer }

procedure TTestJSONSerializer.Setup;
begin
  SetSerUnSer(TMVCJSONSerUnSer.Create);
end;

procedure TTestJSONSerializer.TestSerUnSerObject;
var
  Obj: TMyObject;
  JSON: string;
  Obj2: TMyObject;
begin
  Obj := GetMyObject;
  try
    JSON := SerUnSer.SerializeObject(Obj, []);
    Obj2 := TMyObject.Create;
    try
      SerUnSer.DeserializeObject(JSON, Obj2);
      CheckTrue(Obj.Equals(Obj2));
    finally
      Obj2.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestJSONSerializer.TestSerUnSerObjectList;
var
  ObjList, Obj2List: TObjectList<TMyObject>;
  lJSON: String;
  I: Integer;
begin
  ObjList := GetObjectsList;
  try
    lJSON := SerUnSer.SerializeCollection(ObjList, []);
    Obj2List := TObjectList<TMyObject>.Create(True);
    try
      SerUnSer.DeserializeCollection(lJSON, WrapAsList(Obj2List), TMyObject);
      CheckEquals(ObjList.Count, Obj2List.Count);
      for I := 0 to 9 do
      begin
        CheckTrue(Obj2List[I].Equals(ObjList[I]));
      end;
    finally
      Obj2List.Free;
    end;
  finally
    ObjList.Free;
  end;
end;

initialization

RegisterTest(TTestJSONSerializer.suite);

finalization

end.
