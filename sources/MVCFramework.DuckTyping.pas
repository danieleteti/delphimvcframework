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
// ***************************************************************************

unit MVCFramework.DuckTyping;

{$LEGACYIFEND ON}

interface

uses
  RTTI,
  Classes,
  // superobject,
  Generics.Collections,
  SysUtils,
  TypInfo;

type
  TDuckTypedList = class;

  IList = interface
    ['{2A1BCB3C-17A2-4F8D-B6FB-32B2A1BFE840}']
    function Add(const Value: TObject): Integer;
    procedure Clear;
    function Count: Integer;
    function GetItem(index: Integer): TObject;
  end;

  TDuckListEnumerator = class(TEnumerator<TObject>)
  protected
    FPosition: Int64;
    FDuckTypedList: TDuckTypedList;

  protected
    function DoGetCurrent: TObject; override;
    function DoMoveNext: boolean; override;

  public
    constructor Create(ADuckTypedList: TDuckTypedList);
  end;

  TSortingType = (soAscending, soDescending);

  IWrappedList = interface
    ['{B60AF5A6-7C31-4EAA-8DFB-D8BD3E112EE7}']
    function Count: Integer;
    function GetItem(const index: Integer): TObject;
    procedure Add(const AObject: TObject);
    procedure Clear;
    function GetEnumerator: TDuckListEnumerator;
    function WrappedObject: TObject;
    procedure Sort(const PropertyName: string;
      Order: TSortingType = soAscending);
    function GetOwnsObjects: boolean;
    procedure SetOwnsObjects(const Value: boolean);
    property OwnsObjects: boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  TDuckTypedList = class(TInterfacedObject, IWrappedList)
  private
    FOwnsObject: boolean;
  protected
    FCTX: TRTTIContext;
    FObjectAsDuck: TObject;
    FAddMethod: TRttiMethod;
    FClearMethod: TRttiMethod;
    FCountProperty: TRttiProperty;
    FGetItemMethod: TRttiMethod;
    FGetCountMethod: TRttiMethod;
    function Count: Integer;
    function GetItem(const index: Integer): TObject;
    procedure Add(const AObject: TObject);
    procedure Clear;
    function WrappedObject: TObject;
    procedure QuickSort(List: IWrappedList; L, R: Integer;
      SCompare: TFunc<TObject, TObject, Integer>); overload;

    procedure QuickSort(List: IWrappedList;
      SCompare: TFunc<TObject, TObject, Integer>); overload;
    procedure Sort(const PropertyName: string;
      Order: TSortingType = soAscending);

  public
    constructor Create(AObjectAsDuck: TObject; aOwnsObject: boolean = false);
    destructor Destroy; override;
    function GetEnumerator: TDuckListEnumerator;
    function GetOwnsObjects: boolean;
    procedure SetOwnsObjects(const Value: boolean);
    property OwnsObjects: boolean read GetOwnsObjects write SetOwnsObjects;
    class function CanBeWrappedAsList(const AObjectAsDuck: TObject)
      : boolean; overload;
    class function CanBeWrappedAsList(const AInterfaceAsDuck: IInterface)
      : boolean; overload;
  end;

function WrapAsList(const AObject: TObject; aOwnsObject: boolean = false)
  : IWrappedList;

implementation

uses System.Math,
  MVCFramework.RTTIUtils, MVCFramework.Commons;

constructor TDuckListEnumerator.Create(ADuckTypedList: TDuckTypedList);
begin
  inherited Create;
  FDuckTypedList := ADuckTypedList;
  FPosition := -1;
end;

function TDuckListEnumerator.DoGetCurrent: TObject;
begin
  if FPosition > -1 then
    Result := FDuckTypedList.GetItem(FPosition)
  else
    raise Exception.Create('Enumerator error: Call MoveNext first');
end;

function TDuckListEnumerator.DoMoveNext: boolean;
begin
  if FPosition < FDuckTypedList.Count - 1 then
  begin
    Inc(FPosition);
    Result := True;
  end
  else
    Result := false;
end;

function TDuckTypedList.GetEnumerator: TDuckListEnumerator;
begin
  Result := TDuckListEnumerator.Create(self);
end;

procedure TDuckTypedList.Add(const AObject: TObject);
begin
  FAddMethod.Invoke(FObjectAsDuck, [AObject]);
end;

class function TDuckTypedList.CanBeWrappedAsList(const AObjectAsDuck
  : TObject): boolean;
var
  FCTX: TRTTIContext;
begin
  Result := (FCTX.GetType(AObjectAsDuck.ClassInfo).GetMethod('Add') <> nil) and
    (FCTX.GetType(AObjectAsDuck.ClassInfo).GetMethod('Clear') <> nil)

{$IF CompilerVersion >= 23}
    and (FCTX.GetType(AObjectAsDuck.ClassInfo).GetIndexedProperty('Items')
    .ReadMethod <> nil)

{$IFEND}
    and ((FCTX.GetType(AObjectAsDuck.ClassInfo).GetMethod('GetItem') <> nil) or
    (FCTX.GetType(AObjectAsDuck.ClassInfo).GetMethod('GetElement') <> nil)) and
    (FCTX.GetType(AObjectAsDuck.ClassInfo).GetProperty('Count') <> nil)

end;

class function TDuckTypedList.CanBeWrappedAsList(const AInterfaceAsDuck
  : IInterface): boolean;
var
  FCTX: TRTTIContext;
  LType: TRttiType;
begin
  LType := FCTX.GetType(TObject(AInterfaceAsDuck).ClassInfo);
  Result := (LType.GetMethod('Add') <> nil) and
    (LType.GetMethod('Clear') <> nil)

{$IF CompilerVersion >= 23}
    and (LType.GetIndexedProperty('Items').ReadMethod <> nil)

{$IFEND}
    and (LType.GetMethod('GetItem') <> nil) or
    (LType.GetMethod('GetElement') <> nil) and
    (LType.GetProperty('Count') <> nil)

end;

procedure TDuckTypedList.Clear;
begin
  FClearMethod.Invoke(FObjectAsDuck, []);
end;

function TDuckTypedList.Count: Integer;
begin
  if Assigned(FCountProperty) then
    Result := FCountProperty.GetValue(FObjectAsDuck).AsInteger
  else
    Result := FGetCountMethod.Invoke(FObjectAsDuck, []).AsInteger;

end;

constructor TDuckTypedList.Create(AObjectAsDuck: TObject; aOwnsObject: boolean);
begin
  inherited Create;
  FOwnsObject := aOwnsObject;
  FObjectAsDuck := AObjectAsDuck;
  FAddMethod := FCTX.GetType(AObjectAsDuck.ClassInfo).GetMethod('Add');
  if not Assigned(FAddMethod) then
    raise EMVCException.Create('Cannot find method "Add" in the duck object');
  FClearMethod := FCTX.GetType(AObjectAsDuck.ClassInfo).GetMethod('Clear');
  if not Assigned(FClearMethod) then
    raise EMVCException.Create('Cannot find method "Clear" in the duck object');
  FGetItemMethod := nil;

{$IF CompilerVersion >= 23}
  FGetItemMethod := FCTX.GetType(AObjectAsDuck.ClassInfo)
    .GetIndexedProperty('Items').ReadMethod;

{$IFEND}
  if not Assigned(FGetItemMethod) then
    FGetItemMethod := FCTX.GetType(AObjectAsDuck.ClassInfo)
      .GetMethod('GetItem');
  if not Assigned(FGetItemMethod) then
    FGetItemMethod := FCTX.GetType(AObjectAsDuck.ClassInfo)
      .GetMethod('GetElement');
  if not Assigned(FGetItemMethod) then
    raise EMVCException.Create
      ('Cannot find method Indexed property "Items" or method "GetItem" or method "GetElement" in the duck object');
  FCountProperty := FCTX.GetType(AObjectAsDuck.ClassInfo).GetProperty('Count');
  if not Assigned(FCountProperty) then
  begin
    FGetCountMethod := FCTX.GetType(AObjectAsDuck.ClassInfo).GetMethod('Count');
    if not Assigned(FGetCountMethod) then

      raise EMVCException.Create
        ('Cannot find property/method "Count" in the duck object');
  end;
end;

destructor TDuckTypedList.Destroy;
begin
  if FOwnsObject then
    FreeAndNil(FObjectAsDuck);
  inherited;
end;

function TDuckTypedList.GetItem(const index: Integer): TObject;
begin
  Result := FGetItemMethod.Invoke(FObjectAsDuck, [index]).AsObject;
end;

function TDuckTypedList.GetOwnsObjects: boolean;
begin
  Result := TRTTIUtils.GetProperty(FObjectAsDuck, 'OwnsObjects').AsBoolean
end;

function TDuckTypedList.WrappedObject: TObject;
begin
  Result := FObjectAsDuck;
end;

function WrapAsList(const AObject: TObject; aOwnsObject: boolean): IWrappedList;
begin
  try
    Result := TDuckTypedList.Create(AObject, aOwnsObject);
  except
    Result := nil;
  end;
end;

procedure TDuckTypedList.QuickSort(List: IWrappedList; L, R: Integer;
  SCompare: TFunc<TObject, TObject, Integer>);
var
  I, J: Integer;
  p: TObject;
begin
  { 07/08/2013: This method is based on QuickSort procedure from
    Classes.pas, (c) Borland Software Corp.
    but modified to be part of TDuckListU unit.  It implements the
    standard quicksort algorithm,
    delegating comparison operation to an anonimous.
    The Borland version delegates to a pure function
    pointer, which is problematic in some cases. }
  repeat
    I := L;
    J := R;
    p := List.GetItem((L + R) shr 1);
    repeat
      while SCompare(TObject(List.GetItem(I)), p) < 0 do
        Inc(I);
      while SCompare(TObject(List.GetItem(J)), p) > 0 do
        Dec(J);
      if I <= J then
      begin
        TRTTIUtils.MethodCall(List.WrappedObject, 'Exchange', [I, J]);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(List, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TDuckTypedList.QuickSort(List: IWrappedList;
  SCompare: TFunc<TObject, TObject, Integer>);
begin
  QuickSort(List, 0, List.Count - 1, SCompare);
end;

function CompareValue(const Left, Right: TValue): Integer;
begin
  if Left.IsOrdinal then
  begin
    Result := System.Math.CompareValue(Left.AsOrdinal, Right.AsOrdinal);
  end
  else if Left.Kind = tkFloat then
  begin
    Result := System.Math.CompareValue(Left.AsExtended, Right.AsExtended);
  end
  else if Left.Kind in [tkString, tkUString, tkWString, tkLString] then
  begin
    Result := CompareText(Left.AsString, Right.AsString);
  end
  else
  begin
    Result := 0;
  end;
end;

procedure TDuckTypedList.SetOwnsObjects(const Value: boolean);
begin
  TRTTIUtils.SetProperty(FObjectAsDuck, 'OwnsObjects', Value);
end;

procedure TDuckTypedList.Sort(const PropertyName: string; Order: TSortingType);
begin
  if Order = soAscending then
    QuickSort(self,
      function(Left, Right: TObject): Integer
      begin
        Result := CompareValue(TRTTIUtils.GetProperty(Left, PropertyName),
          TRTTIUtils.GetProperty(Right, PropertyName));
      end)
  else
    QuickSort(self,
      function(Left, Right: TObject): Integer
      begin
        Result := -1 * CompareValue(TRTTIUtils.GetProperty(Left, PropertyName),
          TRTTIUtils.GetProperty(Right, PropertyName));
      end);
end;

end.
