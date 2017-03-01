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
  System.Rtti,
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.TypInfo,
  System.Math;

type

  EMVCDuckTypingException = class(Exception);

  TDuckTypedList = class;

  IList = interface
    ['{2A1BCB3C-17A2-4F8D-B6FB-32B2A1BFE840}']
    function Add(const AValue: TObject): Integer;
    procedure Clear;
    function Count: Integer;
    function GetItem(AIndex: Integer): TObject;
  end;

  TDuckListEnumerator = class(TEnumerator<TObject>)
  private
    FPosition: Int64;
    FDuckTypedList: TDuckTypedList;
  protected
    function DoGetCurrent: TObject; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(const ADuckTypedList: TDuckTypedList);
  end;

  TSortingType = (soAscending, soDescending);

  IMVCList = interface
    ['{35BFF7E7-7CDA-4DCF-8618-33B9E92EA7CA}']
    function GetItem(const AIndex: Integer): TObject;
    function GetEnumerator: TDuckListEnumerator;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const AValue: Boolean);

    function Count: Integer;
    procedure Add(const AObject: TObject);
    procedure Clear;
    function WrappedObject: TObject;
    procedure Sort(const APropertyName: string; AOrder: TSortingType = soAscending);

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  IWrappedList = IMVCList;

  TDuckTypedList = class(TInterfacedObject, IMVCList)
  private
  class var
    GlContext: TRttiContext;
    GlObjectAsDuck: TObject;
    GlAddMethod: TRttiMethod;
    GlClearMethod: TRttiMethod;
    GlCountProperty: TRttiProperty;
    GlGetItemMethod: TRttiMethod;
    GlGetCountMethod: TRttiMethod;
  private
    class constructor Create;
    class destructor Destroy;
  private
    FOwnsObject: Boolean;
    class procedure ClearRttiData;
  protected
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const AValue: Boolean);
    function GetEnumerator: TDuckListEnumerator;
    function Count: Integer;
    function GetItem(const AIndex: Integer): TObject;
    procedure Add(const AObject: TObject);
    procedure Clear;
    procedure QuickSort(AList: IMVCList; L, R: Integer; ACompare: TFunc<TObject, TObject, Integer>); overload;
    procedure QuickSort(AList: IMVCList; ACompare: TFunc<TObject, TObject, Integer>); overload;
    procedure Sort(const APropertyName: string; AOrder: TSortingType = soAscending);
    function WrappedObject: TObject;
  public
    constructor Create(const AObjectAsDuck: TObject; AOwnsObject: Boolean = False);
    destructor Destroy; override;

    class function CanBeWrappedAsList(const AObjectAsDuck: TObject): Boolean; overload;
    class function CanBeWrappedAsList(const AInterfaceAsDuck: IInterface): Boolean; overload;
    class function Wrap(const AObjectAsDuck: TObject): IMVCList;
  end;

function WrapAsList(const AObject: TObject; AOwnsObject: Boolean = False): IMVCList;

implementation

uses
  MVCFramework.RttiUtils,
  MVCFramework.Commons;

{ TDuckListEnumerator }

constructor TDuckListEnumerator.Create(const ADuckTypedList: TDuckTypedList);
begin
  inherited Create;
  FDuckTypedList := ADuckTypedList;
  FPosition := -1;
end;

function TDuckListEnumerator.DoGetCurrent: TObject;
begin
  if (FPosition > -1) then
    Result := FDuckTypedList.GetItem(FPosition)
  else
    raise EMVCDuckTypingException.Create('Enumerator error: Call MoveNext first');
end;

function TDuckListEnumerator.DoMoveNext: Boolean;
begin
  if (FPosition < FDuckTypedList.Count - 1) then
  begin
    Inc(FPosition);
    Result := True;
  end
  else
    Result := False;
end;

{ TDuckTypedList }

function TDuckTypedList.GetEnumerator: TDuckListEnumerator;
begin
  Result := TDuckListEnumerator.Create(Self);
end;

procedure TDuckTypedList.Add(const AObject: TObject);
begin
  if Assigned(GlAddMethod) then
    GlAddMethod.Invoke(GlObjectAsDuck, [AObject]);
end;

class function TDuckTypedList.CanBeWrappedAsList(const AObjectAsDuck: TObject): Boolean;
var
  ListObjType: TRttiType;
begin
  ListObjType := GlContext.GetType(AObjectAsDuck.ClassInfo);

  Result := (ListObjType.GetMethod('Add') <> nil) and (ListObjType.GetMethod('Clear') <> nil)

  {$IF CompilerVersion >= 23}

    and (ListObjType.GetIndexedProperty('Items') <> nil) and (ListObjType.GetIndexedProperty('Items').ReadMethod <> nil)

  {$IFEND}

    and (ListObjType.GetMethod('GetItem') <> nil) or (ListObjType.GetMethod('GetElement') <> nil) and (ListObjType.GetProperty('Count') <> nil);
end;

class function TDuckTypedList.CanBeWrappedAsList(const AInterfaceAsDuck: IInterface): Boolean;
var
  ListObjType: TRttiType;
begin
  ListObjType := GlContext.GetType(TObject(AInterfaceAsDuck).ClassInfo);

  Result := (ListObjType.GetMethod('Add') <> nil) and (ListObjType.GetMethod('Clear') <> nil)

  {$IF CompilerVersion >= 23}

    and (ListObjType.GetIndexedProperty('Items') <> nil) and (ListObjType.GetIndexedProperty('Items').ReadMethod <> nil)

  {$IFEND}

    and (ListObjType.GetMethod('GetItem') <> nil) or (ListObjType.GetMethod('GetElement') <> nil) and (ListObjType.GetProperty('Count') <> nil)
end;

procedure TDuckTypedList.Clear;
begin
  if Assigned(GlClearMethod) then
    GlClearMethod.Invoke(GlObjectAsDuck, []);
end;

class procedure TDuckTypedList.ClearRttiData;
begin
  GlObjectAsDuck := nil;
  GlAddMethod := nil;
  GlClearMethod := nil;
  GlCountProperty := nil;
  GlGetItemMethod := nil;
  GlGetCountMethod := nil;
end;

function TDuckTypedList.Count: Integer;
begin
  Result := 0;
  if Assigned(GlCountProperty) then
    Result := GlCountProperty.GetValue(GlObjectAsDuck).AsInteger
  else if Assigned(GlGetCountMethod) then
    Result := GlGetCountMethod.Invoke(GlObjectAsDuck, []).AsInteger;
end;

class constructor TDuckTypedList.Create;
begin
  GlContext := TRttiContext.Create;
  ClearRttiData;
end;

constructor TDuckTypedList.Create(const AObjectAsDuck: TObject; AOwnsObject: Boolean);
begin
  inherited Create;
  FOwnsObject := AOwnsObject;

  ClearRttiData;

  GlObjectAsDuck := AObjectAsDuck;

  GlAddMethod := GlContext.GetType(AObjectAsDuck.ClassInfo).GetMethod('Add');
  if not Assigned(GlAddMethod) then
    raise EMVCDuckTypingException.Create('Cannot find method "Add" in the duck object');

  GlClearMethod := GlContext.GetType(AObjectAsDuck.ClassInfo).GetMethod('Clear');
  if not Assigned(GlClearMethod) then
    raise EMVCDuckTypingException.Create('Cannot find method "Clear" in the duck object');

  {$IF CompilerVersion >= 23}

  GlGetItemMethod := GlContext.GetType(AObjectAsDuck.ClassInfo).GetIndexedProperty('Items').ReadMethod;

  {$IFEND}

  if not Assigned(GlGetItemMethod) then
    GlGetItemMethod := GlContext.GetType(AObjectAsDuck.ClassInfo).GetMethod('GetItem');

  if not Assigned(GlGetItemMethod) then
    GlGetItemMethod := GlContext.GetType(AObjectAsDuck.ClassInfo).GetMethod('GetElement');

  if not Assigned(GlGetItemMethod) then
    raise EMVCDuckTypingException.Create('Cannot find method Indexed property "Items" or method "GetItem" or method "GetElement" in the duck object');

  GlCountProperty := GlContext.GetType(AObjectAsDuck.ClassInfo).GetProperty('Count');
  if not Assigned(GlCountProperty) then
  begin
    GlGetCountMethod := GlContext.GetType(AObjectAsDuck.ClassInfo).GetMethod('Count');
    if not Assigned(GlGetCountMethod) then
      raise EMVCDuckTypingException.Create('Cannot find property/method "Count" in the duck object');
  end;
end;

class destructor TDuckTypedList.Destroy;
begin
  GlContext.Free;
end;

destructor TDuckTypedList.Destroy;
begin
  if FOwnsObject and Assigned(GlObjectAsDuck) then
    FreeAndNil(GlObjectAsDuck);
  inherited;
end;

function TDuckTypedList.GetItem(const AIndex: Integer): TObject;
begin
  Result := nil;
  if Assigned(GlGetItemMethod) then
    Result := GlGetItemMethod.Invoke(GlObjectAsDuck, [AIndex]).AsObject;
end;

function TDuckTypedList.GetOwnsObjects: Boolean;
begin
  Result := False;
  if Assigned(GlObjectAsDuck) then
    Result := TRttiUtils.GetProperty(GlObjectAsDuck, 'OwnsObjects').AsBoolean
end;

class function TDuckTypedList.Wrap(const AObjectAsDuck: TObject): IMVCList;
var
  ObjType: TRttiType;

  {$IF CompilerVersion >= 23}

  IndexedProperty: TRttiIndexedProperty;

  {$IFEND}

begin
  ObjType := GlContext.GetType(AObjectAsDuck.ClassInfo);

  GlAddMethod := ObjType.GetMethod('Add');
  if not Assigned(GlAddMethod) then
    Exit(nil);

  GlClearMethod := ObjType.GetMethod('Clear');
  if not Assigned(GlClearMethod) then
    Exit(nil);

  GlGetItemMethod := nil;

  {$IF CompilerVersion >= 23}

  IndexedProperty := ObjType.GetIndexedProperty('Items');
  if IndexedProperty = nil then
    Exit(nil);
  GlGetItemMethod := IndexedProperty.ReadMethod;

  {$IFEND}

  if not Assigned(GlGetItemMethod) then
    GlGetItemMethod := ObjType.GetMethod('GetItem');

  if not Assigned(GlGetItemMethod) then
    GlGetItemMethod := ObjType.GetMethod('GetElement');

  if not Assigned(GlGetItemMethod) then
    Exit(nil);

  GlCountProperty := ObjType.GetProperty('Count');
  if not Assigned(GlCountProperty) then
  begin
    GlGetCountMethod := ObjType.GetMethod('Count');
    if not Assigned(GlGetCountMethod) then
      Exit(nil);
  end;

  Result := TDuckTypedList.Create(AObjectAsDuck);
end;

function TDuckTypedList.WrappedObject: TObject;
begin
  Result := GlObjectAsDuck;
end;

function WrapAsList(const AObject: TObject; AOwnsObject: Boolean): IMVCList;
begin
  try
    Result := TDuckTypedList.Create(AObject, AOwnsObject);
  except
    Result := nil;
  end;
end;

procedure TDuckTypedList.QuickSort(AList: IMVCList; L, R: Integer; ACompare: TFunc<TObject, TObject, Integer>);
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
    p := AList.GetItem((L + R) shr 1);
    repeat
      while ACompare(TObject(AList.GetItem(I)), p) < 0 do
        Inc(I);
      while ACompare(TObject(AList.GetItem(J)), p) > 0 do
        Dec(J);
      if I <= J then
      begin
        TRttiUtils.MethodCall(AList.WrappedObject, 'Exchange', [I, J]);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, ACompare);
    L := I;
  until I >= R;
end;

procedure TDuckTypedList.QuickSort(AList: IMVCList; ACompare: TFunc<TObject, TObject, Integer>);
begin
  QuickSort(AList, 0, AList.Count - 1, ACompare);
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

procedure TDuckTypedList.SetOwnsObjects(const AValue: Boolean);
begin
  TRttiUtils.SetProperty(GlObjectAsDuck, 'OwnsObjects', AValue);
end;

procedure TDuckTypedList.Sort(const APropertyName: string; AOrder: TSortingType);
begin
  if AOrder = soAscending then
    QuickSort(self,
      function(Left, Right: TObject): Integer
      begin
        Result := CompareValue(TRttiUtils.GetProperty(Left, APropertyName),
          TRttiUtils.GetProperty(Right, APropertyName));
      end)
  else
    QuickSort(self,
      function(Left, Right: TObject): Integer
      begin
        Result := -1 * CompareValue(TRttiUtils.GetProperty(Left, APropertyName),
          TRttiUtils.GetProperty(Right, APropertyName));
      end);
end;

end.
