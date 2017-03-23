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

unit MVCFramework.DuckTyping;

{$I dmvcframework.inc}

{$LEGACYIFEND ON}

interface

uses
  System.Rtti,
  System.Generics.Collections,
  System.SysUtils,
  System.TypInfo,
  System.Math;

type

  EMVCDuckTypingException = class(Exception);

  TSortingType = (soAscending, soDescending);

  TDuckTypedList = class;

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

  IMVCList = interface
    ['{D5958EC5-60FF-4C7B-81AF-96312174E719}']
    function GetItem(const AIndex: Integer): TObject;
    function GetEnumerator: TDuckListEnumerator;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const AValue: Boolean);

    procedure Add(const AObject: TObject);
    function Count: Integer;
    procedure Clear;

    function IsWrappedList: Boolean; overload;

    function WrappedObject: TObject;
    procedure Sort(const APropertyName: string; const AOrder: TSortingType = soAscending);

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  IWrappedList = IMVCList;

  TDuckTypedList = class(TInterfacedObject, IMVCList)
  private
    FOwnsObject: Boolean;
    FObjectAsDuck: TObject;
    FContext: TRttiContext;
    FObjType: TRttiType;
    FAddMethod: TRttiMethod;
    FClearMethod: TRttiMethod;
    FCountProperty: TRttiProperty;
    FGetItemMethod: TRttiMethod;
    FGetCountMethod: TRttiMethod;
  protected
    function GetItem(const AIndex: Integer): TObject;
    function GetEnumerator: TDuckListEnumerator;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const AValue: Boolean);

    procedure Add(const AObject: TObject);
    function Count: Integer;
    procedure Clear;

    function WrappedObject: TObject;
    procedure Sort(const APropertyName: string; const AOrder: TSortingType = soAscending);

    procedure QuickSort(const AList: IMVCList; ALeft, ARigth: Integer; ACompare: TFunc<TObject, TObject, Integer>); overload;
    procedure QuickSort(const AList: IMVCList; ACompare: TFunc<TObject, TObject, Integer>); overload;
  public
    constructor Create(const AObjectAsDuck: TObject; const AOwnsObject: Boolean = False); overload;
    constructor Create(const AInterfaceAsDuck: IInterface; const AOwnsObject: Boolean = False); overload;
    destructor Destroy; override;

    function IsWrappedList: Boolean; overload;

    class function CanBeWrappedAsList(const AObjectAsDuck: TObject): Boolean; overload; static;
    class function CanBeWrappedAsList(const AInterfaceAsDuck: IInterface): Boolean; overload; static;
    class function Wrap(const AObjectAsDuck: TObject; const AOwnsObject: Boolean = False): IMVCList; static;
  end;

function WrapAsList(const AObject: TObject; AOwnsObject: Boolean = False): IMVCList;

implementation

function WrapAsList(const AObject: TObject; AOwnsObject: Boolean = False): IMVCList;
begin
  Result := TDuckTypedList.Wrap(AObject, AOwnsObject);
end;

function CompareValue(const ALeft, ARight: TValue): Integer;
begin
  if ALeft.IsOrdinal then
  begin
    Result := System.Math.CompareValue(ALeft.AsOrdinal, ARight.AsOrdinal);
  end
  else if ALeft.Kind = tkFloat then
  begin
    Result := System.Math.CompareValue(ALeft.AsExtended, ARight.AsExtended);
  end
  else if ALeft.Kind in [tkString, tkUString, tkWString, tkLString] then
  begin
    Result := CompareText(ALeft.AsString, ARight.AsString);
  end
  else
  begin
    Result := 0;
  end;
end;

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
    raise EMVCDuckTypingException.Create('TDuckListEnumerator exception: call MoveNext first.');
end;

function TDuckListEnumerator.DoMoveNext: Boolean;
begin
  Result := False;
  if (FPosition < FDuckTypedList.Count - 1) then
  begin
    Inc(FPosition);
    Result := True;
  end;
end;

{ TDuckTypedList }

procedure TDuckTypedList.Add(const AObject: TObject);
begin
  if not Assigned(FAddMethod) then
    raise EMVCDuckTypingException.Create('Cannot find method "Add" in the Duck Object.');
  FAddMethod.Invoke(FObjectAsDuck, [AObject]);
end;

class function TDuckTypedList.CanBeWrappedAsList(const AInterfaceAsDuck: IInterface): Boolean;
begin
  Result := CanBeWrappedAsList(TObject(AInterfaceAsDuck));
end;

class function TDuckTypedList.CanBeWrappedAsList(const AObjectAsDuck: TObject): Boolean;
var
  List: IMVCList;
begin
  List := TDuckTypedList.Create(AObjectAsDuck);
  Result := List.IsWrappedList;
end;

procedure TDuckTypedList.Clear;
begin
  if not Assigned(FClearMethod) then
    raise EMVCDuckTypingException.Create('Cannot find method "Clear" in the Duck Object.');
  FClearMethod.Invoke(FObjectAsDuck, []);
end;

function TDuckTypedList.Count: Integer;
begin
  Result := 0;

  if (not Assigned(FGetCountMethod)) and (not Assigned(FCountProperty)) then
    raise EMVCDuckTypingException.Create('Cannot find property/method "Count" in the Duck Object.');

  if Assigned(FCountProperty) then
    Result := FCountProperty.GetValue(FObjectAsDuck).AsInteger
  else if Assigned(FGetCountMethod) then
    Result := FGetCountMethod.Invoke(FObjectAsDuck, []).AsInteger;
end;

constructor TDuckTypedList.Create(const AInterfaceAsDuck: IInterface; const AOwnsObject: Boolean);
begin
  Create(TObject(AInterfaceAsDuck), AOwnsObject);
end;

constructor TDuckTypedList.Create(const AObjectAsDuck: TObject; const AOwnsObject: Boolean);
begin
  inherited Create;
  FOwnsObject := AOwnsObject;
  FObjectAsDuck := AObjectAsDuck;

  if not Assigned(FObjectAsDuck) then
    raise EMVCDuckTypingException.Create('Duck Object can not be null.');

  FContext := TRttiContext.Create;
  FObjType := FContext.GetType(FObjectAsDuck.ClassInfo);

  FAddMethod := nil;
  FClearMethod := nil;
  FGetItemMethod := nil;
  FGetCountMethod := nil;
  FCountProperty := nil;

  if IsWrappedList then
  begin
    FAddMethod := FObjType.GetMethod('Add');
    FClearMethod := FObjType.GetMethod('Clear');

    {$IF CompilerVersion >= 23}

    if Assigned(FObjType.GetIndexedProperty('Items')) then
      FGetItemMethod := FObjType.GetIndexedProperty('Items').ReadMethod;

    {$IFEND}

    if not Assigned(FGetItemMethod) then
      FGetItemMethod := FObjType.GetMethod('GetItem');

    if not Assigned(FGetItemMethod) then
      FGetItemMethod := FObjType.GetMethod('GetElement');

    FGetCountMethod := nil;
    FCountProperty := FObjType.GetProperty('Count');
    if not Assigned(FCountProperty) then
      FGetCountMethod := FObjType.GetMethod('Count');
  end;
end;

destructor TDuckTypedList.Destroy;
begin
  if FOwnsObject and Assigned(FObjectAsDuck) then
    FObjectAsDuck.Free;
  FContext.Free;
  inherited Destroy;
end;

function TDuckTypedList.GetEnumerator: TDuckListEnumerator;
begin
  Result := TDuckListEnumerator.Create(Self);
end;

function TDuckTypedList.GetItem(const AIndex: Integer): TObject;
begin
  if not Assigned(FGetItemMethod) then
    raise EMVCDuckTypingException.Create('Cannot find method Indexed property "Items" or method "GetItem" or method "GetElement" in the Duck Object.');
  Result := FGetItemMethod.Invoke(FObjectAsDuck, [AIndex]).AsObject;
end;

function TDuckTypedList.GetOwnsObjects: Boolean;
var
  Prop: TRttiProperty;
begin
  Result := False;
  Prop := FObjType.GetProperty('OwnsObjects');
  if Assigned(Prop) then
    if Prop.IsReadable then
      Result := Prop.GetValue(FObjectAsDuck).AsBoolean;
end;

function TDuckTypedList.IsWrappedList: Boolean;
var
  ObjectType: TRttiType;
begin
  ObjectType := FContext.GetType(FObjectAsDuck.ClassInfo);

  Result := (ObjectType.GetMethod('Add') <> nil) and (ObjectType.GetMethod('Clear') <> nil)

  {$IF CompilerVersion >= 23}

    and (ObjectType.GetIndexedProperty('Items') <> nil) and (ObjectType.GetIndexedProperty('Items').ReadMethod <> nil)

  {$IFEND}

    and (ObjectType.GetMethod('GetItem') <> nil) or (ObjectType.GetMethod('GetElement') <> nil) and (ObjectType.GetProperty('Count') <> nil);
end;

procedure TDuckTypedList.QuickSort(const AList: IMVCList; ALeft,
  ARigth: Integer; ACompare: TFunc<TObject, TObject, Integer>);
var
  I, J: Integer;
  P: TObject;
  M: TRttiMethod;
  T: TRttiType;
begin
  { 07/08/2013: This method is based on QuickSort procedure from
    Classes.pas, (c) Borland Software Corp.
    but modified to be part of TDuckListU unit.  It implements the
    standard quicksort algorithm,
    delegating comparison operation to an anonimous.
    The Borland version delegates to a pure function
    pointer, which is problematic in some cases. }
  repeat
    I := ALeft;
    J := ARigth;
    P := AList.GetItem((ALeft + ARigth) shr 1);
    repeat
      while ACompare(TObject(AList.GetItem(I)), P) < 0 do
        Inc(I);
      while ACompare(TObject(AList.GetItem(J)), P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := FContext.GetType(AList.WrappedObject.ClassInfo);
        M := T.GetMethod('Exchange');
        if Assigned(M) then
          M.Invoke(AList.WrappedObject, [I, J])
        else
          raise EMVCDuckTypingException.CreateFmt('Cannot find compatible method "%s" in the object', ['Exchange']);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if ALeft < J then
      QuickSort(AList, ALeft, J, ACompare);
    ALeft := I;
  until I >= ARigth;
end;

procedure TDuckTypedList.QuickSort(const AList: IMVCList;
  ACompare: TFunc<TObject, TObject, Integer>);
begin
  QuickSort(AList, 0, AList.Count - 1, ACompare);
end;

procedure TDuckTypedList.SetOwnsObjects(const AValue: Boolean);
var
  Prop: TRttiProperty;
begin
  Prop := FObjType.GetProperty('OwnsObjects');
  if Assigned(Prop) then
    if Prop.IsWritable then
      Prop.SetValue(FObjectAsDuck, AValue)
end;

procedure TDuckTypedList.Sort(const APropertyName: string; const AOrder: TSortingType);
begin
  if (AOrder = soAscending) then
    QuickSort(Self,
      function(ALeft, ARight: TObject): Integer
      var
        PropLeft, PropRight: TRttiProperty;
      begin
        PropLeft := FContext.GetType(ALeft).GetProperty(APropertyName);
        PropRight := FContext.GetType(ARight).GetProperty(APropertyName);
        Result := CompareValue(PropLeft, PropRight);
      end)
  else
    QuickSort(Self,
      function(ALeft, ARight: TObject): Integer
      var
        PropLeft, PropRight: TRttiProperty;
      begin
        PropLeft := FContext.GetType(ALeft).GetProperty(APropertyName);
        PropRight := FContext.GetType(ARight).GetProperty(APropertyName);
        Result := -1 * CompareValue(PropLeft, PropRight);
      end);
end;

class function TDuckTypedList.Wrap(const AObjectAsDuck: TObject; const AOwnsObject: Boolean): IMVCList;
var
  List: IMVCList;
begin
  if AObjectAsDuck is TDuckTypedList then
    Exit(AObjectAsDuck as TDuckTypedList);
  Result := nil;
  List := TDuckTypedList.Create(AObjectAsDuck, AOwnsObject);
  if List.IsWrappedList then
    Result := List;
end;

function TDuckTypedList.WrappedObject: TObject;
begin
  Result := FObjectAsDuck;
end;

end.
