unit BusinessObjectsU;

interface

uses
  MVCFramework.Serializer.Commons, System.Generics.Collections;

type
  TNested = class
  private
    FNestedString: String;
    FNestedDateTime: TDateTime;
    FNestedInteger: Integer;
    FNestedFloat: Double;
  public
    [MapperJSONSer('nestedString')]
    property NestedString: String read FNestedString write FNestedString;
    [MapperJSONSer('nestedDateTime')]
    property NestedDateTime: TDateTime read FNestedDateTime
      write FNestedDateTime;
    [MapperJSONSer('nestedInteger')]
    property NestedInteger: Integer read FNestedInteger
      write FNestedInteger;
    [MapperJSONSer('nestedFloat')]
    property NestedFloat: Double read FNestedFloat
      write FNestedFloat;
  end;

  [MapperJSONNaming(TJSONNameCase.JSONNameLowerCase)]
  TKeyValue = class
  private
    FKey: String;
    FValue: String;
    procedure SetKey(const Value: String);
    procedure SetValue(const Value: String);
  public
    property Key: String read FKey write SetKey;
    property Value: String read FValue write SetValue;
    constructor Create(const aKey, aValue: String);
  end;

  TParent = class
  private
    FNestedProperty: TNested;
    FParentString: String;
    FNestedList: TObjectList<TKeyValue>;
    procedure SetNestedList(const Value: TObjectList<TKeyValue>);
  public
    [MapperJSONSer('parentString')]
    property ParentString: String read FParentString write FParentString;
    [MapperJSONSer('nestedProperty')]
    property NestedProperty: TNested read FNestedProperty write FNestedProperty;
    [MapperJSONSer('nestedList')]
    [MapperListOf(TKeyValue)]
    property NestedList: TObjectList<TKeyValue> read FNestedList
      write SetNestedList;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TResponseTestClass }

constructor TParent.Create;
begin
  inherited;
  FNestedProperty := TNested.Create;
  FNestedList := TObjectList<TKeyValue>.Create(true);
end;

destructor TParent.Destroy;
begin
  FNestedList.Free;
  FNestedProperty.Free;
  inherited;
end;

procedure TParent.SetNestedList(const Value: TObjectList<TKeyValue>);
begin
  if (Value <> FNestedList) then
  begin
    FNestedList.Free;
    FNestedList := Value;
  end;
end;

{ TKeyValue }

constructor TKeyValue.Create(const aKey, aValue: String);
begin
  inherited Create;
  FKey := aKey;
  FValue := aValue;
end;

procedure TKeyValue.SetKey(const Value: String);
begin
  FKey := Value;
end;

procedure TKeyValue.SetValue(const Value: String);
begin
  FValue := Value;
end;

end.
