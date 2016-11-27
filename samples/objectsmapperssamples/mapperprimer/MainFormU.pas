unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, PersonU, System.Generics.Collections,
  ObjectsMappers, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Grids, Vcl.DBGrids;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    FDMemTable1: TFDMemTable;
    btnDatasets: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnDatasetsClick(Sender: TObject);
  private
    procedure Log(const Value: String);
    procedure ClearLog;
  public
    { Public declarations }
  end;

  TPeople = TObjectList<TPerson>;

  TData = class
  private
    FPeople: TObjectList<TPerson>;
    FPerson: TPerson;
    procedure SetPerson(const Value: TPerson);
  public
    { ObjectsMappers ATTR }
    [MapperListOf(TPerson)]
    property People: TPeople read FPeople write FPeople;
    property Person: TPerson read FPerson write SetPerson;
    constructor Create;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.JSON, BusinessObjectsU;

{ TData }

constructor TData.Create;
begin
  inherited;
  FPeople := TObjectList<TPerson>.Create(true);
  FPerson := TPerson.Create;
end;

destructor TData.Destroy;
begin
  FPerson.Free;
  FPeople.Free;
  inherited;
end;

procedure TData.SetPerson(const Value: TPerson);
begin
  FPerson := Value;
end;

{$R *.dfm}


procedure TMainForm.btnDatasetsClick(Sender: TObject);
var
  lJArr: TJSONArray;
  lDSSer: string;
begin
  ClearLog;
  FDMemTable1.Close;
  FDMemTable1.FieldDefs.Clear;
  FDMemTable1.FieldDefs.Add('FirstName', ftString, 50);
  FDMemTable1.FieldDefs.Add('LastName', ftString, 50);
  FDMemTable1.FieldDefs.Add('Age', ftInteger);
  FDMemTable1.FieldDefs.Add('eMail', ftString, 50);
  FDMemTable1.Open;
  FDMemTable1.AppendRecord(['Daniele', 'Teti', 37, 'd.teti@bittime.it']);
  FDMemTable1.AppendRecord(['Bruce', 'Banner', 50, 'hulk@marvel.com']);
  FDMemTable1.AppendRecord(['Peter', 'Parker', 25, 'spidey@marvel.com']);
  FDMemTable1.First;
  lJArr := TJSONArray.Create;
  try
    Log('** Serialize dataset to jsonarray');
    Mapper.DataSetToJSONArray(FDMemTable1, lJArr, False, nil, TFieldNamePolicy.fpUpperCase);
    lDSSer := lJArr.ToJSON;
    Log(lDSSer);
  finally
    lJArr.Free;
  end;
  FDMemTable1.Close;
  Log('** Loading dataset from json array');
  FDMemTable1.Open;
  FDMemTable1.LoadFromJSONArrayString(lDSSer, TFieldNamePolicy.fpUpperCase);
  FDMemTable1.First;
  lJArr := TJSONArray.Create;
  try
    Mapper.DataSetToJSONArray(FDMemTable1, lJArr, False, nil, TFieldNamePolicy.fpUpperCase);
    lDSSer := lJArr.ToJSON;
    Log(lDSSer);
  finally
    lJArr.Free;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  lData: TData;
  P: TPerson;
  lJObj: TJSONObject;
  lNewData: TData;
begin
  ClearLog;
  lData := TData.Create;
  try
    P := TPerson.Create;
    lData.People.Add(P);
    P.FirstName := 'Daniele';
    P.LastName := 'Teti';
    P.DateOfBirth := EncodeDate(1979, 5, 2);

    P := TPerson.Create;
    lData.People.Add(P);
    P.FirstName := 'Peter';
    P.LastName := 'Parker';
    P.DateOfBirth := EncodeDate(1989, 5, 2);

    lData.Person.FirstName := 'Bruce';
    lData.Person.LastName := 'Banner';
    lData.Person.DateOfBirth := EncodeDate(1959, 5, 2);

    lJObj := Mapper.ObjectToJSONObject(lData);
    try
      Log('Serialized form:');
      Log(lJObj.ToJSON);
      Log('Deserializing...');
      lNewData := Mapper.JSONObjectToObject<TData>(lJObj);
      try
        Log('Deserialized object serialization');
        Log(Mapper.ObjectToJSONObjectString(lData));
      finally
        lNewData.Free;
      end;
    finally
      lJObj.Free;
    end;

  finally
    lData.Free;
  end;

end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  LPerson: TPerson;
  LPersonUpper: TPersonUpperCase;
  LPersonLower: TPersonLowerCase;
  LPersonCustomCase: TPersonCustomCase;
begin
  ClearLog;
  LPerson := TPerson.Create;
  LPerson.FirstName := 'Daniele';
  LPerson.LastName := 'Teti';
  LPerson.DateOfBirth := EncodeDate(1979, 11, 11);
  Log('** PLAIN SERIALIZATION');
  Log(Mapper.ObjectToJSONObjectString(LPerson));
  LPerson.Free;

  LPersonUpper := TPersonUpperCase.Create;
  LPersonUpper.FirstName := 'Daniele';
  LPersonUpper.LastName := 'Teti';
  LPersonUpper.DateOfBirth := EncodeDate(1979, 11, 11);
  Log(slinebreak + '** ALL PROPERTIES IN UPPER CASE');
  Log(Mapper.ObjectToJSONObjectString(LPersonUpper));
  LPersonUpper.Free;

  LPersonLower := TPersonLowerCase.Create;
  LPersonLower.FirstName := 'Daniele';
  LPersonLower.LastName := 'Teti';
  LPersonLower.DateOfBirth := EncodeDate(1979, 11, 11);
  Log(slinebreak + '** ALL PROPERTIES IN LOWER CASE');
  Log(Mapper.ObjectToJSONObjectString(LPersonLower));
  LPersonLower.Free;

  LPersonCustomCase := TPersonCustomCase.Create;
  LPersonCustomCase.FirstName := 'Daniele';
  LPersonCustomCase.LastName := 'Teti';
  LPersonCustomCase.DateOfBirth := EncodeDate(1979, 11, 11);
  LPersonCustomCase.WorkEmail := 'd.teti@nowhere.com';
  LPersonCustomCase.PhoneNumber := '555-3445564';
  Log(slinebreak + '** CUSTOM NAMING FOR PROPERTIES');
  Log(Mapper.ObjectToJSONObjectString(LPersonCustomCase));
  LPersonCustomCase.Free;

end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  lParent: TParent;
  lSerialized: string;
  lNewParent: TParent;
begin
  ClearLog;
  lParent := TParent.Create;
  try
    lParent.ParentString := 'This is the parent string';
    lParent.NestedProperty.NestedString := 'my string';
    lParent.NestedProperty.NestedInteger := 1234;
    lParent.NestedProperty.NestedFloat := 1234.5678;
    lParent.NestedProperty.NestedDateTime := Now;
    lParent.NestedList.Add(TKeyValue.Create('firstname', 'Daniele'));
    lParent.NestedList.Add(TKeyValue.Create('lastname', 'Teti'));
    lParent.NestedList.Add(TKeyValue.Create('email', 'd.teti@bittime.it'));

    Log('** Serialize complex object');
    lSerialized := Mapper.ObjectToJSONObjectString(lParent);
    Log(lSerialized);

    Log('** Deserialize complex object');
    lNewParent := Mapper.JSONObjectStringToObject<TParent>(lSerialized);
    try
      lSerialized := Mapper.ObjectToJSONObjectString(lNewParent);
      Log(lSerialized);
    finally
      lNewParent.Free;
    end;
  finally
    lParent.Free;
  end;

end;

procedure TMainForm.ClearLog;
begin
  Memo1.Lines.Clear;
end;

procedure TMainForm.Log(const Value: String);
begin
  Memo1.Lines.Add(StringOfChar('-', 30));
  Memo1.Lines.Add(Value);
end;

end.
