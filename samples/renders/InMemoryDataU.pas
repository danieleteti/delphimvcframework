unit InMemoryDataU;

interface

uses
  System.Generics.Collections,
  BusinessObjectsU, Data.DB;

function GetPeopleList: TObjectList<TPerson>;
function GetDataSet: TDataSet;
function GetPeopleSmallList: TObjectList<TPerson>;
function GetInterfacedPeopleList: TList<IPerson>;

implementation

uses
  System.SysUtils, FireDAC.Comp.Client, Data.SqlTimSt;

var
  GPeople, GPeopleSmall: TObjectList<TPerson>;

function GetInterfacedPeopleList: TList<IPerson>;
var
  lPerson: IPerson;
begin
  Result := TList<IPerson>.Create;
  lPerson := TInterfacedPerson.Create;
  lPerson.Name := 'Daniele Teti';
  lPerson.Age := 40;
  lPerson.DOB := EncodeDate(1979, 11, 4);
  Result.Add(lPerson);
  lPerson := TInterfacedPerson.Create;
  lPerson.Name := 'Peter Parker';
  lPerson.Age := 35;
  lPerson.DOB := EncodeDate(1984, 11, 4);
  Result.Add(lPerson);
end;

procedure PopulateList;
var
  p: TPerson;
  I: Integer;
begin
  GPeople := TObjectList<TPerson>.Create(True);
  for I := 1 to 1000 do
  begin
    p := TPerson.Create;
    p.FirstName := 'Daniele';
    p.LastName := 'Teti';
    p.DOB := EncodeDate(1979, 11, 4);
    p.Married := True;
    GPeople.Add(p);

    p := TPerson.Create;
    p.FirstName := 'John';
    p.LastName := 'Doe';
    p.DOB := EncodeDate(1879, 10, 2);
    p.Married := False;
    GPeople.Add(p);

    p := TPerson.Create;
    p.FirstName := 'Jane';
    p.LastName := 'Doe';
    p.DOB := EncodeDate(1883, 1, 5);
    p.Married := True;
    GPeople.Add(p);
  end;

end;

function GetPeopleList: TObjectList<TPerson>;
begin
  Result := GPeople;
end;

procedure PopulateSmallList;
var
  p: TPerson;
begin
  GPeopleSmall := TObjectList<TPerson>.Create(True);
  p := TPerson.Create;
  p.FirstName := 'Daniele';
  p.LastName := 'Teti';
  p.DOB := EncodeDate(1979, 11, 4);
  p.Married := True;
  GPeopleSmall.Add(p);

  p := TPerson.Create;
  p.FirstName := 'John';
  p.LastName := 'Doe';
  p.DOB := EncodeDate(1879, 10, 2);
  p.Married := False;
  GPeopleSmall.Add(p);

  p := TPerson.Create;
  p.FirstName := 'Jane';
  p.LastName := 'Doe';
  p.DOB := EncodeDate(1883, 1, 5);
  p.Married := True;
  GPeopleSmall.Add(p);
end;

function GetPeopleSmallList: TObjectList<TPerson>;
begin
  Result := GPeopleSmall;
end;

function CreateDataSet: TFDMemTable;
var
  lDS: TFDMemTable;
begin
  lDS := TFDMemTable.Create(nil);
  try
    lDS.FieldDefs.Add('field_string', ftString, 20);
    lDS.FieldDefs.Add('field_time', ftTime);
    lDS.FieldDefs.Add('field_date', ftDate);
    lDS.FieldDefs.Add('field_datetime', ftDateTime);
    lDS.FieldDefs.Add('field_timestamp', ftTimeStamp);
//    lDS.FieldDefs.Add('field_timestamp_with_offset', ftTimeStampOffset);
    lDS.CreateDataSet;
    Result := lDS;
  except
    lDS.Free;
    raise;
  end;
end;

function GetDataSet: TDataSet;
var
  lDS: TFDMemTable;
  I: Integer;
begin
  lDS := CreateDataSet;
  try
    for I := 0 to 0 do
    begin
      lDS.Insert;
      lDS.FieldByName('field_string').AsString := 'Field' + I.ToString;
      lDS.FieldByName('field_time').AsDateTime := Time();
      lDS.FieldByName('field_date').AsDateTime := Date();
      lDS.FieldByName('field_datetime').AsDateTime := Now();
      lDS.FieldByName('field_timestamp').AsSQLTimeStamp := DateTimeToSQLTimeStamp(Now());
//      lDS.FieldByName('field_timestamp_with_offset').AsSQLTimeStampOffset := DateTimeToSQLTimeStampOffset(Now());
      lDS.Post;
    end;
  except
    lDS.Free;
    raise;
  end;
  Result := lDS;
end;

initialization

PopulateList;
PopulateSmallList;

finalization

GPeople.Free;
GPeopleSmall.Free;

end.
