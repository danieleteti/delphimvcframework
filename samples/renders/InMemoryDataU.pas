unit InMemoryDataU;

interface

uses
  System.Generics.Collections,
  BusinessObjectsU;

function GetPeopleList: TObjectList<TPerson>;
function GetPeopleSmallList: TObjectList<TPerson>;

implementation

uses
  System.SysUtils;

var
  GPeople, GPeopleSmall: TObjectList<TPerson>;

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

initialization

PopulateList;
PopulateSmallList;

finalization

GPeople.Free;
GPeopleSmall.Free;

end.
