program jsoncasesample;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  ObjectsMappers,
  PersonU in 'PersonU.pas';

procedure Main;
var
  LPerson: TPerson;
  LPersonUpper: TPersonUpperCase;
  LPersonLower: TPersonLowerCase;
  LPersonCustomCase: TPersonCustomCase;
begin
  LPerson := TPerson.Create;
  LPerson.FirstName := 'Daniele';
  LPerson.LastName := 'Teti';
  LPerson.DateOfBirth := EncodeDate(1979, 11, 11);
  WriteLn('UNCHANGED CASE');
  WriteLn(Mapper.ObjectToJSONObjectString(LPerson));
  LPerson.Free;

  LPersonUpper := TPersonUpperCase.Create;
  LPersonUpper.FirstName := 'Daniele';
  LPersonUpper.LastName := 'Teti';
  LPersonUpper.DateOfBirth := EncodeDate(1979, 11, 11);
  WriteLn(slinebreak + 'UPPER CASE');
  WriteLn(Mapper.ObjectToJSONObjectString(LPersonUpper));
  LPersonUpper.Free;

  LPersonLower := TPersonLowerCase.Create;
  LPersonLower.FirstName := 'Daniele';
  LPersonLower.LastName := 'Teti';
  LPersonLower.DateOfBirth := EncodeDate(1979, 11, 11);
  WriteLn(slinebreak + 'LOWER CASE');
  WriteLn(Mapper.ObjectToJSONObjectString(LPersonLower));
  LPersonLower.Free;

  LPersonCustomCase := TPersonCustomCase.Create;
  LPersonCustomCase.FirstName := 'Daniele';
  LPersonCustomCase.LastName := 'Teti';
  LPersonCustomCase.DateOfBirth := EncodeDate(1979, 11, 11);
  LPersonCustomCase.WorkEmail := 'd.teti@nowhere.com';
  LPersonCustomCase.PhoneNumber := '555-3445564';
  WriteLn(slinebreak + 'CUSTOM CASE');
  WriteLn(Mapper.ObjectToJSONObjectString(LPersonCustomCase));
  LPersonCustomCase.Free;

end;

begin
  try
    Main;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
  ReadLn;

end.
