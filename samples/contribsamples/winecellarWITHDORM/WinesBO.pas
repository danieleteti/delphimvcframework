unit WinesBO;

interface

uses ObjectsMappers, dorm.ObjectStatus, dorm.Mappings;

type

  [MapperJSONNaming(JSONNameLowerCase)]
  TWine = class
  private
    FYEAR: string;
    FNAME: string;
    FPICTURE: String;
    FGRAPES: string;
    FID: integer;
    FDESCRIPTION: string;
    FCOUNTRY: string;
    FREGION: string;
    FObjStatus: tdormObjectStatus;
    procedure SetCOUNTRY(const Value: string);
    procedure SetDESCRIPTION(const Value: string);
    procedure SetGRAPES(const Value: string);
    procedure SetID(const Value: integer);
    procedure SetNAME(const Value: string);
    procedure SetPICTURE(const Value: String);
    procedure SetREGION(const Value: string);
    procedure SetYEAR(const Value: string);
    procedure SetObjStatus(const Value: tdormObjectStatus);
  public
    procedure Validate;
    property id: integer read FID write SetID;
    property name: string read FNAME write SetNAME;
    property year: string read FYEAR write SetYEAR;
    property grapes: string read FGRAPES write SetGRAPES;
    property country: string read FCOUNTRY write SetCOUNTRY;
    property region: string read FREGION write SetREGION;
    property description: string read FDESCRIPTION write SetDESCRIPTION;
    property picture: String read FPICTURE write SetPICTURE;
    [Transient]
    property ObjStatus: TdormObjectStatus read FObjStatus write SetObjStatus;
  end;

implementation

uses
  System.SysUtils;

{ TWine }

procedure TWine.SetCOUNTRY(const Value: string);
begin
  FCOUNTRY := Value;
end;

procedure TWine.SetDESCRIPTION(const Value: string);
begin
  FDESCRIPTION := Value;
end;

procedure TWine.SetGRAPES(const Value: string);
begin
  FGRAPES := Value;
end;

procedure TWine.SetID(const Value: integer);
begin
  FID := Value;
end;

procedure TWine.SetNAME(const Value: string);
begin
  FNAME := Value;
end;

procedure TWine.SetObjStatus(const Value: tdormObjectStatus);
begin
  FObjStatus := Value;
end;

procedure TWine.SetPICTURE(const Value: String);
begin
  FPICTURE := Value;
end;

procedure TWine.SetREGION(const Value: string);
begin
  FREGION := Value;
end;

procedure TWine.SetYEAR(const Value: string);
begin
  FYEAR := Value;
end;

procedure TWine.Validate;
begin
  if name.Length < 4 then
    raise Exception.Create('Wine name must be longer than 3 characters');
end;

end.
