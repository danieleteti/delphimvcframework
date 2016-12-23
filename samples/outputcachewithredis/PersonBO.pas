unit PersonBO;

interface

type
  TPerson = class
  private
    FWORK_PHONE_NUMBER: String;
    FEMAIL: String;
    FFIRST_NAME: String;
    FID: Integer;
    FMOBILE_PHONE_NUMBER: String;
    FLAST_NAME: String;
    procedure SetEMAIL(const Value: String);
    procedure SetFIRST_NAME(const Value: String);
    procedure SetID(const Value: Integer);
    procedure SetLAST_NAME(const Value: String);
    procedure SetMOBILE_PHONE_NUMBER(const Value: String);
    procedure SetWORK_PHONE_NUMBER(const Value: String);
  public
    property ID: Integer read FID write SetID;
    property FIRST_NAME: String read FFIRST_NAME write SetFIRST_NAME;
    property LAST_NAME: String read FLAST_NAME write SetLAST_NAME;
    property WORK_PHONE_NUMBER: String read FWORK_PHONE_NUMBER write SetWORK_PHONE_NUMBER;
    property MOBILE_PHONE_NUMBER: String read FMOBILE_PHONE_NUMBER write SetMOBILE_PHONE_NUMBER;
    property EMAIL: String read FEMAIL write SetEMAIL;
  end;

implementation

{ TPerson }

procedure TPerson.SetEMAIL(const Value: String);
begin
  FEMAIL := Value;
end;

procedure TPerson.SetFIRST_NAME(const Value: String);
begin
  FFIRST_NAME := Value;
end;

procedure TPerson.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TPerson.SetLAST_NAME(const Value: String);
begin
  FLAST_NAME := Value;
end;

procedure TPerson.SetMOBILE_PHONE_NUMBER(const Value: String);
begin
  FMOBILE_PHONE_NUMBER := Value;
end;

procedure TPerson.SetWORK_PHONE_NUMBER(const Value: String);
begin
  FWORK_PHONE_NUMBER := Value;
end;

end.
