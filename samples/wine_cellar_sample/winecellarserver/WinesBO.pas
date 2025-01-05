unit WinesBO;

{ ** Note: In case of Delphi clients you can share the units containing the business objects
  between client and server. This is not required, and could lead to difficult
  dependencies schemas in buig project. In this demo this file is shared between
  clients and server. }

interface

uses System.Generics.Collections, MVCFramework.Serializer.Commons;

type

  [MVCNameCase(ncLowerCase)]
  TWine = class
  private
    FYEAR: string;
    FNAME: string;
    FPICTURE: string;
    FGRAPES: string;
    FID: integer;
    FDESCRIPTION: string;
    FCOUNTRY: string;
    FREGION: string;
    procedure SetCOUNTRY(const Value: string);
    procedure SetDESCRIPTION(const Value: string);
    procedure SetGRAPES(const Value: string);
    procedure SetID(const Value: integer);
    procedure SetNAME(const Value: string);
    procedure SetPICTURE(const Value: string);
    procedure SetREGION(const Value: string);
    procedure SetYEAR(const Value: string);

  public
    property id: integer read FID write SetID;
    property name: string read FNAME write SetNAME;
    property year: string read FYEAR write SetYEAR;
    property grapes: string read FGRAPES write SetGRAPES;
    property country: string read FCOUNTRY write SetCOUNTRY;
    property region: string read FREGION write SetREGION;
    property description: string read FDESCRIPTION write SetDESCRIPTION;
    property picture: string read FPICTURE write SetPICTURE;
  end;

  TWines = class(TObjectList<TWine>)
    constructor Create;
  end;

implementation

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

procedure TWine.SetPICTURE(const Value: string);
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

{ TWines }

constructor TWines.Create;
begin
  inherited Create(True);
end;

end.
