unit EntitiesU;

interface

uses
  MVCFramework.Swagger.Commons;

type
  TPerson = class
  public
    procedure Method1;
    procedure Method2;
  end;

  TPersonRec = record
    FirstName: String;
    LastName: String;
  end;

  TPersonModel = class
  private
    fLastName: String;
    fFirstName: String;
  public
    [MVCSwagJsonSchemaField(stString, 'firstname', 'Person''s FirstName')]
    property FirstName: String read fFirstName write fFirstName;
    [MVCSwagJsonSchemaField(stString, 'lastname', 'Person''s LastName')]
    property LastName: String read fLastName write fLastName;
  end;

  TPersonWithNickNameModel = class(TPersonModel)
  private
    fNickName: String;
  public
    [MVCSwagJsonSchemaField(stString, 'nickname', 'Person''s NickName')]
    property NickName: String read fNickName write fNickName;
  end;

  TTallPersonModel = class(TPersonModel)
  private
    fHeight: String;
  public
    [MVCSwagJsonSchemaField(stInteger, 'height', 'Person''s Height')]
    property Height: String read fHeight write fHeight;
  end;

implementation

{ TPerson }

procedure TPerson.Method1;
begin

end;

procedure TPerson.Method2;
begin

end;

end.
