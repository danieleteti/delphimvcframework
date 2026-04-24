unit ControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  ModelsU;

type
  [MVCPath('/people')]
  TPeopleController = class(TMVCController)
  public
    // Classic split: DTO at the HTTP boundary, AR at the storage layer.
    [MVCPath]
    [MVCHTTPMethod([httpPOST])]
    [MVCProduces('application/json')]
    function CreatePerson([MVCFromBody] aDTO: TPersonDTO): TPerson;

    // "AR as DTO" pattern: the same AR class is deserialized from the
    // body and used as the storage entity. TMVCActiveRecord inherits
    // TMVCValidatable, so the framework runs the FIELD validators at
    // the HTTP boundary too (via the OnValidate override). Only
    // OnStorageValidate stays a strictly save-time rule.
    [MVCPath('/ar')]
    [MVCHTTPMethod([httpPOST])]
    [MVCProduces('application/json')]
    function CreatePersonAR([MVCFromBody] aPerson: TPerson): TPerson;
  end;

implementation

function TPeopleController.CreatePerson(aDTO: TPersonDTO): TPerson;
begin
  // DTO property validators have already passed. Map to AR and Insert -
  // the storage layer now fires (field validators + OnStorageValidate).
  Result := TPerson.Create;
  try
    Result.Name := aDTO.Name;
    Result.Email := aDTO.Email;
    Result.Insert;
  except
    Result.Free;
    raise;
  end;
end;

function TPeopleController.CreatePersonAR(aPerson: TPerson): TPerson;
begin
  aPerson.Insert;
  Result := aPerson;  //Result or aPerson will not be freed twice. This is detected by the framework
end;

end.
