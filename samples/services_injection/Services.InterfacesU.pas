unit Services.InterfacesU;

interface

uses Entities, System.Generics.Collections;


type
  IPeopleService = interface
    ['{347532A0-1B28-40C3-A2E9-51DF62365FE7}']
    function GetAll: TObjectList<TPerson>;
  end;

  IConnectionService = interface
    ['{146C21A5-07E8-456D-8E6D-A72820BD17AA}']
    function GetConnectionName: String;
  end;

implementation

end.
