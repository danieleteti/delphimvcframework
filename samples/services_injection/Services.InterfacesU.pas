unit Services.InterfacesU;

interface

uses Entities, System.Generics.Collections;


type
  IPeopleService = interface
    ['{347532A0-1B28-40C3-A2E9-51DF62365FE7}']
    function GetAll: TObjectList<TPerson>;
  end;

implementation

end.
