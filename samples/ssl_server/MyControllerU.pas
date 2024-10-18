unit MyControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, Generics.Collections;

type

  [MVCPath('/')]
  TMyController = class(TMVCController)
    [MVCPath('/')]
    procedure Index(CTX: TWebContext);
    [MVCPath('/people')]
    procedure GetPeople(CTX: TWebContext);
  end;

implementation

uses
  SysUtils, MyObjectsU;

{ TMyController }

procedure TMyController.GetPeople(CTX: TWebContext);
var
  List: TObjectList<TPerson>;
begin
  List := TObjectList<TPerson>.Create(True);
  List.Add(TPerson.Create('Daniele', 'Teti', 38));
  List.Add(TPerson.Create('John', 'Doe', 35));
  List.Add(TPerson.Create('Jane', 'Doe', 32));
  List.Add(TPerson.Create('Bruce', 'Banner', 60));
  Render<TPerson>(List);
end;

procedure TMyController.Index(CTX: TWebContext);
begin
  Render(TPerson.Create('Daniele', 'Teti', 38));
end;

end.
