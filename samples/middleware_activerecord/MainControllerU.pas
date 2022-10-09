unit MainControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons;

type
  [MVCPath('/api')]
  TMyController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure Index;
  end;

implementation

uses
  MVCFramework.ActiveRecord,
  MVCFramework.SQLGenerators.PostgreSQL,
  System.SysUtils,
  MVCFramework.Logger,
  System.StrUtils,
  Entities;

procedure TMyController.Index;
begin
  Render(ObjectDict().Add('people',
    //TMVCActiveRecord.SelectRQL<TPerson>('and(gt(id, 2),lt(id,6))', 100)
    TMVCActiveRecord.SelectRQL<TPerson>('sort(+personSurname)', 10)
    ));
end;

end.
