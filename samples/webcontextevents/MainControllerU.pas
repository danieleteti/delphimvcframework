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
  public
    [MVCPath('/sums')]
    [MVCHTTPMethod([httpGET])]
    procedure DoSum(
      const [MVCFromQueryString('a')] a: Integer;
      const [MVCFromQueryString('b')] b: Integer);
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger, System.StrUtils, System.Rtti, ServicesU;

procedure TMyController.Index;
begin
  //use Context property to access to the HTTP request and response
  Render('Hello DelphiMVCFramework World');
end;

procedure TMyController.DoSum(const a, b: Integer);
begin
  Render((Context.CustomIntfObject as ICalculator).DoCalc(a,b).ToString);
end;

end.
