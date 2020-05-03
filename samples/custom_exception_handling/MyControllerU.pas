unit MyControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  System.SysUtils;

type

  [MVCPath('/')]
  TMyController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    [MVCPath('/customers/($ID)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomer(const ID: Integer);

    [MVCPath('/error')]
    [MVCHTTPMethod([httpGET])]
    procedure Error;
  end;

  TMyExceptionSeverity = (Fatal, Error, Warning, Information);
  EMyException = class(Exception)
  private
    FSeverity: TMyExceptionSeverity;
    FCode: Integer;
    FDetails: string;
    FDiagnostics: string;
    FExpression: string;
  public
    constructor Create(Msg: string; ASeverity: TMyExceptionSeverity; ACode: Integer;
      ADetails, ADiagnostics, AExpression: string);
    property Severity: TMyExceptionSeverity read FSeverity write FSeverity;
    property Code: Integer read FCode write FCode;
    property Details: string read FDetails write FDetails;
    property Diagnostics: string read FDiagnostics write FDiagnostics;
    property Expression: string read FExpression write FExpression;
  end;



implementation

uses
  MVCFramework.Logger;

procedure TMyController.GetCustomer(const ID: Integer);
begin
  Render204NoContent();
end;

procedure TMyController.Index;
begin
  raise EMyException.Create('My Custom Error', Fatal, 25, 'some real problem',
    'Ensure Patient resource is valid', 'Patient/Identifier/value');
end;

procedure TMyController.Error;
begin
  raise Exception.Create('Standard Error');
end;

constructor EMyException.Create(Msg: string; ASeverity: TMyExceptionSeverity; ACode: Integer;
  ADetails, ADiagnostics, AExpression: string);
begin
  inherited Create(Msg);
  FSeverity := ASeverity;
  FCode := ACode;
  FDetails := ADetails;
  FDiagnostics := ADiagnostics;
  FExpression := AExpression;
end;

end.
