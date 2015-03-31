unit SampleControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/')]
  TSampleController = class(TMVCController)
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean); override;

  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGet])]
    procedure Index(Ctx: TWebContext);

    [MVCPath('/customers')]
    [MVCHTTPMethod([httpGet])]
    [MVCProduces('text/html')]
    procedure CustomersList(Ctx: TWebContext);

    [MVCPath('/customers')]
    [MVCHTTPMethod([httpPost])]
    [MVCProduces('text/html')]
    [MVCConsumes('application/x-www-form-urlencoded')]
    procedure CreateCustomer(Ctx: TWebContext);

  end;

implementation

uses
  System.SysUtils{$IF CompilerVersion >= 27}, System.JSON {$ELSE},
  Data.DBXJSON{$ENDIF};

{ TRoutingSampleController }

procedure TSampleController.CreateCustomer(Ctx: TWebContext);
var
  Arr: TJSONArray;
  FirstName: string;
  LastName: string;
begin
  Arr := TJSONObject.ParseJSONValue(Session['customers']) as TJSONArray;
  try
    FirstName := Ctx.Request.Params['firstName'];
    LastName := Ctx.Request.Params['lastName'];
    Arr.AddElement(TJSONObject.Create.AddPair('firstName', FirstName)
      .AddPair('lastName', LastName));
    Session['customers'] := Arr.ToString;
  finally
    Arr.Free;
  end;
  Redirect('/customers');
end;

procedure TSampleController.OnBeforeAction(Context: TWebContext;
  const AActionNAme: string; var Handled: Boolean);
var
  Arr: TJSONArray;
  cust: TJSONObject;
begin
  inherited;
  if Session['customers'].IsEmpty then
  begin
    Arr := TJSONArray.Create;
    try
      Arr.AddElement(TJSONObject.Create.AddPair('firstName', 'Daniele')
        .AddPair('lastName', 'Teti'));
      Arr.AddElement(TJSONObject.Create.AddPair('firstName', 'Peter')
        .AddPair('lastName', 'Parker'));
      Arr.AddElement(TJSONObject.Create.AddPair('firstName', 'Sue')
        .AddPair('lastName', 'Storm'));
      Session['customers'] := Arr.ToString;
    finally
      Arr.Free;
    end;
  end;
end;

procedure TSampleController.CustomersList(Ctx: TWebContext);
var
  JV: TJSONValue;
begin
  JV := TJSONObject.ParseJSONValue(Session['customers']);
  PushJSONToView('customers', JV);
  LoadView('list');
end;

procedure TSampleController.Index(Ctx: TWebContext);
begin
  Redirect('/customers');
end;

end.
