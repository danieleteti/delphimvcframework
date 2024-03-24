unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm13 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    function GetName(const pGUID: TGUID; const I: TClass): String;
  public
    { Public declarations }
  end;

  IPippo = interface
    ['{627B2E4A-158F-4297-9823-42E4C0D2907C}']
    function SayHello(): String;
  end;

  IPippo2 = interface
    ['{F929B0CA-A699-4E92-8298-2F74C09F03ED}']
    function SayGoodbye(): String;
  end;


  TPippo = class(TInterfacedObject, IPippo, IPippo2)
  public
    function SayHello(): String;
    function SayGoodbye(): String;
    destructor Destroy; override;
  end;

var
  Form13: TForm13;

implementation

uses
  System.TypInfo,
  System.Rtti, MVCFramework.Injector;

{$R *.dfm}

procedure TForm13.Button1Click(Sender: TObject);
begin
  Caption := GetName(IPippo, TPippo);
end;

procedure TForm13.Button2Click(Sender: TObject);
begin
  var lCont := TMVCServiceContainer.Create;
  try
    lCont.RegisterType<TPippo>(IPippo);
    lCont.RegisterType<TPippo>(IPippo2, '', rtSingleton);
    var l0 := lCont.Resolve<IPippo>;
    var l1 := lCont.Resolve<IPippo>;
    Assert(l0 <> l1);
    var l2 := lCont.Resolve<IPippo2>;
    var l3 := lCont.Resolve<IPippo2>;
    Assert(l2 = l3);
  finally
    lCont.Free;
  end;
end;

function TForm13.GetName(const pGUID: TGUID; const I: TClass): String;
begin
  var lCTX := TRttiContext.Create;
  try
    var lType := lCTX.GetType(I);

    if lType = nil then
    begin
      ShowMessage('Cannot find type name')
    end
    else
    begin
      Result := lType.ToString;
      var _: IInterface;
      if Supports(I, pGUID) then
      begin
        ShowMessage('Interfaccia trovata');
      end;
    end;
  finally
    lCTX.Free;
  end;

  var lout: iinterface := nil;
  if Supports(I.Create(), pGUID, lout) then
  begin
    ShowMessage((lout as IPippo).SayHello);
  end;
end;

{ TPippo }

destructor TPippo.Destroy;
begin
  ShowMessage('TPippo.Destroy');
  inherited;
end;

function TPippo.SayGoodbye: String;
begin
  Result := 'TPippo.SayGoodbye';
end;

function TPippo.SayHello: String;
begin
  Result := 'TPippo.SayHello';
end;

end.
