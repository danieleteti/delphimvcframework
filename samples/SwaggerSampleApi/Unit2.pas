unit Unit2;

interface

uses
    Winapi.Windows
  , Winapi.Messages
  , System.SysUtils
  , System.Variants
  , System.Classes
  , System.TypInfo
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StdCtrls
  , DelphiUnit
  ;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  delphi : TDelphiUnit;
  i: Integer;
  NewType : TUnitTypeDefinition;
  AddressType : TUnitTypeDefinition;
  newField : TUnitFieldDefinition;
  method : TUnitMethod;
  param : TUnitParameter;
begin
  delphi := TDelphiUnit.Create;
  try
    delphi.UnitFile := 'delphitest';
    delphi.AddInterfaceUnit('SysUtils');

    AddressType := TUnitTypeDefinition.Create;
    AddressType.TypeName := 'TAddress';
    AddressType.AddAttribute('[MVCDoc(''This is an Address'')]');
    newField := TUnitFieldDefinition.Create;
    newField.AddAttribute('[MVCDoc(''Address'')]');
    newField.FieldName := 'Addr';
    newField.FieldType := 'String';
    AddressType.Fields.Add(newField);

    newField := TUnitFieldDefinition.Create;
    newField.AddAttribute('[MVCDoc(''Address'')]');
    newField.FieldName := 'Addr';
    newField.FieldType := 'String';
    AddressType.Fields.Add(newField);

    newField := TUnitFieldDefinition.Create;
    newField.AddAttribute('[MVCDoc(''Enter Your City'')]');
    newField.FieldName := 'City';
    newField.FieldType := 'String';
    AddressType.Fields.Add(newField);

    newField := TUnitFieldDefinition.Create;
    newField.AddAttribute('[MVCDoc(''Enter Your Postcode'')]');
    newField.FieldName := 'Postcode';
    newField.FieldType := 'String';
    AddressType.Fields.Add(newField);

    delphi.AddType(AddressType);


    NewType := TUnitTypeDefinition.Create;
    NewType.TypeName := 'TEmployee';
    NewType.AddAttribute('[MVCDoc(''This is some text'')]');

    newField := TUnitFieldDefinition.Create;
    newField.AddAttribute('[MVCDoc(''Employee Number'')]');
    newField.AddAttribute('[MVCContraint(0, 10000)]');
    newField.FieldName := 'EmpNo';
    newField.FieldType := 'Integer';
    NewType.Fields.Add(newField);

    newField := TUnitFieldDefinition.Create;
    newField.FieldName := 'EmpName';
    newField.FieldType := 'String';
    NewType.Fields.Add(newField);

    newField := TUnitFieldDefinition.Create;
    newField.FieldName := 'Address';
    newField.FieldType := 'TAddress';
    NewType.Fields.Add(newField);

    method := TUnitMethod.Create;
    method.Name := 'TestMethod';
    method.MethodKind := TMethodKind.mkFunction;
    method.ReturnType := AddressType;

    param := TUnitParameter.Create;
    param.ParamName := 'inFirstParam';
    param.Flags := [pfVar];
    param.ParamType := AddressType;    
    
    method.AddParameter(param);
    NewType.FMethods.Add(method);


    delphi.AddType(NewType);

    Memo1.Lines.Add(delphi.GenerateInterfaceSectionStart);
    Memo1.Lines.Add(delphi.GenerateInterfaceUses);

    if delphi.TypeDefinitions.Count > 0 then
      Memo1.Lines.Add('type');

    for i := 0 to delphi.TypeDefinitions.Count - 1 do
    begin
      Memo1.Lines.Add(delphi.TypeDefinitions[i].GenerateInterface);
    end;

    Memo1.Lines.Add(delphi.GenerateImplementationSectionStart);
    Memo1.Lines.Add(delphi.GenerateImplementationUses);

    for i := 0 to delphi.TypeDefinitions.Count - 1 do
    begin
      for method in delphi.TypeDefinitions[i].GetMethods do
      begin
        Memo1.Lines.Add(method.GenerateImplementation(delphi.TypeDefinitions[i]));
      end;
    end;

    
    Memo1.Lines.Add('end.');

  finally
    FreeAndNil(delphi);
  end;
end;

end.
