unit frmLoadSwaggerJson;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Swag.Common.Types,
  Swag.Doc,
  Swag.Doc.Definition,
  Swag.Doc.Path,
  Swag.Doc.Path.Operation;

type
  TfrmSimpleSwaggerDocDemo = class(TForm)
    Memo1: TMemo;
    btnLoadJSON: TButton;
    lblApiDescription: TLabel;
    procedure btnLoadJSONClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSimpleSwaggerDocDemo: TfrmSimpleSwaggerDocDemo;

implementation

{$R *.dfm}

uses
  Json.Common.Helpers,
  System.IOUtils;

procedure TfrmSimpleSwaggerDocDemo.btnLoadJSONClick(Sender: TObject);
var
  vSwagDoc: TSwagDoc;
begin
  vSwagDoc := TSwagDoc.Create;
  try
    vSwagDoc.LoadFromFile('swagger.json');
    lblApiDescription.Caption := vSwagDoc.Info.Description;
    vSwagDoc.GenerateSwaggerJson;
    Memo1.Lines.Clear;
    Memo1.Lines.Add(vSwagDoc.SwaggerJson.Format);
  finally
    FreeAndNil(vSwagDoc);
  end;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
