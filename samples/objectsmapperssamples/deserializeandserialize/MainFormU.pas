unit MainFormU;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, Vcl.Graphics,
	Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ObjectsMappers, BusinessObjectsU, Vcl.StdCtrls;

type
	TForm1 = class(TForm)
		Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
		procedure Button1Click(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.Button1Click(Sender: TObject);
var
	LResponseSession: TParent;
begin
	LResponseSession := Mapper.JSONObjectStringToObject<TParent>(Memo1.Lines.Text);
	Memo2.Lines.Text := Mapper.ObjectToJSONObjectString(LResponseSession);
end;

end.
