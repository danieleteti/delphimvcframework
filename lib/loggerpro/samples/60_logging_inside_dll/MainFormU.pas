unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm5 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    btnDoSomething: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnDoSomethingClick(Sender: TObject);
  private
    FHandle: NativeUInt;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;
  Init, DeInit, DoSomething: procedure;

implementation

uses
  LoggerPro.GlobalLogger;

{$R *.dfm}


procedure TForm5.btnDoSomethingClick(Sender: TObject);
begin
  DoSomething;
end;

procedure TForm5.Button1Click(Sender: TObject);
begin
  Log.Debug('Loading dll', 'main');
  FHandle := LoadLibrary('mydll.dll');
  Init := GetProcAddress(FHandle, 'Init');
  DeInit := GetProcAddress(FHandle, 'DeInit');
  DoSomething := GetProcAddress(FHandle, 'DoSomething');
  Init();
end;

procedure TForm5.Button2Click(Sender: TObject);
begin
  Log.Debug('UnLoading dll', 'main');
  DeInit();
  FreeLibrary(FHandle);
end;

end.
