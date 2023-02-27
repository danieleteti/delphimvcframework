unit MainFormU;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    procedure btnDEBUGClick(Sender: TObject);
    procedure btnERRORClick(Sender: TObject);
    procedure btnINFOClick(Sender: TObject);
    procedure btnWARNINGClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}


uses
  LoggerProConfig,
  System.Permissions,
{$IFDEF ANDROID}
  Androidapi.JNI.Os,
  Androidapi.JNI.JavaTypes,
  Androidapi.Helpers,
{$ENDIF}
  FMX.DialogService;

procedure TForm2.btnDEBUGClick(Sender: TObject);
begin
  Log.Debug('This is a debug message with TAG1 from mobile', 'TAG1');
end;

procedure TForm2.btnERRORClick(Sender: TObject);
begin
  Log.Error('This is a error message with TAG1 from mobile', 'TAG1');
end;

procedure TForm2.btnINFOClick(Sender: TObject);
begin
  Log.Info('This is a info message with TAG1 from mobile', 'TAG1');
end;

procedure TForm2.btnWARNINGClick(Sender: TObject);
begin
  Log.Warn('This is a warning message with TAG1 from mobile', 'TAG1');
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
{$IFDEF ANDROID}
  PermissionsService.RequestPermissions
    ([JStringtostring(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE)],
    procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
    begin
      if not((Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted)) then
      begin
        TDialogService.ShowMessage('LoggerPro will not work');
      end;
    end,
    procedure(const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc)
    begin
      TDialogService.ShowMessage('The app needs to access to the storage, please allows the next request',
       procedure(const AResult: TModalResult)
       begin
         APostRationaleProc();
       end
      );
    end
    );
{$ENDIF}
end;

end.
