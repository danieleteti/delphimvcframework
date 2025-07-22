program DMVCFrameworkGUITests;



uses
  Vcl.Forms,
  System.SysUtils,
  DUnitX.TestFramework,
  DUnitX.Loggers.GUI.VCL,
  ActiveRecordControllerTestU in 'ActiveRecordControllerTestU.pas',
  ActiveRecordTestsU in 'ActiveRecordTestsU.pas',
  FrameworkTestsU in 'FrameworkTestsU.pas',
  InjectorTestU in 'InjectorTestU.pas',
  IntfObjectPoolTestU in 'IntfObjectPoolTestU.pas',
  JSONRPCTestsU in 'JSONRPCTestsU.pas',
  LiveServerTestU in 'LiveServerTestU.pas',
  ObjectPoolTestU in 'ObjectPoolTestU.pas',
  RESTAdapterTestsU in 'RESTAdapterTestsU.pas',
  Serializers.JsonDataObjectsTestU in 'Serializers.JsonDataObjectsTestU.pas',
  StandaloneServerTestU in 'StandaloneServerTestU.pas',
  TestConstsU in 'TestConstsU.pas',
  TestControllersU in 'TestControllersU.pas',
  BusinessObjectsU in '..\..\..\samples\commons\BusinessObjectsU.pas',
  MVCFramework.Tests.Serializer.Entities in '..\..\common\MVCFramework.Tests.Serializer.Entities.pas',
  MVCFramework.Tests.Serializer.EntitiesModule in '..\..\common\MVCFramework.Tests.Serializer.EntitiesModule.pas',
  MVCFramework.Tests.Serializer.Intf in '..\..\common\MVCFramework.Tests.Serializer.Intf.pas',
  MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes in '..\..\..\..\bitpresenze\commons\dmvcframework\sources\MVCFramework.Serializer.JsonDataObjects.OptionalCustomTypes.pas',
  FDConnectionConfigU in '..\..\common\FDConnectionConfigU.pas',
  ActiveRecordControllerWebModuleU in 'webmodules\ActiveRecordControllerWebModuleU.pas',
  TestServerControllerU in '..\TestServer\TestServerControllerU.pas',
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.Tests.WebModule1 in '..\RESTClient\MVCFramework.Tests.WebModule1.pas' {TestWebModule1: TWebModule},
  MVCFramework.Tests.RESTClient in '..\RESTClient\MVCFramework.Tests.RESTClient.pas',
  MVCFramework.Tests.AppController in '..\RESTClient\MVCFramework.Tests.AppController.pas',
  TestDataSetCSVSerializer in 'TestDataSetCSVSerializer.pas',
  MVCFramework.BloomFilter in '..\..\..\sources\MVCFramework.BloomFilter.pas';

{$R *.res}

begin
  UseConsoleLogger := False;
  TMVCSqids.SQIDS_ALPHABET := 'axDlw8dRnsPCrbZIAEMFG4TQ6gc3iWtOy9v5NBz0LfSmuKV71JHkUhYpej2Xqo';
  TMVCSqids.SQIDS_MIN_LENGTH := 6;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'DUnitX GUI Test Runner';
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;
end.
