// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

unit LoggerTestsU;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  MVCFramework.Logger,
  MVCFramework.Commons,
  LoggerPro,
  LoggerPro.Builder,
  LoggerPro.MemoryAppender;

type
  /// <summary>
  /// Test object for logging
  /// </summary>
  TTestPerson = class
  private
    FName: string;
    FAge: Integer;
  public
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
  end;

  [TestFixture]
  TTestLogger = class
  private
    FMemoryAppender: TLoggerProMemoryRingBufferAppender;
    FTestLogger: ILogWriter;
    FOriginalVerbosityLevel: TLogLevel;
    function GetLastLogItem: TLogItem;
    function GetLastLogMessage: string;
    function GetLastLogTag: string;
    function GetLastLogType: TLogType;
  public
    [SetUp]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // ====== Log Level String Conversion Tests ======
    [Test]
    procedure TestLogLevelToStr;
    [Test]
    procedure TestStrToLogLevel;
    [Test]
    procedure TestStrToLogLevel_WithPrefix;
    [Test]
    procedure TestStrToLogLevel_Invalid;

    // ====== Basic Log Functions Tests ======
    [Test]
    procedure TestLogD_SimpleMessage;
    [Test]
    procedure TestLogI_SimpleMessage;
    [Test]
    procedure TestLogW_SimpleMessage;
    [Test]
    procedure TestLogE_SimpleMessage;
    [Test]
    procedure TestLogF_SimpleMessage;

    // ====== Format String Overloads Tests ======
    [Test]
    procedure TestLogD_FormatString;
    [Test]
    procedure TestLogI_FormatString;
    [Test]
    procedure TestLogW_FormatString;
    [Test]
    procedure TestLogE_FormatString;
    [Test]
    procedure TestLogF_FormatString;

    // ====== Custom Tag Overloads Tests ======
    [Test]
    procedure TestLogD_WithTag;
    [Test]
    procedure TestLogI_WithTag;
    [Test]
    procedure TestLogW_WithTag;
    [Test]
    procedure TestLogE_WithTag;
    [Test]
    procedure TestLogF_WithTag;

    // ====== Format String + Tag Overloads Tests ======
    [Test]
    procedure TestLogD_FormatStringWithTag;
    [Test]
    procedure TestLogI_FormatStringWithTag;
    [Test]
    procedure TestLogW_FormatStringWithTag;
    [Test]
    procedure TestLogE_FormatStringWithTag;
    [Test]
    procedure TestLogF_FormatStringWithTag;

    // ====== Message + Object Overloads Tests ======
    [Test]
    procedure TestLogD_WithObject;
    [Test]
    procedure TestLogI_WithObject;
    [Test]
    procedure TestLogW_WithObject;
    [Test]
    procedure TestLogE_WithObject;
    [Test]
    procedure TestLogF_WithObject;

    // ====== Object Only Overloads Tests ======
    [Test]
    procedure TestLogD_ObjectOnly;
    [Test]
    procedure TestLogI_ObjectOnly;
    [Test]
    procedure TestLogW_ObjectOnly;

    // ====== Exception Logging Tests ======
    [Test]
    procedure TestLogE_Exception;
    [Test]
    procedure TestLogE_ExceptionWithMessage;
    [Test]
    procedure TestLogException;

    // ====== Log Level Check Helpers Tests ======
    [Test]
    procedure TestIsDebugEnabled_WhenDebugLevel;
    [Test]
    procedure TestIsDebugEnabled_WhenInfoLevel;
    [Test]
    procedure TestIsInfoEnabled_WhenDebugLevel;
    [Test]
    procedure TestIsInfoEnabled_WhenWarningLevel;
    [Test]
    procedure TestIsWarningEnabled_WhenDebugLevel;
    [Test]
    procedure TestIsWarningEnabled_WhenErrorLevel;
    [Test]
    procedure TestIsErrorEnabled_WhenDebugLevel;
    [Test]
    procedure TestIsErrorEnabled_WhenFatalLevel;

    // ====== Enter/Exit Method Logging Tests ======
    [Test]
    procedure TestLogEnterMethod;
    [Test]
    procedure TestLogExitMethod;

    // ====== Log by Level Tests ======
    [Test]
    procedure TestLogByLevel_Debug;
    [Test]
    procedure TestLogByLevel_Normal;
    [Test]
    procedure TestLogByLevel_Warning;
    [Test]
    procedure TestLogByLevel_Error;
    [Test]
    procedure TestLogByLevel_Fatal;

    // ====== Builder Configuration Tests ======
    [Test]
    procedure TestCreateLogBuilderWithDefaultConfiguration;
    [Test]
    procedure TestCreateNullLogger;
  end;

implementation

uses
  System.IOUtils;

{ TTestLogger }

procedure TTestLogger.SetUp;
begin
  FOriginalVerbosityLevel := UseLoggerVerbosityLevel;
  UseLoggerVerbosityLevel := TLogLevel.levDebug;

  // Release any existing global logger first
  ReleaseGlobalLogger;

  FMemoryAppender := TLoggerProMemoryRingBufferAppender.Create(1000);
  FTestLogger := LoggerProBuilder
    .WithMinimumLevel(TLogType.Debug)
    .WriteToAppender(FMemoryAppender)
    .Build;

  // Set test logger as default
  SetDefaultLogger(FTestLogger);
end;

procedure TTestLogger.TearDown;
begin
  UseLoggerVerbosityLevel := FOriginalVerbosityLevel;
  // Logger will be released when FTestLogger goes out of scope
end;

function TTestLogger.GetLastLogItem: TLogItem;
var
  LItems: TList<TLogItem>;
begin
  Result := nil;
  LItems := FMemoryAppender.GetLogItems;
  try
    if LItems.Count > 0 then
      Result := LItems[LItems.Count - 1].Clone;
  finally
    // Free all cloned items in the list
    for var I := 0 to LItems.Count - 1 do
      LItems[I].Free;
    LItems.Free;
  end;
end;

function TTestLogger.GetLastLogMessage: string;
var
  LItem: TLogItem;
begin
  Result := '';
  LItem := GetLastLogItem;
  if Assigned(LItem) then
  try
    Result := LItem.LogMessage;
  finally
    LItem.Free;
  end;
end;

function TTestLogger.GetLastLogTag: string;
var
  LItem: TLogItem;
begin
  Result := '';
  LItem := GetLastLogItem;
  if Assigned(LItem) then
  try
    Result := LItem.LogTag;
  finally
    LItem.Free;
  end;
end;

function TTestLogger.GetLastLogType: TLogType;
var
  LItem: TLogItem;
begin
  Result := TLogType.Debug;
  LItem := GetLastLogItem;
  if Assigned(LItem) then
  try
    Result := LItem.LogType;
  finally
    LItem.Free;
  end;
end;

// ====== Log Level String Conversion Tests ======

procedure TTestLogger.TestLogLevelToStr;
begin
  Assert.AreEqual('DEBUG', LogLevelToStr(TLogLevel.levDebug));
  Assert.AreEqual('', LogLevelToStr(TLogLevel.levNormal)); // Normal is empty for readability
  Assert.AreEqual('WARNING', LogLevelToStr(TLogLevel.levWarning));
  Assert.AreEqual('ERROR', LogLevelToStr(TLogLevel.levError));
  Assert.AreEqual('EXCEPTION', LogLevelToStr(TLogLevel.levException));
  Assert.AreEqual('FATAL', LogLevelToStr(TLogLevel.levFatal));
end;

procedure TTestLogger.TestStrToLogLevel;
begin
  Assert.AreEqual(TLogLevel.levDebug, StrToLogLevel('debug'));
  Assert.AreEqual(TLogLevel.levNormal, StrToLogLevel('normal'));
  Assert.AreEqual(TLogLevel.levNormal, StrToLogLevel('info'));
  Assert.AreEqual(TLogLevel.levWarning, StrToLogLevel('warning'));
  Assert.AreEqual(TLogLevel.levError, StrToLogLevel('error'));
  Assert.AreEqual(TLogLevel.levException, StrToLogLevel('exception'));
  Assert.AreEqual(TLogLevel.levFatal, StrToLogLevel('fatal'));
  Assert.AreEqual(TLogLevel.levDebug, StrToLogLevel('')); // Empty defaults to debug
end;

procedure TTestLogger.TestStrToLogLevel_WithPrefix;
begin
  Assert.AreEqual(TLogLevel.levDebug, StrToLogLevel('levDebug'));
  Assert.AreEqual(TLogLevel.levNormal, StrToLogLevel('levNormal'));
  Assert.AreEqual(TLogLevel.levWarning, StrToLogLevel('levWarning'));
  Assert.AreEqual(TLogLevel.levError, StrToLogLevel('levError'));
  Assert.AreEqual(TLogLevel.levFatal, StrToLogLevel('levFatal'));
end;

procedure TTestLogger.TestStrToLogLevel_Invalid;
begin
  Assert.WillRaiseWithMessage(
    procedure
    begin
      StrToLogLevel('invalid_level');
    end,
    EMVCConfigException,
    'Invalid log level: invalid_level');
end;

// ====== Basic Log Functions Tests ======

procedure TTestLogger.TestLogD_SimpleMessage;
begin
  FMemoryAppender.Clear;
  LogD('Debug message');
  Sleep(100); // Allow async logging
  Assert.AreEqual('Debug message', GetLastLogMessage);
  Assert.AreEqual(TLogType.Debug, GetLastLogType);
end;

procedure TTestLogger.TestLogI_SimpleMessage;
begin
  FMemoryAppender.Clear;
  LogI('Info message');
  Sleep(100);
  Assert.AreEqual('Info message', GetLastLogMessage);
  Assert.AreEqual(TLogType.Info, GetLastLogType);
end;

procedure TTestLogger.TestLogW_SimpleMessage;
begin
  FMemoryAppender.Clear;
  LogW('Warning message');
  Sleep(100);
  Assert.AreEqual('Warning message', GetLastLogMessage);
  Assert.AreEqual(TLogType.Warning, GetLastLogType);
end;

procedure TTestLogger.TestLogE_SimpleMessage;
begin
  FMemoryAppender.Clear;
  LogE('Error message');
  Sleep(100);
  Assert.AreEqual('Error message', GetLastLogMessage);
  Assert.AreEqual(TLogType.Error, GetLastLogType);
end;

procedure TTestLogger.TestLogF_SimpleMessage;
begin
  FMemoryAppender.Clear;
  LogF('Fatal message');
  Sleep(100);
  Assert.AreEqual('Fatal message', GetLastLogMessage);
  Assert.AreEqual(TLogType.Fatal, GetLastLogType);
end;

// ====== Format String Overloads Tests ======

procedure TTestLogger.TestLogD_FormatString;
begin
  FMemoryAppender.Clear;
  LogD('User %s logged in with ID %d', ['john', 123]);
  Sleep(100);
  Assert.AreEqual('User john logged in with ID 123', GetLastLogMessage);
  Assert.AreEqual(TLogType.Debug, GetLastLogType);
end;

procedure TTestLogger.TestLogI_FormatString;
begin
  FMemoryAppender.Clear;
  LogI('Processing %d items', [42]);
  Sleep(100);
  Assert.AreEqual('Processing 42 items', GetLastLogMessage);
  Assert.AreEqual(TLogType.Info, GetLastLogType);
end;

procedure TTestLogger.TestLogW_FormatString;
begin
  FMemoryAppender.Clear;
  LogW('Memory usage at %d%%', [85]);
  Sleep(100);
  Assert.AreEqual('Memory usage at 85%', GetLastLogMessage);
  Assert.AreEqual(TLogType.Warning, GetLastLogType);
end;

procedure TTestLogger.TestLogE_FormatString;
begin
  FMemoryAppender.Clear;
  LogE('Failed to connect to %s:%d', ['localhost', 8080]);
  Sleep(100);
  Assert.AreEqual('Failed to connect to localhost:8080', GetLastLogMessage);
  Assert.AreEqual(TLogType.Error, GetLastLogType);
end;

procedure TTestLogger.TestLogF_FormatString;
begin
  FMemoryAppender.Clear;
  LogF('System crash in module %s', ['CoreEngine']);
  Sleep(100);
  Assert.AreEqual('System crash in module CoreEngine', GetLastLogMessage);
  Assert.AreEqual(TLogType.Fatal, GetLastLogType);
end;

// ====== Custom Tag Overloads Tests ======

procedure TTestLogger.TestLogD_WithTag;
begin
  FMemoryAppender.Clear;
  LogD('Debug with custom tag', 'CUSTOM_TAG');
  Sleep(100);
  Assert.AreEqual('Debug with custom tag', GetLastLogMessage);
  Assert.AreEqual('CUSTOM_TAG', GetLastLogTag);
end;

procedure TTestLogger.TestLogI_WithTag;
begin
  FMemoryAppender.Clear;
  LogI('Info with custom tag', 'MY_MODULE');
  Sleep(100);
  Assert.AreEqual('Info with custom tag', GetLastLogMessage);
  Assert.AreEqual('MY_MODULE', GetLastLogTag);
end;

procedure TTestLogger.TestLogW_WithTag;
begin
  FMemoryAppender.Clear;
  LogW('Warning with custom tag', 'SECURITY');
  Sleep(100);
  Assert.AreEqual('Warning with custom tag', GetLastLogMessage);
  Assert.AreEqual('SECURITY', GetLastLogTag);
end;

procedure TTestLogger.TestLogE_WithTag;
begin
  FMemoryAppender.Clear;
  LogE('Error with custom tag', 'DATABASE');
  Sleep(100);
  Assert.AreEqual('Error with custom tag', GetLastLogMessage);
  Assert.AreEqual('DATABASE', GetLastLogTag);
end;

procedure TTestLogger.TestLogF_WithTag;
begin
  FMemoryAppender.Clear;
  LogF('Fatal with custom tag', 'CRITICAL');
  Sleep(100);
  Assert.AreEqual('Fatal with custom tag', GetLastLogMessage);
  Assert.AreEqual('CRITICAL', GetLastLogTag);
end;

// ====== Format String + Tag Overloads Tests ======

procedure TTestLogger.TestLogD_FormatStringWithTag;
begin
  FMemoryAppender.Clear;
  LogD('User %s action', ['admin'], 'AUTH');
  Sleep(100);
  Assert.AreEqual('User admin action', GetLastLogMessage);
  Assert.AreEqual('AUTH', GetLastLogTag);
end;

procedure TTestLogger.TestLogI_FormatStringWithTag;
begin
  FMemoryAppender.Clear;
  LogI('Loaded %d records', [100], 'DATA');
  Sleep(100);
  Assert.AreEqual('Loaded 100 records', GetLastLogMessage);
  Assert.AreEqual('DATA', GetLastLogTag);
end;

procedure TTestLogger.TestLogW_FormatStringWithTag;
begin
  FMemoryAppender.Clear;
  LogW('Cache miss for key %s', ['user_123'], 'CACHE');
  Sleep(100);
  Assert.AreEqual('Cache miss for key user_123', GetLastLogMessage);
  Assert.AreEqual('CACHE', GetLastLogTag);
end;

procedure TTestLogger.TestLogE_FormatStringWithTag;
begin
  FMemoryAppender.Clear;
  LogE('Query failed: %s', ['timeout'], 'SQL');
  Sleep(100);
  Assert.AreEqual('Query failed: timeout', GetLastLogMessage);
  Assert.AreEqual('SQL', GetLastLogTag);
end;

procedure TTestLogger.TestLogF_FormatStringWithTag;
begin
  FMemoryAppender.Clear;
  LogF('Disk %s is full', ['C:'], 'STORAGE');
  Sleep(100);
  Assert.AreEqual('Disk C: is full', GetLastLogMessage);
  Assert.AreEqual('STORAGE', GetLastLogTag);
end;

// ====== Message + Object Overloads Tests ======

procedure TTestLogger.TestLogD_WithObject;
var
  LPerson: TTestPerson;
begin
  LPerson := TTestPerson.Create;
  try
    LPerson.Name := 'John';
    LPerson.Age := 30;
    FMemoryAppender.Clear;
    LogD('Person data:', LPerson);
    Sleep(100);
    Assert.IsTrue(GetLastLogMessage.Contains('Person data:'));
    Assert.IsTrue(GetLastLogMessage.Contains('John'));
    Assert.IsTrue(GetLastLogMessage.Contains('30'));
  finally
    LPerson.Free;
  end;
end;

procedure TTestLogger.TestLogI_WithObject;
var
  LPerson: TTestPerson;
begin
  LPerson := TTestPerson.Create;
  try
    LPerson.Name := 'Jane';
    LPerson.Age := 25;
    FMemoryAppender.Clear;
    LogI('User info:', LPerson);
    Sleep(100);
    Assert.IsTrue(GetLastLogMessage.Contains('User info:'));
    Assert.IsTrue(GetLastLogMessage.Contains('Jane'));
    Assert.AreEqual(TLogType.Info, GetLastLogType);
  finally
    LPerson.Free;
  end;
end;

procedure TTestLogger.TestLogW_WithObject;
var
  LPerson: TTestPerson;
begin
  LPerson := TTestPerson.Create;
  try
    LPerson.Name := 'Bob';
    LPerson.Age := 99;
    FMemoryAppender.Clear;
    LogW('Suspicious user:', LPerson);
    Sleep(100);
    Assert.IsTrue(GetLastLogMessage.Contains('Suspicious user:'));
    Assert.IsTrue(GetLastLogMessage.Contains('Bob'));
    Assert.AreEqual(TLogType.Warning, GetLastLogType);
  finally
    LPerson.Free;
  end;
end;

procedure TTestLogger.TestLogE_WithObject;
var
  LPerson: TTestPerson;
begin
  LPerson := TTestPerson.Create;
  try
    LPerson.Name := 'Error User';
    LPerson.Age := 0;
    FMemoryAppender.Clear;
    LogE('Invalid user:', LPerson);
    Sleep(100);
    Assert.IsTrue(GetLastLogMessage.Contains('Invalid user:'));
    Assert.AreEqual(TLogType.Error, GetLastLogType);
  finally
    LPerson.Free;
  end;
end;

procedure TTestLogger.TestLogF_WithObject;
var
  LPerson: TTestPerson;
begin
  LPerson := TTestPerson.Create;
  try
    LPerson.Name := 'Critical';
    LPerson.Age := -1;
    FMemoryAppender.Clear;
    LogF('Fatal user state:', LPerson);
    Sleep(100);
    Assert.IsTrue(GetLastLogMessage.Contains('Fatal user state:'));
    Assert.AreEqual(TLogType.Fatal, GetLastLogType);
  finally
    LPerson.Free;
  end;
end;

// ====== Object Only Overloads Tests ======

procedure TTestLogger.TestLogD_ObjectOnly;
var
  LPerson: TTestPerson;
begin
  LPerson := TTestPerson.Create;
  try
    LPerson.Name := 'DebugPerson';
    LPerson.Age := 20;
    FMemoryAppender.Clear;
    LogD(LPerson);
    Sleep(100);
    Assert.IsTrue(GetLastLogMessage.Contains('DebugPerson'));
    Assert.IsTrue(GetLastLogMessage.Contains('TTestPerson'));
    Assert.AreEqual(TLogType.Debug, GetLastLogType);
  finally
    LPerson.Free;
  end;
end;

procedure TTestLogger.TestLogI_ObjectOnly;
var
  LPerson: TTestPerson;
begin
  LPerson := TTestPerson.Create;
  try
    LPerson.Name := 'InfoPerson';
    LPerson.Age := 21;
    FMemoryAppender.Clear;
    LogI(LPerson);
    Sleep(100);
    Assert.IsTrue(GetLastLogMessage.Contains('InfoPerson'));
    Assert.AreEqual(TLogType.Info, GetLastLogType);
  finally
    LPerson.Free;
  end;
end;

procedure TTestLogger.TestLogW_ObjectOnly;
var
  LPerson: TTestPerson;
begin
  LPerson := TTestPerson.Create;
  try
    LPerson.Name := 'WarnPerson';
    LPerson.Age := 22;
    FMemoryAppender.Clear;
    LogW(LPerson);
    Sleep(100);
    Assert.IsTrue(GetLastLogMessage.Contains('WarnPerson'));
    Assert.AreEqual(TLogType.Warning, GetLastLogType);
  finally
    LPerson.Free;
  end;
end;

// ====== Exception Logging Tests ======

procedure TTestLogger.TestLogE_Exception;
var
  LException: Exception;
begin
  LException := Exception.Create('Test exception message');
  try
    FMemoryAppender.Clear;
    LogE(LException);
    Sleep(100);
    Assert.IsTrue(GetLastLogMessage.Contains('Test exception message'));
    Assert.AreEqual(TLogType.Error, GetLastLogType);
  finally
    LException.Free;
  end;
end;

procedure TTestLogger.TestLogE_ExceptionWithMessage;
var
  LException: Exception;
begin
  LException := Exception.Create('Original error');
  try
    FMemoryAppender.Clear;
    LogE(LException, 'Context info');
    Sleep(100);
    Assert.IsTrue(GetLastLogMessage.Contains('Original error') or
                  GetLastLogMessage.Contains('Context info'));
    Assert.AreEqual(TLogType.Error, GetLastLogType);
  finally
    LException.Free;
  end;
end;

procedure TTestLogger.TestLogException;
var
  LException: Exception;
begin
  LException := Exception.Create('LogException test');
  try
    FMemoryAppender.Clear;
    LogException(LException);
    Sleep(100);
    Assert.IsTrue(GetLastLogMessage.Contains('LogException test'));
    Assert.AreEqual(TLogType.Error, GetLastLogType);
  finally
    LException.Free;
  end;
end;

// ====== Log Level Check Helpers Tests ======

procedure TTestLogger.TestIsDebugEnabled_WhenDebugLevel;
begin
  UseLoggerVerbosityLevel := TLogLevel.levDebug;
  Assert.IsTrue(IsDebugEnabled, 'Debug should be enabled when level is Debug');
end;

procedure TTestLogger.TestIsDebugEnabled_WhenInfoLevel;
begin
  UseLoggerVerbosityLevel := TLogLevel.levNormal;
  Assert.IsFalse(IsDebugEnabled, 'Debug should be disabled when level is Info/Normal');
end;

procedure TTestLogger.TestIsInfoEnabled_WhenDebugLevel;
begin
  UseLoggerVerbosityLevel := TLogLevel.levDebug;
  Assert.IsTrue(IsInfoEnabled, 'Info should be enabled when level is Debug');
end;

procedure TTestLogger.TestIsInfoEnabled_WhenWarningLevel;
begin
  UseLoggerVerbosityLevel := TLogLevel.levWarning;
  Assert.IsFalse(IsInfoEnabled, 'Info should be disabled when level is Warning');
end;

procedure TTestLogger.TestIsWarningEnabled_WhenDebugLevel;
begin
  UseLoggerVerbosityLevel := TLogLevel.levDebug;
  Assert.IsTrue(IsWarningEnabled, 'Warning should be enabled when level is Debug');
end;

procedure TTestLogger.TestIsWarningEnabled_WhenErrorLevel;
begin
  UseLoggerVerbosityLevel := TLogLevel.levError;
  Assert.IsFalse(IsWarningEnabled, 'Warning should be disabled when level is Error');
end;

procedure TTestLogger.TestIsErrorEnabled_WhenDebugLevel;
begin
  UseLoggerVerbosityLevel := TLogLevel.levDebug;
  Assert.IsTrue(IsErrorEnabled, 'Error should be enabled when level is Debug');
end;

procedure TTestLogger.TestIsErrorEnabled_WhenFatalLevel;
begin
  UseLoggerVerbosityLevel := TLogLevel.levFatal;
  Assert.IsFalse(IsErrorEnabled, 'Error should be disabled when level is Fatal');
end;

// ====== Enter/Exit Method Logging Tests ======

procedure TTestLogger.TestLogEnterMethod;
begin
  FMemoryAppender.Clear;
  LogEnterMethod('TestMethod');
  Sleep(100);
  Assert.IsTrue(GetLastLogMessage.Contains('>>'));
  Assert.IsTrue(GetLastLogMessage.Contains('TestMethod'));
  Assert.AreEqual(TLogType.Info, GetLastLogType);
end;

procedure TTestLogger.TestLogExitMethod;
begin
  FMemoryAppender.Clear;
  LogExitMethod('TestMethod');
  Sleep(100);
  Assert.IsTrue(GetLastLogMessage.Contains('<<'));
  Assert.IsTrue(GetLastLogMessage.Contains('TestMethod'));
  Assert.AreEqual(TLogType.Info, GetLastLogType);
end;

// ====== Log by Level Tests ======

procedure TTestLogger.TestLogByLevel_Debug;
begin
  FMemoryAppender.Clear;
  MVCFramework.Logger.Log(TLogLevel.levDebug, 'Debug by level');
  Sleep(100);
  Assert.AreEqual('Debug by level', GetLastLogMessage);
  Assert.AreEqual(TLogType.Debug, GetLastLogType);
end;

procedure TTestLogger.TestLogByLevel_Normal;
begin
  FMemoryAppender.Clear;
  MVCFramework.Logger.Log(TLogLevel.levNormal, 'Info by level');
  Sleep(100);
  Assert.AreEqual('Info by level', GetLastLogMessage);
  Assert.AreEqual(TLogType.Info, GetLastLogType);
end;

procedure TTestLogger.TestLogByLevel_Warning;
begin
  FMemoryAppender.Clear;
  MVCFramework.Logger.Log(TLogLevel.levWarning, 'Warning by level');
  Sleep(100);
  Assert.AreEqual('Warning by level', GetLastLogMessage);
  Assert.AreEqual(TLogType.Warning, GetLastLogType);
end;

procedure TTestLogger.TestLogByLevel_Error;
begin
  FMemoryAppender.Clear;
  MVCFramework.Logger.Log(TLogLevel.levError, 'Error by level');
  Sleep(100);
  Assert.AreEqual('Error by level', GetLastLogMessage);
  Assert.AreEqual(TLogType.Error, GetLastLogType);
end;

procedure TTestLogger.TestLogByLevel_Fatal;
begin
  FMemoryAppender.Clear;
  MVCFramework.Logger.Log(TLogLevel.levFatal, 'Fatal by level');
  Sleep(100);
  Assert.AreEqual('Fatal by level', GetLastLogMessage);
  Assert.AreEqual(TLogType.Fatal, GetLastLogType);
end;

// ====== Builder Configuration Tests ======

procedure TTestLogger.TestCreateLogBuilderWithDefaultConfiguration;
var
  LBuilder: ILoggerProBuilder;
  LLogger: ILogWriter;
begin
  LBuilder := CreateLogBuilderWithDefaultConfiguration;
  Assert.IsNotNull(LBuilder, 'Builder should not be nil');
  // Builder should be usable
  LLogger := LBuilder.Build;
  Assert.IsNotNull(LLogger, 'Logger from builder should not be nil');
end;

procedure TTestLogger.TestCreateNullLogger;
var
  LLogger: ILogWriter;
begin
  // CreateNullLogger should return a working logger that discards all messages
  LLogger := CreateNullLogger;
  Assert.IsNotNull(LLogger, 'Null logger should not be nil');
  // Null logger should accept logs without crashing
  LLogger.Info('Test', 'tag');
  LLogger.Debug('Test', 'tag');
  LLogger.Warn('Test', 'tag');
  LLogger.Error('Test', 'tag');
  LLogger.Fatal('Test', 'tag');
  // If we get here without exceptions, the null logger works correctly
  Assert.Pass('Null logger accepts all log calls without errors');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestLogger);

end.
