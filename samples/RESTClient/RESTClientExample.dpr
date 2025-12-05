program RESTClientExample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.Generics.Collections,
  MVCFramework.RESTClient,
  MVCFramework.RESTClient.Intf,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  JsonDataObjects,
  DTOs in 'DTOs.pas';

type
  // Helper class for organized examples
  TRESTClientExamples = class
  private
    FClient: IMVCRESTClient;
  public
    constructor Create;
    procedure BasicConfiguration;
    procedure SimpleGETRequest;
    procedure GETWithDeserialization;
    procedure GETUsersList;
    procedure POSTWithJSONBody;
    procedure PUTRequest;
    procedure DELETERequest;
    procedure CustomHeaders;
    procedure AuthenticationExamples;
    procedure RequestResponseHooks;
    procedure FileUploadExample;
    procedure URLEncodedBody;
    procedure ErrorHandling;
    procedure HTTPBinExamples;
  end;

{ TRESTClientExamples }

constructor TRESTClientExamples.Create;
begin
  inherited Create;
  // Basic client configuration
  FClient := TMVCRESTClient.New
    .BaseURL('https://jsonplaceholder.typicode.com')
    .ReadTimeOut(10000)
    .ConnectTimeout(5000)
    .Accept(TMVCMediaType.APPLICATION_JSON)
    .AcceptCharset(TMVCCharset.UTF_8)
    .UserAgent('DelphiMVCFramework Example Client 1.0');
end;

procedure TRESTClientExamples.BasicConfiguration;
begin
  WriteLn('=== BASIC CONFIGURATION ===');
  WriteLn('Base URL: ', FClient.BaseURL);
  WriteLn('Read Timeout: ', FClient.ReadTimeOut);
  WriteLn('Connect Timeout: ', FClient.ConnectTimeout);
  WriteLn('Accept: ', FClient.Accept);
  WriteLn('Accept Charset: ', FClient.AcceptCharset);
  WriteLn('User Agent: ', FClient.UserAgent);
  WriteLn;
end;

procedure TRESTClientExamples.SimpleGETRequest;
var
  lResponse: IMVCRESTResponse;
begin
  WriteLn('=== SIMPLE GET ===');
  try
    lResponse := FClient
      .Resource('/posts/1')
      .Get;
    
    WriteLn('Status Code: ', lResponse.StatusCode);
    WriteLn('Content Type: ', lResponse.ContentType);
    WriteLn('Content Length: ', lResponse.ContentLength);
    WriteLn('Response Body:');
    WriteLn(lResponse.Content);
    WriteLn;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end;

procedure TRESTClientExamples.GETWithDeserialization;
var
  lPost: TPost;
  lResponse: IMVCRESTResponse;
begin
  WriteLn('=== GET WITH DESERIALIZATION ===');
  try
    lPost := TPost.Create;
    try
      lResponse := FClient
        .Resource('/posts/1')
        .Get;
      
      // Deserialize the response into the object
      lResponse.BodyFor(lPost);
      
      WriteLn('Deserialized Post:');
      WriteLn('  ID: ', lPost.Id);
      WriteLn('  User ID: ', lPost.UserId);
      WriteLn('  Title: ', lPost.Title);
      WriteLn('  Body: ', Copy(lPost.Body, 1, 50), '...');
      WriteLn;
    finally
      lPost.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end;

procedure TRESTClientExamples.GETUsersList;
var
  lUsers: TObjectList<TUser>;
  lResponse: IMVCRESTResponse;
  lUser: TUser;
begin
  WriteLn('=== GET USERS LIST ===');
  try
    lUsers := TObjectList<TUser>.Create(True);
    try
      lResponse := FClient
        .Resource('/users')
        .Get;
      
      // Deserialize the list
      lResponse.BodyForListOf(lUsers, TUser);
      
      WriteLn('Found ', lUsers.Count, ' users:');
      for lUser in lUsers do
      begin
        WriteLn('  ', lUser.Id, ') ', lUser.Name, ' (@', lUser.Username, ') - ', lUser.Email);
      end;
      WriteLn;
    finally
      lUsers.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end;

procedure TRESTClientExamples.POSTWithJSONBody;
var
  lPost: TPost;
  lResponse: IMVCRESTResponse;
begin
  WriteLn('=== POST WITH JSON BODY ===');
  try
    lPost := TPost.Create;
    try
      lPost.UserId := 1;
      lPost.Title := 'Example Post from DelphiMVCFramework';
      lPost.Body := 'This is an example post created with DelphiMVCFramework RESTClient';
      
      lResponse := FClient
        .Resource('/posts')
        .AddBody(lPost, False)
        .Post;
      
      WriteLn('POST Response:');
      WriteLn('Status Code: ', lResponse.StatusCode);
      WriteLn('Response Body: ', lResponse.Content);
      WriteLn;
    finally
      lPost.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end;

procedure TRESTClientExamples.PUTRequest;
var
  lPost: TPost;
  lResponse: IMVCRESTResponse;
begin
  WriteLn('=== PUT REQUEST ===');
  try
    lPost := TPost.Create;
    try
      lPost.Id := 1;
      lPost.UserId := 1;
      lPost.Title := 'Updated Post';
      lPost.Body := 'Content updated via PUT';
      
      lResponse := FClient
        .Resource('/posts/1')
        .AddBody(lPost, False)
        .Put;
      
      WriteLn('PUT Response:');
      WriteLn('Status Code: ', lResponse.StatusCode);
      WriteLn('Response Body: ', lResponse.Content);
      WriteLn;
    finally
      lPost.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end;

procedure TRESTClientExamples.DELETERequest;
var
  lResponse: IMVCRESTResponse;
begin
  WriteLn('=== DELETE REQUEST ===');
  try
    lResponse := FClient
      .Resource('/posts/1')
      .Delete;
    
    WriteLn('DELETE Response:');
    WriteLn('Status Code: ', lResponse.StatusCode);
    WriteLn('Response Body: ', lResponse.Content);
    WriteLn;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end;

procedure TRESTClientExamples.CustomHeaders;
var
  lResponse: IMVCRESTResponse;
begin
  WriteLn('=== CUSTOM HEADERS ===');
  try
    lResponse := FClient
      .Resource('/posts/1')
      .AddHeader('X-Custom-Header', 'DelphiMVCFramework')
      .AddHeader('X-Request-ID', '12345')
      .Get;
    
    WriteLn('Request with custom headers:');
    WriteLn('Status Code: ', lResponse.StatusCode);
    WriteLn('Response Headers Count: ', lResponse.Headers.Count);
    WriteLn;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end;

procedure TRESTClientExamples.AuthenticationExamples;
var
  lClientAuth: IMVCRESTClient;
  lResponse: IMVCRESTResponse;
begin
  WriteLn('=== AUTHENTICATION EXAMPLES ===');
  
  // Basic Authentication
  lClientAuth := TMVCRESTClient.New
    .BaseURL('https://httpbin.org')
    .SetBasicAuthorization('testuser', 'testpass');
  
  WriteLn('Basic Auth Header: ', lClientAuth.Authorization);
  
  // Bearer Token
  lClientAuth.SetBearerAuthorization('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.test.token');
  WriteLn('Bearer Auth Header: ', lClientAuth.Authorization);
  
  // Test with httpbin
  try
    lResponse := lClientAuth
      .Resource('/bearer')
      .Get;
    WriteLn('Bearer test Status: ', lResponse.StatusCode);
  except
    on E: Exception do
      WriteLn('Bearer test failed: ', E.Message);
  end;
  
  WriteLn;
end;

procedure TRESTClientExamples.RequestResponseHooks;
var
  lResponse: IMVCRESTResponse;
begin
  WriteLn('=== REQUEST/RESPONSE HOOKS ===');
  try
    lResponse := FClient
      .Resource('/posts/1')
      .SetBeforeRequestProc(
        procedure(aRequest: IHTTPRequest)
        begin
          WriteLn('>> BEFORE REQUEST HOOK:');
          WriteLn('   URL: ', aRequest.URL.ToString);
          WriteLn('   Method: ', aRequest.MethodString);
        end
      )
      .SetResponseCompletedProc(
        procedure(aResponse: IMVCRESTResponse)
        begin
          WriteLn('>> RESPONSE COMPLETED HOOK:');
          WriteLn('   Status: ', aResponse.StatusCode);
          WriteLn('   Content Length: ', aResponse.ContentLength);
        end
      )
      .Get;
    
    WriteLn('Response received via hooks');
    WriteLn;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end;

procedure TRESTClientExamples.FileUploadExample;
var
  lResponse: IMVCRESTResponse;
  lTempFile: string;
  lStringList: TStringList;
begin
  WriteLn('=== FILE UPLOAD ===');
  try
    // Create a temporary file
    lTempFile := TPath.Combine(TPath.GetTempPath, 'test_upload.txt');
    lStringList := TStringList.Create;
    try
      lStringList.Add('This is a test file for DelphiMVCFramework');
      lStringList.Add('Upload created at: ' + DateTimeToStr(Now));
      lStringList.SaveToFile(lTempFile);
      
      // Change base URL to httpbin that supports upload
      lResponse := TMVCRESTClient.New
        .BaseURL('https://httpbin.org')
        .AddFile(lTempFile)
        .Post('/post');
      
      WriteLn('File Upload Response:');
      WriteLn('Status Code: ', lResponse.StatusCode);
      WriteLn('Upload completed successfully');
      WriteLn;
    finally
      lStringList.Free;
      if FileExists(lTempFile) then
        DeleteFile(lTempFile);
    end;
  except
    on E: Exception do
      WriteLn('Upload error: ', E.Message);
  end;
end;

procedure TRESTClientExamples.URLEncodedBody;
var
  lResponse: IMVCRESTResponse;
begin
  WriteLn('=== URL ENCODED BODY ===');
  try
    lResponse := TMVCRESTClient.New
      .BaseURL('https://httpbin.org')
      .AddBodyFieldURLEncoded('field1', 'value1')
      .AddBodyFieldURLEncoded('field2', 'Daniele Teti')
      .AddBodyFieldURLEncoded('framework', 'DelphiMVCFramework')
      .Post('/post');
    
    WriteLn('URL Encoded Response:');
    WriteLn('Status Code: ', lResponse.StatusCode);
    WriteLn('Content Type: ', lResponse.ContentType);
    WriteLn;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end;

procedure TRESTClientExamples.ErrorHandling;
var
  lResponse: IMVCRESTResponse;
begin
  WriteLn('=== ERROR HANDLING ===');
  
  // Test 404
  try
    lResponse := FClient
      .Resource('/posts/999999')
      .Get;
    WriteLn('404 Test - Status Code: ', lResponse.StatusCode);
  except
    on E: Exception do
      WriteLn('404 Exception: ', E.Message);
  end;
  
  // Test timeout (with non-existent URL)
  try
    lResponse := TMVCRESTClient.New
      .BaseURL('https://httpbin.org')
      .ReadTimeOut(1) // 1ms timeout to force timeout
      .Resource('/delay/5') // 5 seconds delay
      .Get;
  except
    on E: Exception do
      WriteLn('Timeout Exception: ', E.Message);
  end;
  
  WriteLn;
end;

procedure TRESTClientExamples.HTTPBinExamples;
var
  lClient: IMVCRESTClient;
  lResponse: IMVCRESTResponse;
  lJson: TJDOJsonObject;
begin
  WriteLn('=== HTTPBIN EXAMPLES ===');
  
  lClient := TMVCRESTClient.New.BaseURL('https://httpbin.org');
  
  // Test headers
  try
    lResponse := lClient
      .Resource('/headers')
      .AddHeader('X-Test-Header', 'DelphiMVCFramework')
      .Get;
    
    WriteLn('Headers Test Status: ', lResponse.StatusCode);
    
    // Parse JSON response to see sent headers
    lJson := TJDOJsonBaseObject.Parse(lResponse.Content) as TJDOJsonObject;
    try
      WriteLn('User-Agent sent: ', lJson.O['headers'].S['User-Agent']);
      if lJson.O['headers'].Contains('X-Test-Header') then
        WriteLn('Custom Header received: ', lJson.O['headers'].S['X-Test-Header']);
    finally
      lJson.Free;
    end;
    
  except
    on E: Exception do
      WriteLn('Headers test failed: ', E.Message);
  end;
  
  // Test IP
  try
    lResponse := lClient
      .Resource('/ip')
      .Get;
    WriteLn('IP Response: ', lResponse.Content);
  except
    on E: Exception do
      WriteLn('IP test failed: ', E.Message);
  end;
  
  WriteLn;
end;

// Main Program
procedure RunExamples;
var
  lExamples: TRESTClientExamples;
begin
  WriteLn('DelphiMVCFramework RESTClient - Complete Examples');
  WriteLn('================================================');
  WriteLn;
  
  lExamples := TRESTClientExamples.Create;
  try
    lExamples.BasicConfiguration;
    lExamples.SimpleGETRequest;
    lExamples.GETWithDeserialization;
    lExamples.GETUsersList;
    lExamples.POSTWithJSONBody;
    lExamples.PUTRequest;
    lExamples.DELETERequest;
    lExamples.CustomHeaders;
    lExamples.AuthenticationExamples;
    lExamples.RequestResponseHooks;
    lExamples.URLEncodedBody;
    lExamples.FileUploadExample;
    lExamples.HTTPBinExamples;
    lExamples.ErrorHandling;
  finally
    lExamples.Free;
  end;
  
  WriteLn('================================================');
  WriteLn('Examples completed. Press ENTER to exit...');
  ReadLn;
end;

begin
  try
    RunExamples;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.
