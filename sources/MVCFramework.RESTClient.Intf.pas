unit MVCFramework.RESTClient.Intf;

interface

uses
  System.Classes,
  System.SysUtils,
  MVCFramework.Serializer.Intf,
  REST.Types,
  Data.DB,
  MVCFramework.Serializer.Commons;

type
  TRESTContentType = REST.Types.TRESTContentType;

  IRESTResponse = interface;

  IRESTClient = interface
    ['{592BC90F-B825-4B3B-84A7-6CA3927BAD69}']

    function BaseURL: string; overload;
    function BaseURL(const aBaseURL: string): IRESTClient; overload;
    function Timeout: Integer; overload;
    function Timeout(const aTimeout: Integer): IRESTClient; overload;
    function RaiseExceptionOn500: Boolean; overload;
    function RaiseExceptionOn500(const aRaiseExceptionOn500: Boolean): IRESTClient; overload;

    function ProxyServer: string; overload;
    function ProxyServer(const aProxyServer: string): IRESTClient; overload;
    function ProxyPort: Integer; overload;
    function ProxyPort(const aProxyPort: Integer): IRESTClient; overload;
    function ProxyUsername: string; overload;
    function ProxyUsername(const aProxyUsername: string): IRESTClient; overload;
    function ProxyPassword: string; overload;
    function ProxyPassword(const aProxyPassword: string): IRESTClient; overload;

    function SetBasicAuthorization(const aUsername, aPassword: string): IRESTClient;
    function SetBearerAuthorization(const aToken: string): IRESTClient;

    function AddHeader(const aName, aValue: string): IRESTClient; overload;
    function ClearHeaders: IRESTClient;

    function AddCookie(const aName, aValue: string): IRESTClient;
    function ClearCookies: IRESTClient;

    function AddPathParam(const aName, aValue: string): IRESTClient; overload;
    function AddPathParam(const aName: string; aValue: Integer): IRESTClient; overload;
    function AddPathParam(const aName: string; aValue: Int64): IRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TGUID): IRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TDateTime): IRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TDate): IRESTClient; overload;
    function AddPathParam(const aName: string; aValue: TTime): IRESTClient; overload;
    function AddPathParam(const aName: string; aValue: Double): IRESTClient; overload;
    function ClearPathParams: IRESTClient;

    function AddQueryStringParam(const aName, aValue: string): IRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: Integer): IRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: Int64): IRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TGUID): IRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TDateTime): IRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TDate): IRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: TTime): IRESTClient; overload;
    function AddQueryStringParam(const aName: string; aValue: Double): IRESTClient; overload;
    function ClearQueryParams: IRESTClient;

    function Accept: string; overload;
    function Accept(const aAccept: string): IRESTClient; overload;
    function AcceptCharset: string; overload;
    function AcceptCharset(const aAcceptCharset: string): IRESTClient; overload;
    function AcceptEncoding: string; overload;
    function AcceptEncoding(const aAcceptEncoding: string): IRESTClient; overload;

    function Resource: string; overload;
    function Resource(const aResource: string): IRESTClient; overload;

    function AddBody(const aBody: string;
      const aContentType: TRESTContentType = ctAPPLICATION_JSON): IRESTClient; overload;
    function AddBody(aBodyStream: TStream; const aContentType: TRESTContentType = ctNone;
      aOwnsStream: Boolean = True): IRESTClient; overload;
    function AddBody(aBodyObject: TObject; const aOwnsObject: Boolean = True): IRESTClient; overload;
    function ClearBody: IRESTClient;

    function AddFile(const aName, aFileName: string;
      const aContentType: TRESTContentType = ctNone): IRESTClient; overload;
    function AddFile(const aFileName: string; const aContentType: TRESTContentType = ctNone): IRESTClient; overload;
    function ClearFiles: IRESTClient;

    function Get: IRESTResponse; overload;
    function Get(const aResource: string): IRESTResponse; overload;

    function Post: IRESTResponse; overload;
    function Post(const aResource: string; const aBody: string = ''): IRESTResponse; overload;
    function Post(const aResource: string; aBody: TObject; const aOwnsBody: Boolean = True): IRESTResponse; overload;

    function Patch: IRESTResponse; overload;
    function Patch(const aResource: string; const aBody: string = ''): IRESTResponse; overload;
    function Patch(const aResource: string; aBody: TObject; const aOwnsBody: Boolean = True): IRESTResponse; overload;

    function Put: IRESTResponse; overload;
    function Put(const aResource: string; const aBody: string = ''): IRESTResponse; overload;
    function Put(const aResource: string; aBody: TObject; const aOwnsBody: Boolean = True): IRESTResponse; overload;

    function Delete: IRESTResponse; overload;
    function Delete(const aResource: string): IRESTResponse; overload;

    function DataSetInsert(const aResource: string; aDataSet: TDataSet; const aIgnoredFields: TMVCIgnoredList = [];
      const aNameCase: TMVCNameCase = ncAsIs): IRESTResponse;
    function DataSetUpdate(const aResource: string; aDataSet: TDataSet; const aIgnoredFields: TMVCIgnoredList = [];
      const aNameCase: TMVCNameCase = ncAsIs): IRESTResponse;
    function DataSetDelete(const aResource: string): IRESTResponse;

    function Serializer: IMVCSerializer;
  end;

  IRESTResponse = interface
    ['{BF611B46-CCD1-47C7-8D8B-82EA0518896B}']

    function Success: Boolean;
    function StatusCode: Integer;
    function StatusText: string;
    function ErrorMessage: string;
    function Headers: TStrings;
    function HeaderByName(const aName: string): string;
    function Server: string;
    function FullRequestURI: string;
    function ContentType: string;
    function ContentEncoding: string;
    function ContentLength: Integer;
    function Content: string;
    function RawBytes: TBytes;
    procedure SaveContentToStream(aStream: TStream);
    procedure SaveContentToFile(const aFileName: string);
  end;

implementation

end.
