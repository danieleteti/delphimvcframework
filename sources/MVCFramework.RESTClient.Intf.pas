unit MVCFramework.RESTClient.Intf;

interface

uses
  System.Classes,
  System.SysUtils;

type
  IRESTClient = interface
    ['{592BC90F-B825-4B3B-84A7-6CA3927BAD69}']

    function BaseURL: string; overload;
    function BaseURL(const ABaseURL: string): IRESTClient; overload;
    function ReadTimeout: Integer; overload;
    function ReadTimeout(const AReadTimeout: Integer): IRESTClient; overload;

    function ProxyServer: string; overload;
    function ProxyServer(const AProxyServer: string): IRESTClient; overload;
    function ProxyPort: Integer; overload;
    function ProxyPort(const AProxyPort: Integer): IRESTClient; overload;
    function ProxyUsername: string; overload;
    function ProxyUsername(const AProxyUsername: string): IRESTClient; overload;
    function ProxyPassword: string; overload;
    function ProxyPassword(const AProxyPassword: string): IRESTClient; overload;

    function SetBasicAuthorization(const AUsername, APassword: string): IRESTClient;
    function SetBearerAuthorization(const AToken: string): IRESTClient;

    function AddHeader(const AName, AValue: string): IRESTClient; overload;
    function AddHeader(const AName: string; AValue: Integer): IRESTClient; overload;
    function AddHeader(const AName: string; AValue: Int64): IRESTClient; overload;
    function AddHeader(const AName: string; AValue: TGUID): IRESTClient; overload;
    function AddHeader(const AName: string; AValue: TDateTime): IRESTClient; overload;
    function AddHeader(const AName: string; AValue: TDate): IRESTClient; overload;
    function AddHeader(const AName: string; AValue: TTime): IRESTClient; overload;
    function AddHeader(const AName: string; AValue: Double): IRESTClient; overload;
    function ClearHeaders: IRESTClient;

    function AddPathParam(const AName, AValue: string): IRESTClient; overload;
    function AddPathParam(const AName: string; AValue: Integer): IRESTClient; overload;
    function AddPathParam(const AName: string; AValue: Int64): IRESTClient; overload;
    function AddPathParam(const AName: string; AValue: TGUID): IRESTClient; overload;
    function AddPathParam(const AName: string; AValue: TDateTime): IRESTClient; overload;
    function AddPathParam(const AName: string; AValue: TDate): IRESTClient; overload;
    function AddPathParam(const AName: string; AValue: TTime): IRESTClient; overload;
    function AddPathParam(const AName: string; AValue: Double): IRESTClient; overload;
    function ClearPathParams: IRESTClient;

    function AddQueryStringParam(const AName, AValue: string): IRESTClient; overload;
    function AddQueryStringParam(const AName: string; AValue: Integer): IRESTClient; overload;
    function AddQueryStringParam(const AName: string; AValue: Int64): IRESTClient; overload;
    function AddQueryStringParam(const AName: string; AValue: TGUID): IRESTClient; overload;
    function AddQueryStringParam(const AName: string; AValue: TDateTime): IRESTClient; overload;
    function AddQueryStringParam(const AName: string; AValue: TDate): IRESTClient; overload;
    function AddQueryStringParam(const AName: string; AValue: TTime): IRESTClient; overload;
    function AddQueryStringParam(const AName: string; AValue: Double): IRESTClient; overload;
    function ClearQueryParams: IRESTClient;

    function Accept: string; overload;
    function Accept(const AAccept: string): IRESTClient; overload;
    function AcceptCharset: string; overload;
    function AcceptCharset(const AAcceptCharset: string): IRESTClient; overload;
    function AcceptEncoding: string; overload;
    function AcceptEncoding(const AAcceptEncoding: string): IRESTClient; overload;

  end;

  IRESTResponse = interface
    ['{BF611B46-CCD1-47C7-8D8B-82EA0518896B}']

    function Success: Boolean; overload;
    function Success(const ASuccess: Boolean): IRESTResponse; overload;

    function StatusCode: Integer; overload;
    function StatusCode(const AStatusCode: Integer): IRESTResponse; overload;
    function StatusText: string; overload;
    function StatusText(const AStatusText: string): IRESTResponse; overload;

    function Headers: TStrings; overload;
    function Headers(const AHeaders: TStrings): IRESTResponse; overload;

    function ContentType: string; overload;
    function ContentType(const AContentType: string): IRESTResponse; overload;
    function ContentEncoding: string; overload;
    function ContentEncoding(const AContentEncoding: string): IRESTResponse; overload;
    function ContentLength: Integer; overload;
    function ContentLength(const AContentLength: Integer): IRESTResponse; overload;

    function Content: string; overload;
    function Content(const AContent: string): IRESTResponse; overload;
    function RawBytes: TBytes; overload;
    function RawBytes(const ARawBytes: TBytes): IRESTResponse; overload;

  end;

implementation

end.
