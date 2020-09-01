object wmSOAPREST: TwmSOAPREST
  OldCreateOrder = False
  OnCreate = WebModuleCreate
  OnDestroy = WebModuleDestroy
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
    end
    item
      Name = 'SoapAction'
      PathInfo = '/soap'
      OnAction = wmSOAPRESTSoapActionAction
    end>
  Height = 230
  Width = 415
  object HTTPSoapDispatcher: THTTPSoapDispatcher
    Dispatcher = HTTPSoapPascalInvoker
    WebDispatch.PathInfo = 'soap*'
    Left = 60
    Top = 11
  end
  object HTTPSoapPascalInvoker: THTTPSoapPascalInvoker
    Converter.Options = [soSendMultiRefObj, soTryAllSchema, soUTF8InHeader]
    Left = 60
    Top = 67
  end
  object WSDLHTMLPublish: TWSDLHTMLPublish
    WebDispatch.MethodType = mtAny
    WebDispatch.PathInfo = 'wsdl*'
    TargetNamespace = 'http://tempuri.org/'
    PublishOptions = [poUTF8ContentType]
    Left = 60
    Top = 123
  end
end
