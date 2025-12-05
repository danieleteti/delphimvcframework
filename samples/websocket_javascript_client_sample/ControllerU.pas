unit ControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons, System.Generics.Collections;

type
  [MVCPath('/api')]
  TMyController = class(TMVCController)
  end;

implementation

uses
  System.StrUtils, System.SysUtils, MVCFramework.Logger;


end.
