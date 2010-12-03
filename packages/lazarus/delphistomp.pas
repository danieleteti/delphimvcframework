{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit delphistomp; 

interface

uses
  StompClient, StompTypes, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('delphistomp', @Register); 
end.
