unit Iocp.FileConst;

interface

uses
  Windows, Messages, SysUtils, Classes;

const
  VFE_UNKNOWN = -1;
  VFE_ABORT = -2;
  VFE_NOT_EXISTS = -3;
  VFE_DISCONNECT = -4;
  VFE_SAME = -5;
  VFE_CHECK_ERROR =  -6;
  VFE_INCOMPLETE_FILE = -7;
  VFE_SYSTEM_ERROR = -127;

implementation

end.
