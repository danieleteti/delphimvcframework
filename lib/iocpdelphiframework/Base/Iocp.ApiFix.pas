unit Iocp.ApiFix;

interface

uses
  Windows;

(*

Delphi自带的IOCP相关的几个函数定义是错的！
在32位程序下不会出问题，但是64位程序里就错了

Delphi XE2支持64位编译，但是XE2直到Update4的定义都是错的，希望以后官方会修正

// 这是Delphi的错误定义，
// CompletionKey: DWORD
// DWORD不管在32位还是64位程序中都是4字节，MSDN中正确的定义是ULONG_PTR
// ULONG_PTR在32位程序中是4字节，在64位程序中是8字节
function CreateIoCompletionPort(FileHandle, ExistingCompletionPort: THandle;
  CompletionKey, NumberOfConcurrentThreads: DWORD): THandle; stdcall;
{$EXTERNALSYM CreateIoCompletionPort}
function GetQueuedCompletionStatus(CompletionPort: THandle;
  var lpNumberOfBytesTransferred, lpCompletionKey: DWORD;
  var lpOverlapped: POverlapped; dwMilliseconds: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetQueuedCompletionStatus}
function PostQueuedCompletionStatus(CompletionPort: THandle; dwNumberOfBytesTransferred: DWORD;
  dwCompletionKey: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;
{$EXTERNALSYM PostQueuedCompletionStatus}
*)

// 后面是我自己根据MSDN相关文档修正后的定义

function CreateIoCompletionPort(FileHandle, ExistingCompletionPort: THandle;
  CompletionKey: ULONG_PTR; NumberOfConcurrentThreads: DWORD): THandle; stdcall;

function GetQueuedCompletionStatus(CompletionPort: THandle;
  var lpNumberOfBytesTransferred: DWORD; var lpCompletionKey: ULONG_PTR;
  var lpOverlapped: POverlapped; dwMilliseconds: DWORD): BOOL; stdcall;

function PostQueuedCompletionStatus(CompletionPort: THandle; dwNumberOfBytesTransferred: DWORD;
  dwCompletionKey: ULONG_PTR; lpOverlapped: POverlapped): BOOL; stdcall;

implementation

function CreateIoCompletionPort; external kernel32 name 'CreateIoCompletionPort';
function GetQueuedCompletionStatus; external kernel32 name 'GetQueuedCompletionStatus';
function PostQueuedCompletionStatus; external kernel32 name 'PostQueuedCompletionStatus';

end.
