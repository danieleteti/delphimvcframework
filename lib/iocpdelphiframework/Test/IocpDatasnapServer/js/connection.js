//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

var AdminInst = null;
var connectionInfo;

function setConnection(host, port, urlPath)
{
  connectionInfo = {"host":host,"port":port,"authentication":null,"pathPrefix":urlPath};
}

function setCredentials(user, password)
{
   if (AdminInst != null)
     return true; // already logged in
   connectionInfo.authentication = convertStringToBase64(user + ":" + password);
   var testCreds = new DSAdmin(connectionInfo).GetPlatformName();

   if ( testCreds != null && testCreds.result != null )
   {
     AdminInst = new ServerFunctionExecutor("DSAdmin", connectionInfo);
     return true;
   }
   else
   {
     return false;
   }
}
