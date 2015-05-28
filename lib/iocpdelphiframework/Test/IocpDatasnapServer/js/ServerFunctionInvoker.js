//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

var loadingString = "loading...";


/*
 * Updates the UI to show all of the classes available to execute functions for
 * and also shows those functions, giving the user the ability to execute a function.
 */
function createClassTree()
{
  clearText();
  document.getElementById('resultWrapper').style.visibility='visible';

  for (var key in JSProxyClassList)
  {
    if (JSProxyClassList.hasOwnProperty(key))
    {
      createTreeForClass(key, JSProxyClassList[key]);
    }
  }
}

/*
 * Creates the 'tree' of functions/parameters with the class being the 'root node'
 */
function createTreeForClass(className, functionArray)
{
  var expandPart = '<a href="javascript:toggleDIV(\'' + className + '\')" class="simple" title="expand/collapse" alt="expand/collapse"><img id="' + className + 'IMG" src="images/expand.png" /></a>';

  var classText = '<div class="classDiv">';
  classText += '<div class="classHeading">' + expandPart + '&nbsp;' + className + '</div>';
  classText += '<div class"classBody" id="' + className + 'DIV" style="display:none">';

  if (functionArray instanceof Array)
  {
    for (i = 0; i < functionArray.length; i++)
    {
      var methodSig = className + "." + functionArray[i];
      var paramDivId = methodSig + "ParamDiv";
      var methodExpandPart = '<a href="javascript:toggleDIV(\'' + paramDivId + '\')" class="simple" title="expand/collapse" alt="expand/collapse"><img id="' + paramDivId + 'IMG" src="images/expand.png" /></a>';


      classText += ( '<div class="methodHeading">' + methodExpandPart + '&nbsp;' + functionArray[i] + '</div>' );
      classText += ( '<div id="' + paramDivId + 'DIV" class="paramDiv" style="display:none">' + loadingString + '</div>' );
    }
  }
  classText += "</div></div><br />";

  appendText(classText);
}

/*
 * Clears the content div of the page.
 * This will generally only be used to remove the login form before adding the class tree
 */
function clearText()
{
  var contentDiv = document.getElementById('contentdiv');
  contentDiv.innerHTML = "";
}

/*
 * Appends the given text to the end of the innerHTML of the content div
 */
function appendText(textToAppend)
{
  var contentDiv = document.getElementById('contentdiv');
  contentDiv.innerHTML = contentDiv.innerHTML + "\n" + textToAppend;
}

/*
 * Creates the parameter form with the eexecute button for a given function
 */
function createParamForm(divId, className, methodName, paramsObj)
{
  var paramDiv = document.getElementById(divId);
  if (paramDiv != null && paramsObj != null)
  {
    var formId = divId + "Form";
    var formString = '<form id="' + formId + '" onsubmit="executeServerFunction(\'' + formId + '\',\'' + className + '\',\'' + methodName + '\'); return false;">\n';

    formString += '<div class="executeForm"><table>';

    var paramIndex = 0;

    for (var key in paramsObj)
    {
      if (paramsObj.hasOwnProperty(key))
      {
        var paramName = key;
        var paramDirection = paramsObj[key].direction;
        var paramType = paramsObj[key].type;

        if (paramDirection == 'In' || paramDirection == 'InOut')
        {
          if((paramIndex++) == 0)
          {
            formString += '<tr class="paramTableHeader"><td>Parameter</td><td>Value</td></tr>\n';
          }

          var fieldId = divId + "." + paramName;
          formString += '<tr><td class="thin">' + paramName + '&nbsp;(' + paramType + '): </td>';
          formString += '<td><input class="long" type="text" id="' + fieldId + '"></input></td></tr>\n';
        }
      }
    }

    formString += "</table>";

    formString += '<div class="executeButtonDiv"><input id="' + formId + 'Button" type="submit" value="EXECUTE" /></div>';

    formString += "</div></form>";

    paramDiv.innerHTML = formString;
  }
}

/*
 * Executes the given method on the server and sets the server response (the result of the call)
 * into the result frame at the bottom of the page.
 */
function executeServerFunction(formId, className, methodName)
{
  updateResultFrame();
  var formObj = document.getElementById(formId);

  var paramString = "";

  var index = 0;
  for (var i = 0; i < formObj.elements.length; i++)
  {
    if (formObj.elements[i].type == 'text')
    {
      if (index++ > 0)
      {
        paramString += ",";
      }
      var thisVal = formObj.elements[i].value;
      if (thisVal.length == 0 ||
           (thisVal.charAt(0) != "{" && thisVal.charAt(0) != "[" && thisVal.charAt(0) != "\"")) {
        thisVal = "\"" + thisVal + "\"";
      }
      paramString += thisVal;
    }
  }

  var authString = null;
  if (AdminInst != null)
  {
    if (AdminInst.authentication != null)
	{
      authString = AdminInst.authentication;
	}
	var connectionInfo = JSON.stringify(AdminInst.connectionInfo);
    var evalString = "( new " + className + "(" + connectionInfo + "))." + methodName + "(" + paramString + ")";
    updateResultFrame(className, methodName, eval(evalString));
  }
}

/*
 * Updates the result frame at the bottom of the page with the given information.
 * If called with no arguments the result frame is cleared.
 */
function updateResultFrame(className, methodName, resultObject)
{
  var callCell = document.getElementById('resultMethodSignature');
  var resultCell = document.getElementById('resultMethodResult');

  if (callCell != null && resultCell != null)
  {
    if ( className == null || methodName == null )
    {
      var methodNameString = "";
      var resultString = "";
    }
    else
    {
      var methodNameString = className + "." + methodName;
      var resultString = resultObject == null ? "[no result]" : JSON.stringify(resultObject);
    }

    callCell.innerHTML = methodNameString;
    resultCell.innerHTML = resultString;
  }
}

/*
 * Expands or collapses the div with the given ID
 */
function toggleDIV(toggleId)
{
  var divID = toggleId + "DIV";
  var imgID = toggleId + "IMG";

  var divObj = document.getElementById(divID);
  var imgObj = document.getElementById(imgID);

  if (divObj != null && imgObj != null)
  {
    if (divObj.style.display != 'none')
    {
      divObj.style.display = 'none';
      imgObj.src="images/expand.png";
    }
    else
    {
      divObj.style.display = '';
      imgObj.src="images/collapse.png";

      //notify that the div has been expanded
      notifyExpand(toggleId);
    }
  }
}

/*
 * Helper function for seeing if a string ends with another string.
 */
function endsWith(toSearchIn, toSearchFor)
{
   var index = toSearchIn.lastIndexOf(toSearchFor);
   return (index >= 0 && ( index + toSearchFor.length == toSearchIn.length) );
}

/*
 * Notified for each toggle with the ID of the div that is being expanded (minus the suffix of "DIV")
 */
function notifyExpand(toggleId)
{
  var paramsDivsuffix = "ParamDiv";

  if (toggleId != null && endsWith(toggleId, paramsDivsuffix))
  {
    var divID = toggleId + "DIV";
    var divObj = document.getElementById(divID);

    if (divObj.innerHTML == loadingString)
    {
      var suffixPos = toggleId.lastIndexOf(paramsDivsuffix);
      var dotPos = toggleId.indexOf('.');

      if (suffixPos > 0 && dotPos > 0 && dotPos < suffixPos )
      {
        var methodSig = toggleId.substr(0, suffixPos);
        var className = methodSig.substr(0, dotPos);
        var methodName = methodSig.substr(dotPos + 1, methodSig.length);

        //calls the server method for describing the given function (getting its parameter information)
        //The result of this call is passed to the callback, which then builds the parameter form for the function
        var thisCallbackWrapper = new ParamCallbackWrapper(divID, className, methodName );
        AdminInst.executeMethod("DescribeMethod", "GET", [methodSig], thisCallbackWrapper.callback );
      }
    }
  }
}

/*
 * Wrapper class around a callback for populating function information,
 * used when building the execute form for a specific function/procedure
 */
function ParamCallbackWrapper(divId, className, methodName, status)
{
  this.divId = divId;
  this.className = className;
  this.methodName = methodName;
  this.callback = function(JSONResult, status) {
    if (JSONResult != null)
    {
      //EXAMPLE: {"result":[{"className.MethodName":{"paramname":{"direction":"in", "type":"TJSONObject"}}}]}
      var resultObj = JSONResult.result[0];
      for (var key in resultObj)
      {
        if (resultObj.hasOwnProperty(key))
        {
          createParamForm(divId, className, methodName, resultObj[key]);

          //should only ever be one method pair returned, where the key is the method name
          break;
        }
      }
    }
  };
}
