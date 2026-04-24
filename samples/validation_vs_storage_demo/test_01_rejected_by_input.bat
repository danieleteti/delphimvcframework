@echo off
rem ---------------------------------------------------------------------------
rem Scenario 1: rejected by the INPUT / boundary layer.
rem
rem Body is empty. The field-level MVCRequired validators on fName / fEmail
rem fire at the HTTP boundary because TMVCActiveRecord inherits TMVCValidatable
rem and its OnValidate override runs the same validator scan used by Insert.
rem The server answers 422 with classname "EMVCValidationException". The
rem action is NEVER called, nothing hits the database.
rem ---------------------------------------------------------------------------

echo.
echo === Scenario 1: rejected by INPUT layer (MVCRequired on fields) ===
echo Body: {}
echo.

curl -i -X POST http://localhost:8989/people ^
  -H "Content-Type: application/json" ^
  -d "{}"

echo.
echo.
echo Expected: HTTP/1.1 422 + classname "EMVCValidationException" in the body.
