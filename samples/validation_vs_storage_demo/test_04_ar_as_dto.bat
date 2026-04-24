@echo off
rem ---------------------------------------------------------------------------
rem Scenario 4: "AR as DTO" pattern - POST /people/ar.
rem
rem The action signature is [MVCFromBody] APerson: TPerson, so the framework
rem deserializes directly into the AR entity.
rem
rem Clean rule the framework follows:
rem   * HTTP boundary (post-deserialization): only PROPERTY validators and
rem     the OnValidate hook run. TPerson has NO property validators, so the
rem     boundary lets every payload through.
rem   * Storage (Insert / Update): field validators + OnStorageValidate run.
rem
rem Net effect: in AR-as-DTO with no property validators, EVERY failure is
rem reported as EMVCStorageValidationException. If you want the boundary to
rem filter a shape check (e.g. MVCRequired), declare the validator on the
rem PUBLIC PROPERTY of the AR, just like you would on a DTO.
rem ---------------------------------------------------------------------------

echo.
echo === 4a: short name + bad email - caught at save ===
curl -s -i -X POST http://localhost:8989/people/ar ^
  -H "Content-Type: application/json" ^
  -d "{\"name\":\"A\",\"email\":\"not-an-email\"}" | findstr /C:"HTTP/" /C:"classname"

echo.
echo === 4b: valid fields, wrong domain - OnStorageValidate at save ===
curl -s -i -X POST http://localhost:8989/people/ar ^
  -H "Content-Type: application/json" ^
  -d "{\"name\":\"Bob\",\"email\":\"bob@elsewhere.org\"}" | findstr /C:"HTTP/" /C:"classname"

echo.
echo === 4c: valid fields, correct domain - persisted ===
curl -s -i -X POST http://localhost:8989/people/ar ^
  -H "Content-Type: application/json" ^
  -d "{\"name\":\"Debora\",\"email\":\"debora@example.com\"}" | findstr /C:"HTTP/" /C:"id"

echo.
echo Expected:
echo   4a: 422 + EMVCStorageValidationException  (MinLength + Email + domain)
echo   4b: 422 + EMVCStorageValidationException  (domain only)
echo   4c: 200 + {"id":...}                      (inserted)
