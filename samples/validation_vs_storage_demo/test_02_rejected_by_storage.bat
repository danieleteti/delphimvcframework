@echo off
rem ---------------------------------------------------------------------------
rem Scenario 2: rejected by the STORAGE layer.
rem
rem Body is syntactically valid: name has 2+ chars, email has a well-formed
rem address. Every field-level attribute validator passes, so the boundary
rem green-lights the request and the action runs. Inside the action,
rem APerson.Insert calls TMVCActiveRecord.Validate, which invokes
rem OnStorageValidate. That rule enforces a storage-only business invariant
rem ("@example.com domain only") and raises EMVCStorageValidationException.
rem
rem Still 422, but the distinct classname shows the error came from the
rem storage layer, not the input layer.
rem ---------------------------------------------------------------------------

echo.
echo === Scenario 2: rejected by STORAGE layer (OnStorageValidate domain rule) ===
echo Body: { "name": "Bob", "email": "bob@elsewhere.org" }
echo.

curl -i -X POST http://localhost:8989/people ^
  -H "Content-Type: application/json" ^
  -d "{\"name\":\"Bob\",\"email\":\"bob@elsewhere.org\"}"

echo.
echo.
echo Expected: HTTP/1.1 422 + classname "EMVCStorageValidationException".
