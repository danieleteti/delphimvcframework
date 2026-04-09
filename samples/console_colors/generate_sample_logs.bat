@echo off
echo ============================================
echo  DMVCFramework Gin-Style Log Screenshot Tool
echo ============================================
echo.
echo Make sure your DMVC server is running on localhost:8080
echo.
pause

REM --- 200 OK responses ---
curl -s -o nul http://localhost:8080/api/people
curl -s -o nul http://localhost:8080/api/people
curl -s -o nul http://localhost:8080/api/people/123

REM --- 404 Not Found ---
curl -s -o nul http://localhost:8080/api/unknown
curl -s -o nul http://localhost:8080/api/people/999/orders
curl -s -o nul -X POST http://localhost:8080/api/people/123

REM --- 400 Bad Request (invalid ID) ---
curl -s -o nul http://localhost:8080/api/people/abc
curl -s -o nul http://localhost:8080/api/people/123xyz

REM --- PUT ---
curl -s -o nul -X PUT -H "Content-Type: application/json" -d "{\"FirstName\":\"John\",\"LastName\":\"Doe\"}" http://localhost:8080/api/people/abc

REM --- DELETE ---
curl -s -o nul -X DELETE http://localhost:8080/api/people/abc

REM --- HEAD ---
curl -s -o nul -I http://localhost:8080/api/people

REM --- More GETs for a nice log ---
curl -s -o nul http://localhost:8080/api/people
curl -s -o nul http://localhost:8080/api/people/123

echo.
echo Done! Check the server console for the colored log output.
