# Test Rate Limit Middleware Sample
# This script tests the rate limiting functionality by making multiple HTTP requests

Write-Host "======================================" -ForegroundColor Cyan
Write-Host "Rate Limit Middleware Test" -ForegroundColor Cyan
Write-Host "======================================" -ForegroundColor Cyan
Write-Host ""

$baseUrl = "http://localhost:8080"
$endpoint = "$baseUrl/api/hello"
$healthEndpoint = "$baseUrl/health"

# Test 1: Make 15 requests to test rate limiting (limit is 10)
Write-Host "Test 1: Making 15 requests to $endpoint" -ForegroundColor Yellow
Write-Host "Expected: First 10 succeed (200), next 5 get rate limited (429)" -ForegroundColor Yellow
Write-Host ""

$successCount = 0
$rateLimitedCount = 0

for ($i = 1; $i -le 15; $i++) {
    try {
        $response = Invoke-WebRequest -Uri $endpoint -Method GET -UseBasicParsing -ErrorAction SilentlyContinue

        $statusCode = $response.StatusCode
        $rateLimit = $response.Headers['X-RateLimit-Limit']
        $remaining = $response.Headers['X-RateLimit-Remaining']
        $reset = $response.Headers['X-RateLimit-Reset']

        if ($statusCode -eq 200) {
            $successCount++
            Write-Host "Request $i`: SUCCESS (200) - Remaining: $remaining/$rateLimit, Reset: $reset" -ForegroundColor Green
        }
    }
    catch {
        if ($_.Exception.Response.StatusCode -eq 429) {
            $rateLimitedCount++
            $retryAfter = $_.Exception.Response.Headers['Retry-After']
            Write-Host "Request $i`: RATE LIMITED (429) - Retry-After: $retryAfter seconds" -ForegroundColor Red
        }
        else {
            Write-Host "Request $i`: ERROR - $($_.Exception.Message)" -ForegroundColor Magenta
        }
    }

    Start-Sleep -Milliseconds 100
}

Write-Host ""
Write-Host "--------------------------------------" -ForegroundColor Cyan
Write-Host "Results:" -ForegroundColor Cyan
Write-Host "  Successful requests: $successCount (expected: 10)" -ForegroundColor $(if ($successCount -eq 10) { "Green" } else { "Yellow" })
Write-Host "  Rate limited requests: $rateLimitedCount (expected: 5)" -ForegroundColor $(if ($rateLimitedCount -eq 5) { "Green" } else { "Yellow" })
Write-Host "--------------------------------------" -ForegroundColor Cyan
Write-Host ""

# Test 2: Test excluded endpoint (health check)
Write-Host "Test 2: Testing excluded endpoint $healthEndpoint" -ForegroundColor Yellow
Write-Host "Expected: Not rate limited (should always return 200)" -ForegroundColor Yellow
Write-Host ""

$healthSuccessCount = 0

for ($i = 1; $i -le 15; $i++) {
    try {
        $response = Invoke-WebRequest -Uri $healthEndpoint -Method GET -UseBasicParsing
        if ($response.StatusCode -eq 200) {
            $healthSuccessCount++
        }
    }
    catch {
        Write-Host "Health check $i`: ERROR - $($_.Exception.Message)" -ForegroundColor Magenta
    }
    Start-Sleep -Milliseconds 50
}

Write-Host "Health checks: $healthSuccessCount/15 succeeded (expected: 15/15)" -ForegroundColor $(if ($healthSuccessCount -eq 15) { "Green" } else { "Yellow" })
Write-Host ""

# Summary
Write-Host "======================================" -ForegroundColor Cyan
Write-Host "Test Summary" -ForegroundColor Cyan
Write-Host "======================================" -ForegroundColor Cyan

$test1Pass = ($successCount -eq 10 -and $rateLimitedCount -eq 5)
$test2Pass = ($healthSuccessCount -eq 15)

Write-Host "Test 1 (Rate Limiting): $(if ($test1Pass) { 'PASS' } else { 'FAIL' })" -ForegroundColor $(if ($test1Pass) { "Green" } else { "Red" })
Write-Host "Test 2 (Excluded Path): $(if ($test2Pass) { 'PASS' } else { 'FAIL' })" -ForegroundColor $(if ($test2Pass) { "Green" } else { "Red" })
Write-Host ""

if ($test1Pass -and $test2Pass) {
    Write-Host "ALL TESTS PASSED!" -ForegroundColor Green
    Write-Host ""
    Write-Host "Rate limiting middleware is working correctly!" -ForegroundColor Green
    exit 0
}
else {
    Write-Host "SOME TESTS FAILED!" -ForegroundColor Red
    Write-Host ""
    Write-Host "Please check the output above for details." -ForegroundColor Yellow
    exit 1
}
