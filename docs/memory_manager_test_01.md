# FIRST TEST USING SEAMM MEMORY MANAGER

**Project under test**: `samples\renders\renders.dproj`

**Platform**: Win64

**OS**: Windows 10 Pro

**RAM**: 16GiB

**Machine Info**:

```
CPU
	Intel(R) Core(TM) i7-8650U CPU @ 1.90GHz

	Base Speed:	2,11 GHz
	Physical processors:	1
	Cores:	4
	Logical Processors:	8
	Cache L1:	256 KB
	Cache L2:	1,0 MB
	Cache L3:	8,0 MB
```



**Action under test**: `/lotofobjects` (produces 3000 json object)

**Kind of test**: 10 concurrent threads issuing a total of 1000 requests.

**Test Phase**: Preliminar. More tests are needed also using distributed machines.

## USING DEFAULT DELPHI 10.3.1 RIO MEMORY MANAGER

```
C:\DEV\dmvcframework\samples\apachemodule\Apache24\bin\ab.exe -c 10 -n 1000 http://127.0.0.1:8080/lotofobjects
This is ApacheBench, Version 2.3 <$Revision: 1748469 $>
Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
Licensed to The Apache Software Foundation, http://www.apache.org/

Benchmarking 127.0.0.1 (be patient)
Completed 100 requests
Completed 200 requests
Completed 300 requests
Completed 400 requests
Completed 500 requests
Completed 600 requests
Completed 700 requests
Completed 800 requests
Completed 900 requests
Completed 1000 requests
Finished 1000 requests


Server Software:        DelphiMVCFramework
Server Hostname:        127.0.0.1
Server Port:            8080

Document Path:          /lotofobjects
Document Length:        251001 bytes

Concurrency Level:      10
Time taken for tests:   33.767 seconds
Complete requests:      1000
Failed requests:        0
Total transferred:      251197000 bytes
HTML transferred:       251001000 bytes
Requests per second:    29.61 [#/sec] (mean)
Time per request:       337.674 [ms] (mean)
Time per request:       33.767 [ms] (mean, across all concurrent requests)
Transfer rate:          7264.69 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    0   0.3      0       1
Processing:    47  337 112.5    320    1183
Waiting:       46  336 112.4    320    1183
Total:         47  337 112.5    321    1183

Percentage of the requests served within a certain time (ms)
  50%    321
  66%    361
  75%    394
  80%    411
  90%    477
  95%    540
  98%    627
  99%    690
 100%   1183 (longest request)
```

## USING SeaMM.dll and SeaRTL.dll (with RDPMM64.pas from R. Della Pasqua)

```
C:\DEV\dmvcframework\samples\apachemodule\Apache24\bin\ab.exe -c 10 -n 1000 http://127.0.0.1:8080/lotofobjects
This is ApacheBench, Version 2.3 <$Revision: 1748469 $>
Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
Licensed to The Apache Software Foundation, http://www.apache.org/

Benchmarking 127.0.0.1 (be patient)
Completed 100 requests
Completed 200 requests
Completed 300 requests
Completed 400 requests
Completed 500 requests
Completed 600 requests
Completed 700 requests
Completed 800 requests
Completed 900 requests
Completed 1000 requests
Finished 1000 requests


Server Software:        DelphiMVCFramework
Server Hostname:        127.0.0.1
Server Port:            8080

Document Path:          /lotofobjects
Document Length:        251001 bytes

Concurrency Level:      10
Time taken for tests:   14.973 seconds
Complete requests:      1000
Failed requests:        0
Total transferred:      251197000 bytes
HTML transferred:       251001000 bytes
Requests per second:    66.79 [#/sec] (mean)
Time per request:       149.730 [ms] (mean)
Time per request:       14.973 [ms] (mean, across all concurrent requests)
Transfer rate:          16383.48 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    0   0.4      0       1
Processing:    84  149  36.9    143     294
Waiting:       84  148  36.8    142     293
Total:         84  149  36.9    143     294

Percentage of the requests served within a certain time (ms)
  50%    143
  66%    163
  75%    174
  80%    181
  90%    201
  95%    219
  98%    235
  99%    249
 100%    294 (longest request)
```


### Summary

The first test is quite impressive. Using the SEAMM we go from 29.61 to 66.79 requests/second. More tests are needed, however this one is a step in a good direction.
**No unit tests have been run, so do not use this setup in production if not for tests purpose.**
