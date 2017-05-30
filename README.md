spread_process_launcher
=====

Use Spread to launch native processes and orchestrate them.

Usage
=====
Launch `curl http://www.google.com` in `nonode@nohost` (we removed auth params for the sake of readibility)
```
curl http://localhost:8080/raw/Process%20launcher/nonode@nohost/curl_google/command --data "curl http://www.google.com 2>&1"
```
Subscribe to what happens about this command
```
curl http://localhost:8080/sse/Process%20launcher/nonode@nohost/curl_google
```
Sample output:
```
id: 1496155526493381
data: Process launcher/nonode%40nohost/curl_google/command
data: nonode@nohost
data: 1496155680561870
data: curl http://www.google.com 2>&1

id: 1496155526493383
data: Process launcher/nonode%40nohost/curl_google/latest_logs
data: nonode@nohost
data: 1496155680647127
data: 1496155680629   % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
1496155680630                                  Dload  Upload   Total   Spent    Left  Speed
100   256  100   256    0     0   5032      0 --:--:-- --:--:-- --:--:--  5120
1496155680646 <HTML><HEAD><meta http-equiv="content-type" content="text/html;charset=utf-8">
1496155680646 <TITLE>302 Moved</TITLE></HEAD><BODY>
1496155680646 <H1>302 Moved</H1>
1496155680646 The document has moved
1496155680646 <A HREF="http://www.google.fr/?gfe_rd=cr&amp;ei=IIYtWYqfJ4bBaNjEhMgN">here</A>.
1496155680646 </BODY></HTML>

id: 1496155526493384
data: Process launcher/nonode%40nohost/curl_google/status
data: nonode@nohost
data: 1496155680653314
data: {"ref":1496155680561870,"status":"done","time":1496155680647,"exit_status":0}

```