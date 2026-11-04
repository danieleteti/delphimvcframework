"""Simulates the attacks APIGuard detects against the running APIGuardSample.

Each attack comes from its own loopback address (127.0.0.x reaches the same
server), so one block does not stop the next scenario; the status endpoint is
read from 127.0.0.1, the only monitor IP by default. Standard library only.

    python test_apiguard.py all
    python test_apiguard.py bruteforce --port 8080

Contributor: Giordano Giaccaglia - KD Software S.r.l.s. - https://www.kdsoftware.it - info@kdsoftware.it
"""

import argparse
import http.client
import json
import os
from collections import Counter

os.system("")  # turns on ANSI colours in the classic Windows console
PORT = 8080
RED, GREEN, YELLOW, RESET = "\033[31m", "\033[32m", "\033[33m", "\033[0m"


def send(ip, path, method="GET", body=None, user_agent="test_apiguard"):
    # Windows connects to 127.0.0.x from 127.0.0.1 unless the source is bound
    conn = http.client.HTTPConnection(ip, PORT, timeout=10, source_address=(ip, 0))
    try:
        headers = {"User-Agent": user_agent}
        if body is not None:
            headers["Content-Type"] = "application/json"
            body = json.dumps(body)
        conn.request(method, path, body=body, headers=headers)
        resp = conn.getresponse()
        return resp.status, resp.read()
    finally:
        conn.close()


def title(text):
    print(f"{YELLOW}{text}{RESET}")


def show(label, ip):
    code, _ = send(ip, "/api/hello")
    color = RED if code == 403 else GREEN
    print(f"  {color}{label}: GET /api/hello from {ip} -> {code}{RESET}")


def scan():
    title("scan: 12 requests to paths that do not exist (rule: 10 x 404 in 60s)")
    for path in ["/admin", "/backup", "/old", "/test", "/db", "/api/v0", "/api/users.bak",
                 "/private", "/tmp", "/log", "/x", "/y"]:
        send("127.0.0.2", path)
    show("after the scan", "127.0.0.2")


def bruteforce():
    title("bruteforce: 5 wrong passwords on /api/login (rule: 5 x 401/403 in 300s)")
    for i in range(1, 6):
        code, _ = send("127.0.0.3", "/api/login", "POST", {"username": "admin", "password": f"guess{i}"})
        print(f"  attempt {i} -> {code}")
    show("after the brute force", "127.0.0.3")


def flood():
    title("flood: 110 requests in a burst (rule: 100 requests in 10s)")
    codes = Counter(send("127.0.0.4", "/api/hello")[0] for _ in range(110))
    for code, count in sorted(codes.items()):
        print(f"  {count} x {code}")


def injection():
    title("injection: one path traversal in the query string (rule: 1 signature hit)")
    code, _ = send("127.0.0.5", "/api/hello?file=..%2F..%2F..%2Fetc%2Fpasswd")
    print(f"  traversal -> {code}")
    show("after the probe", "127.0.0.5")


def agent():
    title("agent: one request with an attack tool User-Agent (rule: 1 hit)")
    code, _ = send("127.0.0.6", "/api/hello", user_agent="sqlmap/1.8#stable (https://sqlmap.org)")
    print(f"  sqlmap -> {code}")
    show("after the probe", "127.0.0.6")


def honeypot():
    title("honeypot (custom rule): one request for /backup.zip")
    code, _ = send("127.0.0.7", "/backup.zip")
    print(f"  /backup.zip -> {code}")
    show("after the probe", "127.0.0.7")


def table(rows, columns):
    if not rows:
        print("    (none)")
        return
    widths = [max(len(c), *(len(str(r.get(c))) for r in rows)) for c in columns]
    print("    " + "  ".join(c.ljust(w) for c, w in zip(columns, widths)))
    for r in rows:
        print("    " + "  ".join(str(r.get(c)).ljust(w) for c, w in zip(columns, widths)))


def status():
    title("status: GET /api-guard/status from 127.0.0.1")
    code, body = send("127.0.0.1", "/api-guard/status")
    if code != 200:
        print(f"  {RED}status -> {code}{RESET}")
        return
    s = json.loads(body)
    print(f"  requests last minute: {s['requests_last_minute']}, total: {s['requests_total']}, "
          f"blocked requests: {s['blocked_requests_total']}")
    print("  blocked IPs:")
    table(s["blocked_ips"], ["ip", "rule", "reason", "expires_at"])
    print("  rules:")
    table(s["rules"], ["name", "enabled", "threshold", "window_seconds", "block_seconds", "detections", "blocks"])
    print("  latest alerts:")
    table(s["alerts"][:10], ["timestamp", "ip", "rule", "path"])
    code, _ = send("127.0.0.2", "/api-guard/status")
    print(f"  the same endpoint from 127.0.0.2 (not a monitor IP) -> {code}")


ATTACKS = {"scan": scan, "bruteforce": bruteforce, "flood": flood, "injection": injection,
           "agent": agent, "honeypot": honeypot, "status": status}


def main():
    global PORT
    parser = argparse.ArgumentParser(description="Simulated attacks against APIGuardSample")
    parser.add_argument("attack", nargs="?", default="all", choices=["all", *ATTACKS])
    parser.add_argument("--port", type=int, default=PORT)
    args = parser.parse_args()
    PORT = args.port
    if args.attack == "all":
        show("normal traffic", "127.0.0.1")
        for run in ATTACKS.values():
            run()
    else:
        ATTACKS[args.attack]()


if __name__ == "__main__":
    main()
