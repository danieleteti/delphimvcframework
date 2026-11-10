"""The same server as test_exewatch_capture.py, against the real ExeWatch: sends an exception of an action and a plain
error, then reads them back with the exewatch CLI and checks the stack that arrived.

Needs: Delphi, the ExeWatch SDK (EXEWATCH_SDK), the exewatch CLI with EXEWATCH_API_KEY (read), and the key of the
application that receives the events in EXEWATCH_TEST_INGEST_KEY (ew_win_...). The events count on its quota.

    python test_exewatch_live.py --app EWSample --env TEST
"""

import argparse
import http.client
import json
import os
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import time

from test_exewatch_capture import build, check, free_port

MAPCLI = os.path.join(os.path.dirname(os.environ.get("EXEWATCH_SDK", "")), "bin", "ExeWatchMapCLI.exe")


def run_server(outdir, key, customer, warmup):
    port = free_port()
    with open(os.path.join(outdir, ".env"), "w") as f:
        f.write(f"exewatch.apikey={key}\nexewatch.customerid={customer}\nserver.port={port}\n")
    p = subprocess.Popen([os.path.join(outdir, "ExeWatchCaptureTest.exe")], cwd=outdir, stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT, creationflags=subprocess.CREATE_NEW_PROCESS_GROUP)
    codes = []
    try:
        for _ in range(75):
            try:
                socket.create_connection(("127.0.0.1", port), timeout=0.4).close()
                break
            except OSError:
                time.sleep(0.2)
        time.sleep(warmup)  # the SDK reads the map in the first moments of the process: see --warmup
        for path in ["/api/boom", "/api/plain", "/api/info"]:
            c = http.client.HTTPConnection("127.0.0.1", port, timeout=10)
            c.request("GET", path)
            codes.append(c.getresponse().status)
            c.close()
        time.sleep(1)
        p.send_signal(signal.CTRL_BREAK_EVENT)  # the server waits for the SDK to send before it ends
        try:
            p.wait(timeout=30)
        except subprocess.TimeoutExpired:
            p.kill()
    finally:
        if p.poll() is None:
            p.kill()
    return codes


def read_errors(app, env, customer):
    cmd = ["exewatch", "errors", "--app", app, "--customer", customer, "--since", "30m", "--full"]
    if env:
        cmd += ["--env", env]
    r = subprocess.run(cmd, capture_output=True, text=True, encoding="utf-8", shell=True)
    try:
        return json.loads(r.stdout).get("items", [])
    except ValueError:
        print(r.stdout[-1500:], r.stderr[-500:])
        return []


def stack(event):
    return json.dumps(event)  # wherever the server puts it: the frames are searched as text


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--app", required=True)
    ap.add_argument("--env", default="")
    ap.add_argument("--warmup", type=float, default=4, help="seconds between the start of the server and the requests")
    args = ap.parse_args()
    key = os.environ.get("EXEWATCH_TEST_INGEST_KEY", "")
    if not key.startswith("ew_"):
        sys.exit("EXEWATCH_TEST_INGEST_KEY is not set")
    customer = f"capture-live-{int(time.time())}"
    outdir = tempfile.mkdtemp(prefix="ew_live_")
    results = []
    try:
        build(outdir, "-GD")  # the map file, linked into the exe: frames with names
        r = subprocess.run([MAPCLI, os.path.join(outdir, "ExeWatchCaptureTest.exe")], capture_output=True, text=True)
        results.append(check("the map file is linked into the exe", "RESULT=INJECTED" in r.stdout, r.stdout[:300]))
        codes = run_server(outdir, key, customer, args.warmup)
        results.append(check("answers are 500, 200, 200", codes == [500, 200, 200], codes))
        errors = []
        for _ in range(12):  # ingestion is not immediate
            errors = read_errors(args.app, args.env, customer)
            if len(errors) >= 2:
                break
            time.sleep(5)
        print(f"customer {customer}: {len(errors)} error events read back")
        boom = [e for e in errors if "/api/boom" in e.get("message", "")]
        plain = [e for e in errors if "a plain error from the action" in e.get("message", "")]
        results.append(check("one event for the failing request, one for the plain error, no more",
                             len(boom) == 1 and len(plain) == 1 and len(errors) == 2,
                             [e.get("message", "")[:60] for e in errors]))
        results.append(check("the exception: the stack names the action that raised",
                             bool(boom) and "TTestController.Boom" in stack(boom[0])))
        results.append(check("the plain error: the stack names the action that logged",
                             bool(plain) and "TTestController.Plain" in stack(plain[0])))
        results.append(check("the plain error: no frame of LoggerPro",
                             bool(plain) and "LoggerPro." not in stack(plain[0])))
        for e in boom + plain:
            print(json.dumps(e, indent=1)[:2500])
    finally:
        shutil.rmtree(outdir, ignore_errors=True)
    sys.exit(0 if all(results) else 1)


if __name__ == "__main__":
    main()
