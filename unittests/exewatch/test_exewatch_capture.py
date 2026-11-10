"""The ExeWatch appender with the capture hook: what a failing request and a plain error deliver to ExeWatch.

Builds ExeWatchCaptureTest.dpr (needs Delphi and the ExeWatch SDK: EXEWATCH_SDK), runs it against a fake ExeWatch server of
this script and checks the events that arrive. Standard library only.

    python test_exewatch_capture.py
"""

import http.client
import http.server
import json
import os
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import threading
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))
RSVARS = r"C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
received = []
lock = threading.Lock()


class FakeExeWatch(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        raw = self.rfile.read(int(self.headers.get("Content-Length", 0)))
        try:
            body = json.loads(raw.decode("utf-8-sig"))
        except ValueError:
            body = {}
        with lock:
            received.append(body)
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(b"{}")

    do_PUT = do_POST

    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(b"{}")

    def log_message(self, *args):
        pass


def free_port():
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def build(outdir, extra="", compiler=None):
    compiler = compiler or os.environ.get("EW_TEST_COMPILER", "dcc32")  # dcc64 for Win64
    sdk = os.environ.get("EXEWATCH_SDK", "")
    paths = ";".join([os.path.join(ROOT, "sources"), os.path.join(ROOT, "lib", "loggerpro"),
                      os.path.join(ROOT, "lib", "dmustache"), os.path.join(ROOT, "lib", "swagdoc", "Source"), sdk])
    bat = os.path.join(outdir, "build.bat")
    with open(bat, "w") as f:
        f.write(f'@echo off\ncall "{RSVARS}"\ncd /d "{HERE}"\n'
                f'{compiler} -B -Q -E"{outdir}" -N"{outdir}\\dcu" -U"{paths}" '
                f'-NSSystem;Winapi;System.Win;Vcl;Data;Data.Win;Web;Soap;Xml {extra} ExeWatchCaptureTest.dpr\n')
    os.makedirs(os.path.join(outdir, "dcu"), exist_ok=True)
    r = subprocess.run(["cmd", "/c", bat], capture_output=True, text=True)
    if not os.path.exists(os.path.join(outdir, "ExeWatchCaptureTest.exe")):
        print(r.stdout[-2000:])
        sys.exit("build failed")


def run_exe(outdir, with_key, requests):
    work = tempfile.mkdtemp(prefix="ew_capture_")
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), FakeExeWatch)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    port = free_port()
    shutil.copy(os.path.join(outdir, "ExeWatchCaptureTest.exe"), work)
    key = "ew_win_TESTTESTTESTTESTTESTTESTTESTTEST" if with_key else ""
    with open(os.path.join(work, ".env"), "w") as f:
        f.write(f"exewatch.apikey={key}\nexewatch.endpoint=http://127.0.0.1:{server.server_address[1]}\nserver.port={port}\n")
    with lock:
        received.clear()
    p = subprocess.Popen([os.path.join(work, "ExeWatchCaptureTest.exe")], cwd=work, stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT, creationflags=subprocess.CREATE_NEW_PROCESS_GROUP)
    codes = {}
    try:
        for _ in range(75):
            try:
                socket.create_connection(("127.0.0.1", port), timeout=0.4).close()
                break
            except OSError:
                time.sleep(0.2)
        for path in requests:
            c = http.client.HTTPConnection("127.0.0.1", port, timeout=10)
            c.request("GET", path)
            codes[path] = c.getresponse().status
            c.close()
        time.sleep(1)
        p.send_signal(signal.CTRL_BREAK_EVENT)
        try:
            p.wait(timeout=20)
        except subprocess.TimeoutExpired:
            p.kill()
    finally:
        if p.poll() is None:
            p.kill()
        server.shutdown()
        shutil.rmtree(work, ignore_errors=True)
    with lock:
        events = [e for b in received if isinstance(b, dict) for e in b.get("events", [])]
    return codes, events


def check(label, ok, detail=""):
    print(f"  {'ok  ' if ok else 'FAIL'} {label}{(' - ' + str(detail)) if detail and not ok else ''}")
    return ok


def frames(event):
    return [l for l in (event.get("extra_data", {}).get("stack_trace") or "").splitlines() if l.strip()]


def main():
    outdir = tempfile.mkdtemp(prefix="ew_capture_build_")
    results = []
    try:
        build(outdir)
        print("with a key")
        codes, events = run_exe(outdir, True, ["/api/boom", "/api/plain", "/api/info"])
        results.append(check("answers are 500, 200, 200", list(codes.values()) == [500, 200, 200], codes))
        errors = [e for e in events if e.get("level") == "error"]
        boom = [e for e in errors if "[PathInfo \"GET /api/boom\"]" in e.get("message", "")]
        plain = [e for e in errors if "a plain error from the action" in e.get("message", "")]
        results.append(check("a failing request: one error event, with the text of the engine unchanged",
                             len(boom) == 1 and boom[0]["message"].startswith("[Exception] Something went wrong"),
                             [e.get("message", "")[:60] for e in errors]))
        results.append(check("no second event for the same failure", len(errors) == 2,
                             [e.get("message", "")[:50] for e in errors]))
        results.append(check("its structured stack_trace is the stack of the raise (many frames, not the logger thread's)",
                             bool(boom) and len(frames(boom[0])) >= 12, len(frames(boom[0])) if boom else "no event"))
        results.append(check("a plain error of the action: one event, structured stack_trace of the call site (many frames)",
                             len(plain) == 1 and len(frames(plain[0])) >= 12, len(frames(plain[0])) if plain else 'no event'))
        infos = [e for e in events if e.get("level") == "info" and "an info from the action" in e.get("message", "")]
        results.append(check("an info: one event and no stack_trace",
                             len(infos) == 1 and not infos[0].get("extra_data", {}).get("stack_trace")))
        print("without a key")
        codes, events = run_exe(outdir, False, ["/api/boom", "/api/info"])
        results.append(check("ExeWatch off: the server answers and nothing is sent", list(codes.values()) == [500, 200]
                             and not events, codes))
    finally:
        shutil.rmtree(outdir, ignore_errors=True)
    sys.exit(0 if all(results) else 1)


if __name__ == "__main__":
    main()
