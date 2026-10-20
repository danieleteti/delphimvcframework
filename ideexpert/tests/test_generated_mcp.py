#!/usr/bin/env python3
"""Runs the MCP projects generated (and compiled) by TestTemplateGenerator.exe.

Run TestTemplateGenerator first: this script starts the executables it left in
output/<case>/Win32/Debug, over HTTP and over stdio, and checks what a generated
MCP server promises. Needs: pip install requests.

    python test_generated_mcp.py [--output-dir output] [--mcp-library C:\\DEV\\mcp-server-delphi]
"""
import argparse
import atexit
import base64
import concurrent.futures
import hashlib
import hmac
import json
import os
import shutil
import socket
import subprocess
import sys
import tempfile
import time

import requests

HERE = os.path.dirname(os.path.abspath(__file__))
PROTOCOL = "2025-03-26"
EXE = "TestProject.exe"
NON_ASCII = "Daniele è qui: € ✓ 日本"

passed = 0
failed = []


def check(name, condition, detail=""):
    global passed
    if condition:
        passed += 1
        print(f"  [PASS] {name}")
    else:
        failed.append(name)
        print(f"  [FAIL] {name}: {detail}")


def free_port():
    s = socket.socket()
    s.bind(("127.0.0.1", 0))
    port = s.getsockname()[1]
    s.close()
    return port


def lan_ip():
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        s.connect(("10.255.255.255", 1))
        return s.getsockname()[0]
    except OSError:
        return ""
    finally:
        s.close()


def can_connect(ip, port):
    s = socket.socket()
    s.settimeout(2)
    try:
        s.connect((ip, port))
        return True
    except OSError:
        return False
    finally:
        s.close()


def deploy(case_dir):
    """The layout of a project built by the IDE: the executable in bin, next to .env.
    Returns a copy of the project folder, so that a run leaves the suite output alone."""
    root = tempfile.mkdtemp(prefix="dmvc_mcp_")
    atexit.register(shutil.rmtree, root, True)
    target = os.path.join(root, os.path.basename(case_dir))
    shutil.copytree(case_dir, target, ignore=shutil.ignore_patterns("Win32", "*.dcu"))
    shutil.copy(os.path.join(case_dir, "Win32", "Debug", EXE), os.path.join(target, "bin", EXE))
    return target


def read_env(project):
    values = {}
    with open(os.path.join(project, "bin", ".env"), encoding="utf-8-sig") as f:
        for line in f:
            line = line.strip()
            if line and not line.startswith("#") and "=" in line:
                key, value = line.split("=", 1)
                values[key] = value
    return values


def set_env(project, **changes):
    path = os.path.join(project, "bin", ".env")
    with open(path, encoding="utf-8-sig") as f:
        lines = f.read().splitlines()
    for key, value in changes.items():
        key = key.replace("__", ".")
        for i, line in enumerate(lines):
            if line.startswith(key + "="):
                lines[i] = f"{key}={value}"
                break
        else:
            lines.append(f"{key}={value}")
    with open(path, "w", encoding="utf-8") as f:
        f.write("\n".join(lines) + "\n")


class HttpServer:
    def __init__(self, project, port):
        self.project, self.port, self.proc = project, port, None
        set_env(project, dmvc__server__port=port)

    def __enter__(self):
        bin_dir = os.path.join(self.project, "bin")
        self.proc = subprocess.Popen([os.path.join(bin_dir, EXE), "--transport", "http"], cwd=bin_dir,
                                     stdin=subprocess.PIPE, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        deadline = time.time() + 30
        while time.time() < deadline and self.proc.poll() is None:
            if can_connect("127.0.0.1", self.port):
                return self
            time.sleep(0.2)
        code = self.proc.poll()
        self.__exit__()
        raise RuntimeError(f"the server did not start (exit code {code}): {self.project}")

    def __exit__(self, *exc):
        if self.proc:
            self.proc.kill()
            self.proc.wait(timeout=10)


def rpc(url, method, params=None, session=None, headers=None):
    payload = {"jsonrpc": "2.0", "id": 1, "method": method}
    if params is not None:
        payload["params"] = params
    hdrs = {"Content-Type": "application/json"}
    if session:
        hdrs["Mcp-Session-Id"] = session
    hdrs.update(headers or {})
    return requests.post(url, data=json.dumps(payload).encode("utf-8"), headers=hdrs, timeout=15)


INIT = {"protocolVersion": PROTOCOL, "capabilities": {}, "clientInfo": {"name": "wizard-test", "version": "1"}}


def http_round(url, headers, expect_resources):
    """initialize, the lists, one call: what a client does first."""
    resp = rpc(url, "initialize", INIT, headers=headers)
    check("initialize: 200 and a session", resp.status_code == 200 and "Mcp-Session-Id" in resp.headers,
          f"{resp.status_code} {resp.text[:200]}")
    if resp.status_code != 200:
        return None
    session = resp.headers["Mcp-Session-Id"]
    # with the bridge on, the library adds a "not for production" note to the name
    check("server name is the project name", resp.json()["result"]["serverInfo"]["name"].startswith("TestProject"),
          resp.text[:200])
    tools = rpc(url, "tools/list", session=session, headers=headers).json()["result"]["tools"]
    names = {t["name"] for t in tools}
    check("tools/list has the sample tool", "hello" in names, str(sorted(names)))
    call = rpc(url, "tools/call", {"name": "hello", "arguments": {"name": NON_ASCII}}, session, headers).json()
    text = call.get("result", {}).get("content", [{}])[0].get("text", "")
    check("tools/call returns the greeting, non-ASCII intact", text == f"Hello, {NON_ASCII}!", str(call)[:300])
    if expect_resources:
        res = rpc(url, "resources/list", session=session, headers=headers).json()["result"]["resources"]
        check("resources/list has the sample resource", len(res) == 1, str(res))
        read = rpc(url, "resources/read", {"uri": res[0]["uri"]}, session, headers).json()
        check("resources/read returns it", "TestProject" in json.dumps(read), str(read)[:300])
        prompts = rpc(url, "prompts/list", session=session, headers=headers).json()["result"]["prompts"]
        check("prompts/list has the sample prompt", [p["name"] for p in prompts] == ["summarize"], str(prompts))
        got = rpc(url, "prompts/get", {"name": "summarize", "arguments": {"text": NON_ASCII}}, session, headers).json()
        check("prompts/get carries the argument", NON_ASCII in json.dumps(got, ensure_ascii=False), str(got)[:300])
    return names


def test_http_token(case_dir, standalone, expect_resources):
    print(f"\n--- HTTP, bearer token: {os.path.basename(case_dir)} ---")
    project = deploy(case_dir)
    port = free_port()
    url = f"http://localhost:{port}/mcp"
    token = read_env(project).get("mcp.auth.token", "")
    check("the wizard wrote a token in .env", len(token) >= 32, repr(token))
    auth = {"Authorization": f"Bearer {token}"}
    with HttpServer(project, port):
        names = http_round(url, auth, expect_resources)
        r = rpc(url, "initialize", INIT)
        check("no token: 401 with WWW-Authenticate",
              r.status_code == 401 and r.headers.get("WWW-Authenticate", "").startswith("Bearer"),
              f"{r.status_code} {dict(r.headers)}")
        r = rpc(url, "initialize", INIT, headers={"Authorization": "Bearer wrong"})
        check("wrong token: 401", r.status_code == 401, str(r.status_code))
        r = requests.delete(url, timeout=10)
        check("DELETE without token: 401", r.status_code == 401, str(r.status_code))
        r = requests.get(url, headers=auth, timeout=10)
        check("GET /mcp: 405", r.status_code == 405, str(r.status_code))
        r = requests.get(url + "/describe", headers=auth, timeout=10)
        check("GET /mcp/describe: 405", r.status_code == 405, str(r.status_code))
        r = rpc(url, "initialize", INIT, headers={**auth, "Origin": "http://evil.example"})
        check("an Origin that is not listed: 403", r.status_code == 403, str(r.status_code))
        if standalone:
            ip = lan_ip()
            if ip and not ip.startswith("127."):
                # Indy refuses the connection; HTTP.sys accepts it and answers 400 to a host it was not given
                try:
                    served = rpc(f"http://{ip}:{port}/mcp", "initialize", INIT, headers=auth).status_code
                except requests.exceptions.RequestException:
                    served = 0
                check(f"answers on localhost only (not on {ip})", served not in (200, 401, 403, 405), str(served))
            else:
                print("  [SKIP] no LAN address on this machine: the bind address was not checked")
    # an Origin listed in .env is let through
    set_env(project, mcp__allowed_origins="http://localhost:5173, http://app.example")
    with HttpServer(project, port):
        r = rpc(url, "initialize", INIT, headers={**auth, "Origin": "http://app.example"})
        check("an Origin listed in mcp.allowed_origins: 200", r.status_code == 200, str(r.status_code))
    # an empty token: the program ends before it listens, and says which key
    set_env(project, mcp__auth__token="")
    bin_dir = os.path.join(project, "bin")
    proc = subprocess.Popen([os.path.join(bin_dir, EXE), "--transport", "http"], cwd=bin_dir, stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    listened = False
    deadline = time.time() + 20
    while proc.poll() is None and time.time() < deadline:
        listened = listened or can_connect("127.0.0.1", port)
        time.sleep(0.1)
    try:
        out, _ = proc.communicate(timeout=1)
        ended = True
    except subprocess.TimeoutExpired:
        proc.kill()
        out, _ = proc.communicate()
        ended = False
    check("empty token: the program ends without ever listening", ended and not listened)
    check("empty token: the exit code says it failed", proc.returncode == 1, str(proc.returncode))
    check("empty token: the message names the key", b"mcp.auth.token" in out, out[-300:].decode("utf-8", "replace"))
    return project, names


class Stdio:
    def __init__(self, project, cwd, args=("--transport", "stdio")):
        self.proc = subprocess.Popen([os.path.join(project, "bin", EXE), *args], cwd=cwd,
                                     stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        self.lines = []

    def call(self, method, params=None, notify=False):
        msg = {"jsonrpc": "2.0", "method": method}
        if not notify:
            msg["id"] = len(self.lines) + 1
        if params is not None:
            msg["params"] = params
        self.proc.stdin.write((json.dumps(msg, ensure_ascii=False) + "\n").encode("utf-8"))
        self.proc.stdin.flush()
        if notify:
            return None
        line = self.proc.stdout.readline()
        self.lines.append(line)
        return json.loads(line.decode("utf-8"))

    def close(self):
        self.proc.stdin.close()
        try:
            self.proc.wait(timeout=15)
        except subprocess.TimeoutExpired:
            self.proc.kill()
            self.proc.wait()
            return None, b"", b""
        return self.proc.returncode, self.proc.stdout.read(), self.proc.stderr.read()


def test_stdio(case_dir, expect_resources, expect_log_file=True):
    print(f"\n--- stdio: {os.path.basename(case_dir)} ---")
    project = deploy(case_dir)
    # the client chooses the working directory: not the executable's
    cwd = tempfile.mkdtemp(prefix="dmvc_mcp_cwd_")
    s = Stdio(project, cwd)
    try:
        init = s.call("initialize", INIT)
        check("initialize", init.get("result", {}).get("serverInfo", {}).get("name") == "TestProject", str(init)[:300])
        s.call("notifications/initialized", notify=True)
        names = {t["name"] for t in s.call("tools/list")["result"]["tools"]}
        check("tools/list has the sample tool", "hello" in names, str(sorted(names)))
        call = s.call("tools/call", {"name": "hello", "arguments": {"name": NON_ASCII}})
        text = call.get("result", {}).get("content", [{}])[0].get("text", "")
        check("tools/call, non-ASCII in both directions", text == f"Hello, {NON_ASCII}!", str(call)[:300])
        if expect_resources:
            res = s.call("resources/list")["result"]["resources"]
            read = s.call("resources/read", {"uri": res[0]["uri"]})
            check("resources/list and resources/read", "TestProject" in json.dumps(read), str(read)[:300])
            got = s.call("prompts/get", {"name": "summarize", "arguments": {"text": NON_ASCII}})
            check("prompts/get", NON_ASCII in json.dumps(got, ensure_ascii=False), str(got)[:300])
        unknown = s.call("no/such/method")
        check("unknown method: a JSON-RPC error, the server stays up", "error" in unknown, str(unknown)[:200])
    finally:
        code, rest, err = s.close()
    check("ends when the client closes the pipe (exit code 0)", code == 0, str(code))
    check("stdout carried nothing but the JSON-RPC answers", rest == b"" and all(
        l.endswith(b"\n") and not l.endswith(b"\r\n") and l.startswith(b"{") for l in s.lines), repr(rest[:200]))
    # stderr is the transport's own diagnostics channel: the library writes one start-up line there
    check("stderr carries no JSON-RPC and no exception", b'"jsonrpc"' not in err and b"Exception" not in err,
          err[:300].decode("utf-8", "replace"))
    logs = os.path.join(project, "bin", "logs")
    has_log = os.path.isdir(logs) and any(os.scandir(logs))
    if expect_log_file:
        check("the log file is next to the executable", has_log, logs)
    else:
        check("logging disabled: no log file", not has_log, logs)
    check("nothing was written in the client's working directory", not os.listdir(cwd), str(os.listdir(cwd)))
    mcp_json = json.load(open(os.path.join(project, ".mcp.json"), encoding="utf-8"))
    server = mcp_json["mcpServers"]["TestProject"]
    check(".mcp.json points at the built executable, relative to the project",
          os.path.isfile(os.path.join(project, server["command"])) and server["args"] == ["--transport", "stdio"],
          str(server))
    return project


def test_default_transport(case_dir, stdio_expected):
    """Started with no arguments: stdio in the "MCP Server" preset, the HTTP server everywhere else."""
    print(f"\n--- no --transport argument: {os.path.basename(case_dir)} ---")
    project = deploy(case_dir)
    port = free_port()
    set_env(project, dmvc__server__port=port)
    if stdio_expected:
        s = Stdio(project, tempfile.mkdtemp(prefix="dmvc_mcp_cwd_"), args=())
        try:
            init = s.call("initialize", INIT)
            check("no arguments: it talks MCP on stdin/stdout",
                  init.get("result", {}).get("serverInfo", {}).get("name") == "TestProject", str(init)[:200])
            check("no arguments: no HTTP server", not can_connect("127.0.0.1", port))
        finally:
            code, rest, err = s.close()
        check("no arguments: stderr says how to get the HTTP server", b"--transport http" in err,
              err[:200].decode("utf-8", "replace"))
        check("no arguments: stdout carried the JSON-RPC answer only", rest == b"" and len(s.lines) == 1)
    else:
        bin_dir = os.path.join(project, "bin")
        proc = subprocess.Popen([os.path.join(bin_dir, EXE)], cwd=bin_dir, stdin=subprocess.PIPE,
                                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        try:
            deadline = time.time() + 30
            while time.time() < deadline and proc.poll() is None and not can_connect("127.0.0.1", port):
                time.sleep(0.2)
            check("no arguments: the HTTP server, as before", can_connect("127.0.0.1", port))
        finally:
            proc.kill()
            proc.wait(timeout=10)


def test_stdio_library_suite(project, library):
    suite = os.path.join(library, "tests", "test_mcp_server_stdio.py")
    if not os.path.isfile(suite):
        print(f"  [SKIP] library stdio suite not found: {suite}")
        return
    cmd = f'"{os.path.join(project, "bin", EXE)}" --transport stdio'
    out = subprocess.run([sys.executable, suite, "--cmd", cmd, "--server-independent"],
                         capture_output=True, text=True, encoding="utf-8", errors="replace", timeout=300)
    tail = "\n".join(out.stdout.strip().splitlines()[-4:])
    check("the library's stdio suite (--server-independent) passes on the generated server",
          out.returncode == 0, tail + out.stderr[-300:])
    print("    " + tail.replace("\n", "\n    "))


def test_bridge(case_dir, calls=1):
    print(f"\n--- REST project with /mcp and the bridge: {os.path.basename(case_dir)} ---")
    project = deploy(case_dir)
    port = free_port()
    url = f"http://localhost:{port}/mcp"
    auth = {"Authorization": f"Bearer {read_env(project)['mcp.auth.token']}"}
    with HttpServer(project, port):
        rest = requests.get(f"http://localhost:{port}/api", timeout=10)
        check("the REST routes still answer next to /mcp", rest.status_code == 200, f"{rest.status_code} {rest.text[:100]}")
        session = rpc(url, "initialize", INIT, headers=auth).headers.get("Mcp-Session-Id")
        tools = rpc(url, "tools/list", session=session, headers=auth).json()["result"]["tools"]
        names = sorted(t["name"] for t in tools)
        bridged = [n for n in names if n != "hello"]
        check("the bridge lists the controller actions", len(bridged) >= 1, str(names))
        check("no DELETE action, no system controller, no MCP endpoint among the tools",
              not any(n.startswith("delete_") or "system" in n or "mcp" in n for n in bridged), str(bridged))
        getters = [n for n in bridged if n.startswith("get_")]
        for i in range(calls):
            call = rpc(url, "tools/call", {"name": getters[0], "arguments": {}}, session, auth).json()
            ok = "result" in call and not call["result"].get("isError", False)
            if not ok or i == calls - 1:
                check(f"a bridged tool call returns the REST response ({i + 1} of {calls}): {getters[0]}", ok,
                      str(call)[:300])
            if not ok:
                break
    return bridged


def test_jwt(case_dir):
    print(f"\n--- /mcp behind the JWT middleware: {os.path.basename(case_dir)} ---")
    project = deploy(case_dir)
    port = free_port()
    url = f"http://localhost:{port}/mcp"
    set_env(project, ADMIN_USER="admin", ADMIN_PASSWORD="secret-for-the-test")
    with HttpServer(project, port):
        r = rpc(url, "initialize", INIT)
        check("no JWT: refused", r.status_code in (401, 403), str(r.status_code))
        r = rpc(url, "initialize", INIT, headers={"Authorization": "Bearer not-a-jwt"})
        check("a bearer that is not a JWT: refused", r.status_code in (401, 403), str(r.status_code))
        r = requests.delete(url, headers={"Mcp-Session-Id": "none"}, timeout=10)
        check("DELETE /mcp without a JWT: refused", r.status_code in (401, 403), str(r.status_code))
        login = requests.post(f"http://localhost:{port}/login", auth=("admin", "secret-for-the-test"), timeout=10)
        check("login returns a token", login.status_code == 200 and "token" in login.json(), login.text[:200])
        auth = {"Authorization": "Bearer " + login.json().get("token", "")}
        r = rpc(url, "initialize", INIT, headers=auth)
        check("a valid JWT: served", r.status_code == 200 and "Mcp-Session-Id" in r.headers,
              f"{r.status_code} {r.text[:200]}")
        if r.status_code == 200:
            call = rpc(url, "tools/call", {"name": "hello", "arguments": {"name": "x"}},
                       r.headers["Mcp-Session-Id"], auth).json()
            check("a tool call with the JWT", "result" in call, str(call)[:200])


def raw_post(url, headers, body, timeout=15):
    return requests.post(url, data=body, headers=headers, timeout=timeout)


def b64u(data):
    return base64.urlsafe_b64encode(data).rstrip(b"=").decode("ascii")


def unb64u(text):
    return base64.urlsafe_b64decode(text + "=" * (-len(text) % 4))


HASHES = {"HS256": hashlib.sha256, "HS384": hashlib.sha384, "HS512": hashlib.sha512}


def forge_jwt(secret, payload, alg="HS512", sign=True):
    """A JWT built here: the test holds the project's secret, so it can issue tokens the server must judge."""
    header = {"alg": alg if sign else "none", "typ": "JWT"}
    signing_input = b64u(json.dumps(header).encode()) + "." + b64u(json.dumps(payload).encode())
    signature = b""
    if sign:
        signature = hmac.new(secret.encode("utf-8"), signing_input.encode("ascii"), HASHES[alg]).digest()
    return signing_input + "." + b64u(signature)


def refused(r):
    return r.status_code in (401, 403)


def test_http_auth_complete(case_dir):
    """/mcp over HTTP behind the generated bearer token, from the first request to the last: every way
    to present, mangle or skip the credential, the session life cycle under it, and the same
    server still answering the right caller afterwards."""
    print(f"\n--- HTTP + bearer token, complete: {os.path.basename(case_dir)} ---")
    project = deploy(case_dir)
    port = free_port()
    url = f"http://localhost:{port}/mcp"
    token = read_env(project)["mcp.auth.token"]
    auth = {"Authorization": f"Bearer {token}"}
    with HttpServer(project, port) as server:
        # --- the credential, presented in every wrong way
        flipped = token[:-1] + ("0" if token[-1] != "0" else "1")
        variants = {
            "a token with its last character changed": {"Authorization": f"Bearer {flipped}"},
            "a truncated token": {"Authorization": f"Bearer {token[:-4]}"},
            "the token with something appended": {"Authorization": f"Bearer {token}x"},
            "the token twice": {"Authorization": f"Bearer {token} {token}"},
            "the Basic scheme carrying the token": {"Authorization": f"Basic {token}"},
            "no scheme at all": {"Authorization": token},
            "an empty bearer": {"Authorization": "Bearer "},
            "the scheme alone": {"Authorization": "Bearer"},
            "the token in another header": {"X-Api-Key": token},
            "the token in a cookie": {"Cookie": f"token={token}"},
            "an enormous Authorization header": {"Authorization": "Bearer " + "A" * 8000},
        }
        for what, headers in variants.items():
            try:
                r = rpc(url, "initialize", INIT, headers=headers)
                check(f"refused: {what}", refused(r) or r.status_code in (400, 431), str(r.status_code))
            except requests.exceptions.RequestException as e:
                check(f"refused: {what}", True, str(e))  # a connection closed on an absurd header is a refusal too
        r = requests.post(url + f"?access_token={token}", data=json.dumps(
            {"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": INIT}),
            headers={"Content-Type": "application/json"}, timeout=15)
        check("refused: the token in the query string", refused(r), str(r.status_code))
        r = rpc(url, "initialize", INIT, headers={"Authorization": f"bearer {token}"})
        check("the scheme name is case-insensitive (RFC 7235): 'bearer <token>' is served", r.status_code == 200,
              str(r.status_code))
        r = rpc(url, "initialize", INIT, headers={"Authorization": f"Bearer   {token}  "})
        check("blanks around the token do not matter", r.status_code == 200, str(r.status_code))

        # --- what a refusal says
        r = rpc(url, "initialize", INIT, headers={"Authorization": "Bearer wrong"})
        check("a 401 carries WWW-Authenticate: Bearer", r.status_code == 401 and
              r.headers.get("WWW-Authenticate", "").startswith("Bearer"), str(dict(r.headers)))
        check("a 401 does not echo the credential or hand out the expected one",
              token not in r.text and "wrong" not in r.text and token not in str(dict(r.headers)), r.text[:200])

        # --- the credential comes before everything else, whatever the request is
        for method in ("PUT", "PATCH", "DELETE", "OPTIONS", "HEAD"):
            r = requests.request(method, url, timeout=10)
            check(f"{method} /mcp with no token is not served", r.status_code in (401, 403, 405),
                  str(r.status_code))
        r = raw_post(url, {"Content-Type": "application/json"}, b"{ this is not json")
        check("malformed JSON with no token: 401, not a parse error", r.status_code == 401, f"{r.status_code} {r.text[:100]}")
        r = raw_post(url, {"Content-Type": "application/json", **auth}, b"{ this is not json")
        check("malformed JSON with the token: the server stays up and answers with an error",
              r.status_code in (200, 400) and ("error" in r.text or r.status_code == 400), f"{r.status_code} {r.text[:150]}")

        # --- the session is not a credential
        init = rpc(url, "initialize", INIT, headers=auth)
        session = init.headers["Mcp-Session-Id"]
        for method, params in (("tools/list", None), ("tools/call", {"name": "hello", "arguments": {"name": "x"}}),
                               ("resources/list", None), ("prompts/list", None), ("ping", None)):
            r = rpc(url, method, params, session=session)
            check(f"{method} with a valid session id and no token: 401", r.status_code == 401, str(r.status_code))
        r = requests.delete(url, headers={"Mcp-Session-Id": session}, timeout=10)
        check("DELETE with a valid session id and no token: 401", r.status_code == 401, str(r.status_code))
        r = rpc(url, "tools/list", session=session, headers=auth)
        check("... and that session is still alive afterwards", r.status_code == 200 and "tools" in r.json().get("result", {}),
              f"{r.status_code} {r.text[:150]}")

        # --- with the token: the protocol still behaves
        r = rpc(url, "ping", session=session, headers=auth)
        check("ping", r.status_code == 200 and "error" not in r.json(), r.text[:150])
        r = rpc(url, "no/such/method", session=session, headers=auth)
        check("an unknown method: a JSON-RPC error (-32601), HTTP 200",
              r.status_code == 200 and r.json().get("error", {}).get("code") == -32601, r.text[:150])
        r = rpc(url, "tools/call", {"name": "no_such_tool", "arguments": {}}, session, auth)
        check("an unknown tool: an error, not a crash", r.status_code in (200, 400) and "error" in r.text.lower(), r.text[:150])
        r = rpc(url, "tools/call", {"name": "hello", "arguments": {}}, session, auth)
        check("a tool called without its argument: an answer, not a crash", r.status_code in (200, 400), str(r.status_code))
        r = requests.post(url, data=json.dumps({"jsonrpc": "2.0", "method": "notifications/initialized"}),
                          headers={"Content-Type": "application/json", "Mcp-Session-Id": session, **auth}, timeout=10)
        check("a notification is accepted (no id, no body)", r.status_code in (200, 202, 204), str(r.status_code))
        r = rpc(url, "tools/call", {"name": "hello", "arguments": {"name": NON_ASCII}}, "no-such-session", auth)
        check("the token with a session id the server never issued: not a success",
              r.status_code != 200 or "error" in r.text.lower(), f"{r.status_code} {r.text[:150]}")

        # --- many callers at once: the credential is checked on every request, none leaks into another
        def call(i):
            good = rpc(url, "tools/call", {"name": "hello", "arguments": {"name": f"n{i}"}}, session, auth)
            bad = rpc(url, "tools/call", {"name": "hello", "arguments": {"name": f"n{i}"}}, session)
            return good, bad
        with concurrent.futures.ThreadPoolExecutor(max_workers=16) as pool:
            results = list(pool.map(call, range(48)))
        ok = all(g.status_code == 200 and g.json()["result"]["content"][0]["text"] == f"Hello, n{i}!"
                 for i, (g, _) in enumerate(results))
        check("48 concurrent authenticated calls: each gets its own answer", ok)
        check("48 concurrent calls with no token: every one refused", all(b.status_code == 401 for _, b in results))

        # --- end of the session
        r = requests.delete(url, headers={"Mcp-Session-Id": session, **auth}, timeout=10)
        check("DELETE with the token ends the session", r.status_code in (200, 204), str(r.status_code))
        r = rpc(url, "tools/list", session=session, headers=auth)
        check("a call on the ended session is not served", r.status_code != 200 or "error" in r.text.lower(),
              f"{r.status_code} {r.text[:150]}")
        r = rpc(url, "initialize", INIT, headers=auth)
        check("a new session can be opened with the same token", r.status_code == 200 and "Mcp-Session-Id" in r.headers,
              str(r.status_code))
        check("after all of it the server process is still running", server.proc.poll() is None)

    # --- the token is read at start: a new one replaces the old
    new_token = "rotated" + token[7:]
    set_env(project, mcp__auth__token=new_token)
    with HttpServer(project, port):
        r = rpc(url, "initialize", INIT, headers=auth)
        check("after a rotation the old token is refused", r.status_code == 401, str(r.status_code))
        r = rpc(url, "initialize", INIT, headers={"Authorization": f"Bearer {new_token}"})
        check("after a rotation the new token is served", r.status_code == 200, str(r.status_code))


def test_rest_and_mcp_auth(case_dir):
    """A REST project with /mcp: the token guards /mcp and only /mcp."""
    print(f"\n--- the token guards /mcp and nothing else: {os.path.basename(case_dir)} ---")
    project = deploy(case_dir)
    port = free_port()
    base = f"http://localhost:{port}"
    token = read_env(project)["mcp.auth.token"]
    with HttpServer(project, port):
        r = requests.get(base + "/api", timeout=10)
        check("the REST route answers with no token (it is not part of /mcp)", r.status_code == 200, str(r.status_code))
        r = requests.get(base + "/api", headers={"Authorization": "Bearer wrong"}, timeout=10)
        check("... and a wrong token on it is not what refuses it", r.status_code == 200, str(r.status_code))
        r = rpc(base + "/mcp", "initialize", INIT)
        check("/mcp with no token: 401", r.status_code == 401, str(r.status_code))
        r = rpc(base + "/mcp", "initialize", INIT, headers={"Authorization": f"Bearer {token}"})
        check("/mcp with the token: served", r.status_code == 200, str(r.status_code))
        r = requests.delete(base + "/mcp", timeout=10)
        check("DELETE /mcp (the session controller) with no token: 401", r.status_code == 401, str(r.status_code))
        r = requests.get(base + "/mcp/describe", timeout=10)
        check("nothing under /mcp is served without the token", r.status_code in (401, 403, 405),
              str(r.status_code))


def test_jwt_complete(case_dir):
    """/mcp behind the JWT middleware: the login, the token it issues, and tokens that look like it but are not."""
    print(f"\n--- HTTP + JWT, complete: {os.path.basename(case_dir)} ---")
    project = deploy(case_dir)
    port = free_port()
    base = f"http://localhost:{port}"
    url = base + "/mcp"
    secret = read_env(project)["JWT_SECRET"]
    with HttpServer(project, port):
        r = requests.post(base + "/login", auth=("admin", "anything"), timeout=10)
        check("the generated handler is fail-closed: no ADMIN_USER set, no login", r.status_code == 401, str(r.status_code))
    set_env(project, ADMIN_USER="admin", ADMIN_PASSWORD="secret-for-the-test")
    with HttpServer(project, port):
        for what, creds in (("a wrong password", ("admin", "nope")), ("a wrong user", ("root", "secret-for-the-test")),
                            ("an empty password", ("admin", ""))):
            r = requests.post(base + "/login", auth=creds, timeout=10)
            check(f"login with {what}: 401", r.status_code == 401, str(r.status_code))
        r = requests.post(base + "/login", timeout=10)
        check("login with no credentials: 401", r.status_code == 401, str(r.status_code))
        login = requests.post(base + "/login", auth=("admin", "secret-for-the-test"), timeout=10)
        check("login with the right credentials: a token", login.status_code == 200 and "token" in login.json(), login.text[:150])
        jwt = login.json()["token"]
        header_b64, payload_b64, _ = jwt.split(".")
        claims = json.loads(unb64u(payload_b64))
        alg = json.loads(unb64u(header_b64)).get("alg")
        check("the token is an HMAC-signed JWT with an expiry", alg in HASHES and "exp" in claims, str(claims))

        def serve(token, scheme="Bearer"):
            return rpc(url, "initialize", INIT, headers={"Authorization": f"{scheme} {token}"})

        r = serve(jwt)
        check("the issued token is served", r.status_code == 200 and "Mcp-Session-Id" in r.headers, str(r.status_code))
        session = r.headers.get("Mcp-Session-Id")
        r = rpc(url, "tools/call", {"name": "hello", "arguments": {"name": NON_ASCII}}, session,
                {"Authorization": f"Bearer {jwt}"})
        check("a tool call with it, non-ASCII intact",
              r.json().get("result", {}).get("content", [{}])[0].get("text") == f"Hello, {NON_ASCII}!", r.text[:150])
        check("the test's own forging reproduces a token the server accepts (same secret, same claims)",
              serve(forge_jwt(secret, claims, alg)).status_code == 200)

        # --- tokens that are not what the server issued
        now = int(time.time())
        tampered = {**claims, "username": "someone-else"}
        other_alg = "HS256" if alg != "HS256" else "HS384"
        forgeries = {
            "an expired token": forge_jwt(secret, {**claims, "exp": now - 3600}, alg),
            "a token signed with another secret": forge_jwt("not-the-secret", claims, alg),
            "the payload changed, the signature kept": header_b64 + "." + b64u(json.dumps(tampered).encode()) + "." + jwt.split(".")[2],
            "alg none, no signature": forge_jwt(secret, claims, sign=False),
            "alg none with a signature part": forge_jwt(secret, claims, sign=False) + "AAAA",
            "another HMAC algorithm than the server's": forge_jwt(secret, claims, other_alg),
            "no username claim": forge_jwt(secret, {k: v for k, v in claims.items() if k != "username"}, alg),
            "not yet valid (nbf in the future)": forge_jwt(secret, {**claims, "nbf": now + 3600}, alg),
            # the first character: the last one of an unpadded base64 signature carries spare bits, changing it can decode to the same bytes
            "the first character of the signature changed": jwt.rsplit(".", 1)[0] + "." + ("A" if jwt.rsplit(".", 1)[1][0] != "A" else "B") + jwt.rsplit(".", 1)[1][1:],
            "two segments only": ".".join(jwt.split(".")[:2]),
            "four segments": jwt + ".x",
            "an empty token": "",
            "a header that is not JSON": "bm90anNvbg." + payload_b64 + "." + jwt.split(".")[2],
        }
        for what, token in forgeries.items():
            r = serve(token)
            check(f"refused with 401, not 500: {what}", refused(r), f"{r.status_code} {r.text[:100]}")
        r = serve(jwt, "Basic")
        check("refused: the JWT under the Basic scheme", refused(r), str(r.status_code))
        r = requests.post(url, data=json.dumps({"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": INIT}),
                          headers={"Content-Type": "application/json"}, auth=("admin", "secret-for-the-test"), timeout=15)
        check("refused: Basic credentials on /mcp (login is for /login)", refused(r), str(r.status_code))

        # --- /mcp and its session controller are guarded the same way
        r = requests.delete(url, headers={"Mcp-Session-Id": session}, timeout=10)
        check("DELETE /mcp with no JWT: refused", refused(r), str(r.status_code))
        r = requests.delete(url, headers={"Mcp-Session-Id": session, "Authorization": f"Bearer {jwt}"}, timeout=10)
        check("DELETE /mcp with the JWT ends the session", r.status_code in (200, 204), str(r.status_code))
        r = requests.get(base + "/api", timeout=10)
        check("a route the handler does not protect is served with no JWT", r.status_code == 200, str(r.status_code))
        r = serve(jwt)
        check("after the forgeries the server still serves the right token", r.status_code == 200, str(r.status_code))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--output-dir", default=os.path.join(HERE, "output"))
    parser.add_argument("--mcp-library", default=os.environ.get("MCP_SERVER_DELPHI", r"C:\DEV\mcp-server-delphi"))
    args = parser.parse_args()

    def case(name):
        path = os.path.join(args.output_dir, name)
        if not os.path.isfile(os.path.join(path, "Win32", "Debug", EXE)):
            raise SystemExit(f"{name} was not compiled: run TestTemplateGenerator.exe first (with the MCP library)")
        return path

    # the "MCP Server" preset on the three console hosts
    for name in ("mcp_preset", "httpsys_mcp", "webbroker_mcp_preset"):
        test_default_transport(case(name), stdio_expected=True)
        test_http_token(case(name), standalone=True, expect_resources=True)
        project = test_stdio(case(name), expect_resources=True)
        test_stdio_library_suite(project, args.mcp_library)
    test_http_token(case("webbroker_mcp"), standalone=False, expect_resources=True)

    # stdio and each logging profile
    test_stdio(case("mcp_stdio_log_console_only"), expect_resources=True)
    test_stdio(case("mcp_stdio_log_json"), expect_resources=True)
    test_stdio(case("mcp_stdio_log_json_console_only"), expect_resources=True)
    test_stdio(case("mcp_stdio_log_disabled"), expect_resources=True, expect_log_file=False)

    # the checkboxes on other project types
    test_default_transport(case("indydirect_mcp_rest_bridge"), stdio_expected=False)
    test_default_transport(case("webbroker_mcp"), stdio_expected=False)
    test_bridge(case("indydirect_mcp_rest_bridge"))
    test_bridge(case("webbroker_mcp"), calls=12)  # a bridged call needs a second web module
    test_http_token(case("indydirect_minimal_api_mcp"), standalone=False, expect_resources=True)
    test_jwt(case("indydirect_mcp_jwt"))
    test_http_auth_complete(case("mcp_preset"))
    test_rest_and_mcp_auth(case("indydirect_mcp_rest_bridge"))
    test_jwt_complete(case("indydirect_mcp_jwt"))

    print(f"\n{passed} passed, {len(failed)} failed")
    for name in failed:
        print("  FAILED: " + name)
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
