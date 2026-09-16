#!/usr/bin/env python3
"""
Integration tests for the DMVCFramework Minimal API sample.

Covers: prefix groups, typed group data (TApiVersion), hook chain
(OnBefore/OnSuccess/OnError/OnAlways) including filter short-circuit
and error-handler replacement.

Boots MinimalAPISample.exe in a child process, runs requests against
http://localhost:8080, asserts status codes and response bodies, then
shuts the server down.

Usage:
    python test_minimal_api.py

Exit code 0 = all green, non-zero = at least one failure.
"""
from __future__ import annotations

import socket
import subprocess
import sys
import time
import unittest
from contextlib import contextmanager
from pathlib import Path

import requests

SCRIPT_DIR = Path(__file__).resolve().parent
EXE = SCRIPT_DIR / "MinimalAPISample.exe"
BASE = "http://localhost:8080"
BOOT_TIMEOUT_S = 10


# ---------------------------------------------------------------------------
def wait_for_port(host: str, port: int, timeout: float) -> None:
    deadline = time.monotonic() + timeout
    last_err: Exception | None = None
    while time.monotonic() < deadline:
        try:
            with socket.create_connection((host, port), timeout=0.5):
                return
        except OSError as e:
            last_err = e
            time.sleep(0.1)
    raise RuntimeError(f"server did not open port {port} within {timeout}s: {last_err}")


@contextmanager
def server():
    if not EXE.exists():
        raise SystemExit(f"missing executable: {EXE}\nbuild it first.")
    proc = subprocess.Popen(
        [str(EXE)],
        cwd=str(SCRIPT_DIR),
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        creationflags=getattr(subprocess, "CREATE_NEW_PROCESS_GROUP", 0),
    )
    try:
        wait_for_port("127.0.0.1", 8080, BOOT_TIMEOUT_S)
        yield proc
    finally:
        try:
            proc.terminate()
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()


# ---------------------------------------------------------------------------
def envelope_message(resp_json):
    """The Body(string) helper wraps strings as {"message": ...}."""
    if isinstance(resp_json, dict) and "message" in resp_json:
        return resp_json["message"]
    return resp_json


# ---------------------------------------------------------------------------
class MinimalAPITests(unittest.TestCase):
    """
    Routes wired in RoutesU.ConfigureRoutes:

      Root group (no prefix, no data, no hooks):
        GET    /health
        GET    /search           (record hybrid binding)

      /v1 group (TApiVersion data, hook stack):
        GET    /v1/people         (handler reads TApiVersion)
        GET    /v1/error          (raises -> OnError + NotFoundMapper)

      /v2 group (TApiVersion data, hook stack):
        GET    /v2/people         (handler reads TApiVersion)
        GET    /v2/people/(id)
        POST   /v2/people
        PUT    /v2/people/(id)
        DELETE /v2/people/(id)

      /v2/admin sub-group (BeforeFilter for X-Admin-Key):
        GET    /v2/admin/stats
    """

    new_person_id: int | None = None

    # ------ basics ---------------------------------------------------------
    def test_010_health(self):
        r = requests.get(f"{BASE}/health")
        self.assertEqual(r.status_code, 200)
        self.assertEqual(envelope_message(r.json()), "OK")

    def test_020_unknown_route_passthrough(self):
        # No minimal route nor controller matches: middleware must NOT swallow.
        r = requests.get(f"{BASE}/no-such-route")
        self.assertEqual(r.status_code, 404)

    # ------ /v1 group: deprecated version ----------------------------------
    def test_100_v1_people_carries_deprecation_flag(self):
        # Group data (TApiVersion) is injected as a handler parameter.
        # Handler returns a real TJsonObject -> body is the raw JSON object,
        # not wrapped in {"message": "..."}.
        r = requests.get(f"{BASE}/v1/people")
        self.assertEqual(r.status_code, 200)
        body = r.json()
        self.assertEqual(body["version"], 1)
        self.assertTrue(body["deprecated"])
        self.assertIn("sunset", body)
        self.assertGreaterEqual(body["count"], 2)

    def test_110_v1_error_handler_maps_not_found_to_404(self):
        # /v1/error raises Exception('thing not found').
        # NotFoundMapper (OnError func) detects "not found" and replaces
        # the default 500 envelope with a 404 NotFound response.
        r = requests.get(f"{BASE}/v1/error")
        self.assertEqual(r.status_code, 404, f"expected 404 got {r.status_code}: {r.text}")
        self.assertIn("not found", envelope_message(r.json()).lower())

    # ------ /v2 group: current version, full CRUD --------------------------
    def test_200_v2_people_no_deprecation(self):
        r = requests.get(f"{BASE}/v2/people")
        self.assertEqual(r.status_code, 200)
        body = r.json()
        self.assertEqual(body["version"], 2)
        self.assertNotIn("deprecated", body)
        self.assertGreaterEqual(body["count"], 2)

    def test_210_v2_get_one(self):
        r = requests.get(f"{BASE}/v2/people/1")
        self.assertEqual(r.status_code, 200)
        person = r.json().get("data", r.json())
        self.assertEqual(person["id"], 1)
        self.assertEqual(person["firstName"], "Daniele")

    def test_220_v2_get_one_not_found(self):
        # The handler returns NotFound() directly — does NOT go through
        # the OnError handler chain (no exception is raised).
        r = requests.get(f"{BASE}/v2/people/9999")
        self.assertEqual(r.status_code, 404)

    def test_230_v2_post_create(self):
        payload = {"firstName": "Mario", "lastName": "Rossi", "age": 30}
        r = requests.post(f"{BASE}/v2/people", json=payload)
        self.assertEqual(r.status_code, 201)
        loc = r.headers.get("location") or r.headers.get("Location")
        self.assertIsNotNone(loc)
        self.assertTrue(loc.startswith("/v2/people/"))
        person = r.json().get("data")
        self.assertEqual(person["firstName"], "Mario")
        type(self).new_person_id = person["id"]

    def test_240_v2_put_update(self):
        nid = type(self).new_person_id
        self.assertIsNotNone(nid)
        r = requests.put(
            f"{BASE}/v2/people/{nid}",
            json={"firstName": "Mario", "lastName": "Verdi", "age": 31},
        )
        self.assertEqual(r.status_code, 204)
        r2 = requests.get(f"{BASE}/v2/people/{nid}")
        self.assertEqual(r2.status_code, 200)
        person = r2.json().get("data")
        self.assertEqual(person["lastName"], "Verdi")

    def test_250_v2_delete(self):
        nid = type(self).new_person_id
        r = requests.delete(f"{BASE}/v2/people/{nid}")
        self.assertEqual(r.status_code, 204)
        r2 = requests.get(f"{BASE}/v2/people/{nid}")
        self.assertEqual(r2.status_code, 404)

    # ------ /v2/admin sub-group: BeforeFilter short-circuit ---------------
    def test_300_admin_without_key_is_blocked(self):
        # No X-Admin-Key -> OnBefore filter returns Status(401), short-circuits.
        r = requests.get(f"{BASE}/v2/admin/stats")
        self.assertEqual(r.status_code, 401)
        self.assertIn("admin key", envelope_message(r.json()))

    def test_310_admin_with_wrong_key_is_blocked(self):
        r = requests.get(
            f"{BASE}/v2/admin/stats",
            headers={"X-Admin-Key": "wrong"},
        )
        self.assertEqual(r.status_code, 401)

    def test_320_admin_with_correct_key_passes(self):
        r = requests.get(
            f"{BASE}/v2/admin/stats",
            headers={"X-Admin-Key": "s3cret"},
        )
        self.assertEqual(r.status_code, 200)
        body = r.json()
        self.assertEqual(body["stats"], "all good")

    # ------ Hybrid record binding -----------------------------------------
    def test_400_search_with_query_and_header(self):
        r = requests.get(
            f"{BASE}/search",
            params={"page": 2, "pageSize": 10},
            headers={"X-Tenant": "acme"},
        )
        self.assertEqual(r.status_code, 200)
        body = r.json()
        self.assertEqual(body["tenant"], "acme")
        self.assertEqual(body["page"], 2)
        self.assertEqual(body["pageSize"], 10)
        self.assertGreaterEqual(body["total"], 1)

    def test_410_search_uses_attribute_defaults(self):
        r = requests.get(f"{BASE}/search")
        self.assertEqual(r.status_code, 200)
        body = r.json()
        self.assertEqual(body["tenant"], "default")
        self.assertEqual(body["page"], 1)
        self.assertEqual(body["pageSize"], 20)


# ---------------------------------------------------------------------------
def main() -> int:
    with server():
        loader = unittest.TestLoader()
        loader.sortTestMethodsUsing = lambda a, b: (a > b) - (a < b)
        suite = loader.loadTestsFromTestCase(MinimalAPITests)
        runner = unittest.TextTestRunner(verbosity=2)
        result = runner.run(suite)
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    sys.exit(main())
