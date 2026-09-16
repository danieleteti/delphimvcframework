#!/usr/bin/env python3
"""
Integration tests for the Minimal API Auth + Logging sample.

Verifies the full hook chain (OnBefore/OnSuccess/OnError/OnAlways with both
procedure and function variants) by reading back the audit log emitted by
each hook via /api/admin/audit.

Usage:
    python test_minimal_api_auth.py
"""
from __future__ import annotations

import json
import socket
import subprocess
import sys
import time
import unittest
from contextlib import contextmanager
from pathlib import Path

import requests

SCRIPT_DIR = Path(__file__).resolve().parent
EXE = SCRIPT_DIR / "MinimalAPIAuthServer.exe"
PORT = 8081
BASE = f"http://localhost:{PORT}"
BOOT_TIMEOUT_S = 10

ALICE = {"Authorization": "Bearer alice-token"}   # role: user
BOB   = {"Authorization": "Bearer bob-token"}     # role: admin


# ---------------------------------------------------------------------------
def wait_for_port(host: str, port: int, timeout: float) -> None:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        try:
            with socket.create_connection((host, port), timeout=0.5):
                return
        except OSError:
            time.sleep(0.1)
    raise RuntimeError(f"server did not open port {port}")


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
        wait_for_port("127.0.0.1", PORT, BOOT_TIMEOUT_S)
        yield proc
    finally:
        try:
            proc.terminate()
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()


def envelope_text(resp_json):
    """The Body(string)/Status(code,msg) helpers wrap strings as {"message": ...}."""
    if isinstance(resp_json, dict) and "message" in resp_json:
        return resp_json["message"]
    return resp_json


def fetch_audit() -> list[str]:
    """Read the audit log via the admin endpoint and clear it for the next test."""
    r = requests.get(f"{BASE}/api/admin/audit", headers=BOB)
    r.raise_for_status()
    inner = envelope_text(r.json())
    if isinstance(inner, str):
        return json.loads(inner)
    return inner


def clear_audit() -> None:
    requests.delete(f"{BASE}/api/admin/audit", headers=BOB)


# ---------------------------------------------------------------------------
class HookChainTests(unittest.TestCase):
    """Verifies that hooks fire in the right order, with the right semantics."""

    def setUp(self):
        clear_audit()

    # --- public group: only logging, no auth -----------------------------

    def test_010_health_runs_observer_chain(self):
        r = requests.get(f"{BASE}/health")
        self.assertEqual(r.status_code, 200)
        self.assertEqual(envelope_text(r.json()), "OK")

        log = fetch_audit()
        # Audit fetch ran in between, so we expect /health entries somewhere.
        # Find them.
        health_lines = [L for L in log if "/health" in L or L.startswith("REQ GET /health")]
        # OnBefore observer
        self.assertTrue(any(L == "REQ GET /health" for L in health_lines),
                        f"missing REQ in {log}")
        # OnSuccess observer
        self.assertTrue(any(L == "RES 200 /health" for L in health_lines),
                        f"missing RES in {log}")
        # OnAlways
        self.assertTrue(any(L == "DONE /health" for L in health_lines),
                        f"missing DONE in {log}")
        self.assertTrue(any(L.startswith("TIME ") and L.endswith("/health") for L in health_lines),
                        f"missing TIME in {log}")

    def test_020_unmapped_exception_runs_error_observer_and_default_500(self):
        r = requests.get(f"{BASE}/throw")
        self.assertEqual(r.status_code, 500)

        log = fetch_audit()
        self.assertTrue(any(L == "REQ GET /throw" for L in log))
        # OnError observer fired
        self.assertTrue(any(L.startswith("ERR ") and "boom" in L for L in log),
                        f"missing ERR in {log}")
        # OnSuccess MUST NOT fire when an exception was raised
        self.assertFalse(any(L == "RES 200 /throw" for L in log),
                         f"unexpected RES success on /throw in {log}")
        # OnAlways MUST fire even when an exception was raised
        self.assertTrue(any(L == "DONE /throw" for L in log),
                        f"missing DONE in {log}")

    def test_030_domain_exception_is_replaced_by_handler(self):
        # ETokenError -> BusinessErrorsFilter returns 401 instead of 500.
        # LoggingFilter sits OUTSIDE BusinessErrors, so it sees the swapped
        # response (401), NOT the original exception. ERR is therefore NOT
        # logged for this request — the inner mapper handled it.
        r = requests.get(f"{BASE}/throw-token")
        self.assertEqual(r.status_code, 401, r.text)
        self.assertIn("token expired", envelope_text(r.json()))

        log = fetch_audit()
        # Outer LoggingFilter sees the mapped 401 as the final response.
        self.assertTrue(any(L == "RES 401 /throw-token" for L in log),
                        f"missing RES 401 in {log}")
        # Inner mapper swallowed the exception, so no ERR.
        self.assertFalse(any(L.startswith("ERR ") and "token expired" in L for L in log),
                         f"ERR should NOT be logged when inner filter mapped: {log}")
        self.assertTrue(any(L == "DONE /throw-token" for L in log))

    # --- /api group: bearer auth filter ----------------------------------

    def test_100_no_token_blocked_by_filter(self):
        r = requests.get(f"{BASE}/api/me")
        self.assertEqual(r.status_code, 401)
        self.assertIn("Bearer token required", envelope_text(r.json()))

        log = fetch_audit()
        self.assertTrue(any(L == "REQ GET /api/me" for L in log))
        self.assertTrue(any(L == "AUTH missing-bearer" for L in log))
        # Handler MUST NOT have run (no RES line for /api/me with status 200)
        self.assertFalse(any(L == "RES 200 /api/me" for L in log),
                         f"handler ran despite filter short-circuit: {log}")
        # OnAlways MUST still run after filter short-circuit
        self.assertTrue(any(L == "DONE /api/me" for L in log),
                        f"missing DONE after short-circuit: {log}")

    def test_110_invalid_token_blocked(self):
        r = requests.get(f"{BASE}/api/me",
                         headers={"Authorization": "Bearer not-a-real-token"})
        self.assertEqual(r.status_code, 401)
        self.assertIn("Invalid token", envelope_text(r.json()))

        log = fetch_audit()
        self.assertTrue(any(L.startswith("AUTH bad-token=") for L in log))
        self.assertTrue(any(L == "DONE /api/me" for L in log))

    def test_120_valid_user_token_passes(self):
        r = requests.get(f"{BASE}/api/me", headers=ALICE)
        self.assertEqual(r.status_code, 200)
        body = json.loads(envelope_text(r.json()))
        self.assertEqual(body["user"], "alice")
        self.assertEqual(body["role"], "user")

        log = fetch_audit()
        self.assertTrue(any(L == "AUTH ok user=alice role=user" for L in log))
        self.assertTrue(any(L == "RES 200 /api/me" for L in log))
        self.assertTrue(any(L == "DONE /api/me" for L in log))

    def test_130_valid_admin_token_passes(self):
        r = requests.get(f"{BASE}/api/me", headers=BOB)
        self.assertEqual(r.status_code, 200)
        body = json.loads(envelope_text(r.json()))
        self.assertEqual(body["user"], "bob")
        self.assertEqual(body["role"], "admin")

    def test_140_authenticated_domain_exception_still_handled(self):
        # Auth chain succeeds, then the handler raises ETokenError.
        # BusinessErrorsFilter (inner, after Logging) replaces it with 401.
        r = requests.get(f"{BASE}/api/throw-token", headers=ALICE)
        self.assertEqual(r.status_code, 401)
        self.assertIn("token expired", envelope_text(r.json()))

        log = fetch_audit()
        self.assertTrue(any(L == "AUTH ok user=alice role=user" for L in log))
        # OUTER Logging sees the mapped 401, NOT the raw exception.
        self.assertTrue(any(L == "RES 401 /api/throw-token" for L in log))
        self.assertTrue(any(L == "DONE /api/throw-token" for L in log))

    # --- /api/admin: nested sub-group with role gate ---------------------

    def test_200_admin_route_blocked_for_user_role(self):
        r = requests.get(f"{BASE}/api/admin/audit", headers=ALICE)
        self.assertEqual(r.status_code, 403)
        self.assertIn('Role "admin" required', envelope_text(r.json()))

        # Read with admin to inspect what we logged.
        log = fetch_audit()
        self.assertTrue(any(L == "AUTH ok user=alice role=user" for L in log))
        self.assertTrue(any("AUTHZ deny user=alice need-role=admin have=user" in L for L in log))
        self.assertTrue(any(L == "DONE /api/admin/audit" for L in log))

    def test_210_admin_route_open_for_admin_role(self):
        r = requests.get(f"{BASE}/api/admin/audit", headers=BOB)
        self.assertEqual(r.status_code, 200)

    # --- ordering check ---------------------------------------------------

    def test_300_hook_order_for_success_request(self):
        # Single isolated request, then assert ordering of the audit lines.
        clear_audit()
        r = requests.get(f"{BASE}/api/me", headers=ALICE)
        self.assertEqual(r.status_code, 200)

        log = fetch_audit()
        # Filter the lines belonging to /api/me only
        relevant = [L for L in log
                    if "/api/me" in L
                    or L.startswith("AUTH ")
                    or L.startswith("AUTHZ ")
                    or L.startswith("TIME ")
                    or L == "DONE /api/me"]
        # Expected partial ordering:
        #   REQ GET /api/me  -> AUTH ok ... -> RES 200 /api/me -> TIME ... /api/me -> DONE /api/me
        try:
            i_req   = next(i for i, L in enumerate(relevant) if L == "REQ GET /api/me")
            i_auth  = next(i for i, L in enumerate(relevant) if L.startswith("AUTH ok"))
            i_res   = next(i for i, L in enumerate(relevant) if L == "RES 200 /api/me")
            i_done  = next(i for i, L in enumerate(relevant) if L == "DONE /api/me")
            i_time  = next(i for i, L in enumerate(relevant) if L.startswith("TIME ") and L.endswith("/api/me"))
        except StopIteration:
            self.fail(f"missing one of the expected lines in {relevant}")

        self.assertLess(i_req, i_auth, "REQ should precede AUTH ok")
        self.assertLess(i_auth, i_res, "AUTH ok should precede RES")
        self.assertLess(i_res, i_done, "RES should precede DONE")
        self.assertLess(i_res, i_time, "RES should precede TIME")

    def test_310_hook_order_for_short_circuited_request(self):
        # Filter denies request -> handler must NOT run, but Always still does.
        clear_audit()
        r = requests.get(f"{BASE}/api/me")  # no token
        self.assertEqual(r.status_code, 401)

        log = fetch_audit()
        relevant = [L for L in log
                    if "/api/me" in L or L.startswith("AUTH ")]
        i_req  = next(i for i, L in enumerate(relevant) if L == "REQ GET /api/me")
        i_auth = next(i for i, L in enumerate(relevant) if L == "AUTH missing-bearer")
        i_done = next(i for i, L in enumerate(relevant) if L == "DONE /api/me")

        self.assertLess(i_req, i_auth)
        self.assertLess(i_auth, i_done)
        self.assertFalse(any(L == "RES 200 /api/me" for L in relevant),
                         "handler must not have run after short-circuit")

    # --- ProblemDetails RFC 7807 envelope -----------------------------------

    def test_500_problem_details_shape(self):
        # Unmapped exception -> {type, title, status, detail, instance}.
        # The framework wraps the TJsonObject body in {"data": ...}.
        r = requests.get(f"{BASE}/throw")
        self.assertEqual(r.status_code, 500)
        body = r.json().get("data", r.json())
        self.assertEqual(body["type"], "about:blank")
        self.assertEqual(body["status"], 500)
        self.assertEqual(body["title"], "Internal Server Error")
        self.assertIn("boom", body["detail"])
        self.assertEqual(body["instance"], "/throw")

    # --- MapMethods (multi-verb shortcut) ----------------------------------

    def test_600_map_methods_get(self):
        r = requests.get(f"{BASE}/api/echo", headers=ALICE)
        self.assertEqual(r.status_code, 200)
        self.assertIn("GET /api/echo", envelope_text(r.json()))

    def test_610_map_methods_post(self):
        r = requests.post(f"{BASE}/api/echo", headers=ALICE)
        self.assertEqual(r.status_code, 200)
        self.assertIn("POST /api/echo", envelope_text(r.json()))

    def test_620_map_methods_put_not_registered(self):
        # Only GET and POST were registered — PUT must NOT match (404).
        r = requests.put(f"{BASE}/api/echo", headers=ALICE)
        self.assertEqual(r.status_code, 404)

    # --- Auto-validation via TMVCValidationEngine -------------------------

    def test_700_validation_passes(self):
        r = requests.post(f"{BASE}/api/widgets",
                          headers={**ALICE, "Content-Type": "application/json"},
                          json={"name": "Hammer", "qty": 5})
        self.assertEqual(r.status_code, 200, r.text)
        self.assertIn("Hammer", envelope_text(r.json()))

    def test_710_validation_fails_short_name(self):
        # Name min length 3 -> EMVCValidationException -> 400 ProblemDetails.
        r = requests.post(f"{BASE}/api/widgets",
                          headers={**ALICE, "Content-Type": "application/json"},
                          json={"name": "A", "qty": 5})
        self.assertEqual(r.status_code, 400, r.text)
        body = r.json().get("data", r.json())
        self.assertEqual(body["status"], 400)
        self.assertEqual(body["title"], "Validation failed")

    def test_720_validation_fails_negative_qty(self):
        r = requests.post(f"{BASE}/api/widgets",
                          headers={**ALICE, "Content-Type": "application/json"},
                          json={"name": "Widget", "qty": -1})
        self.assertEqual(r.status_code, 400, r.text)
        body = r.json().get("data", r.json())
        self.assertEqual(body["status"], 400)

    # --- Route constraints ------------------------------------------------

    def test_800_route_int_constraint_matches(self):
        r = requests.get(f"{BASE}/api/orders/42", headers=ALICE)
        self.assertEqual(r.status_code, 200)
        self.assertIn('"order":42', envelope_text(r.json()))

    def test_810_route_int_constraint_rejects_non_int(self):
        # 'abc' is not an integer -> route does NOT match -> 404
        r = requests.get(f"{BASE}/api/orders/abc", headers=ALICE)
        self.assertEqual(r.status_code, 404)

    def test_820_route_guid_constraint_matches(self):
        r = requests.get(
            f"{BASE}/api/items/11111111-2222-3333-4444-555555555555",
            headers=ALICE)
        self.assertEqual(r.status_code, 200)

    def test_830_route_guid_constraint_rejects_non_guid(self):
        r = requests.get(f"{BASE}/api/items/not-a-guid", headers=ALICE)
        self.assertEqual(r.status_code, 404)


# ---------------------------------------------------------------------------
def main() -> int:
    with server():
        loader = unittest.TestLoader()
        loader.sortTestMethodsUsing = lambda a, b: (a > b) - (a < b)
        suite = loader.loadTestsFromTestCase(HookChainTests)
        runner = unittest.TextTestRunner(verbosity=2)
        result = runner.run(suite)
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    sys.exit(main())
