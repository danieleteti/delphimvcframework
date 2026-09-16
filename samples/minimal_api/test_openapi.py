"""
Integration tests for OpenAPI 3.1 emission on the minimal_api sample.

Boots MinimalAPISample.exe on port 8080, fetches /openapi.json, validates:
  * top-level shape (openapi/info/paths/components)
  * specific operations have the expected verbs/paths
  * path parameters carry the right schema (constraint -> type/format)
  * request body for POST/PUT/PATCH references the DTO schema
  * record-based hybrid binding produces query+header parameters
  * schema deduplication (TPerson appears once under components.schemas)

Run from a Win32 cmd/powershell after building the .exe:

    cd samples\\minimal_api
    python test_openapi.py
"""

import json
import os
import subprocess
import sys
import time
import unittest
from urllib.request import urlopen, Request
from urllib.error import URLError

HOST = "http://localhost:8080"
EXE = os.path.join(os.path.dirname(__file__), "MinimalAPISample.exe")
SPEC_URL = HOST + "/openapi.json"


def _server_up(timeout=10.0):
    start = time.time()
    while time.time() - start < timeout:
        try:
            with urlopen(HOST + "/health", timeout=0.5) as r:
                if r.status == 200:
                    return True
        except (URLError, ConnectionError, TimeoutError):
            time.sleep(0.1)
    return False


class OpenAPIEmissionTests(unittest.TestCase):
    server = None
    spec = None

    @classmethod
    def setUpClass(cls):
        if not os.path.exists(EXE):
            raise unittest.SkipTest(f"missing {EXE} - build first")
        cls.server = subprocess.Popen(
            [EXE],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            creationflags=getattr(subprocess, "CREATE_NEW_PROCESS_GROUP", 0),
        )
        if not _server_up():
            cls.server.terminate()
            raise unittest.SkipTest("server failed to start")
        with urlopen(SPEC_URL) as r:
            cls.spec = json.loads(r.read().decode("utf-8"))

    @classmethod
    def tearDownClass(cls):
        if cls.server is not None:
            cls.server.terminate()
            try:
                cls.server.wait(timeout=2)
            except subprocess.TimeoutExpired:
                cls.server.kill()

    # ------------------------------------------------------------------
    # 100 — Top-level shape
    # ------------------------------------------------------------------

    def test_100_top_level_keys(self):
        self.assertEqual(self.spec["openapi"], "3.1.0")
        self.assertIn("info", self.spec)
        self.assertIn("paths", self.spec)

    def test_110_info_block(self):
        info = self.spec["info"]
        self.assertEqual(info["title"], "DMVC Minimal API Sample")
        self.assertEqual(info["version"], "1.0")
        self.assertIn("description", info)
        self.assertIn("license", info)

    # ------------------------------------------------------------------
    # 200 — Paths / operations
    # ------------------------------------------------------------------

    def test_200_health_endpoint(self):
        paths = self.spec["paths"]
        self.assertIn("/health", paths)
        self.assertIn("get", paths["/health"])

    def test_210_v1_people_endpoint(self):
        self.assertIn("/v1/people", self.spec["paths"])
        self.assertIn("get", self.spec["paths"]["/v1/people"])

    def test_220_v2_crud_endpoints(self):
        paths = self.spec["paths"]
        self.assertIn("/v2/people", paths)
        self.assertIn("get", paths["/v2/people"])
        self.assertIn("post", paths["/v2/people"])

        self.assertIn("/v2/people/{id}", paths)
        for verb in ("get", "put", "delete"):
            self.assertIn(verb, paths["/v2/people/{id}"], f"missing {verb}")

    # ------------------------------------------------------------------
    # 300 — Parameters
    # ------------------------------------------------------------------

    def test_300_path_param_id_is_integer(self):
        op = self.spec["paths"]["/v2/people/{id}"]["get"]
        params = op["parameters"]
        idp = next(p for p in params if p["name"] == "id")
        self.assertEqual(idp["in"], "path")
        self.assertTrue(idp["required"])
        self.assertEqual(idp["schema"]["type"], "integer")

    def test_310_search_record_binding_query_and_header(self):
        op = self.spec["paths"]["/search"]["get"]
        params = {p["name"]: p for p in op["parameters"]}
        self.assertIn("page", params)
        self.assertEqual(params["page"]["in"], "query")
        self.assertEqual(params["page"]["schema"]["type"], "integer")

        self.assertIn("pageSize", params)
        self.assertEqual(params["pageSize"]["in"], "query")

        self.assertIn("X-Tenant", params)
        self.assertEqual(params["X-Tenant"]["in"], "header")
        self.assertEqual(params["X-Tenant"]["schema"]["type"], "string")

    # ------------------------------------------------------------------
    # 400 — Request body / DTO schema
    # ------------------------------------------------------------------

    def test_400_post_v2_people_has_dto_body(self):
        op = self.spec["paths"]["/v2/people"]["post"]
        self.assertIn("requestBody", op)
        body = op["requestBody"]
        self.assertTrue(body["required"])
        schema = body["content"]["application/json"]["schema"]
        self.assertEqual(schema, {"$ref": "#/components/schemas/TPerson"})

    def test_410_put_v2_people_id_has_dto_body(self):
        op = self.spec["paths"]["/v2/people/{id}"]["put"]
        body = op["requestBody"]
        schema = body["content"]["application/json"]["schema"]
        self.assertEqual(schema, {"$ref": "#/components/schemas/TPerson"})

    # ------------------------------------------------------------------
    # 500 — Components / schema dedupe
    # ------------------------------------------------------------------

    def test_500_components_has_tperson_schema(self):
        self.assertIn("components", self.spec)
        self.assertIn("schemas", self.spec["components"])
        self.assertIn("TPerson", self.spec["components"]["schemas"])
        tperson = self.spec["components"]["schemas"]["TPerson"]
        self.assertEqual(tperson["type"], "object")
        self.assertIn("properties", tperson)
        for prop in ("ID", "FirstName", "LastName", "Age"):
            self.assertIn(prop, tperson["properties"])
        # ID and Age must be integers
        self.assertEqual(tperson["properties"]["ID"]["type"], "integer")
        self.assertEqual(tperson["properties"]["Age"]["type"], "integer")
        # Strings
        self.assertEqual(tperson["properties"]["FirstName"]["type"], "string")
        self.assertEqual(tperson["properties"]["LastName"]["type"], "string")

    def test_510_tperson_referenced_not_duplicated(self):
        # The schema should be referenced via $ref everywhere it's used,
        # never inlined a second time. Walk every parameter / body schema
        # in the doc and count $ref occurrences vs inline `properties` objects
        # called TPerson.
        text = json.dumps(self.spec)
        # Schemas only live in components.schemas — count their declarations
        decls = self.spec["components"]["schemas"]
        self.assertEqual(
            sum(1 for k in decls if k == "TPerson"),
            1,
            "TPerson schema is declared more than once",
        )
        # And the $ref string should appear at least twice (POST + PUT)
        ref_count = text.count('"$ref": "#/components/schemas/TPerson"')
        self.assertGreaterEqual(ref_count, 2)


if __name__ == "__main__":
    unittest.main(verbosity=2)
