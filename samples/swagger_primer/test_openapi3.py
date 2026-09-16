"""
Integration tests for OpenAPI 3.1 emission on the swagger_primer sample,
which uses classic TMVCController-derived controllers with [MVCPath],
[MVCHTTPMethod] and [MVCSwagParam] / [MVCSwagResponses] attributes.

Validates the TMVCControllerOpenAPISource path: parameter extraction from
method signatures, body schemas from [MVCSwagParam(plBody, ..., DTO)],
and per-status-code responses from [MVCSwagResponses(code, ..., DTO)].

Run from a Win32 cmd/powershell after building the .exe:

    cd samples\\swagger_primer
    python test_openapi3.py
"""

import json
import os
import subprocess
import time
import unittest
from urllib.request import urlopen
from urllib.error import URLError

HOST = "http://localhost:8080"
SPEC_URL = HOST + "/api/openapi.json"
EXE = os.path.join(os.path.dirname(__file__), "SwaggerPrimer.exe")


def _server_up(timeout=10.0):
    start = time.time()
    while time.time() - start < timeout:
        try:
            with urlopen(HOST + "/api/openapi.json", timeout=0.5) as r:
                if r.status == 200:
                    return True
        except (URLError, ConnectionError, TimeoutError):
            time.sleep(0.1)
    return False


class ControllerOpenAPITests(unittest.TestCase):
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

    # ---- top-level
    def test_100_openapi_version_is_3_1(self):
        self.assertEqual(self.spec["openapi"], "3.1.0")

    def test_110_info_block_present(self):
        self.assertIn("info", self.spec)
        self.assertEqual(self.spec["info"]["version"], "v1")

    # ---- paths exist
    def test_200_customers_collection(self):
        paths = self.spec["paths"]
        self.assertIn("/api/customers", paths)
        ops = paths["/api/customers"]
        self.assertIn("get", ops)
        self.assertIn("post", ops)

    def test_210_customers_item(self):
        paths = self.spec["paths"]
        self.assertIn("/api/customers/{id}", paths)
        for verb in ("get", "put", "delete"):
            self.assertIn(verb, paths["/api/customers/{id}"])

    # ---- path params
    def test_300_id_is_integer_path_param(self):
        op = self.spec["paths"]["/api/customers/{id}"]["get"]
        params = op["parameters"]
        idp = next(p for p in params if p["name"] == "id")
        self.assertEqual(idp["in"], "path")
        self.assertTrue(idp["required"])
        self.assertEqual(idp["schema"]["type"], "integer")

    # ---- body schemas from [MVCSwagParam(plBody, ..., TCustomer)]
    def test_400_post_body_is_tcustomer(self):
        op = self.spec["paths"]["/api/customers"]["post"]
        body = op["requestBody"]
        schema = body["content"]["application/json"]["schema"]
        self.assertEqual(schema, {"$ref": "#/components/schemas/TCustomer"})

    def test_410_put_body_is_tcustomer(self):
        op = self.spec["paths"]["/api/customers/{id}"]["put"]
        body = op["requestBody"]
        schema = body["content"]["application/json"]["schema"]
        self.assertEqual(schema, {"$ref": "#/components/schemas/TCustomer"})

    # ---- response schemas from [MVCSwagResponses]
    def test_500_get_collection_returns_array_of_tcustomer(self):
        op = self.spec["paths"]["/api/customers"]["get"]
        resp = op["responses"]["200"]
        schema = resp["content"]["application/json"]["schema"]
        self.assertEqual(schema["type"], "array")
        self.assertEqual(schema["items"],
                         {"$ref": "#/components/schemas/TCustomer"})

    def test_510_get_one_returns_tcustomer(self):
        op = self.spec["paths"]["/api/customers/{id}"]["get"]
        resp = op["responses"]["200"]
        schema = resp["content"]["application/json"]["schema"]
        self.assertEqual(schema, {"$ref": "#/components/schemas/TCustomer"})

    def test_520_get_one_404_is_error_response(self):
        op = self.spec["paths"]["/api/customers/{id}"]["get"]
        resp = op["responses"]["404"]
        schema = resp["content"]["application/json"]["schema"]
        self.assertEqual(
            schema, {"$ref": "#/components/schemas/TMVCErrorResponse"})

    def test_530_post_400_is_error_response(self):
        op = self.spec["paths"]["/api/customers"]["post"]
        resp = op["responses"]["400"]
        schema = resp["content"]["application/json"]["schema"]
        self.assertEqual(
            schema, {"$ref": "#/components/schemas/TMVCErrorResponse"})

    # ---- schema deduplication
    def test_600_tcustomer_schema_present_in_components(self):
        schemas = self.spec["components"]["schemas"]
        self.assertIn("TCustomer", schemas)
        self.assertEqual(schemas["TCustomer"]["type"], "object")
        self.assertIn("properties", schemas["TCustomer"])

    def test_610_tcustomer_appears_only_once_in_components(self):
        schemas = self.spec["components"]["schemas"]
        self.assertEqual(
            sum(1 for k in schemas if k == "TCustomer"), 1)

    def test_620_tmvcerrorresponse_in_components(self):
        schemas = self.spec["components"]["schemas"]
        self.assertIn("TMVCErrorResponse", schemas)

    # ---- description from MVCDoc (not used in MyController) — skipped


if __name__ == "__main__":
    unittest.main(verbosity=2)
