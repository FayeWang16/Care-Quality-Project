"""Project: Komodo_generalizability - shareable index-based polypharmacy.
Author: Nemo Zhou. Started: 2026-11-09. Updated: 2026-11-09.

Map unique NDC11 to ATC4 with the NDC -> RxCUI -> RxClass approach in
Fabrício Kury's n2c (MIT; see LICENSE-n2c.txt), upstream revision
f7d99340c20c16a54a0aef2564c79385351180e7. This adaptation uses Python's
standard library, explicit unmapped rows, retryable errors, version-specific
caching, and saved provenance. The default uses ATCPROD relations without an
ingredient-ATC fallback. It does not calculate polypharmacy.
"""

import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import re
import sqlite3
import sys
import time
from datetime import datetime, timezone
from urllib.error import HTTPError, URLError
from urllib.parse import urlencode
from urllib.request import Request, urlopen

BASE = "https://rxnav.nlm.nih.gov/REST/"
UPSTREAM_COMMIT = "f7d99340c20c16a54a0aef2564c79385351180e7"


def utc_now():
    return datetime.now(timezone.utc).isoformat()


def digest(path, algorithm="sha256"):
    value = hashlib.new(algorithm)
    with Path(path).open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            value.update(block)
    return value.hexdigest()


def request_json(path, params=None):
    """Keep below the RxNav per-IP limit; never turn a request error into []."""
    url = BASE + path + (("?" + urlencode(params)) if params else "")
    for attempt in range(4):
        time.sleep(0.12)
        try:
            request = Request(url, headers={"Accept": "application/json", "User-Agent": "Komodo-polypharmacy/1"})
            with urlopen(request, timeout=45) as response:
                data = json.load(response)
            if not isinstance(data, dict) or "errorGroup" in data:
                raise ValueError("Unexpected/error RxNav response")
            return data
        except (HTTPError, URLError, TimeoutError, ValueError, OSError) as error:
            if attempt == 3:
                raise RuntimeError(f"RxNav lookup failed after retries: {path}") from error
            time.sleep(2 ** attempt)


def read_versions():
    rxnorm = request_json("version.json")
    versions = {"rxnorm": rxnorm.get("version"), "api": rxnorm.get("apiVersion")}
    for source in ("ATC", "ATCPROD"):
        versions[source] = request_json(f"rxclass/version/{source}.json").get("relaSourceVersion")
    if any(not value for value in versions.values()):
        raise RuntimeError("RxNav did not return the required terminology versions.")
    return versions


class CachedClient:
    def __init__(self, cache_path, versions, relation_policy="atcprod_only"):
        self.connection = sqlite3.connect(cache_path)
        self.connection.execute("CREATE TABLE IF NOT EXISTS responses (key TEXT PRIMARY KEY, value TEXT, obtained_at TEXT)")
        self.version_key = json.dumps({"versions": versions, "relation_policy": relation_policy}, sort_keys=True)

    def get(self, path, params):
        key = self.version_key + "|" + path + "|" + json.dumps(params, sort_keys=True)
        cached = self.connection.execute("SELECT value FROM responses WHERE key = ?", (key,)).fetchone()
        if cached:
            return json.loads(cached[0])
        value = request_json(path, params)
        self.connection.execute("INSERT INTO responses VALUES (?, ?, ?)", (key, json.dumps(value), utc_now()))
        self.connection.commit()
        return value

    def earliest_lookup(self):
        # Includes reused successes from a previous attempt in this version.
        prefix = self.version_key + "|"
        return self.connection.execute("SELECT MIN(obtained_at) FROM responses WHERE substr(key, 1, ?) = ?", (len(prefix), prefix)).fetchone()[0]

    def close(self):
        self.connection.close()


def map_ndc(ndc, client, relation_policy="atcprod_only"):
    if relation_policy not in ("atcprod_only", "n2c_combined"):
        raise ValueError("Unknown relationship-source policy")
    ids = client.get("rxcui.json", {"idtype": "NDC", "id": ndc}).get("idGroup", {}).get("rxnormId", [])
    if not isinstance(ids, list) or any(not str(item).isdigit() for item in ids):
        raise RuntimeError("Unexpected RxCUI response structure.")
    classes = {}
    for rxcui in ids:
        # relaSource is the documented filter. Product-only mapping deliberately
        # leaves products without an ATCPROD relation unmapped; no ATC fallback.
        source = "ATCPROD" if relation_policy == "atcprod_only" else "ALL"
        result = client.get("rxclass/class/byRxcui.json", {"rxcui": rxcui, "relaSource": source})
        entries = result.get("rxclassDrugInfoList", {}).get("rxclassDrugInfo", [])
        if not isinstance(entries, list):
            raise RuntimeError("Unexpected RxClass response structure.")
        for entry in entries:
            item = entry.get("rxclassMinConceptItem", {})
            atc4 = item.get("classId", "")
            allowed_sources = {"ATCPROD"} if relation_policy == "atcprod_only" else {"ATC", "ATCPROD"}
            if (entry.get("relaSource") in allowed_sources and item.get("classType") == "ATC1-4"
                    and re.fullmatch(r"[A-Z][0-9]{2}[A-Z]{2}", atc4)):
                classes.setdefault(atc4, set()).add(entry.get("relaSource", "unknown"))
    return [(code, "|".join(sorted(sources))) for code, sources in sorted(classes.items())]


def run_mapping(input_path, output_dir, mapping_version, relation_policy="atcprod_only"):
    input_path, output_dir = Path(input_path), Path(output_dir)
    ndcs = sorted(set(input_path.read_text(encoding="utf-8-sig").split()))
    if any(not re.fullmatch(r"[0-9]{11}", ndc) for ndc in ndcs):
        raise ValueError("Input must contain one character NDC11 per line.")
    if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_-]{0,127}", mapping_version):
        raise ValueError("Use a simple mapping_version label of at most 128 characters.")
    if relation_policy not in ("atcprod_only", "n2c_combined"):
        raise ValueError("Unknown relationship-source policy")
    output_dir.mkdir(parents=True, exist_ok=True)
    output, manifest_path = output_dir / "ndc11_atc4.csv", output_dir / "manifest.json"
    temporary, manifest_temp = output_dir / "ndc11_atc4.csv.partial", output_dir / "manifest.json.partial"
    input_sha = digest(input_path)
    def agrees(saved):
        return (saved.get("complete") and saved.get("mapping_version") == mapping_version
                and saved.get("input_sha256") == input_sha
                and saved.get("relation_policy", "n2c_combined") == relation_policy)

    if manifest_path.exists():
        saved = json.loads(manifest_path.read_text(encoding="utf-8"))
        if (agrees(saved) and output.exists()
                and saved.get("crosswalk_sha256") == digest(output)):
            print(f"Reusing frozen crosswalk: {output}", flush=True)
            return saved
        raise RuntimeError("This mapping version already has different/incomplete results. Choose a new mapping_version.")

    # A prepared manifest is written before either final filename is published.
    # After interruption, accept only matching input/version/policy AND hashes.
    if manifest_temp.exists():
        try:
            prepared = json.loads(manifest_temp.read_text(encoding="utf-8"))
        except (ValueError, OSError):
            prepared = None
        candidate = output if output.exists() else temporary
        if prepared is not None:
            if not agrees(prepared) or not candidate.exists() or prepared.get("crosswalk_sha256") != digest(candidate):
                raise RuntimeError("Prepared mapping does not match this input, policy, version, or crosswalk. Choose a new mapping_version.")
            if candidate != output:
                os.replace(candidate, output)
            os.replace(manifest_temp, manifest_path)
            print(f"Recovered completed mapping publication: {output}", flush=True)
            return prepared
    if output.exists():
        raise RuntimeError("Crosswalk lacks valid prepared metadata. Choose a new mapping_version.")
    started = utc_now()
    versions_start = read_versions() if ndcs else {}
    client = CachedClient(output_dir / "rxnav_cache.sqlite", versions_start, relation_policy)
    mapped_count = 0
    try:
        with temporary.open("w", encoding="utf-8", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=["ndc11", "atc4", "mapping_status", "mapping_version", "relation_sources", "relation_policy"])
            writer.writeheader()
            for number, ndc in enumerate(ndcs, 1):
                mappings = map_ndc(ndc, client, relation_policy)
                mapped_count += bool(mappings)
                for atc4, sources in mappings or [("", "")]:
                    writer.writerow({"ndc11": ndc, "atc4": atc4,
                                     "mapping_status": "mapped" if atc4 else "unmapped",
                                      "mapping_version": mapping_version, "relation_sources": sources,
                                      "relation_policy": relation_policy})
                if number % 100 == 0 or number == len(ndcs):
                    print(f"{utc_now()} Mapped {number}/{len(ndcs)} unique codes; {mapped_count} have ATC4", flush=True)
        versions_end = read_versions() if ndcs else {}
        earliest = client.earliest_lookup()
    finally:
        client.close()
    changed = versions_start != versions_end
    manifest = {"complete": True, "mapping_version": mapping_version,
                "relation_policy": relation_policy,
                "mapping_source": "n2c_rxnav", "mapping_level": "atc4_to_atc3",
                "run_started_utc": started, "run_completed_utc": utc_now(),
                "earliest_cached_lookup_utc": earliest,
                "versions_start": versions_start, "versions_end": versions_end,
                "versions_changed_during_run": changed,
                "upstream_url": "https://github.com/fabkury/n2c", "upstream_commit": UPSTREAM_COMMIT,
                "mapper_sha256": digest(__file__), "input_sha256": input_sha,
                "crosswalk_sha256": digest(temporary), "crosswalk_md5": digest(temporary, "md5"),
                "n_unique_ndc11": len(ndcs), "n_mapped_ndc11": mapped_count,
                "cache_file": "rxnav_cache.sqlite"}
    # Prepare provenance durably before publication; a same-command retry can
    # recover an interruption before either rename without repeating lookups.
    with manifest_temp.open("w", encoding="utf-8") as handle:
        handle.write(json.dumps(manifest, indent=2) + "\n")
        handle.flush()
        os.fsync(handle.fileno())
    os.replace(temporary, output)
    os.replace(manifest_temp, manifest_path)
    if changed:
        print("Terminology versions changed during this run; the manifest records both versions.", file=sys.stderr)
    print(f"Completed frozen mapping: {output}", flush=True)
    return manifest


def main():
    parser = argparse.ArgumentParser(description="Create/reuse a frozen n2c-based NDC11-to-ATC4 crosswalk.")
    parser.add_argument("--input", required=True, type=Path)
    parser.add_argument("--output-dir", required=True, type=Path)
    parser.add_argument("--mapping-version", required=True)
    parser.add_argument("--relation-policy", choices=("atcprod_only", "n2c_combined"), default="atcprod_only")
    args = parser.parse_args()
    run_mapping(args.input, args.output_dir, args.mapping_version, args.relation_policy)


if __name__ == "__main__":
    main()
