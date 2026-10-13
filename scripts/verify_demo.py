#!/usr/bin/env python3
from __future__ import annotations

import csv
import hashlib
import json
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
CAPTURE = ROOT / "docs" / "demo" / "capture.json"


def run(*args):
    return subprocess.check_output(args,cwd=ROOT,text=True).strip()


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def rows(path):
    with path.open(newline="") as handle:
        return list(csv.DictReader(handle))


def main():
    receipt = json.loads(CAPTURE.read_text())
    source = receipt["source_commit"]
    if subprocess.run(["git","merge-base","--is-ancestor",source,"HEAD"],cwd=ROOT).returncode:
        raise SystemExit("Recorded source is not an ancestor of HEAD")
    if run("git","rev-parse",f"{source}^{{tree}}") != receipt["source_tree"]:
        raise SystemExit("Recorded source tree mismatch")
    for item in receipt["runtime_inputs"]:
        if run("git","rev-parse",f"{source}:{item['path']}") != item["git_blob"] or run("git","rev-parse",f"HEAD:{item['path']}") != item["git_blob"]:
            raise SystemExit(f"Runtime blob mismatch: {item['path']}")
    summary = ROOT/receipt["summary_csv"]["path"]
    if digest(summary) != receipt["summary_csv"]["sha256"]:
        raise SystemExit("Alignment summary checksum mismatch")
    expected = [{key:str(value) for key,value in row.items()} for row in receipt["alignment_summary"]]
    if rows(summary) != expected:
        raise SystemExit("Alignment summary content mismatch")
    if "--skip-r" not in sys.argv:
        with tempfile.TemporaryDirectory() as temp:
            actual = Path(temp)/"actual.csv"
            subprocess.run(["Rscript","scripts/write_alignment_receipt.R",str(actual)],cwd=ROOT,check=True)
            if rows(actual) != expected:
                raise SystemExit("Actual R alignment receipt differs from published receipt")
    for artifact in receipt["artifacts"]:
        path = ROOT/artifact["path"]
        if path.stat().st_size != artifact["bytes"] or digest(path) != artifact["sha256"]:
            raise SystemExit(f"Artifact checksum mismatch: {path}")
        probe = json.loads(run("ffprobe","-v","error","-count_frames","-select_streams","v:0","-show_entries","stream=nb_read_frames","-of","json",str(path)))
        if int(probe["streams"][0]["nb_read_frames"]) != artifact["frames"]:
            raise SystemExit(f"Frame-count mismatch: {path}")
    print(f"verified source {source} and {len(receipt['artifacts'])} fully decoded artifacts")


if __name__ == "__main__":
    main()
