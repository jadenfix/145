#!/usr/bin/env python3
"""Render a deterministic visualization of the committed-data lag receipt."""
from __future__ import annotations

import csv
import hashlib
import json
import subprocess
import tempfile
from datetime import datetime
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "docs" / "demo"
W, H = 960, 540
BG, PANEL = "#07131f", "#102638"
MINT, CYAN, TEXT, MUTED, ORANGE = "#67e8c3", "#5ecdf5", "#eef7fb", "#9bb2c2", "#ffb86b"


def font(size: int, bold: bool = False):
    name = "DejaVuSans-Bold.ttf" if bold else "DejaVuSans.ttf"
    return ImageFont.truetype(f"/usr/share/fonts/truetype/dejavu/{name}", size)


def study_dates():
    with (ROOT / "one45.csv").open(newline="") as handle:
        rows = list(csv.DictReader(handle))
    rows = rows[719:][:769][1:]
    return [datetime.strptime(row["date"], "%m/%d/%Y").date() for row in rows]


def summary(dates):
    return [{
        "lag_months": lag,
        "observations": len(dates) - lag,
        "target_start": dates[lag].isoformat(),
        "target_end": dates[-1].isoformat(),
        "predictor_start": dates[0].isoformat(),
        "predictor_end": dates[-1-lag].isoformat(),
    } for lag in range(1, 5)]


def write_summary(rows):
    path = OUT / "alignment-summary.csv"
    with path.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
    return path


def base(title: str, kicker: str):
    image = Image.new("RGB", (W, H), BG)
    draw = ImageDraw.Draw(image)
    draw.rounded_rectangle((28, 24, W-28, H-28), radius=26, fill=PANEL)
    draw.text((58, 52), kicker.upper(), font=font(15, True), fill=MINT)
    draw.text((58, 79), title, font=font(34, True), fill=TEXT)
    draw.line((58, 130, W-58, 130), fill="#26465a", width=2)
    return image, draw


def footer(draw, source):
    draw.text((58, H-61), f"source {source[:12]}  •  committed data  •  provider-free", font=font(14), fill=MUTED)


def scene_intro(source, rows):
    image, draw = base("Calendar-safe regime inputs", "145 · reproducible research")
    draw.text((58, 166), "A real, checked path from archived monthly prices", font=font(24), fill=TEXT)
    draw.text((58, 201), "to past-only Markov-switching design rows.", font=font(24), fill=TEXT)
    for x, label, color in [(58, "768 months", CYAN), (296, "4 lag lanes", MINT), (517, "0 providers", ORANGE)]:
        draw.rounded_rectangle((x, 276, x+190, 355), radius=15, fill="#17364c")
        draw.text((x+18, 293), label, font=font(21, True), fill=color)
        draw.text((x+18, 326), "verified input", font=font(14), fill=MUTED)
    footer(draw, source)
    return image


def scene_problem(source, rows):
    image, draw = base("A row is not always a month", "correctness gap")
    y = 195
    for x, label in [(86, "JAN"), (329, "FEB"), (572, "MAR")]:
        color = MUTED if label == "FEB" else TEXT
        draw.rounded_rectangle((x, y, x+148, y+76), radius=14, outline=color, width=3)
        draw.text((x+39, y+22), label, font=font(24, True), fill=color)
    draw.line((234, y+38, 329, y+38), fill=CYAN, width=4)
    draw.line((477, y+38, 572, y+38), fill=CYAN, width=4)
    draw.line((347, y+12, 458, y+65), fill=ORANGE, width=6)
    draw.line((458, y+12, 347, y+65), fill=ORANGE, width=6)
    draw.text((86, 319), "Old: MAR's prior row could be JAN and still be called lag 1.", font=font(19), fill=TEXT)
    draw.text((86, 355), "Now: fail with the exact missing-month boundary.", font=font(19, True), fill=MINT)
    footer(draw, source)
    return image


def scene_contract(source, rows):
    image, draw = base("The calendar is part of the contract", "validated alignment")
    rules = [("01", "Dates are valid, unique, increasing"), ("02", "Adjacent month indexes differ by exactly one"), ("03", "Predictors precede targets by the named lag")]
    for i, (num, text) in enumerate(rules):
        y = 165+i*84
        draw.rounded_rectangle((61, y, 119, y+58), radius=15, fill="#17455b")
        draw.text((76, y+15), num, font=font(19, True), fill=MINT)
        draw.text((143, y+14), text, font=font(20), fill=TEXT)
    footer(draw, source)
    return image


def scene_receipt(source, rows):
    image, draw = base("Actual committed-data alignment receipt", "1960-01 → 2023-12")
    headers, xs = ["LAG", "ROWS", "TARGET START", "PREDICTOR END"], [68, 170, 304, 559]
    for x, value in zip(xs, headers):
        draw.text((x, 158), value, font=font(14, True), fill=MUTED)
    for i, row in enumerate(rows):
        y = 198+i*52
        draw.rounded_rectangle((58, y-8, W-58, y+35), radius=9, fill="#153247" if i%2==0 else "#112b3e")
        vals = [f"{row['lag_months']}m", str(row['observations']), row['target_start'], row['predictor_end']]
        for x, value in zip(xs, vals):
            draw.text((x, y), value, font=font(17, x==68), fill=MINT if x==68 else TEXT)
    footer(draw, source)
    return image


def scene_flow(source, rows):
    image, draw = base("One flow, verified end to end", "source-pinned demo")
    nodes = [(57, "one45.csv"), (260, "month guard"), (470, "past inputs"), (681, "MSwM fits")]
    for x, label in nodes:
        draw.rounded_rectangle((x, 195, x+168, 267), radius=15, fill="#173b50", outline=MINT, width=2)
        draw.text((x+18, 218), label, font=font(18, True), fill=TEXT)
    for x in [225, 428, 638]:
        draw.line((x, 231, x+32, 231), fill=CYAN, width=4)
        draw.polygon([(x+32, 231), (x+22, 224), (x+22, 238)], fill=CYAN)
    draw.text((58, 322), "R regressions + real past-only core + media integrity", font=font(22, True), fill=MINT)
    draw.text((58, 362), "Educational retrospective re-estimation only.", font=font(19), fill=TEXT)
    draw.text((58, 391), "No trading, forecasting, or performance claim.", font=font(19), fill=ORANGE)
    footer(draw, source)
    return image


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def frames(path):
    raw = subprocess.check_output(["ffprobe", "-v", "error", "-count_frames", "-select_streams", "v:0", "-show_entries", "stream=nb_read_frames", "-of", "json", str(path)])
    return int(json.loads(raw)["streams"][0]["nb_read_frames"])


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    source = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip()
    source_tree = subprocess.check_output(["git", "rev-parse", "HEAD^{tree}"], cwd=ROOT, text=True).strip()
    dates = study_dates()
    rows = summary(dates)
    summary_path = write_summary(rows)
    scenes = [scene_intro(source, rows), scene_problem(source, rows), scene_contract(source, rows), scene_receipt(source, rows), scene_flow(source, rows)]
    poster = OUT / "poster.png"
    scenes[3].save(poster)
    with tempfile.TemporaryDirectory() as temp_name:
        temp = Path(temp_name)
        for i, scene in enumerate(scenes):
            for j in range(26):
                scene.save(temp / f"frame-{i*26+j:04d}.png")
        mp4, gif = OUT/"calendar-safe-lags.mp4", OUT/"calendar-safe-lags.gif"
        subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-framerate", "20", "-i", str(temp/"frame-%04d.png"), "-c:v", "libx264", "-pix_fmt", "yuv420p", "-vf", "scale=960:540", str(mp4)], check=True)
        subprocess.run(["ffmpeg", "-y", "-loglevel", "error", "-framerate", "20", "-i", str(temp/"frame-%04d.png"), "-vf", "scale=800:450:flags=lanczos,split[s0][s1];[s0]palettegen=max_colors=128[p];[s1][p]paletteuse=dither=bayer", str(gif)], check=True)
    runtime = ["one45.csv", "temporal_inputs.R", "run_regime_core.R", "test_temporal.R", "scripts/write_alignment_receipt.R", "scripts/generate_demo.py"]
    manifest = [{"path": path, "git_blob": subprocess.check_output(["git", "rev-parse", f"HEAD:{path}"], cwd=ROOT, text=True).strip()} for path in runtime]
    artifacts = [{"path": str(path.relative_to(ROOT)), "sha256": digest(path), "bytes": path.stat().st_size, "frames": frames(path)} for path in [gif, mp4, poster]]
    capture = {"source_commit": source, "source_tree": source_tree, "scope": "Committed-data educational monthly-alignment replay; no forecasting, trading, or performance claim.", "study_rows": len(dates), "study_start": dates[0].isoformat(), "study_end": dates[-1].isoformat(), "runtime_inputs": manifest, "alignment_summary": rows, "summary_csv": {"path": str(summary_path.relative_to(ROOT)), "sha256": digest(summary_path)}, "artifacts": artifacts}
    (OUT/"capture.json").write_text(json.dumps(capture,indent=2)+"\n")


if __name__ == "__main__":
    main()
