#!/usr/bin/env python3
"""
Download starred papers listed in slides/syllabus5345.pdf into ./papers

Naming rule (per user request):
- <FirstAuthorFamilyName><Year>.pdf, e.g. Ramey2016.pdf
- If duplicates of the same <FamilyName><Year> exist, append a/b/c... in syllabus order:
    <FamilyName><Year>a.pdf, <FamilyName><Year>b.pdf, ...

This script uses the OpenAlex API to find best open-access PDF links when possible,
and falls back to known direct URLs for a few papers.
"""

from __future__ import annotations

import difflib
import json
import os
import re
import sys
import time
import unicodedata
import urllib.error
import urllib.parse
import urllib.request
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Optional


@dataclass(frozen=True)
class Paper:
    first_author_last: str
    year: int
    title: str
    # Optional manual override (direct PDF URL)
    url: Optional[str] = None


PAPERS_IN_SYLLABUS_ORDER: list[Paper] = [
    Paper("Ramey", 2016, "Macroeconomic shocks and their propagation.", "https://econweb.ucsd.edu/~vramey/research/Shocks_HOM_Ramey.pdf"),
    Paper("Blanchard", 2025, "Convergence? Thoughts about the evolution of mainstream macroeconomics over the last 40 years.", "https://www.nber.org/system/files/chapters/c15152/c15152.pdf"),
    Paper("Newey", 1987, "A Simple, Positive Semi-definite, Heteroskedasticity and Autocorrelation Consistent Covariance Matrix.", "https://www.nber.org/papers/t0055.pdf"),
    Paper("Cogley", 1995, "Effects of the Hodrick-Prescott Filter on Trend and Difference Stationary Time Series: Implications for Business Cycle Research.", "https://public.econ.duke.edu/~boller/Econ.883/cogley_nason_jedc_95.pdf"),
    Paper("Stock", 1999, "Business Cycle Fluctuations in U.S. Macroeconomic Time Series."),
    Paper("Andrews", 2019, "Weak Instruments in Instrumental Variables Regression: Theory and Practice", "https://par.nsf.gov/servlets/purl/10142670"),
    Paper("Blanchard", 1988, "The Dynamic Effects of Aggregate Demand and Supply Disturbances.", "https://www.nber.org/system/files/working_papers/w2737/w2737.pdf"),
    Paper("Bernanke", 1998, "Measuring Monetary Policy."),
    Paper("Christiano", 1999, "Monetary policy shocks: What have we learned and to what end?", "https://www.nber.org/system/files/working_papers/w6400/w6400.pdf"),
    Paper("Bernanke", 2005, "Measuring the effects of monetary policy: A factor-augmented vector autoregressive (FAVAR) approach", "https://www.nber.org/system/files/working_papers/w10220/w10220.pdf"),
    Paper("Gilchrist", 2012, "Credit spreads and business cycle fluctuations."),
    Paper("Jarocinski", 2020, "Deconstructing Monetary Policy Surprises--The Role of Information Shocks."),
    Paper("Romer", 2004, "A New Measure of Monetary Shocks: Derivation and Implications", "https://eml.berkeley.edu/~dromer/papers/AER_September04.pdf"),
    Paper("Nakamura", 2014, "Fiscal stimulus in a monetary union: Evidence from US regions."),
    Paper("Gertler", 2015, "Monetary policy surprises, credit costs, and economic activity."),
    Paper("Ramey", 2018, "Government spending multipliers in good times and in bad: evidence from US historical data."),
    Paper("Romer", 2018, "Why Some Times Are Different: Macroeconomic Policy and the Aftermath of Financial Crises."),
    Paper("Barnichon", 2020, "Identifying Modern Macro Equations with Old Shocks."),
    Paper("Bauer", 2023, "A Reassessment of Monetary Policy Surprises and High-Frequency Identification."),
    Paper("Townsend", 1983, "Forecasting the Forecasts of Others."),
    Paper("Woodford", 2002, "Imperfect Common Knowledge and the Effects of Monetary Policy."),
    Paper("Laubach", 2003, "Measuring the natural rate of interest."),
    Paper("Kose", 2003, "International Business Cycles: World, Region, and Country-Specific Factors."),
    Paper("Gurkaynak", 2005, "Do actions speak louder than words? The response of asset prices to monetary policy actions and statements."),
    Paper("McCracken", 2016, "FRED-MD: A monthly database for macroeconomic research."),
    Paper("Fernald", 2017, "The Disappointing Recovery of Output after 2009."),
    Paper("Swanson", 2021, "Measuring the effects of federal reserve forward guidance and asset purchases on financial markets."),
    Paper("Blanchard", 1980, "The solution of linear difference models under rational expectations."),
    Paper("Kydland", 1982, "Time to Build and Aggregate Fluctuations."),
    Paper("Clarida", 2000, "Monetary Policy Rules and Macroeconomic Stability: Evidence and Some Theory."),
    Paper("Christiano", 2005, "Nominal rigidities and the dynamic effects of a shock to monetary policy."),
    Paper("Smets", 2007, "Shocks and frictions in US business cycles: A Bayesian DSGE approach."),
    Paper("Fernandez-Villaverde", 2007, "ABCs (and Ds) of understanding VARs."),
    Paper("Gertler", 2011, "A model of unconventional monetary policy."),
    Paper("Anzoategui", 2019, "Endogenous technology adoption and R&D as sources of business cycle persistence."),
    Paper("Carroll", 2006, "The method of endogenous gridpoints for solving dynamic stochastic optimization problems."),
    Paper("Gourinchas", 2002, "Consumption over the Life Cycle."),
    Paper("Arellano", 2008, "Default risk and income fluctuations in emerging economies."),
    Paper("Kaplan", 2014, "A model of the consumption response to fiscal stimulus payments."),
    Paper("Krusell", 1998, "Income and wealth heterogeneity in the macroeconomy."),
    Paper("Thomas", 2002, "Is lumpy investment relevant for the business cycle?"),
    Paper("Guerrieri", 2017, "Credit crises, precautionary savings, and the liquidity trap."),
    Paper("Kaplan", 2018, "Monetary policy according to HANK."),
]


UA = "Mozilla/5.0 (compatible; Quantitative-Macro syllabus downloader; +https://openalex.org)"


def ascii_simplify(s: str) -> str:
    s = unicodedata.normalize("NFKD", s)
    s = s.encode("ascii", "ignore").decode("ascii")
    s = re.sub(r"[^A-Za-z0-9]+", "", s)
    return s


def is_pdf_bytes(b: bytes) -> bool:
    return b.startswith(b"%PDF-")


def head_bytes(path: Path, n: int = 8) -> bytes:
    try:
        with path.open("rb") as f:
            return f.read(n)
    except FileNotFoundError:
        return b""


def build_filenames(papers: list[Paper]) -> list[tuple[Paper, str]]:
    bases: list[str] = []
    for p in papers:
        bases.append(f"{ascii_simplify(p.first_author_last)}{p.year}")

    # Count duplicates
    counts: dict[str, int] = {}
    for b in bases:
        counts[b] = counts.get(b, 0) + 1

    # Assign suffixes for duplicates starting from 'a'
    seen: dict[str, int] = {}
    out: list[tuple[Paper, str]] = []
    for p, b in zip(papers, bases, strict=True):
        if counts[b] == 1:
            fname = f"{b}.pdf"
        else:
            idx = seen.get(b, 0)
            seen[b] = idx + 1
            suffix = chr(ord("a") + idx)
            fname = f"{b}{suffix}.pdf"
        out.append((p, fname))
    return out


def openalex_search(title: str) -> list[dict[str, Any]]:
    qs = urllib.parse.urlencode(
        {
            "search": title,
            "per_page": 5,
        }
    )
    url = f"https://api.openalex.org/works?{qs}"
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=30) as resp:
        data = json.loads(resp.read().decode("utf-8"))
    return data.get("results", []) or []


def normalize_title(s: str) -> str:
    s = s.lower()
    s = re.sub(r"[^a-z0-9 ]+", " ", s)
    s = re.sub(r"\\s+", " ", s).strip()
    return s


def best_openalex_match(p: Paper, results: list[dict[str, Any]]) -> Optional[dict[str, Any]]:
    want_title = normalize_title(p.title)
    best: tuple[float, Optional[dict[str, Any]]] = (-1.0, None)

    for r in results:
        title = normalize_title(r.get("title") or "")
        if not title:
            continue
        ratio = difflib.SequenceMatcher(None, want_title, title).ratio()

        score = ratio * 10.0
        year = r.get("publication_year")
        if isinstance(year, int) and year == p.year:
            score += 5.0

        # First author last name match (any authorship)
        want_last = ascii_simplify(p.first_author_last).lower()
        authorships = r.get("authorships") or []
        for a in authorships:
            author = (a or {}).get("author") or {}
            display = author.get("display_name") or ""
            if want_last and want_last in ascii_simplify(display).lower():
                score += 3.0
                break

        if score > best[0]:
            best = (score, r)

    return best[1]


def pick_pdf_url(work: dict[str, Any]) -> Optional[str]:
    # Prefer explicit PDF links if present
    best_oa = work.get("best_oa_location") or {}
    url_for_pdf = best_oa.get("url_for_pdf")
    if isinstance(url_for_pdf, str) and url_for_pdf:
        return url_for_pdf

    url = best_oa.get("url")
    if isinstance(url, str) and url:
        if url.lower().endswith(".pdf"):
            return url
        # Sometimes landing pages still yield a PDF on download; try anyway.
        return url

    oa_url = (work.get("open_access") or {}).get("oa_url")
    if isinstance(oa_url, str) and oa_url:
        return oa_url

    primary = work.get("primary_location") or {}
    primary_pdf = primary.get("url_for_pdf")
    if isinstance(primary_pdf, str) and primary_pdf:
        return primary_pdf

    primary_url = primary.get("url")
    if isinstance(primary_url, str) and primary_url:
        return primary_url

    return None


def download(url: str, out_path: Path, referer: Optional[str] = None) -> tuple[bool, str]:
    tmp = out_path.with_suffix(out_path.suffix + ".part")
    headers = {"User-Agent": UA}
    if referer:
        headers["Referer"] = referer

    req = urllib.request.Request(url, headers=headers)
    try:
        with urllib.request.urlopen(req, timeout=60) as resp:
            data = resp.read()
    except urllib.error.HTTPError as e:
        return False, f"HTTPError {e.code} for {url}"
    except urllib.error.URLError as e:
        return False, f"URLError {e.reason} for {url}"
    except Exception as e:
        return False, f"{type(e).__name__}: {e}"

    if not is_pdf_bytes(data[:8]):
        return False, f"Not a PDF (header={data[:32]!r}) from {url}"

    tmp.write_bytes(data)
    tmp.replace(out_path)
    return True, "ok"


def main() -> int:
    repo_root = Path(__file__).resolve().parents[1]
    papers_dir = repo_root / "papers"
    papers_dir.mkdir(parents=True, exist_ok=True)

    plan = build_filenames(PAPERS_IN_SYLLABUS_ORDER)

    failures: list[dict[str, str]] = []

    for idx, (paper, fname) in enumerate(plan, start=1):
        out_path = papers_dir / fname

        # Skip existing valid PDF
        if out_path.exists() and is_pdf_bytes(head_bytes(out_path, 8)):
            continue

        # If syllabus provided a direct URL, try it first.
        tried: list[str] = []
        if paper.url:
            tried.append(paper.url)
            referer = None
            # NBER sometimes needs a referer
            if "nber.org/system/files" in paper.url:
                # best-effort guess
                m = re.search(r"/(w\\d{4,5}|t\\d{4})/", paper.url)
                if m:
                    referer = f"https://www.nber.org/papers/{m.group(1)}"
            ok, msg = download(paper.url, out_path, referer=referer)
            if ok:
                continue
            failures.append(
                {
                    "file": fname,
                    "first_author": paper.first_author_last,
                    "year": str(paper.year),
                    "title": paper.title,
                    "reason": msg,
                    "tried": paper.url,
                }
            )

        # Try OpenAlex discovery
        try:
            results = openalex_search(paper.title)
        except Exception as e:
            failures.append(
                {
                    "file": fname,
                    "first_author": paper.first_author_last,
                    "year": str(paper.year),
                    "title": paper.title,
                    "reason": f"OpenAlex search failed: {type(e).__name__}: {e}",
                    "tried": ";".join(tried) if tried else "",
                }
            )
            continue

        match = best_openalex_match(paper, results)
        if not match:
            failures.append(
                {
                    "file": fname,
                    "first_author": paper.first_author_last,
                    "year": str(paper.year),
                    "title": paper.title,
                    "reason": "OpenAlex: no match",
                    "tried": ";".join(tried) if tried else "",
                }
            )
            continue

        url = pick_pdf_url(match)
        if not url:
            failures.append(
                {
                    "file": fname,
                    "first_author": paper.first_author_last,
                    "year": str(paper.year),
                    "title": paper.title,
                    "reason": "OpenAlex: no OA URL found",
                    "tried": ";".join(tried) if tried else "",
                }
            )
            continue

        ok, msg = download(url, out_path)
        if not ok:
            failures.append(
                {
                    "file": fname,
                    "first_author": paper.first_author_last,
                    "year": str(paper.year),
                    "title": paper.title,
                    "reason": msg,
                    "tried": ((";".join(tried) + ";") if tried else "") + url,
                }
            )
            continue

        # Be nice to the API / hosts
        time.sleep(0.25)

    if failures:
        fail_path = papers_dir / "FAILED.json"
        fail_path.write_text(json.dumps(failures, indent=2), encoding="utf-8")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())

