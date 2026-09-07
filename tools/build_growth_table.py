#!/usr/bin/env python3
"""Build per-county transition growth tables for all NC counties from official
Census Bureau population estimates and BLS national CPI-U.

Methodology (uniform across all counties):
  * Population: U.S. Census Bureau county PEP estimates.
      - 2016..2020  -> 2010-based vintage (single internally consistent series)
      - 2020..2025  -> 2025-based vintage (supersedes earlier post-2020 vintages)
    The 2020 value is the single bridge between the two vintages (matching the
    Census Bureau's own vintage transition). Transition into year T =
    pop(T)/pop(T-1) - 1.
  * Inflation: BLS national CPI-U annual average (U.S. city average, all items).
    Transition into year T = cpi(T)/cpi(T-1) - 1. Identical for every county.
  * Growth  = inflation + population (decimal fractions).

Reads source CSVs from --census-2010, --census-2025, --cpi. Writes one row per
county per transition year (2017..2025) to --out as CSV:
    fips,county,year,inflation,population,growth
"""
import argparse, csv, os

def load_pops10(path):
    """2010-based vintage PEP county estimates, 2016..2019."""
    d = {}
    with open(path, encoding='latin-1') as f:
        for r in csv.DictReader(f):
            if r['STNAME'] != 'North Carolina':
                continue
            key = ''.join(r[k] for k in ('STATE', 'COUNTY'))
            d[key] = {int(y): int(r['POPESTIMATE%d' % y]) for y in range(2016, 2020)}
    return d

def load_pops25(path):
    """2025-based vintage PEP county estimates, 2020..2025 (includes revised 2020)."""
    d = {}
    with open(path, encoding='latin-1') as f:
        for r in csv.DictReader(f):
            if r['STNAME'] != 'North Carolina':
                continue
            key = ''.join(r[k] for k in ('STATE', 'COUNTY'))
            d[key] = {int(y): int(r['POPESTIMATE%d' % y]) for y in range(2020, 2026)}
    return d

def load_cpi(path):
    d = {}
    with open(path) as f:
        for r in csv.DictReader(f):
            y = int(r['year'])
            d[y] = float(r['value'])
    return d

def pop_for(p10, p25, fips, year):
    """2016..2019 from the 2010-based vintage; 2020..2025 from the 2025-based
    vintage. The revised 2020 estimate is the single bridge value (matching the
    Census Bureau's Vintage 2020+ methodology reset)."""
    if year <= 2019:
        return p10[fips][year]
    return p25[fips][year]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--census-2010', required=True)
    ap.add_argument('--census-2025', required=True)
    ap.add_argument('--cpi', required=True)
    ap.add_argument('--out', required=True)
    ap.add_argument('--fips', action='append', default=None)
    args = ap.parse_args()

    p10 = load_pops10(args.census_2010)
    p25 = load_pops25(args.census_2025)
    cpi = load_cpi(args.cpi)

    want = set(args.fips) if args.fips else (set(p10) & set(p25))
    years = list(range(2017, 2026))
    rows = []
    for fips in sorted(want):
        if fips not in p10 or fips not in p25:
            raise SystemExit('missing fips %s in census data' % fips)
        for y in years:
            p_prev = pop_for(p10, p25, fips, y - 1)
            p_cur = pop_for(p10, p25, fips, y)
            infl = cpi[y] / cpi[y - 1] - 1
            grow = p_cur / p_prev - 1
            rows.append((fips, y, round(infl, 10), round(grow, 10), round(infl + grow, 10)))

    os.makedirs(os.path.dirname(args.out), exist_ok=True)
    with open(args.out, 'w', newline='') as f:
        w = csv.writer(f)
        w.writerow(['fips', 'year', 'inflation', 'population', 'growth'])
        w.writerows(rows)
    print('wrote %d transition rows -> %s' % (len(rows), args.out))

if __name__ == '__main__':
    main()