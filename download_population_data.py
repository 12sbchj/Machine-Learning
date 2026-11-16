

Outputs:
  data/raw/worldbank/extracted/SP.POP.TOTL/API_SP.POP.TOTL_wide.csv  (total population)
  data/derived/worldbank_population_total_country_year.csv           (long, ISO3 x year)
  data/derived/worldbank_population_age_bands_country_year.csv       (age-band counts, ISO3 x year)

Age bands are reconstructed from WDI percentage-of-population indicators multiplied by
the matching sex-specific totals, then summed across sexes.
"""

from __future__ import annotations

import json
import time
import urllib.request

import pandas as pd

from _paths import ROOT, DERIVED, RAW  # noqa: F401

WB_DIR = RAW / "worldbank" / "extracted"

API = "https://api.worldbank.org/v2/country/all/indicator/{ind}?format=json&per_page=20000&date=1995:2023"

# 5-year age bands available in WDI as % of same-sex population, 25+ only (study exposure is 25+).
AGE_BANDS = ["2529", "3034", "3539", "4044", "4549", "5054", "5559",
             "6064", "6569", "7074", "7579", "80UP"]

TOTALS = {"SP.POP.TOTL": "pop_total",
          "SP.POP.TOTL.FE.IN": "pop_female",
          "SP.POP.TOTL.MA.IN": "pop_male"}


def fetch(indicator: str) -> pd.DataFrame:
    url = API.format(ind=indicator)
    for attempt in range(3):
        try:
            with urllib.request.urlopen(url, timeout=120) as resp:
                payload = json.loads(resp.read().decode("utf-8"))
            break
        except Exception as exc:  # noqa: BLE001
            if attempt == 2:
                raise
            print(f"  retry {indicator} after error: {exc}")
            time.sleep(3)
    if len(payload) < 2 or payload[1] is None:
        raise RuntimeError(f"no data returned for {indicator}")
    rows = [
        {"Code": r["countryiso3code"], "Year": int(r["date"]), "value": r["value"]}
        for r in payload[1]
        if r["countryiso3code"] and r["value"] is not None
    ]
    df = pd.DataFrame(rows)
    print(f"  {indicator}: {len(df)} rows, {df['Code'].nunique()} locations")
    return df


def main() -> None:
    DERIVED.mkdir(parents=True, exist_ok=True)

    print("Downloading population totals...")
    totals = {}
    for ind, name in TOTALS.items():
        totals[name] = fetch(ind).rename(columns={"value": name})

    pop = totals["pop_total"]
    for name in ["pop_female", "pop_male"]:
        pop = pop.merge(totals[name], on=["Code", "Year"], how="outer")

    out_dir = WB_DIR / "SP.POP.TOTL"
    out_dir.mkdir(parents=True, exist_ok=True)
    pop.to_csv(out_dir / "API_SP.POP.TOTL_wide.csv", index=False)
    pop.to_csv(DERIVED / "worldbank_population_total_country_year.csv", index=False)
    print(f"Saved totals: {len(pop)} rows -> worldbank_population_total_country_year.csv")

    print("\nDownloading age-band shares (25+)...")
    frames = []
    for band in AGE_BANDS:
        for sex, sex_col in [("FE", "pop_female"), ("MA", "pop_male")]:
            ind = f"SP.POP.{band}.{sex}.5Y"
            try:
                df = fetch(ind).rename(columns={"value": "pct"})
            except Exception as exc:  # noqa: BLE001
                print(f"  SKIP {ind}: {exc}")
                continue
            df["band"] = band
            df["sex_col"] = sex_col
            frames.append(df)

    shares = pd.concat(frames, ignore_index=True)
    shares = shares.merge(pop[["Code", "Year", "pop_female", "pop_male"]],
                          on=["Code", "Year"], how="left")
    # % of same-sex population -> headcount
    shares["count"] = shares.apply(
        lambda r: r["pct"] / 100.0 * r[r["sex_col"]] if pd.notna(r[r["sex_col"]]) else pd.NA,
        axis=1,
    )
    bands = (
        shares.groupby(["Code", "Year", "band"], as_index=False)["count"].sum(min_count=1)
        .pivot(index=["Code", "Year"], columns="band", values="count")
        .reset_index()
    )
    bands.columns.name = None
    bands = bands.rename(columns={b: f"pop_{b}" for b in AGE_BANDS})
    bands.to_csv(DERIVED / "worldbank_population_age_bands_country_year.csv", index=False)
    print(f"Saved age bands: {len(bands)} rows, {bands['Code'].nunique()} locations "
          f"-> worldbank_population_age_bands_country_year.csv")


if __name__ == "__main__":
    main()
