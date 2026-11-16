
Writes to results/tables/revision/ so the primary result tables are left untouched.

  A1  Exposure construction: simple mean vs population-weighted vs WHO-standard-weighted
  A2  Population-weighted regression models (A2c: weighting-intensity spectrum and power)
  A3  Leave-one-country-out for the primary models
  A4  Exclusion scenarios (large exposure swings, sparse outcome coverage)
 """

from __future__ import annotations


import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
from scipy import stats

from _paths import ROOT, PROCESSED, DERIVED, RAW, REV as OUT  # noqa: F401

PANEL = PROCESSED / "country_year_analysis_panel.csv"
POP_TOTAL = DERIVED / "worldbank_population_total_country_year.csv"
POP_BANDS = DERIVED / "worldbank_population_age_bands_country_year.csv"
GBD_TRANSFAT = (RAW / "ihme" / "gbd2021_risk"
                / "IHME_GBD_2021_RISK_1990_2021_TRANSFAT_Y2024M06D05.CSV")

OUTCOMES = {
    "gbd2021_ischemic_heart_disease_deaths_asdr": "ischemic_heart_disease deaths",
    "gbd2021_ischemic_heart_disease_incidence_asdr": "ischemic_heart_disease incidence",
    "gbd2021_ischemic_heart_disease_dalys_disability_adjusted_life_years_asdr": "ischemic_heart_disease DALYs",
    "gbd2021_ischemic_stroke_deaths_asdr": "Ischemic stroke deaths",
    "gbd2021_Diabetes mellitus_deaths_asdr": "Diabetes mellitus",
}
PRIMARY = [
    "gbd2021_ischemic_heart_disease_deaths_asdr",
    "gbd2021_ischemic_heart_disease_incidence_asdr",
    "gbd2021_ischemic_heart_disease_dalys_disability_adjusted_life_years_asdr",
]
BASE_COVARS = ["log_gdp_pc", "urban_pop_pct", "alcohol_liters_pc", "obesity_adults_pct"]

PUBLISHED_LAG5 = {
    "gbd2021_ischemic_heart_disease_deaths_asdr": (2.74, 0.046),
    "gbd2021_ischemic_heart_disease_incidence_asdr": (5.77, 0.012),
    "gbd2021_ischemic_heart_disease_dalys_disability_adjusted_life_years_asdr": (54.00, 0.035),
}

UN_SIDS = [
    "ATG", "BHS", "BHR", "BRB", "BLZ", "CPV", "COM", "CUB", "DMA", "DOM", "FJI", "GRD",
    "GNB", "GUY", "HTI", "JAM", "KIR", "MDV", "MHL", "MUS", "FSM", "NRU", "PLW", "PNG",
    "KNA", "LCA", "VCT", "WSM", "STP", "SYC", "SGP", "SLB", "SUR", "TLS", "TON", "TTO",
    "TUV", "VUT",
]

WHO_STD_WEIGHTS = {
    "25 to 29": 7.93, "30 to 34": 7.61, "35 to 39": 7.15, "40 to 44": 6.59,
    "45 to 49": 6.04, "50 to 54": 5.37, "55 to 59": 4.55, "60 to 64": 3.72,
    "65 to 69": 2.96, "70 to 74": 2.21, "75 to 79": 1.52, "80 to 84": 0.91,
    "85 to 89": 0.44, "90 to 94": 0.15, "95 plus": 0.045,
}


GBD_TO_WB_BAND = {
    "25 to 29": "2529", "30 to 34": "3034", "35 to 39": "3539", "40 to 44": "4044",
    "45 to 49": "4549", "50 to 54": "5054", "55 to 59": "5559", "60 to 64": "6064",
    "65 to 69": "6569", "70 to 74": "7074", "75 to 79": "7579",
    "80 to 84": "80UP", "85 to 89": "80UP", "90 to 94": "80UP", "95 plus": "80UP",
}


def load_panel() -> pd.DataFrame:
    df = pd.read_csv(PANEL).sort_values(["Code", "Year"]).copy()
    df["log_gdp_pc"] = np.log(df["gdp_pc_constant_2015_usd"].clip(lower=1))
    return df


def add_lags(df: pd.DataFrame, exposure_cols: list[str], lags=(0, 3, 5, 10)) -> pd.DataFrame:
    df = df.sort_values(["Code", "Year"]).copy()
    cols = list(dict.fromkeys(exposure_cols + [
        "gdp_pc_constant_2015_usd", "urban_pop_pct", "alcohol_liters_pc",
        "obesity_adults_pct", "smoking_prev_pct", "pop_total",
    ]))
    for lag in lags:
        for col in cols:
            if col not in df.columns:
                continue
            df[f"{col}_lag{lag}"] = df[col] if lag == 0 else df.groupby("Code")[col].shift(lag)
        gdp_lag = f"gdp_pc_constant_2015_usd_lag{lag}"
        if gdp_lag in df.columns:
            df[f"log_gdp_pc_lag{lag}"] = np.log(df[gdp_lag].clip(lower=1))
        for col in exposure_cols:
            lag_col = f"{col}_lag{lag}"
            if lag_col in df.columns:
                df[f"{lag_col}_per01"] = df[lag_col] / 0.1
    return df


def fit_model(df: pd.DataFrame, outcome: str, exposure: str, lag: int,
              extra_covars: list[str] | None = None,
              weights: str | None = None,
              interaction: str | None = None,
              min_countries: int = 50, min_rows: int = 300) -> dict | None:
    """Two-way FE model with country-clustered SE, matching the submitted specification."""
    covars = [f"{c}_lag{lag}" for c in BASE_COVARS] + (extra_covars or [])
    cols = [outcome, "Code", "Year", exposure] + covars
    if weights:
        cols.append(weights)
    if interaction:
        cols.append(interaction)
    cols = [c for c in dict.fromkeys(cols) if c in df.columns]
    model_df = df[(df["Year"] >= 2000) & (df["Year"] <= 2021)][cols].dropna()
    if model_df["Code"].nunique() < min_countries or len(model_df) < min_rows:
        return None

    rhs = [exposure] + [c for c in covars if c in model_df.columns]
    if interaction:
        rhs.append(f"{exposure}:{interaction}")
    rhs += ["C(Year)", "C(Code)"]
    formula = f"{outcome} ~ " + " + ".join(rhs)

    if weights:
        model = smf.wls(formula, data=model_df, weights=model_df[weights]).fit(
            cov_type="cluster", cov_kwds={"groups": model_df["Code"]})
    else:
        model = smf.ols(formula, data=model_df).fit(
            cov_type="cluster", cov_kwds={"groups": model_df["Code"]})

    ci = model.conf_int().loc[exposure]
    res = {
        "coef": model.params[exposure],
        "ci_low": ci[0],
        "ci_high": ci[1],
        "p_value": model.pvalues[exposure],
        "n_rows": int(model.nobs),
        "n_countries": int(model_df["Code"].nunique()),
    }
    if interaction:
        term = f"{exposure}:{interaction}"
        ci_i = model.conf_int().loc[term]
        res |= {
            "interaction_coef": model.params[term],
            "interaction_ci_low": ci_i[0],
            "interaction_ci_high": ci_i[1],
            "interaction_p": model.pvalues[term],
        }
    return res


# A1: exposure construction

def build_exposure_variants() -> pd.DataFrame:
    """Simple mean vs population-weighted vs WHO-standard-weighted trans-fat exposure."""
    cvd = pd.read_csv(RAW / "owid" / "death_rate_from_cardiovascular_disease_age_standardized_ghe.csv")
    name_to_code = cvd[["Entity", "Code"]].drop_duplicates().set_index("Entity")["Code"].to_dict()

    import importlib.util
    spec = importlib.util.spec_from_file_location("bap", ROOT / "scripts" / "build_analysis_panel.py")
    bap = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(bap)

    tf = pd.read_csv(GBD_TRANSFAT, usecols=["location_name", "sex_name", "age_group_name",
                                            "year_id", "unit", "val"])
    tf = tf[(tf["sex_name"] == "Both") & (tf["unit"] == "% energy/day")].copy()
    tf["Entity"] = tf["location_name"].map(
        lambda x: x if x in name_to_code else bap.GBD_LOCATION_NAME_MAP.get(x))
    tf["Code"] = tf["Entity"].map(name_to_code)
    tf = tf[tf["Code"].notna()].copy()
    tf = tf.rename(columns={"year_id": "Year"})

    # (a) simple mean — reproduces the submitted exposure
    simple = (tf.groupby(["Code", "Year"], as_index=False)["val"]
              .mean().rename(columns={"val": "tf_simple_mean"}))

    # (b) WHO-standard-weighted (age-standardised exposure)
    tf["std_w"] = tf["age_group_name"].map(WHO_STD_WEIGHTS)
    std = (tf.assign(num=lambda d: d["val"] * d["std_w"])
             .groupby(["Code", "Year"], as_index=False)
             .apply(lambda g: pd.Series({"tf_who_std": g["num"].sum() / g["std_w"].sum()}),
                    include_groups=False))

    # (c) population-weighted using WDI age bands
    bands = pd.read_csv(POP_BANDS)
    band_long = bands.melt(id_vars=["Code", "Year"], var_name="band", value_name="pop_count")
    band_long["band"] = band_long["band"].str.replace("pop_", "", regex=False)
    tf["band"] = tf["age_group_name"].map(GBD_TO_WB_BAND)
    # average exposure within the shared 80+ band before weighting
    tf_band = (tf.groupby(["Code", "Year", "band"], as_index=False)["val"].mean())
    tf_band = tf_band.merge(band_long, on=["Code", "Year", "band"], how="left")
    tf_band = tf_band.dropna(subset=["pop_count"])
    popw = (tf_band.assign(num=lambda d: d["val"] * d["pop_count"])
            .groupby(["Code", "Year"], as_index=False)
            .apply(lambda g: pd.Series({"tf_pop_weighted": g["num"].sum() / g["pop_count"].sum()}),
                   include_groups=False))

    exposure = simple.merge(std, on=["Code", "Year"], how="left").merge(
        popw, on=["Code", "Year"], how="left")
    exposure.to_csv(DERIVED / "gbd2021_transfat_exposure_variants_country_year.csv", index=False)
    return exposure


def block_a1(exposure: pd.DataFrame) -> pd.DataFrame:
    sub = exposure.dropna(subset=["tf_simple_mean", "tf_pop_weighted", "tf_who_std"])
    rows = [{
        "comparison": "simple mean vs population-weighted",
        "pearson_r": sub["tf_simple_mean"].corr(sub["tf_pop_weighted"]),
        "spearman_r": sub["tf_simple_mean"].corr(sub["tf_pop_weighted"], method="spearman"),
        "mean_difference": (sub["tf_simple_mean"] - sub["tf_pop_weighted"]).mean(),
        "mean_abs_difference": (sub["tf_simple_mean"] - sub["tf_pop_weighted"]).abs().mean(),
        "max_abs_difference": (sub["tf_simple_mean"] - sub["tf_pop_weighted"]).abs().max(),
    }, {
        "comparison": "simple mean vs WHO-standard-weighted",
        "pearson_r": sub["tf_simple_mean"].corr(sub["tf_who_std"]),
        "spearman_r": sub["tf_simple_mean"].corr(sub["tf_who_std"], method="spearman"),
        "mean_difference": (sub["tf_simple_mean"] - sub["tf_who_std"]).mean(),
        "mean_abs_difference": (sub["tf_simple_mean"] - sub["tf_who_std"]).abs().mean(),
        "max_abs_difference": (sub["tf_simple_mean"] - sub["tf_who_std"]).abs().max(),
    }, {
        "comparison": "population-weighted vs WHO-standard-weighted",
        "pearson_r": sub["tf_pop_weighted"].corr(sub["tf_who_std"]),
        "spearman_r": sub["tf_pop_weighted"].corr(sub["tf_who_std"], method="spearman"),
        "mean_difference": (sub["tf_pop_weighted"] - sub["tf_who_std"]).mean(),
        "mean_abs_difference": (sub["tf_pop_weighted"] - sub["tf_who_std"]).abs().mean(),
        "max_abs_difference": (sub["tf_pop_weighted"] - sub["tf_who_std"]).abs().max(),
    }]
    out = pd.DataFrame(rows)
    out["n_country_years"] = len(sub)
    out.to_csv(OUT / "A1_exposure_variant_agreement.csv", index=False)
    return out


def block_a1_models(df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    variants = {
        "simple mean (submitted)": "tf_simple_mean",
        "population-weighted": "tf_pop_weighted",
        "WHO-standard-weighted": "tf_who_std",
    }
    for label, col in variants.items():
        for outcome in PRIMARY:
            for lag in (3, 5):
                res = fit_model(df, outcome, f"{col}_lag{lag}_per01", lag)
                if res:
                    rows.append({"exposure_variant": label, "outcome": OUTCOMES[outcome],
                                 "lag_years": lag, **res})
    out = pd.DataFrame(rows)
    out.to_csv(OUT / "A1_models_by_exposure_variant.csv", index=False)
    return out


# A2: population-weighted regressions

def kish_effective_n(weights: pd.Series) -> float:
    """Effective number of independent units under weighting (Kish 1965)."""
    return (weights.sum() ** 2) / (weights ** 2).sum()


def block_a2(df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for outcome in list(PRIMARY) + ["gbd2023_ischemic_stroke_deaths_asdr",
                                    "gbd2023_cardiovascular_diseases_deaths_asdr"]:
        for lag in (3, 5):
            unw = fit_model(df, outcome, f"gbd_transfat_pct_energy_lag{lag}_per01", lag)
            if unw:
                rows.append({"model": "unweighted (submitted)", "outcome": OUTCOMES[outcome],
                             "lag_years": lag, **unw})
            # Weight by outcome-year (contemporaneous) population: the population the outcome rate
            # applies to. Both A2 and A2c use this same weighting time so the two are consistent.
            wtd = fit_model(df, outcome, f"gbd_transfat_pct_energy_lag{lag}_per01", lag,
                            weights="pop_total")
            if wtd:
                rows.append({"model": "population-weighted", "outcome": OUTCOMES[outcome],
                             "lag_years": lag, **wtd})
    out = pd.DataFrame(rows)
    out.to_csv(OUT / "A2_population_weighted_models.csv", index=False)
    return out


def block_a2c(df: pd.DataFrame) -> pd.DataFrame:
    """Weighting intensity versus information loss.

    Raw population weights concentrate almost all influence in a handful of countries, so the
    weighted model is imprecise by construction rather than informative about the absence of an
    association. Sweeping from no weighting through trimmed and transformed weights to raw
    weights shows how the estimate and the effective sample move together, and the minimum
    detectable effect shows what the weighted model could have found had it been true.
    """
    exposure = "gbd_transfat_pct_energy_lag5_per01"
    covars = [f"{c}_lag5" for c in BASE_COVARS]
    rows = []
    for outcome in PRIMARY:
        d = df[(df["Year"] >= 2000) & (df["Year"] <= 2021)][
            [outcome, "Code", "Year", exposure, "pop_total"] + covars].dropna()
        schemes = {"unweighted": None, "log(population)": np.log(d["pop_total"]),
                   "sqrt(population)": np.sqrt(d["pop_total"])}
        for q in (0.75, 0.90, 0.95, 0.99):
            schemes[f"population, trimmed at {int(q * 100)}th percentile"] = (
                d["pop_total"].clip(upper=d["pop_total"].quantile(q)))
        schemes["population (untrimmed)"] = d["pop_total"]

        formula = f"{outcome} ~ " + " + ".join([exposure] + covars + ["C(Year)", "C(Code)"])
        target = smf.ols(formula, data=d).fit(
            cov_type="cluster", cov_kwds={"groups": d["Code"]}).params[exposure]
        for label, w in schemes.items():
            if w is None:
                m = smf.ols(formula, data=d).fit(cov_type="cluster", cov_kwds={"groups": d["Code"]})
                eff = float(d["Code"].nunique())
            else:
                dw = d.assign(_w=w)
                m = smf.wls(formula, data=dw, weights=dw["_w"]).fit(
                    cov_type="cluster", cov_kwds={"groups": dw["Code"]})
                eff = kish_effective_n(dw.groupby("Code")["_w"].mean())
            ci = m.conf_int().loc[exposure]
            se = m.bse[exposure]
            # Power of this specification to detect an effect the size of the unweighted estimate
            power = float(stats.norm.sf(1.96 - abs(target) / se)
                          + stats.norm.cdf(-1.96 - abs(target) / se))
            rows.append({
                "outcome": OUTCOMES[outcome], "weighting": label,
                "coef": m.params[exposure], "ci_low": ci[0], "ci_high": ci[1],
                "ci_width": ci[1] - ci[0], "p_value": m.pvalues[exposure],
                "effective_n_countries": eff,
                # ~2.8 x SE is the effect a two-sided 5% test detects with 80% power
                "minimum_detectable_effect": 2.8 * se,
                "power_to_detect_unweighted_estimate": power,
                "contains_unweighted_estimate": bool(ci[0] <= target <= ci[1]),
                "n_rows": int(m.nobs), "n_countries": int(d["Code"].nunique()),
            })
    out = pd.DataFrame(rows)
    out.to_csv(OUT / "A2c_weighting_intensity_spectrum.csv", index=False)
    print(out[out["outcome"] == "IHD incidence"][
        ["weighting", "coef", "p_value", "effective_n_countries",
         "minimum_detectable_effect"]].to_string(index=False))
    return out


# A3: leave-one-country-out

def block_a3(df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for outcome in PRIMARY:
        exposure = "gbd_transfat_pct_energy_lag5_per01"
        full = fit_model(df, outcome, exposure, 5)
        if not full:
            continue
        cols = [outcome, "Code", "Year", exposure] + [f"{c}_lag5" for c in BASE_COVARS]
        sample = df[(df["Year"] >= 2000) & (df["Year"] <= 2021)][cols].dropna()
        for code in sorted(sample["Code"].unique()):
            res = fit_model(df[df["Code"] != code], outcome, exposure, 5)
            if res:
                rows.append({
                    "outcome": OUTCOMES[outcome], "excluded_country": code,
                    "coef": res["coef"], "p_value": res["p_value"],
                    "full_sample_coef": full["coef"], "full_sample_p": full["p_value"],
                    "sign_flipped": (res["coef"] > 0) != (full["coef"] > 0),
                    "lost_significance": (res["p_value"] >= 0.05) and (full["p_value"] < 0.05),
                })
        print(f"  A3 {OUTCOMES[outcome]}: {len(sample['Code'].unique())} refits done")
    out = pd.DataFrame(rows)
    out.to_csv(OUT / "A3_leave_one_country_out.csv", index=False)

    summary = (out.groupby("outcome")
               .agg(full_coef=("full_sample_coef", "first"),
                    min_coef=("coef", "min"), max_coef=("coef", "max"),
                    median_coef=("coef", "median"),
                    n_refits=("coef", "size"),
                    n_lost_significance=("lost_significance", "sum"),
                    n_sign_flip=("sign_flipped", "sum"))
               .reset_index())
    most = (out.assign(shift=lambda d: (d["coef"] - d["full_sample_coef"]).abs())
            .sort_values("shift", ascending=False)
            .groupby("outcome").head(3)[["outcome", "excluded_country", "coef", "p_value"]])
    summary.to_csv(OUT / "A3_leave_one_country_out_summary.csv", index=False)
    most.to_csv(OUT / "A3_most_influential_countries.csv", index=False)
    return summary


# A4: exclusion scenarios

def block_a4(df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    exposure_col = "gbd_transfat_pct_energy"

    swing = (df.dropna(subset=[exposure_col])
             .groupby("Code")[exposure_col]
             .agg(lambda s: s.max() - s.min()))
    swing_cut = swing.quantile(0.95)
    big_swing = set(swing[swing > swing_cut].index)

    coverage = df.dropna(subset=["gbd2023_ischemic_heart_disease_deaths_asdr"]).groupby("Code").size()
    sparse = set(coverage[coverage < coverage.quantile(0.10)].index)

    scenarios = {
        "full sample (submitted)": set(),
        "exclude largest exposure swings (top 5%)": big_swing,
        "exclude sparsest outcome coverage (bottom 10%)": sparse,
        "exclude both": big_swing | sparse,
    }
    for label, drop in scenarios.items():
        sub = df[~df["Code"].isin(drop)] if drop else df
        for outcome in PRIMARY:
            res = fit_model(sub, outcome, "gbd_transfat_pct_energy_lag5_per01", 5)
            if res:
                rows.append({"scenario": label, "n_countries_excluded": len(drop),
                             "outcome": OUTCOMES[outcome], "lag_years": 5, **res})
    out = pd.DataFrame(rows)
    out.to_csv(OUT / "A4_exclusion_scenarios.csv", index=False)
    pd.Series(sorted(big_swing)).to_csv(OUT / "A4_excluded_large_swing_countries.csv",
                                        index=False, header=["Code"])
    pd.Series(sorted(sparse)).to_csv(OUT / "A4_excluded_sparse_coverage_countries.csv",
                                     index=False, header=["Code"])
    return out


