from __future__ import annotations


import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import statsmodels.formula.api as smf


from _paths import ROOT, PROCESSED, TABLES, FIGURES  

PANEL = PROCESSED / "country_year_analysis_panel.csv"

OUTCOMES = {
    "gbd2021_ischemic_heart_disease_deaths_asdr": "ischemic_heart_disease deaths",
    "gbd2021_ischemic_heart_disease_incidence_asdr": "ischemic_heart_disease incidence",
    "gbd2021_ischemic_heart_disease_dalys_disability_adjusted_life_years_asdr": "ischemic_heart_disease DALYs",
    "gbd2021_ischemic_stroke_deaths_asdr": "Ischemic stroke deaths",
    "gbd2021_Diabetes mellitus_deaths_asdr": "Diabetes mellitus",
}

BASE_COVARS = [
    "log_gdp_pc",
    "urban_pop_pct",
    "alcohol_liters_pc",
    "obesity_adults_pct",
]


def setup() -> None:
    TABLES.mkdir(parents=True, exist_ok=True)
    FIGURES.mkdir(parents=True, exist_ok=True)


def load_panel() -> pd.DataFrame:
    df = pd.read_csv(PANEL)
    df = df.sort_values(["Code", "Year"]).copy()
    df["log_gdp_pc"] = np.log(df["gdp_pc_constant_2015_usd"].clip(lower=1))
    lag_sources = ["gbd_transfat_pct_energy", "tfa_policy_yes"] + [
        "gdp_pc_constant_2015_usd",
        "urban_pop_pct",
        "alcohol_liters_pc",
        "obesity_adults_pct",
    ]
    for lag in [0, 3, 5, 10]:
        for col in lag_sources:
            if col in df.columns:
                df[f"{col}_lag{lag}"] = df.groupby("Code")[col].shift(lag) if lag else df[col]
        if f"gdp_pc_constant_2015_usd_lag{lag}" in df.columns:
            df[f"log_gdp_pc_lag{lag}"] = np.log(
                df[f"gdp_pc_constant_2015_usd_lag{lag}"].clip(lower=1)
            )
        if f"gbd_transfat_pct_energy_lag{lag}" in df.columns:
            df[f"gbd_transfat_pct_energy_lag{lag}_per01"] = df[f"gbd_transfat_pct_energy_lag{lag}"] / 0.1
    return df


def fit_models(df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for outcome, label in OUTCOMES.items():
        if outcome not in df.columns:
            continue
        for lag in [0, 3, 5, 10]:
            exposure = f"gbd_transfat_pct_energy_lag{lag}_per01"
            covars = [
                f"log_gdp_pc_lag{lag}",
                f"urban_pop_pct_lag{lag}",
                f"alcohol_liters_pc_lag{lag}",
                f"obesity_adults_pct_lag{lag}",
            ]
            cols = [outcome, "Code", "Year", exposure] + covars
            model_df = df[(df["Year"] >= 2000) & (df["Year"] <= 2021)][cols].dropna()
            if model_df["Code"].nunique() < 50 or len(model_df) < 300:
                continue
            for country_fe in [False, True]:
                rhs = [exposure] + covars + ["C(Year)"]
                if country_fe:
                    rhs.append("C(Code)")
                formula = f"{outcome} ~ " + " + ".join(rhs)
                model = smf.ols(formula, data=model_df).fit(
                    cov_type="cluster", cov_kwds={"groups": model_df["Code"]}
                )
                term = exposure
                ci = model.conf_int().loc[term]
                rows.append(
                    {
                        "outcome": outcome,
                        "outcome_label": label,
                        "lag_years": lag,
                        "country_fe": country_fe,
                        "n_rows": int(model.nobs),
                        "n_countries": int(model_df["Code"].nunique()),
                        "coef_per_0_1pct_energy": model.params[term],
                        "se_cluster": model.bse[term],
                        "p_value": model.pvalues[term],
                        "ci_low": ci[0],
                        "ci_high": ci[1],
                        "r_squared": model.rsquared,
                    }
                )
    out = pd.DataFrame(rows)
    out.to_csv(TABLES / "gbd2023_outcome_transfat_lag_panel_models.csv", index=False)
    return out


def fit_policy_models(df: pd.DataFrame) -> pd.DataFrame:
    rows = []
    for outcome, label in OUTCOMES.items():
        if outcome not in df.columns:
            continue
        for lag in [3, 5, 10]:
            exposure = f"tfa_policy_yes_lag{lag}"
            covars = [
                f"log_gdp_pc_lag{lag}",
                f"urban_pop_pct_lag{lag}",
                f"alcohol_liters_pc_lag{lag}",
                f"obesity_adults_pct_lag{lag}",
            ]
            cols = [outcome, "Code", "Year", exposure] + covars
            model_df = df[(df["Year"] >= 2004) & (df["Year"] <= 2021)][cols].dropna()
            if model_df["Code"].nunique() < 50 or len(model_df) < 300:
                continue
            rhs = [exposure] + covars + ["C(Year)", "C(Code)"]
            model = smf.ols(f"{outcome} ~ " + " + ".join(rhs), data=model_df).fit(
                cov_type="cluster", cov_kwds={"groups": model_df["Code"]}
            )
            ci = model.conf_int().loc[exposure]
            rows.append(
                {
                    "outcome": outcome,
                    "outcome_label": label,
                    "lag_years": lag,
                    "country_fe": True,
                    "n_rows": int(model.nobs),
                    "n_countries": int(model_df["Code"].nunique()),
                    "coef_policy_yes": model.params[exposure],
                    "se_cluster": model.bse[exposure],
                    "p_value": model.pvalues[exposure],
                    "ci_low": ci[0],
                    "ci_high": ci[1],
                    "r_squared": model.rsquared,
                }
            )
    out = pd.DataFrame(rows)
    out.to_csv(TABLES / "gbd2023_outcome_tfa_policy_lag_panel_models.csv", index=False)
    return out


def plot_forest(results: pd.DataFrame) -> None:
    if results.empty:
        return
    plot_df = results[(results["country_fe"] == True) & (results["lag_years"] == 5)].copy()
    plot_df = plot_df[plot_df["outcome"].isin(OUTCOMES)]
    if plot_df.empty:
        return
    plot_df = plot_df.sort_values("coef_per_0_1pct_energy")
    y = np.arange(len(plot_df))
    fig, ax = plt.subplots(figsize=(8, 5))
    ax.errorbar(
        plot_df["coef_per_0_1pct_energy"],
        y,
        xerr=[
            plot_df["coef_per_0_1pct_energy"] - plot_df["ci_low"],
            plot_df["ci_high"] - plot_df["coef_per_0_1pct_energy"],
        ],
        fmt="o",
        color="#314f6b",
        ecolor="#6b8195",
        capsize=3,
    )
    ax.axvline(0, color="black", linewidth=0.8)
    ax.set_yticks(y)
    ax.set_yticklabels(plot_df["outcome_label"])
    ax.set_xlabel("Change in ASDR per 0.1% energy/day higher trans fat, 5-year lag")
    ax.set_title("Country fixed-effect panel models")
    fig.tight_layout()
    fig.savefig(FIGURES / "forest_gbd2023_outcomes_transfat_lag5_country_fe.png", dpi=180)
    plt.close(fig)


def main() -> None:
    setup()
    df = load_panel()
    results = fit_models(df)
    fit_policy_models(df)
    plot_forest(results)


if __name__ == "__main__":
    main()
