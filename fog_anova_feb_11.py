"""
FOG_ANOVA_Feb_11

Non-parametric repeated-measures analyses of freezing of gait (FOG)
and related clinical variables across DBS/medication conditions.

Main steps:
- Load clinical FOG dataset
- Compute descriptive statistics by condition
- Run Friedman tests for repeated measures
- Run post-hoc pairwise comparisons with p-value adjustment
- Produce summary plots
"""

from pathlib import Path
from typing import List, Tuple

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

import pingouin as pg  # for Friedman + pairwise non-parametric tests

# -------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------

DATA_DIR = Path(".")
FOG_FILE = DATA_DIR / "deter_data_clinical.csv"  # adapt if needed

PLOT_DIR = DATA_DIR / "fog_plots"
PLOT_DIR.mkdir(parents=True, exist_ok=True)

sns.set_theme(style="whitegrid")

CONDITION_ORDER = [
    "MED_OFF_STIM_OFF",
    "MED_OFF_STIM_ON",
    "MED_ON_STIM_OFF",
    "MED_ON_STIM_ON",
    "MED_ON_STIM_ON_60Hz",
]


# -------------------------------------------------------------------
# Utilities
# -------------------------------------------------------------------

def load_fog_data(path: Path = FOG_FILE) -> pd.DataFrame:
    """Load FOG/clinical dataset."""
    df = pd.read_csv(path, sep=";")
    # Ensure condition factor with fixed order
    if "condition" in df.columns:
        df["condition"] = pd.Categorical(
            df["condition"],
            categories=CONDITION_ORDER,
            ordered=True,
        )
    return df


def descriptive_by_condition(df: pd.DataFrame, var: str) -> pd.DataFrame:
    """Return descriptive stats of `var` by condition."""
    desc = (
        df.groupby("condition")[var]
        .describe()
        .loc[:, ["count", "mean", "std", "min", "25%", "50%", "75%", "max"]]
        .reset_index()
    )
    return desc


def run_friedman(
    df: pd.DataFrame,
    subject_col: str,
    within_col: str,
    dv: str,
) -> pd.DataFrame:
    """
    Run Friedman test for repeated measures.

    df should contain one row per observation per condition.
    """
    # pingouin requires wide format for friedman
    wide = df.pivot(index=subject_col, columns=within_col, values=dv)
    result = pg.friedman(data=wide)
    return result


def pairwise_posthoc(
    df: pd.DataFrame,
    subject_col: str,
    within_col: str,
    dv: str,
    padjust: str = "bonf",
) -> pd.DataFrame:
    """
    Pairwise post-hoc non-parametric comparisons (Wilcoxon signed-rank)
    with p-value adjustment.
    """
    res = pg.pairwise_tests(
        data=df,
        dv=dv,
        within=within_col,
        subject=subject_col,
        parametric=False,
        padjust=padjust,
        tail="two-sided",
    )
    # Restrict to useful columns
    res = res.loc[:, ["A", "B", "T", "W", "p-unc", "p-corr", "p-adjust"]]
    return res


def save_figure(fig: plt.Figure, filename: str, dpi: int = 300) -> None:
    out_path = PLOT_DIR / filename
    fig.savefig(out_path, bbox_inches="tight", dpi=dpi)
    plt.close(fig)


# -------------------------------------------------------------------
# Plotting helpers
# -------------------------------------------------------------------

def boxplot_by_condition(
    df: pd.DataFrame,
    var: str,
    ylabel: str,
    title: str,
    filename: str,
) -> None:
    df_plot = df.dropna(subset=[var, "condition"]).copy()
    fig, ax = plt.subplots(figsize=(7, 4))
    sns.boxplot(
        data=df_plot,
        x="condition",
        y=var,
        order=CONDITION_ORDER,
        ax=ax,
        palette="Blues",
    )
    ax.set_xlabel("")
    ax.set_ylabel(ylabel)
    ax.set_title(title)
    ax.tick_params(axis="x", rotation=45)
    save_figure(fig, filename)


def lineplot_per_subject(
    df: pd.DataFrame,
    subject_col: str,
    var: str,
    ylabel: str,
    title: str,
    filename: str,
) -> None:
    df_plot = df.dropna(subset=[var, "condition", subject_col]).copy()
    fig, ax = plt.subplots(figsize=(7, 4))
    sns.lineplot(
        data=df_plot,
        x="condition",
        y=var,
        hue=subject_col,
        marker="o",
        legend=False,
        ax=ax,
        alpha=0.6,
    )
    ax.set_xlabel("")
    ax.set_ylabel(ylabel)
    ax.set_title(title)
    ax.tick_params(axis="x", rotation=45)
    save_figure(fig, filename)


# -------------------------------------------------------------------
# Main analysis
# -------------------------------------------------------------------

def analyse_variable(
    df: pd.DataFrame,
    subject_col: str,
    var: str,
    label: str,
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """
    Convenience wrapper: descriptive stats, Friedman, and pairwise post-hoc.

    Returns:
        (friedman_table, pairwise_table)
    """
    print(f"\n=== {label} ({var}) ===")

    # Descriptives
    desc = descriptive_by_condition(df, var)
    print("\nDescriptives by condition:")
    print(desc.to_string(index=False))

    # Friedman
    friedman_res = run_friedman(
        df=df,
        subject_col=subject_col,
        within_col="condition",
        dv=var,
    )
    print("\nFriedman test:")
    print(friedman_res.to_string(index=False))

    # Post-hoc
    pairwise_res = pairwise_posthoc(
        df=df,
        subject_col=subject_col,
        within_col="condition",
        dv=var,
        padjust="bonf",
    )
    print("\nPairwise Wilcoxon signed-rank (Bonferroni corrected):")
    print(pairwise_res.to_string(index=False))

    return friedman_res, pairwise_res


def main() -> None:
    # -----------------------------------------------------------------
    # Load data
    # -----------------------------------------------------------------
    df = load_fog_data()

    # Subject identifier column name in your notebook:
    # adjust if you used e.g. "patient" or "patient_name"
    subject_col = "patient" if "patient" in df.columns else "patient_name"

    # -----------------------------------------------------------------
    # Variables analysed in the notebook (adapt names as needed)
    # -----------------------------------------------------------------
    variables = [
        ("DetUPDRS_III", "Deterministic UPDRS III"),
        ("DetItem_3_10", "UPDRS item 3.10 (gait)"),
        ("DetItem_3_11", "UPDRS item 3.11 (freezing)"),
        ("DetItem_3_12", "UPDRS item 3.12"),
        ("DetSWS_time_s", "SWS time (s)"),
        ("DetSWS_N_FOG_Events", "Number of FOG events"),
        ("DetHY", "Hoehn & Yahr"),
        ("DetAIMS", "AIMS"),
        ("DetAXIAL_score", "Axial score"),
    ]

    for var, label in variables:
        if var not in df.columns:
            continue

        # Analysis
        analyse_variable(df, subject_col=subject_col, var=var, label=label)

        # Plots
        boxplot_by_condition(
            df=df,
            var=var,
            ylabel=label,
            title=f"{label} by condition",
            filename=f"{var}_boxplot.png",
        )
        lineplot_per_subject(
            df=df,
            subject_col=subject_col,
            var=var,
            ylabel=label,
            title=f"{label} trajectories across conditions",
            filename=f"{var}_per_subject.png",
        )

    # -----------------------------------------------------------------
    # Example: Before vs After surgery summary (if present)
    # -----------------------------------------------------------------
    # If your notebook had "BeforeSurgery"/"AfterSurgery" comparisons:
    if "SurgeryCondition" in df.columns and "DetUPDRS_III" in df.columns:
        df_sx = df[["SurgeryCondition", "DetUPDRS_III"]].dropna()
        sx_desc = (
            df_sx.groupby("SurgeryCondition")["DetUPDRS_III"]
            .describe()
            .loc[:, ["count", "mean", "std", "min", "25%", "50%", "75%", "max"]]
        )
        print("\n=== DetUPDRS_III Before vs After surgery ===")
        print(sx_desc.to_string())

    print("\nAnalysis completed.")


if __name__ == "__main__":
    main()
