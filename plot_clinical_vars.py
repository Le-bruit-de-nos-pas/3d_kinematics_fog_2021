"""
Plot clinical gait and DBS-related variables.

This script:
- Loads clinical and gait datasets from CSV files
- Cleans and harmonizes condition labels
- Computes summary statistics by condition
- Produces publication-ready plots using seaborn/matplotlib
"""

from pathlib import Path
from typing import List, Optional

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# -------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------

DATA_DIR = Path(".")  # change if your CSVs live elsewhere

CLINICAL_FILE = DATA_DIR / "deter_data_clinical.csv"
GAIT_FILE = DATA_DIR / "gait_data.csv"  # adapt to your actual filename

CONDITION_ORDER = [
    "MED_OFF_STIM_OFF",
    "MED_OFF_STIM_ON",
    "MED_ON_STIM_OFF",
    "MED_ON_STIM_ON_130Hz",
    "MED_ON_STIM_ON_60Hz",
]

PLOT_DIR = DATA_DIR / "plots"
PLOT_DIR.mkdir(parents=True, exist_ok=True)

sns.set_theme(style="white", rc={"axes.facecolor": (0, 0, 0, 0)})


# -------------------------------------------------------------------
# Data loading and preprocessing
# -------------------------------------------------------------------

def load_clinical_data(path: Path = CLINICAL_FILE) -> pd.DataFrame:
    """Load clinical dataset with deterministic UPDRS and SWS data."""
    df = pd.read_csv(path, sep=";")
    # Example: derive mean SWS time per condition
    if "DetSWS_time_s" in df.columns and "condition" in df.columns:
        mean_sws = (
            df.groupby("condition")["DetSWS_time_s"]
            .transform("mean")
            .rename("meanDetSWS_time_s")
        )
        df["meanDetSWS_time_s"] = mean_sws
    return df


def load_gait_data(path: Path = GAIT_FILE) -> pd.DataFrame:
    """Load gait dataset with spatiotemporal variables."""
    df = pd.read_csv(path, sep=";")
    # Standardize condition naming if needed (example based on notebook)
    if "condition" in df.columns:
        df["condition"] = (
            df["condition"]
            .str.replace("Med_", "MED_", case=False)
            .str.replace("Stim_", "STIM_", case=False)
        )
    return df


def ensure_condition_order(df: pd.DataFrame, col: str = "condition") -> pd.DataFrame:
    """Convert condition column to ordered categorical."""
    if col in df.columns:
        df[col] = pd.Categorical(df[col], categories=CONDITION_ORDER, ordered=True)
    return df


# -------------------------------------------------------------------
# Plotting helpers
# -------------------------------------------------------------------

def save_or_show(fig: plt.Figure, filename: Optional[str] = None, dpi: int = 300) -> None:
    """Save figure to file if a filename is given, otherwise show."""
    if filename is not None:
        out_path = PLOT_DIR / filename
        fig.savefig(out_path, bbox_inches="tight", dpi=dpi)
        plt.close(fig)
    else:
        plt.show()


def plot_violin_by_condition(
    df: pd.DataFrame,
    value_col: str,
    title: str,
    ylabel: str,
    filename: Optional[str] = None,
) -> None:
    """Violin + swarm plot of a variable across conditions."""
    df_plot = df.dropna(subset=[value_col, "condition"]).copy()
    df_plot = ensure_condition_order(df_plot)

    fig, ax = plt.subplots(figsize=(8, 5))
    sns.violinplot(
        data=df_plot,
        x="condition",
        y=value_col,
        inner="box",
        cut=0,
        ax=ax,
        color="lightgray",
    )
    sns.swarmplot(
        data=df_plot,
        x="condition",
        y=value_col,
        color="0.2",
        size=4,
        ax=ax,
    )
    ax.set_title(title)
    ax.set_xlabel("")
    ax.set_ylabel(ylabel)
    ax.tick_params(axis="x", rotation=45)

    save_or_show(fig, filename)


def plot_box_by_condition(
    df: pd.DataFrame,
    value_col: str,
    title: str,
    ylabel: str,
    filename: Optional[str] = None,
) -> None:
    """Boxplot of a variable across conditions."""
    df_plot = df.dropna(subset=[value_col, "condition"]).copy()
    df_plot = ensure_condition_order(df_plot)

    fig, ax = plt.subplots(figsize=(8, 5))
    sns.boxplot(
        data=df_plot,
        x="condition",
        y=value_col,
        ax=ax,
        palette="Blues",
    )
    ax.set_title(title)
    ax.set_xlabel("")
    ax.set_ylabel(ylabel)
    ax.tick_params(axis="x", rotation=45)

    save_or_show(fig, filename)


def plot_line_per_patient(
    df: pd.DataFrame,
    value_col: str,
    patient_col: str = "patient_name",
    title: str = "",
    ylabel: str = "",
    filename: Optional[str] = None,
) -> None:
    """Line plot across conditions for each patient (within-subject trajectories)."""
    df_plot = df.dropna(subset=[value_col, "condition", patient_col]).copy()
    df_plot = ensure_condition_order(df_plot)

    fig, ax = plt.subplots(figsize=(8, 5))
    sns.lineplot(
        data=df_plot,
        x="condition",
        y=value_col,
        hue=patient_col,
        marker="o",
        legend=False,
        ax=ax,
        alpha=0.5,
    )
    ax.set_title(title)
    ax.set_xlabel("")
    ax.set_ylabel(ylabel)
    ax.tick_params(axis="x", rotation=45)

    save_or_show(fig, filename)


def plot_pairgrid(
    df: pd.DataFrame,
    vars_to_plot: List[str],
    hue: str = "condition",
    filename: Optional[str] = None,
) -> None:
    """PairGrid for selected variables colored by condition."""
    df_plot = df.dropna(subset=vars_to_plot + [hue]).copy()
    df_plot = ensure_condition_order(df_plot, col=hue)

    g = sns.PairGrid(df_plot, vars=vars_to_plot, hue=hue, corner=True)
    g.map_diag(sns.histplot, kde=True, alpha=0.6)
    g.map_offdiag(sns.scatterplot, s=20, alpha=0.7)
    g.add_legend()

    if filename is not None:
        out_path = PLOT_DIR / filename
        g.fig.savefig(out_path, bbox_inches="tight", dpi=300)
        plt.close(g.fig)
    else:
        plt.show()


# -------------------------------------------------------------------
# Main analysis / plotting
# -------------------------------------------------------------------

def main(save_plots: bool = True) -> None:
    clinical = load_clinical_data()
    gait = load_gait_data()

    # -----------------------------------------------------------------
    # Example 1: Clinical scores by condition
    # -----------------------------------------------------------------
    plot_violin_by_condition(
        clinical,
        value_col="DetUPDRS_III",
        title="Deterministic UPDRS III by Condition",
        ylabel="DetUPDRS III",
        filename="clinical_DetUPDRS_III_violin.png" if save_plots else None,
    )

    plot_box_by_condition(
        clinical,
        value_col="DetAXIAL_score",
        title="Axial Score by Condition",
        ylabel="DetAXIAL Score",
        filename="clinical_DetAXIAL_score_box.png" if save_plots else None,
    )

    # -----------------------------------------------------------------
    # Example 2: SWS (freezing of gait) metrics
    # -----------------------------------------------------------------
    if "DetSWS_time_s" in clinical.columns:
        plot_violin_by_condition(
            clinical,
            value_col="DetSWS_time_s",
            title="SWS Time by Condition",
            ylabel="DetSWS time (s)",
            filename="clinical_SWS_time_violin.png" if save_plots else None,
        )

    # -----------------------------------------------------------------
    # Example 3: Gait variables per patient across conditions
    # -----------------------------------------------------------------
    gait_vars_line = ["Speed", "Cadence"]
    for var in gait_vars_line:
        if var in gait.columns:
            plot_line_per_patient(
                gait,
                value_col=var,
                patient_col="patient_name",
                title=f"{var} across stimulation/medication conditions",
                ylabel=var,
                filename=f"gait_{var}_line_per_patient.png" if save_plots else None,
            )

    # -----------------------------------------------------------------
    # Example 4: Pairwise relationships between gait variables
    # -----------------------------------------------------------------
    gait_vars_pair = [
        v
        for v in [
            "Speed",
            "Cadence",
            "Step_Length_ws",
            "Step_Time_ws",
            "Double_Support_Percent_ws",
        ]
        if v in gait.columns
    ]
    if gait_vars_pair:
        plot_pairgrid(
            gait,
            vars_to_plot=gait_vars_pair,
            hue="condition",
            filename="gait_pairgrid.png" if save_plots else None,
        )


if __name__ == "__main__":
    main(save_plots=True)
