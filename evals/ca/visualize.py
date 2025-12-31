"""Visualization tools for CA (Computational Accuracy) evaluation results."""

import json
from pathlib import Path
from typing import Dict, Any, Optional, List

try:
    import matplotlib.pyplot as plt
    import matplotlib
    import numpy as np
    matplotlib.use("Agg")  # Use non-interactive backend
    HAS_MATPLOTLIB = True
except ImportError:
    HAS_MATPLOTLIB = False
    try:
        import numpy as np
    except ImportError:
        np = None

# CA color palette (soft pastel tones for readability)
COLOR_CA = "#8bc34a"  # CA score (soft green)
COLOR_PERFECT = "#aed581"  # Perfect matches
COLOR_PARTIAL = "#ffcc80"  # Partial matches
COLOR_FAILURE = "#ffab91"  # Failures
COLOR_EXACT = "#81c784"  # Exact matches
COLOR_NORMALIZED = "#ffe082"  # Normalized matches
COLOR_COMPARISON_A = "#42a5f5"  # Deterministic / baseline (blue)
COLOR_COMPARISON_B = "#4caf50"  # LLM-controlled / variant (green)


def load_results(json_file: Path) -> Dict[str, Any]:
    """Load CA results from JSON file."""
    with open(json_file, "r", encoding="utf-8") as f:
        return json.load(f)


def plot_ca_scores(
    results: Dict[str, Any],
    output_file: Optional[Path] = None,
    show_match_types: bool = False,
):
    """
    Create bar chart visualization of CA scores.
    
    Args:
        results: Results dictionary from CA evaluation
        output_file: Optional path to save the plot
        show_match_types: If True, show breakdown by match type (exact, normalized, partial)
    """
    if not HAS_MATPLOTLIB:
        print("Error: matplotlib is required for visualization.")
        print("Install with: pip install matplotlib")
        return
    
    results_dict = results.get("results", {})
    if not results_dict:
        print("No results to visualize.")
        return
    
    # Extract file names and scores
    files = []
    ca_scores = []
    exact_matches = []
    normalized_matches = []
    returncode_matches = []
    
    for file_key, result in results_dict.items():
        if "ca_score" in result and "error" not in result:
            files.append(file_key)
            ca_scores.append(result["ca_score"])
            exact_matches.append(result.get("exact_match", False))
            normalized_matches.append(result.get("normalized_match", False))
            returncode_matches.append(result.get("returncode_match", False))
    
    if not files:
        print("No successful evaluations to visualize.")
        return
    
    # Create figure
    fig, ax = plt.subplots(figsize=(14, 6))
    
    x_pos = range(len(files))
    width = 0.25 if show_match_types else 0.6
    
    # Color bars based on score
    colors = []
    for score in ca_scores:
        if score == 1.0:
            colors.append(COLOR_PERFECT)
        elif score >= 0.5:
            colors.append(COLOR_PARTIAL)
        else:
            colors.append(COLOR_FAILURE)
    
    if show_match_types:
        # Show exact, normalized, and returncode matches as separate bars
        exact_scores = [1.0 if m else 0.0 for m in exact_matches]
        normalized_scores = [0.9 if m and not exact_matches[i] else 0.0 for i, m in enumerate(normalized_matches)]
        returncode_scores = [0.5 if m else 0.0 for m in returncode_matches]
        
        ax.bar([x - width for x in x_pos], exact_scores, width, label="Exact Match", alpha=0.8, color=COLOR_EXACT)
        ax.bar(x_pos, normalized_scores, width, label="Normalized Match", alpha=0.8, color=COLOR_NORMALIZED)
        ax.bar([x + width for x in x_pos], returncode_scores, width, label="Return Code Match", alpha=0.8, color=COLOR_PARTIAL)
        ax.bar([x + 2*width for x in x_pos], ca_scores, width, label="CA Score", alpha=0.9, linewidth=1.5, color=COLOR_CA)
    else:
        ax.bar(x_pos, ca_scores, width, alpha=0.8, color=colors, edgecolor='black', linewidth=0.5)
    
    # Customize plot
    ax.set_xlabel("File", fontsize=12, fontweight='bold')
    ax.set_ylabel("CA Score", fontsize=12, fontweight='bold')
    ax.set_title("Computational Accuracy (CA) Scores by File", fontsize=14, fontweight='bold')
    ax.set_xticks(x_pos)
    ax.set_xticklabels(files, rotation=45, ha='right', fontsize=8)
    ax.set_ylim(0, 1.1)
    ax.axhline(y=1.0, color='green', linestyle='--', alpha=0.3, label='Perfect Match')
    ax.axhline(y=0.5, color='orange', linestyle='--', alpha=0.3, label='Partial Match')
    ax.grid(axis='y', alpha=0.3, linestyle='--')
    
    if show_match_types:
        ax.legend(loc='upper right', fontsize=9)
    else:
        # Add legend for colors
        from matplotlib.patches import Patch
        legend_elements = [
            Patch(facecolor=COLOR_PERFECT, label='Perfect (1.0)'),
            Patch(facecolor=COLOR_PARTIAL, label='Partial (0.5-0.9)'),
            Patch(facecolor=COLOR_FAILURE, label='Failure (0.0)'),
        ]
        ax.legend(handles=legend_elements, loc='upper right', fontsize=9)
    
    plt.tight_layout()
    
    if output_file:
        output_file.parent.mkdir(parents=True, exist_ok=True)
        plt.savefig(output_file, dpi=300, bbox_inches='tight')
        print(f"Plot saved to {output_file}")
    else:
        plt.show()
    
    plt.close()


def plot_ca_distribution(
    results: Dict[str, Any],
    output_file: Optional[Path] = None,
):
    """
    Create histogram showing distribution of CA scores.
    
    Args:
        results: Results dictionary from CA evaluation
        output_file: Optional path to save the plot
    """
    if not HAS_MATPLOTLIB:
        print("Error: matplotlib is required for visualization.")
        print("Install with: pip install matplotlib")
        return
    
    results_dict = results.get("results", {})
    if not results_dict:
        print("No results to visualize.")
        return
    
    # Extract CA scores
    ca_scores = []
    for result in results_dict.values():
        if "ca_score" in result and "error" not in result:
            ca_scores.append(result["ca_score"])
    
    if not ca_scores:
        print("No successful evaluations to visualize.")
        return
    
    # Create figure
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Create histogram
    bins = [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1]
    n, bins, patches = ax.hist(ca_scores, bins=bins, edgecolor='black', alpha=0.7)
    
    # Color bars based on score ranges
    for i, patch in enumerate(patches):
        if bins[i] >= 1.0:
            patch.set_facecolor(COLOR_PERFECT)
        elif bins[i] >= 0.5:
            patch.set_facecolor(COLOR_PARTIAL)
        else:
            patch.set_facecolor(COLOR_FAILURE)
    
    # Add summary statistics
    mean_score = np.mean(ca_scores) if np else sum(ca_scores) / len(ca_scores)
    median_score = np.median(ca_scores) if np else sorted(ca_scores)[len(ca_scores) // 2]
    
    ax.axvline(mean_score, color='red', linestyle='--', linewidth=2, label=f'Mean: {mean_score:.3f}')
    ax.axvline(median_score, color='blue', linestyle='--', linewidth=2, label=f'Median: {median_score:.3f}')
    
    # Customize plot
    ax.set_xlabel("CA Score", fontsize=12, fontweight='bold')
    ax.set_ylabel("Number of Files", fontsize=12, fontweight='bold')
    ax.set_title("Distribution of Computational Accuracy (CA) Scores", fontsize=14, fontweight='bold')
    ax.set_xlim(0, 1.1)
    ax.grid(axis='y', alpha=0.3, linestyle='--')
    ax.legend(loc='upper right', fontsize=10)
    
    # Add text with statistics
    perfect_count = sum(1 for s in ca_scores if s == 1.0)
    partial_count = sum(1 for s in ca_scores if 0 < s < 1.0)
    failure_count = sum(1 for s in ca_scores if s == 0.0)
    
    stats_text = f"Total: {len(ca_scores)}\nPerfect: {perfect_count}\nPartial: {partial_count}\nFailure: {failure_count}"
    ax.text(0.02, 0.98, stats_text, transform=ax.transAxes, fontsize=10,
            verticalalignment='top', bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
    
    plt.tight_layout()
    
    if output_file:
        output_file.parent.mkdir(parents=True, exist_ok=True)
        plt.savefig(output_file, dpi=300, bbox_inches='tight')
        print(f"Plot saved to {output_file}")
    else:
        plt.show()
    
    plt.close()


def plot_match_type_breakdown(
    results: Dict[str, Any],
    output_file: Optional[Path] = None,
):
    """
    Create pie chart showing breakdown of match types.
    
    Args:
        results: Results dictionary from CA evaluation
        output_file: Optional path to save the plot
    """
    if not HAS_MATPLOTLIB:
        print("Error: matplotlib is required for visualization.")
        print("Install with: pip install matplotlib")
        return
    
    results_dict = results.get("results", {})
    if not results_dict:
        print("No results to visualize.")
        return
    
    # Count match types
    perfect = 0
    partial = 0
    failure = 0
    
    for result in results_dict.values():
        if "error" in result:
            failure += 1
        elif "ca_score" in result:
            score = result["ca_score"]
            if score == 1.0:
                perfect += 1
            elif score > 0.0:
                partial += 1
            else:
                failure += 1
    
    if perfect + partial + failure == 0:
        print("No results to visualize.")
        return
    
    # Create figure
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))
    
    # Pie chart for match types
    sizes = [perfect, partial, failure]
    labels = ['Perfect (1.0)', 'Partial (0.5-0.9)', 'Failure (0.0)']
    colors_pie = [COLOR_PERFECT, COLOR_PARTIAL, COLOR_FAILURE]
    explode = (0.05, 0.05, 0.05) if failure > 0 else (0.05, 0.05, 0)
    
    ax1.pie(sizes, explode=explode, labels=labels, colors=colors_pie, autopct='%1.1f%%',
            shadow=True, startangle=90, textprops={'fontsize': 11, 'fontweight': 'bold'})
    ax1.set_title("CA Score Distribution", fontsize=14, fontweight='bold')
    
    # Bar chart for exact vs normalized matches
    exact_count = sum(1 for r in results_dict.values() if r.get("exact_match", False))
    normalized_only = sum(1 for r in results_dict.values() 
                          if r.get("normalized_match", False) and not r.get("exact_match", False))
    no_match = len(results_dict) - exact_count - normalized_only
    
    categories = ['Exact Match', 'Normalized Only', 'No Match']
    counts = [exact_count, normalized_only, no_match]
    colors_bar = [COLOR_EXACT, COLOR_NORMALIZED, COLOR_FAILURE]
    
    bars = ax2.bar(categories, counts, color=colors_bar, alpha=0.8, edgecolor='black', linewidth=1.5)
    ax2.set_ylabel("Number of Files", fontsize=12, fontweight='bold')
    ax2.set_title("Output Match Type Breakdown", fontsize=14, fontweight='bold')
    ax2.grid(axis='y', alpha=0.3, linestyle='--')
    
    # Add value labels on bars
    for bar in bars:
        height = bar.get_height()
        ax2.text(bar.get_x() + bar.get_width()/2., height,
                f'{int(height)}',
                ha='center', va='bottom', fontsize=11, fontweight='bold')
    
    plt.tight_layout()
    
    if output_file:
        output_file.parent.mkdir(parents=True, exist_ok=True)
        plt.savefig(output_file, dpi=300, bbox_inches='tight')
        print(f"Plot saved to {output_file}")
    else:
        plt.show()
    
    plt.close()


def plot_ca_vs_codebleu(
    ca_results: Dict[str, Any],
    codebleu_results: Dict[str, Any],
    output_file: Optional[Path] = None,
):
    """
    Create scatter plot comparing CA scores vs CodeBLEU scores.
    
    Args:
        ca_results: CA results dictionary
        codebleu_results: CodeBLEU results dictionary
        output_file: Optional path to save the plot
    """
    if not HAS_MATPLOTLIB:
        print("Error: matplotlib is required for visualization.")
        print("Install with: pip install matplotlib")
        return
    
    ca_dict = ca_results.get("results", {})
    codebleu_dict = codebleu_results.get("results", {})
    
    if not ca_dict or not codebleu_dict:
        print("No results to visualize.")
        return
    
    # Match files and extract scores
    ca_scores = []
    codebleu_scores = []
    files = []
    
    for file_key in ca_dict.keys():
        if file_key in codebleu_dict:
            ca_result = ca_dict[file_key]
            codebleu_result = codebleu_dict[file_key]
            
            if "ca_score" in ca_result and "error" not in ca_result:
                if "codebleu" in codebleu_result and "error" not in codebleu_result:
                    ca_scores.append(ca_result["ca_score"])
                    codebleu_scores.append(codebleu_result["codebleu"])
                    files.append(file_key)
    
    if not ca_scores:
        print("No matching results to visualize.")
        return
    
    # Create figure
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Create scatter plot
    scatter = ax.scatter(codebleu_scores, ca_scores, alpha=0.6, s=100, c=range(len(files)), 
                        cmap='viridis', edgecolors='black', linewidth=1)
    
    # Add diagonal line (perfect correlation)
    ax.plot([0, 1], [0, 1], 'r--', alpha=0.3, label='Perfect Correlation')
    
    # Customize plot
    ax.set_xlabel("CodeBLEU Score", fontsize=12, fontweight='bold')
    ax.set_ylabel("CA Score", fontsize=12, fontweight='bold')
    ax.set_title("CA Score vs CodeBLEU Score Comparison", fontsize=14, fontweight='bold')
    ax.set_xlim(0, 1.1)
    ax.set_ylim(0, 1.1)
    ax.grid(alpha=0.3, linestyle='--')
    ax.legend(loc='lower right', fontsize=10)
    
    # Add correlation coefficient
    if np:
        correlation = np.corrcoef(codebleu_scores, ca_scores)[0, 1]
        ax.text(0.05, 0.95, f'Correlation: {correlation:.3f}', transform=ax.transAxes,
                fontsize=11, verticalalignment='top',
                bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
    
    # Add colorbar
    cbar = plt.colorbar(scatter, ax=ax)
    cbar.set_label('File Index', fontsize=10)
    
    plt.tight_layout()
    
    if output_file:
        output_file.parent.mkdir(parents=True, exist_ok=True)
        plt.savefig(output_file, dpi=300, bbox_inches='tight')
        print(f"Plot saved to {output_file}")
    else:
        plt.show()
    
    plt.close()


def _extract_summary(results: Dict[str, Any]) -> Dict[str, float]:
    """Return summary metrics with sensible defaults."""
    summary = results.get("summary", {})
    results_dict = results.get("results", {})

    def _rate(key: str) -> float:
        value = summary.get(key)
        if value is not None:
            return float(value)
        if not results_dict:
            return 0.0
        if key == "exact_match_rate":
            total = len(results_dict)
            matches = sum(1 for r in results_dict.values() if r.get("exact_match"))
            return matches / total if total else 0.0
        if key == "normalized_match_rate":
            total = len(results_dict)
            matches = sum(1 for r in results_dict.values() if r.get("normalized_match"))
            return matches / total if total else 0.0
        if key == "returncode_match_rate":
            total = len(results_dict)
            matches = sum(1 for r in results_dict.values() if r.get("returncode_match"))
            return matches / total if total else 0.0
        return 0.0

    def _count(key: str, predicate) -> int:
        value = summary.get(key)
        if value is not None:
            return int(value)
        return sum(1 for r in results_dict.values() if predicate(r))

    return {
        "mean_ca_score": float(summary.get("mean_ca_score", 0.0)),
        "exact_match_rate": _rate("exact_match_rate"),
        "normalized_match_rate": _rate("normalized_match_rate"),
        "returncode_match_rate": _rate("returncode_match_rate"),
        "perfect_matches": _count("perfect_matches", lambda r: r.get("ca_score") == 1.0),
        "partial_matches": _count(
            "partial_matches",
            lambda r: r.get("ca_score") not in (None, 0.0, 1.0),
        ),
        "failures": _count("failures", lambda r: r.get("ca_score") == 0.0 or "error" in r),
        "num_files": int(summary.get("num_files", len(results_dict))),
    }


def plot_summary_comparison(
    results_a: Dict[str, Any],
    results_b: Dict[str, Any],
    labels: List[str],
    output_file: Path,
):
    """Compare headline metrics between two CA result sets."""
    if not HAS_MATPLOTLIB:
        print("Error: matplotlib is required for visualization.")
        print("Install with: pip install matplotlib")
        return

    summary_a = _extract_summary(results_a)
    summary_b = _extract_summary(results_b)

    metrics = [
        ("mean_ca_score", "Mean CA Score"),
        ("exact_match_rate", "Exact Match Rate"),
        ("normalized_match_rate", "Normalized Match Rate"),
        ("returncode_match_rate", "Return Code Match Rate"),
    ]

    values_a = [summary_a[m[0]] for m in metrics]
    values_b = [summary_b[m[0]] for m in metrics]

    fig, ax = plt.subplots(figsize=(10, 6))
    x_pos = range(len(metrics))
    width = 0.35

    ax.bar(
        [x - width / 2 for x in x_pos],
        values_a,
        width,
        label=labels[0],
        color=COLOR_COMPARISON_A,
        alpha=0.85,
    )
    ax.bar(
        [x + width / 2 for x in x_pos],
        values_b,
        width,
        label=labels[1],
        color=COLOR_COMPARISON_B,
        alpha=0.85,
    )

    ax.set_ylabel("Score / Rate", fontsize=12, fontweight="bold")
    ax.set_xticks(list(x_pos))
    ax.set_xticklabels([m[1] for m in metrics], rotation=20, ha="right", fontsize=10)
    ax.set_ylim(0, 1.1)
    ax.set_title("CA Summary Comparison", fontsize=14, fontweight="bold")
    ax.grid(axis="y", linestyle="--", alpha=0.3)
    ax.legend()

    plt.tight_layout()
    output_file.parent.mkdir(parents=True, exist_ok=True)
    plt.savefig(output_file, dpi=300, bbox_inches="tight")
    print(f"Plot saved to {output_file}")
    plt.close()


def plot_match_distribution_comparison(
    results_a: Dict[str, Any],
    results_b: Dict[str, Any],
    labels: List[str],
    output_file: Path,
):
    """Compare match distribution (perfect/partial/failure)."""
    if not HAS_MATPLOTLIB:
        print("Error: matplotlib is required for visualization.")
        print("Install with: pip install matplotlib")
        return

    summary_a = _extract_summary(results_a)
    summary_b = _extract_summary(results_b)

    categories = ["Perfect", "Partial", "Failure"]
    values_a = [
        summary_a["perfect_matches"],
        summary_a["partial_matches"],
        summary_a["failures"],
    ]
    values_b = [
        summary_b["perfect_matches"],
        summary_b["partial_matches"],
        summary_b["failures"],
    ]

    fig, ax = plt.subplots(figsize=(10, 6))
    x_pos = range(len(categories))
    width = 0.35

    ax.bar(
        [x - width / 2 for x in x_pos],
        values_a,
        width,
        label=labels[0],
        color=COLOR_COMPARISON_A,
        alpha=0.85,
    )
    ax.bar(
        [x + width / 2 for x in x_pos],
        values_b,
        width,
        label=labels[1],
        color=COLOR_COMPARISON_B,
        alpha=0.85,
    )

    ax.set_ylabel("Number of Files", fontsize=12, fontweight="bold")
    ax.set_xticks(list(x_pos))
    ax.set_xticklabels(categories, fontsize=11)
    ax.set_title("Match Type Distribution Comparison", fontsize=14, fontweight="bold")
    ax.grid(axis="y", linestyle="--", alpha=0.3)
    ax.legend()

    plt.tight_layout()
    output_file.parent.mkdir(parents=True, exist_ok=True)
    plt.savefig(output_file, dpi=300, bbox_inches="tight")
    print(f"Plot saved to {output_file}")
    plt.close()


def plot_filewise_scores_comparison(
    results_a: Dict[str, Any],
    results_b: Dict[str, Any],
    labels: List[str],
    output_file: Path,
):
    """Compare per-file CA scores."""
    if not HAS_MATPLOTLIB:
        print("Error: matplotlib is required for visualization.")
        print("Install with: pip install matplotlib")
        return

    results_dict_a = results_a.get("results", {})
    results_dict_b = results_b.get("results", {})
    files = sorted(set(results_dict_a.keys()) | set(results_dict_b.keys()))
    if not files:
        print("No overlapping files to visualize.")
        return

    scores_a = [results_dict_a.get(f, {}).get("ca_score", 0.0) or 0.0 for f in files]
    scores_b = [results_dict_b.get(f, {}).get("ca_score", 0.0) or 0.0 for f in files]

    fig, ax = plt.subplots(figsize=(16, 6))
    x_pos = range(len(files))
    width = 0.4

    ax.bar(
        [x - width / 2 for x in x_pos],
        scores_a,
        width,
        label=labels[0],
        color=COLOR_COMPARISON_A,
        alpha=0.85,
    )
    ax.bar(
        [x + width / 2 for x in x_pos],
        scores_b,
        width,
        label=labels[1],
        color=COLOR_COMPARISON_B,
        alpha=0.85,
    )

    ax.set_ylabel("CA Score", fontsize=12, fontweight="bold")
    ax.set_xticks(list(x_pos))
    ax.set_xticklabels(files, rotation=45, ha="right", fontsize=8)
    ax.set_ylim(0, 1.1)
    ax.set_title("Per-file CA Score Comparison", fontsize=14, fontweight="bold")
    ax.grid(axis="y", linestyle="--", alpha=0.3)
    ax.legend()

    plt.tight_layout()
    output_file.parent.mkdir(parents=True, exist_ok=True)
    plt.savefig(output_file, dpi=300, bbox_inches="tight")
    print(f"Plot saved to {output_file}")
    plt.close()


def create_comparison_visualizations(
    results_a: Dict[str, Any],
    results_b: Dict[str, Any],
    output_dir: Path,
    labels: Optional[List[str]] = None,
    prefix: str = "comparison",
):
    """Create comparison visualizations between two CA result sets."""
    if labels is None:
        labels = ["Reference", "Comparison"]

    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    print(f"Creating comparison visualizations in {output_dir}...")

    plot_summary_comparison(
        results_a,
        results_b,
        labels,
        output_file=output_dir / f"{prefix}_summary.png",
    )
    plot_match_distribution_comparison(
        results_a,
        results_b,
        labels,
        output_file=output_dir / f"{prefix}_match_distribution.png",
    )
    plot_filewise_scores_comparison(
        results_a,
        results_b,
        labels,
        output_file=output_dir / f"{prefix}_file_scores.png",
    )

    print(f"All comparison visualizations saved to {output_dir}")


def create_all_visualizations(
    results: Dict[str, Any],
    output_dir: Path,
    prefix: str = "ca",
):
    """
    Create all CA visualizations and save to output directory.
    
    Args:
        results: CA results dictionary
        output_dir: Directory to save visualization files
        prefix: Prefix for output filenames (default: "ca")
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    print(f"Creating CA visualizations in {output_dir}...")
    
    # CA scores bar chart
    plot_ca_scores(
        results,
        output_file=output_dir / f"{prefix}_scores.png",
        show_match_types=False,
    )
    
    # CA scores with match types
    plot_ca_scores(
        results,
        output_file=output_dir / f"{prefix}_scores_detailed.png",
        show_match_types=True,
    )
    
    # Distribution histogram
    plot_ca_distribution(
        results,
        output_file=output_dir / f"{prefix}_distribution.png",
    )
    
    # Match type breakdown
    plot_match_type_breakdown(
        results,
        output_file=output_dir / f"{prefix}_match_breakdown.png",
    )
    
    print(f"All visualizations saved to {output_dir}")


def main():
    """Command-line interface for CA visualization."""
    import argparse
    
    parser = argparse.ArgumentParser(
        description="Visualize CA evaluation results",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "results_file",
        type=str,
        help="Path to CA results JSON file",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=str,
        default=None,
        help="Output directory for visualizations (default: same as results file)",
    )
    parser.add_argument(
        "--prefix",
        type=str,
        default="ca",
        help="Prefix for output filenames (default: ca)",
    )
    parser.add_argument(
        "--type",
        type=str,
        choices=["scores", "distribution", "breakdown", "all"],
        default="all",
        help="Type of visualization to create (default: all)",
    )
    parser.add_argument(
        "--compare-with",
        type=str,
        default=None,
        help="Path to second CA results JSON file for comparison visualizations",
    )
    parser.add_argument(
        "--compare-labels",
        type=str,
        nargs=2,
        metavar=("LABEL_A", "LABEL_B"),
        default=None,
        help="Labels for comparison plots (default: derived from file names)",
    )
    parser.add_argument(
        "--comparison-prefix",
        type=str,
        default="comparison",
        help="Prefix for comparison output filenames (default: comparison)",
    )
    
    args = parser.parse_args()
    
    results_file = Path(args.results_file)
    if not results_file.exists():
        print(f"Error: Results file not found: {results_file}")
        return 1
    
    results = load_results(results_file)
    comparison_results = None
    comparison_labels: Optional[List[str]] = None

    if args.compare_with:
        compare_path = Path(args.compare_with)
        if not compare_path.exists():
            print(f"Error: Comparison file not found: {compare_path}")
            return 1
        comparison_results = load_results(compare_path)
        if args.compare_labels:
            comparison_labels = list(args.compare_labels)
        else:
            comparison_labels = [results_file.stem, compare_path.stem]
    
    if args.output:
        output_dir = Path(args.output)
    else:
        output_dir = results_file.parent / "visuals"
    
    if args.type == "all":
        create_all_visualizations(results, output_dir, prefix=args.prefix)
    elif args.type == "scores":
        plot_ca_scores(results, output_file=output_dir / f"{args.prefix}_scores.png")
    elif args.type == "distribution":
        plot_ca_distribution(results, output_file=output_dir / f"{args.prefix}_distribution.png")
    elif args.type == "breakdown":
        plot_match_type_breakdown(results, output_file=output_dir / f"{args.prefix}_breakdown.png")
    
    if comparison_results:
        create_comparison_visualizations(
            results,
            comparison_results,
            output_dir=output_dir,
            labels=comparison_labels,
            prefix=args.comparison_prefix,
        )

    return 0


if __name__ == "__main__":
    import sys
    sys.exit(main())

