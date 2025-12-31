"""Visualization tools for CodeBLEU evaluation results."""

import json
from pathlib import Path
from typing import Dict, Any, Optional, List, Tuple

try:
    import matplotlib.pyplot as plt
    import matplotlib
    import numpy as np
    from scipy import stats
    matplotlib.use("Agg")  # Use non-interactive backend
    HAS_MATPLOTLIB = True
    HAS_SCIPY = True
except ImportError:
    HAS_MATPLOTLIB = False
    HAS_SCIPY = False
    try:
        import numpy as np
    except ImportError:
        np = None

# CodeBLEU color palette
COLOR_CODEBLEU = "#293241"  # CodeBLEU (total)
COLOR_NGRAM = "#ee6c4d"  # N-gram
COLOR_WEIGHTED_NGRAM = "#e0fbfc"  # Weighted N-gram
COLOR_SYNTAX = "#98c1d9"  # Syntax
COLOR_DATAFLOW = "#3d5a80"  # Data-flow
COLOR_COMPARISON_A = "#42a5f5"  # Deterministic / baseline
COLOR_COMPARISON_B = "#4caf50"  # LLM-controlled / variant


def load_results(json_file: Path) -> Dict[str, Any]:
    """Load results from JSON file."""
    with open(json_file, "r", encoding="utf-8") as f:
        return json.load(f)


def plot_codebleu_scores(
    results: Dict[str, Any],
    output_file: Optional[Path] = None,
    show_components: bool = False,
):
    """
    Create bar chart visualization of CodeBLEU scores.
    
    Args:
        results: Results dictionary from evaluation
        output_file: Optional path to save the plot
        show_components: If True, show component scores (ngram, syntax, dataflow)
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
    codebleu_scores = []
    ngram_scores = []
    weighted_ngram_scores = []
    syntax_scores = []
    dataflow_scores = []
    
    for file_key, result in results_dict.items():
        # Handle both formats:
        # - modernize_and_evaluate.py: has "status" field ("success" or "evaluation_failed")
        # - benchmark.py: no "status" field, just has "codebleu" directly
        has_codebleu = "codebleu" in result
        status_check = "status" not in result or result.get("status") == "success"
        
        if has_codebleu and status_check:
            files.append(file_key.replace(".CBL", ""))
            codebleu_scores.append(result["codebleu"])
            
            if show_components:
                ngram_scores.append(result.get("ngram_match_score", 0.0))
                weighted_ngram_scores.append(result.get("weighted_ngram_match_score", 0.0))
                syntax_scores.append(result.get("syntax_match_score", 0.0))
                dataflow_scores.append(result.get("dataflow_match_score", 0.0))
    
    if not files:
        print("No successful evaluations to visualize.")
        return
    
    # Create figure
    fig, ax = plt.subplots(figsize=(12, 6))
    
    x_pos = range(len(files))
    width = 0.15 if show_components else 0.6
    
    if show_components:
        ax.bar([x - 1.5*width for x in x_pos], ngram_scores, width, label="N-gram", alpha=0.8, color=COLOR_NGRAM)
        ax.bar([x - 0.5*width for x in x_pos], weighted_ngram_scores, width, label="Weighted N-gram", alpha=0.8, color=COLOR_WEIGHTED_NGRAM)
        ax.bar([x + 0.5*width for x in x_pos], syntax_scores, width, label="Syntax", alpha=0.8, color=COLOR_SYNTAX)
        ax.bar([x + 1.5*width for x in x_pos], dataflow_scores, width, label="Data-flow", alpha=0.8, color=COLOR_DATAFLOW)
        ax.bar([x - 2.5*width for x in x_pos], codebleu_scores, width, label="CodeBLEU (total)", alpha=0.9, linewidth=1.5, color=COLOR_CODEBLEU)
    else:
        bars = ax.bar(x_pos, codebleu_scores, width, alpha=0.7, color=COLOR_CODEBLEU)
        
        # Add value labels on bars
        for i, (bar, score) in enumerate(zip(bars, codebleu_scores)):
            height = bar.get_height()
            ax.text(
                bar.get_x() + bar.get_width() / 2.0,
                height,
                f"{score:.3f}",
                ha="center",
                va="bottom",
                fontsize=9,
            )
    
    ax.set_xlabel("Number of Files", fontsize=12, fontweight="bold")
    ax.set_ylabel("Average CodeBLEU Score", fontsize=12, fontweight="bold")
    ax.set_title("CodeBLEU Evaluation Results", fontsize=14, fontweight="bold")
    ax.set_xticks(x_pos)
    ax.set_xticklabels(files, rotation=45, ha="right")
    ax.set_ylim([0, 1.1])
    ax.grid(axis="y", alpha=0.3, linestyle="--")
    ax.axhline(y=1.0, color="green", linestyle="--", alpha=0.5, label="Perfect Score")
    
    if show_components:
        ax.legend(loc="upper right", fontsize=9)
    else:
        ax.legend(["Perfect Score"], loc="upper right")
    
    # Add summary statistics
    summary = results.get("summary", {})
    mean_score = summary.get("mean_codebleu", 0.0)
    ax.axhline(y=mean_score, color="orange", linestyle=":", alpha=0.7, linewidth=2, label=f"Mean: {mean_score:.3f}")
    
    plt.tight_layout()
    
    if output_file:
        output_file.parent.mkdir(parents=True, exist_ok=True)
        plt.savefig(output_file, dpi=300, bbox_inches="tight")
        print(f"Plot saved to: {output_file}")
    else:
        plt.savefig("codebleu_results.png", dpi=300, bbox_inches="tight")
        print("Plot saved to: codebleu_results.png")
    
    plt.close()


def plot_component_comparison(results: Dict[str, Any], output_file: Optional[Path] = None):
    """Create a comparison chart of CodeBLEU components."""
    if not HAS_MATPLOTLIB:
        print("Error: matplotlib is required for visualization.")
        print("Install with: pip install matplotlib")
        return
    
    results_dict = results.get("results", {})
    if not results_dict:
        print("No results to visualize.")
        return
    
    files = []
    ngram_scores = []
    weighted_ngram_scores = []
    syntax_scores = []
    dataflow_scores = []
    
    for file_key, result in results_dict.items():
        # Handle both formats:
        # - modernize_and_evaluate.py: has "status" field ("success" or "evaluation_failed")
        # - benchmark.py: no "status" field, just has "codebleu" directly
        has_codebleu = "codebleu" in result
        status_check = "status" not in result or result.get("status") == "success"
        
        if has_codebleu and status_check:
            files.append(file_key.replace(".CBL", ""))
            ngram_scores.append(result.get("ngram_match_score", 0.0))
            weighted_ngram_scores.append(result.get("weighted_ngram_match_score", 0.0))
            syntax_scores.append(result.get("syntax_match_score", 0.0))
            dataflow_scores.append(result.get("dataflow_match_score", 0.0))
    
    if not files:
        print("No successful evaluations to visualize.")
        return
    
    # Create grouped bar chart
    fig, ax = plt.subplots(figsize=(14, 7))
    
    x_pos = range(len(files))
    width = 0.2
    
    ax.bar([x - 1.5*width for x in x_pos], ngram_scores, width, label="N-gram Match", alpha=0.8, color=COLOR_NGRAM)
    ax.bar([x - 0.5*width for x in x_pos], weighted_ngram_scores, width, label="Weighted N-gram", alpha=0.8, color=COLOR_WEIGHTED_NGRAM)
    ax.bar([x + 0.5*width for x in x_pos], syntax_scores, width, label="Syntax Match", alpha=0.8, color=COLOR_SYNTAX)
    ax.bar([x + 1.5*width for x in x_pos], dataflow_scores, width, label="Data-flow Match", alpha=0.8, color=COLOR_DATAFLOW)
    
    ax.set_xlabel("Number ofFiles", fontsize=12, fontweight="bold")
    ax.set_ylabel("Average Score", fontsize=12, fontweight="bold")
    ax.set_title("CodeBLEU Component Scores Comparison", fontsize=14, fontweight="bold")
    ax.set_xticks(x_pos)
    ax.set_xticklabels(files, rotation=45, ha="right")
    ax.set_ylim([0, 1.1])
    ax.grid(axis="y", alpha=0.3, linestyle="--")
    ax.legend(loc="upper right", fontsize=10)
    
    plt.tight_layout()
    
    if output_file:
        output_file.parent.mkdir(parents=True, exist_ok=True)
        plt.savefig(output_file, dpi=300, bbox_inches="tight")
        print(f"Component comparison plot saved to: {output_file}")
    else:
        plt.savefig("codebleu_components.png", dpi=300, bbox_inches="tight")
        print("Component comparison plot saved to: codebleu_components.png")
    
    plt.close()


def plot_score_distribution(results: Dict[str, Any], output_file: Optional[Path] = None):
    """Create a histogram of CodeBLEU score distribution."""
    if not HAS_MATPLOTLIB:
        print("Error: matplotlib is required for visualization.")
        print("Install with: pip install matplotlib")
        return
    
    results_dict = results.get("results", {})
    codebleu_scores = [
        result["codebleu"]
        for result in results_dict.values()
        if "codebleu" in result and ("status" not in result or result.get("status") == "success")
    ]
    
    if not codebleu_scores:
        print("No successful evaluations to visualize.")
        return
    
    fig, ax = plt.subplots(figsize=(10, 6))
    
    ax.hist(codebleu_scores, bins=min(20, len(codebleu_scores)), alpha=0.7, color=COLOR_CODEBLEU)
    
    summary = results.get("summary", {})
    mean_score = summary.get("mean_codebleu", 0.0)
    min_score = summary.get("min_codebleu", 0.0)
    max_score = summary.get("max_codebleu", 0.0)
    
    ax.axvline(x=mean_score, color="red", linestyle="--", linewidth=2, label=f"Mean: {mean_score:.3f}")
    ax.axvline(x=min_score, color="orange", linestyle=":", linewidth=2, label=f"Min: {min_score:.3f}")
    ax.axvline(x=max_score, color="green", linestyle=":", linewidth=2, label=f"Max: {max_score:.3f}")
    
    ax.set_xlabel("Average CodeBLEU Score", fontsize=12, fontweight="bold")
    ax.set_ylabel("Number of Files", fontsize=12, fontweight="bold")
    ax.set_title("CodeBLEU Score Distribution", fontsize=14, fontweight="bold")
    ax.grid(axis="y", alpha=0.3, linestyle="--")
    ax.legend(loc="upper right")
    
    plt.tight_layout()
    
    if output_file:
        output_file.parent.mkdir(parents=True, exist_ok=True)
        plt.savefig(output_file, dpi=300, bbox_inches="tight")
        print(f"Distribution plot saved to: {output_file}")
    else:
        plt.savefig("codebleu_distribution.png", dpi=300, bbox_inches="tight")
        print("Distribution plot saved to: codebleu_distribution.png")
    
    plt.close()


def plot_component_relationships(
    results: Dict[str, Any],
    output_file: Optional[Path] = None,
):
    """
    Create scatter plots showing relationships between CodeBLEU components with regression analysis.
    
    Creates a 2x2 grid showing:
    1. CodeBLEU vs Syntax Match
    2. CodeBLEU vs N-gram Match
    3. Syntax vs Data-flow Match
    4. CodeBLEU vs Weighted N-gram Match
    """
    if not HAS_MATPLOTLIB:
        print("Error: matplotlib is required for visualization.")
        print("Install with: pip install matplotlib")
        return
    
    if not HAS_SCIPY or np is None:
        print("Error: numpy and scipy are required for regression analysis.")
        print("Install with: pip install numpy scipy")
        return
    
    results_dict = results.get("results", {})
    if not results_dict:
        print("No results to visualize.")
        return
    
    # Extract data
    codebleu_scores = []
    ngram_scores = []
    weighted_ngram_scores = []
    syntax_scores = []
    dataflow_scores = []
    file_names = []
    
    for file_key, result in results_dict.items():
        has_codebleu = "codebleu" in result
        status_check = "status" not in result or result.get("status") == "success"
        
        if has_codebleu and status_check:
            file_names.append(file_key.replace(".CBL", ""))
            codebleu_scores.append(result["codebleu"])
            ngram_scores.append(result.get("ngram_match_score", 0.0))
            weighted_ngram_scores.append(result.get("weighted_ngram_match_score", 0.0))
            syntax_scores.append(result.get("syntax_match_score", 0.0))
            dataflow_scores.append(result.get("dataflow_match_score", 0.0))
    
    if not codebleu_scores:
        print("No successful evaluations to visualize.")
        return
    
    # Convert to numpy arrays
    codebleu = np.array(codebleu_scores)
    ngram = np.array(ngram_scores)
    weighted_ngram = np.array(weighted_ngram_scores)
    syntax = np.array(syntax_scores)
    dataflow = np.array(dataflow_scores)
    
    # Create 2x2 subplot grid
    fig, axes = plt.subplots(2, 2, figsize=(14, 12))
    fig.suptitle("CodeBLEU Component Relationships", fontsize=16, fontweight="bold")
    
    # Helper function to add regression line and statistics
    def add_regression(ax, x, y, xlabel, ylabel, title):
        """Add scatter plot with regression line and statistics box."""
        # Remove NaN values
        mask = ~(np.isnan(x) | np.isnan(y))
        x_clean = x[mask]
        y_clean = y[mask]
        
        if len(x_clean) < 2:
            ax.text(0.5, 0.5, "Insufficient data", ha="center", va="center", transform=ax.transAxes)
            return
        
        # Calculate regression
        slope, intercept, r_value, p_value, std_err = stats.linregress(x_clean, y_clean)
        r_squared = r_value ** 2
        
        # Create scatter plot
        scatter = ax.scatter(x_clean, y_clean, alpha=0.6, s=100, c=COLOR_CODEBLEU, edgecolors='white', linewidths=0.5)
        
        # Add regression line
        x_line = np.linspace(x_clean.min(), x_clean.max(), 100)
        y_line = slope * x_line + intercept
        ax.plot(x_line, y_line, '--', color='gray', linewidth=2, alpha=0.7, label='Regression line')
        
        # Add statistics box
        stats_text = f'β = {slope:.3f}\np = {p_value:.3f}\nR² = {r_squared:.3f}'
        ax.text(0.98, 0.02, stats_text, transform=ax.transAxes,
                fontsize=10, verticalalignment='bottom', horizontalalignment='right',
                bbox=dict(boxstyle='round', facecolor='white', alpha=0.8, edgecolor='gray'))
        
        ax.set_xlabel(xlabel, fontsize=11, fontweight="bold")
        ax.set_ylabel(ylabel, fontsize=11, fontweight="bold")
        ax.set_title(title, fontsize=12, fontweight="bold")
        ax.grid(True, alpha=0.3, linestyle='--')
    
    # Plot 1: CodeBLEU vs Syntax Match
    add_regression(axes[0, 0], syntax, codebleu,
                   "Syntax Match Score",
                   "CodeBLEU Score",
                   "CodeBLEU vs Syntax Match")
    
    # Plot 2: CodeBLEU vs N-gram Match
    add_regression(axes[0, 1], ngram, codebleu,
                   "N-gram Match Score",
                   "CodeBLEU Score",
                   "CodeBLEU vs N-gram Match")
    
    # Plot 3: Syntax vs Data-flow Match
    add_regression(axes[1, 0], dataflow, syntax,
                   "Data-flow Match Score",
                   "Syntax Match Score",
                   "Syntax vs Data-flow Match")
    
    # Plot 4: CodeBLEU vs Weighted N-gram Match
    add_regression(axes[1, 1], weighted_ngram, codebleu,
                   "Weighted N-gram Match Score",
                   "CodeBLEU Score",
                   "CodeBLEU vs Weighted N-gram Match")
    
    plt.tight_layout()
    
    if output_file:
        output_file.parent.mkdir(parents=True, exist_ok=True)
        plt.savefig(output_file, dpi=300, bbox_inches="tight")
        print(f"Component relationships plot saved to: {output_file}")
    else:
        plt.savefig("codebleu_relationships.png", dpi=300, bbox_inches="tight")
        print("Component relationships plot saved to: codebleu_relationships.png")
    
    plt.close()


def _extract_codebleu_summary(results: Dict[str, Any]) -> Dict[str, float]:
    """Compute summary metrics with fallbacks if missing."""
    summary = results.get("summary", {})
    results_dict = results.get("results", {})

    def _mean_from_results(extract_fn) -> float:
        values = []
        for result in results_dict.values():
            has_codebleu = "codebleu" in result
            status_check = "status" not in result or result.get("status") == "success"
            if has_codebleu and status_check:
                values.append(extract_fn(result))
        if not values:
            return 0.0
        return float(sum(values) / len(values))

    metrics = {
        "mean_codebleu": summary.get("mean_codebleu"),
        "mean_ngram_match": summary.get("mean_ngram_match"),
        "mean_weighted_ngram_match": summary.get("mean_weighted_ngram_match"),
        "mean_syntax_match": summary.get("mean_syntax_match"),
        "mean_dataflow_match": summary.get("mean_dataflow_match"),
    }

    fallbacks = {
        "mean_codebleu": lambda r: r.get("codebleu", 0.0),
        "mean_ngram_match": lambda r: r.get("ngram_match_score", 0.0),
        "mean_weighted_ngram_match": lambda r: r.get("weighted_ngram_match_score", 0.0),
        "mean_syntax_match": lambda r: r.get("syntax_match_score", 0.0),
        "mean_dataflow_match": lambda r: r.get("dataflow_match_score", 0.0),
    }

    computed = {}
    for key, value in metrics.items():
        if value is not None:
            computed[key] = float(value)
        else:
            computed[key] = _mean_from_results(fallbacks[key])

    computed["num_files"] = int(summary.get("num_files", len(results_dict)))
    return computed


def _extract_per_file_scores(results: Dict[str, Any]) -> Dict[str, float]:
    """Return per-file CodeBLEU scores with default of 0.0."""
    per_file = {}
    for file_key, result in results.get("results", {}).items():
        has_codebleu = "codebleu" in result
        status_check = "status" not in result or result.get("status") == "success"
        if has_codebleu and status_check:
            per_file[file_key.replace(".CBL", "")] = float(result.get("codebleu", 0.0))
        else:
            per_file[file_key.replace(".CBL", "")] = 0.0
    return per_file


def plot_codebleu_summary_comparison(
    results_a: Dict[str, Any],
    results_b: Dict[str, Any],
    labels: List[str],
    output_file: Path,
):
    """Compare aggregate CodeBLEU metrics for two result sets."""
    if not HAS_MATPLOTLIB:
        print("Error: matplotlib is required for visualization.")
        print("Install with: pip install matplotlib")
        return

    summary_a = _extract_codebleu_summary(results_a)
    summary_b = _extract_codebleu_summary(results_b)

    metrics = [
        ("mean_codebleu", "CodeBLEU"),
        ("mean_ngram_match", "N-gram"),
        ("mean_weighted_ngram_match", "Weighted N-gram"),
        ("mean_syntax_match", "Syntax"),
        ("mean_dataflow_match", "Data-flow"),
    ]

    values_a = [summary_a[m[0]] for m in metrics]
    values_b = [summary_b[m[0]] for m in metrics]

    fig, ax = plt.subplots(figsize=(11, 6))
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

    ax.set_ylabel("Score", fontsize=12, fontweight="bold")
    ax.set_xticks(list(x_pos))
    ax.set_xticklabels([m[1] for m in metrics], fontsize=11)
    ax.set_ylim(0, 1.0)
    ax.set_title("CodeBLEU Summary Comparison", fontsize=14, fontweight="bold")
    ax.grid(axis="y", linestyle="--", alpha=0.3)
    ax.legend()

    plt.tight_layout()
    output_file.parent.mkdir(parents=True, exist_ok=True)
    plt.savefig(output_file, dpi=300, bbox_inches="tight")
    print(f"Plot saved to: {output_file}")
    plt.close()


def plot_codebleu_distribution_comparison(
    results_a: Dict[str, Any],
    results_b: Dict[str, Any],
    labels: List[str],
    output_file: Path,
):
    """Overlay histogram comparison for CodeBLEU distributions."""
    if not HAS_MATPLOTLIB:
        print("Error: matplotlib is required for visualization.")
        print("Install with: pip install matplotlib")
        return

    def _scores(results: Dict[str, Any]) -> List[float]:
        values = []
        for result in results.get("results", {}).values():
            has_codebleu = "codebleu" in result
            status_check = "status" not in result or result.get("status") == "success"
            if has_codebleu and status_check:
                values.append(float(result["codebleu"]))
        return values

    scores_a = _scores(results_a)
    scores_b = _scores(results_b)

    if not scores_a or not scores_b:
        print("Insufficient results to visualize comparison.")
        return

    bins = [i / 20 for i in range(21)]  # 0.0 to 1.0 in 0.05 steps
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.hist(
        scores_a,
        bins=bins,
        alpha=0.6,
        color=COLOR_COMPARISON_A,
        label=f"{labels[0]} (mean { _extract_codebleu_summary(results_a)['mean_codebleu']:.3f})",
        edgecolor="black",
    )
    ax.hist(
        scores_b,
        bins=bins,
        alpha=0.6,
        color=COLOR_COMPARISON_B,
        label=f"{labels[1]} (mean { _extract_codebleu_summary(results_b)['mean_codebleu']:.3f})",
        edgecolor="black",
    )

    ax.set_xlabel("CodeBLEU Score", fontsize=12, fontweight="bold")
    ax.set_ylabel("Number of Files", fontsize=12, fontweight="bold")
    ax.set_title("CodeBLEU Score Distribution Comparison", fontsize=14, fontweight="bold")
    ax.set_xlim(0, 1.0)
    ax.grid(axis="y", linestyle="--", alpha=0.3)
    ax.legend()

    plt.tight_layout()
    output_file.parent.mkdir(parents=True, exist_ok=True)
    plt.savefig(output_file, dpi=300, bbox_inches="tight")
    print(f"Plot saved to: {output_file}")
    plt.close()


def plot_filewise_codebleu_comparison(
    results_a: Dict[str, Any],
    results_b: Dict[str, Any],
    labels: List[str],
    output_file: Path,
):
    """Compare per-file CodeBLEU scores."""
    if not HAS_MATPLOTLIB:
        print("Error: matplotlib is required for visualization.")
        print("Install with: pip install matplotlib")
        return

    scores_a = _extract_per_file_scores(results_a)
    scores_b = _extract_per_file_scores(results_b)
    files = sorted(set(scores_a.keys()) | set(scores_b.keys()))
    if not files:
        print("No overlapping files to visualize.")
        return

    values_a = [scores_a.get(f, 0.0) for f in files]
    values_b = [scores_b.get(f, 0.0) for f in files]

    fig, ax = plt.subplots(figsize=(18, 6))
    x_pos = range(len(files))
    width = 0.4

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

    ax.set_ylabel("CodeBLEU Score", fontsize=12, fontweight="bold")
    ax.set_xticks(list(x_pos))
    ax.set_xticklabels(files, rotation=45, ha="right", fontsize=8)
    ax.set_ylim(0, 1.0)
    ax.set_title("Per-file CodeBLEU Comparison", fontsize=14, fontweight="bold")
    ax.grid(axis="y", linestyle="--", alpha=0.3)
    ax.legend()

    plt.tight_layout()
    output_file.parent.mkdir(parents=True, exist_ok=True)
    plt.savefig(output_file, dpi=300, bbox_inches="tight")
    print(f"Plot saved to: {output_file}")
    plt.close()


def create_codebleu_comparison_visualizations(
    results_a: Dict[str, Any],
    results_b: Dict[str, Any],
    output_dir: Path,
    labels: Optional[List[str]] = None,
    prefix: str = "codebleu_comparison",
):
    """Create comparison visualizations between two CodeBLEU result sets."""
    if labels is None:
        labels = ["Reference", "Comparison"]
    else:
        labels = list(labels)

    def _is_deterministic(label: str) -> bool:
        return "deterministic" in label.lower()

    if len(labels) == 2 and not _is_deterministic(labels[0]) and _is_deterministic(labels[1]):
        # Ensure deterministic results are always plotted first
        results_a, results_b = results_b, results_a
        labels = [labels[1], labels[0]]

    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    print(f"Creating CodeBLEU comparison visuals in {output_dir}...")

    plot_codebleu_summary_comparison(
        results_a,
        results_b,
        labels,
        output_file=output_dir / f"{prefix}_summary.png",
    )
    plot_codebleu_distribution_comparison(
        results_a,
        results_b,
        labels,
        output_file=output_dir / f"{prefix}_distribution.png",
    )
    plot_filewise_codebleu_comparison(
        results_a,
        results_b,
        labels,
        output_file=output_dir / f"{prefix}_file_scores.png",
    )

    print(f"All comparison visuals saved to {output_dir}")


def create_all_visualizations(
    results_json: Path,
    output_dir: Optional[Path] = None,
    prefix: str = "codebleu",
):
    """
    Create all visualizations from results JSON.
    
    Args:
        results_json: Path to results JSON file
        output_dir: Optional directory to save plots (default: data/output/visuals)
        prefix: Prefix for output filenames
    """
    if not HAS_MATPLOTLIB:
        print("Error: matplotlib is required for visualization.")
        print("Install with: pip install matplotlib")
        return
    
    results = load_results(results_json)
    
    if output_dir is None:
        # Default to data/output/visuals directory
        output_dir = Path("data/output/visuals")
    else:
        output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    # Create all visualizations
    plot_codebleu_scores(
        results,
        output_file=output_dir / f"{prefix}_scores.png",
        show_components=False,
    )
    
    plot_codebleu_scores(
        results,
        output_file=output_dir / f"{prefix}_scores_with_components.png",
        show_components=True,
    )
    
    plot_component_comparison(
        results,
        output_file=output_dir / f"{prefix}_components.png",
    )
    
    plot_score_distribution(
        results,
        output_file=output_dir / f"{prefix}_distribution.png",
    )
    
    plot_component_relationships(
        results,
        output_file=output_dir / f"{prefix}_relationships.png",
    )
    
    print(f"\n✅ All visualizations saved to: {output_dir}")


if __name__ == "__main__":
    import argparse
    import sys
    
    parser = argparse.ArgumentParser(
        description="Visualize CodeBLEU evaluation results",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Create all visualizations from results JSON
  python -m evals.codebleu.visualize data/output/opensourcecobol_results.json
  
  # Save to specific directory
  python -m evals.codebleu.visualize results.json -o plots/
  
  # Create only score chart
  python -m evals.codebleu.visualize results.json --scores-only
        """,
    )
    
    parser.add_argument(
        "results_json",
        type=str,
        help="Path to CodeBLEU results JSON file",
    )
    
    parser.add_argument(
        "-o",
        "--output-dir",
        type=str,
        default=None,
        help="Output directory for plots (default: data/output/visuals)",
    )
    
    parser.add_argument(
        "--prefix",
        type=str,
        default="codebleu",
        help="Prefix for output filenames (default: codebleu)",
    )
    
    parser.add_argument(
        "--scores-only",
        action="store_true",
        help="Only create CodeBLEU scores chart",
    )
    
    parser.add_argument(
        "--components-only",
        action="store_true",
        help="Only create component comparison chart",
    )
    
    parser.add_argument(
        "--distribution-only",
        action="store_true",
        help="Only create distribution histogram",
    )
    
    parser.add_argument(
        "--relationships-only",
        action="store_true",
        help="Only create component relationships scatter plots",
    )
    parser.add_argument(
        "--compare-with",
        type=str,
        default=None,
        help="Path to second CodeBLEU results JSON file for comparison plots",
    )
    parser.add_argument(
        "--compare-labels",
        type=str,
        nargs=2,
        metavar=("LABEL_A", "LABEL_B"),
        default=None,
        help="Labels for comparison plots (default: derived from filenames)",
    )
    parser.add_argument(
        "--comparison-prefix",
        type=str,
        default="codebleu_comparison",
        help="Prefix for comparison output filenames",
    )
    
    args = parser.parse_args()
    
    results_json = Path(args.results_json)
    if not results_json.exists():
        print(f"Error: Results file not found: {results_json}")
        sys.exit(1)
    
    results = load_results(results_json)
    comparison_results: Optional[Dict[str, Any]] = None
    comparison_labels: Optional[List[str]] = None

    if args.compare_with:
        compare_path = Path(args.compare_with)
        if not compare_path.exists():
            print(f"Error: Comparison file not found: {compare_path}")
            sys.exit(1)
        comparison_results = load_results(compare_path)
        if args.compare_labels:
            comparison_labels = list(args.compare_labels)
        else:
            comparison_labels = [results_json.stem, compare_path.stem]

    if args.output_dir:
        output_dir = Path(args.output_dir)
    else:
        output_dir = Path("data/output/visuals")
    output_dir.mkdir(parents=True, exist_ok=True)
    
    if args.scores_only:
        plot_codebleu_scores(results, output_file=output_dir / f"{args.prefix}_scores.png")
    elif args.components_only:
        plot_component_comparison(results, output_file=output_dir / f"{args.prefix}_components.png")
    elif args.distribution_only:
        plot_score_distribution(results, output_file=output_dir / f"{args.prefix}_distribution.png")
    elif args.relationships_only:
        plot_component_relationships(results, output_file=output_dir / f"{args.prefix}_relationships.png")
    else:
        create_all_visualizations(results_json, output_dir, args.prefix)

    if comparison_results:
        create_codebleu_comparison_visualizations(
            results,
            comparison_results,
            output_dir=output_dir,
            labels=comparison_labels,
            prefix=args.comparison_prefix,
        )

