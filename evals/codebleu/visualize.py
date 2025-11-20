"""Visualization tools for CodeBLEU evaluation results."""

import json
from pathlib import Path
from typing import Dict, Any, Optional

try:
    import matplotlib.pyplot as plt
    import matplotlib
    matplotlib.use("Agg")  # Use non-interactive backend
    HAS_MATPLOTLIB = True
except ImportError:
    HAS_MATPLOTLIB = False


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
        if result.get("status") == "success" and "codebleu" in result:
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
        ax.bar([x - 1.5*width for x in x_pos], ngram_scores, width, label="N-gram", alpha=0.8)
        ax.bar([x - 0.5*width for x in x_pos], weighted_ngram_scores, width, label="Weighted N-gram", alpha=0.8)
        ax.bar([x + 0.5*width for x in x_pos], syntax_scores, width, label="Syntax", alpha=0.8)
        ax.bar([x + 1.5*width for x in x_pos], dataflow_scores, width, label="Data-flow", alpha=0.8)
        ax.bar([x - 2.5*width for x in x_pos], codebleu_scores, width, label="CodeBLEU (total)", alpha=0.9, edgecolor="black", linewidth=1.5)
    else:
        bars = ax.bar(x_pos, codebleu_scores, width, alpha=0.7, color="steelblue", edgecolor="black")
        
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
    
    ax.set_xlabel("Files", fontsize=12, fontweight="bold")
    ax.set_ylabel("CodeBLEU Score", fontsize=12, fontweight="bold")
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
        if result.get("status") == "success" and "codebleu" in result:
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
    
    ax.bar([x - 1.5*width for x in x_pos], ngram_scores, width, label="N-gram Match", alpha=0.8, color="#1f77b4")
    ax.bar([x - 0.5*width for x in x_pos], weighted_ngram_scores, width, label="Weighted N-gram", alpha=0.8, color="#ff7f0e")
    ax.bar([x + 0.5*width for x in x_pos], syntax_scores, width, label="Syntax Match", alpha=0.8, color="#2ca02c")
    ax.bar([x + 1.5*width for x in x_pos], dataflow_scores, width, label="Data-flow Match", alpha=0.8, color="#d62728")
    
    ax.set_xlabel("Files", fontsize=12, fontweight="bold")
    ax.set_ylabel("Score", fontsize=12, fontweight="bold")
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
        if result.get("status") == "success" and "codebleu" in result
    ]
    
    if not codebleu_scores:
        print("No successful evaluations to visualize.")
        return
    
    fig, ax = plt.subplots(figsize=(10, 6))
    
    ax.hist(codebleu_scores, bins=min(20, len(codebleu_scores)), edgecolor="black", alpha=0.7, color="steelblue")
    
    summary = results.get("summary", {})
    mean_score = summary.get("mean_codebleu", 0.0)
    min_score = summary.get("min_codebleu", 0.0)
    max_score = summary.get("max_codebleu", 0.0)
    
    ax.axvline(x=mean_score, color="red", linestyle="--", linewidth=2, label=f"Mean: {mean_score:.3f}")
    ax.axvline(x=min_score, color="orange", linestyle=":", linewidth=2, label=f"Min: {min_score:.3f}")
    ax.axvline(x=max_score, color="green", linestyle=":", linewidth=2, label=f"Max: {max_score:.3f}")
    
    ax.set_xlabel("CodeBLEU Score", fontsize=12, fontweight="bold")
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
    
    args = parser.parse_args()
    
    results_json = Path(args.results_json)
    if not results_json.exists():
        print(f"Error: Results file not found: {results_json}")
        sys.exit(1)
    
    results = load_results(results_json)
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
    else:
        create_all_visualizations(results_json, output_dir, args.prefix)

