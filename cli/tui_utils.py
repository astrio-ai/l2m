"""TUI utilities for Atlas TUI.

This module provides terminal color detection, intelligent blending,
shimmer effects, and other visual enhancements for Atlas TUI.
"""

import os
import re
import time
from typing import Optional, Tuple

# ANSI color codes - Very muted, minimal brightness
ANSI_RESET = "\033[0m"
ANSI_BOLD = "\033[1m"
ANSI_DIM = "\033[2m"
ANSI_ITALIC = "\033[3m"

# Very muted palette: subdued tones, minimal eye strain
ANSI_WHITE = "\033[38;2;255;255;255m"     # Pure white for important text (#FFFFFF)
ANSI_GRAY = "\033[38;2;120;120;120m"      # Dimmer gray for secondary text (#787878)
ANSI_DARK_GRAY = "\033[38;2;80;80;80m"    # Dark gray for minimal importance (#505050)
ANSI_BRAND = "\033[38;2;39;142;245m"      # Brand blue (#278ef5)
ANSI_CYAN = "\033[38;2;33;150;243m"       # Bright blue for selected (#2196F3)
ANSI_GREEN = "\033[38;2;120;150;120m"     # Very muted green for success (#789678)
ANSI_RED = "\033[38;2;180;90;90m"         # Very muted red for errors (#B45A5A)
ANSI_ORANGE = "\033[38;2;180;130;90m"     # Very muted orange for warnings (#B4825A)
ANSI_PROMPT_BG = "\033[48;2;30;30;30m"    # Darker background for input prompt


class TerminalColors:
    """Detect and manage terminal colors with intelligent theming."""

    _cached_bg: Optional[Tuple[int, int, int]] = None
    _cached_fg: Optional[Tuple[int, int, int]] = None

    @staticmethod
    def is_light(rgb: Tuple[int, int, int]) -> bool:
        """Determine if a color is light using luminance calculation."""
        r, g, b = rgb
        # Rec. 709 luma coefficients
        y = 0.299 * r + 0.587 * g + 0.114 * b
        return y > 128.0

    @staticmethod
    def blend(
        fg: Tuple[int, int, int], bg: Tuple[int, int, int], alpha: float
    ) -> Tuple[int, int, int]:
        """Alpha-blend two RGB colors."""
        r = int(fg[0] * alpha + bg[0] * (1.0 - alpha))
        g = int(fg[1] * alpha + bg[1] * (1.0 - alpha))
        b = int(fg[2] * alpha + bg[2] * (1.0 - alpha))
        return (r, g, b)

    @staticmethod
    def perceptual_distance(a: Tuple[int, int, int], b: Tuple[int, int, int]) -> float:
        """Calculate perceptual color distance using CIE76 formula."""

        def srgb_to_linear(c: int) -> float:
            c_norm = c / 255.0
            if c_norm <= 0.04045:
                return c_norm / 12.92
            return ((c_norm + 0.055) / 1.055) ** 2.4

        def rgb_to_xyz(r: int, g: int, b: int) -> Tuple[float, float, float]:
            r_lin = srgb_to_linear(r)
            g_lin = srgb_to_linear(g)
            b_lin = srgb_to_linear(b)

            x = r_lin * 0.4124 + g_lin * 0.3576 + b_lin * 0.1805
            y = r_lin * 0.2126 + g_lin * 0.7152 + b_lin * 0.0722
            z = r_lin * 0.0193 + g_lin * 0.1192 + b_lin * 0.9505
            return (x, y, z)

        def xyz_to_lab(x: float, y: float, z: float) -> Tuple[float, float, float]:
            # D65 reference white
            xr = x / 0.95047
            yr = y / 1.00000
            zr = z / 1.08883

            def f(t: float) -> float:
                if t > 0.008856:
                    return t ** (1.0 / 3.0)
                return 7.787 * t + 16.0 / 116.0

            fx = f(xr)
            fy = f(yr)
            fz = f(zr)

            l = 116.0 * fy - 16.0
            a_val = 500.0 * (fx - fy)
            b_val = 200.0 * (fy - fz)
            return (l, a_val, b_val)

        x1, y1, z1 = rgb_to_xyz(a[0], a[1], a[2])
        x2, y2, z2 = rgb_to_xyz(b[0], b[1], b[2])

        l1, a1, b1 = xyz_to_lab(x1, y1, z1)
        l2, a2, b2 = xyz_to_lab(x2, y2, z2)

        dl = l1 - l2
        da = a1 - a2
        db = b1 - b2

        return (dl * dl + da * da + db * db) ** 0.5

    @classmethod
    def detect_terminal_bg(cls) -> Optional[Tuple[int, int, int]]:
        """Attempt to detect terminal background color."""
        if cls._cached_bg is not None:
            return cls._cached_bg

        # Try to detect from environment or terminal queries
        # For now, return None (use default)
        # TODO: Implement OSC 11 query for terminal background
        cls._cached_bg = None
        return cls._cached_bg

    @classmethod
    def detect_terminal_fg(cls) -> Optional[Tuple[int, int, int]]:
        """Attempt to detect terminal foreground color."""
        if cls._cached_fg is not None:
            return cls._cached_fg

        # Try to detect from environment or terminal queries
        # For now, return None (use default)
        # TODO: Implement OSC 10 query for terminal foreground
        cls._cached_fg = None
        return cls._cached_fg

    @staticmethod
    def rgb_to_ansi(rgb: Tuple[int, int, int]) -> str:
        """Convert RGB to ANSI 24-bit color code."""
        r, g, b = rgb
        return f"\033[38;2;{r};{g};{b}m"

    @staticmethod
    def supports_truecolor() -> bool:
        """Check if terminal supports 24-bit color."""
        colorterm = os.environ.get("COLORTERM", "")
        if colorterm in ("truecolor", "24bit"):
            return True

        term = os.environ.get("TERM", "")
        if "24bit" in term or "truecolor" in term:
            return True

        return False


class ShimmerEffect:
    """Animated shimmer/wave effect for text during loading states."""

    def __init__(self, text: str):
        self.text = text
        self.start_time = time.time()
        self.period = 2.0  # seconds for one sweep
        self.band_width = 5.0  # character width of the shimmer band

    def render(self) -> str:
        """Render the text with shimmer effect at current time."""
        chars = list(self.text)
        if not chars:
            return ""

        # Calculate sweep position based on elapsed time
        elapsed = time.time() - self.start_time
        padding = 10
        period_length = len(chars) + padding * 2
        pos = ((elapsed % self.period) / self.period) * period_length

        has_truecolor = TerminalColors.supports_truecolor()

        if has_truecolor:
            # Use RGB color blending
            result_parts = []
            base_color = (180, 180, 180)  # Dim gray
            highlight_color = (255, 255, 255)  # Bright white

            for i, ch in enumerate(chars):
                i_pos = i + padding
                dist = abs(i_pos - pos)

                if dist <= self.band_width:
                    # Calculate intensity using cosine wave
                    import math

                    x = math.pi * (dist / self.band_width)
                    intensity = 0.5 * (1.0 + math.cos(x))
                else:
                    intensity = 0.0

                # Blend colors
                highlight = min(max(intensity, 0.0), 1.0)
                r, g, b = TerminalColors.blend(highlight_color, base_color, highlight * 0.9)

                result_parts.append(f"{TerminalColors.rgb_to_ansi((r, g, b))}{ANSI_BOLD}{ch}")

            return "".join(result_parts) + ANSI_RESET
        else:
            # Fallback to simple bold/dim effects
            result_parts = []
            for i, ch in enumerate(chars):
                i_pos = i + padding
                dist = abs(i_pos - pos)

                if dist <= self.band_width * 0.3:
                    result_parts.append(f"{ANSI_BOLD}{ch}")
                elif dist <= self.band_width:
                    result_parts.append(ch)
                else:
                    result_parts.append(f"{ANSI_DIM}{ch}")

            return "".join(result_parts) + ANSI_RESET


class StatusIndicator:
    """Visual status indicators for different agent states."""

    THINKING = f"{ANSI_BRAND}●{ANSI_RESET} "
    TYPING = f"{ANSI_BRAND}▌{ANSI_RESET} "
    SUCCESS = f"{ANSI_GREEN}✓{ANSI_RESET} "
    ERROR = f"{ANSI_RED}✗{ANSI_RESET} "
    WARNING = f"{ANSI_ORANGE}⚠{ANSI_RESET} "
    INFO = f"{ANSI_BRAND}ℹ{ANSI_RESET} "

    @staticmethod
    def spinner_frames() -> list:
        """Return spinner animation frames."""
        return ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]

    @staticmethod
    def get_spinner_frame(index: int) -> str:
        """Get a specific spinner frame."""
        frames = StatusIndicator.spinner_frames()
        return f"{ANSI_BRAND}{frames[index % len(frames)]}{ANSI_RESET}"


class StyleGuide:
    """Muted color style guide: comfortable colors, no brightness."""

    @staticmethod
    def header(text: str) -> str:
        """Format text as a header (white)."""
        return f"{ANSI_WHITE}{text}{ANSI_RESET}"

    @staticmethod
    def secondary(text: str) -> str:
        """Format text as secondary (gray, less important)."""
        return f"{ANSI_GRAY}{text}{ANSI_RESET}"

    @staticmethod
    def dim(text: str) -> str:
        """Format text as dimmed (dark gray, minimal importance)."""
        return f"{ANSI_DARK_GRAY}{text}{ANSI_RESET}"

    @staticmethod
    def user_input(text: str) -> str:
        """Format text as user input (light gray, important)."""
        return f"{ANSI_WHITE}{text}{ANSI_RESET}"

    @staticmethod
    def selected(text: str) -> str:
        """Format text as selected/highlighted (brand color)."""
        return f"{ANSI_BRAND}{text}{ANSI_RESET}"

    @staticmethod
    def success(text: str) -> str:
        """Format text as success (muted green)."""
        return f"{ANSI_GREEN}{text}{ANSI_RESET}"

    @staticmethod
    def error(text: str) -> str:
        """Format text as error (muted red)."""
        return f"{ANSI_RED}{text}{ANSI_RESET}"

    @staticmethod
    def warning(text: str) -> str:
        """Format text as warning (muted orange)."""
        return f"{ANSI_ORANGE}{text}{ANSI_RESET}"

    @staticmethod
    def command(text: str) -> str:
        """Format text as command (brand color, highlighted)."""
        return f"{ANSI_BRAND}{text}{ANSI_RESET}"

    @staticmethod
    def code(text: str) -> str:
        """Format text as inline code (gray)."""
        return f"{ANSI_GRAY}{text}{ANSI_RESET}"

    @staticmethod
    def prompt_background(text: str) -> str:
        """Format text with darker background for input prompt area."""
        return f"{ANSI_PROMPT_BG}{text}{ANSI_RESET}"


def format_separator(width: Optional[int] = None, char: str = "─") -> str:
    """Create a horizontal separator line."""
    if width is None:
        try:
            import shutil

            width = shutil.get_terminal_size().columns
        except Exception:
            width = 80
    return f"{ANSI_BRAND}{char * width}{ANSI_RESET}"


def format_footer_hint(text: str) -> str:
    """Format footer hint text with proper styling."""
    return f"  {ANSI_BRAND}{text}{ANSI_RESET}"


def truncate_with_ellipsis(text: str, max_length: int) -> str:
    """Truncate text with ellipsis if too long."""
    if len(text) <= max_length:
        return text
    return text[: max_length - 1] + "…"

