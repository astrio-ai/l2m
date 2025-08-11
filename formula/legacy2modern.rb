class Legacy2modern < Formula
  desc "AI-Powered Legacy Code Transpilation Engine with Modern CLI"
  homepage "https://github.com/astrio-ai/legacy2modern"
  url "https://github.com/astrio-ai/legacy2modern/archive/refs/tags/v0.1.0.tar.gz"
  sha256 ""  # This will be calculated when you create the release
  license "Apache-2.0"
  head "https://github.com/astrio-ai/legacy2modern.git", branch: "main"

  depends_on "python@3.10"

  def install
    # Install the Python package
    system "python3", "-m", "pip", "install", *std_pip_args, "."
    
    # Make scripts executable
    bin.install "scripts/legacy2modern"
    bin.install "scripts/l2m"
  end

  test do
    # Test that the CLI can be run
    system "#{bin}/legacy2modern", "--help"
    system "#{bin}/l2m", "--help"
  end
end 