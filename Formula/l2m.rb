class L2m < Formula
  include Language::Python::Virtualenv

  desc "AI-powered legacy code modernization tool"
  homepage "https://github.com/astrio-ai/l2m"
  url "https://files.pythonhosted.org/packages/03/42/153c7c7f8fdd6ca124d183df7515b5113c77a34c24dd98bdf07bed70b64a/l2m-0.2.0.tar.gz"
  sha256 "ecf3e100bdab0c552303fc56dc66f213043a3ce0de03b8a3005c901c2a8ecbde"
  license "Apache-2.0"
  head "https://github.com/astrio-ai/l2m.git", branch: "main"

  depends_on "python@3.12"

  resource "importlib-resources" do
    url "https://files.pythonhosted.org/packages/source/i/importlib_resources/importlib_resources-6.1.1.tar.gz"
    sha256 "3893a00122eafde6894c59914446a512f728a0c1a45f9bb9b63721b6bacf0b4a"
  end

  def install
    virtualenv_install_with_resources
  end

  test do
    # Test that l2m can initialize and perform startup activities
    # This exercises the program's initialization logic without requiring user input
    system bin/"l2m", "--exit"
  end
end 
