import os
import unittest
from unittest.mock import patch

from cli.main import main


class TestBrowser(unittest.TestCase):
    @patch("cli.main.launch_gui")
    @patch("cli.main.check_streamlit_install", return_value=True)
    def test_browser_flag_imports_streamlit(self, mock_check_streamlit, mock_launch_gui):
        os.environ["L2M_ANALYTICS"] = "false"

        # Run main with --browser and --yes flags
        main(["--browser", "--yes"])

        # Check that launch_gui was called
        mock_launch_gui.assert_called_once()

        # Note: We can't actually test streamlit import here because we're mocking check_streamlit_install
        # The test verifies that launch_gui is called when --browser flag is used and streamlit is available


if __name__ == "__main__":
    unittest.main()
