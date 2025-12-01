import os
from unittest import TestCase
from unittest.mock import MagicMock, patch

from prompt_toolkit.input import DummyInput
from prompt_toolkit.output import DummyOutput

from cli.main import main


class TestSSLVerification(TestCase):
    def setUp(self):
        self.original_env = os.environ.copy()
        os.environ["OPENAI_API_KEY"] = "test-key"
        os.environ["ATLAS_CHECK_UPDATE"] = "false"
        os.environ["ATLAS_ANALYTICS"] = "false"

    def tearDown(self):
        os.environ.clear()
        os.environ.update(self.original_env)

    @patch("cli.io.InputOutput.offer_url")
    @patch("src.core.models.ModelInfoManager.set_verify_ssl")
    @patch("src.core.llm.litellm._load_litellm")
    @patch("httpx.Client")
    @patch("httpx.AsyncClient")
    @patch("src.core.models.fuzzy_match_models", return_value=[])
    def test_no_verify_ssl_flag_sets_model_info_manager(
        self,
        mock_fuzzy_match,
        mock_async_client,
        mock_client,
        mock_load_litellm,
        mock_set_verify_ssl,
        mock_offer_url,
    ):
        # Prevent actual URL opening
        mock_offer_url.return_value = False
        # Mock the litellm._lazy_module to avoid AttributeError
        mock_load_litellm.return_value = None
        mock_module = MagicMock()

        # Mock Model class to avoid actual model initialization
        with patch("src.core.models.Model") as mock_model:
            # Configure the mock to avoid the TypeError
            mock_model.return_value.info = {}
            mock_model.return_value.validate_environment.return_value = {
                "missing_keys": [],
                "keys_in_environment": [],
            }

            with patch("src.core.llm.litellm._lazy_module", mock_module):
                # Run main with --no-verify-ssl flag
                main(
                    ["--no-verify-ssl", "--exit", "--yes"],
                    input=DummyInput(),
                    output=DummyOutput(),
                )

                # Verify model_info_manager.set_verify_ssl was called with False
                mock_set_verify_ssl.assert_called_once_with(False)

                # Verify httpx clients were created with verify=False
                mock_client.assert_called_once_with(verify=False)
                mock_async_client.assert_called_once_with(verify=False)

                # Verify SSL_VERIFY environment variable was set to empty string
                self.assertEqual(os.environ.get("SSL_VERIFY"), "")

    @patch("cli.io.InputOutput.offer_url")
    @patch("src.core.models.model_info_manager.set_verify_ssl")
    def test_default_ssl_verification(self, mock_set_verify_ssl, mock_offer_url):
        # Prevent actual URL opening
        mock_offer_url.return_value = False
        # Run main without --no-verify-ssl flag
        with patch("cli.main.InputOutput"):
            with patch("src.coders.Coder.create"):
                main(["--exit", "--yes"], input=DummyInput(), output=DummyOutput())

                # Verify model_info_manager.set_verify_ssl was not called
                mock_set_verify_ssl.assert_not_called()

                # Verify SSL_VERIFY environment variable was not set
                self.assertNotIn("SSL_VERIFY", os.environ)
