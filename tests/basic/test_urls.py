import requests
import pytest

from src.core import urls


def test_urls():
    """Test that key URLs are accessible."""
    # Only test URLs that should always exist
    test_urls = [
        urls.website,
        urls.github_repo,
        urls.github_readme,
    ]
    for url in test_urls:
        try:
            response = requests.get(url, timeout=5)
            assert response.status_code == 200, f"URL {url} returned status code {response.status_code}"
        except (requests.exceptions.ConnectionError, requests.exceptions.Timeout) as e:
            pytest.skip(f"Network error accessing {url}: {e}")
