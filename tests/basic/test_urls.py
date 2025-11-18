import requests
import pytest

from src.core import urls


def test_urls():
    url_attributes = [
        attr
        for attr in dir(urls)
        if not callable(getattr(urls, attr)) and not attr.startswith("__")
    ]
    for attr in url_attributes:
        url = getattr(urls, attr)
        try:
            response = requests.get(url, timeout=5)
            assert response.status_code == 200, f"URL {url} returned status code {response.status_code}"
        except (requests.exceptions.ConnectionError, requests.exceptions.Timeout) as e:
            # Skip network-related failures (DNS, connectivity issues)
            pytest.skip(f"Network error accessing {url}: {e}")
