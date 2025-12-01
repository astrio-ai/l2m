#!/usr/bin/env python

import re
import sys

import pypandoc

from src import __version__
from src.core import urls
from src import utils
from src.utils.dump import dump  # noqa: F401

atlas_user_agent = f"Atlas/{__version__} +{urls.website}"

# Playwright is nice because it has a simple way to install dependencies on most
# platforms.

def check_env():
    try:
        from playwright.sync_api import sync_playwright

        has_pip = True
    except ImportError:
        has_pip = False

    try:
        with sync_playwright() as p:
            p.chromium.launch()
            has_chromium = True
    except Exception:
        has_chromium = False

    return has_pip, has_chromium


def has_playwright():
    has_pip, has_chromium = check_env()
    return has_pip and has_chromium


def install_playwright(io):
    has_pip, has_chromium = check_env()
    if has_pip and has_chromium:
        return True

    pip_cmd = utils.get_pip_install(["atlas[playwright]"])
    chromium_cmd = "-m playwright install --with-deps chromium"
    chromium_cmd = [sys.executable] + chromium_cmd.split()

    cmds = ""
    if not has_pip:
        cmds += " ".join(pip_cmd) + "\n"
    if not has_chromium:
        cmds += " ".join(chromium_cmd) + "\n"

    text = f"""For the best web scraping, install Playwright:

{cmds}
See {urls.enable_playwright} for more info.
"""

    io.tool_output(text)
    if not io.confirm_ask("Install playwright?", default="y"):
        return

    if not has_pip:
        success, output = utils.run_install(pip_cmd)
        if not success:
            io.tool_error(output)
            return

    success, output = utils.run_install(chromium_cmd)
    if not success:
        io.tool_error(output)
        return

    return True


class Scraper:
    pandoc_available = None
    playwright_available = None
    playwright_instructions_shown = False

    # Public API...
    def __init__(self, print_error=None, playwright_available=None, verify_ssl=True):
        """
        `print_error` - a function to call to print error/debug info.
        `verify_ssl` - if False, disable SSL certificate verification when scraping.
        """
        if print_error:
            self.print_error = print_error
        else:
            self.print_error = print

        self.playwright_available = playwright_available
        self.verify_ssl = verify_ssl

    def scrape(self, url):
        """
        Scrape a url and turn it into readable markdown if it's HTML.
        If it's plain text or non-HTML, return it as-is.

        `url` - the URL to scrape.
        """

        if self.playwright_available:
            content, mime_type = self.scrape_with_playwright(url)
        else:
            content, mime_type = self.scrape_with_httpx(url)

        if not content:
            # Error already printed by scraping method, just return None
            return None

        # Check if the content is HTML based on MIME type or content
        if (mime_type and mime_type.startswith("text/html")) or (
            mime_type is None and self.looks_like_html(content)
        ):
            self.try_pandoc()
            content = self.html_to_markdown(content)

        return content

    def looks_like_html(self, content):
        """
        Check if the content looks like HTML.
        """
        if isinstance(content, str):
            # Check for common HTML tags
            html_patterns = [
                r"<!DOCTYPE\s+html",
                r"<html",
                r"<head",
                r"<body",
                r"<div",
                r"<p>",
                r"<a\s+href=",
            ]
            return any(re.search(pattern, content, re.IGNORECASE) for pattern in html_patterns)
        return False

    # Internals...
    def scrape_with_playwright(self, url):
        """
        Scrape a URL using Playwright, specifically handling dynamic content.
        """
        import playwright  # noqa: F401
        from playwright.sync_api import Error as PlaywrightError
        from playwright.sync_api import TimeoutError as PlaywrightTimeoutError
        from playwright.sync_api import sync_playwright

        with sync_playwright() as p:
            try:
                browser = p.chromium.launch()
            except Exception as e:
                self.playwright_available = False
                self.print_error(str(e))
                return None, None

            try:
                context = browser.new_context(ignore_https_errors=not self.verify_ssl)
                page = context.new_page()

                user_agent = page.evaluate("navigator.userAgent")
                user_agent = user_agent.replace("Headless", "")
                user_agent = user_agent.replace("headless", "")
                user_agent += " " + atlas_user_agent

                page.set_extra_http_headers({"User-Agent": user_agent})

                # Navigate to the page and wait for it to load
                response = None
                try:
                    response = page.goto(url, wait_until="domcontentloaded", timeout=10000)
                except PlaywrightTimeoutError:
                    print(f"Page didn't quiesce, scraping content anyway: {url}")
                    response = None
                except PlaywrightError as e:
                    # Make error messages more user-friendly
                    error_str = str(e)
                    if "ERR_NAME_NOT_RESOLVED" in error_str or "net::ERR_NAME_NOT_RESOLVED" in error_str:
                        self.print_error(f"Cannot find website: {url}")
                        self.print_error("Please check the URL spelling and try again.")
                    elif "ERR_CERT_COMMON_NAME_INVALID" in error_str or "ERR_CERT_AUTHORITY_INVALID" in error_str:
                        self.print_error(f"SSL certificate error: {url}")
                        self.print_error("The website's security certificate is invalid or the URL may be misspelled.")
                    elif "ERR_CONNECTION_REFUSED" in error_str:
                        self.print_error(f"Connection refused: {url}")
                        self.print_error("The website may be down or blocking requests.")
                    elif "ERR_TIMED_OUT" in error_str:
                        self.print_error(f"Connection timed out: {url}")
                        self.print_error("The website is taking too long to respond.")
                    elif "ERR_CONNECTION_CLOSED" in error_str:
                        self.print_error(f"Connection closed: {url}")
                        self.print_error("The website closed the connection unexpectedly.")
                    elif "ERR_SSL_PROTOCOL_ERROR" in error_str:
                        self.print_error(f"SSL protocol error: {url}")
                        self.print_error("Cannot establish a secure connection to the website.")
                    else:
                        self.print_error(f"Cannot access website: {url}")
                        self.print_error("The URL may be incorrect or the website may be unavailable.")
                    return None, None

                try:
                    content = page.content()
                    mime_type = None
                    if response:
                        content_type = response.header_value("content-type")
                        if content_type:
                            mime_type = content_type.split(";")[0]
                except PlaywrightError as e:
                    self.print_error(f"Error retrieving page content: {str(e)}")
                    content = None
                    mime_type = None
            finally:
                browser.close()

        return content, mime_type

    def scrape_with_httpx(self, url):
        import httpx

        headers = {"User-Agent": f"Mozilla./5.0 ({atlas_user_agent})"}
        try:
            with httpx.Client(
                headers=headers, verify=self.verify_ssl, follow_redirects=True
            ) as client:
                response = client.get(url)
                response.raise_for_status()
                return response.text, response.headers.get("content-type", "").split(";")[0]
        except httpx.HTTPError as http_err:
            self.print_error(f"HTTP error occurred: {http_err}")
        except Exception as err:
            self.print_error(f"An error occurred: {err}")
        return None, None

    def try_pandoc(self):
        if self.pandoc_available is not None:
            return

        try:
            pypandoc.get_pandoc_version()
            self.pandoc_available = True
            return
        except OSError:
            pass

        # Don't try to auto-install pandoc if SSL verification might fail
        # Just mark as unavailable and use basic HTML parsing instead
        try:
            pypandoc.download_pandoc(delete_installer=True)
            self.pandoc_available = True
        except Exception as err:
            # Silently fail - we'll use BeautifulSoup for HTML parsing
            # Only show error in verbose mode to avoid cluttering output
            if "CERTIFICATE_VERIFY_FAILED" in str(err) or "SSL" in str(err):
                # SSL errors are common on macOS, just use fallback
                self.pandoc_available = False
            else:
                self.print_error(f"Unable to install pandoc: {err}")
                self.print_error("Falling back to basic HTML parsing.")
                self.pandoc_available = False

    def html_to_markdown(self, page_source):
        from bs4 import BeautifulSoup

        soup = BeautifulSoup(page_source, "html.parser")
        soup = slimdown_html(soup)
        page_source = str(soup)

        if not self.pandoc_available:
            return page_source

        try:
            md = pypandoc.convert_text(page_source, "markdown", format="html")
        except OSError:
            return page_source

        md = re.sub(r"</div>", "      ", md)
        md = re.sub(r"<div>", "     ", md)

        md = re.sub(r"\n\s*\n", "\n\n", md)

        return md


def slimdown_html(soup):
    for svg in soup.find_all("svg"):
        svg.decompose()

    if soup.img:
        soup.img.decompose()

    for tag in soup.find_all(href=lambda x: x and x.startswith("data:")):
        tag.decompose()

    for tag in soup.find_all(src=lambda x: x and x.startswith("data:")):
        tag.decompose()

    for tag in soup.find_all(True):
        for attr in list(tag.attrs):
            if attr != "href":
                tag.attrs.pop(attr, None)

    return soup


def main(url):
    scraper = Scraper(playwright_available=has_playwright())
    content = scraper.scrape(url)
    print(content)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python playw.py <URL>")
        sys.exit(1)
    main(sys.argv[1])
