# Security Policy

## Supported Versions

We release patches for security vulnerabilities. Which versions are eligible for receiving such patches depends on the CVSS v3.0 Rating:

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |

## Reporting a Vulnerability

Please report (suspected) security vulnerabilities to **[naingoolwin.astrio@gmail.com](mailto:naingoolwin.astrio@gmail.com)**. You will receive a response within 48 hours. If the issue is confirmed, we will release a patch as soon as possible depending on complexity but historically within a few days.

## Security Best Practices

When using L2M:

1. **API Keys**: Never commit your OpenAI API key to version control. Use environment variables or `.env` files (which are gitignored).

2. **Input Validation**: Always validate COBOL input files before processing. The system includes guardrails, but you should also validate files from untrusted sources.

3. **Output Validation**: Review generated Python code before executing it, especially in production environments.

4. **Session Data**: Session databases may contain sensitive information. Ensure proper access controls on `data/sessions.db`.

5. **Dependencies**: Keep dependencies up to date. Run `pip install -r requirements.txt --upgrade` regularly.

## Known Security Considerations

- **Code Execution**: Generated Python code is executed by the Tester Agent. Review test code before execution.
- **File System Access**: The system reads COBOL files and writes output files. Ensure proper file permissions.
- **API Communication**: All communication with OpenAI API uses HTTPS. Ensure your network connection is secure.

## Disclosure Policy

We follow responsible disclosure practices:

1. Report the vulnerability privately to maintainers
2. Allow reasonable time for fixes before public disclosure
3. Credit researchers who responsibly disclose vulnerabilities
4. Provide clear security advisories for fixed vulnerabilities

