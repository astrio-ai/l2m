# OpenLegacy — AI-Powered Transpilation and Refactoring Toolkit

<div align="center">

<!-- Keep the gap above this line, otherwise they won't render correctly! -->
[![GitHub Repo stars](https://img.shields.io/github/stars/astrio-ai/openlegacy)](https://github.com/astrio-ai/openlegacy) 
[![Join us on Discord](https://img.shields.io/discord/1396038465002405948?label=Discord&logo=discord&logoColor=white)](https://discord.gg/2BVwAUzW)
[![Contributing Guide](https://img.shields.io/badge/Contributing-Guide-informational)](https://github.com/openrewrite/.github/blob/main/CONTRIBUTING.md)
</div>

Welcome to **OpenLegacy**, an open-source engine for transforming legacy source code into modern, maintainable software.

Unlike traditional refactoring tools, openlegacy supports **both intelligent transpilation (e.g. COBOL → Python)** and **rule-based structural refactoring**. It combines the speed of automation with the accuracy of AST and rule-driven logic, optionally augmented by Large Language Models (LLMs).

Whether you’re modernizing back-end systems, migrating business logic to the cloud, or refactoring decades-old enterprise code — openlegacy is built to help you do it **safely**, **scalably**, and **transparently**.

## ✨ Features

* 🔁 **Transpile Legacy Code to Modern Languages**  
  Translate legacy code (e.g., COBOL, Java 6, C) into modern languages like Python, JavaScript, or Java 17.

* 🧱 **Rule-Based Structural Refactoring Engine**  
  Built-in and customizable rules for refactoring patterns (e.g., flattening nested logic, extracting functions, simplifying control flow).

* 🤖 **LLM-Augmented Suggestions (Optional)**  
  When enabled, integrate LLMs (like GPT-4 or Claude) to improve code clarity, generate comments, or propose transformations.

* 🛠️ **Modular Architecture**  
  Easily swap out parser frontends, transformation pipelines, or generation backends.

* 📜 **AST and Token-Level Inspection**  
  Hybrid processing supports both rule-level and syntax-tree-level operations.

* 🧪 **Test Harness for Rule Validations**  
  Write unit and integration tests to validate each transformation.

## 🚀 Quickstart

### Prerequisites

- Node.js (v18+)
- Python 3.10+ (for rule-based transpiler backend)
- [Optional] OpenAI API key (for LLM integration)

### 1. Clone the Repo

```bash
git clone https://github.com/Astrio/openlegacy.git
cd openlegacy
```

### 2. Install Dependencies

```bash
# Frontend/CLI
cd packages/cli
npm install

# Backend transpiler
cd ../../packages/core
pip install -r requirements.txt
```
### 3. Run a Simple Transpile

```bash
# From the CLI
openlegacy transpile examples/accounting_system.cob --target=python
```

### 4. Run Refactoring Only

```bash
openlegacy refactor examples/api_v1.py
```

## 📄 License
This project is licensed under the Apache-2.0 License. See the [LICENSE](./LICENSE) file for details.

## 🤝 Contributing
We welcome all contributions — from fixing typos to writing new transformation rules!
See [CONTRIBUTING.md](./CONTRIBUTION.md) for setup instructions, coding guidelines, and how to submit PRs.

### Good First Issues
* Add a new refactor rule
* Test transpilation from Java to Python
* Improve LLM error fallback logic

## 💬 Community & Support
* 📢 Follow our project updates on [X](https://x.com/nolan-lwin)
* 👾 Join our [Discord](https://discord.gg/2BVwAUzW)
* 🧑‍💻 Join the discussion: [GitHub Discussions](https://github.com/astrio-ai/openlegacy/discussions)
* 🧪 Report bugs: [GitHub Issues](https://github.com/astrio-ai/openlegacy/issues)

## 📬 Contact Us
For partnership inquiries or professional use cases:

📧 **[naingoolwin.astrio@gmail.com](mailto:naingoolwin.astrio@gmail.com)**
