<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [DelphiMVCFramework](#delphimvcframework)
  - [Star History](#star-history)
  - [🚀 Quick Links](#-quick-links)
  - [🎯 Key Features](#-key-features)
    - [🏗️ **Architecture & Framework**](#-architecture--framework)
    - [🔐 **Security & Authentication**](#-security--authentication)
    - [💾 **Data Access & ORM**](#-data-access--orm)
    - [🌐 **Web & API Features**](#-web--api-features)
    - [📊 **Data Serialization**](#-data-serialization)
    - [🛠️ **Development & Deployment**](#-development--deployment)
  - [🖥️ **Platform Support**](#-platform-support)
    - [Delphi Versions](#delphi-versions)
    - [Operating Systems](#operating-systems)
    - [Deployment Targets](#deployment-targets)
  - [🔒 **Security & TLS 1.3 Support**](#-security--tls-13-support)
  - [🏁 **Quick Start**](#-quick-start)
  - [📚 **Learning Resources**](#-learning-resources)
    - [📖 **Official Guide**](#-official-guide)
    - [📘 **Technical Guides & Papers**](#-technical-guides--papers)
    - [🎓 **Training & Support**](#-training--support)
  - [🤝 **Community & Support**](#-community--support)
    - [💬 **Get Help**](#-get-help)
    - [🎯 **Support the Project**](#-support-the-project)
    - [👥 **Contributors**](#-contributors)
  - [🏆 **Success Stories**](#-success-stories)
  - [🏢 **Sponsors**](#-sponsors)
  - [📄 **License**](#-license)
  - [🔗 **Quick Navigation**](#-quick-navigation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# DelphiMVCFramework
[![GitHub All Releases](https://img.shields.io/github/downloads/danieleteti/delphimvcframework/total?label=Downloads)](https://github.com/danieleteti/delphimvcframework/releases)
[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/danieleteti/delphimvcframework)
[![Mentioned in Awesome](https://awesome.re/mentioned-badge.svg)](https://github.com/sindresorhus/awesome)

![](https://img.shields.io/badge/stable-dmvcframework--3.4.3--aluminium-blue)
![](https://img.shields.io/badge/beta-dmvcframework--3.5.0--silicon-orange)

**The most popular Delphi RESTful framework on GitHub**

DelphiMVCFramework is a powerful, open-source framework for building RESTful services, JSON-RPC APIs, and web applications with Object Pascal. It provides a complete MVC architecture with built-in ORM, authentication, TLS 1.3 support and extensive middleware support.

## Star History

<a href="https://www.star-history.com/?repos=danieleteti%2Fdelphimvcframework&type=date&legend=top-left">
 <picture>
   <source media="(prefers-color-scheme: dark)" srcset="https://api.star-history.com/image?repos=danieleteti/delphimvcframework&type=date&theme=dark&legend=top-left" />
   <source media="(prefers-color-scheme: light)" srcset="https://api.star-history.com/image?repos=danieleteti/delphimvcframework&type=date&legend=top-left" />
   <img alt="Star History Chart" src="https://api.star-history.com/image?repos=danieleteti/delphimvcframework&type=date&legend=top-left" />
 </picture>
</a>

## 🚀 Quick Links

- **[📖 Official Guide](http://www.danieleteti.it/books/)** - Comprehensive documentation and tutorials
- **[⚡ Quick Start Guide](docs/quickstart_guide.md)** - Get up and running in 5 minutes
- **[📦 Installation](docs/installation_guide.md)** - Step-by-step installation instructions
- **[🎯 Samples](docs/samples_guide.md)** - 40+ examples covering all features
- **[📝 Changelog](docs/changelog.md)** - Version history and release notes

## 🎯 Key Features

### 🏗️ **Architecture & Framework**
- **RESTful compliant** (Richardson Maturity Model Level 3)
- **JSON-RPC 2.0 support** with automatic object remotization
- **MVC pattern implementation** with controller inheritance
- **Middleware system** for request/response processing
- **Dependency injection** support
- **Domain modeling first** approach

### 🔐 **Security & Authentication**
- **JWT (JSON Web Token)** authentication and authorization
- **HTTP Basic Authentication** support
- **Custom authentication** mechanisms
- **CORS (Cross-Origin Resource Sharing)** handling
- **TLS 1.3 support** for secure communications
- **JWT Blacklist middleware** for token revocation

### 💾 **Data Access & ORM**
- **MVCActiveRecord** - Powerful ORM with full CRUD operations
- **Multi-database support**: PostgreSQL, MySQL, MariaDB, Firebird, InterBase, SQLite, Microsoft SQL Server
- **RQL (Resource Query Language)** for flexible querying
- **Named queries** support for optimized database access
- **Connection pooling** and transaction management
- **Automatic entity generation** from database schema

### 🌐 **Web & API Features**
- **WebSocket support** for real-time bidirectional communication (server & client)
- **Content negotiation** with multiple MIME types
- **Server-Side Views** with Mustache and TemplatePro support
- **Static file serving** middleware
- **File upload/download** handling
- **Server-Sent Events (SSE)** for real-time updates
- **HATEOAS** (Hypermedia as the Engine of Application State) support
- **OpenAPI/Swagger** documentation generation
- **Compression** support (gzip, deflate)
- **Rate limiting** with in-memory and Redis-backed implementations

### 📊 **Data Serialization**
- **Flexible JSON serialization/deserialization**
- **Custom serializers** for complex types
- **Nullable types** support
- **Dataset to JSON** conversion
- **Multiple naming conventions** (camelCase, PascalCase, snake_case, etc.)
- **Attribute-based** field mapping

### 🛠️ **Development & Deployment**
- **IDE Wizard** for project creation
- **Comprehensive logging** with LoggerPro integration
- **Built-in profiler** for performance monitoring
- **Unit testing framework** (250+ tests)
- **dotEnv configuration** support
- **Multiple deployment options**: Standalone, Apache module, IIS ISAPI, Linux daemon

## 🖥️ **Platform Support**

### Delphi Versions
- **Delphi 13 Florence** ✅
- **Delphi 12.x Athens** ✅
- **Delphi 11.x Alexandria** ✅
- **Delphi 10.4 Sydney** ✅
- **Delphi 10.3 Rio** ✅
- **Delphi 10.2 Tokyo** ✅
- **Delphi 10.1 Berlin** ✅
- **Delphi 10 Seattle** ✅

### Operating Systems
- **Windows** (32-bit and 64-bit)
- **Linux** (64-bit)
- **Android** (experimental support)

### Deployment Targets
- Console Applications
- Windows Services
- Linux Daemons
- Windows VCL Applications
- Windows or Linux FMX Applications
- Apache Modules (Windows/Linux)
- IIS ISAPI Extensions (Windows)

## 🔒 **Security & TLS 1.3 Support**

DelphiMVCFramework provides enterprise-grade security features including full **TLS 1.3 support**. The framework automatically negotiates the highest available TLS version and provides:

- **Perfect Forward Secrecy** with modern cipher suites
- **Certificate validation** and custom certificate handling
- **SNI (Server Name Indication)** support for multiple SSL certificates
- **HTTP Strict Transport Security (HSTS)** headers
- **Secure cookie** handling with SameSite attributes
- **CSRF protection** mechanisms

TLS 1.3 brings significant security and performance improvements, including faster handshakes and stronger encryption algorithms. DelphiMVCFramework leverages these improvements automatically when deployed in compatible environments.

## 🏁 **Quick Start**

Here's how a simple DMVCFramework controller looks like:

```pascal
// 1. Create a controller
[MVCPath('/api/hello')]
TMyController = class(TMVCController)
public
  [MVCPath('/world')]
  [MVCHTTPMethod([httpGET])]
  function HelloWorld: TMyObject;
end;

// 2. Implement the action
function TMyController.HelloWorld: TMyObject;
begin
  Result := TMyObject.Create('Hello World!');
end;

// 3. Register and start server elsewhere
FMVC := TMVCEngine.Create(Self);
FMVC.AddController(TMyController);
```

**[👉 See full Quick Start Guide](docs/quickstart_guide.md)**

## 📚 **Learning Resources**

### 📖 **Official Guide**
The comprehensive **"DelphiMVCFramework - The Official Guide"** is available in multiple languages:
- [🇬🇧 English](https://leanpub.com/delphimvcframework) (eBook & Hardcover)
- [🇧🇷 Portuguese](https://leanpub.com/delphimvcframework-br)
- [🇪🇸 Spanish](https://leanpub.com/delphimvcframework-es)

### 📘 **Technical Guides & Papers**
Premium guides and video tutorials available at the [DMVCFramework Patreon Shop](https://www.danieleteti.it/patreon-products/):
- **TemplatePro 1.1 - The Definitive Guide** (EN, IT, ES, DE)
- **A Practical Guide to Managing Complex Configurations with .env** (EN, IT)
- **Prompt Engineering - Mastering AI Communication** (EN, IT, ES, DE)
- **Building a Robust Job Queue System with FirebirdSQL** (EN, IT, ES)
- **Practical Guide to API and Webhooks with DelphiMVCFramework** (IT, ES)
- **Understanding JSON-RPC: A Lightweight Remote Procedure Call Protocol** (EN)
- **CRUD Web API with Delphi, DMVCFramework, and PostgreSQL** (EN)
- **MVCActiveRecord Series** - Complete ORM guide in 3 parts (EN, IT)
- **Pagination with RQL in DMVCFramework** (EN)
- **Using Caddy as Reverse Proxy for DMVCFramework Applications** (EN, IT, ES)
- **Localization in DMVCFramework Web Applications** (Video)
- **Sessions in DMVCFramework** (Video)
- **Bag of Words in Delphi** - NLP guide (EN, IT) — Free for Patreon members

### 🎓 **Training & Support**
- **Professional Training**: Available through [bit Time Professionals](https://www.bittimeprofessionals.it) (world wide) and [Delphi Studio ES](https://www.delphistudio.es) (in Spain)
- **Consultancy Services**: Custom development and technical support available through [bit Time Professionals](https://www.bittimeprofessionals.it) (world wide) and [Delphi Studio ES](https://www.delphistudio.es) (in Spain)
- **Community Support**: [Facebook Group](https://www.facebook.com/groups/delphimvcframework) (6000+ members)

## 🤝 **Community & Support**

### 💬 **Get Help**
- [Facebook Group](https://www.facebook.com/groups/delphimvcframework) - Active community with 6000+ members
- [GitHub Issues](https://github.com/danieleteti/delphimvcframework/issues) - Bug reports and feature requests
- [GitHub Discussions](https://github.com/danieleteti/delphimvcframework/discussions) - General questions and discussions

### 🎯 **Support the Project**
- ⭐ **Star this repository** - Help others discover DMVCFramework
- 🐛 **Report bugs** - Help improve the framework
- 📝 **Contribute documentation** - Share your knowledge
- 💰 **[Become a Patron](https://www.patreon.com/delphimvcframework)** - Access to premium contents, videos, article etc. reserved to supporters

### 👥 **Contributors**
<a href="https://github.com/danieleteti/delphimvcframework/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=danieleteti/delphimvcframework" />
</a>

## 🏆 **Success Stories**

> *"DMVCFramework is a great framework. It's very intuitive, fast, easy to use, actually there is nothing more to ask for."* - Samir

> *"I'm still amazed by the DelphiMVCFramework code and documentation. Thank you very much and I am amazed by your quick feedback."* - [Benjamin Yang](https://www.linkedin.com/in/benjamin-yang-4b0609159/), Director of SQLGate

> *"We started the process of migrating our systems to micro services and are loving the DMVCFramework."* - E. Costa


## 🏢 **Sponsors**

DelphiMVCFramework is proudly sponsored by:

| Gold Sponsors |
|---|
| [bit Time Professionals](https://www.bittimeprofessionals.com) |
| [bit Time Software](https://www.bittime.it) |

| Silver Sponsors |
|---|
| [Centro Software](https://www.centrosoftware.com) |
| [Delphi Studio ES](http://www.delphistudio.es) |
| [Orion Law](https://orionlaw.com/) |
| [Vivaticket](https://www.vivaticket.com/) |

## 📄 **License**

DelphiMVCFramework is released under the Apache License 2.0. See [LICENSE](LICENSE) file for details.

## 🔗 **Quick Navigation**

| Documentation | Community |
|---|---|
| [Quick Start](docs/quickstart_guide.md) | [Facebook Group](https://www.facebook.com/groups/delphimvcframework) |
| [Installation](docs/installation_guide.md) | [GitHub Discussions](https://github.com/danieleteti/delphimvcframework/discussions) |
| [Samples](docs/samples_guide.md) | [X](https://x.com/danieleteti) |
| [RQL Guide](docs/rql_guide.md) | [Blog](https://www.danieleteti.it) |
| [dotEnv](docs/dotenv_guide.md) |  |

---

**Made with ❤️ by [Daniele Teti](https://www.danieleteti.it) and the [community](https://github.com/danieleteti/delphimvcframework/graphs/contributors)**
