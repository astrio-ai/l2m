# Multi-File Website Modernization Examples

This directory contains examples demonstrating how Legacy2Modern handles entire static website codebases, not just single HTML files.

## Supported Input Types

The enhanced system now supports:

### 1. **Local Directory** 📂
```
legacy2modern website /path/to/website/directory output/react-website
```

### 2. **ZIP Archive** 📦
```
legacy2modern website website-backup.zip output/react-website
```

### 3. **Git Repository** 🔗
```
legacy2modern website https://github.com/user/legacy-website output/react-website
```

### 4. **Single HTML File** 📄 (for backward compatibility)
```
legacy2modern website index.html output/react-website
```

## Example Scenarios

### Scenario 1: Multi-Page Business Website
```
legacy-website/
├── index.html          # Homepage
├── about.html          # About page
├── services.html       # Services page
├── contact.html        # Contact page
├── css/
│   ├── style.css       # Main stylesheet
│   └── bootstrap.css   # Bootstrap framework
├── js/
│   ├── main.js         # Main JavaScript
│   └── jquery.js       # jQuery library
└── images/
    ├── logo.png
    ├── hero-bg.jpg
    └── team-photos/
```

**Command:**
```bash
legacy2modern website ./legacy-website output/modern-website
```

**What the system analyzes:**
- ✅ Site structure and navigation
- ✅ Shared components across pages
- ✅ Asset dependencies and optimization
- ✅ Cross-page linking patterns
- ✅ Component reusability opportunities

### Scenario 2: E-commerce Website Archive
```
ecommerce-site.zip
├── index.html
├── products/
│   ├── product-list.html
│   ├── product-detail.html
│   └── category.html
├── cart/
│   ├── cart.html
│   └── checkout.html
├── user/
│   ├── login.html
│   ├── register.html
│   └── profile.html
├── assets/
│   ├── css/
│   ├── js/
│   └── images/
└── includes/
    ├── header.php
    ├── footer.php
    └── navigation.php
```

**Command:**
```bash
legacy2modern website ecommerce-site.zip output/modern-ecommerce
```

**What the system analyzes:**
- ✅ Archive structure and file hierarchy
- ✅ PHP includes and dependencies
- ✅ E-commerce specific patterns
- ✅ User flow and navigation complexity
- ✅ Asset optimization opportunities

### Scenario 3: GitHub Repository
```
https://github.com/company/legacy-corporate-site
├── main branch
│   ├── index.html
│   ├── about/
│   ├── services/
│   ├── blog/
│   ├── contact/
│   └── assets/
└── development branch
    └── (additional features)
```

**Command:**
```bash
legacy2modern website https://github.com/company/legacy-corporate-site output/modern-corporate
```

**What the system analyzes:**
- ✅ Git repository metadata
- ✅ Branch structure and history
- ✅ Repository size and complexity
- ✅ Collaborative development patterns
- ✅ Version control integration opportunities

## Enhanced Analysis Features

### 1. **Site Architecture Analysis**
- Total pages and structure
- URL patterns and SEO friendliness
- Content organization across pages
- Architecture score and recommendations

### 2. **Cross-Page Dependencies**
- Shared components identification
- Page linking patterns
- Asset dependencies
- Optimization opportunities

### 3. **Navigation Complexity**
- Menu structure analysis
- Link patterns and accessibility
- Mobile friendliness assessment
- Navigation recommendations

### 4. **Performance Analysis**
- Asset loading patterns
- Resource optimization opportunities
- Caching strategies
- Performance score and recommendations

### 5. **Component Reusability**
- Reusable component identification
- Component pattern recognition
- Extraction opportunities
- Component library planning

### 6. **Asset Optimization**
- CSS/JS bundling opportunities
- Image optimization recommendations
- Font loading optimization
- Compression strategies

## Modernization Strategy

The system generates a comprehensive modernization strategy including:

### **Phases**
1. **Analysis & Planning** - Deep analysis of current state
2. **Component Extraction** - Identify and extract reusable components
3. **Framework Migration** - Convert to modern framework
4. **Asset Optimization** - Optimize and bundle assets
5. **Testing & Deployment** - Quality assurance and deployment

### **Priorities**
- High-impact, low-effort improvements
- Critical user experience enhancements
- Performance optimizations
- Accessibility improvements

### **Timeline Estimation**
- Based on complexity analysis
- Resource requirements
- Risk assessment
- Success metrics definition

## Output Structure

The modernized website will include:

```
modern-website/
├── src/
│   ├── components/          # Reusable components
│   ├── pages/              # Page components
│   ├── layouts/            # Layout components
│   ├── hooks/              # Custom React hooks
│   ├── utils/              # Utility functions
│   └── styles/             # Styled components
├── public/
│   ├── images/             # Optimized images
│   ├── fonts/              # Web fonts
│   └── assets/             # Other assets
├── package.json            # Dependencies
├── README.md               # Setup instructions
└── modernized-report.md    # Detailed modernization report
```

## CLI Commands

### Analyze Only (No Code Generation)
```bash
legacy2modern analyze website ./legacy-website
```

### Modernize with Specific Framework
```bash
legacy2modern website ./legacy-website output/react-website --framework react
legacy2modern website ./legacy-website output/astro-website --framework astro
legacy2modern website ./legacy-website output/nextjs-website --framework nextjs
```

### Interactive Mode
```bash
legacy2modern
# Then use: /website ./legacy-website output/modern-website
```

## Benefits of Multi-File Analysis

1. **Better Component Extraction** - Identifies shared components across pages
2. **Improved Navigation** - Understands site structure and user flows
3. **Asset Optimization** - Consolidates and optimizes shared assets
4. **Performance Insights** - Analyzes loading patterns and bottlenecks
5. **Modernization Strategy** - Provides phased approach with priorities
6. **Risk Assessment** - Identifies potential issues and mitigation strategies

## Real-World Use Cases

- **Corporate Websites** - Multi-page business sites with complex navigation
- **E-commerce Platforms** - Product catalogs with user flows
- **Blog/News Sites** - Content-heavy sites with article templates
- **Portfolio Sites** - Creative portfolios with project showcases
- **Educational Sites** - Course catalogs and learning management systems

The enhanced system transforms the modernization process from a simple file conversion to a comprehensive website transformation with intelligent analysis and strategic planning. 