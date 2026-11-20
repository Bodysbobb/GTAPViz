# GTAPViz: R Package for Automating GTAP Data Processing and Visualization

[![Author](https://img.shields.io/badge/Pattawee.P-blue?label=Author)](https://bodysbobb.github.io/) 
![Last Updated](https://img.shields.io/github/last-commit/Bodysbobb/GTAPViz?label=Last%20Updated&color=blue) 
[![Star](https://img.shields.io/github/stars/Bodysbobb/GTAPViz?style=social)](https://github.com/Bodysbobb/GTAPViz)

[![CRAN](https://www.r-pkg.org/badges/version/GTAPViz)](https://CRAN.R-project.org/package=GTAPViz/)
[![GitHub Version](https://img.shields.io/github/v/tag/Bodysbobb/GTAPViz?label=GitHub%20Version&color=3CB371&sort=semver)](https://github.com/Bodysbobb/GTAPViz/releases/latest)
![License](https://img.shields.io/github/license/Bodysbobb/GTAPViz?color=3CB371)

**Package Help:**  
[![GPT-GTAPViz](https://img.shields.io/badge/GPT--GTAPViz-74aa9c?style=flat-square&logo=openai&logoColor=white)](https://chatgpt.com/g/g-67f87a78396c81919aa2a0676c40e8b3-gtapviz-r/)
[![GTAPViz](https://img.shields.io/badge/GTAPViz-276DC3?style=flat-square&logo=r&logoColor=white)](https://bodysbobb.github.io/GTAPViz/)

---

## **Overview** 

**GTAPViz** is an R package designed to streamline the processing and visualization of GTAP-CGE results. While primarily built for **GTAP simulations, it also supports other `.har` and `.sl4` datasets**. 

With intuitive commands and automated adjustments, users can create, customize, and export high-quality visualizations for journal publications, economic analysis, and research reports—all with minimal coding. 

## **Key Features**  

This package streamlines the creation of figures and tables from `.har` and `.sl4` results, making academic presentations effortless. Some key features are:

- **Effortless Multi-Plot Generation** – Automatically adjusts dimensions, facets, and layout with minimal input.  

- **Smart Plot Adjustments** – Fine-tune visuals easily without manual sizing or layout tweaks.  

- **Dual Export Plot Formats** – Instantly save high-resolution PNG and PDF outputs for slides, papers, and LaTeX. 

- **Publication-Ready Pivot Tables** – Generate clean tables alongside figures—ideal for academic papers.  

- **Streamlined Styling** – Customize colors, fonts, and legends through simple, flexible options.  

- **Powerful Yet Simple** – Built on `ggplot2`, with intuitive `TRUE/FALSE` switches—no advanced coding needed.  

- **Self-Contained Help** – Includes a detailed vignette and internal help—no need to search online.

---

## **Installation**

`GTAPViz` package can be installed directly from CRAN (recommended) using:

```r
install.packages("GTAPViz")
```

---

## **Use GTAPViz with AI (MCP)**

Let an AI app (Claude Desktop, Claude Code, Cursor, ...) run GTAPViz for you. You ask in plain words, it makes the figures and tables on your computer, and it saves the R code it used so you can rerun everything yourself.

### 1. Install (once)

Run in R:

```r
remotes::install_github("Bodysbobb/GTAPViz")
GTAPViz::mcp_install()                  # Claude Desktop
# GTAPViz::mcp_install("claude_code")   # or "cursor"
```

Then restart the AI app.

### 2. Use it

Try the example data that comes with the package:

> Use GTAPViz to load the example data and plot welfare by region

For your own results, give it the folder with your `.sl4` and `.har` files:

> Load my GTAP results in D:/TradeWar/in

Then just ask, for example:

- *"What experiments and regions are there?"*
- *"Plot EV, qgdp and tot by region, with the regions on the vertical axis."*
- *"Show the welfare decomposition, and rename alloc_A1 to 'Allocative efficiency'."*
- *"Which sectors in SEAsia gain the most output? Top 10 only."*
- *"Use the purdue colors, bigger fonts, and also save as PDF."*
- *"Make an Excel table of regional results with experiments as columns."*
- *"Give me the R script."*

The AI tells you where the figures and tables were saved.

### 3. Back in R

Everything the AI does is saved as plain GTAPViz code in `gtapviz_session.R`, in the same output folder. Open it in RStudio and run it to recreate every figure, no AI needed.

You can also chat from inside RStudio: open the **Terminal** tab and run `claude` (Claude Code).

More in the [AI guide](https://pattawee-pp.com/GTAPViz/articles/GTAPViz_AI.html).

---

## **Package Helps**

The most interactive package help is available via [ChatGPT GTAPViz](https://chatgpt.com/g/g-67f87a78396c81919aa2a0676c40e8b3-gtapviz-r/), specifically designed to assist with the GTAPViz package!

For complete instructions on using this package and exploring practical examples:

- [GTAPViz Step-by-Step Guide](https://bodysbobb.github.io/GTAPViz/)  
- [GTAPViz: Plot Catalogs](https://pattawee.shinyapps.io/gtapviz-advanced-plot-configs/)  
- [GTAPViz: Table Catalogs](https://pattawee.shinyapps.io/gtapviz-advanced-table-configs/)

---

## **GTAP Users** 

For GTAP users seeking minimal or no code modifications, please visit [GTAPViz for GTAP Users](https://bodysbobb.github.io/GTAPViz-R-GTAPuser/).

## **Useful Packages for CGE Fellows**

- [HARplus](https://bodysbobb.github.io/HARplus/)

## **License & Author** 

GTAPViz is released under the **MIT License**. See the full **[license](LICENSE)**.  

**Author:**  
**Pattawee Puangchit**  
Ph.D. Candidate, Agricultural Economics  
Purdue University  
Research Assistant at GTAP  
