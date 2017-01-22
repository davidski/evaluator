Evaluator README
================
David F. Severski

-   [Summary](#summary)
-   [Background](#background)
-   [How to Use](#how-to-use)
    -   [Instructions](#instructions)
        -   [Preparing the Environment](#preparing-the-environment)
        -   [Defining Your Security Domains](#defining-your-security-domains)
        -   [Defining Your Controls and Risk Scenarios](#defining-your-controls-and-risk-scenarios)
        -   [Importing the Scenarios](#importing-the-scenarios)
        -   [Running the Simulations](#running-the-simulations)
        -   [Analyzing the Results](#analyzing-the-results)
    -   [Advanced Customization](#advanced-customization)
-   [Where to Go From Here](#where-to-go-from-here)
-   [Contributing](#contributing)
-   [License](#license)

Summary
=======

<img alt="Evaluator Logo" title="Evaluator" src="img/evaluator_logo.jpg" width="100" style="float:right;width:100px;"/>

Evaluator is an open source information security strategic risk analysis toolkit. Based upon the OpenFAIR [taxonomy](https://www2.opengroup.org/ogsys/catalog/C13K) and risk assessment [standard](https://www2.opengroup.org/ogsys/catalog/C13G), Evaluator provides a concrete process for an organization to perform a review of its security program that is quantifiable, repeatable, and data-driven.

For a demonstration of one of the outputs of Evaluator, visit the demonstration copy of [Threat Explorer](https://davidski.shinyapps.io/threat_explorer) (running with dummy data, of course).

Background
==========

The first iterations of Evaluator were created as part of a major healthcare institution's drive to move from a risk assessment program based upon qualitative labels (high/medium/low, etc.) to a quantitative model that could be used to more precisely compare potential projects. Taking a mature risk program and providing statistical sampling with quantitative numbers enabled this organization greater fidelity into its information risks, meeting HIPAA compliance obligations, and providing business leaders from manger to board level with data to drive decision making.

Since its creation, versions of Evaluator have been deployed both in and outside of healthcare.

How to Use
==========

The Evaluator toolkit consists of a series of processes implemented in the [R language](https://www.r-project.org/). Starting from a simple Excel workbook, an organization can document its strategic risk scenarios.
This workbook is then imported and ran through a simulation model to estimate the expected losses for each scenario. These simulations can then be used for detailed analysis (a sample [Shiny](https://shiny.rstudio.com/) application is included) and for creating a formal risk report.

Evaluator takes a domain-driven and framework-independent approach to strategic security risk analysis. Whether you use ISO, COBIT, HITRUST, PCI-DSS, or a custom model for organizing your information security program, so long as you can describe the major areas of your program, the controls in each area, and the threat scenarios for each domain, you can use Evaluator!

Instructions
------------

While not required, reviewing the OpenFAIR methodology and terminology is highly encouraged. This README does not go into depth on the definitions of terms, which the OpenFAIR documents provide. Familiarity with the R language is also highly encouraged.

Running the toolkit involves six steps:

1.  Preparing the environment
2.  Defining your security domains
3.  Defining your controls and risk scenarios
4.  Importing the scenarios
5.  Running the simulations
6.  Analyzing the results

Don't be daunted by the process. Evaluator is with you at every step!

### Preparing the Environment

You will need a copy of R installed on your system. Evaluator should work on any reasonably current version of R (v3.3.2 as of this writing) on any supported platform (Windows, MacOS, or Linux). Using the [RStudio](https://www.rstudio.com/) IDE is strongly recommended, but not required.

Obtain the Evaluator toolkit either by cloning this repository (`git clone https://github.com/davidski/evaluator`) or by downloading and extracting the ZIP file from [GitHub](https://github.com/davidski/evaluator/archive/master.zip).

### Defining Your Security Domains

Evaluator needs to know the domains of your security program. These are the major buckets by which you subdivide your program, typically including areas such as Physical Security, Strategy, Policy, Business Continuity/Disaster Recover, Technical Security, and so forth. Out of the box, Evaluator comes with a demonstration model based upon the [HITRUST CSF](https://hitrustalliance.net/hitrust-csf/). If you have a different domain structure (e.g. ISO2700x, NIST CSF, or COBIT) then you need to edit the `data\domains.csv` file to include the domain names and the domain IDs, a shorthand abbreviation for the domain, such as POL for the Policy domain.

### Defining Your Controls and Risk Scenarios

The most critical piece is the identification of the controls (a/k/a capabilities) and the key risk scenarios associated with each domain. A spreadsheet is used to define these elements. This Excel workbook consists of one tab per domain, with each tab being named after the the domain ID defined in the previous step. Each tab consists of two tables- a controls table and a threats table.

#### Controls Table

The domain controls table is where you define the key objectives of each domain. While the specific controls will be unique to each organizationn, the provided sample spreadsheet may be used as inspiration. Try to avoid simply copying every technical control out of something like ISO 27001 or COBIT, as most such control frameworks are too fine grained to provide the high level overview Evaluator delivers. Typically 50 controls or less can describe organizations up to one to two billion USD in size. Each control must have a unique ID and should be assigned a difficulty (DIFF) score, ranking the maturity of the control on a CMM scale from Initial (lowest score) to Optimized (best of class).

#### Threats Table

The threats table consists of the potential loss scenarios that each domain of your security program addresses. Each scenario is made up of a descriptive field describing who did what to whom, the threat community (actor) that executed the attack (e.g. external hacktivist, internal workforce member, third party vendor), how often the threat actor will have the potential to act upon your assets (TEF), the strength by which they will act against your assets (TCap), the losses should the threat occur (LM), and a comma-separated list of the control IDs that prevent the scenario.

### Importing the Scenarios

To extract the spreadsheet into tidy data files for further analysis, launch RStudio and open the `import_survey.Rmd` notebook, then click on the `Knit` button the notebook (a collection of descriptive text and R code). The notebook will perform perform basic data validation on the workbook and extract the data. If there are data validation errors, the process will abort and an error message will be displayed. To address the data validation errors, correct the spreadsheet and re-knit the notebook.

### Running the Simulations

With the data now ready for simulation, open the `simulate_risk.Rmd` notebook and click on the `Knit` button. By default, Evaluator puts each scenario through 10,000 individual simulated years, modelling how often the threat actor will come into contact with your assets, the strength of the threat actor, the strength of your controls, and the losses involved in any situation where the threat strength exceeds your control strength. This simulation process can be computationally intense. The sample data set takes approximately 5-7 minutes on my primary development machines (last generation Windows-based platforms).

### Analyzing the Results

Open the `explore_scenarios.Rmd` and click on `Run Document` to launch a local copy of the Scenario Explorer application to view information on the individual scenarios as well as a sample overview of the entire program. For more in depth analysis, review the following data files:

| Data File               | Purpose                                                                             |
|:------------------------|:------------------------------------------------------------------------------------|
| simulation\_results.Rds | Full details of each simulated scenario                                             |
| scenarios\_summary.Rds  | Quantitative values of each scenario, as converted from the qualitative spreadsheet |

These data files may be used for your own analysis, or as the data-driven foundation for your own risk report. Sample reports are included in the `analyze_risk.Rmd`, which is a template for a technical risk report, and the `risk_dashboard.Rmd`, which provides a skeleton dashboard for an executive-level risk summary.

Advanced Customization
----------------------

Evaluator makes several assumptions to get you up and running as quickly as possible. For advanced users, there are several different customizations which can be made:

-   Risk tolerances - Organizational risk tolerances at a "medium" and "high" level are defined in `data/risk_tolerances.csv`. Risk tolerance are the levels of aggregate economic loss at which your organization cares. These are not necesarily the same as the size of potential losses from individual scenarios. A good proxy for risk tolerance is the budget authority used in your organization. The size of purchase signoff required at the executive level is generally a good indicator of the minimum floor of high risk tolerance.
-   Qualitative mappings - The translation of qualitative labels such as "Frequent" threat events and "Optimized" controls are defined in `data/qualitative_mappings.csv`. All the values in this mapping can be changed, but they must agree with the values used in the survey spreadsheet. Changing the number of levels used for any qualitative label (e.g. changing High/ Medium/Low to High/Medium/Low/VeryLow) is also unsupported.
-   Styling - The majority of the look and feel for fonts, colors, etc. are defined in the `styles/html-styles.css` and `styles/word-styles-reference.docx` files.

Where to Go From Here
=====================

While Evaluator can do a lot of you, there are areas which this toolkit explicitly does not attempt to address. These include topics such as complex analysis of security risks, interaction between risk scenarios, rolling up multiple levels of risk into aggregations, modelling secondary losses, etc. As you become more comfortable with quantitative risk analysis, you may wish to pursue these topics (and I hope that you do!).

Commercial Software

-   [RiskLens](http://www.risklens.com/), founded by the original creator of the FAIR methodology.

Books/Training

-   [Measuring and Managing Information Risk](https://smile.amazon.com/gp/product/0124202314)
-   [OpenFAIR certification](http://www.opengroup.org/certifications/openfair)

Associations

-   [FAIR Institute](http://www.fairinstitute.org/)
-   [Society of Information Risk Analysts (SIRA)](https://www.societyinforisk.org/)

Contributing
============

Note that this project is governed by a [Code of Conduct](./CODE_OF_CONDUCT.md). By participating in this project you agree to abide by these terms.

License
=======

The [MIT License](LICENSE) applies.
