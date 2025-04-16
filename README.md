# 🧬 Genotyping Tool for Whole Genome Resequencing Data

This Shiny application allows users to genotype for **SNPs** and **Indels** of interest in the genome.


## 🌾 Hosted Versions

- 🔗 **Soybean Deployment (SoyKB):**  
  Use this for querying the soybean 2939 WGRS dataset  
  👉 [https://soykb.org/SoybeanGenotypingTool/](https://soykb.org/tools/genotyping)

- 🔧 **Custom Deployment:**  
  Upload and analyze your indexed VCFs using this app  
  👉 [ https://ansermahmood.shinyapps.io/genotyping_tool/]( https://ansermahmood.shinyapps.io/genotyping_tool/)



## ⚙️ **Local Usage Instructions**

To use this app locally on your data:

1. Clone or download the repository and navigate to the `custom_data_app/` directory.
2. Launch the app in R or RStudio
3. Upload your **indexed VCF files** (`.vcf.gz` and `.vcf.gz.tbi`).
4. Select SNPs of interest using predefined options, manual entry, or CSV upload.
5. View and download genotype summaries.


## 🧬 Use Cases

- Genotype for trait-associated alleles
- Generate tabular summaries and identify accessions with desireable combinations
