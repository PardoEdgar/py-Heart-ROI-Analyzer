# py-Heart-ROI-Analyzer (2026)
This repository contains a python-ImageJ-R tool for extracting pixel intensity and heart function data from cardiac videos using fast manual selection of Region Of Interest (ROI) via OpenCV.
---
## Overview

This repository provides a reproducible workflow to quatify great heart recordings data volume in the land invasive snail *Cornu aspersum*.

This approach combines optocardiography based on pixel intensity changes, video porcessing with semiautomated ROI selection, optocardiogram visualization, heart rate varibility metrics extraction, data analyisis and visualization.

Pixel intensity extraction, trimming of heart recordings, ROI selection tool, HRV metrics extraction and optocardiogram visualization are implemented in a Python-ImageJ Macro combination. Data analysis and visualization are conducted in R. This tool was applied to investigate the cardiac adjustments to face temperature and pressure changes that comes together with elevational gradients caused by massive mountain formations such as the Colombian Andes.

---
## Contents

-`scripts/`: Python, R, and ImageJ Macro scripts for Pixel intensity extraction, trimming of heart recordings, ROI selection tool, HRV metrics extraction, optocardiogram visualization, data analysis and data visualization.

-`Data/`: Exp_1 and Exp_2 data presented in xlsx files.
Exp_1 contains Snail number identification, size (body mass and shell diameter), Heart rate, NN intervals or heart rate variability metrics, levels of treatment factors (Temperature, Site, Gradient direction), sensors data (ambient temperature and pressure and temperature treatments).
Exp2 contains Snail body mass, Heart rate, NN intervals and sensors data (ambient temperature and pressure).

## Reproducibility

-`Google Drive folder/`:  It contains the optocardiograms and the pixel-intensity data over time for all experiments and recordings. Link: https://drive.google.com/drive/folders/1Zj0yrm4FW6JRuhVCadqirOOXD2eTtY_x?usp=sharing

All scripts conducted in Python. ImageJ and R are provided in sequential Order:
1. `Manual_Trimming.py`
2. `Macro_template.ijm`
3. `Automated_heart_pixel_intensity_extraction.py`
4. `Optocardiograms.py`
5. `HRV_metrics_extraction.R`
6. `Data_analysis_&_visualization.R`

---
## Requirements

## Python
- os
- Subprocess
- OpenCV
- numPy
- collections
- sys
- pandas
- matplotlib
- sciPy
- statsmodels
- fnmatch


## R
- tidyverse
- readxl
- RHRV
  
## ImageJ
- Fiji-ImageJ
- Java

---

## Author

- Edgar Alejandro Pardo-Sarmiento
  School of Sciences and Engineering, Universidad del Rosario, Colombia

### Colaborators
- Eva Valentina Montesdeoca
    School of Sciences and Engineering, Universidad del Rosario, Colombia
- J.P. Hernandez
    School of Sciences and Engineering, Universidad del Rosario, Colombia
- Andre J. Riveros
    School of Sciences and Engineering, Universidad del Rosario, Colombia
- Natalia Buitrago-Ricaurte
    School of Medicine and Health Sciences, Universidad del Rosario, Colombia

---

## License
- The source code in this repository is licensed under the MIT license regarding scripts and data. 
