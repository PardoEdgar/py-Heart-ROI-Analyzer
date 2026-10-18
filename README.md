# py-Heart-ROI-Analyzer (2026)
This repository contains a python-ImageJ-R tool for extracting pixel intensity and heart function data from cardiac videos using fast manual selection of Region Of Interest (ROI) via OpenCV.
---
## Overview

This repository provides a reproducible workflow to quatify great heart recordings data volume in the land invasive snail *Cornu aspersum*.

This approach combines optocardiography based on pixel intensity changes, video porcessing with semiautomated ROI selection, optocardiogram visualization, heart rate varibility metrics extraction, data analyisis and visualization.

Pixel intensity extraction, trimming of heart recordings, ROI selection tool, HRV metrics extraction and optocardiogram visualization are implemented in a Python-ImageJ Macro combination. Data analysis and visualization are conducted in R. This tool was applied to investigate the cardiac adjustments to face temperature and pressure changes that comes together with elevational gradients caused by massive mountain formations such as the Colombian Andes.

---
## Contents

-`scripts/`: Python, R, and ImageJ Macro scripts for Pixel intennsity extraction, trimming of heart recordings, ROI selection tool, HRV metrics extraction, optocardiogram visualization, data analysis and data visualization.

-`Data/`: Exp_1 and Exp_2 data presented in xlsx files. Each one contains Snail Size, HR or HRV metrics and sensors data (Temperature and pressure).
## Reproducibility
-`Google Drive folder/`:  It contains the optocardiograms and the pixel-intensity data over time for all experiments and recordings. Link: https://drive.google.com/drive/folders/1Zj0yrm4FW6JRuhVCadqirOOXD2eTtY_x?usp=sharing

All scripts conducted in Python. ImageJ and R are provided in sequential Order:
1. `Automated_Trimming.py`
2. `Manual_Trimming.py`
3. `Macro_template.ijm`
4. `Automated_heart_pixel_intensity_extraction.py`
5. `Optocardiograms.py`
6. `HRV_metrics_extraction.R`
7. `Data_analysis_&_visualization.R`

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

### Colaborators
- Eva Valentina Montesdeoca
- J.P. Hernandez
- Andre J. Riveros
- Natalia Buitrago-Ricaurte

---

## License
- The source code in this repository is licensed under the MIT license regarding scripts and data. 
