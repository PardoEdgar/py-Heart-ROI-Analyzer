# py-Heart-ROI-Analyzer (2026)
This repository contains a python-ImageJ-R tool for extracting pixel intensity and heart function data from cardiac videos using fast manual selection of Region Of Interest (ROI) via OpenCV.
---
##Overwiew

This repository provides a reproducible workflow to quatify great heart recordings data volume in the in the land invasive snail *Cornu aspersum*.

My approach combines optocardiography based on pixel intensity changes, video porcessing Manueal ROI selection, optocardiogram visualization, heart rate varibility metrics extraction, data analyisis and visualization.

Pixel intennsity extraction, trimming of heart recordings, ROI selection tool, HRV metrics extraction and optocardiogram visualization are implemented in Python-ImageJ Macro combination. Data analysis and visualization are implemente in R. This tool was applied to investigate the cardiac adjustments to face temperature and pressure cahnges in the elevational gradient of the Colombian Andes.

---
##Contens

-`scripts/`: Python, R, and ImageJ Macro scripts for Pixel intennsity extraction, trimming of heart recordings, ROI selection tool, HRV metrics extraction, optocardiogram visualization, data analysis and data visualization.
## Reproducibility

All scripts conducted in Python. ImageJ and R are provided in sequential Order:
1.`Automated_Trimming.py`
2.`Manual_Trimming.py`
3.`template.ijm`
4.`Automated_heart_pixel_intensity_extraction.py`
5.`HRV_metrics_extraction.py`
6.`Optocardiogram_visualization.py`
7.`Data_analysis_&_visualization.R`

---
## Requirements

## Python
- os
- Subprocess
- OpenCV (cv2)
- numpy
- collections
- sys
- pandas
- matplotlib
- scipy
- statsmodels
- neuroktit2
- seaborn

## R
- R (Version 4.4.2)
- tidyverse
## ImageJ
- Fiji-ImageJ
- Java

---

## Author

**E. Alejandro Pardo-Sarmiento

# Colaborators
- Eva Valentina Montesdeoca
- J.P. Hernandez
- Andre J. Riveros
- Natalia Buitrago-Ricaurte

---

## License
- The sorce code in this repositroy is licensed ucer the MIT license
-
