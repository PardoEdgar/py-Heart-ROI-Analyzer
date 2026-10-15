"""
Author: Edgar Alejandro Pardo-Sarmiento (2026)
        Departamento de Biología,
        Escuela de Ciencias e Ingeniería,
        Universidad del Rosario, Colombia.

Description:
    This program finds, filtered and allows to visualize and check local valleys from pixel intensity
    signals across time, as proxy of ventricle contractions.

Workflow:
    1. Data files are read and columns data are saved.
    2. Raw signal is filtered and Signal valleys representing ventricle contractions are found,
       filtered and saved.
    3. Signal and found valleys are plotted for visualization and manual inspection, and values
       and time from cheked valleys are saved in csv.

Functions:
    main()
        Executes the full analysis pipeline across the csv data files.
    load_data()
        Loads csv files, saves de data columns and creates the time column from frames data.
    find_valleys()
         find the possible ventricle contractions (valleys) from a filtered signal, and considering
         a treshold based upon the trend signal. Then, a minimun distance is set to avoid duplicate
         local valleys
    visualize_and_save()
        Plots all the pixel intensity data over time and the found valleys (Ventricle contractions).
        Lastly, the time and value of valid valleys found in the signal are saved for further analysis.
"""

from cv2 import split
import os
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from scipy.signal import savgol_filter, find_peaks
from statsmodels.nonparametric.smoothers_lowess import lowess
from fnmatch import fnmatch


FOLDER = r"C:\Users\jandr\Videos\videos prueba\data_extraction"
OUTPUT_GRAPHS = os.path.join(FOLDER, "Graphs")

os.makedirs(OUTPUT_GRAPHS, exist_ok=True)


Exclude = [
    "A3_Cold_Bogota_Elevation_processed_cropped_ROI*",
    "B2_High_Bogota_Elevation_processed_cropped_ROI*",
    "B2_High_LaVega_Elevation_processed_cropped_ROI*",
    "C3_ENV_Bogota_Elevation_UP*",
    "E4_Cold_Vega_Elevation_processed_cropped_ROI*",
    "E7_High_Bogota_Elevation_processed_cropped_ROI*",
    "F5_Cold_Bogota_Elevation_UP*",
    "F9_Cold_Bogota_Elevation_processed_cropped_ROI*",
    "H1_Cold_bOGOTA_Elevation_UP*",
    "I4_cold_LaVega_Elevation_UP*",
    "J5_High_Vega_Elevation_UP*",
    "J7_Cold_LaVega_Elevation_UP*",
    "E1_High_LaVega_Elevation_processed_cropped_ROI*",
    "A8_High_LaVega_Elevation_processed_cropped_ROI*",
    "A1_High_Vega_Elevation_UP*",
    "A1_High_LaVega_Elevation_processed_cropped_ROI*",
    "A3_High_Bogota_Elevation_processed_cropped_ROI*",
    "A3_High_LaVega_Elevation_processed_cropped_ROI*",
    "A3_High_Vega_Elevation_UP*",
    "A8_Cold_Bogota_Elevation_UP*",
    "B4_Cold_LaVega_Elevation_UP*",
    "C1_Cold_Bogota_Elevation_processed_cropped_ROI*",
    "C1_Cold_Vega_Elevation_processed_cropped_ROI*",
    "C1_ENV_LaVega_Elevation_processed_cropped_ROI*",
    "C1_ENV_LaVega_Elevation_UP*",
    "C1_High_Bogota_Elevation_processed_cropped_ROI*",
    "C3_High_Bogota_Elevation_processed_cropped_ROI*",
    "C5_Cold_Vega_Elevation_processed_cropped_ROI*n",
    "C6_Cold_Bogota_Elevation_UP*",
    "C6_ENV_Vega_Elevation_UP*",
    "D4_High_Bogota_Elevation_processed_cropped_ROI*",
    "D6_High_Bogota_Elevation_processed_cropped_ROI*",
    "D8_Cold_Vega_Elevation_processed_cropped_ROI*",
    "E2_Cold_Bogota_Elevation_UP*",
    "E4_High_LaVega_Elevation_processed_cropped_ROI*",
    "E7_ENV_Bogota_Elevation_processed_cropped_ROI*",
    "E9_High_Bogota_Elevation_processed_cropped_ROI*",
    "F2_Cold_Bogota_Elevation_UP*",
    "F3_ENV_Bogota_Elevation_UP*",
    "F9_ENV_Bogota_Elevation_UP*",
    "F9_High_Bogota_Elevation_processed_cropped_ROI*",
    "G2_Cold_Vega_Elevation_processed_cropped_ROI*",
    "G5_ENV_LaVega_Elevation_processed_cropped_ROI*",
    "G6_ENV_LaVega_Elevation_UP*",
    "G7_ENV_Bogota_Elevation_UP*",
    "G7_High_Vega_Elevation_UP*",
    "H5_High_Bogota_Elevation_processed_cropped_ROI*",
    "H5_High_Vega_Elevation_UP*",
    "H6_Cold_LaVega_Elevation_UP*",
    "H7_Cold_Bogota_Elevation_UP*",
    "H7_ENV_Vega_Elevation_UP*",
    "H8_Cold_Bogota_Elevation_processed_cropped_ROI*",
    "H8_Cold_Bogota_Elevation_UP*",
    "H8_High_LaVega_Elevation_processed_cropped_ROI*",
    "H9_High_Bogota_Elevation_UP*",
    "I3_High_Bogota_Elevation_processed_cropped_ROI*",
    "I5_ENV_Vega_Elevation_processed_cropped_ROI*",
    "I6_Cold_BOGOTA_Elevation_UP*",
    "I6_High_Bogota_Elevation_processed_cropped_ROI*",
    "I6_High_Bogota_Elevation_UP*",
    "I6_High_Lavega_Elevation_processed_cropped_ROI*",
    "I6_High_vega_Elevation_UP*",
    "I7_High_LaVega_Elevation_processed_cropped_ROI*",
    "I7_High_Vega_Elevation_UP*",
    "I9_High_bogota_Elevation_processed_cropped_ROI*",
    "J1_Cold_bogota_Elevation_UP*",
    "J1_High_bogota_Elevation_processed_cropped_ROI*",
    "J1_High_bogota_Elevation_UP*",
    "J5_ENV_LaVega_Elevation_processed_cropped_ROI*",
    "J7_ENV_LaVega_Elevation_processed_cropped_ROI*",
    "J7_High_BOGOTA_Elevation_processed_cropped_ROI*",
    "J7_High_LaVEGA_Elevation_processed_cropped_ROI*",
    "J8_High_LaVEGA_Elevation_processed_cropped_ROI*",
    "K1_High_Bogota_Elevation_processed_cropped_ROI*",
    "K1_High_LaVega_Elevation_processed_cropped_ROI*",
    "K2_ENV_LaVega_Elevation_UP*",
    "K2_High_LaVega_Elevation_processed_cropped_ROI*",
    "K3_Cold_bogota_Elevation_processed_cropped_ROI*",
    "K4_Cold_Vega_Elevation_processed_cropped_ROI*",
    "K6_Cold_vega_Elevation_processed_cropped_ROI*",
    "K6_High_Vega_Elevation_UP*",
    "K8_High_Lavega_Elevation_processed_cropped_ROI*",
    "K9_High_Lavega_Elevation_processed_cropped_ROI*",
    "A4_Cold_LaVega_Elevation_UP*",
    "C5_Cold_Vega_Elevation_processed_cropped_ROI*",
    "H8_High_Vega_Elevation_up*",
    "B7_High_Bogota_Elevation_processed_cropped_ROI*",
    "E4_Cold_LaVega_Elevation_UP*",
    "A3_ENV_Bogota_Elevation_UP*",
    "A2_High_Bogota_Elevation_processed_cropped_ROI*",
    "A3_ENV_Vega_Elevation_UP*",
    "G3_ENV_Bogota_Elevation_UP*",
    "E8_Cold_LaVega_Elevation_UP*",
]
Special_case_1 = [
    "A3_Cold_Bogota_Elevation_processed_cropped_ROI*",
    "A3_ENV_Bogota_Elevation_UP*",
    "B2_Cold_LaVega_Elevation_UP*",
    "B2_High_Bogota_Elevation_processed_cropped_ROI*",
    "B2_High_LaVega_Elevation_processed_cropped_ROI*",
    "B3_Cold_Bogota_Elevation_UP*",
    "B4_Cold_Vega_Elevation_processed_cropped_ROI*",
    "B6_Cold_Vega_Elevation_processed_cropped_ROI*",
    "B7_Cold_LaVega_Elevation_UP*",
    "C1_Cold_Bogota_Elevation_UP*",
    "C3_ENV_Bogota_Elevation_UP*",
    "C6_ENV_Bogota_Elevation_processed_cropped_ROI*",
    "C8_High_Bogota_Elevation_processed_cropped_ROI*",
    "D4_High_Vega_Elevation_UP*",
    "D7_Cold_LaVega_Elevation_UP*",
    "E3_ENV_Bogota_Elevation_processed_cropped_ROI*",
    "E4_Cold_Vega_Elevation_processed_cropped_ROI*",
    "E5_Cold_Bogota_Elevation_UP*",
    "E6_Cold_Bogota_Elevation_UP*",
    "E7_Cold_LaVega_Elevation_UP*",
    "E7_High_Bogota_Elevation_processed_cropped_ROI*",
    "F2_High_Bogota_Elevation_UP*",
    "F5_Cold_Bogota_Elevation_UP*",
    "F7_Cold_Bogota_Elevation_UP*",
    "F8_Cold_Bogota_Elevation_UP*",
    "F9_Cold_Bogota_Elevation_processed_cropped_ROI*",
    "G1_cold_Bogota_Elevation_UP*",
    "G1_cold_LaVega_Elevation_UP*",
    "G9_Cold_Vega_Elevation_processed_cropped_ROI*",
    "H1_Cold_Bogota_Elevation_UP*",
    "H4_Cold_LaVega_Elevation_UP*",
    "H5_Cold_LaVega_Elevation_UP*",
    "H7_High_Bogota_Elevation_UP*",
    "I1_Cold_Bogota_Elevation_UP*",
    "I2_Cold_Bogota_Elevation_UP*",
    "I3_cold_Bogota_Elevation_processed_cropped_ROI*",
    "I4_cold_LaVega_Elevation_UP*",
    "I4_ENV_Bogota_Elevation_processed_cropped_ROI*",
    "I6_Cold_LaVega_Elevation_UP*",
    "I7_ENV_Bogota_Elevation_processed_cropped_ROI*",
    "I9_cold_Bogota_Elevation_processed_cropped_ROI*",
    "J1_cold_vega_Elevation_processed_cropped_ROI*",
    "J2_ENV_Bogota_Elevation_processed_cropped_ROI*",
    "J5_Cold_LaVega_Elevation_UP*",
    "J5_High_Bogota_Elevation_processed_cropped_ROI*",
    "J5_High_Vega_Elevation_UP*",
    "A4_High_Bogota_Elevation_UP*",
    "A5_Cold_Bogota_Elevation_processed_cropped_ROI*",
    "B2_Env_LaVega_Elevation_processed_cropped_ROI*",
    "E7_Cold_Vega_Elevation_processed_cropped_ROI*",
    "F2_High_Bogota_Elevation_processed_cropped_ROI*",
    "G2_Cold_Bogota_Elevation_UP*",
    "G2_ENV_Bogota_Elevation_processed_cropped_ROI*",
    "G2_ENV_Bogota_Elevation_UP*",
    "I4_Cold_Bogota_Elevation_processed_cropped_ROI*",
    "I5_ENV_LaVega_Elevation_UP*",
    "I8_Cold_Vega_Elevation_processed_cropped_ROI*",
    "I9_High_Vega_Elevation_UP*",
    "K3_ENV_Bogota_Elevation_processed_cropped_ROI*",
    "K5_Cold_Bogota_Elevation_processed_cropped_ROI*",
]

Special_case_2 = [
    "E2_ENV_Bogota_Elevation_processed_cropped_ROI*",
]


def main():
    all_valleys = []
    for filename, data in load_data():
        if any(fnmatch(filename.lower(), pattern.lower()) for pattern in Exclude):
            print(f"Excluded: {filename}")
            continue
        print(f"\nProcessing {filename}")
        Characters = filename.split("_")
        Snail = Characters[0]
        Temperature = Characters[1]
        Site = Characters[2]
        if "UP" in filename:
            Gradient = "UP"
        else:
            Gradient = "DOWN"
        if Temperature == "Cold":
            Temperature = "3.5C"
        elif Temperature == "ENV":
            Temperature = "22C"
        else:
            Temperature = "40C"
        Site = "LaVega" if Site != "Bogota" else Site
        valleys, processed = find_valleys(data, filename)
        valleys["source_file"] = filename
        valleys["Snail"] = Snail
        valleys["Temperature"] = Temperature
        valleys["Site"] = Site
        valleys["Gradient"] = Gradient
        all_valleys.append(valleys)
        visualize_and_save(processed, valleys, filename)
    all_valleys = pd.concat(all_valleys, ignore_index=True)
    print("All detected valleys:")
    print(all_valleys)
    all_valleys.to_csv(os.path.join(FOLDER, "all_valleys.csv"), index=False)


def load_data():
    for file in os.listdir(FOLDER):
        if file.endswith(".csv"):
            full_path = os.path.join(FOLDER, file)
            data = pd.read_csv(full_path)
            data.columns = data.columns.str.strip()
            data.rename(columns={data.columns[0]: "frames"}, inplace=True)
            if "Mean(Ventricle)" not in data.columns:
                print("Missed Column", file)
                continue
            data = data[["frames", "Mean(Ventricle)"]]
            data["time"] = (data["frames"] / 25) - 0.04
            yield file, data


def find_valleys(data, filename):
    signal = data["Mean(Ventricle)"].values
    time = data["time"].values
    smoothed = savgol_filter(signal, 25, 5)
    trend = lowess(smoothed, time, frac=0.08, return_sorted=False)
    inverted = -smoothed
    valley_indices, _ = find_peaks(inverted)
    if any(fnmatch(filename.lower(), pattern.lower()) for pattern in Special_case_1):
        threshold = 0.98 * trend[valley_indices]
        min_dist = 18
    elif any(fnmatch(filename.lower(), pattern.lower()) for pattern in Special_case_2):
        threshold = 0.95 * trend[valley_indices]
        min_dist = 18
    else:
        threshold = trend[valley_indices]
        min_dist = 10
    valley_values = smoothed[valley_indices]
    valid = valley_values < threshold
    valley_indices = valley_indices[valid]
    filtered = []

    if len(valley_indices) > 0:
        group = [valley_indices[0]]
        for idx in valley_indices[1:]:
            if idx - group[-1] <= min_dist:
                group.append(idx)
            else:
                best = min(group, key=lambda i: smoothed[i])
                filtered.append(best)
                group = [idx]
        best = min(group, key=lambda i: smoothed[i])
        filtered.append(best)
    filtered = np.array(filtered)
    valleys = pd.DataFrame({"time": time[filtered], "value": smoothed[filtered]})
    data["smoothed"] = smoothed
    data["trend"] = trend
    return valleys, data


def visualize_and_save(data, valleys, filename):
    plt.figure()
    if len(valleys) > 0:
        plt.scatter(
            valleys["time"],
            valleys["value"],
            label="Valleys",
            color="red",
            s=15,
            zorder=4,
        )
    plt.plot(data["time"], data["Mean(Ventricle)"], label="Raw signal", zorder=1)
    plt.plot(data["time"], data["smoothed"], label="Smoothed signal", zorder=2)
    plt.plot(data["time"], data["trend"], label="Trend curve", zorder=3)
    plt.xlabel("Time (s)")
    plt.ylabel("Mean Ventricle Signal (Pixel intensity)")
    plt.legend()
    output_path = os.path.join(OUTPUT_GRAPHS, f"{filename}.png")
    plt.savefig(output_path, dpi=300)
    plt.close()
    print("Saved:", output_path)


if __name__ == "__main__":
    main()
