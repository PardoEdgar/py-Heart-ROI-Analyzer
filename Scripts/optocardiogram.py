"""
Author: Edgar Alejandro Pardo-Sarmiento (2026)
        Escuela de Ciencias e Ingeniería,
        Universidad del Rosario, Colombia

Description:
    This program finds, filtered and allows to visualize and check local valleys from pixel intensity signals across time, as proxy of ventricle contractions.

Workflow:
    1. Data files are read and columns data are saved.
    2. Raw signal is filtered and Signal valleys representing ventricle contractions are found, filtered and saved.
    3. Signal and found valleys are plotted for visualization and manual inspection, and values and time from cheked valleys are saved in csv.

Functions:
    main()
        Executes the full analysis pipeline across the csv data files.
    load_data()
        Loads csv files, saves de data columns and creates the time column from frames data.
    find_valleys()
         find the possible ventricle contractions (valleys) from a filtered signal, and considering
         a treshold based upon the trend signal. Then, a minimun distance is set to avoid duplicate local valleys
    visualize_and_save()
        Plots all the pixel intensity data over time and the found valleys (Ventricle contractions).
        Lastly, the time and value of valid valleys found in the signal are saved for further analysis.
"""

import os
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from scipy.signal import savgol_filter, find_peaks
from statsmodels.nonparametric.smoothers_lowess import lowess


FOLDER = r"C:\Users\jandr\Videos\Out"
OUTPUT_GRAPHS = os.path.join(FOLDER, "Graphs")

os.makedirs(OUTPUT_GRAPHS, exist_ok=True)


def main():
    all_valleys = []
    for filename, data in load_data():
        print(f"\nProcessing {filename}")
        valleys, processed = find_valleys(data)
        valleys["source_file"] = filename
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


def find_valleys(data):
    signal = data["Mean(Ventricle)"].values
    time = data["time"].values
    smoothed = savgol_filter(signal, 25, 5)
    trend = lowess(smoothed, time, frac=0.08, return_sorted=False)
    inverted = -smoothed
    valley_indices, _ = find_peaks(inverted)
    valley_values = smoothed[valley_indices]
    threshold = 0.98 * trend[valley_indices]
    valid = valley_values < threshold
    valley_indices = valley_indices[valid]
    min_dist = 18
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
    plt.plot(data["time"], data["Mean(Ventricle)"], label="Raw signal")
    plt.plot(data["time"], data["smoothed"], label="Smoothed signal")
    plt.plot(data["time"], data["trend"], label="Trend curve")
    if len(valleys) > 0:
        plt.scatter(valleys["time"], valleys["value"], label="Valleys")
    plt.xlabel("Time (s)")
    plt.ylabel("Mean Ventricle Signal (Pixel intensity)")
    plt.legend()
    output_path = os.path.join(OUTPUT_GRAPHS, f"{filename}.png")
    plt.savefig(output_path, dpi=300)
    plt.close()
    print("Saved:", output_path)


if __name__ == "__main__":
    main()
