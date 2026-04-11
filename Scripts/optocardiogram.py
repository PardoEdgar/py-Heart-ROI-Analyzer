
""" 
remain to be extracted
    A2_COLD_BOG_DOWN
Non-usable data
    ENV_Bogota_DOWN
        C6_ENV_Bogota_Elevation pero revisar
        G2_ENV_Bogota PERO REVISAR
        I6_ENV_Bogota 
    COLD_BOG_DOWN
        A3_COLD_BOG_DOWN
        B4_COLD_BOG_DOWN pero revisar
        C1_COLD_BOG_DOWN
        F9_COLD_BOG_DOWN
        H8_COLD_BOG_DOWN
        K3_COLD_BOG_DOWN
    ENV_Vega_Down
        C6_ENV_Vega_Elevation
        I5_ENV_Vega_Elevation
        I7_ENV_Vega_Elevation revisar video
    Cold_Vega_Down
        A1_Cold_Vega_Down
        A3_Cold_Vega_Down
        E6_Cold_Vega_Elevation
        G2_Cold_Vega_Elevation
        K4_Cold_Vega
    ENV_Vega_Up
        C6_ENV_Vega_Elevation_UP
    Warm_Vega_Up
        A1_High_Vega_Elevation_UP
        A3_High_Vega_Elevation_UP
        G7_High_Vega_Elevation_UP
        H1_High_Vega_Elevation_UP
        H5_High_Vega_Elevation_UP
        I6_High_Vega_Elevation_Up
        I7_High_Vega_Elevation_Up
        J5_High_Vega_Elevation_UP
    ENV_Bogota_uP
        A3_ENV_Bogota_Elevation_UP
        F3_ENV_Bogota_Elevation_UP
    Cold_Bogota_UP
        F2_ENV_Bogota_Elevation_UP
        J1_Cold_Bogota_Elevation_UP
    """
"""numeros a eliminar: A1, A3, B4, C1, C6, E6, F2, F3, F9, G2, G7, H1, H5, H8, I5, I6, I7, J1, J5, K3, K4"""
import os
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

from scipy.signal import savgol_filter, find_peaks
from statsmodels.nonparametric.smoothers_lowess import lowess


FOLDER = r"C:\Users\jandr\Videos\videos prueba\data_extraction"
OUTPUT_GRAPHS = os.path.join(FOLDER, "Graphs")

os.makedirs(OUTPUT_GRAPHS, exist_ok=True)

def main():

    all_valleys = []

    for filename, data in load_data():

        print(f"\nProcesando {filename}")

        valleys, processed = find_valleys(data)

        valleys["source_file"] = filename
        all_valleys.append(valleys)

        visualize_and_save(processed, valleys, filename)

    all_valleys = pd.concat(all_valleys, ignore_index=True)

    print("\n✅ Todos los valleys detectados:")
    print(all_valleys)

    all_valleys.to_csv(os.path.join(FOLDER, "all_valleys.csv"), index=False)


# --------------------------------------------------
# Cargar archivos uno por uno
# --------------------------------------------------

def load_data():

    for file in os.listdir(FOLDER):

        if file.endswith(".csv"):

            full_path = os.path.join(FOLDER, file)

            data = pd.read_csv(full_path)
            data.columns = data.columns.str.strip()

            data.rename(columns={data.columns[0]: "frames"}, inplace=True)

            if "Mean(Ventricle)" not in data.columns:
                print("⚠ Columna faltante en", file)
                continue

            data = data[["frames", "Mean(Ventricle)"]]
            data["time"] = (data["frames"] / 25) - 0.04

            yield file, data


# --------------------------------------------------
# Detección de valleys
# --------------------------------------------------

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

    valleys = pd.DataFrame({
        "time": time[filtered],
        "value": smoothed[filtered]
    })

    data["smoothed"] = smoothed
    data["trend"] = trend

    return valleys, data


# --------------------------------------------------
# Graficar y guardar
# --------------------------------------------------

def visualize_and_save(data, valleys, filename):

    plt.figure()

    plt.plot(data["time"], data["Mean(Ventricle)"], label="Raw")
    plt.plot(data["time"], data["smoothed"], label="Smoothed")
    plt.plot(data["time"], data["trend"], label="Trend")

    if len(valleys) > 0:
        plt.scatter(valleys["time"], valleys["value"], label="Valleys")

    plt.xlabel("Time (s)")
    plt.ylabel("Mean Ventricle Signal")
    plt.legend()

    output_path = os.path.join(OUTPUT_GRAPHS, f"{filename}.png")
    plt.savefig(output_path, dpi=300)
    plt.close()

    print("✔ Guardado:", output_path)


# --------------------------------------------------

if __name__ == "__main__":
    main()
