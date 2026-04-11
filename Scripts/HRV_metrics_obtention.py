import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import os
import neurokit2 as nk
import numpy as np
folder = r"C:\Users\jandr\Videos\videos prueba\data_extraction"
def main():
    data = load()
    filtered_valleys, metrics_df = compute_basic_metrics(data)
    metrics_df = mass_data(metrics_df)
    print(filtered_valleys, metrics_df)
    temp_order = sorted(metrics_df["Temperature"].unique())
    sns.set_theme(style="whitegrid", context="talk")
    metrics_df.to_csv(os.path.join(folder, "HRV_metrics.csv"), index=False)
    g = sns.catplot(
        data=metrics_df,
        x="Temperature",
        y="pnn100",
        hue="Site",
        col="Gradient",
        kind="box",
        order=temp_order,
        height=4,
        aspect=1,
        showfliers=False
    )

    g.map_dataframe(
        sns.stripplot,
        x="Temperature",
        y="pnn100",
        hue="Site",
        dodge=True,
        alpha=0.1,
        size=2
    )

    plt.show()

def load():
    file = r"C:\Users\jandr\Videos\videos prueba\data_extraction\valleys_data_analysis.csv"
    data = pd.read_csv(file)
    return data

def compute_basic_metrics(data):
    valleys = data.sort_values(
        ["Snail", "Gradient", "Temperature", "Site", "time"]
    ).copy()

    expected_conditions = valleys.groupby(
        ["Temperature", "Gradient", "Site"]
    ).ngroups

    snail_counts = valleys.groupby("Snail").apply(
        lambda x: x.groupby(["Temperature", "Gradient", "Site"]).ngroups
    )

    valid_snails = snail_counts[snail_counts == expected_conditions].index
    
    filtered_valleys = valleys[valleys["Snail"].isin(valid_snails)].copy()

    filtered_valleys["IBI"] = filtered_valleys.groupby(
        ["Snail", "Gradient", "Temperature", "Site"]
    )["time"].diff()

    filtered_valleys = filtered_valleys[filtered_valleys["IBI"] > 0]

    metrics_list = []

    for keys, group in filtered_valleys.groupby(
        ["Snail", "Gradient", "Temperature", "Site"]
        ):
    
        ibi = group["IBI"].values

        if len(ibi) < 3:   
            continue
        ibi_ms = np.asarray(ibi * 1000, dtype=float).flatten()
        ibi_ms = ibi_ms[~np.isnan(ibi_ms)]
        ibi_ms = ibi_ms[ibi_ms > 0]
        
        if len(ibi_ms) < 3:
            continue

        try:
            peaks = nk.intervals_to_peaks(ibi_ms)
            results = nk.hrv_time(peaks=peaks, show=False)
        except Exception as e:
            print("HRV error:", keys, e)
            continue
        
        metrics_list.append({
        "Snail": keys[0],
        "Gradient": keys[1],
        "Temperature": keys[2],
        "Site": keys[3],

        "SDNN": results["HRV_SDNN"].values[0],
        "RMSSD": results["HRV_RMSSD"].values[0],
        "MeanNN": results["HRV_MeanNN"].values[0],
        "MeanHR": 60000 / results["HRV_MeanNN"].values[0],
        "pnn50": np.sum(np.abs(np.diff(ibi_ms)) > 50) / len(np.diff(ibi_ms)) * 100,
        "pnn100": np.sum(np.abs(np.diff(ibi_ms)) > 100) / len(np.diff(ibi_ms)) * 100

         })
    
    metrics_df = pd.DataFrame(metrics_list)
    return filtered_valleys, metrics_df

def mass_data(metrics_df):
    file = r"C:\Users\jandr\Videos\videos prueba\data_extraction\Snail_size.csv"
    mass_data = pd.read_csv(file)

    metrics_df = pd.merge(metrics_df, mass_data, on="Snail", how="left")

    return metrics_df
    

if __name__ == "__main__":
    main()