"""
Creator: E. Alejandro Pardo-Sarmiento
Functions: trim_capture("Trimming videos"); preprocess_frames("Converted gray videos"); select_manual_roi("Select region of interest")
crop_video_manual_roi("Crop the region of interest")
"""
import cv2, numpy as np
from collections import deque
from scipy.signal import savgol_filter, find_peaks

OUT_W = 768
OUT_H = 540

def trim_capture(infile, start_sec=30, duration_sec=60, target_fps=25):
    cap = cv2.VideoCapture(infile)
    if not cap.isOpened():
        print("ERROR: Could not open the video.")
        return None, None

    src_fps = cap.get(cv2.CAP_PROP_FPS)
    print(f"Source FPS: {src_fps}")

    start_frame = int(start_sec * src_fps)
    cap.set(cv2.CAP_PROP_POS_FRAMES, start_frame)

    frames = []
    t = 0.0
    dt = 1.0 / target_fps
    duration = duration_sec

    while t < duration:
        frame_id = int((start_sec + t) * src_fps)
        cap.set(cv2.CAP_PROP_POS_FRAMES, frame_id)
        ret, f = cap.read()
        if not ret:
            break
        frames.append(f)
        t += dt

    cap.release()
    print(f"Captured frames: {len(frames)} at {target_fps} fps")
    return frames, target_fps


def preprocess_frames(frames, temporal_window=5):
    from collections import deque
    q = deque(maxlen=temporal_window)
    out = []
    for f in frames:
        gray = cv2.cvtColor(f, cv2.COLOR_BGR2GRAY)
        q.append(gray.astype(np.float32))
        med = np.median(np.stack(q, axis=0), axis=0)
        den = cv2.GaussianBlur(med.astype(np.uint8), (5,5), 0)
        p1, p99 = np.percentile(den, (1,99))
        den_norm = np.clip((den - p1) * 255.0 / max(1,(p99-p1)), 0, 255).astype(np.uint8)
        out.append(den_norm)
        print("Processing", f)
    return out

import cv2
import numpy as np

def select_manual_roi(frame, max_width=1200):
    h, w = frame.shape[:2]
    scale = 1.0
    if w > max_width:
        scale = max_width / w
        frame_vis = cv2.resize(
            frame,
            (int(w * scale), int(h * scale))
        )
    else:
        frame_vis = frame.copy()
    cv2.destroyAllWindows()

    roi = cv2.selectROI(
        "Select ROI (Heart region)",
        frame_vis,
        fromCenter=False,
        showCrosshair=True
    )
    cv2.destroyAllWindows()

    x, y, rw, rh = roi
    if w == 0 or h == 0:
        print("ROI inválido")
        return None
    rw = int((x + rw)/scale)
    rh = int((y + rh)/scale)
    x1 = int(x/scale)
    y2 = int(y/scale)
    return x1, y2, rw, rh

def crop_video_manual_roi(infile, outfile):
    cap = cv2.VideoCapture(infile)
    fps = cap.get(cv2.CAP_PROP_FPS)
    cap.set(cv2.CAP_PROP_POS_FRAMES, int(60 * fps))
    ret, frame = cap.read()

    if not ret:
        print("Could not read first frame")
        return
    
    # 🔹 Selección manual SOLO UNA VEZ
    roi = select_manual_roi(frame)
    if roi is None:
        return
    print("ROI selected:", roi)
    roi_left, roi_top, roi_right, roi_bottom = roi

    fps = cap.get(cv2.CAP_PROP_FPS)

    out = cv2.VideoWriter(
        outfile,
        cv2.VideoWriter_fourcc(*'mp4v'),
        fps,
        (OUT_W, OUT_H)
    )

    cap.set(cv2.CAP_PROP_POS_FRAMES, 0)

    while True:
        ret, frame = cap.read()
        if not ret:
            break

        crop = frame[roi_top:roi_bottom, roi_left:roi_right]
        if crop.size == 0:
            continue

        crop_resized = cv2.resize(
            crop,
            (OUT_W, OUT_H),
            interpolation=cv2.INTER_AREA
        )

        out.write(crop_resized)

    cap.release()
    out.release()
    print("Manual ROI video saved:", outfile)

import os

folder = r"C:\Users\jandr\Videos\videos prueba\video_processed"

exts = [".mp4", ".avi", ".mov", ".mkv"]

for file in os.listdir(folder):

    # Saltar archivos que ya fueron procesados
    if "_cropped" in file or "_processed" in file:
        continue

    if not any(file.lower().endswith(e) for e in exts):
        continue

    infile = os.path.join(folder, file)
    print(f"\n==============================")
    print(f" Processing new file: {file}")
    print(f"==============================")

    base, ext = os.path.splitext(file)

    cropped_file = os.path.join(folder, f"{base}_cropped{ext}")
    processed_file = os.path.join(folder, f"{base}_processed.avi")

    
    #Recortar ROI
    crop_video_manual_roi(infile, cropped_file)

    # 2️ Trim del recorte
    frames, fps = trim_capture(cropped_file)
    if frames is None:
        print("Skipping file with error.")
        continue

    # 3️Filtrado y normalización
    frames_gray = preprocess_frames(frames)

    height, width = frames_gray[0].shape
    fourcc = cv2.VideoWriter_fourcc(*'MJPG')  # AVI codec
    out_gray = cv2.VideoWriter(processed_file, fourcc, fps, (width, height), isColor=False)

    for f in frames_gray:
      out_gray.write(f)

    out_gray.release()
    os.remove(cropped_file)
    print(f"Final file saved (AVI): {processed_file}")
   
