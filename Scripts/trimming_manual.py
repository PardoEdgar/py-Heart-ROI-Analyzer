"""
Creator: E. Alejandro Pardo-Sarmiento
Affiliation: Escuela de Ciencias e Ingeniería,
             Universidad del Rosario,
             Bogotá, Colombia.

Functions:
-trim_video("Trimming videos")
-preprocess_frames("Converted gray videos")
-select_manual_roi("Select region of interest")
-crop_video_manual_roi("Crop the region of interest")
"""

import os
import cv2
import numpy as np
from collections import deque

Out_width = 768
Out_height = 540
folder = r"C:\Users\jandr\Videos\test\High_Vega_Up"
exts = [".mp4", ".avi"]


def trim_video(infile, start_s=30, duration_s=60, target_fps=25):
    video = cv2.VideoCapture(infile)
    if not video.isOpened():
        print("Could not open the video.")
        return None

    video_fps = video.get(cv2.CAP_PROP_FPS)
    print(f"Video fps: {video_fps}")

    start_frame = int(start_s * video_fps)
    video.set(cv2.CAP_PROP_POS_FRAMES, start_frame)
    frames = []
    t = 0
    dt = 1 / target_fps
    duration = duration_s

    while t < duration:
        frame_id = int((start_s + t) * video_fps)
        video.set(cv2.CAP_PROP_POS_FRAMES, frame_id)
        ret, f = video.read()
        if not ret:
            break
        frames.append(f)
        t += dt

    video.release()
    print(f"Captured frames: {len(frames)} at {target_fps} fps")
    return frames, target_fps


def preprocess_frames(frames):
    out = []
    for f in frames:
        gray = cv2.cvtColor(f, cv2.COLOR_BGR2GRAY)
        out.append(gray)
        print("Processing", f)
    return out


def select_manual_roi(frame, max_width=1200):
    h, w = frame.shape[:2]
    scale = 1
    if w > max_width:
        scale = max_width / w
        frame_vis = cv2.resize(frame, (int(w * scale), int(h * scale)))
    else:
        frame_vis = frame.copy()
    cv2.destroyAllWindows()

    roi = cv2.selectROI(
        "Select ROI in shell", frame_vis, fromCenter=False, showCrosshair=True
    )
    cv2.destroyAllWindows()

    x, y, rw, rh = roi
    if rw == 0 or rh == 0:
        print("Invalid ROI")
        return None
    rw = int((x + rw) / scale)
    rh = int((y + rh) / scale)
    x1 = int(x / scale)
    y2 = int(y / scale)
    return x1, y2, rw, rh


def crop_video_manual_roi(infile, outfile):
    video = cv2.VideoCapture(infile)
    fps = video.get(cv2.CAP_PROP_FPS)
    video.set(cv2.CAP_PROP_POS_FRAMES, int(60 * fps))
    ret, frame = video.read()

    if not ret:
        print("Could not read first frame")
        return

    roi = select_manual_roi(frame)
    if roi is None:
        return
    print("ROI selected:", roi)
    roi_left, roi_top, roi_right, roi_bottom = roi

    fps = video.get(cv2.CAP_PROP_FPS)

    out = cv2.VideoWriter(
        outfile, cv2.VideoWriter_fourcc(*"mp4v"), fps, (Out_width, Out_height)
    )
    video.set(cv2.CAP_PROP_POS_FRAMES, 0)

    while True:
        ret, frame = video.read()
        if not ret:
            break
        crop = frame[roi_top:roi_bottom, roi_left:roi_right]
        if crop.size == 0:
            continue
        crop_resized = cv2.resize(
            crop, (Out_width, Out_height), interpolation=cv2.INTER_AREA
        )
        out.write(crop_resized)

    video.release()
    out.release()
    print("Manual ROI video saved:", outfile)


for file in os.listdir(folder):
    if "_cropped" in file or "_processed" in file:
        continue

    if not any(file.lower().endswith(e) for e in exts):
        continue

    infile = os.path.join(folder, file)
    print(f" Processing new file: {file}")

    base, ext = os.path.splitext(file)

    cropped_file = os.path.join(folder, f"{base}_cropped{ext}")
    processed_file = os.path.join(folder, f"{base}_processed.avi")

    crop_video_manual_roi(infile, cropped_file)

    frames, fps = trim_video(cropped_file)
    if frames is None:
        print("Skipping file with error")
        continue

    frames_gray = preprocess_frames(frames)

    height, width = frames_gray[0].shape
    fourcc = cv2.VideoWriter_fourcc(*"MJPG")
    out_gray = cv2.VideoWriter(
        processed_file, fourcc, fps, (width, height), isColor=False
    )

    for f in frames_gray:
        out_gray.write(f)

    out_gray.release()
    os.remove(cropped_file)
    print(f"Final file saved as: {processed_file}")
