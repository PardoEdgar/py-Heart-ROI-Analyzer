import cv2, numpy as np
from collections import deque
from scipy.signal import savgol_filter, find_peaks


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
def detect_shell_roi(frame):
    hsv = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)

    # Range for yelowhish shell
    lower_shell = np.array([5, 60, 80])
    upper_shell = np.array([18, 255, 255])
    mask = cv2.inRange(hsv, lower_shell, upper_shell)

    # Clean noise
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (7,7))
    mask = cv2.morphologyEx(mask, cv2.MORPH_OPEN, kernel, iterations=2)
    mask = cv2.morphologyEx(mask, cv2.MORPH_CLOSE, kernel, iterations=2)

    contours, _ = cv2.findContours(mask, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    if not contours:
        print("Shell not detected")
        return None, mask

    # Choose largest contour
    c = max(contours, key=cv2.contourArea)
    x, y, w, h = cv2.boundingRect(c)

    # ROI underneath the shell = heart región
    roi_top = max(y - int(h*0.4), 0)                 # Up
    roi_bottom = min(y + int(h*1.5), frame.shape[0]) # Down
    roi_left = max(x - int(w*0.5), 0)                # Left
    roi_right = min(x + int(w*1.5), frame.shape[1]) 

    debug = frame.copy()
    cv2.rectangle(debug, (x, y), (x+w, y+h), (0,255,0), 2)
    cv2.rectangle(debug, (roi_left, roi_top), (roi_right, roi_bottom), (255,0,0), 2)

    ##cv2.imshow("Shell + Heart ROI", debug)
    ##cv2.imshow("Shell mask", mask)
    ##cv2.waitKey(0)
    ##cv2.destroyAllWindows()

    return (roi_left, roi_top, roi_right, roi_bottom), mask

def crop_video_with_fixed_roi(infile, outfile):
    cap = cv2.VideoCapture(infile)
    ret, frame = cap.read()

    if not ret:
        print("Could not read first frame")
        return

    H, W = frame.shape[:2]

    # Fixed ROI  
    roi_top = int(H * 0.15)
    roi_bottom = int(H * 0.65)
    roi_width = int(W * 0.4)          
    roi_shell, _ = detect_shell_roi(frame)

    if roi_shell is not None:
       x1, y1, x2, y2 = roi_shell
       center_x = (x1 + x2) // 2
       print("Center shell-based:", center_x)
    else:
       center_x = W // 2
       print("Shell not detected")

    # Construcción final del ROI
    roi_left = max(0, center_x - roi_width // 2)
    roi_right = min(W, center_x + roi_width // 2)

    print(f" Fixed ROI : {roi_left}:{roi_right} x {roi_top}:{roi_bottom}")

    out = cv2.VideoWriter(
        outfile,
        cv2.VideoWriter_fourcc(*'mp4v'),
        cap.get(cv2.CAP_PROP_FPS),
        (roi_right-roi_left, roi_bottom-roi_top)
    )

    cap.set(cv2.CAP_PROP_POS_FRAMES, 0)
    while True:
        ret, frame = cap.read()
        if not ret:
            break
        crop = frame[roi_top:roi_bottom, roi_left:roi_right]
        out.write(crop)

    cap.release()
    out.release()
    print("Crop video with fixed ROI:", outfile)

import os

folder = r"C:\Users\jandr\Videos\videos prueba"

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

    
    # 1️Recortar ROI
    crop_video_with_fixed_roi(infile, cropped_file)

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
   
