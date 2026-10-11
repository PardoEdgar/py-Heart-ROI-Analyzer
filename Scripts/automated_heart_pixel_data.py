"""
Author: Edgar Alejandro Pardo-Sarmiento (2026)
        Escuela de Ciencias e Ingeniería,
        Universidad del Rosario, Colombia

Description:
    This program automates the extraction of pixel-based physiological data from cardiac video recordings
    across multiple experimental treatments. It integrates video processing, user-defined anatomical
    region selection, and an ImageJ macro execution to obtain pixel intensity measurements.

Workflow:
    1. Video files are read and basic metadata are extracted.
    2. A region of interest (ROI) corresponding to the cardiac structure is defined.
    3. Videos are cropped conserving the ROI.
    4. ImageJ macro is executed to extract pixel intensity over time.

Functions:
    main()
        Executes the full analysis pipeline across all experimental treatments and video files.
    load_video_frame()
        Loads video files and retrieves reference frames.
    Select_ROI()
        Allows user-defined selection of the anatomical region of interest (ROI).
    ImageJ()
        Interfaces with ImageJ to execute macros for automated pixel intensity extraction.
    manual_review()
        After visual inspection of the resulting signal, the user can choose to:
        (y) accept the ROI and continue to the next video,
        (r) repeat the ROI selection for the current video, or
        (q) quit the program and analysis.
"""

import os
import subprocess
import cv2
import sys


FIJI_PATH = r"C:\Users\jandr\Documents\fiji-win64\Fiji.app\fiji-windows-x64.exe"
MACRO_PATH = r"analyze_videos_template.ijm"
BASE_DIRECTORY = r"C:\Users\jandr\Videos\videos prueba\Extracting"
OUT_DIRECTORY = r"C:\Users\jandr\Videos\videos prueba\data_extraction"

Out_width = 768
Out_height = 540


def main():
    for treatment in os.listdir(BASE_DIRECTORY):
        treatment_dir = os.path.join(BASE_DIRECTORY, treatment)
        if not os.path.join(treatment_dir):
            continue
        print(f" Treatment: {treatment}")

        for file in os.listdir(treatment_dir):
            print("Found file:", file)
            if not file.lower().endswith((".mp4", ".avi")):
                continue
            if "_cropped_ROI" in file:
                continue
            infile = os.path.join(treatment_dir, file)
            base, ext = os.path.splitext(file)
            outfile = os.path.join(treatment_dir, f"{base}_cropped_ROI.avi")
            print(f" Processing new file: {file,ext}")
            accept = False
            while accept == False:
                Video, fps, frame_vis, scale = load_video_frame(infile)
                select_ROI(Video, frame_vis, scale, fps, outfile)
                imageJ(outfile)
                decision = manual_review()
                if decision == "y":
                    accept = True
                    print("ROI accepted, moving to next video")

                elif decision == "r":
                    accept = False
                    print("Repeating ROI selection for this video")

                elif decision == "q":
                    print("Analysis stopped by user")
                    sys.exit()


def load_video_frame(infile, max_width=1200):
    Video = cv2.VideoCapture(infile)
    if not Video.isOpened():
        raise OSError(f"Cannot open video file: {infile}")
    fps = Video.get(cv2.CAP_PROP_FPS)
    ret, frame = Video.read()
    if not ret:
        raise ValueError("Could not read frame")

    h, w = frame.shape[:2]
    scale = 1.0
    if w > max_width:
        scale = max_width / w
        frame_vis = cv2.resize(frame, (int(w * scale), int(h * scale)))
    else:
        frame_vis = frame.copy()
    Video.set(cv2.CAP_PROP_POS_FRAMES, 0)
    return Video, fps, frame_vis, scale


def select_ROI(Video, frame_vis, scale, fps, outfile):
    roi = cv2.selectROI(
        "Select ROI (Heart region)", frame_vis, fromCenter=True, showCrosshair=True
    )
    cv2.destroyAllWindows()
    x, y, w, h = roi
    x0 = int(x / scale)
    y0 = int(y / scale)
    x1 = int((x + w) / scale)
    y1 = int((y + h) / scale)
    out = cv2.VideoWriter(
        outfile, cv2.VideoWriter_fourcc(*"MJPG"), fps, (Out_width, Out_height)
    )
    while True:
        ret, frame = Video.read()
        if not ret:
            break
        crop = frame[y0:y1, x0:x1]
        if crop.size == 0:
            continue
        crop_resized = cv2.resize(
            crop, (Out_width, Out_height), interpolation=cv2.INTER_AREA
        )
        out.write(crop_resized)
    Video.release()
    out.release()
    return out


def imageJ(video_path):
    macro_args = f"{video_path},{OUT_DIRECTORY}"
    command = [
        FIJI_PATH,
        "-macro",
        MACRO_PATH,
        macro_args,
    ]
    subprocess.run(command, check=True)


def manual_review():
    while True:
        decision = input(
            "Accept ROI and continue? [y = yes | r = redo ROI | q = quit]: "
        ).lower()

        if decision in ["y", "r", "q"]:
            return decision


if __name__ == "__main__":
    main()
