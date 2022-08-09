# Wearable_reliability

This repository cotains code and data described in the paper "On the reliability of wearable technology: A tutorial on measuring heart rate and heart rate variability in the wild". Namely,

(1) 'wearables_reliability.R' is the code for R that computes between- and within-participant reliability on data that is submitted.

(2) 'wearables_reliability_for_Biostrap_data.R' applies the general code (wearables_reliability) to the specific dataset collected during this study ('Dudarev et al 2022 data.xlsx').

(3) 'Dudarev et al 2022 data.xlsx' contains data generated during the study reported in the paper.
The two sheets in this dataset present:

(a) (Biometric_raw_data) Raw biometric data as recorded by Biostrap wristband, with participant number, date and time of the recording, and participants' state (awake or sleep).

(b) (Aggregated_biometrics_with_mood) Aggregated biometric and subjective mood data, per participant per day. Raw data was cleaned, as described in the paper, and averaged. Subjective mood data is presented for the current and previous day (yesterday).

BPM = beats per minute, a measure of heart rate
HRV = root mean square difference between successive heartbeats, a measure of heart rate variability
