# Quantitative Macro-Analysis of Character Modeling in Pride and Prejudice

This repository contains the computational pipeline and reporting assets for tracking character trajectory shifts across Jane Austen's *Pride and Prejudice* using Local Mahalanobis Distance ($D^2$).

## 🚀 Repository Structure
* `/paper-template`: Houses the standardized `main.tex` template for reporting micro-contextual anomalies, heatmap distributions, and volumetric variance across the components.
* `/src`: Contains the R processing engines utilized to clean raw text inputs, construct the dynamic moving average vectors ($\boldsymbol{\mu}_t$), and calculate the final covariance matrices ($\boldsymbol{\Sigma}_t^{-1}$).

## 🧠 The Foundational Catalyst: Interiority (I)
The entire analytical framework deployed in this repository was inspired by a **Phase 1 Pilot Study tracking Character Interiority Architecture (I)**. This initial layer mapped the distribution of internal monologues, silent psychological reflections, and unuttered emotional states. 

Because tracking internal psychological tension proved highly successful at isolating narrative turning points, the pipeline was scaled up to systematically replicate this math across five external behavioral components. The original Interiority scripts and visualization outputs are preserved here as the methodological baseline.

## 📊 Components Tracked
1. **Name (Mentions) (N)** — Explicit text matrix counts.
2. **Action (A)** — Corporal movement and spatial displacement.
3. **Communication (C)** — Direct dialogue and monologue weight.
4. **Description by Narrator (DN)** — Editorial summaries and focalization.
5. **Discussion of Character by Other Characters (DC)** — Social reputational tracking and gossip.

## 🛠️ Replication Guide
1. Run the respective R script within `/src` to output the corresponding vector metrics and export figures (`Nfigure2.pdf`, `Afigure2.pdf`, etc.).
2. Compile `main.tex` using any standard LaTeX engine (e.g., pdfLaTeX, Overleaf) to generate the complete analytical reference report.
