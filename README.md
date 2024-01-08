# K-NN Algorithm on Greek Alphabet Classification with Hilbert Curve Linearization
This is the repository for the DSCI 100 Research Project, view the report [here](https://github.com/4g3nt81lly/DSCI-100-Project/blob/main/project_report.ipynb) (or [here](https://nbviewer.org/github/4g3nt81lly/DSCI-100-Project/blob/main/project_report.ipynb) if GitHub preview does not work).

## Introduction
The most common way of linearizing images into a single-row feature vector is the zig-zag pattern linearization that simply concatenates an image matrix row-by-row. However, this linearization method entails the loss of proximity features, i.e. pixels that are originally close together, forming certain local features, will no longer be in each other's vicinity after linearization. However, in practice, not preserving these local features might just be one of the reasons why the K-NN classification model is insufficient in practice. K-NN algorithm uses a distance function, which sees features (pixels) as independent values, in conjunction with the majority principle to determine the class of some unseen data and makes no attempt to learn from the underlying patterns among the pixels, ungracefully disregarding the structure of the data (e.g. how pixels are arranged and collectively form macroscopic features, etc.). This renders the K-NN algorithm noise-sensitive and easily gullible, because even just small perturbations, imperceptible to human vision, can greatly impact how it classifies images: it will be extremely difficult for the model to clearly distinguish two images of the same object that differ a lot from one another for almost every pixel (e.g. different lighting, different perspectives, etc.). We have already seen from practice that the performance of a K-NN classification model on the small MNIST dataset still have a long way to go before it becomes practicable. Having inspired by research attempting to implement local feature preservation with K-NN models (Amato & Falchi, 2010), we reckon that, without compromising the intuitiveness of the K-NN algorithm, choosing a linearization method that preserves local features for the K-NN classification model could potentially improve its performance in a Greek Alphabet classification task. Out of all the space-filling curves, Hilbert curve is chosen for its phenomenal locality-preserving capability (Moon et al., 2001): most, if not all, pairs of pixel points on an image mapped by a Hilbert curve will remain close after the image matrix is straightened into a one-dimensional feature vector.

### Research Question
Whether linearizing images with Hilbert Curve would improve the performance of a K-NN model on a Greek alphabet classification task.

### Dataset: Handwritten Greek Letters
This is a 24-class balanced handwritten Greek Letters dataset. It consists of 24 classes (24 Greek letters) with 240 training images (10 for each class) and 96 testing images (4 for each class). The images are greyscale and clearly legible: black pen strokes on white backgrounds. The low-resolution images (14x14) will be used for the classification task. The last column of the two `csv` files are the ground truths. Since mapping with Hilbert curve requires images of the size $2^n\times2^n, n\in\mathbb{Z}^+$ (a square image of width $N$ can be mapped by a $\log_2(N)$'th order Hilbert curve), we will be padding all images by 1 pixel for all four sides.

## Methodology

We would like to see if training K-NN classification models with images linearized with Hilbert curve would significantly and consistently improve the model's performance for all $K$ values, measured using validation accuracies. To investigate this, we will create two copies of the data set, each corresponding to a condition:
1. Regular set (control): all images within the set are regularly linearized using the classic zig-zag mapping.
2. Hilbert set (experimental): all images within the set are mapped and linearized using a 4-th order Hilbert curve.

> To avoid redundancy, the two conditions/models will be referred to as the Baseline Model/Condition (regular linearization) and the HC Model/Condition (Hilbert curve linearization) respectively thereinafter.

All feature vectors (images) in both data sets will be compressed such that every adjacent feature are averaged to form a new feature vector of the size one less than that of its original, embeding the order information within the data.

With a stratified 5-fold cross validation, we will use these two data sets to train and test two separate instances of the K-NN classification model respectively, all the while resampling with varying $K$ values within the interval $[2,51]$ at a step of $1$.

All random processes, including but not limited to data augmentation and cross validation split, will be made deterministic and reproducible by setting an arbitrarily-chosen global seed (`2023`) to eradicate test results variability due to randomness and ensure that we are always taking the same sample from the population.

Some of the details will be further elaborated.

### Hypothesis Testing
**Parameter of Interest**: The population mean difference in validation accuracies (denoted by $\Delta_{acc}$) of the Baseline Model and the HC Model.

**Null Hypothesis**: $H_0:\Delta_{acc}=0$  
There is no significant difference in population mean difference of validation accuracies of the Baseline Model and the HC Model.

**Alternative Hypothesis**: $H_A:\Delta_{acc}>0$  
There is significant improvement in the model's validation accuracies under the HC Condition compared to the Baseline Condition.

The final accuracies will be paired recorded in a 50x4 table as follows:

$$
\begin{array}{|c|c|c|c|}
    \hline
    K & \text{Baseline} & \text{HC} & \Delta_{acc} \\\\ \hline
    2 & 0.83 & 0.84 & 0.01 \\
    3 & 0.85 & 0.82 & -0.03 \\
    \vdots & \vdots & \vdots & \vdots \\
    51 & 0.78 & 0.80 & 0.02 \\\\ \hline
\end{array}
$$

Assuming that observations are randomly drawn from the population, dependent within-pair, and independent pair-to-pair, and that the sample size ($n=50$) is sufficiently large but not exceeding 10% of the population size, we will be using a right-tailed one-sample $t$-test on the mean differences in validation accuracies $\Delta_{acc}$ (a.k.a. paired $t$-test) to test for its significance. The hypothesis test will be conducted at the significance level $\alpha=0.1$ (90% confidence level). $H_0$ will be rejected only if $p<\alpha$, where $p$ is the $p$-value.

### Visualization
1. An accuracy vs. $K$ line plot will be plotted with both conditions on the same plot to not only show the change of accuracy in response to different $K$'s in both conditions but also compare the performance of the two models at different $K$'s.
2. A histogram with an overlayed density line (Kernel Density Estimation) to show the distribution of the accuracy differences.

---

... (view the full report [here](https://github.com/4g3nt81lly/DSCI-100-Project/blob/main/project_report.ipynb) or [here](https://nbviewer.org/github/4g3nt81lly/DSCI-100-Project/blob/main/project_report.ipynb))
