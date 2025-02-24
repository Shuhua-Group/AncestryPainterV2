# AncestryPainter

AncestryPainter is a graphing tool developed by Qidi Feng and Dongsheng Lu in 2018 (https://doi.org/10.1016/j.gpb.2018.05.002). It can visualize the ancestry composition and genetic difference, and merge ancestry proportion matrix output by ancestry inference tools like ADMIXTURE.

Since Aug, 2021, a new version of AncestryPainter has been developed by Shuanghui Chen and her collaborators. Now the related manuscript is under review.

See DESCRIPTION for more details about our team!

## Installation
You can install our software by devtools given good network connectivity.
```
devtools::install_github("Shuhua-Group/AncestryPainterV2")
```
Alternatively, you can obtain the compressed source code package manually, and install it by R command like:
```
install.packages("/path/to/the/package/AncestryPainterV2-[version_ID].tar.gz")
```
or
```
install.packages("/path/to/the/package/AncestryPainterV2-[version_ID].zip")
```

AncestryPainterV2 is developed based on ```R 3.3.3 "Another Canoe"```. The graphing and statistical functions of AncestryPainterV2 are achieved by invoking these attached base packages of R.

```
graphics
grDevices
stats
utils
```

Generally, if you install ```R``` on your device, it is no need to install or load these packages additionally.


## Usage
Please refer to the AncestryPainter_V2.manual.v5.pdf for the usage of AncestryPainter.

## Citation & Contact 

If you use AncestryPainterV2 in your project, please cite:

Chen S, Lei C, Zhao X, Pan Y, Lu D, Xu S. (2024) AncestryPainter 2.0: Visualizing Ancestry Composition and Admixture History Graph. Genome Biol Evol. 16(11):evae249. doi: 10.1093/gbe/evae249.

Our software is available at https://github.com/Shuhua-Group/AncestryPainterV2

If you have any questions or suggestions, welcome to contact us: chenshh@shanghaitech.edu.cn


