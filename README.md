# AncestryPainter

AncestryPainter is a graphing tool developed by Qidi Feng and Dongsheng Lu in 2018 (https://doi.org/10.1016/j.gpb.2018.05.002). It can visualize the ancestry composition and genetic difference, and merge ancestry proportion matrix output by ancestry inference tools like ADMIXTURE.

Since Aug, 2021, a new version of AncestryPainter had been developed by Shuanghui Chen and her collaborators. Now the related manuscript is published.

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

## Update log
### Note on Halo/Bleeding Effect in Sector Plots
Some users of AncestryPainter 2.0 have reported a visual artifact in the sector plot, where the outer ring shows a faint halo or color bleeding from the last ancestry component in the ancestry matrix.

This issue typically occurs when the last ancestry component has an extremely small proportion but is still plotted by default (even if as thin as a line).

To address this, we introduced a new parameter, ```pop.thr```, which defines a minimum ancestry proportion threshold for the components in the matrix. For each individual, if any ancestry component???s proportion is below this threshold, it will be ignored and not plotted.

The threshold is applied to all components to prevent the halo effect from shifting to the second-to-last one.

Default value of the ```pop.thr``` is 5e-5. Please avoid setting this value too big (more than 1e-4 for our example data), as it may exclude meaningful ancestry components and lead to a jagged edge effect. 

If the halo effect does not affect your visualization, you can disable this filtering by setting pop.thr = 0.

## Citation & Contact
If you use AncestryPainterV2 in your project, please cite:

Chen S, Lei C, Zhao X, Pan Y, Lu D, Xu S. (2024) AncestryPainter 2.0: Visualizing Ancestry Composition and Admixture History Graph. Genome Biol Evol. 16(11):evae249. doi: 10.1093/gbe/evae249.

Our software is available at https://github.com/Shuhua-Group/AncestryPainterV2

If you have any questions or suggestions, welcome to contact us: chenshh@shanghaitech.edu.cn or xushua@fudan.edu.cn.