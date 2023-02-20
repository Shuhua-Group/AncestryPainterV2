# AncestryPainter

AncestryPainter is a graphing tool developed by Qidi Feng and Dongsheng Lu in 2018 (https://doi.org/10.1016/j.gpb.2018.05.002). It can visualize the ancestry composition and genetic difference, and merge ancestry proportion matrix output by ancestry inference tools like ADMIXTURE.

Since Aug, 2021, a new version of AncestryPainter has been developed by Shuanghui Chen with her collaborators. Now this project is still ongoing.

See DESCRIPTION for more details about our team!
## Installation
You can install our software by devtools given good network connectivity.
```
devtools::install_github("Shuhua-Group/AncestryPainterV2")
```
Alternatively, you can obtain the compressed package manually, and install it by R command like:
```
install.packages("/path/to/the/package/AncestryPainterV2.0.0.tar.gz")
```
 
## Example

Here are shown the source Code and example files of AncestryPainterV2.

### Ancestry Composition


```
# input
exp_q <- read.table("./inst/extdata/exp_ances.K8.2.Q", header = F)
exp_ind <- read.table('./inst/extdata/exp_ances.K8.2.ind', header = F)
exp_order <- read.table('./inst/extdata/exp_ances.K8.2.order', stringsAsFactors = F, header = F)
exp_cols <- read.table('./inst/extdata/exp_ances.K8.2.color', stringsAsFactors = F, header = F)$V1

# Graphing
pdf("exp_ances.K8.2.pdf", width = 45, height = 45)
  sectorplot(Q = exp_q, ind = exp_ind, target = c("Yoruba", "French", "Han"), poporder = exp_order$V1, pop.lab.col = exp_order$V2, ancescols = exp_cols, tarang1 = 90, tarang2 = 330, legend_mode = T)
dev.off()
```
![](inst/figures/exp_ances.8.jpg)<!-- -->


### Genetic Difference
```
# input
> exp_fst.local <- read.table("./inst/extdata/exp_fst.local.txt", header = F)

# Graphing
> pdf("exp_fst.local.8.pdf", width = 10, height = 10)
>  radiationplot(data = exp_fst.local, target = "KGZ", 
>                legend_mode = T, sorting = T, layers = c(0.03, 0.06, 0.09, 0.12))
> dev.off()
```
![](inst/figures/exp_fst.local.png)<!-- -->


### Ancestry Merging

```
# input
> ancfiles <- list.files("./inst/extdata/", pattern = "[0-9]\\.ancestry", full.names = T)

# Merging
> results <- ancmerge(tar_anc_filelist = ancfiles, ref = ancfiles[1], K = 8)
Time:
2023-01-22 20:00:05
Path:
/home/users/yourpath/AncestryPainterV2
Done.

# Return
> str(results)
List of 4
 $ merged_ancestry   :'data.frame':     2422 obs. of  10 variables:
  ..$ 1 : Factor w/ 2422 levels "abh107","abh119",..: 422 424 430 439 433 431 429 426 438 436 ...
  ..$ 2 : Factor w/ 212 levels "Egyptian","Moroccan_Jew",..: 1 1 1 1 1 1 1 1 1 1 ...
  ..$ 3 : num [1:2422] 0.1166 0.1146 0.1005 0.1238 0.0822 ...
  ..$ 4 : num [1:2422] 0.1253 0.1139 0.0933 0.097 0.0975 ...
  ..$ 5 : num [1:2422] 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 1e-05 ...
  ..$ 6 : num [1:2422] 0.00846 0.00001 0.01142 0.00701 0.00413 ...
  ..$ 7 : num [1:2422] 1.00e-05 4.88e-03 1.00e-05 1.05e-05 1.00e-05 ...
  ..$ 8 : num [1:2422] 0.00001 0.00543 0.00001 0.00001 0.00619 ...
  ..$ 9 : num [1:2422] 0.0867 0.0722 0.1053 0.0826 0.1153 ...
  ..$ 10: num [1:2422] 0.663 0.689 0.689 0.69 0.695 ...
 $ supporting_ratio  :'data.frame':     8 obs. of  4 variables:
  ..$ component     : chr [1:8] "comp1" "comp2" "comp3" "comp4" ...
  ..$ represent_pop : chr [1:8] "Yoruba" "Hadza" "Ju_hoan_North" "Papuan" ...
  ..$ support_counts: int [1:8] 10 2 10 10 10 2 10 10
  ..$ support_ratio : num [1:8] 1 0.2 1 1 1 0.2 1 1
 $ consensus_filelist: chr [1:2] "./inst/extdata//ances.8.10.ancestry" "./inst/extdata//ances.8.4.ancestry"
 $ conflict_filelist : chr [1:8] "./inst/extdata//ances.8.1.ancestry" "./inst/extdata//ances.8.2.ancestry" "./inst/extdata//ances.8.3.ancestry" "./inst/extdata//ances.8.5.ancestry" ...

> results$merged_ancestry[1:5, 1:5]
            1        2         3         4     5
1      Egypt1 Egyptian 0.1165650 0.1253105 1e-05
2     Egypt11 Egyptian 0.1146445 0.1138775 1e-05
3 Egypt22TD21 Egyptian 0.1005125 0.0932620 1e-05
4 Egypt9AQ177 Egyptian 0.1238020 0.0970490 1e-05
5 Egypt5AQ172 Egyptian 0.0821510 0.0974555 1e-05

> results$supporting_ratio
  component represent_pop support_counts support_ratio
1     comp1        Yoruba             10           1.0
2     comp2         Hadza              2           0.2
3     comp3 Ju_hoan_North             10           1.0
4     comp4        Papuan             10           1.0
5     comp5         Chane             10           1.0
6     comp6        Korean              2           0.2
7     comp7          Mala             10           1.0
8     comp8     Sardinian             10           1.0
 
# Saving output
> write.table(results$merged_ancestry, "merged_ancestry.txt", sep = "\t", col.names = F, row.names = F, quote = F)
```

