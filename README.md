AncestryPainter is a graphing tool developed by Qidi Feng and Dongsheng Lu in 2018 (https://doi.org/10.1016/j.gpb.2018.05.002).

Since Aug, 2021, a new version of AncestryPainter was developed by Shuanghui Chen with her collabraters. Now this porject is still ongoing.

Here are shown the source Code and example files of AncestryPainterV2.


# Example

## Ancestry Composition
```
sectorplot(Q = exp_q, ind = exp_ind, target = c("Yoruba", "French", "Han"), poporder = exp_order$V1, popgroup = exp_order$V2, popcols = exp_cols)
```

## Genetic Difference
```
radiationplot(data = exp_fst.local, target = "target", global_mode = T, sorting = T, layers = c(0.03, 0.06, 0.09, 0.12))
```

