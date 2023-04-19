# NoProject
An R package designed for data scientists to manage ggplot2 figures. 
---

*STEP1:* Initiated a new project named <kbd>Demo</kbd> and the first figure in this project named <kbd>Fig1</kbd>
```{R}
proj <- NoProj$new(Proj.Path = '.',Proj.Name = 'Demo')
fig <- NoFig$new(Fig.Name = 'Fig1')

```

*STEP2* Add a plot named <kbd>plt.hist</kbd> into <kbd>Fig1</kbd> in <kbd>Demo</kbd> , while a RDS file was saved as <kbd> Proj.Path/Fig.Name/Plt.Name_Plt.ver.rds</kbd> 
```{R}
(ggplot() + geom_histogram(aes(x = runif(100)))) %>%
  AddPlt(Proj = proj,
         Fig.Name = 'Fig1',
         Plt.Name = 'plt.hist',override = T)
```

*STEP3* Print <kbd>plt.hist</kbd>
```{R, results = 'hold'}
proj[['Fig1']][['plt.hist']]
```
