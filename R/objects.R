
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
# Requirement -------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(patchwork)
library(R6)

# demo --------------------------------------------------------------------
df <- iris %>% `colnames<-`(c('A','B','C','D','Col')) %>% mutate(id = 1:n())

plt.bar <- ggplot(df) +
  geom_bar(aes(x = id, y = A,fill = Col),stat = 'identity');plt.bar

plt.pnt <- ggplot(df) +
  geom_point(aes(A,B,color = Col))

plt.box <- ggplot(df) +
  geom_boxplot(aes(fill = Col, y = C, x = (id %% 3) %>% as.factor()))

plt.box_new <- ggplot(df) +
  geom_boxplot(aes(fill = Col, y = C,
                   x = (id %% 3) %>% as.factor())) +
  geom_point(aes(x = (id %% 3) %>% as.factor(),
                 y = C,
                 col = D),
             position = 'jitter') +
  scale_color_viridis_c() +
  ggsci::scale_fill_aaas();plt.box_new

# classes -----------------------------------------------------------------

# WISHLIST: ColMap
NoPlot <- R6Class(
  classname = 'NoPlot',
  public = list(
    Plt.Name = 'character',
    Plt.Ver  = 'character',
    Plt.Type = 'character',
    # Plt.ColMap = 'list',
    Plt.Text = 'character',
    Plt.Gg = 'ggplot',
    initialize = function(ggplot,
                          Plt.Name,
                          Plt.Ver = 'v1',
                          # Plt.ColMap,
                          Plt.Text){
      self$Plt.Gg   <- ggplot
      self$Plt.Name <- Plt.Name
      self$Plt.Ver  <- Plt.Ver
      self$Plt.Type <- sapply(ggplot$layers,function(layer) {
        class(layer$geom)[1]
      })
      self$Plt.Text <- ifelse(missing(Plt.Text),'',Plt.Text)
    },
    print = function(){
      print(self$Plt.Gg)
    }
  )
)



# Class:NoFig -------------------------------------------------------------

NoFig <- R6Class(
  classname = 'NoFig',
  public = list(
    Fig.Name = 'character',
    Fig.Text = 'character',
    Fig.Plts = 'list',
    initialize = function(Fig.Name,
                          Fig.Text = '',
                          Fig.Plts = list()){
      self$Fig.Name <- Fig.Name
      self$Fig.Text <- Fig.Text
      self$Fig.Plts <- Fig.Plts
    },
    show = function(choose = 'Fig.Name',int = T){
      if(int){
        choose <- readline('Type:\nFig.Name\nFig.Text\nFig.Plts')
        if(choose == ''){
          stop('Enter a slot')
        }
      }
      switch(choose,
             Fig.Name = self$Fig.Name,
             Fig.Text = self$Fig.Text,
             Fig.Plts = names(self$Fig.Plts))
    },
    `[[` = function(Plt.Name){
      self$Fig.Plts[[Plt.Name]]
    }
  ),
  # active ####
  active = list(
    addPlot = function(Plt) {
      Plt.Name = Plt$Plt.Name

      if(sum(Plt.Name %in% names(self$Figs.Plts)) > 0){
        stop('Plt existed')
      } else {
        self$Fig.Plts[[Plt.Name]] <- Plt
      }
    }
  )
)


# Class: NoProj -----------------------------------------------------------

NoProj <- R6Class(
  classname = 'NoProj',
  # public ####
  public = list(
    Proj.Path = 'character',
    Proj.Name = 'character',
    Proj.Text = 'character',
    Proj.Figs = 'list',
    initialize = function(Proj.Path,
                          Proj.Name,
                          Proj.Text = ''){
      Proj.Path <- normalizePath(Proj.Path)
      Proj.Path <- paste0(Proj.Path,'/NoProj_',Proj.Name) %>% gsub("\\\\", "/", .)

      # if(dir.exists(Proj.Path)){
      #   stop('Project folder existed')
      # }
      if(is.null(Proj.Name)){
        stop('Project must have a name')
      }

      self$Proj.Path <- Proj.Path
      self$Proj.Name <- Proj.Name
      self$Proj.Text <- Proj.Text
      self$Proj.Figs <- list()

      dir.create(Proj.Path)
    },
    show = function(choose = 'Proj.Path',int = T){
      if(int){
        choose <- readline('Type:\nProj.Path\nProj.Name\nProj.Text\nProj.Figs\n')
        if(choose == ''){
          stop('Enter a slot')
        }
      }

      switch(choose,
             Proj.Path = self$Proj.Path,
             Proj.Name = self$Proj.Name,
             Proj.Text = self$Proj.Text,
             Proj.Figs = names(self$Proj.Figs))
    },
    `[[` = function(Fig.Name){
      self$Proj.Figs[[Fig.Name]]
    },
    `+` = function(a,b) {paste(a,b)},
    summary = function(){
      sapply(names(self$Proj.Figs), function(Fig.Name){
        sapply(names(self[[Fig.Name]]$Fig.Plts),function(Plt.Name){
          c(Fig.Name,Plt.Name)
        })
      }) %>% bind_cols() %>% t() %>% as.data.frame(row.names = NULL) %>%
        `colnames<-`(c('Fig','Plt'))
    }
  ),
  # active ####
  active = list(
    addFig = function(Fig) {
      Fig.Name = Fig$show(int = F)

      if(sum(Fig.Name %in% names(self$Proj.Figs)) > 0){
        stop('Fig name existed')
      } else {
        self$Proj.Figs[[Fig.Name]] <- Fig
      }
    }
  )
)


# AddFig ------------------------------------------------------------------
AddFig <- function(Fig.Name,
                   Fig.Text = '',
                   NoProj){
  fig <- NoFig$new(Fig.Name,Fig.Text)
  NoProj$addFig <- fig

  dir.create(paste0(NoProj$Proj.Path,'/',Fig.Name))
}

# TEST: Success
proj <- NoProj$new(Proj.Path = '.',Proj.Name = 'Demo')
fig <- NoFig$new(Fig.Name = 'Fig1')
AddFig('Fig1','',proj)
AddFig('Fig2','',proj)
proj$show('Proj.Figs',F)

# AddPlot -----------------------------------------------------------------

AddPlt <- function(ggplot,
                   Proj,
                   Fig.Name,
                   Plt.Name,
                   Plt.Ver = 'v1',
                   # Plt.ColMap,
                   Plt.Text,
                   override = F) {
  if(missing(Fig.Name) ){
    stop('Missed Fig.Name')
  }
  if(missing(Plt.Name) ){
    stop('Missed Fig.Name')
  }
  if(!override){
    if(Plt.Name %in% names(Proj$Proj.Figs[[Fig.Name]]$Fig.Plts)){
      stop('Plot existed')
    }
  }


  noPlt <- NoPlot$new(
    ggplot,
    Plt.Name,
    Plt.Ver,
    # Plt.ColMap,
    Plt.Text)

  saveRDS(ggplot,
          file = paste0(Proj$Proj.Path,'/',
                        Fig.Name,'/',
                        Plt.Name,'_',
                        Plt.Ver,'.rds'))
  Proj$Proj.Figs[[Fig.Name]]$addPlot <- noPlt
}


# TEST: Success
AddPlt(ggplot = plt.box_new,
       Proj = proj,
       Fig.Name = 'Fig1',
       Plt.Name = 'plt.box_new',override = T)


(ggplot() + geom_histogram(aes(x = df$A))) %>%
  AddPlt(Proj = proj,
         Fig.Name = 'Fig1',
         Plt.Name = 'plt.hist',override = T)

plt.bar %>% AddPlt(proj,'Fig1','plt.bar')
plt.pnt %>% AddPlt(proj,'Fig2','plt.pnt')


tmp <- proj$summary()

# export ------------------------------------------------------------------

`[[.NoProj` = function(NoProj,...) NoProj$`[[`(...)
`[[.NoFig` = function(NoFig,...) NoFig$`[[`(...)
`+.NoProj` = function(NoProj,...) NoFig$`+`(...)


proj[['Fig1']]$Fig.Plts

tmp
# TODO
# Env check ---------------------------------------------------------------
# only one NoProj in Global env



ggplot2:::`+.gg`
