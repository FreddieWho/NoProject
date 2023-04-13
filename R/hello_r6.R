# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

setwd('D:/Project/NoBrowser/Test')


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
  geom_boxplot(aes(fill = Col, y = C, x = (id %% 3) %>% as.factor())) +
  ggsci::scale_fill_aaas()

# structure & function ----------------------------------------------------

# Core principle
# + Object Oriented
# + Figure Based

# classes -----------------------------------------------------------------

NoPlot <- R6Class(
  classname = 'NoPlot',
  public = list(
    Plt.Name = 'character',
    Plt.Ver  = 'character',
    Plt.Type = 'character',
    Plt.ColMap = 'list',
    Plt.Text = 'character',
    Plt.gg = 'ggplot'
  )
)



# Class:NoFig -------------------------------------------------------------

NoFig <- R6Class(
  classname = 'NoFig',
  private = list(
    Fig.Name = 'character',
    Fig.Text = 'character',
    Fig.Plts = 'list'
  ),
  public = list(
    initialize = function(Fig.Name,
                          Fig.Text = ''){
      private$Fig.Name <- Fig.Name
      private$Fig.Text <- Fig.Text
      private$Fig.Plts <- NA
    },
    show = function(choose = 'Fig.Name',int = T){
      if(int){
        choose <- readline('Type:\nFig.Name\nFig.Text\nFig.Plts')
        if(choose == ''){
          stop('Enter a slot')
        }
      }

      switch(choose,
             Fig.Name = private$Fig.Name,
             Fig.Text = private$Fig.Text,
             Fig.Plts = names(private$Fig.Plts))
    }
  )
)


# Class: NoProj -----------------------------------------------------------

NoProj <- R6Class(
  classname = 'NoProj',
  # private ####
  private = list(
    Proj.Path = 'character',
    Proj.Name = 'character',
    Proj.Text = 'character',
    Proj.Figs = 'list'
  ),
  # public ####
  public = list(
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

      private$Proj.Path <- Proj.Path
      private$Proj.Name <- Proj.Name
      private$Proj.Text <- Proj.Text
      private$Proj.Figs <- NULL

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
             Proj.Path = private$Proj.Path,
             Proj.Name = private$Proj.Name,
             Proj.Text = private$Proj.Text,
             Proj.Figs = names(private$Proj.Figs))
    }
  ),
  # active ####
  active = list(
    addFig = function(Fig) {
      Fig.Name = Fig$show(int = F)
      private$Proj.Figs <- append(private$Proj.Figs,
                                  list(Fig) %>% `names<-`(Fig.Name))
    }
  )
)

test <- R6Class(
  classname = 'test',
  public = list(
    x = 'character'
  )
)

# Initiating --------------------------------------------------------------


proj <- NoProj$new(Proj.Path = '.',Proj.Name = 'Demo')

fig <- NoFig$new(Fig.Name = 'Fig1')

proj$show('Proj.Figs',F)
proj$addFig <- fig
proj$show('Proj.Figs',F)

fig$show(int = F)
# Add ---------------------------------------------------------------------

AddFig <- function(Proj,
                   Fig.Name,
                   Fig.Text = '') {
  if(is.null(Fig.Name) ){
    stop('Project must have a name')
  }
  if(sum(Fig.Name %in% names(Proj@Proj.Figs)) > 0){
    stop('Figure existed')
  }

  Fig <- NoFig$new(
    Fig.Name = Fig.Name,
    Fig.Text = Fig.Text)

  Proj@Proj.Figs[[Fig.Name]] <- Fig

  Proj
}

# AddFigs <- function(Proj,
#                     Fig.Name,
#                     Fig.Text = '',
#                     Fig.Plts = list()) {
#   if(is.null(Fig.Name) ){
#     stop('Project must have a name')
#   }
#   if(sum(Fig.Name %in% names(Proj@Proj.Figs)) > 0){
#     stop('Figure existed')
#   }
#
#   Fig <- NoFig(
#     Fig.Name = Fig.Name,
#     Fig.Text = Fig.Text,
#     Fig.Plts = list())
#
#
#   .proj.name <- deparse(substitute(Proj))
#   eval(parse(text = eval(expression(paste0(.proj.name,'@Figs$',Fig.Name, " <- ",
#                                            Fig)))))
# }

proj.aF <- AddFigs(proj,'Fig.1')

AddPlt <- function(Proj,
                    Fig.Name,
                    Fig.Text = '',
                    Fig.Plts = list()) {
  if(is.null(Fig.Name) ){
    stop('Project must have a name')
  }
  if(sum(Fig.Name %in% names(Proj@Proj.Figs)) > 0){
    stop('Figure existed')
  }

  Fig <- NoFig(
    Fig.Name = Fig.Name,
    Fig.Text = Fig.Text,
    Fig.Plts = list())

  Proj@Proj.Figs[[Fig.Name]] <- Fig

  Proj
}
