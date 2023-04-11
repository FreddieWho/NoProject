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

# demo --------------------------------------------------------------------
df <- iris %>% `colnames<-`(c('A','B','C','D','Col')) %>% mutate(id = 1:n())

plt.bar <- ggplot(df) +
  geom_bar(aes(x = id, y = A,fill = Col),stat = 'identity');plt.bar

plt.pnt <- ggplot(df) +
  geom_point(aes(A,B,color = Col))

plt.box <- ggplot(df) +
  geom_boxplot(aes(x = Col, y = C, fill = (id %% 3) %>% as.factor()))

# structure & function ----------------------------------------------------

# Core principle
# Object Oriented
# Figure Based

project =
  (
    # Project relevant
    Proj.Name,
    Proj.Path,
    Proj.Text,

    Figs = (

      Figure(
        Fig.Name,
        Fig.Text,

        Plots = (

          Plot(
            Plt.Name,
            Plt.Type,
            Plt.ColMap,
            Plt.Text,
            Plt.gg
          )
        )
      )
    )
  )

# classes -----------------------------------------------------------------

setOldClass(c('gg','ggplot'))

NoPlot <- setClass(
  Class = 'NoPlot',
  slots = list(
    Plt.Name = 'character',
    Plt.Type = 'character',
    Plt.ColMap = 'list',
    Plt.Text = 'character',
    Plt.gg = 'ggplot'
  )
)

NoFig <- setClass(
  Class = 'NoFig',
  slots = list(
    Fig.Grp  = 'character',
    Fig.Name = 'character',
    Fig.Text = 'character',
    Fig.Plts = 'list'
  )
)

NoProj <- setClass(
  Class = 'NoProj',
  slots = list(
    Proj.Path = 'character',
    Proj.Name = 'character',
    Proj.Ver  = 'character',
    Proj.Text = 'character',
    Proj.Figs = 'list'
  )
)



# Initiating --------------------------------------------------------------

init_Proj <- function(Proj.Path,
                      Proj.Name,
                      Proj.Text = '',
                      Proj.Figs = list()) {

  Proj.Path <- normalizePath(Proj.Path)
  Proj.Path <- paste0(Proj.Path,'/NoProj_',Proj.Name) %>% gsub("\\\\", "/", .)

  if(dir.exists(Proj.Path)){
    stop('Project folder existed')
  }
  if(is.null(Proj.Name)){
    stop('Project must have a name')
  }

  dir.create(Proj.Path)

  NoProj(
    Proj.Path = Proj.Path,
    Proj.Name = Proj.Name,
    Proj.Text = Proj.Text,
    Proj.Figs = Proj.Figs
  )
}

proj <- init_Proj(Proj.Path = '.',Proj.Name = 'Demo')




