##############################################################################
##Function: ggarrange <- function(piclist,matrixmap,
##            widrow = rep(1/nrow(matrixmap),nrow(matrixmap)),
##            widcol = rep(1/ncol(matrixmap),ncol(matrixmap)))
##This Function is used to arrange the ggplot object plotting
##piclist is a list which contents all the ggplot objects you want to plot.
##matrixmap is an array uesd to arrange the plotting sub field. 
##widrow is a vector to control the width of every row(s).
##widcol is a vector to control the width of every column(s).
##   e.g. If you want to draw 3 pctures in 2 * 2 subregions like following,
##                |      (Picture 1)      |
##                |(Picture 2) (Picture 3)|
##           you should first set the matrixmap as
##                     [1, 1;
##                      2, 3],
##           that is to say matrixmap = matrix(c(1, 2, 1, 3),nrow = 2)
##        If you want (the width of 1st row) : (the width of 2rd row) = 1: 2
##           you should set widrow = c(1/3, 2/3).
##        Finally, result is ggarrange(YOURPLOTTINGLIST, 
##                                        matrix(c(1,1,2,3),nrow=2,
##                                        c(1/3, 2/3))
##        
##It is similar to the fuction par.
##Enjoy Your Times!!!
##Email: huangbaochenwop@yeah.net
##Tony Huang


ggarrange <- function(piclist,matrixmap,
                         widrow = rep(1/nrow(matrixmap),nrow(matrixmap)),
                         widcol = rep(1/ncol(matrixmap),ncol(matrixmap))){
  
  len.list = length(piclist)
  
  
  #########################################################################
  ## Errors dealing! ######################################################
  
  if (!(is.list(piclist))) {
    stop("Pictures List doesn't exist!") ##picture list error.
  }
  
  for (i in (1:len.list)){
    if (!is.ggplot(piclist[[i]])){
      stop("There exists at least one Object which is not a ggplot object!") ##picture objects error
    }
  }
  
  if (!is.matrix(matrixmap)){
    stop("Matrix map dosen't exist!") ##matrix error
  }
  
  if ((length(widrow) != nrow(matrixmap)) | (length(widcol) != ncol(matrixmap))) {
    stop("Length of rowlen or coolen is different from dimmension of matrixmap!") ##dimmension error
  }
  
  if ((sum(widrow) != 1) | (sum(widcol) != 1)){
    stop("Row/column width proportion error!") ##Row/column proportion error
  }
  
  
  ############################################################################
  ##Plotting!#################################################################
  
  ##New page
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow(matrixmap),
                                           ncol(matrixmap),
                                           widths = unit(widcol,"npc"),
                                           heights = unit(widrow,"npc")
                                           )))
  
  
  ##Plot pictures one by one.
  for (i in (1:length(piclist))){
    
    minfun <- function(vec){
      options(warn = -1)
      a = min(which(vec == i))
      options(warn = 1)
      a
    }
    
    maxfun <- function(vec){
      options(warn = -1)
      a = max(which(vec == i))
      options(warn = 1)
      a
    }
    
    minvec <- function(mat, n){
      a = apply(mat, n, minfun)
      a = unlist(a)
      a = a[a!= -Inf]
      resu = min(a)
      resu
    }
    
    maxvec <- function(mat, n){
      a = apply(mat, n, maxfun)
      a = unlist(a)
      a = a[a!= -Inf]
      resu = max(a)
      resu
    }
    
    
    minrow = minvec(matrixmap, 2)
    maxrow = maxvec(matrixmap, 2)
    mincol = minvec(matrixmap, 1)
    maxcol = maxvec(matrixmap, 1)
    
    
    print(piclist[[i]],
          vp = viewport(layout.pos.row = minrow:maxrow,
                        layout.pos.col = mincol:maxcol))
  }
  
}


  



