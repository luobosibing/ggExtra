\name{ggarrange}
\alias{ggarrange}
\title{Arrange ggplot2 objects to figure matrix.}
\usage{
	ggarrange(piclist = NULL, matrixmap = NULL, 
		widrow = rep(1/nrow(matrixmap),nrow(matrixmap)),
      	widcol = rep(1/ncol(matrixmap),ncol(matrixmap)))
}
\arguments{

	\item{piclist}{A list which contents all the ggplot objects you want to plot.}
	\item{matrixmap}{An array uesd to arrange the plotting sub field.}
	\item{widrow}{A vector to control the width of every row(s).}
	\item{widcol}{A vector to control the width of every column(s).}

}

\description{
	A function is to arrange several ggplot objects to one plotting issue.
}

\details{
	The fuction is useful to draw several sub-figures into one figure matrix or even more complex, it similar to Graphic package fuction par.

	An exmaple is to follow here:

	If you want to draw 3 pctures in 2 * 2 subregions like following,
    
               |      (Picture 1)      |
               |(Picture 2) (Picture 3)|

    you should first set the \emph{matrixmap} as

                    [1, 1;
                     2, 3],

    that is to say matrixmap = matrix(c(1, 2, 1, 3),nrow = 2)
        
    If you want (the width of 1st row) : (the width of 2rd row) = 1: 2,  you should set \emph{widrow} = c(1/3, 2/3).
    Finally, result is ggarrange(YOURPLOTTINGLIST, 
                                 matrix(c(1,2,1,3), nrow = 2),
                                 c(1/3, 2/3))
        
}

\examples{
\donttest{
# Some picture you want to draw together
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(shape = factor(cyl))) + scale_shape(solid = FALSE)

m <- ggplot(movies, aes(y=votes, x=year)) + stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max")

set.seed(6298)
diamonds_small <- diamonds[sample(nrow(diamonds), 1000), ]
ggplot(diamonds_small, aes(x=price)) + geom_bar()
hist_cut <- ggplot(diamonds_small, aes(x=price, fill=cut)) + geom_bar(position="dodge")

pp <- list(p, m, hist_cut)

# Use default setting and draw with matrix [1, 1; 2, 3]
ggarrange(pp, matrix(c(1,2,1,3), nrow = 2))

# Change the balance of rows to 1 : 2
ggarrange(pp, matrix(c(1,2,1,3), nrow = 2), widrow = c(1/3, 2/3))
# Change also the colums
ggarrange(pp, matrix(c(1,2,1,3), nrow = 2), widrow = c(1/3, 2/3), widcol = c(3/4, 1/4))
}	
}
\seealso{
	
}