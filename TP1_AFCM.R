library(FSelector)

data= iris
head(iris)
dataset=iris
### On cr�e une nouvelle colonne pour chaque valeur de la variable cat�gorielle
for(unique_value in unique(dataset$Species)){
 dataset[paste("Species", unique_value, sep = ".")] <- ifelse(dataset$Species== unique_value, 1, 0)
}
head(dataset)
#resultat
> head(dataset)
  Sepal.Length Sepal.Width Petal.Length Petal.Width Species Species.setosa
1          5.1         3.5          1.4         0.2  setosa              1
2          4.9         3.0          1.4         0.2  setosa              1
3          4.7         3.2          1.3         0.2  setosa              1
4          4.6         3.1          1.5         0.2  setosa              1
5          5.0         3.6          1.4         0.2  setosa              1
6          5.4         3.9          1.7         0.4  setosa              1
  Species.versicolor Species.virginica
1                  0                 0
2                  0                 0
3                  0                 0
4                  0                 0
5                  0                 0
6                  0                 0
#commentaire: 
Appliquation de lAFCM sur species
##
 dataset$Species<- NULL
###install(varhandle)
library(varhandle)
library(mlbench)
data(Glass)
data<-Glass
variable <- Glass$Type
data$variable <- unfactor(data$variable )
>data$variable <- unfactor(data$variable )
Erreur dans unfactor(data$variable) : 
  Please provide the obj which can be a matrix, data.frame or a vector.
####################################################################
dataset$Petal.Length <- unfactor(dataset$Petal.Length )
#resultat 
>dataset$Petal.Length <- unfactor(dataset$Petal.Length )
Erreur dans unfactor(dataset$Petal.Length) : 
  Please provide the obj which can be a matrix, data.frame or a vector. The provided obj has the class of numeric
> 
#comment
les variables numeriques ne sont pas factoris�es
##########################################################################
#Mettre les colonnes 1 et 2 � la m�me �chelle
data[,1:2] = scale(data[,1:2])
inspect.na(data)

> inspect.na(data)
NULL
> 
##Remplacer en donn�ees binaires :
# convert to dummy
binary_species <- to.dummy(data$Type, "Type") 
data.frame 
> data.frame 
function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE, 
    fix.empty.names = TRUE, stringsAsFactors = FALSE) 
{
    data.row.names <- if (check.rows && is.null(row.names)) 
        function(current, new, i) {
            if (is.character(current)) 
                new <- as.character(new)
            if (is.character(new)) 
                current <- as.character(current)
            if (anyDuplicated(new)) 
                return(current)
            if (is.null(current)) 
                return(new)
            if (all(current == new) || all(current == "")) 
                return(new)
            stop(gettextf("mismatch of row names in arguments of 'data.frame', item %d", 
                i), domain = NA)
        }
    else function(current, new, i) {
        if (is.null(current)) {
            if (anyDuplicated(new)) {
                warning(gettextf("some row.names duplicated: %s --> row.names NOT used", 
                  paste(which(duplicated(new)), collapse = ",")), 
                  domain = NA)
                current
            }
            else new
        }
        else current
    }
    object <- as.list(substitute(list(...)))[-1L]
    mirn <- missing(row.names)
    mrn <- is.null(row.names)
    x <- list(...)
    n <- length(x)
    if (n < 1L) {
        if (!mrn) {
            if (is.object(row.names) || !is.integer(row.names)) 
                row.names <- as.character(row.names)
            if (anyNA(row.names)) 
                stop("row names contain missing values")
            if (anyDuplicated(row.names)) 
                stop(gettextf("duplicate row.names: %s", 
                  paste(unique(row.names[duplicated(row.names)]), 
                    collapse = ", ")), domain = NA)
        }
        else row.names <- integer()
        return(structure(list(), names = character(), row.names = row.names, 
            class = "data.frame"))
    }
    vnames <- names(x)
    if (length(vnames) != n) 
        vnames <- character(n)
    no.vn <- !nzchar(vnames)
    vlist <- vnames <- as.list(vnames)
    nrows <- ncols <- integer(n)
    for (i in seq_len(n)) {
        xi <- if (is.character(x[[i]]) || is.list(x[[i]])) 
            as.data.frame(x[[i]], optional = TRUE, stringsAsFactors = stringsAsFactors)
        else as.data.frame(x[[i]], optional = TRUE)
        nrows[i] <- .row_names_info(xi)
        ncols[i] <- length(xi)
        namesi <- names(xi)
        if (ncols[i] > 1L) {
            if (length(namesi) == 0L) 
                namesi <- seq_len(ncols[i])
            vnames[[i]] <- if (no.vn[i]) 
                namesi
            else paste(vnames[[i]], namesi, sep = ".")
        }
        else if (length(namesi)) {
            vnames[[i]] <- namesi
        }
        else if (fix.empty.names && no.vn[[i]]) {
            tmpname <- deparse(object[[i]], nlines = 1L)[1L]
            if (startsWith(tmpname, "I(") && endsWith(tmpname, 
                ")")) {
                ntmpn <- nchar(tmpname, "c")
                tmpname <- substr(tmpname, 3L, ntmpn - 1L)
            }
            vnames[[i]] <- tmpname
        }
        if (mirn && nrows[i] > 0L) {
            rowsi <- attr(xi, "row.names")
            if (any(nzchar(rowsi))) 
                row.names <- data.row.names(row.names, rowsi, 
                  i)
        }
        nrows[i] <- abs(nrows[i])
        vlist[[i]] <- xi
    }
    nr <- max(nrows)
    for (i in seq_len(n)[nrows < nr]) {
        xi <- vlist[[i]]
        if (nrows[i] > 0L && (nr%%nrows[i] == 0L)) {
            xi <- unclass(xi)
            fixed <- TRUE
            for (j in seq_along(xi)) {
                xi1 <- xi[[j]]
                if (is.vector(xi1) || is.factor(xi1)) 
                  xi[[j]] <- rep(xi1, length.out = nr)
                else if (is.character(xi1) && inherits(xi1, "AsIs")) 
                  xi[[j]] <- structure(rep(xi1, length.out = nr), 
                    class = class(xi1))
                else if (inherits(xi1, "Date") || inherits(xi1, 
                  "POSIXct")) 
                  xi[[j]] <- rep(xi1, length.out = nr)
                else {
                  fixed <- FALSE
                  break
                }
            }
            if (fixed) {
                vlist[[i]] <- xi
                next
            }
        }
        stop(gettextf("arguments imply differing number of rows: %s", 
            paste(unique(nrows), collapse = ", ")), domain = NA)
    }
    value <- unlist(vlist, recursive = FALSE, use.names = FALSE)
    vnames <- as.character(unlist(vnames[ncols > 0L]))
    if (fix.empty.names && any(noname <- !nzchar(vnames))) 
        vnames[noname] <- paste0("Var.", seq_along(vnames))[noname]
    if (check.names) {
        if (fix.empty.names) 
            vnames <- make.names(vnames, unique = TRUE)
        else {
            nz <- nzchar(vnames)
            vnames[nz] <- make.names(vnames[nz], unique = TRUE)
        }
    }
    names(value) <- vnames
    if (!mrn) {
        if (length(row.names) == 1L && nr != 1L) {
            if (is.character(row.names)) 
                row.names <- match(row.names, vnames, 0L)
            if (length(row.names) != 1L || row.names < 1L || 
                row.names > length(vnames)) 
                stop("'row.names' should specify one of the variables")
            i <- row.names
            row.names <- value[[i]]
            value <- value[-i]
        }
        else if (!is.null(row.names) && length(row.names) != 
            nr) 
            stop("row names supplied are of the wrong length")
    }
    else if (!is.null(row.names) && length(row.names) != nr) {
        warning("row names were found from a short variable and have been discarded")
        row.names <- NULL
    }
    class(value) <- "data.frame"
    if (is.null(row.names)) 
        attr(value, "row.names") <- .set_row_names(nr)
    else {
        if (is.object(row.names) || !is.integer(row.names)) 
            row.names <- as.character(row.names)
        if (anyNA(row.names)) 
            stop("row names contain missing values")
        if (anyDuplicated(row.names)) 
            stop(gettextf("duplicate row.names: %s", paste(unique(row.names[duplicated(row.names)]), 
                collapse = ", ")), domain = NA)
        row.names(value) <- row.names
    }
    value
}
<bytecode: 0x00000000311ad960>
<environment: namespace:base>

head(binary_species)
> head(binary_species)
     Type.1 Type.2 Type.3 Type.5 Type.6 Type.7
[1,]      1      0      0      0      0      0
[2,]      1      0      0      0      0      0
[3,]      1      0      0      0      0      0
[4,]      1      0      0      0      0      0
[5,]      1      0      0      0      0      0
[6,]      1      0      0      0      0      0
> 

Convertir un facteur en classe r�elle :
# use vactor as input 
species <- unfactor(iris$Species) 
# check the class 
class(species)

