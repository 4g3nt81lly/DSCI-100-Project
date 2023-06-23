# AUTHOR: Bingyi (Billy) Li
# CREATED: 2023-06-21


# ======= Hilbert Curve =======


#' log_2(N)-order Hilbert Curve Coordinate at Index
#'
#' Generate a coordinate of log_2(N)'th-order Hilbert Curve at a given index.
#'  Note: This iterative implementation is inspired by (this article)[https://blog.marcinchwedczuk.pl/iterative-algorithm-for-drawing-hilbert-curve.]
#' @param index  The index, within the domain [0, N^2]
#' @param N      The side length of the square formed by the Hilbert Curve.
#'
#' @return       The coordinate in a 2D euclidean plane in the form (x, y).
hilbert.coordinate <- function(index, N) {
    # base case patterns
    positions <- list(list(0, 0), # case 1
                      list(0, 1), # case 2
                      list(1, 1), # case 3
                      list(1, 0)) # case 4
    
    # +1 to use R's 1-based indexing
    coord <- positions[[(index %% 4) + 1]]

    # integer division by 4
    index <- index %/% 4

    x <- coord[[1]]
    y <- coord[[2]]

    n <- 4
    while (n <= N) {
        n.2 <- n %/% 2
        case <- index %% 4
        if (case == 0) {
            # swap x and y
            tmp <- x
            x <- y
            y <- tmp
        } else if (case == 1) {
            y <- y + n.2
        } else if (case == 2) {
            x <- x + n.2
            y <- y + n.2
        } else if (case == 3) {
            tmp <- y
            y <- (n.2 - 1) - x
            x <- (n.2 - 1) - tmp
            x <- x + n.2
        } else {
            # should NEVER get here
            stop('an unknown error has occurred')
        }
        index <- index %/% 4
        n <- n * 2
    }
    return(c(x, y))
}

#' log_2(N)-order Hilbert Curve Map
#'
#' Generate a mapping data frame (coordinates) of log_2(N)-order Hilbert Curve
#'  used for decoding images stored in that sequence.
#' Note: The coordinates are in R's 1-based indexing
#' @param N  The side length of the square formed by the Hilbert Curve.
#'
#' @return   A N^2 x 2 data frame with row-wise 2D Hilbert-Curve coordinates.
get.hilbert.map <- function(N) {
    hilbert.map <- tibble(i = seq_len(N ^ 2) - 1) |>
        rowwise() |>
        mutate(coord = paste(hilbert.coordinate(i, N) + 1, collapse = ' '),
               .keep = 'unused') |>
        separate(coord, sep = ' ', into = c('x', 'y'), convert = TRUE)
    return(hilbert.map)
}

#' Coordinate Pairs
#'
#' Generate coordinate pairs for rendering relevant graphics (e.g. segments,
#'  arrows, etc).
#' Note: For N coordinates, there will be N - 1 coordinate pairs.
#' @param coords  A data frame for coordinates with columns `x` and `y`.
#'
#' @return        An (N - 1)-row data frame with row-wise coordinate pairs.
get.coordinate.pairs <- function(coords) {
    coords |>
        lag(1) |>
        bind_cols(coords, .name_repair = 'minimal') |>
        `colnames<-`(c('x0', 'y0', 'x1', 'y1')) |>
        slice(-1)
}

#' Plot a log_2(N)-order Hilbert Curve
#'
#' Plot a log_2(N)-order Hilbert Curve with a given mapping data frame.
#' @param hilbert.map  A data frame returned by the helper function `get.hilbert.map`.
#'
#' @return             A plot with a log_2(N)-order Hilbert Curve.
hilbert.curve.demo <- function(hilbert.map) {
    N <- sqrt(nrow(hilbert.map))
    
    # convert to cartesian coordinates
    hilbert.map <- hilbert.map |>
        map_at('y', ~ N - . + 1) |>
        as_tibble()
    
    hilbert.demo.plot <- hilbert.map |>
        get.coordinate.pairs() |>
        ggplot(aes(x = x0, y = y0, xend = x1, yend = y1)) +
            # arrows: demonstrating the sequence
            geom_segment(arrow = arrow(length = unit(0.25, 'cm'), type = 'closed'),
                         color = 'darkgray') +
            # each point is mapped to a single feature
            geom_point(data = hilbert.map, aes(x = x, y = y),
                       inherit.aes = FALSE) +
            theme_bw() +
            # no grids and axes
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text = element_blank(),
                  axis.title = element_blank())
    
    return(hilbert.demo.plot)
}


# ======= Data Sets =======


#' Data Set Summary
#'
#' Return an overall class summary for a train/test split.
#' @param training.set  A training set.
#' @param testing.set   A testing set.
#'
#' @return              A summary data frame.
get.summary <- function(training.set, testing.set) {
    training.summary <- training.set |>
        group_by(label) |>
        summarize(training = n())
    testing.summary <- testing.set |>
        group_by(label) |>
        summarize(testing = n())
    
    overall.summary <- bind_cols(training.summary,
                                 testing.summary |> select(-label)) |>
        mutate(total = training + testing)
    
    return(overall.summary)
}

#' Data Set Transformation
#'
#' Transform all images of a data set by a given function.
#'  Note: This is a more general helper function for data augmentation.
#' @param data.set  A data set to be 
#' @param fn        A transformation function that takes in a feature vector
#'                   and returns a transformed feature vector.
#' @param size      A promised image size after the transformation.
#'
#' @return          A transformed data set.
transform.set <- function(data.set, fn, size = NULL, ...) {
    # get columns for image pixels ONLY
    image.set <- data.set |>
        select_if(is.numeric) |>
        as.matrix()
    # get image dimension
    image.dim <- if (is.null(size)) ncol(image.set) else size
    # a new matrix to store transformed images
    new.set <- matrix(, nrow = 0, ncol = image.dim,
                      dimnames = list(NULL, paste0('X', 1:image.dim)))
    # transform each image of the set
    for (i in seq_len(nrow(data.set))) {
        transformed <- image.set[i, ] |>
            fn(...)
        # add to new set
        new.set <- new.set |> rbind(transformed)
    }
    new.set <- as.data.frame(new.set)
    # restore labels if exists
    if ('label' %in% colnames(data.set)) {
        new.set <- mutate(new.set, label = data.set$label)
    }
    # getting rid of auto-generated row names
    rownames(new.set) <- NULL
    return(new.set)
}

#' Data Augmentation
#'
#' Generate an altered copy of the given data set with the given
#'  transformation function.
#' @param data.set  A data set to be augmented.
#' @param fn        A transformation function that takes in a matrix and
#'                   outputs a matrix.
#' @param ...       Variadic arguments passed to `fn`.
#'
#' @return          An augmented data set.
augment.set <- function(data.set, fn, ...) {
    # get image size
    image.size <- ncol(data.set |> select_if(is.numeric))
    
    transform.set(data.set, function(image.array, ...) {
        image.array |>
            # convert to matrix
            as.image.matrix(ncol = sqrt(image.size)) |>
            fn(...) |>
            as.feature.vector()
    }, ...)
}


# ======= Images =======


#' Image Matrix
#'
#' A wrapper function that converts a feature vector to an N x M matrix.
#'  Note: At least one of `nrow = ...` and `ncol = ...` should be provided.
#' @param vec  A one-dimensional feature vector.
#' @param ...  Variadic arguments passed to `matrix`, see note.
#'
#' @return          An N x M matrix.
as.image.matrix <- function(vec, ...) {
    vec |> matrix(byrow = TRUE, ...)
}

#' Feature Vector
#'
#' A wrapper function that converts an N x M matrix to a feature vector.
#'  Note: An inverse (almost) operation of `as.image.matrix`.
#' @param mtx  An N x M matrix.
#'
#' @return          A one-dimensional numeric feature vector.
as.feature.vector <- function(mtx) {
    # t() for conversion by row
    # R's default conversion does it by column
    mtx |> t() |> as.numeric()
}

#' Display Image from Data Set
#'
#' Display the i'th image from a given data set.
#'  Note: This helper function is capable of performing inverse HC
#'  linearization on demand.
#' @param data.set  A data set.
#' @param i         The index of the desired image.
#' @param n         The side length hint of the image.
#' @param hilbert   Whether the image is HC linearized.
show.image <- function(data.set, i = 1, n = 16, hilbert = FALSE, ...) {
    # get image vector from data frame at i'th row
    image.data <- data.set |>
        select_if(is.numeric) |>
        slice(i) |>
        as.numeric()
    if (hilbert) {
        # reassemble image using hilbert map
        mapping <- hilbert.map |>
            bind_cols(pixel = image.data) |>
            as.matrix()
        # blank image matrix
        image.mtx = matrix(, nrow = n, ncol = n)
        # coordinate mapping
        for (row.no. in seq_len(nrow(mapping))) {
            # convert to matrix coordinates
            x <- mapping[row.no., 'x']
            y <- mapping[row.no., 'y']
            pixel <- mapping[row.no., 'pixel']
            # store in new image matrix at (x, y)
            image.mtx[y, x] <- pixel
        }
        image.mtx |>
            rotate.image() |>
            image(col = gray(seq(0, 1, length = 256)),
                  xaxt = 'n', yaxt = 'n', cex.main = 1.5, ...)
        mtext(paste('Label:', data.set[i, 'label']),
              side = 1, line = 1, cex = 1.25)
    } else {
        image.data |>
            as.image.matrix(ncol = n) |>
            # rotate 90 degrees clockwise to display image in its original orientation
            rotate.image() |>
            image(col = gray(seq(0, 1, length = 256)),
                  xaxt = 'n', yaxt = 'n', cex.main = 1.5, ...)
        mtext(paste('Label:', data.set[i, 'label']),
              side = 1, line = 1, cex = 1.25)
    }
}

#' X-padding
#'
#' Padding a given matrix with a given number or random numbers within a given
#'  range.
#' @param mtx     A matrix to be X-padded.
#' @param n       The number of layers to pad.
#' @param x       A number, or a random number range for X-padding the matrix.
#' @param random  Whether the matrix is X-padded with random numbers.
#'                The parameter `x` will be interpreted as a range if `TRUE`.
#'
#' @return        An X-padded matrix.
X.padding <- function(mtx, n = 1, x = 1, random = FALSE) {
    if (random) {
        new.nrow <- nrow(mtx) + 2 * n
        # generate random paddings within a given range
        mtx <- runif(n * ncol(mtx), x[1], x[2]) |>
            matrix(n, ncol(mtx)) |>
            rbind(mtx) |>
            rbind(runif(n * ncol(mtx), x[1], x[2]) |>
                      matrix(n, ncol(mtx)))
        mtx <- runif(n * new.nrow, x[1], x[2]) |>
            matrix(new.nrow, n) |>
            cbind(mtx) |>
            cbind(runif(n * new.nrow, x[1], x[2]) |>
                      matrix(new.nrow, n))
        return(mtx)
    }
    # generate fixed-value paddings
    new.rows <- matrix(x, n, ncol(mtx))
    new.cols <- matrix(x, nrow(mtx) + 2 * n, n)
    mtx <- rbind(new.rows, mtx) |>
        rbind(new.rows)
    mtx <- cbind(new.cols, mtx) |>
        cbind(new.cols)
    return(mtx)
}

#' Matrix Rotation
#'
#' Perform a clockwise rotation on a given matrix by a given angle.
#' @param mtx    A matrix.
#' @param angle  The rotation angle in degrees.
#'               Note: Must be an integer multiple of 90 degrees.
#'
#' @return       A rotated matrix.
rotate.image <- function(mtx, angle = 90) {
    if (angle %% 90 == 0) {
        angle <- angle %% 360
        while (angle > 0) {
            mtx <- mtx[nrow(mtx):1, ] |> t()
            angle <- angle - 90
        }
        return(mtx)
    }
    stop('rotation angle must be an integer multiple of 90 degrees')
}

#' Additive Gaussian Noise
#'
#' Add Gaussian Noise to a given matrix.
#'  Note: By default, a standard Gaussian distribution is used.
#' @param mtx   A matrix.
#' @param mean  The mean of the Gaussian distribution.
#' @param sd    The standard deviation of the Gaussian distribution.
#'
#' @return       A matrix with noise added.
gaussian.noise <- function(mtx, mean = 0, sd = 1) {
    # generate a Gaussian filter
    gaussian.filter <- rnorm(nrow(mtx) * ncol(mtx),
                             mean = mean, sd = sd) |>
        matrix(nrow = nrow(mtx))
    return(mtx + gaussian.filter)
}
