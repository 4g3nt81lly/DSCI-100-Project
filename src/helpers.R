# ======= Hilbert Curve =======

hilbert.coordinate <- function(index, N) {
    #' log_2(N)-order Hilbert Curve Coordinate at Index
    #'
    #' Generate a coordinate of log_2(N)'th-order Hilbert Curve at a given index.
    #' @param index  The index, within the domain [0, N^2]
    #' @param N      The side length of the square formed by the Hilbert Curve.
    #'
    #' @return       The coordinate in a 2D euclidean plane in the form (x, y).
    
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

get.hilbert.map <- function(N) {
    #' log_2(N)-order Hilbert Curve Map
    #'
    #' Generate a mapping data frame (coordinates) of log_2(N)-order Hilbert Curve used
    #'   for decoding images stored in the sequence of the N-order Hilbert curve.
    #' Note: The coordinates are in R's 1-based indexing
    #' @param N  The side length of the square formed by the Hilbert Curve.
    #'
    #' @return   A N^2 x 2 data frame with row-wise 2D Hilbert-Curve coordinates.
    
    hilbert.map <- tibble(i = seq_len(N ^ 2) - 1) |>
        rowwise() |>
        mutate(coord = paste(hilbert.coordinate(i, N) + 1, collapse = ' '),
               .keep = 'unused') |>
        separate(coord, sep = ' ', into = c('x', 'y'), convert = TRUE)
    return(hilbert.map)
}

get.coordinate.pairs <- function(coords) {
    coords |>
        lag(1) |>
        bind_cols(coords, .name_repair = 'minimal') |>
        `colnames<-`(c('x0', 'y0', 'x1', 'y1')) |>
        slice(-1)
}

hilbert.curve.demo <- function(hilbert.map) {
    #' Plot a log_2(N)-order Hilbert Curve
    #'
    #' Plot a log_2(N)-order Hilbert Curve with a given mapping data frame.
    #' @param hilbert.map  A data frame returned by the helper function `get.hilbert.map`.
    #'
    #' @return             A plot.
    
    N <- sqrt(nrow(hilbert.map))
    
    hilbert.map <- hilbert.map |>
        map_at('y', ~ N - . + 1) |>
        as_tibble()
    
    hilbert.demo.plot <- hilbert.map |>
        get.coordinate.pairs() |>
        ggplot(aes(x = x0, y = y0, xend = x1, yend = y1)) +
            geom_segment(arrow = arrow(length = unit(0.25, 'cm'), type = 'closed'),
                         color = 'darkgray') +
            geom_point(data = hilbert.map, aes(x = x, y = y),
                       inherit.aes = FALSE) +
            theme_bw() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),  
                  # no axes
                  axis.ticks = element_blank(),
                  axis.text = element_blank(),
                  axis.title = element_blank())
    
    return(hilbert.demo.plot)
}


# ======= Datasets =======

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
    if ('label' %in% colnames(data.set)) {
        # restore labels
        new.set <- mutate(new.set, label = data.set$label)
    }
    # getting rid of auto-generated row names
    rownames(new.set) <- NULL
    return(new.set)
}

augment.set <- function(data.set, fn, ...) {
    #' Data Augmentation
    #'
    #' Generate an altered copy of the dataset with the given transformation function.
    #' @param data.set  A data frame to be altered.
    #' @param fn        A transformation function that takes in a matrix and outputs a matrix.
    #' @param ...       Variadic arguments passed to `fn`.
    #'
    #' @return          A data frame with transformed observations.
    
    # get columns for image pixels ONLY
    
    image.dim <- ncol(data.set |> select_if(is.numeric))
    
    transform.set(data.set, function(image.array, ...) {
        image.array |>
            as.image.matrix(ncol = sqrt(image.dim)) |>
            fn(...) |>
            as.feature.vector()
    }, ...)
}

# ======= Images =======

as.image.matrix <- function(vec, ...) {
    vec |> matrix(byrow = TRUE, ...)
}

as.feature.vector <- function(mtx) {
    mtx |> t() |> as.numeric()
}

show.image <- function(data.set, i = 1, n = 16, hilbert = FALSE, ...) {
    #' Display an image from a dataset
    #'
    #' Display the i'th image (matrix/array) from a given dataset.
    #' @param data.set  A data frame.
    #' @param i         The number (index) of the desired image.
    #' @param n         The side length hint of the image.
    #' @param hilbert   Whether or not the image is arranged by Hilbert Curve.
    
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

X.padding <- function(mtx, n = 1, x = 1, random = FALSE) {
    #' X-padding
    #'
    #' Zero-padding a given matrix with a given number or random numbers within a given range.
    #' @param mtx     A matrix to be X-padded.
    #' @param n       The number of layers to pad.
    #' @param x       A number, or a random number range for X-padding the matrix.
    #' @param random  Whether or not the matrix is X-padded with random numbers.
    #'                The parameter `x` will be interpreted as a range is `TRUE`.
    #'
    #' @return        A X-padded matrix.
    
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

pixel.binning <- function(image.array, binsize = 2) {
    #' One-dimensional X Pixel Binning
    #'
    #' Perform a 1 x N pixel binning on a given image array.
    #' @param image.array  An image array of the dimension 1 x N.
    #' @param binsize      The size of the bin by which the image array is compressed.
    #'                     The default is `2L`.
    #'
    #' @return        A pixel-binned image array (vector).
    
    scaled.array <- image.array |>
        as.image.matrix(ncol = binsize) |>
        as.data.frame() |>
        rowwise() |> mutate(avg = mean(c_across(everything()))) |>
        pull(avg)
    return(scaled.array)
}

compress.image <- function(image.array) {
    ((lag(image.array, 1) + image.array) / 2) |>
        na.omit() |>
        as.numeric()
}

rotate.image <- function(mtx, angle = 90) {
    #' Matrix Rotation
    #'
    #' Perform a clockwise rotation on a given matrix by a given angle.
    #' @param mtx    A matrix.
    #' @param angle  The rotation angle in degrees.
    #'               Note: Must be an integer multiple of 90 degrees.
    #'
    #' @return       A rotated matrix.
    
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

gaussian.noise <- function(mtx, mean = 0, sd = 1) {
    gaussian.filter <- rnorm(nrow(mtx) * ncol(mtx), mean = mean, sd = sd) |>
        matrix(nrow = nrow(mtx))
    return(mtx + gaussian.filter)
}
