# Theme -------------------------------------------------------------------

#' ggplot2 theme based on the design of <digital-vereint.de>
#' @inheritParams ggplot2::theme_minimal
#' @param axis_family axis font family
#' @param grid panel grid to be shown ("none" or a combination of "X", "Y", "x", and "y")
#'
theme_dv <- function(base_size = 16,
                     base_family = "Fira Sans",
                     axis_family = "Fira Mono",
                     base_line_size = 0.75,
                     base_rect_size = 0.75,
                     grid = "XY") {
    if (length(grid) != 1L || !grepl("^(none|[XYxy]+)$", grid)) {
        stop('`grid` must be a string: "none" or any combination of "X", "Y", "x", and "y"')
    }

    font_fallback(base_family)
    font_fallback(axis_family)

    dark_grey <- "#242424"
    mid_grey <- "#616161"
    # light_grey <- "#f2f2f2"
    blue <- "#004fa3"
    green <- "#598000"
    light_green <- "#edf6d9"

    t <- ggplot2::theme_minimal(
        base_size = base_size,
        base_line_size = base_line_size,
        base_rect_size = base_rect_size
    ) +
        ggplot2::theme(
            text = ggplot2::element_text(
                family = base_family,
                colour = dark_grey
            ),
            line = ggplot2::element_line(colour = light_green),
            plot.title = ggplot2::element_text(
                face = "bold",
                colour = blue,
                size = ggplot2::rel(1.25),
                margin = ggplot2::margin(b = base_size * 2 / 3),
            ),
            plot.subtitle = ggplot2::element_text(
                face = "bold",
                colour = green,
                lineheight = 1.2,
                margin = ggplot2::margin(b = base_size)
            ),
            plot.caption = ggplot2::element_text(
                family = axis_family,
                hjust = 0,
                margin = ggplot2::margin(t = base_size * 2 / 3)
            ),
            axis.title = ggplot2::element_text(
                face = "bold",
                colour = mid_grey
            ),
            axis.title.x = ggplot2::element_text(
                hjust = 1,
                margin = ggplot2::margin(t = base_size * 2 / 3)
            ),
            axis.title.x.top = ggplot2::element_text(
                margin = ggplot2::margin(b = base_size * 2 / 3)
            ),
            axis.title.y = ggplot2::element_text(
                hjust = 1,
                margin = ggplot2::margin(r = base_size * 2 / 3)
            ),
            axis.title.y.right = ggplot2::element_text(
                margin = ggplot2::margin(l = base_size * 2 / 3)
            ),
            axis.text = ggplot2::element_text(family = axis_family),
            axis.text.x = ggplot2::element_text(
                margin = ggplot2::margin(t = base_size / 3)
            ),
            axis.text.y = ggplot2::element_text(
                margin = ggplot2::margin(r = base_size / 2)
            ),
            legend.key.spacing = grid::unit(base_size * 2 / 3, "pt"),
            legend.key.size = grid::unit(base_size * 1.05, "pt"),
            legend.title = ggplot2::element_text(
                face = "bold",
                colour = mid_grey
            ),
            legend.position = "top",
            legend.justification = "left",
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(size = ggplot2::rel(1.05)),
            panel.grid = ggplot2::element_line(colour = light_green),
            panel.grid.minor = ggplot2::element_line(linewidth = ggplot2::rel(1)),
            plot.margin = ggplot2::margin(
                base_rect_size * 40,
                base_rect_size * 40,
                base_rect_size * 40,
                base_rect_size * 40
            ),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            plot.tag = element_text(
                size = ggplot2::rel(1),
                margin = ggplot2::margin(r = base_size / 2, b = base_size / 2)
            )
        )

    if (!grepl("X", grid)) {
        t <- t + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    }
    if (!grepl("Y", grid)) {
        t <- t + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    }
    if (!grepl("x", grid)) {
        t <- t + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
    }
    if (!grepl("y", grid)) {
        t <- t + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    }

    t
}

# Scales ------------------------------------------------------------------

## Colours ----------------------------------------------------------------

#' Colour scales based on the design of <digital-vereint.de>
#'
#' @param reverse Should the order of colours be reversed?
#' @inheritParams `ggplot2::continuous_scale`
#' @inheritDotParams `ggplot2::continuous_scale`
#'
scale_colour_dv_c <- function(reverse = FALSE,
                              guide = "colourbar",
                              ...) {
    ggplot2::continuous_scale(
        aesthetics = "colour",
        palette = scales::gradient_n_pal(dv_palette("sequential", reverse)),
        guide = guide,
        ...
    )
}

scale_fill_dv_c <- function(reverse = FALSE,
                            guide = "colourbar",
                            ...) {
    ggplot2::continuous_scale(
        aesthetics = "fill",
        palette = scales::gradient_n_pal(dv_palette("sequential", reverse)),
        guide = guide,
        ...
    )
}

scale_colour_dv_d <- function(...) {
    ggplot2::discrete_scale(
        aesthetics = "colour",
        palette = scales::manual_pal(dv_palette("qualitative")),
        ...
    )
}

scale_fill_dv_d <- function(...) {
    ggplot2::discrete_scale(
        aesthetics = "fill",
        palette = scales::manual_pal(dv_palette("qualitative")),
        ...
    )
}

dv_palette <- function(option = c("sequential", "qualitative"),
                       reverse = FALSE) {
    option <- match.arg(option)

    cols <- switch(option,
        # Darkest blue and lightest green from DV website, but blue darkened and
        # and green lightened by 10%, then interpolated in HSV space
        sequential = c(
            "#003167",
            "#055975",
            "#0A827E",
            "#119068",
            "#199E4E",
            "#22AB33",
            "#42B92C",
            "#76C637",
            "#ABD443"
        ),
        # Text blue and green from DV website, then each at 50% opacity on white
        qualitative = c("#004fa3", "#598000", "#7fa7d1", "#adbf7f")
    )

    if (reverse) rev(cols) else cols
}

## Others -----------------------------------------------------------------

#' Label numbers in decimal format without trailing zeros
#' For use as `labels` in `ggplot2::scales_*()` functions
#' @source https://stackoverflow.com/a/58896161
#' @inheritParams scales::label_number
#' @inheritDotParams scales::label_number
#'
label_number_trimmed <- function(decimal.mark = ".", ...) {
    # Force evaluation of arguments
    list(decimal.mark, ...)

    function(x) {
        x <- scales::label_number(...)(x)

        stringi::stri_replace_first_regex(
            x,
            paste0(
                r"{^([\d,]+)$|^([\d,]+)\}", decimal.mark, r"{0*$|^([\d,]+\}",
                decimal.mark, r"{[0-9]*?)0*$}"
            ),
            "$1$2$3"
        )
    }
}

# Utils -------------------------------------------------------------------

font_fallback <- function(family, fallback = "") {
    if (!font_is_installed(family)) {
        fallback <- font_match(fallback)

        message(
            "Font ", family, " is not installed.\n",
            "Falling back to default font (", fallback, ").\n"
        )

        return(fallback)
    }

    family
}

font_is_installed <- function(family) font_match(family) == family

font_match <- function(x) systemfonts::font_info(x)[["family"]][[1]]
