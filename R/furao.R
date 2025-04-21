#' Calculate the returns
#' 
#' @param data Data frame in long format.
#' @param tickers Name of the column which holds tickers names. (default = "ticker")
#' @param dates Name of the column which holds dates. (default = "ref_date")
#' @param prices Name of the column which holds stock prices. (default = "price_adjusted")
#' @param returns Name of the column in which calculated returns will be stored. (default = "return")
#' 
.calculate_returns <- function(
    data,
    tickers = "ticker",
    dates = "ref_date",
    prices = "price_adjusted",
    returns = "return"
) {
    data %>%
        dplyr::group_by({{tickers}}) %>%
        dplyr::arrange({{dates}}) %>%
        dplyr::mutate({{returns}} := ((.[[prices]] / dplyr::lag(.[[prices]]))) - 1) %>%
        # FIXME: the mutate() call doesn't work inside the group_by
        # so the first return of ticker n is calculated with last price of ticker n-1, instead of being N/A
        # this is currently hackishly fixed inside get_rolling_betas()
        dplyr::ungroup() %>%
        dplyr::select(-`\"ticker\"`)
}

#' Delete rows so the remaining observations follow a determined time interval
#'
#' @param data Data frame in long format.
#' @param interval Frequency of data: "daily", "weekly", "monthly", "yearly". (default = "month")
#' @param dates Name of the column which holds dates. (default = "ref_date")
#' @param slice_by Vector of columns to group by for just this operation.
#'
.set_frequency <- function(
    data,
    interval = "month",
    dates = "ref_date",
    slice_by = c()
) {
    data %>%
        dplyr::arrange({{dates}}) %>%
        dplyr::mutate(interval_unit = lubridate::ceiling_date(.data[[dates]], unit = interval)) %>%
        dplyr::slice_head(n = 1, by = dplyr::any_of(c("interval_unit", slice_by))) %>%
        dplyr::select(-interval_unit)
}

# Beta definitions

#' Calculate Sharpe's Beta for a given portfolio relative to a market
#'
#' @param data Data frame in wide format.
#' @param portfolio_returns Name of the column which holds the portfolio returns.
#' @param market_returns Name of the column which holds the market returns.
#'
.sharpe_beta <- function(
    data,
    portfolio_returns,
    market_returns
) {
    formula <- stringr::str_c(portfolio_returns, "~", market_returns, sep = " ") %>% as.formula()
    data %>%
        lm(formula, data = .) %>%
        coef %>%
        .[[market_returns]]
}

#' Calculate Rolling Betas
#'
#' @param data Data frame in long format.
#' @param .dates_col Name of the column which holds the dates.
#' @param .prices_col Name of the column which holds the tickers prices.
#' @param .tickers_col Name of the column which holds the tickers names.
#' @param .portfolio_tickers String or vector specifying the ticker(s) the Beta is being calculated for.
#' @param .weights Vector of integers specifying the weights for each ticker in the portfolio. It's internally normalized so the weights sums to 1. Defaults to 1.
#' @param .market_ticker Name of the ticker which the portfolio is being regressed against.
#' @param .beta_fun Function definition of the Beta. (default = Sharpe's Beta)
#' @param .observations Number of observations used to calculate each Beta. (default = 36L)
#' @param .interval Desired frequency of returns: "daily", "weekly", "monthly", "yearly". (default = "month")
#' @param .date_lb Begin point for which the data.frame is truncated for. (inclusive. If left empty will use the entire dataset)
#' @param .date_ub End point for which the data.frame is truncated for. (inclusive. If left empty will use the entire dataset)
#' @param .beta_adj Adjust method for Beta. (not implemented yet, will raise an error if filled.)
#' @param .diagnostics Returns plots and test statistics. Set to FALSE to turn off. (default = "all")
#'
#' @return A list
#' @export
#' 
get_rolling_betas <- function(
    data,
    .dates_col = "ref_date",
    .prices_col = "price_adjusted",
    .tickers_col = "ticker",
    .portfolio_tickers,
    .weights = NULL,
    .market_ticker,
    .beta_fun = .sharpe_beta,
    .observations = 36L,
    .interval = "month",
    .date_lb = NULL,
    .date_ub = NULL,
    .beta_adj = NULL,
    .diagnostics = "all"
) {
    # dealing with necessary adjustments for default parameters
    if(!is.null(.beta_adj)) {
        stop("Adjusting via '.beta_adj' argument is not implemented yet.")
    }
    
    if(is.null(.weights)) {
        tickers_qty <- length(.portfolio_tickers)
        weights <- rep(1L, tickers_qty) / tickers_qty
    } else {
        weights <- .weights / sum(.weights)
    }
    
    if(is.null(.date_lb)) {
        .date_lb <- min(data[[.dates_col]])
    } else if(is.character(.date_lb)) {
        .date_lb <- as.Date(.date_lb)
    }
    
    if(is.null(.date_ub)) {
        .date_ub <- max(data[[.dates_col]])
    } else if(is.character(.date_ub)) {
        .date_ub <- as.Date(.date_ub)
    }
    
    if(.diagnostics == "all") {
        .diagnostics <- c("sw", "ts_plot", "dens_plot", "adf", "acf", "pacf")
    } else {
        .diagnostics <- c(.diagnostics)
    }
    
    
    # subsets and computes the used data
    filtered_data <- data %>%
        dplyr::filter(
            .[[.tickers_col]] %in% c(.portfolio_tickers, .market_ticker)
            & .[[.dates_col]] >= .date_lb
            & .[[.dates_col]] <= .date_ub
            )
    
    weights_tbl <- tibble::tibble({{.tickers_col}} := c(.portfolio_tickers, .market_ticker), .weight = c(weights, 1))
    
    returns <- filtered_data %>%
        .set_frequency(interval = .interval, slice_by = .tickers_col) %>%
        .calculate_returns(tickers = {{.tickers_col}}, dates = {{.dates_col}}, prices = {{.prices_col}}, returns = ".return")

    portfolio_returns <- returns %>%
        dplyr::left_join(weights_tbl, by = "ticker") %>%
        dplyr::mutate(.return = .return * .weight) %>%
        tidyr::pivot_wider(id_cols = .dates_col, names_from = dplyr::all_of(.tickers_col), values_from = ".return") %>%
        dplyr::mutate(.return = rowSums(dplyr::across(dplyr::all_of(.portfolio_tickers)))) %>%
        dplyr::mutate(.return = replace(.return, dplyr::row_number() == 1, NA_real_)) %>%
        dplyr::select(- .portfolio_tickers) %>%
        tidyr::drop_na()
    
    
    # compute our betas
    betas <- portfolio_returns %>%
        slider::slide_vec(~.x, .f = ~ .beta_fun(., portfolio_returns = ".return", market_returns = glue::backtick(.market_ticker)), .before = .observations - 1L, .complete = TRUE) %>%
        tibble::tibble(date = portfolio_returns[[.dates_col]], beta = .) %>%
        tidyr::drop_na()
    
    
    # diagnostics
    if("sw" %in% .diagnostics) {
        shapiro <- stats::shapiro.test(betas$beta)
    }
    
    if("adf" %in% .diagnostics) {
        adf <- tseries::adf.test(betas$beta)
    }
    
    if("ts_plot" %in% .diagnostics) {
        ts_plot <- ggplot2::ggplot(betas, aes(x = date, y = beta)) +
            ggplot2::geom_line(color = "lightblue", linewidth = 0.5) +
            ggplot2::geom_point(color = "blue", size = 1) +
            ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "orange", linewidth = 0.5) +
            ggplot2::labs(
                title = "Time Series Plot of Betas",
                x = "Date",
                y = "Beta"
            ) +
            ggplot2::theme_minimal()
    }
    
    if("dens_plot" %in% .diagnostics) {
        dens_plot <- ggplot2::ggplot(betas, aes(x = beta)) +
            ggplot2::geom_density(fill = "lightblue", color = "blue", alpha = 0.5) +
            ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "orange", linewidth = 0.5) + 
            ggplot2::labs(
                title = "Density Plot of Betas",
                x = "Beta",
                y = "Density"
            ) +
            ggplot2::theme_minimal()
    }
    
    if("acf" %in% .diagnostics) {
        acf_val <- acf(betas$beta, plot = FALSE)
        acf_data <- tibble::tibble(
            lag = c(acf_val$lag),
            acf = c(acf_val$acf)
        )
        acf_plot <- ggplot2::ggplot(acf_data, aes(x = lag, y = acf)) +
            ggplot2::geom_bar(stat = "identity", fill = "black") +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
            ggplot2::labs(
                title = "Autocorrelation Function (ACF)",
                x = "Lag",
                y = "Autocorrelation"
            ) +
            ggplot2::theme_minimal()
    }
    
    if("pacf" %in% .diagnostics) {
        pacf_val <- pacf(betas$beta, plot = FALSE)
        pacf_data <- tibble::tibble(
            lag = c(pacf_val$lag),
            pacf = c(pacf_val$acf)
        )
        pacf_plot <- ggplot2::ggplot(pacf_data, aes(x = lag, y = pacf)) +
            ggplot2::geom_bar(stat = "identity", fill = "black") +
            ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
            ggplot2::labs(
                title = "Partial Autocorrelation Function (PACF)",
                x = "Lag",
                y = "Partial Autocorrelation"
            ) +
            ggplot2::theme_minimal()
    }
    
    
    # prepares our returned object
    retval <- list()
    retval$call <- match.call()
    retval$market <- .market_ticker
    retval$portfolio <- weights_tbl %>% dplyr::filter(.[[.tickers_col]] %in% .portfolio_tickers)
    retval$returns <- returns %>%
        tidyr::pivot_wider(id_cols = .dates_col, names_from = dplyr::all_of(.tickers_col), values_from = ".return") %>%
        dplyr::left_join(portfolio_returns[c(.dates_col, ".return")], by = .dates_col) %>%
        tidyr::drop_na()
    retval$betas <- betas
    if("sw" %in% .diagnostics) {
        retval$shapiro <- shapiro
    }
    if("adf" %in% .diagnostics) {
        retval$adf <- adf
    }
    if("ts_plot" %in% .diagnostics) {
        retval$ts_plot <- ts_plot
    }
    if("dens_plot" %in% .diagnostics) {
        retval$dens_plot <- dens_plot
    }
    if("acf" %in% .diagnostics) {
        retval$acf <- acf_data
        retval$acf_plot <- acf_plot
    }
    if("pacf" %in% .diagnostics) {
        retval$pacf <- pacf_data
        retval$pacf_plot <- pacf_plot
    }
    
    retval
}

#' Prints relevant `get_rolling_betas()` results to an Excel Workbook.
#'
#' @param furao_obj A list as returned by `get_rolling_betas()`
#'
#' @export
#'
excel_out <- function(
    furao_obj,
    .table_filename
) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Summary")
    openxlsx::addWorksheet(wb, "Returns")
    openxlsx::addWorksheet(wb, "Betas")
    openxlsx::addWorksheet(wb, "Diagnostics")
    
    
    call_str <- furao_obj$call %>%
        deparse %>%
        paste(collapse = "")
    openxlsx::writeData(wb, sheet = "Summary", x = c("Call"), xy = c(1, 1))
    openxlsx::writeData(wb, sheet = "Summary", x = call_str, xy = c(1, 2))
    
    openxlsx::writeData(wb, sheet = "Summary", x = c("Market Proxy"), xy = c(1, 4))
    openxlsx::writeData(wb, sheet = "Summary", x = furao_obj$market, xy = c(1, 5))
    openxlsx::writeData(wb, sheet = "Summary", x = c("Portfolio"), xy = c(1, 7))
    openxlsx::writeData(wb, sheet = "Summary", x = furao_obj$portfolio, xy = c(1, 8))
    
    openxlsx::writeData(wb, sheet = "Returns", x = furao_obj$returns)
    openxlsx::writeData(wb, sheet = "Betas", x = furao_obj$betas)
    
    
    header <- c("Test", "Statistic", "P-Value")
    sw <- furao_obj$shapiro
    adf <- furao_obj$adf
    res <- tibble::tibble(
        "Test" = c(sw$method, adf$method),
        "Statistic" = c(sw$statistic, adf$statistic),
        "P-Value" = c(sw$p.value, adf$p.value)
        )
    openxlsx::writeData(wb, sheet = "Diagnostics", x = res)
    
    openxlsx::writeData(wb, sheet = "Diagnostics", x = c("ACF"), xy = c(6, 1))
    openxlsx::writeData(wb, sheet = "Diagnostics", x = furao_obj$acf, xy = c(6, 2))
    
    openxlsx::writeData(wb, sheet = "Diagnostics", x = c("PACF"), xy = c(9, 1))
    openxlsx::writeData(wb, sheet = "Diagnostics", x = furao_obj$pacf, xy = c(9, 2))
    
    t1 <- tempfile(fileext = ".png")
    ggplot2::ggsave(filename = t1, plot = furao_obj$ts_plot, bg = "white")
    openxlsx::insertImage(wb, sheet = "Diagnostics", file = t1, startRow = 20, startCol = 1)
    t2 <- tempfile(fileext = ".png")
    ggplot2::ggsave(filename = t2, plot = furao_obj$dens_plot, bg = "white")
    openxlsx::insertImage(wb, sheet = "Diagnostics", file = t2, startRow = 35, startCol = 1)
    t3 <- tempfile(fileext = ".png")
    ggplot2::ggsave(filename = t3, plot = furao_obj$acf_plot, bg = "white")
    openxlsx::insertImage(wb, sheet = "Diagnostics", file = t3, startRow = 20, startCol = 10)
    t4 <- tempfile(fileext = ".png")
    ggplot2::ggsave(filename = t4, plot = furao_obj$pacf_plot, bg = "white")
    openxlsx::insertImage(wb, sheet = "Diagnostics", file = t4, startRow = 35, startCol = 10)
    
    openxlsx::saveWorkbook(wb, .table_filename)
    unlink(t4)
    unlink(t3)
    unlink(t2)
    unlink(t1)
}