#### Visualization functions ####

#' plot method for sfn_data class
#' 
#' Plot the desired data from a site object
#' 
#' @section ggplot plotting system:
#'   \code{\link{plot}} is a base R function which uses the base R plotting system
#'   to show the plot. We prefer the ggplot plotting system, which allow for
#'   storing the plots in objects and can be subjected to further modifications.
#'   This allow the package users to generate rather simple plots that can be
#'   fine tuned afterwards to the user taste. Generating a \code{\link{plot}}
#'   method for the \code{sfn_data} class returning a ggplot object is not
#'   desired (it change the way plot works and can be misleading about the plot
#'   general usage). So, instead, we offer this function, \code{sfn_plot}.
#'
#' @section Type:
#'   \code{type} argument controls what is going to be plotted. It accepts
#'   the following:
#'   \itemize{
#'     \item{"sapf": It will plot sapflow data vs. TIMESTAMP}
#'     \item{"env": It will plot environmental variables vs. TIMESTMAP}
#'     \item{"ta", "rh", "vpd", "ppfd_in", "netrad", "sw_in", "ext_rad",
#'           "ws", "precip", "swc_shallow" and "swc_deep": They will plot
#'           the corresponding variable vs. TIMESTAMP}
#'   }
#'
#' @param sfn_data sfn_data object to plot
#' 
#' @param type Character indicating which data to plot. See Type section for
#'   detailed information about the available values. Ignored if formula is
#'   provided
#' 
#' @param formula_env Right side formula indicating an environmental variable to
#'   plot vs. the sapflow values. If NULL (default) function will use "type"
#'   to guess which plot show. 
#'
#' @param solar Logical indicating if the solar timestamp must be used instead
#'   of the site timestamp
#'
#' @examples
#' library(ggplot2)
#' 
#' # data
#' data('FOO', package = 'sapfluxnetr')
#' 
#' # plotting directly
#' sfn_plot(FOO, type = 'sapf')
#' 
#' # this could be noisy, you can facet by "Tree" (for sapflow) or by
#' # "Variable" (for environmental data):
#' sfn_plot(FOO, type = 'sapf') +
#'   facet_wrap(~ Tree)
#'
#' sfn_plot(FOO, type = 'env') +
#'   facet_wrap(~ Variable, scales = 'free_y')
#' 
#' # saving and modifying:
#' env_plot <- sfn_plot(FOO, type = 'env', solar = FALSE) +
#'   facet_wrap(~ Variable, scales = 'free_y')
#' env_plot + labs(title = 'Environmental variables facet plot')
#' 
#' # formula
#' sfn_plot(FOO, formula_env = ~ vpd)
#' 
#' @return A ggplot object that can be called to see the plot
#' 
#' @import ggplot2
#' 
#' @export

sfn_plot <- function(
  sfn_data,
  type = c(
    'sapf', 'env',
    'ta', 'rh', 'vpd', 'ppfd_in', 'netrad', 'sw_in', 'ext_rad',
    'ws', 'precip', 'swc_shallow', 'swc_deep'
  ),
  formula_env = NULL,
  solar = TRUE
) {
  
  # if formula, lets do that plot
  if (rlang::is_formula(formula_env)) {
    data <- get_env(sfn_data, solar = solar) %>%
      dplyr::select(.data$TIMESTAMP, !!rlang::get_expr(formula_env)) %>%
      dplyr::inner_join(get_sapf(sfn_data, solar = solar), by = 'TIMESTAMP')
    
    units_char <- paste0(
      unique(
        get_plant_md(sfn_data)[['pl_sap_units']]
      ),
      sep = ' '
    )
    
    res_plot <- data %>%
      tidyr::gather(
        key = 'Tree', value = 'Sapflow',
        -.data$TIMESTAMP, -!!rlang::get_expr(formula_env)
      ) %>%
      ggplot(aes_(x = formula_env, y = ~Sapflow, colour = ~Tree)) +
      geom_point(alpha = 0.2) +
      labs(y = paste0('Sapflow [', units_char, ']'),
           subtitle = paste0('Sap flow vs. ', rlang::get_expr(formula_env)),
           title = get_si_code(sfn_data))
  } else {
    
    # We need to go type by type checking and plotting if type matchs
    type <- match.arg(type)
    
    # sapf
    if (type == 'sapf') {
      data <- get_sapf(sfn_data, solar = solar)
      units_char <- paste0(
        unique(
          get_plant_md(sfn_data)[['pl_sap_units']]
        ),
        sep = ' '
      )
      
      # actual plot
      res_plot <- data %>%
        tidyr::gather(key = 'Tree', value = 'Sapflow', -.data$TIMESTAMP) %>%
        ggplot(aes_(x = ~TIMESTAMP, y = ~Sapflow, colour = ~Tree)) +
        geom_point(alpha = 0.2) +
        labs(y = paste0('Sapflow [', units_char, ']')) +
        scale_x_datetime()
    }
    
    # env
    if (type == 'env') {
      data <- get_env(sfn_data, solar = solar)
      
      # actual plot
      res_plot <- data %>%
        tidyr::gather(key = 'Variable', value = 'Value', -.data$TIMESTAMP) %>%
        ggplot(aes_(x = ~TIMESTAMP, y = ~Value, colour = ~Variable)) +
        geom_point(alpha = 0.4) +
        scale_x_datetime()
    }
    
    # ta
    if (type == 'ta') {
      data <- get_env(sfn_data, solar)
      
      # we need to check if environmental variable exists
      if (is.null(data[['ta']])) {
        stop('Site has not ta data')
      }
      
      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = .data$TIMESTAMP, y = .data$ta)) +
        geom_point(alpha = 0.4, colour = '#C0392B') +
        labs(y = 'Air Temperature [C]') +
        scale_x_datetime()
    }
    
    # rh
    if (type == 'rh') {
      data <- get_env(sfn_data, solar)
      
      if (is.null(data[['rh']])) {
        stop('Site has not rh data')
      }
      
      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = .data$TIMESTAMP, y = .data$rh)) +
        geom_point(alpha = 0.4, colour = '#6BB9F0') +
        labs(y = 'Relative Humidity [%]') +
        scale_x_datetime()
    }
    
    # vpd
    if (type == 'vpd') {
      data <- get_env(sfn_data, solar)
      
      if (is.null(data[['vpd']])) {
        stop('Site has not vpd data')
      }
      
      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = .data$TIMESTAMP, y = .data$vpd)) +
        geom_point(alpha = 0.4, colour = '#6BB9F0') +
        labs(y = 'VPD [kPa]') +
        scale_x_datetime()
    }
    
    # ppfd_in
    if (type == 'ppfd_in') {
      data <- get_env(sfn_data, solar)
      
      if (is.null(data[['ppfd_in']])) {
        stop('Site has not ppfd_in data')
      }
      
      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = .data$TIMESTAMP, y = .data$ppfd_in)) +
        geom_point(alpha = 0.4, colour = '#D35400') +
        labs(y = 'PPFD [?]') +
        scale_x_datetime()
    }
    
    # sw_in
    if (type == 'sw_in') {
      data <- get_env(sfn_data, solar)
      
      if (is.null(data[['sw_in']])) {
        stop('Site has not sw_in data')
      }
      
      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = .data$TIMESTAMP, y = .data$sw_in)) +
        geom_point(alpha = 0.4, colour = '#E87E04') +
        labs(y = 'sw [?]') +
        scale_x_datetime()
    }
    
    # netrad
    if (type == 'netrad') {
      data <- get_env(sfn_data, solar)
      
      if (is.null(data[['netrad']])) {
        stop('Site has not netrad data')
      }
      
      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = .data$TIMESTAMP, y = .data$netrad)) +
        geom_point(alpha = 0.4, colour = '#EB9532') +
        labs(y = 'Net Radiation [?]') +
        scale_x_datetime()
    }
    
    # ext_rad
    if (type == 'ext_rad') {
      data <- get_env(sfn_data, solar)
      
      if (is.null(data[['ext_rad']])) {
        stop('Site has not ext_rad data')
      }
      
      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = .data$TIMESTAMP, y = .data$ext_rad)) +
        geom_point(alpha = 0.4, colour = '#F89406') +
        labs(y = 'Extraterrestrial Radiation [?]') +
        scale_x_datetime()
    }
    
    # ws
    if (type == 'ws') {
      data <- get_env(sfn_data, solar)
      
      if (is.null(data[['ws']])) {
        stop('Site has not ws data')
      }
      
      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = .data$TIMESTAMP, y = .data$ws)) +
        geom_col(alpha = 0.4, fill = '#674172') +
        labs(y = 'Wind Speed [m/s]') +
        scale_x_datetime()
    }
    
    # precip
    if (type == 'precip') {
      data <- get_env(sfn_data, solar)
      
      if (is.null(data[['precip']])) {
        stop('Site has not precip data')
      }
      
      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = .data$TIMESTAMP, y = .data$precip)) +
        geom_col(alpha = 0.4, fill = '#67809F') +
        labs(y = 'Precipitation [?]') +
        scale_x_datetime()
    }
    
    # swc_shallow
    if (type == 'swc_shallow') {
      data <- get_env(sfn_data, solar)
      
      if (is.null(data[['swc_shallow']])) {
        stop('Site has not swc_shallow data')
      }
      
      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = .data$TIMESTAMP, y = .data$swc_shallow)) +
        geom_point(alpha = 0.4, colour = '#26A65B') +
        labs(y = 'SWC Shallow [cm3/cm3]') +
        scale_x_datetime()
    }
    
    # swc_deep
    if (type == 'swc_deep') {
      data <- get_env(sfn_data, solar)
      
      if (is.null(data[['swc_deep']])) {
        stop('Site has not swc_deep data')
      }
      
      # actual plot
      res_plot <- data %>%
        ggplot(aes(x = .data$TIMESTAMP, y = .data$swc_deep)) +
        geom_point(alpha = 0.4, colour = '#019875') +
        labs(y = 'SWC Deep [cm3/cm3]') +
        scale_x_datetime()
    }
    
  }
  
  return(res_plot)
  
}