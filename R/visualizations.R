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
#' @param sfn_data sfn_data object to plot
#' 
#' @param type Character indicating which data to plot. See Type section for
#'   detailed information about the available values
#'
#' @param solar Logical indicating if the solar timestamp must be used instead
#'   of the site timestamp
#'
#' @examples
#' # data
#' data('FOO', package = 'sapfluxnetr')
#' 
#' # plotting directly
#' sfn_plot(FOO, type = 'sapf', solar = FALSE)
#' 
#' # saving and modifying:
#' env_plot <- sfn_plot(FOO, type = 'env', solar = FALSE)
#' env_plot + title('Environmental variables facet plot')
#' 
#' @return A ggplot object that can be called to see the plot
#' 
#' @export

sfn_plot <- function(
  sfn_data,
  type = c(
    'sapf', 'env',
    'ta', 'rh', 'vpd', 'ppfd_in', 'netrad', 'sw_in', 'ext_rad',
    'ws', 'precip', 'swc_shallow', 'swc_deep'
  ),
  solar = TRUE
) {
  
  type <- match.arg(type)
  
  # We need to go type by type checking and plotting if type matchs
  
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
      tidyr::gather(Tree, Sapflow, -TIMESTAMP) %>%
      ggplot(aes(x = TIMESTAMP, y = Sapflow, colour = Tree)) +
      geom_point(alpha = 0.2) +
      labs(y = paste0('Sapflow [', units_char, ']')) +
      scale_x_datetime() +
      facet_wrap('Tree', ncol = 3, scale = 'fixed')
  }
  
  # env
  if (type == 'env') {
    data <- get_env(sfn_data, solar = solar)
    
    # actual plot
    res_plot <- data %>%
      tidyr::gather(Variable, Value, -TIMESTAMP) %>%
      ggplot(aes(x = TIMESTAMP, y = Value, colour = Variable)) +
      geom_point(alpha = 0.4) +
      scale_x_datetime() +
      facet_wrap('Variable', ncol = 3, scale = 'free_y')
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
      ggplot(aes(x = TIMESTAMP, y = ta)) +
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
      ggplot(aes(x = TIMESTAMP, y = rh)) +
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
      ggplot(aes(x = TIMESTAMP, y = vpd)) +
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
      ggplot(aes(x = TIMESTAMP, y = ppfd_in)) +
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
      ggplot(aes(x = TIMESTAMP, y = sw_in)) +
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
      ggplot(aes(x = TIMESTAMP, y = netrad)) +
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
      ggplot(aes(x = TIMESTAMP, y = ext_rad)) +
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
      ggplot(aes(x = TIMESTAMP, y = ws)) +
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
      ggplot(aes(x = TIMESTAMP, y = precip)) +
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
      ggplot(aes(x = TIMESTAMP, y = swc_shallow)) +
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
      ggplot(aes(x = TIMESTAMP, y = swc_deep)) +
      geom_point(alpha = 0.4, colour = '#019875') +
      labs(y = 'SWC Deep [cm3/cm3]') +
      scale_x_datetime()
  }
  
  return(res_plot)
  
}