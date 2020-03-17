###########################################################################################
# This script makes plots showing soil volumetric water content (VWC %) overlaid with
# precipitation events

###########################################################################################

# Load libraries ----

library(ggplot2)
library(dplyr)
library(lubridate)
library(cowplot)

###########################################################################################

# Custom plotting functions ----

theme_sigmaplot <-
  function(ticklen = -0.25) {
    # This function adds Sigma-plot like theme elements to a ggplot object.
    # Use as an additional arg, eg:
    # ggplot() + theme_sigmaplot()
    # 
    # ticklen: how long ticks (interior) will be
    
    sigmaplot <-
      theme(
        panel.background = element_blank(), # Clear background
        panel.border = element_rect(size = 1, fill = NA), # Full border around plot
        legend.key = element_rect(fill = NA), # Each legend item (e.g., lines) doesn't 
        # have a background
        axis.ticks.length.y = unit(ticklen, "cm"), # Tick size
        axis.ticks.length.y.right = unit(ticklen, "cm"), # Tick size
        axis.ticks.length.x = unit(ticklen, "cm"), # Tick size
        # X tick labels are given some padding
        axis.text.x = element_text(
          color = "black",
          margin = margin(
            t = 13,
            r = 0,
            b = 0,
            l = 0,
            unit = "pt"
          )
        ),
        # Y label (right side) is given some padding
        axis.title.y.right = element_text(
          color = "black",
          margin = margin(
            t = 0,
            r = 0,
            b = 0,
            l = 7,
            unit = "pt"
          )
        ),
        # Y label (left side) is given some padding
        axis.title.y.left = element_text(
          color = "black",
          margin = margin(
            t = 0,
            r = 5,
            b = 0,
            l = 0,
            unit = "pt"
          )
        ),
        # Y tick labels (left) are given some padding
        axis.text.y.left = element_text(
          hjust = 1,
          color = "black",
          margin = margin(
            t = 0,
            r = 10,
            b = 0,
            l = 0,
            unit = "pt"
          )
        ),
        # Y tick labels (right) are given some padding
        axis.text.y.right = element_text(
          hjust = 0,
          color = "black",
          margin = margin(
            t = 0,
            r = 0,
            b = 0,
            l = 10,
            unit = "pt"
          )
        )
      )
    return(sigmaplot)
  }


scale_y_custom_ticks <-
  function(scale_factor,
           HYS = F) {
    # This function adds a custom y scale element to a ggplot object.
    # Use as an additional arg, eg:
    # ggplot() + scale_y_custom_ticks()
    # 
    # scale_factor: Factor by which second y axis will differ (eg., different scales)
    # HYS: boolean, whether the site is HYS (HYS will have larger axis limits and more ticks)
    
    if (!(HYS)) {
      # If site is CHY
      obj <-
        scale_y_continuous(
          name = "VWC %", # Title of left axis
          limits = c(0, 0.3), # Limits of both axes (based off left side)
          breaks = c(0,
                     0.1,
                     0.2,
                     0.3), # Left breaks
          expand = c(0, 0), # Removes padding around axes
          labels = c(0,
                     0.1,
                     0.2,
                     0.3), # Left break labels
          sec.axis = sec_axis(
            ~ . * scale_factor, # Apply scaling factor to second axis
            name = "Ambient precipitation (mm)", # Title of right axis
            breaks = c(0,
                       30,
                       60,
                       90), # Right breaks
            labels = c(0,
                       30,
                       60,
                       90), # Right break labels
          )
        )
    } else {
      # If site is HYS
      obj <-
        scale_y_continuous(
          name = "VWC %", # Title of left axis
          limits = c(0, 0.45), # Limits of both axes (based off left side)
          breaks = c(0,
                     0.1,
                     0.2,
                     0.3,
                     0.4), # Left breaks
          expand = c(0, 0), # Removes padding around axes
          labels = c(0,
                     0.1,
                     0.2,
                     0.3,
                     0.4), # Left break labels
          sec.axis = sec_axis(
            ~ . * scale_factor, # Apply scaling factor to second axis
            name = "Ambient precipitation (mm)", # Title of right axis
            breaks = c(0,
                       30,
                       60,
                       90,
                       120), # Right breaks
            labels = c(0,
                       30,
                       60,
                       90,
                       120), # Right break labels
          )
        )
    }
    
    return(obj)
  }


scale_x_custom_ticks <-
  function() {
    # This function adds a custom x scale element to a ggplot object.
    # Use as an additional arg, eg:
    # ggplot() + scale_x_custom_ticks()

    obj <-
      scale_x_date(
        limits = as.Date(c('2017-04-01', '2017-09-15')), # Axis limits as dates
        expand = c(0, 0), # Remove padding around axes
        date_breaks = "month", # Label each month
        date_labels = "%b", # Use the Julian month
        name = "" # No title needed on the x axis
      )
    
    return(obj)
  }


## Provide some config arguments ----

config <-
  function() {
    return(
      list(
        year_to_plot = 2017, ## 2014 - 2017, eg
        bar_color = "#162947",
        drt_color = "#de9f40",
        con_color = "#3e946c",
        scale_factor = 300,
        infile_ppt = infile_ppt
      )
    )
  }

## Clean data ----

prep_data <-
  function(HYS,
           chr_only,
           vwc45) {
    # HYS: boolean, whether the site is HYS (HYS will have larger axis limits and more ticks)
    
    # Gather data file names
    if (!(HYS)) {
      infile_vwc <- infile_vwc_chy
      sheet_name <-  "CHY Daily PPT 2017"
    } else {
      infile_vwc <- infile_vwc_hys
      sheet_name <-  "HYS Daily PPT 2017"
    }
    infile_ppt <- config()[[6]]
    
    # Read VWC data
    vwc <-
      read.csv(infile_vwc, sep = ",", header = T)
    
    vwc$TIMESTAMP <-
      as.Date(vwc$TIMESTAMP, format = "%m/%d/%Y")
    
    if (chr_only) {
      vwc <-
        vwc %>%
        dplyr::filter(Treatment != "INT")
      vwc$Treatment <- recode(vwc$Treatment, "CHR" = "DRT")
    } else {
      vwc$Treatment <-
        recode(vwc$Treatment, "CHR" = "DRT", "INT" = "DRT")
    }
    
    year_to_plot <- config()[[1]]
    
    if (vwc45) {
      vwc_plot <-
        vwc %>%
        dplyr::filter(lubridate::year(TIMESTAMP) == year_to_plot) %>%
        dplyr::group_by(TIMESTAMP, Treatment) %>%
        dplyr::summarise(VWC = mean(VWC45, na.rm = T)) %>%
        dplyr::filter(lubridate::month(TIMESTAMP) >= 4) %>%
        dplyr::filter(lubridate::month(TIMESTAMP) <= 9)
    } else {
      vwc_plot <-
        vwc %>%
        dplyr::filter(lubridate::year(TIMESTAMP) == year_to_plot) %>%
        dplyr::group_by(TIMESTAMP, Treatment) %>%
        dplyr::summarise(VWC = mean(VWC90, na.rm = T)) %>%
        dplyr::filter(lubridate::date(TIMESTAMP) > "2017-04-01") %>%
        dplyr::filter(lubridate::date(TIMESTAMP) < "2017-09-15")
    }
    
    ppt <-
      readxl::read_excel(infile_ppt, sheet = sheet_name)
    
    ppt$date <-
      as.Date(ppt$date, format = "%m/%d/%Y")
    
    ppt <-
      ppt %>%
      dplyr::filter(date > "2017-04-01") %>%
      dplyr::filter(date < "2017-09-15")
    
    return(list(vwc_plot, ppt))
  }

## Main plot function ----

make_vwc_ppt_plot <- function(# Data items
  HYS = F,
  vwc45 = vwc45,
  chr_only = chr_only,
  # Plot items
  legend_title,
  leg_position = c(0.75, 0.8),
  save_file = save_file) {
  # Establish some constants, per the config function
  year_to_plot <- config()[[1]]
  bar_color <- config()[[2]]
  drt_color <- config()[[3]]
  con_color <- config()[[4]]
  scale_factor <- config()[[5]]
  
  tmp <-
    prep_data(HYS,
              chr_only,
              vwc45)
  
  vwc_plot <- tmp[[1]]
  ppt <- tmp[[2]]
  
  gg <-
    ggplot() +
    geom_col(
      data = ppt,
      aes(x = date, y = `ambient precip (mm)` / scale_factor),
      fill = bar_color
    ) +
    geom_line(data = vwc_plot,
              aes(
                x = TIMESTAMP,
                y = VWC,
                group = Treatment,
                color = Treatment
              )) +
    scale_y_custom_ticks(scale_factor,
                         HYS = HYS) +
    scale_x_custom_ticks() +
    scale_color_manual(
      values = c(drt_color, con_color),
      labels = c("Drought", "Control"),
      name = legend_title
    ) +
    theme_sigmaplot() +
    theme(
      legend.position = leg_position,
      legend.title.align = 0.5,
      legend.box.background = element_rect(size = 1, fill = NA)
    )
  
  if (!(HYS)) {
    gg <-
      gg +
      theme(axis.ticks.y = element_line(color = c(
        "transparent",
        "black",
        "black",
        "transparent"
      )))
  } else {
    gg <-
      gg +
      theme(axis.ticks.y = element_line(color = c(
        "transparent",
        "black",
        "black",
        "black",
        "black"
      )))
  }
  
  gg
  ggsave(file = save_file,
         width = 6,
         height = 4)
}
