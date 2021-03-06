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
        panel.background = element_blank(),
        # Clear background
        panel.border = element_rect(size = 1, fill = NA),
        # Full border around plot
        legend.key = element_rect(fill = NA),
        # Each legend item (e.g., lines) doesn't
        # have a background
        axis.ticks.length.y = unit(ticklen, "cm"),
        # Tick size
        axis.ticks.length.y.right = unit(ticklen, "cm"),
        # Tick size
        axis.ticks.x = element_blank(),
        # Tick size
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
        ),
        # Remove x 
      )
    return(sigmaplot)
  }


scale_y_custom_ticks <-
  function(scale_factor,
           left_y_title,
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
          name = left_y_title,
          # Title of left axis
          limits = c(0, 0.36),
          # Limits of both axes (based off left side)
          breaks = c(0,
                     0.1,
                     0.2,
                     0.3),
          # Left breaks
          expand = c(0, 0),
          # Removes padding around axes
          labels = c(0,
                     0.1,
                     0.2,
                     0.3),
          # Left break labels
          sec.axis = sec_axis(
            ~ . * scale_factor,
            # Apply scaling factor to second axis
            name = "Ambient precipitation (mm)",
            # Title of right axis
            breaks = c(0,
                       30,
                       60),
            # Right breaks
            labels = c(0,
                       30,
                       60),
            # Right break labels
          )
        )
    } else {
      # If site is HYS
      obj <-
        scale_y_continuous(
          name = left_y_title,
          # Title of left axis
          limits = c(0, 0.36),
          # Limits of both axes (based off left side)
          breaks = c(0,
                     0.1,
                     0.2,
                     0.3),
          # Left breaks
          expand = c(0, 0),
          # Removes padding around axes
          labels = c(0,
                     0.1,
                     0.2,
                     0.3),
          # Left break labels
          sec.axis = sec_axis(
            ~ . * scale_factor,
            # Apply scaling factor to second axis
            name = "Ambient precipitation (mm)",
            # Title of right axis
            breaks = c(0,
                       30,
                       60),
            # Right breaks
            labels = c(0,
                       30,
                       60),
            # Right break labels
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
        limits = as.Date(c('2017-04-01', '2017-09-15')),
        # Axis limits as dates
        expand = c(0, 0),
        # Remove padding around axes
        date_breaks = "month",
        # Label each month
        date_labels = "%b",
        # Use the Julian month
        name = "" # No title needed on the x axis
      )
    
    return(obj)
  }


## Provide some config arguments ----

config <-
  function() {
    # This function acts as a config with "constants" to be used in data prep and plotting
    
    return(
      list(
        year_to_plot = 2017,
        ## 2014 - 2017, eg
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
    # This function cleans the soil VWC and precipitation event data and prepares
    # it for plotting
    #
    # HYS: boolean, T = site is HYS (HYS will have larger axis limits and more ticks)
    # chr_only: boolean, T = only take the chronic treatment. Otherwise averages
    # chronic and intense treatments as "drought"
    # vwc45: boolean, T = takes the field "vwc45". Otherwise takes "vwc90"
    
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
    
    # Convert vwc timestamp to date format
    vwc$TIMESTAMP <-
      as.Date(vwc$TIMESTAMP, format = "%m/%d/%Y")
    
    # Recode treatments as "DRT" and drop "INT" treatment if specified by chr_only
    if (chr_only) {
      vwc <-
        vwc %>%
        dplyr::filter(Treatment != "INT")
      vwc$Treatment <- recode(vwc$Treatment, "CHR" = "DRT")
    } else {
      vwc$Treatment <-
        recode(vwc$Treatment, "CHR" = "DRT", "INT" = "DRT")
    }
    
    # Select year to plot
    year_to_plot <- config()[[1]]
    
    # Take only the field of data specified by vwc45 argument
    if (vwc45) {
      vwc_plot <-
        vwc %>%
        dplyr::filter(lubridate::year(TIMESTAMP) == year_to_plot) %>% # Keep only this year
        dplyr::group_by(TIMESTAMP, Treatment) %>% # Group by date (day) and treatment
        dplyr::summarise(VWC = mean(VWC45, na.rm = T)) %>% # Take the average VWC across the day
        dplyr::filter(lubridate::date(TIMESTAMP) > "2017-04-01") %>%  # Only want dates after April 1
        dplyr::filter(lubridate::date(TIMESTAMP) < "2017-09-15") # Only want dates before Sept 15
    } else {
      vwc_plot <-
        vwc %>%
        dplyr::filter(lubridate::year(TIMESTAMP) == year_to_plot) %>% # Keep only this year
        dplyr::group_by(TIMESTAMP, Treatment) %>% # Group by date (day) and treatment
        dplyr::summarise(VWC = mean(VWC90, na.rm = T)) %>% # Take the average VWC across the day
        dplyr::filter(lubridate::date(TIMESTAMP) > "2017-04-01") %>% # Only want dates after April 1
        dplyr::filter(lubridate::date(TIMESTAMP) < "2017-09-15") # Only want dates before Sept 15
    }
    
    # Read in precipitation data
    ppt <-
      readxl::read_excel(infile_ppt, sheet = sheet_name)
    
    # Convert date to correct format
    ppt$date <-
      as.Date(ppt$date, format = "%m/%d/%Y")
    
    ppt <-
      ppt %>%
      dplyr::filter(date > "2017-04-01") %>% # Only want dates after April 1
      dplyr::filter(date < "2017-09-15") # Only want dates before Sept 15
    
    # Return list of data frames
    return(list(vwc_plot, ppt))
  }

## Main plot function ----

make_vwc_ppt_plot <- function(# Data items
  HYS = F,
  vwc45 = vwc45,
  chr_only = chr_only,
  # Plot items
  legend_title,
  leg_position = c(0.99, 0.99), # Aligns to top right
  left_y_title,
  save_file = save_file) {
  # This function plots the data
  #
  # HYS: boolean, T = site is HYS (HYS will have larger axis limits and more ticks)
  # chr_only: boolean, T = only take the chronic treatment. Otherwise averages
  # chronic and intense treatments as "drought"
  # vwc45: boolean, T = takes the field "vwc45". Otherwise takes "vwc90"
  # legend_title: String for name of the legend or the plot overall
  # leg_position: Coordinates to indicate position of the legend inside the plot
  # save_file: String of the path and file name where the file is to be saved
  
  # Establish some constants, per the config function
  year_to_plot <- config()[[1]]
  bar_color <- config()[[2]]
  drt_color <- config()[[3]]
  con_color <- config()[[4]]
  scale_factor <- config()[[5]]
  
  # Gather data for plotting
  tmp <-
    prep_data(HYS,
              chr_only,
              vwc45)
  vwc_plot <- tmp[[1]]
  ppt <- tmp[[2]]
  
  # Make plot object
  gg <-
    ggplot() +
    # Precipitation
    geom_col(
      data = ppt,
      aes(x = date, y = `ambient precip (mm)` / scale_factor),
      # Make sure data is scaled accordingly
      fill = bar_color # Color the bars accordingly
    ) +
    # Soil VWC%
    geom_line(data = vwc_plot,
              aes(
                x = TIMESTAMP,
                y = VWC,
                group = Treatment,
                # Different colors per treatment
                color = Treatment # Different colors per treatment
              )) +
    scale_y_custom_ticks(scale_factor,
                         left_y_title,
                         HYS = HYS) + # Add custom ticks for y axes depending on site
    scale_x_custom_ticks() + # Customize x ticks
    scale_color_manual(
      values = c(drt_color, con_color),
      # Line colors
      labels = c("Drought", "Control"),
      # Line labels for treatments
      name = legend_title # Legend title
    ) +
    theme_sigmaplot() + # General themeing / padding
    theme(
      # Additional themeing for formatting legend
      legend.position = leg_position,
      legend.title.align = 0.5, # Center justify text of the legend title
      legend.justification = c("right", "top"), # Align legend by top right corner
      legend.box.background = element_blank(), # Box around the legend
      legend.background = element_blank()
    )
  
  # Color the ticks specifically so that the top and bottom are not overlapping with the
  # box surrounding the overall plot (looks weird and sloppy if this happens)
  if (!(HYS)) {
    gg <-
      gg +
      theme(axis.ticks.y = element_line(color = c(
        "transparent",
        rep("black", 3)
      )))
  } else {
    gg <-
      gg +
      theme(axis.ticks.y = element_line(color = c("transparent",
                                                  rep("black", 4))))
  }
  
  gg
  # Save plot
  ggsave(file = save_file,
         width = 6,
         height = 4)
}
