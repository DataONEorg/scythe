### Great Logo for Scythe Package
### Althea Marks
### Created 2022_3_16

## Install packages

#install.packages("sysfonts")
#install.packages("magick")
#install.packages("hexSticker")
#install.packages("tidyverse")

library(magick)
library(hexSticker)
library(sysfonts)
library(tidyverse)

## Construct hex sticker logo

# import image 

wikicomm_image <- "Logo/Glazed_pottery_tile_man_with_a_scythe_01.jpg"

scythe_logo <- sticker(wikicomm_image, #subplot image
                       s_x = 1, #subplot position relative to 1(center)
                       s_y = 1,
                       s_width = 1.1,
                       s_height = 1.1,
                       package = "Scythe", #text displayed for package name
                       p_size = 20, #font size
                       p_x = 1, # position font
                       p_y = 0.45,
                       p_color = "grey30",
                       dpi = 1000, # plot resolution
                       asp = 1, # aspect ratio of image file
                       h_color = "grey30", # boarder color
                       h_size = 3, # boarder size
                       white_around_sticker = T,
                       spotlight = F,
                       filename = "Logo/Scythe_Hex_Sticker.png")

scythe_logo
