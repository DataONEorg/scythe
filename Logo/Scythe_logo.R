### Great Logo for Scythe Package
### Althea Marks
### Created 2022_3_16

## Install packages

#install.packages("sysfonts")
#install.packages("magick")
#install.packages("hexSticker")
#install.packages("tidyverse")
#install.packages("showtext")

library(magick)
library(hexSticker)
library(sysfonts)
library(tidyverse)

## Construct hex sticker logo

# import image from 
wikicomm_image <- "Logo/Glazed_pottery_tile_man_with_a_scythe_01.jpg"

library(showtext)
# Loading Google fonts (http://www.google.com/fonts)
font_add_google("Architects Daughter", "AD") # My 1st pick - fuller and more upright
font_add_google("Nothing You Could Do", "NYCD") # looks like how 'scythe' sounds, a bit italicized 

showtext_auto() 

scythe_logo <- sticker(wikicomm_image, #subplot image
                       s_x = 1, #subplot position relative to 1(center)
                       s_y = 1,
                       s_width = 1.1,
                       s_height = 1.1,
                       package = "Scythe", #text displayed for package name
                       p_family = "AD",
                       p_size = 75, #font size
                       p_x = 1, # position font
                       p_y = 0.45,
                       p_color = "grey30",
                       dpi = 1000, # plot resolution
                       asp = 1, # aspect ratio of image file
                       h_color = "grey85", # boarder color
                       h_size = 3, # boarder size
                       white_around_sticker = T,
                       spotlight = F,
                       filename = "Logo/Scythe_Hex_Sticker-AD.png")

scythe_logo
