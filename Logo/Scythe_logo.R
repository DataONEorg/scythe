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

scythe_logo <- sticker(wikicomm_image,
                       s_x = 1,
                       s_y = 1,
                       package = "Scythe",
                       p_size = 20,
                       p_x = 1,
                       p_y = 1,
                       p_color = "black",
                       filename = "Logo/Scythe_Hex_Sticker.png")

scythe_logo
