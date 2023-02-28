# load the necessary packages
library(hexSticker) # hexSticker generator
library(magick) # Advanced image processing
library(sysfonts) # font selection
library(tidyverse)


# load the image from the package
royalty_free_image <- image_read(system.file("logo", "grapes-g0cbd3743f_1280.png",
                                             package = "rrclust"
))

# set the package name
package_name <- "rrclust"

# set the package url
url_package <- "https://gitea.cynkra.com/cynkra/rrclust"

# load the fonts from the package
fonts_dataset <- font_files()

# add the font to the system
font_add("Verdana", "Verdana.ttf")

# create your sticker
sticker(
  subplot = royalty_free_image, #* image/ggplot object
  s_x = 1.0, #                   * subplot x-position (left/right position)
  s_y = 0.9, #                   * subplot y-position (up/down position)
  s_width = 1.0, #               * subplot width
  s_height = 0.9, #              * subplot height
  package = package_name, #      * package name to be displayed in hexSticker
  p_x = 1, #                     * package name x-position
  p_y = 1.6, #                   * package name y-position
  p_color = "black", #           * package name color
  p_family = "Verdana", #      * package name font family
  p_fontface = "plain", #        * package name font face
  p_size = 6.5, #                * package name font size
  h_size = 1.2, #                * hexSticker size
  h_fill = "#ffffff", #          * hexSticker background color
  h_color = "#000000", #         * hexsticker border color
  spotlight = FALSE, #           * add spotlight effect to hexSticker
  l_x = 1, #                     * spotlight effect x-position
  l_y = 0.5, #                   * spotlight effect y-position
  l_width = 3, #                 * spotlight effect width
  l_height = 3, #                * spotlight effect height
  l_alpha = 0.4, #               * spotlight effect level
  url = url_package, #           * url to add to the hexSticker
  u_x = 1, #                     * url x-position
  u_y = 0.08, #                  * url y-position
  u_color = "black", #           * url font color
  u_family = "Verdana", #      * url font family
  u_size = 1.0, #                * url font size
  u_angle = 30, #                * url angle
  white_around_sticker = FALSE, # * add white blocks around sticker
  filename = file.path(
    "man", "figures",
    paste0(
      "logo",
      ".png"
    ) #                          * save hexSticker to file
  ),
  asp = 1, #                     * adjust aspect ratio
  dpi = 300 #                    * Dots Per Inch resolution
)

# read the logo.png file and display it in the viewer pane
magick::image_read("man/figures/logo.png")

# restart the R session
rstudioapi::restartSession()
