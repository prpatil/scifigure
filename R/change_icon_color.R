#' Change Icon Color
#'
#' @param icon icon, which is a 4D array
#' @param color color to change, in text form, passed to \code{\link{col2rgb}}
#'
#' @return A 4D array of the icon
#' @export
#'
#' @importFrom grDevices col2rgb
#'
#' @examples
#' icon = scifigure::icons[[2]]
#' color = "blue"
#' original_color = "red"
#' icon2 = change_icon_color(icon, color)
change_icon_color = function(icon, color) {
  icon_i = icon[,,4]
  color = col2rgb(color, alpha = TRUE) / 255
  arr = array(dim = dim(icon))
  for (i in 1:3) {
    arr[,,i] = icon_i * color[i]
  }
  arr[,,4] = icon_i
  return(arr)
}
