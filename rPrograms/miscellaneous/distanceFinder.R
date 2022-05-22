#Write a function to find the distance between 2 points
dist <- function(x1, y1, x2, y2)
{
  distance = sqrt((x1 - x2)^2 + (y1 - y2)^2)
  print(distance)
}
dist(1, 1, 3, 3)