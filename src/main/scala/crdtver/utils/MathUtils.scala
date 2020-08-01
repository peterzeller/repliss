package crdtver.utils

object MathUtils {
  def sign(x: Int): Int =
    if (x > 0) 1
    else if (x == 0) 0
    else -1


  def euclideanDiv(x: Int, y: Int): Int = {
    val r = x / y
    if (x < 0 && r * y != x) r - sign(y) else r
  }

  def euclideanMod(x: Int, y: Int): Int = {
    x - euclideanDiv(x, y) * y
  }
}
