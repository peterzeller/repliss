package crdtver.utils

object MathUtils {
  def sign(x: BigInt): BigInt =
    if (x > 0) 1
    else if (x == 0) 0
    else -1


  def euclideanDiv(x: BigInt, y: BigInt): BigInt = {
    val r = x / y
    if (x < 0 && r * y != x) r - sign(y) else r
  }

  def euclideanMod(x: BigInt, y: BigInt): BigInt = {
    x - euclideanDiv(x, y) * y
  }
}
