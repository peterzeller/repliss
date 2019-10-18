package crdtver.utils


object MapUtils {

  sealed trait JoinInput[+L, +R]

  case class OnlyLeft[L](left: L) extends JoinInput[L, Nothing]

  case class OnlyRight[R](right: R) extends JoinInput[Nothing, R]

  case class Both[L, R](left: L, right: R) extends JoinInput[L, R]


  def joinInput[L, R](l: Option[L], r: Option[R]): JoinInput[L, R] = (l, r) match {
    case (Some(a), Some(b)) => Both(a, b)
    case (Some(a), None) => OnlyLeft(a)
    case (None, Some(b)) => OnlyRight(b)
  }

  implicit class MapExtensions[K, V](base: Map[K, V]) {

    def mergeG[V2, V3](other: Map[K, V2], mergeValues: JoinInput[V, V2] => Option[V3]): Map[K, V3] = {
      (base.keys ++ other.keys)
        .flatMap(k => mergeValues(joinInput(base.get(k), other.get(k))).map(k -> _))
        .toMap
    }

    def merge(other: Map[K, V], mergeValues: (V, V) => V): Map[K, V] = {
      base ++ (other.map { case (k, v) => k -> (base.get(k) match {
        case Some(v2) => mergeValues(v2, v)
        case None => v
      })
      })
    }

  }

}
