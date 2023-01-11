package es.weso.wshex.matcher

case class MatchOptions(mergeOrs: Boolean) {
  def withMergeOrs(mergeOrs: Boolean): MatchOptions = this.copy(mergeOrs = mergeOrs)
}

object MatchOptions {
  def default: MatchOptions = MatchOptions(
    mergeOrs = false
  )
}
