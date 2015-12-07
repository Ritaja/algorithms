import scala.util.control.TailCalls._

/**
  * algorithm reference:
  * @see http://www.algorithmist.com/index.php/Merge_sort
  */
object MergeSort {
  /**
    * compares two List and merges them after sorting.
    * The Collection in scala needs to be ordered
    * in order to be able to work with the
    * comparison operators.
    *
    * @param left one half of the List to merge
    * @param right one half of the List to merge
    * @return sorted List merged from provided Lists
    */
  def merge[T <% Ordered[T]](merged: List[T], left: List[T], right: List[T]): TailRec[List[T]] = {
    if (left.isEmpty) {
      done(right.reverse ::: merged)
    } else if (right.isEmpty) {
      done(right.reverse ::: merged)
    } else if (right.head < left.head) {
      tailcall(merge(right.head :: merged, right.tail, left))
    } else {
      tailcall(merge(left.head :: merged, left.tail, right))
    }
  }

  /**
    * performs the merge sort operation on the provided List.
    *
    * @param unsorted List of to sort
    * @return sorted List
    */
  def sort[T <% Ordered[T]](unsorted: List[T]): List[T] = {
    // already sorted list, no need to sort
    if (unsorted.length <= 1) {
      return unsorted
    }
    //slice up the List into two parts
    var (left, right) = unsorted.splitAt(unsorted.length / 2)
    // recursively call function (till size of left and right are just = 1). The divide step
    left = sort(left)
    right = sort(right)
    // do the comparison on divided parts and merge
    return merge(List(), left, right).result.reverse
  }

  def main(args: Array[String]): Unit ={
    println("Ordered: "+MergeSort.sort(List(3,2,5,4,9,8,7)))
  }
}
