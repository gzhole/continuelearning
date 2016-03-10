import java.util.NoSuchElementException

object QuestionInterview {
  //1. Find the pair that sum up 100
  def find[T](xs: List[T]): List[(T, T)] = xs match {
    case Nil => Nil
    case y :: Nil => Nil
    case y :: ys => {
      val checkMatch = checkMatchFun( y, ys)
      if (checkMatch._1) (y, checkMatch._2) :: find(drop( ys, checkMatch._2 )) else find(ys)
    }
  }

  private def checkMatchFun[T](v: T, xs: List[T]): (Boolean, T) = xs match {
    case Nil => ( false, v)
    case y :: ys => if (check(v, y )) (true, y) else checkMatchFun(v, ys)
  }

  private def sum(v1: Int, v2: Int): Boolean = {
    val sumResult = v1 + v2
    sumResult == 100
  }

  private def check[T](v1: T, v2: T): Boolean = {
    (v1, v2) match {
      case ( a: Int, b: Int) => sum( a, b)
      case _ => throw new NotImplementedError
    }
  }

  private def drop[T](xs: List[T], v: T): List[T] = {
    val ( f, s) = xs span (x => x != v)
    f ++ s.tail

  }
}

object Question1 {
  //1. Find the last element of a list.
  def last[T](xs: List[T]): T = xs match {
    case Nil => throw new NoSuchElementException()
    case y :: Nil => y
    case y :: ys => last( ys)
  }
}

object Question2 {
  //Find the last but one element of a list.
  def last[T](xs: List[T]): T = xs match {
    case Nil => throw new NoSuchElementException()
    case y :: Nil => throw new NoSuchElementException()
    case x :: y :: Nil => x
    case y :: ys => last( ys)
  }
}

object Question3 {
  //Find the Kth element of a list       
  def last[T](xs: List[T])(k: Int): T = {
    if (k > xs.size || k < 1) throw new NotImplementedError else if (k == 1 ) xs.head else last(xs.tail)(k - 1 )
  }
}

object Question4 {
  //Find the number of elements of a list.
  def last[T](xs: List[T]): Int = xs match {
    case Nil => throw new NoSuchElementException()
    case y :: Nil => 1
    case y :: ys => 1 + last( ys)
  }
 
   def length[T](xs: List[T]): Int = (xs foldRight ( 0)) ((x,y)=> 1+y)
}

object Question5 {
  //Reverse a list
  /*
  _ + _     _     _     _     _
     / / | |   | |   | |   | |   | |
    / /  | |   | |   | |   | |   | |
   /0/   |4|   |2|   |9|   |3|   |1|
  / /    | |   | |   | |   | |   | |
 /_/     |_|   |_|   |_|   |_|   |_|
        \____________nums___________/
        *
        */
  def last[T](xs: List[T]): List[T] = (List[T]() /: xs)((x, y) => y :: x)
}

object Question6 {
  //Find out whether a list is a palindrome
  def perform[T](xs: List[T]): Boolean = xs == xs.reverse
}

object Question7 {
  //Flatten a nested list structure.
  //flatten(List(List(1, 1), 2, List(3, List(5, 8))))

  def perform[T](xs: List[Any]): List[Any] = {
    val ys: List[List[Any]] = xs map (x => x match {

      case y :: ys => {
        perform( y :: ys)
      }

      case n => List( n)
    })
    ys.flatten

  }

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case x:: xs => flatten( x:: xs)
    case e => List( e)
  }

  def flatten1(ls: List[Any]): List[Any] = (ls map {
    case x:: xs => flatten( x:: xs)
    case e => List( e)
  }).flatten

}

object Question8 {
  //Eliminate consecutive duplicates of list elements.
  //List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  def compress[T](xs: List[T]): List[T] = (xs foldRight (List[T]()))((x, y) => ( if (y == Nil) x :: Nil else if (x == y.head) y else x :: y))
}

object Question9 {
  //Pack consecutive duplicates of list elements into sublists.
  //scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)) 
  def pack[T](xs: List[T]): List[List[T]] = (xs foldRight (List(List[T]())))((x, y) => (if (y == List(Nil)) List(List(x)) else if (x == y.head.head) (x :: y.head) :: y.tail else List(x) :: y))
}

object Question10 {
  //Run-length encoding of a list.
  //scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  def encode[T](xs: List[T]): List[(Int, T)] = (xs foldRight (List[(Int, T)]()))((x, y) => if (y == Nil) List((1, x)) else if (y.head._2 == x) (y.head._1 + 1, x) :: y.tail else (1, x) :: y)
}

object Question11 {
  //Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
  def encodeModified(xs: List[Any]): List[Any] = {
    Question10.encode(xs) map (x => if (x._1 == 1 ) x._2 else x)
  }
}

object Question12 {
  //Decode a run-length encoded list.
  //scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  //res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  def decode[T](xs: List[(Int, T)]): List[T] = {
    (xs foldRight (List[T]()))((x, y) => unpack(x) ::: y)
  }

  private def unpack[T](t: (Int, T)): List[T] = t match {
    case ( 1, _) => List(t. _2)
    case _ => t. _2 :: unpack((t. _1 - 1, t. _2))

  }
  def decode1[A](ls: List[(Int, A)]): List[A] =
    ls flatMap { e => List.fill(e. _1) (e. _2) }
}

object Question13 {
  //done in 10 
}

object Question14 {
  //Duplicate the elements of a list.
  //scala> duplicate(List('a, 'b, 'c, 'c, 'd))
  //res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd) 
  def duplicate[T](xs: List[T]): List[T] = xs flatMap (x => List.fill( 2) (x))
  def duplicate1[A](ls: List[A]): List[A] = ls flatMap { e => List(e, e) }
}

object Question15 {
  //Duplicate the elements of a list a given number of times.
  // duplicateN(3, List('a, 'b, 'c, 'c, 'd))
  //res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  def duplicateN[T](n: Int, xs: List[T]): List[T] = xs flatMap (x => List.fill(n)( x))
}

object Question16 {
  // Drop every Nth element from a list.
  //scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k) 
  def drop[T](n: Int, xs: List[T]): List[T] = {
    if (n > xs.size) xs else {
      (xs take n - 1) ::: (drop(n, xs drop n))
    }
  }
}

object Question17 {
  //Split a list into two parts.
  //The length of the first part is given. Use a Tuple for your result.
  //scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  def split[T](n: Int, xs: List[T]): (List[T], List[T]) = {
    (xs take n, xs drop n)
  }
}

object Question18 {
  //Extract a slice from a list.
  //Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
  //scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //res0: List[Symbol] = List('d, 'e, 'f, 'g)
  def slice[T](m: Int, n: Int, xs: List[T]): List[T] = {
    (xs drop m) take (n - m)
  }
}

object Question19 {
  // Rotate a list N places to the left.
  //scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

  //scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  def rotate[T](n: Int, xs: List[T]): List[T] = {
    if (n == 0) xs
    else if (n > 0 ) (xs drop n) ::: (xs take n)
    else (xs drop xs.size + n) ::: (xs take xs.size + n)
  }
}

object Question20 {
  //  Remove the Kth element from a list.
  //Return the list and the removed element in a Tuple. Elements are numbered from 0.
  //scala> removeAt(1, List('a, 'b, 'c, 'd))
  //res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  def removeAt[T](n: Int, xs: List[T]): (List[T], T) = {
    ((xs take (n)) ::: (xs drop n + 1), xs(n))
  }
}

object Question21 {
  // Insert an element at a given position into a list.
  //scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
  //res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  def insertAt[T](s: T, n: Int, xs: List[T]): List[Any] = {
    (xs take n) ::: (s :: (xs drop n))
  }
}

object Question22 {
  //Create a list containing all integers within a given range.
  //scala> range(4, 9)
  //res0: List[Int] = List(4, 5, 6, 7, 8, 9)
  def range(m: Int, n: Int): List[Int] = ( m to n).toList
}

object Question23 {
  // Extract a given number of randomly selected elements from a list.
  //scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
  //res0: List[Symbol] = List('e, 'd, 'a)
  //Hint: Use the solution to problem P20 
  private def getRandom(n: Int) = scala.util.Random.nextInt(n)
  def randomSelect[T](n: Int, xs: List[T]): List[T] = {
    require(n <= xs.size);
    (n, xs) match {
      case ( 0, _) => Nil
      case (_, _) => {
        val removeResult = Question20.removeAt(getRandom(xs.size), xs)
        removeResult._2 :: randomSelect(n - 1, removeResult. _1)
      }
    }
  }
}

object Question24 {
  //Lotto: Draw N different random numbers from the set 1..M.
  //scala> lotto(6, 49)
  //res0: List[Int] = List(23, 1, 17, 33, 21, 37) 
  def lotto(n: Int, range: Int): List[Int] = Question23.randomSelect(n, ( 1 to range).toList)
}

object Question25 {
  //Generate a random permutation of the elements of a list.
  //Hint: Use the solution of problem P23.
  //scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
  //res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f) 
  def randomPermute[T](xs: List[T]): List[T] = Question23.randomSelect(xs.size, xs)
}

object Question26 {
  //Generate the combinations of K distinct objects chosen from the N elements of a list.
  //In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
  //scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
  //res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ... 
  /*def combinations [T] (n : Int, xs :List[T]):List[List[T]] = {
   //1. all the possible combination
   //2. filter out the duplicate
   
    val r =  for {
       x      <-      xs
       y      <-      xs
       z      <-      xs
       if (x!=y && x!=z && y !=z)
    } yield (List(x,y,z))
   
    r
  }
 
  //n =1 xs = List(1,2,3) result= List(List(1), List(2), List(3))
 //n =2 xs = List(1,2,3) result = xs.size ^2
 def generator[T] (n: Int, xs: List[T]):List[List[T]] = {
              if (n==1) xs map (x=>List(x))
              else {
               generator(n-1, for {
                     x <- xs
                }yield (x)
               )
              }
 }*/

  // flatMapSublists is like list.flatMap, but instead of passing each element
  // to the function, it passes successive sublists of L.
  private def flatMapSublists[T, A](ls: List[T])(f: (List[T]) => List[List[A]]): List[List[A]] = ls match {
      case Nil => Nil
      case _ :: xs => f(ls) ::: flatMapSublists( xs)(f)
  }

  def combinations[A](n: Int, xs: List[A]): List[List[A]] = if (n == 0 ) List(Nil) else flatMapSublists(xs) { ys => combinations(n - 1, ys.tail) map {x=> ys.head :: x }}
}

object Question27 {
//a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
//scala> group3(List("Aldo", "Beat", " Carla", "David ", "Evi", "Flip", " Gary", "Hugo", "Ida"))
//res0: List[List[List[String]]] = List(List(List( Aldo, Beat), List(Carla , David, Evi), List(Flip, Gary, Hugo, Ida)), ... 
        def group3[T](xs: List[T]):List[List[List[T]]] = {
         for {
          x <- Question26.combinations( 2,xs)
          val difference = xs filterNot(z=>x.contains(z))
          y<-Question26.combinations( 3, difference)
         } yield List(x ,y ,difference filterNot(z=>y.contains(z)) )
       }
}      

object Question28 {
// We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of the list according to their length. E.g. short lists first, longer lists later, or vice versa.
//scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))

def lsort[T](xs: List[List[T]]):List[List[T]] =   xs.sortWith((x,y)=>x.size<y.size)
//Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.
//scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
//Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once. The third and fourth lists have length 3 and there are two list of this length. Finally, the last three lists have length 2. This is the most frequent length.
def lsortFreq[T](xs: List[List[T]]):List[List[T]]= {
//  val tran1 = xs map (x=> (x.size, x))
  val tran2 = xs groupBy(_.size)
  val tran3 = tran2.toSeq.sortWith(_. _2.size<_. _2.size)
  val tran4 = tran3 map (x=> x. _2)
  tran4.toList.flatten
}


}
