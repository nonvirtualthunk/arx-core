package arx.core.rich

import arx.core.units.UnitOfMeasure
import arx.core.ArxImplicits

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 4:44 PM
 * Created by nonvirtualthunk
 */

class ArxList[+T](intern : List[T]) {
	def usum[U <: UnitOfMeasure[U]]( f : (T) => U )(implicit start : U) : U = { intern.map( v => f(v) ).foldLeft(start)( (a,b) => a + b ) }
	def fsum ( f : (T) => Float ) : Float = { intern.foldLeft(0.0f) { (a,v) => a + f(v) } }
	def isum ( f : (T) => Int ) : Int = { intern.foldLeft(0) { (a,v) => a + f(v) } }

	def ofType [E <: AnyRef : Manifest] : List[E] = {
		val erasure = manifest[E].erasure
//			intern.filter ( i => erasure.isAssignableFrom(i.getClass) ).asInstanceOf[List[E]]
		intern.collect { case e if ( manifest[E].erasure.isAssignableFrom(e.getClass) ) => e.asInstanceOf[E] }
	}
	def notOfType [E <: AnyRef : Manifest] : List[E] = {
		intern.collect { case e if ( ! manifest[E].erasure.isAssignableFrom(e.getClass) ) => e.asInstanceOf[E] }
	}
	def findFirstWith[U] ( f : (T) => Option[U] ) : Option[(T,U)] = {
		val i = intern.iterator
		while ( i.hasNext ) {
			val e = i.next()
			f(e) match {
				case Some(v) => return Some((e,v))
				case _ =>
			}
		}
		None
	}
	def without[U >: T] ( t : U ) : List[T] = intern.filterNot { e : T => e == t }
	def firstOfType [E <: AnyRef : Manifest] : Option[E] = {
		val iter = intern.iterator
		while ( iter.hasNext ) {
			val n = iter.next()
			n match {
				case e if ( manifest[E].erasure.isAssignableFrom(e.getClass) ) => { return Some(e.asInstanceOf[E]) }
				case _ =>
			}
		}
		None
//			this.ofType[E].headOption
	}

	def forEachPair[U] ( func : (T,T) => U ) {
		ArxImplicits.forEachPair(intern)(func)
	}

}