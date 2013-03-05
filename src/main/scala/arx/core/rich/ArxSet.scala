package arx.core.rich

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 4:47 PM
 * Created by nonvirtualthunk
 */

class ArxSet[+T](intern : Set[T]) {
	def ofType [E <: AnyRef : Manifest] : Set[E] = {
		intern.collect { case e if ( manifest[E].erasure.isAssignableFrom(e.getClass) ) => e.asInstanceOf[E] }
	}
	def notOfType [E <: AnyRef : Manifest] : Set[E] = {
		intern.collect { case e if ( ! manifest[E].erasure.isAssignableFrom(e.getClass) ) => e.asInstanceOf[E] }
	}
}