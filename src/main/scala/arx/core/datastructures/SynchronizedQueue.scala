package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/4/12
 * Time: 10:26 AM
 * Created by nonvirtualthunk
 */

import collection.mutable

class SynchronizedQueue[T] extends mutable.Queue[T] {
	def dequeueOpt() = {
		synchronized {
			if ( super.nonEmpty ) { Some(super.dequeue()) }
			else { None }
		}
	}

	override def contains(elem : Any) : Boolean = { synchronized { super.contains(elem) } }
	override def clear () { synchronized { super.clear() } }
	override def dequeue() = synchronized { super.dequeue() }
	override def nonEmpty = synchronized { super.nonEmpty }
	override def isEmpty = synchronized { super.isEmpty }
	override def enqueue ( elems : T*) { synchronized[this.type] { super.++=(elems) } }
	override def += ( elem : T ) = { synchronized[this.type] { super.+=(elem) } }
	override def ++= ( elems : TraversableOnce[T]) = { synchronized[this.type] { super.++=(elems) } }
}