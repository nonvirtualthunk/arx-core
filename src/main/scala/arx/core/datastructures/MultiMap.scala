package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/30/12
 * Time: 12:14 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import collection.mutable
import collection.mutable.ListBuffer

class MultiMap[K,V] {
	val intern = new mutable.HashMap[K,ListBuffer[V]]
	def add ( k : K , v : V ) {
		intern.getOrElseUpdate(k,new ListBuffer[V]).append(v)
	}
	def addAll ( k : K , v : Traversable[V] ) { for ( subV <- v ) { add(k,subV) } }
	def remove ( k : K , v : V ) : Boolean = {
		val buf = intern.getOrElse(k,new ListBuffer[V])
		buf.indexOf(v) match {
			case -1 => false
			case i => buf.remove(i); true
		}
	}
	def fetchAndRemoveHead ( k : K ) : Option[V] = {
		val buf = intern.getOrElse(k,new ListBuffer[V])
		if ( buf.isEmpty ) { None }
		else {
			val ret = buf.head
			buf.remove(0)
			Some(ret)
		}
	}
	def removeAll ( k : K ) { intern.remove(k) }
	def clear () { intern.clear() }
	def contains ( k : K ) = intern.contains(k)
	def get ( k : K ) = intern.getOrElse(k,Nil)

	def toList : List[(K,ListBuffer[V])] = intern.toList
	def toCountList : List[(K,Int)] = intern.toList.map( tup => tup._1 -> tup._2.size )

	def values = intern.values
}