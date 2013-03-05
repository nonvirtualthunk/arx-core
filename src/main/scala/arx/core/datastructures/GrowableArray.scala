package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 12/21/12
 * Time: 1:41 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import java.util
import arx.core.THasSortKey

class GrowableArray[T <: AnyRef : Manifest] extends Traversable[T] {
	var _size = 0
	var intern = manifest[T].newArray(16)

	override def size = _size

	def ensureSize ( n : Int ) {
		if ( intern.length <= n ) {
			val newArray = manifest[T].newArray(intern.length*2)
			System.arraycopy(intern,0,newArray,0,intern.length)
			intern = newArray
		}
	}

	def append ( t : T ) {
		ensureSize(size+1)
		intern(size) = t
		_size += 1
	}

	def apply ( i : Int ) = intern(i)
	def update ( i : Int , t : T ) { intern(i) = t }

	def sortBy ( sortKeyFunc : (T) => Int ){
		val comparator = new FunctionComparator(sortKeyFunc)
		util.Arrays.sort(intern,0,size,comparator)
	}

	def foreach[U](f: (T) => U) {
		var i = 0; while ( i < size ) {
			f(intern(i))
		i += 1}
	}

	def remove ( v : T ) {
		var i = 0; while ( i < size ) {
			if ( intern(i) == v ) {
				remove(i)
				i = size
			}
		i += 1}
	}
	def remove ( i : Int ) {
		intern(i) = intern(size-1) //pop and swap
		_size -= 1
	}
	def indexWhere ( f : (T) => Boolean ) : Int = {
		var i = 0; while ( i < size ) {
			if ( f(intern(i)) ) {
				return i
			}
		i += 1}
		-1
	}
}

class GrowableSortableArray[T <: THasSortKey : Manifest] extends GrowableArray[T] {
	def sort () {
		val comparator = new HasSortKeyComparator[T]
		util.Arrays.sort(intern,0,size,comparator)
	}
}

class FunctionComparator[T](func : (T) => Int) extends util.Comparator[T] {
	def compare(o1: T, o2: T) = {
		func(o1) - func(o2)
	}
}

class HasSortKeyComparator[T <: THasSortKey] extends util.Comparator[T]{
	def compare(o1: T, o2: T) = {
		val sortKey1 = o1.sortKey
		val sortKey2 = o2.sortKey
		if ( sortKey1 < sortKey2 ) { -1 }
		else if ( sortKey1 == sortKey2 ) { 0 }
		else { 1 }

	}
}