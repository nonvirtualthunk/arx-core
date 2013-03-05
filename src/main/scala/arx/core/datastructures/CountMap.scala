package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/2/13
 * Time: 12:07 PM
 * Created by nonvirtualthunk
 */

import collection.mutable

class CountMap[K] (defaultCount : Float = 0.0f) {
	val intern = new mutable.HashMap[K,Float]

	def apply ( key : K ) = intern.getOrElse(key,defaultCount)
	def update ( key : K , value : Float ) { intern(key) = value }
	def get ( key : K ) = intern.get(key)
	def getOrElse ( key : K , f : Float ) = intern.getOrElse(key,f)
	def getOrElseUpdate ( key : K , f : Float ) = intern.getOrElseUpdate(key,f)
	def contains ( key : K ) = intern.contains(key)
	def keys = intern.keys
	def keyList = intern.keys.toList
	def keySet = intern.keySet

	def increment ( key : K , incr : Float ) { intern(key) = intern.getOrElse(key,defaultCount) + incr }
	def decrement ( key : K , decr : Float ) { intern(key) = intern.getOrElse(key,defaultCount) - decr }
}