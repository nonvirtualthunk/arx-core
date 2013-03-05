package arx.core.rich

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/13/13
 * Time: 3:05 PM
 * Created by nonvirtualthunk
 */

import java.text.Normalizer
import java.util.regex.Pattern

class ArxString(val intern: String) {
	def -(other: String) : ArxString = {
		var tmp = this
		while ( tmp.intern.lastIndexOf(other) >= 0 ) {
			val idx = tmp.intern.lastIndexOf(other)
			tmp = new ArxString( intern.substring(0,idx) + intern.substring(idx + other.length) )
		}
		tmp
	}
	override def toString = intern

	def toCamelCase : String = {
		val sb = new StringBuilder
		for ( i <- 0 until intern.size ) {
			if ( i != 0 && intern(i-1) == ' ' ) {
				sb.append(intern(i).toUpper)
			} else if ( intern(i) != ' ' ) {
				sb.append(intern(i))
			}
		}
		sb.toString()
	}

	def fromCamelCase : String = {
		var previousWasSpace = false
		val sb = new StringBuilder
		for ( i <- 0 until intern.size ) {
			if ( ! previousWasSpace && i != 0 && intern(i).isUpper ) {
				sb.append(" ")
			}
			sb.append(intern(i))
			if ( intern(i) == ' ' ) { previousWasSpace = true }
			else { previousWasSpace = false }
		}
		sb.toString()
	}
	def capitalizeAll : String = {
		val sb = new StringBuilder
		for ( i <- 0 until intern.size ) {
			if ( i == 0 || intern(i - 1) == ' ' ) {
				sb.append( intern(i).toUpper )
			} else { sb.append( intern(i) ) }
		}
		sb.toString()
	}

	def stripAccents = {
		val normalized = Normalizer.normalize(intern,Normalizer.Form.NFD)
		val pattern = ArxString.accentRemovalPattern
		pattern.matcher(normalized).replaceAll("")
	}
}

object ArxString {
	val accentRemovalPattern = Pattern.compile("\\p{InCombiningDiacriticalMarks}+")
}