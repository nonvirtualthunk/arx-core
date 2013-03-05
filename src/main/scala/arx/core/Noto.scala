package arx.core

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 4:25 PM
 * Created by nonvirtualthunk
 */

object Noto {
	def info ( str : String ) { println(str) }
	def warn ( str : String ) { println("[warn] " + str) }
	def error ( str : String ) { println("[error] " + str) }
	def finest ( str : String ) { println("[finest] " + str) }
}