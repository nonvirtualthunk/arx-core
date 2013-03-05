package arx.core.datastructures

import arx.core.control.KillableActor

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/30/12
 * Time: 12:54 PM
 * Created by nonvirtualthunk
 */


abstract class KillableThread(var level:Int) extends Thread {
	KillableThread.threads ::= this

	var ended = false

	final override def run() {
		while ( ! ended ) {
			whileRunningDo()
		}
	}

	def whileRunningDo ()

	def end () {
		kill()
	}
	def kill () {
		if ( ! ended ) {
			ended = true
			interrupt()
		}
	}
}
object KillableThread {
	var threads = List[KillableThread]()

	def kill (level:Int=10) {
		threads.foreach ( t => if ( t.level <= level ) { t.kill() } )
	}
}
object Killable {
	def kill (level : Int = 10) {
		KillableThread.kill(level)
		KillableActor.kill(level)
	}

	val ApplicationLevel = 10
	val GameLevel = 5
	val TemporaryLevel = 1
}
