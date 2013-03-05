package arx.core.control

import actors.Actor
import collection.mutable.Queue

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 6/26/11
 * Time: 8:17 PM
 * Created by nonvirtualthunk
 */

trait KillableActor extends Actor {
	KillableActor synchronized {
		KillableActor.actors += this
	}
	var contextLevel = KillableActor.GameLevel

	def name : String
}

trait SelfStartingActor extends KillableActor {
	start()
}

object KillableActor {
	val actors = Queue[KillableActor]()

	def kill (context : Int = -1) {
		for ( actor <- actors ){
			if ( context == -1 || actor.contextLevel <= context ) {
				actor ! "exit"
			}
		}
		var break = false
		while (! break) {
			if ( actors.forall { a => a.getState == Actor.State.Terminated || a.contextLevel > context } ) { break = true }
			Thread.sleep(1)
		}
	}

	def printAll () {
		for ( actor <- actors ) {
			println(actor.name)
		}
	}

	val ApplicationLevel = 10
	val GameLevel = 5
	val TemporaryLevel = 1
}