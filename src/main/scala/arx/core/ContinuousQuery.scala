package arx.core

/**
 *
 */

class ContinuousQuery[T : Manifest] ( val matchFunction : (AnyRef) => Option[T] ) extends Serializable {
	def this ( mf : PartialFunction[AnyRef,T] ) { this(mf.lift) }
	@transient var results : Set[T] = Set[T]()
//	@transient val added = new SynchronizedQueue[T]
//	@transient val removed = new SynchronizedQueue[T]
	@transient var source : Option[TContinuousQuerySource] = None

	def add ( x : AnyRef ) {
		matchFunction(x) match {
			case Some(t) => {
				if ( ! results.contains(t) ) {
					results = results + t
	//				added.enqueue(t)
					listeners.foreach( _.queryResultAdded(t) )
				}
			}
			case None =>
		}
	}
	def remove ( x : AnyRef ) {
		matchFunction(x) match {
			case Some(t) => {
				if ( results.contains(t) ) {
					results = results - t
	//				removed.enqueue(t)
					listeners.foreach( _.queryResultRemoved(t) )
				}
			}
			case None =>
		}
	}

	@transient var listeners : List[ContinuousQueryListener[T]] = Nil
	def withListener( l : ContinuousQueryListener[T] ) = { this.listeners ::= l; this }
}
trait ContinuousQueryListener[T] {
	def queryResultAdded ( t : T )
	def queryResultRemoved ( t : T )
}

trait TContinuousQuerySource {
	def registerQuery ( query : ContinuousQuery[_] )
	def unregisterQuery ( query : ContinuousQuery[_] )
}