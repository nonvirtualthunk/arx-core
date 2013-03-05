package arx.core

import traits.TUpdatableEntity

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 2:56 PM
 * Created by nonvirtualthunk
 */


object Application {
	var openGLThread = new ThreadLocal[Boolean]{override def initialValue() = false}

	var registeredUpdatables = List[TUpdatableEntity]()
	var registeredUpdatablesRenderThread = List[TUpdatableEntity]()
	var toDoOnQuit : List[()=>Unit] = Nil

	var ticks = 0

	def isOpenGLThread = openGLThread.get

	def registerForUpdate ( u : TUpdatableEntity ) { registeredUpdatables ::= u }
	def registerForRenderThreadUpdate ( u : TUpdatableEntity ) { registeredUpdatablesRenderThread ::= u }
	def onQuit ( func : () => Unit ) { toDoOnQuit ::= func }
}