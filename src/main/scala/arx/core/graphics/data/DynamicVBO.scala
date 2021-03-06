package arx.core.graphics.data

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/4/12
 * Time: 8:59 AM
 * Created by nonvirtualthunk
 */

import org.lwjgl.opengl.GL15
import java.util.concurrent.atomic.AtomicInteger

class DynamicVBO(activeArrays_arg:Int) extends VBO(activeArrays_arg) {
	var state : AtomicInteger = new AtomicInteger(DynamicVBO.Clean)


	override def writingActive_=(b: Boolean) {
		super.writingActive_=(b)
	}

	override def solidify(usage: Int = GL15.GL_DYNAMIC_DRAW) {
		super.solidify(usage)
		state.set( DynamicVBO.Solidified )
	}

	override def solidifyIfNecessary(usage: Int = GL15.GL_DYNAMIC_DRAW) = {
		if ( state.get == DynamicVBO.Updated ) {
			super.solidifyIfNecessary(usage)
		} else { false }
	}
}

object DynamicVBO {
	val Clean = 0
	val Dirty = 1
	val Updating = 2
	val Updated = 3
	val Solidified = 0
}