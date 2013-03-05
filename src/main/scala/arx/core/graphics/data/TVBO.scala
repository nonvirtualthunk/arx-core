package arx.core.graphics.data

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 4/9/12
 * Time: 11:29 AM
 * Created by nonvirtualthunk
 */

import arx.core.vec._
import org.lwjgl.opengl.GL15

trait TVBOLike {
	def unsolidify ()
	def clear ()
}

trait TVBO extends TVBOLike {
	def writingActive : Boolean
	def writingActive_= ( b : Boolean )
	def bind ()
	def unbind ()
	def solidify (usage: Int = GL15.GL_DYNAMIC_DRAW)
	def solidify (usage: Int, numPointsToSolidify : Int , numIndicesToSolidify : Int )
	def solidifyIfNecessary (usage: Int = GL15.GL_DYNAMIC_DRAW) : Boolean
	def setOpenGLPointers ()
	def unsetOpenGLPointers ()
	def drawElements (primitive: Int,start: Int = 0,length: Int = -1,skipPostDraw : Boolean = false)
	def draw (primitive: Int,start: Int = 0,length: Int = -1)
	def isSolidified : Boolean

	def numPoints: Int
	def numPoints_= (n: Int)
	def numIndices: Int
	def numIndices_= (n: Int)
	def numPointsHint (n: Int)
	def numIndicesHint (n: Int)

	def getC (n: Int): Vec4f
	def getV (n: Int): Vec3f
	def getN (n: Int): Vec3f
	def getT (n: Int): Vec2f

	def getI (n: Int): Int

	def setC (n: Int, v: ReadVec4f)
	def setV (n: Int, v: ReadVec3f)
	def setN (n: Int, v: ReadVec3f)
	def setT (n: Int, v: ReadVec2f)

	def setC (n: Int, r: Float,g: Float, b: Float, a: Float)
	def setV (n: Int, x: Float,y: Float,z: Float)
	def setN (n: Int, x: Float,y: Float,z: Float)
	def setT (n: Int, x: Float,y: Float)

	def setI (n: Int, i: Short)
}