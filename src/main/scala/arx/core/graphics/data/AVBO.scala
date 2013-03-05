package arx.core.graphics.data

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/25/12
 * Time: 2:41 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import org.lwjgl.opengl.GL11._
import collection.mutable.{ArrayBuffer, SynchronizedQueue}
import org.lwjgl.opengl.GL15._
import arx.Prelude
import shader.Shader
import arx.core.vec._
import arx.application.{Conflux, Noto}
import arx.graphics.Util._
import org.lwjgl.util.glu.GLU._
import arx.resource.ResourceManager
import org.lwjgl.opengl.{GL30, GL15, GL21, GL20}
import traits.TRenderTarget
import view.Camera
import simplex3d.math.floatx.functions

class AVBO(var _attribProfile : AttributeProfile) extends DynamicVBO(0) with TRenderTarget {
	points.byteStride = _attribProfile.byteStride

	override def activeArrays_=(i: Int) { if ( i != 0 ) {throw new UnsupportedOperationException }}

	def attribProfile_= ( p : AttributeProfile ) {
		_attribProfile = p
		points.byteStride = p.byteStride
	}
	def attribProfile = _attribProfile

	override def setOpenGLPointers() {
		if ( VBO.boundArrayBuffer != 0 ) {
			var i = _attribProfile.attributes.length - 1;while ( i >= 0 ) {
				VBO.enableVertexAttribArray(i)
				val attribute = _attribProfile.attributes(i)
				val normalize = attribute.dataType == GL_BYTE || attribute.dataType == GL_UNSIGNED_BYTE || attribute.dataType == GL_SHORT || attribute.dataType == GL_UNSIGNED_SHORT ||
										attribute.dataType == GL_INT || attribute.dataType == GL_UNSIGNED_INT
				GL20.glVertexAttribPointer(i,attribute.size,attribute.dataType,normalize,_attribProfile.byteStride,attribute.byteOffset)
			i -= 1}
		} else {
			Noto.severeError("Attribute vbos must be bound to be used, bound : " + VBO.boundArrayBuffer + " name : " + points.name)
		}
	}

	override def unsetOpenGLPointers() {
		var i = 0;while ( i < _attribProfile.attributes.length ) {
			VBO.disableVertexAttribArray(i)
		i += 1}
	}

	override def preDraw() {
		super.preDraw()
	}

	def setA (attribIndex: Int,n : Int, v : ReadVec2f) { points.set(n,_attribProfile.attributes(attribIndex).floatOffset,v) }
	def setA (attribIndex: Int,n : Int, v : ReadVec3f) { points.set(n,_attribProfile.attributes(attribIndex).floatOffset,v) }
	def setA (attribIndex: Int,n : Int, v : ReadVec4f) { points.set(n,_attribProfile.attributes(attribIndex).floatOffset,v) }
	def setA (attribIndex: Int,n : Int, x : Float, y : Float) { points.set(n,_attribProfile.attributes(attribIndex).floatOffset,x,y) }
	def setA (attribIndex: Int,n : Int, x : Float, y : Float, z : Float) { points.set(n,_attribProfile.attributes(attribIndex).floatOffset,x,y,z) }
	def setA (attribIndex: Int,n : Int, r : Float, g : Float, b : Float, a : Float) { points.set(n,_attribProfile.attributes(attribIndex).floatOffset,r,g,b,a) }

	def setAb (attribIndex: Int,n : Int, r : Byte, g : Byte, b : Byte, a : Byte) { points.setB(n,_attribProfile.attributes(attribIndex).byteOffset,r,g,b,a) }
	def setAb (attribIndex: Int,n : Int, packed : Int) { points.setPacked(n,_attribProfile.attributes(attribIndex).byteOffset,packed) }
	def setAbf (attribIndex: Int,n : Int, r : Float, g : Float, b : Float, a : Float) {
		points.setB(n,_attribProfile.attributes(attribIndex).byteOffset,(r*255).toByte,(g*255).toByte,(b*255).toByte,(a*255).toByte) }
	def setAbf (attribIndex: Int,n : Int, r : Float, g : Float, b : Float, a : Float, scale : Int) {
		points.setB(n,_attribProfile.attributes(attribIndex).byteOffset,(r*scale).toByte,(g*scale).toByte,(b*scale).toByte,(a*scale).toByte) }

	override def setC (n: Int, v: ReadVec4f){ throw new UnsupportedOperationException }
	override def setV (n: Int, v: ReadVec3f){ throw new UnsupportedOperationException }
	override def setN (n: Int, v: ReadVec3f){ throw new UnsupportedOperationException }
	override def setT (n: Int, v: ReadVec2f){ throw new UnsupportedOperationException }
	override def setC (n: Int, r: Float,g: Float, b: Float, a: Float){ throw new UnsupportedOperationException }
	override def setC (n: Int, rgba: Int, v: Float){ throw new UnsupportedOperationException }
	override def setV (n: Int, x: Float,y: Float,z: Float){ throw new UnsupportedOperationException }
	override def setN (n: Int, x: Float,y: Float,z: Float){ throw new UnsupportedOperationException }
	override def setT (n: Int, x: Float,y: Float){ throw new UnsupportedOperationException }

	def attributeIndex ( name : String ) = _attribProfile.attributesByName(name)

	def toHF ( fval : Float ) : Int = {
		val fbits = java.lang.Float.floatToIntBits( fval )
	  val sign = fbits >>> 16 & 0x8000;          // sign only
	  var value = ( fbits & 0x7fffffff ) + 0x1000; // rounded value

	  if( value >= 0x47800000 )               // might be or become NaN/Inf
	  {                                     // avoid Inf due to rounding
			if( ( fbits & 0x7fffffff ) >= 0x47800000 )
			{                                 // is or must become NaN/Inf
				 if( value < 0x7f800000 )        // was value but too large
					  return sign | 0x7c00;     // make it +/-Inf
				 return sign | 0x7c00 |        // remains +/-Inf or NaN
					  ( fbits & 0x007fffff ) >>> 13; // keep NaN (and Inf) bits
			}
			return sign | 0x7bff;             // unrounded not quite Inf
	  }
	  if( value >= 0x38800000 ){               // remains normalized value
			return sign | value - 0x38000000 >>> 13; // exp - 127 + 15
	  }
	  if( value < 0x33000000 )     {            // too small for subnormal
			return sign;                      // becomes +/-0
	  }
		value = ( fbits & 0x7fffffff ) >>> 23;  // tmp exp for subnormal calc

		sign | ( ( fbits & 0x7fffff | 0x800000 ) // add subnormal bit
			 + ( 0x800000 >>> value - 102 )     // round depending on cut off
		 >>> 126 - value );   // div by 2^(1-(exp-127+15)) and >> 13 | exp=0
	}


	//TRenderTarget implementation
	def vbo : AVBO = this
	def incrementVertexOffset ( n : Int ) = {
		val r = numPoints
		numPoints = r + n
		r
	}
	def incrementIndexOffset ( n : Int ) = {
		val r = numIndices
		numIndices = r + n
		r
	}
	def vertexOffset = numPoints
	def indexOffset = numIndices
}


case class AttribInfo(size : Int, dataType : Int, byteOffset : Int , name : String) {
	val floatOffset = byteOffset >> 2
}
class AttributeProfile(m : List[(String,(Int,Int))]) {
	var attributes = new Array[AttribInfo](m.size)
	var attributesByName = Map[String,Int]()
	var byteStride = 0


	private var i = 0
	for ( (name,(size,dataType)) <- m ) {
		attributes(i) = AttribInfo(size,dataType,byteOffset = byteStride,name = name)
		byteStride += size * (dataType match {
			case GL_BYTE => 1
			case GL_FLOAT => 4
			case GL_INT => 4
			case GL_SHORT => 2
			case GL_UNSIGNED_BYTE => 1
			case GL30.GL_HALF_FLOAT => 2
			case _ => throw new IllegalStateException("Invalid data type for attribute : " + dataType)
		})
		attributesByName += name -> i
	i += 1}

	var vertexAttributeIndex = 0
	var texCoordAttributeIndex = 1
}
object AttributeProfile {
	def apply ( m : (String,(Int,Int)) * ) = new AttributeProfile(m.toList)
}
object DefaultAttributeProfile extends AttributeProfile( List("vertex" -> (3,GL_FLOAT)) )


object AVBOTest{
	class AVBOTestConflux extends Conflux{
		val avbo = new AVBO( AttributeProfile("vertex" -> (3,GL_FLOAT) , "color" -> (4,GL_FLOAT) , "normal" -> (3,GL_FLOAT) ) )
		lazy val shader : Shader = ResourceManager.shader("shaders/test/AVBOTestShader")
		val camera = new Camera()

		var theta = 0.0f
		var phi = 0.0f

		override def drawGL(){
			camera.update(1.0f)

			glClearColor(0.0f,0.0f,0.0f,1.0f)
			glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)

			GL.glSetState(GL_DEPTH_TEST,true)
			GL.glSetState(GL_CULL_FACE,false)

			glLoadIdentity()

			theta += 0.003f
			phi += 0.001f

			val x = math.cos(theta) * 4.0f
			val y = math.sin(theta) * 4.0f
			camera.eye = (Vec3f(x.toFloat,y.toFloat,math.cos(phi).toFloat * 4.0f)).normalize * 5.0f
			camera._forward = (camera.eye * -1.0f).normalize

			camera.look()

			glBindTexture(GL_TEXTURE_2D,0)

			shader.bind()
//			shader.setUniform("uniformR",0.1f)

			avbo.bind()
//			avbo.drawElements(GL_QUAD,0,-1,skipPostDraw = false)
			avbo.draw(GL_QUAD_STRIP)

			Shader.unbind()

			avbo.unbind()

			normalVBO.bind()
			normalVBO.draw(GL_LINES)
		}
		val normalVBO = new VBO(VBO.Vertex | VBO.Color)
		override def initGL(){

			glMatrixMode(GL_PROJECTION)
			glLoadIdentity()
//			gluOrtho2D(0.0f,1440.0f/900.0f,0.0f,1.0f)
			gluPerspective(45.0f,1440.0f/900.0f,0.01f,100.0f)
			glMatrixMode(GL_MODELVIEW)

			GL.glSetState(GL_BLEND,true);
			glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

			val Vertex = avbo.attributeIndex("vertex")
			val Color = avbo.attributeIndex("color")
			val Normal = avbo.attributeIndex("normal")
			var vi = 0
			val vbo = avbo;
			val radius = 1.0f
			val w = 10;
			val h = 10;
			vbo.numPoints = (w+1)*(h+1) * 6
			vbo.numIndices = (w*h) * 4 * 6

//			def pointIndex (q : Int, x : Int, y : Int) = q * (w+1) * (h+1) + x * (h+1) + y
//			for ( q <- 0 until 6 ; x <- 0 to w ; y <- 0 to h ; mult = if ( q < 3 ) { 1.0f } else { -1.0f } ; xf = x.toFloat/w.toFloat * mult ; yf = y.toFloat/h.toFloat * mult ) {
//				var v = if ( q < 3 ) { Vec3f(-0.5f,-0.5f,-0.5f) } else { Vec3f(0.5f,0.5f,0.5f) }
//				val index = pointIndex(q,x,y)
//				if ( q % 3 == 0 ) { v.x += xf; v.y += yf; }
//				else if ( q % 3 == 1 ) { v.x += xf; v.z += yf; }
//				else if ( q % 3 == 2 ) { v.y += xf; v.z += yf; }
//				v = functions.normalize(v) * (radius)//make it a sphere
//				vbo.setA(Vertex,index,v)
//				vbo.setA(Color,index,1.0f,1.0f,1.0f,1.0f);//x.toFloat / w.toFloat,y.toFloat / h.toFloat,q.toFloat / 6.0f,1.0f)
//				vbo.setA(Normal,index,functions.normalize(v))
//
//				var forward = q < 3
//				if ( q % 3 == 1 ) { forward = ! forward; }
//				if ( x < w && y < h ) {
//					if ( forward ) { //for face culling
//						vbo.setI(vi + 0,index)
//						vbo.setI(vi + 1,pointIndex(q,x+1,y))
//						vbo.setI(vi + 2,pointIndex(q,x+1,y+1))
//						vbo.setI(vi + 3,pointIndex(q,x,y+1))
//					} else {
//						vbo.setI(vi + 3,index)
//						vbo.setI(vi + 2,pointIndex(q,x+1,y))
//						vbo.setI(vi + 1,pointIndex(q,x+1,y+1))
//						vbo.setI(vi + 0,pointIndex(q,x,y+1))
//					}
//					vi += 4
//				}
//			}

			vi = 0
			var ni = 0
			avbo.numPoints = 0
			val rings = 30
			val nsides = 30
			val R = 1.0f
			val r = 0.4f
			val ringDelta = (2.0f * math.Pi / rings.toFloat).toFloat;
			var sideDelta = (2.0f * math.Pi / nsides.toFloat).toFloat;
			var theta = 0.0f;var cosTheta = 1.0f; var sinTheta = 0.0f;
			for ( i <- (rings - 1) to 0 by -1 ) {
				val theta1 = theta + ringDelta;
				val cosTheta1 = (Math.cos(theta1)).toFloat;
				val sinTheta1 = (Math.sin(theta1)).toFloat;

				var phi = 0.0f;
				for ( j <- nsides to 0 by -1 ) {
					avbo.numPoints += 2

					phi += sideDelta;
					val cosPhi = Math.cos(phi).toFloat;
					val sinPhi = Math.sin(phi).toFloat;
					val dist = R + r * cosPhi;
					avbo.setA(Normal,vi,cosTheta1 * cosPhi, sinPhi, -sinTheta1 * cosPhi)
//					avbo.setA(Normal,vi,cosTheta * cosPhi, sinPhi, -sinTheta * cosPhi)
					avbo.setA(Vertex,vi,cosTheta1 * dist, r * sinPhi , -sinTheta1 * dist)
					avbo.setA(Normal,vi+1,cosTheta * cosPhi, sinPhi, -sinTheta * cosPhi)
					avbo.setA(Vertex,vi+1,cosTheta * dist, r * sinPhi, -sinTheta * dist)
					avbo.setA(Color,vi,1.0f,1.0f,1.0f,0.5f)
					avbo.setA(Color,vi+1,1.0f,1.0f,1.0f,0.5f)

					normalVBO.numPoints += 4
//					normalVBO.setV(ni,Vec3f(cosTheta1 * dist, r * sinPhi , -sinTheta1 * dist))
//					normalVBO.setV(ni+1,Vec3f(cosTheta1 * dist, r * sinPhi , -sinTheta1 * dist) + Vec3f(cosTheta * cosPhi, sinPhi, -sinTheta * cosPhi) * 0.2f)
//					normalVBO.setV(ni+2,Vec3f(cosTheta * dist, r * sinPhi, -sinTheta * dist))
//					normalVBO.setV(ni+3,Vec3f(cosTheta * dist, r * sinPhi, -sinTheta * dist) + Vec3f(cosTheta * cosPhi, sinPhi, -sinTheta * cosPhi) * 0.2f)
					for ( i <- 0 until 4 ) { normalVBO.setC(ni+i,1.0f,0.0f,0.0f,1.0f) }
					ni += 4
					vi += 2
				}
				theta = theta1;
				cosTheta = cosTheta1;
				sinTheta = sinTheta1;
			}

			vbo.lastUpdatedMarker += 1
			avbo.solidify(GL_STATIC_DRAW)
			normalVBO.solidify(GL_STATIC_DRAW)
		}
	}

	def main ( args : Array[String] ){
		arx.application.Application.start(new AVBOTestConflux)
	}
}