package arx.core.graphics.data

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/25/12
 * Time: 2:41 PM
 * Created by nonvirtualthunk
 */

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import arx.core.vec._
import org.lwjgl.opengl.{GL30, GL20}
import arx.core.graphics.shader.Shader
import arx.core.graphics.GL

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
			println("Attribute vbos must be bound to be used, bound : " + VBO.boundArrayBuffer + " name : " + points.name)
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