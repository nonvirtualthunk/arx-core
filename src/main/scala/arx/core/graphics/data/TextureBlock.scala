package arx.core.graphics.data

import java.nio.ByteBuffer
import org.lwjgl.opengl.GL11._
import arx.core.vec.Vec2i
import arx.core.vec.{ReadVec2f, Vec4f, Vec2f}
import org.lwjgl.BufferUtils
import java.lang.Math
import scala.math._
import org.lwjgl.opengl._
import collection.mutable
import arx.core.graphics.GL
import arx.core.datastructures.Rect
import arx.core.{ResourceManager, Application}

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/30/11
 * Time: 11:35 AM
 * Created by nonvirtualthunk
 */

@SerialVersionUID(1L)
class TextureBlock(w_base: Int,h_base: Int) {
	def this() { this(1,1) }
	var imageDimensions: Vec2i = Vec2i(w_base,h_base)
	var imageData      : ByteBuffer = BufferUtils.createByteBuffer(w * h * 4)
	var textureID: Int = 0
	var minFilter = GL_LINEAR
	var magFilter = GL_LINEAR
	var borderWidth = 16

	var subTextures = new java.util.HashMap[Image, Array[ReadVec2f]]()
	var openRects = mutable.MutableList[Rect[Int]]( Rect(1,1,w-1,h-1) )

	var pendingCommit = false

	//This should guarantee that the 0,0 pixel is always pure, opaque, white
	if ( w >= 32 && h >= 32 ) {
		var x = 0;while ( x < 32 ) {
			var y = 0;while ( y < 32 ) {
				imageData.put(y * imageDimensions.x * 4 + x * 4 + 0,255.toByte)
				imageData.put(y * imageDimensions.x * 4 + x * 4 + 1,255.toByte)
				imageData.put(y * imageDimensions.x * 4 + x * 4 + 2,255.toByte)
				imageData.put(y * imageDimensions.x * 4 + x * 4 + 3,255.toByte)
			y+=1}
		x+=1}
	}

	override def finalize () {
		release()
	}

	def w = imageDimensions.x
	def h = imageDimensions.y

	def isRectBigEnough(image: Image,rect: Rect[Int]): Boolean = ( rect.width >= effectiveWidth(image) && rect.height >= effectiveHeight(image) )

	def effectiveWidth(image: Image): Int = { image.width + (borderWidth << 1) }
	def effectiveHeight(image: Image): Int = { image.height + (borderWidth << 1) }

	def splitRect(rect: Rect[Int],image: Image): List[Rect[Int]] = {
		var ret = List[Rect[Int]]()
		if ( rect.width > effectiveWidth(image) ){
			ret ::= Rect(rect.x + effectiveWidth(image),rect.y,rect.width - effectiveWidth(image),rect.height)
			rect.width = (effectiveWidth(image))
		}
		if ( rect.height > effectiveHeight(image) ){
			ret ::= Rect(rect.x,rect.y + effectiveHeight(image),rect.width,rect.height - effectiveHeight(image))
			rect.height = (effectiveHeight(image))
		}
		ret
	}

	def addTexture (image: Image): Option[Rect[Int]] = {
		synchronized {
			val possibleRects = openRects filter { isRectBigEnough(image,_) }
			val rectOpt = (possibleRects sortBy (rect => Math.max(rect.width.toDouble / effectiveWidth(image).toDouble,
																  rect.height.toDouble / effectiveHeight(image).toDouble))).headOption
			rectOpt match {
			case Some(sourceRect) =>
				openRects = openRects filter { _ != sourceRect }

				val closedRect = Rect(sourceRect.x,sourceRect.y,effectiveWidth(image),effectiveHeight(image))
				val innerRect = Rect(sourceRect.x + borderWidth,sourceRect.y + borderWidth,image.width,image.height)
				val v4 = new Rect[Float](innerRect.x.toFloat/imageDimensions.x.toFloat,innerRect.y.toFloat/imageDimensions.y.toFloat,
																innerRect.w.toFloat/imageDimensions.x.toFloat,innerRect.h.toFloat/imageDimensions.y.toFloat)
				subTextures.put(image,Array(Vec2f(v4.x,v4.y),Vec2f(v4.x + v4.w,v4.y),Vec2f(v4.x + v4.w,v4.y + v4.h),Vec2f(v4.x,v4.y + v4.h)))
				openRects ++= splitRect(sourceRect,image)
				updateRect(closedRect,image)
				Some[Rect[Int]](sourceRect)
			case None =>
				println("No sufficiently sized Rect found")
				System.out.println(possibleRects)
				System.out.println(openRects)
				None
			}
		}
	}

	def updateRect ( rectangle: Rect[Int] , image: Image) {
		val n = 4
		def src (tx: Int,ty: Int,trgba: Int): Byte = image.raw(tx,ty,trgba)

		//val newImage = Image.withDimensions(effectiveWidth(image),effectiveHeight(image))
		for ( x <- -borderWidth until image.width + borderWidth ){
			for ( y <- -borderWidth until image.height + borderWidth ){
				for ( rgba <- 0 until n ){
					this(x + rectangle.x + borderWidth,y + rectangle.y + borderWidth,rgba) = src( max(min(x,image.width - 1),0),max(min(y,image.height - 1),0),rgba)
				}
			}
		}

		if ( Application.isOpenGLThread ){
			commitTexture(rectangle,image)
		} else {
			pendingCommit = true
		}
	}

	def commitTexture ( rectangle: Rect[Int],image: Image ){
		synchronized {
	//		if ( m_textureID == 0 || rectangle == null || image == null ){
			if ( textureID == 0 ) {
				textureID = glGenTextures()
			}

			GL.bindTexture(textureID)
				glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,minFilter)
				glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,magFilter)
				glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)

				imageData.rewind
				glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,imageDimensions.x,imageDimensions.y,0,GL_RGBA,GL_UNSIGNED_BYTE,imageData)
				//gluBuild2DMipmaps(GL_TEXTURE_2D,GL_RGBA,m_imageDimensions.x,m_imageDimensions.y,GL_RGBA,GL_UNSIGNED_BYTE,m_imageData)
	//			glTexParameteri(GL_TEXTURE_2D,GL14.GL_GENERATE_MIPMAP,GL_TRUE)
				GL30.glGenerateMipmap(GL_TEXTURE_2D)
	//		}
	//		else{
	//			glTexSubImage2D(GL_TEXTURE_2D,0,rectangle.x,rectangle.y,rectangle.width,rectangle.height,GL_RGBA,GL_UNSIGNED_BYTE,image.data)
	//		}
		}
	}


	def apply (x: Int,y: Int,rgba: Int): Byte = {
		val index = byteIndex(x,y,rgba)
		imageData.get(index)
	}
	def get (x: Int,y: Int,rgba: Int): Byte = {
		imageData.get(byteIndex(x,y,rgba))
	}

	def update (x: Int,y: Int,rgba: Int,value: Byte){
		val index = byteIndex(x,y,rgba)
		imageData.put(index,value)
	}
	def set = update _

	def byteIndex (x: Int,y: Int,b: Int): Int = {
		(y * imageDimensions.x * 4) + (x * 4) + b
	}

	def release() {
		GL.destroyTexture(this)
		textureID = 0
	}

	def width: Int = imageDimensions.x

	def height: Int = imageDimensions.y

	def bind() { bind(0) }
	def bind(textureSlot : Int) {
		if ( pendingCommit ) { commitTexture ( null,null ); pendingCommit = false }
		GL.bindTexture(textureSlot,textureID)
	}

	def unbind () {
		if ( ! GL.disabled ) {
			GL.bindTexture(0)
		}
	}

	def getImageWidth: Int = imageDimensions.x

	def getImageHeight: Int = imageDimensions.y

	def texCoord (image: Image,i: Int): ReadVec2f = {
		val v4 = subTextures.get(image)
		v4(i)
	}

	def texCoords (image: Image): Array[ReadVec2f] = {
		subTextures.get(image)
	}
	def getOrElseUpdate (image: Image ) : Array[ReadVec2f] = {
		var v4 = subTextures.get(image)
		if ( v4 == null ) {
			synchronized {
				v4 = subTextures.get(image)
				if ( v4 == null ) { //Once we're in the synchronization block, ensure that v4 is still null, hasn't been changed underneath us
					println("Adding image of dimensions : " + image.width + " x " + image.height)
					addTexture(image)
					v4 = subTextures.get(image)
				}
				v4
			}
		} else {
			v4
		}
	}
	def getOrElseUpdate ( imagePath : String ) : Array[ReadVec2f] = {
		getOrElseUpdate(ResourceManager.getImage(imagePath))
	}
	def apply ( image : Image ) : Array[ReadVec2f] = getOrElseUpdate(image)
	def apply ( image : TToImage ) : Array[ReadVec2f] = getOrElseUpdate(image.image)

	def colorAt ( x: Float , y : Float ) : Vec4f = colorAt ( (x * width).toInt , (y * height).toInt )
	def colorAt ( x : Int , y : Int ) : Vec4f = {
		Vec4f(toUnsignedInt(this(x,y,0)).toFloat / 255.0f,toUnsignedInt(this(x,y,0)).toFloat / 255.0f,
			toUnsignedInt(this(x,y,0)).toFloat / 255.0f,toUnsignedInt(this(x,y,0)).toFloat / 255.0f)
	}
	protected def toUnsignedInt ( b : Byte ) = if ( b < 0 ) { 256 + b.toInt } else { b.toInt }


	def containsTexture ( image: Image ) : Boolean = subTextures.containsKey(image)
	def isSentinel = false
}

object TextureBlock{
	object SentinelTextureBlock extends TextureBlock(1,1) {
		override def isSentinel = true

		val tc = Array(ReadVec2f(0.0f,0.0f),ReadVec2f(0.0f,0.0f),ReadVec2f(0.0f,0.0f),ReadVec2f(0.0f,0.0f))
		override def getOrElseUpdate(image: Image) = tc
		override def texCoords(image: Image) = tc
		override def containsTexture(image: Image) = true
	}

	def Sentinel : TextureBlock = SentinelTextureBlock
}